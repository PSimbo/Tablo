use std::collections::BTreeMap;

use mysql::Conn;
use mysql::Opts;
use mysql::Params;
use mysql::Row;
use mysql::Value as MySqlValue;
use mysql::prelude::Queryable;

use crate::ast::DataType;
use crate::query::QueryResultColumn;
use crate::query::SqlGroupByItem;
use crate::query::SqlQuery;
use crate::query::SqlQueryResultShape;
use crate::value::DatabaseValue;
use crate::value::RecordFieldValue;
use crate::value::RecordPointerValue;
use crate::value::Value;
use crate::value::database_record_field_runtime_value;

use super::records::LoadedRecord;
use super::records::empty_record_pointer;
use super::records::normalize_name;
use super::records::record_group_boundaries;
use super::records::record_pointer;
use super::runtime::DatabaseDriver;
use super::values::runtime_type_name;

pub(super) struct MySqlSession {
	connection: Conn,
}

impl MySqlSession {
	pub fn open(database_name: &str, connection_string: &str) -> Result<Self, String> {
		let url = connection_url(database_name, connection_string)?;
		let options = Opts::from_url(url).map_err(|error| {
			format!("MySQL database `{database_name}` has an invalid runtime connection string: {error}")
		})?;
		let connection = Conn::new(options).map_err(|error| {
			format!("Failed to connect to MySQL database `{database_name}`: {error}")
		})?;

		Ok(Self { connection })
	}
}

impl DatabaseDriver for MySqlSession {
	fn commit_transaction(&mut self, _target_depth: usize, _savepoint_name: &str) -> Result<(), String> {
		Err(unsupported_operation("transaction commits"))
	}

	fn create_record(&mut self, _record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		Err(unsupported_operation("create statements"))
	}

	fn delete_record(&mut self, _record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		Err(unsupported_operation("delete statements"))
	}

	fn execute_query(&mut self, query: &SqlQuery, parameters: Vec<Value>) -> Result<Value, String> {
		let parameters = parameters.into_iter()
			.map(runtime_value_to_mysql)
			.collect::<Result<Vec<_>, _>>()?;
		let rows = self.connection.exec::<Row, _, _>(&query.statement, Params::Positional(parameters))
			.map_err(|error| format!("Failed to execute MySQL query: {error}"))?;

		match &query.result_shape {
			SqlQueryResultShape::IntegerScalar => {
				let row = rows.first().ok_or_else(|| String::from("MySQL scalar query returned no rows."))?;
				let value = row.as_ref(0).ok_or_else(|| String::from("MySQL scalar query returned an empty row."))?;
				Ok(Value::Integer(mysql_integer(value)?))
			}
			SqlQueryResultShape::RecordPointer(columns) => {
				let Some(row) = rows.first() else {
					return Ok(Value::RecordPointer(empty_record_pointer(query, columns)));
				};
				let fields = load_record_fields(row, columns)?;
				let original_fields = fields.clone();
				Ok(Value::RecordPointer(record_pointer(query, columns, fields, original_fields, BTreeMap::new())))
			}
			SqlQueryResultShape::RecordPointerArray(columns) => {
				let mut loaded_records = Vec::with_capacity(rows.len());

				for row in &rows {
					let fields = load_record_fields(row, columns)?;
					loaded_records.push(LoadedRecord {
						group_keys: load_group_keys(row, columns.len(), &query.group_by)?,
						original_fields: fields.clone(),
						fields,
					});
				}

				let boundaries = record_group_boundaries(&loaded_records, &query.group_by);
				let records = loaded_records.into_iter().enumerate()
					.map(|(index, loaded)| Value::RecordPointer(record_pointer(
						query,
						columns,
						loaded.fields,
						loaded.original_fields,
						boundaries.get(index).cloned().unwrap_or_default(),
					)))
					.collect();
				Ok(Value::Array(records))
			}
		}
	}

	fn load_sequence_current(&mut self, _schema_is_implicit: bool, _schema_name: &str, _sequence_name: &str) -> Result<i64, String> {
		Err(unsupported_operation("sequence reads"))
	}

	fn rollback_transaction(&mut self, _target_depth: usize, _savepoint_name: &str) -> Result<(), String> {
		Err(unsupported_operation("transaction rollbacks"))
	}

	fn store_sequence_current(
		&mut self,
		_schema_is_implicit: bool,
		_schema_name: &str,
		_sequence_name: &str,
		_value: i64,
	) -> Result<(), String> {
		Err(unsupported_operation("sequence writes"))
	}

	fn sync_transactions(&mut self, transaction_names: &[String]) -> Result<(), String> {
		if transaction_names.is_empty() {
			Ok(())
		}
		else {
			Err(unsupported_operation("transactions"))
		}
	}

	fn update_record(&mut self, _record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		Err(unsupported_operation("update statements"))
	}
}

fn connection_url<'a>(database_name: &str, connection_string: &'a str) -> Result<&'a str, String> {
	if connection_string.starts_with("mysql://") {
		Ok(connection_string)
	}
	else {
		Err(format!(
			"MySQL connection string for database `{database_name}` must use `mysql://user:password@host/database` format."
		))
	}
}

fn database_value(value: &MySqlValue, data_type: &DataType) -> Result<DatabaseValue, String> {
	match value {
		MySqlValue::Bytes(value) => Ok(DatabaseValue::Text(String::from_utf8(value.clone())
			.map_err(|_| String::from("MySQL returned text data that is not valid UTF-8."))?)),
		MySqlValue::Date(year, month, day, hour, minute, second, microsecond) => {
			match data_type.without_nullability() {
				DataType::Date => Ok(DatabaseValue::Text(format!("{year:04}-{month:02}-{day:02}"))),
				DataType::Timestamp => Ok(DatabaseValue::Text(format_mysql_timestamp(
					*year,
					*month,
					*day,
					*hour,
					*minute,
					*second,
					*microsecond,
				))),
				other => Err(format!("Cannot convert MySQL date/time value to `{}`.", other.name())),
			}
		}
		MySqlValue::Double(value) => Ok(DatabaseValue::Real(value.to_string())),
		MySqlValue::Float(value) => Ok(DatabaseValue::Real(value.to_string())),
		MySqlValue::Int(value) => Ok(DatabaseValue::Integer(*value)),
		MySqlValue::NULL => Ok(DatabaseValue::Null),
		MySqlValue::Time(negative, days, hour, minute, second, microsecond) => {
			if !matches!(data_type.without_nullability(), DataType::Time) {
				return Err(format!("Cannot convert MySQL time value to `{}`.", data_type.name()));
			}

			if *negative || *days != 0 {
				return Err(String::from("MySQL duration-style `TIME` value cannot be converted to Tablo `time`."));
			}

			Ok(DatabaseValue::Text(format_mysql_time(*hour, *minute, *second, *microsecond)))
		}
		MySqlValue::UInt(value) => i64::try_from(*value)
			.map(DatabaseValue::Integer)
			.map_err(|_| format!("MySQL unsigned integer `{value}` exceeds the range of Tablo `int`.")),
	}
}

fn format_mysql_time(hour: u8, minute: u8, second: u8, microsecond: u32) -> String {
	let mut value = format!("{hour:02}:{minute:02}:{second:02}");

	if microsecond != 0 {
		let fractional = format!("{microsecond:06}").trim_end_matches('0').to_string();
		value.push('.');
		value.push_str(&fractional);
	}

	value
}

fn format_mysql_timestamp(
	year: u16,
	month: u8,
	day: u8,
	hour: u8,
	minute: u8,
	second: u8,
	microsecond: u32,
) -> String {
	format!("{year:04}-{month:02}-{day:02}T{}", format_mysql_time(hour, minute, second, microsecond))
}

fn load_group_keys(row: &Row, column_count: usize, group_by: &[SqlGroupByItem]) -> Result<Vec<Value>, String> {
	group_by.iter().enumerate().map(|(index, item)| {
		let value = row.as_ref(column_count + index).ok_or_else(|| String::from("MySQL result is missing a grouping column."))?;
		let value = database_value(value, &item.data_type)?;
		database_record_field_runtime_value(&value, &item.data_type, item.data_type.is_nullable())
	}).collect()
}

fn load_record_fields(row: &Row, columns: &[QueryResultColumn]) -> Result<BTreeMap<String, RecordFieldValue>, String> {
	let mut fields = BTreeMap::new();

	for (index, column) in columns.iter().enumerate() {
		let value = row.as_ref(index).ok_or_else(|| format!("MySQL result is missing column `{}`.", column.column_name))?;
		fields.insert(normalize_name(&column.column_name), RecordFieldValue::Deferred {
			data_type: column.data_type.clone(),
			is_nullable: column.is_nullable,
			value: database_value(value, &column.data_type)?,
		});
	}

	Ok(fields)
}

fn mysql_integer(value: &MySqlValue) -> Result<i64, String> {
	match value {
		MySqlValue::Int(value) => Ok(*value),
		MySqlValue::UInt(value) => i64::try_from(*value)
			.map_err(|_| format!("MySQL unsigned integer `{value}` exceeds the range of Tablo `int`.")),
		other => Err(format!("MySQL scalar query returned a `{}` value instead of an integer.", mysql_type_name(other))),
	}
}

fn mysql_type_name(value: &MySqlValue) -> &'static str {
	match value {
		MySqlValue::Bytes(_) => "text",
		MySqlValue::Date(..) => "date/time",
		MySqlValue::Double(_) => "double",
		MySqlValue::Float(_) => "float",
		MySqlValue::Int(_) => "integer",
		MySqlValue::NULL => "null",
		MySqlValue::Time(..) => "time",
		MySqlValue::UInt(_) => "unsigned integer",
	}
}

fn runtime_value_to_mysql(value: Value) -> Result<MySqlValue, String> {
	match value {
		Value::Boolean(value) => Ok(MySqlValue::Int(if value { 1 } else { 0 })),
		Value::Date(value) => Ok(MySqlValue::Bytes(value.to_string().into_bytes())),
		Value::Decimal(value) => Ok(MySqlValue::Bytes(value.to_string().into_bytes())),
		Value::Integer(value) => Ok(MySqlValue::Int(value)),
		Value::Null => Ok(MySqlValue::NULL),
		Value::Text(value) => Ok(MySqlValue::Bytes(value.into_bytes())),
		Value::Time(value) => Ok(MySqlValue::Bytes(value.to_string().into_bytes())),
		Value::TimeTz(value) => Ok(MySqlValue::Bytes(value.to_string().into_bytes())),
		Value::Timestamp(value) => Ok(MySqlValue::Bytes(value.to_string().into_bytes())),
		Value::TimestampTz(value) => Ok(MySqlValue::Bytes(value.to_string().into_bytes())),
		other => Err(format!("Cannot bind a `{}` value into a MySQL query parameter.", runtime_type_name(&other))),
	}
}

fn unsupported_operation(operation: &str) -> String {
	format!("MySQL {operation} are not implemented yet.")
}

#[cfg(test)]
mod tests {
	use crate::ast::DataType;
	use crate::value::Decimal;
	use crate::value::Value;

	use super::connection_url;
	use super::database_value;
	use super::runtime_value_to_mysql;

	#[test]
	fn accepts_standard_mysql_url() {
		assert_eq!(
			connection_url("ExampleDb", "mysql://tablo@localhost/example").unwrap(),
			"mysql://tablo@localhost/example",
		);
	}

	#[test]
	fn converts_mysql_timestamp_to_iso_text() {
		assert_eq!(
			database_value(&mysql::Value::Date(2026, 7, 20, 14, 30, 12, 340_000), &DataType::Timestamp).unwrap(),
			crate::value::DatabaseValue::Text(String::from("2026-07-20T14:30:12.34")),
		);
	}

	#[test]
	fn preserves_exact_decimal_parameter_text() {
		let decimal = Decimal::from_literal("3.1415926535897932384626433832795028841").unwrap();

		assert_eq!(
			runtime_value_to_mysql(Value::Decimal(decimal)).unwrap(),
			mysql::Value::Bytes(b"3.1415926535897932384626433832795028841".to_vec()),
		);
	}

	#[test]
	fn rejects_duration_style_mysql_time() {
		let error = database_value(&mysql::Value::Time(false, 1, 0, 0, 0, 0), &DataType::Time).unwrap_err();

		assert_eq!(error, "MySQL duration-style `TIME` value cannot be converted to Tablo `time`.");
	}

	#[test]
	fn rejects_non_url_connection_string() {
		let error = connection_url("ExampleDb", "mysql:server=localhost").unwrap_err();

		assert_eq!(
			error,
			"MySQL connection string for database `ExampleDb` must use `mysql://user:password@host/database` format.",
		);
	}

	#[test]
	fn rejects_unsigned_integer_outside_tablo_range() {
		let error = database_value(&mysql::Value::UInt(i64::MAX as u64 + 1), &DataType::Int).unwrap_err();

		assert_eq!(error, "MySQL unsigned integer `9223372036854775808` exceeds the range of Tablo `int`.");
	}

	#[test]
	fn represents_null_parameter_without_a_sentinel_value() {
		assert_eq!(runtime_value_to_mysql(Value::Null).unwrap(), mysql::Value::NULL);
	}
}
