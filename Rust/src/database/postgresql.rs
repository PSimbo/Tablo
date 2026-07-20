use std::collections::BTreeMap;

use native_tls::TlsConnector;
use postgres::Client;
use postgres::types::ToSql;
use postgres_native_tls::MakeTlsConnector;

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

pub(super) struct PostgreSqlSession {
	client: Client,
}

impl PostgreSqlSession {
	pub fn open(database_name: &str, connection_string: &str) -> Result<Self, String> {
		let connection_string = client_connection_string(database_name, connection_string)?;
		let connector = TlsConnector::builder().build().map(MakeTlsConnector::new).map_err(|error| {
			format!("Failed to initialize TLS for PostgreSQL database `{database_name}`: {error}")
		})?;
		let client = Client::connect(connection_string, connector).map_err(|error| {
			format!("Failed to connect to PostgreSQL database `{database_name}`: {error}")
		})?;

		Ok(Self { client })
	}
}

impl DatabaseDriver for PostgreSqlSession {
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
			.map(runtime_value_to_postgresql)
			.collect::<Result<Vec<_>, _>>()?;
		let parameter_refs = parameters.iter()
			.map(|value| value as &(dyn ToSql + Sync))
			.collect::<Vec<_>>();
		let rows = self.client.query(&query.statement, &parameter_refs)
			.map_err(|error| format!("Failed to execute PostgreSQL query: {error}"))?;

		match &query.result_shape {
			SqlQueryResultShape::IntegerScalar => {
				let row = rows.first().ok_or_else(|| String::from("PostgreSQL scalar query returned no rows."))?;
				let result = row.try_get::<_, i64>(0)
					.map_err(|error| format!("Failed to read PostgreSQL integer result: {error}"))?;
				Ok(Value::Integer(result))
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

fn client_connection_string<'a>(database_name: &str, connection_string: &'a str) -> Result<&'a str, String> {
	let value = if connection_string.starts_with("postgresql://") || connection_string.starts_with("postgres://") {
		connection_string
	}
	else {
		connection_string.split_once(':')
			.map(|(_, value)| value)
			.ok_or_else(|| format!("PostgreSQL database `{database_name}` has an invalid runtime connection string."))?
	};

	if value.is_empty() {
		return Err(format!("PostgreSQL connection string for database `{database_name}` must include connection parameters."));
	}

	Ok(value)
}

fn load_group_keys(row: &postgres::Row, column_count: usize, group_by: &[SqlGroupByItem]) -> Result<Vec<Value>, String> {
	group_by.iter().enumerate().map(|(index, item)| {
		let value = text_database_value(row, column_count + index, "grouping column", &item.data_type)?;
		database_record_field_runtime_value(&value, &item.data_type, item.data_type.is_nullable())
	}).collect()
}

fn load_record_fields(row: &postgres::Row, columns: &[QueryResultColumn]) -> Result<BTreeMap<String, RecordFieldValue>, String> {
	let mut fields = BTreeMap::new();

	for (index, column) in columns.iter().enumerate() {
		fields.insert(normalize_name(&column.column_name), RecordFieldValue::Deferred {
			data_type: column.data_type.clone(),
			is_nullable: column.is_nullable,
			value: text_database_value(row, index, &format!("column `{}`", column.column_name), &column.data_type)?,
		});
	}

	Ok(fields)
}

fn normalize_temporal_text(mut value: String, data_type: &DataType) -> String {
	if !matches!(data_type.without_nullability(), DataType::TimeTz | DataType::TimestampTz) {
		return value;
	}

	let offset_start = value.char_indices().rev()
		.find(|(_, ch)| *ch == '+' || *ch == '-')
		.map(|(index, _)| index);

	if offset_start.is_some_and(|index| value.len() - index == 3) {
		value.push_str(":00");
	}

	value
}

fn runtime_value_to_postgresql(value: Value) -> Result<Option<String>, String> {
	match value {
		Value::Boolean(value) => Ok(Some(value.to_string())),
		Value::Date(value) => Ok(Some(value.to_string())),
		Value::Decimal(value) => Ok(Some(value.to_string())),
		Value::Integer(value) => Ok(Some(value.to_string())),
		Value::Null => Ok(None),
		Value::Text(value) => Ok(Some(value)),
		Value::Time(value) => Ok(Some(value.to_string())),
		Value::TimeTz(value) => Ok(Some(value.to_string())),
		Value::Timestamp(value) => Ok(Some(value.to_string())),
		Value::TimestampTz(value) => Ok(Some(value.to_string())),
		other => Err(format!("Cannot bind a `{}` value into a PostgreSQL query parameter.", runtime_type_name(&other))),
	}
}

fn text_database_value(
	row: &postgres::Row,
	index: usize,
	description: &str,
	data_type: &DataType,
) -> Result<DatabaseValue, String> {
	let value = row.try_get::<_, Option<String>>(index)
		.map_err(|error| format!("Failed to read PostgreSQL {description}: {error}"))?;
	Ok(match value {
		Some(value) => DatabaseValue::Text(normalize_temporal_text(value, data_type)),
		None => DatabaseValue::Null,
	})
}

fn unsupported_operation(operation: &str) -> String {
	format!("PostgreSQL {operation} are not implemented yet.")
}

#[cfg(test)]
mod tests {
	use crate::ast::DataType;
	use crate::value::Decimal;
	use crate::value::Value;

	use super::client_connection_string;
	use super::normalize_temporal_text;
	use super::runtime_value_to_postgresql;

	#[test]
	fn extracts_keyword_connection_parameters() {
		assert_eq!(
			client_connection_string("ExampleDb", "postgresql:host=localhost user=tablo").unwrap(),
			"host=localhost user=tablo",
		);
	}

	#[test]
	fn normalizes_postgresql_whole_hour_time_zone_offset() {
		assert_eq!(
			normalize_temporal_text(String::from("2026-07-20 14:30:00+01"), &DataType::TimestampTz),
			"2026-07-20 14:30:00+01:00",
		);
		assert_eq!(
			normalize_temporal_text(String::from("14:30:00+01:30"), &DataType::TimeTz),
			"14:30:00+01:30",
		);
	}

	#[test]
	fn preserves_exact_decimal_parameter_text() {
		let decimal = Decimal::from_literal("3.1415926535897932384626433832795028841").unwrap();

		assert_eq!(
			runtime_value_to_postgresql(Value::Decimal(decimal)).unwrap(),
			Some(String::from("3.1415926535897932384626433832795028841")),
		);
	}

	#[test]
	fn preserves_postgresql_uri() {
		assert_eq!(
			client_connection_string("ExampleDb", "postgresql://tablo@localhost/example").unwrap(),
			"postgresql://tablo@localhost/example",
		);
	}

	#[test]
	fn rejects_empty_connection_parameters() {
		let error = client_connection_string("ExampleDb", "postgresql:").unwrap_err();

		assert_eq!(
			error,
			"PostgreSQL connection string for database `ExampleDb` must include connection parameters.",
		);
	}

	#[test]
	fn represents_null_parameter_without_a_sentinel_value() {
		assert_eq!(runtime_value_to_postgresql(Value::Null).unwrap(), None);
	}
}
