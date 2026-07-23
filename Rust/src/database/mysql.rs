use std::collections::BTreeMap;

use mysql::{ Conn, Error as MySqlDriverError, Opts, OptsBuilder, Params, Row };
use mysql::Value as MySqlValue;
use mysql::consts::CapabilityFlags;
use mysql::prelude::Queryable;

use crate::ast::DataType;
use crate::query::*;
use crate::sql::*;
use crate::value::*;

use super::records::*;
use super::locking::*;
use super::runtime::DatabaseDriver;
use super::transactions::*;
use super::values::runtime_type_name;
use super::writes::*;

pub(super) struct MySqlSession {
	connection: Conn,
	transactions: TransactionState,
}

impl MySqlSession {
	pub fn open(database_name: &str, connection_string: &str) -> Result<Self, String> {
		let url = connection_url(database_name, connection_string)?;
		let options = Opts::from_url(url).map_err(|error| {
			format!("MySQL database `{database_name}` has an invalid runtime connection string: {error}")
		})?;
		let options = OptsBuilder::from_opts(options)
			.additional_capabilities(CapabilityFlags::CLIENT_FOUND_ROWS);
		let connection = Conn::new(options).map_err(|error| {
			format!("Failed to connect to MySQL database `{database_name}`: {error}")
		})?;

		Ok(Self {
			connection,
			transactions: TransactionState::default(),
		})
	}
}

impl DatabaseDriver for MySqlSession {
	fn advance_sequence(&mut self, _schema_is_implicit: bool, _schema_name: &str, sequence_name: &str) -> Result<i64, String> {
		Err(unsupported_sequence(sequence_name))
	}

	fn commit_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String> {
		let Self { connection, transactions } = self;
		transactions.commit(target_depth, savepoint_name, |command| {
			execute_transaction_command(connection, command)
		})
	}

	fn create_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let write = create_record_write(&record, WriteDialect::MySql)?;
		let affected_rows = execute_record_write(&mut self.connection, "create", write)?;

		expect_one_affected_row("MySQL", "create", affected_rows)?;
		Ok(mark_record_created(record))
	}

	fn delete_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let write = delete_record_write(&record, WriteDialect::MySql)?;
		let affected_rows = execute_record_write(&mut self.connection, "delete", write)?;

		expect_one_affected_row("MySQL", "delete", affected_rows)?;
		Ok(mark_record_deleted(record))
	}

	fn execute_query(&mut self, query: &SqlQuery, parameters: Vec<Value>) -> Result<Value, String> {
		let parameters = parameters.into_iter()
			.map(runtime_value_to_mysql)
			.collect::<Result<Vec<_>, _>>()?;
		let transaction_active = self.transactions.is_active();
		let statement = query_statement(query, transaction_active);
		let rows = match self.connection.exec::<Row, _, _>(statement.as_ref(), Params::Positional(parameters)) {
			Ok(rows) => rows,
			Err(error) if transaction_active
				&& query.lock_mode == RecordLockMode::UpdateNoWait
				&& !is_lock_available(&error) => {
				return lock_conflict_result(query).ok_or_else(|| {
					String::from("MySQL reported a record-lock conflict for a query that does not return one record pointer.")
				});
			}
			Err(error) => return Err(format!("Failed to execute MySQL query: {error}")),
		};

		match &query.result_shape {
			SqlQueryResultShape::IntegerScalar => {
				let row = rows.first().ok_or_else(|| String::from("MySQL scalar query returned no rows."))?;
				let value = row.as_ref(0).ok_or_else(|| String::from("MySQL scalar query returned an empty row."))?;
				Ok(Value::Integer(mysql_integer(value)?))
			}
			SqlQueryResultShape::RecordPointer(layout) => {
				let schema = known_record_schema(layout)?;
				let selected_columns = selected_record_columns(layout)?;
				let Some(row) = rows.first() else {
					return Ok(Value::RecordPointer(empty_record_pointer(query, schema)));
				};
				let fields = load_record_fields(row, &selected_columns)?;
				let original_fields = fields.clone();
				Ok(Value::RecordPointer(record_pointer(
					query,
					schema,
					fields,
					original_fields,
					BTreeMap::new(),
					BTreeMap::new(),
				)))
			}
			SqlQueryResultShape::RecordPointerArray(layout) => {
				let schema = known_record_schema(layout)?;
				let selected_columns = selected_record_columns(layout)?;
				let mut loaded_records = Vec::with_capacity(rows.len());

				for row in &rows {
					let fields = load_record_fields(row, &selected_columns)?;
					loaded_records.push(LoadedRecord {
						group_keys: load_group_keys(row, selected_columns.len(), &query.group_by)?,
						original_fields: fields.clone(),
						fields,
						projected_values: load_scalar_projections(row, &query.scalar_projections)?,
					});
				}

				let boundaries = record_group_boundaries(&loaded_records, &query.group_by);
				let records = loaded_records.into_iter().enumerate()
					.map(|(index, loaded)| Value::RecordPointer(record_pointer(
						query,
						schema,
						loaded.fields,
						loaded.original_fields,
						boundaries.get(index).cloned().unwrap_or_default(),
						loaded.projected_values,
					)))
					.collect();
				Ok(Value::Array(records))
			}
		}
	}

	fn load_sequence_current(&mut self, _schema_is_implicit: bool, _schema_name: &str, sequence_name: &str) -> Result<i64, String> {
		Err(unsupported_sequence(sequence_name))
	}

	fn rollback_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String> {
		let Self { connection, transactions } = self;
		transactions.rollback(target_depth, savepoint_name, |command| {
			execute_transaction_command(connection, command)
		})
	}

	fn store_sequence_current(
		&mut self,
		_schema_is_implicit: bool,
		_schema_name: &str,
		sequence_name: &str,
		_value: i64,
	) -> Result<(), String> {
		Err(unsupported_sequence(sequence_name))
	}

	fn sync_transactions(&mut self, transaction_names: &[String]) -> Result<(), String> {
		let Self { connection, transactions } = self;
		transactions.synchronize(transaction_names, |command| {
			execute_transaction_command(connection, command)
		})
	}

	fn update_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let write = update_record_write(&record, WriteDialect::MySql)?;
		let affected_rows = execute_record_write(&mut self.connection, "update", write)?;

		expect_one_affected_row("MySQL", "update", affected_rows)?;
		Ok(mark_record_updated(record))
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

fn execute_record_write(connection: &mut Conn, operation: &str, write: RecordWrite) -> Result<u64, String> {
	let parameters = write.parameters.into_iter()
		.map(runtime_value_to_mysql)
		.collect::<Result<Vec<_>, _>>()?;

	connection.exec_drop(&write.statement, Params::Positional(parameters))
		.map_err(|error| format!("Failed to execute MySQL {operation} statement: {error}"))?;
	Ok(connection.affected_rows())
}

fn execute_transaction_command(connection: &mut Conn, command: TransactionCommand) -> Result<(), String> {
	let statement = transaction_statement(&command);
	connection.query_drop(&statement)
		.map_err(|error| format!("Failed to execute MySQL transaction command `{statement}`: {error}"))
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

fn is_lock_available(error: &MySqlDriverError) -> bool {
	!matches!(error, MySqlDriverError::MySqlError(error) if error.code == 3572)
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

fn load_scalar_projections(
	row: &Row,
	projections: &[SqlScalarProjection],
) -> Result<BTreeMap<u32, Value>, String> {
	projections.iter().map(|projection| {
		let value = row.as_ref(projection.column_index as usize)
			.ok_or_else(|| format!("MySQL result is missing projected value {}.", projection.value_id.0))?;
		let value = database_value(value, &projection.data_type)?;
		let value = database_record_field_runtime_value(
			&value,
			&projection.data_type,
			projection.data_type.is_nullable(),
		)?;
		Ok((projection.value_id.0, value))
	}).collect()
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

fn transaction_statement(command: &TransactionCommand) -> String {
	match command {
		TransactionCommand::Begin => String::from("START TRANSACTION"),
		TransactionCommand::Commit => String::from("COMMIT"),
		TransactionCommand::ReleaseSavepoint(name) => format!("RELEASE SAVEPOINT {}", quote_mysql_identifier(name)),
		TransactionCommand::Rollback => String::from("ROLLBACK"),
		TransactionCommand::RollbackToSavepoint(name) => format!("ROLLBACK TO SAVEPOINT {}", quote_mysql_identifier(name)),
		TransactionCommand::Savepoint(name) => format!("SAVEPOINT {}", quote_mysql_identifier(name)),
	}
}

fn unsupported_sequence(sequence_name: &str) -> String {
	format!("MySQL does not support standalone sequence `{sequence_name}`.")
}

#[cfg(test)]
mod tests {
	use super::*;

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
	fn identifies_mysql_nowait_lock_error() {
		let error = MySqlDriverError::MySqlError(mysql::MySqlError {
			code: 3572,
			message: String::from("Do not wait for lock."),
			state: String::from("HY000"),
		});

		assert!(!is_lock_available(&error));
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
	fn renders_mysql_transaction_commands() {
		assert_eq!(transaction_statement(&TransactionCommand::Begin), "START TRANSACTION");
		assert_eq!(
			transaction_statement(&TransactionCommand::Savepoint(String::from("tablo_tx_1"))),
			"SAVEPOINT `tablo_tx_1`",
		);
		assert_eq!(
			transaction_statement(&TransactionCommand::RollbackToSavepoint(String::from("tablo_tx_1"))),
			"ROLLBACK TO SAVEPOINT `tablo_tx_1`",
		);
		assert_eq!(
			transaction_statement(&TransactionCommand::ReleaseSavepoint(String::from("tablo_tx_1"))),
			"RELEASE SAVEPOINT `tablo_tx_1`",
		);
		assert_eq!(transaction_statement(&TransactionCommand::Commit), "COMMIT");
		assert_eq!(transaction_statement(&TransactionCommand::Rollback), "ROLLBACK");
	}

	#[test]
	fn reports_that_standalone_sequences_are_unsupported() {
		assert_eq!(
			unsupported_sequence("InvoiceNumber"),
			"MySQL does not support standalone sequence `InvoiceNumber`.",
		);
	}

	#[test]
	fn represents_null_parameter_without_a_sentinel_value() {
		assert_eq!(runtime_value_to_mysql(Value::Null).unwrap(), mysql::Value::NULL);
	}
}
