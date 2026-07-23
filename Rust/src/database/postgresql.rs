use std::collections::BTreeMap;

use native_tls::TlsConnector;
use postgres::Client;
use postgres::error::SqlState;
use postgres::types::ToSql;
use postgres_native_tls::MakeTlsConnector;

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

pub(super) struct PostgreSqlSession {
	client: Client,
	next_lock_probe_id: u64,
	transactions: TransactionState,
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

		Ok(Self {
			client,
			next_lock_probe_id: 0,
			transactions: TransactionState::default(),
		})
	}
}

impl DatabaseDriver for PostgreSqlSession {
	fn advance_sequence(&mut self, schema_is_implicit: bool, schema_name: &str, sequence_name: &str) -> Result<i64, String> {
		let sequence = sequence_source(schema_name, sequence_name, schema_is_implicit);
		let row = self.client.query_one(
			"SELECT nextval(CAST(CAST($1 AS TEXT) AS regclass))",
			&[&sequence],
		).map_err(|error| format!("Failed to advance PostgreSQL sequence `{sequence_name}`: {error}"))?;

		row.try_get(0).map_err(|error| format!("Failed to read PostgreSQL sequence `{sequence_name}` value: {error}"))
	}

	fn commit_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String> {
		let Self { client, transactions, .. } = self;
		transactions.commit(target_depth, savepoint_name, |command| {
			execute_transaction_command(client, command)
		})
	}

	fn create_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let write = create_record_write(&record, WriteDialect::PostgreSql)?;
		let affected_rows = execute_record_write(&mut self.client, "create", write)?;

		expect_one_affected_row("PostgreSQL", "create", affected_rows)?;
		Ok(mark_record_created(record))
	}

	fn delete_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let write = delete_record_write(&record, WriteDialect::PostgreSql)?;
		let affected_rows = execute_record_write(&mut self.client, "delete", write)?;

		expect_one_affected_row("PostgreSQL", "delete", affected_rows)?;
		Ok(mark_record_deleted(record))
	}

	fn execute_query(&mut self, query: &SqlQuery, parameters: Vec<Value>) -> Result<Value, String> {
		let parameters = parameters.into_iter()
			.map(runtime_value_to_postgresql)
			.collect::<Result<Vec<_>, _>>()?;
		let parameter_refs = parameters.iter()
			.map(|value| value as &(dyn ToSql + Sync))
			.collect::<Vec<_>>();
		let transaction_active = self.transactions.is_active();
		let statement = query_statement(query, transaction_active);
		let lock_probe = if transaction_active && query.lock_mode == RecordLockMode::UpdateNoWait {
			let savepoint_name = format!("tablo_lock_probe_{}", self.next_lock_probe_id);
			self.next_lock_probe_id += 1;
			self.client.batch_execute(&format!("SAVEPOINT {savepoint_name}"))
				.map_err(|error| format!("Failed to start PostgreSQL record-lock probe: {error}"))?;
			Some(savepoint_name)
		}
		else {
			None
		};
		let rows = match self.client.query(statement.as_ref(), &parameter_refs) {
			Ok(rows) => {
				if let Some(savepoint_name) = &lock_probe {
					self.client.batch_execute(&format!("RELEASE SAVEPOINT {savepoint_name}"))
						.map_err(|error| format!("Failed to complete PostgreSQL record-lock probe: {error}"))?;
				}
				rows
			}
			Err(error) if transaction_active
				&& query.lock_mode == RecordLockMode::UpdateNoWait
				&& error.code() == Some(&SqlState::LOCK_NOT_AVAILABLE) => {
				let savepoint_name = lock_probe.as_ref()
					.expect("PostgreSQL non-waiting lock query must have an internal savepoint.");
				self.client.batch_execute(&format!(
					"ROLLBACK TO SAVEPOINT {savepoint_name}; RELEASE SAVEPOINT {savepoint_name}"
				)).map_err(|recovery_error| {
					format!("Failed to recover from PostgreSQL record-lock conflict: {recovery_error}")
				})?;
				return lock_conflict_result(query).ok_or_else(|| {
					String::from("PostgreSQL reported a record-lock conflict for a query that does not return one record pointer.")
				});
			}
			Err(error) => return Err(format!("Failed to execute PostgreSQL query: {error}")),
		};

		match &query.result_shape {
			SqlQueryResultShape::IntegerScalar => {
				let row = rows.first().ok_or_else(|| String::from("PostgreSQL scalar query returned no rows."))?;
				let result = row.try_get::<_, i64>(0)
					.map_err(|error| format!("Failed to read PostgreSQL integer result: {error}"))?;
				Ok(Value::Integer(result))
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

	fn load_sequence_current(&mut self, schema_is_implicit: bool, schema_name: &str, sequence_name: &str) -> Result<i64, String> {
		let statement = format!(
			"SELECT last_value FROM {}",
			sequence_source(schema_name, sequence_name, schema_is_implicit),
		);
		let row = self.client.query_one(&statement, &[])
			.map_err(|error| format!("Failed to read PostgreSQL sequence `{sequence_name}`: {error}"))?;

		row.try_get(0).map_err(|error| format!("Failed to read PostgreSQL sequence `{sequence_name}` value: {error}"))
	}

	fn rollback_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String> {
		let Self { client, transactions, .. } = self;
		transactions.rollback(target_depth, savepoint_name, |command| {
			execute_transaction_command(client, command)
		})
	}

	fn store_sequence_current(
		&mut self,
		schema_is_implicit: bool,
		schema_name: &str,
		sequence_name: &str,
		value: i64,
	) -> Result<(), String> {
		let sequence = sequence_source(schema_name, sequence_name, schema_is_implicit);
		self.client.query_one(
			"SELECT setval(CAST(CAST($1 AS TEXT) AS regclass), $2, TRUE)",
			&[&sequence, &value],
		).map_err(|error| format!("Failed to set PostgreSQL sequence `{sequence_name}`: {error}"))?;
		Ok(())
	}

	fn sync_transactions(&mut self, transaction_names: &[String]) -> Result<(), String> {
		let Self { client, transactions, .. } = self;
		transactions.synchronize(transaction_names, |command| {
			execute_transaction_command(client, command)
		})
	}

	fn update_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let write = update_record_write(&record, WriteDialect::PostgreSql)?;
		let affected_rows = execute_record_write(&mut self.client, "update", write)?;

		expect_one_affected_row("PostgreSQL", "update", affected_rows)?;
		Ok(mark_record_updated(record))
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

fn execute_record_write(client: &mut Client, operation: &str, write: RecordWrite) -> Result<u64, String> {
	let parameters = write.parameters.into_iter()
		.map(runtime_value_to_postgresql)
		.collect::<Result<Vec<_>, _>>()?;
	let parameter_refs = parameters.iter()
		.map(|value| value as &(dyn ToSql + Sync))
		.collect::<Vec<_>>();

	client.execute(&write.statement, &parameter_refs)
		.map_err(|error| format!("Failed to execute PostgreSQL {operation} statement: {error}"))
}

fn execute_transaction_command(client: &mut Client, command: TransactionCommand) -> Result<(), String> {
	let statement = transaction_statement(&command);
	client.batch_execute(&statement)
		.map_err(|error| format!("Failed to execute PostgreSQL transaction command `{statement}`: {error}"))
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

fn load_scalar_projections(
	row: &postgres::Row,
	projections: &[SqlScalarProjection],
) -> Result<BTreeMap<u32, Value>, String> {
	projections.iter().map(|projection| {
		let value = text_database_value(
			row,
			projection.column_index as usize,
			&format!("projected value {}", projection.value_id.0),
			&projection.data_type,
		)?;
		let value = database_record_field_runtime_value(
			&value,
			&projection.data_type,
			projection.data_type.is_nullable(),
		)?;
		Ok((projection.value_id.0, value))
	}).collect()
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

fn sequence_source(schema_name: &str, sequence_name: &str, schema_is_implicit: bool) -> String {
	if schema_is_implicit {
		quote_identifier(sequence_name)
	}
	else {
		format!("{}.{}", quote_identifier(schema_name), quote_identifier(sequence_name))
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

fn transaction_statement(command: &TransactionCommand) -> String {
	match command {
		TransactionCommand::Begin => String::from("BEGIN"),
		TransactionCommand::Commit => String::from("COMMIT"),
		TransactionCommand::ReleaseSavepoint(name) => format!("RELEASE SAVEPOINT {}", quote_identifier(name)),
		TransactionCommand::Rollback => String::from("ROLLBACK"),
		TransactionCommand::RollbackToSavepoint(name) => format!("ROLLBACK TO SAVEPOINT {}", quote_identifier(name)),
		TransactionCommand::Savepoint(name) => format!("SAVEPOINT {}", quote_identifier(name)),
	}
}

#[cfg(test)]
mod tests {
	use super::*;

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
	fn quotes_postgresql_sequence_sources() {
		assert_eq!(sequence_source("", "InvoiceNumber", true), "\"InvoiceNumber\"");
		assert_eq!(
			sequence_source("Accounting", "invoice\"number", false),
			"\"Accounting\".\"invoice\"\"number\"",
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
	fn renders_postgresql_transaction_commands() {
		assert_eq!(transaction_statement(&TransactionCommand::Begin), "BEGIN");
		assert_eq!(
			transaction_statement(&TransactionCommand::Savepoint(String::from("tablo_tx_1"))),
			"SAVEPOINT \"tablo_tx_1\"",
		);
		assert_eq!(
			transaction_statement(&TransactionCommand::RollbackToSavepoint(String::from("tablo_tx_1"))),
			"ROLLBACK TO SAVEPOINT \"tablo_tx_1\"",
		);
		assert_eq!(
			transaction_statement(&TransactionCommand::ReleaseSavepoint(String::from("tablo_tx_1"))),
			"RELEASE SAVEPOINT \"tablo_tx_1\"",
		);
		assert_eq!(transaction_statement(&TransactionCommand::Commit), "COMMIT");
		assert_eq!(transaction_statement(&TransactionCommand::Rollback), "ROLLBACK");
	}

	#[test]
	fn represents_null_parameter_without_a_sentinel_value() {
		assert_eq!(runtime_value_to_postgresql(Value::Null).unwrap(), None);
	}
}
