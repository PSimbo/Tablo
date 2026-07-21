use std::collections::BTreeMap;
use std::path::PathBuf;

use rusqlite::Connection;
use rusqlite::params_from_iter;
use rusqlite::types::Value as SqlValue;
use rusqlite::types::ValueRef;

use crate::query::*;
use crate::sql::quote_identifier;
use crate::value::*;

use super::records::*;
use super::runtime::DatabaseDriver;
use super::transactions::*;
use super::values::runtime_type_name;
use super::writes::*;

pub(super) struct SqliteSession {
	connection: Connection,
	transactions: TransactionState,
}

impl SqliteSession {
	pub fn open(database_name: &str, connection_string: &str) -> Result<Self, String> {
		let path = path_from_connection_string(database_name, connection_string)?;
		let connection = Connection::open(&path).map_err(|error| {
			format!("Failed to open SQLite database `{}`: {error}", path.display())
		})?;

		Ok(Self {
			connection,
			transactions: TransactionState::default(),
		})
	}
}

impl DatabaseDriver for SqliteSession {
	fn advance_sequence(&mut self, schema_is_implicit: bool, schema_name: &str, sequence_name: &str) -> Result<i64, String> {
		let sequence_source = sequence_source(schema_name, schema_is_implicit);
		let statement = format!(
			"UPDATE {} SET seq = CAST(seq AS INTEGER) + 1 WHERE name = ?1 AND CAST(seq AS INTEGER) < ?2 RETURNING seq",
			sequence_source,
		);
		let advance_existing = |connection: &Connection| connection.query_row(&statement, (sequence_name, i64::MAX), |row| {
			read_sqlite_integer(row, 0)
		});

		match advance_existing(&self.connection) {
			Ok(value) => Ok(value),
			Err(rusqlite::Error::QueryReturnedNoRows) => {
				let initialize_statement = format!(
					"INSERT INTO {sequence_source} (name, seq) SELECT ?1, 1 WHERE NOT EXISTS (SELECT 1 FROM {sequence_source} WHERE name = ?1) RETURNING seq",
				);
				match self.connection.query_row(&initialize_statement, [sequence_name], |row| read_sqlite_integer(row, 0)) {
					Ok(value) => Ok(value),
					Err(rusqlite::Error::QueryReturnedNoRows) => match advance_existing(&self.connection) {
						Ok(value) => Ok(value),
						Err(rusqlite::Error::QueryReturnedNoRows) => {
							let current = self.load_sequence_current(schema_is_implicit, schema_name, sequence_name)?;
							if current == i64::MAX {
								Err(format!("Advancing SQLite sequence `{sequence_name}` would overflow the supported `int` range."))
							}
							else {
								Err(format!("SQLite sequence `{sequence_name}` changed concurrently and could not be advanced."))
							}
						}
						Err(error) => Err(format!("Failed to advance SQLite sequence `{sequence_name}`: {error}")),
					},
					Err(error) => Err(format!("Failed to initialize SQLite sequence `{sequence_name}`: {error}")),
				}
			}
			Err(error) => Err(format!("Failed to advance SQLite sequence `{sequence_name}`: {error}")),
		}
	}

	fn commit_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String> {
		let Self { connection, transactions } = self;
		transactions.commit(target_depth, savepoint_name, |command| {
			execute_transaction_command(connection, command)
		})
	}

	fn create_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let write = create_record_write(&record, WriteDialect::Sqlite)?;
		let parameter_values = write.parameters.into_iter()
			.map(runtime_value_to_sqlite)
			.collect::<Result<Vec<_>, _>>()?;
		let affected_rows = self.connection.execute(&write.statement, params_from_iter(parameter_values))
			.map_err(|error| format!("Failed to execute SQLite create statement: {error}"))?;

		expect_one_affected_row("SQLite", "create", affected_rows as u64)?;
		Ok(mark_record_created(record))
	}

	fn delete_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let write = delete_record_write(&record, WriteDialect::Sqlite)?;
		let parameter_values = write.parameters.into_iter()
			.map(runtime_value_to_sqlite)
			.collect::<Result<Vec<_>, _>>()?;
		let affected_rows = self.connection.execute(&write.statement, params_from_iter(parameter_values))
			.map_err(|error| format!("Failed to execute SQLite delete statement: {error}"))?;

		expect_one_affected_row("SQLite", "delete", affected_rows as u64)?;
		Ok(mark_record_deleted(record))
	}

	fn execute_query(&mut self, query: &SqlQuery, parameters: Vec<Value>) -> Result<Value, String> {
		let parameters = parameters.into_iter().map(runtime_value_to_sqlite).collect::<Result<Vec<_>, _>>()?;
		let mut statement = self.connection.prepare(&query.statement)
			.map_err(|error| format!("Failed to prepare SQLite query: {error}"))?;

		match &query.result_shape {
			SqlQueryResultShape::IntegerScalar => {
				let result = statement.query_row(params_from_iter(parameters), |row| row.get::<_, i64>(0))
					.map_err(|error| format!("Failed to execute SQLite query: {error}"))?;
				Ok(Value::Integer(result))
			}
			SqlQueryResultShape::RecordPointer(layout) => {
				let schema = known_record_schema(layout)?;
				let selected_columns = selected_record_columns(layout)?;
				let mut rows = statement.query(params_from_iter(parameters))
					.map_err(|error| format!("Failed to execute SQLite query: {error}"))?;
				let Some(row) = rows.next().map_err(|error| format!("Failed to read SQLite query result: {error}"))? else {
					return Ok(Value::RecordPointer(empty_record_pointer(query, schema)));
				};
				let fields = load_record_fields(row, &selected_columns)?;
				let original_fields = fields.clone();
				Ok(Value::RecordPointer(record_pointer(query, schema, fields, original_fields, BTreeMap::new())))
			}
			SqlQueryResultShape::RecordPointerArray(layout) => {
				let schema = known_record_schema(layout)?;
				let selected_columns = selected_record_columns(layout)?;
				let mut rows = statement.query(params_from_iter(parameters))
					.map_err(|error| format!("Failed to execute SQLite query: {error}"))?;
				let mut loaded_records = Vec::new();

				while let Some(row) = rows.next().map_err(|error| format!("Failed to read SQLite query result: {error}"))? {
					let fields = load_record_fields(row, &selected_columns)?;
					loaded_records.push(LoadedRecord {
						group_keys: load_group_keys(row, selected_columns.len(), &query.group_by)?,
						original_fields: fields.clone(),
						fields,
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
					)))
					.collect();
				Ok(Value::Array(records))
			}
		}
	}

	fn load_sequence_current(&mut self, schema_is_implicit: bool, schema_name: &str, sequence_name: &str) -> Result<i64, String> {
		let statement = format!("SELECT seq FROM {} WHERE name = ?1", sequence_source(schema_name, schema_is_implicit));
		let result = self.connection.query_row(&statement, [sequence_name], |row| read_sqlite_integer(row, 0));

		match result {
			Ok(value) => Ok(value),
			Err(rusqlite::Error::QueryReturnedNoRows) => Ok(0),
			Err(error) => Err(format!("Failed to read SQLite sequence `{sequence_name}`: {error}")),
		}
	}

	fn rollback_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String> {
		let Self { connection, transactions } = self;
		transactions.rollback(target_depth, savepoint_name, |command| {
			execute_transaction_command(connection, command)
		})
	}

	fn store_sequence_current(
		&mut self,
		schema_is_implicit: bool,
		schema_name: &str,
		sequence_name: &str,
		value: i64,
	) -> Result<(), String> {
		let sequence_source = sequence_source(schema_name, schema_is_implicit);
		let update_statement = format!("UPDATE {sequence_source} SET seq = ?1 WHERE name = ?2");
		let updated_rows = self.connection.execute(&update_statement, (&value, sequence_name))
			.map_err(|error| format!("Failed to update SQLite sequence `{sequence_name}`: {error}"))?;

		if updated_rows == 0 {
			let insert_statement = format!("INSERT INTO {sequence_source} (name, seq) VALUES (?1, ?2)");
			self.connection.execute(&insert_statement, (sequence_name, value))
				.map_err(|error| format!("Failed to initialize SQLite sequence `{sequence_name}`: {error}"))?;
		}

		Ok(())
	}

	fn sync_transactions(&mut self, transaction_names: &[String]) -> Result<(), String> {
		let Self { connection, transactions } = self;
		transactions.synchronize(transaction_names, |command| {
			execute_transaction_command(connection, command)
		})
	}

	fn update_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let write = update_record_write(&record, WriteDialect::Sqlite)?;
		let parameter_values = write.parameters.into_iter()
			.map(runtime_value_to_sqlite)
			.collect::<Result<Vec<_>, _>>()?;
		let affected_rows = self.connection.execute(&write.statement, params_from_iter(parameter_values))
			.map_err(|error| format!("Failed to execute SQLite update statement: {error}"))?;

		expect_one_affected_row("SQLite", "update", affected_rows as u64)?;
		Ok(mark_record_updated(record))
	}
}

fn database_value(value: ValueRef<'_>) -> Result<DatabaseValue, String> {
	match value {
		ValueRef::Blob(value) => Ok(DatabaseValue::Blob(value.to_vec())),
		ValueRef::Integer(value) => Ok(DatabaseValue::Integer(value)),
		ValueRef::Null => Ok(DatabaseValue::Null),
		ValueRef::Real(value) => Ok(DatabaseValue::Real(value.to_string())),
		ValueRef::Text(value) => Ok(DatabaseValue::Text(std::str::from_utf8(value)
			.map_err(|_| String::from("SQLite returned invalid UTF-8 text data."))?.to_string())),
	}
}

fn execute_transaction_command(connection: &mut Connection, command: TransactionCommand) -> Result<(), String> {
	let statement = transaction_statement(&command);
	connection.execute_batch(&statement)
		.map_err(|error| format!("Failed to execute SQLite transaction command `{statement}`: {error}"))
}

fn load_group_keys(row: &rusqlite::Row<'_>, column_count: usize, group_by: &[SqlGroupByItem]) -> Result<Vec<Value>, String> {
	group_by.iter().enumerate().map(|(index, item)| {
		let value = row.get_ref(column_count + index).map_err(|error| format!("Failed to read SQLite grouping column: {error}"))?;
		let value = database_value(value)?;
		database_record_field_runtime_value(&value, &item.data_type, item.data_type.is_nullable())
	}).collect()
}

fn load_record_fields(row: &rusqlite::Row<'_>, columns: &[QueryResultColumn]) -> Result<BTreeMap<String, RecordFieldValue>, String> {
	let mut fields = BTreeMap::new();

	for (index, column) in columns.iter().enumerate() {
		let value = row.get_ref(index).map_err(|error| format!("Failed to read SQLite column `{}`: {error}", column.column_name))?;
		fields.insert(normalize_name(&column.column_name), RecordFieldValue::Deferred {
			data_type: column.data_type.clone(),
			is_nullable: column.is_nullable,
			value: database_value(value)?,
		});
	}

	Ok(fields)
}

fn path_from_connection_string(database_name: &str, connection_string: &str) -> Result<PathBuf, String> {
	let value = connection_string.strip_prefix("sqlite:").ok_or_else(|| {
		format!("SQLite database `{database_name}` has an invalid runtime connection string.")
	})?;

	if value.is_empty() {
		return Err(format!(
			"SQLite connection string for database `{database_name}` must include a database path."
		));
	}

	if value.starts_with("//") && !value.starts_with("///") {
		return Err(format!(
			"SQLite connection string for database `{database_name}` must use `sqlite:/path/to.db`, `sqlite:relative.db`, or `sqlite::memory:`."
		));
	}

	Ok(PathBuf::from(if value.starts_with("///") { &value[2..] } else { value }))
}

fn read_sqlite_integer(row: &rusqlite::Row<'_>, index: usize) -> rusqlite::Result<i64> {
	let value = row.get_ref(index)?;
	sqlite_integer(value).map_err(|message| rusqlite::Error::FromSqlConversionFailure(
		index,
		value.data_type(),
		Box::new(std::io::Error::new(std::io::ErrorKind::InvalidData, message)),
	))
}

fn runtime_value_to_sqlite(value: Value) -> Result<SqlValue, String> {
	match value {
		Value::Boolean(value) => Ok(SqlValue::Integer(if value { 1 } else { 0 })),
		Value::Date(value) => Ok(SqlValue::Text(value.to_string())),
		Value::Decimal(value) => Ok(SqlValue::Text(value.to_string())),
		Value::Integer(value) => Ok(SqlValue::Integer(value)),
		Value::Null => Ok(SqlValue::Null),
		Value::Text(value) => Ok(SqlValue::Text(value)),
		Value::Time(value) => Ok(SqlValue::Text(value.to_string())),
		Value::TimeTz(value) => Ok(SqlValue::Text(value.to_string())),
		Value::Timestamp(value) => Ok(SqlValue::Text(value.to_string())),
		Value::TimestampTz(value) => Ok(SqlValue::Text(value.to_string())),
		other => Err(format!("Cannot bind a `{}` value into a SQLite query parameter.", runtime_type_name(&other))),
	}
}

fn sequence_source(schema_name: &str, schema_is_implicit: bool) -> String {
	if schema_is_implicit { String::from("sqlite_sequence") } else { format!("{}.sqlite_sequence", quote_identifier(schema_name)) }
}

fn sqlite_integer(value: ValueRef<'_>) -> Result<i64, String> {
	match value {
		ValueRef::Integer(value) => Ok(value),
		ValueRef::Text(value) => {
			let text = std::str::from_utf8(value).map_err(|_| String::from("SQLite text value is not valid UTF-8."))?;
			text.parse::<i64>().map_err(|_| format!("SQLite value `{text}` cannot be converted to `int`."))
		}
		other => Err(format!("SQLite {} value cannot be converted to `int`.", sqlite_type_name(other))),
	}
}

fn sqlite_type_name(value: ValueRef<'_>) -> &'static str {
	match value {
		ValueRef::Blob(_) => "blob",
		ValueRef::Integer(_) => "integer",
		ValueRef::Null => "null",
		ValueRef::Real(_) => "real",
		ValueRef::Text(_) => "text",
	}
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
	use std::path::Path;

	use super::*;

	use crate::ast::DataType;

	#[test]
	fn advances_sqlite_sequence_stored_as_text() {
		let connection = Connection::open_in_memory().unwrap();
		connection.execute_batch(
			r#"
				CREATE TABLE InvoiceNumber (Id INTEGER PRIMARY KEY AUTOINCREMENT);
				INSERT INTO InvoiceNumber DEFAULT VALUES;
				UPDATE sqlite_sequence SET seq = CAST('41' AS TEXT) WHERE name = 'InvoiceNumber';
			"#,
		).unwrap();
		let mut session = SqliteSession { connection, transactions: TransactionState::default() };

		assert_eq!(session.advance_sequence(true, "", "InvoiceNumber").unwrap(), 42);
		assert_eq!(session.load_sequence_current(true, "", "InvoiceNumber").unwrap(), 42);
	}

	#[test]
	fn atomically_initializes_missing_sqlite_sequence() {
		let connection = Connection::open_in_memory().unwrap();
		connection.execute_batch(
			"CREATE TABLE InvoiceNumber (Id INTEGER PRIMARY KEY AUTOINCREMENT);",
		).unwrap();
		let mut session = SqliteSession { connection, transactions: TransactionState::default() };

		assert_eq!(session.advance_sequence(true, "", "InvoiceNumber").unwrap(), 1);
		assert_eq!(session.load_sequence_current(true, "", "InvoiceNumber").unwrap(), 1);
	}

	#[test]
	fn commits_outer_work_after_rolling_back_inner_savepoint() {
		let connection = Connection::open_in_memory().unwrap();
		connection.execute_batch("CREATE TABLE Events (Id INTEGER NOT NULL);").unwrap();
		let mut session = SqliteSession { connection, transactions: TransactionState::default() };

		session.sync_transactions(&[String::from("outer")]).unwrap();
		session.connection.execute("INSERT INTO Events (Id) VALUES (1)", []).unwrap();
		session.sync_transactions(&[String::from("outer"), String::from("inner")]).unwrap();
		session.connection.execute("INSERT INTO Events (Id) VALUES (2)", []).unwrap();
		session.rollback_transaction(2, "inner").unwrap();
		session.commit_transaction(1, "outer").unwrap();

		let ids = session.connection.prepare("SELECT Id FROM Events ORDER BY Id").unwrap()
			.query_map([], |row| row.get::<_, i64>(0)).unwrap()
			.collect::<Result<Vec<_>, _>>().unwrap();
		assert_eq!(ids, vec![1]);
	}

	#[test]
	fn loads_selected_fields_while_preserving_complete_record_schema() {
		let connection = Connection::open_in_memory().unwrap();
		connection.execute_batch(
			"CREATE TABLE Customers (Id INTEGER PRIMARY KEY, Name TEXT NOT NULL); INSERT INTO Customers VALUES (1, 'Acme');",
		).unwrap();
		let mut session = SqliteSession { connection, transactions: TransactionState::default() };
		let query = SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			group_by: vec![],
			lock_mode: RecordLockMode::None,
			parameters: vec![],
			result_shape: SqlQueryResultShape::RecordPointer(QueryRecordLayout {
				schema: QueryRecordSchema::Known(vec![
					QueryResultColumn {
						column_name: String::from("Id"),
						data_type: DataType::Int,
						is_nullable: false,
						is_primary_key: true,
					},
					QueryResultColumn {
						column_name: String::from("Name"),
						data_type: DataType::Text,
						is_nullable: false,
						is_primary_key: false,
					},
				]),
				selection: QueryColumnSelection::Indices(vec![1]),
			}),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from("SELECT Name FROM Customers LIMIT 1"),
			table_name: String::from("Customers"),
		};

		let Value::RecordPointer(record) = session.execute_query(&query, vec![]).unwrap() else {
			panic!("Expected a record pointer.");
		};

		assert_eq!(record.column_names, vec![String::from("Id"), String::from("Name")]);
		assert_eq!(record.primary_key_column_names, vec![String::from("Id")]);
		assert!(!record.fields.contains_key("id"));
		assert_eq!(record.fields.get("name").unwrap().materialize().unwrap(), Value::Text(String::from("Acme")));
	}

	#[test]
	fn outer_rollback_discards_committed_inner_savepoint_work() {
		let connection = Connection::open_in_memory().unwrap();
		connection.execute_batch("CREATE TABLE Events (Id INTEGER NOT NULL);").unwrap();
		let mut session = SqliteSession { connection, transactions: TransactionState::default() };

		session.sync_transactions(&[String::from("outer")]).unwrap();
		session.connection.execute("INSERT INTO Events (Id) VALUES (1)", []).unwrap();
		session.sync_transactions(&[String::from("outer"), String::from("inner")]).unwrap();
		session.connection.execute("INSERT INTO Events (Id) VALUES (2)", []).unwrap();
		session.commit_transaction(2, "inner").unwrap();
		session.rollback_transaction(1, "outer").unwrap();

		let count = session.connection.query_row("SELECT COUNT(*) FROM Events", [], |row| row.get::<_, i64>(0)).unwrap();
		assert_eq!(count, 0);
	}

	#[test]
	fn parses_absolute_connection_path() {
		assert_eq!(
			path_from_connection_string("ExampleDb", "sqlite:///tmp/example.sqlite").unwrap(),
			Path::new("/tmp/example.sqlite"),
		);
	}

	#[test]
	fn parses_relative_connection_path() {
		assert_eq!(
			path_from_connection_string("ExampleDb", "sqlite:db/example.sqlite").unwrap(),
			Path::new("db/example.sqlite"),
		);
	}

	#[test]
	fn rejects_invalid_connection_path_shape() {
		let error = path_from_connection_string("ExampleDb", "sqlite://example.sqlite").unwrap_err();

		assert_eq!(
			error,
			"SQLite connection string for database `ExampleDb` must use `sqlite:/path/to.db`, `sqlite:relative.db`, or `sqlite::memory:`.",
		);
	}

	#[test]
	fn rejects_sqlite_sequence_overflow() {
		let connection = Connection::open_in_memory().unwrap();
		connection.execute_batch(&format!(
			r#"
				CREATE TABLE InvoiceNumber (Id INTEGER PRIMARY KEY AUTOINCREMENT);
				INSERT INTO InvoiceNumber DEFAULT VALUES;
				UPDATE sqlite_sequence SET seq = {} WHERE name = 'InvoiceNumber';
			"#,
			i64::MAX,
		)).unwrap();
		let mut session = SqliteSession { connection, transactions: TransactionState::default() };

		assert_eq!(
			session.advance_sequence(true, "", "InvoiceNumber").unwrap_err(),
			"Advancing SQLite sequence `InvoiceNumber` would overflow the supported `int` range.",
		);
	}
}
