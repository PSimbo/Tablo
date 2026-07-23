use std::collections::BTreeMap;

use crate::query::{ LoweredBackendQuery, SqlQuery };
use crate::schema::DatabaseBackend;
use crate::value::{ RecordPointerValue, Value };

use super::config::*;
use super::{ mysql, postgresql, sqlite };

pub(super) trait DatabaseDriver {
	fn advance_sequence(&mut self, schema_is_implicit: bool, schema_name: &str, sequence_name: &str) -> Result<i64, String>;
	fn commit_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String>;
	fn create_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String>;
	fn delete_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String>;
	fn execute_query(&mut self, query: &SqlQuery, parameters: Vec<Value>) -> Result<Value, String>;
	fn load_sequence_current(&mut self, schema_is_implicit: bool, schema_name: &str, sequence_name: &str) -> Result<i64, String>;
	fn rollback_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String>;
	fn store_sequence_current(
		&mut self,
		schema_is_implicit: bool,
		schema_name: &str,
		sequence_name: &str,
		value: i64,
	) -> Result<(), String>;
	fn sync_transactions(&mut self, transaction_names: &[String]) -> Result<(), String>;
	fn update_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String>;
}

pub(crate) struct DatabaseRuntime {
	config: RuntimeDatabaseConfig,
	next_transaction_id: u64,
	#[cfg(test)]
	query_execution_count: usize,
	sessions: BTreeMap<String, Box<dyn DatabaseDriver>>,
	transactions: Vec<TransactionScope>,
}

impl DatabaseRuntime {
	pub(crate) fn advance_sequence(
		&mut self,
		database_name: &str,
		schema_is_implicit: bool,
		schema_name: &str,
		sequence_name: &str,
	) -> Result<i64, String> {
		self.session_mut(database_name)?.advance_sequence(schema_is_implicit, schema_name, sequence_name)
	}

	pub(crate) fn begin_transaction(&mut self) -> Result<(), String> {
		let savepoint_name = format!("tablo_tx_{}", self.next_transaction_id);
		self.next_transaction_id += 1;
		self.transactions.push(TransactionScope {
			database_name: None,
			savepoint_name,
		});
		Ok(())
	}

	pub(crate) fn commit_transaction(&mut self) -> Result<(), String> {
		let transaction = self.transactions.last().cloned().ok_or_else(|| {
			String::from("Cannot commit a transaction because no transaction is active.")
		})?;
		let target_depth = self.transactions.len();

		if let Some(database_name) = self.transaction_database_name() {
			let database_key = normalize_database_name(&database_name);
			let session = self.sessions.get_mut(&database_key)
				.expect("Transaction database session must exist after its first use.");
			session.commit_transaction(target_depth, &transaction.savepoint_name)?;
		}

		self.transactions.pop();
		Ok(())
	}

	pub(crate) fn create_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		self.session_mut(&record.record_type.database_name)?.create_record(record)
	}

	pub(crate) fn delete_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		self.session_mut(&record.record_type.database_name)?.delete_record(record)
	}

	pub(crate) fn execute_query(&mut self, query: &LoweredBackendQuery, parameters: Vec<Value>) -> Result<Value, String> {
		match query {
			LoweredBackendQuery::Sql(query) => {
				let configured_backend = self.config.database(&query.database_name)
					.map(|database| database.backend());
				let query_backend = query.dialect.backend();

				if let Some(configured_backend) = configured_backend {
					if configured_backend != query_backend {
						return Err(format!(
							"Compiled query uses the `{}` backend, but database `{}` is configured for `{}`.",
							query_backend.name(),
							query.database_name,
							configured_backend.name(),
						));
					}
				}

				let result = self.session_mut(&query.database_name)?.execute_query(query, parameters);
				#[cfg(test)]
				{
					self.query_execution_count += 1;
				}
				result
			}
		}
	}

	pub(crate) fn load_sequence_current(
		&mut self,
		database_name: &str,
		schema_is_implicit: bool,
		schema_name: &str,
		sequence_name: &str,
	) -> Result<i64, String> {
		self.session_mut(database_name)?.load_sequence_current(schema_is_implicit, schema_name, sequence_name)
	}

	pub(crate) fn new(config: RuntimeDatabaseConfig) -> Self {
		Self {
			config,
			next_transaction_id: 0,
			#[cfg(test)]
			query_execution_count: 0,
			sessions: BTreeMap::new(),
			transactions: Vec::new(),
		}
	}

	#[cfg(test)]
	pub(crate) fn query_execution_count(&self) -> usize {
		self.query_execution_count
	}

	pub(crate) fn reset(&mut self) {
		self.next_transaction_id = 0;
		#[cfg(test)]
		{
			self.query_execution_count = 0;
		}
		self.sessions.clear();
		self.transactions.clear();
	}

	pub(crate) fn rollback_active_transactions(&mut self) -> Result<(), String> {
		let mut first_error = None;

		while let Some(transaction) = self.transactions.last().cloned() {
			let target_depth = self.transactions.len();

			if let Some(database_name) = self.transaction_database_name() {
				let database_key = normalize_database_name(&database_name);
				let session = self.sessions.get_mut(&database_key)
					.expect("Transaction database session must exist after its first use.");
				let result = session.rollback_transaction(target_depth, &transaction.savepoint_name);

				if let Err(error) = result {
					if first_error.is_none() {
						first_error = Some(error);
					}
				}
			}

			self.transactions.pop();
		}

		match first_error {
			Some(error) => Err(error),
			None => Ok(()),
		}
	}

	pub(crate) fn store_sequence_current(
		&mut self,
		database_name: &str,
		schema_is_implicit: bool,
		schema_name: &str,
		sequence_name: &str,
		value: i64,
	) -> Result<(), String> {
		self.session_mut(database_name)?.store_sequence_current(schema_is_implicit, schema_name, sequence_name, value)
	}

	pub(crate) fn update_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		self.session_mut(&record.record_type.database_name)?.update_record(record)
	}

	fn claim_transaction_database(&mut self, database_name: &str) {
		let Some(transaction) = self.transactions.first_mut() else {
			return;
		};

		if transaction.database_name.is_none() {
			transaction.database_name = Some(database_name.to_string());
		}
	}

	fn session_mut(&mut self, database_name: &str) -> Result<&mut Box<dyn DatabaseDriver>, String> {
		let normalized_name = normalize_database_name(database_name);
		self.validate_transaction_database(database_name)?;

		if !self.sessions.contains_key(&normalized_name) {
			let database = self.config.database(database_name).ok_or_else(|| {
				format!("Database `{database_name}` is not configured at runtime.")
			})?;
			let session = match database.backend() {
				DatabaseBackend::MySql => {
					Box::new(mysql::MySqlSession::open(database_name, database.connection_string())?) as Box<dyn DatabaseDriver>
				}
				DatabaseBackend::PostgreSql => {
					Box::new(postgresql::PostgreSqlSession::open(database_name, database.connection_string())?) as Box<dyn DatabaseDriver>
				}
				DatabaseBackend::Sqlite => {
					Box::new(sqlite::SqliteSession::open(database_name, database.connection_string())?) as Box<dyn DatabaseDriver>
				}
			};
			self.sessions.insert(normalized_name.clone(), session);
		}
		self.claim_transaction_database(database_name);

		let transactions = self.transactions.iter()
			.map(|transaction| transaction.savepoint_name.clone())
			.collect::<Vec<_>>();
		let session = self.sessions.get_mut(&normalized_name)
			.expect("Database session must exist after initialization.");

		session.sync_transactions(&transactions)?;
		Ok(session)
	}

	fn transaction_database_name(&self) -> Option<String> {
		self.transactions.first()
			.and_then(|transaction| transaction.database_name.clone())
	}

	fn validate_transaction_database(&self, database_name: &str) -> Result<(), String> {
		let Some(active_database_name) = self.transaction_database_name() else {
			return Ok(());
		};

		if normalize_database_name(&active_database_name) != normalize_database_name(database_name) {
			return Err(format!(
				"Transaction cannot access database `{database_name}` because it already accesses database `{active_database_name}`. Transactions spanning multiple databases are not supported.",
			));
		}

		Ok(())
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct TransactionScope {
	database_name: Option<String>,
	savepoint_name: String,
}

#[cfg(test)]
mod tests {
	use super::*;

	use crate::query::*;

	#[test]
	fn allows_sequential_transactions_to_use_different_databases() {
		let config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("PrimaryDb", ":memory:")
			.with_sqlite_database("ArchiveDb", ":memory:");
		let mut runtime = DatabaseRuntime::new(config);

		runtime.begin_transaction().unwrap();
		runtime.execute_query(&LoweredBackendQuery::Sql(test_query_for_database("PrimaryDb", SqlDialect::Sqlite)), vec![]).unwrap();
		runtime.commit_transaction().unwrap();
		runtime.begin_transaction().unwrap();
		runtime.execute_query(&LoweredBackendQuery::Sql(test_query_for_database("ArchiveDb", SqlDialect::Sqlite)), vec![]).unwrap();
		runtime.commit_transaction().unwrap();
	}

	#[test]
	fn ignores_previously_opened_database_sessions_when_transaction_begins() {
		let config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("PrimaryDb", ":memory:")
			.with_sqlite_database("ArchiveDb", ":memory:");
		let mut runtime = DatabaseRuntime::new(config);
		runtime.execute_query(&LoweredBackendQuery::Sql(test_query_for_database("PrimaryDb", SqlDialect::Sqlite)), vec![]).unwrap();
		runtime.execute_query(&LoweredBackendQuery::Sql(test_query_for_database("ArchiveDb", SqlDialect::Sqlite)), vec![]).unwrap();

		runtime.begin_transaction().unwrap();
		runtime.execute_query(&LoweredBackendQuery::Sql(test_query_for_database("PrimaryDb", SqlDialect::Sqlite)), vec![]).unwrap();
		let error = runtime.execute_query(
			&LoweredBackendQuery::Sql(test_query_for_database("ArchiveDb", SqlDialect::Sqlite)),
			vec![],
		).unwrap_err();
		runtime.rollback_active_transactions().unwrap();

		assert!(error.contains("Transactions spanning multiple databases are not supported."));
	}

	#[test]
	fn rejects_execution_when_compiled_and_configured_backends_differ() {
		let config = RuntimeDatabaseConfig::new().with_sqlite_database("ExampleDb", ":memory:");
		let mut runtime = DatabaseRuntime::new(config);
		let query = test_query(SqlDialect::PostgreSql);

		let error = runtime.execute_query(&LoweredBackendQuery::Sql(query), vec![]).unwrap_err();

		assert_eq!(
			error,
			"Compiled query uses the `postgresql` backend, but database `ExampleDb` is configured for `sqlite`.",
		);
	}

	#[test]
	fn rejects_second_database_within_nested_transaction_scope() {
		let config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("PrimaryDb", ":memory:")
			.with_sqlite_database("ArchiveDb", ":memory:");
		let mut runtime = DatabaseRuntime::new(config);
		runtime.begin_transaction().unwrap();
		runtime.execute_query(&LoweredBackendQuery::Sql(test_query_for_database("PrimaryDb", SqlDialect::Sqlite)), vec![]).unwrap();
		runtime.begin_transaction().unwrap();

		let error = runtime.execute_query(
			&LoweredBackendQuery::Sql(test_query_for_database("ArchiveDb", SqlDialect::Sqlite)),
			vec![],
		).unwrap_err();
		runtime.rollback_active_transactions().unwrap();

		assert_eq!(
			error,
			"Transaction cannot access database `ArchiveDb` because it already accesses database `PrimaryDb`. Transactions spanning multiple databases are not supported.",
		);
	}

	#[test]
	fn reports_invalid_mysql_connection_string_at_the_session_boundary() {
		let config = RuntimeDatabaseConfig::new()
			.with_database_connection_string("ExampleDb", "mysql:server=localhost")
			.unwrap();
		let mut runtime = DatabaseRuntime::new(config);
		let query = test_query(SqlDialect::MySql);

		let error = runtime.execute_query(&LoweredBackendQuery::Sql(query), vec![]).unwrap_err();

		assert_eq!(
			error,
			"MySQL connection string for database `ExampleDb` must use `mysql://user:password@host/database` format.",
		);
	}

	#[test]
	fn rolls_back_empty_transaction_after_first_database_fails_to_open() {
		let mut runtime = DatabaseRuntime::new(RuntimeDatabaseConfig::new());
		runtime.begin_transaction().unwrap();

		let error = runtime.execute_query(
			&LoweredBackendQuery::Sql(test_query_for_database("MissingDb", SqlDialect::Sqlite)),
			vec![],
		).unwrap_err();

		assert_eq!(error, "Database `MissingDb` is not configured at runtime.");
		runtime.rollback_active_transactions().unwrap();
	}

	fn test_query(dialect: SqlDialect) -> SqlQuery {
		test_query_for_database("ExampleDb", dialect)
	}

	fn test_query_for_database(database_name: &str, dialect: SqlDialect) -> SqlQuery {
		SqlQuery {
			database_name: database_name.to_string(),
			dialect,
			group_by: vec![],
			lock_mode: RecordLockMode::None,
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			scalar_projections: vec![],
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from("SELECT 1"),
			table_name: String::new(),
		}
	}
}
