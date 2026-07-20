use std::collections::BTreeMap;

use crate::query::LoweredBackendQuery;
use crate::query::SqlQuery;
use crate::schema::DatabaseBackend;
use crate::value::RecordPointerValue;
use crate::value::Value;

use super::config::RuntimeDatabaseConfig;
use super::config::normalize_database_name;
use super::postgresql;
use super::sqlite;

pub(super) trait DatabaseDriver {
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
	sessions: BTreeMap<String, Box<dyn DatabaseDriver>>,
	transactions: Vec<TransactionScope>,
}

impl DatabaseRuntime {
	pub(crate) fn begin_transaction(&mut self) -> Result<(), String> {
		let savepoint_name = format!("tablo_tx_{}", self.next_transaction_id);
		self.next_transaction_id += 1;
		self.transactions.push(TransactionScope { savepoint_name });
		self.sync_transactions()
	}

	pub(crate) fn commit_transaction(&mut self) -> Result<(), String> {
		let transaction = self.transactions.last().cloned().ok_or_else(|| {
			String::from("Cannot commit a transaction because no transaction is active.")
		})?;
		let target_depth = self.transactions.len();

		for session in self.sessions.values_mut() {
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

				self.session_mut(&query.database_name)?.execute_query(query, parameters)
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
			sessions: BTreeMap::new(),
			transactions: Vec::new(),
		}
	}

	pub(crate) fn reset(&mut self) {
		self.next_transaction_id = 0;
		self.sessions.clear();
		self.transactions.clear();
	}

	pub(crate) fn rollback_active_transactions(&mut self) -> Result<(), String> {
		let mut first_error = None;

		while let Some(transaction) = self.transactions.last().cloned() {
			let target_depth = self.transactions.len();

			for session in self.sessions.values_mut() {
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

	fn session_mut(&mut self, database_name: &str) -> Result<&mut Box<dyn DatabaseDriver>, String> {
		let normalized_name = normalize_database_name(database_name);

		if !self.sessions.contains_key(&normalized_name) {
			let database = self.config.database(database_name).ok_or_else(|| {
				format!("Database `{database_name}` is not configured at runtime.")
			})?;
			let session = match database.backend() {
				DatabaseBackend::PostgreSql => {
					Box::new(postgresql::PostgreSqlSession::open(database_name, database.connection_string())?) as Box<dyn DatabaseDriver>
				}
				DatabaseBackend::Sqlite => {
					Box::new(sqlite::SqliteSession::open(database_name, database.connection_string())?) as Box<dyn DatabaseDriver>
				}
				backend => {
					return Err(format!(
						"Runtime database execution is not implemented yet for the `{}` backend.",
						backend.name(),
					));
				}
			};
			self.sessions.insert(normalized_name.clone(), session);
		}

		let transactions = self.transactions.iter()
			.map(|transaction| transaction.savepoint_name.clone())
			.collect::<Vec<_>>();
		let session = self.sessions.get_mut(&normalized_name)
			.expect("Database session must exist after initialization.");

		session.sync_transactions(&transactions)?;
		Ok(session)
	}

	fn sync_transactions(&mut self) -> Result<(), String> {
		let transactions = self.transactions.iter()
			.map(|transaction| transaction.savepoint_name.clone())
			.collect::<Vec<_>>();

		for session in self.sessions.values_mut() {
			session.sync_transactions(&transactions)?;
		}

		Ok(())
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct TransactionScope {
	savepoint_name: String,
}

#[cfg(test)]
mod tests {
	use crate::query::LoweredBackendQuery;
	use crate::query::SqlDialect;
	use crate::query::SqlQuery;
	use crate::query::SqlQueryResultShape;

	use super::DatabaseRuntime;
	use super::RuntimeDatabaseConfig;

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
	fn reports_missing_runtime_driver_at_the_session_boundary() {
		let config = RuntimeDatabaseConfig::new()
			.with_database_connection_string("ExampleDb", "mysql:server=localhost")
			.unwrap();
		let mut runtime = DatabaseRuntime::new(config);
		let query = test_query(SqlDialect::MySql);

		let error = runtime.execute_query(&LoweredBackendQuery::Sql(query), vec![]).unwrap_err();

		assert_eq!(
			error,
			"Runtime database execution is not implemented yet for the `mysql` backend.",
		);
	}

	fn test_query(dialect: SqlDialect) -> SqlQuery {
		SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect,
			group_by: vec![],
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from("SELECT 1"),
			table_name: String::new(),
		}
	}
}
