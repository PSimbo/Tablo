use std::collections::BTreeMap;
use std::path::PathBuf;

use crate::schema::DatabaseBackend;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DatabaseConnectionConfig {
	backend: DatabaseBackend,
	connection_string: String,
}

impl DatabaseConnectionConfig {
	pub fn backend(&self) -> DatabaseBackend {
		self.backend
	}

	pub fn connection_string(&self) -> &str {
		&self.connection_string
	}
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct RuntimeDatabaseConfig {
	databases: BTreeMap<String, DatabaseConnectionConfig>,
}

impl RuntimeDatabaseConfig {
	pub fn database(&self, database_name: &str) -> Option<&DatabaseConnectionConfig> {
		self.databases.get(&normalize_database_name(database_name))
	}

	pub fn new() -> Self {
		Self::default()
	}

	pub fn set_database_connection_string(
		&mut self,
		database_name: impl Into<String>,
		connection_string: &str,
	) -> Result<(), String> {
		let database_name = database_name.into();
		let backend = DatabaseBackend::from_connection_string(connection_string)
			.map_err(|message| format!("Invalid connection string for database `{database_name}`: {message}"))?;

		self.databases.insert(
			normalize_database_name(&database_name),
			DatabaseConnectionConfig {
				backend,
				connection_string: connection_string.to_string(),
			},
		);
		Ok(())
	}

	pub fn set_sqlite_database(&mut self, database_name: impl Into<String>, path: impl Into<PathBuf>) {
		let database_name = database_name.into();
		let path = path.into();
		self.databases.insert(
			normalize_database_name(&database_name),
			DatabaseConnectionConfig {
				backend: DatabaseBackend::Sqlite,
				connection_string: format!("sqlite:{}", path.display()),
			},
		);
	}

	pub fn with_database_connection_string(
		mut self,
		database_name: impl Into<String>,
		connection_string: &str,
	) -> Result<Self, String> {
		self.set_database_connection_string(database_name, connection_string)?;
		Ok(self)
	}

	pub fn with_sqlite_database(mut self, database_name: impl Into<String>, path: impl Into<PathBuf>) -> Self {
		self.set_sqlite_database(database_name, path);
		self
	}
}

pub(super) fn normalize_database_name(name: &str) -> String {
	name.to_ascii_lowercase()
}

#[cfg(test)]
mod tests {
	use crate::schema::DatabaseBackend;

	use super::RuntimeDatabaseConfig;

	#[test]
	fn preserves_non_sqlite_database_configuration_before_driver_support_exists() {
		let config = RuntimeDatabaseConfig::new()
			.with_database_connection_string("ExampleDb", "postgresql:host=localhost")
			.unwrap();
		let database = config.database("exampledb").unwrap();

		assert_eq!(database.backend(), DatabaseBackend::PostgreSql);
		assert_eq!(database.connection_string(), "postgresql:host=localhost");
	}

	#[test]
	fn rejects_unknown_backend_scheme() {
		let error = RuntimeDatabaseConfig::new()
			.with_database_connection_string("ExampleDb", "oracle:host=localhost")
			.unwrap_err();

		assert_eq!(
			error,
			"Invalid connection string for database `ExampleDb`: unsupported backend `oracle`.",
		);
	}
}
