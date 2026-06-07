use std::collections::BTreeMap;
use std::path::Path;

use serde::Deserialize;

use crate::vm::RuntimeDatabaseConfig;

#[derive(Debug, Eq, PartialEq)]
pub struct RuntimeConfigError {
	pub message: String,
}

#[derive(Deserialize)]
struct RuntimeConfigFile {
	databases: Option<BTreeMap<String, String>>,
}

impl RuntimeConfigFile {
	fn into_runtime_database_config(self) -> Result<RuntimeDatabaseConfig, RuntimeConfigError> {
		let mut config = RuntimeDatabaseConfig::new();

		for (database_name, connection_string) in self.databases.unwrap_or_default() {
			config.set_database_connection_string(database_name.as_str(), connection_string.as_str())
				.map_err(|message| RuntimeConfigError { message })?;
		}

		Ok(config)
	}
}

pub fn read_runtime_database_config_from_path(path: impl AsRef<Path>) -> Result<RuntimeDatabaseConfig, RuntimeConfigError> {
	let contents = std::fs::read_to_string(path).map_err(|error| RuntimeConfigError {
		message: format!("Failed to read runtime config file: {error}"),
	})?;

	read_runtime_database_config_from_str(&contents)
}

pub fn read_runtime_database_config_from_str(contents: &str) -> Result<RuntimeDatabaseConfig, RuntimeConfigError> {
	let config_file: RuntimeConfigFile = toml::from_str(contents).map_err(|error| RuntimeConfigError {
		message: format!("Invalid runtime config TOML: {error}"),
	})?;

	config_file.into_runtime_database_config()
}

#[cfg(test)]
mod tests {
	use std::path::Path;

	use super::read_runtime_database_config_from_str;
	use super::RuntimeConfigError;

	#[test]
	fn loads_runtime_database_config_from_toml() {
		let config = read_runtime_database_config_from_str(
			r#"
				[databases]
				ExampleDb = "sqlite:data/example.sqlite"
				ArchiveDb = "sqlite::memory:"
			"#,
		).unwrap();

		assert_eq!(
			config.sqlite_database_path("exampledb"),
			Some(Path::new("data/example.sqlite")),
		);
		assert_eq!(
			config.sqlite_database_path("ArchiveDb"),
			Some(Path::new(":memory:")),
		);
	}

	#[test]
	fn rejects_invalid_connection_string_from_runtime_config() {
		let error = read_runtime_database_config_from_str(
			r#"
				[databases]
				ExampleDb = "postgresql:host=localhost"
			"#,
		).unwrap_err();

		assert_eq!(
			error,
			RuntimeConfigError {
				message: String::from(
					"Connection string for database `ExampleDb` uses unsupported backend `postgresql`."
				),
			},
		);
	}

	#[test]
	fn rejects_invalid_runtime_config_toml() {
		let error = read_runtime_database_config_from_str("not = [valid").unwrap_err();

		assert!(matches!(
			error,
			RuntimeConfigError { message } if message.starts_with("Invalid runtime config TOML:")
		));
	}
}
