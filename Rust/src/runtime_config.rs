use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde::Deserialize;

use crate::schema::SchemaCatalog;
use crate::schema_fixture::read_schema_catalog_from_path as read_schema_fixture_catalog_from_path;
use crate::vm::RuntimeDatabaseConfig;

#[derive(Debug, Eq, PartialEq)]
pub struct RuntimeConfigError {
	pub message: String,
}

#[derive(Deserialize)]
struct RuntimeConfigFile {
	databases: Option<BTreeMap<String, String>>,
	schemas: Option<BTreeMap<String, String>>,
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

	fn into_schema_catalog(self, base_directory: &Path) -> Result<SchemaCatalog, RuntimeConfigError> {
		let mut merged_catalog = SchemaCatalog::new();

		for (database_name, schema_path) in self.schemas.unwrap_or_default() {
			let resolved_path = resolve_config_path(base_directory, &schema_path);
			let schema_catalog = read_schema_fixture_catalog_from_path(&resolved_path).map_err(|error| RuntimeConfigError {
				message: format!(
					"Failed to load schema fixture `{}` for database `{}`: {}",
					resolved_path.display(),
					database_name,
					error.message,
				),
			})?;
			let database = schema_catalog.database(&database_name).cloned().ok_or(RuntimeConfigError {
				message: format!(
					"Schema fixture `{}` does not define database `{}`.",
					resolved_path.display(),
					database_name,
				),
			})?;

			merged_catalog.add_database(database).map_err(|error| RuntimeConfigError {
				message: format!("Failed to merge schema fixture for database `{database_name}`: {}", schema_error_message(error)),
			})?;
		}

		Ok(merged_catalog)
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

pub fn read_schema_catalog_from_runtime_config_path(path: impl AsRef<Path>) -> Result<SchemaCatalog, RuntimeConfigError> {
	let path = path.as_ref();
	let contents = std::fs::read_to_string(path).map_err(|error| RuntimeConfigError {
		message: format!("Failed to read runtime config file: {error}"),
	})?;
	let config_file: RuntimeConfigFile = toml::from_str(&contents).map_err(|error| RuntimeConfigError {
		message: format!("Invalid runtime config TOML: {error}"),
	})?;
	let base_directory = path.parent().unwrap_or_else(|| Path::new("."));

	config_file.into_schema_catalog(base_directory)
}

fn resolve_config_path(base_directory: &Path, configured_path: &str) -> PathBuf {
	let path = Path::new(configured_path);

	if path.is_absolute() {
		path.to_path_buf()
	}
	else {
		base_directory.join(path)
	}
}

fn schema_error_message(error: crate::schema::SchemaError) -> String {
	match error {
		crate::schema::SchemaError::AmbiguousDatabaseQualifiedTableName { database_name, table_name } => {
			format!("Table `{table_name}` is ambiguous within database `{database_name}`.")
		}
		crate::schema::SchemaError::AmbiguousSchemaQualifiedTableName {
			active_databases,
			schema_name,
			table_name,
		} => format!(
			"Table `{schema_name}.{table_name}` is ambiguous across active databases: {}.",
			active_databases.join(", "),
		),
		crate::schema::SchemaError::AmbiguousTableName {
			active_databases,
			table_name,
		} => format!(
			"Table `{table_name}` is ambiguous across active databases: {}.",
			active_databases.join(", "),
		),
		crate::schema::SchemaError::DuplicateColumn { column_name, table_name } => {
			format!("Column `{column_name}` is defined more than once in table `{table_name}`.")
		}
		crate::schema::SchemaError::DuplicateDatabase { database_name } => {
			format!("Database `{database_name}` is defined more than once.")
		}
		crate::schema::SchemaError::DuplicateSchema { database_name, schema_name } => {
			format!("Schema `{schema_name}` is defined more than once in database `{database_name}`.")
		}
		crate::schema::SchemaError::DuplicateTable {
			database_name,
			schema_name,
			table_name,
		} => format!(
			"Table `{table_name}` is defined more than once in `{database_name}.{schema_name}`."
		),
		crate::schema::SchemaError::UnknownDatabase { database_name } => {
			format!("Database `{database_name}` is not present in the schema catalog.")
		}
		crate::schema::SchemaError::UnknownSchema { database_name, schema_name } => {
			match database_name {
				Some(database_name) => format!("Schema `{schema_name}` is not present in database `{database_name}`."),
				None => format!("Schema `{schema_name}` is not present in the schema catalog."),
			}
		}
		crate::schema::SchemaError::UnknownTable { table_name } => {
			format!("Table `{table_name}` is not present in the schema catalog.")
		}
	}
}

#[cfg(test)]
mod tests {
	use std::path::Path;
	use std::time::{SystemTime, UNIX_EPOCH};

	use super::read_runtime_database_config_from_str;
	use super::read_schema_catalog_from_runtime_config_path;
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
	fn loads_schema_catalog_from_runtime_config() {
		let temp_dir = unique_temp_directory("runtime_config_schema");
		let schema_path = temp_dir.join("example.schema");
		let config_path = temp_dir.join("tablo.toml");
		std::fs::create_dir_all(&temp_dir).unwrap();
		std::fs::write(
			&schema_path,
			r#"{
				"databases": [
					{
						"backend": "sqlite",
						"name": "ExampleDb",
						"schemas": [
							{
								"name": "Main",
								"is_implicit": true,
								"tables": [
									{ "name": "Customers", "columns": [] }
								]
							}
						]
					}
				]
			}"#,
		).unwrap();
		std::fs::write(
			&config_path,
			r#"
				[schemas]
				ExampleDb = "example.schema"
			"#,
		).unwrap();

		let catalog = read_schema_catalog_from_runtime_config_path(&config_path).unwrap();

		assert!(catalog.database("ExampleDb").is_some());

		let _ = std::fs::remove_file(&schema_path);
		let _ = std::fs::remove_file(&config_path);
		let _ = std::fs::remove_dir(&temp_dir);
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

	#[test]
	fn rejects_schema_mapping_when_fixture_does_not_define_named_database() {
		let temp_dir = unique_temp_directory("runtime_config_missing_db");
		let schema_path = temp_dir.join("example.schema");
		let config_path = temp_dir.join("tablo.toml");
		std::fs::create_dir_all(&temp_dir).unwrap();
		std::fs::write(
			&schema_path,
			r#"{
				"databases": [
					{
						"backend": "sqlite",
						"name": "OtherDb",
						"schemas": []
					}
				]
			}"#,
		).unwrap();
		std::fs::write(
			&config_path,
			r#"
				[schemas]
				ExampleDb = "example.schema"
			"#,
		).unwrap();

		let error = read_schema_catalog_from_runtime_config_path(&config_path).unwrap_err();

		assert_eq!(
			error,
			RuntimeConfigError {
				message: format!(
					"Schema fixture `{}` does not define database `ExampleDb`.",
					schema_path.display(),
				),
			},
		);

		let _ = std::fs::remove_file(&schema_path);
		let _ = std::fs::remove_file(&config_path);
		let _ = std::fs::remove_dir(&temp_dir);
	}

	fn unique_temp_directory(name: &str) -> std::path::PathBuf {
		let nanos = SystemTime::now()
			.duration_since(UNIX_EPOCH)
			.unwrap()
			.as_nanos();
		std::env::temp_dir().join(format!("{name}_{nanos}"))
	}
}
