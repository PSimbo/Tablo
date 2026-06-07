use std::path::Path;

use serde::Deserialize;

use crate::schema::ColumnSchema;
use crate::schema::DatabaseSchema;
use crate::schema::SchemaCatalog;
use crate::schema::SchemaDataType;
use crate::schema::SchemaError;
use crate::schema::TableSchema;

#[derive(Debug, Eq, PartialEq)]
pub struct SchemaFixtureError {
	pub message: String,
}

#[derive(Deserialize)]
struct ColumnFixture {
	data_type: String,
	is_nullable: bool,
	name: String,
}

impl ColumnFixture {
	fn into_column_schema(self) -> Result<ColumnSchema, SchemaFixtureError> {
		Ok(ColumnSchema::new(
			self.name,
			parse_schema_data_type(&self.data_type)?,
			self.is_nullable,
		))
	}
}

#[derive(Deserialize)]
struct DatabaseFixture {
	name: String,
	tables: Vec<TableFixture>,
}

impl DatabaseFixture {
	fn into_database_schema(self) -> Result<DatabaseSchema, SchemaFixtureError> {
		let mut database = DatabaseSchema::new(self.name);

		for table in self.tables {
			database.add_table(table.into_table_schema()?)
				.map_err(schema_error_to_fixture_error)?;
		}

		Ok(database)
	}
}

#[derive(Deserialize)]
struct SchemaCatalogFixture {
	databases: Vec<DatabaseFixture>,
}

impl SchemaCatalogFixture {
	fn into_schema_catalog(self) -> Result<SchemaCatalog, SchemaFixtureError> {
		let mut catalog = SchemaCatalog::new();

		for database in self.databases {
			catalog.add_database(database.into_database_schema()?)
				.map_err(schema_error_to_fixture_error)?;
		}

		Ok(catalog)
	}
}

#[derive(Deserialize)]
struct TableFixture {
	columns: Vec<ColumnFixture>,
	name: String,
}

impl TableFixture {
	fn into_table_schema(self) -> Result<TableSchema, SchemaFixtureError> {
		let mut table = TableSchema::new(self.name);

		for column in self.columns {
			table.add_column(column.into_column_schema()?)
				.map_err(schema_error_to_fixture_error)?;
		}

		Ok(table)
	}
}

pub fn read_schema_catalog_from_path(path: impl AsRef<Path>) -> Result<SchemaCatalog, SchemaFixtureError> {
	let fixture = std::fs::read_to_string(path).map_err(|error| SchemaFixtureError {
		message: format!("Failed to read schema fixture: {error}"),
	})?;

	read_schema_catalog_from_str(&fixture)
}

pub fn read_schema_catalog_from_str(fixture: &str) -> Result<SchemaCatalog, SchemaFixtureError> {
	let fixture: SchemaCatalogFixture = serde_json::from_str(fixture).map_err(|error| SchemaFixtureError {
		message: format!("Invalid schema fixture JSON: {error}"),
	})?;

	fixture.into_schema_catalog()
}

fn parse_schema_data_type(input: &str) -> Result<SchemaDataType, SchemaFixtureError> {
	let trimmed = input.trim();

	if let Some(element_type) = trimmed.strip_prefix('[').and_then(|rest| rest.strip_suffix(']')) {
		return Ok(SchemaDataType::Array(Box::new(parse_schema_data_type(element_type)?)));
	}

	match trimmed.to_ascii_lowercase().as_str() {
		"bin" => Ok(SchemaDataType::Bin),
		"bool" => Ok(SchemaDataType::Bool),
		"date" => Ok(SchemaDataType::Date),
		"dec" => Ok(SchemaDataType::Dec),
		"float" => Ok(SchemaDataType::Float),
		"int" => Ok(SchemaDataType::Int),
		"json" => Ok(SchemaDataType::Json),
		"text" => Ok(SchemaDataType::Text),
		"time" => Ok(SchemaDataType::Time),
		"timestamp" => Ok(SchemaDataType::Timestamp),
		"timestamptz" => Ok(SchemaDataType::TimestampTz),
		"timetz" => Ok(SchemaDataType::TimeTz),
		"uuid" => Ok(SchemaDataType::Uuid),
		_ => Err(SchemaFixtureError {
			message: format!("Unsupported schema fixture data type `{trimmed}`."),
		}),
	}
}

fn schema_error_to_fixture_error(error: SchemaError) -> SchemaFixtureError {
	SchemaFixtureError {
		message: match error {
			SchemaError::AmbiguousTableName { active_databases, table_name } => {
				format!(
					"Table `{table_name}` is ambiguous across active databases: {}.",
					active_databases.join(", ")
				)
			}
			SchemaError::DuplicateColumn { column_name, table_name } => {
				format!("Column `{column_name}` is declared more than once in table `{table_name}`.")
			}
			SchemaError::DuplicateDatabase { database_name } => {
				format!("Database `{database_name}` is declared more than once.")
			}
			SchemaError::DuplicateTable { database_name, table_name } => {
				format!("Table `{table_name}` is declared more than once in database `{database_name}`.")
			}
			SchemaError::UnknownDatabase { database_name } => {
				format!("Unknown database `{database_name}`.")
			}
			SchemaError::UnknownTable { table_name } => {
				format!("Unknown table `{table_name}`.")
			}
		},
	}
}

#[cfg(test)]
mod tests {
	use crate::schema::SchemaDataType;

	use super::read_schema_catalog_from_str;
	use super::SchemaFixtureError;

	#[test]
	fn loads_schema_fixture_catalog() {
		let catalog = read_schema_catalog_from_str(
			r#"{
				"databases": [
					{
						"name": "ExampleDb",
						"tables": [
							{
								"name": "Customers",
								"columns": [
									{ "name": "Id", "data_type": "int", "is_nullable": false },
									{ "name": "Tags", "data_type": "[text]", "is_nullable": true }
								]
							}
						]
					}
				]
			}"#,
		).unwrap();

		let database = catalog.database("exampledb").unwrap();
		let table = database.table("customers").unwrap();
		let column = table.column("tags").unwrap();

		assert_eq!(column.data_type(), &SchemaDataType::Array(Box::new(SchemaDataType::Text)));
		assert!(column.is_nullable());
	}

	#[test]
	fn rejects_duplicate_tables_from_fixture() {
		let error = read_schema_catalog_from_str(
			r#"{
				"databases": [
					{
						"name": "ExampleDb",
						"tables": [
							{ "name": "Customers", "columns": [] },
							{ "name": "customers", "columns": [] }
						]
					}
				]
			}"#,
		).unwrap_err();

		assert_eq!(error, SchemaFixtureError {
			message: String::from("Table `customers` is declared more than once in database `ExampleDb`."),
		});
	}

	#[test]
	fn rejects_unsupported_fixture_data_type() {
		let error = read_schema_catalog_from_str(
			r#"{
				"databases": [
					{
						"name": "ExampleDb",
						"tables": [
							{
								"name": "Customers",
								"columns": [
									{ "name": "Profile", "data_type": "customer", "is_nullable": false }
								]
							}
						]
					}
				]
			}"#,
		).unwrap_err();

		assert_eq!(error, SchemaFixtureError {
			message: String::from("Unsupported schema fixture data type `customer`."),
		});
	}
}
