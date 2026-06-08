use std::path::Path;

use serde::Deserialize;

use crate::schema::ColumnSchema;
use crate::schema::DatabaseBackend;
use crate::schema::DatabaseNamespace;
use crate::schema::DatabaseSchema;
use crate::schema::SchemaCatalog;
use crate::schema::SchemaDataType;
use crate::schema::SchemaError;
use crate::schema::TableSchema;

#[derive(Clone, Debug, Eq, PartialEq)]
enum SchemaTokenKind {
	Comma,
	Identifier(String),
	LeftBracket,
	LeftParenthesis,
	RightBracket,
	RightParenthesis,
	Semicolon,
}

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
	backend: String,
	name: String,
	schemas: Vec<SchemaFixture>,
}

impl DatabaseFixture {
	fn into_database_schema(self) -> Result<DatabaseSchema, SchemaFixtureError> {
		let mut database = DatabaseSchema::with_backend(self.name, parse_database_backend(&self.backend)?);

		for schema in self.schemas {
			database.add_schema(schema.into_database_namespace(&database.name().to_string())?)
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
struct SchemaFixture {
	is_implicit: Option<bool>,
	name: String,
	tables: Vec<TableFixture>,
}

impl SchemaFixture {
	fn into_database_namespace(self, database_name: &str) -> Result<DatabaseNamespace, SchemaFixtureError> {
		let mut schema = if self.is_implicit.unwrap_or(false) {
			DatabaseNamespace::with_implicit(self.name)
		}
		else {
			DatabaseNamespace::new(self.name)
		};

		for table in self.tables {
			schema.add_table(table.into_table_schema()?, database_name)
				.map_err(schema_error_to_fixture_error)?;
		}

		Ok(schema)
	}
}

struct SchemaTextParser {
	position: usize,
	tokens: Vec<SchemaToken>,
}

impl SchemaTextParser {
	fn new(source: &str) -> Self {
		Self {
			position: 0,
			tokens: tokenize_schema_text(source),
		}
	}

	fn consume_keyword(&mut self, keyword: &str) -> bool {
		match self.current() {
			Some(SchemaToken {
				kind: SchemaTokenKind::Identifier(word),
				..
			}) if word.eq_ignore_ascii_case(keyword) => {
				self.next();
				true
			}
			_ => false,
		}
	}

	fn current(&self) -> Option<&SchemaToken> {
		self.tokens.get(self.position)
	}

	fn ensure_database_initialized(
		&self,
		database: &mut Option<DatabaseSchema>,
		backend: &Option<DatabaseBackend>,
		database_name: &Option<String>,
	) -> Result<(), SchemaFixtureError> {
		if database.is_some() {
			return Ok(());
		}

		let backend = backend.ok_or_else(|| SchemaFixtureError {
			message: String::from("Schema file must declare a backend before schemas or tables."),
		})?;
		let database_name = database_name.clone().ok_or_else(|| SchemaFixtureError {
			message: String::from("Schema file must declare a database name before schemas or tables."),
		})?;
		*database = Some(DatabaseSchema::with_backend(database_name, backend));
		Ok(())
	}

	fn error_at_current(&self, message: String) -> SchemaFixtureError {
		let position = self.current().map(|token| token.position).unwrap_or(0);
		SchemaFixtureError {
			message: format!("{message} At byte {position}."),
		}
	}

	fn expect_identifier(&mut self, message: &str) -> Result<String, SchemaFixtureError> {
		match self.next() {
			Some(SchemaToken {
				kind: SchemaTokenKind::Identifier(value),
				..
			}) => Ok(value),
			_ => Err(self.error_at_current(String::from(message))),
		}
	}

	fn expect_semicolon(&mut self, message: &str) -> Result<(), SchemaFixtureError> {
		match self.next() {
			Some(SchemaToken {
				kind: SchemaTokenKind::Semicolon,
				..
			}) => Ok(()),
			_ => Err(self.error_at_current(String::from(message))),
		}
	}

	fn next(&mut self) -> Option<SchemaToken> {
		let token = self.current()?.clone();
		self.position += 1;
		Some(token)
	}

	fn parse_catalog(&mut self) -> Result<SchemaCatalog, SchemaFixtureError> {
		let mut backend = None;
		let mut database_name = None;
		let mut database = None::<DatabaseSchema>;
		let mut current_schema_name = None::<String>;

		while let Some(token) = self.current() {
			match &token.kind {
				SchemaTokenKind::Identifier(word) if word.eq_ignore_ascii_case("backend") => {
					self.next();
					let backend_name = self.expect_identifier("Expected database backend after `backend`.")?;
					self.expect_semicolon("Expected `;` after backend declaration.")?;
					backend = Some(parse_database_backend(&backend_name)?);
				}
				SchemaTokenKind::Identifier(word) if word.eq_ignore_ascii_case("database") => {
					self.next();
					let name = self.expect_identifier("Expected database name after `database`.")?;
					self.expect_semicolon("Expected `;` after database declaration.")?;
					database_name = Some(name);
				}
				SchemaTokenKind::Identifier(word) if word.eq_ignore_ascii_case("schema") => {
					self.next();
					let schema_name = self.expect_identifier("Expected schema name after `schema`.")?;
					let is_implicit = self.consume_keyword("implicit");
					self.expect_semicolon("Expected `;` after schema declaration.")?;
					self.ensure_database_initialized(&mut database, &backend, &database_name)?;
					let database = database.as_mut().unwrap();
					let schema = if is_implicit {
						DatabaseNamespace::with_implicit(schema_name.clone())
					}
					else {
						DatabaseNamespace::new(schema_name.clone())
					};
					database.add_schema(schema).map_err(schema_error_to_fixture_error)?;
					current_schema_name = Some(schema_name);
				}
				SchemaTokenKind::Identifier(word) if word.eq_ignore_ascii_case("create") => {
					self.next();
					let object_type = self.expect_identifier("Expected `schema` or `table` after `create`.")?;

					if object_type.eq_ignore_ascii_case("schema") {
						let schema_name = self.expect_identifier("Expected schema name after `create schema`.")?;
						let is_implicit = self.consume_keyword("implicit");
						self.expect_semicolon("Expected `;` after schema declaration.")?;
						self.ensure_database_initialized(&mut database, &backend, &database_name)?;
						let database = database.as_mut().unwrap();
						let schema = if is_implicit {
							DatabaseNamespace::with_implicit(schema_name.clone())
						}
						else {
							DatabaseNamespace::new(schema_name.clone())
						};
						database.add_schema(schema).map_err(schema_error_to_fixture_error)?;
						current_schema_name = Some(schema_name);
					}
					else if object_type.eq_ignore_ascii_case("table") {
						let table_name = self.expect_identifier("Expected table name after `create table`.")?;
						let table = self.parse_table_schema(table_name)?;
						self.ensure_database_initialized(&mut database, &backend, &database_name)?;
						let database = database.as_mut().unwrap();
						let database_name = database.name().to_string();
						let schema_name = current_schema_name.clone().ok_or_else(|| SchemaFixtureError {
							message: String::from("A schema must be declared before any table definitions."),
						})?;
						let schema = database.schema_mut(&schema_name).ok_or_else(|| SchemaFixtureError {
							message: format!("Schema `{schema_name}` must be declared before adding tables to it."),
						})?;
						schema.add_table(table, &database_name).map_err(schema_error_to_fixture_error)?;
					}
					else {
						return Err(self.error_at_current(format!(
							"Expected `schema` or `table` after `create`, found `{object_type}`."
						)));
					}
				}
				SchemaTokenKind::Identifier(word) if word.eq_ignore_ascii_case("table") => {
					self.next();
					let table_name = self.expect_identifier("Expected table name after `table`.")?;
					let table = self.parse_table_schema(table_name)?;
					self.ensure_database_initialized(&mut database, &backend, &database_name)?;
					let database = database.as_mut().unwrap();
					let database_name = database.name().to_string();
					let schema_name = current_schema_name.clone().ok_or_else(|| SchemaFixtureError {
						message: String::from("A schema must be declared before any table definitions."),
					})?;
					let schema = database.schema_mut(&schema_name).ok_or_else(|| SchemaFixtureError {
						message: format!("Schema `{schema_name}` must be declared before adding tables to it."),
					})?;
					schema.add_table(table, &database_name).map_err(schema_error_to_fixture_error)?;
				}
				_ => {
					return Err(self.error_at_current(String::from(
						"Expected a schema declaration, table declaration, or backend/database header.",
					)));
				}
			}
		}

		let database = database.ok_or_else(|| SchemaFixtureError {
			message: String::from("Schema file must declare at least a database, backend, schema, and one table."),
		})?;
		let mut catalog = SchemaCatalog::new();
		catalog.add_database(database).map_err(schema_error_to_fixture_error)?;
		Ok(catalog)
	}

	fn parse_schema_data_type(&mut self) -> Result<SchemaDataType, SchemaFixtureError> {
		match self.next() {
			Some(SchemaToken {
				kind: SchemaTokenKind::LeftBracket,
				..
			}) => {
				let element_type = self.parse_schema_data_type()?;
				match self.next() {
					Some(SchemaToken {
						kind: SchemaTokenKind::RightBracket,
						..
					}) => Ok(SchemaDataType::Array(Box::new(element_type))),
					_ => Err(self.error_at_current(String::from("Expected `]` after array element type."))),
				}
			}
			Some(SchemaToken {
				kind: SchemaTokenKind::Identifier(data_type),
				..
			}) => parse_schema_data_type(&data_type),
			_ => Err(self.error_at_current(String::from("Expected a schema data type."))),
		}
	}

	fn parse_table_schema(&mut self, table_name: String) -> Result<TableSchema, SchemaFixtureError> {
		match self.next() {
			Some(SchemaToken {
				kind: SchemaTokenKind::LeftParenthesis,
				..
			}) => {}
			_ => {
				return Err(self.error_at_current(String::from(
					"Expected `(` to start table column list.",
				)));
			}
		}

		let mut table = TableSchema::new(table_name);

		loop {
			if matches!(self.current(), Some(SchemaToken { kind: SchemaTokenKind::RightParenthesis, .. })) {
				self.next();
				break;
			}

			let column_name = self.expect_identifier("Expected column name in table definition.")?;
			let data_type = self.parse_schema_data_type()?;
			let is_nullable = if self.consume_keyword("not") {
				let keyword = self.expect_identifier("Expected `null` after `not` in column nullability.")?;
				if !keyword.eq_ignore_ascii_case("null") {
					return Err(self.error_at_current(format!(
						"Expected `null` after `not`, found `{keyword}`."
					)));
				}
				false
			}
			else {
				if self.consume_keyword("null") {
					true
				}
				else {
					false
				}
			};

			table.add_column(ColumnSchema::new(column_name, data_type, is_nullable))
				.map_err(schema_error_to_fixture_error)?;

			match self.current() {
				Some(SchemaToken {
					kind: SchemaTokenKind::Comma,
					..
				}) => {
					self.next();
				}
				Some(SchemaToken {
					kind: SchemaTokenKind::RightParenthesis,
					..
				}) => {
					self.next();
					break;
				}
				_ => {
					return Err(self.error_at_current(String::from(
						"Expected `,` or `)` after table column definition.",
					)));
				}
			}
		}

		self.expect_semicolon("Expected `;` after table definition.")?;
		Ok(table)
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct SchemaToken {
	kind: SchemaTokenKind,
	position: usize,
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
	let trimmed = fixture.trim_start();

	if trimmed.starts_with('{') || trimmed.starts_with('[') {
		let fixture: SchemaCatalogFixture = serde_json::from_str(fixture).map_err(|error| SchemaFixtureError {
			message: format!("Invalid legacy JSON schema fixture: {error}"),
		})?;

		return fixture.into_schema_catalog();
	}

	read_schema_catalog_from_sql_like_str(fixture)
}

fn parse_database_backend(input: &str) -> Result<DatabaseBackend, SchemaFixtureError> {
	match input.trim().to_ascii_lowercase().as_str() {
		"mysql" => Ok(DatabaseBackend::MySql),
		"postgres" | "postgresql" => Ok(DatabaseBackend::PostgreSql),
		"sqlite" => Ok(DatabaseBackend::Sqlite),
		_ => Err(SchemaFixtureError {
			message: format!("Unsupported schema fixture database backend `{input}`."),
		}),
	}
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
		"int" | "integer" => Ok(SchemaDataType::Int),
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

fn read_schema_catalog_from_sql_like_str(source: &str) -> Result<SchemaCatalog, SchemaFixtureError> {
	let mut parser = SchemaTextParser::new(source);
	parser.parse_catalog()
}

fn schema_error_to_fixture_error(error: SchemaError) -> SchemaFixtureError {
	SchemaFixtureError {
		message: match error {
			SchemaError::AmbiguousDatabaseQualifiedTableName { database_name, table_name } => {
				format!("Table `{table_name}` is ambiguous within database `{database_name}`.")
			}
			SchemaError::AmbiguousSchemaQualifiedTableName {
				active_databases,
				schema_name,
				table_name,
			} => {
				format!(
					"Table `{schema_name}.{table_name}` is ambiguous across active databases: {}.",
					active_databases.join(", ")
				)
			}
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
			SchemaError::DuplicateSchema { database_name, schema_name } => {
				format!("Schema `{schema_name}` is declared more than once in database `{database_name}`.")
			}
			SchemaError::DuplicateTable {
				database_name,
				schema_name,
				table_name,
			} => {
				format!(
					"Table `{table_name}` is declared more than once in schema `{schema_name}` of database `{database_name}`."
				)
			}
			SchemaError::UnknownDatabase { database_name } => {
				format!("Unknown database `{database_name}`.")
			}
			SchemaError::UnknownSchema { database_name, schema_name } => {
				match database_name {
					Some(database_name) => {
						format!("Unknown schema `{schema_name}` in database `{database_name}`.")
					}
					None => format!("Unknown schema `{schema_name}`."),
				}
			}
			SchemaError::UnknownTable { table_name } => {
				format!("Unknown table `{table_name}`.")
			}
		},
	}
}

fn tokenize_schema_text(source: &str) -> Vec<SchemaToken> {
	let mut tokens = Vec::new();
	let mut chars = source.char_indices().peekable();

	while let Some((index, ch)) = chars.next() {
		match ch {
			c if c.is_whitespace() => {}
			'-' => {
				if chars.peek().is_some_and(|(_, next)| *next == '-') {
					chars.next();
					while let Some((_, next)) = chars.next() {
						if next == '\n' {
							break;
						}
					}
				}
				else {
					tokens.push(SchemaToken {
						kind: SchemaTokenKind::Identifier(String::from("-")),
						position: index,
					});
				}
			}
			'/' => {
				if chars.peek().is_some_and(|(_, next)| *next == '/') {
					chars.next();
					while let Some((_, next)) = chars.next() {
						if next == '\n' {
							break;
						}
					}
				}
				else if chars.peek().is_some_and(|(_, next)| *next == '*') {
					chars.next();
					let mut depth = 1_usize;
					while let Some((_, next)) = chars.next() {
						if next == '/' && chars.peek().is_some_and(|(_, following)| *following == '*') {
							chars.next();
							depth += 1;
						}
						else if next == '*' && chars.peek().is_some_and(|(_, following)| *following == '/') {
							chars.next();
							depth -= 1;
							if depth == 0 {
								break;
							}
						}
					}
				}
			}
			',' => tokens.push(SchemaToken {
				kind: SchemaTokenKind::Comma,
				position: index,
			}),
			'(' => tokens.push(SchemaToken {
				kind: SchemaTokenKind::LeftParenthesis,
				position: index,
			}),
			')' => tokens.push(SchemaToken {
				kind: SchemaTokenKind::RightParenthesis,
				position: index,
			}),
			'[' => tokens.push(SchemaToken {
				kind: SchemaTokenKind::LeftBracket,
				position: index,
			}),
			']' => tokens.push(SchemaToken {
				kind: SchemaTokenKind::RightBracket,
				position: index,
			}),
			';' => tokens.push(SchemaToken {
				kind: SchemaTokenKind::Semicolon,
				position: index,
			}),
			_ => {
				if ch.is_ascii_alphanumeric() || ch == '_' {
					let mut value = String::from(ch);
					while let Some((_, next)) = chars.peek() {
						if next.is_ascii_alphanumeric() || *next == '_' {
							value.push(*next);
							chars.next();
						}
						else {
							break;
						}
					}
					tokens.push(SchemaToken {
						kind: SchemaTokenKind::Identifier(value),
						position: index,
					});
				}
			}
		}
	}

	tokens
}

#[cfg(test)]
mod tests {
	use crate::schema::SchemaDataType;

	use super::read_schema_catalog_from_str;
	use super::SchemaFixtureError;

	#[test]
	fn accepts_integer_alias_in_sql_like_schema() {
		let catalog = read_schema_catalog_from_str(
			r#"
				backend sqlite;
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id INTEGER not null
				);
			"#,
		).unwrap();

		let database = catalog.database("exampledb").unwrap();
		let schema = database.schema("main").unwrap();
		let table = schema.table("customers").unwrap();
		let column = table.column("id").unwrap();

		assert_eq!(column.data_type(), &SchemaDataType::Int);
		assert!(!column.is_nullable());
	}

	#[test]
	fn loads_single_schema_database_with_implicit_schema_marker() {
		let catalog = read_schema_catalog_from_str(
			r#"
				backend sqlite;
				database ExampleDb;
				schema Main implicit;
				create table Customers ();
			"#,
		).unwrap();

		let database = catalog.database("exampledb").unwrap();
		let schema = database.schema("main").unwrap();

		assert!(schema.is_implicit());
	}

	#[test]
	fn loads_schema_fixture_catalog() {
		let catalog = read_schema_catalog_from_str(
			r#"
				backend postgresql;
				database ExampleDb;
				create schema Public;
				create table Customers (
					Id int not null,
					Tags [text] null
				);
			"#,
		).unwrap();

		let database = catalog.database("exampledb").unwrap();
		let schema = database.schema("public").unwrap();
		let table = schema.table("customers").unwrap();
		let column = table.column("tags").unwrap();

		assert_eq!(column.data_type(), &SchemaDataType::Array(Box::new(SchemaDataType::Text)));
		assert!(column.is_nullable());
	}

	#[test]
	fn rejects_duplicate_tables_from_fixture() {
		let error = read_schema_catalog_from_str(
			r#"
				backend mysql;
				database ExampleDb;
				schema Public;
				table Customers ();
				table customers ();
			"#,
		).unwrap_err();

		assert_eq!(error, SchemaFixtureError {
			message: String::from(
				"Table `customers` is declared more than once in schema `Public` of database `ExampleDb`."
			),
		});
	}

	#[test]
	fn rejects_unsupported_fixture_data_type() {
		let error = read_schema_catalog_from_str(
			r#"
				backend sqlite;
				database ExampleDb;
				schema Public;
				table Customers (
					Profile customer not null
				);
			"#,
		).unwrap_err();

		assert_eq!(error, SchemaFixtureError {
			message: String::from("Unsupported schema fixture data type `customer`."),
		});
	}

	#[test]
	fn still_loads_legacy_json_schema_fixture() {
		let catalog = read_schema_catalog_from_str(
			r#"{
				"databases": [
					{
						"backend": "sqlite",
						"name": "LegacyDb",
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

		assert!(catalog.database("LegacyDb").is_some());
	}
}
