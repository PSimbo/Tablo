use std::path::Path;

use crate::schema::ColumnSchema;
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
		database_name: &Option<String>,
	) -> Result<(), SchemaFixtureError> {
		if database.is_some() {
			return Ok(());
		}

		let database_name = database_name.clone().ok_or_else(|| SchemaFixtureError {
			message: String::from("Schema file must declare a database name before schemas or tables."),
		})?;
		*database = Some(DatabaseSchema::new(database_name));
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
		let mut catalog = SchemaCatalog::new();
		let mut database_name = None;
		let mut database = None::<DatabaseSchema>;
		let mut current_schema_name = None::<String>;

		while let Some(token) = self.current() {
			match &token.kind {
				SchemaTokenKind::Identifier(word) if word.eq_ignore_ascii_case("database") => {
					self.next();
					let name = self.expect_identifier("Expected database name after `database`.")?;
					self.expect_semicolon("Expected `;` after database declaration.")?;
					if let Some(database) = database.take() {
						catalog.add_database(database).map_err(schema_error_to_fixture_error)?;
					}
					database_name = Some(name.clone());
					database = Some(DatabaseSchema::new(name));
					current_schema_name = None;
				}
				SchemaTokenKind::Identifier(word) if word.eq_ignore_ascii_case("schema") => {
					self.next();
					let schema_name = self.expect_identifier("Expected schema name after `schema`.")?;
					let is_implicit = self.consume_keyword("implicit");
					self.expect_semicolon("Expected `;` after schema declaration.")?;
					self.ensure_database_initialized(&mut database, &database_name)?;
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
						self.ensure_database_initialized(&mut database, &database_name)?;
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
						self.ensure_database_initialized(&mut database, &database_name)?;
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
					self.ensure_database_initialized(&mut database, &database_name)?;
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
						"Expected a database declaration, schema declaration, or table declaration.",
					)));
				}
			}
		}

		if let Some(database) = database.take() {
			catalog.add_database(database).map_err(schema_error_to_fixture_error)?;
		}

		if catalog.databases().next().is_none() {
			return Err(SchemaFixtureError {
				message: String::from("Schema file must declare at least a database, schema, and one table."),
			});
		}

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
				self.consume_keyword("null")
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

pub fn read_schema_catalog_from_path(path: impl AsRef<Path>) -> Result<SchemaCatalog, SchemaFixtureError> {
	let fixture = std::fs::read_to_string(path).map_err(|error| SchemaFixtureError {
		message: format!("Failed to read schema file: {error}"),
	})?;

	read_schema_catalog_from_str(&fixture)
}

pub fn read_schema_catalog_from_str(fixture: &str) -> Result<SchemaCatalog, SchemaFixtureError> {
	read_schema_catalog_from_sql_like_str(fixture)
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
		"dec" | "numeric" => Ok(SchemaDataType::Dec),
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
			message: format!("Unsupported schema file data type `{trimmed}`."),
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
					for (_, next) in chars.by_ref() {
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
					for (_, next) in chars.by_ref() {
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
	fn accepts_numeric_alias_in_sql_like_schema() {
		let catalog = read_schema_catalog_from_str(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Balance numeric not null
				);
			"#,
		).unwrap();

		let database = catalog.database("exampledb").unwrap();
		let schema = database.schema("main").unwrap();
		let table = schema.table("customers").unwrap();
		let column = table.column("balance").unwrap();

		assert_eq!(column.data_type(), &SchemaDataType::Dec);
		assert!(!column.is_nullable());
	}

	#[test]
	fn loads_multiple_databases_from_single_schema_file() {
		let catalog = read_schema_catalog_from_str(
			r#"
				database Sales;
				schema Public;
				create table Customers ();

				database Archive;
				schema Main implicit;
				create table Customers ();
			"#,
		).unwrap();

		assert!(catalog.database("sales").is_some());
		assert!(catalog.database("archive").is_some());
		assert!(catalog.database("archive").unwrap().schema("main").unwrap().is_implicit());
	}

	#[test]
	fn loads_schema_fixture_catalog() {
		let catalog = read_schema_catalog_from_str(
			r#"
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
	fn loads_single_schema_database_with_implicit_schema_marker() {
		let catalog = read_schema_catalog_from_str(
			r#"
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
	fn rejects_duplicate_tables_from_fixture() {
		let error = read_schema_catalog_from_str(
			r#"
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
				database ExampleDb;
				schema Public;
				table Customers (
					Profile customer not null
				);
			"#,
		).unwrap_err();

		assert_eq!(error, SchemaFixtureError {
			message: String::from("Unsupported schema file data type `customer`."),
		});
	}

}
