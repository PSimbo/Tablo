use std::collections::BTreeMap;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SchemaDataType {
	Array(Box<SchemaDataType>),
	Bin,
	Bool,
	Date,
	Dec,
	Float,
	Int,
	Json,
	Text,
	Time,
	Timestamp,
	TimestampTz,
	TimeTz,
	Uuid,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SchemaError {
	AmbiguousTableName {
		active_databases: Vec<String>,
		table_name: String,
	},
	DuplicateColumn {
		column_name: String,
		table_name: String,
	},
	DuplicateDatabase {
		database_name: String,
	},
	DuplicateTable {
		database_name: String,
		table_name: String,
	},
	UnknownDatabase {
		database_name: String,
	},
	UnknownTable {
		table_name: String,
	},
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ColumnSchema {
	data_type: SchemaDataType,
	is_nullable: bool,
	name: String,
}

impl ColumnSchema {
	pub fn new(name: impl Into<String>, data_type: SchemaDataType, is_nullable: bool) -> Self {
		Self {
			data_type,
			is_nullable,
			name: name.into(),
		}
	}

	pub fn data_type(&self) -> &SchemaDataType {
		&self.data_type
	}

	pub fn is_nullable(&self) -> bool {
		self.is_nullable
	}

	pub fn name(&self) -> &str {
		&self.name
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DatabaseSchema {
	name: String,
	tables: BTreeMap<String, TableSchema>,
}

impl DatabaseSchema {
	pub fn new(name: impl Into<String>) -> Self {
		Self {
			name: name.into(),
			tables: BTreeMap::new(),
		}
	}

	pub fn add_table(&mut self, table: TableSchema) -> Result<(), SchemaError> {
		let normalized_name = normalize_name(table.name());

		if self.tables.contains_key(&normalized_name) {
			return Err(SchemaError::DuplicateTable {
				database_name: self.name.clone(),
				table_name: table.name().to_string(),
			});
		}

		self.tables.insert(normalized_name, table);
		Ok(())
	}

	pub fn name(&self) -> &str {
		&self.name
	}

	pub fn table(&self, name: &str) -> Option<&TableSchema> {
		self.tables.get(&normalize_name(name))
	}

	pub fn tables(&self) -> impl Iterator<Item = &TableSchema> {
		self.tables.values()
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedTable<'a> {
	database: &'a DatabaseSchema,
	table: &'a TableSchema,
}

impl<'a> ResolvedTable<'a> {
	pub fn database(&self) -> &'a DatabaseSchema {
		self.database
	}

	pub fn table(&self) -> &'a TableSchema {
		self.table
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SchemaCatalog {
	databases: BTreeMap<String, DatabaseSchema>,
}

impl SchemaCatalog {
	pub fn new() -> Self {
		Self {
			databases: BTreeMap::new(),
		}
	}

	pub fn add_database(&mut self, database: DatabaseSchema) -> Result<(), SchemaError> {
		let normalized_name = normalize_name(database.name());

		if self.databases.contains_key(&normalized_name) {
			return Err(SchemaError::DuplicateDatabase {
				database_name: database.name().to_string(),
			});
		}

		self.databases.insert(normalized_name, database);
		Ok(())
	}

	pub fn database(&self, name: &str) -> Option<&DatabaseSchema> {
		self.databases.get(&normalize_name(name))
	}

	pub fn databases(&self) -> impl Iterator<Item = &DatabaseSchema> {
		self.databases.values()
	}

	pub fn resolve_table<'a>(
		&'a self,
		active_databases: &[&str],
		table_name: &str,
	) -> Result<ResolvedTable<'a>, SchemaError> {
		let mut matches = Vec::new();

		for database_name in active_databases {
			let database = self.database(database_name).ok_or(SchemaError::UnknownDatabase {
				database_name: (*database_name).to_string(),
			})?;

			if let Some(table) = database.table(table_name) {
				matches.push(ResolvedTable { database, table });
			}
		}

		match matches.len() {
			0 => Err(SchemaError::UnknownTable {
				table_name: table_name.to_string(),
			}),
			1 => Ok(matches.pop().unwrap()),
			_ => Err(SchemaError::AmbiguousTableName {
				active_databases: active_databases.iter().map(|name| (*name).to_string()).collect(),
				table_name: table_name.to_string(),
			}),
		}
	}
}

impl Default for SchemaCatalog {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TableSchema {
	columns: BTreeMap<String, ColumnSchema>,
	name: String,
}

impl TableSchema {
	pub fn new(name: impl Into<String>) -> Self {
		Self {
			columns: BTreeMap::new(),
			name: name.into(),
		}
	}

	pub fn add_column(&mut self, column: ColumnSchema) -> Result<(), SchemaError> {
		let normalized_name = normalize_name(column.name());

		if self.columns.contains_key(&normalized_name) {
			return Err(SchemaError::DuplicateColumn {
				column_name: column.name().to_string(),
				table_name: self.name.clone(),
			});
		}

		self.columns.insert(normalized_name, column);
		Ok(())
	}

	pub fn column(&self, name: &str) -> Option<&ColumnSchema> {
		self.columns.get(&normalize_name(name))
	}

	pub fn columns(&self) -> impl Iterator<Item = &ColumnSchema> {
		self.columns.values()
	}

	pub fn name(&self) -> &str {
		&self.name
	}
}

fn normalize_name(name: &str) -> String {
	name.to_ascii_lowercase()
}

#[cfg(test)]
mod tests {
	use super::ColumnSchema;
	use super::DatabaseSchema;
	use super::SchemaCatalog;
	use super::SchemaDataType;
	use super::SchemaError;
	use super::TableSchema;

	#[test]
	fn rejects_duplicate_column_names_case_insensitively() {
		let mut table = TableSchema::new("Customers");
		table.add_column(ColumnSchema::new("Id", SchemaDataType::Int, false)).unwrap();

		let error = table.add_column(ColumnSchema::new("id", SchemaDataType::Int, false)).unwrap_err();

		assert_eq!(error, SchemaError::DuplicateColumn {
			column_name: String::from("id"),
			table_name: String::from("Customers"),
		});
	}

	#[test]
	fn rejects_duplicate_database_names_case_insensitively() {
		let mut catalog = SchemaCatalog::new();
		catalog.add_database(DatabaseSchema::new("ExampleDb")).unwrap();

		let error = catalog.add_database(DatabaseSchema::new("exampledb")).unwrap_err();

		assert_eq!(error, SchemaError::DuplicateDatabase {
			database_name: String::from("exampledb"),
		});
	}

	#[test]
	fn rejects_duplicate_table_names_case_insensitively() {
		let mut database = DatabaseSchema::new("ExampleDb");
		database.add_table(TableSchema::new("Customers")).unwrap();

		let error = database.add_table(TableSchema::new("customers")).unwrap_err();

		assert_eq!(error, SchemaError::DuplicateTable {
			database_name: String::from("ExampleDb"),
			table_name: String::from("customers"),
		});
	}

	#[test]
	fn reports_ambiguous_table_name_across_active_databases() {
		let mut sales = DatabaseSchema::new("Sales");
		sales.add_table(TableSchema::new("Customers")).unwrap();

		let mut archive = DatabaseSchema::new("Archive");
		archive.add_table(TableSchema::new("Customers")).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(sales).unwrap();
		catalog.add_database(archive).unwrap();

		let error = catalog.resolve_table(&["sales", "archive"], "customers").unwrap_err();

		assert_eq!(error, SchemaError::AmbiguousTableName {
			active_databases: vec![String::from("sales"), String::from("archive")],
			table_name: String::from("customers"),
		});
	}

	#[test]
	fn reports_unknown_database_when_resolving_table() {
		let catalog = SchemaCatalog::new();

		let error = catalog.resolve_table(&["missing"], "customers").unwrap_err();

		assert_eq!(error, SchemaError::UnknownDatabase {
			database_name: String::from("missing"),
		});
	}

	#[test]
	fn resolves_database_table_and_column_names_case_insensitively() {
		let mut table = TableSchema::new("Customers");
		table.add_column(ColumnSchema::new("Name", SchemaDataType::Text, true)).unwrap();

		let mut database = DatabaseSchema::new("ExampleDb");
		database.add_table(table).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(database).unwrap();

		let database = catalog.database("exampledb").unwrap();
		let table = database.table("customers").unwrap();
		let column = table.column("name").unwrap();

		assert_eq!(database.name(), "ExampleDb");
		assert_eq!(table.name(), "Customers");
		assert_eq!(column.name(), "Name");
		assert_eq!(column.data_type(), &SchemaDataType::Text);
		assert!(column.is_nullable());
	}

	#[test]
	fn resolves_unqualified_table_name_from_active_databases() {
		let mut table = TableSchema::new("Customers");
		table.add_column(ColumnSchema::new("Id", SchemaDataType::Int, false)).unwrap();

		let mut database = DatabaseSchema::new("Sales");
		database.add_table(table).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(database).unwrap();

		let resolved = catalog.resolve_table(&["sales"], "customers").unwrap();

		assert_eq!(resolved.database().name(), "Sales");
		assert_eq!(resolved.table().name(), "Customers");
	}
}
