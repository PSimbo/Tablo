use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DatabaseBackend {
	MySql,
	PostgreSql,
	Sqlite,
}

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
	AmbiguousDatabaseQualifiedSequenceName {
		database_name: String,
		sequence_name: String,
	},
	AmbiguousDatabaseQualifiedTableName {
		database_name: String,
		table_name: String,
	},
	AmbiguousSchemaQualifiedSequenceName {
		active_databases: Vec<String>,
		schema_name: String,
		sequence_name: String,
	},
	AmbiguousSchemaQualifiedTableName {
		active_databases: Vec<String>,
		schema_name: String,
		table_name: String,
	},
	AmbiguousSequenceName {
		active_databases: Vec<String>,
		sequence_name: String,
	},
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
	DuplicateSchema {
		database_name: String,
		schema_name: String,
	},
	DuplicateSequence {
		database_name: String,
		schema_name: String,
		sequence_name: String,
	},
	DuplicateTable {
		database_name: String,
		schema_name: String,
		table_name: String,
	},
	UnknownDatabase {
		database_name: String,
	},
	UnknownSchema {
		database_name: Option<String>,
		schema_name: String,
	},
	UnknownSequence {
		sequence_name: String,
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
pub struct DatabaseNamespace {
	is_implicit: bool,
	name: String,
	sequences: BTreeMap<String, SequenceSchema>,
	tables: BTreeMap<String, TableSchema>,
}

impl DatabaseNamespace {
	pub fn new(name: impl Into<String>) -> Self {
		Self {
			is_implicit: false,
			name: name.into(),
			sequences: BTreeMap::new(),
			tables: BTreeMap::new(),
		}
	}

	pub fn add_sequence(&mut self, sequence: SequenceSchema, database_name: &str) -> Result<(), SchemaError> {
		let normalized_name = normalize_name(sequence.name());

		if self.sequences.contains_key(&normalized_name) {
			return Err(SchemaError::DuplicateSequence {
				database_name: database_name.to_string(),
				schema_name: self.name.clone(),
				sequence_name: sequence.name().to_string(),
			});
		}

		self.sequences.insert(normalized_name, sequence);
		Ok(())
	}

	pub fn add_table(&mut self, table: TableSchema, database_name: &str) -> Result<(), SchemaError> {
		let normalized_name = normalize_name(table.name());

		if self.tables.contains_key(&normalized_name) {
			return Err(SchemaError::DuplicateTable {
				database_name: database_name.to_string(),
				schema_name: self.name.clone(),
				table_name: table.name().to_string(),
			});
		}

		self.tables.insert(normalized_name, table);
		Ok(())
	}

	pub fn is_implicit(&self) -> bool {
		self.is_implicit
	}

	pub fn name(&self) -> &str {
		&self.name
	}

	pub fn sequence(&self, name: &str) -> Option<&SequenceSchema> {
		self.sequences.get(&normalize_name(name))
	}

	pub fn sequences(&self) -> impl Iterator<Item = &SequenceSchema> {
		self.sequences.values()
	}

	pub fn table(&self, name: &str) -> Option<&TableSchema> {
		self.tables.get(&normalize_name(name))
	}

	pub fn tables(&self) -> impl Iterator<Item = &TableSchema> {
		self.tables.values()
	}

	pub fn with_implicit(name: impl Into<String>) -> Self {
		Self {
			is_implicit: true,
			name: name.into(),
			sequences: BTreeMap::new(),
			tables: BTreeMap::new(),
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DatabaseSchema {
	backend: DatabaseBackend,
	name: String,
	schemas: BTreeMap<String, DatabaseNamespace>,
}

impl DatabaseSchema {
	pub fn new(name: impl Into<String>) -> Self {
		Self::with_backend(name, DatabaseBackend::PostgreSql)
	}

	pub fn with_backend(name: impl Into<String>, backend: DatabaseBackend) -> Self {
		Self {
			backend,
			name: name.into(),
			schemas: BTreeMap::new(),
		}
	}

	pub fn add_schema(&mut self, schema: DatabaseNamespace) -> Result<(), SchemaError> {
		let normalized_name = normalize_name(schema.name());

		if self.schemas.contains_key(&normalized_name) {
			return Err(SchemaError::DuplicateSchema {
				database_name: self.name.clone(),
				schema_name: schema.name().to_string(),
			});
		}

		self.schemas.insert(normalized_name, schema);
		Ok(())
	}

	pub fn backend(&self) -> DatabaseBackend {
		self.backend
	}

	pub fn name(&self) -> &str {
		&self.name
	}

	pub fn schema(&self, name: &str) -> Option<&DatabaseNamespace> {
		self.schemas.get(&normalize_name(name))
	}

	pub fn schema_mut(&mut self, name: &str) -> Option<&mut DatabaseNamespace> {
		self.schemas.get_mut(&normalize_name(name))
	}

	pub fn schemas(&self) -> impl Iterator<Item = &DatabaseNamespace> {
		self.schemas.values()
	}

	pub fn set_backend(&mut self, backend: DatabaseBackend) {
		self.backend = backend;
	}

	fn allows_database_table_shorthand(&self) -> bool {
		self.schemas.len() == 1
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedSequence<'a> {
	database: &'a DatabaseSchema,
	schema: &'a DatabaseNamespace,
	sequence: &'a SequenceSchema,
}

impl<'a> ResolvedSequence<'a> {
	pub fn database(&self) -> &'a DatabaseSchema {
		self.database
	}

	pub fn schema(&self) -> &'a DatabaseNamespace {
		self.schema
	}

	pub fn sequence(&self) -> &'a SequenceSchema {
		self.sequence
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedTable<'a> {
	database: &'a DatabaseSchema,
	schema: &'a DatabaseNamespace,
	table: &'a TableSchema,
}

impl<'a> ResolvedTable<'a> {
	pub fn database(&self) -> &'a DatabaseSchema {
		self.database
	}

	pub fn schema(&self) -> &'a DatabaseNamespace {
		self.schema
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

	pub fn database_mut(&mut self, name: &str) -> Option<&mut DatabaseSchema> {
		self.databases.get_mut(&normalize_name(name))
	}

	pub fn databases(&self) -> impl Iterator<Item = &DatabaseSchema> {
		self.databases.values()
	}

	pub fn resolve_database_schema_sequence<'a>(
		&'a self,
		active_databases: &[&str],
		database_name: &str,
		schema_name: &str,
		sequence_name: &str,
	) -> Result<ResolvedSequence<'a>, SchemaError> {
		let database = self.active_database(active_databases, database_name)?;
		let schema = database.schema(schema_name).ok_or(SchemaError::UnknownSchema {
			database_name: Some(database.name().to_string()),
			schema_name: schema_name.to_string(),
		})?;
		let sequence = schema.sequence(sequence_name).ok_or(SchemaError::UnknownSequence {
			sequence_name: format!("{}.{}.{}", database.name(), schema_name, sequence_name),
		})?;

		Ok(ResolvedSequence { database, schema, sequence })
	}

	pub fn resolve_database_schema_table<'a>(
		&'a self,
		active_databases: &[&str],
		database_name: &str,
		schema_name: &str,
		table_name: &str,
	) -> Result<ResolvedTable<'a>, SchemaError> {
		let database = self.active_database(active_databases, database_name)?;
		let schema = database.schema(schema_name).ok_or(SchemaError::UnknownSchema {
			database_name: Some(database.name().to_string()),
			schema_name: schema_name.to_string(),
		})?;
		let table = schema.table(table_name).ok_or(SchemaError::UnknownTable {
			table_name: format!("{}.{}.{}", database.name(), schema_name, table_name),
		})?;

		Ok(ResolvedTable { database, schema, table })
	}

	pub fn resolve_database_sequence<'a>(
		&'a self,
		active_databases: &[&str],
		database_name: &str,
		sequence_name: &str,
	) -> Result<ResolvedSequence<'a>, SchemaError> {
		let database = self.active_database(active_databases, database_name)?;

		if !database.allows_database_table_shorthand() {
			return Err(SchemaError::AmbiguousDatabaseQualifiedSequenceName {
				database_name: database.name().to_string(),
				sequence_name: sequence_name.to_string(),
			});
		}

		let schema = database.schemas().next().unwrap();
		let sequence = schema.sequence(sequence_name).ok_or(SchemaError::UnknownSequence {
			sequence_name: format!("{}.{}", database.name(), sequence_name),
		})?;

		Ok(ResolvedSequence { database, schema, sequence })
	}

	pub fn resolve_database_table<'a>(
		&'a self,
		active_databases: &[&str],
		database_name: &str,
		table_name: &str,
	) -> Result<ResolvedTable<'a>, SchemaError> {
		let database = self.active_database(active_databases, database_name)?;

		if !database.allows_database_table_shorthand() {
			return Err(SchemaError::AmbiguousDatabaseQualifiedTableName {
				database_name: database.name().to_string(),
				table_name: table_name.to_string(),
			});
		}

		let schema = database.schemas().next().unwrap();
		let table = schema.table(table_name).ok_or(SchemaError::UnknownTable {
			table_name: format!("{}.{}", database.name(), table_name),
		})?;

		Ok(ResolvedTable { database, schema, table })
	}

	pub fn resolve_schema_sequence<'a>(
		&'a self,
		active_databases: &[&str],
		schema_name: &str,
		sequence_name: &str,
	) -> Result<ResolvedSequence<'a>, SchemaError> {
		let mut matches = Vec::new();

		for database_name in active_databases {
			let database = self.active_database(active_databases, database_name)?;

			if let Some(schema) = database.schema(schema_name)
				&& let Some(sequence) = schema.sequence(sequence_name) {
				matches.push(ResolvedSequence { database, schema, sequence });
			}
		}

		match matches.len() {
			0 => Err(SchemaError::UnknownSequence {
				sequence_name: format!("{schema_name}.{sequence_name}"),
			}),
			1 => Ok(matches.pop().unwrap()),
			_ => Err(SchemaError::AmbiguousSchemaQualifiedSequenceName {
				active_databases: active_databases.iter().map(|name| (*name).to_string()).collect(),
				schema_name: schema_name.to_string(),
				sequence_name: sequence_name.to_string(),
			}),
		}
	}

	pub fn resolve_schema_table<'a>(
		&'a self,
		active_databases: &[&str],
		schema_name: &str,
		table_name: &str,
	) -> Result<ResolvedTable<'a>, SchemaError> {
		let mut matches = Vec::new();

		for database_name in active_databases {
			let database = self.active_database(active_databases, database_name)?;

			if let Some(schema) = database.schema(schema_name)
				&& let Some(table) = schema.table(table_name) {
				matches.push(ResolvedTable { database, schema, table });
			}
		}

		match matches.len() {
			0 => Err(SchemaError::UnknownTable {
				table_name: format!("{schema_name}.{table_name}"),
			}),
			1 => Ok(matches.pop().unwrap()),
			_ => Err(SchemaError::AmbiguousSchemaQualifiedTableName {
				active_databases: active_databases.iter().map(|name| (*name).to_string()).collect(),
				schema_name: schema_name.to_string(),
				table_name: table_name.to_string(),
			}),
		}
	}

	pub fn resolve_sequence<'a>(
		&'a self,
		active_databases: &[&str],
		sequence_name: &str,
	) -> Result<ResolvedSequence<'a>, SchemaError> {
		let mut matches = Vec::new();

		for database_name in active_databases {
			let database = self.active_database(active_databases, database_name)?;

			for schema in database.schemas() {
				if let Some(sequence) = schema.sequence(sequence_name) {
					matches.push(ResolvedSequence { database, schema, sequence });
				}
			}
		}

		match matches.len() {
			0 => Err(SchemaError::UnknownSequence {
				sequence_name: sequence_name.to_string(),
			}),
			1 => Ok(matches.pop().unwrap()),
			_ => Err(SchemaError::AmbiguousSequenceName {
				active_databases: active_databases.iter().map(|name| (*name).to_string()).collect(),
				sequence_name: sequence_name.to_string(),
			}),
		}
	}

	pub fn resolve_table<'a>(
		&'a self,
		active_databases: &[&str],
		table_name: &str,
	) -> Result<ResolvedTable<'a>, SchemaError> {
		let mut matches = Vec::new();

		for database_name in active_databases {
			let database = self.active_database(active_databases, database_name)?;

			for schema in database.schemas() {
				if let Some(table) = schema.table(table_name) {
					matches.push(ResolvedTable { database, schema, table });
				}
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

	fn active_database<'a>(
		&'a self,
		active_databases: &[&str],
		database_name: &str,
	) -> Result<&'a DatabaseSchema, SchemaError> {
		if !active_databases.iter().any(|active| active.eq_ignore_ascii_case(database_name)) {
			return Err(SchemaError::UnknownDatabase {
				database_name: database_name.to_string(),
			});
		}

		self.database(database_name).ok_or(SchemaError::UnknownDatabase {
			database_name: database_name.to_string(),
		})
	}
}

impl Default for SchemaCatalog {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SequenceSchema {
	name: String,
}

impl SequenceSchema {
	pub fn new(name: impl Into<String>) -> Self {
		Self {
			name: name.into(),
		}
	}

	pub fn name(&self) -> &str {
		&self.name
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
	use super::DatabaseNamespace;
	use super::DatabaseSchema;
	use super::ResolvedSequence;
	use super::ResolvedTable;
	use super::SchemaCatalog;
	use super::SchemaDataType;
	use super::SchemaError;
	use super::SequenceSchema;
	use super::TableSchema;

	fn resolved_names(resolved: ResolvedTable<'_>) -> (String, String, String) {
		(
			resolved.database().name().to_string(),
			resolved.schema().name().to_string(),
			resolved.table().name().to_string(),
		)
	}

	fn resolved_sequence_names(resolved: ResolvedSequence<'_>) -> (String, String, String) {
		(
			resolved.database().name().to_string(),
			resolved.schema().name().to_string(),
			resolved.sequence().name().to_string(),
		)
	}

	fn schema_with_sequence(name: &str, sequence: &str) -> DatabaseNamespace {
		let mut schema = DatabaseNamespace::new(name);
		schema.add_sequence(SequenceSchema::new(sequence), "ExampleDb").unwrap();
		schema
	}

	fn schema_with_table(name: &str, table: &str) -> DatabaseNamespace {
		let mut schema = DatabaseNamespace::new(name);
		schema.add_table(table_with_id(table), "ExampleDb").unwrap();
		schema
	}

	fn table_with_id(name: &str) -> TableSchema {
		let mut table = TableSchema::new(name);
		table.add_column(ColumnSchema::new("Id", SchemaDataType::Int, false)).unwrap();
		table
	}

	#[test]
	fn allows_table_and_sequence_to_share_a_name() {
		let mut schema = DatabaseNamespace::new("Public");
		schema.add_table(table_with_id("Customers"), "ExampleDb").unwrap();
		schema.add_sequence(SequenceSchema::new("Customers"), "ExampleDb").unwrap();

		let mut database = DatabaseSchema::new("ExampleDb");
		database.add_schema(schema).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(database).unwrap();

		let resolved_table = catalog.resolve_table(&["exampledb"], "customers").unwrap();
		let resolved_sequence = catalog.resolve_sequence(&["exampledb"], "customers").unwrap();

		assert_eq!(resolved_table.table().name(), "Customers");
		assert_eq!(resolved_sequence.sequence().name(), "Customers");
	}

	#[test]
	fn rejects_database_table_shorthand_for_multi_schema_database() {
		let mut database = DatabaseSchema::new("ExampleDb");
		database.add_schema(schema_with_table("Public", "Customers")).unwrap();
		database.add_schema(schema_with_table("Archive", "Customers")).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(database).unwrap();

		let error = catalog.resolve_database_table(&["exampledb"], "ExampleDb", "Customers").unwrap_err();

		assert_eq!(error, SchemaError::AmbiguousDatabaseQualifiedTableName {
			database_name: String::from("ExampleDb"),
			table_name: String::from("Customers"),
		});
	}

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
	fn rejects_duplicate_schema_names_case_insensitively() {
		let mut database = DatabaseSchema::new("ExampleDb");
		database.add_schema(DatabaseNamespace::new("Public")).unwrap();

		let error = database.add_schema(DatabaseNamespace::new("public")).unwrap_err();

		assert_eq!(error, SchemaError::DuplicateSchema {
			database_name: String::from("ExampleDb"),
			schema_name: String::from("public"),
		});
	}

	#[test]
	fn rejects_duplicate_table_names_case_insensitively_within_schema() {
		let mut schema = DatabaseNamespace::new("Public");
		schema.add_table(TableSchema::new("Customers"), "ExampleDb").unwrap();

		let error = schema.add_table(TableSchema::new("customers"), "ExampleDb").unwrap_err();

		assert_eq!(error, SchemaError::DuplicateTable {
			database_name: String::from("ExampleDb"),
			schema_name: String::from("Public"),
			table_name: String::from("customers"),
		});
	}

	#[test]
	fn reports_ambiguous_schema_qualified_table_across_active_databases() {
		let mut sales = DatabaseSchema::new("Sales");
		sales.add_schema(schema_with_table("Public", "Customers")).unwrap();

		let mut archive = DatabaseSchema::new("Archive");
		archive.add_schema(schema_with_table("Public", "Customers")).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(sales).unwrap();
		catalog.add_database(archive).unwrap();

		let error = catalog.resolve_schema_table(&["sales", "archive"], "public", "customers").unwrap_err();

		assert_eq!(error, SchemaError::AmbiguousSchemaQualifiedTableName {
			active_databases: vec![String::from("sales"), String::from("archive")],
			schema_name: String::from("public"),
			table_name: String::from("customers"),
		});
	}

	#[test]
	fn reports_ambiguous_unqualified_table_name_across_active_databases() {
		let mut sales = DatabaseSchema::new("Sales");
		sales.add_schema(schema_with_table("Public", "Customers")).unwrap();

		let mut archive = DatabaseSchema::new("Archive");
		archive.add_schema(schema_with_table("Public", "Customers")).unwrap();

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
	fn resolves_database_schema_table_and_column_names_case_insensitively() {
		let mut table = TableSchema::new("Customers");
		table.add_column(ColumnSchema::new("Name", SchemaDataType::Text, true)).unwrap();

		let mut schema = DatabaseNamespace::new("Public");
		schema.add_table(table, "ExampleDb").unwrap();

		let mut database = DatabaseSchema::new("ExampleDb");
		database.add_schema(schema).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(database).unwrap();

		let database = catalog.database("exampledb").unwrap();
		let schema = database.schema("public").unwrap();
		let table = schema.table("customers").unwrap();
		let column = table.column("name").unwrap();

		assert_eq!(database.name(), "ExampleDb");
		assert_eq!(schema.name(), "Public");
		assert_eq!(table.name(), "Customers");
		assert_eq!(column.name(), "Name");
		assert_eq!(column.data_type(), &SchemaDataType::Text);
		assert!(column.is_nullable());
	}

	#[test]
	fn resolves_database_table_shorthand_for_single_schema_database() {
		let mut schema = DatabaseNamespace::with_implicit("Main");
		schema.add_table(table_with_id("Customers"), "ExampleDb").unwrap();

		let mut database = DatabaseSchema::new("ExampleDb");
		database.add_schema(schema).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(database).unwrap();

		let resolved = catalog.resolve_database_table(&["exampledb"], "ExampleDb", "Customers").unwrap();

		assert_eq!(
			resolved_names(resolved),
			(String::from("ExampleDb"), String::from("Main"), String::from("Customers"))
		);
	}

	#[test]
	fn resolves_schema_qualified_table_across_active_databases() {
		let mut sales = DatabaseSchema::new("Sales");
		sales.add_schema(schema_with_table("Crm", "Customers")).unwrap();

		let mut archive = DatabaseSchema::new("Archive");
		archive.add_schema(schema_with_table("Legacy", "Customers")).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(sales).unwrap();
		catalog.add_database(archive).unwrap();

		let resolved = catalog.resolve_schema_table(&["sales", "archive"], "crm", "customers").unwrap();

		assert_eq!(
			resolved_names(resolved),
			(String::from("Sales"), String::from("Crm"), String::from("Customers"))
		);
	}

	#[test]
	fn resolves_unqualified_sequence_name_from_active_databases_when_unique() {
		let mut database = DatabaseSchema::new("Sales");
		database.add_schema(schema_with_sequence("Public", "CustomerId")).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(database).unwrap();

		let resolved = catalog.resolve_sequence(&["sales"], "customerid").unwrap();

		assert_eq!(
			resolved_sequence_names(resolved),
			(String::from("Sales"), String::from("Public"), String::from("CustomerId"))
		);
	}

	#[test]
	fn resolves_unqualified_table_name_from_active_databases_when_unique() {
		let mut database = DatabaseSchema::new("Sales");
		database.add_schema(schema_with_table("Public", "Customers")).unwrap();

		let mut catalog = SchemaCatalog::new();
		catalog.add_database(database).unwrap();

		let resolved = catalog.resolve_table(&["sales"], "customers").unwrap();

		assert_eq!(
			resolved_names(resolved),
			(String::from("Sales"), String::from("Public"), String::from("Customers"))
		);
	}
}
