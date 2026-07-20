use std::collections::BTreeMap;
use std::path::PathBuf;

use rusqlite::Connection;
use rusqlite::params_from_iter;
use rusqlite::types::Value as SqlValue;
use rusqlite::types::ValueRef;

use crate::query::QueryResultColumn;
use crate::query::SqlGroupByItem;
use crate::query::SqlQuery;
use crate::query::SqlQueryResultShape;
use crate::sql::quote_identifier;
use crate::sql::table_source;
use crate::value::DatabaseValue;
use crate::value::RecordFieldValue;
use crate::value::RecordGroupBoundary;
use crate::value::RecordPointerValue;
use crate::value::Value;
use crate::value::database_record_field_runtime_value;

use super::runtime::DatabaseDriver;

pub(super) struct SqliteSession {
	connection: Connection,
	transaction_depth: usize,
}

impl SqliteSession {
	pub fn open(database_name: &str, connection_string: &str) -> Result<Self, String> {
		let path = path_from_connection_string(database_name, connection_string)?;
		let connection = Connection::open(&path).map_err(|error| {
			format!("Failed to open SQLite database `{}`: {error}", path.display())
		})?;

		Ok(Self {
			connection,
			transaction_depth: 0,
		})
	}
}

impl DatabaseDriver for SqliteSession {
	fn commit_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String> {
		if self.transaction_depth < target_depth {
			return Ok(());
		}

		if target_depth == 1 {
			self.connection.execute_batch("COMMIT")
				.map_err(|error| format!("Failed to commit SQLite transaction: {error}"))?;
		}
		else {
			self.connection.execute_batch(&format!("RELEASE SAVEPOINT {}", quote_identifier(savepoint_name)))
				.map_err(|error| format!("Failed to release SQLite transaction savepoint: {error}"))?;
		}

		self.transaction_depth -= 1;
		Ok(())
	}

	fn create_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let table_source = table_source(&record.record_type.schema_name, &record.record_type.table_name, record.schema_is_implicit);
		let column_list = record.column_names.iter().map(|name| quote_identifier(name)).collect::<Vec<_>>().join(", ");
		let placeholders = std::iter::repeat("?").take(record.column_names.len()).collect::<Vec<_>>().join(", ");
		let statement = format!("INSERT INTO {table_source} ({column_list}) VALUES ({placeholders})");
		let parameter_values = record.column_names.iter()
			.map(|column_name| {
				let field = record.fields.get(&normalize_name(column_name)).ok_or_else(|| {
					format!("Record pointer does not contain a field named `{column_name}`.")
				})?;
				runtime_value_to_sqlite(field.materialize()?)
			})
			.collect::<Result<Vec<_>, _>>()?;

		self.connection.execute(&statement, params_from_iter(parameter_values))
			.map_err(|error| format!("Failed to execute SQLite create statement: {error}"))?;
		let original_fields = record.fields.clone();

		Ok(RecordPointerValue {
			is_dirty: false,
			original_fields,
			persisted: true,
			..record
		})
	}

	fn delete_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let table_source = table_source(&record.record_type.schema_name, &record.record_type.table_name, record.schema_is_implicit);
		let predicate_column_names = identity_column_names(&record);
		let predicates = predicate_column_names.iter()
			.map(|name| format!("{} IS ?", quote_identifier(name)))
			.collect::<Vec<_>>()
			.join(" AND ");
		let statement = format!("DELETE FROM {table_source} WHERE {predicates}");
		let parameter_values = original_identity_values(&record, &predicate_column_names)?;
		let affected_rows = self.connection.execute(&statement, params_from_iter(parameter_values))
			.map_err(|error| format!("Failed to execute SQLite delete statement: {error}"))?;

		if affected_rows != 1 {
			return Err(format!("SQLite delete expected to affect 1 row, affected {affected_rows}."));
		}

		Ok(RecordPointerValue {
			exists: false,
			fields: BTreeMap::new(),
			is_dirty: false,
			locked: false,
			original_fields: BTreeMap::new(),
			primary_key_column_names: Vec::new(),
			persisted: false,
			..record
		})
	}

	fn execute_query(&mut self, query: &SqlQuery, parameters: Vec<Value>) -> Result<Value, String> {
		let parameters = parameters.into_iter().map(runtime_value_to_sqlite).collect::<Result<Vec<_>, _>>()?;
		let mut statement = self.connection.prepare(&query.statement)
			.map_err(|error| format!("Failed to prepare SQLite query: {error}"))?;

		match &query.result_shape {
			SqlQueryResultShape::IntegerScalar => {
				let result = statement.query_row(params_from_iter(parameters), |row| row.get::<_, i64>(0))
					.map_err(|error| format!("Failed to execute SQLite query: {error}"))?;
				Ok(Value::Integer(result))
			}
			SqlQueryResultShape::RecordPointer(columns) => {
				let mut rows = statement.query(params_from_iter(parameters))
					.map_err(|error| format!("Failed to execute SQLite query: {error}"))?;
				let Some(row) = rows.next().map_err(|error| format!("Failed to read SQLite query result: {error}"))? else {
					return Ok(Value::RecordPointer(empty_record_pointer(query, columns)));
				};
				let fields = load_record_fields(row, columns)?;
				let original_fields = fields.clone();
				Ok(Value::RecordPointer(record_pointer(query, columns, fields, original_fields, BTreeMap::new())))
			}
			SqlQueryResultShape::RecordPointerArray(columns) => {
				let mut rows = statement.query(params_from_iter(parameters))
					.map_err(|error| format!("Failed to execute SQLite query: {error}"))?;
				let mut loaded_records = Vec::new();

				while let Some(row) = rows.next().map_err(|error| format!("Failed to read SQLite query result: {error}"))? {
					let fields = load_record_fields(row, columns)?;
					loaded_records.push(LoadedRecord {
						group_keys: load_group_keys(row, columns.len(), &query.group_by)?,
						original_fields: fields.clone(),
						fields,
					});
				}

				let boundaries = record_group_boundaries(&loaded_records, &query.group_by);
				let records = loaded_records.into_iter().enumerate()
					.map(|(index, loaded)| Value::RecordPointer(record_pointer(
						query,
						columns,
						loaded.fields,
						loaded.original_fields,
						boundaries.get(index).cloned().unwrap_or_default(),
					)))
					.collect();
				Ok(Value::Array(records))
			}
		}
	}

	fn load_sequence_current(&mut self, schema_is_implicit: bool, schema_name: &str, sequence_name: &str) -> Result<i64, String> {
		let statement = format!("SELECT seq FROM {} WHERE name = ?1", sequence_source(schema_name, schema_is_implicit));
		let result = self.connection.query_row(&statement, [sequence_name], |row| {
			let value = row.get_ref(0)?;
			sqlite_integer(value).map_err(|message| rusqlite::Error::FromSqlConversionFailure(
				0,
				value.data_type(),
				Box::new(std::io::Error::new(std::io::ErrorKind::InvalidData, message)),
			))
		});

		match result {
			Ok(value) => Ok(value),
			Err(rusqlite::Error::QueryReturnedNoRows) => Ok(0),
			Err(error) => Err(format!("Failed to read SQLite sequence `{sequence_name}`: {error}")),
		}
	}

	fn rollback_transaction(&mut self, target_depth: usize, savepoint_name: &str) -> Result<(), String> {
		if self.transaction_depth < target_depth {
			return Ok(());
		}

		if target_depth == 1 {
			self.connection.execute_batch("ROLLBACK")
				.map_err(|error| format!("Failed to roll back SQLite transaction: {error}"))?;
		}
		else {
			let savepoint = quote_identifier(savepoint_name);
			self.connection.execute_batch(&format!("ROLLBACK TO SAVEPOINT {savepoint}; RELEASE SAVEPOINT {savepoint}"))
				.map_err(|error| format!("Failed to roll back SQLite transaction savepoint: {error}"))?;
		}

		self.transaction_depth -= 1;
		Ok(())
	}

	fn store_sequence_current(
		&mut self,
		schema_is_implicit: bool,
		schema_name: &str,
		sequence_name: &str,
		value: i64,
	) -> Result<(), String> {
		let sequence_source = sequence_source(schema_name, schema_is_implicit);
		let update_statement = format!("UPDATE {sequence_source} SET seq = ?1 WHERE name = ?2");
		let updated_rows = self.connection.execute(&update_statement, (&value, sequence_name))
			.map_err(|error| format!("Failed to update SQLite sequence `{sequence_name}`: {error}"))?;

		if updated_rows == 0 {
			let insert_statement = format!("INSERT INTO {sequence_source} (name, seq) VALUES (?1, ?2)");
			self.connection.execute(&insert_statement, (sequence_name, value))
				.map_err(|error| format!("Failed to initialize SQLite sequence `{sequence_name}`: {error}"))?;
		}

		Ok(())
	}

	fn sync_transactions(&mut self, transaction_names: &[String]) -> Result<(), String> {
		while self.transaction_depth < transaction_names.len() {
			if self.transaction_depth == 0 {
				self.connection.execute_batch("BEGIN")
					.map_err(|error| format!("Failed to begin SQLite transaction: {error}"))?;
			}
			else {
				self.connection.execute_batch(&format!(
					"SAVEPOINT {}",
					quote_identifier(&transaction_names[self.transaction_depth]),
				)).map_err(|error| format!("Failed to create SQLite transaction savepoint: {error}"))?;
			}

			self.transaction_depth += 1;
		}

		Ok(())
	}

	fn update_record(&mut self, record: RecordPointerValue) -> Result<RecordPointerValue, String> {
		let table_source = table_source(&record.record_type.schema_name, &record.record_type.table_name, record.schema_is_implicit);
		let assignments = record.column_names.iter().map(|name| format!("{} = ?", quote_identifier(name))).collect::<Vec<_>>().join(", ");
		let predicate_column_names = identity_column_names(&record);
		let predicates = predicate_column_names.iter().map(|name| format!("{} IS ?", quote_identifier(name))).collect::<Vec<_>>().join(" AND ");
		let statement = format!("UPDATE {table_source} SET {assignments} WHERE {predicates}");
		let mut parameter_values = Vec::with_capacity(record.column_names.len() + predicate_column_names.len());

		for column_name in &record.column_names {
			let field = record.fields.get(&normalize_name(column_name)).ok_or_else(|| {
				format!("Record pointer does not contain a field named `{column_name}`.")
			})?;
			parameter_values.push(runtime_value_to_sqlite(field.materialize()?)?);
		}

		parameter_values.extend(original_identity_values(&record, &predicate_column_names)?);
		let affected_rows = self.connection.execute(&statement, params_from_iter(parameter_values))
			.map_err(|error| format!("Failed to execute SQLite update statement: {error}"))?;

		if affected_rows != 1 {
			return Err(format!("SQLite update expected to affect 1 row, affected {affected_rows}."));
		}

		let original_fields = record.fields.clone();
		Ok(RecordPointerValue { is_dirty: false, original_fields, ..record })
	}
}

struct LoadedRecord {
	fields: BTreeMap<String, RecordFieldValue>,
	group_keys: Vec<Value>,
	original_fields: BTreeMap<String, RecordFieldValue>,
}

fn database_value(value: ValueRef<'_>) -> Result<DatabaseValue, String> {
	match value {
		ValueRef::Blob(value) => Ok(DatabaseValue::Blob(value.to_vec())),
		ValueRef::Integer(value) => Ok(DatabaseValue::Integer(value)),
		ValueRef::Null => Ok(DatabaseValue::Null),
		ValueRef::Real(value) => Ok(DatabaseValue::Real(value.to_string())),
		ValueRef::Text(value) => Ok(DatabaseValue::Text(std::str::from_utf8(value)
			.map_err(|_| String::from("SQLite returned invalid UTF-8 text data."))?.to_string())),
	}
}

fn empty_record_pointer(query: &SqlQuery, columns: &[QueryResultColumn]) -> RecordPointerValue {
	RecordPointerValue {
		column_names: columns.iter().map(|column| column.column_name.clone()).collect(),
		exists: false,
		fields: BTreeMap::new(),
		group_boundaries: BTreeMap::new(),
		is_dirty: false,
		locked: false,
		original_fields: BTreeMap::new(),
		primary_key_column_names: primary_key_column_names(columns),
		persisted: false,
		record_type: record_pointer_type(query),
		schema_is_implicit: query.schema_is_implicit,
	}
}

fn identity_column_names(record: &RecordPointerValue) -> Vec<String> {
	if record.primary_key_column_names.is_empty() { record.column_names.clone() } else { record.primary_key_column_names.clone() }
}

fn load_group_keys(row: &rusqlite::Row<'_>, column_count: usize, group_by: &[SqlGroupByItem]) -> Result<Vec<Value>, String> {
	group_by.iter().enumerate().map(|(index, item)| {
		let value = row.get_ref(column_count + index).map_err(|error| format!("Failed to read SQLite grouping column: {error}"))?;
		let value = database_value(value)?;
		database_record_field_runtime_value(&value, &item.data_type, item.data_type.is_nullable())
	}).collect()
}

fn load_record_fields(row: &rusqlite::Row<'_>, columns: &[QueryResultColumn]) -> Result<BTreeMap<String, RecordFieldValue>, String> {
	let mut fields = BTreeMap::new();

	for (index, column) in columns.iter().enumerate() {
		let value = row.get_ref(index).map_err(|error| format!("Failed to read SQLite column `{}`: {error}", column.column_name))?;
		fields.insert(normalize_name(&column.column_name), RecordFieldValue::Deferred {
			data_type: column.data_type.clone(),
			is_nullable: column.is_nullable,
			value: database_value(value)?,
		});
	}

	Ok(fields)
}

fn normalize_name(name: &str) -> String {
	name.to_ascii_lowercase()
}

fn original_identity_values(record: &RecordPointerValue, column_names: &[String]) -> Result<Vec<SqlValue>, String> {
	column_names.iter().map(|column_name| {
		let field = record.original_fields.get(&normalize_name(column_name)).ok_or_else(|| {
			format!("Record pointer is missing original field data for `{column_name}`.")
		})?;
		runtime_value_to_sqlite(field.materialize()?)
	}).collect()
}

fn path_from_connection_string(database_name: &str, connection_string: &str) -> Result<PathBuf, String> {
	let value = connection_string.strip_prefix("sqlite:").ok_or_else(|| {
		format!("SQLite database `{database_name}` has an invalid runtime connection string.")
	})?;

	if value.is_empty() {
		return Err(format!(
			"SQLite connection string for database `{database_name}` must include a database path."
		));
	}

	if value.starts_with("//") && !value.starts_with("///") {
		return Err(format!(
			"SQLite connection string for database `{database_name}` must use `sqlite:/path/to.db`, `sqlite:relative.db`, or `sqlite::memory:`."
		));
	}

	Ok(PathBuf::from(if value.starts_with("///") { &value[2..] } else { value }))
}

fn primary_key_column_names(columns: &[QueryResultColumn]) -> Vec<String> {
	columns.iter().filter(|column| column.is_primary_key).map(|column| column.column_name.clone()).collect()
}

fn record_group_boundaries(records: &[LoadedRecord], group_by: &[SqlGroupByItem]) -> Vec<BTreeMap<String, RecordGroupBoundary>> {
	let mut result = Vec::with_capacity(records.len());

	for index in 0..records.len() {
		let mut boundaries = BTreeMap::new();

		for group_index in 0..group_by.len() {
			let first = index == 0 || records[index - 1].group_keys[..=group_index] != records[index].group_keys[..=group_index];
			let last = index + 1 == records.len() || records[index + 1].group_keys[..=group_index] != records[index].group_keys[..=group_index];

			for key_name in &group_by[group_index].key_names {
				boundaries.insert(normalize_name(key_name), RecordGroupBoundary { first, last });
			}
		}

		result.push(boundaries);
	}

	result
}

fn record_pointer(
	query: &SqlQuery,
	columns: &[QueryResultColumn],
	fields: BTreeMap<String, RecordFieldValue>,
	original_fields: BTreeMap<String, RecordFieldValue>,
	group_boundaries: BTreeMap<String, RecordGroupBoundary>,
) -> RecordPointerValue {
	RecordPointerValue {
		column_names: columns.iter().map(|column| column.column_name.clone()).collect(),
		exists: true,
		fields,
		group_boundaries,
		is_dirty: false,
		locked: false,
		original_fields,
		primary_key_column_names: primary_key_column_names(columns),
		persisted: true,
		record_type: record_pointer_type(query),
		schema_is_implicit: query.schema_is_implicit,
	}
}

fn record_pointer_type(query: &SqlQuery) -> crate::ast::RecordPointerType {
	crate::ast::RecordPointerType {
		database_name: query.database_name.clone(),
		schema_name: query.schema_name.clone(),
		table_name: query.table_name.clone(),
	}
}

fn runtime_type_name(value: &Value) -> &'static str {
	match value {
		Value::Array(_) => "array",
		Value::Boolean(_) => "bool",
		Value::Date(_) => "date",
		Value::Decimal(_) => "dec",
		Value::DecimalRange(_) | Value::IntegerRange(_) => "range",
		Value::Enum(_) => "enum",
		Value::Integer(_) => "int",
		Value::Iterator(_) => "iterator",
		Value::Null => "null",
		Value::Object(_) => "object",
		Value::RecordPointer(_) => "record pointer",
		Value::Reference(_) => "reference",
		Value::Text(_) => "text",
		Value::Time(_) => "time",
		Value::TimeTz(_) => "timetz",
		Value::Timestamp(_) => "timestamp",
		Value::TimestampTz(_) => "timestamptz",
	}
}

fn runtime_value_to_sqlite(value: Value) -> Result<SqlValue, String> {
	match value {
		Value::Boolean(value) => Ok(SqlValue::Integer(if value { 1 } else { 0 })),
		Value::Date(value) => Ok(SqlValue::Text(value.to_string())),
		Value::Decimal(value) => Ok(SqlValue::Text(value.to_string())),
		Value::Integer(value) => Ok(SqlValue::Integer(value)),
		Value::Null => Ok(SqlValue::Null),
		Value::Text(value) => Ok(SqlValue::Text(value)),
		Value::Time(value) => Ok(SqlValue::Text(value.to_string())),
		Value::TimeTz(value) => Ok(SqlValue::Text(value.to_string())),
		Value::Timestamp(value) => Ok(SqlValue::Text(value.to_string())),
		Value::TimestampTz(value) => Ok(SqlValue::Text(value.to_string())),
		other => Err(format!("Cannot bind a `{}` value into a SQLite query parameter.", runtime_type_name(&other))),
	}
}

fn sequence_source(schema_name: &str, schema_is_implicit: bool) -> String {
	if schema_is_implicit { String::from("sqlite_sequence") } else { format!("{}.sqlite_sequence", quote_identifier(schema_name)) }
}

fn sqlite_integer(value: ValueRef<'_>) -> Result<i64, String> {
	match value {
		ValueRef::Integer(value) => Ok(value),
		ValueRef::Text(value) => {
			let text = std::str::from_utf8(value).map_err(|_| String::from("SQLite text value is not valid UTF-8."))?;
			text.parse::<i64>().map_err(|_| format!("SQLite value `{text}` cannot be converted to `int`."))
		}
		other => Err(format!("SQLite {} value cannot be converted to `int`.", sqlite_type_name(other))),
	}
}

fn sqlite_type_name(value: ValueRef<'_>) -> &'static str {
	match value {
		ValueRef::Blob(_) => "blob",
		ValueRef::Integer(_) => "integer",
		ValueRef::Null => "null",
		ValueRef::Real(_) => "real",
		ValueRef::Text(_) => "text",
	}
}

#[cfg(test)]
mod tests {
	use std::path::Path;

	use super::path_from_connection_string;

	#[test]
	fn parses_absolute_connection_path() {
		assert_eq!(
			path_from_connection_string("ExampleDb", "sqlite:///tmp/example.sqlite").unwrap(),
			Path::new("/tmp/example.sqlite"),
		);
	}

	#[test]
	fn parses_relative_connection_path() {
		assert_eq!(
			path_from_connection_string("ExampleDb", "sqlite:db/example.sqlite").unwrap(),
			Path::new("db/example.sqlite"),
		);
	}

	#[test]
	fn rejects_invalid_connection_path_shape() {
		let error = path_from_connection_string("ExampleDb", "sqlite://example.sqlite").unwrap_err();

		assert_eq!(
			error,
			"SQLite connection string for database `ExampleDb` must use `sqlite:/path/to.db`, `sqlite:relative.db`, or `sqlite::memory:`.",
		);
	}
}
