use std::collections::BTreeMap;

use crate::ast::DataType;
use crate::sql::*;
use crate::value::{ RecordPointerValue, Value };

use super::records::normalize_name;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum WriteDialect {
	MySql,
	PostgreSql,
	Sqlite,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct RecordWrite {
	pub parameters: Vec<Value>,
	pub statement: String,
}

pub(super) fn create_record_write(record: &RecordPointerValue, dialect: WriteDialect) -> Result<RecordWrite, String> {
	let mut parameters = Vec::with_capacity(record.column_names.len());
	let mut placeholders = Vec::with_capacity(record.column_names.len());

	for column_name in &record.column_names {
		let field = record_field(&record.fields, column_name)?;
		parameters.push(field.materialize()?);
		placeholders.push(parameter(dialect, parameters.len(), field.data_type())?);
	}

	let column_list = record.column_names.iter()
		.map(|name| quoted_identifier(dialect, name))
		.collect::<Vec<_>>()
		.join(", ");
	let statement = format!(
		"INSERT INTO {} ({column_list}) VALUES ({})",
		table_source(record, dialect),
		placeholders.join(", "),
	);

	Ok(RecordWrite { parameters, statement })
}

pub(super) fn delete_record_write(record: &RecordPointerValue, dialect: WriteDialect) -> Result<RecordWrite, String> {
	let identity_columns = identity_column_names(record);
	let mut parameters = Vec::with_capacity(identity_columns.len());
	let mut predicates = Vec::with_capacity(identity_columns.len());

	for column_name in identity_columns {
		let field = record_field(&record.original_fields, column_name)?;
		parameters.push(field.materialize()?);
		let parameter = parameter(dialect, parameters.len(), field.data_type())?;
		predicates.push(null_safe_predicate(dialect, column_name, &parameter));
	}

	Ok(RecordWrite {
		parameters,
		statement: format!("DELETE FROM {} WHERE {}", table_source(record, dialect), predicates.join(" AND ")),
	})
}

pub(super) fn expect_one_affected_row(
	backend_name: &str,
	operation: &str,
	affected_rows: u64,
) -> Result<(), String> {
	if affected_rows == 1 {
		Ok(())
	}
	else {
		Err(format!("{backend_name} {operation} expected to affect 1 row, affected {affected_rows}."))
	}
}

pub(super) fn mark_record_created(mut record: RecordPointerValue) -> RecordPointerValue {
	record.is_dirty = false;
	record.original_fields = record.fields.clone();
	record.persisted = true;
	record
}

pub(super) fn mark_record_deleted(mut record: RecordPointerValue) -> RecordPointerValue {
	record.exists = false;
	record.fields.clear();
	record.is_dirty = false;
	record.locked = false;
	record.original_fields.clear();
	record.primary_key_column_names.clear();
	record.persisted = false;
	record
}

pub(super) fn mark_record_updated(mut record: RecordPointerValue) -> RecordPointerValue {
	record.is_dirty = false;
	record.original_fields = record.fields.clone();
	record
}

pub(super) fn update_record_write(record: &RecordPointerValue, dialect: WriteDialect) -> Result<RecordWrite, String> {
	let identity_columns = identity_column_names(record);
	let assignment_columns = update_assignment_column_names(record);
	let mut parameters = Vec::with_capacity(assignment_columns.len() + identity_columns.len());
	let mut assignments = Vec::with_capacity(assignment_columns.len());

	for column_name in assignment_columns {
		let field = record_field(&record.fields, column_name)?;
		parameters.push(field.materialize()?);
		assignments.push(format!(
			"{} = {}",
			quoted_identifier(dialect, column_name),
			parameter(dialect, parameters.len(), field.data_type())?,
		));
	}

	let mut predicates = Vec::with_capacity(identity_columns.len());

	for column_name in identity_columns {
		let field = record_field(&record.original_fields, column_name)?;
		parameters.push(field.materialize()?);
		let parameter = parameter(dialect, parameters.len(), field.data_type())?;
		predicates.push(null_safe_predicate(dialect, column_name, &parameter));
	}

	Ok(RecordWrite {
		parameters,
		statement: format!(
			"UPDATE {} SET {} WHERE {}",
			table_source(record, dialect),
			assignments.join(", "),
			predicates.join(" AND "),
		),
	})
}

fn identity_column_names(record: &RecordPointerValue) -> &[String] {
	if record.primary_key_column_names.is_empty() { &record.column_names } else { &record.primary_key_column_names }
}

fn null_safe_predicate(dialect: WriteDialect, column_name: &str, parameter: &str) -> String {
	let column_name = quoted_identifier(dialect, column_name);

	match dialect {
		WriteDialect::MySql => format!("{column_name} <=> {parameter}"),
		WriteDialect::PostgreSql => format!("{column_name} IS NOT DISTINCT FROM {parameter}"),
		WriteDialect::Sqlite => format!("{column_name} IS {parameter}"),
	}
}

fn parameter(dialect: WriteDialect, index: usize, data_type: &DataType) -> Result<String, String> {
	match dialect {
		WriteDialect::MySql | WriteDialect::Sqlite => Ok(String::from("?")),
		WriteDialect::PostgreSql => Ok(format!(
			"CAST(CAST(${index} AS TEXT) AS {})",
			postgresql_type_name(data_type).ok_or_else(|| {
				format!("PostgreSQL cannot store a record field of type `{}` yet.", data_type.name())
			})?,
		)),
	}
}

fn quoted_identifier(dialect: WriteDialect, identifier: &str) -> String {
	match dialect {
		WriteDialect::MySql => quote_mysql_identifier(identifier),
		WriteDialect::PostgreSql | WriteDialect::Sqlite => quote_identifier(identifier),
	}
}

fn record_field<'a>(
	fields: &'a BTreeMap<String, crate::value::RecordFieldValue>,
	column_name: &str,
) -> Result<&'a crate::value::RecordFieldValue, String> {
	fields.get(&normalize_name(column_name)).ok_or_else(|| {
		format!("Record pointer does not contain field data for `{column_name}`.")
	})
}

fn table_source(record: &RecordPointerValue, dialect: WriteDialect) -> String {
	let table_name = quoted_identifier(dialect, &record.record_type.table_name);

	if record.schema_is_implicit {
		table_name
	}
	else {
		format!(
			"{}.{}",
			quoted_identifier(dialect, &record.record_type.schema_name),
			table_name,
		)
	}
}

fn update_assignment_column_names(record: &RecordPointerValue) -> Vec<&String> {
	let available_columns = record.column_names.iter()
		.filter(|column_name| record.fields.contains_key(&normalize_name(column_name)))
		.collect::<Vec<_>>();

	if !record.is_dirty {
		return available_columns;
	}

	let changed_columns = available_columns.iter().copied()
		.filter(|column_name| {
			let normalized_name = normalize_name(column_name);
			record.fields.get(&normalized_name) != record.original_fields.get(&normalized_name)
		})
		.collect::<Vec<_>>();

	if changed_columns.is_empty() {
		available_columns
	}
	else {
		changed_columns
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	use crate::ast::*;
	use crate::value::*;

	#[test]
	fn deleted_record_no_longer_references_persisted_data() {
		let record = mark_record_deleted(test_record());

		assert!(!record.exists);
		assert!(!record.persisted);
		assert!(record.fields.is_empty());
		assert!(record.original_fields.is_empty());
	}

	#[test]
	fn falls_back_to_all_original_columns_without_a_primary_key() {
		let mut record = test_record();
		record.primary_key_column_names.clear();
		let write = delete_record_write(&record, WriteDialect::PostgreSql).unwrap();

		assert_eq!(
			write.statement,
			"DELETE FROM \"app\".\"Customers\" WHERE \"Id\" IS NOT DISTINCT FROM CAST(CAST($1 AS TEXT) AS BIGINT) AND \"Name\" IS NOT DISTINCT FROM CAST(CAST($2 AS TEXT) AS TEXT)",
		);
		assert_eq!(write.parameters, vec![Value::Integer(1), Value::Text(String::from("Acme"))]);
	}

	#[test]
	fn renders_mysql_delete_with_null_safe_primary_key_predicate() {
		let record = test_record();
		let write = delete_record_write(&record, WriteDialect::MySql).unwrap();

		assert_eq!(write.statement, "DELETE FROM `app`.`Customers` WHERE `Id` <=> ?");
		assert_eq!(write.parameters, vec![Value::Integer(1)]);
	}

	#[test]
	fn renders_postgresql_update_with_typed_parameters() {
		let record = test_record();
		let write = update_record_write(&record, WriteDialect::PostgreSql).unwrap();

		assert_eq!(
			write.statement,
			"UPDATE \"app\".\"Customers\" SET \"Id\" = CAST(CAST($1 AS TEXT) AS BIGINT), \"Name\" = CAST(CAST($2 AS TEXT) AS TEXT) WHERE \"Id\" IS NOT DISTINCT FROM CAST(CAST($3 AS TEXT) AS BIGINT)",
		);
		assert_eq!(
			write.parameters,
			vec![Value::Integer(1), Value::Text(String::from("Acme")), Value::Integer(1)],
		);
	}

	#[test]
	fn renders_sqlite_create_with_positional_parameters() {
		let record = test_record();
		let write = create_record_write(&record, WriteDialect::Sqlite).unwrap();

		assert_eq!(
			write.statement,
			"INSERT INTO \"app\".\"Customers\" (\"Id\", \"Name\") VALUES (?, ?)",
		);
		assert_eq!(write.parameters, vec![Value::Integer(1), Value::Text(String::from("Acme"))]);
	}

	#[test]
	fn successful_create_and_update_refresh_original_field_values() {
		let mut pending = test_record();
		pending.persisted = false;
		pending.original_fields.clear();
		let created = mark_record_created(pending);

		assert!(created.persisted);
		assert!(!created.is_dirty);
		assert_eq!(created.original_fields, created.fields);

		let mut changed = created;
		changed.is_dirty = true;
		let updated = mark_record_updated(changed);

		assert!(!updated.is_dirty);
		assert_eq!(updated.original_fields, updated.fields);
	}

	fn test_record() -> RecordPointerValue {
		let fields = BTreeMap::from([
			(
				String::from("id"),
				RecordFieldValue::Materialized {
					data_type: DataType::Int,
					value: Value::Integer(1),
				},
			),
			(
				String::from("name"),
				RecordFieldValue::Materialized {
					data_type: DataType::Text,
					value: Value::Text(String::from("Acme")),
				},
			),
		]);

		RecordPointerValue {
			column_names: vec![String::from("Id"), String::from("Name")],
			exists: true,
			fields: fields.clone(),
			group_boundaries: BTreeMap::new(),
			is_dirty: true,
			locked: false,
			original_fields: fields,
			primary_key_column_names: vec![String::from("Id")],
			projected_values: BTreeMap::new(),
			persisted: true,
			record_type: RecordPointerType {
				database_name: String::from("ExampleDb"),
				schema_name: String::from("app"),
				table_name: String::from("Customers"),
			},
			schema_is_implicit: false,
		}
	}

	#[test]
	fn updates_only_changed_fields_on_partially_loaded_records() {
		let mut record = test_record();
		record.column_names.push(String::from("Notes"));
		record.fields.insert(
			String::from("name"),
			RecordFieldValue::Materialized {
				data_type: DataType::Text,
				value: Value::Text(String::from("Updated")),
			},
		);
		let write = update_record_write(&record, WriteDialect::Sqlite).unwrap();

		assert_eq!(write.statement, "UPDATE \"app\".\"Customers\" SET \"Name\" = ? WHERE \"Id\" IS ?");
		assert_eq!(write.parameters, vec![Value::Text(String::from("Updated")), Value::Integer(1)]);
	}
}
