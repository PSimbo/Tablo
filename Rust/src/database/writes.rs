use std::collections::BTreeMap;

use crate::ast::DataType;
use crate::sql::*;
use crate::value::{ RecordBuffer, Value };

use super::records::normalize_name;

// SQL rendering tests cover all dialects independently of enabled drivers.
#[allow(dead_code)]
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

/// Only materialized assignments are normalized. Deferred database fields retain
/// their original representation and are not eagerly read as a side effect of a write.
pub(super) fn normalize_float_assignments(record: &mut RecordBuffer, dialect: WriteDialect) -> Result<(), String> {
	use crate::float::FloatWidth;
	use crate::value::RecordFieldValue;

	for (name, field) in &mut record.fields {
		let RecordFieldValue::Materialized { value: Value::Float(value), .. } = field else {
			continue;
		};

		let width = if dialect == WriteDialect::Sqlite {
			FloatWidth::Binary64
		} else {
			*record.float_widths.get(name).ok_or_else(|| format!("Physical float width is unknown for column `{name}`."))?
		};

		if width == FloatWidth::Binary32 {
			value.0 = (value.0 as f32) as crate::float::NativeFloat;
		}

		if dialect == WriteDialect::MySql && !value.0.is_finite() {
			return Err(format!("MySQL cannot store non-finite float in column `{name}` after conversion to its physical width."));
		}

		if dialect == WriteDialect::Sqlite && value.0.is_nan() {
			return Err(format!("SQLite cannot store NaN in column `{name}`."));
		}
	}

	Ok(())
}

#[cfg(any(feature = "driver-mysql", feature = "driver-postgresql"))]
pub(super) fn needs_float_widths(record: &RecordBuffer) -> bool {
	record
		.fields
		.iter()
		.any(|(name, field)| field.data_type().without_nullability() == &DataType::Float && !record.float_widths.contains_key(name))
}

pub(super) fn create_record_write(record: &RecordBuffer, dialect: WriteDialect) -> Result<RecordWrite, String> {
	let mut parameters = Vec::with_capacity(record.column_names.len());
	let mut placeholders = Vec::with_capacity(record.column_names.len());

	for column_name in &record.column_names {
		let field = record_field(&record.fields, column_name)?;
		parameters.push(write_parameter(field)?);
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

pub(super) fn delete_record_write(record: &RecordBuffer, dialect: WriteDialect) -> Result<RecordWrite, String> {
	let identity_columns = identity_column_names(record);
	let mut parameters = Vec::with_capacity(identity_columns.len());
	let mut predicates = Vec::with_capacity(identity_columns.len());

	for column_name in identity_columns {
		let field = record_field(&record.original_fields, column_name)?;
		parameters.push(identity_parameter(field)?);
		let parameter = parameter(dialect, parameters.len(), field.data_type())?;
		predicates.push(null_safe_predicate(dialect, column_name, &parameter, field.data_type()));
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

pub(super) fn mark_record_created(mut record: RecordBuffer) -> RecordBuffer {
	record.is_dirty = false;
	record.original_fields = record.fields.clone();
	record.persisted = true;
	record
}

pub(super) fn mark_record_deleted(mut record: RecordBuffer) -> RecordBuffer {
	record.exists = false;
	record.fields.clear();
	record.is_dirty = false;
	record.locked = false;
	record.original_fields.clear();
	record.primary_key_column_names.clear();
	record.persisted = false;
	record
}

pub(super) fn mark_record_updated(mut record: RecordBuffer) -> RecordBuffer {
	record.is_dirty = false;
	record.original_fields = record.fields.clone();
	record
}

pub(super) fn update_record_write(record: &RecordBuffer, dialect: WriteDialect) -> Result<RecordWrite, String> {
	let identity_columns = identity_column_names(record);
	let assignment_columns = update_assignment_column_names(record);
	let mut parameters = Vec::with_capacity(assignment_columns.len() + identity_columns.len());
	let mut assignments = Vec::with_capacity(assignment_columns.len());

	for column_name in assignment_columns {
		let field = record_field(&record.fields, column_name)?;

		parameters.push(write_parameter(field)?);
		assignments.push(format!(
			"{} = {}",
			quoted_identifier(dialect, column_name),
			parameter(dialect, parameters.len(), field.data_type())?,
		));
	}

	let mut predicates = Vec::with_capacity(identity_columns.len());

	for column_name in identity_columns {
		let field = record_field(&record.original_fields, column_name)?;
		parameters.push(identity_parameter(field)?);
		let parameter = parameter(dialect, parameters.len(), field.data_type())?;
		predicates.push(null_safe_predicate(dialect, column_name, &parameter, field.data_type()));
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

// Keep the stored spelling for an unchanged document. Text storage can distinguish
// whitespace and property order when locating a row without a primary key.
fn write_parameter(field: &crate::value::RecordFieldValue) -> Result<Value, String> {
	use crate::value::{DatabaseValue, RecordFieldValue};

	if let RecordFieldValue::Deferred {
		data_type,
		value: DatabaseValue::Text(text) | DatabaseValue::NativeJson(text),
		..
	} = field && data_type.without_nullability() == &DataType::Json {
		return Ok(Value::Text(text.clone()));
	}

	let value = field.materialize()?;

	if field.data_type().without_nullability() == &DataType::Json && value != Value::Null {
		return crate::json::JsonValue::from_value(value).map(Value::Json);
	}

	Ok(value)
}

// A 32-bit runtime may round a fetched binary64 value for Tablo arithmetic.
// Row identity must still compare against the original database value.
fn identity_parameter(field: &crate::value::RecordFieldValue) -> Result<Value, String> {
	use crate::value::{DatabaseValue, RecordFieldValue};

	if let RecordFieldValue::Deferred { data_type, value, .. } = field {
		if data_type.without_nullability() == &DataType::Float {
			match value {
				DatabaseValue::Real(text) | DatabaseValue::Text(text) => {
					if let Ok(original) = text.parse::<f64>() {
						if original.is_finite() && (original as crate::float::NativeFloat) as f64 != original {
							return Ok(Value::Text(text.clone()));
						}
					}
				}
				DatabaseValue::Integer(value) => {
					return Ok(Value::Integer(*value));
				}
				_ => {}
			}
		}
	}

	write_parameter(field)
}

fn identity_column_names(record: &RecordBuffer) -> &[String] {
	if record.primary_key_column_names.is_empty() { &record.column_names } else { &record.primary_key_column_names }
}

fn null_safe_predicate(dialect: WriteDialect, column_name: &str, parameter: &str, ty: &DataType) -> String {
	let column_name = quoted_identifier(dialect, column_name);

	if dialect == WriteDialect::PostgreSql && ty.without_nullability() == &DataType::Json {
		// json preserves its input text. jsonb supplies its own output spelling.
		return format!("CAST({column_name} AS TEXT) IS NOT DISTINCT FROM CASE WHEN pg_typeof({column_name}) = 'jsonb'::regtype THEN CAST(CAST({parameter} AS JSONB) AS TEXT) ELSE CAST({parameter} AS TEXT) END");
	}

	match dialect {
		WriteDialect::MySql => format!("{column_name} <=> {parameter}"),
		WriteDialect::PostgreSql => format!("{column_name} IS NOT DISTINCT FROM {parameter}"),
		WriteDialect::Sqlite => format!("{column_name} IS {parameter}"),
	}
}

fn parameter(dialect: WriteDialect, index: usize, data_type: &DataType) -> Result<String, String> {
	match dialect {
		WriteDialect::MySql if data_type.without_nullability() == &DataType::Json => Ok(String::from("CAST(CONVERT(? USING utf8mb4) AS JSON)")),
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

fn table_source(record: &RecordBuffer, dialect: WriteDialect) -> String {
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

fn update_assignment_column_names(record: &RecordBuffer) -> Vec<&String> {
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
	fn normalizes_float_assignments_before_saving_identity() {
		use crate::float::FloatWidth;
		use crate::float::{Float, NativeFloat};

		let mut record = test_record();

		record.float_widths.insert("amount".into(), FloatWidth::Binary32);
		record.fields.insert("amount".into(), RecordFieldValue::Materialized { data_type: DataType::Float, value: Value::Float(Float(1.000000059604644775390625)) });

		normalize_float_assignments(&mut record, WriteDialect::PostgreSql).unwrap();
		assert_eq!(record.fields["amount"].materialize().unwrap(), Value::Float(Float(1.0)));

		let saved = mark_record_updated(record.clone());
		assert_eq!(saved.original_fields["amount"], saved.fields["amount"]);

		for (input, expected) in [("1e40", NativeFloat::INFINITY), ("-1e40", NativeFloat::NEG_INFINITY), ("-1e-99", -0.0)] {
			record.fields.insert("amount".into(), RecordFieldValue::Materialized { data_type: DataType::Float, value: Value::Float(Float::parse(input).unwrap()) });
			normalize_float_assignments(&mut record, WriteDialect::PostgreSql).unwrap();
			assert_eq!(record.fields["amount"].materialize().unwrap(), Value::Float(Float(expected)));
		}

		record.fields.insert("amount".into(), RecordFieldValue::Materialized { data_type: DataType::Float, value: Value::Float(Float::parse("1e40").unwrap()) });
		assert!(normalize_float_assignments(&mut record.clone(), WriteDialect::MySql).is_err());

		record.float_widths.clear();
		assert!(normalize_float_assignments(&mut record, WriteDialect::PostgreSql).is_err());
	}

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

	fn test_record() -> RecordBuffer {
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

		RecordBuffer {
			float_widths: BTreeMap::new(),
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
