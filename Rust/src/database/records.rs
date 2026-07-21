use std::collections::BTreeMap;

use crate::query::*;
use crate::value::*;

pub(super) struct LoadedRecord {
	pub fields: BTreeMap<String, RecordFieldValue>,
	pub group_keys: Vec<Value>,
	pub original_fields: BTreeMap<String, RecordFieldValue>,
}

pub(super) fn empty_record_pointer(query: &SqlQuery, columns: &[QueryResultColumn]) -> RecordPointerValue {
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

pub(super) fn known_record_schema(layout: &QueryRecordLayout) -> Result<&[QueryResultColumn], String> {
	layout.known_schema().ok_or_else(|| {
		String::from("Runtime-determined database query schemas are not yet supported.")
	})
}

pub(super) fn locked_record_pointer(query: &SqlQuery, columns: &[QueryResultColumn]) -> RecordPointerValue {
	RecordPointerValue {
		column_names: columns.iter().map(|column| column.column_name.clone()).collect(),
		exists: true,
		fields: BTreeMap::new(),
		group_boundaries: BTreeMap::new(),
		is_dirty: false,
		locked: true,
		original_fields: BTreeMap::new(),
		primary_key_column_names: primary_key_column_names(columns),
		persisted: true,
		record_type: record_pointer_type(query),
		schema_is_implicit: query.schema_is_implicit,
	}
}

pub(super) fn normalize_name(name: &str) -> String {
	name.to_ascii_lowercase()
}

pub(super) fn record_group_boundaries(
	records: &[LoadedRecord],
	group_by: &[SqlGroupByItem],
) -> Vec<BTreeMap<String, RecordGroupBoundary>> {
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

pub(super) fn record_pointer(
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

pub(super) fn selected_record_columns(layout: &QueryRecordLayout) -> Result<Vec<QueryResultColumn>, String> {
	layout.selected_known_columns().ok_or_else(|| {
		String::from("Runtime-determined database query field lists are not yet supported.")
	})
}

fn primary_key_column_names(columns: &[QueryResultColumn]) -> Vec<String> {
	columns.iter().filter(|column| column.is_primary_key).map(|column| column.column_name.clone()).collect()
}

fn record_pointer_type(query: &SqlQuery) -> crate::ast::RecordPointerType {
	crate::ast::RecordPointerType {
		database_name: query.database_name.clone(),
		schema_name: query.schema_name.clone(),
		table_name: query.table_name.clone(),
	}
}
