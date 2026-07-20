pub(crate) fn quote_identifier(identifier: &str) -> String {
	format!("\"{}\"", identifier.replace('"', "\"\""))
}

pub(crate) fn table_source(schema_name: &str, table_name: &str, schema_is_implicit: bool) -> String {
	if schema_is_implicit {
		quote_identifier(table_name)
	}
	else {
		format!("{}.{}", quote_identifier(schema_name), quote_identifier(table_name))
	}
}

