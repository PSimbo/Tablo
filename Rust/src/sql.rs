use crate::ast::DataType;

pub(crate) fn quote_identifier(identifier: &str) -> String {
	format!("\"{}\"", identifier.replace('"', "\"\""))
}

pub(crate) fn quote_mysql_identifier(identifier: &str) -> String {
	format!("`{}`", identifier.replace('`', "``"))
}

pub(crate) fn postgresql_type_name(data_type: &DataType) -> Option<&'static str> {
	match data_type.without_nullability() {
		DataType::Bool => Some("BOOLEAN"),
		DataType::Date => Some("DATE"),
		DataType::Dec => Some("NUMERIC"),
		DataType::Int => Some("BIGINT"),
		DataType::Text => Some("TEXT"),
		DataType::Time => Some("TIME"),
		DataType::TimeTz => Some("TIME WITH TIME ZONE"),
		DataType::Timestamp => Some("TIMESTAMP"),
		DataType::TimestampTz => Some("TIMESTAMP WITH TIME ZONE"),
		_ => None,
	}
}
