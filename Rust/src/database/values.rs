use crate::value::Value;

pub(super) fn runtime_type_name(value: &Value) -> &'static str {
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
