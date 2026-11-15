use crate::value::Value;

pub(super) fn runtime_type_name(value: &Value) -> &'static str {
	match value {
		Value::Array(_) => "array",
		Value::Boolean(_) => "bool",
		Value::Date(_) => "date",
		Value::Decimal(_) => "dec",
		Value::DecimalRange(_) | Value::IntegerRange(_) => "range",
		Value::Enum(_) => "enum",
		Value::Float(_) => "float",
		Value::Integer(_) => "int",
		Value::Iterator(_) => "iterator",
		Value::Json(_) => "json",
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

#[cfg(any(feature = "driver-postgresql", test))]
pub(super) fn float_parameter_text(value: crate::float::Float) -> String {
	// A binary32 shortest spelling need not represent the same value when a
	// database parses it as binary64. Widen exactly before formatting.
	(value.0 as f64).to_string()
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::float::{Float, NativeFloat};

	#[test]
	fn float_parameter_text_preserves_value_at_database_precision() {
		for value in [0.1, -0.0, NativeFloat::from_bits(1), NativeFloat::MAX, NativeFloat::INFINITY, NativeFloat::NEG_INFINITY] {
			let text = float_parameter_text(Float(value));
			let parsed = text.parse::<f64>().unwrap();

			assert_eq!(parsed.to_bits(), (value as f64).to_bits());
		}

		assert_eq!(float_parameter_text(Float(NativeFloat::NAN)), "NaN");
	}
}
