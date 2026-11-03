use std::collections::BTreeMap;
use std::fmt;

use num_bigint::BigInt;
use serde::de::{Deserialize, Deserializer, MapAccess, Visitor};
use serde_json::value::RawValue;

/// Parsing produces documents with object or array roots. Field and element
/// reads retain JSON-domain contents until a typed destination requests
/// conversion.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JsonValue(JsonNode);

#[derive(Clone, Debug, Eq, PartialEq)]
enum JsonNode {
	Array(Vec<JsonNode>),
	Boolean(bool),
	Null,
	Number(JsonNumber),
	Object(BTreeMap<String, JsonNode>),
	String(String),
}

#[derive(Clone, Debug)]
struct JsonNumber {
	text: String,
	digits: String,
	exponent: BigInt,
	negative: bool,
}

impl JsonNumber {
	fn new(text: &str) -> Self {
		let (mantissa, exponent) = text.split_once(['e', 'E']).unwrap_or((text, "0"));
		let mut exponent: BigInt = exponent.parse().expect("JSON number syntax was validated");
		let negative = mantissa.starts_with('-');
		let mantissa = mantissa.trim_start_matches('-');
		let (whole, fraction) = mantissa.split_once('.').unwrap_or((mantissa, ""));
		let digits = format!("{whole}{fraction}");
		let digits = digits.trim_start_matches('0');
		let significant = digits.trim_end_matches('0');
		exponent += BigInt::from(digits.len() - significant.len());
		exponent -= BigInt::from(fraction.len());
		Self {
			text: text.to_string(), digits: significant.to_string(),
			exponent: if significant.is_empty() { BigInt::from(0) } else { exponent },
			negative: negative && !significant.is_empty(),
		}
	}
}

impl PartialEq for JsonNumber {
	fn eq(&self, other: &Self) -> bool {
		self.negative == other.negative && self.digits == other.digits && self.exponent == other.exponent
	}
}

impl Eq for JsonNumber {}

// RawValue keeps number tokens out of serde_json's machine-number conversions.
// Decode object entries before collecting them so duplicate keys, including
// escaped equivalents, cannot disappear into a map unnoticed.
struct ObjectEntries(Vec<(String, Box<RawValue>)>);

impl<'de> Deserialize<'de> for ObjectEntries {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		struct ObjectVisitor;
		impl<'de> Visitor<'de> for ObjectVisitor {
			type Value = ObjectEntries;
			fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result { formatter.write_str("a JSON object") }
			fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<Self::Value, M::Error> {
				let mut entries = Vec::new();
				let mut names = std::collections::BTreeSet::new();
				while let Some((name, value)) = map.next_entry::<String, Box<RawValue>>()? {
					if !names.insert(name.clone()) {
						return Err(serde::de::Error::custom(format!("Duplicate JSON property name `{name}`.")));
					}
					entries.push((name, value));
				}
				Ok(ObjectEntries(entries))
			}
		}
		deserializer.deserialize_map(ObjectVisitor)
	}
}

pub(crate) enum JsonMutationKey {
	Field(String, bool),
	Index(i64),
}

impl JsonNode {
	fn parse(raw: &RawValue, depth: usize) -> Result<Self, String> {
		if depth > 128 { return Err(String::from("JSON nesting exceeds the supported depth of 128.")); }
		let text = raw.get();
		let invalid = |error: serde_json::Error| format!("Invalid JSON: {error}");
		match text.as_bytes()[0] {
			b'{' => {
				let entries: ObjectEntries = serde_json::from_str(text).map_err(invalid)?;
				let fields = entries.0.into_iter().map(|(name, value)| Ok((name, Self::parse(&value, depth + 1)?)))
					.collect::<Result<_, String>>()?;
				Ok(Self::Object(fields))
			}
			b'[' => {
				let values: Vec<Box<RawValue>> = serde_json::from_str(text).map_err(invalid)?;
				Ok(Self::Array(values.iter().map(|value| Self::parse(value, depth + 1)).collect::<Result<_, _>>()?))
			}
			b'"' => Ok(Self::String(serde_json::from_str(text).map_err(invalid)?)),
			b't' => Ok(Self::Boolean(true)),
			b'f' => Ok(Self::Boolean(false)),
			b'n' => Ok(Self::Null),
			_ => Ok(Self::Number(JsonNumber::new(text))),
		}
	}

	fn write(&self, output: &mut String) {
		match self {
			Self::Array(values) => {
				output.push('[');
				for (index, value) in values.iter().enumerate() {
					if index > 0 { output.push(','); }
					value.write(output);
				}
				output.push(']');
			}
			Self::Object(fields) => {
				output.push('{');
				for (index, (name, value)) in fields.iter().enumerate() {
					if index > 0 { output.push(','); }
					output.push_str(&serde_json::to_string(name).expect("String serialization cannot fail"));
					output.push(':');
					value.write(output);
				}
				output.push('}');
			}
			Self::String(value) => output.push_str(&serde_json::to_string(value).expect("String serialization cannot fail")),
			Self::Number(value) => output.push_str(&value.text),
			Self::Boolean(value) => output.push_str(if *value { "true" } else { "false" }),
			Self::Null => output.push_str("null"),
		}
	}
}

impl Default for JsonValue {
	fn default() -> Self { Self(JsonNode::Object(BTreeMap::new())) }
}

impl JsonValue {
	pub fn parse(text: &str) -> Result<Self, String> {
		let raw: Box<RawValue> = serde_json::from_str(text).map_err(|error| format!("Invalid JSON: {error}"))?;
		if !matches!(raw.get().as_bytes()[0], b'{' | b'[') {
			return Err(String::from("A JSON document must have an object or array root."));
		}
		Ok(Self(JsonNode::parse(&raw, 0)?))
	}

	pub(crate) fn field(&self, name: &str, quoted: bool) -> Result<Option<Self>, String> {
		let JsonNode::Object(fields) = &self.0 else { return Ok(None); };
		if quoted { return Ok(fields.get(name).cloned().map(Self)); }
		let mut matches = fields.iter().filter(|(key, _)| key.eq_ignore_ascii_case(name));
		let value = matches.next().map(|(_, value)| value.clone());
		if matches.next().is_some() {
			return Err(format!("JSON field `{name}` is ambiguous; use a quoted field name."));
		}
		Ok(value.map(Self))
	}

	pub(crate) fn index(&self, index: i64) -> Result<Self, String> {
		let JsonNode::Array(values) = &self.0 else {
			return Err(String::from("JSON indexing requires an array."));
		};
		let offset = index.checked_sub(1).and_then(|offset| usize::try_from(offset).ok());
		offset.and_then(|offset| values.get(offset)).cloned().map(Self)
			.ok_or_else(|| format!("JSON array index {index} is out of bounds for length {}.", values.len()))
	}

	pub(crate) fn from_value(value: crate::value::Value) -> Result<Self, String> {
		use crate::value::Value;
		Ok(match value {
			Value::Json(value) => value,
			Value::Null => Self(JsonNode::Null),
			Value::Text(value) => Self(JsonNode::String(value)),
			Value::Boolean(value) => Self(JsonNode::Boolean(value)),
			Value::Integer(value) => Self(JsonNode::Number(JsonNumber::new(&value.to_string()))),
			Value::Decimal(value) => Self(JsonNode::Number(JsonNumber::new(&value.to_string()))),
			_ => return Err(String::from("Value cannot be stored in JSON without explicit conversion.")),
		})
	}

	pub(crate) fn assign(mut self, path: &[JsonMutationKey], value: &Self) -> Result<Self, String> {
		fn update(node: &mut JsonNode, path: &[JsonMutationKey], value: &JsonNode) -> Result<(), String> {
			let Some((key, remaining)) = path.split_first() else { return Err(String::from("JSON mutation requires a field or index.")); };
			let child = match (node, key) {
				(JsonNode::Object(fields), JsonMutationKey::Field(name, quoted)) => {
					let name = if *quoted { name.clone() } else {
						let mut matches = fields.keys().filter(|key| key.eq_ignore_ascii_case(name));
						let matched = matches.next().cloned();
						if matches.next().is_some() { return Err(format!("JSON field `{name}` is ambiguous; use a quoted field name.")); }
						matched.unwrap_or_else(|| name.clone())
					};
					if remaining.is_empty() { fields.insert(name, value.clone()); return Ok(()); }
					fields.get_mut(&name).ok_or_else(|| format!("Intermediate JSON field `{name}` is missing."))?
				}
				(JsonNode::Array(values), JsonMutationKey::Index(index)) => {
					let offset = index.checked_sub(1).and_then(|index| usize::try_from(index).ok())
						.filter(|index| *index < values.len() || (remaining.is_empty() && *index == values.len()))
						.ok_or_else(|| format!("JSON array index {index} is out of bounds for length {}.", values.len()))?;
					if remaining.is_empty() {
						if offset == values.len() { values.push(value.clone()); } else { values[offset] = value.clone(); }
						return Ok(());
					}
					&mut values[offset]
				}
				(_, JsonMutationKey::Field(_, _)) => return Err(String::from("JSON field mutation requires an object parent.")),
				(_, JsonMutationKey::Index(_)) => return Err(String::from("JSON indexed mutation requires an array parent.")),
			};
			update(child, remaining, value)
		}
		update(&mut self.0, path, &value.0)?;
		Ok(self)
	}

	pub(crate) fn is_null(&self) -> bool { matches!(self.0, JsonNode::Null) }

	pub(crate) fn convert(&self, target: &crate::ast::DataType) -> Result<crate::value::Value, String> {
		use crate::ast::DataType;
		use crate::value::{Decimal, Value};
		if self.is_null() && target.is_nullable() { return Ok(Value::Null); }
		let incompatible = || format!("JSON content cannot be converted to `{}`.", target.name());
		match (target.without_nullability(), &self.0) {
			(DataType::Any, _) => Ok(Value::Json(self.clone())),
			(DataType::Json, JsonNode::Object(_) | JsonNode::Array(_)) => Ok(Value::Json(self.clone())),
			(DataType::Text, JsonNode::String(value)) => Ok(Value::Text(value.clone())),
			(DataType::Bool, JsonNode::Boolean(value)) => Ok(Value::Boolean(*value)),
			(DataType::Int, JsonNode::Number(value)) => {
				if value.digits.is_empty() { return Ok(Value::Integer(0)); }
				let exponent = i32::try_from(&value.exponent).map_err(|_| incompatible())?;
				if exponent < 0 || value.digits.len().saturating_add(exponent as usize) > 19 { return Err(incompatible()); }
				let text = format!("{}{}{}", if value.negative { "-" } else { "" }, value.digits, "0".repeat(exponent as usize));
				text.parse().map(Value::Integer).map_err(|_| incompatible())
			}
			(DataType::Dec, JsonNode::Number(value)) => {
				if value.digits.is_empty() { return Ok(Value::Decimal(Decimal::from_database_text("0").unwrap())); }
				// Bound the normalized exponent before passing it to fixed-precision decimal parsing.
				let exponent = i32::try_from(&value.exponent).map_err(|_| incompatible())?;
				if !(-(Decimal::MAX_PRECISION as i32)..=Decimal::MAX_PRECISION as i32).contains(&exponent) || value.digits.len() > Decimal::MAX_PRECISION as usize { return Err(incompatible()); }
				let text = format!("{}{}e{}", if value.negative { "-" } else { "" }, value.digits, exponent);
				Decimal::from_database_text(&text).map(Value::Decimal).map_err(|_| incompatible())
			}
			_ => Err(incompatible()),
		}
	}

	pub fn to_text(&self) -> String {
		let mut text = String::new();
		self.0.write(&mut text);
		text
	}

	pub fn debug_children(&self) -> Vec<(String, String, Option<Self>)> {
		let child = |name: String, value: &JsonNode| {
			let mut text = String::new();
			value.write(&mut text);
			let nested = matches!(value, JsonNode::Array(_) | JsonNode::Object(_)).then(|| Self(value.clone()));
			(name, text, nested)
		};
		match &self.0 {
			JsonNode::Object(fields) => fields.iter().map(|(name, value)| child(name.clone(), value)).collect(),
			JsonNode::Array(values) => values.iter().enumerate().map(|(index, value)| child((index + 1).to_string(), value)).collect(),
			_ => Vec::new(),
		}
	}
}

impl fmt::Display for JsonValue {
	fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result { formatter.write_str(&self.to_text()) }
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn validates_unicode_and_preserves_arbitrary_property_names() {
		let value = JsonValue::parse(r#"{"":null,"x":"\uD83D\uDE00\n","a":1,"A":2,"$serde_json::private::Number":"123"}"#).unwrap();
		assert_eq!(value, JsonValue::parse(&value.to_text()).unwrap());
		assert!(value.to_text().contains("😀\\n"));
		for text in [r#"{"a":1,"\u0061":2}"#, r#"["\uD800"]"#, r#"["\uDC00"]"#, "[\"\n\"]"] {
			assert!(JsonValue::parse(text).is_err(), "{text}");
		}
	}

	#[test]
	fn normalizes_numbers_without_expanding_exponents_or_rounding() {
		for (left, right) in [("1", "1.00"), ("-0", "0e999999999999999999999"), ("0.00120", "12e-4"),
			("1200", "1.2e+3"), ("1e-999999999999999999999", "10e-1000000000000000000000")] {
			assert_eq!(JsonValue::parse(&format!("[{left}]")), JsonValue::parse(&format!("[{right}]")));
		}
		assert_ne!(JsonValue::parse("[1.0000000000000000000000000000000000000001]").unwrap(), JsonValue::parse("[1]").unwrap());
	}

	#[test]
	fn rejects_excessive_nesting_and_trailing_content() {
		let nested = format!("{}0{}", "[".repeat(130), "]".repeat(130));
		assert!(JsonValue::parse(&nested).is_err());
		assert!(JsonValue::parse("{} []").is_err());
		assert_eq!(JsonValue::parse(" \n {} \t").unwrap(), JsonValue::default());
	}
}
