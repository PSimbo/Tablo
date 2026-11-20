use std::collections::BTreeMap;
use std::fmt;

use crate::value::Value;

use num_bigint::BigInt;
use crate::numeric::ExactNumber as JsonNumber;
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
			Value::Boolean(value) => Self(JsonNode::Boolean(value)),
			Value::Decimal(value) => Self(JsonNode::Number(JsonNumber::new(&value.to_string()))),
			Value::Enum(value) => Self(JsonNode::String(value.variant_name)),
			Value::Float(value) => {
				if !value.0.is_finite() {
					return Err(String::from("Non-finite float cannot be represented as a JSON number."));
				}

				Self(JsonNode::Number(JsonNumber::new(&value.to_string())))
			}
			Value::Integer(value) => Self(JsonNode::Number(JsonNumber::new(&value.to_string()))),
			Value::Json(value) => value,
			Value::Null => Self(JsonNode::Null),
			Value::Text(value) => Self(JsonNode::String(value)),
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

	pub(crate) fn number_literal(text: &str) -> Result<Self, String> {
		let raw: Box<RawValue> = serde_json::from_str(text).map_err(|error| error.to_string())?;
		let node = JsonNode::parse(&raw, 0)?;
		if !matches!(node, JsonNode::Number(_)) { return Err(String::from("JSON numeric literal requires a number.")); }
		Ok(Self(node))
	}

	pub(crate) fn array_literal(values: Vec<Value>) -> Result<Self, String> {
		values.into_iter().map(Self::from_value).map(|value| value.map(|value| value.0))
			.collect::<Result<Vec<_>, _>>().map(|values| Self(JsonNode::Array(values)))
	}

	pub(crate) fn object_literal(names: &[String], values: Vec<Value>) -> Result<Self, String> {
		let mut fields = BTreeMap::new();
		for (name, value) in names.iter().zip(values) {
			if fields.insert(name.clone(), Self::from_value(value)?.0).is_some() {
				return Err(format!("JSON property `{name}` is specified more than once."));
			}
		}
		Ok(Self(JsonNode::Object(fields)))
	}

	pub(crate) fn to_integer(&self) -> Result<i64, String> {
		let JsonNode::Number(number) = &self.0 else {
			return Err(String::from("Explicit `int` conversion requires a JSON number."));
		};
		let whole_digits = BigInt::from(number.digits.len()) + &number.exponent;

		if number.digits.is_empty() || whole_digits <= BigInt::from(0) {
			return Ok(0);
		}

		let overflow = || String::from("JSON number exceeds the supported integer range after truncation.");

		if whole_digits > BigInt::from(19) {
			return Err(overflow());
		}

		let length = usize::try_from(whole_digits).map_err(|_| overflow())?;
		let mut text = number.digits[..length.min(number.digits.len())].to_string();

		text.extend(std::iter::repeat_n('0', length.saturating_sub(number.digits.len())));

		if number.negative {
			text.insert(0, '-');
		}

		text.parse().map_err(|_| overflow())
	}

	pub(crate) fn to_float(&self) -> Result<crate::float::Float, String> {
		let JsonNode::Number(number) = &self.0 else {
			return Err(String::from("Float conversion requires a JSON number."));
		};

		crate::float::Float::parse(&number.text)
	}

	pub(crate) fn to_decimal(&self) -> Result<crate::value::Decimal, String> {
		let JsonNode::Number(number) = &self.0 else {
			return Err(String::from("Explicit `dec` conversion requires a JSON number."));
		};

		number.to_decimal()
	}

	pub(crate) fn is_document(&self) -> bool { matches!(self.0, JsonNode::Object(_) | JsonNode::Array(_)) }

	pub(crate) fn is_null(&self) -> bool { matches!(self.0, JsonNode::Null) }

	pub(crate) fn convert(&self, target: &crate::ast::DataType) -> Result<crate::value::Value, String> {
		use crate::ast::DataType;
		use crate::value::{Decimal, Value};
		if self.is_null() && target.is_nullable() { return Ok(Value::Null); }
		let incompatible = || format!("JSON content cannot be converted to `{}`.", target.name());
		match (target.without_nullability(), &self.0) {
			(DataType::Any, _) if !self.is_null() => self.native_contents().map_err(|error| error.to_string()),
			(DataType::Json, JsonNode::Object(_) | JsonNode::Array(_)) => Ok(Value::Json(self.clone())),
			(DataType::Text, JsonNode::String(value)) => Ok(Value::Text(value.clone())),
			(DataType::Bool, JsonNode::Boolean(value)) => Ok(Value::Boolean(*value)),
			(DataType::Float, JsonNode::Number(_)) => self.to_float().map(Value::Float),
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

	// Materialize JSON contents for an `any` target, preserving exact numbers.
	// Array elements are nullable; the caller checks nullability of the root.
	fn native_contents(&self) -> Result<Value, crate::object_construction::ObjectConstructionError> {
		match &self.0 {
			JsonNode::Null => Ok(Value::Null),
			JsonNode::String(value) => Ok(Value::Text(value.clone())),
			JsonNode::Boolean(value) => Ok(Value::Boolean(*value)),
			JsonNode::Number(_) => self.convert(&crate::ast::DataType::Dec).map_err(JsonConversion::error),
			JsonNode::Array(values) => values.iter().enumerate().map(|(index, value)| {
				Self(value.clone()).native_contents().map_err(|error| error.at(format!("[{}]", index + 1)))
			}).collect::<Result<Vec<_>, _>>().map(Value::Array),
			JsonNode::Object(_) => Err(JsonConversion::error("JSON objects cannot be converted to `any`; use an explicit object type or retain a whole JSON document.")),
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

// Conversion is deliberately separate from ordinary JSON field access: it
// validates a complete, explicitly selected native shape before publishing it.
pub(crate) struct JsonConversion<'a> {
	descriptors: &'a [crate::bytecode::ObjectTypeDescriptor],
	private_types: &'a [crate::bytecode::ObjectTypeId],
}

impl<'a> JsonConversion<'a> {
	pub(crate) fn new(descriptors: &'a [crate::bytecode::ObjectTypeDescriptor], private_types: &'a [crate::bytecode::ObjectTypeId]) -> Self {
		Self { descriptors, private_types }
	}

	fn error(message: impl Into<String>) -> crate::object_construction::ObjectConstructionError {
		crate::object_construction::ObjectConstructionError::new(message)
	}

	fn descriptor(&self, id: crate::bytecode::ObjectTypeId) -> Result<&'a crate::bytecode::ObjectTypeDescriptor, crate::object_construction::ObjectConstructionError> {
		self.descriptors.iter().find(|descriptor| descriptor.id() == id)
			.ok_or_else(|| Self::error(format!("Unknown object type ID {}.", id.raw())))
	}

	pub(crate) fn export_document(&self, value: Value) -> Result<JsonValue, crate::object_construction::ObjectConstructionError> {
		// The existing text overload remains a parser, not a scalar encoder.
		if let Value::Text(text) = value { return JsonValue::parse(&text).map_err(Self::error); }
		let result = self.export(value)?;
		if !result.is_document() { return Err(Self::error("JSON conversion requires an object or array root.")); }
		Ok(result)
	}

	fn export(&self, value: Value) -> Result<JsonValue, crate::object_construction::ObjectConstructionError> {
		use crate::bytecode::ObjectTypeDescriptorShape;
		use crate::ast::Visibility;
		match value {
			Value::Array(values) | Value::DynamicArray { values, .. } => Ok(JsonValue(JsonNode::Array(values.into_iter().enumerate()
				.map(|(index, value)| self.export(value).map(|value| value.0).map_err(|error| error.at(format!("[{}]", index + 1))))
				.collect::<Result<Vec<_>, _>>()?))),
			Value::Object(object) => {
				let ObjectTypeDescriptorShape::Fields(fields) = self.descriptor(object.object_type_id)?.shape() else {
					return Err(Self::error("Expected a field-shaped object."));
				};
				let mut result = BTreeMap::new();
				for field in fields.iter().filter(|field| field.visibility() == Visibility::Public) {
					let value = object.fields.get(field.name()).ok_or_else(|| Self::error("Incomplete native object.").at(field.name()))?;
					result.insert(field.name().to_owned(), self.export(value.clone()).map_err(|error| error.at(field.name()))?.0);
				}
				Ok(JsonValue(JsonNode::Object(result)))
			}
			Value::Enum(value) => Ok(JsonValue(JsonNode::String(value.variant_name))),
			Value::Date(value) => Ok(JsonValue(JsonNode::String(value.to_string()))),
			Value::Time(value) => Ok(JsonValue(JsonNode::String(value.to_string()))),
			Value::TimeTz(value) => Ok(JsonValue(JsonNode::String(value.to_string()))),
			Value::Timestamp(value) => Ok(JsonValue(JsonNode::String(value.to_string()))),
			Value::TimestampTz(value) => Ok(JsonValue(JsonNode::String(value.to_string()))),
			value => JsonValue::from_value(value).map_err(Self::error),
		}
	}

	pub(crate) fn import_object(&self, value: &JsonValue, id: crate::bytecode::ObjectTypeId) -> Result<Value, crate::object_construction::ObjectConstructionError> {
		use crate::bytecode::{ObjectTypeDescriptorShape, ObjectValueTypeDescriptor};
		use crate::ast::Visibility;
		let descriptor = self.descriptor(id)?;
		let fields = match descriptor.shape() {
			ObjectTypeDescriptorShape::Fields(fields) => fields,
			ObjectTypeDescriptorShape::RootArray(element) => return self.import(value, &ObjectValueTypeDescriptor::Array(Box::new(element.clone()))),
		};
		let JsonNode::Object(properties) = &value.0 else { return Err(Self::error("Expected a JSON object.")); };
		let mut used = std::collections::BTreeSet::new();
		let mut supplied = Vec::new();
		for field in fields {
			let matches = properties.iter().filter(|(name, _)| {
				if field.is_quoted() { *name == field.name() } else { name.eq_ignore_ascii_case(field.name()) }
			}).collect::<Vec<_>>();
			if matches.len() > 1 { return Err(Self::error("Multiple JSON properties match this field.").at(field.name())); }
			let accessible = field.visibility() == Visibility::Public || self.private_types.contains(&id);
			if let Some((name, value)) = matches.first() {
				if !accessible { return Err(Self::error("JSON property targets an inaccessible private field.").at(*name)); }
				used.insert((*name).clone());
				supplied.push((field.name().to_owned(), self.import(&JsonValue((*value).clone()), field.data_type())
					.map_err(|error| error.at(*name))?));
			} else if accessible && field.explicit_default().is_none() {
				return Err(Self::error("Missing JSON property; an explicit field default is required.").at(field.name()));
			}
		}
		if let Some(name) = properties.keys().find(|name| !used.contains(*name)) {
			return Err(Self::error("Unexpected JSON property.").at(name));
		}
		crate::object_construction::ObjectConstructor::new(self.descriptors.iter()).construct(id, supplied)
	}

	fn import(&self, value: &JsonValue, target: &crate::bytecode::ObjectValueTypeDescriptor) -> Result<Value, crate::object_construction::ObjectConstructionError> {
		use crate::bytecode::{ObjectValueTypeDescriptor as T, Constant};
		use crate::ast::DataType;
		use crate::value::{Date, Time, TimeTz, Timestamp, TimestampTz, EnumValue};

		if let T::Nullable(inner) = target {
			return if value.is_null() { Ok(Value::Null) } else { self.import(value, inner) };
		}

		if let T::Union(members) = target {
			let mut matches = members.iter().filter_map(|member| {
				self.import(value, member).ok().map(|value| value.preserve_dynamic_type(member))
			});

			let first = matches.next().ok_or_else(|| Self::error("JSON value matches no union member."))?;

			return if matches.next().is_some() { Err(Self::error("JSON value matches multiple union members.")) } else { Ok(first) };
		}

		if value.is_null() {
			return Err(Self::error("JSON null requires a nullable target."));
		}

		let primitive = match target {
			T::Any => return value.native_contents(),
			T::Array(element) => {
				let JsonNode::Array(values) = &value.0 else { return Err(Self::error("Expected a JSON array.")); };
				return Ok(Value::Array(values.iter().enumerate().map(|(index, value)| self.import(&JsonValue(value.clone()), element)
					.map_err(|error| error.at(format!("[{}]", index + 1)))).collect::<Result<Vec<_>, _>>()?));
			}
			T::Object(id) => return self.import_object(value, *id),
			T::Enum { name, variants, .. } => {
				let JsonNode::String(variant_name) = &value.0 else {
					return Err(Self::error("Expected an enum variant name as a JSON string."));
				};

				let (variant, backing) = variants.iter().find(|(name, _)| name == variant_name)
					.ok_or_else(|| Self::error(format!("Unknown enum variant `{variant_name}` for `{name}`.")))?;

				let backing = match backing {
					Constant::Boolean(value) => Value::Boolean(*value),
					Constant::Date(value) => Value::Date(*value),
					Constant::Decimal(value) => Value::Decimal(value.clone()),
					Constant::Integer(value) => Value::Integer(*value),
					Constant::Text(value) => Value::Text(value.clone()),
				};

				return Ok(Value::Enum(EnumValue { enum_name: name.clone(), variant_name: variant.clone(), backing_value: Box::new(backing) }));
			}
			T::Date | T::Time | T::TimeTz | T::Timestamp | T::TimestampTz => {
				let JsonNode::String(text) = &value.0 else { return Err(Self::error("Expected an ISO date/time string.")); };
				return match target {
					T::Date => Date::from_iso_text(text).map(Value::Date),
					T::Time => Time::from_iso_text(text).map(Value::Time),
					T::TimeTz => TimeTz::from_iso_text(text).map(Value::TimeTz),
					T::Timestamp => Timestamp::from_iso_text(text).map(Value::Timestamp),
					T::TimestampTz => TimestampTz::from_iso_text(text).map(Value::TimestampTz),
					_ => unreachable!(),
				}.map_err(Self::error);
			}
			T::Bool => DataType::Bool,
			T::Dec => DataType::Dec,
			T::Float => DataType::Float,
			T::Int => DataType::Int,
			T::Json => DataType::Json,
			T::Text => DataType::Text,
			T::Range(_) => return Err(Self::error("JSON range conversion is unsupported.")),
			T::Nullable(_) | T::Union(_) => unreachable!(),
		};
		value.convert(&primitive).map_err(Self::error)
	}
}

#[cfg(test)]
mod conversion_tests {
	use super::*;
	use crate::ast::Visibility;
	use crate::bytecode::*;

	#[test]
	fn float_json_round_trips_and_non_finite_export_errors() {
		use crate::float::{Float, NativeFloat};

		let conversion = JsonConversion::new(&[], &[]);

		for number in [0.0, -0.0, 0.1, NativeFloat::MAX, NativeFloat::MIN_POSITIVE, NativeFloat::from_bits(1)] {
			let original = Float(number);
			let json = JsonValue::from_value(Value::Float(original)).unwrap();
			let reparsed = JsonValue::number_literal(&json.to_text()).unwrap();

			assert_eq!(reparsed.to_float().unwrap(), original);
			assert_eq!(conversion.import(&reparsed, &ObjectValueTypeDescriptor::Float).unwrap(), Value::Float(original));
		}

		for number in [NativeFloat::NAN, NativeFloat::INFINITY, NativeFloat::NEG_INFINITY] {
			assert!(JsonValue::from_value(Value::Float(Float(number))).is_err());
		}

		assert!(conversion.import(&JsonValue::number_literal("1.25").unwrap(),
			&ObjectValueTypeDescriptor::Union(vec![ObjectValueTypeDescriptor::Dec, ObjectValueTypeDescriptor::Float])).is_err());
	}

	#[test]
	fn any_contents_materialize_native_values_in_runtime_only_builds() {
		use crate::ast::DataType;
		use crate::value::Decimal;

		let conversion = JsonConversion::new(&[], &[]);

		for spelling in ["3", "3.0", "3e0", "3.25", "-0.125"] {
			let value = JsonValue::number_literal(spelling).unwrap();
			let expected = Value::Decimal(Decimal::from_database_text(spelling).unwrap());
			assert_eq!(value.convert(&DataType::Any).unwrap(), expected);
			assert_eq!(conversion.import(&value, &ObjectValueTypeDescriptor::Any).unwrap(), expected);
		}

		let source = JsonValue::parse(r#"[3,true,null,["2026-11-13"]]"#).unwrap();
		let original = source.clone();

		assert_eq!(source.convert(&DataType::Any).unwrap(), Value::Array(vec![
			Value::Decimal(Decimal::from_database_text("3").unwrap()), Value::Boolean(true), Value::Null,
			Value::Array(vec![Value::Text("2026-11-13".into())]),
		]));

		assert_eq!(source, original);

		for source in ["{}", "[{}]", "[999999999999999999999999999999999999999999]", "[0.123456789012345678901234567890123456789]"] {
			let value = JsonValue::parse(source).unwrap();
			let original = value.clone();
			assert!(value.convert(&DataType::Any).is_err(), "{source}");
			assert!(conversion.import(&value, &ObjectValueTypeDescriptor::Any).is_err(), "{source}");
			assert_eq!(value, original);
		}
	}

	#[test]
	fn runtime_only_conversion_is_atomic_and_obeys_private_access() {
		let id = ObjectTypeId::from_raw(0);
		let descriptors = vec![ObjectTypeDescriptor {
			id, display_name: "Model".into(), shape: ObjectTypeDescriptorShape::Fields(vec![
				ObjectFieldDescriptor {
					name: "amount".into(), data_type: ObjectValueTypeDescriptor::Int,
					explicit_default: None, visibility: Visibility::Public, is_quoted: false,
				},
				ObjectFieldDescriptor {
					name: "hidden".into(), data_type: ObjectValueTypeDescriptor::Text,
					explicit_default: Some(ObjectDefaultValue::Text("default".into())), visibility: Visibility::Private, is_quoted: false,
				},
			]),
		}];
		let public = JsonConversion::new(&descriptors, &[]);
		let source = JsonValue::parse(r#"{"amount":3.5}"#).unwrap();
		let original = source.clone();
		assert!(public.import_object(&source, id).is_err());
		assert_eq!(source, original);
		let source = JsonValue::parse(r#"{"amount":3,"hidden":"secret"}"#).unwrap();
		assert!(public.import_object(&source, id).is_err());
		let private_types = [id];
		let internal = JsonConversion::new(&descriptors, &private_types);
		let object = internal.import_object(&source, id).unwrap();
		assert_eq!(internal.export_document(object).unwrap(), JsonValue::parse(r#"{"amount":3}"#).unwrap());

		let program = Program::new(vec![
			Instruction::PushText(r#"{"amount":3,"hidden":"secret"}"#.into()),
			Instruction::CallBuiltIn(crate::builtins::BuiltInFunction::JsonCast, 1),
			Instruction::ConvertObject { object_type_id: id, private_types: vec![id] },
			Instruction::CallBuiltIn(crate::builtins::BuiltInFunction::JsonCast, 1),
		]).with_object_type_descriptors(descriptors);
		let program = crate::object_file::read_program(&crate::object_file::write_program(&program)).unwrap();
		assert_eq!(crate::vm::VirtualMachine::new().run(&program).unwrap(), Some(Value::Json(JsonValue::parse(r#"{"amount":3}"#).unwrap())));
	}
}
