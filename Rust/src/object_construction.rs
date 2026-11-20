use std::collections::BTreeMap;
use std::fmt;

use crate::bytecode::*;
use crate::value::*;

/// Construction consumes already typed, resolved field values. Name resolution,
/// visibility checks, and JSON conversion policy belong to the caller.
pub struct ObjectConstructor<'a> {
	descriptors: BTreeMap<ObjectTypeId, &'a ObjectTypeDescriptor>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ObjectConstructionError {
	pub path: Vec<String>,
	pub message: String,
}

impl ObjectConstructionError {
	pub(crate) fn new(message: impl Into<String>) -> Self {
		Self { path: Vec::new(), message: message.into() }
	}

	pub(crate) fn at(mut self, component: impl Into<String>) -> Self {
		self.path.insert(0, component.into());
		self
	}
}

impl fmt::Display for ObjectConstructionError {
	fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
		if !self.path.is_empty() { write!(formatter, "{}: ", self.path.join("."))?; }
		formatter.write_str(&self.message)
	}
}

impl std::error::Error for ObjectConstructionError {}

type Result<T> = std::result::Result<T, ObjectConstructionError>;

impl<'a> ObjectConstructor<'a> {
	pub fn new(descriptors: impl IntoIterator<Item = &'a ObjectTypeDescriptor>) -> Self {
		Self { descriptors: descriptors.into_iter().map(|descriptor| (descriptor.id(), descriptor)).collect() }
	}

	fn descriptor(&self, id: ObjectTypeId) -> Result<&'a ObjectTypeDescriptor> {
		self.descriptors.get(&id).copied()
			.ok_or_else(|| ObjectConstructionError::new(format!("Unknown object type ID {}.", id.raw())))
	}

	fn fields(&self, id: ObjectTypeId) -> Result<&'a [ObjectFieldDescriptor]> {
		match self.descriptor(id)?.shape() {
			ObjectTypeDescriptorShape::Fields(fields) => Ok(fields),
			ObjectTypeDescriptorShape::RootArray(_) => Err(ObjectConstructionError::new("Root-array objects do not accept named fields.")),
		}
	}

	pub fn construct(&self, id: ObjectTypeId, supplied: Vec<(String, Value)>) -> Result<Value> {
		let fields = self.fields(id)?;
		let mut supplied = self.collect_fields(id, supplied)?;
		let mut values = BTreeMap::new();
		for field in fields {
			let value = match supplied.remove(field.name()) {
				Some(value) => value,
				None => self.materialize(&self.field_default(id, field.name())?)
					.map_err(|error| error.at(field.name()))?,
			};
			self.validate(field.data_type(), &value).map_err(|error| error.at(field.name()))?;
			values.insert(field.name().to_owned(), value);
		}
		Ok(Value::Object(ObjectValue::new(id, values)))
	}

	fn collect_fields<T>(&self, id: ObjectTypeId, supplied: Vec<(String, T)>) -> Result<BTreeMap<String, T>> {
		let fields = self.fields(id)?;
		let mut values = BTreeMap::new();
		for (name, value) in supplied {
			if !fields.iter().any(|field| field.name() == name) {
				return Err(ObjectConstructionError::new("Unknown object field.").at(name));
			}
			if values.insert(name.clone(), value).is_some() {
				return Err(ObjectConstructionError::new("Object field was supplied more than once.").at(name));
			}
		}
		Ok(values)
	}

	pub fn object_default(&self, id: ObjectTypeId) -> Result<ObjectDefaultValue> {
		self.complete_object(id, &[], &mut Vec::new())
	}

	pub fn field_default(&self, id: ObjectTypeId, name: &str) -> Result<ObjectDefaultValue> {
		self.complete_field(id, name, &mut Vec::new())
	}

	fn complete_field(&self, id: ObjectTypeId, name: &str, active: &mut Vec<(ObjectTypeId, String)>) -> Result<ObjectDefaultValue> {
		let key = (id, name.to_owned());
		if active.contains(&key) {
			return Err(ObjectConstructionError::new("Object defaults require infinite recursive construction.").at(name));
		}
		let field = self.fields(id)?.iter().find(|field| field.name() == name)
			.ok_or_else(|| ObjectConstructionError::new("Unknown object field.").at(name))?;
		active.push(key);
		let value = match field.explicit_default() {
			Some(value) => self.complete_value(value, active),
			None => self.type_default(field.data_type(), active),
		};
		active.pop();
		value.map_err(|error| error.at(name))
	}

	fn complete_object(&self, id: ObjectTypeId, supplied: &[(String, ObjectDefaultValue)], active: &mut Vec<(ObjectTypeId, String)>) -> Result<ObjectDefaultValue> {
		if matches!(self.descriptor(id)?.shape(), ObjectTypeDescriptorShape::RootArray(_)) && supplied.is_empty() {
			return Ok(ObjectDefaultValue::Array(Vec::new()));
		}
		let mut supplied = self.collect_fields(id, supplied.to_vec())?;
		let fields = self.fields(id)?.iter().map(|field| {
			let value = match supplied.remove(field.name()) {
				Some(value) => self.complete_value(&value, active).map_err(|error| error.at(field.name()))?,
				None => self.complete_field(id, field.name(), active)?,
			};
			Ok((field.name().to_owned(), value))
		}).collect::<Result<Vec<_>>>()?;
		Ok(ObjectDefaultValue::Object { fields, object_type_id: id })
	}

	fn complete_value(&self, value: &ObjectDefaultValue, active: &mut Vec<(ObjectTypeId, String)>) -> Result<ObjectDefaultValue> {
		match value {
			ObjectDefaultValue::Array(values) => Ok(ObjectDefaultValue::Array(values.iter().enumerate()
				.map(|(index, value)| self.complete_value(value, active).map_err(|error| error.at(format!("[{}]", index + 1))))
				.collect::<Result<Vec<_>>>()?)),
			ObjectDefaultValue::Object { fields, object_type_id } => self.complete_object(*object_type_id, fields, active),
			other => Ok(other.clone()),
		}
	}

	fn type_default(&self, data_type: &ObjectValueTypeDescriptor, active: &mut Vec<(ObjectTypeId, String)>) -> Result<ObjectDefaultValue> {
		Ok(match data_type {
			ObjectValueTypeDescriptor::Nullable(_) => ObjectDefaultValue::Null,
			ObjectValueTypeDescriptor::Array(_) => ObjectDefaultValue::Array(Vec::new()),
			ObjectValueTypeDescriptor::Bool => ObjectDefaultValue::Boolean(false),
			ObjectValueTypeDescriptor::Date => ObjectDefaultValue::CurrentDate,
			ObjectValueTypeDescriptor::Dec => ObjectDefaultValue::Decimal(Decimal::from_integer(0)),
			ObjectValueTypeDescriptor::Enum { name, default_variant, default_value, .. } => ObjectDefaultValue::Enum {
				backing_value: default_value.clone(), enum_name: name.clone(), variant_name: default_variant.clone(),
			},
			ObjectValueTypeDescriptor::Float => ObjectDefaultValue::Float(crate::float::Float::default()),
			ObjectValueTypeDescriptor::Int => ObjectDefaultValue::Integer(0),
			ObjectValueTypeDescriptor::Json => ObjectDefaultValue::Json(crate::json::JsonValue::default()),
			ObjectValueTypeDescriptor::Object(id) => return self.complete_object(*id, &[], active),
			ObjectValueTypeDescriptor::Text => ObjectDefaultValue::Text(String::new()),
			ObjectValueTypeDescriptor::Time => ObjectDefaultValue::CurrentTime,
			ObjectValueTypeDescriptor::TimeTz => ObjectDefaultValue::CurrentTimeTz,
			ObjectValueTypeDescriptor::Timestamp => ObjectDefaultValue::CurrentTimestamp,
			ObjectValueTypeDescriptor::TimestampTz => ObjectDefaultValue::CurrentTimestampTz,
			ObjectValueTypeDescriptor::Any | ObjectValueTypeDescriptor::Range(_) | ObjectValueTypeDescriptor::Union(_) => {
				return Err(ObjectConstructionError::new("Field type has no supported implicit default."));
			}
		})
	}

	fn materialize(&self, default: &ObjectDefaultValue) -> Result<Value> {
		Ok(match default {
			ObjectDefaultValue::Array(values) => Value::Array(values.iter().enumerate()
				.map(|(index, value)| self.materialize(value).map_err(|error| error.at(format!("[{}]", index + 1))))
				.collect::<Result<Vec<_>>>()?),
			ObjectDefaultValue::Boolean(value) => Value::Boolean(*value),
			ObjectDefaultValue::CurrentDate => Value::Date(Date::current_local()),
			ObjectDefaultValue::CurrentTime => Value::Time(Time::current_local()),
			ObjectDefaultValue::CurrentTimeTz => Value::TimeTz(TimeTz::current_local()),
			ObjectDefaultValue::CurrentTimestamp => Value::Timestamp(Timestamp::current_local()),
			ObjectDefaultValue::CurrentTimestampTz => Value::TimestampTz(TimestampTz::current_local()),
			ObjectDefaultValue::Date(value) => Value::Date(*value),
			ObjectDefaultValue::Decimal(value) => Value::Decimal(value.clone()),
			ObjectDefaultValue::Enum { backing_value, enum_name, variant_name } => Value::Enum(EnumValue {
				backing_value: Box::new(match backing_value {
					Constant::Boolean(value) => Value::Boolean(*value),
					Constant::Date(value) => Value::Date(*value),
					Constant::Decimal(value) => Value::Decimal(value.clone()),
					Constant::Integer(value) => Value::Integer(*value),
					Constant::Text(value) => Value::Text(value.clone()),
				}),
				enum_name: enum_name.clone(), variant_name: variant_name.clone(),
			}),
			ObjectDefaultValue::Float(value) => Value::Float(*value),
			ObjectDefaultValue::FloatLiteral(value) => Value::Float(value.value),
			ObjectDefaultValue::Integer(value) => Value::Integer(*value),
			ObjectDefaultValue::Json(value) => Value::Json(value.clone()),
			ObjectDefaultValue::Null => Value::Null,
			ObjectDefaultValue::Object { fields, object_type_id } => return self.construct(*object_type_id,
				fields.iter().map(|(name, value)| self.materialize(value)
					.map(|value| (name.clone(), value)).map_err(|error| error.at(name))).collect::<Result<Vec<_>>>()?),
			ObjectDefaultValue::Text(value) => Value::Text(value.clone()),
			ObjectDefaultValue::Time(value) => Value::Time(*value),
			ObjectDefaultValue::TimeTz(value) => Value::TimeTz(*value),
			ObjectDefaultValue::Timestamp(value) => Value::Timestamp(*value),
			ObjectDefaultValue::TimestampTz(value) => Value::TimestampTz(*value),
		})
	}

	fn validate(&self, data_type: &ObjectValueTypeDescriptor, value: &Value) -> Result<()> {
		let matches = match (data_type, value) {
			(ObjectValueTypeDescriptor::Any, Value::Null) => false,
			(ObjectValueTypeDescriptor::Any, Value::Json(value)) => !value.is_null(),
			(ObjectValueTypeDescriptor::Any, _) => true,
			(ObjectValueTypeDescriptor::Nullable(_), Value::Null) => true,
			(ObjectValueTypeDescriptor::Nullable(inner), value) => return self.validate(inner, value),
			(ObjectValueTypeDescriptor::Array(inner), Value::Array(values) | Value::DynamicArray { values, .. }) => {
				for (index, value) in values.iter().enumerate() {
					self.validate(inner, value).map_err(|error| error.at(format!("[{}]", index + 1)))?;
				}
				true
			}
			(ObjectValueTypeDescriptor::Object(id), value) => match self.descriptor(*id)?.shape() {
				ObjectTypeDescriptorShape::RootArray(inner) => return self.validate(&ObjectValueTypeDescriptor::Array(Box::new(inner.clone())), value),
				ObjectTypeDescriptorShape::Fields(fields) => {
					if let Value::Object(object) = value {
						if object.object_type_id != *id { false }
						else {
							if fields.len() != object.fields.len() {
								return Err(ObjectConstructionError::new("Supplied object is not a complete instance."));
							}
							for field in fields {
								let value = object.fields.get(field.name()).ok_or_else(|| ObjectConstructionError::new("Supplied object is missing a field.").at(field.name()))?;
								self.validate(field.data_type(), value).map_err(|error| error.at(field.name()))?;
							}
							true
						}
					} else { false }
				}
			},
			(ObjectValueTypeDescriptor::Union(members), value) => members.iter().any(|member| self.validate(member, value).is_ok()),
			(ObjectValueTypeDescriptor::Json, Value::Json(value)) => value.is_document(),
			(ObjectValueTypeDescriptor::Enum { name, .. }, Value::Enum(value)) => name == &value.enum_name,
			(ObjectValueTypeDescriptor::Range(inner), Value::IntegerRange(_)) => matches!(inner.as_ref(), ObjectValueTypeDescriptor::Int | ObjectValueTypeDescriptor::Dec),
			(ObjectValueTypeDescriptor::Range(inner), Value::DecimalRange(_)) => matches!(inner.as_ref(), ObjectValueTypeDescriptor::Dec),
			(ObjectValueTypeDescriptor::Bool, Value::Boolean(_))
			| (ObjectValueTypeDescriptor::Date, Value::Date(_))
			| (ObjectValueTypeDescriptor::Dec, Value::Decimal(_) | Value::Integer(_))
			| (ObjectValueTypeDescriptor::Float, Value::Float(_))
			| (ObjectValueTypeDescriptor::Int, Value::Integer(_))
			| (ObjectValueTypeDescriptor::Text, Value::Text(_))
			| (ObjectValueTypeDescriptor::Time, Value::Time(_))
			| (ObjectValueTypeDescriptor::TimeTz, Value::TimeTz(_))
			| (ObjectValueTypeDescriptor::Timestamp, Value::Timestamp(_))
			| (ObjectValueTypeDescriptor::TimestampTz, Value::TimestampTz(_)) => true,
			_ => false,
		};
		if matches { Ok(()) } else { Err(ObjectConstructionError::new("Value does not match the declared field type.")) }
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::ast::Visibility;

	fn field(name: &str, data_type: ObjectValueTypeDescriptor, explicit_default: Option<ObjectDefaultValue>) -> ObjectFieldDescriptor {
		ObjectFieldDescriptor { name: name.into(), data_type, explicit_default, is_quoted: false, visibility: Visibility::Private }
	}

	fn descriptor(fields: Vec<ObjectFieldDescriptor>) -> ObjectTypeDescriptor {
		ObjectTypeDescriptor { id: ObjectTypeId::from_raw(0), display_name: "Model".into(), shape: ObjectTypeDescriptorShape::Fields(fields) }
	}

	#[test]
	fn constructs_defaults_without_the_compiler() {
		let descriptor = descriptor(vec![
			field("number", ObjectValueTypeDescriptor::Int, None),
			field("date", ObjectValueTypeDescriptor::Date, None),
			field("status", ObjectValueTypeDescriptor::Enum {
				name: "Status".into(), default_variant: "Ready".into(), default_value: Constant::Integer(7), variants: vec![("Ready".into(), Constant::Integer(7))],
			}, None),
			field("json", ObjectValueTypeDescriptor::Json, None),
		]);
		let constructor = ObjectConstructor::new([&descriptor]);
		let before = Date::current_local();
		let Value::Object(value) = constructor.construct(descriptor.id(), Vec::new()).unwrap() else { panic!() };
		let after = Date::current_local();
		assert_eq!(value.fields["number"], Value::Integer(0));
		assert!(value.fields["date"] == Value::Date(before) || value.fields["date"] == Value::Date(after));
		assert_eq!(value.fields["json"], Value::Json(crate::json::JsonValue::default()));
		let Value::Enum(status) = &value.fields["status"] else { panic!() };
		assert_eq!(status.variant_name, "Ready");
		assert_eq!(*status.backing_value, Value::Integer(7));
	}

	#[test]
	fn rejects_invalid_or_cyclic_descriptor_defaults_without_panicking() {
		let id = ObjectTypeId::from_raw(0);
		let recursive = descriptor(vec![field("next", ObjectValueTypeDescriptor::Object(id), None)]);
		let error = ObjectConstructor::new([&recursive]).construct(id, Vec::new()).unwrap_err();
		assert!(error.message.contains("recursive"));
		assert_eq!(error.path, ["next", "next"]);

		let missing = descriptor(vec![field("child", ObjectValueTypeDescriptor::Object(ObjectTypeId::from_raw(99)), None)]);
		let error = ObjectConstructor::new([&missing]).construct(id, Vec::new()).unwrap_err();
		assert!(error.message.contains("99"));
		assert_eq!(error.path, ["child"]);

		let union = descriptor(vec![field("choice", ObjectValueTypeDescriptor::Union(vec![ObjectValueTypeDescriptor::Int, ObjectValueTypeDescriptor::Text]), None)]);
		let constructor = ObjectConstructor::new([&union]);
		assert!(constructor.construct(id, Vec::new()).is_err());
		assert!(constructor.construct(id, vec![("choice".into(), Value::Integer(3))]).is_ok());
	}

	#[test]
	fn nested_defaults_retain_paths_and_supplied_objects_require_nominal_identity() {
		let id = ObjectTypeId::from_raw(0);
		let invalid = descriptor(vec![field("values", ObjectValueTypeDescriptor::Array(Box::new(ObjectValueTypeDescriptor::Int)),
			Some(ObjectDefaultValue::Array(vec![ObjectDefaultValue::Text("wrong".into())])))]);
		let error = ObjectConstructor::new([&invalid]).construct(id, Vec::new()).unwrap_err();
		assert_eq!(error.path, ["values", "[1]"]);

		let child = ObjectTypeDescriptor { id: ObjectTypeId::from_raw(1), display_name: "Child".into(), shape: ObjectTypeDescriptorShape::Fields(Vec::new()) };
		let parent = descriptor(vec![field("child", ObjectValueTypeDescriptor::Object(child.id()), None)]);
		let constructor = ObjectConstructor::new([&parent, &child]);
		let wrong = Value::Object(ObjectValue::new(ObjectTypeId::from_raw(2), BTreeMap::new()));
		assert_eq!(constructor.construct(id, vec![("child".into(), wrong)]).unwrap_err().path, ["child"]);
	}

	#[test]
	fn root_array_defaults_remain_arrays() {
		let descriptor = ObjectTypeDescriptor {
			id: ObjectTypeId::from_raw(0), display_name: "Items".into(),
			shape: ObjectTypeDescriptorShape::RootArray(ObjectValueTypeDescriptor::Int),
		};
		let constructor = ObjectConstructor::new([&descriptor]);
		assert_eq!(constructor.object_default(descriptor.id()).unwrap(), ObjectDefaultValue::Array(Vec::new()));
		assert!(constructor.construct(descriptor.id(), vec![("item".into(), Value::Integer(1))]).is_err());
	}
}

#[cfg(test)]
mod vm_tests {
	use super::*;
	use crate::ast::Visibility;

	#[test]
	fn make_object_fills_omitted_fields_after_round_trip_without_compiler() {
		let id = ObjectTypeId::from_raw(0);
		let program = Program::new(vec![Instruction::MakeObject { object_type_id: id, field_names: Vec::new() }])
			.with_object_type_descriptors(vec![ObjectTypeDescriptor {
				id, display_name: "Model".into(),
				shape: ObjectTypeDescriptorShape::Fields(vec![ObjectFieldDescriptor {
					name: "status".into(), is_quoted: false, visibility: Visibility::Private,
					data_type: ObjectValueTypeDescriptor::Enum {
						name: "Status".into(), default_variant: "Ready".into(), default_value: Constant::Integer(7), variants: vec![("Ready".into(), Constant::Integer(7))],
					},
					explicit_default: None,
				}]),
			}]);
		let program = crate::object_file::read_program(&crate::object_file::write_program(&program)).unwrap();
		let value = crate::vm::VirtualMachine::new().run(&program).unwrap().unwrap();
		let Value::Object(object) = value else { panic!() };
		let Value::Enum(status) = &object.fields["status"] else { panic!() };
		assert_eq!(status.variant_name, "Ready");
		assert_eq!(*status.backing_value, Value::Integer(7));
	}
}
