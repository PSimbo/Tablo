use crate::ast::DataType;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuiltInFunction {
	Len,
}

impl BuiltInFunction {
	pub fn from_id(id: u8) -> Option<Self> {
		match id {
			1 => Some(Self::Len),
			_ => None,
		}
	}

	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			"len" => Some(Self::Len),
			_ => None,
		}
	}

	pub fn id(self) -> u8 {
		match self {
			Self::Len => 1,
		}
	}

	pub fn name(self) -> &'static str {
		match self {
			Self::Len => "len",
		}
	}

	pub fn return_type(self, argument_types: &[DataType]) -> Option<DataType> {
		match self {
			Self::Len => match argument_types {
				[DataType::Array(_)] | [DataType::EmptyArray] => Some(DataType::Int),
				_ => None,
			}
		}
	}

	pub fn supports_arity(self, argument_count: usize) -> bool {
		match self {
			Self::Len => argument_count == 1,
		}
	}
}
