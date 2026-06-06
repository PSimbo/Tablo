use crate::ast::DataType;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuiltInFunction {
	Disp,
	Displn,
	Len,
}

impl BuiltInFunction {
	pub fn from_id(id: u8) -> Option<Self> {
		match id {
			1 => Some(Self::Len),
			2 => Some(Self::Disp),
			3 => Some(Self::Displn),
			_ => None,
		}
	}

	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			"len" => Some(Self::Len),
			"disp" => Some(Self::Disp),
			"displn" => Some(Self::Displn),
			_ => None,
		}
	}

	pub fn id(self) -> u8 {
		match self {
			Self::Len => 1,
			Self::Disp => 2,
			Self::Displn => 3,
		}
	}

	pub fn name(self) -> &'static str {
		match self {
			Self::Len => "len",
			Self::Disp => "disp",
			Self::Displn => "displn",
		}
	}

	pub fn return_type(self, argument_types: &[DataType]) -> Option<DataType> {
		match self {
			Self::Len => match argument_types {
				[DataType::Array(_)] | [DataType::EmptyArray] => Some(DataType::Int),
				_ => None,
			},
			Self::Disp | Self::Displn => match argument_types {
				[DataType::Text] => Some(DataType::Void),
				_ => None,
			},
		}
	}

	pub fn supports_arity(self, argument_count: usize) -> bool {
		match self {
			Self::Len | Self::Disp | Self::Displn => argument_count == 1,
		}
	}

	pub fn produces_runtime_value(self) -> bool {
		match self {
			Self::Len => true,
			Self::Disp | Self::Displn => false,
		}
	}
}
