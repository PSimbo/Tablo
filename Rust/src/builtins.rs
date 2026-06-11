use crate::ast::DataType;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuiltInFunction {
	Disp,
	Displn,
	Exists,
	Len,
	Locked,
}

impl BuiltInFunction {
	pub fn from_id(id: u8) -> Option<Self> {
		match id {
			1 => Some(Self::Len),
			2 => Some(Self::Disp),
			3 => Some(Self::Displn),
			4 => Some(Self::Exists),
			5 => Some(Self::Locked),
			_ => None,
		}
	}

	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			"len" => Some(Self::Len),
			"disp" => Some(Self::Disp),
			"displn" => Some(Self::Displn),
			"exists" => Some(Self::Exists),
			"locked" => Some(Self::Locked),
			_ => None,
		}
	}

	pub fn id(self) -> u8 {
		match self {
			Self::Len => 1,
			Self::Disp => 2,
			Self::Displn => 3,
			Self::Exists => 4,
			Self::Locked => 5,
		}
	}

	pub fn name(self) -> &'static str {
		match self {
			Self::Len => "len",
			Self::Disp => "disp",
			Self::Displn => "displn",
			Self::Exists => "exists",
			Self::Locked => "locked",
		}
	}

	pub fn return_type(self, argument_types: &[DataType]) -> Option<DataType> {
		fn strip_non_null(data_type: &DataType) -> &DataType {
			match data_type {
				DataType::NonNull(inner) => strip_non_null(inner),
				other => other,
			}
		}

		match self {
			Self::Len => match argument_types {
				[arg] if matches!(strip_non_null(arg), DataType::Array(_) | DataType::EmptyArray) => {
					Some(DataType::NonNull(Box::new(DataType::Int)))
				}
				_ => None,
			},
			Self::Disp | Self::Displn => match argument_types {
				[arg] if matches!(strip_non_null(arg), DataType::Text) => Some(DataType::Void),
				_ => None,
			},
			Self::Exists | Self::Locked => match argument_types {
				[arg] if matches!(strip_non_null(arg), DataType::RecordPointer(_)) => {
					Some(DataType::NonNull(Box::new(DataType::Bool)))
				}
				_ => None,
			},
		}
	}

	pub fn supports_arity(self, argument_count: usize) -> bool {
		match self {
			Self::Len | Self::Disp | Self::Displn | Self::Exists | Self::Locked => argument_count == 1,
		}
	}

	pub fn produces_runtime_value(self) -> bool {
		match self {
			Self::Len | Self::Exists | Self::Locked => true,
			Self::Disp | Self::Displn => false,
		}
	}
}
