use crate::ast::DataType;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuiltInFunction {
	BoolCast,
	DateCast,
	DecCast,
	Disp,
	Displn,
	Exists,
	IntCast,
	Len,
	Locked,
	TextCast,
}

impl BuiltInFunction {
	pub fn from_id(id: u8) -> Option<Self> {
		match id {
			1 => Some(Self::Len),
			2 => Some(Self::Disp),
			3 => Some(Self::Displn),
			4 => Some(Self::Exists),
			5 => Some(Self::Locked),
			6 => Some(Self::IntCast),
			7 => Some(Self::TextCast),
			8 => Some(Self::DecCast),
			9 => Some(Self::BoolCast),
			10 => Some(Self::DateCast),
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
			"int" => Some(Self::IntCast),
			"text" => Some(Self::TextCast),
			"dec" => Some(Self::DecCast),
			"bool" => Some(Self::BoolCast),
			"date" => Some(Self::DateCast),
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
			Self::IntCast => 6,
			Self::TextCast => 7,
			Self::DecCast => 8,
			Self::BoolCast => 9,
			Self::DateCast => 10,
		}
	}

	pub fn name(self) -> &'static str {
		match self {
			Self::Len => "len",
			Self::Disp => "disp",
			Self::Displn => "displn",
			Self::Exists => "exists",
			Self::Locked => "locked",
			Self::IntCast => "int",
			Self::TextCast => "text",
			Self::DecCast => "dec",
			Self::BoolCast => "bool",
			Self::DateCast => "date",
		}
	}

	pub fn return_type(self, argument_types: &[DataType]) -> Option<DataType> {
		match self {
			Self::Len => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Array(_) | DataType::EmptyArray) => {
					Some(DataType::Int)
				}
				_ => None,
			},
			Self::Disp | Self::Displn => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Text) => Some(DataType::Void),
				_ => None,
			},
			Self::Exists | Self::Locked => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::RecordPointer(_)) => {
					Some(DataType::Bool)
				}
				_ => None,
			},
			Self::IntCast | Self::TextCast | Self::DecCast | Self::BoolCast | Self::DateCast => None,
		}
	}

	pub fn supports_arity(self, argument_count: usize) -> bool {
		match self {
			Self::Len | Self::Disp | Self::Displn | Self::Exists | Self::Locked
			| Self::IntCast | Self::TextCast | Self::DecCast | Self::BoolCast | Self::DateCast => argument_count == 1,
		}
	}

	pub fn produces_runtime_value(self) -> bool {
		match self {
			Self::Len | Self::Exists | Self::Locked
			| Self::IntCast | Self::TextCast | Self::DecCast | Self::BoolCast | Self::DateCast => true,
			Self::Disp | Self::Displn => false,
		}
	}
}
