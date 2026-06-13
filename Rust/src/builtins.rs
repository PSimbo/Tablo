use crate::ast::DataType;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuiltInFunction {
	BoolCast,
	Contains,
	CountOf,
	DateCast,
	DecCast,
	Disp,
	Displn,
	Exists,
	IndexOf,
	IntCast,
	Len,
	Locked,
	Split,
	TextCast,
	Trim,
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
			11 => Some(Self::Contains),
			12 => Some(Self::Trim),
			13 => Some(Self::CountOf),
			14 => Some(Self::IndexOf),
			15 => Some(Self::Split),
			_ => None,
		}
	}

	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			"bool" => Some(Self::BoolCast),
			"contains" => Some(Self::Contains),
			"countof" => Some(Self::CountOf),
			"date" => Some(Self::DateCast),
			"dec" => Some(Self::DecCast),
			"disp" => Some(Self::Disp),
			"displn" => Some(Self::Displn),
			"exists" => Some(Self::Exists),
			"indexof" => Some(Self::IndexOf),
			"int" => Some(Self::IntCast),
			"len" => Some(Self::Len),
			"locked" => Some(Self::Locked),
			"split" => Some(Self::Split),
			"text" => Some(Self::TextCast),
			"trim" => Some(Self::Trim),
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
			Self::Contains => 11,
			Self::Trim => 12,
			Self::CountOf => 13,
			Self::IndexOf => 14,
			Self::Split => 15,
		}
	}

	pub fn name(self) -> &'static str {
		match self {
			Self::BoolCast => "bool",
			Self::Contains => "contains",
			Self::CountOf => "countof",
			Self::DateCast => "date",
			Self::DecCast => "dec",
			Self::Disp => "disp",
			Self::Displn => "displn",
			Self::Exists => "exists",
			Self::IndexOf => "indexof",
			Self::IntCast => "int",
			Self::Len => "len",
			Self::Locked => "locked",
			Self::Split => "split",
			Self::TextCast => "text",
			Self::Trim => "trim",
		}
	}

	pub fn return_type(self, argument_types: &[DataType]) -> Option<DataType> {
		match self {
			Self::Contains => match argument_types {
				[left, right]
					if matches!(left.without_nullability(), DataType::Text)
						&& matches!(right.without_nullability(), DataType::Text) => {
					Some(DataType::Bool)
				}
				[left, right]
					if matches!(
						left.without_nullability(),
						DataType::Array(element_type) if matches!(element_type.without_nullability(), DataType::Text)
					)
						&& matches!(right.without_nullability(), DataType::Text) => {
					Some(DataType::Bool)
				}
				_ => None,
			},
			Self::CountOf => match argument_types {
				[left, right]
					if matches!(left.without_nullability(), DataType::Text)
						&& matches!(right.without_nullability(), DataType::Text) => {
					Some(DataType::Int)
				}
				[left, right]
					if matches!(left.without_nullability(), DataType::Text)
						&& matches!(
							right.without_nullability(),
							DataType::Array(element_type) if matches!(element_type.without_nullability(), DataType::Text)
						) => {
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
			Self::IndexOf => match argument_types {
				[left, right]
					if matches!(left.without_nullability(), DataType::Text)
						&& matches!(right.without_nullability(), DataType::Text) => {
					Some(DataType::Int.into_nullable())
				}
				[left, right]
					if matches!(left.without_nullability(), DataType::Text)
						&& matches!(
							right.without_nullability(),
							DataType::Array(element_type) if matches!(element_type.without_nullability(), DataType::Text)
						) => {
					Some(DataType::Int.into_nullable())
				}
				_ => None,
			},
			Self::Len => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Array(_) | DataType::EmptyArray) => {
					Some(DataType::Int)
				}
				_ => None,
			},
			Self::Split => match argument_types {
				[left, right]
					if matches!(left.without_nullability(), DataType::Text)
						&& matches!(right.without_nullability(), DataType::Text) => {
					Some(DataType::Array(Box::new(DataType::Text)))
				}
				_ => None,
			},
			Self::Trim => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Text) => Some(DataType::Text),
				_ => None,
			},
			Self::IntCast | Self::TextCast | Self::DecCast | Self::BoolCast | Self::DateCast => None,
		}
	}

	pub fn supports_arity(self, argument_count: usize) -> bool {
		match self {
			Self::Contains | Self::CountOf | Self::IndexOf | Self::Split => argument_count == 2,
			Self::Len | Self::Disp | Self::Displn | Self::Exists | Self::Locked
			| Self::Trim
			| Self::IntCast | Self::TextCast | Self::DecCast | Self::BoolCast | Self::DateCast => argument_count == 1,
		}
	}

	pub fn produces_runtime_value(self) -> bool {
		match self {
			Self::Len | Self::Contains | Self::CountOf | Self::Exists | Self::IndexOf | Self::Locked | Self::Split | Self::Trim
			| Self::IntCast | Self::TextCast | Self::DecCast | Self::BoolCast | Self::DateCast => true,
			Self::Disp | Self::Displn => false,
		}
	}
}
