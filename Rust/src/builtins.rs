use crate::ast::DataType;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuiltInFunction {
	BoolCast,
	Contains,
	CountOf,
	DateCast,
	Day,
	DecCast,
	Disp,
	Displn,
	Exists,
	Format,
	Hour,
	IndexOf,
	IntCast,
	Len,
	Locked,
	Minute,
	Month,
	Second,
	Split,
	TextCast,
	Trim,
	Year,
}

impl BuiltInFunction {
	pub fn all() -> &'static [Self] {
		&[
			Self::BoolCast,
			Self::Contains,
			Self::CountOf,
			Self::DateCast,
			Self::Day,
			Self::DecCast,
			Self::Disp,
			Self::Displn,
			Self::Exists,
			Self::Format,
			Self::Hour,
			Self::IndexOf,
			Self::IntCast,
			Self::Len,
			Self::Locked,
			Self::Minute,
			Self::Month,
			Self::Second,
			Self::Split,
			Self::TextCast,
			Self::Trim,
			Self::Year,
		]
	}

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
			16 => Some(Self::Day),
			17 => Some(Self::Month),
			18 => Some(Self::Year),
			19 => Some(Self::Hour),
			20 => Some(Self::Minute),
			21 => Some(Self::Second),
			22 => Some(Self::Format),
			_ => None,
		}
	}

	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			"bool" => Some(Self::BoolCast),
			"contains" => Some(Self::Contains),
			"countof" => Some(Self::CountOf),
			"date" => Some(Self::DateCast),
			"day" => Some(Self::Day),
			"dec" => Some(Self::DecCast),
			"disp" => Some(Self::Disp),
			"displn" => Some(Self::Displn),
			"exists" => Some(Self::Exists),
			"format" => Some(Self::Format),
			"hour" => Some(Self::Hour),
			"indexof" => Some(Self::IndexOf),
			"int" => Some(Self::IntCast),
			"len" => Some(Self::Len),
			"locked" => Some(Self::Locked),
			"minute" => Some(Self::Minute),
			"month" => Some(Self::Month),
			"second" => Some(Self::Second),
			"split" => Some(Self::Split),
			"text" => Some(Self::TextCast),
			"trim" => Some(Self::Trim),
			"year" => Some(Self::Year),
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
			Self::Day => 16,
			Self::Month => 17,
			Self::Year => 18,
			Self::Hour => 19,
			Self::Minute => 20,
			Self::Second => 21,
			Self::Format => 22,
		}
	}

	pub fn name(self) -> &'static str {
		match self {
			Self::BoolCast => "bool",
			Self::Contains => "contains",
			Self::CountOf => "countof",
			Self::DateCast => "date",
			Self::Day => "day",
			Self::DecCast => "dec",
			Self::Disp => "disp",
			Self::Displn => "displn",
			Self::Exists => "exists",
			Self::Format => "format",
			Self::Hour => "hour",
			Self::IndexOf => "indexof",
			Self::IntCast => "int",
			Self::Len => "len",
			Self::Locked => "locked",
			Self::Minute => "minute",
			Self::Month => "month",
			Self::Second => "second",
			Self::Split => "split",
			Self::TextCast => "text",
			Self::Trim => "trim",
			Self::Year => "year",
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
			Self::Day => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Date | DataType::Timestamp | DataType::TimestampTz) => Some(DataType::Int),
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
			Self::Format => match argument_types {
				[left, right]
					if matches!(left.without_nullability(), DataType::Date | DataType::Dec | DataType::Int | DataType::Time | DataType::Timestamp)
						&& matches!(right.without_nullability(), DataType::Text) => {
					Some(DataType::Text)
				}
				_ => None,
			},
			Self::Hour => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Time | DataType::TimeTz | DataType::Timestamp | DataType::TimestampTz) => Some(DataType::Int),
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
				[arg] if matches!(arg.without_nullability(), DataType::Array(_) | DataType::EmptyArray | DataType::Text) => {
					Some(DataType::Int)
				}
				_ => None,
			},
			Self::Minute => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Time | DataType::TimeTz | DataType::Timestamp | DataType::TimestampTz) => Some(DataType::Int),
				_ => None,
			},
			Self::Month => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Date | DataType::Timestamp | DataType::TimestampTz) => Some(DataType::Int),
				_ => None,
			},
			Self::Second => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Time | DataType::TimeTz | DataType::Timestamp | DataType::TimestampTz) => Some(DataType::Int),
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
			Self::Year => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Date | DataType::Timestamp | DataType::TimestampTz) => Some(DataType::Int),
				_ => None,
			},
			Self::DateCast => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Text) => Some(DataType::Date),
				_ => None,
			},
			Self::IntCast => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Text) => Some(DataType::Int),
				_ => None,
			},
			Self::TextCast | Self::DecCast | Self::BoolCast => None,
		}
	}

	pub fn supports_arity(self, argument_count: usize) -> bool {
		match self {
			Self::Contains
			| Self::CountOf
			| Self::Format
			| Self::IndexOf
			| Self::Split => argument_count == 2,
			Self::BoolCast
			| Self::DateCast
			| Self::Day
			| Self::DecCast
			| Self::Disp
			| Self::Displn
			| Self::Exists
			| Self::Hour
			| Self::IntCast
			| Self::Len
			| Self::Locked
			| Self::Minute
			| Self::Month
			| Self::Second
			| Self::TextCast
			| Self::Trim
			| Self::Year => argument_count == 1,
		}
	}

	pub fn produces_runtime_value(self) -> bool {
		match self {
			Self::BoolCast
			| Self::Contains
			| Self::CountOf
			| Self::DateCast
			| Self::Day
			| Self::DecCast
			| Self::Exists
			| Self::Format
			| Self::Hour
			| Self::IndexOf
			| Self::IntCast
			| Self::Len
			| Self::Locked
			| Self::Minute
			| Self::Month
			| Self::Second
			| Self::Split
			| Self::TextCast
			| Self::Trim
			| Self::Year => true,
			Self::Disp | Self::Displn => false,
		}
	}
}
