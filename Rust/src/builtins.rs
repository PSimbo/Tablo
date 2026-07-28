use crate::ast::DataType;

const BUILTIN_BOOL_CAST: u8 = 1;
const BUILTIN_CONTAINS: u8 = BUILTIN_BOOL_CAST + 1;
const BUILTIN_COUNT_OF: u8 = BUILTIN_CONTAINS + 1;
const BUILTIN_DATE_CAST: u8 = BUILTIN_COUNT_OF + 1;
const BUILTIN_DAY: u8 = BUILTIN_DATE_CAST + 1;
const BUILTIN_DEC_CAST: u8 = BUILTIN_DAY + 1;
const BUILTIN_DISP: u8 = BUILTIN_DEC_CAST + 1;
const BUILTIN_DISPLN: u8 = BUILTIN_DISP + 1;
const BUILTIN_EXISTS: u8 = BUILTIN_DISPLN + 1;
const BUILTIN_FIRST_OF: u8 = BUILTIN_EXISTS + 1;
const BUILTIN_FORMAT: u8 = BUILTIN_FIRST_OF + 1;
const BUILTIN_HOUR: u8 = BUILTIN_FORMAT + 1;
const BUILTIN_INDEX_OF: u8 = BUILTIN_HOUR + 1;
const BUILTIN_INT_CAST: u8 = BUILTIN_INDEX_OF + 1;
const BUILTIN_LAST_OF: u8 = BUILTIN_INT_CAST + 1;
const BUILTIN_LEN: u8 = BUILTIN_LAST_OF + 1;
const BUILTIN_LOCKED: u8 = BUILTIN_LEN + 1;
const BUILTIN_MINUTE: u8 = BUILTIN_LOCKED + 1;
const BUILTIN_MONTH: u8 = BUILTIN_MINUTE + 1;
const BUILTIN_SECOND: u8 = BUILTIN_MONTH + 1;
const BUILTIN_SEQ_NEXT: u8 = BUILTIN_SECOND + 1;
const BUILTIN_SPLIT: u8 = BUILTIN_SEQ_NEXT + 1;
const BUILTIN_TEXT_CAST: u8 = BUILTIN_SPLIT + 1;
const BUILTIN_TRIM: u8 = BUILTIN_TEXT_CAST + 1;
const BUILTIN_YEAR: u8 = BUILTIN_TRIM + 1;

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
	FirstOf,
	Format,
	Hour,
	IndexOf,
	IntCast,
	LastOf,
	Len,
	Locked,
	Minute,
	Month,
	Second,
	SeqNext,
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
			Self::FirstOf,
			Self::Format,
			Self::Hour,
			Self::IndexOf,
			Self::IntCast,
			Self::LastOf,
			Self::Len,
			Self::Locked,
			Self::Minute,
			Self::Month,
			Self::Second,
			Self::SeqNext,
			Self::Split,
			Self::TextCast,
			Self::Trim,
			Self::Year,
		]
	}

	pub fn from_id(id: u8) -> Option<Self> {
		match id {
			BUILTIN_BOOL_CAST => Some(Self::BoolCast),
			BUILTIN_CONTAINS => Some(Self::Contains),
			BUILTIN_COUNT_OF => Some(Self::CountOf),
			BUILTIN_DATE_CAST => Some(Self::DateCast),
			BUILTIN_DAY => Some(Self::Day),
			BUILTIN_DEC_CAST => Some(Self::DecCast),
			BUILTIN_DISP => Some(Self::Disp),
			BUILTIN_DISPLN => Some(Self::Displn),
			BUILTIN_EXISTS => Some(Self::Exists),
			BUILTIN_FIRST_OF => Some(Self::FirstOf),
			BUILTIN_FORMAT => Some(Self::Format),
			BUILTIN_HOUR => Some(Self::Hour),
			BUILTIN_INDEX_OF => Some(Self::IndexOf),
			BUILTIN_INT_CAST => Some(Self::IntCast),
			BUILTIN_LAST_OF => Some(Self::LastOf),
			BUILTIN_LEN => Some(Self::Len),
			BUILTIN_LOCKED => Some(Self::Locked),
			BUILTIN_MINUTE => Some(Self::Minute),
			BUILTIN_MONTH => Some(Self::Month),
			BUILTIN_SECOND => Some(Self::Second),
			BUILTIN_SEQ_NEXT => Some(Self::SeqNext),
			BUILTIN_SPLIT => Some(Self::Split),
			BUILTIN_TEXT_CAST => Some(Self::TextCast),
			BUILTIN_TRIM => Some(Self::Trim),
			BUILTIN_YEAR => Some(Self::Year),
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
			"firstof" => Some(Self::FirstOf),
			"format" => Some(Self::Format),
			"hour" => Some(Self::Hour),
			"indexof" => Some(Self::IndexOf),
			"int" => Some(Self::IntCast),
			"lastof" => Some(Self::LastOf),
			"len" => Some(Self::Len),
			"locked" => Some(Self::Locked),
			"minute" => Some(Self::Minute),
			"month" => Some(Self::Month),
			"second" => Some(Self::Second),
			"seqnext" => Some(Self::SeqNext),
			"split" => Some(Self::Split),
			"text" => Some(Self::TextCast),
			"trim" => Some(Self::Trim),
			"year" => Some(Self::Year),
			_ => None,
		}
	}

	pub fn id(self) -> u8 {
		match self {
			Self::BoolCast => BUILTIN_BOOL_CAST,
			Self::Contains => BUILTIN_CONTAINS,
			Self::CountOf => BUILTIN_COUNT_OF,
			Self::DateCast => BUILTIN_DATE_CAST,
			Self::Day => BUILTIN_DAY,
			Self::DecCast => BUILTIN_DEC_CAST,
			Self::Disp => BUILTIN_DISP,
			Self::Displn => BUILTIN_DISPLN,
			Self::Exists => BUILTIN_EXISTS,
			Self::FirstOf => BUILTIN_FIRST_OF,
			Self::Format => BUILTIN_FORMAT,
			Self::Hour => BUILTIN_HOUR,
			Self::IndexOf => BUILTIN_INDEX_OF,
			Self::IntCast => BUILTIN_INT_CAST,
			Self::LastOf => BUILTIN_LAST_OF,
			Self::Len => BUILTIN_LEN,
			Self::Locked => BUILTIN_LOCKED,
			Self::Minute => BUILTIN_MINUTE,
			Self::Month => BUILTIN_MONTH,
			Self::Second => BUILTIN_SECOND,
			Self::SeqNext => BUILTIN_SEQ_NEXT,
			Self::Split => BUILTIN_SPLIT,
			Self::TextCast => BUILTIN_TEXT_CAST,
			Self::Trim => BUILTIN_TRIM,
			Self::Year => BUILTIN_YEAR,
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
			Self::FirstOf => "firstof",
			Self::Format => "format",
			Self::Hour => "hour",
			Self::IndexOf => "indexof",
			Self::IntCast => "int",
			Self::LastOf => "lastof",
			Self::Len => "len",
			Self::Locked => "locked",
			Self::Minute => "minute",
			Self::Month => "month",
			Self::Second => "second",
			Self::SeqNext => "seqnext",
			Self::Split => "split",
			Self::TextCast => "text",
			Self::Trim => "trim",
			Self::Year => "year",
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
			| Self::FirstOf
			| Self::Format
			| Self::Hour
			| Self::IndexOf
			| Self::IntCast
			| Self::LastOf
			| Self::Len
			| Self::Locked
			| Self::Minute
			| Self::Month
			| Self::Second
			| Self::SeqNext
			| Self::Split
			| Self::TextCast
			| Self::Trim
			| Self::Year => true,
			Self::Disp | Self::Displn => false,
		}
	}

	pub fn return_type(self, argument_types: &[DataType]) -> Result<Option<DataType>, ()> {
		if matches!(self, Self::Disp | Self::Displn) {
			return match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::Text) => Ok(None),
				_ => Err(()),
			};
		}

		self.value_return_type(argument_types).map(Some).ok_or(())
	}

	pub fn signature_labels(self) -> Vec<String> {
		self.signatures().iter()
			.map(|signature| signature.label(self.name()))
			.collect()
	}

	pub fn signatures(self) -> Vec<BuiltInSignature> {
		use BuiltInParameterType::*;

		match self {
			Self::BoolCast => vec![
				Self::signature(&[("v", EnumBacked(DataType::Bool))], Some(DataType::Bool)),
			],
			Self::Contains => vec![
				Self::signature(&[("str", Text), ("sub", Text)], Some(DataType::Bool)),
				Self::signature(&[("arr", ArrayText), ("elem", Text)], Some(DataType::Bool)),
			],
			Self::CountOf => vec![
				Self::signature(&[("str", Text), ("arr", ArrayText)], Some(DataType::Int)),
				Self::signature(&[("sub", Text), ("str", Text)], Some(DataType::Int)),
			],
			Self::DateCast => vec![
				Self::signature(&[("d", Text)], Some(DataType::Date)),
				Self::signature(&[("d", EnumBacked(DataType::Date))], Some(DataType::Date)),
			],
			Self::Day => vec![
				Self::signature(&[("d", Date)], Some(DataType::Int)),
				Self::signature(&[("t", Timestamp)], Some(DataType::Int)),
				Self::signature(&[("t", TimestampTz)], Some(DataType::Int)),
			],
			Self::DecCast => vec![
				Self::signature(&[("v", EnumBacked(DataType::Dec))], Some(DataType::Dec)),
			],
			Self::Disp => vec![
				Self::signature(&[("fmt", Text)], None),
			],
			Self::Displn => vec![
				Self::signature(&[("fmt", Text)], None),
			],
			Self::Exists => vec![
				Self::signature(&[("v", RecordPointer)], Some(DataType::Bool)),
			],
			Self::FirstOf => vec![
				Self::variadic_signature("v1", Any, "v2", ArrayAny, DataType::Bool),
			],
			Self::Format => vec![
				Self::signature(&[("v", Dec), ("pattern", Text)], Some(DataType::Text)),
				Self::signature(&[("v", Int), ("pattern", Text)], Some(DataType::Text)),
				Self::signature(&[("v", Date), ("pattern", Text)], Some(DataType::Text)),
				Self::signature(&[("v", Time), ("pattern", Text)], Some(DataType::Text)),
				Self::signature(&[("v", Timestamp), ("pattern", Text)], Some(DataType::Text)),
			],
			Self::Hour => vec![
				Self::signature(&[("t", Time)], Some(DataType::Int)),
				Self::signature(&[("t", TimeTz)], Some(DataType::Int)),
				Self::signature(&[("t", Timestamp)], Some(DataType::Int)),
				Self::signature(&[("t", TimestampTz)], Some(DataType::Int)),
			],
			Self::IndexOf => vec![
				Self::signature(&[("str", Text), ("arr", ArrayText)], Some(DataType::Int.into_nullable())),
				Self::signature(&[("sub", Text), ("str", Text)], Some(DataType::Int.into_nullable())),
			],
			Self::IntCast => vec![
				Self::signature(&[("v", Bool)], Some(DataType::Int)),
				Self::signature(&[("v", Text)], Some(DataType::Int)),
				Self::signature(&[("v", EnumBacked(DataType::Int))], Some(DataType::Int)),
			],
			Self::LastOf => vec![
				Self::variadic_signature("v1", Any, "v2", ArrayAny, DataType::Bool),
			],
			Self::Len => vec![
				Self::signature(&[("v", ArrayAny)], Some(DataType::Int)),
				Self::signature(&[("str", Text)], Some(DataType::Int)),
			],
			Self::Locked => vec![
				Self::signature(&[("v", RecordPointer)], Some(DataType::Bool)),
			],
			Self::Minute => vec![
				Self::signature(&[("t", Time)], Some(DataType::Int)),
				Self::signature(&[("t", TimeTz)], Some(DataType::Int)),
				Self::signature(&[("t", Timestamp)], Some(DataType::Int)),
				Self::signature(&[("t", TimestampTz)], Some(DataType::Int)),
			],
			Self::Month => vec![
				Self::signature(&[("d", Date)], Some(DataType::Int)),
				Self::signature(&[("t", Timestamp)], Some(DataType::Int)),
				Self::signature(&[("t", TimestampTz)], Some(DataType::Int)),
			],
			Self::Second => vec![
				Self::signature(&[("t", Time)], Some(DataType::Int)),
				Self::signature(&[("t", TimeTz)], Some(DataType::Int)),
				Self::signature(&[("t", Timestamp)], Some(DataType::Int)),
				Self::signature(&[("t", TimestampTz)], Some(DataType::Int)),
			],
			Self::SeqNext => vec![
				Self::signature(&[("s", Sequence)], Some(DataType::Int)),
			],
			Self::Split => vec![
				Self::signature(
					&[("str", Text), ("by", Text)],
					Some(DataType::Array(Box::new(DataType::Text))),
				),
			],
			Self::TextCast => vec![
				Self::signature(&[("v", EnumBacked(DataType::Text))], Some(DataType::Text)),
			],
			Self::Trim => vec![
				Self::signature(&[("str", Text)], Some(DataType::Text)),
			],
			Self::Year => vec![
				Self::signature(&[("d", Date)], Some(DataType::Int)),
				Self::signature(&[("t", Timestamp)], Some(DataType::Int)),
				Self::signature(&[("t", TimestampTz)], Some(DataType::Int)),
			],
		}
	}

	pub fn supports_arity(self, argument_count: usize) -> bool {
		match self {
			Self::Contains
			| Self::CountOf
			| Self::Format
			| Self::IndexOf
			| Self::Split => argument_count == 2,
			Self::FirstOf
			| Self::LastOf => argument_count >= 1,
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
			| Self::SeqNext
			| Self::TextCast
			| Self::Trim
			| Self::Year => argument_count == 1,
		}
	}

	fn signature(
		parameters: &[(&'static str, BuiltInParameterType)],
		return_type: Option<DataType>,
	) -> BuiltInSignature {
		BuiltInSignature {
			parameters: parameters.iter()
				.map(|(name, data_type)| BuiltInParameter {
					name,
					data_type: data_type.clone(),
					is_variadic: false,
				})
				.collect(),
			return_type,
		}
	}

	fn value_return_type(self, argument_types: &[DataType]) -> Option<DataType> {
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
			Self::Disp | Self::Displn => None,
			Self::Exists | Self::Locked => match argument_types {
				[arg] if matches!(arg.without_nullability(), DataType::RecordPointer(_)) => {
					Some(DataType::Bool)
				}
				_ => None,
			},
			Self::FirstOf | Self::LastOf => match argument_types {
				[] => None,
				_ => Some(DataType::Bool),
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
				[arg] if matches!(arg.without_nullability(), DataType::Bool) => Some(DataType::Int),
				[arg] if matches!(arg.without_nullability(), DataType::Text) => Some(DataType::Int),
				_ => None,
			},
			Self::SeqNext => None,
			Self::TextCast | Self::DecCast | Self::BoolCast => None,
		}
	}

	fn variadic_signature(
		required_name: &'static str,
		required_type: BuiltInParameterType,
		variadic_name: &'static str,
		variadic_type: BuiltInParameterType,
		return_type: DataType,
	) -> BuiltInSignature {
		BuiltInSignature {
			parameters: vec![
				BuiltInParameter {
					name: required_name,
					data_type: required_type,
					is_variadic: false,
				},
				BuiltInParameter {
					name: variadic_name,
					data_type: variadic_type,
					is_variadic: true,
				},
			],
			return_type: Some(return_type),
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BuiltInParameterType {
	Any,
	ArrayAny,
	ArrayText,
	Bool,
	Date,
	Dec,
	EnumBacked(DataType),
	Int,
	RecordPointer,
	Sequence,
	Text,
	Time,
	TimeTz,
	Timestamp,
	TimestampTz,
}

impl BuiltInParameterType {
	fn name(&self) -> String {
		match self {
			Self::Any => String::from("any"),
			Self::ArrayAny => String::from("[any]"),
			Self::ArrayText => String::from("[text]"),
			Self::Bool => String::from("bool"),
			Self::Date => String::from("date"),
			Self::Dec => String::from("dec"),
			Self::EnumBacked(backing_type) => format!("<{}-backed enum>", backing_type.name()),
			Self::Int => String::from("int"),
			Self::RecordPointer => String::from("rec <table>"),
			Self::Sequence => String::from("seq <sequence>"),
			Self::Text => String::from("text"),
			Self::Time => String::from("time"),
			Self::TimeTz => String::from("timetz"),
			Self::Timestamp => String::from("timestamp"),
			Self::TimestampTz => String::from("timestamptz"),
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BuiltInParameter {
	pub name: &'static str,
	pub data_type: BuiltInParameterType,
	pub is_variadic: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BuiltInSignature {
	pub parameters: Vec<BuiltInParameter>,
	pub return_type: Option<DataType>,
}

impl BuiltInSignature {
	pub fn label(&self, function_name: &str) -> String {
		let parameters = self.parameters.iter()
			.map(|parameter| {
				let prefix = if parameter.is_variadic { "..." } else { "" };
				format!("{prefix}{}: {}", parameter.name, parameter.data_type.name())
			})
			.collect::<Vec<_>>()
			.join(", ");
		let return_type = self.return_type.as_ref()
			.map(|data_type| format!(": {}", data_type.name()))
			.unwrap_or_default();

		format!("{function_name}({parameters}){return_type}")
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn describes_stable_parameter_names_for_overloaded_built_ins() {
		let signatures = BuiltInFunction::Contains.signatures();

		assert_eq!(signatures.len(), 2);
		assert_eq!(
			signatures[0].parameters.iter().map(|parameter| parameter.name).collect::<Vec<_>>(),
			["str", "sub"],
		);
		assert_eq!(
			signatures[1].parameters.iter().map(|parameter| parameter.name).collect::<Vec<_>>(),
			["arr", "elem"],
		);
	}

	#[test]
	fn distinguishes_invalid_no_return_and_value_return_signatures() {
		assert_eq!(BuiltInFunction::Disp.return_type(&[DataType::Text]), Ok(None));
		assert_eq!(BuiltInFunction::Len.return_type(&[DataType::Text]), Ok(Some(DataType::Int)));
		assert_eq!(BuiltInFunction::Len.return_type(&[DataType::Bool]), Err(()));
	}

	#[test]
	fn formats_source_facing_signature_labels() {
		assert_eq!(
			BuiltInFunction::Contains.signature_labels(),
			[
				"contains(str: text, sub: text): bool",
				"contains(arr: [text], elem: text): bool",
			],
		);
		assert_eq!(BuiltInFunction::Disp.signature_labels(), ["disp(fmt: text)"]);
		assert_eq!(
			BuiltInFunction::FirstOf.signature_labels(),
			["firstof(v1: any, ...v2: [any]): bool"],
		);
	}

	#[test]
	fn represents_special_built_in_parameter_categories_explicitly() {
		assert_eq!(
			BuiltInFunction::SeqNext.signatures()[0].parameters[0].data_type,
			BuiltInParameterType::Sequence,
		);
		assert_eq!(
			BuiltInFunction::Exists.signatures()[0].parameters[0].data_type,
			BuiltInParameterType::RecordPointer,
		);
		assert_eq!(
			BuiltInFunction::IntCast.signatures()[2].parameters[0].data_type,
			BuiltInParameterType::EnumBacked(DataType::Int),
		);
	}
}
