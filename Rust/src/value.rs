use std::collections::BTreeMap;
use std::fmt::Display;

use crate::ast::DataType;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DeferredSqliteValue {
	Blob(Vec<u8>),
	Integer(i64),
	Null,
	Real(String),
	Text(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IteratorState {
	Array(ArrayIterator),
	DecimalRange(DecimalRangeIterator),
	IntegerRange(IntegerRangeIterator),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RecordFieldValue {
	DeferredSqlite {
		data_type: DataType,
		is_nullable: bool,
		value: DeferredSqliteValue,
	},
	Materialized(Value),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
	Array(Vec<Value>),
	Boolean(bool),
	Date(Date),
	Decimal(Decimal),
	DecimalRange(DecimalRange),
	Enum(EnumValue),
	Integer(i64),
	IntegerRange(IntegerRange),
	Iterator(IteratorState),
	Null,
	Object(BTreeMap<String, Value>),
	RecordPointer(RecordPointerValue),
	Reference(LocalReference),
	Text(String),
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Array(values) => {
				write!(f, "[")?;

				for (index, value) in values.iter().enumerate() {
					if index > 0 {
						write!(f, ", ")?;
					}

					write!(f, "{value}")?;
				}

				write!(f, "]")
			}
			Value::Boolean(value) => write!(f, "{value}"),
			Value::Date(value) => write!(f, "{value}"),
			Value::Decimal(value) => write!(f, "{value}"),
			Value::DecimalRange(range) => write!(f, "{range}"),
			Value::Enum(value) => write!(f, "{}", value.variant_name),
			Value::Integer(value) => write!(f, "{value}"),
			Value::IntegerRange(range) => write!(f, "{range}"),
			Value::Iterator(_) => write!(f, "<iterator>"),
			Value::Null => write!(f, "null"),
			Value::Object(fields) => {
				write!(f, "{{")?;

				for (index, (name, value)) in fields.iter().enumerate() {
					if index > 0 {
						write!(f, ", ")?;
					}

					write!(f, "{name}: {value}")?;
				}

				write!(f, "}}")
			}
			Value::RecordPointer(record) => {
				if !record.exists {
					write!(f, "<record pointer: missing>")
				}
				else if record.locked {
					write!(f, "<record pointer: locked>")
				}
				else {
					write!(f, "<record pointer>")
				}
			}
			Value::Reference(_) => write!(f, "<reference>"),
			Value::Text(value) => write!(f, "{value}"),
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArrayIterator {
	pub elements: Vec<Value>,
	pub next_index: usize,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Date {
	pub day: u8,
	pub month: u8,
	pub year: i32,
}

impl Date {
	pub fn current_local() -> Self {
		let now = time::OffsetDateTime::now_local()
			.unwrap_or_else(|_| time::OffsetDateTime::now_utc());
		let date = now.date();

		Self {
			day: date.day(),
			month: date.month() as u8,
			year: date.year(),
		}
	}

	pub fn from_literal(literal: &str) -> Result<Self, String> {
		let value = literal.strip_prefix('@').ok_or(String::from("Date literals must begin with `@`."))?;
		let mut parts = value.split('-');
		let year = parts.next().ok_or(String::from("Date literal must use `yyyy-mm-dd` format."))?;
		let month = parts.next().ok_or(String::from("Date literal must use `yyyy-mm-dd` format."))?;
		let day = parts.next().ok_or(String::from("Date literal must use `yyyy-mm-dd` format."))?;

		if parts.next().is_some() || year.len() != 4 || month.len() != 2 || day.len() != 2 {
			return Err(String::from("Date literal must use `yyyy-mm-dd` format."));
		}

		let year = year.parse::<i32>().map_err(|_| format!("Invalid date literal `{literal}`."))?;
		let month = month.parse::<u8>().map_err(|_| format!("Invalid date literal `{literal}`."))?;
		let day = day.parse::<u8>().map_err(|_| format!("Invalid date literal `{literal}`."))?;
		Self::from_parts(year, month, day).map_err(|_| format!("Invalid date literal `{literal}`."))
	}

	pub fn from_parts(year: i32, month: u8, day: u8) -> Result<Self, String> {
		if month == 0 || month > 12 {
			return Err(String::from("Month must be in the range 1..=12."));
		}

		let max_day = days_in_month(year, month);
		if day == 0 || day > max_day {
			return Err(format!("Day must be in the range 1..={max_day} for {year:04}-{month:02}."));
		}

		Ok(Self { day, month, year })
	}

	fn from_sqlite_text(value: &str) -> Result<Self, String> {
		let mut parts = value.split('-');
		let year = parts.next().ok_or_else(|| format!("Invalid SQLite date value `{value}`."))?;
		let month = parts.next().ok_or_else(|| format!("Invalid SQLite date value `{value}`."))?;
		let day = parts.next().ok_or_else(|| format!("Invalid SQLite date value `{value}`."))?;

		if parts.next().is_some() || year.len() != 4 || month.len() != 2 || day.len() != 2 {
			return Err(format!("Invalid SQLite date value `{value}`."));
		}

		let year = year.parse::<i32>().map_err(|_| format!("Invalid SQLite date value `{value}`."))?;
		let month = month.parse::<u8>().map_err(|_| format!("Invalid SQLite date value `{value}`."))?;
		let day = day.parse::<u8>().map_err(|_| format!("Invalid SQLite date value `{value}`."))?;
		Self::from_parts(year, month, day).map_err(|_| format!("Invalid SQLite date value `{value}`."))
	}
}

impl Display for Date {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:04}-{:02}-{:02}", self.year, self.month, self.day)
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Decimal {
	pub coefficient: i128,
	pub precision: u8,
	pub scale: u8,
}

impl Decimal {
	pub const MAX_PRECISION: u8 = 38;

	pub fn checked_add(&self, rhs: &Self) -> Result<Self, String> {
		let (lhs, rhs, scale) = align_decimal_operands(self, rhs)?;
		let coefficient = lhs.checked_add(rhs).ok_or(String::from("Decimal addition overflowed the supported precision."))?;
		Self::from_parts(coefficient, scale)
	}

	pub fn checked_div(&self, rhs: &Self) -> Result<Self, String> {
		if rhs.coefficient == 0 {
			return Err(String::from("Division by zero."));
		}

		let numerator = self.coefficient.checked_mul(pow10(rhs.scale as u32)?)
			.ok_or(String::from("Decimal division overflowed the supported precision."))?;
		let denominator = rhs.coefficient.checked_mul(pow10(self.scale as u32)?)
			.ok_or(String::from("Decimal division overflowed the supported precision."))?;

		let sign = if (numerator < 0) ^ (denominator < 0) { -1 } else { 1 };
		let numerator = numerator.abs();
		let denominator = denominator.abs();

		let whole_part = numerator / denominator;
		let whole_digits = whole_digits_required(whole_part);

		if whole_digits > Self::MAX_PRECISION as usize {
			return Err(String::from("Decimal division produced a result that exceeds the supported precision."));
		}

		let scale = Self::MAX_PRECISION as usize - whole_digits;
		let mut coefficient_digits = whole_part.to_string();
		let mut remainder = numerator % denominator;

		for _ in 0..scale {
			remainder = remainder.checked_mul(10)
				.ok_or(String::from("Decimal division overflowed the supported precision."))?;

			let digit = remainder / denominator;
			remainder %= denominator;
			coefficient_digits.push(char::from(b'0' + digit as u8));
		}

		let mut coefficient = coefficient_digits.parse::<i128>()
			.map_err(|_| String::from("Decimal division overflowed the supported precision."))?;

		if remainder.checked_mul(2).is_some_and(|double_remainder| double_remainder >= denominator) {
			coefficient = coefficient.checked_add(1)
				.ok_or(String::from("Decimal division overflowed the supported precision."))?;
		}

		if sign < 0 {
			coefficient = coefficient.checked_neg()
				.ok_or(String::from("Decimal division overflowed the supported precision."))?;
		}

		Self::from_parts(coefficient, scale as u8)
	}

	pub fn checked_mul(&self, rhs: &Self) -> Result<Self, String> {
		let coefficient = self.coefficient.checked_mul(rhs.coefficient).ok_or(String::from("Decimal multiplication overflowed the supported precision."))?;
		let scale = self.scale.checked_add(rhs.scale).ok_or(String::from("Decimal multiplication overflowed the supported precision."))?;
		Self::from_parts(coefficient, scale)
	}

	pub fn checked_rem(&self, rhs: &Self) -> Result<Self, String> {
		if rhs.coefficient == 0 {
			return Err(String::from("Modulo by zero."));
		}

		let (lhs, rhs, scale) = align_decimal_operands(self, rhs)?;
		let coefficient = lhs % rhs;
		Self::from_parts(coefficient, scale)
	}

	pub fn checked_sub(&self, rhs: &Self) -> Result<Self, String> {
		let (lhs, rhs, scale) = align_decimal_operands(self, rhs)?;
		let coefficient = lhs.checked_sub(rhs).ok_or(String::from("Decimal subtraction overflowed the supported precision."))?;
		Self::from_parts(coefficient, scale)
	}

	pub fn from_integer(value: i64) -> Self {
		Self::from_parts(value as i128, 0).unwrap()
	}

	pub fn from_integer_with_scale(value: i64, scale: u8) -> Result<Self, String> {
		let multiplier = pow10(scale as u32)?;
		let coefficient = (value as i128)
			.checked_mul(multiplier)
			.ok_or(String::from("Decimal conversion overflowed the supported precision."))?;

		Self::from_parts(coefficient, scale)
	}

	pub fn from_literal(literal: &str) -> Result<Self, String> {
		let (integer_part, fractional_part) = literal.split_once('.').ok_or(String::from("Invalid decimal literal."))?;

		if fractional_part.is_empty() {
			return Err(String::from("Decimal literals must have at least one digit after the decimal point."));
		}

		let integer_digits = normalize_integer_digits(integer_part);
		let fractional_digits = fractional_part.replace('_', "");
		let coefficient_digits = integer_digits.clone() + &fractional_digits;
		let coefficient = coefficient_digits.parse::<i128>().map_err(|_| format!("Decimal literal `{literal}` exceeds the supported precision."))?;
		let precision = integer_digits.len() + fractional_digits.len();
		let scale = fractional_digits.len();

		if precision > Self::MAX_PRECISION as usize {
			return Err(format!("Decimal literal `{literal}` exceeds the supported precision."));
		}

		Ok(Self {
			coefficient,
			precision: precision as u8,
			scale: scale as u8,
		})
	}

	pub fn negated(mut self) -> Self {
		self.coefficient = self.coefficient.saturating_neg();
		self
	}

	pub fn to_scale_with_precision(&self, precision: u8, scale: u8) -> Result<Self, String> {
		let whole_digits = precision.checked_sub(scale).ok_or(String::from("Invalid decimal precision and scale."))?;
		let current_whole_digits = self.whole_digits();

		if current_whole_digits > whole_digits as usize {
			return Err(String::from("Integer value cannot be converted to the required decimal precision and scale."));
		}

		if self.scale > scale {
			return Err(String::from("Integer value cannot be converted to the required decimal precision and scale."));
		}

		let multiplier = pow10((scale - self.scale) as u32)?;
		let coefficient = self.coefficient.checked_mul(multiplier).ok_or(String::from("Decimal conversion overflowed the supported precision."))?;

		Ok(Self {
			coefficient,
			precision,
			scale,
		})
	}

	fn from_parts(coefficient: i128, scale: u8) -> Result<Self, String> {
		let (coefficient, scale) = normalize_decimal_parts(coefficient, scale);
		let precision = decimal_precision(coefficient, scale)?;

		Ok(Self {
			coefficient,
			precision,
			scale,
		})
	}

	fn whole_digits(&self) -> usize {
		(self.precision as usize).saturating_sub(self.scale as usize)
	}
}

impl Display for Decimal {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let sign = if self.coefficient < 0 { "-" } else { "" };
		let digits = self.coefficient.abs().to_string();

		if self.scale == 0 {
			return write!(f, "{}{}", sign, digits);
		}

		let scale = self.scale as usize;

		if digits.len() <= scale {
			let zeros = "0".repeat(scale - digits.len());
			return write!(f, "{}0.{}{}", sign, zeros, digits);
		}

		let integer_len = digits.len() - scale;
		let integer_part = &digits[..integer_len];
		let fractional_part = &digits[integer_len..];

		write!(f, "{}{}.{}", sign, integer_part, fractional_part)
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DecimalRange {
	pub end: Decimal,
	pub start: Decimal,
	pub step: Option<Decimal>,
}

impl Display for DecimalRange {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:", self.start)?;

		if let Some(step) = &self.step {
			write!(f, "{}:", step)?;
		}

		write!(f, "{}", self.end)
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DecimalRangeIterator {
	pub end: Decimal,
	pub next_value: Option<Decimal>,
	pub step: Decimal,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumValue {
	pub backing_value: Box<Value>,
	pub enum_name: String,
	pub variant_name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntegerRange {
	pub end: i64,
	pub start: i64,
	pub step: Option<i64>,
}

impl Display for IntegerRange {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:", self.start)?;

		if let Some(step) = self.step {
			write!(f, "{}:", step)?;
		}

		write!(f, "{}", self.end)
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntegerRangeIterator {
	pub end: i64,
	pub next_value: Option<i64>,
	pub step: i64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LocalReference {
	pub frame_index: usize,
	pub slot: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecordPointerValue {
	pub exists: bool,
	pub fields: BTreeMap<String, RecordFieldValue>,
	pub locked: bool,
}

pub fn sqlite_record_field_runtime_value(
	value: &DeferredSqliteValue,
	data_type: &DataType,
	is_nullable: bool,
) -> Result<Value, String> {
	if is_nullable && matches!(value, DeferredSqliteValue::Null) {
		return Ok(Value::Null);
	}

	match data_type {
		DataType::Bool => match value {
			DeferredSqliteValue::Integer(value) => Ok(Value::Boolean(*value != 0)),
			other => Err(format!(
				"Cannot convert SQLite value `{}` to `bool`.",
				deferred_sqlite_value_name(other),
			)),
		},
		DataType::Date => match value {
			DeferredSqliteValue::Text(value) => Ok(Value::Date(Date::from_sqlite_text(value)?)),
			other => Err(format!(
				"Cannot convert SQLite value `{}` to `date`.",
				deferred_sqlite_value_name(other),
			)),
		},
		DataType::Dec => match value {
			DeferredSqliteValue::Integer(value) => Ok(Value::Decimal(Decimal::from_integer(*value))),
			DeferredSqliteValue::Real(value) => Ok(Value::Decimal(Decimal::from_literal(value)?)),
			DeferredSqliteValue::Text(value) => Ok(Value::Decimal(Decimal::from_literal(value)?)),
			other => Err(format!(
				"Cannot convert SQLite value `{}` to `dec`.",
				deferred_sqlite_value_name(other),
			)),
		},
		DataType::Int => match value {
			DeferredSqliteValue::Integer(value) => Ok(Value::Integer(*value)),
			other => Err(format!(
				"Cannot convert SQLite value `{}` to `int`.",
				deferred_sqlite_value_name(other),
			)),
		},
		DataType::Nullable(inner) => sqlite_record_field_runtime_value(value, inner, true),
		DataType::Text => match value {
			DeferredSqliteValue::Text(value) => Ok(Value::Text(value.clone())),
			other => Err(format!(
				"Cannot convert SQLite value `{}` to `text`.",
				deferred_sqlite_value_name(other),
			)),
		},
		other => Err(format!(
			"SQLite record pointer fields do not yet support runtime conversion to `{}`.",
			data_type_name_for_sqlite_runtime(other),
		)),
	}
}

fn align_decimal_operands(lhs: &Decimal, rhs: &Decimal) -> Result<(i128, i128, u8), String> {
	let scale = lhs.scale.max(rhs.scale);
	let lhs_factor = pow10((scale - lhs.scale) as u32)?;
	let rhs_factor = pow10((scale - rhs.scale) as u32)?;
	let lhs_coefficient = lhs.coefficient.checked_mul(lhs_factor).ok_or(String::from("Decimal alignment overflowed the supported precision."))?;
	let rhs_coefficient = rhs.coefficient.checked_mul(rhs_factor).ok_or(String::from("Decimal alignment overflowed the supported precision."))?;

	Ok((lhs_coefficient, rhs_coefficient, scale))
}

fn data_type_name_for_sqlite_runtime(data_type: &DataType) -> &'static str {
	match data_type {
		DataType::Any => "any",
		DataType::Array(_) => "array",
		DataType::Bool => "bool",
		DataType::Date => "date",
		DataType::Dec => "dec",
		DataType::EmptyArray => "empty array",
		DataType::Int => "int",
		DataType::Null => "null",
		DataType::Nullable(inner) => data_type_name_for_sqlite_runtime(inner),
		DataType::Object(_) => "object",
		DataType::Range(_) => "range",
		DataType::RecordPointer(_) => "record pointer",
		DataType::Text => "text",
		DataType::Union(_) => "union",
		DataType::Void => "void",
	}
}

fn days_in_month(year: i32, month: u8) -> u8 {
	match month {
		1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
		4 | 6 | 9 | 11 => 30,
		2 if is_leap_year(year) => 29,
		2 => 28,
		_ => 0,
	}
}

fn decimal_precision(coefficient: i128, scale: u8) -> Result<u8, String> {
	let digits = coefficient.abs().to_string().len();
	let precision = digits.max(scale as usize + 1);

	if precision > Decimal::MAX_PRECISION as usize {
		return Err(String::from("Decimal value exceeds the supported precision."));
	}

	Ok(precision as u8)
}

fn deferred_sqlite_value_name(value: &DeferredSqliteValue) -> &'static str {
	match value {
		DeferredSqliteValue::Blob(_) => "blob",
		DeferredSqliteValue::Integer(_) => "integer",
		DeferredSqliteValue::Null => "null",
		DeferredSqliteValue::Real(_) => "real",
		DeferredSqliteValue::Text(_) => "text",
	}
}

fn is_leap_year(year: i32) -> bool {
	(year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

fn normalize_decimal_parts(mut coefficient: i128, mut scale: u8) -> (i128, u8) {
	if coefficient == 0 {
		return (0, 0);
	}

	while scale > 0 && coefficient % 10 == 0 {
		coefficient /= 10;
		scale -= 1;
	}

	(coefficient, scale)
}

fn normalize_integer_digits(integer_part: &str) -> String {
	let digits = integer_part.replace('_', "");
	let trimmed = digits.trim_start_matches('0');

	if trimmed.is_empty() {
		String::from("0")
	}
	else {
		trimmed.to_string()
	}
}

fn pow10(exponent: u32) -> Result<i128, String> {
	let mut value = 1_i128;

	for _ in 0..exponent {
		value = value.checked_mul(10).ok_or(String::from("Decimal value exceeds the supported precision."))?;
	}

	Ok(value)
}

fn whole_digits_required(whole_part: i128) -> usize {
	if whole_part == 0 {
		1
	}
	else {
		whole_part.to_string().len()
	}
}

#[cfg(test)]
mod tests {
	use super::Decimal;

	#[test]
	fn accepts_38_digit_decimal_literal() {
		let decimal = Decimal::from_literal("3.1415926535897932384626433832795028841").unwrap();

		assert_eq!(decimal.precision, 38);
		assert_eq!(decimal.scale, 37);
		assert_eq!(decimal.to_string(), "3.1415926535897932384626433832795028841");
	}

	#[test]
	fn formats_decimal_with_scale() {
		let decimal = Decimal::from_literal("1.20").unwrap();

		assert_eq!(decimal.to_string(), "1.20");
	}

	#[test]
	fn negates_decimal() {
		let decimal = Decimal::from_literal("1.25").unwrap().negated();

		assert_eq!(decimal.to_string(), "-1.25");
	}

	#[test]
	fn divides_exact_values_without_trailing_zero_noise() {
		let lhs = Decimal::from_literal("1.0").unwrap();
		let rhs = Decimal::from_literal("2.0").unwrap();
		let result = lhs.checked_div(&rhs).unwrap();

		assert_eq!(result.to_string(), "0.5");
	}

	#[test]
	fn divides_with_rounding_when_result_repeats() {
		let lhs = Decimal::from_literal("2.0").unwrap();
		let rhs = Decimal::from_literal("3.0").unwrap();
		let result = lhs.checked_div(&rhs).unwrap();

		assert_eq!(result.to_string(), "0.6666666666666666666666666666666666667");
	}

	#[test]
	fn parses_decimal_literal_exactly() {
		let decimal = Decimal::from_literal(".50").unwrap();

		assert_eq!(decimal.coefficient, 50);
		assert_eq!(decimal.precision, 3);
		assert_eq!(decimal.scale, 2);
	}

	#[test]
	fn rejects_39_digit_decimal_literal() {
		let error = Decimal::from_literal("3.14159265358979323846264338327950288415").unwrap_err();

		assert_eq!(error, "Decimal literal `3.14159265358979323846264338327950288415` exceeds the supported precision.");
	}
}
