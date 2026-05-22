use std::fmt::Display;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
	Decimal(Decimal),
	Integer(i64),
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Decimal(value) => write!(f, "{value}"),
			Value::Integer(value) => write!(f, "{value}"),
		}
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

		let mut numerator = self.coefficient;
		let denominator = rhs.coefficient;
		let mut scale = self.scale.saturating_sub(rhs.scale);

		while numerator % denominator != 0 {
			if scale >= Self::MAX_PRECISION {
				return Err(String::from("Decimal division produced a result that exceeds the supported precision."));
			}

			numerator = numerator.checked_mul(10).ok_or(String::from("Decimal division overflowed the supported precision."))?;
			scale += 1;
		}

		let coefficient = numerator / denominator;
		Self::from_parts(coefficient, scale)
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

fn align_decimal_operands(lhs: &Decimal, rhs: &Decimal) -> Result<(i128, i128, u8), String> {
	let scale = lhs.scale.max(rhs.scale);
	let lhs_factor = pow10((scale - lhs.scale) as u32)?;
	let rhs_factor = pow10((scale - rhs.scale) as u32)?;
	let lhs_coefficient = lhs.coefficient.checked_mul(lhs_factor).ok_or(String::from("Decimal alignment overflowed the supported precision."))?;
	let rhs_coefficient = rhs.coefficient.checked_mul(rhs_factor).ok_or(String::from("Decimal alignment overflowed the supported precision."))?;

	Ok((lhs_coefficient, rhs_coefficient, scale))
}

fn decimal_precision(coefficient: i128, scale: u8) -> Result<u8, String> {
	let digits = coefficient.abs().to_string().len();
	let precision = digits.max(scale as usize + 1);

	if precision > Decimal::MAX_PRECISION as usize {
		return Err(String::from("Decimal value exceeds the supported precision."));
	}

	Ok(precision as u8)
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
