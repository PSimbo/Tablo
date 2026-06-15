use crate::value::Decimal;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum FractionMode {
	Automatic,
	Fixed(usize),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NumericFormatTarget {
	Decimal,
	Integer,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Radix {
	Decimal,
	HexLower,
	HexUpper,
	Octal,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NumericFormatError {
	pub message: String,
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NumericFormatPattern {
	decimal_point: Option<char>,
	fraction: Option<FractionMode>,
	group_separator: Option<char>,
	group_size: Option<usize>,
	left_padding: usize,
	min_whole_digits: usize,
	radix: Radix,
	right_padding: usize,
	show_sign: bool,
}

impl NumericFormatPattern {
	pub fn format_decimal(&self, value: &Decimal) -> Result<String, NumericFormatError> {
		let (sign, coefficient) = if value.coefficient < 0 {
			("-", value.coefficient.unsigned_abs())
		}
		else if self.show_sign {
			("+", value.coefficient as u128)
		}
		else {
			("", value.coefficient as u128)
		};

		let (whole_digits, fraction_digits) = match self.fraction {
			Some(FractionMode::Fixed(scale)) => split_rounded_decimal_parts(coefficient, value.scale as usize, scale),
			Some(FractionMode::Automatic) => split_trimmed_decimal_parts(coefficient, value.scale as usize),
			None => {
				return Err(NumericFormatError {
					message: String::from("Decimal format pattern is missing a fractional component."),
					position: 0,
				});
			}
		};

		let whole_digits = zero_pad_left(whole_digits, self.min_whole_digits);
		let grouped_whole = apply_grouping(&whole_digits, self.group_separator, self.group_size);

		let mut core = String::new();
		core.push_str(sign);
		core.push_str(&grouped_whole);

		match self.fraction {
			Some(FractionMode::Fixed(_)) => {
				core.push(self.decimal_point.unwrap_or('.'));
				core.push_str(&fraction_digits.unwrap_or_default());
			}
			Some(FractionMode::Automatic) => {
				if let Some(fraction_digits) = fraction_digits
					&& !fraction_digits.is_empty() {
					core.push(self.decimal_point.unwrap_or('.'));
					core.push_str(&fraction_digits);
				}
			}
			None => {}
		}

		for _ in 0..self.right_padding {
			core.push(' ');
		}

		let minimum_width = self.minimum_width();
		let padding_needed = minimum_width.saturating_sub(core.chars().count());
		if padding_needed == 0 {
			return Ok(core);
		}

		Ok(format!("{}{}", " ".repeat(padding_needed), core))
	}

	pub fn format_integer(&self, value: i64) -> String {
		let (sign, absolute) = if value < 0 {
			("-", value.unsigned_abs() as u128)
		}
		else if self.show_sign {
			("+", value as u128)
		}
		else {
			("", value as u128)
		};

		let radix = match self.radix {
			Radix::Decimal => 10,
			Radix::HexLower | Radix::HexUpper => 16,
			Radix::Octal => 8,
		};
		let mut digits = to_radix_string(absolute, radix);
		if self.radix == Radix::HexUpper {
			digits.make_ascii_uppercase();
		}
		let digits = zero_pad_left(digits, self.min_whole_digits);
		let grouped = apply_grouping(&digits, self.group_separator, self.group_size);

		let mut core = String::new();
		core.push_str(sign);
		core.push_str(&grouped);

		let minimum_width = self.minimum_width();
		let padding_needed = minimum_width.saturating_sub(core.chars().count());
		if padding_needed == 0 {
			return core;
		}

		format!("{}{}", " ".repeat(padding_needed), core)
	}

	fn minimum_width(&self) -> usize {
		self.left_padding
			+ self.min_whole_digits
			+ self.group_separator.map(|_| pattern_separator_count(self.min_whole_digits, self.group_size.unwrap_or(0))).unwrap_or(0)
			+ if self.show_sign { 1 } else { 0 }
			+ match self.fraction {
				Some(FractionMode::Fixed(scale)) => 1 + scale,
				Some(FractionMode::Automatic) => 0,
				None => 0,
			}
			+ self.right_padding
	}

	pub fn parse(source: &str, target: NumericFormatTarget) -> Result<Self, NumericFormatError> {
		if source.is_empty() {
			return Err(NumericFormatError {
				message: String::from("Numeric format string must not be empty."),
				position: 0,
			});
		}

		let chars: Vec<char> = source.chars().collect();
		let mut index = 0;
		let show_sign = if chars.first() == Some(&'+') {
			index += 1;
			true
		}
		else {
			false
		};

		let left_padding_start = index;
		while index < chars.len() && chars[index] == '_' {
			index += 1;
		}
		let left_padding = index - left_padding_start;

		let trailing_padding_start = chars.len().saturating_sub(chars.iter().rev().take_while(|ch| **ch == '_').count());
		let right_padding = chars.len().saturating_sub(trailing_padding_start);
		let body_end = trailing_padding_start;
		let body: String = chars[index..body_end].iter().collect();

		if body.is_empty() {
			return Err(NumericFormatError {
				message: String::from("Numeric format string must contain at least one whole-digit marker."),
				position: index,
			});
		}

		let (whole_pattern, decimal_point, fraction) = match target {
			NumericFormatTarget::Integer => {
				if body.contains('.') || body.contains(',') {
					let whole_parse = parse_whole_pattern(&body, index)?;
					if whole_parse.group_separator.is_none() {
						return Err(NumericFormatError {
							message: String::from("Integer numeric format strings must not contain a decimal point."),
							position: index,
						});
					}
				}

				(body, None, None)
			}
			NumericFormatTarget::Decimal => {
				let Some(relative_point_index) = body.char_indices().rev().find_map(|(offset, ch)| {
					if ch == '.' || ch == ',' {
						Some(offset)
					}
					else {
						None
					}
				}) else {
					return Err(NumericFormatError {
						message: String::from("Decimal numeric format strings must contain a decimal point."),
						position: body_end,
					});
				};

				let decimal_point = body[relative_point_index..].chars().next().unwrap();
				let whole_pattern = body[..relative_point_index].to_string();
				let fractional_pattern = &body[relative_point_index + decimal_point.len_utf8()..];

				if whole_pattern.is_empty() {
					return Err(NumericFormatError {
						message: String::from("Decimal numeric format strings must contain at least one whole-digit marker before the decimal point."),
						position: index,
					});
				}

				if fractional_pattern.chars().any(|ch| ch != '0') {
					return Err(NumericFormatError {
						message: String::from("Only `0` characters may appear after the decimal point in a decimal numeric format string."),
						position: index + relative_point_index + decimal_point.len_utf8(),
					});
				}

				let fraction = if fractional_pattern.is_empty() {
					FractionMode::Automatic
				}
				else {
					FractionMode::Fixed(fractional_pattern.chars().count())
				};

				(whole_pattern, Some(decimal_point), Some(fraction))
			}
		};

		let whole = parse_whole_pattern(&whole_pattern, index)?;

		if target == NumericFormatTarget::Decimal {
			if whole.radix != Radix::Decimal {
				return Err(NumericFormatError {
					message: String::from("Decimal numeric format strings must use `1` as the whole-digit marker."),
					position: index,
				});
			}

			if whole.group_separator == decimal_point {
				return Err(NumericFormatError {
					message: String::from("Decimal point character and thousands separator character must not be the same."),
					position: index + whole_pattern.len(),
				});
			}
		}

		Ok(Self {
			decimal_point,
			fraction,
			group_separator: whole.group_separator,
			group_size: whole.group_size,
			left_padding,
			min_whole_digits: whole.digit_count,
			radix: whole.radix,
			right_padding,
			show_sign,
		})
	}
}

struct WholePatternParse {
	digit_count: usize,
	group_separator: Option<char>,
	group_size: Option<usize>,
	radix: Radix,
}

fn apply_grouping(digits: &str, separator: Option<char>, group_size: Option<usize>) -> String {
	let Some(separator) = separator else {
		return digits.to_string();
	};
	let Some(group_size) = group_size else {
		return digits.to_string();
	};
	if group_size == 0 || digits.len() <= group_size {
		return digits.to_string();
	}

	let mut parts = Vec::new();
	let mut index = digits.len();
	while index > group_size {
		parts.push(digits[index - group_size..index].to_string());
		index -= group_size;
	}
	parts.push(digits[..index].to_string());
	parts.reverse();
	parts.join(&separator.to_string())
}

fn is_group_separator(ch: char) -> bool {
	matches!(ch, ',' | '.' | ' ' | '-')
}

fn pattern_separator_count(min_digits: usize, group_size: usize) -> usize {
	if group_size == 0 || min_digits <= group_size {
		0
	}
	else {
		(min_digits - 1) / group_size
	}
}

fn parse_whole_pattern(source: &str, pattern_start: usize) -> Result<WholePatternParse, NumericFormatError> {
	let mut separator_index = None;
	let mut separator_char = None;

	for (index, ch) in source.char_indices() {
		if is_group_separator(ch) {
			if separator_index.is_some() {
				return Err(NumericFormatError {
					message: String::from("Numeric format strings must not contain more than one thousands separator."),
					position: pattern_start + index,
				});
			}
			separator_index = Some(index);
			separator_char = Some(ch);
		}
	}

	let digits_only: String = source.chars().filter(|ch| !is_group_separator(*ch)).collect();
	if digits_only.is_empty() {
		return Err(NumericFormatError {
			message: String::from("Numeric format string must contain at least one whole-digit marker."),
			position: pattern_start,
		});
	}

	let radix = if digits_only.chars().all(|ch| ch == '1') {
		Radix::Decimal
	}
	else if digits_only.chars().all(|ch| ch == 'x') {
		Radix::HexLower
	}
	else if digits_only.chars().all(|ch| ch == 'X') {
		Radix::HexUpper
	}
	else if digits_only.chars().all(|ch| ch == 'o') {
		Radix::Octal
	}
	else {
		let mixed_index = source.char_indices()
			.find(|(_, ch)| !is_group_separator(*ch))
			.map(|(index, _)| index)
			.unwrap_or(0);
		return Err(NumericFormatError {
			message: String::from("Numeric format string must use only one whole-digit marker kind."),
			position: pattern_start + mixed_index,
		});
	};

	if let Some(index) = separator_index {
		if index == source.len() - 1 {
			return Err(NumericFormatError {
				message: String::from("Thousands separator must not appear at the end of the whole-part pattern."),
				position: pattern_start + index,
			});
		}
	}

	let group_size = separator_index.map(|index| {
		source[index + separator_char.unwrap().len_utf8()..]
			.chars()
			.filter(|ch| !is_group_separator(*ch))
			.count()
	});

	Ok(WholePatternParse {
		digit_count: digits_only.chars().count(),
		group_separator: separator_char,
		group_size,
		radix,
	})
}

fn split_rounded_decimal_parts(coefficient: u128, input_scale: usize, output_scale: usize) -> (String, Option<String>) {
	let adjusted = if input_scale > output_scale {
		let divisor = 10_u128.pow((input_scale - output_scale) as u32);
		let quotient = coefficient / divisor;
		let remainder = coefficient % divisor;
		if remainder.saturating_mul(2) >= divisor {
			quotient + 1
		}
		else {
			quotient
		}
	}
	else if input_scale < output_scale {
		coefficient * 10_u128.pow((output_scale - input_scale) as u32)
	}
	else {
		coefficient
	};

	split_scaled_integer(adjusted, output_scale)
}

fn split_scaled_integer(coefficient: u128, scale: usize) -> (String, Option<String>) {
	if scale == 0 {
		return (coefficient.to_string(), None);
	}

	let mut digits = coefficient.to_string();
	if digits.len() <= scale {
		let zeros = "0".repeat(scale + 1 - digits.len());
		digits = format!("{zeros}{digits}");
	}

	let split = digits.len() - scale;
	let whole = digits[..split].to_string();
	let fraction = digits[split..].to_string();
	(whole, Some(fraction))
}

fn split_trimmed_decimal_parts(mut coefficient: u128, mut scale: usize) -> (String, Option<String>) {
	while scale > 0 && coefficient % 10 == 0 {
		coefficient /= 10;
		scale -= 1;
	}

	split_scaled_integer(coefficient, scale)
}

fn to_radix_string(mut value: u128, radix: u32) -> String {
	if value == 0 {
		return String::from("0");
	}

	let alphabet = b"0123456789abcdef";
	let mut digits = Vec::new();
	while value > 0 {
		let digit = (value % radix as u128) as usize;
		digits.push(alphabet[digit] as char);
		value /= radix as u128;
	}
	digits.reverse();
	digits.into_iter().collect()
}

fn zero_pad_left(mut digits: String, min_digits: usize) -> String {
	while digits.len() < min_digits {
		digits.insert(0, '0');
	}
	digits
}

#[cfg(test)]
mod tests {
	use super::NumericFormatPattern;
	use super::NumericFormatTarget;
	use crate::value::Decimal;

	#[test]
	fn formats_automatic_decimal_without_decimal_point_when_not_needed() {
		let pattern = NumericFormatPattern::parse("1.", NumericFormatTarget::Decimal).unwrap();

		assert_eq!(pattern.format_decimal(&Decimal::from_integer(12)).unwrap(), "12");
	}

	#[test]
	fn formats_fixed_decimal() {
		let pattern = NumericFormatPattern::parse("1.00", NumericFormatTarget::Decimal).unwrap();

		assert_eq!(pattern.format_decimal(&Decimal::from_literal("12.3456").unwrap()).unwrap(), "12.35");
	}

	#[test]
	fn formats_grouped_integer() {
		let pattern = NumericFormatPattern::parse("1,111", NumericFormatTarget::Integer).unwrap();

		assert_eq!(pattern.format_integer(1234567), "1,234,567");
	}

	#[test]
	fn parses_integer_pattern() {
		let pattern = NumericFormatPattern::parse("+__11", NumericFormatTarget::Integer).unwrap();

		assert_eq!(pattern.format_integer(12), "  +12");
	}

	#[test]
	fn rejects_decimal_pattern_with_non_decimal_whole_digits() {
		let error = NumericFormatPattern::parse("x.00", NumericFormatTarget::Decimal).unwrap_err();

		assert_eq!(error.message, "Decimal numeric format strings must use `1` as the whole-digit marker.");
	}
}
