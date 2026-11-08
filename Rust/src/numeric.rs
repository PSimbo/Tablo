use num_bigint::BigInt;

use crate::value::Decimal;

#[derive(Clone, Debug)]
pub(crate) struct ExactNumber {
	pub(crate) text: String,
	pub(crate) digits: String,
	pub(crate) exponent: BigInt,
	pub(crate) negative: bool,
}

impl ExactNumber {
	pub(crate) fn new(text: &str) -> Self {
		let (mantissa, exponent) = text.split_once(['e', 'E']).unwrap_or((text, "0"));
		let mut exponent: BigInt = exponent.parse().expect("Numeric syntax was validated");
		let negative = mantissa.starts_with('-');
		let mantissa = mantissa.trim_start_matches('-');
		let (whole, fraction) = mantissa.split_once('.').unwrap_or((mantissa, ""));
		let digits = format!("{whole}{fraction}");
		let digits = digits.trim_start_matches('0');
		let significant = digits.trim_end_matches('0');

		exponent += BigInt::from(digits.len() - significant.len());
		exponent -= BigInt::from(fraction.len());

		Self {
			text: text.to_string(), digits: significant.to_string(),
			exponent: if significant.is_empty() { BigInt::from(0) } else { exponent },
			negative: negative && !significant.is_empty(),
		}
	}
}

impl PartialEq for ExactNumber {
	fn eq(&self, other: &Self) -> bool {
		self.negative == other.negative && self.digits == other.digits && self.exponent == other.exponent
	}
}

impl Eq for ExactNumber {}

impl ExactNumber {
	pub(crate) fn parse(text: &str) -> Result<Self, String> {
		let invalid = || String::from("Invalid numeric text for `dec` conversion.");
		let unsigned = text.strip_prefix(['+', '-']).unwrap_or(text);
		let (mantissa, exponent) = unsigned.split_once(['e', 'E']).unwrap_or((unsigned, "0"));
		let exponent_digits = exponent.strip_prefix(['+', '-']).unwrap_or(exponent);
		let (whole, fraction) = mantissa.split_once('.').unwrap_or((mantissa, ""));

		if (whole.is_empty() && fraction.is_empty()) || exponent_digits.is_empty()
			|| !whole.bytes().all(|byte| byte.is_ascii_digit())
			|| !fraction.bytes().all(|byte| byte.is_ascii_digit())
			|| !exponent_digits.bytes().all(|byte| byte.is_ascii_digit()) {
			return Err(invalid());
		}

		Ok(Self::new(text.strip_prefix('+').unwrap_or(text)))
	}

	pub(crate) fn to_decimal(&self) -> Result<Decimal, String> {
		if self.digits.is_empty() {
			return Ok(Decimal::from_integer(0));
		}

		let overflow = || String::from("Number exceeds the supported decimal range.");
		let capacity = Decimal::MAX_PRECISION as usize;
		let point = BigInt::from(self.digits.len()) + &self.exponent;

		if point > BigInt::from(capacity) {
			return Err(overflow());
		}

		if point == BigInt::from(capacity) && self.digits.len() > capacity
			&& self.digits.as_bytes()[..capacity].iter().all(|digit| *digit == b'9') {
			return Err(overflow());
		}

		// The decimal representation reserves at least one whole digit. Bound
		// the retained coefficient before expanding an arbitrary JSON exponent.
		let whole_digits = if point > BigInt::from(1) { usize::try_from(&point).map_err(|_| overflow())? } else { 1 };
		let scale = capacity - whole_digits;
		let retained = point + BigInt::from(scale);

		if retained < BigInt::from(0) {
			return Ok(Decimal::from_integer(0));
		}

		let retained = usize::try_from(retained).map_err(|_| overflow())?;
		let mut coefficient = if retained == 0 { 0 } else {
			let prefix = &self.digits[..retained.min(self.digits.len())];
			let coefficient = prefix.parse::<i128>().map_err(|_| overflow())?;
			coefficient * 10_i128.pow(retained.saturating_sub(self.digits.len()) as u32)
		};

		if self.digits.as_bytes().get(retained).is_some_and(|digit| *digit >= b'5') {
			coefficient += 1;
		}

		if self.negative {
			coefficient = -coefficient;
		}

		Decimal::from_database_text(&format!("{coefficient}e-{scale}")).map_err(|_| overflow())
	}
}
