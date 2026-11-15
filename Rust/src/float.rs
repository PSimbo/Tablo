//! Target-native IEEE 754 values. Structural equality compares bits; language
//! comparisons use IEEE arithmetic (including unordered NaNs and equal zeros).
#[cfg(target_pointer_width = "32")]
pub type NativeFloat = f32;
#[cfg(target_pointer_width = "64")]
pub type NativeFloat = f64;

#[derive(Clone, Copy, Debug, Default)]
pub struct Float(pub NativeFloat);

/// Retain source precision in artifacts; cache the native value on compilation/load.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FloatLiteral {
	pub(crate) text: String,
	pub(crate) value: Float,
}

impl FloatLiteral {
	pub fn parse(text: &str) -> Result<Self, String> {
		Ok(Self { text: text.into(), value: Float::parse(text)? })
	}
}

impl PartialEq for Float {
	fn eq(&self, other: &Self) -> bool { self.0.to_bits() == other.0.to_bits() }
}

impl Eq for Float {}

impl std::fmt::Display for Float {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { self.0.fmt(f) }
}

impl Float {
	pub fn parse(text: &str) -> Result<Self, String> {
		text.parse::<NativeFloat>().map(Self).map_err(|_| "Invalid floating-point text.".into())
	}

	pub fn from_integer(value: i64) -> Self {
		Self(value as NativeFloat)
	}

	pub fn to_integer(self) -> Result<i64, String> {
		let value = self.0.trunc();

		if !value.is_finite() || value < -9223372036854775808.0 || value >= 9223372036854775808.0 {
			return Err("Floating-point value is outside the integer range.".into());
		}

		Ok(value as i64)
	}

	/// Exact decimal expansion of the represented binary value, before rounding.
	pub fn exact_text(self) -> Result<String, String> {
		if !self.0.is_finite() {
			return Err("Non-finite floating-point value cannot be converted to a finite number.".into());
		}

		let bits = (self.0 as f64).to_bits();
		let exponent = ((bits >> 52) & 0x7ff) as i32;
		let fraction = bits & ((1u64 << 52) - 1);
		let (significand, power) = if exponent == 0 { (fraction, -1074) } else { (fraction | (1u64 << 52), exponent - 1023 - 52) };
		let mut coefficient = num_bigint::BigUint::from(significand);
		let scale = if power < 0 {
			coefficient *= num_bigint::BigUint::from(5u8).pow((-power) as u32);
			-power
		} else {
			coefficient <<= power as usize;
			0
		};

		Ok(format!("{}{}e-{}", if self.0.is_sign_negative() { "-" } else { "" }, coefficient, scale))
	}

	pub fn to_decimal(self) -> Result<crate::value::Decimal, String> {
		crate::numeric::ExactNumber::parse(&self.exact_text()?)?.to_decimal()
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::bytecode::{Instruction, Program};
	use crate::value::Value;

	#[test]
	fn target_width_and_ieee_boundaries() {
		assert_eq!(std::mem::size_of::<NativeFloat>(), std::mem::size_of::<usize>());
		let mantissa_bits = NativeFloat::MANTISSA_DIGITS;
		let boundary = (1u64 << mantissa_bits) as i64;
		assert_eq!(Float::from_integer(boundary), Float::from_integer(boundary + 1));
		assert_eq!(Float(NativeFloat::MAX * 2.0).0, NativeFloat::INFINITY);
		let smallest = NativeFloat::from_bits(1);
		assert!(smallest.is_subnormal());
		assert_eq!(smallest / 2.0, 0.0);
		assert!(((-smallest) / 2.0).is_sign_negative());

		for value in [0.0, -0.0, smallest, NativeFloat::MIN_POSITIVE, NativeFloat::MAX, NativeFloat::INFINITY, NativeFloat::NEG_INFINITY] {
			assert_eq!(Float::parse(&Float(value).to_string()).unwrap(), Float(value));
		}
	}

	#[test]
	fn decimal_conversion_uses_exact_binary_value_and_integer_conversion_checks_bounds() {
		assert_eq!(Float(0.5).to_decimal().unwrap(), crate::value::Decimal::from_literal("0.5").unwrap());
		assert_ne!(Float(0.1).to_decimal().unwrap(), crate::value::Decimal::from_literal("0.1").unwrap());
		assert_eq!(Float(-3.75).to_integer().unwrap(), -3);
		assert_eq!(Float(-9223372036854775808.0).to_integer().unwrap(), i64::MIN);
		assert!(Float(9223372036854775808.0).to_integer().is_err());

		for value in [NativeFloat::NAN, NativeFloat::INFINITY, NativeFloat::NEG_INFINITY] {
			assert!(Float(value).to_integer().is_err());
			assert!(Float(value).to_decimal().is_err());
		}
	}

	#[test]
	fn compiler_free_vm_and_artifacts_preserve_ieee_values() {
		for value in [0.0, -0.0, NativeFloat::MAX, NativeFloat::from_bits(1), NativeFloat::INFINITY, NativeFloat::NEG_INFINITY, NativeFloat::NAN] {
			let program = Program::new(vec![Instruction::PushFloat(Float(value))]);
			let restored = crate::object_file::read_program(&crate::object_file::write_program(&program)).unwrap();
			assert_eq!(program, restored);
			assert_eq!(crate::vm::VirtualMachine::new().run(&restored).unwrap(), Some(Value::Float(Float(value))));
		}

		let program = Program::new(vec![Instruction::PushFloat(Float(0.0)), Instruction::PushFloat(Float(0.0)), Instruction::Divide]);
		let Some(Value::Float(value)) = crate::vm::VirtualMachine::new().run(&program).unwrap() else {
			panic!("Expected float");
		};

		assert!(value.0.is_nan());
	}
}

/// Database storage width discovered by drivers; not part of language/schema types.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[doc(hidden)]
pub enum FloatWidth { Binary32, Binary64 }
