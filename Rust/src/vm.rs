use crate::bytecode::Program;
use crate::bytecode::Instruction;
use crate::value::Decimal;
use crate::value::Value;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VmError {
	pub instruction_index: usize,
	pub message: String,
}

#[derive(Default)]
pub struct VirtualMachine {
	stack: Vec<Value>,
}

impl VirtualMachine {
	pub fn new() -> Self {
		Self {
			stack: Vec::new(),
		}
	}

	pub fn run(&mut self, program: &Program) -> Result<Option<Value>, VmError> {
		self.stack.clear();

		for (instruction_index, instruction) in program.instructions.iter().enumerate() {
			self.execute_instruction(instruction, instruction_index)?;
		}

		Ok(self.stack.pop())
	}

	fn execute_instruction(&mut self, instruction: &Instruction, instruction_index: usize) -> Result<(), VmError> {
		match instruction {
			Instruction::Add => {
				let rhs = self.pop_numeric(instruction_index)?;
				let lhs = self.pop_numeric(instruction_index)?;
				self.stack.push(add_values(lhs, rhs, instruction_index)?);
				Ok(())
			}
			Instruction::Divide => {
				let rhs = self.pop_numeric(instruction_index)?;
				let lhs = self.pop_numeric(instruction_index)?;
				self.stack.push(divide_values(lhs, rhs, instruction_index)?);
				Ok(())
			}
			Instruction::Modulo => {
				let rhs = self.pop_numeric(instruction_index)?;
				let lhs = self.pop_numeric(instruction_index)?;
				self.stack.push(modulo_values(lhs, rhs, instruction_index)?);
				Ok(())
			}
			Instruction::Multiply => {
				let rhs = self.pop_numeric(instruction_index)?;
				let lhs = self.pop_numeric(instruction_index)?;
				self.stack.push(multiply_values(lhs, rhs, instruction_index)?);
				Ok(())
			}
			Instruction::Negate => {
				let value = self.pop_numeric(instruction_index)?;
				self.stack.push(negate_value(value));
				Ok(())
			}
			Instruction::PushBoolean(value) => {
				self.stack.push(Value::Boolean(*value));
				Ok(())
			}
			Instruction::PushDecimal(value) => {
				self.stack.push(Value::Decimal(value.clone()));
				Ok(())
			}
			Instruction::PushInteger(value) => {
				self.stack.push(Value::Integer(*value));
				Ok(())
			}
			Instruction::Subtract => {
				let rhs = self.pop_numeric(instruction_index)?;
				let lhs = self.pop_numeric(instruction_index)?;
				self.stack.push(subtract_values(lhs, rhs, instruction_index)?);
				Ok(())
			}
		}
	}

	fn pop_numeric(&mut self, instruction_index: usize) -> Result<Value, VmError> {
		let value = self.stack.pop().ok_or(VmError {
			instruction_index,
			message: String::from("Stack underflow while reading numeric operand."),
		})?;

		if matches!(value, Value::Boolean(_)) {
			return Err(VmError {
				instruction_index,
				message: String::from("Expected a numeric operand, found a Boolean value."),
			});
		}

		Ok(value)
	}
}

fn add_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs.saturating_add(rhs))),
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_add(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn coerce_numeric_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<(Decimal, Decimal), VmError> {
	match (lhs, rhs) {
		(Value::Decimal(lhs), Value::Decimal(rhs)) => Ok((lhs, rhs)),
		(Value::Decimal(lhs), Value::Integer(rhs)) => {
			let rhs = Decimal::from_integer(rhs)
				.to_scale_with_precision(lhs.precision, lhs.scale)
				.map_err(|message| vm_error(instruction_index, message))?;

			Ok((lhs, rhs))
		}
		(Value::Integer(lhs), Value::Decimal(rhs)) => {
			let lhs = Decimal::from_integer(lhs)
				.to_scale_with_precision(rhs.precision, rhs.scale)
				.map_err(|message| vm_error(instruction_index, message))?;

			Ok((lhs, rhs))
		}
		(Value::Integer(lhs), Value::Integer(rhs)) => Ok((Decimal::from_integer(lhs), Decimal::from_integer(rhs))),
		_ => Err(vm_error(instruction_index, String::from("Expected numeric operands."))),
	}
}

fn divide_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Integer(lhs), Value::Integer(rhs)) => {
			if rhs == 0 {
				return Err(vm_error(instruction_index, String::from("Division by zero.")));
			}

			Ok(Value::Integer(lhs / rhs))
		}
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_div(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn modulo_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Integer(lhs), Value::Integer(rhs)) => {
			if rhs == 0 {
				return Err(vm_error(instruction_index, String::from("Modulo by zero.")));
			}

			Ok(Value::Integer(lhs % rhs))
		}
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_rem(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn multiply_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs.saturating_mul(rhs))),
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_mul(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn negate_value(value: Value) -> Value {
	match value {
		Value::Boolean(_) => unreachable!("Boolean values are rejected before numeric negation."),
		Value::Decimal(mut decimal) => {
			decimal.coefficient = decimal.coefficient.saturating_neg();
			Value::Decimal(decimal)
		}
		Value::Integer(integer) => Value::Integer(integer.saturating_neg()),
	}
}

fn subtract_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs.saturating_sub(rhs))),
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_sub(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn vm_error(instruction_index: usize, message: String) -> VmError {
	VmError {
		instruction_index,
		message,
	}
}

#[cfg(test)]
mod tests {
	use crate::bytecode::Instruction;
	use crate::bytecode::Program;
	use crate::value::Decimal;
	use crate::value::Value;

	use super::VirtualMachine;
	use super::VmError;

	#[test]
	fn rejects_addition_without_enough_operands() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::Add,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 1,
			message: String::from("Stack underflow while reading numeric operand."),
		});
	}

	#[test]
	fn rejects_division_by_zero() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(0),
			Instruction::Divide,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 2,
			message: String::from("Division by zero."),
		});
	}

	#[test]
	fn rejects_modulo_by_zero() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(0),
			Instruction::Modulo,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 2,
			message: String::from("Modulo by zero."),
		});
	}

	#[test]
	fn runs_addition_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::Add,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_boolean_literal_program() {
		let program = Program::new(vec![
			Instruction::PushBoolean(true),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_decimal_literal_program() {
		let program = Program::new(vec![
			Instruction::PushDecimal(Decimal::from_literal("1.25").unwrap()),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Decimal(Decimal::from_literal("1.25").unwrap())));
	}

	#[test]
	fn runs_integer_literal_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(42),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(42)));
	}

	#[test]
	fn runs_mixed_integer_and_decimal_addition() {
		let program = Program::new(vec![
			Instruction::PushInteger(2),
			Instruction::PushDecimal(Decimal::from_literal("1.25").unwrap()),
			Instruction::Add,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Decimal(Decimal::from_literal("3.25").unwrap())));
	}

	#[test]
	fn runs_other_arithmetic_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(20),
			Instruction::PushInteger(5),
			Instruction::Divide,
			Instruction::PushInteger(3),
			Instruction::Multiply,
			Instruction::PushInteger(1),
			Instruction::Subtract,
			Instruction::PushInteger(4),
			Instruction::Modulo,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_unary_negation_for_decimal() {
		let program = Program::new(vec![
			Instruction::PushDecimal(Decimal::from_literal("1.25").unwrap()),
			Instruction::Negate,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Decimal(Decimal::from_literal("1.25").unwrap().negated())));
	}

	#[test]
	fn runs_unary_negation_for_integer() {
		let program = Program::new(vec![
			Instruction::PushInteger(42),
			Instruction::Negate,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(-42)));
	}
}
