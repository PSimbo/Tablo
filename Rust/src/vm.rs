use crate::bytecode::Program;
use crate::bytecode::Instruction;
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
			Instruction::PushInteger(value) => {
				self.stack.push(Value::Integer(*value));
				Ok(())
			}
			Instruction::Add => {
				let rhs = self.pop_integer(instruction_index)?;
				let lhs = self.pop_integer(instruction_index)?;

				self.stack.push(Value::Integer(lhs.saturating_add(rhs)));
				Ok(())
			}
			Instruction::Subtract => {
				let rhs = self.pop_integer(instruction_index)?;
				let lhs = self.pop_integer(instruction_index)?;

				self.stack.push(Value::Integer(lhs.saturating_sub(rhs)));
				Ok(())
			}
			Instruction::Multiply => {
				let rhs = self.pop_integer(instruction_index)?;
				let lhs = self.pop_integer(instruction_index)?;

				self.stack.push(Value::Integer(lhs.saturating_mul(rhs)));
				Ok(())
			}
			Instruction::Divide => {
				let rhs = self.pop_integer(instruction_index)?;
				let lhs = self.pop_integer(instruction_index)?;

				if rhs == 0 {
					return Err(VmError {
						instruction_index,
						message: String::from("Division by zero."),
					});
				}

				self.stack.push(Value::Integer(lhs / rhs));
				Ok(())
			}
			Instruction::Modulo => {
				let rhs = self.pop_integer(instruction_index)?;
				let lhs = self.pop_integer(instruction_index)?;

				if rhs == 0 {
					return Err(VmError {
						instruction_index,
						message: String::from("Modulo by zero."),
					});
				}

				self.stack.push(Value::Integer(lhs % rhs));
				Ok(())
			}
		}
	}

	fn pop_integer(&mut self, instruction_index: usize) -> Result<i64, VmError> {
		let value = self.stack.pop().ok_or(VmError {
			instruction_index,
			message: String::from("Stack underflow while reading integer operand."),
		})?;

		match value {
			Value::Integer(integer) => Ok(integer),
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::bytecode::Instruction;
	use crate::bytecode::Program;
	use crate::value::Value;

	use super::VirtualMachine;
	use super::VmError;

	#[test]
	fn runs_integer_literal_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(42),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(42)));
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
	fn rejects_addition_without_enough_operands() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::Add,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 1,
			message: String::from("Stack underflow while reading integer operand."),
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
}
