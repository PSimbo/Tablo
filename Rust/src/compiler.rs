use crate::ast::BinaryOperator;
use crate::ast::DecimalLiteral;
use crate::ast::Expr;
use crate::bytecode::Instruction;
use crate::bytecode::Program;

#[derive(Default)]
pub struct Compiler;

impl Compiler {
	pub fn new() -> Self {
		Self
	}

	pub fn compile_expression(&mut self, expression: &Expr) -> Program {
		let mut instructions = Vec::new();
		self.compile_into(expression, &mut instructions);
		Program::new(instructions)
	}

	fn compile_into(&mut self, expression: &Expr, instructions: &mut Vec<Instruction>) {
		let _ = self;

		match expression {
			Expr::Decimal(DecimalLiteral { value }) => {
				instructions.push(Instruction::PushDecimal(value.clone()));
			}
			Expr::Integer(integer) => {
				instructions.push(Instruction::PushInteger(integer.value));
			}
			Expr::Binary(binary) => {
				self.compile_into(&binary.left, instructions);
				self.compile_into(&binary.right, instructions);

				match binary.operator {
					BinaryOperator::Add => instructions.push(Instruction::Add),
					BinaryOperator::Divide => instructions.push(Instruction::Divide),
					BinaryOperator::Modulo => instructions.push(Instruction::Modulo),
					BinaryOperator::Multiply => instructions.push(Instruction::Multiply),
					BinaryOperator::Subtract => instructions.push(Instruction::Subtract),
				}
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::ast::BinaryExpr;
	use crate::ast::BinaryOperator;
	use crate::ast::DecimalLiteral;
	use crate::ast::Expr;
	use crate::ast::IntegerLiteral;
	use crate::bytecode::Instruction;
	use crate::value::Decimal;

	use super::Compiler;

	#[test]
	fn compiles_addition_in_post_order() {
		let expression = Expr::Binary(BinaryExpr {
			left: Box::new(Expr::Integer(IntegerLiteral {
				value: 1,
			})),
			operator: BinaryOperator::Add,
			right: Box::new(Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Integer(IntegerLiteral {
					value: 2,
				})),
				operator: BinaryOperator::Add,
				right: Box::new(Expr::Integer(IntegerLiteral {
					value: 3,
				})),
			})),
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::PushInteger(3),
			Instruction::Add,
			Instruction::Add,
		]);
	}

	#[test]
	fn compiles_decimal_literal() {
		let expression = Expr::Decimal(DecimalLiteral {
			value: Decimal::from_literal("1.25").unwrap(),
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushDecimal(Decimal::from_literal("1.25").unwrap()),
		]);
	}

	#[test]
	fn compiles_integer_literal() {
		let expression = Expr::Integer(IntegerLiteral {
			value: 42,
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushInteger(42),
		]);
	}

	#[test]
	fn compiles_mixed_arithmetic_in_post_order() {
		let expression = Expr::Binary(BinaryExpr {
			left: Box::new(Expr::Integer(IntegerLiteral {
				value: 9,
			})),
			operator: BinaryOperator::Subtract,
			right: Box::new(Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Integer(IntegerLiteral {
					value: 4,
				})),
				operator: BinaryOperator::Multiply,
				right: Box::new(Expr::Integer(IntegerLiteral {
					value: 2,
				})),
			})),
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushInteger(9),
			Instruction::PushInteger(4),
			Instruction::PushInteger(2),
			Instruction::Multiply,
			Instruction::Subtract,
		]);
	}
}
