use std::collections::BTreeMap;

use crate::ast::AssignmentExpr;
use crate::ast::AssignmentOperator;
use crate::ast::BinaryOperator;
use crate::ast::BooleanLiteral;
use crate::ast::DecimalLiteral;
use crate::ast::Expr;
use crate::ast::IdentifierExpr;
use crate::ast::Program as AstProgram;
use crate::ast::Statement;
use crate::ast::UnaryExpr;
use crate::ast::UnaryOperator;
use crate::ast::VariableDeclaration;
use crate::bytecode::Instruction;
use crate::bytecode::Program;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompileError {
	pub message: String,
}

#[derive(Default)]
pub struct Compiler {
	locals: BTreeMap<String, u32>,
	next_local_slot: u32,
}

impl Compiler {
	pub fn new() -> Self {
		Self {
			locals: BTreeMap::new(),
			next_local_slot: 0,
		}
	}

	pub fn compile_expression(&mut self, expression: &Expr) -> Program {
		let mut instructions = Vec::new();
		self.compile_into(expression, &mut instructions);
		Program::new(instructions)
	}

	fn compile_into(&mut self, expression: &Expr, instructions: &mut Vec<Instruction>) {
		let _ = self;

		match expression {
			Expr::Assignment(AssignmentExpr { operator, target, value }) => {
				let slot = self.locals.get(&target.name)
					.copied()
					.unwrap_or_else(|| panic!("Identifier `{}` must be declared before use.", target.name));

				match operator {
					AssignmentOperator::AddAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into(value, instructions);
						instructions.push(Instruction::Add);
					}
					AssignmentOperator::Assign => {
						self.compile_into(value, instructions);
					}
					AssignmentOperator::DivideAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into(value, instructions);
						instructions.push(Instruction::Divide);
					}
					AssignmentOperator::ModuloAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into(value, instructions);
						instructions.push(Instruction::Modulo);
					}
					AssignmentOperator::MultiplyAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into(value, instructions);
						instructions.push(Instruction::Multiply);
					}
					AssignmentOperator::SubtractAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into(value, instructions);
						instructions.push(Instruction::Subtract);
					}
				}

				instructions.push(Instruction::StoreLocal(slot));
				instructions.push(Instruction::LoadLocal(slot));
			}
			Expr::Binary(binary) => {
				self.compile_into(&binary.left, instructions);
				self.compile_into(&binary.right, instructions);

				match binary.operator {
					BinaryOperator::Add => instructions.push(Instruction::Add),
					BinaryOperator::And => instructions.push(Instruction::And),
					BinaryOperator::Divide => instructions.push(Instruction::Divide),
					BinaryOperator::Equal => instructions.push(Instruction::Equal),
					BinaryOperator::GreaterThan => instructions.push(Instruction::GreaterThan),
					BinaryOperator::GreaterThanOrEqual => instructions.push(Instruction::GreaterThanOrEqual),
					BinaryOperator::LessThan => instructions.push(Instruction::LessThan),
					BinaryOperator::LessThanOrEqual => instructions.push(Instruction::LessThanOrEqual),
					BinaryOperator::Modulo => instructions.push(Instruction::Modulo),
					BinaryOperator::Multiply => instructions.push(Instruction::Multiply),
					BinaryOperator::NotEqual => instructions.push(Instruction::NotEqual),
					BinaryOperator::Or => instructions.push(Instruction::Or),
					BinaryOperator::Subtract => instructions.push(Instruction::Subtract),
					BinaryOperator::Xor => instructions.push(Instruction::Xor),
				}
			}
			Expr::Boolean(BooleanLiteral { value }) => {
				instructions.push(Instruction::PushBoolean(*value));
			}
			Expr::Decimal(DecimalLiteral { value }) => {
				instructions.push(Instruction::PushDecimal(value.clone()));
			}
			Expr::Identifier(IdentifierExpr { name }) => {
				let slot = self.locals.get(name)
					.copied()
					.unwrap_or_else(|| panic!("Identifier `{name}` must be declared before use."));
				instructions.push(Instruction::LoadLocal(slot));
			}
			Expr::Integer(integer) => {
				instructions.push(Instruction::PushInteger(integer.value));
			}
			Expr::Unary(UnaryExpr { operand, operator }) => {
				self.compile_into(operand, instructions);

				match operator {
					UnaryOperator::Negate => instructions.push(Instruction::Negate),
					UnaryOperator::Not => instructions.push(Instruction::Not),
				}
			}
		}
	}

	fn compile_into_checked(&mut self, expression: &Expr, instructions: &mut Vec<Instruction>) -> Result<(), CompileError> {
		match expression {
			Expr::Assignment(AssignmentExpr { operator, target, value }) => {
				let slot = self.locals.get(&target.name).copied().ok_or(CompileError {
					message: format!("Variable `{}` is not declared in this scope.", target.name),
				})?;

				match operator {
					AssignmentOperator::AddAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into_checked(value, instructions)?;
						instructions.push(Instruction::Add);
					}
					AssignmentOperator::Assign => {
						self.compile_into_checked(value, instructions)?;
					}
					AssignmentOperator::DivideAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into_checked(value, instructions)?;
						instructions.push(Instruction::Divide);
					}
					AssignmentOperator::ModuloAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into_checked(value, instructions)?;
						instructions.push(Instruction::Modulo);
					}
					AssignmentOperator::MultiplyAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into_checked(value, instructions)?;
						instructions.push(Instruction::Multiply);
					}
					AssignmentOperator::SubtractAssign => {
						instructions.push(Instruction::LoadLocal(slot));
						self.compile_into_checked(value, instructions)?;
						instructions.push(Instruction::Subtract);
					}
				}

				instructions.push(Instruction::StoreLocal(slot));
				instructions.push(Instruction::LoadLocal(slot));
				Ok(())
			}
			Expr::Identifier(IdentifierExpr { name }) => {
				let slot = self.locals.get(name).copied().ok_or(CompileError {
					message: format!("Variable `{name}` is not declared in this scope."),
				})?;
				instructions.push(Instruction::LoadLocal(slot));
				Ok(())
			}
			Expr::Binary(binary) => {
				self.compile_into_checked(&binary.left, instructions)?;
				self.compile_into_checked(&binary.right, instructions)?;

				match binary.operator {
					BinaryOperator::Add => instructions.push(Instruction::Add),
					BinaryOperator::And => instructions.push(Instruction::And),
					BinaryOperator::Divide => instructions.push(Instruction::Divide),
					BinaryOperator::Equal => instructions.push(Instruction::Equal),
					BinaryOperator::GreaterThan => instructions.push(Instruction::GreaterThan),
					BinaryOperator::GreaterThanOrEqual => instructions.push(Instruction::GreaterThanOrEqual),
					BinaryOperator::LessThan => instructions.push(Instruction::LessThan),
					BinaryOperator::LessThanOrEqual => instructions.push(Instruction::LessThanOrEqual),
					BinaryOperator::Modulo => instructions.push(Instruction::Modulo),
					BinaryOperator::Multiply => instructions.push(Instruction::Multiply),
					BinaryOperator::NotEqual => instructions.push(Instruction::NotEqual),
					BinaryOperator::Or => instructions.push(Instruction::Or),
					BinaryOperator::Subtract => instructions.push(Instruction::Subtract),
					BinaryOperator::Xor => instructions.push(Instruction::Xor),
				}

				Ok(())
			}
			Expr::Unary(UnaryExpr { operand, operator }) => {
				self.compile_into_checked(operand, instructions)?;

				match operator {
					UnaryOperator::Negate => instructions.push(Instruction::Negate),
					UnaryOperator::Not => instructions.push(Instruction::Not),
				}

				Ok(())
			}
			_ => {
				self.compile_into(expression, instructions);
				Ok(())
			}
		}
	}

	pub fn compile_program(&mut self, program: &AstProgram) -> Result<Program, CompileError> {
		self.locals.clear();
		self.next_local_slot = 0;

		let mut instructions = Vec::new();

		for statement in &program.statements {
			self.compile_statement(statement, &mut instructions)?;
		}

		if let Some(result) = &program.result {
			self.compile_into_checked(result, &mut instructions)?;
		}

		Ok(Program::new(instructions))
	}

	fn compile_statement(&mut self, statement: &Statement, instructions: &mut Vec<Instruction>) -> Result<(), CompileError> {
		match statement {
			Statement::Expression(expression) => {
				self.compile_into_checked(expression, instructions)?;
				instructions.push(Instruction::Pop);
				Ok(())
			}
			Statement::VariableDeclaration(VariableDeclaration { initial_value, name, .. }) => {
				if self.locals.contains_key(name) {
					return Err(CompileError {
						message: format!("Variable `{name}` is already declared in this scope."),
					});
				}

				let slot = self.next_local_slot;
				self.next_local_slot += 1;

				let initial_value = initial_value.as_ref().ok_or(CompileError {
					message: format!("Variable `{name}` must currently have an initializer."),
				})?;
				self.compile_into_checked(initial_value, instructions)?;
				instructions.push(Instruction::StoreLocal(slot));
				self.locals.insert(name.clone(), slot);

				Ok(())
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::ast::AssignmentExpr;
	use crate::ast::AssignmentOperator;
	use crate::ast::BinaryExpr;
	use crate::ast::BinaryOperator;
	use crate::ast::BooleanLiteral;
	use crate::ast::DataType;
	use crate::ast::DecimalLiteral;
	use crate::ast::Expr;
	use crate::ast::IdentifierExpr;
	use crate::ast::IntegerLiteral;
	use crate::ast::Program as AstProgram;
	use crate::ast::Statement;
	use crate::ast::UnaryExpr;
	use crate::ast::UnaryOperator;
	use crate::ast::VariableDeclaration;
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
	fn compiles_boolean_literal() {
		let expression = Expr::Boolean(BooleanLiteral {
			value: true,
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushBoolean(true),
		]);
	}

	#[test]
	fn compiles_compound_assignment_expression() {
		let program = AstProgram {
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Integer(IntegerLiteral {
						value: 5,
					})),
					name: String::from("x"),
				}),
			],
			result: Some(Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::AddAssign,
				target: IdentifierExpr {
					name: String::from("x"),
				},
				value: Box::new(Expr::Integer(IntegerLiteral {
					value: 3,
				})),
			})),
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.instructions, vec![
			Instruction::PushInteger(5),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::PushInteger(3),
			Instruction::Add,
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
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
	fn compiles_equality_expression() {
		let expression = Expr::Binary(BinaryExpr {
			left: Box::new(Expr::Boolean(BooleanLiteral {
				value: true,
			})),
			operator: BinaryOperator::Equal,
			right: Box::new(Expr::Boolean(BooleanLiteral {
				value: false,
			})),
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushBoolean(true),
			Instruction::PushBoolean(false),
			Instruction::Equal,
		]);
	}

	#[test]
	fn compiles_expression_statement_to_pop_its_result() {
		let program = AstProgram {
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Integer(IntegerLiteral {
						value: 5,
					})),
					name: String::from("x"),
				}),
				Statement::Expression(Expr::Assignment(AssignmentExpr {
					operator: AssignmentOperator::AddAssign,
					target: IdentifierExpr {
						name: String::from("x"),
					},
					value: Box::new(Expr::Integer(IntegerLiteral {
						value: 3,
					})),
				})),
			],
			result: Some(Expr::Identifier(IdentifierExpr {
				name: String::from("x"),
			})),
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.instructions, vec![
			Instruction::PushInteger(5),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::PushInteger(3),
			Instruction::Add,
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::Pop,
			Instruction::LoadLocal(0),
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
	fn compiles_logical_not() {
		let expression = Expr::Unary(UnaryExpr {
			operand: Box::new(Expr::Boolean(BooleanLiteral {
				value: false,
			})),
			operator: UnaryOperator::Not,
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushBoolean(false),
			Instruction::Not,
		]);
	}

	#[test]
	fn compiles_logical_xor() {
		let expression = Expr::Binary(BinaryExpr {
			left: Box::new(Expr::Boolean(BooleanLiteral {
				value: true,
			})),
			operator: BinaryOperator::Xor,
			right: Box::new(Expr::Boolean(BooleanLiteral {
				value: false,
			})),
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushBoolean(true),
			Instruction::PushBoolean(false),
			Instruction::Xor,
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

	#[test]
	fn compiles_program_with_variable_declarations() {
		let program = AstProgram {
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Integer(IntegerLiteral {
						value: 1,
					})),
					name: String::from("x"),
				}),
			],
			result: Some(Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Identifier(IdentifierExpr {
					name: String::from("x"),
				})),
				operator: BinaryOperator::Add,
				right: Box::new(Expr::Integer(IntegerLiteral {
					value: 2,
				})),
			})),
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.instructions, vec![
			Instruction::PushInteger(1),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::PushInteger(2),
			Instruction::Add,
		]);
	}

	#[test]
	fn compiles_unary_negation() {
		let expression = Expr::Unary(UnaryExpr {
			operand: Box::new(Expr::Integer(IntegerLiteral {
				value: 42,
			})),
			operator: UnaryOperator::Negate,
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushInteger(42),
			Instruction::Negate,
		]);
	}
}
