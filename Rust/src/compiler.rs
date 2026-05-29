// This module is now primarily responsible for bytecode emission from a
// semantically validated AST. It still owns local-slot allocation, but most
// name resolution and type checking now live in `semantic::analyzer`.

use crate::ast::AssignmentExpr;
use crate::ast::AssignmentOperator;
use crate::ast::BinaryOperator;
use crate::ast::BlockStatement;
use crate::ast::BooleanLiteral;
use crate::ast::DecimalLiteral;
use crate::ast::Expr;
use crate::ast::IdentifierExpr;
use crate::ast::IfStatement;
use crate::ast::Program as AstProgram;
use crate::ast::Statement;
use crate::ast::TextLiteral;
use crate::ast::UnaryExpr;
use crate::ast::UnaryOperator;
use crate::ast::VariableDeclaration;
use crate::bytecode::Instruction;
use crate::bytecode::Program;
use crate::semantic::analyzer::SemanticAnalyzer;
use crate::semantic::scope::ScopeStack;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompileError {
	pub message: String,
	pub position: usize,
}

#[derive(Default)]
pub struct Compiler {
	locals: ScopeStack<Local>,
	next_local_slot: u32,
}

#[derive(Clone, Copy)]
struct Local {
	slot: u32,
}

impl Compiler {
	pub fn compile_expression(&mut self, expression: &Expr) -> Program {
		let mut instructions = Vec::new();
		self.compile_into(expression, &mut instructions);
		Program::new(instructions)
	}

	pub fn compile_program(&mut self, program: &AstProgram) -> Result<Program, CompileError> {
		SemanticAnalyzer::new().validate_program(program)?;

		self.locals = ScopeStack::default();
		self.next_local_slot = 0;

		let mut instructions = Vec::new();
		self.enter_scope();

		for statement in &program.statements {
			self.compile_statement(statement, &mut instructions)?;
		}

		if let Some(result) = &program.result {
			self.compile_into(result, &mut instructions);
		}

		self.exit_scope();
		Ok(Program::new(instructions))
	}

	pub fn new() -> Self {
		Self {
			locals: ScopeStack::default(),
			next_local_slot: 0,
		}
	}

	fn compile_error(&self, position: usize, message: impl Into<String>) -> CompileError {
		CompileError {
			message: message.into(),
			position,
		}
	}

	fn compile_into(&mut self, expression: &Expr, instructions: &mut Vec<Instruction>) {
		let _ = self;

		match expression {
			Expr::Assignment(AssignmentExpr { operator, target, value, .. }) => {
				let slot = self.lookup_local(&target.name)
					.map(|local| local.slot)
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
			Expr::Boolean(BooleanLiteral { value, .. }) => {
				instructions.push(Instruction::PushBoolean(*value));
			}
			Expr::Decimal(DecimalLiteral { value, .. }) => {
				instructions.push(Instruction::PushDecimal(value.clone()));
			}
			Expr::Identifier(IdentifierExpr { name, .. }) => {
				let slot = self.lookup_local(name)
					.map(|local| local.slot)
					.unwrap_or_else(|| panic!("Identifier `{name}` must be declared before use."));
				instructions.push(Instruction::LoadLocal(slot));
			}
			Expr::Integer(integer) => {
				instructions.push(Instruction::PushInteger(integer.value));
			}
			Expr::Text(TextLiteral { value, .. }) => {
				instructions.push(Instruction::PushText(value.clone()));
			}
			Expr::Unary(UnaryExpr { operand, operator, .. }) => {
				self.compile_into(operand, instructions);

				match operator {
					UnaryOperator::Negate => instructions.push(Instruction::Negate),
					UnaryOperator::Not => instructions.push(Instruction::Not),
				}
			}
		}
	}

	fn compile_statement(&mut self, statement: &Statement, instructions: &mut Vec<Instruction>) -> Result<(), CompileError> {
		match statement {
			Statement::Block(BlockStatement { statements, .. }) => {
				self.enter_scope();

				for statement in statements {
					self.compile_statement(statement, instructions)?;
				}

				self.exit_scope();
				Ok(())
			}
			Statement::Expression(expression) => {
				self.compile_into(expression, instructions);
				instructions.push(Instruction::Pop);
				Ok(())
			}
			Statement::If(IfStatement {
				condition,
				else_branch,
				then_branch,
				..
			}) => {
				self.compile_into(condition, instructions);
				let jump_if_false_index = instructions.len();
				instructions.push(Instruction::JumpIfFalse(0));

				self.compile_statement(&Statement::Block(then_branch.clone()), instructions)?;

				if let Some(else_branch) = else_branch {
					let jump_to_end_index = instructions.len();
					instructions.push(Instruction::Jump(0));

					let else_target = instructions.len() as u32;
					instructions[jump_if_false_index] = Instruction::JumpIfFalse(else_target);
					self.compile_statement(else_branch, instructions)?;

					let end_target = instructions.len() as u32;
					instructions[jump_to_end_index] = Instruction::Jump(end_target);
				}
				else {
					let end_target = instructions.len() as u32;
					instructions[jump_if_false_index] = Instruction::JumpIfFalse(end_target);
				}

				Ok(())
			}
			Statement::VariableDeclaration(VariableDeclaration { data_type: _, initial_value, is_const, name, position }) => {
				let slot = self.next_local_slot;
				self.next_local_slot += 1;

				let initial_value = initial_value.as_ref().ok_or(self.compile_error(
					*position,
					if *is_const {
						format!("Constant `{name}` must currently have an initializer.")
					}
					else {
						format!("Variable `{name}` must currently have an initializer.")
					},
				))?;
				self.compile_into(initial_value, instructions);
				instructions.push(Instruction::StoreLocal(slot));
				self.declare_local(name.clone(), Local {
					slot,
				});

				Ok(())
			}
		}
	}

	fn declare_local(&mut self, name: String, local: Local) {
		self.locals.declare(name, local);
	}

	fn enter_scope(&mut self) {
		self.locals.enter_scope();
	}

	fn exit_scope(&mut self) {
		self.locals.exit_scope();
	}

	fn lookup_local(&self, name: &str) -> Option<Local> {
		self.locals.lookup(name).copied()
	}
}

#[cfg(test)]
mod tests {
	use crate::ast::AssignmentExpr;
	use crate::ast::AssignmentOperator;
	use crate::ast::BinaryExpr;
	use crate::ast::BinaryOperator;
	use crate::ast::BlockStatement;
	use crate::ast::BooleanLiteral;
	use crate::ast::DataType;
	use crate::ast::DecimalLiteral;
	use crate::ast::Expr;
	use crate::ast::IdentifierExpr;
	use crate::ast::IfStatement;
	use crate::ast::IntegerLiteral;
	use crate::ast::Program as AstProgram;
	use crate::ast::Statement;
	use crate::ast::TextLiteral;
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
				position: 0,
				value: 1,
			})),
			operator: BinaryOperator::Add,
			position: 0,
			right: Box::new(Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 2,
				})),
				operator: BinaryOperator::Add,
				position: 0,
				right: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
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
			position: 0,
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
						position: 0,
						value: 5,
					})),
					is_const: false,
					name: String::from("x"),
					position: 0,
				}),
			],
			result: Some(Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::AddAssign,
				position: 0,
				target: IdentifierExpr {
					name: String::from("x"),
					position: 0,
				},
				value: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
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
			position: 0,
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
				position: 0,
				value: true,
			})),
			operator: BinaryOperator::Equal,
			position: 0,
			right: Box::new(Expr::Boolean(BooleanLiteral {
				position: 0,
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
						position: 0,
						value: 5,
					})),
					is_const: false,
					name: String::from("x"),
					position: 0,
				}),
				Statement::Expression(Expr::Assignment(AssignmentExpr {
					operator: AssignmentOperator::AddAssign,
					position: 0,
					target: IdentifierExpr {
						name: String::from("x"),
						position: 0,
					},
					value: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 3,
					})),
				})),
			],
			result: Some(Expr::Identifier(IdentifierExpr {
				name: String::from("x"),
				position: 0,
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
	fn compiles_if_else_statement() {
		let program = AstProgram {
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 1,
					})),
					is_const: false,
					name: String::from("x"),
					position: 0,
				}),
				Statement::If(IfStatement {
					condition: Expr::Boolean(BooleanLiteral {
						position: 0,
						value: false,
					}),
					else_branch: Some(Box::new(Statement::Block(BlockStatement {
						position: 0,
						statements: vec![
							Statement::Expression(Expr::Assignment(AssignmentExpr {
								operator: AssignmentOperator::Assign,
								position: 0,
								target: IdentifierExpr {
									name: String::from("x"),
									position: 0,
								},
								value: Box::new(Expr::Integer(IntegerLiteral {
									position: 0,
									value: 3,
								})),
							})),
						],
					}))),
					position: 0,
					then_branch: BlockStatement {
						position: 0,
						statements: vec![
							Statement::Expression(Expr::Assignment(AssignmentExpr {
								operator: AssignmentOperator::Assign,
								position: 0,
								target: IdentifierExpr {
									name: String::from("x"),
									position: 0,
								},
								value: Box::new(Expr::Integer(IntegerLiteral {
									position: 0,
									value: 2,
								})),
							})),
						],
					},
				}),
			],
			result: Some(Expr::Identifier(IdentifierExpr {
				name: String::from("x"),
				position: 0,
			})),
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.instructions, vec![
			Instruction::PushInteger(1),
			Instruction::StoreLocal(0),
			Instruction::PushBoolean(false),
			Instruction::JumpIfFalse(9),
			Instruction::PushInteger(2),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::Pop,
			Instruction::Jump(13),
			Instruction::PushInteger(3),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::Pop,
			Instruction::LoadLocal(0),
		]);
	}

	#[test]
	fn compiles_if_without_else_statement() {
		let program = AstProgram {
			statements: vec![
				Statement::If(IfStatement {
					condition: Expr::Boolean(BooleanLiteral {
						position: 0,
						value: true,
					}),
					else_branch: None,
					position: 0,
					then_branch: BlockStatement {
						position: 0,
						statements: vec![
							Statement::Expression(Expr::Integer(IntegerLiteral {
								position: 0,
								value: 1,
							})),
						],
					},
				}),
			],
			result: None,
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.instructions, vec![
			Instruction::PushBoolean(true),
			Instruction::JumpIfFalse(4),
			Instruction::PushInteger(1),
			Instruction::Pop,
		]);
	}

	#[test]
	fn compiles_integer_literal() {
		let expression = Expr::Integer(IntegerLiteral {
			position: 0,
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
				position: 0,
				value: false,
			})),
			operator: UnaryOperator::Not,
			position: 0,
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
				position: 0,
				value: true,
			})),
			operator: BinaryOperator::Xor,
			position: 0,
			right: Box::new(Expr::Boolean(BooleanLiteral {
				position: 0,
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
				position: 0,
				value: 9,
			})),
			operator: BinaryOperator::Subtract,
			position: 0,
			right: Box::new(Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 4,
				})),
				operator: BinaryOperator::Multiply,
				position: 0,
				right: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
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
						position: 0,
						value: 1,
					})),
					is_const: false,
					name: String::from("x"),
					position: 0,
				}),
			],
			result: Some(Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Identifier(IdentifierExpr {
					name: String::from("x"),
					position: 0,
				})),
				operator: BinaryOperator::Add,
				position: 0,
				right: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
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
	fn compiles_text_literal() {
		let expression = Expr::Text(TextLiteral {
			position: 0,
			value: String::from("hello"),
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushText(String::from("hello")),
		]);
	}

	#[test]
	fn compiles_unary_negation() {
		let expression = Expr::Unary(UnaryExpr {
			operand: Box::new(Expr::Integer(IntegerLiteral {
				position: 0,
				value: 42,
			})),
			operator: UnaryOperator::Negate,
			position: 0,
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.instructions, vec![
			Instruction::PushInteger(42),
			Instruction::Negate,
		]);
	}

	#[test]
	fn compiles_const_declaration() {
		let program = AstProgram {
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 5,
					})),
					is_const: true,
					name: String::from("x"),
					position: 0,
				}),
			],
			result: Some(Expr::Identifier(IdentifierExpr {
				name: String::from("x"),
				position: 0,
			})),
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.instructions, vec![
			Instruction::PushInteger(5),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
		]);
	}

	#[test]
	fn rejects_assignment_to_const() {
		let program = AstProgram {
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 5,
					})),
					is_const: true,
					name: String::from("x"),
					position: 0,
				}),
			],
			result: Some(Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::Assign,
				position: 0,
				target: IdentifierExpr {
					name: String::from("x"),
					position: 0,
				},
				value: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 3,
				})),
			})),
		};

		let error = Compiler::new().compile_program(&program).unwrap_err();

		assert_eq!(error.message, "Constant `x` cannot be assigned using `=`.");
	}

	#[test]
	fn rejects_compound_assignment_when_result_type_changes() {
		let program = AstProgram {
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 5,
					})),
					is_const: false,
					name: String::from("x"),
					position: 0,
				}),
			],
			result: Some(Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::AddAssign,
				position: 0,
				target: IdentifierExpr {
					name: String::from("x"),
					position: 0,
				},
				value: Box::new(Expr::Decimal(DecimalLiteral {
					position: 0,
					value: Decimal::from_literal("1.5").unwrap(),
				})),
			})),
		};

		let error = Compiler::new().compile_program(&program).unwrap_err();

		assert_eq!(error.message, "Cannot assign a value of type `dec` to a variable of type `int`.");
	}

	#[test]
	fn rejects_non_boolean_if_condition() {
		let program = AstProgram {
			statements: vec![
				Statement::If(IfStatement {
					condition: Expr::Integer(IntegerLiteral {
						position: 3,
						value: 1,
					}),
					else_branch: None,
					position: 0,
					then_branch: BlockStatement {
						position: 0,
						statements: Vec::new(),
					},
				}),
			],
			result: None,
		};

		let error = Compiler::new().compile_program(&program).unwrap_err();

		assert_eq!(error.message, "`if` condition must be of type `bool`, found `int`.");
		assert_eq!(error.position, 3);
	}

	#[test]
	fn rejects_wrong_type_in_assignment() {
		let program = AstProgram {
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 5,
					})),
					is_const: false,
					name: String::from("x"),
					position: 0,
				}),
			],
			result: Some(Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::Assign,
				position: 0,
				target: IdentifierExpr {
					name: String::from("x"),
					position: 0,
				},
				value: Box::new(Expr::Text(TextLiteral {
					position: 0,
					value: String::from("hello"),
				})),
			})),
		};

		let error = Compiler::new().compile_program(&program).unwrap_err();

		assert_eq!(error.message, "Cannot assign a value of type `text` to a variable of type `int`.");
	}

	#[test]
	fn rejects_wrong_type_in_variable_initializer() {
		let program = AstProgram {
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Boolean(BooleanLiteral {
						position: 0,
						value: true,
					})),
					is_const: false,
					name: String::from("x"),
					position: 0,
				}),
			],
			result: None,
		};

		let error = Compiler::new().compile_program(&program).unwrap_err();

		assert_eq!(error.message, "Cannot assign a value of type `bool` to a variable of type `int`.");
	}
}
