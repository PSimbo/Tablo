// This module is now primarily responsible for bytecode emission from a
// semantically validated AST. It still owns local-slot allocation, but most
// name resolution and type checking now live in `semantic::analyzer`.

use crate::ast::ArrayLiteral;
use crate::ast::AssignmentExpr;
use crate::ast::AssignmentOperator;
use crate::ast::AssignmentTarget;
use crate::ast::BlockStatement;
use crate::ast::BooleanLiteral;
use crate::ast::CallExpr;
use crate::ast::DecimalLiteral;
use crate::ast::Expr;
use crate::ast::FunctionDeclaration;
use crate::ast::IdentifierExpr;
use crate::ast::IfStatement;
use crate::ast::IndexExpr;
use crate::ast::Program as AstProgram;
use crate::ast::ReturnStatement;
use crate::ast::Statement;
use crate::ast::TextLiteral;
use crate::ast::UnaryExpr;
use crate::ast::VariableDeclaration;
use crate::ast::WhileStatement;
use crate::bytecode::CodeBody;
use crate::bytecode::CompiledFunction;
use crate::bytecode::Instruction;
use crate::bytecode::Program;
use crate::semantic::analyzer::SemanticAnalyzer;
use crate::semantic::analyzer::SemanticProgram;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompileError {
	pub message: String,
	pub position: usize,
}

#[derive(Default)]
pub struct Compiler;

impl AssignmentOperator {
	fn compound_instruction(self) -> Option<Instruction> {
		match self {
			AssignmentOperator::Assign => None,
			AssignmentOperator::AddAssign => Some(Instruction::Add),
			AssignmentOperator::DivideAssign => Some(Instruction::Divide),
			AssignmentOperator::ModuloAssign => Some(Instruction::Modulo),
			AssignmentOperator::MultiplyAssign => Some(Instruction::Multiply),
			AssignmentOperator::SubtractAssign => Some(Instruction::Subtract),
		}
	}
}

impl crate::ast::BinaryOperator {
	fn instruction(self) -> Instruction {
		match self {
			crate::ast::BinaryOperator::Add => Instruction::Add,
			crate::ast::BinaryOperator::And => Instruction::And,
			crate::ast::BinaryOperator::Divide => Instruction::Divide,
			crate::ast::BinaryOperator::Equal => Instruction::Equal,
			crate::ast::BinaryOperator::GreaterThan => Instruction::GreaterThan,
			crate::ast::BinaryOperator::GreaterThanOrEqual => Instruction::GreaterThanOrEqual,
			crate::ast::BinaryOperator::LessThan => Instruction::LessThan,
			crate::ast::BinaryOperator::LessThanOrEqual => Instruction::LessThanOrEqual,
			crate::ast::BinaryOperator::Modulo => Instruction::Modulo,
			crate::ast::BinaryOperator::Multiply => Instruction::Multiply,
			crate::ast::BinaryOperator::NotEqual => Instruction::NotEqual,
			crate::ast::BinaryOperator::Or => Instruction::Or,
			crate::ast::BinaryOperator::Subtract => Instruction::Subtract,
			crate::ast::BinaryOperator::Xor => Instruction::Xor,
		}
	}
}

impl crate::ast::UnaryOperator {
	fn instruction(self) -> Instruction {
		match self {
			crate::ast::UnaryOperator::Negate => Instruction::Negate,
			crate::ast::UnaryOperator::Not => Instruction::Not,
		}
	}
}

impl Compiler {
	pub fn compile_expression(&mut self, expression: &Expr) -> Program {
		let mut instructions = Vec::new();
		self.compile_into(expression, &SemanticProgram::default(), &mut instructions);
		Program::new(instructions)
	}

	pub fn compile_program(&mut self, program: &AstProgram) -> Result<Program, CompileError> {
		let semantic_program = SemanticAnalyzer::new().analyze_program(program)?;
		let mut functions = Vec::with_capacity(program.functions.len());

		for function in &program.functions {
			functions.push(self.compile_function(function, &semantic_program)?);
		}

		let mut instructions = Vec::new();

		for statement in &program.statements {
			self.compile_statement(statement, &semantic_program, &mut instructions)?;
		}

		if let Some(result) = &program.result {
			self.compile_into(result, &semantic_program, &mut instructions);
		}

		Ok(Program::from_parts_with_functions(
			crate::bytecode::ConstantPool::default(),
			CodeBody::new(instructions),
			functions,
		))
	}

	pub fn new() -> Self {
		Self
	}

	fn compile_assignment(&mut self, slot: u32, operator: AssignmentOperator, value: &Expr, semantic_program: &SemanticProgram, instructions: &mut Vec<Instruction>) {
		if let Some(instruction) = operator.compound_instruction() {
			instructions.push(Instruction::LoadLocal(slot));
			self.compile_into(value, semantic_program, instructions);
			instructions.push(instruction);
		}
		else {
			self.compile_into(value, semantic_program, instructions);
		}

		instructions.push(Instruction::StoreLocal(slot));
		instructions.push(Instruction::LoadLocal(slot));
	}

	fn compile_error(&self, position: usize, message: impl Into<String>) -> CompileError {
		CompileError {
			message: message.into(),
			position,
		}
	}

	fn compile_function(&mut self, function: &FunctionDeclaration, semantic_program: &SemanticProgram) -> Result<CompiledFunction, CompileError> {
		let mut instructions = Vec::new();
		self.compile_statement(&Statement::Block(function.body.clone()), semantic_program, &mut instructions)?;

		Ok(CompiledFunction::new(
			Some(function.name.clone()),
			CodeBody::new(instructions),
		))
	}

	fn compile_indexed_assignment(
		&mut self,
		slot: u32,
		operator: AssignmentOperator,
		index: &Expr,
		value: &Expr,
		semantic_program: &SemanticProgram,
		instructions: &mut Vec<Instruction>,
	) {
		instructions.push(Instruction::LoadLocal(slot));
		self.compile_into(index, semantic_program, instructions);

		if let Some(instruction) = operator.compound_instruction() {
			instructions.push(Instruction::Dup2);
			instructions.push(Instruction::LoadIndex);
			self.compile_into(value, semantic_program, instructions);
			instructions.push(instruction);
		}
		else {
			self.compile_into(value, semantic_program, instructions);
		}

		instructions.push(Instruction::StoreIndex);
		instructions.push(Instruction::StoreLocal(slot));
	}

	fn compile_into(&mut self, expression: &Expr, semantic_program: &SemanticProgram, instructions: &mut Vec<Instruction>) {
		let _ = self;

		match expression {
			Expr::Array(ArrayLiteral { elements, .. }) => {
				for element in elements {
					self.compile_into(element, semantic_program, instructions);
				}

				instructions.push(Instruction::MakeArray(elements.len() as u32));
			}
			Expr::Assignment(AssignmentExpr { operator, target, value, .. }) => {
				match target {
					AssignmentTarget::Identifier(target) => {
						let slot = semantic_program.identifier_slot(target.position)
							.unwrap_or_else(|| panic!("Missing slot for identifier `{}`.", target.name));
						self.compile_assignment(slot, *operator, value, semantic_program, instructions);
					}
					AssignmentTarget::Index(target) => {
						let slot = semantic_program.identifier_slot(target.array.position)
							.unwrap_or_else(|| panic!("Missing slot for identifier `{}`.", target.array.name));
						self.compile_indexed_assignment(slot, *operator, &target.index, value, semantic_program, instructions);
					}
				}
			}
			Expr::Binary(binary) => {
				self.compile_into(&binary.left, semantic_program, instructions);
				self.compile_into(&binary.right, semantic_program, instructions);
				instructions.push(binary.operator.instruction());
			}
			Expr::Boolean(BooleanLiteral { value, .. }) => {
				instructions.push(Instruction::PushBoolean(*value));
			}
			Expr::Call(CallExpr { arguments, .. }) => {
				for argument in arguments {
					self.compile_into(argument, semantic_program, instructions);
				}

				if let Some(built_in) = semantic_program.built_in_call_target(expression.position()) {
					instructions.push(Instruction::CallBuiltIn(built_in, arguments.len() as u32));
				}
				else {
					let function_index = semantic_program.call_target(expression.position())
						.unwrap_or_else(|| panic!("Missing function target for call expression."));
					instructions.push(Instruction::Call(function_index, arguments.len() as u32));
				}
			}
			Expr::Decimal(DecimalLiteral { value, .. }) => {
				instructions.push(Instruction::PushDecimal(value.clone()));
			}
			Expr::Identifier(IdentifierExpr { name, .. }) => {
				let slot = semantic_program.identifier_slot(expression.position())
					.unwrap_or_else(|| panic!("Missing slot for identifier `{name}`."));
				instructions.push(Instruction::LoadLocal(slot));
			}
			Expr::Index(IndexExpr { array, index, .. }) => {
				self.compile_into(array, semantic_program, instructions);
				self.compile_into(index, semantic_program, instructions);
				instructions.push(Instruction::LoadIndex);
			}
			Expr::Integer(integer) => {
				instructions.push(Instruction::PushInteger(integer.value));
			}
			Expr::Text(TextLiteral { value, .. }) => {
				instructions.push(Instruction::PushText(value.clone()));
			}
			Expr::Unary(UnaryExpr { operand, operator, .. }) => {
				self.compile_into(operand, semantic_program, instructions);
				instructions.push(operator.instruction());
			}
		}
	}

	fn compile_statement(&mut self, statement: &Statement, semantic_program: &SemanticProgram, instructions: &mut Vec<Instruction>) -> Result<(), CompileError> {
		match statement {
			Statement::Block(BlockStatement { statements, .. }) => {
				for statement in statements {
					self.compile_statement(statement, semantic_program, instructions)?;
				}

				Ok(())
			}
			Statement::Expression(expression) => {
				self.compile_into(expression, semantic_program, instructions);

				if expression_produces_runtime_value(expression, semantic_program) {
					instructions.push(Instruction::Pop);
				}

				Ok(())
			}
			Statement::If(IfStatement {
				condition,
				else_branch,
				then_branch,
				..
			}) => {
				self.compile_into(condition, semantic_program, instructions);
				let jump_if_false_index = instructions.len();
				instructions.push(Instruction::JumpIfFalse(0));

				self.compile_statement(&Statement::Block(then_branch.clone()), semantic_program, instructions)?;

				if let Some(else_branch) = else_branch {
					let jump_to_end_index = instructions.len();
					instructions.push(Instruction::Jump(0));

					let else_target = instructions.len() as u32;
					instructions[jump_if_false_index] = Instruction::JumpIfFalse(else_target);
					self.compile_statement(else_branch, semantic_program, instructions)?;

					let end_target = instructions.len() as u32;
					instructions[jump_to_end_index] = Instruction::Jump(end_target);
				}
				else {
					let end_target = instructions.len() as u32;
					instructions[jump_if_false_index] = Instruction::JumpIfFalse(end_target);
				}

				Ok(())
			}
			Statement::Return(ReturnStatement { value, .. }) => {
				if let Some(value) = value {
					self.compile_into(value, semantic_program, instructions);
					instructions.push(Instruction::Return);
				}
				else {
					instructions.push(Instruction::ReturnVoid);
				}

				Ok(())
			}
			Statement::VariableDeclaration(VariableDeclaration { data_type: _, initial_value, is_const, name, position }) => {
				let slot = semantic_program.declaration_slot(*position).ok_or(self.compile_error(
					*position,
					format!("Missing slot for variable declaration `{name}`."),
				))?;

				let initial_value = initial_value.as_ref().ok_or(self.compile_error(
					*position,
					if *is_const {
						format!("Constant `{name}` must currently have an initializer.")
					}
					else {
						format!("Variable `{name}` must currently have an initializer.")
					},
				))?;
				self.compile_into(initial_value, semantic_program, instructions);
				instructions.push(Instruction::StoreLocal(slot));

				Ok(())
			}
			Statement::While(WhileStatement {
				body,
				condition,
				..
			}) => {
				let loop_start = instructions.len() as u32;
				self.compile_into(condition, semantic_program, instructions);
				let jump_if_false_index = instructions.len();
				instructions.push(Instruction::JumpIfFalse(0));

				self.compile_statement(&Statement::Block(body.clone()), semantic_program, instructions)?;
				instructions.push(Instruction::Jump(loop_start));

				let loop_end = instructions.len() as u32;
				instructions[jump_if_false_index] = Instruction::JumpIfFalse(loop_end);

				Ok(())
			}
		}
	}
}

fn expression_produces_runtime_value(expression: &Expr, semantic_program: &SemanticProgram) -> bool {
	match expression {
		Expr::Call(call) => semantic_program.call_return_type(call.position) != Some(crate::ast::DataType::Void),
		_ => true,
	}
}

#[cfg(test)]
mod tests {
	use crate::ast::ArrayLiteral;
	use crate::ast::AssignmentExpr;
	use crate::ast::AssignmentOperator;
	use crate::ast::AssignmentTarget;
	use crate::ast::BinaryExpr;
	use crate::ast::BinaryOperator;
	use crate::ast::BlockStatement;
	use crate::ast::BooleanLiteral;
	use crate::ast::CallExpr;
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
	use crate::ast::WhileStatement;
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

		assert_eq!(program.entry.instructions, vec![
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

		assert_eq!(program.entry.instructions, vec![
			Instruction::PushBoolean(true),
		]);
	}

	#[test]
	fn compiles_built_in_len_call() {
		let program = AstProgram {
			functions: vec![],
			statements: vec![],
			result: Some(Expr::Call(CallExpr {
				arguments: vec![
					Expr::Array(ArrayLiteral {
						elements: vec![
							Expr::Integer(IntegerLiteral {
								position: 0,
								value: 1,
							}),
							Expr::Integer(IntegerLiteral {
								position: 0,
								value: 2,
							}),
						],
						position: 0,
					}),
				],
				callee: IdentifierExpr {
					name: String::from("len"),
					position: 0,
				},
				position: 0,
			})),
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.entry.instructions, vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::MakeArray(2),
			Instruction::CallBuiltIn(crate::builtins::BuiltInFunction::Len, 1),
		]);
	}

	#[test]
	fn compiles_compound_assignment_expression() {
		let program = AstProgram {
			functions: vec![],
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
				target: AssignmentTarget::Identifier(IdentifierExpr {
					name: String::from("x"),
					position: 0,
				}),
				value: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 3,
				})),
			})),
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.entry.instructions, vec![
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
	fn compiles_compound_indexed_assignment_expression() {
		let program = AstProgram {
			functions: vec![],
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Array(Box::new(DataType::Int)),
					initial_value: Some(Expr::Array(ArrayLiteral {
						elements: vec![
							Expr::Integer(IntegerLiteral {
								position: 0,
								value: 5,
							}),
							Expr::Integer(IntegerLiteral {
								position: 0,
								value: 10,
							}),
						],
						position: 0,
					})),
					is_const: false,
					name: String::from("xs"),
					position: 0,
				}),
			],
			result: Some(Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::AddAssign,
				position: 0,
				target: AssignmentTarget::Index(crate::ast::ArrayIndexAssignmentTarget {
					array: IdentifierExpr {
						name: String::from("xs"),
						position: 0,
					},
					index: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					})),
					position: 0,
				}),
				value: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 3,
				})),
			})),
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.entry.instructions, vec![
			Instruction::PushInteger(5),
			Instruction::PushInteger(10),
			Instruction::MakeArray(2),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::PushInteger(2),
			Instruction::Dup2,
			Instruction::LoadIndex,
			Instruction::PushInteger(3),
			Instruction::Add,
			Instruction::StoreIndex,
			Instruction::StoreLocal(0),
		]);
	}

	#[test]
	fn compiles_const_declaration() {
		let program = AstProgram {
			functions: vec![],
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

		assert_eq!(bytecode.entry.instructions, vec![
			Instruction::PushInteger(5),
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

		assert_eq!(program.entry.instructions, vec![
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

		assert_eq!(program.entry.instructions, vec![
			Instruction::PushBoolean(true),
			Instruction::PushBoolean(false),
			Instruction::Equal,
		]);
	}

	#[test]
	fn compiles_expression_statement_to_pop_its_result() {
		let program = AstProgram {
			functions: vec![],
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
					target: AssignmentTarget::Identifier(IdentifierExpr {
						name: String::from("x"),
						position: 0,
					}),
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

		assert_eq!(bytecode.entry.instructions, vec![
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
			functions: vec![],
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
								target: AssignmentTarget::Identifier(IdentifierExpr {
									name: String::from("x"),
									position: 0,
								}),
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
								target: AssignmentTarget::Identifier(IdentifierExpr {
									name: String::from("x"),
									position: 0,
								}),
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

		assert_eq!(bytecode.entry.instructions, vec![
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
			functions: vec![],
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

		assert_eq!(bytecode.entry.instructions, vec![
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

		assert_eq!(program.entry.instructions, vec![
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

		assert_eq!(program.entry.instructions, vec![
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

		assert_eq!(program.entry.instructions, vec![
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

		assert_eq!(program.entry.instructions, vec![
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
			functions: vec![],
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

		assert_eq!(bytecode.entry.instructions, vec![
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

		assert_eq!(program.entry.instructions, vec![
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

		assert_eq!(program.entry.instructions, vec![
			Instruction::PushInteger(42),
			Instruction::Negate,
		]);
	}

	#[test]
	fn compiles_while_statement() {
		let program = AstProgram {
			functions: vec![],
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Int,
					initial_value: Some(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 0,
					})),
					is_const: false,
					name: String::from("x"),
					position: 0,
				}),
				Statement::While(WhileStatement {
					body: BlockStatement {
						position: 0,
						statements: vec![
							Statement::Expression(Expr::Assignment(AssignmentExpr {
								operator: AssignmentOperator::AddAssign,
								position: 0,
								target: AssignmentTarget::Identifier(IdentifierExpr {
									name: String::from("x"),
									position: 0,
								}),
								value: Box::new(Expr::Integer(IntegerLiteral {
									position: 0,
									value: 1,
								})),
							})),
						],
					},
					condition: Expr::Binary(BinaryExpr {
						left: Box::new(Expr::Identifier(IdentifierExpr {
							name: String::from("x"),
							position: 0,
						})),
						operator: BinaryOperator::LessThan,
						position: 0,
						right: Box::new(Expr::Integer(IntegerLiteral {
							position: 0,
							value: 3,
						})),
					}),
					position: 0,
				}),
			],
			result: Some(Expr::Identifier(IdentifierExpr {
				name: String::from("x"),
				position: 0,
			})),
		};

		let bytecode = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(bytecode.entry.instructions, vec![
			Instruction::PushInteger(0),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::PushInteger(3),
			Instruction::LessThan,
			Instruction::JumpIfFalse(13),
			Instruction::LoadLocal(0),
			Instruction::PushInteger(1),
			Instruction::Add,
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::Pop,
			Instruction::Jump(2),
			Instruction::LoadLocal(0),
		]);
	}

	#[test]
	fn rejects_assignment_to_const() {
		let program = AstProgram {
			functions: vec![],
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
				target: AssignmentTarget::Identifier(IdentifierExpr {
					name: String::from("x"),
					position: 0,
				}),
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
			functions: vec![],
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
				target: AssignmentTarget::Identifier(IdentifierExpr {
					name: String::from("x"),
					position: 0,
				}),
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
			functions: vec![],
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
	fn rejects_non_boolean_while_condition() {
		let program = AstProgram {
			functions: vec![],
			statements: vec![
				Statement::While(WhileStatement {
					body: BlockStatement {
						position: 0,
						statements: Vec::new(),
					},
					condition: Expr::Integer(IntegerLiteral {
						position: 6,
						value: 1,
					}),
					position: 0,
				}),
			],
			result: None,
		};

		let error = Compiler::new().compile_program(&program).unwrap_err();

		assert_eq!(error.message, "`while` condition must be of type `bool`, found `int`.");
		assert_eq!(error.position, 6);
	}

	#[test]
	fn rejects_wrong_type_in_assignment() {
		let program = AstProgram {
			functions: vec![],
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
				target: AssignmentTarget::Identifier(IdentifierExpr {
					name: String::from("x"),
					position: 0,
				}),
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
			functions: vec![],
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
