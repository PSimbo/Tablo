// This module is now primarily responsible for bytecode emission from a
// semantically validated AST. It still owns local-slot allocation, but most
// name resolution and type checking now live in `semantic::analyzer`.

use crate::ast::ArrayLiteral;
use crate::ast::AssignmentExpr;
use crate::ast::AssignmentOperator;
use crate::ast::AssignmentTarget;
use crate::ast::BlockStatement;
use crate::ast::BooleanLiteral;
use crate::ast::BreakStatement;
use crate::ast::CallExpr;
use crate::ast::ContinueStatement;
use crate::ast::DecimalLiteral;
use crate::ast::Expr;
use crate::ast::ForStatement;
use crate::ast::FunctionDeclaration;
use crate::ast::IdentifierExpr;
use crate::ast::IfStatement;
use crate::ast::IndexExpr;
use crate::ast::Program as AstProgram;
use crate::ast::RangeExpr;
use crate::ast::ReturnStatement;
use crate::ast::Statement;
use crate::ast::TextLiteral;
use crate::ast::UnaryExpr;
use crate::ast::VariableDeclaration;
use crate::ast::WhileStatement;
use crate::bytecode::CodeBody;
use crate::bytecode::CodeBodyDebugInfo;
use crate::bytecode::CompiledFunction;
use crate::bytecode::DebugInfo;
use crate::bytecode::Instruction;
use crate::bytecode::LocalVariableDebugInfo;
use crate::bytecode::Program;
use crate::semantic::analyzer::SemanticAnalyzer;
use crate::semantic::analyzer::SemanticProgram;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompileError {
	pub message: String,
	pub position: usize,
}

#[derive(Default)]
pub struct Compiler {
	loop_stack: Vec<LoopContext>,
}

#[derive(Default)]
struct EmissionState {
	instructions: Vec<Instruction>,
	local_scope_stack: Vec<Vec<usize>>,
	locals: Vec<LocalVariableDebugInfo>,
	positions: Vec<u32>,
}

#[derive(Default)]
struct LoopContext {
	break_jump_indices: Vec<usize>,
	continue_jump_indices: Vec<usize>,
	loop_start: u32,
}

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
		self.loop_stack.clear();
		let mut emission = EmissionState::default();
		self.compile_into(expression, &SemanticProgram::default(), &mut emission);
		Program::from_parts_with_debug(
			crate::bytecode::ConstantPool::default(),
			CodeBody::new(emission.instructions),
			DebugInfo::new(
				vec![CodeBodyDebugInfo::new(None, emission.positions, emission.locals, None)],
				vec![],
			),
		)
	}

	pub fn compile_program(&mut self, program: &AstProgram) -> Result<Program, CompileError> {
		self.loop_stack.clear();
		let semantic_program = SemanticAnalyzer::new().analyze_program(program)?;
		self.compile_program_with_semantics(program, &semantic_program)
	}

	pub fn compile_standalone_program(&mut self, program: &AstProgram) -> Result<Program, CompileError> {
		self.loop_stack.clear();
		let semantic_program = SemanticAnalyzer::new().analyze_standalone_program(program)?;
		self.compile_program_with_semantics(program, &semantic_program)
	}

	pub fn new() -> Self {
		Self {
			loop_stack: Vec::new(),
		}
	}

	fn close_all_debug_scopes(&self, emission: &mut EmissionState) {
		while !emission.local_scope_stack.is_empty() {
			self.exit_debug_scope(emission);
		}
	}

	fn compile_assignment(&mut self, slot: u32, operator: AssignmentOperator, value: &Expr, semantic_program: &SemanticProgram, emission: &mut EmissionState) {
		if let Some(instruction) = operator.compound_instruction() {
			self.emit(emission, Instruction::LoadLocal(slot), value.position());
			self.compile_into(value, semantic_program, emission);
			self.emit(emission, instruction, value.position());
		}
		else {
			self.compile_into(value, semantic_program, emission);
		}

		self.emit(emission, Instruction::StoreLocal(slot), value.position());
		self.emit(emission, Instruction::LoadLocal(slot), value.position());
	}

	fn compile_entry_body(
		&mut self,
		program: &AstProgram,
		semantic_program: &SemanticProgram,
		emission: &mut EmissionState,
	) -> Result<(), CompileError> {
		for statement in &program.statements {
			self.compile_statement(statement, semantic_program, emission)?;
		}

		if let Some(result) = &program.result {
			self.compile_into(result, semantic_program, emission);
		}

		Ok(())
	}

	fn compile_error(&self, position: usize, message: impl Into<String>) -> CompileError {
		CompileError {
			message: message.into(),
			position,
		}
	}

	fn compile_function(&mut self, function: &FunctionDeclaration, semantic_program: &SemanticProgram) -> Result<(CompiledFunction, CodeBodyDebugInfo), CompileError> {
		self.loop_stack.clear();
		let mut emission = EmissionState::default();
		self.enter_debug_scope(&mut emission);

		for parameter in &function.parameters {
			let slot = semantic_program.declaration_slot(parameter.position).ok_or(self.compile_error(
				parameter.position,
				format!("Missing slot for parameter `{}`.", parameter.name),
			))?;
			self.record_local_debug(
				&mut emission,
				parameter.name.clone(),
				slot,
				data_type_name(&parameter.data_type),
				false,
				0,
			);
		}

		self.compile_statement(&Statement::Block(function.body.clone()), semantic_program, &mut emission)?;
		self.close_all_debug_scopes(&mut emission);

		Ok((
			CompiledFunction::new(
				Some(function.name.clone()),
				CodeBody::new(emission.instructions),
			),
			CodeBodyDebugInfo::new(Some(function.name.clone()), emission.positions, emission.locals, None),
		))
	}

	fn compile_indexed_assignment(
		&mut self,
		slot: u32,
		operator: AssignmentOperator,
		index: &Expr,
		value: &Expr,
		semantic_program: &SemanticProgram,
		emission: &mut EmissionState,
	) {
		self.emit(emission, Instruction::LoadLocal(slot), index.position());
		self.compile_into(index, semantic_program, emission);

		if let Some(instruction) = operator.compound_instruction() {
			self.emit(emission, Instruction::Dup2, index.position());
			self.emit(emission, Instruction::LoadIndex, index.position());
			self.compile_into(value, semantic_program, emission);
			self.emit(emission, instruction, value.position());
		}
		else {
			self.compile_into(value, semantic_program, emission);
		}

		self.emit(emission, Instruction::StoreIndex, value.position());
		self.emit(emission, Instruction::StoreLocal(slot), value.position());
	}

	fn compile_into(&mut self, expression: &Expr, semantic_program: &SemanticProgram, emission: &mut EmissionState) {
		let _ = self;

		match expression {
			Expr::Array(ArrayLiteral { elements, .. }) => {
				for element in elements {
					self.compile_into(element, semantic_program, emission);
				}

				self.emit(emission, Instruction::MakeArray(elements.len() as u32), expression.position());
			}
			Expr::Assignment(AssignmentExpr { operator, target, value, .. }) => {
				match target {
					AssignmentTarget::Identifier(target) => {
						let slot = semantic_program.identifier_slot(target.position)
							.unwrap_or_else(|| panic!("Missing slot for identifier `{}`.", target.name));
						self.compile_assignment(slot, *operator, value, semantic_program, emission);
					}
					AssignmentTarget::Index(target) => {
						let slot = semantic_program.identifier_slot(target.array.position)
							.unwrap_or_else(|| panic!("Missing slot for identifier `{}`.", target.array.name));
						self.compile_indexed_assignment(slot, *operator, &target.index, value, semantic_program, emission);
					}
				}
			}
			Expr::Binary(binary) => {
				self.compile_into(&binary.left, semantic_program, emission);
				self.compile_into(&binary.right, semantic_program, emission);
				self.emit(emission, binary.operator.instruction(), binary.position);
			}
			Expr::Boolean(BooleanLiteral { value, .. }) => {
				self.emit(emission, Instruction::PushBoolean(*value), expression.position());
			}
			Expr::Call(CallExpr { arguments, .. }) => {
				for argument in arguments {
					self.compile_into(argument, semantic_program, emission);
				}

				if let Some(built_in) = semantic_program.built_in_call_target(expression.position()) {
					self.emit(emission, Instruction::CallBuiltIn(built_in, arguments.len() as u32), expression.position());
				}
				else {
					let function_index = semantic_program.call_target(expression.position())
						.unwrap_or_else(|| panic!("Missing function target for call expression."));
					self.emit(emission, Instruction::Call(function_index, arguments.len() as u32), expression.position());
				}
			}
			Expr::Decimal(DecimalLiteral { value, .. }) => {
				self.emit(emission, Instruction::PushDecimal(value.clone()), expression.position());
			}
			Expr::Identifier(IdentifierExpr { name, .. }) => {
				let slot = semantic_program.identifier_slot(expression.position())
					.unwrap_or_else(|| panic!("Missing slot for identifier `{name}`."));
				self.emit(emission, Instruction::LoadLocal(slot), expression.position());
			}
			Expr::Index(IndexExpr { array, index, .. }) => {
				self.compile_into(array, semantic_program, emission);
				self.compile_into(index, semantic_program, emission);
				self.emit(emission, Instruction::LoadIndex, expression.position());
			}
			Expr::Integer(integer) => {
				self.emit(emission, Instruction::PushInteger(integer.value), expression.position());
			}
			Expr::Range(RangeExpr { start, step, end, .. }) => {
				self.compile_into(start, semantic_program, emission);

				if let Some(step) = step {
					self.compile_into(step, semantic_program, emission);
					self.compile_into(end, semantic_program, emission);
					self.emit(emission, Instruction::MakeSteppedRange, expression.position());
				}
				else {
					self.compile_into(end, semantic_program, emission);
					self.emit(emission, Instruction::MakeRange, expression.position());
				}
			}
			Expr::Text(TextLiteral { value, .. }) => {
				self.emit(emission, Instruction::PushText(value.clone()), expression.position());
			}
			Expr::Unary(UnaryExpr { operand, operator, .. }) => {
				self.compile_into(operand, semantic_program, emission);
				self.emit(emission, operator.instruction(), expression.position());
			}
		}
	}

	fn compile_program_with_semantics(&mut self, program: &AstProgram, semantic_program: &SemanticProgram) -> Result<Program, CompileError> {
		let mut functions = Vec::with_capacity(program.functions.len());
		let mut code_body_debug = Vec::with_capacity(program.functions.len() + 1);

		for function in &program.functions {
			let (compiled_function, debug_info) = self.compile_function(function, &semantic_program)?;
			functions.push(compiled_function);
			code_body_debug.push(debug_info);
		}

		if let Some(function_index) = semantic_program.entry_point_function_index() {
			return Ok(Program::from_entry_function(
				crate::bytecode::ConstantPool::default(),
				function_index,
				functions,
				DebugInfo::new(code_body_debug, vec![]),
			));
		}

		let mut emission = EmissionState::default();
		self.enter_debug_scope(&mut emission);
		self.compile_entry_body(program, semantic_program, &mut emission)?;

		self.close_all_debug_scopes(&mut emission);
		code_body_debug.push(CodeBodyDebugInfo::new(None, emission.positions, emission.locals, None));

		Ok(Program::from_parts_with_functions_and_debug(
			crate::bytecode::ConstantPool::default(),
			CodeBody::new(emission.instructions),
			functions,
			DebugInfo::new(code_body_debug, vec![]),
		))
	}

	fn compile_statement(&mut self, statement: &Statement, semantic_program: &SemanticProgram, emission: &mut EmissionState) -> Result<(), CompileError> {
		match statement {
			Statement::Block(BlockStatement { statements, .. }) => {
				self.enter_debug_scope(emission);

				for statement in statements {
					self.compile_statement(statement, semantic_program, emission)?;
				}

				self.exit_debug_scope(emission);
				Ok(())
			}
			Statement::Break(BreakStatement { position }) => {
				let Some(loop_context) = self.loop_stack.last_mut() else {
					return Err(self.compile_error(
						*position,
						String::from("`break` may only be used inside a `while` or `for` loop."),
					));
				};
				loop_context.break_jump_indices.push(emission.instructions.len());
				self.emit(emission, Instruction::Jump(0), *position);
				Ok(())
			}
			Statement::Continue(ContinueStatement { position }) => {
				let Some(loop_context) = self.loop_stack.last_mut() else {
					return Err(self.compile_error(
						*position,
						String::from("`continue` may only be used inside a `while` or `for` loop."),
					));
				};
				loop_context.continue_jump_indices.push(emission.instructions.len());
				self.emit(emission, Instruction::Jump(0), *position);
				Ok(())
			}
			Statement::Expression(expression) => {
				self.compile_into(expression, semantic_program, emission);

				if expression_produces_runtime_value(expression, semantic_program) {
					self.emit(emission, Instruction::Pop, expression.position());
				}

				Ok(())
			}
			Statement::For(ForStatement {
				body,
				iterable,
				position,
				variable,
			}) => {
				let variable_slot = semantic_program.declaration_slot(variable.position).ok_or(self.compile_error(
					variable.position,
					format!("Missing slot for loop variable `{}`.", variable.name),
				))?;
				let iterator_slot = semantic_program.iterator_slot(*position).ok_or(self.compile_error(
					*position,
					String::from("Missing iterator slot for `for` statement."),
				))?;

				self.enter_debug_scope(emission);
				self.compile_into(iterable, semantic_program, emission);
				self.emit(emission, Instruction::IterInit, iterable.position());
				self.emit(emission, Instruction::StoreLocal(iterator_slot), *position);

				let loop_start = emission.instructions.len() as u32;
				self.loop_stack.push(LoopContext {
					break_jump_indices: Vec::new(),
					continue_jump_indices: Vec::new(),
					loop_start,
				});

				self.emit(emission, Instruction::LoadLocal(iterator_slot), *position);
				self.emit(emission, Instruction::IterHasNext, iterable.position());
				let jump_if_false_index = emission.instructions.len();
				self.emit(emission, Instruction::JumpIfFalse(0), *position);

				self.emit(emission, Instruction::LoadLocal(iterator_slot), *position);
				self.emit(emission, Instruction::IterNext, iterable.position());
				self.emit(emission, Instruction::StoreLocal(iterator_slot), *position);
				self.emit(emission, Instruction::StoreLocal(variable_slot), variable.position);
				let variable_type = semantic_program.declaration_type(variable.position).ok_or(self.compile_error(
					variable.position,
					format!("Missing declared type for loop variable `{}`.", variable.name),
				))?;
				self.record_local_debug(
					emission,
					variable.name.clone(),
					variable_slot,
					data_type_name(variable_type),
					false,
					emission.instructions.len() as u32,
				);

				self.compile_statement(&Statement::Block(body.clone()), semantic_program, emission)?;
				self.emit(emission, Instruction::Jump(loop_start), *position);

				let loop_end = emission.instructions.len() as u32;
				emission.instructions[jump_if_false_index] = Instruction::JumpIfFalse(loop_end);

				let loop_context = self.loop_stack.pop()
					.expect("Loop context must exist while compiling a for statement.");

				for break_jump_index in loop_context.break_jump_indices {
					emission.instructions[break_jump_index] = Instruction::Jump(loop_end);
				}

				for continue_jump_index in loop_context.continue_jump_indices {
					emission.instructions[continue_jump_index] = Instruction::Jump(loop_context.loop_start);
				}

				self.exit_debug_scope(emission);
				Ok(())
			}
			Statement::If(IfStatement {
				condition,
				else_branch,
				then_branch,
				..
			}) => {
				self.compile_into(condition, semantic_program, emission);
				let jump_if_false_index = emission.instructions.len();
				self.emit(emission, Instruction::JumpIfFalse(0), condition.position());

				self.compile_statement(&Statement::Block(then_branch.clone()), semantic_program, emission)?;

				if let Some(else_branch) = else_branch {
					let jump_to_end_index = emission.instructions.len();
					self.emit(emission, Instruction::Jump(0), statement_position(statement));

					let else_target = emission.instructions.len() as u32;
					emission.instructions[jump_if_false_index] = Instruction::JumpIfFalse(else_target);
					self.compile_statement(else_branch, semantic_program, emission)?;

					let end_target = emission.instructions.len() as u32;
					emission.instructions[jump_to_end_index] = Instruction::Jump(end_target);
				}
				else {
					let end_target = emission.instructions.len() as u32;
					emission.instructions[jump_if_false_index] = Instruction::JumpIfFalse(end_target);
				}

				Ok(())
			}
			Statement::Return(ReturnStatement { value, .. }) => {
				if let Some(value) = value {
					self.compile_into(value, semantic_program, emission);
					self.emit(emission, Instruction::Return, value.position());
				}
				else {
					self.emit(emission, Instruction::ReturnVoid, statement_position(statement));
				}

				Ok(())
			}
			Statement::VariableDeclaration(VariableDeclaration { data_type, initial_value, is_const, name, position }) => {
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
				self.compile_into(initial_value, semantic_program, emission);
				self.emit(emission, Instruction::StoreLocal(slot), *position);
				self.record_local_debug(
					emission,
					name.clone(),
					slot,
					data_type_name(data_type),
					*is_const,
					emission.instructions.len() as u32,
				);

				Ok(())
			}
			Statement::While(WhileStatement {
				body,
				condition,
				..
			}) => {
				let loop_start = emission.instructions.len() as u32;
				self.loop_stack.push(LoopContext {
					break_jump_indices: Vec::new(),
					continue_jump_indices: Vec::new(),
					loop_start,
				});
				self.compile_into(condition, semantic_program, emission);
				let jump_if_false_index = emission.instructions.len();
				self.emit(emission, Instruction::JumpIfFalse(0), condition.position());

				self.compile_statement(&Statement::Block(body.clone()), semantic_program, emission)?;
				self.emit(emission, Instruction::Jump(loop_start), statement_position(statement));

				let loop_end = emission.instructions.len() as u32;
				emission.instructions[jump_if_false_index] = Instruction::JumpIfFalse(loop_end);

				let loop_context = self.loop_stack.pop()
					.expect("Loop context must exist while compiling a while statement.");

				for break_jump_index in loop_context.break_jump_indices {
					emission.instructions[break_jump_index] = Instruction::Jump(loop_end);
				}

				for continue_jump_index in loop_context.continue_jump_indices {
					emission.instructions[continue_jump_index] = Instruction::Jump(loop_context.loop_start);
				}

				Ok(())
			}
		}
	}

	fn emit(&self, emission: &mut EmissionState, instruction: Instruction, position: usize) {
		emission.instructions.push(instruction);
		emission.positions.push(position.min(u32::MAX as usize) as u32);
	}

	fn enter_debug_scope(&self, emission: &mut EmissionState) {
		emission.local_scope_stack.push(Vec::new());
	}

	fn exit_debug_scope(&self, emission: &mut EmissionState) {
		let Some(local_indices) = emission.local_scope_stack.pop() else {
			return;
		};

		let scope_end = emission.instructions.len() as u32;

		for local_index in local_indices {
			if let Some(local) = emission.locals.get_mut(local_index) {
				*local = LocalVariableDebugInfo::new(
					local.name().to_string(),
					local.slot(),
					local.declared_type().to_string(),
					local.is_const(),
					local.scope_start(),
					scope_end,
				);
			}
		}
	}

	fn record_local_debug(
		&self,
		emission: &mut EmissionState,
		name: String,
		slot: u32,
		declared_type: String,
		is_const: bool,
		scope_start: u32,
	) {
		let local_index = emission.locals.len();
		emission.locals.push(LocalVariableDebugInfo::new(
			name,
			slot,
			declared_type,
			is_const,
			scope_start,
			scope_start,
		));

		if let Some(scope) = emission.local_scope_stack.last_mut() {
			scope.push(local_index);
		}
	}
}

fn data_type_name(data_type: &crate::ast::DataType) -> String {
	match data_type {
		crate::ast::DataType::Array(element_type) => format!("[{}]", data_type_name(element_type)),
		crate::ast::DataType::Bool => String::from("bool"),
		crate::ast::DataType::Dec => String::from("dec"),
		crate::ast::DataType::EmptyArray => String::from("empty array"),
		crate::ast::DataType::Int => String::from("int"),
		crate::ast::DataType::Range(element_type) => format!("range<{}>", data_type_name(element_type)),
		crate::ast::DataType::Text => String::from("text"),
		crate::ast::DataType::Void => String::from("void"),
	}
}

fn expression_produces_runtime_value(expression: &Expr, semantic_program: &SemanticProgram) -> bool {
	match expression {
		Expr::Call(call) => semantic_program.call_return_type(call.position) != Some(crate::ast::DataType::Void),
		_ => true,
	}
}

fn statement_position(statement: &Statement) -> usize {
	match statement {
		Statement::Block(block) => block.position,
		Statement::Break(statement) => statement.position,
		Statement::Continue(statement) => statement.position,
		Statement::Expression(expression) => expression.position(),
		Statement::For(statement) => statement.position,
		Statement::If(statement) => statement.position,
		Statement::Return(statement) => statement.position,
		Statement::VariableDeclaration(statement) => statement.position,
		Statement::While(statement) => statement.position,
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
	use crate::ast::BreakStatement;
	use crate::ast::CallExpr;
	use crate::ast::ContinueStatement;
	use crate::ast::DataType;
	use crate::ast::DecimalLiteral;
	use crate::ast::Expr;
	use crate::ast::ForStatement;
	use crate::ast::IdentifierExpr;
	use crate::ast::IfStatement;
	use crate::ast::IntegerLiteral;
	use crate::ast::Program as AstProgram;
	use crate::ast::RangeExpr;
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

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
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

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
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
	fn compiles_for_statement() {
		let program = AstProgram {
			functions: vec![],
			statements: vec![
				Statement::For(ForStatement {
					body: BlockStatement {
						position: 0,
						statements: vec![
							Statement::Expression(Expr::Identifier(IdentifierExpr {
								name: String::from("value"),
								position: 1,
							})),
						],
					},
					iterable: Expr::Array(ArrayLiteral {
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
					position: 0,
					variable: IdentifierExpr {
						name: String::from("value"),
						position: 1,
					},
				}),
			],
			result: None,
		};

		let program = Compiler::new().compile_program(&program).unwrap();

		assert_eq!(program.entry_code().unwrap().instructions, vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::MakeArray(2),
			Instruction::IterInit,
			Instruction::StoreLocal(1),
			Instruction::LoadLocal(1),
			Instruction::IterHasNext,
			Instruction::JumpIfFalse(15),
			Instruction::LoadLocal(1),
			Instruction::IterNext,
			Instruction::StoreLocal(1),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::Pop,
			Instruction::Jump(5),
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
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

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
			Instruction::PushInteger(1),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::PushInteger(2),
			Instruction::Add,
		]);
	}

	#[test]
	fn compiles_range_expression() {
		let expression = Expr::Range(RangeExpr {
			start: Box::new(Expr::Integer(IntegerLiteral {
				position: 0,
				value: 0,
			})),
			step: None,
			end: Box::new(Expr::Integer(IntegerLiteral {
				position: 2,
				value: 10,
			})),
			position: 1,
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.entry_code().unwrap().instructions, vec![
			Instruction::PushInteger(0),
			Instruction::PushInteger(10),
			Instruction::MakeRange,
		]);
	}

	#[test]
	fn compiles_text_literal() {
		let expression = Expr::Text(TextLiteral {
			position: 0,
			value: String::from("hello"),
		});

		let program = Compiler::new().compile_expression(&expression);

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(program.entry_code().unwrap().instructions, vec![
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
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
	fn compiles_while_statement_with_break_and_continue() {
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
							Statement::If(IfStatement {
								condition: Expr::Binary(BinaryExpr {
									left: Box::new(Expr::Identifier(IdentifierExpr {
										name: String::from("x"),
										position: 0,
									})),
									operator: BinaryOperator::LessThan,
									position: 0,
									right: Box::new(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 2,
									})),
								}),
								else_branch: None,
								position: 0,
								then_branch: BlockStatement {
									position: 0,
									statements: vec![
										Statement::Continue(ContinueStatement {
											position: 0,
										}),
									],
								},
							}),
							Statement::Break(BreakStatement {
								position: 0,
							}),
						],
					},
					condition: Expr::Boolean(BooleanLiteral {
						position: 0,
						value: true,
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

		assert_eq!(bytecode.entry_code().unwrap().instructions, vec![
			Instruction::PushInteger(0),
			Instruction::StoreLocal(0),
			Instruction::PushBoolean(true),
			Instruction::JumpIfFalse(17),
			Instruction::LoadLocal(0),
			Instruction::PushInteger(1),
			Instruction::Add,
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::Pop,
			Instruction::LoadLocal(0),
			Instruction::PushInteger(2),
			Instruction::LessThan,
			Instruction::JumpIfFalse(15),
			Instruction::Jump(2),
			Instruction::Jump(17),
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
