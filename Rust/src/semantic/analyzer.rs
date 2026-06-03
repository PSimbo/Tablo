use std::collections::BTreeMap;

use crate::ast::AssignmentExpr;
use crate::ast::AssignmentOperator;
use crate::ast::AssignmentTarget;
use crate::ast::BinaryExpr;
use crate::ast::BinaryOperator;
use crate::ast::BlockStatement;
use crate::ast::BreakStatement;
use crate::ast::CallExpr;
use crate::ast::ContinueStatement;
use crate::ast::DataType;
use crate::ast::Expr;
use crate::ast::ForStatement;
use crate::ast::FunctionDeclaration;
use crate::ast::FunctionParameter;
use crate::ast::IdentifierExpr;
use crate::ast::IfStatement;
use crate::ast::IndexExpr;
use crate::ast::Program;
use crate::ast::RangeExpr;
use crate::ast::ReturnStatement;
use crate::ast::Statement;
use crate::ast::UnaryExpr;
use crate::ast::UnaryOperator;
use crate::ast::VariableDeclaration;
use crate::ast::WhileStatement;
use crate::builtins::BuiltInFunction;
use crate::compiler::CompileError;

use super::scope::ScopeStack;

// This pass is responsible for name resolution and type checking. It does not
// emit bytecode directly, but it resolves locals and function targets so code
// generation can remain simple.
#[derive(Default)]
pub struct SemanticAnalyzer {
	current_return_type: Option<DataType>,
	functions: BTreeMap<String, FunctionSignature>,
	locals: ScopeStack<LocalBinding>,
	loop_depth: usize,
	next_local_slot: u32,
	semantic_program: SemanticProgram,
}

#[derive(Clone)]
struct LocalBinding {
	data_type: DataType,
	is_const: bool,
	slot: u32,
}

#[derive(Clone)]
struct FunctionSignature {
	function_index: u32,
	parameter_types: Vec<DataType>,
	return_type: DataType,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SemanticProgram {
	built_in_call_targets: BTreeMap<usize, BuiltInFunction>,
	call_return_types: BTreeMap<usize, DataType>,
	call_targets: BTreeMap<usize, u32>,
	declaration_slots: BTreeMap<usize, u32>,
	declaration_types: BTreeMap<usize, DataType>,
	entry_point_function_index: Option<u32>,
	entry_point_position: Option<usize>,
	identifier_slots: BTreeMap<usize, u32>,
	iterator_slots: BTreeMap<usize, u32>,
}

impl SemanticProgram {
	pub fn built_in_call_target(&self, position: usize) -> Option<BuiltInFunction> {
		self.built_in_call_targets.get(&position).copied()
	}

	pub fn call_return_type(&self, position: usize) -> Option<DataType> {
		self.call_return_types.get(&position).cloned()
	}

	pub fn call_target(&self, position: usize) -> Option<u32> {
		self.call_targets.get(&position).copied()
	}

	pub fn declaration_slot(&self, position: usize) -> Option<u32> {
		self.declaration_slots.get(&position).copied()
	}

	pub fn declaration_type(&self, position: usize) -> Option<&DataType> {
		self.declaration_types.get(&position)
	}

	pub fn entry_point_function_index(&self) -> Option<u32> {
		self.entry_point_function_index
	}

	pub fn entry_point_position(&self) -> Option<usize> {
		self.entry_point_position
	}

	pub fn identifier_slot(&self, position: usize) -> Option<u32> {
		self.identifier_slots.get(&position).copied()
	}

	pub fn iterator_slot(&self, position: usize) -> Option<u32> {
		self.iterator_slots.get(&position).copied()
	}
}

impl SemanticAnalyzer {
	pub fn new() -> Self {
		Self {
			current_return_type: None,
			functions: BTreeMap::new(),
			locals: ScopeStack::default(),
			loop_depth: 0,
			next_local_slot: 0,
			semantic_program: SemanticProgram::default(),
		}
	}

	pub fn analyze_program(&mut self, program: &Program) -> Result<SemanticProgram, CompileError> {
		self.current_return_type = None;
		self.functions = BTreeMap::new();
		self.locals = ScopeStack::default();
		self.loop_depth = 0;
		self.next_local_slot = 0;
		self.semantic_program = SemanticProgram::default();

		self.collect_function_signatures(&program.functions)?;

		for function in &program.functions {
			self.validate_function_declaration(function)?;
		}

		self.enter_scope();

		for statement in &program.statements {
			self.validate_statement(statement)?;
		}

		if let Some(result) = &program.result {
			self.infer_expression_type(result)?;
		}

		self.exit_scope();

		Ok(self.semantic_program.clone())
	}

	pub fn analyze_standalone_program(&mut self, program: &Program) -> Result<SemanticProgram, CompileError> {
		let semantic_program = self.analyze_program(program)?;
		let Some(main_function) = program.functions.iter().find(|function| function.name == "Main") else {
			return Err(self.compile_error(
				0,
				String::from("Standalone Tablo programs must define `fn Main(args: [text]) int`."),
			));
		};

		self.validate_main_entry_point(program, main_function)?;

		let mut semantic_program = semantic_program;
		semantic_program.entry_point_function_index = self.functions.get("Main").map(|signature| signature.function_index);
		semantic_program.entry_point_position = Some(main_function.position);
		Ok(semantic_program)
	}

	pub fn validate_program(&mut self, program: &Program) -> Result<(), CompileError> {
		self.analyze_program(program).map(|_| ())
	}

	fn assignment_result_type(
		&self,
		operator: AssignmentOperator,
		target: &DataType,
		value: &DataType,
		position: usize,
	) -> Result<DataType, CompileError> {
		match operator {
			AssignmentOperator::Assign => {
				self.ensure_assignable(target, value, position)?;
				Ok(target.clone())
			}
			AssignmentOperator::AddAssign => {
				let result = self.binary_result_type(BinaryOperator::Add, target, value, position)?;
				self.ensure_assignable(target, &result, position)?;
				Ok(target.clone())
			}
			AssignmentOperator::DivideAssign => {
				let result = self.binary_result_type(BinaryOperator::Divide, target, value, position)?;
				self.ensure_assignable(target, &result, position)?;
				Ok(target.clone())
			}
			AssignmentOperator::ModuloAssign => {
				let result = self.binary_result_type(BinaryOperator::Modulo, target, value, position)?;
				self.ensure_assignable(target, &result, position)?;
				Ok(target.clone())
			}
			AssignmentOperator::MultiplyAssign => {
				let result = self.binary_result_type(BinaryOperator::Multiply, target, value, position)?;
				self.ensure_assignable(target, &result, position)?;
				Ok(target.clone())
			}
			AssignmentOperator::SubtractAssign => {
				let result = self.binary_result_type(BinaryOperator::Subtract, target, value, position)?;
				self.ensure_assignable(target, &result, position)?;
				Ok(target.clone())
			}
		}
	}

	fn binary_result_type(
		&self,
		operator: BinaryOperator,
		lhs: &DataType,
		rhs: &DataType,
		position: usize,
	) -> Result<DataType, CompileError> {
		match operator {
			BinaryOperator::Add => {
				if lhs == &DataType::Text || rhs == &DataType::Text {
					return Ok(DataType::Text);
				}

				match (lhs, rhs) {
					(DataType::Array(lhs_element), DataType::Array(rhs_element)) => {
						return Ok(DataType::Array(Box::new(
							self.merge_array_element_types(lhs_element, rhs_element, position)?,
						)));
					}
					(DataType::Array(element_type), DataType::EmptyArray)
					| (DataType::EmptyArray, DataType::Array(element_type)) => {
						return Ok(DataType::Array(element_type.clone()));
					}
					(DataType::EmptyArray, DataType::EmptyArray) => {
						return Ok(DataType::EmptyArray);
					}
					_ => {}
				}

				self.numeric_result_type(lhs, rhs, position)
			}
			BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor => {
				self.require_boolean_operands(operator, lhs, rhs, position)?;
				Ok(DataType::Bool)
			}
			BinaryOperator::Divide | BinaryOperator::Modulo | BinaryOperator::Multiply | BinaryOperator::Subtract => {
				self.numeric_result_type(lhs, rhs, position)
			}
			BinaryOperator::Equal | BinaryOperator::NotEqual => {
				self.require_equality_operands(lhs, rhs, position)?;
				Ok(DataType::Bool)
			}
			BinaryOperator::GreaterThan | BinaryOperator::GreaterThanOrEqual | BinaryOperator::LessThan | BinaryOperator::LessThanOrEqual => {
				self.require_ordering_operands(lhs, rhs, position)?;
				Ok(DataType::Bool)
			}
		}
	}

	fn block_guarantees_return(&self, block: &BlockStatement) -> bool {
		block.statements.iter().any(|statement| self.statement_guarantees_return(statement))
	}

	fn collect_function_signatures(&mut self, functions: &[FunctionDeclaration]) -> Result<(), CompileError> {
		for (index, function) in functions.iter().enumerate() {
			if self.functions.contains_key(&function.name) {
				return Err(self.compile_error(
					function.position,
					format!("Function `{}` is already declared in this scope.", function.name),
				));
			}

			let mut parameter_types = Vec::with_capacity(function.parameters.len());

			for parameter in &function.parameters {
				self.validate_non_void_data_type(
					&parameter.data_type,
					parameter.position,
					format!("Parameter `{}` cannot have type `{}`.", parameter.name, self.data_type_name(&parameter.data_type)),
				)?;
				parameter_types.push(parameter.data_type.clone());
			}

			if !matches!(function.return_type, DataType::Void) {
				self.validate_non_void_data_type(
					&function.return_type,
					function.position,
					format!(
						"Function `{}` cannot return `{}`.",
						function.name,
						self.data_type_name(&function.return_type),
					),
				)?;
			}

			self.functions.insert(function.name.clone(), FunctionSignature {
				function_index: index as u32,
				parameter_types,
				return_type: function.return_type.clone(),
			});
		}

		Ok(())
	}

	fn compile_error(&self, position: usize, message: impl Into<String>) -> CompileError {
		CompileError {
			message: message.into(),
			position,
		}
	}

	fn current_scope_contains(&self, name: &str) -> bool {
		self.locals.contains_in_current_scope(name)
	}

	fn data_type_name(&self, data_type: &DataType) -> String {
		match data_type {
			DataType::Array(element_type) => format!("[{}]", self.data_type_name(element_type)),
			DataType::Bool => String::from("bool"),
			DataType::Dec => String::from("dec"),
			DataType::EmptyArray => String::from("empty array"),
			DataType::Int => String::from("int"),
			DataType::Range(element_type) => format!("range<{}>", self.data_type_name(element_type)),
			DataType::Text => String::from("text"),
			DataType::Void => String::from("void"),
		}
	}

	fn declare_local(&mut self, name: String, local: LocalBinding) {
		self.locals.declare(name, local);
	}

	fn ensure_assignable(&self, target: &DataType, value: &DataType, position: usize) -> Result<(), CompileError> {
		if target == value || (target == &DataType::Dec && value == &DataType::Int) {
			return Ok(());
		}

		match (target, value) {
			(DataType::Array(target_element), DataType::Array(value_element)) => {
				self.ensure_assignable(target_element, value_element, position)
			}
			(DataType::Array(_), DataType::EmptyArray) => Ok(()),
			_ => Err(self.compile_error(
				position,
				format!(
					"Cannot assign a value of type `{}` to a variable of type `{}`.",
					self.data_type_name(value),
					self.data_type_name(target),
				),
			)),
		}
	}

	fn enter_scope(&mut self) {
		self.locals.enter_scope();
	}

	fn exit_scope(&mut self) {
		self.locals.exit_scope();
	}

	fn infer_array_literal_type(&mut self, elements: &[Expr], position: usize) -> Result<DataType, CompileError> {
		let mut element_type: Option<DataType> = None;

		for element in elements {
			let candidate = self.infer_expression_type(element)?;

			element_type = Some(match &element_type {
				None => candidate,
				Some(existing) => self.merge_array_element_types(existing, &candidate, position)?,
			});
		}

		match element_type {
			Some(element_type) => Ok(DataType::Array(Box::new(element_type))),
			None => Ok(DataType::EmptyArray),
		}
	}

	fn infer_built_in_call_type(&mut self, built_in: BuiltInFunction, arguments: &[Expr], position: usize) -> Result<DataType, CompileError> {
		if !built_in.supports_arity(arguments.len()) {
			return Err(self.compile_error(
				position,
				format!(
					"Built-in function `{}` expects 1 argument(s), found {}.",
					built_in.name(),
					arguments.len(),
				),
			));
		}

		let argument_types = arguments.iter()
			.map(|argument| self.infer_expression_type(argument))
			.collect::<Result<Vec<_>, _>>()?;

		let return_type = built_in.return_type(&argument_types).ok_or(self.compile_error(
			arguments.first().map_or(position, Expr::position),
			format!(
				"Built-in function `{}` does not accept an argument of type `{}`.",
				built_in.name(),
				self.data_type_name(argument_types.first().unwrap()),
			),
		))?;

		self.semantic_program.built_in_call_targets.insert(position, built_in);
		self.semantic_program.call_return_types.insert(position, return_type.clone());
		Ok(return_type)
	}

	fn infer_expression_type(&mut self, expression: &Expr) -> Result<DataType, CompileError> {
		match expression {
			Expr::Array(array) => self.infer_array_literal_type(array.elements.as_slice(), array.position),
			Expr::Assignment(AssignmentExpr { operator, target, value, .. }) => {
				match target {
					AssignmentTarget::Identifier(target) => {
						let local = self.lookup_local(&target.name).ok_or(self.compile_error(
							target.position,
							format!("Variable `{}` is not declared in this scope.", target.name),
						))?;
						self.semantic_program.identifier_slots.insert(target.position, local.slot);

						if local.is_const {
							let operation = match operator {
								AssignmentOperator::Assign => "=",
								AssignmentOperator::AddAssign => "+=",
								AssignmentOperator::DivideAssign => "/=",
								AssignmentOperator::ModuloAssign => "%=",
								AssignmentOperator::MultiplyAssign => "*=",
								AssignmentOperator::SubtractAssign => "-=",
							};

							return Err(self.compile_error(
								expression.position(),
								format!("Constant `{}` cannot be assigned using `{operation}`.", target.name),
							));
						}

						let value_type = self.infer_expression_type(value)?;
						self.assignment_result_type(*operator, &local.data_type, &value_type, expression.position())
					}
					AssignmentTarget::Index(target) => {
						let local = self.lookup_local(&target.array.name).ok_or(self.compile_error(
							target.array.position,
							format!("Variable `{}` is not declared in this scope.", target.array.name),
						))?;
						self.semantic_program.identifier_slots.insert(target.array.position, local.slot);

						if local.is_const {
							let operation = match operator {
								AssignmentOperator::Assign => "=",
								AssignmentOperator::AddAssign => "+=",
								AssignmentOperator::DivideAssign => "/=",
								AssignmentOperator::ModuloAssign => "%=",
								AssignmentOperator::MultiplyAssign => "*=",
								AssignmentOperator::SubtractAssign => "-=",
							};

							return Err(self.compile_error(
								expression.position(),
								format!("Constant `{}` cannot be assigned using `{operation}`.", target.array.name),
							));
						}

						let index_type = self.infer_expression_type(&target.index)?;

						if index_type != DataType::Int {
							return Err(self.compile_error(
								target.index.position(),
								format!("Array index must be of type `int`, found `{}`.", self.data_type_name(&index_type)),
							));
						}

						let element_type = match &local.data_type {
							DataType::Array(element_type) => element_type.as_ref(),
							other => {
								return Err(self.compile_error(
									target.array.position,
									format!("Indexed assignment requires an array operand, found `{}`.", self.data_type_name(other)),
								));
							}
						};

						let value_type = self.infer_expression_type(value)?;
						self.assignment_result_type(*operator, element_type, &value_type, expression.position())
					}
				}
			}
			Expr::Binary(BinaryExpr { left, operator, right, .. }) => {
				let left_type = self.infer_expression_type(left)?;
				let right_type = self.infer_expression_type(right)?;
				self.binary_result_type(*operator, &left_type, &right_type, expression.position())
			}
			Expr::Boolean(_) => Ok(DataType::Bool),
			Expr::Call(CallExpr { arguments, callee, .. }) => {
				if let Some(signature) = self.functions.get(&callee.name).cloned() {
					if arguments.len() != signature.parameter_types.len() {
						return Err(self.compile_error(
							expression.position(),
							format!(
								"Function `{}` expects {} argument(s), found {}.",
								callee.name,
								signature.parameter_types.len(),
								arguments.len(),
							),
						));
					}

					for (argument, parameter_type) in arguments.iter().zip(signature.parameter_types.iter()) {
						let argument_type = self.infer_expression_type(argument)?;
						self.ensure_assignable(parameter_type, &argument_type, argument.position())?;
					}

					self.semantic_program.call_targets.insert(expression.position(), signature.function_index);
					self.semantic_program.call_return_types.insert(expression.position(), signature.return_type.clone());
					Ok(signature.return_type)
				}
				else if let Some(built_in) = BuiltInFunction::from_name(&callee.name) {
					self.infer_built_in_call_type(built_in, arguments, expression.position())
				}
				else {
					Err(self.compile_error(
						callee.position,
						format!("Function `{}` is not declared in this scope.", callee.name),
					))
				}
			}
			Expr::Decimal(_) => Ok(DataType::Dec),
			Expr::Identifier(IdentifierExpr { name, .. }) => {
				let local = self.lookup_local(name).ok_or(self.compile_error(
					expression.position(),
					format!("Variable `{name}` is not declared in this scope."),
				))?;
				self.semantic_program.identifier_slots.insert(expression.position(), local.slot);
				Ok(local.data_type)
			}
			Expr::Index(IndexExpr { array, index, .. }) => {
				let array_type = self.infer_expression_type(array)?;
				let index_type = self.infer_expression_type(index)?;

				if index_type != DataType::Int {
					return Err(self.compile_error(
						index.position(),
						format!("Array index must be of type `int`, found `{}`.", self.data_type_name(&index_type)),
					));
				}

				match array_type {
					DataType::Array(element_type) => Ok(*element_type),
					DataType::EmptyArray => Err(self.compile_error(
						array.position(),
						String::from("Cannot index an empty array literal without a known element type."),
					)),
					other => Err(self.compile_error(
						array.position(),
						format!("Array indexing requires an array operand, found `{}`.", self.data_type_name(&other)),
					)),
				}
			}
			Expr::Integer(_) => Ok(DataType::Int),
			Expr::Range(RangeExpr { start, step, end, position }) => {
				let start_type = self.infer_expression_type(start)?;
				let end_type = self.infer_expression_type(end)?;

				if !self.is_numeric_type(&start_type) || !self.is_numeric_type(&end_type) {
					return Err(self.compile_error(
						*position,
						format!(
							"Range bounds must be numeric, found `{}` and `{}`.",
							self.data_type_name(&start_type),
							self.data_type_name(&end_type),
						),
					));
				}

				let element_type = if let Some(step) = step {
					let step_type = self.infer_expression_type(step)?;

					if !self.is_numeric_type(&step_type) {
						return Err(self.compile_error(
							step.position(),
							format!("Range step must be numeric, found `{}`.", self.data_type_name(&step_type)),
						));
					}

					self.merge_array_element_types(
						&self.merge_array_element_types(&start_type, &end_type, *position)?,
						&step_type,
						*position,
					)?
				}
				else {
					self.merge_array_element_types(&start_type, &end_type, *position)?
				};

				Ok(DataType::Range(Box::new(element_type)))
			}
			Expr::Text(_) => Ok(DataType::Text),
			Expr::Unary(UnaryExpr { operand, operator, .. }) => {
				let operand_type = self.infer_expression_type(operand)?;

				match operator {
					UnaryOperator::Negate => {
						if self.is_numeric_type(&operand_type) {
							Ok(operand_type)
						}
						else {
							Err(self.compile_error(
								expression.position(),
								format!("Unary `-` requires a numeric operand, found `{}`.", self.data_type_name(&operand_type)),
							))
						}
					}
					UnaryOperator::Not => {
						if operand_type == DataType::Bool {
							Ok(DataType::Bool)
						}
						else {
							Err(self.compile_error(
								expression.position(),
								format!("Unary `not` requires a `bool` operand, found `{}`.", self.data_type_name(&operand_type)),
							))
						}
					}
				}
			}
		}
	}

	fn is_numeric_type(&self, data_type: &DataType) -> bool {
		matches!(data_type, DataType::Dec | DataType::Int)
	}

	fn lookup_local(&self, name: &str) -> Option<LocalBinding> {
		self.locals.lookup(name).cloned()
	}

	fn merge_array_element_types(&self, lhs: &DataType, rhs: &DataType, position: usize) -> Result<DataType, CompileError> {
		if lhs == rhs {
			return Ok(lhs.clone());
		}

		if self.is_numeric_type(lhs) && self.is_numeric_type(rhs) {
			return Ok(if lhs == &DataType::Int && rhs == &DataType::Int {
				DataType::Int
			}
			else {
				DataType::Dec
			});
		}

		if let (DataType::Array(lhs_element), DataType::Array(rhs_element)) = (lhs, rhs) {
			return Ok(DataType::Array(Box::new(self.merge_array_element_types(lhs_element, rhs_element, position)?)));
		}

		Err(self.compile_error(
			position,
			format!(
				"Array literal elements must have compatible types, found `{}` and `{}`.",
				self.data_type_name(lhs),
				self.data_type_name(rhs),
			),
		))
	}

	fn numeric_result_type(&self, lhs: &DataType, rhs: &DataType, position: usize) -> Result<DataType, CompileError> {
		if !self.is_numeric_type(lhs) || !self.is_numeric_type(rhs) {
			return Err(self.compile_error(
				position,
				format!(
					"Expected numeric operands, found `{}` and `{}`.",
					self.data_type_name(lhs),
					self.data_type_name(rhs),
				),
			));
		}

		if lhs == &DataType::Int && rhs == &DataType::Int {
			Ok(DataType::Int)
		}
		else {
			Ok(DataType::Dec)
		}
	}

	fn operator_name(&self, operator: BinaryOperator) -> &'static str {
		match operator {
			BinaryOperator::Add => "+",
			BinaryOperator::And => "and",
			BinaryOperator::Divide => "/",
			BinaryOperator::Equal => "==",
			BinaryOperator::GreaterThan => ">",
			BinaryOperator::GreaterThanOrEqual => ">=",
			BinaryOperator::LessThan => "<",
			BinaryOperator::LessThanOrEqual => "<=",
			BinaryOperator::Modulo => "%",
			BinaryOperator::Multiply => "*",
			BinaryOperator::NotEqual => "!=",
			BinaryOperator::Or => "or",
			BinaryOperator::Subtract => "-",
			BinaryOperator::Xor => "xor",
		}
	}

	fn require_boolean_operands(&self, operator: BinaryOperator, lhs: &DataType, rhs: &DataType, position: usize) -> Result<(), CompileError> {
		if lhs == &DataType::Bool && rhs == &DataType::Bool {
			return Ok(());
		}

		Err(self.compile_error(
			position,
			format!(
				"Operator `{}` requires `bool` operands, found `{}` and `{}`.",
				self.operator_name(operator),
				self.data_type_name(lhs),
				self.data_type_name(rhs),
			),
		))
	}

	fn require_equality_operands(&self, lhs: &DataType, rhs: &DataType, position: usize) -> Result<(), CompileError> {
		if lhs == rhs || (self.is_numeric_type(lhs) && self.is_numeric_type(rhs)) {
			return Ok(());
		}

		match (lhs, rhs) {
			(DataType::Array(lhs_element), DataType::Array(rhs_element)) => self.require_equality_operands(lhs_element, rhs_element, position),
			(DataType::Array(_), DataType::EmptyArray)
			| (DataType::EmptyArray, DataType::Array(_))
			| (DataType::EmptyArray, DataType::EmptyArray) => Ok(()),
			(DataType::Range(_), _) | (_, DataType::Range(_)) => Err(self.compile_error(
				position,
				format!(
					"Equality comparison is not supported between `{}` and `{}`.",
					self.data_type_name(lhs),
					self.data_type_name(rhs),
				),
			)),
			_ => Err(self.compile_error(
				position,
				format!(
					"Equality comparison is not supported between `{}` and `{}`.",
					self.data_type_name(lhs),
					self.data_type_name(rhs),
				),
			)),
		}
	}

	fn require_ordering_operands(&self, lhs: &DataType, rhs: &DataType, position: usize) -> Result<(), CompileError> {
		if (lhs == &DataType::Text && rhs == &DataType::Text) || (self.is_numeric_type(lhs) && self.is_numeric_type(rhs)) {
			return Ok(());
		}

		Err(self.compile_error(
			position,
			format!(
				"Ordering comparison is not supported between `{}` and `{}`.",
				self.data_type_name(lhs),
				self.data_type_name(rhs),
			),
		))
	}

	fn statement_guarantees_return(&self, statement: &Statement) -> bool {
		match statement {
			Statement::Block(block) => self.block_guarantees_return(block),
			Statement::Break(_) | Statement::Continue(_) => false,
			Statement::If(if_statement) => {
				self.block_guarantees_return(&if_statement.then_branch)
					&& if_statement.else_branch.as_ref().is_some_and(|else_branch| self.statement_guarantees_return(else_branch))
			}
			Statement::Return(_) => true,
			Statement::Expression(_) | Statement::For(_) | Statement::VariableDeclaration(_) | Statement::While(_) => false,
		}
	}

	fn validate_function_declaration(&mut self, function: &FunctionDeclaration) -> Result<(), CompileError> {
		let saved_locals = std::mem::take(&mut self.locals);
		let saved_loop_depth = self.loop_depth;
		let saved_next_local_slot = self.next_local_slot;
		let saved_return_type = self.current_return_type.clone();

		self.locals = ScopeStack::default();
		self.loop_depth = 0;
		self.next_local_slot = 0;
		self.current_return_type = Some(function.return_type.clone());
		self.enter_scope();

		for parameter in &function.parameters {
			self.validate_function_parameter(parameter)?;
		}

		self.validate_statement(&Statement::Block(function.body.clone()))?;

		if function.return_type != DataType::Void && !self.block_guarantees_return(&function.body) {
			return Err(self.compile_error(
				function.position,
				format!(
					"Function `{}` must return a value of type `{}` on all paths.",
					function.name,
					self.data_type_name(&function.return_type),
				),
			));
		}

		self.exit_scope();
		self.locals = saved_locals;
		self.loop_depth = saved_loop_depth;
		self.next_local_slot = saved_next_local_slot;
		self.current_return_type = saved_return_type;

		Ok(())
	}

	fn validate_function_parameter(&mut self, parameter: &FunctionParameter) -> Result<(), CompileError> {
		self.validate_non_void_data_type(
			&parameter.data_type,
			parameter.position,
			format!("Parameter `{}` cannot have type `{}`.", parameter.name, self.data_type_name(&parameter.data_type)),
		)?;

		if self.current_scope_contains(&parameter.name) {
			return Err(self.compile_error(
				parameter.position,
				format!("Parameter `{}` is already declared in this scope.", parameter.name),
			));
		}

		let slot = self.next_local_slot;
		self.next_local_slot += 1;
		self.semantic_program.declaration_slots.insert(parameter.position, slot);
		self.semantic_program.declaration_types.insert(parameter.position, parameter.data_type.clone());
		self.declare_local(parameter.name.clone(), LocalBinding {
			data_type: parameter.data_type.clone(),
			is_const: false,
			slot,
		});

		Ok(())
	}

	fn validate_main_entry_point(&self, program: &Program, main_function: &FunctionDeclaration) -> Result<(), CompileError> {
		if let Some(statement) = program.statements.first() {
			return Err(self.compile_error(
				statement_position(statement),
				String::from("Top-level executable statements are not permitted when `Main` is defined."),
			));
		}

		if let Some(result) = &program.result {
			return Err(self.compile_error(
				result.position(),
				String::from("Top-level executable statements are not permitted when `Main` is defined."),
			));
		}

		if main_function.parameters.len() != 1
			|| main_function.parameters[0].name != "args"
			|| main_function.parameters[0].data_type != DataType::Array(Box::new(DataType::Text))
			|| main_function.return_type != DataType::Int
		{
			return Err(self.compile_error(
				main_function.position,
				String::from("Entry-point function `Main` must have the exact signature `fn Main(args: [text]) int`."),
			));
		}

		Ok(())
	}

	fn validate_non_void_data_type(&self, data_type: &DataType, position: usize, message: String) -> Result<(), CompileError> {
		match data_type {
			DataType::Void | DataType::EmptyArray => Err(self.compile_error(position, message)),
			DataType::Array(element_type) => self.validate_non_void_data_type(element_type, position, message),
			_ => Ok(()),
		}
	}

	fn validate_statement(&mut self, statement: &Statement) -> Result<(), CompileError> {
		match statement {
			Statement::Block(BlockStatement { statements, .. }) => {
				self.enter_scope();

				for statement in statements {
					self.validate_statement(statement)?;
				}

				self.exit_scope();
				Ok(())
			}
			Statement::Break(BreakStatement { position }) => {
				if self.loop_depth == 0 {
					return Err(self.compile_error(
						*position,
						String::from("`break` may only be used inside a `while` or `for` loop."),
					));
				}

				Ok(())
			}
			Statement::Continue(ContinueStatement { position }) => {
				if self.loop_depth == 0 {
					return Err(self.compile_error(
						*position,
						String::from("`continue` may only be used inside a `while` or `for` loop."),
					));
				}

				Ok(())
			}
			Statement::Expression(expression) => {
				self.infer_expression_type(expression)?;
				Ok(())
			}
			Statement::For(ForStatement { body, iterable, position, variable }) => {
				let iterable_type = self.infer_expression_type(iterable)?;
				let variable_type = match iterable_type {
					DataType::Array(element_type) => *element_type,
					DataType::Range(element_type) => *element_type,
					DataType::EmptyArray => {
						return Err(self.compile_error(
							iterable.position().max(*position),
							String::from("`for` iterable must have a known element type."),
						));
					}
					other => {
						return Err(self.compile_error(
							iterable.position().max(*position),
							format!("`for` iterable must be an array or range, found `{}`.", self.data_type_name(&other)),
						));
					}
				};

				self.enter_scope();

				if self.current_scope_contains(&variable.name) {
					self.exit_scope();
					return Err(self.compile_error(
						variable.position,
						format!("Variable `{}` is already declared in this scope.", variable.name),
					));
				}

				let loop_variable_slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.declaration_slots.insert(variable.position, loop_variable_slot);
				self.semantic_program.declaration_types.insert(variable.position, variable_type.clone());
				self.declare_local(variable.name.clone(), LocalBinding {
					data_type: variable_type,
					is_const: false,
					slot: loop_variable_slot,
				});

				let iterator_slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.iterator_slots.insert(*position, iterator_slot);

				self.loop_depth += 1;
				let validation_result = self.validate_statement(&Statement::Block(body.clone()));
				self.loop_depth -= 1;
				self.exit_scope();
				validation_result
			}
			Statement::If(IfStatement { condition, else_branch, position, then_branch }) => {
				let condition_type = self.infer_expression_type(condition)?;

				if condition_type != DataType::Bool {
					return Err(self.compile_error(
						condition.position().max(*position),
						format!("`if` condition must be of type `bool`, found `{}`.", self.data_type_name(&condition_type)),
					));
				}

				self.validate_statement(&Statement::Block(then_branch.clone()))?;

				if let Some(else_branch) = else_branch {
					self.validate_statement(else_branch)?;
				}

				Ok(())
			}
			Statement::Return(ReturnStatement { position, value }) => {
				let return_type = self.current_return_type.clone().ok_or(self.compile_error(
					*position,
					String::from("`return` may only be used inside a function body."),
				))?;

				match (return_type, value) {
					(DataType::Void, None) => Ok(()),
					(DataType::Void, Some(value)) => Err(self.compile_error(
						value.position(),
						String::from("A `void` function cannot return a value."),
					)),
					(expected_type, Some(value)) => {
						let value_type = self.infer_expression_type(value)?;
						self.ensure_assignable(&expected_type, &value_type, value.position())
					}
					(expected_type, None) => Err(self.compile_error(
						*position,
						format!("Function must return a value of type `{}`.", self.data_type_name(&expected_type)),
					)),
				}
			}
			Statement::VariableDeclaration(VariableDeclaration { data_type, initial_value, is_const, name, position }) => {
				self.validate_non_void_data_type(
					data_type,
					*position,
					format!(
						"{} `{name}` cannot have type `{}`.",
						if *is_const { "Constant" } else { "Variable" },
						self.data_type_name(data_type),
					),
				)?;

				if self.current_scope_contains(name) {
					return Err(self.compile_error(
						*position,
						format!("Variable `{name}` is already declared in this scope."),
					));
				}

				let initial_value = initial_value.as_ref().ok_or(self.compile_error(
					*position,
					if *is_const {
						format!("Constant `{name}` must currently have an initializer.")
					}
					else {
						format!("Variable `{name}` must currently have an initializer.")
					},
				))?;
				let initial_type = self.infer_expression_type(initial_value)?;
				self.ensure_assignable(data_type, &initial_type, initial_value.position())?;

				let slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.declaration_slots.insert(*position, slot);
				self.semantic_program.declaration_types.insert(*position, data_type.clone());
				self.declare_local(name.clone(), LocalBinding {
					data_type: data_type.clone(),
					is_const: *is_const,
					slot,
				});

				Ok(())
			}
			Statement::While(WhileStatement { body, condition, position }) => {
				let condition_type = self.infer_expression_type(condition)?;

				if condition_type != DataType::Bool {
					return Err(self.compile_error(
						condition.position().max(*position),
						format!("`while` condition must be of type `bool`, found `{}`.", self.data_type_name(&condition_type)),
					));
				}

				self.loop_depth += 1;
				self.validate_statement(&Statement::Block(body.clone()))?;
				self.loop_depth -= 1;
				Ok(())
			}
		}
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
