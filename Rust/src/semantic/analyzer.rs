use crate::ast::AssignmentExpr;
use crate::ast::AssignmentOperator;
use crate::ast::BinaryExpr;
use crate::ast::BinaryOperator;
use crate::ast::BlockStatement;
use crate::ast::DataType;
use crate::ast::Expr;
use crate::ast::IdentifierExpr;
use crate::ast::IfStatement;
use crate::ast::Program;
use crate::ast::Statement;
use crate::ast::UnaryExpr;
use crate::ast::UnaryOperator;
use crate::ast::VariableDeclaration;
use crate::compiler::CompileError;

use super::scope::ScopeStack;

// This pass is responsible for name resolution and type checking. It does not
// yet produce an annotated intermediate form, but it establishes a clear
// separation between semantic validation and bytecode emission.
#[derive(Default)]
pub struct SemanticAnalyzer {
	locals: ScopeStack<LocalBinding>,
}

#[derive(Clone, Copy)]
struct LocalBinding {
	data_type: DataType,
	is_const: bool,
}

impl SemanticAnalyzer {
	pub fn new() -> Self {
		Self {
			locals: ScopeStack::default(),
		}
	}

	pub fn validate_program(&mut self, program: &Program) -> Result<(), CompileError> {
		self.locals = ScopeStack::default();
		self.enter_scope();

		for statement in &program.statements {
			self.validate_statement(statement)?;
		}

		if let Some(result) = &program.result {
			self.infer_expression_type(result)?;
		}

		self.exit_scope();
		Ok(())
	}

	fn assignment_result_type(&self, operator: AssignmentOperator, target: DataType, value: DataType, position: usize) -> Result<DataType, CompileError> {
		match operator {
			AssignmentOperator::AddAssign => {
				let result = self.binary_result_type(BinaryOperator::Add, target, value, position)?;
				self.ensure_assignable(target, result, position)?;
				Ok(target)
			}
			AssignmentOperator::Assign => {
				self.ensure_assignable(target, value, position)?;
				Ok(target)
			}
			AssignmentOperator::DivideAssign => {
				let result = self.binary_result_type(BinaryOperator::Divide, target, value, position)?;
				self.ensure_assignable(target, result, position)?;
				Ok(target)
			}
			AssignmentOperator::ModuloAssign => {
				let result = self.binary_result_type(BinaryOperator::Modulo, target, value, position)?;
				self.ensure_assignable(target, result, position)?;
				Ok(target)
			}
			AssignmentOperator::MultiplyAssign => {
				let result = self.binary_result_type(BinaryOperator::Multiply, target, value, position)?;
				self.ensure_assignable(target, result, position)?;
				Ok(target)
			}
			AssignmentOperator::SubtractAssign => {
				let result = self.binary_result_type(BinaryOperator::Subtract, target, value, position)?;
				self.ensure_assignable(target, result, position)?;
				Ok(target)
			}
		}
	}

	fn binary_result_type(&self, operator: BinaryOperator, lhs: DataType, rhs: DataType, position: usize) -> Result<DataType, CompileError> {
		match operator {
			BinaryOperator::Add => {
				if lhs == DataType::Text || rhs == DataType::Text {
					return Ok(DataType::Text);
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

	fn compile_error(&self, position: usize, message: impl Into<String>) -> CompileError {
		CompileError {
			message: message.into(),
			position,
		}
	}

	fn current_scope_contains(&self, name: &str) -> bool {
		self.locals.contains_in_current_scope(name)
	}

	fn data_type_name(&self, data_type: DataType) -> &'static str {
		match data_type {
			DataType::Bool => "bool",
			DataType::Dec => "dec",
			DataType::Int => "int",
			DataType::Text => "text",
		}
	}

	fn declare_local(&mut self, name: String, local: LocalBinding) {
		self.locals.declare(name, local);
	}

	fn ensure_assignable(&self, target: DataType, value: DataType, position: usize) -> Result<(), CompileError> {
		if target == value || (target == DataType::Dec && value == DataType::Int) {
			return Ok(());
		}

		Err(self.compile_error(
			position,
			format!(
				"Cannot assign a value of type `{}` to a variable of type `{}`.",
				self.data_type_name(value),
				self.data_type_name(target),
			),
		))
	}

	fn enter_scope(&mut self) {
		self.locals.enter_scope();
	}

	fn exit_scope(&mut self) {
		self.locals.exit_scope();
	}

	fn infer_expression_type(&self, expression: &Expr) -> Result<DataType, CompileError> {
		match expression {
			Expr::Assignment(AssignmentExpr { operator, target, value, .. }) => {
				let local = self.lookup_local(&target.name).ok_or(self.compile_error(
					target.position,
					format!("Variable `{}` is not declared in this scope.", target.name),
				))?;

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
				self.assignment_result_type(*operator, local.data_type, value_type, expression.position())
			}
			Expr::Binary(BinaryExpr { left, operator, right, .. }) => {
				let left_type = self.infer_expression_type(left)?;
				let right_type = self.infer_expression_type(right)?;
				self.binary_result_type(*operator, left_type, right_type, expression.position())
			}
			Expr::Boolean(_) => Ok(DataType::Bool),
			Expr::Decimal(_) => Ok(DataType::Dec),
			Expr::Identifier(IdentifierExpr { name, .. }) => {
				self.lookup_local(name).map(|local| local.data_type).ok_or(self.compile_error(
					expression.position(),
					format!("Variable `{name}` is not declared in this scope."),
				))
			}
			Expr::Integer(_) => Ok(DataType::Int),
			Expr::Text(_) => Ok(DataType::Text),
			Expr::Unary(UnaryExpr { operand, operator, .. }) => {
				let operand_type = self.infer_expression_type(operand)?;

				match operator {
					UnaryOperator::Negate => {
						if self.is_numeric_type(operand_type) {
							Ok(operand_type)
						}
						else {
							Err(self.compile_error(
								expression.position(),
								format!("Unary `-` requires a numeric operand, found `{}`.", self.data_type_name(operand_type)),
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
								format!("Unary `not` requires a `bool` operand, found `{}`.", self.data_type_name(operand_type)),
							))
						}
					}
				}
			}
		}
	}

	fn is_numeric_type(&self, data_type: DataType) -> bool {
		matches!(data_type, DataType::Dec | DataType::Int)
	}

	fn lookup_local(&self, name: &str) -> Option<LocalBinding> {
		self.locals.lookup(name).copied()
	}

	fn numeric_result_type(&self, lhs: DataType, rhs: DataType, position: usize) -> Result<DataType, CompileError> {
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

		if lhs == DataType::Int && rhs == DataType::Int {
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

	fn require_boolean_operands(&self, operator: BinaryOperator, lhs: DataType, rhs: DataType, position: usize) -> Result<(), CompileError> {
		if lhs == DataType::Bool && rhs == DataType::Bool {
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

	fn require_equality_operands(&self, lhs: DataType, rhs: DataType, position: usize) -> Result<(), CompileError> {
		if lhs == rhs || (self.is_numeric_type(lhs) && self.is_numeric_type(rhs)) {
			return Ok(());
		}

		Err(self.compile_error(
			position,
			format!(
				"Equality comparison is not supported between `{}` and `{}`.",
				self.data_type_name(lhs),
				self.data_type_name(rhs),
			),
		))
	}

	fn require_ordering_operands(&self, lhs: DataType, rhs: DataType, position: usize) -> Result<(), CompileError> {
		if (lhs == DataType::Text && rhs == DataType::Text) || (self.is_numeric_type(lhs) && self.is_numeric_type(rhs)) {
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
			Statement::Expression(expression) => {
				self.infer_expression_type(expression)?;
				Ok(())
			}
			Statement::If(IfStatement {
				condition,
				else_branch,
				position,
				then_branch,
			}) => {
				let condition_type = self.infer_expression_type(condition)?;

				if condition_type != DataType::Bool {
					return Err(self.compile_error(
						condition.position().max(*position),
						format!("`if` condition must be of type `bool`, found `{}`.", self.data_type_name(condition_type)),
					));
				}

				self.validate_statement(&Statement::Block(then_branch.clone()))?;

				if let Some(else_branch) = else_branch {
					self.validate_statement(else_branch)?;
				}

				Ok(())
			}
			Statement::VariableDeclaration(VariableDeclaration { data_type, initial_value, is_const, name, position }) => {
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
				self.ensure_assignable(*data_type, initial_type, initial_value.position())?;
				self.declare_local(name.clone(), LocalBinding {
					data_type: *data_type,
					is_const: *is_const,
				});

				Ok(())
			}
		}
	}
}
