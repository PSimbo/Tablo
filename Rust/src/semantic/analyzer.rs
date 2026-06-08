use std::collections::BTreeMap;

use crate::ast::AssignmentExpr;
use crate::ast::AssignmentOperator;
use crate::ast::AssignmentTarget;
use crate::ast::BinaryExpr;
use crate::ast::BinaryOperator;
use crate::ast::BlockStatement;
use crate::ast::BreakStatement;
use crate::ast::CallArgument;
use crate::ast::CallExpr;
use crate::ast::ContinueStatement;
use crate::ast::CountExpr;
use crate::ast::DataType;
use crate::ast::Expr;
use crate::ast::FieldAccessExpr;
use crate::ast::FindExpr;
use crate::ast::ForRecordStatement;
use crate::ast::ForStatement;
use crate::ast::FunctionDeclaration;
use crate::ast::FunctionParameter;
use crate::ast::IdentifierExpr;
use crate::ast::IfStatement;
use crate::ast::IndexExpr;
use crate::ast::ObjectConstructionExpr;
use crate::ast::ObjectDeclaration;
use crate::ast::Program;
use crate::ast::RangeExpr;
use crate::ast::RecordPointerDeclaration;
use crate::ast::RecordPointerType;
use crate::ast::ReturnStatement;
use crate::ast::Statement;
use crate::ast::TableReference;
use crate::ast::UnaryExpr;
use crate::ast::UnaryOperator;
use crate::ast::VariableDeclaration;
use crate::ast::WhileStatement;
use crate::ast::WithDeclaration;
use crate::builtins::BuiltInFunction;
use crate::compiler::CompileError;
use crate::query::LoweredBackendQuery;
use crate::query::QueryBinaryExpr;
use crate::query::QueryBinaryOperator;
use crate::query::QueryColumnReference;
use crate::query::QueryCountPlan;
use crate::query::QueryExpr;
use crate::query::QueryFindPlan;
use crate::query::QueryForPlan;
use crate::query::QueryLiteral;
use crate::query::QueryLoweringError;
use crate::query::QueryOrderByItem;
use crate::query::QueryParameter;
use crate::query::QueryResultColumn;
use crate::query::QueryUnaryExpr;
use crate::query::QueryUnaryOperator;
use crate::schema::SchemaCatalog;
use crate::schema::DatabaseBackend;
use crate::schema::SchemaDataType;
use crate::schema::SchemaError;

use super::scope::ScopeStack;

// This pass is responsible for name resolution and type checking. It does not
// emit bytecode directly, but it resolves locals and function targets so code
// generation can remain simple.
#[derive(Default)]
pub struct SemanticAnalyzer {
	current_return_type: Option<DataType>,
	current_schema_catalog: Option<SchemaCatalog>,
	functions: ScopeStack<FunctionSignature>,
	locals: ScopeStack<LocalBinding>,
	loop_depth: usize,
	next_function_index: u32,
	next_local_slot: u32,
	semantic_program: SemanticProgram,
}

#[derive(Clone)]
struct FunctionParameterSignature {
	data_type: DataType,
	is_by_ref: bool,
	name: String,
}

#[derive(Clone)]
struct FunctionSignature {
	function_index: u32,
	parameters: Vec<FunctionParameterSignature>,
	return_type: DataType,
}

#[derive(Clone)]
struct LocalBinding {
	data_type: DataType,
	is_const: bool,
	slot: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedTableReference {
	pub database_name: String,
	pub schema_name: String,
	pub table_name: String,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SemanticProgram {
	active_databases: Vec<String>,
	built_in_call_targets: BTreeMap<usize, BuiltInFunction>,
	call_argument_reference_slots: BTreeMap<usize, Vec<Option<u32>>>,
	call_return_types: BTreeMap<usize, DataType>,
	call_targets: BTreeMap<usize, u32>,
	compiled_count_queries: BTreeMap<usize, LoweredBackendQuery>,
	compiled_find_queries: BTreeMap<usize, LoweredBackendQuery>,
	compiled_for_record_queries: BTreeMap<usize, LoweredBackendQuery>,
	declaration_slots: BTreeMap<usize, u32>,
	declaration_types: BTreeMap<usize, DataType>,
	entry_point_function_index: Option<u32>,
	entry_point_position: Option<usize>,
	function_declaration_targets: BTreeMap<usize, u32>,
	identifier_slots: BTreeMap<usize, u32>,
	iterator_slots: BTreeMap<usize, u32>,
	lowered_count_queries: BTreeMap<usize, QueryCountPlan>,
	lowered_find_queries: BTreeMap<usize, QueryFindPlan>,
	lowered_for_record_queries: BTreeMap<usize, QueryForPlan>,
	object_declarations: BTreeMap<String, ObjectDeclaration>,
	resolved_tables: BTreeMap<usize, ResolvedTableReference>,
}

impl SemanticProgram {
	pub fn active_databases(&self) -> &[String] {
		&self.active_databases
	}

	pub fn built_in_call_target(&self, position: usize) -> Option<BuiltInFunction> {
		self.built_in_call_targets.get(&position).copied()
	}

	pub fn call_argument_reference_slots(&self, position: usize) -> Option<&[Option<u32>]> {
		self.call_argument_reference_slots.get(&position).map(Vec::as_slice)
	}

	pub fn call_return_type(&self, position: usize) -> Option<DataType> {
		self.call_return_types.get(&position).cloned()
	}

	pub fn call_target(&self, position: usize) -> Option<u32> {
		self.call_targets.get(&position).copied()
	}

	pub fn compiled_count_query(&self, position: usize) -> Option<&LoweredBackendQuery> {
		self.compiled_count_queries.get(&position)
	}

	pub fn compiled_find_query(&self, position: usize) -> Option<&LoweredBackendQuery> {
		self.compiled_find_queries.get(&position)
	}

	pub fn compiled_for_record_query(&self, position: usize) -> Option<&LoweredBackendQuery> {
		self.compiled_for_record_queries.get(&position)
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

	pub fn function_declaration_target(&self, position: usize) -> Option<u32> {
		self.function_declaration_targets.get(&position).copied()
	}

	pub fn identifier_slot(&self, position: usize) -> Option<u32> {
		self.identifier_slots.get(&position).copied()
	}

	pub fn iterator_slot(&self, position: usize) -> Option<u32> {
		self.iterator_slots.get(&position).copied()
	}

	pub fn lowered_count_query(&self, position: usize) -> Option<&QueryCountPlan> {
		self.lowered_count_queries.get(&position)
	}

	pub fn lowered_find_query(&self, position: usize) -> Option<&QueryFindPlan> {
		self.lowered_find_queries.get(&position)
	}

	pub fn lowered_for_record_query(&self, position: usize) -> Option<&QueryForPlan> {
		self.lowered_for_record_queries.get(&position)
	}

	pub fn object_declaration(&self, name: &str) -> Option<&ObjectDeclaration> {
		self.object_declarations.get(name)
	}

	pub fn resolved_table(&self, position: usize) -> Option<&ResolvedTableReference> {
		self.resolved_tables.get(&position)
	}
}

impl SemanticAnalyzer {
	pub fn new() -> Self {
		Self {
			current_return_type: None,
			current_schema_catalog: None,
			functions: ScopeStack::default(),
			locals: ScopeStack::default(),
			loop_depth: 0,
			next_function_index: 0,
			next_local_slot: 0,
			semantic_program: SemanticProgram::default(),
		}
	}

	pub fn analyze_program(&mut self, program: &Program) -> Result<SemanticProgram, CompileError> {
		self.analyze_program_with_schema(program, None)
	}

	pub fn analyze_program_with_schema(
		&mut self,
		program: &Program,
		schema_catalog: Option<&SchemaCatalog>,
	) -> Result<SemanticProgram, CompileError> {
		self.current_return_type = None;
		self.functions = ScopeStack::default();
		self.locals = ScopeStack::default();
		self.loop_depth = 0;
		self.next_function_index = 0;
		self.next_local_slot = 0;
		self.current_schema_catalog = schema_catalog.cloned();
		self.semantic_program = SemanticProgram::default();

		self.validate_with_declarations(&program.with_declarations, schema_catalog)?;
		self.collect_object_declarations(&program.objects)?;
		self.functions.enter_scope();
		self.collect_scope_function_signatures(&program.functions, &program.statements)?;

		for object in &program.objects {
			self.validate_object_declaration(object)?;
		}

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
		self.functions.exit_scope();

		Ok(self.semantic_program.clone())
	}

	pub fn analyze_standalone_program(&mut self, program: &Program) -> Result<SemanticProgram, CompileError> {
		self.analyze_standalone_program_with_schema(program, None)
	}

	pub fn analyze_standalone_program_with_schema(
		&mut self,
		program: &Program,
		schema_catalog: Option<&SchemaCatalog>,
	) -> Result<SemanticProgram, CompileError> {
		let semantic_program = self.analyze_program_with_schema(program, schema_catalog)?;
		let Some(main_function) = program.functions.iter().find(|function| function.name == "Main") else {
			return Err(self.compile_error(
				0,
				String::from("Standalone Tablo programs must define `fn Main(args: [text]) int`."),
			));
		};

		self.validate_main_entry_point(program, main_function)?;

		let mut semantic_program = semantic_program;
		semantic_program.entry_point_function_index = semantic_program.function_declaration_target(main_function.position);
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

	fn collect_object_declarations(&mut self, objects: &[ObjectDeclaration]) -> Result<(), CompileError> {
		for object in objects {
			if self.semantic_program.object_declarations.contains_key(&object.name) {
				return Err(self.compile_error(
					object.position,
					format!("Object `{}` is already declared in this module.", object.name),
				));
			}

			self.semantic_program.object_declarations.insert(object.name.clone(), object.clone());
		}

		Ok(())
	}

	fn collect_scope_function_signatures(&mut self, functions: &[FunctionDeclaration], statements: &[Statement]) -> Result<(), CompileError> {
		for function in functions {
			self.declare_function_signature(function)?;
		}

		for statement in statements {
			if let Statement::FunctionDeclaration(function) = statement {
				self.declare_function_signature(function)?;
			}
		}

		Ok(())
	}

	fn declare_function_signature(&mut self, function: &FunctionDeclaration) -> Result<(), CompileError> {
		if self.functions.contains_in_current_scope(&function.name) {
			return Err(self.compile_error(
				function.position,
				format!("Function `{}` is already declared in this scope.", function.name),
			));
		}

		let mut parameters = Vec::with_capacity(function.parameters.len());

		for parameter in &function.parameters {
			self.validate_non_void_data_type(
				&parameter.data_type,
				parameter.position,
				format!("Parameter `{}` cannot have type `{}`.", parameter.name, self.data_type_name(&parameter.data_type)),
			)?;
			parameters.push(FunctionParameterSignature {
				data_type: parameter.data_type.clone(),
				is_by_ref: parameter.is_by_ref,
				name: parameter.name.clone(),
			});
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

		let function_index = self.next_function_index;
		self.functions.declare(function.name.clone(), FunctionSignature {
			function_index,
			parameters,
			return_type: function.return_type.clone(),
		});
		self.semantic_program.function_declaration_targets.insert(function.position, function_index);
		self.next_function_index += 1;
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

	fn data_type_from_schema_type(&self, data_type: &SchemaDataType) -> Result<DataType, CompileError> {
		match data_type {
			SchemaDataType::Array(element_type) => Ok(DataType::Array(Box::new(self.data_type_from_schema_type(element_type)?))),
			SchemaDataType::Bool => Ok(DataType::Bool),
			SchemaDataType::Dec | SchemaDataType::Float => Ok(DataType::Dec),
			SchemaDataType::Int => Ok(DataType::Int),
			SchemaDataType::Text => Ok(DataType::Text),
			other => Err(self.compile_error(
				0,
				format!("Schema data type `{:?}` is not yet supported by database query semantic analysis.", other),
			)),
		}
	}

	fn data_type_has_implicit_default(&self, data_type: &DataType) -> bool {
		match data_type {
			DataType::Array(_) | DataType::Bool | DataType::Dec | DataType::Int | DataType::Object(_) | DataType::Text => true,
			DataType::EmptyArray | DataType::Range(_) | DataType::RecordPointer(_) | DataType::Void => false,
		}
	}

	fn data_type_name(&self, data_type: &DataType) -> String {
		match data_type {
			DataType::Array(element_type) => format!("[{}]", self.data_type_name(element_type)),
			DataType::Bool => String::from("bool"),
			DataType::Dec => String::from("dec"),
			DataType::EmptyArray => String::from("empty array"),
			DataType::Int => String::from("int"),
			DataType::Object(name) => name.clone(),
			DataType::Range(element_type) => format!("range<{}>", self.data_type_name(element_type)),
			DataType::RecordPointer(_) => String::from("record pointer"),
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

	fn infer_built_in_call_type(&mut self, built_in: BuiltInFunction, arguments: &[CallArgument], position: usize) -> Result<DataType, CompileError> {
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

		for argument in arguments {
			if argument.is_by_ref {
				return Err(self.compile_error(
					argument.position,
					format!("Built-in function `{}` does not accept by-reference arguments.", built_in.name()),
				));
			}
		}

		let argument_types = arguments.iter()
			.map(|argument| self.infer_expression_type(&argument.value))
			.collect::<Result<Vec<_>, _>>()?;

		let return_type = built_in.return_type(&argument_types).ok_or(self.compile_error(
			arguments.first().map_or(position, |argument| argument.value.position()),
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

	fn infer_built_in_call_type_for_query(
		&mut self,
		built_in: BuiltInFunction,
		arguments: &[CallArgument],
		position: usize,
		table: &TableReference,
	) -> Result<DataType, CompileError> {
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

		for argument in arguments {
			if argument.is_by_ref {
				return Err(self.compile_error(
					argument.position,
					format!("Built-in function `{}` does not accept by-reference arguments.", built_in.name()),
				));
			}
		}

		let argument_types = arguments.iter()
			.map(|argument| self.infer_query_expression_type(&argument.value, table))
			.collect::<Result<Vec<_>, _>>()?;

		let return_type = built_in.return_type(&argument_types).ok_or(self.compile_error(
			arguments.first().map_or(position, |argument| argument.value.position()),
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

	fn infer_count_expression_type(&mut self, count: &CountExpr) -> Result<DataType, CompileError> {
		let lowered_query = self.lower_count_query(count)?;

		if let Some(where_clause) = &count.where_clause {
			let where_type = self.infer_query_expression_type(where_clause, &count.table)?;

			if where_type != DataType::Bool {
				return Err(self.compile_error(
					where_clause.position(),
					format!("`where` clause must evaluate to `bool`, found `{}`.", self.data_type_name(&where_type)),
				));
			}
		}

		let compiled_query = lowered_query.lower_to_backend().map_err(|error| self.compile_error(
			count.position,
			query_lowering_error_message(error),
		))?;

		self.semantic_program.compiled_count_queries.insert(count.position, compiled_query);
		self.semantic_program.lowered_count_queries.insert(count.position, lowered_query);
		Ok(DataType::Int)
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
					AssignmentTarget::Field(target) => {
						let local = self.lookup_local(&target.object.name).ok_or(self.compile_error(
							target.object.position,
							format!("Variable `{}` is not declared in this scope.", target.object.name),
						))?;
						self.semantic_program.identifier_slots.insert(target.object.position, local.slot);

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
								format!("Constant `{}` cannot be assigned using `{operation}`.", target.object.name),
							));
						}

						let mut current_type = local.data_type.clone();

						for field in &target.fields {
							match current_type {
								DataType::Object(ref object_name) => {
									current_type = self.lookup_object_field(object_name, &field.name)
										.ok_or(self.compile_error(
											field.position,
											format!("Object `{object_name}` does not contain a field named `{}`.", field.name),
										))?
										.data_type
										.clone();
								}
								ref other => {
									return Err(self.compile_error(
										field.position,
										format!("Field access requires an object operand, found `{}`.", self.data_type_name(other)),
									));
								}
							}
						}

						let value_type = self.infer_expression_type(value)?;
						self.assignment_result_type(*operator, &current_type, &value_type, expression.position())
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
				if let Some(signature) = self.lookup_function(&callee.name).cloned() {
					if arguments.len() != signature.parameters.len() {
						return Err(self.compile_error(
							expression.position(),
							format!(
								"Function `{}` expects {} argument(s), found {}.",
								callee.name,
								signature.parameters.len(),
								arguments.len(),
							),
						));
					}

					let mut reference_slots = Vec::with_capacity(arguments.len());

					for (argument, parameter) in arguments.iter().zip(signature.parameters.iter()) {
						let argument_type = self.infer_expression_type(&argument.value)?;

						if parameter.is_by_ref {
							if !argument.is_by_ref {
								return Err(self.compile_error(
									argument.position,
									format!("Parameter `{}` must be passed by reference.", parameter.name),
								));
							}

							let Expr::Identifier(identifier) = &argument.value else {
								return Err(self.compile_error(
									argument.position,
									String::from("By-reference arguments must be plain identifiers."),
								));
							};

							let local = self.lookup_local(&identifier.name).ok_or(self.compile_error(
								identifier.position,
								format!("Variable `{}` is not declared in this scope.", identifier.name),
							))?;

							if argument_type != parameter.data_type {
								return Err(self.compile_error(
									argument.position,
									format!(
										"By-reference argument for parameter `{}` must have type `{}`, found `{}`.",
										parameter.name,
										self.data_type_name(&parameter.data_type),
										self.data_type_name(&argument_type),
									),
								));
							}

							reference_slots.push(Some(local.slot));
						}
						else {
							if argument.is_by_ref {
								return Err(self.compile_error(
									argument.position,
									format!("Parameter `{}` must be passed by value.", parameter.name),
								));
							}

							self.ensure_assignable(&parameter.data_type, &argument_type, argument.value.position())?;
							reference_slots.push(None);
						}
					}

					self.semantic_program.call_targets.insert(expression.position(), signature.function_index);
					self.semantic_program.call_argument_reference_slots.insert(expression.position(), reference_slots);
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
			Expr::Count(count) => self.infer_count_expression_type(count),
			Expr::Decimal(_) => Ok(DataType::Dec),
			Expr::FieldAccess(FieldAccessExpr { field, object, .. }) => {
				let object_type = self.infer_expression_type(object)?;

				match object_type {
					DataType::Object(name) => {
						let field_type = self.lookup_object_field(&name, &field.name)
							.ok_or(self.compile_error(
								field.position,
								format!("Object `{name}` does not contain a field named `{}`.", field.name),
							))?
							.data_type
							.clone();
						Ok(field_type)
					}
					DataType::RecordPointer(record_pointer) => self.infer_record_pointer_field_access_type(
						&record_pointer,
						field.position,
						&field.name,
					),
					other => Err(self.compile_error(
						object.position(),
						format!("Field access requires an object operand, found `{}`.", self.data_type_name(&other)),
					)),
				}
			}
			Expr::Find(find) => self.infer_find_expression_type(find),
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

				match array_type {
					DataType::Array(element_type) => match index_type {
						DataType::Int => Ok(*element_type),
						DataType::Range(index_element_type) if *index_element_type == DataType::Int => {
							Ok(DataType::Array(element_type))
						}
						DataType::Range(index_element_type) => Err(self.compile_error(
							index.position(),
							format!(
								"Array slicing requires a range of `int`, found `range<{}>`.",
								self.data_type_name(&index_element_type),
							),
						)),
						other => Err(self.compile_error(
							index.position(),
							format!("Array index must be of type `int`, found `{}`.", self.data_type_name(&other)),
						)),
					},
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
			Expr::ObjectConstruction(ObjectConstructionExpr {
				fields,
				object_type_name,
				position,
			}) => {
				let object_declaration = self.semantic_program.object_declaration(object_type_name)
					.cloned()
					.ok_or(self.compile_error(
						*position,
						format!("Object `{object_type_name}` is not declared in this module."),
					))?;
				let mut provided_fields = BTreeMap::new();

				for field in fields {
					let Some(object_field) = object_declaration.fields.iter().find(|object_field| object_field.name == field.name) else {
						return Err(self.compile_error(
							field.position,
							format!("Object `{object_type_name}` does not contain a field named `{}`.", field.name),
						));
					};

					if provided_fields.insert(field.name.clone(), ()).is_some() {
						return Err(self.compile_error(
							field.position,
							format!("Field `{}` is specified more than once when constructing `{object_type_name}`.", field.name),
						));
					}

					let value_type = self.infer_expression_type(&field.value)?;
					self.ensure_assignable(&object_field.data_type, &value_type, field.value.position())?;
				}

				for object_field in &object_declaration.fields {
					if !provided_fields.contains_key(&object_field.name)
						&& object_field.default_value.is_none()
						&& !self.data_type_has_implicit_default(&object_field.data_type)
					{
						return Err(self.compile_error(
							*position,
							format!("Field `{}` must be specified when constructing `{object_type_name}`.", object_field.name),
						));
					}
				}

				Ok(DataType::Object(object_type_name.clone()))
			}
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

	fn infer_find_expression_type(&mut self, find: &FindExpr) -> Result<DataType, CompileError> {
		let lowered_query = self.lower_find_query(find)?;
		let record_pointer = {
			let resolved_table = self.resolve_table_reference(&find.table)?;
			RecordPointerType {
				database_name: resolved_table.database().name().to_string(),
				schema_name: resolved_table.schema().name().to_string(),
				table_name: resolved_table.table().name().to_string(),
			}
		};

		if let Some(where_clause) = &find.where_clause {
			let where_type = self.infer_query_expression_type(where_clause, &find.table)?;

			if where_type != DataType::Bool {
				return Err(self.compile_error(
					where_clause.position(),
					format!("`where` clause must evaluate to `bool`, found `{}`.", self.data_type_name(&where_type)),
				));
			}
		}

		for order_by in &find.order_by {
			let order_type = self.infer_query_expression_type(&order_by.expression, &find.table)?;

			if order_type == DataType::Void {
				return Err(self.compile_error(
					order_by.position,
					String::from("`order by` expressions must produce a runtime value."),
				));
			}
		}

		let compiled_query = lowered_query.lower_to_backend().map_err(|error| self.compile_error(
			find.position,
			query_lowering_error_message(error),
		))?;
		self.semantic_program.compiled_find_queries.insert(find.position, compiled_query);
		self.semantic_program.lowered_find_queries.insert(find.position, lowered_query);

		Ok(DataType::RecordPointer(record_pointer))
	}

	fn infer_query_expression_type(
		&mut self,
		expression: &Expr,
		table: &TableReference,
	) -> Result<DataType, CompileError> {
		match expression {
			Expr::Array(_) => Err(self.compile_error(
				expression.position(),
				String::from("Array literals are not yet supported in `where` clauses."),
			)),
			Expr::Assignment(_) => Err(self.compile_error(
				expression.position(),
				String::from("Assignments are not permitted in `where` clauses."),
			)),
			Expr::Binary(BinaryExpr { left, operator, right, .. }) => {
				let left_type = self.infer_query_expression_type(left, table)?;
				let right_type = self.infer_query_expression_type(right, table)?;
				self.binary_result_type(*operator, &left_type, &right_type, expression.position())
			}
			Expr::Boolean(_) => Ok(DataType::Bool),
			Expr::Call(CallExpr { arguments, callee, .. }) => {
				if let Some(signature) = self.lookup_function(&callee.name).cloned() {
					if arguments.len() != signature.parameters.len() {
						return Err(self.compile_error(
							expression.position(),
							format!(
								"Function `{}` expects {} argument(s), found {}.",
								callee.name,
								signature.parameters.len(),
								arguments.len(),
							),
						));
					}

					for (argument, parameter) in arguments.iter().zip(signature.parameters.iter()) {
						if argument.is_by_ref {
							return Err(self.compile_error(
								argument.position,
								String::from("By-reference arguments are not permitted in `where` clauses."),
							));
						}

						let argument_type = self.infer_query_expression_type(&argument.value, table)?;
						self.ensure_assignable(&parameter.data_type, &argument_type, argument.value.position())?;
					}

					Ok(signature.return_type)
				}
				else if let Some(built_in) = BuiltInFunction::from_name(&callee.name) {
					let query_arguments = arguments.iter().map(|argument| CallArgument {
						is_by_ref: argument.is_by_ref,
						position: argument.position,
						value: argument.value.clone(),
					}).collect::<Vec<_>>();

					self.infer_built_in_call_type_for_query(built_in, &query_arguments, expression.position(), table)
				}
				else {
					Err(self.compile_error(
						callee.position,
						format!("Function `{}` is not declared in this scope.", callee.name),
					))
				}
			}
			Expr::Count(_) => Err(self.compile_error(
				expression.position(),
				String::from("Nested database queries are not yet supported in `where` clauses."),
			)),
			Expr::Decimal(_) => Ok(DataType::Dec),
			Expr::FieldAccess(field_access) => self.infer_query_field_access_type(field_access, table),
			Expr::Find(_) => Err(self.compile_error(
				expression.position(),
				String::from("Nested `find` queries are not yet supported in `where` clauses."),
			)),
			Expr::Identifier(identifier) => {
				if let Some(local) = self.lookup_local(&identifier.name) {
					self.semantic_program.identifier_slots.insert(identifier.position, local.slot);
					return Ok(local.data_type);
				}

				let (table_name, column_type) = {
					let resolved_table = self.resolve_table_reference(table)?;
					let table_name = resolved_table.table().name().to_string();
					let column_type = resolved_table.table().column(&identifier.name)
						.map(|column| column.data_type().clone())
						.ok_or(self.compile_error(
							identifier.position,
							format!("Field `{}` does not exist on table `{table_name}`.", identifier.name),
						))?;
					(table_name, column_type)
				};

				let _ = table_name;
				self.data_type_from_schema_type(&column_type)
			}
			Expr::Index(_) | Expr::ObjectConstruction(_) | Expr::Range(_) => Err(self.compile_error(
				expression.position(),
				String::from("This expression form is not yet supported in `where` clauses."),
			)),
			Expr::Integer(_) => Ok(DataType::Int),
			Expr::Text(_) => Ok(DataType::Text),
			Expr::Unary(UnaryExpr { operand, operator, .. }) => {
				let operand_type = self.infer_query_expression_type(operand, table)?;

				match operator {
					UnaryOperator::Negate => {
						if !matches!(operand_type, DataType::Int | DataType::Dec) {
							return Err(self.compile_error(
								operand.position(),
								format!("Unary `-` requires a numeric operand, found `{}`.", self.data_type_name(&operand_type)),
							));
						}

						Ok(operand_type)
					}
					UnaryOperator::Not => {
						if operand_type != DataType::Bool {
							return Err(self.compile_error(
								operand.position(),
								format!("Logical `not` requires a `bool` operand, found `{}`.", self.data_type_name(&operand_type)),
							));
						}

						Ok(DataType::Bool)
					}
				}
			}
		}
	}

	fn infer_query_field_access_type(
		&mut self,
		field_access: &FieldAccessExpr,
		table: &TableReference,
	) -> Result<DataType, CompileError> {
		if let Expr::Identifier(identifier) = field_access.object.as_ref() {
			if let Some(local) = self.lookup_local(&identifier.name) {
				self.semantic_program.identifier_slots.insert(identifier.position, local.slot);

				return match &local.data_type {
					DataType::Object(name) => {
						let field_type = self.lookup_object_field(name, &field_access.field.name)
							.ok_or(self.compile_error(
								field_access.field.position,
								format!("Object `{name}` does not contain a field named `{}`.", field_access.field.name),
							))?
							.data_type
							.clone();
						Ok(field_type)
					}
					other => Err(self.compile_error(
						identifier.position,
						format!("Field access requires an object operand, found `{}`.", self.data_type_name(other)),
					)),
				};
			}

			let (resolved_table_name, column_type) = {
				let resolved_table = self.resolve_table_reference(table)?;
				let resolved_table_name = resolved_table.table().name().to_string();

				if !identifier.name.eq_ignore_ascii_case(&resolved_table_name) {
					return Err(self.compile_error(
						identifier.position,
						format!(
							"Qualified field reference must use the target table name `{resolved_table_name}`.",
						),
					));
				}

				let column_type = resolved_table.table().column(&field_access.field.name)
					.map(|column| column.data_type().clone())
					.ok_or(self.compile_error(
						field_access.field.position,
						format!(
							"Field `{}` does not exist on table `{resolved_table_name}`.",
							field_access.field.name,
						),
					))?;

				(resolved_table_name, column_type)
			};

			let _ = resolved_table_name;
			return self.data_type_from_schema_type(&column_type);
		}

		Err(self.compile_error(
			field_access.position,
			String::from("Only simple `table.field` or local object field access is supported in `where` clauses."),
		))
	}

	fn infer_record_pointer_field_access_type(
		&mut self,
		record_pointer: &RecordPointerType,
		field_position: usize,
		field_name: &str,
	) -> Result<DataType, CompileError> {
		let schema_catalog = self.current_schema_catalog.as_ref().ok_or(self.compile_error(
			field_position,
			String::from("Record pointer field access requires schema metadata, but none was supplied."),
		))?;
		let database = schema_catalog.database(&record_pointer.database_name).ok_or(self.compile_error(
			field_position,
			format!("Unknown database `{}` for record pointer field access.", record_pointer.database_name),
		))?;
		let schema = database.schema(&record_pointer.schema_name).ok_or(self.compile_error(
			field_position,
			format!(
				"Unknown schema `{}` on database `{}` for record pointer field access.",
				record_pointer.schema_name,
				record_pointer.database_name,
			),
		))?;
		let table = schema.table(&record_pointer.table_name).ok_or(self.compile_error(
			field_position,
			format!(
				"Unknown table `{}` on database `{}.{}` for record pointer field access.",
				record_pointer.table_name,
				record_pointer.database_name,
				record_pointer.schema_name,
			),
		))?;
		let column = table.column(field_name).ok_or(self.compile_error(
			field_position,
			format!(
				"Field `{field_name}` does not exist on table `{}`.",
				record_pointer.table_name,
			),
		))?;

		self.data_type_from_schema_type(column.data_type())
	}

	fn is_numeric_type(&self, data_type: &DataType) -> bool {
		matches!(data_type, DataType::Dec | DataType::Int)
	}

	fn is_truthy_condition_type(&self, data_type: &DataType) -> bool {
		matches!(data_type, DataType::Bool | DataType::RecordPointer(_))
	}

	fn lookup_function(&self, name: &str) -> Option<&FunctionSignature> {
		self.functions.lookup(name)
	}

	fn lookup_local(&self, name: &str) -> Option<LocalBinding> {
		self.locals.lookup(name).cloned()
	}

	fn lookup_object_field(&self, object_name: &str, field_name: &str) -> Option<&crate::ast::ObjectFieldDeclaration> {
		self.semantic_program.object_declaration(object_name)?
			.fields
			.iter()
			.find(|field| field.name == field_name)
	}

	fn lower_count_query(&mut self, count: &CountExpr) -> Result<QueryCountPlan, CompileError> {
		let (backend, database_name, schema_name, schema_is_implicit, table_name) = {
			let resolved_table = self.resolve_table_reference(&count.table)?;
			(
				resolved_table.database().backend(),
				resolved_table.database().name().to_string(),
				resolved_table.schema().name().to_string(),
				resolved_table.schema().is_implicit(),
				resolved_table.table().name().to_string(),
			)
		};
		let filter = count.where_clause.as_ref()
			.map(|where_clause| self.lower_query_expression(where_clause, &count.table, backend))
			.transpose()?;

		Ok(QueryCountPlan {
			backend,
			database_name,
			filter,
			schema_is_implicit,
			schema_name,
			table_name,
		})
	}

	fn lower_find_query(&mut self, find: &FindExpr) -> Result<QueryFindPlan, CompileError> {
		let (backend, database_name, schema_name, schema_is_implicit, table_name, record_column_schemas) = {
			let resolved_table = self.resolve_table_reference(&find.table)?;
			let record_column_schemas = resolved_table.table().columns()
				.map(|column| (column.name().to_string(), column.data_type().clone(), column.is_nullable()))
				.collect::<Vec<_>>();
			(
				resolved_table.database().backend(),
				resolved_table.database().name().to_string(),
				resolved_table.schema().name().to_string(),
				resolved_table.schema().is_implicit(),
				resolved_table.table().name().to_string(),
				record_column_schemas,
			)
		};
		let record_columns = record_column_schemas.into_iter()
			.map(|(column_name, schema_type, is_nullable)| Ok(QueryResultColumn {
				column_name,
				data_type: self.data_type_from_schema_type(&schema_type)?,
				is_nullable,
			}))
			.collect::<Result<Vec<_>, CompileError>>()?;
		let filter = find.where_clause.as_ref()
			.map(|where_clause| self.lower_query_expression(where_clause, &find.table, backend))
			.transpose()?;
		let order_by = find.order_by.iter()
			.map(|item| Ok(QueryOrderByItem {
				direction: item.direction,
				expression: self.lower_query_expression(&item.expression, &find.table, backend)?,
			}))
			.collect::<Result<Vec<_>, CompileError>>()?;

		Ok(QueryFindPlan {
			backend,
			database_name,
			filter,
			kind: find.kind,
			order_by,
			record_columns,
			schema_is_implicit,
			schema_name,
			table_name,
		})
	}

	fn lower_for_record_query(&mut self, for_record: &ForRecordStatement) -> Result<QueryForPlan, CompileError> {
		let (backend, database_name, schema_name, schema_is_implicit, table_name, record_column_schemas) = {
			let resolved_table = self.resolve_table_reference(&for_record.table)?;
			let record_column_schemas = resolved_table.table().columns()
				.map(|column| (column.name().to_string(), column.data_type().clone(), column.is_nullable()))
				.collect::<Vec<_>>();
			(
				resolved_table.database().backend(),
				resolved_table.database().name().to_string(),
				resolved_table.schema().name().to_string(),
				resolved_table.schema().is_implicit(),
				resolved_table.table().name().to_string(),
				record_column_schemas,
			)
		};
		let record_columns = record_column_schemas.into_iter()
			.map(|(column_name, schema_type, is_nullable)| Ok(QueryResultColumn {
				column_name,
				data_type: self.data_type_from_schema_type(&schema_type)?,
				is_nullable,
			}))
			.collect::<Result<Vec<_>, CompileError>>()?;
		let filter = for_record.where_clause.as_ref()
			.map(|where_clause| self.lower_query_expression(where_clause, &for_record.table, backend))
			.transpose()?;
		let order_by = for_record.order_by.iter()
			.map(|item| Ok(QueryOrderByItem {
				direction: item.direction,
				expression: self.lower_query_expression(&item.expression, &for_record.table, backend)?,
			}))
			.collect::<Result<Vec<_>, CompileError>>()?;

		Ok(QueryForPlan {
			backend,
			database_name,
			filter,
			order_by,
			record_columns,
			schema_is_implicit,
			schema_name,
			table_name,
		})
	}

	fn lower_query_binary_operator(&self, operator: BinaryOperator) -> QueryBinaryOperator {
		match operator {
			BinaryOperator::Add => QueryBinaryOperator::Add,
			BinaryOperator::And => QueryBinaryOperator::And,
			BinaryOperator::Divide => QueryBinaryOperator::Divide,
			BinaryOperator::Equal => QueryBinaryOperator::Equal,
			BinaryOperator::GreaterThan => QueryBinaryOperator::GreaterThan,
			BinaryOperator::GreaterThanOrEqual => QueryBinaryOperator::GreaterThanOrEqual,
			BinaryOperator::LessThan => QueryBinaryOperator::LessThan,
			BinaryOperator::LessThanOrEqual => QueryBinaryOperator::LessThanOrEqual,
			BinaryOperator::Modulo => QueryBinaryOperator::Modulo,
			BinaryOperator::Multiply => QueryBinaryOperator::Multiply,
			BinaryOperator::NotEqual => QueryBinaryOperator::NotEqual,
			BinaryOperator::Or => QueryBinaryOperator::Or,
			BinaryOperator::Subtract => QueryBinaryOperator::Subtract,
			BinaryOperator::Xor => QueryBinaryOperator::Xor,
		}
	}

	fn lower_query_expression(
		&mut self,
		expression: &Expr,
		table: &TableReference,
		backend: DatabaseBackend,
	) -> Result<QueryExpr, CompileError> {
		let _ = backend;

		match expression {
			Expr::Binary(BinaryExpr { left, operator, right, .. }) => Ok(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(self.lower_query_expression(left, table, backend)?),
				operator: self.lower_query_binary_operator(*operator),
				right: Box::new(self.lower_query_expression(right, table, backend)?),
			})),
			Expr::Boolean(boolean) => Ok(QueryExpr::Literal(QueryLiteral::Boolean(boolean.value))),
			Expr::Call(CallExpr { callee, .. }) => Err(self.compile_error(
				expression.position(),
				format!("Function `{}` is not yet supported in lowered database query expressions.", callee.name),
			)),
			Expr::Decimal(decimal) => Ok(QueryExpr::Literal(QueryLiteral::Decimal(decimal.value.clone()))),
			Expr::FieldAccess(field_access) => self.lower_query_field_access(field_access, table, backend),
			Expr::Identifier(identifier) => {
				if let Some(local) = self.lookup_local(&identifier.name) {
					self.semantic_program.identifier_slots.insert(identifier.position, local.slot);
					return Ok(QueryExpr::Parameter(QueryParameter {
						data_type: local.data_type,
						slot: local.slot,
					}));
				}

				let (table_name, column_name, column_type) = {
					let resolved_table = self.resolve_table_reference(table)?;
					let table_name = resolved_table.table().name().to_string();
					let Some(column) = resolved_table.table().column(&identifier.name) else {
						return Err(self.compile_error(
							identifier.position,
							format!("Field `{}` does not exist on table `{table_name}`.", identifier.name),
						));
					};
					(
						table_name,
						column.name().to_string(),
						column.data_type().clone(),
					)
				};

				Ok(QueryExpr::Column(QueryColumnReference {
					column_name,
					data_type: self.data_type_from_schema_type(&column_type)?,
					table_name,
				}))
			}
			Expr::Integer(integer) => Ok(QueryExpr::Literal(QueryLiteral::Integer(integer.value))),
			Expr::Text(text) => Ok(QueryExpr::Literal(QueryLiteral::Text(text.value.clone()))),
			Expr::Unary(UnaryExpr { operand, operator, .. }) => Ok(QueryExpr::Unary(QueryUnaryExpr {
				operand: Box::new(self.lower_query_expression(operand, table, backend)?),
				operator: match operator {
					UnaryOperator::Negate => QueryUnaryOperator::Negate,
					UnaryOperator::Not => QueryUnaryOperator::Not,
				},
			})),
			_ => Err(self.compile_error(
				expression.position(),
				String::from("This expression form cannot yet be lowered into a database query."),
			)),
		}
	}

	fn lower_query_field_access(
		&mut self,
		field_access: &FieldAccessExpr,
		table: &TableReference,
		backend: DatabaseBackend,
	) -> Result<QueryExpr, CompileError> {
		let _ = backend;

		if let Expr::Identifier(identifier) = field_access.object.as_ref() {
			if let Some(local) = self.lookup_local(&identifier.name) {
				self.semantic_program.identifier_slots.insert(identifier.position, local.slot);

				return Err(self.compile_error(
					field_access.position,
					format!(
						"Object field access such as `{}.{}` is not yet supported in lowered database query expressions.",
						identifier.name,
						field_access.field.name,
					),
				));
			}

			let resolved_table = self.resolve_table_reference(table)?;
			let resolved_table_name = resolved_table.table().name().to_string();

			if !identifier.name.eq_ignore_ascii_case(&resolved_table_name) {
				return Err(self.compile_error(
					identifier.position,
					format!("Qualified field reference must use the target table name `{resolved_table_name}`."),
				));
			}

			let (column_name, column_type) = {
				let Some(column) = resolved_table.table().column(&field_access.field.name) else {
					return Err(self.compile_error(
						field_access.field.position,
						format!(
							"Field `{}` does not exist on table `{resolved_table_name}`.",
							field_access.field.name,
						),
					));
				};
				(column.name().to_string(), column.data_type().clone())
			};

			return Ok(QueryExpr::Column(QueryColumnReference {
				column_name,
				data_type: self.data_type_from_schema_type(&column_type)?,
				table_name: resolved_table_name,
			}));
		}

		Err(self.compile_error(
			field_access.position,
			String::from("Only simple `table.field` access may be lowered into a database query."),
		))
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

	fn resolve_table_reference(
		&mut self,
		table: &TableReference,
	) -> Result<crate::schema::ResolvedTable<'_>, CompileError> {
		if let Some(resolved) = self.semantic_program.resolved_tables.get(&table.position) {
			let schema_catalog = self.current_schema_catalog.as_ref().ok_or(self.compile_error(
				table.position,
				String::from("Database queries require schema metadata, but none was supplied."),
			))?;

			return schema_catalog
				.resolve_database_schema_table(
					&self.semantic_program.active_databases.iter().map(String::as_str).collect::<Vec<_>>(),
					&resolved.database_name,
					&resolved.schema_name,
					&resolved.table_name,
				)
				.map_err(|error| self.schema_error_to_compile_error(table.position, error));
		}

		let schema_catalog = self.current_schema_catalog.as_ref().ok_or(self.compile_error(
			table.position,
			String::from("Database queries require schema metadata, but none was supplied."),
		))?;
		let active_databases = self.semantic_program.active_databases.iter().map(String::as_str).collect::<Vec<_>>();

		let resolved = match table.components.as_slice() {
			[table_name] => schema_catalog.resolve_table(&active_databases, &table_name.name),
			[first, table_name] => {
				match schema_catalog.resolve_database_table(&active_databases, &first.name, &table_name.name) {
					Ok(resolved) => Ok(resolved),
					Err(SchemaError::UnknownDatabase { .. }) | Err(SchemaError::AmbiguousDatabaseQualifiedTableName { .. }) => {
						schema_catalog.resolve_schema_table(&active_databases, &first.name, &table_name.name)
					}
					Err(other) => Err(other),
				}
			}
			[database, schema, table_name] => {
				schema_catalog.resolve_database_schema_table(&active_databases, &database.name, &schema.name, &table_name.name)
			}
			_ => Err(SchemaError::UnknownTable {
				table_name: String::from("<invalid table reference>"),
			}),
		}.map_err(|error| self.schema_error_to_compile_error(table.position, error))?;

		self.semantic_program.resolved_tables.insert(table.position, ResolvedTableReference {
			database_name: resolved.database().name().to_string(),
			schema_name: resolved.schema().name().to_string(),
			table_name: resolved.table().name().to_string(),
		});

		Ok(resolved)
	}

	fn schema_error_to_compile_error(&self, position: usize, error: SchemaError) -> CompileError {
		self.compile_error(position, match error {
			SchemaError::AmbiguousDatabaseQualifiedTableName { database_name, table_name } => {
				format!(
					"Table reference `{database_name}.{table_name}` is ambiguous because database `{database_name}` contains multiple schemas."
				)
			}
			SchemaError::AmbiguousSchemaQualifiedTableName { active_databases, schema_name, table_name } => {
				format!(
					"Table reference `{schema_name}.{table_name}` is ambiguous across active databases: {}.",
					active_databases.join(", ")
				)
			}
			SchemaError::AmbiguousTableName { active_databases, table_name } => {
				format!(
					"Table reference `{table_name}` is ambiguous across active databases: {}.",
					active_databases.join(", ")
				)
			}
			SchemaError::DuplicateColumn { .. }
			| SchemaError::DuplicateDatabase { .. }
			| SchemaError::DuplicateSchema { .. }
			| SchemaError::DuplicateTable { .. } => {
				String::from("The supplied schema catalog is internally inconsistent.")
			}
			SchemaError::UnknownDatabase { database_name } => {
				format!("Database `{database_name}` is not present in the supplied schema catalog.")
			}
			SchemaError::UnknownSchema { database_name, schema_name } => {
				match database_name {
					Some(database_name) => {
						format!("Schema `{schema_name}` does not exist in database `{database_name}`.")
					}
					None => format!("Schema `{schema_name}` does not exist in the active databases."),
				}
			}
			SchemaError::UnknownTable { table_name } => {
				format!("Table `{table_name}` is not present in the active databases.")
			}
		})
	}

	fn statement_guarantees_return(&self, statement: &Statement) -> bool {
		match statement {
			Statement::Block(block) => self.block_guarantees_return(block),
			Statement::Break(_) | Statement::Continue(_) => false,
			Statement::FunctionDeclaration(_) => false,
			Statement::If(if_statement) => {
				self.block_guarantees_return(&if_statement.then_branch)
					&& if_statement.else_branch.as_ref().is_some_and(|else_branch| self.statement_guarantees_return(else_branch))
			}
			Statement::RecordPointerDeclaration(_) => false,
			Statement::Return(_) => true,
			Statement::Expression(_) | Statement::For(_) | Statement::ForRecord(_) | Statement::VariableDeclaration(_) | Statement::While(_) => false,
		}
	}

	fn validate_block(&mut self, statements: &[Statement]) -> Result<(), CompileError> {
		self.enter_scope();
		self.functions.enter_scope();
		self.collect_scope_function_signatures(&[], statements)?;

		for statement in statements {
			self.validate_statement(statement)?;
		}

		self.functions.exit_scope();
		self.exit_scope();
		Ok(())
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

		self.validate_block(function.body.statements.as_slice())?;

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

	fn validate_literal_expression(&mut self, expression: &Expr) -> Result<(), CompileError> {
		match expression {
			Expr::Array(array) => {
				for element in &array.elements {
					self.validate_literal_expression(element)?;
				}
				Ok(())
			}
			Expr::Boolean(_) | Expr::Decimal(_) | Expr::Integer(_) | Expr::Text(_) => Ok(()),
			Expr::ObjectConstruction(construction) => {
				for field in &construction.fields {
					self.validate_literal_expression(&field.value)?;
				}
				Ok(())
			}
			_ => Err(self.compile_error(
				expression.position(),
				String::from("Object field defaults must be literals."),
			)),
		}
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
			|| main_function.parameters[0].is_by_ref
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
			DataType::Object(name) => {
				if self.semantic_program.object_declarations.contains_key(name) {
					Ok(())
				}
				else {
					Err(self.compile_error(
						position,
						format!("Object type `{name}` is not declared in this module."),
					))
				}
			}
			DataType::RecordPointer(_) => Err(self.compile_error(position, message)),
			_ => Ok(()),
		}
	}

	fn validate_object_declaration(&mut self, object: &ObjectDeclaration) -> Result<(), CompileError> {
		let mut field_names = BTreeMap::new();

		for field in &object.fields {
			if field_names.insert(field.name.clone(), ()).is_some() {
				return Err(self.compile_error(
					field.position,
					format!("Field `{}` is already declared on object `{}`.", field.name, object.name),
				));
			}

			self.validate_non_void_data_type(
				&field.data_type,
				field.position,
				format!("Field `{}` cannot have type `{}`.", field.name, self.data_type_name(&field.data_type)),
			)?;

			if let Some(default_value) = &field.default_value {
				self.validate_literal_expression(default_value)?;
				let default_type = self.infer_expression_type(default_value)?;
				self.ensure_assignable(&field.data_type, &default_type, default_value.position())?;
			}
		}

		Ok(())
	}

	fn validate_statement(&mut self, statement: &Statement) -> Result<(), CompileError> {
		match statement {
			Statement::Block(BlockStatement { statements, .. }) => self.validate_block(statements),
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
			Statement::FunctionDeclaration(function) => self.validate_function_declaration(function),
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
			Statement::ForRecord(ForRecordStatement {
				body,
				is_mut,
				order_by,
				position,
				table,
				variable,
				where_clause,
			}) => {
				let lowered_query = self.lower_for_record_query(&ForRecordStatement {
					body: body.clone(),
					is_mut: *is_mut,
					order_by: order_by.clone(),
					position: *position,
					table: table.clone(),
					variable: variable.clone(),
					where_clause: where_clause.clone(),
				})?;
				let record_pointer = {
					let resolved_table = self.resolve_table_reference(table)?;
					RecordPointerType {
						database_name: resolved_table.database().name().to_string(),
						schema_name: resolved_table.schema().name().to_string(),
						table_name: resolved_table.table().name().to_string(),
					}
				};

				if let Some(where_clause) = where_clause {
					let where_type = self.infer_query_expression_type(where_clause, table)?;

					if where_type != DataType::Bool {
						return Err(self.compile_error(
							where_clause.position(),
							format!("`where` clause must evaluate to `bool`, found `{}`.", self.data_type_name(&where_type)),
						));
					}
				}

				for order_by in order_by {
					let order_type = self.infer_query_expression_type(&order_by.expression, table)?;

					if order_type == DataType::Void {
						return Err(self.compile_error(
							order_by.position,
							String::from("`order by` expressions must produce a runtime value."),
						));
					}
				}

				let compiled_query = lowered_query.lower_to_backend().map_err(|error| self.compile_error(
					*position,
					query_lowering_error_message(error),
				))?;
				self.semantic_program.compiled_for_record_queries.insert(*position, compiled_query);
				self.semantic_program.lowered_for_record_queries.insert(*position, lowered_query);

				self.enter_scope();

				if self.current_scope_contains(&variable.name) {
					self.exit_scope();
					return Err(self.compile_error(
						variable.position,
						format!("Record pointer `{}` is already declared in this scope.", variable.name),
					));
				}

				let loop_variable_slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.declaration_slots.insert(variable.position, loop_variable_slot);
				self.semantic_program.declaration_types.insert(
					variable.position,
					DataType::RecordPointer(record_pointer.clone()),
				);
				self.declare_local(variable.name.clone(), LocalBinding {
					data_type: DataType::RecordPointer(record_pointer),
					is_const: !*is_mut,
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

				if !self.is_truthy_condition_type(&condition_type) {
					return Err(self.compile_error(
						condition.position().max(*position),
						format!("`if` condition must be of type `bool` or `record pointer`, found `{}`.", self.data_type_name(&condition_type)),
					));
				}

				self.validate_statement(&Statement::Block(then_branch.clone()))?;

				if let Some(else_branch) = else_branch {
					self.validate_statement(else_branch)?;
				}

				Ok(())
			}
			Statement::RecordPointerDeclaration(RecordPointerDeclaration { initial_value, is_mut, name, position }) => {
				if self.current_scope_contains(name) {
					return Err(self.compile_error(
						*position,
						format!("Record pointer `{name}` is already declared in this scope."),
					));
				}

				let initial_type = self.infer_expression_type(initial_value)?;
				let DataType::RecordPointer(_) = initial_type else {
					return Err(self.compile_error(
						initial_value.position(),
						format!(
							"Record pointer `{name}` must be initialized from a record pointer value, found `{}`.",
							self.data_type_name(&initial_type),
						),
					));
				};

				let slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.declaration_slots.insert(*position, slot);
				self.semantic_program.declaration_types.insert(*position, initial_type.clone());
				self.declare_local(name.clone(), LocalBinding {
					data_type: initial_type,
					is_const: !*is_mut,
					slot,
				});

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

	fn validate_with_declarations(
		&mut self,
		with_declarations: &[WithDeclaration],
		schema_catalog: Option<&SchemaCatalog>,
	) -> Result<(), CompileError> {
		for with_declaration in with_declarations {
			if with_declaration.databases.is_empty() {
				return Err(self.compile_error(
					with_declaration.position,
					String::from("`with` declaration must include at least one database name."),
				));
			}

			for database in &with_declaration.databases {
				if self.semantic_program.active_databases.iter().any(|active| active.eq_ignore_ascii_case(&database.name)) {
					return Err(self.compile_error(
						database.position,
						format!("Database `{}` is already active in this module.", database.name),
					));
				}

				if let Some(schema_catalog) = schema_catalog {
					if schema_catalog.database(&database.name).is_none() {
						return Err(self.compile_error(
							database.position,
							format!("Database `{}` is not present in the supplied schema catalog.", database.name),
						));
					}
				}
				else {
					return Err(self.compile_error(
						database.position,
						String::from("Cannot validate `with` declarations without a supplied schema catalog."),
					));
				}

				self.semantic_program.active_databases.push(database.name.clone());
			}
		}

		Ok(())
	}
}

fn database_backend_name(backend: DatabaseBackend) -> &'static str {
	match backend {
		DatabaseBackend::MySql => "mysql",
		DatabaseBackend::PostgreSql => "postgresql",
		DatabaseBackend::Sqlite => "sqlite",
	}
}

fn query_binary_operator_name(operator: QueryBinaryOperator) -> &'static str {
	match operator {
		QueryBinaryOperator::Add => "+",
		QueryBinaryOperator::And => "and",
		QueryBinaryOperator::Divide => "/",
		QueryBinaryOperator::Equal => "=",
		QueryBinaryOperator::GreaterThan => ">",
		QueryBinaryOperator::GreaterThanOrEqual => ">=",
		QueryBinaryOperator::LessThan => "<",
		QueryBinaryOperator::LessThanOrEqual => "<=",
		QueryBinaryOperator::Modulo => "%",
		QueryBinaryOperator::Multiply => "*",
		QueryBinaryOperator::NotEqual => "!=",
		QueryBinaryOperator::Or => "or",
		QueryBinaryOperator::Subtract => "-",
		QueryBinaryOperator::Xor => "xor",
	}
}

fn query_lowering_error_message(error: QueryLoweringError) -> String {
	match error {
		QueryLoweringError::UnsupportedBackend { backend } => {
			format!(
				"Database query execution is not implemented yet for the `{}` backend.",
				database_backend_name(backend),
			)
		}
		QueryLoweringError::UnsupportedOperator { backend, operator } => {
			format!(
				"Database query operator `{}` is not implemented yet for the `{}` backend.",
				query_binary_operator_name(operator),
				database_backend_name(backend),
			)
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
		Statement::ForRecord(statement) => statement.position,
		Statement::FunctionDeclaration(statement) => statement.position,
		Statement::If(statement) => statement.position,
		Statement::RecordPointerDeclaration(statement) => statement.position,
		Statement::Return(statement) => statement.position,
		Statement::VariableDeclaration(statement) => statement.position,
		Statement::While(statement) => statement.position,
	}
}

#[cfg(test)]
mod tests {
	use crate::ast::BlockStatement;
	use crate::ast::Expr;
	use crate::ast::IdentifierExpr;
	use crate::ast::IfStatement;
	use crate::ast::RecordPointerType;
	use crate::ast::Statement;
	use crate::query::QueryBinaryExpr;
	use crate::query::QueryBinaryOperator;
	use crate::query::QueryColumnReference;
	use crate::query::QueryCountPlan;
	use crate::query::QueryExpr;
	use crate::query::QueryLiteral;
	use crate::query::QueryParameter;
	use crate::schema::DatabaseBackend;
	use crate::schema_fixture::read_schema_catalog_from_str;
	use crate::source::SourceText;
	use crate::syntax::lexer::Lexer;
	use crate::syntax::parser::Parser;

	use super::DataType;
	use super::LocalBinding;
	use super::SemanticAnalyzer;

	fn parse_count_expression(source: &str) -> crate::ast::CountExpr {
		match parse_expression(source) {
			Expr::Count(count) => count,
			other => panic!("Expected count expression, found {other:?}."),
		}
	}

	fn parse_expression(source: &str) -> Expr {
		let mut lexer = Lexer::new(SourceText::new(source));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		let program = parser.parse_program().unwrap();
		program.result.unwrap()
	}

	fn parse_find_expression(source: &str) -> crate::ast::FindExpr {
		match parse_expression(source) {
			Expr::Find(find) => find,
			other => panic!("Expected find expression, found {other:?}."),
		}
	}

	#[test]
	fn accepts_record_pointer_as_if_condition() {
		let statement = Statement::If(IfStatement {
			condition: Expr::Identifier(IdentifierExpr {
				name: String::from("cust"),
				position: 3,
			}),
			else_branch: None,
			position: 0,
			then_branch: BlockStatement {
				position: 8,
				statements: vec![],
			},
		});
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("cust"),
			LocalBinding {
				data_type: DataType::RecordPointer(RecordPointerType {
					database_name: String::from("ExampleDb"),
					schema_name: String::from("Main"),
					table_name: String::from("Customers"),
				}),
				is_const: false,
				slot: 2,
			},
		);

		analyzer.validate_statement(&statement).unwrap();
	}

	#[test]
	fn infers_exists_call_type_for_record_pointer() {
		let expression = parse_expression("exists(cust)");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("cust"),
			LocalBinding {
				data_type: DataType::RecordPointer(RecordPointerType {
					database_name: String::from("ExampleDb"),
					schema_name: String::from("Main"),
					table_name: String::from("Customers"),
				}),
				is_const: false,
				slot: 2,
			},
		);

		let data_type = analyzer.infer_expression_type(&expression).unwrap();

		assert_eq!(data_type, DataType::Bool);
	}

	#[test]
	fn infers_field_access_type_for_record_pointer_local() {
		let schema = read_schema_catalog_from_str(
			r#"{
				"databases": [
					{
						"backend": "sqlite",
						"name": "ExampleDb",
						"schemas": [
							{
								"name": "Main",
								"is_implicit": true,
								"tables": [
									{
										"name": "Customers",
										"columns": [
											{ "name": "Id", "data_type": "int", "is_nullable": false },
											{ "name": "Name", "data_type": "text", "is_nullable": false }
										]
									}
								]
							}
						]
					}
				]
			}"#,
		).unwrap();
		let expression = parse_expression("cust.name");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("cust"),
			LocalBinding {
				data_type: DataType::RecordPointer(RecordPointerType {
					database_name: String::from("ExampleDb"),
					schema_name: String::from("Main"),
					table_name: String::from("Customers"),
				}),
				is_const: false,
				slot: 2,
			},
		);

		let data_type = analyzer.infer_expression_type(&expression).unwrap();

		assert_eq!(data_type, DataType::Text);
	}

	#[test]
	fn infers_record_pointer_type_for_find_expression() {
		let schema = read_schema_catalog_from_str(
			r#"{
				"databases": [
					{
						"backend": "sqlite",
						"name": "ExampleDb",
						"schemas": [
							{
								"name": "Main",
								"is_implicit": true,
								"tables": [
									{
										"name": "Customers",
										"columns": [
											{ "name": "Id", "data_type": "int", "is_nullable": false },
											{ "name": "Name", "data_type": "text", "is_nullable": false }
										]
									}
								]
							}
						]
					}
				]
			}"#,
		).unwrap();
		let find = parse_find_expression("find first customers where id = 1");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.semantic_program.active_databases = vec![String::from("exampledb")];

		let data_type = analyzer.infer_find_expression_type(&find).unwrap();

		assert_eq!(data_type, DataType::RecordPointer(RecordPointerType {
			database_name: String::from("ExampleDb"),
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}));
	}

	#[test]
	fn infers_record_pointer_type_for_find_expression_with_order_by() {
		let schema = read_schema_catalog_from_str(
			r#"{
				"databases": [
					{
						"backend": "sqlite",
						"name": "ExampleDb",
						"schemas": [
							{
								"name": "Main",
								"is_implicit": true,
								"tables": [
									{
										"name": "Customers",
										"columns": [
											{ "name": "Id", "data_type": "int", "is_nullable": false },
											{ "name": "Name", "data_type": "text", "is_nullable": false }
										]
									}
								]
							}
						]
					}
				]
			}"#,
		).unwrap();
		let find = parse_find_expression("find first customers order by name desc, id");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.semantic_program.active_databases = vec![String::from("exampledb")];

		let data_type = analyzer.infer_find_expression_type(&find).unwrap();

		assert_eq!(data_type, DataType::RecordPointer(RecordPointerType {
			database_name: String::from("ExampleDb"),
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}));
	}

	#[test]
	fn lowers_count_query_to_backend_aware_ir() {
		let schema = read_schema_catalog_from_str(
			r#"{
				"databases": [
					{
						"backend": "sqlite",
						"name": "ExampleDb",
						"schemas": [
							{
								"name": "Main",
								"is_implicit": true,
								"tables": [
									{
										"name": "Customers",
										"columns": [
											{ "name": "Id", "data_type": "int", "is_nullable": false },
											{ "name": "Active", "data_type": "bool", "is_nullable": false }
										]
									}
								]
							}
						]
					}
				]
			}"#,
		).unwrap();
		let count = parse_count_expression("count customers where id = targetId and active = true");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.semantic_program.active_databases = vec![String::from("exampledb")];
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("targetId"),
			LocalBinding {
				data_type: DataType::Int,
				is_const: false,
				slot: 7,
			},
		);

		let query = analyzer.lower_count_query(&count).unwrap();

		assert_eq!(query, QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Binary(QueryBinaryExpr {
					left: Box::new(QueryExpr::Column(QueryColumnReference {
						column_name: String::from("Id"),
						data_type: DataType::Int,
						table_name: String::from("Customers"),
					})),
					operator: QueryBinaryOperator::Equal,
					right: Box::new(QueryExpr::Parameter(QueryParameter {
						data_type: DataType::Int,
						slot: 7,
					})),
				})),
				operator: QueryBinaryOperator::And,
				right: Box::new(QueryExpr::Binary(QueryBinaryExpr {
					left: Box::new(QueryExpr::Column(QueryColumnReference {
						column_name: String::from("Active"),
						data_type: DataType::Bool,
						table_name: String::from("Customers"),
					})),
					operator: QueryBinaryOperator::Equal,
					right: Box::new(QueryExpr::Literal(QueryLiteral::Boolean(true))),
				})),
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		});
	}
}
