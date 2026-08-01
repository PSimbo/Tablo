use std::collections::{ BTreeMap, BTreeSet };

use crate::ast::*;
use crate::builtins::{ BuiltInFunction, BuiltInParameterType };
use crate::bytecode::*;
use crate::compiler::CompileError;
use crate::format_string::*;
use crate::query::*;
use crate::schema::*;
use crate::source::SourceText;

use super::scope::ScopeStack;

const BUILT_IN_ENUM_TYPE_PREFIX: &str = "__tablo_builtin_";

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CallArgumentBinding {
	OmittedDefault(Expr),
	OmittedNull,
	RequestedDefault(Expr),
	Supplied(u32),
	Variadic(Vec<u32>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EnumValue {
	Constant(Constant),
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum ObjectTypeReferencePathComponent {
	ArrayElement,
	NullableValue,
	RangeElement,
	UnionMember(u32),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RecordPointerInitialization {
	Existing,
	New,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RecordPointerOrigin {
	ForLoop,
	IfBinding,
	Parameter,
	VariableDeclaration,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum CallArgumentBindingError {
	DuplicateParameter {
		argument_index: usize,
		parameter_name: String,
	},
	InvalidDefault {
		argument_index: usize,
	},
	MissingRequiredParameter {
		parameter_name: String,
	},
	NamedArgumentAfterVariadic {
		argument_index: usize,
	},
	PositionalArgumentAfterNamed {
		argument_index: usize,
	},
	TooManyArguments {
		argument_index: usize,
	},
	UnknownParameter {
		argument_index: usize,
		parameter_name: String,
	},
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum LexicalDeclarationKind {
	Enum,
	Function,
	Local,
	Object,
}

impl LexicalDeclarationKind {
	fn description(self) -> &'static str {
		match self {
			Self::Enum => "an enum",
			Self::Function => "a function",
			Self::Local => "a variable",
			Self::Object => "an object type",
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ObjectDefaultConstructionState {
	Explicit {
		object_type_id: ObjectTypeId,
		position: usize,
	},
	Implicit(ObjectTypeId),
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct DeclarationScopeId(u32);

impl DeclarationScopeId {
	const MODULE: Self = Self(0);
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GroupBoundaryCallInfo {
	pub key_names: Vec<String>,
	pub record_slot: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NewRecordColumn {
	pub data_type: DataType,
	pub name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NewRecordLayout {
	pub columns: Vec<NewRecordColumn>,
	pub record_type: RecordPointerType,
	pub schema_is_implicit: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecordPointerBindingInfo {
	pub assigned_fields: BTreeSet<String>,
	pub data_type: RecordPointerType,
	escape_positions: BTreeSet<usize>,
	pub escapes_analysis: bool,
	field_assignments: Vec<RecordPointerFieldAssignment>,
	field_reads: Vec<RecordPointerFieldRead>,
	identity_requires_all_fields: bool,
	pub identity_fields: BTreeSet<String>,
	pub initialization: RecordPointerInitialization,
	pub is_mutable: bool,
	pub origin: RecordPointerOrigin,
	query_position: Option<usize>,
	pub read_fields: BTreeSet<String>,
}

impl RecordPointerBindingInfo {
	// None means that field-list minimization is unsafe and the complete row is required.
	pub fn required_query_fields(&self) -> Option<BTreeSet<String>> {
		if self.escapes_analysis || self.identity_requires_all_fields {
			return None;
		}

		let mut fields = self.read_fields.union(&self.identity_fields).cloned().collect::<BTreeSet<_>>();
		fields.extend(self.assigned_fields.iter().filter_map(|path| path.split('.').next().map(String::from)));
		Some(fields)
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedObjectType {
	containing_object_id: Option<ObjectTypeId>,
	declaration: ObjectDeclaration,
	display_name: String,
	has_named_constructor: bool,
	id: ObjectTypeId,
	scope_id: DeclarationScopeId,
	source_name: Option<String>,
	visibility: Visibility,
}

impl ResolvedObjectType {
	pub fn containing_object_id(&self) -> Option<ObjectTypeId> {
		self.containing_object_id
	}

	pub fn declaration(&self) -> &ObjectDeclaration {
		&self.declaration
	}

	pub fn display_name(&self) -> &str {
		&self.display_name
	}

	pub fn has_named_constructor(&self) -> bool {
		self.has_named_constructor
	}

	pub fn id(&self) -> ObjectTypeId {
		self.id
	}

	pub fn scope_id(&self) -> DeclarationScopeId {
		self.scope_id
	}

	pub fn source_name(&self) -> Option<&str> {
		self.source_name.as_deref()
	}

	pub fn visibility(&self) -> Visibility {
		self.visibility
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedObjectTypeReference {
	pub object_type_id: ObjectTypeId,
	pub path: Vec<ObjectTypeReferencePathComponent>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedSequenceReference {
	pub database_name: String,
	pub schema_is_implicit: bool,
	pub schema_name: String,
	pub sequence_name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedTableReference {
	pub database_name: String,
	pub schema_is_implicit: bool,
	pub schema_name: String,
	pub table_name: String,
}

// This pass is responsible for name resolution and type checking. It does not
// emit bytecode directly, but it resolves locals and function targets so code
// generation can remain simple.
#[derive(Default)]
pub struct SemanticAnalyzer {
	current_non_null_assumptions: Vec<Expr>,
	current_return_type: Option<DataType>,
	current_schema_catalog: Option<SchemaCatalog>,
	current_source_name: Option<String>,
	declaration_kinds: ScopeStack<LexicalDeclarationKind>,
	enums: ScopeStack<EnumBinding>,
	find_lock_mode: RecordLockMode,
	function_depth: usize,
	functions: ScopeStack<Vec<FunctionSignature>>,
	function_overload_aliases: Vec<FunctionOverloadAlias>,
	group_boundary_contexts: Vec<GroupBoundaryContext>,
	locals: ScopeStack<LocalBinding>,
	loop_depth: usize,
	next_function_index: u32,
	next_local_slot: u32,
	null_comparison_operands: BTreeSet<usize>,
	object_type_bindings: ScopeStack<ObjectTypeId>,
	query_optimizations_disabled: bool,
	root_source_name: Option<String>,
	semantic_program: SemanticProgram,
	sequence_aliases: ScopeStack<Option<ResolvedSequenceReference>>,
	top_level_function_source_names: Vec<String>,
	top_level_object_display_names: Vec<String>,
	top_level_object_source_names: Vec<String>,
}

impl SemanticAnalyzer {
	pub fn analyze_program(&mut self, program: &AstProgram) -> Result<SemanticProgram, CompileError> {
		self.analyze_program_with_schema(program, None)
	}

	pub fn analyze_program_with_schema(
		&mut self,
		program: &AstProgram,
		schema_catalog: Option<&SchemaCatalog>,
	) -> Result<SemanticProgram, CompileError> {
		self.current_non_null_assumptions.clear();
		self.current_return_type = None;
		self.declaration_kinds = ScopeStack::default();
		self.functions = ScopeStack::default();
		self.enums = ScopeStack::default();
		self.find_lock_mode = RecordLockMode::None;
		self.group_boundary_contexts.clear();
		self.locals = ScopeStack::default();
		self.sequence_aliases = ScopeStack::default();
		self.function_depth = 0;
		self.loop_depth = 0;
		self.next_function_index = 0;
		self.next_local_slot = 0;
		self.null_comparison_operands.clear();
		self.object_type_bindings = ScopeStack::default();
		self.current_source_name = self.root_source_name.clone();
		self.current_schema_catalog = schema_catalog.cloned();
		self.semantic_program = SemanticProgram::default();

		self.validate_with_declarations(&program.with_declarations, schema_catalog)?;
		self.declaration_kinds.enter_scope();
		self.object_type_bindings.enter_scope();
		self.collect_object_declarations(&program.objects)?;
		self.current_source_name = self.root_source_name.clone();
		self.enums.enter_scope();
		self.collect_scope_enum_declarations(&program.statements)?;
		self.functions.enter_scope();
		self.collect_scope_function_signatures(&program.functions, &program.statements)?;
		self.collect_function_overload_aliases()?;

		self.validate_object_type_graph(&program.objects)?;

		self.current_source_name = self.root_source_name.clone();
		for (index, function) in program.functions.iter().enumerate() {
			self.current_source_name = self.top_level_function_source_names.get(index)
				.cloned()
				.or_else(|| self.root_source_name.clone());
			self.validate_function_declaration(function)?;
		}

		self.current_source_name = self.root_source_name.clone();

		self.enter_scope();

		for statement in &program.statements {
			self.validate_statement(statement)?;
		}

		if let Some(result) = &program.result {
			self.infer_expression_type(result)?;
		}

		self.exit_scope();
		self.functions.exit_scope();
		self.enums.exit_scope();
		self.object_type_bindings.exit_scope();
		self.declaration_kinds.exit_scope();
		self.finalize_record_pointer_usage(program);
		self.apply_record_pointer_field_selections()?;
		let mut query_plan = plan_program_queries(program);
		query_plan.populate_analyzed_metadata(|kind, position, record_binding_position| {
			match kind {
				PlannedQueryKind::Count => self.semantic_program.lowered_count_queries.get(&position)
					.map(|query| AnalyzedQueryMetadata {
						backend_capabilities: query_planning_capabilities(query.backend),
						captured_parameters: query.captured_parameters(),
						database_name: query.database_name.clone(),
						expressions_are_infallible: query.expressions_are_infallible(),
						is_read_only: true,
						record_slot: None,
						result_semantics: PlannedQueryResultSemantics {
							cardinality: PlannedQueryResultCardinality::Scalar,
							error_timing: PlannedQueryErrorTiming::AtQueryStart,
							has_grouping: false,
							has_limit: false,
							has_ordering: false,
						},
					}),
				PlannedQueryKind::Find => self.semantic_program.lowered_find_queries.get(&position)
					.map(|query| AnalyzedQueryMetadata {
						backend_capabilities: query_planning_capabilities(query.backend),
						captured_parameters: query.captured_parameters(),
						database_name: query.database_name.clone(),
						expressions_are_infallible: query.expressions_are_infallible(),
						is_read_only: query.lock_mode == RecordLockMode::None,
						record_slot: None,
						result_semantics: PlannedQueryResultSemantics {
							cardinality: PlannedQueryResultCardinality::AtMostOne,
							error_timing: PlannedQueryErrorTiming::AtQueryStart,
							has_grouping: false,
							has_limit: true,
							has_ordering: !query.order_by.is_empty(),
						},
					}),
				PlannedQueryKind::ForRecord => self.semantic_program.lowered_for_record_queries.get(&position)
					.map(|query| AnalyzedQueryMetadata {
						backend_capabilities: query_planning_capabilities(query.backend),
						captured_parameters: query.captured_parameters(),
						database_name: query.database_name.clone(),
						expressions_are_infallible: query.expressions_are_infallible(),
						is_read_only: query.lock_mode == RecordLockMode::None,
						record_slot: record_binding_position
							.and_then(|position| self.semantic_program.declaration_slots.get(&position).copied()),
						result_semantics: PlannedQueryResultSemantics {
							cardinality: PlannedQueryResultCardinality::Many,
							error_timing: PlannedQueryErrorTiming::AtQueryStart,
							has_grouping: !query.group_by.is_empty(),
							has_limit: query.limit.is_some(),
							has_ordering: !query.order_by.is_empty() || !query.group_by.is_empty(),
						},
					}),
			}
		});
		if self.query_optimizations_disabled {
			query_plan.disable_optimizations();
		}
		self.record_selected_query_shapes(&query_plan)?;
		self.compile_selected_query_shapes()?;
		self.semantic_program.query_plan = query_plan;

		Ok(self.semantic_program.clone())
	}

	pub fn analyze_standalone_program(&mut self, program: &AstProgram) -> Result<SemanticProgram, CompileError> {
		self.analyze_standalone_program_with_schema(program, None)
	}

	pub fn analyze_standalone_program_with_schema(
		&mut self,
		program: &AstProgram,
		schema_catalog: Option<&SchemaCatalog>,
	) -> Result<SemanticProgram, CompileError> {
		let semantic_program = self.analyze_program_with_schema(program, schema_catalog)?;
		let Some(main_function) = program.functions.iter().find(|function| function.name == "Main") else {
			return Err(self.compile_error(
				0,
				String::from("Standalone Tablo programs must define `fn Main(args: [text]): int`."),
			));
		};

		self.validate_main_entry_point(program, main_function)?;

		let mut semantic_program = semantic_program;
		semantic_program.entry_point_function_index = semantic_program.function_declaration_target(main_function.position);
		semantic_program.entry_point_position = Some(main_function.position);
		Ok(semantic_program)
	}

	pub fn new() -> Self {
		let analyzer = Self {
			current_non_null_assumptions: Vec::new(),
			current_return_type: None,
			current_schema_catalog: None,
			current_source_name: None,
			declaration_kinds: ScopeStack::default(),
			enums: ScopeStack::default(),
			find_lock_mode: RecordLockMode::None,
			function_depth: 0,
			functions: ScopeStack::default(),
			function_overload_aliases: Vec::new(),
			group_boundary_contexts: Vec::new(),
			locals: ScopeStack::default(),
			loop_depth: 0,
			next_function_index: 0,
			next_local_slot: 0,
			null_comparison_operands: BTreeSet::new(),
			object_type_bindings: ScopeStack::default(),
			query_optimizations_disabled: false,
			root_source_name: None,
			semantic_program: SemanticProgram::default(),
			sequence_aliases: ScopeStack::default(),
			top_level_function_source_names: Vec::new(),
			top_level_object_display_names: Vec::new(),
			top_level_object_source_names: Vec::new(),
		};
		analyzer.assert_valid_built_in_overload_sets();
		analyzer
	}

	pub fn set_query_optimizations_enabled(&mut self, enabled: bool) {
		self.query_optimizations_disabled = !enabled;
	}

	pub fn set_root_source_name(&mut self, source_name: Option<String>) {
		self.root_source_name = source_name;
	}

	pub fn set_top_level_function_source_names(&mut self, source_names: Vec<String>) {
		self.top_level_function_source_names = source_names;
	}

	pub fn set_top_level_object_display_names(&mut self, display_names: Vec<String>) {
		self.top_level_object_display_names = display_names;
	}

	pub fn set_top_level_object_source_names(&mut self, source_names: Vec<String>) {
		self.top_level_object_source_names = source_names;
	}

	pub fn validate_program(&mut self, program: &AstProgram) -> Result<(), CompileError> {
		self.analyze_program(program).map(|_| ())
	}

	pub(crate) fn set_function_overload_aliases(&mut self, aliases: Vec<FunctionOverloadAlias>) {
		self.function_overload_aliases = aliases;
	}

	fn apply_query_field_selection(
		layout: &mut QueryRecordLayout,
		required_fields: Option<&BTreeSet<String>>,
	) {
		let Some(required_fields) = required_fields else {
			layout.selection = QueryColumnSelection::All;
			return;
		};
		let QueryRecordSchema::Known(columns) = &layout.schema else {
			layout.selection = QueryColumnSelection::RuntimeDetermined;
			return;
		};
		let mut indices = Vec::with_capacity(required_fields.len());

		for field_name in required_fields {
			let Some(index) = columns.iter().position(|column| column.column_name.eq_ignore_ascii_case(field_name)) else {
				layout.selection = QueryColumnSelection::All;
				return;
			};
			indices.push(index as u32);
		}

		indices.sort_unstable();
		indices.dedup();
		layout.selection = QueryColumnSelection::Indices(indices);
	}

	fn apply_record_pointer_field_selections(&mut self) -> Result<(), CompileError> {
		let mut requirements = BTreeMap::<usize, Option<BTreeSet<String>>>::new();

		for binding in self.semantic_program.record_pointer_bindings.values() {
			let Some(query_position) = binding.query_position else {
				continue;
			};
			let fields = binding.required_query_fields();

			requirements.entry(query_position)
				.and_modify(|existing| {
					if let (Some(existing), Some(fields)) = (existing.as_mut(), fields.as_ref()) {
						existing.extend(fields.iter().cloned());
					}
					else {
						*existing = None;
					}
				})
				.or_insert(fields);
		}

		for (query_position, required_fields) in requirements {
			if let Some(plan) = self.semantic_program.lowered_find_queries.get_mut(&query_position) {
				Self::apply_query_field_selection(&mut plan.record_layout, required_fields.as_ref());
				let plan = plan.clone();
				let compiled_query = lower_find_query(&plan).map_err(|error| self.compile_error(
					query_position,
					query_lowering_error_message(error),
				))?;
				self.semantic_program.compiled_find_queries.insert(query_position, compiled_query);
			}
			else if let Some(plan) = self.semantic_program.lowered_for_record_queries.get_mut(&query_position) {
				Self::apply_query_field_selection(&mut plan.record_layout, required_fields.as_ref());
				let plan = plan.clone();
				let compiled_query = lower_for_query(&plan).map_err(|error| self.compile_error(
					query_position,
					query_lowering_error_message(error),
				))?;
				self.semantic_program.compiled_for_record_queries.insert(query_position, compiled_query);
			}
		}

		Ok(())
	}

	fn assert_valid_built_in_overload_sets(&self) {
		for built_in in BuiltInFunction::all() {
			let signatures = self.built_in_validation_signatures(*built_in);
			let unselectable = self.unselectable_overload_indices(&signatures);
			assert!(
				unselectable.is_empty(),
				"Built-in function `{}` has unselectable overloads at indices {unselectable:?}.",
				built_in.name(),
			);
		}
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
		let lhs_non_null = lhs.is_non_nullable();
		let rhs_non_null = rhs.is_non_nullable();

		if matches!(operator, BinaryOperator::Equal | BinaryOperator::NotEqual) {
			match (lhs, rhs) {
				(DataType::Null, DataType::Null) => return Ok(DataType::Bool),
				(DataType::Null, compared) | (compared, DataType::Null) => {
					let compared = compared.without_nullability();
					self.require_equality_operands(compared, compared, position)?;
					return Ok(DataType::Bool);
				}
				_ => {}
			}
		}

		let lhs = lhs.without_nullability();
		let rhs = rhs.without_nullability();

		let result = match operator {
			BinaryOperator::Add => {
				if lhs == &DataType::Text || rhs == &DataType::Text {
					Ok(DataType::Text)
				}
				else {
					match (lhs, rhs) {
						(DataType::Array(lhs_element), DataType::Array(rhs_element)) => {
							Ok(DataType::Array(Box::new(
								self.merge_array_element_types(lhs_element, rhs_element, position)?,
							)))
						}
						(DataType::Array(element_type), DataType::EmptyArray)
						| (DataType::EmptyArray, DataType::Array(element_type)) => {
							Ok(DataType::Array(element_type.clone()))
						}
						(DataType::EmptyArray, DataType::EmptyArray) => {
							Ok(DataType::EmptyArray)
						}
						_ => self.numeric_result_type(lhs, rhs, position),
					}
				}
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
		}?;

		Ok(if !lhs_non_null || !rhs_non_null {
			result.into_nullable()
		}
		else {
			result
		})
	}

	fn bind_call_arguments(
		&self,
		signature: &FunctionSignature,
		arguments: &[CallArgument],
	) -> Result<Vec<CallArgumentBinding>, CallArgumentBindingError> {
		let mut bindings = vec![None; signature.parameters.len()];
		let mut next_positional_parameter = 0;
		let mut saw_named_argument = false;
		let mut saw_variadic_argument = false;
		let variadic_parameter_index = signature.parameters.iter()
			.position(|parameter| parameter.is_variadic);

		for (argument_index, argument) in arguments.iter().enumerate() {
			let parameter_index = if let Some(name) = &argument.name {
				if saw_variadic_argument {
					return Err(CallArgumentBindingError::NamedArgumentAfterVariadic {
						argument_index,
					});
				}
				saw_named_argument = true;
				signature.parameters.iter()
					.position(|parameter| parameter.name == name.name)
					.ok_or_else(|| CallArgumentBindingError::UnknownParameter {
						argument_index,
						parameter_name: name.name.clone(),
					})?
			}
			else {
				if saw_named_argument {
					let parameter_index = variadic_parameter_index.ok_or(
						CallArgumentBindingError::PositionalArgumentAfterNamed {
							argument_index,
						},
					)?;
					match &mut bindings[parameter_index] {
						None => {
							bindings[parameter_index] = Some(CallArgumentBinding::Variadic(vec![argument_index as u32]));
						}
						Some(CallArgumentBinding::Variadic(argument_indices)) => {
							argument_indices.push(argument_index as u32);
						}
						Some(_) => {
							return Err(CallArgumentBindingError::DuplicateParameter {
								argument_index,
								parameter_name: signature.parameters[parameter_index].name.clone(),
							});
						}
					}
					saw_variadic_argument = true;
					continue;
				}

				if Some(next_positional_parameter) == variadic_parameter_index {
					match &mut bindings[next_positional_parameter] {
						None => {
							bindings[next_positional_parameter] = Some(CallArgumentBinding::Variadic(vec![argument_index as u32]));
						}
						Some(CallArgumentBinding::Variadic(argument_indices)) => {
							argument_indices.push(argument_index as u32);
						}
						Some(_) => {
							return Err(CallArgumentBindingError::DuplicateParameter {
								argument_index,
								parameter_name: signature.parameters[next_positional_parameter].name.clone(),
							});
						}
					}
					saw_variadic_argument = true;
					continue;
				}

				let parameter_index = next_positional_parameter;
				next_positional_parameter += 1;
				parameter_index
			};

			let Some(binding) = bindings.get(parameter_index) else {
				return Err(CallArgumentBindingError::TooManyArguments {
					argument_index,
				});
			};
			if binding.is_some() {
				return Err(CallArgumentBindingError::DuplicateParameter {
					argument_index,
					parameter_name: signature.parameters[parameter_index].name.clone(),
				});
			}

			if argument.default_argument().is_some() {
				let parameter = &signature.parameters[parameter_index];
				if parameter.is_by_ref || parameter.is_variadic {
					return Err(CallArgumentBindingError::InvalidDefault {
						argument_index,
					});
				}
				let Some(default_value) = signature.parameter_defaults[parameter_index].clone() else {
					return Err(CallArgumentBindingError::InvalidDefault {
						argument_index,
					});
				};
				bindings[parameter_index] = Some(CallArgumentBinding::RequestedDefault(default_value));
			}
			else {
				bindings[parameter_index] = Some(CallArgumentBinding::Supplied(argument_index as u32));
			}
		}

		for (parameter_index, binding) in bindings.iter_mut().enumerate() {
			if binding.is_some() {
				continue;
			}

			let parameter = &signature.parameters[parameter_index];
			if parameter.is_by_ref {
				return Err(CallArgumentBindingError::MissingRequiredParameter {
					parameter_name: parameter.name.clone(),
				});
			}

			*binding = if parameter.is_variadic {
				Some(CallArgumentBinding::Variadic(Vec::new()))
			}
			else if let Some(default_value) = &signature.parameter_defaults[parameter_index] {
				Some(CallArgumentBinding::OmittedDefault(default_value.clone()))
			}
			else if parameter.data_type.is_nullable() {
				Some(CallArgumentBinding::OmittedNull)
			}
			else {
				return Err(CallArgumentBindingError::MissingRequiredParameter {
					parameter_name: parameter.name.clone(),
				});
			};
		}

		Ok(bindings.into_iter().map(Option::unwrap).collect())
	}

	fn block_guarantees_return(&self, block: &BlockStatement) -> bool {
		block.statements.iter().any(|statement| self.statement_guarantees_return(statement))
	}

	fn built_in_validation_data_type(data_type: BuiltInParameterType) -> Option<DataType> {
		Some(match data_type {
			BuiltInParameterType::Any => DataType::Any,
			BuiltInParameterType::ArrayAny => DataType::Array(Box::new(DataType::Any)),
			BuiltInParameterType::ArrayText => DataType::Array(Box::new(DataType::Text)),
			BuiltInParameterType::Bool => DataType::Bool,
			BuiltInParameterType::Date => DataType::Date,
			BuiltInParameterType::Dec => DataType::Dec,
			BuiltInParameterType::EnumBacked(backing_type) => {
				DataType::Object(format!("{BUILT_IN_ENUM_TYPE_PREFIX}{}_enum", backing_type.name()).into())
			}
			BuiltInParameterType::Int => DataType::Int,
			BuiltInParameterType::Sequence => return None,
			BuiltInParameterType::Text => DataType::Text,
			BuiltInParameterType::Time => DataType::Time,
			BuiltInParameterType::TimeTz => DataType::TimeTz,
			BuiltInParameterType::Timestamp => DataType::Timestamp,
			BuiltInParameterType::TimestampTz => DataType::TimestampTz,
		})
	}

	fn built_in_validation_signatures(&self, built_in: BuiltInFunction) -> Vec<FunctionSignature> {
		built_in.signatures().into_iter()
			.enumerate()
			.filter_map(|(function_index, signature)| {
				let parameters = signature.parameters.into_iter()
					.map(|parameter| {
						Some(FunctionParameterSignature {
							data_type: Self::built_in_validation_data_type(parameter.data_type)?,
							has_default: false,
							is_by_ref: false,
							is_variadic: parameter.is_variadic,
							name: parameter.name.to_string(),
							sequence: None,
						})
					})
					.collect::<Option<Vec<_>>>()?;

				Some(FunctionSignature {
					function_index: function_index as u32,
					parameter_defaults: vec![None; parameters.len()],
					parameters,
					return_type: signature.return_type,
				})
			})
			.collect()
	}

	fn call_argument_binding_compile_error(
		&self,
		callable_kind: &str,
		name: &str,
		arguments: &[CallArgument],
		call_position: usize,
		error: CallArgumentBindingError,
	) -> CompileError {
		match error {
			CallArgumentBindingError::DuplicateParameter { argument_index, parameter_name } => {
				let argument = &arguments[argument_index];
				self.compile_error(
					argument.name.as_ref().map_or(argument.position, |name| name.position),
					format!(
						"Parameter `{parameter_name}` is supplied more than once in the call to {callable_kind} `{name}`.",
					),
				)
			}
			CallArgumentBindingError::InvalidDefault { argument_index } => {
				let argument = &arguments[argument_index];
				self.compile_error(
					argument.default_argument().map_or(argument.position, |default| default.position),
					format!("`default` is not valid for this argument to {callable_kind} `{name}`."),
				)
			}
			CallArgumentBindingError::MissingRequiredParameter { parameter_name } => {
				self.compile_error(
					call_position,
					format!(
						"Required parameter `{parameter_name}` is not supplied in the call to {callable_kind} `{name}`.",
					),
				)
			}
			CallArgumentBindingError::NamedArgumentAfterVariadic { argument_index } => {
				let argument = &arguments[argument_index];
				self.compile_error(
					argument.name.as_ref().map_or(argument.position, |name| name.position),
					format!(
						"A named argument cannot follow trailing variadic arguments in the call to {callable_kind} `{name}`.",
					),
				)
			}
			CallArgumentBindingError::PositionalArgumentAfterNamed { argument_index } => {
				self.compile_error(
					arguments[argument_index].position,
					format!(
						"A positional argument cannot follow a named argument unless it binds to a variadic parameter in the call to {callable_kind} `{name}`.",
					),
				)
			}
			CallArgumentBindingError::TooManyArguments { argument_index } => {
				self.compile_error(
					arguments[argument_index].position,
					format!("Too many arguments were supplied to {callable_kind} `{name}`."),
				)
			}
			CallArgumentBindingError::UnknownParameter { argument_index, parameter_name } => {
				let argument = &arguments[argument_index];
				self.compile_error(
					argument.name.as_ref().map_or(argument.position, |name| name.position),
					format!(
						"Named argument `{parameter_name}` does not match any parameter of {callable_kind} `{name}`.",
					),
				)
			}
		}
	}

	fn call_argument_binding_rejection_description(&self, error: &CallArgumentBindingError) -> String {
		match error {
			CallArgumentBindingError::DuplicateParameter { parameter_name, .. } => {
				format!("binds parameter `{parameter_name}` more than once")
			}
			CallArgumentBindingError::InvalidDefault { .. } => {
				String::from("uses `default` for a parameter that does not declare a usable default")
			}
			CallArgumentBindingError::MissingRequiredParameter { parameter_name } => {
				format!("does not supply required parameter `{parameter_name}`")
			}
			CallArgumentBindingError::NamedArgumentAfterVariadic { .. } => {
				String::from("places a named argument after trailing variadic arguments")
			}
			CallArgumentBindingError::PositionalArgumentAfterNamed { .. } => {
				String::from("places a positional argument after a named argument without a variadic parameter")
			}
			CallArgumentBindingError::TooManyArguments { .. } => {
				String::from("does not accept this many arguments")
			}
			CallArgumentBindingError::UnknownParameter { parameter_name, .. } => {
				format!("has no parameter named `{parameter_name}`")
			}
		}
	}

	fn call_argument_types_in_binding_order(
		argument_types: &[DataType],
		bindings: &[CallArgumentBinding],
	) -> Vec<DataType> {
		bindings.iter()
			.flat_map(|binding| {
				match binding {
					CallArgumentBinding::Supplied(argument_index) => {
						vec![argument_types[*argument_index as usize].clone()]
					}
					CallArgumentBinding::Variadic(argument_indices) => {
						argument_indices.iter()
							.map(|argument_index| argument_types[*argument_index as usize].clone())
							.collect()
					}
					CallArgumentBinding::OmittedDefault(_)
					| CallArgumentBinding::OmittedNull
					| CallArgumentBinding::RequestedDefault(_) => Vec::new(),
				}
			})
			.collect()
	}

	fn call_arguments_in_binding_order<'a>(
		arguments: &'a [CallArgument],
		bindings: &[CallArgumentBinding],
	) -> Vec<&'a CallArgument> {
		bindings.iter()
			.flat_map(|binding| {
				match binding {
					CallArgumentBinding::Supplied(argument_index) => {
						vec![&arguments[*argument_index as usize]]
					}
					CallArgumentBinding::Variadic(argument_indices) => {
						argument_indices.iter()
							.map(|argument_index| &arguments[*argument_index as usize])
							.collect()
					}
					CallArgumentBinding::OmittedDefault(_)
					| CallArgumentBinding::OmittedNull
					| CallArgumentBinding::RequestedDefault(_) => Vec::new(),
				}
			})
			.collect()
	}

	fn call_binding_accepts_types(
		&self,
		parameter: &FunctionParameterSignature,
		binding: &CallArgumentBinding,
		arguments: &[CallArgument],
		argument_sequences: &[Option<ResolvedSequenceReference>],
		argument_types: &[Option<DataType>],
	) -> bool {
		self.call_binding_type_rejection(
			parameter,
			binding,
			arguments,
			argument_sequences,
			argument_types,
		).is_none()
	}

	fn call_binding_type_rejection(
		&self,
		parameter: &FunctionParameterSignature,
		binding: &CallArgumentBinding,
		arguments: &[CallArgument],
		argument_sequences: &[Option<ResolvedSequenceReference>],
		argument_types: &[Option<DataType>],
	) -> Option<String> {
		match binding {
			CallArgumentBinding::Supplied(argument_index) => {
				let argument_index = *argument_index as usize;
				let argument = &arguments[argument_index];
				let argument_type = argument_types[argument_index].as_ref()
					.expect("Supplied call argument bindings must refer to expressions.");
				if let Some(expected_sequence) = &parameter.sequence {
					return if !argument.is_by_ref
						&& argument_sequences[argument_index].as_ref() == Some(expected_sequence) {
						None
					}
					else {
						Some(format!(
							"rejected argument for parameter `{}`: expected sequence `{}.{}`",
							parameter.name,
							expected_sequence.schema_name,
							expected_sequence.sequence_name,
						))
					};
				}
				if parameter.is_by_ref != argument.is_by_ref {
					return Some(if parameter.is_by_ref {
						format!("requires parameter `{}` to be passed by reference", parameter.name)
					}
					else {
						format!("requires parameter `{}` to be passed by value", parameter.name)
					});
				}

				let accepts_type = if parameter.is_by_ref {
					parameter.data_type == *argument_type
				}
				else {
					self.is_function_argument_assignable(&parameter.data_type, argument_type)
				};
				if accepts_type {
					None
				}
				else {
					Some(format!(
						"rejected argument for parameter `{}`: expected `{}`, found `{}`",
						parameter.name,
						parameter.data_type.name(),
						argument_type.name(),
					))
				}
			}
			CallArgumentBinding::Variadic(argument_indices) => {
				let DataType::Array(element_type) = &parameter.data_type else {
					return Some(format!("has an invalid variadic parameter `{}`", parameter.name));
				};
				argument_indices.iter().find_map(|argument_index| {
					let argument_index = *argument_index as usize;
					let argument_type = argument_types[argument_index].as_ref()
						.expect("Variadic call argument bindings must refer to expressions.");
					if arguments[argument_index].is_by_ref {
						Some(format!("requires variadic parameter `{}` to be passed by value", parameter.name))
					}
					else if self.is_function_argument_assignable(element_type, argument_type) {
						None
					}
					else {
						Some(format!(
							"rejected argument for variadic parameter `{}`: expected `{}`, found `{}`",
							parameter.name,
							element_type.name(),
							argument_type.name(),
						))
					}
				})
			}
			CallArgumentBinding::OmittedDefault(_)
			| CallArgumentBinding::OmittedNull
			| CallArgumentBinding::RequestedDefault(_) => None,
		}
	}

	fn collect_function_overload_aliases(&mut self) -> Result<(), CompileError> {
		for alias in self.function_overload_aliases.clone() {
			let mut signatures = Vec::new();
			for target_name in &alias.target_names {
				let target_signatures = self.functions.lookup(target_name)
					.unwrap_or_else(|| panic!("Missing imported function target `{target_name}`."));
				signatures.extend(target_signatures.iter().cloned());
			}

			self.current_source_name = Some(alias.source_name.clone());
			self.validate_function_overload_set(
				&alias.display_name,
				&signatures,
				alias.position,
			)?;
			self.declaration_kinds.declare(
				alias.alias_name.clone(),
				LexicalDeclarationKind::Function,
			);
			self.functions.declare(alias.alias_name, signatures);
		}
		self.current_source_name = self.root_source_name.clone();
		Ok(())
	}

	fn collect_object_declarations(&mut self, objects: &[ObjectDeclaration]) -> Result<(), CompileError> {
		for (index, object) in objects.iter().enumerate() {
			self.current_source_name = self.top_level_object_source_names.get(index)
				.cloned()
				.or_else(|| self.root_source_name.clone());
			if let Some(existing) = self.current_scope_declaration_kind(&object.name) {
				if existing == LexicalDeclarationKind::Object {
					return Err(self.compile_error(
						object.position,
						format!("Object `{}` is already declared in this scope.", object.name),
					));
				}
				return Err(self.declaration_conflict_error(
					"Object",
					&object.name,
					object.position,
					existing,
				));
			}

			let id = ObjectTypeId(
				u32::try_from(self.semantic_program.object_types.len())
					.expect("Object type count exceeded the supported identity range."),
			);
			self.semantic_program.object_type_ids_by_name.insert(object.name.clone(), id);
			self.semantic_program.object_types.insert(id, ResolvedObjectType {
				containing_object_id: None,
				declaration: object.clone(),
				display_name: self.top_level_object_display_names.get(index)
					.cloned()
					.unwrap_or_else(|| object.name.clone()),
				has_named_constructor: object.has_explicit_name
					&& matches!(object.shape, ObjectDeclarationShape::Fields(_)),
				id,
				scope_id: DeclarationScopeId::MODULE,
				source_name: self.current_source_name.clone(),
				visibility: object.visibility,
			});
			self.declaration_kinds.declare(object.name.clone(), LexicalDeclarationKind::Object);
			self.object_type_bindings.declare(object.name.clone(), id);
		}

		for object in objects {
			let Some(containing_name) = &object.containing_object_name else {
				continue;
			};
			let containing_object_id = self.lookup_object_type_id(containing_name)
				.expect("Parser produced an inline object without its containing declaration.");
			let object_type_id = self.lookup_object_type_id(&object.name)
				.expect("Collected object type is missing its resolved identity.");
			self.semantic_program.object_types.get_mut(&object_type_id)
				.expect("Collected object type is missing its semantic declaration.")
				.containing_object_id = Some(containing_object_id);
		}

		self.resolve_inline_object_visibility();
		Ok(())
	}

	fn collect_referenced_object_names(data_type: &DataType, names: &mut BTreeSet<String>) {
		match data_type {
			DataType::Array(element_type) | DataType::Nullable(element_type) => {
				Self::collect_referenced_object_names(element_type, names);
			}
			DataType::Object(name) => {
				names.insert(name.to_string());
			}
			DataType::Union(member_types) => {
				for member_type in member_types {
					Self::collect_referenced_object_names(member_type, names);
				}
			}
			_ => {}
		}
	}

	fn collect_scope_enum_declarations(&mut self, statements: &[Statement]) -> Result<(), CompileError> {
		for statement in statements {
			if let Statement::EnumDeclaration(enum_declaration) = statement {
				self.declare_enum_signature(enum_declaration)?;
			}
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

	fn compile_error(&self, position: usize, message: impl Into<String>) -> CompileError {
		let message = message.into();

		CompileError {
			message: self.current_source_name.as_ref()
				.filter(|source_name| Some(source_name.as_str()) != self.root_source_name.as_deref())
				.map(|source_name| crate::encode_external_source_diagnostic("Compile error", source_name, position, &message))
				.unwrap_or(message),
			position,
		}
	}

	fn compile_selected_query_shapes(&mut self) -> Result<(), CompileError> {
		let shapes = self.semantic_program.query_for_shapes.iter()
			.map(|(position, shape)| (*position, shape.clone()))
			.collect::<Vec<_>>();

		for (position, shape) in shapes {
			let compiled_shape = lower_for_shape(&shape).map_err(|error| self.compile_error(
				position,
				query_lowering_error_message(error),
			))?;
			self.semantic_program.compiled_query_for_shapes.insert(position, compiled_shape);
		}

		Ok(())
	}

	fn current_scope_declaration_kind(&self, name: &str) -> Option<LexicalDeclarationKind> {
		if !self.declaration_kinds.contains_in_current_scope(name) {
			return None;
		}
		self.declaration_kinds.lookup(name).copied()
	}

	fn data_type_display_name(&self, data_type: &DataType) -> String {
		match data_type {
			DataType::Array(element_type) => format!("[{}]", self.data_type_display_name(element_type)),
			DataType::Nullable(inner) => format!("{}?", self.data_type_display_name(inner)),
			DataType::Object(name) => self.lookup_object_type(name)
				.map(|object_type| object_type.display_name().to_string())
				.unwrap_or_else(|| name.name.clone()),
			DataType::Range(element_type) => format!("range<{}>", self.data_type_display_name(element_type)),
			DataType::Union(members) => members.iter()
				.map(|member| self.data_type_display_name(member))
				.collect::<Vec<_>>()
				.join(" | "),
			other => other.name(),
		}
	}

	fn data_type_from_schema_column(&self, data_type: &SchemaDataType, is_nullable: bool) -> Result<DataType, CompileError> {
		let data_type = self.data_type_from_schema_type(data_type)?;
		Ok(if is_nullable {
			data_type.into_nullable()
		}
		else {
			data_type
		})
	}

	fn data_type_from_schema_type(&self, data_type: &SchemaDataType) -> Result<DataType, CompileError> {
		match data_type {
			SchemaDataType::Array(element_type) => Ok(DataType::Array(Box::new(self.data_type_from_schema_type(element_type)?))),
			SchemaDataType::Bool => Ok(DataType::Bool),
			SchemaDataType::Date => Ok(DataType::Date),
			SchemaDataType::Dec | SchemaDataType::Float => Ok(DataType::Dec),
			SchemaDataType::Int => Ok(DataType::Int),
			SchemaDataType::Text => Ok(DataType::Text),
			SchemaDataType::Time => Ok(DataType::Time),
			SchemaDataType::TimeTz => Ok(DataType::TimeTz),
			SchemaDataType::Timestamp => Ok(DataType::Timestamp),
			SchemaDataType::TimestampTz => Ok(DataType::TimestampTz),
			other => Err(self.compile_error(
				0,
				format!("Schema data type `{:?}` is not yet supported by database query semantic analysis.", other),
			)),
		}
	}

	fn data_type_has_implicit_default(&self, data_type: &DataType) -> bool {
		match data_type {
			DataType::Any
			| DataType::Array(_)
			| DataType::Bool
			| DataType::Date
			| DataType::Dec
			| DataType::Int
			| DataType::Object(_)
			| DataType::Text
			| DataType::Time
			| DataType::TimeTz
			| DataType::Timestamp
			| DataType::TimestampTz
			| DataType::Union(_) => true,
			DataType::Nullable(_) => true,
			DataType::EmptyArray
			| DataType::Null
			| DataType::Range(_)
			| DataType::RecordPointer(_) => false,
		}
	}

	fn declaration_conflict_error(
		&self,
		declaration: &str,
		name: &str,
		position: usize,
		existing: LexicalDeclarationKind,
	) -> CompileError {
		self.compile_error(
			position,
			format!(
				"{declaration} `{name}` conflicts with {} declared in the same scope.",
				existing.description(),
			),
		)
	}

	fn declare_enum_signature(&mut self, enum_declaration: &EnumDeclaration) -> Result<(), CompileError> {
		if let Some(existing) = self.current_scope_declaration_kind(&enum_declaration.name) {
			if existing == LexicalDeclarationKind::Enum {
				return Err(self.compile_error(
					enum_declaration.position,
					format!("Enum `{}` is already declared in this scope.", enum_declaration.name),
				));
			}
			return Err(self.declaration_conflict_error(
				"Enum",
				&enum_declaration.name,
				enum_declaration.position,
				existing,
			));
		}

		let variants = self.resolve_enum_variants(enum_declaration)?;
		self.declaration_kinds.declare(enum_declaration.name.clone(), LexicalDeclarationKind::Enum);
		self.enums.declare(enum_declaration.name.clone(), EnumBinding {
			backing_type: enum_declaration.backing_type.clone(),
			variants: variants.clone(),
		});
		self.semantic_program.enum_declarations.insert(enum_declaration.name.clone(), enum_declaration.clone());
		self.semantic_program.enum_variants.insert(enum_declaration.name.clone(), variants);
		Ok(())
	}

	fn declare_function_signature(&mut self, function: &FunctionDeclaration) -> Result<(), CompileError> {
		match self.current_scope_declaration_kind(&function.name) {
			Some(LexicalDeclarationKind::Function) => {}
			Some(existing) => {
				return Err(self.declaration_conflict_error(
					"Function",
					&function.name,
					function.position,
					existing,
				));
			}
			None => {
				self.declaration_kinds.declare(function.name.clone(), LexicalDeclarationKind::Function);
			}
		}

		let mut parameters = Vec::with_capacity(function.parameters.len());

		for parameter in &function.parameters {
			if parameter.is_by_ref && parameter.default_value.is_some() {
				return Err(self.compile_error(
					parameter.position,
					format!("By-reference parameter `{}` cannot define a default value.", parameter.name),
				));
			}
			if matches!(&parameter.data_type, FunctionParameterType::Sequence(_)) {
				if parameter.is_by_ref {
					return Err(self.compile_error(
						parameter.position,
						format!("Sequence parameter `{}` already refers to its underlying sequence and cannot use `&`.", parameter.name),
					));
				}
				if parameter.default_value.is_some() {
					return Err(self.compile_error(
						parameter.position,
						format!("Sequence parameter `{}` cannot define a default value.", parameter.name),
					));
				}
			}

			let (parameter_type, sequence) = self.resolve_function_parameter_type(parameter)?;

			if function.visibility == Visibility::Public
				&& let FunctionParameterType::Value(data_type) = &parameter.data_type {
				self.validate_public_object_type_reference(
					data_type,
					parameter.position,
					format!("Public function `{}` parameter `{}`", function.name, parameter.name),
				)?;
			}

			parameters.push(FunctionParameterSignature {
				data_type: parameter_type,
				has_default: parameter.default_value.is_some(),
				is_by_ref: parameter.is_by_ref,
				is_variadic: parameter.is_variadic,
				name: parameter.name.clone(),
				sequence,
			});
		}

		if let Some(return_type) = &function.return_type {
			self.validate_declared_data_type(
				return_type,
				function.position,
				format!(
					"Function `{}` cannot return `{}`.",
					function.name,
					return_type.name(),
				),
			)?;

			if function.visibility == Visibility::Public {
				self.validate_public_object_type_reference(
					return_type,
					function.position,
					format!("Public function `{}` return type", function.name),
				)?;
			}
		}

		let function_index = self.next_function_index;
		let signature = FunctionSignature {
			function_index,
			parameter_defaults: function.parameters.iter()
				.map(|parameter| parameter.default_value.clone())
				.collect(),
			parameters,
			return_type: function.return_type.clone(),
		};

		if self.functions.contains_in_current_scope(&function.name) {
			let mut overloads = self.functions.lookup(&function.name).unwrap().clone();
			if overloads.iter().any(|existing| existing.parameters == signature.parameters) {
				return Err(self.compile_error(
					function.position,
					format!("Function overload `{}` duplicates an existing callable signature in this scope.", function.name),
				));
			}

			overloads.push(signature.clone());
			self.validate_function_overload_set(&function.name, &overloads, function.position)?;

			self.functions.lookup_mut(&function.name).unwrap().push(signature);
		}
		else {
			self.functions.declare(function.name.clone(), vec![signature]);
		}

		self.semantic_program.function_declaration_targets.insert(function.position, function_index);
		self.next_function_index += 1;
		Ok(())
	}

	fn declare_local(&mut self, name: String, local: LocalBinding) {
		self.declaration_kinds.declare(name.clone(), LexicalDeclarationKind::Local);
		self.sequence_aliases.declare(name.clone(), None);
		self.locals.declare(name, local);
	}

	fn describe_function_signature(&self, name: &str, signature: &FunctionSignature) -> String {
		let parameters = signature.parameters.iter()
			.map(|parameter| {
				let variadic = if parameter.is_variadic { "..." } else { "" };
				let reference = if parameter.is_by_ref { "&" } else { "" };
				let default = if parameter.has_default { " = default" } else { "" };
				let data_type = match (&parameter.sequence, &parameter.data_type) {
					(Some(sequence), _) => {
						format!(
							"seq {}.{}.{}",
							sequence.database_name,
							sequence.schema_name,
							sequence.sequence_name,
						)
					}
					(None, DataType::RecordPointer(record_pointer)) => {
						format!(
							"rec {}.{}.{}",
							record_pointer.database_name,
							record_pointer.schema_name,
							record_pointer.table_name,
						)
					}
					(None, other) => other.name(),
				};
				format!(
					"{variadic}{}: {reference}{}{default}",
					parameter.name,
					data_type,
				)
			})
			.collect::<Vec<_>>()
			.join(", ");
		format!("{name}({parameters})")
	}

	fn describe_object_default_value(&self, expression: &Expr) -> ObjectDefaultValue {
		match expression {
			Expr::Array(array) => ObjectDefaultValue::Array(
				array.elements.iter()
					.map(|element| self.describe_object_default_value(element))
					.collect(),
			),
			Expr::Boolean(value) => ObjectDefaultValue::Boolean(value.value),
			Expr::Date(value) => ObjectDefaultValue::Date(value.value.clone()),
			Expr::Decimal(value) => ObjectDefaultValue::Decimal(value.value.clone()),
			Expr::Integer(value) => ObjectDefaultValue::Integer(value.value),
			Expr::Null(_) => ObjectDefaultValue::Null,
			Expr::ObjectConstruction(object) => ObjectDefaultValue::Object {
				fields: object.fields.iter()
					.map(|field| (
						field.name.clone(),
						self.describe_object_default_value(&field.value),
					))
					.collect(),
				object_type_id: self.semantic_program.object_construction_type_id(object.position)
					.expect("Validated object default is missing its resolved type identity."),
			},
			Expr::Text(value) => ObjectDefaultValue::Text(value.value.clone()),
			Expr::Time(value) => ObjectDefaultValue::Time(value.value.clone()),
			Expr::TimeTz(value) => ObjectDefaultValue::TimeTz(value.value.clone()),
			Expr::Timestamp(value) => ObjectDefaultValue::Timestamp(value.value.clone()),
			Expr::TimestampTz(value) => ObjectDefaultValue::TimestampTz(value.value.clone()),
			_ => unreachable!("Validated object defaults contain only literal expressions."),
		}
	}

	fn describe_object_value_type(&self, data_type: &DataType) -> ObjectValueTypeDescriptor {
		match data_type {
			DataType::Any => ObjectValueTypeDescriptor::Any,
			DataType::Array(element_type) => ObjectValueTypeDescriptor::Array(Box::new(
				self.describe_object_value_type(element_type),
			)),
			DataType::Bool => ObjectValueTypeDescriptor::Bool,
			DataType::Date => ObjectValueTypeDescriptor::Date,
			DataType::Dec => ObjectValueTypeDescriptor::Dec,
			DataType::Int => ObjectValueTypeDescriptor::Int,
			DataType::Nullable(inner) => ObjectValueTypeDescriptor::Nullable(Box::new(
				self.describe_object_value_type(inner),
			)),
			DataType::Object(name) => {
				if let Some(object_type_id) = self.lookup_object_type_id(name) {
					ObjectValueTypeDescriptor::Object(object_type_id)
				}
				else {
					debug_assert!(self.lookup_enum(name).is_some());
					ObjectValueTypeDescriptor::Enum(name.to_string())
				}
			}
			DataType::Range(element_type) => ObjectValueTypeDescriptor::Range(Box::new(
				self.describe_object_value_type(element_type),
			)),
			DataType::Text => ObjectValueTypeDescriptor::Text,
			DataType::Time => ObjectValueTypeDescriptor::Time,
			DataType::TimeTz => ObjectValueTypeDescriptor::TimeTz,
			DataType::Timestamp => ObjectValueTypeDescriptor::Timestamp,
			DataType::TimestampTz => ObjectValueTypeDescriptor::TimestampTz,
			DataType::Union(members) => ObjectValueTypeDescriptor::Union(
				members.iter()
					.map(|member| self.describe_object_value_type(member))
					.collect(),
			),
			DataType::EmptyArray | DataType::Null | DataType::RecordPointer(_) => {
				unreachable!("Validated object declarations contain only declarable value types.")
			}
		}
	}

	fn effective_object_data_type(&self, data_type: &DataType) -> Option<DataType> {
		match data_type {
			DataType::Object(name) => self.lookup_object_type(name)?
				.declaration()
				.array_element_type()
				.map(|element_type| DataType::Array(Box::new(element_type.clone()))),
			DataType::Nullable(inner) => {
				let DataType::Object(name) = inner.as_ref() else {
					return None;
				};

				self.lookup_object_type(name)?
					.declaration()
					.array_element_type()
					.map(|element_type| DataType::Array(Box::new(element_type.clone())).into_nullable())
			}
			_ => None,
		}
	}

	fn ensure_assignable(&self, target: &DataType, value: &DataType, position: usize) -> Result<(), CompileError> {
		if self.is_assignable(target, value) {
			return Ok(());
		}

		Err(self.compile_error(
			position,
			format!(
				"Cannot assign a value of type `{}` to a variable of type `{}`.",
				self.data_type_display_name(value),
				self.data_type_display_name(target),
			),
		))
	}

	fn ensure_function_argument_assignable(
		&self,
		target: &DataType,
		value: &DataType,
		position: usize,
	) -> Result<(), CompileError> {
		if self.is_function_argument_assignable(target, value) {
			return Ok(());
		}

		Err(self.compile_error(
			position,
			format!(
				"Cannot assign a value of type `{}` to a variable of type `{}`.",
				self.data_type_display_name(value),
				self.data_type_display_name(target),
			),
		))
	}

	fn ensure_object_field_default_assignable(
		&self,
		field: &ObjectFieldDeclaration,
		value: &DataType,
		position: usize,
	) -> Result<(), CompileError> {
		if self.is_assignable(&field.data_type, value) {
			return Ok(());
		}

		Err(self.compile_error(
			position,
			format!(
				"Default value for field `{}` has type `{}`, which is not assignable to `{}`.",
				field.name,
				self.data_type_display_name(value),
				self.data_type_display_name(&field.data_type),
			),
		))
	}

	fn enter_scope(&mut self) {
		self.declaration_kinds.enter_scope();
		self.locals.enter_scope();
		self.sequence_aliases.enter_scope();
	}

	fn enum_literal_constant(&self, expression: &Expr, data_type: &DataType) -> Option<Constant> {
		match (expression, data_type) {
			(Expr::Boolean(boolean), DataType::Bool) => Some(Constant::Boolean(boolean.value)),
			(Expr::Date(date), DataType::Date) => Some(Constant::Date(date.value)),
			(Expr::Decimal(decimal), DataType::Dec) => Some(Constant::Decimal(decimal.value.clone())),
			(Expr::Integer(integer), DataType::Int) => Some(Constant::Integer(integer.value)),
			(Expr::Text(text), DataType::Text) => Some(Constant::Text(text.value.clone())),
			(
				Expr::Unary(UnaryExpr {
					operator: UnaryOperator::Negate,
					operand,
					..
				}),
				DataType::Int,
			) => {
				let Expr::Integer(integer) = operand.as_ref() else {
					return None;
				};

				integer.value.checked_neg().map(Constant::Integer)
			}
			(
				Expr::Unary(UnaryExpr {
					operator: UnaryOperator::Negate,
					operand,
					..
				}),
				DataType::Dec,
			) => {
				let Expr::Decimal(decimal) = operand.as_ref() else {
					return None;
				};

				let mut value = decimal.value.clone();
				value.coefficient = value.coefficient.checked_neg()?;
				Some(Constant::Decimal(value))
			}
			_ => None,
		}
	}

	fn enum_variant_binding(&self, enum_name: &str, variant_name: &str) -> Option<&EnumValue> {
		self.lookup_enum(enum_name)?.variants.get(variant_name)
	}

	fn exit_scope(&mut self) {
		self.declaration_kinds.exit_scope();
		self.locals.exit_scope();
		self.sequence_aliases.exit_scope();
	}

	fn expressions_match_for_non_null_refinement(&self, lhs: &Expr, rhs: &Expr) -> bool {
		match (lhs, rhs) {
			(Expr::Identifier(lhs), Expr::Identifier(rhs)) => lhs.name == rhs.name,
			(Expr::FieldAccess(lhs), Expr::FieldAccess(rhs)) => {
				lhs.field.name == rhs.field.name
					&& self.expressions_match_for_non_null_refinement(lhs.object.as_ref(), rhs.object.as_ref())
			}
			_ => false,
		}
	}

	fn extend_named_call_shapes(
		parameters: &[FunctionParameterSignature],
		parameter_index: usize,
		fixed_parameter_count: usize,
		shape: FunctionCallShape,
		shapes: &mut Vec<FunctionCallShape>,
	) {
		if parameter_index == fixed_parameter_count {
			shapes.push(shape);
			return;
		}

		let parameter = &parameters[parameter_index];
		let is_omittable = !parameter.is_by_ref
			&& (parameter.has_default || parameter.data_type.is_nullable());
		if is_omittable {
			Self::extend_named_call_shapes(
				parameters,
				parameter_index + 1,
				fixed_parameter_count,
				shape.clone(),
				shapes,
			);
		}

		let mut supplied_shape = shape;
		Self::push_call_shape_argument(
			&mut supplied_shape,
			parameter,
			Some(parameter.name.clone()),
			parameter.data_type.clone(),
		);
		Self::extend_named_call_shapes(
			parameters,
			parameter_index + 1,
			fixed_parameter_count,
			supplied_shape,
			shapes,
		);
	}

	fn finalize_record_pointer_usage(&mut self, program: &AstProgram) {
		let reachable_positions = super::ssa::reachable_read_positions(program, &self.semantic_program);

		for binding in self.semantic_program.record_pointer_bindings.values_mut() {
			binding.escape_positions.retain(|position| reachable_positions.contains(position));
			binding.escapes_analysis = !binding.escape_positions.is_empty();
			binding.field_assignments.retain(|assignment| reachable_positions.contains(&assignment.position));
			binding.assigned_fields = binding.field_assignments.iter()
				.map(|assignment| assignment.field_path.clone())
				.collect();
			binding.field_reads.retain(|read| reachable_positions.contains(&read.position));
			binding.read_fields = binding.field_reads.iter()
				.map(|read| read.field_name.clone())
				.collect();
		}
	}

	fn function_call_shapes(
		&self,
		signature: &FunctionSignature,
		max_variadic_arguments: usize,
	) -> Vec<FunctionCallShape> {
		let variadic_parameter_index = signature.parameters.iter()
			.position(|parameter| parameter.is_variadic);
		let fixed_parameter_count = variadic_parameter_index.unwrap_or(signature.parameters.len());
		let mut fixed_shapes = Vec::new();

		for positional_count in 0..=fixed_parameter_count {
			let mut shape = FunctionCallShape {
				arguments: Vec::new(),
				argument_sequences: Vec::new(),
				argument_types: Vec::new(),
			};
			for parameter in &signature.parameters[..positional_count] {
				Self::push_call_shape_argument(&mut shape, parameter, None, parameter.data_type.clone());
			}
			Self::extend_named_call_shapes(
				&signature.parameters,
				positional_count,
				fixed_parameter_count,
				shape,
				&mut fixed_shapes,
			);
		}

		let Some(variadic_parameter_index) = variadic_parameter_index else {
			return fixed_shapes;
		};
		let variadic_parameter = &signature.parameters[variadic_parameter_index];
		let DataType::Array(element_type) = &variadic_parameter.data_type else {
			unreachable!("Variadic parameters must have array types.");
		};
		let mut shapes = Vec::new();

		for fixed_shape in fixed_shapes {
			let mut named_array_shape = fixed_shape.clone();
			Self::push_call_shape_argument(
				&mut named_array_shape,
				variadic_parameter,
				Some(variadic_parameter.name.clone()),
				variadic_parameter.data_type.clone(),
			);
			shapes.push(named_array_shape);

			for variadic_count in 0..=max_variadic_arguments {
				let mut trailing_shape = fixed_shape.clone();
				for _ in 0..variadic_count {
					Self::push_call_shape_argument(
						&mut trailing_shape,
						variadic_parameter,
						None,
						element_type.as_ref().clone(),
					);
				}
				shapes.push(trailing_shape);
			}
		}

		shapes
	}

	fn group_by_item_key_names(&self, item: &crate::ast::GroupByItem) -> Vec<String> {
		let mut names = Vec::new();

		if let Some(alias) = &item.alias {
			names.push(alias.name.clone());
		}

		if let Some(name) = simple_group_by_expression_name(&item.expression) {
			if !names.iter().any(|existing| existing.eq_ignore_ascii_case(&name)) {
				names.push(name);
			}
		}

		names
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

	fn infer_built_in_call_type(
		&mut self,
		built_in: BuiltInFunction,
		arguments: &[CallArgument],
		position: usize,
	) -> Result<Option<DataType>, CompileError> {
		self.reject_default_arguments_for_built_in(built_in, arguments)?;

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

		if matches!(built_in, BuiltInFunction::FirstOf | BuiltInFunction::LastOf) {
			return self.infer_group_boundary_call_type(built_in, arguments, position).map(Some);
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
			.map(|argument| {
				let expression = argument.expression().expect("Built-in arguments must be expressions after default-marker validation.");
				self.infer_expression_type(expression)
			})
			.collect::<Result<Vec<_>, _>>()?;

		if built_in == BuiltInFunction::SeqNext {
			let [argument] = arguments else {
				return Err(self.compile_error(position, String::from("Built-in function `seqnext` expects 1 argument(s), found 0.")));
			};
			let expression = argument.expression().expect("Built-in arguments must be expressions after default-marker validation.");
			let Some(resolved_sequence) = self.semantic_program.resolved_sequence(expression.position()).cloned() else {
				return Err(self.compile_error(
					expression.position(),
					String::from("Built-in function `seqnext` requires a sequence reference."),
				));
			};
			self.semantic_program.sequence_call_targets.insert(position, resolved_sequence);
			self.semantic_program.call_return_types.insert(position, DataType::Int);
			return Ok(Some(DataType::Int));
		}

		if let Some(return_type) = self.infer_enum_downcast_built_in_type(built_in, &argument_types, position)? {
			self.semantic_program.built_in_call_targets.insert(position, built_in);
			self.semantic_program.call_return_types.insert(position, return_type.clone());
			return Ok(Some(return_type));
		}

		let signatures = self.built_in_validation_signatures(built_in);
		let (_, argument_bindings) = self.select_function_overload(
			"built-in function",
			built_in.name(),
			&signatures,
			arguments,
			&argument_types.iter().cloned().map(Some).collect::<Vec<_>>(),
			position,
		)?;
		let ordered_arguments = Self::call_arguments_in_binding_order(arguments, &argument_bindings);
		let argument_types = Self::call_argument_types_in_binding_order(&argument_types, &argument_bindings);
		self.semantic_program.call_argument_bindings.insert(position, argument_bindings);

		let return_type = built_in.return_type(&argument_types).map_err(|_| self.compile_error(
			arguments.first().map_or(position, |argument| argument.expression().unwrap().position()),
			format!(
				"Built-in function `{}` does not accept an argument of type `{}`.",
				built_in.name(),
				argument_types.first().unwrap().name(),
			),
		))?;

		if built_in == BuiltInFunction::Format {
			self.validate_format_built_in(&ordered_arguments, &argument_types)?;
		}

		self.semantic_program.built_in_call_targets.insert(position, built_in);
		if let Some(return_type) = &return_type {
			self.semantic_program.call_return_types.insert(position, return_type.clone());
		}
		Ok(return_type)
	}

	fn infer_built_in_call_type_for_query(
		&mut self,
		built_in: BuiltInFunction,
		arguments: &[CallArgument],
		position: usize,
		table: &TableReference,
	) -> Result<DataType, CompileError> {
		self.reject_default_arguments_for_built_in(built_in, arguments)?;

		if matches!(built_in, BuiltInFunction::FirstOf | BuiltInFunction::LastOf) {
			return Err(self.compile_error(
				position,
				format!("Built-in function `{}` may only be used inside the body of a grouped `for rec` loop.", built_in.name()),
			));
		}

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

		let mut argument_types = arguments.iter()
			.map(|argument| {
				let expression = argument.expression().expect("Built-in arguments must be expressions after default-marker validation.");
				self.infer_query_expression_type(expression, table)
			})
			.collect::<Result<Vec<_>, _>>()?;

		let signatures = self.built_in_validation_signatures(built_in);
		if !signatures.is_empty() {
			let (_, argument_bindings) = self.select_function_overload(
				"built-in function",
				built_in.name(),
				&signatures,
				arguments,
				&argument_types.iter().cloned().map(Some).collect::<Vec<_>>(),
				position,
			)?;
			argument_types = Self::call_argument_types_in_binding_order(&argument_types, &argument_bindings);
			self.semantic_program.call_argument_bindings.insert(position, argument_bindings);
		}

		let return_type = built_in.return_type(&argument_types).map_err(|_| self.compile_error(
			arguments.first().map_or(position, |argument| argument.expression().unwrap().position()),
			format!(
				"Built-in function `{}` does not accept an argument of type `{}`.",
				built_in.name(),
				argument_types.first().unwrap().name(),
			),
		))?;

		self.semantic_program.built_in_call_targets.insert(position, built_in);
		if let Some(return_type) = &return_type {
			self.semantic_program.call_return_types.insert(position, return_type.clone());
		}
		return_type.ok_or_else(|| self.compile_error(
			position,
			format!("Built-in function `{}` does not return a value.", built_in.name()),
		))
	}

	fn infer_call_type(&mut self, call: &CallExpr) -> Result<Option<DataType>, CompileError> {
		let CallExpr { arguments, callee, position } = call;

		if let Some(signatures) = self.lookup_functions(&callee.name).map(<[FunctionSignature]>::to_vec) {
			let mut argument_types = Vec::with_capacity(arguments.len());
			for argument in arguments {
				argument_types.push(match argument.expression() {
					Some(expression) => Some(self.infer_expression_type(expression)?),
					None => None,
				});
			}
			let (signature, argument_bindings) = self.select_function_overload(
				"function",
				&callee.name,
				&signatures,
				arguments,
				&argument_types,
				*position,
			)?;
			let mut reference_slots = vec![None; arguments.len()];

			for (parameter_index, binding) in argument_bindings.iter().enumerate() {
				let parameter = &signature.parameters[parameter_index];
				let argument_indices = match binding {
					CallArgumentBinding::Supplied(argument_index) => {
						std::slice::from_ref(argument_index)
					}
					CallArgumentBinding::Variadic(argument_indices) => argument_indices.as_slice(),
					CallArgumentBinding::OmittedDefault(default_value)
					| CallArgumentBinding::RequestedDefault(default_value) => {
						if matches!(parameter.data_type.without_nullability(), DataType::RecordPointer(_)) {
							self.record_record_pointer_escape(default_value);
						}
						continue;
					}
					CallArgumentBinding::OmittedNull => continue,
				};

				for argument_index in argument_indices {
					let argument_index = *argument_index as usize;
					let argument = &arguments[argument_index];
					let argument_type = argument_types[argument_index].as_ref()
						.expect("Supplied call argument bindings must refer to expressions.");
					let expression = argument.expression()
						.expect("Supplied call argument bindings must refer to expressions.");
					if let Some(expected_sequence) = &parameter.sequence {
						let actual_sequence = self.semantic_program.resolved_sequence(expression.position());
						if actual_sequence != Some(expected_sequence) {
							return Err(self.compile_error(
								expression.position(),
								format!(
									"Argument for sequence parameter `{}` must refer to sequence `{}.{}`.",
									parameter.name,
									expected_sequence.schema_name,
									expected_sequence.sequence_name,
								),
							));
						}
						continue;
					}
					if parameter.is_by_ref {
						if !argument.is_by_ref {
							return Err(self.compile_error(
								argument.position,
								format!("Parameter `{}` must be passed by reference.", parameter.name),
							));
						}

						let Expr::Identifier(identifier) = expression else {
							return Err(self.compile_error(
								argument.position,
								String::from("By-reference arguments must be plain identifiers."),
							));
						};

						let local = self.lookup_local(&identifier.name).ok_or(self.compile_error(
							identifier.position,
							format!("Variable `{}` is not declared in this scope.", identifier.name),
						))?;

						if local.is_const {
							return Err(self.compile_error(
								argument.position,
								format!("Constant `{}` cannot be passed by reference.", identifier.name),
							));
						}

						if argument_type != &parameter.data_type {
							return Err(self.compile_error(
								argument.position,
								format!(
									"By-reference argument for parameter `{}` must have type `{}`, found `{}`.",
									parameter.name,
									parameter.data_type.name(),
									argument_type.name(),
								),
							));
						}

						reference_slots[argument_index] = Some(local.slot);
					}
					else {
						if argument.is_by_ref {
							return Err(self.compile_error(
								argument.position,
								format!("Parameter `{}` must be passed by value.", parameter.name),
							));
						}

						let expected_type = if parameter.is_variadic
							&& matches!(binding, CallArgumentBinding::Variadic(_)) {
							let DataType::Array(element_type) = &parameter.data_type else {
								unreachable!("Variadic parameters must have array types.");
							};
							element_type.as_ref()
						}
						else {
							&parameter.data_type
						};
						self.ensure_function_argument_assignable(expected_type, argument_type, expression.position())?;
					}

					if !parameter.is_variadic
						&& matches!(parameter.data_type.without_nullability(), DataType::RecordPointer(_)) {
						self.record_record_pointer_escape(expression);
					}
				}
			}

			self.semantic_program.call_targets.insert(*position, signature.function_index);
			self.semantic_program.call_argument_bindings.insert(*position, argument_bindings);
			self.semantic_program.call_argument_reference_slots.insert(*position, reference_slots);
			if let Some(return_type) = &signature.return_type {
				self.semantic_program.call_return_types.insert(*position, return_type.clone());
			}
			Ok(signature.return_type)
		}
		else if let Some(declaration_kind) = self.declaration_kinds.lookup(&callee.name).copied() {
			Err(self.compile_error(
				callee.position,
				format!(
					"Identifier `{}` is not callable because it refers to {} in the nearest scope.",
					callee.name,
					declaration_kind.description(),
				),
			))
		}
		else if let Some(built_in) = BuiltInFunction::from_name(&callee.name) {
			self.infer_built_in_call_type(built_in, arguments, *position)
		}
		else {
			Err(self.compile_error(
				callee.position,
				format!("Function `{}` is not declared in this scope.", callee.name),
			))
		}
	}

	fn infer_count_expression_type(&mut self, count: &CountExpr) -> Result<DataType, CompileError> {
		let lowered_query = self.lower_count_query(count)?;

		if let Some(where_clause) = &count.where_clause {
			let where_type = self.infer_query_expression_type(where_clause, &count.table)?;

			if where_type.without_nullability() != &DataType::Bool {
				return Err(self.compile_error(
					where_clause.position(),
					format!("`where` clause must evaluate to `bool`, found `{}`.", where_type.name()),
				));
			}
		}

		let compiled_query = lower_count_query(&lowered_query).map_err(|error| self.compile_error(
			count.position,
			query_lowering_error_message(error),
		))?;

		self.semantic_program.compiled_count_queries.insert(count.position, compiled_query);
		self.semantic_program.lowered_count_queries.insert(count.position, lowered_query);
		Ok(DataType::Int)
	}

	fn infer_enum_downcast_built_in_type(
		&self,
		built_in: BuiltInFunction,
		argument_types: &[DataType],
		position: usize,
	) -> Result<Option<DataType>, CompileError> {
		let target_type = match built_in {
			BuiltInFunction::IntCast => DataType::Int,
			BuiltInFunction::TextCast => DataType::Text,
			BuiltInFunction::DecCast => DataType::Dec,
			BuiltInFunction::BoolCast => DataType::Bool,
			BuiltInFunction::DateCast => DataType::Date,
			_ => return Ok(None),
		};

		let [argument_type] = argument_types else {
			return Ok(None);
		};

		let DataType::Object(enum_name) = argument_type.without_nullability() else {
			return Ok(None);
		};

		let Some(enum_binding) = self.lookup_enum(enum_name) else {
			return Ok(None);
		};

		if enum_binding.backing_type != target_type {
			return Err(self.compile_error(
				position,
				format!(
					"Built-in function `{}` cannot cast enum `{}` because its backing type is `{}`.",
					built_in.name(),
					enum_name,
					enum_binding.backing_type.name(),
				),
			));
		}

		Ok(Some(target_type))
	}

	fn infer_expression_type(&mut self, expression: &Expr) -> Result<DataType, CompileError> {
		let data_type = match expression {
			Expr::Array(array) => self.infer_array_literal_type(array.elements.as_slice(), array.position),
			Expr::Assignment(AssignmentExpr { operator, target, value, .. }) => {
				match target {
					AssignmentTarget::Identifier(target) => {
						if let Some(resolved_sequence) = self.lookup_sequence_alias(&target.name) {
							if *operator != AssignmentOperator::Assign {
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
									format!("Sequence `{}` cannot be assigned using `{operation}`.", target.name),
								));
							}

							self.record_resolved_sequence(target.position, &resolved_sequence);
							let value_type = self.infer_expression_type(value)?;
							self.assignment_result_type(*operator, &DataType::Int, &value_type, expression.position())
						}
						else if let Some(local) = self.lookup_local(&target.name) {
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

							if matches!(local.data_type.without_nullability(), DataType::RecordPointer(_)) {
								self.record_record_pointer_escape(value);
							}

							self.assignment_result_type(*operator, &local.data_type, &value_type, expression.position())
						}
						else if let Some(resolved_sequence) = self.try_resolve_unqualified_sequence(&target.name, target.position)? {
							if *operator != AssignmentOperator::Assign {
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
									format!("Sequence `{}` cannot be assigned using `{operation}`.", target.name),
								));
							}

							self.record_resolved_sequence(target.position, &resolved_sequence);
							let value_type = self.infer_expression_type(value)?;
							self.assignment_result_type(*operator, &DataType::Int, &value_type, expression.position())
						}
						else {
							Err(self.compile_error(
								target.position,
								format!("Variable `{}` is not declared in this scope.", target.name),
							))
						}
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

						if index_type.without_nullability() != &DataType::Int {
							return Err(self.compile_error(
								target.index.position(),
								format!("Array index must be of type `int`, found `{}`.", index_type.name()),
							));
						}

						let element_type = match local.data_type.without_nullability() {
							DataType::Array(element_type) => element_type.as_ref(),
							other => {
								return Err(self.compile_error(
									target.array.position,
									format!("Indexed assignment requires an array operand, found `{}`.", other.name()),
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
								DataType::RecordPointer(ref record_pointer) => {
									current_type = self.infer_record_pointer_field_access_type(
										record_pointer,
										field.position,
										&field.name,
									)?;
								}
								ref other => {
									return Err(self.compile_error(
										field.position,
										format!("Field access requires an object operand, found `{}`.", other.name()),
									));
								}
							}
						}

						if matches!(local.data_type.without_nullability(), DataType::RecordPointer(_)) {
							self.record_record_pointer_assignment(
								local.declaration_position,
								&target.fields,
								target.object.position,
							);
							if (*operator != AssignmentOperator::Assign || target.fields.len() > 1)
								&& let Some(field) = target.fields.first() {
								self.record_record_pointer_read(local.declaration_position, &field.name, target.object.position);
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
				let result = self.binary_result_type(*operator, &left_type, &right_type, expression.position())?;
				self.record_null_comparison_semantics(
					expression.position(),
					left,
					&left_type,
					*operator,
					right,
					&right_type,
				);
				Ok(result)
			}
			Expr::Boolean(_) => Ok(DataType::Bool),
			Expr::Call(call) => self.infer_call_type(call)?.ok_or_else(|| {
				let callable_kind = if BuiltInFunction::from_name(&call.callee.name).is_some() {
					"Built-in function"
				}
				else {
					"Function"
				};
				self.compile_error(
					call.position,
					format!("{callable_kind} `{}` does not return a value.", call.callee.name),
				)
			}),
			Expr::Count(count) => self.infer_count_expression_type(count),
			Expr::Date(_) => Ok(DataType::Date),
			Expr::Decimal(_) => Ok(DataType::Dec),
			Expr::FieldAccess(FieldAccessExpr { field, object, .. }) => {
				if let Expr::Identifier(identifier) = object.as_ref()
					&& self.lookup_local(&identifier.name).is_none()
					&& let Some(enum_value) = self.enum_variant_binding(&identifier.name, &field.name) {
					self.semantic_program.enum_variant_values.insert(expression.position(), enum_value.clone());
					return Ok(DataType::Object(identifier.name.clone().into()));
				}

				if let Some(resolved_sequence) = self.try_resolve_sequence_expression(expression)? {
					self.record_resolved_sequence(expression.position(), &resolved_sequence);
					return Ok(DataType::Int);
				}

				self.record_record_pointer_field_access_read(expression);

				let object_type = self.infer_expression_type(object)?;
				let object_type = object_type.without_nullability().clone();

				match object_type {
					DataType::Object(name) => {
						if self.lookup_enum(&name).is_some() {
							return Err(self.compile_error(
								field.position,
								format!("Enum `{name}` does not contain a variant named `{}`.", field.name),
							));
						}

						if self.lookup_object_type(&name)
							.and_then(|object| object.declaration().array_element_type())
							.is_some() {
							return Err(self.compile_error(
								field.position,
								format!("Field access requires an object operand, found `{}`.", DataType::Object(name).name()),
							));
						}

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
						format!("Field access requires an object operand, found `{}`.", other.name()),
					)),
				}
			}
			Expr::Find(find) => self.infer_find_expression_type(find),
			Expr::Identifier(IdentifierExpr { name, .. }) => {
				if let Some(resolved_sequence) = self.lookup_sequence_alias(name) {
					self.record_resolved_sequence(expression.position(), &resolved_sequence);
					Ok(DataType::Int)
				}
				else if let Some(local) = self.lookup_local(name) {
					self.semantic_program.identifier_slots.insert(expression.position(), local.slot);
					Ok(local.data_type)
				}
				else if let Some(resolved_sequence) = self.try_resolve_unqualified_sequence(name, expression.position())? {
					self.record_resolved_sequence(expression.position(), &resolved_sequence);
					Ok(DataType::Int)
				}
				else {
					Err(self.compile_error(
						expression.position(),
						format!("Variable `{name}` is not declared in this scope."),
					))
				}
			}
			Expr::Index(IndexExpr { array, index, .. }) => {
				let mut array_type = self.infer_expression_type(array)?;
				if let Some(effective_array_type) = self.effective_object_data_type(&array_type) {
					array_type = effective_array_type;
				}
				let index_type = self.infer_expression_type(index)?;
				let index_type = index_type.without_nullability().clone();
				let array_type = array_type.without_nullability().clone();

				match array_type {
					DataType::Array(element_type) => match index_type {
						DataType::Int => Ok(*element_type),
						DataType::Range(index_element_type) if index_element_type.without_nullability() == &DataType::Int => {
							Ok(DataType::Array(element_type))
						}
						DataType::Range(index_element_type) => Err(self.compile_error(
							index.position(),
							format!(
								"Array slicing requires a range of `int`, found `range<{}>`.",
								index_element_type.name(),
							),
						)),
						other => Err(self.compile_error(
							index.position(),
							format!("Array index must be of type `int`, found `{}`.", other.name()),
						)),
					},
					DataType::Text => match index_type {
						DataType::Int => Ok(DataType::Text),
						DataType::Range(index_element_type) if index_element_type.without_nullability() == &DataType::Int => {
							Ok(DataType::Text)
						}
						DataType::Range(index_element_type) => Err(self.compile_error(
							index.position(),
							format!(
								"Text slicing requires a range of `int`, found `range<{}>`.",
								index_element_type.name(),
							),
						)),
						other => Err(self.compile_error(
							index.position(),
							format!("Text index must be of type `int`, found `{}`.", other.name()),
						)),
					},
					DataType::EmptyArray => Err(self.compile_error(
						array.position(),
						String::from("Cannot index an empty array literal without a known element type."),
					)),
					other => Err(self.compile_error(
						array.position(),
						format!("Array indexing requires an array operand, found `{}`.", other.name()),
					)),
				}
			}
			Expr::Integer(_) => Ok(DataType::Int),
			Expr::New(new_expression) => self.infer_new_expression_type(new_expression),
			Expr::Null(_) => Ok(DataType::Null),
			Expr::ObjectConstruction(ObjectConstructionExpr {
				fields,
				object_type_name,
				position,
			}) => {
				let object_type_id = self.lookup_object_type_id(object_type_name)
					.ok_or_else(|| self.unknown_object_type_error(*position, object_type_name))?;
				let object_declaration = self.semantic_program.object_type(object_type_id)
					.map(ResolvedObjectType::declaration)
					.cloned()
					.expect("Resolved object type binding is missing its semantic declaration.");
				self.semantic_program.object_construction_type_ids.insert(*position, object_type_id);
				let object_fields = object_declaration.fields().ok_or(self.compile_error(
					*position,
					format!("Object `{object_type_name}` uses array syntax and cannot be constructed with named fields."),
				))?;
				let mut provided_fields = BTreeMap::new();

				for field in fields {
					let Some(object_field) = object_fields.iter().find(|object_field| object_field.name == field.name) else {
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

				for object_field in object_fields {
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

				Ok(DataType::Object(object_type_name.clone().into()))
			}
			Expr::Range(RangeExpr { start, step, end, position }) => {
				let start_type = self.infer_expression_type(start)?;
				let end_type = self.infer_expression_type(end)?;

				if !self.is_numeric_type(&start_type) || !self.is_numeric_type(&end_type) {
					return Err(self.compile_error(
						*position,
						format!(
							"Range bounds must be numeric, found `{}` and `{}`.",
							start_type.name(),
							end_type.name(),
						),
					));
				}

				let element_type = if let Some(step) = step {
					let step_type = self.infer_expression_type(step)?;

					if !self.is_numeric_type(&step_type) {
						return Err(self.compile_error(
							step.position(),
							format!("Range step must be numeric, found `{}`.", step_type.name()),
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
			Expr::Ternary(TernaryExpr {
				condition,
				false_branch,
				position,
				true_branch,
			}) => {
				let condition_type = self.infer_expression_type(condition)?;

				if !self.is_truthy_condition_type(&condition_type) {
					return Err(self.compile_error(
						condition.position().max(*position),
						format!("Ternary condition must be of type `bool` or `record pointer`, found `{}`.", condition_type.name()),
					));
				}

				let true_type = self.infer_expression_type_with_branch_refinements(true_branch, condition, true)?;
				let false_type = self.infer_expression_type_with_branch_refinements(false_branch, condition, false)?;
				self.infer_ternary_result_type(&true_type, &false_type, *position)
			}
			Expr::Text(_) => Ok(DataType::Text),
			Expr::Time(_) => Ok(DataType::Time),
			Expr::TimeTz(_) => Ok(DataType::TimeTz),
			Expr::Timestamp(_) => Ok(DataType::Timestamp),
			Expr::TimestampTz(_) => Ok(DataType::TimestampTz),
			Expr::Unary(UnaryExpr { operand, operator, .. }) => {
				let operand_type = self.infer_expression_type(operand)?;
				let operand_type = operand_type.without_nullability().clone();

				match operator {
					UnaryOperator::Exists => {
						let is_field_access = matches!(operand.as_ref(), Expr::FieldAccess(_))
							&& self.semantic_program.enum_variant_value(operand.position()).is_none()
							&& self.semantic_program.resolved_sequence(operand.position()).is_none();

						if is_field_access || matches!(operand_type, DataType::RecordPointer(_)) {
							Ok(DataType::Bool)
						}
						else {
							Err(self.compile_error(
								expression.position(),
								format!(
									"Unary `exists` requires a record pointer or field-access operand, found `{}`.",
									operand_type.name(),
								),
							))
						}
					}
					UnaryOperator::Locked => {
						if matches!(operand_type, DataType::RecordPointer(_)) {
							if self.is_read_only_record_pointer_expression(operand) {
								self.record_warning(
									expression.position(),
									String::from(
										"`locked` is always false for a read-only record pointer because no record lock is requested.",
									),
								);
							}
							Ok(DataType::Bool)
						}
						else {
							Err(self.compile_error(
								expression.position(),
								format!("Unary `locked` requires a record pointer operand, found `{}`.", operand_type.name()),
							))
						}
					}
					UnaryOperator::Negate => {
						if self.is_numeric_type(&operand_type) {
							Ok(operand_type)
						}
						else {
							Err(self.compile_error(
								expression.position(),
								format!("Unary `-` requires a numeric operand, found `{}`.", operand_type.name()),
							))
						}
					}
					UnaryOperator::Not => {
						if self.is_truthy_condition_type(&operand_type) {
							Ok(DataType::Bool)
						}
						else {
							Err(self.compile_error(
								expression.position(),
								format!("Unary `not` requires a `bool` or `record pointer` operand, found `{}`.", operand_type.name()),
							))
						}
					}
				}
			}
		}?;

		Ok(self.refine_expression_type_from_assumptions(expression, data_type))
	}

	fn infer_expression_type_with_branch_refinements(
		&mut self,
		expression: &Expr,
		condition: &Expr,
		branch_taken_when_condition_is_truthy: bool,
	) -> Result<DataType, CompileError> {
		let Some(refined_expression) = self.refined_expression_for_branch(condition, branch_taken_when_condition_is_truthy) else {
			return self.infer_expression_type(expression);
		};

		self.current_non_null_assumptions.push(refined_expression);
		let result = self.infer_expression_type(expression);
		self.current_non_null_assumptions.pop();
		result
	}

	fn infer_expression_type_with_find_lock_mode(
		&mut self,
		expression: &Expr,
		lock_mode: RecordLockMode,
	) -> Result<DataType, CompileError> {
		let previous_lock_mode = self.find_lock_mode;
		self.find_lock_mode = lock_mode;
		let result = self.infer_expression_type(expression);
		self.find_lock_mode = previous_lock_mode;
		result
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

			if where_type.without_nullability() != &DataType::Bool {
				return Err(self.compile_error(
					where_clause.position(),
					format!("`where` clause must evaluate to `bool`, found `{}`.", where_type.name()),
				));
			}
		}

		for order_by in &find.order_by {
			self.infer_query_expression_type(&order_by.expression, &find.table)?;
		}

		let compiled_query = lower_find_query(&lowered_query).map_err(|error| self.compile_error(
			find.position,
			query_lowering_error_message(error),
		))?;
		self.semantic_program.compiled_find_queries.insert(find.position, compiled_query);
		self.semantic_program.lowered_find_queries.insert(find.position, lowered_query);

		Ok(DataType::RecordPointer(record_pointer))
	}

	fn infer_group_boundary_call_type(
		&mut self,
		built_in: BuiltInFunction,
		arguments: &[CallArgument],
		position: usize,
	) -> Result<DataType, CompileError> {
		for argument in arguments {
			if argument.is_by_ref {
				return Err(self.compile_error(
					argument.position,
					format!("Built-in function `{}` does not accept by-reference arguments.", built_in.name()),
				));
			}
		}

		let signatures = self.built_in_validation_signatures(built_in);
		let argument_types = arguments.iter()
			.map(|argument| {
				Some(match argument.expression() {
					Some(Expr::Array(_)) => DataType::Array(Box::new(DataType::Any)),
					_ => DataType::Any,
				})
			})
			.collect::<Vec<_>>();
		let (signature, argument_bindings) = self.select_function_overload(
			"built-in function",
			built_in.name(),
			&signatures,
			arguments,
			&argument_types,
			position,
		)?;
		let mut grouping_expressions = Vec::new();

		for (parameter, binding) in signature.parameters.iter().zip(argument_bindings.iter()) {
			match binding {
				CallArgumentBinding::Supplied(argument_index) if parameter.is_variadic => {
					let argument = &arguments[*argument_index as usize];
					let expression = argument.expression()
						.expect("Built-in arguments must be expressions after default-marker validation.");
					let Expr::Array(array) = expression else {
						return Err(self.compile_error(
							expression.position(),
							format!(
								"Named variadic argument `{}` for built-in function `{}` must be an array literal of grouping levels.",
								parameter.name,
								built_in.name(),
							),
						));
					};
					grouping_expressions.extend(array.elements.iter());
				}
				CallArgumentBinding::Supplied(argument_index) => {
					grouping_expressions.push(
						arguments[*argument_index as usize].expression()
							.expect("Built-in arguments must be expressions after default-marker validation.")
					);
				}
				CallArgumentBinding::Variadic(argument_indices) => {
					grouping_expressions.extend(argument_indices.iter().map(|argument_index| {
						arguments[*argument_index as usize].expression()
							.expect("Built-in arguments must be expressions after default-marker validation.")
					}));
				}
				CallArgumentBinding::OmittedDefault(_)
				| CallArgumentBinding::OmittedNull
				| CallArgumentBinding::RequestedDefault(_) => {
					unreachable!("Group-boundary built-ins do not have omitted arguments.");
				}
			}
		}

		let Some(context) = self.group_boundary_contexts.last() else {
			return Err(self.compile_error(
				position,
				format!("Built-in function `{}` may only be used inside a grouped `for rec` loop.", built_in.name()),
			));
		};

		if grouping_expressions.len() > context.group_keys.len() {
			return Err(self.compile_error(
				position,
				format!(
					"Built-in function `{}` references {} grouping level(s), but the current loop only has {}.",
					built_in.name(),
					grouping_expressions.len(),
					context.group_keys.len(),
				),
			));
		}

		let mut key_names = Vec::with_capacity(grouping_expressions.len());

		for (index, expression) in grouping_expressions.iter().enumerate() {
			let Some(key_name) = simple_group_by_expression_name(expression) else {
				return Err(self.compile_error(
					expression.position(),
					format!("Arguments to `{}` must identify grouping levels by alias or simple field reference.", built_in.name()),
				));
			};
			let expected_names = &context.group_keys[index];

			if !expected_names.iter().any(|expected| expected.eq_ignore_ascii_case(&key_name)) {
				return Err(self.compile_error(
					expression.position(),
					format!(
						"Grouping level `{}` does not match grouping level {} for the current `for rec` loop.",
						key_name,
						index + 1,
					),
				));
			}

			key_names.push(expected_names.first().cloned().unwrap_or(key_name));
		}

		self.semantic_program.built_in_call_targets.insert(position, built_in);
		self.semantic_program.call_return_types.insert(position, DataType::Bool);
		self.semantic_program.call_argument_bindings.insert(position, argument_bindings);
		self.semantic_program.group_boundary_calls.insert(position, GroupBoundaryCallInfo {
			key_names,
			record_slot: context.record_slot,
		});
		Ok(DataType::Bool)
	}

	fn infer_new_expression_type(&mut self, new_expression: &NewExpr) -> Result<DataType, CompileError> {
		let (record_type, schema_is_implicit, table_columns) = {
			let resolved_table = self.resolve_table_reference(&new_expression.table)?;
			let table_columns = resolved_table.table().columns()
				.map(|column| (column.name().to_string(), column.data_type().clone(), column.is_nullable()))
				.collect::<Vec<_>>();

			(
				RecordPointerType {
					database_name: resolved_table.database().name().to_string(),
					schema_name: resolved_table.schema().name().to_string(),
					table_name: resolved_table.table().name().to_string(),
				},
				resolved_table.schema().is_implicit(),
				table_columns,
			)
		};
		let mut columns = Vec::new();

		for (column_name, schema_data_type, is_nullable) in table_columns {
			let mut data_type = self.data_type_from_schema_type(&schema_data_type)?;

			if is_nullable {
				data_type = data_type.into_nullable();
			}

			columns.push(NewRecordColumn {
				data_type,
				name: column_name,
			});
		}

		self.semantic_program.new_record_layouts.insert(new_expression.position, NewRecordLayout {
			columns,
			record_type: record_type.clone(),
			schema_is_implicit,
		});

		Ok(DataType::RecordPointer(record_type))
	}

	fn infer_query_expression_type(
		&mut self,
		expression: &Expr,
		table: &TableReference,
	) -> Result<DataType, CompileError> {
		match expression {
			Expr::Array(array) => self.infer_array_literal_type(&array.elements, array.position),
			Expr::Assignment(_) => Err(self.compile_error(
				expression.position(),
				String::from("Assignments are not permitted in `where` clauses."),
			)),
			Expr::Binary(BinaryExpr { left, operator, right, .. }) => {
				let left_type = self.infer_query_expression_type(left, table)?;
				let right_type = self.infer_query_expression_type(right, table)?;
				let result = self.binary_result_type(*operator, &left_type, &right_type, expression.position())?;
				self.record_null_comparison_semantics(
					expression.position(),
					left,
					&left_type,
					*operator,
					right,
					&right_type,
				);
				Ok(result)
			}
			Expr::Boolean(_) => Ok(DataType::Bool),
			Expr::Call(CallExpr { arguments, callee, .. }) => {
				if let Some(signatures) = self.lookup_functions(&callee.name).map(<[FunctionSignature]>::to_vec) {
					let mut argument_types = Vec::with_capacity(arguments.len());
					for argument in arguments {
						if argument.is_by_ref {
							return Err(self.compile_error(
								argument.position,
								String::from("By-reference arguments are not permitted in `where` clauses."),
							));
						}

						argument_types.push(match argument.expression() {
							Some(expression) => Some(self.infer_query_expression_type(expression, table)?),
							None => None,
						});
					}
					let (signature, argument_bindings) = self.select_function_overload(
						"function",
						&callee.name,
						&signatures,
						arguments,
						&argument_types,
						expression.position(),
					)?;

					for (parameter_index, binding) in argument_bindings.iter().enumerate() {
						let argument_indices = match binding {
							CallArgumentBinding::Supplied(argument_index) => std::slice::from_ref(argument_index),
							CallArgumentBinding::Variadic(argument_indices) => argument_indices.as_slice(),
							_ => continue,
						};
						let parameter = &signature.parameters[parameter_index];
						let expected_type = if matches!(binding, CallArgumentBinding::Variadic(_)) {
							let DataType::Array(element_type) = &parameter.data_type else {
								unreachable!("Variadic parameters must have array types.");
							};
							element_type.as_ref()
						}
						else {
							&parameter.data_type
						};

						for argument_index in argument_indices {
							let argument_index = *argument_index as usize;
							let argument = &arguments[argument_index];
							let argument_type = argument_types[argument_index].as_ref()
								.expect("Supplied call argument bindings must refer to expressions.");
							let argument_expression = argument.expression()
								.expect("Supplied call argument bindings must refer to expressions.");
							self.ensure_function_argument_assignable(
								expected_type,
								argument_type,
								argument_expression.position(),
							)?;
						}
					}

					signature.return_type.ok_or_else(|| self.compile_error(
						expression.position(),
						format!("Function `{}` does not return a value.", callee.name),
					))
				}
				else if let Some(declaration_kind) = self.declaration_kinds.lookup(&callee.name).copied() {
					Err(self.compile_error(
						callee.position,
						format!(
							"Identifier `{}` is not callable because it refers to {} in the nearest scope.",
							callee.name,
							declaration_kind.description(),
						),
					))
				}
				else if let Some(built_in) = BuiltInFunction::from_name(&callee.name) {
					self.infer_built_in_call_type_for_query(built_in, arguments, expression.position(), table)
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
			Expr::Date(_) => Ok(DataType::Date),
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

				let (table_name, column_type, is_nullable) = {
					let resolved_table = self.resolve_table_reference(table)?;
					let table_name = resolved_table.table().name().to_string();
					let column = resolved_table.table().column(&identifier.name).cloned();
					let column = column.ok_or(self.compile_error(
							identifier.position,
							format!("Field `{}` does not exist on table `{table_name}`.", identifier.name),
						))?;
					(table_name, column.data_type().clone(), column.is_nullable())
				};

				let _ = table_name;
				self.data_type_from_schema_column(&column_type, is_nullable)
			}
			Expr::Index(_)
			| Expr::New(_)
			| Expr::ObjectConstruction(_)
			| Expr::Range(_)
			| Expr::Ternary(_) => Err(self.compile_error(
				expression.position(),
				String::from("This expression form is not yet supported in `where` clauses."),
			)),
			Expr::Integer(_) => Ok(DataType::Int),
			Expr::Null(_) => Ok(DataType::Null),
			Expr::Text(_) => Ok(DataType::Text),
			Expr::Time(_) => Ok(DataType::Time),
			Expr::TimeTz(_) => Ok(DataType::TimeTz),
			Expr::Timestamp(_) => Ok(DataType::Timestamp),
			Expr::TimestampTz(_) => Ok(DataType::TimestampTz),
			Expr::Unary(UnaryExpr { operand, operator, .. }) => {
				if matches!(operator, UnaryOperator::Exists | UnaryOperator::Locked) {
					return Err(self.compile_error(
						expression.position(),
						format!("Unary `{}` is not yet supported in database query expressions.", match operator {
							UnaryOperator::Exists => "exists",
							UnaryOperator::Locked => "locked",
							_ => unreachable!(),
						}),
					));
				}

				let operand_type = self.infer_query_expression_type(operand, table)?;

				match operator {
					UnaryOperator::Negate => {
						if !matches!(operand_type, DataType::Int | DataType::Dec) {
							return Err(self.compile_error(
								operand.position(),
								format!("Unary `-` requires a numeric operand, found `{}`.", operand_type.name()),
							));
						}

						Ok(operand_type)
					}
					UnaryOperator::Not => {
						if operand_type.without_nullability() != &DataType::Bool {
							return Err(self.compile_error(
								operand.position(),
								format!("Logical `not` requires a `bool` operand, found `{}`.", operand_type.name()),
							));
						}

						Ok(DataType::Bool)
					}
					UnaryOperator::Exists | UnaryOperator::Locked => unreachable!(),
				}
			}
		}
	}

	fn infer_query_field_access_type(
		&mut self,
		field_access: &FieldAccessExpr,
		table: &TableReference,
	) -> Result<DataType, CompileError> {
		if let Some((base_identifier, fields)) = self.query_field_access_chain(field_access) {
			if self.lookup_local(&base_identifier.name).is_some() {
				let (_, _, data_type) = self.resolve_query_local_field_access(base_identifier, &fields)?;
				return Ok(data_type);
			}

			if fields.len() > 1 {
				return Err(self.compile_error(
					field_access.position,
					String::from("Only simple `table.field` access is supported for query table columns."),
				));
			}

			let (resolved_table_name, column_type, is_nullable) = {
				let resolved_table = self.resolve_table_reference(table)?;
				let resolved_table_name = resolved_table.table().name().to_string();

				if !base_identifier.name.eq_ignore_ascii_case(&resolved_table_name) {
					return Err(self.compile_error(
						base_identifier.position,
						format!(
							"Qualified field reference must use the target table name `{resolved_table_name}`.",
						),
					));
				}

				let field = fields[0];
				let column = resolved_table.table().column(&field.name).cloned();
				let column = column.ok_or(self.compile_error(
						field.position,
						format!(
							"Field `{}` does not exist on table `{resolved_table_name}`.",
							field.name,
						),
					))?;

				(resolved_table_name, column.data_type().clone(), column.is_nullable())
			};

			let _ = resolved_table_name;
			return self.data_type_from_schema_column(&column_type, is_nullable);
		}

		Err(self.compile_error(
			field_access.position,
			String::from("Only chained local object or record-pointer field access and simple `table.field` access are supported in `where` clauses."),
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

		self.data_type_from_schema_column(column.data_type(), column.is_nullable())
	}

	fn infer_ternary_result_type(
		&self,
		true_type: &DataType,
		false_type: &DataType,
		position: usize,
	) -> Result<DataType, CompileError> {
		if self.is_assignable(true_type, false_type) {
			return Ok(true_type.clone());
		}

		if self.is_assignable(false_type, true_type) {
			return Ok(false_type.clone());
		}

		match (true_type, false_type) {
			(DataType::Null, other) => return Ok(other.clone().into_nullable()),
			(other, DataType::Null) => return Ok(other.clone().into_nullable()),
			_ => {}
		}

		Err(self.compile_error(
			position,
			format!(
				"Ternary branches must produce compatible types, found `{}` and `{}`.",
				true_type.name(),
				false_type.name(),
			),
		))
	}

	fn is_assignable(&self, target: &DataType, value: &DataType) -> bool {
		self.is_assignable_with_numeric_conversion(target, value, true)
	}

	fn is_assignable_with_numeric_conversion(
		&self,
		target: &DataType,
		value: &DataType,
		allow_numeric_conversion: bool,
	) -> bool {
		if let Some(target_effective) = self.effective_object_data_type(target) {
			return self.is_assignable_with_numeric_conversion(&target_effective, value, allow_numeric_conversion);
		}

		if let Some(value_effective) = self.effective_object_data_type(value) {
			return self.is_assignable_with_numeric_conversion(target, &value_effective, allow_numeric_conversion);
		}

		if target == &DataType::Any {
			return true;
		}

		match (target, value) {
			(DataType::Nullable(_), DataType::Null) => {
				return true;
			}
			(DataType::Nullable(target_inner), DataType::Nullable(value_inner)) => {
				return self.is_assignable_with_numeric_conversion(target_inner, value_inner, allow_numeric_conversion);
			}
			(DataType::Nullable(target_inner), DataType::EmptyArray) => {
				return matches!(target_inner.as_ref(), DataType::Array(_));
			}
			(DataType::Nullable(target_inner), _) => {
				return self.is_assignable_with_numeric_conversion(target_inner, value, allow_numeric_conversion);
			}
			(_, DataType::Nullable(_)) => {
				return false;
			}
			_ => {}
		}

		if let Some(matches) = self.named_data_types_have_same_identity(target, value) {
			return matches;
		}

		if target == value
			|| (allow_numeric_conversion && target == &DataType::Dec && value == &DataType::Int) {
			return true;
		}

		match (target, value) {
			(DataType::Array(target_element), DataType::Array(value_element)) => {
				self.is_assignable_with_numeric_conversion(target_element, value_element, allow_numeric_conversion)
			}
			(DataType::Array(_), DataType::EmptyArray) => true,
			(DataType::Union(target_members), DataType::Union(value_members)) => {
				value_members.iter().all(|value_member| {
					target_members.iter().any(|target_member| {
						self.is_assignable_with_numeric_conversion(target_member, value_member, allow_numeric_conversion)
					})
				})
			}
			(DataType::Union(target_members), _) => {
				target_members.iter().any(|target_member| {
					self.is_assignable_with_numeric_conversion(target_member, value, allow_numeric_conversion)
				})
			}
			_ => false,
		}
	}

	fn is_function_argument_assignable(&self, target: &DataType, value: &DataType) -> bool {
		self.is_assignable_with_numeric_conversion(target, value, false)
	}

	fn is_numeric_type(&self, data_type: &DataType) -> bool {
		matches!(data_type.without_nullability(), DataType::Dec | DataType::Int)
	}

	fn is_read_only_record_pointer_expression(&self, expression: &Expr) -> bool {
		match expression {
			Expr::Find(_) => true,
			Expr::Identifier(identifier) => self.lookup_local(&identifier.name)
				.is_some_and(|local| local.is_const),
			_ => false,
		}
	}

	fn is_truthy_condition_type(&self, data_type: &DataType) -> bool {
		matches!(data_type.without_nullability(), DataType::Bool | DataType::RecordPointer(_))
	}

	fn is_valid_enum_backing_type(&self, data_type: &DataType) -> bool {
		matches!(
			data_type,
			DataType::Bool
				| DataType::Date
				| DataType::Dec
				| DataType::Int
				| DataType::Text
				| DataType::Time
				| DataType::TimeTz
				| DataType::Timestamp
				| DataType::TimestampTz
		)
	}

	fn local_declaration_error(
		&self,
		declaration: &str,
		name: &str,
		position: usize,
		duplicate_message: String,
	) -> Option<CompileError> {
		self.current_scope_declaration_kind(name).map(|existing| {
			if existing == LexicalDeclarationKind::Local {
				self.compile_error(position, duplicate_message)
			}
			else {
				self.declaration_conflict_error(declaration, name, position, existing)
			}
		})
	}

	fn lookup_enum(&self, name: &str) -> Option<&EnumBinding> {
		if self.declaration_kinds.lookup(name) != Some(&LexicalDeclarationKind::Enum) {
			return None;
		}
		self.enums.lookup(name)
	}

	fn lookup_functions(&self, name: &str) -> Option<&[FunctionSignature]> {
		if self.declaration_kinds.lookup(name) != Some(&LexicalDeclarationKind::Function) {
			return None;
		}
		self.functions.lookup(name)
			.map(Vec::as_slice)
	}

	fn lookup_local(&self, name: &str) -> Option<LocalBinding> {
		if self.declaration_kinds.lookup(name) != Some(&LexicalDeclarationKind::Local) {
			return None;
		}
		self.locals.lookup(name).cloned()
	}

	fn lookup_object_field(&self, object_name: &str, field_name: &str) -> Option<&crate::ast::ObjectFieldDeclaration> {
		self.lookup_object_type(object_name)?
			.declaration()
			.fields()?
			.iter()
			.find(|field| field.name == field_name)
	}

	fn lookup_object_type(&self, name: &str) -> Option<&ResolvedObjectType> {
		self.semantic_program.object_type(self.lookup_object_type_id(name)?)
	}

	fn lookup_object_type_id(&self, name: &str) -> Option<ObjectTypeId> {
		if self.declaration_kinds.lookup(name) != Some(&LexicalDeclarationKind::Object) {
			return None;
		}
		self.object_type_bindings.lookup(name).copied()
	}

	fn lookup_sequence_alias(&self, name: &str) -> Option<ResolvedSequenceReference> {
		self.sequence_aliases.lookup(name).cloned().flatten()
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
		let (backend, database_name, schema_name, schema_is_implicit, table_name, schema_column_definitions) = {
			let resolved_table = self.resolve_table_reference(&find.table)?;
			let primary_key_columns = resolved_table.table().primary_key_columns()
				.into_iter()
				.map(|column| column.name().to_string())
				.collect::<BTreeSet<_>>();
			let schema_column_definitions = resolved_table.table().columns()
				.map(|column| (
					column.name().to_string(),
					column.data_type().clone(),
					column.is_nullable(),
					primary_key_columns.contains(column.name()),
				))
				.collect::<Vec<_>>();
			(
				resolved_table.database().backend(),
				resolved_table.database().name().to_string(),
				resolved_table.schema().name().to_string(),
				resolved_table.schema().is_implicit(),
				resolved_table.table().name().to_string(),
				schema_column_definitions,
			)
		};
		let schema_columns = schema_column_definitions.into_iter()
			.map(|(column_name, schema_type, is_nullable, is_primary_key)| Ok(QueryResultColumn {
				column_name,
				data_type: self.data_type_from_schema_type(&schema_type)?,
				is_nullable,
				is_primary_key,
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
			lock_mode: self.find_lock_mode,
			order_by,
			record_layout: QueryRecordLayout::all_known(schema_columns),
			schema_is_implicit,
			schema_name,
			table_name,
		})
	}

	fn lower_for_record_query(
		&mut self,
		for_record: &ForRecordStatement,
		limit: Option<QueryParameter>,
	) -> Result<QueryForPlan, CompileError> {
		let (backend, database_name, schema_name, schema_is_implicit, table_name, schema_column_definitions) = {
			let resolved_table = self.resolve_table_reference(&for_record.table)?;
			let primary_key_columns = resolved_table.table().primary_key_columns()
				.into_iter()
				.map(|column| column.name().to_string())
				.collect::<BTreeSet<_>>();
			let schema_column_definitions = resolved_table.table().columns()
				.map(|column| (
					column.name().to_string(),
					column.data_type().clone(),
					column.is_nullable(),
					primary_key_columns.contains(column.name()),
				))
				.collect::<Vec<_>>();
			(
				resolved_table.database().backend(),
				resolved_table.database().name().to_string(),
				resolved_table.schema().name().to_string(),
				resolved_table.schema().is_implicit(),
				resolved_table.table().name().to_string(),
				schema_column_definitions,
			)
		};
		let schema_columns = schema_column_definitions.into_iter()
			.map(|(column_name, schema_type, is_nullable, is_primary_key)| Ok(QueryResultColumn {
				column_name,
				data_type: self.data_type_from_schema_type(&schema_type)?,
				is_nullable,
				is_primary_key,
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
		let group_by = for_record.group_by.iter()
			.map(|item| {
				let data_type = self.infer_query_expression_type(&item.expression, &for_record.table)?;
				Ok(QueryGroupByItem {
					alias: item.alias.as_ref().map(|alias| alias.name.clone()),
					data_type,
					expression: self.lower_query_expression(&item.expression, &for_record.table, backend)?,
					key_names: self.group_by_item_key_names(item),
				})
			})
			.collect::<Result<Vec<_>, CompileError>>()?;

		Ok(QueryForPlan {
			backend,
			database_name,
			filter,
			group_by,
			limit,
			lock_mode: if for_record.is_mut { RecordLockMode::Update } else { RecordLockMode::None },
			order_by,
			record_layout: QueryRecordLayout::all_known(schema_columns),
			schema_is_implicit,
			schema_name,
			table_name,
		})
	}

	fn lower_query_binary_operator(
		&self,
		operator: BinaryOperator,
		result_type: &DataType,
	) -> QueryBinaryOperator {
		match operator {
			BinaryOperator::Add if matches!(
				result_type.without_nullability(),
				DataType::Array(_) | DataType::Text
			) => QueryBinaryOperator::Concatenate,
			BinaryOperator::Add => QueryBinaryOperator::Add,
			BinaryOperator::And => QueryBinaryOperator::And,
			BinaryOperator::Divide if result_type.without_nullability() == &DataType::Int => QueryBinaryOperator::IntegerDivide,
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
		match expression {
			Expr::Array(_) => Err(self.compile_error(
				expression.position(),
				format!(
					"Array literals are only supported as the first argument to `contains(...)` in `{}` database query expressions.",
					backend.name(),
				),
			)),
			Expr::Binary(BinaryExpr { left, operator, right, .. }) => {
				let result_type = self.infer_query_expression_type(expression, table)?;

				if let Some(value) = self.semantic_program.constant_boolean_expression(expression.position()) {
					return Ok(QueryExpr::Literal(QueryLiteral::Boolean(value)));
				}

				let null_checked_operand = match (left.as_ref(), right.as_ref()) {
					(Expr::Null(_), operand) | (operand, Expr::Null(_)) => Some(operand),
					_ => None,
				};
				if let Some(operand) = null_checked_operand {
					return Ok(QueryExpr::Unary(QueryUnaryExpr {
						operand: Box::new(self.lower_query_expression(operand, table, backend)?),
						operator: match operator {
							BinaryOperator::Equal => QueryUnaryOperator::IsNull,
							BinaryOperator::NotEqual => QueryUnaryOperator::IsNotNull,
							_ => unreachable!("Only equality operators may compare against `null`."),
						},
					}));
				}

				Ok(QueryExpr::Binary(QueryBinaryExpr {
					left: Box::new(self.lower_query_expression(left, table, backend)?),
					operator: self.lower_query_binary_operator(*operator, &result_type),
					right: Box::new(self.lower_query_expression(right, table, backend)?),
				}))
			}
			Expr::Boolean(boolean) => Ok(QueryExpr::Literal(QueryLiteral::Boolean(boolean.value))),
			Expr::Call(CallExpr { arguments, callee, position }) => {
				let Some(built_in) = BuiltInFunction::from_name(&callee.name) else {
					return Err(self.compile_error(
						callee.position,
						format!(
							"Function `{}` is not supported in `{}` database query expressions.",
							callee.name,
							backend.name(),
						),
					));
				};
				if !matches!(
					built_in,
					BuiltInFunction::Contains
						| BuiltInFunction::CountOf
						| BuiltInFunction::IndexOf
						| BuiltInFunction::Trim
						| BuiltInFunction::Day
						| BuiltInFunction::Month
						| BuiltInFunction::Year
						| BuiltInFunction::Hour
						| BuiltInFunction::Minute
						| BuiltInFunction::Second
				) {
					return Err(self.compile_error(
						callee.position,
						format!(
							"Function `{}` is not supported in `{}` database query expressions.",
							callee.name,
							backend.name(),
						),
					));
				}
				self.infer_built_in_call_type_for_query(built_in, arguments, *position, table)?;
				let argument_bindings = self.semantic_program.call_argument_bindings(*position)
					.expect("Built-in query calls must have canonical argument bindings.");
				let ordered_arguments = Self::call_arguments_in_binding_order(arguments, argument_bindings);

				match built_in {
					BuiltInFunction::Contains => {
						let left = ordered_arguments[0].expression()
							.expect("Built-in arguments must be expressions after default-marker validation.");
						let left_type = self.infer_query_expression_type(left, table)?;

						if matches!(
							left_type.without_nullability(),
							DataType::Array(element_type) if !matches!(element_type.without_nullability(), DataType::Text)
						) {
							return Err(self.compile_error(
								expression.position(),
								String::from(
									"Built-in function `contains` is only supported for `[text]` array arguments in database query expressions.",
								),
							));
						}

						if matches!(
							left,
							Expr::Array(_)
						) && !matches!(
							left_type.without_nullability(),
							DataType::Array(element_type) if matches!(element_type.without_nullability(), DataType::Text)
						) {
							return Err(self.compile_error(
								expression.position(),
								String::from(
									"Built-in function `contains` is only supported for `[text]` array arguments in database query expressions.",
								),
							));
						}
					}
					BuiltInFunction::CountOf | BuiltInFunction::IndexOf => {
						let right = ordered_arguments[1].expression()
							.expect("Built-in arguments must be expressions after default-marker validation.");
						let right_type = self.infer_query_expression_type(right, table)?;

						if matches!(
							right_type.without_nullability(),
							DataType::Array(element_type) if matches!(element_type.without_nullability(), DataType::Text)
						) {
							return Err(self.compile_error(
								expression.position(),
								format!(
									"Built-in function `{}` is not yet supported for array arguments in database query expressions.",
									built_in.name(),
								),
							));
						}
					}
					BuiltInFunction::Trim => {}
					BuiltInFunction::Day
					| BuiltInFunction::Month
					| BuiltInFunction::Year
					| BuiltInFunction::Hour
					| BuiltInFunction::Minute
					| BuiltInFunction::Second => {}
					_ => unreachable!("Unsupported query built-ins are rejected before argument analysis."),
				}

				Ok(QueryExpr::BuiltInCall(QueryBuiltInCall {
					arguments: ordered_arguments.iter()
						.enumerate()
						.map(|(index, argument)| {
							let argument = argument.expression()
								.expect("Built-in arguments must be expressions after default-marker validation.");
							if built_in == BuiltInFunction::Contains && index == 0
								&& let Expr::Array(array) = argument {
								return Ok(QueryExpr::ArrayLiteral(
									array.elements.iter()
										.map(|element| self.lower_query_expression(element, table, backend))
										.collect::<Result<Vec<_>, _>>()?,
								));
							}

							self.lower_query_expression(argument, table, backend)
						})
						.collect::<Result<Vec<_>, _>>()?,
					built_in,
				}))
			}
			Expr::Date(date) => Ok(QueryExpr::Literal(QueryLiteral::Date(date.value))),
			Expr::Decimal(decimal) => Ok(QueryExpr::Literal(QueryLiteral::Decimal(decimal.value.clone()))),
			Expr::FieldAccess(field_access) => self.lower_query_field_access(field_access, table, backend),
			Expr::Identifier(identifier) => {
				if let Some(local) = self.lookup_local(&identifier.name) {
					self.semantic_program.identifier_slots.insert(identifier.position, local.slot);
					return Ok(QueryExpr::Parameter(QueryParameter {
						data_type: local.data_type,
						field_path: Vec::new(),
						slot: local.slot,
					}));
				}

				let (table_name, column_name, column_type, is_nullable) = {
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
						column.is_nullable(),
					)
				};

				Ok(QueryExpr::Column(QueryColumnReference {
					column_name,
					data_type: self.data_type_from_schema_column(&column_type, is_nullable)?,
					table_name,
				}))
			}
			Expr::Integer(integer) => Ok(QueryExpr::Literal(QueryLiteral::Integer(integer.value))),
			Expr::Text(text) => Ok(QueryExpr::Literal(QueryLiteral::Text(text.value.clone()))),
			Expr::Time(time) => Ok(QueryExpr::Literal(QueryLiteral::Time(time.value))),
			Expr::TimeTz(time) => Ok(QueryExpr::Literal(QueryLiteral::TimeTz(time.value))),
			Expr::Timestamp(timestamp) => Ok(QueryExpr::Literal(QueryLiteral::Timestamp(timestamp.value))),
			Expr::TimestampTz(timestamp) => Ok(QueryExpr::Literal(QueryLiteral::TimestampTz(timestamp.value))),
			Expr::Unary(UnaryExpr { operand, operator, .. }) => Ok(QueryExpr::Unary(QueryUnaryExpr {
				operand: Box::new(self.lower_query_expression(operand, table, backend)?),
				operator: match operator {
					UnaryOperator::Negate => QueryUnaryOperator::Negate,
					UnaryOperator::Not => QueryUnaryOperator::Not,
					UnaryOperator::Exists | UnaryOperator::Locked => {
						unreachable!("Presence and lock operators are rejected before database-query lowering.")
					}
				},
			})),
			_ => Err(self.compile_error(
				expression.position(),
				format!(
					"This expression cannot be used in a `{}` database query.",
					backend.name(),
				),
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

		if let Some((base_identifier, fields)) = self.query_field_access_chain(field_access) {
			if self.lookup_local(&base_identifier.name).is_some() {
				let (slot, field_path, data_type) = self.resolve_query_local_field_access(base_identifier, &fields)?;
				return Ok(QueryExpr::Parameter(QueryParameter {
					data_type,
					field_path,
					slot,
				}));
			}

			if fields.len() > 1 {
				return Err(self.compile_error(
					field_access.position,
					String::from("Only simple `table.field` access is supported for query table columns."),
				));
			}

			let resolved_table = self.resolve_table_reference(table)?;
			let resolved_table_name = resolved_table.table().name().to_string();

			if !base_identifier.name.eq_ignore_ascii_case(&resolved_table_name) {
				return Err(self.compile_error(
					base_identifier.position,
					format!("Qualified field reference must use the target table name `{resolved_table_name}`."),
				));
			}

			let field = fields[0];
			let (column_name, column_type, is_nullable) = {
				let Some(column) = resolved_table.table().column(&field.name) else {
					return Err(self.compile_error(
						field.position,
						format!(
							"Field `{}` does not exist on table `{resolved_table_name}`.",
							field.name,
						),
					));
				};
				(column.name().to_string(), column.data_type().clone(), column.is_nullable())
			};

			return Ok(QueryExpr::Column(QueryColumnReference {
				column_name,
				data_type: self.data_type_from_schema_column(&column_type, is_nullable)?,
				table_name: resolved_table_name,
			}));
		}

		Err(self.compile_error(
			field_access.position,
			String::from("Only chained local object or record-pointer field access and simple `table.field` access may be used in a database query."),
		))
	}

	fn merge_array_element_types(&self, lhs: &DataType, rhs: &DataType, position: usize) -> Result<DataType, CompileError> {
		let lhs_non_null = lhs.is_non_nullable();
		let rhs_non_null = rhs.is_non_nullable();
		let lhs = lhs.without_nullability();
		let rhs = rhs.without_nullability();

		let result = if lhs == rhs {
			lhs.clone()
		}
		else if self.is_numeric_type(lhs) && self.is_numeric_type(rhs) {
			if lhs == &DataType::Int && rhs == &DataType::Int {
				DataType::Int
			}
			else {
				DataType::Dec
			}
		}
		else if let (DataType::Array(lhs_element), DataType::Array(rhs_element)) = (lhs, rhs) {
			DataType::Array(Box::new(self.merge_array_element_types(lhs_element, rhs_element, position)?))
		}
		else {
			return Err(self.compile_error(
				position,
				format!(
					"Array literal elements must have compatible types, found `{}` and `{}`.",
					lhs.name(),
					rhs.name(),
				),
			));
		};

		Ok(if !lhs_non_null || !rhs_non_null {
			result.into_nullable()
		}
		else {
			result
		})
	}

	fn named_data_types_have_same_identity(&self, target: &DataType, value: &DataType) -> Option<bool> {
		let (DataType::Object(target_name), DataType::Object(value_name)) = (target, value) else {
			return None;
		};
		let target_object_id = self.lookup_object_type_id(target_name);
		let value_object_id = self.lookup_object_type_id(value_name);

		Some(match (target_object_id, value_object_id) {
			(Some(target_id), Some(value_id)) => target_id == value_id,
			(Some(_), None) | (None, Some(_)) => false,
			(None, None) => target_name == value_name
				&& (self.lookup_enum(target_name).is_some()
					|| target_name.starts_with(BUILT_IN_ENUM_TYPE_PREFIX)),
		})
	}

	fn numeric_result_type(&self, lhs: &DataType, rhs: &DataType, position: usize) -> Result<DataType, CompileError> {
		if !self.is_numeric_type(lhs) || !self.is_numeric_type(rhs) {
			return Err(self.compile_error(
				position,
				format!(
					"Expected numeric operands, found `{}` and `{}`.",
					lhs.name(),
					rhs.name(),
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

	fn push_call_shape_argument(
		shape: &mut FunctionCallShape,
		parameter: &FunctionParameterSignature,
		name: Option<String>,
		data_type: DataType,
	) {
		shape.arguments.push(CallArgument {
			is_by_ref: parameter.is_by_ref,
			name: name.map(|name| IdentifierExpr {
				name,
				position: 0,
			}),
			position: 0,
			value: CallArgumentValue::Expression(Expr::Null(NullLiteral {
				position: 0,
			})),
		});
		shape.argument_sequences.push(parameter.sequence.clone());
		shape.argument_types.push(Some(data_type));
	}

	fn query_field_access_chain<'a>(
		&self,
		field_access: &'a FieldAccessExpr,
	) -> Option<(&'a IdentifierExpr, Vec<&'a IdentifierExpr>)> {
		let mut fields = vec![&field_access.field];
		let mut object = field_access.object.as_ref();

		loop {
			match object {
				Expr::FieldAccess(field_access) => {
					fields.push(&field_access.field);
					object = field_access.object.as_ref();
				}
				Expr::Identifier(identifier) => {
					fields.reverse();
					return Some((identifier, fields));
				}
				_ => return None,
			}
		}
	}

	fn record_null_comparison_semantics(
		&mut self,
		expression_position: usize,
		left: &Expr,
		left_type: &DataType,
		operator: BinaryOperator,
		right: &Expr,
		right_type: &DataType,
	) {
		if !matches!(operator, BinaryOperator::Equal | BinaryOperator::NotEqual) {
			return;
		}

		let (compared_expression, compared_type) = match (left_type, right_type) {
			(DataType::Null, DataType::Null) => {
				self.semantic_program.constant_boolean_expressions.insert(
					expression_position,
					operator == BinaryOperator::Equal,
				);
				return;
			}
			(DataType::Null, compared_type) => (right, compared_type),
			(compared_type, DataType::Null) => (left, compared_type),
			_ => return,
		};

		if compared_type.is_nullable() {
			self.null_comparison_operands.insert(compared_expression.position());
		}
		else {
			self.semantic_program.constant_boolean_expressions.insert(
				expression_position,
				operator == BinaryOperator::NotEqual,
			);
		}
	}

	fn record_pointer_identity_requirements(
		&self,
		record_pointer: &RecordPointerType,
	) -> (BTreeSet<String>, bool) {
		let Some(schema_catalog) = self.current_schema_catalog.as_ref() else {
			return (BTreeSet::new(), true);
		};
		let Some(database) = schema_catalog.database(&record_pointer.database_name) else {
			return (BTreeSet::new(), true);
		};
		let Some(schema) = database.schema(&record_pointer.schema_name) else {
			return (BTreeSet::new(), true);
		};
		let Some(table) = schema.table(&record_pointer.table_name) else {
			return (BTreeSet::new(), true);
		};
		let identity_fields = table.primary_key_columns().into_iter()
			.map(|column| column.name().to_string())
			.collect::<BTreeSet<_>>();

		if identity_fields.is_empty() {
			(BTreeSet::new(), true)
		}
		else {
			(identity_fields, false)
		}
	}

	fn record_record_pointer_assignment(
		&mut self,
		position: usize,
		field_path: &[crate::ast::IdentifierExpr],
		assignment_position: usize,
	) {
		let Some(binding) = self.semantic_program.record_pointer_bindings.get_mut(&position) else {
			return;
		};

		let path = field_path.iter().map(|field| field.name.as_str()).collect::<Vec<_>>().join(".");
		let assignment = RecordPointerFieldAssignment {
			field_path: path,
			position: assignment_position,
		};
		if !binding.field_assignments.contains(&assignment) {
			binding.field_assignments.push(assignment);
		}
	}

	fn record_record_pointer_escape(&mut self, expression: &Expr) {
		let Expr::Identifier(identifier) = expression else {
			return;
		};
		let Some(local) = self.lookup_local(&identifier.name) else {
			return;
		};

		if !matches!(local.data_type.without_nullability(), DataType::RecordPointer(_)) {
			return;
		}

		let Some(binding) = self.semantic_program.record_pointer_bindings.get_mut(&local.declaration_position) else {
			return;
		};

		binding.escape_positions.insert(identifier.position);
	}

	fn record_record_pointer_field_access_read(&mut self, expression: &Expr) {
		let Expr::FieldAccess(field_access) = expression else {
			return;
		};
		let Some((base_identifier, fields)) = self.query_field_access_chain(field_access) else {
			return;
		};
		let Some(field) = fields.first() else {
			return;
		};
		let Some(local) = self.lookup_local(&base_identifier.name) else {
			return;
		};

		if matches!(local.data_type.without_nullability(), DataType::RecordPointer(_)) {
			self.record_record_pointer_read(local.declaration_position, &field.name, base_identifier.position);
		}
	}

	fn record_record_pointer_read(&mut self, position: usize, field_name: &str, read_position: usize) {
		let canonical_name = self.semantic_program.record_pointer_bindings.get(&position)
			.and_then(|binding| {
				let schema_catalog = self.current_schema_catalog.as_ref()?;
				let database = schema_catalog.database(&binding.data_type.database_name)?;
				let schema = database.schema(&binding.data_type.schema_name)?;
				let table = schema.table(&binding.data_type.table_name)?;
				table.column(field_name).map(|column| column.name().to_string())
			})
			.unwrap_or_else(|| field_name.to_string());
		let Some(binding) = self.semantic_program.record_pointer_bindings.get_mut(&position) else {
			return;
		};

		let read = RecordPointerFieldRead {
			field_name: canonical_name,
			position: read_position,
		};
		if !binding.field_reads.contains(&read) {
			binding.field_reads.push(read);
		}
	}

	fn record_resolved_sequence(
		&mut self,
		position: usize,
		resolved: &ResolvedSequenceReference,
	) {
		self.semantic_program.resolved_sequences.insert(position, resolved.clone());
	}

	fn record_selected_query_shapes(&mut self, query_plan: &ProgramQueryPlan) -> Result<(), CompileError> {
		for query in query_plan.queries() {
			let PlannedQueryExecution::MergeWith {
				query: enclosing_query_id,
			} = query.execution else {
				continue;
			};
			let enclosing_query = query_plan.query(enclosing_query_id).ok_or_else(|| {
				self.compile_error(query.position, String::from("Proven query optimization references an unknown enclosing query."))
			})?;
			let enclosing_record_slot = enclosing_query.record_slot.ok_or_else(|| {
				self.compile_error(query.position, String::from("Proven query optimization requires an enclosing record slot."))
			})?;
			let enclosing_for_query = self.semantic_program.lowered_for_record_queries
				.get(&enclosing_query.position)
				.cloned()
				.ok_or_else(|| {
					self.compile_error(query.position, String::from("Proven query optimization requires an enclosing record query."))
				})?;
			let count_query = self.semantic_program.lowered_count_queries
				.get(&query.position)
				.cloned()
				.ok_or_else(|| {
					self.compile_error(query.position, String::from("Proven count optimization is missing its neutral query plan."))
				})?;
			let value_id = QueryProjectedValueId(u32::try_from(query.id.0).map_err(|_| {
				self.compile_error(query.position, String::from("Program contains too many queries to identify a projected value."))
			})?);
			let correlations = query.captured_parameters.iter()
				.map(|parameter| QueryCorrelation {
					outer_field_path: parameter.field_path.clone(),
					parameter: QueryParameter {
						data_type: parameter.data_type.clone(),
						field_path: parameter.field_path.clone(),
						slot: parameter.slot,
					},
				})
				.collect();
			let shape = self.semantic_program.query_for_shapes
				.entry(enclosing_query.position)
				.or_insert_with(|| QueryForShape {
					query: enclosing_for_query,
					scalar_projections: Vec::new(),
				});
			shape.scalar_projections.push(QueryScalarProjection {
				expression: QueryScalarProjectionExpression::CorrelatedCount(QueryCorrelatedCount {
					correlations,
					query: count_query,
				}),
				value_id,
			});
			self.semantic_program.query_projected_value_bindings.insert(
				query.position,
				QueryProjectedValueBinding {
					enclosing_record_slot,
					value_id,
				},
			);
		}

		Ok(())
	}

	fn record_warning(&mut self, position: usize, message: String) {
		let source_name = self.current_source_name.clone();
		let duplicate = self.semantic_program.warnings.iter().any(|warning| {
			warning.position == position
				&& warning.message == message
				&& warning.source_name == source_name
		});

		if !duplicate {
			self.semantic_program.warnings.push(SemanticWarning {
				message,
				position,
				source_name,
			});
		}
	}

	fn refine_expression_type_from_assumptions(&self, expression: &Expr, data_type: DataType) -> DataType {
		if self.current_non_null_assumptions.iter().any(|assumed| self.expressions_match_for_non_null_refinement(assumed, expression))
			&& let DataType::Nullable(inner) = data_type {
			return *inner;
		}

		data_type
	}

	fn refined_expression_for_branch(
		&self,
		condition: &Expr,
		branch_taken_when_condition_is_truthy: bool,
	) -> Option<Expr> {
		let Expr::Binary(BinaryExpr {
			left,
			operator,
			right,
			..
		}) = condition else {
			return None;
		};

		let refines_non_null = match operator {
			BinaryOperator::NotEqual => branch_taken_when_condition_is_truthy,
			BinaryOperator::Equal => !branch_taken_when_condition_is_truthy,
			_ => return None,
		};

		if !refines_non_null {
			return None;
		}

		let candidate = match (left.as_ref(), right.as_ref()) {
			(candidate, Expr::Null(_)) => candidate,
			(Expr::Null(_), candidate) => candidate,
			_ => return None,
		};

		if !matches!(candidate, Expr::FieldAccess(_) | Expr::Identifier(_)) {
			return None;
		}

		if !self.null_comparison_operands.contains(&candidate.position()) {
			return None;
		}

		Some(candidate.clone())
	}

	fn register_record_pointer_binding(
		&mut self,
		position: usize,
		record_pointer: RecordPointerType,
		initialization: RecordPointerInitialization,
		is_mutable: bool,
		origin: RecordPointerOrigin,
		query_position: Option<usize>,
	) {
		let (identity_fields, identity_requires_all_fields) = if is_mutable
			&& initialization == RecordPointerInitialization::Existing {
			self.record_pointer_identity_requirements(&record_pointer)
		}
		else {
			(BTreeSet::new(), false)
		};

		self.semantic_program.record_pointer_bindings.insert(position, RecordPointerBindingInfo {
			assigned_fields: BTreeSet::new(),
			data_type: record_pointer,
			escape_positions: BTreeSet::new(),
			escapes_analysis: false,
			field_assignments: Vec::new(),
			field_reads: Vec::new(),
			identity_requires_all_fields,
			identity_fields,
			initialization,
			is_mutable,
			origin,
			query_position,
			read_fields: BTreeSet::new(),
		});
	}

	fn reject_default_arguments_for_built_in(
		&self,
		built_in: BuiltInFunction,
		arguments: &[CallArgument],
	) -> Result<(), CompileError> {
		if let Some(default) = arguments.iter().find_map(CallArgument::default_argument) {
			return Err(self.compile_error(
				default.position,
				format!(
					"`default` cannot be used when calling built-in function `{}` because it has no declared parameter default.",
					built_in.name(),
				),
			));
		}

		Ok(())
	}

	fn requested_default_error(
		&self,
		signatures: &[FunctionSignature],
		arguments: &[CallArgument],
	) -> Option<CompileError> {
		for argument in arguments {
			let Some(default) = argument.default_argument() else {
				continue;
			};
			let name = argument.name.as_ref()
				.expect("The parser only permits `default` in named arguments.");
			let matching_parameters = signatures.iter()
				.filter_map(|signature| {
					signature.parameters.iter()
						.position(|parameter| parameter.name == name.name)
						.map(|index| (&signature.parameters[index], signature.parameter_defaults[index].is_some()))
				})
				.collect::<Vec<_>>();

			if matching_parameters.is_empty()
				|| matching_parameters.iter().any(|(parameter, has_default)| {
					!parameter.is_by_ref && !parameter.is_variadic && *has_default
				}) {
				continue;
			}

			let message = if matching_parameters.iter().all(|(parameter, _)| parameter.is_by_ref) {
				format!("`default` cannot be used for by-reference parameter `{}`.", name.name)
			}
			else if matching_parameters.iter().all(|(parameter, _)| parameter.is_variadic) {
				format!("`default` cannot be used for variadic parameter `{}`.", name.name)
			}
			else {
				format!(
					"`default` can only be used for parameter `{}` when it declares a default expression.",
					name.name,
				)
			};

			return Some(self.compile_error(default.position, message));
		}

		None
	}

	fn require_boolean_operands(&self, operator: BinaryOperator, lhs: &DataType, rhs: &DataType, position: usize) -> Result<(), CompileError> {
		if lhs.without_nullability() == &DataType::Bool && rhs.without_nullability() == &DataType::Bool {
			return Ok(());
		}

		Err(self.compile_error(
			position,
			format!(
				"Operator `{}` requires `bool` operands, found `{}` and `{}`.",
				self.operator_name(operator),
				lhs.name(),
				rhs.name(),
			),
		))
	}

	fn require_equality_operands(&self, lhs: &DataType, rhs: &DataType, position: usize) -> Result<(), CompileError> {
		let lhs = lhs.without_nullability();
		let rhs = rhs.without_nullability();

		if lhs == &DataType::Any || rhs == &DataType::Any {
			return Ok(());
		}

		if matches!(lhs, DataType::Union(_) | DataType::Range(_))
			|| matches!(rhs, DataType::Union(_) | DataType::Range(_)) {
			return Err(self.compile_error(
				position,
				format!(
					"Equality comparison is not supported between `{}` and `{}`.",
					lhs.name(),
					rhs.name(),
				),
			));
		}

		match (lhs, rhs) {
			(DataType::Array(lhs_element), DataType::Array(rhs_element)) => {
				return self.require_equality_operands(lhs_element, rhs_element, position);
			}
			(DataType::Array(_), DataType::EmptyArray)
			| (DataType::EmptyArray, DataType::Array(_))
			| (DataType::EmptyArray, DataType::EmptyArray) => return Ok(()),
			_ => {}
		}

		if lhs == rhs || (self.is_numeric_type(lhs) && self.is_numeric_type(rhs)) {
			return Ok(());
		}

		Err(self.compile_error(
			position,
			format!(
				"Equality comparison is not supported between `{}` and `{}`.",
				lhs.name(),
				rhs.name(),
			),
		))
	}

	fn require_ordering_operands(&self, lhs: &DataType, rhs: &DataType, position: usize) -> Result<(), CompileError> {
		let lhs = lhs.without_nullability();
		let rhs = rhs.without_nullability();

		if (lhs == &DataType::Text && rhs == &DataType::Text)
			|| (lhs == &DataType::Date && rhs == &DataType::Date)
			|| (lhs == &DataType::Time && rhs == &DataType::Time)
			|| (lhs == &DataType::TimeTz && rhs == &DataType::TimeTz)
			|| (lhs == &DataType::Timestamp && rhs == &DataType::Timestamp)
			|| (lhs == &DataType::TimestampTz && rhs == &DataType::TimestampTz)
			|| (self.is_numeric_type(lhs) && self.is_numeric_type(rhs))
		{
			return Ok(());
		}

		if matches!(lhs, DataType::Union(_)) || matches!(rhs, DataType::Union(_)) {
			return Err(self.compile_error(
				position,
				format!(
					"Ordering comparison is not supported between `{}` and `{}`.",
					lhs.name(),
					rhs.name(),
				),
			));
		}

		Err(self.compile_error(
			position,
			format!(
				"Ordering comparison is not supported between `{}` and `{}`.",
				lhs.name(),
				rhs.name(),
			),
		))
	}

	fn resolve_declared_object_type_references(
		&self,
		data_type: &DataType,
		path: &mut Vec<ObjectTypeReferencePathComponent>,
		references: &mut Vec<ResolvedObjectTypeReference>,
	) -> Result<(), CompileError> {
		match data_type {
			DataType::Array(element_type) => {
				path.push(ObjectTypeReferencePathComponent::ArrayElement);
				self.resolve_declared_object_type_references(element_type, path, references)?;
				path.pop();
			}
			DataType::Nullable(inner) => {
				path.push(ObjectTypeReferencePathComponent::NullableValue);
				self.resolve_declared_object_type_references(inner, path, references)?;
				path.pop();
			}
			DataType::Object(name) => {
				if let Some(object_type_id) = self.lookup_object_type_id(name) {
					references.push(ResolvedObjectTypeReference {
						object_type_id,
						path: path.clone(),
					});
				}
				else if self.lookup_enum(name).is_none() {
					return Err(self.unknown_object_type_error(name.position, name));
				}
			}
			DataType::Range(element_type) => {
				path.push(ObjectTypeReferencePathComponent::RangeElement);
				self.resolve_declared_object_type_references(element_type, path, references)?;
				path.pop();
			}
			DataType::Union(members) => {
				for (index, member) in members.iter().enumerate() {
					path.push(ObjectTypeReferencePathComponent::UnionMember(
						u32::try_from(index).expect("Union member count exceeded the supported identity path range."),
					));
					self.resolve_declared_object_type_references(member, path, references)?;
					path.pop();
				}
			}
			_ => {}
		}

		Ok(())
	}

	fn resolve_enum_variants(&self, enum_declaration: &EnumDeclaration) -> Result<BTreeMap<String, EnumValue>, CompileError> {
		if enum_declaration.variants.is_empty() {
			return Err(self.compile_error(
				enum_declaration.position,
				format!("Enum `{}` must declare at least one variant.", enum_declaration.name),
			));
		}

		if !self.is_valid_enum_backing_type(&enum_declaration.backing_type) {
			return Err(self.compile_error(
				enum_declaration.position,
				format!(
					"Enum backing type `{}` is not supported. Enum backing types must be non-nullable primitive types other than `json`.",
					enum_declaration.backing_type.name(),
				),
			));
		}

		let mut variants = BTreeMap::new();
		let mut next_value = 1_i64;

		for variant in &enum_declaration.variants {
			if variants.contains_key(&variant.name) {
				return Err(self.compile_error(
					variant.position,
					format!("Enum variant `{}` is already declared on enum `{}`.", variant.name, enum_declaration.name),
				));
			}

			let (value, advance_next_int) = if let Some(expression) = &variant.value {
				let Some(value) = self.enum_literal_constant(expression, &enum_declaration.backing_type) else {
					return Err(self.compile_error(
						expression.position(),
						format!(
							"Enum variant `{}` on enum `{}` must use a literal value of type `{}`.",
							variant.name,
							enum_declaration.name,
							enum_declaration.backing_type.name(),
						),
					));
				};

				(value, enum_declaration.backing_type == DataType::Int)
			}
			else {
				if enum_declaration.backing_type != DataType::Int {
					return Err(self.compile_error(
						variant.position,
						format!(
							"Enum variant `{}` on enum `{}` must specify a value because the backing type is `{}`.",
							variant.name,
							enum_declaration.name,
							enum_declaration.backing_type.name(),
						),
					));
				}

				let value = next_value;
				(Constant::Integer(value), true)
			};

			if advance_next_int
				&& let Constant::Integer(value) = &value {
				next_value = value.checked_add(1).ok_or(self.compile_error(
					variant.position,
					format!(
						"Enum variant `{}` on enum `{}` exceeds the supported `int` range.",
						variant.name,
						enum_declaration.name,
					),
				))?;
			}

			variants.insert(variant.name.clone(), EnumValue::Constant(value));
		}

		Ok(variants)
	}

	fn resolve_function_parameter_type(
		&mut self,
		parameter: &FunctionParameter,
	) -> Result<(DataType, Option<ResolvedSequenceReference>), CompileError> {
		match &parameter.data_type {
			FunctionParameterType::RecordPointer(table) => {
				let resolved_table = self.resolve_table_reference(table)?;
				Ok((DataType::RecordPointer(RecordPointerType {
					database_name: resolved_table.database().name().to_string(),
					schema_name: resolved_table.schema().name().to_string(),
					table_name: resolved_table.table().name().to_string(),
				}), None))
			}
			FunctionParameterType::Sequence(sequence) => {
				let resolved = self.resolve_sequence_reference(sequence)?;
				Ok((DataType::Int, Some(ResolvedSequenceReference {
					database_name: resolved.database().name().to_string(),
					schema_is_implicit: resolved.schema().is_implicit(),
					schema_name: resolved.schema().name().to_string(),
					sequence_name: resolved.sequence().name().to_string(),
				})))
			}
			FunctionParameterType::Value(data_type) => {
				self.validate_declared_data_type(
					data_type,
					parameter.position,
					format!("Parameter `{}` cannot have type `{}`.", parameter.name, parameter.data_type.name()),
				)?;
				Ok((data_type.clone(), None))
			}
		}
	}

	fn resolve_inline_object_visibility(&mut self) {
		loop {
			let mut exposed_inline_ids = BTreeSet::new();
			for object_type in self.semantic_program.object_types.values()
				.filter(|object_type| object_type.visibility == Visibility::Public)
			{
				let mut exposed_names = BTreeSet::new();
				match &object_type.declaration.shape {
					ObjectDeclarationShape::Array(element_type) => {
						Self::collect_referenced_object_names(element_type, &mut exposed_names);
					}
					ObjectDeclarationShape::Fields(fields) => {
						for field in fields.iter().filter(|field| field.visibility == Visibility::Public) {
							Self::collect_referenced_object_names(&field.data_type, &mut exposed_names);
						}
					}
				}

				for name in exposed_names {
					let Some(exposed_id) = self.lookup_object_type_id(&name) else {
						continue;
					};
					if self.semantic_program.object_type(exposed_id)
						.is_some_and(|exposed_type| exposed_type.containing_object_id == Some(object_type.id))
					{
						exposed_inline_ids.insert(exposed_id);
					}
				}
			}

			let mut changed = false;
			for id in exposed_inline_ids {
				let object_type = self.semantic_program.object_types.get_mut(&id)
					.expect("Exposed inline object type is missing its semantic declaration.");
				if object_type.visibility == Visibility::Private {
					object_type.visibility = Visibility::Public;
					changed = true;
				}
			}

			if !changed {
				break;
			}
		}
	}

	fn resolve_query_local_field_access(
		&mut self,
		base_identifier: &IdentifierExpr,
		fields: &[&IdentifierExpr],
	) -> Result<(u32, Vec<String>, DataType), CompileError> {
		let local = self.lookup_local(&base_identifier.name).ok_or(self.compile_error(
			base_identifier.position,
			format!("Variable `{}` is not declared in this scope.", base_identifier.name),
		))?;
		self.semantic_program.identifier_slots.insert(base_identifier.position, local.slot);

		if matches!(local.data_type.without_nullability(), DataType::RecordPointer(_))
			&& let Some(field) = fields.first() {
			self.record_record_pointer_read(local.declaration_position, &field.name, base_identifier.position);
		}

		let mut current_type = local.data_type.clone();
		let mut field_path = Vec::with_capacity(fields.len());

		for field in fields {
			field_path.push(field.name.clone());
			current_type = match current_type.without_nullability() {
				DataType::Object(name) => self.lookup_object_field(name, &field.name)
					.ok_or(self.compile_error(
						field.position,
						format!("Object `{name}` does not contain a field named `{}`.", field.name),
					))?
					.data_type
					.clone(),
				DataType::RecordPointer(record_pointer) => self.infer_record_pointer_field_access_type(
					record_pointer,
					field.position,
					&field.name,
				)?,
				other => {
					return Err(self.compile_error(
						field.position,
						format!("Field access requires an object operand, found `{}`.", other.name()),
					));
				}
			};
		}

		Ok((local.slot, field_path, current_type))
	}

	fn resolve_sequence_reference(
		&mut self,
		sequence: &SequenceReference,
	) -> Result<crate::schema::ResolvedSequence<'_>, CompileError> {
		if let Some(resolved) = self.semantic_program.resolved_sequences.get(&sequence.position) {
			let schema_catalog = self.current_schema_catalog.as_ref().ok_or(self.compile_error(
				sequence.position,
				String::from("Database sequences require schema metadata, but none was supplied."),
			))?;

			let resolved = schema_catalog.resolve_database_schema_sequence(
				&self.semantic_program.active_databases.iter().map(String::as_str).collect::<Vec<_>>(),
				&resolved.database_name,
				&resolved.schema_name,
				&resolved.sequence_name,
			).map_err(|error| self.schema_error_to_compile_error(sequence.position, error))?;
			self.validate_sequence_backend(sequence.position, &resolved)?;
			return Ok(resolved);
		}

		let schema_catalog = self.current_schema_catalog.as_ref().ok_or(self.compile_error(
			sequence.position,
			String::from("Database sequences require schema metadata, but none was supplied."),
		))?;
		let active_databases = self.semantic_program.active_databases.iter().map(String::as_str).collect::<Vec<_>>();

		let resolved = match sequence.components.as_slice() {
			[sequence_name] => schema_catalog.resolve_sequence(&active_databases, &sequence_name.name),
			[first, sequence_name] => {
				match schema_catalog.resolve_database_sequence(&active_databases, &first.name, &sequence_name.name) {
					Ok(resolved) => Ok(resolved),
					Err(SchemaError::UnknownDatabase { .. }) | Err(SchemaError::AmbiguousDatabaseQualifiedSequenceName { .. }) => {
						schema_catalog.resolve_schema_sequence(&active_databases, &first.name, &sequence_name.name)
					}
					Err(other) => Err(other),
				}
			}
			[database, schema, sequence_name] => {
				schema_catalog.resolve_database_schema_sequence(&active_databases, &database.name, &schema.name, &sequence_name.name)
			}
			_ => Err(SchemaError::UnknownSequence {
				sequence_name: String::from("<invalid sequence reference>"),
			}),
		}.map_err(|error| self.schema_error_to_compile_error(sequence.position, error))?;

		self.validate_sequence_backend(sequence.position, &resolved)?;
		Ok(resolved)
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
			schema_is_implicit: resolved.schema().is_implicit(),
			schema_name: resolved.schema().name().to_string(),
			table_name: resolved.table().name().to_string(),
		});

		Ok(resolved)
	}

	fn schema_error_to_compile_error(&self, position: usize, error: SchemaError) -> CompileError {
		self.compile_error(position, match error {
			SchemaError::AmbiguousDatabaseQualifiedSequenceName { database_name, sequence_name } => {
				format!(
					"Sequence reference `{database_name}.{sequence_name}` is ambiguous because database `{database_name}` contains multiple schemas."
				)
			}
			SchemaError::AmbiguousDatabaseQualifiedTableName { database_name, table_name } => {
				format!(
					"Table reference `{database_name}.{table_name}` is ambiguous because database `{database_name}` contains multiple schemas."
				)
			}
			SchemaError::AmbiguousSchemaQualifiedSequenceName { active_databases, schema_name, sequence_name } => {
				format!(
					"Sequence reference `{schema_name}.{sequence_name}` is ambiguous across active databases: {}.",
					active_databases.join(", ")
				)
			}
			SchemaError::AmbiguousSchemaQualifiedTableName { active_databases, schema_name, table_name } => {
				format!(
					"Table reference `{schema_name}.{table_name}` is ambiguous across active databases: {}.",
					active_databases.join(", ")
				)
			}
			SchemaError::AmbiguousSequenceName { active_databases, sequence_name } => {
				format!(
					"Sequence reference `{sequence_name}` is ambiguous across active databases: {}.",
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
			| SchemaError::DuplicateSequence { .. }
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
			SchemaError::UnknownSequence { sequence_name } => {
				format!("Sequence `{sequence_name}` is not present in the active databases.")
			}
			SchemaError::UnknownTable { table_name } => {
				format!("Table `{table_name}` is not present in the active databases.")
			}
		})
	}

	fn select_function_overload(
		&self,
		callable_kind: &str,
		name: &str,
		signatures: &[FunctionSignature],
		arguments: &[CallArgument],
		argument_types: &[Option<DataType>],
		position: usize,
	) -> Result<(FunctionSignature, Vec<CallArgumentBinding>), CompileError> {
		let argument_sequences = arguments.iter()
			.map(|argument| {
				argument.expression()
					.and_then(|expression| self.semantic_program.resolved_sequence(expression.position()))
					.cloned()
			})
			.collect::<Vec<_>>();

		if signatures.len() == 1 {
			let argument_bindings = self.bind_call_arguments(&signatures[0], arguments).map_err(|error| {
				self.requested_default_error(signatures, arguments).unwrap_or_else(|| {
					self.call_argument_binding_compile_error(
						callable_kind,
						name,
						arguments,
						position,
						error,
					)
				})
			})?;
			return Ok((signatures[0].clone(), argument_bindings));
		}

		let mut candidates = Vec::new();
		let mut rejections = Vec::new();
		for signature in signatures {
			let description = self.describe_function_signature(name, signature);
			let argument_bindings = match self.bind_call_arguments(signature, arguments) {
				Ok(argument_bindings) => argument_bindings,
				Err(error) => {
					rejections.push(format!(
						"Candidate `{description}` {}.",
						self.call_argument_binding_rejection_description(&error),
					));
					continue;
				}
			};

			let rejection = signature.parameters.iter()
				.zip(argument_bindings.iter())
				.find_map(|(parameter, binding)| {
					self.call_binding_type_rejection(
						parameter,
						binding,
						arguments,
						&argument_sequences,
						argument_types,
					)
				});
			if let Some(rejection) = rejection {
				rejections.push(format!("Candidate `{description}` {rejection}."));
			}
			else {
				candidates.push((signature.clone(), argument_bindings));
			}
		}

		match candidates.as_slice() {
			[(signature, argument_bindings)] => Ok((signature.clone(), argument_bindings.clone())),
			[] => Err(self.requested_default_error(signatures, arguments).unwrap_or_else(|| {
				let rejection_summary = rejections.join(" ");
				self.compile_error(
					position,
					format!(
						"No overload of {callable_kind} `{name}` accepts the supplied arguments. {rejection_summary}",
					),
				)
			})),
			_ => {
				let overloads = candidates.iter()
					.map(|(signature, _)| format!("`{}`", self.describe_function_signature(name, signature)))
					.collect::<Vec<_>>()
					.join(", ");
				Err(self.compile_error(
					position,
					format!("Call to {callable_kind} `{name}` is ambiguous between the following overloads: {overloads}."),
				))
			}
		}
	}

	fn sequence_reference_from_expression(&self, expression: &Expr) -> Option<SequenceReference> {
		match expression {
			Expr::Identifier(identifier) => Some(SequenceReference {
				components: vec![identifier.clone()],
				position: identifier.position,
			}),
			Expr::FieldAccess(field_access) => {
				let (base, fields) = self.query_field_access_chain(field_access)?;
				if self.lookup_local(&base.name).is_some() {
					return None;
				}

				let mut components = vec![base.clone()];
				components.extend(fields.into_iter().cloned());
				Some(SequenceReference {
					position: base.position,
					components,
				})
			}
			_ => None,
		}
	}

	fn signature_accepts_call_shape(
		&self,
		signature: &FunctionSignature,
		shape: &FunctionCallShape,
	) -> bool {
		let Ok(bindings) = self.bind_call_arguments(signature, &shape.arguments) else {
			return false;
		};

		signature.parameters.iter()
			.zip(bindings.iter())
			.all(|(parameter, binding)| {
				self.call_binding_accepts_types(
					parameter,
					binding,
					&shape.arguments,
					&shape.argument_sequences,
					&shape.argument_types,
				)
			})
	}

	fn statement_guarantees_return(&self, statement: &Statement) -> bool {
		match statement {
			Statement::Block(block) => self.block_guarantees_return(block),
			Statement::Break(_) | Statement::Continue(_) => false,
			Statement::Create(_) => false,
			Statement::Delete(_) => false,
			Statement::EnumDeclaration(_) => false,
			Statement::FunctionDeclaration(_) => false,
			Statement::If(if_statement) => {
				self.block_guarantees_return(&if_statement.then_branch)
					&& if_statement.else_branch.as_ref().is_some_and(|else_branch| self.statement_guarantees_return(else_branch))
			}
			Statement::RecordPointerDeclaration(_) => false,
			Statement::Return(_) => true,
			Statement::Transaction(transaction_statement) => self.block_guarantees_return(&transaction_statement.body),
			Statement::Expression(_)
			| Statement::For(_)
			| Statement::ForRecord(_)
			| Statement::Update(_)
			| Statement::Use(_)
			| Statement::VariableDeclaration(_)
			| Statement::While(_) => false,
		}
	}

	fn try_resolve_sequence_expression(
		&mut self,
		expression: &Expr,
	) -> Result<Option<ResolvedSequenceReference>, CompileError> {
		let Some(reference) = self.sequence_reference_from_expression(expression) else {
			return Ok(None);
		};

		if reference.components.len() == 1 && self.lookup_local(&reference.components[0].name).is_some() {
			return Ok(None);
		}

		let resolved = self.resolve_sequence_reference(&reference)?;
		Ok(Some(ResolvedSequenceReference {
			database_name: resolved.database().name().to_string(),
			schema_is_implicit: resolved.schema().is_implicit(),
			schema_name: resolved.schema().name().to_string(),
			sequence_name: resolved.sequence().name().to_string(),
		}))
	}

	fn try_resolve_unqualified_sequence(
		&mut self,
		name: &str,
		position: usize,
	) -> Result<Option<ResolvedSequenceReference>, CompileError> {
		let schema_catalog = match self.current_schema_catalog.as_ref() {
			Some(schema_catalog) => schema_catalog,
			None => return Ok(None),
		};
		let active_databases = self.semantic_program.active_databases.iter().map(String::as_str).collect::<Vec<_>>();

		match schema_catalog.resolve_sequence(&active_databases, name) {
			Ok(resolved) => {
				self.validate_sequence_backend(position, &resolved)?;
				Ok(Some(ResolvedSequenceReference {
					database_name: resolved.database().name().to_string(),
					schema_is_implicit: resolved.schema().is_implicit(),
					schema_name: resolved.schema().name().to_string(),
					sequence_name: resolved.sequence().name().to_string(),
				}))
			}
			Err(SchemaError::UnknownSequence { .. }) => Ok(None),
			Err(error) => Err(self.schema_error_to_compile_error(position, error)),
		}
	}

	fn unknown_object_type_error(&self, position: usize, name: &str) -> CompileError {
		if !name.contains('.') {
			let matching_inline_names = self.semantic_program.object_types.values()
				.filter(|object_type| {
					object_type.containing_object_id().is_some()
						&& object_type.has_named_constructor()
						&& object_type.display_name().rsplit('.').next() == Some(name)
				})
				.map(|object_type| object_type.display_name())
				.collect::<Vec<_>>();
			if let [qualified_name] = matching_inline_names.as_slice() {
				return self.compile_error(
					position,
					format!("Named inline object type `{name}` must be referenced as `{qualified_name}`."),
				);
			}
		}

		if let Some((containing_name, nested_name)) = name.rsplit_once('.')
			&& self.lookup_object_type(containing_name).is_some() {
			return self.compile_error(
				position,
				format!("Object `{containing_name}` does not contain a named inline object type `{nested_name}`."),
			);
		}

		self.compile_error(
			position,
			format!("Object type `{name}` is not declared in this module."),
		)
	}

	fn unselectable_overload_indices(&self, signatures: &[FunctionSignature]) -> Vec<usize> {
		let max_variadic_arguments = signatures.iter()
			.map(|signature| signature.parameters.len())
			.max()
			.unwrap_or(0) + 1;

		signatures.iter().enumerate()
			.filter_map(|(target_index, target)| {
				let has_unique_call = self.function_call_shapes(target, max_variadic_arguments)
					.iter()
					.any(|shape| {
						let matching_indices = signatures.iter().enumerate()
							.filter_map(|(index, signature)| {
								self.signature_accepts_call_shape(signature, shape).then_some(index)
							})
							.collect::<Vec<_>>();
						matching_indices.as_slice() == [target_index]
					});
				(!has_unique_call).then_some(target_index)
			})
			.collect()
	}

	fn validate_block(&mut self, statements: &[Statement]) -> Result<(), CompileError> {
		self.enter_scope();
		self.enums.enter_scope();
		self.functions.enter_scope();
		self.collect_scope_enum_declarations(statements)?;
		self.collect_scope_function_signatures(&[], statements)?;

		for statement in statements {
			self.validate_statement(statement)?;
		}

		self.functions.exit_scope();
		self.enums.exit_scope();
		self.exit_scope();
		Ok(())
	}

	fn validate_declared_data_type(
		&mut self,
		data_type: &DataType,
		position: usize,
		message: String,
	) -> Result<(), CompileError> {
		self.validate_declared_data_type_structure(data_type, position, &message)?;
		let mut references = Vec::new();
		self.resolve_declared_object_type_references(
			data_type,
			&mut Vec::new(),
			&mut references,
		)?;
		if !references.is_empty() {
			self.semantic_program.object_type_references.insert(position, references);
		}
		Ok(())
	}

	fn validate_declared_data_type_structure(
		&self,
		data_type: &DataType,
		position: usize,
		message: &str,
	) -> Result<(), CompileError> {
		match data_type {
			DataType::EmptyArray | DataType::Null => Err(self.compile_error(position, message)),
			DataType::Array(element_type) => {
				self.validate_declared_data_type_structure(element_type, position, message)
			}
			DataType::Nullable(inner) => {
				match inner.as_ref() {
					DataType::Any => Err(self.compile_error(
						position,
						String::from("The `any` type may not currently be marked as nullable."),
					)),
					DataType::Union(_) => Err(self.compile_error(
						position,
						String::from("Nullable union types are not yet supported."),
					)),
					_ => self.validate_declared_data_type_structure(inner, position, message),
				}
			}
			DataType::Union(members) => {
				for member in members {
					self.validate_declared_data_type_structure(member, position, message)?;
				}

				Ok(())
			}
			DataType::Object(_) => Ok(()),
			DataType::RecordPointer(_) => Err(self.compile_error(position, message)),
			_ => Ok(()),
		}
	}

	fn validate_default_construction_expression(
		&mut self,
		expression: &Expr,
		active: &mut Vec<(ObjectDefaultConstructionState, usize)>,
		path: &mut Vec<ObjectDefaultPathStep>,
	) -> Result<(), CompileError> {
		match expression {
			Expr::Array(array) => {
				for element in &array.elements {
					self.validate_default_construction_expression(element, active, path)?;
				}
			}
			Expr::ObjectConstruction(construction) => {
				let object_type_id = self.semantic_program.object_construction_type_id(construction.position)
					.expect("Validated object default construction is missing its resolved identity.");
				self.validate_explicit_object_default(construction, object_type_id, active, path)?;
			}
			_ => {}
		}

		Ok(())
	}

	fn validate_default_construction_field(
		&mut self,
		object_type: &ResolvedObjectType,
		field: &ObjectFieldDeclaration,
		value: Option<&Expr>,
		active: &mut Vec<(ObjectDefaultConstructionState, usize)>,
		path: &mut Vec<ObjectDefaultPathStep>,
	) -> Result<(), CompileError> {
		let default_value = value.or(field.default_value.as_ref());
		path.push(ObjectDefaultPathStep {
			label: format!("{}.{}", object_type.display_name(), field.name),
			position: default_value.map(Expr::position).unwrap_or(field.position),
		});

		let result = if let Some(default_value) = default_value {
			self.validate_default_construction_expression(default_value, active, path)
		}
		else {
			self.validate_implicit_data_type_default(&field.data_type, field.position, active, path)
		};

		path.pop();
		result
	}

	fn validate_default_construction_state(
		&self,
		state: ObjectDefaultConstructionState,
		active: &[(ObjectDefaultConstructionState, usize)],
		path: &[ObjectDefaultPathStep],
	) -> Result<(), CompileError> {
		let Some((_, path_start)) = active.iter().find(|(active_state, _)| *active_state == state) else {
			return Ok(());
		};
		let cycle = path[*path_start..].iter()
			.map(|step| format!("`{}`", step.label))
			.collect::<Vec<_>>()
			.join(" -> ");
		let position = path.last().map(|step| step.position).unwrap_or(0);

		Err(self.compile_error(
			position,
			format!("Object default construction cycle through {cycle} would never terminate."),
		))
	}

	fn validate_enum_declaration(&mut self, enum_declaration: &EnumDeclaration) -> Result<(), CompileError> {
		let binding = self.lookup_enum(&enum_declaration.name).ok_or(self.compile_error(
			enum_declaration.position,
			format!("Enum `{}` was not registered before validation.", enum_declaration.name),
		))?;

		if binding.backing_type.is_nullable() {
			return Err(self.compile_error(
				enum_declaration.position,
				String::from("Enum backing types must be non-nullable."),
			));
		}

		if !self.is_valid_enum_backing_type(&binding.backing_type) {
			return Err(self.compile_error(
				enum_declaration.position,
				format!(
					"Enum backing type `{}` is not supported. Enum backing types must be non-nullable primitive types other than `json`.",
					binding.backing_type.name(),
				),
			));
		}

		Ok(())
	}

	fn validate_explicit_object_default(
		&mut self,
		construction: &ObjectConstructionExpr,
		object_type_id: ObjectTypeId,
		active: &mut Vec<(ObjectDefaultConstructionState, usize)>,
		path: &mut Vec<ObjectDefaultPathStep>,
	) -> Result<(), CompileError> {
		let state = ObjectDefaultConstructionState::Explicit {
			object_type_id,
			position: construction.position,
		};
		self.validate_default_construction_state(state, active, path)?;

		let object_type = self.semantic_program.object_type(object_type_id)
			.cloned()
			.expect("Resolved object default construction is missing its declaration.");
		let Some(fields) = object_type.declaration().fields() else {
			return Ok(());
		};
		let fields = fields.to_vec();
		let saved_source_name = self.current_source_name.clone();
		self.current_source_name = object_type.source_name().map(String::from);
		active.push((state, path.len()));

		for field in &fields {
			let provided_value = construction.fields.iter()
				.find(|provided| provided.name == field.name)
				.map(|provided| &provided.value);
			if let Err(error) = self.validate_default_construction_field(
				&object_type,
				field,
				provided_value,
				active,
				path,
			) {
				return Err(error);
			}
		}

		active.pop();
		self.current_source_name = saved_source_name;
		Ok(())
	}

	fn validate_format_built_in(
		&self,
		arguments: &[&CallArgument],
		argument_types: &[DataType],
	) -> Result<(), CompileError> {
		let [value_type, _pattern_type] = argument_types else {
			return Ok(());
		};
		let Some(Expr::Text(pattern)) = arguments.get(1).and_then(|argument| argument.expression()) else {
			return Ok(());
		};

		let target = match value_type.without_nullability() {
			DataType::Int => NumericFormatTarget::Integer,
			DataType::Dec => NumericFormatTarget::Decimal,
			_ => {
				let target = match value_type.without_nullability() {
					DataType::Date => TemporalFormatTarget::Date,
					DataType::Time => TemporalFormatTarget::Time,
					DataType::Timestamp => TemporalFormatTarget::Timestamp,
					_ => return Ok(()),
				};

				TemporalFormatPattern::parse(&pattern.value, target).map_err(|error| self.compile_error(
					pattern.position + error.position,
					format!("Invalid temporal format string: {}", error.message),
				))?;
				return Ok(());
			}
		};

		NumericFormatPattern::parse(&pattern.value, target).map_err(|error| self.compile_error(
			pattern.position + error.position,
			format!("Invalid numeric format string: {}", error.message),
		))?;
		Ok(())
	}

	fn validate_function_declaration(&mut self, function: &FunctionDeclaration) -> Result<(), CompileError> {
		let saved_locals = std::mem::take(&mut self.locals);
		let saved_sequence_aliases = std::mem::take(&mut self.sequence_aliases);
		let saved_function_depth = self.function_depth;
		let saved_loop_depth = self.loop_depth;
		let saved_next_local_slot = self.next_local_slot;
		let saved_return_type = self.current_return_type.clone();
		let saved_source_name = self.current_source_name.clone();

		if function.visibility == Visibility::Public && self.function_depth > 0 {
			return Err(self.compile_error(
				function.position,
				format!("Function `{}` cannot be declared `pub` inside another function.", function.name),
			));
		}

		self.locals = ScopeStack::default();
		self.sequence_aliases = ScopeStack::default();
		self.function_depth += 1;
		self.loop_depth = 0;
		self.next_local_slot = 0;
		self.current_return_type = function.return_type.clone();
		self.enter_scope();

		for parameter in &function.parameters {
			self.validate_function_parameter_default(parameter)?;
		}

		for parameter in &function.parameters {
			self.validate_function_parameter(parameter)?;
		}

		self.validate_block(function.body.statements.as_slice())?;

		if let Some(return_type) = &function.return_type
			&& !self.block_guarantees_return(&function.body) {
			return Err(self.compile_error(
				function.position,
				format!(
					"Function `{}` must return a value of type `{}` on all paths.",
					function.name,
					return_type.name(),
				),
			));
		}

		self.exit_scope();
		self.locals = saved_locals;
		self.sequence_aliases = saved_sequence_aliases;
		self.function_depth = saved_function_depth;
		self.loop_depth = saved_loop_depth;
		self.next_local_slot = saved_next_local_slot;
		self.current_return_type = saved_return_type;
		self.current_source_name = saved_source_name;

		Ok(())
	}

	fn validate_function_parameter(&mut self, parameter: &FunctionParameter) -> Result<(), CompileError> {
		let (parameter_type, sequence) = self.resolve_function_parameter_type(parameter)?;

		if let Some(error) = self.local_declaration_error(
			"Parameter",
			&parameter.name,
			parameter.position,
			format!("Parameter `{}` is already declared in this scope.", parameter.name),
		) {
			return Err(error);
		}

		let slot = self.next_local_slot;
		self.next_local_slot += 1;
		self.semantic_program.declaration_slots.insert(parameter.position, slot);
		self.semantic_program.declaration_types.insert(parameter.position, parameter_type.clone());
		if let DataType::RecordPointer(record_pointer) = &parameter_type {
			self.register_record_pointer_binding(
				parameter.position,
				record_pointer.clone(),
				RecordPointerInitialization::Existing,
				true,
				RecordPointerOrigin::Parameter,
				None,
			);
		}
		self.declare_local(parameter.name.clone(), LocalBinding {
			declaration_position: parameter.position,
			data_type: parameter_type,
			is_const: false,
			slot,
		});
		if let Some(sequence) = sequence {
			self.sequence_aliases.declare(parameter.name.clone(), Some(sequence));
		}

		Ok(())
	}

	fn validate_function_parameter_default(&mut self, parameter: &FunctionParameter) -> Result<(), CompileError> {
		let Some(default_value) = &parameter.default_value else {
			return Ok(());
		};

		self.validate_function_parameter_default_expression(default_value)?;
		let (parameter_type, _) = self.resolve_function_parameter_type(parameter)?;
		let default_type = self.infer_expression_type(default_value)?;
		self.ensure_assignable(&parameter_type, &default_type, default_value.position())
	}

	fn validate_function_parameter_default_expression(&self, expression: &Expr) -> Result<(), CompileError> {
		match expression {
			Expr::Array(array) => {
				for element in &array.elements {
					self.validate_function_parameter_default_expression(element)?;
				}
			}
			Expr::Binary(binary) => {
				self.validate_function_parameter_default_expression(&binary.left)?;
				self.validate_function_parameter_default_expression(&binary.right)?;
			}
			Expr::Call(call) => {
				for argument in &call.arguments {
					if let Some(expression) = argument.expression() {
						self.validate_function_parameter_default_expression(expression)?;
					}
				}
			}
			Expr::FieldAccess(field_access) => {
				// A bare identifier here may be a type qualifier such as `Status.Active`.
				// Ordinary value identifiers remain unavailable during subsequent type resolution.
				if !matches!(&*field_access.object, Expr::Identifier(_)) {
					self.validate_function_parameter_default_expression(&field_access.object)?;
				}
			}
			Expr::Index(index) => {
				self.validate_function_parameter_default_expression(&index.array)?;
				self.validate_function_parameter_default_expression(&index.index)?;
			}
			Expr::ObjectConstruction(construction) => {
				for field in &construction.fields {
					self.validate_function_parameter_default_expression(&field.value)?;
				}
			}
			Expr::Ternary(ternary) => {
				self.validate_function_parameter_default_expression(&ternary.condition)?;
				self.validate_function_parameter_default_expression(&ternary.true_branch)?;
				self.validate_function_parameter_default_expression(&ternary.false_branch)?;
			}
			Expr::Unary(unary) => {
				self.validate_function_parameter_default_expression(&unary.operand)?;
			}
			Expr::Boolean(_)
			| Expr::Date(_)
			| Expr::Decimal(_)
			| Expr::Integer(_)
			| Expr::Null(_)
			| Expr::Text(_)
			| Expr::Time(_)
			| Expr::TimeTz(_)
			| Expr::Timestamp(_)
			| Expr::TimestampTz(_) => {}
			Expr::Identifier(_) => {
				return Err(self.compile_error(
					expression.position(),
					String::from("A default expression cannot directly reference a variable, constant, or parameter."),
				));
			}
			Expr::Assignment(_)
			| Expr::Count(_)
			| Expr::Find(_)
			| Expr::New(_)
			| Expr::Range(_) => {
				return Err(self.compile_error(
					expression.position(),
					String::from("This expression form is not permitted in a function parameter default."),
				));
			}
		}

		Ok(())
	}

	fn validate_function_overload_set(
		&self,
		name: &str,
		signatures: &[FunctionSignature],
		position: usize,
	) -> Result<(), CompileError> {
		let unselectable_indices = self.unselectable_overload_indices(signatures);
		if unselectable_indices.is_empty() {
			return Ok(());
		}

		let descriptions = unselectable_indices.iter()
			.map(|index| {
				format!(
					"`{}`",
					self.describe_function_signature(name, &signatures[*index]),
				)
			})
			.collect::<Vec<_>>()
			.join(", ");
		let noun = if unselectable_indices.len() == 1 {
			"overload"
		}
		else {
			"overloads"
		};
		Err(self.compile_error(
			position,
			format!(
				"Function overload set `{name}` is invalid because the following {noun} cannot be selected uniquely: {descriptions}.",
			),
		))
	}

	fn validate_implicit_data_type_default(
		&mut self,
		data_type: &DataType,
		type_position: usize,
		active: &mut Vec<(ObjectDefaultConstructionState, usize)>,
		path: &mut Vec<ObjectDefaultPathStep>,
	) -> Result<(), CompileError> {
		if !matches!(data_type, DataType::Object(_)) {
			return Ok(());
		}

		let Some(object_type_id) = self.semantic_program.object_type_id_for_reference(type_position, &[]) else {
			return Ok(());
		};

		self.validate_implicit_object_default(object_type_id, active, path)
	}

	fn validate_implicit_object_default(
		&mut self,
		object_type_id: ObjectTypeId,
		active: &mut Vec<(ObjectDefaultConstructionState, usize)>,
		path: &mut Vec<ObjectDefaultPathStep>,
	) -> Result<(), CompileError> {
		let state = ObjectDefaultConstructionState::Implicit(object_type_id);
		self.validate_default_construction_state(state, active, path)?;

		let object_type = self.semantic_program.object_type(object_type_id)
			.cloned()
			.expect("Resolved object default is missing its declaration.");
		let Some(fields) = object_type.declaration().fields() else {
			return Ok(());
		};
		let fields = fields.to_vec();
		let saved_source_name = self.current_source_name.clone();
		self.current_source_name = object_type.source_name().map(String::from);
		active.push((state, path.len()));

		for field in &fields {
			if let Err(error) = self.validate_default_construction_field(
				&object_type,
				field,
				None,
				active,
				path,
			) {
				return Err(error);
			}
		}

		active.pop();
		self.current_source_name = saved_source_name;
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
			Expr::Boolean(_)
			| Expr::Date(_)
			| Expr::Decimal(_)
			| Expr::Integer(_)
			| Expr::Null(_)
			| Expr::Text(_)
			| Expr::Time(_)
			| Expr::TimeTz(_)
			| Expr::Timestamp(_)
			| Expr::TimestampTz(_) => Ok(()),
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

	fn validate_main_entry_point(&self, program: &AstProgram, main_function: &FunctionDeclaration) -> Result<(), CompileError> {
		if let Some(statement) = program.statements.iter().find(|statement| !matches!(statement, Statement::EnumDeclaration(_) | Statement::Use(_))) {
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
			|| main_function.parameters[0].data_type != FunctionParameterType::Value(DataType::Array(Box::new(DataType::Text)))
			|| main_function.return_type != Some(DataType::Int)
		{
			return Err(self.compile_error(
				main_function.position,
				String::from("Entry-point function `Main` must have the exact signature `fn Main(args: [text]): int`."),
			));
		}

		Ok(())
	}

	fn validate_object_declaration(&mut self, object: &ObjectDeclaration) -> Result<(), CompileError> {
		let object_visibility = self.lookup_object_type(&object.name)
			.expect("Object declaration is missing its resolved semantic type.")
			.visibility();
		match &object.shape {
			crate::ast::ObjectDeclarationShape::Array(element_type) => {
				self.validate_declared_data_type(
					element_type,
					object.position,
					format!("Array-shaped object `{}` cannot have element type `{}`.", object.name, element_type.name()),
				)?;
				if object_visibility == Visibility::Public {
					self.validate_public_object_type_reference(
						element_type,
						object.position,
						format!("Public root-array object `{}`", object.name),
					)?;
				}
			}
			crate::ast::ObjectDeclarationShape::Fields(fields) => {
				let mut declared_fields = Vec::<&ObjectFieldDeclaration>::new();

				for field in fields {
					if field.visibility == Visibility::Public && object_visibility == Visibility::Private {
						return Err(self.compile_error(
							field.position,
							format!("Field `{}` cannot be public because object `{}` is private.", field.name, object.name),
						));
					}

					if declared_fields.iter().any(|existing| {
						object_field_declarations_conflict(existing, field)
					}) {
						return Err(self.compile_error(
							field.position,
							format!("Field `{}` is already declared on object `{}`.", field.name, object.name),
						));
					}
					declared_fields.push(field);

					self.validate_declared_data_type(
						&field.data_type,
						field.position,
						format!("Field `{}` cannot have type `{}`.", field.name, field.data_type.name()),
					)?;

					if field.visibility == Visibility::Public {
						self.validate_public_object_type_reference(
							&field.data_type,
							field.position,
							format!("Public field `{}` on object `{}`", field.name, object.name),
						)?;
					}

					if let Some(default_value) = &field.default_value {
						self.validate_literal_expression(default_value)?;
						let default_type = self.infer_expression_type(default_value)?;
						self.ensure_object_field_default_assignable(field, &default_type, default_value.position())?;
					}
				}
			}
		}

		Ok(())
	}

	fn validate_object_default_construction_cycles(&mut self) -> Result<(), CompileError> {
		let object_type_ids = self.semantic_program.object_types.keys().copied().collect::<Vec<_>>();

		for object_type_id in object_type_ids {
			self.validate_implicit_object_default(
				object_type_id,
				&mut Vec::new(),
				&mut Vec::new(),
			)?;
		}

		Ok(())
	}

	fn validate_object_type_graph(&mut self, objects: &[ObjectDeclaration]) -> Result<(), CompileError> {
		for (index, object) in objects.iter().enumerate() {
			self.current_source_name = self.top_level_object_source_names.get(index)
				.cloned()
				.or_else(|| self.root_source_name.clone());
			self.validate_object_declaration(object)?;
		}

		self.validate_object_default_construction_cycles()?;
		self.semantic_program.object_type_descriptors = self.semantic_program.object_types.iter()
			.map(|(id, object_type)| {
				(*id, ObjectTypeDescriptor {
					display_name: object_type.display_name().to_string(),
					id: *id,
					shape: match &object_type.declaration().shape {
						ObjectDeclarationShape::Fields(fields) => ObjectTypeDescriptorShape::Fields(
							fields.iter()
								.map(|field| ObjectFieldDescriptor {
									data_type: self.describe_object_value_type(&field.data_type),
									explicit_default: field.default_value.as_ref()
										.map(|value| self.describe_object_default_value(value)),
									is_quoted: field.is_quoted,
									name: field.name.clone(),
									visibility: field.visibility,
								})
								.collect(),
						),
						ObjectDeclarationShape::Array(element_type) => ObjectTypeDescriptorShape::RootArray(
							self.describe_object_value_type(element_type),
						),
					},
				})
			})
			.collect();
		Ok(())
	}

	fn validate_public_object_type_reference(
		&self,
		data_type: &DataType,
		position: usize,
		declaration_description: String,
	) -> Result<(), CompileError> {
		let mut referenced_names = BTreeSet::new();
		Self::collect_referenced_object_names(data_type, &mut referenced_names);
		for referenced_name in referenced_names {
			let Some(object_type) = self.lookup_object_type(&referenced_name) else {
				continue;
			};
			if object_type.visibility() == Visibility::Private {
				return Err(self.compile_error(
					position,
					format!("{declaration_description} cannot expose private object type `{referenced_name}`."),
				));
			}
		}

		Ok(())
	}

	fn validate_sequence_backend(
		&self,
		position: usize,
		resolved: &crate::schema::ResolvedSequence<'_>,
	) -> Result<(), CompileError> {
		if resolved.database().backend() == DatabaseBackend::MySql {
			return Err(self.compile_error(
				position,
				format!(
					"Sequence `{}` cannot be used because database `{}` uses MySQL, which does not support standalone sequences.",
					resolved.sequence().name(),
					resolved.database().name(),
				),
			));
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
			Statement::Create(CreateStatement { position, target }) => {
				let local = self.lookup_local(&target.name).ok_or(self.compile_error(
					target.position,
					format!("Variable `{}` is not declared in this scope.", target.name),
				))?;
				self.semantic_program.identifier_slots.insert(target.position, local.slot);

				let DataType::RecordPointer(_) = local.data_type.without_nullability() else {
					return Err(self.compile_error(
						target.position,
						format!("`create` requires a record pointer operand, found `{}`.", local.data_type.name()),
					));
				};

				if local.is_const {
					return Err(self.compile_error(
						target.position,
						format!("`create` requires a mutable record pointer, but `{}` is immutable.", target.name),
					));
				}

				let binding = self.semantic_program.record_pointer_binding(local.declaration_position).ok_or(self.compile_error(
					*position,
					String::from("Internal error: missing record pointer binding metadata for `create` statement."),
				))?;

				if binding.initialization != RecordPointerInitialization::New {
					return Err(self.compile_error(
						target.position,
						String::from("`create` requires a record pointer declared from a `new` expression."),
					));
				}

				Ok(())
			}
			Statement::Delete(DeleteStatement { target, .. }) => {
				let local = self.lookup_local(&target.name).ok_or(self.compile_error(
					target.position,
					format!("Variable `{}` is not declared in this scope.", target.name),
				))?;
				self.semantic_program.identifier_slots.insert(target.position, local.slot);

				let DataType::RecordPointer(_) = local.data_type.without_nullability() else {
					return Err(self.compile_error(
						target.position,
						format!("`delete` requires a record pointer operand, found `{}`.", local.data_type.name()),
					));
				};

				if local.is_const {
					return Err(self.compile_error(
						target.position,
						format!("`delete` requires a mutable record pointer, but `{}` is immutable.", target.name),
					));
				}

				Ok(())
			}
			Statement::EnumDeclaration(enum_declaration) => self.validate_enum_declaration(enum_declaration),
			Statement::Expression(expression) => {
				if let Expr::Call(call) = expression {
					self.infer_call_type(call)?;
				}
				else {
					self.infer_expression_type(expression)?;
				}
				Ok(())
			}
			Statement::FunctionDeclaration(function) => self.validate_function_declaration(function),
			Statement::For(ForStatement { body, iterable, position, variable }) => {
				let iterable_type = self.infer_expression_type(iterable)?;
				let iterable_type = iterable_type.without_nullability().clone();
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
							format!("`for` iterable must be an array or range, found `{}`.", other.name()),
						));
					}
				};

				self.enter_scope();

				if let Some(error) = self.local_declaration_error(
					"Variable",
					&variable.name,
					variable.position,
					format!("Variable `{}` is already declared in this scope.", variable.name),
				) {
					self.exit_scope();
					return Err(error);
				}

				let loop_variable_slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.declaration_slots.insert(variable.position, loop_variable_slot);
				self.semantic_program.declaration_types.insert(variable.position, variable_type.clone());
				self.declare_local(variable.name.clone(), LocalBinding {
					declaration_position: variable.position,
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
				group_by,
				is_mut,
				limit,
				order_by,
				position,
				table,
				variable,
				where_clause,
			}) => {
				let limit_parameter = if let Some(limit) = limit {
					let limit_type = self.infer_expression_type(limit)?;

					if limit_type != DataType::Int {
						return Err(self.compile_error(
							limit.position(),
							format!("`limit` clause must evaluate to `int`, found `{}`.", limit_type.name()),
						));
					}

					let slot = self.next_local_slot;
					self.next_local_slot += 1;
					self.semantic_program.for_record_limit_slots.insert(*position, slot);

					Some(QueryParameter {
						data_type: DataType::Int,
						field_path: Vec::new(),
						slot,
					})
				}
				else {
					None
				};

				let lowered_query = self.lower_for_record_query(&ForRecordStatement {
					body: body.clone(),
					group_by: group_by.clone(),
					is_mut: *is_mut,
					limit: limit.clone(),
					order_by: order_by.clone(),
					position: *position,
					table: table.clone(),
					variable: variable.clone(),
					where_clause: where_clause.clone(),
				}, limit_parameter)?;
				let record_pointer = {
					let resolved_table = self.resolve_table_reference(table)?;
					RecordPointerType {
						database_name: resolved_table.database().name().to_string(),
						schema_name: resolved_table.schema().name().to_string(),
						table_name: resolved_table.table().name().to_string(),
					}
				};

				if !group_by.is_empty() && !order_by.is_empty() {
					return Err(self.compile_error(
						*position,
						String::from("A query may not specify both `group by` and `order by`."),
					));
				}

				if let Some(where_clause) = where_clause {
					let where_type = self.infer_query_expression_type(where_clause, table)?;

					if where_type.without_nullability() != &DataType::Bool {
						return Err(self.compile_error(
							where_clause.position(),
							format!("`where` clause must evaluate to `bool`, found `{}`.", where_type.name()),
						));
					}
				}

				for group_by in group_by {
					self.infer_query_expression_type(&group_by.expression, table)?;
				}

				for order_by in order_by {
					self.infer_query_expression_type(&order_by.expression, table)?;
				}

				let compiled_query = lower_for_query(&lowered_query).map_err(|error| self.compile_error(
					*position,
					query_lowering_error_message(error),
				))?;
				let group_boundary_keys = lowered_query.group_by.iter()
					.map(|item| item.key_names.clone())
					.collect::<Vec<_>>();
				self.semantic_program.compiled_for_record_queries.insert(*position, compiled_query);
				self.semantic_program.lowered_for_record_queries.insert(*position, lowered_query);

				self.enter_scope();

				if let Some(error) = self.local_declaration_error(
					"Record pointer",
					&variable.name,
					variable.position,
					format!("Record pointer `{}` is already declared in this scope.", variable.name),
				) {
					self.exit_scope();
					return Err(error);
				}

				let loop_variable_slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.declaration_slots.insert(variable.position, loop_variable_slot);
				self.semantic_program.declaration_types.insert(
					variable.position,
					DataType::RecordPointer(record_pointer.clone()),
				);
				self.register_record_pointer_binding(
					variable.position,
					record_pointer.clone(),
					RecordPointerInitialization::Existing,
					*is_mut,
					RecordPointerOrigin::ForLoop,
					Some(*position),
				);
				self.declare_local(variable.name.clone(), LocalBinding {
					declaration_position: variable.position,
					data_type: DataType::RecordPointer(record_pointer),
					is_const: !*is_mut,
					slot: loop_variable_slot,
				});

				let iterator_slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.iterator_slots.insert(*position, iterator_slot);

				self.loop_depth += 1;
				if !group_boundary_keys.is_empty() {
					self.group_boundary_contexts.push(GroupBoundaryContext {
						group_keys: group_boundary_keys,
						record_slot: loop_variable_slot,
					});
				}
				let validation_result = self.validate_statement(&Statement::Block(body.clone()));
				if !group_by.is_empty() {
					self.group_boundary_contexts.pop();
				}
				self.loop_depth -= 1;
				self.exit_scope();
				validation_result
			}
			Statement::If(IfStatement { condition, else_branch, position, then_branch }) => {
				match condition {
					IfCondition::Expression(condition) => {
						let condition_type = self.infer_expression_type(condition)?;

						if !self.is_truthy_condition_type(&condition_type) {
							return Err(self.compile_error(
								condition.position().max(*position),
								format!("`if` condition must be of type `bool` or `record pointer`, found `{}`.", condition_type.name()),
							));
						}

						self.validate_statement(&Statement::Block(then_branch.clone()))?;
					}
					IfCondition::RecordPointerBinding(RecordPointerDeclaration { initial_value, name, position, .. }) => {
						let initial_type = self.infer_expression_type(initial_value)?;
						let DataType::RecordPointer(_) = initial_type.without_nullability() else {
							return Err(self.compile_error(
								initial_value.position(),
								format!(
									"`if rec` binding `{name}` must be initialized from a record pointer value, found `{}`.",
									initial_type.name(),
								),
							));
						};
						self.record_record_pointer_escape(initial_value);

						self.enter_scope();
						let slot = self.next_local_slot;
						self.next_local_slot += 1;
						self.semantic_program.declaration_slots.insert(*position, slot);
						self.semantic_program.declaration_types.insert(*position, initial_type.clone());
						let DataType::RecordPointer(record_pointer) = initial_type.without_nullability().clone() else {
							unreachable!("checked above");
						};
						self.register_record_pointer_binding(
							*position,
							record_pointer,
							RecordPointerInitialization::Existing,
							false,
							RecordPointerOrigin::IfBinding,
							match initial_value {
								Expr::Find(find) => Some(find.position),
								_ => None,
							},
						);
						self.declare_local(name.clone(), LocalBinding {
							declaration_position: *position,
							data_type: initial_type,
							is_const: true,
							slot,
						});
						let validation_result = self.validate_statement(&Statement::Block(then_branch.clone()));
						self.exit_scope();
						validation_result?;
					}
				}

				if let Some(else_branch) = else_branch {
					self.validate_statement(else_branch)?;
				}

				Ok(())
			}
			Statement::RecordPointerDeclaration(RecordPointerDeclaration { initial_value, is_mut, name, position }) => {
				if let Some(error) = self.local_declaration_error(
					"Record pointer",
					name,
					*position,
					format!("Record pointer `{name}` is already declared in this scope."),
				) {
					return Err(error);
				}

				let initial_type = self.infer_expression_type_with_find_lock_mode(
					initial_value,
					if *is_mut { RecordLockMode::UpdateNoWait } else { RecordLockMode::None },
				)?;
				let DataType::RecordPointer(_) = initial_type.without_nullability() else {
					return Err(self.compile_error(
						initial_value.position(),
						format!(
							"Record pointer `{name}` must be initialized from a record pointer value, found `{}`.",
							initial_type.name(),
						),
					));
				};
				self.record_record_pointer_escape(initial_value);

				let slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.declaration_slots.insert(*position, slot);
				self.semantic_program.declaration_types.insert(*position, initial_type.clone());
				let DataType::RecordPointer(record_pointer) = initial_type.without_nullability().clone() else {
					unreachable!("checked above");
				};
				self.register_record_pointer_binding(
					*position,
					record_pointer,
					if matches!(initial_value, Expr::New(_)) {
						RecordPointerInitialization::New
					}
					else {
						RecordPointerInitialization::Existing
					},
					*is_mut,
					RecordPointerOrigin::VariableDeclaration,
					match initial_value {
						Expr::Find(find) => Some(find.position),
						_ => None,
					},
				);
				self.declare_local(name.clone(), LocalBinding {
					declaration_position: *position,
					data_type: initial_type,
					is_const: !*is_mut,
					slot,
				});

				Ok(())
			}
			Statement::Return(ReturnStatement { position, value }) => {
				if self.function_depth == 0 {
					return Err(self.compile_error(
						*position,
						String::from("`return` may only be used inside a function body."),
					));
				}

				let return_type = self.current_return_type.clone();
				match (return_type, value) {
					(None, None) => Ok(()),
					(None, Some(value)) => Err(self.compile_error(
						value.position(),
						String::from("A function without a return type cannot return a value."),
					)),
					(Some(expected_type), Some(value)) => {
						let value_type = self.infer_expression_type(value)?;
						self.ensure_assignable(&expected_type, &value_type, value.position())
					}
					(Some(expected_type), None) => Err(self.compile_error(
						*position,
						format!("Function must return a value of type `{}`.", expected_type.name()),
					)),
				}
			}
			Statement::Transaction(TransactionStatement { body, .. }) => {
				self.validate_statement(&Statement::Block(body.clone()))
			}
			Statement::Update(UpdateStatement { target, .. }) => {
				let local = self.lookup_local(&target.name).ok_or(self.compile_error(
					target.position,
					format!("Variable `{}` is not declared in this scope.", target.name),
				))?;
				self.semantic_program.identifier_slots.insert(target.position, local.slot);

				let DataType::RecordPointer(_) = local.data_type.without_nullability() else {
					return Err(self.compile_error(
						target.position,
						format!("`update` requires a record pointer operand, found `{}`.", local.data_type.name()),
					));
				};

				if local.is_const {
					return Err(self.compile_error(
						target.position,
						format!("`update` requires a mutable record pointer, but `{}` is immutable.", target.name),
					));
				}

				Ok(())
			}
			Statement::Use(UseDeclaration { .. }) => Ok(()),
			Statement::VariableDeclaration(VariableDeclaration { data_type, initial_value, is_const, name, position }) => {
				self.validate_declared_data_type(
					data_type,
					*position,
					format!(
						"{} `{name}` cannot have type `{}`.",
						if *is_const { "Constant" } else { "Variable" },
						data_type.name(),
					),
				)?;

				if let Some(error) = self.local_declaration_error(
					"Variable",
					name,
					*position,
					format!("Variable `{name}` is already declared in this scope."),
				) {
					return Err(error);
				}

				if let Some(initial_value) = initial_value.as_ref() {
					let initial_type = self.infer_expression_type(initial_value)?;
					self.ensure_assignable(data_type, &initial_type, initial_value.position())?;
				}
				else if *is_const {
					return Err(self.compile_error(
						*position,
						format!("Constant `{name}` must currently have an initializer."),
					));
				}

				let slot = self.next_local_slot;
				self.next_local_slot += 1;
				self.semantic_program.declaration_slots.insert(*position, slot);
				self.semantic_program.declaration_types.insert(*position, data_type.clone());
				self.declare_local(name.clone(), LocalBinding {
					declaration_position: *position,
					data_type: data_type.clone(),
					is_const: *is_const,
					slot,
				});

				Ok(())
			}
			Statement::While(WhileStatement { body, condition, position }) => {
				let condition_type = self.infer_expression_type(condition)?;

				if condition_type.without_nullability() != &DataType::Bool {
					return Err(self.compile_error(
						condition.position().max(*position),
						format!("`while` condition must be of type `bool`, found `{}`.", condition_type.name()),
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

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SemanticProgram {
	active_databases: Vec<String>,
	built_in_call_targets: BTreeMap<usize, BuiltInFunction>,
	call_argument_bindings: BTreeMap<usize, Vec<CallArgumentBinding>>,
	call_argument_reference_slots: BTreeMap<usize, Vec<Option<u32>>>,
	call_return_types: BTreeMap<usize, DataType>,
	call_targets: BTreeMap<usize, u32>,
	compiled_count_queries: BTreeMap<usize, LoweredBackendQuery>,
	compiled_find_queries: BTreeMap<usize, LoweredBackendQuery>,
	compiled_for_record_queries: BTreeMap<usize, LoweredBackendQuery>,
	compiled_query_for_shapes: BTreeMap<usize, LoweredBackendQuery>,
	constant_boolean_expressions: BTreeMap<usize, bool>,
	declaration_slots: BTreeMap<usize, u32>,
	declaration_types: BTreeMap<usize, DataType>,
	entry_point_function_index: Option<u32>,
	entry_point_position: Option<usize>,
	enum_declarations: BTreeMap<String, EnumDeclaration>,
	enum_variant_values: BTreeMap<usize, EnumValue>,
	enum_variants: BTreeMap<String, BTreeMap<String, EnumValue>>,
	for_record_limit_slots: BTreeMap<usize, u32>,
	function_declaration_targets: BTreeMap<usize, u32>,
	group_boundary_calls: BTreeMap<usize, GroupBoundaryCallInfo>,
	identifier_slots: BTreeMap<usize, u32>,
	iterator_slots: BTreeMap<usize, u32>,
	lowered_count_queries: BTreeMap<usize, QueryCountPlan>,
	lowered_find_queries: BTreeMap<usize, QueryFindPlan>,
	lowered_for_record_queries: BTreeMap<usize, QueryForPlan>,
	new_record_layouts: BTreeMap<usize, NewRecordLayout>,
	object_construction_type_ids: BTreeMap<usize, ObjectTypeId>,
	object_type_descriptors: BTreeMap<ObjectTypeId, ObjectTypeDescriptor>,
	object_type_ids_by_name: BTreeMap<String, ObjectTypeId>,
	object_type_references: BTreeMap<usize, Vec<ResolvedObjectTypeReference>>,
	object_types: BTreeMap<ObjectTypeId, ResolvedObjectType>,
	query_plan: ProgramQueryPlan,
	query_for_shapes: BTreeMap<usize, QueryForShape>,
	query_projected_value_bindings: BTreeMap<usize, QueryProjectedValueBinding>,
	record_pointer_bindings: BTreeMap<usize, RecordPointerBindingInfo>,
	resolved_sequences: BTreeMap<usize, ResolvedSequenceReference>,
	resolved_tables: BTreeMap<usize, ResolvedTableReference>,
	sequence_call_targets: BTreeMap<usize, ResolvedSequenceReference>,
	warnings: Vec<SemanticWarning>,
}

impl SemanticProgram {
	pub fn active_databases(&self) -> &[String] {
		&self.active_databases
	}

	pub fn built_in_call_target(&self, position: usize) -> Option<BuiltInFunction> {
		self.built_in_call_targets.get(&position).copied()
	}

	pub fn call_argument_bindings(&self, position: usize) -> Option<&[CallArgumentBinding]> {
		self.call_argument_bindings.get(&position).map(Vec::as_slice)
	}

	pub fn call_argument_reference_slots(&self, position: usize) -> Option<&[Option<u32>]> {
		self.call_argument_reference_slots.get(&position).map(Vec::as_slice)
	}

	pub fn call_return_type(&self, position: usize) -> Option<DataType> {
		self.call_return_types.get(&position).cloned()
	}

	pub fn call_returns_value(&self, position: usize) -> bool {
		self.call_return_types.contains_key(&position)
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

	pub fn compiled_query_for_shape(&self, position: usize) -> Option<&LoweredBackendQuery> {
		self.compiled_query_for_shapes.get(&position)
	}

	pub fn constant_boolean_expression(&self, position: usize) -> Option<bool> {
		self.constant_boolean_expressions.get(&position).copied()
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

	pub fn enum_declaration(&self, name: &str) -> Option<&EnumDeclaration> {
		self.enum_declarations.get(name)
	}

	pub fn enum_variant(&self, enum_name: &str, variant_name: &str) -> Option<&EnumValue> {
		self.enum_variants.get(enum_name)?.get(variant_name)
	}

	pub fn enum_variant_value(&self, position: usize) -> Option<&EnumValue> {
		self.enum_variant_values.get(&position)
	}

	pub fn for_record_limit_slot(&self, position: usize) -> Option<u32> {
		self.for_record_limit_slots.get(&position).copied()
	}

	pub fn function_declaration_target(&self, position: usize) -> Option<u32> {
		self.function_declaration_targets.get(&position).copied()
	}

	pub fn group_boundary_call(&self, position: usize) -> Option<&GroupBoundaryCallInfo> {
		self.group_boundary_calls.get(&position)
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

	pub fn new_record_layout(&self, position: usize) -> Option<&NewRecordLayout> {
		self.new_record_layouts.get(&position)
	}

	pub fn object_construction_type_id(&self, position: usize) -> Option<ObjectTypeId> {
		self.object_construction_type_ids.get(&position).copied()
	}

	pub fn object_type(&self, id: ObjectTypeId) -> Option<&ResolvedObjectType> {
		self.object_types.get(&id)
	}

	pub fn object_type_by_name(&self, name: &str) -> Option<&ResolvedObjectType> {
		self.object_type(self.object_type_id(name)?)
	}

	pub fn object_type_default(&self, id: ObjectTypeId) -> Option<ObjectDefaultValue> {
		self.complete_object_default(id, &[])
	}

	pub fn object_type_descriptor(&self, id: ObjectTypeId) -> Option<&ObjectTypeDescriptor> {
		self.object_type_descriptors.get(&id)
	}

	pub fn object_type_descriptors(&self) -> impl Iterator<Item = &ObjectTypeDescriptor> {
		self.object_type_descriptors.values()
	}

	pub fn object_type_id(&self, name: &str) -> Option<ObjectTypeId> {
		self.object_type_ids_by_name.get(name).copied()
	}

	pub fn object_type_id_for_reference(
		&self,
		position: usize,
		path: &[ObjectTypeReferencePathComponent],
	) -> Option<ObjectTypeId> {
		self.object_type_references(position)?
			.iter()
			.find(|reference| reference.path == path)
			.map(|reference| reference.object_type_id)
	}

	pub fn object_type_references(&self, position: usize) -> Option<&[ResolvedObjectTypeReference]> {
		self.object_type_references.get(&position).map(Vec::as_slice)
	}

	pub fn query_for_shape(&self, position: usize) -> Option<&QueryForShape> {
		self.query_for_shapes.get(&position)
	}

	pub fn query_plan(&self) -> &ProgramQueryPlan {
		&self.query_plan
	}

	pub fn query_projected_value_binding(&self, position: usize) -> Option<&QueryProjectedValueBinding> {
		self.query_projected_value_bindings.get(&position)
	}

	pub fn record_pointer_binding(&self, position: usize) -> Option<&RecordPointerBindingInfo> {
		self.record_pointer_bindings.get(&position)
	}

	pub fn resolved_sequence(&self, position: usize) -> Option<&ResolvedSequenceReference> {
		self.resolved_sequences.get(&position)
	}

	pub fn resolved_table(&self, position: usize) -> Option<&ResolvedTableReference> {
		self.resolved_tables.get(&position)
	}

	pub fn sequence_call_target(&self, position: usize) -> Option<&ResolvedSequenceReference> {
		self.sequence_call_targets.get(&position)
	}

	pub fn warnings(&self) -> &[SemanticWarning] {
		&self.warnings
	}

	fn complete_explicit_object_default(&self, value: &ObjectDefaultValue) -> Option<ObjectDefaultValue> {
		match value {
			ObjectDefaultValue::Array(values) => Some(ObjectDefaultValue::Array(
				values.iter()
					.map(|value| self.complete_explicit_object_default(value))
					.collect::<Option<Vec<_>>>()?,
			)),
			ObjectDefaultValue::Object { fields, object_type_id } => {
				self.complete_object_default(*object_type_id, fields)
			}
			other => Some(other.clone()),
		}
	}

	fn complete_object_default(
		&self,
		id: ObjectTypeId,
		supplied_fields: &[(String, ObjectDefaultValue)],
	) -> Option<ObjectDefaultValue> {
		let descriptor = self.object_type_descriptor(id)?;

		match descriptor.shape() {
			ObjectTypeDescriptorShape::Fields(fields) => {
				let fields = fields.iter()
					.map(|field| {
						let value = supplied_fields.iter()
							.find(|(name, _)| name == field.name())
							.map(|(_, value)| self.complete_explicit_object_default(value))
							.or_else(|| field.explicit_default().map(|value| {
								self.complete_explicit_object_default(value)
							}))
							.unwrap_or_else(|| self.default_for_object_value_type(field.data_type()));

						Some((field.name().to_string(), value?))
					})
					.collect::<Option<Vec<_>>>()?;

				Some(ObjectDefaultValue::Object {
					fields,
					object_type_id: id,
				})
			}
			ObjectTypeDescriptorShape::RootArray(_) => Some(ObjectDefaultValue::Array(Vec::new())),
		}
	}

	fn default_for_object_value_type(&self, data_type: &ObjectValueTypeDescriptor) -> Option<ObjectDefaultValue> {
		match data_type {
			ObjectValueTypeDescriptor::Any | ObjectValueTypeDescriptor::Nullable(_) => {
				Some(ObjectDefaultValue::Null)
			}
			ObjectValueTypeDescriptor::Array(_) => Some(ObjectDefaultValue::Array(Vec::new())),
			ObjectValueTypeDescriptor::Bool => Some(ObjectDefaultValue::Boolean(false)),
			ObjectValueTypeDescriptor::Date => Some(ObjectDefaultValue::CurrentDate),
			ObjectValueTypeDescriptor::Dec => Some(ObjectDefaultValue::Decimal(
				crate::value::Decimal::from_integer(0),
			)),
			ObjectValueTypeDescriptor::Enum(name) => {
				let declaration = self.enum_declaration(name)?;
				let variant = declaration.variants.first()?;
				let EnumValue::Constant(backing_value) = self.enum_variant(name, &variant.name)?.clone();

				Some(ObjectDefaultValue::Enum {
					backing_value,
					enum_name: name.clone(),
					variant_name: variant.name.clone(),
				})
			}
			ObjectValueTypeDescriptor::Int => Some(ObjectDefaultValue::Integer(0)),
			ObjectValueTypeDescriptor::Object(id) => self.complete_object_default(*id, &[]),
			ObjectValueTypeDescriptor::Text => Some(ObjectDefaultValue::Text(String::new())),
			ObjectValueTypeDescriptor::Time => Some(ObjectDefaultValue::CurrentTime),
			ObjectValueTypeDescriptor::TimeTz => Some(ObjectDefaultValue::CurrentTimeTz),
			ObjectValueTypeDescriptor::Timestamp => Some(ObjectDefaultValue::CurrentTimestamp),
			ObjectValueTypeDescriptor::TimestampTz => Some(ObjectDefaultValue::CurrentTimestampTz),
			ObjectValueTypeDescriptor::Range(_) | ObjectValueTypeDescriptor::Union(_) => None,
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SemanticWarning {
	pub message: String,
	pub position: usize,
	pub source_name: Option<String>,
}

impl SemanticWarning {
	pub fn format_with_source_name(&self, source: &str, source_name: Option<&str>) -> String {
		let warning_source_name = self.source_name.as_deref().or(source_name);

		if warning_source_name != source_name
			&& let Some(warning_source_name) = warning_source_name
			&& let Ok(warning_source) = std::fs::read_to_string(warning_source_name) {
			return SourceText::new(warning_source).format_diagnostic_with_source_name(
				"Compile warning",
				self.position,
				&self.message,
				Some(warning_source_name),
			);
		}

		SourceText::new(source).format_diagnostic_with_source_name(
			"Compile warning",
			self.position,
			&self.message,
			warning_source_name,
		)
	}
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionOverloadAlias {
	pub alias_name: String,
	pub display_name: String,
	pub position: usize,
	pub source_name: String,
	pub target_names: Vec<String>,
}

#[derive(Clone)]
struct EnumBinding {
	backing_type: DataType,
	variants: BTreeMap<String, EnumValue>,
}

#[derive(Clone)]
struct FunctionCallShape {
	arguments: Vec<CallArgument>,
	argument_sequences: Vec<Option<ResolvedSequenceReference>>,
	argument_types: Vec<Option<DataType>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct FunctionParameterSignature {
	data_type: DataType,
	has_default: bool,
	is_by_ref: bool,
	is_variadic: bool,
	name: String,
	sequence: Option<ResolvedSequenceReference>,
}

#[derive(Clone)]
struct FunctionSignature {
	function_index: u32,
	parameter_defaults: Vec<Option<Expr>>,
	parameters: Vec<FunctionParameterSignature>,
	return_type: Option<DataType>,
}

#[derive(Clone)]
struct GroupBoundaryContext {
	group_keys: Vec<Vec<String>>,
	record_slot: u32,
}

#[derive(Clone)]
struct LocalBinding {
	declaration_position: usize,
	data_type: DataType,
	is_const: bool,
	slot: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ObjectDefaultPathStep {
	label: String,
	position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct RecordPointerFieldAssignment {
	field_path: String,
	position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct RecordPointerFieldRead {
	field_name: String,
	position: usize,
}

fn object_field_declarations_conflict(
	left: &ObjectFieldDeclaration,
	right: &ObjectFieldDeclaration,
) -> bool {
	if left.is_quoted && right.is_quoted {
		left.name == right.name
	}
	else {
		left.name.eq_ignore_ascii_case(&right.name)
	}
}

fn query_binary_operator_name(operator: QueryBinaryOperator) -> &'static str {
	match operator {
		QueryBinaryOperator::Add => "+",
		QueryBinaryOperator::And => "and",
		QueryBinaryOperator::Concatenate => "+",
		QueryBinaryOperator::Divide => "/",
		QueryBinaryOperator::Equal => "=",
		QueryBinaryOperator::GreaterThan => ">",
		QueryBinaryOperator::GreaterThanOrEqual => ">=",
		QueryBinaryOperator::IntegerDivide => "/",
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
		QueryLoweringError::UnsupportedBuiltIn { backend, built_in } => {
			format!(
				"Function `{}` is not supported in `{}` database query expressions.",
				built_in.name(),
				backend.name(),
			)
		}
		QueryLoweringError::UnsupportedBackend { backend } => {
			format!(
				"Database query execution is not implemented yet for the `{}` backend.",
				backend.name(),
			)
		}
		QueryLoweringError::UnsupportedExpression { backend, description } => {
			format!(
				"Query expression `{description}` is not supported by the `{}` backend.",
				backend.name(),
			)
		}
		QueryLoweringError::UnsupportedOperator { backend, operator } => {
			format!(
				"Database query operator `{}` is not implemented yet for the `{}` backend.",
				query_binary_operator_name(operator),
				backend.name(),
			)
		}
	}
}

fn simple_group_by_expression_name(expression: &Expr) -> Option<String> {
	match expression {
		Expr::Identifier(identifier) => Some(identifier.name.clone()),
		Expr::FieldAccess(field_access) => {
			if matches!(field_access.object.as_ref(), Expr::Identifier(_)) {
				Some(field_access.field.name.clone())
			}
			else {
				None
			}
		}
		_ => None,
	}
}

fn statement_position(statement: &Statement) -> usize {
	match statement {
		Statement::Block(block) => block.position,
		Statement::Break(statement) => statement.position,
		Statement::Continue(statement) => statement.position,
		Statement::Create(statement) => statement.position,
		Statement::Delete(statement) => statement.position,
		Statement::EnumDeclaration(statement) => statement.position,
		Statement::Expression(expression) => expression.position(),
		Statement::For(statement) => statement.position,
		Statement::ForRecord(statement) => statement.position,
		Statement::FunctionDeclaration(statement) => statement.position,
		Statement::If(statement) => statement.position,
		Statement::RecordPointerDeclaration(statement) => statement.position,
		Statement::Return(statement) => statement.position,
		Statement::Transaction(statement) => statement.position,
		Statement::Update(statement) => statement.position,
		Statement::Use(statement) => statement.position,
		Statement::VariableDeclaration(statement) => statement.position,
		Statement::While(statement) => statement.position,
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	use crate::schema_fixture::*;
	use crate::source::*;
	use crate::syntax::lexer::*;
	use crate::syntax::parser::*;

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

	fn parse_program(source: &str) -> crate::ast::AstProgram {
		let mut lexer = Lexer::new(SourceText::new(source));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		parser.parse_program().unwrap()
	}

	fn sqlite_test_schema(source: &str, database_name: &str) -> crate::schema::SchemaCatalog {
		let mut schema = read_schema_catalog_from_str(source).unwrap();
		schema.database_mut(database_name).unwrap().set_backend(DatabaseBackend::Sqlite);
		schema
	}

	#[test]
	fn accepts_create_statement_for_new_mutable_record_pointer() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { rec mut cust = new Customers; create cust; return 0; }"
		);
		let mut analyzer = SemanticAnalyzer::new();

		analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
	}

	#[test]
	fn accepts_fallthrough_and_bare_return_in_function_without_return_type() {
		let program = parse_program(
			"fn Log(stop: bool) { if stop { return; } }\n\
			fn Main(args: [text]): int { Log(false); return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		analyzer.analyze_standalone_program(&program).unwrap();
	}

	#[test]
	fn accepts_no_return_calls_as_expression_statements() {
		let program = parse_program(
			"fn Log() {}\n\
			fn Main(args: [text]): int { Log(); displn('x'); return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		analyzer.analyze_standalone_program(&program).unwrap();
	}

	#[test]
	fn accepts_record_pointer_as_if_condition() {
		let statement = Statement::If(IfStatement {
			condition: IfCondition::Expression(Expr::Identifier(IdentifierExpr {
				name: String::from("cust"),
				position: 3,
			})),
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
				declaration_position: 3,
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
	fn allows_compatible_literal_object_field_defaults() {
		let program = parse_program(
			"obj Child { value: int = 1, };\n\
			obj Config {\n\
				quantity: int = 2,\n\
				label: text? = null,\n\
				values: [int] = [1, 2],\n\
				child: Child = Child { value: 3 },\n\
				choice: int | text = 'ready',\n\
			};\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		analyzer.analyze_standalone_program(&program).unwrap();
	}

	#[test]
	fn allows_public_function_to_expose_public_named_inline_object_type() {
		let program = parse_program(
			"pub obj Envelope {\n\
				pub payload: obj Payload { pub value: int, },\n\
			};\n\
			pub fn Read(value: Envelope.Payload): Envelope.Payload { return value; }\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		analyzer.analyze_standalone_program(&program).unwrap();
	}

	#[test]
	fn allows_quoted_object_fields_that_differ_only_by_case() {
		let program = parse_program(
			"obj Model { \"Value\": int, \"value\": text, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		analyzer.analyze_standalone_program(&program).unwrap();
	}

	#[test]
	fn allows_recursive_object_defaults_broken_by_nullable_and_array_fields() {
		let program = parse_program(
			"obj LinkedNode { next: LinkedNode?, };\n\
			obj TreeNode { children: [TreeNode], };\n\
			obj First { other: Second?, };\n\
			obj Second { origin: First, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		analyzer.analyze_standalone_program(&program).unwrap();
	}

	#[test]
	fn allows_terminating_explicit_recursive_object_field_default() {
		let program = parse_program(
			"obj LinkedNode {\n\
				next: LinkedNode? = LinkedNode { next: null },\n\
			};\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		analyzer.analyze_standalone_program(&program).unwrap();
	}

	#[test]
	fn assigns_stable_object_type_ids_and_preserves_declaration_metadata() {
		let source = "obj Outer { child: obj Child { value: int, }, anonymous: { value: int, }, };\n\
			obj Collection [int];\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.set_root_source_name(Some(String::from("models.tablo")));

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let outer_id = semantic_program.object_type_id("Outer").unwrap();
		let child_id = semantic_program.object_type_id("Outer.Child").unwrap();
		let anonymous_id = semantic_program.object_type_id("Outer.anonymous").unwrap();
		let collection_id = semantic_program.object_type_id("Collection").unwrap();
		let outer = semantic_program.object_type(outer_id).unwrap();
		let child = semantic_program.object_type(child_id).unwrap();
		let anonymous = semantic_program.object_type(anonymous_id).unwrap();
		let collection = semantic_program.object_type(collection_id).unwrap();

		assert_ne!(outer_id, child_id);
		assert_ne!(child_id, anonymous_id);
		assert_ne!(outer_id, collection_id);
		assert_eq!(outer.display_name(), "Outer");
		assert_eq!(outer.source_name(), Some("models.tablo"));
		assert_eq!(outer.declaration().position, source.find("obj Outer").unwrap());
		assert_eq!(child.containing_object_id(), Some(outer_id));
		assert_eq!(anonymous.containing_object_id(), Some(outer_id));
		assert_eq!(anonymous.declaration().position, source.find("anonymous:").unwrap());
		assert_eq!(child.scope_id(), outer.scope_id());
		assert!(outer.has_named_constructor());
		assert!(child.has_named_constructor());
		assert!(!anonymous.has_named_constructor());
		assert!(!collection.has_named_constructor());
	}

	#[test]
	fn binds_object_construction_to_declared_type_identity() {
		let source = "obj Model { value: int, };\n\
			fn Main(args: [text]): int { var model: Model = Model {}; return model.value; }";
		let program = parse_program(source);
		let construction_position = source.find("Model {}").unwrap() + "Model ".len();
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let object_type_id = semantic_program.object_type_id("Model").unwrap();

		assert_eq!(
			semantic_program.object_construction_type_id(construction_position),
			Some(object_type_id),
		);
	}

	#[test]
	fn built_in_overload_sets_have_unique_call_shapes() {
		let analyzer = SemanticAnalyzer::new();

		for built_in in BuiltInFunction::all() {
			let signatures = analyzer.built_in_validation_signatures(*built_in);
			let unselectable = analyzer.unselectable_overload_indices(&signatures);

			assert!(
				unselectable.is_empty(),
				"Built-in function `{}` has unselectable overloads at indices {unselectable:?}.",
				built_in.name(),
			);
		}
	}

	#[test]
	fn compares_object_types_using_resolved_identity_instead_of_source_spelling() {
		let program = parse_program(
			"obj First { value: int, };\n\
			obj Second { value: int, };",
		);
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.declaration_kinds.enter_scope();
		analyzer.object_type_bindings.enter_scope();
		analyzer.collect_object_declarations(&program.objects).unwrap();
		let first_id = analyzer.lookup_object_type_id("First").unwrap();
		let second_id = analyzer.lookup_object_type_id("Second").unwrap();
		analyzer.declaration_kinds.declare(
			String::from("FirstAlias"),
			LexicalDeclarationKind::Object,
		);
		analyzer.declaration_kinds.declare(
			String::from("SecondAlias"),
			LexicalDeclarationKind::Object,
		);
		analyzer.object_type_bindings.declare(String::from("FirstAlias"), first_id);
		analyzer.object_type_bindings.declare(String::from("SecondAlias"), second_id);

		assert!(analyzer.is_assignable(
			&DataType::Object(String::from("First").into()),
			&DataType::Object(String::from("FirstAlias").into()),
		));
		assert!(!analyzer.is_assignable(
			&DataType::Object(String::from("First").into()),
			&DataType::Object(String::from("SecondAlias").into()),
		));
	}

	#[test]
	fn distinguishes_value_and_no_value_function_calls_in_semantic_metadata() {
		let program = parse_program(
			"fn Log() {}\n\
			fn Value(): int { return 1; }\n\
			fn Main(args: [text]): int { Log(); Value(); displn('x'); len('x'); return 0; }",
		);
		let main = program.functions.iter().find(|function| function.name == "Main").unwrap();
		let Statement::Expression(Expr::Call(log_call)) = &main.body.statements[0] else {
			panic!("Expected `Log` call.");
		};
		let Statement::Expression(Expr::Call(value_call)) = &main.body.statements[1] else {
			panic!("Expected `Value` call.");
		};
		let Statement::Expression(Expr::Call(displn_call)) = &main.body.statements[2] else {
			panic!("Expected `displn` call.");
		};
		let Statement::Expression(Expr::Call(len_call)) = &main.body.statements[3] else {
			panic!("Expected `len` call.");
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();

		assert!(!semantic_program.call_returns_value(log_call.position));
		assert!(semantic_program.call_returns_value(value_call.position));
		assert_eq!(semantic_program.call_return_type(log_call.position), None);
		assert_eq!(semantic_program.call_return_type(value_call.position), Some(DataType::Int));
		assert!(!semantic_program.call_returns_value(displn_call.position));
		assert!(semantic_program.call_returns_value(len_call.position));
		assert_eq!(semantic_program.call_return_type(displn_call.position), None);
		assert_eq!(semantic_program.call_return_type(len_call.position), Some(DataType::Int));
	}

	#[test]
	fn does_not_offer_nested_query_optimization_for_mutable_queries() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
				create table Orders (
					Id int not null,
					CustomerId int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			concat!(
				"with exampledb;\n",
				"fn Main(args: [text]): int {\n",
				"  for rec customer in Customers {\n",
				"    for rec mut customerOrder in Orders where CustomerId == customer.Id {}\n",
				"  }\n",
				"  return 0;\n",
				"}",
			),
		);
		let Statement::ForRecord(outer_loop) = &program.functions[0].body.statements[0] else {
			panic!("Expected outer record loop.");
		};
		let Statement::ForRecord(inner_loop) = &outer_loop.body.statements[0] else {
			panic!("Expected inner record loop.");
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let query_plan = semantic_program.query_plan();
		let outer_query = query_plan.queries().iter()
			.find(|query| query.position == outer_loop.position)
			.unwrap();
		let inner_query = query_plan.queries().iter()
			.find(|query| query.position == inner_loop.position)
			.unwrap();

		assert_eq!(outer_query.is_read_only, Some(true));
		assert_eq!(inner_query.is_read_only, Some(false));
		assert!(inner_query.optimization_opportunities.is_empty());
		assert_eq!(
			inner_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::SemanticEquivalenceNotProven),
		);
	}

	#[test]
	fn does_not_record_record_pointer_escapes_for_presence_operators_or_unreachable_calls() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null, Name text not null);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			concat!(
				"with exampledb;\n",
				"fn Visit(cust: rec Customers) {}\n",
				"fn Main(args: [text]): int {\n",
				"  rec cust = find first Customers;\n",
				"  var present: bool = exists cust;\n",
				"  return 0;\n",
				"  Visit(cust);\n",
				"}",
			),
		);
		let main_function = program.functions.iter().find(|function| function.name == "Main").unwrap();
		let declaration_position = match &main_function.body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();

		assert!(!semantic_program.record_pointer_binding(declaration_position).unwrap().escapes_analysis);
	}

	#[test]
	fn does_not_warn_when_locked_is_applied_to_mutable_record_pointer() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null primary key);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\n\
			fn Main(args: [text]): int {\n\
				rec mut cust = find first Customers;\n\
				var unavailable: bool = locked cust;\n\
				return 0;\n\
			}",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();

		assert!(semantic_program.warnings().is_empty());
	}

	#[test]
	fn immutable_record_pointers_do_not_require_identity_fields() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null primary key, Name text not null);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { rec cust = find first Customers; displn(cust.Name); return 0; }"
		);
		let declaration_position = match &program.functions[0].body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let binding = semantic_program.record_pointer_binding(declaration_position).unwrap();

		assert!(binding.identity_fields.is_empty());
		assert_eq!(binding.required_query_fields(), Some(BTreeSet::from([String::from("Name")])));
	}

	#[test]
	fn rejects_exists_operator_for_non_field_value() {
		let expression = parse_expression("exists value");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("value"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Int,
				is_const: false,
				slot: 2,
			},
		);

		let error = analyzer.infer_expression_type(&expression).unwrap_err();

		assert_eq!(
			error.message,
			"Unary `exists` requires a record pointer or field-access operand, found `int`.",
		);
	}

	#[test]
	fn infers_bool_for_null_comparison_with_nullable_any_value() {
		let expression = parse_expression("value == null");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("value"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Nullable(Box::new(DataType::Any)),
				is_const: false,
				slot: 1,
			},
		);

		let data_type = analyzer.infer_expression_type(&expression).unwrap();

		assert_eq!(data_type, DataType::Bool);
	}

	#[test]
	fn infers_bool_for_null_comparison_with_nullable_array_of_any() {
		let expression = parse_expression("value == null");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("value"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Nullable(Box::new(DataType::Array(Box::new(DataType::Any)))),
				is_const: false,
				slot: 1,
			},
		);

		let data_type = analyzer.infer_expression_type(&expression).unwrap();

		assert_eq!(data_type, DataType::Bool);
	}

	#[test]
	fn infers_exists_operator_type_for_record_pointer() {
		let expression = parse_expression("exists(cust)");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("cust"),
			LocalBinding {
				declaration_position: 0,
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
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			"ExampleDb",
		);
		let expression = parse_expression("cust.name");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("cust"),
			LocalBinding {
				declaration_position: 0,
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
	fn infers_nullable_ternary_expression_type_when_one_branch_is_null() {
		let expression = Expr::Ternary(TernaryExpr {
			condition: Box::new(Expr::Boolean(crate::ast::BooleanLiteral {
				position: 0,
				value: true,
			})),
			false_branch: Box::new(Expr::Null(crate::ast::NullLiteral { position: 0 })),
			position: 0,
			true_branch: Box::new(Expr::Text(crate::ast::TextLiteral {
				position: 0,
				value: String::from("value"),
			})),
		});
		let mut analyzer = SemanticAnalyzer::new();

		let data_type = analyzer.infer_expression_type(&expression).unwrap();

		assert_eq!(data_type, DataType::Nullable(Box::new(DataType::Text)));
	}

	#[test]
	fn infers_record_pointer_type_for_find_expression() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			"ExampleDb",
		);
		let find = parse_find_expression("find first customers where id == 1");
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
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			"ExampleDb",
		);
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
	fn infers_ternary_expression_type() {
		let expression = Expr::Ternary(TernaryExpr {
			condition: Box::new(Expr::Boolean(crate::ast::BooleanLiteral {
				position: 0,
				value: true,
			})),
			false_branch: Box::new(Expr::Integer(crate::ast::IntegerLiteral {
				position: 0,
				value: 2,
			})),
			position: 0,
			true_branch: Box::new(Expr::Decimal(crate::ast::DecimalLiteral {
				position: 0,
				value: crate::value::Decimal::from_literal("1.5").unwrap(),
			})),
		});
		let mut analyzer = SemanticAnalyzer::new();

		let data_type = analyzer.infer_expression_type(&expression).unwrap();

		assert_eq!(data_type, DataType::Dec);
	}

	#[test]
	fn inner_local_shadows_outer_function_for_call_resolution() {
		let program = parse_program(
			"fn Read(): int { return 1; }\n\
			fn Main(args: [text]): int {\n\
				{\n\
					var Read: int = 2;\n\
					Read();\n\
				}\n\
				return 0;\n\
			}",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Identifier `Read` is not callable because it refers to a variable in the nearest scope.",
		);
	}

	#[test]
	fn looks_up_runtime_object_descriptor_by_resolved_identity() {
		let program = parse_program(
			"obj Model { value: int, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let model_id = semantic_program.object_type_id("Model").unwrap();
		let descriptor = semantic_program.object_type_descriptor(model_id).unwrap();

		assert_eq!(descriptor.id(), model_id);
		assert_eq!(descriptor.display_name(), "Model");
		assert_eq!(
			semantic_program.object_type_descriptor(ObjectTypeId(u32::MAX)),
			None,
		);
	}

	#[test]
	fn lowers_count_query_to_backend_aware_ir() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Active bool not null
				);
			"#,
			"ExampleDb",
		);
		let count = parse_count_expression("count customers where id == targetId and active == true");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.semantic_program.active_databases = vec![String::from("exampledb")];
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("targetId"),
			LocalBinding {
				declaration_position: 7,
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
						field_path: Vec::new(),
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

	#[test]
	fn lowers_nested_object_field_access_in_where_clause_to_query_parameter() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"obj ConfigValues { FieldA: int, };\nobj Config { Values: ConfigValues, };"
		);
		let count = parse_count_expression("count Customers where Customers.Id == config.Values.FieldA");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.semantic_program.active_databases = vec![String::from("exampledb")];
		analyzer.collect_object_declarations(&program.objects).unwrap();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("config"),
			LocalBinding {
				declaration_position: 6,
				data_type: DataType::Object(String::from("Config").into()),
				is_const: false,
				slot: 6,
			},
		);

		let query = analyzer.lower_count_query(&count).unwrap();

		assert_eq!(query, QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Column(QueryColumnReference {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					table_name: String::from("Customers"),
				})),
				operator: QueryBinaryOperator::Equal,
				right: Box::new(QueryExpr::Parameter(QueryParameter {
					data_type: DataType::Int,
					field_path: vec![String::from("Values"), String::from("FieldA")],
					slot: 6,
				})),
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		});
	}

	#[test]
	fn lowers_object_field_access_in_where_clause_to_query_parameter() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program("obj Config { FieldA: int, };");
		let count = parse_count_expression("count Customers where Customers.Id == config.FieldA");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.semantic_program.active_databases = vec![String::from("exampledb")];
		analyzer.collect_object_declarations(&program.objects).unwrap();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("config"),
			LocalBinding {
				declaration_position: 5,
				data_type: DataType::Object(String::from("Config").into()),
				is_const: false,
				slot: 5,
			},
		);

		let query = analyzer.lower_count_query(&count).unwrap();

		assert_eq!(query, QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Column(QueryColumnReference {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					table_name: String::from("Customers"),
				})),
				operator: QueryBinaryOperator::Equal,
				right: Box::new(QueryExpr::Parameter(QueryParameter {
					data_type: DataType::Int,
					field_path: vec![String::from("FieldA")],
					slot: 5,
				})),
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		});
	}

	#[test]
	fn lowers_record_pointer_field_access_in_where_clause_to_query_parameter() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table OuterTable (
					Id int not null
				);
				create table InnerTable (
					Id int not null
				);
			"#,
			"ExampleDb",
		);
		let count = parse_count_expression("count InnerTable where InnerTable.Id == outer.Id");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.semantic_program.active_databases = vec![String::from("exampledb")];
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("outer"),
			LocalBinding {
				declaration_position: 4,
				data_type: DataType::RecordPointer(RecordPointerType {
					database_name: String::from("ExampleDb"),
					schema_name: String::from("Main"),
					table_name: String::from("OuterTable"),
				}),
				is_const: false,
				slot: 4,
			},
		);

		let query = analyzer.lower_count_query(&count).unwrap();

		assert_eq!(query, QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Column(QueryColumnReference {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					table_name: String::from("InnerTable"),
				})),
				operator: QueryBinaryOperator::Equal,
				right: Box::new(QueryExpr::Parameter(QueryParameter {
					data_type: DataType::Int,
					field_path: vec![String::from("Id")],
					slot: 4,
				})),
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("InnerTable"),
		});
	}

	#[test]
	fn lowers_text_addition_in_query_expression_as_concatenation() {
		let mut schema = read_schema_catalog_from_str(
			r#"
				database ExampleDb;
				schema Public implicit;
				create table Customers (
					Name text not null
				);
			"#,
		).unwrap();
		schema.database_mut("ExampleDb").unwrap().set_backend(DatabaseBackend::PostgreSql);
		let count = parse_count_expression("count customers where name + ' Ltd.' == expectedName");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.semantic_program.active_databases = vec![String::from("exampledb")];
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("expectedName"),
			LocalBinding {
				declaration_position: 7,
				data_type: DataType::Text,
				is_const: false,
				slot: 7,
			},
		);

		let query = analyzer.lower_count_query(&count).unwrap();
		let QueryExpr::Binary(comparison) = query.filter.as_ref().unwrap() else {
			panic!("Expected comparison expression.");
		};
		let QueryExpr::Binary(concatenation) = comparison.left.as_ref() else {
			panic!("Expected concatenation expression.");
		};

		assert_eq!(query.backend, DatabaseBackend::PostgreSql);
		assert_eq!(comparison.operator, QueryBinaryOperator::Equal);
		assert_eq!(concatenation.operator, QueryBinaryOperator::Concatenate);
	}

	#[test]
	fn lowers_year_built_in_in_where_clause_to_query_built_in_call() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program("obj Config { When: date, };");
		let count = parse_count_expression("count Customers where Customers.Id == year(config.When)");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.current_schema_catalog = Some(schema);
		analyzer.semantic_program.active_databases = vec![String::from("exampledb")];
		analyzer.collect_object_declarations(&program.objects).unwrap();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("config"),
			LocalBinding {
				declaration_position: 5,
				data_type: DataType::Object(String::from("Config").into()),
				is_const: false,
				slot: 5,
			},
		);

		let query = analyzer.lower_count_query(&count).unwrap();

		assert_eq!(query, QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Column(QueryColumnReference {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					table_name: String::from("Customers"),
				})),
				operator: QueryBinaryOperator::Equal,
				right: Box::new(QueryExpr::BuiltInCall(QueryBuiltInCall {
					arguments: vec![
						QueryExpr::Parameter(QueryParameter {
							data_type: DataType::Date,
							field_path: vec![String::from("When")],
							slot: 5,
						}),
					],
					built_in: BuiltInFunction::Year,
				})),
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		});
	}

	#[test]
	fn makes_inline_object_publicly_nameable_through_public_field_path() {
		let source = "pub obj Outer { pub inner: obj Inner { pub value: int, }, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let inner = semantic_program.object_type_by_name("Outer.Inner").unwrap();

		assert_eq!(inner.visibility(), Visibility::Public);
		assert_eq!(
			inner.declaration().fields().unwrap()[0].visibility,
			Visibility::Public,
		);
	}

	#[test]
	fn mutable_record_loops_without_primary_keys_require_complete_rows() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table AuditLog (Category text not null, Message text not null);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { for rec mut entry in AuditLog { delete entry; } return 0; }"
		);
		let (query_position, variable_position) = match &program.functions[0].body.statements[0] {
			Statement::ForRecord(statement) => (statement.position, statement.variable.position),
			other => panic!("Expected record-pointer loop, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let binding = semantic_program.record_pointer_binding(variable_position).unwrap();

		assert!(binding.identity_fields.is_empty());
		assert_eq!(binding.required_query_fields(), None);
		assert_eq!(
			semantic_program.lowered_for_record_query(query_position).unwrap().record_layout.selection,
			QueryColumnSelection::All,
		);
	}

	#[test]
	fn preserves_composite_primary_key_fields_for_mutable_record_pointers() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table OrderLines (
					OrderId int not null primary key,
					LineId int not null primary key,
					Description text not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			concat!(
				"with exampledb;\n",
				"fn Main(args: [text]): int {\n",
				"  rec mut line = find first OrderLines;\n",
				"  line.Description = 'Updated';\n",
				"  update line;\n",
				"  return 0;\n",
				"}",
			),
		);
		let declaration_position = match &program.functions[0].body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let binding = semantic_program.record_pointer_binding(declaration_position).unwrap();
		let identity_fields = BTreeSet::from([String::from("LineId"), String::from("OrderId")]);
		let required_fields = BTreeSet::from([
			String::from("Description"),
			String::from("LineId"),
			String::from("OrderId"),
		]);

		assert_eq!(binding.identity_fields, identity_fields);
		assert_eq!(binding.required_query_fields(), Some(required_fields));
	}

	#[test]
	fn preserves_private_by_default_object_and_field_visibility() {
		let source = "pub obj PublicModel { pub exposed: int, hidden: int, };\n\
			obj PrivateModel { hidden: int, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let public_model = semantic_program.object_type_by_name("PublicModel").unwrap();
		let private_model = semantic_program.object_type_by_name("PrivateModel").unwrap();
		let fields = public_model.declaration().fields().unwrap();

		assert_eq!(public_model.visibility(), Visibility::Public);
		assert_eq!(private_model.visibility(), Visibility::Private);
		assert_eq!(fields[0].visibility, Visibility::Public);
		assert_eq!(fields[1].visibility, Visibility::Private);
	}

	#[test]
	fn proves_only_semantically_equivalent_correlated_count_merge() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Divisor int not null
				);
				create table Orders (
					CustomerId int not null,
					Amount int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			concat!(
				"with exampledb;\n",
				"fn Main(args: [text]): int {\n",
				"  for rec safeCustomer in Customers {\n",
				"    var safeCount: int = count Orders where CustomerId == safeCustomer.Id;\n",
				"  }\n",
				"  for rec earlyCustomer in Customers {\n",
				"    var earlyCount: int = count Orders where CustomerId == earlyCustomer.Id;\n",
				"    break;\n",
				"  }\n",
				"  for rec fallibleCustomer in Customers {\n",
				"    var fallibleCount: int = count Orders where CustomerId == fallibleCustomer.Id and Amount / fallibleCustomer.Divisor > 0;\n",
				"  }\n",
				"  return 0;\n",
				"}",
			),
		);
		let Statement::ForRecord(safe_loop) = &program.functions[0].body.statements[0] else {
			panic!("Expected safe parent query.");
		};
		let Statement::ForRecord(early_exit_loop) = &program.functions[0].body.statements[1] else {
			panic!("Expected early-exit parent query.");
		};
		let Statement::ForRecord(fallible_loop) = &program.functions[0].body.statements[2] else {
			panic!("Expected fallible parent query.");
		};
		let count_position = |statement: &ForRecordStatement| {
			let Statement::VariableDeclaration(VariableDeclaration {
				initial_value: Some(Expr::Count(count)),
				..
			}) = &statement.body.statements[0] else {
				panic!("Expected nested count query.");
			};
			count.position
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let query_plan = semantic_program.query_plan();
		let query_at_position = |position| query_plan.queries().iter()
			.find(|query| query.position == position)
			.unwrap();
		let safe_parent = query_at_position(safe_loop.position);
		let safe_count = query_at_position(count_position(safe_loop));
		let early_exit_parent = query_at_position(early_exit_loop.position);
		let early_exit_count = query_at_position(count_position(early_exit_loop));
		let fallible_count = query_at_position(count_position(fallible_loop));

		assert_eq!(
			safe_count.proven_optimization,
			Some(PlannedQueryOptimizationStrategy::MergeCorrelatedCountWith {
				query: safe_parent.id,
			}),
		);
		assert!(semantic_program.query_for_shape(safe_loop.position).is_some());
		assert!(semantic_program.compiled_query_for_shape(safe_loop.position).is_some());
		assert!(semantic_program.query_projected_value_binding(count_position(safe_loop)).is_some());
		assert_eq!(
			safe_count.execution,
			PlannedQueryExecution::MergeWith {
				query: safe_parent.id,
			},
		);
		assert!(early_exit_parent.body_may_exit_early);
		assert!(!early_exit_count.optimization_opportunities.is_empty());
		assert_eq!(early_exit_count.proven_optimization, None);
		assert!(semantic_program.query_for_shape(early_exit_loop.position).is_none());
		assert!(semantic_program.query_projected_value_binding(count_position(early_exit_loop)).is_none());
		assert_eq!(
			early_exit_count.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::SemanticEquivalenceNotProven),
		);
		assert_eq!(fallible_count.expressions_are_infallible, Some(false));
		assert!(!fallible_count.optimization_opportunities.is_empty());
		assert_eq!(fallible_count.proven_optimization, None);
		assert!(semantic_program.query_for_shape(fallible_loop.position).is_none());
		assert!(semantic_program.query_projected_value_binding(count_position(fallible_loop)).is_none());
		assert_eq!(
			fallible_count.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::SemanticEquivalenceNotProven),
		);
	}

	#[test]
	fn records_compound_record_pointer_assignment_as_read_and_write() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null, Name text not null);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { rec mut cust = find first Customers; cust.Name += ' Ltd.'; return 0; }"
		);
		let declaration_position = match &program.functions[0].body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let binding = semantic_program.record_pointer_binding(declaration_position).unwrap();

		assert_eq!(binding.assigned_fields, BTreeSet::from([String::from("Name")]));
		assert_eq!(binding.read_fields, BTreeSet::from([String::from("Name")]));
	}

	#[test]
	fn records_for_loop_record_pointer_field_reads() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null, Name text not null);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { for rec cust in Customers { displn(cust.Name); } return 0; }"
		);
		let variable_position = match &program.functions[0].body.statements[0] {
			Statement::ForRecord(statement) => statement.variable.position,
			other => panic!("Expected record-pointer loop, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let binding = semantic_program.record_pointer_binding(variable_position).unwrap();

		assert_eq!(binding.read_fields, BTreeSet::from([String::from("Name")]));
	}

	#[test]
	fn records_record_pointer_assignment_metadata_for_local_declaration() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { rec mut cust = find first Customers where Id == 1; cust.Name = 'Ada'; return 0; cust.Id = 2; }"
		);
		let declaration_position = match &program.functions[0].body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let binding = semantic_program.record_pointer_binding(declaration_position).unwrap();

		assert_eq!(binding.origin, RecordPointerOrigin::VariableDeclaration);
		assert!(binding.is_mutable);
		assert_eq!(binding.assigned_fields, BTreeSet::from([String::from("Name")]));
		assert!(binding.read_fields.is_empty());
	}

	#[test]
	fn records_record_pointer_escape_through_aliases() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null, Name text not null);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { rec cust = find first Customers; rec copy = cust; return 0; }"
		);
		let declaration_position = match &program.functions[0].body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();

		assert!(semantic_program.record_pointer_binding(declaration_position).unwrap().escapes_analysis);
	}

	#[test]
	fn records_record_pointer_escapes_through_user_function_calls() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null, Name text not null);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			concat!(
				"with exampledb;\n",
				"fn Visit(cust: rec Customers) {}\n",
				"fn Borrow(cust: &rec Customers) {}\n",
				"fn Main(args: [text]): int {\n",
				"  rec custValue = find first Customers;\n",
				"  rec mut custRef = find first Customers;\n",
				"  Visit(custValue);\n",
				"  Borrow(&custRef);\n",
				"  return 0;\n",
				"}",
			),
		);
		let main_function = program.functions.iter().find(|function| function.name == "Main").unwrap();
		let first_position = match &main_function.body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let second_position = match &main_function.body.statements[1] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let first_query_position = match &main_function.body.statements[0] {
			Statement::RecordPointerDeclaration(RecordPointerDeclaration { initial_value: Expr::Find(find), .. }) => find.position,
			other => panic!("Expected record pointer initialized by `find`, found {other:?}."),
		};
		let second_query_position = match &main_function.body.statements[1] {
			Statement::RecordPointerDeclaration(RecordPointerDeclaration { initial_value: Expr::Find(find), .. }) => find.position,
			other => panic!("Expected record pointer initialized by `find`, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();

		assert!(semantic_program.record_pointer_binding(first_position).unwrap().escapes_analysis);
		assert!(semantic_program.record_pointer_binding(second_position).unwrap().escapes_analysis);
		assert_eq!(
			semantic_program.lowered_find_query(first_query_position).unwrap().record_layout.selection,
			QueryColumnSelection::All,
		);
		assert_eq!(
			semantic_program.lowered_find_query(second_query_position).unwrap().record_layout.selection,
			QueryColumnSelection::All,
		);
	}

	#[test]
	fn records_record_pointer_field_read_used_as_query_parameter() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null);
				create table Orders (Id int not null, CustomerId int not null);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { rec cust = find first Customers; rec ord = find first Orders where CustomerId == cust.Id; return 0; }"
		);
		let declaration_position = match &program.functions[0].body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let binding = semantic_program.record_pointer_binding(declaration_position).unwrap();

		assert_eq!(binding.read_fields, BTreeSet::from([String::from("Id")]));
	}

	#[test]
	fn records_direct_record_pointer_field_reads() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { rec cust = find first Customers; displn(cust.name); return cust.Id; }"
		);
		let declaration_position = match &program.functions[0].body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let binding = semantic_program.record_pointer_binding(declaration_position).unwrap();

		assert_eq!(binding.read_fields, BTreeSet::from([String::from("Id"), String::from("Name")]));
	}

	#[test]
	fn records_meaningful_nullable_null_comparison_for_branch_refinement() {
		let expression = parse_expression("value != null");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("value"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Nullable(Box::new(DataType::Text)),
				is_const: false,
				slot: 1,
			},
		);

		assert_eq!(analyzer.infer_expression_type(&expression).unwrap(), DataType::Bool);
		assert!(analyzer.refined_expression_for_branch(&expression, true).is_some());
		assert!(analyzer.refined_expression_for_branch(&expression, false).is_none());
	}

	#[test]
	fn records_nested_query_structure_in_semantic_program() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
				create table Orders (
					Id int not null,
					CustomerId int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			concat!(
				"with exampledb;\n",
				"fn Main(args: [text]): int {\n",
				"  for rec customer in Customers {\n",
				"    rec firstOrder = find first Orders where CustomerId == customer.Id;\n",
				"    var orderCount: int = count Orders where CustomerId == customer.Id;\n",
				"    for rec customerOrder in Orders where CustomerId == customer.Id group by CustomerId {}\n",
				"  }\n",
				"  return 0;\n",
				"}",
			),
		);
		let Statement::ForRecord(outer_loop) = &program.functions[0].body.statements[0] else {
			panic!("Expected outer record loop.");
		};
		let Statement::RecordPointerDeclaration(RecordPointerDeclaration {
			initial_value: Expr::Find(find),
			..
		}) = &outer_loop.body.statements[0] else {
			panic!("Expected nested find query.");
		};
		let Statement::VariableDeclaration(VariableDeclaration {
			initial_value: Some(Expr::Count(count)),
			..
		}) = &outer_loop.body.statements[1] else {
			panic!("Expected nested count query.");
		};
		let Statement::ForRecord(inner_loop) = &outer_loop.body.statements[2] else {
			panic!("Expected inner record loop.");
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let query_plan = semantic_program.query_plan();
		let outer_query = query_plan.queries().iter()
			.find(|query| query.position == outer_loop.position)
			.unwrap();
		let find_query = query_plan.queries().iter()
			.find(|query| query.position == find.position)
			.unwrap();
		let count_query = query_plan.queries().iter()
			.find(|query| query.position == count.position)
			.unwrap();
		let inner_query = query_plan.queries().iter()
			.find(|query| query.position == inner_loop.position)
			.unwrap();
		let expected_general_opportunity = vec![
			PlannedQueryOptimizationOpportunity::MergeOrBatchWith {
				query: outer_query.id,
			},
		];
		let expected_count_opportunity = vec![
			PlannedQueryOptimizationOpportunity::MergeCorrelatedCountWith {
				query: outer_query.id,
			},
		];

		assert_eq!(query_plan.queries().len(), 4);
		assert_eq!(outer_query.enclosing_query, None);
		assert_eq!(find_query.enclosing_query, Some(outer_query.id));
		assert_eq!(count_query.enclosing_query, Some(outer_query.id));
		assert_eq!(inner_query.enclosing_query, Some(outer_query.id));
		assert_eq!(outer_query.database_name.as_deref(), Some("ExampleDb"));
		assert_eq!(outer_query.is_read_only, Some(true));
		assert_eq!(find_query.result_semantics, Some(PlannedQueryResultSemantics {
			cardinality: PlannedQueryResultCardinality::AtMostOne,
			error_timing: PlannedQueryErrorTiming::AtQueryStart,
			has_grouping: false,
			has_limit: true,
			has_ordering: false,
		}));
		assert_eq!(count_query.result_semantics, Some(PlannedQueryResultSemantics {
			cardinality: PlannedQueryResultCardinality::Scalar,
			error_timing: PlannedQueryErrorTiming::AtQueryStart,
			has_grouping: false,
			has_limit: false,
			has_ordering: false,
		}));
		assert_eq!(inner_query.result_semantics, Some(PlannedQueryResultSemantics {
			cardinality: PlannedQueryResultCardinality::Many,
			error_timing: PlannedQueryErrorTiming::AtQueryStart,
			has_grouping: true,
			has_limit: false,
			has_ordering: true,
		}));
		assert_eq!(find_query.optimization_opportunities, expected_general_opportunity);
		assert_eq!(count_query.optimization_opportunities, expected_count_opportunity);
		assert_eq!(inner_query.optimization_opportunities, expected_general_opportunity);
		assert_eq!(
			count_query.proven_optimization,
			Some(PlannedQueryOptimizationStrategy::MergeCorrelatedCountWith {
				query: outer_query.id,
			}),
		);
		assert_eq!(find_query.proven_optimization, None);
		assert_eq!(inner_query.proven_optimization, None);
		let query_shape = semantic_program.query_for_shape(outer_loop.position).unwrap();
		let projection = &query_shape.scalar_projections[0];
		let QueryScalarProjectionExpression::CorrelatedCount(projected_count) = &projection.expression;
		let projected_binding = semantic_program.query_projected_value_binding(count.position).unwrap();

		assert_eq!(&query_shape.query, semantic_program.lowered_for_record_query(outer_loop.position).unwrap());
		assert_eq!(query_shape.scalar_projections.len(), 1);
		assert_eq!(&projected_count.query, semantic_program.lowered_count_query(count.position).unwrap());
		assert_eq!(projected_count.correlations, vec![QueryCorrelation {
			outer_field_path: vec![String::from("Id")],
			parameter: QueryParameter {
				data_type: DataType::Int,
				field_path: vec![String::from("Id")],
				slot: outer_query.record_slot.unwrap(),
			},
		}]);
		assert_eq!(projected_binding.value_id, projection.value_id);
		assert_eq!(projected_binding.enclosing_record_slot, outer_query.record_slot.unwrap());
		assert_eq!(
			outer_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::NoOptimizationOpportunity),
		);
		assert_eq!(
			find_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::NoSupportedStrategy),
		);
		assert_eq!(
			count_query.execution,
			PlannedQueryExecution::MergeWith {
				query: outer_query.id,
			},
		);
		assert_eq!(
			inner_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::NoSupportedStrategy),
		);
	}

	#[test]
	fn records_non_nullable_null_comparison_as_constant_without_refinement() {
		let expression = parse_expression("value != null");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("value"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Text,
				is_const: false,
				slot: 1,
			},
		);

		assert_eq!(analyzer.infer_expression_type(&expression).unwrap(), DataType::Bool);
		assert_eq!(
			analyzer.semantic_program.constant_boolean_expression(expression.position()),
			Some(true),
		);
		assert!(analyzer.refined_expression_for_branch(&expression, true).is_none());
		assert!(analyzer.refined_expression_for_branch(&expression, false).is_none());
	}

	#[test]
	fn records_query_parameter_dependencies_at_query_start() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
				create table Orders (
					Id int not null,
					CustomerId int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			concat!(
				"with exampledb;\n",
				"fn Main(args: [text]): int {\n",
				"  var minimumId: int = 1;\n",
				"  var customerCount: int = count Customers where Id >= minimumId and Id != minimumId;\n",
				"  rec firstCustomer = find first Customers where Id >= minimumId;\n",
				"  for rec customer in Customers where Id >= minimumId {\n",
				"    for rec customerOrder in Orders where CustomerId == customer.Id and Id >= minimumId {}\n",
				"  }\n",
				"  return 0;\n",
				"}",
			),
		);
		let Statement::VariableDeclaration(minimum_id) = &program.functions[0].body.statements[0] else {
			panic!("Expected minimum ID declaration.");
		};
		let Statement::VariableDeclaration(VariableDeclaration {
			initial_value: Some(Expr::Count(count)),
			..
		}) = &program.functions[0].body.statements[1] else {
			panic!("Expected count query.");
		};
		let Statement::RecordPointerDeclaration(RecordPointerDeclaration {
			initial_value: Expr::Find(find),
			..
		}) = &program.functions[0].body.statements[2] else {
			panic!("Expected find query.");
		};
		let Statement::ForRecord(outer_loop) = &program.functions[0].body.statements[3] else {
			panic!("Expected outer record loop.");
		};
		let Statement::ForRecord(inner_loop) = &outer_loop.body.statements[0] else {
			panic!("Expected inner record loop.");
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let minimum_id_slot = semantic_program.declaration_slot(minimum_id.position).unwrap();
		let customer_slot = semantic_program.declaration_slot(outer_loop.variable.position).unwrap();
		let query_plan = semantic_program.query_plan();
		let query_at_position = |position| query_plan.queries().iter()
			.find(|query| query.position == position)
			.unwrap();
		let minimum_id_parameter = PlannedQueryParameter {
			data_type: DataType::Int,
			evaluation: PlannedQueryParameterEvaluation::AtQueryStart,
			field_path: Vec::new(),
			slot: minimum_id_slot,
			source: PlannedQueryParameterSource::Local,
		};
		let customer_id_parameter = PlannedQueryParameter {
			data_type: DataType::Int,
			evaluation: PlannedQueryParameterEvaluation::AtQueryStart,
			field_path: vec![String::from("Id")],
			slot: customer_slot,
			source: PlannedQueryParameterSource::EnclosingQuery(
				query_at_position(outer_loop.position).id,
			),
		};

		assert_eq!(query_at_position(count.position).captured_parameters, vec![minimum_id_parameter.clone()]);
		assert_eq!(query_at_position(find.position).captured_parameters, vec![minimum_id_parameter.clone()]);
		assert_eq!(query_at_position(outer_loop.position).captured_parameters, vec![minimum_id_parameter.clone()]);

		let inner_parameters = &query_at_position(inner_loop.position).captured_parameters;
		assert_eq!(inner_parameters.len(), 2);
		assert!(inner_parameters.contains(&customer_id_parameter));
		assert!(inner_parameters.contains(&minimum_id_parameter));
		let inner_query = query_at_position(inner_loop.position);
		assert!(inner_query.optimization_opportunities.is_empty());
		assert_eq!(
			inner_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::SemanticEquivalenceNotProven),
		);
	}

	#[test]
	fn records_record_pointer_parameter_metadata() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Visit(cust: rec Customers, other: &rec Customers) {}"
		);
		let first_parameter = &program.functions[0].parameters[0];
		let second_parameter = &program.functions[0].parameters[1];
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_program_with_schema(&program, Some(&schema)).unwrap();
		let first_binding = semantic_program.record_pointer_binding(first_parameter.position).unwrap();
		let second_binding = semantic_program.record_pointer_binding(second_parameter.position).unwrap();

		assert_eq!(first_binding.origin, RecordPointerOrigin::Parameter);
		assert_eq!(second_binding.origin, RecordPointerOrigin::Parameter);
		assert!(first_binding.is_mutable);
		assert!(second_binding.is_mutable);
		assert!(first_binding.assigned_fields.is_empty());
		assert!(second_binding.assigned_fields.is_empty());
	}

	#[test]
	fn refines_nullable_identifier_across_nested_ternary_expression() {
		let expression = parse_expression("val1 != '' ? val1 : val2 != null ? val2 : ''");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("val1"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Text,
				is_const: false,
				slot: 1,
			},
		);
		analyzer.declare_local(
			String::from("val2"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Nullable(Box::new(DataType::Text)),
				is_const: false,
				slot: 2,
			},
		);

		let data_type = analyzer.infer_expression_type(&expression).unwrap();

		assert_eq!(data_type, DataType::Text);
	}

	#[test]
	fn refines_nullable_identifier_to_non_null_in_ternary_true_branch() {
		let expression = parse_expression("value != null ? value : ''");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("value"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Nullable(Box::new(DataType::Text)),
				is_const: false,
				slot: 1,
			},
		);

		let data_type = analyzer.infer_expression_type(&expression).unwrap();

		assert_eq!(data_type, DataType::Text);
	}

	#[test]
	fn refines_nullable_object_field_access_to_non_null_in_ternary_true_branch() {
		let expression = parse_expression("config.TestDate != null ? config.TestDate : today");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.collect_object_declarations(&[
			crate::ast::ObjectDeclaration {
				containing_object_name: None,
				has_explicit_name: true,
				name: String::from("Config"),
				position: 0,
				shape: crate::ast::ObjectDeclarationShape::Fields(vec![
					crate::ast::ObjectFieldDeclaration {
						data_type: DataType::Nullable(Box::new(DataType::Date)),
						default_value: None,
						is_quoted: false,
						name: String::from("TestDate"),
						position: 0,
						visibility: Visibility::Private,
					},
				]),
				visibility: Visibility::Private,
			},
		]).unwrap();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("config"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Object(String::from("Config").into()),
				is_const: false,
				slot: 1,
			},
		);
		analyzer.declare_local(
			String::from("today"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Date,
				is_const: false,
				slot: 2,
			},
		);

		let data_type = analyzer.infer_expression_type(&expression).unwrap();

		assert_eq!(data_type, DataType::Date);
	}

	#[test]
	fn rejects_bare_return_from_value_returning_function() {
		let program = parse_program(
			"fn Value(): int { return; }\n\
			fn Main(args: [text]): int { return Value(); }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(error.message, "Function must return a value of type `int`.");
	}

	#[test]
	fn rejects_built_in_without_return_type_in_value_context() {
		let program = parse_program(
			"fn Main(args: [text]): int { var result: int = displn('x'); return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(error.message, "Built-in function `displn` does not return a value.");
	}

	#[test]
	fn rejects_case_insensitive_duplicate_unquoted_object_fields() {
		let source = "obj Model { Value: int, value: text, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Field `value` is already declared on object `Model`."),
				position: source.find("value: text").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_create_statement_for_non_new_record_pointer() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\nfn Main(args: [text]): int { rec mut cust = find first Customers where Id == 1; create cust; return 0; }"
		);
		let mut analyzer = SemanticAnalyzer::new();
		let error = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap_err();

		assert_eq!(error.message, "`create` requires a record pointer declared from a `new` expression.");
	}

	#[test]
	fn rejects_direct_implicit_object_default_cycle() {
		let source = "obj InvalidNode { next: InvalidNode, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Object default construction cycle through `InvalidNode.next` would never terminate.",
				),
				position: source.find("next:").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_duplicate_module_object_declaration() {
		let source = "obj Model { alpha: int, };\n\
			obj Model { beta: int, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object `Model` is already declared in this scope."),
				position: source.rfind("obj Model").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_duplicate_named_inline_object_declaration() {
		let source = "obj Envelope {\n\
				alpha: obj Payload { value: int, },\n\
				beta: obj Payload { value: text, },\n\
			};\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object `Envelope.Payload` is already declared in this scope."),
				position: source.rfind("obj Payload").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_equality_comparison_between_incompatible_concrete_types() {
		let expression = parse_expression("true == 1");
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.infer_expression_type(&expression).unwrap_err();

		assert_eq!(
			error.message,
			"Equality comparison is not supported between `bool` and `int`.",
		);
	}

	#[test]
	fn rejects_exact_duplicate_quoted_object_fields() {
		let source = "obj Model { \"Value\": int, \"Value\": text, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Field `Value` is already declared on object `Model`."),
				position: source.rfind("\"Value\"").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_fallthrough_from_value_returning_function() {
		let program = parse_program(
			"fn Value(flag: bool): int { if flag { return 1; } }\n\
			fn Main(args: [text]): int { return Value(true); }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(error.message, "Function `Value` must return a value of type `int` on all paths.");
	}

	#[test]
	fn rejects_incompatible_object_field_default() {
		let source = "obj Config { quantity: int = 'many', };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Default value for field `quantity` has type `text`, which is not assignable to `int`.",
				),
				position: source.find("'many'").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_incompatible_object_typed_field_default() {
		let source = "obj Expected { value: int, };\n\
			obj Actual { value: int, };\n\
			obj Config { child: Expected = Actual {}, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Default value for field `child` has type `Actual`, which is not assignable to `Expected`.",
				),
				position: source.find("Actual {}").unwrap() + "Actual ".len(),
			},
		);
	}

	#[test]
	fn rejects_indirect_implicit_object_default_cycle() {
		let source = "obj First { other: Second, };\n\
			obj Second { origin: First, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Object default construction cycle through `First.other` -> `Second.origin` would never terminate.",
				),
				position: source.find("origin: First").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_local_that_conflicts_with_function_in_same_scope() {
		let program = parse_program(
			"fn Main(args: [text]): int {\n\
				fn Read(): int { return 1; }\n\
				var Read: int = 2;\n\
				return Read;\n\
			}",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Variable `Read` conflicts with a function declared in the same scope.",
		);
	}

	#[test]
	fn rejects_locked_operator_for_non_record_pointer() {
		let expression = parse_expression("locked value");
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.enter_scope();
		analyzer.declare_local(
			String::from("value"),
			LocalBinding {
				declaration_position: 0,
				data_type: DataType::Int,
				is_const: false,
				slot: 2,
			},
		);

		let error = analyzer.infer_expression_type(&expression).unwrap_err();

		assert_eq!(error.message, "Unary `locked` requires a record pointer operand, found `int`.");
	}

	#[test]
	fn rejects_module_enum_that_conflicts_with_object_type() {
		let program = parse_program(
			"obj Shared { value: int, };\n\
			enum Shared: int { Value: 1 }\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Enum `Shared` conflicts with an object type declared in the same scope.",
		);
	}

	#[test]
	fn rejects_module_function_that_conflicts_with_object_type() {
		let program = parse_program(
			"obj Shared { value: int, };\n\
			fn Shared(): int { return 1; }\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Function `Shared` conflicts with an object type declared in the same scope.",
		);
	}

	#[test]
	fn rejects_named_default_binding_for_variadic_parameter() {
		let Expr::Call(call) = parse_expression("Collect(values: default)") else {
			panic!("Expected call expression.");
		};
		let signature = FunctionSignature {
			function_index: 0,
			parameter_defaults: vec![None],
			parameters: vec![FunctionParameterSignature {
				data_type: DataType::Array(Box::new(DataType::Int)),
				has_default: false,
				is_by_ref: false,
				is_variadic: true,
				name: String::from("values"),
				sequence: None,
			}],
			return_type: None,
		};
		let analyzer = SemanticAnalyzer::new();

		assert!(analyzer.bind_call_arguments(&signature, &call.arguments).is_err());
		let error = analyzer.requested_default_error(&[signature], &call.arguments).unwrap();
		assert_eq!(error.message, "`default` cannot be used for variadic parameter `values`.");
		assert_eq!(error.position, 16);
	}

	#[test]
	fn rejects_nonliteral_expression_in_array_object_field_default() {
		let source = "obj Config { values: [int] = [1, 1 + 1], };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object field defaults must be literals."),
				position: source.find("1 + 1").unwrap() + 2,
			},
		);
	}

	#[test]
	fn rejects_nonliteral_expression_in_object_typed_field_default() {
		let source = "obj Child { value: int, };\n\
			obj Config { child: Child = Child { value: 1 + 1 }, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object field defaults must be literals."),
				position: source.find("1 + 1").unwrap() + 2,
			},
		);
	}

	#[test]
	fn rejects_nonliteral_object_field_default() {
		let source = "obj Config { quantity: int = 1 + 1, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object field defaults must be literals."),
				position: source.find("1 + 1").unwrap() + 2,
			},
		);
	}

	#[test]
	fn rejects_null_comparison_for_range_value() {
		let expression = parse_expression("(1:2) == null");
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.infer_expression_type(&expression).unwrap_err();

		assert_eq!(
			error.message,
			"Equality comparison is not supported between `range<int>` and `range<int>`.",
		);
	}

	#[test]
	fn rejects_null_default_for_non_nullable_object_field() {
		let source = "obj Config { quantity: int = null, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Default value for field `quantity` has type `null`, which is not assignable to `int`.",
				),
				position: source.find("null").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_public_field_behind_private_inline_field_path() {
		let program = parse_program(
			"pub obj Outer { inner: obj Inner { pub value: int, }, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Field `value` cannot be public because object `Outer.Inner` is private.",
		);
	}

	#[test]
	fn rejects_public_field_exposing_inline_type_from_private_object_path() {
		let program = parse_program(
			"obj PrivateOuter { child: obj Child { value: int, }, };\n\
			pub obj PublicModel { pub child: PrivateOuter.Child, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Public field `child` on object `PublicModel` cannot expose private object type `PrivateOuter.Child`.",
		);
	}

	#[test]
	fn rejects_public_field_exposing_private_root_object_type() {
		let program = parse_program(
			"obj PrivateModel { value: int, };\n\
			pub obj PublicModel { pub model: PrivateModel, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Public field `model` on object `PublicModel` cannot expose private object type `PrivateModel`.",
		);
	}

	#[test]
	fn rejects_public_field_exposing_wrapped_private_object_type() {
		let program = parse_program(
			"obj PrivateModel { value: int, };\n\
			pub obj PublicModel { pub models: [text | PrivateModel?], };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Public field `models` on object `PublicModel` cannot expose private object type `PrivateModel`.",
		);
	}

	#[test]
	fn rejects_public_field_on_private_object() {
		let program = parse_program(
			"obj PrivateModel { pub exposed: int, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Field `exposed` cannot be public because object `PrivateModel` is private.",
		);
	}

	#[test]
	fn rejects_public_function_parameter_exposing_private_named_inline_object_type() {
		let source = "obj PrivateOuter { child: obj Child { value: int, }, };\n\
			pub fn Read(value: PrivateOuter.Child) {}\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Public function `Read` parameter `value` cannot expose private object type `PrivateOuter.Child`.",
				),
				position: source.find("value: PrivateOuter.Child").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_public_function_parameter_exposing_private_object_type() {
		let source = "obj PrivateModel { value: int, };\n\
			pub fn Read(value: [PrivateModel?]) {}\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Public function `Read` parameter `value` cannot expose private object type `PrivateModel`.",
				),
				position: source.find("value: [PrivateModel?]").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_public_function_return_exposing_private_object_type() {
		let source = "obj PrivateModel { value: int, };\n\
			pub fn Read(): text | PrivateModel { return ''; }\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Public function `Read` return type cannot expose private object type `PrivateModel`.",
				),
				position: source.find("fn Read").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_public_nested_function_declaration() {
		let program = parse_program(
			"fn Outer() { pub fn Inner() {} }"
		);
		let mut analyzer = SemanticAnalyzer::new();
		let error = analyzer.analyze_program(&program).unwrap_err();

		assert_eq!(error.message, "Function `Inner` cannot be declared `pub` inside another function.");
	}

	#[test]
	fn rejects_public_root_array_exposing_private_root_object_type() {
		let program = parse_program(
			"pub obj PublicCollection [PrivateModel];\n\
			obj PrivateModel { value: int, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Public root-array object `PublicCollection` cannot expose private object type `PrivateModel`.",
		);
	}

	#[test]
	fn rejects_public_root_array_exposing_wrapped_private_root_object_type() {
		let program = parse_program(
			"pub obj PublicCollection [text | PrivateModel?];\n\
			obj PrivateModel { value: int, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Public root-array object `PublicCollection` cannot expose private object type `PrivateModel`.",
		);
	}

	#[test]
	fn rejects_query_optimization_when_execution_context_cannot_be_preserved() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
				create table Orders (
					Id int not null,
					CustomerId int not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			concat!(
				"with exampledb;\n",
				"fn Main(args: [text]): int {\n",
				"  var marker: int = 0;\n",
				"  for rec conditionalParent in Customers {\n",
				"    if conditionalParent.Id > 0 {\n",
				"      for rec conditionalChild in Orders where CustomerId == conditionalParent.Id {}\n",
				"    }\n",
				"  }\n",
				"  for rec transactionParent in Customers {\n",
				"    transaction {\n",
				"      for rec transactionChild in Orders where CustomerId == transactionParent.Id {}\n",
				"    }\n",
				"  }\n",
				"  for rec mutationParent in Customers {\n",
				"    marker = mutationParent.Id;\n",
				"    for rec mutationChild in Orders where CustomerId == mutationParent.Id {}\n",
				"  }\n",
				"  for rec repeatedParent in Customers {\n",
				"    for iteration in 1:2 {\n",
				"      for rec repeatedChild in Orders where CustomerId == repeatedParent.Id {}\n",
				"    }\n",
				"  }\n",
				"  return 0;\n",
				"}",
			),
		);
		let Statement::ForRecord(conditional_parent) = &program.functions[0].body.statements[1] else {
			panic!("Expected conditional parent query.");
		};
		let Statement::If(conditional) = &conditional_parent.body.statements[0] else {
			panic!("Expected conditional statement.");
		};
		let Statement::ForRecord(conditional_child) = &conditional.then_branch.statements[0] else {
			panic!("Expected conditional child query.");
		};
		let Statement::ForRecord(transaction_parent) = &program.functions[0].body.statements[2] else {
			panic!("Expected transaction parent query.");
		};
		let Statement::Transaction(transaction) = &transaction_parent.body.statements[0] else {
			panic!("Expected transaction statement.");
		};
		let Statement::ForRecord(transaction_child) = &transaction.body.statements[0] else {
			panic!("Expected transaction child query.");
		};
		let Statement::ForRecord(mutation_parent) = &program.functions[0].body.statements[3] else {
			panic!("Expected mutation parent query.");
		};
		let Statement::ForRecord(mutation_child) = &mutation_parent.body.statements[1] else {
			panic!("Expected mutation child query.");
		};
		let Statement::ForRecord(repeated_parent) = &program.functions[0].body.statements[4] else {
			panic!("Expected repeated parent query.");
		};
		let Statement::For(repeated_loop) = &repeated_parent.body.statements[0] else {
			panic!("Expected repeated ordinary loop.");
		};
		let Statement::ForRecord(repeated_child) = &repeated_loop.body.statements[0] else {
			panic!("Expected repeated child query.");
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let query_plan = semantic_program.query_plan();
		let query_at_position = |position| query_plan.queries().iter()
			.find(|query| query.position == position)
			.unwrap();
		let conditional_child_query = query_at_position(conditional_child.position);
		let transaction_parent_query = query_at_position(transaction_parent.position);
		let transaction_child_query = query_at_position(transaction_child.position);
		let mutation_parent_query = query_at_position(mutation_parent.position);
		let mutation_child_query = query_at_position(mutation_child.position);
		let repeated_child_query = query_at_position(repeated_child.position);

		assert_eq!(conditional_child_query.control_flow, PlannedQueryControlFlow::Conditional);
		assert!(conditional_child_query.optimization_opportunities.is_empty());
		assert_eq!(
			conditional_child_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::SemanticEquivalenceNotProven),
		);
		assert_ne!(transaction_child_query.transaction_scopes, transaction_parent_query.transaction_scopes);
		assert!(transaction_child_query.optimization_opportunities.is_empty());
		assert_eq!(
			transaction_child_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::SemanticEquivalenceNotProven),
		);
		assert!(mutation_parent_query.body_may_have_side_effects);
		assert_eq!(mutation_child_query.control_flow, PlannedQueryControlFlow::Direct);
		assert!(mutation_child_query.optimization_opportunities.is_empty());
		assert_eq!(
			mutation_child_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::SemanticEquivalenceNotProven),
		);
		assert_eq!(repeated_child_query.control_flow, PlannedQueryControlFlow::Repeated);
		assert!(repeated_child_query.optimization_opportunities.is_empty());
		assert_eq!(
			repeated_child_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::SemanticEquivalenceNotProven),
		);
	}

	#[test]
	fn rejects_quoted_object_field_conflicting_with_unquoted_field() {
		let source = "obj Model { value: int, \"VALUE\": text, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Field `VALUE` is already declared on object `Model`."),
				position: source.find("\"VALUE\"").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_repeating_explicit_nullable_object_field_default() {
		let source = "obj InvalidNode {\n\
				next: InvalidNode? = InvalidNode {},\n\
			};\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Object default construction cycle through `InvalidNode.next` would never terminate.",
				),
				position: source.find("InvalidNode {}").unwrap() + "InvalidNode ".len(),
			},
		);
	}

	#[test]
	fn rejects_repeating_explicit_recursive_array_default() {
		let source = "obj InvalidTree {\n\
				children: [InvalidTree] = [InvalidTree {}],\n\
			};\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from(
					"Object default construction cycle through `InvalidTree.children` would never terminate.",
				),
				position: source.find("[InvalidTree {}]").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_unknown_named_inline_object_on_known_containing_path() {
		let program = parse_program(
			"obj Envelope { payload: Envelope.Missing, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Object `Envelope` does not contain a named inline object type `Missing`.",
		);
	}

	#[test]
	fn rejects_unknown_named_inline_object_on_nested_containing_path() {
		let source = "obj Envelope {\n\
				payload: obj Payload { value: int, },\n\
				missing: Envelope.Payload.Missing,\n\
			};\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object `Envelope.Payload` does not contain a named inline object type `Missing`."),
				position: source.find("Envelope.Payload.Missing").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_unknown_object_field_type() {
		let source = "obj Envelope { payload: Missing, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object type `Missing` is not declared in this module."),
				position: source.find("Missing").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_unknown_object_field_type_within_array() {
		let source = "obj Envelope { payloads: [Missing], };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object type `Missing` is not declared in this module."),
				position: source.find("Missing").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_unknown_object_field_type_within_union() {
		let source = "obj Envelope { payload: text | Missing, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object type `Missing` is not declared in this module."),
				position: source.find("Missing").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_unknown_qualified_object_field_type() {
		let source = "obj Envelope { payload: Missing.Payload, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Object type `Missing.Payload` is not declared in this module."),
				position: source.find("Missing.Payload").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_unqualified_named_inline_object_with_complete_path_suggestion() {
		let program = parse_program(
			"obj Envelope {\n\
				payload: obj Payload { value: int, },\n\
				previousPayload: Payload,\n\
			};\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error.message,
			"Named inline object type `Payload` must be referenced as `Envelope.Payload`.",
		);
	}

	#[test]
	fn rejects_unquoted_object_field_conflicting_with_quoted_field() {
		let source = "obj Model { \"Value\": int, value: text, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(
			error,
			CompileError {
				message: String::from("Field `value` is already declared on object `Model`."),
				position: source.find("value: text").unwrap(),
			},
		);
	}

	#[test]
	fn rejects_user_function_without_return_type_in_value_context() {
		let program = parse_program(
			"fn Log() {}\n\
			fn Main(args: [text]): int { var result: int = Log(); return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(error.message, "Function `Log` does not return a value.");
	}

	#[test]
	fn rejects_value_return_from_function_without_return_type() {
		let program = parse_program(
			"fn Log() { return 1; }\n\
			fn Main(args: [text]): int { Log(); return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let error = analyzer.analyze_standalone_program(&program).unwrap_err();

		assert_eq!(error.message, "A function without a return type cannot return a value.");
	}

	#[test]
	fn resolves_forward_object_type_reference_to_declared_identity() {
		let source = "obj Holder { value: Later, };\n\
			obj Later { value: int, };\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let later_id = semantic_program.object_type_id("Later").unwrap();

		assert_eq!(
			semantic_program.object_type_id_for_reference(source.find("value: Later").unwrap(), &[]),
			Some(later_id),
		);
	}

	#[test]
	fn resolves_named_inline_object_using_complete_containing_path() {
		let source = "obj Envelope {\n\
				payload: obj Payload { value: int, },\n\
				previousPayload: Envelope.Payload,\n\
			};\n\
			fn Main(args: [text]): int {\n\
				var payload: Envelope.Payload = Envelope.Payload { value: 1 };\n\
				return payload.value;\n\
			}";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let payload_id = semantic_program.object_type_id("Envelope.Payload").unwrap();
		let construction_position = source.find("Envelope.Payload {").unwrap() + "Envelope.Payload ".len();

		assert_eq!(
			semantic_program.object_type_references(source.find("previousPayload:").unwrap()),
			Some([ResolvedObjectTypeReference {
				object_type_id: payload_id,
				path: vec![],
			}].as_slice()),
		);
		assert_eq!(
			semantic_program.object_construction_type_id(construction_position),
			Some(payload_id),
		);
	}

	#[test]
	fn resolves_object_constructions_within_parameter_defaults() {
		let source = "obj Sample { value: int, };\n\
			fn Read(\n\
				value: Sample = Sample { value: 1 },\n\
				field: int = Sample { value: 2 }.value\n\
			): int { return value.value + field; }\n\
			fn Main(args: [text]): int { return Read(); }";
		let program = parse_program(source);
		let first_construction_position = source.find("Sample { value: 1 }").unwrap() + "Sample ".len();
		let second_construction_position = source.find("Sample { value: 2 }").unwrap() + "Sample ".len();
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let sample_id = semantic_program.object_type_id("Sample").unwrap();

		assert_eq!(
			semantic_program.object_construction_type_id(first_construction_position),
			Some(sample_id),
		);
		assert_eq!(
			semantic_program.object_construction_type_id(second_construction_position),
			Some(sample_id),
		);
	}

	#[test]
	fn resolves_object_references_within_declared_type_shapes() {
		let source = "obj Model { value: int, };\n\
			obj Envelope {\n\
				direct: Model,\n\
				optional: Model?,\n\
				items: [Model],\n\
				choice: text | Model,\n\
				payload: obj Payload { value: int, },\n\
				payloads: [text | Envelope.Payload],\n\
			};\n\
			fn Transform(value: Model, values: [Model]): Model {\n\
				var local: Model = value;\n\
				return local;\n\
			}\n\
			fn Main(args: [text]): int { return 0; }";
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let model_id = semantic_program.object_type_id("Model").unwrap();
		let expected = |path| vec![ResolvedObjectTypeReference {
			object_type_id: model_id,
			path,
		}];

		assert_eq!(
			semantic_program.object_type_references(source.find("direct:").unwrap()),
			Some(expected(vec![]).as_slice()),
		);
		assert_eq!(
			semantic_program.object_type_references(source.find("optional:").unwrap()),
			Some(expected(vec![ObjectTypeReferencePathComponent::NullableValue]).as_slice()),
		);
		assert_eq!(
			semantic_program.object_type_references(source.find("items:").unwrap()),
			Some(expected(vec![ObjectTypeReferencePathComponent::ArrayElement]).as_slice()),
		);
		assert_eq!(
			semantic_program.object_type_references(source.find("choice:").unwrap()),
			Some(expected(vec![ObjectTypeReferencePathComponent::UnionMember(1)]).as_slice()),
		);

		let payload_id = semantic_program.object_type_id("Envelope.Payload").unwrap();

		assert_eq!(
			semantic_program.object_type_references(source.find("payloads:").unwrap()),
			Some(vec![ResolvedObjectTypeReference {
				object_type_id: payload_id,
				path: vec![
					ObjectTypeReferencePathComponent::ArrayElement,
					ObjectTypeReferencePathComponent::UnionMember(1),
				],
			}].as_slice()),
		);

		let transform_position = source.find("fn Transform").unwrap();
		let value_parameter_position = transform_position
			+ source[transform_position..].find("value: Model").unwrap();
		let values_parameter_position = transform_position
			+ source[transform_position..].find("values: [Model]").unwrap();

		assert_eq!(
			semantic_program.object_type_references(value_parameter_position),
			Some(expected(vec![]).as_slice()),
		);
		assert_eq!(
			semantic_program.object_type_references(values_parameter_position),
			Some(expected(vec![ObjectTypeReferencePathComponent::ArrayElement]).as_slice()),
		);
		assert_eq!(
			semantic_program.object_type_references(source.find("var local: Model").unwrap()),
			Some(expected(vec![]).as_slice()),
		);
		assert_eq!(
			semantic_program.object_type_references(transform_position),
			Some(expected(vec![]).as_slice()),
		);
	}

	#[test]
	fn retains_reads_from_reachable_branches_and_excludes_dead_code() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Ignored text not null,
					Name text not null
				);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			concat!(
				"with exampledb;\n",
				"fn Main(args: [text]): int {\n",
				"  rec cust = find first Customers;\n",
				"  var useName: bool = true;\n",
				"  if useName { displn(cust.Name); } else { var id: int = cust.Id; }\n",
				"  return 0;\n",
				"  var ignored: text = cust.Ignored;\n",
				"}",
			),
		);
		let declaration_position = match &program.functions[0].body.statements[0] {
			Statement::RecordPointerDeclaration(statement) => statement.position,
			other => panic!("Expected record pointer declaration, found {other:?}."),
		};
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();
		let binding = semantic_program.record_pointer_binding(declaration_position).unwrap();

		assert_eq!(binding.read_fields, BTreeSet::from([String::from("Id"), String::from("Name")]));
	}

	#[test]
	fn runtime_object_descriptors_construct_complete_defaults_recursively() {
		let program = parse_program(
			"enum Mode { Ready }\n\
			obj Leaf { quantity: int = 7, label: text, };\n\
			obj Config {\n\
				leaf: Leaf,\n\
				partial: Leaf = Leaf { quantity: 9 },\n\
				optional: Leaf?,\n\
				items: [Leaf],\n\
				active: bool,\n\
				amount: dec,\n\
				mode: Mode,\n\
				created: date,\n\
			};\n\
			obj Configs [Config];\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let leaf_id = semantic_program.object_type_id("Leaf").unwrap();
		let config_id = semantic_program.object_type_id("Config").unwrap();
		let ObjectDefaultValue::Object { fields, object_type_id } = semantic_program
			.object_type_default(config_id)
			.unwrap()
		else {
			panic!("Expected a field-shaped object default.");
		};
		let fields = fields.into_iter().collect::<BTreeMap<_, _>>();

		assert_eq!(object_type_id, config_id);
		assert_eq!(
			fields["leaf"],
			ObjectDefaultValue::Object {
				fields: vec![
					(String::from("quantity"), ObjectDefaultValue::Integer(7)),
					(String::from("label"), ObjectDefaultValue::Text(String::new())),
				],
				object_type_id: leaf_id,
			},
		);
		assert_eq!(
			fields["partial"],
			ObjectDefaultValue::Object {
				fields: vec![
					(String::from("quantity"), ObjectDefaultValue::Integer(9)),
					(String::from("label"), ObjectDefaultValue::Text(String::new())),
				],
				object_type_id: leaf_id,
			},
		);
		assert_eq!(fields["optional"], ObjectDefaultValue::Null);
		assert_eq!(fields["items"], ObjectDefaultValue::Array(Vec::new()));
		assert_eq!(fields["active"], ObjectDefaultValue::Boolean(false));
		assert_eq!(
			fields["amount"],
			ObjectDefaultValue::Decimal(crate::value::Decimal::from_integer(0)),
		);
		assert_eq!(
			fields["mode"],
			ObjectDefaultValue::Enum {
				backing_value: Constant::Integer(1),
				enum_name: String::from("Mode"),
				variant_name: String::from("Ready"),
			},
		);
		assert_eq!(fields["created"], ObjectDefaultValue::CurrentDate);

		let configs_id = semantic_program.object_type_id("Configs").unwrap();
		assert_eq!(
			semantic_program.object_type_default(configs_id),
			Some(ObjectDefaultValue::Array(Vec::new())),
		);
	}

	#[test]
	fn runtime_object_descriptors_distinguish_field_and_root_array_shapes() {
		let program = parse_program(
			"obj Model { value: int, };\n\
			obj Models [Model];\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let model = semantic_program.object_type_descriptor(
			semantic_program.object_type_id("Model").unwrap(),
		).unwrap();
		let models = semantic_program.object_type_descriptor(
			semantic_program.object_type_id("Models").unwrap(),
		).unwrap();

		assert!(matches!(model.shape(), ObjectTypeDescriptorShape::Fields(_)));
		assert!(matches!(models.shape(), ObjectTypeDescriptorShape::RootArray(_)));
	}

	#[test]
	fn runtime_object_descriptors_keep_identical_nominal_types_distinct() {
		let program = parse_program(
			"obj Left { value: int, };\n\
			obj Right { value: int, };\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let left_id = semantic_program.object_type_id("Left").unwrap();
		let right_id = semantic_program.object_type_id("Right").unwrap();
		let left = semantic_program.object_type_descriptor(left_id).unwrap();
		let right = semantic_program.object_type_descriptor(right_id).unwrap();

		assert_ne!(left_id, right_id);
		assert_eq!(left.shape(), right.shape());
		assert_ne!(left, right);
		assert_eq!(
			semantic_program.object_type_default(left_id),
			Some(ObjectDefaultValue::Object {
				fields: vec![(String::from("value"), ObjectDefaultValue::Integer(0))],
				object_type_id: left_id,
			}),
		);
		assert_eq!(
			semantic_program.object_type_default(right_id),
			Some(ObjectDefaultValue::Object {
				fields: vec![(String::from("value"), ObjectDefaultValue::Integer(0))],
				object_type_id: right_id,
			}),
		);
	}

	#[test]
	fn runtime_object_descriptors_retain_field_types_defaults_and_nested_identities() {
		let program = parse_program(
			"obj Child { value: int = 1, };\n\
			obj Config {\n\
				name: text = 'default',\n\
				child: Child? = Child { value: 2 },\n\
				choices: [text | Child],\n\
			};\n\
			obj Children [Child];\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let child_id = semantic_program.object_type_id("Child").unwrap();
		let config = semantic_program.object_type_descriptor(
			semantic_program.object_type_id("Config").unwrap(),
		).unwrap();
		let ObjectTypeDescriptorShape::Fields(fields) = config.shape() else {
			panic!("Expected field-shaped object descriptor.");
		};

		assert_eq!(fields.iter().map(ObjectFieldDescriptor::name).collect::<Vec<_>>(), vec![
			"name",
			"child",
			"choices",
		]);
		assert_eq!(fields[0].data_type(), &ObjectValueTypeDescriptor::Text);
		assert_eq!(
			fields[0].explicit_default(),
			Some(&ObjectDefaultValue::Text(String::from("default"))),
		);
		assert_eq!(
			fields[1].data_type(),
			&ObjectValueTypeDescriptor::Nullable(Box::new(
				ObjectValueTypeDescriptor::Object(child_id),
			)),
		);
		assert_eq!(
			fields[1].explicit_default(),
			Some(&ObjectDefaultValue::Object {
				fields: vec![
					(String::from("value"), ObjectDefaultValue::Integer(2)),
				],
				object_type_id: child_id,
			}),
		);
		assert_eq!(
			fields[2].data_type(),
			&ObjectValueTypeDescriptor::Array(Box::new(
				ObjectValueTypeDescriptor::Union(vec![
					ObjectValueTypeDescriptor::Text,
					ObjectValueTypeDescriptor::Object(child_id),
				]),
			)),
		);

		let children = semantic_program.object_type_descriptor(
			semantic_program.object_type_id("Children").unwrap(),
		).unwrap();
		assert_eq!(
			children.shape(),
			&ObjectTypeDescriptorShape::RootArray(
				ObjectValueTypeDescriptor::Object(child_id),
			),
		);
	}

	#[test]
	fn runtime_object_descriptors_retain_public_and_private_fields() {
		let program = parse_program(
			"pub obj Model {\n\
				privateValue: int,\n\
				pub publicValue: int,\n\
			};\n\
			fn Main(args: [text]): int { return 0; }",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		let model = semantic_program.object_type_descriptor(
			semantic_program.object_type_id("Model").unwrap(),
		).unwrap();
		let ObjectTypeDescriptorShape::Fields(fields) = model.shape() else {
			panic!("Expected field-shaped object descriptor.");
		};

		assert_eq!(fields.len(), 2);
		assert_eq!(fields[0].name(), "privateValue");
		assert_eq!(fields[0].visibility(), Visibility::Private);
		assert_eq!(fields[1].name(), "publicValue");
		assert_eq!(fields[1].visibility(), Visibility::Public);
	}

	#[test]
	fn warns_when_locked_is_applied_to_read_only_record_pointer() {
		let schema = sqlite_test_schema(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null);
			"#,
			"ExampleDb",
		);
		let program = parse_program(
			"with exampledb;\n\
			fn Main(args: [text]): int {\n\
				rec cust = find first Customers;\n\
				var unavailable: bool = locked cust;\n\
				return 0;\n\
			}",
		);
		let mut analyzer = SemanticAnalyzer::new();

		let semantic_program = analyzer.analyze_standalone_program_with_schema(&program, Some(&schema)).unwrap();

		assert_eq!(semantic_program.warnings().len(), 1);
		assert_eq!(
			semantic_program.warnings()[0].message,
			"`locked` is always false for a read-only record pointer because no record lock is requested.",
		);
	}
}
