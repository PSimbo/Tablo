use crate::ast::*;

use super::QueryParameter;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum PlannedQueryControlFlow {
	Conditional,
	#[default]
	Direct,
	Repeated,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryErrorTiming {
	AtQueryStart,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryExecution {
	BatchWith {
		query: PlannedQueryId,
	},
	Independent {
		reason: PlannedQueryIndependentReason,
	},
	MergeWith {
		query: PlannedQueryId,
	},
}

impl PlannedQueryExecution {
	pub fn independent_reason(self) -> Option<PlannedQueryIndependentReason> {
		match self {
			Self::Independent { reason } => Some(reason),
			Self::BatchWith { .. } | Self::MergeWith { .. } => None,
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryIndependentReason {
	AnalysisIncomplete,
	NoOptimizationOpportunity,
	NoSupportedStrategy,
	OptimizationsDisabled,
	SemanticEquivalenceNotProven,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryKind {
	Count,
	Find,
	ForRecord,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryOptimizationOpportunity {
	MergeOrBatchWith {
		query: PlannedQueryId,
	},
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryParameterEvaluation {
	AtQueryStart,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryParameterSource {
	EnclosingQuery(PlannedQueryId),
	Local,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryResultCardinality {
	AtMostOne,
	Many,
	Scalar,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlannedQuery {
	pub body_may_have_side_effects: bool,
	pub captured_parameters: Vec<PlannedQueryParameter>,
	pub control_flow: PlannedQueryControlFlow,
	pub database_name: Option<String>,
	pub enclosing_query: Option<PlannedQueryId>,
	pub execution: PlannedQueryExecution,
	pub id: PlannedQueryId,
	pub is_read_only: Option<bool>,
	pub kind: PlannedQueryKind,
	pub optimization_opportunities: Vec<PlannedQueryOptimizationOpportunity>,
	pub position: usize,
	pub record_slot: Option<u32>,
	pub result_semantics: Option<PlannedQueryResultSemantics>,
	pub transaction_scopes: Vec<PlannedTransactionScopeId>,
	record_binding_position: Option<usize>,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct PlannedQueryId(pub usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlannedQueryParameter {
	pub data_type: DataType,
	pub evaluation: PlannedQueryParameterEvaluation,
	pub field_path: Vec<String>,
	pub slot: u32,
	pub source: PlannedQueryParameterSource,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlannedQueryResultSemantics {
	pub cardinality: PlannedQueryResultCardinality,
	pub error_timing: PlannedQueryErrorTiming,
	pub has_grouping: bool,
	pub has_limit: bool,
	pub has_ordering: bool,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct PlannedTransactionScopeId(pub usize);

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ProgramQueryPlan {
	queries: Vec<PlannedQuery>,
}

impl ProgramQueryPlan {
	pub fn queries(&self) -> &[PlannedQuery] {
		&self.queries
	}

	pub fn query(&self, id: PlannedQueryId) -> Option<&PlannedQuery> {
		self.queries.get(id.0)
	}

	pub(crate) fn disable_optimizations(&mut self) {
		for query in &mut self.queries {
			query.optimization_opportunities.clear();
			query.execution = PlannedQueryExecution::Independent {
				reason: PlannedQueryIndependentReason::OptimizationsDisabled,
			};
		}
	}

	pub(crate) fn populate_analyzed_metadata(
		&mut self,
		mut metadata_for_query: impl FnMut(
			PlannedQueryKind,
			usize,
			Option<usize>,
		) -> Option<AnalyzedQueryMetadata>,
	) {
		for query in &mut self.queries {
			let Some(metadata) = metadata_for_query(
				query.kind,
				query.position,
				query.record_binding_position,
			) else {
				continue;
			};

			query.captured_parameters = metadata.captured_parameters
				.into_iter()
				.map(|parameter| PlannedQueryParameter {
					data_type: parameter.data_type,
					evaluation: PlannedQueryParameterEvaluation::AtQueryStart,
					field_path: parameter.field_path,
					slot: parameter.slot,
					source: PlannedQueryParameterSource::Local,
				})
				.collect();
			query.database_name = Some(metadata.database_name);
			query.is_read_only = Some(metadata.is_read_only);
			query.record_slot = metadata.record_slot;
			query.result_semantics = Some(metadata.result_semantics);
		}

		self.classify_parameter_sources();
		self.identify_nested_read_only_opportunities();
		self.finalize_execution_decisions();
	}

	fn classify_parameter_sources(&mut self) {
		let query_contexts = self.queries.iter()
			.map(|query| (query.enclosing_query, query.record_slot))
			.collect::<Vec<_>>();

		for query in &mut self.queries {
			for parameter in &mut query.captured_parameters {
				let mut enclosing_query = query.enclosing_query;

				while let Some(query_id) = enclosing_query {
					let (next_enclosing_query, record_slot) = query_contexts[query_id.0];

					if record_slot == Some(parameter.slot) {
						parameter.source = PlannedQueryParameterSource::EnclosingQuery(query_id);
						break;
					}

					enclosing_query = next_enclosing_query;
				}
			}
		}
	}

	fn finalize_execution_decisions(&mut self) {
		for query in &mut self.queries {
			let reason = if !query.optimization_opportunities.is_empty() {
				PlannedQueryIndependentReason::NoSupportedStrategy
			}
			else if query.enclosing_query.is_some() {
				PlannedQueryIndependentReason::SemanticEquivalenceNotProven
			}
			else {
				PlannedQueryIndependentReason::NoOptimizationOpportunity
			};

			query.execution = PlannedQueryExecution::Independent {
				reason,
			};
		}
	}

	fn identify_nested_read_only_opportunities(&mut self) {
		for query_index in 0..self.queries.len() {
			let Some(enclosing_query_id) = self.queries[query_index].enclosing_query else {
				continue;
			};
			let enclosing_query = &self.queries[enclosing_query_id.0];
			let is_same_database = self.queries[query_index].database_name.is_some()
				&& self.queries[query_index].database_name == enclosing_query.database_name;
			let captures_enclosing_record = self.queries[query_index].captured_parameters.iter()
				.any(|parameter| {
					parameter.source == PlannedQueryParameterSource::EnclosingQuery(enclosing_query_id)
				});
			let captures_only_query_records = self.queries[query_index].captured_parameters.iter()
				.all(|parameter| {
					matches!(parameter.source, PlannedQueryParameterSource::EnclosingQuery(_))
				});

			if self.queries[query_index].is_read_only == Some(true)
				&& enclosing_query.is_read_only == Some(true)
				&& is_same_database
				&& self.queries[query_index].control_flow == PlannedQueryControlFlow::Direct
				&& self.queries[query_index].transaction_scopes == enclosing_query.transaction_scopes
				&& !enclosing_query.body_may_have_side_effects
				&& captures_enclosing_record
				&& captures_only_query_records {
				self.queries[query_index].optimization_opportunities.push(
					PlannedQueryOptimizationOpportunity::MergeOrBatchWith {
						query: enclosing_query_id,
					},
				);
			}
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct AnalyzedQueryMetadata {
	pub captured_parameters: Vec<QueryParameter>,
	pub database_name: String,
	pub is_read_only: bool,
	pub record_slot: Option<u32>,
	pub result_semantics: PlannedQueryResultSemantics,
}

#[derive(Default)]
struct QueryPlanBuilder {
	control_flow: PlannedQueryControlFlow,
	next_transaction_scope_id: usize,
	queries: Vec<PlannedQuery>,
	transaction_scopes: Vec<PlannedTransactionScopeId>,
}

impl QueryPlanBuilder {
	fn add_query(
		&mut self,
		position: usize,
		kind: PlannedQueryKind,
		enclosing_query: Option<PlannedQueryId>,
		record_binding_position: Option<usize>,
	) -> PlannedQueryId {
		let id = PlannedQueryId(self.queries.len());
		self.queries.push(PlannedQuery {
			body_may_have_side_effects: false,
			captured_parameters: Vec::new(),
			control_flow: self.control_flow,
			database_name: None,
			enclosing_query,
			execution: PlannedQueryExecution::Independent {
				reason: PlannedQueryIndependentReason::AnalysisIncomplete,
			},
			id,
			is_read_only: None,
			kind,
			optimization_opportunities: Vec::new(),
			position,
			record_binding_position,
			record_slot: None,
			result_semantics: None,
			transaction_scopes: self.transaction_scopes.clone(),
		});
		id
	}

	fn visit_block(&mut self, block: &BlockStatement, enclosing_query: Option<PlannedQueryId>) {
		for statement in &block.statements {
			self.visit_statement(statement, enclosing_query);
		}
	}

	fn visit_expression(&mut self, expression: &Expr, enclosing_query: Option<PlannedQueryId>) {
		match expression {
			Expr::Array(array) => {
				for element in &array.elements {
					self.visit_expression(element, enclosing_query);
				}
			}
			Expr::Assignment(assignment) => {
				if let AssignmentTarget::Index(target) = &assignment.target {
					self.visit_expression(&target.index, enclosing_query);
				}
				self.visit_expression(&assignment.value, enclosing_query);
			}
			Expr::Binary(binary) => {
				self.visit_expression(&binary.left, enclosing_query);
				let previous_control_flow = self.control_flow;

				if matches!(binary.operator, BinaryOperator::And | BinaryOperator::Or) {
					self.control_flow = PlannedQueryControlFlow::Conditional;
				}

				self.visit_expression(&binary.right, enclosing_query);
				self.control_flow = previous_control_flow;
			}
			Expr::Call(call) => {
				for argument in &call.arguments {
					self.visit_expression(&argument.value, enclosing_query);
				}
			}
			Expr::Count(count) => {
				let query_id = self.add_query(
					count.position,
					PlannedQueryKind::Count,
					enclosing_query,
					None,
				);

				if let Some(where_clause) = &count.where_clause {
					self.visit_expression(where_clause, Some(query_id));
				}
			}
			Expr::FieldAccess(field_access) => {
				self.visit_expression(&field_access.object, enclosing_query);
			}
			Expr::Find(find) => {
				let query_id = self.add_query(
					find.position,
					PlannedQueryKind::Find,
					enclosing_query,
					None,
				);

				if let Some(where_clause) = &find.where_clause {
					self.visit_expression(where_clause, Some(query_id));
				}

				for item in &find.order_by {
					self.visit_expression(&item.expression, Some(query_id));
				}
			}
			Expr::Index(index) => {
				self.visit_expression(&index.array, enclosing_query);
				self.visit_expression(&index.index, enclosing_query);
			}
			Expr::ObjectConstruction(construction) => {
				for field in &construction.fields {
					self.visit_expression(&field.value, enclosing_query);
				}
			}
			Expr::Range(range) => {
				self.visit_expression(&range.start, enclosing_query);
				self.visit_expression(&range.end, enclosing_query);

				if let Some(step) = &range.step {
					self.visit_expression(step, enclosing_query);
				}
			}
			Expr::Ternary(ternary) => {
				self.visit_expression(&ternary.condition, enclosing_query);
				let previous_control_flow = self.control_flow;
				self.control_flow = PlannedQueryControlFlow::Conditional;
				self.visit_expression(&ternary.true_branch, enclosing_query);
				self.visit_expression(&ternary.false_branch, enclosing_query);
				self.control_flow = previous_control_flow;
			}
			Expr::Unary(unary) => self.visit_expression(&unary.operand, enclosing_query),
			Expr::Boolean(_)
			| Expr::Date(_)
			| Expr::Decimal(_)
			| Expr::Identifier(_)
			| Expr::Integer(_)
			| Expr::New(_)
			| Expr::Null(_)
			| Expr::Text(_)
			| Expr::Time(_)
			| Expr::TimeTz(_)
			| Expr::Timestamp(_)
			| Expr::TimestampTz(_) => {}
		}
	}

	fn visit_for_record(&mut self, statement: &ForRecordStatement, enclosing_query: Option<PlannedQueryId>) {
		let query_id = self.add_query(
			statement.position,
			PlannedQueryKind::ForRecord,
			enclosing_query,
			Some(statement.variable.position),
		);

		if let Some(where_clause) = &statement.where_clause {
			self.visit_expression(where_clause, Some(query_id));
		}

		for item in &statement.group_by {
			self.visit_expression(&item.expression, Some(query_id));
		}

		for item in &statement.order_by {
			self.visit_expression(&item.expression, Some(query_id));
		}

		if let Some(limit) = &statement.limit {
			self.visit_expression(limit, Some(query_id));
		}

		self.queries[query_id.0].body_may_have_side_effects = block_may_have_side_effects(&statement.body);
		let previous_control_flow = self.control_flow;
		self.control_flow = PlannedQueryControlFlow::Direct;
		self.visit_block(&statement.body, Some(query_id));
		self.control_flow = previous_control_flow;
	}

	fn visit_function(&mut self, function: &FunctionDeclaration) {
		let previous_control_flow = self.control_flow;
		let previous_transaction_scopes = std::mem::take(&mut self.transaction_scopes);
		self.control_flow = PlannedQueryControlFlow::Direct;
		self.visit_block(&function.body, None);
		self.control_flow = previous_control_flow;
		self.transaction_scopes = previous_transaction_scopes;
	}

	fn visit_statement(&mut self, statement: &Statement, enclosing_query: Option<PlannedQueryId>) {
		match statement {
			Statement::Block(block) => self.visit_block(block, enclosing_query),
			Statement::Expression(expression) => self.visit_expression(expression, enclosing_query),
			Statement::For(statement) => {
				self.visit_expression(&statement.iterable, enclosing_query);
				let previous_control_flow = self.control_flow;
				self.control_flow = PlannedQueryControlFlow::Repeated;
				self.visit_block(&statement.body, enclosing_query);
				self.control_flow = previous_control_flow;
			}
			Statement::ForRecord(statement) => self.visit_for_record(statement, enclosing_query),
			Statement::FunctionDeclaration(function) => self.visit_function(function),
			Statement::If(statement) => {
				match &statement.condition {
					IfCondition::Expression(expression) => {
						self.visit_expression(expression, enclosing_query);
					}
					IfCondition::RecordPointerBinding(binding) => {
						self.visit_expression(&binding.initial_value, enclosing_query);
					}
				}

				let previous_control_flow = self.control_flow;
				self.control_flow = PlannedQueryControlFlow::Conditional;
				self.visit_block(&statement.then_branch, enclosing_query);

				if let Some(else_branch) = &statement.else_branch {
					self.visit_statement(else_branch, enclosing_query);
				}
				self.control_flow = previous_control_flow;
			}
			Statement::RecordPointerDeclaration(declaration) => {
				self.visit_expression(&declaration.initial_value, enclosing_query);
			}
			Statement::Return(statement) => {
				if let Some(value) = &statement.value {
					self.visit_expression(value, enclosing_query);
				}
			}
			Statement::Transaction(statement) => {
				let transaction_scope = PlannedTransactionScopeId(self.next_transaction_scope_id);
				self.next_transaction_scope_id += 1;
				self.transaction_scopes.push(transaction_scope);
				self.visit_block(&statement.body, enclosing_query);
				self.transaction_scopes.pop();
			}
			Statement::VariableDeclaration(declaration) => {
				if let Some(initial_value) = &declaration.initial_value {
					self.visit_expression(initial_value, enclosing_query);
				}
			}
			Statement::While(statement) => {
				let previous_control_flow = self.control_flow;
				self.control_flow = PlannedQueryControlFlow::Repeated;
				self.visit_expression(&statement.condition, enclosing_query);
				self.visit_block(&statement.body, enclosing_query);
				self.control_flow = previous_control_flow;
			}
			Statement::Break(_)
			| Statement::Continue(_)
			| Statement::Create(_)
			| Statement::Delete(_)
			| Statement::EnumDeclaration(_)
			| Statement::Update(_)
			| Statement::Use(_) => {}
		}
	}
}

pub fn plan_program_queries(program: &AstProgram) -> ProgramQueryPlan {
	let mut builder = QueryPlanBuilder::default();

	for function in &program.functions {
		builder.visit_function(function);
	}

	for statement in &program.statements {
		builder.visit_statement(statement, None);
	}

	if let Some(result) = &program.result {
		builder.visit_expression(result, None);
	}

	ProgramQueryPlan {
		queries: builder.queries,
	}
}

fn block_may_have_side_effects(block: &BlockStatement) -> bool {
	block.statements.iter().any(statement_may_have_side_effects)
}

fn expression_may_have_side_effects(expression: &Expr) -> bool {
	match expression {
		Expr::Array(array) => array.elements.iter().any(expression_may_have_side_effects),
		Expr::Assignment(_) | Expr::Call(_) => true,
		Expr::Binary(binary) => {
			expression_may_have_side_effects(&binary.left)
				|| expression_may_have_side_effects(&binary.right)
		}
		Expr::FieldAccess(field_access) => expression_may_have_side_effects(&field_access.object),
		Expr::Index(index) => {
			expression_may_have_side_effects(&index.array)
				|| expression_may_have_side_effects(&index.index)
		}
		Expr::ObjectConstruction(construction) => construction.fields.iter()
			.any(|field| expression_may_have_side_effects(&field.value)),
		Expr::Range(range) => {
			expression_may_have_side_effects(&range.start)
				|| expression_may_have_side_effects(&range.end)
				|| range.step.as_ref().is_some_and(|step| expression_may_have_side_effects(step))
		}
		Expr::Ternary(ternary) => {
			expression_may_have_side_effects(&ternary.condition)
				|| expression_may_have_side_effects(&ternary.true_branch)
				|| expression_may_have_side_effects(&ternary.false_branch)
		}
		Expr::Unary(unary) => expression_may_have_side_effects(&unary.operand),
		Expr::Boolean(_)
		| Expr::Count(_)
		| Expr::Date(_)
		| Expr::Decimal(_)
		| Expr::Find(_)
		| Expr::Identifier(_)
		| Expr::Integer(_)
		| Expr::New(_)
		| Expr::Null(_)
		| Expr::Text(_)
		| Expr::Time(_)
		| Expr::TimeTz(_)
		| Expr::Timestamp(_)
		| Expr::TimestampTz(_) => false,
	}
}

fn statement_may_have_side_effects(statement: &Statement) -> bool {
	match statement {
		Statement::Block(block) | Statement::Transaction(TransactionStatement { body: block, .. }) => {
			block_may_have_side_effects(block)
		}
		Statement::Create(_) | Statement::Delete(_) | Statement::Update(_) => true,
		Statement::Expression(expression) => expression_may_have_side_effects(expression),
		Statement::For(statement) => {
			expression_may_have_side_effects(&statement.iterable)
				|| block_may_have_side_effects(&statement.body)
		}
		Statement::ForRecord(statement) => {
			statement.is_mut || block_may_have_side_effects(&statement.body)
		}
		Statement::FunctionDeclaration(_) => false,
		Statement::If(statement) => {
			let condition_has_side_effects = match &statement.condition {
				IfCondition::Expression(expression) => expression_may_have_side_effects(expression),
				IfCondition::RecordPointerBinding(binding) => {
					binding.is_mut || expression_may_have_side_effects(&binding.initial_value)
				}
			};

			condition_has_side_effects
				|| block_may_have_side_effects(&statement.then_branch)
				|| statement.else_branch.as_ref()
					.is_some_and(|branch| statement_may_have_side_effects(branch))
		}
		Statement::RecordPointerDeclaration(declaration) => declaration.is_mut
			|| expression_may_have_side_effects(&declaration.initial_value),
		Statement::Return(statement) => statement.value.as_ref()
			.is_some_and(expression_may_have_side_effects),
		Statement::VariableDeclaration(declaration) => declaration.initial_value.as_ref()
			.is_some_and(expression_may_have_side_effects),
		Statement::While(statement) => {
			expression_may_have_side_effects(&statement.condition)
				|| block_may_have_side_effects(&statement.body)
		}
		Statement::Break(_)
		| Statement::Continue(_)
		| Statement::EnumDeclaration(_)
		| Statement::Use(_) => false,
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	use crate::source::*;
	use crate::syntax::lexer::*;
	use crate::syntax::parser::*;

	fn parse_program(source: &str) -> AstProgram {
		let mut lexer = Lexer::new(SourceText::new(source));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		parser.parse_program().unwrap()
	}

	fn query_at_position(plan: &ProgramQueryPlan, position: usize) -> &PlannedQuery {
		plan.queries().iter()
			.find(|query| query.position == position)
			.unwrap()
	}

	#[test]
	fn does_not_offer_cross_database_query_optimization() {
		let program = parse_program(
			concat!(
				"fn Main(args: [text]) int {\n",
				"  for rec customer in Customers {\n",
				"    for rec customerOrder in Orders where CustomerId == customer.Id {}\n",
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
		let outer_slot = 1;
		let mut plan = plan_program_queries(&program);

		plan.populate_analyzed_metadata(|kind, position, _| {
			if position == outer_loop.position {
				return Some(AnalyzedQueryMetadata {
					captured_parameters: Vec::new(),
					database_name: String::from("Primary"),
					is_read_only: true,
					record_slot: Some(outer_slot),
					result_semantics: PlannedQueryResultSemantics {
						cardinality: PlannedQueryResultCardinality::Many,
						error_timing: PlannedQueryErrorTiming::AtQueryStart,
						has_grouping: false,
						has_limit: false,
						has_ordering: false,
					},
				});
			}

			if position == inner_loop.position {
				return Some(AnalyzedQueryMetadata {
					captured_parameters: vec![QueryParameter {
						data_type: DataType::Int,
						field_path: vec![String::from("Id")],
						slot: outer_slot,
					}],
					database_name: String::from("Secondary"),
					is_read_only: true,
					record_slot: Some(2),
					result_semantics: PlannedQueryResultSemantics {
						cardinality: PlannedQueryResultCardinality::Many,
						error_timing: PlannedQueryErrorTiming::AtQueryStart,
						has_grouping: false,
						has_limit: false,
						has_ordering: false,
					},
				});
			}

			panic!("Unexpected {kind:?} query at position {position}.");
		});

		let outer_query = query_at_position(&plan, outer_loop.position);
		let inner_query = query_at_position(&plan, inner_loop.position);

		assert_eq!(
			inner_query.captured_parameters[0].source,
			PlannedQueryParameterSource::EnclosingQuery(outer_query.id),
		);
		assert!(inner_query.optimization_opportunities.is_empty());
		assert_eq!(
			inner_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::SemanticEquivalenceNotProven),
		);
	}

	#[test]
	fn nested_function_queries_are_not_enclosed_by_declaration_context() {
		let program = parse_program(
			concat!(
				"fn Main(args: [text]) int {\n",
				"  for rec customer in Customers {\n",
				"    fn CountOrders() int { return count Orders; }\n",
				"  }\n",
				"  return 0;\n",
				"}",
			),
		);
		let Statement::ForRecord(outer_loop) = &program.functions[0].body.statements[0] else {
			panic!("Expected outer record loop.");
		};
		let Statement::FunctionDeclaration(function) = &outer_loop.body.statements[0] else {
			panic!("Expected nested function declaration.");
		};
		let Statement::Return(ReturnStatement {
			value: Some(Expr::Count(count)),
			..
		}) = &function.body.statements[0] else {
			panic!("Expected count query in nested function.");
		};

		let plan = plan_program_queries(&program);
		let outer_query = query_at_position(&plan, outer_loop.position);
		let count_query = query_at_position(&plan, count.position);

		assert_eq!(outer_query.enclosing_query, None);
		assert_eq!(count_query.enclosing_query, None);
	}

	#[test]
	fn records_nested_query_structure_with_independent_execution() {
		let program = parse_program(
			concat!(
				"fn Main(args: [text]) int {\n",
				"  for rec customer in Customers {\n",
				"    rec customerOrder = find first Orders;\n",
				"    for rec item in Items {\n",
				"      var noteCount: int = count Notes;\n",
				"    }\n",
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
		let Statement::ForRecord(inner_loop) = &outer_loop.body.statements[1] else {
			panic!("Expected nested record loop.");
		};
		let Statement::VariableDeclaration(VariableDeclaration {
			initial_value: Some(Expr::Count(count)),
			..
		}) = &inner_loop.body.statements[0] else {
			panic!("Expected nested count query.");
		};

		let plan = plan_program_queries(&program);
		let outer_query = query_at_position(&plan, outer_loop.position);
		let find_query = query_at_position(&plan, find.position);
		let inner_query = query_at_position(&plan, inner_loop.position);
		let count_query = query_at_position(&plan, count.position);

		assert_eq!(plan.queries().len(), 4);
		assert_eq!(plan.query(outer_query.id), Some(&PlannedQuery {
			body_may_have_side_effects: false,
			captured_parameters: Vec::new(),
			control_flow: PlannedQueryControlFlow::Direct,
			database_name: None,
			enclosing_query: None,
			execution: PlannedQueryExecution::Independent {
				reason: PlannedQueryIndependentReason::AnalysisIncomplete,
			},
			id: outer_query.id,
			is_read_only: None,
			kind: PlannedQueryKind::ForRecord,
			optimization_opportunities: Vec::new(),
			position: outer_loop.position,
			record_binding_position: Some(outer_loop.variable.position),
			record_slot: None,
			result_semantics: None,
			transaction_scopes: Vec::new(),
		}));
		assert_eq!(find_query.enclosing_query, Some(outer_query.id));
		assert_eq!(inner_query.enclosing_query, Some(outer_query.id));
		assert_eq!(count_query.enclosing_query, Some(inner_query.id));
		assert!(plan.queries().iter().all(|query| {
			query.execution.independent_reason()
				== Some(PlannedQueryIndependentReason::AnalysisIncomplete)
		}));
	}
}
