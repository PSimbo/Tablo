use crate::ast::*;

use super::QueryParameter;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryExecution {
	Independent,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryKind {
	Count,
	Find,
	ForRecord,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlannedQueryParameterEvaluation {
	AtQueryStart,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlannedQuery {
	pub captured_parameters: Vec<PlannedQueryParameter>,
	pub enclosing_query: Option<PlannedQueryId>,
	pub execution: PlannedQueryExecution,
	pub id: PlannedQueryId,
	pub kind: PlannedQueryKind,
	pub position: usize,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct PlannedQueryId(pub usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlannedQueryParameter {
	pub data_type: DataType,
	pub evaluation: PlannedQueryParameterEvaluation,
	pub field_path: Vec<String>,
	pub slot: u32,
}

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

	pub(crate) fn populate_captured_parameters(
		&mut self,
		mut parameters_for_query: impl FnMut(PlannedQueryKind, usize) -> Vec<QueryParameter>,
	) {
		for query in &mut self.queries {
			query.captured_parameters = parameters_for_query(query.kind, query.position)
				.into_iter()
				.map(|parameter| PlannedQueryParameter {
					data_type: parameter.data_type,
					evaluation: PlannedQueryParameterEvaluation::AtQueryStart,
					field_path: parameter.field_path,
					slot: parameter.slot,
				})
				.collect();
		}
	}
}

#[derive(Default)]
struct QueryPlanBuilder {
	queries: Vec<PlannedQuery>,
}

impl QueryPlanBuilder {
	fn add_query(
		&mut self,
		position: usize,
		kind: PlannedQueryKind,
		enclosing_query: Option<PlannedQueryId>,
	) -> PlannedQueryId {
		let id = PlannedQueryId(self.queries.len());
		self.queries.push(PlannedQuery {
			captured_parameters: Vec::new(),
			enclosing_query,
			execution: PlannedQueryExecution::Independent,
			id,
			kind,
			position,
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
				self.visit_expression(&binary.right, enclosing_query);
			}
			Expr::Call(call) => {
				for argument in &call.arguments {
					self.visit_expression(&argument.value, enclosing_query);
				}
			}
			Expr::Count(count) => {
				let query_id = self.add_query(count.position, PlannedQueryKind::Count, enclosing_query);

				if let Some(where_clause) = &count.where_clause {
					self.visit_expression(where_clause, Some(query_id));
				}
			}
			Expr::FieldAccess(field_access) => {
				self.visit_expression(&field_access.object, enclosing_query);
			}
			Expr::Find(find) => {
				let query_id = self.add_query(find.position, PlannedQueryKind::Find, enclosing_query);

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
				self.visit_expression(&ternary.true_branch, enclosing_query);
				self.visit_expression(&ternary.false_branch, enclosing_query);
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
		let query_id = self.add_query(statement.position, PlannedQueryKind::ForRecord, enclosing_query);

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

		self.visit_block(&statement.body, Some(query_id));
	}

	fn visit_function(&mut self, function: &FunctionDeclaration) {
		self.visit_block(&function.body, None);
	}

	fn visit_statement(&mut self, statement: &Statement, enclosing_query: Option<PlannedQueryId>) {
		match statement {
			Statement::Block(block) => self.visit_block(block, enclosing_query),
			Statement::Expression(expression) => self.visit_expression(expression, enclosing_query),
			Statement::For(statement) => {
				self.visit_expression(&statement.iterable, enclosing_query);
				self.visit_block(&statement.body, enclosing_query);
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

				self.visit_block(&statement.then_branch, enclosing_query);

				if let Some(else_branch) = &statement.else_branch {
					self.visit_statement(else_branch, enclosing_query);
				}
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
				self.visit_block(&statement.body, enclosing_query);
			}
			Statement::VariableDeclaration(declaration) => {
				if let Some(initial_value) = &declaration.initial_value {
					self.visit_expression(initial_value, enclosing_query);
				}
			}
			Statement::While(statement) => {
				self.visit_expression(&statement.condition, enclosing_query);
				self.visit_block(&statement.body, enclosing_query);
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
			captured_parameters: Vec::new(),
			enclosing_query: None,
			execution: PlannedQueryExecution::Independent,
			id: outer_query.id,
			kind: PlannedQueryKind::ForRecord,
			position: outer_loop.position,
		}));
		assert_eq!(find_query.enclosing_query, Some(outer_query.id));
		assert_eq!(inner_query.enclosing_query, Some(outer_query.id));
		assert_eq!(count_query.enclosing_query, Some(inner_query.id));
		assert!(plan.queries().iter().all(|query| query.execution == PlannedQueryExecution::Independent));
	}
}
