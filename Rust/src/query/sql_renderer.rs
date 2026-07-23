use crate::ast::{ DataType, OrderByDirection };

use super::*;

pub(super) trait SqlRenderer {
	fn dialect(&self) -> SqlDialect;

	fn lower_expression(
		&self,
		expression: &QueryExpr,
		parameters: &mut Vec<SqlParameter>,
	) -> Result<String, QueryLoweringError>;

	fn planning_capabilities(&self) -> PlannedQueryBackendCapabilities {
		PlannedQueryBackendCapabilities::default()
	}

	fn quote_identifier(&self, identifier: &str) -> String;

	fn result_column(&self, table_name: &str, column_name: &str, _data_type: &DataType) -> String {
		format!(
			"{}.{}",
			self.quote_identifier(table_name),
			self.quote_identifier(column_name),
		)
	}

	fn result_expression(
		&self,
		expression: &QueryExpr,
		_data_type: &DataType,
		parameters: &mut Vec<SqlParameter>,
	) -> Result<String, QueryLoweringError> {
		self.lower_expression(expression, parameters)
	}

	fn result_sql_expression(&self, expression: &str, _data_type: &DataType) -> String {
		expression.to_string()
	}

	fn table_source(&self, schema_name: &str, table_name: &str, schema_is_implicit: bool) -> String {
		if schema_is_implicit {
			self.quote_identifier(table_name)
		}
		else {
			format!("{}.{}", self.quote_identifier(schema_name), self.quote_identifier(table_name))
		}
	}
}

pub(super) fn lower_count(
	renderer: &impl SqlRenderer,
	plan: &QueryCountPlan,
) -> Result<SqlQuery, QueryLoweringError> {
	let mut parameters = Vec::new();
	let table_source = renderer.table_source(&plan.schema_name, &plan.table_name, plan.schema_is_implicit);
	let mut statement = format!("SELECT COUNT(*) FROM {table_source}");

	if let Some(filter) = &plan.filter {
		statement.push_str(" WHERE ");
		statement.push_str(&renderer.lower_expression(filter, &mut parameters)?);
	}

	Ok(SqlQuery {
		database_name: plan.database_name.clone(),
		dialect: renderer.dialect(),
		group_by: vec![],
		lock_mode: RecordLockMode::None,
		parameters,
		result_shape: SqlQueryResultShape::IntegerScalar,
		scalar_projections: vec![],
		schema_is_implicit: plan.schema_is_implicit,
		schema_name: plan.schema_name.clone(),
		statement,
		table_name: plan.table_name.clone(),
	})
}

pub(super) fn lower_find(
	renderer: &impl SqlRenderer,
	plan: &QueryFindPlan,
) -> Result<SqlQuery, QueryLoweringError> {
	let mut parameters = Vec::new();
	let table_source = renderer.table_source(&plan.schema_name, &plan.table_name, plan.schema_is_implicit);
	let selected_columns = selected_columns(renderer, &plan.record_layout)?;
	let select_columns = selected_columns.iter()
		.map(|column| renderer.result_column(&plan.table_name, &column.column_name, &column.data_type))
		.collect::<Vec<_>>();
	let select_columns = if select_columns.is_empty() { String::from("1") } else { select_columns.join(", ") };
	let mut statement = format!("SELECT {select_columns} FROM {table_source}");

	if let Some(filter) = &plan.filter {
		statement.push_str(" WHERE ");
		statement.push_str(&renderer.lower_expression(filter, &mut parameters)?);
	}

	if !plan.order_by.is_empty() {
		statement.push_str(" ORDER BY ");
		let mut order_by = Vec::with_capacity(plan.order_by.len());

		for item in &plan.order_by {
			let expression = renderer.lower_expression(&item.expression, &mut parameters)?;
			let direction = match effective_find_order_direction(plan.kind, item.direction) {
				OrderByDirection::Ascending => "ASC",
				OrderByDirection::Descending => "DESC",
			};
			order_by.push(format!("{expression} {direction}"));
		}

		statement.push_str(&order_by.join(", "));
	}

	statement.push_str(" LIMIT 1");

	Ok(SqlQuery {
		database_name: plan.database_name.clone(),
		dialect: renderer.dialect(),
		group_by: vec![],
		lock_mode: plan.lock_mode,
		parameters,
		result_shape: SqlQueryResultShape::RecordPointer(plan.record_layout.clone()),
		scalar_projections: vec![],
		schema_is_implicit: plan.schema_is_implicit,
		schema_name: plan.schema_name.clone(),
		statement,
		table_name: plan.table_name.clone(),
	})
}

pub(super) fn lower_for(
	renderer: &impl SqlRenderer,
	plan: &QueryForPlan,
) -> Result<SqlQuery, QueryLoweringError> {
	lower_for_with_scalar_projections(renderer, plan, &[])
}

pub(super) fn lower_for_shape(
	renderer: &impl SqlRenderer,
	shape: &QueryForShape,
) -> Result<SqlQuery, QueryLoweringError> {
	lower_for_with_scalar_projections(renderer, &shape.query, &shape.scalar_projections)
}

fn lower_for_with_scalar_projections(
	renderer: &impl SqlRenderer,
	plan: &QueryForPlan,
	scalar_projections: &[QueryScalarProjection],
) -> Result<SqlQuery, QueryLoweringError> {
	let mut parameters = Vec::new();
	let outer_alias = (!scalar_projections.is_empty()).then_some("__tablo_outer");
	let outer_table_reference = outer_alias.unwrap_or(&plan.table_name);
	let mut table_source = renderer.table_source(&plan.schema_name, &plan.table_name, plan.schema_is_implicit);
	if let Some(alias) = outer_alias {
		table_source.push_str(" AS ");
		table_source.push_str(&renderer.quote_identifier(alias));
	}
	let selected_columns = selected_columns(renderer, &plan.record_layout)?;
	let mut select_columns = selected_columns.iter()
		.map(|column| renderer.result_column(outer_table_reference, &column.column_name, &column.data_type))
		.collect::<Vec<_>>();

	for item in &plan.group_by {
		let expression = rewrite_outer_expression(&item.expression, &plan.table_name, outer_table_reference)?;
		select_columns.push(renderer.result_expression(&expression, &item.data_type, &mut parameters)?);
	}

	let first_scalar_column = select_columns.len();
	let mut lowered_scalar_projections = Vec::with_capacity(scalar_projections.len());
	for (index, projection) in scalar_projections.iter().enumerate() {
		let expression = lower_scalar_projection(
			renderer,
			plan,
			outer_table_reference,
			projection,
			&mut parameters,
		)?;
		select_columns.push(expression);
		lowered_scalar_projections.push(SqlScalarProjection {
			column_index: (first_scalar_column + index) as u32,
			data_type: DataType::Int,
			value_id: projection.value_id,
		});
	}

	if select_columns.is_empty() {
		select_columns.push(String::from("1"));
	}

	let mut statement = format!("SELECT {} FROM {table_source}", select_columns.join(", "));

	if let Some(filter) = &plan.filter {
		let filter = rewrite_outer_expression(filter, &plan.table_name, outer_table_reference)?;
		statement.push_str(" WHERE ");
		statement.push_str(&renderer.lower_expression(&filter, &mut parameters)?);
	}

	if !plan.group_by.is_empty() {
		statement.push_str(" ORDER BY ");
		let mut group_by = Vec::with_capacity(plan.group_by.len());

		for item in &plan.group_by {
			let expression = rewrite_outer_expression(&item.expression, &plan.table_name, outer_table_reference)?;
			group_by.push(renderer.lower_expression(&expression, &mut parameters)?);
		}

		statement.push_str(&group_by.join(", "));
	}
	else if !plan.order_by.is_empty() {
		statement.push_str(" ORDER BY ");
		let mut order_by = Vec::with_capacity(plan.order_by.len());

		for item in &plan.order_by {
			let expression = rewrite_outer_expression(&item.expression, &plan.table_name, outer_table_reference)?;
			let expression = renderer.lower_expression(&expression, &mut parameters)?;
			let direction = match item.direction {
				OrderByDirection::Ascending => "ASC",
				OrderByDirection::Descending => "DESC",
			};
			order_by.push(format!("{expression} {direction}"));
		}

		statement.push_str(&order_by.join(", "));
	}

	if let Some(limit) = &plan.limit {
		let expression = renderer.lower_expression(
			&QueryExpr::Parameter(limit.clone()),
			&mut parameters,
		)?;
		statement.push_str(" LIMIT ");
		statement.push_str(&expression);
	}

	Ok(SqlQuery {
		database_name: plan.database_name.clone(),
		dialect: renderer.dialect(),
		group_by: plan.group_by.iter()
			.map(|item| SqlGroupByItem {
				data_type: item.data_type.clone(),
				key_names: item.key_names.clone(),
			})
			.collect(),
		lock_mode: plan.lock_mode,
		parameters,
		result_shape: SqlQueryResultShape::RecordPointerArray(plan.record_layout.clone()),
		scalar_projections: lowered_scalar_projections,
		schema_is_implicit: plan.schema_is_implicit,
		schema_name: plan.schema_name.clone(),
		statement,
		table_name: plan.table_name.clone(),
	})
}

fn lower_scalar_projection(
	renderer: &impl SqlRenderer,
	enclosing_query: &QueryForPlan,
	outer_table_reference: &str,
	projection: &QueryScalarProjection,
	parameters: &mut Vec<SqlParameter>,
) -> Result<String, QueryLoweringError> {
	let QueryScalarProjectionExpression::CorrelatedCount(count) = &projection.expression;
	if count.query.backend != enclosing_query.backend
		|| count.query.database_name != enclosing_query.database_name {
		return Err(QueryLoweringError::UnsupportedExpression {
			backend: renderer.dialect().backend(),
			description: String::from("cross-backend correlated scalar query"),
		});
	}

	let child_alias = format!("__tablo_count_{}", projection.value_id.0);
	let mut table_source = renderer.table_source(
		&count.query.schema_name,
		&count.query.table_name,
		count.query.schema_is_implicit,
	);
	table_source.push_str(" AS ");
	table_source.push_str(&renderer.quote_identifier(&child_alias));
	let mut statement = format!("SELECT COUNT(*) FROM {table_source}");

	if let Some(filter) = &count.query.filter {
		let filter = rewrite_correlated_expression(
			filter,
			&count.query.table_name,
			&child_alias,
			outer_table_reference,
			&count.correlations,
			renderer.dialect().backend(),
		)?;
		statement.push_str(" WHERE ");
		statement.push_str(&renderer.lower_expression(&filter, parameters)?);
	}

	let expression = renderer.result_sql_expression(&format!("({statement})"), &DataType::Int);
	Ok(format!(
		"{expression} AS {}",
		renderer.quote_identifier(&format!("__tablo_projected_{}", projection.value_id.0)),
	))
}

fn rewrite_correlated_expression(
	expression: &QueryExpr,
	child_table_name: &str,
	child_table_reference: &str,
	outer_table_reference: &str,
	correlations: &[QueryCorrelation],
	backend: crate::schema::DatabaseBackend,
) -> Result<QueryExpr, QueryLoweringError> {
	rewrite_query_expression(
		expression,
		&|column| {
			let mut column = column.clone();
			if column.table_name.eq_ignore_ascii_case(child_table_name) {
				column.table_name = child_table_reference.to_string();
			}
			Ok(column)
		},
		&|parameter| {
			let Some(correlation) = correlations.iter()
				.find(|correlation| correlation.parameter == *parameter)
			else {
				return Ok(QueryExpr::Parameter(parameter.clone()));
			};
			let [column_name] = correlation.outer_field_path.as_slice() else {
				return Err(QueryLoweringError::UnsupportedExpression {
					backend,
					description: String::from("nested correlated record field path"),
				});
			};
			Ok(QueryExpr::Column(QueryColumnReference {
				column_name: column_name.clone(),
				data_type: parameter.data_type.clone(),
				table_name: outer_table_reference.to_string(),
			}))
		},
	)
}

fn rewrite_outer_expression(
	expression: &QueryExpr,
	table_name: &str,
	table_reference: &str,
) -> Result<QueryExpr, QueryLoweringError> {
	rewrite_query_expression(
		expression,
		&|column| {
			let mut column = column.clone();
			if column.table_name.eq_ignore_ascii_case(table_name) {
				column.table_name = table_reference.to_string();
			}
			Ok(column)
		},
		&|parameter| Ok(QueryExpr::Parameter(parameter.clone())),
	)
}

fn rewrite_query_expression(
	expression: &QueryExpr,
	rewrite_column: &impl Fn(&QueryColumnReference) -> Result<QueryColumnReference, QueryLoweringError>,
	rewrite_parameter: &impl Fn(&QueryParameter) -> Result<QueryExpr, QueryLoweringError>,
) -> Result<QueryExpr, QueryLoweringError> {
	Ok(match expression {
		QueryExpr::ArrayLiteral(elements) => QueryExpr::ArrayLiteral(elements.iter()
			.map(|element| rewrite_query_expression(element, rewrite_column, rewrite_parameter))
			.collect::<Result<Vec<_>, _>>()?),
		QueryExpr::Binary(binary) => QueryExpr::Binary(QueryBinaryExpr {
			left: Box::new(rewrite_query_expression(&binary.left, rewrite_column, rewrite_parameter)?),
			operator: binary.operator.clone(),
			right: Box::new(rewrite_query_expression(&binary.right, rewrite_column, rewrite_parameter)?),
		}),
		QueryExpr::BuiltInCall(call) => QueryExpr::BuiltInCall(QueryBuiltInCall {
			arguments: call.arguments.iter()
				.map(|argument| rewrite_query_expression(argument, rewrite_column, rewrite_parameter))
				.collect::<Result<Vec<_>, _>>()?,
			built_in: call.built_in,
		}),
		QueryExpr::Column(column) => QueryExpr::Column(rewrite_column(column)?),
		QueryExpr::Literal(literal) => QueryExpr::Literal(literal.clone()),
		QueryExpr::Parameter(parameter) => rewrite_parameter(parameter)?,
		QueryExpr::Unary(unary) => QueryExpr::Unary(QueryUnaryExpr {
			operand: Box::new(rewrite_query_expression(&unary.operand, rewrite_column, rewrite_parameter)?),
			operator: unary.operator.clone(),
		}),
	})
}

fn selected_columns(
	renderer: &impl SqlRenderer,
	layout: &QueryRecordLayout,
) -> Result<Vec<QueryResultColumn>, QueryLoweringError> {
	layout.selected_known_columns().ok_or_else(|| QueryLoweringError::UnsupportedExpression {
		backend: renderer.dialect().backend(),
		description: String::from("runtime-determined query field selection"),
	})
}
