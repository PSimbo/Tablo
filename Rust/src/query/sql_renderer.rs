use crate::ast::OrderByDirection;

use super::QueryCountPlan;
use super::QueryExpr;
use super::QueryFindPlan;
use super::QueryForPlan;
use super::QueryLoweringError;
use super::SqlDialect;
use super::SqlGroupByItem;
use super::SqlParameter;
use super::SqlQuery;
use super::SqlQueryResultShape;
use super::effective_find_order_direction;

pub(super) trait SqlRenderer {
	fn dialect(&self) -> SqlDialect;

	fn lower_expression(
		&self,
		expression: &QueryExpr,
		parameters: &mut Vec<SqlParameter>,
	) -> Result<String, QueryLoweringError>;

	fn quote_identifier(&self, identifier: &str) -> String;

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
		parameters,
		result_shape: SqlQueryResultShape::IntegerScalar,
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
	let select_columns = plan.record_columns.iter()
		.map(|column| format!(
			"{}.{}",
			renderer.quote_identifier(&plan.table_name),
			renderer.quote_identifier(&column.column_name),
		))
		.collect::<Vec<_>>()
		.join(", ");
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
		parameters,
		result_shape: SqlQueryResultShape::RecordPointer(plan.record_columns.clone()),
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
	let mut parameters = Vec::new();
	let table_source = renderer.table_source(&plan.schema_name, &plan.table_name, plan.schema_is_implicit);
	let mut select_columns = plan.record_columns.iter()
		.map(|column| format!(
			"{}.{}",
			renderer.quote_identifier(&plan.table_name),
			renderer.quote_identifier(&column.column_name),
		))
		.collect::<Vec<_>>();

	for item in &plan.group_by {
		select_columns.push(renderer.lower_expression(&item.expression, &mut parameters)?);
	}

	let mut statement = format!("SELECT {} FROM {table_source}", select_columns.join(", "));

	if let Some(filter) = &plan.filter {
		statement.push_str(" WHERE ");
		statement.push_str(&renderer.lower_expression(filter, &mut parameters)?);
	}

	if !plan.group_by.is_empty() {
		statement.push_str(" ORDER BY ");
		let mut group_by = Vec::with_capacity(plan.group_by.len());

		for item in &plan.group_by {
			group_by.push(renderer.lower_expression(&item.expression, &mut parameters)?);
		}

		statement.push_str(&group_by.join(", "));
	}
	else if !plan.order_by.is_empty() {
		statement.push_str(" ORDER BY ");
		let mut order_by = Vec::with_capacity(plan.order_by.len());

		for item in &plan.order_by {
			let expression = renderer.lower_expression(&item.expression, &mut parameters)?;
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
		parameters,
		result_shape: SqlQueryResultShape::RecordPointerArray(plan.record_columns.clone()),
		schema_is_implicit: plan.schema_is_implicit,
		schema_name: plan.schema_name.clone(),
		statement,
		table_name: plan.table_name.clone(),
	})
}
