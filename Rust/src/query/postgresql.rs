use crate::ast::OrderByDirection;
use crate::builtins::BuiltInFunction;
use crate::schema::DatabaseBackend;
use crate::sql::quote_identifier;
use crate::sql::table_source;

use super::QueryBinaryOperator;
use super::QueryCountPlan;
use super::QueryExpr;
use super::QueryFindPlan;
use super::QueryLiteral;
use super::QueryLoweringError;
use super::QueryUnaryOperator;
use super::SqlDialect;
use super::SqlParameter;
use super::SqlQuery;
use super::SqlQueryResultShape;
use super::effective_find_order_direction;

pub(super) fn lower_count(plan: &QueryCountPlan) -> Result<SqlQuery, QueryLoweringError> {
	let mut parameters = Vec::new();
	let table_source = table_source(&plan.schema_name, &plan.table_name, plan.schema_is_implicit);
	let mut statement = format!("SELECT COUNT(*) FROM {table_source}");

	if let Some(filter) = &plan.filter {
		statement.push_str(" WHERE ");
		statement.push_str(&lower_expression(filter, &mut parameters)?);
	}

	Ok(SqlQuery {
		database_name: plan.database_name.clone(),
		dialect: SqlDialect::PostgreSql,
		group_by: vec![],
		parameters,
		result_shape: SqlQueryResultShape::IntegerScalar,
		schema_is_implicit: plan.schema_is_implicit,
		schema_name: plan.schema_name.clone(),
		statement,
		table_name: plan.table_name.clone(),
	})
}

pub(super) fn lower_find(plan: &QueryFindPlan) -> Result<SqlQuery, QueryLoweringError> {
	let mut parameters = Vec::new();
	let table_source = table_source(&plan.schema_name, &plan.table_name, plan.schema_is_implicit);
	let select_columns = plan.record_columns.iter()
		.map(|column| format!("{}.{}", quote_identifier(&plan.table_name), quote_identifier(&column.column_name)))
		.collect::<Vec<_>>()
		.join(", ");
	let mut statement = format!("SELECT {select_columns} FROM {table_source}");

	if let Some(filter) = &plan.filter {
		statement.push_str(" WHERE ");
		statement.push_str(&lower_expression(filter, &mut parameters)?);
	}

	if !plan.order_by.is_empty() {
		statement.push_str(" ORDER BY ");
		let mut order_by = Vec::with_capacity(plan.order_by.len());

		for item in &plan.order_by {
			let expression = lower_expression(&item.expression, &mut parameters)?;
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
		dialect: SqlDialect::PostgreSql,
		group_by: vec![],
		parameters,
		result_shape: SqlQueryResultShape::RecordPointer(plan.record_columns.clone()),
		schema_is_implicit: plan.schema_is_implicit,
		schema_name: plan.schema_name.clone(),
		statement,
		table_name: plan.table_name.clone(),
	})
}

fn lower_built_in(
	call: &super::QueryBuiltInCall,
	parameters: &mut Vec<SqlParameter>,
) -> Result<String, QueryLoweringError> {
	let result = match call.built_in {
		BuiltInFunction::Contains => match &call.arguments[0] {
			QueryExpr::ArrayLiteral(values) => {
				let right = lower_expression(&call.arguments[1], parameters)?;
				let mut lowered_values = Vec::with_capacity(values.len());

				for value in values {
					lowered_values.push(lower_expression(value, parameters)?);
				}

				format!("({right} IN ({}))", lowered_values.join(", "))
			}
			_ => {
				let left = lower_expression(&call.arguments[0], parameters)?;
				let right = lower_expression(&call.arguments[1], parameters)?;
				format!("(STRPOS({left}, {right}) > 0)")
			}
		},
		BuiltInFunction::CountOf => {
			let left = lower_expression(&call.arguments[0], parameters)?;
			let right = lower_expression(&call.arguments[1], parameters)?;
			format!("CASE WHEN LENGTH({left}) = 0 THEN 0 ELSE ((LENGTH({right}) - LENGTH(REPLACE({right}, {left}, ''))) / LENGTH({left})) END")
		}
		BuiltInFunction::Day => temporal_part("DAY", &call.arguments[0], parameters)?,
		BuiltInFunction::Hour => temporal_part("HOUR", &call.arguments[0], parameters)?,
		BuiltInFunction::IndexOf => {
			let left = lower_expression(&call.arguments[0], parameters)?;
			let right = lower_expression(&call.arguments[1], parameters)?;
			format!("NULLIF(STRPOS({right}, {left}), 0)")
		}
		BuiltInFunction::Minute => temporal_part("MINUTE", &call.arguments[0], parameters)?,
		BuiltInFunction::Month => temporal_part("MONTH", &call.arguments[0], parameters)?,
		BuiltInFunction::Second => temporal_part("SECOND", &call.arguments[0], parameters)?,
		BuiltInFunction::Trim => format!("TRIM({})", lower_expression(&call.arguments[0], parameters)?),
		BuiltInFunction::Year => temporal_part("YEAR", &call.arguments[0], parameters)?,
		built_in => {
			return Err(QueryLoweringError::UnsupportedBuiltIn {
				backend: DatabaseBackend::PostgreSql,
				built_in,
			});
		}
	};

	Ok(result)
}

fn lower_expression(expression: &QueryExpr, parameters: &mut Vec<SqlParameter>) -> Result<String, QueryLoweringError> {
	match expression {
		QueryExpr::ArrayLiteral(_) => Err(QueryLoweringError::UnsupportedExpression {
			backend: DatabaseBackend::PostgreSql,
			description: String::from("array literal outside a supported built-in call"),
		}),
		QueryExpr::Binary(binary) => {
			let left = lower_expression(&binary.left, parameters)?;
			let right = lower_expression(&binary.right, parameters)?;
			let operator = match binary.operator {
				QueryBinaryOperator::Add => "+",
				QueryBinaryOperator::And => "AND",
				QueryBinaryOperator::Concatenate => "||",
				QueryBinaryOperator::Divide => "/",
				QueryBinaryOperator::Equal => "=",
				QueryBinaryOperator::GreaterThan => ">",
				QueryBinaryOperator::GreaterThanOrEqual => ">=",
				QueryBinaryOperator::LessThan => "<",
				QueryBinaryOperator::LessThanOrEqual => "<=",
				QueryBinaryOperator::Modulo => "%",
				QueryBinaryOperator::Multiply => "*",
				QueryBinaryOperator::NotEqual => "<>",
				QueryBinaryOperator::Or => "OR",
				QueryBinaryOperator::Subtract => "-",
				QueryBinaryOperator::Xor => {
					return Ok(format!("((({left}) AND NOT ({right})) OR (NOT ({left}) AND ({right})))"));
				}
			};

			Ok(format!("({left} {operator} {right})"))
		}
		QueryExpr::BuiltInCall(call) => lower_built_in(call, parameters),
		QueryExpr::Column(column) => Ok(format!(
			"{}.{}",
			quote_identifier(&column.table_name),
			quote_identifier(&column.column_name),
		)),
		QueryExpr::Literal(QueryLiteral::Boolean(value)) => Ok(if *value { String::from("TRUE") } else { String::from("FALSE") }),
		QueryExpr::Literal(QueryLiteral::Date(value)) => Ok(typed_temporal_literal("DATE", &value.to_string())),
		QueryExpr::Literal(QueryLiteral::Decimal(value)) => Ok(value.to_string()),
		QueryExpr::Literal(QueryLiteral::Integer(value)) => Ok(value.to_string()),
		QueryExpr::Literal(QueryLiteral::Text(value)) => Ok(quote_text_literal(value)),
		QueryExpr::Literal(QueryLiteral::Time(value)) => Ok(typed_temporal_literal("TIME", &value.to_string())),
		QueryExpr::Literal(QueryLiteral::TimeTz(value)) => Ok(typed_temporal_literal("TIME WITH TIME ZONE", &value.to_string())),
		QueryExpr::Literal(QueryLiteral::Timestamp(value)) => Ok(typed_temporal_literal("TIMESTAMP", &value.to_string())),
		QueryExpr::Literal(QueryLiteral::TimestampTz(value)) => Ok(typed_temporal_literal("TIMESTAMP WITH TIME ZONE", &value.to_string())),
		QueryExpr::Parameter(parameter) => {
			let index = parameters.len() as u32 + 1;
			parameters.push(SqlParameter {
				data_type: parameter.data_type.clone(),
				field_path: parameter.field_path.clone(),
				index,
				slot: parameter.slot,
			});
			Ok(format!("${index}"))
		}
		QueryExpr::Unary(unary) => {
			let operand = lower_expression(&unary.operand, parameters)?;
			Ok(match unary.operator {
				QueryUnaryOperator::Negate => format!("(-{operand})"),
				QueryUnaryOperator::Not => format!("(NOT {operand})"),
			})
		}
	}
}

fn quote_text_literal(value: &str) -> String {
	format!("'{}'", value.replace('\'', "''"))
}

fn temporal_part(
	part: &str,
	argument: &QueryExpr,
	parameters: &mut Vec<SqlParameter>,
) -> Result<String, QueryLoweringError> {
	let value = lower_expression(argument, parameters)?;
	Ok(format!("CAST(TRUNC(EXTRACT({part} FROM {value})) AS BIGINT)"))
}

fn typed_temporal_literal(data_type: &str, value: &str) -> String {
	format!("{data_type} {}", quote_text_literal(value))
}
