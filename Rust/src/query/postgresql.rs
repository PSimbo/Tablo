use crate::ast::DataType;
use crate::builtins::BuiltInFunction;
use crate::schema::DatabaseBackend;
use crate::sql::postgresql_type_name;
use crate::sql::quote_identifier as quote_ansi_identifier;

use super::*;
use super::sql_renderer::SqlRenderer;

pub(super) struct PostgreSqlRenderer;

impl SqlRenderer for PostgreSqlRenderer {
	fn dialect(&self) -> SqlDialect {
		SqlDialect::PostgreSql
	}

	fn lower_expression(
		&self,
		expression: &QueryExpr,
		parameters: &mut Vec<SqlParameter>,
	) -> Result<String, QueryLoweringError> {
		lower_expression(expression, parameters)
	}

	fn planning_capabilities(&self) -> PlannedQueryBackendCapabilities {
		PlannedQueryBackendCapabilities {
			merge_correlated_count: true,
		}
	}

	fn quote_identifier(&self, identifier: &str) -> String {
		quote_ansi_identifier(identifier)
	}

	fn result_column(&self, table_name: &str, column_name: &str, _data_type: &DataType) -> String {
		format!(
			"CAST({}.{} AS TEXT)",
			quote_ansi_identifier(table_name),
			quote_ansi_identifier(column_name),
		)
	}

	fn result_expression(
		&self,
		expression: &QueryExpr,
		_data_type: &DataType,
		parameters: &mut Vec<SqlParameter>,
	) -> Result<String, QueryLoweringError> {
		Ok(format!("CAST({} AS TEXT)", lower_expression(expression, parameters)?))
	}

	fn result_sql_expression(&self, expression: &str, _data_type: &DataType) -> String {
		format!("CAST({expression} AS TEXT)")
	}
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
				QueryBinaryOperator::IntegerDivide => "/",
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
			quote_ansi_identifier(&column.table_name),
			quote_ansi_identifier(&column.column_name),
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
			Ok(postgresql_parameter(index, &parameter.data_type)?)
		}
		QueryExpr::Unary(unary) => {
			let operand = lower_expression(&unary.operand, parameters)?;
			Ok(match unary.operator {
				QueryUnaryOperator::IsNotNull => format!("({operand} IS NOT NULL)"),
				QueryUnaryOperator::IsNull => format!("({operand} IS NULL)"),
				QueryUnaryOperator::Negate => format!("(-{operand})"),
				QueryUnaryOperator::Not => format!("(NOT {operand})"),
			})
		}
	}
}

fn postgresql_parameter(index: u32, data_type: &DataType) -> Result<String, QueryLoweringError> {
	let sql_type = match postgresql_type_name(data_type) {
		Some(sql_type) => sql_type,
		None => {
			return Err(QueryLoweringError::UnsupportedExpression {
				backend: DatabaseBackend::PostgreSql,
				description: format!("parameter of type `{}`", data_type.name()),
			});
		}
	};

	Ok(format!("CAST(CAST(${index} AS TEXT) AS {sql_type})"))
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
