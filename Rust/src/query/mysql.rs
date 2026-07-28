use crate::builtins::BuiltInFunction;
use crate::schema::DatabaseBackend;
use crate::sql::quote_mysql_identifier;

use super::*;
use super::sql_renderer::SqlRenderer;

pub(super) struct MySqlRenderer;

impl SqlRenderer for MySqlRenderer {
	fn dialect(&self) -> SqlDialect {
		SqlDialect::MySql
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
		quote_mysql_identifier(identifier)
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
				format!("(LOCATE({right}, {left}) > 0)")
			}
		},
		BuiltInFunction::CountOf => {
			let left = lower_expression(&call.arguments[0], parameters)?;
			let right = lower_expression(&call.arguments[1], parameters)?;
			format!("CASE WHEN CHAR_LENGTH({left}) = 0 THEN 0 ELSE ((CHAR_LENGTH({right}) - CHAR_LENGTH(REPLACE({right}, {left}, ''))) DIV CHAR_LENGTH({left})) END")
		}
		BuiltInFunction::Day => temporal_part("DAY", &call.arguments[0], parameters)?,
		BuiltInFunction::Hour => temporal_part("HOUR", &call.arguments[0], parameters)?,
		BuiltInFunction::IndexOf => {
			let left = lower_expression(&call.arguments[0], parameters)?;
			let right = lower_expression(&call.arguments[1], parameters)?;
			format!("NULLIF(LOCATE({left}, {right}), 0)")
		}
		BuiltInFunction::Minute => temporal_part("MINUTE", &call.arguments[0], parameters)?,
		BuiltInFunction::Month => temporal_part("MONTH", &call.arguments[0], parameters)?,
		BuiltInFunction::Second => temporal_part("SECOND", &call.arguments[0], parameters)?,
		BuiltInFunction::Trim => format!("TRIM({})", lower_expression(&call.arguments[0], parameters)?),
		BuiltInFunction::Year => temporal_part("YEAR", &call.arguments[0], parameters)?,
		built_in => {
			return Err(QueryLoweringError::UnsupportedBuiltIn {
				backend: DatabaseBackend::MySql,
				built_in,
			});
		}
	};

	Ok(result)
}

fn lower_expression(expression: &QueryExpr, parameters: &mut Vec<SqlParameter>) -> Result<String, QueryLoweringError> {
	match expression {
		QueryExpr::ArrayLiteral(_) => Err(QueryLoweringError::UnsupportedExpression {
			backend: DatabaseBackend::MySql,
			description: String::from("array literal outside a supported built-in call"),
		}),
		QueryExpr::Binary(binary) => {
			let left = lower_expression(&binary.left, parameters)?;
			let right = lower_expression(&binary.right, parameters)?;

			if binary.operator == QueryBinaryOperator::Concatenate {
				return Ok(format!("CONCAT({left}, {right})"));
			}

			let operator = match binary.operator {
				QueryBinaryOperator::Add => "+",
				QueryBinaryOperator::And => "AND",
				QueryBinaryOperator::Concatenate => unreachable!(),
				QueryBinaryOperator::Divide => "/",
				QueryBinaryOperator::Equal => "=",
				QueryBinaryOperator::GreaterThan => ">",
				QueryBinaryOperator::GreaterThanOrEqual => ">=",
				QueryBinaryOperator::IntegerDivide => "DIV",
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
			quote_mysql_identifier(&column.table_name),
			quote_mysql_identifier(&column.column_name),
		)),
		QueryExpr::Literal(QueryLiteral::Boolean(value)) => Ok(if *value { String::from("TRUE") } else { String::from("FALSE") }),
		QueryExpr::Literal(QueryLiteral::Date(value)) => Ok(typed_temporal_literal("DATE", &value.to_string())),
		QueryExpr::Literal(QueryLiteral::Decimal(value)) => Ok(value.to_string()),
		QueryExpr::Literal(QueryLiteral::Integer(value)) => Ok(value.to_string()),
		QueryExpr::Literal(QueryLiteral::Text(value)) => Ok(quote_text_literal(value)),
		QueryExpr::Literal(QueryLiteral::Time(value)) => Ok(typed_temporal_literal("TIME", &value.to_string())),
		QueryExpr::Literal(QueryLiteral::TimeTz(value)) => Ok(quote_text_literal(&value.to_string())),
		QueryExpr::Literal(QueryLiteral::Timestamp(value)) => Ok(typed_temporal_literal("TIMESTAMP", &value.to_string())),
		QueryExpr::Literal(QueryLiteral::TimestampTz(value)) => Ok(quote_text_literal(&value.to_string())),
		QueryExpr::Parameter(parameter) => {
			let index = parameters.len() as u32 + 1;
			parameters.push(SqlParameter {
				data_type: parameter.data_type.clone(),
				field_path: parameter.field_path.clone(),
				index,
				slot: parameter.slot,
			});
			Ok(String::from("?"))
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

fn quote_text_literal(value: &str) -> String {
	format!("'{}'", value.replace('\'', "''"))
}

fn temporal_part(
	part: &str,
	argument: &QueryExpr,
	parameters: &mut Vec<SqlParameter>,
) -> Result<String, QueryLoweringError> {
	let value = lower_expression(argument, parameters)?;
	Ok(format!("CAST(FLOOR(EXTRACT({part} FROM {value})) AS SIGNED)"))
}

fn typed_temporal_literal(data_type: &str, value: &str) -> String {
	format!("{data_type} {}", quote_text_literal(value))
}
