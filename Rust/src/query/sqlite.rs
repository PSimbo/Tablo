use crate::builtins::BuiltInFunction;
use crate::schema::DatabaseBackend;
use crate::sql::quote_identifier as quote_ansi_identifier;

use super::QueryBinaryOperator;
use super::QueryExpr;
use super::QueryLiteral;
use super::QueryLoweringError;
use super::QueryUnaryOperator;
use super::SqlDialect;
use super::SqlParameter;
use super::sql_renderer::SqlRenderer;

pub(super) struct SqliteRenderer;

impl SqlRenderer for SqliteRenderer {
	fn dialect(&self) -> SqlDialect {
		SqlDialect::Sqlite
	}

	fn lower_expression(
		&self,
		expression: &QueryExpr,
		parameters: &mut Vec<SqlParameter>,
	) -> Result<String, QueryLoweringError> {
		lower_expression(expression, parameters)
	}

	fn quote_identifier(&self, identifier: &str) -> String {
		quote_ansi_identifier(identifier)
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
				format!("(INSTR({left}, {right}) > 0)")
			}
		},
		BuiltInFunction::CountOf => {
			let left = lower_expression(&call.arguments[0], parameters)?;
			let right = lower_expression(&call.arguments[1], parameters)?;
			format!("CASE WHEN LENGTH({left}) = 0 THEN 0 ELSE ((LENGTH({right}) - LENGTH(REPLACE({right}, {left}, ''))) / LENGTH({left})) END")
		}
		BuiltInFunction::Day => temporal_part("%d", &call.arguments[0], parameters)?,
		BuiltInFunction::Hour => temporal_part("%H", &call.arguments[0], parameters)?,
		BuiltInFunction::IndexOf => {
			let left = lower_expression(&call.arguments[0], parameters)?;
			let right = lower_expression(&call.arguments[1], parameters)?;
			format!("NULLIF(INSTR({right}, {left}), 0)")
		}
		BuiltInFunction::Minute => temporal_part("%M", &call.arguments[0], parameters)?,
		BuiltInFunction::Month => temporal_part("%m", &call.arguments[0], parameters)?,
		BuiltInFunction::Second => temporal_part("%S", &call.arguments[0], parameters)?,
		BuiltInFunction::Trim => format!("TRIM({})", lower_expression(&call.arguments[0], parameters)?),
		BuiltInFunction::Year => temporal_part("%Y", &call.arguments[0], parameters)?,
		built_in => {
			return Err(QueryLoweringError::UnsupportedBuiltIn {
				backend: DatabaseBackend::Sqlite,
				built_in,
			});
		}
	};

	Ok(result)
}

fn lower_expression(expression: &QueryExpr, parameters: &mut Vec<SqlParameter>) -> Result<String, QueryLoweringError> {
	match expression {
		QueryExpr::ArrayLiteral(_) => Err(QueryLoweringError::UnsupportedExpression {
			backend: DatabaseBackend::Sqlite,
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
				QueryBinaryOperator::NotEqual => "!=",
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
		QueryExpr::Literal(QueryLiteral::Boolean(value)) => Ok(if *value { String::from("1") } else { String::from("0") }),
		QueryExpr::Literal(QueryLiteral::Date(value)) => Ok(quote_text_literal(&value.to_string())),
		QueryExpr::Literal(QueryLiteral::Decimal(value)) => Ok(value.to_string()),
		QueryExpr::Literal(QueryLiteral::Integer(value)) => Ok(value.to_string()),
		QueryExpr::Literal(QueryLiteral::Text(value)) => Ok(quote_text_literal(value)),
		QueryExpr::Literal(QueryLiteral::Time(value)) => Ok(quote_text_literal(&value.to_string())),
		QueryExpr::Literal(QueryLiteral::TimeTz(value)) => Ok(quote_text_literal(&value.to_string())),
		QueryExpr::Literal(QueryLiteral::Timestamp(value)) => Ok(quote_text_literal(&value.to_string())),
		QueryExpr::Literal(QueryLiteral::TimestampTz(value)) => Ok(quote_text_literal(&value.to_string())),
		QueryExpr::Parameter(parameter) => {
			let index = parameters.len() as u32 + 1;
			parameters.push(SqlParameter {
				data_type: parameter.data_type.clone(),
				field_path: parameter.field_path.clone(),
				index,
				slot: parameter.slot,
			});
			Ok(format!("?{index}"))
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
	format: &str,
	argument: &QueryExpr,
	parameters: &mut Vec<SqlParameter>,
) -> Result<String, QueryLoweringError> {
	let value = lower_expression(argument, parameters)?;
	Ok(format!("CAST(STRFTIME('{format}', {value}) AS INTEGER)"))
}
