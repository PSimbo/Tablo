use crate::ast::DataType;
use crate::schema::DatabaseBackend;
use crate::value::Decimal;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LoweredBackendQuery {
	Sql(SqlQuery),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QueryBinaryOperator {
	Add,
	And,
	Divide,
	Equal,
	GreaterThan,
	GreaterThanOrEqual,
	LessThan,
	LessThanOrEqual,
	Modulo,
	Multiply,
	NotEqual,
	Or,
	Subtract,
	Xor,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QueryExpr {
	Binary(QueryBinaryExpr),
	Column(QueryColumnReference),
	Literal(QueryLiteral),
	Parameter(QueryParameter),
	Unary(QueryUnaryExpr),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QueryLiteral {
	Boolean(bool),
	Decimal(Decimal),
	Integer(i64),
	Text(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QueryLoweringError {
	UnsupportedBackend {
		backend: DatabaseBackend,
	},
	UnsupportedOperator {
		backend: DatabaseBackend,
		operator: QueryBinaryOperator,
	},
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QueryUnaryOperator {
	Negate,
	Not,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SqlDialect {
	Sqlite,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryBinaryExpr {
	pub left: Box<QueryExpr>,
	pub operator: QueryBinaryOperator,
	pub right: Box<QueryExpr>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryColumnReference {
	pub column_name: String,
	pub data_type: DataType,
	pub table_name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryCountPlan {
	pub backend: DatabaseBackend,
	pub database_name: String,
	pub filter: Option<QueryExpr>,
	pub schema_is_implicit: bool,
	pub schema_name: String,
	pub table_name: String,
}

impl QueryCountPlan {
	pub fn lower_to_backend(&self) -> Result<LoweredBackendQuery, QueryLoweringError> {
		match self.backend {
			DatabaseBackend::Sqlite => Ok(LoweredBackendQuery::Sql(self.lower_to_sqlite())),
			other => Err(QueryLoweringError::UnsupportedBackend {
				backend: other,
			}),
		}
	}

	fn lower_to_sqlite(&self) -> SqlQuery {
		let mut parameters = Vec::new();
		let table_source = if self.schema_is_implicit {
			quote_identifier(&self.table_name)
		}
		else {
			format!(
				"{}.{}",
				quote_identifier(&self.schema_name),
				quote_identifier(&self.table_name),
			)
		};

		let mut statement = format!("SELECT COUNT(*) FROM {table_source}");

		if let Some(filter) = &self.filter {
			statement.push_str(" WHERE ");
			statement.push_str(&lower_query_expr_sqlite(filter, &mut parameters));
		}

		SqlQuery {
			dialect: SqlDialect::Sqlite,
			parameters,
			statement,
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryParameter {
	pub data_type: DataType,
	pub slot: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryUnaryExpr {
	pub operand: Box<QueryExpr>,
	pub operator: QueryUnaryOperator,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SqlParameter {
	pub data_type: DataType,
	pub index: u32,
	pub slot: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SqlQuery {
	pub dialect: SqlDialect,
	pub parameters: Vec<SqlParameter>,
	pub statement: String,
}

fn lower_query_expr_sqlite(expression: &QueryExpr, parameters: &mut Vec<SqlParameter>) -> String {
	match expression {
		QueryExpr::Binary(binary) => {
			let left = lower_query_expr_sqlite(&binary.left, parameters);
			let right = lower_query_expr_sqlite(&binary.right, parameters);
			let operator = match binary.operator {
				QueryBinaryOperator::Add => "+",
				QueryBinaryOperator::And => "AND",
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
					let xor = format!("(({left}) AND NOT ({right})) OR (NOT ({left}) AND ({right}))");
					return format!("({xor})");
				}
			};

			format!("({left} {operator} {right})")
		}
		QueryExpr::Column(column) => {
			format!(
				"{}.{}",
				quote_identifier(&column.table_name),
				quote_identifier(&column.column_name),
			)
		}
		QueryExpr::Literal(QueryLiteral::Boolean(value)) => {
			if *value { String::from("1") } else { String::from("0") }
		}
		QueryExpr::Literal(QueryLiteral::Decimal(value)) => value.to_string(),
		QueryExpr::Literal(QueryLiteral::Integer(value)) => value.to_string(),
		QueryExpr::Literal(QueryLiteral::Text(value)) => quote_text_literal(value),
		QueryExpr::Parameter(parameter) => {
			let index = parameters.len() as u32 + 1;
			parameters.push(SqlParameter {
				data_type: parameter.data_type.clone(),
				index,
				slot: parameter.slot,
			});
			format!("?{index}")
		}
		QueryExpr::Unary(unary) => match unary.operator {
			QueryUnaryOperator::Negate => {
				let operand = lower_query_expr_sqlite(&unary.operand, parameters);
				format!("(-{operand})")
			}
			QueryUnaryOperator::Not => {
				let operand = lower_query_expr_sqlite(&unary.operand, parameters);
				format!("(NOT {operand})")
			}
		},
	}
}

fn quote_identifier(identifier: &str) -> String {
	format!("\"{}\"", identifier.replace('"', "\"\""))
}

fn quote_text_literal(value: &str) -> String {
	format!("'{}'", value.replace('\'', "''"))
}

#[cfg(test)]
mod tests {
	use crate::ast::DataType;
	use crate::schema::DatabaseBackend;
	use crate::value::Decimal;

	use super::LoweredBackendQuery;
	use super::QueryBinaryExpr;
	use super::QueryBinaryOperator;
	use super::QueryColumnReference;
	use super::QueryCountPlan;
	use super::QueryExpr;
	use super::QueryLiteral;
	use super::QueryParameter;
	use super::QueryUnaryExpr;
	use super::QueryUnaryOperator;
	use super::SqlDialect;
	use super::SqlParameter;
	use super::SqlQuery;

	#[test]
	fn lowers_sqlite_count_plan_to_sql_query() {
		let query = QueryCountPlan {
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
						slot: 3,
					})),
				})),
				operator: QueryBinaryOperator::And,
				right: Box::new(QueryExpr::Unary(QueryUnaryExpr {
					operand: Box::new(QueryExpr::Binary(QueryBinaryExpr {
						left: Box::new(QueryExpr::Column(QueryColumnReference {
							column_name: String::from("Active"),
							data_type: DataType::Bool,
							table_name: String::from("Customers"),
						})),
						operator: QueryBinaryOperator::Equal,
						right: Box::new(QueryExpr::Literal(QueryLiteral::Boolean(false))),
					})),
					operator: QueryUnaryOperator::Not,
				})),
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			dialect: SqlDialect::Sqlite,
			parameters: vec![
				SqlParameter {
					data_type: DataType::Int,
					index: 1,
					slot: 3,
				},
			],
			statement: String::from(
				"SELECT COUNT(*) FROM \"Customers\" WHERE ((\"Customers\".\"Id\" = ?1) AND (NOT (\"Customers\".\"Active\" = 0)))"
			),
		}));
	}

	#[test]
	fn lowers_sqlite_decimal_and_text_literals() {
		let query = QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Literal(QueryLiteral::Decimal(
					Decimal::from_literal("12.50").unwrap(),
				))),
				operator: QueryBinaryOperator::Equal,
				right: Box::new(QueryExpr::Literal(QueryLiteral::Text(String::from("hi 'there'")))),
			})),
			schema_is_implicit: false,
			schema_name: String::from("Reporting"),
			table_name: String::from("Metrics"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			dialect: SqlDialect::Sqlite,
			parameters: vec![],
			statement: String::from(
				"SELECT COUNT(*) FROM \"Reporting\".\"Metrics\" WHERE (12.50 = 'hi ''there''')"
			),
		}));
	}
}
