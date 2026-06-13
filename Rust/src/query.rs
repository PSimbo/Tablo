use crate::builtins::BuiltInFunction;
use crate::ast::DataType;
use crate::ast::FindKind;
use crate::ast::OrderByDirection;
use crate::schema::DatabaseBackend;
use crate::value::Date;
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
	ArrayLiteral(Vec<QueryExpr>),
	Binary(QueryBinaryExpr),
	BuiltInCall(QueryBuiltInCall),
	Column(QueryColumnReference),
	Literal(QueryLiteral),
	Parameter(QueryParameter),
	Unary(QueryUnaryExpr),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QueryLiteral {
	Boolean(bool),
	Date(Date),
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
pub enum SqlQueryResultShape {
	IntegerScalar,
	RecordPointer(Vec<QueryResultColumn>),
	RecordPointerArray(Vec<QueryResultColumn>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryBinaryExpr {
	pub left: Box<QueryExpr>,
	pub operator: QueryBinaryOperator,
	pub right: Box<QueryExpr>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryBuiltInCall {
	pub arguments: Vec<QueryExpr>,
	pub built_in: BuiltInFunction,
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
		let table_source = sqlite_table_source(&self.schema_name, &self.table_name, self.schema_is_implicit);

		let mut statement = format!("SELECT COUNT(*) FROM {table_source}");

		if let Some(filter) = &self.filter {
			statement.push_str(" WHERE ");
			statement.push_str(&lower_query_expr_sqlite(filter, &mut parameters));
		}

		SqlQuery {
			database_name: self.database_name.clone(),
			dialect: SqlDialect::Sqlite,
			parameters,
			result_shape: SqlQueryResultShape::IntegerScalar,
			statement,
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryFindPlan {
	pub backend: DatabaseBackend,
	pub database_name: String,
	pub filter: Option<QueryExpr>,
	pub kind: FindKind,
	pub order_by: Vec<QueryOrderByItem>,
	pub record_columns: Vec<QueryResultColumn>,
	pub schema_is_implicit: bool,
	pub schema_name: String,
	pub table_name: String,
}

impl QueryFindPlan {
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
		let table_source = sqlite_table_source(&self.schema_name, &self.table_name, self.schema_is_implicit);
		let select_columns = self.record_columns.iter()
			.map(|column| {
				format!(
					"{}.{}",
					quote_identifier(&self.table_name),
					quote_identifier(&column.column_name),
				)
			})
			.collect::<Vec<_>>()
			.join(", ");
		let mut statement = format!("SELECT {select_columns} FROM {table_source}");

		if let Some(filter) = &self.filter {
			statement.push_str(" WHERE ");
			statement.push_str(&lower_query_expr_sqlite(filter, &mut parameters));
		}

		if !self.order_by.is_empty() {
			statement.push_str(" ORDER BY ");
			let order_by = self.order_by.iter()
				.map(|item| {
					let expression = lower_query_expr_sqlite(&item.expression, &mut parameters);
					let direction = match effective_find_order_direction(self.kind, item.direction) {
						OrderByDirection::Ascending => "ASC",
						OrderByDirection::Descending => "DESC",
					};
					format!("{expression} {direction}")
				})
				.collect::<Vec<_>>()
				.join(", ");
			statement.push_str(&order_by);
		}

		statement.push_str(" LIMIT 1");

		SqlQuery {
			database_name: self.database_name.clone(),
			dialect: SqlDialect::Sqlite,
			parameters,
			result_shape: SqlQueryResultShape::RecordPointer(self.record_columns.clone()),
			statement,
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryForPlan {
	pub backend: DatabaseBackend,
	pub database_name: String,
	pub filter: Option<QueryExpr>,
	pub order_by: Vec<QueryOrderByItem>,
	pub record_columns: Vec<QueryResultColumn>,
	pub schema_is_implicit: bool,
	pub schema_name: String,
	pub table_name: String,
}

impl QueryForPlan {
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
		let table_source = sqlite_table_source(&self.schema_name, &self.table_name, self.schema_is_implicit);
		let select_columns = self.record_columns.iter()
			.map(|column| {
				format!(
					"{}.{}",
					quote_identifier(&self.table_name),
					quote_identifier(&column.column_name),
				)
			})
			.collect::<Vec<_>>()
			.join(", ");
		let mut statement = format!("SELECT {select_columns} FROM {table_source}");

		if let Some(filter) = &self.filter {
			statement.push_str(" WHERE ");
			statement.push_str(&lower_query_expr_sqlite(filter, &mut parameters));
		}

		if !self.order_by.is_empty() {
			statement.push_str(" ORDER BY ");
			let order_by = self.order_by.iter()
				.map(|item| {
					let expression = lower_query_expr_sqlite(&item.expression, &mut parameters);
					let direction = match item.direction {
						OrderByDirection::Ascending => "ASC",
						OrderByDirection::Descending => "DESC",
					};
					format!("{expression} {direction}")
				})
				.collect::<Vec<_>>()
				.join(", ");
			statement.push_str(&order_by);
		}

		SqlQuery {
			database_name: self.database_name.clone(),
			dialect: SqlDialect::Sqlite,
			parameters,
			result_shape: SqlQueryResultShape::RecordPointerArray(self.record_columns.clone()),
			statement,
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryOrderByItem {
	pub direction: OrderByDirection,
	pub expression: QueryExpr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryParameter {
	pub data_type: DataType,
	pub field_path: Vec<String>,
	pub slot: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryResultColumn {
	pub column_name: String,
	pub data_type: DataType,
	pub is_nullable: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryUnaryExpr {
	pub operand: Box<QueryExpr>,
	pub operator: QueryUnaryOperator,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SqlParameter {
	pub data_type: DataType,
	pub field_path: Vec<String>,
	pub index: u32,
	pub slot: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SqlQuery {
	pub database_name: String,
	pub dialect: SqlDialect,
	pub parameters: Vec<SqlParameter>,
	pub result_shape: SqlQueryResultShape,
	pub statement: String,
}

fn effective_find_order_direction(kind: FindKind, direction: OrderByDirection) -> OrderByDirection {
	match kind {
		FindKind::Any | FindKind::First => direction,
		FindKind::Last => match direction {
			OrderByDirection::Ascending => OrderByDirection::Descending,
			OrderByDirection::Descending => OrderByDirection::Ascending,
		},
	}
}

fn lower_query_expr_sqlite(expression: &QueryExpr, parameters: &mut Vec<SqlParameter>) -> String {
	match expression {
		QueryExpr::ArrayLiteral(_) => {
			panic!("Array literals are only supported within specific lowered SQLite query expressions.")
		}
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
		QueryExpr::BuiltInCall(call) => match call.built_in {
			BuiltInFunction::Contains => {
				match &call.arguments[0] {
					QueryExpr::ArrayLiteral(values) => {
						let right = lower_query_expr_sqlite(&call.arguments[1], parameters);
						let values = values.iter()
							.map(|value| lower_query_expr_sqlite(value, parameters))
							.collect::<Vec<_>>()
							.join(", ");
						format!("({right} IN ({values}))")
					}
					_ => {
						let left = lower_query_expr_sqlite(&call.arguments[0], parameters);
						let right = lower_query_expr_sqlite(&call.arguments[1], parameters);
						format!("(INSTR({left}, {right}) > 0)")
					}
				}
			}
			BuiltInFunction::CountOf => {
				let left = lower_query_expr_sqlite(&call.arguments[0], parameters);
				let right = lower_query_expr_sqlite(&call.arguments[1], parameters);
				format!(
					"CASE WHEN LENGTH({left}) = 0 THEN 0 ELSE ((LENGTH({right}) - LENGTH(REPLACE({right}, {left}, ''))) / LENGTH({left})) END"
				)
			}
			BuiltInFunction::IndexOf => {
				let left = lower_query_expr_sqlite(&call.arguments[0], parameters);
				let right = lower_query_expr_sqlite(&call.arguments[1], parameters);
				format!("NULLIF(INSTR({right}, {left}), 0)")
			}
			BuiltInFunction::Trim => {
				let value = lower_query_expr_sqlite(&call.arguments[0], parameters);
				format!("TRIM({value})")
			}
			other => panic!(
				"Built-in function `{}` is not supported in lowered SQLite query expressions.",
				other.name(),
			),
		},
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
		QueryExpr::Literal(QueryLiteral::Date(value)) => quote_text_literal(&value.to_string()),
		QueryExpr::Literal(QueryLiteral::Decimal(value)) => value.to_string(),
		QueryExpr::Literal(QueryLiteral::Integer(value)) => value.to_string(),
		QueryExpr::Literal(QueryLiteral::Text(value)) => quote_text_literal(value),
		QueryExpr::Parameter(parameter) => {
			let index = parameters.len() as u32 + 1;
			parameters.push(SqlParameter {
				data_type: parameter.data_type.clone(),
				field_path: parameter.field_path.clone(),
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

fn sqlite_table_source(schema_name: &str, table_name: &str, schema_is_implicit: bool) -> String {
	if schema_is_implicit {
		quote_identifier(table_name)
	}
	else {
		format!(
			"{}.{}",
			quote_identifier(schema_name),
			quote_identifier(table_name),
		)
	}
}

#[cfg(test)]
mod tests {
	use crate::ast::DataType;
	use crate::ast::FindKind;
	use crate::ast::OrderByDirection;
	use crate::builtins::BuiltInFunction;
	use crate::schema::DatabaseBackend;
	use crate::value::Decimal;

	use super::LoweredBackendQuery;
	use super::QueryBinaryExpr;
	use super::QueryBinaryOperator;
	use super::QueryBuiltInCall;
	use super::QueryColumnReference;
	use super::QueryCountPlan;
	use super::QueryExpr;
	use super::QueryFindPlan;
	use super::QueryForPlan;
	use super::QueryLiteral;
	use super::QueryOrderByItem;
	use super::QueryParameter;
	use super::QueryResultColumn;
	use super::QueryUnaryExpr;
	use super::QueryUnaryOperator;
	use super::SqlDialect;
	use super::SqlParameter;
	use super::SqlQuery;
	use super::SqlQueryResultShape;

	#[test]
	fn lowers_sqlite_built_in_text_functions() {
		let query = QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::BuiltInCall(QueryBuiltInCall {
				arguments: vec![
					QueryExpr::BuiltInCall(QueryBuiltInCall {
						arguments: vec![
							QueryExpr::Column(QueryColumnReference {
								column_name: String::from("Name"),
								data_type: DataType::Text,
								table_name: String::from("Customers"),
							}),
						],
						built_in: BuiltInFunction::Trim,
					}),
					QueryExpr::Literal(QueryLiteral::Text(String::from("Ada"))),
				],
				built_in: BuiltInFunction::Contains,
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			statement: String::from(
				"SELECT COUNT(*) FROM \"Customers\" WHERE (INSTR(TRIM(\"Customers\".\"Name\"), 'Ada') > 0)"
			),
		}));
	}

	#[test]
	fn lowers_sqlite_contains_with_array_literal_to_in_expression() {
		let query = QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::BuiltInCall(QueryBuiltInCall {
				arguments: vec![
					QueryExpr::ArrayLiteral(vec![
						QueryExpr::Literal(QueryLiteral::Text(String::from("ALPHA"))),
						QueryExpr::Literal(QueryLiteral::Text(String::from("BRAVO"))),
					]),
					QueryExpr::Column(QueryColumnReference {
						column_name: String::from("Code"),
						data_type: DataType::Text,
						table_name: String::from("Tbl"),
					}),
				],
				built_in: BuiltInFunction::Contains,
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Tbl"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			statement: String::from(
				"SELECT COUNT(*) FROM \"Tbl\" WHERE (\"Tbl\".\"Code\" IN ('ALPHA', 'BRAVO'))"
			),
		}));
	}

	#[test]
	fn lowers_sqlite_countof_and_indexof_text_functions() {
		let query = QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::BuiltInCall(QueryBuiltInCall {
					arguments: vec![
						QueryExpr::Literal(QueryLiteral::Text(String::from("Ada"))),
						QueryExpr::Column(QueryColumnReference {
							column_name: String::from("Name"),
							data_type: DataType::Text,
							table_name: String::from("Customers"),
						}),
					],
					built_in: BuiltInFunction::CountOf,
				})),
				operator: QueryBinaryOperator::GreaterThan,
				right: Box::new(QueryExpr::BuiltInCall(QueryBuiltInCall {
					arguments: vec![
						QueryExpr::Literal(QueryLiteral::Text(String::from("A"))),
						QueryExpr::Column(QueryColumnReference {
							column_name: String::from("Name"),
							data_type: DataType::Text,
							table_name: String::from("Customers"),
						}),
					],
					built_in: BuiltInFunction::IndexOf,
				})),
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			statement: String::from(
				"SELECT COUNT(*) FROM \"Customers\" WHERE (CASE WHEN LENGTH('Ada') = 0 THEN 0 ELSE ((LENGTH(\"Customers\".\"Name\") - LENGTH(REPLACE(\"Customers\".\"Name\", 'Ada', ''))) / LENGTH('Ada')) END > NULLIF(INSTR(\"Customers\".\"Name\", 'A'), 0))"
			),
		}));
	}

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
						field_path: Vec::new(),
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
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			parameters: vec![
				SqlParameter {
					data_type: DataType::Int,
					field_path: Vec::new(),
					index: 1,
					slot: 3,
				},
			],
			result_shape: SqlQueryResultShape::IntegerScalar,
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
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			statement: String::from(
				"SELECT COUNT(*) FROM \"Reporting\".\"Metrics\" WHERE (12.50 = 'hi ''there''')"
			),
		}));
	}

	#[test]
	fn lowers_sqlite_find_plan_to_sql_query() {
		let query = QueryFindPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Column(QueryColumnReference {
					column_name: String::from("Active"),
					data_type: DataType::Bool,
					table_name: String::from("Customers"),
				})),
				operator: QueryBinaryOperator::Equal,
				right: Box::new(QueryExpr::Literal(QueryLiteral::Boolean(true))),
			})),
			kind: FindKind::Last,
			order_by: vec![
				QueryOrderByItem {
					direction: OrderByDirection::Ascending,
					expression: QueryExpr::Column(QueryColumnReference {
						column_name: String::from("Name"),
						data_type: DataType::Text,
						table_name: String::from("Customers"),
					}),
				},
			],
			record_columns: vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
				},
				QueryResultColumn {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					is_nullable: false,
				},
			],
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			parameters: vec![],
			result_shape: SqlQueryResultShape::RecordPointer(vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
				},
				QueryResultColumn {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					is_nullable: false,
				},
			]),
			statement: String::from(
				"SELECT \"Customers\".\"Id\", \"Customers\".\"Name\" FROM \"Customers\" WHERE (\"Customers\".\"Active\" = 1) ORDER BY \"Customers\".\"Name\" DESC LIMIT 1"
			),
		}));
	}

	#[test]
	fn lowers_sqlite_for_record_plan_to_sql_query() {
		let query = QueryForPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Column(QueryColumnReference {
					column_name: String::from("Active"),
					data_type: DataType::Bool,
					table_name: String::from("Customers"),
				})),
				operator: QueryBinaryOperator::Equal,
				right: Box::new(QueryExpr::Literal(QueryLiteral::Boolean(true))),
			})),
			order_by: vec![
				QueryOrderByItem {
					direction: OrderByDirection::Descending,
					expression: QueryExpr::Column(QueryColumnReference {
						column_name: String::from("Name"),
						data_type: DataType::Text,
						table_name: String::from("Customers"),
					}),
				},
			],
			record_columns: vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
				},
				QueryResultColumn {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					is_nullable: false,
				},
			],
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			parameters: vec![],
			result_shape: SqlQueryResultShape::RecordPointerArray(vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
				},
				QueryResultColumn {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					is_nullable: false,
				},
			]),
			statement: String::from(
				"SELECT \"Customers\".\"Id\", \"Customers\".\"Name\" FROM \"Customers\" WHERE (\"Customers\".\"Active\" = 1) ORDER BY \"Customers\".\"Name\" DESC"
			),
		}));
	}
}
