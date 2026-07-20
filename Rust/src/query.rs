use crate::ast::DataType;
use crate::ast::FindKind;
use crate::ast::OrderByDirection;
use crate::builtins::BuiltInFunction;
use crate::schema::DatabaseBackend;
use crate::value::Date;
use crate::value::Decimal;
use crate::value::Time;
use crate::value::TimeTz;
use crate::value::Timestamp;
use crate::value::TimestampTz;

mod postgresql;
mod sqlite;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LoweredBackendQuery {
	Sql(SqlQuery),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QueryBinaryOperator {
	Add,
	And,
	Concatenate,
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
	Time(Time),
	TimeTz(TimeTz),
	Timestamp(Timestamp),
	TimestampTz(TimestampTz),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QueryLoweringError {
	UnsupportedBackend {
		backend: DatabaseBackend,
	},
	UnsupportedBuiltIn {
		backend: DatabaseBackend,
		built_in: BuiltInFunction,
	},
	UnsupportedExpression {
		backend: DatabaseBackend,
		description: String,
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
	MySql,
	PostgreSql,
	Sqlite,
}

impl SqlDialect {
	pub fn backend(self) -> DatabaseBackend {
		match self {
			Self::MySql => DatabaseBackend::MySql,
			Self::PostgreSql => DatabaseBackend::PostgreSql,
			Self::Sqlite => DatabaseBackend::Sqlite,
		}
	}
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryForPlan {
	pub backend: DatabaseBackend,
	pub database_name: String,
	pub filter: Option<QueryExpr>,
	pub group_by: Vec<QueryGroupByItem>,
	pub limit: Option<QueryExpr>,
	pub order_by: Vec<QueryOrderByItem>,
	pub record_columns: Vec<QueryResultColumn>,
	pub schema_is_implicit: bool,
	pub schema_name: String,
	pub table_name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryGroupByItem {
	pub alias: Option<String>,
	pub data_type: DataType,
	pub expression: QueryExpr,
	pub key_names: Vec<String>,
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
	pub is_primary_key: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryUnaryExpr {
	pub operand: Box<QueryExpr>,
	pub operator: QueryUnaryOperator,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SqlGroupByItem {
	pub data_type: DataType,
	pub key_names: Vec<String>,
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
	pub group_by: Vec<SqlGroupByItem>,
	pub parameters: Vec<SqlParameter>,
	pub result_shape: SqlQueryResultShape,
	pub schema_is_implicit: bool,
	pub schema_name: String,
	pub statement: String,
	pub table_name: String,
}

pub fn lower_count_query(plan: &QueryCountPlan) -> Result<LoweredBackendQuery, QueryLoweringError> {
	match plan.backend {
		DatabaseBackend::PostgreSql => postgresql::lower_count(plan).map(LoweredBackendQuery::Sql),
		DatabaseBackend::Sqlite => sqlite::lower_count(plan).map(LoweredBackendQuery::Sql),
		backend => Err(QueryLoweringError::UnsupportedBackend { backend }),
	}
}

pub fn lower_find_query(plan: &QueryFindPlan) -> Result<LoweredBackendQuery, QueryLoweringError> {
	match plan.backend {
		DatabaseBackend::PostgreSql => postgresql::lower_find(plan).map(LoweredBackendQuery::Sql),
		DatabaseBackend::Sqlite => sqlite::lower_find(plan).map(LoweredBackendQuery::Sql),
		backend => Err(QueryLoweringError::UnsupportedBackend { backend }),
	}
}

pub fn lower_for_query(plan: &QueryForPlan) -> Result<LoweredBackendQuery, QueryLoweringError> {
	match plan.backend {
		DatabaseBackend::Sqlite => sqlite::lower_for(plan).map(LoweredBackendQuery::Sql),
		backend => Err(QueryLoweringError::UnsupportedBackend { backend }),
	}
}

pub(super) fn effective_find_order_direction(kind: FindKind, direction: OrderByDirection) -> OrderByDirection {
	match kind {
		FindKind::Any | FindKind::First => direction,
		FindKind::Last => match direction {
			OrderByDirection::Ascending => OrderByDirection::Descending,
			OrderByDirection::Descending => OrderByDirection::Ascending,
		},
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
	use super::QueryGroupByItem;
	use super::QueryLiteral;
	use super::QueryOrderByItem;
	use super::QueryParameter;
	use super::QueryResultColumn;
	use super::QueryUnaryExpr;
	use super::QueryUnaryOperator;
	use super::SqlDialect;
	use super::SqlGroupByItem;
	use super::SqlParameter;
	use super::SqlQuery;
	use super::SqlQueryResultShape;
	use super::lower_count_query;
	use super::lower_find_query;
	use super::lower_for_query;

	trait TestQueryPlan {
		fn lower_to_backend(&self) -> Result<LoweredBackendQuery, super::QueryLoweringError>;
	}

	impl TestQueryPlan for QueryCountPlan {
		fn lower_to_backend(&self) -> Result<LoweredBackendQuery, super::QueryLoweringError> {
			lower_count_query(self)
		}
	}

	impl TestQueryPlan for QueryFindPlan {
		fn lower_to_backend(&self) -> Result<LoweredBackendQuery, super::QueryLoweringError> {
			lower_find_query(self)
		}
	}

	impl TestQueryPlan for QueryForPlan {
		fn lower_to_backend(&self) -> Result<LoweredBackendQuery, super::QueryLoweringError> {
			lower_for_query(self)
		}
	}

	#[test]
	fn lowers_postgresql_contains_with_array_literal_to_in_expression() {
		let query = QueryCountPlan {
			backend: DatabaseBackend::PostgreSql,
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
						table_name: String::from("Items"),
					}),
				],
				built_in: BuiltInFunction::Contains,
			})),
			schema_is_implicit: true,
			schema_name: String::from("public"),
			table_name: String::from("Items"),
		}.lower_to_backend().unwrap();

		let LoweredBackendQuery::Sql(query) = query;
		assert_eq!(
			query.statement,
			"SELECT COUNT(*) FROM \"Items\" WHERE (\"Items\".\"Code\" IN ('ALPHA', 'BRAVO'))",
		);
	}

	#[test]
	fn lowers_postgresql_count_plan_with_numbered_parameters() {
		let query = QueryCountPlan {
			backend: DatabaseBackend::PostgreSql,
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
						field_path: vec![String::from("MinimumId")],
						slot: 3,
					})),
				})),
				operator: QueryBinaryOperator::And,
				right: Box::new(QueryExpr::Binary(QueryBinaryExpr {
					left: Box::new(QueryExpr::Column(QueryColumnReference {
						column_name: String::from("Active"),
						data_type: DataType::Bool,
						table_name: String::from("Customers"),
					})),
					operator: QueryBinaryOperator::Equal,
					right: Box::new(QueryExpr::Literal(QueryLiteral::Boolean(true))),
				})),
			})),
			schema_is_implicit: false,
			schema_name: String::from("Reporting"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::PostgreSql,
			group_by: vec![],
			parameters: vec![
				SqlParameter {
					data_type: DataType::Int,
					field_path: vec![String::from("MinimumId")],
					index: 1,
					slot: 3,
				},
			],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: false,
			schema_name: String::from("Reporting"),
			statement: String::from(
				"SELECT COUNT(*) FROM \"Reporting\".\"Customers\" WHERE ((\"Customers\".\"Id\" = $1) AND (\"Customers\".\"Active\" = TRUE))"
			),
			table_name: String::from("Customers"),
		}));
	}

	#[test]
	fn lowers_postgresql_find_last_plan_with_reversed_ordering() {
		let record_columns = vec![
			QueryResultColumn {
				column_name: String::from("Id"),
				data_type: DataType::Int,
				is_nullable: false,
				is_primary_key: true,
			},
			QueryResultColumn {
				column_name: String::from("Name"),
				data_type: DataType::Text,
				is_nullable: false,
				is_primary_key: false,
			},
		];
		let query = QueryFindPlan {
			backend: DatabaseBackend::PostgreSql,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Column(QueryColumnReference {
					column_name: String::from("Active"),
					data_type: DataType::Bool,
					table_name: String::from("Customers"),
				})),
				operator: QueryBinaryOperator::Equal,
				right: Box::new(QueryExpr::Parameter(QueryParameter {
					data_type: DataType::Bool,
					field_path: vec![],
					slot: 4,
				})),
			})),
			kind: FindKind::Last,
			order_by: vec![QueryOrderByItem {
				direction: OrderByDirection::Ascending,
				expression: QueryExpr::Column(QueryColumnReference {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					table_name: String::from("Customers"),
				}),
			}],
			record_columns: record_columns.clone(),
			schema_is_implicit: false,
			schema_name: String::from("Public"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::PostgreSql,
			group_by: vec![],
			parameters: vec![SqlParameter {
				data_type: DataType::Bool,
				field_path: vec![],
				index: 1,
				slot: 4,
			}],
			result_shape: SqlQueryResultShape::RecordPointer(record_columns),
			schema_is_implicit: false,
			schema_name: String::from("Public"),
			statement: String::from(
				"SELECT \"Customers\".\"Id\", \"Customers\".\"Name\" FROM \"Public\".\"Customers\" WHERE (\"Customers\".\"Active\" = $1) ORDER BY \"Customers\".\"Name\" DESC LIMIT 1"
			),
			table_name: String::from("Customers"),
		}));
	}

	#[test]
	fn lowers_postgresql_text_and_temporal_expressions() {
		let query = QueryCountPlan {
			backend: DatabaseBackend::PostgreSql,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::Binary(QueryBinaryExpr {
					left: Box::new(QueryExpr::Binary(QueryBinaryExpr {
						left: Box::new(QueryExpr::BuiltInCall(QueryBuiltInCall {
							arguments: vec![QueryExpr::Column(QueryColumnReference {
								column_name: String::from("Name"),
								data_type: DataType::Text,
								table_name: String::from("Customers"),
							})],
							built_in: BuiltInFunction::Trim,
						})),
						operator: QueryBinaryOperator::Concatenate,
						right: Box::new(QueryExpr::Literal(QueryLiteral::Text(String::from(" Ltd.")))),
					})),
					operator: QueryBinaryOperator::Equal,
					right: Box::new(QueryExpr::Literal(QueryLiteral::Text(String::from("Acme Ltd.")))),
				})),
				operator: QueryBinaryOperator::And,
				right: Box::new(QueryExpr::Binary(QueryBinaryExpr {
					left: Box::new(QueryExpr::BuiltInCall(QueryBuiltInCall {
						arguments: vec![QueryExpr::Column(QueryColumnReference {
							column_name: String::from("Created"),
							data_type: DataType::Date,
							table_name: String::from("Customers"),
						})],
						built_in: BuiltInFunction::Year,
					})),
					operator: QueryBinaryOperator::GreaterThanOrEqual,
					right: Box::new(QueryExpr::Literal(QueryLiteral::Integer(2020))),
				})),
			})),
			schema_is_implicit: true,
			schema_name: String::from("public"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		let LoweredBackendQuery::Sql(query) = query;
		assert_eq!(query.dialect, SqlDialect::PostgreSql);
		assert_eq!(
			query.statement,
			"SELECT COUNT(*) FROM \"Customers\" WHERE (((TRIM(\"Customers\".\"Name\") || ' Ltd.') = 'Acme Ltd.') AND (CAST(TRUNC(EXTRACT(YEAR FROM \"Customers\".\"Created\")) AS BIGINT) >= 2020))",
		);
	}

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
			group_by: vec![],
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from(
				"SELECT COUNT(*) FROM \"Customers\" WHERE (INSTR(TRIM(\"Customers\".\"Name\"), 'Ada') > 0)"
			),
			table_name: String::from("Customers"),
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
			group_by: vec![],
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from(
				"SELECT COUNT(*) FROM \"Tbl\" WHERE (\"Tbl\".\"Code\" IN ('ALPHA', 'BRAVO'))"
			),
			table_name: String::from("Tbl"),
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
			group_by: vec![],
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from(
				"SELECT COUNT(*) FROM \"Customers\" WHERE (CASE WHEN LENGTH('Ada') = 0 THEN 0 ELSE ((LENGTH(\"Customers\".\"Name\") - LENGTH(REPLACE(\"Customers\".\"Name\", 'Ada', ''))) / LENGTH('Ada')) END > NULLIF(INSTR(\"Customers\".\"Name\", 'A'), 0))"
			),
			table_name: String::from("Customers"),
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
			group_by: vec![],
			parameters: vec![
				SqlParameter {
					data_type: DataType::Int,
					field_path: Vec::new(),
					index: 1,
					slot: 3,
				},
			],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from(
				"SELECT COUNT(*) FROM \"Customers\" WHERE ((\"Customers\".\"Id\" = ?1) AND (NOT (\"Customers\".\"Active\" = 0)))"
			),
			table_name: String::from("Customers"),
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
			group_by: vec![],
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: false,
			schema_name: String::from("Reporting"),
			statement: String::from(
				"SELECT COUNT(*) FROM \"Reporting\".\"Metrics\" WHERE (12.50 = 'hi ''there''')"
			),
			table_name: String::from("Metrics"),
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
					is_primary_key: true,
				},
				QueryResultColumn {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					is_nullable: false,
					is_primary_key: false,
				},
			],
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			group_by: vec![],
			parameters: vec![],
			result_shape: SqlQueryResultShape::RecordPointer(vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
					is_primary_key: true,
				},
				QueryResultColumn {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					is_nullable: false,
					is_primary_key: false,
				},
			]),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from(
				"SELECT \"Customers\".\"Id\", \"Customers\".\"Name\" FROM \"Customers\" WHERE (\"Customers\".\"Active\" = 1) ORDER BY \"Customers\".\"Name\" DESC LIMIT 1"
			),
			table_name: String::from("Customers"),
		}));
	}

	#[test]
	fn lowers_sqlite_for_record_group_by_plan_to_ordered_sql_query() {
		let query = QueryForPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: None,
			group_by: vec![
				QueryGroupByItem {
					alias: Some(String::from("country")),
					data_type: DataType::Text,
					expression: QueryExpr::Column(QueryColumnReference {
						column_name: String::from("Country"),
						data_type: DataType::Text,
						table_name: String::from("Customers"),
					}),
					key_names: vec![String::from("country"), String::from("Country")],
				},
				QueryGroupByItem {
					alias: None,
					data_type: DataType::Text,
					expression: QueryExpr::Column(QueryColumnReference {
						column_name: String::from("City"),
						data_type: DataType::Text,
						table_name: String::from("Customers"),
					}),
					key_names: vec![String::from("City")],
				},
			],
			limit: None,
			order_by: vec![],
			record_columns: vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
					is_primary_key: true,
				},
			],
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			group_by: vec![
				SqlGroupByItem {
					data_type: DataType::Text,
					key_names: vec![String::from("country"), String::from("Country")],
				},
				SqlGroupByItem {
					data_type: DataType::Text,
					key_names: vec![String::from("City")],
				},
			],
			parameters: vec![],
			result_shape: SqlQueryResultShape::RecordPointerArray(vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
					is_primary_key: true,
				},
			]),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from(
				"SELECT \"Customers\".\"Id\", \"Customers\".\"Country\", \"Customers\".\"City\" FROM \"Customers\" ORDER BY \"Customers\".\"Country\", \"Customers\".\"City\""
			),
			table_name: String::from("Customers"),
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
			group_by: vec![],
			limit: None,
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
					is_primary_key: true,
				},
				QueryResultColumn {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					is_nullable: false,
					is_primary_key: false,
				},
			],
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			group_by: vec![],
			parameters: vec![],
			result_shape: SqlQueryResultShape::RecordPointerArray(vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
					is_primary_key: true,
				},
				QueryResultColumn {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					is_nullable: false,
					is_primary_key: false,
				},
			]),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from(
				"SELECT \"Customers\".\"Id\", \"Customers\".\"Name\" FROM \"Customers\" WHERE (\"Customers\".\"Active\" = 1) ORDER BY \"Customers\".\"Name\" DESC"
			),
			table_name: String::from("Customers"),
		}));
	}

	#[test]
	fn lowers_sqlite_for_record_plan_with_limit_to_sql_query() {
		let query = QueryForPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: None,
			group_by: vec![],
			limit: Some(QueryExpr::Parameter(QueryParameter {
				data_type: DataType::Int,
				field_path: vec![],
				slot: 2,
			})),
			order_by: vec![],
			record_columns: vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
					is_primary_key: true,
				},
			],
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			group_by: vec![],
			parameters: vec![
				SqlParameter {
					data_type: DataType::Int,
					field_path: vec![],
					index: 1,
					slot: 2,
				},
			],
			result_shape: SqlQueryResultShape::RecordPointerArray(vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
					is_primary_key: true,
				},
			]),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from(
				"SELECT \"Customers\".\"Id\" FROM \"Customers\" LIMIT max((?1), 0)"
			),
			table_name: String::from("Customers"),
		}));
	}

	#[test]
	fn lowers_sqlite_temporal_extractor_built_in_functions() {
		let query = QueryCountPlan {
			backend: DatabaseBackend::Sqlite,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::BuiltInCall(QueryBuiltInCall {
				arguments: vec![
					QueryExpr::Parameter(QueryParameter {
						data_type: DataType::Date,
						field_path: vec![String::from("When")],
						slot: 5,
					}),
				],
				built_in: BuiltInFunction::Year,
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			group_by: vec![],
			parameters: vec![
				SqlParameter {
					data_type: DataType::Date,
					field_path: vec![String::from("When")],
					index: 1,
					slot: 5,
				},
			],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from(
				"SELECT COUNT(*) FROM \"Customers\" WHERE CAST(STRFTIME('%Y', ?1) AS INTEGER)"
			),
			table_name: String::from("Customers"),
		}));
	}
}
