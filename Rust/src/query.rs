use crate::ast::*;
use crate::builtins::BuiltInFunction;
use crate::schema::DatabaseBackend;
use crate::value::*;

mod mysql;
mod postgresql;
mod sql_renderer;
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
	IntegerDivide,
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
	pub limit: Option<QueryParameter>,
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
		DatabaseBackend::MySql => sql_renderer::lower_count(&mysql::MySqlRenderer, plan).map(LoweredBackendQuery::Sql),
		DatabaseBackend::PostgreSql => sql_renderer::lower_count(&postgresql::PostgreSqlRenderer, plan).map(LoweredBackendQuery::Sql),
		DatabaseBackend::Sqlite => sql_renderer::lower_count(&sqlite::SqliteRenderer, plan).map(LoweredBackendQuery::Sql),
	}
}

pub fn lower_find_query(plan: &QueryFindPlan) -> Result<LoweredBackendQuery, QueryLoweringError> {
	match plan.backend {
		DatabaseBackend::MySql => sql_renderer::lower_find(&mysql::MySqlRenderer, plan).map(LoweredBackendQuery::Sql),
		DatabaseBackend::PostgreSql => sql_renderer::lower_find(&postgresql::PostgreSqlRenderer, plan).map(LoweredBackendQuery::Sql),
		DatabaseBackend::Sqlite => sql_renderer::lower_find(&sqlite::SqliteRenderer, plan).map(LoweredBackendQuery::Sql),
	}
}

pub fn lower_for_query(plan: &QueryForPlan) -> Result<LoweredBackendQuery, QueryLoweringError> {
	match plan.backend {
		DatabaseBackend::MySql => sql_renderer::lower_for(&mysql::MySqlRenderer, plan).map(LoweredBackendQuery::Sql),
		DatabaseBackend::PostgreSql => sql_renderer::lower_for(&postgresql::PostgreSqlRenderer, plan).map(LoweredBackendQuery::Sql),
		DatabaseBackend::Sqlite => sql_renderer::lower_for(&sqlite::SqliteRenderer, plan).map(LoweredBackendQuery::Sql),
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
	use super::*;

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
	fn lowers_mysql_count_plan_with_dialect_specific_expressions() {
		let query = QueryCountPlan {
			backend: DatabaseBackend::MySql,
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
					right: Box::new(QueryExpr::Parameter(QueryParameter {
						data_type: DataType::Int,
						field_path: vec![],
						slot: 7,
					})),
				})),
			})),
			schema_is_implicit: false,
			schema_name: String::from("Reporting"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::MySql,
			group_by: vec![],
			parameters: vec![SqlParameter {
				data_type: DataType::Int,
				field_path: vec![],
				index: 1,
				slot: 7,
			}],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: false,
			schema_name: String::from("Reporting"),
			statement: String::from(
				"SELECT COUNT(*) FROM `Reporting`.`Customers` WHERE ((CONCAT(TRIM(`Customers`.`Name`), ' Ltd.') = 'Acme Ltd.') AND (CAST(FLOOR(EXTRACT(YEAR FROM `Customers`.`Created`)) AS SIGNED) >= ?))"
			),
			table_name: String::from("Customers"),
		}));
	}

	#[test]
	fn lowers_mysql_find_last_plan_with_reversed_ordering() {
		let record_columns = vec![QueryResultColumn {
			column_name: String::from("Id"),
			data_type: DataType::Int,
			is_nullable: false,
			is_primary_key: true,
		}];
		let query = QueryFindPlan {
			backend: DatabaseBackend::MySql,
			database_name: String::from("ExampleDb"),
			filter: None,
			kind: FindKind::Last,
			order_by: vec![QueryOrderByItem {
				direction: OrderByDirection::Ascending,
				expression: QueryExpr::Column(QueryColumnReference {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					table_name: String::from("Customers"),
				}),
			}],
			record_columns: record_columns.clone(),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		let LoweredBackendQuery::Sql(query) = query;
		assert_eq!(query.dialect, SqlDialect::MySql);
		assert_eq!(query.result_shape, SqlQueryResultShape::RecordPointer(record_columns));
		assert_eq!(
			query.statement,
			"SELECT `Customers`.`Id` FROM `Customers` ORDER BY `Customers`.`Id` DESC LIMIT 1",
		);
	}

	#[test]
	fn lowers_mysql_grouped_for_record_plan_with_limit() {
		let query = QueryForPlan {
			backend: DatabaseBackend::MySql,
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
			group_by: vec![QueryGroupByItem {
				alias: Some(String::from("code")),
				data_type: DataType::Text,
				expression: QueryExpr::BuiltInCall(QueryBuiltInCall {
					arguments: vec![QueryExpr::Column(QueryColumnReference {
						column_name: String::from("Code"),
						data_type: DataType::Text,
						table_name: String::from("Items"),
					})],
					built_in: BuiltInFunction::Trim,
				}),
				key_names: vec![String::from("code")],
			}],
			limit: Some(QueryParameter {
				data_type: DataType::Int,
				field_path: vec![],
				slot: 8,
			}),
			order_by: vec![],
			record_columns: vec![QueryResultColumn {
				column_name: String::from("Id"),
				data_type: DataType::Int,
				is_nullable: false,
				is_primary_key: true,
			}],
			schema_is_implicit: false,
			schema_name: String::from("Inventory"),
			table_name: String::from("Items"),
		}.lower_to_backend().unwrap();

		let LoweredBackendQuery::Sql(query) = query;
		assert_eq!(query.dialect, SqlDialect::MySql);
		assert_eq!(query.parameters.len(), 1);
		assert_eq!(query.parameters[0].index, 1);
		assert_eq!(query.parameters[0].slot, 8);
		assert_eq!(
			query.statement,
			"SELECT `Items`.`Id`, TRIM(`Items`.`Code`) FROM `Inventory`.`Items` WHERE (`Items`.`Code` IN ('ALPHA', 'BRAVO')) ORDER BY TRIM(`Items`.`Code`) LIMIT ?",
		);
	}

	#[test]
	fn lowers_mysql_temporal_extractor_built_in_functions() {
		let cases = [
			(BuiltInFunction::Day, "DAY", DataType::Date, "CreatedDate"),
			(BuiltInFunction::Month, "MONTH", DataType::Date, "CreatedDate"),
			(BuiltInFunction::Year, "YEAR", DataType::Date, "CreatedDate"),
			(BuiltInFunction::Hour, "HOUR", DataType::Timestamp, "CreatedAt"),
			(BuiltInFunction::Minute, "MINUTE", DataType::Timestamp, "CreatedAt"),
			(BuiltInFunction::Second, "SECOND", DataType::Timestamp, "CreatedAt"),
		];

		for (built_in, part, data_type, column_name) in cases {
			let query = QueryCountPlan {
				backend: DatabaseBackend::MySql,
				database_name: String::from("ExampleDb"),
				filter: Some(QueryExpr::Binary(QueryBinaryExpr {
					left: Box::new(QueryExpr::BuiltInCall(QueryBuiltInCall {
						arguments: vec![QueryExpr::Column(QueryColumnReference {
							column_name: String::from(column_name),
							data_type,
							table_name: String::from("Events"),
						})],
						built_in,
					})),
					operator: QueryBinaryOperator::Equal,
					right: Box::new(QueryExpr::Literal(QueryLiteral::Integer(1))),
				})),
				schema_is_implicit: true,
				schema_name: String::from("Main"),
				table_name: String::from("Events"),
			}.lower_to_backend().unwrap();
			let LoweredBackendQuery::Sql(query) = query;

			assert_eq!(
				query.statement,
				format!(
					"SELECT COUNT(*) FROM `Events` WHERE (CAST(FLOOR(EXTRACT({part} FROM `Events`.`{column_name}`)) AS SIGNED) = 1)"
				),
			);
		}
	}

	#[test]
	fn lowers_mysql_text_search_built_in_functions() {
		let name_column = QueryExpr::Column(QueryColumnReference {
			column_name: String::from("Name"),
			data_type: DataType::Text,
			table_name: String::from("Customers"),
		});
		let query = QueryCountPlan {
			backend: DatabaseBackend::MySql,
			database_name: String::from("ExampleDb"),
			filter: Some(QueryExpr::Binary(QueryBinaryExpr {
				left: Box::new(QueryExpr::BuiltInCall(QueryBuiltInCall {
					arguments: vec![
						name_column.clone(),
						QueryExpr::Literal(QueryLiteral::Text(String::from("Ada"))),
					],
					built_in: BuiltInFunction::Contains,
				})),
				operator: QueryBinaryOperator::And,
				right: Box::new(QueryExpr::Binary(QueryBinaryExpr {
					left: Box::new(QueryExpr::BuiltInCall(QueryBuiltInCall {
						arguments: vec![
							QueryExpr::Literal(QueryLiteral::Text(String::from("A"))),
							name_column.clone(),
						],
						built_in: BuiltInFunction::CountOf,
					})),
					operator: QueryBinaryOperator::GreaterThanOrEqual,
					right: Box::new(QueryExpr::BuiltInCall(QueryBuiltInCall {
						arguments: vec![
							QueryExpr::Literal(QueryLiteral::Text(String::from("A"))),
							name_column,
						],
						built_in: BuiltInFunction::IndexOf,
					})),
				})),
			})),
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		let LoweredBackendQuery::Sql(query) = query;
		assert_eq!(
			query.statement,
			"SELECT COUNT(*) FROM `Customers` WHERE ((LOCATE('Ada', `Customers`.`Name`) > 0) AND (CASE WHEN CHAR_LENGTH('A') = 0 THEN 0 ELSE ((CHAR_LENGTH(`Customers`.`Name`) - CHAR_LENGTH(REPLACE(`Customers`.`Name`, 'A', ''))) DIV CHAR_LENGTH('A')) END >= NULLIF(LOCATE('A', `Customers`.`Name`), 0)))",
		);
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
				"SELECT COUNT(*) FROM \"Reporting\".\"Customers\" WHERE ((\"Customers\".\"Id\" = CAST(CAST($1 AS TEXT) AS BIGINT)) AND (\"Customers\".\"Active\" = TRUE))"
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
				"SELECT CAST(\"Customers\".\"Id\" AS TEXT), CAST(\"Customers\".\"Name\" AS TEXT) FROM \"Public\".\"Customers\" WHERE (\"Customers\".\"Active\" = CAST(CAST($1 AS TEXT) AS BOOLEAN)) ORDER BY \"Customers\".\"Name\" DESC LIMIT 1"
			),
			table_name: String::from("Customers"),
		}));
	}

	#[test]
	fn lowers_postgresql_grouped_for_record_plan_with_limit() {
		let record_columns = vec![QueryResultColumn {
			column_name: String::from("Id"),
			data_type: DataType::Int,
			is_nullable: false,
			is_primary_key: true,
		}];
		let query = QueryForPlan {
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
			group_by: vec![QueryGroupByItem {
				alias: Some(String::from("country")),
				data_type: DataType::Text,
				expression: QueryExpr::BuiltInCall(QueryBuiltInCall {
					arguments: vec![QueryExpr::Column(QueryColumnReference {
						column_name: String::from("Country"),
						data_type: DataType::Text,
						table_name: String::from("Customers"),
					})],
					built_in: BuiltInFunction::Trim,
				}),
				key_names: vec![String::from("country")],
			}],
			limit: Some(QueryParameter {
				data_type: DataType::Int,
				field_path: vec![],
				slot: 5,
			}),
			order_by: vec![],
			record_columns: record_columns.clone(),
			schema_is_implicit: false,
			schema_name: String::from("Public"),
			table_name: String::from("Customers"),
		}.lower_to_backend().unwrap();

		assert_eq!(query, LoweredBackendQuery::Sql(SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::PostgreSql,
			group_by: vec![SqlGroupByItem {
				data_type: DataType::Text,
				key_names: vec![String::from("country")],
			}],
			parameters: vec![
				SqlParameter {
					data_type: DataType::Bool,
					field_path: vec![],
					index: 1,
					slot: 4,
				},
				SqlParameter {
					data_type: DataType::Int,
					field_path: vec![],
					index: 2,
					slot: 5,
				},
			],
			result_shape: SqlQueryResultShape::RecordPointerArray(record_columns),
			schema_is_implicit: false,
			schema_name: String::from("Public"),
			statement: String::from(
				"SELECT CAST(\"Customers\".\"Id\" AS TEXT), CAST(TRIM(\"Customers\".\"Country\") AS TEXT) FROM \"Public\".\"Customers\" WHERE (\"Customers\".\"Active\" = CAST(CAST($1 AS TEXT) AS BOOLEAN)) ORDER BY TRIM(\"Customers\".\"Country\") LIMIT CAST(CAST($2 AS TEXT) AS BIGINT)"
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
			limit: Some(QueryParameter {
				data_type: DataType::Int,
				field_path: vec![],
				slot: 2,
			}),
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
				"SELECT \"Customers\".\"Id\" FROM \"Customers\" LIMIT ?1"
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

	#[test]
	fn preserves_query_metadata_across_sql_dialects() {
		let record_columns = vec![QueryResultColumn {
			column_name: String::from("Id"),
			data_type: DataType::Int,
			is_nullable: false,
			is_primary_key: true,
		}];
		let sqlite_plan = QueryForPlan {
			backend: DatabaseBackend::Sqlite,
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
					slot: 3,
				})),
			})),
			group_by: vec![],
			limit: Some(QueryParameter {
				data_type: DataType::Int,
				field_path: vec![],
				slot: 4,
			}),
			order_by: vec![],
			record_columns,
			schema_is_implicit: false,
			schema_name: String::from("Public"),
			table_name: String::from("Customers"),
		};
		let mut mysql_plan = sqlite_plan.clone();
		mysql_plan.backend = DatabaseBackend::MySql;
		let mut postgresql_plan = sqlite_plan.clone();
		postgresql_plan.backend = DatabaseBackend::PostgreSql;

		let LoweredBackendQuery::Sql(mysql) = mysql_plan.lower_to_backend().unwrap();
		let LoweredBackendQuery::Sql(sqlite) = sqlite_plan.lower_to_backend().unwrap();
		let LoweredBackendQuery::Sql(postgresql) = postgresql_plan.lower_to_backend().unwrap();

		assert_eq!(sqlite.database_name, mysql.database_name);
		assert_eq!(sqlite.database_name, postgresql.database_name);
		assert_eq!(sqlite.group_by, mysql.group_by);
		assert_eq!(sqlite.group_by, postgresql.group_by);
		assert_eq!(sqlite.parameters, mysql.parameters);
		assert_eq!(sqlite.parameters, postgresql.parameters);
		assert_eq!(sqlite.result_shape, mysql.result_shape);
		assert_eq!(sqlite.result_shape, postgresql.result_shape);
		assert_eq!(sqlite.schema_is_implicit, mysql.schema_is_implicit);
		assert_eq!(sqlite.schema_is_implicit, postgresql.schema_is_implicit);
		assert_eq!(sqlite.schema_name, mysql.schema_name);
		assert_eq!(sqlite.schema_name, postgresql.schema_name);
		assert_eq!(sqlite.table_name, mysql.table_name);
		assert_eq!(sqlite.table_name, postgresql.table_name);
		assert_ne!(sqlite.dialect, mysql.dialect);
		assert_ne!(sqlite.dialect, postgresql.dialect);
		assert_ne!(sqlite.statement, mysql.statement);
		assert_ne!(sqlite.statement, postgresql.statement);
		assert!(mysql.statement.contains("= ?"));
		assert!(sqlite.statement.contains("?1"));
		assert!(postgresql.statement.contains("$1"));
	}
}
