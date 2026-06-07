use crate::ast::DataType;
use crate::schema::DatabaseBackend;
use crate::value::Decimal;

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
pub enum QueryUnaryOperator {
	Negate,
	Not,
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
	pub schema_name: String,
	pub table_name: String,
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
