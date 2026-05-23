use crate::value::Decimal;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryOperator {
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DataType {
	Bool,
	Dec,
	Int,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
	Boolean(BooleanLiteral),
	Identifier(IdentifierExpr),
	Integer(IntegerLiteral),
	Decimal(DecimalLiteral),
	Binary(BinaryExpr),
	Unary(UnaryExpr),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
	VariableDeclaration(VariableDeclaration),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
	Negate,
	Not,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinaryExpr {
	pub left: Box<Expr>,
	pub operator: BinaryOperator,
	pub right: Box<Expr>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BooleanLiteral {
	pub value: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DecimalLiteral {
	pub value: Decimal,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdentifierExpr {
	pub name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntegerLiteral {
	pub value: i64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
	pub result: Option<Expr>,
	pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnaryExpr {
	pub operand: Box<Expr>,
	pub operator: UnaryOperator,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VariableDeclaration {
	pub data_type: DataType,
	pub initial_value: Expr,
	pub name: String,
}
