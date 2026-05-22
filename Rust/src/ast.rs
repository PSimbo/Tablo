use crate::value::Decimal;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryOperator {
	Add,
	Divide,
	Modulo,
	Multiply,
	Subtract,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
	Integer(IntegerLiteral),
	Decimal(DecimalLiteral),
	Binary(BinaryExpr),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinaryExpr {
	pub left: Box<Expr>,
	pub operator: BinaryOperator,
	pub right: Box<Expr>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DecimalLiteral {
	pub value: Decimal,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntegerLiteral {
	pub value: i64,
}
