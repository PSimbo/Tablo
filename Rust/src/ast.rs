use crate::value::Decimal;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AssignmentOperator {
	Assign,
	AddAssign,
	DivideAssign,
	ModuloAssign,
	MultiplyAssign,
	SubtractAssign,
}

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
	Text,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
	Assignment(AssignmentExpr),
	Binary(BinaryExpr),
	Boolean(BooleanLiteral),
	Decimal(DecimalLiteral),
	Identifier(IdentifierExpr),
	Integer(IntegerLiteral),
	Text(TextLiteral),
	Unary(UnaryExpr),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
	Block(BlockStatement),
	Expression(Expr),
	If(IfStatement),
	VariableDeclaration(VariableDeclaration),
	While(WhileStatement),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
	Negate,
	Not,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AssignmentExpr {
	pub operator: AssignmentOperator,
	pub position: usize,
	pub target: IdentifierExpr,
	pub value: Box<Expr>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinaryExpr {
	pub left: Box<Expr>,
	pub operator: BinaryOperator,
	pub position: usize,
	pub right: Box<Expr>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BlockStatement {
	pub position: usize,
	pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BooleanLiteral {
	pub position: usize,
	pub value: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DecimalLiteral {
	pub position: usize,
	pub value: Decimal,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdentifierExpr {
	pub name: String,
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IfStatement {
	pub condition: Expr,
	pub else_branch: Option<Box<Statement>>,
	pub position: usize,
	pub then_branch: BlockStatement,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntegerLiteral {
	pub position: usize,
	pub value: i64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
	pub result: Option<Expr>,
	pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TextLiteral {
	pub position: usize,
	pub value: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnaryExpr {
	pub operand: Box<Expr>,
	pub operator: UnaryOperator,
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VariableDeclaration {
	pub data_type: DataType,
	pub is_const: bool,
	pub initial_value: Option<Expr>,
	pub name: String,
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WhileStatement {
	pub body: BlockStatement,
	pub condition: Expr,
	pub position: usize,
}

impl Expr {
	pub fn position(&self) -> usize {
		match self {
			Expr::Assignment(expression) => expression.position,
			Expr::Binary(expression) => expression.position,
			Expr::Boolean(expression) => expression.position,
			Expr::Decimal(expression) => expression.position,
			Expr::Identifier(expression) => expression.position,
			Expr::Integer(expression) => expression.position,
			Expr::Text(expression) => expression.position,
			Expr::Unary(expression) => expression.position,
		}
	}
}
