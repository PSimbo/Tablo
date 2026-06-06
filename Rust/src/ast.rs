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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AssignmentTarget {
	Identifier(IdentifierExpr),
	Index(ArrayIndexAssignmentTarget),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DataType {
	Array(Box<DataType>),
	Bool,
	Dec,
	EmptyArray,
	Int,
	Range(Box<DataType>),
	Text,
	Void,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
	Array(ArrayLiteral),
	Assignment(AssignmentExpr),
	Binary(BinaryExpr),
	Boolean(BooleanLiteral),
	Call(CallExpr),
	Decimal(DecimalLiteral),
	Identifier(IdentifierExpr),
	Index(IndexExpr),
	Integer(IntegerLiteral),
	Range(RangeExpr),
	Text(TextLiteral),
	Unary(UnaryExpr),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
	Block(BlockStatement),
	Break(BreakStatement),
	Continue(ContinueStatement),
	Expression(Expr),
	For(ForStatement),
	If(IfStatement),
	Return(ReturnStatement),
	VariableDeclaration(VariableDeclaration),
	While(WhileStatement),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
	Negate,
	Not,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArrayIndexAssignmentTarget {
	pub array: IdentifierExpr,
	pub index: Box<Expr>,
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArrayLiteral {
	pub elements: Vec<Expr>,
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AssignmentExpr {
	pub operator: AssignmentOperator,
	pub position: usize,
	pub target: AssignmentTarget,
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
pub struct BreakStatement {
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BooleanLiteral {
	pub position: usize,
	pub value: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CallArgument {
	pub is_by_ref: bool,
	pub position: usize,
	pub value: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CallExpr {
	pub arguments: Vec<CallArgument>,
	pub callee: IdentifierExpr,
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ContinueStatement {
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DecimalLiteral {
	pub position: usize,
	pub value: Decimal,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ForStatement {
	pub body: BlockStatement,
	pub iterable: Expr,
	pub position: usize,
	pub variable: IdentifierExpr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDeclaration {
	pub body: BlockStatement,
	pub name: String,
	pub parameters: Vec<FunctionParameter>,
	pub position: usize,
	pub return_type: DataType,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionParameter {
	pub data_type: DataType,
	pub is_by_ref: bool,
	pub name: String,
	pub position: usize,
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
pub struct IndexExpr {
	pub array: Box<Expr>,
	pub index: Box<Expr>,
	pub position: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntegerLiteral {
	pub position: usize,
	pub value: i64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
	pub functions: Vec<FunctionDeclaration>,
	pub result: Option<Expr>,
	pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RangeExpr {
	pub end: Box<Expr>,
	pub position: usize,
	pub start: Box<Expr>,
	pub step: Option<Box<Expr>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReturnStatement {
	pub position: usize,
	pub value: Option<Expr>,
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
			Expr::Array(expression) => expression.position,
			Expr::Assignment(expression) => expression.position,
			Expr::Binary(expression) => expression.position,
			Expr::Boolean(expression) => expression.position,
			Expr::Call(expression) => expression.position,
			Expr::Decimal(expression) => expression.position,
			Expr::Identifier(expression) => expression.position,
			Expr::Index(expression) => expression.position,
			Expr::Integer(expression) => expression.position,
			Expr::Range(expression) => expression.position,
			Expr::Text(expression) => expression.position,
			Expr::Unary(expression) => expression.position,
		}
	}
}
