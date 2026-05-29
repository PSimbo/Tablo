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
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
	Negate,
	Not,
}

#[derive(Clone, Debug)]
pub struct AssignmentExpr {
	pub operator: AssignmentOperator,
	pub position: usize,
	pub target: IdentifierExpr,
	pub value: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
	pub left: Box<Expr>,
	pub operator: BinaryOperator,
	pub position: usize,
	pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct BlockStatement {
	pub position: usize,
	pub statements: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct BooleanLiteral {
	pub position: usize,
	pub value: bool,
}

#[derive(Clone, Debug)]
pub struct DecimalLiteral {
	pub position: usize,
	pub value: Decimal,
}

#[derive(Clone, Debug)]
pub struct IdentifierExpr {
	pub name: String,
	pub position: usize,
}

#[derive(Clone, Debug)]
pub struct IfStatement {
	pub condition: Expr,
	pub else_branch: Option<Box<Statement>>,
	pub position: usize,
	pub then_branch: BlockStatement,
}

#[derive(Clone, Debug)]
pub struct IntegerLiteral {
	pub position: usize,
	pub value: i64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
	pub result: Option<Expr>,
	pub statements: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct TextLiteral {
	pub position: usize,
	pub value: String,
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
	pub operand: Box<Expr>,
	pub operator: UnaryOperator,
	pub position: usize,
}

#[derive(Clone, Debug)]
pub struct VariableDeclaration {
	pub data_type: DataType,
	pub is_const: bool,
	pub initial_value: Option<Expr>,
	pub name: String,
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

impl PartialEq for AssignmentExpr {
	fn eq(&self, other: &Self) -> bool {
		self.operator == other.operator
			&& self.target == other.target
			&& self.value == other.value
	}
}

impl Eq for AssignmentExpr {}

impl PartialEq for BinaryExpr {
	fn eq(&self, other: &Self) -> bool {
		self.left == other.left
			&& self.operator == other.operator
			&& self.right == other.right
	}
}

impl Eq for BinaryExpr {}

impl PartialEq for BlockStatement {
	fn eq(&self, other: &Self) -> bool {
		self.statements == other.statements
	}
}

impl Eq for BlockStatement {}

impl PartialEq for BooleanLiteral {
	fn eq(&self, other: &Self) -> bool {
		self.value == other.value
	}
}

impl Eq for BooleanLiteral {}

impl PartialEq for DecimalLiteral {
	fn eq(&self, other: &Self) -> bool {
		self.value == other.value
	}
}

impl Eq for DecimalLiteral {}

impl PartialEq for IdentifierExpr {
	fn eq(&self, other: &Self) -> bool {
		self.name == other.name
	}
}

impl Eq for IdentifierExpr {}

impl PartialEq for IfStatement {
	fn eq(&self, other: &Self) -> bool {
		self.condition == other.condition
			&& self.else_branch == other.else_branch
			&& self.then_branch == other.then_branch
	}
}

impl Eq for IfStatement {}

impl PartialEq for IntegerLiteral {
	fn eq(&self, other: &Self) -> bool {
		self.value == other.value
	}
}

impl Eq for IntegerLiteral {}

impl PartialEq for TextLiteral {
	fn eq(&self, other: &Self) -> bool {
		self.value == other.value
	}
}

impl Eq for TextLiteral {}

impl PartialEq for UnaryExpr {
	fn eq(&self, other: &Self) -> bool {
		self.operand == other.operand
			&& self.operator == other.operator
	}
}

impl Eq for UnaryExpr {}

impl PartialEq for VariableDeclaration {
	fn eq(&self, other: &Self) -> bool {
		self.data_type == other.data_type
			&& self.is_const == other.is_const
			&& self.initial_value == other.initial_value
			&& self.name == other.name
	}
}

impl Eq for VariableDeclaration {}
