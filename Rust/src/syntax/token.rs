#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
	Asterisk,
	BangEqual,
	Dash,
	DecimalLiteral,
	EndOfFile,
	EqualEqual,
	FalseKeyword,
	ForwardSlash,
	GreaterThan,
	GreaterThanOrEqual,
	IntegerLiteral,
	LeftParenthesis,
	LessThan,
	LessThanOrEqual,
	Percent,
	Plus,
	RightParenthesis,
	TrueKeyword,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
	pub end: usize,
	pub kind: TokenKind,
	pub lexeme: String,
	pub start: usize,
}
