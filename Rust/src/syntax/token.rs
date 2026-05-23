#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
	AndKeyword,
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
	NotKeyword,
	OrKeyword,
	Percent,
	Plus,
	RightParenthesis,
	TrueKeyword,
	XorKeyword,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
	pub end: usize,
	pub kind: TokenKind,
	pub lexeme: String,
	pub start: usize,
}
