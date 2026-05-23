#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
	Asterisk,
	Dash,
	DecimalLiteral,
	EndOfFile,
	FalseKeyword,
	ForwardSlash,
	IntegerLiteral,
	LeftParenthesis,
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
