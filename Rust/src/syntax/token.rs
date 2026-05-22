#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
	Asterisk,
	Dash,
	EndOfFile,
	ForwardSlash,
	IntegerLiteral,
	LeftParenthesis,
	Percent,
	Plus,
	RightParenthesis,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
	pub end: usize,
	pub kind: TokenKind,
	pub lexeme: String,
	pub start: usize,
}
