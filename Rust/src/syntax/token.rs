#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
	Asterisk,
	Dash,
	EndOfFile,
	ForwardSlash,
	IntegerLiteral,
	Percent,
	Plus,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
	pub end: usize,
	pub kind: TokenKind,
	pub lexeme: String,
	pub start: usize,
}
