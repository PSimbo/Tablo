use crate::source::SourceText;

use super::token::TokenKind;
use super::token::Token;

pub struct Lexer {
	end_of_file: bool,
	position: usize,
	source: SourceText,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LexError {
	pub position: usize,
	pub message: String,
}

impl Lexer {
	pub fn new(source: SourceText) -> Self {
		Self {
			end_of_file: false,
			position: 0,
			source,
		}
	}

	pub fn source(&self) -> &SourceText {
		&self.source
	}

	pub fn next_token(&mut self) -> Result<Option<Token>, LexError> {
		self.skip_whitespace();

		if self.position >= self.source.as_str().len() {
			if self.end_of_file {
				return Ok(None);
			}

			self.end_of_file = true;

			return Ok(Some(Token {
				end: self.position,
				kind: TokenKind::EndOfFile,
				lexeme: String::new(),
				start: self.position,
			}));
		}

		let start = self.position;
		let next = self.peek_char().unwrap();

		if next.is_ascii_digit() {
			let (kind, lexeme) = self.lex_numeric_literal(false);
			let end = self.position;

			return Ok(Some(Token {
				end,
				kind,
				lexeme,
				start,
			}));
		}

		if next == '.' && self.peek_next_char().is_some_and(|c| c.is_ascii_digit()) {
			let (kind, lexeme) = self.lex_numeric_literal(true);
			let end = self.position;

			return Ok(Some(Token {
				end,
				kind,
				lexeme,
				start,
			}));
		}

		if next.is_ascii_alphabetic() {
			let (kind, lexeme) = self.lex_keyword()?;
			let end = self.position;

			return Ok(Some(Token {
				end,
				kind,
				lexeme,
				start,
			}));
		}

		let token_kind = match next {
			'+' => TokenKind::Plus,
			'-' => TokenKind::Dash,
			'*' => TokenKind::Asterisk,
			'/' => TokenKind::ForwardSlash,
			'%' => TokenKind::Percent,
			'(' => TokenKind::LeftParenthesis,
			')' => TokenKind::RightParenthesis,
			_ => {
				return Err(LexError {
					position: self.position,
					message: format!("Unexpected character `{next}`."),
				});
			}
		};

		self.advance_char();

		Ok(Some(Token {
			end: self.position,
			kind: token_kind,
			lexeme: next.to_string(),
			start,
		}))
	}

	pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
		let mut tokens = Vec::new();

		while let Some(token) = self.next_token()? {
			let is_end_of_file = token.kind == TokenKind::EndOfFile;
			tokens.push(token);

			if is_end_of_file {
				break;
			}
		}

		Ok(tokens)
	}

	fn advance_char(&mut self) -> Option<char> {
		let next = self.peek_char()?;
		self.position += next.len_utf8();
		Some(next)
	}

	fn lex_digit_sequence(&mut self) {
		let mut previous_was_underscore = false;

		while let Some(next) = self.peek_char() {
			if next.is_ascii_digit() {
				previous_was_underscore = false;
				self.advance_char();
				continue;
			}

			if next == '_' && !previous_was_underscore && self.peek_next_char().is_some_and(|c| c.is_ascii_digit()) {
				previous_was_underscore = true;
				self.advance_char();
				continue;
			}

			break;
		}
	}

	fn lex_numeric_literal(&mut self, starts_with_decimal_separator: bool) -> (TokenKind, String) {
		let start = self.position;

		if starts_with_decimal_separator {
			self.advance_char();
			self.lex_digit_sequence();
			return (TokenKind::DecimalLiteral, self.source.as_str()[start..self.position].to_string());
		}

		self.lex_digit_sequence();

		if self.peek_char() == Some('.') && self.peek_next_char().is_some_and(|c| c.is_ascii_digit()) {
			self.advance_char();
			self.lex_digit_sequence();
			return (TokenKind::DecimalLiteral, self.source.as_str()[start..self.position].to_string());
		}

		(TokenKind::IntegerLiteral, self.source.as_str()[start..self.position].to_string())
	}

	fn lex_keyword(&mut self) -> Result<(TokenKind, String), LexError> {
		let start = self.position;

		while self.peek_char().is_some_and(|next| next.is_ascii_alphanumeric() || next == '_') {
			self.advance_char();
		}

		let lexeme = self.source.as_str()[start..self.position].to_string();
		let kind = match lexeme.as_str() {
			"false" => TokenKind::FalseKeyword,
			"true" => TokenKind::TrueKeyword,
			_ => {
				return Err(LexError {
					position: start,
					message: format!("Unexpected identifier `{lexeme}`."),
				});
			}
		};

		Ok((kind, lexeme))
	}

	fn peek_char(&self) -> Option<char> {
		self.source.as_str()[self.position..].chars().next()
	}

	fn peek_next_char(&self) -> Option<char> {
		let mut chars = self.source.as_str()[self.position..].chars();
		chars.next()?;
		chars.next()
	}

	fn skip_whitespace(&mut self) {
		while self.peek_char().is_some_and(char::is_whitespace) {
			self.advance_char();
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::source::SourceText;

	use super::Lexer;
	use super::TokenKind;

	#[test]
	fn tokenizes_all_supported_arithmetic_operators() {
		let mut lexer = Lexer::new(SourceText::new("1 - 2 * 3 / 4 % 5 + 6"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 12);
		assert_eq!(tokens[0].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[1].kind, TokenKind::Dash);
		assert_eq!(tokens[2].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[3].kind, TokenKind::Asterisk);
		assert_eq!(tokens[4].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[5].kind, TokenKind::ForwardSlash);
		assert_eq!(tokens[6].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[7].kind, TokenKind::Percent);
		assert_eq!(tokens[8].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[9].kind, TokenKind::Plus);
		assert_eq!(tokens[10].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[11].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_boolean_literals() {
		let mut lexer = Lexer::new(SourceText::new("true false"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 3);
		assert_eq!(tokens[0].kind, TokenKind::TrueKeyword);
		assert_eq!(tokens[0].lexeme, "true");
		assert_eq!(tokens[1].kind, TokenKind::FalseKeyword);
		assert_eq!(tokens[1].lexeme, "false");
		assert_eq!(tokens[2].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_decimal_literals() {
		let mut lexer = Lexer::new(SourceText::new("1.25 + .5"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 4);
		assert_eq!(tokens[0].kind, TokenKind::DecimalLiteral);
		assert_eq!(tokens[0].lexeme, "1.25");
		assert_eq!(tokens[1].kind, TokenKind::Plus);
		assert_eq!(tokens[2].kind, TokenKind::DecimalLiteral);
		assert_eq!(tokens[2].lexeme, ".5");
		assert_eq!(tokens[3].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_integer_addition_expression() {
		let mut lexer = Lexer::new(SourceText::new("1 + 2"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 4);
		assert_eq!(tokens[0].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[0].lexeme, "1");
		assert_eq!(tokens[1].kind, TokenKind::Plus);
		assert_eq!(tokens[1].lexeme, "+");
		assert_eq!(tokens[2].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[2].lexeme, "2");
		assert_eq!(tokens[3].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_integer_literal_with_digit_separators() {
		let mut lexer = Lexer::new(SourceText::new("1_234"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 2);
		assert_eq!(tokens[0].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[0].lexeme, "1_234");
		assert_eq!(tokens[1].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_negative_decimal_literal_as_dash_then_decimal_literal() {
		let mut lexer = Lexer::new(SourceText::new("-1.25 + -.5"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 6);
		assert_eq!(tokens[0].kind, TokenKind::Dash);
		assert_eq!(tokens[0].lexeme, "-");
		assert_eq!(tokens[1].kind, TokenKind::DecimalLiteral);
		assert_eq!(tokens[1].lexeme, "1.25");
		assert_eq!(tokens[2].kind, TokenKind::Plus);
		assert_eq!(tokens[3].kind, TokenKind::Dash);
		assert_eq!(tokens[3].lexeme, "-");
		assert_eq!(tokens[4].kind, TokenKind::DecimalLiteral);
		assert_eq!(tokens[4].lexeme, ".5");
		assert_eq!(tokens[5].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_negative_integer_literal_as_dash_then_integer_literal() {
		let mut lexer = Lexer::new(SourceText::new("-123 + -4_567"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 6);
		assert_eq!(tokens[0].kind, TokenKind::Dash);
		assert_eq!(tokens[0].lexeme, "-");
		assert_eq!(tokens[1].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[1].lexeme, "123");
		assert_eq!(tokens[2].kind, TokenKind::Plus);
		assert_eq!(tokens[3].kind, TokenKind::Dash);
		assert_eq!(tokens[3].lexeme, "-");
		assert_eq!(tokens[4].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[4].lexeme, "4_567");
		assert_eq!(tokens[5].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_parentheses() {
		let mut lexer = Lexer::new(SourceText::new("(1 + 2)"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 6);
		assert_eq!(tokens[0].kind, TokenKind::LeftParenthesis);
		assert_eq!(tokens[1].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[2].kind, TokenKind::Plus);
		assert_eq!(tokens[3].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[4].kind, TokenKind::RightParenthesis);
		assert_eq!(tokens[5].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn rejects_unexpected_character() {
		let mut lexer = Lexer::new(SourceText::new("1 ? 2"));
		let error = lexer.tokenize().unwrap_err();

		assert_eq!(error.position, 2);
		assert_eq!(error.message, "Unexpected character `?`.");
	}

	#[test]
	fn rejects_unexpected_identifier() {
		let mut lexer = Lexer::new(SourceText::new("maybe"));
		let error = lexer.tokenize().unwrap_err();

		assert_eq!(error.position, 0);
		assert_eq!(error.message, "Unexpected identifier `maybe`.");
	}
}
