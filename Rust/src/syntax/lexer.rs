use crate::source::SourceText;

use super::token::TokenKind;
use super::token::Token;

#[derive(Clone, Copy)]
enum StringTokenMode {
	Continuation,
	Start,
}

pub struct Lexer {
	end_of_file: bool,
	interpolated_string_stack: Vec<usize>,
	position: usize,
	resume_interpolated_string: bool,
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
			interpolated_string_stack: Vec::new(),
			position: 0,
			resume_interpolated_string: false,
			source,
		}
	}

	pub fn source(&self) -> &SourceText {
		&self.source
	}

	pub fn next_token(&mut self) -> Result<Option<Token>, LexError> {
		if self.resume_interpolated_string {
			self.resume_interpolated_string = false;
			return Ok(Some(self.lex_string_token(StringTokenMode::Continuation)?));
		}

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

		if next == '\'' {
			return Ok(Some(self.lex_string_token(StringTokenMode::Start)?));
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
			'+' if self.peek_next_char() == Some('=') => {
				self.advance_char();
				self.advance_char();

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::PlusEqual,
					lexeme: String::from("+="),
					start,
				}));
			}
			'+' => TokenKind::Plus,
			'-' if self.peek_next_char() == Some('=') => {
				self.advance_char();
				self.advance_char();

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::MinusEqual,
					lexeme: String::from("-="),
					start,
				}));
			}
			'-' => TokenKind::Dash,
			'*' if self.peek_next_char() == Some('=') => {
				self.advance_char();
				self.advance_char();

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::MultiplyEqual,
					lexeme: String::from("*="),
					start,
				}));
			}
			'*' => TokenKind::Asterisk,
			'/' if self.peek_next_char() == Some('=') => {
				self.advance_char();
				self.advance_char();

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::SlashEqual,
					lexeme: String::from("/="),
					start,
				}));
			}
			'/' => TokenKind::ForwardSlash,
			'%' if self.peek_next_char() == Some('=') => {
				self.advance_char();
				self.advance_char();

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::PercentEqual,
					lexeme: String::from("%="),
					start,
				}));
			}
			'%' => TokenKind::Percent,
			':' => TokenKind::Colon,
			';' => TokenKind::Semicolon,
			'(' => TokenKind::LeftParenthesis,
			')' => TokenKind::RightParenthesis,
			'}' if !self.interpolated_string_stack.is_empty() => {
				self.advance_char();
				self.resume_interpolated_string = true;

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::RightBrace,
					lexeme: String::from("}"),
					start,
				}));
			}
			'!' if self.peek_next_char() == Some('=') => {
				self.advance_char();
				self.advance_char();

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::BangEqual,
					lexeme: String::from("!="),
					start,
				}));
			}
			'=' if self.peek_next_char() == Some('=') => {
				self.advance_char();
				self.advance_char();

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::EqualEqual,
					lexeme: String::from("=="),
					start,
				}));
			}
			'=' => TokenKind::Equal,
			'>' if self.peek_next_char() == Some('=') => {
				self.advance_char();
				self.advance_char();

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::GreaterThanOrEqual,
					lexeme: String::from(">="),
					start,
				}));
			}
			'>' => TokenKind::GreaterThan,
			'<' if self.peek_next_char() == Some('=') => {
				self.advance_char();
				self.advance_char();

				return Ok(Some(Token {
					end: self.position,
					kind: TokenKind::LessThanOrEqual,
					lexeme: String::from("<="),
					start,
				}));
			}
			'<' => TokenKind::LessThan,
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

	fn lex_keyword(&mut self) -> Result<(TokenKind, String), LexError> {
		let start = self.position;

		while self.peek_char().is_some_and(|next| next.is_ascii_alphanumeric() || next == '_') {
			self.advance_char();
		}

		let lexeme = self.source.as_str()[start..self.position].to_string();
		let kind = match lexeme.as_str() {
			"and" => TokenKind::AndKeyword,
			"bool" => TokenKind::BoolKeyword,
			"dec" => TokenKind::DecKeyword,
			"false" => TokenKind::FalseKeyword,
			"int" => TokenKind::IntKeyword,
			"not" => TokenKind::NotKeyword,
			"or" => TokenKind::OrKeyword,
			"text" => TokenKind::TextKeyword,
			"true" => TokenKind::TrueKeyword,
			"var" => TokenKind::VarKeyword,
			"xor" => TokenKind::XorKeyword,
			_ => TokenKind::Identifier,
		};

		Ok((kind, lexeme))
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

	fn lex_string_token(&mut self, mode: StringTokenMode) -> Result<Token, LexError> {
		let start = self.position;
		let trim_count = match mode {
			StringTokenMode::Continuation => *self.interpolated_string_stack.last().unwrap_or(&0),
			StringTokenMode::Start => self.line_indentation_before(start),
		};

		if matches!(mode, StringTokenMode::Start) {
			self.advance_char();
		}

		let mut value = String::new();

		while let Some(next) = self.peek_char() {
			if next == '\'' {
				self.advance_char();
				let value = trim_multiline_string(&value, trim_count);

				let kind = match mode {
					StringTokenMode::Start => TokenKind::StringLiteral,
					StringTokenMode::Continuation => TokenKind::InterpolatedStringEnd,
				};

				if matches!(mode, StringTokenMode::Continuation) {
					self.interpolated_string_stack.pop();
				}

				return Ok(Token {
					end: self.position,
					kind,
					lexeme: value,
					start,
				});
			}

			if next == '\0' {
				return Err(LexError {
					position: self.position,
					message: String::from("String literals may not contain the null character."),
				});
			}

			if next == '$' && self.peek_next_char() == Some('{') {
				self.advance_char();
				self.advance_char();
				let value = trim_multiline_string(&value, trim_count);

				if matches!(mode, StringTokenMode::Start) {
					self.interpolated_string_stack.push(trim_count);
				}

				let kind = match mode {
					StringTokenMode::Start => TokenKind::InterpolatedStringStart,
					StringTokenMode::Continuation => TokenKind::InterpolatedStringMiddle,
				};

				return Ok(Token {
					end: self.position,
					kind,
					lexeme: value,
					start,
				});
			}

			if next == '\\' {
				self.advance_char();
				let escape = self.peek_char().ok_or(LexError {
					position: self.position,
					message: String::from("Unterminated escape sequence in string literal."),
				})?;

				let decoded = match escape {
					'\'' => '\'',
					'\\' => '\\',
					'b' => '\u{0008}',
					'f' => '\u{000C}',
					'n' => '\n',
					'r' => '\r',
					't' => '\t',
					'v' => '\u{000B}',
					_ => {
						return Err(LexError {
							position: self.position,
							message: format!("Unsupported escape sequence `\\{escape}`."),
						});
					}
				};

				self.advance_char();
				value.push(decoded);
				continue;
			}

			self.advance_char();
			value.push(next);
		}

		Err(LexError {
			position: start,
			message: String::from("Unterminated string literal."),
		})
	}

	fn line_indentation_before(&self, position: usize) -> usize {
		let prefix = &self.source.as_str()[..position];
		let line_start = prefix.rfind('\n').map_or(0, |index| index + 1);

		prefix[line_start..]
			.chars()
			.take_while(|ch| ch.is_whitespace() && *ch != '\n' && *ch != '\r')
			.count()
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

fn trim_multiline_string(value: &str, trim_count: usize) -> String {
	if !value.contains('\n') || trim_count == 0 {
		return value.to_string();
	}

	let mut result = String::new();
	let mut at_line_start = true;
	let mut remaining_trim = trim_count;

	for ch in value.chars() {
		if at_line_start {
			if ch == '\n' {
				result.push(ch);
				remaining_trim = trim_count;
				continue;
			}

			if remaining_trim > 0 && ch.is_whitespace() && ch != '\r' {
				remaining_trim -= 1;
				continue;
			}

			at_line_start = false;
		}

		result.push(ch);

		if ch == '\n' {
			at_line_start = true;
			remaining_trim = trim_count;
		}
	}

	result
}

#[cfg(test)]
mod tests {
	use crate::source::SourceText;

	use super::Lexer;
	use super::TokenKind;

	#[test]
	fn rejects_unexpected_character() {
		let mut lexer = Lexer::new(SourceText::new("1 ? 2"));
		let error = lexer.tokenize().unwrap_err();

		assert_eq!(error.position, 2);
		assert_eq!(error.message, "Unexpected character `?`.");
	}

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
	fn tokenizes_assignment_operators() {
		let mut lexer = Lexer::new(SourceText::new("x = y += z -= a *= b /= c %= d"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[1].kind, TokenKind::Equal);
		assert_eq!(tokens[3].kind, TokenKind::PlusEqual);
		assert_eq!(tokens[5].kind, TokenKind::MinusEqual);
		assert_eq!(tokens[7].kind, TokenKind::MultiplyEqual);
		assert_eq!(tokens[9].kind, TokenKind::SlashEqual);
		assert_eq!(tokens[11].kind, TokenKind::PercentEqual);
	}

	#[test]
	fn tokenizes_boolean_literals() {
		let mut lexer = Lexer::new(SourceText::new("true false and or not xor bool dec int text var"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 12);
		assert_eq!(tokens[0].kind, TokenKind::TrueKeyword);
		assert_eq!(tokens[0].lexeme, "true");
		assert_eq!(tokens[1].kind, TokenKind::FalseKeyword);
		assert_eq!(tokens[1].lexeme, "false");
		assert_eq!(tokens[2].kind, TokenKind::AndKeyword);
		assert_eq!(tokens[3].kind, TokenKind::OrKeyword);
		assert_eq!(tokens[4].kind, TokenKind::NotKeyword);
		assert_eq!(tokens[5].kind, TokenKind::XorKeyword);
		assert_eq!(tokens[6].kind, TokenKind::BoolKeyword);
		assert_eq!(tokens[7].kind, TokenKind::DecKeyword);
		assert_eq!(tokens[8].kind, TokenKind::IntKeyword);
		assert_eq!(tokens[9].kind, TokenKind::TextKeyword);
		assert_eq!(tokens[10].kind, TokenKind::VarKeyword);
		assert_eq!(tokens[11].kind, TokenKind::EndOfFile);
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
	fn tokenizes_identifier() {
		let mut lexer = Lexer::new(SourceText::new("maybe"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 2);
		assert_eq!(tokens[0].kind, TokenKind::Identifier);
		assert_eq!(tokens[0].lexeme, "maybe");
		assert_eq!(tokens[1].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_identifier_and_colon() {
		let mut lexer = Lexer::new(SourceText::new("var x: int = value;"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 8);
		assert_eq!(tokens[0].kind, TokenKind::VarKeyword);
		assert_eq!(tokens[1].kind, TokenKind::Identifier);
		assert_eq!(tokens[1].lexeme, "x");
		assert_eq!(tokens[2].kind, TokenKind::Colon);
		assert_eq!(tokens[3].kind, TokenKind::IntKeyword);
		assert_eq!(tokens[4].kind, TokenKind::Equal);
		assert_eq!(tokens[4].lexeme, "=");
		assert_eq!(tokens[5].kind, TokenKind::Identifier);
		assert_eq!(tokens[5].lexeme, "value");
		assert_eq!(tokens[6].kind, TokenKind::Semicolon);
		assert_eq!(tokens[7].kind, TokenKind::EndOfFile);
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
	fn tokenizes_relational_operators() {
		let mut lexer = Lexer::new(SourceText::new("1 == 2 != 3 > 4 >= 5 < 6 <= 7"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 14);
		assert_eq!(tokens[1].kind, TokenKind::EqualEqual);
		assert_eq!(tokens[3].kind, TokenKind::BangEqual);
		assert_eq!(tokens[5].kind, TokenKind::GreaterThan);
		assert_eq!(tokens[7].kind, TokenKind::GreaterThanOrEqual);
		assert_eq!(tokens[9].kind, TokenKind::LessThan);
		assert_eq!(tokens[11].kind, TokenKind::LessThanOrEqual);
		assert_eq!(tokens[13].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_multiline_interpolated_string_with_indentation_trimming() {
		let mut lexer = Lexer::new(SourceText::new(
			"    '\n        hello ${name}\n        world\n    '",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[0].kind, TokenKind::InterpolatedStringStart);
		assert_eq!(tokens[0].lexeme, "\n    hello ");
		assert_eq!(tokens[3].kind, TokenKind::InterpolatedStringEnd);
		assert_eq!(tokens[3].lexeme, "\n    world\n");
	}

	#[test]
	fn tokenizes_multiline_string_with_indentation_trimming() {
		let mut lexer = Lexer::new(SourceText::new(
			"    '\n        first\n        second\n    '",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
		assert_eq!(tokens[0].lexeme, "\n    first\n    second\n");
	}

	#[test]
	fn tokenizes_string_interpolation() {
		let mut lexer = Lexer::new(SourceText::new("'hello ${name}!';"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[0].kind, TokenKind::InterpolatedStringStart);
		assert_eq!(tokens[0].lexeme, "hello ");
		assert_eq!(tokens[1].kind, TokenKind::Identifier);
		assert_eq!(tokens[1].lexeme, "name");
		assert_eq!(tokens[2].kind, TokenKind::RightBrace);
		assert_eq!(tokens[3].kind, TokenKind::InterpolatedStringEnd);
		assert_eq!(tokens[3].lexeme, "!");
		assert_eq!(tokens[4].kind, TokenKind::Semicolon);
		assert_eq!(tokens[5].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_string_literal() {
		let mut lexer = Lexer::new(SourceText::new("'hello\\nworld'"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 2);
		assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
		assert_eq!(tokens[0].lexeme, "hello\nworld");
		assert_eq!(tokens[1].kind, TokenKind::EndOfFile);
	}
}
