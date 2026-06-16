use crate::source::SourceText;

use super::token::Token;
use super::token::TokenKind;

#[derive(Clone, Copy)]
struct MultilineStringLayout {
	first_line_padding: usize,
	trim_count: usize,
}

#[derive(Clone, Copy)]
enum StringTokenMode {
	Continuation,
	Start,
}

pub struct Lexer {
	end_of_file: bool,
	interpolated_string_stack: Vec<MultilineStringLayout>,
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

		self.skip_ignored_text()?;

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

		if next == '"' {
			let (kind, lexeme) = self.lex_quoted_identifier()?;
			let end = self.position;

			return Ok(Some(Token {
				end,
				kind,
				lexeme,
				start,
			}));
		}

		if next == '@' {
			let (kind, lexeme) = self.lex_temporal_literal()?;
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
			'&' => TokenKind::Ampersand,
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
			'|' => TokenKind::Pipe,
			':' => TokenKind::Colon,
			',' => TokenKind::Comma,
			'.' => TokenKind::Dot,
			'[' => TokenKind::LeftBracket,
			'{' => TokenKind::LeftBrace,
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
			'}' => TokenKind::RightBrace,
			']' => TokenKind::RightBracket,
			'?' => TokenKind::Question,
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
			"any" => TokenKind::AnyKeyword,
			"asc" => TokenKind::AscKeyword,
			"bool" => TokenKind::BoolKeyword,
			"break" => TokenKind::BreakKeyword,
			"by" => TokenKind::ByKeyword,
			"const" => TokenKind::ConstKeyword,
			"continue" => TokenKind::ContinueKeyword,
			"count" => TokenKind::CountKeyword,
			"create" => TokenKind::CreateKeyword,
			"date" => TokenKind::DateKeyword,
			"dec" => TokenKind::DecKeyword,
			"desc" => TokenKind::DescKeyword,
			"else" => TokenKind::ElseKeyword,
			"enum" => TokenKind::EnumKeyword,
			"false" => TokenKind::FalseKeyword,
			"find" => TokenKind::FindKeyword,
			"first" => TokenKind::FirstKeyword,
			"fn" => TokenKind::FnKeyword,
			"for" => TokenKind::ForKeyword,
			"from" => TokenKind::FromKeyword,
			"if" => TokenKind::IfKeyword,
			"in" => TokenKind::InKeyword,
			"int" => TokenKind::IntKeyword,
			"last" => TokenKind::LastKeyword,
			"mut" => TokenKind::MutKeyword,
			"new" => TokenKind::NewKeyword,
			"null" => TokenKind::NullKeyword,
			"not" => TokenKind::NotKeyword,
			"obj" => TokenKind::ObjKeyword,
			"or" => TokenKind::OrKeyword,
			"order" => TokenKind::OrderKeyword,
			"pub" => TokenKind::PubKeyword,
			"rec" => TokenKind::RecKeyword,
			"return" => TokenKind::ReturnKeyword,
			"text" => TokenKind::TextKeyword,
			"time" => TokenKind::TimeKeyword,
			"timestamp" => TokenKind::TimestampKeyword,
			"timestamptz" => TokenKind::TimestampTzKeyword,
			"timetz" => TokenKind::TimeTzKeyword,
			"true" => TokenKind::TrueKeyword,
			"use" => TokenKind::UseKeyword,
			"var" => TokenKind::VarKeyword,
			"void" => TokenKind::VoidKeyword,
			"where" => TokenKind::WhereKeyword,
			"while" => TokenKind::WhileKeyword,
			"with" => TokenKind::WithKeyword,
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

	fn lex_quoted_identifier(&mut self) -> Result<(TokenKind, String), LexError> {
		let start = self.position;
		self.advance_char();

		let mut value = String::new();

		while let Some(next) = self.peek_char() {
			if next == '"' {
				self.advance_char();

				if self.peek_char() == Some('"') {
					self.advance_char();
					value.push('"');
					continue;
				}

				return Ok((TokenKind::Identifier, value));
			}

			self.advance_char();
			value.push(next);
		}

		Err(LexError {
			position: start,
			message: String::from("Unterminated quoted identifier."),
		})
	}

	fn lex_string_token(&mut self, mode: StringTokenMode) -> Result<Token, LexError> {
		let start = self.position;
		let layout = match mode {
			StringTokenMode::Continuation => self.interpolated_string_stack
				.last()
				.copied()
				.unwrap_or(MultilineStringLayout {
					first_line_padding: 0,
					trim_count: 0,
				}),
			StringTokenMode::Start => self.scan_multiline_string_layout(start)?,
		};

		if matches!(mode, StringTokenMode::Start) {
			self.advance_char();
		}

		let mut value = String::new();

		while let Some(next) = self.peek_char() {
			if next == '\'' {
				self.advance_char();
				let value = layout.apply(&value, matches!(mode, StringTokenMode::Start));

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
				let value = layout.apply(&value, matches!(mode, StringTokenMode::Start));

				if matches!(mode, StringTokenMode::Start) {
					self.interpolated_string_stack.push(layout);
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

	fn lex_temporal_literal(&mut self) -> Result<(TokenKind, String), LexError> {
		let start = self.position;
		self.advance_char();

		let mut digit_count = 0;
		while self.peek_char().is_some_and(|next| {
			next.is_ascii_digit() || matches!(next, '-' | ':' | '.' | 'T' | 'Z' | '+' )
		}) {
			if self.peek_char().unwrap().is_ascii_digit() {
				digit_count += 1;
			}
			self.advance_char();
		}

		if digit_count == 0 {
			return Err(LexError {
				position: start,
				message: String::from("Expected a date/time literal after `@`."),
			});
		}

		Ok((TokenKind::DateLiteral, self.source.as_str()[start..self.position].to_string()))
	}

	fn line_character_count_including(&self, position: usize) -> usize {
		let prefix = &self.source.as_str()[..position];
		let line_start = prefix.rfind('\n').map_or(0, |index| index + 1);

		prefix[line_start..].chars().count() + 1
	}

	fn peek_char(&self) -> Option<char> {
		self.source.as_str()[self.position..].chars().next()
	}

	fn peek_next_char(&self) -> Option<char> {
		let mut chars = self.source.as_str()[self.position..].chars();
		chars.next()?;
		chars.next()
	}

	fn peek_next_two_chars_are(&self, expected: &str) -> bool {
		self.source.as_str()[self.position..].starts_with(expected)
	}

	fn scan_interpolation_end(&self, mut position: usize) -> Result<usize, LexError> {
		while position < self.source.as_str().len() {
			let next = self.source.as_str()[position..].chars().next().unwrap();

			if next == '\'' {
				position = self.scan_string_end(position)?;
				continue;
			}

			position += next.len_utf8();

			if next == '}' {
				return Ok(position);
			}
		}

		Err(LexError {
			position,
			message: String::from("Unterminated string interpolation."),
		})
	}

	fn scan_multiline_string_layout(&self, start: usize) -> Result<MultilineStringLayout, LexError> {
		let end = self.scan_string_end(start)?;
		let source = self.source.as_str();
		let opening_column = self.line_character_count_including(start);
		let content = &source[start + '\''.len_utf8()..end - '\''.len_utf8()];

		if !content.contains('\n') {
			return Ok(MultilineStringLayout {
				first_line_padding: 0,
				trim_count: 0,
			});
		}

		let content_lines: Vec<&str> = content.split('\n').collect();
		let mut minimum_indent = opening_column;

		for (index, line) in content_lines.iter().enumerate().skip(1) {
			let mut indent = line.chars()
				.take_while(|ch| ch.is_whitespace() && *ch != '\r')
				.count();

			let is_last_line = index == content_lines.len() - 1;
			let line_is_only_whitespace = line.chars().all(|ch| ch.is_whitespace() && ch != '\r');

			if is_last_line && line_is_only_whitespace {
				// Treat an indentation-only final line as including the closing quote.
				indent += 1;
			}

			minimum_indent = minimum_indent.min(indent);
		}

		Ok(MultilineStringLayout {
			first_line_padding: opening_column.saturating_sub(minimum_indent),
			trim_count: minimum_indent,
		})
	}

	fn scan_string_end(&self, start: usize) -> Result<usize, LexError> {
		let mut position = start + '\''.len_utf8();

		while position < self.source.as_str().len() {
			let next = self.source.as_str()[position..].chars().next().unwrap();

			if next == '\\' {
				position += next.len_utf8();

				if position >= self.source.as_str().len() {
					return Err(LexError {
						position,
						message: String::from("Unterminated escape sequence in string literal."),
					});
				}

				let escaped = self.source.as_str()[position..].chars().next().unwrap();
				position += escaped.len_utf8();
				continue;
			}

			if next == '\'' {
				return Ok(position + next.len_utf8());
			}

			if next == '$' {
				let mut chars = self.source.as_str()[position..].chars();
				chars.next();
				if chars.next() == Some('{') {
					position = self.scan_interpolation_end(position + 2)?;
					continue;
				}
			}

			position += next.len_utf8();
		}

		Err(LexError {
			position: start,
			message: String::from("Unterminated string literal."),
		})
	}

	fn skip_ignored_text(&mut self) -> Result<(), LexError> {
		loop {
			let start = self.position;

			while self.peek_char().is_some_and(char::is_whitespace) {
				self.advance_char();
			}

			if self.peek_next_two_chars_are("//") {
				self.advance_char();
				self.advance_char();
				self.skip_single_line_comment();
				continue;
			}

			if self.peek_next_two_chars_are("/*") {
				self.skip_multiline_comment()?;
				continue;
			}

			if self.position == start {
				return Ok(());
			}
		}
	}

	fn skip_multiline_comment(&mut self) -> Result<(), LexError> {
		let start = self.position;
		let mut depth = 0usize;

		while self.position < self.source.as_str().len() {
			if self.peek_next_two_chars_are("/*") {
				self.advance_char();
				self.advance_char();
				depth += 1;
				continue;
			}

			if self.peek_next_two_chars_are("*/") {
				self.advance_char();
				self.advance_char();
				depth -= 1;

				if depth == 0 {
					return Ok(());
				}

				continue;
			}

			self.advance_char();
		}

		Err(LexError {
			position: start,
			message: String::from("Unterminated multi-line comment."),
		})
	}

	fn skip_single_line_comment(&mut self) {
		while let Some(next) = self.peek_char() {
			if next == '\n' || self.peek_next_two_chars_are("/*") {
				return;
			}

			self.advance_char();
		}
	}
}

impl MultilineStringLayout {
	fn apply(&self, value: &str, is_start_segment: bool) -> String {
		let first_line_padding = if is_start_segment {
			self.first_line_padding
		}
		else {
			0
		};

		trim_multiline_string(value, self.trim_count, first_line_padding)
	}
}

fn trim_multiline_string(value: &str, trim_count: usize, first_line_padding: usize) -> String {
	if !value.contains('\n') || trim_count == 0 {
		return value.to_string();
	}

	let mut result = String::new();
	let mut at_line_start = false;
	let mut remaining_trim = trim_count;

	if first_line_padding > 0 {
		result.push_str(&" ".repeat(first_line_padding));
	}

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
	fn preserves_first_line_indentation_in_multiline_string() {
		let mut lexer = Lexer::new(SourceText::new(
			"foo = '    - Bar\n               - Baz';",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[2].kind, TokenKind::StringLiteral);
		assert_eq!(tokens[2].lexeme, "    - Bar\n        - Baz");
	}

	#[test]
	fn rejects_unexpected_character() {
		let mut lexer = Lexer::new(SourceText::new("1 $ 2"));
		let error = lexer.tokenize().unwrap_err();

		assert_eq!(error.position, 2);
		assert_eq!(error.message, "Unexpected character `$`.");
	}

	#[test]
	fn rejects_unterminated_quoted_identifier() {
		let mut lexer = Lexer::new(SourceText::new("\"unfinished"));
		let error = lexer.tokenize().unwrap_err();

		assert_eq!(error.position, 0);
		assert_eq!(error.message, "Unterminated quoted identifier.");
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
	fn tokenizes_ampersand() {
		let mut lexer = Lexer::new(SourceText::new("&x"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[0].kind, TokenKind::Ampersand);
		assert_eq!(tokens[1].kind, TokenKind::Identifier);
		assert_eq!(tokens[2].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_array_literal_brackets() {
		let mut lexer = Lexer::new(SourceText::new("[1, 2]"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 6);
		assert_eq!(tokens[0].kind, TokenKind::LeftBracket);
		assert_eq!(tokens[1].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[2].kind, TokenKind::Comma);
		assert_eq!(tokens[3].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[4].kind, TokenKind::RightBracket);
		assert_eq!(tokens[5].kind, TokenKind::EndOfFile);
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
	fn tokenizes_function_signature_punctuation() {
		let mut lexer = Lexer::new(SourceText::new("fn add(a: int, b: int) int { return a; }"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[0].kind, TokenKind::FnKeyword);
		assert_eq!(tokens[4].kind, TokenKind::Colon);
		assert_eq!(tokens[6].kind, TokenKind::Comma);
		assert_eq!(tokens[8].kind, TokenKind::Colon);
		assert_eq!(tokens[13].kind, TokenKind::ReturnKeyword);
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
	fn tokenizes_keyword_literals() {
		let mut lexer = Lexer::new(SourceText::new("any asc and bool break by const continue count create date dec desc else find first fn for from if in last mut new not order or pub rec return text time timestamp timestamptz timetz true use var void where while with xor"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 44);
		assert_eq!(tokens[0].kind, TokenKind::AnyKeyword);
		assert_eq!(tokens[0].lexeme, "any");
		assert_eq!(tokens[1].kind, TokenKind::AscKeyword);
		assert_eq!(tokens[2].kind, TokenKind::AndKeyword);
		assert_eq!(tokens[3].kind, TokenKind::BoolKeyword);
		assert_eq!(tokens[4].kind, TokenKind::BreakKeyword);
		assert_eq!(tokens[5].kind, TokenKind::ByKeyword);
		assert_eq!(tokens[6].kind, TokenKind::ConstKeyword);
		assert_eq!(tokens[7].kind, TokenKind::ContinueKeyword);
		assert_eq!(tokens[8].kind, TokenKind::CountKeyword);
		assert_eq!(tokens[9].kind, TokenKind::CreateKeyword);
		assert_eq!(tokens[10].kind, TokenKind::DateKeyword);
		assert_eq!(tokens[11].kind, TokenKind::DecKeyword);
		assert_eq!(tokens[12].kind, TokenKind::DescKeyword);
		assert_eq!(tokens[13].kind, TokenKind::ElseKeyword);
		assert_eq!(tokens[14].kind, TokenKind::FindKeyword);
		assert_eq!(tokens[15].kind, TokenKind::FirstKeyword);
		assert_eq!(tokens[16].kind, TokenKind::FnKeyword);
		assert_eq!(tokens[17].kind, TokenKind::ForKeyword);
		assert_eq!(tokens[18].kind, TokenKind::FromKeyword);
		assert_eq!(tokens[19].kind, TokenKind::IfKeyword);
		assert_eq!(tokens[20].kind, TokenKind::InKeyword);
		assert_eq!(tokens[21].kind, TokenKind::LastKeyword);
		assert_eq!(tokens[22].kind, TokenKind::MutKeyword);
		assert_eq!(tokens[23].kind, TokenKind::NewKeyword);
		assert_eq!(tokens[24].kind, TokenKind::NotKeyword);
		assert_eq!(tokens[25].kind, TokenKind::OrderKeyword);
		assert_eq!(tokens[26].kind, TokenKind::OrKeyword);
		assert_eq!(tokens[27].kind, TokenKind::PubKeyword);
		assert_eq!(tokens[28].kind, TokenKind::RecKeyword);
		assert_eq!(tokens[29].kind, TokenKind::ReturnKeyword);
		assert_eq!(tokens[30].kind, TokenKind::TextKeyword);
		assert_eq!(tokens[31].kind, TokenKind::TimeKeyword);
		assert_eq!(tokens[32].kind, TokenKind::TimestampKeyword);
		assert_eq!(tokens[33].kind, TokenKind::TimestampTzKeyword);
		assert_eq!(tokens[34].kind, TokenKind::TimeTzKeyword);
		assert_eq!(tokens[35].kind, TokenKind::TrueKeyword);
		assert_eq!(tokens[36].kind, TokenKind::UseKeyword);
		assert_eq!(tokens[37].kind, TokenKind::VarKeyword);
		assert_eq!(tokens[38].kind, TokenKind::VoidKeyword);
		assert_eq!(tokens[39].kind, TokenKind::WhereKeyword);
		assert_eq!(tokens[40].kind, TokenKind::WhileKeyword);
		assert_eq!(tokens[41].kind, TokenKind::WithKeyword);
		assert_eq!(tokens[42].kind, TokenKind::XorKeyword);
		assert_eq!(tokens[43].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_date_literal() {
		let mut lexer = Lexer::new(SourceText::new("@2025-06-10"));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 2);
		assert_eq!(tokens[0].kind, TokenKind::DateLiteral);
		assert_eq!(tokens[0].lexeme, "@2025-06-10");
		assert_eq!(tokens[1].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_multiline_interpolated_string_using_global_minimum_indentation() {
		let mut lexer = Lexer::new(SourceText::new(
			"    '\n        hello ${name}\n      world\n    '",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[0].kind, TokenKind::InterpolatedStringStart);
		assert_eq!(tokens[0].lexeme, "\n   hello ");
		assert_eq!(tokens[3].kind, TokenKind::InterpolatedStringEnd);
		assert_eq!(tokens[3].lexeme, "\n world\n");
	}

	#[test]
	fn tokenizes_multiline_interpolated_string_with_indentation_trimming() {
		let mut lexer = Lexer::new(SourceText::new(
			"    '\n        hello ${name}\n        world\n    '",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[0].kind, TokenKind::InterpolatedStringStart);
		assert_eq!(tokens[0].lexeme, "\n   hello ");
		assert_eq!(tokens[3].kind, TokenKind::InterpolatedStringEnd);
		assert_eq!(tokens[3].lexeme, "\n   world\n");
	}

	#[test]
	fn tokenizes_multiline_string_trim_based_on_opening_delimiter_position() {
		let mut lexer = Lexer::new(SourceText::new(
			"var foo: text ='- Bar\n                    - Baz';",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[5].kind, TokenKind::StringLiteral);
		assert_eq!(tokens[5].lexeme, "- Bar\n    - Baz");
	}

	#[test]
	fn tokenizes_multiline_string_with_indentation_trimming() {
		let mut lexer = Lexer::new(SourceText::new(
			"    '\n        first\n        second\n    '",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
		assert_eq!(tokens[0].lexeme, "\n   first\n   second\n");
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
	fn tokenizes_quoted_identifier() {
		let mut lexer = Lexer::new(SourceText::new("\"Customer Name\""));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 2);
		assert_eq!(tokens[0].kind, TokenKind::Identifier);
		assert_eq!(tokens[0].lexeme, "Customer Name");
		assert_eq!(tokens[1].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_quoted_identifier_matching_keyword() {
		let mut lexer = Lexer::new(SourceText::new("\"return\""));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 2);
		assert_eq!(tokens[0].kind, TokenKind::Identifier);
		assert_eq!(tokens[0].lexeme, "return");
		assert_eq!(tokens[1].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_quoted_identifier_with_embedded_quote() {
		let mut lexer = Lexer::new(SourceText::new("\"say \"\"hello\"\"\""));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 2);
		assert_eq!(tokens[0].kind, TokenKind::Identifier);
		assert_eq!(tokens[0].lexeme, "say \"hello\"");
		assert_eq!(tokens[1].kind, TokenKind::EndOfFile);
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

	#[test]
	fn tokenizes_time_related_temporal_literals() {
		for source in ["@12:34", "@11:22:33+04:30", "@2019-11-28T05:19:03", "@2009-01-09T13:47Z"] {
			let mut lexer = Lexer::new(SourceText::new(source));
			let tokens = lexer.tokenize().unwrap();

			assert_eq!(tokens.len(), 2);
			assert_eq!(tokens[0].kind, TokenKind::DateLiteral);
			assert_eq!(tokens[0].lexeme, source);
			assert_eq!(tokens[1].kind, TokenKind::EndOfFile);
		}
	}

	#[test]
	fn trims_using_closing_delimiter_line_when_it_is_the_smallest() {
		let mut lexer = Lexer::new(SourceText::new(
			"foo = '- Bar\n           - Baz\n  ';",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens[2].kind, TokenKind::StringLiteral);
		assert_eq!(tokens[2].lexeme, "    - Bar\n        - Baz\n");
	}

	#[test]
	fn tokenizes_nested_multiline_comments() {
		let mut lexer = Lexer::new(SourceText::new(
			"var x: int = 1; /* outer /* inner */ still outer */ x",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 9);
		assert_eq!(tokens[0].kind, TokenKind::VarKeyword);
		assert_eq!(tokens[1].kind, TokenKind::Identifier);
		assert_eq!(tokens[2].kind, TokenKind::Colon);
		assert_eq!(tokens[3].kind, TokenKind::IntKeyword);
		assert_eq!(tokens[4].kind, TokenKind::Equal);
		assert_eq!(tokens[5].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[6].kind, TokenKind::Semicolon);
		assert_eq!(tokens[7].kind, TokenKind::Identifier);
		assert_eq!(tokens[8].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_single_line_comments() {
		let mut lexer = Lexer::new(SourceText::new(
			"var x: int = 1; // ignore this\nx",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 9);
		assert_eq!(tokens[0].kind, TokenKind::VarKeyword);
		assert_eq!(tokens[7].kind, TokenKind::Identifier);
		assert_eq!(tokens[7].lexeme, "x");
		assert_eq!(tokens[8].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn tokenizes_single_line_comment_until_multiline_comment_starts() {
		let mut lexer = Lexer::new(SourceText::new(
			"1 // ignored /* block */ + 2",
		));
		let tokens = lexer.tokenize().unwrap();

		assert_eq!(tokens.len(), 4);
		assert_eq!(tokens[0].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[1].kind, TokenKind::Plus);
		assert_eq!(tokens[2].kind, TokenKind::IntegerLiteral);
		assert_eq!(tokens[3].kind, TokenKind::EndOfFile);
	}

	#[test]
	fn rejects_unterminated_multiline_comment() {
		let mut lexer = Lexer::new(SourceText::new("1 /* missing end"));
		let error = lexer.tokenize().unwrap_err();

		assert_eq!(error.position, 2);
		assert_eq!(error.message, "Unterminated multi-line comment.");
	}
}
