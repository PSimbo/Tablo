#![allow(non_camel_case_types)]

use std::fmt::Display;

use TokenType::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenType {
	TOKEN_COMMENT,
	TOKEN_END_OF_FILE,
	TOKEN_IDENTIFIER,
	TOKEN_LIT_DECIMAL,
	TOKEN_LIT_HEXADECIMAL,
	TOKEN_LIT_INTEGER,
	TOKEN_LIT_OCTAL,
	TOKEN_LIT_STRING,
	TOKEN_OP_ADDITION, // `+`
	TOKEN_OP_ADDITION_ASSIGNMENT, // `+=`
	TOKEN_OP_ASSIGNMENT, // `=`
	TOKEN_OP_DIVISION, // `/`
	TOKEN_OP_DIVISION_ASSIGNMENT, // `/=`
	TOKEN_OP_EQUAL, // `==`
	TOKEN_OP_GREATER_THAN, // `>`
	TOKEN_OP_GREATER_THAN_OR_EQUAL, // `>=`
	TOKEN_OP_LESS_THAN, // `<`
	TOKEN_OP_LESS_THAN_OR_EQUAL, // `<=`
	TOKEN_OP_MODULUS, // `%`
	TOKEN_OP_MODULUS_ASSIGNMENT, // `%=`
	TOKEN_OP_MULTIPLICATION, // `*`
	TOKEN_OP_MULTIPLICATION_ASSIGNMENT, // `*=`
	TOKEN_OP_NOT_EQUAL, // `!=`
	TOKEN_OP_SUBTRACTION, // `-`
	TOKEN_OP_SUBTRACTION_ASSIGNMENT, // `-=`
	TOKEN_KW_AND, // `and`
	TOKEN_KW_ELSE, // `else`
	TOKEN_KW_FALSE, // `false`
	TOKEN_KW_FOR, // `for`
	TOKEN_KW_IF, // `if`
	TOKEN_KW_NOT, // `not`
	TOKEN_KW_NULL, // `null`
	TOKEN_KW_OR, // `or`
	TOKEN_KW_RETURN, // `return`
	TOKEN_KW_TRUE, // `true`
	TOKEN_KW_VAR, // `var`
	TOKEN_KW_WHERE, // `where`
	TOKEN_SYM_ARRAY_END, // `]`
	TOKEN_SYM_ARRAY_START, // `[`
	TOKEN_SYM_BLOCK_END, // `}`
	TOKEN_SYM_BLOCK_START, // `{`
	TOKEN_SYM_DECIMAL_SEPARATOR, // `.`
	TOKEN_SYM_EXCLAMATION, // `!` -- Not currently used for anything but reserved for future use.
	// TOKEN_SYM_FIELD_SEPARATOR, // `.`
	TOKEN_SYM_GROUPING_END, // `)`
	TOKEN_SYM_GROUPING_START, // `(`
	TOKEN_SYM_IDENTIFIER_DELIMITER, // `"`
	TOKEN_SYM_LIST_SEPARATOR, // `,`
	TOKEN_SYM_STRING_DELIMITER, // `'`
	TOKEN_SYM_STRING_INTERPOLATION_END, // `}`
	TOKEN_SYM_STRING_INTERPOLATION_START, // `${`
	TOKEN_SYM_TERMINATOR, // `;`
	TOKEN_SYM_TYPE_SEPARATOR, // `:`
}

impl Display for TokenType {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			TOKEN_COMMENT => write!(f, "COMMENT"),
			TOKEN_END_OF_FILE => write!(f, "END OF FILE"),
			TOKEN_IDENTIFIER => write!(f, "IDENTIFIER"),
			TOKEN_KW_AND => write!(f, "LOGICAL AND"),
			TOKEN_KW_ELSE => write!(f, "KEYWORD ELSE"),
			TOKEN_KW_FALSE => write!(f, "KEYWORD FALSE"),
			TOKEN_KW_FOR => write!(f, "KEYWORD FOR"),
			TOKEN_KW_IF => write!(f, "KEYWORD IF"),
			TOKEN_KW_NOT => write!(f, "LOGICAL NOT"),
			TOKEN_KW_NULL => write!(f, "NULL"),
			TOKEN_KW_OR => write!(f, "LOGICAL OR"),
			TOKEN_KW_RETURN => write!(f, "KEYWORD RETURN"),
			TOKEN_KW_TRUE => write!(f, "KEYWORD TRUE"),
			TOKEN_KW_VAR => write!(f, "KEYWORD VAR"),
			TOKEN_KW_WHERE => write!(f, "KEYWORD WHERE"),
			TOKEN_LIT_DECIMAL => write!(f, "DECIMAL LITERAL"),
			TOKEN_LIT_HEXADECIMAL => write!(f, "HEXADECIMAL LITERAL"),
			TOKEN_LIT_INTEGER => write!(f, "INTEGER LITERAL"),
			TOKEN_LIT_OCTAL => write!(f, "OCTAL LITERAL"),
			TOKEN_LIT_STRING => write!(f, "STRING LITERAL"),
			TOKEN_OP_ADDITION => write!(f, "PLUS"),
			TOKEN_OP_ADDITION_ASSIGNMENT => write!(f, "PLUS EQUALS"),
			TOKEN_OP_ASSIGNMENT => write!(f, "ASSIGNMENT"),
			TOKEN_OP_DIVISION => write!(f, "DIVIDE"),
			TOKEN_OP_DIVISION_ASSIGNMENT => write!(f, "DIVIDE EQUALS"),
			TOKEN_OP_EQUAL => write!(f, "IS EQUAL"),
			TOKEN_OP_GREATER_THAN => write!(f, "GREATER THAN"),
			TOKEN_OP_GREATER_THAN_OR_EQUAL => write!(f, "GREATER THAN OR EQUAL"),
			TOKEN_OP_LESS_THAN => write!(f, "LESS THAN"),
			TOKEN_OP_LESS_THAN_OR_EQUAL => write!(f, "LESS THAN OR EQUAL"),
			TOKEN_OP_MODULUS => write!(f, "MODULUS"),
			TOKEN_OP_MODULUS_ASSIGNMENT => write!(f, "MODULO EQUALS"),
			TOKEN_OP_MULTIPLICATION => write!(f, "MULTIPLY"),
			TOKEN_OP_MULTIPLICATION_ASSIGNMENT => write!(f, "TIMES EQUALS"),
			TOKEN_OP_NOT_EQUAL => write!(f, "NOT EQUAL"),
			TOKEN_OP_SUBTRACTION => write!(f, "MINUS"),
			TOKEN_OP_SUBTRACTION_ASSIGNMENT => write!(f, "MINUS EQUALS"),
			TOKEN_SYM_ARRAY_END => write!(f, "CLOSE ARRAY"),
			TOKEN_SYM_ARRAY_START => write!(f, "OPEN ARRAY"),
			TOKEN_SYM_BLOCK_END => write!(f, "CLOSE BLOCK"),
			TOKEN_SYM_BLOCK_START => write!(f, "OPEN BLOCK"),
			TOKEN_SYM_DECIMAL_SEPARATOR => write!(f, "DECIMAL SEPARATOR"),
			TOKEN_SYM_EXCLAMATION => write!(f, "EXCLAMATION MARK"),
			// TOKEN_SYM_FIELD_SEPARATOR => write!(f, "FIELD SEPARATOR"),
			TOKEN_SYM_GROUPING_END => write!(f, "CLOSE PARENTHESIS"),
			TOKEN_SYM_GROUPING_START => write!(f, "OPEN PARENTHESIS"),
			TOKEN_SYM_IDENTIFIER_DELIMITER => write!(f, "IDENTIFIER DELIMITER"),
			TOKEN_SYM_LIST_SEPARATOR => write!(f, "COMMA"),
			TOKEN_SYM_STRING_DELIMITER => write!(f, "STRING DELIMITER"),
			TOKEN_SYM_STRING_INTERPOLATION_END => write!(f, "END STRING INTERP"),
			TOKEN_SYM_STRING_INTERPOLATION_START => write!(f, "START STRING INTERP"),
			TOKEN_SYM_TERMINATOR => write!(f, "TERMINATOR"),
			TOKEN_SYM_TYPE_SEPARATOR => write!(f, "COLON"),
		}
	}
}

#[allow(unused)]
#[derive(Clone)]
pub struct Token {
	pub column: usize,
	pub end: usize, // Byte position of end of token.
	pub line: usize,
	pub start: usize, // Byte position of start of token.
	pub token_type: TokenType,
}

impl Token {
	fn len_bytes(&self, lexer: &Lexer) -> usize {
		lexer.source[self.start..self.end].len()
	}

	fn len_chars(&self, lexer: &Lexer) -> usize {
		lexer.source[self.start..self.end].chars().count()
	}

	fn new(lexer: &Lexer) -> Self {
		Self {
			column: lexer.current.column,
			end: lexer.current.position,
			line: lexer.current.line,
			start: lexer.current.position,
			token_type: TOKEN_END_OF_FILE,
		}
	}

	fn new_fixed_length(lexer: &Lexer, token_type: TokenType) -> Self {
		let mut token = Token::new(lexer);

		token.token_type = token_type;
		token.end = token.start + match token_type {
			TOKEN_OP_ADDITION
			| TOKEN_OP_ASSIGNMENT
			| TOKEN_OP_DIVISION
			| TOKEN_OP_GREATER_THAN
			| TOKEN_OP_LESS_THAN
			| TOKEN_OP_MODULUS
			| TOKEN_OP_MULTIPLICATION
			| TOKEN_OP_SUBTRACTION
			| TOKEN_SYM_ARRAY_END
			| TOKEN_SYM_ARRAY_START
			| TOKEN_SYM_BLOCK_END
			| TOKEN_SYM_BLOCK_START
			| TOKEN_SYM_DECIMAL_SEPARATOR
			| TOKEN_SYM_EXCLAMATION
			// | TOKEN_SYM_FIELD_SEPARATOR
			| TOKEN_SYM_GROUPING_END
			| TOKEN_SYM_GROUPING_START
			| TOKEN_SYM_IDENTIFIER_DELIMITER
			| TOKEN_SYM_LIST_SEPARATOR
			| TOKEN_SYM_STRING_DELIMITER
			| TOKEN_SYM_STRING_INTERPOLATION_END
			| TOKEN_SYM_TERMINATOR
			| TOKEN_SYM_TYPE_SEPARATOR => 1,
			TOKEN_OP_ADDITION_ASSIGNMENT
			| TOKEN_OP_DIVISION_ASSIGNMENT
			| TOKEN_OP_EQUAL
			| TOKEN_OP_GREATER_THAN_OR_EQUAL
			| TOKEN_OP_LESS_THAN_OR_EQUAL
			| TOKEN_OP_MODULUS_ASSIGNMENT
			| TOKEN_OP_MULTIPLICATION_ASSIGNMENT
			| TOKEN_OP_NOT_EQUAL
			| TOKEN_OP_SUBTRACTION_ASSIGNMENT
			| TOKEN_SYM_STRING_INTERPOLATION_START => 2,
			_ => unreachable!("Variable length tokens cannot be created using this method.")
		};

		token
	}

	fn new_variable_length(lexer: &Lexer, token_type: TokenType, len: usize) -> Self {
		let mut token = Token::new(lexer);

		token.token_type = token_type;
		token.end = token.start + len;

		token
	}

	pub fn to_str<'lexer>(&self, lexer: &'lexer Lexer) -> &'lexer str {
		&lexer.source[self.start..self.end]
	}
}

pub struct Lexer {
	current: LexPosition,
	pub emit_comments: bool,
	previous: LexPosition,
	pub source: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum LexContext {
	Default,
	QuotedIdentifier,
	String,
}

#[derive(Clone, Copy)]
struct LexPosition {
	column: usize,
	context: LexContext,
	line: usize,
	position: usize, // Current byte position into source string.
	string_nesting_level: usize, // For nesting strings inside string interpolations.
}

impl Lexer {
	pub fn end_of_file(&self) -> bool {
		self.current.position >= self.source.len()
	}

	pub fn new(source: impl Into<String>) -> Self {
		Self {
			current: LexPosition {
				column: 1,
				context: LexContext::Default,
				line: 1,
				position: 0,
				string_nesting_level: 0,
			},
			emit_comments: false,
			previous: LexPosition {
				column: 1,
				context: LexContext::Default,
				line: 1,
				position: 0,
				string_nesting_level: 0,
			},
			source: source.into(),
		}
	}

	pub fn next(&mut self) -> Token {
		self.get_token(true)
	}

	#[allow(unused)]
	pub fn peek(&mut self) -> Token {
		self.get_token(false)
	}

	#[allow(unused)]
	pub fn reset(&mut self) {
		self.current.column = 1;
		self.current.context = LexContext::Default;
		self.current.line = 1;
		self.current.position = 0;
		self.current.string_nesting_level = 0;
		self.previous.column = 1;
		self.previous.context = LexContext::Default;
		self.previous.line = 1;
		self.previous.position = 0;
		self.previous.string_nesting_level = 0;
	}

	#[allow(unused)]
	pub fn rewind(&mut self) {
		self.current = self.previous;
	}

	fn check_for_keyword_and_update_token(&self, token: &mut Token) {
		token.token_type = match token.to_str(self) {
			"and" => TOKEN_KW_AND,
			"else" => TOKEN_KW_ELSE,
			"false" => TOKEN_KW_FALSE,
			"for" => TOKEN_KW_FOR,
			"if" => TOKEN_KW_IF,
			"not" => TOKEN_KW_NOT,
			"null" => TOKEN_KW_NULL,
			"or" => TOKEN_KW_OR,
			"return" => TOKEN_KW_RETURN,
			"true" => TOKEN_KW_TRUE,
			"var" => TOKEN_KW_VAR,
			"where" => TOKEN_KW_WHERE,
			_ => token.token_type,
		};
	}

	fn expect(s: &str, predicate: impl Fn(char) -> bool, msg: impl Into<String>) -> Result<char, String> {
		let c = s.chars().next();

		let c = match c {
			Some(c) => c,
			None => return Err("Unexpected end of file.".to_string()),
		};

		if !predicate(c) {
			return Err(format!("{} Found `{}`", msg.into(), c));
		}

		Ok(c)
	}

	#[inline]
	fn get_source_at_current(&self) -> &str {
		&self.source[self.current.position..]
	}

	fn get_token(&mut self, advance: bool) -> Token {
		let token = match self.current.context {
			LexContext::Default => self.lex_token(advance),
			LexContext::QuotedIdentifier => self.lex_quoted_identifier(advance),
			LexContext::String => self.lex_string(advance),
		};

		let token = match token {
			Ok(t) => t,
			Err(e) => panic!("{}", e),
		};

		if advance {
			self.previous = self.current;

			self.current.column += token.len_chars(self);
			self.current.position += token.len_bytes(self);
		}

		token
	}

	#[inline]
	fn is_decimal_character(c: char) -> bool {
		c >= '0' && c <= '9'
	}

	#[inline]
	fn is_hexadecimal_character(c: char) -> bool {
		c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'
	}

	#[inline]
	fn is_octal_character(c: char) -> bool {
		c >= '0' && c <= '7'
	}

	fn is_unquoted_identifier_character(c: char, first: bool) -> bool {
		(!first && (c >= '0' && c <= '9')) || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_'
	}

	fn lex_comment(&mut self) -> Result<Token, String> {
		let source = self.get_source_at_current();

		let source = if let Some(idx) = source.find('\n') {
			&source[..idx]
		}
		else {
			source
		};

		let mut token = Token::new(&self);
		token.token_type = TOKEN_COMMENT;
		token.end = token.start + source.len();

		Ok(token)
	}

	fn lex_decimal_literal(&self) -> Result<Token, String> {
		let source = self.get_source_at_current();
		let first_char = source.chars().next().unwrap();

		let mut p: usize = 0;

		if first_char != '.' {
			let prefix = self.lex_integral_literal(source, 10, "decimal")?;
			p += prefix.len_bytes(self);
		}

		Lexer::expect(&source[p..], |c| c == '.', "Expected `.`.")?;
		p += 1;

		let suffix = self.lex_integral_literal(&source[p..], 10, "decimal")?;
		p += suffix.len_bytes(self);

		let mut token = Token::new(&self);
		token.token_type = TOKEN_LIT_DECIMAL;
		token.end = token.start + p;

		Ok(token)
	}

	fn lex_hexadecimal_literal(&self) -> Result<Token, String> {
		self.lex_integral_literal(self.get_source_at_current(), 16, "hexadecimal")
	}

	fn lex_integer_literal(&self) -> Result<Token, String> {
		self.lex_integral_literal(self.get_source_at_current(), 10, "integer")
	}

	fn lex_integral_literal(&self, source: &str, base: usize, label: &str) -> Result<Token, String> {
		let (token_type, prefix, predicate) = match base {
			8 => (TOKEN_LIT_OCTAL, 2, Lexer::is_octal_character as fn(char) -> bool),
			10 => (TOKEN_LIT_INTEGER, 0, Lexer::is_decimal_character as fn(char) -> bool),
			16 => (TOKEN_LIT_HEXADECIMAL, 2, Lexer::is_hexadecimal_character as fn(char) -> bool),
			_ => unreachable!("Invalid base passed to lex_integral_literal."),
		};

		let mut p: usize = prefix;

		match Lexer::expect(&source[p..], |c| predicate(c), format!("Expected a {} digit.", label)) {
			Ok(_) => {
				p += 1;
			},
			Err(e) => return Err(e),
		};

		while let Some(mut c) = source[p..].chars().next() {
			let mut q = p;

			if c == '_' {
				q += 1;
				c = if let Some(c2) = source[q..].chars().next() {
					c2
				}
				else {
					return Err("Unexpected end of file.".to_string());
				}
			}

			if !predicate(c) {
				break;
			}

			p = q + 1;
		}

		let mut token = Token::new(&self);
		token.token_type = token_type;
		token.end = token.start + p;

		Ok(token)
	}

	fn lex_octal_literal(&self) -> Result<Token, String> {
		self.lex_integral_literal(self.get_source_at_current(), 8, "octal")
	}

	fn lex_quoted_identifier(&mut self, advance: bool) -> Result<Token, String> {
		let source = self.get_source_at_current();
		if source.chars().next().is_some_and(|c| c == '"') {
			// End of identifier.
			if advance {
				self.current.context = LexContext::Default;
			}

			return Ok(Token::new_fixed_length(self, TOKEN_SYM_IDENTIFIER_DELIMITER));
		}

		let mut p: usize = 0;

		while let Some(c) = source[p..].chars().next() {
			if c == '"' {
				// Check for escaped double quote.
				let c2 = source[p..].chars().nth(1);

				if c2.is_some_and(|c| c == '"') {
					p += 2;
					continue;
				}

				return Ok(Token::new_variable_length(&self, TOKEN_IDENTIFIER, p));
			}

			p += c.len_utf8();
		}
		
		Err("Unexpected end of file.".to_string())
	}

	fn lex_string(&mut self, advance: bool) -> Result<Token, String> {
		let source = self.get_source_at_current();

		if let Some(c) = source.chars().next() {
			if c == '\'' {
				// Close string.
				if advance {
					self.current.context = LexContext::Default;
					self.current.string_nesting_level = std::cmp::max(self.current.string_nesting_level - 1, 0);
				}

				return Ok(Token::new_fixed_length(self, TOKEN_SYM_STRING_DELIMITER));
			}
			else if let Some(c2) = source.chars().nth(1) && c2 == '{' {
				// Start of string interpolation.
				if advance {
					self.current.context = LexContext::Default;
				}

				return Ok(Token::new_fixed_length(self, TOKEN_SYM_STRING_INTERPOLATION_START));
			}
		}

		let mut p: usize = 0;

		while let Some(c) = source[p..].chars().next() {
			// Check for possible string termination characters.
			if c == '\'' || c == '$' {
				let c2 = source[p..].chars().nth(1).unwrap_or('\0');

				if c == '\'' && c2 == '\'' {
					// Escaped single quote. Continue string.
					p += 2;
					continue;
				}
				else if c == '$' && c2 != '{' {
					// Not the start of string interpolation. Continue string.
					p += 1;
					continue;
				}

				// End of string.
				return Ok(Token::new_variable_length(self, TOKEN_LIT_STRING, p));
			}

			p += c.len_utf8();
		}
		
		Err("Unexpected end of file.".to_string())
	}

	fn lex_token(&mut self, advance: bool) -> Result<Token, String> {
		self.skip_non_token_characters();

		if self.end_of_file() {
			return Ok(Token::new(self));
		}

		let source = self.get_source_at_current();
		let c: char = source.chars().next().unwrap();

		match c {
			',' => Ok(Token::new_fixed_length(self, TOKEN_SYM_LIST_SEPARATOR)),
			';' => Ok(Token::new_fixed_length(self, TOKEN_SYM_TERMINATOR)),
			':' => Ok(Token::new_fixed_length(self, TOKEN_SYM_TYPE_SEPARATOR)),
			'(' => Ok(Token::new_fixed_length(self, TOKEN_SYM_GROUPING_START)),
			')' => Ok(Token::new_fixed_length(self, TOKEN_SYM_GROUPING_END)),
			'[' => Ok(Token::new_fixed_length(self, TOKEN_SYM_ARRAY_START)),
			']' => Ok(Token::new_fixed_length(self, TOKEN_SYM_ARRAY_END)),
			'{' => Ok(Token::new_fixed_length(self, TOKEN_SYM_BLOCK_START)),
			'}' => {
				// Might be the end of a string interpolation.
				if self.current.string_nesting_level > 0 {
					if advance {
						self.current.context = LexContext::String;
					}

					Ok(Token::new_fixed_length(self, TOKEN_SYM_STRING_INTERPOLATION_END))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_SYM_BLOCK_END))
				}
			},
			'\'' => {
				if advance {
					self.current.context = LexContext::String;
					self.current.string_nesting_level += 1;
				}

				Ok(Token::new_fixed_length(self, TOKEN_SYM_STRING_DELIMITER))
			},
			'"' => {
				if advance {
					self.current.context = LexContext::QuotedIdentifier;
				}

				Ok(Token::new_fixed_length(self, TOKEN_SYM_IDENTIFIER_DELIMITER))
			},
			'+' => {
				if source.chars().nth(1).is_some_and(|c| c == '=') {
					Ok(Token::new_fixed_length(self, TOKEN_OP_ADDITION_ASSIGNMENT))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_OP_ADDITION))
				}
			},
			'-' => {
				if source.chars().nth(1).is_some_and(|c| c == '=') {
					Ok(Token::new_fixed_length(self, TOKEN_OP_SUBTRACTION_ASSIGNMENT))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_OP_SUBTRACTION))
				}
			},
			'*' => {
				if source.chars().nth(1).is_some_and(|c| c == '=') {
					Ok(Token::new_fixed_length(self, TOKEN_OP_MULTIPLICATION_ASSIGNMENT))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_OP_MULTIPLICATION))
				}
			},
			'/' => {
				let token_type = source.chars().nth(1);

				if token_type.is_some_and(|c| c == '/') {
					self.lex_comment()
				}
				else if token_type.is_some_and(|c| c == '=') {
					Ok(Token::new_fixed_length(self, TOKEN_OP_DIVISION_ASSIGNMENT))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_OP_DIVISION))
				}
			},
			'%' => {
				if source.chars().nth(1).is_some_and(|c| c == '=') {
					Ok(Token::new_fixed_length(self, TOKEN_OP_MODULUS_ASSIGNMENT))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_OP_MODULUS))
				}
			},
			'=' => {
				if source.chars().nth(1).is_some_and(|c| c == '=') {
					Ok(Token::new_fixed_length(self, TOKEN_OP_EQUAL))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_OP_ASSIGNMENT))
				}
			},
			'!' => {
				if source.chars().nth(1).is_some_and(|c| c == '=') {
					Ok(Token::new_fixed_length(self, TOKEN_OP_NOT_EQUAL))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_SYM_EXCLAMATION))
				}
			},
			'<' => {
				if source.chars().nth(1).is_some_and(|c| c == '=') {
					Ok(Token::new_fixed_length(self, TOKEN_OP_LESS_THAN_OR_EQUAL))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_OP_LESS_THAN))
				}
			},
			'>' => {
				if source.chars().nth(1).is_some_and(|c| c == '=') {
					Ok(Token::new_fixed_length(self, TOKEN_OP_GREATER_THAN_OR_EQUAL))
				}
				else {
					Ok(Token::new_fixed_length(self, TOKEN_OP_GREATER_THAN))
				}
			},
'0'..='9' | '.' => {
				let c2 = source.chars().nth(1);

				if c2.is_none() {
					if c == '.' {
						Ok(Token::new_fixed_length(self, TOKEN_SYM_DECIMAL_SEPARATOR))
					}
					else {
						self.lex_integer_literal().map_err(|e| format!("Failed to parse integer literal. {}", e))
					}
				}
				else {
					let c2 = c2.unwrap();

					if c == '0' && (c2 == 'X' || c2 == 'x') {
						self.lex_hexadecimal_literal().map_err(|e| format!("Failed to parse hexadecimal literal. {}", e))
					}
					else if c == '0' && (c2 == 'O' || c2 == 'o') {
						self.lex_octal_literal().map_err(|e| format!("Failed to parse octal literal. {}", e))
					}
					else {
						// Could be either a decimal or integer literal. Try decimal first.
						match self.lex_decimal_literal() {
							Ok(t) => Ok(t),
							Err(_) => match c {
								'.' => {
									Ok(Token::new_fixed_length(self, TOKEN_SYM_DECIMAL_SEPARATOR))
								},
								_ => match self.lex_integer_literal() {
									Ok(t) => Ok(t),
									Err(e) => {
										// If we failed to parse a decimal but the first character is `.` then don't error -- return the token.
										if c == '.' {
											Ok(Token::new_fixed_length(self, TOKEN_SYM_DECIMAL_SEPARATOR))
										}
										else {
											Err(format!("Failed to parse decimal literal. {}", e))
										}
									},
								}
							}
						}
					}
				}
			},
			_ => {
				if !Lexer::is_unquoted_identifier_character(c, true) {
					return Err(format!("Expected identifier. Found: `{}`.", c));
				}

				self.lex_unquoted_identifier_or_keyword()
			},
		}
	}

	fn lex_unquoted_identifier_or_keyword(&self) -> Result<Token, String> {
		let source = self.get_source_at_current();
		let mut i: usize = 0;
		let mut p: usize = 0;

		while let Some(c) = source.chars().nth(i) && Lexer::is_unquoted_identifier_character(c, p == 0) {
			i += 1;
			p += c.len_utf8();
		}

		let mut token = Token::new(&self);
		token.token_type = TOKEN_IDENTIFIER;
		token.end = token.start + p;

		self.check_for_keyword_and_update_token(&mut token);

		Ok(token)
	}

	fn skip_comment(&mut self) {
		let source = &self.source[self.current.position..];

		let source = if let Some(idx) = source.find('\n') {
			&source[..idx]
		}
		else {
			source
		};

		self.current.column += source.chars().count();
		self.current.position += source.len();
	}

	fn skip_non_token_characters(&mut self) {
		while let Some(c) = self.source[self.current.position..].chars().next() {
			match c {
				' ' | '\t' => {
					self.current.column += 1;
					self.current.position += 1;
				},
				'\n' => {
					self.current.line += 1;
					self.current.column = 1;
					self.current.position += 1;
				},
				'\r' => {
					self.current.position += 1;
				},
				'/' => {
					if self.emit_comments {
						return;
					}

					if let Some(c2) = self.source[self.current.position..].chars().nth(1) {
						match c2 {
							'/' => {
								self.skip_comment();
							},
							_ => return,
						}
					}
				},
				_ => return,
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	macro_rules! assert_token {
		(
			$lexer:expr,
			$token:expr,
			$line:expr,
			$column:expr,
			$token_type:expr,
			$to_str:expr
			$(,)?
		) => {{
			let token = $token;
			assert_eq!(token.line, $line);
			assert_eq!(token.column, $column);
			assert_eq!(token.token_type, $token_type);
			assert_eq!(token.to_str($lexer), $to_str);
		}};
	}

	#[test]
	fn test_lex_boolean_literals() {
		let mut lexer = Lexer::new("true false");

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_KW_TRUE, "true");
		assert_token!(&lexer, lexer.next(), 1, 6, TOKEN_KW_FALSE, "false");
		assert_token!(&lexer, lexer.next(), 1, 11, TOKEN_END_OF_FILE, "");
	}

	#[test]
	fn test_lex_brackets() {
		let mut lexer = Lexer::new("([{x }  ]   )");

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_SYM_GROUPING_START, "(");
		assert_token!(&lexer, lexer.next(), 1, 2, TOKEN_SYM_ARRAY_START, "[");
		assert_token!(&lexer, lexer.next(), 1, 3, TOKEN_SYM_BLOCK_START, "{");
		assert_token!(&lexer, lexer.next(), 1, 4, TOKEN_IDENTIFIER, "x");
		assert_token!(&lexer, lexer.next(), 1, 6, TOKEN_SYM_BLOCK_END, "}");
		assert_token!(&lexer, lexer.next(), 1, 9, TOKEN_SYM_ARRAY_END, "]");
		assert_token!(&lexer, lexer.next(), 1, 13, TOKEN_SYM_GROUPING_END, ")");
		assert_token!(&lexer, lexer.next(), 1, 14, TOKEN_END_OF_FILE, "");
	}

	#[test]
	fn test_lex_comments() {
		let mut lexer = Lexer::new("x\ny //A comment\nz // On last line");

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_IDENTIFIER, "x");
		assert_token!(&lexer, lexer.next(), 2, 1, TOKEN_IDENTIFIER, "y");
		assert_token!(&lexer, lexer.next(), 3, 1, TOKEN_IDENTIFIER, "z");
		assert_token!(&lexer, lexer.next(), 3, 18, TOKEN_END_OF_FILE, "");

		let mut lexer = Lexer::new("x\ny //A comment\nz // On last line");
		lexer.emit_comments = true;

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_IDENTIFIER, "x");
		assert_token!(&lexer, lexer.next(), 2, 1, TOKEN_IDENTIFIER, "y");
		assert_token!(&lexer, lexer.next(), 2, 3, TOKEN_COMMENT, "//A comment");
		assert_token!(&lexer, lexer.next(), 3, 1, TOKEN_IDENTIFIER, "z");
		assert_token!(&lexer, lexer.next(), 3, 3, TOKEN_COMMENT, "// On last line");
		assert_token!(&lexer, lexer.next(), 3, 18, TOKEN_END_OF_FILE, "");
	}

	#[test]
	fn test_lex_comparison_operators() {
		let mut lexer = Lexer::new("0 < a <= 5 != 10 >= b > 20 == 0");

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_LIT_INTEGER, "0");
		assert_token!(&lexer, lexer.next(), 1, 3, TOKEN_OP_LESS_THAN, "<");
		assert_token!(&lexer, lexer.next(), 1, 5, TOKEN_IDENTIFIER, "a");
		assert_token!(&lexer, lexer.next(), 1, 7, TOKEN_OP_LESS_THAN_OR_EQUAL, "<=");
		assert_token!(&lexer, lexer.next(), 1, 10, TOKEN_LIT_INTEGER, "5");
		assert_token!(&lexer, lexer.next(), 1, 12, TOKEN_OP_NOT_EQUAL, "!=");
		assert_token!(&lexer, lexer.next(), 1, 15, TOKEN_LIT_INTEGER, "10");
		assert_token!(&lexer, lexer.next(), 1, 18, TOKEN_OP_GREATER_THAN_OR_EQUAL, ">=");
		assert_token!(&lexer, lexer.next(), 1, 21, TOKEN_IDENTIFIER, "b");
		assert_token!(&lexer, lexer.next(), 1, 23, TOKEN_OP_GREATER_THAN, ">");
		assert_token!(&lexer, lexer.next(), 1, 25, TOKEN_LIT_INTEGER, "20");
		assert_token!(&lexer, lexer.next(), 1, 28, TOKEN_OP_EQUAL, "==");
		assert_token!(&lexer, lexer.next(), 1, 31, TOKEN_LIT_INTEGER, "0");
		assert_token!(&lexer, lexer.next(), 1, 32, TOKEN_END_OF_FILE, "");
	}

	#[test]
	fn test_lex_identifiers() {
		let mut lexer = Lexer::new("foo bar_baz \"1st\" \"quoted ident\" \"π\" \"你好\" \"🌍\" \"inner \"\" quote\"");

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_IDENTIFIER, "foo");
		assert_token!(&lexer, lexer.next(), 1, 5, TOKEN_IDENTIFIER, "bar_baz");

		assert_token!(&lexer, lexer.next(), 1, 13, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");
		assert_token!(&lexer, lexer.next(), 1, 14, TOKEN_IDENTIFIER, "1st");
		assert_token!(&lexer, lexer.next(), 1, 17, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");
		
		assert_token!(&lexer, lexer.next(), 1, 19, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");
		assert_token!(&lexer, lexer.next(), 1, 20, TOKEN_IDENTIFIER, "quoted ident");
		assert_token!(&lexer, lexer.next(), 1, 32, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");

		assert_token!(&lexer, lexer.next(), 1, 34, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");
		assert_token!(&lexer, lexer.next(), 1, 35, TOKEN_IDENTIFIER, "π");
		assert_token!(&lexer, lexer.next(), 1, 36, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");

		assert_token!(&lexer, lexer.next(), 1, 38, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");
		assert_token!(&lexer, lexer.next(), 1, 39, TOKEN_IDENTIFIER, "你好");
		assert_token!(&lexer, lexer.next(), 1, 41, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");

		assert_token!(&lexer, lexer.next(), 1, 43, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");
		assert_token!(&lexer, lexer.next(), 1, 44, TOKEN_IDENTIFIER, "🌍");
		assert_token!(&lexer, lexer.next(), 1, 45, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");

		assert_token!(&lexer, lexer.next(), 1, 47, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");
		assert_token!(&lexer, lexer.next(), 1, 48, TOKEN_IDENTIFIER, "inner \"\" quote");
		assert_token!(&lexer, lexer.next(), 1, 62, TOKEN_SYM_IDENTIFIER_DELIMITER, "\"");

		assert_token!(&lexer, lexer.next(), 1, 63, TOKEN_END_OF_FILE, "");
	}

	#[test]
	fn test_lex_numeric_literals() {
		let mut lexer = Lexer::new("a + 0x12_ef * 0o12_67 / -2.5 % 5 - .123");

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_IDENTIFIER, "a");
		assert_token!(&lexer, lexer.next(), 1, 3, TOKEN_OP_ADDITION, "+");
		assert_token!(&lexer, lexer.next(), 1, 5, TOKEN_LIT_HEXADECIMAL, "0x12_ef");
		assert_token!(&lexer, lexer.next(), 1, 13, TOKEN_OP_MULTIPLICATION, "*");
		assert_token!(&lexer, lexer.next(), 1, 15, TOKEN_LIT_OCTAL, "0o12_67");
		assert_token!(&lexer, lexer.next(), 1, 23, TOKEN_OP_DIVISION, "/");
		assert_token!(&lexer, lexer.next(), 1, 25, TOKEN_OP_SUBTRACTION, "-");
		assert_token!(&lexer, lexer.next(), 1, 26, TOKEN_LIT_DECIMAL, "2.5");
		assert_token!(&lexer, lexer.next(), 1, 30, TOKEN_OP_MODULUS, "%");
		assert_token!(&lexer, lexer.next(), 1, 32, TOKEN_LIT_INTEGER, "5");
		assert_token!(&lexer, lexer.next(), 1, 34, TOKEN_OP_SUBTRACTION, "-");
		assert_token!(&lexer, lexer.next(), 1, 36, TOKEN_LIT_DECIMAL, ".123");
		assert_token!(&lexer, lexer.next(), 1, 40, TOKEN_END_OF_FILE, "");
	}

	#[test]
	fn test_lex_punctuation() {
		let mut lexer = Lexer::new("; : . ,");

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_SYM_TERMINATOR, ";");
		assert_token!(&lexer, lexer.next(), 1, 3, TOKEN_SYM_TYPE_SEPARATOR, ":");
		assert_token!(&lexer, lexer.next(), 1, 5, TOKEN_SYM_DECIMAL_SEPARATOR, ".");
		assert_token!(&lexer, lexer.next(), 1, 7, TOKEN_SYM_LIST_SEPARATOR, ",");
		assert_token!(&lexer, lexer.next(), 1, 8, TOKEN_END_OF_FILE, "");
	}

	#[test]
	fn test_lex_strings() {
		let mut lexer = Lexer::new("'foo' 'with space' 'bar ${ baz + 'nested' }' 'inner '' quote'");

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_SYM_STRING_DELIMITER, "'");
		assert_token!(&lexer, lexer.next(), 1, 2, TOKEN_LIT_STRING, "foo");
		assert_token!(&lexer, lexer.next(), 1, 5, TOKEN_SYM_STRING_DELIMITER, "'");

		assert_token!(&lexer, lexer.next(), 1, 7, TOKEN_SYM_STRING_DELIMITER, "'");
		assert_token!(&lexer, lexer.next(), 1, 8, TOKEN_LIT_STRING, "with space");
		assert_token!(&lexer, lexer.next(), 1, 18, TOKEN_SYM_STRING_DELIMITER, "'");

		assert_token!(&lexer, lexer.next(), 1, 20, TOKEN_SYM_STRING_DELIMITER, "'");
		assert_token!(&lexer, lexer.next(), 1, 21, TOKEN_LIT_STRING, "bar ");
		assert_token!(&lexer, lexer.next(), 1, 25, TOKEN_SYM_STRING_INTERPOLATION_START, "${");
		assert_token!(&lexer, lexer.next(), 1, 28, TOKEN_IDENTIFIER, "baz");
		assert_token!(&lexer, lexer.next(), 1, 32, TOKEN_OP_ADDITION, "+");
		assert_token!(&lexer, lexer.next(), 1, 34, TOKEN_SYM_STRING_DELIMITER, "'");
		assert_token!(&lexer, lexer.next(), 1, 35, TOKEN_LIT_STRING, "nested");
		assert_token!(&lexer, lexer.next(), 1, 41, TOKEN_SYM_STRING_DELIMITER, "'");
		assert_token!(&lexer, lexer.next(), 1, 43, TOKEN_SYM_STRING_INTERPOLATION_END, "}");
		assert_token!(&lexer, lexer.next(), 1, 44, TOKEN_SYM_STRING_DELIMITER, "'");

		assert_token!(&lexer, lexer.next(), 1, 46, TOKEN_SYM_STRING_DELIMITER, "'");
		assert_token!(&lexer, lexer.next(), 1, 47, TOKEN_LIT_STRING, "inner '' quote");
		assert_token!(&lexer, lexer.next(), 1, 61, TOKEN_SYM_STRING_DELIMITER, "'");

		assert_token!(&lexer, lexer.next(), 1, 62, TOKEN_END_OF_FILE, "");
	}

	#[test]
	fn test_lex_whitespace() {
		let src = "
	

        
  ";
		let mut lexer = Lexer::new(src);

		assert_token!(&lexer, lexer.next(), 5, 3, TOKEN_END_OF_FILE, "");
	}

	#[test]
	fn test_rewind() {
		let mut lexer = Lexer::new("first second third");

		assert_token!(&lexer, lexer.next(), 1, 1, TOKEN_IDENTIFIER, "first");
		assert_token!(&lexer, lexer.next(), 1, 7, TOKEN_IDENTIFIER, "second");

		lexer.rewind();
		assert_token!(&lexer, lexer.next(), 1, 7, TOKEN_IDENTIFIER, "second");
		assert_token!(&lexer, lexer.next(), 1, 14, TOKEN_IDENTIFIER, "third");

		lexer.rewind();
		assert_token!(&lexer, lexer.next(), 1, 14, TOKEN_IDENTIFIER, "third");

		assert_token!(&lexer, lexer.next(), 1, 19, TOKEN_END_OF_FILE, "");
	}
}
