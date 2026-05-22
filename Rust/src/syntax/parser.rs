use crate::ast::BinaryExpr;
use crate::ast::BinaryOperator;
use crate::ast::Expr;
use crate::ast::IntegerLiteral;

use super::token::Token;
use super::token::TokenKind;

pub struct Parser {
	position: usize,
	tokens: Vec<Token>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
// IMPORTANT: Do not reorder these variants. Their declaration order defines
// the operator precedence used by the Pratt parser.
enum BindingPower {
	Default,
	Additive,
	Multiplicative,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
	pub message: String,
	pub position: usize,
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Self {
		Self {
			position: 0,
			tokens,
		}
	}

	pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
		let expression = self.parse_expression_with_binding_power(BindingPower::Default)?;
		self.expect_end_of_file()?;
		Ok(expression)
	}

	fn current(&self) -> Option<&Token> {
		self.tokens.get(self.position)
	}

	fn expect_end_of_file(&self) -> Result<(), ParseError> {
		match self.current() {
			Some(token) if token.kind == TokenKind::EndOfFile => Ok(()),
			Some(token) => Err(ParseError {
				message: format!("Unexpected token `{}` after end of expression.", token.lexeme),
				position: token.start,
			}),
			None => Err(ParseError {
				message: String::from("Expected end of input."),
				position: 0,
			}),
		}
	}

	fn next(&mut self) -> Option<Token> {
		let token = self.current()?.clone();
		self.position += 1;
		Some(token)
	}

	fn parse_expression_with_binding_power(&mut self, binding_power: BindingPower) -> Result<Expr, ParseError> {
		let mut left = self.parse_prefix()?;

		while let Some(token) = self.current() {
			let next_binding_power = match token.kind {
				TokenKind::Plus | TokenKind::Dash => BindingPower::Additive,
				TokenKind::Asterisk
				| TokenKind::ForwardSlash
				| TokenKind::Percent => BindingPower::Multiplicative,
				_ => break,
			};

			if next_binding_power <= binding_power {
				break;
			}

			let operator = self.next().unwrap();
			left = self.parse_infix(left, operator, next_binding_power)?;
		}

		Ok(left)
	}

	fn parse_group_expression(&mut self, start: usize) -> Result<Expr, ParseError> {
		let expression = self.parse_expression_with_binding_power(BindingPower::Default)?;
		let closing = self.next().ok_or(ParseError {
			message: String::from("Expected `)` to close grouped expression."),
			position: start,
		})?;

		if closing.kind != TokenKind::RightParenthesis {
			return Err(ParseError {
				message: format!("Expected `)` to close grouped expression, found `{}`.", closing.lexeme),
				position: closing.start,
			});
		}

		Ok(expression)
	}

	fn parse_infix(&mut self, left: Expr, operator: Token, binding_power: BindingPower) -> Result<Expr, ParseError> {
		match operator.kind {
			TokenKind::Asterisk => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Multiply,
					right: Box::new(right),
				}))
			}
			TokenKind::Dash => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Subtract,
					right: Box::new(right),
				}))
			}
			TokenKind::ForwardSlash => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Divide,
					right: Box::new(right),
				}))
			}
			TokenKind::Percent => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Modulo,
					right: Box::new(right),
				}))
			}
			TokenKind::Plus => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Add,
					right: Box::new(right),
				}))
			}
			_ => Err(ParseError {
				message: format!("Unexpected infix operator `{}`.", operator.lexeme),
				position: operator.start,
			}),
		}
	}

	fn parse_integer_literal(&self, token: Token) -> Result<Expr, ParseError> {
		let digits: String = token.lexeme.chars().filter(|c| *c != '_').collect();
		let value = digits.parse::<i64>().map_err(|_| ParseError {
			message: format!("Integer literal `{}` is out of range.", token.lexeme),
			position: token.start,
		})?;

		Ok(Expr::Integer(IntegerLiteral {
			value,
		}))
	}

	fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
		let token = self.next().ok_or(ParseError {
			message: String::from("Expected an expression."),
			position: 0,
		})?;

		match token.kind {
			TokenKind::IntegerLiteral => self.parse_integer_literal(token),
			TokenKind::LeftParenthesis => self.parse_group_expression(token.start),
			TokenKind::EndOfFile => Err(ParseError {
				message: String::from("Expected an expression."),
				position: token.start,
			}),
			_ => Err(ParseError {
				message: format!("Unexpected token `{}` at start of expression.", token.lexeme),
				position: token.start,
			}),
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::ast::BinaryExpr;
	use crate::ast::BinaryOperator;
	use crate::ast::Expr;
	use crate::ast::IntegerLiteral;
	use crate::source::SourceText;

	use super::super::lexer::Lexer;
	use super::Parser;

	fn parse(source: &str) -> Expr {
		let mut lexer = Lexer::new(SourceText::new(source));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		parser.parse_expression().unwrap()
	}

	#[test]
	fn parses_grouped_expression() {
		assert_eq!(
			parse("(1 + 2) * 3"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Integer(IntegerLiteral {
						value: 1,
					})),
					operator: BinaryOperator::Add,
					right: Box::new(Expr::Integer(IntegerLiteral {
						value: 2,
					})),
				})),
				operator: BinaryOperator::Multiply,
				right: Box::new(Expr::Integer(IntegerLiteral {
					value: 3,
				})),
			})
		);
	}

	#[test]
	fn parses_integer_literal() {
		assert_eq!(
			parse("42"),
			Expr::Integer(IntegerLiteral { value: 42 })
		);
	}

	#[test]
	fn parses_left_associative_addition() {
		assert_eq!(
			parse("1 + 2 + 3"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Integer(IntegerLiteral {
						value: 1,
					})),
					operator: BinaryOperator::Add,
					right: Box::new(Expr::Integer(IntegerLiteral {
						value: 2,
					})),
				})),
				operator: BinaryOperator::Add,
				right: Box::new(Expr::Integer(IntegerLiteral {
					value: 3,
				})),
			})
		);
	}

	#[test]
	fn parses_left_associative_subtraction() {
		assert_eq!(
			parse("9 - 3 - 1"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Integer(IntegerLiteral {
						value: 9,
					})),
					operator: BinaryOperator::Subtract,
					right: Box::new(Expr::Integer(IntegerLiteral {
						value: 3,
					})),
				})),
				operator: BinaryOperator::Subtract,
				right: Box::new(Expr::Integer(IntegerLiteral {
					value: 1,
				})),
			})
		);
	}

	#[test]
	fn parses_multiplicative_precedence_over_addition() {
		assert_eq!(
			parse("1 + 2 * 3"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Integer(IntegerLiteral {
					value: 1,
				})),
				operator: BinaryOperator::Add,
				right: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Integer(IntegerLiteral {
						value: 2,
					})),
					operator: BinaryOperator::Multiply,
					right: Box::new(Expr::Integer(IntegerLiteral {
						value: 3,
					})),
				})),
			})
		);
	}

	#[test]
	fn rejects_missing_closing_parenthesis() {
		let mut lexer = Lexer::new(SourceText::new("(1 + 2"));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		let error = parser.parse_expression().unwrap_err();

		assert_eq!(error.message, "Expected `)` to close grouped expression, found ``.");
		assert_eq!(error.position, 6);
	}

	#[test]
	fn rejects_trailing_operator() {
		let mut lexer = Lexer::new(SourceText::new("1 +"));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		let error = parser.parse_expression().unwrap_err();

		assert_eq!(error.message, "Expected an expression.");
		assert_eq!(error.position, 3);
	}

	#[test]
	fn rejects_trailing_tokens() {
		let mut lexer = Lexer::new(SourceText::new("1 2"));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		let error = parser.parse_expression().unwrap_err();

		assert_eq!(error.message, "Unexpected token `2` after end of expression.");
		assert_eq!(error.position, 2);
	}
}
