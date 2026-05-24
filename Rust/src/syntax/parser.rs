use crate::ast::AssignmentExpr;
use crate::ast::AssignmentOperator;
use crate::ast::BinaryExpr;
use crate::ast::BinaryOperator;
use crate::ast::BooleanLiteral;
use crate::ast::DataType;
use crate::ast::DecimalLiteral;
use crate::ast::Expr;
use crate::ast::IdentifierExpr;
use crate::ast::IntegerLiteral;
use crate::ast::Program;
use crate::ast::Statement;
use crate::ast::TextLiteral;
use crate::ast::UnaryExpr;
use crate::ast::UnaryOperator;
use crate::ast::VariableDeclaration;
use crate::value::Decimal;

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
	LogicalOr,
	LogicalXor,
	LogicalAnd,
	Equality,
	Comparison,
	Additive,
	Multiplicative,
	Unary,
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
		let expression = self.parse_assignment_expression()?;
		self.expect_end_of_file()?;
		Ok(expression)
	}

	pub fn parse_program(&mut self) -> Result<Program, ParseError> {
		let mut statements = Vec::new();
		let mut result = None;

		loop {
			match self.current() {
				Some(token) if token.kind == TokenKind::EndOfFile => break,
				Some(token) if token.kind == TokenKind::VarKeyword => {
					statements.push(self.parse_variable_declaration_statement()?);
				}
				Some(_) => {
					let expression = self.parse_assignment_expression()?;

					if self.current().is_some_and(|token| token.kind == TokenKind::Semicolon) {
						self.next();
						statements.push(Statement::Expression(expression));
						continue;
					}

					result = Some(expression);
					break;
				}
				None => break,
			}
		}

		self.expect_end_of_file()?;

		Ok(Program {
			result,
			statements,
		})
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

	fn expect_token(&mut self, expected_kind: TokenKind, message: &str) -> Result<Token, ParseError> {
		let token = self.next().ok_or(ParseError {
			message: String::from(message),
			position: 0,
		})?;

		if token.kind != expected_kind {
			return Err(ParseError {
				message: format!("{message} Found `{}` instead.", token.lexeme),
				position: token.start,
			});
		}

		Ok(token)
	}

	fn next(&mut self) -> Option<Token> {
		let token = self.current()?.clone();
		self.position += 1;
		Some(token)
	}

	fn parse_assignment_expression(&mut self) -> Result<Expr, ParseError> {
		let left = self.parse_expression_with_binding_power(BindingPower::Default)?;
		let Some(token) = self.current() else {
			return Ok(left);
		};

		let operator = match token.kind {
			TokenKind::Equal => AssignmentOperator::Assign,
			TokenKind::MinusEqual => AssignmentOperator::SubtractAssign,
			TokenKind::MultiplyEqual => AssignmentOperator::MultiplyAssign,
			TokenKind::PercentEqual => AssignmentOperator::ModuloAssign,
			TokenKind::PlusEqual => AssignmentOperator::AddAssign,
			TokenKind::SlashEqual => AssignmentOperator::DivideAssign,
			_ => return Ok(left),
		};

		let target = match left {
			Expr::Identifier(target) => target,
			_ => {
				return Err(ParseError {
					message: String::from("Assignment target must be an identifier."),
					position: token.start,
				});
			}
		};

		self.next();
		let value = self.parse_assignment_expression()?;

		Ok(Expr::Assignment(AssignmentExpr {
			operator,
			target,
			value: Box::new(value),
		}))
	}

	fn parse_boolean_literal(&self, token: Token) -> Result<Expr, ParseError> {
		let value = match token.kind {
			TokenKind::FalseKeyword => false,
			TokenKind::TrueKeyword => true,
			_ => {
				return Err(ParseError {
					message: format!("Token `{}` is not a Boolean literal.", token.lexeme),
					position: token.start,
				});
			}
		};

		Ok(Expr::Boolean(BooleanLiteral {
			value,
		}))
	}

	fn parse_data_type(&mut self) -> Result<DataType, ParseError> {
		let token = self.next().ok_or(ParseError {
			message: String::from("Expected a data type."),
			position: 0,
		})?;

		match token.kind {
			TokenKind::BoolKeyword => Ok(DataType::Bool),
			TokenKind::DecKeyword => Ok(DataType::Dec),
			TokenKind::IntKeyword => Ok(DataType::Int),
			TokenKind::TextKeyword => Ok(DataType::Text),
			_ => Err(ParseError {
				message: format!("Expected a supported data type, found `{}`.", token.lexeme),
				position: token.start,
			}),
		}
	}

	fn parse_decimal_literal(&self, token: Token) -> Result<Expr, ParseError> {
		let value = Decimal::from_literal(&token.lexeme).map_err(|message| ParseError {
			message,
			position: token.start,
		})?;

		Ok(Expr::Decimal(DecimalLiteral {
			value,
		}))
	}

	fn parse_expression_with_binding_power(&mut self, binding_power: BindingPower) -> Result<Expr, ParseError> {
		let mut left = self.parse_prefix()?;

		while let Some(token) = self.current() {
			let next_binding_power = match token.kind {
				TokenKind::AndKeyword => BindingPower::LogicalAnd,
				TokenKind::Asterisk => BindingPower::Multiplicative,
				TokenKind::BangEqual => BindingPower::Equality,
				TokenKind::Dash => BindingPower::Additive,
				TokenKind::EqualEqual => BindingPower::Equality,
				TokenKind::ForwardSlash => BindingPower::Multiplicative,
				TokenKind::GreaterThan => BindingPower::Comparison,
				TokenKind::GreaterThanOrEqual => BindingPower::Comparison,
				TokenKind::LessThan => BindingPower::Comparison,
				TokenKind::LessThanOrEqual => BindingPower::Comparison,
				TokenKind::OrKeyword => BindingPower::LogicalOr,
				TokenKind::Percent => BindingPower::Multiplicative,
				TokenKind::Plus => BindingPower::Additive,
				TokenKind::XorKeyword => BindingPower::LogicalXor,
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

	fn parse_identifier_expression(&self, token: Token) -> Expr {
		Expr::Identifier(IdentifierExpr {
			name: token.lexeme,
		})
	}

	fn parse_infix(&mut self, left: Expr, operator: Token, binding_power: BindingPower) -> Result<Expr, ParseError> {
		match operator.kind {
			TokenKind::AndKeyword => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::And,
					right: Box::new(right),
				}))
			}
			TokenKind::Asterisk => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Multiply,
					right: Box::new(right),
				}))
			}
			TokenKind::BangEqual => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::NotEqual,
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
			TokenKind::EqualEqual => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Equal,
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
			TokenKind::GreaterThan => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::GreaterThan,
					right: Box::new(right),
				}))
			}
			TokenKind::GreaterThanOrEqual => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::GreaterThanOrEqual,
					right: Box::new(right),
				}))
			}
			TokenKind::LessThan => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::LessThan,
					right: Box::new(right),
				}))
			}
			TokenKind::LessThanOrEqual => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::LessThanOrEqual,
					right: Box::new(right),
				}))
			}
			TokenKind::OrKeyword => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Or,
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
			TokenKind::XorKeyword => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Xor,
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

	fn parse_interpolated_string(&mut self, token: Token) -> Result<Expr, ParseError> {
		let mut parts = vec![Expr::Text(TextLiteral {
			value: token.lexeme,
		})];

		loop {
			let expression = self.parse_assignment_expression()?;
			parts.push(expression);

			self.expect_token(TokenKind::RightBrace, "Expected `}` to close string interpolation.")?;
			let segment = self.next().ok_or(ParseError {
				message: String::from("Expected a string segment after interpolation."),
				position: 0,
			})?;

			match segment.kind {
				TokenKind::InterpolatedStringMiddle => {
					parts.push(Expr::Text(TextLiteral {
						value: segment.lexeme,
					}));
				}
				TokenKind::InterpolatedStringEnd => {
					parts.push(Expr::Text(TextLiteral {
						value: segment.lexeme,
					}));
					break;
				}
				_ => {
					return Err(ParseError {
						message: format!("Expected interpolated string continuation, found `{}`.", segment.lexeme),
						position: segment.start,
					});
				}
			}
		}

		let mut parts = parts.into_iter();
		let mut expression = parts.next().unwrap();

		for part in parts {
			expression = Expr::Binary(BinaryExpr {
				left: Box::new(expression),
				operator: BinaryOperator::Add,
				right: Box::new(part),
			});
		}

		Ok(expression)
	}

	fn parse_negation_expression(&mut self) -> Result<Expr, ParseError> {
		let operand = self.parse_expression_with_binding_power(BindingPower::Unary)?;

		Ok(Expr::Unary(UnaryExpr {
			operand: Box::new(operand),
			operator: UnaryOperator::Negate,
		}))
	}

	fn parse_not_expression(&mut self) -> Result<Expr, ParseError> {
		let operand = self.parse_expression_with_binding_power(BindingPower::Unary)?;

		Ok(Expr::Unary(UnaryExpr {
			operand: Box::new(operand),
			operator: UnaryOperator::Not,
		}))
	}

	fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
		let token = self.next().ok_or(ParseError {
			message: String::from("Expected an expression."),
			position: 0,
		})?;

		match token.kind {
			TokenKind::Dash => self.parse_negation_expression(),
			TokenKind::DecimalLiteral => self.parse_decimal_literal(token),
			TokenKind::EndOfFile => Err(ParseError {
				message: String::from("Expected an expression."),
				position: token.start,
			}),
			TokenKind::FalseKeyword | TokenKind::TrueKeyword => self.parse_boolean_literal(token),
			TokenKind::Identifier => Ok(self.parse_identifier_expression(token)),
			TokenKind::IntegerLiteral => self.parse_integer_literal(token),
			TokenKind::InterpolatedStringStart => self.parse_interpolated_string(token),
			TokenKind::LeftParenthesis => self.parse_group_expression(token.start),
			TokenKind::NotKeyword => self.parse_not_expression(),
			TokenKind::StringLiteral => Ok(self.parse_text_literal(token)),
			_ => Err(ParseError {
				message: format!("Unexpected token `{}` at start of expression.", token.lexeme),
				position: token.start,
			}),
		}
	}

	fn parse_text_literal(&self, token: Token) -> Expr {
		Expr::Text(TextLiteral {
			value: token.lexeme,
		})
	}

	fn parse_variable_declaration_statement(&mut self) -> Result<Statement, ParseError> {
		self.expect_token(TokenKind::VarKeyword, "Expected `var` to start variable declaration.")?;
		let name = self.expect_token(TokenKind::Identifier, "Expected variable name.")?;
		self.expect_token(TokenKind::Colon, "Expected `:` after variable name.")?;
		let data_type = self.parse_data_type()?;
		let initial_value = if self.current().is_some_and(|token| token.kind == TokenKind::Equal) {
			self.next();
			Some(self.parse_assignment_expression()?)
		} else {
			None
		};
		self.expect_token(TokenKind::Semicolon, "Expected `;` after variable declaration.")?;

		Ok(Statement::VariableDeclaration(VariableDeclaration {
			data_type,
			initial_value,
			name: name.lexeme,
		}))
	}
}

#[cfg(test)]
mod tests {
	use crate::ast::AssignmentExpr;
	use crate::ast::AssignmentOperator;
	use crate::ast::BinaryExpr;
	use crate::ast::BinaryOperator;
	use crate::ast::BooleanLiteral;
	use crate::ast::DataType;
	use crate::ast::DecimalLiteral;
	use crate::ast::Expr;
	use crate::ast::IdentifierExpr;
	use crate::ast::IntegerLiteral;
	use crate::ast::Program;
	use crate::ast::Statement;
	use crate::ast::TextLiteral;
	use crate::ast::UnaryExpr;
	use crate::ast::UnaryOperator;
	use crate::ast::VariableDeclaration;
	use crate::source::SourceText;
	use crate::value::Decimal;

	use super::super::lexer::Lexer;
	use super::Parser;

	fn parse(source: &str) -> Expr {
		let mut lexer = Lexer::new(SourceText::new(source));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		parser.parse_expression().unwrap()
	}

	fn parse_program(source: &str) -> Program {
		let mut lexer = Lexer::new(SourceText::new(source));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		parser.parse_program().unwrap()
	}

	#[test]
	fn parses_assignment_right_associatively() {
		assert_eq!(
			parse("x = y = 1"),
			Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::Assign,
				target: IdentifierExpr {
					name: String::from("x"),
				},
				value: Box::new(Expr::Assignment(AssignmentExpr {
					operator: AssignmentOperator::Assign,
					target: IdentifierExpr {
						name: String::from("y"),
					},
					value: Box::new(Expr::Integer(IntegerLiteral {
						value: 1,
					})),
				})),
			})
		);
	}

	#[test]
	fn parses_boolean_literal() {
		assert_eq!(
			parse("true"),
			Expr::Boolean(BooleanLiteral {
				value: true,
			})
		);
	}

	#[test]
	fn parses_compound_assignment_expression() {
		assert_eq!(
			parse("x += 1"),
			Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::AddAssign,
				target: IdentifierExpr {
					name: String::from("x"),
				},
				value: Box::new(Expr::Integer(IntegerLiteral {
					value: 1,
				})),
			})
		);
	}

	#[test]
	fn parses_comparison_more_tightly_than_equality() {
		assert_eq!(
			parse("1 < 2 == true"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Integer(IntegerLiteral {
						value: 1,
					})),
					operator: BinaryOperator::LessThan,
					right: Box::new(Expr::Integer(IntegerLiteral {
						value: 2,
					})),
				})),
				operator: BinaryOperator::Equal,
				right: Box::new(Expr::Boolean(BooleanLiteral {
					value: true,
				})),
			})
		);
	}

	#[test]
	fn parses_decimal_literal() {
		assert_eq!(
			parse(".5"),
			Expr::Decimal(DecimalLiteral {
				value: Decimal::from_literal(".5").unwrap(),
			})
		);
	}

	#[test]
	fn parses_expression_statement_before_final_expression() {
		assert_eq!(
			parse_program("var x: int = 1;\nx += 2;\nx"),
			Program {
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Int,
						initial_value: Some(Expr::Integer(IntegerLiteral {
							value: 1,
						})),
						name: String::from("x"),
					}),
					Statement::Expression(Expr::Assignment(AssignmentExpr {
						operator: AssignmentOperator::AddAssign,
						target: IdentifierExpr {
							name: String::from("x"),
						},
						value: Box::new(Expr::Integer(IntegerLiteral {
							value: 2,
						})),
					})),
				],
				result: Some(Expr::Identifier(IdentifierExpr {
					name: String::from("x"),
				})),
			}
		);
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
	fn parses_interpolated_string() {
		assert_eq!(
			parse("'hello ${name}!'"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Text(TextLiteral {
						value: String::from("hello "),
					})),
					operator: BinaryOperator::Add,
					right: Box::new(Expr::Identifier(IdentifierExpr {
						name: String::from("name"),
					})),
				})),
				operator: BinaryOperator::Add,
				right: Box::new(Expr::Text(TextLiteral {
					value: String::from("!"),
				})),
			})
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
	fn parses_logical_and_more_tightly_than_or() {
		assert_eq!(
			parse("true or false and true"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Boolean(BooleanLiteral {
					value: true,
				})),
				operator: BinaryOperator::Or,
				right: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Boolean(BooleanLiteral {
						value: false,
					})),
					operator: BinaryOperator::And,
					right: Box::new(Expr::Boolean(BooleanLiteral {
						value: true,
					})),
				})),
			})
		);
	}

	#[test]
	fn parses_logical_xor_between_and_and_or() {
		assert_eq!(
			parse("true or false xor true and false"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Boolean(BooleanLiteral {
					value: true,
				})),
				operator: BinaryOperator::Or,
				right: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Boolean(BooleanLiteral {
						value: false,
					})),
					operator: BinaryOperator::Xor,
					right: Box::new(Expr::Binary(BinaryExpr {
						left: Box::new(Expr::Boolean(BooleanLiteral {
							value: true,
						})),
						operator: BinaryOperator::And,
						right: Box::new(Expr::Boolean(BooleanLiteral {
							value: false,
						})),
					})),
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
	fn parses_not_expression() {
		assert_eq!(
			parse("not false"),
			Expr::Unary(UnaryExpr {
				operand: Box::new(Expr::Boolean(BooleanLiteral {
					value: false,
				})),
				operator: UnaryOperator::Not,
			})
		);
	}

	#[test]
	fn parses_program_with_variable_declarations() {
		assert_eq!(
			parse_program("var x: int = 1;\nvar y: int = 2;\nx + y"),
			Program {
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Int,
						initial_value: Some(Expr::Integer(IntegerLiteral {
							value: 1,
						})),
						name: String::from("x"),
					}),
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Int,
						initial_value: Some(Expr::Integer(IntegerLiteral {
							value: 2,
						})),
						name: String::from("y"),
					}),
				],
				result: Some(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Identifier(IdentifierExpr {
						name: String::from("x"),
					})),
					operator: BinaryOperator::Add,
					right: Box::new(Expr::Identifier(IdentifierExpr {
						name: String::from("y"),
					})),
				})),
			}
		);
	}

	#[test]
	fn parses_relational_operator_after_addition() {
		assert_eq!(
			parse("1 + 2 < 4"),
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
				operator: BinaryOperator::LessThan,
				right: Box::new(Expr::Integer(IntegerLiteral {
					value: 4,
				})),
			})
		);
	}

	#[test]
	fn parses_string_literal() {
		assert_eq!(
			parse("'hello'"),
			Expr::Text(TextLiteral {
				value: String::from("hello"),
			})
		);
	}

	#[test]
	fn parses_text_variable_declaration() {
		assert_eq!(
			parse_program("var name: text = 'Tablo';"),
			Program {
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Text,
						initial_value: Some(Expr::Text(TextLiteral {
							value: String::from("Tablo"),
						})),
						name: String::from("name"),
					}),
				],
				result: None,
			}
		);
	}

	#[test]
	fn parses_unary_negation() {
		assert_eq!(
			parse("-42"),
			Expr::Unary(UnaryExpr {
				operand: Box::new(Expr::Integer(IntegerLiteral {
					value: 42,
				})),
				operator: UnaryOperator::Negate,
			})
		);
	}

	#[test]
	fn parses_unary_negation_before_multiplication() {
		assert_eq!(
			parse("-2 * 3"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Unary(UnaryExpr {
					operand: Box::new(Expr::Integer(IntegerLiteral {
						value: 2,
					})),
					operator: UnaryOperator::Negate,
				})),
				operator: BinaryOperator::Multiply,
				right: Box::new(Expr::Integer(IntegerLiteral {
					value: 3,
				})),
			})
		);
	}

	#[test]
	fn parses_variable_declaration_without_initializer() {
		assert_eq!(
			parse_program("var x: int;"),
			Program {
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Int,
						initial_value: None,
						name: String::from("x"),
					}),
				],
				result: None,
			}
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
