use crate::ast::ArrayIndexAssignmentTarget;
use crate::ast::ArrayLiteral;
use crate::ast::AssignmentExpr;
use crate::ast::AssignmentOperator;
use crate::ast::AssignmentTarget;
use crate::ast::BinaryExpr;
use crate::ast::BinaryOperator;
use crate::ast::BlockStatement;
use crate::ast::BooleanLiteral;
use crate::ast::BreakStatement;
use crate::ast::CallArgument;
use crate::ast::CallExpr;
use crate::ast::ContinueStatement;
use crate::ast::DataType;
use crate::ast::DecimalLiteral;
use crate::ast::Expr;
use crate::ast::ForStatement;
use crate::ast::FunctionDeclaration;
use crate::ast::FunctionParameter;
use crate::ast::IdentifierExpr;
use crate::ast::IfStatement;
use crate::ast::IndexExpr;
use crate::ast::IntegerLiteral;
use crate::ast::Program;
use crate::ast::RangeExpr;
use crate::ast::ReturnStatement;
use crate::ast::Statement;
use crate::ast::TextLiteral;
use crate::ast::UnaryExpr;
use crate::ast::UnaryOperator;
use crate::ast::VariableDeclaration;
use crate::ast::WhileStatement;
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
	Call,
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
		let mut functions = Vec::new();
		let mut statements = Vec::new();
		let mut result = None;

		loop {
			match self.current() {
				Some(token) if token.kind == TokenKind::EndOfFile => break,
				Some(token) if token.kind == TokenKind::FnKeyword => {
					functions.push(self.parse_function_declaration()?);
				}
				Some(token) if matches!(token.kind, TokenKind::BreakKeyword | TokenKind::ConstKeyword | TokenKind::ContinueKeyword | TokenKind::ForKeyword | TokenKind::IfKeyword | TokenKind::LeftBrace | TokenKind::ReturnKeyword | TokenKind::VarKeyword | TokenKind::WhileKeyword) => {
					statements.push(self.parse_statement()?);
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
			functions,
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

	fn parse_array_literal(&mut self, start: usize) -> Result<Expr, ParseError> {
		let mut elements = Vec::new();

		if !self.current().is_some_and(|token| token.kind == TokenKind::RightBracket) {
			loop {
				elements.push(self.parse_assignment_expression()?);

				match self.current() {
					Some(token) if token.kind == TokenKind::Comma => {
						self.next();
					}
					_ => break,
				}
			}
		}

		self.expect_token(TokenKind::RightBracket, "Expected `]` to close array literal.")?;

		Ok(Expr::Array(ArrayLiteral {
			elements,
			position: start,
		}))
	}

	fn parse_assignment_expression(&mut self) -> Result<Expr, ParseError> {
		let left = self.parse_range_expression()?;
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
		let operator_position = token.start;

		let target = match left {
			Expr::Identifier(target) => AssignmentTarget::Identifier(target),
			Expr::Index(IndexExpr { array, index, position }) => {
				let array = match *array {
					Expr::Identifier(identifier) => identifier,
					_ => {
						return Err(ParseError {
							message: String::from("Indexed assignment target must be an identifier-based array access."),
							position: operator_position,
						});
					}
				};

				AssignmentTarget::Index(ArrayIndexAssignmentTarget {
					array,
					index,
					position,
				})
			}
			_ => {
				return Err(ParseError {
					message: String::from("Assignment target must be an identifier or indexed array element."),
					position: token.start,
				});
			}
		};

		self.next();
		let value = self.parse_assignment_expression()?;

		Ok(Expr::Assignment(AssignmentExpr {
			operator,
			position: operator_position,
			target,
			value: Box::new(value),
		}))
	}

	fn parse_block_statement(&mut self) -> Result<Statement, ParseError> {
		let start = self.expect_token(TokenKind::LeftBrace, "Expected `{` to start block.")?;
		let mut statements = Vec::new();

		loop {
			match self.current() {
				Some(token) if token.kind == TokenKind::EndOfFile => {
					return Err(ParseError {
						message: String::from("Expected `}` to close block."),
						position: start.start,
					});
				}
				Some(token) if token.kind == TokenKind::RightBrace => {
					self.next();
					break;
				}
				Some(_) => statements.push(self.parse_statement()?),
				None => {
					return Err(ParseError {
						message: String::from("Expected `}` to close block."),
						position: start.start,
					});
				}
			}
		}

		Ok(Statement::Block(BlockStatement {
			position: start.start,
			statements,
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
			position: token.start,
			value,
		}))
	}

	fn parse_break_statement(&mut self) -> Result<Statement, ParseError> {
		let break_keyword = self.expect_token(TokenKind::BreakKeyword, "Expected `break` to start break statement.")?;
		self.expect_token(TokenKind::Semicolon, "Expected `;` after break statement.")?;

		Ok(Statement::Break(BreakStatement {
			position: break_keyword.start,
		}))
	}

	fn parse_call_expression(&mut self, callee: Expr, start: usize) -> Result<Expr, ParseError> {
		let callee = match callee {
			Expr::Identifier(identifier) => identifier,
			_ => {
				return Err(ParseError {
					message: String::from("Function call target must be an identifier."),
					position: start,
				});
			}
		};

		let mut arguments = Vec::new();

		if !self.current().is_some_and(|token| token.kind == TokenKind::RightParenthesis) {
			loop {
				let argument_position = self.current().map_or(start, |token| token.start);
				let is_by_ref = if self.current().is_some_and(|token| token.kind == TokenKind::Ampersand) {
					self.next();
					true
				}
				else {
					false
				};
				let value = self.parse_assignment_expression()?;
				arguments.push(CallArgument {
					is_by_ref,
					position: argument_position,
					value,
				});

				match self.current() {
					Some(token) if token.kind == TokenKind::Comma => {
						self.next();
					}
					_ => break,
				}
			}
		}

		let closing = self.next().ok_or(ParseError {
			message: String::from("Expected `)` to close function call."),
			position: start,
		})?;

		if closing.kind != TokenKind::RightParenthesis {
			return Err(ParseError {
				message: format!("Expected `)` to close function call, found `{}`.", closing.lexeme),
				position: closing.start,
			});
		}

		Ok(Expr::Call(CallExpr {
			arguments,
			callee,
			position: start,
		}))
	}

	fn parse_continue_statement(&mut self) -> Result<Statement, ParseError> {
		let continue_keyword = self.expect_token(TokenKind::ContinueKeyword, "Expected `continue` to start continue statement.")?;
		self.expect_token(TokenKind::Semicolon, "Expected `;` after continue statement.")?;

		Ok(Statement::Continue(ContinueStatement {
			position: continue_keyword.start,
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
			TokenKind::LeftBracket => {
				let element_type = self.parse_data_type()?;
				self.expect_token(TokenKind::RightBracket, "Expected `]` after array element type.")?;
				Ok(DataType::Array(Box::new(element_type)))
			}
			TokenKind::TextKeyword => Ok(DataType::Text),
			TokenKind::VoidKeyword => Ok(DataType::Void),
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
			position: token.start,
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
				TokenKind::LeftBracket => BindingPower::Call,
				TokenKind::LeftParenthesis => BindingPower::Call,
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

	fn parse_for_statement(&mut self) -> Result<Statement, ParseError> {
		let for_keyword = self.expect_token(TokenKind::ForKeyword, "Expected `for` to start for statement.")?;
		let variable = self.expect_token(TokenKind::Identifier, "Expected loop variable name.")?;
		self.expect_token(TokenKind::InKeyword, "Expected `in` after loop variable name.")?;
		let iterable = self.parse_assignment_expression()?;
		let body = match self.parse_block_statement()? {
			Statement::Block(block) => block,
			_ => unreachable!("Block parser must return a block statement."),
		};

		Ok(Statement::For(ForStatement {
			body,
			iterable,
			position: for_keyword.start,
			variable: IdentifierExpr {
				name: variable.lexeme,
				position: variable.start,
			},
		}))
	}

	fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration, ParseError> {
		let function_keyword = self.expect_token(TokenKind::FnKeyword, "Expected `fn` to start function declaration.")?;
		let name = self.expect_token(TokenKind::Identifier, "Expected function name.")?;
		self.expect_token(TokenKind::LeftParenthesis, "Expected `(` after function name.")?;

		let mut parameters = Vec::new();

		if !self.current().is_some_and(|token| token.kind == TokenKind::RightParenthesis) {
			loop {
				parameters.push(self.parse_function_parameter()?);

				match self.current() {
					Some(token) if token.kind == TokenKind::Comma => {
						self.next();
					}
					_ => break,
				}
			}
		}

		self.expect_token(TokenKind::RightParenthesis, "Expected `)` after parameter list.")?;
		let return_type = self.parse_data_type()?;
		let body = match self.parse_block_statement()? {
			Statement::Block(block) => block,
			_ => unreachable!("Block parser must return a block statement."),
		};

		Ok(FunctionDeclaration {
			body,
			name: name.lexeme,
			parameters,
			position: function_keyword.start,
			return_type,
		})
	}

	fn parse_function_parameter(&mut self) -> Result<FunctionParameter, ParseError> {
		let name = self.expect_token(TokenKind::Identifier, "Expected parameter name.")?;
		self.expect_token(TokenKind::Colon, "Expected `:` after parameter name.")?;
		let is_by_ref = if self.current().is_some_and(|token| token.kind == TokenKind::Ampersand) {
			self.next();
			true
		}
		else {
			false
		};
		let data_type = self.parse_data_type()?;

		Ok(FunctionParameter {
			data_type,
			is_by_ref,
			name: name.lexeme,
			position: name.start,
		})
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
			position: token.start,
		})
	}

	fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
		let if_keyword = self.expect_token(TokenKind::IfKeyword, "Expected `if` to start if statement.")?;
		let condition = self.parse_assignment_expression()?;
		let then_branch = match self.parse_block_statement()? {
			Statement::Block(block) => block,
			_ => unreachable!("Block parser must return a block statement."),
		};

		let else_branch = if self.current().is_some_and(|token| token.kind == TokenKind::ElseKeyword) {
			self.next();

			Some(Box::new(match self.current() {
				Some(token) if token.kind == TokenKind::IfKeyword => self.parse_if_statement()?,
				Some(token) if token.kind == TokenKind::LeftBrace => self.parse_block_statement()?,
				Some(token) => {
					return Err(ParseError {
						message: format!("Expected `if` or `{{` after `else`, found `{}`.", token.lexeme),
						position: token.start,
					});
				}
				None => {
					return Err(ParseError {
						message: String::from("Expected `if` or `{` after `else`."),
						position: if_keyword.start,
					});
				}
			}))
		}
		else {
			None
		};

		Ok(Statement::If(IfStatement {
			condition,
			else_branch,
			position: if_keyword.start,
			then_branch,
		}))
	}

	fn parse_index_expression(&mut self, array: Expr, start: usize) -> Result<Expr, ParseError> {
		let index = self.parse_assignment_expression()?;
		self.expect_token(TokenKind::RightBracket, "Expected `]` to close array index expression.")?;

		Ok(Expr::Index(IndexExpr {
			array: Box::new(array),
			index: Box::new(index),
			position: start,
		}))
	}

	fn parse_infix(&mut self, left: Expr, operator: Token, binding_power: BindingPower) -> Result<Expr, ParseError> {
		match operator.kind {
			TokenKind::AndKeyword => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::And,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::Asterisk => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Multiply,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::BangEqual => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::NotEqual,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::Dash => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Subtract,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::EqualEqual => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Equal,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::ForwardSlash => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Divide,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::GreaterThan => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::GreaterThan,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::GreaterThanOrEqual => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::GreaterThanOrEqual,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::LeftBracket => self.parse_index_expression(left, operator.start),
			TokenKind::LeftParenthesis => self.parse_call_expression(left, operator.start),
			TokenKind::LessThan => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::LessThan,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::LessThanOrEqual => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::LessThanOrEqual,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::OrKeyword => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Or,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::Percent => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Modulo,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::Plus => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Add,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::XorKeyword => {
				let right = self.parse_expression_with_binding_power(binding_power)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Xor,
					position: operator.start,
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
			position: token.start,
			value,
		}))
	}

	fn parse_interpolated_string(&mut self, token: Token) -> Result<Expr, ParseError> {
		let mut parts = vec![Expr::Text(TextLiteral {
			position: token.start,
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
						position: segment.start,
						value: segment.lexeme,
					}));
				}
				TokenKind::InterpolatedStringEnd => {
					parts.push(Expr::Text(TextLiteral {
						position: segment.start,
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
				position: part.position(),
				right: Box::new(part),
			});
		}

		Ok(expression)
	}

	fn parse_negation_expression_with_position(&mut self, position: usize) -> Result<Expr, ParseError> {
		let operand = self.parse_expression_with_binding_power(BindingPower::Unary)?;

		Ok(Expr::Unary(UnaryExpr {
			operand: Box::new(operand),
			operator: UnaryOperator::Negate,
			position,
		}))
	}

	fn parse_not_expression(&mut self, position: usize) -> Result<Expr, ParseError> {
		let operand = self.parse_expression_with_binding_power(BindingPower::Unary)?;

		Ok(Expr::Unary(UnaryExpr {
			operand: Box::new(operand),
			operator: UnaryOperator::Not,
			position,
		}))
	}

	fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
		let token = self.next().ok_or(ParseError {
			message: String::from("Expected an expression."),
			position: 0,
		})?;

		match token.kind {
			TokenKind::Dash => self.parse_negation_expression_with_position(token.start),
			TokenKind::DecimalLiteral => self.parse_decimal_literal(token),
			TokenKind::EndOfFile => Err(ParseError {
				message: String::from("Expected an expression."),
				position: token.start,
			}),
			TokenKind::FalseKeyword | TokenKind::TrueKeyword => self.parse_boolean_literal(token),
			TokenKind::Identifier => Ok(self.parse_identifier_expression(token)),
			TokenKind::IntegerLiteral => self.parse_integer_literal(token),
			TokenKind::InterpolatedStringStart => self.parse_interpolated_string(token),
			TokenKind::LeftBracket => self.parse_array_literal(token.start),
			TokenKind::LeftParenthesis => self.parse_group_expression(token.start),
			TokenKind::NotKeyword => self.parse_not_expression(token.start),
			TokenKind::StringLiteral => Ok(self.parse_text_literal(token)),
			_ => Err(ParseError {
				message: format!("Unexpected token `{}` at start of expression.", token.lexeme),
				position: token.start,
			}),
		}
	}

	fn parse_range_expression(&mut self) -> Result<Expr, ParseError> {
		let start = self.parse_expression_with_binding_power(BindingPower::Default)?;

		if !self.current().is_some_and(|token| token.kind == TokenKind::Colon) {
			return Ok(start);
		}

		let colon = self.next().unwrap();
		let middle = self.parse_expression_with_binding_power(BindingPower::Default)?;

		if self.current().is_some_and(|token| token.kind == TokenKind::Colon) {
			self.next();
			let end = self.parse_expression_with_binding_power(BindingPower::Default)?;

			return Ok(Expr::Range(RangeExpr {
				end: Box::new(end),
				position: colon.start,
				start: Box::new(start),
				step: Some(Box::new(middle)),
			}));
		}

		Ok(Expr::Range(RangeExpr {
			end: Box::new(middle),
			position: colon.start,
			start: Box::new(start),
			step: None,
		}))
	}

	fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
		let return_keyword = self.expect_token(TokenKind::ReturnKeyword, "Expected `return` to start return statement.")?;
		let value = if self.current().is_some_and(|token| token.kind == TokenKind::Semicolon) {
			None
		}
		else {
			Some(self.parse_assignment_expression()?)
		};

		self.expect_token(TokenKind::Semicolon, "Expected `;` after return statement.")?;

		Ok(Statement::Return(ReturnStatement {
			position: return_keyword.start,
			value,
		}))
	}

	fn parse_statement(&mut self) -> Result<Statement, ParseError> {
		match self.current() {
			Some(token) if token.kind == TokenKind::BreakKeyword => self.parse_break_statement(),
			Some(token) if token.kind == TokenKind::ContinueKeyword => self.parse_continue_statement(),
			Some(token) if token.kind == TokenKind::ForKeyword => self.parse_for_statement(),
			Some(token) if token.kind == TokenKind::FnKeyword => {
				Ok(Statement::FunctionDeclaration(self.parse_function_declaration()?))
			}
			Some(token) if token.kind == TokenKind::IfKeyword => self.parse_if_statement(),
			Some(token) if token.kind == TokenKind::LeftBrace => self.parse_block_statement(),
			Some(token) if token.kind == TokenKind::ReturnKeyword => self.parse_return_statement(),
			Some(token) if token.kind == TokenKind::WhileKeyword => self.parse_while_statement(),
			Some(token) if matches!(token.kind, TokenKind::ConstKeyword | TokenKind::VarKeyword) => {
				self.parse_variable_declaration_statement()
			}
			Some(_) => {
				let expression = self.parse_assignment_expression()?;
				self.expect_token(TokenKind::Semicolon, "Expected `;` after expression statement.")?;
				Ok(Statement::Expression(expression))
			}
			None => Err(ParseError {
				message: String::from("Expected a statement."),
				position: 0,
			}),
		}
	}

	fn parse_text_literal(&self, token: Token) -> Expr {
		Expr::Text(TextLiteral {
			position: token.start,
			value: token.lexeme,
		})
	}

	fn parse_variable_declaration_statement(&mut self) -> Result<Statement, ParseError> {
		let declaration_keyword = self.next().ok_or(ParseError {
			message: String::from("Expected `var` or `const` to start variable declaration."),
			position: 0,
		})?;

		let is_const = match declaration_keyword.kind {
			TokenKind::ConstKeyword => true,
			TokenKind::VarKeyword => false,
			_ => {
				return Err(ParseError {
					message: format!("Expected `var` or `const` to start variable declaration. Found `{}` instead.", declaration_keyword.lexeme),
					position: declaration_keyword.start,
				});
			}
		};

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
			is_const,
			name: name.lexeme,
			position: declaration_keyword.start,
		}))
	}

	fn parse_while_statement(&mut self) -> Result<Statement, ParseError> {
		let while_keyword = self.expect_token(TokenKind::WhileKeyword, "Expected `while` to start while statement.")?;
		let condition = self.parse_assignment_expression()?;
		let body = match self.parse_block_statement()? {
			Statement::Block(block) => block,
			_ => unreachable!("Block parser must return a block statement."),
		};

		Ok(Statement::While(WhileStatement {
			body,
			condition,
			position: while_keyword.start,
		}))
	}
}

#[cfg(test)]
mod tests {
	use crate::ast::ArrayIndexAssignmentTarget;
	use crate::ast::ArrayLiteral;
	use crate::ast::AssignmentExpr;
	use crate::ast::AssignmentOperator;
	use crate::ast::AssignmentTarget;
	use crate::ast::BinaryExpr;
	use crate::ast::BinaryOperator;
	use crate::ast::BlockStatement;
	use crate::ast::BooleanLiteral;
	use crate::ast::BreakStatement;
	use crate::ast::CallArgument;
	use crate::ast::CallExpr;
	use crate::ast::ContinueStatement;
	use crate::ast::DataType;
	use crate::ast::DecimalLiteral;
	use crate::ast::Expr;
	use crate::ast::ForStatement;
	use crate::ast::FunctionDeclaration;
	use crate::ast::FunctionParameter;
	use crate::ast::IdentifierExpr;
	use crate::ast::IfStatement;
	use crate::ast::IndexExpr;
	use crate::ast::IntegerLiteral;
	use crate::ast::Program;
	use crate::ast::RangeExpr;
	use crate::ast::ReturnStatement;
	use crate::ast::Statement;
	use crate::ast::TextLiteral;
	use crate::ast::UnaryExpr;
	use crate::ast::UnaryOperator;
	use crate::ast::VariableDeclaration;
	use crate::ast::WhileStatement;
	use crate::source::SourceText;
	use crate::value::Decimal;

	use super::super::lexer::Lexer;
	use super::Parser;

	fn normalize_assignment_target(target: AssignmentTarget) -> AssignmentTarget {
		match target {
			AssignmentTarget::Identifier(identifier) => AssignmentTarget::Identifier(normalize_identifier(identifier)),
			AssignmentTarget::Index(target) => AssignmentTarget::Index(ArrayIndexAssignmentTarget {
				array: normalize_identifier(target.array),
				index: Box::new(normalize_expr(*target.index)),
				position: 0,
			}),
		}
	}

	fn normalize_block(block: BlockStatement) -> BlockStatement {
		BlockStatement {
			position: 0,
			statements: block.statements.into_iter().map(normalize_statement).collect(),
		}
	}

	fn normalize_call_argument(argument: CallArgument) -> CallArgument {
		CallArgument {
			is_by_ref: argument.is_by_ref,
			position: 0,
			value: normalize_expr(argument.value),
		}
	}

	fn normalize_expr(expression: Expr) -> Expr {
		match expression {
			Expr::Array(ArrayLiteral { elements, .. }) => Expr::Array(ArrayLiteral {
				elements: elements.into_iter().map(normalize_expr).collect(),
				position: 0,
			}),
			Expr::Assignment(AssignmentExpr {
				operator,
				target,
				value,
				..
			}) => Expr::Assignment(AssignmentExpr {
				operator,
				position: 0,
				target: normalize_assignment_target(target),
				value: Box::new(normalize_expr(*value)),
			}),
			Expr::Binary(BinaryExpr {
				left,
				operator,
				right,
				..
			}) => Expr::Binary(BinaryExpr {
				left: Box::new(normalize_expr(*left)),
				operator,
				position: 0,
				right: Box::new(normalize_expr(*right)),
			}),
			Expr::Boolean(BooleanLiteral { value, .. }) => Expr::Boolean(BooleanLiteral {
				position: 0,
				value,
			}),
			Expr::Call(CallExpr {
				arguments,
				callee,
				..
			}) => Expr::Call(CallExpr {
				arguments: arguments.into_iter().map(normalize_call_argument).collect(),
				callee: normalize_identifier(callee),
				position: 0,
			}),
			Expr::Decimal(DecimalLiteral { value, .. }) => Expr::Decimal(DecimalLiteral {
				position: 0,
				value,
			}),
			Expr::Identifier(identifier) => Expr::Identifier(normalize_identifier(identifier)),
			Expr::Index(IndexExpr { array, index, .. }) => Expr::Index(IndexExpr {
				array: Box::new(normalize_expr(*array)),
				index: Box::new(normalize_expr(*index)),
				position: 0,
			}),
			Expr::Integer(IntegerLiteral { value, .. }) => Expr::Integer(IntegerLiteral {
				position: 0,
				value,
			}),
			Expr::Range(RangeExpr {
				start,
				step,
				end,
				..
			}) => Expr::Range(RangeExpr {
				start: Box::new(normalize_expr(*start)),
				step: step.map(|step| Box::new(normalize_expr(*step))),
				end: Box::new(normalize_expr(*end)),
				position: 0,
			}),
			Expr::Text(TextLiteral { value, .. }) => Expr::Text(TextLiteral {
				position: 0,
				value,
			}),
			Expr::Unary(UnaryExpr {
				operand,
				operator,
				..
			}) => Expr::Unary(UnaryExpr {
				operand: Box::new(normalize_expr(*operand)),
				operator,
				position: 0,
			}),
		}
	}

	fn normalize_for_statement(for_statement: ForStatement) -> ForStatement {
		ForStatement {
			body: normalize_block(for_statement.body),
			iterable: normalize_expr(for_statement.iterable),
			position: 0,
			variable: normalize_identifier(for_statement.variable),
		}
	}

	fn normalize_function_declaration(function: FunctionDeclaration) -> FunctionDeclaration {
		FunctionDeclaration {
			body: normalize_block(function.body),
			name: function.name,
			parameters: function.parameters.into_iter().map(normalize_function_parameter).collect(),
			position: 0,
			return_type: function.return_type,
		}
	}

	fn normalize_function_parameter(parameter: FunctionParameter) -> FunctionParameter {
		FunctionParameter {
			data_type: parameter.data_type,
			is_by_ref: parameter.is_by_ref,
			name: parameter.name,
			position: 0,
		}
	}

	fn normalize_identifier(identifier: IdentifierExpr) -> IdentifierExpr {
		IdentifierExpr {
			name: identifier.name,
			position: 0,
		}
	}

	fn normalize_program(program: Program) -> Program {
		Program {
			functions: program.functions.into_iter().map(normalize_function_declaration).collect(),
			result: program.result.map(normalize_expr),
			statements: program.statements.into_iter().map(normalize_statement).collect(),
		}
	}

	fn normalize_statement(statement: Statement) -> Statement {
		match statement {
			Statement::Block(block) => Statement::Block(normalize_block(block)),
			Statement::Break(_) => Statement::Break(BreakStatement {
				position: 0,
			}),
			Statement::Continue(_) => Statement::Continue(ContinueStatement {
				position: 0,
			}),
			Statement::Expression(expression) => Statement::Expression(normalize_expr(expression)),
			Statement::For(for_statement) => Statement::For(normalize_for_statement(for_statement)),
			Statement::FunctionDeclaration(function) => {
				Statement::FunctionDeclaration(normalize_function_declaration(function))
			}
			Statement::If(if_statement) => Statement::If(normalize_if_statement(if_statement)),
			Statement::Return(return_statement) => Statement::Return(normalize_return_statement(return_statement)),
			Statement::VariableDeclaration(declaration) => {
				Statement::VariableDeclaration(normalize_variable_declaration(declaration))
			}
			Statement::While(while_statement) => Statement::While(normalize_while_statement(while_statement)),
		}
	}

	fn normalize_if_statement(if_statement: IfStatement) -> IfStatement {
		IfStatement {
			condition: normalize_expr(if_statement.condition),
			else_branch: if_statement.else_branch.map(|branch| Box::new(normalize_statement(*branch))),
			position: 0,
			then_branch: normalize_block(if_statement.then_branch),
		}
	}

	fn normalize_return_statement(return_statement: ReturnStatement) -> ReturnStatement {
		ReturnStatement {
			position: 0,
			value: return_statement.value.map(normalize_expr),
		}
	}

	fn normalize_variable_declaration(declaration: VariableDeclaration) -> VariableDeclaration {
		VariableDeclaration {
			data_type: declaration.data_type,
			is_const: declaration.is_const,
			initial_value: declaration.initial_value.map(normalize_expr),
			name: declaration.name,
			position: 0,
		}
	}

	fn normalize_while_statement(while_statement: WhileStatement) -> WhileStatement {
		WhileStatement {
			body: normalize_block(while_statement.body),
			condition: normalize_expr(while_statement.condition),
			position: 0,
		}
	}

	fn parse(source: &str) -> Expr {
		let mut lexer = Lexer::new(SourceText::new(source));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		normalize_expr(parser.parse_expression().unwrap())
	}

	fn parse_program(source: &str) -> Program {
		let mut lexer = Lexer::new(SourceText::new(source));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		normalize_program(parser.parse_program().unwrap())
	}

	#[test]
	fn parses_array_index_expression() {
		let program = parse_program("xs[1]");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			result: Some(Expr::Index(IndexExpr {
				array: Box::new(Expr::Identifier(IdentifierExpr {
					name: String::from("xs"),
					position: 0,
				})),
				index: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 1,
				})),
				position: 0,
			})),
			statements: vec![],
		});
	}

	#[test]
	fn parses_array_literal() {
		let program = parse_program("[1, 2]");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			result: Some(Expr::Array(ArrayLiteral {
				elements: vec![
					Expr::Integer(IntegerLiteral {
						position: 0,
						value: 1,
					}),
					Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					}),
				],
				position: 0,
			})),
			statements: vec![],
		});
	}

	#[test]
	fn parses_array_slice_expression() {
		let program = parse_program("xs[2:4]");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			result: Some(Expr::Index(IndexExpr {
				array: Box::new(Expr::Identifier(IdentifierExpr {
					name: String::from("xs"),
					position: 0,
				})),
				index: Box::new(Expr::Range(RangeExpr {
					start: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					})),
					step: None,
					end: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 4,
					})),
					position: 0,
				})),
				position: 0,
			})),
			statements: vec![],
		});
	}

	#[test]
	fn parses_array_variable_declaration() {
		let program = parse_program("var xs: [int] = [];");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			result: None,
			statements: vec![
				Statement::VariableDeclaration(VariableDeclaration {
					data_type: DataType::Array(Box::new(DataType::Int)),
					initial_value: Some(Expr::Array(ArrayLiteral {
						elements: vec![],
						position: 0,
					})),
					is_const: false,
					name: String::from("xs"),
					position: 0,
				}),
			],
		});
	}

	#[test]
	fn parses_assignment_right_associatively() {
		assert_eq!(
			parse("x = y = 1"),
			Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::Assign,
				position: 0,
				target: AssignmentTarget::Identifier(IdentifierExpr {
					name: String::from("x"),
					position: 0,
				}),
				value: Box::new(Expr::Assignment(AssignmentExpr {
					operator: AssignmentOperator::Assign,
					position: 0,
					target: AssignmentTarget::Identifier(IdentifierExpr {
						name: String::from("y"),
						position: 0,
					}),
					value: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 1,
					})),
				})),
			})
		);
	}

	#[test]
	fn parses_block_statement() {
		assert_eq!(
			parse_program("{ var x: int = 1; x += 2; }"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::Block(BlockStatement {
						position: 0,
						statements: vec![
							Statement::VariableDeclaration(VariableDeclaration {
								data_type: DataType::Int,
								initial_value: Some(Expr::Integer(IntegerLiteral {
									position: 0,
									value: 1,
								})),
								is_const: false,
								name: String::from("x"),
								position: 0,
							}),
							Statement::Expression(Expr::Assignment(AssignmentExpr {
								operator: AssignmentOperator::AddAssign,
								position: 0,
								target: AssignmentTarget::Identifier(IdentifierExpr {
									position: 0,
									name: String::from("x"),
								}),
								value: Box::new(Expr::Integer(IntegerLiteral {
									position: 0,
									value: 2,
								})),
							})),
						],
					}),
				],
				result: None,
			}
		);
	}

	#[test]
	fn parses_boolean_literal() {
		assert_eq!(
			parse("true"),
			Expr::Boolean(BooleanLiteral {
				position: 0,
				value: true,
			})
		);
	}

	#[test]
	fn parses_by_reference_function_call_expression() {
		assert_eq!(
			parse("bump(&x)"),
			Expr::Call(CallExpr {
				arguments: vec![
					CallArgument {
						is_by_ref: true,
						position: 0,
						value: Expr::Identifier(IdentifierExpr {
							name: String::from("x"),
							position: 0,
						}),
					},
				],
				callee: IdentifierExpr {
					name: String::from("bump"),
					position: 0,
				},
				position: 0,
			})
		);
	}

	#[test]
	fn parses_by_reference_function_parameter() {
		assert_eq!(
			parse_program("fn bump(value: &int) void { value += 1; }"),
			Program {
				functions: vec![
					FunctionDeclaration {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::Expression(Expr::Assignment(AssignmentExpr {
									operator: AssignmentOperator::AddAssign,
									position: 0,
									target: AssignmentTarget::Identifier(IdentifierExpr {
										name: String::from("value"),
										position: 0,
									}),
									value: Box::new(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 1,
									})),
								})),
							],
						},
						name: String::from("bump"),
						parameters: vec![
							FunctionParameter {
								data_type: DataType::Int,
								is_by_ref: true,
								name: String::from("value"),
								position: 0,
							},
						],
						position: 0,
						return_type: DataType::Void,
					},
				],
				result: None,
				statements: vec![],
			}
		);
	}

	#[test]
	fn parses_compound_assignment_expression() {
		assert_eq!(
			parse("x += 1"),
			Expr::Assignment(AssignmentExpr {
				operator: AssignmentOperator::AddAssign,
				position: 0,
				target: AssignmentTarget::Identifier(IdentifierExpr {
					name: String::from("x"),
					position: 0,
				}),
				value: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
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
						position: 0,
						value: 1,
					})),
					position: 0,
					operator: BinaryOperator::LessThan,
					right: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					})),
				})),
				operator: BinaryOperator::Equal,
				position: 0,
				right: Box::new(Expr::Boolean(BooleanLiteral {
					position: 0,
					value: true,
				})),
			})
		);
	}

	#[test]
	fn parses_compound_indexed_assignment_expression() {
		let program = parse_program("xs[1] += 2;");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			result: None,
			statements: vec![
				Statement::Expression(Expr::Assignment(AssignmentExpr {
					operator: AssignmentOperator::AddAssign,
					position: 0,
					target: AssignmentTarget::Index(ArrayIndexAssignmentTarget {
						array: IdentifierExpr {
							name: String::from("xs"),
							position: 0,
						},
						index: Box::new(Expr::Integer(IntegerLiteral {
							position: 0,
							value: 1,
						})),
						position: 0,
					}),
					value: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					})),
				})),
			],
		});
	}

	#[test]
	fn parses_const_variable_declaration() {
		assert_eq!(
			parse_program("const name: text = 'Tablo';"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Text,
						initial_value: Some(Expr::Text(TextLiteral {
							position: 0,
							value: String::from("Tablo"),
						})),
						is_const: true,
						name: String::from("name"),
						position: 0,
					}),
				],
				result: None,
			}
		);
	}

	#[test]
	fn parses_decimal_literal() {
		assert_eq!(
			parse(".5"),
			Expr::Decimal(DecimalLiteral {
				position: 0,
				value: Decimal::from_literal(".5").unwrap(),
			})
		);
	}

	#[test]
	fn parses_expression_statement_before_final_expression() {
		assert_eq!(
			parse_program("var x: int = 1;\nx += 2;\nx"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Int,
						initial_value: Some(Expr::Integer(IntegerLiteral {
							position: 0,
							value: 1,
						})),
						is_const: false,
						name: String::from("x"),
						position: 0,
					}),
					Statement::Expression(Expr::Assignment(AssignmentExpr {
						operator: AssignmentOperator::AddAssign,
						position: 0,
						target: AssignmentTarget::Identifier(IdentifierExpr {
							position: 0,
							name: String::from("x"),
						}),
						value: Box::new(Expr::Integer(IntegerLiteral {
							position: 0,
							value: 2,
						})),
					})),
				],
				result: Some(Expr::Identifier(IdentifierExpr {
					position: 0,
					name: String::from("x"),
				})),
			}
		);
	}

	#[test]
	fn parses_for_statement() {
		assert_eq!(
			parse_program("for value in [1, 2] { value; }"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::For(ForStatement {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::Expression(Expr::Identifier(IdentifierExpr {
									name: String::from("value"),
									position: 0,
								})),
							],
						},
						iterable: Expr::Array(ArrayLiteral {
							elements: vec![
								Expr::Integer(IntegerLiteral {
									position: 0,
									value: 1,
								}),
								Expr::Integer(IntegerLiteral {
									position: 0,
									value: 2,
								}),
							],
							position: 0,
						}),
						position: 0,
						variable: IdentifierExpr {
							name: String::from("value"),
							position: 0,
						},
					}),
				],
				result: None,
			}
		);
	}

	#[test]
	fn parses_function_call_expression() {
		assert_eq!(
			parse("add(1, 2)"),
			Expr::Call(CallExpr {
				arguments: vec![
					CallArgument {
						is_by_ref: false,
						position: 0,
						value: Expr::Integer(IntegerLiteral {
							position: 0,
							value: 1,
						}),
					},
					CallArgument {
						is_by_ref: false,
						position: 0,
						value: Expr::Integer(IntegerLiteral {
							position: 0,
							value: 2,
						}),
					},
				],
				callee: IdentifierExpr {
					name: String::from("add"),
					position: 0,
				},
				position: 0,
			})
		);
	}

	#[test]
	fn parses_function_declaration() {
		assert_eq!(
			parse_program("fn add(a: int, b: int) int { return a + b; }\nadd(1, 2)"),
			Program {
				functions: vec![
					FunctionDeclaration {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::Return(ReturnStatement {
									position: 0,
									value: Some(Expr::Binary(BinaryExpr {
										left: Box::new(Expr::Identifier(IdentifierExpr {
											name: String::from("a"),
											position: 0,
										})),
										operator: BinaryOperator::Add,
										position: 0,
										right: Box::new(Expr::Identifier(IdentifierExpr {
											name: String::from("b"),
											position: 0,
										})),
									})),
								}),
							],
						},
						name: String::from("add"),
						parameters: vec![
							FunctionParameter {
								data_type: DataType::Int,
								is_by_ref: false,
								name: String::from("a"),
								position: 0,
							},
							FunctionParameter {
								data_type: DataType::Int,
								is_by_ref: false,
								name: String::from("b"),
								position: 0,
							},
						],
						position: 0,
						return_type: DataType::Int,
					},
				],
				result: Some(Expr::Call(CallExpr {
					arguments: vec![
						CallArgument {
							is_by_ref: false,
							position: 0,
							value: Expr::Integer(IntegerLiteral {
								position: 0,
								value: 1,
							}),
						},
						CallArgument {
							is_by_ref: false,
							position: 0,
							value: Expr::Integer(IntegerLiteral {
								position: 0,
								value: 2,
							}),
						},
					],
					callee: IdentifierExpr {
						name: String::from("add"),
						position: 0,
					},
					position: 0,
				})),
				statements: vec![],
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
						position: 0,
						value: 1,
					})),
					operator: BinaryOperator::Add,
					position: 0,
					right: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					})),
				})),
				operator: BinaryOperator::Multiply,
				position: 0,
				right: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 3,
				})),
			})
		);
	}

	#[test]
	fn parses_if_else_if_statement() {
		assert_eq!(
			parse_program("if false { var x: int = 1; } else if true { var y: int = 2; }"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::If(IfStatement {
						condition: Expr::Boolean(BooleanLiteral {
							position: 0,
							value: false,
						}),
						else_branch: Some(Box::new(Statement::If(IfStatement {
							condition: Expr::Boolean(BooleanLiteral {
								position: 0,
								value: true,
							}),
							else_branch: None,
							position: 0,
							then_branch: BlockStatement {
								position: 0,
								statements: vec![
									Statement::VariableDeclaration(VariableDeclaration {
										data_type: DataType::Int,
										initial_value: Some(Expr::Integer(IntegerLiteral {
											position: 0,
											value: 2,
										})),
										is_const: false,
										name: String::from("y"),
										position: 0,
									}),
								],
							},
						}))),
						position: 0,
						then_branch: BlockStatement {
							position: 0,
							statements: vec![
								Statement::VariableDeclaration(VariableDeclaration {
									data_type: DataType::Int,
									initial_value: Some(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 1,
									})),
									is_const: false,
									name: String::from("x"),
									position: 0,
								}),
							],
						},
					}),
				],
				result: None,
			}
		);
	}

	#[test]
	fn parses_if_else_statement() {
		assert_eq!(
			parse_program("if true { var x: int = 1; } else { var x: int = 2; }"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::If(IfStatement {
						condition: Expr::Boolean(BooleanLiteral {
							position: 0,
							value: true,
						}),
						else_branch: Some(Box::new(Statement::Block(BlockStatement {
							position: 0,
							statements: vec![
								Statement::VariableDeclaration(VariableDeclaration {
									data_type: DataType::Int,
									initial_value: Some(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 2,
									})),
									is_const: false,
									name: String::from("x"),
									position: 0,
								}),
							],
						}))),
						position: 0,
						then_branch: BlockStatement {
							position: 0,
							statements: vec![
								Statement::VariableDeclaration(VariableDeclaration {
									data_type: DataType::Int,
									initial_value: Some(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 1,
									})),
									is_const: false,
									name: String::from("x"),
									position: 0,
								}),
							],
						},
					}),
				],
				result: None,
			}
		);
	}

	#[test]
	fn parses_if_statement() {
		assert_eq!(
			parse_program("if true { var x: int = 1; }"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::If(IfStatement {
						condition: Expr::Boolean(BooleanLiteral {
							position: 0,
							value: true,
						}),
						else_branch: None,
						position: 0,
						then_branch: BlockStatement {
							position: 0,
							statements: vec![
								Statement::VariableDeclaration(VariableDeclaration {
									data_type: DataType::Int,
									initial_value: Some(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 1,
									})),
									is_const: false,
									name: String::from("x"),
									position: 0,
								}),
							],
						},
					}),
				],
				result: None,
			}
		);
	}

	#[test]
	fn parses_indexed_assignment_expression() {
		let program = parse_program("xs[1] = 2;");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			result: None,
			statements: vec![
				Statement::Expression(Expr::Assignment(AssignmentExpr {
					operator: AssignmentOperator::Assign,
					position: 0,
					target: AssignmentTarget::Index(ArrayIndexAssignmentTarget {
						array: IdentifierExpr {
							name: String::from("xs"),
							position: 0,
						},
						index: Box::new(Expr::Integer(IntegerLiteral {
							position: 0,
							value: 1,
						})),
						position: 0,
					}),
					value: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					})),
				})),
			],
		});
	}

	#[test]
	fn parses_integer_literal() {
		assert_eq!(
			parse("42"),
			Expr::Integer(IntegerLiteral { position: 0, value: 42 })
		);
	}

	#[test]
	fn parses_interpolated_string() {
		assert_eq!(
			parse("'hello ${name}!'"),
			Expr::Binary(BinaryExpr {
				left: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Text(TextLiteral {
						position: 0,
						value: String::from("hello "),
					})),
					operator: BinaryOperator::Add,
					position: 0,
					right: Box::new(Expr::Identifier(IdentifierExpr {
						name: String::from("name"),
						position: 0,
					})),
				})),
				operator: BinaryOperator::Add,
				position: 0,
				right: Box::new(Expr::Text(TextLiteral {
					position: 0,
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
						position: 0,
						value: 1,
					})),
					operator: BinaryOperator::Add,
					position: 0,
					right: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					})),
				})),
				operator: BinaryOperator::Add,
				position: 0,
				right: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
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
						position: 0,
						value: 9,
					})),
					operator: BinaryOperator::Subtract,
					position: 0,
					right: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 3,
					})),
				})),
				operator: BinaryOperator::Subtract,
				position: 0,
				right: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
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
					position: 0,
					value: true,
				})),
				operator: BinaryOperator::Or,
				position: 0,
				right: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Boolean(BooleanLiteral {
						position: 0,
						value: false,
					})),
					operator: BinaryOperator::And,
					position: 0,
					right: Box::new(Expr::Boolean(BooleanLiteral {
						position: 0,
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
					position: 0,
					value: true,
				})),
				operator: BinaryOperator::Or,
				position: 0,
				right: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Boolean(BooleanLiteral {
						position: 0,
						value: false,
					})),
					operator: BinaryOperator::Xor,
					position: 0,
					right: Box::new(Expr::Binary(BinaryExpr {
						left: Box::new(Expr::Boolean(BooleanLiteral {
							position: 0,
							value: true,
						})),
						operator: BinaryOperator::And,
						position: 0,
						right: Box::new(Expr::Boolean(BooleanLiteral {
							position: 0,
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
					position: 0,
					value: 1,
				})),
				operator: BinaryOperator::Add,
				position: 0,
				right: Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					})),
					operator: BinaryOperator::Multiply,
					position: 0,
					right: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 3,
					})),
				})),
			})
		);
	}

	#[test]
	fn parses_nested_function_declaration() {
		assert_eq!(
			parse_program("fn outer() void { fn inner(value: &int) void { value += 1; } }"),
			Program {
				functions: vec![
					FunctionDeclaration {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::FunctionDeclaration(FunctionDeclaration {
									body: BlockStatement {
										position: 0,
										statements: vec![
											Statement::Expression(Expr::Assignment(AssignmentExpr {
												operator: AssignmentOperator::AddAssign,
												position: 0,
												target: AssignmentTarget::Identifier(IdentifierExpr {
													name: String::from("value"),
													position: 0,
												}),
												value: Box::new(Expr::Integer(IntegerLiteral {
													position: 0,
													value: 1,
												})),
											})),
										],
									},
									name: String::from("inner"),
									parameters: vec![
										FunctionParameter {
											data_type: DataType::Int,
											is_by_ref: true,
											name: String::from("value"),
											position: 0,
										},
									],
									position: 0,
									return_type: DataType::Void,
								}),
							],
						},
						name: String::from("outer"),
						parameters: vec![],
						position: 0,
						return_type: DataType::Void,
					},
				],
				result: None,
				statements: vec![],
			}
		);
	}

	#[test]
	fn parses_not_expression() {
		assert_eq!(
			parse("not false"),
			Expr::Unary(UnaryExpr {
				operand: Box::new(Expr::Boolean(BooleanLiteral {
					position: 0,
					value: false,
				})),
				operator: UnaryOperator::Not,
				position: 0,
			})
		);
	}

	#[test]
	fn parses_program_with_variable_declarations() {
		assert_eq!(
			parse_program("var x: int = 1;\nvar y: int = 2;\nx + y"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Int,
						initial_value: Some(Expr::Integer(IntegerLiteral {
							position: 0,
							value: 1,
						})),
						is_const: false,
						name: String::from("x"),
						position: 0,
					}),
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Int,
						initial_value: Some(Expr::Integer(IntegerLiteral {
							position: 0,
							value: 2,
						})),
						is_const: false,
						name: String::from("y"),
						position: 0,
					}),
				],
				result: Some(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Identifier(IdentifierExpr {
						name: String::from("x"),
						position: 0,
					})),
					operator: BinaryOperator::Add,
					position: 0,
					right: Box::new(Expr::Identifier(IdentifierExpr {
						name: String::from("y"),
						position: 0,
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
						position: 0,
						value: 1,
					})),
					operator: BinaryOperator::Add,
					position: 0,
					right: Box::new(Expr::Integer(IntegerLiteral {
						position: 0,
						value: 2,
					})),
				})),
				operator: BinaryOperator::LessThan,
				position: 0,
				right: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 4,
				})),
			})
		);
	}

	#[test]
	fn parses_simple_range_expression() {
		assert_eq!(
			parse("0:10"),
			Expr::Range(RangeExpr {
				start: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 0,
				})),
				step: None,
				end: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 10,
				})),
				position: 0,
			})
		);
	}

	#[test]
	fn parses_stepped_range_expression() {
		assert_eq!(
			parse("0:2:10"),
			Expr::Range(RangeExpr {
				start: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 0,
				})),
				step: Some(Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 2,
				}))),
				end: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
					value: 10,
				})),
				position: 0,
			})
		);
	}

	#[test]
	fn parses_string_literal() {
		assert_eq!(
			parse("'hello'"),
			Expr::Text(TextLiteral {
				position: 0,
				value: String::from("hello"),
			})
		);
	}

	#[test]
	fn parses_text_variable_declaration() {
		assert_eq!(
			parse_program("var name: text = 'Tablo';"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Text,
						initial_value: Some(Expr::Text(TextLiteral {
							position: 0,
							value: String::from("Tablo"),
						})),
						is_const: false,
						name: String::from("name"),
						position: 0,
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
					position: 0,
					value: 42,
				})),
				operator: UnaryOperator::Negate,
				position: 0,
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
						position: 0,
						value: 2,
					})),
					operator: UnaryOperator::Negate,
					position: 0,
				})),
				operator: BinaryOperator::Multiply,
				position: 0,
				right: Box::new(Expr::Integer(IntegerLiteral {
					position: 0,
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
				functions: vec![],
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Int,
						initial_value: None,
						is_const: false,
						name: String::from("x"),
						position: 0,
					}),
				],
				result: None,
			}
		);
	}

	#[test]
	fn parses_while_statement() {
		assert_eq!(
			parse_program("while x < 3 { x += 1; }"),
			Program {
				functions: vec![],
				statements: vec![
					Statement::While(WhileStatement {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::Expression(Expr::Assignment(AssignmentExpr {
									operator: AssignmentOperator::AddAssign,
									position: 0,
									target: AssignmentTarget::Identifier(IdentifierExpr {
										name: String::from("x"),
										position: 0,
									}),
									value: Box::new(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 1,
									})),
								})),
							],
						},
						condition: Expr::Binary(BinaryExpr {
							left: Box::new(Expr::Identifier(IdentifierExpr {
								name: String::from("x"),
								position: 0,
							})),
							operator: BinaryOperator::LessThan,
							position: 0,
							right: Box::new(Expr::Integer(IntegerLiteral {
								position: 0,
								value: 3,
							})),
						}),
						position: 0,
					}),
				],
				result: None,
			}
		);
	}

	#[test]
	fn parses_while_statement_with_break_and_continue() {
		assert_eq!(
			parse_program("while true { continue; break; }"),
			Program {
				functions: vec![],
				result: None,
				statements: vec![
					Statement::While(WhileStatement {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::Continue(ContinueStatement {
									position: 0,
								}),
								Statement::Break(BreakStatement {
									position: 0,
								}),
							],
						},
						condition: Expr::Boolean(BooleanLiteral {
							position: 0,
							value: true,
						}),
						position: 0,
					}),
				],
			}
		);
	}

	#[test]
	fn rejects_else_without_block_or_if() {
		let mut lexer = Lexer::new(SourceText::new("if true { } else true"));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		let error = parser.parse_program().unwrap_err();

		assert_eq!(error.message, "Expected `if` or `{` after `else`, found `true`.");
		assert_eq!(error.position, 17);
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
