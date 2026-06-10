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
use crate::ast::CountExpr;
use crate::ast::DataType;
use crate::ast::DecimalLiteral;
use crate::ast::Expr;
use crate::ast::FieldAccessExpr;
use crate::ast::FindExpr;
use crate::ast::FindKind;
use crate::ast::ForRecordStatement;
use crate::ast::ForStatement;
use crate::ast::FunctionDeclaration;
use crate::ast::FunctionParameter;
use crate::ast::IdentifierExpr;
use crate::ast::IfStatement;
use crate::ast::IndexExpr;
use crate::ast::IntegerLiteral;
use crate::ast::ObjectConstructionExpr;
use crate::ast::ObjectConstructionField;
use crate::ast::ObjectDeclaration;
use crate::ast::ObjectFieldAssignmentTarget;
use crate::ast::ObjectFieldDeclaration;
use crate::ast::OrderByDirection;
use crate::ast::OrderByItem;
use crate::ast::Program;
use crate::ast::RangeExpr;
use crate::ast::RecordPointerDeclaration;
use crate::ast::ReturnStatement;
use crate::ast::Statement;
use crate::ast::TableReference;
use crate::ast::TextLiteral;
use crate::ast::UnaryExpr;
use crate::ast::UnaryOperator;
use crate::ast::VariableDeclaration;
use crate::ast::WhileStatement;
use crate::ast::WithDeclaration;
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
		let mut objects = Vec::new();
		let mut statements = Vec::new();
		let mut result = None;
		let mut with_declarations = Vec::new();

		loop {
			match self.current() {
				Some(token) if token.kind == TokenKind::EndOfFile => break,
				Some(token) if token.kind == TokenKind::FnKeyword => {
					functions.push(self.parse_function_declaration()?);
				}
				Some(token) if token.kind == TokenKind::ObjKeyword => {
					objects.extend(self.parse_object_declaration()?);
				}
				Some(token) if token.kind == TokenKind::WithKeyword => {
					with_declarations.push(self.parse_with_declaration()?);
				}
				Some(token) if matches!(token.kind, TokenKind::BreakKeyword | TokenKind::ConstKeyword | TokenKind::ContinueKeyword | TokenKind::ForKeyword | TokenKind::IfKeyword | TokenKind::LeftBrace | TokenKind::RecKeyword | TokenKind::ReturnKeyword | TokenKind::VarKeyword | TokenKind::WhileKeyword) => {
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
			objects,
			result,
			statements,
			with_declarations,
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

	fn extract_field_assignment_target(
		&self,
		field_access: FieldAccessExpr,
		position: usize,
	) -> Result<ObjectFieldAssignmentTarget, ParseError> {
		let mut fields = vec![field_access.field];
		let mut object = *field_access.object;

		loop {
			match object {
				Expr::FieldAccess(field_access) => {
					fields.push(field_access.field);
					object = *field_access.object;
				}
				Expr::Identifier(identifier) => {
					fields.reverse();
					return Ok(ObjectFieldAssignmentTarget {
						fields,
						object: identifier,
						position,
					});
				}
				_ => {
					return Err(ParseError {
						message: String::from("Object field assignment target must be an identifier-based field access."),
						position,
					});
				}
			}
		}
	}

	fn is_qualified_identifier_expr(&self, expression: &Expr) -> bool {
		match expression {
			Expr::Identifier(_) => true,
			Expr::FieldAccess(field_access) => self.is_qualified_identifier_expr(&field_access.object),
			_ => false,
		}
	}

	fn next(&mut self) -> Option<Token> {
		let token = self.current()?.clone();
		self.position += 1;
		Some(token)
	}

	fn object_field_array_element_name(&self, field_name: &str, member_index: usize) -> String {
		let base = self.object_field_member_name(field_name, member_index);
		format!("{base}.Element")
	}

	fn object_field_member_name(&self, field_name: &str, member_index: usize) -> String {
		if member_index == 0 {
			String::from(field_name)
		}
		else {
			format!("{field_name}$member{}", member_index + 1)
		}
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
			Expr::FieldAccess(field_access) => {
				AssignmentTarget::Field(self.extract_field_assignment_target(field_access, operator_position)?)
			}
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
					message: String::from("Assignment target must be an identifier, object field, or indexed array element."),
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

	fn parse_count_expression(&mut self, start: usize) -> Result<Expr, ParseError> {
		let table = self.parse_table_reference()?;
		let where_clause = if self.current().is_some_and(|token| token.kind == TokenKind::WhereKeyword) {
			self.next();
			Some(Box::new(self.parse_query_expression_with_binding_power(BindingPower::Default, true)?))
		}
		else {
			None
		};

		Ok(Expr::Count(CountExpr {
			position: start,
			table,
			where_clause,
		}))
	}

	fn parse_data_type(&mut self) -> Result<DataType, ParseError> {
		let mut data_types = vec![self.parse_primary_data_type()?];

		while self.current().is_some_and(|token| token.kind == TokenKind::Pipe) {
			self.next();
			let next_type = self.parse_primary_data_type()?;

			if !data_types.contains(&next_type) {
				data_types.push(next_type);
			}
		}

		if data_types.len() == 1 {
			Ok(data_types.pop().unwrap())
		}
		else {
			Ok(DataType::Union(data_types))
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

	fn parse_expression_with_binding_power(
		&mut self,
		binding_power: BindingPower,
		treat_equal_as_equality: bool,
	) -> Result<Expr, ParseError> {
		let mut left = self.parse_prefix()?;

		while let Some(token) = self.current() {
			let next_binding_power = match token.kind {
				TokenKind::AndKeyword => BindingPower::LogicalAnd,
				TokenKind::Asterisk => BindingPower::Multiplicative,
				TokenKind::BangEqual => BindingPower::Equality,
				TokenKind::Dash => BindingPower::Additive,
				TokenKind::Dot => BindingPower::Call,
				TokenKind::Equal if treat_equal_as_equality => BindingPower::Equality,
				TokenKind::EqualEqual => BindingPower::Equality,
				TokenKind::ForwardSlash => BindingPower::Multiplicative,
				TokenKind::GreaterThan => BindingPower::Comparison,
				TokenKind::GreaterThanOrEqual => BindingPower::Comparison,
				TokenKind::LeftBrace if self.is_qualified_identifier_expr(&left) => BindingPower::Call,
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
			left = self.parse_infix(left, operator, next_binding_power, treat_equal_as_equality)?;
		}

		Ok(left)
	}

	fn parse_field_access_expression(&mut self, object: Expr, start: usize) -> Result<Expr, ParseError> {
		let field = self.expect_token(TokenKind::Identifier, "Expected field name after `.`.")?;

		Ok(Expr::FieldAccess(FieldAccessExpr {
			field: IdentifierExpr {
				name: field.lexeme,
				position: field.start,
			},
			object: Box::new(object),
			position: start,
		}))
	}

	fn parse_find_expression(&mut self, start: usize) -> Result<Expr, ParseError> {
		let kind = match self.current().map(|token| token.kind) {
			Some(TokenKind::FirstKeyword) => {
				self.next();
				FindKind::First
			}
			Some(TokenKind::LastKeyword) => {
				self.next();
				FindKind::Last
			}
			_ => FindKind::Any,
		};
		let table = self.parse_table_reference()?;
		let where_clause = if self.current().is_some_and(|token| token.kind == TokenKind::WhereKeyword) {
			self.next();
			Some(Box::new(self.parse_query_expression_with_binding_power(BindingPower::Default, true)?))
		}
		else {
			None
		};
		let order_by = self.parse_optional_order_by_clause()?;

		Ok(Expr::Find(FindExpr {
			kind,
			order_by,
			position: start,
			table,
			where_clause,
		}))
	}

	fn parse_for_record_statement(&mut self, position: usize) -> Result<Statement, ParseError> {
		self.expect_token(TokenKind::RecKeyword, "Expected `rec` after `for`.")?;
		let is_mut = if self.current().is_some_and(|token| token.kind == TokenKind::MutKeyword) {
			self.next();
			true
		}
		else {
			false
		};
		let variable = self.expect_token(TokenKind::Identifier, "Expected record loop variable name.")?;
		self.expect_token(TokenKind::InKeyword, "Expected `in` after record loop variable name.")?;
		let table = self.parse_table_reference()?;
		let where_clause = if self.current().is_some_and(|token| token.kind == TokenKind::WhereKeyword) {
			self.next();
			Some(Box::new(self.parse_query_expression_with_binding_power(BindingPower::Default, true)?))
		}
		else {
			None
		};
		let order_by = self.parse_optional_order_by_clause()?;
		let body = match self.parse_block_statement()? {
			Statement::Block(block) => block,
			_ => unreachable!("Block parser must return a block statement."),
		};

		Ok(Statement::ForRecord(ForRecordStatement {
			body,
			is_mut,
			order_by,
			position,
			table,
			variable: IdentifierExpr {
				name: variable.lexeme,
				position: variable.start,
			},
			where_clause,
		}))
	}

	fn parse_for_statement(&mut self) -> Result<Statement, ParseError> {
		let for_keyword = self.expect_token(TokenKind::ForKeyword, "Expected `for` to start for statement.")?;

		if self.current().is_some_and(|token| token.kind == TokenKind::RecKeyword) {
			return self.parse_for_record_statement(for_keyword.start);
		}

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
		let expression = self.parse_expression_with_binding_power(BindingPower::Default, false)?;
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

	fn parse_infix(
		&mut self,
		left: Expr,
		operator: Token,
		binding_power: BindingPower,
		treat_equal_as_equality: bool,
	) -> Result<Expr, ParseError> {
		match operator.kind {
			TokenKind::AndKeyword => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::And,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::Asterisk => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Multiply,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::BangEqual => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::NotEqual,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::Dash => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Subtract,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::Dot => self.parse_field_access_expression(left, operator.start),
			TokenKind::Equal if treat_equal_as_equality => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Equal,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::EqualEqual => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Equal,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::ForwardSlash => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Divide,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::GreaterThan => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::GreaterThan,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::GreaterThanOrEqual => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::GreaterThanOrEqual,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::LeftBrace => self.parse_object_construction_expression(left, operator.start),
			TokenKind::LeftBracket => self.parse_index_expression(left, operator.start),
			TokenKind::LeftParenthesis => self.parse_call_expression(left, operator.start),
			TokenKind::LessThan => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::LessThan,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::LessThanOrEqual => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::LessThanOrEqual,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::OrKeyword => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Or,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::Percent => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Modulo,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::Plus => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

				Ok(Expr::Binary(BinaryExpr {
					left: Box::new(left),
					operator: BinaryOperator::Add,
					position: operator.start,
					right: Box::new(right),
				}))
			}
			TokenKind::XorKeyword => {
				let right = self.parse_expression_with_binding_power(binding_power, treat_equal_as_equality)?;

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
		let operand = self.parse_expression_with_binding_power(BindingPower::Unary, false)?;

		Ok(Expr::Unary(UnaryExpr {
			operand: Box::new(operand),
			operator: UnaryOperator::Negate,
			position,
		}))
	}

	fn parse_not_expression(&mut self, position: usize) -> Result<Expr, ParseError> {
		let operand = self.parse_expression_with_binding_power(BindingPower::Unary, false)?;

		Ok(Expr::Unary(UnaryExpr {
			operand: Box::new(operand),
			operator: UnaryOperator::Not,
			position,
		}))
	}

	fn parse_object_construction_expression(&mut self, object_type: Expr, start: usize) -> Result<Expr, ParseError> {
		let object_type_name = self.qualified_identifier_name_from_expr(object_type, start)?;

		let mut fields = Vec::new();

		if !self.current().is_some_and(|token| token.kind == TokenKind::RightBrace) {
			loop {
				let name = self.expect_token(TokenKind::Identifier, "Expected object field name.")?;
				self.expect_token(TokenKind::Colon, "Expected `:` after object field name.")?;
				let value = self.parse_assignment_expression()?;
				fields.push(ObjectConstructionField {
					name: name.lexeme,
					position: name.start,
					value,
				});

				match self.current() {
					Some(token) if token.kind == TokenKind::Comma => {
						self.next();

						if self.current().is_some_and(|token| token.kind == TokenKind::RightBrace) {
							break;
						}
					}
					_ => break,
				}
			}
		}

		self.expect_token(TokenKind::RightBrace, "Expected `}` to close object construction.")?;

		Ok(Expr::ObjectConstruction(ObjectConstructionExpr {
			fields,
			object_type_name,
			position: start,
		}))
	}

	fn parse_object_declaration(&mut self) -> Result<Vec<ObjectDeclaration>, ParseError> {
		let object_keyword = self.expect_token(TokenKind::ObjKeyword, "Expected `obj` to start object declaration.")?;
		let name = self.expect_token(TokenKind::Identifier, "Expected object type name.")?;
		let objects = self.parse_object_shape_declaration(name.lexeme, object_keyword.start)?;
		self.expect_token(TokenKind::Semicolon, "Expected `;` after object declaration.")?;
		Ok(objects)
	}

	fn parse_object_field_data_type(
		&mut self,
		containing_object_name: &str,
		field_name: &str,
		field_position: usize,
	) -> Result<(DataType, Vec<ObjectDeclaration>), ParseError> {
		let mut data_types = Vec::new();
		let mut nested_objects = Vec::new();

		loop {
			let member_index = data_types.len();
			let (data_type, member_nested_objects) = self.parse_object_field_type_component(
				containing_object_name,
				field_name,
				field_position,
				member_index,
			)?;

			if !data_types.contains(&data_type) {
				data_types.push(data_type);
			}

			nested_objects.extend(member_nested_objects);

			if !self.current().is_some_and(|token| token.kind == TokenKind::Pipe) {
				break;
			}

			self.next();
		}

		let data_type = if data_types.len() == 1 {
			data_types.pop().unwrap()
		}
		else {
			DataType::Union(data_types)
		};

		Ok((data_type, nested_objects))
	}

	fn parse_object_field_declaration(
		&mut self,
		containing_object_name: &str,
	) -> Result<(ObjectFieldDeclaration, Vec<ObjectDeclaration>), ParseError> {
		let name = self.expect_token(TokenKind::Identifier, "Expected object field name.")?;
		self.expect_token(TokenKind::Colon, "Expected `:` after object field name.")?;
		let (data_type, mut nested_objects) = self.parse_object_field_data_type(containing_object_name, &name.lexeme, name.start)?;

		let default_value = if self.current().is_some_and(|token| token.kind == TokenKind::Equal) {
			self.next();
			Some(self.parse_assignment_expression()?)
		}
		else {
			None
		};

		Ok((
			ObjectFieldDeclaration {
				data_type,
				default_value,
				name: name.lexeme,
				position: name.start,
			},
			std::mem::take(&mut nested_objects),
		))
	}

	fn parse_object_field_type_component(
		&mut self,
		containing_object_name: &str,
		field_name: &str,
		field_position: usize,
		member_index: usize,
	) -> Result<(DataType, Vec<ObjectDeclaration>), ParseError> {
		if self.current().is_some_and(|token| token.kind == TokenKind::LeftBracket) {
			self.next();
			let element_field_name = self.object_field_array_element_name(field_name, member_index);
			let (element_type, nested_objects) = self.parse_object_field_data_type(
				containing_object_name,
				&element_field_name,
				field_position,
			)?;
			self.expect_token(TokenKind::RightBracket, "Expected `]` after array element type.")?;
			return Ok((DataType::Array(Box::new(element_type)), nested_objects));
		}

		if self.current().is_some_and(|token| token.kind == TokenKind::ObjKeyword) {
			let object_keyword = self.expect_token(TokenKind::ObjKeyword, "Expected `obj` to start inline object declaration.")?;
			let object_name = self.expect_token(TokenKind::Identifier, "Expected inline object type name.")?;
			let qualified_name = format!("{containing_object_name}.{}", object_name.lexeme);
			return Ok((
				DataType::Object(qualified_name.clone()),
				self.parse_object_shape_declaration(qualified_name, object_keyword.start)?,
			));
		}

		if self.current().is_some_and(|token| token.kind == TokenKind::LeftBrace) {
			let qualified_name = format!(
				"{containing_object_name}.{}",
				self.object_field_member_name(field_name, member_index),
			);

			return Ok((
				DataType::Object(qualified_name.clone()),
				self.parse_object_shape_declaration(qualified_name, field_position)?,
			));
		}

		Ok((self.parse_primary_data_type()?, vec![]))
	}

	fn parse_object_shape_declaration(
		&mut self,
		name: String,
		position: usize,
	) -> Result<Vec<ObjectDeclaration>, ParseError> {
		self.expect_token(TokenKind::LeftBrace, "Expected `{` to start object field list.")?;

		let mut fields = Vec::new();
		let mut nested_objects = Vec::new();

		if !self.current().is_some_and(|token| token.kind == TokenKind::RightBrace) {
			loop {
				let (field, field_nested_objects) = self.parse_object_field_declaration(&name)?;
				fields.push(field);
				nested_objects.extend(field_nested_objects);

				match self.current() {
					Some(token) if token.kind == TokenKind::Comma => {
						self.next();

						if self.current().is_some_and(|token| token.kind == TokenKind::RightBrace) {
							break;
						}
					}
					_ => break,
				}
			}
		}

		self.expect_token(TokenKind::RightBrace, "Expected `}` to close object declaration.")?;
		let mut objects = vec![ObjectDeclaration {
			fields,
			name,
			position,
		}];
		objects.extend(nested_objects);
		Ok(objects)
	}

	fn parse_optional_order_by_clause(&mut self) -> Result<Vec<OrderByItem>, ParseError> {
		if !self.current().is_some_and(|token| token.kind == TokenKind::OrderKeyword) {
			return Ok(vec![]);
		}

		self.next();
		self.expect_token(TokenKind::ByKeyword, "Expected `by` after `order`.")?;
		let mut items = Vec::new();

		loop {
			let expression = self.parse_query_expression_with_binding_power(BindingPower::Default, true)?;
			let direction = match self.current().map(|token| token.kind) {
				Some(TokenKind::AscKeyword) => {
					self.next();
					OrderByDirection::Ascending
				}
				Some(TokenKind::DescKeyword) => {
					self.next();
					OrderByDirection::Descending
				}
				_ => OrderByDirection::Ascending,
			};

			items.push(OrderByItem {
				direction,
				position: expression.position(),
				expression,
			});

			if !self.current().is_some_and(|token| token.kind == TokenKind::Comma) {
				break;
			}

			self.next();
		}

		Ok(items)
	}

	fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
		let token = self.next().ok_or(ParseError {
			message: String::from("Expected an expression."),
			position: 0,
		})?;

		match token.kind {
			TokenKind::CountKeyword => self.parse_count_expression(token.start),
			TokenKind::Dash => self.parse_negation_expression_with_position(token.start),
			TokenKind::DecimalLiteral => self.parse_decimal_literal(token),
			TokenKind::EndOfFile => Err(ParseError {
				message: String::from("Expected an expression."),
				position: token.start,
			}),
			TokenKind::FalseKeyword | TokenKind::TrueKeyword => self.parse_boolean_literal(token),
			TokenKind::FindKeyword => self.parse_find_expression(token.start),
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

	fn parse_primary_data_type(&mut self) -> Result<DataType, ParseError> {
		let token = self.next().ok_or(ParseError {
			message: String::from("Expected a data type."),
			position: 0,
		})?;

		match token.kind {
			TokenKind::AnyKeyword => Ok(DataType::Any),
			TokenKind::BoolKeyword => Ok(DataType::Bool),
			TokenKind::DecKeyword => Ok(DataType::Dec),
			TokenKind::Identifier => Ok(DataType::Object(self.parse_qualified_identifier_name(token)?)),
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

	fn parse_qualified_identifier_name(&mut self, first: Token) -> Result<String, ParseError> {
		let mut name = first.lexeme;

		while self.current().is_some_and(|token| token.kind == TokenKind::Dot) {
			self.next();
			let component = self.expect_token(TokenKind::Identifier, "Expected identifier after `.`.")?;
			name.push('.');
			name.push_str(&component.lexeme);
		}

		Ok(name)
	}

	fn parse_query_expression_with_binding_power(
		&mut self,
		binding_power: BindingPower,
		treat_equal_as_equality: bool,
	) -> Result<Expr, ParseError> {
		let mut left = self.parse_prefix()?;

		while let Some(token) = self.current() {
			let next_binding_power = match token.kind {
				TokenKind::AndKeyword => BindingPower::LogicalAnd,
				TokenKind::Asterisk => BindingPower::Multiplicative,
				TokenKind::BangEqual => BindingPower::Equality,
				TokenKind::Dash => BindingPower::Additive,
				TokenKind::Dot => BindingPower::Call,
				TokenKind::Equal if treat_equal_as_equality => BindingPower::Equality,
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
			left = self.parse_infix(left, operator, next_binding_power, treat_equal_as_equality)?;
		}

		Ok(left)
	}

	fn parse_range_expression(&mut self) -> Result<Expr, ParseError> {
		let start = self.parse_expression_with_binding_power(BindingPower::Default, false)?;

		if !self.current().is_some_and(|token| token.kind == TokenKind::Colon) {
			return Ok(start);
		}

		let colon = self.next().unwrap();
		let middle = self.parse_expression_with_binding_power(BindingPower::Default, false)?;

		if self.current().is_some_and(|token| token.kind == TokenKind::Colon) {
			self.next();
			let end = self.parse_expression_with_binding_power(BindingPower::Default, false)?;

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

	fn parse_record_pointer_declaration_statement(&mut self) -> Result<Statement, ParseError> {
		let rec_keyword = self.expect_token(TokenKind::RecKeyword, "Expected `rec` to start record pointer declaration.")?;
		let is_mut = if self.current().is_some_and(|token| token.kind == TokenKind::MutKeyword) {
			self.next();
			true
		}
		else {
			false
		};
		let name = self.expect_token(TokenKind::Identifier, "Expected record pointer name.")?;
		self.expect_token(TokenKind::Equal, "Expected `=` after record pointer name.")?;
		let initial_value = self.parse_assignment_expression()?;
		self.expect_token(TokenKind::Semicolon, "Expected `;` after record pointer declaration.")?;

		Ok(Statement::RecordPointerDeclaration(RecordPointerDeclaration {
			initial_value,
			is_mut,
			name: name.lexeme,
			position: rec_keyword.start,
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
			Some(token) if token.kind == TokenKind::RecKeyword => self.parse_record_pointer_declaration_statement(),
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

	fn parse_table_reference(&mut self) -> Result<TableReference, ParseError> {
		let first = self.expect_token(TokenKind::Identifier, "Expected table name.")?;
		let mut components = vec![IdentifierExpr {
			name: first.lexeme,
			position: first.start,
		}];

		while self.current().is_some_and(|token| token.kind == TokenKind::Dot) {
			self.next();
			let next = self.expect_token(TokenKind::Identifier, "Expected identifier after `.` in table reference.")?;
			components.push(IdentifierExpr {
				name: next.lexeme,
				position: next.start,
			});
		}

		if components.len() > 3 {
			return Err(ParseError {
				message: String::from("Table references may contain at most three identifier components."),
				position: components[3].position,
			});
		}

		Ok(TableReference {
			position: components[0].position,
			components,
		})
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

	fn parse_with_declaration(&mut self) -> Result<WithDeclaration, ParseError> {
		let with_keyword = self.expect_token(TokenKind::WithKeyword, "Expected `with` to start database declaration.")?;
		let mut databases = Vec::new();

		loop {
			let database = self.expect_token(TokenKind::Identifier, "Expected database name after `with`.")?;
			databases.push(IdentifierExpr {
				name: database.lexeme,
				position: database.start,
			});

			match self.current() {
				Some(token) if token.kind == TokenKind::Comma => {
					self.next();
				}
				_ => break,
			}
		}

		self.expect_token(TokenKind::Semicolon, "Expected `;` after `with` declaration.")?;

		Ok(WithDeclaration {
			databases,
			position: with_keyword.start,
		})
	}

	fn qualified_identifier_name_from_expr(&self, expression: Expr, position: usize) -> Result<String, ParseError> {
		match expression {
			Expr::Identifier(identifier) => Ok(identifier.name),
			Expr::FieldAccess(field_access) => {
				let mut name = self.qualified_identifier_name_from_expr(*field_access.object, position)?;
				name.push('.');
				name.push_str(&field_access.field.name);
				Ok(name)
			}
			_ => Err(ParseError {
				message: String::from("Object construction target must be an identifier or qualified type name."),
				position,
			}),
		}
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
	use crate::ast::CountExpr;
	use crate::ast::DataType;
	use crate::ast::DecimalLiteral;
	use crate::ast::Expr;
	use crate::ast::FieldAccessExpr;
	use crate::ast::FindExpr;
	use crate::ast::FindKind;
	use crate::ast::ForRecordStatement;
	use crate::ast::ForStatement;
	use crate::ast::FunctionDeclaration;
	use crate::ast::FunctionParameter;
	use crate::ast::IdentifierExpr;
	use crate::ast::IfStatement;
	use crate::ast::IndexExpr;
	use crate::ast::IntegerLiteral;
	use crate::ast::ObjectConstructionExpr;
	use crate::ast::ObjectConstructionField;
	use crate::ast::ObjectDeclaration;
	use crate::ast::ObjectFieldAssignmentTarget;
	use crate::ast::ObjectFieldDeclaration;
	use crate::ast::OrderByDirection;
	use crate::ast::OrderByItem;
	use crate::ast::Program;
	use crate::ast::RangeExpr;
	use crate::ast::RecordPointerDeclaration;
	use crate::ast::ReturnStatement;
	use crate::ast::Statement;
	use crate::ast::TableReference;
	use crate::ast::TextLiteral;
	use crate::ast::UnaryExpr;
	use crate::ast::UnaryOperator;
	use crate::ast::VariableDeclaration;
	use crate::ast::WhileStatement;
	use crate::ast::WithDeclaration;
	use crate::source::SourceText;
	use crate::value::Decimal;

	use super::super::lexer::Lexer;
	use super::Parser;

	fn normalize_assignment_target(target: AssignmentTarget) -> AssignmentTarget {
		match target {
			AssignmentTarget::Field(target) => AssignmentTarget::Field(ObjectFieldAssignmentTarget {
				fields: target.fields.into_iter().map(normalize_identifier).collect(),
				object: normalize_identifier(target.object),
				position: 0,
			}),
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
			Expr::Count(CountExpr { table, where_clause, .. }) => Expr::Count(CountExpr {
				position: 0,
				table: normalize_table_reference(table),
				where_clause: where_clause.map(|where_clause| Box::new(normalize_expr(*where_clause))),
			}),
			Expr::Decimal(DecimalLiteral { value, .. }) => Expr::Decimal(DecimalLiteral {
				position: 0,
				value,
			}),
			Expr::FieldAccess(FieldAccessExpr { field, object, .. }) => Expr::FieldAccess(FieldAccessExpr {
				field: normalize_identifier(field),
				object: Box::new(normalize_expr(*object)),
				position: 0,
			}),
			Expr::Find(FindExpr { kind, order_by, table, where_clause, .. }) => Expr::Find(FindExpr {
				kind,
				order_by: order_by.into_iter().map(normalize_order_by_item).collect(),
				position: 0,
				table: normalize_table_reference(table),
				where_clause: where_clause.map(|expression| Box::new(normalize_expr(*expression))),
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
			Expr::ObjectConstruction(ObjectConstructionExpr {
				fields,
				object_type_name,
				..
			}) => Expr::ObjectConstruction(ObjectConstructionExpr {
				fields: fields.into_iter().map(normalize_object_construction_field).collect(),
				object_type_name,
				position: 0,
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

	fn normalize_for_record_statement(for_statement: ForRecordStatement) -> ForRecordStatement {
		ForRecordStatement {
			body: normalize_block(for_statement.body),
			is_mut: for_statement.is_mut,
			order_by: for_statement.order_by.into_iter().map(normalize_order_by_item).collect(),
			position: 0,
			table: normalize_table_reference(for_statement.table),
			variable: normalize_identifier(for_statement.variable),
			where_clause: for_statement.where_clause.map(|expression| Box::new(normalize_expr(*expression))),
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

	fn normalize_if_statement(if_statement: IfStatement) -> IfStatement {
		IfStatement {
			condition: normalize_expr(if_statement.condition),
			else_branch: if_statement.else_branch.map(|branch| Box::new(normalize_statement(*branch))),
			position: 0,
			then_branch: normalize_block(if_statement.then_branch),
		}
	}

	fn normalize_object_construction_field(field: ObjectConstructionField) -> ObjectConstructionField {
		ObjectConstructionField {
			name: field.name,
			position: 0,
			value: normalize_expr(field.value),
		}
	}

	fn normalize_object_declaration(object: ObjectDeclaration) -> ObjectDeclaration {
		ObjectDeclaration {
			fields: object.fields.into_iter().map(normalize_object_field_declaration).collect(),
			name: object.name,
			position: 0,
		}
	}

	fn normalize_object_field_declaration(field: ObjectFieldDeclaration) -> ObjectFieldDeclaration {
		ObjectFieldDeclaration {
			data_type: field.data_type,
			default_value: field.default_value.map(normalize_expr),
			name: field.name,
			position: 0,
		}
	}

	fn normalize_order_by_item(item: OrderByItem) -> OrderByItem {
		OrderByItem {
			direction: item.direction,
			expression: normalize_expr(item.expression),
			position: 0,
		}
	}

	fn normalize_program(program: Program) -> Program {
		Program {
			functions: program.functions.into_iter().map(normalize_function_declaration).collect(),
			objects: program.objects.into_iter().map(normalize_object_declaration).collect(),
			result: program.result.map(normalize_expr),
			statements: program.statements.into_iter().map(normalize_statement).collect(),
			with_declarations: program.with_declarations.into_iter().map(normalize_with_declaration).collect(),
		}
	}

	fn normalize_record_pointer_declaration(declaration: RecordPointerDeclaration) -> RecordPointerDeclaration {
		RecordPointerDeclaration {
			initial_value: normalize_expr(declaration.initial_value),
			is_mut: declaration.is_mut,
			name: declaration.name,
			position: 0,
		}
	}

	fn normalize_return_statement(return_statement: ReturnStatement) -> ReturnStatement {
		ReturnStatement {
			position: 0,
			value: return_statement.value.map(normalize_expr),
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
			Statement::ForRecord(for_statement) => Statement::ForRecord(normalize_for_record_statement(for_statement)),
			Statement::FunctionDeclaration(function) => {
				Statement::FunctionDeclaration(normalize_function_declaration(function))
			}
			Statement::If(if_statement) => Statement::If(normalize_if_statement(if_statement)),
			Statement::RecordPointerDeclaration(declaration) => {
				Statement::RecordPointerDeclaration(normalize_record_pointer_declaration(declaration))
			}
			Statement::Return(return_statement) => Statement::Return(normalize_return_statement(return_statement)),
			Statement::VariableDeclaration(declaration) => {
				Statement::VariableDeclaration(normalize_variable_declaration(declaration))
			}
			Statement::While(while_statement) => Statement::While(normalize_while_statement(while_statement)),
		}
	}

	fn normalize_table_reference(table: TableReference) -> TableReference {
		TableReference {
			components: table.components.into_iter().map(normalize_identifier).collect(),
			position: 0,
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

	fn normalize_with_declaration(with_declaration: WithDeclaration) -> WithDeclaration {
		WithDeclaration {
			databases: with_declaration.databases.into_iter().map(normalize_identifier).collect(),
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
	fn parses_anonymous_inline_object_declaration_as_field_qualified_object() {
		let program = parse_program(
			"obj Outer { inner: { value: int, }, label: text, };"
		);

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Object(String::from("Outer.inner")),
							default_value: None,
							name: String::from("inner"),
							position: 0,
						},
						ObjectFieldDeclaration {
							data_type: DataType::Text,
							default_value: None,
							name: String::from("label"),
							position: 0,
						},
					],
					name: String::from("Outer"),
					position: 0,
				},
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Int,
							default_value: None,
							name: String::from("value"),
							position: 0,
						},
					],
					name: String::from("Outer.inner"),
					position: 0,
				},
			],
			result: None,
			statements: vec![],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_anonymous_inline_object_declaration_in_array_field() {
		let program = parse_program(
			"obj Outer { items: [{ value: int, }], };"
		);

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Array(Box::new(DataType::Object(String::from("Outer.items.Element")))),
							default_value: None,
							name: String::from("items"),
							position: 0,
						},
					],
					name: String::from("Outer"),
					position: 0,
				},
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Int,
							default_value: None,
							name: String::from("value"),
							position: 0,
						},
					],
					name: String::from("Outer.items.Element"),
					position: 0,
				},
			],
			result: None,
			statements: vec![],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_any_data_type_in_variable_and_array_positions() {
		assert_eq!(
			parse_program("fn Main(args: [text]) int { var value: any = 1; var values: [any] = []; return 0; }"),
			Program {
				functions: vec![
					FunctionDeclaration {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::VariableDeclaration(VariableDeclaration {
									data_type: DataType::Any,
									initial_value: Some(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 1,
									})),
									is_const: false,
									name: String::from("value"),
									position: 0,
								}),
								Statement::VariableDeclaration(VariableDeclaration {
									data_type: DataType::Array(Box::new(DataType::Any)),
									initial_value: Some(Expr::Array(ArrayLiteral {
										elements: vec![],
										position: 0,
									})),
									is_const: false,
									name: String::from("values"),
									position: 0,
								}),
								Statement::Return(ReturnStatement {
									position: 0,
									value: Some(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 0,
									})),
								}),
							],
						},
						name: String::from("Main"),
						parameters: vec![
							FunctionParameter {
								data_type: DataType::Array(Box::new(DataType::Text)),
								is_by_ref: false,
								name: String::from("args"),
								position: 0,
							},
						],
						position: 0,
						return_type: DataType::Int,
					},
				],
				objects: vec![],
				result: None,
				statements: vec![],
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_array_index_expression() {
		let program = parse_program("xs[1]");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
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
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_array_literal() {
		let program = parse_program("[1, 2]");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
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
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_array_slice_expression() {
		let program = parse_program("xs[2:4]");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
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
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_array_variable_declaration() {
		let program = parse_program("var xs: [int] = [];");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
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
			with_declarations: vec![],
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
				objects: vec![],
				result: None,
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
				with_declarations: vec![],
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
				objects: vec![],
				result: None,
				statements: vec![],
				with_declarations: vec![],
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
			objects: vec![],
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
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_const_variable_declaration() {
		assert_eq!(
			parse_program("const name: text = 'Tablo';"),
			Program {
				functions: vec![],
				objects: vec![],
				result: None,
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
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_count_expression_with_where_clause() {
		let program = parse_program("count sales.customers where active = true");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
			result: Some(Expr::Count(CountExpr {
				position: 0,
				table: TableReference {
					components: vec![
						IdentifierExpr {
							name: String::from("sales"),
							position: 0,
						},
						IdentifierExpr {
							name: String::from("customers"),
							position: 0,
						},
					],
					position: 0,
				},
				where_clause: Some(Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Identifier(IdentifierExpr {
						name: String::from("active"),
						position: 0,
					})),
					operator: BinaryOperator::Equal,
					position: 0,
					right: Box::new(Expr::Boolean(BooleanLiteral {
						position: 0,
						value: true,
					})),
				}))),
			})),
			statements: vec![],
			with_declarations: vec![],
		});
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
				objects: vec![],
				result: Some(Expr::Identifier(IdentifierExpr {
					position: 0,
					name: String::from("x"),
				})),
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
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_find_expression_with_order_by_clause() {
		let program = parse_program("find customers order by name desc, id");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
			result: Some(Expr::Find(FindExpr {
				kind: FindKind::Any,
				order_by: vec![
					OrderByItem {
						direction: OrderByDirection::Descending,
						expression: Expr::Identifier(IdentifierExpr {
							name: String::from("name"),
							position: 0,
						}),
						position: 0,
					},
					OrderByItem {
						direction: OrderByDirection::Ascending,
						expression: Expr::Identifier(IdentifierExpr {
							name: String::from("id"),
							position: 0,
						}),
						position: 0,
					},
				],
				position: 0,
				table: TableReference {
					components: vec![
						IdentifierExpr {
							name: String::from("customers"),
							position: 0,
						},
					],
					position: 0,
				},
				where_clause: None,
			})),
			statements: vec![],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_find_first_expression_with_where_clause() {
		let program = parse_program("find first sales.customers where active = true");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
			result: Some(Expr::Find(FindExpr {
				kind: FindKind::First,
				order_by: vec![],
				position: 0,
				table: TableReference {
					components: vec![
						IdentifierExpr {
							name: String::from("sales"),
							position: 0,
						},
						IdentifierExpr {
							name: String::from("customers"),
							position: 0,
						},
					],
					position: 0,
				},
				where_clause: Some(Box::new(Expr::Binary(BinaryExpr {
					left: Box::new(Expr::Identifier(IdentifierExpr {
						name: String::from("active"),
						position: 0,
					})),
					operator: BinaryOperator::Equal,
					position: 0,
					right: Box::new(Expr::Boolean(BooleanLiteral {
						position: 0,
						value: true,
					})),
				}))),
			})),
			statements: vec![],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_find_last_expression() {
		let program = parse_program("find last customers");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
			result: Some(Expr::Find(FindExpr {
				kind: FindKind::Last,
				order_by: vec![],
				position: 0,
				table: TableReference {
					components: vec![
						IdentifierExpr {
							name: String::from("customers"),
							position: 0,
						},
					],
					position: 0,
				},
				where_clause: None,
			})),
			statements: vec![],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_for_record_statement() {
		assert_eq!(
			normalize_program(parse_program("for rec mut cust in customers where active = true order by name desc { cust.name; }")),
			Program {
				functions: vec![],
				objects: vec![],
				result: None,
				statements: vec![
					Statement::ForRecord(ForRecordStatement {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::Expression(Expr::FieldAccess(FieldAccessExpr {
									field: IdentifierExpr {
										name: String::from("name"),
										position: 0,
									},
									object: Box::new(Expr::Identifier(IdentifierExpr {
										name: String::from("cust"),
										position: 0,
									})),
									position: 0,
								})),
							],
						},
						is_mut: true,
						order_by: vec![
							OrderByItem {
								direction: OrderByDirection::Descending,
								expression: Expr::Identifier(IdentifierExpr {
									name: String::from("name"),
									position: 0,
								}),
								position: 0,
							},
						],
						position: 0,
						table: TableReference {
							components: vec![
								IdentifierExpr {
									name: String::from("customers"),
									position: 0,
								},
							],
							position: 0,
						},
						variable: IdentifierExpr {
							name: String::from("cust"),
							position: 0,
						},
						where_clause: Some(Box::new(Expr::Binary(BinaryExpr {
							left: Box::new(Expr::Identifier(IdentifierExpr {
								name: String::from("active"),
								position: 0,
							})),
							operator: BinaryOperator::Equal,
							position: 0,
							right: Box::new(Expr::Boolean(BooleanLiteral {
								position: 0,
								value: true,
							})),
						}))),
					}),
				],
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_for_statement() {
		assert_eq!(
			parse_program("for value in [1, 2] { value; }"),
			Program {
				functions: vec![],
				objects: vec![],
				result: None,
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
				with_declarations: vec![],
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
				objects: vec![],
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
				with_declarations: vec![],
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
				objects: vec![],
				result: None,
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
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_if_else_statement() {
		assert_eq!(
			parse_program("if true { var x: int = 1; } else { var x: int = 2; }"),
			Program {
				functions: vec![],
				objects: vec![],
				result: None,
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
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_if_statement() {
		assert_eq!(
			parse_program("if true { var x: int = 1; }"),
			Program {
				functions: vec![],
				objects: vec![],
				result: None,
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
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_indexed_assignment_expression() {
		let program = parse_program("xs[1] = 2;");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
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
			with_declarations: vec![],
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
	fn parses_named_inline_object_declaration_as_qualified_object() {
		let program = parse_program(
			"obj Outer { inner: obj Inner { value: int, }, label: text, };"
		);

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Object(String::from("Outer.Inner")),
							default_value: None,
							name: String::from("inner"),
							position: 0,
						},
						ObjectFieldDeclaration {
							data_type: DataType::Text,
							default_value: None,
							name: String::from("label"),
							position: 0,
						},
					],
					name: String::from("Outer"),
					position: 0,
				},
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Int,
							default_value: None,
							name: String::from("value"),
							position: 0,
						},
					],
					name: String::from("Outer.Inner"),
					position: 0,
				},
			],
			result: None,
			statements: vec![],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_named_inline_object_declaration_in_array_field() {
		let program = parse_program(
			"obj Outer { items: [obj Item { value: int, }], };"
		);

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Array(Box::new(DataType::Object(String::from("Outer.Item")))),
							default_value: None,
							name: String::from("items"),
							position: 0,
						},
					],
					name: String::from("Outer"),
					position: 0,
				},
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Int,
							default_value: None,
							name: String::from("value"),
							position: 0,
						},
					],
					name: String::from("Outer.Item"),
					position: 0,
				},
			],
			result: None,
			statements: vec![],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_named_inline_object_declaration_in_union_field() {
		let program = parse_program(
			"obj Envelope { payload: text | obj Payload { value: int, }, };"
		);

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Union(vec![
								DataType::Text,
								DataType::Object(String::from("Envelope.Payload")),
							]),
							default_value: None,
							name: String::from("payload"),
							position: 0,
						},
					],
					name: String::from("Envelope"),
					position: 0,
				},
				ObjectDeclaration {
					fields: vec![
						ObjectFieldDeclaration {
							data_type: DataType::Int,
							default_value: None,
							name: String::from("value"),
							position: 0,
						},
					],
					name: String::from("Envelope.Payload"),
					position: 0,
				},
			],
			result: None,
			statements: vec![],
			with_declarations: vec![],
		});
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
				objects: vec![],
				result: None,
				statements: vec![],
				with_declarations: vec![],
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
	fn parses_object_declaration_and_construction() {
		assert_eq!(
			parse_program("obj Person { name: text = '', age: int, };\nPerson { age: 1 }.age"),
			Program {
				functions: vec![],
				objects: vec![
					ObjectDeclaration {
						fields: vec![
							ObjectFieldDeclaration {
								data_type: DataType::Text,
								default_value: Some(Expr::Text(TextLiteral {
									position: 0,
									value: String::from(""),
								})),
								name: String::from("name"),
								position: 0,
							},
							ObjectFieldDeclaration {
								data_type: DataType::Int,
								default_value: None,
								name: String::from("age"),
								position: 0,
							},
						],
						name: String::from("Person"),
						position: 0,
					},
				],
				result: Some(Expr::FieldAccess(FieldAccessExpr {
					field: IdentifierExpr {
						name: String::from("age"),
						position: 0,
					},
					object: Box::new(Expr::ObjectConstruction(ObjectConstructionExpr {
						fields: vec![
							ObjectConstructionField {
								name: String::from("age"),
								position: 0,
								value: Expr::Integer(IntegerLiteral {
									position: 0,
									value: 1,
								}),
							},
						],
						object_type_name: String::from("Person"),
						position: 0,
					})),
					position: 0,
				})),
				statements: vec![],
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_object_field_assignment_expression() {
		let program = parse_program("person.address.line1 = 'Updated';");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
			result: None,
			statements: vec![
				Statement::Expression(Expr::Assignment(AssignmentExpr {
					operator: AssignmentOperator::Assign,
					position: 0,
					target: AssignmentTarget::Field(ObjectFieldAssignmentTarget {
						fields: vec![
							IdentifierExpr {
								name: String::from("address"),
								position: 0,
							},
							IdentifierExpr {
								name: String::from("line1"),
								position: 0,
							},
						],
						object: IdentifierExpr {
							name: String::from("person"),
							position: 0,
						},
						position: 0,
					}),
					value: Box::new(Expr::Text(TextLiteral {
						position: 0,
						value: String::from("Updated"),
					})),
				})),
			],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_program_with_variable_declarations() {
		assert_eq!(
			parse_program("var x: int = 1;\nvar y: int = 2;\nx + y"),
			Program {
				functions: vec![],
				objects: vec![],
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
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_qualified_object_construction_target() {
		let program = parse_program("Outer.Inner { value: 1 };");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
			result: None,
			statements: vec![
				Statement::Expression(Expr::ObjectConstruction(ObjectConstructionExpr {
					fields: vec![
						ObjectConstructionField {
							name: String::from("value"),
							position: 0,
							value: Expr::Integer(IntegerLiteral {
								position: 0,
								value: 1,
							}),
						},
					],
					object_type_name: String::from("Outer.Inner"),
					position: 0,
				})),
			],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_qualified_object_type_names_in_type_positions() {
		let program = parse_program(
			"fn Main(args: [text]) Outer.Inner { var value: [Outer.Inner | text] = []; return value; }"
		);

		assert_eq!(normalize_program(program), Program {
			functions: vec![
				FunctionDeclaration {
					body: BlockStatement {
						position: 0,
						statements: vec![
							Statement::VariableDeclaration(VariableDeclaration {
								data_type: DataType::Array(Box::new(DataType::Union(vec![
									DataType::Object(String::from("Outer.Inner")),
									DataType::Text,
								]))),
								initial_value: Some(Expr::Array(ArrayLiteral {
									elements: vec![],
									position: 0,
								})),
								is_const: false,
								name: String::from("value"),
								position: 0,
							}),
							Statement::Return(ReturnStatement {
								position: 0,
								value: Some(Expr::Identifier(IdentifierExpr {
									name: String::from("value"),
									position: 0,
								})),
							}),
						],
					},
					name: String::from("Main"),
					parameters: vec![
						FunctionParameter {
							data_type: DataType::Array(Box::new(DataType::Text)),
							is_by_ref: false,
							name: String::from("args"),
							position: 0,
						},
					],
					position: 0,
					return_type: DataType::Object(String::from("Outer.Inner")),
				},
			],
			objects: vec![],
			result: None,
			statements: vec![],
			with_declarations: vec![],
		});
	}

	#[test]
	fn parses_record_pointer_declaration() {
		let program = parse_program("rec mut cust = find first customers where active = true;");

		assert_eq!(normalize_program(program), Program {
			functions: vec![],
			objects: vec![],
			result: None,
			statements: vec![
				Statement::RecordPointerDeclaration(RecordPointerDeclaration {
					initial_value: Expr::Find(FindExpr {
						kind: FindKind::First,
						order_by: vec![],
						position: 0,
						table: TableReference {
							components: vec![IdentifierExpr {
								name: String::from("customers"),
								position: 0,
							}],
							position: 0,
						},
						where_clause: Some(Box::new(Expr::Binary(BinaryExpr {
							left: Box::new(Expr::Identifier(IdentifierExpr {
								name: String::from("active"),
								position: 0,
							})),
							operator: BinaryOperator::Equal,
							position: 0,
							right: Box::new(Expr::Boolean(BooleanLiteral {
								position: 0,
								value: true,
							})),
						}))),
					}),
					is_mut: true,
					name: String::from("cust"),
					position: 0,
				}),
			],
			with_declarations: vec![],
		});
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
				objects: vec![],
				result: None,
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
				with_declarations: vec![],
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
	fn parses_union_data_type_in_variable_and_array_positions() {
		assert_eq!(
			parse_program("fn Main(args: [text]) int { var value: int | text = 1; var values: [int | text] = []; return 0; }"),
			Program {
				functions: vec![
					FunctionDeclaration {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::VariableDeclaration(VariableDeclaration {
									data_type: DataType::Union(vec![DataType::Int, DataType::Text]),
									initial_value: Some(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 1,
									})),
									is_const: false,
									name: String::from("value"),
									position: 0,
								}),
								Statement::VariableDeclaration(VariableDeclaration {
									data_type: DataType::Array(Box::new(DataType::Union(vec![DataType::Int, DataType::Text]))),
									initial_value: Some(Expr::Array(ArrayLiteral {
										elements: vec![],
										position: 0,
									})),
									is_const: false,
									name: String::from("values"),
									position: 0,
								}),
								Statement::Return(ReturnStatement {
									position: 0,
									value: Some(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 0,
									})),
								}),
							],
						},
						name: String::from("Main"),
						parameters: vec![
							FunctionParameter {
								data_type: DataType::Array(Box::new(DataType::Text)),
								is_by_ref: false,
								name: String::from("args"),
								position: 0,
							},
						],
						position: 0,
						return_type: DataType::Int,
					},
				],
				objects: vec![],
				result: None,
				statements: vec![],
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_variable_declaration_without_initializer() {
		assert_eq!(
			parse_program("var x: int;"),
			Program {
				functions: vec![],
				objects: vec![],
				result: None,
				statements: vec![
					Statement::VariableDeclaration(VariableDeclaration {
						data_type: DataType::Int,
						initial_value: None,
						is_const: false,
						name: String::from("x"),
						position: 0,
					}),
				],
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_while_statement() {
		assert_eq!(
			parse_program("while x < 3 { x += 1; }"),
			Program {
				functions: vec![],
				objects: vec![],
				result: None,
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
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_while_statement_with_break_and_continue() {
		assert_eq!(
			parse_program("while true { continue; break; }"),
			Program {
				functions: vec![],
				objects: vec![],
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
				with_declarations: vec![],
			}
		);
	}

	#[test]
	fn parses_with_declaration_before_objects_and_functions() {
		assert_eq!(
			parse_program("with exampledb, archivedb;\nobj Person { name: text, };\nfn Main(args: [text]) int { return 0; }"),
			Program {
				functions: vec![
					FunctionDeclaration {
						body: BlockStatement {
							position: 0,
							statements: vec![
								Statement::Return(ReturnStatement {
									position: 0,
									value: Some(Expr::Integer(IntegerLiteral {
										position: 0,
										value: 0,
									})),
								}),
							],
						},
						name: String::from("Main"),
						parameters: vec![
							FunctionParameter {
								data_type: DataType::Array(Box::new(DataType::Text)),
								is_by_ref: false,
								name: String::from("args"),
								position: 0,
							},
						],
						position: 0,
						return_type: DataType::Int,
					},
				],
				objects: vec![
					ObjectDeclaration {
						fields: vec![
							ObjectFieldDeclaration {
								data_type: DataType::Text,
								default_value: None,
								name: String::from("name"),
								position: 0,
							},
						],
						name: String::from("Person"),
						position: 0,
					},
				],
				result: None,
				statements: vec![],
				with_declarations: vec![
					WithDeclaration {
						databases: vec![
							IdentifierExpr {
								name: String::from("exampledb"),
								position: 0,
							},
							IdentifierExpr {
								name: String::from("archivedb"),
								position: 0,
							},
						],
						position: 0,
					},
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
