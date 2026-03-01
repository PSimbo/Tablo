#![allow(unused)]

use std::fmt::Display;

use colored::*;
use colored::ColoredString;

use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::lexer::TokenType::*;

type LedFunction = fn(&mut Parser, left: Ast, binding_power: BindingPower) -> Ast;
type NudFunction = fn(&mut Parser) -> Ast;

trait AstNode: std::any::Any + Clone + 'static {
	fn get_parse_error_message() -> &'static str;
}

#[derive(Clone)]
pub enum Ast {
	Assignment(AssignmentStmt),
	Binary(BinaryExpr),
	Block(BlockStmt),
	Boolean(BooleanLiteral),
	Declaration(DeclarationStmt),
	ForLoop(ForLoop),
	Identifier(Identifier),
	If(IfStmt),
	Integer(IntegerLiteral),
	String(StringLiteral),
}

impl Ast {
	fn unwrap_ref<T: AstNode>(&self, parser: &mut Parser) -> &T {
		let any: &dyn std::any::Any = match &self {
			Ast::Assignment(a) => a,
			Ast::Binary(a) => a,
			Ast::Block(a) => a,
			Ast::Boolean(a) => a,
			Ast::Declaration(a) => a,
			Ast::ForLoop(a) => a,
			Ast::Identifier(a) => a,
			Ast::If(a) => a,
			Ast::Integer(a) => a,
			Ast::String(a) => a,
		};

		let result = (*any).downcast_ref::<T>();

		if result.is_none() {
			throw_parse_error(parser, T::get_parse_error_message());
		}

		result.unwrap()
	}

	fn unwrap<T: AstNode>(&self, parser: &mut Parser) -> T {
		self.unwrap_ref::<T>(parser).clone()
	}
}

#[derive(Clone)]
pub struct AssignmentStmt {
	pub lhs: Box<Ast>,
	pub operator: Token,
	pub rhs: Box<Ast>,
}

impl AstNode for AssignmentStmt {
	fn get_parse_error_message() -> &'static str {
		"Expected an assignment statement."
	}
}

#[derive(Clone)]
pub struct BinaryExpr {
	pub left: Box<Ast>,
	pub operator: Token,
	pub right: Box<Ast>,
}

impl AstNode for BinaryExpr {
	fn get_parse_error_message() -> &'static str {
		"Expected a binary expression."
	}
}

#[derive(Clone)]
pub struct BlockStmt {
	pub body: Vec<Ast>,
}

impl AstNode for BlockStmt {
	fn get_parse_error_message() -> &'static str {
		"Expected a block."
	}
}

#[derive(Clone)]
pub struct BooleanLiteral {
	pub value: bool,
}

impl AstNode for BooleanLiteral {
	fn get_parse_error_message() -> &'static str {
		"Expected a Boolean literal."
	}
}

#[derive(Clone)]
pub struct DeclarationStmt {
	pub variable: Box<Ast>, // Either an `Identifier` or an `Assignment`.
}

impl AstNode for DeclarationStmt {
	fn get_parse_error_message() -> &'static str {
		"Expected a variable declaration."
	}
}

#[derive(Clone)]
pub struct ForLoop {
	pub body: Box<Ast>,
	pub is_mutable: bool,
	pub record_name: Identifier,
	pub table_name: Identifier,
	pub where_clause: Option<Box<Ast>>,
}

impl AstNode for ForLoop {
	fn get_parse_error_message() -> &'static str {
		"Expected a for loop."
	}
}

#[derive(Clone)]
pub struct Identifier {
	pub is_quoted: bool,
	pub name: Token,
}

impl Identifier {
	pub fn to_str(&self, lexer: &Lexer) -> String {
		if self.is_quoted {
			String::from("\"") + self.name.to_str(lexer) + "\""
		}
		else {
			String::from(self.name.to_str(lexer))
		}
	}
}

impl AstNode for Identifier {
	fn get_parse_error_message() -> &'static str {
		"Expected an identifier."
	}
}

#[derive(Clone)]
pub struct IfStmt {
	pub condition: Box<Ast>,
	pub else_arm: Option<Box<Ast>>,
	pub if_arm: Box<Ast>,
}

impl AstNode for IfStmt {
	fn get_parse_error_message() -> &'static str {
		"Expected an if statement."
	}
}

#[derive(Clone)]
pub struct IntegerLiteral {
	pub value: i64,
}

impl AstNode for IntegerLiteral {
	fn get_parse_error_message() -> &'static str {
		"Expected an integer literal."
	}
}

#[derive(Clone)]
pub struct StringLiteral {
	pub string: Token,
}

impl StringLiteral {
	pub fn to_str(&self, lexer: &Lexer) -> String {
		String::from("'") + self.string.to_str(lexer) + "'"
	}
}

impl AstNode for StringLiteral {
	fn get_parse_error_message() -> &'static str {
		"Expected a string literal."
	}
}

// IMPORTANT: Do not re-order the binding powers. They form an ordered set.
#[derive(Clone, Copy, Eq, PartialEq, PartialOrd)]
enum BindingPower {
	Default,
	Assignment,
	LogicalOr,
	LogicalAnd,
	Equality,
	Relational,
	Additive,
	Multiplicative,
	Unary,
	Exponential,
	Call,
	Grouping,
	Literal,
}

pub struct ParseError<'a> {
	message: &'a str,
	source: String,
	token: Token,
}

impl Display for ParseError<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let bad_token = &self.token;

		let lines_around = 2;
		let max_width = 80;

		// Get the lines to display as an iterator.
		let line_above = if self.token.line > lines_around { self.token.line - lines_around } else { 1 };
		let lines_above = std::cmp::min(self.token.line - 1, lines_around);
		let num_lines = lines_above + lines_around + 1;
		let lines_iter = self.source.lines().enumerate().skip(line_above - 1).take(num_lines);
		let max_line_num = lines_iter.clone().last().map_or(1, |(i, _)| /*self.token.line +*/ i + 1 /*- lines_above*/);
		let line_num_padding: usize = (max_line_num.checked_ilog10().unwrap_or(0) + 1).try_into().unwrap();

		let bad_token_line = bad_token.line - line_above + 1;
		let bad_token_column = bad_token.column;

		let lines: Vec<String> = lines_iter.map(|(_, line)| {
			let max_w = max_width - line_num_padding - 1;
			let mut l = line.to_string();

			if l.len() > max_w {
				l.truncate(max_w);
			}

			l
		}).collect();

		// Get all of the tokens in the trimmed lines.
		let mut trimmed_source = lines.iter().fold(String::new(), |a, b| a + b + "\n");
		trimmed_source.truncate(trimmed_source.len() - 1);
		let mut lexer = Lexer::new(trimmed_source.clone());
		lexer.emit_comments = true;

		// Collect all of the slice and formatting information.
		type FormatterFn = fn(&str) -> ColoredString;
		let comment_formatter: FormatterFn = |s| s.green();
		let identifier_formatter: FormatterFn = |s| s.cyan();
		let keyword_formatter: FormatterFn = |s| s.blue();
		let numeric_formatter: FormatterFn = |s| s.yellow();
		let punctuation_formatter: FormatterFn = |s| s.white();
		let string_formatter: FormatterFn = |s| s.magenta();

		let mut format_info: Vec<(usize, usize, ColoredString)> = Vec::new();

		while !lexer.end_of_file() {
			let token = lexer.next();
			let src = &lexer.source[token.start..token.end];

			let mut formatted = match token.token_type {
				TOKEN_COMMENT => comment_formatter(src),
				TOKEN_IDENTIFIER
				| TOKEN_SYM_IDENTIFIER_DELIMITER => identifier_formatter(src),
				TOKEN_KW_AND
				| TOKEN_KW_ELSE
				| TOKEN_KW_FALSE
				| TOKEN_KW_FOR
				| TOKEN_KW_IF
				| TOKEN_KW_NOT
				| TOKEN_KW_NULL
				| TOKEN_KW_OR
				| TOKEN_KW_RETURN
				| TOKEN_KW_TRUE
				| TOKEN_KW_VAR
				| TOKEN_KW_WHERE => keyword_formatter(src),
				TOKEN_LIT_DECIMAL
				| TOKEN_LIT_HEXADECIMAL
				| TOKEN_LIT_INTEGER
				| TOKEN_LIT_OCTAL => numeric_formatter(src),
				TOKEN_OP_ASSIGNMENT
				| TOKEN_OP_ADDITION
				| TOKEN_OP_ADDITION_ASSIGNMENT
				| TOKEN_OP_DIVISION
				| TOKEN_OP_DIVISION_ASSIGNMENT
				| TOKEN_OP_EQUAL
				| TOKEN_OP_GREATER_THAN
				| TOKEN_OP_GREATER_THAN_OR_EQUAL
				| TOKEN_OP_LESS_THAN
				| TOKEN_OP_LESS_THAN_OR_EQUAL
				| TOKEN_OP_MODULUS
				| TOKEN_OP_MODULUS_ASSIGNMENT
				| TOKEN_OP_MULTIPLICATION
				| TOKEN_OP_MULTIPLICATION_ASSIGNMENT
				| TOKEN_OP_NOT_EQUAL
				| TOKEN_OP_SUBTRACTION
				| TOKEN_OP_SUBTRACTION_ASSIGNMENT
				| TOKEN_SYM_ARRAY_END
				| TOKEN_SYM_ARRAY_START
				| TOKEN_SYM_BLOCK_END
				| TOKEN_SYM_BLOCK_START
				| TOKEN_SYM_DECIMAL_SEPARATOR
				| TOKEN_SYM_EXCLAMATION
				| TOKEN_SYM_GROUPING_END
				| TOKEN_SYM_GROUPING_START
				| TOKEN_SYM_LIST_SEPARATOR
				| TOKEN_SYM_STRING_INTERPOLATION_END
				| TOKEN_SYM_STRING_INTERPOLATION_START
				| TOKEN_SYM_TERMINATOR
				| TOKEN_SYM_TYPE_SEPARATOR => punctuation_formatter(src),
				TOKEN_LIT_STRING
				| TOKEN_SYM_STRING_DELIMITER => string_formatter(src),
				TOKEN_END_OF_FILE => break,
			};

			if token.line == bad_token_line && token.column == bad_token_column {
				formatted = src.red().bold().underline();
			}

			format_info.push((token.start, token.end, formatted));
		}

		// Loop through the formatting information backwards so that the indices are valid when applied.
		for info in format_info.iter().rev() {
			let formatted_str = info.2.to_string();
			trimmed_source.replace_range(info.0..info.1, &formatted_str);
		}

		// We now split the formatted source code again so that we can prepend line numbers and limit the line lengths.
		let lines_iter = trimmed_source.lines().enumerate();

		for (i, line) in lines_iter {
			let line_num = self.token.line + i - lines_above;
			let line_num_str = format!("{:>1$}", line_num, line_num_padding).black().on_white();

			writeln!(f, "{} {}", line_num_str, line)?;
		}

		write!(f, "\n{}", self.message)
	}
}

pub struct Parser {
	pub lexer: Lexer,
}

impl Parser {
	pub fn new_from_source(source: &str) -> Self {
		Self {
			lexer: Lexer::new(source),
		}
	}

	pub fn new_with_lexer(lexer: Lexer) -> Self {
		Self {
			lexer: lexer,
		}
	}

	pub fn parse(&mut self) -> Vec<Ast> {
		let mut ast: Vec<Ast> = Vec::new();

		while !self.lexer.end_of_file() {
			let statement = self.parse_statement(BindingPower::Default);

			if let Some(s) = statement {
				ast.push(s);
			}
		}

		ast
	}

	fn parse_statement(&mut self, binding_power: BindingPower) -> Option<Ast> {
		let ast = parse_expression(self, binding_power);

		let ast = if let Some(ast) = ast {
			ast
		}
		else {
			return None;
		};

		let is_block = match ast {
			Ast::Block(_) | Ast::ForLoop(_) | Ast::If(_) => true,
			_ => false,
		};

		if !is_block {
			let token = self.lexer.next();

			if token.token_type != TOKEN_SYM_TERMINATOR {
				throw_parse_error(self, "Expected `;`.");
			}
		}

		Some(ast)
	}
}

fn get_binding_power(token_type: &TokenType) -> Option<BindingPower> {
	match token_type {
		TOKEN_KW_AND => Some(BindingPower::LogicalAnd),
		TOKEN_KW_OR => Some(BindingPower::LogicalOr),
		TOKEN_OP_ADDITION => Some(BindingPower::Additive),
		TOKEN_OP_ADDITION_ASSIGNMENT => Some(BindingPower::Assignment),
		TOKEN_OP_ASSIGNMENT => Some(BindingPower::Assignment),
		TOKEN_OP_DIVISION => Some(BindingPower::Multiplicative),
		TOKEN_OP_DIVISION_ASSIGNMENT => Some(BindingPower::Assignment),
		TOKEN_OP_GREATER_THAN => Some(BindingPower::Relational),
		TOKEN_OP_GREATER_THAN_OR_EQUAL => Some(BindingPower::Relational),
		TOKEN_OP_LESS_THAN => Some(BindingPower::Relational),
		TOKEN_OP_LESS_THAN_OR_EQUAL => Some(BindingPower::Relational),
		TOKEN_OP_MODULUS_ASSIGNMENT => Some(BindingPower::Assignment),
		TOKEN_OP_MULTIPLICATION => Some(BindingPower::Multiplicative),
		TOKEN_OP_MULTIPLICATION_ASSIGNMENT => Some(BindingPower::Assignment),
		TOKEN_OP_SUBTRACTION => Some(BindingPower::Additive),
		TOKEN_OP_SUBTRACTION_ASSIGNMENT => Some(BindingPower::Assignment),
		TOKEN_SYM_TERMINATOR => Some(BindingPower::Default),
		_ => None,
	}
}

fn get_expression_led_handler(token_type: &TokenType) -> Option<LedFunction> {
	match token_type {
		TOKEN_OP_ADDITION => Some(parse_binary_expression),
		TOKEN_OP_ADDITION_ASSIGNMENT => Some(parse_assignment),
		TOKEN_OP_ASSIGNMENT => Some(parse_assignment),
		TOKEN_OP_DIVISION => Some(parse_binary_expression),
		TOKEN_OP_DIVISION_ASSIGNMENT => Some(parse_assignment),
		TOKEN_OP_GREATER_THAN => Some(parse_binary_expression),
		TOKEN_OP_GREATER_THAN_OR_EQUAL => Some(parse_binary_expression),
		TOKEN_OP_LESS_THAN => Some(parse_binary_expression),
		TOKEN_OP_LESS_THAN_OR_EQUAL => Some(parse_binary_expression),
		TOKEN_OP_MODULUS => Some(parse_assignment),
		TOKEN_OP_MODULUS_ASSIGNMENT => Some(parse_assignment),
		TOKEN_OP_MULTIPLICATION => Some(parse_binary_expression),
		TOKEN_OP_MULTIPLICATION_ASSIGNMENT => Some(parse_assignment),
		TOKEN_OP_SUBTRACTION => Some(parse_binary_expression),
		TOKEN_OP_SUBTRACTION_ASSIGNMENT => Some(parse_assignment),
		_ => None,
	}
}

fn get_expression_nud_handler(token_type: &TokenType) -> Option<NudFunction> {
	match token_type {
		TOKEN_IDENTIFIER => Some(parse_literal),
		TOKEN_KW_FALSE => Some(parse_literal),
		TOKEN_KW_FOR => Some(parse_for_loop),
		TOKEN_KW_IF => Some(parse_if_statement),
		TOKEN_KW_TRUE => Some(parse_literal),
		TOKEN_KW_VAR => Some(parse_declaration),
		TOKEN_LIT_INTEGER => Some(parse_literal),
		TOKEN_SYM_ARRAY_START => Some(parse_block_statement),
		TOKEN_SYM_IDENTIFIER_DELIMITER => Some(parse_quoted_identifier),
		TOKEN_SYM_GROUPING_START => Some(parse_grouping_expression),
		TOKEN_SYM_STRING_DELIMITER => Some(parse_string),
		_ => None,
	}
}

#[inline]
fn is_assignment_operator(token_type: &TokenType) -> bool {
	*token_type == TOKEN_OP_ADDITION_ASSIGNMENT
		|| *token_type == TOKEN_OP_ASSIGNMENT
		|| *token_type == TOKEN_OP_DIVISION_ASSIGNMENT
		|| *token_type == TOKEN_OP_MODULUS_ASSIGNMENT
		|| *token_type == TOKEN_OP_MULTIPLICATION_ASSIGNMENT
		|| *token_type == TOKEN_OP_SUBTRACTION_ASSIGNMENT
}

fn parse_assignment(parser: &mut Parser, left: Ast, _: BindingPower) -> Ast {
	match left {
		Ast::Identifier(_) => (),
		_ => throw_parse_error(parser, "Expected an identifier."),
	}

	let operator = parser.lexer.next();

	if !is_assignment_operator(&operator.token_type) {
		throw_parse_error(parser, "Expected an assignment operator.");
	}

	let rhs = parse_expression(parser, BindingPower::Assignment);

	let rhs = match rhs {
		Some(rhs) => rhs,
		None => throw_parse_error(parser, "Expected an expression."),
	};

	Ast::Assignment(AssignmentStmt {
		lhs: Box::new(left),
		operator: operator,
		rhs: Box::new(rhs),
	})
}

fn parse_binary_expression(parser: &mut Parser, left: Ast, binding_power: BindingPower) -> Ast {
	let operator = parser.lexer.next();
	let right = parse_expression(parser, binding_power).unwrap();

	Ast::Binary(BinaryExpr {
		left: Box::new(left),
		operator: operator,
		right: Box::new(right),
	})
}

fn parse_block_statement(parser: &mut Parser) -> Ast {
	let token = parser.lexer.next();
	if token.token_type != TOKEN_SYM_BLOCK_START {
		throw_parse_error(parser, "Expected a `{`.");
	}

	let mut body: Vec<Ast> = Vec::new();
	let mut token = parser.lexer.peek();
	while !parser.lexer.end_of_file() && token.token_type != TOKEN_SYM_BLOCK_END {
		let statement = parser.parse_statement(BindingPower::Default);

		if let Some(s) = statement {
			body.push(s);
		}

		token = parser.lexer.peek();
	}

	let token = parser.lexer.next();
	if token.token_type != TOKEN_SYM_BLOCK_END {
		throw_parse_error(parser, "Expected a `}`.");
	}

	Ast::Block(BlockStmt {
		body: body
	})
}

fn parse_declaration(parser: &mut Parser) -> Ast {
	let token = parser.lexer.next();

	if token.token_type != TOKEN_KW_VAR {
		throw_parse_error(parser, "Expected keyword `var`.");
	}

	let variable = match parse_expression(parser, BindingPower::Default) {
		Some(Ast::Assignment(e)) => Ast::Assignment(e),
		Some(Ast::Identifier(e)) => Ast::Identifier(e),
		_ => throw_parse_error(parser, "Expected an identifier or assignment."),
	};

	Ast::Declaration(DeclarationStmt {
		variable: Box::new(variable),
	})
}

fn parse_expression(parser: &mut Parser, binding_power: BindingPower) -> Option<Ast> {
	pratt(parser, binding_power, get_expression_nud_handler, get_expression_led_handler)
}

fn parse_for_loop(parser: &mut Parser) -> Ast {
	let token = parser.lexer.next();
	if token.token_type != TOKEN_KW_FOR {
		throw_parse_error(parser, "Expected keyword `for`.");
	}

	todo!();
}

fn parse_grouping_expression(parser: &mut Parser) -> Ast {
	let token = parser.lexer.peek();

	if token.token_type != TOKEN_SYM_GROUPING_START {
		panic!("Unexpected token.");
	}

	parser.lexer.next();

	let expression = parse_expression(parser, BindingPower::Default).unwrap();
	let token = parser.lexer.peek();

	if token.token_type != TOKEN_SYM_GROUPING_END {
		panic!("Unexpected token.");
	}

	parser.lexer.next();

	expression
}

fn parse_if_statement(parser: &mut Parser) -> Ast {
	let token = parser.lexer.next();
	if token.token_type != TOKEN_KW_IF {
		throw_parse_error(parser, "Expected keyword `if`.");
	}

	let condition = parse_expression(parser, BindingPower::LogicalOr);

	if condition.is_none() {
		throw_parse_error(parser, "Expected an expression.");
	}

	let if_arm = parser.parse_statement(BindingPower::Default);
	if if_arm.is_none() {
		throw_parse_error(parser, "Expected a statement or block.");
	}

	let token = parser.lexer.peek();
	let else_arm = if token.token_type == TOKEN_KW_ELSE {
		parser.lexer.next();
		if let Some(s) = parser.parse_statement(BindingPower::Default) {
			Some(Box::new(s))
		}
		else { None }
	}
	else { None };

	Ast::If(IfStmt {
		condition: Box::new(condition.unwrap()),
		else_arm: else_arm,
		if_arm: Box::new(if_arm.unwrap()),
	})
}

fn parse_literal(parser: &mut Parser) -> Ast {
	let token = parser.lexer.peek();

	let ast = match token.token_type {
		TOKEN_IDENTIFIER => Ast::Identifier(Identifier {
			name: token,
			is_quoted: false,
		}),
		TOKEN_KW_FALSE => Ast::Boolean(BooleanLiteral {
			value: false,
		}),
		TOKEN_KW_TRUE => Ast::Boolean(BooleanLiteral {
			value: true,
		}),
		TOKEN_LIT_INTEGER => Ast::Integer(IntegerLiteral {
			value: token.to_str(&parser.lexer).parse().unwrap(),
		}),
		TOKEN_SYM_IDENTIFIER_DELIMITER => {
			return parse_quoted_identifier(parser);
		},
		_ => throw_parse_error(parser, "Expected a literal."),
	};
	parser.lexer.next();

	ast
}

fn parse_quoted_identifier(parser: &mut Parser) -> Ast {
	let token = parser.lexer.next();
	if token.token_type != TOKEN_SYM_IDENTIFIER_DELIMITER {
		throw_parse_error(parser, "Expected a `\"`.");
	}

	let token = parser.lexer.peek();
	if token.token_type != TOKEN_IDENTIFIER {
		throw_parse_error(parser, "Expected an identifier.");
	}

	let identifier = parser.lexer.next();

	let token = parser.lexer.next();
	if token.token_type != TOKEN_SYM_IDENTIFIER_DELIMITER {
		throw_parse_error(parser, "Expected a `\"`.");
	}

	Ast::Identifier(Identifier {
		name: identifier,
		is_quoted: true,
	})
}

fn parse_string(parser: &mut Parser) -> Ast {
	let token = parser.lexer.next();
	if token.token_type != TOKEN_SYM_STRING_DELIMITER {
		throw_parse_error(parser, "Expected a `'`.");
	}

	let string = parser.lexer.next();
	if string.token_type != TOKEN_LIT_STRING {
		throw_parse_error(parser, "Expected a string.");
	}

	let token = parser.lexer.next();
	if token.token_type != TOKEN_SYM_STRING_DELIMITER {
		throw_parse_error(parser, "Expected a `'`.");
	}

	Ast::String(StringLiteral {
		string: string,
	})
}

fn pratt(parser: &mut Parser, binding_power: BindingPower, get_nud_handler: fn(token_type: &TokenType) -> Option<NudFunction>, get_led_handler: fn(token_type: &TokenType) -> Option<LedFunction>) -> Option<Ast> {
	// Parse the current token as a Null Denotation.
	let token = parser.lexer.peek();

	if token.token_type == TOKEN_END_OF_FILE {
		return None;
	}

	let nud_handler = get_nud_handler(&token.token_type);

	if nud_handler.is_none() {
		if token.token_type == TOKEN_END_OF_FILE {
			parser.lexer.next();
			return None;
		}

		let msg = format!("Unexpected token: `{}`.", token.to_str(&parser.lexer));
		parser.lexer.next();
		throw_parse_error(parser, msg.as_str());
	}

	let nud_handler = nud_handler.unwrap();
	let mut left: Ast = nud_handler(parser);

	// While we have a Left Denotation and the binding power of the current
	// token is less than the provided binding power, continue to parse the
	// left-hand-side.
	let mut token = parser.lexer.peek();
	let mut cur_binding_power = get_binding_power(&token.token_type);

	while cur_binding_power.is_some_and(|bp| bp > binding_power) {
		let led_handler = get_led_handler(&token.token_type);

		if led_handler.is_none() {
			let msg = format!("Unexpected token: `{}`.", token.to_str(&parser.lexer));
			parser.lexer.next();
			throw_parse_error(parser, msg.as_str());
		}

		let led_handler = led_handler.unwrap();
		left = led_handler(parser, left, get_binding_power(&token.token_type).unwrap());

		token = parser.lexer.peek();
		cur_binding_power = get_binding_power(&token.token_type);
	}

	Some(left)
}

#[inline]
fn throw_parse_error(parser: &mut Parser, msg: &str) -> ! {
	parser.lexer.rewind(); // Need to go back to the invalid token.

	let message = msg.to_string();
	panic!("{}", ParseError {
		message: message.as_str(),
		source: parser.lexer.source.clone(),
		token: parser.lexer.peek(),
	});
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_addition() {
		let mut parser = Parser::new_from_source("a + 3 - 7;");
		let p = &mut parser;

		let ast = p.parse();

		let statement = ast.first().unwrap();

		let subtraction = statement.unwrap_ref::<BinaryExpr>(p);
		assert_eq!(subtraction.operator.token_type, TOKEN_OP_SUBTRACTION);

		let literal = subtraction.right.unwrap_ref::<IntegerLiteral>(p);
		assert_eq!(literal.value, 7);

		let addition = subtraction.left.unwrap_ref::<BinaryExpr>(p);
		assert_eq!(addition.operator.token_type, TOKEN_OP_ADDITION);

		let identifier = addition.left.unwrap_ref::<Identifier>(p);
		assert_eq!(identifier.name.to_str(&p.lexer), "a");

		let literal = addition.right.unwrap_ref::<IntegerLiteral>(p);
		assert_eq!(literal.value, 3);
	}

	#[test]
	fn test_parse_assignment() {
		let mut parser = Parser::new_from_source("
var x = true;
var y;
y = x;
		");
		let p = &mut parser;

		let mut ast = p.parse();
		let mut ast = ast.iter_mut();

		let statement = ast.next().unwrap();
		let declaration = statement.unwrap_ref::<DeclarationStmt>(p);

		let assignment = declaration.variable.as_ref().unwrap_ref::<AssignmentStmt>(p);
		let lhs = assignment.lhs.unwrap_ref::<Identifier>(p);
		let rhs = assignment.rhs.unwrap_ref::<BooleanLiteral>(p);
		assert_eq!(lhs.to_str(&p.lexer), "x");
		assert_eq!(assignment.operator.token_type, TOKEN_OP_ASSIGNMENT);
		assert_eq!(rhs.value, true);

		let statement = ast.next().unwrap();
		let declaration = statement.unwrap_ref::<DeclarationStmt>(p);

		let identifier = declaration.variable.as_ref().unwrap_ref::<Identifier>(p);
		assert_eq!(identifier.to_str(&p.lexer), "y");

		let statement = ast.next().unwrap();
		let assignment = statement.unwrap_ref::<AssignmentStmt>(p);
		let lhs = assignment.lhs.unwrap_ref::<Identifier>(p);
		let rhs = assignment.rhs.unwrap_ref::<Identifier>(p);
		assert_eq!(lhs.to_str(&p.lexer), "y");
		assert_eq!(assignment.operator.token_type, TOKEN_OP_ASSIGNMENT);
		assert_eq!(rhs.name.to_str(&p.lexer), "x");
	}
}
