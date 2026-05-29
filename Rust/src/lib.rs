use std::path::Path;
use std::fmt::Display;

pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod object_file;
pub mod source;
pub mod syntax;
pub mod value;
pub mod vm;

use bytecode::Program;
use compiler::CompileError;
use compiler::Compiler;
use object_file::read_program_from_path;
use object_file::ObjectFileError;
use object_file::write_program_to_path;
use source::SourceText;
use syntax::lexer::LexError;
use syntax::lexer::Lexer;
use syntax::parser::ParseError;
use syntax::parser::Parser;
use value::Value;
use vm::VirtualMachine;
use vm::VmError;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TabloError {
	Compile(CompileError),
	Lex(LexError),
	ObjectFile(ObjectFileError),
	Parse(ParseError),
	Runtime(VmError),
}

impl Display for TabloError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TabloError::Compile(error) => write!(f, "Compile error: {}", error.message),
			TabloError::Lex(error) => write!(f, "Lex error at byte {}: {}", error.position, error.message),
			TabloError::ObjectFile(error) => write!(f, "Object file error at byte {}: {}", error.offset, error.message),
			TabloError::Parse(error) => write!(f, "Parse error at byte {}: {}", error.position, error.message),
			TabloError::Runtime(error) => write!(f, "Runtime error at instruction {}: {}", error.instruction_index, error.message),
		}
	}
}

impl TabloError {
	pub fn format_with_source(&self, source: &str) -> String {
		let source = SourceText::new(source);

		match self {
			TabloError::Compile(error) => source.format_diagnostic("Compile error", error.position, &error.message),
			TabloError::Lex(error) => source.format_diagnostic("Lex error", error.position, &error.message),
			TabloError::Parse(error) => source.format_diagnostic("Parse error", error.position, &error.message),
			_ => self.to_string(),
		}
	}
}

pub fn compile(source: impl Into<String>, output_path: impl AsRef<Path>) -> Result<(), TabloError> {
	let program = compile_to_program(source)?;
	write_program_to_path(output_path, &program).map_err(TabloError::ObjectFile)
}

pub fn run(source: impl Into<String>) -> Result<Option<Value>, TabloError> {
	let program = compile_to_program(source)?;
	run_program(&program)
}

pub fn run_file(path: impl AsRef<Path>) -> Result<Option<Value>, TabloError> {
	let program = read_program_from_path(path).map_err(TabloError::ObjectFile)?;
	run_program(&program)
}

pub fn run_program(program: &Program) -> Result<Option<Value>, TabloError> {
	let mut vm = VirtualMachine::new();
	vm.run(&program).map_err(TabloError::Runtime)
}

fn compile_to_program(source: impl Into<String>) -> Result<Program, TabloError> {
	let source = SourceText::new(source);
	let mut lexer = Lexer::new(source);
	let tokens = lexer.tokenize().map_err(TabloError::Lex)?;
	let mut parser = Parser::new(tokens);
	let program = parser.parse_program().map_err(TabloError::Parse)?;

	Compiler::new().compile_program(&program).map_err(TabloError::Compile)
}

#[cfg(test)]
mod tests {
	use std::path::PathBuf;

	use crate::bytecode::Instruction;
	use crate::object_file::read_program_from_path;
	use crate::value::Value;

	use super::compile;
	use super::run;
	use super::run_file;
	use super::run_program;
	use super::TabloError;

	#[test]
	fn compiles_source_text_to_object_file() {
		let output_path = unique_test_output_path("compiles_source_text_to_object_file");
		compile("1 + 2", &output_path).unwrap();
		let program = read_program_from_path(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(program.instructions, vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::Add,
		]);
	}

	#[test]
	fn formats_compile_error_with_line_and_column() {
		let source = "var x: int = true;\nx";
		let error = run(source).unwrap_err();

		assert_eq!(
			error.format_with_source(source),
			"Compile error at line 1, column 14: Cannot assign a value of type `bool` to a variable of type `int`.\n  |\n1 | var x: int = true;\n  |              ^"
		);
	}

	#[test]
	fn formats_if_condition_compile_error_with_line_and_column() {
		let source = "if 1 {\n}\n";
		let error = run(source).unwrap_err();

		assert_eq!(
			error.format_with_source(source),
			"Compile error at line 1, column 4: `if` condition must be of type `bool`, found `int`.\n  |\n1 | if 1 {\n  |    ^"
		);
	}

	#[test]
	fn formats_source_error_with_line_and_column() {
		let source = "1 + 2\n?";
		let error = run(source).unwrap_err();

		assert_eq!(
			error.format_with_source(source),
			"Lex error at line 2, column 1: Unexpected character `?`.\n  |\n2 | ?\n  | ^"
		);
	}

	#[test]
	fn rejects_39_digit_decimal_source_text() {
		let error = run("3.14159265358979323846264338327950288415").unwrap_err();

		assert_eq!(error, TabloError::Parse(crate::syntax::parser::ParseError {
			position: 0,
			message: String::from("Decimal literal `3.14159265358979323846264338327950288415` exceeds the supported precision."),
		}));
	}

	#[test]
	fn rejects_assignment_to_const_source_text() {
		let error = run("const x: int = 5;\nx = 3").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Constant `x` cannot be assigned using `=`."),
			position: 20,
		}));
	}

	#[test]
	fn rejects_out_of_scope_variable_source_text() {
		let error = run("{ var x: int = 1; }\nx").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Variable `x` is not declared in this scope."),
			position: 20,
		}));
	}

	#[test]
	fn rejects_wrong_type_in_assignment_source_text() {
		let error = run("var x: int = 5;\nx = 'hello'").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot assign a value of type `text` to a variable of type `int`."),
			position: 18,
		}));
	}

	#[test]
	fn rejects_wrong_type_in_variable_initializer_source_text() {
		let error = run("var x: int = true;\nx").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot assign a value of type `bool` to a variable of type `int`."),
			position: 13,
		}));
	}

	#[test]
	fn returns_lex_error_from_single_call_api() {
		let error = run("1 ? 2").unwrap_err();

		assert_eq!(error, TabloError::Lex(crate::syntax::lexer::LexError {
			position: 2,
			message: String::from("Unexpected character `?`."),
		}));
	}

	#[test]
	fn runs_38_digit_decimal_literal() {
		let result = run("3.1415926535897932384626433832795028841").unwrap();

		assert_eq!(result, Some(Value::Decimal(
			crate::value::Decimal::from_literal("3.1415926535897932384626433832795028841").unwrap()
		)));
	}

	#[test]
	fn runs_boolean_source_text() {
		let result = run("true").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_compound_decimal_assignment_source_text() {
		let result = run("var x: dec = 8.0;\nx /= 2.0").unwrap();

		assert_eq!(result, Some(Value::Decimal(crate::value::Decimal::from_integer(4))));
	}

	#[test]
	fn runs_compound_integer_assignment_source_text() {
		let result = run("var x: int = 5;\nx += 3").unwrap();

		assert_eq!(result, Some(Value::Integer(8)));
	}

	#[test]
	fn runs_const_source_text() {
		let result = run("const x: int = 5;\nx").unwrap();

		assert_eq!(result, Some(Value::Integer(5)));
	}

	#[test]
	fn runs_block_scope_with_shadowing() {
		let result = run("var x: int = 1;\n{\n  var x: int = 2;\n  x += 3;\n}\nx").unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_decimal_object_file() {
		let output_path = unique_test_output_path("runs_decimal_object_file");
		compile("1.25 + .5", &output_path).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Decimal(crate::value::Decimal::from_literal("1.75").unwrap())));
	}

	#[test]
	fn runs_decimal_source_text() {
		let result = run("1.25 + .5").unwrap();

		assert_eq!(result, Some(Value::Decimal(crate::value::Decimal::from_literal("1.75").unwrap())));
	}

	#[test]
	fn runs_equality_source_text() {
		let result = run("2 == 2.0").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_if_else_if_source_text() {
		let result = run("var x: int = 1;\nif false { x = 2; } else if true { x = 3; }\nx").unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_if_else_source_text() {
		let result = run("var x: int = 1;\nif false { x = 2; } else { x = 3; }\nx").unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_if_source_text() {
		let result = run("var x: int = 1;\nif true { x = 2; }\nx").unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_expression_statement_before_final_expression() {
		let result = run("var x: int = 5;\nx += 1;\nx").unwrap();

		assert_eq!(result, Some(Value::Integer(6)));
	}

	#[test]
	fn runs_interpolated_string_source_text() {
		let result = run("var name: text = 'world';\n'hello ${name}!'").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello world!"))));
	}

	#[test]
	fn runs_logical_source_text() {
		let result = run("not false and true or false").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_logical_xor_source_text() {
		let result = run("true or false xor true and false").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_object_file() {
		let output_path = unique_test_output_path("runs_object_file");
		compile("8 / 2", &output_path).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(4)));
	}

	#[test]
	fn runs_program() {
		let program = crate::bytecode::Program::new(vec![
			Instruction::PushInteger(7),
			Instruction::PushInteger(5),
			Instruction::Subtract,
		]);

		let result = run_program(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_relational_source_text() {
		let result = run("1 + 2 < 4").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_repeating_decimal_division() {
		let result = run("2.0 / 3.0").unwrap();

		assert_eq!(result, Some(Value::Decimal(
			crate::value::Decimal::from_literal("0.6666666666666666666666666666666666667").unwrap()
		)));
	}

	#[test]
	fn runs_source_text() {
		let result = run("1 + 2 + 3").unwrap();

		assert_eq!(result, Some(Value::Integer(6)));
	}

	#[test]
	fn runs_source_text_with_variable_declarations() {
		let result = run("var x: int = 1;\nvar y: int = 2;\nx + y").unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_text_concatenation_source_text() {
		let result = run("'hello ' + 42").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello 42"))));
	}

	#[test]
	fn runs_text_relational_source_text() {
		let result = run("'apple' < 'banana'").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_text_source_text() {
		let result = run("'hello'").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello"))));
	}

	#[test]
	fn runs_unary_negated_decimal_source_text() {
		let result = run("-1.25").unwrap();

		assert_eq!(result, Some(Value::Decimal(crate::value::Decimal::from_literal("1.25").unwrap().negated())));
	}

	#[test]
	fn runs_unary_negated_integer_source_text() {
		let result = run("-42").unwrap();

		assert_eq!(result, Some(Value::Integer(-42)));
	}

	fn unique_test_output_path(test_name: &str) -> PathBuf {
		let mut path = std::env::temp_dir();
		let process_id = std::process::id();
		let nanos = std::time::SystemTime::now()
			.duration_since(std::time::UNIX_EPOCH)
			.unwrap()
			.as_nanos();

		path.push(format!("tablo_{test_name}_{process_id}_{nanos}.tbo"));
		path
	}
}
