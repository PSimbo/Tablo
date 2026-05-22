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
	Lex(LexError),
	ObjectFile(ObjectFileError),
	Parse(ParseError),
	Runtime(VmError),
}

impl Display for TabloError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TabloError::Lex(error) => write!(f, "Lex error at byte {}: {}", error.position, error.message),
			TabloError::ObjectFile(error) => write!(f, "Object file error at byte {}: {}", error.offset, error.message),
			TabloError::Parse(error) => write!(f, "Parse error at byte {}: {}", error.position, error.message),
			TabloError::Runtime(error) => write!(f, "Runtime error at instruction {}: {}", error.instruction_index, error.message),
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
	let expression = parser.parse_expression().map_err(TabloError::Parse)?;

	Ok(Compiler::new().compile_expression(&expression))
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
	fn returns_lex_error_from_single_call_api() {
		let error = run("1 ? 2").unwrap_err();

		assert_eq!(error, TabloError::Lex(crate::syntax::lexer::LexError {
			position: 2,
			message: String::from("Unexpected character `?`."),
		}));
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
	fn runs_source_text() {
		let result = run("1 + 2 + 3").unwrap();

		assert_eq!(result, Some(Value::Integer(6)));
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
