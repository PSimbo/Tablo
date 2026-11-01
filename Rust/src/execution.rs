use std::fmt::Display;
use std::path::Path;

use crate::{bytecode, vm};
use crate::bytecode::Program;
use crate::database::RuntimeDatabaseConfig;
use crate::object_file::{ObjectFileError, read_program_from_path};
use crate::value::Value;
use crate::vm::{VirtualMachine, VmError};
#[cfg(feature = "compiler")]
use crate::{compiler::CompileError, frontend::decode_external_source_diagnostic, source::SourceText, syntax::lexer::LexError, syntax::parser::ParseError};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TabloError {
	#[cfg(feature = "compiler")]
	Compile(CompileError),
	#[cfg(feature = "compiler")]
	Lex(LexError),
	ObjectFile(ObjectFileError),
	#[cfg(feature = "compiler")]
	Parse(ParseError),
	Runtime(VmError),
}

#[cfg(feature = "compiler")]
impl TabloError {
	pub fn format_with_source(&self, source: &str) -> String {
		self.format_with_source_name(source, None)
	}

	pub fn format_with_source_name(&self, source: &str, source_name: Option<&str>) -> String {
		let source = SourceText::new(source);

		match self {
			#[cfg(feature = "compiler")]
			TabloError::Compile(error) => {
				if let Some((category, diagnostic_source_name, position, message)) = decode_external_source_diagnostic(&error.message) {
					if let Ok(external_source) = std::fs::read_to_string(&diagnostic_source_name) {
						return SourceText::new(external_source)
							.format_diagnostic_with_source_name(category, position, &message, Some(&diagnostic_source_name));
					}

					return format!("{category} in {}:{}: {}", diagnostic_source_name, position, message);
				}

				source.format_diagnostic_with_source_name("Compile error", error.position, &error.message, source_name)
			}
			#[cfg(feature = "compiler")]
			TabloError::Lex(error) => source.format_diagnostic_with_source_name("Lex error", error.position, &error.message, source_name),
			#[cfg(feature = "compiler")]
			TabloError::Parse(error) => source.format_diagnostic_with_source_name("Parse error", error.position, &error.message, source_name),
			_ => self.to_string(),
		}
	}
}

impl Display for TabloError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			#[cfg(feature = "compiler")]
			TabloError::Compile(error) => {
				if let Some((category, _, _, message)) = decode_external_source_diagnostic(&error.message) {
					write!(f, "{category}: {message}")
				}
				else {
					write!(f, "Compile error: {}", error.message)
				}
			}
			#[cfg(feature = "compiler")]
			TabloError::Lex(error) => write!(f, "Lex error at byte {}: {}", error.position, error.message),
			TabloError::ObjectFile(error) => write!(f, "Object file error at byte {}: {}", error.offset, error.message),
			#[cfg(feature = "compiler")]
			TabloError::Parse(error) => write!(f, "Parse error at byte {}: {}", error.position, error.message),
			TabloError::Runtime(error) => {
				write!(f, "Runtime error: {}", error.message)?;

				if !error.stack_trace.is_empty() {
					write!(f, "\nStack trace:")?;

					for frame in &error.stack_trace {
						write!(f, "\n  at {}", format_stack_frame(frame))?;
					}
				}
				else if let Some(location) = &error.source_location {
					write!(f, "\nStack trace:\n  at {}", format_source_location(location, true))?;
				}
				else {
					write!(f, "\nStack trace:\n  at instruction {}", error.instruction_index)?;
				}

				Ok(())
			}
		}
	}
}

pub fn run_file(path: impl AsRef<Path>) -> Result<Option<Value>, TabloError> {
	run_file_with_arguments(path, &[])
}

pub fn run_file_with_arguments(
	path: impl AsRef<Path>,
	arguments: &[String],
) -> Result<Option<Value>, TabloError> {
	let program = read_program_from_path(path).map_err(TabloError::ObjectFile)?;
	run_program_with_arguments(&program, arguments)
}

pub fn run_file_with_database_config(
	path: impl AsRef<Path>,
	database_config: RuntimeDatabaseConfig,
) -> Result<Option<Value>, TabloError> {
	run_file_with_database_config_and_arguments(path, database_config, &[])
}

pub fn run_file_with_database_config_and_arguments(
	path: impl AsRef<Path>,
	database_config: RuntimeDatabaseConfig,
	arguments: &[String],
) -> Result<Option<Value>, TabloError> {
	let program = read_program_from_path(path).map_err(TabloError::ObjectFile)?;
	run_program_with_database_config_and_arguments(&program, database_config, arguments)
}

pub fn run_program(program: &Program) -> Result<Option<Value>, TabloError> {
	run_program_with_arguments(program, &[])
}

pub fn run_program_with_arguments(
	program: &Program,
	arguments: &[String],
) -> Result<Option<Value>, TabloError> {
	let mut vm = VirtualMachine::new();
	vm.run_with_arguments(program, arguments).map_err(TabloError::Runtime)
}

pub fn run_program_with_database_config(
	program: &Program,
	database_config: RuntimeDatabaseConfig,
) -> Result<Option<Value>, TabloError> {
	run_program_with_database_config_and_arguments(program, database_config, &[])
}

pub fn run_program_with_database_config_and_arguments(
	program: &Program,
	database_config: RuntimeDatabaseConfig,
	arguments: &[String],
) -> Result<Option<Value>, TabloError> {
	let mut vm = VirtualMachine::with_database_config(database_config);
	vm.run_with_arguments(program, arguments).map_err(TabloError::Runtime)
}

fn format_stack_frame(frame: &vm::VmStackFrame) -> String {
	match &frame.source_location {
		Some(location) => format_source_location(location, true),
		None => format!("instruction {}", frame.instruction_index),
	}
}

fn format_source_location(location: &bytecode::SourceLocation, include_body_name: bool) -> String {
	let position = if let Some(display_name) = location.display_name() {
		format!("{display_name}:{}:{}", location.line(), location.column())
	}
	else {
		format!("line {}, column {}", location.line(), location.column())
	};

	if include_body_name
		&& let Some(body_name) = location.body_name() {
		return format!("{body_name} ({position})");
	}

	position
}

