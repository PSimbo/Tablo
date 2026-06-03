use std::path::Path;
use std::fmt::Display;

pub mod ast;
pub mod builtins;
pub mod bytecode;
pub mod compiler;
pub mod object_file;
pub mod semantic;
pub mod source;
pub mod syntax;
pub mod value;
pub mod vm;

use bytecode::Program;
use bytecode::SourceFileDebugInfo;
use compiler::CompileError;
use compiler::Compiler;
use object_file::ObjectFileError;
use object_file::read_program_from_path;
use object_file::write_program_to_path;
use source::SourceText;
use syntax::lexer::LexError;
use syntax::lexer::Lexer;
use syntax::parser::ParseError;
use syntax::parser::Parser;
use value::Value;
use vm::VirtualMachine;
use vm::VmError;

#[cfg_attr(not(test), allow(dead_code))]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CompilationTarget {
	Snippet,
	Standalone,
}

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

	if include_body_name {
		if let Some(body_name) = location.body_name() {
			return format!("{body_name} ({position})");
		}
	}

	position
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
	let program = compile_to_program_with_name(source, None)?;
	write_program_to_path(output_path, &program).map_err(TabloError::ObjectFile)
}

pub fn compile_with_source_name(
	source: impl Into<String>,
	source_name: impl Into<String>,
	output_path: impl AsRef<Path>
) -> Result<(), TabloError> {
	let source_name = source_name.into();
	let program = compile_to_program_with_name(source, Some(source_name.as_str()))?;
	write_program_to_path(output_path, &program).map_err(TabloError::ObjectFile)
}

pub fn run(source: impl Into<String>) -> Result<Option<Value>, TabloError> {
	let program = compile_to_program_with_name(source, None)?;
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

pub(crate) fn compile_to_program_with_name(source: impl Into<String>, source_name: Option<&str>) -> Result<Program, TabloError> {
	compile_source_to_program_with_name(source, source_name, CompilationTarget::Standalone)
}

fn attach_source_debug_info(program: &mut Program, source: &SourceText, source_name: Option<&str>) {
	let source_file = SourceFileDebugInfo::from_source(
		source_name.unwrap_or("<source>"),
		source,
	);
	program.debug_info_mut().attach_source_file(source_file);
}

fn compile_ast_program(program: &ast::Program, target: CompilationTarget) -> Result<Program, TabloError> {
	let mut compiler = Compiler::new();

	match target {
		CompilationTarget::Snippet => compiler.compile_program(program).map_err(TabloError::Compile),
		CompilationTarget::Standalone => compiler.compile_standalone_program(program).map_err(TabloError::Compile),
	}
}

fn compile_source_to_program_with_name(
	source: impl Into<String>,
	source_name: Option<&str>,
	target: CompilationTarget,
) -> Result<Program, TabloError> {
	let source = SourceText::new(source);
	let program = parse_source_text(&source)?;
	let mut program = compile_ast_program(&program, target)?;
	attach_source_debug_info(&mut program, &source, source_name);
	Ok(program)
}

fn parse_source_text(source: &SourceText) -> Result<ast::Program, TabloError> {
	let mut lexer = Lexer::new(source.clone());
	let tokens = lexer.tokenize().map_err(TabloError::Lex)?;
	let mut parser = Parser::new(tokens);
	parser.parse_program().map_err(TabloError::Parse)
}

#[cfg(test)]
mod tests {
	use std::path::PathBuf;

	use crate::bytecode::Instruction;
	use crate::object_file::read_program_from_path;
	use crate::object_file::write_program_to_path;
	use crate::value::Value;

	use super::compile_source_to_program_with_name;
	use super::compile;
	use super::compile_with_source_name;
	use super::run;
	use super::run_file;
	use super::run_program;
	use super::CompilationTarget;
	use super::TabloError;

	fn standalone_expression(expression: &str) -> String {
		format!("fn Main(args: [text]) int {{ return {expression}; }}")
	}

	fn standalone_body(body: &str) -> String {
		format!("fn Main(args: [text]) int {{\n{body}\n}}")
	}

	fn evaluate_snippet(source: &str) -> Result<Option<Value>, TabloError> {
		let program = compile_source_to_program_with_name(source, None, CompilationTarget::Snippet)?;
		run_program(&program)
	}

	fn compile_snippet_to_object_file(source: &str, output_path: &std::path::Path) -> Result<(), TabloError> {
		let program = compile_source_to_program_with_name(source, None, CompilationTarget::Snippet)?;
		write_program_to_path(output_path, &program).map_err(TabloError::ObjectFile)
	}

	#[test]
	fn compiles_source_text_to_object_file() {
		let output_path = unique_test_output_path("compiles_source_text_to_object_file");
		compile("fn Main(args: [text]) int { return 1 + 2; }", &output_path).unwrap();
		let program = read_program_from_path(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(program.entry.instructions, vec![
			Instruction::MakeArray(0),
			Instruction::Call(0, 1),
		]);
	}

	#[test]
	fn formats_array_index_type_error_with_line_and_column() {
		let source = standalone_body("var xs: [int] = [1, 2];\nreturn xs['1'];");
		let error = run(&source).unwrap_err();

		assert_eq!(
			error.format_with_source(&source),
			"Compile error at line 3, column 11: Array index must be of type `int`, found `text`.\n  |\n3 | return xs['1'];\n  |           ^"
		);
	}

	#[test]
	fn formats_break_outside_loop_compile_error_with_line_and_column() {
		let source = "break;\n";
		let error = evaluate_snippet(source).unwrap_err();

		assert_eq!(
			error.format_with_source(source),
			"Compile error at line 1, column 1: `break` may only be used inside a `while` or `for` loop.\n  |\n1 | break;\n  | ^"
		);
	}

	#[test]
	fn formats_compile_error_with_line_and_column() {
		let source = standalone_body("var x: int = true;\nreturn x;");
		let error = run(&source).unwrap_err();

		assert_eq!(
			error.format_with_source(&source),
			"Compile error at line 2, column 14: Cannot assign a value of type `bool` to a variable of type `int`.\n  |\n2 | var x: int = true;\n  |              ^"
		);
	}

	#[test]
	fn formats_if_condition_compile_error_with_line_and_column() {
		let source = standalone_body("if 1 {\n}\nreturn 0;");
		let error = run(&source).unwrap_err();

		assert_eq!(
			error.format_with_source(&source),
			"Compile error at line 2, column 4: `if` condition must be of type `bool`, found `int`.\n  |\n2 | if 1 {\n  |    ^"
		);
	}

	#[test]
	fn formats_missing_function_return_error_with_line_and_column() {
		let source = "fn Main(args: [text]) int { return add(1, 2); }\nfn add(a: int, b: int) int {\n  a + b;\n}";
		let error = run(source).unwrap_err();

		assert_eq!(
			error.format_with_source(source),
			"Compile error at line 2, column 1: Function `add` must return a value of type `int` on all paths.\n  |\n2 | fn add(a: int, b: int) int {\n  | ^"
		);
	}

	#[test]
	fn formats_runtime_stack_trace_with_function_names() {
		let source = "fn inner() int {\n  var xs: [int] = [1];\n  return xs[2];\n}\nfn outer() int {\n  return inner();\n}\nouter()";
		let error = evaluate_snippet(source).unwrap_err();

		assert_eq!(
			error.to_string(),
			"Runtime error: Array index 2 is out of bounds for length 1.\nStack trace:\n  at inner (<source>:3:12)\n  at outer (<source>:6:15)\n  at <source>:8:6"
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
	fn formats_while_condition_compile_error_with_line_and_column() {
		let source = standalone_body("while 1 {\n}\nreturn 0;");
		let error = run(&source).unwrap_err();

		assert_eq!(
			error.format_with_source(&source),
			"Compile error at line 2, column 7: `while` condition must be of type `bool`, found `int`.\n  |\n2 | while 1 {\n  |       ^"
		);
	}

	#[test]
	fn preserves_debug_metadata_in_object_file() {
		let output_path = unique_test_output_path("preserves_debug_metadata_in_object_file");
		compile_with_source_name("fn Main(args: [text]) int {\nvar x: int = 1;\nreturn x;\n}", "example.tablo", &output_path).unwrap();
		let program = read_program_from_path(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		let debug = program.debug_info();
		assert_eq!(debug.source_files().len(), 1);
		assert_eq!(debug.source_files()[0].display_name(), "example.tablo");
		assert_eq!(debug.code_bodies().len(), 2);
		assert_eq!(debug.code_bodies()[0].body_name(), Some("Main"));
		assert_eq!(debug.code_bodies()[0].locals().len(), 2);
		assert_eq!(debug.code_bodies()[0].locals()[1].name(), "x");
		assert_eq!(debug.code_bodies()[0].locals()[1].slot(), 1);
		assert_eq!(debug.code_bodies()[0].locals()[1].declared_type(), "int");
	}

	#[test]
	fn rejects_39_digit_decimal_source_text() {
		let error = evaluate_snippet("3.14159265358979323846264338327950288415").unwrap_err();

		assert_eq!(error, TabloError::Parse(crate::syntax::parser::ParseError {
			position: 0,
			message: String::from("Decimal literal `3.14159265358979323846264338327950288415` exceeds the supported precision."),
		}));
	}

	#[test]
	fn rejects_assignment_to_const_source_text() {
		let error = evaluate_snippet("const x: int = 5;\nx = 3").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Constant `x` cannot be assigned using `=`."),
			position: 20,
		}));
	}

	#[test]
	fn rejects_invalid_main_signature_source_text() {
		let error = run("fn Main() int { return 0; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Entry-point function `Main` must have the exact signature `fn Main(args: [text]) int`."),
			position: 0,
		}));
	}

	#[test]
	fn rejects_len_with_non_array_argument_source_text() {
		let error = evaluate_snippet("len(1)").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `len` does not accept an argument of type `int`."),
			position: 4,
		}));
	}

	#[test]
	fn rejects_missing_main_in_standalone_source_text() {
		let error = run("var x: int = 1;").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Standalone Tablo programs must define `fn Main(args: [text]) int`."),
			position: 0,
		}));
	}

	#[test]
	fn rejects_non_iterable_for_source_text() {
		let error = evaluate_snippet("for value in 1 {\n}\n").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("`for` iterable must be an array or range, found `int`."),
			position: 13,
		}));
	}

	#[test]
	fn rejects_non_numeric_range_source_text() {
		let error = evaluate_snippet("'a':1").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Range bounds must be numeric, found `text` and `int`."),
			position: 3,
		}));
	}

	#[test]
	fn rejects_out_of_bounds_array_index_source_text() {
		let error = evaluate_snippet("var xs: [int] = [10, 20];\nxs[3]").unwrap_err();

		assert_eq!(error, TabloError::Runtime(crate::vm::VmError {
			instruction_index: 6,
			message: String::from("Array index 3 is out of bounds for length 2."),
			source_location: Some(crate::bytecode::SourceLocation::new(
				None,
				3,
				Some(String::from("<source>")),
				2,
			)),
			stack_trace: vec![
				crate::vm::VmStackFrame {
					instruction_index: 6,
					source_location: Some(crate::bytecode::SourceLocation::new(
						None,
						3,
						Some(String::from("<source>")),
						2,
					)),
				},
			],
		}));
	}

	#[test]
	fn rejects_out_of_scope_variable_source_text() {
		let error = evaluate_snippet("{ var x: int = 1; }\nx").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Variable `x` is not declared in this scope."),
			position: 20,
		}));
	}

	#[test]
	fn rejects_top_level_code_when_main_is_present_source_text() {
		let error = run("fn Main(args: [text]) int { return 0; }\n1 + 2").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Top-level executable statements are not permitted when `Main` is defined."),
			position: 42,
		}));
	}

	#[test]
	fn rejects_wrong_type_in_assignment_source_text() {
		let error = evaluate_snippet("var x: int = 5;\nx = 'hello'").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot assign a value of type `text` to a variable of type `int`."),
			position: 18,
		}));
	}

	#[test]
	fn rejects_wrong_type_in_variable_initializer_source_text() {
		let error = evaluate_snippet("var x: int = true;\nx").unwrap_err();

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
		let result = evaluate_snippet("3.1415926535897932384626433832795028841").unwrap();

		assert_eq!(result, Some(Value::Decimal(
			crate::value::Decimal::from_literal("3.1415926535897932384626433832795028841").unwrap()
		)));
	}

	#[test]
	fn runs_array_concatenation_source_text() {
		let result = evaluate_snippet("[1, 2] + [3, 4]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(1),
			Value::Integer(2),
			Value::Integer(3),
			Value::Integer(4),
		])));
	}

	#[test]
	fn runs_array_equality_source_text() {
		let result = evaluate_snippet("[1, 2] == [1, 2]").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_array_index_source_text() {
		let result = evaluate_snippet("var xs: [int] = [10, 20, 30];\nxs[2]").unwrap();

		assert_eq!(result, Some(Value::Integer(20)));
	}

	#[test]
	fn runs_array_length_source_text() {
		let result = evaluate_snippet("var xs: [int] = [10, 20, 30];\nlen(xs)").unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_array_literal_source_text() {
		let result = evaluate_snippet("var xs: [int] = [1, 2, 3];\nxs").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(1),
			Value::Integer(2),
			Value::Integer(3),
		])));
	}

	#[test]
	fn runs_array_object_file() {
		let output_path = unique_test_output_path("runs_array_object_file");
		compile_snippet_to_object_file("var xs: [int] = [1, 2];\nxs[1]", &output_path).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_block_scope_with_shadowing() {
		let result = run(standalone_body("var x: int = 1;\n{\n  var x: int = 2;\n  x += 3;\n}\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_boolean_source_text() {
		let result = evaluate_snippet("true").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_break_and_continue_source_text() {
		let result = run(standalone_body(
			"var x: int = 0;\nwhile true {\n  x += 1;\n  if x < 3 {\n    continue;\n  }\n  break;\n}\nreturn x;"
		)).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_compound_array_concatenation_source_text() {
		let result = evaluate_snippet("var xs: [int] = [1, 2];\nxs += [3, 4];\nxs").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(1),
			Value::Integer(2),
			Value::Integer(3),
			Value::Integer(4),
		])));
	}

	#[test]
	fn runs_compound_decimal_assignment_source_text() {
		let result = evaluate_snippet("var x: dec = 8.0;\nx /= 2.0").unwrap();

		assert_eq!(result, Some(Value::Decimal(crate::value::Decimal::from_integer(4))));
	}

	#[test]
	fn runs_compound_integer_assignment_source_text() {
		let result = run(standalone_body("var x: int = 5;\nx += 3;\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(8)));
	}

	#[test]
	fn runs_const_source_text() {
		let result = run(standalone_body("const x: int = 5;\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(5)));
	}

	#[test]
	fn runs_compound_indexed_assignment_source_text() {
		let result = evaluate_snippet("var xs: [int] = [10, 20, 30];\nxs[2] += 5;\nxs").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(10),
			Value::Integer(25),
			Value::Integer(30),
		])));
	}

	#[test]
	fn runs_decimal_object_file() {
		let output_path = unique_test_output_path("runs_decimal_object_file");
		compile_snippet_to_object_file("1.25 + .5", &output_path).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Decimal(crate::value::Decimal::from_literal("1.75").unwrap())));
	}

	#[test]
	fn runs_decimal_range_source_text() {
		let result = evaluate_snippet("0.0:0.1:0.3").unwrap();

		assert_eq!(result, Some(Value::DecimalRange(crate::value::DecimalRange {
			start: crate::value::Decimal::from_literal("0.0").unwrap(),
			step: Some(crate::value::Decimal::from_literal("0.1").unwrap()),
			end: crate::value::Decimal::from_literal("0.3").unwrap(),
		})));
	}

	#[test]
	fn runs_decimal_source_text() {
		let result = evaluate_snippet("1.25 + .5").unwrap();

		assert_eq!(result, Some(Value::Decimal(crate::value::Decimal::from_literal("1.75").unwrap())));
	}

	#[test]
	fn runs_empty_array_literal_source_text() {
		let result = evaluate_snippet("var xs: [int] = [];\nxs").unwrap();

		assert_eq!(result, Some(Value::Array(vec![])));
	}

	#[test]
	fn runs_equality_source_text() {
		let result = evaluate_snippet("2 == 2.0").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_expression_statement_before_final_expression() {
		let result = run(standalone_body("var x: int = 5;\nx += 1;\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(6)));
	}

	#[test]
	fn runs_for_array_source_text() {
		let result = run(standalone_body("var total: int = 0;\nfor value in [1, 2, 3] {\n  total += value;\n}\nreturn total;")).unwrap();

		assert_eq!(result, Some(Value::Integer(6)));
	}

	#[test]
	fn runs_for_decimal_range_source_text() {
		let result = evaluate_snippet("var total: dec = 0.0;\nfor value in 0.0:0.5:1.0 {\n  total += value;\n}\ntotal").unwrap();

		assert_eq!(result, Some(Value::Decimal(
			crate::value::Decimal::from_literal("1.5").unwrap()
		)));
	}

	#[test]
	fn runs_for_integer_range_source_text() {
		let result = run(standalone_body("var total: int = 0;\nfor value in 1:3 {\n  total += value;\n}\nreturn total;")).unwrap();

		assert_eq!(result, Some(Value::Integer(6)));
	}

	#[test]
	fn runs_for_with_break_and_continue_source_text() {
		let result = run(standalone_body(
			"var total: int = 0;\nfor value in [1, 2, 3, 4] {\n  if value == 2 {\n    continue;\n  }\n  if value == 4 {\n    break;\n  }\n  total += value;\n}\nreturn total;"
		)).unwrap();

		assert_eq!(result, Some(Value::Integer(4)));
	}

	#[test]
	fn runs_function_call_source_text() {
		let result = run("fn Main(args: [text]) int { return add(1, 2); }\nfn add(a: int, b: int) int { return a + b; }").unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_function_object_file() {
		let output_path = unique_test_output_path("runs_function_object_file");
		compile("fn Main(args: [text]) int { return add(1, 2); }\nfn add(a: int, b: int) int { return a + b; }", &output_path).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_if_else_if_source_text() {
		let result = run(standalone_body("var x: int = 1;\nif false { x = 2; } else if true { x = 3; }\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_if_else_source_text() {
		let result = run(standalone_body("var x: int = 1;\nif false { x = 2; } else { x = 3; }\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_if_source_text() {
		let result = run(standalone_body("var x: int = 1;\nif true { x = 2; }\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_indexed_assignment_append_source_text() {
		let result = evaluate_snippet("var xs: [int] = [10, 20];\nxs[3] = 30;\nxs").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(10),
			Value::Integer(20),
			Value::Integer(30),
		])));
	}

	#[test]
	fn runs_indexed_assignment_source_text() {
		let result = evaluate_snippet("var xs: [int] = [10, 20, 30];\nxs[2] = 99;\nxs").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(10),
			Value::Integer(99),
			Value::Integer(30),
		])));
	}

	#[test]
	fn runs_integer_range_source_text() {
		let result = evaluate_snippet("0:10").unwrap();

		assert_eq!(result, Some(Value::IntegerRange(crate::value::IntegerRange {
			start: 0,
			step: None,
			end: 10,
		})));
	}

	#[test]
	fn runs_interpolated_string_source_text() {
		let result = evaluate_snippet("var name: text = 'world';\n'hello ${name}!'").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello world!"))));
	}

	#[test]
	fn runs_len_on_empty_array_literal_source_text() {
		let result = run(standalone_expression("len([])")).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_logical_source_text() {
		let result = evaluate_snippet("not false and true or false").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_logical_xor_source_text() {
		let result = evaluate_snippet("true or false xor true and false").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_main_entry_point_source_text() {
		let result = run("fn Main(args: [text]) int { return 7; }").unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_object_file() {
		let output_path = unique_test_output_path("runs_object_file");
		compile("fn Main(args: [text]) int { return 8 / 2; }", &output_path).unwrap();
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
		let result = evaluate_snippet("1 + 2 < 4").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_repeating_decimal_division() {
		let result = evaluate_snippet("2.0 / 3.0").unwrap();

		assert_eq!(result, Some(Value::Decimal(
			crate::value::Decimal::from_literal("0.6666666666666666666666666666666666667").unwrap()
		)));
	}

	#[test]
	fn runs_source_text() {
		let result = run(standalone_expression("1 + 2 + 3")).unwrap();

		assert_eq!(result, Some(Value::Integer(6)));
	}

	#[test]
	fn runs_source_text_with_comments() {
		let result = run(standalone_body(
			"var x: int = 1; // ignore this until the block comment starts /* still ignored */\n\
			 /* outer /* inner */ done */\n\
			 return x + 2;",
		)).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_source_text_with_variable_declarations() {
		let result = run(standalone_body("var x: int = 1;\nvar y: int = 2;\nreturn x + y;")).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_text_concatenation_source_text() {
		let result = evaluate_snippet("'hello ' + 42").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello 42"))));
	}

	#[test]
	fn runs_text_relational_source_text() {
		let result = evaluate_snippet("'apple' < 'banana'").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_text_source_text() {
		let result = evaluate_snippet("'hello'").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello"))));
	}

	#[test]
	fn runs_void_function_as_expression_statement() {
		let result = run("fn Main(args: [text]) int { var x: int = 1; bump(x); return x; }\nfn bump(value: int) void { return; }").unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_while_source_text() {
		let result = run(standalone_body("var x: int = 0;\nwhile x < 3 { x += 1; }\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_unary_negated_decimal_source_text() {
		let result = evaluate_snippet("-1.25").unwrap();

		assert_eq!(result, Some(Value::Decimal(crate::value::Decimal::from_literal("1.25").unwrap().negated())));
	}

	#[test]
	fn runs_unary_negated_integer_source_text() {
		let result = run(standalone_expression("-42")).unwrap();

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
