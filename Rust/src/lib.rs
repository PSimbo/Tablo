use std::collections::BTreeSet;
use std::fmt::Display;
use std::path::Path;
use std::path::PathBuf;

pub mod ast;
pub mod builtins;
pub mod bytecode;
pub mod compiler;
pub mod debugger;
pub mod format_string;
pub mod object_file;
pub mod query;
pub mod runtime_config;
pub mod schema;
pub mod schema_fixture;
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
use schema::SchemaCatalog;
use source::SourceText;
use syntax::lexer::LexError;
use syntax::lexer::Lexer;
use syntax::parser::ParseError;
use syntax::parser::Parser;
use value::Value;
use vm::RuntimeDatabaseConfig;
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

	if include_body_name
		&& let Some(body_name) = location.body_name() {
		return format!("{body_name} ({position})");
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

pub fn compile_with_source_name_and_schema(
	source: impl Into<String>,
	source_name: impl Into<String>,
	output_path: impl AsRef<Path>,
	schema_catalog: &SchemaCatalog,
) -> Result<(), TabloError> {
	let source_name = source_name.into();
	let program = compile_source_to_program_with_name_and_schema(
		source,
		Some(source_name.as_str()),
		CompilationTarget::Standalone,
		Some(schema_catalog),
	)?;
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

pub fn run_file_with_database_config(
	path: impl AsRef<Path>,
	database_config: RuntimeDatabaseConfig,
) -> Result<Option<Value>, TabloError> {
	let program = read_program_from_path(path).map_err(TabloError::ObjectFile)?;
	run_program_with_database_config(&program, database_config)
}

pub fn run_program(program: &Program) -> Result<Option<Value>, TabloError> {
	let mut vm = VirtualMachine::new();
	vm.run(program).map_err(TabloError::Runtime)
}

pub fn run_program_with_database_config(
	program: &Program,
	database_config: RuntimeDatabaseConfig,
) -> Result<Option<Value>, TabloError> {
	let mut vm = VirtualMachine::with_database_config(database_config);
	vm.run(program).map_err(TabloError::Runtime)
}

pub fn run_with_database_config(
	source: impl Into<String>,
	database_config: RuntimeDatabaseConfig,
) -> Result<Option<Value>, TabloError> {
	let program = compile_to_program_with_name(source, None)?;
	run_program_with_database_config(&program, database_config)
}

pub(crate) fn compile_to_program_with_name(source: impl Into<String>, source_name: Option<&str>) -> Result<Program, TabloError> {
	compile_source_to_program_with_name_and_schema(source, source_name, CompilationTarget::Standalone, None)
}

fn attach_source_debug_info(program: &mut Program, source: &SourceText, source_name: Option<&str>) {
	let source_file = SourceFileDebugInfo::from_source(
		source_name.unwrap_or("<source>"),
		source,
	);
	program.debug_info_mut().attach_source_file(source_file);
}

fn canonical_module_key(module_path: &Path) -> PathBuf {
	std::fs::canonicalize(module_path).unwrap_or_else(|_| module_path.to_path_buf())
}

fn compile_ast_program_with_schema(
	program: &ast::Program,
	target: CompilationTarget,
	schema_catalog: Option<&SchemaCatalog>,
) -> Result<Program, TabloError> {
	let mut compiler = Compiler::new();

	match target {
		CompilationTarget::Snippet => compiler.compile_program_with_schema(program, schema_catalog).map_err(TabloError::Compile),
		CompilationTarget::Standalone => compiler.compile_standalone_program_with_schema(program, schema_catalog).map_err(TabloError::Compile),
	}
}

fn compile_source_to_program_with_name_and_schema(
	source: impl Into<String>,
	source_name: Option<&str>,
	target: CompilationTarget,
	schema_catalog: Option<&SchemaCatalog>,
) -> Result<Program, TabloError> {
	let source = SourceText::new(source);
	let program = parse_source_text(&source)?;
	validate_module_graph(&program, source_name).map_err(TabloError::Compile)?;
	let mut program = compile_ast_program_with_schema(&program, target, schema_catalog)?;
	attach_source_debug_info(&mut program, &source, source_name);
	Ok(program)
}

fn first_nested_use_position(program: &ast::Program) -> Option<usize> {
	for function in &program.functions {
		if let Some(position) = first_use_in_statements(&function.body.statements) {
			return Some(position);
		}
	}

	first_use_in_non_top_level_blocks(&program.statements)
}

fn first_use_in_non_top_level_blocks(statements: &[ast::Statement]) -> Option<usize> {
	for statement in statements {
		match statement {
			ast::Statement::Block(block) => {
				if let Some(position) = first_use_in_statements(&block.statements) {
					return Some(position);
				}
			}
			ast::Statement::For(for_statement) => {
				if let Some(position) = first_use_in_statements(&for_statement.body.statements) {
					return Some(position);
				}
			}
			ast::Statement::ForRecord(for_statement) => {
				if let Some(position) = first_use_in_statements(&for_statement.body.statements) {
					return Some(position);
				}
			}
			ast::Statement::FunctionDeclaration(function) => {
				if let Some(position) = first_use_in_statements(&function.body.statements) {
					return Some(position);
				}
			}
			ast::Statement::If(if_statement) => {
				if let Some(position) = first_use_in_statements(&if_statement.then_branch.statements) {
					return Some(position);
				}

				if let Some(else_branch) = &if_statement.else_branch
					&& let Some(position) = first_use_in_statement(else_branch) {
					return Some(position);
				}
			}
			ast::Statement::While(while_statement) => {
				if let Some(position) = first_use_in_statements(&while_statement.body.statements) {
					return Some(position);
				}
			}
			_ => {}
		}
	}

	None
}

fn first_use_in_statement(statement: &ast::Statement) -> Option<usize> {
	match statement {
		ast::Statement::Use(use_declaration) => Some(use_declaration.position),
		ast::Statement::Block(block) => first_use_in_statements(&block.statements),
		ast::Statement::For(for_statement) => first_use_in_statements(&for_statement.body.statements),
		ast::Statement::ForRecord(for_statement) => first_use_in_statements(&for_statement.body.statements),
		ast::Statement::FunctionDeclaration(function) => first_use_in_statements(&function.body.statements),
		ast::Statement::If(if_statement) => {
			first_use_in_statements(&if_statement.then_branch.statements)
				.or_else(|| if_statement.else_branch.as_ref().and_then(|else_branch| first_use_in_statement(else_branch)))
		}
		ast::Statement::While(while_statement) => first_use_in_statements(&while_statement.body.statements),
		_ => None,
	}
}

fn first_use_in_statements(statements: &[ast::Statement]) -> Option<usize> {
	for statement in statements {
		if let Some(position) = first_use_in_statement(statement) {
			return Some(position);
		}
	}

	None
}

fn first_use_statement(program: &ast::Program) -> Option<&ast::UseDeclaration> {
	for statement in &program.statements {
		if let ast::Statement::Use(use_declaration) = statement {
			return Some(use_declaration);
		}
	}

	None
}

fn parse_source_text(source: &SourceText) -> Result<ast::Program, TabloError> {
	let mut lexer = Lexer::new(source.clone());
	let tokens = lexer.tokenize().map_err(TabloError::Lex)?;
	let mut parser = Parser::new(tokens);
	parser.parse_program().map_err(TabloError::Parse)
}

fn resolve_module_path(base_directory: &Path, module_path: &str) -> PathBuf {
	let mut resolved = base_directory.join(module_path);

	if resolved.extension().is_none() {
		resolved.set_extension("tablo");
	}

	resolved
}

fn validate_module_graph(program: &ast::Program, source_name: Option<&str>) -> Result<(), CompileError> {
	if let Some(position) = first_nested_use_position(program) {
		return Err(CompileError {
			message: String::from("Nested `use` declarations are not yet supported during module resolution."),
			position,
		});
	}

	let first_use = first_use_statement(program);
	let Some(first_use) = first_use else {
		return Ok(());
	};
	let Some(source_name) = source_name else {
		return Err(CompileError {
			message: String::from("Module imports require a source file path so relative `use` statements can be resolved."),
			position: first_use.position,
		});
	};

	let root_path = Path::new(source_name);
	let root_directory = root_path.parent().unwrap_or_else(|| Path::new("."));
	let mut visited = BTreeSet::new();
	validate_module_imports_in_program(program, root_directory, &mut visited)
}

fn validate_module_imports_in_program(
	program: &ast::Program,
	base_directory: &Path,
	visited: &mut BTreeSet<PathBuf>,
) -> Result<(), CompileError> {
	for statement in &program.statements {
		let ast::Statement::Use(use_declaration) = statement else {
			continue;
		};
		let module_path = resolve_module_path(base_directory, &use_declaration.module_path);
		let module_key = canonical_module_key(&module_path);

		if visited.contains(&module_key) {
			continue;
		}

		let source = std::fs::read_to_string(&module_path).map_err(|error| CompileError {
			message: format!(
				"Failed to read imported module `{}` from `{}`: {}",
				use_declaration.module_path,
				module_path.display(),
				error,
			),
			position: use_declaration.position,
		})?;
		let imported_program = parse_source_text(&SourceText::new(source)).map_err(|error| CompileError {
			message: format!(
				"Failed to parse imported module `{}` from `{}`: {}",
				use_declaration.module_path,
				module_path.display(),
				error,
			),
			position: use_declaration.position,
		})?;
		let exported_functions = imported_program.functions.iter()
			.filter(|function| function.visibility == ast::Visibility::Public)
			.map(|function| function.name.as_str())
			.collect::<BTreeSet<_>>();

		if let Some(imported_names) = &use_declaration.imported_names {
			for imported_name in imported_names {
				if !exported_functions.contains(imported_name.name.as_str()) {
					return Err(CompileError {
						message: format!(
							"Function `{}` is not exported by module `{}`.",
							imported_name.name,
							use_declaration.module_path,
						),
						position: imported_name.position,
					});
				}
			}
		}

		visited.insert(module_key);
		let next_base_directory = module_path.parent().unwrap_or_else(|| Path::new("."));
		validate_module_imports_in_program(&imported_program, next_base_directory, visited)?;
	}

	Ok(())
}

#[cfg(test)]
mod tests {
	use std::fs;
	use std::path::PathBuf;

	use rusqlite::Connection;

	use crate::bytecode::Instruction;
	use crate::object_file::read_program_from_path;
	use crate::object_file::write_program_to_path;
	use crate::schema::DatabaseBackend;
	use crate::schema::SchemaCatalog;
	use crate::schema_fixture::read_schema_catalog_from_str;
	use crate::value::Value;
	use crate::vm::RuntimeDatabaseConfig;

	use super::CompilationTarget;
	use super::TabloError;
	use super::compile;
	use super::compile_source_to_program_with_name_and_schema;
	use super::compile_with_source_name;
	use super::run;
	use super::run_file;
	use super::run_program;
	use super::run_program_with_database_config;

	fn compile_snippet_to_object_file(source: &str, output_path: &std::path::Path) -> Result<(), TabloError> {
		let program = compile_source_to_program_with_name_and_schema(source, None, CompilationTarget::Snippet, None)?;
		write_program_to_path(output_path, &program).map_err(TabloError::ObjectFile)
	}

	fn compile_snippet_with_schema_fixture(source: &str, schema_fixture: &str) -> Result<(crate::bytecode::Program, SchemaCatalog), TabloError> {
		compile_snippet_with_schema_fixture_and_backends(source, schema_fixture, &[])
	}

	fn compile_snippet_with_schema_fixture_and_backends(
		source: &str,
		schema_fixture: &str,
		backends: &[(&str, DatabaseBackend)],
	) -> Result<(crate::bytecode::Program, SchemaCatalog), TabloError> {
		let schema = schema_catalog_from_fixture_with_backends(schema_fixture, backends)?;
		let program = compile_source_to_program_with_name_and_schema(source, None, CompilationTarget::Snippet, Some(&schema))?;
		Ok((program, schema))
	}

	fn compile_standalone_with_schema_fixture_and_backends(
		source: &str,
		schema_fixture: &str,
		backends: &[(&str, DatabaseBackend)],
	) -> Result<(crate::bytecode::Program, SchemaCatalog), TabloError> {
		let schema = schema_catalog_from_fixture_with_backends(schema_fixture, backends)?;
		let program = compile_source_to_program_with_name_and_schema(source, None, CompilationTarget::Standalone, Some(&schema))?;
		Ok((program, schema))
	}

	fn create_sqlite_test_database(name: &str, setup_sql: &str) -> PathBuf {
		let path = unique_test_output_path(name).with_extension("sqlite");
		let connection = Connection::open(&path).unwrap();
		connection.execute_batch(setup_sql).unwrap();
		path
	}

	fn evaluate_snippet(source: &str) -> Result<Option<Value>, TabloError> {
		let program = compile_source_to_program_with_name_and_schema(source, None, CompilationTarget::Snippet, None)?;
		run_program(&program)
	}

	fn schema_catalog_from_fixture_with_backends(
		schema_fixture: &str,
		backends: &[(&str, DatabaseBackend)],
	) -> Result<SchemaCatalog, TabloError> {
		let schema = read_schema_catalog_from_str(schema_fixture)
			.map_err(|error| TabloError::Compile(crate::compiler::CompileError {
				message: error.message,
				position: 0,
			}))?;

		let mut schema = schema;
		for (database_name, backend) in backends {
			let database = schema.database_mut(database_name).ok_or_else(|| {
				TabloError::Compile(crate::compiler::CompileError {
					message: format!("Test schema does not define database `{database_name}`."),
					position: 0,
				})
			})?;
			database.set_backend(*backend);
		}

		Ok(schema)
	}

	fn standalone_body(body: &str) -> String {
		format!("fn Main(args: [text]) int {{\n{body}\n}}")
	}

	fn standalone_expression(expression: &str) -> String {
		format!("fn Main(args: [text]) int {{ return {expression}; }}")
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

	fn write_test_source_file(test_name: &str, file_name: &str, source: &str) -> PathBuf {
		let mut directory = std::env::temp_dir();
		let process_id = std::process::id();
		let nanos = std::time::SystemTime::now()
			.duration_since(std::time::UNIX_EPOCH)
			.unwrap()
			.as_nanos();

		directory.push(format!("tablo_{test_name}_{process_id}_{nanos}"));
		fs::create_dir_all(&directory).unwrap();

		let path = directory.join(file_name);
		fs::write(&path, source).unwrap();
		path
	}

	#[test]
	fn compiles_source_text_to_object_file() {
		let output_path = unique_test_output_path("compiles_source_text_to_object_file");
		compile("fn Main(args: [text]) int { return 1 + 2; }", &output_path).unwrap();
		let program = read_program_from_path(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(program.entry_function_index(), Some(0));
	}

	#[test]
	fn compiles_snippet_with_schema_fixture_for_future_schema_aware_tests() {
		let (program, schema) = compile_snippet_with_schema_fixture(
			"1 + 2",
			r#"
				database ExampleDb;
				schema Public;
				create table Customers (
					Id int not null
				);
			"#,
		).unwrap();

		assert_eq!(run_program(&program).unwrap(), Some(Value::Integer(3)));
		assert!(schema.database("exampledb").is_some());
	}

	#[test]
	fn creates_sqlite_record_from_new_record_pointer() {
		let database_path = create_sqlite_test_database(
			"creates_sqlite_record_from_new_record_pointer",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int {\n    rec mut cust = new Customers;\n    cust.Id = 7;\n    cust.Name = 'Ada';\n    create cust;\n    return count Customers where Id == 7 and Name == 'Ada';\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn creates_sqlite_record_with_nullable_column_defaulting_to_null() {
		let database_path = create_sqlite_test_database(
			"creates_sqlite_record_with_nullable_column_defaulting_to_null",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int {\n    rec mut cust = new Customers;\n    cust.Id = 5;\n    create cust;\n    return count Customers where Id == 5;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let connection = Connection::open(&database_path).unwrap();
		let inserted_name: Option<String> = connection.query_row(
			"SELECT Name FROM Customers WHERE Id = 5",
			[],
			|row| row.get(0),
		).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(1)));
		assert_eq!(inserted_name, None);
	}

	#[test]
	fn defaults_omitted_nullable_date_field_to_null() {
		let result = evaluate_snippet(
			"obj Example { when: date?, };\nvar example: Example = Example { };\nexample.when"
		).unwrap();

		assert_eq!(result, Some(Value::Null));
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
			"Compile error at line 2, column 4: `if` condition must be of type `bool` or `record pointer`, found `int`.\n  |\n2 | if 1 {\n  |    ^"
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
			"Parse error at line 2, column 1: Expected `:` after ternary true branch.\n  |\n2 | ?\n  | ^"
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
	fn omits_synthetic_entry_frame_from_standalone_runtime_stack_trace() {
		let source = "fn inner() int {\n  var xs: [int] = [1];\n  return xs[2];\n}\nfn Main(args: [text]) int {\n  return inner();\n}";
		let error = run(source).unwrap_err();

		assert_eq!(
			error.to_string(),
			"Runtime error: Array index 2 is out of bounds for length 1.\nStack trace:\n  at inner (<source>:3:12)\n  at Main (<source>:6:15)"
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
		assert_eq!(debug.code_bodies().len(), 1);
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
	fn rejects_ambiguous_unqualified_table_in_count_expression() {
		let error = compile_snippet_with_schema_fixture(
			"with sales, archive;\ncount customers where true",
			r#"
				database Sales;
				schema Public;
				create table Customers ();

				database Archive;
				schema Public;
				create table Customers ();
			"#,
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Table reference `customers` is ambiguous across active databases: sales, archive."),
			position: 27,
		}));
	}

	#[test]
	fn rejects_arithmetic_on_union_typed_values() {
		let error = run("fn Main(args: [text]) int { var value: int | text = 1; return value + 1; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Expected numeric operands, found `int | text` and `int`."),
			position: 68,
		}));
	}

	#[test]
	fn rejects_assigning_null_to_non_nullable_variable() {
		let error = run("fn Main(args: [text]) int { var value: text = null; return 0; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot assign a value of type `null` to a variable of type `text`."),
			position: 46,
		}));
	}

	#[test]
	fn rejects_assignment_from_any_to_specific_type() {
		let error = run("fn Main(args: [text]) int { var value: any = 1; var total: int = value; return total; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot assign a value of type `any` to a variable of type `int`."),
			position: 65,
		}));
	}

	#[test]
	fn rejects_assignment_of_non_member_type_to_union() {
		let error = run("fn Main(args: [text]) int { var value: int | text = true; return 0; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot assign a value of type `bool` to a variable of type `int | text`."),
			position: 52,
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
	fn rejects_by_reference_argument_for_built_in_source_text() {
		let error = evaluate_snippet("var xs: [int] = [1];\nlen(&xs)").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `len` does not accept by-reference arguments."),
			position: 25,
		}));
	}

	#[test]
	fn rejects_contains_with_non_text_or_text_array_argument_source_text() {
		let error = evaluate_snippet("contains(1, 'x')").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `contains` does not accept an argument of type `int`."),
			position: 9,
		}));
	}

	#[test]
	fn rejects_count_expression_for_unsupported_backend() {
		let error = compile_snippet_with_schema_fixture_and_backends(
			"with exampledb;\ncount customers where active == true",
			r#"
				database ExampleDb;
				schema Public;
				create table Customers (
					Active bool not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::MySql)],
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Database query execution is not implemented yet for the `mysql` backend."),
			position: 16,
		}));
	}

	#[test]
	fn rejects_countof_with_non_text_or_text_array_argument_source_text() {
		let error = evaluate_snippet("countof(1, 'x')").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `countof` does not accept an argument of type `int`."),
			position: 8,
		}));
	}

	#[test]
	fn rejects_decimal_range_array_slice_source_text() {
		let error = evaluate_snippet("var xs: [int] = [10, 20, 30];\nxs[1.0:2.0]").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Array slicing requires a range of `int`, found `range<dec>`."),
			position: 36,
		}));
	}

	#[test]
	fn rejects_decimal_range_text_slice_source_text() {
		let error = evaluate_snippet("var s: text = 'hello';\ns[1.0:2.0]").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Text slicing requires a range of `int`, found `range<dec>`."),
			position: 28,
		}));
	}

	#[test]
	fn rejects_disp_with_non_text_argument_source_text() {
		let error = evaluate_snippet("disp(1)").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `disp` does not accept an argument of type `int`."),
			position: 5,
		}));
	}

	#[test]
	fn rejects_enum_downcast_to_wrong_backing_type() {
		let error = run(
			"enum Color { Red, Blue }\nfn Main(args: [text]) int { var color: Color = Color.Red; var value: text = text(color); return 0; }"
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `text` cannot cast enum `Color` because its backing type is `int`."),
			position: 105,
		}));
	}

	#[test]
	fn rejects_equality_comparison_on_any_values() {
		let error = run("fn Main(args: [text]) int { var left: any = 1; var right: any = 2; var same: bool = left == right; return 0; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Equality comparison is not supported between `any` and `any`."),
			position: 89,
		}));
	}

	#[test]
	fn rejects_if_rec_binding_used_inside_its_condition() {
		let error = compile_source_to_program_with_name_and_schema(
			"with exampledb;\nfn Main(args: [text]) int { if rec user = find first Customers where Id == 1 and user.Id == 1 { return user.Id; } return -1; }",
			None,
			CompilationTarget::Standalone,
			Some(&read_schema_catalog_from_str(
				r#"
					database ExampleDb;
					schema Main implicit;
					create table Customers (
						Id int not null
					);
				"#,
			).unwrap()),
		).unwrap_err();

		assert_eq!(
			error.format_with_source("with exampledb;\nfn Main(args: [text]) int { if rec user = find first Customers where Id == 1 and user.Id == 1 { return user.Id; } return -1; }"),
			"Compile error at line 2, column 82: Qualified field reference must use the target table name `Customers`.\n  |\n2 | fn Main(args: [text]) int { if rec user = find first Customers where Id == 1 and user.Id == 1 { return user.Id; } return -1; }\n  |                                                                                  ^"
		);
	}

	#[test]
	fn rejects_implicit_outer_variable_capture_in_nested_function_source_text() {
		let error = run(
			"fn Main(args: [text]) int { var x: int = 1; fn inner() int { return x; } return inner(); }"
		).unwrap_err();

		match error {
			TabloError::Compile(compile_error) => {
				assert_eq!(compile_error.message, "Variable `x` is not declared in this scope.");
			}
			other => panic!("expected compile error, found {other:?}"),
		}
	}

	#[test]
	fn rejects_indexof_with_non_text_or_text_array_argument_source_text() {
		let error = evaluate_snippet("indexof(1, 'x')").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `indexof` does not accept an argument of type `int`."),
			position: 8,
		}));
	}

	#[test]
	fn rejects_invalid_literal_numeric_format_string_at_compile_time() {
		let error = evaluate_snippet("format(12.0, 'x.00')").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Invalid numeric format string: Decimal numeric format strings must use `1` as the whole-digit marker."),
			position: 13,
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
	fn rejects_invalid_text_to_date_cast_at_runtime() {
		let error = evaluate_snippet("date('2026-02-30')").unwrap_err();

		assert_eq!(
			error.to_string(),
			"Runtime error: Built-in function `date` could not parse `2026-02-30` as an ISO-8601 date.\nStack trace:\n  at <source>:1:5"
		);
	}

	#[test]
	fn rejects_invalid_text_to_int_cast_at_runtime() {
		let error = evaluate_snippet("int('abc')").unwrap_err();

		assert_eq!(
			error.to_string(),
			"Runtime error: Built-in function `int` could not parse `abc` as an `int`.\nStack trace:\n  at <source>:1:4"
		);
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
	fn rejects_missing_by_reference_argument_source_text() {
		let error = run(
			"fn Main(args: [text]) int { var x: int = 1; bump(x); return x; }\nfn bump(value: &int) void { value += 1; }"
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Parameter `value` must be passed by reference."),
			position: 49,
		}));
	}

	#[test]
	fn rejects_missing_imported_module_file() {
		let root_path = write_test_source_file(
			"rejects_missing_imported_module_file_root",
			"main.tablo",
			"use './Missing';\nfn Main(args: [text]) int { return 0; }",
		);

		let error = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap_err();

		match error {
			TabloError::Compile(compile_error) => {
				assert_eq!(compile_error.position, 0);
				assert!(compile_error.message.starts_with("Failed to read imported module `./Missing` from `"));
			}
			other => panic!("expected compile error, found {other:?}"),
		}

		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
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
	fn rejects_named_use_of_non_public_function() {
		let root_path = write_test_source_file(
			"rejects_named_use_of_non_public_function_root",
			"main.tablo",
			"use UsefulHelper from './Helpers';\nfn Main(args: [text]) int { return 0; }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(&helper_path, "fn UsefulHelper() int { return 1; }").unwrap();

		let error = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Function `UsefulHelper` is not exported by module `./Helpers`."),
			position: 4,
		}));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn rejects_nested_use_during_module_resolution() {
		let root_path = write_test_source_file(
			"rejects_nested_use_during_module_resolution_root",
			"main.tablo",
			"fn Main(args: [text]) int {\n\tuse './Helpers';\n\treturn 0;\n}",
		);

		let error = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Nested `use` declarations are not yet supported during module resolution."),
			position: 29,
		}));

		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
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
	fn rejects_non_record_pointer_initializer_for_rec_declaration_source_text() {
		let error = run(standalone_body("rec cust = 1;\nreturn 0;")).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Record pointer `cust` must be initialized from a record pointer value, found `int`."),
			position: 39,
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
					locals: vec![
						crate::vm::VmVisibleLocal {
							declared_type: String::from("[int]"),
							is_const: false,
							name: String::from("xs"),
							slot: 0,
							value: crate::value::Value::Array(vec![
								crate::value::Value::Integer(10),
								crate::value::Value::Integer(20),
							]),
						},
					],
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
	fn rejects_split_with_non_text_argument_source_text() {
		let error = evaluate_snippet("split(1, ',')").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `split` does not accept an argument of type `int`."),
			position: 6,
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
	fn rejects_trim_with_non_text_argument_source_text() {
		let error = evaluate_snippet("trim(1)").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `trim` does not accept an argument of type `int`."),
			position: 5,
		}));
	}

	#[test]
	fn rejects_unknown_database_in_with_declaration() {
		let error = compile_snippet_with_schema_fixture(
			"with missingdb;\n1 + 2",
			r#"
				database ExampleDb;
				schema Public;
				create table Customers ();
			"#,
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Database `missingdb` is not present in the supplied schema catalog."),
			position: 5,
		}));
	}

	#[test]
	fn rejects_unknown_table_in_count_expression() {
		let error = compile_snippet_with_schema_fixture(
			"with exampledb;\ncount missing where true",
			r#"
				database ExampleDb;
				schema Public;
				create table Customers (
					Active bool not null
				);
			"#,
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Table `missing` is not present in the active databases."),
			position: 22,
		}));
	}

	#[test]
	fn rejects_use_without_source_file_path() {
		let error = compile_source_to_program_with_name_and_schema(
			"use './Helpers';\nfn Main(args: [text]) int { return 0; }",
			None,
			CompilationTarget::Standalone,
			None,
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Module imports require a source file path so relative `use` statements can be resolved."),
			position: 0,
		}));
	}

	#[test]
	fn rejects_void_order_by_expression_in_find_query() {
		let error = compile_snippet_with_schema_fixture_and_backends(
			"with exampledb;\nfind customers order by disp('x')",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap_err();

		match error {
			TabloError::Compile(compile_error) => {
				assert_eq!(compile_error.message, "Function `disp` is not yet supported in lowered database query expressions.");
			}
			other => panic!("expected compile error, found {other:?}"),
		}
	}

	#[test]
	fn rejects_with_declaration_without_schema_catalog() {
		let error = evaluate_snippet("with exampledb;\n1 + 2").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot validate `with` declarations without a supplied schema catalog."),
			position: 5,
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

		assert_eq!(error, TabloError::Parse(crate::syntax::parser::ParseError {
			position: 2,
			message: String::from("Expected `:` after ternary true branch."),
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
	fn runs_anonymous_inline_object_declaration_in_array_field_object_file() {
		let output_path = unique_test_output_path("runs_anonymous_inline_object_declaration_in_array_field_object_file");
		compile(
			"obj Outer { items: [{ value: int, }], };\nfn Main(args: [text]) int { var item: Outer.items.Element = Outer.items.Element { value: 7 }; var outer: Outer = Outer { items: [item] }; return outer.items[1].value; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_anonymous_inline_object_declaration_in_array_field_source_text() {
		let result = run(
			"obj Outer { items: [{ value: int, }], };\nfn Main(args: [text]) int { var item: Outer.items.Element = Outer.items.Element { value: 7 }; var outer: Outer = Outer { items: [item] }; return outer.items[1].value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_anonymous_inline_object_declaration_in_union_field_source_text() {
		let result = run(
			"obj Envelope { payload: text | { value: int, }, };\nfn Main(args: [text]) int { var payload: Envelope.payloadMember2 = Envelope.payloadMember2 { value: 7 }; var envelope: Envelope = Envelope { payload: payload }; return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_anonymous_inline_object_declaration_source_text() {
		let result = run(
			"obj Outer { inner: { value: int, }, };\nfn Main(args: [text]) int { var inner: Outer.inner = Outer.inner { value: 7 }; var outer: Outer = Outer { inner: inner }; return outer.inner.value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
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
	fn runs_array_fill_with_mixed_decimal_integer_multiplication_source_text() {
		let result = run(
			"fn Main(args: [text]) int {\n  var foo: [dec] = [];\n\n  for i in 2:20 {\n    foo[i / 2] = 0.75 * i;\n  }\n\n  return 0;\n}"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
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
	fn runs_array_slice_source_text() {
		let result = evaluate_snippet("var xs: [int] = [10, 20, 30, 40];\nxs[2:4]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(20),
			Value::Integer(30),
			Value::Integer(40),
		])));
	}

	#[test]
	fn runs_assignment_between_by_reference_parameters_source_text() {
		let result = run(
			"fn Main(args: [text]) int { var x: int = 1; var y: int = 5; copy(&x, &y); return x; }\nfn copy(dst: &int, src: &int) void { dst = src; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(5)));
	}

	#[test]
	fn runs_block_scope_with_shadowing() {
		let result = run(standalone_body("var x: int = 1;\n{\n  var x: int = 2;\n  x += 3;\n}\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_bool_backed_enum_source_text() {
		let result = run(
			"enum Flag: bool { Off: false, On: true }\nfn Main(args: [text]) int { var flag: Flag = Flag.On; if (flag == Flag.On) { return 1; } return 0; }"
		).unwrap();

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
	fn runs_by_reference_function_call_source_text() {
		let result = run(
			"fn Main(args: [text]) int { var x: int = 1; bump(&x); return x; }\nfn bump(value: &int) void { value += 1; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
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
	fn runs_contains_on_array_of_text_source_text() {
		let result = evaluate_snippet("var xs: [text] = ['Ada', 'Bea'];\ncontains(xs, 'Bea')").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_contains_on_text_source_text() {
		let result = evaluate_snippet("contains('  Ada  ', 'Ada')").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_countof_on_array_of_text_source_text() {
		let result = evaluate_snippet("var xs: [text] = ['Ada', 'Bea', 'Ada'];\ncountof('Ada', xs)").unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_countof_on_text_source_text() {
		let result = evaluate_snippet("countof('na', 'banana')").unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_date_backed_enum_source_text() {
		let result = run(
			"enum Holiday: date { NewYear: @2026-01-01, Christmas: @2026-12-25 }\nfn Main(args: [text]) int { var holiday: Holiday = Holiday.Christmas; if (holiday == Holiday.Christmas) { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_date_cast_from_text_source_text() {
		let result = evaluate_snippet("date('2026-06-16')").unwrap();

		assert_eq!(result, Some(Value::Date(crate::value::Date::from_parts(2026, 6, 16).unwrap())));
	}

	#[test]
	fn runs_date_comparison_operators_object_file() {
		let output_path = unique_test_output_path("runs_date_comparison_operators_object_file");
		compile_snippet_to_object_file(
			"var results: [bool] = [@2025-06-10 == @2025-06-10, @2025-06-10 != @2025-06-11, @2025-06-10 < @2025-06-11, @2025-06-10 <= @2025-06-10, @2025-06-11 > @2025-06-10, @2025-06-11 >= @2025-06-11];\nresults",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_date_comparison_operators_source_text() {
		let result = evaluate_snippet("[@2025-06-10 == @2025-06-10, @2025-06-10 != @2025-06-11, @2025-06-10 < @2025-06-11, @2025-06-10 <= @2025-06-10, @2025-06-11 > @2025-06-10, @2025-06-11 >= @2025-06-11]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_date_equality_object_file() {
		let output_path = unique_test_output_path("runs_date_equality_object_file");
		compile_snippet_to_object_file("var same: bool = @2025-06-10 == @2025-06-10;\nsame", &output_path).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_date_literal_source_text() {
		let result = evaluate_snippet("@2025-06-10").unwrap();

		assert_eq!(result, Some(Value::Date(crate::value::Date::from_literal("@2025-06-10").unwrap())));
	}

	#[test]
	fn runs_date_ordering_source_text() {
		let result = evaluate_snippet("@2025-06-10 < @2025-06-11").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_date_variable_comparison_in_if_source_text() {
		let result = run(
			"fn Main(args: [text]) int { const dateToday: date = @2026-06-14; const later: date = @2026-07-01; if dateToday > later { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_date_variable_without_initializer_with_current_date_default() {
		let current_date = crate::value::Date::current_local();
		let source = format!(
			"fn Main(args: [text]) int {{ var value: date; if value == @{:04}-{:02}-{:02} {{ return 1; }} return 0; }}",
			current_date.year,
			current_date.month,
			current_date.day,
		);
		let result = run(&source).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_day_month_year_built_ins() {
		let result = evaluate_snippet("[day(@2025-06-14), month(@2025-06-14), year(@2025-06-14)]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(14),
			Value::Integer(6),
			Value::Integer(2025),
		])));
	}

	#[test]
	fn runs_decimal_backed_enum_source_text() {
		let result = run(
			"enum Rate: dec { Reduced: 0.05, Standard: 0.20 }\nfn Main(args: [text]) int { var rate: Rate = Rate.Standard; if (rate == Rate.Standard) { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
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
	fn runs_disp_as_expression_statement_source_text() {
		let result = evaluate_snippet("disp('hello');\n1").unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_displn_in_standalone_source_text() {
		let result = run("fn Main(args: [text]) int { displn('hello'); return 0; }").unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_empty_array_literal_source_text() {
		let result = evaluate_snippet("var xs: [int] = [];\nxs").unwrap();

		assert_eq!(result, Some(Value::Array(vec![])));
	}

	#[test]
	fn runs_enum_downcast_to_int_backing_type() {
		let result = run(
			"enum Color { Red, Green: 3, Blue }\nfn Main(args: [text]) int { var color: Color = Color.Green; return int(color); }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_enum_downcast_to_text_backing_type() {
		let result = run(
			"enum Status: text { Pending: 'PENDING', Complete: 'COMPLETE' }\nfn Main(args: [text]) int { var status: Status = Status.Complete; if text(status) == 'COMPLETE' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_equality_source_text() {
		let result = evaluate_snippet("2 == 2.0").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_exists_for_missing_sqlite_find_query() {
		let database_path = create_sqlite_test_database(
			"runs_exists_for_missing_sqlite_find_query",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Active INTEGER NOT NULL
				);
				INSERT INTO Customers (Id, Active) VALUES (1, 1), (2, 0), (3, 1);
			"#,
		);
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			"with exampledb;\nexists(find first customers where id == 999)",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Active bool not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Boolean(false)));
	}

	#[test]
	fn runs_expression_statement_before_final_expression() {
		let result = run(standalone_body("var x: int = 5;\nx += 1;\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(6)));
	}

	#[test]
	fn runs_find_query_with_contains_array_literal_membership() {
		let database_path = create_sqlite_test_database(
			"runs_find_query_with_contains_array_literal_membership",
			r#"
				CREATE TABLE Tbl (
					Id INTEGER NOT NULL,
					Code TEXT NOT NULL,
					TableNum INTEGER NOT NULL
				);
				INSERT INTO Tbl (Id, Code, TableNum) VALUES
					(1, 'ALPHA', 7),
					(2, 'BRAVO', 7),
					(3, 'CHARLIE', 9);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { rec c = find first Tbl where TableNum == 7 and contains(['ALPHA', 'CHARLIE'], Code) order by Id; if c { return c.Id; } return 0; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Tbl (
					Id int not null,
					Code text not null,
					TableNum int not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_find_query_with_countof_and_indexof_text_functions() {
		let database_path = create_sqlite_test_database(
			"runs_find_query_with_countof_and_indexof_text_functions",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES
					(1, 'Banana'),
					(2, 'Pear');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { rec cust = find first Customers where countof('na', Name) == 2 and indexof('Ba', Name) == 1; if cust { return cust.Id; } return 0; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_find_query_with_trim_and_contains() {
		let database_path = create_sqlite_test_database(
			"runs_find_query_with_trim_and_contains",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES
					(1, '  Ada Lovelace  '),
					(2, 'Bea');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { rec cust = find first Customers where contains(trim(Name), 'Ada'); if cust { return cust.Id; } return 0; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(1)));
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
	fn runs_format_on_decimal_with_automatic_fraction_source_text() {
		let result = evaluate_snippet("[format(12.0, '1.'), format(12.5, '1.')]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Text(String::from("12")),
			Value::Text(String::from("12.5")),
		])));
	}

	#[test]
	fn runs_format_on_decimal_with_fixed_fraction_source_text() {
		let result = evaluate_snippet("format(12.3456, '1.00')").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("12.35"))));
	}

	#[test]
	fn runs_format_on_integer_source_text() {
		let result = evaluate_snippet("format(1234567, '1,111')").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("1,234,567"))));
	}

	#[test]
	fn runs_function_call_source_text() {
		let result = run("fn Main(args: [text]) int { return add(1, 2); }\nfn add(a: int, b: int) int { return a + b; }").unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_function_calls_with_record_pointer_parameters() {
		let database_path = create_sqlite_test_database(
			"runs_function_calls_with_record_pointer_parameters",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES (1, 'Ada');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn ReadName(cust: rec Customers) text { return cust.Name; }\nfn Rename(cust: &rec Customers) void { cust.Name = 'Ada Ltd'; }\nfn Main(args: [text]) int { rec mut cust = find first Customers where Id == 1; if cust { if ReadName(cust) == 'Ada' { Rename(&cust); if cust.Name == 'Ada Ltd' { return 1; } } } return 0; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(1)));
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
	fn runs_hour_minute_second_built_ins() {
		let result = evaluate_snippet("[hour(@12:34:56), minute(@12:34:56), second(@12:34:56)]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(12),
			Value::Integer(34),
			Value::Integer(56),
		])));
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
	fn runs_if_rec_else_when_record_is_missing() {
		let database_path = create_sqlite_test_database(
			"runs_if_rec_else_when_record_is_missing",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { if rec user = find first Customers where Id == 1 { return user.Id; } else { return -1; } }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(-1)));
	}

	#[test]
	fn runs_if_rec_when_record_is_found() {
		let database_path = create_sqlite_test_database(
			"runs_if_rec_when_record_is_found",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES
					(1, 'Ada');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { if rec user = find first Customers where Id == 1 { return user.Id; } return -1; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(1)));
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
	fn runs_indexof_on_array_of_text_source_text() {
		let result = evaluate_snippet("var xs: [text] = ['Ada', 'Bea'];\nindexof('Bea', xs)").unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_indexof_on_text_source_text() {
		let result = evaluate_snippet("indexof('na', 'banana')").unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_indexof_returns_null_when_not_found_source_text() {
		let result = evaluate_snippet("indexof('zz', 'banana')").unwrap();

		assert_eq!(result, Some(Value::Null));
	}

	#[test]
	fn runs_int_backed_enum_source_text() {
		let result = run(
			"enum Color { Red, Green: 3, Blue }\nfn Main(args: [text]) int { var color: Color; if (color == Color.Red) { color = Color.Blue; } if (color == Color.Blue) { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_int_cast_from_text_source_text() {
		let result = evaluate_snippet("int('42')").unwrap();

		assert_eq!(result, Some(Value::Integer(42)));
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
	fn runs_integer_variable_without_initializer_with_default() {
		let result = run("fn Main(args: [text]) int { var value: int; return value; }").unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
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
	fn runs_named_inline_object_declaration_in_array_field_object_file() {
		let output_path = unique_test_output_path("runs_named_inline_object_declaration_in_array_field_object_file");
		compile(
			"obj Outer { items: [obj Item { value: int, }], };\nfn Main(args: [text]) int { var item: Outer.Item = Outer.Item { value: 7 }; var outer: Outer = Outer { items: [item] }; return outer.items[1].value; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_named_inline_object_declaration_in_array_field_source_text() {
		let result = run(
			"obj Outer { items: [obj Item { value: int, }], };\nfn Main(args: [text]) int { var item: Outer.Item = Outer.Item { value: 7 }; var outer: Outer = Outer { items: [item] }; return outer.items[1].value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_named_inline_object_declaration_in_union_field_source_text() {
		let result = run(
			"obj Envelope { payload: text | obj Payload { value: int, }, };\nfn Main(args: [text]) int { var payload: Envelope.Payload = Envelope.Payload { value: 7 }; var envelope: Envelope = Envelope { payload: payload }; return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_named_inline_object_declaration_source_text() {
		let result = run(
			"obj Outer { inner: obj Inner { value: int, }, };\nfn Main(args: [text]) int { var inner: Outer.Inner = Outer.Inner { value: 7 }; var outer: Outer = Outer { inner: inner }; return outer.inner.value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_nested_by_reference_function_call_source_text() {
		let result = run(
			"fn Main(args: [text]) int { var x: int = 1; fn bump(value: &int) void { value += 1; } bump(&x); return x; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_nested_function_call_before_declaration_source_text() {
		let result = run(
			"fn Main(args: [text]) int { return add(1, 2); fn add(a: int, b: int) int { return a + b; } }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_nested_function_call_source_text() {
		let result = run(
			"fn Main(args: [text]) int { fn add(a: int, b: int) int { return a + b; } return add(1, 2); }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_nested_object_default_construction_source_text() {
		let result = evaluate_snippet(
			"obj Address { line1: text = 'Unknown', };\nobj Person { name: text = '', address: Address, };\nvar person: Person = Person { name: 'Alice' };\nperson.address"
		).unwrap();

		assert_eq!(
			result,
			Some(Value::Object(std::collections::BTreeMap::from([
				(String::from("line1"), Value::Text(String::from("Unknown"))),
			]))),
		);
	}

	#[test]
	fn runs_nested_object_field_assignment_source_text() {
		let result = run(
			"obj Address { line1: text = 'Unknown', };\nobj Person { name: text = '', address: Address, };\nfn Main(args: [text]) int { var person: Person = Person { name: 'Alice', address: Address { } }; person.address.line1 = 'Updated'; if person.address.line1 == 'Updated' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_not_on_record_pointer() {
		let database_path = create_sqlite_test_database(
			"runs_not_on_record_pointer",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { rec user = find first Customers where Id == 1; if not user { return -1; } return 1; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(-1)));
	}

	#[test]
	fn runs_null_literal_assignment_and_comparison() {
		let result = run(
			"fn Main(args: [text]) int { var value: text? = null; if value == null { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_nullable_array_variable_without_initializer_as_null() {
		let result = evaluate_snippet("var values: [int]?;\nvalues").unwrap();

		assert_eq!(result, Some(Value::Null));
	}

	#[test]
	fn runs_nullable_date_comparison_with_null_when_value_is_non_null() {
		let result = run(
			"fn Main(args: [text]) int { var value: date? = @2026-06-14; if value == null { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_nullable_date_comparison_with_null_when_value_is_null() {
		let result = run(
			"fn Main(args: [text]) int { var value: date? = null; if value == null { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_nullable_variable_without_initializer_as_null() {
		let result = evaluate_snippet("var value: int?;\nvalue").unwrap();

		assert_eq!(result, Some(Value::Null));
	}

	#[test]
	fn runs_object_default_field_values_source_text() {
		let result = run(
			"obj Person { name: text = 'Anonymous', age: int, };\nfn Main(args: [text]) int { var person: Person = Person { age: 30 }; if person.name == 'Anonymous' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_object_field_access_source_text() {
		let result = run(
			"obj Person { name: text = '', age: int, };\nfn Main(args: [text]) int { var person: Person = Person { age: 30 }; return person.age; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(30)));
	}

	#[test]
	fn runs_object_field_assignment_object_file() {
		let output_path = unique_test_output_path("runs_object_field_assignment_object_file");
		compile(
			"obj Counter { value: int = 0, };\nfn Main(args: [text]) int { var counter: Counter = Counter { }; counter.value += 2; return counter.value; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_object_field_compound_assignment_source_text() {
		let result = run(
			"obj Counter { value: int = 0, };\nfn Main(args: [text]) int { var counter: Counter = Counter { }; counter.value += 2; return counter.value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_object_field_without_explicit_default_using_default() {
		let result = run(
			"obj Counter { value: int, };\nfn Main(args: [text]) int { var counter: Counter = Counter { }; return counter.value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
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
	fn runs_object_object_file() {
		let output_path = unique_test_output_path("runs_object_object_file");
		compile(
			"obj Person { name: text = '', age: int, };\nfn Main(args: [text]) int { var person: Person = Person { age: 30 }; return person.age; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(30)));
	}

	#[test]
	fn runs_object_with_implicit_any_field_defaulting_to_null() {
		let result = evaluate_snippet(
			"obj Envelope { payload: any, };\nvar env: Envelope = Envelope { };\nenv.payload"
		).unwrap();

		assert_eq!(result, Some(Value::Null));
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
	fn runs_quoted_identifier_matching_keyword() {
		let result = run(
			"fn Main(args: [text]) int { var \"return\": int = 1; return \"return\"; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
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
	fn runs_root_array_shaped_object_declaration_with_anonymous_element_source_text() {
		let result = run(
			"obj CustomerCollection [{ name: text, }];\nfn Main(args: [text]) int { var customers: CustomerCollection = [CustomerCollection.Element { name: 'Alice' }]; if customers[1].name == 'Alice' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_root_array_shaped_object_declaration_with_named_element_object_file() {
		let output_path = unique_test_output_path("runs_root_array_shaped_object_declaration_with_named_element_object_file");
		compile(
			"obj CustomerCollection [obj Customer { name: text, }];\nfn Main(args: [text]) int { var customers: CustomerCollection = [CustomerCollection.Customer { name: 'Alice' }]; if customers[1].name == 'Alice' { return 1; } return 0; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_root_array_shaped_object_declaration_with_named_element_source_text() {
		let result = run(
			"obj CustomerCollection [obj Customer { name: text, }];\nfn Main(args: [text]) int { var customers: CustomerCollection = [CustomerCollection.Customer { name: 'Alice' }]; if customers[1].name == 'Alice' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
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
	fn runs_split_source_text() {
		let result = evaluate_snippet("split('Alpha,Beta,Gamma', ',')").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Text(String::from("Alpha")),
			Value::Text(String::from("Beta")),
			Value::Text(String::from("Gamma")),
		])));
	}

	#[test]
	fn runs_split_with_empty_separator_source_text() {
		let result = evaluate_snippet("split('Alpha', '')").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Text(String::from("Alpha")),
		])));
	}

	#[test]
	fn runs_sqlite_count_query_from_snippet() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_count_query_from_snippet",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Active INTEGER NOT NULL
				);
				INSERT INTO Customers (Id, Active) VALUES (1, 1), (2, 0), (3, 1);
			"#,
		);
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			"with exampledb;\ncount customers where active == true",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Active bool not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_sqlite_count_query_with_local_parameter_after_object_round_trip() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_count_query_with_local_parameter_after_object_round_trip",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Active INTEGER NOT NULL
				);
				INSERT INTO Customers (Id, Active) VALUES (1, 1), (2, 0), (3, 1);
			"#,
		);
		let output_path = unique_test_output_path("sqlite_count_query_program");
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { var targetId: int = 2; return count customers where id == targetId; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Active bool not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		write_program_to_path(&output_path, &program).unwrap();
		let decoded = read_program_from_path(&output_path).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&decoded, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_sqlite_find_first_query_from_snippet() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_find_first_query_from_snippet",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Active INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Active, Name) VALUES
					(2, 1, 'Bea'),
					(1, 1, 'Ada'),
					(3, 0, 'Cam');
			"#,
		);
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			"with exampledb;\n(find first customers where active == true order by id).name",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Active bool not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Text(String::from("Ada"))));
	}

	#[test]
	fn runs_sqlite_find_last_query_after_object_round_trip() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_find_last_query_after_object_round_trip",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Active INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Active, Name) VALUES
					(2, 1, 'Bea'),
					(1, 1, 'Ada'),
					(4, 1, 'Dee'),
					(3, 0, 'Cam');
			"#,
		);
		let output_path = unique_test_output_path("sqlite_find_query_program");
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { return (find last customers where active == true order by id).id; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Active bool not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		write_program_to_path(&output_path, &program).unwrap();
		let decoded = read_program_from_path(&output_path).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&decoded, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(4)));
	}

	#[test]
	fn runs_sqlite_find_query_bound_to_rec_declaration() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_find_query_bound_to_rec_declaration",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Active INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Active, Name) VALUES
					(2, 1, 'Bea'),
					(1, 1, 'Ada'),
					(3, 0, 'Cam');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { rec cust = find first customers where active == true order by id; if cust.name == 'Ada' { return cust.id; } return 0; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Active bool not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_sqlite_find_query_with_record_pointer_field_parameter() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_find_query_with_record_pointer_field_parameter",
			r#"
				CREATE TABLE OuterTable (
					Id INTEGER NOT NULL
				);
				CREATE TABLE InnerTable (
					Id INTEGER NOT NULL
				);
				INSERT INTO OuterTable (Id) VALUES (2);
				INSERT INTO InnerTable (Id) VALUES (1), (2), (3);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int {\n    rec outer = find first OuterTable where Id == 2;\n    if outer {\n        rec inner = find first InnerTable where InnerTable.Id == outer.Id;\n        if inner {\n            return inner.Id;\n        }\n    }\n    return 0;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table OuterTable (
					Id int not null
				);
				create table InnerTable (
					Id int not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_sqlite_find_query_when_accessed_nullable_field_is_null() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_find_query_when_accessed_nullable_field_is_null",
			r#"
				CREATE TABLE Customer (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL,
					Address1 TEXT NULL,
					Country TEXT NOT NULL
				);
				INSERT INTO Customer (Id, Name, Address1, Country) VALUES
					(2, 'Acme Ltd.', NULL, 'US');
			"#,
		);
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			"with test;\n(find first Customer).Address1",
			r#"
				database Test;
				schema Main implicit;
				create table Customer (
					Id int not null,
					Name text not null,
					Address1 text null,
					Country text not null
				);
			"#,
			&[("Test", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("Test", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Null));
	}

	#[test]
	fn runs_sqlite_find_query_when_unused_nullable_field_is_null() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_find_query_when_unused_nullable_field_is_null",
			r#"
				CREATE TABLE Customer (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL,
					Address1 TEXT NULL,
					Country TEXT NOT NULL
				);
				INSERT INTO Customer (Id, Name, Address1, Country) VALUES
					(2, 'Acme Ltd.', NULL, 'US');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with test;\nfn Main(args: [text]) int { rec temp = find first Customer; return temp.Id; }",
			r#"
				database Test;
				schema Main implicit;
				create table Customer (
					Id int not null,
					Name text not null,
					Address1 text null,
					Country text not null
				);
			"#,
			&[("Test", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("Test", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(2)));
	}


	#[test]
	fn runs_sqlite_for_record_query_loop() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_for_record_query_loop",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Active INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Active, Name) VALUES
					(2, 1, 'Bea'),
					(1, 1, 'Ada'),
					(3, 0, 'Cam');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]) int { var total: int = 0; for rec cust in customers where active == true order by id { total += cust.id; } return total; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Active bool not null,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_temporal_component_built_ins_on_timestamp() {
		let result = evaluate_snippet("[day(@2025-06-14T12:34:56), hour(@2025-06-14T12:34:56), second(@2025-06-14T12:34:56)]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(14),
			Value::Integer(12),
			Value::Integer(56),
		])));
	}

	#[test]
	fn runs_temporal_component_built_ins_on_timezoned_values() {
		let result = evaluate_snippet("[hour(@11:22:33+04:30), day(@2009-01-09T13:47Z)]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(11),
			Value::Integer(9),
		])));
	}

	#[test]
	fn runs_text_backed_enum_source_text() {
		let result = run(
			"enum Status: text { Pending: 'PENDING', Complete: 'COMPLETE' }\nfn Main(args: [text]) int { var status: Status = Status.Complete; if (status == Status.Complete) { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_text_concatenation_source_text() {
		let result = evaluate_snippet("'hello ' + 42").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello 42"))));
	}

	#[test]
	fn runs_text_index_source_text() {
		let result = evaluate_snippet("var s: text = 'hello';\ns[2]").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("e"))));
	}

	#[test]
	fn runs_text_length_source_text() {
		let result = evaluate_snippet("len('hello')").unwrap();

		assert_eq!(result, Some(Value::Integer(5)));
	}

	#[test]
	fn runs_text_relational_source_text() {
		let result = evaluate_snippet("'apple' < 'banana'").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_text_slice_source_text() {
		let result = evaluate_snippet("var s: text = 'hello';\ns[2:4]").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("ell"))));
	}

	#[test]
	fn runs_text_source_text() {
		let result = evaluate_snippet("'hello'").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello"))));
	}

	#[test]
	fn runs_time_comparison_operators_object_file() {
		let output_path = unique_test_output_path("runs_time_comparison_operators_object_file");
		compile_snippet_to_object_file(
			"var results: [bool] = [@12:34:56 == @12:34:56, @12:34:56 != @12:34:57, @12:34:56 < @12:34:57, @12:34:56 <= @12:34:56, @12:34:57 > @12:34:56, @12:34:57 >= @12:34:57];\nresults",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_time_comparison_operators_source_text() {
		let result = evaluate_snippet("[@12:34:56 == @12:34:56, @12:34:56 != @12:34:57, @12:34:56 < @12:34:57, @12:34:56 <= @12:34:56, @12:34:57 > @12:34:56, @12:34:57 >= @12:34:57]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_time_equality_source_text() {
		let result = evaluate_snippet("var value: time;\nvalue == value").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_time_literal_source_text() {
		let result = evaluate_snippet("@12:34:56.98765").unwrap();

		assert_eq!(result, Some(Value::Time(crate::value::Time::from_literal("@12:34:56.98765").unwrap())));
	}

	#[test]
	fn runs_time_variable_without_initializer_with_time_default() {
		let result = evaluate_snippet("var value: time;\nvalue").unwrap();

		assert!(matches!(result, Some(Value::Time(_))));
	}

	#[test]
	fn runs_timestamp_comparison_operators_object_file() {
		let output_path = unique_test_output_path("runs_timestamp_comparison_operators_object_file");
		compile_snippet_to_object_file(
			"var results: [bool] = [@2025-06-14T12:34:56 == @2025-06-14T12:34:56, @2025-06-14T12:34:56 != @2025-06-14T12:34:57, @2025-06-14T12:34:56 < @2025-06-14T12:34:57, @2025-06-14T12:34:56 <= @2025-06-14T12:34:56, @2025-06-14T12:34:57 > @2025-06-14T12:34:56, @2025-06-14T12:34:57 >= @2025-06-14T12:34:57];\nresults",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_timestamp_comparison_operators_source_text() {
		let result = evaluate_snippet("[@2025-06-14T12:34:56 == @2025-06-14T12:34:56, @2025-06-14T12:34:56 != @2025-06-14T12:34:57, @2025-06-14T12:34:56 < @2025-06-14T12:34:57, @2025-06-14T12:34:56 <= @2025-06-14T12:34:56, @2025-06-14T12:34:57 > @2025-06-14T12:34:56, @2025-06-14T12:34:57 >= @2025-06-14T12:34:57]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_timestamp_literal_source_text() {
		let result = evaluate_snippet("@2019-11-28T05:19:03").unwrap();

		assert_eq!(result, Some(Value::Timestamp(crate::value::Timestamp::from_literal("@2019-11-28T05:19:03").unwrap())));
	}

	#[test]
	fn runs_timestamp_ordering_source_text() {
		let result = evaluate_snippet("var value: timestamp;\nvalue <= value").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_timestamp_variable_comparison_in_if_source_text() {
		let result = run(
			"fn Main(args: [text]) int { const earlier: timestamp = @2026-06-14T12:00:00; const later: timestamp = @2026-07-01T00:00:00; if earlier > later { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_timestamp_variable_without_initializer_with_timestamp_default() {
		let result = evaluate_snippet("var value: timestamp;\nvalue").unwrap();

		assert!(matches!(result, Some(Value::Timestamp(_))));
	}

	#[test]
	fn runs_timestamptz_comparison_operators_object_file() {
		let output_path = unique_test_output_path("runs_timestamptz_comparison_operators_object_file");
		compile_snippet_to_object_file(
			"var results: [bool] = [@2025-06-14T11:00+01:00 == @2025-06-14T10:00Z, @2025-06-14T11:00+01:00 != @2025-06-14T10:30Z, @2025-06-14T11:00+01:00 < @2025-06-14T10:30+00:00, @2025-06-14T11:00+01:00 <= @2025-06-14T10:00Z, @2025-06-14T10:30+00:00 > @2025-06-14T11:00+01:00, @2025-06-14T10:00Z >= @2025-06-14T11:00+01:00];\nresults",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_timestamptz_comparison_operators_source_text() {
		let result = evaluate_snippet("[@2025-06-14T11:00+01:00 == @2025-06-14T10:00Z, @2025-06-14T11:00+01:00 != @2025-06-14T10:30Z, @2025-06-14T11:00+01:00 < @2025-06-14T10:30+00:00, @2025-06-14T11:00+01:00 <= @2025-06-14T10:00Z, @2025-06-14T10:30+00:00 > @2025-06-14T11:00+01:00, @2025-06-14T10:00Z >= @2025-06-14T11:00+01:00]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_timestamptz_literal_source_text() {
		let result = evaluate_snippet("@2009-01-09T13:47Z").unwrap();

		assert_eq!(result, Some(Value::TimestampTz(crate::value::TimestampTz::from_literal("@2009-01-09T13:47Z").unwrap())));
	}

	#[test]
	fn runs_timestamptz_variable_without_initializer_with_timestamptz_default() {
		let result = evaluate_snippet("var value: timestamptz;\nvalue").unwrap();

		assert!(matches!(result, Some(Value::TimestampTz(_))));
	}

	#[test]
	fn runs_timetz_comparison_operators_object_file() {
		let output_path = unique_test_output_path("runs_timetz_comparison_operators_object_file");
		compile_snippet_to_object_file(
			"var results: [bool] = [@11:00+01:00 == @10:00Z, @11:00+01:00 != @10:30Z, @11:00+01:00 < @10:30+00:00, @11:00+01:00 <= @10:00Z, @10:30+00:00 > @11:00+01:00, @10:00Z >= @11:00+01:00];\nresults",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_timetz_comparison_operators_source_text() {
		let result = evaluate_snippet("[@11:00+01:00 == @10:00Z, @11:00+01:00 != @10:30Z, @11:00+01:00 < @10:30+00:00, @11:00+01:00 <= @10:00Z, @10:30+00:00 > @11:00+01:00, @10:00Z >= @11:00+01:00]").unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
			Value::Boolean(true),
		])));
	}

	#[test]
	fn runs_timetz_literal_source_text() {
		let result = evaluate_snippet("@11:22:33+04:30").unwrap();

		assert_eq!(result, Some(Value::TimeTz(crate::value::TimeTz::from_literal("@11:22:33+04:30").unwrap())));
	}

	#[test]
	fn runs_timetz_variable_without_initializer_with_timetz_default() {
		let result = evaluate_snippet("var value: timetz;\nvalue").unwrap();

		assert!(matches!(result, Some(Value::TimeTz(_))));
	}

	#[test]
	fn runs_trim_source_text() {
		let result = evaluate_snippet("trim('  Ada  ')").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("Ada"))));
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

	#[test]
	fn runs_union_typed_program_after_object_round_trip() {
		let output_path = unique_test_output_path("runs_union_typed_program_after_object_round_trip");
		compile(
			"obj Envelope { payload: int | text = 1, };\nfn Main(args: [text]) int { var value: int | text = 'hello'; var env: Envelope = Envelope { payload: value }; return 0; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_union_typed_variable_and_object_field_source_text() {
		let result = run(
			"obj Envelope { payload: int | text = 1, };\nfn Main(args: [text]) int { var value: int | text = 1; var env: Envelope = Envelope { }; return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
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
	fn stringifies_enum_value_as_variant_name() {
		let result = run(
			"enum Color { Red, Green: 3, Blue }\nfn Main(args: [text]) int { var color: Color; color = Color.Blue; var message: text = 'Selected: ${ color }'; if message == 'Selected: Blue' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn validates_named_use_against_public_functions_in_imported_module() {
		let root_path = write_test_source_file(
			"validates_named_use_against_public_functions_in_imported_module_root",
			"main.tablo",
			"use UsefulHelper from './Helpers';\nfn Main(args: [text]) int { return 0; }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"pub fn UsefulHelper() int { return 1; }\nfn HiddenHelper() int { return 2; }",
		).unwrap();

		let result = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		);

		assert!(result.is_ok());

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}
}
