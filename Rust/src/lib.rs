use std::collections::{ BTreeMap, BTreeSet };
use std::fmt::Display;
use std::path::{ Path, PathBuf };

pub mod ast;
pub mod builtins;
pub mod bytecode;
pub mod compiler;
pub mod completion;
pub mod database;
pub mod debugger;
pub mod diagnostics;
pub mod format_string;
pub mod object_file;
pub mod query;
pub mod runtime_config;
pub mod schema;
pub mod schema_fixture;
pub mod semantic;
pub mod source;
pub mod syntax;
pub mod utils;
pub mod value;
pub mod vm;

mod sql;

use bytecode::*;
use compiler::{ CompileError , Compiler };
use database::RuntimeDatabaseConfig;
use object_file::*;
use schema::SchemaCatalog;
use semantic::analyzer::{ FunctionOverloadAlias, SemanticAnalyzer, SemanticWarning };
use semantic::ssa::*;
use source::SourceText;
use syntax::lexer::{ LexError, Lexer };
use syntax::parser::{ ParseError, Parser };
use utils::canonicalize_or_original;
use value::Value;
use vm::{ VirtualMachine, VmError };

const EXTERNAL_SOURCE_DIAGNOSTIC_PREFIX: &str = "__tablo_external_diagnostic__";

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

impl TabloError {
	pub fn format_with_source(&self, source: &str) -> String {
		self.format_with_source_name(source, None)
	}

	pub fn format_with_source_name(&self, source: &str, source_name: Option<&str>) -> String {
		let source = SourceText::new(source);

		match self {
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
			TabloError::Lex(error) => source.format_diagnostic_with_source_name("Lex error", error.position, &error.message, source_name),
			TabloError::Parse(error) => source.format_diagnostic_with_source_name("Parse error", error.position, &error.message, source_name),
			_ => self.to_string(),
		}
	}
}

impl Display for TabloError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TabloError::Compile(error) => {
				if let Some((category, _, _, message)) = decode_external_source_diagnostic(&error.message) {
					write!(f, "{category}: {message}")
				}
				else {
					write!(f, "Compile error: {}", error.message)
				}
			}
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

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CompilationReport {
	pub warnings: Vec<SemanticWarning>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SourceAnalysis {
	pub local_usage: ProgramLocalUsage,
	pub warnings: Vec<SemanticWarning>,
}

struct ImportedFunctionTargets {
	position: usize,
	target_names: Vec<String>,
}

#[derive(Clone)]
struct LinkedModule {
	exported_functions: BTreeMap<String, String>,
	functions: Vec<ast::FunctionDeclaration>,
	source_file: SourceFileDebugInfo,
	with_declarations: Vec<ast::WithDeclaration>,
}

struct LinkedProgram {
	function_overload_aliases: Vec<FunctionOverloadAlias>,
	function_source_files: Vec<SourceFileDebugInfo>,
	function_source_indices: Vec<u32>,
	program: ast::AstProgram,
	root_source_file: SourceFileDebugInfo,
	top_level_function_source_names: Vec<String>,
}

#[derive(Default)]
struct ModuleLinker {
	function_overload_aliases: Vec<FunctionOverloadAlias>,
	loaded_modules: BTreeMap<PathBuf, LinkedModule>,
	next_import_alias_id: u32,
	next_module_id: u32,
}

impl ModuleLinker {
	fn add_imported_function_target(
		imported_functions: &mut BTreeMap<String, ImportedFunctionTargets>,
		name: &str,
		target_name: &str,
		position: usize,
	) {
		let targets = imported_functions.entry(name.to_string())
			.or_insert_with(|| ImportedFunctionTargets {
				position,
				target_names: Vec::new(),
			});
		targets.position = position;

		if !targets.target_names.iter().any(|existing| existing == target_name) {
			targets.target_names.push(target_name.to_string());
		}
	}

	fn collect_import_bindings(
		&mut self,
		program: &ast::AstProgram,
		base_directory: &Path,
		source_name: &str,
	) -> Result<BTreeMap<String, String>, CompileError> {
		let mut imported_functions = BTreeMap::<String, ImportedFunctionTargets>::new();

		for statement in &program.statements {
			let ast::Statement::Use(use_declaration) = statement else {
				continue;
			};
			let module_path = resolve_module_path(base_directory, &use_declaration.module_path);
			let linked_module = self.load_module(&module_path, use_declaration)?;

			if let Some(imported_names) = &use_declaration.imported_names {
				for imported_name in imported_names {
					let Some(target_name) = linked_module.exported_functions.get(&imported_name.name) else {
						return Err(CompileError {
							message: format!(
								"Function `{}` is not exported by module `{}`.",
								imported_name.name,
								use_declaration.module_path,
							),
							position: imported_name.position,
						});
					};

					Self::add_imported_function_target(
						&mut imported_functions,
						&imported_name.name,
						target_name,
						imported_name.position,
					);
				}
			}
			else {
				for (name, target_name) in &linked_module.exported_functions {
					Self::add_imported_function_target(
						&mut imported_functions,
						name,
						target_name,
						use_declaration.position,
					);
				}
			}
		}

		let mut import_bindings = BTreeMap::new();
		for (display_name, targets) in imported_functions {
			let alias_name = format!(
				"__tablo_import_{}_{}",
				self.next_import_alias_id,
				display_name,
			);
			self.next_import_alias_id += 1;
			import_bindings.insert(display_name.clone(), alias_name.clone());
			self.function_overload_aliases.push(FunctionOverloadAlias {
				alias_name,
				display_name,
				position: targets.position,
				source_name: source_name.to_string(),
				target_names: targets.target_names,
			});
		}

		Ok(import_bindings)
	}

	fn include_local_function_overloads(
		&mut self,
		import_bindings: &BTreeMap<String, String>,
		local_targets: &BTreeMap<String, String>,
	) {
		for (source_name, alias_name) in import_bindings {
			let Some(local_target) = local_targets.get(source_name) else {
				continue;
			};
			let alias = self.function_overload_aliases.iter_mut()
				.find(|alias| alias.alias_name == *alias_name)
				.unwrap_or_else(|| panic!("Missing imported function alias `{alias_name}`."));

			if !alias.target_names.iter().any(|target| target == local_target) {
				alias.target_names.push(local_target.clone());
			}
		}
	}

	fn linked_function_count(&self) -> usize {
		self.loaded_modules.values()
			.map(|module| module.functions.len())
			.sum()
	}

	fn linked_function_groups(&self) -> Vec<(u32, &[ast::FunctionDeclaration])> {
		self.loaded_modules.values()
			.enumerate()
			.map(|(index, module)| (index as u32 + 1, module.functions.as_slice()))
			.collect()
	}

	fn linked_functions(&self) -> Vec<ast::FunctionDeclaration> {
		self.loaded_modules.values()
			.flat_map(|module| module.functions.iter().cloned())
			.collect()
	}

	fn linked_source_files(&self) -> Vec<SourceFileDebugInfo> {
		self.loaded_modules.values()
			.map(|module| module.source_file.clone())
			.collect()
	}

	fn linked_with_declarations(&self) -> Vec<ast::WithDeclaration> {
		self.loaded_modules.values()
			.flat_map(|module| module.with_declarations.iter().cloned())
			.collect()
	}

	fn load_module(
		&mut self,
		module_path: &Path,
		use_declaration: &ast::UseDeclaration,
	) -> Result<LinkedModule, CompileError> {
		let module_key = canonical_module_key(module_path);

		if let Some(linked_module) = self.loaded_modules.get(&module_key) {
			return Ok(linked_module.clone());
		}

		let source = std::fs::read_to_string(module_path).map_err(|error| CompileError {
			message: format!(
				"Failed to read imported module `{}` from `{}`: {}",
				use_declaration.module_path,
				module_path.display(),
				error,
			),
			position: use_declaration.position,
		})?;
		let source_text = SourceText::new(source);
		let mut program = parse_source_text(&source_text).map_err(|error| match error {
			TabloError::Lex(error) => CompileError {
				message: encode_external_source_diagnostic(
					"Lex error",
					&module_key.display().to_string(),
					error.position,
					&error.message,
				),
				position: error.position,
			},
			TabloError::Parse(error) => CompileError {
				message: encode_external_source_diagnostic(
					"Parse error",
					&module_key.display().to_string(),
					error.position,
					&error.message,
				),
				position: error.position,
			},
			other => CompileError {
				message: format!(
					"Failed to parse imported module `{}` from `{}`: {}",
					use_declaration.module_path,
					module_path.display(),
					other,
				),
				position: use_declaration.position,
			},
		})?;
		let base_directory = module_path.parent().unwrap_or_else(|| Path::new("."));
		let import_bindings = self.collect_import_bindings(
			&program,
			base_directory,
			&module_key.display().to_string(),
		)?;
		let module_id = self.next_module_id;
		self.next_module_id += 1;
		let top_level_renames = build_top_level_function_renames(&program, module_id);
		self.include_local_function_overloads(&import_bindings, &top_level_renames);
		let source_file = SourceFileDebugInfo::from_source(module_key.display().to_string(), &source_text);

		for function in &mut program.functions {
			if let Some(renamed) = top_level_renames.get(&function.name) {
				function.name = renamed.clone();
			}

			rewrite_function_declaration_calls(function, &top_level_renames, &import_bindings);
		}

		let exported_functions = program.functions.iter()
			.filter(|function| function.visibility == ast::Visibility::Public)
			.map(|function| {
				let original_name = unmangled_export_name(&function.name, &top_level_renames)
					.unwrap_or_else(|| function.name.clone());
				(original_name, function.name.clone())
			})
			.collect();
		let linked_module = LinkedModule {
			exported_functions,
			functions: program.functions,
			source_file,
			with_declarations: program.with_declarations,
		};
		self.loaded_modules.insert(module_key, linked_module.clone());
		Ok(linked_module)
	}
}

pub fn analyze_with_source_name(
	source: impl Into<String>,
	source_name: impl Into<String>,
) -> Result<SourceAnalysis, TabloError> {
	let source_name = source_name.into();
	analyze_source_local_usage_with_name_and_schema(source, Some(source_name.as_str()), None)
}

pub fn analyze_with_source_name_and_schema(
	source: impl Into<String>,
	source_name: impl Into<String>,
	schema_catalog: &SchemaCatalog,
) -> Result<SourceAnalysis, TabloError> {
	let source_name = source_name.into();
	analyze_source_local_usage_with_name_and_schema(source, Some(source_name.as_str()), Some(schema_catalog))
}

pub fn check(source: impl Into<String>) -> Result<(), TabloError> {
	compile_to_program_with_name(source, None).map(|_| ())
}

pub fn check_with_source_name(
	source: impl Into<String>,
	source_name: impl Into<String>,
) -> Result<(), TabloError> {
	let source_name = source_name.into();
	compile_to_program_with_name(source, Some(source_name.as_str())).map(|_| ())
}

pub fn check_with_source_name_and_schema(
	source: impl Into<String>,
	source_name: impl Into<String>,
	schema_catalog: &SchemaCatalog,
) -> Result<(), TabloError> {
	let source_name = source_name.into();
	compile_source_to_program_with_name_and_schema(
		source,
		Some(source_name.as_str()),
		CompilationTarget::Standalone,
		Some(schema_catalog),
	).map(|_| ())
}

pub fn compile(source: impl Into<String>, output_path: impl AsRef<Path>) -> Result<(), TabloError> {
	compile_report(source, output_path).map(|_| ())
}

pub fn compile_report(
	source: impl Into<String>,
	output_path: impl AsRef<Path>,
) -> Result<CompilationReport, TabloError> {
	let (program, warnings) = compile_source_to_program_with_name_and_schema_and_warnings(
		source,
		None,
		CompilationTarget::Standalone,
		None,
	)?;
	write_program_to_path(output_path, &program).map_err(TabloError::ObjectFile)?;
	Ok(CompilationReport { warnings })
}

pub fn compile_with_source_name(
	source: impl Into<String>,
	source_name: impl Into<String>,
	output_path: impl AsRef<Path>
) -> Result<(), TabloError> {
	compile_with_source_name_report(source, source_name, output_path).map(|_| ())
}

pub fn compile_with_source_name_and_schema(
	source: impl Into<String>,
	source_name: impl Into<String>,
	output_path: impl AsRef<Path>,
	schema_catalog: &SchemaCatalog,
) -> Result<(), TabloError> {
	compile_with_source_name_and_schema_report(source, source_name, output_path, schema_catalog)
		.map(|_| ())
}

pub fn compile_with_source_name_and_schema_report(
	source: impl Into<String>,
	source_name: impl Into<String>,
	output_path: impl AsRef<Path>,
	schema_catalog: &SchemaCatalog,
) -> Result<CompilationReport, TabloError> {
	let source_name = source_name.into();
	let (program, warnings) = compile_source_to_program_with_name_and_schema_and_warnings(
		source,
		Some(source_name.as_str()),
		CompilationTarget::Standalone,
		Some(schema_catalog),
	)?;
	write_program_to_path(output_path, &program).map_err(TabloError::ObjectFile)?;
	Ok(CompilationReport { warnings })
}

pub fn compile_with_source_name_report(
	source: impl Into<String>,
	source_name: impl Into<String>,
	output_path: impl AsRef<Path>
) -> Result<CompilationReport, TabloError> {
	let source_name = source_name.into();
	let (program, warnings) = compile_source_to_program_with_name_and_schema_and_warnings(
		source,
		Some(source_name.as_str()),
		CompilationTarget::Standalone,
		None,
	)?;
	write_program_to_path(output_path, &program).map_err(TabloError::ObjectFile)?;
	Ok(CompilationReport { warnings })
}

pub fn discover_project_config_path(file_path: &Path) -> Option<PathBuf> {
	let mut current = file_path.parent()?;

	loop {
		let candidate = current.join("tablo.toml");
		if candidate.is_file() {
			return Some(candidate);
		}

		current = current.parent()?;
	}
}

pub fn local_usage_with_source_name(
	source: impl Into<String>,
	source_name: impl Into<String>,
) -> Result<ProgramLocalUsage, TabloError> {
	analyze_with_source_name(source, source_name).map(|analysis| analysis.local_usage)
}

pub fn local_usage_with_source_name_and_schema(
	source: impl Into<String>,
	source_name: impl Into<String>,
	schema_catalog: &SchemaCatalog,
) -> Result<ProgramLocalUsage, TabloError> {
	analyze_with_source_name_and_schema(source, source_name, schema_catalog)
		.map(|analysis| analysis.local_usage)
}

pub(crate) fn encode_external_source_diagnostic(
	category: &str,
	source_name: &str,
	position: usize,
	message: &str,
) -> String {
	format!(
		"{EXTERNAL_SOURCE_DIAGNOSTIC_PREFIX}\u{1f}{category}\u{1f}{source_name}\u{1f}{position}\u{1f}{message}"
	)
}

pub fn run(source: impl Into<String>) -> Result<Option<Value>, TabloError> {
	let program = compile_to_program_with_name(source, None)?;
	run_program(&program)
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

fn analyze_source_local_usage_with_name_and_schema(
	source: impl Into<String>,
	source_name: Option<&str>,
	schema_catalog: Option<&SchemaCatalog>,
) -> Result<SourceAnalysis, TabloError> {
	let source = SourceText::new(source);
	let program = parse_source_text(&source)?;
	validate_module_graph(&program, source_name).map_err(TabloError::Compile)?;
	let linked_program = link_program_modules(&program, &source, source_name).map_err(TabloError::Compile)?;
	let mut analyzer = SemanticAnalyzer::new();
	analyzer.set_function_overload_aliases(linked_program.function_overload_aliases.clone());
	analyzer.set_root_source_name(Some(linked_program.root_source_file.display_name().to_string()));
	analyzer.set_top_level_function_source_names(linked_program.top_level_function_source_names.clone());
	let semantic_program = analyzer.analyze_program_with_schema(&linked_program.program, schema_catalog)
		.map_err(TabloError::Compile)?;
	let local_usage = analyze_program_local_usage(&linked_program.program, &semantic_program);
	let root_source_name = linked_program.root_source_file.display_name();
	let warnings = semantic_program.warnings().iter()
		.filter(|warning| warning.source_name.as_deref().is_none_or(|name| name == root_source_name))
		.cloned()
		.collect();

	Ok(SourceAnalysis {
		local_usage: filter_root_source_local_usage(local_usage, &linked_program),
		warnings,
	})
}

fn attach_source_debug_info(program: &mut Program, linked_program: &LinkedProgram) {
	let debug_info = program.debug_info_mut();
	let root_source_file_index = debug_info.add_source_file(linked_program.root_source_file.clone());
	let mut imported_source_file_indices = Vec::with_capacity(linked_program.function_source_files.len());
	let code_body_count = debug_info.code_bodies().len();

	for source_file in &linked_program.function_source_files {
		imported_source_file_indices.push(debug_info.add_source_file(source_file.clone()));
	}

	for (body_index, source_file_index) in linked_program.function_source_indices.iter().enumerate() {
		let resolved_index = if *source_file_index == 0 {
			root_source_file_index
		}
		else {
			imported_source_file_indices[(*source_file_index - 1) as usize]
		};
		debug_info.set_code_body_source_file(body_index, resolved_index);
	}

	for body_index in linked_program.function_source_indices.len()..code_body_count {
		debug_info.set_code_body_source_file(body_index, root_source_file_index);
	}
}

fn build_top_level_function_renames(program: &ast::AstProgram, module_id: u32) -> BTreeMap<String, String> {
	program.functions.iter()
		.map(|function| {
			(
				function.name.clone(),
				format!("__tablo_module_{module_id}_{}", function.name),
			)
		})
		.collect()
}

fn canonical_module_key(module_path: &Path) -> PathBuf {
	canonicalize_or_original(module_path)
}

fn collect_function_source_indices(program: &ast::AstProgram, source_file_index: u32) -> Vec<u32> {
	let mut source_indices = Vec::new();

	for function in &program.functions {
		extend_function_source_indices(function, source_file_index, &mut source_indices);
	}

	source_indices
}

fn collect_local_function_names(statements: &[ast::Statement]) -> Vec<String> {
	statements.iter()
		.filter_map(|statement| match statement {
			ast::Statement::FunctionDeclaration(function) => Some(function.name.clone()),
			_ => None,
		})
		.collect()
}

fn compile_ast_program_with_schema_and_analyzer(
	program: &ast::AstProgram,
	target: CompilationTarget,
	schema_catalog: Option<&SchemaCatalog>,
	mut analyzer: SemanticAnalyzer,
) -> Result<(Program, Vec<SemanticWarning>), TabloError> {
	let semantic_program = match target {
		CompilationTarget::Snippet => analyzer.analyze_program_with_schema(program, schema_catalog),
		CompilationTarget::Standalone => analyzer.analyze_standalone_program_with_schema(program, schema_catalog),
	}.map_err(TabloError::Compile)?;
	let warnings = semantic_program.warnings().to_vec();

	let mut compiler = Compiler::new();
	let program = compiler.compile_program_with_existing_semantics(program, &semantic_program)
		.map_err(TabloError::Compile)?;
	Ok((program, warnings))
}

fn compile_source_to_program_with_name_and_schema(
	source: impl Into<String>,
	source_name: Option<&str>,
	target: CompilationTarget,
	schema_catalog: Option<&SchemaCatalog>,
) -> Result<Program, TabloError> {
	compile_source_to_program_with_name_and_schema_and_warnings(
		source,
		source_name,
		target,
		schema_catalog,
	).map(|(program, _)| program)
}

fn compile_source_to_program_with_name_and_schema_and_warnings(
	source: impl Into<String>,
	source_name: Option<&str>,
	target: CompilationTarget,
	schema_catalog: Option<&SchemaCatalog>,
) -> Result<(Program, Vec<SemanticWarning>), TabloError> {
	let source = SourceText::new(source);
	let program = parse_source_text(&source)?;
	validate_module_graph(&program, source_name).map_err(TabloError::Compile)?;
	let linked_program = link_program_modules(&program, &source, source_name).map_err(TabloError::Compile)?;
	let mut analyzer = SemanticAnalyzer::new();
	analyzer.set_function_overload_aliases(linked_program.function_overload_aliases.clone());
	analyzer.set_root_source_name(Some(linked_program.root_source_file.display_name().to_string()));
	analyzer.set_top_level_function_source_names(linked_program.top_level_function_source_names.clone());
	let (mut program, warnings) = compile_ast_program_with_schema_and_analyzer(
		&linked_program.program,
		target,
		schema_catalog,
		analyzer,
	)?;
	attach_source_debug_info(&mut program, &linked_program);
	Ok((program, warnings))
}

fn decode_external_source_diagnostic(message: &str) -> Option<(&str, String, usize, String)> {
	let payload = message.strip_prefix(EXTERNAL_SOURCE_DIAGNOSTIC_PREFIX)?
		.strip_prefix('\u{1f}')?;
	let mut parts = payload.splitn(4, '\u{1f}');
	let category = parts.next()?;
	let source_name = parts.next()?.to_string();
	let position = parts.next()?.parse::<usize>().ok()?;
	let diagnostic_message = parts.next()?.to_string();
	Some((category, source_name, position, diagnostic_message))
}

fn extend_function_source_indices(
	function: &ast::FunctionDeclaration,
	source_file_index: u32,
	source_indices: &mut Vec<u32>,
) {
	source_indices.push(source_file_index);

	for statement in &function.body.statements {
		extend_function_source_indices_from_statement(statement, source_file_index, source_indices);
	}
}

fn extend_function_source_indices_from_statement(
	statement: &ast::Statement,
	source_file_index: u32,
	source_indices: &mut Vec<u32>,
) {
	match statement {
		ast::Statement::Block(block) => {
			for statement in &block.statements {
				extend_function_source_indices_from_statement(statement, source_file_index, source_indices);
			}
		}
		ast::Statement::For(for_statement) => {
			for statement in &for_statement.body.statements {
				extend_function_source_indices_from_statement(statement, source_file_index, source_indices);
			}
		}
		ast::Statement::ForRecord(for_statement) => {
			for statement in &for_statement.body.statements {
				extend_function_source_indices_from_statement(statement, source_file_index, source_indices);
			}
		}
		ast::Statement::FunctionDeclaration(function) => {
			extend_function_source_indices(function, source_file_index, source_indices);
		}
		ast::Statement::If(if_statement) => {
			for statement in &if_statement.then_branch.statements {
				extend_function_source_indices_from_statement(statement, source_file_index, source_indices);
			}

			if let Some(else_branch) = &if_statement.else_branch {
				extend_function_source_indices_from_statement(else_branch, source_file_index, source_indices);
			}
		}
		ast::Statement::Transaction(transaction_statement) => {
			for statement in &transaction_statement.body.statements {
				extend_function_source_indices_from_statement(statement, source_file_index, source_indices);
			}
		}
		ast::Statement::While(while_statement) => {
			for statement in &while_statement.body.statements {
				extend_function_source_indices_from_statement(statement, source_file_index, source_indices);
			}
		}
		ast::Statement::Break(_)
		| ast::Statement::Continue(_)
		| ast::Statement::Create(_)
		| ast::Statement::Delete(_)
		| ast::Statement::EnumDeclaration(_)
		| ast::Statement::Expression(_)
		| ast::Statement::RecordPointerDeclaration(_)
		| ast::Statement::Return(_)
		| ast::Statement::Update(_)
		| ast::Statement::Use(_)
		| ast::Statement::VariableDeclaration(_) => {}
	}
}

fn filter_root_source_local_usage(local_usage: ProgramLocalUsage, linked_program: &LinkedProgram) -> ProgramLocalUsage {
	let functions = local_usage.functions.into_iter()
		.zip(linked_program.function_source_indices.iter().copied())
		.filter_map(|(function_usage, source_index)| {
			if source_index == 0 {
				Some(function_usage)
			}
			else {
				None
			}
		})
		.collect::<Vec<FunctionLocalUsage>>();

	ProgramLocalUsage {
		functions,
	}
}

fn first_nested_use_position(program: &ast::AstProgram) -> Option<usize> {
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

fn first_use_statement(program: &ast::AstProgram) -> Option<&ast::UseDeclaration> {
	for statement in &program.statements {
		if let ast::Statement::Use(use_declaration) = statement {
			return Some(use_declaration);
		}
	}

	None
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

fn link_program_modules(
	program: &ast::AstProgram,
	source: &SourceText,
	source_name: Option<&str>,
) -> Result<LinkedProgram, CompileError> {
	let root_source_file = SourceFileDebugInfo::from_source(source_name.unwrap_or("<source>"), source);
	let root_display_name = root_source_file.display_name().to_string();

	let Some(source_name) = source_name else {
		return Ok(LinkedProgram {
			function_overload_aliases: Vec::new(),
			function_source_files: Vec::new(),
			function_source_indices: collect_function_source_indices(program, 0),
			program: program.clone(),
			root_source_file,
			top_level_function_source_names: program.functions.iter()
				.map(|_| root_display_name.clone())
				.collect(),
		});
	};

	if first_use_statement(program).is_none() {
		return Ok(LinkedProgram {
			function_overload_aliases: Vec::new(),
			function_source_files: Vec::new(),
			function_source_indices: collect_function_source_indices(program, 0),
			program: program.clone(),
			root_source_file,
			top_level_function_source_names: program.functions.iter()
				.map(|_| root_display_name.clone())
				.collect(),
		});
	}

	let root_path = Path::new(source_name);
	let root_directory = root_path.parent().unwrap_or_else(|| Path::new("."));
	let mut linker = ModuleLinker::default();
	let import_bindings = linker.collect_import_bindings(
		program,
		root_directory,
		&root_display_name,
	)?;
	let root_function_targets = program.functions.iter()
		.map(|function| (function.name.clone(), function.name.clone()))
		.collect();
	linker.include_local_function_overloads(&import_bindings, &root_function_targets);
	let mut linked_program = program.clone();

	for function in &mut linked_program.functions {
		rewrite_function_declaration_calls(function, &BTreeMap::new(), &import_bindings);
	}

	for statement in &mut linked_program.statements {
		rewrite_statement_calls(statement, &BTreeMap::new(), &import_bindings, &[]);
	}

	if let Some(result) = &mut linked_program.result {
		rewrite_expression_calls(result, &BTreeMap::new(), &import_bindings, &[]);
	}

	linked_program.functions = linker.linked_functions()
		.into_iter()
		.chain(linked_program.functions)
		.collect();
	linked_program.statements.retain(|statement| !matches!(statement, ast::Statement::Use(_)));
	linked_program.with_declarations = merge_with_declarations(
		&linked_program.with_declarations,
		&linker.linked_with_declarations(),
	);
	let imported_source_files = linker.linked_source_files();
	let mut top_level_function_source_names = Vec::new();
	let mut function_source_indices = Vec::new();

	for (source_file_index, functions) in linker.linked_function_groups() {
		let display_name = imported_source_files[(source_file_index - 1) as usize].display_name().to_string();
		top_level_function_source_names.extend(functions.iter().map(|_| display_name.clone()));

		for function in functions {
			extend_function_source_indices(function, source_file_index, &mut function_source_indices);
		}
	}

	top_level_function_source_names.extend(
		linked_program.functions[linker.linked_function_count()..].iter()
			.map(|_| root_display_name.clone()),
	);

	for function in &linked_program.functions[linker.linked_function_count()..] {
		extend_function_source_indices(function, 0, &mut function_source_indices);
	}

	Ok(LinkedProgram {
		function_overload_aliases: linker.function_overload_aliases,
		function_source_files: imported_source_files,
		function_source_indices,
		program: linked_program,
		root_source_file,
		top_level_function_source_names,
	})
}

fn merge_with_declarations(
	base_declarations: &[ast::WithDeclaration],
	imported_declarations: &[ast::WithDeclaration],
) -> Vec<ast::WithDeclaration> {
	let mut seen = BTreeSet::new();
	let mut merged = Vec::new();

	for declaration in base_declarations.iter().chain(imported_declarations.iter()) {
		let mut databases = Vec::new();

		for database in &declaration.databases {
			let key = database.name.to_ascii_lowercase();
			if seen.insert(key) {
				databases.push(database.clone());
			}
		}

		if !databases.is_empty() {
			merged.push(ast::WithDeclaration {
				databases,
				position: declaration.position,
			});
		}
	}

	merged
}

fn parse_source_text(source: &SourceText) -> Result<ast::AstProgram, TabloError> {
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

fn rewrite_expression_calls(
	expression: &mut ast::Expr,
	top_level_renames: &BTreeMap<String, String>,
	import_bindings: &BTreeMap<String, String>,
	shadowed_function_names: &[String],
) {
	match expression {
		ast::Expr::Array(array) => {
			for element in &mut array.elements {
				rewrite_expression_calls(element, top_level_renames, import_bindings, shadowed_function_names);
			}
		}
		ast::Expr::Assignment(assignment) => {
			match &mut assignment.target {
				ast::AssignmentTarget::Field(target) => {
					let _ = target;
				}
				ast::AssignmentTarget::Identifier(_) => {}
				ast::AssignmentTarget::Index(target) => {
					rewrite_expression_calls(&mut target.index, top_level_renames, import_bindings, shadowed_function_names);
				}
			}

			rewrite_expression_calls(&mut assignment.value, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Expr::Binary(binary) => {
			rewrite_expression_calls(&mut binary.left, top_level_renames, import_bindings, shadowed_function_names);
			rewrite_expression_calls(&mut binary.right, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Expr::Boolean(_)
		| ast::Expr::Date(_)
		| ast::Expr::Decimal(_)
		| ast::Expr::Integer(_)
		| ast::Expr::Null(_)
		| ast::Expr::Text(_)
		| ast::Expr::Time(_)
		| ast::Expr::TimeTz(_)
		| ast::Expr::Timestamp(_)
		| ast::Expr::TimestampTz(_) => {}
		ast::Expr::Call(call) => {
			for argument in &mut call.arguments {
				if let Some(expression) = argument.expression_mut() {
					rewrite_expression_calls(expression, top_level_renames, import_bindings, shadowed_function_names);
				}
			}

			if !shadowed_function_names.iter().any(|name| name == &call.callee.name) {
				if let Some(renamed) = import_bindings.get(&call.callee.name) {
					call.callee.name = renamed.clone();
				}
				else if let Some(renamed) = top_level_renames.get(&call.callee.name) {
					call.callee.name = renamed.clone();
				}
			}
		}
		ast::Expr::Count(count) => {
			if let Some(where_clause) = &mut count.where_clause {
				rewrite_expression_calls(where_clause, top_level_renames, import_bindings, shadowed_function_names);
			}
		}
		ast::Expr::FieldAccess(field_access) => {
			rewrite_expression_calls(&mut field_access.object, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Expr::Find(find) => {
			if let Some(where_clause) = &mut find.where_clause {
				rewrite_expression_calls(where_clause, top_level_renames, import_bindings, shadowed_function_names);
			}

			for order_by in &mut find.order_by {
				rewrite_expression_calls(&mut order_by.expression, top_level_renames, import_bindings, shadowed_function_names);
			}
		}
		ast::Expr::Identifier(_) | ast::Expr::New(_) => {}
		ast::Expr::Index(index) => {
			rewrite_expression_calls(&mut index.array, top_level_renames, import_bindings, shadowed_function_names);
			rewrite_expression_calls(&mut index.index, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Expr::ObjectConstruction(object_construction) => {
			for field in &mut object_construction.fields {
				rewrite_expression_calls(&mut field.value, top_level_renames, import_bindings, shadowed_function_names);
			}
		}
		ast::Expr::Range(range) => {
			rewrite_expression_calls(&mut range.start, top_level_renames, import_bindings, shadowed_function_names);
			rewrite_expression_calls(&mut range.end, top_level_renames, import_bindings, shadowed_function_names);
			if let Some(step) = &mut range.step {
				rewrite_expression_calls(step, top_level_renames, import_bindings, shadowed_function_names);
			}
		}
		ast::Expr::Ternary(ternary) => {
			rewrite_expression_calls(&mut ternary.condition, top_level_renames, import_bindings, shadowed_function_names);
			rewrite_expression_calls(&mut ternary.true_branch, top_level_renames, import_bindings, shadowed_function_names);
			rewrite_expression_calls(&mut ternary.false_branch, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Expr::Unary(unary) => {
			rewrite_expression_calls(&mut unary.operand, top_level_renames, import_bindings, shadowed_function_names);
		}
	}
}

fn rewrite_function_declaration_calls(
	function: &mut ast::FunctionDeclaration,
	top_level_renames: &BTreeMap<String, String>,
	import_bindings: &BTreeMap<String, String>,
) {
	for parameter in &mut function.parameters {
		if let Some(default_value) = &mut parameter.default_value {
			rewrite_expression_calls(default_value, top_level_renames, import_bindings, &[]);
		}
	}

	rewrite_statements_calls(&mut function.body.statements, top_level_renames, import_bindings, &[]);
}

fn rewrite_statement_calls(
	statement: &mut ast::Statement,
	top_level_renames: &BTreeMap<String, String>,
	import_bindings: &BTreeMap<String, String>,
	shadowed_function_names: &[String],
) {
	match statement {
		ast::Statement::Block(block) => {
			rewrite_statements_calls(&mut block.statements, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Statement::Expression(expression) => {
			rewrite_expression_calls(expression, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Statement::For(for_statement) => {
			rewrite_expression_calls(&mut for_statement.iterable, top_level_renames, import_bindings, shadowed_function_names);
			rewrite_statements_calls(&mut for_statement.body.statements, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Statement::ForRecord(for_statement) => {
			if let Some(where_clause) = &mut for_statement.where_clause {
				rewrite_expression_calls(where_clause, top_level_renames, import_bindings, shadowed_function_names);
			}

			for order_by in &mut for_statement.order_by {
				rewrite_expression_calls(&mut order_by.expression, top_level_renames, import_bindings, shadowed_function_names);
			}

			for group_by in &mut for_statement.group_by {
				rewrite_expression_calls(&mut group_by.expression, top_level_renames, import_bindings, shadowed_function_names);
			}

			if let Some(limit) = &mut for_statement.limit {
				rewrite_expression_calls(limit, top_level_renames, import_bindings, shadowed_function_names);
			}

			rewrite_statements_calls(&mut for_statement.body.statements, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Statement::FunctionDeclaration(function) => {
			rewrite_function_declaration_calls(function, top_level_renames, import_bindings);
		}
		ast::Statement::If(if_statement) => {
			match &mut if_statement.condition {
				ast::IfCondition::Expression(expression) => {
					rewrite_expression_calls(expression, top_level_renames, import_bindings, shadowed_function_names);
				}
				ast::IfCondition::RecordPointerBinding(binding) => {
					rewrite_expression_calls(&mut binding.initial_value, top_level_renames, import_bindings, shadowed_function_names);
				}
			}

			rewrite_statements_calls(&mut if_statement.then_branch.statements, top_level_renames, import_bindings, shadowed_function_names);
			if let Some(else_branch) = &mut if_statement.else_branch {
				rewrite_statement_calls(else_branch, top_level_renames, import_bindings, shadowed_function_names);
			}
		}
		ast::Statement::RecordPointerDeclaration(record_pointer) => {
			rewrite_expression_calls(&mut record_pointer.initial_value, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Statement::Return(return_statement) => {
			if let Some(value) = &mut return_statement.value {
				rewrite_expression_calls(value, top_level_renames, import_bindings, shadowed_function_names);
			}
		}
		ast::Statement::Transaction(transaction_statement) => {
			rewrite_statements_calls(&mut transaction_statement.body.statements, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Statement::VariableDeclaration(variable) => {
			if let Some(initial_value) = &mut variable.initial_value {
				rewrite_expression_calls(initial_value, top_level_renames, import_bindings, shadowed_function_names);
			}
		}
		ast::Statement::While(while_statement) => {
			rewrite_expression_calls(&mut while_statement.condition, top_level_renames, import_bindings, shadowed_function_names);
			rewrite_statements_calls(&mut while_statement.body.statements, top_level_renames, import_bindings, shadowed_function_names);
		}
		ast::Statement::Break(_)
		| ast::Statement::Continue(_)
		| ast::Statement::Create(_)
		| ast::Statement::Delete(_)
		| ast::Statement::EnumDeclaration(_)
		| ast::Statement::Update(_)
		| ast::Statement::Use(_) => {}
	}
}

fn rewrite_statements_calls(
	statements: &mut [ast::Statement],
	top_level_renames: &BTreeMap<String, String>,
	import_bindings: &BTreeMap<String, String>,
	shadowed_function_names: &[String],
) {
	let local_function_names = collect_local_function_names(statements);
	let mut visible_shadowed_names = shadowed_function_names.to_vec();
	visible_shadowed_names.extend(local_function_names);

	for statement in statements {
		rewrite_statement_calls(statement, top_level_renames, import_bindings, &visible_shadowed_names);
	}
}

fn unmangled_export_name(name: &str, top_level_renames: &BTreeMap<String, String>) -> Option<String> {
	top_level_renames.iter()
		.find_map(|(original, renamed)| if renamed == name { Some(original.clone()) } else { None })
}

fn validate_module_graph(program: &ast::AstProgram, source_name: Option<&str>) -> Result<(), CompileError> {
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
	program: &ast::AstProgram,
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
		let canonical_path = module_key.display().to_string();
		let imported_program = parse_source_text(&SourceText::new(source)).map_err(|error| match error {
			TabloError::Lex(error) => CompileError {
				message: encode_external_source_diagnostic(
					"Lex error",
					&canonical_path,
					error.position,
					&error.message,
				),
				position: error.position,
			},
			TabloError::Parse(error) => CompileError {
				message: encode_external_source_diagnostic(
					"Parse error",
					&canonical_path,
					error.position,
					&error.message,
				),
				position: error.position,
			},
			other => CompileError {
				message: format!(
					"Failed to parse imported module `{}` from `{}`: {}",
					use_declaration.module_path,
					module_path.display(),
					other,
				),
				position: use_declaration.position,
			},
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

	use rusqlite::Connection;

	use super::*;

	use crate::query::*;
	use crate::schema::*;
	use crate::schema_fixture::*;

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

	fn compile_standalone_with_query_optimizations(
		source: &str,
		schema: &SchemaCatalog,
		query_optimizations_enabled: bool,
	) -> Result<(crate::bytecode::Program, crate::semantic::analyzer::SemanticProgram), TabloError> {
		let source = SourceText::new(source);
		let ast_program = parse_source_text(&source)?;
		let mut analyzer = SemanticAnalyzer::new();
		analyzer.set_query_optimizations_enabled(query_optimizations_enabled);
		let semantic_program = analyzer.analyze_standalone_program_with_schema(&ast_program, Some(schema))
			.map_err(TabloError::Compile)?;
		let program = Compiler::new()
			.compile_program_with_existing_semantics(&ast_program, &semantic_program)
			.map_err(TabloError::Compile)?;

		Ok((program, semantic_program))
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

	fn run_program_with_database_config_and_query_count(
		program: &Program,
		database_config: RuntimeDatabaseConfig,
	) -> (Result<Option<Value>, TabloError>, usize) {
		let mut vm = VirtualMachine::with_database_config(database_config);
		let result = vm.run(program).map_err(TabloError::Runtime);
		let query_execution_count = vm.database_query_execution_count();
		(result, query_execution_count)
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
		format!("fn Main(args: [text]): int {{\n{body}\n}}")
	}

	fn standalone_expression(expression: &str) -> String {
		format!("fn Main(args: [text]): int {{ return {expression}; }}")
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
	fn accepts_optional_overloads_distinguished_by_parameter_name() {
		let result = run(
			"fn format(value: int, radix: int = 10): int { return radix; }\n\
			fn format(value: int, prefix: text = ''): int { return len(prefix); }\n\
			fn Main(args: [text]): int {\n\
			    return format(1, radix: 16) + format(1, prefix: 'xx');\n\
			}"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(18)));
	}

	#[test]
	fn accepts_recursively_composed_default_expressions() {
		let result = run(
			"obj Sample { Value: int, };\n\
			enum Choice { One }\n\
			fn Identity(value: int): int { return value; }\n\
			fn Evaluate(\n\
			    literalValue: int = 1,\n\
			    calculatedValue: int = day(@2026-07-28) - 27,\n\
			    enumValue: Choice = Choice.One,\n\
			    arrayValue: [int] = [1, 2],\n\
			    indexedValue: int = [10, 20][1],\n\
			    objectValue: Sample = Sample { Value: 1 },\n\
			    fieldValue: int = Sample { Value: 1 }.Value,\n\
			    selectedValue: int = true ? 1 : 0,\n\
			    calledValue: int = Identity(1)\n\
			): int {\n\
			    return literalValue + calculatedValue + int(enumValue) + arrayValue[1]\n\
			        + indexedValue / 10 + objectValue.Value + fieldValue + selectedValue + calledValue;\n\
			}\n\
			fn Main(args: [text]): int { return Evaluate(); }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(9)));
	}

	#[test]
	fn assigns_imported_functions_to_their_own_source_file_in_debug_metadata() {
		let root_path = write_test_source_file(
			"assigns_imported_functions_to_their_own_source_file_in_debug_metadata_root",
			"main.tablo",
			"use Helper from './Helpers';\nfn Main(args: [text]): int { return Helper(); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"pub fn Helper(): int { return PrivateHelper(); }\nfn PrivateHelper(): int { return 1; }",
		).unwrap();

		let program = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap();

		let helper_body_index = program.functions().iter()
			.position(|function| function.name() == Some("__tablo_module_0_Helper"))
			.unwrap();
		let private_helper_body_index = program.functions().iter()
			.position(|function| function.name() == Some("__tablo_module_0_PrivateHelper"))
			.unwrap();
		let main_body_index = program.functions().iter()
			.position(|function| function.name() == Some("Main"))
			.unwrap();

		assert_eq!(program.debug_location(helper_body_index, 0).unwrap().display_name(), Some(helper_path.to_str().unwrap()));
		assert_eq!(program.debug_location(private_helper_body_index, 0).unwrap().display_name(), Some(helper_path.to_str().unwrap()));
		assert_eq!(program.debug_location(main_body_index, 0).unwrap().display_name(), Some(root_path.to_str().unwrap()));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn auto_creates_new_record_before_break() {
		let database_path = create_sqlite_test_database(
			"auto_creates_new_record_before_break",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    while true {\n        rec mut cust = new Customers;\n        cust.Id = 13;\n        cust.Name = 'Cia';\n        break;\n    }\n    return count Customers where Id == 13 and Name == 'Cia';\n}",
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
	fn auto_creates_new_record_before_continue() {
		let database_path = create_sqlite_test_database(
			"auto_creates_new_record_before_continue",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    var i: int = 0;\n    while i < 2 {\n        i += 1;\n        rec mut cust = new Customers;\n        cust.Id = i;\n        cust.Name = 'Dee';\n        if i == 1 {\n            continue;\n        }\n    }\n    return count Customers where Name == 'Dee';\n}",
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

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn auto_creates_new_record_before_return() {
		let database_path = create_sqlite_test_database(
			"auto_creates_new_record_before_return",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    rec mut cust = new Customers;\n    cust.Id = 11;\n    cust.Name = 'Bea';\n    return 5;\n}",
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
		let connection = Connection::open(&database_path).unwrap();
		let inserted_count: i64 = connection.query_row(
			"SELECT COUNT(*) FROM Customers WHERE Id = 11 AND Name = 'Bea'",
			[],
			|row| row.get(0),
		).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(5)));
		assert_eq!(inserted_count, 1);
	}

	#[test]
	fn auto_creates_new_record_on_block_exit() {
		let database_path = create_sqlite_test_database(
			"auto_creates_new_record_on_block_exit",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    {\n        rec mut cust = new Customers;\n        cust.Id = 7;\n        cust.Name = 'Ada';\n    }\n    return count Customers where Id == 7 and Name == 'Ada';\n}",
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
	fn auto_update_cleanup_ignores_unchanged_missing_record_pointer() {
		let database_path = create_sqlite_test_database(
			"auto_update_cleanup_ignores_unchanged_missing_record_pointer",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    rec mut cust = find first Customers where Id == -1;\n    return 0;\n}",
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

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn auto_updates_mutable_for_record_loop_variable_each_iteration() {
		let database_path = create_sqlite_test_database(
			"auto_updates_mutable_for_record_loop_variable_each_iteration",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES (10, 'Ada');
				INSERT INTO Customers (Id, Name) VALUES (11, 'Ada');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    for rec mut cust in Customers where Name == 'Ada' {\n        cust.Name = 'Updated';\n    }\n    return count Customers where Name == 'Updated';\n}",
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

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn auto_updates_mutable_record_pointer_when_scope_exits() {
		let database_path = create_sqlite_test_database(
			"auto_updates_mutable_record_pointer_when_scope_exits",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES (8, 'Ada');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    {\n        rec mut cust = find first Customers where Id == 8;\n        cust.Name = 'Mina';\n    }\n    return count Customers where Id == 8 and Name == 'Mina';\n}",
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
	fn clamps_negative_for_record_limit_to_zero_before_query_execution() {
		let database_path = create_sqlite_test_database(
			"clamps_negative_for_record_limit_to_zero_before_query_execution",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL
				);
				INSERT INTO Customers (Id) VALUES (10);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    var rows: int = 0;\n    for rec cust in Customers limit -1 {\n        rows += 1;\n    }\n    return rows;\n}",
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

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn commits_auto_created_record_after_successful_transaction() {
		let database_path = create_sqlite_test_database(
			"commits_auto_created_record_after_successful_transaction",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    transaction {\n        {\n            rec mut cust = new Customers;\n            cust.Id = 19;\n            cust.Name = 'Iris';\n        }\n    }\n    return count Customers where Id == 19 and Name == 'Iris';\n}",
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
	fn compilation_report_preserves_semantic_warnings() {
		let schema = schema_catalog_from_fixture_with_backends(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let (_, warnings) = compile_source_to_program_with_name_and_schema_and_warnings(
			"with ExampleDb;\n\
			fn Main(args: [text]): int {\n\
				rec cust = find first Customers;\n\
				var unavailable: bool = locked cust;\n\
				return 0;\n\
			}",
			Some("main.tablo"),
			CompilationTarget::Standalone,
			Some(&schema),
		).unwrap();

		assert_eq!(warnings.len(), 1);
		assert!(warnings[0].message.contains("`locked` is always false"));
	}

	#[test]
	fn compiles_mysql_count_query_through_normal_compiler_path() {
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			"with exampledb;\nvar divisor: int = 2;\nvar minimumId: int = 10;\ncount customers where id / divisor >= minimumId",
			r#"
				database ExampleDb;
				schema Reporting;
				create table Customers (
					Id int not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::MySql)],
		).unwrap();

		let LoweredBackendQuery::Sql(query) = &program.queries()[0];
		assert_eq!(query.dialect, SqlDialect::MySql);
		assert_eq!(
			query.statement,
			"SELECT COUNT(*) FROM `Reporting`.`Customers` WHERE ((`Customers`.`Id` DIV ?) >= ?)",
		);
		assert_eq!(query.parameters.len(), 2);
		assert_eq!(query.parameters[0].index, 1);
		assert_eq!(query.parameters[1].index, 2);
	}

	#[test]
	fn compiles_mysql_find_query_through_normal_compiler_path() {
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			concat!(
				"with exampledb;\n",
				"var requiredActive: bool = true;\n",
				"rec mut customer = find last Customers ",
				"where Active == requiredActive order by Name asc;\n",
				"0",
			),
			r#"
				database ExampleDb;
				schema Reporting;
				create table Customers (
					Id int primary key,
					Name text not null,
					Active bool not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::MySql)],
		).unwrap();

		let LoweredBackendQuery::Sql(query) = &program.queries()[0];
		assert_eq!(query.dialect, SqlDialect::MySql);
		assert_eq!(query.lock_mode, RecordLockMode::UpdateNoWait);
		assert_eq!(
			query.statement,
			concat!(
				"SELECT `Customers`.`Id` ",
				"FROM `Reporting`.`Customers` WHERE (`Customers`.`Active` = ?) ",
				"ORDER BY `Customers`.`Name` DESC LIMIT 1",
			),
		);
		let SqlQueryResultShape::RecordPointer(layout) = &query.result_shape else {
			panic!("Expected record-pointer query result metadata.");
		};
		let columns = layout.known_schema().expect("Expected a statically known record schema.");
		assert!(columns.iter().any(|column| column.column_name == "Id" && column.is_primary_key));
		assert_eq!(
			layout.selected_known_columns().unwrap().iter().map(|column| column.column_name.as_str()).collect::<Vec<_>>(),
			vec!["Id"],
		);
	}

	#[test]
	fn compiles_mysql_grouped_for_query_through_normal_compiler_path() {
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			concat!(
				"with exampledb;\n",
				"var requiredActive: bool = true;\n",
				"var maxRows: int = 5;\n",
				"for rec mut customer in Customers where Active == requiredActive ",
				"group by trim(Country) as country limit maxRows { }\n",
				"0",
			),
			r#"
				database ExampleDb;
				schema Reporting;
				create table Customers (
					Id int primary key,
					Name text not null,
					Country text not null,
					Active bool not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::MySql)],
		).unwrap();

		let LoweredBackendQuery::Sql(query) = &program.queries()[0];
		assert_eq!(query.dialect, SqlDialect::MySql);
		assert_eq!(query.lock_mode, RecordLockMode::Update);
		assert_eq!(
			query.statement,
			concat!(
				"SELECT `Customers`.`Id`, TRIM(`Customers`.`Country`) ",
				"FROM `Reporting`.`Customers` WHERE (`Customers`.`Active` = ?) ",
				"ORDER BY TRIM(`Customers`.`Country`) LIMIT ?",
			),
		);
		assert_eq!(query.parameters.len(), 2);
		assert_eq!(query.parameters[0].index, 1);
		assert_eq!(query.parameters[1].index, 2);
		assert_eq!(query.group_by[0].key_names, vec![String::from("country")]);
		let SqlQueryResultShape::RecordPointerArray(layout) = &query.result_shape else {
			panic!("Expected record-pointer array query result metadata.");
		};
		assert_eq!(
			layout.selected_known_columns().unwrap().iter().map(|column| column.column_name.as_str()).collect::<Vec<_>>(),
			vec!["Id"],
		);
	}

	#[test]
	fn compiles_postgresql_count_query_through_normal_compiler_path() {
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			"with exampledb;\nvar minimumId: int = 10;\ncount customers where id >= minimumId",
			r#"
				database ExampleDb;
				schema Public;
				create table Customers (
					Id int not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::PostgreSql)],
		).unwrap();

		let LoweredBackendQuery::Sql(query) = &program.queries()[0];
		assert_eq!(query.dialect, SqlDialect::PostgreSql);
		assert_eq!(query.lock_mode, RecordLockMode::None);
		assert_eq!(
			query.statement,
			"SELECT COUNT(*) FROM \"Public\".\"Customers\" WHERE (\"Customers\".\"Id\" >= CAST(CAST($1 AS TEXT) AS BIGINT))",
		);
		assert_eq!(query.parameters.len(), 1);
	}

	#[test]
	fn compiles_postgresql_find_query_through_normal_compiler_path() {
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			concat!(
				"with exampledb;\n",
				"var requiredActive: bool = true;\n",
				"rec customer = find last Customers ",
				"where Active == requiredActive order by Name asc;\n",
				"0",
			),
			r#"
				database ExampleDb;
				schema Public;
				create table Customers (
					Id int primary key,
					Name text not null,
					Active bool not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::PostgreSql)],
		).unwrap();

		let LoweredBackendQuery::Sql(query) = &program.queries()[0];
		assert_eq!(query.dialect, SqlDialect::PostgreSql);
		assert_eq!(query.lock_mode, RecordLockMode::None);
		assert_eq!(
			query.statement,
			concat!(
				"SELECT 1 FROM \"Public\".\"Customers\" ",
				"WHERE (\"Customers\".\"Active\" = CAST(CAST($1 AS TEXT) AS BOOLEAN)) ",
				"ORDER BY \"Customers\".\"Name\" DESC LIMIT 1",
			),
		);
		let SqlQueryResultShape::RecordPointer(layout) = &query.result_shape else {
			panic!("Expected record-pointer query result metadata.");
		};
		let columns = layout.known_schema().expect("Expected a statically known record schema.");
		assert_eq!(columns.len(), 3);
		assert!(columns.iter().any(|column| column.column_name == "Id" && column.is_primary_key));
		assert!(layout.selected_known_columns().unwrap().is_empty());
	}

	#[test]
	fn compiles_postgresql_grouped_for_query_through_normal_compiler_path() {
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			concat!(
				"with exampledb;\n",
				"var requiredActive: bool = true;\n",
				"var maxRows: int = 5;\n",
				"for rec customer in Customers where Active == requiredActive ",
				"group by trim(Country) as country limit maxRows { }\n",
				"0",
			),
			r#"
				database ExampleDb;
				schema Public;
				create table Customers (
					Id int primary key,
					Name text not null,
					Country text not null,
					Active bool not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::PostgreSql)],
		).unwrap();

		let LoweredBackendQuery::Sql(query) = &program.queries()[0];
		assert_eq!(query.dialect, SqlDialect::PostgreSql);
		assert_eq!(
			query.statement,
			concat!(
				"SELECT CAST(TRIM(\"Customers\".\"Country\") AS TEXT) FROM \"Public\".\"Customers\" ",
				"WHERE (\"Customers\".\"Active\" = CAST(CAST($1 AS TEXT) AS BOOLEAN)) ",
				"ORDER BY TRIM(\"Customers\".\"Country\") LIMIT CAST(CAST($2 AS TEXT) AS BIGINT)",
			),
		);
		let SqlQueryResultShape::RecordPointerArray(layout) = &query.result_shape else {
			panic!("Expected record-pointer array query result metadata.");
		};
		let columns = layout.known_schema().expect("Expected a statically known record schema.");
		assert_eq!(columns.len(), 4);
		assert!(layout.selected_known_columns().unwrap().is_empty());
		assert_eq!(query.group_by.len(), 1);
		assert_eq!(query.group_by[0].key_names, vec![String::from("country")]);
	}

	#[test]
	fn compiles_source_text_to_object_file() {
		let output_path = unique_test_output_path("compiles_source_text_to_object_file");
		compile("fn Main(args: [text]): int { return 1 + 2; }", &output_path).unwrap();
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
	fn compiles_sqlite_for_record_query_with_minimized_field_list() {
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			concat!(
				"with exampledb;\n",
				"fn Main(args: [text]): int {\n",
				"  for rec customer in Customers { displn(customer.Name); }\n",
				"  return 0;\n",
				"}",
			),
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null primary key,
					Name text not null,
					Notes text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();

		let LoweredBackendQuery::Sql(query) = &program.queries()[0];
		assert_eq!(query.dialect, SqlDialect::Sqlite);
		assert_eq!(query.statement, "SELECT \"Customers\".\"Name\" FROM \"Customers\"");
		let SqlQueryResultShape::RecordPointerArray(layout) = &query.result_shape else {
			panic!("Expected record-pointer array query result metadata.");
		};
		assert_eq!(
			layout.selected_known_columns().unwrap().iter().map(|column| column.column_name.as_str()).collect::<Vec<_>>(),
			vec!["Name"],
		);
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
			"with exampledb;\nfn Main(args: [text]): int {\n    rec mut cust = new Customers;\n    cust.Id = 7;\n    cust.Name = 'Ada';\n    create cust;\n    return count Customers where Id == 7 and Name == 'Ada';\n}",
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
			"with exampledb;\nfn Main(args: [text]): int {\n    rec mut cust = new Customers;\n    cust.Id = 5;\n    create cust;\n    return count Customers where Id == 5;\n}",
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
	fn deleted_record_pointer_no_longer_exists() {
		let database_path = create_sqlite_test_database(
			"deleted_record_pointer_no_longer_exists",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES (32, 'Ada');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    rec mut cust = find first Customers where Id == 32;\n    delete cust;\n    if exists cust {\n        return 1;\n    }\n    return 0;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null primary key,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn deletes_sqlite_record_from_mutable_record_pointer() {
		let database_path = create_sqlite_test_database(
			"deletes_sqlite_record_from_mutable_record_pointer",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES (31, 'Ada');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    rec mut cust = find first Customers where Id == 31;\n    delete cust;\n    return count Customers where Id == 31;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null primary key,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn evaluates_explicit_arguments_before_defaults_in_the_specified_orders() {
		let database_path = create_sqlite_test_database(
			"evaluates_explicit_arguments_before_defaults_in_the_specified_orders",
			r#"
				CREATE TABLE CallOrder (
					Id INTEGER PRIMARY KEY AUTOINCREMENT,
					Name TEXT NOT NULL
				);
				INSERT INTO CallOrder (Name) VALUES ('Initial');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\n\
			fn NextValue(): int { return seqnext(CallOrder); }\n\
			fn Combine(\n\
			    firstValue: int = NextValue(),\n\
			    secondValue: int = NextValue(),\n\
			    thirdValue: int = NextValue()\n\
			): int {\n\
			    return firstValue * 100 + secondValue * 10 + thirdValue;\n\
			}\n\
			fn Main(args: [text]): int {\n\
			    const ordered: int = Combine(secondValue: NextValue(), firstValue: NextValue(), thirdValue: default);\n\
			    const allDefault: int = Combine();\n\
			    const fullyExplicit: int = Combine(firstValue: 1, secondValue: 2, thirdValue: 9);\n\
			    return ordered == 324 and allDefault == 567 and fullyExplicit == 129 and CallOrder == 7 ? 1 : 0;\n\
			}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table CallOrder (
					Id int not null,
					Name text not null
				);
				create sequence CallOrder;
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
	fn evaluates_for_record_limit_once_before_query_execution() {
		let database_path = create_sqlite_test_database(
			"evaluates_for_record_limit_once_before_query_execution",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL
				);
				INSERT INTO Customers (Id) VALUES (10);
				INSERT INTO Customers (Id) VALUES (11);
				INSERT INTO Customers (Id) VALUES (12);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn GetLimit(callCount: &int): int { callCount += 1; return 2; }\nfn Main(args: [text]): int {\n    var callCount: int = 0;\n    var rows: int = 0;\n    for rec cust in Customers limit GetLimit(&callCount) {\n        rows += 1;\n    }\n    return callCount * 10 + rows;\n}",
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

		assert_eq!(result, Some(Value::Integer(12)));
	}

	#[test]
	fn evaluates_named_argument_for_built_in_function() {
		let result = evaluate_snippet("len(str: 'abc')").unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn executes_merged_correlated_count_with_independent_fallback() {
		let database_path = create_sqlite_test_database(
			"executes_merged_correlated_count_with_independent_fallback",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL
				);
				CREATE TABLE Orders (
					CustomerId INTEGER NOT NULL
				);
				INSERT INTO Customers (Id) VALUES (1), (2);
				INSERT INTO Orders (CustomerId) VALUES (1), (1), (2);
			"#,
		);
		let schema = schema_catalog_from_fixture_with_backends(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
				create table Orders (
					CustomerId int not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let source = concat!(
			"with exampledb;\n",
			"fn Main(args: [text]): int {\n",
			"  for rec customer in Customers order by Id {\n",
			"    var orderCount: int = count Orders where CustomerId == customer.Id;\n",
			"    var values: [int] = [10];\n",
			"    var selected: int = values[orderCount];\n",
			"  }\n",
			"  return 0;\n",
			"}",
		);
		let (planned_program, planned_semantics) =
			compile_standalone_with_query_optimizations(source, &schema, true).unwrap();
		let (unoptimized_program, _) =
			compile_standalone_with_query_optimizations(source, &schema, false).unwrap();
		let planned_main = planned_program.functions()
			.get(planned_program.entry_function_index().unwrap() as usize)
			.unwrap();
		let unoptimized_main = unoptimized_program.functions()
			.get(unoptimized_program.entry_function_index().unwrap() as usize)
			.unwrap();
		let planned_execute_count = planned_main.body().instructions.iter()
			.filter(|instruction| matches!(instruction, Instruction::ExecuteQuery(_)))
			.count();
		let unoptimized_execute_count = unoptimized_main.body().instructions.iter()
			.filter(|instruction| matches!(instruction, Instruction::ExecuteQuery(_)))
			.count();
		let projected_load_count = planned_main.body().instructions.iter()
			.filter(|instruction| matches!(instruction, Instruction::LoadProjectedValue(_)))
			.count();
		let projected_load_index = planned_main.body().instructions.iter()
			.position(|instruction| matches!(instruction, Instruction::LoadProjectedValue(_)))
			.unwrap();
		let projected_load_location = planned_program.debug_location(
			planned_program.entry_function_index().unwrap() as usize,
			projected_load_index,
		).unwrap();
		let merged_query = planned_program.queries().first().unwrap();
		let LoweredBackendQuery::Sql(merged_query) = merged_query;

		assert_eq!(planned_program.queries().len(), 1);
		assert_eq!(unoptimized_program.queries().len(), 2);
		assert_eq!(planned_execute_count, 1);
		assert_eq!(unoptimized_execute_count, 2);
		assert_eq!(projected_load_count, 1);
		assert_eq!(
			projected_load_location.column(),
			source.find("count Orders").unwrap() as u32 + 1,
		);
		assert_eq!(merged_query.scalar_projections.len(), 1);
		assert!(merged_query.statement.contains("SELECT COUNT(*)"));
		assert!(planned_semantics.query_plan().queries().iter().any(|query| {
			matches!(query.execution, PlannedQueryExecution::MergeWith { .. })
		}));

		let round_tripped_program = read_program(&write_program(&planned_program)).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let (planned_result, planned_query_execution_count) =
			run_program_with_database_config_and_query_count(&round_tripped_program, database_config.clone());
		let (unoptimized_result, unoptimized_query_execution_count) =
			run_program_with_database_config_and_query_count(&unoptimized_program, database_config);
		let planned_error = planned_result.unwrap_err();
		let unoptimized_error = unoptimized_result.unwrap_err();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(planned_query_execution_count, 1);
		assert_eq!(unoptimized_query_execution_count, 2);
		assert_eq!(planned_error.to_string(), unoptimized_error.to_string());
		assert_eq!(
			planned_error.to_string(),
			"Runtime error: Array index 2 is out of bounds for length 1.\nStack trace:\n  at Main (line 1, column 224)",
		);
	}

	#[test]
	fn exists_returns_false_for_field_beyond_null_intermediate_object() {
		let result = evaluate_snippet(
			"obj Child { name: text, };\n\
			obj Example { child: Child?, };\n\
			var example: Example = Example {};\n\
			exists example.child.name"
		).unwrap();

		assert_eq!(result, Some(Value::Boolean(false)));
	}

	#[test]
	fn exists_returns_true_for_present_object_field_with_null_value() {
		let result = evaluate_snippet(
			"obj Example { value: text?, };\nvar example: Example = Example {};\nexists example.value"
		).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn exists_returns_true_for_recursively_defaulted_object_field() {
		let result = evaluate_snippet(
			"obj Child { name: text, };\n\
			obj Example { child: Child, };\n\
			var example: Example = Example {};\n\
			exists example.child.name"
		).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn formats_array_index_type_error_with_line_and_column() {
		let source = standalone_body("var xs: [int] = [1, 2];\nreturn xs['1'];");
		let error = run(&source).unwrap_err();

		assert_eq!(
			error.format_with_source(&source),
			"Compile error in <source>:3:11: Array index must be of type `int`, found `text`."
		);
	}

	#[test]
	fn formats_break_outside_loop_compile_error_with_line_and_column() {
		let source = "break;\n";
		let error = evaluate_snippet(source).unwrap_err();

		assert_eq!(
			error.format_with_source(source),
			"Compile error in <source>:1:1: `break` may only be used inside a `while` or `for` loop."
		);
	}

	#[test]
	fn formats_compile_error_with_line_and_column() {
		let source = standalone_body("var x: int = true;\nreturn x;");
		let error = run(&source).unwrap_err();

		assert_eq!(
			error.format_with_source(&source),
			"Compile error in <source>:2:14: Cannot assign a value of type `bool` to a variable of type `int`."
		);
	}

	#[test]
	fn formats_if_condition_compile_error_with_line_and_column() {
		let source = standalone_body("if 1 {\n}\nreturn 0;");
		let error = run(&source).unwrap_err();

		assert_eq!(
			error.format_with_source(&source),
			"Compile error in <source>:2:4: `if` condition must be of type `bool` or `record pointer`, found `int`."
		);
	}

	#[test]
	fn formats_imported_module_compile_errors_against_imported_file() {
		let root_path = write_test_source_file(
			"formats_imported_module_compile_errors_against_imported_file_root",
			"main.tablo",
			"use './Helpers';\nfn Main(args: [text]): int { return Helper(); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(&helper_path, "pub fn Helper(): int {\n\tvar value: int = 'oops';\n\treturn value;\n}").unwrap();
		let root_source = fs::read_to_string(&root_path).unwrap();

		let error = compile_source_to_program_with_name_and_schema(
			root_source.clone(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap_err();
		let formatted = error.format_with_source_name(&root_source, Some(root_path.to_str().unwrap()));

		assert!(formatted.contains(&helper_path.display().to_string()));
		assert!(formatted.contains("Compile error"));
		assert!(formatted.contains("Cannot assign a value of type `text` to a variable of type `int`."));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn formats_imported_module_parse_errors_against_imported_file() {
		let root_path = write_test_source_file(
			"formats_imported_module_parse_errors_against_imported_file_root",
			"main.tablo",
			"use './Helpers';\nfn Main(args: [text]): int { return Helper(); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(&helper_path, "pub fn Helper(): int {\n\treturn 1\n").unwrap();
		let root_source = fs::read_to_string(&root_path).unwrap();

		let error = compile_source_to_program_with_name_and_schema(
			root_source.clone(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap_err();
		let formatted = error.format_with_source_name(&root_source, Some(root_path.to_str().unwrap()));

		assert!(formatted.contains(&helper_path.display().to_string()));
		assert!(formatted.contains("Parse error"));
		assert!(formatted.contains("Expected `;` after return statement."));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn formats_missing_function_return_error_with_line_and_column() {
		let source = "fn Main(args: [text]): int { return add(1, 2); }\nfn add(a: int, b: int): int {\n  a + b;\n}";
		let error = run(source).unwrap_err();

		assert_eq!(
			error.format_with_source(source),
			"Compile error in <source>:2:1: Function `add` must return a value of type `int` on all paths."
		);
	}

	#[test]
	fn formats_runtime_stack_trace_with_function_names() {
		let source = "fn inner(): int {\n  var xs: [int] = [1];\n  return xs[2];\n}\nfn outer(): int {\n  return inner();\n}\nouter()";
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
			"Parse error in <source>:2:1: Expected `:` after ternary true branch."
		);
	}

	#[test]
	fn formats_while_condition_compile_error_with_line_and_column() {
		let source = standalone_body("while 1 {\n}\nreturn 0;");
		let error = run(&source).unwrap_err();

		assert_eq!(
			error.format_with_source(&source),
			"Compile error in <source>:2:7: `while` condition must be of type `bool`, found `int`."
		);
	}

	#[test]
	fn groups_for_record_loop_iterations_by_ordering_group_keys() {
		let database_path = create_sqlite_test_database(
			"groups_for_record_loop_iterations_by_ordering_group_keys",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Country TEXT NOT NULL,
					City TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Country, City) VALUES (30, 'US', 'New York');
				INSERT INTO Customers (Id, Country, City) VALUES (20, 'CA', 'Toronto');
				INSERT INTO Customers (Id, Country, City) VALUES (10, 'CA', 'Ottawa');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    var firstId: int = 0;\n    for rec cust in Customers group by Country as country, City {\n        firstId = cust.Id;\n        break;\n    }\n    return firstId;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Country text not null,
					City text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(10)));
	}

	#[test]
	fn ignores_return_types_and_default_contents_when_validating_overloads() {
		let error = run(
			"fn resolve(value: int = 1): int { return value; }\n\
			fn resolve(value: int = 2): text { return 'duplicate'; }\n\
			fn Main(args: [text]): int { return 0; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Function overload `resolve` duplicates an existing callable signature in this scope.",
		);
	}

	#[test]
	fn keeps_auto_created_record_after_later_runtime_error_without_transaction() {
		let database_path = create_sqlite_test_database(
			"keeps_auto_created_record_after_later_runtime_error_without_transaction",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    {\n        rec mut cust = new Customers;\n        cust.Id = 17;\n        cust.Name = 'Eli';\n    }\n    return 1 / 0;\n}",
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
		let result = run_program_with_database_config(&program, database_config);
		let connection = Connection::open(&database_path).unwrap();
		let inserted_count: i64 = connection.query_row(
			"SELECT COUNT(*) FROM Customers WHERE Id = 17 AND Name = 'Eli'",
			[],
			|row| row.get(0),
		).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert!(result.is_err());
		assert_eq!(inserted_count, 1);
	}

	#[test]
	fn limits_for_record_loop_iterations() {
		let database_path = create_sqlite_test_database(
			"limits_for_record_loop_iterations",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES (10, 'Ada');
				INSERT INTO Customers (Id, Name) VALUES (11, 'Bea');
				INSERT INTO Customers (Id, Name) VALUES (12, 'Cia');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    var total: int = 0;\n    var maxRows: int = 2;\n    for rec cust in Customers order by Id limit maxRows {\n        total += cust.Id;\n    }\n    return total;\n}",
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

		assert_eq!(result, Some(Value::Integer(21)));
	}

	#[test]
	fn locked_returns_false_for_sqlite_find_query() {
		let database_path = create_sqlite_test_database(
			"locked_returns_false_for_sqlite_find_query",
			r#"
				CREATE TABLE Customers (Id INTEGER NOT NULL);
				INSERT INTO Customers (Id) VALUES (1);
			"#,
		);
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			"with exampledb;\nlocked (find first customers where id == 1)",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null);
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
	fn merged_correlated_count_handles_empty_null_duplicate_and_multiple_row_results() {
		let database_path = create_sqlite_test_database(
			"merged_correlated_count_handles_empty_null_duplicate_and_multiple_row_results",
			r#"
				CREATE TABLE Parents (
					Id INTEGER NOT NULL,
					CorrelationKey INTEGER,
					ExpectedCount INTEGER NOT NULL
				);
				CREATE TABLE Children (
					CorrelationKey INTEGER
				);
				INSERT INTO Parents (Id, CorrelationKey, ExpectedCount) VALUES
					(1, NULL, 0),
					(2, 7, 3),
					(3, 7, 3),
					(4, 8, 0);
				INSERT INTO Children (CorrelationKey) VALUES
					(NULL),
					(NULL),
					(7),
					(7),
					(7),
					(9);
			"#,
		);
		let schema = schema_catalog_from_fixture_with_backends(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Parents (
					Id int not null,
					CorrelationKey int null,
					ExpectedCount int not null
				);
				create table Children (
					CorrelationKey int null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let source = concat!(
			"with exampledb;\n",
			"fn Main(args: [text]): int {\n",
			"  for rec parent in Parents order by Id {\n",
			"    var childCount: int = count Children where CorrelationKey == parent.CorrelationKey;\n",
			"    var checked: int = [0][childCount == parent.ExpectedCount ? 1 : 2];\n",
			"  }\n",
			"  for rec missing in Parents where Id < 0 {\n",
			"    var missingCount: int = count Children where CorrelationKey == missing.CorrelationKey;\n",
			"    var unreachable: int = [0][2];\n",
			"  }\n",
			"  return 0;\n",
			"}",
		);
		let (planned_program, planned_semantics) =
			compile_standalone_with_query_optimizations(source, &schema, true).unwrap();
		let (unoptimized_program, _) =
			compile_standalone_with_query_optimizations(source, &schema, false).unwrap();
		let merge_count = planned_semantics.query_plan().queries().iter()
			.filter(|query| matches!(query.execution, PlannedQueryExecution::MergeWith { .. }))
			.count();

		assert_eq!(merge_count, 2, "{:#?}", planned_semantics.query_plan().queries());
		assert_eq!(planned_program.queries().len(), 2);
		assert_eq!(unoptimized_program.queries().len(), 4);

		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let (planned_result, planned_query_execution_count) =
			run_program_with_database_config_and_query_count(&planned_program, database_config.clone());
		let (unoptimized_result, unoptimized_query_execution_count) =
			run_program_with_database_config_and_query_count(&unoptimized_program, database_config);
		let _ = std::fs::remove_file(&database_path);
		let planned_result = planned_result.unwrap();
		let unoptimized_result = unoptimized_result.unwrap();

		assert_eq!(planned_query_execution_count, 2);
		assert_eq!(unoptimized_query_execution_count, 6);
		assert_eq!(planned_result, Some(Value::Integer(0)));
		assert_eq!(planned_result, unoptimized_result);
	}

	#[test]
	fn merged_correlated_count_runs_in_grouped_limited_loop_inside_nested_transactions() {
		let database_path = create_sqlite_test_database(
			"merged_correlated_count_runs_in_grouped_limited_loop_inside_nested_transactions",
			r#"
				CREATE TABLE Parents (
					Id INTEGER NOT NULL,
					GroupKey TEXT NOT NULL,
					ExpectedCount INTEGER NOT NULL
				);
				CREATE TABLE Children (
					GroupKey TEXT NOT NULL
				);
				INSERT INTO Parents (Id, GroupKey, ExpectedCount) VALUES
					(1, 'A', 2),
					(2, 'A', 2),
					(3, 'B', 1),
					(4, 'C', 99);
				INSERT INTO Children (GroupKey) VALUES
					('A'),
					('A'),
					('B');
			"#,
		);
		let schema = schema_catalog_from_fixture_with_backends(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Parents (
					Id int not null,
					GroupKey text not null,
					ExpectedCount int not null
				);
				create table Children (
					GroupKey text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let source = concat!(
			"with exampledb;\n",
			"fn Main(args: [text]): int {\n",
			"  transaction {\n",
			"    transaction {\n",
			"      for rec parent in Parents group by GroupKey limit 3 {\n",
			"        var childCount: int = count Children where GroupKey == parent.GroupKey;\n",
			"        var checked: int = [0][childCount == parent.ExpectedCount ? 1 : 2];\n",
			"      }\n",
			"    }\n",
			"  }\n",
			"  return 0;\n",
			"}",
		);
		let (planned_program, planned_semantics) =
			compile_standalone_with_query_optimizations(source, &schema, true).unwrap();
		let (unoptimized_program, _) =
			compile_standalone_with_query_optimizations(source, &schema, false).unwrap();
		let merged_query = planned_semantics.query_plan().queries().iter()
			.find(|query| matches!(query.execution, PlannedQueryExecution::MergeWith { .. }))
			.unwrap_or_else(|| panic!("{:#?}", planned_semantics.query_plan().queries()));

		assert_eq!(merged_query.transaction_scopes.len(), 2);
		assert_eq!(planned_program.queries().len(), 1);
		assert_eq!(unoptimized_program.queries().len(), 2);

		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let (planned_result, planned_query_execution_count) =
			run_program_with_database_config_and_query_count(&planned_program, database_config.clone());
		let (unoptimized_result, unoptimized_query_execution_count) =
			run_program_with_database_config_and_query_count(&unoptimized_program, database_config);
		let _ = std::fs::remove_file(&database_path);
		let planned_result = planned_result.unwrap();
		let unoptimized_result = unoptimized_result.unwrap();

		assert_eq!(planned_query_execution_count, 1);
		assert_eq!(unoptimized_query_execution_count, 4);
		assert_eq!(planned_result, Some(Value::Integer(0)));
		assert_eq!(planned_result, unoptimized_result);
	}

	#[test]
	fn merged_correlated_count_supports_multi_level_query_nesting() {
		let database_path = create_sqlite_test_database(
			"merged_correlated_count_supports_multi_level_query_nesting",
			r#"
				CREATE TABLE Parents (
					Id INTEGER NOT NULL
				);
				CREATE TABLE Children (
					Id INTEGER NOT NULL,
					ParentId INTEGER NOT NULL,
					ExpectedCount INTEGER NOT NULL
				);
				CREATE TABLE Grandchildren (
					ChildId INTEGER NOT NULL
				);
				INSERT INTO Parents (Id) VALUES (1), (2), (3);
				INSERT INTO Children (Id, ParentId, ExpectedCount) VALUES
					(10, 1, 2),
					(11, 1, 0),
					(20, 2, 1);
				INSERT INTO Grandchildren (ChildId) VALUES
					(10),
					(10),
					(20);
			"#,
		);
		let schema = schema_catalog_from_fixture_with_backends(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Parents (
					Id int not null
				);
				create table Children (
					Id int not null,
					ParentId int not null,
					ExpectedCount int not null
				);
				create table Grandchildren (
					ChildId int not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let source = concat!(
			"with exampledb;\n",
			"fn Main(args: [text]): int {\n",
			"  for rec parent in Parents order by Id {\n",
			"    for rec child in Children where ParentId == parent.Id order by Id {\n",
			"      var grandchildCount: int = count Grandchildren where ChildId == child.Id;\n",
			"      var checked: int = [0][grandchildCount == child.ExpectedCount ? 1 : 2];\n",
			"    }\n",
			"  }\n",
			"  return 0;\n",
			"}",
		);
		let (planned_program, planned_semantics) =
			compile_standalone_with_query_optimizations(source, &schema, true).unwrap();
		let (unoptimized_program, _) =
			compile_standalone_with_query_optimizations(source, &schema, false).unwrap();
		let merged_count_query = planned_semantics.query_plan().queries().iter()
			.find(|query| matches!(query.execution, PlannedQueryExecution::MergeWith { .. }))
			.unwrap_or_else(|| panic!("{:#?}", planned_semantics.query_plan().queries()));
		let PlannedQueryExecution::MergeWith {
			query: enclosing_query_id,
		} = merged_count_query.execution else {
			unreachable!();
		};
		let enclosing_query = planned_semantics.query_plan().query(enclosing_query_id).unwrap();

		assert_eq!(enclosing_query.kind, PlannedQueryKind::ForRecord);
		assert!(enclosing_query.enclosing_query.is_some());
		assert_eq!(
			enclosing_query.execution.independent_reason(),
			Some(PlannedQueryIndependentReason::NoSupportedStrategy),
		);
		assert_eq!(planned_program.queries().len(), 2);
		assert_eq!(unoptimized_program.queries().len(), 3);

		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let (planned_result, planned_query_execution_count) =
			run_program_with_database_config_and_query_count(&planned_program, database_config.clone());
		let (unoptimized_result, unoptimized_query_execution_count) =
			run_program_with_database_config_and_query_count(&unoptimized_program, database_config);
		let _ = std::fs::remove_file(&database_path);
		let planned_result = planned_result.unwrap();
		let unoptimized_result = unoptimized_result.unwrap();

		assert_eq!(planned_query_execution_count, 4);
		assert_eq!(unoptimized_query_execution_count, 7);
		assert_eq!(planned_result, Some(Value::Integer(0)));
		assert_eq!(planned_result, unoptimized_result);
	}

	#[test]
	fn merges_compatible_overloads_imported_from_multiple_modules() {
		let root_path = write_test_source_file(
			"merges_compatible_overloads_imported_from_multiple_modules_root",
			"main.tablo",
			"use Convert from './IntegerHelpers';\n\
			use Convert from './TextHelpers';\n\
			fn Main(args: [text]): int { return Convert(2) + Convert('x'); }",
		);
		let integer_path = root_path.parent().unwrap().join("IntegerHelpers.tablo");
		let text_path = root_path.parent().unwrap().join("TextHelpers.tablo");
		fs::write(
			&integer_path,
			"pub fn Convert(value: int): int { return value; }",
		).unwrap();
		fs::write(
			&text_path,
			"pub fn Convert(value: text): int { return 10; }",
		).unwrap();

		let program = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap();
		let result = run_program(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(12)));

		let _ = fs::remove_file(integer_path);
		let _ = fs::remove_file(text_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn merges_local_and_imported_function_overloads() {
		let root_path = write_test_source_file(
			"merges_local_and_imported_function_overloads_root",
			"main.tablo",
			"use Convert from './Helpers';\n\
			fn Convert(value: int): int { return value; }\n\
			fn Main(args: [text]): int { return Convert(2) + Convert('x'); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"pub fn Convert(value: text): int { return 10; }",
		).unwrap();

		let program = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap();
		let result = run_program(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(12)));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn omits_synthetic_entry_frame_from_standalone_runtime_stack_trace() {
		let source = "fn inner(): int {\n  var xs: [int] = [1];\n  return xs[2];\n}\nfn Main(args: [text]): int {\n  return inner();\n}";
		let error = run(source).unwrap_err();

		assert_eq!(
			error.to_string(),
			"Runtime error: Array index 2 is out of bounds for length 1.\nStack trace:\n  at inner (<source>:3:12)\n  at Main (<source>:6:15)"
		);
	}

	#[test]
	fn planned_query_execution_matches_optimizations_disabled_baseline() {
		let database_path = create_sqlite_test_database(
			"planned_query_execution_matches_optimizations_disabled_baseline",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL
				);
				CREATE TABLE Orders (
					Id INTEGER NOT NULL,
					CustomerId INTEGER NOT NULL
				);
				INSERT INTO Customers (Id) VALUES (1), (2);
				INSERT INTO Orders (Id, CustomerId) VALUES
					(10, 1),
					(11, 1),
					(20, 2);
			"#,
		);
		let schema = schema_catalog_from_fixture_with_backends(
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
				create table Orders (
					Id int not null,
					CustomerId int not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let source = concat!(
			"with exampledb;\n",
			"fn Main(args: [text]): int {\n",
			"  for rec customer in Customers order by Id {\n",
			"    var orderCount: int = count Orders where CustomerId == customer.Id;\n",
			"    rec firstOrder = find first Orders where CustomerId == customer.Id order by Id;\n",
			"    for rec customerOrder in Orders where CustomerId == customer.Id order by Id {\n",
			"      return orderCount + customerOrder.Id;\n",
			"    }\n",
			"  }\n",
			"  return -1;\n",
			"}",
		);
		let (planned_program, planned_semantics) =
			compile_standalone_with_query_optimizations(source, &schema, true).unwrap();
		let (unoptimized_program, unoptimized_semantics) =
			compile_standalone_with_query_optimizations(source, &schema, false).unwrap();
		let planned_candidates = planned_semantics.query_plan().queries().iter()
			.filter(|query| !query.optimization_opportunities.is_empty())
			.collect::<Vec<_>>();

		assert_eq!(planned_candidates.len(), 3);
		assert!(planned_candidates.iter().all(|query| {
			let expected_reason = match query.kind {
				PlannedQueryKind::Count => PlannedQueryIndependentReason::SemanticEquivalenceNotProven,
				PlannedQueryKind::Find | PlannedQueryKind::ForRecord => {
					PlannedQueryIndependentReason::NoSupportedStrategy
				}
			};
			query.execution.independent_reason() == Some(expected_reason)
		}));
		assert!(unoptimized_semantics.query_plan().queries().iter().all(|query| {
			query.optimization_opportunities.is_empty()
				&& query.proven_optimization.is_none()
				&& query.execution.independent_reason() == Some(PlannedQueryIndependentReason::OptimizationsDisabled)
		}));

		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let planned_result = run_program_with_database_config(&planned_program, database_config.clone()).unwrap();
		let unoptimized_result = run_program_with_database_config(&unoptimized_program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(planned_result, Some(Value::Integer(12)));
		assert_eq!(unoptimized_result, planned_result);
	}

	#[test]
	fn prefers_declared_default_over_nullable_parameter_default() {
		let result = run(
			"fn Main(args: [text]): int { return inspect(); }\n\
			fn inspect(value: int? = 7): int { return value == 7 ? 1 : 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn preserves_debug_metadata_in_object_file() {
		let output_path = unique_test_output_path("preserves_debug_metadata_in_object_file");
		compile_with_source_name("fn Main(args: [text]): int {\nvar x: int = 1;\nreturn x;\n}", "example.tablo", &output_path).unwrap();
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
	fn preserves_evaluation_order_and_references_with_variadic_arguments() {
		let result = run(
			"fn next(counter: &int): int { counter += 1; return counter; }\n\
			fn store(target: &int, head: int, ...values: [int]) {\n\
			    target = head * 100 + values[1] * 10 + values[2];\n\
			}\n\
			fn Main(args: [text]): int {\n\
			    var counter: int = 0;\n\
			    var result: int = 0;\n\
			    store(target: &result, head: next(&counter), next(&counter), next(&counter));\n\
			    return result == 123 and counter == 3 ? 1 : 0;\n\
			}"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
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
	fn rejects_ambiguous_call_between_defaulted_overloads() {
		let error = run(
			"fn Main(args: [text]): int { return choose(); }\n\
			fn choose(value: int = 1): int { return value; }\n\
			fn choose(value: text = 'x'): int { return 2; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Call to function `choose` is ambiguous between the following overloads: `choose(value: int = default)`, `choose(value: text = default)`.",
		);
	}

	#[test]
	fn rejects_ambiguous_fixed_and_variadic_overload_call() {
		let error = run(
			"fn choose(value: int): int { return 1; }\n\
			fn choose(...values: [int]): int { return 2; }\n\
			fn Main(args: [text]): int { return choose(1); }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Call to function `choose` is ambiguous between the following overloads: `choose(value: int)`, `choose(...values: [int])`.",
		);
	}

	#[test]
	fn rejects_ambiguous_overload_call_introduced_by_nullable_compatibility() {
		let error = run(
			"fn choose(left: int): int { return 1; }\n\
			fn choose(right: int?): int { return 2; }\n\
			fn Main(args: [text]): int { return choose(1); }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Call to function `choose` is ambiguous between the following overloads: `choose(left: int)`, `choose(right: int?)`.",
		);
	}

	#[test]
	fn rejects_ambiguous_positional_overload_call() {
		let error = run(
			"fn Main(args: [text]): int { return choose(1); }\n\
			fn choose(left: int): int { return left; }\n\
			fn choose(right: int): int { return right; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Call to function `choose` is ambiguous between the following overloads: `choose(left: int)`, `choose(right: int)`.",
		);
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
		let error = run("fn Main(args: [text]): int { var value: int | text = 1; return value + 1; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Expected numeric operands, found `int | text` and `int`."),
			position: 69,
		}));
	}

	#[test]
	fn rejects_assigning_null_to_non_nullable_variable() {
		let error = run("fn Main(args: [text]): int { var value: text = null; return 0; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot assign a value of type `null` to a variable of type `text`."),
			position: 47,
		}));
	}

	#[test]
	fn rejects_assignment_from_any_to_specific_type() {
		let error = run("fn Main(args: [text]): int { var value: any = 1; var total: int = value; return total; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot assign a value of type `any` to a variable of type `int`."),
			position: 66,
		}));
	}

	#[test]
	fn rejects_assignment_of_non_member_type_to_union() {
		let error = run("fn Main(args: [text]): int { var value: int | text = true; return 0; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Cannot assign a value of type `bool` to a variable of type `int | text`."),
			position: 53,
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
	fn rejects_call_to_private_helper_from_imported_module() {
		let root_path = write_test_source_file(
			"rejects_call_to_private_helper_from_imported_module_root",
			"main.tablo",
			"use AddTwo from './Helpers';\nfn Main(args: [text]): int { return AddOne(5); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"pub fn AddTwo(value: int): int { return AddOne(value) + 1; }\nfn AddOne(value: int): int { return value + 1; }",
		).unwrap();

		let error = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Function `AddOne` is not declared in this scope."),
			position: 65,
		}));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn rejects_call_when_no_function_overload_matches() {
		let error = run(
			"fn Main(args: [text]): int { return choose(true); }\n\
			fn choose(value: int): int { return value; }\n\
			fn choose(value: text): int { return 0; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"No overload of function `choose` accepts the supplied arguments. Candidate `choose(value: int)` rejected argument for parameter `value`: expected `int`, found `bool`. Candidate `choose(value: text)` rejected argument for parameter `value`: expected `text`, found `bool`.",
		);
	}

	#[test]
	fn rejects_constant_passed_by_reference() {
		let error = run(
			"fn Main(args: [text]): int { const value: int = 1; bump(&value); return value; }\n\
			fn bump(value: &int) { value += 1; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(error.message, "Constant `value` cannot be passed by reference.");
	}

	#[test]
	fn rejects_contains_with_non_text_or_text_array_argument_source_text() {
		let error = evaluate_snippet("contains(1, 'x')").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from(
				"No overload of built-in function `contains` accepts the supplied arguments. Candidate `contains(str: text, sub: text)` rejected argument for parameter `str`: expected `text`, found `int`. Candidate `contains(arr: [text], elem: text)` rejected argument for parameter `arr`: expected `[text]`, found `int`."
			),
			position: 8,
		}));
	}

	#[test]
	fn rejects_countof_with_non_text_or_text_array_argument_source_text() {
		let error = evaluate_snippet("countof(1, 'x')").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from(
				"No overload of built-in function `countof` accepts the supplied arguments. Candidate `countof(str: text, arr: [text])` rejected argument for parameter `str`: expected `text`, found `int`. Candidate `countof(sub: text, str: text)` rejected argument for parameter `sub`: expected `text`, found `int`."
			),
			position: 7,
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
	fn rejects_default_parameter_expression_that_references_parameter() {
		let error = run(
			"fn Main(args: [text]): int { return calculate(2); }\n\
			fn calculate(left: int, right: int = left): int { return left + right; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"A default expression cannot directly reference a variable, constant, or parameter.",
		);
	}

	#[test]
	fn rejects_default_parameter_value_with_incompatible_type() {
		let error = run(
			"fn Main(args: [text]): int { return 0; }\n\
			fn calculate(value: int = 'wrong'): int { return value; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(error.message, "Cannot assign a value of type `text` to a variable of type `int`.");
	}

	#[test]
	fn rejects_defaults_on_all_by_reference_parameter_forms() {
		for (parameter, expected_name) in [
			("value: &int? = null", "value"),
			("record: &rec Customers = null", "record"),
			("sequence: &seq InvoiceNumber = 1", "sequence"),
		] {
			let error = run(format!(
				"fn Main(args: [text]): int {{ return 0; }}\nfn inspect({parameter}) {{}}"
			)).unwrap_err();

			let TabloError::Compile(error) = error else {
				panic!("Expected a compile error.");
			};
			assert_eq!(
				error.message,
				format!("By-reference parameter `{expected_name}` cannot define a default value."),
			);
		}
	}

	#[test]
	fn rejects_direct_name_captures_in_default_expressions() {
		for (source, expected_name) in [
			(
				"fn Invalid(base: int, value: int = base) {}\nfn Main(args: [text]): int { return 0; }",
				"base",
			),
			(
				"fn Main(args: [text]): int { var callerValue: int = 1; return Invalid(); }\nfn Invalid(value: int = callerValue): int { return value; }",
				"callerValue",
			),
			(
				"fn Main(args: [text]): int { var localValue: int = 1; fn Invalid(value: int = localValue) {} return 0; }",
				"localValue",
			),
			(
				"fn Main(args: [text]): int { const localValue: int = 1; fn Invalid(value: int = localValue) {} return 0; }",
				"localValue",
			),
		] {
			let error = run(source).unwrap_err();
			let TabloError::Compile(error) = error else {
				panic!("Expected a compile error.");
			};

			assert_eq!(
				error.message,
				"A default expression cannot directly reference a variable, constant, or parameter.",
			);
			assert_eq!(&source[error.position..error.position + expected_name.len()], expected_name);
		}
	}

	#[test]
	fn rejects_disallowed_default_expression_forms() {
		for source in [
			"fn Invalid(value: any = (target = 1)) {}\nfn Main(args: [text]): int { return 0; }",
			"fn Invalid(value: any = (1:2)) {}\nfn Main(args: [text]): int { return 0; }",
			"fn Invalid(value: int = count Customers) {}\nfn Main(args: [text]): int { return 0; }",
		] {
			let error = run(source).unwrap_err();
			let TabloError::Compile(error) = error else {
				panic!("Expected a compile error.");
			};

			assert_eq!(
				error.message,
				"This expression form is not permitted in a function parameter default.",
			);
		}
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
	fn rejects_duplicate_function_overload_signature() {
		let error = run(
			"fn Main(args: [text]): int { return 0; }\n\
			fn choose(value: int): int { return value; }\n\
			fn choose(value: int): text { return 'duplicate'; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Function overload `choose` duplicates an existing callable signature in this scope.",
		);
	}

	#[test]
	fn rejects_duplicate_named_and_positional_argument_binding() {
		let error = run(
			"fn Main(args: [text]): int { return subtract(9, left: 2); }\n\
			fn subtract(left: int, right: int): int { return left - right; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(error.message, "Arguments do not match the parameters of function `subtract`.");
	}

	#[test]
	fn rejects_enum_downcast_to_wrong_backing_type() {
		let error = run(
			"enum Color { Red, Blue }\nfn Main(args: [text]): int { var color: Color = Color.Red; var value: text = text(color); return 0; }"
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Built-in function `text` cannot cast enum `Color` because its backing type is `int`."),
			position: 106,
		}));
	}

	#[test]
	fn rejects_equality_comparison_on_any_values() {
		let error = run("fn Main(args: [text]): int { var left: any = 1; var right: any = 2; var same: bool = left == right; return 0; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Equality comparison is not supported between `any` and `any`."),
			position: 90,
		}));
	}

	#[test]
	fn rejects_fixed_overload_shadowed_by_variadic_parameter_without_call() {
		let error = run(
			"fn append(value: int) {}\n\
			fn append(value: int, ...others: [int]) {}\n\
			fn Main(args: [text]): int { return 0; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Function overload set `append` is invalid because the following overload cannot be selected uniquely: `append(value: int)`.",
		);
	}

	#[test]
	fn rejects_group_boundary_built_in_without_required_named_argument() {
		let error = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    for rec cust in Customers group by Country as country {\n        if firstof(v2: [country]) { return 1; }\n    }\n    return 0;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Country text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap_err();
		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};

		assert_eq!(
			error.message,
			"Arguments do not match the parameters of built-in function `firstof`.",
		);
	}

	#[test]
	fn rejects_if_rec_binding_used_inside_its_condition() {
		let error = compile_source_to_program_with_name_and_schema(
			"with exampledb;\nfn Main(args: [text]): int { if rec user = find first Customers where Id == 1 and user.Id == 1 { return user.Id; } return -1; }",
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
			error.format_with_source("with exampledb;\nfn Main(args: [text]): int { if rec user = find first Customers where Id == 1 and user.Id == 1 { return user.Id; } return -1; }"),
			"Compile error in <source>:2:83: Qualified field reference must use the target table name `Customers`."
		);
	}

	#[test]
	fn rejects_implicit_int_to_decimal_function_argument_conversion() {
		let error = run(
			"fn Main(args: [text]): int { accept(1); return 0; }\n\
			fn accept(value: dec) {}"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(error.message, "Cannot assign a value of type `int` to a variable of type `dec`.");
	}

	#[test]
	fn rejects_implicit_outer_variable_capture_in_nested_function_source_text() {
		let error = run(
			"fn Main(args: [text]): int { var x: int = 1; fn inner(): int { return x; } return inner(); }"
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
			message: String::from(
				"No overload of built-in function `indexof` accepts the supplied arguments. Candidate `indexof(str: text, arr: [text])` rejected argument for parameter `str`: expected `text`, found `int`. Candidate `indexof(sub: text, str: text)` rejected argument for parameter `sub`: expected `text`, found `int`."
			),
			position: 7,
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
	fn rejects_invalid_literal_temporal_format_string_at_compile_time() {
		let error = evaluate_snippet("format(@2026-06-20, 'YYYY-MM-DD hh:mm')").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Invalid temporal format string: Temporal format token `hh` is not valid when formatting a `date` value."),
			position: 31,
		}));
	}

	#[test]
	fn rejects_invalid_main_signature_source_text() {
		let error = run("fn Main(): int { return 0; }").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Entry-point function `Main` must have the exact signature `fn Main(args: [text]): int`."),
			position: 0,
		}));
	}

	#[test]
	fn rejects_invalid_named_literal_numeric_format_string_at_compile_time() {
		let source = "format(pattern: 'x.00', v: 12.0)";
		let error = evaluate_snippet(source).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Invalid numeric format string: Decimal numeric format strings must use `1` as the whole-digit marker."),
			position: source.find("'x.00'").unwrap(),
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
			message: String::from(
				"No overload of built-in function `len` accepts the supplied arguments. Candidate `len(v: [any])` rejected argument for parameter `v`: expected `[any]`, found `int`. Candidate `len(str: text)` rejected argument for parameter `str`: expected `text`, found `int`."
			),
			position: 3,
		}));
	}

	#[test]
	fn rejects_missing_by_reference_argument_source_text() {
		let error = run(
			"fn Main(args: [text]): int { var x: int = 1; bump(x); return x; }\nfn bump(value: &int) { value += 1; }"
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Parameter `value` must be passed by reference."),
			position: 50,
		}));
	}

	#[test]
	fn rejects_missing_imported_module_file() {
		let root_path = write_test_source_file(
			"rejects_missing_imported_module_file_root",
			"main.tablo",
			"use './Missing';\nfn Main(args: [text]): int { return 0; }",
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
			message: String::from("Standalone Tablo programs must define `fn Main(args: [text]): int`."),
			position: 0,
		}));
	}

	#[test]
	fn rejects_multiple_unselectable_overloads_in_three_function_set() {
		let error = run(
			"fn combine(value: int, left: int = 0) {}\n\
			fn combine(value: int, right: int = 0) {}\n\
			fn combine(value: int, left: int = 0, right: int = 0) {}\n\
			fn Main(args: [text]): int { return 0; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Function overload set `combine` is invalid because the following overloads cannot be selected uniquely: `combine(value: int, left: int = default)`, `combine(value: int, right: int = default)`.",
		);
	}

	#[test]
	fn rejects_mysql_sequence_references_during_compilation() {
		let error = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int { return seqnext(InvoiceNumber); }",
			r#"
				database ExampleDb;
				schema Reporting;
				create sequence InvoiceNumber;
			"#,
			&[("ExampleDb", DatabaseBackend::MySql)],
		).unwrap_err();

		match error {
			TabloError::Compile(error) => assert_eq!(
				error.message,
				"Sequence `InvoiceNumber` cannot be used because database `ExampleDb` uses MySQL, which does not support standalone sequences.",
			),
			other => panic!("Expected compile error, found {other:?}."),
		}
	}

	#[test]
	fn rejects_named_default_for_built_in_without_declared_default() {
		let source = "fn Main(args: [text]): int { return len(value: default); }";
		let error = run(source).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from(
				"`default` cannot be used when calling built-in function `len` because it has no declared parameter default."
			),
			position: source.find("default").unwrap(),
		}));
	}

	#[test]
	fn rejects_named_default_for_by_reference_parameter() {
		let source =
			"fn Main(args: [text]): int { return inspect(value: default); }\n\
			fn inspect(value: &int): int { return value; }";
		let error = run(source).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("`default` cannot be used for by-reference parameter `value`."),
			position: source.find("default").unwrap(),
		}));
	}

	#[test]
	fn rejects_named_default_for_parameter_without_declared_default() {
		let source =
			"fn Main(args: [text]): int { return inspect(value: default); }\n\
			fn inspect(value: int?): int { return value == null ? 1 : 0; }";
		let error = run(source).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from(
				"`default` can only be used for parameter `value` when it declares a default expression."
			),
			position: source.find("default").unwrap(),
		}));
	}

	#[test]
	fn rejects_named_scalar_for_variadic_parameter() {
		let error = run(
			"fn collect(...values: [int]): int { return len(values); }\n\
			fn Main(args: [text]): int { return collect(values: 1); }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(error.message, "Cannot assign a value of type `int` to a variable of type `[int]`.");
	}

	#[test]
	fn rejects_named_use_of_non_public_function() {
		let root_path = write_test_source_file(
			"rejects_named_use_of_non_public_function_root",
			"main.tablo",
			"use UsefulHelper from './Helpers';\nfn Main(args: [text]): int { return 0; }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(&helper_path, "fn UsefulHelper(): int { return 1; }").unwrap();

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
			"fn Main(args: [text]): int {\n\tuse './Helpers';\n\treturn 0;\n}",
		);

		let error = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Nested `use` declarations are not yet supported during module resolution."),
			position: 30,
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
	fn rejects_non_nullable_overload_shadowed_by_nullable_overload() {
		let error = run(
			"fn inspect(value: int) {}\n\
			fn inspect(value: int?) {}\n\
			fn Main(args: [text]): int { return 0; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Function overload set `inspect` is invalid because the following overload cannot be selected uniquely: `inspect(value: int)`.",
		);
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
			position: 40,
		}));
	}

	#[test]
	fn rejects_nullable_argument_for_non_nullable_parameter() {
		let error = run(
			"fn accept(value: int): int { return value; }\n\
			fn Main(args: [text]): int { var value: int? = 1; return accept(value); }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(error.message, "Cannot assign a value of type `int?` to a variable of type `int`.");
	}

	#[test]
	fn rejects_omitted_reference_to_nullable_value() {
		let error = run(
			"fn Main(args: [text]): int { return inspect(); }\nfn inspect(value: &text?): int { return value == null ? 1 : 0; }"
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Arguments do not match the parameters of function `inspect`."),
			position: 43,
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
	fn rejects_query_table_field_in_for_record_limit() {
		let error = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    for rec cust in Customers limit Id {}\n    return 0;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap_err();

		match error {
			TabloError::Compile(compile_error) => {
				assert_eq!(compile_error.message, "Variable `Id` is not declared in this scope.");
			}
			other => panic!("Expected compile error, found {other:?}."),
		}
	}

	#[test]
	fn rejects_scalar_named_group_boundary_variadic_argument() {
		let error = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    for rec cust in Customers group by Country as country, City {\n        if firstof(v1: country, v2: City) { return 1; }\n    }\n    return 0;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Country text not null,
					City text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap_err();
		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};

		assert_eq!(
			error.message,
			"Named variadic argument `v2` for built-in function `firstof` must be an array literal of grouping levels.",
		);
	}

	#[test]
	fn rejects_shorter_overload_shadowed_by_optional_parameter() {
		let error = run(
			"fn example(value: int) {}\n\
			fn example(value: int, precision: int = 0) {}\n\
			fn Main(args: [text]): int { return 0; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Function overload set `example` is invalid because the following overload cannot be selected uniquely: `example(value: int)`.",
		);
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
		let error = run("fn Main(args: [text]): int { return 0; }\n1 + 2").unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Top-level executable statements are not permitted when `Main` is defined."),
			position: 43,
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
	fn rejects_unknown_named_argument() {
		let error = run(
			"fn Main(args: [text]): int { return subtract(foo: 9, right: 2); }\n\
			fn subtract(left: int, right: int): int { return left - right; }"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(error.message, "Arguments do not match the parameters of function `subtract`.");
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
	fn rejects_unselectable_nested_function_overload() {
		let error = run(
			"fn Main(args: [text]): int {\n\
			    fn inspect(value: int) {}\n\
			    fn inspect(value: int?) {}\n\
			    return 0;\n\
			}"
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Function overload set `inspect` is invalid because the following overload cannot be selected uniquely: `inspect(value: int)`.",
		);
	}

	#[test]
	fn rejects_unselectable_overload_set_combined_by_imports() {
		let root_source =
			"use Inspect from './StrictHelpers';\n\
			use Inspect from './NullableHelpers';\n\
			fn Main(args: [text]): int { return 0; }";
		let root_path = write_test_source_file(
			"rejects_unselectable_overload_set_combined_by_imports_root",
			"main.tablo",
			root_source,
		);
		let strict_path = root_path.parent().unwrap().join("StrictHelpers.tablo");
		let nullable_path = root_path.parent().unwrap().join("NullableHelpers.tablo");
		fs::write(
			&strict_path,
			"pub fn Inspect(value: int) {}",
		).unwrap();
		fs::write(
			&nullable_path,
			"pub fn Inspect(value: int?) {}",
		).unwrap();

		let error = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap_err();

		let TabloError::Compile(error) = error else {
			panic!("Expected a compile error.");
		};
		assert_eq!(
			error.message,
			"Function overload set `Inspect` is invalid because the following overload cannot be selected uniquely: `Inspect(value: int)`.",
		);
		assert_eq!(error.position, root_source.rfind("Inspect").unwrap());

		let _ = fs::remove_file(strict_path);
		let _ = fs::remove_file(nullable_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn rejects_use_without_source_file_path() {
		let error = compile_source_to_program_with_name_and_schema(
			"use './Helpers';\nfn Main(args: [text]): int { return 0; }",
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
				assert_eq!(compile_error.message, "Function `disp` is not supported in `sqlite` database query expressions.");
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
	fn reorders_named_arguments_for_built_in_function() {
		let result = evaluate_snippet("contains(sub: 'abl', str: 'Tablo')").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn reports_unsupported_postgresql_query_expression_at_its_source_position() {
		let source = "with exampledb;\ncount Customers where len(Name) > 0";
		let error = compile_snippet_with_schema_fixture_and_backends(
			source,
			r#"
				database ExampleDb;
				schema Public;
				create table Customers (
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::PostgreSql)],
		).unwrap_err();

		assert_eq!(error, TabloError::Compile(crate::compiler::CompileError {
			message: String::from("Function `len` is not supported in `postgresql` database query expressions."),
			position: source.find("len").unwrap(),
		}));
	}

	#[test]
	fn resolves_default_expression_calls_in_the_declaration_scope() {
		let result = run(
			"fn DefaultValue(): int { return 1; }\n\
			fn ReadDefault(value: int = DefaultValue()): int { return value; }\n\
			fn Main(args: [text]): int {\n\
			    fn DefaultValue(): int { return 2; }\n\
			    return ReadDefault();\n\
			}"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn resolves_fixed_and_variadic_overloads_without_preference() {
		let result = run(
			"fn choose(value: int, label: text): int { return 1; }\n\
			fn choose(value: int, ...others: [int]): int { return 2; }\n\
			fn Main(args: [text]): int {\n\
			    return choose(1, 'label') * 10 + choose(1, 2, 3);\n\
			}"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(12)));
	}

	#[test]
	fn resolves_imported_default_expression_calls_in_the_declaration_module() {
		let root_path = write_test_source_file(
			"resolves_imported_default_expression_calls_in_the_declaration_module",
			"main.tablo",
			"use ReadDefault from './Helpers';\n\
			fn DefaultValue(): int { return 9; }\n\
			fn Main(args: [text]): int { return ReadDefault(); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"fn DefaultValue(): int { return 7; }\n\
			pub fn ReadDefault(value: int = DefaultValue()): int { return value; }",
		).unwrap();
		let source = fs::read_to_string(&root_path).unwrap();
		let program = compile_source_to_program_with_name_and_schema(
			source,
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap();

		let result = run_program(&program).unwrap();

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn resolves_nested_default_expression_calls_in_the_declaration_scope() {
		let result = run(
			"fn Main(args: [text]): int { return Outer(); }\n\
			fn Outer(): int {\n\
			    fn DefaultValue(): int { return 5; }\n\
			    fn ReadDefault(value: int = DefaultValue()): int { return value; }\n\
			    return ReadDefault();\n\
			}"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(5)));
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
	fn rolls_back_auto_created_record_after_transaction_runtime_error() {
		let database_path = create_sqlite_test_database(
			"rolls_back_auto_created_record_after_transaction_runtime_error",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    transaction {\n        {\n            rec mut cust = new Customers;\n            cust.Id = 23;\n            cust.Name = 'Noor';\n        }\n        var fail: int = 1 / 0;\n    }\n    return 0;\n}",
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
		let result = run_program_with_database_config(&program, database_config);
		let connection = Connection::open(&database_path).unwrap();
		let inserted_count: i64 = connection.query_row(
			"SELECT COUNT(*) FROM Customers WHERE Id = 23 AND Name = 'Noor'",
			[],
			|row| row.get(0),
		).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert!(result.is_err());
		assert_eq!(inserted_count, 0);
	}

	#[test]
	fn rolls_back_explicit_delete_inside_transaction_after_runtime_error() {
		let database_path = create_sqlite_test_database(
			"rolls_back_explicit_delete_inside_transaction_after_runtime_error",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES (33, 'Ada');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    transaction {\n        rec mut cust = find first Customers where Id == 33;\n        delete cust;\n        var fail: int = 1 / 0;\n    }\n    return 0;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null primary key,
					Name text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config);
		let connection = Connection::open(&database_path).unwrap();
		let stored_count: i64 = connection.query_row(
			"SELECT COUNT(*) FROM Customers WHERE Id = 33 AND Name = 'Ada'",
			[],
			|row| row.get(0),
		).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert!(result.is_err());
		assert_eq!(stored_count, 1);
	}

	#[test]
	fn rolls_back_explicit_update_inside_transaction_after_runtime_error() {
		let database_path = create_sqlite_test_database(
			"rolls_back_explicit_update_inside_transaction_after_runtime_error",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES (9, 'Ada');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    transaction {\n        rec mut cust = find first Customers where Id == 9;\n        cust.Name = 'Noor';\n        update cust;\n        var fail: int = 1 / 0;\n    }\n    return 0;\n}",
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
		let result = run_program_with_database_config(&program, database_config);
		let connection = Connection::open(&database_path).unwrap();
		let stored_name: String = connection.query_row(
			"SELECT Name FROM Customers WHERE Id = 9",
			[],
			|row| row.get(0),
		).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert!(result.is_err());
		assert_eq!(stored_name, "Ada");
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
			"obj Outer { items: [{ value: int, }], };\nfn Main(args: [text]): int { var item: Outer.items.Element = Outer.items.Element { value: 7 }; var outer: Outer = Outer { items: [item] }; return outer.items[1].value; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_anonymous_inline_object_declaration_in_array_field_source_text() {
		let result = run(
			"obj Outer { items: [{ value: int, }], };\nfn Main(args: [text]): int { var item: Outer.items.Element = Outer.items.Element { value: 7 }; var outer: Outer = Outer { items: [item] }; return outer.items[1].value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_anonymous_inline_object_declaration_in_union_field_source_text() {
		let result = run(
			"obj Envelope { payload: text | { value: int, }, };\nfn Main(args: [text]): int { var payload: Envelope.payloadMember2 = Envelope.payloadMember2 { value: 7 }; var envelope: Envelope = Envelope { payload: payload }; return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_anonymous_inline_object_declaration_source_text() {
		let result = run(
			"obj Outer { inner: { value: int, }, };\nfn Main(args: [text]): int { var inner: Outer.inner = Outer.inner { value: 7 }; var outer: Outer = Outer { inner: inner }; return outer.inner.value; }"
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
			"fn Main(args: [text]): int {\n  var foo: [dec] = [];\n\n  for i in 2:20 {\n    foo[i / 2] = 0.75 * i;\n  }\n\n  return 0;\n}"
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
			"fn Main(args: [text]): int { var x: int = 1; var y: int = 5; copy(&x, &y); return x; }\nfn copy(dst: &int, src: &int) { dst = src; }"
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
			"enum Flag: bool { Off: false, On: true }\nfn Main(args: [text]): int { var flag: Flag = Flag.On; if (flag == Flag.On) { return 1; } return 0; }"
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
			"fn Main(args: [text]): int { var x: int = 1; bump(&x); return x; }\nfn bump(value: &int) { value += 1; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_call_with_array_default_value() {
		let result = run(
			"fn Main(args: [text]): int { return size(); }\n\
			fn size(values: [int] = [2, 3, 4]): int { return len(values); }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_call_with_default_expression() {
		let result = run(
			"fn Main(args: [text]): int { return calculate(); }\n\
			fn DefaultValue(): int { return 6; }\n\
			fn calculate(value: int = DefaultValue()): int { return value + 1; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_call_with_explicitly_requested_default() {
		let result = run(
			"fn Main(args: [text]): int { return inspect(value: default); }\n\
			fn inspect(value: int? = 7): int { return value == 7 ? 1 : 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_call_with_defaulted_parameter() {
		let result = run(
			"fn Main(args: [text]): int { return add(3); }\n\
			fn add(left: int, right: int = 4): int { return left + right; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_call_with_named_argument_after_omitted_defaults() {
		let result = run(
			"fn Main(args: [text]): int { return combine(third: 6); }\n\
			fn combine(firstValue: int = 1, secondValue: int = 2, third: int = 3): int {\n\
			    return firstValue * 100 + secondValue * 10 + third;\n\
			}"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(126)));
	}

	#[test]
	fn runs_call_with_omitted_defaults_after_object_round_trip() {
		let output_path = unique_test_output_path("runs_call_with_omitted_defaults_after_object_round_trip");
		compile(
			"fn Main(args: [text]): int { return combine(third: 6); }\n\
			fn combine(firstValue: int = 1, secondValue: int = 2, third: int = 3): int {\n\
			    return firstValue * 100 + secondValue * 10 + third;\n\
			}",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(126)));
	}

	#[test]
	fn runs_call_with_omitted_nullable_parameter() {
		let result = run(
			"fn Main(args: [text]): int { return isMissing(); }\n\
			fn isMissing(value: text?): int { return value == null ? 1 : 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
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
			"enum Holiday: date { NewYear: @2026-01-01, Christmas: @2026-12-25 }\nfn Main(args: [text]): int { var holiday: Holiday = Holiday.Christmas; if (holiday == Holiday.Christmas) { return 1; } return 0; }"
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
			"fn Main(args: [text]): int { const dateToday: date = @2026-06-14; const later: date = @2026-07-01; if dateToday > later { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_date_variable_without_initializer_with_current_date_default() {
		let current_date = crate::value::Date::current_local();
		let source = format!(
			"fn Main(args: [text]): int {{ var value: date; if value == @{:04}-{:02}-{:02} {{ return 1; }} return 0; }}",
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
			"enum Rate: dec { Reduced: 0.05, Standard: 0.20 }\nfn Main(args: [text]): int { var rate: Rate = Rate.Standard; if (rate == Rate.Standard) { return 1; } return 0; }"
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
		let result = run("fn Main(args: [text]): int { displn('hello'); return 0; }").unwrap();

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
			"enum Color { Red, Green: 3, Blue }\nfn Main(args: [text]): int { var color: Color = Color.Green; return int(color); }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_enum_downcast_to_text_backing_type() {
		let result = run(
			"enum Status: text { Pending: 'PENDING', Complete: 'COMPLETE' }\nfn Main(args: [text]): int { var status: Status = Status.Complete; if text(status) == 'COMPLETE' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_equality_source_text() {
		let result = evaluate_snippet("2 == 2.0").unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_exists_for_existing_sqlite_find_query() {
		let database_path = create_sqlite_test_database(
			"runs_exists_for_existing_sqlite_find_query",
			r#"
				CREATE TABLE Customers (Id INTEGER NOT NULL);
				INSERT INTO Customers (Id) VALUES (1);
			"#,
		);
		let (program, _) = compile_snippet_with_schema_fixture_and_backends(
			"with exampledb;\nexists (find first customers where id == 1)",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (Id int not null);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

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
			"with exampledb;\nexists (find first customers where id == 999)",
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
			"with exampledb;\nfn Main(args: [text]): int { rec c = find first Tbl where TableNum == 7 and contains(['ALPHA', 'CHARLIE'], Code) order by Id; if c { return c.Id; } return 0; }",
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
			"with exampledb;\nfn Main(args: [text]): int { rec cust = find first Customers where countof('na', Name) == 2 and indexof('Ba', Name) == 1; if cust { return cust.Id; } return 0; }",
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
			"with exampledb;\nfn Main(args: [text]): int { rec cust = find first Customers where contains(trim(Name), 'Ada'); if cust { return cust.Id; } return 0; }",
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
	fn runs_firstof_and_lastof_in_grouped_for_record_loop() {
		let database_path = create_sqlite_test_database(
			"runs_firstof_and_lastof_in_grouped_for_record_loop",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Country TEXT NOT NULL,
					City TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Country, City) VALUES (30, 'US', 'New York');
				INSERT INTO Customers (Id, Country, City) VALUES (20, 'CA', 'Toronto');
				INSERT INTO Customers (Id, Country, City) VALUES (10, 'CA', 'Ottawa');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    var firstCountries: int = 0;\n    var lastCountries: int = 0;\n    var firstCities: int = 0;\n    for rec cust in Customers group by Country as country, City {\n        if firstof(country) {\n            firstCountries += 1;\n        }\n        if lastof(country) {\n            lastCountries += 1;\n        }\n        if firstof(country, City) {\n            firstCities += 1;\n        }\n    }\n    return firstCountries * 100 + lastCountries * 10 + firstCities;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Country text not null,
					City text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(223)));
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
	fn runs_format_on_date_source_text() {
		let result = evaluate_snippet("format(@2026-06-20, 'YYYY-MM-DD')").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("2026-06-20"))));
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
	fn runs_format_on_time_source_text() {
		let result = evaluate_snippet("format(@15:04:09, 'hh:mm AM')").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("03:04 PM"))));
	}

	#[test]
	fn runs_format_on_timestamp_source_text() {
		let result = evaluate_snippet("format(@2026-06-20T15:04:09, 'WWW, D MMM YYYY')").unwrap();

		assert_eq!(result, Some(Value::Text(String::from("Sat, 20 Jun 2026"))));
	}

	#[test]
	fn runs_function_call_source_text() {
		let result = run("fn Main(args: [text]): int { return add(1, 2); }\nfn add(a: int, b: int): int { return a + b; }").unwrap();

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
			"with exampledb;\nfn ReadName(cust: rec Customers): text { return cust.Name; }\nfn Rename(cust: &rec Customers) { cust.Name = 'Ada Ltd'; }\nfn Main(args: [text]): int { rec mut cust = find first Customers where Id == 1; if cust { if ReadName(cust) == 'Ada' { Rename(&cust); if cust.Name == 'Ada Ltd' { return 1; } } } return 0; }",
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
		compile("fn Main(args: [text]): int { return add(1, 2); }\nfn add(a: int, b: int): int { return a + b; }", &output_path).unwrap();
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
			"with exampledb;\nfn Main(args: [text]): int { if rec user = find first Customers where Id == 1 { return user.Id; } else { return -1; } }",
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
			"with exampledb;\nfn Main(args: [text]): int { if rec user = find first Customers where Id == 1 { return user.Id; } return -1; }",
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
	fn runs_imported_function_with_default_expression() {
		let root_path = write_test_source_file(
			"runs_imported_function_with_default_expression_root",
			"main.tablo",
			"use Add from './Helpers';\nfn Main(args: [text]): int { return Add(); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"pub fn Add(value: int = DefaultValue()): int { return value + 1; }\n\
			fn DefaultValue(): int { return 6; }",
		).unwrap();

		let program = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap();
		let result = run_program(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn runs_imported_public_function_that_calls_private_helper() {
		let root_path = write_test_source_file(
			"runs_imported_public_function_that_calls_private_helper_root",
			"main.tablo",
			"use AddTwo from './Helpers';\nfn Main(args: [text]): int { return AddTwo(5); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"pub fn AddTwo(value: int): int { return AddOne(value) + 1; }\nfn AddOne(value: int): int { return value + 1; }",
		).unwrap();

		let program = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap();
		let result = run_program(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
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
			"enum Color { Red, Green: 3, Blue }\nfn Main(args: [text]): int { var color: Color; if (color == Color.Red) { color = Color.Blue; } if (color == Color.Blue) { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_int_cast_from_bool_source_text() {
		let result = evaluate_snippet("[int(false), int(true)]").unwrap();

		assert_eq!(
			result,
			Some(Value::Array(vec![
				Value::Integer(0),
				Value::Integer(1),
			])),
		);
	}

	#[test]
	fn runs_int_cast_from_text_source_text() {
		let result = evaluate_snippet("int('42')").unwrap();

		assert_eq!(result, Some(Value::Integer(42)));
	}

	#[test]
	fn runs_int_cast_with_named_argument_source_text() {
		let result = evaluate_snippet("int(v: true)").unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
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
		let result = run("fn Main(args: [text]): int { var value: int; return value; }").unwrap();

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
		let result = run("fn Main(args: [text]): int { return 7; }").unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_mixed_positional_and_named_arguments() {
		let result = run(
			"fn Main(args: [text]): int { return subtract(9, right: 2); }\n\
			fn subtract(left: int, right: int): int { return left - right; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_named_and_mixed_group_boundary_built_in_arguments() {
		let database_path = create_sqlite_test_database(
			"runs_named_and_mixed_group_boundary_built_in_arguments",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Country TEXT NOT NULL,
					City TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Country, City) VALUES (30, 'US', 'New York');
				INSERT INTO Customers (Id, Country, City) VALUES (20, 'CA', 'Toronto');
				INSERT INTO Customers (Id, Country, City) VALUES (10, 'CA', 'Ottawa');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    var firstCountries: int = 0;\n    var lastCountries: int = 0;\n    var namedArrayCities: int = 0;\n    var trailingCities: int = 0;\n    for rec cust in Customers group by Country as country, City {\n        if firstof(v1: country) {\n            firstCountries += 1;\n        }\n        if lastof(v1: country) {\n            lastCountries += 1;\n        }\n        if firstof(v2: [City], v1: country) {\n            namedArrayCities += 1;\n        }\n        if lastof(v1: country, City) {\n            trailingCities += 1;\n        }\n    }\n    return firstCountries * 1000 + lastCountries * 100 + namedArrayCities * 10 + trailingCities;\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null,
					Country text not null,
					City text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(2233)));
	}

	#[test]
	fn runs_named_arguments_in_parameter_order() {
		let result = run(
			"fn Main(args: [text]): int { return subtract(right: 2, left: 9); }\n\
			fn subtract(left: int, right: int): int { return left - right; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_named_arguments_with_source_order_evaluation() {
		let result = run(
			"fn Main(args: [text]): int {\n\
			    var value: int = 0;\n\
			    return combine(later: next(&value), earlier: next(&value));\n\
			}\n\
			fn next(value: &int): int { value += 1; return value; }\n\
			fn combine(earlier: int, later: int): int { return earlier * 10 + later; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(21)));
	}

	#[test]
	fn runs_named_built_in_arguments_in_database_query() {
		let database_path = create_sqlite_test_database(
			"runs_named_built_in_arguments_in_database_query",
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
			"with exampledb;\nfn Main(args: [text]): int { rec cust = find first Customers where contains(sub: 'Ada', str: trim(str: Name)); if cust { return cust.Id; } return 0; }",
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
	fn runs_named_by_reference_arguments() {
		let result = run(
			"fn Main(args: [text]): int {\n\
			    var left: int = 1;\n\
			    var right: int = 2;\n\
			    swap(right: &right, left: &left);\n\
			    return left * 10 + right;\n\
			}\n\
			fn swap(left: &int, right: &int) {\n\
			    var old_left: int = left;\n\
			    left = right;\n\
			    right = old_left;\n\
			}"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(21)));
	}

	#[test]
	fn runs_named_default_marker_inside_parameter_default_expression() {
		let result = run(
			"fn Main(args: [text]): int { return calculate(); }\n\
			fn Identity(value: int = 6): int { return value; }\n\
			fn calculate(value: int = Identity(value: default)): int { return value + 1; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_named_imported_function_overloads() {
		let root_path = write_test_source_file(
			"runs_named_imported_function_overloads_root",
			"main.tablo",
			"use Convert from './Helpers';\nfn Main(args: [text]): int { return Convert(value: 2) + Convert(value: 'x') + Convert(right: 4, left: 3); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"pub fn Convert(value: int): int { return value; }\n\
			pub fn Convert(value: text): int { return 10; }\n\
			pub fn Convert(left: int, right: int): int { return left + right; }",
		).unwrap();

		let program = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap();
		let result = run_program(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(19)));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn runs_named_inline_object_declaration_in_array_field_object_file() {
		let output_path = unique_test_output_path("runs_named_inline_object_declaration_in_array_field_object_file");
		compile(
			"obj Outer { items: [obj Item { value: int, }], };\nfn Main(args: [text]): int { var item: Outer.Item = Outer.Item { value: 7 }; var outer: Outer = Outer { items: [item] }; return outer.items[1].value; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_named_inline_object_declaration_in_array_field_source_text() {
		let result = run(
			"obj Outer { items: [obj Item { value: int, }], };\nfn Main(args: [text]): int { var item: Outer.Item = Outer.Item { value: 7 }; var outer: Outer = Outer { items: [item] }; return outer.items[1].value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_named_inline_object_declaration_in_union_field_source_text() {
		let result = run(
			"obj Envelope { payload: text | obj Payload { value: int, }, };\nfn Main(args: [text]): int { var payload: Envelope.Payload = Envelope.Payload { value: 7 }; var envelope: Envelope = Envelope { payload: payload }; return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_named_inline_object_declaration_source_text() {
		let result = run(
			"obj Outer { inner: obj Inner { value: int, }, };\nfn Main(args: [text]): int { var inner: Outer.Inner = Outer.Inner { value: 7 }; var outer: Outer = Outer { inner: inner }; return outer.inner.value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(7)));
	}

	#[test]
	fn runs_nested_by_reference_function_call_source_text() {
		let result = run(
			"fn Main(args: [text]): int { var x: int = 1; fn bump(value: &int) { value += 1; } bump(&x); return x; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_nested_function_call_before_declaration_source_text() {
		let result = run(
			"fn Main(args: [text]): int { return add(1, 2); fn add(a: int, b: int): int { return a + b; } }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_nested_function_call_source_text() {
		let result = run(
			"fn Main(args: [text]): int { fn add(a: int, b: int): int { return a + b; } return add(1, 2); }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_nested_function_overloads_that_shadow_outer_functions() {
		let result = run(
			"fn choose(value: int): int { return 100; }\n\
			fn Main(args: [text]): int {\n\
				fn choose(value: bool): int { return 1; }\n\
				fn choose(value: text): int { return 2; }\n\
				return choose(true) + choose('x');\n\
			}"
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
			"obj Address { line1: text = 'Unknown', };\nobj Person { name: text = '', address: Address, };\nfn Main(args: [text]): int { var person: Person = Person { name: 'Alice', address: Address { } }; person.address.line1 = 'Updated'; if person.address.line1 == 'Updated' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_nested_ternary_with_nullable_text_guarded_by_null_check() {
		let result = run(
			"fn Main(args: [text]): int { var val1: text = ''; var val2: text? = 'Foo'; var target: text = val1 != '' ? val1 : val2 != null ? val2 : ''; if target == 'Foo' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_no_return_function_as_expression_statement() {
		let result = run("fn Main(args: [text]): int { var x: int = 1; bump(x); return x; }\nfn bump(value: int) { return; }").unwrap();

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
			"with exampledb;\nfn Main(args: [text]): int { rec user = find first Customers where Id == 1; if not user { return -1; } return 1; }",
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
			"fn Main(args: [text]): int { var value: text? = null; if value == null { return 1; } return 0; }"
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
			"fn Main(args: [text]): int { var value: date? = @2026-06-14; if value == null { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_nullable_date_comparison_with_null_when_value_is_null() {
		let result = run(
			"fn Main(args: [text]): int { var value: date? = null; if value == null { return 1; } return 0; }"
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
			"obj Person { name: text = 'Anonymous', age: int, };\nfn Main(args: [text]): int { var person: Person = Person { age: 30 }; if person.name == 'Anonymous' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_object_field_access_source_text() {
		let result = run(
			"obj Person { name: text = '', age: int, };\nfn Main(args: [text]): int { var person: Person = Person { age: 30 }; return person.age; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(30)));
	}

	#[test]
	fn runs_object_field_assignment_object_file() {
		let output_path = unique_test_output_path("runs_object_field_assignment_object_file");
		compile(
			"obj Counter { value: int = 0, };\nfn Main(args: [text]): int { var counter: Counter = Counter { }; counter.value += 2; return counter.value; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_object_field_compound_assignment_source_text() {
		let result = run(
			"obj Counter { value: int = 0, };\nfn Main(args: [text]): int { var counter: Counter = Counter { }; counter.value += 2; return counter.value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_object_field_without_explicit_default_using_default() {
		let result = run(
			"obj Counter { value: int, };\nfn Main(args: [text]): int { var counter: Counter = Counter { }; return counter.value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_object_file() {
		let output_path = unique_test_output_path("runs_object_file");
		compile("fn Main(args: [text]): int { return 8 / 2; }", &output_path).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(4)));
	}

	#[test]
	fn runs_object_object_file() {
		let output_path = unique_test_output_path("runs_object_object_file");
		compile(
			"obj Person { name: text = '', age: int, };\nfn Main(args: [text]): int { var person: Person = Person { age: 30 }; return person.age; }",
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
	fn runs_overloaded_function_calls_selected_by_arity_and_type() {
		let result = run(
			"fn Main(args: [text]): int { return identify(1) + identify('x') + add(2, 3); }\n\
			fn identify(value: int): int { return 1; }\n\
			fn identify(value: text): int { return 10; }\n\
			fn add(value: int): int { return value; }\n\
			fn add(left: int, right: int): int { return left + right; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(16)));
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
			"fn Main(args: [text]): int { var \"return\": int = 1; return \"return\"; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_read_of_quoted_sqlite_sequence_name() {
		let database_path = create_sqlite_test_database(
			"runs_read_of_quoted_sqlite_sequence_name",
			r#"
				CREATE TABLE TempSeqTable (
					Id INTEGER PRIMARY KEY AUTOINCREMENT,
					Name TEXT NOT NULL
				);
				INSERT INTO TempSeqTable (Name) VALUES ('First');
				UPDATE sqlite_sequence SET name = 'temp-seq' WHERE name = 'TempSeqTable';
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int { return \"temp-seq\"; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table TempSeqTable (
					Id int not null,
					Name text not null
				);
				create sequence "temp-seq";
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
			"obj CustomerCollection [{ name: text, }];\nfn Main(args: [text]): int { var customers: CustomerCollection = [CustomerCollection.Element { name: 'Alice' }]; if customers[1].name == 'Alice' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_root_array_shaped_object_declaration_with_named_element_object_file() {
		let output_path = unique_test_output_path("runs_root_array_shaped_object_declaration_with_named_element_object_file");
		compile(
			"obj CustomerCollection [obj Customer { name: text, }];\nfn Main(args: [text]): int { var customers: CustomerCollection = [CustomerCollection.Customer { name: 'Alice' }]; if customers[1].name == 'Alice' { return 1; } return 0; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_root_array_shaped_object_declaration_with_named_element_source_text() {
		let result = run(
			"obj CustomerCollection [obj Customer { name: text, }];\nfn Main(args: [text]): int { var customers: CustomerCollection = [CustomerCollection.Customer { name: 'Alice' }]; if customers[1].name == 'Alice' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_seqnext_on_quoted_sqlite_sequence_name() {
		let database_path = create_sqlite_test_database(
			"runs_seqnext_on_quoted_sqlite_sequence_name",
			r#"
				CREATE TABLE TempSeqTable (
					Id INTEGER PRIMARY KEY AUTOINCREMENT,
					Name TEXT NOT NULL
				);
				INSERT INTO TempSeqTable (Name) VALUES ('First');
				UPDATE sqlite_sequence SET name = 'temp-seq' WHERE name = 'TempSeqTable';
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int { return seqnext(\"temp-seq\"); }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table TempSeqTable (
					Id int not null,
					Name text not null
				);
				create sequence "temp-seq";
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
	fn runs_seqnext_on_sqlite_sequence() {
		let database_path = create_sqlite_test_database(
			"runs_seqnext_on_sqlite_sequence",
			r#"
				CREATE TABLE InvoiceNumber (
					Id INTEGER PRIMARY KEY AUTOINCREMENT,
					Name TEXT NOT NULL
				);
				INSERT INTO InvoiceNumber (Name) VALUES ('First');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int { return seqnext(InvoiceNumber); }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table InvoiceNumber (
					Id int not null,
					Name text not null
				);
				create sequence InvoiceNumber;
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
	fn runs_seqnext_when_sqlite_sequence_value_is_stored_as_text() {
		let database_path = create_sqlite_test_database(
			"runs_seqnext_when_sqlite_sequence_value_is_stored_as_text",
			r#"
				CREATE TABLE TempSeqTable (
					Id INTEGER PRIMARY KEY AUTOINCREMENT,
					Name TEXT NOT NULL
				);
				INSERT INTO TempSeqTable (Name) VALUES ('First');
				UPDATE sqlite_sequence
					SET name = 'temp-seq',
						seq = CAST('0' AS TEXT)
					WHERE name = 'TempSeqTable';
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int { return seqnext(\"temp-seq\"); }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table TempSeqTable (
					Id int not null,
					Name text not null
				);
				create sequence "temp-seq";
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
	fn runs_seqnext_with_named_sequence_argument() {
		let database_path = create_sqlite_test_database(
			"runs_seqnext_with_named_sequence_argument",
			r#"
				CREATE TABLE InvoiceNumber (
					Id INTEGER PRIMARY KEY AUTOINCREMENT,
					Name TEXT NOT NULL
				);
				INSERT INTO InvoiceNumber (Name) VALUES ('First');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int { return seqnext(s: InvoiceNumber); }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table InvoiceNumber (
					Id int not null,
					Name text not null
				);
				create sequence InvoiceNumber;
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
			"with exampledb;\nfn Main(args: [text]): int { var targetId: int = 2; return count customers where id == targetId; }",
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
			"with exampledb;\nfn Main(args: [text]): int { return (find last customers where active == true order by id).id; }",
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
			"with exampledb;\nfn Main(args: [text]): int { rec cust = find first customers where active == true order by id; if cust.name == 'Ada' { return cust.id; } return 0; }",
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
			"with exampledb;\nfn Main(args: [text]): int {\n    rec outer = find first OuterTable where Id == 2;\n    if outer {\n        rec inner = find first InnerTable where InnerTable.Id == outer.Id;\n        if inner {\n            return inner.Id;\n        }\n    }\n    return 0;\n}",
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
			"with test;\nfn Main(args: [text]): int { rec temp = find first Customer; return temp.Id; }",
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
			"with exampledb;\nfn Main(args: [text]): int { var total: int = 0; for rec cust in customers where active == true order by id { total += cust.id; } return total; }",
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
	fn runs_sqlite_sequence_assignment() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_sequence_assignment",
			r#"
				CREATE TABLE InvoiceNumber (
					Id INTEGER PRIMARY KEY AUTOINCREMENT,
					Name TEXT NOT NULL
				);
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int { InvoiceNumber = 10; return InvoiceNumber; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table InvoiceNumber (
					Id int not null,
					Name text not null
				);
				create sequence InvoiceNumber;
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(10)));
	}

	#[test]
	fn runs_sqlite_sequence_read_when_table_and_sequence_share_a_name() {
		let database_path = create_sqlite_test_database(
			"runs_sqlite_sequence_read_when_table_and_sequence_share_a_name",
			r#"
				CREATE TABLE InvoiceNumber (
					Id INTEGER PRIMARY KEY AUTOINCREMENT,
					Name TEXT NOT NULL
				);
				INSERT INTO InvoiceNumber (Name) VALUES ('First');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int { return InvoiceNumber; }",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table InvoiceNumber (
					Id int not null,
					Name text not null
				);
				create sequence InvoiceNumber;
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
	fn runs_ternary_with_nullable_object_field_guarded_by_null_check() {
		let result = run(
			"obj Config { TestDate: date?, };\nfn Main(args: [text]): int { var config: Config = Config { TestDate: @2026-06-14 }; const today: date = @2026-06-20; const testDate: date = config.TestDate != null ? config.TestDate : today; if testDate == @2026-06-14 { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_text_backed_enum_source_text() {
		let result = run(
			"enum Status: text { Pending: 'PENDING', Complete: 'COMPLETE' }\nfn Main(args: [text]): int { var status: Status = Status.Complete; if (status == Status.Complete) { return 1; } return 0; }"
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
			"fn Main(args: [text]): int { const earlier: timestamp = @2026-06-14T12:00:00; const later: timestamp = @2026-07-01T00:00:00; if earlier > later { return 1; } return 0; }"
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
			"obj Envelope { payload: int | text = 1, };\nfn Main(args: [text]): int { var value: int | text = 'hello'; var env: Envelope = Envelope { payload: value }; return 0; }",
			&output_path,
		).unwrap();
		let result = run_file(&output_path).unwrap();
		let _ = std::fs::remove_file(&output_path);

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_union_typed_variable_and_object_field_source_text() {
		let result = run(
			"obj Envelope { payload: int | text = 1, };\nfn Main(args: [text]): int { var value: int | text = 1; var env: Envelope = Envelope { }; return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(0)));
	}

	#[test]
	fn runs_variadic_calls_with_trailing_and_named_array_arguments() {
		let result = run(
			"fn summarize(head: int = 1, ...values: [int]): int {\n\
			    return head * 1000\n\
			        + len(values) * 100\n\
			        + (len(values) > 0 ? values[1] * 10 : 0)\n\
			        + (len(values) > 1 ? values[2] : 0);\n\
			}\n\
			fn Main(args: [text]): int {\n\
			    const empty: int = summarize(4);\n\
			    const positional: int = summarize(4, 2, 3);\n\
			    const afterNamed: int = summarize(head: 5, 6, 7);\n\
			    const namedArray: int = summarize(head: 8, values: [9, 1]);\n\
			    const omittedFixed: int = summarize(values: [7, 6]);\n\
			    const requestedDefault: int = summarize(head: default, 4, 5);\n\
			    return empty == 4000\n\
			        and positional == 4223\n\
			        and afterNamed == 5267\n\
			        and namedArray == 8291\n\
			        and omittedFixed == 1276\n\
			        and requestedDefault == 1245 ? 1 : 0;\n\
			}"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn runs_while_source_text() {
		let result = run(standalone_body("var x: int = 0;\nwhile x < 3 { x += 1; }\nreturn x;")).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_wildcard_imported_function_overloads() {
		let root_path = write_test_source_file(
			"runs_wildcard_imported_function_overloads_root",
			"main.tablo",
			"use './Helpers';\nfn Main(args: [text]): int { return Convert(value: 2) + Convert(value: 'x'); }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"pub fn Convert(value: int): int { return value; }\n\
			pub fn Convert(value: text): int { return 10; }",
		).unwrap();

		let program = compile_source_to_program_with_name_and_schema(
			fs::read_to_string(&root_path).unwrap(),
			Some(root_path.to_str().unwrap()),
			CompilationTarget::Standalone,
			None,
		).unwrap();
		let result = run_program(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(12)));

		let _ = fs::remove_file(helper_path);
		let _ = fs::remove_file(&root_path);
		let _ = fs::remove_dir(root_path.parent().unwrap());
	}

	#[test]
	fn selects_function_overload_by_named_argument() {
		let result = run(
			"fn Main(args: [text]): int { return choose(right: 1); }\n\
			fn choose(left: int): int { return 10; }\n\
			fn choose(right: int): int { return 20; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(20)));
	}

	#[test]
	fn selects_function_overloads_by_reference_mode() {
		let result = run(
			"fn Main(args: [text]): int { var value: int = 2; return inspect(value) + inspect(&value); }\n\
			fn inspect(value: int): int { return value; }\n\
			fn inspect(value: &int): int { value += 1; return value; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(5)));
	}

	#[test]
	fn selects_int_overload_without_implicit_decimal_argument_conversion() {
		let result = run(
			"fn Main(args: [text]): int { return identify(1); }\n\
			fn identify(value: int): int { return 1; }\n\
			fn identify(value: dec): int { return 10; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn selects_nullable_overload_for_nullable_argument() {
		let result = run(
			"fn choose(left: int): int { return 1; }\n\
			fn choose(right: int?): int { return 2; }\n\
			fn Main(args: [text]): int { var value: int? = null; return choose(value); }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn stringifies_enum_value_as_variant_name() {
		let result = run(
			"enum Color { Red, Green: 3, Blue }\nfn Main(args: [text]): int { var color: Color; color = Color.Blue; var message: text = 'Selected: ${ color }'; if message == 'Selected: Blue' { return 1; } return 0; }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn supplies_command_line_arguments_to_main() {
		let program = compile_to_program_with_name(
			"fn Main(args: [text]): int { if args[1] == 'first' and args[2] == 'second' { return len(args); } return -1; }",
			None,
		).unwrap();
		let arguments = vec![String::from("first"), String::from("second")];

		let result = run_program_with_arguments(&program, &arguments).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn supplies_non_nullable_argument_to_nullable_parameter() {
		let result = run(
			"fn accept(value: int?): int { return value != null ? value : 0; }\n\
			fn Main(args: [text]): int { return accept(3); }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn updates_sqlite_record_from_mutable_record_pointer() {
		let database_path = create_sqlite_test_database(
			"updates_sqlite_record_from_mutable_record_pointer",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name) VALUES (7, 'Ada');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    rec mut cust = find first Customers where Id == 7;\n    cust.Name = 'Grace';\n    update cust;\n    return count Customers where Id == 7 and Name == 'Grace';\n}",
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
	fn updates_sqlite_record_using_primary_key_metadata() {
		let database_path = create_sqlite_test_database(
			"updates_sqlite_record_using_primary_key_metadata",
			r#"
				CREATE TABLE Customers (
					Id INTEGER NOT NULL,
					Name TEXT NOT NULL,
					Notes TEXT NOT NULL
				);
				INSERT INTO Customers (Id, Name, Notes) VALUES (12, 'Ada', 'Keep');
			"#,
		);
		let (program, _) = compile_standalone_with_schema_fixture_and_backends(
			"with exampledb;\nfn Main(args: [text]): int {\n    rec mut cust = find first Customers where Id == 12;\n    cust.Name = 'Grace';\n    update cust;\n    return count Customers where Id == 12 and Name == 'Grace' and Notes == 'Keep';\n}",
			r#"
				database ExampleDb;
				schema Main implicit;
				create table Customers (
					Id int not null primary key,
					Name text not null,
					Notes text not null
				);
			"#,
			&[("ExampleDb", DatabaseBackend::Sqlite)],
		).unwrap();
		let LoweredBackendQuery::Sql(find_query) = &program.queries()[0];
		let SqlQueryResultShape::RecordPointer(layout) = &find_query.result_shape else {
			panic!("Expected record-pointer query result metadata.");
		};
		assert_eq!(
			layout.selected_known_columns().unwrap().iter().map(|column| column.column_name.as_str()).collect::<Vec<_>>(),
			vec!["Id", "Name"],
		);
		let database_config = RuntimeDatabaseConfig::new()
			.with_sqlite_database("ExampleDb", &database_path);
		let result = run_program_with_database_config(&program, database_config).unwrap();
		let _ = std::fs::remove_file(&database_path);

		assert_eq!(result, Some(Value::Integer(1)));
	}

	#[test]
	fn uses_named_default_marker_for_overload_binding_without_a_type() {
		let result = run(
			"fn Main(args: [text]): int { return resolve(radix: default); }\n\
			fn resolve(radix: int = 10): int { return radix; }\n\
			fn resolve(prefix: text = ''): int { return len(prefix); }"
		).unwrap();

		assert_eq!(result, Some(Value::Integer(10)));
	}

	#[test]
	fn validates_named_use_against_public_functions_in_imported_module() {
		let root_path = write_test_source_file(
			"validates_named_use_against_public_functions_in_imported_module_root",
			"main.tablo",
			"use UsefulHelper from './Helpers';\nfn Main(args: [text]): int { return 0; }",
		);
		let helper_path = root_path.parent().unwrap().join("Helpers.tablo");
		fs::write(
			&helper_path,
			"pub fn UsefulHelper(): int { return 1; }\nfn HiddenHelper(): int { return 2; }",
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
