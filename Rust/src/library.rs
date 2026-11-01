use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::ast::{AstProgram, DataType, FunctionParameterType, Visibility};
use crate::bytecode::*;
use crate::compiler::CompileError;
use crate::source::SourceText;

// The interface is declaration-only Tablo syntax. Keeping it separate from the
// object payload allows the ordinary overload and argument rules to apply at
// compile time without shipping or recompiling function implementations.
#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct Manifest {
	compiler: String,
	interface: String,
	exports: Vec<u32>,
}

#[derive(Clone)]
pub(crate) struct Library {
	pub interface: String,
	pub exports: Vec<u32>,
	pub program: Program,
}

#[derive(Clone)]
pub(crate) struct LibraryImport {
	pub library: Library,
	pub bindings: Vec<(String, usize, u32)>,
}

impl Library {
	pub fn read(path: &Path) -> Result<Self, String> {
		let bytes = std::fs::read(path).map_err(|error| error.to_string())?;
		if bytes.get(..4) != Some(b"TLB1") {
			return Err(String::from("Invalid Tablo library header; expected TLB1."));
		}
		let length = bytes.get(4..8).ok_or("Truncated Tablo library header.")?;
		let end = 8usize.checked_add(u32::from_le_bytes(length.try_into().unwrap()) as usize)
			.ok_or("Invalid Tablo library manifest length.")?;
		let manifest: Manifest = serde_json::from_slice(bytes.get(8..end).ok_or("Truncated Tablo library manifest.")?)
			.map_err(|error| format!("Invalid Tablo library manifest: {error}"))?;
		if manifest.compiler != env!("CARGO_PKG_VERSION") {
			return Err(format!("Library requires Tablo {}; this compiler is {}. Rebuild the library.", manifest.compiler, env!("CARGO_PKG_VERSION")));
		}
		let program = crate::object_file::read_program(&bytes[end..]).map_err(|error| error.message)?;
		let library = Self { interface: manifest.interface, exports: manifest.exports, program };
		library.validate()?;
		Ok(library)
	}

	pub fn write(&self) -> Result<Vec<u8>, String> {
		self.validate()?;
		let manifest = serde_json::to_vec(&Manifest {
			compiler: env!("CARGO_PKG_VERSION").to_string(),
			interface: self.interface.clone(),
			exports: self.exports.clone(),
		}).map_err(|error| error.to_string())?;
		let length = u32::try_from(manifest.len()).map_err(|_| "Library interface is too large.")?;
		let mut bytes = b"TLB1".to_vec();
		bytes.extend_from_slice(&length.to_le_bytes());
		bytes.extend_from_slice(&manifest);
		bytes.extend_from_slice(&crate::object_file::write_program(&self.program));
		Ok(bytes)
	}

	fn validate(&self) -> Result<(), String> {
		let ast = crate::frontend::parse_source_text(&SourceText::new(self.interface.clone())).map_err(|error| error.to_string())?;
		validate_interface(&ast)?;
		if self.exports.len() != ast.functions.len() {
			return Err(String::from("Library export count does not match its interface."));
		}
		if self.program.entry_code().is_none_or(|body| !body.instructions.is_empty()) {
			return Err(String::from("A Tablo library cannot contain an executable entry point."));
		}
		if !self.program.object_type_descriptors().is_empty() {
			return Err(String::from("Object types in compiled libraries are not supported yet."));
		}
		for function in self.program.functions() {
			for instruction in &function.body().instructions {
				match instruction {
					Instruction::Call(index, _) if *index as usize >= self.program.functions().len() => return Err(String::from("Invalid library function reference.")),
					Instruction::ExecuteQuery(index) if *index as usize >= self.program.queries().len() => return Err(String::from("Invalid library query reference.")),
					Instruction::MakeObject { .. } => return Err(String::from("Object types in compiled libraries are not supported yet.")),
					_ => {},
				}
			}
		}
		for body in self.program.debug_info().code_bodies() {
			if body.source_file_index().is_some_and(|index| index as usize >= self.program.debug_info().source_files().len()) {
				return Err(String::from("Invalid library debug source reference."));
			}
		}
		let mut seen = std::collections::BTreeSet::new();
		for (declaration, index) in ast.functions.iter().zip(&self.exports) {
			let function = self.program.functions().get(*index as usize).ok_or("Invalid library export index.")?;
			if !seen.insert(index) || function.name() != Some(declaration.name.as_str()) || function.return_type() != declaration.return_type.as_ref() {
				return Err(String::from("Library export does not match its compiled function."));
			}
		}
		Ok(())
	}
}

pub(crate) fn validate_interface(program: &AstProgram) -> Result<(), String> {
	if program.functions.is_empty() || !program.objects.is_empty() || !program.statements.is_empty()
		|| program.result.is_some() || !program.with_declarations.is_empty() {
		return Err(String::from("A library interface must contain public function declarations only."));
	}
	fn supported(data_type: &DataType) -> bool {
		match data_type {
			DataType::Object(_) | DataType::RecordPointer(_) => false,
			DataType::Array(element) | DataType::Nullable(element) | DataType::Range(element) => supported(element),
			DataType::Union(members) => members.iter().all(supported),
			_ => true,
		}
	}
	for function in &program.functions {
		if function.visibility != Visibility::Public || !function.body.statements.is_empty() {
			return Err(String::from("Library interface functions must be public and have empty bodies."));
		}
		if function.return_type.as_ref().is_some_and(|data_type| !supported(data_type))
			|| function.parameters.iter().any(|parameter| match &parameter.data_type {
				FunctionParameterType::Value(data_type) => !supported(data_type),
				_ => true,
			}) {
			return Err(String::from("Library function interfaces do not yet support object, record, or sequence types."));
		}
	}
	let mut analyzer = crate::semantic::analyzer::SemanticAnalyzer::new();
	analyzer.set_external_functions(program.functions.iter().map(|function| function.name.clone()).collect());
	analyzer.analyze_program(program).map_err(|error| format!("Invalid library interface: {}", error.message))?;
	Ok(())
}

pub(crate) fn link(program: Program, declarations: &AstProgram, imports: &[LibraryImport]) -> Result<Program, CompileError> {
	if imports.is_empty() { return Ok(program); }
	let error = |message: &str| CompileError { message: message.into(), position: 0 };
	let declarations = crate::compiler::collect_all_function_declarations(declarations);
	let mut functions = program.functions().to_vec();
	let mut queries = program.queries().to_vec();
	let mut sources = program.debug_info().source_files().to_vec();
	let mut debug = program.debug_info().code_bodies()[..functions.len()].to_vec();
	let entry_debug = program.debug_info().code_bodies().get(functions.len()).cloned();
	for import in imports {
		let query_offset = u32::try_from(queries.len()).map_err(|_| error("Too many linked queries."))?;
		let source_offset = u32::try_from(sources.len()).map_err(|_| error("Too many linked source files."))?;
		let library = &import.library.program;
		let mut targets = vec![None; library.functions().len()];
		for (name, position, export) in &import.bindings {
			let index = declarations.iter().position(|function| function.name == *name && function.position == *position)
				.ok_or_else(|| error("Missing linked library declaration."))?;
			targets[*export as usize] = Some(index);
		}
		let mut next_index = functions.len();
		let targets = targets.into_iter().map(|target| {
			let index = target.unwrap_or_else(|| { let index = next_index; next_index += 1; index });
			u32::try_from(index).map_err(|_| error("Too many linked functions."))
		}).collect::<Result<Vec<_>, _>>()?;
		let mut relocated = Vec::new();
		let mut relocated_debug = Vec::new();
		for (index, function) in library.functions().iter().enumerate() {
			let mut body = function.body().clone();
			for instruction in &mut body.instructions {
				match instruction {
					Instruction::Call(target, _) => *target = targets[*target as usize],
					Instruction::ExecuteQuery(target) => *target = target.checked_add(query_offset).ok_or_else(|| error("Too many linked queries."))?,
					_ => {},
				}
			}
			relocated.push(CompiledFunction::new(function.name().map(String::from), function.return_type().cloned(), body));
			let body_debug = library.debug_info().code_bodies().get(index);
			relocated_debug.push(match body_debug {
				Some(info) => CodeBodyDebugInfo::new(info.body_name().map(String::from), info.instruction_positions().to_vec(), info.locals().to_vec(),
					info.source_file_index().map(|index| index.checked_add(source_offset).ok_or_else(|| error("Too many linked source files."))).transpose()?),
				None => CodeBodyDebugInfo::new(function.name().map(String::from), Vec::new(), Vec::new(), None),
			});
		}
		for ((function, body_debug), target) in relocated.into_iter().zip(relocated_debug).zip(targets) {
			let index = target as usize;
			if index < functions.len() {
				functions[index] = CompiledFunction::new(functions[index].name().map(String::from), function.return_type().cloned(), function.body().clone());
				debug[index] = body_debug;
			}
			else {
				functions.push(function);
				debug.push(body_debug);
			}
		}
		queries.extend_from_slice(library.queries());
		sources.extend_from_slice(library.debug_info().source_files());
	}
	if let Some(entry_debug) = entry_debug { debug.push(entry_debug); }
	let debug = DebugInfo::new(debug, sources);
	let linked = match program.entry_point() {
		EntryPoint::Function(index) => Program::from_entry_function_with_queries(program.constant_pool().clone(), *index, functions, queries, debug),
		EntryPoint::Code(body) => Program::from_parts_with_functions_queries_and_debug(program.constant_pool().clone(), body.clone(), functions, queries, debug),
	};
	Ok(linked.with_object_type_descriptors(program.object_type_descriptors().to_vec()))
}
