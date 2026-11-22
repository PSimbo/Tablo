use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use sha2::{Digest, Sha256};
use serde::{Deserialize, Serialize};

use crate::ast::{AstProgram, DataType, FunctionParameterType, IdentifierExpr, RecordPointerType, ResolvedSequenceReference, Visibility};
use crate::bytecode::*;
use crate::compiler::CompileError;
use crate::semantic::analyzer::SemanticProgram;
use crate::source::SourceText;

// The interface is declaration-only Tablo syntax. Keeping it separate from the
// object payload allows the ordinary overload and argument rules to apply at
// compile time without shipping or recompiling function implementations.
#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct Manifest {
	#[serde(default, skip_serializing_if = "TypeDependencies::is_empty")]
	dependencies: TypeDependencies,
	#[serde(default, skip_serializing_if = "String::is_empty")]
	module: String,
	compiler: String,
	interface: String,
	exports: Vec<u32>,
	#[serde(default, skip_serializing_if = "Vec::is_empty")]
	record_tables: Vec<RecordPointerType>,
	#[serde(default, skip_serializing_if = "Vec::is_empty")]
	parameter_sequences: Vec<ResolvedSequenceReference>,
	#[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
	objects: BTreeMap<String, u32>,
}

#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub(crate) struct TypeDependencies {
	// Source-facing names visible in this library's declarations.
	pub imports: BTreeMap<String, String>,
	pub modules: BTreeMap<String, TypeModule>,
	// Consumer-independent declaration names to object IDs in this payload.
	pub ids: BTreeMap<String, u32>,
}

impl TypeDependencies {
	fn is_empty(&self) -> bool {
		self.imports.is_empty() && self.modules.is_empty() && self.ids.is_empty()
	}
}

#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct TypeModule {
	pub label: String,
	pub declarations: String,
	pub imports: BTreeMap<String, String>,
}

pub(crate) fn object_name(owner: &str, name: &str) -> String {
	format!("__tablo_library_{owner}_{name}")
}

impl TypeModule {
	pub fn objects(&self, owner: &str) -> Result<Vec<crate::ast::ObjectDeclaration>, String> {
		if owner.len() != 64 || !owner.bytes().all(|byte| byte.is_ascii_hexdigit()) {
			return Err("Invalid compiled library type owner.".into());
		}

		let mut ast = crate::frontend::parse_source_text(&SourceText::new(self.declarations.clone())).map_err(|error| error.to_string())?;

		if !ast.statements.is_empty() || !ast.functions.is_empty() || !ast.with_declarations.is_empty() || ast.result.is_some() {
			return Err("Bundled type module must contain only object declarations.".into());
		}

		let mut names = self.imports.clone();

		for object in &ast.objects {
			names.insert(object.name.clone(), object_name(owner, &object.name));
		}

		crate::frontend::rewrite_object_declaration_type_names(&mut ast.objects, &names, true);

		Ok(ast.objects)
	}

	pub fn source_name(&self, owner: &str) -> String {
		format!("<compiled>/{owner}/{}", self.label)
	}
}

#[derive(Clone)]
pub(crate) struct Library {
	pub interface: String,
	pub module: String,
	pub owner: String,
	pub dependencies: TypeDependencies,
	pub exports: Vec<u32>,
	pub record_tables: Vec<RecordPointerType>,
	pub parameter_sequences: Vec<ResolvedSequenceReference>,
	pub objects: BTreeMap<String, u32>,
	pub program: Program,
}

#[derive(Clone)]
pub(crate) struct LibraryImport {
	pub object_names: BTreeMap<String, String>,
	pub own_module: TypeModule,
	pub library: Library,
	pub bindings: Vec<(String, usize, u32)>,
}

fn matches_database_reference(parts: &[IdentifierExpr], database: &str, schema: &str, name: &str) -> bool {
	if database.is_empty() || schema.is_empty() {
		return false;
	}

	match parts {
		[item] => item.name.eq_ignore_ascii_case(name),
		[qualifier, item] => item.name.eq_ignore_ascii_case(name)
			&& (qualifier.name.eq_ignore_ascii_case(database) || qualifier.name.eq_ignore_ascii_case(schema)),
		[db, namespace, item] => db.name.eq_ignore_ascii_case(database)
			&& namespace.name.eq_ignore_ascii_case(schema) && item.name.eq_ignore_ascii_case(name),
		_ => false,
	}
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
		let library = Self {
			interface: manifest.interface,
			module: manifest.module,
			owner: format!("{:x}", Sha256::digest(&bytes)),
			dependencies: manifest.dependencies,
			exports: manifest.exports,
			record_tables: manifest.record_tables,
			parameter_sequences: manifest.parameter_sequences,
			objects: manifest.objects,
			program,
		};

		library.validate()?;

		Ok(library)
	}

	pub fn write(&self) -> Result<Vec<u8>, String> {
		self.validate()?;

		let manifest = serde_json::to_vec(&Manifest {
			compiler: env!("CARGO_PKG_VERSION").to_string(),
			interface: self.interface.clone(),
			module: self.module.clone(),
			dependencies: self.dependencies.clone(),
			exports: self.exports.clone(),
			record_tables: self.record_tables.clone(),
			parameter_sequences: self.parameter_sequences.clone(),
			objects: self.objects.clone(),
		}).map_err(|error| error.to_string())?;

		let length = u32::try_from(manifest.len()).map_err(|_| "Library interface is too large.")?;
		let mut bytes = b"TLB1".to_vec();

		bytes.extend_from_slice(&length.to_le_bytes());
		bytes.extend_from_slice(&manifest);
		bytes.extend_from_slice(&crate::object_file::write_program(&self.program));

		Ok(bytes)
	}

	pub fn bind_database_parameters(&self, ast: &mut AstProgram) -> Result<(), String> {
		let mut records = self.record_tables.iter();
		let mut sequences = self.parameter_sequences.iter();

		for parameter in ast.functions.iter_mut().flat_map(|function| &mut function.parameters) {
			if let FunctionParameterType::RecordPointer { table, compiled_table, .. } = &mut parameter.data_type {
				let record = records.next().ok_or("Missing library record-parameter table.")?;

				if !matches_database_reference(&table.components, &record.database_name, &record.schema_name, &record.table_name) {
					return Err(format!("Invalid table identity for library record parameter `{}`.", parameter.name));
				}

				*compiled_table = Some(record.clone());
			}
			else if let FunctionParameterType::Sequence { sequence, compiled_sequence } = &mut parameter.data_type {
				let binding = sequences.next().ok_or("Missing library sequence-parameter binding.")?;

				if !matches_database_reference(&sequence.components, &binding.database_name, &binding.schema_name, &binding.sequence_name) {
					return Err(format!("Invalid sequence identity for library parameter `{}`.", parameter.name));
				}

				*compiled_sequence = Some(binding.clone());
			}
		}

		if sequences.next().is_some() {
			return Err("Unexpected library sequence-parameter binding.".into());
		}

		if records.next().is_some() {
			return Err("Unexpected library record-parameter table.".into());
		}

		Ok(())
	}

	fn validate(&self) -> Result<(), String> {
		let mut ast = crate::frontend::parse_source_text(&SourceText::new(self.interface.clone())).map_err(|error| error.to_string())?;

		if (ast.functions.is_empty() && !ast.objects.iter().any(|object| object.visibility == Visibility::Public))
			|| !ast.statements.is_empty()
			|| ast.result.is_some() || !ast.with_declarations.is_empty() {
			return Err(String::from("A library interface must contain object declarations or public function declarations."));
		}

		if self.objects.len() != ast.objects.len() {
			return Err("Library object declarations do not match the manifest.".into());
		}

		let mut object_ids = self.objects.clone();
		let mut sources = vec![String::from("<interface>"); ast.objects.len()];
		let mut displays: Vec<_> = ast.objects.iter().map(|object| object.name.clone()).collect();

		crate::frontend::rewrite_object_declaration_type_names(&mut ast.objects, &self.dependencies.imports, false);

		for function in &mut ast.functions {
			crate::frontend::rewrite_function_declaration_calls(function, &BTreeMap::new(), &BTreeMap::new(), &self.dependencies.imports);
		}

		for (owner, module) in &self.dependencies.modules {
			for object in module.objects(owner)? {
				let display = object.name.strip_prefix(&object_name(owner, "")).ok_or("Invalid bundled object name.")?;
				let id = self.dependencies.ids.get(&object.name).ok_or("Missing bundled object ID.")?;

				object_ids.insert(object.name.clone(), *id);
				displays.push(display.to_owned());
				sources.push(module.source_name(owner));
				ast.objects.push(object);
			}
		}
		if object_ids.len() != self.objects.len() + self.dependencies.ids.len()
			|| object_ids.values().any(|id| *id == ObjectTypeId::RUNTIME_ERROR.raw())
			|| object_ids.values().collect::<BTreeSet<_>>().len() != object_ids.len()
			|| self.dependencies.imports.values().any(|target| !self.dependencies.ids.contains_key(target)) {
			return Err("Invalid bundled object bindings.".into());
		}

		self.bind_database_parameters(&mut ast)?;

		let semantics = validate_interface(&ast, sources, displays)?;

		if self.exports.len() != ast.functions.len() {
			return Err(String::from("Library export count does not match its interface."));
		}

		if self.program.entry_code().is_none_or(|body| !body.instructions.is_empty()) {
			return Err(String::from("A Tablo library cannot contain an executable entry point."));
		}

		let mut ids = BTreeMap::from([(ObjectTypeId::RUNTIME_ERROR, ObjectTypeId::RUNTIME_ERROR)]);

		for object in &ast.objects {
			let id = object_ids.get(&object.name).ok_or("Missing library object binding.")?;
			ids.insert(semantics.object_type_id(&object.name).ok_or("Missing interface object.")?, ObjectTypeId::from_raw(*id));
		}

		for descriptor in semantics.object_type_descriptors() {
			if descriptor.id() == ObjectTypeId::RUNTIME_ERROR {
				continue;
			}

			let relocated = relocate_descriptor(descriptor.clone(), &ids)?;

			if self.program.object_type_descriptor(relocated.id()) != Some(&relocated) {
				return Err(format!("Library object `{}` does not match its compiled descriptor.", descriptor.display_name()));
			}
		}

		for function in self.program.functions() {
			for instruction in &function.body().instructions {
				match instruction {
					Instruction::Call(index, _) if *index as usize >= self.program.functions().len() => return Err(String::from("Invalid library function reference.")),
					Instruction::ExecuteQuery(index) if *index as usize >= self.program.queries().len() => return Err(String::from("Invalid library query reference.")),
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

fn validate_interface(program: &AstProgram, sources: Vec<String>, displays: Vec<String>) -> Result<SemanticProgram, String> {
	fn supported(data_type: &DataType) -> bool {
		match data_type {
			DataType::RecordPointer(_) => false,
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
				FunctionParameterType::RecordPointer { .. } => !parameter.is_by_ref,
				FunctionParameterType::Sequence { .. } => parameter.is_by_ref,
				FunctionParameterType::Value(data_type) => !supported(data_type),
			}) {

			return Err(String::from("Library function interfaces require `&rec` for record parameters and `seq` for sequence parameters."));
		}
	}

	let mut analyzer = crate::semantic::analyzer::SemanticAnalyzer::new();

	analyzer.set_root_source_name(Some(String::from("<interface>")));
	analyzer.set_top_level_object_source_names(sources);
	analyzer.set_top_level_object_display_names(displays);
	analyzer.set_external_functions(program.functions.iter().map(|function| function.name.clone()).collect());
	analyzer.analyze_program(program).map_err(|error| format!("Invalid library interface: {}", error.message))
}

type ObjectIds = BTreeMap<ObjectTypeId, ObjectTypeId>;

fn relocate_id(id: &mut ObjectTypeId, ids: &ObjectIds) -> Result<(), String> {
	*id = *ids.get(id).ok_or_else(|| format!("Unknown library object ID {}.", id.raw()))?;
	Ok(())
}

fn relocate_type(ty: &mut ObjectValueTypeDescriptor, ids: &ObjectIds) -> Result<(), String> {
	use ObjectValueTypeDescriptor as Type;

	match ty {
		Type::Object(id) => relocate_id(id, ids)?,
		Type::Array(inner) | Type::Nullable(inner) | Type::Range(inner) => relocate_type(inner, ids)?,
		Type::Union(types) => {
			for ty in types {
				relocate_type(ty, ids)?;
			}
		}
		_ => {}
	}

	Ok(())
}

fn relocate_default(value: &mut ObjectDefaultValue, ids: &ObjectIds) -> Result<(), String> {
	match value {
		ObjectDefaultValue::Array(values) => {
			for value in values {
				relocate_default(value, ids)?;
			}
		}
		ObjectDefaultValue::Object { object_type_id, fields } => {
			relocate_id(object_type_id, ids)?;

			for (_, value) in fields {
				relocate_default(value, ids)?;
			}
		}
		_ => {}
	}

	Ok(())
}

fn relocate_descriptor(mut descriptor: ObjectTypeDescriptor, ids: &ObjectIds) -> Result<ObjectTypeDescriptor, String> {
	relocate_id(&mut descriptor.id, ids)?;

	match &mut descriptor.shape {
		ObjectTypeDescriptorShape::RootArray(element) => relocate_type(element, ids)?,
		ObjectTypeDescriptorShape::Fields(fields) => {
			for field in fields {
				relocate_type(&mut field.data_type, ids)?;

				if let Some(value) = &mut field.explicit_default {
					relocate_default(value, ids)?;
				}
			}
		}
	}

	Ok(descriptor)
}

fn relocate_instruction(instruction: &mut Instruction, ids: &ObjectIds) -> Result<(), String> {
	match instruction {
		Instruction::MakeObject { object_type_id, .. } => relocate_id(object_type_id, ids)?,
		Instruction::ConvertObject { object_type_id, private_types } => {
			relocate_id(object_type_id, ids)?;

			for id in private_types {
				relocate_id(id, ids)?;
			}
		}
		Instruction::PreserveDynamicType(ty) => relocate_type(ty, ids)?,
		Instruction::BeginCatch { clauses, .. } => {
			for clause in clauses {
				if let Some((ty, _)) = &mut clause.binding {
					relocate_type(ty, ids)?;
				}
			}
		}
		_ => {}
	}

	Ok(())
}

fn link_object_types(
	import: &LibraryImport,
	semantics: &SemanticProgram,
	descriptors: &mut Vec<ObjectTypeDescriptor>,
	scopes: &mut Vec<ObjectScopeDebugInfo>,
) -> Result<ObjectIds, String> {
	let library = &import.library.program;
	let mut next_id = descriptors.iter().map(|descriptor| descriptor.id().raw())
		.filter(|id| *id != ObjectTypeId::RUNTIME_ERROR.raw()).max().unwrap_or(0);
	let mut ids = ObjectIds::from([(ObjectTypeId::RUNTIME_ERROR, ObjectTypeId::RUNTIME_ERROR)]);

	for (name, original) in &import.library.objects {
		let linked = import.object_names.get(name)
			.and_then(|name| semantics.object_type_id(name))
			.ok_or_else(|| format!("Missing linked declaration for library object `{name}`."))?;
		ids.insert(ObjectTypeId::from_raw(*original), linked);
	}

	for (name, original) in &import.library.dependencies.ids {
		let linked = semantics.object_type_id(name).ok_or_else(|| format!("Missing bundled declaration for `{name}`."))?;
		ids.insert(ObjectTypeId::from_raw(*original), linked);
	}

	// Dependencies used only by the compiled bodies have no interface stub.
	// Allocate all their IDs before copying any mutually recursive shapes.
	let mut private_descriptors = Vec::new();

	for descriptor in library.object_type_descriptors() {
		if let std::collections::btree_map::Entry::Vacant(entry) = ids.entry(descriptor.id()) {
			next_id = next_id
				.checked_add(1)
				.filter(|id| *id != ObjectTypeId::RUNTIME_ERROR.raw())
				.ok_or_else(|| String::from("Too many linked object types."))?;
			entry.insert(ObjectTypeId::from_raw(next_id));
			private_descriptors.push(descriptor);
		}
	}

	for descriptor in private_descriptors {
		descriptors.push(relocate_descriptor(descriptor.clone(), &ids)?);
	}

	for mut scope in library.debug_info().object_scopes.clone() {
		for (_, id) in &mut scope.bindings {
			relocate_id(id, &ids)?;
		}

		for id in &mut scope.private_types {
			relocate_id(id, &ids)?;
		}

		if !scopes.contains(&scope) {
			scopes.push(scope);
		}
	}

	Ok(ids)
}

pub(crate) fn link(
	program: Program,
	declarations: &AstProgram,
	imports: &[LibraryImport],
	semantics: &SemanticProgram,
) -> Result<Program, CompileError> {
	if imports.is_empty() {
		return Ok(program);
	}

	let error = |message: &str| CompileError { message: message.into(), position: 0 };
	let declarations = crate::compiler::collect_all_function_declarations(declarations);
	let mut descriptors = program.object_type_descriptors().to_vec();
	let mut scopes = program.debug_info().object_scopes.clone();
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
		let ids = link_object_types(import, semantics, &mut descriptors, &mut scopes)
			.map_err(|message| error(&message))?;

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
					_ => relocate_instruction(instruction, &ids).map_err(|message| error(&message))?,
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
				functions[index] = CompiledFunction::new(functions[index].name().map(String::from), functions[index].return_type().cloned(), function.body().clone());
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

	if let Some(entry_debug) = entry_debug {
		debug.push(entry_debug);
	}

	let mut debug = DebugInfo::new(debug, sources);
	debug.object_scopes = scopes;

	let linked = match program.entry_point() {
		EntryPoint::Function(index) => Program::from_entry_function_with_queries(program.constant_pool().clone(), *index, functions, queries, debug),
		EntryPoint::Code(body) => Program::from_parts_with_functions_queries_and_debug(program.constant_pool().clone(), body.clone(), functions, queries, debug),
	};

	Ok(linked.with_object_type_descriptors(descriptors))
}
