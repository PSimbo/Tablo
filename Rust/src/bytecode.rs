// These types represent the current in-memory bytecode model used by both the
// compiler and the VM. It is intentionally small today, but it will likely
// grow into a richer structure once functions, sections, and debug metadata
// are introduced.

use crate::ast::RecordPointerType;
use crate::builtins::BuiltInFunction;
use crate::query::LoweredBackendQuery;
use crate::source::SourceText;
use crate::value::Decimal;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Constant {
	Boolean(bool),
	Date(crate::value::Date),
	Decimal(Decimal),
	Integer(i64),
	Text(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
	Add,
	AdvanceSequence {
		database_name: String,
		schema_is_implicit: bool,
		schema_name: String,
		sequence_name: String,
	},
	And,
	Call(u32, u32),
	CallBuiltIn(BuiltInFunction, u32),
	CreateRecord,
	Divide,
	Dup2,
	Equal,
	ExecuteQuery(u32),
	GreaterThan,
	GreaterThanOrEqual,
	IterHasNext,
	IterInit,
	IterNext,
	Jump(u32),
	JumpIfFalse(u32),
	LessThan,
	LessThanOrEqual,
	LoadField(String),
	LoadFieldPath(Vec<String>),
	LoadIndex,
	LoadLocal(u32),
	LoadReference(u32),
	LoadSequenceCurrent {
		database_name: String,
		schema_is_implicit: bool,
		schema_name: String,
		sequence_name: String,
	},
	MakeArray(u32),
	MakeObject(Vec<String>),
	MakeRange,
	MakeRecordPointer {
		field_names: Vec<String>,
		record_type: RecordPointerType,
		schema_is_implicit: bool,
	},
	MakeSteppedRange,
	Modulo,
	Multiply,
	Negate,
	Not,
	NotEqual,
	Or,
	Pop,
	PushBoolean(bool),
	PushCurrentDate,
	PushCurrentTime,
	PushCurrentTimeTz,
	PushCurrentTimestamp,
	PushCurrentTimestampTz,
	PushDate(crate::value::Date),
	PushDecimal(Decimal),
	PushEnumValue {
		backing_value: Constant,
		enum_name: String,
		variant_name: String,
	},
	PushInteger(i64),
	PushNull,
	PushText(String),
	PushTime(crate::value::Time),
	PushTimeTz(crate::value::TimeTz),
	PushTimestamp(crate::value::Timestamp),
	PushTimestampTz(crate::value::TimestampTz),
	Return,
	ReturnVoid,
	StoreFieldPath(Vec<String>),
	StoreIndex,
	StoreLocal(u32),
	StoreSequenceCurrent {
		database_name: String,
		schema_is_implicit: bool,
		schema_name: String,
		sequence_name: String,
	},
	Subtract,
	Xor,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CodeBody {
	pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CodeBodyDebugInfo {
	body_name: Option<String>,
	instruction_positions: Vec<u32>,
	locals: Vec<LocalVariableDebugInfo>,
	source_file_index: Option<u32>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompiledFunction {
	body: CodeBody,
	name: Option<String>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ConstantPool {
	entries: Vec<Constant>,
}

// Debug metadata stays at the bytecode layer so the compiler can attach source
// positions as it emits code, while line/column formatting can still be
// derived later from the original source text when needed.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct DebugInfo {
	code_bodies: Vec<CodeBodyDebugInfo>,
	source_files: Vec<SourceFileDebugInfo>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EntryPoint {
	Code(CodeBody),
	Function(u32),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InstructionSite {
	body_index: usize,
	instruction_index: usize,
	source_location: SourceLocation,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalVariableDebugInfo {
	declared_type: String,
	is_const: bool,
	name: String,
	scope_end: u32,
	scope_start: u32,
	slot: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
	constants: ConstantPool,
	debug: DebugInfo,
	entry_point: EntryPoint,
	functions: Vec<CompiledFunction>,
	queries: Vec<LoweredBackendQuery>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceFileDebugInfo {
	display_name: String,
	line_starts: Vec<u32>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceLocation {
	body_name: Option<String>,
	column: u32,
	display_name: Option<String>,
	line: u32,
}

impl CodeBody {
	pub fn new(instructions: Vec<Instruction>) -> Self {
		Self {
			instructions,
		}
	}
}

impl CompiledFunction {
	pub fn new(name: Option<String>, body: CodeBody) -> Self {
		Self {
			body,
			name,
		}
	}

	pub fn body(&self) -> &CodeBody {
		&self.body
	}

	pub fn name(&self) -> Option<&str> {
		self.name.as_deref()
	}
}

impl ConstantPool {
	pub fn new(entries: Vec<Constant>) -> Self {
		Self {
			entries,
		}
	}

	pub fn entries(&self) -> &[Constant] {
		&self.entries
	}

	pub fn is_empty(&self) -> bool {
		self.entries.is_empty()
	}
}

impl DebugInfo {
	pub fn add_source_file(&mut self, source_file: SourceFileDebugInfo) -> u32 {
		let source_file_index = self.source_files.len() as u32;
		self.source_files.push(source_file);
		source_file_index
	}

	pub fn attach_source_file(&mut self, source_file: SourceFileDebugInfo) {
		let source_file_index = self.add_source_file(source_file);

		for code_body in &mut self.code_bodies {
			if code_body.source_file_index.is_none() {
				code_body.source_file_index = Some(source_file_index);
			}
		}
	}

	pub fn code_bodies(&self) -> &[CodeBodyDebugInfo] {
		&self.code_bodies
	}

	pub fn is_empty(&self) -> bool {
		self.code_bodies.is_empty() && self.source_files.is_empty()
	}

	pub fn set_code_body_source_file(&mut self, body_index: usize, source_file_index: u32) {
		if let Some(code_body) = self.code_bodies.get_mut(body_index) {
			code_body.source_file_index = Some(source_file_index);
		}
	}

	pub fn source_files(&self) -> &[SourceFileDebugInfo] {
		&self.source_files
	}
}

impl Program {
	pub fn new(instructions: Vec<Instruction>) -> Self {
		Self::from_parts(ConstantPool::default(), CodeBody::new(instructions))
	}

	pub fn from_entry_function(
		constants: ConstantPool,
		entry_function_index: u32,
		functions: Vec<CompiledFunction>,
		debug: DebugInfo,
	) -> Self {
		Self::from_entry_function_with_queries(constants, entry_function_index, functions, Vec::new(), debug)
	}

	pub fn from_entry_function_with_queries(
		constants: ConstantPool,
		entry_function_index: u32,
		functions: Vec<CompiledFunction>,
		queries: Vec<LoweredBackendQuery>,
		debug: DebugInfo,
	) -> Self {
		Self {
			constants,
			debug,
			entry_point: EntryPoint::Function(entry_function_index),
			functions,
			queries,
		}
	}

	pub fn from_parts(constants: ConstantPool, entry: CodeBody) -> Self {
		Self::from_parts_with_functions_and_debug(constants, entry, Vec::new(), DebugInfo::default())
	}

	pub fn from_parts_with_debug(constants: ConstantPool, entry: CodeBody, debug: DebugInfo) -> Self {
		Self::from_parts_with_functions_and_debug(constants, entry, Vec::new(), debug)
	}

	pub fn from_parts_with_functions(constants: ConstantPool, entry: CodeBody, functions: Vec<CompiledFunction>) -> Self {
		Self::from_parts_with_functions_and_debug(constants, entry, functions, DebugInfo::default())
	}

	pub fn from_parts_with_functions_and_debug(
		constants: ConstantPool,
		entry: CodeBody,
		functions: Vec<CompiledFunction>,
		debug: DebugInfo
	) -> Self {
		Self::from_parts_with_functions_queries_and_debug(constants, entry, functions, Vec::new(), debug)
	}

	pub fn from_parts_with_functions_queries_and_debug(
		constants: ConstantPool,
		entry: CodeBody,
		functions: Vec<CompiledFunction>,
		queries: Vec<LoweredBackendQuery>,
		debug: DebugInfo
	) -> Self {
		Self {
			constants,
			debug,
			entry_point: EntryPoint::Code(entry),
			functions,
			queries,
		}
	}

	pub fn constant_pool(&self) -> &ConstantPool {
		&self.constants
	}

	pub fn debug_info(&self) -> &DebugInfo {
		&self.debug
	}

	pub fn debug_info_mut(&mut self) -> &mut DebugInfo {
		&mut self.debug
	}

	pub fn debug_location(&self, body_index: usize, instruction_index: usize) -> Option<SourceLocation> {
		let code_body = self.debug.code_bodies.get(body_index)?;
		let position = *code_body.instruction_positions.get(instruction_index)? as usize;
		let source_file = code_body.source_file_index
			.and_then(|index| self.debug.source_files.get(index as usize));
		let (line, column) = source_file
			.map(|source_file| source_file.line_and_column(position))
			.unwrap_or((1, position as u32 + 1));

		Some(SourceLocation {
			body_name: code_body.body_name.clone(),
			column,
			display_name: source_file.map(|source_file| source_file.display_name.clone()),
			line,
		})
	}

	pub fn entry_code(&self) -> Option<&CodeBody> {
		match &self.entry_point {
			EntryPoint::Code(code_body) => Some(code_body),
			EntryPoint::Function(_) => None,
		}
	}

	pub fn entry_function_index(&self) -> Option<u32> {
		match self.entry_point {
			EntryPoint::Code(_) => None,
			EntryPoint::Function(function_index) => Some(function_index),
		}
	}

	pub fn entry_point(&self) -> &EntryPoint {
		&self.entry_point
	}

	pub fn functions(&self) -> &[CompiledFunction] {
		&self.functions
	}

	pub fn instruction_sites_for_source_line(&self, display_name: &str, line: u32) -> Vec<InstructionSite> {
		let mut sites = Vec::new();

		for (body_index, code_body) in self.debug.code_bodies.iter().enumerate() {
			for instruction_index in 0..code_body.instruction_positions.len() {
				let Some(location) = self.debug_location(body_index, instruction_index) else {
					continue;
				};

				if location.display_name() == Some(display_name) && location.line() == line {
					sites.push(InstructionSite::new(body_index, instruction_index, location));
				}
			}
		}

		sites
	}

	pub fn instructions(&self) -> &[Instruction] {
		match &self.entry_point {
			EntryPoint::Code(code_body) => &code_body.instructions,
			EntryPoint::Function(_) => &[],
		}
	}

	pub fn queries(&self) -> &[LoweredBackendQuery] {
		&self.queries
	}

	pub fn visible_locals(&self, body_index: usize, instruction_index: usize) -> Vec<LocalVariableDebugInfo> {
		self.debug.code_bodies
			.get(body_index)
			.map(|code_body| code_body.visible_locals(instruction_index))
			.unwrap_or_default()
	}
}

impl CodeBodyDebugInfo {
	pub fn new(
		body_name: Option<String>,
		instruction_positions: Vec<u32>,
		locals: Vec<LocalVariableDebugInfo>,
		source_file_index: Option<u32>
	) -> Self {
		Self {
			body_name,
			instruction_positions,
			locals,
			source_file_index,
		}
	}

	pub fn body_name(&self) -> Option<&str> {
		self.body_name.as_deref()
	}

	pub fn instruction_positions(&self) -> &[u32] {
		&self.instruction_positions
	}

	pub fn locals(&self) -> &[LocalVariableDebugInfo] {
		&self.locals
	}

	pub fn visible_locals(&self, instruction_index: usize) -> Vec<LocalVariableDebugInfo> {
		let instruction_index = instruction_index as u32;

		self.locals
			.iter()
			.filter(|local| local.scope_start <= instruction_index && instruction_index < local.scope_end)
			.cloned()
			.collect()
	}

	pub fn source_file_index(&self) -> Option<u32> {
		self.source_file_index
	}
}

impl InstructionSite {
	pub fn new(body_index: usize, instruction_index: usize, source_location: SourceLocation) -> Self {
		Self {
			body_index,
			instruction_index,
			source_location,
		}
	}

	pub fn body_index(&self) -> usize {
		self.body_index
	}

	pub fn instruction_index(&self) -> usize {
		self.instruction_index
	}

	pub fn source_location(&self) -> &SourceLocation {
		&self.source_location
	}
}

impl DebugInfo {
	pub fn new(code_bodies: Vec<CodeBodyDebugInfo>, source_files: Vec<SourceFileDebugInfo>) -> Self {
		Self {
			code_bodies,
			source_files,
		}
	}
}

impl LocalVariableDebugInfo {
	pub fn new(
		name: impl Into<String>,
		slot: u32,
		declared_type: impl Into<String>,
		is_const: bool,
		scope_start: u32,
		scope_end: u32,
	) -> Self {
		Self {
			declared_type: declared_type.into(),
			is_const,
			name: name.into(),
			scope_end,
			scope_start,
			slot,
		}
	}

	pub fn declared_type(&self) -> &str {
		&self.declared_type
	}

	pub fn is_const(&self) -> bool {
		self.is_const
	}

	pub fn name(&self) -> &str {
		&self.name
	}

	pub fn scope_end(&self) -> u32 {
		self.scope_end
	}

	pub fn scope_start(&self) -> u32 {
		self.scope_start
	}

	pub fn slot(&self) -> u32 {
		self.slot
	}
}

impl SourceFileDebugInfo {
	pub fn new(display_name: impl Into<String>, line_starts: Vec<u32>) -> Self {
		Self {
			display_name: display_name.into(),
			line_starts,
		}
	}

	pub fn from_source(display_name: impl Into<String>, source: &SourceText) -> Self {
		Self::new(display_name, source.line_starts().into_iter().map(|start| start as u32).collect())
	}

	pub fn display_name(&self) -> &str {
		&self.display_name
	}

	pub fn line_starts(&self) -> &[u32] {
		&self.line_starts
	}

	fn line_and_column(&self, position: usize) -> (u32, u32) {
		let position = position.min(u32::MAX as usize) as u32;
		let line_index = self.line_starts.partition_point(|line_start| *line_start <= position).saturating_sub(1);
		let line_start = self.line_starts.get(line_index).copied().unwrap_or(0);
		(line_index as u32 + 1, position.saturating_sub(line_start) + 1)
	}
}

impl SourceLocation {
	pub fn new(body_name: Option<String>, column: u32, display_name: Option<String>, line: u32) -> Self {
		Self {
			body_name,
			column,
			display_name,
			line,
		}
	}

	pub fn body_name(&self) -> Option<&str> {
		self.body_name.as_deref()
	}

	pub fn column(&self) -> u32 {
		self.column
	}

	pub fn display_name(&self) -> Option<&str> {
		self.display_name.as_deref()
	}

	pub fn line(&self) -> u32 {
		self.line
	}
}

#[cfg(test)]
mod tests {
	use super::CodeBody;
	use super::CompiledFunction;
	use super::ConstantPool;
	use super::Instruction;
	use super::Program;

	#[test]
	fn retains_compiled_functions_alongside_entry_code() {
		let entry = CodeBody::new(vec![
			Instruction::PushInteger(1),
		]);
		let helper = CompiledFunction::new(
			Some(String::from("helper")),
			CodeBody::new(vec![
				Instruction::PushInteger(2),
			]),
		);

		let program = Program::from_parts_with_functions(ConstantPool::default(), entry.clone(), vec![helper.clone()]);

		assert_eq!(program.entry_code(), Some(&entry));
		assert_eq!(program.functions(), &[helper]);
	}
}
