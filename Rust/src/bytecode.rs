// These types represent the current in-memory bytecode model used by both the
// compiler and the VM. It is intentionally small today, but it will likely
// grow into a richer structure once functions, sections, and debug metadata
// are introduced.

use crate::builtins::BuiltInFunction;
use crate::source::SourceText;
use crate::value::Decimal;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Constant {
	Boolean(bool),
	Decimal(Decimal),
	Integer(i64),
	Text(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
	Add,
	And,
	Call(u32, u32),
	CallBuiltIn(BuiltInFunction, u32),
	Divide,
	Dup2,
	Equal,
	GreaterThan,
	GreaterThanOrEqual,
	IterHasNext,
	IterInit,
	IterNext,
	Jump(u32),
	JumpIfFalse(u32),
	LessThan,
	LessThanOrEqual,
	LoadIndex,
	LoadLocal(u32),
	MakeArray(u32),
	MakeRange,
	MakeSteppedRange,
	Modulo,
	Multiply,
	Negate,
	Not,
	NotEqual,
	Or,
	Pop,
	PushBoolean(bool),
	PushDecimal(Decimal),
	PushInteger(i64),
	PushText(String),
	Return,
	ReturnVoid,
	StoreIndex,
	StoreLocal(u32),
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
	source_file_index: Option<u32>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompiledFunction {
	body: CodeBody,
	name: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
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
pub struct Program {
	constants: ConstantPool,
	debug: DebugInfo,
	functions: Vec<CompiledFunction>,
	pub entry: CodeBody,
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
	pub fn attach_source_file(&mut self, source_file: SourceFileDebugInfo) {
		let source_file_index = self.source_files.len() as u32;
		self.source_files.push(source_file);

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

	pub fn source_files(&self) -> &[SourceFileDebugInfo] {
		&self.source_files
	}
}

impl Program {
	pub fn new(instructions: Vec<Instruction>) -> Self {
		Self::from_parts(ConstantPool::default(), CodeBody::new(instructions))
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
		Self {
			constants,
			debug,
			functions,
			entry,
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

	pub fn functions(&self) -> &[CompiledFunction] {
		&self.functions
	}

	pub fn instructions(&self) -> &[Instruction] {
		&self.entry.instructions
	}
}

impl CodeBodyDebugInfo {
	pub fn new(body_name: Option<String>, instruction_positions: Vec<u32>, source_file_index: Option<u32>) -> Self {
		Self {
			body_name,
			instruction_positions,
			source_file_index,
		}
	}

	pub fn body_name(&self) -> Option<&str> {
		self.body_name.as_deref()
	}

	pub fn instruction_positions(&self) -> &[u32] {
		&self.instruction_positions
	}

	pub fn source_file_index(&self) -> Option<u32> {
		self.source_file_index
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

impl Default for ConstantPool {
	fn default() -> Self {
		Self {
			entries: Vec::new(),
		}
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

		assert_eq!(program.entry, entry);
		assert_eq!(program.functions(), &[helper]);
	}
}
