// These types represent the current in-memory bytecode model used by both the
// compiler and the VM. It is intentionally small today, but it will likely
// grow into a richer structure once functions, sections, and debug metadata
// are introduced.

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
	Divide,
	Equal,
	GreaterThan,
	GreaterThanOrEqual,
	Jump(u32),
	JumpIfFalse(u32),
	LessThan,
	LessThanOrEqual,
	LoadIndex,
	LoadLocal(u32),
	MakeArray(u32),
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
	instruction_positions: Vec<usize>,
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

	pub fn functions(&self) -> &[CompiledFunction] {
		&self.functions
	}

	pub fn instructions(&self) -> &[Instruction] {
		&self.entry.instructions
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
