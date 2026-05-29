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
	Divide,
	Equal,
	GreaterThan,
	GreaterThanOrEqual,
	Jump(u32),
	JumpIfFalse(u32),
	LessThan,
	LessThanOrEqual,
	LoadLocal(u32),
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
		Self::from_parts_with_debug(constants, entry, DebugInfo::default())
	}

	pub fn from_parts_with_debug(constants: ConstantPool, entry: CodeBody, debug: DebugInfo) -> Self {
		Self {
			constants,
			debug,
			entry,
		}
	}

	pub fn constant_pool(&self) -> &ConstantPool {
		&self.constants
	}

	pub fn debug_info(&self) -> &DebugInfo {
		&self.debug
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
