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
pub struct ConstantPool {
	entries: Vec<Constant>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
	constants: ConstantPool,
	pub entry: CodeBody,
}

impl CodeBody {
	pub fn new(instructions: Vec<Instruction>) -> Self {
		Self {
			instructions,
		}
	}
}

impl ConstantPool {
	pub fn entries(&self) -> &[Constant] {
		&self.entries
	}

	pub fn is_empty(&self) -> bool {
		self.entries.is_empty()
	}
}

impl Program {
	pub fn new(instructions: Vec<Instruction>) -> Self {
		Self {
			constants: ConstantPool::default(),
			entry: CodeBody::new(instructions),
		}
	}

	pub fn constant_pool(&self) -> &ConstantPool {
		&self.constants
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
