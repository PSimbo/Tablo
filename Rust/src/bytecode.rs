use crate::value::Decimal;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
	Add,
	And,
	Divide,
	Equal,
	GreaterThan,
	GreaterThanOrEqual,
	LessThan,
	LessThanOrEqual,
	Modulo,
	Multiply,
	Negate,
	Not,
	NotEqual,
	Or,
	PushBoolean(bool),
	PushDecimal(Decimal),
	PushInteger(i64),
	Subtract,
	Xor,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
	pub instructions: Vec<Instruction>,
}

impl Program {
	pub fn new(instructions: Vec<Instruction>) -> Self {
		Self {
			instructions,
		}
	}
}
