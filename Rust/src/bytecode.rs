use crate::value::Decimal;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
	Add,
	Divide,
	Modulo,
	Multiply,
	Negate,
	PushBoolean(bool),
	PushDecimal(Decimal),
	PushInteger(i64),
	Subtract,
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
