use std::path::Path;

use crate::bytecode::Instruction;
use crate::bytecode::Program;
use crate::value::Decimal;

const MAGIC_BYTES: [u8; 4] = *b"TBO0";
const FORMAT_VERSION: u16 = 1;
const OPCODE_ADD: u8 = 1;
const OPCODE_AND: u8 = 2;
const OPCODE_DIVIDE: u8 = 3;
const OPCODE_EQUAL: u8 = 4;
const OPCODE_GREATER_THAN: u8 = 5;
const OPCODE_GREATER_THAN_OR_EQUAL: u8 = 6;
const OPCODE_LESS_THAN: u8 = 7;
const OPCODE_LESS_THAN_OR_EQUAL: u8 = 8;
const OPCODE_LOAD_LOCAL: u8 = 9;
const OPCODE_MODULO: u8 = 10;
const OPCODE_MULTIPLY: u8 = 11;
const OPCODE_PUSH_BOOLEAN: u8 = 12;
const OPCODE_PUSH_DECIMAL: u8 = 13;
const OPCODE_PUSH_INTEGER: u8 = 14;
const OPCODE_STORE_LOCAL: u8 = 15;
const OPCODE_SUBTRACT: u8 = 16;
const OPCODE_NEGATE: u8 = 17;
const OPCODE_NOT_EQUAL: u8 = 18;
const OPCODE_NOT: u8 = 19;
const OPCODE_OR: u8 = 20;
const OPCODE_XOR: u8 = 21;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ObjectFileError {
	pub offset: usize,
	pub message: String,
}

pub fn read_program(bytes: &[u8]) -> Result<Program, ObjectFileError> {
	let mut reader = ObjectFileReader::new(bytes);

	reader.expect_magic_bytes()?;
	reader.expect_format_version()?;

	let instruction_count = reader.read_u32()? as usize;
	let mut instructions = Vec::with_capacity(instruction_count);

	for _ in 0..instruction_count {
		instructions.push(reader.read_instruction()?);
	}

	if !reader.is_at_end() {
		return Err(ObjectFileError {
			offset: reader.offset,
			message: String::from("Unexpected trailing data after final instruction."),
		});
	}

	Ok(Program::new(instructions))
}

pub fn read_program_from_path(path: impl AsRef<Path>) -> Result<Program, ObjectFileError> {
	let bytes = std::fs::read(path).map_err(|error| ObjectFileError {
		offset: 0,
		message: format!("Failed to read object file: {error}"),
	})?;

	read_program(&bytes)
}

pub fn write_program(program: &Program) -> Vec<u8> {
	let mut bytes = Vec::new();

	bytes.extend_from_slice(&MAGIC_BYTES);
	bytes.extend_from_slice(&FORMAT_VERSION.to_le_bytes());
	bytes.extend_from_slice(&(program.instructions.len() as u32).to_le_bytes());

	for instruction in &program.instructions {
		match instruction {
			Instruction::Add => bytes.push(OPCODE_ADD),
			Instruction::And => bytes.push(OPCODE_AND),
			Instruction::Divide => bytes.push(OPCODE_DIVIDE),
			Instruction::Equal => bytes.push(OPCODE_EQUAL),
			Instruction::GreaterThan => bytes.push(OPCODE_GREATER_THAN),
			Instruction::GreaterThanOrEqual => bytes.push(OPCODE_GREATER_THAN_OR_EQUAL),
			Instruction::LessThan => bytes.push(OPCODE_LESS_THAN),
			Instruction::LessThanOrEqual => bytes.push(OPCODE_LESS_THAN_OR_EQUAL),
			Instruction::LoadLocal(slot) => {
				bytes.push(OPCODE_LOAD_LOCAL);
				bytes.extend_from_slice(&slot.to_le_bytes());
			}
			Instruction::Modulo => bytes.push(OPCODE_MODULO),
			Instruction::Multiply => bytes.push(OPCODE_MULTIPLY),
			Instruction::Negate => bytes.push(OPCODE_NEGATE),
			Instruction::Not => bytes.push(OPCODE_NOT),
			Instruction::NotEqual => bytes.push(OPCODE_NOT_EQUAL),
			Instruction::Or => bytes.push(OPCODE_OR),
			Instruction::PushBoolean(value) => {
				bytes.push(OPCODE_PUSH_BOOLEAN);
				bytes.push(u8::from(*value));
			}
			Instruction::PushDecimal(value) => {
				bytes.push(OPCODE_PUSH_DECIMAL);
				bytes.extend_from_slice(&value.coefficient.to_le_bytes());
				bytes.push(value.precision);
				bytes.push(value.scale);
			}
			Instruction::PushInteger(value) => {
				bytes.push(OPCODE_PUSH_INTEGER);
				bytes.extend_from_slice(&value.to_le_bytes());
			}
			Instruction::StoreLocal(slot) => {
				bytes.push(OPCODE_STORE_LOCAL);
				bytes.extend_from_slice(&slot.to_le_bytes());
			}
			Instruction::Subtract => bytes.push(OPCODE_SUBTRACT),
			Instruction::Xor => bytes.push(OPCODE_XOR),
		}
	}

	bytes
}

pub fn write_program_to_path(path: impl AsRef<Path>, program: &Program) -> Result<(), ObjectFileError> {
	let bytes = write_program(program);

	std::fs::write(path, bytes).map_err(|error| ObjectFileError {
		offset: 0,
		message: format!("Failed to write object file: {error}"),
	})
}

struct ObjectFileReader<'a> {
	bytes: &'a [u8],
	offset: usize,
}

impl<'a> ObjectFileReader<'a> {
	fn expect_format_version(&mut self) -> Result<(), ObjectFileError> {
		let version = self.read_u16()?;

		if version != FORMAT_VERSION {
			return Err(ObjectFileError {
				offset: MAGIC_BYTES.len(),
				message: format!("Unsupported object file version {version}."),
			});
		}

		Ok(())
	}

	fn expect_magic_bytes(&mut self) -> Result<(), ObjectFileError> {
		let magic = self.read_exact(MAGIC_BYTES.len())?;

		if magic != MAGIC_BYTES {
			return Err(ObjectFileError {
				offset: 0,
				message: String::from("Invalid object file magic bytes."),
			});
		}

		Ok(())
	}

	fn is_at_end(&self) -> bool {
		self.offset >= self.bytes.len()
	}

	fn new(bytes: &'a [u8]) -> Self {
		Self {
			bytes,
			offset: 0,
		}
	}

	fn read_bool(&mut self) -> Result<bool, ObjectFileError> {
		match self.read_u8()? {
			0 => Ok(false),
			1 => Ok(true),
			value => Err(ObjectFileError {
				offset: self.offset - 1,
				message: format!("Invalid Boolean value {value}."),
			}),
		}
	}

	fn read_decimal(&mut self) -> Result<Decimal, ObjectFileError> {
		let mut coefficient_bytes = [0; 16];
		coefficient_bytes.copy_from_slice(self.read_exact(16)?);
		let coefficient = i128::from_le_bytes(coefficient_bytes);
		let precision = self.read_u8()?;
		let scale = self.read_u8()?;

		Ok(Decimal {
			coefficient,
			precision,
			scale,
		})
	}

	fn read_exact(&mut self, len: usize) -> Result<&'a [u8], ObjectFileError> {
		let end = self.offset + len;

		if end > self.bytes.len() {
			return Err(ObjectFileError {
				offset: self.offset,
				message: String::from("Unexpected end of object file."),
			});
		}

		let slice = &self.bytes[self.offset..end];
		self.offset = end;
		Ok(slice)
	}

	fn read_i64(&mut self) -> Result<i64, ObjectFileError> {
		let mut bytes = [0; 8];
		bytes.copy_from_slice(self.read_exact(8)?);
		Ok(i64::from_le_bytes(bytes))
	}

	fn read_instruction(&mut self) -> Result<Instruction, ObjectFileError> {
		let opcode_offset = self.offset;
		let opcode = self.read_u8()?;

		match opcode {
			OPCODE_ADD => Ok(Instruction::Add),
			OPCODE_AND => Ok(Instruction::And),
			OPCODE_DIVIDE => Ok(Instruction::Divide),
			OPCODE_EQUAL => Ok(Instruction::Equal),
			OPCODE_GREATER_THAN => Ok(Instruction::GreaterThan),
			OPCODE_GREATER_THAN_OR_EQUAL => Ok(Instruction::GreaterThanOrEqual),
			OPCODE_LESS_THAN => Ok(Instruction::LessThan),
			OPCODE_LESS_THAN_OR_EQUAL => Ok(Instruction::LessThanOrEqual),
			OPCODE_LOAD_LOCAL => Ok(Instruction::LoadLocal(self.read_u32()?)),
			OPCODE_MODULO => Ok(Instruction::Modulo),
			OPCODE_MULTIPLY => Ok(Instruction::Multiply),
			OPCODE_NEGATE => Ok(Instruction::Negate),
			OPCODE_NOT => Ok(Instruction::Not),
			OPCODE_NOT_EQUAL => Ok(Instruction::NotEqual),
			OPCODE_OR => Ok(Instruction::Or),
			OPCODE_PUSH_BOOLEAN => Ok(Instruction::PushBoolean(self.read_bool()?)),
			OPCODE_PUSH_DECIMAL => Ok(Instruction::PushDecimal(self.read_decimal()?)),
			OPCODE_PUSH_INTEGER => Ok(Instruction::PushInteger(self.read_i64()?)),
			OPCODE_STORE_LOCAL => Ok(Instruction::StoreLocal(self.read_u32()?)),
			OPCODE_SUBTRACT => Ok(Instruction::Subtract),
			OPCODE_XOR => Ok(Instruction::Xor),
			_ => Err(ObjectFileError {
				offset: opcode_offset,
				message: format!("Unknown opcode {opcode}."),
			}),
		}
	}

	fn read_u8(&mut self) -> Result<u8, ObjectFileError> {
		Ok(self.read_exact(1)?[0])
	}

	fn read_u16(&mut self) -> Result<u16, ObjectFileError> {
		let mut bytes = [0; 2];
		bytes.copy_from_slice(self.read_exact(2)?);
		Ok(u16::from_le_bytes(bytes))
	}

	fn read_u32(&mut self) -> Result<u32, ObjectFileError> {
		let mut bytes = [0; 4];
		bytes.copy_from_slice(self.read_exact(4)?);
		Ok(u32::from_le_bytes(bytes))
	}
}

#[cfg(test)]
mod tests {
	use crate::bytecode::Instruction;
	use crate::bytecode::Program;

	use super::read_program;
	use super::write_program;
	use super::ObjectFileError;

	#[test]
	fn rejects_invalid_magic_bytes() {
		let error = read_program(b"NOPE").unwrap_err();

		assert_eq!(error, ObjectFileError {
			offset: 0,
			message: String::from("Invalid object file magic bytes."),
		});
	}

	#[test]
	fn rejects_unknown_opcode() {
		let mut bytes = write_program(&Program::new(Vec::new()));
		bytes[6..10].copy_from_slice(&1u32.to_le_bytes());
		bytes.push(255);

		let error = read_program(&bytes).unwrap_err();

		assert_eq!(error, ObjectFileError {
			offset: 10,
			message: String::from("Unknown opcode 255."),
		});
	}

	#[test]
	fn round_trips_boolean_program_bytes() {
		let program = Program::new(vec![
			Instruction::PushBoolean(true),
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_decimal_program_bytes() {
		let program = Program::new(vec![
			Instruction::PushDecimal(crate::value::Decimal::from_literal("1.25").unwrap()),
			Instruction::PushDecimal(crate::value::Decimal::from_literal(".5").unwrap()),
			Instruction::Add,
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_program_bytes() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::Add,
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}
}
