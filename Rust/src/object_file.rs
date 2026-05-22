use std::path::Path;

use crate::bytecode::Instruction;
use crate::bytecode::Program;
use crate::value::Decimal;

const MAGIC_BYTES: [u8; 4] = *b"TBO0";
const FORMAT_VERSION: u16 = 1;
const OPCODE_ADD: u8 = 1;
const OPCODE_DIVIDE: u8 = 2;
const OPCODE_MODULO: u8 = 3;
const OPCODE_MULTIPLY: u8 = 4;
const OPCODE_PUSH_DECIMAL: u8 = 5;
const OPCODE_PUSH_INTEGER: u8 = 6;
const OPCODE_SUBTRACT: u8 = 7;

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
			Instruction::Divide => bytes.push(OPCODE_DIVIDE),
			Instruction::Modulo => bytes.push(OPCODE_MODULO),
			Instruction::Multiply => bytes.push(OPCODE_MULTIPLY),
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
			Instruction::Subtract => bytes.push(OPCODE_SUBTRACT),
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
	fn new(bytes: &'a [u8]) -> Self {
		Self {
			bytes,
			offset: 0,
		}
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

	fn is_at_end(&self) -> bool {
		self.offset >= self.bytes.len()
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

	fn read_instruction(&mut self) -> Result<Instruction, ObjectFileError> {
		let opcode_offset = self.offset;
		let opcode = self.read_u8()?;

		match opcode {
			OPCODE_ADD => Ok(Instruction::Add),
			OPCODE_DIVIDE => Ok(Instruction::Divide),
			OPCODE_MODULO => Ok(Instruction::Modulo),
			OPCODE_MULTIPLY => Ok(Instruction::Multiply),
			OPCODE_PUSH_DECIMAL => Ok(Instruction::PushDecimal(self.read_decimal()?)),
			OPCODE_PUSH_INTEGER => Ok(Instruction::PushInteger(self.read_i64()?)),
			OPCODE_SUBTRACT => Ok(Instruction::Subtract),
			_ => Err(ObjectFileError {
				offset: opcode_offset,
				message: format!("Unknown opcode {opcode}."),
			}),
		}
	}

	fn read_i64(&mut self) -> Result<i64, ObjectFileError> {
		let mut bytes = [0; 8];
		bytes.copy_from_slice(self.read_exact(8)?);
		Ok(i64::from_le_bytes(bytes))
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
