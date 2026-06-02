// This module encodes and decodes the current `.tbo` object file format.
// The format is still deliberately simple, but this is another likely split
// point once the object file grows support for multiple sections and debug
// metadata.

use std::path::Path;

use crate::builtins::BuiltInFunction;
use crate::bytecode::CodeBody;
use crate::bytecode::CompiledFunction;
use crate::bytecode::ConstantPool;
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
const OPCODE_JUMP: u8 = 7;
const OPCODE_JUMP_IF_FALSE: u8 = 8;
const OPCODE_LESS_THAN: u8 = 9;
const OPCODE_LESS_THAN_OR_EQUAL: u8 = 10;
const OPCODE_LOAD_LOCAL: u8 = 11;
const OPCODE_MODULO: u8 = 12;
const OPCODE_MULTIPLY: u8 = 13;
const OPCODE_PUSH_BOOLEAN: u8 = 14;
const OPCODE_PUSH_DECIMAL: u8 = 15;
const OPCODE_PUSH_INTEGER: u8 = 16;
const OPCODE_STORE_LOCAL: u8 = 17;
const OPCODE_SUBTRACT: u8 = 18;
const OPCODE_NEGATE: u8 = 19;
const OPCODE_NOT_EQUAL: u8 = 20;
const OPCODE_NOT: u8 = 21;
const OPCODE_OR: u8 = 22;
const OPCODE_XOR: u8 = 23;
const OPCODE_POP: u8 = 24;
const OPCODE_PUSH_TEXT: u8 = 25;
const OPCODE_CALL: u8 = 26;
const OPCODE_RETURN: u8 = 27;
const OPCODE_RETURN_VOID: u8 = 28;
const OPCODE_MAKE_ARRAY: u8 = 29;
const OPCODE_LOAD_INDEX: u8 = 30;
const OPCODE_STORE_INDEX: u8 = 31;
const OPCODE_CALL_BUILT_IN: u8 = 32;
const OPCODE_DUP2: u8 = 33;
const OPCODE_MAKE_RANGE: u8 = 34;
const OPCODE_MAKE_STEPPED_RANGE: u8 = 35;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ObjectFileError {
	pub offset: usize,
	pub message: String,
}

// The on-disk format is still effectively "header plus one entry code body",
// but representing it as a layout with sections now gives us a much cleaner
// path toward additional code bodies and embedded debug metadata later on.
#[derive(Clone, Debug, Eq, PartialEq)]
struct ObjectFileLayout {
	sections: Vec<ObjectFileSection>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum ObjectFileSection {
	Function(CompiledFunction),
	EntryCode(CodeBody),
}

pub fn read_program(bytes: &[u8]) -> Result<Program, ObjectFileError> {
	let mut reader = ObjectFileReader::new(bytes);

	reader.expect_magic_bytes()?;
	reader.expect_format_version()?;
	let layout = reader.read_layout()?;

	if !reader.is_at_end() {
		return Err(ObjectFileError {
			offset: reader.offset,
			message: String::from("Unexpected trailing data after final instruction."),
		});
	}

	layout.into_program()
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
	let layout = ObjectFileLayout::from_program(program);

	bytes.extend_from_slice(&MAGIC_BYTES);
	bytes.extend_from_slice(&FORMAT_VERSION.to_le_bytes());
	layout.write_to(&mut bytes);

	bytes
}

fn write_code_body(bytes: &mut Vec<u8>, code_body: &CodeBody) {
	bytes.extend_from_slice(&(code_body.instructions.len() as u32).to_le_bytes());

	for instruction in &code_body.instructions {
		write_instruction(bytes, instruction);
	}
}

fn write_instruction(bytes: &mut Vec<u8>, instruction: &Instruction) {
	match instruction {
		Instruction::Add => bytes.push(OPCODE_ADD),
		Instruction::And => bytes.push(OPCODE_AND),
		Instruction::Call(function_index, argument_count) => {
			bytes.push(OPCODE_CALL);
			bytes.extend_from_slice(&function_index.to_le_bytes());
			bytes.extend_from_slice(&argument_count.to_le_bytes());
		}
		Instruction::CallBuiltIn(built_in, argument_count) => {
			bytes.push(OPCODE_CALL_BUILT_IN);
			bytes.push(built_in.id());
			bytes.extend_from_slice(&argument_count.to_le_bytes());
		}
		Instruction::Divide => bytes.push(OPCODE_DIVIDE),
		Instruction::Dup2 => bytes.push(OPCODE_DUP2),
		Instruction::Equal => bytes.push(OPCODE_EQUAL),
		Instruction::GreaterThan => bytes.push(OPCODE_GREATER_THAN),
		Instruction::GreaterThanOrEqual => bytes.push(OPCODE_GREATER_THAN_OR_EQUAL),
		Instruction::Jump(target) => {
			bytes.push(OPCODE_JUMP);
			bytes.extend_from_slice(&target.to_le_bytes());
		}
		Instruction::JumpIfFalse(target) => {
			bytes.push(OPCODE_JUMP_IF_FALSE);
			bytes.extend_from_slice(&target.to_le_bytes());
		}
		Instruction::LessThan => bytes.push(OPCODE_LESS_THAN),
		Instruction::LessThanOrEqual => bytes.push(OPCODE_LESS_THAN_OR_EQUAL),
		Instruction::LoadIndex => bytes.push(OPCODE_LOAD_INDEX),
		Instruction::LoadLocal(slot) => {
			bytes.push(OPCODE_LOAD_LOCAL);
			bytes.extend_from_slice(&slot.to_le_bytes());
		}
		Instruction::MakeArray(element_count) => {
			bytes.push(OPCODE_MAKE_ARRAY);
			bytes.extend_from_slice(&element_count.to_le_bytes());
		}
		Instruction::MakeRange => bytes.push(OPCODE_MAKE_RANGE),
		Instruction::MakeSteppedRange => bytes.push(OPCODE_MAKE_STEPPED_RANGE),
		Instruction::Modulo => bytes.push(OPCODE_MODULO),
		Instruction::Multiply => bytes.push(OPCODE_MULTIPLY),
		Instruction::Negate => bytes.push(OPCODE_NEGATE),
		Instruction::Not => bytes.push(OPCODE_NOT),
		Instruction::NotEqual => bytes.push(OPCODE_NOT_EQUAL),
		Instruction::Or => bytes.push(OPCODE_OR),
		Instruction::Pop => bytes.push(OPCODE_POP),
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
		Instruction::PushText(value) => {
			bytes.push(OPCODE_PUSH_TEXT);
			bytes.extend_from_slice(&(value.len() as u32).to_le_bytes());
			bytes.extend_from_slice(value.as_bytes());
		}
		Instruction::Return => bytes.push(OPCODE_RETURN),
		Instruction::ReturnVoid => bytes.push(OPCODE_RETURN_VOID),
		Instruction::StoreIndex => bytes.push(OPCODE_STORE_INDEX),
		Instruction::StoreLocal(slot) => {
			bytes.push(OPCODE_STORE_LOCAL);
			bytes.extend_from_slice(&slot.to_le_bytes());
		}
		Instruction::Subtract => bytes.push(OPCODE_SUBTRACT),
		Instruction::Xor => bytes.push(OPCODE_XOR),
	}
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

	fn read_code_body(&mut self) -> Result<CodeBody, ObjectFileError> {
		let instruction_count = self.read_u32()? as usize;
		let mut instructions = Vec::with_capacity(instruction_count);

		for _ in 0..instruction_count {
			instructions.push(self.read_instruction()?);
		}

		Ok(CodeBody::new(instructions))
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

		self.read_instruction_payload(opcode, opcode_offset)
	}

	fn read_instruction_payload(&mut self, opcode: u8, opcode_offset: usize) -> Result<Instruction, ObjectFileError> {
		match opcode {
			OPCODE_ADD => Ok(Instruction::Add),
			OPCODE_AND => Ok(Instruction::And),
			OPCODE_CALL => Ok(Instruction::Call(self.read_u32()?, self.read_u32()?)),
			OPCODE_CALL_BUILT_IN => {
				let built_in_id = self.read_u8()?;
				let argument_count = self.read_u32()?;
				let built_in = BuiltInFunction::from_id(built_in_id).ok_or(ObjectFileError {
					offset: self.offset - 5,
					message: format!("Unknown built-in function id {built_in_id}."),
				})?;
				Ok(Instruction::CallBuiltIn(built_in, argument_count))
			}
			OPCODE_DIVIDE => Ok(Instruction::Divide),
			OPCODE_DUP2 => Ok(Instruction::Dup2),
			OPCODE_EQUAL => Ok(Instruction::Equal),
			OPCODE_GREATER_THAN => Ok(Instruction::GreaterThan),
			OPCODE_GREATER_THAN_OR_EQUAL => Ok(Instruction::GreaterThanOrEqual),
			OPCODE_JUMP => Ok(Instruction::Jump(self.read_u32()?)),
			OPCODE_JUMP_IF_FALSE => Ok(Instruction::JumpIfFalse(self.read_u32()?)),
			OPCODE_LESS_THAN => Ok(Instruction::LessThan),
			OPCODE_LESS_THAN_OR_EQUAL => Ok(Instruction::LessThanOrEqual),
			OPCODE_LOAD_INDEX => Ok(Instruction::LoadIndex),
			OPCODE_LOAD_LOCAL => Ok(Instruction::LoadLocal(self.read_u32()?)),
			OPCODE_MAKE_ARRAY => Ok(Instruction::MakeArray(self.read_u32()?)),
			OPCODE_MAKE_RANGE => Ok(Instruction::MakeRange),
			OPCODE_MAKE_STEPPED_RANGE => Ok(Instruction::MakeSteppedRange),
			OPCODE_MODULO => Ok(Instruction::Modulo),
			OPCODE_MULTIPLY => Ok(Instruction::Multiply),
			OPCODE_NEGATE => Ok(Instruction::Negate),
			OPCODE_NOT => Ok(Instruction::Not),
			OPCODE_NOT_EQUAL => Ok(Instruction::NotEqual),
			OPCODE_OR => Ok(Instruction::Or),
			OPCODE_POP => Ok(Instruction::Pop),
			OPCODE_PUSH_BOOLEAN => Ok(Instruction::PushBoolean(self.read_bool()?)),
			OPCODE_PUSH_DECIMAL => Ok(Instruction::PushDecimal(self.read_decimal()?)),
			OPCODE_PUSH_INTEGER => Ok(Instruction::PushInteger(self.read_i64()?)),
			OPCODE_PUSH_TEXT => Ok(Instruction::PushText(self.read_string()?)),
			OPCODE_RETURN => Ok(Instruction::Return),
			OPCODE_RETURN_VOID => Ok(Instruction::ReturnVoid),
			OPCODE_STORE_INDEX => Ok(Instruction::StoreIndex),
			OPCODE_STORE_LOCAL => Ok(Instruction::StoreLocal(self.read_u32()?)),
			OPCODE_SUBTRACT => Ok(Instruction::Subtract),
			OPCODE_XOR => Ok(Instruction::Xor),
			_ => Err(ObjectFileError {
				offset: opcode_offset,
				message: format!("Unknown opcode {opcode}."),
			}),
		}
	}

	fn read_layout(&mut self) -> Result<ObjectFileLayout, ObjectFileError> {
		let function_count = self.read_u32()? as usize;
		let mut sections = Vec::with_capacity(function_count + 1);

		for index in 0..function_count {
			sections.push(ObjectFileSection::Function(CompiledFunction::new(
				Some(format!("function_{index}")),
				self.read_code_body()?,
			)));
		}

		sections.push(ObjectFileSection::EntryCode(self.read_code_body()?));

		Ok(ObjectFileLayout {
			sections,
		})
	}

	fn read_string(&mut self) -> Result<String, ObjectFileError> {
		let len = self.read_u32()? as usize;
		let bytes = self.read_exact(len)?;
		String::from_utf8(bytes.to_vec()).map_err(|_| ObjectFileError {
			offset: self.offset - len,
			message: String::from("Invalid UTF-8 string data in object file."),
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

impl ObjectFileLayout {
	fn from_program(program: &Program) -> Self {
		debug_assert!(
			program.constant_pool().is_empty(),
			"The current object file format does not yet serialize constant-pool entries."
		);
		debug_assert!(
			program.debug_info().is_empty(),
			"The current object file format does not yet serialize debug metadata."
		);

		let mut sections = Vec::with_capacity(program.functions().len() + 1);

		for function in program.functions() {
			sections.push(ObjectFileSection::Function(function.clone()));
		}

		sections.push(ObjectFileSection::EntryCode(program.entry.clone()));

		Self { sections }
	}

	fn into_program(self) -> Result<Program, ObjectFileError> {
		let mut entry_code = None;
		let mut functions = Vec::new();

		for section in self.sections {
			match section {
				ObjectFileSection::Function(function) => {
					functions.push(function);
				}
				ObjectFileSection::EntryCode(code_body) => {
					if entry_code.is_some() {
						return Err(ObjectFileError {
							offset: 0,
							message: String::from("Object file contains more than one entry code section."),
						});
					}

					entry_code = Some(code_body);
				}
			}
		}

		let entry_code = entry_code.ok_or(ObjectFileError {
			offset: 0,
			message: String::from("Object file does not contain an entry code section."),
		})?;

		Ok(Program::from_parts_with_functions(ConstantPool::default(), entry_code, functions))
	}

	fn write_to(&self, bytes: &mut Vec<u8>) {
		let function_count = self.sections.iter()
			.filter(|section| matches!(section, ObjectFileSection::Function(_)))
			.count() as u32;
		bytes.extend_from_slice(&function_count.to_le_bytes());

		for section in &self.sections {
			match section {
				ObjectFileSection::Function(function) => write_code_body(bytes, function.body()),
				ObjectFileSection::EntryCode(code_body) => write_code_body(bytes, code_body),
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::builtins::BuiltInFunction;
	use crate::bytecode::CodeBody;
	use crate::bytecode::CompiledFunction;
	use crate::bytecode::ConstantPool;
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
		bytes[10..14].copy_from_slice(&1u32.to_le_bytes());
		bytes.push(255);

		let error = read_program(&bytes).unwrap_err();

		assert_eq!(error, ObjectFileError {
			offset: 14,
			message: String::from("Unknown opcode 255."),
		});
	}

	#[test]
	fn round_trips_array_program_bytes() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::MakeArray(2),
			Instruction::PushInteger(1),
			Instruction::LoadIndex,
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
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

	#[test]
	fn round_trips_program_bytes_with_built_in_call() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::MakeArray(2),
			Instruction::CallBuiltIn(BuiltInFunction::Len, 1),
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_program_bytes_with_compiled_function() {
		let program = Program::from_parts_with_functions(
			ConstantPool::default(),
			CodeBody::new(vec![
				Instruction::PushInteger(1),
				Instruction::PushInteger(2),
				Instruction::Call(0, 2),
			]),
			vec![
				CompiledFunction::new(
					Some(String::from("add")),
					CodeBody::new(vec![
						Instruction::LoadLocal(0),
						Instruction::LoadLocal(1),
						Instruction::Add,
						Instruction::Return,
					]),
				),
			],
		);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, Program::from_parts_with_functions(
			ConstantPool::default(),
			CodeBody::new(vec![
				Instruction::PushInteger(1),
				Instruction::PushInteger(2),
				Instruction::Call(0, 2),
			]),
			vec![
				CompiledFunction::new(
					Some(String::from("function_0")),
					CodeBody::new(vec![
						Instruction::LoadLocal(0),
						Instruction::LoadLocal(1),
						Instruction::Add,
						Instruction::Return,
					]),
				),
			],
		));
	}

	#[test]
	fn round_trips_program_bytes_with_dup2() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::Dup2,
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_program_bytes_with_range() {
		let program = Program::new(vec![
			Instruction::PushInteger(0),
			Instruction::PushInteger(10),
			Instruction::MakeRange,
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_text_program_bytes() {
		let program = Program::new(vec![
			Instruction::PushText(String::from("hello")),
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}
}
