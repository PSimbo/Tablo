// This module encodes and decodes the current `.tbo` object file format.
// The format is still deliberately simple, but this is another likely split
// point once the object file grows support for multiple sections and debug
// metadata.

use std::path::Path;

use crate::ast::DataType;
use crate::builtins::BuiltInFunction;
use crate::bytecode::*;
use crate::query::*;
use crate::value::Decimal;

const MAGIC_BYTES: [u8; 4] = *b"TBO0";

const FORMAT_VERSION: u16 = 1; // Leave at 1 until intial development is complete.

const OPCODE_ADD: u8 = 1;
const OPCODE_ADVANCE_SEQUENCE: u8 = OPCODE_ADD + 1;
const OPCODE_AND: u8 = OPCODE_ADVANCE_SEQUENCE + 1;
const OPCODE_BEGIN_TRANSACTION: u8 = OPCODE_AND + 1;
const OPCODE_CALL: u8 = OPCODE_BEGIN_TRANSACTION + 1;
const OPCODE_CALL_BUILT_IN: u8 = OPCODE_CALL + 1;
const OPCODE_COMMIT_TRANSACTION: u8 = OPCODE_CALL_BUILT_IN + 1;
const OPCODE_CREATE_RECORD: u8 = OPCODE_COMMIT_TRANSACTION + 1;
const OPCODE_CREATE_RECORD_IF_PENDING: u8 = OPCODE_CREATE_RECORD + 1;
const OPCODE_DELETE_RECORD: u8 = OPCODE_CREATE_RECORD_IF_PENDING + 1;
const OPCODE_DIVIDE: u8 = OPCODE_DELETE_RECORD + 1;
const OPCODE_DUP2: u8 = OPCODE_DIVIDE + 1;
const OPCODE_EQUAL: u8 = OPCODE_DUP2 + 1;
const OPCODE_EXECUTE_QUERY: u8 = OPCODE_EQUAL + 1;
const OPCODE_EXISTS: u8 = OPCODE_EXECUTE_QUERY + 1;
const OPCODE_FIELD_PATH_EXISTS: u8 = OPCODE_EXISTS + 1;
const OPCODE_GREATER_THAN: u8 = OPCODE_FIELD_PATH_EXISTS + 1;
const OPCODE_GREATER_THAN_OR_EQUAL: u8 = OPCODE_GREATER_THAN + 1;
const OPCODE_ITER_HAS_NEXT: u8 = OPCODE_GREATER_THAN_OR_EQUAL + 1;
const OPCODE_ITER_INIT: u8 = OPCODE_ITER_HAS_NEXT + 1;
const OPCODE_ITER_NEXT: u8 = OPCODE_ITER_INIT + 1;
const OPCODE_JUMP: u8 = OPCODE_ITER_NEXT + 1;
const OPCODE_JUMP_IF_FALSE: u8 = OPCODE_JUMP + 1;
const OPCODE_LESS_THAN: u8 = OPCODE_JUMP_IF_FALSE + 1;
const OPCODE_LESS_THAN_OR_EQUAL: u8 = OPCODE_LESS_THAN + 1;
const OPCODE_LOAD_FIELD: u8 = OPCODE_LESS_THAN_OR_EQUAL + 1;
const OPCODE_LOAD_FIELD_PATH: u8 = OPCODE_LOAD_FIELD + 1;
const OPCODE_LOAD_INDEX: u8 = OPCODE_LOAD_FIELD_PATH + 1;
const OPCODE_LOAD_LOCAL: u8 = OPCODE_LOAD_INDEX + 1;
const OPCODE_LOAD_PROJECTED_VALUE: u8 = OPCODE_LOAD_LOCAL + 1;
const OPCODE_LOAD_REFERENCE: u8 = OPCODE_LOAD_PROJECTED_VALUE + 1;
const OPCODE_LOAD_SEQUENCE_CURRENT: u8 = OPCODE_LOAD_REFERENCE + 1;
const OPCODE_LOCKED: u8 = OPCODE_LOAD_SEQUENCE_CURRENT + 1;
const OPCODE_MAKE_ARRAY: u8 = OPCODE_LOCKED + 1;
const OPCODE_MAKE_OBJECT: u8 = OPCODE_MAKE_ARRAY + 1;
const OPCODE_MAKE_RANGE: u8 = OPCODE_MAKE_OBJECT + 1;
const OPCODE_MAKE_RECORD_POINTER: u8 = OPCODE_MAKE_RANGE + 1;
const OPCODE_MAKE_STEPPED_RANGE: u8 = OPCODE_MAKE_RECORD_POINTER + 1;
const OPCODE_MODULO: u8 = OPCODE_MAKE_STEPPED_RANGE + 1;
const OPCODE_MULTIPLY: u8 = OPCODE_MODULO + 1;
const OPCODE_NEGATE: u8 = OPCODE_MULTIPLY + 1;
const OPCODE_NOT: u8 = OPCODE_NEGATE + 1;
const OPCODE_NOT_EQUAL: u8 = OPCODE_NOT + 1;
const OPCODE_OR: u8 = OPCODE_NOT_EQUAL + 1;
const OPCODE_POP: u8 = OPCODE_OR + 1;
const OPCODE_PUSH_BOOLEAN: u8 = OPCODE_POP + 1;
const OPCODE_PUSH_CURRENT_DATE: u8 = OPCODE_PUSH_BOOLEAN + 1;
const OPCODE_PUSH_CURRENT_TIME: u8 = OPCODE_PUSH_CURRENT_DATE + 1;
const OPCODE_PUSH_CURRENT_TIMESTAMP: u8 = OPCODE_PUSH_CURRENT_TIME + 1;
const OPCODE_PUSH_CURRENT_TIMESTAMP_TZ: u8 = OPCODE_PUSH_CURRENT_TIMESTAMP + 1;
const OPCODE_PUSH_CURRENT_TIME_TZ: u8 = OPCODE_PUSH_CURRENT_TIMESTAMP_TZ + 1;
const OPCODE_PUSH_DATE: u8 = OPCODE_PUSH_CURRENT_TIME_TZ + 1;
const OPCODE_PUSH_DECIMAL: u8 = OPCODE_PUSH_DATE + 1;
const OPCODE_PUSH_ENUM_VALUE: u8 = OPCODE_PUSH_DECIMAL + 1;
const OPCODE_PUSH_INTEGER: u8 = OPCODE_PUSH_ENUM_VALUE + 1;
const OPCODE_PUSH_NULL: u8 = OPCODE_PUSH_INTEGER + 1;
const OPCODE_PUSH_TEXT: u8 = OPCODE_PUSH_NULL + 1;
const OPCODE_PUSH_TIME: u8 = OPCODE_PUSH_TEXT + 1;
const OPCODE_PUSH_TIMESTAMP: u8 = OPCODE_PUSH_TIME + 1;
const OPCODE_PUSH_TIMESTAMP_TZ: u8 = OPCODE_PUSH_TIMESTAMP + 1;
const OPCODE_PUSH_TIME_TZ: u8 = OPCODE_PUSH_TIMESTAMP_TZ + 1;
const OPCODE_REORDER_CALL_ARGUMENTS: u8 = OPCODE_PUSH_TIME_TZ + 1;
const OPCODE_RETURN: u8 = OPCODE_REORDER_CALL_ARGUMENTS + 1;
const OPCODE_RETURN_NO_VALUE: u8 = OPCODE_RETURN + 1;
const OPCODE_STORE_FIELD_PATH: u8 = OPCODE_RETURN_NO_VALUE + 1;
const OPCODE_STORE_INDEX: u8 = OPCODE_STORE_FIELD_PATH + 1;
const OPCODE_STORE_LOCAL: u8 = OPCODE_STORE_INDEX + 1;
const OPCODE_STORE_SEQUENCE_CURRENT: u8 = OPCODE_STORE_LOCAL + 1;
const OPCODE_SUBTRACT: u8 = OPCODE_STORE_SEQUENCE_CURRENT + 1;
const OPCODE_UPDATE_RECORD: u8 = OPCODE_SUBTRACT + 1;
const OPCODE_UPDATE_RECORD_IF_CHANGED: u8 = OPCODE_UPDATE_RECORD + 1;
const OPCODE_XOR: u8 = OPCODE_UPDATE_RECORD_IF_CHANGED + 1;

const DATA_TYPE_TAG_ANY: u8 = 1;
const DATA_TYPE_TAG_ARRAY: u8 = DATA_TYPE_TAG_ANY + 1;
const DATA_TYPE_TAG_BOOL: u8 = DATA_TYPE_TAG_ARRAY + 1;
const DATA_TYPE_TAG_DATE: u8 = DATA_TYPE_TAG_BOOL + 1;
const DATA_TYPE_TAG_DEC: u8 = DATA_TYPE_TAG_DATE + 1;
const DATA_TYPE_TAG_EMPTY_ARRAY: u8 = DATA_TYPE_TAG_DEC + 1;
const DATA_TYPE_TAG_INT: u8 = DATA_TYPE_TAG_EMPTY_ARRAY + 1;
const DATA_TYPE_TAG_NULLABLE: u8 = DATA_TYPE_TAG_INT + 1;
const DATA_TYPE_TAG_OBJECT: u8 = DATA_TYPE_TAG_NULLABLE + 1;
const DATA_TYPE_TAG_RANGE: u8 = DATA_TYPE_TAG_OBJECT + 1;
const DATA_TYPE_TAG_RECORD_POINTER: u8 = DATA_TYPE_TAG_RANGE + 1;
const DATA_TYPE_TAG_TEXT: u8 = DATA_TYPE_TAG_RECORD_POINTER + 1;
const DATA_TYPE_TAG_TIME: u8 = DATA_TYPE_TAG_TEXT + 1;
const DATA_TYPE_TAG_TIMESTAMP: u8 = DATA_TYPE_TAG_TIME + 1;
const DATA_TYPE_TAG_TIMESTAMP_TZ: u8 = DATA_TYPE_TAG_TIMESTAMP + 1;
const DATA_TYPE_TAG_TIME_TZ: u8 = DATA_TYPE_TAG_TIMESTAMP_TZ + 1;
const DATA_TYPE_TAG_UNION: u8 = DATA_TYPE_TAG_TIME_TZ + 1;

const QUERY_KIND_SQL: u8 = 1;

const SQL_DIALECT_SQLITE: u8 = 1;
const SQL_DIALECT_POSTGRESQL: u8 = 2;
const SQL_DIALECT_MYSQL: u8 = 3;

const SQL_LOCK_NONE: u8 = 0;
const SQL_LOCK_UPDATE: u8 = 1;
const SQL_LOCK_UPDATE_NO_WAIT: u8 = 2;

const SQL_RECORD_SCHEMA_KNOWN: u8 = 1;
const SQL_RECORD_SCHEMA_RUNTIME: u8 = 2;

const SQL_RESULT_INTEGER_SCALAR: u8 = 1;
const SQL_RESULT_RECORD_POINTER: u8 = 2;
const SQL_RESULT_RECORD_POINTER_ARRAY: u8 = 3;

const SQL_COLUMN_SELECTION_ALL: u8 = 1;
const SQL_COLUMN_SELECTION_INDICES: u8 = 2;
const SQL_COLUMN_SELECTION_RUNTIME: u8 = 3;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ObjectFileError {
	pub offset: usize,
	pub message: String,
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

pub fn write_program_to_path(path: impl AsRef<Path>, program: &Program) -> Result<(), ObjectFileError> {
	let bytes = write_program(program);

	std::fs::write(path, bytes).map_err(|error| ObjectFileError {
		offset: 0,
		message: format!("Failed to write object file: {error}"),
	})
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum ObjectFileSection {
	EntryCode(CodeBody),
	EntryFunction(u32),
	Function(CompiledFunction),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ObjectFileLayout {
	debug: DebugInfo,
	queries: Vec<LoweredBackendQuery>,
	sections: Vec<ObjectFileSection>,
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
				message: format!("Unsupported object file version {version}; expected version {FORMAT_VERSION}."),
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

	fn read_constant_value(&mut self) -> Result<crate::bytecode::Constant, ObjectFileError> {
		let tag_offset = self.offset;
		match self.read_u8()? {
			1 => Ok(crate::bytecode::Constant::Boolean(self.read_bool()?)),
			2 => Ok(crate::bytecode::Constant::Date(crate::value::Date::from_parts(
				self.read_i32()?,
				self.read_u8()?,
				self.read_u8()?,
			).map_err(|message| ObjectFileError {
				offset: self.offset.saturating_sub(6),
				message,
			})?)),
			3 => Ok(crate::bytecode::Constant::Decimal(self.read_decimal()?)),
			4 => Ok(crate::bytecode::Constant::Integer(self.read_i64()?)),
			5 => Ok(crate::bytecode::Constant::Text(self.read_string()?)),
			tag => Err(ObjectFileError {
				offset: tag_offset,
				message: format!("Unknown inline constant tag {tag}."),
			}),
		}
	}

	fn read_data_type(&mut self) -> Result<DataType, ObjectFileError> {
		let tag_offset = self.offset;
		match self.read_u8()? {
			DATA_TYPE_TAG_ANY => Ok(DataType::Any),
			DATA_TYPE_TAG_ARRAY => Ok(DataType::Array(Box::new(self.read_data_type()?))),
			DATA_TYPE_TAG_BOOL => Ok(DataType::Bool),
			DATA_TYPE_TAG_DEC => Ok(DataType::Dec),
			DATA_TYPE_TAG_EMPTY_ARRAY => Ok(DataType::EmptyArray),
			DATA_TYPE_TAG_INT => Ok(DataType::Int),
			DATA_TYPE_TAG_OBJECT => Ok(DataType::Object(self.read_string()?.into())),
			DATA_TYPE_TAG_RANGE => Ok(DataType::Range(Box::new(self.read_data_type()?))),
			DATA_TYPE_TAG_TEXT => Ok(DataType::Text),
			DATA_TYPE_TAG_RECORD_POINTER => Ok(DataType::RecordPointer(crate::ast::RecordPointerType {
				database_name: self.read_string()?,
				schema_name: self.read_string()?,
				table_name: self.read_string()?,
			})),
			DATA_TYPE_TAG_UNION => {
				let member_count = self.read_u32()? as usize;
				let mut members = Vec::with_capacity(member_count);

				for _ in 0..member_count {
					members.push(self.read_data_type()?);
				}

				Ok(DataType::Union(members))
			}
			DATA_TYPE_TAG_DATE => Ok(DataType::Date),
			DATA_TYPE_TAG_NULLABLE => Ok(DataType::Nullable(Box::new(self.read_data_type()?))),
			DATA_TYPE_TAG_TIME => Ok(DataType::Time),
			DATA_TYPE_TAG_TIME_TZ => Ok(DataType::TimeTz),
			DATA_TYPE_TAG_TIMESTAMP => Ok(DataType::Timestamp),
			DATA_TYPE_TAG_TIMESTAMP_TZ => Ok(DataType::TimestampTz),
			tag => Err(ObjectFileError {
				offset: tag_offset,
				message: format!("Unknown data type tag {tag}."),
			}),
		}
	}

	fn read_debug_info(&mut self) -> Result<DebugInfo, ObjectFileError> {
		let source_file_count = self.read_u32()? as usize;
		let mut source_files = Vec::with_capacity(source_file_count);

		for _ in 0..source_file_count {
			let display_name = self.read_string()?;
			let line_start_count = self.read_u32()? as usize;
			let mut line_starts = Vec::with_capacity(line_start_count);

			for _ in 0..line_start_count {
				line_starts.push(self.read_u32()?);
			}

			source_files.push(SourceFileDebugInfo::new(display_name, line_starts));
		}

		let code_body_count = self.read_u32()? as usize;
		let mut code_bodies = Vec::with_capacity(code_body_count);

		for _ in 0..code_body_count {
			let body_name = if self.read_bool()? {
				Some(self.read_string()?)
			}
			else {
				None
			};
			let source_file_index = if self.read_bool()? {
				Some(self.read_u32()?)
			}
			else {
				None
			};
			let position_count = self.read_u32()? as usize;
			let mut positions = Vec::with_capacity(position_count);

			for _ in 0..position_count {
				positions.push(self.read_u32()?);
			}

			let local_count = self.read_u32()? as usize;
			let mut locals = Vec::with_capacity(local_count);

			for _ in 0..local_count {
				let name = self.read_string()?;
				let slot = self.read_u32()?;
				let declared_type = self.read_string()?;
				let is_const = self.read_bool()?;
				let scope_start = self.read_u32()?;
				let scope_end = self.read_u32()?;
				locals.push(LocalVariableDebugInfo::new(
					name,
					slot,
					declared_type,
					is_const,
					scope_start,
					scope_end,
				));
			}

			code_bodies.push(CodeBodyDebugInfo::new(body_name, positions, locals, source_file_index));
		}

		Ok(DebugInfo::new(code_bodies, source_files))
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

	fn read_i32(&mut self) -> Result<i32, ObjectFileError> {
		let mut bytes = [0; 4];
		bytes.copy_from_slice(self.read_exact(4)?);
		Ok(i32::from_le_bytes(bytes))
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
			OPCODE_ADVANCE_SEQUENCE => Ok(Instruction::AdvanceSequence {
				database_name: self.read_string()?,
				schema_is_implicit: self.read_bool()?,
				schema_name: self.read_string()?,
				sequence_name: self.read_string()?,
			}),
			OPCODE_AND => Ok(Instruction::And),
			OPCODE_BEGIN_TRANSACTION => Ok(Instruction::BeginTransaction),
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
			OPCODE_COMMIT_TRANSACTION => Ok(Instruction::CommitTransaction),
			OPCODE_CREATE_RECORD => Ok(Instruction::CreateRecord),
			OPCODE_CREATE_RECORD_IF_PENDING => Ok(Instruction::CreateRecordIfPending),
			OPCODE_DELETE_RECORD => Ok(Instruction::DeleteRecord),
			OPCODE_DIVIDE => Ok(Instruction::Divide),
			OPCODE_DUP2 => Ok(Instruction::Dup2),
			OPCODE_EQUAL => Ok(Instruction::Equal),
			OPCODE_EXECUTE_QUERY => Ok(Instruction::ExecuteQuery(self.read_u32()?)),
			OPCODE_EXISTS => Ok(Instruction::Exists),
			OPCODE_FIELD_PATH_EXISTS => Ok(Instruction::FieldPathExists(self.read_string_vec()?)),
			OPCODE_GREATER_THAN => Ok(Instruction::GreaterThan),
			OPCODE_GREATER_THAN_OR_EQUAL => Ok(Instruction::GreaterThanOrEqual),
			OPCODE_ITER_HAS_NEXT => Ok(Instruction::IterHasNext),
			OPCODE_ITER_INIT => Ok(Instruction::IterInit),
			OPCODE_ITER_NEXT => Ok(Instruction::IterNext),
			OPCODE_JUMP => Ok(Instruction::Jump(self.read_u32()?)),
			OPCODE_JUMP_IF_FALSE => Ok(Instruction::JumpIfFalse(self.read_u32()?)),
			OPCODE_LESS_THAN => Ok(Instruction::LessThan),
			OPCODE_LESS_THAN_OR_EQUAL => Ok(Instruction::LessThanOrEqual),
			OPCODE_LOAD_FIELD => Ok(Instruction::LoadField(self.read_string()?)),
			OPCODE_LOAD_FIELD_PATH => Ok(Instruction::LoadFieldPath(self.read_string_vec()?)),
			OPCODE_LOAD_INDEX => Ok(Instruction::LoadIndex),
			OPCODE_LOAD_LOCAL => Ok(Instruction::LoadLocal(self.read_u32()?)),
			OPCODE_LOAD_PROJECTED_VALUE => Ok(Instruction::LoadProjectedValue(self.read_u32()?)),
			OPCODE_LOAD_REFERENCE => Ok(Instruction::LoadReference(self.read_u32()?)),
			OPCODE_LOAD_SEQUENCE_CURRENT => Ok(Instruction::LoadSequenceCurrent {
				database_name: self.read_string()?,
				schema_is_implicit: self.read_bool()?,
				schema_name: self.read_string()?,
				sequence_name: self.read_string()?,
			}),
			OPCODE_LOCKED => Ok(Instruction::Locked),
			OPCODE_MAKE_ARRAY => Ok(Instruction::MakeArray(self.read_u32()?)),
			OPCODE_MAKE_OBJECT => Ok(Instruction::MakeObject {
				object_type_id: ObjectTypeId::from_raw(self.read_u32()?),
				field_names: self.read_string_vec()?,
			}),
			OPCODE_MAKE_RECORD_POINTER => Ok(Instruction::MakeRecordPointer {
				field_names: self.read_string_vec()?,
				field_types: {
					let field_count = self.read_u32()? as usize;
					let mut field_types = Vec::with_capacity(field_count);

					for _ in 0..field_count {
						field_types.push(self.read_data_type()?);
					}

					field_types
				},
				record_type: crate::ast::RecordPointerType {
					database_name: self.read_string()?,
					schema_name: self.read_string()?,
					table_name: self.read_string()?,
				},
				schema_is_implicit: self.read_bool()?,
			}),
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
			OPCODE_PUSH_CURRENT_DATE => Ok(Instruction::PushCurrentDate),
			OPCODE_PUSH_CURRENT_TIME => Ok(Instruction::PushCurrentTime),
			OPCODE_PUSH_CURRENT_TIME_TZ => Ok(Instruction::PushCurrentTimeTz),
			OPCODE_PUSH_CURRENT_TIMESTAMP => Ok(Instruction::PushCurrentTimestamp),
			OPCODE_PUSH_CURRENT_TIMESTAMP_TZ => Ok(Instruction::PushCurrentTimestampTz),
			OPCODE_PUSH_DATE => Ok(Instruction::PushDate(crate::value::Date::from_parts(
				self.read_i32()?,
				self.read_u8()?,
				self.read_u8()?,
			).map_err(|message| ObjectFileError {
				offset: self.offset.saturating_sub(6),
				message,
			})?)),
			OPCODE_PUSH_DECIMAL => Ok(Instruction::PushDecimal(self.read_decimal()?)),
			OPCODE_PUSH_ENUM_VALUE => Ok(Instruction::PushEnumValue {
				backing_value: self.read_constant_value()?,
				enum_name: self.read_string()?,
				variant_name: self.read_string()?,
			}),
			OPCODE_PUSH_INTEGER => Ok(Instruction::PushInteger(self.read_i64()?)),
			OPCODE_PUSH_NULL => Ok(Instruction::PushNull),
			OPCODE_PUSH_TEXT => Ok(Instruction::PushText(self.read_string()?)),
			OPCODE_PUSH_TIME => Ok(Instruction::PushTime(crate::value::Time::from_iso_text(&self.read_string()?).map_err(|message| ObjectFileError {
				offset: self.offset,
				message,
			})?)),
			OPCODE_PUSH_TIME_TZ => Ok(Instruction::PushTimeTz(crate::value::TimeTz::from_iso_text(&self.read_string()?).map_err(|message| ObjectFileError {
				offset: self.offset,
				message,
			})?)),
			OPCODE_PUSH_TIMESTAMP => Ok(Instruction::PushTimestamp(crate::value::Timestamp::from_iso_text(&self.read_string()?).map_err(|message| ObjectFileError {
				offset: self.offset,
				message,
			})?)),
			OPCODE_PUSH_TIMESTAMP_TZ => Ok(Instruction::PushTimestampTz(crate::value::TimestampTz::from_iso_text(&self.read_string()?).map_err(|message| ObjectFileError {
				offset: self.offset,
				message,
			})?)),
			OPCODE_REORDER_CALL_ARGUMENTS => {
				let argument_count = self.read_u32()? as usize;
				let mut argument_order = Vec::with_capacity(argument_count);

				for _ in 0..argument_count {
					argument_order.push(self.read_u32()?);
				}

				Ok(Instruction::ReorderCallArguments(argument_order))
			}
			OPCODE_RETURN => Ok(Instruction::Return),
			OPCODE_RETURN_NO_VALUE => Ok(Instruction::ReturnNoValue),
			OPCODE_STORE_FIELD_PATH => Ok(Instruction::StoreFieldPath(self.read_string_vec()?)),
			OPCODE_STORE_INDEX => Ok(Instruction::StoreIndex),
			OPCODE_STORE_LOCAL => Ok(Instruction::StoreLocal(self.read_u32()?)),
			OPCODE_STORE_SEQUENCE_CURRENT => Ok(Instruction::StoreSequenceCurrent {
				database_name: self.read_string()?,
				schema_is_implicit: self.read_bool()?,
				schema_name: self.read_string()?,
				sequence_name: self.read_string()?,
			}),
			OPCODE_SUBTRACT => Ok(Instruction::Subtract),
			OPCODE_UPDATE_RECORD => Ok(Instruction::UpdateRecord),
			OPCODE_UPDATE_RECORD_IF_CHANGED => Ok(Instruction::UpdateRecordIfChanged),
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

		for _ in 0..function_count {
			let name = if self.read_bool()? {
				Some(self.read_string()?)
			}
			else {
				None
			};

			let return_type = if self.read_bool()? {
				Some(self.read_data_type()?)
			}
			else {
				None
			};

			sections.push(ObjectFileSection::Function(CompiledFunction::new(
				name,
				return_type,
				self.read_code_body()?,
			)));
		}

		match self.read_u8()? {
			0 => sections.push(ObjectFileSection::EntryCode(self.read_code_body()?)),
			1 => sections.push(ObjectFileSection::EntryFunction(self.read_u32()?)),
			kind => {
				return Err(ObjectFileError {
					offset: self.offset - 1,
					message: format!("Unknown entry point kind {kind}."),
				});
			}
		}

		let query_count = self.read_u32()? as usize;
		let mut queries = Vec::with_capacity(query_count);

		for _ in 0..query_count {
			queries.push(self.read_lowered_query()?);
		}

		let debug = if self.is_at_end() {
			DebugInfo::default()
		}
		else {
			self.read_debug_info()?
		};

		Ok(ObjectFileLayout {
			debug,
			queries,
			sections,
		})
	}

	fn read_lowered_query(&mut self) -> Result<LoweredBackendQuery, ObjectFileError> {
		let kind_offset = self.offset;
		match self.read_u8()? {
			QUERY_KIND_SQL => Ok(LoweredBackendQuery::Sql(self.read_sql_query()?)),
			kind => Err(ObjectFileError {
				offset: kind_offset,
				message: format!("Unknown query kind {kind}."),
			}),
		}
	}

	fn read_query_record_layout(&mut self) -> Result<QueryRecordLayout, ObjectFileError> {
		let schema_offset = self.offset;
		let schema = match self.read_u8()? {
			SQL_RECORD_SCHEMA_KNOWN => {
				let column_count = self.read_u32()? as usize;
				let mut columns = Vec::with_capacity(column_count);

				for _ in 0..column_count {
					columns.push(QueryResultColumn {
						column_name: self.read_string()?,
						data_type: self.read_data_type()?,
						is_nullable: self.read_bool()?,
						is_primary_key: self.read_bool()?,
					});
				}

				QueryRecordSchema::Known(columns)
			}
			SQL_RECORD_SCHEMA_RUNTIME => QueryRecordSchema::RuntimeDetermined,
			kind => return Err(ObjectFileError {
				offset: schema_offset,
				message: format!("Unknown SQL record schema kind {kind}."),
			}),
		};
		let selection_offset = self.offset;
		let selection = match self.read_u8()? {
			SQL_COLUMN_SELECTION_ALL => QueryColumnSelection::All,
			SQL_COLUMN_SELECTION_INDICES => {
				let index_count = self.read_u32()? as usize;
				let mut indices = Vec::with_capacity(index_count);

				for _ in 0..index_count {
					indices.push(self.read_u32()?);
				}

				QueryColumnSelection::Indices(indices)
			}
			SQL_COLUMN_SELECTION_RUNTIME => QueryColumnSelection::RuntimeDetermined,
			kind => return Err(ObjectFileError {
				offset: selection_offset,
				message: format!("Unknown SQL column selection kind {kind}."),
			}),
		};

		Ok(QueryRecordLayout { schema, selection })
	}

	fn read_sql_query(&mut self) -> Result<SqlQuery, ObjectFileError> {
		let dialect_offset = self.offset;
		let dialect = match self.read_u8()? {
			SQL_DIALECT_SQLITE => SqlDialect::Sqlite,
			SQL_DIALECT_POSTGRESQL => SqlDialect::PostgreSql,
			SQL_DIALECT_MYSQL => SqlDialect::MySql,
			dialect => {
				return Err(ObjectFileError {
					offset: dialect_offset,
					message: format!("Unknown SQL dialect {dialect}."),
				});
			}
		};
		let lock_mode_offset = self.offset;
		let lock_mode = match self.read_u8()? {
			SQL_LOCK_NONE => RecordLockMode::None,
			SQL_LOCK_UPDATE => RecordLockMode::Update,
			SQL_LOCK_UPDATE_NO_WAIT => RecordLockMode::UpdateNoWait,
			mode => {
				return Err(ObjectFileError {
					offset: lock_mode_offset,
					message: format!("Unknown SQL record lock mode {mode}."),
				});
			}
		};
		let database_name = self.read_string()?;
		let statement = self.read_string()?;
		let result_shape = match self.read_u8()? {
			SQL_RESULT_INTEGER_SCALAR => SqlQueryResultShape::IntegerScalar,
			SQL_RESULT_RECORD_POINTER => SqlQueryResultShape::RecordPointer(self.read_query_record_layout()?),
			SQL_RESULT_RECORD_POINTER_ARRAY => SqlQueryResultShape::RecordPointerArray(self.read_query_record_layout()?),
			kind => {
				return Err(ObjectFileError {
					offset: self.offset - 1,
					message: format!("Unknown SQL query result kind {kind}."),
				});
			}
		};
		let parameter_count = self.read_u32()? as usize;
		let mut parameters = Vec::with_capacity(parameter_count);

		for _ in 0..parameter_count {
			parameters.push(SqlParameter {
				data_type: self.read_data_type()?,
				field_path: self.read_string_vec()?,
				index: self.read_u32()?,
				slot: self.read_u32()?,
			});
		}
		let group_by_count = self.read_u32()? as usize;
		let mut group_by = Vec::with_capacity(group_by_count);

		for _ in 0..group_by_count {
			group_by.push(SqlGroupByItem {
				data_type: self.read_data_type()?,
				key_names: self.read_string_vec()?,
			});
		}
		let scalar_projection_count = self.read_u32()? as usize;
		let mut scalar_projections = Vec::with_capacity(scalar_projection_count);

		for _ in 0..scalar_projection_count {
			scalar_projections.push(SqlScalarProjection {
				column_index: self.read_u32()?,
				data_type: self.read_data_type()?,
				value_id: QueryProjectedValueId(self.read_u32()?),
			});
		}

		Ok(SqlQuery {
			database_name,
			dialect,
			group_by,
			lock_mode,
			parameters,
			result_shape,
			scalar_projections,
			schema_is_implicit: self.read_bool()?,
			schema_name: self.read_string()?,
			statement,
			table_name: self.read_string()?,
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

	fn read_string_vec(&mut self) -> Result<Vec<String>, ObjectFileError> {
		let count = self.read_u32()? as usize;
		let mut values = Vec::with_capacity(count);

		for _ in 0..count {
			values.push(self.read_string()?);
		}

		Ok(values)
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

fn write_code_body(bytes: &mut Vec<u8>, code_body: &CodeBody) {
	bytes.extend_from_slice(&(code_body.instructions.len() as u32).to_le_bytes());

	for instruction in &code_body.instructions {
		write_instruction(bytes, instruction);
	}
}

fn write_data_type(bytes: &mut Vec<u8>, data_type: &DataType) {
	match data_type {
		DataType::Any => bytes.push(DATA_TYPE_TAG_ANY),
		DataType::Array(element_type) => {
			bytes.push(DATA_TYPE_TAG_ARRAY);
			write_data_type(bytes, element_type);
		}
		DataType::Bool => bytes.push(DATA_TYPE_TAG_BOOL),
		DataType::Date => bytes.push(DATA_TYPE_TAG_DATE),
		DataType::Dec => bytes.push(DATA_TYPE_TAG_DEC),
		DataType::EmptyArray => bytes.push(DATA_TYPE_TAG_EMPTY_ARRAY),
		DataType::Int => bytes.push(DATA_TYPE_TAG_INT),
		DataType::Null => panic!("internal data type `null` must not be serialized into object files"),
		DataType::Nullable(inner) => {
			bytes.push(DATA_TYPE_TAG_NULLABLE);
			write_data_type(bytes, inner);
		}
		DataType::Object(name) => {
			bytes.push(DATA_TYPE_TAG_OBJECT);
			bytes.extend_from_slice(&(name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(name.as_bytes());
		}
		DataType::Range(element_type) => {
			bytes.push(DATA_TYPE_TAG_RANGE);
			write_data_type(bytes, element_type);
		}
		DataType::Text => bytes.push(DATA_TYPE_TAG_TEXT),
		DataType::Time => bytes.push(DATA_TYPE_TAG_TIME),
		DataType::TimeTz => bytes.push(DATA_TYPE_TAG_TIME_TZ),
		DataType::Timestamp => bytes.push(DATA_TYPE_TAG_TIMESTAMP),
		DataType::TimestampTz => bytes.push(DATA_TYPE_TAG_TIMESTAMP_TZ),
		DataType::RecordPointer(record_pointer) => {
			bytes.push(DATA_TYPE_TAG_RECORD_POINTER);
			for value in [&record_pointer.database_name, &record_pointer.schema_name, &record_pointer.table_name] {
				bytes.extend_from_slice(&(value.len() as u32).to_le_bytes());
				bytes.extend_from_slice(value.as_bytes());
			}
		}
		DataType::Union(members) => {
			bytes.push(DATA_TYPE_TAG_UNION);
			bytes.extend_from_slice(&(members.len() as u32).to_le_bytes());

			for member in members {
				write_data_type(bytes, member);
			}
		}
	}
}

fn write_instruction(bytes: &mut Vec<u8>, instruction: &Instruction) {
	match instruction {
		Instruction::Add => bytes.push(OPCODE_ADD),
		Instruction::AdvanceSequence { database_name, schema_is_implicit, schema_name, sequence_name } => {
			bytes.push(OPCODE_ADVANCE_SEQUENCE);
			bytes.extend_from_slice(&(database_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(database_name.as_bytes());
			bytes.push(u8::from(*schema_is_implicit));
			bytes.extend_from_slice(&(schema_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(schema_name.as_bytes());
			bytes.extend_from_slice(&(sequence_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(sequence_name.as_bytes());
		}
		Instruction::And => bytes.push(OPCODE_AND),
		Instruction::BeginTransaction => bytes.push(OPCODE_BEGIN_TRANSACTION),
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
		Instruction::CommitTransaction => bytes.push(OPCODE_COMMIT_TRANSACTION),
		Instruction::CreateRecord => bytes.push(OPCODE_CREATE_RECORD),
		Instruction::CreateRecordIfPending => bytes.push(OPCODE_CREATE_RECORD_IF_PENDING),
		Instruction::DeleteRecord => bytes.push(OPCODE_DELETE_RECORD),
		Instruction::Divide => bytes.push(OPCODE_DIVIDE),
		Instruction::Dup2 => bytes.push(OPCODE_DUP2),
		Instruction::Equal => bytes.push(OPCODE_EQUAL),
		Instruction::ExecuteQuery(query_index) => {
			bytes.push(OPCODE_EXECUTE_QUERY);
			bytes.extend_from_slice(&query_index.to_le_bytes());
		}
		Instruction::Exists => bytes.push(OPCODE_EXISTS),
		Instruction::FieldPathExists(field_path) => {
			bytes.push(OPCODE_FIELD_PATH_EXISTS);
			bytes.extend_from_slice(&(field_path.len() as u32).to_le_bytes());

			for field_name in field_path {
				bytes.extend_from_slice(&(field_name.len() as u32).to_le_bytes());
				bytes.extend_from_slice(field_name.as_bytes());
			}
		}
		Instruction::GreaterThan => bytes.push(OPCODE_GREATER_THAN),
		Instruction::GreaterThanOrEqual => bytes.push(OPCODE_GREATER_THAN_OR_EQUAL),
		Instruction::IterHasNext => bytes.push(OPCODE_ITER_HAS_NEXT),
		Instruction::IterInit => bytes.push(OPCODE_ITER_INIT),
		Instruction::IterNext => bytes.push(OPCODE_ITER_NEXT),
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
		Instruction::LoadField(field_name) => {
			bytes.push(OPCODE_LOAD_FIELD);
			bytes.extend_from_slice(&(field_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(field_name.as_bytes());
		}
		Instruction::LoadFieldPath(field_path) => {
			bytes.push(OPCODE_LOAD_FIELD_PATH);
			bytes.extend_from_slice(&(field_path.len() as u32).to_le_bytes());

			for field_name in field_path {
				bytes.extend_from_slice(&(field_name.len() as u32).to_le_bytes());
				bytes.extend_from_slice(field_name.as_bytes());
			}
		}
		Instruction::LoadIndex => bytes.push(OPCODE_LOAD_INDEX),
		Instruction::LoadLocal(slot) => {
			bytes.push(OPCODE_LOAD_LOCAL);
			bytes.extend_from_slice(&slot.to_le_bytes());
		}
		Instruction::LoadProjectedValue(value_id) => {
			bytes.push(OPCODE_LOAD_PROJECTED_VALUE);
			bytes.extend_from_slice(&value_id.to_le_bytes());
		}
		Instruction::LoadReference(slot) => {
			bytes.push(OPCODE_LOAD_REFERENCE);
			bytes.extend_from_slice(&slot.to_le_bytes());
		}
		Instruction::LoadSequenceCurrent { database_name, schema_is_implicit, schema_name, sequence_name } => {
			bytes.push(OPCODE_LOAD_SEQUENCE_CURRENT);
			bytes.extend_from_slice(&(database_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(database_name.as_bytes());
			bytes.push(u8::from(*schema_is_implicit));
			bytes.extend_from_slice(&(schema_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(schema_name.as_bytes());
			bytes.extend_from_slice(&(sequence_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(sequence_name.as_bytes());
		}
		Instruction::Locked => bytes.push(OPCODE_LOCKED),
		Instruction::MakeArray(element_count) => {
			bytes.push(OPCODE_MAKE_ARRAY);
			bytes.extend_from_slice(&element_count.to_le_bytes());
		}
		Instruction::MakeObject { field_names, object_type_id } => {
			bytes.push(OPCODE_MAKE_OBJECT);
			bytes.extend_from_slice(&object_type_id.raw().to_le_bytes());
			bytes.extend_from_slice(&(field_names.len() as u32).to_le_bytes());

			for field_name in field_names {
				bytes.extend_from_slice(&(field_name.len() as u32).to_le_bytes());
				bytes.extend_from_slice(field_name.as_bytes());
			}
		}
		Instruction::MakeRange => bytes.push(OPCODE_MAKE_RANGE),
		Instruction::MakeRecordPointer {
			field_names,
			field_types,
			record_type,
			schema_is_implicit,
		} => {
			bytes.push(OPCODE_MAKE_RECORD_POINTER);
			bytes.extend_from_slice(&(field_names.len() as u32).to_le_bytes());

			for field_name in field_names {
				bytes.extend_from_slice(&(field_name.len() as u32).to_le_bytes());
				bytes.extend_from_slice(field_name.as_bytes());
			}
			bytes.extend_from_slice(&(field_types.len() as u32).to_le_bytes());

			for field_type in field_types {
				write_data_type(bytes, field_type);
			}

			bytes.extend_from_slice(&(record_type.database_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(record_type.database_name.as_bytes());
			bytes.extend_from_slice(&(record_type.schema_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(record_type.schema_name.as_bytes());
			bytes.extend_from_slice(&(record_type.table_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(record_type.table_name.as_bytes());
			bytes.push(u8::from(*schema_is_implicit));
		}
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
		Instruction::PushCurrentDate => bytes.push(OPCODE_PUSH_CURRENT_DATE),
		Instruction::PushCurrentTime => bytes.push(OPCODE_PUSH_CURRENT_TIME),
		Instruction::PushCurrentTimeTz => bytes.push(OPCODE_PUSH_CURRENT_TIME_TZ),
		Instruction::PushCurrentTimestamp => bytes.push(OPCODE_PUSH_CURRENT_TIMESTAMP),
		Instruction::PushCurrentTimestampTz => bytes.push(OPCODE_PUSH_CURRENT_TIMESTAMP_TZ),
		Instruction::PushDate(value) => {
			bytes.push(OPCODE_PUSH_DATE);
			bytes.extend_from_slice(&value.year.to_le_bytes());
			bytes.push(value.month);
			bytes.push(value.day);
		}
		Instruction::PushTime(value) => {
			bytes.push(OPCODE_PUSH_TIME);
			let text = value.to_string();
			bytes.extend_from_slice(&(text.len() as u32).to_le_bytes());
			bytes.extend_from_slice(text.as_bytes());
		}
		Instruction::PushTimeTz(value) => {
			bytes.push(OPCODE_PUSH_TIME_TZ);
			let text = value.to_string();
			bytes.extend_from_slice(&(text.len() as u32).to_le_bytes());
			bytes.extend_from_slice(text.as_bytes());
		}
		Instruction::PushTimestamp(value) => {
			bytes.push(OPCODE_PUSH_TIMESTAMP);
			let text = value.to_string();
			bytes.extend_from_slice(&(text.len() as u32).to_le_bytes());
			bytes.extend_from_slice(text.as_bytes());
		}
		Instruction::PushTimestampTz(value) => {
			bytes.push(OPCODE_PUSH_TIMESTAMP_TZ);
			let text = value.to_string();
			bytes.extend_from_slice(&(text.len() as u32).to_le_bytes());
			bytes.extend_from_slice(text.as_bytes());
		}
		Instruction::PushDecimal(value) => {
			bytes.push(OPCODE_PUSH_DECIMAL);
			bytes.extend_from_slice(&value.coefficient.to_le_bytes());
			bytes.push(value.precision);
			bytes.push(value.scale);
		}
		Instruction::PushEnumValue {
			backing_value,
			enum_name,
			variant_name,
		} => {
			bytes.push(OPCODE_PUSH_ENUM_VALUE);
			write_inline_constant(bytes, backing_value);
			bytes.extend_from_slice(&(enum_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(enum_name.as_bytes());
			bytes.extend_from_slice(&(variant_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(variant_name.as_bytes());
		}
		Instruction::PushInteger(value) => {
			bytes.push(OPCODE_PUSH_INTEGER);
			bytes.extend_from_slice(&value.to_le_bytes());
		}
		Instruction::PushNull => bytes.push(OPCODE_PUSH_NULL),
		Instruction::PushText(value) => {
			bytes.push(OPCODE_PUSH_TEXT);
			bytes.extend_from_slice(&(value.len() as u32).to_le_bytes());
			bytes.extend_from_slice(value.as_bytes());
		}
		Instruction::ReorderCallArguments(argument_order) => {
			bytes.push(OPCODE_REORDER_CALL_ARGUMENTS);
			bytes.extend_from_slice(&(argument_order.len() as u32).to_le_bytes());

			for argument_index in argument_order {
				bytes.extend_from_slice(&argument_index.to_le_bytes());
			}
		}
		Instruction::Return => bytes.push(OPCODE_RETURN),
		Instruction::ReturnNoValue => bytes.push(OPCODE_RETURN_NO_VALUE),
		Instruction::StoreFieldPath(field_path) => {
			bytes.push(OPCODE_STORE_FIELD_PATH);
			bytes.extend_from_slice(&(field_path.len() as u32).to_le_bytes());

			for field_name in field_path {
				bytes.extend_from_slice(&(field_name.len() as u32).to_le_bytes());
				bytes.extend_from_slice(field_name.as_bytes());
			}
		}
		Instruction::StoreIndex => bytes.push(OPCODE_STORE_INDEX),
		Instruction::StoreLocal(slot) => {
			bytes.push(OPCODE_STORE_LOCAL);
			bytes.extend_from_slice(&slot.to_le_bytes());
		}
		Instruction::StoreSequenceCurrent { database_name, schema_is_implicit, schema_name, sequence_name } => {
			bytes.push(OPCODE_STORE_SEQUENCE_CURRENT);
			bytes.extend_from_slice(&(database_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(database_name.as_bytes());
			bytes.push(u8::from(*schema_is_implicit));
			bytes.extend_from_slice(&(schema_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(schema_name.as_bytes());
			bytes.extend_from_slice(&(sequence_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(sequence_name.as_bytes());
		}
		Instruction::Subtract => bytes.push(OPCODE_SUBTRACT),
		Instruction::UpdateRecord => bytes.push(OPCODE_UPDATE_RECORD),
		Instruction::UpdateRecordIfChanged => bytes.push(OPCODE_UPDATE_RECORD_IF_CHANGED),
		Instruction::Xor => bytes.push(OPCODE_XOR),
	}
}

fn write_inline_constant(bytes: &mut Vec<u8>, constant: &crate::bytecode::Constant) {
	match constant {
		crate::bytecode::Constant::Boolean(value) => {
			bytes.push(1);
			bytes.push(u8::from(*value));
		}
		crate::bytecode::Constant::Date(value) => {
			bytes.push(2);
			bytes.extend_from_slice(&value.year.to_le_bytes());
			bytes.push(value.month);
			bytes.push(value.day);
		}
		crate::bytecode::Constant::Decimal(value) => {
			bytes.push(3);
			bytes.extend_from_slice(&value.coefficient.to_le_bytes());
			bytes.push(value.precision);
			bytes.push(value.scale);
		}
		crate::bytecode::Constant::Integer(value) => {
			bytes.push(4);
			bytes.extend_from_slice(&value.to_le_bytes());
		}
		crate::bytecode::Constant::Text(value) => {
			bytes.push(5);
			bytes.extend_from_slice(&(value.len() as u32).to_le_bytes());
			bytes.extend_from_slice(value.as_bytes());
		}
	}
}

fn write_lowered_query(bytes: &mut Vec<u8>, query: &LoweredBackendQuery) {
	match query {
		LoweredBackendQuery::Sql(query) => {
			bytes.push(QUERY_KIND_SQL);
			write_sql_query(bytes, query);
		}
	}
}

fn write_sql_query(bytes: &mut Vec<u8>, query: &SqlQuery) {
	bytes.push(match query.dialect {
		SqlDialect::MySql => SQL_DIALECT_MYSQL,
		SqlDialect::PostgreSql => SQL_DIALECT_POSTGRESQL,
		SqlDialect::Sqlite => SQL_DIALECT_SQLITE,
	});
	bytes.push(match query.lock_mode {
		RecordLockMode::None => SQL_LOCK_NONE,
		RecordLockMode::Update => SQL_LOCK_UPDATE,
		RecordLockMode::UpdateNoWait => SQL_LOCK_UPDATE_NO_WAIT,
	});
	bytes.extend_from_slice(&(query.database_name.len() as u32).to_le_bytes());
	bytes.extend_from_slice(query.database_name.as_bytes());
	bytes.extend_from_slice(&(query.statement.len() as u32).to_le_bytes());
	bytes.extend_from_slice(query.statement.as_bytes());

	match &query.result_shape {
		SqlQueryResultShape::IntegerScalar => bytes.push(SQL_RESULT_INTEGER_SCALAR),
		SqlQueryResultShape::RecordPointer(layout) => {
			bytes.push(SQL_RESULT_RECORD_POINTER);
			write_query_record_layout(bytes, layout);
		}
		SqlQueryResultShape::RecordPointerArray(layout) => {
			bytes.push(SQL_RESULT_RECORD_POINTER_ARRAY);
			write_query_record_layout(bytes, layout);
		}
	}

	bytes.extend_from_slice(&(query.parameters.len() as u32).to_le_bytes());

	for parameter in &query.parameters {
		write_data_type(bytes, &parameter.data_type);
		bytes.extend_from_slice(&(parameter.field_path.len() as u32).to_le_bytes());
		for component in &parameter.field_path {
			bytes.extend_from_slice(&(component.len() as u32).to_le_bytes());
			bytes.extend_from_slice(component.as_bytes());
		}
		bytes.extend_from_slice(&parameter.index.to_le_bytes());
		bytes.extend_from_slice(&parameter.slot.to_le_bytes());
	}
	bytes.extend_from_slice(&(query.group_by.len() as u32).to_le_bytes());
	for item in &query.group_by {
		write_data_type(bytes, &item.data_type);
		bytes.extend_from_slice(&(item.key_names.len() as u32).to_le_bytes());
		for key_name in &item.key_names {
			bytes.extend_from_slice(&(key_name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(key_name.as_bytes());
		}
	}
	bytes.extend_from_slice(&(query.scalar_projections.len() as u32).to_le_bytes());
	for projection in &query.scalar_projections {
		bytes.extend_from_slice(&projection.column_index.to_le_bytes());
		write_data_type(bytes, &projection.data_type);
		bytes.extend_from_slice(&projection.value_id.0.to_le_bytes());
	}
	bytes.push(u8::from(query.schema_is_implicit));
	bytes.extend_from_slice(&(query.schema_name.len() as u32).to_le_bytes());
	bytes.extend_from_slice(query.schema_name.as_bytes());
	bytes.extend_from_slice(&(query.table_name.len() as u32).to_le_bytes());
	bytes.extend_from_slice(query.table_name.as_bytes());
}

impl ObjectFileLayout {
	fn from_program(program: &Program) -> Self {
		debug_assert!(
			program.constant_pool().is_empty(),
			"The current object file format does not yet serialize constant-pool entries."
		);

		let mut sections = Vec::with_capacity(program.functions().len() + 1);

		for function in program.functions() {
			sections.push(ObjectFileSection::Function(function.clone()));
		}

		match program.entry_point() {
			EntryPoint::Code(code_body) => sections.push(ObjectFileSection::EntryCode(code_body.clone())),
			EntryPoint::Function(function_index) => sections.push(ObjectFileSection::EntryFunction(*function_index)),
		}

		Self {
			debug: program.debug_info().clone(),
			queries: program.queries().to_vec(),
			sections,
		}
	}

	fn into_program(self) -> Result<Program, ObjectFileError> {
		let mut entry_code = None;
		let mut entry_function = None;
		let mut functions = Vec::new();

		for section in self.sections {
			match section {
				ObjectFileSection::EntryFunction(function_index) => {
					if entry_code.is_some() || entry_function.is_some() {
						return Err(ObjectFileError {
							offset: 0,
							message: String::from("Object file contains more than one entry point section."),
						});
					}

					entry_function = Some(function_index);
				}
				ObjectFileSection::Function(function) => {
					functions.push(function);
				}
				ObjectFileSection::EntryCode(code_body) => {
					if entry_code.is_some() || entry_function.is_some() {
						return Err(ObjectFileError {
							offset: 0,
							message: String::from("Object file contains more than one entry point section."),
						});
					}

					entry_code = Some(code_body);
				}
			}
		}

		if let Some(function_index) = entry_function {
			return Ok(Program::from_entry_function_with_queries(
				ConstantPool::default(),
				function_index,
				functions,
				self.queries,
				self.debug,
			));
		}

		let entry_code = entry_code.ok_or(ObjectFileError {
			offset: 0,
			message: String::from("Object file does not contain an entry point section."),
		})?;

		Ok(Program::from_parts_with_functions_queries_and_debug(
			ConstantPool::default(),
			entry_code,
			functions,
			self.queries,
			self.debug,
		))
	}

	fn write_to(&self, bytes: &mut Vec<u8>) {
		let function_count = self.sections.iter()
			.filter(|section| matches!(section, ObjectFileSection::Function(_)))
			.count() as u32;
		bytes.extend_from_slice(&function_count.to_le_bytes());

		for section in &self.sections {
			match section {
				ObjectFileSection::EntryFunction(_) => {}
				ObjectFileSection::Function(function) => {
					bytes.push(u8::from(function.name().is_some()));
					if let Some(name) = function.name() {
						bytes.extend_from_slice(&(name.len() as u32).to_le_bytes());
						bytes.extend_from_slice(name.as_bytes());
					}

					bytes.push(u8::from(function.return_type().is_some()));
					if let Some(return_type) = function.return_type() {
						write_data_type(bytes, return_type);
					}

					write_code_body(bytes, function.body());
				}
				ObjectFileSection::EntryCode(_) => {}
			}
		}

		match self.sections.iter().find(|section| !matches!(section, ObjectFileSection::Function(_))) {
			Some(ObjectFileSection::EntryCode(code_body)) => {
				bytes.push(0);
				write_code_body(bytes, code_body);
			}
			Some(ObjectFileSection::EntryFunction(function_index)) => {
				bytes.push(1);
				bytes.extend_from_slice(&function_index.to_le_bytes());
			}
			Some(ObjectFileSection::Function(_)) | None => unreachable!("Object file layout must include a non-function entry point section."),
		}

		bytes.extend_from_slice(&(self.queries.len() as u32).to_le_bytes());
		for query in &self.queries {
			write_lowered_query(bytes, query);
		}

		bytes.extend_from_slice(&(self.debug.source_files().len() as u32).to_le_bytes());

		for source_file in self.debug.source_files() {
			bytes.extend_from_slice(&(source_file.display_name().len() as u32).to_le_bytes());
			bytes.extend_from_slice(source_file.display_name().as_bytes());
			bytes.extend_from_slice(&(source_file.line_starts().len() as u32).to_le_bytes());

			for line_start in source_file.line_starts() {
				bytes.extend_from_slice(&line_start.to_le_bytes());
			}
		}

		bytes.extend_from_slice(&(self.debug.code_bodies().len() as u32).to_le_bytes());

		for code_body in self.debug.code_bodies() {
			bytes.push(u8::from(code_body.body_name().is_some()));
			if let Some(body_name) = code_body.body_name() {
				bytes.extend_from_slice(&(body_name.len() as u32).to_le_bytes());
				bytes.extend_from_slice(body_name.as_bytes());
			}

			bytes.push(u8::from(code_body.source_file_index().is_some()));
			if let Some(source_file_index) = code_body.source_file_index() {
				bytes.extend_from_slice(&source_file_index.to_le_bytes());
			}

			bytes.extend_from_slice(&(code_body.instruction_positions().len() as u32).to_le_bytes());
			for position in code_body.instruction_positions() {
				bytes.extend_from_slice(&position.to_le_bytes());
			}

			bytes.extend_from_slice(&(code_body.locals().len() as u32).to_le_bytes());
			for local in code_body.locals() {
				bytes.extend_from_slice(&(local.name().len() as u32).to_le_bytes());
				bytes.extend_from_slice(local.name().as_bytes());
				bytes.extend_from_slice(&local.slot().to_le_bytes());
				bytes.extend_from_slice(&(local.declared_type().len() as u32).to_le_bytes());
				bytes.extend_from_slice(local.declared_type().as_bytes());
				bytes.push(u8::from(local.is_const()));
				bytes.extend_from_slice(&local.scope_start().to_le_bytes());
				bytes.extend_from_slice(&local.scope_end().to_le_bytes());
			}
		}
	}
}

fn write_query_record_layout(bytes: &mut Vec<u8>, layout: &QueryRecordLayout) {
	match &layout.schema {
		QueryRecordSchema::Known(columns) => {
			bytes.push(SQL_RECORD_SCHEMA_KNOWN);
			bytes.extend_from_slice(&(columns.len() as u32).to_le_bytes());

			for column in columns {
				bytes.extend_from_slice(&(column.column_name.len() as u32).to_le_bytes());
				bytes.extend_from_slice(column.column_name.as_bytes());
				write_data_type(bytes, &column.data_type);
				bytes.push(if column.is_nullable { 1 } else { 0 });
				bytes.push(if column.is_primary_key { 1 } else { 0 });
			}
		}
		QueryRecordSchema::RuntimeDetermined => bytes.push(SQL_RECORD_SCHEMA_RUNTIME),
	}

	match &layout.selection {
		QueryColumnSelection::All => bytes.push(SQL_COLUMN_SELECTION_ALL),
		QueryColumnSelection::Indices(indices) => {
			bytes.push(SQL_COLUMN_SELECTION_INDICES);
			bytes.extend_from_slice(&(indices.len() as u32).to_le_bytes());

			for index in indices {
				bytes.extend_from_slice(&index.to_le_bytes());
			}
		}
		QueryColumnSelection::RuntimeDetermined => bytes.push(SQL_COLUMN_SELECTION_RUNTIME),
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	use crate::ast::*;

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
		let mut bytes = write_program(&Program::new(vec![
			Instruction::PushInteger(1),
		]));
		bytes[15] = 255;

		let error = read_program(&bytes).unwrap_err();

		assert_eq!(error, ObjectFileError {
			offset: 15,
			message: String::from("Unknown opcode 255."),
		});
	}

	#[test]
	fn rejects_unsupported_object_file_version() {
		let mut bytes = write_program(&Program::new(vec![
			Instruction::PushInteger(1),
		]));
		bytes[MAGIC_BYTES.len()..MAGIC_BYTES.len() + 2].copy_from_slice(&2_u16.to_le_bytes());

		let error = read_program(&bytes).unwrap_err();

		assert_eq!(error, ObjectFileError {
			offset: MAGIC_BYTES.len(),
			message: String::from("Unsupported object file version 2; expected version 1."),
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
	fn round_trips_current_date_program_bytes() {
		let program = Program::new(vec![
			Instruction::PushCurrentDate,
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_date_program_bytes() {
		let program = Program::new(vec![
			Instruction::PushDate(crate::value::Date::from_literal("@2025-06-10").unwrap()),
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
	fn round_trips_delete_program_bytes() {
		let program = Program::new(vec![
			Instruction::DeleteRecord,
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_new_record_field_types() {
		let program = Program::new(vec![
			Instruction::PushInteger(0),
			Instruction::PushNull,
			Instruction::MakeRecordPointer {
				field_names: vec![String::from("Id"), String::from("Name")],
				field_types: vec![DataType::Int, DataType::Text.into_nullable()],
				record_type: RecordPointerType {
					database_name: String::from("ExampleDb"),
					schema_name: String::from("Main"),
					table_name: String::from("Customers"),
				},
				schema_is_implicit: true,
			},
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_presence_and_lock_operator_instructions() {
		let program = Program::new(vec![
			Instruction::LoadLocal(0),
			Instruction::Exists,
			Instruction::LoadLocal(0),
			Instruction::Locked,
			Instruction::LoadLocal(1),
			Instruction::FieldPathExists(vec![String::from("customer"), String::from("name")]),
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
	fn round_trips_program_bytes_with_any_type_and_null_instruction() {
		let program = Program::from_parts_with_functions(
			ConstantPool::default(),
			CodeBody::new(vec![
				Instruction::PushNull,
				Instruction::Return,
			]),
			vec![],
		);

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
					Some(DataType::Int),
					CodeBody::new(vec![
						Instruction::LoadLocal(0),
						Instruction::LoadLocal(1),
						Instruction::Add,
						Instruction::Return,
					]),
				),
				CompiledFunction::new(
					Some(String::from("log")),
					None,
					CodeBody::new(vec![
						Instruction::LoadLocal(0),
						Instruction::CallBuiltIn(BuiltInFunction::Displn, 1),
						Instruction::ReturnNoValue,
					]),
				),
			],
		);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
		assert_eq!(decoded.functions()[0].return_type(), Some(&DataType::Int));
		assert_eq!(decoded.functions()[1].return_type(), None);
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
	fn round_trips_program_bytes_with_object_field_path() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::MakeObject {
				field_names: vec![String::from("value")],
				object_type_id: ObjectTypeId::from_raw(1),
			},
			Instruction::LoadFieldPath(vec![String::from("value")]),
			Instruction::PushInteger(2),
			Instruction::MakeObject {
				field_names: vec![String::from("value")],
				object_type_id: ObjectTypeId::from_raw(2),
			},
			Instruction::PushInteger(3),
			Instruction::StoreFieldPath(vec![String::from("value")]),
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
	fn round_trips_program_bytes_with_reordered_call_arguments() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::ReorderCallArguments(vec![1, 0]),
			Instruction::Call(0, 2),
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_query_scalar_projections_and_projected_value_loads() {
		let query = SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::Sqlite,
			group_by: vec![],
			lock_mode: RecordLockMode::None,
			parameters: vec![],
			result_shape: SqlQueryResultShape::RecordPointerArray(QueryRecordLayout::all_known(vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
					is_primary_key: true,
				},
			])),
			scalar_projections: vec![SqlScalarProjection {
				column_index: 1,
				data_type: DataType::Int,
				value_id: QueryProjectedValueId(4),
			}],
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from("SELECT Id, 2 FROM Customers"),
			table_name: String::from("Customers"),
		};
		let program = Program::from_parts_with_functions_queries_and_debug(
			ConstantPool::default(),
			CodeBody::new(vec![
				Instruction::ExecuteQuery(0),
				Instruction::LoadProjectedValue(4),
			]),
			vec![],
			vec![LoweredBackendQuery::Sql(query)],
			DebugInfo::default(),
		);

		assert_eq!(read_program(&write_program(&program)).unwrap(), program);
	}

	#[test]
	fn round_trips_runtime_determined_query_record_layout() {
		let query = SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::PostgreSql,
			group_by: vec![],
			lock_mode: RecordLockMode::None,
			parameters: vec![],
			result_shape: SqlQueryResultShape::RecordPointer(QueryRecordLayout {
				schema: QueryRecordSchema::RuntimeDetermined,
				selection: QueryColumnSelection::RuntimeDetermined,
			}),
			scalar_projections: vec![],
			schema_is_implicit: true,
			schema_name: String::new(),
			statement: String::from("SELECT * FROM runtime_table"),
			table_name: String::new(),
		};
		let program = Program::from_parts_with_functions_queries_and_debug(
			ConstantPool::default(),
			CodeBody::new(vec![Instruction::ExecuteQuery(0)]),
			vec![],
			vec![LoweredBackendQuery::Sql(query)],
			DebugInfo::default(),
		);

		let decoded = read_program(&write_program(&program)).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_sql_query_record_lock_mode() {
		let record_layout = QueryRecordLayout {
			schema: QueryRecordSchema::Known(vec![
				QueryResultColumn {
					column_name: String::from("Id"),
					data_type: DataType::Int,
					is_nullable: false,
					is_primary_key: true,
				},
				QueryResultColumn {
					column_name: String::from("Name"),
					data_type: DataType::Text,
					is_nullable: false,
					is_primary_key: false,
				},
			]),
			selection: QueryColumnSelection::Indices(vec![1]),
		};
		let query = SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect: SqlDialect::PostgreSql,
			group_by: vec![],
			lock_mode: RecordLockMode::UpdateNoWait,
			parameters: vec![],
			result_shape: SqlQueryResultShape::RecordPointer(record_layout),
			scalar_projections: vec![],
			schema_is_implicit: true,
			schema_name: String::from("Public"),
			statement: String::from("SELECT 1"),
			table_name: String::from("Example"),
		};
		let program = Program::from_parts_with_functions_queries_and_debug(
			ConstantPool::default(),
			CodeBody::new(vec![Instruction::ExecuteQuery(0)]),
			vec![],
			vec![LoweredBackendQuery::Sql(query)],
			DebugInfo::default(),
		);

		let decoded = read_program(&write_program(&program)).unwrap();

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

	#[test]
	fn round_trips_transaction_program_bytes() {
		let program = Program::new(vec![
			Instruction::BeginTransaction,
			Instruction::CommitTransaction,
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}

	#[test]
	fn round_trips_update_program_bytes() {
		let program = Program::new(vec![
			Instruction::UpdateRecord,
			Instruction::UpdateRecordIfChanged,
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}
}
