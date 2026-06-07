// This module encodes and decodes the current `.tbo` object file format.
// The format is still deliberately simple, but this is another likely split
// point once the object file grows support for multiple sections and debug
// metadata.

use std::path::Path;

use crate::ast::DataType;
use crate::builtins::BuiltInFunction;
use crate::bytecode::CodeBody;
use crate::bytecode::CodeBodyDebugInfo;
use crate::bytecode::CompiledFunction;
use crate::bytecode::ConstantPool;
use crate::bytecode::DebugInfo;
use crate::bytecode::EntryPoint;
use crate::bytecode::Instruction;
use crate::bytecode::LocalVariableDebugInfo;
use crate::bytecode::Program;
use crate::bytecode::SourceFileDebugInfo;
use crate::query::LoweredBackendQuery;
use crate::query::QueryResultColumn;
use crate::query::SqlDialect;
use crate::query::SqlParameter;
use crate::query::SqlQuery;
use crate::query::SqlQueryResultShape;
use crate::value::Decimal;

const MAGIC_BYTES: [u8; 4] = *b"TBO0";
const FORMAT_VERSION: u16 = 1;
const OPCODE_ADD: u8 = 1;
const OPCODE_AND: u8 = 2;
const OPCODE_DIVIDE: u8 = 3;
const OPCODE_EQUAL: u8 = 4;
const OPCODE_GREATER_THAN: u8 = 5;
const OPCODE_GREATER_THAN_OR_EQUAL: u8 = 6;
const OPCODE_ITER_HAS_NEXT: u8 = 7;
const OPCODE_ITER_INIT: u8 = 8;
const OPCODE_ITER_NEXT: u8 = 9;
const OPCODE_JUMP: u8 = 10;
const OPCODE_JUMP_IF_FALSE: u8 = 11;
const OPCODE_LESS_THAN: u8 = 12;
const OPCODE_LESS_THAN_OR_EQUAL: u8 = 13;
const OPCODE_LOAD_LOCAL: u8 = 14;
const OPCODE_LOAD_REFERENCE: u8 = 15;
const OPCODE_MODULO: u8 = 16;
const OPCODE_MULTIPLY: u8 = 17;
const OPCODE_PUSH_BOOLEAN: u8 = 18;
const OPCODE_PUSH_DECIMAL: u8 = 19;
const OPCODE_PUSH_INTEGER: u8 = 20;
const OPCODE_STORE_LOCAL: u8 = 21;
const OPCODE_SUBTRACT: u8 = 22;
const OPCODE_NEGATE: u8 = 23;
const OPCODE_NOT_EQUAL: u8 = 24;
const OPCODE_NOT: u8 = 25;
const OPCODE_OR: u8 = 26;
const OPCODE_XOR: u8 = 27;
const OPCODE_POP: u8 = 28;
const OPCODE_PUSH_TEXT: u8 = 29;
const OPCODE_CALL: u8 = 30;
const OPCODE_RETURN: u8 = 31;
const OPCODE_RETURN_VOID: u8 = 32;
const OPCODE_MAKE_ARRAY: u8 = 33;
const OPCODE_LOAD_INDEX: u8 = 34;
const OPCODE_STORE_INDEX: u8 = 35;
const OPCODE_CALL_BUILT_IN: u8 = 36;
const OPCODE_DUP2: u8 = 37;
const OPCODE_MAKE_RANGE: u8 = 38;
const OPCODE_MAKE_STEPPED_RANGE: u8 = 39;
const OPCODE_MAKE_OBJECT: u8 = 40;
const OPCODE_LOAD_FIELD: u8 = 41;
const OPCODE_LOAD_FIELD_PATH: u8 = 42;
const OPCODE_STORE_FIELD_PATH: u8 = 43;
const OPCODE_EXECUTE_QUERY: u8 = 44;
const QUERY_KIND_SQL: u8 = 1;
const SQL_DIALECT_SQLITE: u8 = 1;
const SQL_RESULT_INTEGER_SCALAR: u8 = 1;
const SQL_RESULT_RECORD_POINTER: u8 = 2;

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

	fn read_data_type(&mut self) -> Result<DataType, ObjectFileError> {
		let tag_offset = self.offset;
		match self.read_u8()? {
			1 => Ok(DataType::Array(Box::new(self.read_data_type()?))),
			2 => Ok(DataType::Bool),
			3 => Ok(DataType::Dec),
			4 => Ok(DataType::EmptyArray),
			5 => Ok(DataType::Int),
			6 => Ok(DataType::Object(self.read_string()?)),
			7 => Ok(DataType::Range(Box::new(self.read_data_type()?))),
			8 => Ok(DataType::Text),
			9 => Ok(DataType::Void),
			10 => Ok(DataType::RecordPointer(crate::ast::RecordPointerType {
				database_name: self.read_string()?,
				schema_name: self.read_string()?,
				table_name: self.read_string()?,
			})),
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
			OPCODE_EXECUTE_QUERY => Ok(Instruction::ExecuteQuery(self.read_u32()?)),
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
			OPCODE_LOAD_REFERENCE => Ok(Instruction::LoadReference(self.read_u32()?)),
			OPCODE_MAKE_ARRAY => Ok(Instruction::MakeArray(self.read_u32()?)),
			OPCODE_MAKE_OBJECT => {
				let field_count = self.read_u32()? as usize;
				let mut field_names = Vec::with_capacity(field_count);

				for _ in 0..field_count {
					field_names.push(self.read_string()?);
				}

				Ok(Instruction::MakeObject(field_names))
			}
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
			OPCODE_STORE_FIELD_PATH => Ok(Instruction::StoreFieldPath(self.read_string_vec()?)),
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

	fn read_sql_query(&mut self) -> Result<SqlQuery, ObjectFileError> {
		let dialect_offset = self.offset;
		let dialect = match self.read_u8()? {
			SQL_DIALECT_SQLITE => SqlDialect::Sqlite,
			dialect => {
				return Err(ObjectFileError {
					offset: dialect_offset,
					message: format!("Unknown SQL dialect {dialect}."),
				});
			}
		};
		let database_name = self.read_string()?;
		let statement = self.read_string()?;
		let result_shape = match self.read_u8()? {
			SQL_RESULT_INTEGER_SCALAR => SqlQueryResultShape::IntegerScalar,
			SQL_RESULT_RECORD_POINTER => {
				let column_count = self.read_u32()? as usize;
				let mut columns = Vec::with_capacity(column_count);

				for _ in 0..column_count {
					columns.push(QueryResultColumn {
						column_name: self.read_string()?,
						data_type: self.read_data_type()?,
					});
				}

				SqlQueryResultShape::RecordPointer(columns)
			}
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
				index: self.read_u32()?,
				slot: self.read_u32()?,
			});
		}

		Ok(SqlQuery {
			database_name,
			dialect,
			parameters,
			result_shape,
			statement,
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
		DataType::Array(element_type) => {
			bytes.push(1);
			write_data_type(bytes, element_type);
		}
		DataType::Bool => bytes.push(2),
		DataType::Dec => bytes.push(3),
		DataType::EmptyArray => bytes.push(4),
		DataType::Int => bytes.push(5),
		DataType::Object(name) => {
			bytes.push(6);
			bytes.extend_from_slice(&(name.len() as u32).to_le_bytes());
			bytes.extend_from_slice(name.as_bytes());
		}
		DataType::Range(element_type) => {
			bytes.push(7);
			write_data_type(bytes, element_type);
		}
		DataType::Text => bytes.push(8),
		DataType::Void => bytes.push(9),
		DataType::RecordPointer(record_pointer) => {
			bytes.push(10);
			for value in [&record_pointer.database_name, &record_pointer.schema_name, &record_pointer.table_name] {
				bytes.extend_from_slice(&(value.len() as u32).to_le_bytes());
				bytes.extend_from_slice(value.as_bytes());
			}
		}
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
		Instruction::ExecuteQuery(query_index) => {
			bytes.push(OPCODE_EXECUTE_QUERY);
			bytes.extend_from_slice(&query_index.to_le_bytes());
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
		Instruction::LoadReference(slot) => {
			bytes.push(OPCODE_LOAD_REFERENCE);
			bytes.extend_from_slice(&slot.to_le_bytes());
		}
		Instruction::MakeArray(element_count) => {
			bytes.push(OPCODE_MAKE_ARRAY);
			bytes.extend_from_slice(&element_count.to_le_bytes());
		}
		Instruction::MakeObject(field_names) => {
			bytes.push(OPCODE_MAKE_OBJECT);
			bytes.extend_from_slice(&(field_names.len() as u32).to_le_bytes());

			for field_name in field_names {
				bytes.extend_from_slice(&(field_name.len() as u32).to_le_bytes());
				bytes.extend_from_slice(field_name.as_bytes());
			}
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
		Instruction::Subtract => bytes.push(OPCODE_SUBTRACT),
		Instruction::Xor => bytes.push(OPCODE_XOR),
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
		SqlDialect::Sqlite => SQL_DIALECT_SQLITE,
	});
	bytes.extend_from_slice(&(query.database_name.len() as u32).to_le_bytes());
	bytes.extend_from_slice(query.database_name.as_bytes());
	bytes.extend_from_slice(&(query.statement.len() as u32).to_le_bytes());
	bytes.extend_from_slice(query.statement.as_bytes());
	match &query.result_shape {
		SqlQueryResultShape::IntegerScalar => bytes.push(SQL_RESULT_INTEGER_SCALAR),
		SqlQueryResultShape::RecordPointer(columns) => {
			bytes.push(SQL_RESULT_RECORD_POINTER);
			bytes.extend_from_slice(&(columns.len() as u32).to_le_bytes());

			for column in columns {
				bytes.extend_from_slice(&(column.column_name.len() as u32).to_le_bytes());
				bytes.extend_from_slice(column.column_name.as_bytes());
				write_data_type(bytes, &column.data_type);
			}
		}
	}
	bytes.extend_from_slice(&(query.parameters.len() as u32).to_le_bytes());

	for parameter in &query.parameters {
		write_data_type(bytes, &parameter.data_type);
		bytes.extend_from_slice(&parameter.index.to_le_bytes());
		bytes.extend_from_slice(&parameter.slot.to_le_bytes());
	}
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
				ObjectFileSection::Function(function) => write_code_body(bytes, function.body()),
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
	fn round_trips_program_bytes_with_object_field_path() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::MakeObject(vec![String::from("value")]),
			Instruction::LoadFieldPath(vec![String::from("value")]),
			Instruction::PushInteger(2),
			Instruction::MakeObject(vec![String::from("value")]),
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
	fn round_trips_text_program_bytes() {
		let program = Program::new(vec![
			Instruction::PushText(String::from("hello")),
		]);

		let bytes = write_program(&program);
		let decoded = read_program(&bytes).unwrap();

		assert_eq!(decoded, program);
	}
}
