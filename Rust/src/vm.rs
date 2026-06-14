use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use rusqlite::Connection;
use rusqlite::params_from_iter;
use rusqlite::types::Value as SqlValue;
use rusqlite::types::ValueRef as SqlValueRef;

use crate::builtins::BuiltInFunction;
use crate::bytecode::CodeBody;
use crate::bytecode::Instruction;
use crate::bytecode::Program;
use crate::bytecode::SourceLocation;
use crate::query::LoweredBackendQuery;
use crate::query::QueryResultColumn;
use crate::query::SqlDialect;
use crate::query::SqlQuery;
use crate::query::SqlQueryResultShape;
use crate::value::Decimal;
use crate::value::DecimalRange;
use crate::value::DecimalRangeIterator;
use crate::value::DeferredSqliteValue;
use crate::value::EnumValue as RuntimeEnumValue;
use crate::value::IntegerRange;
use crate::value::IntegerRangeIterator;
use crate::value::IteratorState;
use crate::value::LocalReference;
use crate::value::RecordFieldValue;
use crate::value::RecordPointerValue;
use crate::value::Value;
use crate::value::sqlite_record_field_runtime_value;

#[derive(Clone, Copy)]
enum ComparisonKind {
	GreaterThan,
	GreaterThanOrEqual,
	LessThan,
	LessThanOrEqual,
}

struct CallFrame {
	base_stack_len: usize,
	function_index: Option<usize>,
	instruction_index: usize,
	locals: Vec<Value>,
}

enum ExecutionOutcome {
	Call(u32, Vec<Value>),
	Continue(Option<usize>),
	Return(Option<Value>),
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct RuntimeDatabaseConfig {
	sqlite_databases: BTreeMap<String, PathBuf>,
}

impl RuntimeDatabaseConfig {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn set_database_connection_string(
		&mut self,
		database_name: impl Into<String>,
		connection_string: &str,
	) -> Result<(), String> {
		let database_name = database_name.into();
		let (scheme, value) = connection_string.split_once(':').ok_or_else(|| {
			format!(
				"Connection string for database `{database_name}` must use the form `<backend>:<value>`."
			)
		})?;

		match scheme.to_ascii_lowercase().as_str() {
			"sqlite" => {
				let path = sqlite_path_from_connection_string(&database_name, value)?;
				self.set_sqlite_database(database_name, path);
				Ok(())
			}
			other => Err(format!(
				"Connection string for database `{database_name}` uses unsupported backend `{other}`."
			)),
		}
	}

	pub fn set_sqlite_database(&mut self, database_name: impl Into<String>, path: impl Into<PathBuf>) {
		self.sqlite_databases.insert(normalize_database_name(&database_name.into()), path.into());
	}

	pub fn sqlite_database_path(&self, database_name: &str) -> Option<&Path> {
		self.sqlite_databases.get(&normalize_database_name(database_name)).map(PathBuf::as_path)
	}

	pub fn with_database_connection_string(
		mut self,
		database_name: impl Into<String>,
		connection_string: &str,
	) -> Result<Self, String> {
		self.set_database_connection_string(database_name, connection_string)?;
		Ok(self)
	}

	pub fn with_sqlite_database(mut self, database_name: impl Into<String>, path: impl Into<PathBuf>) -> Self {
		self.set_sqlite_database(database_name, path);
		self
	}
}

#[derive(Default)]
pub struct VirtualMachine {
	database_config: RuntimeDatabaseConfig,
	finished_result: Option<Option<Value>>,
	frames: Vec<CallFrame>,
	stack: Vec<Value>,
}

pub(crate) enum VmExecutionState {
	Completed(Option<Value>),
	Running,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VmError {
	pub instruction_index: usize,
	pub message: String,
	pub source_location: Option<SourceLocation>,
	pub stack_trace: Vec<VmStackFrame>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VmStackFrame {
	pub instruction_index: usize,
	pub locals: Vec<VmVisibleLocal>,
	pub source_location: Option<SourceLocation>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VmVisibleLocal {
	pub declared_type: String,
	pub is_const: bool,
	pub name: String,
	pub slot: u32,
	pub value: Value,
}

impl VmError {
	pub fn current_frame(&self) -> Option<&VmStackFrame> {
		self.stack_trace.first()
	}

	pub fn stack_frames(&self) -> &[VmStackFrame] {
		&self.stack_trace
	}
}

impl VmStackFrame {
	pub fn instruction_index(&self) -> usize {
		self.instruction_index
	}

	pub fn locals(&self) -> &[VmVisibleLocal] {
		&self.locals
	}

	pub fn local(&self, name: &str) -> Option<&VmVisibleLocal> {
		self.locals.iter().find(|local| local.name == name)
	}

	pub fn source_location(&self) -> Option<&SourceLocation> {
		self.source_location.as_ref()
	}
}

impl VmVisibleLocal {
	pub fn declared_type(&self) -> &str {
		&self.declared_type
	}

	pub fn is_const(&self) -> bool {
		self.is_const
	}

	pub fn name(&self) -> &str {
		&self.name
	}

	pub fn slot(&self) -> u32 {
		self.slot
	}

	pub fn value(&self) -> &Value {
		&self.value
	}
}

impl VirtualMachine {
	pub fn new() -> Self {
		Self {
			database_config: RuntimeDatabaseConfig::default(),
			finished_result: None,
			frames: Vec::new(),
			stack: Vec::new(),
		}
	}

	pub fn run(&mut self, program: &Program) -> Result<Option<Value>, VmError> {
		self.begin_execution(program);

		loop {
			match self.step(program)? {
				VmExecutionState::Completed(result) => return Ok(result),
				VmExecutionState::Running => {}
			}
		}
	}

	pub fn with_database_config(database_config: RuntimeDatabaseConfig) -> Self {
		Self {
			database_config,
			..Self::new()
		}
	}

	pub(crate) fn begin_execution(&mut self, _program: &Program) {
		self.finished_result = None;
		self.frames.clear();
		self.stack.clear();
		match _program.entry_point() {
			crate::bytecode::EntryPoint::Code(_) => {
				self.frames.push(CallFrame::new(None, 0, Vec::new()));
			}
			crate::bytecode::EntryPoint::Function(function_index) => {
				self.frames.push(CallFrame::new(
					Some(*function_index as usize),
					0,
					vec![Value::Array(Vec::new())],
				));
			}
		}
	}

	pub(crate) fn current_instruction_site(&self, program: &Program) -> Option<(usize, usize)> {
		let frame = self.frames.last()?;
		Some((self.frame_debug_body_index(program, frame), frame.instruction_index))
	}

	pub(crate) fn current_stack_depth(&self) -> usize {
		self.frames.len()
	}

	pub(crate) fn current_stack_frames(&self, program: &Program) -> Vec<VmStackFrame> {
		self.frames
			.iter()
			.rev()
			.enumerate()
			.map(|(reverse_index, frame)| {
				let frame_index = self.frames.len() - 1 - reverse_index;
				let debug_body_index = self.frame_debug_body_index(program, frame);
				VmStackFrame {
					instruction_index: frame.instruction_index,
					locals: self.resolve_visible_locals(program, frame_index, debug_body_index, frame.instruction_index),
					source_location: program.debug_location(debug_body_index, frame.instruction_index),
				}
			})
			.collect()
	}

	pub(crate) fn step(&mut self, program: &Program) -> Result<VmExecutionState, VmError> {
		if self.frames.is_empty() {
			return Ok(VmExecutionState::Completed(self.finished_result.take().unwrap_or(None)));
		}

		let frame_index = self.frames.len() - 1;
		let instruction_index = self.frames[frame_index].instruction_index;
		let code_body = self.current_code_body(program, &self.frames[frame_index]);

		if instruction_index >= code_body.instructions.len() {
			let result = self.stack.pop();
			return Ok(self.finish_current_frame(result));
		}

		let instruction = code_body.instructions[instruction_index].clone();
		let mut locals = std::mem::take(&mut self.frames[frame_index].locals);
		let outcome = self.execute_instruction(program, frame_index, &instruction, instruction_index, &mut locals);
		self.frames[frame_index].locals = locals;
		let outcome = outcome.map_err(|error| self.enrich_vm_error(program, error))?;

		match outcome {
			ExecutionOutcome::Call(function_index, arguments) => {
				if program.functions().get(function_index as usize).is_none() {
					return Err(self.enrich_vm_error(program, VmError {
						instruction_index,
						message: format!("Function index {} does not exist.", function_index),
						source_location: None,
						stack_trace: Vec::new(),
					}));
				}

				self.frames.push(CallFrame::new(Some(function_index as usize), self.stack.len(), arguments));
				Ok(VmExecutionState::Running)
			}
			ExecutionOutcome::Continue(next_instruction_index) => {
				self.frames[frame_index].instruction_index = next_instruction_index.unwrap_or(instruction_index + 1);
				Ok(VmExecutionState::Running)
			}
			ExecutionOutcome::Return(result) => Ok(self.finish_current_frame(result)),
		}
	}

	fn current_code_body<'a>(&self, program: &'a Program, frame: &CallFrame) -> &'a CodeBody {
		match frame.function_index {
			Some(function_index) => program.functions()[function_index].body(),
			None => program.entry_code().expect("code-entry programs must provide entry code"),
		}
	}

	fn enrich_vm_error(&self, program: &Program, mut error: VmError) -> VmError {
		if error.source_location.is_none() {
			error.source_location = self.current_instruction_site(program)
				.and_then(|(body_index, instruction_index)| program.debug_location(body_index, instruction_index));
		}

		error.stack_trace = self.current_stack_frames(program);
		error
	}

	fn execute_instruction(
		&mut self,
		_program: &Program,
		frame_index: usize,
		instruction: &Instruction,
		instruction_index: usize,
		locals: &mut Vec<Value>
	) -> Result<ExecutionOutcome, VmError> {
		match instruction {
			Instruction::Add => {
				let rhs = self.pop_value(instruction_index)?;
				let lhs = self.pop_value(instruction_index)?;
				self.stack.push(add_values(lhs, rhs, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::And => {
				let rhs = self.pop_boolean(instruction_index)?;
				let lhs = self.pop_boolean(instruction_index)?;
				self.stack.push(Value::Boolean(lhs && rhs));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Call(function_index, argument_count) => {
				let arguments = self.pop_call_arguments(*argument_count as usize, instruction_index)?;
				Ok(ExecutionOutcome::Call(*function_index, arguments))
			}
			Instruction::CallBuiltIn(built_in, argument_count) => {
				let arguments = self.pop_call_arguments(*argument_count as usize, instruction_index)?;
				let result = self.run_built_in_function(*built_in, arguments, instruction_index)?;
				if let Some(result) = result {
					self.stack.push(result);
				}
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Dup2 => {
				if self.stack.len() < 2 {
					return Err(VmError {
						instruction_index,
						message: String::from("Stack underflow while duplicating operands."),
						source_location: None,
						stack_trace: Vec::new(),
					});
				}

				let lhs = self.stack[self.stack.len() - 2].clone();
				let rhs = self.stack[self.stack.len() - 1].clone();
				self.stack.push(lhs);
				self.stack.push(rhs);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Divide => {
				let rhs = self.pop_numeric(instruction_index)?;
				let lhs = self.pop_numeric(instruction_index)?;
				self.stack.push(divide_values(lhs, rhs, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Equal => {
				let rhs = self.pop_value(instruction_index)?;
				let lhs = self.pop_value(instruction_index)?;
				self.stack.push(equals_value(lhs, rhs, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::ExecuteQuery(query_index) => {
				let query = _program.queries().get(*query_index as usize).ok_or(vm_error(
					instruction_index,
					format!("Compiled query index {} does not exist.", query_index),
				))?;
				let result = self.execute_query(query, locals, instruction_index)?;
				self.stack.push(result);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::GreaterThan => {
				let rhs = self.pop_value(instruction_index)?;
				let lhs = self.pop_value(instruction_index)?;
				self.stack.push(compare_values(lhs, rhs, instruction_index, ComparisonKind::GreaterThan)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::GreaterThanOrEqual => {
				let rhs = self.pop_value(instruction_index)?;
				let lhs = self.pop_value(instruction_index)?;
				self.stack.push(compare_values(lhs, rhs, instruction_index, ComparisonKind::GreaterThanOrEqual)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::IterHasNext => {
				let iterator = self.pop_value(instruction_index)?;
				self.stack.push(Value::Boolean(iterator_has_next(&iterator, instruction_index)?));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::IterInit => {
				let iterable = self.pop_value(instruction_index)?;
				self.stack.push(make_iterator_value(iterable, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::IterNext => {
				let iterator = self.pop_value(instruction_index)?;
				let (value, iterator) = iterator_next_value(iterator, instruction_index)?;
				self.stack.push(value);
				self.stack.push(iterator);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Jump(target) => {
				Ok(ExecutionOutcome::Continue(Some(*target as usize)))
			}
			Instruction::JumpIfFalse(target) => {
				let value = self.pop_condition_value(instruction_index)?;

				if value {
					Ok(ExecutionOutcome::Continue(None))
				}
				else {
					Ok(ExecutionOutcome::Continue(Some(*target as usize)))
				}
			}
			Instruction::LessThan => {
				let rhs = self.pop_value(instruction_index)?;
				let lhs = self.pop_value(instruction_index)?;
				self.stack.push(compare_values(lhs, rhs, instruction_index, ComparisonKind::LessThan)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::LessThanOrEqual => {
				let rhs = self.pop_value(instruction_index)?;
				let lhs = self.pop_value(instruction_index)?;
				self.stack.push(compare_values(lhs, rhs, instruction_index, ComparisonKind::LessThanOrEqual)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::LoadField(field_name) => {
				let object = self.pop_value(instruction_index)?;
				self.stack.push(load_field_value(object, field_name, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::LoadFieldPath(field_path) => {
				let object = self.pop_value(instruction_index)?;
				self.stack.push(load_field_path_value(object, field_path, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::LoadIndex => {
				let index = self.pop_value(instruction_index)?;
				let array = self.pop_value(instruction_index)?;
				self.stack.push(load_index_value(array, index, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::LoadLocal(slot) => {
				let value = locals.get(*slot as usize).cloned().ok_or(VmError {
					instruction_index,
					message: format!("Local slot {} has not been initialized.", slot),
					source_location: None,
					stack_trace: Vec::new(),
				})?;
				self.stack.push(self.resolve_runtime_value(value, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::LoadReference(slot) => {
				let value = locals.get(*slot as usize).cloned().ok_or(VmError {
					instruction_index,
					message: format!("Local slot {} has not been initialized.", slot),
					source_location: None,
					stack_trace: Vec::new(),
				})?;
				let reference = match value {
					Value::Reference(reference) => reference,
					_ => LocalReference {
						frame_index,
						slot: *slot as usize,
					},
				};
				self.stack.push(Value::Reference(reference));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::MakeArray(element_count) => {
				let mut values = Vec::with_capacity(*element_count as usize);

				for _ in 0..*element_count {
					values.push(self.pop_value(instruction_index)?);
				}

				values.reverse();
				self.stack.push(Value::Array(values));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::MakeObject(field_names) => {
				let mut values = Vec::with_capacity(field_names.len());

				for _ in 0..field_names.len() {
					values.push(self.pop_value(instruction_index)?);
				}

				values.reverse();
				let fields = field_names.iter().cloned().zip(values).collect();
				self.stack.push(Value::Object(fields));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::MakeRange => {
				let end = self.pop_value(instruction_index)?;
				let start = self.pop_value(instruction_index)?;
				self.stack.push(make_range_value(start, None, end, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::MakeSteppedRange => {
				let end = self.pop_value(instruction_index)?;
				let step = self.pop_value(instruction_index)?;
				let start = self.pop_value(instruction_index)?;
				self.stack.push(make_range_value(start, Some(step), end, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Modulo => {
				let rhs = self.pop_numeric(instruction_index)?;
				let lhs = self.pop_numeric(instruction_index)?;
				self.stack.push(modulo_values(lhs, rhs, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Multiply => {
				let rhs = self.pop_numeric(instruction_index)?;
				let lhs = self.pop_numeric(instruction_index)?;
				self.stack.push(multiply_values(lhs, rhs, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Negate => {
				let value = self.pop_numeric(instruction_index)?;
				self.stack.push(negate_value(value));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Not => {
				let value = self.pop_condition_value(instruction_index)?;
				self.stack.push(Value::Boolean(!value));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::NotEqual => {
				let rhs = self.pop_value(instruction_index)?;
				let lhs = self.pop_value(instruction_index)?;
				self.stack.push(not_equals_value(lhs, rhs, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Or => {
				let rhs = self.pop_boolean(instruction_index)?;
				let lhs = self.pop_boolean(instruction_index)?;
				self.stack.push(Value::Boolean(lhs || rhs));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Pop => {
				self.pop_value(instruction_index)?;
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushBoolean(value) => {
				self.stack.push(Value::Boolean(*value));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushCurrentDate => {
				self.stack.push(Value::Date(crate::value::Date::current_local()));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushCurrentTime => {
				self.stack.push(Value::Time(crate::value::Time::current_local()));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushCurrentTimeTz => {
				self.stack.push(Value::TimeTz(crate::value::TimeTz::current_local()));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushCurrentTimestamp => {
				self.stack.push(Value::Timestamp(crate::value::Timestamp::current_local()));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushCurrentTimestampTz => {
				self.stack.push(Value::TimestampTz(crate::value::TimestampTz::current_local()));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushDate(value) => {
				self.stack.push(Value::Date(*value));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushDecimal(value) => {
				self.stack.push(Value::Decimal(value.clone()));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushEnumValue {
				backing_value,
				enum_name,
				variant_name,
			} => {
				self.stack.push(Value::Enum(RuntimeEnumValue {
					backing_value: Box::new(runtime_value_from_constant(backing_value)),
					enum_name: enum_name.clone(),
					variant_name: variant_name.clone(),
				}));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushInteger(value) => {
				self.stack.push(Value::Integer(*value));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushNull => {
				self.stack.push(Value::Null);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushText(value) => {
				self.stack.push(Value::Text(value.clone()));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Return => Ok(ExecutionOutcome::Return(Some(self.pop_value(instruction_index)?))),
			Instruction::ReturnVoid => Ok(ExecutionOutcome::Return(None)),
			Instruction::Subtract => {
				let rhs = self.pop_numeric(instruction_index)?;
				let lhs = self.pop_numeric(instruction_index)?;
				self.stack.push(subtract_values(lhs, rhs, instruction_index)?);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::StoreFieldPath(field_path) => {
				let value = self.pop_value(instruction_index)?;
				let object = self.pop_value(instruction_index)?;
				let (assigned_value, updated_object) = store_field_path_value(object, field_path, value, instruction_index)?;
				self.stack.push(assigned_value);
				self.stack.push(updated_object);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::StoreIndex => {
				let value = self.pop_value(instruction_index)?;
				let index = self.pop_value(instruction_index)?;
				let array = self.pop_value(instruction_index)?;
				let (assigned_value, updated_array) = store_index_value(array, index, value, instruction_index)?;
				self.stack.push(assigned_value);
				self.stack.push(updated_array);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::StoreLocal(slot) => {
				let value = self.pop_value(instruction_index)?;
				let slot = *slot as usize;

				if locals.len() <= slot {
					locals.resize(slot + 1, Value::Boolean(false));
				}

				if let Value::Reference(reference) = locals[slot].clone() {
					self.store_reference_value(reference, value, instruction_index)?;
				}
				else {
					locals[slot] = value;
				}
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Xor => {
				let rhs = self.pop_boolean(instruction_index)?;
				let lhs = self.pop_boolean(instruction_index)?;
				self.stack.push(Value::Boolean(lhs ^ rhs));
				Ok(ExecutionOutcome::Continue(None))
			}
		}
	}

	fn execute_query(
		&self,
		query: &LoweredBackendQuery,
		locals: &[Value],
		instruction_index: usize,
	) -> Result<Value, VmError> {
		match query {
			LoweredBackendQuery::Sql(query) => self.execute_sql_query(query, locals, instruction_index),
		}
	}

	fn execute_sql_query(
		&self,
		query: &SqlQuery,
		locals: &[Value],
		instruction_index: usize,
	) -> Result<Value, VmError> {
		match query.dialect {
			SqlDialect::Sqlite => self.execute_sqlite_query(query, locals, instruction_index),
		}
	}

	fn execute_sqlite_query(
		&self,
		query: &SqlQuery,
		locals: &[Value],
		instruction_index: usize,
	) -> Result<Value, VmError> {
		let database_path = self.database_config.sqlite_database_path(&query.database_name).ok_or(vm_error(
			instruction_index,
			format!("SQLite database `{}` is not configured at runtime.", query.database_name),
		))?;
		let connection = Connection::open(database_path).map_err(|error| vm_error(
			instruction_index,
			format!("Failed to open SQLite database `{}`: {error}", database_path.display()),
		))?;
		let mut statement = connection.prepare(&query.statement).map_err(|error| vm_error(
			instruction_index,
			format!("Failed to prepare SQLite query: {error}"),
		))?;
		let parameter_values = query.parameters.iter()
			.map(|parameter| self.sqlite_parameter_value(parameter.slot, &parameter.field_path, locals, instruction_index))
			.collect::<Result<Vec<_>, _>>()?;

		match &query.result_shape {
			SqlQueryResultShape::IntegerScalar => {
				let result = statement.query_row(params_from_iter(parameter_values), |row| row.get::<_, i64>(0))
					.map_err(|error| vm_error(
						instruction_index,
						format!("Failed to execute SQLite query: {error}"),
					))?;
				Ok(Value::Integer(result))
			}
			SqlQueryResultShape::RecordPointer(columns) => {
				let mut rows = statement.query(params_from_iter(parameter_values)).map_err(|error| vm_error(
					instruction_index,
					format!("Failed to execute SQLite query: {error}"),
				))?;
				let Some(row) = rows.next().map_err(|error| vm_error(
					instruction_index,
					format!("Failed to read SQLite query result: {error}"),
				))? else {
					return Ok(Value::RecordPointer(RecordPointerValue {
						exists: false,
						fields: BTreeMap::new(),
						locked: false,
					}));
				};
				let fields = load_sqlite_record_fields(row, columns, instruction_index)?;
				Ok(Value::RecordPointer(RecordPointerValue {
					exists: true,
					fields,
					locked: false,
				}))
			}
			SqlQueryResultShape::RecordPointerArray(columns) => {
				let mut rows = statement.query(params_from_iter(parameter_values)).map_err(|error| vm_error(
					instruction_index,
					format!("Failed to execute SQLite query: {error}"),
				))?;
				let mut records = Vec::new();

				while let Some(row) = rows.next().map_err(|error| vm_error(
					instruction_index,
					format!("Failed to read SQLite query result: {error}"),
				))? {
					let fields = load_sqlite_record_fields(row, columns, instruction_index)?;
					records.push(Value::RecordPointer(RecordPointerValue {
						exists: true,
						fields,
						locked: false,
					}));
				}

				Ok(Value::Array(records))
			}
		}
	}

	fn finish_current_frame(&mut self, result: Option<Value>) -> VmExecutionState {
		let frame = self.frames.pop().expect("A frame must exist while finishing execution.");
		self.stack.truncate(frame.base_stack_len);

		if let Some(caller) = self.frames.last_mut() {
			caller.instruction_index += 1;

			if let Some(result) = result {
				self.stack.push(result);
			}

			VmExecutionState::Running
		}
		else {
			self.finished_result = Some(result.clone());
			VmExecutionState::Completed(result)
		}
	}

	fn frame_debug_body_index(&self, program: &Program, frame: &CallFrame) -> usize {
		frame.function_index.unwrap_or(program.functions().len())
	}

	fn load_reference_value(&self, reference: LocalReference, instruction_index: usize) -> Result<Value, VmError> {
		let frame = self.frames.get(reference.frame_index).ok_or(vm_error(
			instruction_index,
			String::from("Reference target frame is no longer available."),
		))?;
		let value = frame.locals.get(reference.slot).cloned().ok_or(vm_error(
			instruction_index,
			format!("Reference target slot {} has not been initialized.", reference.slot),
		))?;
		self.resolve_runtime_value(value, instruction_index)
	}

	fn pop_boolean(&mut self, instruction_index: usize) -> Result<bool, VmError> {
		let value = self.stack.pop().ok_or(VmError {
			instruction_index,
			message: String::from("Stack underflow while reading Boolean operand."),
			source_location: None,
			stack_trace: Vec::new(),
		})?;

		match value {
			Value::Boolean(value) => Ok(value),
			_ => Err(VmError {
				instruction_index,
				message: String::from("Expected a Boolean operand."),
				source_location: None,
				stack_trace: Vec::new(),
			}),
		}
	}

	fn pop_call_arguments(&mut self, argument_count: usize, instruction_index: usize) -> Result<Vec<Value>, VmError> {
		let mut arguments = Vec::with_capacity(argument_count);

		for _ in 0..argument_count {
			arguments.push(self.stack.pop().ok_or(VmError {
				instruction_index,
				message: String::from("Stack underflow while reading call argument."),
				source_location: None,
				stack_trace: Vec::new(),
			})?);
		}

		arguments.reverse();
		Ok(arguments)
	}

	fn pop_condition_value(&mut self, instruction_index: usize) -> Result<bool, VmError> {
		let value = self.pop_value(instruction_index)?;

		match value {
			Value::Boolean(value) => Ok(value),
			Value::RecordPointer(record) => Ok(record.exists && !record.locked),
			Value::Null => Err(vm_error(
				instruction_index,
				String::from("Expected a `bool` or `record pointer` condition, found `null`."),
			)),
			other => Err(vm_error(
				instruction_index,
				format!("Expected a `bool` or `record pointer` condition, found `{}`.", type_name(&other)),
			)),
		}
	}

	fn pop_numeric(&mut self, instruction_index: usize) -> Result<Value, VmError> {
		let value = self.pop_value(instruction_index)?;

		if matches!(value, Value::Array(_)
			| Value::Boolean(_)
			| Value::Date(_)
			| Value::DecimalRange(_)
			| Value::Enum(_)
			| Value::IntegerRange(_)
			| Value::Iterator(_)
			| Value::Null
			| Value::Object(_)
			| Value::Reference(_)
			| Value::Text(_)
			| Value::Time(_)
			| Value::TimeTz(_)
			| Value::Timestamp(_)
			| Value::TimestampTz(_)) {
			return Err(VmError {
				instruction_index,
				message: format!("Expected a numeric operand, found a {} value.", type_name(&value)),
				source_location: None,
				stack_trace: Vec::new(),
			});
		}

		if matches!(value, Value::RecordPointer(_)) {
			return Err(VmError {
				instruction_index,
				message: format!("Expected a numeric operand, found a {} value.", type_name(&value)),
				source_location: None,
				stack_trace: Vec::new(),
			});
		}

		Ok(value)
	}

	fn pop_value(&mut self, instruction_index: usize) -> Result<Value, VmError> {
		let value = self.stack.pop().ok_or(VmError {
			instruction_index,
			message: String::from("Stack underflow while reading operand."),
			source_location: None,
			stack_trace: Vec::new(),
		})?;

		self.resolve_runtime_value(value, instruction_index)
	}

	fn resolve_runtime_value(&self, value: Value, instruction_index: usize) -> Result<Value, VmError> {
		match value {
			Value::Reference(reference) => self.load_reference_value(reference, instruction_index),
			other => Ok(other),
		}
	}

	fn resolve_visible_locals(
		&self,
		program: &Program,
		frame_index: usize,
		debug_body_index: usize,
		instruction_index: usize,
	) -> Vec<VmVisibleLocal> {
		program.visible_locals(debug_body_index, instruction_index)
			.into_iter()
			.filter_map(|local| {
				let value = self.frames.get(frame_index)?.locals.get(local.slot() as usize)?.clone();
				let value = self.resolve_runtime_value(value, instruction_index).ok()?;

				Some(VmVisibleLocal {
					declared_type: local.declared_type().to_string(),
					is_const: local.is_const(),
					name: local.name().to_string(),
					slot: local.slot(),
					value,
				})
			})
			.collect()
	}

	fn run_built_in_function(
		&mut self,
		built_in: BuiltInFunction,
		arguments: Vec<Value>,
		instruction_index: usize,
	) -> Result<Option<Value>, VmError> {
		match built_in {
			BuiltInFunction::Contains => match arguments.as_slice() {
				[Value::Text(value), Value::Text(substring)] => {
					Ok(Some(Value::Boolean(value.contains(substring))))
				}
				[Value::Array(values), Value::Text(element)] => {
					let contains = values.iter().any(|value| matches!(value, Value::Text(item) if item == element));
					Ok(Some(Value::Boolean(contains)))
				}
				[left, right] => Err(vm_error(
					instruction_index,
					format!(
						"Built-in function `contains` does not accept `{}` and `{}` values.",
						type_name(left),
						type_name(right),
					),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `contains` expects 2 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::CountOf => match arguments.as_slice() {
				[Value::Text(value), Value::Array(values)] => {
					let count = values.iter()
						.filter(|item| matches!(item, Value::Text(element) if element == value))
						.count() as i64;
					Ok(Some(Value::Integer(count)))
				}
				[Value::Text(substring), Value::Text(value)] => {
					Ok(Some(Value::Integer(count_non_overlapping_substrings(value, substring) as i64)))
				}
				[left, right] => Err(vm_error(
					instruction_index,
					format!(
						"Built-in function `countof` does not accept `{}` and `{}` values.",
						type_name(left),
						type_name(right),
					),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `countof` expects 2 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::Len => match arguments.as_slice() {
				[Value::Array(values)] => Ok(Some(Value::Integer(values.len() as i64))),
				[Value::Text(value)] => Ok(Some(Value::Integer(value.chars().count() as i64))),
				[value] => Err(vm_error(
					instruction_index,
					format!("Built-in function `len` does not accept a `{}` value.", type_name(value)),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `len` expects 1 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::Disp => match arguments.as_slice() {
				[Value::Text(value)] => {
					print!("{value}");
					Ok(None)
				}
				[value] => Err(vm_error(
					instruction_index,
					format!("Built-in function `disp` does not accept a `{}` value.", type_name(value)),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `disp` expects 1 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::Displn => match arguments.as_slice() {
				[Value::Text(value)] => {
					print!("{value}");
					println!();
					Ok(None)
				}
				[value] => Err(vm_error(
					instruction_index,
					format!("Built-in function `displn` does not accept a `{}` value.", type_name(value)),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `displn` expects 1 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::Exists => match arguments.as_slice() {
				[Value::RecordPointer(record)] => Ok(Some(Value::Boolean(record.exists))),
				[value] => Err(vm_error(
					instruction_index,
					format!("Built-in function `exists` does not accept a `{}` value.", type_name(value)),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `exists` expects 1 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::IndexOf => match arguments.as_slice() {
				[Value::Text(value), Value::Array(values)] => {
					let index = values.iter()
						.position(|item| matches!(item, Value::Text(element) if element == value))
						.map(|index| Value::Integer(index as i64 + 1))
						.unwrap_or(Value::Null);
					Ok(Some(index))
				}
				[Value::Text(substring), Value::Text(value)] => {
					let result = value.find(substring)
						.map(|index| Value::Integer(index as i64 + 1))
						.unwrap_or(Value::Null);
					Ok(Some(result))
				}
				[left, right] => Err(vm_error(
					instruction_index,
					format!(
						"Built-in function `indexof` does not accept `{}` and `{}` values.",
						type_name(left),
						type_name(right),
					),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `indexof` expects 2 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::Locked => match arguments.as_slice() {
				[Value::RecordPointer(record)] => Ok(Some(Value::Boolean(record.locked))),
				[value] => Err(vm_error(
					instruction_index,
					format!("Built-in function `locked` does not accept a `{}` value.", type_name(value)),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `locked` expects 1 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::Split => match arguments.as_slice() {
				[Value::Text(value), Value::Text(separator)] => {
					if separator.is_empty() {
						return Ok(Some(Value::Array(vec![Value::Text(value.clone())])));
					}

					Ok(Some(Value::Array(
						value.split(separator)
							.map(|part| Value::Text(part.to_string()))
							.collect(),
					)))
				}
				[left, right] => Err(vm_error(
					instruction_index,
					format!(
						"Built-in function `split` does not accept `{}` and `{}` values.",
						type_name(left),
						type_name(right),
					),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `split` expects 2 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::Trim => match arguments.as_slice() {
				[Value::Text(value)] => Ok(Some(Value::Text(value.trim().to_string()))),
				[value] => Err(vm_error(
					instruction_index,
					format!("Built-in function `trim` does not accept a `{}` value.", type_name(value)),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `trim` expects 1 argument(s), found {}.", arguments.len()),
				)),
			},
			BuiltInFunction::IntCast
			| BuiltInFunction::TextCast
			| BuiltInFunction::DecCast
			| BuiltInFunction::BoolCast
			| BuiltInFunction::DateCast => match arguments.as_slice() {
				[Value::Enum(value)] => Ok(Some((*value.backing_value).clone())),
				[value] => Err(vm_error(
					instruction_index,
					format!("Built-in function `{}` does not accept a `{}` value.", built_in.name(), type_name(value)),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `{}` expects 1 argument(s), found {}.", built_in.name(), arguments.len()),
				)),
			},
		}
	}

	fn sqlite_parameter_value(
		&self,
		slot: u32,
		field_path: &[String],
		locals: &[Value],
		instruction_index: usize,
	) -> Result<SqlValue, VmError> {
		let value = locals.get(slot as usize).cloned().ok_or(vm_error(
			instruction_index,
			format!("Local slot {} has not been initialized.", slot),
		))?;
		let value = self.resolve_runtime_value(value, instruction_index)?;
		let value = if field_path.is_empty() {
			value
		}
		else {
			load_field_path_value(value, field_path, instruction_index)?
		};

		match value {
			Value::Boolean(value) => Ok(SqlValue::Integer(if value { 1 } else { 0 })),
			Value::Date(value) => Ok(SqlValue::Text(value.to_string())),
			Value::Decimal(value) => Ok(SqlValue::Text(value.to_string())),
			Value::Integer(value) => Ok(SqlValue::Integer(value)),
			Value::Null => Ok(SqlValue::Null),
			Value::Text(value) => Ok(SqlValue::Text(value)),
			Value::Time(value) => Ok(SqlValue::Text(value.to_string())),
			Value::TimeTz(value) => Ok(SqlValue::Text(value.to_string())),
			Value::Timestamp(value) => Ok(SqlValue::Text(value.to_string())),
			Value::TimestampTz(value) => Ok(SqlValue::Text(value.to_string())),
			other => Err(vm_error(
				instruction_index,
				format!("Cannot bind a `{}` value into a SQLite query parameter.", type_name(&other)),
			)),
		}
	}

	fn store_reference_value(&mut self, reference: LocalReference, value: Value, instruction_index: usize) -> Result<(), VmError> {
		let target = {
			let frame = self.frames.get(reference.frame_index).ok_or(vm_error(
				instruction_index,
				String::from("Reference target frame is no longer available."),
			))?;
			frame.locals.get(reference.slot).cloned().ok_or(vm_error(
				instruction_index,
				format!("Reference target slot {} has not been initialized.", reference.slot),
			))?
		};

		match target {
			Value::Reference(next_reference) => self.store_reference_value(next_reference, value, instruction_index),
			_ => {
				let frame = self.frames.get_mut(reference.frame_index).ok_or(vm_error(
					instruction_index,
					String::from("Reference target frame is no longer available."),
				))?;
				frame.locals[reference.slot] = value;
				Ok(())
			}
		}
	}
}

impl CallFrame {
	fn new(function_index: Option<usize>, base_stack_len: usize, locals: Vec<Value>) -> Self {
		Self {
			base_stack_len,
			function_index,
			instruction_index: 0,
			locals,
		}
	}
}

fn add_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Array(mut lhs), Value::Array(rhs)) => {
			lhs.extend(rhs);
			Ok(Value::Array(lhs))
		}
		(Value::Text(lhs), Value::Text(rhs)) => Ok(Value::Text(lhs + &rhs)),
		(Value::Text(lhs), rhs) => Ok(Value::Text(lhs + &stringify_value(&rhs))),
		(lhs, Value::Text(rhs)) => Ok(Value::Text(stringify_value(&lhs) + &rhs)),
		(Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs.saturating_add(rhs))),
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_add(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn align_comparable_numeric_values(lhs: &Decimal, rhs: &Decimal, instruction_index: usize) -> Result<(i128, i128), VmError> {
	let scale = lhs.scale.max(rhs.scale);
	let lhs_factor = pow10_i128((scale - lhs.scale) as u32)
		.map_err(|message| vm_error(instruction_index, message))?;
	let rhs_factor = pow10_i128((scale - rhs.scale) as u32)
		.map_err(|message| vm_error(instruction_index, message))?;
	let lhs = lhs.coefficient.checked_mul(lhs_factor)
		.ok_or_else(|| vm_error(instruction_index, String::from("Numeric comparison overflowed the supported precision.")))?;
	let rhs = rhs.coefficient.checked_mul(rhs_factor)
		.ok_or_else(|| vm_error(instruction_index, String::from("Numeric comparison overflowed the supported precision.")))?;
	Ok((lhs, rhs))
}

fn coerce_numeric_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<(Decimal, Decimal), VmError> {
	match (lhs, rhs) {
		(Value::Decimal(lhs), Value::Decimal(rhs)) => Ok((lhs, rhs)),
		(Value::Decimal(lhs), Value::Integer(rhs)) => {
			let rhs = Decimal::from_integer_with_scale(rhs, lhs.scale)
				.map_err(|message| vm_error(instruction_index, message))?;

			Ok((lhs, rhs))
		}
		(Value::Integer(lhs), Value::Decimal(rhs)) => {
			let lhs = Decimal::from_integer_with_scale(lhs, rhs.scale)
				.map_err(|message| vm_error(instruction_index, message))?;

			Ok((lhs, rhs))
		}
		(Value::Integer(lhs), Value::Integer(rhs)) => Ok((Decimal::from_integer(lhs), Decimal::from_integer(rhs))),
		_ => Err(vm_error(instruction_index, String::from("Expected numeric operands."))),
	}
}

fn coerce_range_operand(value: Value, instruction_index: usize) -> Result<Decimal, VmError> {
	match value {
		Value::Decimal(value) => Ok(value),
		Value::Integer(value) => Ok(Decimal::from_integer(value)),
		other => Err(vm_error(
			instruction_index,
			format!("Range operands must be numeric, found `{}`.", type_name(&other)),
		)),
	}
}

fn compare_decimals(lhs: &Decimal, rhs: &Decimal, instruction_index: usize) -> Result<std::cmp::Ordering, VmError> {
	let (lhs, rhs) = align_comparable_numeric_values(lhs, rhs, instruction_index)?;
	Ok(lhs.cmp(&rhs))
}

fn compare_values(lhs: Value, rhs: Value, instruction_index: usize, kind: ComparisonKind) -> Result<Value, VmError> {
	if matches!(lhs, Value::Enum(_)) || matches!(rhs, Value::Enum(_)) {
		return Err(vm_error(
			instruction_index,
			format!("Cannot compare `{}` and `{}` for ordering.", type_name(&lhs), type_name(&rhs)),
		));
	}

	if let (Value::Date(lhs), Value::Date(rhs)) = (&lhs, &rhs) {
		let ordering = lhs.cmp(rhs);
		let value = match kind {
			ComparisonKind::GreaterThan => ordering.is_gt(),
			ComparisonKind::GreaterThanOrEqual => ordering.is_ge(),
			ComparisonKind::LessThan => ordering.is_lt(),
			ComparisonKind::LessThanOrEqual => ordering.is_le(),
		};

		return Ok(Value::Boolean(value));
	}

	if let (Value::Time(lhs), Value::Time(rhs)) = (&lhs, &rhs) {
		let ordering = lhs.cmp(rhs);
		let value = match kind {
			ComparisonKind::GreaterThan => ordering.is_gt(),
			ComparisonKind::GreaterThanOrEqual => ordering.is_ge(),
			ComparisonKind::LessThan => ordering.is_lt(),
			ComparisonKind::LessThanOrEqual => ordering.is_le(),
		};

		return Ok(Value::Boolean(value));
	}

	if let (Value::TimeTz(lhs), Value::TimeTz(rhs)) = (&lhs, &rhs) {
		let ordering = lhs.cmp(rhs);
		let value = match kind {
			ComparisonKind::GreaterThan => ordering.is_gt(),
			ComparisonKind::GreaterThanOrEqual => ordering.is_ge(),
			ComparisonKind::LessThan => ordering.is_lt(),
			ComparisonKind::LessThanOrEqual => ordering.is_le(),
		};

		return Ok(Value::Boolean(value));
	}

	if let (Value::Timestamp(lhs), Value::Timestamp(rhs)) = (&lhs, &rhs) {
		let ordering = lhs.cmp(rhs);
		let value = match kind {
			ComparisonKind::GreaterThan => ordering.is_gt(),
			ComparisonKind::GreaterThanOrEqual => ordering.is_ge(),
			ComparisonKind::LessThan => ordering.is_lt(),
			ComparisonKind::LessThanOrEqual => ordering.is_le(),
		};

		return Ok(Value::Boolean(value));
	}

	if let (Value::TimestampTz(lhs), Value::TimestampTz(rhs)) = (&lhs, &rhs) {
		let ordering = lhs.cmp(rhs);
		let value = match kind {
			ComparisonKind::GreaterThan => ordering.is_gt(),
			ComparisonKind::GreaterThanOrEqual => ordering.is_ge(),
			ComparisonKind::LessThan => ordering.is_lt(),
			ComparisonKind::LessThanOrEqual => ordering.is_le(),
		};

		return Ok(Value::Boolean(value));
	}

	if let (Value::Text(lhs), Value::Text(rhs)) = (&lhs, &rhs) {
		let ordering = lhs.cmp(rhs);
		let value = match kind {
			ComparisonKind::GreaterThan => ordering.is_gt(),
			ComparisonKind::GreaterThanOrEqual => ordering.is_ge(),
			ComparisonKind::LessThan => ordering.is_lt(),
			ComparisonKind::LessThanOrEqual => ordering.is_le(),
		};

		return Ok(Value::Boolean(value));
	}

	if matches!(lhs, Value::Date(_)
			| Value::Time(_)
			| Value::TimeTz(_)
			| Value::Timestamp(_)
			| Value::TimestampTz(_))
		|| matches!(rhs, Value::Date(_)
			| Value::Time(_)
			| Value::TimeTz(_)
			| Value::Timestamp(_)
			| Value::TimestampTz(_))
		|| matches!(lhs, Value::Text(_))
		|| matches!(rhs, Value::Text(_)) {
		return Err(vm_error(
			instruction_index,
			format!("Cannot compare `{}` and `{}` for ordering.", type_name(&lhs), type_name(&rhs)),
		));
	}

	let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
	let ordering = compare_decimals(&lhs, &rhs, instruction_index)?;
	let value = match kind {
		ComparisonKind::GreaterThan => ordering.is_gt(),
		ComparisonKind::GreaterThanOrEqual => ordering.is_ge(),
		ComparisonKind::LessThan => ordering.is_lt(),
		ComparisonKind::LessThanOrEqual => ordering.is_le(),
	};

	Ok(Value::Boolean(value))
}

fn count_non_overlapping_substrings(value: &str, substring: &str) -> usize {
	if substring.is_empty() {
		return 0;
	}

	let mut count = 0;
	let mut offset = 0;

	while let Some(index) = value[offset..].find(substring) {
		count += 1;
		offset += index + substring.len();
	}

	count
}

fn deferred_sqlite_value(value: SqlValueRef<'_>) -> Result<DeferredSqliteValue, String> {
	match value {
		SqlValueRef::Blob(value) => Ok(DeferredSqliteValue::Blob(value.to_vec())),
		SqlValueRef::Integer(value) => Ok(DeferredSqliteValue::Integer(value)),
		SqlValueRef::Null => Ok(DeferredSqliteValue::Null),
		SqlValueRef::Real(value) => Ok(DeferredSqliteValue::Real(value.to_string())),
		SqlValueRef::Text(value) => Ok(DeferredSqliteValue::Text(
			std::str::from_utf8(value)
				.map_err(|_| String::from("SQLite returned invalid UTF-8 text data."))?
				.to_string(),
		)),
	}
}

fn divide_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Integer(lhs), Value::Integer(rhs)) => {
			if rhs == 0 {
				return Err(vm_error(instruction_index, String::from("Division by zero.")));
			}

			Ok(Value::Integer(lhs / rhs))
		}
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_div(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn equals_value(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	let value = match (lhs, rhs) {
		(Value::Array(lhs), Value::Array(rhs)) => lhs == rhs,
		(Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
		(Value::Date(lhs), Value::Date(rhs)) => lhs == rhs,
		(Value::Enum(lhs), Value::Enum(rhs)) => {
			lhs.enum_name == rhs.enum_name && equals_value(*lhs.backing_value, *rhs.backing_value, instruction_index)? == Value::Boolean(true)
		}
		(Value::Null, Value::Null) => true,
		(Value::Null, _) | (_, Value::Null) => false,
		(Value::Object(lhs), Value::Object(rhs)) => lhs == rhs,
		(Value::RecordPointer(lhs), Value::RecordPointer(rhs)) => lhs == rhs,
		(Value::Text(lhs), Value::Text(rhs)) => lhs == rhs,
		(Value::Time(lhs), Value::Time(rhs)) => lhs == rhs,
		(Value::TimeTz(lhs), Value::TimeTz(rhs)) => lhs == rhs,
		(Value::Timestamp(lhs), Value::Timestamp(rhs)) => lhs == rhs,
		(Value::TimestampTz(lhs), Value::TimestampTz(rhs)) => lhs == rhs,
		(lhs @ Value::Enum(_), rhs) | (lhs, rhs @ Value::Enum(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::Iterator(_), rhs) | (lhs, rhs @ Value::Iterator(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::Array(_), rhs) | (lhs, rhs @ Value::Array(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::Date(_), rhs) | (lhs, rhs @ Value::Date(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::Time(_), rhs) | (lhs, rhs @ Value::Time(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::TimeTz(_), rhs) | (lhs, rhs @ Value::TimeTz(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::Timestamp(_), rhs) | (lhs, rhs @ Value::Timestamp(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::TimestampTz(_), rhs) | (lhs, rhs @ Value::TimestampTz(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::Object(_), rhs) | (lhs, rhs @ Value::Object(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::RecordPointer(_), rhs) | (lhs, rhs @ Value::RecordPointer(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::Text(_), rhs) | (lhs, rhs @ Value::Text(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs @ Value::Boolean(_), rhs) | (lhs, rhs @ Value::Boolean(_)) => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot compare `{}` and `{}` for equality.", type_name(&lhs), type_name(&rhs)),
			));
		}
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			compare_decimals(&lhs, &rhs, instruction_index)?.is_eq()
		}
	};

	Ok(Value::Boolean(value))
}

fn iterator_has_next(iterator: &Value, instruction_index: usize) -> Result<bool, VmError> {
	let iterator = match iterator {
		Value::Iterator(iterator) => iterator,
		other => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot iterate over a `{}` value.", type_name(other)),
			));
		}
	};

	match iterator {
		IteratorState::Array(iterator) => Ok(iterator.next_index < iterator.elements.len()),
		IteratorState::DecimalRange(iterator) => Ok(iterator.next_value.is_some()),
		IteratorState::IntegerRange(iterator) => Ok(iterator.next_value.is_some()),
	}
}

fn iterator_next_value(iterator: Value, instruction_index: usize) -> Result<(Value, Value), VmError> {
	let iterator = match iterator {
		Value::Iterator(iterator) => iterator,
		other => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot iterate over a `{}` value.", type_name(&other)),
			));
		}
	};

	match iterator {
		IteratorState::Array(mut iterator) => {
			let value = iterator.elements.get(iterator.next_index).cloned().ok_or(vm_error(
				instruction_index,
				String::from("Iterator is exhausted."),
			))?;
			iterator.next_index += 1;
			Ok((value, Value::Iterator(IteratorState::Array(iterator))))
		}
		IteratorState::DecimalRange(mut iterator) => {
			let value = iterator.next_value.clone().ok_or(vm_error(
				instruction_index,
				String::from("Iterator is exhausted."),
			))?;
			let next_value = value.checked_add(&iterator.step)
				.map_err(|message| vm_error(instruction_index, message))?;
			let ordering = compare_decimals(&next_value, &iterator.end, instruction_index)?;
			let in_bounds = if iterator.step.coefficient > 0 {
				!ordering.is_gt()
			}
			else {
				!ordering.is_lt()
			};
			iterator.next_value = if in_bounds { Some(next_value) } else { None };
			Ok((Value::Decimal(value), Value::Iterator(IteratorState::DecimalRange(iterator))))
		}
		IteratorState::IntegerRange(mut iterator) => {
			let value = iterator.next_value.ok_or(vm_error(
				instruction_index,
				String::from("Iterator is exhausted."),
			))?;
			let next_value = value.checked_add(iterator.step);
			let in_bounds = next_value.is_some_and(|next_value| {
				if iterator.step > 0 {
					next_value <= iterator.end
				}
				else {
					next_value >= iterator.end
				}
			});
			iterator.next_value = if in_bounds { next_value } else { None };
			Ok((Value::Integer(value), Value::Iterator(IteratorState::IntegerRange(iterator))))
		}
	}
}

fn load_field_path_value(object: Value, field_path: &[String], instruction_index: usize) -> Result<Value, VmError> {
	let mut value = object;

	for field_name in field_path {
		value = load_field_value(value, field_name, instruction_index)?;
	}

	Ok(value)
}

fn load_field_value(object: Value, field_name: &str, instruction_index: usize) -> Result<Value, VmError> {
	match object {
		Value::Object(fields) => fields.get(field_name).cloned().ok_or(vm_error(
			instruction_index,
			format!("Object does not contain a field named `{field_name}`."),
		)),
		Value::RecordPointer(record) => {
			if !record.exists {
				return Err(vm_error(
					instruction_index,
					String::from("Cannot access fields on a record pointer that does not reference a record."),
				));
			}
			if record.locked {
				return Err(vm_error(
					instruction_index,
					String::from("Cannot access fields on a locked record pointer."),
				));
			}

			record.fields.get(&normalize_record_field_name(field_name))
				.ok_or(vm_error(
					instruction_index,
					format!("Record pointer does not contain a field named `{field_name}`."),
				))
				.and_then(|field| resolve_record_field_value(field, instruction_index))
		}
		other => Err(vm_error(
			instruction_index,
			format!("Field access requires an object operand, found `{}`.", type_name(&other)),
		)),
	}
}

fn load_index_value(array: Value, index: Value, instruction_index: usize) -> Result<Value, VmError> {
	let values = match array {
		Value::Array(values) => values,
		other => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot index a `{}` value.", type_name(&other)),
			));
		}
	};

	match index {
		Value::Integer(index) => {
			if index < 1 || index as usize > values.len() {
				return Err(vm_error(
					instruction_index,
					format!("Array index {} is out of bounds for length {}.", index, values.len()),
				));
			}

			Ok(values[index as usize - 1].clone())
		}
		Value::IntegerRange(range) => load_range_slice_value(values, range, instruction_index),
		Value::DecimalRange(_) => Err(vm_error(
			instruction_index,
			String::from("Array slicing requires a range of `int`."),
		)),
		other => Err(vm_error(
			instruction_index,
			format!("Array index must be an `int`, found `{}`.", type_name(&other)),
		)),
	}
}

fn load_range_slice_value(values: Vec<Value>, range: IntegerRange, instruction_index: usize) -> Result<Value, VmError> {
	let step = range.step.unwrap_or(1);

	if step == 0 {
		return Err(vm_error(instruction_index, String::from("Range step cannot be zero.")));
	}

	let mut result = Vec::new();
	let mut current = Some(range.start);

	while let Some(index) = current {
		let in_bounds = if step > 0 {
			index <= range.end
		}
		else {
			index >= range.end
		};

		if !in_bounds {
			break;
		}

		if index < 1 || index as usize > values.len() {
			return Err(vm_error(
				instruction_index,
				format!("Array index {} is out of bounds for length {}.", index, values.len()),
			));
		}

		result.push(values[index as usize - 1].clone());
		current = index.checked_add(step);
	}

	Ok(Value::Array(result))
}

fn load_sqlite_record_fields(
	row: &rusqlite::Row<'_>,
	columns: &[QueryResultColumn],
	instruction_index: usize,
) -> Result<BTreeMap<String, RecordFieldValue>, VmError> {
	let mut fields = BTreeMap::new();

	for (index, column) in columns.iter().enumerate() {
		let value = row.get_ref(index).map_err(|error| vm_error(
			instruction_index,
			format!("Failed to read SQLite column `{}`: {error}", column.column_name),
		))?;
		fields.insert(
			normalize_record_field_name(&column.column_name),
			RecordFieldValue::DeferredSqlite {
				data_type: column.data_type.clone(),
				is_nullable: column.is_nullable,
				value: deferred_sqlite_value(value).map_err(|message| vm_error(instruction_index, message))?,
			},
		);
	}

	Ok(fields)
}

fn make_iterator_value(iterable: Value, instruction_index: usize) -> Result<Value, VmError> {
	match iterable {
		Value::Array(elements) => Ok(Value::Iterator(IteratorState::Array(crate::value::ArrayIterator {
			elements,
			next_index: 0,
		}))),
		Value::DecimalRange(range) => {
			let step = range.step.unwrap_or_else(|| Decimal::from_integer(1));

			if step.coefficient == 0 {
				return Err(vm_error(instruction_index, String::from("Range step cannot be zero.")));
			}

			let ordering = compare_decimals(&range.start, &range.end, instruction_index)?;
			let in_bounds = if step.coefficient > 0 {
				!ordering.is_gt()
			}
			else {
				!ordering.is_lt()
			};

			Ok(Value::Iterator(IteratorState::DecimalRange(DecimalRangeIterator {
				end: range.end,
				next_value: if in_bounds { Some(range.start) } else { None },
				step,
			})))
		}
		Value::IntegerRange(range) => {
			let step = range.step.unwrap_or(1);

			if step == 0 {
				return Err(vm_error(instruction_index, String::from("Range step cannot be zero.")));
			}

			let in_bounds = if step > 0 {
				range.start <= range.end
			}
			else {
				range.start >= range.end
			};

			Ok(Value::Iterator(IteratorState::IntegerRange(IntegerRangeIterator {
				end: range.end,
				next_value: if in_bounds { Some(range.start) } else { None },
				step,
			})))
		}
		other => Err(vm_error(
			instruction_index,
			format!("Cannot iterate over a `{}` value.", type_name(&other)),
		)),
	}
}

fn make_range_value(start: Value, step: Option<Value>, end: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (start, step, end) {
		(Value::Integer(start), Some(Value::Integer(step)), Value::Integer(end)) => {
			if step == 0 {
				return Err(vm_error(instruction_index, String::from("Range step cannot be zero.")));
			}

			Ok(Value::IntegerRange(IntegerRange {
				end,
				start,
				step: Some(step),
			}))
		}
		(Value::Integer(start), None, Value::Integer(end)) => Ok(Value::IntegerRange(IntegerRange {
			end,
			start,
			step: None,
		})),
		(start, step, end) => {
			let step_was_explicit = step.is_some();
			let start = coerce_range_operand(start, instruction_index)?;
			let step = coerce_range_operand(step.unwrap_or(Value::Integer(1)), instruction_index)?;
			let end = coerce_range_operand(end, instruction_index)?;

			if step.coefficient == 0 {
				return Err(vm_error(instruction_index, String::from("Range step cannot be zero.")));
			}

			Ok(Value::DecimalRange(DecimalRange {
				end,
				start,
				step: if step_was_explicit { Some(step) } else { None },
			}))
		}
	}
}

fn modulo_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Integer(lhs), Value::Integer(rhs)) => {
			if rhs == 0 {
				return Err(vm_error(instruction_index, String::from("Modulo by zero.")));
			}

			Ok(Value::Integer(lhs % rhs))
		}
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_rem(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn multiply_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs.saturating_mul(rhs))),
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_mul(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn negate_value(value: Value) -> Value {
	match value {
		Value::Array(_) => unreachable!("Array values are rejected before numeric negation."),
		Value::Boolean(_) => unreachable!("Boolean values are rejected before numeric negation."),
		Value::Date(_) => unreachable!("Date values are rejected before numeric negation."),
		Value::Decimal(mut decimal) => {
			decimal.coefficient = decimal.coefficient.saturating_neg();
			Value::Decimal(decimal)
		}
		Value::DecimalRange(_) => unreachable!("Range values are rejected before numeric negation."),
		Value::Enum(_) => unreachable!("Enum values are rejected before numeric negation."),
		Value::Integer(integer) => Value::Integer(integer.saturating_neg()),
		Value::IntegerRange(_) => unreachable!("Range values are rejected before numeric negation."),
		Value::Iterator(_) => unreachable!("Iterator values are rejected before numeric negation."),
		Value::Null => unreachable!("Null values are rejected before numeric negation."),
		Value::Object(_) => unreachable!("Object values are rejected before numeric negation."),
		Value::RecordPointer(_) => unreachable!("Record pointer values are rejected before numeric negation."),
		Value::Reference(_) => unreachable!("Reference values are resolved before numeric negation."),
		Value::Text(_) => unreachable!("Text values are rejected before numeric negation."),
		Value::Time(_) => unreachable!("Time values are rejected before numeric negation."),
		Value::TimeTz(_) => unreachable!("Time-zone-aware time values are rejected before numeric negation."),
		Value::Timestamp(_) => unreachable!("Timestamp values are rejected before numeric negation."),
		Value::TimestampTz(_) => unreachable!("Time-zone-aware timestamp values are rejected before numeric negation."),
	}
}

fn normalize_database_name(name: &str) -> String {
	name.to_ascii_lowercase()
}

fn normalize_record_field_name(name: &str) -> String {
	name.to_ascii_lowercase()
}

fn not_equals_value(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match equals_value(lhs, rhs, instruction_index)? {
		Value::Boolean(value) => Ok(Value::Boolean(!value)),
		Value::Date(_) => unreachable!("Date values are rejected before logical negation."),
		Value::Time(_) => unreachable!("Time values are rejected before logical negation."),
		Value::TimeTz(_) => unreachable!("Time-zone-aware time values are rejected before logical negation."),
		Value::Timestamp(_) => unreachable!("Timestamp values are rejected before logical negation."),
		Value::TimestampTz(_) => unreachable!("Time-zone-aware timestamp values are rejected before logical negation."),
		_ => unreachable!("Equality comparisons always produce Boolean results."),
	}
}

fn pow10_i128(exponent: u32) -> Result<i128, String> {
	let mut value = 1_i128;

	for _ in 0..exponent {
		value = value.checked_mul(10)
			.ok_or(String::from("Numeric comparison overflowed the supported precision."))?;
	}

	Ok(value)
}

fn runtime_value_from_constant(constant: &crate::bytecode::Constant) -> Value {
	match constant {
		crate::bytecode::Constant::Boolean(value) => Value::Boolean(*value),
		crate::bytecode::Constant::Date(value) => Value::Date(*value),
		crate::bytecode::Constant::Decimal(value) => Value::Decimal(value.clone()),
		crate::bytecode::Constant::Integer(value) => Value::Integer(*value),
		crate::bytecode::Constant::Text(value) => Value::Text(value.clone()),
	}
}

fn resolve_record_field_value(field: &RecordFieldValue, instruction_index: usize) -> Result<Value, VmError> {
	match field {
		RecordFieldValue::Materialized(value) => Ok(value.clone()),
		RecordFieldValue::DeferredSqlite { data_type, is_nullable, value } => {
			sqlite_record_field_runtime_value(value, data_type, *is_nullable)
				.map_err(|message| vm_error(instruction_index, message))
		}
	}
}

fn sqlite_path_from_connection_string(database_name: &str, value: &str) -> Result<PathBuf, String> {
	if value.is_empty() {
		return Err(format!(
			"SQLite connection string for database `{database_name}` must include a database path."
		));
	}

	if value.starts_with("//") && !value.starts_with("///") {
		return Err(format!(
			"SQLite connection string for database `{database_name}` must use `sqlite:/path/to.db`, `sqlite:relative.db`, or `sqlite::memory:`."
		));
	}

	let normalized = if value.starts_with("///") {
		&value[2..]
	}
	else {
		value
	};

	Ok(PathBuf::from(normalized))
}

fn store_field_path_into_object(
	object: Value,
	field_path: &[String],
	value: Value,
	instruction_index: usize,
) -> Result<Value, VmError> {
	let Value::Object(mut fields) = object else {
		return Err(vm_error(
			instruction_index,
			String::from("Field assignment requires an object operand."),
		));
	};

	let Some((field_name, remaining_path)) = field_path.split_first() else {
		return Err(vm_error(
			instruction_index,
			String::from("Field assignment requires at least one field name."),
		));
	};

	if remaining_path.is_empty() {
		if !fields.contains_key(field_name) {
			return Err(vm_error(
				instruction_index,
				format!("Object does not contain a field named `{field_name}`."),
			));
		}

		fields.insert(field_name.clone(), value);
		return Ok(Value::Object(fields));
	}

	let child = fields.remove(field_name).ok_or(vm_error(
		instruction_index,
		format!("Object does not contain a field named `{field_name}`."),
	))?;
	let updated_child = store_field_path_into_object(child, remaining_path, value, instruction_index)?;
	fields.insert(field_name.clone(), updated_child);
	Ok(Value::Object(fields))
}

fn store_field_path_value(
	object: Value,
	field_path: &[String],
	value: Value,
	instruction_index: usize,
) -> Result<(Value, Value), VmError> {
	let updated_object = store_field_path_into_object(object, field_path, value.clone(), instruction_index)?;
	Ok((value, updated_object))
}

fn store_index_value(array: Value, index: Value, value: Value, instruction_index: usize) -> Result<(Value, Value), VmError> {
	let index = match index {
		Value::Integer(value) => value,
		other => {
			return Err(vm_error(
				instruction_index,
				format!("Array index must be an `int`, found `{}`.", type_name(&other)),
			));
		}
	};

	let mut values = match array {
		Value::Array(values) => values,
		other => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot index a `{}` value.", type_name(&other)),
			));
		}
	};

	if index < 1 {
		return Err(vm_error(
			instruction_index,
			format!("Array index {} is out of bounds for length {}.", index, values.len()),
		));
	}

	let zero_based_index = index as usize - 1;

	if zero_based_index < values.len() {
		values[zero_based_index] = value.clone();
		return Ok((value, Value::Array(values)));
	}

	if zero_based_index == values.len() {
		values.push(value.clone());
		return Ok((value, Value::Array(values)));
	}

	Err(vm_error(
		instruction_index,
		format!("Array index {} is out of bounds for length {}.", index, values.len()),
	))
}

fn stringify_value(value: &Value) -> String {
	match value {
		Value::Array(values) => {
			let mut result = String::from("[");

			for (index, value) in values.iter().enumerate() {
				if index > 0 {
					result.push_str(", ");
				}

				result.push_str(&stringify_value(value));
			}

			result.push(']');
			result
		}
		Value::Boolean(value) => value.to_string(),
		Value::Date(value) => value.to_string(),
		Value::Decimal(value) => value.to_string(),
		Value::DecimalRange(value) => value.to_string(),
		Value::Enum(value) => value.variant_name.clone(),
		Value::Integer(value) => value.to_string(),
		Value::IntegerRange(value) => value.to_string(),
		Value::Iterator(_) => String::from("<iterator>"),
		Value::Null => String::from("null"),
		Value::Object(fields) => {
			let mut result = String::from("{");

			for (index, (name, value)) in fields.iter().enumerate() {
				if index > 0 {
					result.push_str(", ");
				}

				result.push_str(name);
				result.push_str(": ");
				result.push_str(&stringify_value(value));
			}

			result.push('}');
			result
		}
		Value::RecordPointer(record) => {
			if !record.exists {
				String::from("<record pointer: missing>")
			}
			else if record.locked {
				String::from("<record pointer: locked>")
			}
			else {
				String::from("<record pointer>")
			}
		}
		Value::Reference(_) => String::from("<reference>"),
		Value::Text(value) => value.clone(),
		Value::Time(value) => value.to_string(),
		Value::TimeTz(value) => value.to_string(),
		Value::Timestamp(value) => value.to_string(),
		Value::TimestampTz(value) => value.to_string(),
	}
}

fn subtract_values(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match (lhs, rhs) {
		(Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs.saturating_sub(rhs))),
		(lhs, rhs) => {
			let (lhs, rhs) = coerce_numeric_values(lhs, rhs, instruction_index)?;
			Ok(Value::Decimal(lhs.checked_sub(&rhs).map_err(|message| vm_error(instruction_index, message))?))
		}
	}
}

fn type_name(value: &Value) -> &'static str {
	match value {
		Value::Array(_) => "array",
		Value::Boolean(_) => "bool",
		Value::Date(_) => "date",
		Value::Decimal(_) => "dec",
		Value::DecimalRange(_) => "range",
		Value::Enum(_) => "enum",
		Value::Integer(_) => "int",
		Value::IntegerRange(_) => "range",
		Value::Iterator(_) => "iterator",
		Value::Null => "null",
		Value::Object(_) => "object",
		Value::RecordPointer(_) => "record pointer",
		Value::Reference(_) => "reference",
		Value::Text(_) => "text",
		Value::Time(_) => "time",
		Value::TimeTz(_) => "timetz",
		Value::Timestamp(_) => "timestamp",
		Value::TimestampTz(_) => "timestamptz",
	}
}

fn vm_error(instruction_index: usize, message: String) -> VmError {
	VmError {
		instruction_index,
		message,
		source_location: None,
		stack_trace: Vec::new(),
	}
}

#[cfg(test)]
mod tests {
	use crate::builtins::BuiltInFunction;
	use crate::bytecode::CodeBody;
	use crate::bytecode::CodeBodyDebugInfo;
	use crate::bytecode::ConstantPool;
	use crate::bytecode::DebugInfo;
	use crate::bytecode::Instruction;
	use crate::bytecode::LocalVariableDebugInfo;
	use crate::bytecode::Program;
	use crate::value::Decimal;
	use crate::value::DecimalRange;
	use crate::value::Value;

	use super::RuntimeDatabaseConfig;
	use super::VirtualMachine;
	use super::VmError;

	#[test]
	fn captures_visible_locals_in_runtime_stack_frame() {
		let program = Program::from_parts_with_debug(
			ConstantPool::default(),
			CodeBody::new(vec![
				Instruction::PushInteger(0),
				Instruction::StoreLocal(0),
				Instruction::LoadLocal(0),
				Instruction::PushInteger(0),
				Instruction::Divide,
			]),
			DebugInfo::new(
				vec![CodeBodyDebugInfo::new(
					None,
					vec![0, 0, 0, 0, 0],
					vec![LocalVariableDebugInfo::new("x", 0, "int", false, 1, 5)],
					None,
				)],
				vec![],
			),
		);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error.message, "Division by zero.");
		assert_eq!(error.stack_trace.len(), 1);
		assert_eq!(
			error.stack_trace[0].locals,
			vec![super::VmVisibleLocal {
				declared_type: String::from("int"),
				is_const: false,
				name: String::from("x"),
				slot: 0,
				value: Value::Integer(0),
			}],
		);
		assert_eq!(error.current_frame().unwrap().instruction_index(), 4);
		assert_eq!(error.current_frame().unwrap().local("x").unwrap().declared_type(), "int");
		assert_eq!(error.current_frame().unwrap().local("x").unwrap().value(), &Value::Integer(0));
		assert!(error.current_frame().unwrap().local("missing").is_none());
	}

	#[test]
	fn parses_absolute_sqlite_connection_string() {
		let config = RuntimeDatabaseConfig::new()
			.with_database_connection_string("ExampleDb", "sqlite:///tmp/example.sqlite")
			.unwrap();

		assert_eq!(
			config.sqlite_database_path("ExampleDb"),
			Some(std::path::Path::new("/tmp/example.sqlite")),
		);
	}

	#[test]
	fn parses_relative_sqlite_connection_string() {
		let config = RuntimeDatabaseConfig::new()
			.with_database_connection_string("ExampleDb", "sqlite:db/example.sqlite")
			.unwrap();

		assert_eq!(
			config.sqlite_database_path("exampledb"),
			Some(std::path::Path::new("db/example.sqlite")),
		);
	}

	#[test]
	fn rejects_addition_without_enough_operands() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::Add,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 1,
			message: String::from("Stack underflow while reading operand."),
			source_location: None,
			stack_trace: vec![
				super::VmStackFrame {
					instruction_index: 1,
					locals: vec![],
					source_location: None,
				},
			],
		});
	}

	#[test]
	fn rejects_division_by_zero() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(0),
			Instruction::Divide,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 2,
			message: String::from("Division by zero."),
			source_location: None,
			stack_trace: vec![
				super::VmStackFrame {
					instruction_index: 2,
					locals: vec![],
					source_location: None,
				},
			],
		});
	}

	#[test]
	fn rejects_invalid_sqlite_connection_string_shape() {
		let error = RuntimeDatabaseConfig::new()
			.with_database_connection_string("ExampleDb", "sqlite://example.sqlite")
			.unwrap_err();

		assert_eq!(
			error,
			String::from(
				"SQLite connection string for database `ExampleDb` must use `sqlite:/path/to.db`, `sqlite:relative.db`, or `sqlite::memory:`."
			),
		);
	}

	#[test]
	fn rejects_logical_and_with_numeric_operand() {
		let program = Program::new(vec![
			Instruction::PushBoolean(true),
			Instruction::PushInteger(1),
			Instruction::And,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 2,
			message: String::from("Expected a Boolean operand."),
			source_location: None,
			stack_trace: vec![
				super::VmStackFrame {
					instruction_index: 2,
					locals: vec![],
					source_location: None,
				},
			],
		});
	}

	#[test]
	fn rejects_mixed_boolean_and_numeric_equality() {
		let program = Program::new(vec![
			Instruction::PushBoolean(true),
			Instruction::PushInteger(1),
			Instruction::Equal,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 2,
			message: String::from("Cannot compare `bool` and `int` for equality."),
			source_location: None,
			stack_trace: vec![
				super::VmStackFrame {
					instruction_index: 2,
					locals: vec![],
					source_location: None,
				},
			],
		});
	}

	#[test]
	fn rejects_mixed_text_and_numeric_equality() {
		let program = Program::new(vec![
			Instruction::PushText(String::from("1")),
			Instruction::PushInteger(1),
			Instruction::Equal,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 2,
			message: String::from("Cannot compare `text` and `int` for equality."),
			source_location: None,
			stack_trace: vec![
				super::VmStackFrame {
					instruction_index: 2,
					locals: vec![],
					source_location: None,
				},
			],
		});
	}

	#[test]
	fn rejects_modulo_by_zero() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(0),
			Instruction::Modulo,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 2,
			message: String::from("Modulo by zero."),
			source_location: None,
			stack_trace: vec![
				super::VmStackFrame {
					instruction_index: 2,
					locals: vec![],
					source_location: None,
				},
			],
		});
	}

	#[test]
	fn rejects_unsupported_connection_string_backend() {
		let error = RuntimeDatabaseConfig::new()
			.with_database_connection_string("ExampleDb", "postgresql:host=localhost")
			.unwrap_err();

		assert_eq!(
			error,
			String::from("Connection string for database `ExampleDb` uses unsupported backend `postgresql`."),
		);
	}

	#[test]
	fn rejects_zero_range_step() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(0),
			Instruction::PushInteger(5),
			Instruction::MakeSteppedRange,
		]);

		let error = VirtualMachine::new().run(&program).unwrap_err();

		assert_eq!(error, VmError {
			instruction_index: 3,
			message: String::from("Range step cannot be zero."),
			source_location: None,
			stack_trace: vec![
				super::VmStackFrame {
					instruction_index: 3,
					locals: vec![],
					source_location: None,
				},
			],
		});
	}

	#[test]
	fn runs_addition_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::Add,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_array_concatenation_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::MakeArray(2),
			Instruction::PushInteger(3),
			Instruction::PushInteger(4),
			Instruction::MakeArray(2),
			Instruction::Add,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(1),
			Value::Integer(2),
			Value::Integer(3),
			Value::Integer(4),
		])));
	}

	#[test]
	fn runs_array_index_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(10),
			Instruction::PushInteger(20),
			Instruction::MakeArray(2),
			Instruction::PushInteger(2),
			Instruction::LoadIndex,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(20)));
	}

	#[test]
	fn runs_array_slice_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(10),
			Instruction::PushInteger(20),
			Instruction::PushInteger(30),
			Instruction::PushInteger(40),
			Instruction::MakeArray(4),
			Instruction::PushInteger(2),
			Instruction::PushInteger(3),
			Instruction::MakeRange,
			Instruction::LoadIndex,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(20),
			Value::Integer(30),
		])));
	}

	#[test]
	fn runs_array_store_index_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(10),
			Instruction::PushInteger(20),
			Instruction::MakeArray(2),
			Instruction::PushInteger(2),
			Instruction::PushInteger(99),
			Instruction::StoreIndex,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(10),
			Value::Integer(99),
		])));
	}

	#[test]
	fn runs_boolean_literal_program() {
		let program = Program::new(vec![
			Instruction::PushBoolean(true),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_built_in_disp_program_without_leaving_stack_value() {
		let program = Program::new(vec![
			Instruction::PushText(String::from("hello")),
			Instruction::CallBuiltIn(BuiltInFunction::Disp, 1),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, None);
	}

	#[test]
	fn runs_built_in_len_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::PushInteger(3),
			Instruction::MakeArray(3),
			Instruction::CallBuiltIn(BuiltInFunction::Len, 1),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_compound_array_store_index_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(10),
			Instruction::PushInteger(20),
			Instruction::MakeArray(2),
			Instruction::PushInteger(2),
			Instruction::Dup2,
			Instruction::LoadIndex,
			Instruction::PushInteger(5),
			Instruction::Add,
			Instruction::StoreIndex,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Array(vec![
			Value::Integer(10),
			Value::Integer(25),
		])));
	}

	#[test]
	fn runs_decimal_range_program() {
		let program = Program::new(vec![
			Instruction::PushDecimal(Decimal::from_literal("0.0").unwrap()),
			Instruction::PushDecimal(Decimal::from_literal("0.1").unwrap()),
			Instruction::PushDecimal(Decimal::from_literal("0.3").unwrap()),
			Instruction::MakeSteppedRange,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::DecimalRange(DecimalRange {
			start: Decimal::from_literal("0.0").unwrap(),
			step: Some(Decimal::from_literal("0.1").unwrap()),
			end: Decimal::from_literal("0.3").unwrap(),
		})));
	}

	#[test]
	fn runs_equality_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(2),
			Instruction::PushDecimal(Decimal::from_literal("2.0").unwrap()),
			Instruction::Equal,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_decimal_literal_program() {
		let program = Program::new(vec![
			Instruction::PushDecimal(Decimal::from_literal("1.25").unwrap()),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Decimal(Decimal::from_literal("1.25").unwrap())));
	}

	#[test]
	fn runs_integer_literal_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(42),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(42)));
	}

	#[test]
	fn runs_jump_if_false_program() {
		let program = Program::new(vec![
			Instruction::PushBoolean(false),
			Instruction::JumpIfFalse(4),
			Instruction::PushInteger(1),
			Instruction::Jump(5),
			Instruction::PushInteger(2),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_jump_program() {
		let program = Program::new(vec![
			Instruction::Jump(2),
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(2)));
	}

	#[test]
	fn runs_local_store_and_load_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::StoreLocal(0),
			Instruction::LoadLocal(0),
			Instruction::PushInteger(2),
			Instruction::Add,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_logical_and_program() {
		let program = Program::new(vec![
			Instruction::PushBoolean(true),
			Instruction::PushBoolean(false),
			Instruction::And,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Boolean(false)));
	}

	#[test]
	fn runs_logical_not_program() {
		let program = Program::new(vec![
			Instruction::PushBoolean(false),
			Instruction::Not,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_logical_or_program() {
		let program = Program::new(vec![
			Instruction::PushBoolean(false),
			Instruction::PushBoolean(true),
			Instruction::Or,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_logical_xor_program() {
		let program = Program::new(vec![
			Instruction::PushBoolean(true),
			Instruction::PushBoolean(false),
			Instruction::Xor,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_mixed_decimal_and_larger_integer_multiplication() {
		let program = Program::new(vec![
			Instruction::PushDecimal(Decimal::from_literal("0.75").unwrap()),
			Instruction::PushInteger(20),
			Instruction::Multiply,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Decimal(Decimal::from_integer(15))));
	}

	#[test]
	fn runs_mixed_integer_and_decimal_addition() {
		let program = Program::new(vec![
			Instruction::PushInteger(2),
			Instruction::PushDecimal(Decimal::from_literal("1.25").unwrap()),
			Instruction::Add,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Decimal(Decimal::from_literal("3.25").unwrap())));
	}

	#[test]
	fn runs_other_arithmetic_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(20),
			Instruction::PushInteger(5),
			Instruction::Divide,
			Instruction::PushInteger(3),
			Instruction::Multiply,
			Instruction::PushInteger(1),
			Instruction::Subtract,
			Instruction::PushInteger(4),
			Instruction::Modulo,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(3)));
	}

	#[test]
	fn runs_relational_program() {
		let program = Program::new(vec![
			Instruction::PushInteger(1),
			Instruction::PushInteger(2),
			Instruction::LessThan,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_text_concatenation_program() {
		let program = Program::new(vec![
			Instruction::PushText(String::from("hello ")),
			Instruction::PushInteger(42),
			Instruction::Add,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello 42"))));
	}

	#[test]
	fn runs_text_literal_program() {
		let program = Program::new(vec![
			Instruction::PushText(String::from("hello")),
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Text(String::from("hello"))));
	}

	#[test]
	fn runs_text_relational_program() {
		let program = Program::new(vec![
			Instruction::PushText(String::from("apple")),
			Instruction::PushText(String::from("banana")),
			Instruction::LessThan,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Boolean(true)));
	}

	#[test]
	fn runs_unary_negation_for_decimal() {
		let program = Program::new(vec![
			Instruction::PushDecimal(Decimal::from_literal("1.25").unwrap()),
			Instruction::Negate,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Decimal(Decimal::from_literal("1.25").unwrap().negated())));
	}

	#[test]
	fn runs_unary_negation_for_integer() {
		let program = Program::new(vec![
			Instruction::PushInteger(42),
			Instruction::Negate,
		]);

		let result = VirtualMachine::new().run(&program).unwrap();

		assert_eq!(result, Some(Value::Integer(-42)));
	}
}
