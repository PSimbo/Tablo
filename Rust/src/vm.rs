use crate::builtins::BuiltInFunction;
use crate::bytecode::CodeBody;
use crate::bytecode::CompiledFunction;
use crate::bytecode::Instruction;
use crate::bytecode::Program;
use crate::bytecode::SourceLocation;
use crate::value::Decimal;
use crate::value::DecimalRange;
use crate::value::DecimalRangeIterator;
use crate::value::IntegerRange;
use crate::value::IntegerRangeIterator;
use crate::value::IteratorState;
use crate::value::Value;

#[derive(Clone, Copy)]
enum ComparisonKind {
	GreaterThan,
	GreaterThanOrEqual,
	LessThan,
	LessThanOrEqual,
}

struct CallFrame<'a> {
	code_body: &'a CodeBody,
	debug_body_index: usize,
	instruction_index: usize,
	locals: Vec<Value>,
}

enum ExecutionOutcome {
	Continue(Option<usize>),
	Return(Option<Value>),
}

#[derive(Default)]
pub struct VirtualMachine {
	stack: Vec<Value>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VmError {
	pub instruction_index: usize,
	pub message: String,
	pub source_location: Option<SourceLocation>,
}

impl VirtualMachine {
	pub fn new() -> Self {
		Self {
			stack: Vec::new(),
		}
	}

	pub fn run(&mut self, program: &Program) -> Result<Option<Value>, VmError> {
		self.stack.clear();
		self.run_code_body(program, &program.entry, program.functions().len(), Vec::new())
	}

	fn run_code_body(
		&mut self,
		program: &Program,
		code_body: &CodeBody,
		debug_body_index: usize,
		locals: Vec<Value>
	) -> Result<Option<Value>, VmError> {
		let base_stack_len = self.stack.len();
		let mut frame = CallFrame::new(code_body, debug_body_index, locals);

		while frame.instruction_index < frame.code_body.instructions.len() {
			let outcome = self.execute_instruction(
				program,
				&frame.code_body.instructions[frame.instruction_index],
				frame.instruction_index,
				&mut frame.locals,
			).map_err(|error| enrich_vm_error(program, frame.debug_body_index, error))?;

			match outcome {
				ExecutionOutcome::Continue(next_instruction_index) => {
					frame.instruction_index = next_instruction_index.unwrap_or(frame.instruction_index + 1);
				}
				ExecutionOutcome::Return(result) => {
					self.stack.truncate(base_stack_len);
					return Ok(result);
				}
			}
		}

		let result = self.stack.pop();
		self.stack.truncate(base_stack_len);
		Ok(result)
	}

	fn execute_instruction(
		&mut self,
		program: &Program,
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
				let function = program.functions().get(*function_index as usize).ok_or(VmError {
					instruction_index,
					message: format!("Function index {} does not exist.", function_index),
					source_location: None,
				})?;
				let arguments = self.pop_call_arguments(*argument_count as usize, instruction_index)?;
				let result = self.run_function(program, function, *function_index as usize, arguments, instruction_index)?;

				if let Some(result) = result {
					self.stack.push(result);
				}

				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::CallBuiltIn(built_in, argument_count) => {
				let arguments = self.pop_call_arguments(*argument_count as usize, instruction_index)?;
				let result = self.run_built_in_function(*built_in, arguments, instruction_index)?;
				self.stack.push(result);
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::Dup2 => {
				if self.stack.len() < 2 {
					return Err(VmError {
						instruction_index,
						message: String::from("Stack underflow while duplicating operands."),
						source_location: None,
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
				let value = self.pop_boolean(instruction_index)?;

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
				})?;
				self.stack.push(value);
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
				let value = self.pop_boolean(instruction_index)?;
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
			Instruction::PushDecimal(value) => {
				self.stack.push(Value::Decimal(value.clone()));
				Ok(ExecutionOutcome::Continue(None))
			}
			Instruction::PushInteger(value) => {
				self.stack.push(Value::Integer(*value));
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

				locals[slot] = value;
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

	fn pop_boolean(&mut self, instruction_index: usize) -> Result<bool, VmError> {
		let value = self.stack.pop().ok_or(VmError {
			instruction_index,
			message: String::from("Stack underflow while reading Boolean operand."),
			source_location: None,
		})?;

		match value {
			Value::Boolean(value) => Ok(value),
			_ => Err(VmError {
				instruction_index,
				message: String::from("Expected a Boolean operand."),
				source_location: None,
			}),
		}
	}

	fn pop_call_arguments(&mut self, argument_count: usize, instruction_index: usize) -> Result<Vec<Value>, VmError> {
		let mut arguments = Vec::with_capacity(argument_count);

		for _ in 0..argument_count {
			arguments.push(self.pop_value(instruction_index)?);
		}

		arguments.reverse();
		Ok(arguments)
	}

	fn pop_numeric(&mut self, instruction_index: usize) -> Result<Value, VmError> {
		let value = self.stack.pop().ok_or(VmError {
			instruction_index,
			message: String::from("Stack underflow while reading numeric operand."),
			source_location: None,
		})?;

		if matches!(value, Value::Array(_) | Value::Boolean(_) | Value::DecimalRange(_) | Value::IntegerRange(_) | Value::Iterator(_) | Value::Text(_)) {
			return Err(VmError {
				instruction_index,
				message: format!("Expected a numeric operand, found a {} value.", type_name(&value)),
				source_location: None,
			});
		}

		Ok(value)
	}

	fn pop_value(&mut self, instruction_index: usize) -> Result<Value, VmError> {
		self.stack.pop().ok_or(VmError {
			instruction_index,
			message: String::from("Stack underflow while reading operand."),
			source_location: None,
		})
	}

	fn run_built_in_function(
		&mut self,
		built_in: BuiltInFunction,
		arguments: Vec<Value>,
		instruction_index: usize,
	) -> Result<Value, VmError> {
		match built_in {
			BuiltInFunction::Len => match arguments.as_slice() {
				[Value::Array(values)] => Ok(Value::Integer(values.len() as i64)),
				[value] => Err(vm_error(
					instruction_index,
					format!("Built-in function `len` does not accept a `{}` value.", type_name(value)),
				)),
				_ => Err(vm_error(
					instruction_index,
					format!("Built-in function `len` expects 1 argument(s), found {}.", arguments.len()),
				)),
			}
		}
	}

	fn run_function(
		&mut self,
		program: &Program,
		function: &CompiledFunction,
		function_index: usize,
		arguments: Vec<Value>,
		_: usize
	) -> Result<Option<Value>, VmError> {
		self.run_code_body(program, function.body(), function_index, arguments)
	}
}

impl<'a> CallFrame<'a> {
	fn new(code_body: &'a CodeBody, debug_body_index: usize, locals: Vec<Value>) -> Self {
		Self {
			code_body,
			debug_body_index,
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
			let rhs = Decimal::from_integer(rhs)
				.to_scale_with_precision(lhs.precision, lhs.scale)
				.map_err(|message| vm_error(instruction_index, message))?;

			Ok((lhs, rhs))
		}
		(Value::Integer(lhs), Value::Decimal(rhs)) => {
			let lhs = Decimal::from_integer(lhs)
				.to_scale_with_precision(rhs.precision, rhs.scale)
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

	if matches!(lhs, Value::Text(_)) || matches!(rhs, Value::Text(_)) {
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

fn enrich_vm_error(program: &Program, debug_body_index: usize, mut error: VmError) -> VmError {
	error.source_location = program.debug_location(debug_body_index, error.instruction_index);
	error
}

fn equals_value(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	let value = match (lhs, rhs) {
		(Value::Array(lhs), Value::Array(rhs)) => lhs == rhs,
		(Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
		(Value::Text(lhs), Value::Text(rhs)) => lhs == rhs,
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

fn load_index_value(array: Value, index: Value, instruction_index: usize) -> Result<Value, VmError> {
	let index = match index {
		Value::Integer(value) => value,
		other => {
			return Err(vm_error(
				instruction_index,
				format!("Array index must be an `int`, found `{}`.", type_name(&other)),
			));
		}
	};

	let values = match array {
		Value::Array(values) => values,
		other => {
			return Err(vm_error(
				instruction_index,
				format!("Cannot index a `{}` value.", type_name(&other)),
			));
		}
	};

	if index < 1 || index as usize > values.len() {
		return Err(vm_error(
			instruction_index,
			format!("Array index {} is out of bounds for length {}.", index, values.len()),
		));
	}

	Ok(values[index as usize - 1].clone())
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
		Value::Decimal(mut decimal) => {
			decimal.coefficient = decimal.coefficient.saturating_neg();
			Value::Decimal(decimal)
		}
		Value::DecimalRange(_) => unreachable!("Range values are rejected before numeric negation."),
		Value::Integer(integer) => Value::Integer(integer.saturating_neg()),
		Value::IntegerRange(_) => unreachable!("Range values are rejected before numeric negation."),
		Value::Iterator(_) => unreachable!("Iterator values are rejected before numeric negation."),
		Value::Text(_) => unreachable!("Text values are rejected before numeric negation."),
	}
}

fn not_equals_value(lhs: Value, rhs: Value, instruction_index: usize) -> Result<Value, VmError> {
	match equals_value(lhs, rhs, instruction_index)? {
		Value::Boolean(value) => Ok(Value::Boolean(!value)),
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
		Value::Decimal(value) => value.to_string(),
		Value::DecimalRange(value) => value.to_string(),
		Value::Integer(value) => value.to_string(),
		Value::IntegerRange(value) => value.to_string(),
		Value::Iterator(_) => String::from("<iterator>"),
		Value::Text(value) => value.clone(),
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
		Value::Decimal(_) => "dec",
		Value::DecimalRange(_) => "range",
		Value::Integer(_) => "int",
		Value::IntegerRange(_) => "range",
		Value::Iterator(_) => "iterator",
		Value::Text(_) => "text",
	}
}

fn vm_error(instruction_index: usize, message: String) -> VmError {
	VmError {
		instruction_index,
		message,
		source_location: None,
	}
}

#[cfg(test)]
mod tests {
	use crate::builtins::BuiltInFunction;
	use crate::bytecode::Instruction;
	use crate::bytecode::Program;
	use crate::value::Decimal;
	use crate::value::DecimalRange;
	use crate::value::Value;

	use super::VirtualMachine;
	use super::VmError;

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
		});
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
		});
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
