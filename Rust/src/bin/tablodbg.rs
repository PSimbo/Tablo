use std::collections::BTreeMap;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::path::PathBuf;

use clap::Parser as ClapParser;
use colored::Colorize;
use serde::Deserialize;
use serde_json::Value as JsonValue;
use serde_json::json;
use tablo::TabloError;
use tablo::bytecode::Program;
use tablo::bytecode::SourceLocation;
use tablo::debugger::DebuggerSession;
use tablo::debugger::DebuggerStop;
use tablo::debugger::InstructionBreakpoint;
use tablo::debugger::PauseReason;
use tablo::object_file::read_program_from_path;
use tablo::runtime_config::read_runtime_database_config_from_path;
use tablo::value::RecordFieldValue;
use tablo::value::Value;
use tablo::value::sqlite_record_field_runtime_value;
use tablo::vm::VmError;
use tablo::vm::VmStackFrame;

#[derive(ClapParser, Debug)]
#[command(name = "tablodbg")]
#[command(about = "Prototype debugger for compiled Tablo object files.")]
#[command(after_help = "Interactive mode:\n  tablodbg program.tbo\n\nDAP mode:\n  tablodbg --dap\n\nInteractive commands:\n  continue | c               Resume execution\n  step-in | s | si           Step into the next instruction\n  step-over | n              Step over the current call or instruction\n  step-out | finish | fin | o Run until the current frame returns\n  break <SOURCE:LINE> | b    Add a breakpoint after launch\n  breakpoints | bl           List the current breakpoints\n  clear-breakpoints | bc     Remove all breakpoints\n  stack | bt | where         Show the current stack trace\n  locals | l                 Show locals in the current frame\n  help | h | ?               Show debugger command help\n  quit | q | exit            Exit the debugger")]
struct Args {
	#[arg(value_name = "INPUT")]
	input_path: Option<PathBuf>,
	#[arg(short = 'b', long = "breakpoint", value_name = "SOURCE:LINE")]
	breakpoints: Vec<String>,
	#[arg(long = "dap", help = "Run as a DAP server over stdin/stdout.")]
	dap: bool,
}

enum DebugCommand {
	AddBreakpoint(String),
	Breakpoints,
	ClearBreakpoints,
	Continue,
	Help,
	Locals,
	Quit,
	Stack,
	StepIn,
	StepOut,
	StepOver,
}

#[derive(Clone)]
enum VariableContainer {
	Locals(usize),
	Value(Value),
}

#[derive(Deserialize)]
struct DapRequest {
	arguments: Option<JsonValue>,
	command: String,
	seq: i64,
	#[serde(rename = "type")]
	type_name: String,
}

struct DapServer {
	breakpoints_by_source: BTreeMap<String, Vec<InstructionBreakpoint>>,
	next_variables_reference: i64,
	program: Option<&'static Program>,
	runtime_error: Option<VmError>,
	session: Option<DebuggerSession<'static>>,
	stop: Option<DebuggerStop>,
	variable_containers: BTreeMap<i64, VariableContainer>,
}

impl DapServer {
	fn new() -> Self {
		Self {
			breakpoints_by_source: BTreeMap::new(),
			next_variables_reference: 1,
			program: None,
			runtime_error: None,
			session: None,
			stop: None,
			variable_containers: BTreeMap::new(),
		}
	}

	fn current_paused_state(&self) -> Option<&tablo::debugger::PausedState> {
		self.stop.as_ref()?.paused_state()
	}

	fn current_stack_frames(&self) -> Option<&[VmStackFrame]> {
		if let Some(paused) = self.current_paused_state() {
			return Some(paused.stack_frames());
		}

		self.runtime_error.as_ref().map(|error| error.stack_frames())
	}

	fn current_exception_message(&self) -> Option<&str> {
		self.runtime_error.as_ref().map(|error| error.message.as_str())
	}

	fn handle_configuration_done(&mut self, request_seq: i64) -> Result<Vec<JsonValue>, String> {
		let all_breakpoints = self.all_breakpoints();
		let session = self.session.as_mut().ok_or(String::from("No program is currently loaded."))?;
		let stop = if all_breakpoints.is_empty() {
			session.pause_at_start()
		}
		else {
			session.resume().map_err(|error| TabloError::Runtime(error).to_string())?
		};

		self.stop = Some(stop.clone());
		self.runtime_error = None;
		self.reset_variable_containers();

		let mut messages = vec![dap_response(request_seq, "configurationDone", json!({}))];
		messages.extend(self.messages_for_stop(&stop, Some("entry")));
		Ok(messages)
	}

	fn handle_continue(&mut self, request_seq: i64) -> Result<Vec<JsonValue>, String> {
		if self.runtime_error.is_some() {
			return Ok(vec![
				dap_response(request_seq, "continue", json!({ "allThreadsContinued": true })),
				dap_event("terminated", json!({})),
			]);
		}

		let session = self.session.as_mut().ok_or(String::from("No program is currently loaded."))?;
		let stop = match session.resume() {
			Ok(stop) => {
				self.runtime_error = None;
				stop
			}
			Err(error) => {
				self.stop = None;
				self.runtime_error = Some(error.clone());
				self.reset_variable_containers();
				let mut messages = vec![dap_response(
					request_seq,
					"continue",
					json!({ "allThreadsContinued": true }),
				)];
				messages.extend(self.messages_for_runtime_error(&error));
				return Ok(messages);
			}
		};
		self.stop = Some(stop.clone());
		self.reset_variable_containers();

		let mut messages = vec![dap_response(
			request_seq,
			"continue",
			json!({ "allThreadsContinued": true }),
		)];
		messages.extend(self.messages_for_stop(&stop, None));
		Ok(messages)
	}

	fn handle_disconnect(&mut self, request_seq: i64) -> Vec<JsonValue> {
		vec![
			dap_response(request_seq, "disconnect", json!({})),
			dap_event("terminated", json!({})),
		]
	}

	fn handle_initialize(&mut self, request_seq: i64) -> Vec<JsonValue> {
		vec![
			dap_response(
				request_seq,
				"initialize",
				json!({
					"supportsConfigurationDoneRequest": true,
					"supportsStepInTargetsRequest": false,
					"supportsTerminateRequest": false,
					"supportsConditionalBreakpoints": false,
					"supportsExceptionInfoRequest": true,
					"supportsFunctionBreakpoints": false,
					"supportsInstructionBreakpoints": false,
				}),
			),
			dap_event("initialized", json!({})),
		]
	}

	fn handle_launch(&mut self, request_seq: i64, arguments: Option<JsonValue>) -> Result<Vec<JsonValue>, String> {
		let arguments = arguments.unwrap_or(JsonValue::Null);
		let path = arguments.get("program")
			.and_then(JsonValue::as_str)
			.or_else(|| arguments.get("inputPath").and_then(JsonValue::as_str))
			.ok_or(String::from("Launch request must include a `program` path."))?;
		let config_path = arguments.get("projectConfigPath")
			.and_then(JsonValue::as_str)
			.or_else(|| arguments.get("configPath").and_then(JsonValue::as_str));

		let program = read_program_from_path(path).map_err(|error| TabloError::ObjectFile(error).to_string())?;
		let program = Box::leak(Box::new(program));
		let mut session = if let Some(config_path) = config_path {
			let database_config = read_runtime_database_config_from_path(config_path)
				.map_err(|error| format!("Failed to load runtime config `{config_path}`: {}", error.message))?;
			DebuggerSession::with_database_config(program, database_config)
		}
		else {
			DebuggerSession::new(program)
		};

		if !self.all_breakpoints().is_empty() {
			session.set_breakpoints(self.all_breakpoints());
		}

		self.program = Some(program);
		self.runtime_error = None;
		self.session = Some(session);
		self.stop = None;
		self.reset_variable_containers();

		Ok(vec![dap_response(request_seq, "launch", json!({}))])
	}

	fn handle_scopes(&mut self, request_seq: i64, arguments: Option<JsonValue>) -> Result<Vec<JsonValue>, String> {
		let frame_id = frame_id_from_arguments(arguments)?;
		let stack_frames = self.current_stack_frames().ok_or(String::from("Program is not currently paused."))?;

		if frame_id == 0 || frame_id > stack_frames.len() as i64 {
			return Err(format!("Unknown frame id {frame_id}."));
		}

		let locals_reference = self.register_container(VariableContainer::Locals((frame_id - 1) as usize));

		Ok(vec![dap_response(
			request_seq,
			"scopes",
			json!({
				"scopes": [
					{
						"name": "Locals",
						"presentationHint": "locals",
						"variablesReference": locals_reference,
						"expensive": false,
					}
				]
			}),
		)])
	}

	fn handle_set_breakpoints(
		&mut self,
		request_seq: i64,
		arguments: Option<JsonValue>,
	) -> Result<Vec<JsonValue>, String> {
		let program = self.program.ok_or(String::from("No program is currently loaded."))?;
		let session = self.session.as_ref().ok_or(String::from("No program is currently loaded."))?;
		let arguments = arguments.unwrap_or(JsonValue::Null);
		let source = arguments.get("source").cloned().unwrap_or(JsonValue::Null);
		let source_path = source.get("path").and_then(JsonValue::as_str);
		let source_name = source.get("name").and_then(JsonValue::as_str);
		let source_key = source_path.or(source_name).ok_or(String::from("Breakpoint source must include `path` or `name`."))?;

		let requested_lines = if let Some(breakpoints) = arguments.get("breakpoints").and_then(JsonValue::as_array) {
			breakpoints.iter().filter_map(|breakpoint| breakpoint.get("line").and_then(JsonValue::as_u64).map(|line| line as u32)).collect::<Vec<_>>()
		}
		else {
			arguments.get("lines")
				.and_then(JsonValue::as_array)
				.map(|lines| lines.iter().filter_map(|line| line.as_u64().map(|line| line as u32)).collect::<Vec<_>>())
				.unwrap_or_default()
		};

		let resolved = session.resolve_source_breakpoints(source_key, &requested_lines);
		self.breakpoints_by_source.insert(source_key.to_string(), resolved.clone());
		let all_breakpoints = self.all_breakpoints();
		let session = self.session.as_mut().ok_or(String::from("No program is currently loaded."))?;
		session.set_breakpoints(all_breakpoints);

		let breakpoint_results = requested_lines.into_iter().map(|line| {
			let resolved_site = resolved.iter().find(|breakpoint| {
				program
					.debug_location(breakpoint.body_index(), breakpoint.instruction_index())
					.is_some_and(|location| location.line() == line)
			});

			match resolved_site {
				Some(breakpoint) => {
					let location = program.debug_location(breakpoint.body_index(), breakpoint.instruction_index());
					json!({
						"verified": true,
						"line": location.as_ref().map(|location| location.line()).unwrap_or(line),
						"column": location.as_ref().map(|location| location.column()).unwrap_or(1),
					})
				}
				None => json!({
					"verified": false,
					"line": line,
				}),
			}
		}).collect::<Vec<_>>();

		Ok(vec![dap_response(
			request_seq,
			"setBreakpoints",
			json!({ "breakpoints": breakpoint_results }),
		)])
	}

	fn handle_stack_trace(&self, request_seq: i64) -> Result<Vec<JsonValue>, String> {
		let stack_frames = self.current_stack_frames().ok_or(String::from("Program is not currently paused."))?;
		let stack_frames = stack_frames.iter().enumerate().map(|(index, frame)| {
			let source = frame.source_location().map(|location| {
				json!({
					"name": location.display_name().unwrap_or("<source>"),
					"path": location.display_name(),
				})
			});

			json!({
				"id": (index + 1) as i64,
				"name": frame.source_location()
					.and_then(|location| location.body_name())
					.unwrap_or("<entry>"),
				"line": frame.source_location().map(|location| location.line()).unwrap_or(1),
				"column": frame.source_location().map(|location| location.column()).unwrap_or(1),
				"source": source,
			})
		}).collect::<Vec<_>>();

		Ok(vec![dap_response(
			request_seq,
			"stackTrace",
			json!({
				"stackFrames": stack_frames,
				"totalFrames": self.current_stack_frames().map(|frames| frames.len()).unwrap_or(0),
			}),
		)])
	}

	fn handle_exception_info(&self, request_seq: i64) -> Result<Vec<JsonValue>, String> {
		let description = self.current_exception_message().ok_or(String::from("Program is not currently paused on an exception."))?;

		Ok(vec![dap_response(
			request_seq,
			"exceptionInfo",
			json!({
				"exceptionId": "runtime",
				"description": description,
				"breakMode": "always",
			}),
		)])
	}

	fn handle_step_in(&mut self, request_seq: i64) -> Result<Vec<JsonValue>, String> {
		if self.runtime_error.is_some() {
			return Err(String::from("Program is paused on an exception and cannot step further."));
		}

		let session = self.session.as_mut().ok_or(String::from("No program is currently loaded."))?;
		let stop = match session.step_in() {
			Ok(stop) => {
				self.runtime_error = None;
				stop
			}
			Err(error) => {
				self.stop = None;
				self.runtime_error = Some(error.clone());
				self.reset_variable_containers();
				let mut messages = vec![dap_response(request_seq, "stepIn", json!({}))];
				messages.extend(self.messages_for_runtime_error(&error));
				return Ok(messages);
			}
		};
		self.stop = Some(stop.clone());
		self.reset_variable_containers();

		let mut messages = vec![dap_response(request_seq, "stepIn", json!({}))];
		messages.extend(self.messages_for_stop(&stop, None));
		Ok(messages)
	}

	fn handle_step_out(&mut self, request_seq: i64) -> Result<Vec<JsonValue>, String> {
		if self.runtime_error.is_some() {
			return Err(String::from("Program is paused on an exception and cannot step further."));
		}

		let session = self.session.as_mut().ok_or(String::from("No program is currently loaded."))?;
		let stop = match session.step_out() {
			Ok(stop) => {
				self.runtime_error = None;
				stop
			}
			Err(error) => {
				self.stop = None;
				self.runtime_error = Some(error.clone());
				self.reset_variable_containers();
				let mut messages = vec![dap_response(request_seq, "stepOut", json!({}))];
				messages.extend(self.messages_for_runtime_error(&error));
				return Ok(messages);
			}
		};
		self.stop = Some(stop.clone());
		self.reset_variable_containers();

		let mut messages = vec![dap_response(request_seq, "stepOut", json!({}))];
		messages.extend(self.messages_for_stop(&stop, None));
		Ok(messages)
	}

	fn handle_step_over(&mut self, request_seq: i64) -> Result<Vec<JsonValue>, String> {
		if self.runtime_error.is_some() {
			return Err(String::from("Program is paused on an exception and cannot step further."));
		}

		let session = self.session.as_mut().ok_or(String::from("No program is currently loaded."))?;
		let stop = match session.step_over() {
			Ok(stop) => {
				self.runtime_error = None;
				stop
			}
			Err(error) => {
				self.stop = None;
				self.runtime_error = Some(error.clone());
				self.reset_variable_containers();
				let mut messages = vec![dap_response(request_seq, "next", json!({}))];
				messages.extend(self.messages_for_runtime_error(&error));
				return Ok(messages);
			}
		};
		self.stop = Some(stop.clone());
		self.reset_variable_containers();

		let mut messages = vec![dap_response(request_seq, "next", json!({}))];
		messages.extend(self.messages_for_stop(&stop, None));
		Ok(messages)
	}

	fn handle_threads(&self, request_seq: i64) -> Vec<JsonValue> {
		vec![dap_response(
			request_seq,
			"threads",
			json!({
				"threads": [
					{ "id": 1, "name": "Main" }
				]
			}),
		)]
	}

	fn handle_variables(&mut self, request_seq: i64, arguments: Option<JsonValue>) -> Result<Vec<JsonValue>, String> {
		let stack_frames = self.current_stack_frames().ok_or(String::from("Program is not currently paused."))?;
		let variables_reference = arguments
			.unwrap_or(JsonValue::Null)
			.get("variablesReference")
			.and_then(JsonValue::as_i64)
			.ok_or(String::from("Variables request must include `variablesReference`."))?;

		let Some(container) = self.variable_containers.get(&variables_reference).cloned() else {
			return Err(format!("Unknown variables reference {variables_reference}."));
		};

		let variables = match container {
			VariableContainer::Locals(frame_index) => {
				let frame = stack_frames.get(frame_index).ok_or_else(|| format!("Unknown frame id {}.", frame_index + 1))?;
				let locals = frame.locals().iter().map(|local| {
					(
						local.name().to_string(),
						local.declared_type().to_string(),
						local.value().clone(),
					)
				}).collect::<Vec<_>>();
				let mut variables = Vec::new();

				for (name, declared_type, value) in locals {
					let child_reference = self.variables_reference_for_value(value.clone());
					variables.push(json!({
						"name": name,
						"value": value.to_string(),
						"type": declared_type,
						"variablesReference": child_reference,
					}));
				}

				variables
			}
			VariableContainer::Value(value) => self.child_variables_for_value(&value)?,
		};

		Ok(vec![dap_response(
			request_seq,
			"variables",
			json!({ "variables": variables }),
		)])
	}

	fn messages_for_stop(&self, stop: &DebuggerStop, reason_override: Option<&str>) -> Vec<JsonValue> {
		match stop {
			DebuggerStop::Completed(result) => {
				let mut messages = Vec::new();
				if let Some(result) = result {
					messages.push(dap_event("output", json!({
						"category": "stdout",
						"output": format!("Program completed with result: {result}\n"),
					})));
				}
				messages.push(dap_event("terminated", json!({})));
				messages
			}
			DebuggerStop::Paused(paused) => {
				let reason = reason_override.unwrap_or(match paused.reason() {
					PauseReason::Breakpoint => "breakpoint",
					PauseReason::StepIn | PauseReason::StepOut | PauseReason::StepOver => "step",
				});

				vec![dap_event("stopped", json!({
					"reason": reason,
					"threadId": 1,
					"allThreadsStopped": true,
				}))]
			}
		}
	}

	fn messages_for_runtime_error(&self, error: &VmError) -> Vec<JsonValue> {
		vec![
			dap_event("stopped", json!({
				"reason": "exception",
				"text": error.message,
				"description": error.message,
				"threadId": 1,
				"allThreadsStopped": true,
			})),
		]
	}

	fn all_breakpoints(&self) -> Vec<InstructionBreakpoint> {
		self.breakpoints_by_source
			.values()
			.flat_map(|breakpoints| breakpoints.iter().copied())
			.collect()
	}

	fn child_variables_for_value(&mut self, value: &Value) -> Result<Vec<JsonValue>, String> {
		match value {
			Value::Array(values) => Ok(values.iter().enumerate().map(|(index, value)| {
				let child_reference = self.variables_reference_for_value(value.clone());
				json!({
					"name": format!("[{}]", index + 1),
					"value": value.to_string(),
					"type": variable_type_name(value),
					"variablesReference": child_reference,
				})
			}).collect()),
			Value::Null => Ok(Vec::new()),
			Value::Object(fields) => Ok(fields.iter().map(|(name, value)| {
				let child_reference = self.variables_reference_for_value(value.clone());
				json!({
					"name": name,
					"value": value.to_string(),
					"type": variable_type_name(value),
					"variablesReference": child_reference,
				})
			}).collect()),
			Value::RecordPointer(record) => {
				if !record.exists || record.locked {
					return Ok(Vec::new());
				}

				let mut variables = Vec::new();

				for (name, field) in &record.fields {
					let value = debugger_value_for_record_field(field)?;
					let child_reference = self.variables_reference_for_value(value.clone());
					variables.push(json!({
						"name": name,
						"value": value.to_string(),
						"type": variable_type_name(&value),
						"variablesReference": child_reference,
					}));
				}

				Ok(variables)
			}
			_ => Ok(Vec::new()),
		}
	}

	fn register_container(&mut self, container: VariableContainer) -> i64 {
		let variables_reference = self.next_variables_reference;
		self.next_variables_reference += 1;
		self.variable_containers.insert(variables_reference, container);
		variables_reference
	}

	fn reset_variable_containers(&mut self) {
		self.next_variables_reference = 1;
		self.variable_containers.clear();
	}

	fn variables_reference_for_value(&mut self, value: Value) -> i64 {
		match value {
			Value::Array(_) | Value::Object(_) | Value::RecordPointer(_) => {
				self.register_container(VariableContainer::Value(value))
			}
			_ => 0,
		}
	}
}

fn debugger_value_for_record_field(field: &RecordFieldValue) -> Result<Value, String> {
	match field {
		RecordFieldValue::Materialized(value) => Ok(value.clone()),
		RecordFieldValue::DeferredSqlite {
			data_type,
			is_nullable,
			value,
		} => sqlite_record_field_runtime_value(value, data_type, *is_nullable),
	}
}

fn variable_type_name(value: &Value) -> &'static str {
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
	}
}

fn main() {
	let args = Args::parse();

	if args.dap {
		if let Err(message) = run_dap_server() {
			eprintln!("{message}");
			std::process::exit(1);
		}
		return;
	}

	let Some(input_path) = args.input_path else {
		eprintln!("Interactive mode requires an input object file path. Use `tablodbg --dap` for DAP mode.");
		std::process::exit(1);
	};

	let program = match read_program_from_path(&input_path) {
		Ok(program) => program,
		Err(error) => {
			eprintln!("{}", TabloError::ObjectFile(error));
			std::process::exit(1);
		}
	};

	let mut session = DebuggerSession::new(&program);

	match resolve_breakpoints(&program, &session, &args.breakpoints) {
		Ok(breakpoints) => session.set_breakpoints(breakpoints),
		Err(message) => {
			eprintln!("{message}");
			std::process::exit(1);
		}
	}

	let initial_stop = if args.breakpoints.is_empty() {
		session.pause_at_start()
	}
	else {
		match session.resume() {
			Ok(stop) => stop,
			Err(error) => {
				eprintln!("{}", TabloError::Runtime(error));
				std::process::exit(1);
			}
		}
	};

	run_debug_loop(&program, &mut session, initial_stop);
}

fn dap_error_response(request: &DapRequest, message: impl Into<String>) -> JsonValue {
	json!({
		"type": "response",
		"request_seq": request.seq,
		"success": false,
		"command": request.command,
		"message": message.into(),
	})
}

fn dap_event(event: &str, body: JsonValue) -> JsonValue {
	json!({
		"type": "event",
		"event": event,
		"body": body,
	})
}

fn dap_response(request_seq: i64, command: &str, body: JsonValue) -> JsonValue {
	json!({
		"type": "response",
		"request_seq": request_seq,
		"success": true,
		"command": command,
		"body": body,
	})
}

fn emit_dap_message(writer: &mut impl Write, message: &JsonValue) -> Result<(), String> {
	let payload = serde_json::to_vec(message).map_err(|error| format!("Failed to encode DAP message: {error}"))?;
	write!(writer, "Content-Length: {}\r\n\r\n", payload.len())
		.map_err(|error| format!("Failed to write DAP header: {error}"))?;
	writer.write_all(&payload).map_err(|error| format!("Failed to write DAP payload: {error}"))?;
	writer.flush().map_err(|error| format!("Failed to flush DAP output: {error}"))?;
	Ok(())
}

fn emit_dap_messages(writer: &mut impl Write, messages: &[JsonValue]) -> Result<(), String> {
	for message in messages {
		emit_dap_message(writer, message)?;
	}

	Ok(())
}

fn frame_id_from_arguments(arguments: Option<JsonValue>) -> Result<i64, String> {
	arguments
		.unwrap_or(JsonValue::Null)
		.get("frameId")
		.and_then(JsonValue::as_i64)
		.ok_or(String::from("Request must include `frameId`."))
}

fn format_location(location: &SourceLocation) -> String {
	let position = match location.display_name() {
		Some(display_name) => format!("{display_name}:{}:{}", location.line(), location.column()),
		None => format!("line {}, column {}", location.line(), location.column()),
	};

	match location.body_name() {
		Some(body_name) => format!("{body_name} ({position})"),
		None => position,
	}
}

fn parse_breakpoint_spec(spec: &str) -> Result<(&str, u32), String> {
	let Some((source_name, line)) = spec.rsplit_once(':') else {
		return Err(format!(
			"Invalid breakpoint `{spec}`. Expected the form `SOURCE:LINE`, for example `example.tablo:12`."
		));
	};

	let line = line.parse::<u32>().map_err(|_| {
		format!("Invalid breakpoint `{spec}`. The line number must be a positive integer.")
	})?;

	if line == 0 {
		return Err(format!("Invalid breakpoint `{spec}`. Line numbers are 1-based."));
	}

	Ok((source_name, line))
}

fn parse_command(input: &str) -> Result<DebugCommand, String> {
	match input.trim() {
		"" | "n" | "next" | "step-over" => Ok(DebugCommand::StepOver),
		"?" | "h" | "help" => Ok(DebugCommand::Help),
		"bc" | "clear-breakpoints" => Ok(DebugCommand::ClearBreakpoints),
		"bl" | "breakpoints" => Ok(DebugCommand::Breakpoints),
		"c" | "continue" => Ok(DebugCommand::Continue),
		"l" | "locals" => Ok(DebugCommand::Locals),
		"o" | "finish" | "fin" | "so" | "step-out" => Ok(DebugCommand::StepOut),
		"q" | "quit" | "exit" => Ok(DebugCommand::Quit),
		"s" | "si" | "step" | "step-in" => Ok(DebugCommand::StepIn),
		"bt" | "stack" | "where" => Ok(DebugCommand::Stack),
		command if command.starts_with("b ") || command.starts_with("break ") => {
			let spec = command.split_once(' ').map(|(_, spec)| spec.trim()).unwrap_or_default();
			if spec.is_empty() {
				Err(String::from("Breakpoint command requires a `SOURCE:LINE` argument, for example `b example.tablo:12`."))
			}
			else {
				Ok(DebugCommand::AddBreakpoint(spec.to_string()))
			}
		}
		command => Err(format!("Unknown command `{command}`. Type `help` to see the available commands.")),
	}
}

fn print_completion(stop: &DebuggerStop) {
	if let Some(result) = stop.completed_result() {
		match result {
			Some(result) => println!("{} {result}", "Program completed with result:".green()),
			None => println!("{}", "Program completed.".green()),
		}
	}
}

fn print_current_frame(frame: &VmStackFrame) {
	match frame.source_location() {
		Some(location) => println!("{} {}", "Paused at".yellow(), format_location(location).bold()),
		None => println!("{} instruction {}", "Paused at".yellow(), frame.instruction_index()),
	}
}

fn print_help() {
	println!("{}", "Commands:".bold());
	println!("  break <SOURCE:LINE> / b    Add a breakpoint, e.g. `b example.tablo:12`");
	println!("  breakpoints / bl           List the current breakpoints");
	println!("  clear-breakpoints / bc     Remove all breakpoints");
	println!("  continue / c               Resume execution");
	println!("  step-in / s / si           Step into the next instruction");
	println!("  step-over / n              Step over the current call or instruction");
	println!("  step-out / finish / fin / o Run until the current frame returns");
	println!("  stack / bt                 Show the current stack trace");
	println!("  locals / l                 Show locals in the current frame");
	println!("  help / h / ?               Show this help");
	println!("  quit / q                   Exit the debugger");
}

fn print_locals(frame: &VmStackFrame) {
	if frame.locals().is_empty() {
		println!("{}", "No visible locals in the current frame.".dimmed());
		return;
	}

	println!("{}", "Locals:".bold());

	for local in frame.locals() {
		let mutability = if local.is_const() { "const" } else { "var" };
		println!(
			"  {} {}: {} = {}",
			mutability.dimmed(),
			local.name().bold(),
			local.declared_type().cyan(),
			local.value()
		);
	}
}

fn print_stack(paused: &tablo::debugger::PausedState) {
	println!("{}", "Stack trace:".bold());

	for frame in paused.stack_frames() {
		match frame.source_location() {
			Some(location) => println!("  at {}", format_location(location)),
			None => println!("  at instruction {}", frame.instruction_index()),
		}
	}
}

fn print_breakpoints(session: &DebuggerSession<'_>, program: &Program) {
	let breakpoints = session.breakpoints();

	if breakpoints.is_empty() {
		println!("{}", "No breakpoints are currently set.".dimmed());
		return;
	}

	println!("{}", "Breakpoints:".bold());

	for breakpoint in breakpoints {
		match program.debug_location(breakpoint.body_index(), breakpoint.instruction_index()) {
			Some(location) => println!("  {}", format_location(&location)),
			None => println!(
				"  body {}, instruction {}",
				breakpoint.body_index(),
				breakpoint.instruction_index()
			),
		}
	}
}

fn prompt_command() -> Result<DebugCommand, String> {
	print!("{}", "tablodbg> ".bold());
	let _ = std::io::stdout().flush();

	let mut line = String::new();
	std::io::stdin()
		.read_line(&mut line)
		.map_err(|error| format!("Failed to read debugger input: {error}"))?;

	parse_command(&line)
}

fn read_dap_message(reader: &mut impl BufRead) -> Result<Option<String>, String> {
	let mut content_length = None;

	loop {
		let mut header_line = String::new();
		let bytes_read = reader.read_line(&mut header_line).map_err(|error| format!("Failed to read DAP header: {error}"))?;

		if bytes_read == 0 {
			return Ok(None);
		}

		let trimmed = header_line.trim_end_matches(['\r', '\n']);

		if trimmed.is_empty() {
			break;
		}

		if let Some(value) = trimmed.strip_prefix("Content-Length:") {
			let length = value.trim().parse::<usize>().map_err(|_| format!("Invalid DAP Content-Length header `{trimmed}`."))?;
			content_length = Some(length);
		}
	}

	let content_length = content_length.ok_or(String::from("Missing DAP Content-Length header."))?;
	let mut payload = vec![0; content_length];
	reader.read_exact(&mut payload).map_err(|error| format!("Failed to read DAP payload: {error}"))?;
	String::from_utf8(payload)
		.map(Some)
		.map_err(|_| String::from("DAP payload was not valid UTF-8."))
}

fn resolve_breakpoints(
	program: &Program,
	session: &DebuggerSession<'_>,
	breakpoint_specs: &[String],
) -> Result<Vec<InstructionBreakpoint>, String> {
	let mut breakpoints = Vec::new();

	for spec in breakpoint_specs {
		let (source_name, line) = parse_breakpoint_spec(spec)?;
		let resolved = session.resolve_source_breakpoints(source_name, &[line]);

		if resolved.is_empty() {
			let available_sources: Vec<_> = program.debug_info()
				.source_files()
				.iter()
				.map(|source_file| source_file.display_name().to_string())
				.collect();

			let sources_hint = if available_sources.is_empty() {
				String::from("No source-file debug metadata is present in this object file.")
			}
			else {
				format!("Known source files: {}", available_sources.join(", "))
			};

			return Err(format!(
				"Could not resolve breakpoint `{spec}` to an executable instruction. {sources_hint}"
			));
		}

		breakpoints.extend(resolved);
	}

	Ok(breakpoints)
}

fn run_dap_server() -> Result<(), String> {
	let stdin = std::io::stdin();
	let stdout = std::io::stdout();
	let mut reader = BufReader::new(stdin.lock());
	let mut writer = stdout.lock();
	let mut server = DapServer::new();

	while let Some(message) = read_dap_message(&mut reader)? {
		let request: DapRequest = serde_json::from_str(&message)
			.map_err(|error| format!("Failed to decode DAP request: {error}"))?;

		if request.type_name != "request" {
			continue;
		}

		let result = match request.command.as_str() {
			"configurationDone" => server.handle_configuration_done(request.seq),
			"continue" => server.handle_continue(request.seq),
			"disconnect" => Ok(server.handle_disconnect(request.seq)),
			"exceptionInfo" => server.handle_exception_info(request.seq),
			"initialize" => Ok(server.handle_initialize(request.seq)),
			"launch" => server.handle_launch(request.seq, request.arguments.clone()),
			"next" => server.handle_step_over(request.seq),
			"scopes" => server.handle_scopes(request.seq, request.arguments.clone()),
			"setBreakpoints" => server.handle_set_breakpoints(request.seq, request.arguments.clone()),
			"stackTrace" => server.handle_stack_trace(request.seq),
			"stepIn" => server.handle_step_in(request.seq),
			"stepOut" => server.handle_step_out(request.seq),
			"threads" => Ok(server.handle_threads(request.seq)),
			"variables" => server.handle_variables(request.seq, request.arguments.clone()),
			command => Err(format!("Unsupported DAP request `{command}`.")),
		};

		match result {
			Ok(messages) => {
				emit_dap_messages(&mut writer, &messages)?;
				if request.command == "disconnect" {
					return Ok(());
				}
			}
			Err(message) => {
				emit_dap_message(&mut writer, &dap_error_response(&request, message))?;
			}
		}
	}

	Ok(())
}

fn run_debug_loop(program: &Program, session: &mut DebuggerSession<'_>, mut stop: DebuggerStop) {
	loop {
		match &stop {
			DebuggerStop::Completed(_) => {
				print_completion(&stop);
				return;
			}
			DebuggerStop::Paused(paused) => {
				if let Some(frame) = paused.current_frame() {
					print_current_frame(frame);
				}
			}
		}

		let command = match prompt_command() {
			Ok(command) => command,
			Err(message) => {
				eprintln!("{message}");
				continue;
			}
		};

		stop = match command {
			DebugCommand::AddBreakpoint(spec) => {
				let specs = vec![spec];
				let breakpoints = match resolve_breakpoints(program, session, &specs) {
					Ok(breakpoints) => breakpoints,
					Err(message) => {
						eprintln!("{message}");
						continue;
					}
				};
				session.add_breakpoints(breakpoints);
				print_breakpoints(session, program);
				continue;
			}
			DebugCommand::Breakpoints => {
				print_breakpoints(session, program);
				continue;
			}
			DebugCommand::ClearBreakpoints => {
				session.clear_breakpoints();
				println!("{}", "Cleared all breakpoints.".dimmed());
				continue;
			}
			DebugCommand::Continue => match session.resume() {
				Ok(stop) => stop,
				Err(error) => {
					eprintln!("{}", TabloError::Runtime(error));
					return;
				}
			},
			DebugCommand::Help => {
				print_help();
				continue;
			}
			DebugCommand::Locals => {
				if let Some(paused) = stop.paused_state()
					&& let Some(frame) = paused.current_frame() {
					print_locals(frame);
				}
				continue;
			}
			DebugCommand::Quit => return,
			DebugCommand::Stack => {
				if let Some(paused) = stop.paused_state() {
					print_stack(paused);
				}
				continue;
			}
			DebugCommand::StepIn => match session.step_in() {
				Ok(stop) => stop,
				Err(error) => {
					eprintln!("{}", TabloError::Runtime(error));
					return;
				}
			},
			DebugCommand::StepOut => match session.step_out() {
				Ok(stop) => stop,
				Err(error) => {
					eprintln!("{}", TabloError::Runtime(error));
					return;
				}
			}
			DebugCommand::StepOver => match session.step_over() {
				Ok(stop) => stop,
				Err(error) => {
					eprintln!("{}", TabloError::Runtime(error));
					return;
				}
			},
		};
	}
}

#[cfg(test)]
mod tests {
	use std::collections::BTreeMap;

	use serde_json::Value as JsonValue;
	use tablo::ast::DataType;
	use tablo::value::DeferredSqliteValue;
	use tablo::value::RecordFieldValue;
	use tablo::value::RecordPointerValue;
	use tablo::value::Value;

	use super::DapServer;

	#[test]
	fn expands_record_pointer_fields_in_variables_view() {
		let mut fields = BTreeMap::new();
		fields.insert(
			String::from("address1"),
			RecordFieldValue::DeferredSqlite {
				data_type: DataType::Text,
				is_nullable: true,
				value: DeferredSqliteValue::Null,
			},
		);
		fields.insert(
			String::from("id"),
			RecordFieldValue::Materialized(Value::Integer(2)),
		);

		let value = Value::RecordPointer(RecordPointerValue {
			exists: true,
			fields,
			locked: false,
		});
		let mut server = DapServer::new();
		let variables = server.child_variables_for_value(&value).unwrap();

		assert_eq!(variables.len(), 2);
		assert_eq!(variables[0].get("name"), Some(&JsonValue::String(String::from("address1"))));
		assert_eq!(variables[0].get("value"), Some(&JsonValue::String(String::from("null"))));
		assert_eq!(variables[0].get("type"), Some(&JsonValue::String(String::from("null"))));
		assert_eq!(variables[1].get("name"), Some(&JsonValue::String(String::from("id"))));
		assert_eq!(variables[1].get("value"), Some(&JsonValue::String(String::from("2"))));
		assert_eq!(variables[1].get("type"), Some(&JsonValue::String(String::from("int"))));
	}
}
