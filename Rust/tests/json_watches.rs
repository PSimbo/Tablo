#![cfg(feature = "compiler")]

use tablo::bytecode::{DebugInfoLevel, Program};
use tablo::value::Value;
use tablo::vm::{VirtualMachine, VmStackFrame};

fn compile(source: &str, path: &std::path::Path) -> Program {
	let output = path.with_extension("tobj");
	tablo::executable::compile_object(source, path, &output, None, DebugInfoLevel::Full).unwrap();
	tablo::object_file::read_program_from_path(output).unwrap()
}

fn paused_frames(program: &Program) -> Vec<VmStackFrame> {
	let Err(tablo::TabloError::Runtime(error)) = tablo::run_program(program) else { panic!("Expected a paused runtime error"); };
	error.stack_trace
}

fn watch(program: &Program, frame: &VmStackFrame, expression: &str) -> Value {
	VirtualMachine::evaluate_watch_expression_in_program(expression, frame, program).unwrap()
}

#[test]
fn conversions_in_watches_use_serialized_descriptors_and_preserve_locals() {
	let directory = tempfile::tempdir().unwrap();
	let source = r#"pub obj Model { pub amount: int, pub extra: any?, hidden: int = 7 };
		pub obj Outer { pub inner: obj Inner { pub value: int } };
		pub obj Rows [int];
		fn Main(args: [text]): int {
			var data: json = { "amount": 3, "extra": [3, null], "hidden": 9 };
			var model: Model = Model(data);
			return 1 / 0;
		}"#;
	let program = compile(source, &directory.path().join("main.tablo"));
	let frames = paused_frames(&program);
	let frame = &frames[0];
	let original = frame.clone();
	assert_eq!(watch(&program, frame, "Model(data)"), frame.local("model").unwrap().value);
	assert_eq!(watch(&program, frame, "Model(data).hidden"), Value::Integer(9));
	assert_eq!(watch(&program, frame, "text(json(Model(data)))"), Value::Text(r#"{"amount":3,"extra":[3,null]}"#.into()));
	assert_eq!(watch(&program, frame, r#"Outer.Inner({ "value": 4 }).value"#), Value::Integer(4));
	assert_eq!(watch(&program, frame, "text(json(Rows([1, 2])))"), Value::Text("[1,2]".into()));
	for expression in ["Model(v: data)", "Model()", "Model(data, data)", "Model(&data)", "Model('{}')", "Model(data = data)", "Model({})", "Model({ \"amount\": 3.5, \"extra\": null })"] {
		assert!(VirtualMachine::evaluate_watch_expression_in_program(expression, frame, &program).is_err(), "{expression}");
	}
	assert_eq!(*frame, original);
}

#[test]
fn watch_conversion_access_follows_selected_module_frame_without_source_files() {
	let directory = tempfile::tempdir().unwrap();
	let model_path = directory.path().join("Models.tablo");
	std::fs::write(&model_path, r#"pub obj Model { pub value: int, hidden: int = 7 };
		obj Secret { value: int };
		pub obj Outer { pub inner: obj Inner { pub value: int } };
		pub fn Stop(data: json): int { return 1 / 0; }"#).unwrap();
	let program = compile(r#"use './Models'; fn Main(args: [text]): int {
		var data: json = { "value": 3, "hidden": 9 }; return Stop(data);
	}"#, &directory.path().join("main.tablo"));
	std::fs::remove_file(model_path).unwrap();
	let frames = paused_frames(&program);
	assert_eq!(frames.len(), 2);
	assert_eq!(watch(&program, &frames[1], r#"Outer.Inner({ "value": 4 }).value"#), Value::Integer(4));
	assert_eq!(watch(&program, &frames[0], "Model(data).hidden"), Value::Integer(9));
	assert_eq!(watch(&program, &frames[1], r#"text(json(Model({ "value": 3 })))"#), Value::Text(r#"{"value":3}"#.into()));
	let error = VirtualMachine::evaluate_watch_expression_in_program("Model(data)", &frames[1], &program).unwrap_err();
	assert!(error.message.contains("hidden"), "{}", error.message);
	assert!(VirtualMachine::evaluate_watch_expression_in_program("Secret({})", &frames[1], &program).is_err());
	assert_eq!(watch(&program, &frames[0], r#"Secret({ "value": 2 }).value"#), Value::Integer(2));
}

#[test]
fn stripped_debug_info_does_not_grant_conversion_scope_access() {
	let directory = tempfile::tempdir().unwrap();
	let program = compile("obj Model { hidden: int = 7 }; fn Main(args: [text]): int { return 1 / 0; }", &directory.path().join("main.tablo"));
	for level in [DebugInfoLevel::Lines, DebugInfoLevel::None] {
		let mut stripped = program.clone();
		stripped.debug_info_mut().retain_level(level);
		let stripped = tablo::object_file::read_program(&tablo::object_file::write_program(&stripped)).unwrap();
		let frames = paused_frames(&stripped);
		assert!(VirtualMachine::evaluate_watch_expression_in_program("Model({})", &frames[0], &stripped).is_err());
	}
}

#[test]
fn float_json_watches_share_runtime_conversion_rules() {
	let directory = tempfile::tempdir().unwrap();
	let program = compile(r#"pub obj Model { pub value: float };
		fn Main(args: [text]): int {
			var data: json = { "value": 0.1 };
			var model: Model = Model(data);
			return 1 / 0;
		}"#, &directory.path().join("float.tablo"));
	let frames = paused_frames(&program);
	let frame = &frames[0];
	assert_eq!(watch(&program, frame, "float(data.value)"), Value::Float(tablo::float::Float(0.1)));
	assert_eq!(watch(&program, frame, "Model(data)"), frame.local("model").unwrap().value);
	assert_eq!(watch(&program, frame, "float(json(model).value)"), Value::Float(tablo::float::Float(0.1)));
	assert!(VirtualMachine::evaluate_watch_expression_in_program(r#"json([float('NaN')])"#, frame, &program).is_err());
}
