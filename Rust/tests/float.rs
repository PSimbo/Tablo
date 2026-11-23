#![cfg(feature = "compiler")]
use tablo::value::Value;

fn succeeds(source: &str) {
	assert_eq!(tablo::run(source).unwrap(), Some(Value::Integer(1)));
	let directory = tempfile::tempdir().unwrap();
	let path = directory.path().join("float.tobj");
	tablo::compile(source, &path).unwrap();
	assert_eq!(tablo::run_program(&tablo::object_file::read_program_from_path(path).unwrap()).unwrap(), Some(Value::Integer(1)));
}

#[test]
fn native_float_arithmetic_and_contextual_literals() {
	succeeds("fn Twice(value: float): float { return value * 2; } fn Main(args: [text]): int { var a: float = 1.25; var b: float = 2; var c: float = a + 0.5; var values: [float] = [1.5, 2]; if Twice(1.25) == float('2.5') and c == float('1.75') and b == float(2) and values[1] == a + float('0.25') { return 1; } return 0; }");
}

#[test]
fn ieee_special_values_and_signed_zero() {
	succeeds("fn Main(args: [text]): int { var zero: float; var negative: float = -0.0; var inf: float = float(1) / zero; var nan: float = zero / zero; if inf == float('inf') and float(1) / negative == float('-inf') and nan != nan and not (nan < zero) and not (nan >= zero) and negative == zero { return 1; } return 0; }");
}

#[test]
fn explicit_conversions_and_strict_function_arguments() {
	succeeds("fn Main(args: [text]): int { var n: float = float('3.75'); var d: dec = dec(n); var boxed: any = 123.45; if int(n) == 3 and d == 3.75 and boxed == 123.45 { return 1; } return 0; }");
	for statement in ["var value: int = float(3);", "var value: dec = float(3);", "var d: dec = 3.5; var value: float = d;", "var value: float = float(3) + dec(2);"] {
		assert!(tablo::check(format!("fn Main(args: [text]): int {{ {statement} return 0; }}")).is_err(), "{statement}");
	}
	for expression in ["int(float('NaN'))", "int(float('inf'))", "int(float('9223372036854775808'))", "dec(float('inf'))", "float('no')"] {
		assert!(matches!(tablo::run(format!("fn Main(args: [text]): int {{ var value: any = {expression}; return 0; }}")), Err(tablo::TabloError::Runtime(_))), "{expression}");
	}
}

#[test]
fn containers_defaults_and_reference_assignments_preserve_float_values() {
	succeeds("obj Model { value: float = 1.25, values: [float] = [2.5] }; fn Set(value: &float) { value = 7; } fn Read(value: float = 2.5): float { return value; } fn Main(args: [text]): int { var model: Model; var ints: [int] = [1, 2]; var floats: [float] = ints; var optional: float?; var n: int = 3; optional = n; var f: float = 1.0; Set(&f); if f == 7 and Read() == 2.5 and model.value == 1.25 and floats[2] == 2 and optional == 3 { return 1; } return 0; }");
	assert!(tablo::check("fn Read(value: float): int { return 1; } fn Main(args: [text]): int { var integer: int = 1; return Read(integer); }").is_err());
}

#[test]
fn contextual_precision_and_default_literal_overloads() {
	succeeds("fn Read(value: float): float { return value; } fn Choose(value: dec): int { return 1; } fn Choose(value: float): int { return 0; } fn Main(args: [text]): int { var large: float = 1234567890123456789012345678901234567890.5; var fine: float = 0.123456789012345678901234567890123456789; if large == float('1234567890123456789012345678901234567890.5') and fine == Read(0.123456789012345678901234567890123456789) and Choose(1.25) == 1 { return 1; } return 0; }");
}

#[test]
fn library_float_defaults_survive_without_library_source() {
	use tablo::bytecode::DebugInfoLevel;
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	let library_path = root.join("Numbers.tablo");
	let library = "pub fn Twice(value: float = 1.25): float { return value * 2; }";
	std::fs::write(&library_path, library).unwrap();
	tablo::executable::compile_library(library, &library_path, &root.join("Numbers.tlib"), None, DebugInfoLevel::Full).unwrap();
	std::fs::remove_file(library_path).unwrap();
	let source = "use './Numbers'; fn Main(args: [text]): int { if Twice() == float('2.5') and Twice(2.5) == float(5) { return 1; } return 0; }";
	let output = root.join("main.tobj");
	tablo::executable::compile_object(source, &root.join("main.tablo"), &output, None, DebugInfoLevel::Full).unwrap();
	assert_eq!(tablo::run_program(&tablo::object_file::read_program_from_path(output).unwrap()).unwrap(), Some(Value::Integer(1)));
}

#[test]
fn float_watch_arithmetic_and_conversions_match_compiled_code() {
	use tablo::vm::{VirtualMachine, VmStackFrame, VmVisibleLocal};
	let frame = VmStackFrame { awaited_at: None, instruction_index: 0, source_location: None, locals: vec![VmVisibleLocal {
		name: "value".into(), slot: 0, declared_type: "float".into(), is_const: false,
		value: Value::Float(tablo::float::Float(1.25)),
	}] };
	assert_eq!(VirtualMachine::evaluate_watch_expression("value + 0.5", &frame).unwrap(), Value::Float(tablo::float::Float(1.75)));
	assert_eq!(VirtualMachine::evaluate_watch_expression("value + (0.5 + 0.25)", &frame).unwrap(), Value::Float(tablo::float::Float(2.0)));
	assert_eq!(VirtualMachine::evaluate_watch_expression("(0.5 + 0.25) + value", &frame).unwrap(), Value::Float(tablo::float::Float(2.0)));
	assert_eq!(VirtualMachine::evaluate_watch_expression("0.5 + value", &frame).unwrap(), Value::Float(tablo::float::Float(1.75)));
	assert_eq!(VirtualMachine::evaluate_watch_expression("text(float('inf'))", &frame).unwrap(), Value::Text("inf".into()));
	assert_eq!(VirtualMachine::evaluate_watch_expression("int(value)", &frame).unwrap(), Value::Integer(1));
	assert_eq!(VirtualMachine::evaluate_watch_expression("float(0.123456789012345678901234567890123456789)", &frame).unwrap(), Value::Float(tablo::float::Float::parse("0.123456789012345678901234567890123456789").unwrap()));
}

#[test]
fn explicit_literal_casts_and_targeted_ternaries() {
	succeeds("fn Read(value: float): float { return value; } fn Main(args: [text]): int { var f: float = true ? 1.25 : 3.5; if f == 1.25 and Read(1.25 + 2.5) == 3.75 and float(0.123456789012345678901234567890123456789) == float('0.123456789012345678901234567890123456789') { return 1; } return 0; }");
}

#[test]
#[cfg(target_os = "linux")]
#[ignore = "builds embedded and shared native runtimes with Cargo; run explicitly with --ignored"]
fn floats_run_in_embedded_and_shared_native_runtimes() {
	use tablo::bytecode::DebugInfoLevel;
	use tablo::executable::{ExecutableBuildOptions, RuntimeMode, compile_executable};
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	let source = "fn Main(args: [text]): int { var f: float = 1.25; var zero: float; var nan: float = zero / zero; if f + 0.5 == 1.75 and nan != nan and float(1) / zero == float('inf') and int(f * 2) == 2 { return 42; } return 0; }";
	for (name, runtime_mode) in [("embedded", RuntimeMode::Embedded), ("shared", RuntimeMode::Shared)] {
		let runtime_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
		let options = ExecutableBuildOptions { cache_dir: runtime_dir.join("target/embedded-test-cache"), runtime_dir, offline: true, runtime: runtime_mode, debug_info: DebugInfoLevel::Full };
		compile_executable(source, &root.join("main.tablo"), &root.join(name), None, &options).unwrap();
		let output = std::process::Command::new(root.join(name)).current_dir(root).env_remove("TABLO_CONFIG").output().unwrap();
		assert_eq!(output.status.code(), Some(42), "{}", String::from_utf8_lossy(&output.stderr));
	}
}

#[test]
fn floats_compose_with_text_unions_and_array_concatenation() {
	succeeds("obj Model { value: float | text, values: [float] }; fn Main(args: [text]): int { var number: int = 2; var left: [float] = [1.25]; var right: [int] = [2]; var model: Model = Model { value: number, values: left + right }; var boxed: any = model.value; if 'value=' + left[1] == 'value=1.25' and left[1] + '!' == '1.25!' and boxed == float(2) and model.values[2] == float(2) { return 1; } return 0; }");
}

#[test]
fn float_context_accepts_integer_literals_beyond_int_capacity() {
	succeeds("fn Read(value: float): float { return value; } fn Main(args: [text]): int { var f: float = 18446744073709551616; if f == float('18446744073709551616') and Read(-18446744073709551616) == -f { return 1; } return 0; }");
	assert!(tablo::check("fn Main(args: [text]): int { var value: int = 18446744073709551616; return 0; }").is_err());
}

#[test]
fn arithmetic_literals_adopt_float_context_on_either_side() {
	succeeds("fn Read(value: float): float { return value; } fn Read(value: text): float { return 0; } fn Main(args: [text]): int { var f: float = 1.25; if (0.5 + 0.75) == f and f == (0.5 + 0.75) and Read(0.5 + 0.75) == f and Read(-(0.5 + 0.75)) == -f and (0.5 + 0.75) + f == 2.5 { return 1; } return 0; }");
}

#[test]
fn json_float_fields_literals_mutation_and_objects_round_trip() {
	succeeds(r#"pub obj Model { pub value: float, pub values: [float], pub optional: float? };
		fn Read(value: float): float { return value; }
		fn Main(args: [text]): int {
			var source: json = json('{"value":0.1,"values":[1.25,-0.0],"optional":null}');
			var value: float = source.value;
			var model: Model = Model(source);
			var exported: json = json(model);
			var restored: Model = Model(exported);
			var literal: json = { "value": value, "array": [value] };
			literal.value = float(2);
			literal.array[1] = value;
			var negativeZero: float = restored.values[2];
			var native: any = source.value;
			if value == float('0.1') and Read(source.value) == value and float(source.value) == value
				and restored.value == value and restored.optional == null and float(literal.value) == 2
				and float(literal.array[1]) == value and float(1) / negativeZero == float('-inf')
				and native == dec('0.1') { return 1; }
			return 0;
		}"#);
}

#[test]
fn json_float_ieee_boundaries_and_invalid_inputs() {
	succeeds(r#"fn Main(args: [text]): int {
		var data: json = json('[1e9999,-1e9999,-1e-9999,0.123456789012345678901234567890123456789]');
		var positive: float = data[1]; var negative: float = data[2]; var zero: float = data[3];
		var rounded: float = data[4];
		if positive == float('inf') and negative == float('-inf') and float(1) / zero == negative
			and rounded == float('0.123456789012345678901234567890123456789') { return 1; } return 0;
	}"#);
	for value in ["null", "true", "\"0.5\"", "{}", "[]"] {
		for conversion in ["data[1]", "float(data[1])"] {
			let source = format!("fn Main(args: [text]): int {{ var data: json = json('[{value}]'); var value: float = {conversion}; return 0; }}");
			assert!(matches!(tablo::run(source), Err(tablo::TabloError::Runtime(_))), "{value}: {conversion}");
		}
	}
	for value in ["NaN", "inf", "-inf"] {
		for statement in [format!(r#"var data: json = {{ "value": float('{value}') }};"#),
			format!("var data: json = json('[0]'); data[1] = float('{value}');"),
			format!("var values: [float] = [float('{value}')]; var data: json = json(values);")] {
			let result = tablo::run(format!("fn Main(args: [text]): int {{ {statement} return 0; }}"));
			assert!(matches!(result, Err(tablo::TabloError::Runtime(_))), "{result:?}");
		}
	}
}
