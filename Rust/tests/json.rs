#![cfg(feature = "compiler")]

use tablo::value::Value;

fn assert_program(source: &str, expected: Value) {
	assert_eq!(tablo::run(source).unwrap(), Some(expected.clone()));
	let directory = tempfile::tempdir().unwrap();
	let path = directory.path().join("program.tobj");
	tablo::compile(source, &path).unwrap();
	let program = tablo::object_file::read_program_from_path(path).unwrap();
	assert_eq!(tablo::run_program(&program).unwrap(), Some(expected));
}

fn assert_runtime_failure(source: &str) {
	assert!(matches!(tablo::run(source), Err(tablo::TabloError::Runtime(_))), "{source}");
	let directory = tempfile::tempdir().unwrap();
	let path = directory.path().join("program.tobj");
	tablo::compile(source, &path).unwrap();
	let program = tablo::object_file::read_program_from_path(path).unwrap();
	assert!(matches!(tablo::run_program(&program), Err(tablo::TabloError::Runtime(_))), "{source}");
}

#[test]
fn defaults_signatures_containers_and_serialization_survive_object_files() {
	assert_program(r#"
		obj Envelope { data: json };
		fn Echo(value: json): json { return value; }
		fn Main(args: [text]): int {
			var empty: json;
			var absent: json?;
			var envelope: Envelope;
			var values: [json] = [Echo(json('[1,true,null,"hello"]')), empty];
			if text(values[1]) == '[1,true,null,"hello"]' and text(empty) == '{}'
				and text(envelope.data) == '{}' and absent == null and empty != null { return 1; }
			return 0;
		}
	"#, Value::Integer(1));
}

#[test]
fn equality_is_structural_exact_and_separate_from_native_values() {
	assert_program(r#"
		fn Main(args: [text]): int {
			var left: json = json('{"a":[1,1.0,1e0,-0],"b":true}');
			var right: json = json('{"b":true,"a":[1.0,1e0,1,0.0]}');
			var boxed: any = left;
			if left == right and boxed == right
				and json('[9007199254740993]') != json('[9007199254740992]')
				and json('[1e999999999999999999999]') == json('[10e999999999999999999998]')
				and json('{"A":1}') != json('{"a":1}')
				and json('[1,2]') != json('[2,1]')
				and boxed != '[1]' { return 1; }
			return 0;
		}
	"#, Value::Integer(1));
}

#[test]
fn parsing_preserves_large_and_precise_numbers() {
	assert_program(r#"fn Main(args: [text]): int {
		var original: text = '[12345678901234567890123456789012345678901234567890,0.12345678901234567890123456789012345679,1e999999999999999999999]';
		var value: json = json(original);
		var copy: json = value;
		if text(copy) == original and json(text(copy)) == value { return 1; }
		return 0;
	}"#, Value::Integer(1));
}

#[test]
fn rejects_fragments_malformed_documents_and_decoded_duplicate_keys() {
	for input in ["null", "42", "true", "\"hello\"", "", "{} {}", "[1,]", "{\"x\":1,\"x\":2}",
		"{\"x\":{\"a\":1,\"\\u0061\":2}}", "[01]", "[NaN]", "[\"\\uD800\"]"] {
		let escaped = input.replace('\\', "\\\\").replace('\'', "\\'");
		let source = format!("fn Main(args: [text]): int {{ var value: json = json('{escaped}'); return 0; }}");
		let error = tablo::run(&source).unwrap_err();
		assert!(matches!(error, tablo::TabloError::Runtime(_)), "{input}: {error:?}");
	}
}

#[test]
fn rejects_unsupported_json_operators_and_enum_backing() {
	for source in [
		"fn Main(args: [text]): int { var value: json = null; return 0; }",
		"fn Main(args: [text]): int { return json('{}') + json('{}'); }",
		"fn Main(args: [text]): int { if json('{}') < json('{}') { return 1; } return 0; }",
		"fn Main(args: [text]): int { if not json('{}') { return 1; } return 0; }",
		"enum Choice: json { A = 1 }; fn Main(args: [text]): int { return 0; }",
	] {
		assert!(tablo::check(source).is_err(), "{source}");
	}
}

#[test]
fn json_keyword_and_watch_conversions_are_available() {
	use tablo::source::SourceText;
	use tablo::syntax::{lexer::Lexer, token::TokenKind};
	let tokens = Lexer::new(SourceText::new("json JSON \"json\"")).tokenize().unwrap();
	assert_eq!(tokens[0].kind, TokenKind::JsonKeyword);
	assert_eq!(tokens[1].kind, TokenKind::JsonKeyword);
	assert_eq!(tokens[2].kind, TokenKind::Identifier);
	let frame = tablo::vm::VmStackFrame { awaited_at: None, instruction_index: 0, locals: Vec::new(), source_location: None };
	assert_eq!(tablo::vm::VirtualMachine::evaluate_watch_expression("text(v: json(v: '[1e999999999999999999999]'))", &frame).unwrap(),
		Value::Text("[1e999999999999999999999]".into()));
}

#[test]
fn json_signatures_and_defaults_link_without_library_sources() {
	use tablo::bytecode::DebugInfoLevel;
	use tablo::executable::{compile_library, compile_object};
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	let source = r#"pub fn Echo(value: json = [999999999999999999999999999999999999999, json('[1e999999999999999999999]')]): json { return value; }"#;
	let path = root.join("Library.tablo");
	std::fs::write(&path, source).unwrap();
	compile_library(source, &path, &root.join("Library.tlib"), None, DebugInfoLevel::Full).unwrap();
	std::fs::remove_file(path).unwrap();
	let facade = "use './Library'; pub fn Value(): json { return Echo(); }";
	let path = root.join("Facade.tablo");
	std::fs::write(&path, facade).unwrap();
	compile_library(facade, &path, &root.join("Facade.tlib"), None, DebugInfoLevel::Full).unwrap();
	std::fs::remove_file(path).unwrap();
	std::fs::remove_file(root.join("Library.tlib")).unwrap();
	let main = "use './Facade'; fn Main(args: [text]): int { if text(Value()) == '[999999999999999999999999999999999999999,[1e999999999999999999999]]' { return 42; } return 0; }";
	let path = root.join("main.tablo");
	std::fs::write(&path, main).unwrap();
	compile_object(main, &path, &root.join("main.tobj"), None, DebugInfoLevel::Full).unwrap();
	std::fs::remove_file(root.join("Facade.tlib")).unwrap();
	let program = tablo::object_file::read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program(&program).unwrap(), Some(Value::Integer(42)));
}

#[test]
#[cfg(target_os = "linux")]
#[ignore = "builds embedded and shared native runtimes with Cargo; run explicitly with --ignored"]
fn json_runs_in_embedded_and_shared_runtimes_without_drivers() {
	use tablo::bytecode::DebugInfoLevel;
	use tablo::executable::{ExecutableBuildOptions, RuntimeMode, compile_executable};
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	let source = r#"enum State { A: 1, B: 2 }
	pub obj Payload { pub amount: int, pub state: State, pub values: any };
	fn Main(args: [text]): int {
		var converted: Payload = Payload({ "amount": 42, "state": State.B, "values": [3, null, true] });
		var value: json = json('[9007199254740993,1e999999999999999999999]');
		var data: json = { "Value": 40 + 2, "value": null, "huge": [000999999999999999999999999999999999999999] };
		var mutation: json = json('{"values":[0]}');
		mutation.values[1] = 41;
		mutation.values[2] = 42;
		var result: int = mutation.values[2];
		var indexed: json = json('[{"Value":42}]')[1];
		if json(converted) == json('{"amount":42,"state":"B","values":[3,null,true]}') and json(text(value)) == value and text(value) == '[9007199254740993,1e999999999999999999999]'
			and exists data."value" and data."value" == null and indexed."Value" == json('[42]')[1]
			and dec(42) == 42.0 and dec('1.99999999999999999999999999999999999999') == 2.0
			and int(json('[3.9]')[1]) == 3 and dec(json('[1.25]')[1]) == 1.25 { return result; }
		return 0;
	}"#;
	let path = root.join("main.tablo");
	std::fs::write(&path, source).unwrap();
	for (runtime, name) in [(RuntimeMode::Embedded, "embedded"), (RuntimeMode::Shared, "shared")] {
		let runtime_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
		let options = ExecutableBuildOptions {
			cache_dir: runtime_dir.join("target/embedded-test-cache"), runtime_dir, offline: true,
			debug_info: DebugInfoLevel::Lines, runtime,
		};
		compile_executable(source, &path, &root.join(name), None, &options).unwrap();
	}
	std::fs::remove_file(path).unwrap();
	for name in ["embedded", "shared"] {
		let output = std::process::Command::new(root.join(name)).current_dir(root).env_remove("TABLO_CONFIG").output().unwrap();
		assert_eq!(output.status.code(), Some(42), "{}", String::from_utf8_lossy(&output.stderr));
	}
	assert!(root.join("lib/libtablo_runtime_0_1_none.so").is_file());
}

#[test]
fn reads_nested_fields_into_typed_destinations_and_parameters() {
	assert_program(r#"
		fn Check(name: text, active: bool, age: int, score: dec, child: json): int {
			if name == 'Ada' and active and age == 42 and score == 1.25 and text(child) == '[1,2]' { return 1; }
			return 0;
		}
		fn Main(args: [text]): int {
			var data: json = json('{"Customer":{"Name":"Ada","active":true,"age":42.0,"score":1.25,"child":[1,2],"absent":null}}');
			var name: text = data.customer.name;
			var active: bool = data.customer.active;
			var age: int = data.customer.age;
			var score: dec = data.customer.score;
			var child: json = data.customer.child;
			var absent: text? = data.customer.absent;
			name = data.customer.name;
			if name == 'Ada' and active and age == 42 and score == 1.25 and absent == null
				and data.customer.absent == null and text(child) == '[1,2]' {
				return Check(data.customer.name, data.customer.active, data.customer.age, data.customer.score, data.customer.child);
			}
			return 0;
		}
	"#, Value::Integer(1));
}

#[test]
fn json_presence_preserves_quoted_names_and_handles_untraversable_paths() {
	assert_program(r#"fn Main(args: [text]): int {
		var data: json = json('{"Name":null,"name":{"x":1},"scalar":1,"array":[]}');
		var missing: json?;
		var number: int = data."name".x;
		if exists data."Name" and exists data."name".x and not exists data."Name".x
			and not exists data.missing.x and not exists data.scalar.x and not exists data.array.x
			and not exists missing.x and number == 1 { return 1; }
		return 0;
	}"#, Value::Integer(1));
}

#[test]
fn json_reads_fail_for_missing_ambiguous_and_incompatible_values() {
	for expression in ["data.name", "data.missing", "data.scalar.x", "data.nil.x", "data.array.x"] {
		let source = format!(r#"fn Main(args: [text]): int {{
			var data: json = json('{{"Name":1,"name":2,"scalar":3,"nil":null,"array":[]}}');
			var value: int = {expression}; return 0;
		}}"#);
		assert!(matches!(tablo::run(&source), Err(tablo::TabloError::Runtime(_))), "{source}");
	}
	for (target, value) in [("int", "1.5"), ("int", "9223372036854775808"), ("int", "1e999999999999999999999"),
		("int", "\"42\""), ("text", "true"), ("bool", "1"), ("json", "42"), ("json", "null"),
		("dec", "1e999999999999999999999"), ("text", "null")] {
		let source = format!(r#"fn Main(args: [text]): int {{ var data: json = json('{{"v":{value}}}'); var v: {target} = data.v; return 0; }}"#);
		assert!(matches!(tablo::run(&source), Err(tablo::TabloError::Runtime(_))), "{source}");
	}
	let source = r#"fn Main(args: [text]): int { var data: json = json('{"Name":1,"name":2}'); if exists data.name { return 1; } return 0; }"#;
	assert!(tablo::run(source).unwrap_err().to_string().contains("ambiguous"));
}

#[test]
fn json_argument_overloads_do_not_inspect_contents() {
	let source = r#"
		fn Choose(value: int): int { return 1; }
		fn Choose(value: text): int { return 2; }
		fn Main(args: [text]): int { var data: json = json('{"value":1}'); return Choose(data.value); }
	"#;
	assert!(tablo::check(source).unwrap_err().to_string().to_lowercase().contains("ambiguous"));
	let source = r#"obj Item { value: int };
		fn Read(value: json): int { return 1; }
		fn Read(value: Item): int { return 2; }
		fn Main(args: [text]): int { return Read({ "different": true }); }"#;
	assert!(tablo::check(source).unwrap_err().to_string().to_lowercase().contains("ambiguous"));
}

#[test]
fn json_fields_convert_in_returns_objects_arrays_and_builtin_arguments() {
	assert_program(r#"
		obj Result { name: text, total: int };
		fn Name(data: json): text { return data.name; }
		fn Main(args: [text]): int {
			var data: json = json('{"name":"Ada","total":3,"child":{"x":1}}');
			var result: Result = Result { name: data.name, total: data.total };
			var names: [text] = [data.name];
			result.name = data.name;
			names[1] = data.name;
			if Name(data) == result.name and names[1] == 'Ada' and len(data.name) == result.total
				and data.child == json('{"x":1}') { return 1; }
			return 0;
		}
	"#, Value::Integer(1));
}

#[test]
fn json_field_conversions_preserve_the_source_and_numeric_boundaries() {
	assert_program(r#"fn Main(args: [text]): int {
		var data: json = json('{"min":-9223372036854775808,"max":9223372036854775807,"precise":0.1234567890123456789012345678901234567,"zero":0e999999999999999999999}');
		var copy: json = data;
		var minimum: int = data.min;
		var maximum: int = data.max;
		var precise: dec = data.precise;
		var zero: dec = data.zero;
		if minimum == -9223372036854775807 - 1 and maximum == 9223372036854775807
			and precise == 0.1234567890123456789012345678901234567 and zero == 0 and data == copy { return 1; }
		return 0;
	}"#, Value::Integer(1));
}

#[test]
fn json_watch_field_access_preserves_quoted_names() {
	let frame = tablo::vm::VmStackFrame { awaited_at: None, instruction_index: 0, locals: Vec::new(), source_location: None };
	assert_eq!(tablo::vm::VirtualMachine::evaluate_watch_expression(r#"exists json('{"Name":null,"name":1}')."Name""#, &frame).unwrap(), Value::Boolean(true));
	assert!(tablo::vm::VirtualMachine::evaluate_watch_expression(r#"exists json('{"Name":null,"name":1}').name"#, &frame).unwrap_err().message.contains("ambiguous"));
}

#[test]
fn indexes_json_arrays_and_converts_elements_in_typed_contexts() {
	assert_program(r#"
		fn Number(value: int): int { return value; }
		fn Name(value: json): text { return value[1].name; }
		fn Main(args: [text]): int {
			var data: json = json('[{"Name":"Ada","values":[42,1.25,true,null]},[1,2]]');
			var original: json = data;
			var index: int = 1;
			var name: text = data[index].name;
			var number: int = data[1].values[1];
			var decimal: dec = data[1].values[2];
			var flag: bool = data[1].values[3];
			var absent: text? = data[1].values[4];
			var nested: json = data[2];
			var finalValue: int = data[2][2];
			var numbers: [int] = [data[1].values[1], data[2][2]];
			number = data[1].values[1];
			if name == 'Ada' and Name(data) == name and number == 42 and decimal == 1.25
				and flag and absent == null and data[1].values[4] == null
				and text(nested) == '[1,2]' and finalValue == 2 and numbers[1] == 42
				and exists data[1]."Name" and not exists data[1].missing
				and data == original { return Number(data[1].values[1]); }
			return 0;
		}
	"#, Value::Integer(42));
}

#[test]
fn json_indexing_preserves_exact_numbers_before_conversion() {
	assert_program(r#"fn Main(args: [text]): int {
		var values: json = json('[9007199254740993,1e999999999999999999999,[1.0000000000000000000000000000000000000001]]');
		var equivalent: json = json('[9007199254740993.0,10e999999999999999999998]');
		var nested: json = values[3];
		if values[1] == equivalent[1] and values[2] == equivalent[2]
			and text(nested) == '[1.0000000000000000000000000000000000000001]' { return 1; }
		return 0;
	}"#, Value::Integer(1));
}

#[test]
fn json_indexing_rejects_invalid_bounds_and_non_array_values() {
	for expression in ["data[0]", "data[-1]", "data[6]", "data[9223372036854775807]",
		"data[-9223372036854775807 - 1]", "data[1][1]", "data[2][1]", "data[3][1]",
		"data[4][1]", "data[5][1]", "json('{}')[1]", "absent[1]"] {
		let source = format!(r#"fn Main(args: [text]): int {{
			var data: json = json('[{{}},"text",1,null,[]]'); var absent: json?;
			var value: any = {expression}; return 0;
		}}"#);
		assert!(matches!(tablo::run(&source), Err(tablo::TabloError::Runtime(_))), "{source}");
	}
	for value in ["\"42\"", "1.5", "null", "1e999999999999999999999"] {
		let source = format!(r#"fn Main(args: [text]): int {{ var data: json = json('[{value}]'); var value: int = data[1]; return 0; }}"#);
		assert!(matches!(tablo::run(&source), Err(tablo::TabloError::Runtime(_))), "{source}");
	}
}

#[test]
fn json_indices_require_integers_and_reject_compound_mutation() {
	for statement in ["var value: any = data['1'];", "var value: any = data[1.0];",
		"var value: any = data[true];", "var value: any = data[1:2];",
		"data[1] += 2;"] {
		let source = format!("fn Main(args: [text]): int {{ var data: json = json('[1,2]'); {statement} return 0; }}");
		assert!(tablo::check(&source).is_err(), "{source}");
	}
	let source = r#"
		fn Choose(value: int): int { return 1; }
		fn Choose(value: text): int { return 2; }
		fn Main(args: [text]): int { return Choose(json('[1]')[1]); }
	"#;
	assert!(tablo::check(source).unwrap_err().to_string().to_lowercase().contains("ambiguous"));
	let source = r#"obj Item { value: int };
		fn Read(value: json): int { return 1; }
		fn Read(value: Item): int { return 2; }
		fn Main(args: [text]): int { return Read({ "different": true }); }"#;
	assert!(tablo::check(source).unwrap_err().to_string().to_lowercase().contains("ambiguous"));
}

#[test]
fn json_watch_expressions_support_nested_indexing() {
	let frame = tablo::vm::VmStackFrame { awaited_at: None, instruction_index: 0, locals: Vec::new(), source_location: None };
	assert_eq!(tablo::vm::VirtualMachine::evaluate_watch_expression(r#"json('[{"v":[null]}]')[1].v[1] == null"#, &frame).unwrap(), Value::Boolean(true));
	assert!(tablo::vm::VirtualMachine::evaluate_watch_expression("json('[]')[1]", &frame).unwrap_err().message.contains("out of bounds"));
}

#[test]
fn mutates_json_fields_and_arrays_without_changing_copies() {
	assert_program(r#"
		fn Change(value: json): json { value.items[1].name = 'Changed'; return value; }
		fn Main(args: [text]): int {
			var data: json = json('{"Name":"old","items":[{"name":"Ada"}],"numbers":[1]}');
			var original: json = data;
			data.name = 'new';
			data."name" = null;
			data.items[1].name = 'Grace';
			data.numbers[2] = 1.25;
			data.numbers[1] = true;
			data.extra = json('{"large":1e999999999999999999999}');
			data.exact = data.extra.large;
			var assigned: int = (data.answer = 42);
			var changed: json = Change(data);
			var name: text = data."Name";
			var nested: text = data.items[1].name;
			var changedName: text = changed.items[1].name;
			if name == 'new' and nested == 'Grace' and changedName == 'Changed' and assigned == 42
				and data."name" == null and exists data."name"
				and data.exact == data.extra.large and text(data.numbers) == '[true,1.25]'
				and text(original) == '{"Name":"old","items":[{"name":"Ada"}],"numbers":[1]}' { return 1; }
			return 0;
		}
	"#, Value::Integer(1));
}

#[test]
fn json_mutation_supports_root_arrays_and_evaluates_indices_once() {
	assert_program(r#"fn Main(args: [text]): int {
		var data: json = json('[[0]]');
		var index: int = 0;
		data[(index += 1)][1] = 5;
		data[2] = json('[]');
		data[2][1] = null;
		data[1][2] = data[1][1];
		if index == 1 and text(data) == '[[5,5],[null]]' { return 1; }
		return 0;
	}"#, Value::Integer(1));
}

#[test]
fn json_mutation_rejects_missing_intermediates_ambiguity_and_sparse_arrays() {
	for statement in ["data.name = 1;", "data.missing.x = 1;", "data.nil.x = 1;", "data.scalar.x = 1;",
		"data.items[0] = 1;", "data.items[3] = 1;", "data.items[2].x = 1;",
		"data.items[-9223372036854775807 - 1] = 1;", "data.items[9223372036854775807] = 1;",
		"data.scalar[1] = 1;", "absent.x = 1;"] {
		let source = format!(r#"fn Main(args: [text]): int {{
			var data: json = json('{{"Name":1,"name":2,"nil":null,"scalar":1,"items":[0]}}'); var absent: json?;
			{statement} return 0;
		}}"#);
		assert!(matches!(tablo::run(&source), Err(tablo::TabloError::Runtime(_))), "{source}");
	}
	for statement in ["var values: [int] = [1]; data.x = values;", "data.x = args;", "data.x += 1;", "data.items['1'] = 1;"] {
		let source = format!("fn Main(args: [text]): int {{ var data: json = json('{{}}'); {statement} return 0; }}");
		assert!(tablo::check(&source).is_err(), "{source}");
	}
	for statement in ["data.x = 1;", "data[1] = 1;", "data.items[1].x = 1;", "data.\"x\" = 1;"] {
		let source = format!("fn Main(args: [text]): int {{ const data: json = json('{{}}'); {statement} return 0; }}");
		assert!(tablo::check(&source).is_err(), "{source}");
	}
}

#[test]
fn explicit_integer_conversion_truncates_without_changing_json() {
	assert_program(r#"fn Main(args: [text]): int {
		var data: json = json('[3.9,-3.9,9223372036854775807.9,-9223372036854775808.9,1e-999999999999999]');
		var original: text = text(data);
		if int(data[1]) == 3 and int(data[2]) == -3
			and int(data[3]) == 9223372036854775807
			and int(data[4]) == (-9223372036854775807 - 1)
			and int(data[5]) == 0 and int(-3.9) == -3
			and text(data) == original { return 1; }
		return 0;
	}"#, Value::Integer(1));
}

#[test]
fn explicit_integer_conversion_rejects_range_errors_and_non_numeric_json() {
	for contents in ["9223372036854775808", "-9223372036854775809", "1e999999999999999", "null", "true", "\"3\"", "{}", "[]"] {
		let source = format!("fn Main(args: [text]): int {{ var data: json = json('[{contents}]'); return int(data[1]); }}");
		assert_runtime_failure(&source);
	}
}

#[test]
fn explicit_decimal_conversion_rounds_to_nearest_with_ties_away_from_zero() {
	let prefix = "1.123456789012345678901234567890123456";
	for (tail, expected_tail) in [("74", "7"), ("75", "8"), ("76", "8")] {
		for sign in ["", "-"] {
			let input = format!("{sign}{prefix}{tail}");
			let expected = format!("{sign}{prefix}{expected_tail}");
			assert_program(&format!(r#"fn Main(args: [text]): int {{
				var data: json = json('[{input}]');
				var original: text = text(data);
				if dec(data[1]) == {expected} and dec(v: '{input}') == {expected}
					and text(data) == original {{ return 1; }}
				return 0;
			}}"#), Value::Integer(1));
		}
	}
}

#[test]
fn explicit_decimal_conversion_handles_integers_exponents_and_rounding_carry() {
	assert_program(r#"fn Main(args: [text]): int {
		if dec(42) == 42.0 and dec(-42) == -42.0
			and dec('1.25e3') == 1250.0 and dec('+001.25') == 1.25
			and dec('.5') == 0.5 and dec('1.') == 1.0
			and dec('1.99999999999999999999999999999999999999') == 2.0
			and dec('1e-999999999999999999999') == 0.0
			and dec('5e-38') == 0.0000000000000000000000000000000000001
			and dec('-5e-38') == -0.0000000000000000000000000000000000001
			and dec('4e-38') == 0.0 { return 1; }
		return 0;
	}"#, Value::Integer(1));
}

#[test]
fn explicit_decimal_conversion_rejects_invalid_text_range_errors_and_wrong_json_kinds() {
	for text in ["", " ", "+-1", "--1", "1e", "1e+", "NaN", "Infinity", "1.2.3", "1_000", "1e999999999999999999999", "99999999999999999999999999999999999999.1"] {
		let source = format!("fn Main(args: [text]): int {{ var value: dec = dec('{text}'); return 0; }}");
		assert_runtime_failure(&source);
	}
	for contents in ["null", "true", "\"1.5\"", "{}", "[]", "1e999999999999999999999"] {
		let source = format!("fn Main(args: [text]): int {{ var data: json = json('[{contents}]'); var value: dec = dec(data[1]); return 0; }}");
		assert_runtime_failure(&source);
	}
}

#[test]
fn json_literals_construct_nested_values_and_evaluate_expressions() {
	assert_program(r#"
		fn Next(counter: &int): int { counter += 1; return counter; }
		fn Main(args: [text]): int {
			var counter: int = 0;
			var name: text = 'Alice';
			var original: json = json('{"active":true}');
			var data: json = {
				"name": 'Hello ${name}', "first": Next(&counter),
				"values": [Next(&counter), { "last": Next(&counter) }, null, true, 'hello'],
				"details": original, "empty": {},
			};
			data.details.active = false;
			if counter == 3 and int(data."first") == 1 and int(data.values[1]) == 2
				and int(data.values[2]."last") == 3 and original.active == json('[true]')[1]
				and data.name == json('["Hello Alice"]')[1] and exists data.values[2]."last" { return 1; }
			return 0;
		}
	"#, Value::Integer(1));
}

#[test]
fn json_literals_preserve_large_signed_numbers_and_decimal_precision() {
	assert_program(r#"fn Main(args: [text]): int {
		var data: json = { "huge": 12345678901234567890123456789012345678901234567890,
			"negative": -12345678901234567890123456789012345678901234567890,
			"fraction": .123456789012345678901234567890123456789,
			"nested": [000999999999999999999999999999999999999999] };
		if data == json('{"huge":12345678901234567890123456789012345678901234567890,"negative":-12345678901234567890123456789012345678901234567890,"fraction":0.123456789012345678901234567890123456789,"nested":[999999999999999999999999999999999999999]}') { return 1; }
		return 0;
	}"#, Value::Integer(1));
}

#[test]
fn json_literals_use_assignment_return_field_and_parameter_contexts() {
	assert_program(r#"
		obj Envelope { payload: json };
		fn Produce(): json { return { "items": [1, 'two'] }; }
		fn Read(data: json): int { return data.value; }
		fn Main(args: [text]): int {
			var optional: json? = {};
			var values: [json] = [{ "value": 1 }, [2, 3]];
			var envelope: Envelope = Envelope { payload: { "value": 4 } };
			envelope.payload = [true, null];
			optional = { "next": Produce() };
			values[1] = { "value": 5 };
			if Read(data: { "value": 6 }) == 6 and int(values[1].value) == 5
				and envelope.payload[1] == json('[true]')[1] and optional.next.items[2] == json('["two"]')[1] { return 1; }
			return 0;
		}
	"#, Value::Integer(1));
}

#[test]
fn json_literal_keys_preserve_case_and_reject_exact_duplicates() {
	assert_program(r#"fn Main(args: [text]): int {
		var data: json = { "Name": 1, "name": 2, "property-name": 'yes' };
		if int(data."Name") == 1 and int(data."name") == 2 and data."property-name" == json('["yes"]')[1] { return 1; }
		return 0;
	}"#, Value::Integer(1));
	let source = r#"fn Main(args: [text]): int { var data: json = { "same": 1, "same": 2 }; return 0; }"#;
	assert!(tablo::run(source).unwrap_err().to_string().contains("specified more than once"));
}

#[test]
fn json_literals_support_defaults_builtins_and_mutation() {
	assert_program(r#"obj Envelope { payload: json = { "value": -999999999999999999999999999999999999999 } };
		fn Read(value: json = { "value": 7 }): int { return value.value; }
		fn Main(args: [text]): int {
			var envelope: Envelope = Envelope {};
			var data: json = {};
			data.items = [1, { "value": Read() }];
			data.items[1] = { "nested": [true, null] };
			if int(data.items[2].value) == 7 and data.items[1].nested == json('[true,null]')
				and text({ "value": 3 }) == '{"value":3}' and text([1, true]) == '[1,true]'
				and envelope.payload == json('{"value":-999999999999999999999999999999999999999}') { return 1; }
			return 0;
		}"#, Value::Integer(1));
}

#[test]
fn json_literal_overloads_do_not_inspect_contents() {
	let source = r#"fn Read(value: json): int { return 1; }
		fn Read(value: [int]): int { return 2; }
		fn Main(args: [text]): int { return Read(['text']); }"#;
	assert!(tablo::check(source).unwrap_err().to_string().to_lowercase().contains("ambiguous"));
	let source = r#"obj Item { value: int };
		fn Read(value: json): int { return 1; }
		fn Read(value: Item): int { return 2; }
		fn Main(args: [text]): int { return Read({ "different": true }); }"#;
	assert!(tablo::check(source).unwrap_err().to_string().to_lowercase().contains("ambiguous"));
	assert_program(r#"fn Read(value: json, flag: int): int { return 1; }
		fn Read(value: [int], flag: text): int { return 2; }
		fn Main(args: [text]): int {
			var data: json = [1];
			if Read(['text'], 1) == 1 and Read([2], 'native') == 2 and Read(data, 1) == 1 { return 1; }
			return 0;
		}"#, Value::Integer(1));
}

#[test]
fn json_literals_reject_implicit_native_containers_and_oversized_arithmetic() {
	for statement in [
		"var data: json = 1;",
		"var data: json = { unquoted: 1 };",
		"var values: [int] = [1]; var data: json = { \"values\": values };",
		"var data: json = [999999999999999999999999999999999999999 + 1];",
		"var data: json = [1 + 999999999999999999999999999999999999999];",
	] {
		let source = format!("fn Main(args: [text]): int {{ {statement} return 0; }}");
		assert!(tablo::check(&source).is_err(), "{source}");
	}
}

#[test]
fn json_literals_support_variadic_parameters() {
	assert_program(r#"fn Read(...values: [json]): int { return values[2].value; }
		fn Main(args: [text]): int { return Read([], { "value": 7 }); }"#, Value::Integer(7));
}

#[test]
fn json_literals_in_watches_match_compiled_expressions() {
	let frame = tablo::vm::VmStackFrame {
		awaited_at: None,
		instruction_index: 0,
		locals: vec![tablo::vm::VmVisibleLocal {
			declared_type: "int".into(), is_const: false, name: "total".into(),
			slot: 0, value: Value::Integer(3),
		}],
		source_location: None,
	};
	for (expression, expected) in [
		(r#"text({ "value": total + 1, "message": 'Count ${total}', "nested": [true, null, {}] })"#, r#"{"message":"Count 3","nested":[true,null,{}],"value":4}"#),
		(r#"text(v: [000999999999999999999999999999999999999999, -.123456789012345678901234567890123456789])"#, "[999999999999999999999999999999999999999,-0.123456789012345678901234567890123456789]"),
		(r#"text({ "Name": 1, "name": 2, "copy": json('[1e999999999999999999999]') })"#, r#"{"Name":1,"copy":[1e999999999999999999999],"name":2}"#),
		("text([])", "[]"),
		("text({})", "{}"),
	] {
		let source = format!("fn Main(args: [text]): int {{ var total: int = 3; if {expression} == '{expected}' {{ return 1; }} return 0; }}");
		assert_program(&source, Value::Integer(1));
		assert_eq!(tablo::vm::VirtualMachine::evaluate_watch_expression(expression, &frame).unwrap(), Value::Text(expected.into()), "{expression}");
	}
	assert_eq!(frame.locals[0].value, Value::Integer(3));
}

#[test]
fn json_literal_watches_reject_invalid_values_and_side_effects() {
	let frame = tablo::vm::VmStackFrame { awaited_at: None, instruction_index: 0, locals: Vec::new(), source_location: None };
	for expression in [
		r#"text({ unquoted: 1 })"#,
		r#"text({ "same": 1, "same": 2 })"#,
		r#"text([999999999999999999999999999999999999999 + 1])"#,
		r#"text([split('a,b', ',')])"#,
		r#"text([displn('side effect')])"#,
		r#"text([missing = 1])"#,
		r#"text([Unknown()])"#,
		r#"json({ "value": 1 })"#,
	] {
		assert!(tablo::vm::VirtualMachine::evaluate_watch_expression(expression, &frame).is_err(), "{expression}");
	}
	assert_eq!(tablo::vm::VirtualMachine::evaluate_watch_expression("[1, 2]", &frame).unwrap(),
		Value::Array(vec![Value::Integer(1), Value::Integer(2)]));
	assert!(tablo::vm::VirtualMachine::evaluate_watch_expression(r#"{ "value": 1 }"#, &frame).is_err());
}

#[test]
fn reassigning_a_converted_number_preserves_oversized_siblings_after_serialization() {
	assert_program(r#"
		fn Main(args: [text]): int {
			var data: json = { "fraction": 3.75, "nested": [123456789012345678901234567890123456789012345678901] };
			data.fraction = int(data.fraction);
			var restored: json = json(text(data));
			if restored == json('{"fraction":3,"nested":[123456789012345678901234567890123456789012345678901]}') {
				return 1;
			}
			return 0;
		}
	"#, Value::Integer(1));
}


#[test]
fn json_null_is_a_distinct_scalar() {
	assert_program("
		fn Main(args: [text]): int {
			var nested: json = [JsonNull];
			var value: any = json(' null ');
			if value == JsonNull and JsonNull != null and nested[1] == JsonNull { return 1; }
			return 0;
		}
	", Value::Integer(1));

	for target in ["json", "json?"] {
		let source = format!("fn Main(args: [text]): int {{ var value: {target} = JsonNull; return 0; }}");
		assert!(matches!(tablo::run(&source), Err(tablo::TabloError::Compile(_))));
	}

	for declaration in ["var value: json?", "var value: json"] {
		let source = format!("fn Main(args: [text]): int {{ {declaration} = json('null'); return 0; }}");
		assert_runtime_failure(&source);
	}

	assert_runtime_failure("
		fn Take(value: json): int { return 1; }
		fn Main(args: [text]): int { return Take(json('null')); }
	");
	assert_runtime_failure("
		fn Main(args: [text]): int { var values: [json] = [json('null')]; return 0; }
	");
}
