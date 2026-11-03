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
	let frame = tablo::vm::VmStackFrame { instruction_index: 0, locals: Vec::new(), source_location: None };
	assert_eq!(tablo::vm::VirtualMachine::evaluate_watch_expression("text(v: json(v: '[1e999999999999999999999]'))", &frame).unwrap(),
		Value::Text("[1e999999999999999999999]".into()));
}

#[test]
fn json_signatures_and_defaults_link_without_library_sources() {
	use tablo::bytecode::DebugInfoLevel;
	use tablo::executable::{compile_library, compile_object};
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	let source = r#"pub fn Echo(value: json = json('[1e999999999999999999999]')): json { return value; }"#;
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
	let main = "use './Facade'; fn Main(args: [text]): int { if text(Value()) == '[1e999999999999999999999]' { return 42; } return 0; }";
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
	let source = r#"fn Main(args: [text]): int {
		var value: json = json('[9007199254740993,1e999999999999999999999]');
		var data: json = json('{"Value":42,"value":null}');
		var mutation: json = json('{"values":[0]}');
		mutation.values[1] = 41;
		mutation.values[2] = 42;
		var result: int = mutation.values[2];
		var indexed: json = json('[{"Value":42}]')[1];
		if json(text(value)) == value and text(value) == '[9007199254740993,1e999999999999999999999]'
			and exists data."value" and data."value" == null and indexed."Value" == json('[42]')[1] { return result; }
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
	let frame = tablo::vm::VmStackFrame { instruction_index: 0, locals: Vec::new(), source_location: None };
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
}

#[test]
fn json_watch_expressions_support_nested_indexing() {
	let frame = tablo::vm::VmStackFrame { instruction_index: 0, locals: Vec::new(), source_location: None };
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
	for statement in ["data.x = [1];", "data.x = args;", "data.x += 1;", "data.items['1'] = 1;"] {
		let source = format!("fn Main(args: [text]): int {{ var data: json = json('{{}}'); {statement} return 0; }}");
		assert!(tablo::check(&source).is_err(), "{source}");
	}
	for statement in ["data.x = 1;", "data[1] = 1;", "data.items[1].x = 1;", "data.\"x\" = 1;"] {
		let source = format!("fn Main(args: [text]): int {{ const data: json = json('{{}}'); {statement} return 0; }}");
		assert!(tablo::check(&source).is_err(), "{source}");
	}
}
