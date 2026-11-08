#![cfg(feature = "compiler")]
use tablo::value::Value;

fn succeeds(source: &str) {
	assert_eq!(tablo::run(source).unwrap(), Some(Value::Integer(1)));
	let directory = tempfile::tempdir().unwrap();
	let path = directory.path().join("conversion.tobj");
	tablo::compile(source, &path).unwrap();
	let program = tablo::object_file::read_program_from_path(path).unwrap();
	assert_eq!(tablo::run_program(&program).unwrap(), Some(Value::Integer(1)));
}

#[test]
fn explicit_conversion_uses_defaults_exact_numbers_and_public_exports() {
	succeeds(r#"pub obj Model { pub number: int, pub label: text = 'default', privateValue: int = 7 };
		fn Main(args: [text]): int {
			var source: json = { "NUMBER": 3.0, "privateValue": 9 };
			var model: Model = Model(source);
			var result: json = json(model);
			if model.number == 3 and model.label == 'default' and model.privateValue == 9
				and not exists result.privateValue and result == json('{"number":3,"label":"default"}') { return 1; }
			return 0;
		}"#);
}

#[test]
fn conversion_rejects_missing_extra_null_and_inexact_values() {
	for payload in [r#"{}"#, r#"{"number":3,"extra":1}"#, r#"{"number":null}"#, r#"{"number":3.5}"#] {
		let source = format!("pub obj Model {{ pub number: int }}; fn Main(args: [text]): int {{ var model: Model = Model(json('{payload}')); return 0; }}");
		assert!(matches!(tablo::run(source), Err(tablo::TabloError::Runtime(_))), "{payload}");
	}
}

#[test]
fn any_requires_a_value_and_any_nullable_accepts_null() {
	for statement in ["var value: any;", "var value: any = null;", "var absent: int?; var value: any = absent;"] {
		assert!(tablo::check(format!("fn Main(args: [text]): int {{ {statement} return 0; }}")).is_err(), "{statement}");
	}
	succeeds("fn Main(args: [text]): int { var value: any?; if value == null { return 1; } return 0; }");
}

#[test]
fn conversion_handles_nested_arrays_nullable_any_and_iso_dates() {
	succeeds(r#"pub obj Child { pub value: int };
		pub obj Model { pub children: [Child], pub extra: any?, pub dateValue: date };
		fn Main(args: [text]): int {
			var model: Model = Model(json('{"children":[{"value":3}],"extra":null,"dateValue":"2026-11-11"}'));
			var result: json = json(model);
			if model.children[1].value == 3 and model.extra == null
				and result == json('{"children":[{"value":3}],"extra":null,"dateValue":"2026-11-11"}') { return 1; }
			return 0;
		}"#);
}

#[test]
fn enum_and_union_conversions_reject_ambiguity() {
	succeeds(r#"enum State: int { Ready: 1, Done: 2 }
		pub obj Model { pub state: State, pub choice: int | text };
		fn Main(args: [text]): int {
			var model: Model = Model(json('{"state":"Done","choice":"text"}'));
			if model.state == State.Done and json(model) == json('{"state":"Done","choice":"text"}') { return 1; }
			return 0;
		}"#);
	for (declarations, payload) in [
		("pub obj Model { pub choice: int | dec };", r#"{"choice":3}"#),
		("enum State: int { A: 1, B: 1 } pub obj Model { pub choice: State };", r#"{"choice":1}"#),
		("enum State: int { A: 1, B: 2 } pub obj Model { pub choice: State };", r#"{"choice":3}"#),
	] {
		let source = format!("{declarations} fn Main(args: [text]): int {{ var model: Model = Model(json('{payload}')); return 0; }}");
		assert!(matches!(tablo::run(&source), Err(tablo::TabloError::Runtime(_))), "{source}");
	}
}

#[test]
fn nullable_fields_still_require_presence_or_an_explicit_default() {
	succeeds(r#"pub obj Model { pub optional: int? = null, pub value: any? };
		fn Main(args: [text]): int {
			var model: Model = Model(json('{"value":null}'));
			if model.optional == null and model.value == null { return 1; } return 0;
		}"#);
	for (field, payload) in [("optional: int?", "{}"), ("value: any", r#"{"value":null}"#)] {
		let source = format!("pub obj Model {{ pub {field} }}; fn Main(args: [text]): int {{ var model: Model = Model(json('{payload}')); return 0; }}");
		assert!(matches!(tablo::run(source), Err(tablo::TabloError::Runtime(_))));
	}
}

#[test]
fn conversion_enforces_scope_visibility_across_source_modules() {
	let directory = tempfile::tempdir().unwrap();
	let library = directory.path().join("Models.tablo");
	std::fs::write(&library, r#"pub obj Model { pub value: int, hidden: int = 7 };
		pub fn ReadHidden(value: Model): int { return value.hidden; }
		pub fn Import(value: json): Model { return Model(value); }"#).unwrap();
	for (payload, succeeds) in [(r#"{"value":3}"#, true), (r#"{"value":3,"hidden":9}"#, false)] {
		let source = format!("use './Models'; fn Main(args: [text]): int {{ var model: Model = Model(json('{payload}')); return ReadHidden(model); }}");
		let path = directory.path().join("main.tobj");
		tablo::compile_with_source_name(source, directory.path().join("main.tablo").to_string_lossy(), &path).unwrap();
		let result = tablo::run_program(&tablo::object_file::read_program_from_path(path).unwrap());
		if succeeds { assert_eq!(result.unwrap(), Some(Value::Integer(7))); } else { assert!(result.is_err()); }
	}
	let source = r#"use './Models'; fn Main(args: [text]): int {
		var model: Model = Import(json('{"value":3,"hidden":9}'));
		if not exists json(model).hidden { return ReadHidden(model); } return 0;
	}"#;
	let path = directory.path().join("inside.tobj");
	tablo::compile_with_source_name(source, directory.path().join("main.tablo").to_string_lossy(), &path).unwrap();
	assert_eq!(tablo::run_program(&tablo::object_file::read_program_from_path(path).unwrap()).unwrap(), Some(Value::Integer(9)));
}

#[test]
fn conversion_reports_nested_paths_and_converts_numeric_json_in_any() {
	let source = r#"pub obj Child { pub amount: int }; pub obj Model { pub children: [Child] };
		fn Main(args: [text]): int { var model: Model = Model(json('{"children":[{"amount":1.5}]}')); return 0; }"#;
	let error = tablo::run(source).unwrap_err().to_string();
	assert!(error.contains("children.[1].amount"), "{error}");
	succeeds(r#"pub obj Model { pub value: any };
		fn Main(args: [text]): int {
			var source: json = json('{"value":3.25}');
			var model: Model = Model(source);
			if json(model) == source { return 1; } return 0;
		}"#);
}

#[test]
fn matching_and_manual_private_export_are_explicit() {
	succeeds(r#"pub obj Model { pub "Name": int, pub "name": int, hidden: text = 'secret' };
		fn Main(args: [text]): int {
			var model: Model = Model(json('{"Name":1,"name":2}'));
			var result: json = json(model);
			if exists result.hidden { return 0; }
			result.hidden = model.hidden;
			if result == json('{"Name":1,"name":2,"hidden":"secret"}') { return 1; } return 0;
		}"#);
	let source = r#"pub obj Model { pub name: int };
		fn Main(args: [text]): int { var model: Model = Model(json('{"Name":1,"name":2}')); return 0; }"#;
	assert!(matches!(tablo::run(source), Err(tablo::TabloError::Runtime(_))));
}

#[test]
fn root_arrays_and_iso_temporal_fields_round_trip() {
	succeeds(r#"pub obj Rows [int];
		pub obj Dates { pub timeValue: time, pub timestampValue: timestamp, pub zoned: timestamptz };
		fn Main(args: [text]): int {
			var rows: Rows = Rows(json('[1,2,3]'));
			var dates: Dates = Dates(json('{"timeValue":"12:30:00","timestampValue":"2026-11-11T12:30:00","zoned":"2026-11-11T12:30:00+02:00"}'));
			if json(rows) == json('[1,2,3]') and json(dates) == json('{"timeValue":"12:30:00","timestampValue":"2026-11-11T12:30:00","zoned":"2026-11-11T12:30:00+02:00"}') { return 1; }
			return 0;
		}"#);
}

#[test]
fn any_rejects_runtime_json_null_but_nullable_any_accepts_it() {
	assert!(matches!(tablo::run("fn Main(args: [text]): int { var value: any = json('[null]')[1]; return 0; }"), Err(tablo::TabloError::Runtime(_))));
	succeeds("fn Main(args: [text]): int { var value: any? = json('[null]')[1]; if value == null { return 1; } return 0; }");
	assert!(tablo::check("obj Model { value: any }; fn Main(args: [text]): int { var model: Model; return 0; }").is_err());
}

#[test]
fn qualified_inline_types_support_positional_conversion_arguments() {
	succeeds(r#"pub obj Outer { pub inner: obj Inner { pub value: int } };
		fn Main(args: [text]): int {
			var value: Outer.Inner = Outer.Inner(json('{"value":3}'));
			if value.value == 3 { return 1; } return 0;
		}"#);
}

#[test]
fn json_conversion_is_explicit_and_invalid_iso_text_fails() {
	for source in [
		"pub obj Model { pub value: int }; fn Main(args: [text]): int { var model: Model = json('{}'); return 0; }",
		"pub obj Model { pub value: int }; fn Main(args: [text]): int { var model: Model; var data: json = model; return 0; }",
	] {
		assert!(tablo::check(source).is_err());
	}
	let source = r#"pub obj Model { pub dateValue: date };
		fn Main(args: [text]): int { var model: Model = Model(json('{"dateValue":"11/11/2026"}')); return 0; }"#;
	assert!(matches!(tablo::run(source), Err(tablo::TabloError::Runtime(_))));
}

#[test]
fn conversion_example_runs() {
	assert_eq!(tablo::run(include_str!("../../Examples/json_conversion.tablo")).unwrap(), Some(Value::Integer(0)));
}

#[test]
fn explicit_nested_defaults_apply_only_when_the_property_is_omitted() {
	succeeds(r#"pub obj Child { pub value: int }; pub obj Model { pub child: Child = Child {} };
		fn Main(args: [text]): int {
			var model: Model = Model(json('{}'));
			if model.child.value == 0 { return 1; } return 0;
		}"#);
	let source = r#"pub obj Child { pub value: int }; pub obj Model { pub child: Child = Child {} };
		fn Main(args: [text]): int { var model: Model = Model(json('{"child":{}}')); return 0; }"#;
	assert!(matches!(tablo::run(source), Err(tablo::TabloError::Runtime(_))));
}

#[test]
fn conversion_constructors_reject_named_arguments() {
	for argument in ["v: json('{}')", "payload: json('{}')", "v: default"] {
		let source = format!("obj Model {{ value: int }}; fn Main(args: [text]): int {{ var model: Model = Model({argument}); return 0; }}");
		assert!(tablo::check(source).is_err(), "{argument}");
	}
}

#[test]
fn enum_names_work_in_literals_mutations_and_duplicate_backing_imports() {
	succeeds(r#"enum State: int { Ready: 1, Done: 1 }
		pub obj Model { pub state: State };
		fn Main(args: [text]): int {
			var model: Model = Model({ "state": 'Done' });
			var data: json = { "state": State.Ready, "items": [State.Done] };
			data.state = model.state;
			if data == json('{"state":"Done","items":["Done"]}') and json(model) == json('{"state":"Done"}') { return 1; } return 0;
		}"#);
	for payload in [r#"{"state":"done"}"#, r#"{"state":"Missing"}"#, r#"{"state":1}"#] {
		let source = format!("enum State: int {{ Done: 1 }} pub obj Model {{ pub state: State }}; fn Main(args: [text]): int {{ var model: Model = Model(json('{payload}')); return 0; }}");
		assert!(matches!(tablo::run(source), Err(tablo::TabloError::Runtime(_))));
	}
}

#[test]
fn any_contents_reject_objects_and_inexact_numbers_with_paths() {
	for payload in [r#"{"value":{}}"#, r#"{"value":[{}]}"#, r#"{"value":999999999999999999999999999999999999999999}"#] {
		let source = format!("pub obj Model {{ pub value: any }}; fn Main(args: [text]): int {{ var model: Model = Model(json('{payload}')); return 0; }}");
		let error = tablo::run(source).unwrap_err().to_string();
		assert!(error.contains("value"), "{error}");
	}
	succeeds(r#"pub obj Model { pub value: any }; fn Main(args: [text]): int {
		var model: Model = Model({ "value": [3, true, null, ['2026-11-13']] });
		var document: any = json('{}');
		if json(model) == json('{"value":[3,true,null,["2026-11-13"]]}') and json(document) == json('{}') { return 1; } return 0;
	}"#);
}
