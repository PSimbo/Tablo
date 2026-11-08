#![cfg(feature = "compiler")]

use tablo::object_construction::ObjectConstructor;
use tablo::value::Value;

fn program(source: &str) -> tablo::bytecode::Program {
	let directory = tempfile::tempdir().unwrap();
	let path = directory.path().join("objects.tobj");
	tablo::compile(source, &path).unwrap();
	tablo::object_file::read_program_from_path(path).unwrap()
}

#[test]
fn runtime_construction_completes_defaults_from_serialized_descriptors() {
	let program = program(r#"enum Status: int { Ready: 7, Done: 9 }
		obj Leaf { number: int = 4, label: text };
		obj Config {
			leaf: Leaf = Leaf { number: 8 }, items: [Leaf], optional: Leaf?,
			status: Status, data: json = { "values": [999999999999999999999999999999999999999] },
			title: text = 'default'
		};
		fn Main(args: [text]): int { var value: Config; return value.leaf.number; }"#);
	let constructor = ObjectConstructor::new(program.object_type_descriptors().iter());
	let id = program.object_type_descriptors().iter().find(|d| d.display_name() == "Config").unwrap().id();
	let Value::Object(value) = constructor.construct(id, vec![("title".into(), Value::Text("supplied".into()))]).unwrap() else { panic!() };
	assert_eq!(value.object_type_id, id);
	assert_eq!(value.fields["title"], Value::Text("supplied".into()));
	assert_eq!(value.fields["optional"], Value::Null);
	assert_eq!(value.fields["items"], Value::Array(Vec::new()));
	let Value::Object(leaf) = &value.fields["leaf"] else { panic!() };
	assert_eq!(leaf.fields["number"], Value::Integer(8));
	assert_eq!(leaf.fields["label"], Value::Text(String::new()));
	let Value::Enum(status) = &value.fields["status"] else { panic!() };
	assert_eq!(status.variant_name, "Ready");
	assert_eq!(*status.backing_value, Value::Integer(7));
	assert_eq!(tablo::run_program(&program).unwrap(), Some(Value::Integer(8)));
}

#[test]
fn runtime_construction_distinguishes_omission_null_and_invalid_fields() {
	let program = program(r#"obj Model { number: int = 7, optional: int? = 9, values: [int] };
		fn Main(args: [text]): int { return 0; }"#);
	let constructor = ObjectConstructor::new(program.object_type_descriptors().iter());
	let id = program.object_type_descriptors()[0].id();
	let Value::Object(value) = constructor.construct(id, vec![("optional".into(), Value::Null)]).unwrap() else { panic!() };
	assert_eq!(value.fields["number"], Value::Integer(7));
	assert_eq!(value.fields["optional"], Value::Null);
	for fields in [
		vec![("number".into(), Value::Null)],
		vec![("missing".into(), Value::Integer(1))],
		vec![("number".into(), Value::Integer(1)), ("number".into(), Value::Integer(2))],
		vec![("values".into(), Value::Array(vec![Value::Text("wrong".into())]))],
	] {
		let error = constructor.construct(id, fields).unwrap_err();
		assert!(!error.path.is_empty(), "{error}");
	}
}

#[test]
fn runtime_construction_preserves_terminating_recursive_defaults() {
	let program = program(r#"obj Node { next: Node? = Node { next: null }, children: [Node] };
		fn Main(args: [text]): int { var node: Node; return 0; }"#);
	let constructor = ObjectConstructor::new(program.object_type_descriptors().iter());
	let Value::Object(node) = constructor.construct(program.object_type_descriptors()[0].id(), Vec::new()).unwrap() else { panic!() };
	let Value::Object(next) = &node.fields["next"] else { panic!() };
	assert_eq!(next.fields["next"], Value::Null);
	assert_eq!(next.fields["children"], Value::Array(Vec::new()));
}
