#![cfg(feature = "compiler")]

use tablo::{TabloError, value::Value};

#[test]
fn catches_conversion_in_callee_and_restores_the_callers_stack() {
	let source = r#"
		fn Read(data: json): int { return data.amount; }

		fn Main(args: [text]): int {
			var result: int = 0;
			try {
				result = 3;
				result = 100 + Read({ "amount": 1.5 });
			} catch {
				result += Read({ "amount": 7 });
			}
			return result;
		}
	"#;
	let directory = tempfile::tempdir().unwrap();
	let path = directory.path().join("catch.tobj");
	tablo::compile(source, &path).unwrap();
	let program = tablo::object_file::read_program_from_path(path).unwrap();
	assert_eq!(tablo::run_program(&program).unwrap(), Some(Value::Integer(10)));
}

#[test]
fn failure_in_handler_reaches_outer_handler() {
	let result = tablo::run("
		fn Main(args: [text]): int {
			try {
				try { fail 'first'; } catch { fail 'second'; }
			} catch { return 1; }
		}
	").unwrap();
	assert_eq!(result, Some(Value::Integer(1)));
}

#[test]
fn leaving_try_on_break_retires_handler() {
	let result = tablo::run("
		fn Main(args: [text]): int {
			while true {
				try { break; } catch { return 0; }
			}
			fail 'outside';
		}
	");
	let Err(TabloError::Runtime(error)) = result else {
		panic!("Expected uncaught failure");
	};
	assert_eq!(error.message, "outside");
	assert_eq!(error.stack_trace.len(), 1);
}

#[test]
fn successful_try_skips_catch() {
	let result = tablo::run("
		fn Main(args: [text]): int {
			var value: int = 0;
			try { value = 1; } catch { fail 'unexpected'; }
			return value;
		}
	").unwrap();
	assert_eq!(result, Some(Value::Integer(1)));
}

#[test]
fn rejects_null_failure_and_fallthrough_handler() {
	for source in [
		"fn Main(args: [text]): int { fail null; }",
		"fn Main(args: [text]): int { try { return 1; } catch {} }",
	] {
		assert!(matches!(tablo::run(source), Err(TabloError::Compile(_))));
	}
}

#[test]
fn continue_and_break_leave_only_the_handlers_inside_the_loop() {
	let result = tablo::run("
		fn Main(args: [text]): int {
			var visits: int = 0;
			try {
				while true {
					try {
						visits += 1;
						if visits == 1 { continue; }
						break;
					} catch { return 0; }
				}
				fail 'outer';
			} catch { return visits; }
		}
	").unwrap();
	assert_eq!(result, Some(Value::Integer(2)));
}

#[test]
fn uncaught_failure_preserves_the_message_and_call_stack() {
	let result = tablo::run("
		fn Stop(reason: text): int { fail reason; }
		fn Main(args: [text]): int { return Stop('stopped'); }
	");
	let Err(TabloError::Runtime(error)) = result else {
		panic!("Expected an uncaught runtime failure");
	};
	assert_eq!(error.message, "stopped");
	assert_eq!(error.stack_trace.len(), 2);
}

fn round_trip(source: &str, expected: i64) {
	let directory = tempfile::tempdir().unwrap();
	let path = directory.path().join("handlers.tobj");
	tablo::compile(source, &path).unwrap();
	let program = tablo::object_file::read_program_from_path(path).unwrap();
	assert_eq!(tablo::run_program(&program).unwrap(), Some(Value::Integer(expected)));
}

#[test]
fn typed_handlers_keep_nominal_identity_and_dynamic_payloads() {
	round_trip("
		obj Missing { id: int };
		obj Duplicate { id: int };
		fn Raise(value: any) { fail value; }
		fn Main(args: [text]): int {
			try { Raise(Missing { id: 42 }); }
			catch err: Duplicate { return 1; }
			catch err: Missing { return err.id; }
			catch { return 2; }
			return 3;
		}", 42);
}

#[test]
fn typed_dispatch_uses_source_order_and_no_numeric_coercion() {
	round_trip("
		fn Main(args: [text]): int {
			try { fail 7; }
			catch err: dec { return 1; }
			catch err: float { return 2; }
			catch err: int { return err; }
			catch err: any { return 3; }
		}", 7);
}

#[test]
fn runtime_failure_binds_an_object_with_a_diagnostic() {
	round_trip(r#"fn Main(args: [text]): int {
		var data: json = { "value": 1.5 };
		try { return data.value; }
		catch reason: text { return 1; }
		catch err: RuntimeError { if err.message != '' { return 7; } return 2; }
	}"#, 7);
}

#[test]
fn finally_runs_on_success_failure_and_loop_exit() {
	round_trip("
		fn Main(args: [text]): int {
			var result: int = 0;
			try {
				try { fail 'problem'; }
				catch ignored: int { return 0; }
				finally { result += 1; }
			}
			catch message: text { result += 2; }
			finally { result *= 3; }
			while true {
				try { break; } finally { result += 1; }
			}
			return result;
		}", 10);
}

#[test]
fn return_runs_finally_and_a_new_failure_replaces_the_return() {
	round_trip("
		fn Inner(): int {
			try { return 1; } finally { fail 7; }
		}
		fn Main(args: [text]): int {
			try { return Inner(); } catch value: int { return value; }
		}", 7);
}

#[test]
fn unmatched_failure_preserves_original_stack_through_finally() {
	let result = tablo::run("
		fn Inner() { fail 'original'; }
		fn Main(args: [text]): int {
			try { Inner(); } catch value: int { return value; } finally {}
			return 0;
		}");
	let Err(TabloError::Runtime(error)) = result else {
		panic!("Expected unhandled failure");
	};
	assert_eq!(error.message, "original");
	assert_eq!(error.stack_trace.len(), 2);
}

#[test]
fn rejects_invalid_bindings_and_handler_order() {
	for statement in [
		"try {} catch err: text { err = 'changed'; }",
		"try {} catch err: text {} displn(err);",
		"try {} catch text {}",
		"try {} catch {} catch err: text {}",
		"try {} catch err: UnknownType {}",
		"var absent: any? = null; fail absent;",
	] {
		let source = format!("fn Main(args: [text]): int {{ {statement} return 0; }}");
		assert!(tablo::check(source).is_err(), "{statement}");
	}
}

#[test]
fn empty_arrays_keep_their_declared_type_through_any() {
	round_trip("
		fn Raise(value: any) { fail value; }
		fn Main(args: [text]): int {
			var empty: [text] = [];
			try { Raise(empty); }
			catch numbers: [int] { return 0; }
			catch strings: [text] { return 1 + len(strings); }
			return 0;
		}", 1);
}

#[test]
fn error_types_include_enums_and_named_array_types() {
	round_trip("
		enum Status: int { Missing: 7 }
		obj Ids [int];
		fn Main(args: [text]): int {
			var ids: Ids = [3];
			try {
				try { fail ids; }
				catch numbers: [int] { return 0; }
				catch caught: Ids { if caught[1] == 3 { fail Status.Missing; } }
			}
			catch value: int { return 1; }
			catch value: Status { return int(value); }
			return 0;
		}", 7);
}

#[test]
fn same_named_local_enums_have_distinct_identity() {
	round_trip("
		fn Raise() {
			enum State: int { Bad: 1 }
			fail State.Bad;
		}
		fn Main(args: [text]): int {
			enum State: int { Bad: 1 }
			try { Raise(); }
			catch err: State { return 0; }
			catch { return 7; }
			return 1;
		}", 7);
}

#[test]
fn finally_keeps_a_return_value_across_a_nested_handled_failure() {
	round_trip("
		fn Next(result: &int): int {
			try { return result; }
			finally {
				try { fail 'nested'; } catch ignored: text { result += 1; }
			}
		}
		fn Main(args: [text]): int {
			var result: int = 3;
			var saved: int = Next(&result);
			return saved * 10 + result;
		}", 34);
}

#[test]
fn transfer_from_finally_supersedes_pending_transfer() {
	round_trip("
		fn Main(args: [text]): int {
			var visits: int = 0;
			while true {
				try { return 99; }
				finally {
					visits += 1;
					if visits == 1 { continue; }
					break;
				}
			}
			return visits;
		}", 2);
}

#[test]
fn catch_types_cross_a_module_boundary() {
	let directory = tempfile::tempdir().unwrap();
	std::fs::write(directory.path().join("Errors.tablo"), "
		pub obj Missing { pub id: int };
		pub fn Raise() { fail Missing { id: 9 }; }
		pub fn Message(err: RuntimeError): text { return err.message; }
	").unwrap();
	let path = directory.path().join("main.tobj");
	tablo::compile_with_source_name("
		use './Errors';
		fn Main(args: [text]): int {
			try { Raise(); } catch err: Missing {
				try { var quotient: int = 1 / 0; }
				catch runtime: RuntimeError { if Message(runtime) != '' { return err.id; } }
			}
			return 0;
		}", directory.path().join("main.tablo").to_string_lossy(), &path).unwrap();
	let program = tablo::object_file::read_program_from_path(path).unwrap();
	assert_eq!(tablo::run_program(&program).unwrap(), Some(Value::Integer(9)));
}

#[test]
fn arrays_in_union_fields_keep_their_type_after_json_import() {
	round_trip(r#"
		obj Model { choice: int | [text] };
		fn Main(args: [text]): int {
			var model: Model = Model(json('{"choice":[]}'));
			try { fail model.choice; }
			catch wrong: [int] { return 0; }
			catch expected: [text] { return 1 + len(expected); }
		}
	"#, 1);
}

#[test]
fn failure_from_a_catch_runs_finally_and_skips_sibling_handlers() {
	round_trip("
		fn Main(args: [text]): int {
			var result: int = 0;
			try {
				try { fail 'first'; }
				catch reason: text { fail 5; }
				catch sibling: int { return 0; }
				finally { result += 2; }
			}
			catch value: int { return result + value; }
			return 1;
		}", 7);
}
