#![cfg(feature = "compiler")]

use tablo::bytecode::Program;
use tablo::debugger::DebuggerSession;
use tablo::object_file::read_program_from_path;
use tablo::value::Value;
use tablo::{TabloError, compile_with_source_name, run_program};

fn compile_object(source: &str) -> Program {
	let directory = tempfile::tempdir().unwrap();
	let path = directory.path().join("async.tobj");
	compile_with_source_name(source, "async.tablo", &path).unwrap();
	read_program_from_path(path).unwrap()
}

#[test]
fn blocking_main_and_nested_suspension_restore_pending_operands() {
	let source = "async fn Answer(): int { return 42; }
		fn Main(args: [text]): int { return block Answer(); }";
	assert_eq!(run_program(&compile_object(source)).unwrap(), Some(Value::Integer(42)));

	let source = "async fn Child(n: int): int { return n; }
		async fn Outer(): int { return 20 + suspend Child(20); }
		fn Main(args: [text]): int { return 2 + block Outer(); }";
	assert_eq!(run_program(&compile_object(source)).unwrap(), Some(Value::Integer(42)));
}

#[test]
fn unwinding_through_suspended_callers_runs_cleanup_and_restores_the_handler() {
	let source = "async fn Child(): int { fail 'child'; }
		async fn Outer(): int { try { return 3 + suspend Child(); } finally { } }
		fn Main(args: [text]): int {
			try { return 99 + block Outer(); }
			catch err: text { return err == 'child' ? 42 : 0; }
		}";
	assert_eq!(run_program(&compile_object(source)).unwrap(), Some(Value::Integer(42)));
}

#[test]
fn child_cleanup_finishes_before_a_parent_catches_the_failure() {
	let program = compile_object("async fn Child(): int {
		try { fail 7; } finally { fail 'cleanup'; }
	}
	async fn Outer(): int { return suspend Child(); }
	fn Main(args: [text]): int {
		try { return block Outer(); }
		catch err: text { return err == 'cleanup' ? 42 : 0; }
	}");
	assert_eq!(run_program(&program).unwrap(), Some(Value::Integer(42)));
}

#[test]
fn runtime_errors_keep_the_live_tablo_call_chain() {
	for (divisor, expected) in [
		(0, vec![("Divide", 2), ("Child", 5), ("Outer", 8), ("Main", 12)]),
		(1, vec![("Divide", 2), ("Outer", 9), ("Main", 12)]),
	] {
		let source = format!("fn Divide(d: int): int {{
			return 1 / d;
		}}
		async fn Child(): int {{
			return Divide({divisor});
		}}
		async fn Outer(): int {{
			var n: int = suspend Child();
			return Divide(n - 1);
		}}
		fn Main(args: [text]): int {{
			return block Outer();
		}}");
		let Err(TabloError::Runtime(error)) = run_program(&compile_object(&source)) else {
			panic!("Expected division by zero");
		};
		let trace: Vec<_> = error.stack_trace.iter().map(|frame| {
			let site = frame.source_location().unwrap();
			(site.body_name().unwrap(), site.line())
		}).collect();
		assert_eq!(trace, expected);
	}
}

#[test]
fn step_out_and_over_follow_the_waiting_caller() {
	let program = compile_object("async fn Child(): int {
		return 20;
	}
	async fn Outer(): int {
		var base: int = 20;
		var n: int = suspend Child();
		return base + n;
	}
	fn Main(args: [text]): int {
		var n: int = block Outer();
		return n + 2;
	}");
	let mut session = DebuggerSession::new(&program);
	session.set_breakpoints(session.resolve_source_breakpoints("async.tablo", &[2]));
	let stop = session.resume().unwrap();
	let paused = stop.paused_state().unwrap();
	assert_eq!(paused.stack_frames()[1].local("base").unwrap().value(), &Value::Integer(20));
	for (name, line) in [("Outer", 7), ("Main", 11)] {
		let stop = session.step_out().unwrap();
		let site = stop.paused_state().unwrap().current_frame().unwrap().source_location().unwrap();
		assert_eq!((site.body_name(), site.line()), (Some(name), line));
	}

	let mut session = DebuggerSession::new(&program);
	session.set_breakpoints(session.resolve_source_breakpoints("async.tablo", &[6]));
	session.resume().unwrap();
	let stop = session.step_over().unwrap();
	let frame = stop.paused_state().unwrap().current_frame().unwrap();
	assert_eq!(frame.source_location().unwrap().line(), 7);
	assert_eq!(frame.local("n").unwrap().value(), &Value::Integer(20));
}

#[test]
fn stepping_out_of_a_void_child_stops_before_the_callers_next_statement() {
	let program = compile_object("async fn Child() {
		var x: int = 1;
	}
	fn Main(args: [text]): int {
		block Child();
		return 42;
	}");
	let mut session = DebuggerSession::new(&program);
	session.set_breakpoints(session.resolve_source_breakpoints("async.tablo", &[2]));
	session.resume().unwrap();
	let stop = session.step_out().unwrap();
	let site = stop.paused_state().unwrap().current_frame().unwrap().source_location().unwrap();
	assert_eq!((site.body_name(), site.line()), (Some("Main"), 6));
}

#[test]
fn checks_async_call_contexts() {
	for (expression, diagnostic) in [
		("Answer()", "requires `block`"),
		("suspend Answer()", "async context"),
		("block Sync()", "synchronous"),
		("block len(args)", "async function"),
	] {
		let source = format!("async fn Answer(): int {{ return 42; }} fn Sync(): int {{ return 0; }}
			fn Main(args: [text]): int {{ return {expression}; }}");
		let error = tablo::run(source).unwrap_err();
		assert!(error.to_string().contains(diagnostic), "{error}");
	}
}

#[test]
fn catches_can_inspect_suppressed_values_and_their_logical_stacks() {
	let source = "async fn Leaf() {}
		async fn Worker() {
			try {
				try { suspend Leaf(); }
				finally { fail 41; }
			}
			finally { fail 'outer cleanup'; }
		}
		async fn Fault() { fail 'original'; }
		fn Main(args: [text]): int {
			try {
				transaction {
					try { async { Worker(); Fault(); } }
					catch err: text, details {
						if err != 'original' or len(details) != 2 { fail 'wrong failures'; }
						var detail: SuppressedFailure = details[1];
						if detail.value != 41 or details[2].value != 'outer cleanup' { fail 'wrong values'; }
						if len(detail.stackTrace) < 2 { fail 'missing trace'; }
					}
				}
			}
			catch err: text, details {
				if err == 'original' and len(details) == 2 { return 42; }
			}
			return 0;
		}";
	assert_eq!(run_program(&compile_object(source)).unwrap(), Some(Value::Integer(42)));

	let source = source.split("fn Main").next().unwrap().to_owned()
		+ "fn Main(args: [text]): int { async { Worker(); Fault(); } return 0; }";
	let Err(TabloError::Runtime(error)) = run_program(&compile_object(&source)) else {
		panic!("Expected a failure");
	};
	assert_eq!(error.message, "original");
	assert_eq!(error.suppressed.len(), 2);
	for (secondary, (line, message)) in error.suppressed.iter().zip([(5, "41"), (7, "outer cleanup")]) {
		let site = secondary.stack_frames()[0].source_location().unwrap();
		assert_eq!((site.body_name(), site.line(), secondary.message.as_str()), (Some("Worker"), line, message));
	}
	let diagnostic = TabloError::Runtime(error).to_string();
	assert!(diagnostic.contains("During cancellation cleanup: 41\nStack trace:\n  at Worker"));
}

#[test]
fn ordinary_failures_bind_an_empty_suppressed_list() {
	let source = "fn Main(args: [text]): int {
		try { fail 42; }
		catch n: int, details { return n + len(details); }
	}";
	assert_eq!(run_program(&compile_object(source)).unwrap(), Some(Value::Integer(42)));
}

#[test]
fn a_finally_already_in_progress_reports_only_failures_that_escape_cleanup() {
	let source = "async fn Leaf() {}
		async fn Worker() {
			try { return; }
			finally {
				suspend Leaf();
				try { fail 'handled'; } catch local: text {}
				fail 'close failed';
			}
		}
		async fn Fault() { fail 42; }
		fn Main(args: [text]): int {
			try { async { Worker(); Fault(); } }
			catch reason: int, details {
				if len(details) == 1 and details[1].value == 'close failed' { return reason; }
			}
			return 0;
		}";
	assert_eq!(run_program(&compile_object(source)).unwrap(), Some(Value::Integer(42)));
}
