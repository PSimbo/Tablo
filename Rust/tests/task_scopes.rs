#![cfg(feature = "compiler")]

use tablo::debugger::DebuggerSession;
use tablo::object_file::read_program_from_path;
use tablo::value::Value;
use tablo::{TabloError, compile_with_source_name, run_program};

#[test]
fn task_results_survive_copies_and_repeated_waits() {
	let source = "async fn Square(n: int): int { return n * n; }
		fn Main(args: [text]): int {
			var base: int = 3;
			var result: int = async {
				var square: task<int> = Square(base);
				var second: task<int> = Square(4);
				var copy: task<int> = square;
				base = 5;
				return suspend second + suspend copy + suspend square;
			};
			return result + base;
		}";
	assert_eq!(tablo::run(source).unwrap(), Some(Value::Integer(39)));
}

#[test]
fn scope_joins_unobserved_children_and_contains_early_returns() {
	let source = "async fn Bad(): int { fail 'unobserved'; }
		fn Main(args: [text]): int {
			try {
				var n: int = async { Bad(); if true { return 2; } return 0; };
				return n;
			} catch message: text { return message == 'unobserved' ? 42 : 0; }
		}";
	assert_eq!(tablo::run(source).unwrap(), Some(Value::Integer(42)));
}

#[test]
fn child_failure_identifies_spawn_and_debugger_steps_out_at_wait() {
	let source = "async fn Child(n: int): int {
		return 12 / n;
	}
	fn Main(args: [text]): int {
		return async {
			var child: task<int> = Child(2);
			var other: task<int> = Child(1);
			var result: int = suspend child;
			return result + suspend other;
		};
	}";
	let directory = tempfile::tempdir().unwrap();
	let filename = directory.path().join("test.tobj");
	compile_with_source_name(source, "tasks.tablo", &filename).unwrap();
	let program = read_program_from_path(&filename).unwrap();
	let mut debugger = DebuggerSession::new(&program);
	debugger.set_breakpoints(debugger.resolve_source_breakpoints("tasks.tablo", &[2]));
	let stop = debugger.resume().unwrap();
	assert_eq!(stop.paused_state().unwrap().stack_frames()[1].source_location().unwrap().line(), 8);
	debugger.clear_breakpoints();
	let stop = debugger.step_out().unwrap();
	assert_eq!(stop.paused_state().unwrap().current_frame().unwrap().source_location().unwrap().line(), 9);
	assert_eq!(run_program(&program).unwrap(), Some(Value::Integer(18)));

	compile_with_source_name(source.replace("Child(2)", "Child(0)"), "tasks.tablo", &filename).unwrap();
	let Err(TabloError::Runtime(error)) = run_program(&read_program_from_path(filename).unwrap()) else { panic!("Expected failure") };
	assert_eq!(error.stack_trace[0].source_location().unwrap().line(), 2);
	assert_eq!(error.stack_trace[1].source_location().unwrap().line(), 6);
	assert_eq!(error.stack_trace[1].awaited_at.as_ref().unwrap().line(), 8);
	assert!(TabloError::Runtime(error).to_string().contains("awaited at tasks.tablo:8:"));
}

#[test]
fn handles_cannot_escape_or_borrow_caller_locals() {
	for (body, diagnostic) in [
		("var h: task<int> = Child(); var hidden: any = h;", "task handle"),
		("var h: task<int> = Child(); fail h;", "task handle"),
		("var x: int = 1; Borrow(&x);", "cannot borrow"),
		("var x: int = 1; suspend x;", "requires a task handle"),
	] {
		let source = format!("async fn Child(): int {{ return 1; }}
			async fn Borrow(x: &int): int {{ return x; }}
			fn Main(args: [text]): int {{ async {{ {body} }} return 0; }}");
		let error = tablo::run(&source).unwrap_err().to_string();
		assert!(error.contains(diagnostic), "{error}");
	}
}

#[test]
fn stepping_over_a_wait_skips_the_other_runnable_child() {
	let source = "async fn Square(n: int): int { return n * n; }
	fn Main(args: [text]): int {
		return async {
			Square(3);
			var child: task<int> = Square(4);
			var result: int = suspend child;
			return result;
		};
	}";
	let directory = tempfile::tempdir().unwrap();
	let file = directory.path().join("step.tobj");
	compile_with_source_name(source, "step.tablo", &file).unwrap();
	let program = read_program_from_path(file).unwrap();
	let mut debugger = DebuggerSession::new(&program);
	debugger.set_breakpoints(debugger.resolve_source_breakpoints("step.tablo", &[6]));
	let stop = debugger.resume().unwrap();
	assert_eq!(stop.paused_state().unwrap().current_frame().unwrap().source_location().unwrap().line(), 6);
	debugger.clear_breakpoints();
	let stop = debugger.step_over().unwrap();
	let frame = stop.paused_state().unwrap().current_frame().unwrap();
	assert_eq!(frame.source_location().unwrap().line(), 7);
}

#[test]
fn async_block_returns_accept_the_contextual_result_type() {
	let source = "fn Main(args: [text]): int {
		var x: float = async { if true { return 2; } return 1.25; };
		var numbers: [int] = async { return []; };
		return int(x) + len(numbers);
	}";
	assert_eq!(tablo::run(source).unwrap(), Some(Value::Integer(2)));
}

#[test]
fn catching_a_failed_wait_still_fails_the_owning_scope() {
	let source = "async fn Bad(): int { fail 'initial failure'; }
		fn Main(args: [text]): int {
			var effect: int = 0;
			try {
				async {
					var child: task<int> = Bad();
					try { suspend child; } catch local: text { effect += 1; }
				}
			} catch outer: text {
				if outer == 'initial failure' { effect += 10; }
			}
			return effect;
		}";
	assert_eq!(tablo::run(source).unwrap(), Some(Value::Integer(11)));
}

#[test]
fn nested_async_expression_keeps_its_return_and_function_scope() {
	let source = "fn Main(args: [text]): int {
		return async {
			async fn Answer(): int { return 41; }
			var taskValue: task<int> = Answer();
			var inner: int = async { return suspend taskValue; };
			return inner + 1;
		};
	}";
	assert_eq!(tablo::run(source).unwrap(), Some(Value::Integer(42)));
}

#[test]
fn source_steps_follow_the_selected_frame_across_a_sibling_switch() {
	let source = "async fn Leaf(): int { return 2; }
	async fn Waiting(): int {
		var base: int = 40;
		var value: int = suspend Leaf();
		return base + value;
	}
	async fn Peer(): int {
		return 3;
	}
	fn Main(args: [text]): int {
		return async {
			var child: task<int> = Waiting();
			Peer();
			return suspend child;
		};
	}";
	let directory = tempfile::tempdir().unwrap();
	let file = directory.path().join("step.tobj");
	compile_with_source_name(source, "step.tablo", &file).unwrap();
	let program = read_program_from_path(file).unwrap();
	let mut debugger = DebuggerSession::new(&program);
	debugger.set_breakpoints(debugger.resolve_source_breakpoints("step.tablo", &[4, 8]));
	debugger.resume().unwrap();
	let stop = debugger.step_over().unwrap();
	let stack = stop.paused_state().unwrap().stack_frames();
	assert_eq!(stack[0].source_location().unwrap().body_name(), Some("Peer"));
	assert_eq!(stack[1].source_location().unwrap().body_name(), Some("Main"));
	debugger.clear_breakpoints();
	let stop = debugger.step_out().unwrap();
	let site = stop.paused_state().unwrap().current_frame().unwrap().source_location().unwrap();
	assert_eq!((site.body_name(), site.line()), (Some("Main"), 15));

	let mut debugger = DebuggerSession::new(&program);
	debugger.set_breakpoints(debugger.resolve_source_breakpoints("step.tablo", &[4]));
	debugger.resume().unwrap();
	debugger.clear_breakpoints();
	let stop = debugger.step_over().unwrap();
	let frame = stop.paused_state().unwrap().current_frame().unwrap();
	assert_eq!(frame.source_location().unwrap().line(), 5);
	assert_eq!(frame.local("base").unwrap().value(), &Value::Integer(40));
	assert_eq!(frame.local("value").unwrap().value(), &Value::Integer(2));
	assert_eq!(debugger.resume().unwrap().completed_result(), Some(&Some(Value::Integer(42))));
}

#[test]
fn step_in_follows_the_requested_handle_and_failures_keep_its_ancestry() {
	let source = "async fn Other(): int { return 3; }
	async fn Fault(): int { return 10 / 0; }
	async fn Target(): int { return suspend Fault(); }
	fn Main(args: [text]): int {
		return async {
			Other();
			var targetTask: task<int> = Target();
			Other();
			return suspend targetTask;
		};
	}";
	let directory = tempfile::tempdir().unwrap();
	let file = directory.path().join("trace.tobj");
	compile_with_source_name(source, "trace.tablo", &file).unwrap();
	let program = read_program_from_path(file).unwrap();
	let mut debugger = DebuggerSession::new(&program);
	debugger.set_breakpoints(debugger.resolve_source_breakpoints("trace.tablo", &[9]));
	debugger.resume().unwrap();
	debugger.clear_breakpoints();
	let stop = debugger.step_in().unwrap();
	let frame = stop.paused_state().unwrap().current_frame().unwrap();
	assert_eq!(frame.source_location().unwrap().body_name(), Some("Target"));

	let error = debugger.resume().unwrap_err();
	let lines: Vec<_> = error.stack_frames().iter().map(|frame| frame.source_location().unwrap().line()).collect();
	assert_eq!(lines, [2, 3, 7]);
	assert_eq!(error.stack_frames()[2].awaited_at.as_ref().unwrap().line(), 9);
}

#[test]
fn commit_rethrows_the_directly_awaited_childs_original_failure_and_trace() {
	let source = "async fn Bad() {
		fail 42;
	}
	fn Main(args: [text]): int {
		transaction {
			try { block Bad(); }
			catch n: int { if n != 42 { fail 'wrong payload'; } }
		}
		return 0;
	}";

	let dir = tempfile::tempdir().unwrap();
	let object = dir.path().join("failure.tobj");
	compile_with_source_name(source, "failure.tablo", &object).unwrap();
	let program = read_program_from_path(object).unwrap();
	let Err(TabloError::Runtime(error)) = run_program(&program) else {
		panic!("Expected the transaction to fail at commit");
	};

	let lines: Vec<_> = error.stack_frames().iter().map(|frame| frame.source_location().unwrap().line()).collect();
	assert_eq!(lines, [2, 6]);
}

#[test]
fn a_child_that_handles_its_own_failure_can_commit() {
	let source = "fn Bad() { fail 42; }
		async fn Recover(): int {
			try { Bad(); }
			catch n: int { return n; }
			return 0;
		}
		fn Main(args: [text]): int {
			transaction { return block Recover(); }
		}";

	assert_eq!(tablo::run(source).unwrap(), Some(Value::Integer(42)));
}
