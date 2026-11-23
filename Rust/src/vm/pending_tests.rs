use super::*;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

fn program(source: &str) -> Program {
	let dir = tempfile::tempdir().unwrap();
	let path = dir.path().join("pending.tobj");
	crate::compile_with_source_name(source, "pending.tablo", &path).unwrap();
	crate::object_file::read_program_from_path(path).unwrap()
}

#[derive(Default)]
struct Gate {
	open: Cell<bool>,
	waker: RefCell<Option<Waker>>,
}

impl Gate {
	fn open(&self) {
		self.open.set(true);
		self.waker.borrow_mut().take().expect("Operation must already be parked").wake();
	}

	fn operation(self: Rc<Self>) -> impl Future<Output = OperationResult> {
		std::future::poll_fn(move |context| {
			if self.open.get() {
				return Poll::Ready(Ok(Some(Value::Integer(1))));
			}

			*self.waker.borrow_mut() = Some(context.waker().clone());

			Poll::Pending
		})
	}
}

#[test]
fn a_woken_child_resumes_while_its_sibling_is_still_pending() {
	let program = program("async fn Slow(): int { return 10 + len('slow'); }
		async fn Peer(): int { return 20 + len('peer'); }
		fn Main(args: [text]): int {
			return async {
				var a: task<int> = Slow();
				var b: task<int> = Peer();
				return suspend a + suspend b;
			};
		}");
	let mut vm = VirtualMachine::new();
	vm.begin_execution(&program);
	let slow = Rc::new(Gate::default());
	let peer = Rc::new(Gate::default());
	let mut events = Vec::new();
	for _ in 0..100 {
		let frame = vm.frames.last().unwrap();
		let code = vm.current_code_body(&program, frame);
		let index = frame.instruction_index;
		let instruction = &code.instructions[index];
		if matches!(instruction, Instruction::CallBuiltIn(BuiltInFunction::Len, 1)) && frame.pending.is_none() {
			let Some(Value::Text(name)) = vm.stack.pop() else {
				panic!("Missing native argument");
			};
			let gate = if name == "peer" {
				slow.open();
				peer.clone()
			}
			else {
				slow.clone()
			};
			events.push(name);
			vm.start_native_operation(gate.operation(), index).unwrap();
		}
		else if matches!(instruction, Instruction::Return) && vm.stack.last() == Some(&Value::Integer(11)) {
			events.push("slow completed".into());
			peer.open();
		}

		let Poll::Ready(step) = vm.poll_step(&program) else {
			panic!("Expected a runnable task");
		};
		if let VmExecutionState::Completed(result) = step.unwrap() {
			assert_eq!(result, Some(Value::Integer(32)));
			assert_eq!(events, ["slow", "peer", "slow completed"]);
			return;
		}
	}
	panic!("Tasks did not complete");
}

#[test]
fn sibling_failure_cancels_a_pending_grandchild_before_reaching_the_parent() {
	let program = program("async fn Fault(): int { fail 'original'; }
		async fn FirstTask(): int { return suspend Fault(); }
		async fn Grandchild(): int {
			try { return len('wait'); }
			catch err: text { return len('unexpected catch'); }
			finally { len('grandchild cleanup'); }
		}
		async fn Worker(): int {
			try { return suspend Grandchild(); }
			finally { len('worker cleanup'); }
		}
		fn Main(args: [text]): int {
			try {
				return async {
					var a: task<int> = FirstTask();
					Worker();
					return suspend a;
				};
			} catch err: text {
				if err == 'original' { return len('parent'); }
				return 0;
			}
		}");
	let mut vm = VirtualMachine::new();
	vm.begin_execution(&program);
	let gate = Rc::new(Gate::default());
	let mut events = Vec::new();
	for _ in 0..120 {
		let frame = vm.frames.last().unwrap();
		let index = frame.instruction_index;
		let code = vm.current_code_body(&program, frame);
		if matches!(code.instructions[index], Instruction::CallBuiltIn(BuiltInFunction::Len, 1))
			&& frame.pending.is_none() {
			let Some(Value::Text(marker)) = vm.stack.last() else {
				panic!("Missing marker");
			};
			events.push(marker.clone());
			if marker == "wait" {
				vm.stack.pop();
				vm.start_native_operation(gate.clone().operation(), index).unwrap();
			}
		}

		let Poll::Ready(step) = vm.poll_step(&program) else {
			panic!("Expected a runnable task");
		};
		if let VmExecutionState::Completed(result) = step.unwrap() {
			assert_eq!(result, Some(Value::Integer(6)));
			assert_eq!(events, ["wait", "grandchild cleanup", "worker cleanup", "parent"]);
			assert_eq!(Rc::strong_count(&gate), 1);
			return;
		}
	}
	panic!("Cancellation did not complete");
}

#[test]
fn idle_executor_receives_a_native_wakeup_from_another_thread() {
	use std::sync::atomic::{AtomicBool, Ordering};
	use std::sync::mpsc;

	let program = program("fn Main(args: [text]): int { return 40 + len(args); }");
	let mut vm = VirtualMachine::new();
	vm.begin_execution(&program);
	loop {
		let frame = vm.frames.last().unwrap();
		let index = frame.instruction_index;
		if matches!(vm.current_code_body(&program, frame).instructions[index], Instruction::CallBuiltIn(BuiltInFunction::Len, 1)) {
			vm.stack.pop();
			let (sender, receiver) = mpsc::sync_channel(1);
			let done = Arc::new(AtomicBool::new(false));
			let completed = done.clone();
			let future = std::future::poll_fn(move |context| {
				if done.load(Ordering::Acquire) {
					return Poll::Ready(Ok(Some(Value::Integer(2))));
				}
				if !context.waker().will_wake(Waker::noop()) {
					sender.try_send(context.waker().clone()).unwrap();
				}
				Poll::Pending
			});
			vm.start_native_operation(future, index).unwrap();
			assert!(vm.poll_step(&program).is_pending());
			assert!(vm.poll_step(&program).is_pending());
			let waker = receiver.try_recv().unwrap();
			let worker = std::thread::spawn(move || {
				completed.store(true, Ordering::Release);
				waker.wake();
			});
			loop {
				if let VmExecutionState::Completed(value) = vm.step(&program).unwrap() {
					worker.join().unwrap();
					assert_eq!(value, Some(Value::Integer(42)));
					return;
				}
			}
		}
		vm.step(&program).unwrap();
	}
}
