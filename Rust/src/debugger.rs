use std::collections::BTreeSet;

use crate::bytecode::InstructionSite;
use crate::bytecode::Program;
use crate::value::Value;
use crate::vm::VirtualMachine;
use crate::vm::VmError;
use crate::vm::VmExecutionState;
use crate::vm::VmStackFrame;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DebuggerStop {
	Completed(Option<Value>),
	Paused(PausedState),
}

impl DebuggerStop {
	pub fn completed_result(&self) -> Option<&Option<Value>> {
		match self {
			DebuggerStop::Completed(result) => Some(result),
			DebuggerStop::Paused(_) => None,
		}
	}

	pub fn paused_state(&self) -> Option<&PausedState> {
		match self {
			DebuggerStop::Completed(_) => None,
			DebuggerStop::Paused(state) => Some(state),
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PauseReason {
	Breakpoint,
	Step,
}

pub struct DebuggerSession<'a> {
	breakpoints: BTreeSet<InstructionBreakpoint>,
	program: &'a Program,
	skip_breakpoint_once: Option<InstructionBreakpoint>,
	started: bool,
	vm: VirtualMachine,
}

impl<'a> DebuggerSession<'a> {
	pub fn new(program: &'a Program) -> Self {
		Self {
			breakpoints: BTreeSet::new(),
			program,
			skip_breakpoint_once: None,
			started: false,
			vm: VirtualMachine::new(),
		}
	}

	pub fn resolve_source_breakpoints(&self, display_name: &str, lines: &[u32]) -> Vec<InstructionBreakpoint> {
		lines.iter()
			.filter_map(|line| {
				self.program
					.instruction_sites_for_source_line(display_name, *line)
					.into_iter()
					.next()
					.map(InstructionBreakpoint::from)
			})
			.collect()
	}

	pub fn resume(&mut self) -> Result<DebuggerStop, VmError> {
		self.ensure_started();

		loop {
			if let Some(breakpoint) = self.current_breakpoint() {
				if self.skip_breakpoint_once != Some(breakpoint) {
					self.skip_breakpoint_once = Some(breakpoint);
					return Ok(DebuggerStop::Paused(self.paused_state(PauseReason::Breakpoint)));
				}
			}

			self.skip_breakpoint_once = None;

			match self.vm.step(self.program)? {
				VmExecutionState::Completed(result) => return Ok(DebuggerStop::Completed(result)),
				VmExecutionState::Running => {}
			}
		}
	}

	pub fn set_breakpoints(&mut self, breakpoints: Vec<InstructionBreakpoint>) {
		self.breakpoints = breakpoints.into_iter().collect();
	}

	pub fn step_in(&mut self) -> Result<DebuggerStop, VmError> {
		self.ensure_started();
		self.skip_breakpoint_once = None;

		match self.vm.step(self.program)? {
			VmExecutionState::Completed(result) => Ok(DebuggerStop::Completed(result)),
			VmExecutionState::Running => Ok(DebuggerStop::Paused(self.paused_state(PauseReason::Step))),
		}
	}

	fn current_breakpoint(&self) -> Option<InstructionBreakpoint> {
		let (body_index, instruction_index) = self.vm.current_instruction_site(self.program)?;
		let breakpoint = InstructionBreakpoint {
			body_index,
			instruction_index,
		};

		self.breakpoints.contains(&breakpoint).then_some(breakpoint)
	}

	fn ensure_started(&mut self) {
		if !self.started {
			self.vm.begin_execution(self.program);
			self.started = true;
		}
	}

	fn paused_state(&self, reason: PauseReason) -> PausedState {
		PausedState {
			reason,
			stack_frames: self.vm.current_stack_frames(self.program),
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct InstructionBreakpoint {
	body_index: usize,
	instruction_index: usize,
}

impl InstructionBreakpoint {
	pub fn body_index(&self) -> usize {
		self.body_index
	}

	pub fn instruction_index(&self) -> usize {
		self.instruction_index
	}
}

impl From<InstructionSite> for InstructionBreakpoint {
	fn from(site: InstructionSite) -> Self {
		Self {
			body_index: site.body_index(),
			instruction_index: site.instruction_index(),
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PausedState {
	reason: PauseReason,
	stack_frames: Vec<VmStackFrame>,
}

impl PausedState {
	pub fn current_frame(&self) -> Option<&VmStackFrame> {
		self.stack_frames.first()
	}

	pub fn reason(&self) -> PauseReason {
		self.reason
	}

	pub fn stack_frames(&self) -> &[VmStackFrame] {
		&self.stack_frames
	}
}

#[cfg(test)]
mod tests {
	use crate::bytecode::SourceFileDebugInfo;
	use crate::compiler::Compiler;
	use crate::source::SourceText;
	use crate::syntax::lexer::Lexer;
	use crate::syntax::parser::Parser;
	use crate::value::Value;

	use super::DebuggerSession;
	use super::DebuggerStop;
	use super::PauseReason;

	fn compile_debug_program(source: &str, display_name: &str) -> crate::bytecode::Program {
		let source_text = SourceText::new(source);
		let mut lexer = Lexer::new(source_text.clone());
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		let program = parser.parse_program().unwrap();
		let mut program = Compiler::new().compile_program(&program).unwrap();
		program.debug_info_mut().attach_source_file(SourceFileDebugInfo::from_source(display_name, &source_text));
		program
	}

	#[test]
	fn pauses_at_source_breakpoint_with_visible_locals() {
		let source = "var x: int = 1;\nvar y: int = 2;\nx + y";
		let program = compile_debug_program(source, "example.tablo");
		let mut session = DebuggerSession::new(&program);
		let breakpoints = session.resolve_source_breakpoints("example.tablo", &[2]);
		assert_eq!(breakpoints.len(), 1);

		session.set_breakpoints(breakpoints);

		let stop = session.resume().unwrap();
		let paused = stop.paused_state().unwrap();
		assert_eq!(paused.reason(), PauseReason::Breakpoint);
		let frame = paused.current_frame().unwrap();
		assert_eq!(frame.source_location().unwrap().display_name(), Some("example.tablo"));
		assert_eq!(frame.source_location().unwrap().line(), 2);
		assert_eq!(frame.local("x").unwrap().value(), &Value::Integer(1));
		assert!(frame.local("y").is_none());
	}

	#[test]
	fn resumes_past_breakpoint_to_completion() {
		let source = "var x: int = 1;\nvar y: int = 2;\nx + y";
		let program = compile_debug_program(source, "example.tablo");
		let mut session = DebuggerSession::new(&program);
		let breakpoints = session.resolve_source_breakpoints("example.tablo", &[2]);
		session.set_breakpoints(breakpoints);

		assert!(matches!(session.resume().unwrap(), DebuggerStop::Paused(_)));

		let stop = session.resume().unwrap();
		assert_eq!(stop.completed_result(), Some(&Some(Value::Integer(3))));
	}

	#[test]
	fn steps_one_instruction_after_starting() {
		let source = "var x: int = 1;\nx";
		let program = compile_debug_program(source, "example.tablo");
		let mut session = DebuggerSession::new(&program);

		let stop = session.step_in().unwrap();
		let paused = stop.paused_state().unwrap();
		assert_eq!(paused.reason(), PauseReason::Step);
		assert_eq!(paused.current_frame().unwrap().source_location().unwrap().line(), 1);
	}
}
