use std::io::Write;
use std::path::PathBuf;

use clap::Parser as ClapParser;
use colored::Colorize;
use tablo::TabloError;
use tablo::bytecode::Program;
use tablo::bytecode::SourceLocation;
use tablo::debugger::DebuggerSession;
use tablo::debugger::DebuggerStop;
use tablo::object_file::read_program_from_path;
use tablo::vm::VmStackFrame;

#[derive(ClapParser, Debug)]
#[command(name = "tablodbg")]
#[command(about = "Prototype interactive debugger for compiled Tablo object files.")]
#[command(after_help = "Interactive commands:\n  continue | c               Resume execution\n  step-in | s | si           Step into the next instruction\n  step-over | n              Step over the current call or instruction\n  step-out | finish | fin | o Run until the current frame returns\n  break <SOURCE:LINE> | b    Add a breakpoint after launch\n  breakpoints | bl           List the current breakpoints\n  clear-breakpoints | bc     Remove all breakpoints\n  stack | bt | where         Show the current stack trace\n  locals | l                 Show locals in the current frame\n  help | h | ?               Show debugger command help\n  quit | q | exit            Exit the debugger")]
struct Args {
	#[arg(value_name = "INPUT")]
	input_path: PathBuf,
	#[arg(short = 'b', long = "breakpoint", value_name = "SOURCE:LINE")]
	breakpoints: Vec<String>,
}

enum DebugCommand {
	AddBreakpoint(String),
	Breakpoints,
	ClearBreakpoints,
	Continue,
	Help,
	Locals,
	Quit,
	Stack,
	StepIn,
	StepOut,
	StepOver,
}

fn main() {
	let args = Args::parse();
	let program = match read_program_from_path(&args.input_path) {
		Ok(program) => program,
		Err(error) => {
			eprintln!("{}", TabloError::ObjectFile(error));
			std::process::exit(1);
		}
	};

	let mut session = DebuggerSession::new(&program);

	match resolve_breakpoints(&program, &session, &args.breakpoints) {
		Ok(breakpoints) => session.set_breakpoints(breakpoints),
		Err(message) => {
			eprintln!("{message}");
			std::process::exit(1);
		}
	}

	let initial_stop = if args.breakpoints.is_empty() {
		match pause_at_program_start(&mut session) {
			Ok(stop) => stop,
			Err(error) => {
				eprintln!("{error}");
				std::process::exit(1);
			}
		}
	}
	else {
		match session.resume() {
			Ok(stop) => stop,
			Err(error) => {
				eprintln!("{}", TabloError::Runtime(error));
				std::process::exit(1);
			}
		}
	};

	run_debug_loop(&program, &mut session, initial_stop);
}

fn format_location(location: &SourceLocation) -> String {
	let position = match location.display_name() {
		Some(display_name) => format!("{display_name}:{}:{}", location.line(), location.column()),
		None => format!("line {}, column {}", location.line(), location.column()),
	};

	match location.body_name() {
		Some(body_name) => format!("{body_name} ({position})"),
		None => position,
	}
}

fn parse_breakpoint_spec(spec: &str) -> Result<(&str, u32), String> {
	let Some((source_name, line)) = spec.rsplit_once(':') else {
		return Err(format!(
			"Invalid breakpoint `{spec}`. Expected the form `SOURCE:LINE`, for example `example.tablo:12`."
		));
	};

	let line = line.parse::<u32>().map_err(|_| {
		format!("Invalid breakpoint `{spec}`. The line number must be a positive integer.")
	})?;

	if line == 0 {
		return Err(format!("Invalid breakpoint `{spec}`. Line numbers are 1-based."));
	}

	Ok((source_name, line))
}

fn parse_command(input: &str) -> Result<DebugCommand, String> {
	match input.trim() {
		"" | "n" | "next" | "step-over" => Ok(DebugCommand::StepOver),
		"?" | "h" | "help" => Ok(DebugCommand::Help),
		"bc" | "clear-breakpoints" => Ok(DebugCommand::ClearBreakpoints),
		"bl" | "breakpoints" => Ok(DebugCommand::Breakpoints),
		"c" | "continue" => Ok(DebugCommand::Continue),
		"l" | "locals" => Ok(DebugCommand::Locals),
		"o" | "finish" | "fin" | "so" | "step-out" => Ok(DebugCommand::StepOut),
		"q" | "quit" | "exit" => Ok(DebugCommand::Quit),
		"s" | "si" | "step" | "step-in" => Ok(DebugCommand::StepIn),
		"bt" | "stack" | "where" => Ok(DebugCommand::Stack),
		command if command.starts_with("b ") || command.starts_with("break ") => {
			let spec = command.split_once(' ').map(|(_, spec)| spec.trim()).unwrap_or_default();
			if spec.is_empty() {
				Err(String::from("Breakpoint command requires a `SOURCE:LINE` argument, for example `b example.tablo:12`."))
			}
			else {
				Ok(DebugCommand::AddBreakpoint(spec.to_string()))
			}
		}
		command => Err(format!("Unknown command `{command}`. Type `help` to see the available commands.")),
	}
}

fn print_completion(stop: &DebuggerStop) {
	if let Some(result) = stop.completed_result() {
		match result {
			Some(result) => println!("{} {result}", "Program completed with result:".green()),
			None => println!("{}", "Program completed.".green()),
		}
	}
}

fn print_current_frame(frame: &VmStackFrame) {
	match frame.source_location() {
		Some(location) => println!("{} {}", "Paused at".yellow(), format_location(location).bold()),
		None => println!("{} instruction {}", "Paused at".yellow(), frame.instruction_index()),
	}
}

fn print_help() {
	println!("{}", "Commands:".bold());
	println!("  break <SOURCE:LINE> / b    Add a breakpoint, e.g. `b example.tablo:12`");
	println!("  breakpoints / bl           List the current breakpoints");
	println!("  clear-breakpoints / bc     Remove all breakpoints");
	println!("  continue / c               Resume execution");
	println!("  step-in / s / si           Step into the next instruction");
	println!("  step-over / n              Step over the current call or instruction");
	println!("  step-out / finish / fin / o Run until the current frame returns");
	println!("  stack / bt                 Show the current stack trace");
	println!("  locals / l                 Show locals in the current frame");
	println!("  help / h / ?               Show this help");
	println!("  quit / q                   Exit the debugger");
}

fn print_locals(frame: &VmStackFrame) {
	if frame.locals().is_empty() {
		println!("{}", "No visible locals in the current frame.".dimmed());
		return;
	}

	println!("{}", "Locals:".bold());

	for local in frame.locals() {
		let mutability = if local.is_const() { "const" } else { "var" };
		println!(
			"  {} {}: {} = {}",
			mutability.dimmed(),
			local.name().bold(),
			local.declared_type().cyan(),
			local.value()
		);
	}
}

fn print_stack(paused: &tablo::debugger::PausedState) {
	println!("{}", "Stack trace:".bold());

	for frame in paused.stack_frames() {
		match frame.source_location() {
			Some(location) => println!("  at {}", format_location(location)),
			None => println!("  at instruction {}", frame.instruction_index()),
		}
	}
}

fn print_breakpoints(session: &DebuggerSession<'_>, program: &Program) {
	let breakpoints = session.breakpoints();

	if breakpoints.is_empty() {
		println!("{}", "No breakpoints are currently set.".dimmed());
		return;
	}

	println!("{}", "Breakpoints:".bold());

	for breakpoint in breakpoints {
		match program.debug_location(breakpoint.body_index(), breakpoint.instruction_index()) {
			Some(location) => println!("  {}", format_location(&location)),
			None => println!(
				"  body {}, instruction {}",
				breakpoint.body_index(),
				breakpoint.instruction_index()
			),
		}
	}
}

fn prompt_command() -> Result<DebugCommand, String> {
	print!("{}", "tablodbg> ".bold());
	let _ = std::io::stdout().flush();

	let mut line = String::new();
	std::io::stdin()
		.read_line(&mut line)
		.map_err(|error| format!("Failed to read debugger input: {error}"))?;

	parse_command(&line)
}

fn resolve_breakpoints(
	program: &Program,
	session: &DebuggerSession<'_>,
	breakpoint_specs: &[String],
) -> Result<Vec<tablo::debugger::InstructionBreakpoint>, String> {
	let mut breakpoints = Vec::new();

	for spec in breakpoint_specs {
		let (source_name, line) = parse_breakpoint_spec(spec)?;
		let resolved = session.resolve_source_breakpoints(source_name, &[line]);

		if resolved.is_empty() {
			let available_sources: Vec<_> = program.debug_info()
				.source_files()
				.iter()
				.map(|source_file| source_file.display_name().to_string())
				.collect();

			let sources_hint = if available_sources.is_empty() {
				String::from("No source-file debug metadata is present in this object file.")
			}
			else {
				format!("Known source files: {}", available_sources.join(", "))
			};

			return Err(format!(
				"Could not resolve breakpoint `{spec}` to an executable instruction. {sources_hint}"
			));
		}

		breakpoints.extend(resolved);
	}

	Ok(breakpoints)
}

fn pause_at_program_start(session: &mut DebuggerSession<'_>) -> Result<DebuggerStop, TabloError> {
	let first = session.step_in().map_err(TabloError::Runtime)?;

	let should_step_again = first
		.paused_state()
		.and_then(|paused| paused.current_frame())
		.and_then(|frame| frame.source_location())
		.and_then(|location| location.body_name())
		!= Some("Main");

	if should_step_again {
		session.step_in().map_err(TabloError::Runtime)
	}
	else {
		Ok(first)
	}
}

fn run_debug_loop(program: &Program, session: &mut DebuggerSession<'_>, mut stop: DebuggerStop) {
	loop {
		match &stop {
			DebuggerStop::Completed(_) => {
				print_completion(&stop);
				return;
			}
			DebuggerStop::Paused(paused) => {
				if let Some(frame) = paused.current_frame() {
					print_current_frame(frame);
				}
			}
		}

		let command = match prompt_command() {
			Ok(command) => command,
			Err(message) => {
				eprintln!("{message}");
				continue;
			}
		};

		stop = match command {
			DebugCommand::AddBreakpoint(spec) => {
				let specs = vec![spec];
				let breakpoints = match resolve_breakpoints(program, session, &specs) {
					Ok(breakpoints) => breakpoints,
					Err(message) => {
						eprintln!("{message}");
						continue;
					}
				};
				session.add_breakpoints(breakpoints);
				print_breakpoints(session, program);
				continue;
			}
			DebugCommand::Breakpoints => {
				print_breakpoints(session, program);
				continue;
			}
			DebugCommand::ClearBreakpoints => {
				session.clear_breakpoints();
				println!("{}", "Cleared all breakpoints.".dimmed());
				continue;
			}
			DebugCommand::Continue => match session.resume() {
				Ok(stop) => stop,
				Err(error) => {
					eprintln!("{}", TabloError::Runtime(error));
					return;
				}
			},
			DebugCommand::Help => {
				print_help();
				continue;
			}
			DebugCommand::Locals => {
				if let Some(paused) = stop.paused_state() {
					if let Some(frame) = paused.current_frame() {
						print_locals(frame);
					}
				}
				continue;
			}
			DebugCommand::Quit => return,
			DebugCommand::Stack => {
				if let Some(paused) = stop.paused_state() {
					print_stack(paused);
				}
				continue;
			}
			DebugCommand::StepIn => match session.step_in() {
				Ok(stop) => stop,
				Err(error) => {
					eprintln!("{}", TabloError::Runtime(error));
					return;
				}
			},
			DebugCommand::StepOut => match session.step_out() {
				Ok(stop) => stop,
				Err(error) => {
					eprintln!("{}", TabloError::Runtime(error));
					return;
				}
			}
			DebugCommand::StepOver => match session.step_over() {
				Ok(stop) => stop,
				Err(error) => {
					eprintln!("{}", TabloError::Runtime(error));
					return;
				}
			},
		};
	}
}
