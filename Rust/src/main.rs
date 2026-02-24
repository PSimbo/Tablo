use clap::{CommandFactory, Parser as ClapParser};
use std::io;
use std::io::Write;

mod lexer;

use lexer::Lexer;

#[derive(ClapParser, Debug)]
struct Args {
	#[arg(value_name = "FILENAME")]
	file_name: Option<String>,
	#[arg(long = "ast", action)]
	print_ast: bool,
	#[arg(long = "tokens", action)]
	print_tokens: bool,
}

fn main() {
	/* We want to have a high-degree of control over how runtime errors are
	   handled. To that end, we disable Rust's standard panic hook. */
	suppress_rust_panic_hook();

	let args = Args::parse();

	let file_name = if let Some(file_name) = args.file_name.as_deref() {
		file_name
	}
	else {
		let mut cmd = Args::command();
		cmd.print_help().unwrap();
		return;
	};

	let source = std::fs::read_to_string(file_name).expect("Failed to read file at specified path.");

	if args.print_tokens {
		print_tokens(&source);
		return;
	}

	if args.print_ast {
		// print_ast(&source);
		return;
	}
}

fn print_tokens(source: &str) {
	let mut stdout = io::stdout();
	stdout.flush().unwrap();

	let mut lexer = Lexer::new(source);

	while !lexer.end_of_file() {
		let token = lexer.next();

		println!("{}: `{}`", token.token_type, token.to_str(&lexer))
	}
}

fn suppress_rust_panic_hook() {
	std::panic::set_hook(Box::new(|info: &std::panic::PanicHookInfo| {
		let msg = info.payload().downcast_ref::<String>().cloned().unwrap_or("Unhandled exception".to_string());
		eprintln!("{}", msg);
	}))
}
