use std::path::PathBuf;

use clap::Parser as ClapParser;
use tablo::compile;

#[derive(ClapParser, Debug)]
#[command(name = "tabloc")]
#[command(about = "Compile Tablo source files to Tablo object files.")]
struct Args {
	#[arg(value_name = "INPUT")]
	input_path: PathBuf,
	#[arg(value_name = "OUTPUT")]
	output_path: Option<PathBuf>,
}

fn main() {
	let args = Args::parse();
	let output_path = args.output_path.unwrap_or_else(|| default_output_path(&args.input_path));

	let source = match std::fs::read_to_string(&args.input_path) {
		Ok(source) => source,
		Err(error) => {
			eprintln!("Failed to read `{}`: {error}", args.input_path.display());
			std::process::exit(1);
		}
	};

	if let Err(error) = compile(source, &output_path) {
		eprintln!("{error}");
		std::process::exit(1);
	}
}

fn default_output_path(input_path: &PathBuf) -> PathBuf {
	let mut output_path = input_path.clone();
	output_path.set_extension("tbo");
	output_path
}
