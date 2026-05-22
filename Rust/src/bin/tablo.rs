use std::path::PathBuf;

use clap::Parser as ClapParser;
use tablo::run_file;

#[derive(ClapParser, Debug)]
#[command(name = "tablo")]
#[command(about = "Run compiled Tablo object files.")]
struct Args {
	#[arg(value_name = "INPUT")]
	input_path: PathBuf,
}

fn main() {
	let args = Args::parse();

	match run_file(&args.input_path) {
		Ok(Some(value)) => println!("{value}"),
		Ok(None) => {}
		Err(error) => {
			eprintln!("{error}");
			std::process::exit(1);
		}
	}
}
