use std::path::PathBuf;

use clap::Parser;
use tablo::executable::{ExecutableBuildOptions, ExecutableBuildError, RuntimeMode, compile_executable};

#[derive(Parser)]
#[command(about = "Build an experimental Tablo executable on Linux.")]
struct Args {
	input: PathBuf,
	#[arg(long, default_value = "embedded")]
	runtime: RuntimeMode,
	#[arg(short, long)]
	output: PathBuf,
	#[arg(long, help = "Runtime source directory containing Cargo.toml, Cargo.lock, and src/")]
	runtime_dir: PathBuf,
	#[arg(long, help = "Writable directory for reusable native builds")]
	cache_dir: PathBuf,
	#[arg(long)]
	offline: bool,
	#[arg(long)]
	config: Option<PathBuf>,
}

fn main() {
	if let Err(error) = build(Args::parse()) {
		eprintln!("{error}");
		std::process::exit(1);
	}
}

fn build(args: Args) -> Result<(), String> {
	let source = std::fs::read_to_string(&args.input).map_err(|error| error.to_string())?;
	let config = args.config.or_else(|| tablo::utils::existing_child_path(std::path::Path::new("."), "tablo.toml"));
	let schema = config.map(tablo::runtime_config::read_schema_catalog_from_runtime_config_path)
		.transpose().map_err(|error| error.message)?;
	let options = ExecutableBuildOptions { runtime_dir: args.runtime_dir, cache_dir: args.cache_dir, offline: args.offline, debug_info: tablo::bytecode::DebugInfoLevel::Lines, runtime: args.runtime };
	let report = compile_executable(&source, &args.input, &args.output, schema.as_ref(), &options)
		.map_err(|error| match error {
			ExecutableBuildError::Compile(error) => error.format_with_source_name(&source, Some(&args.input.to_string_lossy())),
			ExecutableBuildError::Build(message) => message,
		})?;
	for warning in report.warnings {
		eprintln!("{}", warning.format_with_source_name(&source, Some(&args.input.to_string_lossy())));
	}
	Ok(())
}
