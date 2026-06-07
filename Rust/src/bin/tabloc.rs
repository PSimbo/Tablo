use std::path::PathBuf;

use clap::Parser as ClapParser;
use tablo::compile_with_source_name_and_schema;
use tablo::compile_with_source_name;
use tablo::schema_fixture::read_schema_catalog_from_path;

#[derive(ClapParser, Debug)]
#[command(name = "tabloc")]
#[command(about = "Compile Tablo source files to Tablo object files.")]
#[command(long_about = "Compile Tablo source files to Tablo object files.\n\nFor schema-aware database features, pass --schema with a backend-neutral JSON schema fixture.\nDirect PostgreSQL/MySQL/SQLite DDL input is not supported by the CLI yet.")]
struct Args {
	#[arg(
		long = "schema",
		value_name = "PATH",
		help = "Path to a backend-neutral JSON schema fixture for schema-aware compilation. Direct DDL input is not supported yet."
	)]
	schema_fixture_path: Option<PathBuf>,

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

	let compile_result = if let Some(schema_fixture_path) = &args.schema_fixture_path {
		let schema_catalog = match read_schema_catalog_from_path(schema_fixture_path) {
			Ok(schema_catalog) => schema_catalog,
			Err(error) => {
				eprintln!("Failed to load schema fixture `{}`: {}", schema_fixture_path.display(), error.message);
				std::process::exit(1);
			}
		};

		compile_with_source_name_and_schema(
			source.as_str(),
			args.input_path.display().to_string(),
			&output_path,
			&schema_catalog,
		)
	}
	else {
		compile_with_source_name(source.as_str(), args.input_path.display().to_string(), &output_path)
	};

	if let Err(error) = compile_result {
		eprintln!("{}", error.format_with_source(&source));
		std::process::exit(1);
	}
}

fn default_output_path(input_path: &PathBuf) -> PathBuf {
	let mut output_path = input_path.clone();
	output_path.set_extension("tbo");
	output_path
}
