use std::path::PathBuf;

use clap::Parser as ClapParser;
use tablo::compile_with_source_name_and_schema;
use tablo::compile_with_source_name;
use tablo::runtime_config::read_schema_catalog_from_runtime_config_path;
use tablo::schema_fixture::read_schema_catalog_from_path;

#[derive(ClapParser, Debug)]
#[command(name = "tabloc")]
#[command(about = "Compile Tablo source files to Tablo object files.")]
#[command(long_about = "Compile Tablo source files to Tablo object files.\n\nFor schema-aware database features, pass --schema with a limited SQL-like schema file, or use [schemas] entries in tablo.toml.\nThe current schema parser intentionally supports only a small subset of SQL-like syntax.")]
struct Args {
	#[arg(
		long = "config",
		value_name = "PATH",
		help = "Path to a TOML project config file. If omitted, `tabloc` will use `./tablo.toml` when present. Explicit --schema overrides config-file schema entries."
	)]
	config_path: Option<PathBuf>,

	#[arg(
		long = "schema",
		value_name = "PATH",
		help = "Path to a limited SQL-like schema file for schema-aware compilation."
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

	let config_path = match resolve_project_config_path(args.config_path.as_ref()) {
		Ok(path) => path,
		Err(error) => {
			eprintln!("{error}");
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
	else if let Some(config_path) = &config_path {
		let schema_catalog = match read_schema_catalog_from_runtime_config_path(config_path) {
			Ok(schema_catalog) => schema_catalog,
			Err(error) => {
				eprintln!("Failed to load schema configuration from `{}`: {}", config_path.display(), error.message);
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

fn default_project_config_path(current_dir: &std::path::Path) -> Option<PathBuf> {
	let path = current_dir.join("tablo.toml");

	if path.is_file() {
		Some(path)
	}
	else {
		None
	}
}

fn resolve_project_config_path(config_path: Option<&PathBuf>) -> Result<Option<PathBuf>, String> {
	if let Some(config_path) = config_path {
		return Ok(Some(config_path.clone()));
	}

	let current_dir = std::env::current_dir()
		.map_err(|error| format!("Failed to determine current directory while searching for `tablo.toml`: {error}"))?;
	Ok(default_project_config_path(&current_dir))
}

#[cfg(test)]
mod tests {
	use std::path::PathBuf;
	use std::time::{SystemTime, UNIX_EPOCH};

	use super::default_project_config_path;

	#[test]
	fn discovers_default_project_config_when_present() {
		let temp_dir = unique_temp_directory("tabloc_project_config_present");
		let config_path = temp_dir.join("tablo.toml");
		std::fs::create_dir_all(&temp_dir).unwrap();
		std::fs::write(&config_path, "[schemas]\n").unwrap();

		assert_eq!(default_project_config_path(&temp_dir), Some(config_path.clone()));

		let _ = std::fs::remove_file(&config_path);
		let _ = std::fs::remove_dir(&temp_dir);
	}

	#[test]
	fn does_not_discover_default_project_config_when_missing() {
		let temp_dir = unique_temp_directory("tabloc_project_config_missing");
		std::fs::create_dir_all(&temp_dir).unwrap();

		assert_eq!(default_project_config_path(&temp_dir), None);

		let _ = std::fs::remove_dir(&temp_dir);
	}

	fn unique_temp_directory(name: &str) -> PathBuf {
		let nanos = SystemTime::now()
			.duration_since(UNIX_EPOCH)
			.unwrap()
			.as_nanos();
		std::env::temp_dir().join(format!("{name}_{nanos}"))
	}
}
