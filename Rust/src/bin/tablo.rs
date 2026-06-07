use std::path::PathBuf;

use clap::Parser as ClapParser;
use tablo::run_file;
use tablo::run_file_with_database_config;
use tablo::vm::RuntimeDatabaseConfig;

#[derive(ClapParser, Debug)]
#[command(name = "tablo")]
#[command(about = "Run compiled Tablo object files.")]
struct Args {
	#[arg(
		long = "db",
		value_name = "NAME=CONNECTION_STRING",
		help = "Map a Tablo database name to a connection string. May be repeated."
	)]
	databases: Vec<String>,

	#[arg(value_name = "INPUT")]
	input_path: PathBuf,
}

fn main() {
	let args = Args::parse();
	let database_config = match build_database_config(&args.databases) {
		Ok(config) => config,
		Err(error) => {
			eprintln!("{error}");
			std::process::exit(1);
		}
	};

	let result = if args.databases.is_empty() {
		run_file(&args.input_path)
	}
	else {
		run_file_with_database_config(&args.input_path, database_config)
	};

	match result {
		Ok(_) => {}
		Err(error) => {
			eprintln!("{error}");
			std::process::exit(1);
		}
	}
}

fn build_database_config(mappings: &[String]) -> Result<RuntimeDatabaseConfig, String> {
	let mut config = RuntimeDatabaseConfig::new();

	for mapping in mappings {
		let (database_name, connection_string) = parse_database_mapping(mapping)?;
		config.set_database_connection_string(database_name, connection_string)?;
	}

	Ok(config)
}

fn parse_database_mapping(mapping: &str) -> Result<(&str, &str), String> {
	let (database_name, connection_string) = mapping.split_once('=').ok_or_else(|| {
		String::from("Database mappings must use the form `NAME=CONNECTION_STRING`.")
	})?;

	if database_name.is_empty() {
		return Err(String::from("Database mappings must include a database name before `=`."));
	}

	if connection_string.is_empty() {
		return Err(format!(
			"Database mapping for `{database_name}` must include a connection string after `=`."
		));
	}

	Ok((database_name, connection_string))
}

#[cfg(test)]
mod tests {
	use std::path::Path;

	use super::build_database_config;
	use super::parse_database_mapping;

	#[test]
	fn builds_runtime_database_config_from_cli_mappings() {
		let config = build_database_config(&[String::from("ExampleDb=sqlite:data/example.sqlite")]).unwrap();

		assert_eq!(
			config.sqlite_database_path("exampledb"),
			Some(Path::new("data/example.sqlite")),
		);
	}

	#[test]
	fn parses_database_mapping() {
		assert_eq!(
			parse_database_mapping("ExampleDb=sqlite:data/example.sqlite").unwrap(),
			("ExampleDb", "sqlite:data/example.sqlite"),
		);
	}

	#[test]
	fn rejects_database_mapping_without_equals() {
		let error = parse_database_mapping("ExampleDb").unwrap_err();

		assert_eq!(error, String::from("Database mappings must use the form `NAME=CONNECTION_STRING`."));
	}
}
