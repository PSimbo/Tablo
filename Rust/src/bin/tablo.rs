use std::path::PathBuf;

use clap::Parser as ClapParser;
use tablo::run_file;
use tablo::run_file_with_database_config;
use tablo::runtime_config::read_runtime_database_config_from_path;
use tablo::utils::existing_child_path;
use tablo::vm::RuntimeDatabaseConfig;

#[derive(ClapParser, Debug)]
#[command(name = "tablo")]
#[command(about = "Run compiled Tablo object files.")]
struct Args {
	#[arg(
		long = "config",
		value_name = "PATH",
		help = "Path to a TOML runtime config file. If omitted, `tablo` will use `./tablo.toml` when present. Command-line --db mappings override config-file entries."
	)]
	config_path: Option<PathBuf>,

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
	let config_path = match resolve_runtime_config_path(args.config_path.as_ref()) {
		Ok(path) => path,
		Err(error) => {
			eprintln!("{error}");
			std::process::exit(1);
		}
	};
	let database_config = match build_database_config(config_path.as_ref(), &args.databases) {
		Ok(config) => config,
		Err(error) => {
			eprintln!("{error}");
			std::process::exit(1);
		}
	};

	let has_database_config = config_path.is_some() || !args.databases.is_empty();

	let result = if has_database_config {
		run_file_with_database_config(&args.input_path, database_config)
	}
	else {
		run_file(&args.input_path)
	};

	match result {
		Ok(_) => {}
		Err(error) => {
			eprintln!("{error}");
			std::process::exit(1);
		}
	}
}

fn build_database_config(
	config_path: Option<&PathBuf>,
	mappings: &[String],
) -> Result<RuntimeDatabaseConfig, String> {
	let mut config = if let Some(config_path) = config_path {
		read_runtime_database_config_from_path(config_path)
			.map_err(|error| format!("Failed to load runtime config `{}`: {}", config_path.display(), error.message))?
	}
	else {
		RuntimeDatabaseConfig::new()
	};

	for mapping in mappings {
		let (database_name, connection_string) = parse_database_mapping(mapping)?;
		config.set_database_connection_string(database_name, connection_string)?;
	}

	Ok(config)
}

fn default_runtime_config_path(current_dir: &std::path::Path) -> Option<PathBuf> {
	existing_child_path(current_dir, "tablo.toml")
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

fn resolve_runtime_config_path(config_path: Option<&PathBuf>) -> Result<Option<PathBuf>, String> {
	if let Some(config_path) = config_path {
		return Ok(Some(config_path.clone()));
	}

	let current_dir = std::env::current_dir()
		.map_err(|error| format!("Failed to determine current directory while searching for `tablo.toml`: {error}"))?;
	Ok(default_runtime_config_path(&current_dir))
}

#[cfg(test)]
mod tests {
	use std::path::Path;
	use std::path::PathBuf;

	use super::build_database_config;
	use super::default_runtime_config_path;
	use super::parse_database_mapping;
	use tablo::utils::unique_temp_directory;
	use tablo::utils::unique_temp_path;

	#[test]
	fn builds_runtime_database_config_from_cli_mappings() {
		let config = build_database_config(None, &[String::from("ExampleDb=sqlite:data/example.sqlite")]).unwrap();

		assert_eq!(
			config.sqlite_database_path("exampledb"),
			Some(Path::new("data/example.sqlite")),
		);
	}

	#[test]
	fn command_line_database_mappings_override_config_file_entries() {
		let config_path = unique_temp_path("tablo_runtime_config_override", "toml");
		std::fs::write(
			&config_path,
			r#"
				[databases]
				ExampleDb = "sqlite:data/from-config.sqlite"
			"#,
		).unwrap();

		let config = build_database_config(
			Some(&config_path),
			&[String::from("ExampleDb=sqlite:data/from-cli.sqlite")],
		).unwrap();
		let _ = std::fs::remove_file(&config_path);

		assert_eq!(
			config.sqlite_database_path("ExampleDb"),
			Some(Path::new("data/from-cli.sqlite")),
		);
	}

	#[test]
	fn discovers_default_runtime_config_file_when_present() {
		let temp_dir = unique_temp_directory("tablo_runtime_config_default");
		let config_path = temp_dir.join("tablo.toml");
		std::fs::create_dir_all(&temp_dir).unwrap();
		std::fs::write(&config_path, "[databases]\n").unwrap();

		assert_eq!(default_runtime_config_path(&temp_dir), Some(config_path.clone()));

		let _ = std::fs::remove_file(&config_path);
		let _ = std::fs::remove_dir(&temp_dir);
	}

	#[test]
	fn does_not_discover_default_runtime_config_file_when_missing() {
		let temp_dir = unique_temp_directory("tablo_runtime_config_missing");
		std::fs::create_dir_all(&temp_dir).unwrap();

		assert_eq!(default_runtime_config_path(&temp_dir), None);

		let _ = std::fs::remove_dir(&temp_dir);
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
