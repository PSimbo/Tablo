use std::path::{Path, PathBuf};

use clap::{Parser, ValueEnum};
use serde::Deserialize;

use tablo::bytecode::DebugInfoLevel;
use tablo::executable::{ExecutableBuildOptions, ExecutableBuildError, RuntimeMode, compile_executable, compile_object, compile_library};
use tablo::runtime_config::read_schema_catalog_from_runtime_config_path;
use tablo::utils::existing_child_path;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, ValueEnum)]
enum Emit {
	#[default]
	Executable,
	Object,
	Library,
}

#[derive(Parser, Debug)]
#[command(name = "tabloc", version, about = "Compile Tablo source to an executable, standalone bytecode object, or Tablo library.")]
#[command(after_help = "Executable output supports embedded and shared runtimes on Linux.\nUse --emit object --debug-info full for tablodbg; objects are not yet independently linkable.\nDatabase backends and schemas are selected through tablo.toml or --config, not --schema.")]
struct Args {
	#[arg(value_name = "INPUT")]
	input_path: PathBuf,
	#[arg(short = 'o', long = "output", value_name = "PATH")]
	output_path: Option<PathBuf>,
	#[arg(long, value_enum, default_value = "executable")]
	emit: Emit,
	#[arg(long, value_parser = ["embedded", "shared"], help = "Runtime placement for executable output (default: embedded)")]
	runtime: Option<String>,
	#[arg(long, value_name = "none|lines|full", help = "Tablo debug information (default: lines)")]
	debug_info: Option<DebugInfoLevel>,
	#[arg(long = "config", value_name = "PATH", help = "Project TOML file (default: ./tablo.toml when present)")]
	config_path: Option<PathBuf>,
	#[arg(long, value_name = "PATH", help = "Runtime sources (default: runtime/ beside the compiler executable)")]
	runtime_dir: Option<PathBuf>,
	#[arg(long, value_name = "PATH", help = "Build cache (default: $XDG_CACHE_HOME/tablo or $HOME/.cache/tablo)")]
	cache_dir: Option<PathBuf>,
	#[arg(long, help = "Disallow Cargo network access during executable builds")]
	offline: bool,
}

#[derive(Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct BuildSettings {
	runtime: Option<String>,
	debug_info: Option<String>,
}

#[derive(Default, Deserialize)]
struct ProjectSettings {
	#[serde(default)]
	build: BuildSettings,
}

struct CliError {
	message: String,
	status: i32,
}

impl CliError {
	fn build(message: impl Into<String>) -> Self {
		Self { message: message.into(), status: 1 }
	}

	fn usage(message: impl Into<String>) -> Self {
		Self { message: message.into(), status: 2 }
	}
}

fn main() {
	if let Err(error) = compile(Args::parse()) {
		eprintln!("{}", error.message);
		std::process::exit(error.status);
	}
}

fn compile(args: Args) -> Result<(), CliError> {
	if args.emit != Emit::Executable && (args.runtime.is_some() || args.runtime_dir.is_some() || args.cache_dir.is_some() || args.offline) {
		return Err(CliError::usage("--runtime, --runtime-dir, --cache-dir, and --offline apply to executable output only."));
	}
	let current_dir = std::env::current_dir().map_err(|error| CliError::build(error.to_string()))?;
	let config_path = args.config_path.or_else(|| existing_child_path(&current_dir, "tablo.toml"));
	let project = read_project_settings(config_path.as_deref()).map_err(CliError::build)?;
	let debug_info = resolve_debug_info(args.debug_info, project.build.debug_info.as_deref()).map_err(CliError::usage)?;
	let runtime = if args.emit == Emit::Executable {
		args.runtime.as_deref().or(project.build.runtime.as_deref()).unwrap_or("embedded").parse().map_err(CliError::usage)?
	}
	else {
		RuntimeMode::Embedded
	};
	let output = args.output_path.unwrap_or_else(|| default_output_path(&args.input_path, args.emit));
	let source = std::fs::read_to_string(&args.input_path)
		.map_err(|error| CliError::build(format!("Failed to read `{}`: {error}", args.input_path.display())))?;
	let schema = config_path.as_ref().map(read_schema_catalog_from_runtime_config_path).transpose()
		.map_err(|error| CliError::build(error.message))?;
	let report = match args.emit {
		Emit::Executable => {
			let runtime_dir = match args.runtime_dir {
				Some(path) => path,
				None => installed_runtime_dir(&std::env::current_exe().map_err(|error| CliError::build(error.to_string()))?)
					.map_err(CliError::build)?,
			};
			let cache_dir = match args.cache_dir {
				Some(path) => path,
				None => default_cache_dir(std::env::var_os("XDG_CACHE_HOME").map(PathBuf::from).as_deref(), std::env::var_os("HOME").map(PathBuf::from).as_deref())
					.map_err(CliError::build)?,
			};
			let options = ExecutableBuildOptions { runtime_dir, cache_dir, offline: args.offline, debug_info, runtime };
			compile_executable(&source, &args.input_path, &output, schema.as_ref(), &options)
		}
		Emit::Object => compile_object(&source, &args.input_path, &output, schema.as_ref(), debug_info),
		Emit::Library => compile_library(&source, &args.input_path, &output, schema.as_ref(), debug_info),
	}.map_err(|error| CliError::build(match error {
		ExecutableBuildError::Compile(error) => error.format_with_source_name(&source, Some(&args.input_path.to_string_lossy())),
		ExecutableBuildError::Build(message) => message,
	}))?;
	for warning in report.warnings {
		eprintln!("{}", warning.format_with_source_name(&source, Some(&args.input_path.to_string_lossy())));
	}
	Ok(())
}

fn read_project_settings(path: Option<&Path>) -> Result<ProjectSettings, String> {
	match path {
		Some(path) => {
			let contents = std::fs::read_to_string(path).map_err(|error| format!("Failed to read config `{}`: {error}", path.display()))?;
			toml::from_str(&contents).map_err(|error| format!("Invalid config `{}`: {error}", path.display()))
		}
		None => Ok(ProjectSettings::default()),
	}
}

fn resolve_debug_info(explicit: Option<DebugInfoLevel>, configured: Option<&str>) -> Result<DebugInfoLevel, String> {
	match explicit {
		Some(level) => Ok(level),
		None => configured.unwrap_or("lines").parse(),
	}
}

fn default_output_path(input: &Path, emit: Emit) -> PathBuf {
	let mut output = input.to_path_buf();
	output.set_extension(match emit {
		Emit::Executable if cfg!(windows) => "exe",
		Emit::Executable => "",
		Emit::Object => "tobj",
		Emit::Library => "tlib",
	});
	output
}

fn installed_runtime_dir(executable: &Path) -> Result<PathBuf, String> {
	let path = executable.parent().unwrap_or(Path::new(".")).join("runtime");
	if path.join("Cargo.toml").is_file() && path.join("Cargo.lock").is_file() && path.join("src/lib.rs").is_file() {
		Ok(path)
	}
	else {
		Err(format!("No runtime source bundle found at `{}`. Install a bundle there or pass --runtime-dir PATH (use --runtime-dir Rust from the Tablo checkout root).", path.display()))
	}
}

fn default_cache_dir(xdg: Option<&Path>, home: Option<&Path>) -> Result<PathBuf, String> {
	if let Some(path) = xdg.filter(|path| path.is_absolute()) {
		return Ok(path.join("tablo"));
	}
	if let Some(path) = home.filter(|path| path.is_absolute()) {
		return Ok(path.join(".cache/tablo"));
	}
	Err(String::from("Cannot determine a per-user build cache; pass --cache-dir PATH or set HOME or an absolute XDG_CACHE_HOME."))
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn rejects_legacy_output_argument_and_schema_override() {
		assert!(Args::try_parse_from(["tabloc", "main.tablo", "main.tbo"]).is_err());
		assert!(Args::try_parse_from(["tabloc", "--schema", "schema.sql", "main.tablo"]).is_err());
	}

	#[test]
	fn selects_output_kind_explicitly_instead_of_from_extension() {
		let args = Args::try_parse_from(["tabloc", "main.tablo", "-o", "main.tobj"]).unwrap();
		assert_eq!(args.emit, Emit::Executable);
		assert_eq!(default_output_path(Path::new("src/main.tablo"), Emit::Object), PathBuf::from("src/main.tobj"));
	}

	#[test]
	fn applies_debug_info_precedence() {
		assert_eq!(resolve_debug_info(Some(DebugInfoLevel::Full), Some("none")).unwrap(), DebugInfoLevel::Full);
		assert_eq!(resolve_debug_info(None, Some("none")).unwrap(), DebugInfoLevel::None);
		assert_eq!(resolve_debug_info(None, None).unwrap(), DebugInfoLevel::Lines);
		assert!(resolve_debug_info(None, Some("invalid")).is_err());
	}

	#[test]
	fn selects_absolute_cache_paths_and_ignores_invalid_xdg_paths() {
		assert_eq!(default_cache_dir(Some(Path::new("/cache")), Some(Path::new("/home/user"))).unwrap(), PathBuf::from("/cache/tablo"));
		assert_eq!(default_cache_dir(Some(Path::new("relative")), Some(Path::new("/home/user"))).unwrap(), PathBuf::from("/home/user/.cache/tablo"));
		assert!(default_cache_dir(None, None).is_err());
	}

	#[test]
	fn discovers_runtime_sources_relative_to_installed_compiler() {
		let directory = tempfile::tempdir().unwrap();
		let executable = directory.path().join("tabloc");
		assert!(installed_runtime_dir(&executable).unwrap_err().contains("--runtime-dir"));
		let runtime = directory.path().join("runtime");
		std::fs::create_dir_all(runtime.join("src")).unwrap();
		for file in ["Cargo.toml", "Cargo.lock", "src/lib.rs"] {
			std::fs::write(runtime.join(file), "").unwrap();
		}
		assert_eq!(installed_runtime_dir(&executable).unwrap(), runtime);
	}
}
