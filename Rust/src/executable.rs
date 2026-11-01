use std::fmt::{Display, Formatter};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::bytecode::{DebugInfoLevel, Program};
use crate::runtime::check_version;
use crate::runtime_abi::TabloVersion;
use crate::runtime_drivers::required_drivers;
use crate::schema::SchemaCatalog;
use crate::{CompilationReport, CompilationTarget, TabloError};

mod shared;

#[derive(Debug)]
pub enum ExecutableBuildError {
	Compile(TabloError),
	Build(String),
}

impl Display for ExecutableBuildError {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Compile(error) => Display::fmt(error, formatter),
			Self::Build(message) => formatter.write_str(message),
		}
	}
}

impl From<std::io::Error> for ExecutableBuildError {
	fn from(error: std::io::Error) -> Self {
		Self::Build(format!("Executable build I/O error: {error}"))
	}
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum RuntimeMode {
	#[default]
	Embedded,
	Shared,
}

impl std::str::FromStr for RuntimeMode {
	type Err = String;

	fn from_str(value: &str) -> Result<Self, String> {
		match value {
			"embedded" => Ok(Self::Embedded),
			"shared" => Ok(Self::Shared),
			_ => Err(format!("Unknown runtime mode `{value}`; expected embedded or shared.")),
		}
	}
}

/// The builder uses a local runtime source bundle and Cargo's dependency
/// cache. Keeping these paths explicit lets callers control build storage without
/// tying deployed programs to the compiler installation or checkout.
pub struct ExecutableBuildOptions {
	pub runtime_dir: PathBuf,
	pub cache_dir: PathBuf,
	pub offline: bool,
	pub debug_info: DebugInfoLevel,
	pub runtime: RuntimeMode,
}

pub fn compile_executable(
	source: &str,
	source_path: &Path,
	output_path: &Path,
	schema_catalog: Option<&SchemaCatalog>,
	options: &ExecutableBuildOptions,
) -> Result<CompilationReport, ExecutableBuildError> {
	if !cfg!(target_os = "linux") {
		return Err(ExecutableBuildError::Build(String::from("Executable builds currently support Linux hosts only.")));
	}
	let (program, report, output_path) = compile_for_output(source, source_path, output_path, schema_catalog, options.debug_info)?;
	let drivers = required_drivers(&program, schema_catalog).map_err(ExecutableBuildError::Build)?;
	let runtime_dir = fs::canonicalize(&options.runtime_dir)?;
	let manifest: toml::Value = toml::from_str(&fs::read_to_string(runtime_dir.join("Cargo.toml"))?)
		.map_err(|error| ExecutableBuildError::Build(format!("Invalid runtime Cargo.toml: {error}")))?;
	if manifest.get("package").and_then(|package| package.get("name")).and_then(toml::Value::as_str) != Some("tabloc") {
		return Err(ExecutableBuildError::Build(String::from("The runtime bundle must contain the `tabloc` package.")));
	}
	let version = manifest.get("package").and_then(|package| package.get("version")).and_then(toml::Value::as_str)
		.ok_or_else(|| ExecutableBuildError::Build(String::from("The runtime bundle must declare its Tablo version.")))?;
	let runtime_version = TabloVersion::parse(version).map_err(ExecutableBuildError::Build)?;
	let artifact_version = TabloVersion::current();
	check_version(runtime_version, artifact_version).map_err(ExecutableBuildError::Build)?;

	// Keep the caller's working directory for both discovery and the build. A
	// generated directory must not silently select a different rustup override.
	let rustc = tool_output("rustc", &["-vV"])?;
	let host = rustc.lines().find_map(|line| line.strip_prefix("host: "))
		.ok_or_else(|| ExecutableBuildError::Build(String::from("Could not determine the Rust host target.")))?;
	if !host.contains("-linux-") {
		return Err(ExecutableBuildError::Build(format!("Unsupported Rust host target `{host}`.")));
	}
	tool_output("cargo", &["--version"])?;
	fs::create_dir_all(&options.cache_dir)?;
	let cache_dir = fs::canonicalize(&options.cache_dir)?;
	if options.runtime == RuntimeMode::Shared {
		shared::build(&program, &output_path, &runtime_dir, &cache_dir, host, (runtime_version, drivers), options.offline)?;
		return Ok(report);
	}
	let project = tempfile::Builder::new().prefix("tablo-build-").tempdir_in(&cache_dir)?;
	let name = project.path().file_name().unwrap().to_str().unwrap();
	fs::create_dir(project.path().join("src"))?;
	let runtime_path = runtime_dir.to_str().ok_or_else(|| {
		ExecutableBuildError::Build(String::from("The runtime source path must be valid UTF-8 for Cargo."))
	})?;
	let quoted_path = toml::Value::String(runtime_path.to_string()).to_string();
	let features = drivers.cargo_features();
	let manifest = format!(
		"[package]\nname = \"{name}\"\nversion = \"0.0.0\"\nedition = \"2024\"\n\n[workspace]\n\n[dependencies]\ntablo = {{ package = \"tabloc\", path = {quoted_path}, default-features = false, features = {features} }}\n"
	);
	fs::write(project.path().join("Cargo.toml"), manifest)?;
	// The launcher adds only one local package to the supplied dependency graph.
	// Cargo's --locked check verifies that no dependency resolution changed.
	let mut lockfile = fs::read_to_string(runtime_dir.join("Cargo.lock"))?;
	lockfile.push_str(&format!("\n[[package]]\nname = \"{name}\"\nversion = \"0.0.0\"\ndependencies = [\"tabloc\"]\n"));
	fs::write(project.path().join("Cargo.lock"), lockfile)?;
	fs::write(project.path().join("src/program.tbo"), crate::object_file::write_program(&program))?;
	fs::write(project.path().join("src/main.rs"), launcher_source(artifact_version))?;

	eprintln!("Building embedded Tablo executable (Cargo reuses cached dependencies)...");
	resolve_runtime_lock(project.path(), host, options.offline)?;
	build_cargo(&project.path().join("Cargo.toml"), &cache_dir.join("target"), host, &["--bin", name], options.offline)?;
	publish_executable(&cache_dir.join("target").join(host).join("debug").join(name), &output_path)?;
	Ok(report)
}

/// Writes the existing standalone bytecode encoding for the runtime and debugger.
/// This is not yet a relocatable object for Tablo library linking.
pub fn compile_object(
	source: &str,
	source_path: &Path,
	output_path: &Path,
	schema_catalog: Option<&SchemaCatalog>,
	debug_info: DebugInfoLevel,
) -> Result<CompilationReport, ExecutableBuildError> {
	let (program, report, output_path) = compile_for_output(source, source_path, output_path, schema_catalog, debug_info)?;
	let bytes = crate::object_file::write_program(&program);
	publish_output(&mut bytes.as_slice(), &output_path, false)?;
	Ok(report)
}

pub fn compile_library(
	source: &str,
	source_path: &Path,
	output_path: &Path,
	schema_catalog: Option<&SchemaCatalog>,
	debug_info: DebugInfoLevel,
) -> Result<CompilationReport, ExecutableBuildError> {
	let (mut library, warnings) = crate::frontend::compile_library_source(source, &source_path.to_string_lossy(), schema_catalog)
		.map_err(ExecutableBuildError::Compile)?;
	let output_path = checked_output_path(&library.program, output_path)?;
	library.program.debug_info_mut().retain_level(debug_info);
	let bytes = library.write().map_err(ExecutableBuildError::Build)?;
	publish_output(&mut bytes.as_slice(), &output_path, false)?;
	Ok(CompilationReport { warnings })
}

fn compile_for_output(
	source: &str,
	source_path: &Path,
	output_path: &Path,
	schema_catalog: Option<&SchemaCatalog>,
	debug_info: DebugInfoLevel,
) -> Result<(Program, CompilationReport, PathBuf), ExecutableBuildError> {
	let source_name = source_path.to_string_lossy();
	let (mut program, warnings) = crate::compile_source_to_program_with_name_and_schema_and_warnings(
		source, Some(&source_name), CompilationTarget::Standalone, schema_catalog,
	).map_err(ExecutableBuildError::Compile)?;
	let output_path = checked_output_path(&program, output_path)?;
	// Check all source paths before removing optional debug records.
	program.debug_info_mut().retain_level(debug_info);
	Ok((program, CompilationReport { warnings }, output_path))
}

fn checked_output_path(program: &Program, output_path: &Path) -> Result<PathBuf, ExecutableBuildError> {
	let output_path = resolved_output_path(output_path)?;
	for source_file in program.debug_info().source_files() {
		if fs::canonicalize(source_file.display_name()).ok().as_ref() == Some(&output_path) {
			return Err(ExecutableBuildError::Build(format!("Output `{}` would overwrite a source file.", output_path.display())));
		}
	}
	Ok(output_path)
}

fn resolve_runtime_lock(project: &Path, host: &str, offline: bool) -> Result<(), ExecutableBuildError> {
	let lock_path = project.join("Cargo.lock");
	let supplied = fs::read_to_string(&lock_path)?;
	// Optional dependencies can disappear from Cargo's resolved lockfile. Let
	// Cargo prune it, but reject any package/version/checksum outside the supplied
	// graph before allowing compilation or dependency build scripts to run.
	let mut command = Command::new("cargo");
	command.args(["metadata", "--format-version", "1", "--filter-platform", host, "--manifest-path"])
		.arg(project.join("Cargo.toml")).stdin(Stdio::null()).stdout(Stdio::null()).stderr(Stdio::inherit());
	if offline { command.arg("--offline"); }
	if !command.status()?.success() {
		return Err(ExecutableBuildError::Build(String::from("Could not resolve the runtime dependency graph.")));
	}
	verify_locked_packages(&supplied, &fs::read_to_string(lock_path)?)
}

fn verify_locked_packages(supplied: &str, resolved: &str) -> Result<(), ExecutableBuildError> {
	fn packages(contents: &str) -> Result<std::collections::BTreeSet<Vec<String>>, ExecutableBuildError> {
		let lock: toml::Value = toml::from_str(contents).map_err(|error| ExecutableBuildError::Build(format!("Invalid dependency lockfile: {error}")))?;
		let entries = lock.get("package").and_then(toml::Value::as_array)
			.ok_or_else(|| ExecutableBuildError::Build(String::from("Dependency lockfile contains no packages.")))?;
		Ok(entries.iter().map(|package| ["name", "version", "source", "checksum"].iter()
			.map(|key| package.get(*key).and_then(toml::Value::as_str).unwrap_or("").to_string()).collect()).collect())
	}
	if !packages(resolved)?.is_subset(&packages(supplied)?) {
		return Err(ExecutableBuildError::Build(String::from("Runtime dependency resolution changed a locked package version or checksum; update the supplied runtime bundle deliberately before building.")));
	}
	Ok(())
}

fn build_cargo(manifest: &Path, target_dir: &Path, host: &str, selection: &[&str], offline: bool) -> Result<(), ExecutableBuildError> {
	let mut cargo = Command::new("cargo");
	cargo.args(["build", "--locked", "--target", host, "--manifest-path"])
		.arg(manifest).args(selection).arg("--target-dir").arg(target_dir)
		.stdin(Stdio::null()).stdout(Stdio::inherit()).stderr(Stdio::inherit());
	if offline {
		cargo.arg("--offline");
	}
	let status = cargo.status().map_err(|error| ExecutableBuildError::Build(format!("Failed to invoke Cargo: {error}")))?;
	if !status.success() {
		return Err(ExecutableBuildError::Build(format!("Native build failed ({status}); see Cargo diagnostics above.")));
	}
	Ok(())
}

fn tool_output(tool: &str, arguments: &[&str]) -> Result<String, ExecutableBuildError> {
	let output = Command::new(tool).args(arguments).stdin(Stdio::null()).output()
		.map_err(|error| ExecutableBuildError::Build(format!("Could not run `{tool}`; a Rust toolchain and native linker are required: {error}")))?;
	if !output.status.success() {
		return Err(ExecutableBuildError::Build(format!("`{tool}` failed: {}", String::from_utf8_lossy(&output.stderr))));
	}
	String::from_utf8(output.stdout).map_err(|error| ExecutableBuildError::Build(format!("Invalid output from `{tool}`: {error}")))
}

fn resolved_output_path(path: &Path) -> Result<PathBuf, ExecutableBuildError> {
	if path.is_dir() {
		return Err(ExecutableBuildError::Build(format!("Output `{}` is a directory; a filename is required.", path.display())));
	}
	if path.exists() {
		return Ok(fs::canonicalize(path)?);
	}
	let parent = path.parent().filter(|path| !path.as_os_str().is_empty()).unwrap_or(Path::new("."));
	let name = path.file_name().ok_or_else(|| ExecutableBuildError::Build(String::from("An output filename is required.")))?;
	Ok(fs::canonicalize(parent)?.join(name))
}

fn publish_executable(built_path: &Path, output_path: &Path) -> Result<(), ExecutableBuildError> {
	publish_output(&mut fs::File::open(built_path)?, output_path, true)
}

fn publish_output(input: &mut impl std::io::Read, output_path: &Path, executable: bool) -> Result<(), ExecutableBuildError> {
	let staging = tempfile::Builder::new().prefix(".tablo-output-").tempdir_in(output_path.parent().unwrap())?;
	let staged_path = staging.path().join("executable");
	let mut options = fs::OpenOptions::new();
	options.write(true).create_new(true);
	#[cfg(unix)] {
		use std::os::unix::fs::OpenOptionsExt;
		options.mode(if executable { 0o777 } else { 0o666 });
	}
	let mut output = options.open(&staged_path)?;
	std::io::copy(input, &mut output)?;
	output.sync_all()?;
	fs::rename(&staged_path, output_path)?;
	Ok(())
}

fn launcher_source(version: TabloVersion) -> String {
	format!(r#"fn main() {{
	let version = tablo::runtime_abi::TabloVersion {{ major: {}, minor: {}, patch: {} }};
	let arguments: Vec<String> = std::env::args().skip(1).collect();
	match tablo::runtime::run_package(include_bytes!("program.tbo"), version, &arguments) {{
		Ok(status) => std::process::exit(status),
		Err(error) => {{ eprintln!("{{error}}"); std::process::exit(1); }}
	}}
}}
"#, version.major, version.minor, version.patch)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn failed_publication_preserves_existing_output() {
		let directory = tempfile::tempdir().unwrap();
		let output = directory.path().join("app");
		fs::write(&output, "previous executable").unwrap();
		assert!(publish_executable(&directory.path().join("missing"), &output).is_err());
		assert_eq!(fs::read_to_string(&output).unwrap(), "previous executable");
		assert_eq!(fs::read_dir(directory.path()).unwrap().count(), 1);
	}

	#[test]
	fn rejects_output_overwriting_an_imported_source_before_building() {
		let directory = tempfile::tempdir().unwrap();
		let input = directory.path().join("main.tablo");
		let imported = directory.path().join("Helper.tablo");
		let source = "use './Helper'; fn Main(args: [text]): int { return Answer(); }";
		fs::write(&input, source).unwrap();
		fs::write(&imported, "pub fn Answer(): int { return 42; }").unwrap();
		let options = ExecutableBuildOptions { runtime_dir: directory.path().join("missing"), cache_dir: directory.path().join("cache"), offline: true, debug_info: DebugInfoLevel::Full, runtime: RuntimeMode::Embedded };
		let error = compile_executable(source, &input, &imported, None, &options).unwrap_err();
		assert!(error.to_string().contains("would overwrite a source file"));
	}
	#[test]
	fn permits_pruning_locked_packages_but_rejects_version_and_checksum_changes() {
		let package = "[[package]]\nname = 'dependency'\nversion = '1.2.3'\nsource = 'registry+example'\nchecksum = 'original'\n";
		let unused = "[[package]]\nname = 'unused'\nversion = '4.0.0'\n";
		let supplied = format!("{package}{unused}");
		verify_locked_packages(&supplied, package).unwrap();
		assert!(verify_locked_packages(&supplied, &package.replace("1.2.3", "1.2.4")).is_err());
		assert!(verify_locked_packages(&supplied, &package.replace("original", "changed")).is_err());
	}

}
