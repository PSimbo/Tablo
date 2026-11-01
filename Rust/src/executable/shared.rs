use std::fs;
use std::path::Path;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use super::{ExecutableBuildError, TabloVersion, build_cargo, publish_executable, publish_output, resolve_runtime_lock};
use crate::bytecode::Program;
use crate::runtime_drivers::DriverSet;

#[derive(Deserialize, Serialize)]
struct LibraryManifest {
	version: String,
	target: String,
	drivers: String,
	sha256: String,
}

pub(super) fn build(
	program: &Program,
	output: &Path,
	runtime_dir: &Path,
	cache: &Path,
	host: &str,
	runtime_variant: (TabloVersion, DriverSet),
	offline: bool,
) -> Result<(), ExecutableBuildError> {
	// Cargo locks its build, but releases that lock before we copy the result.
	// Keep a separate lock through publication so another build cannot replace
	// the cached shared library between linking and deployment.
	eprintln!("Building shared Tablo runtime and launcher (Cargo reuses cached dependencies)...");
	let _cache_lock = lock(&cache.join("shared-runtime.lock"))?;
	let (runtime_version, drivers) = runtime_variant;
	let variant = drivers.name();
	let features = drivers.cargo_features();
	let project = cache.join(format!("shared-runtime-{variant}"));
	fs::create_dir_all(project.join("src"))?;
	let library = format!("tablo_runtime_{}_{}_{variant}", runtime_version.major, runtime_version.minor);
	let filename = format!("lib{library}.so");
	let runtime_path = runtime_dir.to_str().ok_or_else(|| error("The runtime source path must be valid UTF-8 for Cargo."))?;
	let path = toml::Value::String(runtime_path.to_string()).to_string();
	write_if_changed(&project.join("Cargo.toml"), format!(
		"[package]\nname = \"tablo-shared-runtime-{variant}\"\nversion = \"0.0.0\"\nedition = \"2024\"\n\n[workspace]\n\n[lib]\nname = \"{library}\"\ncrate-type = [\"cdylib\"]\n\n[dependencies]\ntablo = {{ package = \"tabloc\", path = {path}, default-features = false, features = {features} }}\n"
	).as_bytes())?;
	let mut lockfile = fs::read_to_string(runtime_dir.join("Cargo.lock"))?;
	lockfile.push_str(&format!("\n[[package]]\nname = \"tablo-shared-runtime-{variant}\"\nversion = \"0.0.0\"\ndependencies = [\"tabloc\"]\n"));
	write_if_changed(&project.join("Cargo.lock"), lockfile.as_bytes())?;
	write_if_changed(&project.join("src/lib.rs"), include_bytes!("templates/shared_runtime.rs"))?;
	write_if_changed(&project.join("build.rs"), format!(
		"fn main() {{ println!(\"cargo:rustc-link-arg=-Wl,-soname,{filename}\"); }}\n"
	).as_bytes())?;
	resolve_runtime_lock(&project, host, offline)?;
	let target_dir = cache.join("target");
	build_cargo(&project.join("Cargo.toml"), &target_dir, host, &["--lib"], offline)?;
	let library_dir = target_dir.join(host).join("debug");
	let built_library = library_dir.join(&filename);

	let launcher = tempfile::Builder::new().prefix("tablo-launcher-").tempdir_in(cache)?;
	let name = launcher.path().file_name().unwrap().to_str().unwrap();
	fs::create_dir(launcher.path().join("src"))?;
	fs::write(launcher.path().join("Cargo.toml"), format!(
		"[package]\nname = \"{name}\"\nversion = \"0.0.0\"\nedition = \"2024\"\n\n[workspace]\n"
	))?;
	fs::write(launcher.path().join("Cargo.lock"), format!(
		"version = 4\n\n[[package]]\nname = \"{name}\"\nversion = \"0.0.0\"\n"
	))?;
	let search_path = library_dir.to_str().ok_or_else(|| error("The cache path must be valid UTF-8 for native linking."))?;
	if search_path.contains(['\n', '\r']) {
		return Err(error("The cache path must not contain line breaks."));
	}
	fs::write(launcher.path().join("build.rs"), format!(
		"fn main() {{\n println!(\"cargo:rustc-link-search=native={{}}\", {search_path:?});\n println!(\"cargo:rustc-link-arg=-Wl,--enable-new-dtags,-rpath,$ORIGIN/lib\");\n}}\n"
	))?;
	let version = TabloVersion::current();
	let source = include_str!("templates/shared_launcher.rs")
		.replace("__TABLO_LIBRARY__", &library)
		.replace("__TABLO_DRIVERS__", &drivers.bits().to_string())
		.replace("__TABLO_VERSION__", &format!("TabloVersion {{ major: {}, minor: {}, patch: {} }}", version.major, version.minor, version.patch));
	fs::write(launcher.path().join("src/main.rs"), source)?;
	fs::write(launcher.path().join("src/abi.rs"), include_bytes!("../runtime_abi.rs"))?;
	fs::write(launcher.path().join("src/program.tbo"), crate::object_file::write_program(program))?;
	build_cargo(&launcher.path().join("Cargo.toml"), &target_dir, host, &["--bin", name], offline)?;

	deploy_library(&built_library, &output.parent().unwrap().join("lib"), host, runtime_version, version, &variant)?;
	publish_executable(&library_dir.join(name), output)?;
	Ok(())
}

fn lock(path: &Path) -> Result<fs::File, ExecutableBuildError> {
	let file = fs::OpenOptions::new().read(true).write(true).create(true).truncate(false).open(path)?;
	file.lock()?;
	Ok(file)
}

fn write_if_changed(path: &Path, contents: &[u8]) -> Result<(), ExecutableBuildError> {
	if fs::read(path).ok().as_deref() != Some(contents) {
		fs::write(path, contents)?;
	}
	Ok(())
}

fn deploy_library(
	built: &Path,
	directory: &Path,
	host: &str,
	runtime: TabloVersion,
	artifact: TabloVersion,
	drivers: &str,
) -> Result<(), ExecutableBuildError> {
	fs::create_dir_all(directory)?;
	let _deployment_lock = lock(&directory.join(".tablo-runtime.lock"))?;
	let destination = directory.join(built.file_name().unwrap());
	let manifest_path = destination.with_extension("so.json");
	let bytes = fs::read(built)?;
	let hash = format!("{:x}", Sha256::digest(&bytes));
	let metadata = LibraryManifest { version: runtime.to_string(), target: host.to_string(), drivers: drivers.to_string(), sha256: hash };
	match fs::read(&destination) {
		Ok(existing) if existing == bytes => {},
		Ok(existing) => {
			let manifest = fs::read(&manifest_path).ok().and_then(|bytes| serde_json::from_slice::<LibraryManifest>(&bytes).ok());
			if let Some(manifest) = manifest {
				let version = TabloVersion::parse(&manifest.version).ok();
				if artifact != (TabloVersion { major: 0, minor: 0, patch: 0 })
					&& version.is_some_and(|version| version.accepts(artifact))
					&& manifest.target == host && manifest.drivers == drivers
					&& manifest.sha256 == format!("{:x}", Sha256::digest(&existing))
				{
					return Ok(());
				}
			}
			return Err(error(format!(
				"Existing runtime `{}` is different and could not be verified as compatible. Build into a new deployment directory or explicitly update that library and its .so.json manifest; the existing executable and library were preserved.",
				destination.display(),
			)));
		}
		Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
			publish_output(&mut bytes.as_slice(), &destination, false)?;
		}
		Err(error) => return Err(error.into()),
	}
	// The manifest is build-time bookkeeping, never an execution dependency.
	// A later build can repair a missing manifest when library bytes are identical.
	let manifest = serde_json::to_vec_pretty(&metadata).map_err(|failure| error(failure.to_string()))?;
	publish_output(&mut manifest.as_slice(), &manifest_path, false)?;
	Ok(())
}

fn error(message: impl Into<String>) -> ExecutableBuildError {
	ExecutableBuildError::Build(message.into())
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn reuses_compatible_patches_without_downgrading_and_rejects_damaged_libraries() {
		let temp = tempfile::tempdir().unwrap();
		let built = temp.path().join("libtablo_runtime_0_1_all.so");
		let deployment = temp.path().join("lib");
		let older = TabloVersion::parse("0.1.2").unwrap();
		let newer = TabloVersion::parse("0.1.5").unwrap();
		fs::write(&built, "newer runtime").unwrap();
		deploy_library(&built, &deployment, "host", newer, newer, "all").unwrap();
		fs::write(&built, "older runtime").unwrap();
		deploy_library(&built, &deployment, "host", older, older, "all").unwrap();
		let installed = deployment.join(built.file_name().unwrap());
		assert_eq!(fs::read_to_string(&installed).unwrap(), "newer runtime");
		fs::write(&installed, "damaged runtime").unwrap();
		assert!(deploy_library(&built, &deployment, "host", older, older, "all").is_err());
		assert_eq!(fs::read_to_string(&installed).unwrap(), "damaged runtime");
	}

	#[test]
	fn does_not_replace_incompatible_or_different_experimental_libraries() {
		let temp = tempfile::tempdir().unwrap();
		let built = temp.path().join("libtablo_runtime_0_1_all.so");
		let deployment = temp.path().join("lib");
		let version = TabloVersion::parse("0.1.2").unwrap();
		fs::write(&built, "original").unwrap();
		deploy_library(&built, &deployment, "host", version, version, "all").unwrap();
		fs::write(&built, "replacement").unwrap();
		assert!(deploy_library(&built, &deployment, "host", version, TabloVersion::parse("0.1.3").unwrap(), "all").is_err());
		assert!(deploy_library(&built, &deployment, "host", version, TabloVersion::parse("0.0.0").unwrap(), "all").is_err());
		assert!(deploy_library(&built, &deployment, "other-host", version, version, "all").is_err());
	}
}
