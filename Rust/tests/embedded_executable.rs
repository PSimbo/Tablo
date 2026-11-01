#![cfg(target_os = "linux")]

use std::fs;
use std::path::PathBuf;
use std::process::Command;

use tablo::executable::{ExecutableBuildOptions, compile_executable};

#[test]
#[ignore = "builds native executables with Cargo; run explicitly with --ignored"]
fn builds_relocates_and_rebuilds_an_embedded_application() {
	let directory = tempfile::tempdir().unwrap();
	let source_path = directory.path().join("main.tablo");
	let output_path = directory.path().join("application");
	let runtime_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
	let cache_dir = runtime_dir.join("target/embedded-test-cache");
	let options = ExecutableBuildOptions { runtime_dir: runtime_dir.clone(), cache_dir, offline: true, debug_info: tablo::bytecode::DebugInfoLevel::Lines, runtime: tablo::executable::RuntimeMode::Embedded };
	let source = "fn Main(args: [text]): int { if len(args) == 2 { if args[1] == '--help' and args[2] == 'two words' { return 7; } } return 2; }";
	fs::write(&source_path, source).unwrap();
	compile_executable(source, &source_path, &output_path, None, &options).unwrap();

	let deployment = directory.path().join("deployment with spaces");
	fs::create_dir(&deployment).unwrap();
	let deployed = deployment.join("application");
	fs::rename(&output_path, &deployed).unwrap();
	fs::remove_file(&source_path).unwrap();
	let result = Command::new(&deployed).args(["--help", "two words"]).current_dir(&deployment)
		.env_remove("TABLO_CONFIG").output().unwrap();
	assert_eq!(result.status.code(), Some(7), "{}", String::from_utf8_lossy(&result.stderr));
	assert!(result.stdout.is_empty());

	let missing_config = Command::new(&deployed).current_dir(&deployment)
		.env("TABLO_CONFIG", "missing.toml").output().unwrap();
	assert_eq!(missing_config.status.code(), Some(1));
	assert!(String::from_utf8_lossy(&missing_config.stderr).contains("Failed to load runtime config"));
	fs::write(deployment.join("tablo.toml"), "invalid TOML [").unwrap();
	let invalid_config = Command::new(&deployed).current_dir(&deployment)
		.env_remove("TABLO_CONFIG").output().unwrap();
	assert_eq!(invalid_config.status.code(), Some(1));
	fs::write(deployment.join("selected.toml"), "[databases]\n").unwrap();
	let selected_config = Command::new(&deployed).current_dir(&deployment)
		.env("TABLO_CONFIG", "selected.toml").output().unwrap();
	assert_eq!(selected_config.status.code(), Some(2), "{}", String::from_utf8_lossy(&selected_config.stderr));
	fs::remove_file(deployment.join("tablo.toml")).unwrap();

	let changed_source = "fn Main(args: [text]): int { return 9; }";
	fs::write(&source_path, changed_source).unwrap();
	compile_executable(changed_source, &source_path, &deployed, None, &options).unwrap();
	assert_eq!(Command::new(&deployed).current_dir(&deployment).env_remove("TABLO_CONFIG").status().unwrap().code(), Some(9));
	let previous = fs::read(&deployed).unwrap();
	assert!(compile_executable("invalid Tablo", &source_path, &deployed, None, &options).is_err());
	assert_eq!(fs::read(&deployed).unwrap(), previous);

	let broken_runtime = directory.path().join("broken-runtime");
	fs::create_dir(&broken_runtime).unwrap();
	fs::copy(runtime_dir.join("Cargo.toml"), broken_runtime.join("Cargo.toml")).unwrap();
	fs::copy(runtime_dir.join("Cargo.lock"), broken_runtime.join("Cargo.lock")).unwrap();
	let broken_options = ExecutableBuildOptions { runtime_dir: broken_runtime, ..options };
	assert!(compile_executable(changed_source, &source_path, &deployed, None, &broken_options).is_err());
	assert_eq!(fs::read(&deployed).unwrap(), previous);
}

#[test]
#[ignore = "builds native executables with Cargo; run explicitly with --ignored"]
fn compiler_cli_builds_executable_by_default_and_honors_config_overrides() {
	let directory = tempfile::tempdir().unwrap();
	let runtime = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
	let cache = runtime.join("target/embedded-test-cache");
	fs::write(directory.path().join("main.tablo"), "fn Main(args: [text]): int { return len(args); }").unwrap();
	fs::write(directory.path().join("tablo.toml"), "[build]\nruntime = 'shared'\ndebug_info = 'none'\n").unwrap();
	let result = Command::new(env!("CARGO_BIN_EXE_tabloc"))
		.args(["main.tablo", "--runtime", "embedded", "--runtime-dir"]).arg(&runtime)
		.arg("--cache-dir").arg(&cache).arg("--offline").current_dir(directory.path()).output().unwrap();
	assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
	let application = directory.path().join("main");
	assert_eq!(&fs::read(&application).unwrap()[..4], b"\x7fELF");
	fs::remove_file(directory.path().join("main.tablo")).unwrap();
	let result = Command::new(&application).args(["--help", "second argument"]).current_dir(directory.path())
		.env_remove("TABLO_CONFIG").output().unwrap();
	assert_eq!(result.status.code(), Some(2), "{}", String::from_utf8_lossy(&result.stderr));

	let original = fs::read(&application).unwrap();
	fs::write(directory.path().join("source"), "fn Main(args: [text]): int { return 0; }").unwrap();
	let collision = Command::new(env!("CARGO_BIN_EXE_tabloc"))
		.args(["source", "--runtime", "embedded", "--runtime-dir"]).arg(&runtime)
		.arg("--cache-dir").arg(&cache).arg("--offline").current_dir(directory.path()).output().unwrap();
	assert_eq!(collision.status.code(), Some(1));
	assert!(String::from_utf8_lossy(&collision.stderr).contains("would overwrite a source file"));
	assert_eq!(fs::read(&application).unwrap(), original);
}
