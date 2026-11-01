use std::fs;
use std::process::{Command, Output};

use tablo::bytecode::Program;
use tablo::object_file::read_program_from_path;

fn compiler(directory: &std::path::Path, arguments: &[&str]) -> Output {
	Command::new(env!("CARGO_BIN_EXE_tabloc")).args(arguments).current_dir(directory)
		.env("PATH", "").output().unwrap()
}

fn assert_success(output: &Output) {
	assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

fn has_locals(program: &Program) -> bool {
	program.debug_info().code_bodies().iter().any(|body| !body.locals().is_empty())
}

#[test]
fn writes_objects_without_native_tools_and_selects_debug_information() {
	let directory = tempfile::tempdir().unwrap();
	let source = "fn Main(args: [text]): int { var value: int = len(args); return value; }";
	fs::write(directory.path().join("main.tablo"), source).unwrap();
	assert_success(&compiler(directory.path(), &["--emit", "object", "main.tablo"]));
	let object_path = directory.path().join("main.tobj");
	let lines = read_program_from_path(&object_path).unwrap();
	assert!(!lines.debug_info().is_empty());
	assert!(!has_locals(&lines));
	assert_eq!(tablo::run_program_with_arguments(&lines, &["hello".into()]).unwrap(), Some(tablo::value::Value::Integer(1)));

	fs::write(directory.path().join("tablo.toml"), "[build]\ndebug_info = 'none'\nruntime = 'shared'\n").unwrap();
	assert_success(&compiler(directory.path(), &["--emit", "object", "main.tablo"]));
	assert!(read_program_from_path(&object_path).unwrap().debug_info().is_empty());
	assert_success(&compiler(directory.path(), &["--emit", "object", "--debug-info", "full", "main.tablo"]));
	let full = read_program_from_path(&object_path).unwrap();
	assert!(has_locals(&full));
	assert_eq!(lines.debug_info().source_files(), full.debug_info().source_files());
}

#[test]
fn rejects_old_arguments_and_unsupported_output_without_touching_files() {
	let directory = tempfile::tempdir().unwrap();
	for arguments in [
		vec!["main.tablo", "main.tbo"],
		vec!["--schema", "schema.sql", "main.tablo"],
		vec!["--emit", "library", "main.tablo"],
		vec!["--emit", "object", "--runtime", "embedded", "main.tablo"],
		vec!["--emit", "object", "--offline", "main.tablo"],
	] {
		let output = compiler(directory.path(), &arguments);
		assert_eq!(output.status.code(), Some(2), "{:?}: {}", arguments, String::from_utf8_lossy(&output.stderr));
	}
	assert_eq!(fs::read_dir(directory.path()).unwrap().count(), 0);
	assert_success(&compiler(directory.path(), &["--help"]));
	assert_success(&compiler(directory.path(), &["--version"]));
}

#[test]
fn protects_sources_and_previous_outputs_even_without_debug_records() {
	let directory = tempfile::tempdir().unwrap();
	let input = directory.path().join("main.tablo");
	let helper = directory.path().join("Helper.tablo");
	let source = "use './Helper'; fn Main(args: [text]): int { return Answer(); }";
	fs::write(&input, source).unwrap();
	fs::write(&helper, "pub fn Answer(): int { return 42; }").unwrap();
	for output_path in ["main.tablo", "Helper.tablo"] {
		let result = compiler(directory.path(), &["--emit", "object", "--debug-info", "none", "-o", output_path, "main.tablo"]);
		assert_eq!(result.status.code(), Some(1));
		assert!(String::from_utf8_lossy(&result.stderr).contains("would overwrite a source file"));
	}
	assert_eq!(fs::read_to_string(&input).unwrap(), source);
	assert_success(&compiler(directory.path(), &["--emit", "object", "main.tablo"]));
	let object_path = directory.path().join("main.tobj");
	let previous = fs::read(&object_path).unwrap();
	fs::write(&input, "invalid Tablo").unwrap();
	assert_eq!(compiler(directory.path(), &["--emit", "object", "main.tablo"]).status.code(), Some(1));
	assert_eq!(fs::read(&object_path).unwrap(), previous);
}

#[test]
fn reports_missing_explicit_config_and_runtime_bundle() {
	let directory = tempfile::tempdir().unwrap();
	fs::write(directory.path().join("main.tablo"), "fn Main(args: [text]): int { return 0; }").unwrap();
	let missing = compiler(directory.path(), &["--emit", "object", "--config", "missing.toml", "main.tablo"]);
	assert_eq!(missing.status.code(), Some(1));
	assert!(String::from_utf8_lossy(&missing.stderr).contains("missing.toml"));
	let executable = directory.path().join("tabloc");
	fs::copy(env!("CARGO_BIN_EXE_tabloc"), &executable).unwrap();
	let missing_runtime = Command::new(executable).arg("main.tablo").current_dir(directory.path()).output().unwrap();
	assert_eq!(missing_runtime.status.code(), Some(1));
	assert!(String::from_utf8_lossy(&missing_runtime.stderr).contains("No runtime source bundle"));
	assert!(!directory.path().join("main").exists());
}
