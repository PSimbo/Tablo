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
		vec!["--emit", "library", "--runtime", "shared", "main.tablo"],
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

#[test]
fn links_relocated_libraries_and_transitive_dependencies_without_sources() {
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	fs::create_dir(root.join("source")).unwrap();
	fs::create_dir(root.join("deploy")).unwrap();
	fs::write(root.join("source/Helper.tablo"), "pub fn Twice(value: int): int { return value * 2; }").unwrap();
	fs::write(root.join("source/Math.tablo"), "use './Helper';
		fn Hidden(value: int): int { fn Local(value: int): int { return Twice(value); } return Local(value); }
		pub fn Answer(value: int = 20): int { return Hidden(value) + 2; }
		pub fn Answer(value: text): int { return len(value); }
		pub fn Factorial(value: int): int { if value <= 1 { return 1; } return value * Factorial(value - 1); }").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "--debug-info", "full", "source/Math.tablo", "-o", "deploy/Math.tlib"]));
	fs::remove_dir_all(root.join("source")).unwrap();
	fs::write(root.join("deploy/Facade.tablo"), "use './Math'; pub fn Result(): int { return Answer() + Answer('abc') + Factorial(4); }").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "deploy/Facade.tablo"]));
	fs::remove_file(root.join("deploy/Math.tlib")).unwrap();
	fs::remove_file(root.join("deploy/Facade.tablo")).unwrap();
	fs::write(root.join("main.tablo"), "use Result from './deploy/Facade.tlib'; fn Main(args: [text]): int { return Result(); }").unwrap();
	assert_success(&compiler(root, &["--emit", "object", "--debug-info", "full", "main.tablo"]));
	fs::remove_dir_all(root.join("deploy")).unwrap();
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program_with_arguments(&program, &[]).unwrap(), Some(tablo::value::Value::Integer(69)));
	assert!(program.debug_info().source_files().iter().any(|source| source.display_name().ends_with("Math.tablo")));
}

#[test]
fn library_interfaces_enforce_visibility_types_and_preserve_outputs() {
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	let source = "fn Hidden(): int { return 42; } pub fn Value(value: int): int { return value + Hidden(); }";
	fs::write(root.join("Math.tablo"), source).unwrap();
	assert_success(&compiler(root, &["--emit", "library", "--debug-info", "none", "Math.tablo"]));
	let bytes = fs::read(root.join("Math.tlib")).unwrap();
	for (statement, message) in [
		("use Hidden from './Math.tlib'; fn Main(args: [text]): int { return Hidden(); }", "not exported"),
		("use './Math.tlib'; fn Main(args: [text]): int { return Value('bad'); }", "type `text`"),
	] {
		fs::write(root.join("main.tablo"), statement).unwrap();
		let output = compiler(root, &["--emit", "object", "main.tablo"]);
		assert_eq!(output.status.code(), Some(1));
		assert!(String::from_utf8_lossy(&output.stderr).contains(message), "{}", String::from_utf8_lossy(&output.stderr));
		assert!(!root.join("main.tobj").exists());
	}
	let overwrite = compiler(root, &["--emit", "library", "--debug-info", "none", "Math.tablo", "-o", "Math.tablo"]);
	assert_eq!(overwrite.status.code(), Some(1));
	assert_eq!(fs::read_to_string(root.join("Math.tablo")).unwrap(), source);
	fs::write(root.join("Math.tablo"), "pub fn Value(): int { return 'invalid'; }").unwrap();
	assert_eq!(compiler(root, &["--emit", "library", "Math.tablo"]).status.code(), Some(1));
	assert_eq!(fs::read(root.join("Math.tlib")).unwrap(), bytes);
	fs::write(root.join("main.tablo"), "use './Math.tlib'; fn Main(args: [text]): int { return Value(0); }").unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program_with_arguments(&program, &[]).unwrap(), Some(tablo::value::Value::Integer(42)));
	for bytes in [b"TLB1".as_slice(), b"TLB1\xff\xff\xff\xff", b"invalid library"] {
		fs::write(root.join("Math.tlib"), bytes).unwrap();
		let output = compiler(root, &["--emit", "object", "main.tablo"]);
		assert_eq!(output.status.code(), Some(1));
		assert!(String::from_utf8_lossy(&output.stderr).contains("library"), "{}", String::from_utf8_lossy(&output.stderr));
	}
}

#[test]
fn library_calls_preserve_reference_variadic_and_default_arguments() {
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	fs::write(root.join("Calls.tablo"), "pub fn Increment(value: &int) { value += 1; }
		pub fn Sum(start: int = 4, ...values: [int]): int { var total: int = start; for value in values { total += value; } return total; }
		pub fn Echo(value: [text]): [text] { return value; }").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "Calls.tablo"]));
	fs::remove_file(root.join("Calls.tablo")).unwrap();
	fs::write(root.join("main.tablo"), "use './Calls'; fn Main(args: [text]): int {
		var value: int = 5; Increment(&value); return value + Sum(start: default, 2, 3) + len(Echo(['a', 'b'])); }").unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program_with_arguments(&program, &[]).unwrap(), Some(tablo::value::Value::Integer(17)));
}

#[test]
fn links_library_queries_alongside_consumer_queries() {
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	fs::write(root.join("Queries.tablo"), "with ExampleDb; pub fn CountCustomers(): int { return count Customers; }").unwrap();
	fs::write(root.join("schema.sql"), "database ExampleDb; schema Main implicit; create table Customers (Id int not null);").unwrap();
	fs::write(root.join("tablo.toml"), "[databases]\nExampleDb = 'sqlite:data.sqlite'\n[schemas]\nExampleDb = 'schema.sql'\n").unwrap();
	let database_path = root.join("data.sqlite");
	let database = rusqlite::Connection::open(&database_path).unwrap();
	database.execute_batch("CREATE TABLE Customers (Id INTEGER NOT NULL); INSERT INTO Customers VALUES (1), (2);").unwrap();
	drop(database);
	assert_success(&compiler(root, &["--emit", "library", "Queries.tablo"]));
	fs::remove_file(root.join("Queries.tablo")).unwrap();
	fs::write(root.join("main.tablo"), "use './Queries'; with ExampleDb; fn Main(args: [text]): int { return (count Customers where Id == 1) + CountCustomers(); }").unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	fs::remove_file(root.join("Queries.tlib")).unwrap();
	let mut config = tablo::database::RuntimeDatabaseConfig::new();
	config.set_database_connection_string("ExampleDb", &format!("sqlite:{}", database_path.display())).unwrap();
	assert_eq!(tablo::run_program_with_database_config(&program, config).unwrap(), Some(tablo::value::Value::Integer(3)));
}

#[test]
fn rejects_unsupported_library_interfaces_and_top_level_execution() {
	let directory = tempfile::tempdir().unwrap();
	for source in [
		"pub obj Item { value: int } pub fn Make(): Item { return Item { value: 1 }; }",
		"pub fn Value(): int { return 1; } var value: int = Value();",
		"fn Private(): int { return 1; }",
		"fn Private(): int { return 1; } pub fn Value(value: int = Private()): int { return value; }",
	] {
		fs::write(directory.path().join("library.tablo"), source).unwrap();
		let output = compiler(directory.path(), &["--emit", "library", "library.tablo"]);
		assert_eq!(output.status.code(), Some(1), "{}", String::from_utf8_lossy(&output.stderr));
		assert!(!directory.path().join("library.tlib").exists());
	}
}
