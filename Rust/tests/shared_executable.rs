#![cfg(target_os = "linux")]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn build(source: &Path, output: &Path, mode: Option<&str>) -> Output {
	let runtime = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
	let mut command = Command::new(env!("CARGO_BIN_EXE_tabloc"));
	command.arg(source).arg("-o").arg(output)
		.arg("--runtime-dir").arg(&runtime)
		.arg("--cache-dir").arg(runtime.join("target/embedded-test-cache"))
		.arg("--offline").current_dir(source.parent().unwrap());
	if let Some(mode) = mode {
		command.args(["--runtime", mode]);
	}
	command.output().unwrap()
}

fn run(application: &Path, arguments: &[&str]) -> Output {
	Command::new(application).args(arguments).current_dir(application.parent().unwrap())
		.env_remove("TABLO_CONFIG").env_remove("LD_LIBRARY_PATH").output().unwrap()
}

fn assert_success(output: &Output) {
	assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

#[test]
#[ignore = "builds native executables and a shared runtime with Cargo; run explicitly with --ignored"]
fn shares_relocates_and_validates_a_runtime_without_embedding_it() {
	let directory = tempfile::tempdir().unwrap();
	let source = directory.path().join("main.tablo");
	fs::write(&source, "fn Main(args: [text]): int { if len(args) == 0 { return 1 / len(args); } if args[1] == '--help' { return len(args) + 5; } return 0; }").unwrap();
	fs::write(directory.path().join("tablo.toml"), "[build]\nruntime = 'shared'\n").unwrap();
	let deployment = directory.path().join("dist");
	fs::create_dir(&deployment).unwrap();
	let embedded = deployment.join("embedded");
	let shared = deployment.join("shared");
	assert_success(&build(&source, &embedded, Some("embedded")));
	assert_success(&build(&source, &shared, None));
	let library = fs::read_dir(deployment.join("lib")).unwrap().map(|entry| entry.unwrap().path())
		.find(|path| path.extension().is_some_and(|extension| extension == "so")).unwrap();
	let library_name = library.file_name().unwrap().to_owned();
	assert!(library_name.to_str().unwrap().ends_with("_none.so"));
	let original_library = fs::read(&library).unwrap();
	let modified = fs::metadata(&library).unwrap().modified().unwrap();
	let second = deployment.join("second");
	assert_success(&build(&source, &second, None));
	assert_eq!(fs::metadata(&library).unwrap().modified().unwrap(), modified);
	assert_eq!(fs::read(&library).unwrap(), original_library);
	assert!(fs::metadata(&shared).unwrap().len() < fs::metadata(&embedded).unwrap().len());

	for application in [&embedded, &shared, &second] {
		let result = run(application, &["--help", "two words"]);
		assert_eq!(result.status.code(), Some(7), "{}", String::from_utf8_lossy(&result.stderr));
		let failed = run(application, &[]);
		assert_eq!(failed.status.code(), Some(1));
		assert!(String::from_utf8_lossy(&failed.stderr).to_lowercase().contains("division by zero"));
	}
	let missing_config = Command::new(&shared).env("TABLO_CONFIG", "missing.toml").current_dir(&deployment).output().unwrap();
	assert_eq!(missing_config.status.code(), Some(1));
	assert!(String::from_utf8_lossy(&missing_config.stderr).contains("Failed to load runtime config"));

	let dynamic = Command::new("readelf").arg("-d").arg(&shared).output().unwrap();
	assert_success(&dynamic);
	let tags = String::from_utf8_lossy(&dynamic.stdout);
	assert!(tags.lines().any(|line| line.contains("NEEDED") && line.contains(library_name.to_str().unwrap())));
	assert!(tags.lines().any(|line| line.contains("RUNPATH") && line.contains("[$ORIGIN/lib]")));

	let old_executable = fs::read(&shared).unwrap();
	fs::write(&library, "damaged runtime").unwrap();
	let conflict = build(&source, &shared, None);
	assert!(!conflict.status.success());
	assert!(String::from_utf8_lossy(&conflict.stderr).contains("could not be verified as compatible"));
	assert_eq!(fs::read(&shared).unwrap(), old_executable);
	assert_eq!(fs::read(&library).unwrap(), b"damaged runtime");
	fs::write(&library, &original_library).unwrap();

	let relocated = directory.path().join("relocated application");
	fs::rename(&deployment, &relocated).unwrap();
	fs::remove_file(&source).unwrap();
	let shared = relocated.join("shared");
	let library = relocated.join("lib").join(&library_name);
	fs::remove_file(library.with_extension("so.json")).unwrap();
	assert_eq!(run(&shared, &["--help", "relocated"]).status.code(), Some(7));
	let hidden = library.with_extension("hidden");
	fs::rename(&library, &hidden).unwrap();
	let missing = run(&shared, &["--help"]);
	assert!(!missing.status.success());
	assert!(String::from_utf8_lossy(&missing.stderr).contains(library_name.to_str().unwrap()));
	fs::rename(&hidden, &library).unwrap();

	// Supply a callable but incompatible runtime. Its run entry point aborts if
	// invoked, so an ordinary exit proves the launcher rejected the version first.
	let stub = directory.path().join("incompatible.rs");
	fs::write(&stub, r#"
#[repr(C)] pub struct Version { major: u64, minor: u64, patch: u64 }
#[repr(C)] pub struct Argument { data: *const u8, len: usize }
#[unsafe(no_mangle)] pub extern "C" fn tablo_runtime_version() -> Version { Version { major: 99, minor: 0, patch: 0 } }
#[unsafe(no_mangle)] pub extern "C" fn tablo_runtime_drivers() -> u32 { 7 }
#[unsafe(no_mangle)] pub unsafe extern "C" fn tablo_runtime_run(_: *const u8, _: usize, _: Version, _: *const Argument, _: usize, _: *mut i32) -> i32 { std::process::abort() }
"#).unwrap();
	fs::rename(&library, &hidden).unwrap();
	let compile_stub = Command::new("rustc").args(["--edition", "2024", "--crate-type", "cdylib"])
		.arg(&stub).arg("-o").arg(&library).output().unwrap();
	assert_success(&compile_stub);
	let incompatible = run(&shared, &["--help"]);
	assert_eq!(incompatible.status.code(), Some(1));
	assert!(String::from_utf8_lossy(&incompatible.stderr).contains("cannot run on runtime 99.0.0"));
}

#[test]
#[ignore = "builds native executables with the SQLite-only runtime; run explicitly with --ignored"]
fn selects_sqlite_for_imported_queries_and_rejects_a_library_without_its_driver() {
	let directory = tempfile::tempdir().unwrap();
	let source = directory.path().join("main.tablo");
	fs::write(&source, "use CountCustomers from './Queries'; fn Main(args: [text]): int { return CountCustomers(); }").unwrap();
	fs::write(directory.path().join("Queries.tablo"), "with ExampleDb; pub fn CountCustomers(): int { return count Customers; }").unwrap();
	fs::write(directory.path().join("schema.sql"), "database ExampleDb; schema Main implicit; create table Customers (Id int not null);").unwrap();
	fs::write(directory.path().join("tablo.toml"), "[databases]\nExampleDb = 'sqlite:data.sqlite'\nUnused = 'mysql://unused:unused@localhost/unused'\n[schemas]\nExampleDb = 'schema.sql'\n").unwrap();
	let database = rusqlite::Connection::open(directory.path().join("data.sqlite")).unwrap();
	database.execute_batch("CREATE TABLE Customers (Id INTEGER NOT NULL); INSERT INTO Customers VALUES (1), (2);").unwrap();
	drop(database);
	for mode in ["embedded", "shared"] {
		let output = directory.path().join(mode);
		assert_success(&build(&source, &output, Some(mode)));
		let result = run(&output, &[]);
		assert_eq!(result.status.code(), Some(2), "{}", String::from_utf8_lossy(&result.stderr));
	}
	let library = fs::read_dir(directory.path().join("lib")).unwrap().map(|entry| entry.unwrap().path())
		.find(|path| path.extension().is_some_and(|extension| extension == "so")).unwrap();
	assert!(library.file_name().unwrap().to_str().unwrap().ends_with("_sqlite.so"));
	let stub = directory.path().join("missing_driver.rs");
	fs::write(&stub, r#"
#[repr(C)] pub struct Version { major: u64, minor: u64, patch: u64 }
#[repr(C)] pub struct Argument { data: *const u8, len: usize }
#[unsafe(no_mangle)] pub extern "C" fn tablo_runtime_version() -> Version { Version { major: 0, minor: 1, patch: 0 } }
#[unsafe(no_mangle)] pub extern "C" fn tablo_runtime_drivers() -> u32 { 0 }
#[unsafe(no_mangle)] pub unsafe extern "C" fn tablo_runtime_run(_: *const u8, _: usize, _: Version, _: *const Argument, _: usize, _: *mut i32) -> i32 { std::process::abort() }
"#).unwrap();
	let result = Command::new("rustc").args(["--edition", "2024", "--crate-type", "cdylib"]).arg(&stub).arg("-o").arg(&library).output().unwrap();
	assert_success(&result);
	let rejected = run(&directory.path().join("shared"), &[]);
	assert_eq!(rejected.status.code(), Some(1));
	assert!(String::from_utf8_lossy(&rejected.stderr).contains("does not include all required database drivers"));
}
