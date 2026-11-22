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
fn compiled_library_customers_share_identity_and_preserve_private_defaults() {
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	fs::write(root.join("Customers.tablo"), "
		pub obj Customer { pub name: text, code: int = 41 };
		pub fn Code(customer: Customer): int { return customer.code; }
		pub fn Make(label: text): Customer { return Customer { name: label }; }
		pub fn Reject(customer: Customer) { fail customer; }
	").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "--debug-info", "none", "Customers.tablo"]));
	fs::remove_file(root.join("Customers.tablo")).unwrap();

	fs::write(root.join("Aardvark.tablo"), "pub obj Marker { pub value: int };").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "Aardvark.tablo"]));
	fs::remove_file(root.join("Aardvark.tablo")).unwrap();

	let imports = "use Marker from './Aardvark'; use Make, Code, Reject from './Customers';";
	fs::write(root.join("main.tablo"), format!("{imports}
		fn Main(args: [text]): int {{
			var customer: Customer = Customer {{ name: 'Ada' }};
			var other: Customer = Make('Ada');
			if customer.name != 'Ada' or other != customer {{ return 0; }}
			try {{ Reject(other); }}
			catch marker: Marker {{ return 0; }}
			catch caught: Customer {{ return Code(caught) + 1; }}
			return 0;
		}}")).unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program_with_arguments(&program, &[]).unwrap(), Some(tablo::value::Value::Integer(42)));

	fs::write(root.join("main.tablo"), format!("{imports} fn Main(args: [text]): int {{ return Make('Ada').code; }}")).unwrap();
	let denied = compiler(root, &["--emit", "object", "main.tablo"]);
	assert!(!denied.status.success());
	assert!(String::from_utf8_lossy(&denied.stderr).contains("private"), "{}", String::from_utf8_lossy(&denied.stderr));

	// A changed interface default must not override the compiled layout.
	let path = root.join("Customers.tlib");
	let mut bytes = fs::read(&path).unwrap();
	let manifest_len = u32::from_le_bytes(bytes[4..8].try_into().unwrap()) as usize;
	let default = b"code: int = 41";
	let offset = bytes[8..8 + manifest_len].windows(default.len()).position(|text| text == default).unwrap();
	bytes[8 + offset + default.len() - 1] = b'2';
	fs::write(path, bytes).unwrap();
	let mismatch = compiler(root, &["--emit", "object", "main.tablo"]);
	assert!(!mismatch.status.success());
	assert!(String::from_utf8_lossy(&mismatch.stderr).contains("compiled descriptor"));

}

#[test]
fn library_object_dependencies_survive_a_second_link() {
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	fs::write(root.join("Model.tablo"), r#"
		obj Details { level: int = 7 };
		pub obj Customer { pub name: text, details: Details = Details { level: 19 } };
		pub fn Read(): int {
			var c: Customer = Customer(json('{"name":"Ada"}'));
			try { fail c; }
			catch value: Customer { return value.details.level; }
			return 0;
		}
	"#).unwrap();
	assert_success(&compiler(root, &["--emit", "library", "Model.tablo"]));
	fs::remove_file(root.join("Model.tablo")).unwrap();

	fs::write(root.join("Facade.tablo"), "use Read from './Model'; pub fn Result(): int { return Read(); }").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "--debug-info", "none", "Facade.tablo"]));
	fs::remove_file(root.join("Model.tlib")).unwrap();
	fs::remove_file(root.join("Facade.tablo")).unwrap();

	fs::write(root.join("main.tablo"), "
		use Result from './Facade';
		obj Customer { unrelated: text };
		fn Main(args: [text]): int { return Result(); }
	").unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	fs::remove_file(root.join("Facade.tlib")).unwrap();
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program_with_arguments(&program, &[]).unwrap(), Some(tablo::value::Value::Integer(19)));
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
		"obj Hidden { value: int }; pub fn Value(item: Hidden): int { return item.value; }",
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

#[test]
fn compiled_signatures_share_the_declaring_librarys_object() {
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	fs::write(root.join("Models.tablo"), "
		pub obj Address { pub city: text };
		pub obj Customer { pub name: text, pub address: Address, code: int = 41 };
		pub fn Code(c: Customer): int { return c.code; }
	").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "--debug-info", "none", "Models.tablo"]));
	fs::remove_file(root.join("Models.tablo")).unwrap();
	fs::write(root.join("Orders.tablo"), "
		use Customer from './Models';
		pub fn FindCustomer(): Customer { return Customer { name: 'Ada', address: Address { city: 'Bath' } }; }
		pub fn Echo(c: Customer): Customer { return c; }
		pub fn Echo(value: text): text { return value; }
	").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "--debug-info", "none", "Orders.tablo"]));
	fs::remove_file(root.join("Orders.tablo")).unwrap();
	for imports in [
		"use FindCustomer, Echo from './Orders'; use Code from './Models';",
		"use Code from './Models'; use FindCustomer, Echo from './Orders';",
	] {
		fs::write(root.join("main.tablo"), format!("{imports} fn Main(args: [text]): int {{
			var c: Customer = Echo(FindCustomer());
			if c.name != 'Ada' {{ return 0; }}
			return Code(c) + 1;
		}}")).unwrap();
		assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
		let program = read_program_from_path(root.join("main.tobj")).unwrap();
		assert_eq!(tablo::run_program_with_arguments(&program, &[]).unwrap(), Some(tablo::value::Value::Integer(42)));
	}

	fs::write(root.join("main.tablo"), "use Echo from './Orders';
		fn Main(args: [text]): int {
			var c: Customer = Customer { name: 'Ada', address: Address { city: 'Bath' } };
			return Echo(c).name == Echo('Ada') ? 1 : 0;
		}").unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program_with_arguments(&program, &[]).unwrap(), Some(tablo::value::Value::Integer(1)));

	fs::write(root.join("Other.tablo"), "pub obj Customer { pub name: text };").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "Other.tablo"]));
	fs::remove_file(root.join("Other.tablo")).unwrap();
	fs::write(root.join("main.tablo"), "use Customer from './Other'; use FindCustomer from './Orders';
		fn Main(args: [text]): int { return 0; }").unwrap();
	let conflict = compiler(root, &["--emit", "object", "main.tablo"]);
	assert!(!conflict.status.success());
	assert!(String::from_utf8_lossy(&conflict.stderr).contains("conflicts"), "{}", String::from_utf8_lossy(&conflict.stderr));

	fs::write(root.join("Relay.tablo"), "use FindCustomer from './Orders'; pub fn Fetch(): Customer { return FindCustomer(); }").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "Relay.tablo"]));
	fs::remove_file(root.join("Relay.tablo")).unwrap();
	fs::remove_file(root.join("Orders.tlib")).unwrap();

	fs::write(root.join("main.tablo"), "use Fetch from './Relay'; use Code from './Models';
		fn Main(args: [text]): int { return Code(Fetch()); }").unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program_with_arguments(&program, &[]).unwrap(), Some(tablo::value::Value::Integer(41)));

	fs::remove_file(root.join("Models.tlib")).unwrap();
	fs::write(root.join("main.tablo"), "use Fetch from './Relay';
		fn Main(args: [text]): int { var c: Customer = Fetch(); return c.code; }").unwrap();
	let denied = compiler(root, &["--emit", "object", "main.tablo"]);
	assert!(!denied.status.success());
	assert!(String::from_utf8_lossy(&denied.stderr).contains("private"), "{}", String::from_utf8_lossy(&denied.stderr));
	fs::write(root.join("main.tablo"), "use Fetch from './Relay'; fn Main(args: [text]): int {
		var a: Address = Fetch().address; return a.city == 'Bath' ? 1 : 0;
	}").unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	fs::remove_file(root.join("Relay.tlib")).unwrap();
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program_with_arguments(&program, &[]).unwrap(), Some(tablo::value::Value::Integer(1)));
}

#[test]
fn compiled_record_parameters_share_the_callers_buffer_and_require_the_declaring_table() {
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	let schema = "schema Main implicit; create table Customers (Id int primary key, Name text not null);";

	for name in ["Sales", "Archive"] {
		fs::write(root.join(format!("{name}.sql")), format!("database {name}; {schema}")).unwrap();
	}
	fs::write(root.join("tablo.toml"), "[databases]\nSales = 'sqlite:sales.sqlite'\nArchive = 'sqlite:archive.sqlite'\n[schemas]\nSales = 'Sales.sql'\nArchive = 'Archive.sql'\n").unwrap();
	let database_path = root.join("sales.sqlite");
	let database = rusqlite::Connection::open(&database_path).unwrap();
	database.execute_batch("CREATE TABLE Customers (Id INTEGER PRIMARY KEY, Name TEXT NOT NULL);
		INSERT INTO Customers VALUES (1, 'Ada'), (2, 'Grace');").unwrap();

	fs::write(root.join("Customers.tablo"), "with Sales;
		pub fn CustomerName(c: &rec Customers): text { return c.Name; }
		pub fn RenameCustomer(c: &rec mut Customers, name: text) { c.Name = name; }
		pub fn SelectSecond(c: &rec mut Customers) { c = find first Customers where Id == 2; }").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "--debug-info", "none", "Customers.tablo"]));
	fs::remove_file(root.join("Customers.tablo")).unwrap();

	let imports = "use './Customers.tlib'; with Sales; with Archive;";
	fs::write(root.join("main.tablo"), format!("{imports}
		fn Main(args: [text]): int {{
			if rec existing = find first Sales.Main.Customers where Id == 1 {{
				if CustomerName(&existing) != 'Ada' {{ return 0; }}
			}}
			{{
				rec mut c = find first Sales.Main.Customers where Id == 1;
				if CustomerName(&c) != 'Ada' {{ return 0; }}
				RenameCustomer(&c, 'Mina');
				if c.Name != 'Mina' {{ return 0; }}
				if (count Sales.Main.Customers where Name == 'Ada') != 1 {{ return 0; }}
				SelectSecond(&c);
				if CustomerName(&c) != 'Grace' {{ return 0; }}
				RenameCustomer(&c, 'Noor');
			}}
			return count Sales.Main.Customers where Name == 'Mina' or Name == 'Noor';
		}}")).unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	let config = tablo::database::RuntimeDatabaseConfig::new().with_sqlite_database("Sales", &database_path);
	assert_eq!(tablo::run_program_with_database_config(&program, config).unwrap(), Some(tablo::value::Value::Integer(2)));

	for (active, call, message) in [
		("Archive", "rec mut c = find first Archive.Main.Customers; return len(CustomerName(&c));", "requires table `Sales.Main.Customers`, found `Archive.Main.Customers`"),
		("Sales", "if rec c = find first Sales.Main.Customers { RenameCustomer(&c, 'X'); } return 0;", "mutable"),
		("Sales", "rec mut c = find first Sales.Main.Customers; return len(CustomerName(c));", "reference"),
	] {
		fs::write(root.join("main.tablo"), format!("use './Customers'; with {active}; fn Main(args: [text]): int {{ {call} }}")).unwrap();
		let result = compiler(root, &["--emit", "object", "main.tablo"]);
		assert!(!result.status.success());
		assert!(String::from_utf8_lossy(&result.stderr).contains(message), "{}", String::from_utf8_lossy(&result.stderr));
	}
}

#[test]
fn compiled_sequence_parameters_preserve_the_declaring_sequence() {
	let directory = tempfile::tempdir().unwrap();
	let root = directory.path();
	for name in ["Sales", "Archive"] {
		fs::write(root.join(format!("{name}.sql")), format!("database {name}; schema Main implicit; create sequence InvoiceNumber;")).unwrap();
	}
	fs::write(root.join("tablo.toml"), "[databases]\nSales = 'sqlite:sales.sqlite'\nArchive = 'sqlite:archive.sqlite'\n[schemas]\nSales = 'Sales.sql'\nArchive = 'Archive.sql'\n").unwrap();

	fs::write(root.join("Counters.tablo"), "with Sales;
		fn Next(s: seq InvoiceNumber): int { return seqnext(s); }
		pub fn Advance(s: seq InvoiceNumber): int { return Next(s); }").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "--debug-info", "none", "Counters.tablo"]));
	fs::remove_file(root.join("Counters.tablo")).unwrap();

	for (active, argument, expected) in [
		("Archive", "InvoiceNumber", "Sales.Main.InvoiceNumber"),
		("Sales", "4", "Sales.Main.InvoiceNumber"),
		("Sales", "&InvoiceNumber", "without `&`"),
	] {
		fs::write(root.join("main.tablo"), format!("use Advance from './Counters'; with {active};
			fn Main(args: [text]): int {{ return Advance({argument}); }}")).unwrap();
		let output = compiler(root, &["--emit", "object", "main.tablo"]);
		assert!(!output.status.success());
		assert!(String::from_utf8_lossy(&output.stderr).contains(expected), "{}", String::from_utf8_lossy(&output.stderr));
	}

	fs::write(root.join("Invoices.tablo"), "use Advance from './Counters'; with Sales;
		pub fn Issue(s: seq Sales.Main.InvoiceNumber): int { return Advance(s); }").unwrap();
	assert_success(&compiler(root, &["--emit", "library", "Invoices.tablo"]));
	fs::remove_file(root.join("Invoices.tablo")).unwrap();
	fs::remove_file(root.join("Counters.tlib")).unwrap();

	fs::write(root.join("main.tablo"), "use Issue from './Invoices'; with Sales;
		fn Main(args: [text]): int {
			InvoiceNumber = 4;
			if Issue(InvoiceNumber) != 5 { return 0; }
			return InvoiceNumber;
		}").unwrap();
	assert_success(&compiler(root, &["--emit", "object", "main.tablo"]));
	fs::write(root.join("Sales.sql"), "database Sales; schema Main; create sequence InvoiceNumber;").unwrap();
	let mismatch = compiler(root, &["--emit", "object", "main.tablo"]);
	assert!(!mismatch.status.success());
	assert!(String::from_utf8_lossy(&mismatch.stderr).contains("different schema qualification"), "{}", String::from_utf8_lossy(&mismatch.stderr));
	fs::remove_file(root.join("Invoices.tlib")).unwrap();

	let path = root.join("sales.sqlite");
	let database = rusqlite::Connection::open(&path).unwrap();
	database.execute_batch("CREATE TABLE InvoiceNumber (Id INTEGER PRIMARY KEY AUTOINCREMENT);
		INSERT INTO InvoiceNumber DEFAULT VALUES;").unwrap();
	let config = tablo::database::RuntimeDatabaseConfig::new().with_sqlite_database("Sales", &path);
	let program = read_program_from_path(root.join("main.tobj")).unwrap();
	assert_eq!(tablo::run_program_with_database_config(&program, config).unwrap(), Some(tablo::value::Value::Integer(5)));
}
