#![cfg(feature = "compiler")]

use tablo::database::RuntimeDatabaseConfig;
use tablo::schema::{DatabaseBackend, SchemaCatalog};
use tablo::value::Value;

struct Fixture {
	schema: SchemaCatalog,
	config: RuntimeDatabaseConfig,
}

impl Fixture {
	fn new(backend: DatabaseBackend, namespace: &str, config: RuntimeDatabaseConfig) -> Self {
		let mut schema = tablo::schema_fixture::read_schema_catalog_from_str(&format!(
			"database ExampleDb; schema {namespace};
			create table Docs (Id int not null, Payload json not null, Optional json null);"
		)).unwrap();

		schema.database_mut("ExampleDb").unwrap().set_backend(backend);

		Self { schema, config }
	}

	fn run(&self, body: &str) -> Result<Option<Value>, tablo::TabloError> {
		let source = format!("with ExampleDb; fn Main(args: [text]): int {{ {body} }}");
		let dir = tempfile::tempdir().unwrap();
		let path = dir.path().join("test.tobj");

		tablo::compile_with_source_name_and_schema(source, "json_database.tablo", &path, &self.schema)?;

		let program = tablo::object_file::read_program_from_path(path).unwrap();

		tablo::run_program_with_database_config(&program, self.config.clone())
	}

	fn succeeds(&self, body: &str) {
		assert_eq!(self.run(body).unwrap(), Some(Value::Integer(1)));
	}

	fn exercise(&self) {
		self.succeeds(r#"
			rec mut row = new Docs;
			row.Id = 1;
			row.Payload = { "name": 'Åsa', "items": [null, 2] };
			create row;
			row.Optional = [true, null];
			update row;
			return 1;
		"#);
		self.succeeds(r#"
			rec mut row = find first Docs where Id == 1;
			if row.Payload != json('{"items":[null,2],"name":"Åsa"}') or row.Optional != json('[true,null]') {
				return 0;
			}
			row.Optional = null;
			update row;
			return 1;
		"#);
		self.succeeds(r#"
			for rec row in Docs where Optional == null {
				var item: int = row.Payload.items[2];
				if row.Optional == null and item == 2 { return 1; }
			}
			return 0;
		"#);
		self.succeeds("rec mut row = find first Docs where Id == 1; delete row; return count Docs == 0 ? 1 : 0;");
	}

	#[cfg(feature = "driver-sqlite")]
	fn rejects_scalar_root(&self) {
		let error = self.run("rec row = find first Docs where Id == 8; return row.Optional == null ? 1 : 0;").unwrap_err();
		assert!(matches!(error, tablo::TabloError::Runtime(_)));
		assert!(error.to_string().contains("object or array root"), "{error}");
	}

	#[cfg(any(feature = "driver-mysql", feature = "driver-postgresql"))]
	fn exercise_scalar_rows(&self) {
		self.succeeds("
			rec mut row = find first Docs where Id == 8;
			if row.Optional != JsonNull or row.Optional == null { return 0; }
			var document: any = row.Payload;
			if document != json('{}') { return 0; }
			row.Payload = '42';
			row.Optional = null;
			update row;
			return 1;
		");

		self.succeeds("
			rec mut row = find first Docs where Id == 8;
			var s: text = row.Payload;
			if s != '42' or row.Optional != null { return 0; }
			row.Payload = 3.5;
			row.Optional = true;
			update row;
			return 1;
		");

		let error = self.run("rec row = find first Docs where Id == 8; var n: int = row.Payload; return n;").unwrap_err();
		assert!(matches!(error, tablo::TabloError::Runtime(_)));
		assert!(error.to_string().contains("int"), "{error}");

		self.succeeds("
			rec mut row = find first Docs where Id == 8;
			var n: dec = row.Payload;
			var b: bool? = row.Optional;
			if row.Payload + 0.5 != 4 or n != 3.5 or b != true { return 0; }
			row.Payload = json('null');
			update row;
			return 1;
		");

		self.succeeds("
			rec mut row = find first Docs where Id == 8;
			if row.Payload != JsonNull { return 0; }
			row.Optional = JsonNull;
			update row;
			delete row;
			return 1;
		");
	}

}

#[cfg(feature = "driver-sqlite")]
#[test]
fn sqlite_document_storage() {
	let dir = tempfile::tempdir().unwrap();
	let path = dir.path().join("records.sqlite");
	let db = rusqlite::Connection::open(&path).unwrap();

	db.execute_batch("CREATE TABLE Docs (Id INTEGER, Payload TEXT NOT NULL, Optional TEXT)").unwrap();

	let config = RuntimeDatabaseConfig::new().with_sqlite_database("ExampleDb", path);
	let fixture = Fixture::new(DatabaseBackend::Sqlite, "main implicit", config);

	fixture.exercise();

	let error = fixture.run("return count Docs where Payload == Payload;").unwrap_err();

	assert!(matches!(error, tablo::TabloError::Compile(_)));
	assert!(error.to_string().contains("compare JSON documents"), "{error}");

	db.execute("INSERT INTO Docs VALUES (2, ?, NULL)", ["{ \"z\": 1, \"a\": 2 }"]).unwrap();
	fixture.succeeds("rec mut row = find first Docs where Id == 2; row.Id = 3; update row; delete row; return count Docs == 0 ? 1 : 0;");
	db.execute("INSERT INTO Docs VALUES (8, '{}', 'null')", []).unwrap();
	fixture.rejects_scalar_root();
}

#[cfg(any(feature = "driver-postgresql", feature = "driver-mysql"))]
fn namespace() -> String {
	let stamp = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
	format!("tablo_json_{}_{stamp}", std::process::id())
}

#[cfg(feature = "driver-postgresql")]
#[test]
#[ignore = "requires dedicated PostgreSQL server via TABLO_TEST_POSTGRES"]
fn postgresql_json_and_jsonb_storage() {
	let url = std::env::var("TABLO_TEST_POSTGRES").unwrap();
	let mut db = postgres::Client::connect(&url, postgres::NoTls).unwrap();
	let schema = namespace();

	db.batch_execute(&format!("CREATE SCHEMA {schema}")).unwrap();

	let config = RuntimeDatabaseConfig::new().with_database_connection_string("ExampleDb", &format!("postgresql:{url}")).unwrap();
	let fixture = Fixture::new(DatabaseBackend::PostgreSql, &schema, config);

	let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
		for ty in ["json", "jsonb"] {
			db.batch_execute(&format!(r#"CREATE TABLE {schema}."Docs" ("Id" integer, "Payload" {ty} NOT NULL, "Optional" {ty})"#)).unwrap();
			fixture.exercise();
			db.batch_execute(&format!(r#"INSERT INTO {schema}."Docs" VALUES (8, '{{}}', 'null')"#)).unwrap();
			fixture.exercise_scalar_rows();
			db.batch_execute(&format!(r#"DROP TABLE {schema}."Docs""#)).unwrap();
		}
	}));

	db.batch_execute(&format!("DROP SCHEMA {schema} CASCADE")).unwrap();

	if let Err(error) = result {
		std::panic::resume_unwind(error);
	}
}

#[cfg(feature = "driver-mysql")]
#[test]
#[ignore = "requires dedicated MySQL server via TABLO_TEST_MYSQL"]
fn mysql_document_storage() {
	use mysql::prelude::Queryable;

	let url = std::env::var("TABLO_TEST_MYSQL").unwrap();
	let mut db = mysql::Conn::new(mysql::Opts::from_url(&url).unwrap()).unwrap();
	let schema = namespace();

	db.query_drop(format!("CREATE SCHEMA {schema}")).unwrap();

	let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
		db.query_drop(format!("CREATE TABLE {schema}.Docs (Id INTEGER, Payload JSON NOT NULL, Optional JSON)")).unwrap();

		let config = RuntimeDatabaseConfig::new().with_database_connection_string("ExampleDb", &url).unwrap();
		let fixture = Fixture::new(DatabaseBackend::MySql, &schema, config);

		fixture.exercise();
		db.query_drop(format!("INSERT INTO {schema}.Docs VALUES (8, '{{}}', 'null')")).unwrap();
		fixture.exercise_scalar_rows();

		let error = fixture.run(r#"
			rec mut row = new Docs;
			row.Id = 9;
			row.Payload = json('[1.00000000000000000000000000000000000001]');
			create row;
			return 1;
		"#).unwrap_err();

		assert!(matches!(error, tablo::TabloError::Runtime(_)));
		assert!(error.to_string().contains("losing precision"), "{error}");
		fixture.succeeds("var rows: int = count Docs where Id == 9; return rows == 0 ? 1 : 0;");
	}));

	db.query_drop(format!("DROP SCHEMA {schema}")).unwrap();

	if let Err(error) = result {
		std::panic::resume_unwind(error);
	}
}
