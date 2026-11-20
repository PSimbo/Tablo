#![cfg(all(feature = "compiler", feature = "driver-mysql", feature = "driver-postgresql"))]

use mysql::prelude::Queryable;
use tablo::database::RuntimeDatabaseConfig;
use tablo::schema::DatabaseBackend;
use tablo::value::Value;

// These tests create and drop a uniquely named namespace in the supplied server.
// Supply dedicated test servers, not production connection strings.
enum Connection {
	MySql(mysql::Conn),
	PostgreSql(postgres::Client),
}

struct Fixture {
	connection: Connection,
	namespace: String,
	url: String,
	backend: DatabaseBackend,
}

impl Fixture {
	fn new(backend: DatabaseBackend, url: String) -> Self {
		let timestamp = std::time::SystemTime::now()
			.duration_since(std::time::UNIX_EPOCH)
			.unwrap()
			.as_nanos();
		let namespace = format!("tablo_float_{}_{timestamp}", std::process::id());
		let connection = match backend {
			DatabaseBackend::MySql => {
				let options = mysql::Opts::from_url(&url).unwrap();
				Connection::MySql(mysql::Conn::new(options).unwrap())
			}
			DatabaseBackend::PostgreSql => {
				Connection::PostgreSql(postgres::Client::connect(&url, postgres::NoTls).unwrap())
			}
			_ => unreachable!(),
		};
		let mut fixture = Self { connection, namespace, url, backend };

		fixture.sql(&format!("CREATE SCHEMA {}", fixture.namespace));
		fixture.sql(&match backend {
			DatabaseBackend::MySql => format!(
				"CREATE TABLE {}.Numbers (
					Id INTEGER NOT NULL,
					Amount FLOAT NOT NULL,
					Wide DOUBLE NOT NULL,
					Optional FLOAT NULL
				)",
				fixture.namespace,
			),
			_ => format!(
				r#"CREATE TABLE {}."Numbers" (
					"Id" INTEGER NOT NULL,
					"Amount" REAL NOT NULL,
					"Wide" DOUBLE PRECISION NOT NULL,
					"Optional" REAL NULL
				)"#,
				fixture.namespace,
			),
		});

		fixture
	}

	fn sql(&mut self, sql: &str) {
		match &mut self.connection {
			Connection::MySql(connection) => connection.query_drop(sql).unwrap(),
			Connection::PostgreSql(connection) => connection.batch_execute(sql).unwrap(),
		}
	}

	fn run(&self, body: &str) -> Result<Option<Value>, tablo::TabloError> {
		let mut schema = tablo::schema_fixture::read_schema_catalog_from_str(&format!(
			"database ExampleDb;
			schema {};
			create table Numbers (
				Id int not null,
				Amount float not null,
				Wide float not null,
				Optional float null
			);",
			self.namespace,
		)).unwrap();

		schema.database_mut("ExampleDb").unwrap().set_backend(self.backend);

		let directory = tempfile::tempdir().unwrap();
		let output = directory.path().join("float.tobj");
		let source = format!(
			"with ExampleDb;
			fn Main(args: [text]): int {{
				{body}
			}}"
		);

		tablo::compile_with_source_name_and_schema(source, "float.tablo", &output, &schema)?;

		let program = tablo::object_file::read_program_from_path(output).unwrap();
		let config_url = if self.backend == DatabaseBackend::PostgreSql {
			format!("postgresql:{}", self.url)
		}
		else {
			self.url.clone()
		};
		let config = RuntimeDatabaseConfig::new()
			.with_database_connection_string("ExampleDb", &config_url)
			.unwrap();

		tablo::run_program_with_database_config(&program, config)
	}

	fn succeeds(&self, body: &str) {
		assert_eq!(self.run(body).unwrap(), Some(Value::Integer(1)));
	}

	fn exercise(&mut self) {
		let table = if self.backend == DatabaseBackend::MySql {
			format!("{}.Numbers", self.namespace)
		}
		else {
			format!("{}.\"Numbers\"", self.namespace)
		};

		self.sql(&format!("DELETE FROM {table}"));

		// Seed the binary64 value on the server so a 32-bit client cannot round
		// it before storage. The original value participates in both predicates.
		self.sql(&format!("INSERT INTO {table} VALUES (8, 0, 0.1, NULL)"));
		self.succeeds(r#"
			rec mut row = find first Numbers where Id == 8;

			if row.Wide != float('0.1') {
				return 0;
			}

			row.Id = 9;
			update row;
			delete row;

			var remaining: int = count Numbers;
			return remaining == 0 ? 1 : 0;
		"#);

		// Create rounds once; both subsequent writes use the rounded identity (no primary key).
		self.succeeds(r#"
			rec mut row = new Numbers;
			row.Id = 1;
			row.Amount = 1.000000059604644775390625;
			row.Wide = 0.1;
			create row;

			if row.Amount != float(1) {
				return 0;
			}

			rec stored = find first Numbers where Id == 1;

			if stored.Wide != float('0.1') {
				return 0;
			}

			row.Wide = 0.25;
			update row;
			row.Amount = 1.000000178813934326171875;
			update row;

			if row.Amount != float('1.0000002384185791015625') {
				return 0;
			}

			return 1;
		"#);

		self.succeeds(r#"
			rec mut row = find first Numbers where Id == 1;

			if row.Amount != float('1.0000002384185791015625')
				or row.Wide != float('0.25') or row.Optional != null {
				return 0;
			}

			row.Amount = float('1.401298464324817e-45');
			update row;
			row.Wide = 0.5;
			update row;

			return 1;
		"#);

		self.succeeds(r#"
			rec mut row = find first Numbers where Id == 1;

			if row.Amount != float('1.401298464324817e-45') {
				return 0;
			}

			row.Amount = float('-1e-99');
			update row;
			row.Wide = 0.75;
			update row;

			return 1;
		"#);

		if self.backend == DatabaseBackend::PostgreSql {
			self.succeeds(r#"
				rec mut row = find first Numbers where Id == 1;

				if float(1) / row.Amount != float('-inf') {
					return 0;
				}

				row.Amount = float('1e40');
				update row;

				if row.Amount != float('inf') {
					return 0;
				}

				row.Wide = 1.0;
				update row;

				return 1;
			"#);

			self.succeeds(r#"
				rec mut row = find first Numbers where Id == 1;

				if row.Amount != float('inf') {
					return 0;
				}

				row.Amount = float('-1e40');
				update row;
				row.Wide = 2.0;
				update row;
				row.Amount = float('NaN');
				update row;
				row.Wide = 3.0;
				update row;

				if row.Amount == row.Amount {
					return 0;
				}

				return 1;
			"#);
		}
		else {
			for value in ["1e40", "-1e40", "NaN"] {
				let result = self.run(&format!(
					"rec mut row = find first Numbers where Id == 1;
					row.Amount = float('{value}');
					update row;

					return 0;"
				));

				assert!(matches!(result, Err(tablo::TabloError::Runtime(_))), "{result:?}");

				self.succeeds(r#"
					rec row = find first Numbers where Id == 1;

					if row.Amount == float(0) and row.Wide == float('0.75') {
						return 1;
					}

					return 0;
				"#);
			}
		}

		self.succeeds(r#"
			rec mut row = find first Numbers where Id == 1;
			delete row;

			var remaining: int = count Numbers where Id == 1;

			if remaining == 0 {
				return 1;
			}

			return 0;
		"#);
	}
}

impl Drop for Fixture {
	fn drop(&mut self) {
		let suffix = if self.backend == DatabaseBackend::PostgreSql {
			" CASCADE"
		}
		else {
			""
		};
		let sql = format!("DROP SCHEMA {}{suffix}", self.namespace);

		match &mut self.connection {
			Connection::MySql(connection) => {
				let _ = connection.query_drop(sql);
			}
			Connection::PostgreSql(connection) => {
				let _ = connection.batch_execute(&sql);
			}
		}
	}
}

#[test]
#[ignore = "requires dedicated PostgreSQL server via TABLO_TEST_POSTGRES"]
fn postgresql_float_widths_live() {
	let url = std::env::var("TABLO_TEST_POSTGRES").expect("Set TABLO_TEST_POSTGRES");

	Fixture::new(DatabaseBackend::PostgreSql, url).exercise();
}

#[test]
#[ignore = "requires dedicated MySQL server via TABLO_TEST_MYSQL"]
fn mysql_float_widths_live() {
	let url = std::env::var("TABLO_TEST_MYSQL").expect("Set TABLO_TEST_MYSQL");

	Fixture::new(DatabaseBackend::MySql, url).exercise();
}
