use crate::schema::DatabaseBackend;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct DriverSet(u32);

impl DriverSet {
	pub fn available() -> Self {
		Self((cfg!(feature = "driver-mysql") as u32)
			| ((cfg!(feature = "driver-postgresql") as u32) << 1)
			| ((cfg!(feature = "driver-sqlite") as u32) << 2))
	}

	pub fn bits(self) -> u32 {
		self.0
	}

	pub fn insert(&mut self, backend: DatabaseBackend) {
		self.0 |= match backend {
			DatabaseBackend::MySql => 1,
			DatabaseBackend::PostgreSql => 2,
			DatabaseBackend::Sqlite => 4,
		};
	}

	pub fn contains(self, backend: DatabaseBackend) -> bool {
		let mut required = Self::default();
		required.insert(backend);
		self.0 & required.0 == required.0
	}

	pub fn name(self) -> String {
		let names = [DatabaseBackend::MySql, DatabaseBackend::PostgreSql, DatabaseBackend::Sqlite]
			.into_iter().filter(|backend| self.contains(*backend)).map(DatabaseBackend::name).collect::<Vec<_>>();
		if names.is_empty() { String::from("none") } else { names.join("_") }
	}

	#[cfg(feature = "compiler")]
	pub(crate) fn cargo_features(self) -> String {
		let features = [DatabaseBackend::MySql, DatabaseBackend::PostgreSql, DatabaseBackend::Sqlite]
			.into_iter().filter(|backend| self.contains(*backend))
			.map(|backend| format!("\"driver-{}\"", backend.name())).collect::<Vec<_>>();
		format!("[{}]", features.join(", "))
	}
}

/// Inspect all compiled functions, including imported ones. Sequence operations
/// do not carry a backend in the current object encoding, so the compiler must
/// resolve their database names against the compilation schema catalog.
#[cfg(feature = "compiler")]
pub(crate) fn required_drivers(program: &crate::bytecode::Program, schema: Option<&crate::schema::SchemaCatalog>) -> Result<DriverSet, String> {
	use crate::bytecode::Instruction;
	let mut drivers = DriverSet::default();
	for query in program.queries() {
		let crate::query::LoweredBackendQuery::Sql(query) = query;
		drivers.insert(query.dialect.backend());
	}
	let instructions = program.instructions().iter().chain(program.functions().iter().flat_map(|function| function.body().instructions.iter()));
	for instruction in instructions {
		let database = match instruction {
			Instruction::AdvanceSequence { database_name, .. }
			| Instruction::LoadSequenceCurrent { database_name, .. }
			| Instruction::StoreSequenceCurrent { database_name, .. } => database_name,
			Instruction::MakeRecordPointer { record_type, .. } => &record_type.database_name,
			_ => continue,
		};
		let backend = schema.and_then(|catalog| catalog.database(database)).map(|database| database.backend())
			.ok_or_else(|| format!("Cannot determine the runtime driver for database `{database}` from the compilation schema catalog."))?;
		drivers.insert(backend);
	}
	Ok(drivers)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn driver_names_and_masks_are_stable() {
		let mut set = DriverSet::default();
		assert_eq!(set.name(), "none");
		set.insert(DatabaseBackend::Sqlite);
		set.insert(DatabaseBackend::MySql);
		set.insert(DatabaseBackend::Sqlite);
		assert_eq!(set.name(), "mysql_sqlite");
		assert_eq!(set.bits(), 5);
		assert!(!set.contains(DatabaseBackend::PostgreSql));
	}
	#[cfg(feature = "compiler")]
	#[test]
	fn selects_queries_and_sequences_but_ignores_unused_catalog_databases() {
		use crate::bytecode::{CodeBody, CompiledFunction, Instruction, Program};
		use crate::schema_fixture::read_schema_catalog_from_str;
		let mut schema = read_schema_catalog_from_str("database Active; schema Main implicit; create sequence Counter; database Unused; schema Main implicit; create table Customers (Id int not null);").unwrap();
		schema.database_mut("Active").unwrap().set_backend(DatabaseBackend::Sqlite);
		schema.database_mut("Unused").unwrap().set_backend(DatabaseBackend::MySql);
		let empty = Program::new(vec![]);
		assert_eq!(required_drivers(&empty, Some(&schema)).unwrap().name(), "none");
		let function = CompiledFunction::new(Some("Imported".into()), None, CodeBody::new(vec![Instruction::AdvanceSequence {
			database_name: "Active".into(), schema_is_implicit: true, schema_name: "Main".into(), sequence_name: "Counter".into(),
		}]));
		let program = Program::from_entry_function(Default::default(), 0, vec![function], Default::default());
		assert_eq!(required_drivers(&program, Some(&schema)).unwrap().name(), "sqlite");
		assert!(required_drivers(&program, None).is_err());

		let source = "with Active; with Unused; fn Main(args: [text]): int { return seqnext(Counter) + count Unused.Main.Customers; }";
		let (program, _) = crate::compile_source_to_program_with_name_and_schema_and_warnings(source, None, crate::CompilationTarget::Standalone, Some(&schema)).unwrap();
		assert_eq!(required_drivers(&program, Some(&schema)).unwrap().name(), "mysql_sqlite");
	}

}
