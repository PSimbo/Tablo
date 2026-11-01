use std::path::PathBuf;

use crate::database::RuntimeDatabaseConfig;
use crate::runtime_abi::{RuntimeArgument, TabloVersion};
use crate::runtime_config::read_runtime_database_config_from_path;
use crate::value::Value;
use crate::TabloError;

pub(crate) fn check_version(runtime: TabloVersion, artifact: TabloVersion) -> Result<(), String> {
	if runtime.accepts(artifact) {
		Ok(())
	}
	else {
		Err(format!("Tablo artifact version {artifact} cannot run on runtime {runtime}; matching major/minor and an equal or newer runtime patch are required."))
	}
}

/// Executes linked bytes without opening the executable or the original sources.
/// Configuration is resolved at execution time so build-machine paths and
/// credentials do not become deployment settings.
pub fn run_package(bytes: &[u8], artifact_version: TabloVersion, arguments: &[String]) -> Result<i32, String> {
	check_version(TabloVersion::current(), artifact_version)?;
	let program = crate::object_file::read_program(bytes).map_err(|error| TabloError::ObjectFile(error).to_string())?;
	let config_path = match std::env::var_os("TABLO_CONFIG") {
		Some(path) => Some(PathBuf::from(path)),
		None => crate::utils::existing_child_path(&std::env::current_dir().map_err(|error| error.to_string())?, "tablo.toml"),
	};
	let config = match config_path {
		Some(path) => read_runtime_database_config_from_path(&path)
			.map_err(|error| format!("Failed to load runtime config `{}`: {}", path.display(), error.message))?,
		None => RuntimeDatabaseConfig::new(),
	};
	for query in program.queries() {
		let crate::query::LoweredBackendQuery::Sql(query) = query;
		if !crate::runtime_drivers::DriverSet::available().contains(query.dialect.backend()) {
			return Err(format!("This runtime was built without the {} driver required by database `{}`.", query.dialect.backend().name(), query.database_name));
		}
	}
	let result = crate::run_program_with_database_config_and_arguments(&program, config, arguments)
		.map_err(|error| error.to_string())?;
	main_exit_status(result)
}

fn main_exit_status(result: Option<Value>) -> Result<i32, String> {
	let Some(Value::Integer(status)) = result else {
		return Err(String::from("Runtime error: Entry-point function `Main` did not return an `int` exit status."));
	};
	i32::try_from(status).map_err(|_| format!("Runtime error: Entry-point function `Main` returned exit status {status}, which is outside the supported 32-bit signed range."))
}

/// Runs a package across the shared-library boundary. Zero means execution
/// succeeded and `exit_status` contains Main's result; one means a runtime error.
/// All buffers are borrowed, and Rust allocations stay inside the runtime.
///
/// # Safety
/// Non-null pointers must be aligned and valid for their stated lengths for the
/// entire call. `exit_status` must point to writable i32 storage. Callers must not
/// mutate borrowed buffers during execution. No Rust panic may unwind to callers.
pub unsafe fn run_shared_package(
	bytes: *const u8,
	length: usize,
	version: TabloVersion,
	arguments: *const RuntimeArgument,
	argument_count: usize,
	exit_status: *mut i32,
) -> i32 {
	let result = std::panic::catch_unwind(|| -> Result<i32, String> {
		if exit_status.is_null() || !exit_status.is_aligned() {
			return Err(String::from("Invalid exit-status pointer passed to the Tablo runtime."));
		}
		let bytes = unsafe { borrowed_slice(bytes, length)? };
		let arguments = unsafe { borrowed_slice(arguments, argument_count)? };
		let arguments = arguments.iter().map(|argument| {
			let bytes = unsafe { borrowed_slice(argument.data, argument.len)? };
			std::str::from_utf8(bytes).map(str::to_owned).map_err(|error| format!("Invalid UTF-8 application argument: {error}"))
		}).collect::<Result<Vec<_>, _>>()?;
		run_package(bytes, version, &arguments)
	});
	match result {
		Ok(Ok(status)) => {
			unsafe { exit_status.write(status); }
			0
		}
		Ok(Err(error)) => {
			use std::io::Write;
			let _ = writeln!(std::io::stderr(), "{error}");
			1
		}
		Err(_) => {
			use std::io::Write;
			let _ = writeln!(std::io::stderr(), "Internal Tablo runtime panic.");
			1
		}
	}
}

unsafe fn borrowed_slice<'a, T>(pointer: *const T, length: usize) -> Result<&'a [T], String> {
	if length == 0 {
		return Ok(&[]);
	}
	if pointer.is_null() || !pointer.is_aligned() || length > isize::MAX as usize / std::mem::size_of::<T>() {
		return Err(String::from("Invalid buffer passed to the Tablo runtime."));
	}
	Ok(unsafe { std::slice::from_raw_parts(pointer, length) })
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn applies_version_policy_and_artifact_only_experimental_bypass() {
		let artifact = TabloVersion::parse("0.3.2").unwrap();
		for (runtime, accepted) in [("0.3.1", false), ("0.3.2", true), ("0.3.5", true), ("0.4.0", false), ("1.3.2", false), ("0.0.0", false)] {
			let runtime = TabloVersion::parse(runtime).unwrap();
			assert_eq!(runtime.accepts(artifact), accepted);
			assert!(runtime.accepts(TabloVersion::parse("0.0.0").unwrap()));
		}
	}

	#[test]
	fn rejects_invalid_versions() {
		for version in ["", "0.1", "0.1.2.3", "01.2.3", "0.-1.2", "0.1.2-dev", "0.1.18446744073709551616"] {
			assert!(TabloVersion::parse(version).is_err(), "{version}");
		}
	}

	#[test]
	fn experimental_artifacts_still_require_valid_bytecode() {
		let error = run_package(b"invalid", TabloVersion::parse("0.0.0").unwrap(), &[]).unwrap_err();
		assert!(error.contains("Object file error"));
	}

	#[test]
	fn validates_application_exit_status() {
		assert_eq!(main_exit_status(Some(Value::Integer(-1))).unwrap(), -1);
		assert!(main_exit_status(None).is_err());
		assert!(main_exit_status(Some(Value::Integer(i64::MAX))).is_err());
	}

	#[test]
	fn shared_boundary_preserves_exit_status_on_invalid_buffers() {
		let mut status = 42;
		let result = unsafe { run_shared_package(std::ptr::null(), 1, TabloVersion::current(), std::ptr::null(), 0, &mut status) };
		assert_eq!(result, 1);
		assert_eq!(status, 42);
	}

	#[cfg(feature = "compiler")]
	#[test]
	fn shared_boundary_separates_application_failure_status_from_runtime_failure() {
		let program = crate::compile_to_program_with_name("fn Main(args: [text]): int { return 7; }", None).unwrap();
		let bytes = crate::object_file::write_program(&program);
		let mut status = 0;
		let result = unsafe { run_shared_package(bytes.as_ptr(), bytes.len(), TabloVersion::current(), std::ptr::null(), 0, &mut status) };
		assert_eq!(result, 0);
		assert_eq!(status, 7);
	}
}
