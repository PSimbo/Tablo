#[allow(dead_code)]
mod abi;
use abi::{RuntimeArgument, TabloVersion};

#[link(name = "__TABLO_LIBRARY__")]
unsafe extern "C" {
	fn tablo_runtime_version() -> TabloVersion;
	fn tablo_runtime_run(bytes: *const u8, length: usize, version: TabloVersion, arguments: *const RuntimeArgument, argument_count: usize, exit_status: *mut i32) -> i32;
}

fn main() {
	let required = __TABLO_VERSION__;
	let actual = unsafe { tablo_runtime_version() };
	if !actual.accepts(required) {
		eprintln!("Tablo artifact version {required} cannot run on runtime {actual}; matching major/minor and an equal or newer runtime patch are required.");
		std::process::exit(1);
	}
	let arguments: Vec<String> = std::env::args().skip(1).collect();
	let arguments: Vec<RuntimeArgument> = arguments.iter().map(|argument| RuntimeArgument { data: argument.as_ptr(), len: argument.len() }).collect();
	let bytes = include_bytes!("program.tbo");
	let mut exit_status = 0;
	let result = unsafe { tablo_runtime_run(bytes.as_ptr(), bytes.len(), required, arguments.as_ptr(), arguments.len(), &mut exit_status) };
	std::process::exit(if result == 0 { exit_status } else { 1 });
}
