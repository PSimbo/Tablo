use tablo::runtime_abi::{RuntimeArgument, TabloVersion};

#[unsafe(no_mangle)]
pub extern "C" fn tablo_runtime_version() -> TabloVersion {
	TabloVersion::current()
}

#[unsafe(no_mangle)]
pub extern "C" fn tablo_runtime_drivers() -> u32 {
	tablo::runtime_drivers::DriverSet::available().bits()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tablo_runtime_run(
	bytes: *const u8,
	length: usize,
	version: TabloVersion,
	arguments: *const RuntimeArgument,
	argument_count: usize,
	exit_status: *mut i32,
) -> i32 {
	unsafe { tablo::runtime::run_shared_package(bytes, length, version, arguments, argument_count, exit_status) }
}
