#![cfg(feature = "compiler")]

use std::path::{Path, PathBuf};
use std::process::Command;

fn compile(compiler: &Path, directory: &Path, emit: &str, input: &str) {
	let output = Command::new(compiler)
		.args(["--emit", emit, input])
		.current_dir(directory)
		.output()
		.unwrap();

	assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

#[test]
#[ignore = "requires runnable 32-bit and 64-bit binaries via TABLO_FLOAT_32_BIN_DIR and TABLO_FLOAT_64_BIN_DIR"]
fn float_literals_and_library_defaults_use_the_loading_runtime_width() {
	let binaries = [
		(32, PathBuf::from(std::env::var("TABLO_FLOAT_32_BIN_DIR").expect("Set TABLO_FLOAT_32_BIN_DIR"))),
		(64, PathBuf::from(std::env::var("TABLO_FLOAT_64_BIN_DIR").expect("Set TABLO_FLOAT_64_BIN_DIR"))),
	];

	for (_, producer) in &binaries {
		let directory = tempfile::tempdir().unwrap();
		let root = directory.path();
		let library_path = root.join("Numbers.tablo");

		std::fs::write(&library_path, r#"
			pub fn Read(value: float = 16777217.0): float {
				return value;
			}
		"#).unwrap();
		compile(&producer.join("tabloc"), root, "library", "Numbers.tablo");
		std::fs::remove_file(library_path).unwrap();

		std::fs::write(root.join("main.tablo"), r#"
			use './Numbers';

			obj Model {
				value: float = 16777217.0
			};

			fn Main(args: [text]): int {
				var value: float = 16777217.0;
				var model: Model;

				if model.value != value or Read() != value {
					return 1;
				}

				if int(value) == 16777216 {
					return 32;
				}

				return int(value) == 16777217 ? 64 : 1;
			}
		"#).unwrap();

		compile(&producer.join("tabloc"), root, "object", "main.tablo");

		for (expected_width, consumer) in &binaries {
			let output = Command::new(consumer.join("tablo"))
				.arg(root.join("main.tobj"))
				.current_dir(root)
				.output()
				.unwrap();

			assert_eq!(
				output.status.code(),
				Some(*expected_width),
				"producer: {}, consumer: {}, stderr: {}",
				producer.display(),
				consumer.display(),
				String::from_utf8_lossy(&output.stderr),
			);
		}
	}
}
