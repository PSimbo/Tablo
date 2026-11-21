# Testing Tablo

Run these commands from the repository root. Tests marked `#[ignore]` require additional tools or services and do not run in the standard suite.

## Standard tests

```sh
cargo test --manifest-path Rust/Cargo.toml
cargo test --manifest-path Rust/Cargo.toml --no-default-features --lib
```

The second command checks the compiler-free runtime without database drivers.

## Native executable integration tests

```sh
cargo test --manifest-path Rust/Cargo.toml --test embedded_executable --test shared_executable -- --ignored
cargo test --manifest-path Rust/Cargo.toml --test float floats_run_in_embedded_and_shared_native_runtimes -- --ignored
```

These tests use offline Cargo builds and reuse `Rust/target/embedded-test-cache`. Ensure the required dependencies are cached first. The shared-runtime tests also require `readelf` (from binutils).

## Live database float tests

Use dedicated test servers. These tests create and drop uniquely named schemas/databases, so the configured users need permission to do so. They cover automatic column-width discovery, IEEE rounding and special values, and repeated updates/deletes without a primary key. Programs are compiled and reloaded from object files before execution.

```sh
TABLO_TEST_POSTGRES='host=/path/to/socket port=5432 user=test dbname=postgres sslmode=disable' cargo test --manifest-path Rust/Cargo.toml --test float_database postgresql_float_widths_live -- --ignored
TABLO_TEST_MYSQL='mysql://test@localhost/mysql?socket=%2Fpath%2Fto%2Fmysql.sock' cargo test --manifest-path Rust/Cargo.toml --test float_database mysql_float_widths_live -- --ignored
```

To exercise the 32-bit drivers on Linux x86-64, install the i686 Rust target, 32-bit linker and libc, and matching i386 OpenSSL development files. With both test connection strings exported, run the same live tests under i686:

```sh
cargo test --manifest-path Rust/Cargo.toml --target i686-unknown-linux-gnu --test float_database -- --ignored
```

For a cross-build, point `I686_UNKNOWN_LINUX_GNU_OPENSSL_INCLUDE_DIR` and `I686_UNKNOWN_LINUX_GNU_OPENSSL_LIB_DIR` at the target headers and libraries if Cargo cannot locate them. The tests insert a binary64 `0.1` directly through SQL and verify that the runtime can update and delete that row after reading the value as a native float.

Replace the example connection strings with those for your test servers. The MySQL socket path in the URL is percent-encoded.

## JSON database tests

The default suite exercises JSON record storage on SQLite. With the dedicated test connection strings above exported, run the live tests for PostgreSQL (`json` and `jsonb`) and MySQL (`JSON`).

```sh
cargo test --manifest-path Rust/Cargo.toml --test json_database -- --ignored
```

The tests create and drop isolated schemas and exercise keyless record creation, updates, query loops, deletion, and scalar reads and writes on the native JSON backends. They distinguish SQL NULL from `JsonNull` and check that a fractional JSON number cannot implicitly supply an `int`. The MySQL test also checks that a write which would round a JSON number leaves the table unchanged.

## 32-bit float verification

On a Linux x86-64 host with a 32-bit linker and libc installed, the i686 executables can run directly in 32-bit compatibility mode:

```sh
rustup target add i686-unknown-linux-gnu
cargo test --manifest-path Rust/Cargo.toml --target i686-unknown-linux-gnu --no-default-features --features compiler,driver-sqlite
cargo test --manifest-path Rust/Cargo.toml --target i686-unknown-linux-gnu --no-default-features --lib
```

These commands cover the compiler/SQLite runtime and the compiler-free runtime respectively. They do not enable MySQL/PostgreSQL drivers, which also require matching 32-bit TLS dependencies.

The ignored `float_cross_target` test verifies that numeric literals, object defaults, and compiled-library defaults adopt the loading runtime's float width regardless of which compiler produced the artifact. Build both sets of compiler/runtime binaries, then supply their absolute directories through `TABLO_FLOAT_32_BIN_DIR` and `TABLO_FLOAT_64_BIN_DIR`:

```sh
cargo build --manifest-path Rust/Cargo.toml --bins
cargo build --manifest-path Rust/Cargo.toml --target i686-unknown-linux-gnu --no-default-features --features compiler,driver-sqlite --bins
TABLO_FLOAT_32_BIN_DIR="$PWD/Rust/target/i686-unknown-linux-gnu/debug" TABLO_FLOAT_64_BIN_DIR="$PWD/Rust/target/debug" cargo test --manifest-path Rust/Cargo.toml --test float_cross_target -- --ignored
```

On a 32-bit runtime, field reads use binary32 even when the database stores binary64. Row-identity comparisons preserve the original database value when native conversion would lose precision; this allows updates/deletes to locate the stored row without changing the precision of ordinary Tablo arithmetic.

## Manual VS Code debugger checks

Follow the [VS Code extension setup and debugging instructions](../VSCode/README.md) to run the extension from this checkout.

For a float smoke test:

1. Pause after initializing float variables, including a finite value, negative zero, infinity, and NaN.
2. Check the displayed types and values in Locals and Watch. Evaluate arithmetic, comparisons, and explicit `int()`, `dec()`, and `text()` conversions, including expected failures for non-finite numeric conversions.
3. Step through a float assignment and confirm that Locals and Watch refresh correctly. Check a float field in an object and an element in an array as well.

Automated debugger tests do not replace this manual editor check. Record which architecture and extension build were checked when reporting results.
