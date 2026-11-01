# Building and Running Tablo

This guide describes the current Rust prototype. Run the commands below from the repository root.

For the current Rust implementation, standalone source files compiled with `tabloc` must define an entry-point function with the exact signature `fn Main(args: [text]): int`.

`tabloc` now produces an embedded Linux executable by default. From the repository root:

```sh
mkdir -p dist
printf 'fn Main(args: [text]): int { return len(args); }\n' > dist/hello.tablo
cargo run --manifest-path Rust/Cargo.toml --bin tabloc -- \
  dist/hello.tablo --runtime-dir Rust --cache-dir Rust/target/executable-cache
dist/hello first second
```

The program returns the number of arguments as its exit status (`2` above). Use `-o PATH` to choose another output filename; the default removes `.tablo`. Output parent directories must already exist. The executable can run without the source file or a Tablo installation, but still needs compatible system libraries. Building requires a Rust toolchain and the existing crate's native dependencies.

For shared-runtime output, add `--runtime shared`:

```sh
cargo run --manifest-path Rust/Cargo.toml --bin tabloc -- \
  dist/hello.tablo --runtime shared --runtime-dir Rust
```

Deploy the executable together with its `lib/` directory. The compiler creates a library such as `lib/libtablo_runtime_0_1_all.so`, where `0_1` identifies the major/minor series and `all` indicates that this first implementation includes all database drivers. Multiple executables in the same directory can share that library. The launcher checks the library's actual Tablo version before running the program; the major/minor must match and the runtime patch must be at least the producing compiler's patch, except for experimental `0.0.0` artifacts.

The adjacent `.so.json` file records the library's version, target, driver set, and content hash for subsequent builds; it is not needed to run the application. `tabloc` reuses identical library bytes or an existing library whose manifest and content hash verify compatibility. It preserves a compatible newer patch rather than downgrading it. If the existing library cannot be verified as compatible, the build fails without replacing it or the existing executable. Use a new deployment directory or deliberately update the library and its manifest before rebuilding. Experimental builds only reuse identical deployed library bytes.

An installed compiler looks for a runtime source bundle in `runtime/` beside its executable. The bundle must contain `Cargo.toml`, `Cargo.lock`, and `src/`; automated bundle installation is not yet provided. Use `--runtime-dir PATH` to select sources explicitly, as in the checkout example. Builds use `$XDG_CACHE_HOME/tablo` or `$HOME/.cache/tablo` unless `--cache-dir` is supplied. Add `--offline` to prohibit dependency downloads during the generated Cargo build.

`--config PATH` selects the compilation configuration; otherwise `./tablo.toml` is used when present. Database backends and schema paths come from its `[databases]` and `[schemas]` entries. Optional `[build]` settings include `runtime = "embedded"` (or `"shared"`) and `debug_info = "lines"`; command-line settings take precedence. The executable separately reads `TABLO_CONFIG` or its execution working directory's `tablo.toml` at startup. All application arguments, including `--help`, are passed to `Main`.

For the existing runtime and debugger, request a standalone bytecode object explicitly:

```sh
cargo run --manifest-path Rust/Cargo.toml --bin tabloc -- \
  --emit object --debug-info full dist/hello.tablo -o dist/hello.tobj
cargo run --manifest-path Rust/Cargo.toml --bin tablodbg -- dist/hello.tobj
```

Object output does not require a native build toolchain and defaults to `.tobj`. Existing `.tbo` files remain readable. These objects still contain complete programs requiring `Main`; independently linkable objects and Tablo libraries are not implemented yet. The bytecode format version is unchanged.

`--debug-info lines` is the default and keeps source locations without debugger local-variable records. `full` retains those records; `none` removes optional debug records. VS Code requests full debug information automatically. The former positional output argument and `--schema` option are no longer accepted; use `-o` and TOML configuration. Tablo library output is not implemented yet.

Run the native integration tests explicitly with `cargo test --manifest-path Rust/Cargo.toml --test embedded_executable --test shared_executable -- --ignored`. They use offline Cargo builds and reuse `Rust/target/embedded-test-cache`. The shared-runtime test also requires `readelf` (from binutils).
