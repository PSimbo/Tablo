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

Deploy the executable together with its `lib/` directory. The compiler creates a library such as `lib/libtablo_runtime_0_1_sqlite.so`, where `0_1` identifies the major/minor series and `sqlite` identifies the included database drivers. Database-free programs use the `none` variant; multiple drivers appear in a stable order, such as `mysql_postgresql_sqlite`. Multiple executables in the same directory can share that library. The launcher checks the library's actual driver capabilities and Tablo version before running the program; the major/minor must match and the runtime patch must be at least the producing compiler's patch, except for experimental `0.0.0` artifacts.

The adjacent `.so.json` file records the library's version, target, driver set, and content hash for subsequent builds; it is not needed to run the application. `tabloc` reuses identical library bytes or an existing library whose manifest and content hash verify compatibility. It preserves a compatible newer patch rather than downgrading it. If the existing library cannot be verified as compatible, the build fails without replacing it or the existing executable. Use a new deployment directory or deliberately update the library and its manifest before rebuilding. Experimental builds only reuse identical deployed library bytes.

An installed compiler looks for a runtime source bundle in `runtime/` beside its executable. The bundle must contain `Cargo.toml`, `Cargo.lock`, and `src/`; automated bundle installation is not yet provided. Use `--runtime-dir PATH` to select sources explicitly, as in the checkout example. Builds use `$XDG_CACHE_HOME/tablo` or `$HOME/.cache/tablo` unless `--cache-dir` is supplied. Add `--offline` to prohibit dependency downloads during the generated Cargo build.

`--config PATH` selects the compilation configuration; otherwise `./tablo.toml` is used when present. Database backends and schema paths come from its `[databases]` and `[schemas]` entries. Optional `[build]` settings include `runtime = "embedded"` (or `"shared"`) and `debug_info = "lines"`; command-line settings take precedence. The executable separately reads `TABLO_CONFIG` or its execution working directory's `tablo.toml` at startup. All application arguments, including `--help`, are passed to `Main`.

Generated embedded and shared runtimes exclude compiler, editor, and debugger expression-evaluation code. `tabloc` selects drivers from the compiled program, including imported functions, queries, record operations, and sequences. Unused database entries in the configuration do not add drivers. The normal development build still enables the compiler and all three drivers. For a runtime-only library build, use `cargo build --manifest-path Rust/Cargo.toml --no-default-features --lib`, optionally adding features such as `--features driver-sqlite`.

Generated builds resolve the smaller dependency graph from the supplied lockfile, verify that no package version or checksum was introduced or changed, and then build with `--locked`.

For the existing runtime and debugger, request a standalone bytecode object explicitly:

```sh
cargo run --manifest-path Rust/Cargo.toml --bin tabloc -- \
  --emit object --debug-info full dist/hello.tablo -o dist/hello.tobj
cargo run --manifest-path Rust/Cargo.toml --bin tablodbg -- dist/hello.tobj
```

Object output does not require a native build toolchain and defaults to `.tobj`. The `.tbo` extension is also accepted, but legacy `TBO0` contents require recompilation. These objects still contain complete programs requiring `Main`; independently linkable `.tobj` modules are not implemented yet. Objects record the producing Tablo version using the header described below.

To build a reusable Tablo function library, export functions with `pub` and use `--emit library`. A library does not require `Main`:

```tablo
// Math.tablo
fn Twice(value: int): int { return value * 2; }
pub fn Answer(value: int = 20): int { return Twice(value) + 2; }
```

```sh
cargo run --manifest-path Rust/Cargo.toml --bin tabloc -- --emit library Math.tablo
```

This writes `Math.tlib`; `-o PATH` selects another filename. Consumers use the existing import syntax:

```tablo
use Answer from './Math.tlib';
fn Main(args: [text]): int { return Answer(); }
```

An extensionless import such as `use './Math';` selects `Math.tablo` when it exists, otherwise `Math.tlib`. Specify `.tlib` explicitly to select the compiled library when both exist. Compile the consumer normally to an executable, or use `--emit object` for a `.tobj` program. Library code is statically linked into either output; execution requires neither the `.tlib` file nor its sources. Libraries also include their imported implementation dependencies, so a library can be distributed as one file and used to build further libraries.

The first library implementation supports public functions with built-in value types, including arrays, nullable types, overloads, reference parameters, variadic parameters, and defaults. Private functions remain internal. Defaults must be expressible using the public interface and built-ins; they cannot reference private helpers. Database operations inside function bodies are compiled with the build's schema configuration. Consumers must supply compatible runtime database configuration; native driver selection may also require the compilation schema configuration.

Object types in libraries, and object, record, or sequence types in public signatures, are not supported yet. Top-level executable statements and empty public interfaces are rejected. Libraries currently require the same compiler package version. The separate `.tlib` container holds declarations and bytecode; it does not change the `.tobj` encoding. `--debug-info` applies to libraries as well, and consumers cannot restore debug information stripped when a library was built.

`--debug-info lines` is the default and keeps source locations without debugger local-variable records. `full` retains those records; `none` removes optional debug records. VS Code requests full debug information automatically. The former positional output argument and `--schema` option are no longer accepted; use `-o` and TOML configuration.

Run the native integration tests explicitly with `cargo test --manifest-path Rust/Cargo.toml --test embedded_executable --test shared_executable -- --ignored`. They use offline Cargo builds and reuse `Rust/target/embedded-test-cache`. The shared-runtime test also requires `readelf` (from binutils).

Object version compatibility
----------------------------

Object payloads now use a `TBO1` header followed by the producing Tablo version packed into a little-endian `u32`: 8 bits for major, 8 for minor, and 16 for patch. The reader requires matching major/minor and a reader patch at least as new as the artifact's. Artifact `0.0.0` skips only the version check. Old `TBO0` objects and libraries containing them require recompilation. See [Tablo Object File Format](../Tablo%20Object%20File%20Format.md) for the encoding and compatibility rules. The library manifest's existing exact compiler-version check remains an additional restriction on library imports.
