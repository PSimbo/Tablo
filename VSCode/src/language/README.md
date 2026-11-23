# Language features

The default completion provider suggests keywords, local functions and in-scope task handles from the open document. Syntax highlighting includes `async`, `block`, `suspend` and `task<T>`.

For compiler diagnostics, imported function completions and call signature help, build the language server from the repository root.

```sh
cargo build --manifest-path Rust/Cargo.toml --bin tablolsp
```

Enable `tablo.language.enableLsp` in VS Code. Set `tablo.language.tablolspPath` to the built executable or put `tablolsp` on your PATH. Type `(` or `,` inside a call to see its signatures and active parameter. Async overloads are marked `async` for both source and compiled library imports.
