# Tablo VS Code Extension

This folder contains the Visual Studio Code extension for Tablo.

The current implementation focuses on debugger integration via `tablodbg --dap`.
When launched from a `.tablo` file, the extension attempts to compile the
source to a hidden `.tbo` file before starting the debugger.

The extension also includes:

- an initial native VS Code completion provider for `.tablo` files
- optional experimental LSP-backed diagnostics via `tablolsp`

The native completion provider currently offers:

- keyword, primitive-type, and built-in-function completions
- document-symbol completions for functions, objects, enums, and local names
- member completions for `Enum.Variant` and simple `obj`-typed variables where
  the type can be inferred directly from the current document

Current structure:

- `src/extension.ts`
  - extension activation entry point
- `src/debug/`
  - DAP-related registration and launch
- `src/language/`
  - native language features and the lightweight `tablolsp` client wiring

The extension currently assumes:

- compiled Tablo programs are debugged from `.tbo` object files
- `.tablo` source files can be compiled automatically into `.tablo/debug/`
- `tablodbg` is either on `PATH` or configured explicitly
- `tablolsp` is either on `PATH` or configured explicitly when LSP is enabled
- an optional `tablo.toml` path can be supplied through `tablo.debug.projectConfigPath`

Settings of interest:

- `tablo.debug.tablocPath`
- `tablo.debug.tablodbgPath`
- `tablo.debug.projectConfigPath`
- `tablo.language.enableLsp`
- `tablo.language.tablolspPath`
