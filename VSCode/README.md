# Tablo VS Code Extension

This folder contains the Visual Studio Code extension for Tablo.

The current implementation focuses on debugger integration via `tablodbg --dap`.
When launched from a `.tablo` file, the extension attempts to compile the
source to a hidden `.tbo` file before starting the debugger. In future, I plan
to add LSP capabilities to this extension, but not yet.

Planned structure:

- `src/extension.ts`
  - extension activation entry point
- `src/debug/`
  - DAP-related registration and launch
- `src/language/`
  - future LSP/language-feature implementation

The extension currently assumes:

- compiled Tablo programs are debugged from `.tbo` object files
- `.tablo` source files can be compiled automatically into `.tablo/debug/`
- `tablodbg` is either on `PATH` or configured explicitly
- an optional `tablo.toml` path can be supplied through `tablo.debug.projectConfigPath`

Future work will likely add:

- `.tablo` language registration and syntax support
- LSP client integration
- richer debug configuration validation
