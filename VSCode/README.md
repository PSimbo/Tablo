# Tablo VS Code Extension

The extension provides syntax highlighting, code completion, and debugging for Tablo. You can run it directly from this checkout without packaging or installing it.

1. Install VS Code and Node.js with npm. Make sure the `code` command is available in your terminal.

2. From the repository root, install the extension's dependencies and build it:

   ```sh
   npm --prefix VSCode ci
   npm --prefix VSCode run build
   ```

3. Launch a development window with the extension enabled (Linux/macOS shell):

   ```sh
   code --new-window --extensionDevelopmentPath="$PWD/VSCode" .
   ```

   In PowerShell, use:

   ```powershell
   code --new-window --extensionDevelopmentPath="$($PWD.Path)/VSCode" .
   ```

4. Open a `.tablo` file in the new window. Syntax highlighting and completion should be available. Press **Ctrl+Space** to request completions.

To debug programs, first build the compiler and debugger from the repository root using a Rust toolchain:

```sh
cargo build --manifest-path Rust/Cargo.toml --bin tabloc --bin tablodbg
```

In the development window's Settings, set these values to the **absolute paths** of the built executables:

- `tablo.debug.tablocPath`: `<repository>/Rust/target/debug/tabloc`
- `tablo.debug.tablodbgPath`: `<repository>/Rust/target/debug/tablodbg`

On Windows, include the `.exe` suffix. Alternatively, put both executables on `PATH` before launching VS Code.

Open and save a Tablo program containing `fn Main(args: [text]): int`, set a breakpoint, and press **F5**. Choose **Tablo** if prompted. The extension compiles the program into `.tablo/debug/` and starts the debugger. If something fails, open **View → Output** and select **Tablo** for details.

After changing extension code, run `npm --prefix VSCode run build` again, then run **Developer: Reload Window** from the development window's Command Palette.

Experimental language-server support is optional; see [language features](src/language/README.md) for details.
