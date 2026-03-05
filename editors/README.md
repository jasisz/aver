# Aver — Editor Support

Aver ships with:

- a VS Code extension backed by the `aver-lsp` language server
- Sublime Text syntax highlighting

## VS Code

### Features

| Feature | Description |
|---------|-------------|
| Syntax highlighting | TextMate grammar for keywords, types, effects, string interpolation, and `?` descriptions |
| Diagnostics | Lex, parse, type, and project-policy diagnostics shown inline |
| Completion | Built-in namespaces, user-defined types, user functions, and cross-module members |
| Hover | Function source previews, type definitions, variable types, and namespace member signatures |
| Go-to-definition | Cross-file navigation for functions, types, and bindings |
| Signature help | Parameter hints inside function calls |

All features work cross-module. `depends [...]` is resolved from the workspace root, matching `aver run`.

### Install

Commands below assume you are in the repository root.

```bash
# Install the Aver CLI
cargo install aver-lang

# Build the LSP server
cargo build -p aver-lsp --release
ln -sf $(pwd)/target/release/aver-lsp /usr/local/bin/aver-lsp

# Install the VS Code extension from the repo
ln -snf $(pwd)/editors/vscode ~/.vscode/extensions/aver-lang
```

Open a `.av` file and the extension will activate automatically.

To point VS Code at a different LSP binary, set `aver.lsp.path` in editor settings.

Alternative install method:

```bash
cp -r editors/vscode/ ~/.vscode/extensions/aver-lang
```

Or package the extension and install it through **Extensions -> ... -> Install from VSIX**.

## Sublime Text

Copy `editors/sublime/` to your Packages directory:

```bash
# macOS
cp -r editors/sublime/ ~/Library/Application\ Support/Sublime\ Text/Packages/Aver/

# Linux
cp -r editors/sublime/ ~/.config/sublime-text/Packages/Aver/

# Windows
cp -r editors/sublime/ %APPDATA%\Sublime Text\Packages\Aver\
```

Hot-reloads with no restart needed. Select **Aver** from the language menu in the bottom-right corner.

## What's highlighted

- Keywords: `fn`, `type`, `record`, `module`, `match`, `verify`, `decision`, `depends`, `exposes`, `effects`
- Reserved words (parse errors): `val`, `var`
- Effect declarations: `! [Console, Http, Disk]`
- Function descriptions: `? "..."`
- String interpolation: `"Hello, {name}!"`
- Escape sequences: `\n`, `\t`, `\\`, `\"`, etc.
- Built-in types: `Int`, `Float`, `String`, `Bool`, `Unit`, `Result`, `Option`, `List`, `Map`, `Fn`
- Built-in values: `true`, `false`
- Namespace calls: `Console.print`, `Http.get`, `Disk.readText`, `Tcp.send`, `Int.fromString`, `String.len`, `List.map`, `Map.get`, `Char.toCode`, `Byte.toHex`
- Qualified constructors: `Result.Ok`, `Option.Some`, `Option.None`
- ADR keys: `date`, `reason`, `chosen`, `rejected`, `impacts`, `author`
- Numbers: integers, floats, scientific notation (`1.5e-3`)
- Comments: `// ...`
