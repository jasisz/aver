# Aver — Editor Support

## VSCode

Copy `vscode/` to your extensions directory and restart VSCode:

```bash
cp -r vscode/ ~/.vscode/extensions/aver-language/
```

Or: **Extensions → ⋯ → Install from VSIX** (after packaging with `vsce package`).

## Sublime Text

Copy `sublime/` to your Packages directory:

```bash
# macOS
cp -r sublime/ ~/Library/Application\ Support/Sublime\ Text/Packages/Aver/

# Linux
cp -r sublime/ ~/.config/sublime-text/Packages/Aver/

# Windows
cp -r sublime/ %APPDATA%\Sublime Text\Packages\Aver\
```

Hot-reloads — no restart needed. Select **Aver** from the language menu in the bottom-right corner.

## What's highlighted

- Keywords: `fn`, `type`, `record`, `module`, `match`, `verify`, `decision`, `depends`, `exposes`, `effects`
- Reserved words (parse errors): `val`, `var`
- Effect declarations: `! [Console, Http, Disk]`
- Function descriptions: `? "..."`
- String interpolation: `"Hello, {name}!"`
- Escape sequences: `\n`, `\t`, `\\`, `\"`, etc.
- Built-in types: `Int`, `Float`, `String`, `Bool`, `Unit`, `Result`, `Option`, `List`, `Map`, `Fn`
- Built-in values: `true`, `false`
- Namespace calls: `Console.print`, `Http.get`, `Disk.readText`, `Tcp.send`, `Int.fromString`, `String.length`, `List.map`, `Map.get`, `Char.toCode`, `Byte.toHex`
- Qualified constructors: `Result.Ok`, `Option.None`, `Shape.Circle`
- ADR keys: `date`, `reason`, `chosen`, `rejected`, `impacts`, `author`
- Numbers: integers, floats, scientific notation (`1.5e-3`)
- Comments: `// ...`
