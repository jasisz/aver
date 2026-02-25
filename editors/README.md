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

- Keywords: `fn`, `val`, `var`, `type`, `record`, `module`, `match`, `verify`, `decision`
- Effect declarations: `! [Network, Disk]`
- Function descriptions: `? "..."`
- String interpolation: `"Hello, {name}!"`
- Built-in types: `Int`, `Float`, `String`, `Bool`, `Result`, `Option`, `List`
- Built-in values: `true`, `false`, `Ok`, `Err`, `Some`, `None`
- Service calls: `Console.print`, `Network.get`, `Disk.readText`
- Qualified constructors: `Shape.Circle`, `Models.User`
- ADR keys: `date`, `reason`, `chosen`, `rejected`, `impacts`
- Comments: `-- ...`
