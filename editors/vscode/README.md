# Aver Language

VS Code support for the Aver programming language.

## Features

- syntax highlighting for Aver
- inline diagnostics from `aver-lsp`
- completion for namespaces, effects, functions, and module members
- hover with function contracts, verify summaries, and decision context
- go-to-definition across modules
- outline symbols for module structure and nested `verify`
- code lenses for `verify` and `decision` blocks
- native `Format Document` support

## Requirements

Install the Aver language server first:

```bash
cargo install aver-lsp
```

If your `aver-lsp` binary is not in `PATH`, set `aver.lsp.path` in VS Code settings.
