# aver-lsp

`aver-lsp` is the language server for the Aver programming language.

It provides:

- diagnostics for parse, type, and project-policy checks
- completion for Aver namespaces, effects, functions, and module members
- hover with function contracts, verify summaries, and decision context
- go-to-definition across local and dependent modules
- document symbols for modules, decisions, functions, and nested `verify`
- code lenses for `verify` and `decision` blocks
- native `Format Document` support through Aver's formatter

## Install

```bash
cargo install aver-lsp
```

The server expects an Aver-capable editor client such as the VS Code extension in `editors/vscode`.

By default the VS Code extension looks for an `aver-lsp` binary in `PATH`.

## Repository

- language + CLI: `aver-lang`
- shared runtime: `aver-rt`
- editor docs: `editors/README.md`
