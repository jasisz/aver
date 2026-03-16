# Code Generation

There are two code-generation commands:

- [Rust backend](rust.md): deployment-oriented Cargo project generation via `aver compile`
- [Lean backend](lean.md): proof export for the pure subset of Aver via `aver proof`

They solve different problems and now have different CLI entrypoints.

## `aver compile`

```
aver compile <FILE> [OPTIONS]

Options:
  -o, --output <OUTPUT>            Output directory for the generated project
      --name <NAME>                Project name (default: derived from file name)
      --module-root <MODULE_ROOT>  Resolve `depends [...]` from this root (default: current working directory)
```

## `aver proof`

```
aver proof <FILE> [OPTIONS]

Options:
  -o, --output <OUTPUT>            Output directory for the generated project
      --name <NAME>                Project name (default: derived from file name)
      --module-root <MODULE_ROOT>  Resolve `depends [...]` from this root (default: current working directory)
      --verify-mode <VERIFY_MODE>  Verify emission mode: auto | sorry | theorem-skeleton
```

## Quick routing

Use Rust when you want:
- a normal Cargo project
- deployment without the Aver interpreter
- Rust tests generated from `verify`

Use Lean when you want:
- proof artifacts for pure Aver code
- `verify` as executable Lean checks
- `verify law` as candidate universal theorems for supported shapes, with
  sampled or checked-domain fallback for the rest
- a path from Aver code to formal verification

## Adding a new backend

To add a new generated backend such as `js`, `go`, or `python`:

1. Add a new CLI command or extend an existing backend command in `src/main/cli.rs`
2. Create `src/codegen/<target>/mod.rs` with `pub fn transpile(ctx: &CodegenContext) -> ProjectOutput`
3. Add the command handler in `src/main/commands.rs`
4. Add `pub mod <target>;` in `src/codegen/mod.rs`

`CodegenContext` is backend-agnostic. It carries the type-checked AST, function signatures, memo metadata, and module dependencies.
