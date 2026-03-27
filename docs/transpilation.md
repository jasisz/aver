# Code Generation

There are two code-generation commands:

- [Rust backend](rust.md): deployment-oriented Cargo project generation via `aver compile`
- [Lean backend](lean.md): proof export for the pure subset of Aver via `aver proof`
- [Dafny backend](dafny.md): Z3-powered automated law verification via `aver proof --backend dafny`

They solve different problems and share the same `CodegenContext` infrastructure.

## `aver compile`

```
aver compile <FILE> [OPTIONS]

Options:
  -o, --output <OUTPUT>            Output directory for the generated project
      --name <NAME>                Project name (default: derived from file name)
      --module-root <MODULE_ROOT>  Resolve `depends [...]` from this root (default: current working directory)
      --with-replay                Emit optional record/replay runtime support
      --guest-entry <GUEST_ENTRY>  Scope replay/policy to this generated guest entry (requires --with-replay)
      --policy <POLICY>            Runtime policy mode: embed | runtime
      --with-self-host-support     Emit extra self-host-only runtime support (requires --guest-entry and runtime policy)
```

## `aver proof`

```
aver proof <FILE> [OPTIONS]

Options:
  -o, --output <OUTPUT>            Output directory for the generated project
      --name <NAME>                Project name (default: derived from file name)
      --module-root <MODULE_ROOT>  Resolve `depends [...]` from this root (default: current working directory)
      --backend <BACKEND>          Proof backend: lean (default) or dafny
      --verify-mode <VERIFY_MODE>  Lean only: auto | sorry | theorem-skeleton
```

## Quick routing

Use Rust when you want:
- a normal Cargo project
- deployment without the Aver interpreter
- Rust tests generated from `verify`

Use Lean when you want:
- proof artifacts for pure Aver code
- `verify` as executable Lean checks (`native_decide`)
- `verify law` as candidate universal theorems for supported shapes, with
  sampled or checked-domain fallback for the rest
- a path from Aver code to formal verification

Use Dafny when you want:
- automated `verify law` checking without writing proof tactics
- Z3/SMT solver attempting universal proofs for you
- a quick smoke test of whether your laws are provable

## Lean vs Dafny

| | Lean | Dafny |
|---|---|---|
| Verify cases | `native_decide` — always works | Not emitted (Z3 can't compute) |
| Verify laws | Hand-crafted tactic strategies | Z3 attempts automatically |
| Proof quality | Kernel-verified (gold standard) | SMT-checked (no counterexample found) |
| Effort | High (strategy per pattern) | Zero (just emit and run) |
| External deps | Lean 4 + Lake | Dafny + .NET + Z3 |

Both backends complement each other. Lean is the formal proof target; Dafny is the automated verification target.

## Adding a new backend

To add a new generated backend such as `js`, `go`, or `python`:

1. Add a new CLI command or extend an existing backend command in `src/main/cli.rs`
2. Create `src/codegen/<target>/mod.rs` with `pub fn transpile(ctx: &CodegenContext) -> ProjectOutput`
3. Add the command handler in `src/main/commands.rs`
4. Add `pub mod <target>;` in `src/codegen/mod.rs`

`CodegenContext` is backend-agnostic. It carries the type-checked AST, function signatures, memo metadata, and module dependencies.
