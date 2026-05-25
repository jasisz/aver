# Code Generation

There are two code-generation commands:

- [Rust backend](rust.md): deployment-oriented Cargo project generation via `aver compile`
- [Lean backend](lean.md): proof export for pure Aver code and Oracle-lifted classified effects via `aver proof`
- [Dafny backend](dafny.md): Z3-powered automated law verification via `aver proof --backend dafny`

They solve different problems and share the same `CodegenContext` infrastructure.

## `aver compile`

```
aver compile <FILE> [OPTIONS]

Options:
  -o, --output <OUTPUT>            Output directory for the generated project
      --name <NAME>                Project name (default: derived from file name)
      --module-root <MODULE_ROOT>  Resolve `depends [...]` from this root (default: current working directory)
      --target <TARGET>            rust (default) | wasm-gc | wasip2
      --with-replay                Emit optional record/replay runtime support
      --guest-entry <GUEST_ENTRY>  Scope replay/policy to this generated guest entry (requires --with-replay)
      --policy <POLICY>            Runtime policy mode: embed | runtime
      --with-self-host-support     Emit extra self-host-only runtime support (requires --guest-entry and runtime policy)
      --emit-ir-after <PASS>       Print IR after the named pipeline stage and exit before codegen.
                                   PASS ∈ { parse, tco, typecheck, interp_lower, buffer_build, resolve, last_use, analyze }.
                                   Use diff -u between two stages to see exactly which expressions a pass rewrote.
      --explain-passes             Run the full pipeline (no codegen) and print a per-pass diagnostic report.
                                   Reports tail-call conversions, interpolations lowered, fusion sites + sinks,
                                   slots resolved, last-use markers, alloc/recursion facts.
      --json                       Emit the per-pass report as JSON (with --explain-passes); shape is
                                   { schema_version: 1, passes: [{ stage, summary, details: [...] }, ... ] }.
```

### `--emit-ir-after` quick map

The compiler runs IR transforms in a fixed seven-stage pipeline (see `src/ir/pipeline.rs`). `--emit-ir-after=PASS` short-circuits before codegen and prints the IR snapshot right after the named stage:

| Stage          | What changes between stages                                                |
|----------------|---------------------------------------------------------------------------|
| `parse`        | AST as the parser emitted it; baseline                                     |
| `tco`          | Tail-position recursive calls become `<tail-call:fn>(args)`                |
| `typecheck`    | Read-only — IR identical to `tco`, errors land in stdout                    |
| `interp_lower` | `"a${x}b"` desugars to `__buf_finalize(__buf_append(... __to_str(x) ...))` |
| `buffer_build` | `String.join(<builder>(args, []), sep)` rewrites to `__buf_finalize(<builder>__buffered(...))` and synthesizes the buffered variant |
| `resolve`      | `Expr::Ident` → `<name>` (resolved slot), `<resolved>` collapse for unknown |
| `last_use`     | Final references annotated as `<name:last>` so backends MOVE instead of COPY |
| `analyze`      | FnDef headers gain fact tags `[no_alloc, locals=N, recursive×N, body=…]`    |

### `--explain-passes` — per-pass diagnostic report

Same pipeline, different lens. Instead of dumping IR shapes, prints a structured report of what each pass actually decided:

```
$ aver compile fuse_demo.av --explain-passes
compiler pipeline — per-pass report
====================================

[tco] 1 callsite(s) converted to tail calls
  • build: 0 → 1 tail call(s)

[typecheck] 3 top-level item(s) checked, no errors

[interp_lower] no interpolations to lower

[buffer_build] 1 fusion site(s) rewritten, 1 buffered variant(s) synthesized
  • sink build: 1 rewrite(s)
  • synthesized build__buffered

[resolve] 12 ident(s) resolved to slot lookups across 2 fn(s)

[last_use] 11 of 12 resolved slot(s) marked last-use (move-eligible)

[analyze] 3 fn(s) analyzed: 0 no-alloc, 2 recursive, 0 mutual-TCO member(s)
```

Pair with `--emit-ir-after=PASS` when the report says something fired and you want to see the resulting IR. Use case: build a CI gate that fails when buffer_build stops fusing on a known canonical site, or when a hot fn loses its `no_alloc` status.

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

### Debugging a law that didn't auto-prove

When a `verify <fn> law` emits `sorry` (Lean) or empty-body (Dafny), the question is always: did the lowerer fail to classify the shape, or did it classify and the backend's auto-proof fell short?

The proof pipeline runs three IR transforms before codegen — `refinement_lower`, `contract_lower`, `law_lower` — and `--emit-ir-after` dumps `ProofIR` at each stage. The decisive snapshot is `law_lower`:

```bash
aver compile examples/data/quicksort.av --emit-ir-after=law_lower
```

Each `verify <fn> law` shows up with the strategy the classifier pinned. Read the result:

- A concrete strategy (`Commutative { op: Add }`, `Induction { measure: List, ... }`, `MapUpdatePostcondition { kind: HasAfter, ... }`, `LinearRecurrence2SpecEquivalence { impl_fn, spec_fn, helper_fn }`, …) means the lowerer recognized the shape. If the backend then emits `sorry`/empty-body, the gap is in the backend's tactic emission for that strategy — open an issue against the proof backend, not the law.
- `BackendDispatch` means the classifier had no shape match and punted to the backend's generic fallback. The fix is either a new strategy in the classifier or a source-level rewrite into a shape the classifier already knows.

Pair with `--emit-ir-after=refinement_lower` when the law quantifies over a refinement type (e.g. `Natural`) and you want to confirm the predicate rode through to the law's quantifier. Pair with `--emit-ir-after=contract_lower` when a `when` clause is supposed to become a theorem premise.

## Quick routing

Use Rust when you want:
- a normal Cargo project
- deployment without the Aver runtime
- Rust tests generated from `verify`

Use Lean when you want:
- proof artifacts for pure Aver code
- proof artifacts for classified effectful laws via Oracle lifting
- `verify` as executable Lean checks (`native_decide`)
- `verify law` as candidate universal theorems for supported shapes, with
  sampled or checked-domain fallback for the rest
- a path from Aver code to formal verification

Use Dafny when you want:
- automated `verify law` checking without writing proof tactics
- automated checking of Oracle-lifted classified effect laws
- Z3/SMT solver attempting universal proofs for you
- a quick smoke test of whether your laws are provable

## Lean vs Dafny

| | Lean | Dafny |
|---|---|---|
| Verify cases | `native_decide` — always works | Not emitted (Z3 can't compute) |
| Verify laws | Hand-crafted tactic strategies, including Oracle-lifted classified effects | Z3 attempts automatically, including Oracle-lifted classified effects |
| Proof quality | Kernel-verified (gold standard) | SMT-checked (no counterexample found) |
| Effort | High (strategy per pattern) | Zero (just emit and run) |
| External deps | Lean 4 + Lake | Dafny + .NET + Z3 |

Both backends complement each other. Lean is the formal proof target; Dafny is the automated verification target.

## Adding a new backend

To add a new generated backend such as `js`, `go`, or `python`:

1. Add a new CLI command or extend an existing backend command in `src/main/cli.rs`
2. Create `src/codegen/<target>/mod.rs` with `pub fn transpile(ctx: &CodegenContext) -> ProjectOutput`. Take `&mut CodegenContext` if your backend depends on derived facts (`mutual_tco_members`, `recursive_fns`, `fn_analyses`) — the entry point can call `ctx.refresh_facts()` upfront to keep test stubs working.
3. Add the command handler in `src/main/commands.rs`
4. Add `pub mod <target>;` in `src/codegen/mod.rs`

`CodegenContext` is backend-agnostic. It carries the type-checked AST, function signatures, memo metadata, module dependencies, and the IR-level analysis facts (`mutual_tco_members`, `recursive_fns`, `fn_analyses`) populated by the pipeline's `analyze` stage.

### Pipeline contract — what your backend sees

The seven-stage pipeline (`src/ir/pipeline.rs`) commits to a specific IR shape per stage. Where you wire your backend in determines which AST nodes you handle and which intrinsics you emit:

- **Runtime backends** (VM, WASM, Rust today) run with the full pipeline. They never see `Expr::InterpolatedStr` (`interp_lower` desugars it to `__buf_*` + `__to_str` intrinsics) or the canonical `String.join(<builder>(args, []), sep)` shape (`buffer_build` rewrites it to `__buf_finalize(<builder>__buffered(...))` and synthesizes the buffered variant). Implement the four `__buf_*` intrinsics + `__to_str`; you get O(N) string ops and deforestation for free.
- **Proof backends** (Lean, Dafny) skip `interp_lower` and `buffer_build` because they consume source-level IR. They handle `Expr::InterpolatedStr` and `String.join` natively. Pass `apply_traversal_lowering: false` to `build_codegen_context`.
- **REPL** is the only legitimate consumer of pre-resolve IR (single-statement evaluation, throwaway). VM keeps its `compile_interpolated_str` for this path.

A new backend chooses where on this spectrum it sits. Default: full pipeline (cheapest backend code, free deforestation).

For per-pass introspection while debugging your backend, use `aver compile <FILE> --emit-ir-after=PASS` to print the IR snapshot the codegen will receive.
