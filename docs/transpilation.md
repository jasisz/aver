# Code Generation

Two commands generate code, and between them they drive three backends:

- [Rust backend](rust.md): deployment-oriented Cargo project generation via `aver compile`
- [Lean backend](lean.md): proof export for pure Aver code and Oracle-lifted classified effects via `aver proof`

The backends solve different problems. They share the same `CodegenContext` infrastructure.

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
                                   PASS ∈ { parse, tco, typecheck, interp_lower, buffer_build, chars_fusion, list_build, resolve, last_use, analyze }.
                                   Use diff -u between two stages to see exactly which expressions a pass rewrote.
      --explain-passes             Run the full pipeline (no codegen) and print a per-pass diagnostic report.
                                   Reports tail-call conversions, interpolations lowered, fusion sites + sinks,
                                   slots resolved, last-use markers, alloc/recursion facts.
      --json                       Emit the per-pass report as JSON (with --explain-passes); shape is
                                   { schema_version: 1, passes: [{ stage, summary, details: [...] }, ... ] }.
```

### `--emit-ir-after` quick map

The compiler runs its IR transforms in a fixed stage order (see `src/ir/pipeline.rs`). `--emit-ir-after=PASS` stops before codegen and prints the IR snapshot taken right after the named stage:

| Stage          | What changes between stages                                                |
|----------------|---------------------------------------------------------------------------|
| `parse`        | AST as the parser emitted it; baseline                                     |
| `tco`          | Tail-position recursive calls become `<tail-call:fn>(args)`                |
| `typecheck`    | Read-only: IR identical to `tco`, errors land in stdout                     |
| `interp_lower` | `"a${x}b"` desugars to `__buf_finalize(__buf_append(... __to_str(x) ...))` |
| `buffer_build` | `String.join(<builder>(args, []), sep)` rewrites to `__buf_finalize(<builder>__buffered(...))` and synthesizes the buffered variant |
| `chars_fusion` | `String.chars(s)` consumed linearly by a self-recursive loop becomes a `__str_cursor_*` walk over `s` with a synthesized `<loop>__cursor` variant, and a match over single-character literals becomes a `__str_code1*` codepoint comparison |
| `list_build` | a loop that collects with `List.prepend` and reverses on the way out becomes a `__lst_*` builder threaded through a synthesized `<loop>__collected` variant, with the call sites that start the accumulator at `[]` moved onto it; when the collected result's only reader is the standard library's `Bytes.fromList`, the variant is retargeted to the `__byt_*` byte builder and the `fromList` call is deleted |
| `byte_sink` | wasm-gc/wasip2's byte-only gate: run complete list-build detection on a copy, then replay from pristine IR and commit only variants retargeted to `__byt_*`; generic `__lst_*` never reaches those backends |
| `resolve`      | `Expr::Ident` → `<name>` (resolved slot), `<resolved>` collapse for unknown |
| `last_use`     | Final references annotated as `<name:last>` so backends MOVE instead of COPY |
| `analyze`      | FnDef headers gain fact tags `[no_alloc, locals=N, recursive×N]`    |

### `--explain-passes` — per-pass diagnostic report

This runs the same pipeline, but instead of dumping IR shapes it prints a structured report of what each pass decided:

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

When the report says a pass fired and you want to see the resulting IR, follow up with `--emit-ir-after=PASS`. One use is a CI gate that fails when buffer_build stops fusing at a known canonical site, or when a hot fn loses its `no_alloc` status.

## `aver proof`

```
aver proof <FILE> [OPTIONS]

Options:
  -o, --output <OUTPUT>            Output directory for the generated project
      --name <NAME>                Project name (default: derived from file name)
      --module-root <MODULE_ROOT>  Resolve `depends [...]` from this root (default: current working directory)
      --verify-mode <VERIFY_MODE>  auto | sorry | theorem-skeleton
```

The export only describes code that exists in your source. Passes that synthesize code of their own (`interp_lower`'s string-buffer chain, `buffer_build`'s fused sink, and the traversal fusion that follows them) cannot reach a proof. The pipeline snapshots the AST before the first of them and the proof stages read that copy, so no flag on any caller can put an entity you never wrote into a theorem. A pass that only rewrites code you did write runs on the copy as well. Today that is `escape`, which takes a record built at a call site and only read by the callee, and replaces it with the callee's own body. It runs on the copy because a certificate has to state its theorems about the same program its certified bytes were compiled from. An exported proof therefore describes your source as your artifact was built from it, and `aver compile --target wasm-gc --certify` produces a model and a binary that render one and the same program. `--emit-ir-after=chars_fusion` shows what the runtime backends compile for the ENTRY module, including fused sinks and character cursors (dependencies are loaded pristine on that diagnostic path). The export does not read that dump.

### Debugging a law that didn't auto-prove

When a `verify <fn> law` emits `sorry`, there are two possibilities. Either the lowerer failed to classify the shape, or it classified the shape and the backend's auto-proof fell short.

The proof pipeline runs three IR transforms before codegen (`refinement_lower`, `contract_lower`, `law_lower`), and `--emit-ir-after` dumps `ProofIR` at each stage. The snapshot that settles the question is `law_lower`:

```bash
aver compile examples/data/quicksort.av --emit-ir-after=law_lower
```

Each `verify <fn> law` appears with the strategy the classifier pinned to it:

- A concrete strategy (`Commutative { op: Add }`, `Induction { measure: List, ... }`, `MapUpdatePostcondition { kind: HasAfter, ... }`, `LinearRecurrence2SpecEquivalence`, …) means the lowerer recognized the shape. If the backend then emits `sorry`, the gap is in the backend's tactic emission for that strategy. Open an issue against the proof backend; the law is fine.
- `BackendDispatch` means no shape matched and the classifier handed the law to the backend's generic fallback. Fix it with a new strategy in the classifier, or rewrite the source into a shape the classifier already knows.

Use `--emit-ir-after=refinement_lower` as well when the law quantifies over a refinement type (e.g. `Natural`) and you want to confirm the predicate reached the law's quantifier. Use `--emit-ir-after=contract_lower` when a `when` clause should become a theorem premise.

## Quick routing

Use Rust when you want:
- a normal Cargo project
- deployment without the Aver runtime
- Rust tests generated from `verify`

Use Lean when you want:
- proof artifacts for pure Aver code
- proof artifacts for classified effectful laws via Oracle lifting
- `verify` as executable Lean checks (`native_decide`)
- `verify law` as candidate universal theorems for supported shapes, with sampled or checked-domain fallback for the rest
- a path from Aver code to formal verification

## Adding a new backend

To add a new generated backend such as `js`, `go`, or `python`:

1. Add a new CLI command or extend an existing backend command in `src/main/cli.rs`
2. Create `src/codegen/<target>/mod.rs` with `pub fn transpile(ctx: &CodegenContext) -> ProjectOutput`. Take `&mut CodegenContext` if your backend depends on derived facts (`mutual_tco_members`, `recursive_fns`, `fn_analyses`); the entry point can then call `ctx.refresh_facts()` first so test stubs keep working.
3. Add the command handler in `src/main/commands.rs`
4. Add `pub mod <target>;` in `src/codegen/mod.rs`

`CodegenContext` is backend-agnostic. It carries the type-checked AST, function signatures, module dependencies, and the IR-level analysis facts (`mutual_tco_members`, `recursive_fns`, `fn_analyses`) populated by the pipeline's `analyze` stage.

### Pipeline contract — what your backend sees

The seven-stage pipeline (`src/ir/pipeline.rs`) fixes the IR shape at each stage. The point where you attach your backend decides which AST nodes you handle and which intrinsics you emit:

- **Runtime backends** enable each fabricating pass only after lowering its closed intrinsic contract. VM and Rust implement the full mutable-buffer/list-builder set: `interp_lower` removes `Expr::InterpolatedStr`, `buffer_build` lowers buffered joins through `__buf_*` / `__to_str`, and `list_build` uses `__lst_*` or the byte-retargeted `__byt_*` sink. wasm-gc and wasip2 enable `buffer_build` with a growable GC byte array, logical length, and explicit first-fragment bit; they keep `interp_lower` off on purpose, because native fixed-part interpolation already uses a variadic concat with one allocation. They also enable `chars_fusion`: its nine `__str_*` cursor/codepoint intrinsics walk the existing UTF-8 String array by byte offset and use the shared Unicode case tables. The independent byte-only list-build gate lowers canonical `Bytes.fromList` consumers through `__byt_*` directly into the nominal packed-u8 carrier; generic `__lst_*` sinks remain disabled. Certified wasm-gc artifacts temporarily retain source traversal until the independent byte-level certificate wall classifies the handwritten String-builder, cursor, and byte-sink helpers; runtime and ordinary compile artifacts use them. A backend that cannot lower one of these contracts must leave that pass off. Emitting a fabricated call that can trap is not an option.
- **The proof backend** (Lean) skips `interp_lower`, `buffer_build`, `chars_fusion`, `list_build`, and `byte_sink` because it consumes source-level IR. It handles `Expr::InterpolatedStr`, `String.join` and `String.chars` natively. Pass `apply_traversal_lowering: false` to `build_codegen_context`.
- **REPL** is the only legitimate consumer of pre-resolve IR (single-statement evaluation, throwaway). VM keeps its `compile_interpolated_str` for this path.

A new backend picks its place on this spectrum. The default is the full pipeline, which means the least backend code and deforestation at no extra cost.

To inspect individual passes while debugging your backend, run `aver compile <FILE> --emit-ir-after=PASS`. It prints the IR snapshot your codegen will receive.
