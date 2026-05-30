# Core MIR — RFC

Status: pinned for Phase 1 of [#252](https://github.com/jasisz/aver/issues/252) (0.24 epic).
Open for amendment via PR before Phase 2 lands the data model.

## Motivation

Typed Resolved HIR is the right foundation — stable identity via `FnId` / `TypeId` / `CtorId`, type stamps via `Spanned<ResolvedExpr>.ty()`, no string side channels. But every runtime backend still re-walks `ResolvedExpr` and re-derives executable semantics independently:

- the VM interpreter walks pattern arms and rebuilds branch dispatch
- the Rust backend interprets `Expr::ErrorProp` and emits `?`
- the wasm-gc backend interprets the same node and emits result-tag dispatch
- the wasip2 backend forks again

That's four implementations of "what does `?` mean executably", four implementations of "what does `match` mean executably", four implementations of `Try` / record / constructor / tail call semantics.

The concrete cost surfaced in 0.23 stage 8b: the post-pipeline AST had already desugared `?` into nested `match Result.Err -> Err`, so the `ResultPipelineChain` recognizer in `analysis::shape` couldn't see "this was a `?`-chain" any more. The fix was a `step_fns` side-channel on `ModulePattern::ResultPipelineChain` — a smuggled list of the original step fns, carried through because the AST itself no longer remembers. That side-channel is symptomatic. The real fix is to stop the lowering pipeline from collapsing semantic operations before backends and analyzers get to read them.

MIR is that "stop collapsing". It's an executable middle-end where backend-neutral semantics live — `Try` as a node, `match` as a structured operation, constructors with `CtorId`, tail calls with `FnId` — and every consumer (VM, Rust, wasm-gc, wasip2, and future inliner / monomorphizer / escape analyzer) reads from it instead of forking.

## Non-goals

This RFC is deliberate about what MIR is **not**:

- **MIR is not a flatter form of HIR.** Phase 1–4 keep structured `match` and structured expressions. A later Flat MIR / LIR with explicit basic blocks may follow if optimizer or backend needs demand it.
- **MIR does not subsume ProofIR.** ProofIR answers "what does this program prove". MIR answers "what does this program execute". They share `FnId` / `TypeId` / `CtorId` identity via `SymbolTable` but stay separate IRs with separate consumers.
- **MIR does not replace the existing frontend / normalization pipeline.** TCO, typecheck, `interp_lower`, `buffer_build`, `escape`, `last_use`, `BuildSymbols`, `NameResolve` all stay. MIR consumes their output.
- **MIR does not destroy target-native semantics.** `?` doesn't desugar to `match` just because it's now in MIR. Each backend chooses its final lowering: Rust emits `?`, VM emits a tag check + early return, wasm-gc emits result-tag dispatch.

## Inputs

- `ResolvedProgramView` — entry items + dep modules, post-pipeline.
- `SymbolTable` — `FnId` / `TypeId` / `CtorId` resolution.
- Optionally `ProgramShape` (from `analysis::shape`) when an optimizer pass downstream wants typed recognition payloads (`ModulePattern::WrapperOverRecursion`, `ResultPipelineChain`, etc.). Not required for Phase 1–4.

No new parser, no new typechecker. MIR construction is a lowering pass over already-typed already-resolved HIR.

## Outputs

`MirProgram` keyed by `FnId`. Per-fn body is a structured tree of `MirExpr` (Phase 2 details), not basic blocks. Backends consume:

```text
let mir_fn = mir_program.fn_by_id(fn_id);
// walk mir_fn.body, emit per-target code
```

The exact field layout lands in Phase 2 alongside snapshot-style dumps. Phase 1 only pins the shape decisions below.

## Pinned shape decisions (Phase 1)

### `Try` is a node, not desugared

```text
Try(value)
TryBind { value, ok_binding, ok_body }
```

with canonical semantics `evaluate value; if Ok(v), continue with v; if Err(e), return Err(e)`.

Backends pick their final shape:

- Rust emits `?` directly on the underlying `Result<T, E>`.
- VM emits an `is_err` tag check + early return.
- wasm-gc emits a `br_on_struct` against the result discriminator.
- Proof-oriented paths may still expand to explicit `match` if a strategy needs the branch form (rare; usually they consume MIR or HIR depending on the recognizer's needs).

This is the **single most important** decision of the RFC. It's the direct response to the 0.23 stage 8b problem: every later analyzer or backend sees the `Try` node intact.

### `match` stays structured

```text
Match {
  subject,
  arms: Vec<MirMatchArm>,
}
MirMatchArm { pattern, body }
```

Patterns reference `CtorId` for constructor variants (not stringified constructor names), `Literal` for literal arms, structural `Cons` / `EmptyList` for list patterns, `Tuple` for nested tuples, `Wildcard` / `Ident` for catch-alls.

Flattening to terminator-style basic blocks (`Switch`, jumps, joins) is **not** in this RFC. A later Flat MIR / LIR can do that if optimizer demands it.

### Constructors keep `CtorId`

`Construct(CtorId, args)`. No stringified `"Result.Ok"` lookup. `RecordCreate(TypeId, fields)` and `RecordUpdate(base, updates)` follow the same rule.

### Tail calls keep `FnId`

`TailCall { target: FnId, args }`. Whether a call is a tail call is a property MIR carries (set during lowering, derived from HIR's `TailCall` after the TCO pass). Backends decide whether to emit a real tail call (wasm-gc tail-call instructions) or a loop rewrite (Rust).

### `IndependentProduct` stays a node

```text
IndependentProduct { items, unwrap_results: bool }
```

The mode (`?!` vs `!`) is carried as a bool. The compile-time `independence` mode (`complete` / `cancel` / `sequential`) is **not** carried in MIR — that's a runtime policy decision the backends consult separately. MIR represents the source-level operation, not the chosen schedule.

### Effects live on `MirFn`, not on every call

```text
MirFn { fn_id, params, return_type, effects: Vec<EffectName>, body }
```

Per-call-site effect annotation is on the table for Phase 6 (effect scheduling), but for Phase 1–4 the function-level effect list is enough. Backends already consume it from HIR; the move to MIR is a copy-through.

### Identity is typed

Every reference to a user declaration uses an ID type:

| Concept | ID type |
|---|---|
| function | `FnId` |
| nominal type | `TypeId` |
| constructor variant | `CtorId` |
| module | `ModuleId` |
| local binding | `LocalId` (introduced in Phase 2) |

No string-based lookups, no name-based re-derivation. `SymbolTable` is the source of truth and an input to construction.

### Source spans travel through MIR

Each `MirExpr` carries a `Span` for diagnostics + future correlation with `ProofIR`. The exact field shape lands in Phase 2.

## Phase plan

### Phase 1 — RFC + boundaries (this PR)

- `src/ir/mir/mod.rs` skeleton with the doc-comment summary.
- `src/ir/mir/RFC.md` (this document).
- `pub mod mir;` in `src/ir/mod.rs`.
- No types yet, no lowering, no consumer.

### Phase 2 — Data model + dump

- `MirProgram`, `MirFn`, `MirExpr`, `MirMatchArm`, `MirPattern`, `MirTerminator`, `LocalId`.
- Textual dump (`fmt::Display` impl) suitable for snapshot tests.
- `aver compile --emit-ir-after=mir` wires the dump.
- Snapshot tests on a small fixed-shape corpus (`examples/data/sum_acc.av`, `examples/core/result_chain.av`, `examples/data/fibonacci.av`).

### Phase 3 — HIR → MIR lowering, in waves

One PR per wave so review surface stays bounded:

1. literals + locals + binops + `return` + `Project`
2. user calls + builtin calls + constructors + record create / update
3. `match` (structured), `Try` / `TryBind`, `TailCall`, `IndependentProduct`

Each wave: dump snapshot for a representative example, no behavioral change for any backend yet.

### Phase 4 — VM vertical slice

- VM consumes MIR for the lowered subset.
- HIR fallback for un-migrated constructs is **explicit** — every fallback site has a named `// FALLBACK(HIR): why` comment and a test that asserts the construct still works.
- Existing VM snapshot tests stay green.
- New `tests/mir_vm_parity.rs` proves `Try`, `match`, constructors, records, and calls run the same through MIR as through HIR on a small corpus.

## Open questions (deferred until Phase 2 review)

These don't block Phase 1. They get decided when the data model lands.

- **`MirExpr` shape** — expression-based (every node has a value), block-based (`Let` chains end in a `Terminator`), or hybrid structured IR? Hybrid is the strawman. Phase 2 PR picks.
- **`LocalId` stability** — assigned at MIR construction time, or carried from HIR's existing slot indices? Carrying is simpler; assigning is more flexible for later optimizers. Phase 2 picks.
- **Effects per call site** — does Phase 1–4 carry call-site effects, or only function-level? Function-level only, deferred to Phase 6 if needed.
- **`?` on user-defined `Result`-like types** — Phase 1 pins `Try` for built-in `Result<T, E>`. Whether user-defined sum types with an `Ok` / `Err` shape can ride the same node is an extension question. Phase 3 wave 3 picks (when `Try` lowering lands).
- ~~**`TryBind` vs `Let { value: Try(_), … }`**~~ — **resolved**: `TryBind` dropped during wave 3 prep; the composition `Let { binding, value: Try(step()), body }` carries identical semantics and saves one variant. Consumers that need to recognize the bind-and-propagate pattern walk `Let` and inspect `value.node` for `MirExpr::Try`.
- **wasm-gc flatten ordering** — wasm-gc currently flattens multi-module programs into a single AST before codegen. Does Phase 5 (wasm-gc on MIR) flatten in MIR or in HIR? Out of scope for 0.24 but noted so reviewers know it exists.

## Guardrails

- `Try` is a node. Don't desugar. (Restated for the third time because it's the most important thing.)
- No flag-day backend rewrite. Phase 4 = VM only.
- No string-based identity. Every declaration ref goes through an ID type.
- ProofIR is separate. Don't merge.
- Existing frontend / normalization passes (`TCO`, `typecheck`, `interp_lower`, `buffer_build`, `escape`, etc.) stay where they are. The 0.24 epic does not move them.
- A pass moves into MIR only when a concrete reviewer-checkable win is identified — not on principle, not for purity.

## Definition of done (for the 0.24 epic, not for this PR)

- `src/ir/mir/` exists with the documented model + dump.
- HIR → MIR lowering covers the common executable expression set.
- VM compiles and runs the shipped corpus through the MIR path.
- `aver compile --emit-ir-after=mir` works on every shipped example.
- Snapshot tests pin `Try`, `match`, constructors, records, and calls.
- CHANGELOG entry covers user-visible additions (dump flag, new IR layer, behavioral parity).
- Codename + tagline picked during release prep.
