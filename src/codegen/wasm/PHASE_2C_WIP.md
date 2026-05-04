# Phase 2c — WASM emitter integration for deforestation lowering

Phase 2b landed runtime helpers (`rt_buffer_new`, `rt_buffer_append_str`,
`rt_buffer_finalize`, `OBJ_BUFFER` kind=13). Detection layer +
`CodegenContext` threading from earlier phases makes
`ctx.buffer_build_sinks` and `ctx.buffer_fusion_sites` available
during emit. What's still missing: actually wiring the matched fns
and call sites to the helpers.

## The two blockers

### (1) Synthesizing the `<fn>__buffered` variant

The buffered variant has a different param list (drop `acc`, add
`__buf: i32` + `__sep: i32`) and a body that mutates an external
buffer. Aver's AST has no native expression for `mut_borrow_append`
— `rt_buffer_append_str` is a runtime helper, not a builtin. Three
candidate paths, none small:

- **A. New AST node `Expr::RuntimeCall(name, args)`.** Synthesized
  variant FnDefs have legitimate AST; existing `emit_fn_body`
  pipeline handles them. Cost: touches AST, parser (won't accept
  syntactically but synth is post-parse so fine), every visitor
  (alloc info, last-use, vars, infer, etc.) needs a passthrough
  arm. Estimated 300–500 LOC across many files.

- **B. Direct WASM IR emission for the buffered body.** Bypass AST
  for the body; hand-emit `Instruction::*` sequence. Need to
  reproduce match dispatch, TCO loop, local management, no-alloc
  fast path heuristics — all of which `emit_fn_body` already
  handles for normal fns. Risk: drift from the production emit
  path; bugs in TCO or no-alloc might appear only here. ~500–800 LOC
  of careful WASM emit.

- **C. AST rewriting hack: introduce `__buf_append(elem)` as a
  pseudo-builtin** that lowering recognises and emits as
  `rt_buffer_append_str` with an implicit buffer arg threaded via a
  hidden local. `__buf` becomes a special parameter name. Synth
  variant uses normal AST builtins everywhere except for the magic
  name, similar to how `Vector.set` already gets fused via owned-
  mutate dispatch when a last-use slot is detected (release notes
  0.14.0). ~150 LOC, but introduces a magic identifier convention.

### (2) Rewriting the fusion-site call expression

`String.join(matched_fn(args, []), sep)` → buffer alloc + buffered
variant call + finalize. Lives in expression emit
(`src/codegen/wasm/expr/emit.rs`); the existing String.join lowering
needs a special-case branch when the first arg is a sink fn call.
~100 LOC. Less risky than (1); depends on (1) producing the
buffered variant.

## Current branch state

Phase 1+1.5+2a+2b+ctx-threading are landed and pass full test
suite + WASM runtime end-to-end test. Detection pipeline emits
useful info via `aver check`:

```
↻ 1 buffer-build sink(s) [allRows], 1 fusion site(s)
```

No behavior change in the WASM emitter; helpers are dead code
until Phase 2c lands.

## Recommended path forward — option C' (revised after review)

Pick **(C')** — option (C) with the following corrections that
came out of follow-up review. The original (C) sketch had a real
correctness bug and an unresolved-type echo of a foot-gun the type
checker just got fixed for in commits `6450dd1` and `cf443d0`.

### Critical correction: buffer threading

`rt_buffer_append_str` may return a NEW pointer after grow (the old
buffer's payload was memcpy'd into a fresh allocation). The
buffered variant body MUST thread this through bindings, not
discard it:

```aver
fn allRows__buffered(row, charsW, ..., view, __buf, __sep) -> Buffer
    match row >= charsH
        true  -> __buf
        false ->
            __buf1 = __buf_append_sep_unless_first(__buf, __sep)
            __row  = renderRow(row, charsW, ..., view)
            __buf2 = __buf_append(__buf1, __row)
            allRows__buffered(row + 1, charsW, ..., view, __buf2, __sep)
```

The `_ =` (discard) shape sketched in the original (C) plan would
work for ~all calls until the first grow, then read freed/relocated
memory and corrupt randomly — worst kind of bug.

### Both intrinsics return Buffer in both branches

`__buf_append_sep_unless_first(buf, sep) -> Buffer`:
- `buf.len > 0` → `rt_buffer_append_str(buf, sep)` (may grow)
- `buf.len == 0` → return `buf` unchanged

Never `Unit`, never branch-only. Otherwise threading breaks the
same way as the discard case.

### Typed internal intrinsics, not magic strings

Don't add `__buf_append` to the type checker as an unresolved generic
fallback. That's exactly the loophole pattern that just got
fixed for `Result.withDefault` etc. Instead introduce them as
typed internal intrinsics in the resolver:

```rust
enum InternalIntrinsic {
    BufferAppend,                    // Buffer × String -> Buffer
    BufferAppendSepUnlessFirst,      // Buffer × String -> Buffer
}
```

Resolver rejects user code that names `__buf_*` (single allow-list
table); only codegen-synthesized FnDef can produce these calls.
WASM emitter dispatches on the resolved internal symbol, not on
string match. Same `Expr::FnCall` shape, just a different resolved
target — no new AST node.

### Trigger conservatism

`find_fusion_sites` currently checks `String.join(matched_fn(...),
sep)` but doesn't verify the acc-position arg in `matched_fn(...)`
is a literal `[]`. If a user passes a non-empty initial list, the
buffered variant silently drops those elements. Tighten the
detector before lowering: require `Expr::List([])` at acc index.

### Alloc info nuance

Buffered fns are classified as **allocating** (correct: grow path
calls `rt_alloc`). Don't try the no-alloc fast path on them.
Last-use analysis treats the result of every `__buf_append*` as a
fresh value (via the binding threading), so frame compaction +
relocation stay sound. The cost: buffered fns pay the standard GC
framing per call — fine, the win is in skipping cons cells, not
framing.

## Sub-phase split (C')

- **Phase 2c.1** — Add `InternalIntrinsic` enum + resolver entries
  for `__buf_append` / `__buf_append_sep_unless_first` with typed
  signatures. Resolver rejects user references to these names.
  Type checker accepts them via the resolved-symbol path. ~80 LOC
  across resolver + checker.

- **Phase 2c.2** — Synthesize buffered FnDef variants in
  CodegenContext after detection. Body uses regular AST with
  `Stmt::Binding` threading and `Expr::FnCall` to the resolved
  internal intrinsics. Tighten `find_fusion_sites` to require
  literal `[]` at acc-position. ~150 LOC.

- **Phase 2c.3** — WASM lowering for the two intrinsics: emit
  `Call(rt_buffer_append_str)` directly for `BufferAppend`, emit
  conditional sep + append for `BufferAppendSepUnlessFirst`.
  Rewrite fusion-site call expressions to `rt_buffer_new` +
  buffered call + `rt_buffer_finalize`. ~120 LOC.

- **Phase 2c.4** — Bench fractal demo. Expected fullView wall-time
  drop ~107 ms → ~30–40 ms; size unchanged (HTML output identical
  by construction). ~0 LOC; verification only.

Each sub-phase landable independently. Approximate total: 3–5
focused days of work with each sub-phase being a single-session
chunk. Total LOC ~350 vs the original (C) estimate of 150–250 —
the extra is mostly the resolver + intrinsic plumbing that buys
us type safety and prevents the threading bug.

## Why this is queued, not abandoned

The detection + helpers work that's already on this branch is
real value: future deforestation work has a foundation (analyzer,
diagnostic, runtime helpers, ABI contract test). The remaining
emitter integration is a careful piece of engineering that
benefits from proper attention rather than a rushed merge.
