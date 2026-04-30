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

## Recommended path forward

Pick **(C)** for Phase 2c. Smallest blast radius, leverages
existing builtin-fusion machinery, and the magic name `__buf` /
`__sep` is internal-synth-only — never written by users.

Sub-phase split:
- **Phase 2c.1** — synthesize buffered FnDef variants in
  CodegenContext. Body uses regular AST plus the magic
  `__buf_append(elem)` / `__buf_append_sep()` calls.
- **Phase 2c.2** — extend WASM builtin lowering to recognise
  `__buf_append*` and emit `rt_buffer_append_str` directly, with
  the buffer arg coming from the synthesized fn's `__buf` param.
- **Phase 2c.3** — rewrite fusion-site call expressions to
  `rt_buffer_new` + buffered call + `rt_buffer_finalize`.
- **Phase 2c.4** — bench fractal demo, expected fullView wall-time
  drop ~107 ms → ~30–40 ms.

Each sub-phase is landable independently. Approximate total: 3–5
focused days of work, with each sub-phase being a single-session
chunk.

## Why this is queued, not abandoned

The detection + helpers work that's already on this branch is
real value: future deforestation work has a foundation (analyzer,
diagnostic, runtime helpers, ABI contract test). The remaining
emitter integration is a careful piece of engineering that
benefits from proper attention rather than a rushed merge.
