# Phase 2c.3 — known bug after rewrite wires up

After Phase 2c.3a/b/c/d landed, `aver compile` on a program with a
buffer-build fusion site (e.g. fractal's `String.join(allRows(.., []), "\n")`)
produces:

    WASM emit produced invalid bytecode: type mismatch: expected i32, found i64 (at offset 0xbf1)

The detection + synthesis + rewrite layers are correct — verified by:

- `aver check` reports the right sink + fusion-site count.
- `synthesize_buffered_variants` builds the expected buffered FnDef
  with proper threading via expression composition (test
  `synthesizes_buffered_variant_from_real_builder` covers shape).
- `find_fusion_sites` finds the canonical case in fractal.av.

The breakage is in the WASM emit of the rewritten / synthesized
code. Likely causes (to investigate):

1. The synthesized FnDef param type `Buffer` parses as
   `Type::Named("Buffer")`, which `aver_type_to_wasm` correctly
   maps to `WasmType::I32`. But the body's `__buf_append` /
   `__buf_finalize` calls might leave i64 on the stack somewhere
   the emitter expects i32 (Buffer is an i32 ptr; the i32→i64
   confusion suggests one of the intrinsic dispatches has a wrong
   instruction sequence).

2. The call to `<fn>__buffered` from the rewritten fusion site
   might not resolve to a fn index because the buffered variant
   is appended as a plain `TopLevel::FnDef` — the WASM emitter
   walks `ctx.items` for user fns AND `ctx.synthesized_buffered_fns`,
   so we now have BOTH copies in the list. Duplicate entry might
   be confusing fn-index resolution. (Check `user_fns` collection
   logic in `src/codegen/wasm/emitter.rs`.)

3. The synthesized body uses `Expr::TailCall(target)` where
   `target = "<fn>__buffered"`. After resolver runs, the target
   string in `TailCallData` stays the same — but the emitter's
   tail-call lowering looks up by name in `mutual_tco_members` /
   `fn_indices`. The buffered variant might not be classified
   into the right tail-call group.

Next session: verify by adding a `wasm-tools print` dump of the
emitted user.wasm BEFORE wasm-merge bundles in the runtime — that
will show where the i64 sneaks in. Most likely fix: drop the
duplicate-entry path (run `run_buffer_build_pass` BEFORE
`build_context` parses fn_defs out of items, so synthesized fns
flow through `items → fn_defs` once instead of being added twice).

What's already on this branch that's sound:

- Phase 2b runtime helpers (rt_buffer_new/append_str/finalize)
- Phase 2c.1 emitter import wiring
- Phase 2c.2 synthesizer with C'-correct buffer threading
- Phase 2c.3a ctx field for synthesized fns
- Phase 2c.3b emitter user_fns iteration over synth list
- Phase 2c.3c sep-unless-first conditional dispatch + new/finalize dispatch
- Phase 2c.3d AST rewrite + classify_named_callee recognition + commands.rs pre-resolver wiring

Type-check + verify tests all green; the breakage shows only when
emitting WASM bytes from a program containing a fusion site.
Programs without sinks compile fine.
