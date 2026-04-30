# Phase 2c — root cause for fractal CF runtime crash

After Phase 2c.3a/b/c/d landed, fractal compiles cleanly under
`--preset cloudflare`, the wasm validates, the runtime helpers
are wired, the synthesized `<fn>__buffered` body has
retain/rebase calls covering view + buf + sep across TCO
compaction. Local execution under wasmtime works (`aver run --wasm`
on a multi-module bench reproducing the same shape passes 1000
iterations cleanly). But Cloudflare deploy returns 500 with:

    aver_http_handle threw: RuntimeError: memory access out of bounds

## Root cause

The synthesized buffered fn's false-arm body uses expression
composition for buffer threading:

    __buf_append(__buf_append_sep_unless_first(__buf, __sep),
                 renderRow(row, charsW, ..., view))

WASM evaluation order leaves the buffer pointer on the stack
between the two intrinsic calls:

    1. push __buf, __sep       (param i32s)
    2. call sep_unless_first   (pops 2, pushes 1 i32 buf)
                               # stack: [.., buf]
    3. push args for renderRow (row, charsW, ..., view)
    4. call renderRow          # ALLOCATES — may trigger GC
                               # stack: [.., buf (STALE), elem]
    5. call __buf_append       # uses stale buf

Between step 2 and step 5 the WASM stack carries an i32 heap
pointer that's NOT a GC root. When step 4's renderRow allocates
enough to cross the watermark, `rt_collect_*` runs, the buffer
object gets compacted to a new location, and step 5 reads through
the now-stale pointer. Result: OOB access.

The TCO compaction retain/rebase logic correctly handles
inter-iteration buffer threading (frame-level), but it doesn't
help with intra-expression GC during a single iteration.

## Fix design (C' refined — what the review originally hinted at)

The C' review explicitly said: thread buffer through Stmt::Binding
sequence, not expression composition. I used composition for
brevity. The bindings approach would naturally route each
intermediate through a local slot, which Aver's frame compaction
already retains across mid-expression GC.

Match-arm bodies are single expressions in Aver's AST, so the
sequencing has to live somewhere else. Approach: synthesize TWO
fns per matched buffer-build:

    fn <fn>__buffered(args, __buf, __sep) -> Buffer
        match terminating_cond
            true  -> __buf
            false -> <fn>__buffered_step(args, __buf, __sep)

    fn <fn>__buffered_step(args, __buf, __sep) -> Buffer
        __buf1 = __buf_append_sep_unless_first(__buf, __sep)
        __elem = renderRow(args)
        __buf2 = __buf_append(__buf1, __elem)
        <fn>__buffered(next_args, __buf2, __sep)

Each Stmt::Binding (`__buf1`, `__elem`, `__buf2`) compiles to a
fn-local slot. Aver's standard fn-body compaction analysis already
treats those slots as GC roots. Mid-expression allocation in
`__elem`'s computation can move objects, and the stored `__buf1`
gets rebased along with everything else in the frame.

`<fn>__buffered_step` is just a normal fn call from `<fn>__buffered`,
so its arg-passing already runs through the standard call
convention with proper retain/rebase coverage.

## What's solid as-is on this branch

- Phase 1: detection (sinks + fusion sites)
- Phase 1.5: aver check diagnostic surfacing both
- Phase 2a: ConsumerKind enum
- Phase 2b: WASM runtime helpers (rt_buffer_new/append/finalize +
  OBJ_BUFFER kind=13 + GC dispatch + ABI export contract test)
- Phase 2c.1: emitter import wiring
- Phase 2c.2: synthesizer (current expression-composition shape;
  needs the helper-fn restructuring above to fix the GC issue)
- Phase 2c.3a/b/c/d: ctx threading, builtin dispatch (__buf_new/
  append/append_sep_unless_first/finalize), classify_named_callee
  recognition, run_buffer_build_pass + commands.rs wiring
- Phase 2c.3 follow-up: type-wrap on i64→i32 cap, dep-module
  pre-pass invocation, dedup of synth in build_context vs items,
  sig injection (fn_sigs entries for synthesized variants AND for
  the four __buf_* intrinsics so infer_aver_type returns
  Type::Named("Buffer") and TCO compaction retains the buf).

The detection/synthesis/rewrite pipeline is structurally correct.
The remaining work is just the synthesizer body shape — replacing
expression composition with the two-fn helper approach.
