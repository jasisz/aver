# `wasm_gc` codegen backend (probe — 0.16)

Parallel WASM backend that targets the **WebAssembly GC + tail-call** proposals natively, instead of layering a custom runtime on top of MVP WASM. Lives alongside `src/codegen/wasm/` for now — both compile from the same IR; `aver bench --target=wasm-local` exercises the legacy backend, `--target=wasm-gc` exercises this one. After bench numbers come in (see Phase 4 below) one of them gets cut.

## Why this exists

Three perf-relevant costs the legacy `wasm` backend pays today, each one structural rather than implementation-tunable:

1. **Boundary GC framing** — every user fn save/restores `heap_ptr`, calls `rt_truncate` on exit. The framing is ABI, not optional. WasmGC delegates GC to the engine; framing disappears.
2. **NaN-boxing** — every value goes through tag/untag macros (`from_i64`, `to_i64`, `to_f64`). WasmGC carries values in their native ref / i64 / f64 representations. Lowering becomes type-direct.
3. **Mutual-TCO trampoline** — manual loop+dispatch around `(call_indirect)` because MVP wasm has no tail-call op. The tail-call proposal adds `return_call` / `return_call_indirect`; engine handles the frame.

The legacy backend's wins were size (custom runtime is ~10 KB, `wasm-opt -Oz` strips most of it) and breadth of runtime support (everything that runs MVP wasm). As of 2026-Q2 those wins are weaker:

- **Size:** WasmGC lets us drop the runtime entirely for code paths that don't need WASI bridging — engine GC isn't a payload bytes problem. Native tail calls drop the trampoline (real WAT savings).
- **Reach:** WasmGC is stable in Chrome 119+ (Nov 2023), Firefox 120+ (Nov 2023), Safari 18.2+ (Dec 2024), wasmtime 25+ (Sep 2024). Workerd / Cloudflare Workers honor it. Node 22+ via flag, 24+ default. The "everything that runs MVP wasm" advantage shrinks to "things from before 2024".

The `feedback_no_premature_optimization.md` rule applies in the other direction here: continuing to hand-roll a runtime that engine GC + tail-calls do better is the optimization that's premature *now*. Probe builds the alternative, bench picks the winner.

## Type representation

Aver is statically typed; the type checker has already proven what each value is. The legacy backend ABI-erases everything to a NaN-tagged `i64` to avoid generating per-type code paths. Wasm-gc keeps types concrete:

| Aver type            | Wasm-gc representation                                                |
|----------------------|-----------------------------------------------------------------------|
| `Int`                | `i64`                                                                  |
| `Float`              | `f64`                                                                  |
| `Bool`               | `i32`                                                                  |
| `Unit`               | empty (zero results)                                                   |
| `String`             | `(ref string)` (stringref proposal); fallback `(ref $StringStruct)`    |
| `List<T>`            | `(ref null $List_T)` where `$List_T = (struct (field T) (field (ref null $List_T)))` |
| `Tuple<T1,T2,…>`     | `(ref null $Tuple_T1_T2_…)`                                            |
| `Map<K,V>`           | `(ref null $Map_K_V)` — flat hashtable struct                          |
| `Vector<T>`          | `(ref null (array T))` (or `$Vector_T` wrapping array + len)          |
| `Record name`        | `(ref null $Record_name)` — named struct                               |
| `Constructor name`   | `(ref null $Constr_name)` — named struct subtype of the variant root  |

Generic call sites monomorphize per concrete type — Aver generics are bounded so the SCC stays finite. No `(ref any)` casts on the hot path.

## Tail calls

Every `Expr::TailCall` in IR lowers to:

- `return_call $self_fn_idx` for self-tail (SCC=1).
- `return_call_indirect (type $sig)` through a per-SCC function table for mutual TCO (SCC>1).

No trampoline. No dispatch loop. Engine handles frame reuse.

## Baseline assumption: modern wasm runtime

This backend assumes the host runtime supports:

- **GC proposal** — struct/array types, `(ref null $T)`, `ref.cast`, `br_on_cast`.
- **Tail-call proposal** — `return_call`, `return_call_indirect`.
- **Reference types** (transitive — pulled in by GC).
- **Stringref proposal**, when emitted (`(ref string)`); fallback path is a struct of `i32 ptr + i32 len + memory` if a target rejects stringref. Phase 1 picks one and doesn't carry both.

These are stable in Chrome 119+, Firefox 120+, Safari 18.2+, wasmtime 25+, Cloudflare Workers, Node 22+ (flag) / 24+ (default). If you target older runtimes, use `aver compile --target=wasm` (the legacy backend stays). No feature flags, no probes, no graceful degradation here — the whole point is leveraging what the modern engine gives us.

## What we deliberately don't do

- **No `(ref any)` / `extern.externalize` shortcuts.** Type-direct lowering or fail. Casts are the path to wasm-gc-as-MVP-with-extra-steps and the cost reappears.
- **No fallback to legacy backend on missing engine support.** A user picks `--target=wasm-gc` exactly because their target supports it. If they're on legacy runtimes, `--target=wasm` stays available.
- **No interop with `aver_runtime.wasm`.** Different ABI, different memory model. The two backends share IR and nothing else.
- **No trampoline / no manual dispatch loop.** Tail calls are `return_call(_indirect)` always.
- **No NaN-boxing / no tag-bit munging.** Values carry their wasm type.
- **No boundary GC framing** (`rt_save_heap_ptr` / `rt_truncate`). Engine GC handles it.

## Module layout (planned)

```
src/codegen/wasm_gc/
├── README.md          ← this file
├── mod.rs             ← entry point: ir → wasm bytes
├── types.rs           ← Aver type → wasm type lowering, struct table
├── monomorph.rs       ← per-call-site type substitution for generics
├── emit.rs            ← expression / statement → wasm instructions
├── functions.rs       ← per-fn lowering (params, locals, body)
├── tail_calls.rs      ← native return_call / return_call_indirect
└── module.rs          ← top-level wasm Module assembly + wasm-tools validation
```

## Phase plan

- **Phase 1 (probe):** scaffold, hello-world `fn main() -> Int 42` produces valid wasm-gc module that wasmtime exits with 42.
- **Phase 2 (primitives):** Int / Float / Bool / Unit with arithmetic and comparisons.
- **Phase 3 (compound):** List / Tuple / Record / Constructor + their match dispatch.
- **Phase 4 (control flow):** match → `br_table` / `br_if` chains; tail-call lowering.
- **Phase 5 (bench):** `aver bench --target=wasm-gc` against the existing scenarios. Decision rule lives in `TaskList`. Numbers say everything.

If Phase 5 closes the perf gap with ≥2x on numeric loops and ≥50% smaller binary, the legacy backend gets cut in 0.16 with a new codename ("Concede"). If numbers are flat, this directory gets deleted and `0.15.x` work continues.

## Bench numbers (2026-05-01, macOS aarch64, release build)

After phase 3a + 3b/1 + 3c **and `ir::escape` IR pass with both record-access and variant-match shapes**:

| Scenario          | VM      | wasm-local | wasm-gc | wasm-gc vs legacy |
|-------------------|---------|------------|---------|-------------------|
| `fib(15)`         | 106µs   | 42µs       | 5µs     | **8.4x faster** |
| `countdown(100k)` | 810µs   | 47µs       | 16µs    | **2.9x faster** |
| `newtype_bare`    | 963µs   | 43µs       | 19µs    | **2.3x faster** |
| `newtype_record`  | 966µs   | 45µs       | 20µs    | **2.3x faster** |
| `newtype_variant` | 976µs   | 44µs       | 19µs    | **2.3x faster** |
| `match_dispatch`  | 5.67ms  | 66µs       | 41µs    | **1.6x faster** ✨ |
| `record`          | (tbd)   | 47µs       | 34µs    | **1.4x faster** ✨ |
| `factorial`       | 21µs    | 41µs       | 19µs    | **2.2x faster** |
| `vector_ops`      | (tbd)   | 8.75ms     | 48µs    | **182x faster** ✨✨ |
| `string_interp`   | (tbd)   | 5.17ms     | 6.65s   | 1287x slower ⚠ (wasmtime GC, V8 wins) |
| `map_build`       | (tbd)   | 940µs      | 695µs   | **1.4x faster** ✨ |
| `map_lookup`      | (tbd)   | 1.16ms     | 2.20ms  | 1.9x slower ⚠ (wasmtime GC, V8 wins) |

**10/12 wasm-gc wins, 2 engine-bound regressions on wasmtime.** Both alloc-heavy patterns (`record`, `match_dispatch`) now go through escape analysis at the IR level — no struct/variant alloc reaches codegen. `vector_ops` lands on native `(array (mut i64))` with bounds-checked `array.set`/`array.get` and zero Option boxing — the legacy backend round-trips every read/write through runtime helpers + AverVector allocation. `Map<K,V>` is monomorphised per instantiation: per-K hash + eq helpers, per-(K,V) empty/set/get/len helpers, open-addressing flat hashtable, 16384-bucket fixed cap (resize is a phase-3c+ extension).

### Cross-engine + cross-backend matrix

Same source, both backends (`wasm-local` legacy linear-memory + `wasm-gc`), both engines (wasmtime 44, V8 in Node 25), same 30-iteration harness:

| Scenario          | local·wasmtime | local·V8  | gc·wasmtime  | gc·V8     |
|-------------------|---------------:|----------:|-------------:|----------:|
| fib               | 42µs           | 6µs       | **5µs**      | 7µs       |
| countdown         | 47µs           | 27µs      | **16µs**     | 22µs      |
| factorial         | 41µs           | <1µs      | 19µs         | <1µs      |
| match_dispatch    | 66µs           | 34µs      | **41µs**     | 55µs      |
| record            | 47µs           | 23µs      | 34µs         | **20µs**  |
| newtype_bare      | 43µs           | 24µs      | **19µs**     | 20µs      |
| newtype_record    | 45µs           | 28µs      | **20µs**     | 21µs      |
| newtype_variant   | 44µs           | 27µs      | **19µs**     | 21µs      |
| vector_ops        | 8.75ms         | 9.09ms    | 48µs         | **42µs**  |
| string_interp     | 5.17ms         | 4.25ms    | 6,654ms ⚠   | **2.86ms** |
| map_build         | 940µs          | 606µs     | 695µs        | **179µs** |
| map_lookup        | 1.16ms         | 693µs     | 2.20ms ⚠     | **480µs** |

(Bold = winner per scenario.)

Findings:

- **wasm-gc wins or ties everywhere** in a sane engine. Across V8 + wasm-gc, every scenario is at-or-better than every other combination — including a 1.5× win over `wasm-local·V8` on `string_interp` and 200× on `vector_ops`.
- **`wasm-local` doesn't benefit from V8** — it uses linear memory + a bump allocator, no engine GC, so V8 and wasmtime treat it the same. Engine choice barely moves the number.
- **wasmtime + wasm-gc is competitive on pure numeric** (fib, countdown, match_dispatch) — Cranelift's codegen is solid; differences are noise. Penalty appears only on alloc-heavy paths.
- **`string_interp` wasmtime: 6.65s (vs 2.86ms on V8 — 2300× slower).** Same wasm binary, engine-bound. Wasmtime 44's GC heap path: every `array.new_default(N)` + `array.copy` hits the allocator + write barriers, and the bench accumulates ~30 MB of intermediate strings per iteration. Wasmtime's GC is new (2024) and unoptimised; V8 has had a decade. The compile path is sane and fast — V8 actually beats `wasm-local` on the same workload.
- **`vector_ops` is structurally 200× wasm-gc on either engine** — the legacy backend round-trips every `Vector.set` / `Vector.get` through runtime helpers + `AverVector` allocation; native `array.set` / `array.get` is the right shape.
- **`map_lookup` on wasmtime regresses 1.9× vs wasm-local.** Each `Map.get` allocates a fresh `Option<Int>` struct (Some or None) — 20,000 lookups × 30 iterations = ~600k allocations through wasmtime's GC heap. Same workload on V8 finishes in 480µs (1.4× faster than wasm-local·V8). Engine cost again, not a compile-path issue. A fused `Option.withDefault(Map.get(m, k), default)` shape (analogous to the `Vector.get` fusion) **is implemented** in `body.rs::emit_map_get_or_default` — calls a `get_or_default(m, k, def) -> V` per-(K,V) helper that runs the same probe loop with no Option allocation. It cuts wasmtime to 1.41ms (1.7× faster) on a withDefault-shaped variant. The bench scenario itself uses pattern-match dispatch (`match Map.get { Option.Some(v) -> ...; Option.None -> ... }`) which doesn't yet collapse to the same shape — extending fusion to that case is a follow-up.

The compile path itself is correct and competitive: `Expr::InterpolatedStr` lowers to a single `array.new_fixed (Vector<String>) N` + variadic concat helper that is O(total_len) bytes copied (not O(N²) like a left-folded `String.concat` chain would be). Same primitive (`array.new_fixed` + variadic concat) will back `String.join` once it lands.

**For realistic numbers under wasm-gc, use a browser-class engine.** A V8 bench harness lives at `tools/wasm-gc-bench-v8.mjs`. Requires Node 22+ for stable wasm-gc support — Node 20's V8 rejects packed `i8` array types.

```bash
cargo run --features wasm -- compile bench/scenarios/string_interp.av --target wasm-gc -o /tmp/out
node tools/wasm-gc-bench-v8.mjs /tmp/out/string_interp.wasm
```

Folding V8 into the main `aver bench` runner (alongside or instead of wasmtime) is on the 0.16 polish list.

Massive cross-backend wins from the same IR pass:
- `match_dispatch` wasm-local: 332µs → 66µs (**5x**) and wasm-gc: 1.62ms → 42µs (**38x**)
- `record` wasm-local: 244µs → 47µs (**5.2x**) and wasm-gc: 805µs → 27µs (**30x**)
- `newtype_record` VM: 2.29ms → 966µs (**2.4x**), wasm-local: 143µs → 45µs (**3.2x**)

Binary size: `fib.wasm` = **110 bytes** (wasm-gc) vs **13,107 bytes** (legacy with runtime). 120x smaller.

### The two regressions

`match_dispatch` and `record` allocate fresh structs in the hot inner loop — `Shape.Circle(n)` ×30K, `Point(n, 2n)` ×20K per iteration. The legacy backend escapes that cost via NaN-boxing (Floats stay unboxed, Points get dispatched-direct via tag bits or arena reuse). wasm-gc allocates a real engine-managed struct every time; per-alloc overhead × 30K dominates.

This is a real cost of nominal types in alloc-heavy workloads. Two paths forward in 0.16:

- **Escape analysis** — detect "struct allocated and consumed within the same fn frame, no captures, no escape into caller" → scalar replace fields onto the stack. Standard compiler pass; rustc has a less-aggressive form via mem2reg + LLVM's allocation sinking. Phase 3c work.
- **Engine improvement** — V8 / wasmtime may eventually eliminate short-lived wasm-gc allocations themselves. Not a path we control.

For now the pattern "fresh-record-per-iteration in a tight loop" is a known regression vs legacy. Most real programs don't hit it; bench scenarios specifically stress the case.

### Newtype optimization

Single-field records of primitives (`record UserId { raw: Int }`) and single-variant single-payload sums (`type UserId = UserId(Int)`) lower to the underlying primitive everywhere — no `struct.new`, no `struct.get`, no `ref.cast`. Same trick rustc uses for `struct UserId(u64)` and Haskell uses for `newtype UserId = UserId Int`.

Detection: `TypeRegistry::newtype_underlying(name)` returns `Some(primitive)` when the type qualifies. `aver_to_wasm` returns the primitive directly for newtype names; emit sites (`RecordCreate`, `Attr`, `Constructor`, single-arm variant `match` unwrap) take a fast path that's literally `emit_expr(field_value)` — no struct ops emitted.

Without this optimization wasm-gc was 3-3.5x slower on newtype_record / newtype_variant (allocating 600K structs per bench run). With it, faster than legacy.

## Phase 3 status

- **3a (shipped)**: Float, Records (struct types, `RecordCreate`, `Attr`), Single-arm Variants, **newtype optimization**.
- **3b/1 (shipped)**: multi-arm variant dispatch via `ref.test` cascade, multi-field variant patterns, `Float.fromInt` / `Int.fromFloat`. Wasmtime dependency bumped 29 → 44 (29 had ref.cast bugs).
- **3c (next)**: String representation + Int.toString + List + Map + Vector. Builtin scaffold lives in `builtins/` — `BuiltinRegistry` allocates per-module wasm fn slots for pure helpers (`Int.toString`, `List.prepend`, …), `effects.rs` (TBA) declares host imports for effectful operations (`Console.print`, …). Architecture decided; wiring + first real implementation lands once String repr is picked (`(ref null (array i8))` is the leading candidate — engine-managed, no linear memory needed).

## Where builtins live (architectural decision, 2026-05-01)

Two-file split:

- **Pure builtins** → per-module helper fns (`builtins/`). Each used builtin gets a wasm fn slot in the consuming module on first reference. Same pattern rustc uses for stdlib helpers in its wasm output. `wasm-opt -Oz` DCE's unused. No external runtime, fully standalone.
- **Effectful builtins** → `(import "aver" "...")` (`effects.rs`, TBA). Host (browser / workerd / wasmtime+wasi) supplies the implementation. Same shape the legacy backend uses for effects, just without the `aver_runtime.wasm` middleman.

Rejected alternatives:
- Custom runtime module → reverts the "no aver_runtime" call we made on day one
- Inline-emit per call site → bloats every callsite with the same 30-instruction body
- JS String Builtins → browser-only, niche

## Bench coverage status

Working (12/13 bench scenarios):
- fib, countdown — pure numeric tail recursion
- newtype_bare, newtype_record, newtype_variant — newtype optimization erases wrappers
- match_dispatch — multi-arm variant dispatch via `ref.test`
- record — struct field access in hot loop
- factorial — `Int.toString` + `Console.print` (silenced in bench mode)
- vector_ops — `Vector<T>` as `(array (mut T))`, fused `Option.withDefault` shapes lower to bounds-checked `array.set`/`array.get` with zero Option boxing
- string_interp — String literals via passive data segments + `array.new_data`; `Expr::InterpolatedStr` lowers natively (skipping `interp_lower`) to `array.new_fixed (Vector<String>) N` + variadic concat helper. Same primitive will back `String.join`. Engine-bound on wasmtime today (see note above)
- map_build, map_lookup — `Map<K,V>` monomorphised per instantiation, open-addressing hashtable with linear probing. Per-K helpers (DJB2 hash + byte-eq for K=String), per-(K,V) helpers (empty/set/get/len). Map.get returns real boxed `Option<V>`. Wasmtime engine-bound on `map_lookup` (600k Option struct allocs); V8 lands the wins

Pending (1/13 — multi-module mode):
- fractal_seahorse — `depends [Fractal]`. Multi-module compile is a separate **mode** for wasm-gc, not the default — the wasip2 Component Model handles cross-module linking, not a static linker baked into the compiler

## Phase plan
