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

After phase 3a (Float + Records + Variants + newtype opt), 3b/1 (multi-arm dispatch), and **3c (String repr + Int.toString + String.len + Console.print as host import)**:

| Scenario          | VM      | wasm-local | wasm-gc | wasm-gc vs legacy |
|-------------------|---------|------------|---------|-------------------|
| `fib(15)`         | 109µs   | 41µs       | 5µs     | **8.2x faster** |
| `countdown(100k)` | 814µs   | 44µs       | 17µs    | **2.6x faster** |
| `newtype_bare`    | 969µs   | 42µs       | 19µs    | **2.2x faster** |
| `newtype_record`  | 2.29ms  | 143µs      | 51µs    | **2.8x faster** |
| `newtype_variant` | 2.28ms  | 201µs      | 48µs    | **4.2x faster** |
| `match_dispatch`  | 7.37ms  | 356µs      | 1.54ms  | 4.3x SLOWER ⚠️ |
| `record`          | 3.98ms  | 244µs      | 805µs   | 3.3x SLOWER ⚠️ |
| `factorial`       | 20µs    | 38µs       | 19µs    | **2.0x faster** |

6/8 — wasm-gc beats legacy by 2.0× to 8.2×. 2/8 — alloc-heavy hot-loop regressions persist (same scenarios as before — engine GC overhead vs NaN-boxing for short-lived structs).

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

Working (8/13 bench scenarios):
- fib, countdown — pure numeric tail recursion
- newtype_bare, newtype_record, newtype_variant — newtype optimization erases wrappers
- match_dispatch — multi-arm variant dispatch via `ref.test`
- record — struct field access in hot loop
- factorial — `Int.toString` + `Console.print` (silenced in bench mode)

Pending (5/13 — phase 3c continues):
- string_interp, fractal_seahorse — String literals + `__buf_*` interp lowering
- map_build, map_lookup — `Map<K,V>` repr (flat hashtable struct)
- vector_ops — `Vector<T>` (wasm `(array T)`)

## Phase plan
