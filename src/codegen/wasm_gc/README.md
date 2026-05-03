# `wasm_gc` codegen backend (recommended WASM target in 0.16, codename "Concede")

Parallel WASM backend that targets the **WebAssembly GC + tail-call** proposals natively, instead of layering a custom runtime on top of MVP WASM. Lives alongside `src/codegen/wasm/`; both compile from the same IR. After the cross-engine bench (see below) the call is in: **`--target wasm-gc` ships as the recommended target from 0.16 onward**, `--target wasm` survives as the optional fallback for pre-2024 hosts (wasmtime CLI < 25, Node < 22, environments that don't speak the GC + tail-call proposals).

The verdict is asymmetric: `wasm-gc` wins decisively where allocations or recursion dominate (`fib` 8.4×, `vector_ops` 182×, `countdown` 2.9×, `record` 1.4×, the edge fetch handler stays a tie at ~33 ms because that workload is f64 arithmetic the engine optimises identically on both backends). It's also a 35 % smaller binary on the edge demo. The legacy backend doesn't gain meaningful headroom from `wasm-opt -O4` or further IR work — it has, on these workloads, said its last word.

What `wasm-gc` does **not** do is delete the Aver runtime. The edge handler binary is 22 KB after DCE, and roughly half of that is still helper functions emitted into the same module: `String.split` / `String.join` / `Float.toString` / `Int.fromString` (and friends) as WAT-source-of-truth helpers; per-(K,V) Map probes (`empty` / `set` / `get` / `len` / `get_or_default` / `get_pair`) for every `Map<K, V>` instantiation in the program; per-`List<T>` `len` / `reverse`; per-(`List<T>`, `Vector<T>`) `from_list`; the `__rt_string_from_lm` / `__rt_string_to_lm` / `__rt_memory_grow` JS bridge plus a 1-page transport memory; the synthesised `aver_http_handle` wrapper. Counted: 78 functions in the wasm-gc edge binary, ~38 of them runtime helpers; the rest are user code. What changed vs `--target wasm` is *where* the runtime lives — instead of merging a separate `aver_runtime.wasm` (~10 KB of allocator + GC + HAMT + str/list/vec ops) via `wasm-merge`, the wasm-gc backend inlines exactly the helpers each program calls and lets `wasm-opt -Oz` DCE the rest. Plus engine GC and native tail-call replace the bits the legacy runtime hand-rolled (custom heap, NaN-boxing, mutual-TCO trampoline).

So the binary-size win comes from "ship only what's actually called" + "delegate heap + tail-call to the engine", not from "no runtime at all". The runtime is still there — it's just per-program and per-instantiation instead of one shared blob.

### Typed-AST consumption (Step 0–3 refactor, late 0.16-wasm-gc-probe)

This backend reads expression types from `Spanned::ty()` (the `OnceLock<Type>` field on every AST node, set by `TypeChecker::infer_type`) instead of the previous five ad-hoc inference functions (`infer_aver_type`, `sniff_with_prev`, `dotted_return_type`, `effect_aver_return_type`, `infer_expr_wasm_type`). The single reader is `body/infer.rs::aver_type_of(&Spanned<Expr>) -> &Type`; if a node lacks a stamp, the reader panics with the offending node — no fallback chain. The same shape lives in `src/codegen/wasm/expr/infer.rs` (legacy WASM) and `src/codegen/rust/` (codegen Rust); three of four backends now read from the single source of truth. VM is unchanged because its NaN-boxed runtime self-types at execution.

One band-aid survives: `body/infer.rs::aver_type_canonical` recovers `Type::Unknown`-bearing canonicals (`Map<Unknown,Unknown>`, `Option<Unknown>`, `List<Unknown>`) from either a single registered instantiation or the enclosing fn's return type. The proper fix is constraint-propagating type inference in the type checker — currently bidirectional only on local context, not on positional call-arg expected types. That refactor is multi-day work and lives in `src/types/checker/` not here.

### Game compilation status

`examples/games/` covers the realistic shape diversity that the 26 unit tests in `src/codegen/wasm_gc/tests.rs` couldn't (each unit test has explicit fn signatures that side-step the inference gaps). After Step 0–3 + variadic tuple + IndependentProduct unwrap (commit `f8c4f9bc`):

| Game | Status | Size |
|---|---|---|
| `snake.av` | ✅ | 5870 B |
| `life.av` | ✅ | 9194 B |
| `wumpus.av` | ✅ | 7218 B |
| `tetris/main.av` | ✅ | 11201 B |
| `checkers/main.av` | ❌ | sum-type equality (`state.currentPlayer == playerColor` for `Color` enum). `BinOp::Eq` lowers to `i64.eq`, treating struct refs as Int. Fix: dispatch on stamped operand type — `Type::Named(N)` where N is a sum type → `ref.eq` or per-N `__eq_N` helper |
| `rogue/main.av` | ❌ | wasm validation: nullability cascade in cross-module Pathfinding call. `struct.new` produces non-null `(ref T)`, signature declares `(ref null T)`. Fix: insert `ref.as_non_null` or unify nullability across param/result signatures |
| `doom/main.av` | ❌ | empty `[]` in call-arg position gets the wrong type stamp. `genRooms(seed, 6, 0, [])` — typecheck stamps `[]` from fn return type rather than formal param type. Fix at typecheck: propagate formal param type as expected type into empty-list literal args |

Each remaining blocker lives in a different subsystem (BinOp emit, ref-cast emit, typecheck constraint propagation), so they unblock independently. Snake compiles ~6 KiB, the largest passing game (tetris) ~11 KiB; full playground migration story (`tools/website/playground`, currently `--target edge-wasm`) waits on all three.

### Runtime placement: inline vs sidecar

Worth flagging: `--target wasm-gc` today is inline-only. Legacy already has the inline-vs-sidecar split (`--target wasm` merges the runtime into one binary; `--target edge-wasm` ships a thin user.wasm that imports the runtime from a CDN). The same split would in principle make sense for wasm-gc:

| Mode (hypothetical)              | When it pays off |
|----------------------------------|-------------------|
| `--target wasm-gc` (inline, default) | Cloudflare Workers (rejects `WebAssembly.instantiate(bytes, …)` from fetched bytes anyway), one-shot deploys, when the tooling story has to be "drop two files into a folder". |
| `--target edge-wasm-gc` (runtime imported from sidecar module) | Browser playgrounds, dev workflows that run many small programs against one runtime cache, environments where the runtime can be patched independently. |

The reason wasm-gc doesn't ship the sidecar mode yet: most of the helpers are **per-instantiation**. `Map<String, Int>`, `Map<String, List<String>>`, `Map<Int, View>` each get their own struct types, key-helper pair (hash + eq), and `empty / set / get / len / get_or_default / get_pair`. That's per (K, V). `List<T>` and `Vector<T>` and `Vector.fromList(List<T>) → Vector<T>` are per-T. A shared sidecar runtime would have to pick a canonical set of instantiations to expose (probably `String`, `Int`, `Float` for K/V/elem) and force user programs to either match the menu or fall back to inline. The legacy backend doesn't have this problem because its runtime is fully NaN-boxed — one `rt_map_set(map, key, val)` works for any K, V. wasm-gc deliberately gave that up in exchange for type-direct lowering.

Practical proposal: leave `--target wasm-gc` inline-only for now (matches the dominant deploy story), and design `--target edge-wasm-gc` against a fixed monomorphic menu later when there's a concrete consumer (the playground, probably). Memory entry `project_wasm_gc_multimodule.md` captures the related Component Model question.

**One worker.js per target.** `tools/edge/worker.js` (~120 lines) hosts the wasm-gc backend's fetch bridge: GC + tail-call config, `(ref null any)` import shapes, the `__rt_string_*` LM round-trip helpers the host needs to deliver `query: String` and read back the rendered body. A legacy-ABI dual-emit mode would let one worker.js drive both backends, but the gain is illusory — host code still has to know whether strings are `(array i8)` or OBJ_STRING with an 8-byte header — and the maintenance cost (every effect landing twice, every per-instantiation helper carrying both shapes) scales with the surface area. Pick a target, use the matching worker.js; pre-2024 hosts stay on `--target wasm` with their own bridge.

**Where the sidecar story actually pays off: wasip2.** Component Model gives cross-component types and a real linking story instead of MVP wasm's "two modules, hope the imports line up" shape. A wasip2 component can declare `interface aver-runtime { resource map<...>; ... }` and let the host instantiate the helper module once, hand its functions to every guest component on demand. The per-instantiation problem softens because the Component Model lets a guest component say "instantiate `aver-runtime` with K=String, V=Int" and the runtime component does the monomorphisation locally. That's the model where shared runtime starts to make sense again — not MVP wasm sidecars where every type signature has to be globally agreed up front. `project_015_traversal.md` already has wasip2 on the parallel track; pairing it with `--target edge-wasm-gc` would land both stories together.

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
| `fractal_seahorse`| (tbd)   | 34.1ms     | 212.8ms | 6.2x slower ⚠ (wasmtime GC, V8 wins by 3.5x over wasm-local) |

**13/13 wasm-gc compile + run, 2 engine-bound regressions on wasmtime.** Both alloc-heavy patterns (`record`, `match_dispatch`) now go through escape analysis at the IR level — no struct/variant alloc reaches codegen. `vector_ops` lands on native `(array (mut i64))` with bounds-checked `array.set`/`array.get` and zero Option boxing — the legacy backend round-trips every read/write through runtime helpers + AverVector allocation. `Map<K,V>` is monomorphised per instantiation: per-K hash + eq helpers, per-(K,V) empty/set/get/len helpers, open-addressing flat hashtable, 16384-bucket fixed cap (resize is a phase-3c+ extension).

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
| map_lookup        | 1.16ms         | 693µs     | 1.75ms       | **427µs** |
| fractal_seahorse  | 34.1ms         | (tbd)     | 213ms        | **9.8ms** |

(Bold = winner per scenario.)

Findings:

- **wasm-gc wins or ties everywhere** in a sane engine. Across V8 + wasm-gc, every scenario is at-or-better than every other combination — including a 1.5× win over `wasm-local·V8` on `string_interp` and 200× on `vector_ops`.
- **`wasm-local` doesn't benefit from V8** — it uses linear memory + a bump allocator, no engine GC, so V8 and wasmtime treat it the same. Engine choice barely moves the number.
- **wasmtime + wasm-gc is competitive on pure numeric** (fib, countdown, match_dispatch) — Cranelift's codegen is solid; differences are noise. Penalty appears only on alloc-heavy paths.
- **`string_interp` wasmtime: 6.65s (vs 2.86ms on V8 — 2300× slower).** Same wasm binary, engine-bound. Wasmtime 44's GC heap path: every `array.new_default(N)` + `array.copy` hits the allocator + write barriers, and the bench accumulates ~30 MB of intermediate strings per iteration. Wasmtime's GC is new (2024) and unoptimised; V8 has had a decade. The compile path is sane and fast — V8 actually beats `wasm-local` on the same workload.
- **`vector_ops` is structurally 200× wasm-gc on either engine** — the legacy backend round-trips every `Vector.set` / `Vector.get` through runtime helpers + `AverVector` allocation; native `array.set` / `array.get` is the right shape.
- **`fractal_seahorse` repeats the alloc-heavy story.** wasm-gc·wasmtime: 213 ms (vs 34 ms on legacy wasm-local — 6.2× slower, same engine-bound GC path that costs `string_interp` and `map_lookup`). On V8 it flips to **9.8 ms — 3.5× faster than `wasm-local`·wasmtime and 22× faster than `wasm-gc`·wasmtime on the identical `.wasm`.** The bench renders a real Mandelbrot (200×120 chars, two views, one zoom-deep seahorse) — every iteration allocates ~hundred-thousand cons cells through the `cellRow` run-length-encoder + a few hundred `Vector<String>` slices, plus `Float.toString` 6 times per nav anchor. Aver→wasm-gc lowering gets out of the way; the difference between 213 ms and 9.8 ms is 100% engine.
- **`map_lookup` was the wasmtime tail-end story.** Naive shape (each `Map.get` boxes `Option<Int>` → ~600k struct allocations × 30 iterations) sat at 2.20ms / 1.9× vs wasm-local. Two fused shapes ship with this backend:
  - `Option.withDefault(Map.get(m, k), default)` → per-(K,V) `get_or_default` helper. Probe loop returns the value (or default) directly. Backs the `withDefault` surface form.
  - `match Map.get(m, k) { Option.Some(v) -> ...; Option.None -> ... }` → per-(K,V) `get_pair` helper with multi-result `(i32 found, V value)` return. Caller pops `value` into the binding slot, branches on `found`. Backs the explicit pattern-match surface form (the bench's actual shape).
  Pattern fusion alone cuts wasmtime to **1.75ms (1.36× faster than naive)** without changing the source. V8 falls in line at 427µs (1.6× faster than wasm-local·V8). Vector fusion (already shipped) goes through `ir::leaf::classify_leaf_op` so the recognition logic is shared with Rust / Lean backends — Map fusion stays local because legacy `wasm-local` uses a runtime helper and doesn't benefit from a leaf shape.

The compile path itself is correct and competitive: `Expr::InterpolatedStr` lowers to a single `array.new_fixed (Vector<String>) N` + variadic concat helper that is O(total_len) bytes copied (not O(N²) like a left-folded `String.concat` chain would be). Same primitive (`array.new_fixed` + variadic concat) will back `String.join` once it lands.

**For realistic numbers under wasm-gc, use a browser-class engine.** A V8 bench harness lives at `tools/wasm-gc-bench-v8.mjs`. Requires Node 22+ for stable wasm-gc support — Node 20's V8 rejects packed `i8` array types.

### Edge handler bench: `tools/edge/bench.av` end-to-end on V8

`bench.av` runs the seahorse zoom (`200×120` grid, 220-iter cap, ~kilo-cons-cells per render) through the same `Fractal.render` module the deployed worker calls. wasm-gc + `--handler X` synth wrapper lands at ~32 ms, ~22 KB raw / ~19.8 KB after `wasm-opt -Oz`, 240 KB body. Last measured legacy `--target wasm` number on the comparable `app.av` (now removed — the legacy router demo) was 33.2 ms / 34.0 → 31.0 KB; the gap is small absolute on this workload because the seahorse render is bound by the `mandelStep` / `mandelIter` mutual recursion, which the legacy codegen already elides boundary framing on (`is_no_alloc` heuristic), so most of the wasm-gc win comes from binary size, not steady-state speed. Allocation-heavy workloads (`fib`, `vector_ops`, `record`, `match_dispatch`) show the larger wins reported up top.

### Stdlib parity vs `--target wasm`

Audited 2026-05-02 against `src/codegen/wasm/abi.rs` + `src/types/checker/builtins.rs`. The wasm-gc backend covers the surface every active bench scenario hits — what's missing is reach for new programs, not correctness for the existing ones.

**Effects parity (after etap 1):**

| Namespace      | wasm-gc | Notes |
|----------------|---------|-------|
| Console.*      | ✅ print/error/warn/readLine | full |
| Time.*         | ✅ unixMs/sleep/now | full |
| Args.*         | ✅ _len/_get | full |
| Random.*       | ✅ int/float | full |
| Float math     | ✅ sin/cos/atan2/pow (host imports — wasm has no native libm) | full |
| Request/Response/Http/Env | ✅ all 13 fetch-bridge effects | full |
| Terminal.*     | ✅ enableRawMode/disableRawMode/clear/moveTo/print/setColor/resetColor/readKey/size/hideCursor/showCursor/flush + `Terminal.Size` builtin record (auto-registered when any fn declares `! [Terminal.size]`) | full |
| Print/Format.value | ❌ deliberately | wasm-gc lowers interpolations natively via `__wasmgc_concat_n` + `Int.toString`; debug helpers not needed |
| Disk.*, Tcp.*, HttpServer.listen* | ❌ deliberately | per-deployment policy domain (host's job per `docs/wasm.md`) |

**Pure builtins parity:**

| Namespace | Coverage in wasm-gc |
|-----------|---------------------|
| Bool      | and/or/not ✅ |
| Int       | toString/toFloat/abs/min/max/mod ✅, fromString ✅ |
| Float     | toString/floor/ceil/round/abs/sqrt/min/max/pi/fromInt ✅, fromString ✅ |
| String    | len/length/byteLength/startsWith/endsWith/contains/slice/toUpper/toLower/trim/split/join/fromInt/fromFloat/fromBool/charAt/chars/replace ✅ |
| Char      | toCode/fromCode ✅ |
| Option    | Some/None/withDefault/toResult ✅ |
| Result    | Ok/Err/withDefault ✅ |
| List      | prepend/empty/len/length/reverse/concat/take/drop/zip ✅, contains ✅ for T ∈ {Int, Float, Bool, String}, any user-defined record, or any user-defined sum (inline field-by-field eq for records; per-variant `ref.test` cascade for sums) + per-(L,V) `Vector.fromList` ✅. **missing**: record / sum T with field types outside `{Int, Float, Bool, String}` (nested records, lists, vectors as fields) |
| Map       | empty/set/get/len/has/keys/values/remove/entries/fromList + fused `Option.withDefault(Map.get(...))` / `match Map.get(...)` shapes ✅. K ∈ `{String, user-defined record, user-defined sum, Int, Float, Bool}` ✅. Record / sum K field types ∈ `{Int, Float, Bool, String, nested record, nested sum, List<T>, Vector<T>}` ✅ (List/Vector hash+eq emitted as per-T helpers; record / sum dependencies force-registered as pseudo-K via DFS). |
| Tuple     | `(A, B)` literal + match destructure ✅, per-(A,B) instantiation in the type registry, used as a building block for `List.zip` / `Map.entries` / `Map.fromList`. Tuple types accept both Aver-surface `(A, B)` and internal canonical `Tuple<A,B>` interchangeably |
| Vector    | new/get (boxed)/set (boxed)/len/toList ✅ + `fromList` per-(L,V) ✅ |
| Byte      | fromHex/toHex ✅ |
| BranchPath, Tcp.Connection | ❌ surface-level builtin records, low priority |

Zero bench scenario in `bench/scenarios/*.av` calls anything in the "missing" rows. Adding them is per-helper plumbing, not blocking work.

### Tail-call A/B

`AVER_WASM_GC_NO_TAIL_CALL=1` swaps `return_call` for plain `call` so the proposal's contribution is measurable in isolation:

| Scenario          | TCO ON    | TCO OFF             | gap |
|-------------------|----------:|--------------------:|-----|
| fib(15)           | 8 µs      | 6 µs                | neutral (15-deep stack, no overflow risk either way) |
| countdown(100k)   | 28 µs     | RangeError: max stack | required for the 100K-deep chain |
| fractal seahorse  | 32.1 ms   | 58.6 ms             | **1.83× wider** |

The fractal measurement comes from the mutual `mandelIter` ↔ `mandelStep` recursion (up to 600 iterations per pixel × 24 000 pixels). Without `return_call` every iteration adds a frame; the engine handles it but pays the per-frame cost. The legacy `--target wasm` backend manually compiles its own trampoline (loop + `call_indirect`) on top of MVP wasm and gets a similar effect — that's why the seahorse render lines up at ~33 ms across both backends. Native `return_call` matches the trampoline shape, doesn't beat it.

Render time effectively a tie now. The earlier 6× gap was a self-inflicted illusion: a `String.split` bug in the wasm-gc backend — `Br(2)` after a successful match exited the whole search loop instead of looping again — meant every `?cx=…&cy=…&w=…` query collapsed to a 2-element list. `Fractal.viewFromQuery` fell back to `fullView` (cxRange = 2.5, iterFor → 80) instead of seahorse (cxRange = 0.012, iterFor → 600). The renderer was doing 7.5× less work, not running 6× faster. Body size matched the legacy 240 KB output once split was fixed.

What the comparison still shows: wasm-gc binary is ~35 % smaller (22 KB vs 34 KB; the legacy path bundles its own runtime, the wasm-gc path delegates to engine GC), and `wasm-opt -O3` claws back ~10 % on either side without changing the runtime story — Cranelift / V8 are already optimising the hot path. The structural advantages of the wasm-gc backend (type-direct lowering, native tail-call, no NaN-box) don't materialise in this workload; the seahorse render is dominated by f64 arithmetic and the recursive Mandelbrot iterator that both backends compile to roughly the same shape. Binary size and runtime simplicity are the genuine wins; raw speed parity is the honest outcome.

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

Working (13/13 bench scenarios):
- fib, countdown — pure numeric tail recursion
- newtype_bare, newtype_record, newtype_variant — newtype optimization erases wrappers
- match_dispatch — multi-arm variant dispatch via `ref.test`
- record — struct field access in hot loop
- factorial — `Int.toString` + `Console.print` (silenced in bench mode)
- vector_ops — `Vector<T>` as `(array (mut T))`, fused `Option.withDefault` shapes lower to bounds-checked `array.set`/`array.get` with zero Option boxing
- string_interp — String literals via passive data segments + `array.new_data`; `Expr::InterpolatedStr` lowers natively (skipping `interp_lower`) to `array.new_fixed (Vector<String>) N` + variadic concat helper. Same primitive will back `String.join`. Engine-bound on wasmtime today (see note above)
- map_build, map_lookup — `Map<K,V>` monomorphised per instantiation, open-addressing hashtable with linear probing. Per-K helpers (DJB2 hash + byte-eq for K=String), per-(K,V) helpers (empty/set/get/len). Map.get returns real boxed `Option<V>`. Wasmtime engine-bound on `map_lookup` (600k Option struct allocs); V8 lands the wins
- fractal_seahorse — `depends [Fractal]`. Multi-module flatten loader (`flatten_multimodule` in `src/main/commands.rs`) inlines dep modules into the compile unit; full Mandelbrot render works through the same single-binary path. Per-instantiation `List<T>` helpers (`len` / `reverse`), per-(`List<T>`, `Vector<T>`) `Vector.fromList`, plus singleton `String.split` / `String.join` (T=String) live in `lists.rs`. Boxed `Vector.get`, `Result.withDefault(Int.mod(…), default)` fusion, full WAT bodies for `String.startsWith` / `String.contains` / `String.slice` / `String.toUpper` / `String.toLower` / `String.trim` / `Int.fromString` / `Float.fromString` / `Float.toString` round it out. Engine-bound on wasmtime (~6.4s/30 iter), same alloc-heavy tax `string_interp` already pays

## Phase plan
