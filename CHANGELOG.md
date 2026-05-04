# Changelog

All notable changes to Aver are documented here. Starting with 0.10.0, minor releases get a codename — short, evocative, and it tells you what the release was really about.

## Unreleased

## 0.16 "Anneal" (unreleased)

> _Heat-treat the internals — every backend now agrees what `Console.print` is, what's a type, and that engine GC beats hand-rolling._

### Breaking changes
- **`Console.print` / `Console.error` / `Console.warn` / `Terminal.print` / `Terminal.setColor` now take `String`.** The previous polymorphic signature (a `Printable` type variable that accepted any value and let the runtime stringify per-type) is gone. Stringification is the caller's job — use interpolation `"{x}"` for primitives, write a per-type render helper for compound shapes. Programs that called `Console.print(record)` / `Console.print(some_list)` will fail typecheck with `Argument 1 of 'Console.print': expected String, got <Type>` and the message points at the call site to migrate. Every backend (VM, legacy wasm, wasm-gc, Rust, self-host) loses its per-type format dispatch; replay/record stores plain string bytes per call; the effect ABI becomes trivial across hosts.

### Added
- **`--target wasm-gc`** — native WebAssembly GC + tail-call output, the recommended WASM target. No NaN-boxing, no boundary GC framing, no inline runtime. Per-instantiation monomorphisation for `Vector<T>`, `List<T>`, `Map<K, V>`, `Option<T>`, `Result<T, E>`, `Tuple<A, B>` (incl. nested paren tuples like `(Int, (Int, Int))`). Full Aver type parity: tuples, cross-collection nesting, sum-type and record map keys, all 12 `Terminal.*` effects, multi-binding variant patterns, `?` / `?!` / Result-Unit / Tuple-Unit shapes, generic constructors driven by bidirectional inference. Wasm-gc wins V8 microbenches on alloc / recursion / collection workloads (vector_ops 269×, map_build 5.67×, record_access 3.37×); `--optimize size` wins 19-32 % across the example games.
- **`aver run --wasm-gc`** — embedded wasmtime executor with engine GC + tail calls. Full effect surface wired against `aver_rt::*`: Args, Console (incl. `readLine`), Time, Random, Float math, Terminal, Disk (read/write/append/exists/delete/listDir/makeDir), Env, Tcp (connect/writeLine/readLine/close/send/ping), Http (get/head/delete/post/put/patch). Multi-module entries auto-resolve their module root from the entry file's `depends [...]`, so `aver run --wasm-gc projects/payment_ops/main.av` works without `cd` or `--module-root`. Output matches the VM byte-for-byte modulo time/randomness on every audited core / data / games / services / apps / modules / playground sample.
- **Browser playground migrated to `--target wasm-gc`.** Engine GC replaces the `aver_runtime.wasm` sidecar; binaries are self-contained. Modern-browser baseline (Chrome 119+ / Firefox 120+ / Safari 18.2+).
- **`--target wasm-gc --optimize size`** — wasm-opt pipeline passes `--enable-gc --enable-reference-types`, factory exports + LM transport survive `wasm-metadce`, per-program binaries shrink 19-32 %.
- **`--preset cloudflare` migrated to wasm-gc.** Expands to `--target wasm-gc --pack cloudflare` and requires `--handler <fn>`. Smaller wasm + smaller `worker.js` than the legacy `--target wasm` bundle, no `wasm-merge` dependency.

### Changed
- **Soundier internals across the type system.** `Type::Unknown` / `Type::Any` removed from the static type enum: generic builtin positions use named `Type::Var(K|V|T|E)` resolved per call site by `match_expected_type` substitution; error recovery uses `Type::Invalid` (never compatible with concrete types). Bidirectional inference drives expected types into generic constructors and collection literals — `Map.empty`, `Option.None`, `Result.Ok/Err`, empty list literals all type without backend recovery shims, including in tail-call argument position. `Char.fromCode` accepts the full Unicode range (Doom Braille, etc.).
- **Resolver is arm-scoped.** Pattern bindings live per arm (`MatchArm.binding_slots`) instead of a function-flat name → slot map. Two arms can share a binding name with different field types — `recordedAt(event: TaskEvent)` reuses `at` across `TaskCreated.at: String` / `CommentAdded` (where `at` projects from a record), `serializeTaskEvent` reuses `deadline` across `TaskCreated.deadline: Option<String>` and `DeadlineSet.deadline: String` — and each arm's slot is allocated independently. The resolver runs as a single-pass walk with a scope stack so ident lookup respects shadowing.
- **Slot types lifted into IR** as `FnResolution.local_slot_types` — every backend that needs typed locals consumes the same table instead of re-walking patterns. Sibling sumtypes sharing a bare variant name (e.g. `Query.ProviderSummary` vs `QueryOutput.ProviderSummary` in payment_ops) are disambiguated by `(parent, bare)` keys throughout resolver and wasm-gc registry. Multi-arm tuple-of-Constructors matches and chained-Attr value access (`Domain.Types.TaskStatus.Blocked`) flatten through one shared rewrite path used by both VM and wasm-gc.

### Coverage
- Of the 71 single-file or entry-point examples + project mains, 68 run identically to the VM under `aver run --wasm-gc` (every `examples/{core, data, games, formal, wasm, services, apps, modules}` and `projects/*` plus 13 of 14 playground samples). The remaining 3 are intentionally bad code that *show* compile errors (`examples/diagnostics/{lint_demo, test_errors}.av`, `tools/website/playground/sources/examples/effect-violation.av`).

### Removed
- **Per-version runtime artifacts (`tools/website/runtime/`, `averlang.dev/runtime/`).** Wasm-gc binaries are self-contained — there's no shared `aver_runtime.wasm` to fetch and cache, so the versioned CDN tree, the `latest/` pointer, and the `release.py` builder that emitted them all go. The legacy `--target wasm` bundle inlines its own runtime via `wasm-merge` and doesn't rely on the CDN either.
- **`--bridge` rejected under `--target wasm-gc`.** The legacy `--bridge fetch | wasip1 | none` axis is bound to the legacy NaN-boxed backend. Mixing it with wasm-gc was previously silently ignored — now the compiler errors with a hint pointing at the correct shape: `--handler <fn>` (and `--preset cloudflare --handler <fn>`) for HTTP, the planned `--target wasip2` for standalone-WASI deployments. wasm-gc skips preview 1 by design — porting the legacy `aver_to_wasi.wasm` shim to GC string types would re-implement an ABI we want to leave behind.

### Roadmap
- **`--target wasip2`** (Component Model output, `wasi:http/proxy` + `wasi:filesystem` + `wasi:sockets`) is the modern wasm-gc companion for standalone-runtime deployments. Lands in 0.17. Once shipped, `Http.*` / `HttpServer.listen` / `Disk.*` / `Tcp.*` all become ✅ on the standalone-WASI side instead of stubbing.

## 0.15.2 "Traversal" — observability lands, regression gate closes (unreleased)

> _Every pipeline pass exposes its decisions through one typed shape; bench reports gain the metrics that make regressions catchable; CI gates them per-host without per-runner branching._

### Added
- **`aver compile --explain-passes [--json]`** — typed per-pass diagnostic report (TCO conversions, interpolations lowered, fusion sites + sinks fired, slots resolved, alloc/recursion facts). JSON shape is one `{stage, data}` block per pass with stage-specific fields; `schema_version: 1` pins the contract for CI gates (`tests/explain_passes_spec.rs`).
- **`aver bench` populates `compiler_visible_allocs`** (IR-level via `NeutralAllocPolicy`, same across all three targets) and **`response_bytes`** (`vm`: rendered return value via `aver_display`; `wasm-local`: `fd_write` iovec sum; `rust`: subprocess stdout). `--compare` gates both as exact-match alongside p50/p95.
- **`aver bench --baseline-dir DIR`** auto-picks `<host.os>-<host.arch>-<backend.name>.json`; silent skip when no match. Directory mode supports `--save-baseline` (NDJSON, same shape `--baseline-dir` reads) — `bench/baselines/macos-aarch64-vm.json` ships as the reference.
- **GitHub Actions `Bench Gate` job** runs `aver bench bench/scenarios/ --target=vm --baseline-dir bench/baselines/ --fail-on-regression` on every PR; results upload as 30-day artifact.

### Changed
- **`PipelineResult.buffer_build`** is now a typed `BufferBuildPassReport` (sinks, synthesized fns, per-sink rewrite counts) instead of an opaque `(usize, usize)` tuple.
- **`aver check`** no longer prints the `↻ N buffer-build sink(s) […]` summary — same data with richer detail is now in `aver compile --explain-passes`.

## 0.15.1 "Traversal" — pipeline foundation + observability (2026-05-01)

> _The compiler grew a real pipeline. Every IR transform is a named ordered stage, every consumer reads derived facts from one place, every backend's relationship to the IR is a documented contract._

### Added
- **Ordered 7-stage pipeline** (`tco → typecheck → interp_lower → buffer_build → resolve → last_use → analyze`) as the single source of truth, with per-stage public functions and per-stage `PipelineConfig` flags.
- **`Analyze` stage** centralises five ad-hoc IR analyses (alloc info, mutual-TCO membership, recursive fns, callsite counts, body classification) into one `PipelineResult.analysis` codegen reads from.
- **`LastUse` stage** split out of the resolver — `<name:last>` markers in IR dump show which slots VM/Rust/WASM MOVE instead of COPY.
- **`aver compile --emit-ir-after=PASS`** prints the IR snapshot after any pipeline stage; `diff -u` between two stages shows exactly what each pass rewrote.
- **`aver bench` scenario harness** with three targets (`vm` / `wasm-local` / `rust`), TOML manifests, NDJSON for batch runs, `--save-baseline` / `--compare` / `--fail-on-regression`, and `backend` + `host` identity blocks in every report. Thirteen scenarios shared with `cargo bench` via `include_str!`. See [docs/bench.md](docs/bench.md).

### Fixed
- **Proof export traversal leak.** Dep modules ignored the proof-export flag and ran deforestation anyway, leaking synthesized `<fn>__buffered` variants into Lean/Dafny output.
- **Playground bypassed deforestation.** Browser-compiled WASM/Rust artifacts were measurably slower than equivalent CLI compiles. All playground compile / replay / record paths now use the canonical pipeline.

### Changed
- **`PipelineConfig` is per-stage booleans.** No bundled `apply_traversal_lowering` knob; `stop_after` removed. Caller-level proof-vs-runtime distinction translates to per-stage flags at the CLI boundary.
- **`pipeline::*` is the single entry point** for IR transforms and analyses — direct calls to `tco::transform_program` / `resolver::resolve_program` / `run_type_check_full` etc. are gone from production and tests.
- **Codegen consumers read derived facts from `PipelineResult.analysis` / `CodegenContext`** instead of recomputing. Aver's module DAG makes per-module unions sound (cross-module SCCs are mathematically impossible — pinned in `src/ir/analyze.rs` doc and memory).
- **VM compile API consolidated.** All three entry points take `analysis: Option<&AnalysisResult>` as a peer parameter; no `_and_analysis` variant.

### Pipeline contract (new)
- **WASM and Rust codegen no longer handle `Expr::InterpolatedStr`** — `interp_lower` is now a mandatory predecessor stage. ~100 lines of dead code + helper enums deleted, replaced with `unreachable!()` + contract comments.
- **VM keeps its `compile_interpolated_str`** — the REPL is the only legitimate consumer of pre-lower IR.

## 0.15.0 "Traversal" (2026-04-30)

> _The compiler now eliminates intermediate list traversals it can prove are consumed once — `String.join` builders, interpolation chains, and external-reverse pipelines fuse to direct buffer writes across all three execution backends._

### Added
- **Buffer-build deforestation across VM, WASM, and Rust.** `String.join(<recursive_prepend_builder>(args, []), sep)` no longer materialises the intermediate `List<String>`. The IR-level pass detects the canonical sink shape (`match cond { true -> List.reverse(acc); false -> recurse(... List.prepend(elem, acc)) }`), synthesizes a `<sink>__buffered` variant that threads a mutable `Buffer` through the tail-call chain, and rewrites matching call sites to `__buf_finalize(<sink>__buffered(args.., __buf_new(8192), sep))`. Each backend then lowers the four `__buf_*` intrinsics: VM dispatches to dedicated bytecode opcodes (`BUFFER_NEW`/`APPEND_STR`/`APPEND_SEP_UNLESS_FIRST`/`FINALIZE`) backed by a `Vec<Option<String>>` pool on the VM struct; Rust uses `String::with_capacity` + `push_str` (with `Buffer = String` aliased in `aver-rt`); WASM uses an `OBJ_BUFFER` heap object and `rt_buffer_*` runtime helpers. Local benches: 8× speedup on Rust (`buildLines(500_000, []) → String.join`), 2.7× on VM, 24% on WASM under workerd.
- **String interpolation lowers through the same buffer pipeline.** `"a${x}b${y}c"` no longer chains `str_concat` calls — every interpolation goes through a new IR pass (`src/ir/interp_lower.rs`) that desugars to `__buf_finalize(__buf_append(... __buf_new(N), __to_str(x)))`. Coercion of non-string parts uses a new `__to_str` intrinsic (VM: `CONCAT` against empty, Rust: `aver_rt::aver_display`, WASM: existing `emit_value_to_str` host bridge). Universal speedup — every Aver program with f-strings benefits, not just the niche of recursive `String.join` builders. Old O(N²) chained-concat shape (each step allocates a string of cumulative length) becomes O(N + total_len) buffer-write.
- **External-reverse builder shape.** The buffer-build detector now also matches sinks shaped `match list { [] -> acc; [head, ..rest] -> recurse(rest, List.prepend(elem, acc)) }` — common in payment_ops / workflow_engine codebases under the `*Into` naming convention (`serializeEntriesInto`, `filterSubjectInto`, `renderEventItemsInto`). Call sites spelled `String.join(List.reverse(<sink>(args, [])), sep)` fuse against these. Per-shape kind alignment + acc-position empty-list precondition checked the same way in detection and rewrite, so `aver check` can't over-report sites the rewrite would refuse.
- **Antipattern lints (`aver check` traversal warnings).** Three patterns the deforestation pass deliberately *doesn't* fuse get surfaced as warnings instead — the fuse-vs-warn split says: silently fuse only when Aver has no idiomatic alternative; warn when it does. Flagged: `Vector.fromList(<sink>(args, []))` (suggest `Vector.new(N, default)` + `Option.withDefault(Vector.set(v, i, x), v)` owned-mutate fast path), `Map.fromList(<sink>(args, []))` (suggest `Map.empty()` + `Map.set` chain), and standalone `List.reverse(<sink>(args, []))` whose result isn't fed straight into a `String.join` (the fusion path covers that case; the lint catches sites where the wrapper is something else like `renderLines` and fusion can't chain through). Acc-position-empty-list precondition checked the same way the rewrite checks it, so the warning never offers broken advice.

### Fixed
- **Dead HAMT pointer walk in WASM boundary GC.** The HAMT runtime was replaced by the flat `OBJ_MAP` (kind=12) earlier, but the kind=13/15 pointer-walks for `HAMT_NODE`/`HAMT_COLLISION` stayed in `collect_end` and `retain_i32`. They never fired on main because no live object used those kinds — but Phase 2c reuses kind=13 for `OBJ_BUFFER`, and the stale walk was treating buffer payload bytes as inner pointers, calling `rt_retain_i32`/`rt_rebase_i32` on garbage. On wasmtime the pseudo-pointers happened to land outside the collect range and were silently passed through; on Cloudflare Workers v8 the layout differed and they corrupted memory mid-collect, surfacing as `aver_http_handle threw: RuntimeError: memory access out of bounds`. Removing the dead walks fixes it.
- **`Result.withDefault` / `Option.withDefault` / `Option.toResult` first-arg validation.** All three combinators were registered with `Type::Unknown` parameters in `src/types/checker/builtins.rs`, so `Result.withDefault(Vector.get(v, i), 0)` (Vector.get returns `Option<T>`, not `Result<T, E>`) compiled cleanly through `aver check` and silently returned the default at runtime — every lookup folded to the fallback value with no error surfaced. The special-case handlers in `infer/expr.rs` already had the right return-type inference but skipped argument validation; they now emit a real type error when the first argument is not the expected wrapper. `Type::Unknown` still flows through as escape hatch for genuinely polymorphic returns.
- **`HttpServer.listenWith` context-handler type linkage.** The second argument (user-defined context) and the handler's first parameter must share the same type — in the `weather.av` style, both are `WeatherContext`. The builtin sig leaves context as `Type::Unknown` (Aver builtins don't carry parametric polymorphism), so the linkage is enforced as a cross-arg check after the standard sig-based validation: extract the handler's `Type::Fn` first param, compare with the inferred type of the context arg, emit a type error on mismatch. Same logic covers `SelfHostRuntime.httpServerListenWith`.
- **`SelfHostRuntime.httpServerListen{,With}` handler typing.** Both aliases declared their handler parameter as `Type::Unknown`, so callers could pass an `Int` where a `Fn(HttpRequest) -> HttpResponse` was expected and the compile would proceed past type-check. Tightened to `http_handler()` / `http_handler_with_context()` to mirror the public `HttpServer.listen{,With}` shape.

## 0.14.2 (2026-04-29)

### Added
- **Pure no-alloc fast path on both WASM and VM.** New shared `compute_alloc_info` analyzer (`src/ir/alloc_info.rs`) parametrised by a backend-specific `AllocPolicy` trait classifies every user fn as allocating or not, walking the call graph to fixpoint. The WASM emitter then skips the prologue `heap_ptr` save and epilogue `rt_truncate` for no-alloc bodies; the mutual-TCO trampoline drops its watermark check, per-iter heap_ptr save, and adaptive compaction when every member is no-alloc. The VM compiler tags qualifying chunks so `TAIL_CALL_KNOWN` / `TAIL_CALL_SELF` skip `finalize_frame_locals_for_tail_call`, `CALL_KNOWN` parks dummy zero marks, and `RETURN` short-circuits the standard fast-return path. Mandelbrot bench (160×96 × 100 reps × 80 iter, mutual-TCO `mandelStep ↔ mandelIter` with cardioid + period-2 bulb pre-tests) runs ~4× faster on WASM and ~22% faster on VM. Closes the bulk of the float-tight-loop gap with peer typed-wasm + Cranelift toolchains (down from ~3× slower to ~1.07× slower, apples-to-apples).
- **`HttpRequest.query: String` field** for the fetch bridge. `req.query` returns the URL search string without the leading `?` (previously dropped silently — `req.body` worked but `req.query` was simply absent from the builtin record). Plumbed through `builtin_records.rs`, `abi.rs` (`Request.query` → `request_query`), the fetch-bridge field-emit map, the cloudflare `worker.js` template, and the type-checker's net-request field list. Enables URL-driven server-side state without an extra parser layer.
- **Friendly compile-time error for HTTP types under non-fetch bridges.** `aver compile … --target wasm` (no bridge) on a program declaring `fn handler(req: HttpRequest) -> HttpResponse` previously produced invalid bytecode (`type mismatch: expected i64, found i32` deep in the record-shape codegen). The emitter now rejects HttpRequest/HttpResponse parameters/returns under any non-fetch adapter before emit, naming the offending fn and pointing at `--preset cloudflare` / `--bridge fetch`.
- **`/fractal` on `edge.averlang.dev` becomes interactive.** Replaces the previous static Braille pair with a single-panel 200×120 half-block Mandelbrot driven by URL query params (`cx`, `cy`, `w`). Per-pixel 16-step colour palette (each cell `<i c=XX>▀</i>` packs top/bot palette indices into one 2-hex attribute, ~38% smaller HTML than per-cell two-class form). Pan / zoom / reset buttons + six landmark presets (seahorse, mini-Mandelbrot, double spiral, elephant valley, tendrils, eastern bulb) all encode their target view in the URL — every navigation fires a fresh handler invocation with `cache-control: no-store`, no precomputed images. Iter cap auto-scales (100 / 150 / 250) so deep zooms stay sharp at modest extra compute. Production warm TTFB ~35-40 ms (keep-alive); first hit ~70 ms with TLS handshake.

### Changed
- **WASM mutual-TCO trampoline collapses to a flat shared slot row when every member has an identical typed signature and is no-alloc** (`mandelStep ↔ mandelIter` style). Per-tail-call args reshape skips its `eval → tmp → target` double-copy under those conditions and writes args directly into target slots in reverse stack order. Eliminates ~10 `local.set` per iter from the trampoline hot loop.

### Docs
- `docs/wasm.md` — reframed `aver.toml` policy as a deliberate host concern, not a missing feature. New "Policy is the Host's Job" section cites wasmtime/WASI `--allow-net`, Cloudflare Workers `services`/`fetch` bindings, browser CSP `connect-src`, and Fastly Compute backend whitelists as the actual enforcement layers; build-time concerns (effect declarations, deterministic mocks, independence invariants) stay with us.

### Tooling
- `tools/release.py` — GitHub release titles now carry the codename for thematic releases (`Aver 0.14.0 "Edge"`); patches stay plain. Cascade-bump logic tightened to fire only for genuine publish-blockers (`aver-rt → aver-memory`); `aver-lsp` no longer auto-bumps just because `aver-lang` does.

## 0.14.1 (2026-04-29)

### Removed
- **`Header` record.** 0.14 standardised HTTP headers as `Map<String, List<String>>` across every backend; the per-entry `Header { name, value }` record left over from the old `List<Header>` shape was unreachable from any built-in HTTP type and is now retired. User code that constructed `Header` literals must now build a `Map<String, List<String>>` directly.

### Fixed
- **`Http.post` / `Http.put` / `Http.patch` oracle signature** now reports the headers parameter as `Map<String, List<String>>` instead of the stale `List<Header>` from the pre-0.14 shape. Verify-mode mocks and effect classification see the correct type.
- **Self-host codegen template** (`--with-self-host-support`) emitted `headers: AverList<Header>` for HTTP request/response handling. Now emits `headers: HttpHeaders` (= `AverMap<AverStr, AverList<AverStr>>`), matching the runtime shape every other backend already used.

### Docs
- `docs/services.md`: `Http.*` signatures and `HttpRequest` / `HttpResponse` records updated to the `Map<String, List<String>>` headers shape.

## 0.14.0 "Edge" (2026-04-29)

> _Aver's WASM backend becomes a deployable edge target: small user modules, a shared runtime, and explicit host bridges._

### Added
- **`--target edge-wasm`** emits a thin `user.wasm` that imports a separately hosted `aver_runtime`, so browser and edge deployments can cache the runtime once.
- **Host bridges:** `--bridge fetch` for JS/Workers-style hosts and `--bridge wasip1` for standalone WASI preview 1 execution.
- **Cloudflare Workers pack output (`--preset cloudflare`)** — drops `worker.js` (ES-module bootstrap that wires `aver/*` host imports against `console.*` / `Date.now()` / `Math.random()` / Fetch + JSPI for `Http.*`) and `wrangler.toml` next to a single bundled `user.wasm`. Cloudflare Workers reject `WebAssembly.instantiate(bytes, …)` from runtime-fetched bytes, so the preset uses `--target wasm` (runtime inlined via `wasm-merge`) for static-import shape; `--target edge-wasm` stays for browsers / Deno / Bun where the runtime CDN at `averlang.dev/runtime/` works. Runtime artifacts, checksums, and manifest are published independently under `/runtime/`.
- **WASM host coverage for `Env.*`, `Console.warn`, `Http.*`, request/response headers, and multi-value header flow.**
- **WASM `Map` runtime** — flat hashtable with structural hashing/equality across all hashable key types (Int, Float, Bool, String, heap), O(1) `Map.len`, owned-mutate fast path on `Map.set` when last-use analysis proves sole ownership. `map build 5k` bench: WASM 963 µs vs VM 1.33 ms. Replaces the prior placeholder linked-list shape that was O(N²) on TCO build patterns.
- **`--optimize size|speed`** for the WASM optimization pipeline.
- **Runtime artifacts at `/runtime/`**: `aver_runtime.wasm`, `aver_to_wasi.wasm`, WAT companions, checksums, versioned URLs, and `latest/`.

### Changed
- **HTTP headers are `Map<String, List<String>>`** across request/response records and `Http.post` / `Http.put` / `Http.patch`.
- **WASM map ABI is polymorphic**, with key kind and value pointer flags passed explicitly.
- **WAT is the source of truth for the standalone runtime**, and emit-time WASM validation is part of the compile path.
- **WASM `Vector.set` is O(1) in TCO build loops** via owned-mutate dispatch — fused `Option.withDefault(Vector.set(v, i, x), v)` where `v` is a last-use slot lowers to an inline bounds check + `i64.store`. Same trick the VM uses; matches its perf profile on `vector get/set 5k`.

### Removed
- **`--adapter` → `--bridge`** and **`--wasm-opt oz|o3` → `--optimize size|speed`**.
- **`--target wat` and `--target wasm+wat`**; use standard WASM tooling for WAT output.

### Fixed
- WASM `Map<Int|Float|Bool, V>` validation and lookup issues.
- VM map structural equality for heap keys.
- Cloudflare/fetch bridge response headers no longer get dropped.

### Known limitations
- `Http.*` under `--bridge wasip1` returns a transport error; real WASI HTTP belongs in a later Component Model target.

## 0.13.0 "Limit" (2026-04-27)

> _Pure core. Explicit shell. Auditable boundary. Aver learns to say no — at the module's edge, to extra calls, to hostile worlds._

### Added
- **`aver verify --hostile` / `aver audit --hostile`** — runs every `verify <fn> law <name>` block under an adversarial expansion of its `given` clauses. Typed value domains (`given x: Int = [3]`) get augmented with the per-type boundary set (`0`, `1`, `-1`, `i64::MIN`, `i64::MAX`; for `Float` add `±Inf` and `NaN`; for `Str` add empty / NUL-embedded / multi-byte / 1024-char). On top of that, classified effects get multiplied by adversarial profiles (frozen / fast-forward / backward clocks, always-min / always-max / alternating random, network-down responses, empty / always-error filesystem, …) — `given` for an effect is just the worlds *you* listed, hostile mode adds more on top, and `law` form already quantifies universally over every stub. Failures that surface only here mean the law isn't universal — either weaken it with `when <precondition>` to scope it to the worlds where it actually holds, downgrade from `law` to `verify <fn>` cases-form when the claim is really stub-specific, or accept the profile as a real production world the impl should handle.
- **Module-level effect boundary** — `effects [...]` declaration on the module header. Every function's `! [Effect]` must be covered by the module's declared surface, enforced at type-check. A namespace entry like `Disk` admits any `Disk.*` method; method-level `Disk.readText` admits only that one. Modules with functions but no `effects [...]` get a soft warning nudging them to declare the boundary; you opt in when you want the enforcement.
- **`when` over oracle stubs (oracle assumptions).** In `verify <fn> trace law <name>`, the `when` predicate may now reference an effect-given oracle directly: `when clock(BranchPath.Root, 1) > clock(BranchPath.Root, 0)` declares "this law assumes a monotonic clock". Hostile profiles that violate the assumption are skipped; profiles that respect it run normally. Read-your-writes, conservation, idempotent fetch, protocol order — invariants you used to leave in comments now live in source. Skipped count is shown alongside passed / failed; if every adversarial profile is skipped, you get a vacuous-under-hostile warning so the law isn't silently uncovered.
- **`.trace.count(method)`** — quantitative trace assertion. Returns the number of trace events whose method matches the argument (`Console.print`, `Http.get(...)`, etc.). Complements `.contains` (boolean any-match) with the count form so laws can pin "this fn calls the API exactly once" or "no extra disk reads under hostile profiles".
- **`verify-hostile-mismatch` diagnostic slug** — distinct from the regular `verify-mismatch`, so CI gates can route declared-world regressions and adversarial-world surfacings to separate channels: `jq '.diagnostics[] | select(.slug == "verify-hostile-mismatch")'`. Each carries `from_hostile: true` and an `origin` field with the profile label (`hostile effect profile: Time.unixMs/saturated`, `hostile boundary expansion`).
- **Oracle invariants in proof export — documented and enforced.** The trust header in every `aver proof --backend lean` / `--backend dafny` artifact spells out the per-effect bounds the runtime guarantees: `Random.int` stays in `[min, max]`, `Random.float` in `[0.0, 1.0]`, `Time.unixMs ≥ 0`, `Disk.exists` is total. Lifted theorems quantify over subtype-encoded oracle types (`RandomIntInBounds`, `RandomFloatInUnit`, `TimeUnixMsNonneg`) — the bound is a constrained quantifier at the lemma level, not a free side-condition you have to discharge separately.
- **Playground hostile toggle** — checkbox next to the Audit button. When checked, audit runs the hostile pipeline; the Verify panel and the structured JSON output carry the same dual-run breakdown (declared vs hostile, with the per-block scorecard).
- **Showcase example: hostile clock.** `examples/formal/hostile_clock.av` (and the Oracle dropdown in the playground): a deadline check that passes under real time and breaks under the saturated-clock profile — concrete demo of what `--hostile` finds that plain `verify` misses.
- **Dual-run breakdown in TTY and JSON.** Per-block summary line shows `(1/1 declared, 11/35 hostile, 3 skipped by `when`)` so you can see at a glance whether a regression is in the declared world or only surfaces under adversarial profiles. JSON output adds four counters on `verify_summary.blocks[]` — `declared_passed`, `declared_failed`, `hostile_passed`, `hostile_failed` — for tooling that wants to split "law regression" from "hostile coverage gap".
- **Cartesian cap.** Hostile expansion stops at 10,000 cases per block (same ceiling the parser uses for declared `given` ranges). Over-budget blocks get a clear error pointing at the law and listing the projected size; tighten the `given`, add a `when`, or run that block without `--hostile`.

## 0.12.0 "Atlas" (2026-04-25)

> _Multi-module Aver projects export to Lean and Dafny end-to-end. Generated proofs shrank by ~85% on pure-math examples. All 42 canonical examples now pass both proof backends._

### Added
- **Multi-module proof export.** Projects with `depends [...]` produce one `.lean` / `.dfy` file per module, plus a shared `AverCommon` library carrying only the helpers your code actually references. Submodule paths like `Models.User` land at `Models/User.{lean,dfy}`. Works on rogue, doom, tetris, checkers, notepad — every multi-module example in the repo, including those mixing same-named fns across modules and modules sharing a name with a record.
- **`?` operator now works in pure proof export.** Previously `cmd = parseCommand(x)?; <rest>` produced uncompilable proof code; now correctly lowers to `match … { Result.Ok(cmd) -> <rest>; Result.Err(e) -> Result.Err(e) }` before reaching Lean / Dafny. Fixes mission_control.
- **Concrete `Float` evaluation in Lean proofs.** `Float.floor`, `Float.round`, `Float.ceil`, `Float.toInt` now match the runtime exactly (IEEE 754 + saturating cast, including NaN → 0 and ±∞ → i64 bounds). `verify` cases over Float values get real `native_decide` evaluation in Lean instead of opaque stubs. Edge-case behaviour is asserted by `native_decide` proofs that re-run on every Lake build.
- **`Float.sin`, `Float.cos`, `Float.atan2`** are now proof-exportable across both backends, not just runtime. doom's raycaster compiles to Lean / Dafny.
- **`Terminal.size` is now a verifiable effect.** Oracle signature `() -> Terminal.Size`, same shape as `Args.get` / `Env.get`. Example: `examples/formal/terminal_size_snapshot.av`.
- **Playground download menu.** The ⬇ button now expands into Aver source / WASM binary / Rust project / Lean 4 proof / Dafny proof — your in-browser source compiles and ships as `.av` / `.wasm` / `.zip`.

### Changed
- **Generated proof files shrank dramatically.** Lean and Dafny now emit only the helpers, records, and trust-assumption headers your code actually references — no more 1500-line preamble for a 20-line pure-math file. Calculator and pure-laws examples shrank ~85-90% in Lean, ~70-90% in Dafny; effectful examples 25-40%.
- **Shared backend infrastructure.** Built-in records, helpers, and per-module emission logic live in single shared modules consumed by Lean / Dafny / WASM. Adding a new built-in record shape is one line in `codegen::builtin_records`.
- **`aver context` output is denser (schema_version: 7, breaking).** Markdown signatures now use the Aver source form `name(args) -> Ret ! [Effects]` instead of a separate `effects:` line. JSON `records.fields` and `types.variants` are pre-formatted strings (`["id: String", ...]`, `["IngestWebhooks(String, String)", ...]`) instead of nested objects, and arrays are rendered inline (`[a, b, c]`) while objects stay multi-line. Net effect on the showcase projects: workflow_engine fits 78/131 elements at 24kb (was 47/131); payment_ops fits 98/98 at 32kb with room to spare. Anything parsing CONTEXT.json should switch from `fields[i].name` / `fields[i].type` access to `split(": ")` on the string.

## 0.11.0 "Oracle" (2026-04-24)

> _Effectful functions get verified now — bind an oracle with `given`, check the trace, or export the universal law to Lean & Dafny. Dafny caught up with Lean across every recursive shape._

### Added
- **Oracle law and trace docs/example** — `docs/oracle.md` now separates proof-oriented `verify <fn> law` over explicit oracles from cases-form `verify <fn> trace` for `.result` / `.trace.*` assertions. Added `examples/formal/oracle_trace.av` as the short runnable example.
- **Broader Oracle effect classification** — Oracle now covers CLI input (`Console.readLine`), disk operation/result effects, one-shot TCP (`Tcp.send` / `Tcp.ping`), `Time.sleep`, and terminal trace/input calls that do not depend on modal terminal state.
- **Dafny proof backend reaches feature parity with Lean on recursion.** Shared recursion classifier (`codegen::recursion::detect`) + AST transform now feed both backends. Dafny emits mutual-recursion SCCs as fuel-guarded `function fn__fuel(fuel: nat, …) decreases fuel { … }` pairs with plan-specific metrics, parallel to Lean's `def fn__fuel (fuel : Nat) …`. Shapes that admit no total default or use `?` that doesn't lower fall back to `function {:axiom}` — the Dafny analogue of Lean's `partial def`. Lemmas over opaque fns short-circuit to `assume {:axiom} <ensures>;`, the Dafny analogue of Lean's `sorry`. Across the 23 canonical examples: 12 are clean on both backends (full proof), 9 have matching proof gaps (Dafny axiom/assume vs. Lean sorry), 2 are pre-existing codegen gaps orthogonal to the recursion story.

### Changed
- **Effectful verification story** — README, language guide, and proof-backend docs now distinguish Oracle verification for classified effects from record/replay for ambient state, persistent protocols, terminal modes, and server callbacks.
- **`codegen::recursion` module** — `RecursionPlan` enum, `ProofModeIssue`, recursion classifier, and `rewrite_recursive_calls_{body,expr}` AST transform pulled out of `codegen::lean` into the shared module. Lean and Dafny both consume it. `RecursionPlan::IntAscending` now holds the bound as an Aver AST node (`Spanned<Expr>`) instead of a Lean-rendered string — each backend renders it in its own idiom.

## 0.10.1 (2026-04-23)

### Added
- **`aver run --expr '<call>'` / `--input-file PATH`** — record or run any function, not just `main`. Pass a call like `aver run src/tax.av -e 'loadTaxRate("PL")' --record dir/` and the recording's `entry_fn` and `input` are populated from the call; `aver replay` picks it up unchanged. Repeat `-e` to batch. Supports literal, list/tuple, and ADT-constructor arguments (`Result.Ok(5)`, `Shape.Circle(1.0)`, nested). Function calls / arithmetic / variables in arg position stay out of scope — wrap them in a helper function and call that instead. The same capability is exposed in the playground's Trace panel.

## 0.10.0 "Telltale" (2026-04-21)

> _Tooling now shows its work — parse errors point at the exact token with a repair, audit runs three axes in one shot, and every program can be recorded & replayed in the browser._

### Added
- **`aver audit`** — one command that runs static checks, verify blocks, and format compliance together. CI-friendly exit code, `--json` for pipelines.
- **`aver format --check` / `aver format --check --json`** — non-mutating format verification. Every rewrite reports a structured `FormatViolation` with a stable `rule` slug: `tab-indent`, `bad-function-header`, `effects-unsorted`, `effects-reshape`, `verify-misplaced`, `excess-blank`, `module-intent-reshape`, `decision-inline`, `trailing-whitespace`, `missing-final-newline`. Agents and linters can key off specific rules instead of free-text diffs.
- **Naming convention checker** — flags non-camelCase functions / fields and non-PascalCase types / modules / variants as stable diagnostics (`bad-fn-name`, `bad-type-name`, `bad-module-name`, `bad-variant-name`, `bad-field-name`). Runs as part of `aver check` and `aver audit`.
- **Canonical diagnostic bundle across every CLI command** — `aver check --json`, `aver verify --json`, `aver why --json`, `aver audit --json`, `aver format --check --json` all emit `AnalysisReport` NDJSON now. One schema, `schema_version: 1`, documented in `docs/diagnostics-schema.md`.
- **Reworked [browser playground](https://averlang.dev/playground/)** — multi-file editor, interactive record & replay, full parity with CLI audit / why / context / format. Go play.

### Changed
- **Format engine rewritten around structured violations** — the formatter used to report "needs format" as opaque before/after diffs. Every normalization pass now tracks per-line rule violations with original source-line numbers, so `--check` output points at the exact line that needs the specific fix instead of dumping a reformatted file. `needs-format` stays as the aggregate marker; each `FormatViolation` rides alongside with its rule slug.
- **Effects list in `aver context`** — `signature` is now params + return type only; effects live on the sibling `effects` array. Lets renderers show them without duplicating `! [...]` on screen.

### Fixed
- **Parse errors landed on line 1:1 with no hint** — the formatter stripped the real span and emitted a single red line that didn't tell you where or why. Now: real line/col pulled from the parser, source snippet with `^^^` caret under the offending token (clamped to the last char for EOL errors like Unterminated string), and repair hints for common shapes (`Expected '[' after '!'` → `! [Console.print, ...]`, missing `module <Name>`, map `=>` syntax, tuple-needs-2-elements, …).
- **`Time.sleep` on wasm32-unknown-unknown panicked** ("can't sleep") — the browser runtime now makes it a no-op; native builds keep real blocking sleep.

## 0.9.7 (2026-04-16)

### Changed
- **Pre-compiled self-host** — `--self-host` no longer generates Rust code and runs `cargo build` at runtime. The self-host interpreter is compiled as a `[[bin]]` target alongside `aver`, so `cargo install aver-lang` provides both binaries out of the box. No Rust toolchain needed at runtime.
- **Cargo package cleanup** — published crate excludes `self_hosted/`, `examples/`, `tools/`, `editors/`, etc. Only `src/` and essentials ship. Self-host generated code lives in `src/self_host/`.

### Added
- **Release script** (`tools/release.py`) — automates version bumps, self-host regeneration, playground rebuild, crates.io publish, and GitHub release creation.

### Fixed
- **`--self-host` crash from crates.io install** — the runtime codegen tried to read `aver-rt/Cargo.toml` relative to the installed crate source, which doesn't exist. Eliminated by removing runtime codegen entirely.
- **Rust codegen: TCO invariant hoisting of variant constructors** — enum variant constructors (e.g., `Val::ValStr`) were hoisted as loop invariants, causing move errors in generated Rust. Now excluded alongside builtins.

### Removed
- Runtime codegen pipeline for self-host (`build_self_host_binary`, fingerprinting, `cargo build --offline` invocation).

## 0.9.6 (2026-04-16)

### Performance
- **VECTOR_SET_OR_KEEP in-place mutation** — fused `Option.withDefault(Vector.set(v, i, val), v)` always has sole ownership; the opcode now mutates the vector directly at its arena slot. Vector get/set 5k: 17ms → 816µs (20× faster).
- **Skip promotion rewrite for bulk types without young refs** — vectors/maps/tuples of inline ints skip O(n) per-element rewrite during young→yard promotion. Vector get/set 5k: 17ms → 12.6ms (−26%).
- **VM arena in-place map rewrite** — map promotion rewrites NanValue pairs in-place via `Rc::make_mut` instead of rebuilding the HashMap. Map build 5k: 24ms → 1.3ms (18.9×). COW ops inherit source allocation space, skipping redundant promotion.

### Changed
- **Unified symbol resolution for uppercase dotted paths** — `classify_leaf_op` now classifies uppercase `Expr::Attr` paths (Option.None, variant constructors, module function refs) via three new `LeafOp` variants instead of returning `None`. Eliminates duplicate resolution logic from Rust and WASM backends. WASM backend now routes `Expr::Attr` through the shared IR layer.

### Added
- **WASM `Option.toResult` builtin** — `Some(v) → Ok(v)`, `None → Err(err_value)`.

### Fixed
- **WASM cross-module variant constructor resolution** — hierarchical module paths like `Domain.Types.TaskStatus.Blocked` now resolve correctly (previously only single-level bases worked).
- **Rust codegen: TCO invariant hoisting of builtin callees** — `List.prepend` and similar builtin namespace refs were hoisted as standalone value expressions, generating invalid Rust (`List.prepend`). Now excluded from hoisting since they're compile-time constants.

### Removed
- Dead `entry_has_young_refs` function.

## 0.9.5 (2026-04-15)

### Changed
- **Shared symbol layer** — module visibility, type registration, and symbol resolution now go through a single shared layer. All backends consume the same `ModuleExports` and `SymbolRegistry` instead of building their own views.
- **Unified module loader** — `load_module_tree()` replaces independent loaders in the VM compiler and type checker. Proper circular-import detection and module-name validation everywhere.

### Fixed
- **WASM codegen wrong types for private module helpers** — dependency modules with private functions returning non-Int types (e.g. `padTwo` returning `String`) caused invalid WASM. Codegen now has full signatures for all emitted functions.
- **Rust codegen missing `Arc::new()` for cross-module recursive types** — self-host binary failed to compile from a clean cache. Constructor boxed-position lookup now handles qualified names.

### Removed
- Suffix-matching heuristic in VM type resolution
- Dual-key registration in type checker (replaced by alias-based lookup)
- Checker's `ModuleSigCache` and cycle-detection stack (handled by shared loader)

## 0.9.4 (2026-04-15)

### Fixed
- **Rust codegen crash with forwarded arguments** — `aver compile` and `aver run --self-host` failed on programs with forwarded local variables after the unified resolver change in 0.9.3.

## 0.9.3 (2026-04-15)

### Performance
- **Map/Vector in-place mutation everywhere** — collections are now mutated in-place whenever the variable is at its last use, not just inside tail-call loops. VM map build 50K entries: **39s → 0.9s** (43× faster). Sequential `Map.set` chains without recursion also benefit.
- **WASM handles large collections** — map/list build with 50K+ entries no longer crashes. Previously overflowed at ~5450 entries.

### Changed
- **Unified variable resolution** — all backends (VM, Rust codegen, WASM, Lean, Dafny) now share a single resolver and liveness analysis. ~675 lines of duplicated ownership logic removed.
- **`rebuild_playground.py`** — now also rebuilds the in-browser compiler (`aver_bg.wasm`), not just game modules.

### Fixed
- **Map.set not using fast path with computed keys** — `Map.set(m, Int.toString(n), n)` was incorrectly falling back to the slow clone path. Now correctly detected as in-place-safe.

## 0.9.2 (2026-04-13)

### Fixed
- **WASM mutual TCO memory leak** — dead temporaries (HUD strings, record updates) accumulated across iterations in mutual tail-call loops because nested function calls masked garbage from the yard heuristic. Replaced per-iteration skip threshold with watermark-based adaptive compaction (triggers when garbage exceeds 16KB since last collection). Game of Life editor loop now stable at ~15KB instead of growing to 10+ MB.

### Added
- **`$heap_ptr` WASM export** — modules export the bump allocator position for host-side memory inspection.
- **Playground memory display** — live heap usage shown in status bar, throttled to 500ms updates.
- **Playground rogue touch controls** — added descend stairs (>) button.
- **`llms.txt` improvements** — expanded namespace signatures, explicit "do NOT exist" operator section, verify block rules, qualified constructor examples. Driven by vera-bench LLM evaluation data.

## 0.9.1 (2026-04-10)

### Added
- **Aver Playground** — write and run Aver in the browser at [averlang.dev](https://averlang.dev). In-browser compiler, 7 playable games, source viewer, syntax highlighting.

### Fixed
- **WASM variant equality** — nullary variants (`Color.White == Color.White`) now compare correctly.
- **WASM variant display** — shows type names instead of `Variant#0`.
- **WASM `Console.readLine`** — properly wrapped in `Result.Ok`, blocking via SharedArrayBuffer.

## 0.9.0 (2026-04-09)

### Changed
- **VM is the default backend** — `aver run`, `verify`, `replay`, and `repl` now use the bytecode VM directly. The `--vm` flag is no longer needed and has been removed.

### Removed
- **Tree-walking interpreter** — ~7500 lines removed. The VM covers all use cases the interpreter handled.

### Added
- **WASM backend** — `aver compile --target wasm` and `aver run --wasm`. Own `aver/*` import ABI with `--adapter wasi` for standalone wasmtime. Works with built-in host, browser JS shim, or custom host.
- **Browser WASM runner** — `tools/wasm-runner/` with terminal canvas rendering and keyboard input.
- **`aver-memory` crate** — standalone NaN-boxed value representation and arena allocator.

## 0.8.2 (2026-04-03)

### Added
- **Root-parallel checkers AI** — the checkers example now uses independent products to score root moves in parallel.

### Fixed
- **Self-host stability** — running `self_hosted/main.av` through the host interpreter and host VM is much more stable. Fixes cover qualified module calls, constructor pattern matching, resolver slot binding, and VM runtime aliases.
- **VM parallel map imports** — child VMs in independent products no longer silently drop map contents, fixing incorrect fallback behaviour in self-host runs.
- **Codegen: record field access** — accessing a field of a borrowed record parameter in return position now emits the required `.clone()`.
- **Codegen: memoized recursive functions** — call sites for auto-memoized recursive functions now correctly pass arguments by reference, matching the generated function signature.
- **Codegen: independent products with cancel mode** — `?!` expressions no longer produce invalid `let`-in-expression Rust code when cancel mode is active.
- **VM: no-main programs** — the VM now finishes silently when a program has no `fn main`, consistent with the interpreter.

## 0.8.1 (2026-04-03)

### Added
- **`examples/apps/status_board.av`** — an offline terminal dashboard that makes independent-product pipeline overlap visible in the VM and compiled Rust backends.

### Fixed
- **VM parallel child contexts** — child VMs now rebase `MATCH_DISPATCH` / `MATCH_DISPATCH_CONST` inline `NanValue` payloads when building the static arena for independent products. This fixes crashes when a branch returns or matches on heap-backed constant strings.
- **VM regressions for child arenas** — added coverage for direct child-VM calls, nested string interpolation, and full `CALL_PAR` execution so this class of arena-rebasing bugs stays closed.

## 0.8.0 (2026-04-02)

### Added
- **Independent products (`?!` / `!`)** — a tuple followed by `!` is a product of independent computations. `?!` adds Result unwrapping — all must succeed or the first error propagates. Independence is structural: tuple elements cannot reference each other. Composes recursively, giving fan-out parallelism and pipeline overlap with no new language concepts. No async, no futures, no channels — just products and independence. See [docs/independence.md](docs/independence.md).
- **Parallel execution** — compiled Aver programs run `?!` / `!` elements on separate threads. Two HTTP calls that each take 2 seconds complete in ~3 seconds, not 4. Recursive `?!` over a list fans out the entire tree.
- **Replay groups** — effects inside a `?!` / `!` product are order-independent in replay. Reordering independent code does not break recorded sessions.
- **`aver check` independence hazards** — branch-pair warnings for likely unsafe overlaps in independent products, including `Console`/`Terminal`, `Tcp`, `HttpServer`, and mutating `Disk` / `Http` / `Env` effects. Warnings can be suppressed via `[[check.suppress]]` with a mandatory reason.
- **`List.take` / `List.drop`** — list windowing helpers for bounded fan-out and batching patterns.
- **`aver why`** — justification coverage tracer. Scores every function as justified, partial, or unjustified based on description, verify blocks, and coverage. `--verbose` and `--json` output modes.
- **`[[check.suppress]]` in `aver.toml`** — suppress specific warnings with a mandatory `reason`.

### Changed
- **Thread-safe runtime** — all runtime types use atomic reference counting, enabling parallel execution of independent products.
- **Structured replay diagnostics** — per-error-type diagnostics with source locations. Args diff hint shown even without `--check-args`.
- **Independent-product replay matching** — replay now keys grouped effects by `branch_path + effect_occurrence + effect_type + effect_args`, so nested and repeated effects replay deterministically across reordering.
- **VM independent products** — `CALL_PAR` now carries callable values rather than only statically resolved function ids, so aliases like `f = foo; (f(x), f(y))!` work. Branch VMs import a thin static arena plus per-branch inputs/outputs instead of cloning the whole parent heap.
- **Compiled Rust cancellation** — generated Rust now uses cooperative cancel checkpoints for `?!` in `mode = "cancel"` and keeps branch closures borrow-checker-safe.

## 0.7.3 (2026-03-30)

### Added
- **`aver verify --json`** — structured NDJSON output: `block-result` per verify block, `diagnostic` per failure, `summary` at end.
- **`aver verify --verbose`** — failure diagnostics with source snippets and full fields (given/law context for specs).
- **`aver replay --json`** — NDJSON output: `replay-result` per recording, `summary` at end.
- **Structured verify diagnostics** — verify failures use the same diagnostic system as `aver check`: `fail[verify-mismatch]`, `fail[verify-runtime-error]`, `fail[verify-unexpected-err]` with `at:`, `block:`, `case:`, `expected:`/`actual:`, source snippets with carets. Normal mode caps to 3 diagnostics per block.

### Changed
- **Inline variants (TAG 14)** — single-field variants whose payload is a small int (±268M), bool, unit, or none are now NaN-boxed inline (8 bytes, zero arena allocation). Pattern matching and field extraction skip arena indirection entirely.
- **Unified NDJSON format** — `check`, `verify`, and `replay` all emit `{"schema_version":1,"kind":"..."}` envelope with summary events.
- **Verify output redesign** — per-file grouping, one-line block summaries with failure type breakdown, streaming output. Skipped files (type errors) show count + hint about `--module-root`.
- **Codegen: let-destructuring** — single-arm irrefutable matches (`match x: (a, b) -> expr`) now emit `let (a, b) = x; expr` instead of a full match block.

### Fixed
- `aver check --json` no longer emits human-readable lines mixed with JSON.
- `aver replay` no longer duplicates "Replay:" prefix in error messages.
- `aver run --verify` now shows file path and source snippets in failure diagnostics.

## 0.7.2 (2026-03-29)

### Added
- **Structured error messages** — `aver check` shows source snippets, repair suggestions, and semantic error categories (`type-mismatch`, `unused-binding`, `missing-verify`). Use `--verbose` for full context on warnings.
- **Unused binding warnings** — `aver check` warns on bindings that are defined but never used. Prefix with `_` to silence.
- **`aver check --json`** — structured JSON output for editor and CI integrations.
- **`Map<T, Unit>` as set** — Lean codegen emits `Finset T`, Dafny emits `set<T>`. See [docs](docs/language.md#sets).
- **Common Pushback FAQ** — [docs/pushback.md](docs/pushback.md) covers frequent questions and objections about the language.

### Changed
- **Faster compiled code** — generated Rust is significantly faster across all benchmarks: pattern matching -66%, maps -13%, records -14%, vectors -19%. The self-hosted interpreter is 7-25% faster depending on workload. Fused IR ops (`IntModOrDefault`, `ListIndexGet`) eliminate intermediate allocations; codegen now skips unnecessary clones on Copy fields, drops `&` on numeric arithmetic, and matches borrowed params without cloning the subject.
- **LSP** — Vector namespace completions, updated List members, `exposes opaque` support in document symbols.
- **Editor highlighting** — VSCode and Sublime grammars updated with all current namespaces and keywords.
- Aver formatter keeps medium effect lists on one line when they fit.

### Fixed
- `Console.error`/`Console.warn` in self-hosted now route to stderr.
- `--with-self-host-support` enforces guest-entry contract.

## 0.7.1 (2026-03-27)

### Changed
- `aver run --self-host` now caches its generated helper per installed Aver/self-host build instead of per guest `module_root`, so switching projects no longer forces a rebuild.
- Self-hosted guest `aver.toml` policy is now loaded at runtime from the guest module root and starts only at the guest boundary, matching scoped replay behavior.
- Cold `--self-host` runs now print short progress messages while Aver generates and builds the cached helper.
- `aver compile` now exposes runtime policy mode explicitly via `--policy embed|runtime`; plain codegen defaults to `embed`, while `--with-replay` defaults to `runtime`.

### Fixed
- `aver run --self-host` no longer misclassifies qualified user module calls like `Map.generateMap` or `Time.foo` as builtins just because they share a builtin namespace prefix. Self-hosted module programs such as `examples/games/rogue` now execute correctly again.

## 0.7.0 (2026-03-26)

**Breaking:** `List.get` and `List.append` removed. Use `Vector` for indexed access.

### Added
- **`Vector<T>`** — indexed sequence with O(1) get/set. API: `Vector.new`, `Vector.get`, `Vector.set`, `Vector.len`, `Vector.fromList`, `Vector.toList`. `Vector.set` returns `Option<Vector<T>>`.
- **Mutual TCO** in codegen — mutually recursive functions compiled to trampoline dispatch loops.
- **Namespace effect shorthand** — `! [Disk]` covers all `Disk.*` effects.
- **Self-host CLI path** — `aver run --self-host` and `aver replay --self-host` now run through the Aver-in-Aver interpreter compiled to a cached Rust binary.
- **Scoped generated replay runtime** — `aver compile --with-replay --guest-entry <fn>` emits replay support that starts record/replay and `aver.toml` policy at an explicit guest boundary instead of the process boundary.

### Changed
- **`List` is now purely recursive** — `prepend`, `head`, `tail`, `concat`, `reverse`, `contains`, `find`, `any`, `zip`. No indexed access.
- Idiomatic pattern: build with `List.prepend` → `List.reverse` → `Vector.fromList` (zero-copy on Flat lists).
- Compiled projects use LTO + `codegen-units = 1` for faster release builds.
- Self-hosted interpreter ~1.5× faster (COW maps, `Rc<str>` strings, Vector env).
- Installed `aver` now bundles the `self_hosted/` sources directly, so `aver run --self-host` bootstraps its cached helper binary automatically without a separate self-host install step.
- Generated Rust projects now target Rust 2024.
- `aver check` no longer suggests granular namespace effects on wrappers that also require the broad namespace transitively through a callee.
- `benches/comparison_bench.rs` now measures the real `aver run --self-host` CLI path instead of a stale standalone `aver-self` binary from `$HOME/.cargo/bin`.

### Removed
- `List.get`, `List.append`.

## 0.6.1

Highlights:
- VM is 25–54% faster across benchmarks; interpreter-to-VM speed ratios improved from 5–7× to 7–13×.
- Added `aver run --profile` for opcode/function-level VM profiling.
- Fixed several VM correctness and memory issues, including match fallthrough, deep-list return overflow, and request-local stable-space retention.

### Added
- `aver run --profile` — VM execution profile with opcode counts, function stats, and opcode-pair analysis.
- Game of Life example (`examples/games/life.av`) with terminal visualization and FPS counter.
- Self-hosted interpreter project in Aver (`self_hosted/`).

### Changed
- Added specialized VM handling for common unwrap/default, boolean branch, and fused-load patterns.
- Added frameless calls for small leaf functions to reduce hot-path call overhead.
- Bool `match` on `true/false` now compiles to a direct conditional branch.
- Refined VM value layout to reduce wrapper overhead and speed up dispatch.
- `Terminal.size` now returns a record with `width`/`height` fields instead of a tuple. Generated Rust requires `aver-rt >= 0.3.1`.
- `aver context --json` now uses `serde` serialization.
- Lean proof export now emits universal theorems with `sorry` when auto-proof fails.

### Fixed
- Exhaustiveness checker hang on recursive sum types.
- `MATCH_DISPATCH_CONST` fallthrough causing infinite recursion in patterns like `fib(n)`.
- Arena stack overflow on deep list returns.
- `HttpServer` callback stable-space retention across requests.
- Lean export reserved-word conflict for `toString`.

## 0.6.0

### Added
- **Bytecode VM** — `aver run --vm` compiles Aver to a stack-based bytecode VM with NaN-boxed values, region-based arena memory (young/yard/handoff/stable), dedicated list opcodes, structural persistent lists, and thin-function fast return paths. 5-9x faster than the tree-walking interpreter on compute-heavy workloads.
- **Terminal service** — `Terminal.*` namespace (12 methods) for raw-mode terminal I/O via crossterm: cursor control, colored output, non-blocking key input, screen management. Behind `terminal` cargo feature (enabled by default).
- **Terminal guard** — `aver run` installs a drop guard that restores terminal state (cursor, colors, raw mode) on exit, panic, or runtime error.
- **Bool namespace** — `Bool.or`, `Bool.and`, `Bool.not` pure builtins for logical combinators.

### Changed
- `aver-rt::AverList` now packs repeated `append` chains into segmented chunk spines, improving list-heavy workloads in both the interpreter and generated Rust.

## 0.5.5

### Added
- **Opaque types** — `exposes opaque [TypeName]` in module declarations. Types listed as opaque are visible in signatures but cannot be constructed, field-accessed, or pattern-matched from outside the defining module. Enforced at compile time by the typechecker. See `docs/language.md` for usage.
- `aver context --focus <symbol>` builds context around a specific function's dependency cone (callees, types, verify blocks, decisions)
- Priority scoring for `aver context` budget allocation: elements with more verify coverage, spec references, and focus relevance are included first
- Type-aware verify sample selection: scorer uses fn return type to pick diverse cases (Ok + Err for Result fns, true + false for Bool, per-constructor for sum types)
- Granular verify coverage warnings: checker now reports missing Result Ok/Err, Option Some/None, Bool true/false, and sum type variant cases

### Changed
- `src/checker.rs` split into `src/checker/` module (coverage, verify, intent, law)

## 0.5.4

### Added
- **Dafny verification backend** — `aver proof --backend dafny` generates a `.dfy` file with Z3-powered automated proofs for `verify law` blocks; complements Lean's `native_decide` / tactic approach
- **Random service** — `Random.int(min, max)` and `Random.float()` with `! [Random]` effect, backed by `aver_rt::random` (OS entropy via `rand` crate behind feature flag)
- **Shared `Builtin` enum** — `codegen/builtins.rs` defines all pure Aver builtins (~80 variants); adding a new builtin forces all backends to handle it via exhaustive match
- **Shared codegen utilities** — `codegen/common.rs` now provides `escape_string_literal`, `split_type_params`, `escape_reserved_word`, `parse_type_annotation`, and `to_lower_first` used by all three backends
- `docs/dafny.md` documenting the Dafny backend, its two-layer contract (sample assertions + universal lemma), and Lean vs Dafny comparison
- Wumpus example (`examples/apps/wumpus.av`) — Hunt the Wumpus with dodecahedron topology, effectful random, full verify coverage (30/30)

### Changed
- `aver proof` now accepts `--backend lean|dafny` (default: `lean`)
- **Lean auto-proof simplified** — removed brittle indirect-recursion and recursive map-presence strategies; the backend now honestly rejects patterns it can't prove instead of generating fragile proofs. Helper-law dependency hints guide users toward layered verification.
- Dafny emits `verify law` as both capped sample assertions (max 5) and a universal `lemma`; `verify` cases are not emitted (Z3 can't compute deep recursion)
- All three codegen backends (Lean, Dafny, Rust) now share reserved-word escaping, string literal escaping, and type annotation parsing via `codegen/common.rs`
- Rust codegen now gates `aver-rt` features (`http`, `random`) based on which services the program actually uses

## 0.5.3

### Added
- `projects/payment_ops` as a medium-size dirty-backoffice showcase covering provider normalization, replay, settlement reconciliation, manual-review cases, and audit trail

### Fixed
- Rust codegen ownership for list / tuple / map literals and record updates, so valid Aver programs no longer emit generated Rust that fails with move errors in these patterns

## 0.5.2

### Added
- `workflow_codegen_bench` for repeatable end-to-end comparisons between interpreter, VM, and generated Rust on `projects/workflow_engine`

### Changed
- interpreter function bodies now lower to shared `ExprId`-based runtime nodes, so the evaluation hot path no longer carries cloned AST fragments through continuations
- `aver check` now tells users with non-tail-recursive functions to either rewrite them into tail recursion or make them a spec, and canonical spec functions no longer emit that warning
- generated Rust now pins `aver-rt = "=0.2.1"` so current codegen matches the shared runtime features it emits

### Fixed
- Rust codegen regressions around nested builtin-argument liveness, same-arity mutual tail calls, and memoized recursive named types, restoring generated builds for examples such as `grok_s_language`, `red_black_tree`, and `mysql`


## 0.5.1

### Added
- native LSP document formatting via the shared Aver formatter
- richer `aver-lsp` editor UX: effect-aware completion, verify/decision code lenses, contract-first hover, and document symbols with nested `verify`
- publishable `aver-lsp` crate metadata and docs for installing the language server separately from `aver-lang`

### Changed
- editor install docs now target `cargo install aver-lsp` plus editor extension installation, with local source-build setup kept as a development path

## 0.5.0

### Added
- `Args.get()` as an explicit runtime service for CLI arguments (`List<String>`)
- round-trip law coverage for naturally invertible examples, including `json`, `grok_s_language`, and `notepad/store`
- `aver context --budget` with `kb` / `mb` suffixes for prompt-sized exports
- `aver context` selection metadata in JSON and in the `--output` summary, including included depth and next-depth size
- modular Rust code generation that emits `src/aver_generated/...` instead of flattening all Aver code into one giant `main.rs`
- directory inputs for `aver check` and `aver verify`, so one command can walk a whole example or project tree
- `projects/workflow_engine` as a serious medium-sized Aver application core, covering projects, tasks, workflow rules, audit trail, notifications, and CLI/query flows
- `aver check` warnings for recursive functions that still contain non-tail recursive callsites after TCO, with accumulator-style guidance
- iterative interpreter expression evaluation backed by a heap continuation stack instead of the Rust call stack

### Changed
- **Breaking:** effect aliases (`effects X = [...]`) were removed; declare concrete method effects directly in `! [...]`
- **Breaking:** broad namespace declarations such as `! [Http]` no longer satisfy child effects like `Http.get`
- `aver verify` now checks only declared `left => right` examples; coverage-style diagnostics moved to `aver check`
- `aver check` now reports coverage hints as warnings and no longer exits non-zero because of warnings alone
- `aver context` now defaults to `--depth auto --budget 10kb` instead of walking dependencies without a budget
- `aver context --json` stays human/LLM-oriented: compact signatures, short verify strings, omitted empty sections, and skipped long verify cases
- examples were reorganized into `core/`, `data/`, `formal/`, `modules/`, `services/`, and `apps/` under a shared `--module-root examples`, while standalone showcase apps now live under `projects/`

### Fixed
- `aver verify --deps` now verifies transitive dependencies
- exposed sum types and constructors now resolve correctly across module boundaries
- fully-qualified constructor patterns now work consistently in parsing, typechecking, exhaustiveness, and runtime matching
- `Result<Unit, String>` now accepts `Unit` cleanly and renders `Unit` consistently
- `unused exposes` diagnostics now resolve real symbol usage from AST and point at the module's `exposes` line
- Rust codegen now resolves module-qualified Aver calls/types without flattening sibling modules into one ambiguous Rust namespace
- Rust codegen now routes `Args.get()` through `aver-rt`
- deep `AverList` teardown and `append -> match` / `tail` paths in `aver-rt`, removing shared stack-overflow cliffs for both the interpreter and generated Rust
- `String.slice` semantics are now shared between the interpreter and `aver-rt`, including negative-index clamping

## 0.4.0

### Added
- `aver proof` as a dedicated Lean proof-export command
- `aver --version`
- docs for `Unit`, `main` returning `Result<Unit, String>`, and the `HttpServer.listen` / `listenWith` callback model

### Changed
- **Breaking:** `aver compile` now targets Rust only
- **Breaking:** Lean export moved from `aver compile -t lean` to `aver proof`
- **Breaking:** Lean CLI flags were renamed from `--lean-verify` to `--verify-mode`
- **Breaking:** match patterns now reject positional record destructuring such as `User(name, age)`; bind the record and use field access instead
- **Breaking:** constructor patterns must now be qualified (`Shape.Circle`, `Result.Ok`, `Option.None`) instead of bare `Circle` / `Some` / `None`
- CLI/docs were split around two separate backend intents: deployment (`compile`) and proof export (`proof`)

### Fixed
- Lean proof export now respects qualified cross-module calls such as `Examples.Json.toString` during function emission ordering
- Lean prelude now injects built-in `Header`, `HttpRequest`, `HttpResponse`, and `Tcp.Connection` support when generated code references those runtime types
- Lean `List.get` now preserves Aver's `Int` index semantics, including negative indices returning `Option.None`
- `examples/notepad/routes.av` proof export now builds successfully under Lean with `aver proof --verify-mode auto`
- parser/typechecker/interpreter specs were aligned with the qualified-constructor pattern rules and explicit record binding model

## 0.3.0

### Added
- `aver-rt` as a shared Rust runtime crate for transpiled projects and interpreter adapters
- `aver check --deps` to run contract checks for transitive `depends [...]` modules
- deterministic replay now walks nested recording directories
- recursion-first list runtime based on persistent `AverList`

### Changed
- **Breaking:** function bodies now use indentation only; `fn ... = expr` shorthand was removed
- **Breaking:** `|>` pipe operator was removed
- **Breaking:** `List` was simplified to a recursion-first API: `len`, `get`, `prepend`, `append`, `concat`, `reverse`, `contains`, `zip`
- **Breaking:** `List.push`, `List.head`, `List.tail`, `List.map`, `List.filter`, `List.fold`, `List.find`, `List.any`, and `List.flatMap` were removed
- **Breaking:** removed `aver decisions`; decision export now lives under `aver context --decisions-only`
- Rust transpilation now depends on the published `aver-rt` crate by default, with optional `AVER_RUNTIME_PATH` override for local runtime hacking
- `aver check` contract diagnostics now always include line numbers
- Decision `impacts` now accepts both validated symbols and semantic strings
- `input`, `expect`, `case`, `where`, `effect`, `service`, `needs` are no longer reserved keywords
- README and docs were restructured around quickstart, AI-first positioning, and the current CLI/runtime model

### Fixed
- old `= expr` syntax now fails consistently in parser and formatter with an actionable migration error
- `decisions/*.av` updated to conform to strict impacts validation and namespaced console usage
- renamed `examples/type_errors.av` to `examples/test_errors.av` with expanded checker diagnostics coverage

## 0.2.3

### Added
- `verify ... law ...` blocks with typed `given` domains (`a..b` ranges and explicit lists)
- Lean emission for `verify law`: named law theorems, sample theorems, and universal theorem skeletons
- Lean verify modes in CLI: `--lean-verify auto|sorry|theorem-skeleton`
- `--lean-proof-mode` fail-fast gate for proof-unsafe Lean transpilation paths
- Deeper match exhaustiveness analysis for nested and recursive patterns

### Changed
- `aver check` now treats missing verify on pure non-trivial functions as an error
- `verify law` skips regular case-level target-call heuristics used by `verify` case blocks
- Decision block fields (`date`, `author`, `reason`, `chosen`, `impacts`, etc.) are contextual (no longer globally reserved keywords)
- File-based commands require exactly one `module` declaration as the first top-level item

### Fixed
- Exhaustiveness checker stack overflow on recursive sum types with 2+ variants
- Empty `verify` blocks are rejected explicitly
- Rust codegen now fails fast on unresolved unknown types instead of panicking
- Lean codegen/parser ordering and mutual-recursion proof-mode integration issues
- Multiple Clippy-level borrow/style issues in builtin dispatcher paths

## 0.2.2

### Added
- Lean transpilation target in CLI: `aver compile -t lean`
- Lean codegen backend module structure (`src/codegen/lean/*`)
- Transpilation docs for Lean target in README and `docs/transpilation.md`

### Fixed
- Shared deterministic function ordering via call-graph SCC topo order (callee-before-caller) for codegen backends
- Lean forward-reference failures in emitted code (e.g. helper emitted after use)
- Lean prelude: avoid reserved keyword `from` in generated `String.slice`
- Lean `AverMap.set` now preserves key order when updating existing keys

## 0.2.1

### Added
- HttpServer: skip real TCP server in `--record` mode (return Unit immediately)
- Example recordings: console_demo, disk_demo, http_demo, notepad
- Recording snapshots persist on every effect (long-running processes safe)
- Module sum types exported via `exposes`
- Map literal syntax in examples

### Fixed
- Silent body discard when `=` appears after bindings in fn body
- Architecture decision output formatting
- README: clarify no Int/Float promotion, document missing Disk methods

## 0.2.0

### Added
- **Aver-to-Rust transpiler** (`aver transpile`) with full service support, module inlining, last-use analysis, and copy-type elision
- **LSP server** and **VSCode extension** with diagnostics, hover, go-to-definition, and module dependency caching
- **Deterministic replay** (`--record` / `aver replay`) — record effectful runs, replay without I/O
- **Static match exhaustiveness checking** at compile time
- **Compile-time variable resolution** — `Ident` → `Resolved(slot)` for O(1) lookup in fn bodies
- **Auto-memoization** of pure recursive functions with memo-safe arguments
- **Tail-call optimization** — self and mutual recursion without stack overflow
- **Typed bindings** — `name: Type = expr` with type checker validation
- **Tuple values** and tuple destructuring in match patterns
- **Map type** with `Map.get`, `Map.set`, `Map.keys`, `Map.values`, `Map.has`, `Map.remove`, `Map.size`
- **Map literal syntax** — `{"key" => value, ...}`
- **Record update syntax** — `Type.update(base, field = newVal)`
- **Multiline expressions** inside `()`, `[]`, `{}` delimiters
- **`Char` namespace** — `Char.toCode`, `Char.fromCode` for Unicode operations
- **`Byte` namespace** — `Byte.toHex`, `Byte.fromHex`
- **`String` additions** — `String.charAt`, `String.toLower`, `String.toUpper`
- **`List` additions** — `List.find`, `List.any`, `List.contains`, `List.zip`, `List.flatMap`
- **`Result.withDefault`**, **`Option.withDefault`**, **`Option.toResult`** combinators
- **Generic type inference** for Option/Result combinators
- **JSON parser** (`examples/json.av`) — full RFC 8259 with `\uXXXX` surrogate pairs, control char validation
- **Persistent Tcp connections** — `Tcp.connect`/`writeLine`/`readLine`/`close` with opaque `Tcp.Connection`
- **Redis RESP client** (`examples/redis.av`)
- **HttpServer service** — `HttpServer.listen` and `HttpServer.listenWith` (explicit context parameter)
- **Weather microservice example** — HttpServer + Http + Redis cache
- **Notepad REST API example** — multi-module CRUD app with Disk persistence
- **Agent challenge infrastructure** — prepare.sh, evaluate.sh, 3 challenges for AI agent testing
- **Interactive REPL** (`aver repl`) — stateful, multi-line, type-checked
- **`aver context`** — project context export for LLM consumption (Markdown + `--json`)
- **`aver decisions`** — generated architecture decision docs
- **`aver check --strict`** mode
- Human-readable parser error messages via `TokenKind` Display
- Editor support: VSCode extension + Sublime Text syntax highlighting
- Prepared for crates.io publication as `aver-lang`

### Changed
- **Breaking:** `List.get`, `List.head`, `List.tail` now return `Option` (was raw value / error)
- **Breaking:** `String.length` renamed to `String.len`
- **Breaking:** `val`/`var` keywords removed — all bindings are `name = expr`, always immutable
- **Breaking:** flat builtins removed (`print`, `len`, `map`, `filter`, `fold`, `str`, `int`, `abs`, etc.) — use namespaced equivalents (`Console.print`, `List.len`, `List.map`, ...)
- **Breaking:** `Ok`/`Err`/`Some`/`None` keywords removed — use `Result.Ok`, `Result.Err`, `Option.Some`, `Option.None`
- **Breaking:** `Any` removed from surface syntax — `Type::Unknown` is internal only
- **Breaking:** colon-only type annotations (`x: Int` not `x Int`)
- Renamed `Network` service to `Http`
- Env uses `Rc<Value>` with slot-based frames for resolved functions
- Closures removed — functions see globals at call time, not capture time
- Pipeline: parse → TCO transform → typecheck → resolve → interpret
- Verify warnings only for pure non-trivial functions
- File size warning raised to 250 lines

### Fixed
- Constructor rules enforced: named fields required for records, positional for sum variants
- Empty list binding rejection without type annotation
- Pipe RHS parsing tightened
- Tuple memo hashing
- `{{ }}` brace escapes in string highlighting
- Module function scope and memo collisions
- Match arm body error message after unexpected newline
- LSP UTF-16 position handling

## 0.1.0

Initial release of the Aver language interpreter.

### Core language
- Significant indentation (Python-like)
- Immutable bindings (`name = expr`)
- Functions with descriptions (`?`), effect declarations (`! [Effect]`), and type annotations
- `match` as the only branching construct (no `if`/`else` by design)
- No loops — `List.map`/`filter`/`fold` for iteration
- String interpolation with `{expr}`
- `|>` pipe operator
- `?` error propagation operator

### Type system
- Static type checker with named types, generics, `Result<T, E>`, `Option<T>`
- User-defined sum types and records
- Function types with effect annotations (`Fn(A) -> B ! [Effect]`)
- List pattern matching (`[]`, `[h, ..t]`)

### Built-in namespaces
- `Int`, `Float`, `String`, `List` — pure operations
- `Console` — print, error, warn, readLine (`! [Console]`)
- `Http` — GET, HEAD, DELETE, POST, PUT, PATCH (`! [Http]`)
- `Disk` — readText, writeText, appendText, exists, delete, deleteDir, listDir, makeDir (`! [Disk]`)
- `Tcp` — send, ping (`! [Tcp]`)

### Module system
- `module` blocks with `intent`, `depends`, `exposes`
- Dot-path imports (`depends [Examples.Foo]`)
- Named effect sets (`effects AppIO = [Console, Disk]`)

### Tooling
- `aver run` — execute programs
- `aver verify` — run verify blocks as tests
- `aver check` — static analysis (types, effects, style)
- `verify` blocks — declarative equality-based test cases
- `decision` blocks — architectural decisions as code
