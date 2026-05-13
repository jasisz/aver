# `--target wasip2` (Component Model)

> Status: Phase 1 of 0.18 "Span". This document is the contract. Anything not on this page is out of scope until the contract is updated and a decision block in `decisions/architecture.av` says otherwise.

`--target wasip2` produces a WebAssembly Component (`.component.wasm` plus a sibling `.wit`) that imports WASI 0.2 worlds **directly** — no preview-1 adapter, no compatibility bridge. Aver effects lower to WIT/WASI imports; Aver values stay private inside the core module; WIT/canonical ABI is the only thing the host sees. The component is intended for WASI 0.2 Component Model hosts such as wasmtime, Spin, NGINX Unit, wasmCloud, and Fermyon Cloud. Exact support depends on the world and interfaces used by the generated WIT — Component Model alone is not sufficient; the host also has to provide the specific interfaces the world declares.

This is not a general "export Aver as a WIT library" feature. In 0.18 the only public export shape is `wasi:cli/run` (the entry function the `wasi:cli/command` world requires). Arbitrary Aver functions are not exported as WIT interfaces. The component is something a host runs, not a typed library other components link against.

## Two targets, two jobs

| Target | Job | Hosts |
|---|---|---|
| `--target wasm-gc` | Portable core wasm with engine GC + tail calls. Self-contained binary, host wires `aver/*` imports. | Browsers (Chrome 119+, Firefox 120+, Safari 18.2+), Cloudflare Workers (via `--preset cloudflare --handler <fn>`), Node 22+, Deno, Bun, embedded wasmtime |
| `--target wasip2` | WASI 0.2 component whose public import/export surface is described by WIT. The wasm-gc emitter produces core imports/exports in canonical-ABI-compatible shapes; component-type metadata declares which WIT world they correspond to; `ComponentEncoder` builds the actual component boundary from the two. | wasmtime, Spin 3.x, NGINX Unit, wasmCloud, Fermyon Cloud — any host that takes a `.component.wasm` AND provides the world's interfaces |

Cloudflare Workers and browsers do not run components natively; they stay on `--target wasm-gc`. `--preset cloudflare` is a wasm-gc preset and stays that way.

## Why no preview-1 adapter

The preview-1 component adapter is the right tool for migrating existing preview-1 wasm modules into the Component Model — it preserves their original ABI and translates calls at the boundary. Aver does not need that. Aver effects are typed and declared in source (`! [Console.print, Time.unixMs]`); there is no preview-1 ABI to preserve. Routing through the adapter would just replicate one compatibility shim with another. Direct WIT lowering is the natural shape for a language whose effects already declare the host capabilities the component imports.

## Architecture

```
Aver effect call site
  └─► Aver-side glue
        Aver value (GC string / list / record / variant / option / result)
        is marshalled into the canonical ABI's core boundary representation:
        ptr+len, retptr, handle (i32), tag+payload, …

core wasm import / export
  └─► plain core wasm types and signatures
        i32 / i64 / f32 / f64 / refs as applicable
        canonical-ABI-compatible shapes

component-type metadata
  └─► describes which WIT world / interface those core signatures
        correspond to. Embedded as a `component-type:<world>` custom
        section in the core module via `wit-encoder`.

ComponentEncoder
  └─► builds the actual component boundary from core module + metadata.
        Produces `.component.wasm`. The host sees the WIT view; it never
        sees Aver's runtime object layout.
```

The wasm-gc emitter does **not** implement the Component Model boundary. It emits core imports/exports in canonical-ABI-compatible shapes and inserts Aver-side glue at effect call sites. Component-type metadata says what those core signatures mean in WIT. `wit_component::ComponentEncoder` does the actual lifting/lowering at component build time.

## Component contract

Eight properties every `--target wasip2` build must satisfy:

1. **Imports are declared effects only.** Every WIT import in the component is justified by at least one declared Aver effect (`! [...]`), and every declared effect either lowers to one or more WIT imports in the selected world or is rejected at compile time. A single Aver call may translate into several WIT calls in the generated glue (e.g., `Console.print` cache + stream write); a single WIT interface may serve many Aver effects (e.g., `wasi:io/streams` for both stdout writes and stdin reads). No silent capability creep, no host hooks beyond what the source asks for.
2. **Exports are the handler shape only.** A program with a `main` function exports `wasi:cli/run`; a program compiled with `--world wasi:http/proxy` (Phase 3 / 0.19) exports `wasi:http/incoming-handler`. No internal Aver functions, types, or runtime helpers leak out as public exports.
3. **All public ABI goes through WIT.** Anything that crosses the component boundary uses canonical WIT types: strings, lists, records, variants, results. No Aver-specific encoding.
4. **No Aver values cross the boundary.** Per-instantiation `Map<K, V>`, `List<T>`, `Vector<T>`, `Option<T>`, `Result<T, E>`, tuples, records, and variants stay inside the user core module. The canonical ABI for engine-GC types is still pre-proposal upstream; we do not encode anything that would break when it lands.
5. **Generated WIT is emitted next to the artifact.** `aver compile --target wasip2 -o out` produces `out/<name>.component.wasm` and `out/<name>.wit`. The WIT is human-readable and is the source of truth for what the component imports and exports — no hidden surface in custom sections.
6. **Component validates with `wasm-tools`.** `wasm-tools validate --features component-model out/<name>.component.wasm` exits zero on every artifact `aver compile --target wasip2` produces. Bench scenarios and example programs are gated on this in CI.
7. **WASI resources stay implementation-internal.** Stdout / stderr `output-stream` handles, filesystem descriptors, pollables, and similar resource handles may be cached and reused inside the per-effect glue. They are **not** exposed as Aver-level values. There is no `Resource<T>` / `Handle<T>` / `Stream<T>` type on the Aver surface in 0.18. Adding one is a deliberate language decision for 0.19+, not a side effect of WIT lowering.
8. **Filesystem access is preopen-scoped.** `Disk.*` paths resolve only against WASI preopened directories. Absolute paths and paths that escape preopens return `Result.Err("path not preopened")` (a *dynamic* host capability gap, distinct from compile-time rejects). The Aver source-level `Disk` API stays unchanged; the wasip2 lowering enforces the WASI capability model at the boundary.

## `aver run --wasip2 file.av`

Compiles the source to a `wasi:cli/command` component, instantiates it via embedded wasmtime, and runs the `wasi:cli/run` export:

- Effects are recorded at the **Aver call level**, above the WIT import boundary. Recordings are interchangeable with VM, wasm-gc, and self-host traces (same `recording.json` shape since 0.16.1).
- Diagnostics are Aver-shaped. Wasmtime trap messages translate through the same path that `aver run --wasm-gc` uses today; users see Aver source spans, not core-wasm offsets.
- No build cache. Compile is fast enough that adding a cache layer is not worth the cache-invalidation contract.
- `--record <dir>` / `--replay <recording.json>` are **not yet wired** for `--wasip2` and the flags are rejected at CLI time. Recording requires a separate plumbing pass against the canonical-ABI WASI imports; until that lands, use `aver run --wasm-gc --record` (recordings are interchangeable across backends). The earlier sentence about effects being recorded "at the Aver call level" describes the cross-backend recording shape, not what `--wasip2` itself accepts.

External hosts: `wasmtime run` for command components is the canonical path; `wasmtime serve` and other server-capable hosts (Spin's `wasi:http/proxy` runtime, NGINX Unit) target the HTTP/proxy world, which is Phase 3 / 0.19+ and out of 0.18 scope.

## `aver compile --target wasip2 -o out`

Produces:

```
out/
  <name>.component.wasm    -- the component
  <name>.wit               -- generated WIT, human-readable
```

Flags:

- `--world <world>` — which WIT world the component targets. Two values: `wasi:cli/command` (default — long-running process exporting `wasi:cli/run.run`) and `wasi:http/proxy` (HTTP server, exporting `wasi:http/incoming-handler.handle`; shipped in 0.19). The proxy world pairs with `--handler <fn>` (same flag the wasm-gc + Cloudflare path uses) — names the user fn with signature `Fn(HttpRequest) -> HttpResponse` that becomes the proxy handler. The compile path is purely flag-driven; `main`'s body can stay portable (`HttpServer.listen(port, handler)` runs the same source under `aver run` on the VM, lowers to a no-op when wasip2 proxy codegen takes over). Programs whose effects do not fit the chosen world fail at compile time with `target-effect-unsupported` pointing at the offending call.
- `--optimize {size,speed}` — **rejected** on `--target wasip2`. Upstream `wasm-opt` does not yet handle wasm-gc + Component Model bytes cleanly, so the flag is refused at the CLI rather than silently dropped. Use `--target wasm-gc` if you need post-pass size/speed optimization; we will wire it for wasip2 once the toolchain catches up.

The compiler does not shell out. WIT emission goes through `wit-encoder`; component-type metadata is encoded via `wit-component::metadata` and embedded as a custom section in the core module; the actual component wrap goes through `wit_component::ComponentEncoder`. Single binary, no toolchain to install on the user's machine.

## Effect mapping

Aver effects lower directly to WASI 0.2 imports. The mapping is fixed per effect; a single Aver call at the source level may translate into one or several WIT calls in the generated glue (e.g., a `Console.print` may cache the stdout `output-stream` resource handle once and call `wasi:io/streams.[method]write` per print).

| Aver effect | WIT import (the glue calls into) |
|---|---|
| `Args.get` | `wasi:cli/environment.get-arguments` |
| `Env.get` | `wasi:cli/environment.get-environment` |
| `Env.set` | **Compile-rejected** — WASI 0.2 environment is read-only by design (no host can ever satisfy a write). Same "cannot-ever-support" category as `Terminal.*`. |
| `Console.print` / `error` / `warn` | `wasi:cli/stdout.get-stdout` / `wasi:cli/stderr.get-stderr` (cached) + `wasi:io/streams.output-stream.[method]blocking-write-and-flush`. 0.18 uses blocking write-and-flush for command-component semantics and simple replayability — one `Console.*` call ⇒ at most one host-side flush, easy to record/replay deterministically. WASI output-streams are fundamentally non-blocking with a polling model; `blocking-write-and-flush` is a binding-level convenience helper that bundles `check-write` + `write` + `flush` + `subscribe`/`poll` into one call. Buffered stdout/stderr could land later as an optimisation, but the semantic unit stays the Aver `Console` call. |
| `Console.readLine` | `wasi:cli/stdin.get-stdin` (cached) + `wasi:io/streams.input-stream.[method]blocking-read` |
| `Disk.readText` / `writeText` / `appendText` / `exists` / `delete` / `deleteDir` / `listDir` / `makeDir` | `wasi:filesystem/preopens.get-directories` (cached) + `wasi:filesystem/types.[method]*`. Paths outside preopens return `Result.Err("path not preopened")` — capability model, contract point 8. |
| `Time.now` / `unixMs` | `wasi:clocks/wall-clock.now` (Time.now formats RFC3339 guest-side via Howard Hinnant's `civil_from_days`) |
| `Time.sleep` | `wasi:clocks/monotonic-clock.subscribe-duration` + `wasi:io/poll.poll` + `[resource-drop]pollable` (per-call pollable, real wait — not busy-loop) |
| `Random.int` / `float` | `wasi:random/random.get-random-u64` + Aver-side range scaling. This is the secure `wasi:random/random` interface (same contract as `get-random-bytes`, just returning 8 cryptographically-secure bytes packed into a u64); we deliberately do NOT use `wasi:random/insecure.get-insecure-random-u64`. If we later need finer byte-level control (e.g. for `Random.bytes(n)`), the switch to `get-random-bytes` is mechanical. |
| `Http.{get, head, delete, post, put, patch}` | `wasi:http/outgoing-handler.handle` + the future-incoming-response / incoming-response choreography (Phase 2 / 0.19 shipped). Method tag selects `outgoing-request.set-method`. Body-bearing verbs marshal a request body via `request.body` + `outgoing-body.write` + chunked `blocking-write-and-flush` + `outgoing-body.finish`. Headers (request and response) lower as `Map<String, List<String>>`; multi-valued field names preserve server emit order. `error-code` variant discriminants surface as per-variant `http: <name>` Err messages (39 cases). |
| `HttpServer.listen` | `wasi:http/incoming-handler.handle` export (Phase 3 / 0.19 shipped). Requires `--world wasi:http/proxy --handler <fn>`. The handler wrapper decodes the host-supplied incoming-request into an Aver `HttpRequest` (method via the 10-case variant, path-with-query split into path/query, headers iteration as `Map<String, List<String>>`, body via `incoming-body.stream` + drained `input-stream.blocking-read`), runs the user's `fn(HttpRequest) -> HttpResponse`, marshals the result into an outgoing-response (`outgoing-response` constructor + `set-status-code` + body via `outgoing-body.write` + chunked `blocking-write-and-flush` + `outgoing-body.finish`), and calls `response-outparam.set`. `Content-Length` is synthesised from the response body byte count. The `port` argument to `HttpServer.listen` in source is honoured by the VM but ignored by wasip2 codegen — the host's listener flag (`wasmtime serve --addr=:N` etc.) binds the socket. |
| `HttpServer.listenWith` | **Compile-rejected** — deferred one iteration; requires per-instance wasm-global context plumbing. |
| `Tcp.*` | **Compile-rejected** — out of 0.19 client scope (Phase 2.1 / 0.19+) |
| `Terminal.*` (12 methods) | **Compile-rejected** — WASI 0.2 has no raw/cooked-mode operations |

### Why `Terminal.*` / `Env.set` are rejected, not stubbed

The axis is **static target capability** vs **dynamic host capability**. `Result.Err` stubs are reserved for *dynamic* host capability gaps: missing preopen (`Disk.readText("/etc/passwd")` on a host that didn't preopen `/`), missing env var, denied permission. A target that *cannot ever* support an effect is a different category and gets a different shape — a compile-time `target-effect-unsupported` error.

In 0.18:

- **`Terminal.*`** — WASI 0.2 has `wasi:cli/terminal-input` and `terminal-output` as TTY signals, but no standardised raw/cooked-mode operations (`set-raw-mode`, `set-echo`, `get-window-size`). The capability is structurally absent.
- **`Env.set`** — WASI 0.2 environment is read-only. There is no host implementation that could ever satisfy a write. Silent no-op would be a trap: source declares "I set X" and the program runs as if it succeeded while the environment is unchanged.
- **`Tcp.*`, `HttpServer.listenWith`** — out of 0.19 scope by deliberate design (Phase 2.1 / `listenWith` deferred one iteration). Same compile-time `target-effect-unsupported` shape so the user sees one consistent error type rather than a mix of stubs and rejects.

(Earlier 0.18 betas grouped `Time.sleep` with the structural rejects on the assumption that the pollable model was out of scope. That was a scoping mistake — pollables can be wrapped *inside* a single helper without leaking to source. Phase 1.4c shipped `__rt_time_sleep` doing exactly that, so `Time.sleep` lowers natively now.)

Compile output for any of these:

```
error[target-effect-unsupported]:
  Terminal.readKey requires raw terminal input.
  --target wasip2 does not provide Terminal effects.
  Use:
    --target wasm-gc for browser/interactive terminal hosts
    aver run on VM for local terminal programs
  Or replace Terminal.* with Console.* / Args / stdin-compatible APIs.
```

## Phasing inside 0.18

| Phase | Scope | Status |
|---|---|---|
| 0 | Audit legacy coupling, wire `wit-component`/`wit-encoder` deps, prove the wrap pipeline | ✅ shipped |
| 1.0 / 1.1 | `--target wasip2` CLI plumbing, end-to-end pipeline for no-effect programs | ✅ shipped |
| 1.2 | `wasi:cli/stdout` + `wasi:io/streams` glue. `Console.print` / `error` / `warn` → stream write end-to-end | ✅ shipped |
| 1.3 | `wasi:cli/stdin` + `wasi:cli/environment`. `Console.readLine` / `Args.get` / `Env.get` | ✅ shipped |
| 1.4 | `wasi:clocks/wall-clock.now` for `Time.now` / `Time.unixMs`; `wasi:random` for `Random.*`; `wasi:clocks/monotonic-clock.subscribe-duration` + `wasi:io/poll.poll` for `Time.sleep`. | ✅ shipped |
| 1.5 | `wasi:filesystem`. All seven `Disk.*` methods (`exists` / `readText` / `writeText` / `appendText` / `delete` / `deleteDir` / `makeDir` / `listDir`). Paths resolve relative to the cached preopen. | ✅ shipped |
| 1.6 | Reject `Terminal.*` / `Env.set` at compile time as permanent (WASI 0.2 has no terminal interface; environment is read-only). `Http.*` / `Tcp.*` / `HttpServer.*` deferred to 0.19+. | ✅ shipped |
| 1.7 | `aver run --wasip2` (embedded wasmtime + `wasmtime-wasi`) with CWD preopened as `.` | ✅ shipped |
| 1.8 | Drop the legacy `--target wasm` backend (`src/codegen/wasm/`, `wasm-legacy` feature, `--bridge` flag, `wasm-runtime` subcommand, legacy bundling in `src/main/commands.rs`) | ✅ shipped |

## Out of scope for 0.18

- **Outgoing HTTP** (`wasi:http/outgoing-handler`) — Phase 2 / 0.19. Direct WIT lowering, same mechanism as Phase 1; just more types to marshal.
- **HTTP server** (`wasi:http/incoming-handler` / `wasi:http/proxy` world) — Phase 3 / 0.19 or 0.20. Different export shape (handler exposes WIT export, host calls in).
- **TCP sockets** (`wasi:sockets/tcp`) — Phase 2 / 0.19. Open question whether Aver wants long-lived socket handles as a language concept.
- **Resources / streams / pollables on the Aver surface** — implementation only in 0.18. If Aver grows a `Resource<T>` type, that is a deliberate language decision for 0.19+.
- **WASI 0.3** — async ABI / `future<T>` / `stream<T>` are real but not finalised. 0.2 hosts will be virtualised by 0.3 hosts per upstream commitment, so we lose nothing by waiting.
- **`wasi:keyvalue`, `wasi:logging`, `wasi:config`, `wasi:tls`, `wasi:blobstore`, `wasi:nn`** — none.
- **Cross-component shared runtime** — requires GC types in the canonical ABI; that proposal is upstream pre-proposal. Per-instantiation helpers stay inline.
- **`jco transpile` as a derived target** for browsers / Node — possible 0.19+ if there is concrete demand.

## References

- WASI 0.2 release tracker: https://github.com/WebAssembly/WASI/releases
- Component Model spec: https://github.com/WebAssembly/component-model
- `wit-component` crate: https://docs.rs/wit-component
- `wit-encoder` crate: https://docs.rs/wit-encoder
- `wit_component::metadata` (custom section encoding): https://docs.rs/wit-component/latest/wit_component/metadata/
- GC in canonical ABI (pre-proposal): https://github.com/WebAssembly/component-model/issues/525
