# `--target wasip2` (Component Model)

> Status: Phase 1 of 0.18 "Span". This document is the contract. Anything not on this page is out of scope until the contract is updated and a decision block in `decisions/architecture.av` says otherwise.

`--target wasip2` produces a WebAssembly Component (`.component.wasm` plus a sibling `.wit`) that imports WASI 0.2 worlds **directly** — no preview-1 adapter, no compatibility bridge. Aver effects lower to WIT/WASI imports; Aver values stay private inside the core module; WIT/canonical ABI is the only thing the host sees. The component runs on every Component Model host: `wasmtime run` / `wasmtime serve`, Spin, NGINX Unit, wasmCloud, Fermyon Cloud.

## Two targets, two jobs

| Target | Job | Hosts |
|---|---|---|
| `--target wasm-gc` | Portable core wasm with engine GC + tail calls. Self-contained binary, host wires `aver/*` imports. | Browsers (Chrome 119+, Firefox 120+, Safari 18.2+), Cloudflare Workers (via `--preset cloudflare --handler <fn>`), Node 22+, Deno, Bun, embedded wasmtime |
| `--target wasip2` | WASI 0.2 component with WIT-typed import surface. Imports satisfied by the host's standard WASI implementation. | wasmtime, Spin 3.x, NGINX Unit, wasmCloud, Fermyon Cloud, anything that takes a `.component.wasm` |

Cloudflare Workers and browsers do not run components natively; they stay on `--target wasm-gc`. `--preset cloudflare` is a wasm-gc preset and stays that way.

## Why no preview-1 adapter

The preview-1 component adapter is the right tool for migrating existing preview-1 wasm modules into the Component Model — it preserves their original ABI and translates calls at the boundary. Aver does not need that. Aver effects are typed and declared in source (`! [Console.print, Time.unixMs]`); there is no preview-1 ABI to preserve. Routing through the adapter would replace the legacy `--bridge wasip1` shape with another compatibility bridge — exactly the architecture 0.18 is removing. Direct WIT lowering is the natural shape for a language whose effects already match WIT semantics.

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

Seven properties every `--target wasip2` build must satisfy:

1. **Imports are declared effects only.** Every WIT import the component declares maps to an effect that appears in the program's `! [...]` lists, and every declared effect maps to a WIT import. No silent capability creep, no host hooks beyond what the source asks for.
2. **Exports are the handler shape only.** A program with a `main` function exports `wasi:cli/run`; a program compiled with `--world wasi:http/proxy` (Phase 3 / 0.19) exports `wasi:http/incoming-handler`. No internal Aver functions, types, or runtime helpers leak out as public exports.
3. **All public ABI goes through WIT.** Anything that crosses the component boundary uses canonical WIT types: strings, lists, records, variants, results. No Aver-specific encoding.
4. **No Aver values cross the boundary.** Per-instantiation `Map<K, V>`, `List<T>`, `Vector<T>`, `Option<T>`, `Result<T, E>`, tuples, records, and variants stay inside the user core module. The canonical ABI for engine-GC types is still pre-proposal upstream; we do not encode anything that would break when it lands.
5. **Generated WIT is emitted next to the artifact.** `aver compile --target wasip2 -o out` produces `out/<name>.component.wasm` and `out/<name>.wit`. The WIT is human-readable and is the source of truth for what the component imports and exports — no hidden surface in custom sections.
6. **Component validates with `wasm-tools`.** `wasm-tools validate --features component-model out/<name>.component.wasm` exits zero on every artifact `aver compile --target wasip2` produces. Bench scenarios and example programs are gated on this in CI.
7. **WASI resources stay implementation-internal.** Stdout / stderr `output-stream` handles, filesystem descriptors, pollables, and similar resource handles may be cached and reused inside the per-effect glue. They are **not** exposed as Aver-level values. There is no `Resource<T>` / `Handle<T>` / `Stream<T>` type on the Aver surface in 0.18. Adding one is a deliberate language decision for 0.19+, not a side effect of WIT lowering.

## `aver run --wasip2 file.av`

Compiles the source to a component, instantiates it via embedded wasmtime, and runs the `wasi:cli/run` export:

- Effects are recorded at the **Aver call level**, above the WIT import boundary. Recordings are interchangeable with VM, wasm-gc, and self-host traces (same `recording.json` shape since 0.16.1).
- Diagnostics are Aver-shaped. Wasmtime trap messages translate through the same path that `aver run --wasm-gc` uses today; users see Aver source spans, not core-wasm offsets.
- No build cache. Compile is fast enough that adding a cache layer is not worth the cache-invalidation contract.
- `--record <dir>` and `--replay <recording.json>` work on Phase 1 effects (Console / Args / Env / Time / Random / basic Disk) in 0.18.

## `aver compile --target wasip2 -o out`

Produces:

```
out/
  <name>.component.wasm    -- the component
  <name>.wit               -- generated WIT, human-readable
```

Flags:

- `--world <world>` — which WIT world the component targets. Default `wasi:cli/command`. Other accepted values in 0.18: `wasi:http/proxy` is reserved for Phase 3 / 0.19 and rejected at compile time. Programs whose effects do not fit the chosen world fail at compile time with `target-effect-unsupported` pointing at the offending call.
- `--optimize {size,speed}` — passes through to `wasm-opt` on the inner core module before component wrapping. Same flag as `--target wasm-gc`.

The compiler does not shell out. WIT emission goes through `wit-encoder`; component-type metadata is encoded via `wit-component::metadata` and embedded as a custom section in the core module; the actual component wrap goes through `wit_component::ComponentEncoder`. Single binary, no toolchain to install on the user's machine.

## Effect mapping

Aver effects lower directly to WASI 0.2 imports. The mapping is fixed per effect; a single Aver call at the source level may translate into one or several WIT calls in the generated glue (e.g., a `Console.print` may cache the stdout `output-stream` resource handle once and call `wasi:io/streams.[method]write` per print).

| Aver effect | WIT import (the glue calls into) |
|---|---|
| `Args.get` | `wasi:cli/environment.get-arguments` |
| `Env.get` / `Env.set` | `wasi:cli/environment.get-environment` (`set` is no-op on WASI 0.2 — environment is read-only) |
| `Console.print` / `error` / `warn` | `wasi:cli/stdout.get-stdout` (cached) + `wasi:io/streams.[method]write` |
| `Console.readLine` | `wasi:cli/stdin.get-stdin` (cached) + `wasi:io/streams.[method]blocking-read` |
| `Disk.readText` / `writeText` / `appendText` / `exists` / `delete` / `deleteDir` / `listDir` / `makeDir` | `wasi:filesystem/preopens.get-directories` (cached) + `wasi:filesystem/types.[method]*` |
| `Time.now` / `unixMs` | `wasi:clocks/wall-clock.now` |
| `Time.sleep` | `wasi:clocks/monotonic-clock.now` + busy-wait (`subscribe-duration` is async/poll, out of 0.18 scope) |
| `Random.int` / `float` | `wasi:random/random.get-random-bytes` + Aver-side decode |
| `Http.*` | **Compile-rejected** — out of 0.18 scope (Phase 2 / 0.19) |
| `HttpServer.listen` / `listenWith` | **Compile-rejected** — out of 0.18 scope (Phase 3 / 0.19) |
| `Tcp.*` | **Compile-rejected** — out of 0.18 scope (Phase 2 / 0.19) |
| `Terminal.*` (12 methods) | **Compile-rejected** — WASI 0.2 has no raw/cooked-mode operations |

### Why `Terminal.*` is rejected, not stubbed

WASI 0.2 has `wasi:cli/terminal-input` and `terminal-output` as TTY signals, but no standardised raw/cooked-mode operations (`set-raw-mode`, `set-echo`, `get-window-size`). A program that declares `! [Terminal.readKey]` is statically incompatible with the wasip2 target — that is a target-level error, not a runtime host failure. Compile fails with:

```
error[target-effect-unsupported]:
  Terminal.readKey requires raw terminal input.
  --target wasip2 does not provide Terminal effects.
  Use:
    --target wasm-gc for browser/interactive terminal hosts
    aver run on VM for local terminal programs
  Or replace Terminal.* with Console.* / Args / stdin-compatible APIs.
```

`Result.Err` stubs are reserved for *dynamic* host capability gaps: missing preopen, missing env var, denied permission. A target that *cannot ever* support an effect is a different category and gets a different shape. Same rule applies to HTTP/sockets in 0.18: they are out of scope by design, so the compiler rejects them rather than stubbing.

## Phasing inside 0.18

| Phase | Scope | Status target |
|---|---|---|
| 0 | Audit legacy coupling, wire `wit-component`/`wit-encoder` deps, prove the wrap pipeline | Foundation |
| 1.0 / 1.1 | `--target wasip2` CLI plumbing, end-to-end pipeline for no-effect programs | 0.18 core (done) |
| 1.2 | `wasi:cli/stdout` + `wasi:io/streams` glue. `Console.print` → stream write end-to-end | 0.18 core |
| 1.3 | `wasi:cli/stdin` + `wasi:cli/environment`. `Console.readLine` / `Args.get` / `Env.get` | 0.18 core |
| 1.4 | `wasi:clocks` + `wasi:random`. `Time.*` / `Random.*` | 0.18 core |
| 1.5 | `wasi:filesystem`. Basic `Disk.*` (read/write/exists/delete/listDir/makeDir) | 0.18 core |
| 1.6 | Reject `Terminal` / `Http` / `Tcp` / `HttpServer` at compile time with `target-effect-unsupported` | 0.18 core |
| 1.7 | `aver run --wasip2` (embedded wasmtime + `wasmtime-wasi`) | 0.18 core |

After Phase 1 lands green and the effect matrix in `docs/effects.md` has no "maybe this works" cells for 0.18 scope, the legacy `--target wasm` backend is deleted: `src/codegen/wasm/`, the `wasm-legacy` Cargo feature, the `--bridge` flag, the `Bridge` enum, the `wasm-runtime` subcommand, and the legacy bundling code in `src/main/commands.rs`. See decision `DropLegacyNanBoxedWasm` in `decisions/architecture.av`.

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
