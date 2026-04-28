# Aver — Effect Support Matrix

Every effect in the standard library has a typed signature on the source side and a runtime implementation on each backend. Some backends host every effect natively, others stub or short-circuit them when the underlying platform can't satisfy the contract. This document catalogues which backend × bridge combination supports which effect, and what "supported" actually means in each cell.

## Backends and bridges

Aver's surface compiles to multiple backends, and the WASM backend itself splits along a `--bridge` axis that picks the host ABI:

| Compilation path | Output | Where it runs |
|---|---|---|
| **VM** (`aver run`) | bytecode interpreter | local CLI, dev loop |
| **Rust codegen** (`aver compile`) | Cargo project + native binary | server-side Rust deployments |
| **WASM** `--target wasm` `--bridge fetch` | single `.wasm` + JS shim ABI | Cloudflare Workers, Deno, Bun, browser playgrounds, any JS host |
| **WASM** `--target wasm` `--bridge wasip1` | single `.wasm` + WASI preview-1 shim | standalone `wasmtime`, `wasmer`, browser shims |
| **WASM** `--target wasm` `--bridge none` | single `.wasm`, `aver/*` imports unresolved | custom embedders that wire imports themselves |
| **WASM** `--target edge-wasm` | thin `user.wasm` + imported `aver_runtime.wasm` | edge / browser deployments where the runtime is shared cross-program (Cloudflare CDN, etc.) |
| **Lean / Dafny proof export** (`aver proof`) | `.lean` / `.dfy` projects | offline verification |
| **Self-host** (`aver run --self-host`) | Aver-in-Aver bootstrap | development sanity, replay coverage |

`--target edge-wasm` runs over the same `--bridge` axis as `--target wasm` — every effect cell that's `✅` for `fetch` or `wasip1` works the same way under edge-wasm with that bridge. The two targets only differ in how `aver_runtime.*` is delivered (inlined via `wasm-merge` for `--target wasm`, imported as a separate module for `--target edge-wasm`).

`Lean` / `Dafny` columns describe the **proof export** treatment, not the runtime. Effects render as Oracle-style stubs with effect-list contracts and invariant lemmas; user-side theorems carry the per-effect bounds (`Random.int` in `[min, max]`, `Time.unixMs ≥ 0`, …) as hypotheses. See `docs/oracle.md` for the full Oracle model.

## Legend

| Symbol | Meaning |
|---|---|
| ✅ | Real implementation; the effect does what its source-side signature promises |
| ⚠️ | Partial / convention-based; documented caveat on the cell |
| ❌ | Stubbed; the call typechecks and runs but returns a documented sentinel (`Result.Err`, `Option.None`, `Unit`) — programs branch through the failure shape, not crash |
| n/a | Concept doesn't apply on this host (e.g. `HttpServer.listen` under `--bridge fetch` — Workers IS the server, the handler shape is the API) |

## Matrix

| Effect | VM | Rust | `--bridge fetch` | `--bridge wasip1` | `--bridge none` | Lean | Dafny |
|---|---|---|---|---|---|---|---|
| `Args.get` | ✅ | ✅ | ⚠️ from URL query | ✅ `wasi.args_get` | host wires | Oracle | Oracle |
| `Console.print` | ✅ | ✅ | ✅ `console.log` | ✅ `fd_write(1)` | host wires | Oracle | Oracle |
| `Console.error` | ✅ | ✅ | ✅ `console.error` | ✅ `fd_write(2)` | host wires | Oracle | Oracle |
| `Console.warn` | ✅ | ✅ | ✅ `console.warn` | ✅ `fd_write(2)` | host wires | Oracle | Oracle |
| `Console.readLine` | ✅ | ✅ | ⚠️ no stdin in CF | ✅ `fd_read(0)` | host wires | Oracle | Oracle |
| `Disk.readText` / `writeText` / `appendText` | ✅ | ✅ | ❌ no FS | ❌ stub (preview-2 ticket) | host wires | Oracle | Oracle |
| `Disk.exists` / `delete` / `deleteDir` / `listDir` / `makeDir` | ✅ | ✅ | ❌ no FS | ❌ stub | host wires | Oracle | Oracle |
| `Env.get` | ✅ | ✅ | ✅ Workers `env` binding | ✅ `wasi.environ_get` walker | host wires | Oracle | Oracle |
| `Env.set` | ✅ | ✅ | ⚠️ no-op (`env` frozen) | ⚠️ no-op (no `setenv` in preview 1) | host wires | Oracle | Oracle |
| `Http.get` / `head` / `delete` / `post` / `put` / `patch` | ✅ | ✅ | ✅ JSPI-suspending `fetch()` | ❌ `Result.Err` (lands as `--target wasip2`) | host wires | Oracle | Oracle |
| `HttpServer.listen` / `listenWith` | ✅ (`runtime-net`) | ✅ (`runtime-net`) | n/a — handler shape | ❌ stub (lands as `--target wasip2 --world wasi:http/proxy`) | host wires | Oracle | Oracle |
| `Random.int` | ✅ | ✅ | ✅ `Math.random` | ✅ `wasi.random_get` | host wires | Oracle (`[min, max]` lemma) | Oracle |
| `Random.float` | ✅ | ✅ | ✅ `Math.random` | ✅ `wasi.random_get` | host wires | Oracle (`[0.0, 1.0)` lemma) | Oracle |
| `Tcp.connect` / `send` / `ping` / `writeLine` / `readLine` / `close` | ✅ | ✅ | ❌ no raw TCP | ❌ stub (preview-2 ticket) | host wires | Oracle | Oracle |
| `Terminal.*` (12 methods) | ✅ via `crossterm` (`terminal` feature) | ✅ via `crossterm` | ❌ no terminal | ❌ no terminal | host wires | Oracle | Oracle |
| `Time.now` (ISO string) | ✅ | ✅ | ✅ `new Date().toISOString()` | ✅ `wasi.clock_time_get` | host wires | Oracle | Oracle |
| `Time.unixMs` | ✅ | ✅ | ✅ `Date.now()` | ✅ `wasi.clock_time_get` | host wires | Oracle (`≥ 0` lemma) | Oracle |
| `Time.sleep` | ✅ | ✅ | ⚠️ blocks worker isolate (use sparingly) | ✅ busy-wait via `clock_time_get` | host wires | Oracle | Oracle |

`Print.value` and `Format.value` are runtime helpers (host-side `console.log` formatting + string-interpolation backing) rather than user-facing effects; they're wired everywhere alongside `Console.print`.

## Notes per host

### `--bridge fetch` (JS hosts)

Targets Cloudflare Workers, Deno Deploy, Bun, browser playgrounds, and any embedder that supplies the Web Fetch API plus standard JS primitives. `Http.*` synchronously calls into a JSPI-suspending host import (`WebAssembly.Suspending` wraps the `await fetch(...)` shape, `WebAssembly.promising` wraps the exported `aver_http_handle`), so the wasm guest can cross the sync/async boundary cleanly. Hosts without JSPI (older Node, embedders predating the proposal) fall back to a sync stub returning `Result.Err`.

Filesystem and raw TCP effects don't have JS-host equivalents and stub to `❌`. `HttpServer.listen` is `n/a` because the worker IS the server — handler shape (`fn handler(req: HttpRequest) -> HttpResponse`) is the API the host calls per request.

### `--bridge wasip1` (WASI preview 1)

Targets standalone `wasmtime` / `wasmer` and any embedder satisfying `wasi_snapshot_preview1.*`. Preview 1 covers stdin/stdout/stderr, files, args, env, time, random — but explicitly does **not** ship sockets, an HTTP client, or a server runtime. Those effects stub to `Result.Err`.

The right path for HTTP under standalone wasm isn't another bridge — it's `--target wasip2` (component model output, `wasi:http/proxy` world, Aver's pure handler shape mapping 1:1 to `incoming-handler`). Tracked for 0.15+.

### `--bridge none`

The compiler emits the `aver/*` imports as unresolved. The consuming host wires them at instantiate time — `aver run --wasm`, the playground's JS shim, custom Rust embedders with `wasmtime`'s `Linker`, etc. Every effect is whatever the host says it is; the matrix shows `host wires` as a placeholder.

### Future: `--target wasip2` (Component Model)

Distinct from the existing `--bridge` axis. Component Model is a different compilation target (`.component.wasm` output, WIT worlds, host-owned resource handles), not a swap-out shim — so it gets its own `--target` rather than a `--bridge wasip2` flag that would conflate two compilation models.

```
--target wasip2 --world wasi:http/proxy   # default world for 0.15
--target wasip2 --world wasi:cli/command  # later
```

The effect matrix once `wasip2` ships:
- `Http.*` → ✅ via WIT-typed request/response/streams
- `HttpServer.listen` → ✅ via `wasi:http/proxy`'s `incoming-handler` (handler shape maps 1:1 from Aver's `fn handler(req) -> resp`)
- `Disk.*` → ✅ via `wasi:filesystem`
- `Tcp.*` → ✅ via `wasi:sockets`

That table fills in once the target lands; for now, `--target wasm --bridge fetch` covers the JS-host edge story end to end and `--target wasm --bridge wasip1` covers offline-CLI compute + files + env.
