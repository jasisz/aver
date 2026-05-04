# Aver — Effect Support Matrix

Every effect in the standard library has a typed signature on the source side and a runtime implementation on each backend. Some backends host every effect natively, others stub or short-circuit them when the underlying platform can't satisfy the contract. This document catalogues which backend supports which effect, and what "supported" actually means in each cell.

## Backends

| Compilation path | Output | Where it runs |
|---|---|---|
| **VM** (`aver run`) | bytecode interpreter | local CLI, dev loop |
| **Rust codegen** (`aver compile`) | Cargo project + native binary | server-side Rust deployments |
| **wasm-gc** (`--target wasm-gc`, recommended) | self-contained `.wasm` with engine GC + tail calls; per-instantiation helpers DCE'd to what the program calls. `--handler <fn>` synthesises a fetch-style HTTP wrapper; `--preset cloudflare --handler <fn>` packages it for Workers | Cloudflare Workers, modern browsers (Chrome 119+, Firefox 120+, Safari 18.2+), wasmtime 25+, Node 22+, Deno, Bun |
| **wasm legacy** (`--target wasm`, pre-2024 hosts) | single bundled `.wasm` with NaN-boxed runtime inlined via `wasm-merge`. `--bridge fetch` for JS hosts, `--bridge wasip1` for standalone WASI preview 1, `--bridge none` for custom embedders | older runtimes that don't speak the GC + tail-call proposals |
| **Lean / Dafny proof export** (`aver proof`) | `.lean` / `.dfy` projects | offline verification |
| **Self-host** (`aver run --self-host`) | Aver-in-Aver bootstrap | development sanity, replay coverage |

The two WASM rows are independent compilation paths, not bridges of one path. `--target wasm-gc` does not accept `--bridge` — its HTTP shape is `--handler <fn>`, and standalone-runtime support arrives via the planned `--target wasip2` Component Model output (see *Future* at the bottom). `--target wasm` keeps the legacy `--bridge fetch | wasip1 | none` axis for hosts that can't run wasm-gc.

`Lean` / `Dafny` columns describe the **proof export** treatment, not the runtime. Effects render as Oracle-style stubs with effect-list contracts and invariant lemmas; user-side theorems carry the per-effect bounds (`Random.int` in `[min, max]`, `Time.unixMs ≥ 0`, …) as hypotheses. See `docs/oracle.md` for the full Oracle model.

## Legend

| Symbol | Meaning |
|---|---|
| ✅ | Real implementation; the effect does what its source-side signature promises |
| ⚠️ | Partial / convention-based; documented caveat on the cell |
| ❌ | Stubbed; the call typechecks and runs but returns a documented sentinel (`Result.Err`, `Option.None`, `Unit`) — programs branch through the failure shape, not crash |
| n/a | Concept doesn't apply on this host (e.g. `HttpServer.listen` under a fetch-style host — the worker IS the server, the handler shape is the API) |

## Matrix

The wasm-gc column covers the **default invocation** (`--target wasm-gc`, host wires `aver/*` imports). The HTTP-handler shape (`--handler <fn>`, `--preset cloudflare`) is the same column with `Request.*` / `Response.*` host imports replacing the corresponding effect cells when `aver_http_handle()` runs — see *Notes per backend* below.

| Effect | VM | Rust | **wasm-gc** | `wasm` `--bridge fetch` | `wasm` `--bridge wasip1` | `wasm` `--bridge none` | Lean | Dafny |
|---|---|---|---|---|---|---|---|---|
| `Args.get` | ✅ | ✅ | ✅ wasmtime / host wires | ⚠️ from URL query | ✅ `wasi.args_get` | host wires | Oracle | Oracle |
| `Console.print` | ✅ | ✅ | ✅ wasmtime / `console.log` | ✅ `console.log` | ✅ `fd_write(1)` | host wires | Oracle | Oracle |
| `Console.error` | ✅ | ✅ | ✅ wasmtime / `console.error` | ✅ `console.error` | ✅ `fd_write(2)` | host wires | Oracle | Oracle |
| `Console.warn` | ✅ | ✅ | ✅ wasmtime / `console.warn` | ✅ `console.warn` | ✅ `fd_write(2)` | host wires | Oracle | Oracle |
| `Console.readLine` | ✅ | ✅ | ✅ wasmtime / host stdin | ⚠️ no stdin in CF | ✅ `fd_read(0)` | host wires | Oracle | Oracle |
| `Disk.readText` / `writeText` / `appendText` | ✅ | ✅ | ✅ wasmtime / ❌ in JS hosts | ❌ no FS | ❌ stub (preview-2 ticket) | host wires | Oracle | Oracle |
| `Disk.exists` / `delete` / `deleteDir` / `listDir` / `makeDir` | ✅ | ✅ | ✅ wasmtime / ❌ in JS hosts | ❌ no FS | ❌ stub | host wires | Oracle | Oracle |
| `Env.get` | ✅ | ✅ | ✅ wasmtime / Workers `env` | ✅ Workers `env` binding | ✅ `wasi.environ_get` walker | host wires | Oracle | Oracle |
| `Env.set` | ✅ | ✅ | ⚠️ wasmtime / no-op in JS | ⚠️ no-op (`env` frozen) | ⚠️ no-op (no `setenv` in preview 1) | host wires | Oracle | Oracle |
| `Http.get` / `head` / `delete` / `post` / `put` / `patch` | ✅ | ✅ | ✅ wasmtime / ✅ JSPI-suspending `fetch()` | ✅ JSPI-suspending `fetch()` | ❌ `Result.Err` (lands as `--target wasip2`) | host wires | Oracle | Oracle |
| `HttpServer.listen` / `listenWith` | ✅ (`runtime-net`) | ✅ (`runtime-net`) | n/a — `--handler <fn>` shape | n/a — handler shape | ❌ stub (lands as `--target wasip2 --world wasi:http/proxy`) | host wires | Oracle | Oracle |
| `Random.int` | ✅ | ✅ | ✅ wasmtime / `Math.random` | ✅ `Math.random` | ✅ `wasi.random_get` | host wires | Oracle (`[min, max]` lemma) | Oracle |
| `Random.float` | ✅ | ✅ | ✅ wasmtime / `Math.random` | ✅ `Math.random` | ✅ `wasi.random_get` | host wires | Oracle (`[0.0, 1.0)` lemma) | Oracle |
| `Tcp.connect` / `send` / `ping` / `writeLine` / `readLine` / `close` | ✅ | ✅ | ✅ wasmtime / ❌ in JS hosts | ❌ no raw TCP | ❌ stub (preview-2 ticket) | host wires | Oracle | Oracle |
| `Terminal.*` (12 methods) | ✅ via `crossterm` (`terminal` feature) | ✅ via `crossterm` | ✅ wasmtime / ❌ in JS hosts | ❌ no terminal | ❌ no terminal | host wires | Oracle | Oracle |
| `Time.now` (ISO string) | ✅ | ✅ | ✅ wasmtime / `new Date().toISOString()` | ✅ `new Date().toISOString()` | ✅ `wasi.clock_time_get` | host wires | Oracle | Oracle |
| `Time.unixMs` | ✅ | ✅ | ✅ wasmtime / `Date.now()` | ✅ `Date.now()` | ✅ `wasi.clock_time_get` | host wires | Oracle (`≥ 0` lemma) | Oracle |
| `Time.sleep` | ✅ | ✅ | ✅ wasmtime / ⚠️ blocks worker isolate | ⚠️ blocks worker isolate (use sparingly) | ✅ busy-wait via `clock_time_get` | host wires | Oracle | Oracle |

`Print.value` / `Format.value` are no longer needed — `Console.print` / `error` / `warn` take `String` since 0.16, so stringification happens at the call site (interpolation `"{x}"` for primitives, a per-type render fn for compound shapes).

## Notes per backend

### wasm-gc (`--target wasm-gc`)

The recommended target. Same `aver/*` import surface across every host that runs the binary — the difference is who supplies the implementation, and that's reflected in cells that read "wasmtime / `<JS thing>`":

- **`aver run --wasm-gc <file>`** — embedded wasmtime executor with the full effect surface (Args, Console incl. `readLine`, Time, Random, Float math, Terminal, Disk, Env, Tcp, Http) wired against `aver_rt::*`. This is the cell on the left of the slash.
- **JS hosts (Cloudflare Workers, browsers, Deno, Bun, Node 22+)** — playground / `worker.js` template / custom embedder satisfies the `aver/*` imports. JS-host effects available are the cell on the right of the slash. Disk / raw TCP / Terminal don't have native JS equivalents and stub to `Result.Err` / `Option.None` / `Unit`.

`--handler <fn>` (and the bundled `--preset cloudflare --handler <fn>`) generates an `aver_http_handle()` synthesised wrapper that consumes Request fields via dedicated host imports (`request_method`, `request_url`, `request_query`, `request_body`, `request_headers_load`) and writes the response via `response_text` / `response_set_header`. Inside the handler body, `Http.*` calls still go through the standard effect surface (✅ JSPI-suspending `fetch()` on Workers, ✅ wasmtime if you ever ran the same handler under `aver run --wasm-gc`).

`HttpServer.listen` is n/a on wasm-gc — the deployment shape is "the host calls into your handler", which is exactly what `--handler <fn>` declares. There's no listening loop to write.

### wasm legacy (`--target wasm`)

For runtimes that don't speak wasm-gc + tail-call proposals. The legacy backend bundles a custom NaN-boxed runtime (alloc, GC, hashmap, string/list/vector ops) and picks one of three bridges:

- **`--bridge fetch`** — Cloudflare Workers, Deno Deploy, Bun, browser playgrounds, any embedder that supplies the Web Fetch API plus standard JS primitives. `Http.*` synchronously calls into a JSPI-suspending host import (`WebAssembly.Suspending` wraps the `await fetch(...)` shape, `WebAssembly.promising` wraps the exported `aver_http_handle`). Hosts without JSPI fall back to a sync stub returning `Result.Err`. Filesystem and raw TCP stub to `❌`. `HttpServer.listen` is `n/a` (worker IS the server).
- **`--bridge wasip1`** — standalone `wasmtime` / `wasmer`, anything satisfying `wasi_snapshot_preview1.*`. Covers stdin/stdout/stderr, files, args, env, time, random; preview 1 explicitly does **not** ship sockets, an HTTP client, or a server runtime, so those effects stub to `Result.Err`.
- **`--bridge none`** — `aver/*` imports stay unresolved. The consuming host wires them at instantiate time. Every effect is whatever the host says it is; the matrix shows `host wires`.

### Why no `--bridge wasip1` for wasm-gc

By design. wasm-gc + preview 1 would mean "modern engine GC + retrograde standalone runtime" — preview 1 is being deprecated upstream in favour of preview 2 / the Component Model, and porting the legacy `aver_to_wasi.wasm` shim to wasm-gc string types would re-implement an ABI we want to leave behind. The standalone-runtime story for wasm-gc is `--target wasip2` (planned, see below).

## Future: `--target wasip2` (Component Model)

Distinct from `--bridge`. Component Model is a different compilation target (`.component.wasm` output, WIT worlds, host-owned resource handles), not a swap-out shim — so it gets its own `--target` rather than another bridge.

```
aver compile app.av --target wasip2 --world wasi:http/proxy
aver compile app.av --target wasip2 --world wasi:cli/command
```

The matrix once `wasip2` ships, paired with wasm-gc:

- `Http.*` → ✅ via WIT-typed request/response/streams
- `HttpServer.listen` → ✅ via `wasi:http/proxy`'s `incoming-handler` (handler shape maps 1:1 from Aver's `fn handler(req) -> resp`)
- `Disk.*` → ✅ via `wasi:filesystem`
- `Tcp.*` → ✅ via `wasi:sockets`
- `Args` / `Env` / `Time` / `Random` / `Console` → ✅ via the corresponding WIT interfaces

That row fills in once the target lands. For now: `--target wasm-gc` covers Workers + browsers + Node + wasmtime end to end via `aver/*` imports, and `--target wasm --bridge wasip1` covers offline-CLI compute + files + env on legacy preview-1 runtimes.
