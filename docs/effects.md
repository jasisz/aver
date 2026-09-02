# Aver — Effect Support Matrix

Every effect in the standard library has a typed signature on the source side and a runtime implementation on each backend. Some backends host every effect natively, others stub or short-circuit them when the underlying platform can't satisfy the contract. This document catalogues which backend supports which effect, and what "supported" actually means in each cell.

## Backends

| Compilation path | Output | Where it runs |
|---|---|---|
| **VM** (`aver run`) | bytecode interpreter | local CLI, dev loop |
| **Rust codegen** (`aver compile`) | Cargo project + native binary | server-side Rust deployments |
| **wasm-gc** (`--target wasm-gc`) | self-contained `.wasm` with engine GC + tail calls; per-instantiation helpers DCE'd to what the program calls. `--handler <fn>` synthesises a fetch-style HTTP wrapper; `--preset cloudflare --handler <fn>` packages it for Workers | Cloudflare Workers, modern browsers (Chrome 119+, Firefox 120+, Safari 18.2+), wasmtime 25+, Node 22+, Deno, Bun |
| **wasip2** (`--target wasip2`) | `.component.wasm` + sibling `.wit`. wasm-gc core module wrapped via `wit-component`; Aver effects lower directly to canonical-ABI WASI imports — no preview-1 adapter | wasmtime, Spin, NGINX Unit, wasmCloud, every other Component Model host |
| **Lean / Dafny proof export** (`aver proof`) | `.lean` / `.dfy` projects | offline verification |
| **Self-host** (`aver run --self-host`) | Aver-in-Aver bootstrap | development sanity, replay coverage |

The two WASM rows are independent compilation paths. `--target wasm-gc` covers JS hosts and embedded wasmtime via `aver/*` host imports; `--target wasip2` covers Component Model hosts via canonical-ABI WIT imports. The pre-2024 NaN-boxed `--target wasm` backend was dropped in 0.18 (Phase 1.8 of "Span") — modern hosts run the wasm-gc pipeline, standalone runtimes use wasip2.

`Lean` / `Dafny` columns describe the **proof export** treatment, not the runtime. Effects render as Oracle-style stubs with effect-list contracts and invariant lemmas; user-side theorems carry the per-effect bounds (`Random.int` in `[min, max]`, `Time.unixMs ≥ 0`, …) as hypotheses. See `docs/oracle.md` for the full Oracle model.

## Legend

| Symbol | Meaning |
|---|---|
| ✅ | Real implementation; the effect does what its source-side signature promises |
| ⚠️ | Partial / convention-based; documented caveat on the cell |
| ❌ | Stubbed; the call typechecks and runs but returns a documented sentinel (`Result.Err`, `Option.None`, `Unit`) — programs branch through the failure shape, not crash |
| n/a | Concept doesn't apply on this host (e.g. process signals in a fetch-style worker) |

## Matrix

The wasm-gc column covers the **default invocation** (`--target wasm-gc`, host wires `aver/*` imports). The HTTP-handler shape (`--handler <fn>`, `--preset cloudflare`) is the same column with `Request.*` / `Response.*` host imports replacing the corresponding effect cells when `aver_http_handle()` runs — see *Notes per backend* below. The wasip2 column is what `--target wasip2` produces today; cells marked `n/a` indicate the effect can't structurally land on WASI 0.2 and is rejected by the standard capability target manifest before code generation.

| Effect | VM | Rust | **wasm-gc** | **wasip2** | Lean | Dafny |
|---|---|---|---|---|---|---|
| `Args.get` | ✅ | ✅ | ✅ wasmtime / host wires | ✅ `wasi:cli/environment.get-arguments` | Oracle | Oracle |
| `Console.print` | ✅ | ✅ | ✅ wasmtime / `console.log` | ✅ `wasi:cli/stdout` + `blocking-write-and-flush` | Oracle | Oracle |
| `Console.error` | ✅ | ✅ | ✅ wasmtime / `console.error` | ✅ `wasi:cli/stderr` + `blocking-write-and-flush` | Oracle | Oracle |
| `Console.warn` | ✅ | ✅ | ✅ wasmtime / `console.warn` | ✅ `wasi:cli/stderr` (warn → stderr) | Oracle | Oracle |
| `Console.readLine` | ✅ | ✅ | ✅ wasmtime / host stdin | ✅ `wasi:cli/stdin` + `blocking-read` line loop | Oracle | Oracle |
| `Disk.readText` / `writeText` / `appendText` | ✅ | ✅ | ✅ wasmtime / ❌ in JS hosts | ✅ `wasi:filesystem/preopens` + `open-at` + via-stream | Oracle | Oracle |
| `Disk.readBytes` / `readBytesAt` / `writeBytes` / `appendBytes` | ✅ exact octets | ✅ exact octets | ✅ wasmtime / host wires | ✅ raw WASI streams; positional reads are bounded and EOF-short | Oracle | Oracle |
| `Disk.size` | ✅ | ✅ | ✅ wasmtime / host wires | ✅ descriptor `stat-at` metadata | Oracle | Oracle |
| `Disk.exists` / `delete` / `deleteDir` / `listDir` / `makeDir` | ✅ | ✅ | ✅ wasmtime / ❌ in JS hosts | ✅ `wasi:filesystem/types` (stat-at / unlink-file-at / etc.) | Oracle | Oracle |
| `Disk.sync` | ✅ `fsync` on a file or a directory; on Windows a directory sync is a no-op `Ok` (NTFS journals metadata) | ✅ same as VM | ✅ wasmtime, same as VM / ❌ in JS hosts | ✅ `open-at` + `[method]descriptor.sync` | Oracle | Oracle |
| `Env.get` | ✅ | ✅ | ✅ wasmtime / Workers `env` | ✅ `wasi:cli/environment.get-environment` + linear search | Oracle | Oracle |
| `Env.set` | ✅ | ✅ | ⚠️ wasmtime / no-op in JS | n/a — WASI 0.2 environment is read-only by design | Oracle | Oracle |
| `Http.get` / `head` / `delete` / `post` / `put` / `patch` | ✅ | ✅ | ✅ wasmtime / ✅ JSPI-suspending `fetch()` | ✅ `wasi:http/outgoing-handler` | Oracle | Oracle |
| `Random.int` | ✅ | ✅ | ✅ wasmtime / `Math.random` | ✅ `wasi:random/random.get-random-u64` + range scale | Oracle (`[min, max]` lemma) | Oracle |
| `Random.float` | ✅ | ✅ | ✅ wasmtime / `Math.random` | ✅ `wasi:random/random.get-random-u64` → `[0.0, 1.0)` | Oracle (`[0.0, 1.0)` lemma) | Oracle |
| `Process.stopRequested` | ✅ SIGINT/SIGTERM | ✅ SIGINT/SIGTERM | ✅ wasmtime SIGINT/SIGTERM / `false` in browser and Worker hosts | n/a — WASI 0.2 has no process-signal binding | Oracle (monotonic across calls) | Oracle (monotonic across calls) |
| `Tcp.*` | ✅ | ✅ | ✅ wasmtime / ❌ in JS hosts | ✅ `wasi:sockets`; `poll` uses input-stream subscriptions + `wasi:io/poll` | Oracle | Oracle |
| `Terminal.*` (12 methods) | ✅ via `crossterm` (`terminal` feature) | ✅ via `crossterm` | ✅ wasmtime / ❌ in JS hosts | n/a — WASI 0.2 has no terminal interface | Oracle | Oracle |
| `Time.now` (ISO string) | ✅ | ✅ | ✅ wasmtime / `new Date().toISOString()` | ✅ `wasi:clocks/wall-clock.now` + guest-side civil_from_days | Oracle | Oracle |
| `Time.unixMs` | ✅ | ✅ | ✅ wasmtime / `Date.now()` | ✅ `wasi:clocks/wall-clock.now` → ms | Oracle (`≥ 0` lemma) | Oracle |
| `Time.sleep` | ✅ | ✅ | ✅ wasmtime / ⚠️ blocks worker isolate | ✅ `wasi:clocks/monotonic-clock.subscribe-duration` + `wasi:io/poll.poll` | Oracle | Oracle |

`Print.value` / `Format.value` are no longer needed — `Console.print` / `error` / `warn` take `String` since 0.16, so stringification happens at the call site (interpolation `"{x}"` for primitives, a per-type render fn for compound shapes).

`Process.stopRequested` is cooperative polling, not asynchronous cancellation.
SIGINT/SIGTERM only flips a process-global flag from false to true; user code
chooses a safe point to observe it and perform cleanup. It does not interrupt a
blocking `Tcp.readLine`, `Console.readLine`, or `Time.sleep`, and it is not wired
into independent-product cancellation. Use bounded waits such as `Tcp.poll`
when a long-running loop must remain responsive to a stop request.

Incoming HTTP is a composition rather than an effect family. Native VM and
Rust programs run the ordinary Aver `HttpServer` module over the `Tcp.*` and
`Process.stopRequested` rows above; pure `HttpWire` owns HTTP/1.1 framing.
Fetch-style wasm-gc and `wasi:http/proxy` deployments instead select a
`Fn(HttpRequest) -> Http.Response` explicitly with `--handler`, because the host
already owns the listener.

## Notes per backend

### wasm-gc (`--target wasm-gc`)

The recommended target. Same `aver/*` import surface across every host that runs the binary — the difference is who supplies the implementation, and that's reflected in cells that read "wasmtime / `<JS thing>`":

- **`aver run --wasm-gc <file>`** — embedded wasmtime executor with the full effect surface (Args, Console incl. `readLine`, Time, Random, Process, Float math, Terminal, Disk, Env, Tcp, Http) wired against `aver_rt::*`. This is the cell on the left of the slash.
- **JS hosts (Cloudflare Workers, browsers, Deno, Bun, Node 22+)** — playground / `worker.js` template / custom embedder satisfies the `aver/*` imports. JS-host effects available are the cell on the right of the slash. Disk / raw TCP / Terminal don't have native JS equivalents and stub to `Result.Err` / `Option.None` / `Unit`.

The bridge also has one internal, non-effect import: `aver.provider_contract_violation(message, caller_fn_idx)`. The compiler includes it only when a literal call discharges a `Result` validation boundary. A conforming host reports the supplied provider `Err` text and returns; the guest immediately traps, so this diagnostic can never become a fallback value and is never part of record/replay.

Program-defined capabilities use a separate contract-derived
`aver:user/cap-…` import namespace and native wasm-gc values, including
`externref` resources and full `Int = ℤ`. The compiler exports the factories a
host needs to construct and inspect GC values. An external JavaScript host may
bind these imports directly; `aver run/replay --wasm-gc` instead adapts the
project's target-neutral Rust `ProviderBinding` through the same ABI. See
[`wasm-gc-custom-capabilities.md`](wasm-gc-custom-capabilities.md).

`--handler <fn>` (and the bundled `--preset cloudflare --handler <fn>`) generates an `aver_http_handle()` synthesised wrapper that consumes Request fields via dedicated host imports (`request_method`, `request_url`, `request_query`, `request_body`, `request_headers_load`) and writes the response via `response_text` / `response_set_header`. Inside the handler body, `Http.*` calls still go through the standard effect surface (✅ JSPI-suspending `fetch()` on Workers, ✅ wasmtime if you ever ran the same handler under `aver run --wasm-gc`).

In fetch-style deployment the host calls the selected `--handler <fn>`; there
is no listening loop or synthetic listener effect in the program.

### wasip2 (`--target wasip2`)

Component Model output landed in 0.18 "Span". Aver effects lower **directly** to canonical-ABI WASI imports — the wasm-gc backend emits a core module shaped to canonical-ABI conventions, the wrapper embeds a `component-type:wasi:cli/command` custom section via `wit-component::metadata`, and the resulting `.component.wasm` runs on every Component Model host (wasmtime, Spin, NGINX Unit, wasmCloud, …) without a preview-1 adapter. See [`docs/wasip2.md`](wasip2.md) for the full contract.

```
aver compile app.av --target wasip2 -o out
aver run app.av --wasip2  -- alpha beta   # embedded wasmtime + wasmtime-wasi
```

What lands today (0.18) vs. deferred:

- ✅ Console, Args.get, Env.get, Time, Random, Disk, outgoing Http, and all Tcp operations including binary reads/writes, `poll`, and `readSome`.
- ✅ Incoming HTTP through `--world wasi:http/proxy --handler <fn>`.
- n/a Process.stopRequested, Env.set, Terminal.* — structurally absent from WASI 0.2 (no process-signal binding, a read-only environment, and no terminal interface). Rejected at compile time.

Effect calls > 4 KB on `Console.*` / `Disk.write*` chunk through `blocking-write-and-flush` (wasmtime-wasi enforces a 4096-byte limit per call); the chunked-write loop lives in `emit_chunked_blocking_write` and is shared by both call sites. `Time.sleep` uses `subscribe-duration` + `poll` + `[resource-drop]pollable` (real wait, not busy-loop).
