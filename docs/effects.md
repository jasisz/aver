# Aver — Effect Support Matrix

Every standard library effect has a typed signature in source and a runtime implementation on each backend. Some backends run every effect natively. Others stub or short-circuit an effect when the platform can't meet its contract. This page lists which backend supports which effect, and what "supported" means in each cell.

## Backends

| Compilation path | Output | Where it runs |
|---|---|---|
| **VM** (`aver run`) | bytecode interpreter | local CLI, dev loop |
| **Rust codegen** (`aver compile`) | Cargo project + native binary | server-side Rust deployments |
| **wasm-gc** (`--target wasm-gc`) | self-contained `.wasm` with engine GC + tail calls; per-instantiation helpers are DCE'd down to what the program calls. `--handler <fn>` synthesises a fetch-style HTTP wrapper; `--preset cloudflare --handler <fn>` packages it for Workers | Cloudflare Workers, modern browsers (Chrome 119+, Firefox 120+, Safari 18.2+), wasmtime 25+, Node 22+, Deno, Bun |
| **wasip2** (`--target wasip2`) | `.component.wasm` + sibling `.wit`. wasm-gc core module wrapped via `wit-component`; Aver effects lower directly to canonical-ABI WASI imports, with no preview-1 adapter | wasmtime, Spin, NGINX Unit, wasmCloud, every other Component Model host |
| **Lean / Dafny proof export** (`aver proof`) | `.lean` / `.dfy` projects | offline verification |
| **Self-host** (`aver run --self-host`) | Aver-in-Aver bootstrap | development sanity, replay coverage |

The two WASM rows are separate compilation paths. `--target wasm-gc` serves JS hosts and embedded wasmtime through `aver/*` host imports. `--target wasip2` serves Component Model hosts through canonical-ABI WIT imports. The pre-2024 NaN-boxed `--target wasm` backend was dropped in 0.18 (Phase 1.8 of "Span"). Modern hosts run the wasm-gc pipeline and standalone runtimes use wasip2.

The `Lean` / `Dafny` columns describe how **proof export** treats each effect. They do not describe runtime behavior. Effects become Oracle-style stubs with effect-list contracts and invariant lemmas, and user theorems take the per-effect bounds (`Random.int` in `[min, max]`, `Time.unixMs ≥ 0`, …) as hypotheses. The full Oracle model is in `docs/oracle.md`.

## Legend

| Symbol | Meaning |
|---|---|
| ✅ | Real implementation; the effect does what its source-side signature promises |
| ⚠️ | Partial / convention-based; documented caveat on the cell |
| ❌ | Stubbed. The call typechecks and runs but returns a documented sentinel (`Result.Err`, `Option.None`, `Unit`), so the program takes its failure branch instead of crashing |
| n/a | Concept doesn't apply on this host (e.g. process signals in a fetch-style worker) |

## Matrix

The wasm-gc column covers the **default invocation** (`--target wasm-gc`, with the host wiring the `aver/*` imports). The HTTP-handler shape (`--handler <fn>`, `--preset cloudflare`) uses the same column, except that `Request.*` / `Response.*` host imports replace the matching effect cells while `aver_http_handle()` runs. *Notes per backend* below has the details. The wasip2 column is what `--target wasip2` produces today. An `n/a` cell means WASI 0.2 has no place for that effect, and the standard capability target manifest rejects it before code generation.

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
| `Env.set` | ✅ | ✅ | ⚠️ wasmtime / no-op in JS | n/a: WASI 0.2 environment is read-only by design | Oracle | Oracle |
| `Http.get` / `head` / `delete` / `post` / `put` / `patch` | ✅ | ✅ | ✅ wasmtime / ✅ JSPI-suspending `fetch()` | ✅ `wasi:http/outgoing-handler` | Oracle | Oracle |
| `Random.int` | ✅ | ✅ | ✅ wasmtime / `Math.random` | ✅ `wasi:random/random.get-random-u64` + range scale | Oracle (`[min, max]` lemma) | Oracle |
| `Random.float` | ✅ | ✅ | ✅ wasmtime / `Math.random` | ✅ `wasi:random/random.get-random-u64` → `[0.0, 1.0)` | Oracle (`[0.0, 1.0)` lemma) | Oracle |
| `Process.stopRequested` | ✅ SIGINT/SIGTERM | ✅ SIGINT/SIGTERM | ✅ wasmtime SIGINT/SIGTERM / `false` in browser and Worker hosts | n/a: WASI 0.2 has no process-signal binding | Oracle (monotonic across calls) | Oracle (monotonic across calls) |
| `Tcp.connect` / `close` / `writeLine` / `writeBytes` / `writeNow` / `readLine` / `readBytes` / `readSome` / `readNow` / `poll` / `send` / `sendBytes` / `ping` | ✅ | ✅ | ✅ wasmtime / ❌ in JS hosts | ✅ `wasi:sockets`; `poll` uses input-stream and output-stream subscriptions + `wasi:io/poll`; `readNow` / `writeNow` use the non-blocking `read` / `check-write` + `write` + `flush` stream methods | Oracle | Oracle |
| `Tcp.beginConnect` / `dialled` / `listen` / `accept` / `peerAddress` / `closeDial` / `closeListener` | ✅ | ✅ | ✅ wasmtime / ❌ in JS hosts | n/a: this target binds no dial, listener, or peer-address socket resource. Rejected at compile time | Oracle | Oracle |
| `Terminal.*` (12 methods) | ✅ via `crossterm` (`terminal` feature) | ✅ via `crossterm` | ✅ wasmtime / ❌ in JS hosts | n/a: WASI 0.2 has no terminal interface | Oracle | Oracle |
| `Time.now` (ISO string) | ✅ | ✅ | ✅ wasmtime / `new Date().toISOString()` | ✅ `wasi:clocks/wall-clock.now` + guest-side civil_from_days | Oracle | Oracle |
| `Time.unixMs` | ✅ | ✅ | ✅ wasmtime / `Date.now()` | ✅ `wasi:clocks/wall-clock.now` → ms | Oracle (`≥ 0` lemma) | Oracle |
| `Time.sleep` | ✅ | ✅ | ✅ wasmtime / ⚠️ blocks worker isolate | ✅ `wasi:clocks/monotonic-clock.subscribe-duration` + `wasi:io/poll.poll` | Oracle | Oracle |

`Print.value` / `Format.value` are no longer needed. Since 0.16 `Console.print` / `error` / `warn` take a `String`, so the call site does the stringifying (interpolation `"{x}"` for primitives, a per-type render fn for compound shapes).

`Process.stopRequested` is cooperative polling. SIGINT/SIGTERM only flips a process-global flag from false to true, and user code picks a safe point to read it and clean up. Nothing is cancelled asynchronously. It does not interrupt a blocking `Tcp.readLine`, `Console.readLine`, or `Time.sleep`, and it is not wired into independent-product cancellation. A long-running loop that must react to a stop request should use bounded waits such as `Tcp.poll`.

Incoming HTTP has no effect family of its own. It is built from other pieces. Native VM and Rust programs run the ordinary Aver `HttpServer` module over the `Tcp.*` and `Process.stopRequested` rows above, and pure `HttpWire` does the HTTP/1.1 framing. Fetch-style wasm-gc and `wasi:http/proxy` deployments pick a `Fn(HttpRequest) -> Http.Response` explicitly with `--handler` instead, because there the host already owns the listener.

## Notes per backend

### wasm-gc (`--target wasm-gc`)

This is the recommended target. Every host that runs the binary sees the same `aver/*` import surface. Hosts differ only in who supplies the implementation, which is why some cells read "wasmtime / `<JS thing>`":

- **`aver run --wasm-gc <file>`**: an embedded wasmtime executor with the full effect surface (Args, Console incl. `readLine`, Time, Random, Process, Float math, Terminal, Disk, Env, Tcp, Http) wired to `aver_rt::*`. This is the left side of the slash.
- **JS hosts (Cloudflare Workers, browsers, Deno, Bun, Node 22+)**: the playground, the `worker.js` template or a custom embedder supplies the `aver/*` imports. What a JS host offers is the right side of the slash. Disk, raw TCP and Terminal have no native JS equivalent and stub to `Result.Err` / `Option.None` / `Unit`.

The bridge has one more import, internal and not an effect: `aver.provider_contract_violation(message, caller_fn_idx)`. The compiler adds it only when a literal call discharges a `Result` validation boundary. A conforming host reports the provider's `Err` text and returns, and the guest traps right away. The diagnostic can therefore never turn into a fallback value, and it is never part of record/replay.

Program-defined capabilities use their own `aver:user/cap-…` import namespace, derived from the contract, and native wasm-gc values, including `externref` resources and full `Int = ℤ`. The compiler exports the factories a host needs to build and inspect GC values. An external JavaScript host can bind these imports directly. `aver run/replay --wasm-gc` instead adapts the project's target-neutral Rust `ProviderBinding` through the same ABI. See [`wasm-gc-custom-capabilities.md`](wasm-gc-custom-capabilities.md).

`--handler <fn>` (and the bundled `--preset cloudflare --handler <fn>`) generates a wrapper, `aver_http_handle()`. It reads Request fields through dedicated host imports (`request_method`, `request_url`, `request_query`, `request_body`, `request_headers_load`) and writes the response through `response_text` / `response_set_header`. Inside the handler body, `Http.*` calls still use the standard effect surface (✅ JSPI-suspending `fetch()` on Workers, ✅ wasmtime if you run the same handler under `aver run --wasm-gc`).

In a fetch-style deployment the host calls the selected `--handler <fn>`. The program has no listening loop and no synthetic listener effect.

### wasip2 (`--target wasip2`)

Component Model output arrived in 0.18 "Span". Aver effects lower **directly** to canonical-ABI WASI imports. The wasm-gc backend emits a core module that follows canonical-ABI conventions, the wrapper embeds a `component-type:wasi:cli/command` custom section via `wit-component::metadata`, and the resulting `.component.wasm` runs on any Component Model host (wasmtime, Spin, NGINX Unit, wasmCloud, …) without a preview-1 adapter. The full contract is in [`docs/wasip2.md`](wasip2.md).

```
aver compile app.av --target wasip2 -o out
aver run app.av --wasip2  -- alpha beta   # embedded wasmtime + wasmtime-wasi
```

What works today (0.18) and what is deferred:

- ✅ Console, Args.get, Env.get, Time, Random, Disk, outgoing Http, and the connected half of Tcp (`connect`, `close`, `writeLine`, `writeBytes`, `writeNow`, `readLine`, `readBytes`, `readSome`, `readNow`, `poll`, `send`, `sendBytes`, `ping`). That includes binary reads and writes, the two non-blocking primitives, and `Sending` write-readiness polling.
- ✅ Incoming HTTP through `--world wasi:http/proxy --handler <fn>`.
- n/a Process.stopRequested, Env.set, Terminal.*: WASI 0.2 has nowhere to put them (no process-signal binding, a read-only environment, no terminal interface). Rejected at compile time.
- n/a Tcp.beginConnect, dialled, listen, accept, peerAddress, closeDial, closeListener: this target binds no dial, listener, or peer-address socket resource. A program that listens, or dials without blocking, should use `wasm-gc`, generated Rust, or the VM. Rejected at compile time.

`Console.*` / `Disk.write*` calls over 4 KB are split into chunks through `blocking-write-and-flush`, because wasmtime-wasi allows at most 4096 bytes per call. Both call sites share the chunked-write loop in `emit_chunked_blocking_write`. `Time.sleep` uses `subscribe-duration` + `poll` + `[resource-drop]pollable`, so it really waits and does not busy-loop.
