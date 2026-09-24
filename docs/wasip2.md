# `--target wasip2` (Component Model)

> Status: Phase 1 of 0.18 "Span". This document is the contract. Anything not on this page is out of scope until the contract is updated and a decision block in `decisions/architecture.av` says otherwise.

`--target wasip2` produces a WebAssembly Component (`.component.wasm` plus a sibling `.wit`) that imports WASI 0.2 worlds **directly**, with no preview-1 adapter and no compatibility bridge. Aver effects lower to WIT/WASI imports. Aver values stay private inside the core module, and the host sees only WIT and the canonical ABI. The component is meant for WASI 0.2 Component Model hosts such as wasmtime, Spin, NGINX Unit, wasmCloud and Fermyon Cloud. Exact support depends on the world and interfaces the generated WIT uses. Component Model support alone is not enough: the host also has to provide the specific interfaces the world declares.

In 0.18 the only public export shape is `wasi:cli/run` (the entry function the `wasi:cli/command` world requires). Arbitrary Aver functions are not exported as WIT interfaces, so this is no general way to export Aver as a WIT library. A host runs the component. Other components do not link against it as a typed library.

## Two targets, two jobs

| Target | Job | Hosts |
|---|---|---|
| `--target wasm-gc` | Portable core wasm with engine GC + tail calls. Self-contained binary; the host wires the `aver/*` imports. | Browsers (Chrome 119+, Firefox 120+, Safari 18.2+), Cloudflare Workers (via `--preset cloudflare --handler <fn>`), Node 22+, Deno, Bun, embedded wasmtime |
| `--target wasip2` | WASI 0.2 component whose public import/export surface is described by WIT. The wasm-gc emitter produces core imports/exports in canonical-ABI-compatible shapes, component-type metadata declares which WIT world they correspond to, and `ComponentEncoder` builds the actual component boundary from the two. | wasmtime, Spin 3.x, NGINX Unit, wasmCloud, Fermyon Cloud: any host that takes a `.component.wasm` AND provides the world's interfaces |

Cloudflare Workers and browsers do not run components natively, so they stay on `--target wasm-gc`. `--preset cloudflare` is a wasm-gc preset and will stay one.

## Why no preview-1 adapter

The preview-1 component adapter is the right tool for moving existing preview-1 wasm modules into the Component Model. It keeps their original ABI and translates calls at the boundary. Aver does not need that. Aver effects are typed and declared in source (`! [Console.print, Time.unixMs]`), so there is no preview-1 ABI to preserve, and going through the adapter would only replace one compatibility shim with another. The effects already declare the host capabilities the component imports, so Aver lowers them to WIT directly.

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

The wasm-gc emitter does **not** implement the Component Model boundary. It emits core imports/exports in canonical-ABI-compatible shapes and inserts Aver-side glue at effect call sites. Component-type metadata says what those core signatures mean in WIT. `wit_component::ComponentEncoder` does the actual lifting and lowering when the component is built.

## Artifact certificates

`aver compile app.av --target wasip2 --certify -o out/` emits `out/cert/` beside the component. The package binds the hash of the full delivered `app.component.wasm`. A declared prefix/core/suffix envelope identifies the exact embedded core bytes that the existing Wasm certificate wall consumes. The verifier splits only at those declared lengths and checks byte equality. It does not parse or walk the component to find the core module.

The wall picks a finite import registry from the manifest target. For `wasip2`, that registry holds the exact 75 canonical-ABI module/name pairs the compiler can emit, with their pinned WASI interface versions. An unknown interface, operation or version still fails closed with an artifact-specific reason. Custom-capability imports, which are derived from their contracts, use the same exact hashed-namespace grammar on both Wasm targets.

## Component contract

Every `--target wasip2` build must satisfy these eight properties:

1. **Imports are source-declared boundaries only.** Every WIT import in the component is justified by at least one declared Aver effect (`! [...]`) or a required operation of a loaded capability contract. Every declared standard effect either lowers to one or more WIT imports in the selected world or is rejected at compile time. One Aver call may become several WIT calls in the generated glue (e.g., `Console.print` cache + stream write), and one WIT interface may serve many Aver effects (e.g., `wasi:io/streams` for both stdout writes and stdin reads). The component picks up no capability and no host hook beyond what the source asks for.
2. **Exports are the handler shape only.** A program with a `main` function exports `wasi:cli/run`. A program compiled with `--world wasi:http/proxy` (Phase 3 / 0.19) exports `wasi:http/incoming-handler`. Internal Aver functions, types and runtime helpers never become public exports.
3. **All public ABI goes through WIT.** Anything that crosses the component boundary uses canonical WIT types: strings, lists, records, variants, results. There is no Aver-specific encoding.
4. **No Aver values cross the boundary.** Per-instantiation `Map<K, V>`, `List<T>`, `Vector<T>`, `Option<T>`, `Result<T, E>`, tuples, records and variants stay inside the user core module. The canonical ABI for engine-GC types is still a pre-proposal upstream, and we encode nothing that would break when it lands.
5. **Generated WIT is emitted next to the artifact.** `aver compile --target wasip2 -o out` produces `out/<name>.component.wasm` and `out/<name>.wit`. The WIT is human-readable and is the source of truth for what the component imports and exports. Custom sections carry no hidden surface.
6. **Component validates with `wasm-tools`.** `wasm-tools validate --features component-model out/<name>.component.wasm` exits zero on every artifact `aver compile --target wasip2` produces. CI gates bench scenarios and example programs on this.
7. **WASI resources stay implementation-internal.** Stdout / stderr `output-stream` handles, filesystem descriptors, pollables and similar resource handles may be cached and reused inside the per-effect glue. They are **not** exposed as Aver-level values. The Aver surface has no `Resource<T>` / `Handle<T>` / `Stream<T>` type in 0.18. Adding one would be a deliberate language decision for 0.19+, and WIT lowering will not add one as a side effect.
8. **Filesystem access is preopen-scoped.** `Disk.*` paths resolve only against WASI preopened directories. Absolute paths and paths that escape preopens return `Result.Err("path not preopened")`. That is a *dynamic* host capability gap, separate from compile-time rejects. The source-level `Disk` API does not change; the wasip2 lowering enforces the WASI capability model at the boundary.

## `aver run --wasip2 file.av`

This compiles the source to a `wasi:cli/command` component, instantiates it in embedded wasmtime, and runs the `wasi:cli/run` export:

- Effects are recorded at the **Aver call level**, above the WIT import boundary. Recordings are interchangeable with VM, wasm-gc and self-host traces (the same `recording.json` shape since 0.16.1).
- Diagnostics are Aver-shaped. Wasmtime trap messages go through the same translation that `aver run --wasm-gc` uses today, so users see Aver source spans instead of core-wasm offsets.
- No build cache. Compiling is fast enough that a cache layer is not worth its cache-invalidation contract.
- `--record <dir>` / `--replay <recording.json>` are **not yet wired** for `--wasip2`, and the CLI rejects both flags. Recording needs a separate plumbing pass against the canonical-ABI WASI imports. Until that lands, use `aver run --wasm-gc --record` (recordings are interchangeable across backends). The first bullet, about effects recorded "at the Aver call level", describes the recording shape shared across backends. It does not mean `--wasip2` accepts these flags.

External hosts: `wasmtime run` is the canonical path for command components, and `wasmtime serve` is the canonical local runner for the HTTP/proxy world.

## Host compatibility matrix for the HTTP/proxy world

The component the wasm-gc backend emits uses the WebAssembly **wasm-gc** and **tail-call** proposals. WASI 0.2 itself is stable and supported across hosts, but those two engine proposals are still opt-in on most runtimes. Pick a host that ships them enabled or lets you turn them on with a flag. The results below were checked against the eight tests in `tests/wasip2_http_handler_stress.rs` (echo / large body / routing / method dispatch / headers / JSON / concurrent / sequential).

| Host | Status | Notes |
|---|---|---|
| `wasmtime serve` 43.x | ✅ works | Pass `-W gc=y -W tail-call=y`. The address binds via `--addr=ip:port` (e.g. `--addr=127.0.0.1:8080`). The bound port shows up on stderr as `Serving HTTP on http://...:N/`, which helps with `--addr=:0` ephemeral binds in test harnesses. |
| Embedded wasmtime via `wasmtime-wasi-http` | ✅ works | Enable `Config::wasm_gc(true)` + `Config::wasm_tail_call(true)` on the engine and plumb a `wasmtime_wasi_http::WasiHttpCtx`. It is the same engine `wasmtime serve` runs on. |
| `jco serve` (Bytecode Alliance) on Node ≥ 22 | ✅ works | `npx @bytecodealliance/jco serve component.wasm --host 127.0.0.1 --port N`. It transpiles the component to JS + core wasm modules and runs them on V8, which has had wasm-gc + tail-call enabled since 22.0. V8 is a different engine from wasmtime, so a pass here shows the component is portable across engine implementations and not only across wasmtime variants. Node 20 rejects it with `Unknown type code 0x4e, enable with --experimental-wasm-gc`. That flag can't be set through `NODE_OPTIONS`, so use Node 22+ instead of working around it. |
| Spin 3.5.x | ❌ rejected at load | The bundled wasmtime does not enable the wasm-gc proposal (`rec group usage requires 'gc' proposal to be enabled`). There is no user-facing flag to override it, and the runtime-config TOML has no `[wasmtime]` table. This follows Spin upstream: once their bundled wasmtime turns the proposal on (or exposes a flag), the same `.component.wasm` will run unchanged. |
| NGINX Unit 1.34.x (`wasm-wasi-component`) | ❌ rejected at load | Same root cause as Spin. Unit's `wasm_wasi_component.unit.so` module embeds wasmtime with the wasm-gc proposal off (tested with the `unit:wasm` Docker image). The error is identical: `rec group usage requires 'gc' proposal to be enabled`. It will work once Unit upgrades its bundled wasmtime build. |
| WasmEdge 0.16.x | ❌ component model experimental | `--enable-component` exists, but the validator is still under construction. It rejects our component with `Alias export: Export index 0 exceeds available component instance index 0` before wasm-gc or tail-call even come up. Re-test once their component-model validator stabilises. |
| Wasmer 7.x | ❌ no component model | `error: ... encoded as a component but the WebAssembly component model feature is not enabled`. Component support is not on Wasmer's near-term roadmap. |
| wasmCloud `wash` 2.x | ⚠️ different shape | `wash dev` is a mesh-deployment daemon that expects a full wasmCloud project + manifest, not a standalone serve. There is no quick equivalent of `wasmtime serve component.wasm`. A wasmCloud project around our component would presumably work (their host is wasmtime-based, with GC enabled in recent versions), but checking that takes a project scaffold, which was out of scope for this round. |
| Fermyon Cloud, Fastly Compute | ⚠️ untested | Cloud-only deployments that would need an account and a push. Both are spec-compatible with `wasi:http/proxy`; whether each enables wasm-gc + tail-call depends on its bundled runtime build. |

The component itself is portable. The only host requirement is a WASI 0.2 wasi:http/proxy host with the wasm-gc and tail-call proposals on. Wider host coverage is waiting on host updates and needs no codegen changes in Aver.

## Running `Tcp.*` programs (Phase 4.2.x in flight, 0.20)

`Tcp.*` programs compile to the same `wasi:cli/command` world as the other CLI effects, but at run time they also need the wasi-sockets imports enabled. With `wasmtime run`:

```
wasmtime run \
    -W gc=y -W tail-call=y \
    -S inherit-network=y \
    -S allow-ip-name-lookup=y \
    -S tcp=y \
    component.wasm
```

| Flag | Why |
|---|---|
| `-W gc=y -W tail-call=y` | Engine proposals, the same requirement as the HTTP/proxy world. |
| `-S inherit-network=y` | Gives the guest access to the host's network stack. Without it every wasi-sockets call returns the default-deny error. |
| `-S allow-ip-name-lookup=y` | Enables `wasi:sockets/ip-name-lookup`. It is required even for IP-literal hosts like `"127.0.0.1"`; without it `resolve-addresses` rejects every input. |
| `-S tcp=y` | Enables `wasi:sockets/tcp`. Without it `create-tcp-socket` traps before the connect can start. |

`-S udp=y` is not needed, because Aver's `Tcp.*` does not touch wasi:sockets/udp. Embedded wasmtime hosts get the same capability through `WasiCtxBuilder::inherit_network()` + `allow_ip_name_lookup(true)` + `socket_addr_check(...)`.

## `aver compile --target wasip2 -o out`

Produces:

```
out/
  <name>.component.wasm    -- the component
  <name>.wit               -- generated WIT, human-readable
```

Flags:

- `--world <world>` picks the WIT world the component targets. There are two values: `wasi:cli/command` (the default, a long-running process exporting `wasi:cli/run.run`) and `wasi:http/proxy` (an HTTP server exporting `wasi:http/incoming-handler.handle`, shipped in 0.19). The proxy world goes with `--handler <fn>` (the same flag the wasm-gc + Cloudflare path uses), which names the user fn with signature `Fn(HttpRequest) -> Http.Response` that becomes the proxy handler. The compile path is driven only by flags. No listener call is inferred from `main`, and native serving is the separate `HttpServer` module over `Tcp`. Programs whose effects do not fit the chosen target fail at compile time with `capability-target-unsupported`, which names the capability, the operations the program required, and both contract hashes.
- `--optimize {size,speed}` is **rejected** on `--target wasip2`. Upstream `wasm-opt` does not yet handle wasm-gc + Component Model bytes cleanly, so the CLI refuses the flag instead of silently dropping it. Use `--target wasm-gc` if you need post-pass size/speed optimization. We will wire it for wasip2 once the toolchain catches up.

The compiler does not shell out. WIT emission goes through `wit-encoder`. Component-type metadata is encoded with `wit-component::metadata` and embedded as a custom section in the core module. The component wrap itself goes through `wit_component::ComponentEncoder`. It all happens in one binary, with no toolchain to install on the user's machine.

### Custom capability imports (phase 3a)

Calling one operation of a program-defined capability selects its complete contract. If every declared operation parameter and result is `Unit`, `Bool`, `Float` or `String`, `aver compile --target wasip2` emits one generated WIT interface and imports it into the selected world. Pure and effectful capability modules use the same transport path. `semantics`, Oracle, replay and hostile profiles keep their existing language and proof meaning and do not change the ABI.

The boundary mapping is small and exact on purpose:

| Aver | WIT / canonical ABI |
|---|---|
| `Unit` | no value slot |
| `Bool` | `bool` / flat `i32` |
| `Float` | `f64` |
| `String` | `string`; copied between Aver GC storage and canonical linear memory |

`Int` is not narrowed to `s64`, because Aver integers are arbitrary precision. `Result`, `Option`, tuples, lists/vectors/maps, represented records/sums and opaque resources are also outside phase 3a. One unsupported type makes the whole contract `unsupported[wit-boundary-type-unsupported]`; there is no partial interface. The diagnostic names the capability, the operation, the exact parameter/result position, the offending Aver type, `contract_hash` and `model_hash`.

Generated interface identity contains an injective encoding of the module name and the full `contract_hash`. Operations are sorted and encoded deterministically. Parameters are positional (`p0`, `p1`, …), so renaming source parameters or changing declaration order does not change the WIT bytes. Both hashes appear in the interface docs. `model_hash` audits source semantics and says nothing about ABI layout, so it does not change transport identity. If none of a capability's operations is used, no interface is emitted. If any is used, the provider binds the full contract.

Compilation leaves this import unresolved on purpose. The artifact is `host-bound[component-import-required]`; it is not `provided`. Install the interface implementation in a Component Model host/linker, then instantiate the component. For local execution, `aver run app.av --wasip2` uses the project's schema-1 `[providers]` manifest and cached Rust host whenever the program reaches a bound capability. It validates the full contract through the ordinary native `ProviderRegistry`, installs the generated WIT functions in the embedded wasmtime linker at run time, and converts phase-3a values to and from the same transport-neutral `ProviderValue` tree the VM and generated Rust use. Provider faults, panics and wrong return shapes keep their provider-boundary diagnostics and do not surface as canonical-ABI traps.

Without a binding, `aver run --wasip2` reports `error[capability-provider-missing]` before linking. Aver never discovers or downloads a package implicitly. `aver compile --target wasip2` still produces an unresolved, portable component. The cached Rust composition is a shortcut for running and does not change the artifact contract.

## Effect mapping

Aver effects lower directly to WASI 0.2 imports. The mapping is fixed per effect. One Aver call in source may become one or several WIT calls in the generated glue (e.g., a `Console.print` may cache the stdout `output-stream` resource handle once and call `wasi:io/streams.[method]write` per print).

| Aver effect | WIT import (the glue calls into) |
|---|---|
| `Args.get` | `wasi:cli/environment.get-arguments` |
| `Env.get` | `wasi:cli/environment.get-environment` |
| `Env.set` | **Compile-rejected**. The WASI 0.2 environment is read-only by design, so no host can ever satisfy a write. Same "cannot-ever-support" category as `Terminal.*`. |
| `Console.print` / `error` / `warn` | `wasi:cli/stdout.get-stdout` / `wasi:cli/stderr.get-stderr` (cached) + `wasi:io/streams.output-stream.[method]blocking-write-and-flush`. 0.18 uses blocking write-and-flush for command-component semantics and simple replay: one `Console.*` call ⇒ at most one host-side flush, which is easy to record and replay deterministically. WASI output-streams are non-blocking with a polling model; `blocking-write-and-flush` is a binding-level convenience helper that bundles `check-write` + `write` + `flush` + `subscribe`/`poll` into one call. Buffered stdout/stderr could come later as an optimisation, but the semantic unit stays the Aver `Console` call. |
| `Console.readLine` | `wasi:cli/stdin.get-stdin` (cached) + `wasi:io/streams.input-stream.[method]blocking-read` |
| `Disk.readText` / `writeText` / `appendText` / `readBytes` / `readBytesAt` / `writeBytes` / `appendBytes` / `size` / `exists` / `delete` / `deleteDir` / `listDir` / `makeDir` / `sync` | `wasi:filesystem/preopens.get-directories` (cached) + `wasi:filesystem/types.[method]*`. Binary calls use raw stream octets with no UTF-8 conversion. `readBytesAt` passes its offset to `read-via-stream`, grows a bounded buffer only as data arrives, and treats EOF as a successful short read. `size` reads descriptor metadata. `sync` opens the path read-only (a file or a directory, since a plain `open-at` admits both), calls `[method]descriptor.sync`, and drops the descriptor. Paths outside preopens return `Result.Err("path not preopened")` (the capability model, contract point 8). |
| `Time.now` / `unixMs` | `wasi:clocks/wall-clock.now` (Time.now formats RFC3339 guest-side via Howard Hinnant's `civil_from_days`) |
| `Time.sleep` | `wasi:clocks/monotonic-clock.subscribe-duration` + `wasi:io/poll.poll` + `[resource-drop]pollable` (one pollable per call and a real wait, no busy-loop) |
| `Random.int` / `float` | `wasi:random/random.get-random-u64` + Aver-side range scaling. This is the secure `wasi:random/random` interface (same contract as `get-random-bytes`, returning 8 cryptographically secure bytes packed into a u64). We deliberately do NOT use `wasi:random/insecure.get-insecure-random-u64`. If we later need finer byte-level control (e.g. for `Random.bytes(n)`), switching to `get-random-bytes` is mechanical. |
| `Process.stopRequested` | **Compile-rejected**. WASI 0.2 has no SIGINT/SIGTERM delivery binding. Use a wasm-gc host that supplies `aver.process_stop_requested`. |
| `Http.{get, head, delete, post, put, patch}` | `wasi:http/outgoing-handler.handle` + the future-incoming-response / incoming-response choreography (Phase 2 / 0.19 shipped). The method tag selects `outgoing-request.set-method`. Body-bearing verbs marshal a request body via `request.body` + `outgoing-body.write` + chunked `blocking-write-and-flush` + `outgoing-body.finish`. Headers (request and response) lower as `Map<String, List<String>>`, and multi-valued field names keep the order the server emitted them in. `error-code` variant discriminants surface as per-variant `http: <name>` Err messages (39 cases). |
| Incoming HTTP handler | `wasi:http/incoming-handler.handle` export (Phase 3 / 0.19 shipped). Requires `--world wasi:http/proxy --handler <fn>`. The handler wrapper decodes the host-supplied incoming-request into an Aver `HttpRequest` (method via the 10-case variant, path-with-query split into path/query, headers iterated as `Map<String, List<String>>`, body via `incoming-body.stream` + drained `input-stream.blocking-read`), runs the user's `fn(HttpRequest) -> Http.Response`, marshals the result into an outgoing-response (`outgoing-response` constructor + `set-status-code` + body via `outgoing-body.write` + chunked `blocking-write-and-flush` + `outgoing-body.finish`), and calls `response-outparam.set`. `Content-Length` is synthesised from the response body byte count. The host's listener flag (`wasmtime serve --addr=:N` etc.) binds the socket. |
| `Tcp.{connect, close, writeLine, writeBytes, readLine, readBytes, send, sendBytes, ping}` | `wasi:sockets/{instance-network, ip-name-lookup, tcp-create-socket, tcp}` (Phase 4 / 0.20 "Pulse" shipped, hardened through five peer-review passes). `__rt_tcp_connect` walks lazy-network init → resolve-addresses → async pollable loop → first-IPv4 → create-tcp-socket → start/finish-connect → pool-slot allocation via first-free scan → `Tcp.Connection` materialise. The 256-slot pool refuses the 257th simultaneous connect with `Err("tcp: connection limit reached (256 max)")`, matching the HashMap-len gate in `aver-rt::tcp::connect`. A closed slot can be reused immediately. `Tcp.close` drops streams, shuts down and drops the socket; a second close on the same handle surfaces `Err("tcp: unknown connection")`. `writeLine`/`readLine` pass text through chunked blocking-write / 1-byte blocking-read against the pooled streams. writeLine appends `\r\n`, and readLine strips the trailing `\r` only when the line ends with `\n` (a CR inside the payload is kept). `writeBytes` writes nominal `Bytes` through the same persistent out-stream with no framing or encoding. `readBytes` loops blocking-read until it has the exact requested count and returns nominal `Bytes`; short reads and invalid counts are catchable errors. Any real persistent read/write failure drops both streams and the socket and marks the slot stale; argument validation does not. Stale-handle paths on the persistent methods (null pool, slot-scan miss, `in_use == 0`) surface `Err("tcp: unknown connection")`, so callers can tell a closed handle from a real wasi-side I/O failure or peer EOF. `Tcp.send` is fully **ephemeral**: inline DNS + socket + connect (no pool slot), a raw write on the wire (no `\r\n` appended) + `shutdown(send)`, then read-until-EOF capped at 10 MiB. Stream errors split as `stream-error.last-operation-failed → Err("tcp: stream error")` vs `stream-error.closed → Ok(buf)`. `Tcp.sendBytes` is the byte-clean ephemeral sibling. It has the same inline dial + `shutdown(send)` + read-to-EOF shape, but both payload and response are nominal `Bytes`. `Bytes.fromList` rejects invalid raw integers before TCP is called, and the backend keeps a fail-closed check for malformed internal carriers. No UTF-8 conversion happens in either direction. `Tcp.ping` is also **ephemeral**: the same inline dial as `send` without the read/write phase, then it drops streams + socket on success and returns `Result.Ok(())`. It takes no pool slot, so a program holding 256 live `Tcp.connect` handles can still ping. `Tcp.Connection` is a capability resource (the type checker rejects construction and field reads). IPv6 results from the resolver are skipped (first IPv4 wins), and v1 has no in-line `subscribe-duration` connect timeout. |
| `Tcp.poll` / `Tcp.readSome` / `Tcp.readNow` / `Tcp.writeNow` | `poll` accepts the common `Map<Int, Tcp.Socket>` API but admits only `Tcp.Socket.Connected` and `Tcp.Socket.Sending` values on wasip2. The other two variants carry a `Tcp.Dial` or a `Tcp.Listener`, which only the compile-rejected operations below can mint, so on this target a program cannot build one. It subscribes every `Connected` candidate's input stream and every `Sending` candidate's output stream (`output-stream.subscribe`), adds one duration pollable for the timeout, maps ready dense indices back to caller-owned keys, sorts them with Aver's arbitrary-precision `Int` order, and drops every pollable. A `Listening` or `Dialing` value returns an explicit `Result.Err` and does not trap. `readSome` performs one bounded `input-stream.blocking-read` and returns empty `Bytes` only for clean EOF. `readNow` performs one non-blocking `input-stream.read`: an empty result is `Ok(None)`, and the `closed` stream error is `Ok(Some(empty))`. `writeNow` asks `output-stream.check-write` for the current permit, writes at most that many bytes with the never-blocking `output-stream.write`, requests `flush`, and returns the count; a zero permit is `Ok(0)`. Actual read or write errors poison the pool slot; argument validation and polling do not. |
| `Tcp.{beginConnect, dialled, listen, accept, peerAddress, closeDial, closeListener}` | **Compile-rejected**. This target's socket binding covers blocking connects and connected-socket I/O only, with no non-blocking dial, listener or peer-address resource. A program that calls one of the seven is refused with `capability-target-unsupported`, naming exactly which operations it asked for. The thirteen operations above, `Tcp.connect` included, are unaffected. |
| `Terminal.*` (12 methods) | **Compile-rejected**. WASI 0.2 has no raw/cooked-mode operations. |
| `Wait.poll` | The one wait of a turn, over the same `wasi:io/poll` set `Tcp.poll` uses. It walks a `Map<Int, Wait.Item>`, subscribes every `Socket` item's stream exactly as the socket poll does, and reports every `Job` key as ready without subscribing anything. A job on this target runs inline at `begin`, so it is done the moment it began. A wait set holding a job therefore polls with a zero timeout and returns at once; a set of sockets alone honours the timeout unchanged. It adds no WIT import and uses the same five canonical-ABI slots the socket poll already names. |
| `Work.cancel` and a job kind's `begin` / `take` | The core module answers these itself. `begin` calls the function `aver.toml` binds with `work = "Module.function"` and mints a handle that already carries the answer, `take` gives the VM's four answers in the VM's words, and `cancel` drops an answer nobody collected. Nothing crosses the component boundary, so a job kind needs no WIT interface and its task and result types never have to be WIT-lowerable. `[work] max-jobs` decides nothing here and says so at the program door; `--record` stays refused. See [services.md](services.md#on-wasm-gc-and-wasip2). |

The wasip2 connected-socket pool is fixed at 256 slots for now, which matches the native default. If `[effects.Tcp].max_connections` selects another value and the program uses `Tcp.connect`, compile/run prints `warning[tcp-connection-limit-unsupported]` instead of silently claiming the deployment policy was applied. Native VM, generated Rust and the in-process wasm-gc host use the configured shared limit for connected sockets and dials.

### Header maps grow with the headers

Request and response headers reach Aver as a `Map<String, List<String>>`, built one header at a time through the same `Map.set` helper every Aver map uses. They grow the same way any map does: the map starts at 16 buckets and doubles as header names arrive. A request with an ordinary handful of headers allocates a table sized for a handful, and there is no header count at which the guest suddenly fails. The table used to be fixed at 16384 buckets, and a peer sending more distinct header names than that could stop the guest. That can no longer happen. Any cap the host places on header count still applies first; the guest imposes none of its own. See the `Map` size note in [`docs/cli.md`](cli.md).

### Generated coordinators

Programs whose loop is generated compile with `--target wasip2` and run with `aver run --wasip2`, provided their own effects are supported. The generated turn does not observe signals, and its `Run.View.stopping` field stays `false`, so a run ends once nothing is seated or when the entry's `stop` says so. A `stop` that waits only for `view.stopping` needs another completion condition while processes remain seated. This rule is specific to the generated loop on this target; explicit `Process.stopRequested` calls still fail the capability check. VM, Rust and wasm-gc keep their host signal observation. The guide fixture, keyed families, cancellation and multiple job kinds run as command components. A program using unsupported operations such as `Tcp.listen` still needs another target.

### Why `Process.stopRequested` / `Terminal.*` / `Env.set` / the `Tcp` dial and listener operations are rejected, not stubbed

The dividing line is **static target capability** vs **dynamic host capability**. `Result.Err` stubs are reserved for *dynamic* host capability gaps: a missing preopen (`Disk.readText("/etc/passwd")` on a host that didn't preopen `/`), a missing env var, a denied permission. A target that does not offer an effect at all is a different category and gets a different shape: a compile-time `capability-target-unsupported` error naming the capability, the operations the program required, and both contract hashes.

The rejected set:

- **`Process.stopRequested`**: WASI 0.2 has no standard process-signal delivery binding. Returning `false` would claim that shutdown was not requested, when the truth is that the target cannot observe it.
- **`Terminal.*`**: WASI 0.2 has `wasi:cli/terminal-input` and `terminal-output` as TTY signals, but no standardised raw/cooked-mode operations (`set-raw-mode`, `set-echo`, `get-window-size`). The capability is structurally absent.
- **`Env.set`**: the WASI 0.2 environment is read-only, and no host implementation could ever satisfy a write. A silent no-op would be a trap: the source says "I set X", and the program runs as if it succeeded while the environment stays unchanged.
- **`Tcp.{beginConnect, dialled, listen, accept, peerAddress, closeDial, closeListener}`**: this backend's socket binding has no dial or listener resource to hand back, so the operation has nothing to return. For one release these seven shipped as `Result.Err` stubs, on the grounds that portable source stays catchable. That put them on the wrong side of the line. A stub says the world refused this call today, so a program written against it treats a permanent property of the target as a transient failure to retry or fall back from. The refusal is now a compile error like the three above, and the stubs were withdrawn on purpose.

(Earlier 0.18 betas grouped `Time.sleep` with the structural rejects, on the assumption that the pollable model was out of scope. That was a scoping mistake: pollables can be wrapped *inside* a single helper without leaking to source. Phase 1.4c shipped `__rt_time_sleep` doing exactly that, so `Time.sleep` now lowers natively.)

Compile output for any of these:

```
error[capability-target-unsupported]: target `wasip2` cannot bind capability `Tcp`
  reason[standard-operations-unavailable]: standard capability `Tcp` cannot bind operation(s) Tcp.accept, Tcp.closeListener, Tcp.listen on `wasip2`: this target's WASI 0.2 socket binding owns blocking connects and connected-socket I/O only, with no non-blocking dial, listener, or peer-address resource; use `Tcp.connect` and the connected-socket operations
  required operations: Tcp.accept, Tcp.close, Tcp.closeListener, Tcp.listen, Tcp.poll, Tcp.readSome, Tcp.writeBytes
  contract_hash: sha256:2f32788e...
  model_hash: sha256:4d6a9183...
```

A capability the target cannot bind at all (`Process`, `Terminal`) reports `reason[standard-binding-unavailable]` in the same shape.

## Phasing inside 0.18

| Phase | Scope | Status |
|---|---|---|
| 0 | Audit legacy coupling, wire `wit-component`/`wit-encoder` deps, prove the wrap pipeline | ✅ shipped |
| 1.0 / 1.1 | `--target wasip2` CLI plumbing, end-to-end pipeline for no-effect programs | ✅ shipped |
| 1.2 | `wasi:cli/stdout` + `wasi:io/streams` glue. `Console.print` / `error` / `warn` → stream write end-to-end | ✅ shipped |
| 1.3 | `wasi:cli/stdin` + `wasi:cli/environment`. `Console.readLine` / `Args.get` / `Env.get` | ✅ shipped |
| 1.4 | `wasi:clocks/wall-clock.now` for `Time.now` / `Time.unixMs`; `wasi:random` for `Random.*`; `wasi:clocks/monotonic-clock.subscribe-duration` + `wasi:io/poll.poll` for `Time.sleep`. | ✅ shipped |
| 1.5 | `wasi:filesystem`. Every `Disk.*` method (`exists` / `readText` / `writeText` / `appendText` / `readBytes` / `readBytesAt` / `writeBytes` / `appendBytes` / `size` / `delete` / `deleteDir` / `makeDir` / `listDir` / `sync`). Paths resolve relative to the cached preopen. | ✅ shipped |
| 1.6 | Reject `Terminal.*` / `Env.set` at compile time as permanent (WASI 0.2 has no terminal interface; the environment is read-only). `Http.*`, `Tcp.*` and the incoming-handler export were deferred to later phases. | ✅ shipped |
| 1.7 | `aver run --wasip2` (embedded wasmtime + `wasmtime-wasi`) with CWD preopened as `.` | ✅ shipped |
| 1.8 | Drop the legacy `--target wasm` backend (`src/codegen/wasm/`, `wasm-legacy` feature, `--bridge` flag, `wasm-runtime` subcommand, legacy bundling in `src/main/commands.rs`) | ✅ shipped |

## Out of scope for 0.18

- **Outgoing HTTP** (`wasi:http/outgoing-handler`): Phase 2 / 0.19. Direct WIT lowering, the same mechanism as Phase 1 with more types to marshal.
- **HTTP server** (`wasi:http/incoming-handler` / `wasi:http/proxy` world): Phase 3 / 0.19 or 0.20. The export shape differs: the handler exposes a WIT export and the host calls in.
- **TCP sockets** (`wasi:sockets/tcp`): Phase 2 / 0.19. It is an open question whether Aver wants long-lived socket handles as a language concept.
- **Resources / streams / pollables on the Aver surface**: implementation only in 0.18. If Aver grows a `Resource<T>` type, that will be a deliberate language decision for 0.19+.
- **WASI 0.3**: the async ABI, `future<T>` and `stream<T>` exist but are not finalised. Upstream has committed that 0.3 hosts will virtualise 0.2 hosts, so waiting costs us nothing.
- **`wasi:keyvalue`, `wasi:logging`, `wasi:config`, `wasi:tls`, `wasi:blobstore`, `wasi:nn`**: none.
- **Cross-component shared runtime**: needs GC types in the canonical ABI, and that proposal is still a pre-proposal upstream. Per-instantiation helpers stay inline.
- **`jco transpile` as a derived target** for browsers / Node: possible in 0.19+ if there is concrete demand.

## References

- WASI 0.2 release tracker: https://github.com/WebAssembly/WASI/releases
- Component Model spec: https://github.com/WebAssembly/component-model
- `wit-component` crate: https://docs.rs/wit-component
- `wit-encoder` crate: https://docs.rs/wit-encoder
- `wit_component::metadata` (custom section encoding): https://docs.rs/wit-component/latest/wit_component/metadata/
- GC in canonical ABI (pre-proposal): https://github.com/WebAssembly/component-model/issues/525
