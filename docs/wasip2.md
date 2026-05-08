# `--target wasip2` (Component Model)

> Status: planned for 0.18 "Span". This document is the contract. Anything not on this page is out of scope until the contract is updated and a decision block in `decisions/architecture.av` says otherwise.

`--target wasip2` produces a WebAssembly Component (`.component.wasm` plus a sibling `.wit`) that imports WASI 0.2 worlds and runs on every Component Model host: `wasmtime run` / `wasmtime serve`, Spin, NGINX Unit, wasmCloud, Fermyon Cloud. It is a peer of `--target wasm-gc`, not a successor.

## Two targets, two jobs

| Target | Job | Hosts |
|---|---|---|
| `--target wasm-gc` | Portable core wasm with engine GC + tail calls. Self-contained binary, host wires `aver/*` imports. | Browsers (Chrome 119+, Firefox 120+, Safari 18.2+), Cloudflare Workers (via `--preset cloudflare --handler <fn>`), Node 22+, Deno, Bun, embedded wasmtime |
| `--target wasip2` | WASI 0.2 component with WIT-typed import surface. Imports satisfied by the host's standard WASI implementation. | wasmtime, Spin 3.x, NGINX Unit, wasmCloud, Fermyon Cloud, anything that takes a `.component.wasm` |

Cloudflare Workers and browsers do not run components natively; they stay on `--target wasm-gc`. `--preset cloudflare` is a wasm-gc preset and stays that way.

## Component contract

The seven properties every `--target wasip2` build must satisfy:

1. **Imports are declared effects only.** Every WASI 0.2 import the component declares maps to an effect that appears in the program's `! [...]` lists, and every declared effect maps to a WASI 0.2 import. No silent capability creep, no host hooks beyond what the source asks for.
2. **Exports are the handler shape only.** A program with a `main` function exports the `wasi:cli/run` shape; a program compiled with `--world wasi:http/proxy` exports `wasi:http/incoming-handler`. No internal Aver functions, types, or runtime helpers leak out as public exports.
3. **All public ABI goes through WIT.** Anything that crosses the component boundary — request/response, file paths, environment values, return codes — uses canonical WIT types: strings, lists, records, variants, resources. No Aver-specific encoding on the boundary.
4. **No Aver GC refs cross the boundary.** Per-instantiation `Map<K, V>`, `List<T>`, `Vector<T>`, `Option<T>`, `Result<T, E>`, tuples, records, and variants stay inside the user core module. The canonical ABI for GC types is still pre-proposal upstream; we do not encode anything that would break when it lands.
5. **Generated WIT is emitted next to the artifact.** `aver compile --target wasip2 -o out` produces `out/<name>.component.wasm` and `out/<name>.wit`. The WIT is human-readable and is the source of truth for what the component imports and exports — no hidden surface in custom sections.
6. **Component validates with `wasm-tools`.** `wasm-tools validate --features component-model out/<name>.component.wasm` exits zero on every artifact `aver compile --target wasip2` produces. Bench scenarios and example programs are gated on this in CI.
7. **wasmtime and the preview-1 adapter are pinned together.** The `wasmtime` crate version, `wit-component`, and `wasi-preview1-component-adapter-provider` are bumped as a group; the build refuses to start if their majors disagree. We do not ship a "use whatever adapter is on your machine" story.

## `aver run --wasip2 file.av`

Compiles the source to a component, instantiates it via embedded wasmtime, and runs the `wasi:cli/run` export:

- Effects are recorded at the **Aver call level**, above the WIT import boundary. Recordings are interchangeable with VM, wasm-gc, and self-host traces (same `recording.json` shape since 0.16.1).
- Diagnostics are Aver-shaped. Wasmtime trap messages translate through the same path that `aver run --wasm-gc` uses today; users see Aver source spans, not core-wasm offsets.
- No build cache. Compile is fast enough that adding a cache layer is not worth the cache-invalidation contract.
- `--record <dir>` and `--replay <recording.json>` work on Phase 1 effects (cli/filesystem/clocks/random) in 0.18. HTTP and sockets recording lands as part of Phase 2 if it falls cleanly out of the same recorder; otherwise 0.19.

## `aver compile --target wasip2 -o out`

Produces:

```
out/
  <name>.component.wasm    -- the component
  <name>.wit               -- generated WIT, human-readable
```

Flags:

- `--world <world>` — which WIT world the component targets. Default `wasi:cli/command`. Other accepted values in 0.18: `wasi:http/proxy` (Phase 3 / experimental). Programs whose effects do not fit the chosen world fail at compile time with `target-effect-unsupported` pointing at the offending call.
- `--optimize {size,speed}` — passes through to `wasm-opt` on the inner core module before component wrapping. Same flag as `--target wasm-gc`.

The compiler does not shell out. WIT emission goes through `wit-encoder`; component wrapping goes through `wit-component::ComponentEncoder` with a pre-built preview-1 adapter from `wasi-preview1-component-adapter-provider`. Single binary, no toolchain to install on the user's machine.

## Effect mapping

| Aver effect | WASI 0.2 import |
|---|---|
| `Args.get` / `Env.get` / `Env.set` | `wasi:cli/environment` |
| `Console.print` / `error` / `warn` | `wasi:cli/{stdout, stderr}` via `wasi:io/streams` |
| `Console.readLine` | `wasi:cli/stdin` via `wasi:io/streams` |
| `Disk.readText` / `writeText` / `appendText` / `exists` / `delete` / `deleteDir` / `listDir` / `makeDir` | `wasi:filesystem/types` + `wasi:filesystem/preopens` |
| `Time.now` / `unixMs` | `wasi:clocks/wall-clock` |
| `Time.sleep` | `wasi:clocks/monotonic-clock` |
| `Random.int` / `float` | `wasi:random/random` |
| `Http.get` / `head` / `delete` / `post` / `put` / `patch` | `wasi:http/outgoing-handler` (Phase 2) |
| `HttpServer.listen` / `listenWith` | `wasi:http/incoming-handler` (Phase 3, 0.19 unless trivial) |
| `Tcp.connect` / `send` / `ping` / `writeLine` / `readLine` / `close` | `wasi:sockets/tcp` + `wasi:sockets/instance-network` (Phase 2) |
| `Terminal.*` (12 methods) | **Hard reject** at compile time |

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

`Result.Err` stubs are reserved for *dynamic* host capability gaps: missing preopen, missing env var, no network, denied permission. A target that *cannot ever* support an effect is a different category and gets a different shape.

## Phasing inside 0.18

| Phase | Scope | Status target |
|---|---|---|
| 0 | Audit legacy coupling, wire `wit-component`/`wit-encoder`/adapter-provider deps, POC component build | Foundation |
| 1 | `wasi:cli` + `wasi:filesystem` + `wasi:clocks` + `wasi:random`. `aver run --wasip2`, `aver compile --target wasip2 --world wasi:cli/command`. Record/replay parity for these effects. | 0.18 core |
| 2 | `wasi:http/outgoing-handler` + `wasi:sockets/tcp`. Recording for HTTP/TCP if it falls out cleanly; otherwise deferred. | 0.18 core |
| 3 | `wasi:http/incoming-handler` server shape (`--world wasi:http/proxy`). | 0.18 experimental flag if trivial, otherwise 0.19 |

After Phase 1+2 land green and the effect matrix in `docs/effects.md` has no "maybe this works" cells, the legacy `--target wasm` backend is deleted: `src/codegen/wasm/`, the `wasm-legacy` Cargo feature, the `--bridge` flag, the `Bridge` enum, the `wasm-runtime` subcommand, and the legacy bundling code in `src/main/commands.rs`. See decision `DropLegacyNanBoxedWasm` in `decisions/architecture.av`.

## Out of scope for 0.18

- `wasi:keyvalue`, `wasi:logging`, `wasi:config`, `wasi:tls`, `wasi:blobstore`, `wasi:nn`. Stable enough or not; either way they introduce policy/capability/replay questions worth their own iteration.
- WASI 0.3. The async ABI / `future<T>` / `stream<T>` are real but not finalised. 0.2 hosts will be virtualised by 0.3 hosts per upstream commitment, so we lose nothing by waiting.
- Cross-component shared runtime. Requires GC types in the canonical ABI; that proposal is upstream pre-proposal. Per-instantiation helpers stay inline.
- `jco transpile` as a derived target for browsers / Node. Possible 0.19+ if there is concrete demand.
- Component composition, multi-component linking, dynamic component loading. Component Model has the primitives; we do not surface them yet.

## References

- WASI 0.2 release tracker: https://github.com/WebAssembly/WASI/releases
- Component Model spec: https://github.com/WebAssembly/component-model
- `wit-component` crate: https://docs.rs/wit-component
- `wit-encoder` crate: https://docs.rs/wit-encoder
- Preview-1 component adapter: https://github.com/bytecodealliance/wasmtime/tree/main/crates/wasi-preview1-component-adapter
- GC in canonical ABI (pre-proposal): https://github.com/WebAssembly/component-model/issues/525
