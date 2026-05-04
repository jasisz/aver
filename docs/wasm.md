# WASM Backend

Aver has two WASM backends. The recommended target since 0.16 is `--target wasm-gc` — native WebAssembly GC + tail-call output, no custom runtime, modern-host baseline (Chrome 119+, Firefox 120+, Safari 18.2+, wasmtime 25+, Node 22+, Cloudflare Workers). The legacy `--target wasm` backend is a fallback for pre-2024 hosts; it lives in `src/codegen/wasm/` and is documented at the bottom.

## `--target wasm-gc` (recommended)

```bash
aver compile app.av --target wasm-gc -o out
aver compile app.av --target wasm-gc --optimize size -o out
aver compile app.av --target wasm-gc --optimize speed -o out
aver compile app.av --preset cloudflare --handler handler -o out
```

Self-contained binary. The engine handles GC, recursion, and tail calls; per-instantiation helpers (per-`Map<K,V>` probes, per-`List<T>` ops, per-(K,V) eq helpers, the `__rt_string_*` LM transport) are inlined into the same module and DCE'd by `wasm-opt -Oz` to "what this program actually calls". No shared sidecar runtime to fetch, no `wasm-merge` step, no NaN-boxing.

`aver run --wasm-gc app.av` runs the same artifact through an embedded wasmtime executor with the full effect surface (Args, Console, Time, Random, Float math, Terminal, Disk, Env, Tcp, Http). Output matches the VM byte-for-byte modulo time/randomness on every audited example + project main.

### Boundary types

- `Int -> i64`, `Float -> f64`, `Bool -> i32`, `Unit -> nothing`.
- `String -> (ref null $string)` — engine-managed `(array i8)` UTF-8.
- `List<T>`, `Vector<T>`, `Map<K, V>`, records, variants, tuples — all monomorphised to engine GC structs/arrays per instantiation. The wasm-gc type registry is local to the final flattened module; cross-module calls compile to plain `call $fn` after multi-module flattening.

Newtype optimisation: a single-field record over a primitive (`record UserId { raw: Int }`) and a single-payload single-variant sum (`type UserId = UserId(Int)`) lower to the underlying primitive everywhere — no `struct.new`, no `struct.get`, no `ref.cast`. Same trick rustc uses for `struct UserId(u64)`.

### Effect imports

Effectful imports follow the `aver/*` namespace — `aver/console_print(s: ref null $string)`, `aver/time_unix_ms()`, `aver/disk_read_text(path: ref null $string) -> ref null $string`, etc. The string transport sits behind two helpers the host calls into: `__rt_string_from_lm(written: i32) -> (ref null $string)` and `__rt_string_to_lm(s: ref null $string) -> i32`. Encoding is straight UTF-8; the host writes/reads into LM (linear memory) page 0 and exchanges a guest-managed string ref for the byte count.

The exact import surface lives in [`src/codegen/wasm_gc/effects.rs`](../src/codegen/wasm_gc/effects.rs). `Console.print`/`error`/`warn` and `Terminal.print`/`setColor` all take `String` since 0.16 — stringification is the caller's job (interpolation `"{x}"` for primitives, a per-type render fn for compound shapes).

### `--preset cloudflare`

```bash
aver compile app.av --preset cloudflare --handler handler -o out
# expands to: --target wasm-gc --pack cloudflare --handler handler
```

Drops `app.wasm` (wasm-gc binary), `worker.js` (~140-line LM string transport adapter), and a `wrangler.toml` template into `out/`. The handler must have signature `Fn(HttpRequest) -> HttpResponse`. The compiler synthesises an `aver_http_handle()` wrapper that:

1. reads request fields via `Request.*` host imports,
2. allocates an `HttpRequest` struct,
3. calls the user's handler,
4. walks the response's `headers: Map<String, List<String>>` and dispatches one `Response.setHeader(name, value)` per (key, value) pair,
5. finalises with `Response.text(status, body)`.

Routing, response shape, and header semantics stay in Aver. Workerd's V8 has stable wasm-gc + tail calls, so no compat flags beyond a recent `compatibility_date` are needed.

### Browser host

The reference browser host is [`tools/website/playground/`](../tools/website/playground/). It instantiates a wasm-gc binary with the `aver/*` imports wired against `console.log`, `Date.now()`, `crypto.getRandomValues`, etc., translates JS strings to/from `(ref null $string)` via `__rt_string_*`, and renders `Terminal.*` against a retained text grid. For interactive `Terminal.readKey()` workloads, serve with `python3 tools/website/serve.py 4173` so the page gets the cross-origin isolation headers needed for shared-memory input.

A minimal embedder needs:

- `WebAssembly.instantiate(bytes, { aver: {...host imports...} })`
- The `__rt_string_from_lm` / `__rt_string_to_lm` round-trip for any string-valued effect
- A 1-page LM transport buffer for the round-trip (the wasm module exports `memory` + `__rt_memory_grow(pages)`)

### Policy is the host's job

The WASM artifact does **not** embed `aver.toml` runtime policy:

- **What the program declares it needs** — effect imports, deterministic mocks for replay, independence-mode invariants — is build-time semantics, encoded in the WASM module.
- **What the program is allowed to do at runtime** — URL/host whitelists, filesystem scoping, rate limits, capability tokens — is enforcement, and every wasm host already has a richer model than `aver.toml` could express portably (`wasmtime --allow-net=...`, Workers `services` bindings, browser CSP, Fastly backend allowlist).

A trap on a missing import is a free `deny`. Re-encoding allow/deny rules into the WASM artifact would duplicate what the host already enforces. `aver.toml` stays fully effective for VM and `--self-host`; under WASM the only fields with a job to do are the build-time ones (deterministic mocks, independence cancel).

### Limitations

- **Multi-module is compile-time flatten** — `depends [...]` works, but the backend emits one standalone module. The Component Model is a future separate mode.
- **Wasmtime GC tax on alloc-heavy hot loops** — wasmtime 44's GC heap path costs ~3-22× vs V8 on `string_interp` / `map_lookup` / `fractal_seahorse` (same wasm, different engine). On V8 (Workers, browsers, Node 22+) wasm-gc wins or ties everywhere. Run alloc-heavy benchmarks under V8 before blaming the compile path.
- **Independence mode** — `cancel` vs `complete` has no effect since WASM execution is single-threaded.

## `--target wasm` (legacy fallback)

Kept for environments that don't speak the GC + tail-call proposals — wasmtime CLI < 25, Node < 22, anything pre-2024. Programs are bundled into a single module that imports from a custom NaN-boxed runtime; the runtime is inlined via `wasm-merge` so deployment is one file.

```bash
aver compile app.av --target wasm
aver compile app.av --target wasm --optimize size
aver compile app.av --target wasm --bridge wasip1   # standalone WASI preview-1
```

`--optimize` requires `binaryen` (`wasm-metadce` and `wasm-opt`) on PATH.

### Boundary ABI

- `Int -> i64`, `Float -> f64`, `Bool -> i32`, heap-backed values (`String`, `List`, `Map`, `Vector`, records, variants, wrappers) are NaN-boxed `i64`s carrying a tag and a 32-bit pointer into linear memory.
- Strings cross the boundary as `(ptr, len)`; heap strings are 8-byte header + UTF-8 bytes.
- `Print.value` / `Format.value` take `(tag: i32, value: i64)`.
- `Terminal.readKey` returns `(ptr, len)` for `Some(String)` and `(-1, 0)` for `Option.None`.

The canonical import table lives in [`src/codegen/wasm/abi.rs`](../src/codegen/wasm/abi.rs).

### Memory model

Single bump-heap allocator (`$alloc`) with boundary compaction at function return and TCO iteration boundaries. ~1.5 KB of emitted WASM. Function returns walk `collect_begin → retain → collect_end → rebase`; self-call TCO uses an `iter_mark` (skip-on-small-allocations heuristic, ≤256 bytes) to avoid per-iteration compaction; mutual TCO uses a 16 KB watermark. Modules export `$heap_ptr` and `$alloc(size: i32) -> i32` for host-side memory inspection.

### Built-in host

`aver run app.av --wasm` compiles with the legacy ABI and executes the module with a built-in wasmtime host in [`src/main/commands.rs`](../src/main/commands.rs). The built-in host covers `Console.*`, `Terminal.*`, `Random.int`, `Time.*`, `Print.value`, `Format.value`, and `Float.{sin,cos,atan2,pow}` — Disk, Http, Tcp, Env, and Args are not available; use `--wasm-gc` for those (full effect surface).

### Minimal browser host

Enough to run console-style examples compiled with `aver compile hello.av --target wasm`:

```html
<pre id="out"></pre>
<script type="module">
const out = document.querySelector("#out");
const td = new TextDecoder();
const te = new TextEncoder();
let instance;

const mem = () => new Uint8Array(instance.exports.memory.buffer);
const readBytes = (ptr, len) => mem().slice(ptr, ptr + len);
const readStringObj = (ptr) => {
  const view = new DataView(instance.exports.memory.buffer);
  const len = Number(view.getBigUint64(ptr, true) & 0xffffffffn);
  return td.decode(readBytes(ptr + 8, len));
};
const formatTagged = (tag, val) => {
  switch (tag) {
    case 0: return BigInt.asIntN(64, val).toString();
    case 1: {
      const buf = new ArrayBuffer(8);
      const view = new DataView(buf);
      view.setBigUint64(0, BigInt.asUintN(64, val), true);
      return String(view.getFloat64(0, true));
    }
    case 2: return val !== 0n ? "true" : "false";
    case 3: return readStringObj(Number(val));
    default: return String(val);
  }
};
const writeGuestString = (text) => {
  const bytes = te.encode(text);
  if (bytes.length <= 32) { mem().set(bytes, 96); return [96, bytes.length]; }
  const ptr = instance.exports.alloc(bytes.length);
  mem().set(bytes, ptr);
  return [ptr, bytes.length];
};

const imports = {
  aver: {
    console_print(ptr, len) { out.textContent += td.decode(readBytes(ptr, len)); },
    console_error(ptr, len) { out.textContent += td.decode(readBytes(ptr, len)); },
    console_readLine() { return writeGuestString(""); },
    print_value(tag, val) { out.textContent += formatTagged(tag, val); },
    format_value(tag, val) { return writeGuestString(formatTagged(tag, val)); },
    random_int(min) { return min; },
    time_now() { return writeGuestString(new Date().toISOString()); },
    time_unixMs() { return BigInt(Date.now()); },
    time_sleep() {},
    math_sin(x) { return Math.sin(x); },
    math_cos(x) { return Math.cos(x); },
    math_atan2(y, x) { return Math.atan2(y, x); },
    math_pow(base, exp) { return Math.pow(base, exp); },
  },
};

({ instance } = await WebAssembly.instantiateStreaming(fetch("/hello.wasm"), imports));
instance.exports._start();
</script>
```

The same shape works in Node via `WebAssembly.instantiate(...)` with a `Buffer`.

## Playground maintenance

```bash
python3 tools/website/rebuild_playground.py
```

Syncs mirrored game sources under `tools/website/playground/sources/`, rebuilds the shipped `.wasm` files with `--target wasm-gc --optimize size`, and refreshes the size labels shown on the website.
