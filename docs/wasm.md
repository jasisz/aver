# WASM Backend

This backend compiles Aver to standalone `.wasm` modules with a typed ABI:

- `Int -> i64`
- `Float -> f64`
- `Bool -> i32`
- heap-backed values (`String`, `List`, `Map`, `Vector`, records, variants, wrappers) -> `i32` pointer

Default output uses the `aver/*` import ABI. That keeps the generated module host-neutral: the same `.wasm` can run under the built-in wasmtime host, a browser JS shim, or a custom embedder.

## Adapters

- `aver compile app.av --target wasm`
  Emits `aver/*` imports such as `aver/console_print`, `aver/time_unixMs`, `aver/format_value`.
- `aver compile app.av --target wasm --wasm-opt oz`
  Post-processes the emitted module with `wasm-opt -Oz` for smaller binaries.
- `aver compile app.av --target wasm --wasm-opt o3`
  Post-processes the emitted module with `wasm-opt -O3` for speed-oriented optimization.
- `aver compile app.av --target wasm --adapter wasi`
  Emits WASI imports for standalone `wasmtime`.

`--wasm-opt` requires `binaryen` (`wasm-opt`) to be installed. The toolchain passes the required WASM feature flags automatically because Aver modules use bulk-memory ops and multi-value imports.

## Built-in Host

`aver run app.av --wasm` compiles with the `aver/*` ABI and executes the module with a built-in wasmtime host in [src/main/commands.rs](../src/main/commands.rs).

The built-in host currently provides:

- `Console.*`
- `Terminal.*`
- `Random.int`
- `Time.now`, `Time.unixMs`, `Time.sleep`
- `Print.value`, `Format.value`
- `Float.sin`, `Float.cos`, `Float.atan2`, `Float.pow`

## Host ABI Shape

The canonical import table lives in [abi.rs](../src/codegen/wasm/abi.rs).

A few practical rules:

- strings cross the boundary as `ptr + len`
- heap strings inside Aver memory are string objects: 8-byte header, then UTF-8 bytes
- `Print.value` / `Format.value` take `(tag: i32, value: i64)`
- `Terminal.readKey` returns `(ptr, len)` for `Some(String)` and `(-1, 0)` for `Option.None`
- `Terminal.size` returns `(width: i32, height: i32)` and codegen wraps that into `Terminal.Size`

## Memory Model

The WASM backend uses a single bump-heap allocator (`$alloc`) with boundary compaction at function return and TCO iteration boundaries. No separate GC runtime — the full model is ~1.5 KB of emitted WASM.

**Function return**: `collect_begin(mark)` → `retain_i32(result)` deep-copies reachable objects to a temp area → `collect_end()` rebases internal pointers and copies back → `rebase_i32(result)`. Dead objects between mark and heap_ptr are reclaimed.

**TCO iterations (yard semantics)**: an `iter_mark` is saved at each loop iteration. If the iteration allocated very little (≤256 bytes), compaction is skipped entirely (O(1) — accumulator pattern like list building). Otherwise, full compaction from the function's `fn_mark` reclaims dead objects from previous iterations (replacement pattern like game loops).

**Thin/parent-thin frames**: small pure functions (leaf computations, dispatch) skip boundary work entirely — no mark saved, no compaction on return.

**Exported `alloc`**: modules export `$alloc(size: i32) -> i32` so hosts can allocate guest memory safely for strings returned from host imports.

## Limitations

- **`aver.toml` policy** — the WASM binary does not embed runtime policy. Effect restrictions (`hosts`, `paths`, `keys`) are the host's responsibility. The built-in host does not yet read `aver.toml`. Independence mode (`cancel` vs `complete`) has no effect since WASM execution is single-threaded.
- **Multi-module `depends`** — not supported yet in WASM codegen.
- **Services** — the built-in host provides Console, Terminal, Random, Time, and math. Disk, Http, Tcp, Env, and Args are not available.

## Optimized Patterns

- `Option.withDefault(Vector.get(v, i), literal)` → inline bounds check + direct `i64.load`, no Option wrapper allocation
- `Map.set` with unique keys → prepend only (no rebuild), O(1) insert
- String concatenation uses `memory.copy` for bulk byte transfer

## Minimal Browser Host

This is enough to run console-style examples compiled with `aver compile hello.av --target wasm`:

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

There is also a real browser host in [tools/wasm-runner/README.md](../tools/wasm-runner/README.md) that lets you upload a compiled `.wasm` module and run it in-page. For interactive `Terminal.readKey()` workloads, serve it with `python3 serve.py 4173` so the page gets the isolation headers needed for shared-memory input.

## Terminal In Browsers

`Terminal.*` is available in the built-in wasmtime host and in the static browser runner.

For browsers, the rendering policy is still host-specific:

- `Terminal.print` can target a `<pre>`, DOM tree, `<canvas>`, or terminal emulator widget
- `Terminal.moveTo` / `clear` / `setColor` / `flush` map naturally to a retained text grid
- `Terminal.readKey` should be driven from browser keyboard events into a small queue
- `enableRawMode` / `disableRawMode` are usually "capture keyboard events" rather than literal TTY mode

The runner in `tools/wasm-runner` is the reference host for that mapping.
