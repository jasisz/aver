// tools/edge-gc — Cloudflare Workers bootstrap for the wasm-gc
// backend. Mirrors `tools/edge/dist/worker.js` but talks to the
// wasm-gc ABI: strings live as `(ref null (array i8))` (engine-
// managed) and travel between JS and guest through a tiny
// linear-memory transport buffer + two `__rt_string_*` exports.
//
// Why an LM transport instead of per-byte exports: per-byte calls
// would trigger one JS↔wasm boundary crossing per UTF-8 byte, ~100
// ns each on V8. A 50 KB fractal page would spend ~10 ms purely on
// I/O, eclipsing the actual render. Bulk-copy via TextEncoder/
// TextDecoder + a single `__rt_string_from_lm` / `__rt_string_to_lm`
// call per direction stays inside one boundary crossing; the loop
// runs at native wasm speed.
//
// Why no `aver/console_print` etc.: this handler's only effect
// surface is the exported `handle(query) -> String` function.
// Future expansions (Time, Console, Http) need their imports back —
// see `tools/edge/dist/worker.js` for the legacy-backend shape.

import userWasm from "./handler.wasm";

let exports = null;

const encoder = new TextEncoder();
const decoder = new TextDecoder("utf-8");

async function init() {
  // userWasm is a `WebAssembly.Module` (Wrangler's `CompiledWasm`
  // import binding). Direct `instantiate(module, imports)` returns
  // the Instance; no `{ instance, module }` wrapper.
  const instance = await WebAssembly.instantiate(userWasm, {});
  exports = instance.exports;
}

function memU8() {
  // `memory.grow` detaches the previous ArrayBuffer, so we re-view
  // every call rather than caching. Build runs are short enough
  // that the overhead is invisible vs a per-call detach check.
  return new Uint8Array(exports.memory.buffer);
}

function ensurePages(needed) {
  const current = exports.__rt_memory_pages();
  if (needed > current) exports.__rt_memory_grow(needed - current);
}

function jsToAver(text) {
  // Worst-case UTF-8 is 3 bytes per JS char (4 with surrogate pairs,
  // but those span two JS chars so the bound holds). Grow LM
  // upfront so `encodeInto` never trips the buffer end.
  const upperBytes = text.length * 3;
  ensurePages(((upperBytes + 65535) >> 16) || 1);
  const { written } = encoder.encodeInto(text, memU8());
  return exports.__rt_string_from_lm(written);
}

function averToJs(s) {
  const len = exports.__rt_string_to_lm(s);
  // Re-view in case the guest's loop triggered memory.grow during
  // the array-to-LM copy (1 page initial, max 16, so worst case
  // 1 MiB). decode'd subarray is a fresh JS string.
  return decoder.decode(memU8().subarray(0, len));
}

export default {
  async fetch(request) {
    if (!exports) await init();
    const url = new URL(request.url);
    // The Aver handler takes the query string (after `?`), not a
    // full URL — that's the surface `Fractal.viewFromQuery` parses.
    const query = url.search.slice(1);
    const queryRef = jsToAver(query);
    const responseRef = exports.handle(queryRef);
    const body = averToJs(responseRef);
    return new Response(body, {
      headers: { "content-type": "text/html;charset=utf-8" },
    });
  },
};
