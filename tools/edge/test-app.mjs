// Local check: run `aver_http_handle` over a synthetic request, dump
// the resulting (status, content-type, body) before deploying via
// wrangler. Mirrors what worker.js does.
//
// Usage: ~/.nvm/versions/node/v25.2.1/bin/node tools/edge-gc/test-app.mjs

import fs from 'node:fs';

const bytes = fs.readFileSync('/tmp/edge-app-gc/app.wasm');
const module = new WebAssembly.Module(bytes);

let exports = null;
let pendingReq = null;
let pendingBody = '';
let pendingResponse = null;

const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8');

function memU8() { return new Uint8Array(exports.memory.buffer); }
function ensurePages(n) {
  const cur = exports.__rt_memory_pages();
  if (n > cur) exports.__rt_memory_grow(n - cur);
}
function jsToAver(text) {
  const s = text ?? '';
  const upperBytes = s.length * 3;
  ensurePages(((upperBytes + 65535) >> 16) || 1);
  const { written } = encoder.encodeInto(s, memU8());
  return exports.__rt_string_from_lm(written);
}
function averToJs(s) {
  const len = exports.__rt_string_to_lm(s);
  return decoder.decode(memU8().subarray(0, len));
}
function buildHeadersMap(jsHeaders) {
  const grouped = new Map();
  for (const [name, value] of jsHeaders) {
    const lower = name.toLowerCase();
    if (!grouped.has(lower)) grouped.set(lower, [value]);
    else grouped.get(lower).push(value);
  }
  let map = exports.__rt_map_string_list_string_empty();
  for (const [name, values] of grouped) {
    let list = null;
    for (let i = values.length - 1; i >= 0; i--) {
      list = exports.__rt_list_string_cons(jsToAver(values[i]), list);
    }
    map = exports.__rt_map_string_list_string_set(map, jsToAver(name), list);
  }
  return map;
}

const imports = {
  aver: {
    time_unix_ms: () => BigInt(Date.now()),
    request_method: () => jsToAver(pendingReq.method),
    request_url: () => jsToAver(pendingReq.path),
    request_query: () => jsToAver(pendingReq.query),
    request_body: () => jsToAver(pendingBody),
    request_headers_load: () => buildHeadersMap(pendingReq.headers),
    response_text: (status, bodyRef) => {
      pendingResponse.status = Number(status);
      pendingResponse.body = averToJs(bodyRef);
    },
    response_set_header: (nameRef, valueRef) => {
      pendingResponse.headers.push([averToJs(nameRef), averToJs(valueRef)]);
    },
  },
};
const instance = new WebAssembly.Instance(module, imports);
exports = instance.exports;

function run(method, path, query, body, headers) {
  pendingReq = { method, path, query, headers: new Map(Object.entries(headers ?? {})) };
  pendingBody = body ?? '';
  pendingResponse = { status: 200, body: '', headers: [] };
  exports.aver_http_handle();
  return pendingResponse;
}

const cases = [
  { name: 'GET /', method: 'GET', path: '/', query: '' },
  {
    name: 'GET /api (cf-ipcountry: PL)',
    method: 'GET',
    path: '/api',
    query: '',
    headers: { 'cf-ipcountry': 'PL' },
  },
  { name: 'GET /llms.txt', method: 'GET', path: '/llms.txt', query: '' },
  {
    name: 'GET /fractal seahorse',
    method: 'GET',
    path: '/fractal',
    query: 'cx=-0.7463&cy=0.1102&w=0.012',
  },
  { name: 'GET /missing (404)', method: 'GET', path: '/nope', query: '' },
];

for (const c of cases) {
  const t0 = process.hrtime.bigint();
  const r = run(c.method, c.path, c.query, '', c.headers);
  const t1 = process.hrtime.bigint();
  console.log(
    `\n--- ${c.name} (${(Number(t1 - t0) / 1e6).toFixed(2)}ms) ---\n` +
    `  status: ${r.status}\n` +
    `  headers: ${r.headers.map(([k, v]) => `${k}=${v}`).join('; ') || '(none)'}\n` +
    `  body[0..200]: ${r.body.slice(0, 200).replace(/\n/g, '\\n')}\n` +
    `  body length: ${r.body.length}`
  );
}

// Bench: 30 iters of /fractal seahorse (full handler shape — request
// fields + headers map build + render + headers walk + response_text)
// so the number is comparable to `tools/edge/test-app.mjs`.
const BENCH_N = 30;
const samples = [];
const benchHeaders = { 'cf-ipcountry': 'PL' };
for (let i = 0; i < BENCH_N; i++) {
  const a = process.hrtime.bigint();
  run('GET', '/fractal', 'cx=-0.7463&cy=0.1102&w=0.012', '', benchHeaders);
  const b = process.hrtime.bigint();
  samples.push(Number(b - a) / 1e6);
}
samples.sort((a, b) => a - b);
const fmt = (x) => x.toFixed(3);
console.log(
  `\nbench /fractal (${BENCH_N} iters, V8 ${process.version}): ` +
  `min=${fmt(samples[0])}ms p50=${fmt(samples[Math.floor(BENCH_N / 2)])}ms ` +
  `p95=${fmt(samples[Math.floor(BENCH_N * 0.95)])}ms ` +
  `mean=${fmt(samples.reduce((a, b) => a + b, 0) / BENCH_N)}ms`
);
