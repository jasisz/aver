// Local check: invoke the legacy `--target wasm --bridge fetch`
// edge bundle's `aver_http_handle` against synthetic requests, then
// bench the seahorse render. Mirrors `tools/edge-gc/test-app.mjs`
// shape so the two backends report comparable numbers on identical
// (status, content-type, body) outputs.
//
// Requires Node 22+ for stable URL/Request/Response globals.

import fs from 'node:fs';

const wasmBytes = fs.readFileSync(
  new URL('./dist/app.wasm', import.meta.url),
);
const wasmModule = new WebAssembly.Module(wasmBytes);

let userExports = null;
let userMemory = null;
let pending = null;
let cachedBuffer = null;
let cachedDataView = null;
let cachedUint8 = null;
const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8');

function refreshViews() {
  if (cachedBuffer !== userMemory.buffer) {
    cachedBuffer = userMemory.buffer;
    cachedDataView = new DataView(cachedBuffer);
    cachedUint8 = new Uint8Array(cachedBuffer);
  }
}

function writeAverString(text) {
  const upper = text.length * 3;
  const ptr = userExports.alloc(upper + 8);
  refreshViews();
  const { written } = encoder.encodeInto(
    text,
    cachedUint8.subarray(ptr + 8, ptr + 8 + upper),
  );
  cachedDataView.setUint32(ptr, written, true);
  cachedDataView.setUint32(ptr + 4, 0, true);
  return ptr;
}

function readString(ptr, len) {
  refreshViews();
  return decoder.decode(cachedUint8.subarray(ptr, ptr + len));
}

const OBJ_LIST_CONS = 4n;
const OBJ_TUPLE = 7n;
const KIND_STR = 3;

function consCell(headValue, tail, headPtrFlag) {
  const ptr = userExports.alloc(24);
  refreshViews();
  const high = (Number(OBJ_LIST_CONS) << 24) | (headPtrFlag & 0xffff);
  cachedDataView.setUint32(ptr, 2, true);
  cachedDataView.setUint32(ptr + 4, high, true);
  cachedDataView.setBigUint64(ptr + 8, headValue, true);
  cachedDataView.setInt32(ptr + 16, tail | 0, true);
  cachedDataView.setInt32(ptr + 20, 0, true);
  return ptr;
}

function tupleStrList(namePtr, valuesListPtr) {
  const ptr = userExports.alloc(24);
  refreshViews();
  const high = (Number(OBJ_TUPLE) << 24) | 0x3;
  cachedDataView.setUint32(ptr, 2, true);
  cachedDataView.setUint32(ptr + 4, high, true);
  cachedDataView.setUint32(ptr + 8, namePtr, true);
  cachedDataView.setUint32(ptr + 12, 0, true);
  cachedDataView.setUint32(ptr + 16, valuesListPtr, true);
  cachedDataView.setUint32(ptr + 20, 0, true);
  return ptr;
}

function buildHeadersMap(grouped) {
  if (typeof userExports.rt_map_from_list !== 'function') return 0;
  let listTail = 0;
  for (const [name, values] of grouped) {
    let valueList = 0;
    for (let i = values.length - 1; i >= 0; i--) {
      const valStr = writeAverString(values[i]);
      valueList = consCell(BigInt(valStr), valueList, 1);
    }
    const nameStr = writeAverString(name);
    const tuple = tupleStrList(nameStr, valueList);
    listTail = consCell(BigInt(tuple), listTail, 1);
  }
  return userExports.rt_map_from_list(listTail, KIND_STR, 1);
}

const imports = {
  aver: {
    console_print: (ptr, len) => console.log('[wasm]', readString(ptr, len)),
    console_error: (ptr, len) => console.error('[wasm]', readString(ptr, len)),
    console_warn:  (ptr, len) => console.warn('[wasm]', readString(ptr, len)),
    time_unixMs:   () => BigInt(Date.now()),
    random_int: (lo, hi) => {
      const span = hi - lo + 1n;
      return lo + BigInt(Math.floor(Math.random() * Number(span)));
    },
    random_float: () => Math.random(),
    request_method: () => writeAverString(pending.method),
    request_url: () => writeAverString(pending.path),
    request_query: () => writeAverString(pending.query),
    request_body: () => writeAverString(pending.body),
    request_headers_load: () => buildHeadersMap(pending.headers),
    response_text: (status, ptr, len) => {
      const existingHeaders = pending.response?.headers ?? [];
      pending.response = {
        status,
        body: readString(ptr, len),
        headers: existingHeaders,
      };
      return 1;
    },
    response_set_header: (namePtr, nameLen, valuePtr, valueLen) => {
      if (!pending.response) pending.response = { headers: [] };
      if (!pending.response.headers) pending.response.headers = [];
      pending.response.headers.push([
        readString(namePtr, nameLen),
        readString(valuePtr, valueLen),
      ]);
    },
    env_get: () => -1,
    env_set: () => {},
    http_clear_request_headers: () => {},
    http_add_request_header: () => {},
    http_send: () => [0n, 0, 0, writeAverString('Http.send not implemented in this harness')],
    print_value: () => {},
    format_value: () => [0, 0],
  },
};

const instance = new WebAssembly.Instance(wasmModule, imports);
userExports = instance.exports;
userMemory = userExports.memory;

function run(method, path, query, body, headersGrouped) {
  pending = {
    method, path, query, body,
    headers: headersGrouped ?? new Map(),
    response: null,
  };
  userExports.aver_http_handle(0);
  return pending.response;
}

const cases = [
  { name: 'GET /', method: 'GET', path: '/', query: '' },
  {
    name: 'GET /api (cf-ipcountry: PL)',
    method: 'GET', path: '/api', query: '',
    headers: new Map([['cf-ipcountry', ['PL']]]),
  },
  { name: 'GET /llms.txt', method: 'GET', path: '/llms.txt', query: '' },
  {
    name: 'GET /fractal seahorse',
    method: 'GET', path: '/fractal', query: 'cx=-0.7463&cy=0.1102&w=0.012',
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
    `  headers: ${(r.headers ?? []).map(([k, v]) => `${k}=${v}`).join('; ') || '(none)'}\n` +
    `  body[0..200]: ${(r.body ?? '').slice(0, 200).replace(/\n/g, '\\n')}\n` +
    `  body length: ${(r.body ?? '').length}`
  );
}

// Bench: 30 iters of /fractal seahorse render, same shape as tools/edge-gc.
const BENCH_N = 30;
const samples = [];
const benchHeaders = new Map([['cf-ipcountry', ['PL']]]);
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
