// Local sanity check: instantiate handler.wasm under V8 (Node 22+),
// call `handle(query)` once, dump the first 200 chars + length.
// Mirrors what worker.js does on Cloudflare Workers; running this
// before `wrangler dev` confirms the bridge isn't broken.
//
// Usage: ~/.nvm/versions/node/v25.2.1/bin/node tools/edge-gc/test-local.mjs

import fs from 'node:fs';

const bytes = fs.readFileSync('/tmp/edge-gc/handler.wasm');
const module = new WebAssembly.Module(bytes);
const instance = new WebAssembly.Instance(module, {});
const exports = instance.exports;

const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8');

function ensurePages(needed) {
  const current = exports.__rt_memory_pages();
  if (needed > current) exports.__rt_memory_grow(needed - current);
}

function jsToAver(text) {
  const upperBytes = text.length * 3;
  ensurePages(((upperBytes + 65535) >> 16) || 1);
  const { written } = encoder.encodeInto(text, new Uint8Array(exports.memory.buffer));
  return exports.__rt_string_from_lm(written);
}

function averToJs(s) {
  const len = exports.__rt_string_to_lm(s);
  return decoder.decode(new Uint8Array(exports.memory.buffer).subarray(0, len));
}

const query = 'cx=-0.7463&cy=0.1102&w=0.012';
console.log(`query: "${query}"`);

const t0 = process.hrtime.bigint();
const queryRef = jsToAver(query);
const responseRef = exports.handle(queryRef);
const body = averToJs(responseRef);
const t1 = process.hrtime.bigint();

console.log(`render: ${Number(t1 - t0) / 1e6}ms`);
console.log(`body: ${body.length} bytes`);
console.log(`first 200 chars: ${body.slice(0, 200)}`);
console.log(`contains "seahorse": ${body.includes('seahorse')}`);
console.log(`contains "<!doctype html>": ${body.includes('<!doctype html>')}`);

// Quick benchmark: 30 iters.
const N = 30;
const samples = [];
for (let i = 0; i < N; i++) {
  const a = process.hrtime.bigint();
  averToJs(exports.handle(jsToAver(query)));
  const b = process.hrtime.bigint();
  samples.push(Number(b - a) / 1e6);
}
samples.sort((a, b) => a - b);
const fmt = (x) => x.toFixed(3);
console.log(
  `\nbench (${N} iters, V8 ${process.version}): ` +
  `min=${fmt(samples[0])}ms p50=${fmt(samples[Math.floor(N / 2)])}ms ` +
  `p95=${fmt(samples[Math.floor(N * 0.95)])}ms ` +
  `mean=${fmt(samples.reduce((a, b) => a + b, 0) / N)}ms`
);
