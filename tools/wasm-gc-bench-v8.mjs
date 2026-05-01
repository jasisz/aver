// V8 bench harness for wasm-gc modules. Mirrors the wasmtime-based
// `aver bench --target=wasm-gc` runner, but executes the same `.wasm`
// under V8 (Node 22+) — necessary because wasmtime 44's GC heap is
// dramatically slower than V8 on alloc-heavy workloads (`string_interp`
// is 2300x faster on V8). See `src/codegen/wasm_gc/README.md` for the
// cross-engine table.
//
// Usage:
//   aver compile bench/scenarios/<name>.av --target wasm-gc -o /tmp/out
//   node tools/wasm-gc-bench-v8.mjs /tmp/out/<name>.wasm
//
// Requires Node 22+ for stable wasm-gc; Node 20 ships an older V8 that
// rejects packed `i8` array types ("invalid value type 0x78").
import fs from 'node:fs';
import process from 'node:process';

const path = process.argv[2];
if (!path) {
  console.error('usage: node wasm-gc-bench-v8.mjs <module.wasm>');
  process.exit(1);
}

const bytes = fs.readFileSync(path);
const module = new WebAssembly.Module(bytes);
const imports = {
  // Mirror the bench-mode stubs for `aver/*` host imports from
  // src/bench/runner.rs — `console_print` no-ops, `time_unix_ms`
  // returns 0n for deterministic runs.
  aver: {
    console_print: (_ref) => {},
    time_unix_ms: () => 0n,
  },
};
const instance = new WebAssembly.Instance(module, imports);
const main = instance.exports.main;
if (!main) {
  console.error('module has no `main` export');
  process.exit(1);
}

// Warmup matches `aver bench` (3 iters).
for (let i = 0; i < 3; i++) main();

const N = 30;
const samples = [];
for (let i = 0; i < N; i++) {
  const t0 = process.hrtime.bigint();
  main();
  const t1 = process.hrtime.bigint();
  samples.push(Number(t1 - t0) / 1e6); // ms
}
samples.sort((a, b) => a - b);
const min = samples[0];
const p50 = samples[Math.floor(N / 2)];
const p95 = samples[Math.floor(N * 0.95)];
const max = samples[N - 1];
const mean = samples.reduce((a, b) => a + b, 0) / N;
const fmt = (x) => x.toFixed(3);
console.log(
  `v8 wall_time: min=${fmt(min)}ms p50=${fmt(p50)}ms p95=${fmt(p95)}ms max=${fmt(max)}ms mean=${fmt(mean)}ms`,
);
