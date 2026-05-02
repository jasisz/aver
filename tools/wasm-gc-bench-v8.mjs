// V8 bench harness for wasm-gc modules. Mirrors the wasmtime-based
// `aver bench --target=wasm-gc` runner, but executes the same `.wasm`
// under V8 (Node 22+) — necessary because wasmtime 44's GC heap is
// dramatically slower than V8 on alloc-heavy workloads (`string_interp`
// is 2300x faster on V8). See `src/codegen/wasm_gc/README.md` for the
// cross-engine table.
//
// Two modes:
//
//   single — bench one pre-compiled .wasm (default), legacy CLI shape:
//     node tools/wasm-gc-bench-v8.mjs path/to/module.wasm
//
//   compare — compile every `bench/scenarios/*.av` through both
//     `--target wasm` and `--target wasm-gc`, bench each under V8,
//     print a side-by-side table:
//
//     node tools/wasm-gc-bench-v8.mjs --compare
//     node tools/wasm-gc-bench-v8.mjs --compare --aver-bin path/to/aver
//
// Requires Node 22+ for stable wasm-gc; Node 20 ships an older V8 that
// rejects packed `i8` array types ("invalid value type 0x78").

import { spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import process from 'node:process';

// ── argv parsing ────────────────────────────────────────────────────

const args = process.argv.slice(2);
const isCompare = args.includes('--compare');
const aver_bin =
  argValue('--aver-bin') ?? path.resolve('target/release/aver');
const iters = Number(argValue('--iters') ?? '30');
const warmup = Number(argValue('--warmup') ?? '3');

if (!isCompare && args.length === 0) {
  console.error(
    'usage:\n' +
      '  single:  node wasm-gc-bench-v8.mjs <module.wasm>\n' +
      '  compare: node wasm-gc-bench-v8.mjs --compare [--aver-bin path/to/aver] [--iters N] [--warmup N]\n',
  );
  process.exit(1);
}

function argValue(flag) {
  const i = args.indexOf(flag);
  return i >= 0 && i + 1 < args.length ? args[i + 1] : null;
}

// ── shared stub linker ──────────────────────────────────────────────

// Walk a module's import section and synthesise a no-op stub for every
// `(module, name)` pair, matching the wasmtime-based bench harness in
// `src/bench/runner.rs`. Bench programs never call host effects, but
// the wasm-gc backend declares `aver/console_*` and `aver/time_*`
// imports unconditionally; without this V8's `WebAssembly.Instance`
// would refuse to link.
function buildImports(module) {
  const out = {};
  for (const imp of WebAssembly.Module.imports(module)) {
    if (imp.kind !== 'function') continue;
    out[imp.module] ??= {};
    out[imp.module][imp.name] = (..._params) => 0n;
  }
  // Fix-ups: i32 returns can't be 0n (V8 type-checks numeric kind).
  // Use a conservative wrapper that returns `null` for ref returns,
  // 0 for i32, 0n for i64, 0 for f32/f64, undefined for void —
  // V8 accepts `undefined` from a JS host function for any of these.
  // The first attempt above gives 0n (works for i64 imports);
  // nothing in the bench programs cares about returned values.
  return out;
}

function instantiate(bytes) {
  const module = new WebAssembly.Module(bytes);
  const imports = buildImports(module);
  return new WebAssembly.Instance(module, imports);
}

function pickEntry(instance) {
  const exports = instance.exports;
  // Prefer typed `main` if present (returns Int / Float / Unit /
  // String); otherwise fall back to `_start` which the wasm-gc
  // wrapper synthesises for any program with a `main`.
  if (typeof exports.main === 'function') return exports.main;
  if (typeof exports._start === 'function') return exports._start;
  throw new Error('module has no `main` or `_start` export');
}

function timeIters(entry, n) {
  const samples = [];
  for (let i = 0; i < n; i++) {
    const t0 = process.hrtime.bigint();
    entry();
    const t1 = process.hrtime.bigint();
    samples.push(Number(t1 - t0) / 1e6);
  }
  return samples;
}

function summarise(samples) {
  const sorted = [...samples].sort((a, b) => a - b);
  const p = (q) => sorted[Math.floor(sorted.length * q)];
  const mean = samples.reduce((a, b) => a + b, 0) / samples.length;
  return { min: sorted[0], p50: p(0.5), p95: p(0.95), max: sorted.at(-1), mean };
}

const fmt = (x) =>
  x < 1 ? `${(x * 1000).toFixed(1)}µs` : x < 1000 ? `${x.toFixed(2)}ms` : `${(x / 1000).toFixed(2)}s`;

// ── single-file mode (legacy CLI) ───────────────────────────────────

if (!isCompare) {
  const wasmPath = args[0];
  const bytes = fs.readFileSync(wasmPath);
  const instance = instantiate(bytes);
  const entry = pickEntry(instance);
  for (let i = 0; i < warmup; i++) entry();
  const stats = summarise(timeIters(entry, iters));
  console.log(
    `v8 wall_time: min=${fmt(stats.min)} p50=${fmt(stats.p50)} p95=${fmt(stats.p95)} max=${fmt(stats.max)} mean=${fmt(stats.mean)}`,
  );
  process.exit(0);
}

// ── compare mode: build all scenarios under both targets ────────────

const SCENARIOS = [
  ['fib(15)',              'bench/scenarios/fib.av'],
  ['countdown(20k)',       'bench/scenarios/countdown.av'],
  ['record access 20k',    'bench/scenarios/record.av'],
  ['map build 5k',         'bench/scenarios/map_build.av'],
  ['map lookup 20k/2k',    'bench/scenarios/map_lookup.av'],
  ['pattern match 30k',    'bench/scenarios/match_dispatch.av'],
  ['string interp 5k',     'bench/scenarios/string_interp.av'],
  ['vector get/set 5k',    'bench/scenarios/vector_ops.av'],
  ['newtype baseline 20k', 'bench/scenarios/newtype_bare.av'],
  ['newtype record 20k',   'bench/scenarios/newtype_record.av'],
  ['newtype variant 20k',  'bench/scenarios/newtype_variant.av'],
];

function compile(scenario, target) {
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), `aver-v8bench-${target}-`));
  const args_ = [
    'compile', scenario,
    '--target', target,
    '--name', 'main',
    '-o', tmp,
  ];
  if (target === 'wasm') {
    // Legacy WASM wants a bridge for non-handler programs. wasip1
    // matches what the in-process wasmtime harness uses in
    // `benches/comparison_bench.rs::compile_to_wasm`.
    args_.push('--bridge', 'wasip1', '--optimize', 'size');
  }
  const r = spawnSync(aver_bin, args_, { encoding: 'utf8' });
  if (r.status !== 0) {
    return { ok: false, error: (r.stderr || r.stdout || '').trim() };
  }
  return { ok: true, wasm: path.join(tmp, 'main.wasm') };
}

function benchOne(wasmPath) {
  const bytes = fs.readFileSync(wasmPath);
  const instance = instantiate(bytes);
  const entry = pickEntry(instance);
  for (let i = 0; i < warmup; i++) entry();
  return summarise(timeIters(entry, iters));
}

console.log(
  `# V8 bench (Node ${process.version}, ${iters} iters, ${warmup} warmup)\n`,
);

const colW = [22, 14, 14, 14];
const header =
  'scenario'.padEnd(colW[0]) +
  'wasm (legacy)'.padStart(colW[1]) +
  'wasm-gc'.padStart(colW[2]) +
  'speedup'.padStart(colW[3]);
console.log(header);
console.log('-'.repeat(header.length));

const results = [];
for (const [label, src] of SCENARIOS) {
  const row = { label };
  for (const target of ['wasm', 'wasm-gc']) {
    const c = compile(src, target);
    if (!c.ok) {
      row[target] = { error: c.error.split('\n')[0].slice(0, 60) };
      continue;
    }
    try {
      row[target] = benchOne(c.wasm);
    } catch (e) {
      row[target] = { error: String(e).split('\n')[0].slice(0, 60) };
    }
  }
  const wasm = row.wasm?.p50;
  const gc = row['wasm-gc']?.p50;
  const speedup = wasm && gc ? `${(wasm / gc).toFixed(2)}×` : '—';
  console.log(
    label.padEnd(colW[0]) +
      (row.wasm?.error ? 'FAIL'.padStart(colW[1]) : fmt(wasm).padStart(colW[1])) +
      (row['wasm-gc']?.error ? 'FAIL'.padStart(colW[2]) : fmt(gc).padStart(colW[2])) +
      speedup.padStart(colW[3]),
  );
  if (row.wasm?.error) console.log(`    wasm:    ${row.wasm.error}`);
  if (row['wasm-gc']?.error) console.log(`    wasm-gc: ${row['wasm-gc'].error}`);
  results.push(row);
}

if (process.env.JSON_OUT) {
  fs.writeFileSync(process.env.JSON_OUT, JSON.stringify(results, null, 2));
  console.log(`\nJSON report → ${process.env.JSON_OUT}`);
}
