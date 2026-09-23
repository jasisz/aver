# Benchmarking

Aver has two benchmark workflows. **`cargo bench`** uses Criterion to compare backends against each other, and **`aver bench`** runs scenario-based regression checks. Both read the same `bench/scenarios/*.av` files, so a change to one source reaches both.

## Layout

```
bench/scenarios/
├── *.av               # Aver source — single source of truth
├── *.toml             # manifest per scenario
benches/
├── comparison_bench.rs  # cargo bench (criterion timing across VM/WASM/Rust/self-host)
└── nan_value_bench.rs
```

The eleven historic cargo-bench programs (`fib`, `countdown`, `record`, `map_build`, `map_lookup`, `match_dispatch`, `string_interp`, `vector_ops`, `newtype_{bare,record,variant}`) live in `bench/scenarios/`. `comparison_bench.rs` reads them with `include_str!`, and `aver bench` finds them through the manifest's `entry` field. To add a scenario, put `bench/scenarios/foo.av` and `bench/scenarios/foo.toml` in place and it appears in both harnesses.

## `aver bench` — scenario harness

It takes three kinds of input: a single `.av` file, a `.toml` manifest, or a directory.

```bash
aver bench foo.av                                    # ad-hoc — defaults (30 iter, 3 warmup)
aver bench foo.av --iterations=50 --warmup=5         # ad-hoc with overrides
aver bench bench/scenarios/foo.toml                  # named manifest (per-scenario tolerance + expected)
aver bench bench/scenarios/                          # directory mode, all *.toml
aver bench bench/scenarios/ --json                   # NDJSON, one line per scenario
```

An `.av` file is the quick way to ask "did I regress this fn?". The harness synthesizes a manifest with the default tolerance and no `[expected]`. A `.toml` manifest is for repeatable measurement: it is committed to the repo, names the scenario, sets a per-scenario tolerance, and will later carry expected-byte checks. Directory mode globs every `*.toml` and skips `.av` files, which have no per-scenario settings and must be run explicitly.

Use `.av` for a one-off measurement. Use `.toml` for scenarios kept in the inventory that gate `--compare baseline.json`.

### Targets

```bash
aver bench bench/scenarios/fib.toml --target=vm           # default — in-process VM
aver bench bench/scenarios/fib.toml --target=wasm-gc      # embedded wasmtime, GC engine (--features wasm)
aver bench bench/scenarios/fib.toml --target=wasm-gc-v8   # subprocess Node/V8 host
aver bench bench/scenarios/fib.toml --target=rust         # native binary, subprocess per iter
```

| Target        | What runs                                                                  | Spawn cost |
|---------------|----------------------------------------------------------------------------|------------|
| `vm`          | `vm::compile_program_with_modules` + `VM::run`, in-process                 | none       |
| `wasm-gc`     | `aver compile --target wasm-gc` → embedded wasmtime engine (GC + tail-call) | none       |
| `wasm-gc-v8`  | `aver compile --target wasm-gc` → subprocess `node` running it via V8       | ~5-15 ms   |
| `rust`        | `aver compile --target rust` + `cargo build --release` → spawn             | ~1-2 ms    |

For programs that use no real host effects (no print, no fs, no rand), the wasm-gc bench target stubs the `aver/*` host imports in-process. The stubs return `errno 0` for every call, so programs that need real I/O cannot be benchmarked on this target today.

The Rust target spawns a new process on every iteration. That costs about 1-2 ms on macOS and dominates the wall-clock time of programs whose pure compute finishes faster than that. cargo bench's `run_external` measurements work the same way.

### Manifest format

```toml
name       = "fib"          # optional; defaults to file stem
entry      = "fib.av"       # path relative to manifest dir
iterations = 30
warmup     = 3
args       = []             # CLI args passed to the program

[expected]                  # optional — checks response_bytes when populated
# response_bytes     = 5000
# response_bytes_min = 1000
# response_bytes_max = 9000

[tolerance]                 # optional — used by --compare
wall_time_p50_pct = 25.0    # default 20.0
wall_time_p95_pct = 35.0    # default 30.0
```

### Report shape

```json
{
  "scenario":  { "name", "entry", "target", "iterations_count", "warmup_count" },
  "backend":   { "name", "aver_version", "build", "wasmtime_version" },
  "host":      { "os", "arch", "cpus" },
  "iterations":{ "min_ms", "max_ms", "mean_ms", "p50_ms", "p95_ms", "p99_ms" },
  "response_bytes":          null,
  "expected_match":          null,
  "passes_applied":          ["tco", "typecheck", ...],
  "compiler_visible_allocs": null
}
```

`backend.aver_version` is the package version of the binary that ran the bench (`CARGO_PKG_VERSION` at compile time). `backend.build` is `release` or `debug`. `backend.wasmtime_version` is set only for `--target=wasm-gc`. `host.os`/`host.arch` come from `std::env::consts`; `host.cpus` from `std::thread::available_parallelism`.

`compiler_visible_allocs` is filled in since 0.15.2 (the IR-level count of alloc sites, via `NeutralAllocPolicy`). `response_bytes` stays `null` until stdout capture lands later in the cycle.

### Baseline + regression gate (single scenario)

```bash
# Capture once on a stable machine
aver bench bench/scenarios/fib.toml --save-baseline /tmp/fib-baseline.json

# Compare current vs baseline
aver bench bench/scenarios/fib.toml --compare /tmp/fib-baseline.json

# Gate CI / hooks on the diff
aver bench bench/scenarios/fib.toml \
    --compare /tmp/fib-baseline.json \
    --fail-on-regression
```

Each scenario sets its own tolerances in the `[tolerance]` table of its TOML (`wall_time_p50_pct = 25.0`, etc.). `--fail-on-regression` exits 1 when any gated metric goes over budget. `compiler_visible_allocs` is gated too, by exact match: any growth past the baseline counts as a regression.

### Baseline + regression gate (directory mode, the CI shape)

```bash
# Capture all scenarios once on the target host
aver bench bench/scenarios/ \
    --target=vm \
    --save-baseline bench/baselines/<host.os>-<host.arch>-vm.json

# Gate every PR run against the committed baseline
aver bench bench/scenarios/ \
    --target=vm \
    --baseline-dir bench/baselines/ \
    --fail-on-regression
```

`--baseline-dir DIR` picks `<host.os>-<host.arch>-<backend.name>.json` from `DIR` to match the current machine. If there is no matching file, the gate is skipped without a message. One CI workflow therefore gates wherever a baseline is pinned and runs cleanly on hosts that have none. The repo currently ships `bench/baselines/macos-aarch64-vm.json`. The Linux baseline is captured automatically on the first CI run; commit `bench/baselines/linux-x86_64-vm.json` from that artifact to turn on gating there.

The CI `Bench Gate` job in `.github/workflows/ci.yml` runs `aver bench bench/scenarios/ --target=vm --baseline-dir bench/baselines/ --fail-on-regression --json` on pushes to main and to exact release-candidate branches. The results are uploaded as an artifact kept for 30 days.

### NDJSON output for streaming

```bash
aver bench bench/scenarios/ --json | jq -c '.scenario.name + ": " + (.iterations.p50_ms|tostring)'
```

With `--json`, directory mode prints one report per line, which streams easily into `jq`, dashboards or downstream regression tools. The report has the same shape for every target, so consumers do not need to branch on `backend.name`.

### Release script integration

The two-phase release requires this CI gate on the exact `release/X.Y.Z` commit before publishing. The one-shot compatibility path in `tools/release.py verify()` runs the same scenario suite locally:

```python
run([str(REPO_ROOT / "target" / "release" / "aver"), "bench",
     str(REPO_ROOT / "bench" / "scenarios"), "--json"])
```

The numbers are not gated here yet (the CI gate is 0.15.2, with checked-in baselines and cross-machine calibration); the run only has to succeed. It catches pipeline and VM regressions that unit tests miss. A real program that compiles fine but crashes in bytecode dispatch shows up here.

## `cargo bench` — Criterion comparison

```bash
cargo bench --bench comparison_bench --features wasm
```

This runs every scenario on the VM, WASM, codegen and self-hosted backends. Criterion keeps baselines locally:

```bash
cargo bench --bench comparison_bench --features wasm -- --save-baseline 0.15.0
cargo bench --bench comparison_bench --features wasm -- --baseline 0.15.0
```

The HTML report is written to `target/criterion/`. Use `cargo bench` to compare backends ("is WASM faster than VM on map_build?") and `aver bench` to catch regressions of one backend on a stable target.

## When to use which

| Question                                                | Workflow      |
|---------------------------------------------------------|---------------|
| "Did I regress fib on VM compared to last week?"       | `aver bench --compare` |
| "Is WASM faster than VM for map lookups?"              | `cargo bench` (cross-target HTML report) |
| "Should I block this PR for slow bench numbers?"        | `aver bench --fail-on-regression` once baselines land |
| "What's the canonical pipeline cost for this scenario?" | `aver bench --target=vm --json`, parse `iterations.p50_ms` |
| "Are these numbers from your machine or mine?"          | `report.host.os/arch/cpus`, `report.backend.aver_version/build` |

## Adding a scenario

1. `bench/scenarios/myprog.av`: the Aver source. It must define `fn main` (any return type).
2. `bench/scenarios/myprog.toml`: the manifest, pointing at `myprog.av`.
3. Run it: `aver bench bench/scenarios/myprog.toml`.
4. To include it in cargo bench, add `const MYPROG_SRC: &str = include_str!("../bench/scenarios/myprog.av");` and a `tests` entry in `benches/comparison_bench.rs`.

Nothing else is needed. There is no code generation, no manifest registry and no CI change, because directory mode globs every scenario in alphabetical order.
