# Benchmarking

Aver ships two complementary benchmark workflows: **`cargo bench`** for Criterion-driven cross-backend comparison, and **`aver bench`** for scenario-based regression checks. Both read from the same `bench/scenarios/*.av` files — change one source, both workflows pick it up.

## Layout

```
bench/scenarios/
├── *.av               # Aver source — single source of truth
├── *.toml             # manifest per scenario
benches/
├── comparison_bench.rs  # cargo bench (criterion timing across VM/WASM/Rust/self-host)
└── nan_value_bench.rs
```

The eleven historic cargo-bench programs (`fib`, `countdown`, `record`, `map_build`, `map_lookup`, `match_dispatch`, `string_interp`, `vector_ops`, `newtype_{bare,record,variant}`) live in `bench/scenarios/`. `comparison_bench.rs` reads them via `include_str!`; `aver bench` reads them through the manifest's `entry` field. Add a new scenario by dropping `bench/scenarios/foo.av` + `bench/scenarios/foo.toml`; it shows up in both harnesses.

## `aver bench` — scenario harness

Three input shapes:

```bash
aver bench foo.av                                    # ad-hoc — defaults (30 iter, 3 warmup)
aver bench foo.av --iterations=50 --warmup=5         # ad-hoc with overrides
aver bench bench/scenarios/foo.toml                  # named manifest (per-scenario tolerance + expected)
aver bench bench/scenarios/                          # directory mode, all *.toml
aver bench bench/scenarios/ --json                   # NDJSON, one line per scenario
```

`.av` is the quick path for "did I regress this fn?" — synthesized manifest with default tolerance, no `[expected]`. `.toml` is the named-and-pinned path for repeatable measurement (committed to repo, named scenario, per-scenario tolerance, future expected-byte checks). Directory mode globs every `*.toml` (skips `.av` — those need explicit invocation since they have no per-scenario knobs).

Use `.av` for one-off measurement, `.toml` for inventoried scenarios that gate `--compare baseline.json`.

### Targets

```bash
aver bench bench/scenarios/fib.toml --target=vm           # default — in-process VM
aver bench bench/scenarios/fib.toml --target=wasm-local   # wasmtime in-process (--features wasm)
aver bench bench/scenarios/fib.toml --target=rust         # native binary, subprocess per iter
```

| Target       | What runs                                                        | Spawn cost |
|--------------|------------------------------------------------------------------|------------|
| `vm`         | `vm::compile_program_with_modules` + `VM::run`, in-process       | none       |
| `wasm-local` | `aver compile --target wasm --bridge wasip1` → wasmtime instance | none       |
| `rust`       | `aver compile --target rust` + `cargo build --release` → spawn   | ~1-2 ms    |

WASM imports are stubbed in-process for bench programs that don't touch host effects (no print, no fs, no rand). Programs that need real WASI behaviour aren't bench candidates today; the stubs return `errno 0` for every call.

The Rust target spawns a fresh process every iteration — that's ~1-2 ms on macOS, dominating wall-clock for programs that finish in pure compute under that. Same shape as cargo bench's `run_external` measurements.

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

`backend.aver_version` is the package version of the binary that ran the bench (`CARGO_PKG_VERSION` at compile time). `backend.build` is `release` or `debug`. `backend.wasmtime_version` is set only for `--target=wasm-local`. `host.os`/`host.arch` come from `std::env::consts`; `host.cpus` from `std::thread::available_parallelism`.

`response_bytes` and `compiler_visible_allocs` ship as `Option<usize>` from day one but stay `null` in 0.15.1 — stdout capture and IR-level alloc counting land in 0.15.2.

### Baseline + regression gate (local)

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

Tolerances are configurable per-scenario via `[tolerance]` in the TOML (`wall_time_p50_pct = 25.0`, etc.). `--fail-on-regression` exits 1 when any gated metric exceeds budget. `--compare` is single-scenario only; directory-mode comparison is the 0.15.2 baseline-snapshot workflow.

### NDJSON output for streaming

```bash
aver bench bench/scenarios/ --json | jq -c '.scenario.name + ": " + (.iterations.p50_ms|tostring)'
```

Directory mode emits one report per line when `--json` is set. Trivially streamable to `jq`, dashboards, or downstream regression tools — the report is identical in shape across targets so consumers don't branch on `backend.name`.

### Release script integration

`tools/release.py verify()` runs the full scenario suite in directory mode as a smoke gate before publishing:

```python
run([str(REPO_ROOT / "target" / "release" / "aver"), "bench",
     str(REPO_ROOT / "bench" / "scenarios"), "--json"])
```

Numbers aren't gated yet (CI gate is 0.15.2 with checked-in baselines + cross-machine calibration); the run must succeed. Catches pipeline / VM regressions that unit tests miss — a real program that compiles fine but crashes in bytecode dispatch will surface here.

## `cargo bench` — Criterion comparison

```bash
cargo bench --bench comparison_bench --features wasm
```

Runs every scenario across VM / WASM / codegen / self-hosted. Criterion handles baselines locally:

```bash
cargo bench --bench comparison_bench --features wasm -- --save-baseline 0.15.0
cargo bench --bench comparison_bench --features wasm -- --baseline 0.15.0
```

The HTML report drops in `target/criterion/`. Use this for cross-backend comparison ("is WASM faster than VM on map_build?"); use `aver bench` for per-backend regression on a stable target.

## When to use which

| Question                                                | Workflow      |
|---------------------------------------------------------|---------------|
| "Did I regress fib on VM compared to last week?"       | `aver bench --compare` |
| "Is WASM faster than VM for map lookups?"              | `cargo bench` (cross-target HTML report) |
| "Should I block this PR for slow bench numbers?"        | `aver bench --fail-on-regression` once baselines land |
| "What's the canonical pipeline cost for this scenario?" | `aver bench --target=vm --json`, parse `iterations.p50_ms` |
| "Are these numbers from your machine or mine?"          | `report.host.os/arch/cpus`, `report.backend.aver_version/build` |

## Adding a scenario

1. `bench/scenarios/myprog.av` — the Aver source. Must define `fn main` (any return type).
2. `bench/scenarios/myprog.toml` — manifest pointing at `myprog.av`.
3. Run it: `aver bench bench/scenarios/myprog.toml`.
4. Pick up by cargo bench: add `const MYPROG_SRC: &str = include_str!("../bench/scenarios/myprog.av");` and a `tests` entry in `benches/comparison_bench.rs`.

That's the full setup. No code generation, no manifest registry, no CI changes — directory mode globs everything alphabetically.
