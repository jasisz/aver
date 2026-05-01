You are an expert at using the Aver toolchain in this repository. Use the installed `aver` binary directly.

## Main commands

### Run

```bash
aver run file.av
aver run file.av --module-root .
aver run file.av -- arg1 arg2 arg3
```

- Aver program args are available through `Args.get()`
- `--record <dir>` records effect traces for replay

### Check

```bash
aver check file-or-dir --module-root . --deps
```

`check` handles static contract diagnostics:
- missing `intent =`
- missing `?` descriptions on relevant functions
- missing `verify` on pure, non-trivial, non-`main` functions
- coverage-style warnings for thin `verify` examples
- file size warnings

Warnings do not make `check` fail.

### Verify

```bash
aver verify file-or-dir --module-root . --deps
```

`verify` runs only declared `left => right` examples.
It fails on:
- mismatched examples
- parse/type errors
- execution errors

It is not a coverage tool.

### Format

```bash
aver format .
aver format examples
aver format examples --check
```

`format` accepts files or directories and walks `.av` files recursively.

### Audit

```bash
aver audit file-or-dir --module-root . --deps
```

`audit` is the single-shot CI gate that runs all three axes at once:

1. static checks (same diagnostics as `check`)
2. `verify` execution (same as `verify`)
3. `format --check` (structural compliance)

Output is a flat list of `error[slug]:` / `warning[slug]:` lines plus a
summary footer: `N files | X check errors | Y verify failures | Z format`.
Any non-zero count fails the command.

- warnings (e.g. `independence-hazard`, `non-tail-recursion`) do not fail
  the audit — they are advisory
- errors come from the same machinery as `check` / `verify` / `format`, so
  slugs are stable and match `docs/diagnostics-slugs.md`
- prefer `aver audit` over chaining `check && verify && format --check` —
  it runs the pipeline once and reports everything in one place

Use it before showing a snippet to the user or committing docs examples;
it catches illegal `?!` usages, match-arm body-on-next-line parse errors,
and effect-type mismatches that a naked `aver run` can miss when the VM
short-circuits on the first failure.

`--hostile` (0.13+) layers adversarial worlds on top of every
`verify <fn> law` block — typed `given`s get type-boundary values,
classified effects get hostile profiles. Failures use the separate
slug `verify-hostile-mismatch` so CI can route declared-world vs
adversarial-world regressions to different channels.

### Context

```bash
aver context file.av --module-root .
```

Default:
- `--depth auto`
- `--budget 10kb`

This is the preferred AI discovery workflow:

1. start with a small budget
2. inspect the architecture map
3. look at selection metadata
4. zoom in only where needed

Examples:

```bash
aver context examples/modules/app.av --budget 10kb
aver context projects/workflow_engine/main.av --module-root projects/workflow_engine --budget 24kb
aver context projects/workflow_engine/main.av --module-root projects/workflow_engine --json --budget 24kb --output projects/workflow_engine/CONTEXT.json
```

Notes:
- `--depth N` and `--depth unlimited` bypass the auto-budget behavior
- `--decisions-only` exports only `decision` blocks
- selection metadata is printed to stdout and embedded in JSON output

### Compile

```bash
aver compile file.av -o /tmp/out --module-root .
aver compile file.av --target wasm -o /tmp/out
aver compile file.av --target wasm --wasm-opt oz -o /tmp/out
aver compile file.av --emit-ir-after=PASS
aver compile file.av --explain-passes
```

- Default: Rust codegen, emits a modular Cargo project
- `--target wasm`: standalone WASM module with aver/* imports
- `--wasm-opt oz`: post-process with binaryen for ~50% size reduction
- `--emit-ir-after=PASS`: print the IR snapshot after the named pipeline stage and exit before codegen. PASS ∈ { `parse`, `tco`, `typecheck`, `interp_lower`, `buffer_build`, `resolve`, `last_use`, `analyze` }. `diff -u` between two stages shows exactly what each pass rewrote.
- `--explain-passes`: run the full pipeline (no codegen) and print a per-pass diagnostic report — tail-call conversions, interpolations lowered, fusion sites rewritten + sinks synthesized, slots resolved, last-use markers annotated, alloc/recursion facts. Drives failable-invariant CI checks ("fail if buffer_build no longer fires on the canonical shape", "fail if hot fn loses no-alloc status"). Pair with `--json` for typed-per-stage shape: `{schema_version: 1, passes: [{stage, data: {...stage-specific fields}}, ...]}` — buffer_build's `data` exposes `rewrites`, `synthesized`, `sinks`, `rewrites_by_sink`; analyze's exposes `total_fns`, `no_alloc_fns`, `recursive_fns`, `mutual_tco_members`. `jq '.passes[] | select(.stage=="buffer_build") | .data.rewrites'` instead of regex-parsing summary strings.

### Bench

```bash
aver bench foo.av                                            # ad-hoc, defaults (30 iter, 3 warmup)
aver bench foo.av --iterations=50 --warmup=5                 # ad-hoc with overrides
aver bench bench/scenarios/fib.toml                          # named manifest
aver bench bench/scenarios/fib.toml --json                   # structured report
aver bench bench/scenarios/                                  # directory mode (every *.toml)
aver bench bench/scenarios/ --json                           # NDJSON
aver bench bench/scenarios/fib.toml --target=wasm-local      # requires --features wasm
aver bench bench/scenarios/fib.toml --target=rust            # native binary, subprocess per iter
aver bench bench/scenarios/fib.toml --save-baseline base.json
aver bench bench/scenarios/fib.toml --compare base.json --fail-on-regression
```

- Three input shapes: `.av` (ad-hoc, defaults + `--iterations` / `--warmup` overrides), `.toml` (named manifest with per-scenario tolerance + expected shape), directory (globs `*.toml`).
- Three targets: `vm` (default, in-process), `wasm-local` (wasmtime in-process), `rust` (native binary).
- Reports include `backend` (aver version, build, wasmtime version) and `host` (os/arch/cpus) so cross-machine runs disambiguate.
- `--save-baseline` / `--compare` need a `.toml` manifest (per-scenario tolerance lives there).
- See [docs/bench.md](docs/bench.md) for the full reference.

### Proof

```bash
aver proof file.av -o /tmp/proof --module-root . --verify-mode auto
```

Lean export modes:
- `auto`
- `sorry`
- `theorem-skeleton`

### Replay

```bash
aver replay recordings/ --test --diff
```

Use replay for effectful debugging and regression capture.

## Recommended workflows

### Logic bug

1. add or tighten a `verify`
2. run `aver verify ...`
3. fix code
4. keep the example

### Effect bug

1. run with `--record`
2. inspect replay artifact
3. run `aver replay ... --test --diff`

### Project discovery

1. `aver context <entry> --budget 10kb`
2. if needed, raise budget or target a specific module
3. only then open raw source files

## aver.toml

Project-level config (deployment guardrails + check tweaks):

```toml
[effects.Http]
hosts = ["api.example.com", "*.internal.corp"]

[effects.Disk]
paths = ["./data/**"]

[effects.Env]
keys = ["APP_*", "TOKEN"]

[[check.suppress]]
slug = "non-tail-recursion"
files = ["**/eval/**"]
reason = "Tree-walking interpreter — CPS would destroy correspondence."
```

Effect-host / path / key allowlists narrow which hosts, files, and env keys the runtime will admit. `[[check.suppress]]` lets a project waive specific lint slugs in specific paths with a reason.
