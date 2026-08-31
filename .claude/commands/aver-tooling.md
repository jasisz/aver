You are an expert at using the Aver toolchain in this repository. Use the installed `aver` binary directly.

## Main commands

### Run

```bash
aver run file.av
aver run file.av --module-root .
aver run file.av -- arg1 arg2 arg3
aver run file.av --module-root . --wasip2
```

- Aver program args are available through `Args.get()`
- `--record <dir>` records effect traces for replay
- A file names a program: the entry module plus everything it reaches
  through `depends [...]`, plus the `[providers]` bindings of the project's
  `aver.toml`. When the program reaches a capability that `aver.toml` binds
  to a Rust package, `run` builds that provider host once (the first build
  says which packages it links and where they come from) and reuses it from
  the cache afterwards; with `--wasip2` it adapts the WIT-lowerable bindings
  to Component Model imports, while `--wasm-gc` adapts the complete provider
  value vocabulary through the contract-derived raw ABI. `--self-host` has no
  provider host and refuses such a program with
  `error[capability-provider-unhosted]`.
  A project without `[providers]` never invokes Cargo

### Check

```bash
aver check file-or-dir --module-root .
```

`check` reports every module of the program — the entry plus everything it
reaches through `depends [...]` — leaves first, one `Check:` section per
module; a directory is the union of the programs rooted at each file, each
module reported once. Embedded standard modules are typed but not reported.
Any module with an error fails the command. `check` handles static contract
diagnostics:
- missing `intent =`
- missing `?` descriptions on relevant functions
- missing `verify` on pure, non-trivial, non-`main` functions
- coverage-style warnings for thin `verify` examples
- file size warnings
- exposed names nothing in the checked program imports (`unused-expose`);
  a directory input judges exposes over every program in it, a single
  file only over its own program, so a name used solely by a sibling
  program outside the input is reported

Warnings do not make `check` fail.

### Capabilities

```bash
aver capabilities file.av --module-root .
aver capabilities file.av --module-root . --json
```

`capabilities` prints the target-total binding manifest for every capability in
the loaded dependency closure. Each contract has exactly one row for `vm`,
`rust`, `wasm-gc`, and `wasip2`: `provided`, `host-bound`, or
`unsupported(reason)`. Declared and actually required operation sets are shown
separately, so an unused contract is auditable without blocking a target.

Use `--json` for the deterministic `schemaVersion = 1` envelope consumed by
tooling and future adapter planners. `capability-provider-missing` means a target
can accept a binding but none was installed; `capability-target-unsupported`
means that target has no adapter mechanism for the contract and carries a stable
reason code.

### Verify

```bash
aver verify file-or-dir --module-root .
aver verify file-or-dir -j 8
aver verify file-or-dir --wasm-gc
```

`verify` runs the declared `left => right` examples and laws of every module
of the program — the entry plus everything it reaches through
`depends [...]` — leaves first, one `Verify:` section per module, and a
`Summary: N modules | …` line. A directory is the union of the programs
rooted at each file, each module verified once. Embedded standard modules
are not sampled (their blocks are verified per release). A failing case in
any module fails the command; pointing at a single module is still the fast
way to iterate on it.

VM verification resolves one project graph and prepares each project module
once, then runs independent files and declared cases in a bounded worker pool.
`-j N` / `--jobs N` sets that pool's maximum size; it defaults to the machine's
available parallelism. `-j 1` disables concurrency without giving up graph
reuse, which makes it the useful comparison/debugging mode. Reports are always
collected in input, module, block, and case order, so sequential and parallel
runs emit identical text/JSON and failure coordinates. `--hostile` keeps the
base/profile cases of a block sequential, and `--wasm-gc` keeps cases within a
module sequential; independent input files may still share the bounded pool.

It fails on:
- mismatched examples
- parse/type errors
- execution errors
- cases it could not answer within their step budget

It is not a coverage tool.

Every case runs under a per-case budget of VM opcodes — 1,000,000 by default.
A case that exceeds it is **declined**: a third outcome, counted separately
from passes and failures, reported as `fail[verify-declined]: case not
answered`, and failing the command. A decline is not a counter-example. It
says the case was not checked, which is why it can never be read as a pass.

A project whose corpus legitimately contains expensive cases raises the
budget for the function it knows about, in `aver.toml`, with a written reason
— see `[verify]` below. The report then names the cases the raise bought:

```
  ✓ checkScript      1119/1119
    checkScript case 41: 8.2M steps (limit 50M, aver.toml [[verify.costly]] fn = "checkScript")
```

`--wasm-gc` runs the same cases on the same budget, converted to wasmtime
fuel by one documented factor, so a case is runnable on both lanes or on
neither.

A file whose program `verify` could not run — a type error anywhere in it,
a backend or provider failure, a refusal by verify itself — is **not
checked**, and the report says so rather than leaving it out. Such files are
listed under a heading naming the reason, with the number of verify blocks
that went unchecked with each one; the module that carries the fault is the
one named, not the entry that depends on it. The summary line gains a
`| N file(s) not checked` member and is never green while N is above zero,
and `--json` carries `files_skipped` and `blocks_unchecked` in its summary
record. Modules that did verify before the walk stopped still report their
cases. A stop that happens before the first module runs — a project file that
does not parse, for instance — counts every module of that program, not just
the entry. The buckets that say `aver check` will not help are the ones verify
itself is known to own: a `--wasm-gc` backend refusal, a provider composition
failure, and verify's own refusals. Anything else is treated as a source error
and points at `aver check`.

Declined cases and unchecked files are independent counts and a run can carry
both at once: the summary states each of them separately, `--json` carries
`cases_declined` (only when something was declined) alongside `files_skipped`
and `blocks_unchecked`, and either one above zero keeps the run out of green
and out of exit code 0.

When the project's `aver.toml` binds providers and a module reaches one of
those capabilities, the cases run inside the cached provider host, so a
configured pure provider executes in ordinary VM cases. An exact operation
`given` remains a case-local override. Project bindings unrelated to a module
are ignored for that module instead of causing a skip or a fake type error.
`verify --wasm-gc` has no configured-provider adapter yet and refuses such a
program; ordinary `run --wasm-gc` and `replay --wasm-gc` do use the cached
provider host.

`--wasm-gc` (0.17.3+) executes the same cases via the wasm-gc backend instead of the VM — cross-target check that catches divergence between VM and wasm-gc codegen on equality. The host decodes a single Bool per case (wasm-gc lowers `==` per-type via eq_helpers natively). Failure diagnostics show the actual runtime value for primitive return types (Int/Float/Bool/String). Trace projections (`.trace.*`), classified-effect Oracle stubs (`given X: Time = stub`), and case bodies mentioning `BranchPath` are rejected upfront with a pointer back to VM verify — those features depend on namespace-value dispatch and runtime override that the wasm-gc backend doesn't have yet.

### Format

```bash
aver format .
aver format examples
aver format examples --check
```

`format` accepts files or directories and walks `.av` files recursively.

### Audit

```bash
aver audit file-or-dir --module-root .
```

`audit` is the single-shot CI gate that runs all three axes at once:

1. static checks (same diagnostics as `check`)
2. `verify` execution (same as `verify`)
3. `format --check` (structural compliance)

Like `check` and `verify`, `audit` walks the whole program: the entry plus
everything it reaches through `depends [...]`, leaves first, one `Audit:`
section per module. A directory is the union of the programs rooted at
its files, each module audited once, so a dependency outside the
directory is audited too. Embedded standard modules are typed but not
audited.

When the project's `aver.toml` binds providers, the verify axis runs through
the same cached provider host as `aver verify`. Static checks and formatting
remain provider-neutral. A project without `[providers]` never invokes Cargo.

Output is a flat list of `error[slug]:` / `warning[slug]:` lines plus a
summary footer: `N modules | X check errors | Y verify failures | Z format`.
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

### Shape

```bash
aver shape file.av
aver shape file.av --summary
aver shape file.av --json
aver shape file.av --lint           # opt-in lint vs aver.toml expectations
aver shape file.av --module-root .

# Corpus mode: pass a directory and `aver shape` walks every .av
# underneath, prints a per-file table + aggregate Kind / Layer /
# archetype distributions, and runs the same --lint check against
# every file in one pass.
aver shape src/
aver shape src/ --summary           # aggregate only, no per-file table
aver shape src/ --json              # NDJSON, one object per file
aver shape src/ --lint              # exit 1 if any file mismatches its expected layer
```

`aver.toml` config:

```toml
# Per-project layer fingerprints — override the built-in v0 baseline.
[[shape.layer]]
name = "Domain"
match = 40
recursion = 25
pipeline = 0
orchestration = 5
helpers = 30

# Path → expected layer. `--lint` flags mismatches; without these
# entries, `--lint` is a silent no-op.
[[shape.expected]]
glob = "src/parse/**"
layer = "Parse"
```

Static module-shape analyzer — an *architectural smell radar*, not a classifier of truth. The histogram is the fact; Kind and Layer are interpretation. Output is auditable: every interpretation comes with the metric that drove it (confidence + margin + top-3 candidates for Layer; the rule that mapped the vector to Kind), so reviewers can decide whether to trust the label.

Three views in one run:

1. **Per-fn archetype** — 14 labels (`scc-mutual`, `structural-recursion`, `match-dispatcher`, `pipeline-result`, `manual-result-adapter`, `renderer-formatter`, `match-on-value`, `orchestration`, `effectful-leaf`, `let-pipeline`, `constructor-wrapper`, `data-as-function`, `trivial-helper`, `pure-expression`). Multi-label per fn; output lists every label that fires plus a primary pick.
2. **ModuleShape vector + Kind** — 5 dims (`purity`, `entry`, `state_shape`, `type_surface`, `api_shape`). Kind is a single label projected from the vector: `ServiceClient`, `Orchestration`, `SmartConstructor`, `DataModule`, `PureHelpers`, `Library`, `EffectfulLibrary`, `EffectfulShell`. `purity` is `Pure` / `ClassifiedEffectful` (all effects are Oracle one-shot req/resp shape) / `ShellEffectful` (contains shell/lifecycle effect like `Tcp.listen` — Oracle skips by design, not because the classifier doesn't recognize it).
3. **Architectural Layer** — `Domain | Parse | Command | AiStrategy | RenderUi | Infra` by Euclidean distance between the per-module archetype histogram and the fingerprint table. Two metrics: `confidence` (absolute fit to the best fingerprint) and `margin` (distance gap to runner-up). Low confidence OR low margin marks the verdict `uncertain`, with explicit "best: X" wording so the user doesn't read it as a hard label. Runners-up (top-3 closest layers with distances) are always printed so the user sees how decisive the call is. Confidence is penalized on tiny modules (<5 fns capped at 0.2, <10 fns softened by 0.7×).

Verification appears as an orthogonal section — what verify blocks the source carries (`Cases`, `Laws`, `Trace`, `Mixed`), how many blocks, and per-fn coverage. Static read of the source, doesn't run VM.

Use cases:
- "What is this module structurally?" — first glance before reading
- "Does the directory layer match what the histogram looks like?" — architectural lint (full `--lint` mode + `[[shape.expected]]` config is the next iteration)
- LLM context enrichment — Kind + ModuleShape are stable per-module facts worth attaching to AI prompts about that file

Notes:
- `--summary` collapses per-fn listing to the header + histogram; same content otherwise
- `--json` emits an audit-friendly structure with `facts` + `vector` + `kind` + `histogram` + `layer` + `fns` all side by side, so consumers can pick any layer

### Compile

```bash
aver compile file.av -o /tmp/out --module-root .
aver compile file.av --target wasm-gc -o /tmp/out
aver compile file.av --target wasip2 -o /tmp/out
aver compile file.av --target wasm-gc --optimize size -o /tmp/out
aver compile file.av --preset cloudflare --handler handler -o /tmp/out
aver compile file.av --emit-ir-after=PASS
aver compile file.av --explain-passes
```

- Default: Rust codegen, emits a modular Cargo project
- `--target wasm-gc`: native WebAssembly GC + tail-call output. Self-contained binary, engine handles GC/recursion, per-instantiation helpers DCE'd to what each program calls. Modern host baseline (Chrome 119+, Firefox 120+, Safari 18.2+, wasmtime 25+, Node 22+, Cloudflare Workers).
- `--target wasip2`: WASI 0.2 / Component Model output for wasmtime and other component hosts. It wraps the wasm-gc core with `wit-component`; see `docs/wasip2.md` for the supported effect surface.
- `--optimize size|speed`: post-process with binaryen `-Oz` (size) or `-O3` (speed). It drops the name section, so a wasm trap reports `<wasm function N>` instead of the name of the function that trapped — `aver compile` says so on stderr for a program whose named helpers it is about to strip.

- `--preset cloudflare --handler <fn>`: Cloudflare Workers pack — `--target wasm-gc --pack cloudflare`, drops `worker.js` + `wrangler.toml` next to the wasm. `<fn>` must have signature `Fn(HttpRequest) -> Http.Response`.
- `--emit-ir-after=PASS`: print the IR snapshot after the named pipeline stage and exit before codegen. PASS ∈ { `parse`, `tco`, `typecheck`, `interp_lower`, `buffer_build`, `resolve`, `last_use`, `analyze`, `escape`, `build_symbols`, `name_resolve`, `refinement_lower`, `contract_lower`, `law_lower` }. `diff -u` between two stages shows exactly what each pass rewrote.
- `--explain-passes`: run the full pipeline (no codegen) and print a per-pass diagnostic report — tail-call conversions, interpolations lowered, fusion sites rewritten + sinks synthesized, slots resolved, last-use markers annotated, alloc/recursion facts. Drives failable-invariant CI checks ("fail if buffer_build no longer fires on the canonical shape", "fail if hot fn loses no-alloc status"). Pair with `--json` for typed-per-stage shape: `{schema_version: 1, passes: [{stage, data: {...stage-specific fields}}, ...]}` — buffer_build's `data` exposes `rewrites`, `synthesized`, `sinks`, `rewrites_by_sink`; analyze's exposes `total_fns`, `no_alloc_fns`, `recursive_fns`, `mutual_tco_members`. `jq '.passes[] | select(.stage=="buffer_build") | .data.rewrites'` instead of regex-parsing summary strings.

**`Map` size on wasm-gc.** A `Map<K, V>` compiled to `wasm-gc` (and therefore to `wasip2`, which wraps it) is an open-addressing table that starts at 16 buckets and doubles whenever an insert would take it past three quarters full, rehashing its entries into the wider table. `Map.set` therefore takes keys until memory runs out, the same as on the VM and the Rust target, and no size is a cliff. The table used to be fixed at 16384 buckets, and the 16385th distinct key had nowhere to go; that ceiling is gone.

### Artifact certificates

```bash
aver compile app.av --target wasm-gc --certify -o out/
aver compile app.av --target wasip2 --certify -o out/
aver-cert verify out/app.wasm out/cert
aver-cert verify out/app.component.wasm out/cert
aver-cert check out/app.wasm out/cert
aver-cert explain out/app.wasm out/cert
aver cert verify out/app.wasm out/cert
aver cert verify out/app.component.wasm out/cert
aver cert check out/app.wasm out/cert
aver cert explain out/app.wasm out/cert
```

`--certify` emits a version-1 artifact certificate for admitted exports of an
exact wasm-gc module or wasip2 component. A wasip2 package hashes the delivered
`.component.wasm`; its declared prefix/core/suffix envelope binds the exact
embedded core bytes consumed by the existing Wasm wall. Install `aver-cert`
separately; it is an independently versioned verifier using Lean 4.32. A
crates.io compiler install needs `--features wasm` for wasm-gc, plus `wasip2`
for component output. Verification also requires a standard Elan installation
for the pinned toolchain.

`check` is the faster development preflight. It trusts the freshly built or
explicitly cached `.olean` closure, skips the final `leanchecker --fresh`
replay, and prints `CHECKED`, never `CERTIFIED`. Use strict `verify` for release
or admission gates.

`aver cert ...` is only a subprocess shortcut. It forwards the original
arguments, standard streams, and exit status to a sibling `aver-cert` binary or
one on `PATH`; `aver` contains no linked verification fallback. `explain`
performs the same full check as `verify` before printing the report, and
`inspect` is an alias of `explain`.

This command is different from `aver verify`: source `verify` runs examples,
whereas `aver cert verify` kernel-checks a behavioral certificate for compiled
WebAssembly. See the
[certificate guide](https://github.com/jasisz/aver/blob/main/docs/certification.md)
and
[architecture](https://github.com/jasisz/aver/blob/main/docs/certification-architecture.md)
for the admitted families and trust boundary.

### Bench

```bash
aver bench foo.av                                            # ad-hoc, defaults (30 iter, 3 warmup)
aver bench foo.av --iterations=50 --warmup=5                 # ad-hoc with overrides
aver bench bench/scenarios/fib.toml                          # named manifest
aver bench bench/scenarios/fib.toml --json                   # structured report
aver bench bench/scenarios/                                  # directory mode (every *.toml)
aver bench bench/scenarios/ --json                           # NDJSON
aver bench bench/scenarios/fib.toml --target=wasm-gc         # embedded wasmtime, requires --features wasm
aver bench bench/scenarios/fib.toml --target=wasm-gc-v8      # the same bytes under Node/V8
aver bench bench/scenarios/fib.toml --target=rust            # native binary, subprocess per iter
aver bench bench/scenarios/fib.toml --save-baseline base.json
aver bench bench/scenarios/fib.toml --compare base.json --fail-on-regression
aver bench bench/scenarios/ --save-baseline bench/baselines/<host>-<arch>-vm.json   # capture baseline (NDJSON)
aver bench bench/scenarios/ --baseline-dir bench/baselines/ --fail-on-regression   # CI gate
```

- Three input shapes: `.av` (ad-hoc, defaults + `--iterations` / `--warmup` overrides), `.toml` (named manifest with per-scenario tolerance + expected shape), directory (globs `*.toml`).
- Four targets: `vm` (default, in-process), `wasm-gc` (wasmtime in-process), `wasm-gc-v8` (Node/V8 subprocess), and `rust` (native binary).
- Reports include `backend` (aver version, build, wasmtime version) and `host` (os/arch/cpus) so cross-machine runs disambiguate.
- `--save-baseline` works in both single-scenario (pretty JSON) and directory (NDJSON) mode. `--compare` is single-scenario only.
- `--baseline-dir DIR` auto-picks `<host.os>-<host.arch>-<backend.name>.json` from `DIR`. Silent skip when no matching baseline exists — single workflow gates wherever a baseline is pinned. CI uses this.
- See the [benchmark guide](https://github.com/jasisz/aver/blob/main/docs/bench.md) for the full reference.

### Proof

```bash
aver proof file.av -o /tmp/proof --module-root . --verify-mode auto
```

Lean export modes:
- `auto`
- `sorry`
- `theorem-skeleton`

`--check` builds the export with `lake` and gates on the result (`--check-json`
for a machine-readable summary); `--gate <baseline>` / `--write-baseline` are
the proof ratchet; `--minimize` (Lean, implies `--check`) collapses each
auto-proof to the single tactic that actually closed it. See
[docs/lean.md](lean.md) for the proof workflow, the `--check` summary fields,
and `--minimize`.

### Replay

```bash
aver replay recordings/ --test --diff
```

Use replay for effectful debugging and regression capture.

### Agent connect

```bash
aver agent-connect            # this project: skills + a marked AGENTS.md section
aver agent-connect --global   # ~/.claude/skills/ instead, AGENTS.md untouched
aver agent-connect --print    # the language guide on stdout, nothing written
```

The language guide and this toolchain guide ship inside the `aver` binary, so an install carries them and nothing has to be fetched. `aver agent-connect` writes them out as `.claude/skills/aver/SKILL.md` and `.claude/skills/aver-tooling/SKILL.md`, then creates or refreshes a short pointer section in `AGENTS.md` between `<!-- aver agent-connect: start -->` and `<!-- aver agent-connect: end -->`.

There is no blessed agent workflow here. Take the files and keep whatever prompt, harness, or command you already use — `--print` exists precisely so an agent that wants one file rather than a skill directory can have it.

Safety:
- bytes outside the `AGENTS.md` markers are never touched, and a file with no markers is appended to, never rewritten
- a `SKILL.md` that exists without the `aver agent-connect: managed file` marker is refused by name, never overwritten
- re-running produces no diff; every line of the summary says `created`, `updated`, or `unchanged`
- exit is nonzero only on a refusal or an IO error

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

1. `aver agent-connect` once, so the language and toolchain guides are in the project
2. `aver context <entry> --budget 10kb`
3. if needed, raise budget or target a specific module
4. only then open raw source files

## aver.toml

Project-level config (deployment guardrails + check tweaks):

```toml
[effects.Http]
hosts = ["api.example.com", "*.internal.corp"]

[effects.Disk]
paths = ["./data/**"]

[effects.Env]
keys = ["APP_*", "TOKEN"]

[effects.Tcp]
connect_timeout_secs = 5
request_idle_timeout_secs = 30
max_connections = 256

[[check.suppress]]
slug = "verify-coverage"
files = ["domain/checks.av"]
fn = "eachInBranch"            # optional exact function scope
reason = "Its Result error arm is uninhabited by constructible inputs."

[verify]
step-limit = 1_000_000          # per-case opcode budget (default)
max-cases  = 10_000             # ceiling on `given`-domain expansion (default)

[[verify.costly]]
fn         = "checkScript"
files      = ["domain/scriptcases*.av"]
step-limit = 50_000_000          # raise the per-case budget for this fn
max-cases  = 40_000              # raise the case ceiling for this fn
reason     = "Bitcoin Core corpus includes consensus-max 10,000-byte scripts"
```

Effect-host / path / key allowlists narrow which hosts, files, and env keys the runtime will admit. Tcp's positive-integer settings configure connection establishment, one-shot request idle timeouts, and one shared limit for established/accepted connections plus in-flight dials; they never impose a deadline on persistent session I/O. Unknown or misplaced keys inside an effect section are errors. `[[check.suppress]]` lets a project waive specific lint slugs in specific paths, optionally for one exact function, with a reason.

Disk path patterns have deliberately small, explicit semantics:

| Pattern | Meaning |
|---|---|
| `path` or `path/**` | That path and its entire subtree |
| `.`, `./`, or `./**` | The project-relative subtree |
| `/` or `/**` | Every absolute path from the filesystem root |
| `**` | Invalid; use `./**` or `/**` to state the intended boundary |

An empty pattern, unsupported `*` placement, or a `..`-rooted pattern is also a config-load error. An absent `paths` key or `paths = []` keeps the existing allow-all behavior. Matching is string-only: Aver normalizes `.` and `..` in the caller-supplied path without resolving it against the working directory or touching the filesystem. A project-relative pattern therefore does not admit an absolute spelling of the same in-project file.

`[verify]` budgets in detail:

- `step-limit` is the per-case opcode budget `aver verify` installs before every case. The default, 1,000,000, is what stops a tail-recursive function without a base case: Aver's tail-call optimization turns that into a goto-loop with no stack growth, so nothing else would. Raising it globally trades that bail-out away for every case in the project; `[[verify.costly]]` trades it away for one function, which is almost always what you mean.
- `[[verify.costly]]` raises the budgets for the verify blocks of one function. `fn` is required. `reason` is required and must be non-empty — the same rule `[[check.suppress]]` has, for the same reason: a budget nobody explained is a budget nobody can retire. `files` is optional and uses the same anchored globs, matched against the file's path relative to the module root.
- An entry carries both dials, and must set at least one: `step-limit` for a fn whose cases are slow, `max-cases` for a fn whose `given` domain is wide, both for a fn that is both. An entry that sets neither is a config error, and so is a dial that is not above the number already in force — `[[verify.costly]]` says "this case is expensive, give it room", and a lower number says the opposite. So an entry only ever raises, and where several entries match one block the most permissive value wins, for both dials. First-match-wins would make the order of the entries part of what the file means: an entry added near the top would silently change which one governs a block elsewhere, and a reader would have to scan the whole list to know what applies. This way each entry is an independent statement about one function, and adding one can only loosen, never re-point.
- The report names every case that needed more than the project default, with its step count and the entry that raised it, so a raise is never a silent licence.
- An entry that matched no verify block during a run is reported on stderr — separately for "matched no verified file" and "matched files but no block of that fn" — and never changes the exit code. An entry another entry out-granted is not reported: it matched a live block, and losing a tie-break says nothing about whether the declaration is still true of the project.
- `max-cases` is the ceiling on how many cases one verify block may expand into, on both sides: the `given` domain the parser expands and the `--hostile` cartesian the runner expands on top of it. Both fail loudly with the count rather than truncating, because a truncated case list is a claim you did not make. Each expanded case clones an expression pair, so raising this costs parse-time memory in proportion to the new ceiling. `[verify] max-cases` moves it for the whole project; a `[[verify.costly]]` entry moves it for the blocks of the one function it names, which is usually what a wide corpus actually means.
- The ceiling belongs to the file, not to one command. Every command that reads your `.av` files parses them under it — `check`, `run`, `compile`, `proof`, `audit`, `format`, `shape`, `context`, `why`, `capabilities`, `replay`, `bench` — as does the dependency walk each of them performs. A `given` domain your project declared legal is legal at every door, a domain over the ceiling is refused at every door, and the message names the number that actually applied rather than the built-in one.
- Neither setting changes what a program means. The same source verifies the same way under any budget; it just gets more or less room to finish.

`[[check.suppress]]` rules in detail:

- `files` globs match the file's path **relative to the module root**, so a leading `./` is insignificant on either side and every spelling of the same file (`aver check domain/version.av`, `aver check ./domain/version.av`, `aver check .`, an absolute path, or a dependency module of the program) honours the same rule. A rule with no `files` key applies everywhere.
- `fn` is optional. When present, the rule matches only diagnostics carrying that exact function name; a file-level diagnostic cannot accidentally match it. This is the narrow form for an unavoidable residue such as one `verify-coverage` Result arm. Without `fn`, the rule keeps its existing file-wide meaning.
- `aver check` and `aver audit` apply the same rules. Both print how many warnings a file's waivers removed.
- Suppression applies to warnings only. It can never hide an `error[...]`, a verify failure, or the `needs-format` result, so a waiver can never change a command's exit code.
- A rule that removes nothing during a whole-directory run (`aver check .`, `aver audit .`) is reported on stderr, telling you whether its globs matched no checked file at all or matched files whose warning no longer fires. Single-file runs stay quiet, since they legitimately never exercise rules scoped to other paths.
