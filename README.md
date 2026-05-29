<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/logo-dark.svg">
    <source media="(prefers-color-scheme: light)" srcset="docs/logo-light.svg">
    <img alt="aver" src="docs/logo-dark.svg" width="360">
  </picture>
</p>

# Aver

Aver is a statically typed language designed for AI to write in and humans to review, with a bytecode VM for runtime execution, a Rust backend for deployment, a WASM backend for browser and embedded targets, Lean proof export for pure logic and classified effectful laws, and Dafny verification for automated law checking via Z3.

It is built around one idea: the risky part of AI-written code is usually not syntax, it is missing intent. Aver makes that intent explicit and machine-readable:

- effects are part of the function signature
- decisions live next to the code they explain
- pure behavior lives in colocated `verify` blocks
- selected effectful behavior can be verified with explicit stubs and trace assertions
- effectful runs can be recorded and replayed deterministically

The toolchain (run / verify / check / audit / why / context / compile / bench / proof / replay) lives in [docs/cli.md](docs/cli.md). Backend-specific deep dives: [docs/bench.md](docs/bench.md), [docs/transpilation.md](docs/transpilation.md), [docs/lean.md](docs/lean.md), [docs/dafny.md](docs/dafny.md), [docs/wasm.md](docs/wasm.md), [docs/rust.md](docs/rust.md).

This is not a language optimized for humans to type by hand all day. It is optimized for AI to generate code that humans can inspect, constrain, test, and ship.

**Prompting an LLM to write Aver?** Feed it [`llms.txt`](https://averlang.dev/llms.txt) (419 lines, curated). Same file at repo root ([`llms.txt`](llms.txt)) and at [averlang.dev/llms.txt](https://averlang.dev/llms.txt) — `.av` extension, single-line match arms, qualified constructors, classified effects, `verify` block shapes, and every other rule that catches model-generated code on first try. Longer reference cut: [`llms-full.txt`](https://averlang.dev/llms-full.txt).

Website: [averlang.dev](https://averlang.dev)  
Browser playground: [averlang.dev/playground](https://averlang.dev/playground/)

Read the [Aver Manifesto](https://jasisz.github.io/aver-language/) for the longer argument, [Common Pushback](docs/pushback.md) for questions and objections, or [intent-trace](https://github.com/jasisz/intent-trace) for an empirical benchmark comparing Aver's legibility against Python variants across 108 AI-reviewed code changes.

---

## Quickstart

### Install from crates.io

```bash
cargo install aver-lang
```

Then try it with a tiny file:

```bash
cat > hello.av <<'EOF'
module Hello
    intent =
        "Tiny intro module."
    exposes [greet]

fn greet(name: String) -> String
    ? "Greets a user."
    "Hello, {name}"

fn runCli() -> Unit
    ? "Starts the CLI."
      "Prints one rendered response."
    ! [Args.get, Console.print]
    Console.print("todo")

verify greet
    greet("Aver") => "Hello, Aver"

fn main() -> Unit
    ! [Console.print]
    Console.print(greet("Aver"))
EOF

aver run      hello.av
aver run      hello.av --self-host
aver verify   hello.av
aver verify   hello.av --json
aver check    hello.av
aver check    hello.av --json
aver audit    hello.av
aver audit    hello.av --json
aver why      hello.av
aver context  hello.av
aver shape    hello.av
aver compile  hello.av -o out/
(cd out && cargo run)
```

`aver run`, `verify`, `check`, and `replay` use the bytecode VM by default. The old
`--vm` flag is gone.

`Unit` is Aver's "no meaningful value" type, roughly like `void` and rendered as `()` in diagnostics. `main` often returns `Unit`, but it can also return `Result<Unit, String>`; `aver run` treats `Result.Err(...)` from `main` as a process failure.

### Build from source

```bash
git clone https://github.com/jasisz/aver
cd aver
cargo install --path . --force                              # full release build (LTO, ~3 min)
cargo install --path . --force --profile release-fast       # local dev build  (no LTO,  ~30 s)

aver run      examples/core/calculator.av
aver run      examples/core/calculator.av --self-host
aver verify   examples/core/calculator.av
aver check    examples/core/calculator.av
aver audit    examples/core/calculator.av
aver why      examples/core/calculator.av
aver context  examples/core/calculator.av
aver compile  examples/core/calculator.av -o out/
(cd out && cargo run)
aver proof    examples/formal/law_auto.av -o proof/
(cd proof && lake build)
aver run      examples/services/console_demo.av --record recordings/
aver run      examples/core/calculator.av -e 'safeDivide(10, 2)' --record recordings/
aver run      examples/services/console_demo.av --wasm-gc --record recordings/
aver replay   recordings/ --test --diff
aver replay   recordings/ --test --check-args
aver replay   recordings/ --test --json
aver replay   recordings/rec-…json --wasm-gc
```

Recordings are byte-compatible across the VM, self-host, and wasm-gc backends — a trace written by any one of them replays cleanly under any of the three.

Requires: Rust stable toolchain.

### Editor support

For editor integration:

```bash
cargo install aver-lsp
```

Then install the VS Code extension `Aver.aver-lang`, or configure your editor to start the `aver-lsp` binary directly. See [editors/README.md](editors/README.md) for VS Code, Sublime Text, and manual LSP setup notes.

---

## Small example

```aver
module Payments
    intent =
        "Processes transactions with an explicit audit trail."
    exposes [charge]

decision UseResultNotExceptions
    date = "2024-01-15"
    reason =
        "Invisible exceptions lose money at runtime."
        "Callers must handle failure — Result forces that at the call site."
    chosen = "Result"
    rejected = ["Exceptions", "Nullable"]
    impacts = [charge]

fn charge(account: String, amount: Int) -> Result<String, String>
    ? "Charges account. Returns txn ID or a human-readable error."
    match amount
        0 -> Result.Err("Cannot charge zero")
        _ -> Result.Ok("txn-{account}-{amount}")

verify charge
    charge("alice", 100) => Result.Ok("txn-alice-100")
    charge("bob",   0)   => Result.Err("Cannot charge zero")
```

No `if`/`else`. No loops. No exceptions. No nulls. No implicit side effects.

---

## Deliberate constraints

Aver is intentionally opinionated. These omissions are part of the design, not missing features:

- no `if`/`else` — branching goes through `match`
- no `for`/`while` — iteration is recursion or explicit list operations
- no exceptions — failure is `Result`
- no `null` — absence is `Option`
- no closures — functions are top-level and explicit
- no async/await, streams, or channels — `(a, b)?!` declares independent computations; the runtime handles the rest. See [docs/independence.md](docs/independence.md)
- no hidden state behind effects — `Disk.write` does not affect a later `Disk.read`; if state matters, it lives in pure user data (see below)

The point is to remove classes of implicit behavior that are easy for AI to generate and annoying for humans to audit.

For the fuller language rationale, see [docs/language.md](docs/language.md).

---

## Pure core, effect shell

> Prove the model, not the world.

Aver splits programs into two layers:

- **Pure core** — explicit data, explicit state transitions, laws. Read-after-write consistency, ordering, accumulation — all of that is a property of *your* data structures (a `FileStore`, a `PaymentLedger`, a `WorkflowState`), proven by `verify` over pure functions.
- **Effect shell** — `Disk.*`, `Time.*`, `Tcp.*`, `Http.*`, `Env.*`. One-shot calls to the world. Oracle stubs are **stateless**: an `Env.set` does not change a later `Env.get`, a first `Time.now` does not constrain the next, a `Disk.write` does not seed a later `Disk.read`.

This is by design. Wall clocks are not monotonic in the real world (NTP, leap, suspend, VM clock skew). Filesystems are not transactional. Aver does not pretend external services have nicer laws than the platform actually promises — that would prove guarantees the OS never gave.

If a property depends on memory across effect calls, model the state in pure user code. The shell stays thin.

Runnable patterns: [`examples/formal/file_store_pure_core.av`](examples/formal/file_store_pure_core.av) + [`examples/formal/file_store_shell.av`](examples/formal/file_store_shell.av), [`examples/formal/clock_as_data.av`](examples/formal/clock_as_data.av). Background: [docs/oracle.md](docs/oracle.md#effect-stubs-are-stateless).

---

## Why Aver exists

LLMs can produce function bodies quickly. They are much worse at preserving the information reviewers actually need:

- what a function is allowed to do
- why a design was chosen
- what behavior must keep holding after a refactor
- what a new human or model needs to understand the codebase without reading everything

Traditional languages usually push that into comments, external docs, stale tests, or team memory. Aver makes those concerns part of the language and tooling.

The intended workflow is explicit: AI writes Aver, humans review contracts and intent, and execution happens through the bytecode VM during development, with deployment through Rust code generation or WASM compilation.

---

## Execution modes

### Default (VM)

```bash
aver run hello.av
```

All commands use the bytecode VM. For VM internals, see [docs/vm.md](docs/vm.md).

### WASM

```bash
# Native WebAssembly GC + tail-call output for browsers / Workers
aver compile hello.av --target wasm-gc

# Run through embedded wasmtime (uses the same wasm-gc emit path)
aver run hello.av --wasm-gc

# WASI 0.2 / Component Model output (wasmtime, Spin, NGINX Unit, etc.)
aver compile hello.av --target wasip2

# Run a wasip2 component through embedded wasmtime + wasi
aver run hello.av --wasip2 -- some-arg
```

`--target wasm-gc` is the default modern target (Chrome 119+ /
Firefox 120+ / Safari 18.2+ / wasmtime 25+ / Node 22+ / Workers).
`--target wasip2` is its peer for the Component Model side. The
pre-2024 NaN-boxed wasm32 backend (`--target wasm` + `--bridge`)
was dropped in 0.18 — see [`src/codegen/wasm_gc/README.md`](src/codegen/wasm_gc/README.md)
and [`docs/wasip2.md`](docs/wasip2.md) for the design notes.

### Native Rust

```bash
aver compile hello.av              # generates Rust/Cargo project
aver compile hello.av --policy embed   # bake aver.toml into binary
aver compile hello.av --policy runtime # load aver.toml at runtime
```

See [docs/rust.md](docs/rust.md) for the generated code model.

### Self-hosted

```bash
aver run hello.av --self-host
```

Runs through the Aver interpreter written in Aver itself, compiled to Rust and cached on demand. See [self_hosted/README.md](self_hosted/README.md).

---

## What Aver makes explicit

### Effects

```aver
fn processPayment(amount: Int) -> Result<String, String>
    ? "Validates and records the charge. Pure — no network, no disk."
    match amount
        0 -> Result.Err("Cannot charge zero")
        _ -> Result.Ok("txn-{amount}")
```

```aver
fn fetchExchangeRate(currency: String) -> Result<HttpResponse, String>
    ? "Fetches live rate from the ECB feed."
    ! [Http.get]
    Http.get("https://api.ecb.europa.eu/rates/{currency}")
```

Effects such as `Http.get`, `Disk.readText`, and `Console.print` are part of the signature. Missing declarations are type errors. The runtime enforces the same boundary as a backstop.

Effects can be granular or namespace-wide:

- `! [Http.get]` allows only `Http.get`
- `! [Http]` covers all `Http.*` effects

`aver check` suggests narrowing a broad namespace effect when a function only needs a smaller subset.

Runtime policy can narrow the allowed destinations further via `aver.toml`:

```toml
[effects.Http]
hosts = ["api.example.com", "*.internal.corp"]

[effects.Disk]
paths = ["./data/**"]

[effects.Env]
keys = ["APP_*", "PUBLIC_*"]
```

Think of this as two separate controls:

- code answers: what kind of I/O is allowed?
- policy answers: which concrete destinations are allowed?

Generated Rust can use the same scoped runtime machinery when you compile with `--with-replay`; see [docs/rust.md](docs/rust.md).

### Decisions

```aver
decision UseResultNotExceptions
    date = "2024-01-15"
    reason =
        "Invisible exceptions lose money at runtime."
        "Callers must handle failure — Result forces that at the call site."
    chosen = "Result"
    rejected = ["Exceptions", "Nullable"]
    impacts = [charge, refund, settle]
    author = "team"
```

`decision` blocks are first-class syntax, colocated with the code they explain.

Query only the decision history for a module graph:

```bash
aver context decisions/architecture.av --decisions-only
```

`impacts`, `chosen`, and `rejected` accept either validated symbols or quoted semantic labels.

### Context export

```bash
aver context examples/core/calculator.av
```

Aver walks the dependency graph and emits a compact context summary: module intent, public signatures, effect declarations, verify samples, and decisions. The goal is not to dump the whole source tree; it is to export the contract-level view that another human or LLM needs first.

By default, `aver context` uses `--depth auto --budget 10kb` with priority scoring: elements with more verify coverage, spec references, and decisions are included first. `--depth N` and `--depth unlimited` bypass that budget. Long verify examples are skipped rather than bloating the artifact.

Use `--focus <symbol>` to build context around a specific function — its callees, types, verify blocks, and decisions are prioritized within the budget:

```bash
aver context examples/data/json.av --focus fromString
```

This makes token budget a navigation primitive. Another human or model can start with a small architecture map, zoom into the modules that matter, or focus on a single function's dependency cone.

If you want a larger export for a medium project, raise the budget explicitly:

```bash
aver context projects/workflow_engine/main.av \
  --module-root projects/workflow_engine \
  --json \
  --budget 24kb \
  --output projects/workflow_engine/CONTEXT.json
```

When `--output` is used, Aver also prints a short selection summary to stdout, for example:

```text
mode auto, included depth 2, used 22622b, budget 24kb, truncated, next depth 3 would use 40739b
```

The same selection metadata is embedded in JSON output so you can see whether the export stopped because of the budget.

Example shape:

```markdown
## Module: Calculator
> Safe calculator demonstrating Result types, match expressions, and co-located verification.

### `safeDivide(a: Int, b: Int) -> Result<Int, String>`
> Safe integer division. Returns Err when divisor is zero.
verify: `safeDivide(10, 2)` → `Result.Ok(5)`

## Decisions
### NoExceptions (2024-01-15)
**Chosen:** Result — **Rejected:** Exceptions, Nullable
```

### Verify

```aver
verify charge
    charge("alice", 100) => Result.Ok("txn-alice-100")
    charge("bob",   0)   => Result.Err("Cannot charge zero")
    charge("x",    -1)   => Result.Ok("txn-x--1")
```

`verify` blocks stay next to the function they cover. `aver check` treats a missing `verify` block on a pure, non-trivial, non-`main` function as a contract error.

Regular verify:

```aver
verify add
    add(1, 2) => 3
    add(0, 0) => 0
```

Law verify:

```aver
verify add law commutative
    given a: Int = -2..2
    given b: Int = [-1, 0, 1]
    add(a, b) => add(b, a)
```

`verify ... law ...` is deterministic, not random sampling. Cases are generated as the cartesian product of explicit domains, capped at `10_000`.

For the proof-oriented style where a law relates an implementation to a pure spec function, see [docs/language.md](docs/language.md) and [docs/lean.md](docs/lean.md).

Oracle laws for classified effects:

```aver
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int
    ? "Deterministic Random.int stub."
    4

fn pickOne() -> Int
    ? "Rolls once."
    ! [Random.int]
    Random.int(1, 6)

verify pickOne law usesOracle
    given rnd: Random.int = [fairDie]
    pickOne() => rnd(BranchPath.Root, 0, 1, 6)
```

`verify <fn> law <name>` is the proof-oriented Oracle form: `aver proof` lifts the effectful function to a pure function with explicit oracle parameters and can emit a universal theorem over that oracle. Use cases-form `verify <fn> trace` when you want runtime assertions over `.result` and `.trace.*`, such as `trace.contains(Random.int(1, 6))`. See [docs/oracle.md](docs/oracle.md) for the full model.

### Replay

Use deterministic replay for stateful, interactive, or external effectful code:

1. run once against real services and record the effect trace
2. replay offline with no real network, disk, or TCP calls
3. use `--diff` and `--test` to turn recordings into a regression suite

```bash
aver run    examples/services/console_demo.av --record recordings/
aver replay recordings/rec-123.json --diff
aver replay recordings/ --test --diff

# Same flow under wasm-gc — recordings are interchangeable across the
# VM, self-host, and wasm-gc backends.
aver run    examples/services/console_demo.av --wasm-gc --record recordings/
aver replay recordings/rec-123.json --wasm-gc
```

Use Oracle laws when classified effects should become proof obligations over explicit stubs. Use `verify <fn> trace` when you need runtime assertions over `.result` or `.trace.*`. Use replay when the flow depends on ambient state, persistent TCP sessions, terminal modes, or server callbacks.

---

## Common commands

```
aver check   file.av
aver run     file.av
aver verify  file.av
aver context file.av
aver compile file.av -o out/
```

Full per-command reference, including flags, replay, formatting, REPL, and the audit / why / proof / bench surface: [docs/cli.md](docs/cli.md). Both `check` and `verify` accept `--deps` to walk transitive `depends [...]` modules; `aver verify --wasm-gc` runs the same cases through the wasm-gc backend as a cross-target check.

---

## Language and runtime

Aver is intentionally small. The core model is:

- immutable bindings only
- `match` instead of `if`/`else`
- `Result` and `Option` instead of exceptions and `null`
- top-level functions only, with no closures
- explicit method-level effects
- module-based structure via `module`, `depends`, and `exposes`
- automatic memoization and tail-call optimization for eligible code

For the surface-language guide, see [docs/language.md](docs/language.md).

For constructor rules and edge cases, see [docs/constructors.md](docs/constructors.md).

For namespaces, effectful services, and the standard library, see [docs/services.md](docs/services.md).

## Execution and proof backends

Aver has four backend paths:

- VM-based workflow for `run`, `check`, `verify`, `replay`, and `context`
- Rust compilation for generating a native Cargo project with `aver compile`
- Lean proof export for pure core logic, Oracle-lifted classified effectful laws, and `verify` / `verify law` obligations with `aver proof`
  Supported law shapes become real universal theorems; the rest stay as
  executable samples or checked-domain theorems instead of fake proofs.
- Dafny verification for automated `verify law` checking via Z3 with `aver proof --backend dafny`

The VM and generated Rust share practical behavior through `aver-rt`: list teardown, deep `append -> match` paths, and string helpers such as `String.slice` are intentionally centralized there so one runtime fix can improve both execution paths.

Typical Rust flow:

```bash
aver compile examples/core/calculator.av -o out/
cd out
cargo run
```

Typical Lean flow:

```bash
aver proof examples/formal/law_auto.av --verify-mode auto -o out/
cd out
lake build
```

Typical Dafny flow:

```bash
aver proof examples/data/fibonacci.av --backend dafny -o out/
cd out
dafny verify fibonacci.dfy
```

Rust is the deployment backend. Lean and Dafny are complementary proof backends:

- **Lean** handles `verify` cases via `native_decide` (100% success on concrete examples) and supported `verify law` shapes via hand-crafted tactic strategies
- **Dafny** emits `verify law` blocks as lemmas and lets Z3 attempt automated proofs — no tactic authoring needed, but limited on concrete computation

For backend-specific details, see:
- [docs/rust.md](docs/rust.md) for Cargo generation and deployment flow
- [docs/lean.md](docs/lean.md) for proof export, formal-verification path, and current Lean examples
- [docs/dafny.md](docs/dafny.md) for Dafny verification and Z3-powered law checking
- [docs/oracle.md](docs/oracle.md) for Oracle laws, classified-effect stubs, and trace assertions

---

## Examples

Shared examples under `examples/` resolve from `--module-root examples`.
They are grouped by role:
- `core/` for language and syntax tours
- `data/` for pure data structures and parsers
- `formal/` for Lean-oriented proof examples
- `modules/` for import and module-root examples
- `services/` for effectful adapter demos
- `apps/` for small multi-file applications under the shared examples root
- `games/` for interactive terminal games (Snake, Tetris, Braille Doom)
Standalone multi-file showcase projects live under `projects/` and use their own local module roots.

Repository layout rule:
- files under `examples/` share one root: `--module-root examples`
- each folder under `projects/` is its own root, for example `--module-root projects/workflow_engine`

Typical commands:

```bash
aver check examples/modules/app.av --module-root examples --deps
aver check projects/workflow_engine/main.av --module-root projects/workflow_engine --deps
aver check projects/payment_ops/main.av --module-root projects/payment_ops --deps
```

Curated shared examples:

| File | Demonstrates |
|------|-------------|
| `core/hello.av` | Functions, string interpolation, verify |
| `core/calculator.av` | Result types, match, decision blocks |
| `core/shapes.av` | Sum types, qualified constructors (`Shape.Circle`), match on variants |
| `data/fibonacci.av` | Tail recursion, records, decision blocks |
| `formal/law_auto.av` | Lean proof export, `verify law`, conservative universal auto-proofs plus sampled/domain fallback |
| `formal/spec_laws.av` | Implementation-vs-spec laws (`verify foo law fooSpec`) and Lean spec theorems for supported shapes |
| `formal/oracle_trace.av` | Oracle laws and trace assertions for classified effects with explicit stubs |
| `apps/mission_control.av` | Command parser, pure state machine, effectful shell |
| `games/wumpus.av` | Hunt the Wumpus: cave exploration, match-driven control flow |
| `modules/app.av` | Module imports via `depends [Data.Fibonacci]` |
| `services/console_demo.av` | Console service and replay-friendly effectful flow |
| `services/http_demo.av` | HTTP service with sub-effects: `Http.get`, `Http.post` |
| `services/weather.av` | End-to-end service: `HttpServer` + `Http` + `Tcp` |
| `apps/notepad/app.av` | Multi-file HTTP app under the shared `examples` module root |
| `games/snake.av` | Terminal Snake: immutable state, TCO game loop, Terminal service |
| `games/tetris/main.av` | Modular Tetris: sum types, 2D grid, collision, line clearing (4 modules, 66 verify cases) |
| `games/checkers/main.av` | Checkers with alpha-beta AI: cursor UI, forced capture, decision trace (5 modules, 144 verify cases) |
| `games/doom/main.av` | Braille Doom: raycasting FPS with Unicode Braille rendering, procedural levels, 3 enemy types with AI, shooting, wall textures (7 modules) |
| `core/test_errors.av` | Intentional `aver check` failures: type errors + verify/decision/effect diagnostics |

Standalone projects:

| File | Demonstrates |
|------|-------------|
| `projects/workflow_engine/main.av` | Explicit app/domain/infra flow, event replay, derived events, verify-driven orchestration |
| `projects/payment_ops/main.av` | Dirty payment backoffice flow: provider normalization, replay, settlement reconcile, manual-review cases, audit trail |
| `self_hosted/main.av` | Full self-hosted interpreter: lexer, parser, resolver, evaluator — all 55 examples pass. Compiles to native via `aver compile` and powers `aver run --self-host`. |

See `examples/` and `projects/` for the full set.
For repository self-documentation via decision exports, see `decisions/architecture.av`.

---

## Documentation

| Document | Contents |
|----------|----------|
| [docs/language.md](docs/language.md) | Surface-language guide: syntax, semantics, modules, and deliberate omissions |
| [docs/formatting.md](docs/formatting.md) | Formatter behavior and guarantees |
| [docs/constructors.md](docs/constructors.md) | Constructor rules and parsing contract |
| [editors/README.md](editors/README.md) | VS Code + LSP setup and Sublime Text support |
| [docs/services.md](docs/services.md) | Full API reference for all namespaces (signatures, effects, notes) |
| [docs/oracle.md](docs/oracle.md) | Oracle: effectful laws, trace assertions, and proof export for classified effects |
| [docs/vm.md](docs/vm.md) | Bytecode VM design note: execution model, `NanValue`, opcodes, effects |
| [docs/types.md](docs/types.md) | Key data types (compiler, AST, runtime) |
| [docs/extending.md](docs/extending.md) | How to add keywords, namespace functions, expression types |
| [docs/transpilation.md](docs/transpilation.md) | Overview of `aver compile` and `aver proof` |
| [docs/rust.md](docs/rust.md) | Rust backend: deployment-oriented Cargo generation |
| [docs/wasm.md](docs/wasm.md) | WASM backend: ABI, memory model, browser hosting |
| [docs/lean.md](docs/lean.md) | Lean backend: proof export and formal-verification path |
| [docs/dafny.md](docs/dafny.md) | Dafny backend: Z3-powered automated law verification |
| [docs/independence.md](docs/independence.md) | Independent products: the semantic model behind `?!` and `!` |
| [docs/research.md](docs/research.md) | Narrow related work for effects, Oracle, independent products, and proof targets |
| [docs/pushback.md](docs/pushback.md) | Common pushback: questions, objections, and honest answers |
| [docs/decisions.md](docs/decisions.md) | Decision export generated via `aver context --decisions-only` |
