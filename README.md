# Aver

Aver is a statically typed language designed for AI to write in and humans to review, with a fast interpreter for iteration, a Rust backend for deployment, Lean proof export for pure core logic, and Dafny verification for automated law checking via Z3.

It is built around one idea: the risky part of AI-written code is usually not syntax, it is missing intent. Aver makes that intent explicit and machine-readable:

- effects are part of the function signature
- decisions live next to the code they explain
- pure behavior lives in colocated `verify` blocks
- effectful behavior can be recorded and replayed deterministically
- `aver context` exports the contract-level view of a module graph for humans or LLMs
- `aver compile` turns an Aver module graph into a Rust/Cargo project
- `aver proof` exports the pure subset of an Aver module graph to a Lean 4 proof project (default) or Dafny verification file (`--backend dafny`)

This is not a language optimized for humans to type by hand all day. It is optimized for AI to generate code that humans can inspect, constrain, test, and ship.

Read the [Aver Manifesto](https://jasisz.github.io/aver-language/) for the longer argument.

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
aver verify   hello.av
aver check    hello.av
aver context  hello.av
aver compile  hello.av -o out/
(cd out && cargo run)
```

`Unit` is Aver's "no meaningful value" type, roughly like `void` and rendered as `()` in diagnostics. `main` often returns `Unit`, but it can also return `Result<Unit, String>`; `aver run` treats `Result.Err(...)` from `main` as a process failure.

### Build from source

```bash
git clone https://github.com/jasisz/aver
cd aver
cargo install --path . --force

aver run      examples/core/calculator.av
aver verify   examples/core/calculator.av
aver check    examples/core/calculator.av
aver context  examples/core/calculator.av
aver compile  examples/core/calculator.av -o out/
(cd out && cargo run)
aver proof    examples/formal/law_auto.av -o proof/
(cd proof && lake build)
aver run      examples/services/console_demo.av --record recordings/
aver replay   recordings/ --test --diff
```

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

- no `if`/`else` - branching goes through `match`
- no `for`/`while` - iteration is recursion or explicit list operations
- no exceptions - failure is `Result`
- no `null` - absence is `Option`
- no closures - functions are top-level and explicit

The point is to remove classes of implicit behavior that are easy for AI to generate and annoying for humans to audit.

For the fuller language rationale, see [docs/language.md](docs/language.md).

---

## Why Aver exists

LLMs can produce function bodies quickly. They are much worse at preserving the information reviewers actually need:

- what a function is allowed to do
- why a design was chosen
- what behavior must keep holding after a refactor
- what a new human or model needs to understand the codebase without reading everything

Traditional languages usually push that into comments, external docs, stale tests, or team memory. Aver makes those concerns part of the language and tooling.

The intended workflow is explicit: AI writes Aver, humans review contracts and intent, and deployment happens either through the interpreter or by transpiling to Rust.

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

Effects are exact:

- `! [Http.get]` allows only `Http.get`
- `! [Http]` does not cover `Http.get`

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

By default, `aver context` uses `--depth auto --budget 10kb`: it tries depth `0`, `1`, `2`, ... and keeps the deepest result that still fits the byte budget. `--depth N` and `--depth unlimited` bypass that budget. Long verify examples are skipped rather than bloating the artifact.

This makes token budget a navigation primitive. Another human or model can start with a small architecture map, then zoom into the modules that matter instead of reading the whole tree up front.

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

`verify` blocks stay next to the function they cover. `aver check` treats a missing `verify` block on a pure, non-trivial, non-`main` function as a contract error. Effectful flows are intentionally handled separately via replay.

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

### Replay

Use deterministic replay for effectful code:

1. run once against real services and record the effect trace
2. replay offline with no real network, disk, or TCP calls
3. use `--diff` and `--test` to turn recordings into a regression suite

```bash
aver run    examples/services/console_demo.av --record recordings/
aver replay recordings/rec-123.json --diff
aver replay recordings/ --test --diff
```

Pure logic belongs in `verify`. Effectful flows belong in replay recordings.

---

## Common commands

```
aver check   file.av
aver run     file.av
aver verify  file.av
aver context file.av
aver compile file.av -o out/
```

`aver verify` runs only the example cases from `verify` blocks and fails on mismatches or example errors. `aver check` handles static contract diagnostics such as missing `verify` blocks and coverage-style warnings. Both `check` and `verify` accept `--deps` to walk transitive `depends [...]` modules.

For recursive code, `aver check` also warns when a recursive function still contains non-tail recursive calls after TCO. Tail-recursive code remains the preferred shape for large linear traversals; the warning is there to point out where accumulator-style rewrites may matter.

`aver context` defaults to `--depth auto --budget 10kb`. Use `--budget 24kb`, `--depth 2`, or `--depth unlimited` when you want a deeper export.

For replay, formatting, REPL, and the full command surface, use `aver --help` and the docs below.

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

- interpreter-first workflow for `run`, `check`, `verify`, `replay`, and `context`
- Rust compilation for generating a native Cargo project with `aver compile`
- Lean proof export for pure core logic and `verify` / `verify law` obligations with `aver proof`
  Supported law shapes become real universal theorems; the rest stay as
  executable samples or checked-domain theorems instead of fake proofs.
- Dafny verification for automated `verify law` checking via Z3 with `aver proof --backend dafny`

The interpreter and generated Rust now share more practical behavior through `aver-rt` than the name alone suggests: list teardown, deep `append -> match` paths, and string helpers such as `String.slice` are intentionally centralized there so one runtime fix can improve both execution paths.

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
- **Dafny** emits only `verify law` blocks as lemmas and lets Z3 attempt automated proofs — no tactic authoring needed, but limited on concrete computation

For backend-specific details, see:
- [docs/rust.md](docs/rust.md) for Cargo generation and deployment flow
- [docs/lean.md](docs/lean.md) for proof export, formal-verification path, and current Lean examples
- [docs/dafny.md](docs/dafny.md) for Dafny verification and Z3-powered law checking

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
| `apps/mission_control.av` | Command parser, pure state machine, effectful shell |
| `modules/app.av` | Module imports via `depends [Data.Fibonacci]` |
| `services/console_demo.av` | Console service and replay-friendly effectful flow |
| `services/http_demo.av` | HTTP service with sub-effects: `Http.get`, `Http.post` |
| `services/weather.av` | End-to-end service: `HttpServer` + `Http` + `Tcp` |
| `apps/notepad/app.av` | Multi-file HTTP app under the shared `examples` module root |
| `core/test_errors.av` | Intentional `aver check` failures: type errors + verify/decision/effect diagnostics |

Standalone projects:

| File | Demonstrates |
|------|-------------|
| `projects/workflow_engine/main.av` | Explicit app/domain/infra flow, event replay, derived events, verify-driven orchestration |
| `projects/payment_ops/main.av` | Dirty payment backoffice flow: provider normalization, replay, settlement reconcile, manual-review cases, audit trail |

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
| [docs/types.md](docs/types.md) | Key data types (compiler, AST, runtime) |
| [docs/extending.md](docs/extending.md) | How to add keywords, namespace functions, expression types |
| [docs/transpilation.md](docs/transpilation.md) | Overview of `aver compile` and `aver proof` |
| [docs/rust.md](docs/rust.md) | Rust backend: deployment-oriented Cargo generation |
| [docs/lean.md](docs/lean.md) | Lean backend: proof export and formal-verification path |
| [docs/dafny.md](docs/dafny.md) | Dafny backend: Z3-powered automated law verification |
| [docs/decisions.md](docs/decisions.md) | Decision export generated via `aver context --decisions-only` |
