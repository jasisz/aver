# Aver

Aver is a statically typed language designed for AI to write in and humans to review, with a fast interpreter for iteration and a Rust transpiler for deployment.

It is built around one idea: the risky part of AI-written code is usually not syntax, it is missing intent. Aver makes that intent explicit and machine-readable:

- effects are part of the function signature
- decisions live next to the code they explain
- pure behavior lives in colocated `verify` blocks
- effectful behavior can be recorded and replayed deterministically
- `aver context` exports the contract-level view of a module graph for humans or LLMs
- `aver compile -t rust` turns an Aver module graph into a Rust/Cargo project

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
    = "Hello, {name}"

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
aver compile  hello.av -t rust -o out/
(cd out && cargo run)
```

### Build from source

```bash
git clone https://github.com/jasisz/aver
cd aver
cargo build --release

cargo run -- run      examples/calculator.av
cargo run -- verify   examples/calculator.av
cargo run -- check    examples/calculator.av
cargo run -- context  examples/calculator.av
cargo run -- compile  examples/calculator.av -t rust -o out/
(cd out && cargo run)
cargo run -- run      examples/services/console_demo.av --record recordings/
cargo run -- replay   recordings/ --test --diff
```

Requires: Rust stable toolchain.

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

Effects are hierarchical:

- `! [Http.get]` allows only `Http.get`
- `! [Http]` allows all `Http.*` methods

`aver check` also enforces minimal effects: prefer method-level declarations over broad namespace-level ones when possible.

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
aver context examples/calculator.av
```

Aver walks the dependency graph and emits a compact context summary: module intent, public signatures, effect declarations, verify samples, and decisions. The goal is not to dump the whole source tree; it is to export the contract-level view that another human or LLM needs first.

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
aver compile file.av -t rust -o out/
```

For replay, formatting, REPL, and the full command surface, use `aver --help` and the docs below.

---

## Language and runtime

Aver is intentionally small. The core model is:

- immutable bindings only
- `match` instead of `if`/`else`
- `Result` and `Option` instead of exceptions and `null`
- top-level functions only, with no closures
- explicit effects and named effect aliases
- module-based structure via `module`, `depends`, and `exposes`
- automatic memoization and tail-call optimization for eligible code

For the surface-language guide, see [docs/language.md](docs/language.md).

For constructor rules and edge cases, see [docs/constructors.md](docs/constructors.md).

For namespaces, effectful services, and the standard library, see [docs/services.md](docs/services.md).

## Interpreter and transpiler

Aver has three execution paths:

- interpreter-first workflow for `run`, `check`, `verify`, `replay`, and `context`
- Rust transpilation for generating a native Cargo project with `aver compile -t rust`
- optional Lean backend for proof-oriented export of pure core logic

Typical Rust transpilation flow:

```bash
aver compile examples/calculator.av -t rust -o out/
cd out
cargo run
```

---

## Examples

Curated examples:

| File | Demonstrates |
|------|-------------|
| `hello.av` | Functions, string interpolation, verify |
| `calculator.av` | Result types, match, decision blocks |
| `shapes.av` | Sum types, qualified constructors (`Shape.Circle`), match on variants |
| `fibonacci.av` | Tail recursion, records, decision blocks |
| `mission_control.av` | Command parser, pure state machine, effectful shell |
| `app.av` | Module imports via `depends [Examples.Fibonacci]` |
| `services/console_demo.av` | Console service and replay-friendly effectful flow |
| `services/http_demo.av` | HTTP service with sub-effects: `Http.get`, `Http.post` |
| `services/weather.av` | End-to-end service: `HttpServer` + `Http` + `Tcp` |
| `decisions/architecture.av` | The interpreter documents itself in Aver |
| `test_errors.av` | Intentional `aver check` failures: type errors + verify/decision/effect diagnostics |

See `examples/` for the full set.

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
| [docs/transpilation.md](docs/transpilation.md) | Transpilation (`aver compile`): targets, flags, supported features |
| [docs/decisions.md](docs/decisions.md) | Decision export generated via `aver context --decisions-only` |
