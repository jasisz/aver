# Aver

> *Imagine AI writes 90% of your code. What do you need to trust it?*

Aver is hard to write, clean to read — and in a world where AI does the writing, that's the only trade-off that matters.

Not better prompts. Not more reviews. You need a language that makes the AI's intent **verifiable** — statically, at the call site, before a single line runs.

Aver is that language.

---

## The problem with AI-generated code

LLMs are good at writing functions. They're terrible at communicating:

- what a function is *allowed* to do (call the network? write files? read secrets?)
- *why* a particular approach was chosen over the alternatives
- whether a change *still passes* the invariants the original author had in mind
- what an AI needs to know to continue working on a 50-file codebase it's never seen

Traditional languages leave all of this as implicit knowledge in someone's head, in stale docs, or nowhere at all. That was fine when humans wrote all the code. It isn't now.

---

## Aver's answer, feature by feature

### "How do I know this function doesn't make HTTP calls?"

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
    ! [Http]
    Http.get("https://api.ecb.europa.eu/rates/{currency}")
```

Effect declarations (`! [Http]`, `! [Disk]`, `! [Console]`, `! [Tcp]`, `! [HttpServer]`) are part of the signature. The type checker enforces them — a function that calls `Http.get` without declaring `! [Http]` is a **type error, not a warning**. The runtime enforces the same boundary as a backstop.

You can read any function in Aver and know exactly what it's capable of — without running it, without reading its body.

### "Why was this decision made?"

```aver
decision UseResultNotExceptions
    date = "2024-01-15"
    reason =
        "Invisible exceptions lose money at runtime."
        "Callers must handle failure — Result forces that at the call site."
    chosen = Result
    rejected = [Exceptions, Nullable]
    impacts = [charge, refund, settle]
    author = "team"
```

`decision` blocks are first-class syntax, co-located with the code they describe. Not a Confluence page that goes stale. Not a commit message no one reads. Queryable:

```bash
aver context payments.av --decisions-only
```

Three months later — human or AI — you know *why* the code looks the way it does.

### "How does an AI understand this codebase without reading everything?"

```bash
aver context payments.av
```

Aver traverses the dependency graph and emits a compact summary — module intents, public signatures, effect declarations, verify cases, and all decision blocks — typically 1–3k tokens. Enough for an LLM to understand contracts, boundaries, and rationale without the full source.

```markdown
## Module: Payments
> Processes transactions with an explicit audit trail.

### `charge(account: String, amount: Int) -> Result<String, String>` ! [Http, Ledger]
> Charges account. Returns txn ID or a human-readable error.
verify: `charge("alice", 100)` → `Result.Ok("txn-alice-100")`, `charge("bob", 0)` → `Result.Err("Cannot charge zero")`

## Decisions
### UseResultNotExceptions (2024-01-15)
**Chosen:** Result — **Rejected:** Exceptions, Nullable
> Invisible exceptions lose money at runtime...
```

This is what AI-assisted development actually needs: context that travels with the code, not context that lives in someone's memory.

### "How do I know a refactor didn't break anything?"

```aver
verify charge
    charge("alice", 100) => Result.Ok("txn-alice-100")
    charge("bob",   0)   => Result.Err("Cannot charge zero")
    charge("x",    -1)   => Result.Ok("txn-x--1")
```

`verify` blocks are intended to stay close to the function they cover (same module, usually right after the definition). `aver check` warns when a non-`main` function has no `verify` block by name. They run with `aver verify` (or `aver run --verify`) under the same type/effect checks as normal code.

### "How do I test effectful code without flaky mocks?"

Use deterministic replay.

1. Run once against real services and record the effect trace.
2. Replay offline: same effect sequence, same outcomes, no real network/disk/TCP calls.
3. Use `--diff`/`--test` to turn recordings into a regression suite.

```bash
# Real execution + capture
aver run payments.av --record recordings/

# Deterministic replay (single file or whole directory)
aver replay recordings/rec-123.json --diff
aver replay recordings/ --test --diff
```

No mock framework. No mock code. No mock maintenance.

In Aver, effectful tests are just replay files:

1. capture once,
2. replay forever,
3. optionally tweak one recorded effect outcome to create a new edge-case test.

Pure logic stays in `verify`; effectful flows are covered by replay recordings. Testing stays embarrassingly simple.

---

## Full example

```aver
module Payments
    intent =
        "Processes transactions with an explicit audit trail."
    depends [Ledger, Models.User]
    exposes [charge]

decision UseResultNotExceptions
    date = "2024-01-15"
    reason =
        "Invisible exceptions lose money at runtime."
        "Callers must handle failure — Result forces that at compile time."
    chosen = Result
    rejected = [Exceptions, Nullable]
    impacts = [charge]

fn charge(account: String, amount: Int) -> Result<String, String>
    ? "Charges account. Returns txn ID or a human-readable error."
    ! [Http, Ledger]
    match amount
        0 -> Result.Err("Cannot charge zero")
        _ -> Result.Ok("txn-{account}-{amount}")

verify charge
    charge("alice", 100) => Result.Ok("txn-alice-100")
    charge("bob",   0)   => Result.Err("Cannot charge zero")
```

No `if`/`else`. No loops. No exceptions. No nulls. No magic.

---

## CLI

```
aver run       file.av                   # type-check, then execute
aver run       file.av --verify          # execute + run verify blocks
aver run       file.av --record recs/    # execute + record effect trace
aver replay    recs/rec-123.json         # replay one recording offline
aver replay    recs/ --test --diff       # replay suite; fail on output mismatch
aver check     file.av                   # type errors + intent/desc warnings
aver verify    file.av                   # run all verify blocks
aver context   file.av                   # export project context (Markdown)
aver context   file.av --json            # export project context (JSON)
aver context   file.av --decisions-only        # decision blocks only (Markdown)
aver context   file.av --decisions-only --json # decision blocks only (JSON)
aver decisions                          # ADR-style decisions from decisions/architecture.av
aver decisions --docs                   # regenerate docs/decisions.md from decision blocks
aver repl                              # interactive REPL
```

`run`, `check`, `verify`, and `context` also accept `--module-root <path>` to override import base (default: current working directory).

---

## Language reference

### Types

Primitive: `Int`, `Float`, `String`, `Bool`, `Unit`
Compound: `Result<T, E>`, `Option<T>`, `List<T>`, `Map<K, V>`, `(A, B, ...)`, `Fn(A) -> B`, `Fn(A) -> B ! [Effect]`
User-defined sum types: `type Shape` → `Shape.Circle(Float)`, `Shape.Rect(Float, Float)`
User-defined product types: `record User` → `User(name = "Alice", age = 30)`, `u.name`

### Bindings

All bindings are immutable. No `val`/`var` keywords — they are parse errors.

```aver
name = "Alice"
age: Int = 30
xs: List<Int> = []
```

Optional type annotation provides a hint to the type checker; the annotation wins over inference when both are compatible. Binding to an empty list literal without a type annotation (`x = []`) is a type error.

Duplicate binding of the same name in the same scope is a type error.

### Operators

Arithmetic: `+`, `-`, `*`, `/` with mixed Int/Float promotion.
Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`.
Pipe: `value |> fn` — passes the left-hand value as the sole argument to the right-hand function.
Error propagation: `expr?` — unwraps `Result.Ok`, propagates `Result.Err` as a `RuntimeError`.

### String interpolation

Expressions inside `{}` are evaluated at runtime:

```aver
greeting = "Hello, {name}! You are {age} years old."
```

### Constructors

UpperCamel callee = constructor, lowerCamel = function call. Records use named args (`User(name = "A", age = 1)`), variants use positional args (`Shape.Circle(3.14)`), zero-arg constructors are bare singletons (`Option.None`, `Shape.Point`).

All constructors are namespaced — no bare `Ok`/`Err`/`Some`/`None`:

```aver
Result.Ok(42)
Result.Err("not found")
Option.Some("hello")
Option.None
```

### Match expressions

`match` is the only branching construct (no `if`/`else`). Patterns:

```aver
match value
    42 -> "exact"                          // literal
    _ -> "anything"                        // wildcard
    x -> "bound to {x}"                    // identifier binding
    [] -> "empty list"                     // empty list
    [h, ..t] -> "head {h}, tail {t}"       // list cons
    Result.Ok(v) -> "success: {v}"         // constructor
    Result.Err(e) -> "error: {e}"
    Shape.Circle(r) -> "circle r={r}"
    Shape.Point -> "point"
    User(name, age) -> "user {name}"       // record positional destructuring
    (a, b) -> "pair: {a}, {b}"             // tuple destructuring
    ((x, y), z) -> "nested: {x}"          // nested tuple
```

Nested match in match arms is supported. Arm body must follow `->` on the same line — extract complex expressions into a named function.

### Record update

Creates a new record with overridden fields, preserving all other fields:

```aver
updated = User.update(u, age = 31)
```

### Map literals

```aver
m = {"key" => value, "other" => 42}
```

`=>` is required inside map literals; `:` stays type-only.

### Effect aliases

Named effect sets reduce repetition:

```aver
effects AppIO = [Console, Disk]

fn main() -> Unit
    ! [AppIO]
    // ...
```

### Functions

```aver
fn add(a: Int, b: Int) -> Int = a + b

fn charge(account: String, amount: Int) -> Result<String, String>
    ? "Charges account. Returns txn ID or error."
    ! [Http]
    match amount
        0 -> Result.Err("Cannot charge zero")
        _ -> Result.Ok("txn-{account}-{amount}")
```

- `? "..."` — optional prose description (part of the signature)
- `! [Effect]` — optional effect declaration (statically and runtime enforced)
- `= expr` — single-expression shorthand
- Block body with indentation for multi-statement functions

### No closures

All user-defined functions are top-level. At call time, a function sees globals + its own parameters — no closure capture at definition time.
There is no lambda syntax. Higher-order APIs (for example `List.map`, `List.filter`, `List.any`) take a top-level function name.

### Auto-memoization

Pure recursive functions with memo-safe arguments (scalars, records/variants of scalars) are automatically memoized at runtime. No keyword needed — the compiler detects eligibility via call-graph analysis (Tarjan SCC). Cache is capped at 4096 entries per function.

### Tail-call optimization

Self and mutual tail recursion is optimized automatically. A transform pass after parsing rewrites tail-position calls into a trampoline — no stack growth for recursive functions. Tail position = last expression in function body, or each arm body in a `match` at tail position.

### Modules

Module imports resolve from a module root (`--module-root`, default: current working directory).

```aver
module Payments
    intent = "Processes transactions."
    depends [Examples.Fibonacci]
    exposes [charge]
```

`depends [Examples.Fibonacci]` → `examples/fibonacci.av`, call as `Examples.Fibonacci.fn(...)`.
`depends [Examples.Models.User]` → `examples/models/user.av`, call as `Examples.Models.User.fn(...)`.

### Static type checking

Type errors block `run`, `check`, and `verify`. No partial execution. The checker covers function bodies, top-level statements, effect propagation, and duplicate binding detection.

---

## What Aver deliberately omits

| Absent | Reason |
|--------|--------|
| `if`/`else` | `match` is exhaustive — no silent missing cases |
| `for`/`while` | Use `map`, `filter`, `fold` — iteration is data transformation |
| `null` | `Option<T>` with `Some`/`None` only |
| Exceptions | `Result<T, E>` only — errors are values |
| Global mutable state | No shared mutable state by design |
| Closures | All functions are top-level — no captured variables, explicit is better than implicit |
| Magic | No decorators, no implicit behaviour, no runtime reflection |

---

## Built-in services

Aver ships built-in namespaces for I/O. All require explicit effect declarations — the typechecker and runtime both enforce this.

| Namespace | Effect | Key functions |
|-----------|--------|---------------|
| `Console` | `! [Console]` | `print`, `error`, `warn`, `readLine` |
| `Http` | `! [Http]` | `get`, `post`, `put`, `patch`, `head`, `delete` |
| `HttpServer` | `! [HttpServer]` | `listen`, `listenWith` |
| `Disk` | `! [Disk]` | `readText`, `writeText`, `exists`, `delete`, `listDir`, `makeDir` |
| `Tcp` | `! [Tcp]` | `connect`, `writeLine`, `readLine`, `close`, `send`, `ping` |

Pure namespaces (no effects):

| Namespace | Key functions |
|-----------|---------------|
| `Int` | `fromString`, `fromFloat`, `toString`, `toFloat`, `abs`, `min`, `max`, `mod` |
| `Float` | `fromString`, `fromInt`, `toString`, `abs`, `floor`, `ceil`, `round`, `min`, `max` |
| `String` | `length`, `byteLength`, `charAt`, `startsWith`, `endsWith`, `contains`, `slice`, `trim`, `split`, `replace`, `join`, `chars`, `fromInt`, `fromFloat`, `fromBool` |
| `List` | `len`, `map`, `filter`, `fold`, `get`, `push`, `head`, `tail` |
| `Map` | `empty`, `fromList`, `set`, `get`, `has`, `remove`, `keys`, `values`, `entries`, `len` |
| `Char` | `toCode` (String→Int), `fromCode` (Int→Option\<String\>) — not a type, operates on String/Int |
| `Byte` | `toHex` (Int→Result), `fromHex` (String→Result) — not a type, operates on Int/String |

Full API reference: [docs/services.md](docs/services.md)

---

## Getting started

```bash
git clone https://github.com/jasisz/aver
cd aver
cargo build --release

cargo run -- run      examples/calculator.av
cargo run -- verify   examples/calculator.av
cargo run -- check    examples/calculator.av
cargo run -- run      examples/services/console_demo.av --record recordings/
cargo run -- replay   recordings/ --test --diff
cargo run -- context  decisions/architecture.av --decisions-only
cargo run -- context  examples/calculator.av
cargo run -- repl
```

Requires: Rust stable toolchain.

---

## Examples

| File | Demonstrates |
|------|-------------|
| `hello.av` | Functions, string interpolation, pipe, verify |
| `calculator.av` | Result types, match, decision blocks |
| `lists.av` | List literals, map / filter / fold |
| `shapes.av` | Sum types, qualified constructors (`Shape.Circle`), match on variants |
| `user_record.av` | Record types, field access, positional match |
| `fibonacci.av` | Tail recursion, records, decision blocks |
| `app.av` | Module imports via `depends [Examples.Fibonacci]` |
| `app_dot.av` | Dot-path imports (`depends [Examples.Models.User]`) |
| `services/http_demo.av` | HTTP service: GET, POST, response handling |
| `services/disk_demo.av` | Disk service: full I/O walkthrough |
| `services/console_demo.av` | Console service: print, error, warn, readLine |
| `services/tcp_demo.av` | TCP persistent connections (`Tcp.Connection`) |
| `services/weather.av` | End-to-end service: `HttpServer` + `Http` + `Tcp` |
| `decisions/architecture.av` | The interpreter documents itself in Aver |
| `type_errors.av` | Intentional type errors — shows what the checker catches |

---

## Documentation

| Document | Contents |
|----------|----------|
| [docs/services.md](docs/services.md) | Full API reference for all namespaces (signatures, effects, notes) |
| [docs/types.md](docs/types.md) | Key data types (compiler, AST, runtime) |
| [docs/extending.md](docs/extending.md) | How to add keywords, namespace functions, expression types |
| [docs/decisions.md](docs/decisions.md) | Partially generated ADR document from `decision` blocks |

---

## Project status

Implemented in Rust with extensive automated test coverage.

- [x] Lexer with significant indentation (Python-style INDENT/DEDENT)
- [x] Recursive-descent parser — hand-written, no libraries
- [x] Static type checker — blocks execution on type errors
- [x] Effect system — statically enforced + runtime call-edge gate
- [x] `verify` block runner — co-located tests
- [x] `decision` tooling — queryable ADRs via `aver context --decisions-only` and docs generation via `aver decisions --docs`
- [x] List builtins: `map`, `filter`, `fold`, `get`, `head`, `tail`, `push`, `find`, `any`, `zip`, `flatMap`
- [x] User-defined sum types (`type`) and product types (`record`)
- [x] List pattern matching (`[]`, `[h, ..t]`), tuple patterns (`(a, b)`, nested)
- [x] Module imports (`depends [Examples.Foo]`, `depends [Examples.Models.User]`)
- [x] AI context export — `aver context` emits Markdown or JSON
- [x] Interactive REPL — persistent state, multi-line, type-checked
- [x] Built-in services — Console, Http, HttpServer, Disk, Tcp — `! [Effect]` enforced everywhere
- [x] Deterministic replay — `aver run --record` + `aver replay` for effectful regression testing
- [x] Record update — `User.update(u, age = 31)`
- [x] Auto-memoization — pure recursive functions with scalar args
- [x] Tail-call optimization — self and mutual recursion
- [x] Effect aliases — `effects AppIO = [Console, Disk]`
- [x] Map, Char, Byte pure namespaces
- [x] Pipe operator — `value |> fn`
- [x] Error propagation — `expr?` on Result values
- [x] String interpolation — `"Hello, {name}!"`
- [x] Compile-time variable resolution
