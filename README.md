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
    match amount:
        0 -> Err("Cannot charge zero")
        _ -> Ok("txn-{amount}")
```

```aver
fn fetchExchangeRate(currency: String) -> Result<Float, String>
    ? "Fetches live rate from the ECB feed."
    ! [Network]
    Network.get("https://api.ecb.europa.eu/rates/{currency}")
```

Effect declarations (`! [Network]`, `! [Disk]`, `! [Console]`) are part of the signature. The type checker enforces them — a function that calls `Network.get` without declaring `! [Network]` is a **type error, not a warning**. The runtime enforces the same boundary as a backstop.

You can read any function in Aver and know exactly what it's capable of — without running it, without reading its body.

### "Why was this decision made?"

```aver
decision UseResultNotExceptions:
    date: "2024-01-15"
    reason:
        "Invisible exceptions lose money at runtime."
        "Callers must handle failure — Result forces that at the call site."
    chosen: Result
    rejected: [Exceptions, Nullable]
    impacts: [charge, refund, settle]
    author: "team"
```

`decision` blocks are first-class syntax, co-located with the code they describe. Not a Confluence page that goes stale. Not a commit message no one reads. Queryable:

```bash
aver decisions payments.av
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

### `charge(account: String, amount: Int) -> Result<String, String>` ! [Network, Ledger]
> Charges account. Returns txn ID or a human-readable error.
verify: `charge("alice", 100)` → `Ok("txn-alice-100")`, `charge("bob", 0)` → `Err("Cannot charge zero")`

## Decisions
### UseResultNotExceptions (2024-01-15)
**Chosen:** Result — **Rejected:** Exceptions, Nullable
> Invisible exceptions lose money at runtime...
```

This is what AI-assisted development actually needs: context that travels with the code, not context that lives in someone's memory.

### "How do I know a refactor didn't break anything?"

```aver
verify charge:
    charge("alice", 100) => Ok("txn-alice-100")
    charge("bob",   0)   => Err("Cannot charge zero")
    charge("x",    -1)   => Ok("txn-x--1")
```

`verify` blocks are intended to stay close to the function they cover (same module, usually right after the definition). `aver check` warns when a non-`main` function has no `verify` block by name. They run with `aver verify` (or `aver run --verify`) under the same type/effect checks as normal code.

---

## Full example

```aver
module Payments
    intent:
        "Processes transactions with an explicit audit trail."
    depends [Ledger, Models.User]
    exposes [charge]

decision UseResultNotExceptions:
    date: "2024-01-15"
    reason:
        "Invisible exceptions lose money at runtime."
        "Callers must handle failure — Result forces that at compile time."
    chosen: Result
    rejected: [Exceptions, Nullable]
    impacts: [charge]

fn charge(account: String, amount: Int) -> Result<String, String>
    ? "Charges account. Returns txn ID or a human-readable error."
    ! [Network, Ledger]
    match amount:
        0 -> Err("Cannot charge zero")
        _ -> Ok("txn-{account}-{amount}")

verify charge:
    charge("alice", 100) => Ok("txn-alice-100")
    charge("bob",   0)   => Err("Cannot charge zero")
```

No `if`/`else`. No loops. No exceptions. No nulls. No magic.

---

## CLI

```
aver run       file.av        # type-check, then execute
aver check     file.av        # type errors + intent/desc warnings
aver verify    file.av        # run all verify blocks
aver decisions file.av        # print all decision blocks
aver context   file.av        # export project context (Markdown)
aver context   file.av --json # export project context (JSON)
aver repl                     # interactive REPL
```

---

## Type system

Primitive: `Int`, `Float`, `String`, `Bool`, `Unit`
Compound: `Result<T, E>`, `Option<T>`, `List<T>`, `Fn(A) -> B`, `Fn(A) -> B ! [Effect]`
User-defined sum types: `type Shape` → `Shape.Circle(Float)`, `Shape.Rect(Float, Float)`
User-defined product types: `record User` → `User(name: "Alice", age: 30)`, `u.name`
Expression type ascription: `expr: Type` (e.g. `[]: List<Header>`)

Module imports: `depends [Fibonacci]` → `fibonacci.av`, call as `Fibonacci.fn(...)`
Dot-path imports: `depends [Models.User]` → `models/user.av`, call as `Models.User.fn(...)`

Type errors block `run`, `check`, and `verify`. No partial execution.

---

## What Aver deliberately omits

| Absent | Reason |
|--------|--------|
| `if`/`else` | `match` is exhaustive — no silent missing cases |
| `for`/`while` | Use `map`, `filter`, `fold` — iteration is data transformation |
| `null` | `Option<T>` with `Some`/`None` only |
| Exceptions | `Result<T, E>` only — errors are values |
| Global mutable state | No shared mutable state by design |
| Magic | No decorators, no implicit behaviour, no runtime reflection |

---

## Built-in services

Aver ships built-in namespaces for I/O. All require explicit effect declarations — the typechecker and runtime both enforce this.

### Console

```aver
fn log(msg: String) -> Unit
    ! [Console]
    Console.print(msg)      -- stdout
    Console.warn(msg)       -- stderr, prefixed [warn]
    Console.error(msg)      -- stderr
    val line = Console.readLine()  -- Result<String, String>
```

### Network

```aver
fn fetchUser(id: Int) -> Result<NetworkResponse, String>
    ! [Network]
    Network.get("https://api.example.com/users/{id}")
```

| Method | Args | Returns |
|--------|------|---------|
| `Network.get(url)` | `String` | `Result<NetworkResponse, String>` |
| `Network.head(url)` | `String` | `Result<NetworkResponse, String>` |
| `Network.delete(url)` | `String` | `Result<NetworkResponse, String>` |
| `Network.post(url, body, contentType, headers)` | `String, String, String, List<Header>` | `Result<NetworkResponse, String>` |
| `Network.put` / `Network.patch` | same | same |

`headers` is statically typed as `List<Header>`. Use `[]` when you do not need extra headers.

`NetworkResponse` fields: `resp.status` (Int), `resp.body` (String), `resp.headers` (List of `{name, value}` records).
`Ok` for any completed HTTP exchange including 4xx/5xx. `Err` only for transport failures.

### Disk

```aver
fn loadConfig(path: String) -> Result<String, String>
    ! [Disk]
    Disk.readText(path)
```

| Method | Returns |
|--------|---------|
| `Disk.readText(path)` | `Result<String, String>` |
| `Disk.writeText(path, content)` | `Result<Unit, String>` |
| `Disk.appendText(path, content)` | `Result<Unit, String>` |
| `Disk.exists(path)` | `Bool` |
| `Disk.delete(path)` | `Result<Unit, String>` — files only |
| `Disk.deleteDir(path)` | `Result<Unit, String>` — recursive |
| `Disk.listDir(path)` | `Result<List<String>, String>` |
| `Disk.makeDir(path)` | `Result<Unit, String>` — `mkdir -p` semantics |

---

## Getting started

```bash
git clone https://github.com/jasisz/aver
cd aver
cargo build --release

cargo run -- run      examples/calculator.av
cargo run -- verify   examples/calculator.av
cargo run -- check    examples/calculator.av
cargo run -- decisions decisions/architecture.av
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
| `app.av` | Module imports via `depends [Fibonacci]` |
| `app_dot.av` | Dot-path imports (`depends [Models.User]`) |
| `http_demo.av` | Network service: GET, POST, response handling |
| `disk_demo.av` | Disk service: full I/O walkthrough |
| `console_demo.av` | Console service: print, error, warn, readLine |
| `decisions/architecture.av` | The interpreter documents itself in Aver |
| `type_errors.av` | Intentional type errors — shows what the checker catches |

---

## Project status

Implemented in Rust with extensive automated test coverage (300+ tests).

- [x] Lexer with significant indentation (Python-style INDENT/DEDENT)
- [x] Recursive-descent parser — hand-written, no libraries
- [x] Static type checker — blocks execution on type errors
- [x] Effect system — statically enforced + runtime call-edge gate
- [x] `verify` block runner — co-located tests
- [x] `decision` block indexer — queryable ADRs
- [x] List builtins: `map`, `filter`, `fold`, `get`, `head`, `tail`, `push`
- [x] User-defined sum types (`type`) and product types (`record`)
- [x] List pattern matching (`[]`, `[h, ..t]`)
- [x] Module imports (`depends [Foo]`, `depends [Models.User]`)
- [x] AI context export — `aver context` emits Markdown or JSON
- [x] Interactive REPL — persistent state, multi-line, type-checked
- [x] Built-in services — Console, Network, Disk — `! [Effect]` enforced everywhere
