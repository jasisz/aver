# Aver

> *Code is a letter to the next reader. Often, that reader is an AI.*

Aver is a programming language designed for AI-assisted development. In an era where code is increasingly generated and refactored by LLMs, traditional comments and scattered docs are no longer enough. Aver bakes architectural context and intent directly into the language's grammar.

## Why Aver?

Most languages optimise for the CPU. Aver optimises for understanding — human and machine alike.

**`?` Intent** — every function carries a prose description of what it does and why. Not a comment that rots in isolation, but a semantic contract attached to the signature.

**`!` Effects** — side effects are declared explicitly (`! [Network, Ledger]`). If a function "gets dirty", it must say so. Violations are type errors, and runtime enforces the same boundary as a backstop.

**`decision` blocks** — architectural decisions live in the codebase, not in a stale Confluence page. The *why* behind every *how*, queryable without leaving your editor.

**`verify` blocks** — tests are physically attached to the functions they cover. Self-verifying modules are the only way to scale safe AI automation.

Explicit `depends` for code, explicit `! [Effect]` for capabilities.

## Syntax at a glance

```aver
module Payments
    intent:
        "Processes transactions with an explicit audit trail."
    depends [Ledger, Fraud, Models.User]

decision UseResultNotExceptions:
    date: "2024-01-15"
    reason:
        "Invisible exceptions lose money."
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

## Philosophy

Aver doesn't trust the developer — and it certainly doesn't trust the AI. It enforces strict **Context-First** hygiene:

- Function with effects but no description `?` → **Warning**
- `verify` case fails → **Error**
- Type mismatch anywhere in the call graph → **Blocked before execution**

Aver isn't for moving fast and breaking things. It's for building systems that stay maintainable for years, regardless of who — or what — is writing the code.

## The type system

Primitive types: `Int`, `Float`, `String`, `Bool`, `Unit`
Compound types: `Result<T, E>`, `Option<T>`, `List<T>`, `Fn(A, B) -> C`, `Fn(A) -> C ! [Effect]`
User-defined sum types: `type Shape` with variants `Shape.Circle(Float)`, `Shape.Rect(Float, Float)`, `Shape.Point`
User-defined product types: `record User` with named fields, field access `u.name`
Escape hatch: `Any` — compatible with everything, opt-in only

Module imports at runtime:
- `depends [Fibonacci]` -> `fibonacci.av` / `Fibonacci.av`, call `Fibonacci.fn(...)`
- `depends [Models.User]` -> `models/user.av` / `Models/User.av`, call `Models.User.fn(...)`

Type errors block `run`, `check`, and `verify`. The checker runs before a single line executes. Runtime also enforces declared effects on every call edge.

## What Aver deliberately omits

| Absent | Reason |
|--------|--------|
| `if`/`else` | `match` is the only branching construct — exhaustive, no silent omissions |
| `for`/`while` | No imperative iteration; use `map`, `filter`, `fold` |
| `null` | `Option<T>` with `Some`/`None` only |
| Exceptions | `Result<T, E>` only — errors are values, not control flow |
| Global mutable state | Planned: `service` blocks with explicit `needs:` dependencies |
| Magic | No decorators, no implicit behaviour, no runtime reflection |

## CLI

```
aver run       file.av             # type-check, then execute
aver check     file.av             # type errors + intent/desc warnings
aver verify    file.av             # run all verify blocks
aver decisions file.av             # print all decision blocks
aver context   file.av             # export project context (Markdown)
aver context   file.av --json      # export project context (JSON)
aver context   file.av -o ctx.md   # write to file instead of stdout
aver repl                          # interactive REPL
```

## AI context export

`aver context` traverses the dependency graph starting from an entry file and emits a compact summary of everything an LLM needs to understand the project — without seeing the full source.

```bash
aver context examples/calculator.av
```

```markdown
# Aver Context — examples/calculator.av

## Module: Calculator
> Safe calculator demonstrating Result types, match expressions, and co-located verification.

### `safeDivide(a: Int, b: Int) -> Result<Int, String>`
> Safe integer division. Returns Err when divisor is zero.
verify: `safeDivide(10, 2)` → `Ok(5)`, `safeDivide(7, 0)` → `Err("Division by zero")`

---

## Decisions

### NoExceptions (2024-01-15)
**Chosen:** Result — **Rejected:** Exceptions, Nullable
> Exceptions make error paths invisible at the call site...
```

What it collects per file:

| Field | Source |
|-------|--------|
| Module intent | `intent:` block |
| Public function signatures | `fn` defs, filtered by `exposes [...]` |
| Effect declarations | `! [Console, Disk]` on each function |
| Prose descriptions | `? "..."` attached to functions |
| Verify cases | up to 3 per function from `verify` blocks |
| Type and record definitions | `type` / `record` |
| Effect set aliases | `effects AppIO = [Console, Disk]` |
| Architectural decisions | `decision` blocks from all reachable files |

The result is typically 1–3 k tokens — enough context for an LLM to understand contracts, boundaries, and rationale without the full implementation.

## Network service

Aver ships a built-in `Network` namespace for HTTP. All methods require `! [Network]` — the typechecker and runtime both enforce this.

```aver
record Header
    name: String
    value: String

fn fetchUser(id: Int) -> Any
    ! [Network]
    Network.get("https://api.example.com/users/{id}")

fn createItem(url: String, body: String, token: String) -> Any
    ! [Network]
    val headers = [Header(name: "Authorization", value: "Bearer {token}")]
    Network.post(url, body, "application/json", headers)
```

| Method | Signature |
|--------|-----------|
| `Network.get(url)` | `String -> Result<NetworkResponse, String> ! [Network]` |
| `Network.head(url)` | `String -> Result<NetworkResponse, String> ! [Network]` |
| `Network.delete(url)` | `String -> Result<NetworkResponse, String> ! [Network]` |
| `Network.post(url, body, contentType, headers)` | `String, String, String, Any -> Result<NetworkResponse, String> ! [Network]` |
| `Network.put(url, body, contentType, headers)` | same |
| `Network.patch(url, body, contentType, headers)` | same |

**`NetworkResponse` shape** (runtime record — field access via `.`):

```aver
resp.status   -- Int    (e.g. 200, 404, 500)
resp.body     -- String (up to 10 MB; Err if exceeded)
resp.headers  -- List of records with .name and .value String fields
```

**Semantics:**
- `Ok(response)` for any completed HTTP exchange, including 4xx and 5xx — status codes are values, not exceptions
- `Err(String)` only for transport failures (connection refused, timeout, DNS error)
- Response body limit: 10 MB — returns `Err` if exceeded (no silent truncation)
- Default timeout: 10 seconds
- `headers` for POST/PUT/PATCH: any `List` of records that have `name: String` and `value: String` fields — `type_name` is not checked

## Getting started

```bash
git clone https://github.com/jasisz/aver
cd aver
cargo build --release
# binary at target/release/aver

cargo run -- run      examples/calculator.av
cargo run -- verify   examples/calculator.av
cargo run -- check    examples/calculator.av
cargo run -- decisions decisions/architecture.av
```

Requires: Rust stable toolchain.

## Examples

| File | Demonstrates |
|------|-------------|
| `hello.av` | Functions, string interpolation, pipe, verify |
| `calculator.av` | Result types, match, decision blocks |
| `lists.av` | List literals, map / filter / fold |
| `shapes.av` | Sum types, qualified constructors (`Shape.Circle`), match on variants |
| `user_record.av` | Record types, field access, positional match |
| `fibonacci.av` | Tail recursion, records (`FibStats`), single-pass accumulator, decision blocks |
| `app.av` | Runtime module imports via `depends [Fibonacci]` |
| `app_dot.av` | Dot-path imports and nested namespaces (`depends [Models.User]`) |
| `models/user.av` | Module loaded via dot-path from `app_dot.av` |
| `decisions/decisions.av` | Decision blocks as first-class constructs |
| `decisions/architecture.av` | The interpreter documents itself in Aver — [see it](decisions/architecture.av) |
| `type_errors.av` | Intentional type errors; shows what the checker catches |

## Project status

Implemented in Rust, zero warnings.

- [x] Lexer with significant indentation (Python-style INDENT/DEDENT)
- [x] Recursive-descent parser — no libraries, hand-written
- [x] Static type checker — blocks execution on errors
- [x] Effect system — statically enforced + runtime call-edge gate
- [x] `verify` block runner — co-located tests
- [x] `decision` block indexer — queryable ADRs
- [x] List builtins: `map`, `filter`, `fold`, `get`, `head`, `tail`, `push`
- [x] User-defined sum types (`type`) with qualified constructors (`Shape.Circle`)
- [x] User-defined product types (`record`) with field access and positional match
- [x] List pattern matching (`[]`, `[h, ..t]`)
- [x] Module imports at runtime (`depends [Foo]`, `depends [Models.User]`)
- [x] AI context export — `aver context` emits Markdown or JSON from the dependency graph
- [x] Interactive REPL — `aver repl` with persistent state, multi-line input, type-checking
