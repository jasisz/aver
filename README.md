# Aver

> *Code is a letter to the next reader. Often, that reader is an AI.*

Aver is a programming language designed for AI-assisted development. In an era where code is increasingly generated and refactored by LLMs, traditional comments and scattered docs are no longer enough. Aver bakes architectural context and intent directly into the language's grammar.

## Why Aver?

Most languages optimise for the CPU. Aver optimises for understanding — human and machine alike.

**`?` Intent** — every function carries a prose description of what it does and why. Not a comment that rots in isolation, but a semantic contract attached to the signature.

**`!` Effects** — side effects are declared explicitly (`! [Network, Ledger]`). If a function "gets dirty", it must say so. Violations are type errors, not warnings.

**`decision` blocks** — architectural decisions live in the codebase, not in a stale Confluence page. The *why* behind every *how*, queryable without leaving your editor.

**`verify` blocks** — tests are physically attached to the functions they cover. Self-verifying modules are the only way to scale safe AI automation.

## Syntax at a glance

```aver
module Payments
    intent:
        "Processes transactions with an explicit audit trail."
    depends [Ledger, Fraud]

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
Compound types: `Result<T, E>`, `Option<T>`, `List<T>`
Escape hatch: `Any` — compatible with everything, opt-in only

Type errors block `run`, `check`, and `verify`. The checker runs before a single line executes.

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
aver run       file.av    # type-check, then execute
aver check     file.av    # type errors + intent/desc warnings
aver verify    file.av    # run all verify blocks
aver decisions file.av    # print all decision blocks
```

## Getting started

```bash
git clone https://github.com/jasisz/aver
cd aver
cargo build --release
# binary at target/release/aver

cargo run -- run      examples/calculator.av
cargo run -- verify   examples/calculator.av
cargo run -- check    examples/calculator.av
cargo run -- decisions examples/architecture.av
```

Requires: Rust stable toolchain.

## Examples

| File | Demonstrates |
|------|-------------|
| `hello.av` | Functions, string interpolation, pipe, verify |
| `calculator.av` | Result types, match, decision blocks |
| `lists.av` | List literals, map / filter / fold |
| `decisions.av` | Decision blocks as first-class constructs |
| `architecture.av` | The interpreter's own architecture — documented in Aver |
| `type_errors.av` | Intentional type errors; shows what the checker catches |

## Project status

Implemented in Rust. 204 tests, zero warnings.

- [x] Lexer with significant indentation (Python-style INDENT/DEDENT)
- [x] Recursive-descent parser — no libraries, hand-written
- [x] Static type checker — blocks execution on errors
- [x] Effect system — statically enforced, violations are type errors
- [x] `verify` block runner — co-located tests
- [x] `decision` block indexer — queryable ADRs
- [x] List builtins: `map`, `filter`, `fold`, `get`, `head`, `tail`, `push`
- [ ] List pattern matching (`[]`, `[h, ..t]`)
- [ ] User-defined types (`type` keyword)
- [ ] Module imports at runtime
- [ ] AI context export — semantic maps to JSON/Markdown for LLM context windows
