# Aver

Aver is a statically-typed programming language designed for AI-assisted development. Its interpreter is written in Rust. The core premise: code is a letter to the next reader — who is increasingly an AI. Aver makes that letter as readable as possible without sacrificing rigour. Every function carries an optional prose description, every mutable variable can document why it exists, architectural decisions live as `decision` blocks right next to the code they describe, and inline `verify` blocks are the testing primitive.

## Language tour

### Functions

```aver
fn greet(name: String) -> String
    ? "Returns a personalised greeting."
    = "Hello, {name}!"

fn safeDivide(a: Int, b: Int) -> Result<Int, String>
    ? "Safe integer division. Returns Err when divisor is zero."
    ! [Io]
    match b:
        0 -> Err("Division by zero")
        _ -> Ok(a / b)
```

- `? "..."` — prose description attached to the function signature
- `! [Effect]` — declared effects (statically enforced)
- `= expr` — single-expression body shorthand
- Block body: indented statements, last expression is the return value

### Bindings

```aver
val name = "Alice"          // immutable
var count = 0               // mutable
    reason: "Tracks retries."
count = count + 1           // reassignment (no keyword)
```

### Match

`match` is the only branching construct — there is no `if`/`else` by design.

```aver
fn classify(n: Int) -> String
    = match n:
        0 -> "zero"
        _ -> "other"

fn handle(result: Result<Int, String>) -> String
    = match result:
        Ok(v)  -> str(v)
        Err(e) -> e
```

### Pipe operator

```aver
val loud = greet("Aver") |> shout
```

### Lists and higher-order functions

```aver
val numbers  = [1, 2, 3, 4, 5]
val doubled  = map(numbers, double)
val positives = filter(numbers, isPositive)
val total    = fold(numbers, 0, add)
```

Builtins: `len`, `get`, `head`, `tail`, `push`, `map`, `filter`, `fold`.

### Verify blocks

Tests live next to the code they cover.

```aver
verify safeDivide:
    safeDivide(10, 2) => Ok(5)
    safeDivide(7,  0) => Err("Division by zero")
```

`=>` separates input from expected output. Run with `aver verify file.av`.

### Module blocks

```aver
module Calculator
    intent:
        "Safe calculator demonstrating Result types and match expressions."
    exposes [safeDivide]
    depends [Core]
```

### Decision blocks

Architectural decision records as a language construct.

```aver
decision NoExceptions:
    date: "2024-01-15"
    reason:
        "Exceptions make error paths invisible at the call site."
        "Result forces the caller to acknowledge failure explicitly."
    chosen: Result
    rejected: [Exceptions, Nullable]
    impacts: [safeDivide, safeRoot]
```

Query all decisions in a file: `aver decisions examples/architecture.av`

## Type system

Primitive types: `Int`, `Float`, `String`, `Bool`, `Unit`.
Compound types: `Result<T, E>`, `Option<T>`, `List<T>`.
Escape hatch: `Any` — compatible with everything, opt-in.

Type errors block `run`, `check`, and `verify`. Unknown type annotations are hard errors, not silent `Any`.

## What Aver deliberately omits

| Absent | Reason |
|--------|--------|
| `if`/`else` | `match` is the only branching construct; exhaustive matching forces explicit handling of all cases |
| `for`/`while` loops | No imperative iteration; use `map`, `filter`, `fold` |
| `null` | `Option<T>` with `Some`/`None` only |
| Exceptions | `Result<T, E>` with `Ok`/`Err` only; errors are values |
| Global mutable state | Only `service` blocks with explicit `needs:` dependencies (planned) |
| Magic | No decorators, no implicit behaviour, no runtime reflection |

## CLI

```
aver run       file.av    # type-check then execute
aver check     file.av    # type errors + intent/desc warnings
aver verify    file.av    # run all verify blocks
aver decisions file.av    # print all decision blocks
```

## Building

Prerequisites: Rust toolchain (stable).

```bash
git clone <repo>
cd lumen-rs
cargo build --release
# binary at target/release/aver
```

Or run directly without installing:

```bash
cargo run -- run examples/hello.av
cargo run -- verify examples/calculator.av
cargo run -- check examples/calculator.av
cargo run -- decisions examples/architecture.av
```

## Examples

The `examples/` directory contains:

| File | Demonstrates |
|------|-------------|
| `hello.av` | Basic syntax: functions, string interpolation, pipe, verify |
| `calculator.av` | Result types, match, decision blocks, verify |
| `lists.av` | List literals, map/filter/fold, verify |
| `decisions.av` | Decision blocks as first-class language constructs |
| `architecture.av` | The interpreter's own architecture documented in Aver |
| `type_errors.av` | Intentional type errors — shows what the checker catches |

## Tests

```bash
cargo test    # 204 tests, 0 warnings
```

| File | What it covers | Tests |
|------|---------------|-------|
| `tests/lexer_spec.rs` | Token kinds, INDENT/DEDENT, interpolation, comments | 58 |
| `tests/parser_spec.rs` | AST shape for all constructs | 41 |
| `tests/typechecker_spec.rs` | Valid programs pass; all error categories fail correctly | 29 |
| `tests/eval_spec.rs` | Arithmetic, builtins, lists, match, pipe, HOFs | 65 |

## Status

Working: full pipeline (lex → parse → type-check → execute), all example programs, 204 passing tests.

Not yet implemented (planned): list pattern matching (`[]`, `[h, ..t]`), user-defined types (`type`), proper `?` short-circuit across function boundaries, module imports at runtime, service blocks.

## Design principles

1. **Intent over implementation** — signatures, descriptions, and decisions tell the full story without running the code
2. **Errors are values** — `Result<T, E>` everywhere; no exceptions
3. **Co-location** — tests (`verify`) and decisions (`decision`) live next to the code they describe
4. **Static by default** — type errors block execution; `Any` is an opt-in escape hatch
5. **No magic** — every behaviour is explicit and visible in the source
