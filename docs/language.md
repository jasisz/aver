# Aver — Language Guide

This document covers the surface language: syntax, semantics, modules, and the intentional omissions.

For constructor-specific rules, see [constructors.md](constructors.md).

For namespaces, services, and standard library APIs, see [services.md](services.md).

## Types

Primitive: `Int`, `Float`, `String`, `Bool`, `Unit`
Compound: `Result<T, E>`, `Option<T>`, `List<T>`, `Map<K, V>`, `(A, B, ...)`, `Fn(A) -> B`, `Fn(A) -> B ! [Effect]`
User-defined sum types: `type Shape` → `Shape.Circle(Float)`, `Shape.Rect(Float, Float)`
User-defined product types: `record User` → `User(name = "Alice", age = 30)`, `u.name`

`Unit` means "no meaningful value". It is similar to `void`, but still a real type; diagnostics render the value as `()`. Effectful functions such as `Console.print` commonly return `Unit`.

## Bindings

All bindings are immutable. No `val`/`var` keywords — they are parse errors.

```aver
name = "Alice"
age: Int = 30
xs: List<Int> = []
```

Optional type annotation provides a hint to the type checker; the annotation wins over inference when both are compatible. Binding to an empty list literal without a type annotation (`x = []`) is a type error.

Duplicate binding of the same name in the same scope is a type error.

## Operators

Arithmetic: `+`, `-`, `*`, `/` — operands must match (`Int+Int`, `Float+Float`, `String+String`). No implicit promotion; use `Int.toFloat` / `Float.fromInt` to convert.
Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`.
Error propagation: `expr?` — unwraps `Result.Ok`, propagates `Result.Err` as a `RuntimeError`.

## String interpolation

Expressions inside `{}` are evaluated at runtime:

```aver
greeting = "Hello, {name}! You are {age} years old."
```

## Constructors

UpperCamel callee = constructor, lowerCamel = function call. Records use named args (`User(name = "A", age = 1)`), variants use positional args (`Shape.Circle(3.14)`), zero-arg constructors are bare singletons (`Option.None`, `Shape.Point`).

All constructors are namespaced — no bare `Ok`/`Err`/`Some`/`None`:

```aver
Result.Ok(42)
Result.Err("not found")
Option.Some("hello")
Option.None
```

## Match expressions

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
    (a, b) -> "pair: {a}, {b}"             // tuple destructuring
    ((x, y), z) -> "nested: {x}"           // nested tuple
```

Constructor patterns are always qualified (`Result.Ok`, `Option.None`, `Shape.Circle`). Records do not support positional destructuring in patterns; bind the whole record and use field access (`user.name`, `user.age`).

Nested match in match arms is supported. Arm body must follow `->` on the same line — extract complex expressions into a named function.

## Record update

Creates a new record with overridden fields, preserving all other fields:

```aver
updated = User.update(u, age = 31)
```

## Map literals

```aver
m = {"key" => value, "other" => 42}
```

`=>` is required inside map literals; `:` stays type-only.

## Effect aliases

Named effect sets reduce repetition:

```aver
effects AppIO = [Console.print, Disk.readText]

fn main() -> Unit
    ! [AppIO]
    // ...
```

## Command-line arguments

Programs access CLI arguments via the `Args` service:

```aver
fn main() -> Unit
    ! [Args.get, Console.print]
    args = Args.get()
    Console.print(args)
```

Run with: `aver run file.av -- arg1 arg2 arg3`

Arguments after `--` are available as `List<String>`. Without `--`, the list is empty. `Args.get()` requires `! [Args.get]` — argument access is visible in the signature like any other effect.

## Functions

```aver
fn add(a: Int, b: Int) -> Int
    a + b

fn fetchUser(id: String) -> Result<HttpResponse, String>
    ? "Fetches a user record from an API."
    ! [Http.get]
    Http.get("https://api.example.com/users/{id}")
```

- `? "..."` — optional prose description (part of the signature)
- `aver check` warns when non-`main` functions omit the description
- `! [Effect]` — optional effect declaration (statically and runtime enforced)
- method-level effects are supported: `Http.get`, `Disk.readText`, `Console.print`
- top-level functions are first-class values and can be passed where `Fn(...)` is expected
- `main` often returns `Unit`, but `Result<Unit, String>` is also common; `aver run` treats `Result.Err(...)` returned from `main` as a runtime failure
- function bodies use indentation
- the last expression in a function body is the return value

## Verify blocks

Regular `verify` blocks live directly under the function they cover:

```aver
verify add
    add(0, 0) => 0
    add(2, 3) => 5
```

Law-style verify blocks express finite universal checks over explicit domains:

```aver
verify add law commutative
    given a: Int = -2..2
    given b: Int = [-1, 0, 1]
    add(a, b) => add(b, a)
```

If the identifier after `law` is the name of an existing pure function and the law body compares `foo(args)` against `fooSpec(args)`, Aver treats that as a spec law. `verify fib law fibSpec` is the preferred way to say "fib should match fibSpec".

This is an intentional style choice. In Aver, the author should usually write a simple spec function and a law relating the implementation to that spec, instead of writing proof-oriented invariants directly in surface code.

`verify` is deterministic, not random. Regular cases run exactly as written. `verify ... law ...` expands the cartesian product of explicit `given` domains, capped at `10_000` cases.

`aver check` expects pure, non-trivial, non-`main` functions to carry a colocated `verify` block. Effectful flows should be tested through record/replay rather than `verify`.

## Decision blocks

`decision` blocks are first-class top-level syntax for design rationale:

```aver
decision UseResultNotExceptions
    date = "2024-01-15"
    reason =
        "Invisible exceptions lose control flow."
        "Result keeps failure explicit at the call site."
    chosen = "Result"
    rejected = ["Exceptions", "Nullable"]
    impacts = [charge, refund, settle]
    author = "team"
```

`chosen`, `rejected`, and `impacts` may reference validated symbols or quoted semantic labels. Decisions are exported through `aver context ... --decisions-only`.

## No closures

All user-defined functions are top-level. At call time, a function sees globals + its own parameters — no closure capture at definition time.
Top-level functions are still first-class values, so higher-order builtins such as `HttpServer.listenWith(port, context, handle)` work without introducing lambda syntax or hidden captures.
There is no lambda syntax. List processing is typically written with recursion and pattern matching rather than callback-based helpers.

## Common patterns

```aver
fn sum(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [head, ..tail] -> head + sum(tail)
```

```aver
hasAlice = List.contains(["alice", "bob"], "alice")
```

```aver
ages = Map.fromList([("alice", 30), ("bob", 25)])
maybe_age = Map.get(ages, "alice")
```

## Auto-memoization

Pure recursive functions with memo-safe arguments (scalars, records/variants of scalars) are automatically memoized at runtime. No keyword needed — the compiler detects eligibility via call-graph analysis (Tarjan SCC). Cache is capped at 4096 entries per function.

## Tail-call optimization

Self and mutual tail recursion is optimized automatically. A transform pass after parsing rewrites tail-position calls into a trampoline — no stack growth for recursive functions. Tail position = last expression in function body, or each arm body in a `match` at tail position.

## Modules

Module imports resolve from a module root (`--module-root`, default: current working directory).
Each module file must start with `module <Name>` and contain exactly one module declaration.

```aver
module Payments
    intent = "Processes transactions."
    depends [Examples.Fibonacci]
    exposes [charge]
```

`depends [Examples.Fibonacci]` → `examples/fibonacci.av`, call as `Examples.Fibonacci.fn(...)`.
`depends [Examples.Models.User]` → `examples/models/user.av`, call as `Examples.Models.User.fn(...)`.

## Static type checking

Type errors block `run`, `check`, and `verify`. No partial execution. The checker covers function bodies, top-level statements, effect propagation, and duplicate binding detection.

## What Aver deliberately omits

| Absent | Reason |
|--------|--------|
| `if`/`else` | `match` is exhaustive — no silent missing cases |
| `for`/`while` | Use recursion, pattern matching, and explicit list operations |
| `null` | `Option<T>` with `Some`/`None` only |
| Exceptions | `Result<T, E>` only — errors are values |
| Global mutable state | No shared mutable state by design |
| Closures | All functions are top-level — no captured variables, explicit is better than implicit |
| Magic | No decorators, no implicit behaviour, no runtime reflection |
