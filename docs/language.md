# Aver — Language Guide

This document covers the surface language: syntax, semantics, modules, and the intentional omissions.

For constructor-specific rules, see [constructors.md](constructors.md).

For namespaces, services, and standard library APIs, see [services.md](services.md).

For Oracle laws and trace assertions over classified effects, see [oracle.md](oracle.md).

## Types

Primitive: `Int`, `Float`, `String`, `Bool`, `Unit`
Compound: `Result<T, E>`, `Option<T>`, `List<T>`, `Vector<T>`, `Map<K, V>` (`K` must be a type that orders — see [Map literals](#map-literals)), `(A, B, ...)`, `Fn(A) -> B`, `Fn(A) -> B ! [Effect]`

Each primitive has exactly one spelling — the string type is written `String`, never abbreviated.

There is no dedicated `Set` type — use `Map<T, Unit>` (see [Sets](#sets) below).
User-defined sum types: `type Shape` → `Shape.Circle(Float)`, `Shape.Rect(Float, Float)`
User-defined product types: `record User` → `User(name = "Alice", age = 30)`, `u.name`

Declare them with the type name on its own line and the members indented beneath — one variant per line for a sum type, one `field: Type` per line for a record:

```aver
type Shape           // sum type
    Circle(Float)
    Rect(Float, Float)
    Point            // zero-arg variant — a bare singleton (Shape.Point)

record User          // product type
    name: String
    age: Int
```

`Unit` means "no meaningful value". It is similar to `void`, but still a real type; diagnostics render the value as `()`. Effectful functions such as `Console.print` commonly return `Unit`.

## Bindings

All bindings are immutable. No `val`/`var` keywords — they are parse errors.

The leading `__` namespace is reserved for the compiler. User-written names
for modules, types, variants, fields, functions, operations, resources,
parameters, bindings, match-pattern binders, and decisions cannot begin with
two underscores. Double underscores elsewhere in a name remain legal
(`walk__cursor` is a valid name). Names in that namespace that the compiler
defines — the `__loopStart` / `__LoopOutcome` protocol of a
[yielding function](#yielding-functions), for example — are ordinary
functions and types of the module: user code calls them, annotates with
them, and matches on their constructors like on anything it wrote itself.

```aver
name = "Alice"
age: Int = 30
xs: List<Int> = []
```

Optional type annotation provides a hint to the type checker; the annotation wins over inference when both are compatible. Binding to an empty list literal without a type annotation (`x = []`) is a type error.

Every name means one thing in its scope. Duplicate binding of the same name in the same scope is a type error, and so is shadowing: a binder — a function parameter, a statement binding, or a match-pattern binding — may not reuse any name already visible at that point, including a top-level function of its own module and the enclosing function's own name. The error names both sides and where the shadowed one is defined; the fix is one rename. Sibling match arms may bind the same name (neither is in the other's scope), and cross-module names are always `Module.fn`-qualified, so nothing outside the file can collide. In `aver repl` the session is the scope: the rule reads everything entered so far together with the entry being read, so a binder may not spell a function defined in an earlier entry — a refused entry is not added to the session, and `:clear` starts a fresh one.

## Operators

Arithmetic: `+`, `-`, `*` — operands must match (`Int+Int`, `Float+Float`, `String+String`). No implicit promotion; use `Float.fromInt` / `Int.fromFloat` to convert. The `/` operator is **Float-only**; integer `/` is a type error. For integers use `Int.div(a, b) : Result<Int, String>` (Euclidean; `b == 0` → `Result.Err`) and `Int.mod(a, b) : Result<Int, String>` — there is no integer `%`. `Int` is arbitrary-precision (ℤ): no overflow, no wraparound.
Bit-level operations live in the `Bits` namespace, not in the operator set: `Bits.and`, `Bits.or`, `Bits.xor`, `Bits.not` are `Int -> Int` under infinite two's complement (`Bits.not(x) == -x - 1`, `Bits.and(-1, x) == x`), and `Bits.shiftLeft(x, n)` / `Bits.shiftRight(x, n)` / `Bits.low(x, width)` are `x * 2^n` / `floor(x / 2^n)` / `x mod 2^width`. Their `Result<Int, String>` rejects negative counts; the materialization cap applies only when a result may grow (`shiftLeft`, and `low` for negative inputs), never to shrinking `shiftRight`. A syntactic bounded non-negative literal discharges `shiftLeft`/`low`, while any non-negative literal discharges `shiftRight`. `Bits` is a namespace, not a type: nothing here is a machine word, and `Int` still never overflows or wraps — width is requested explicitly through `Bits.low`, never implied. See [docs/services.md](services.md#bits-namespace).

Literal-divisor discharge: when the divisor of `Int.div` / `Int.mod` is a syntactic nonzero integer literal — `Int.div(x, 2)`, `Int.mod(x, -3)` — the call cannot fail, so it types as plain `Int` and every backend emits the division directly (no `Result`, no unwrapping). The boundary is exactly "a syntactic integer literal other than `0`, optionally under one unary minus": a `0` literal, an identifier, a named constant, or a constant expression like `8 + 8` all keep the `Result<Int, String>` type unchanged. Parentheses are transparent here, because the parser erases them around a single expression: `(16)`, `(-16)` and `-(16)` are the same syntax tree as `16` and `-16`, so all three discharge — while `(0)` is still zero and `(k)` is still an identifier, and both keep the `Result` type. This is a typing rule for these two functions only, not a general constant-propagation or refinement mechanism.
Literal smart-constructor discharge: the same idea extends to a validating smart constructor over a `List<Int>` carrier — the shape `stdlib/bytes.av` uses. When the argument is a syntactic list of integer literals and every element is inside the interval the refinement itself proves, the call cannot reach its `Result.Err` branch, so it types as the refined type and constructs the value directly: `Bytes.fromList([0, 10, 255]) : Bytes`, no `?` and no `match`. The empty list `Bytes.fromList([])` discharges too. The boundary is narrow and entirely syntactic on the argument side: there must be exactly one argument, it must be a list literal written out at the call site, and every element must be a plain integer literal with at most one unary minus. What decides is the function the call resolves to, never how it is spelled: `Bytes.fromList(...)` from outside and a bare `fromList(...)` inside the defining module both reach the constructor and both discharge, while a module that declares its own `fromList` shadows the imported one as usual — that call means the local function and is not discharged at all. Everything else keeps `Result<Bytes, String>` unchanged — an identifier (`Bytes.fromList(values)`), a computed list (`Bytes.fromList(List.concat(a, b))`), a computed element (`Bytes.fromList([n * 2])`), an out-of-range literal (`Bytes.fromList([65, 256])`), a negative one (`Bytes.fromList([-1])`), or a literal beyond `i64`. The bound is never hardcoded: it is read off the refinement's own validating predicate, so a user-defined refinement with a different range discharges against that range, and a record with no smart constructor never discharges at all. Programs run under `--self-host` are refused with an explicit error when they contain a discharged call, because the self-hosted resolver does not yet carry the rule.
Literal effect-contract discharge applies the same user-facing rule to `Random.int` and `Time.sleep`: proven-valid literal arguments remove the `Result` wrapper, but the effect still executes. The backend unwrap is private and fail-closed. If a provider or Oracle stub returns `Err` despite those proven arguments, execution faults as a contract violation; the compiler never calls `Result.withDefault` or invents a random/sleep result.
Unary minus negates a numeric expression: `-n` (equivalent to `0 - n`), and numeric literals may be written negative (`-3`, `-1.5`).
Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`.
Error propagation: `expr?` — unwraps `Result.Ok`, propagates `Result.Err` as a `RuntimeError`.
Independent products: `(a, b)!` — product of independent computations. `(a, b)?!` — same, with Result unwrapping (all must succeed or first error propagates). Elements cannot reference each other; independence is structural. Composes recursively for fan-out parallelism. See [independence.md](independence.md).

## String interpolation

Expressions inside `{}` are evaluated at runtime:

```aver
greeting = "Hello, {name}! You are {age} years old."
```

Interpolation renders primitives only: an embedded expression must be an `Int`, a `Float`, a `Bool` or a `String`. Embedding anything else — a list, a record, a tuple, a `Map`, an `Option`/`Result`, a `Vector`, a refinement or other named type — is a type error, because an interpolation site is a display site and Aver requires every conversion to `String` to be named in the source. There is no built-in renderer for compound values and none is planned: write a function that returns `String` and interpolate its result (`"cart: {cartLine(item)}"`), or convert at the call site with an explicit conversion such as `String.fromInt(n)`. The rule is the same one that makes `Console.print(list)` a type error; the interpolated form is only sugar over the same display.

An embed whose type inference never pinned is rejected too, with a diagnostic saying the type could not be determined. This happens when the value flows from a still-open generic — matching on a bare `Option.None` or a bare `[]` binds the arm's variable to a type nothing in the program fixes. Give the subject a concrete type (`match someOption` where `someOption: Option<Int>`) and the embed becomes an ordinary primitive or an ordinary compound, with the ordinary answer in each case.

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
    42 -> "exact"                          // Int literal
    "verack" -> "known command"            // String literal
    _ -> "anything"                        // wildcard
    x -> "bound to {x}"                    // identifier binding
    [] -> "empty list"                     // empty list
    [h, ..t] -> "head {h}, {List.len(t)} more"  // list cons
    Result.Ok(v) -> "success: {v}"         // constructor
    Result.Err(e) -> "error: {e}"
    Shape.Circle(r) -> "circle r={r}"
    Shape.Point -> "point"
    (a, b) -> "pair: {a}, {b}"             // tuple destructuring
    ((x, y), z) -> "nested: {x}"           // nested tuple
```

Constructor patterns are always qualified (`Result.Ok`, `Option.None`, `Shape.Circle`). Records do not support positional destructuring in patterns; bind the whole record and use field access (`user.name`, `user.age`).

Nested match in match arms is supported. Arm body must follow `->` on the same line — extract complex expressions into a named function.

### Literal patterns

An arm may be a literal instead of a binding; it fires when the subject equals it. This is how you dispatch on a command name or a tag byte — there is no `else if` to reach for, and no reason to spread the decision over a chain of single-purpose helper functions:

```aver
fn handle(command: String) -> Int
    ? "Dispatch on the wire command name."
    match command
        "verack" -> 1
        "version" -> 2
        "inv" -> 3
        "tx" -> 4
        _ -> 0

fn varIntWidth(head: Int) -> Int
    ? "253 introduces two more bytes, 254 four, 255 eight."
    match head
        253 -> 2
        254 -> 4
        255 -> 8
        _ -> 1
```

`Int`, `String`, `Float` and `Bool` literals are all valid patterns. `Bool` is the only one a match can exhaust by listing (`true` and `false`), so it is the only one that needs no catch-all; an `Int`, `String` or `Float` match must end in a wildcard `_` or an identifier arm, or the checker rejects it with `Non-exhaustive match: missing catch-all (_) pattern`. Repeating a literal is rejected too — the later arm can never fire, and the error names the line that already covers it.

Three things that look like literal patterns are parse errors:

- a negative number — `-1 -> …` does not parse, because the `-` is a separate token and a pattern is not an expression. Branch on a comparison instead (`match n < 0` with `true ->` / `false ->`), or normalize the subject before the match.
- an integer beyond 64 bits, even though `Int` itself is arbitrary-precision. The error points at the replacement: `match n == 1267650600228229401496703205376`.
- an interpolated string — `"{x}" -> …` is rejected, because a pattern is a constant. Compare with `==` when the expected value is computed.

`Float` literal patterns compare exactly, so `0.1 + 0.2` does not match a `0.3` arm. Use them only for sentinels you produced yourself; otherwise branch on a comparison.

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

A map iterates its entries sorted by key — when you run a program, in a compiled binary, and in the exported proof model — so its key type must be one all of those can order the same way. Most types are: `Int` numerically, `String` by codepoint, `Bool` false-first, a list or a `Bytes` lexicographically, a tuple componentwise, a record by its FIELD NAMES, a variant by its CONSTRUCTOR NAME and then its payload. Ordering a record by field name rather than by the order the fields were declared in is deliberate: declaration order is not observable anywhere else — a record is built and read by name — so ordering by it would make reordering two fields change how every map on that key iterates.

`Float` is the exception and cannot be a map key: a NaN has no place in the finite range, and neither a compiled binary nor the proof model can state an order the other agrees with. Nor can a `Map` or a `Vector`, which have no order of their own. The rule reaches through your own types, so a record with a `Float` field cannot key a map either, and the error names the field it found. Float stays legal as a map *value*.

## Effects

Effects are exact method names:

```aver
fn main() -> Unit
    ! [Console.print, Disk.readText]
    Console.print("starting")
    _ = Disk.readText("data.txt")
```

Both granular and namespace shorthand declarations are supported. `! [Disk.readText]` declares a single effect, while `! [Disk]` covers all `Disk.*` effects (namespace shorthand). `aver check` suggests narrowing when a shorthand could be more specific. `effects X = [...]` aliases are no longer supported.

Entries are separated by commas, and the comma is required: `! [Console.error Console.print]` is a parse error naming the effect it stopped after, not two effects. The list may be written across several lines, and a trailing comma is allowed.

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

`aver run` starts from `main` by default. To record or run any other top-level function, pass `-e '<call>'` (repeat for a batch) or `--input-file PATH`: `aver run file.av -e 'load("PL")' --record recordings/`. Arguments are limited to literals in 0.10.1; wrap complex inputs in a helper function.

## Functions

```aver
fn add(a: Int, b: Int) -> Int
    a + b

fn fetchUser(id: String) -> Result<Http.Response, String>
    ? "Fetches a user record from an API."
    ! [Http.get]
    Http.get("https://api.example.com/users/{id}")
```

- `? "..."` — optional prose description (part of the signature)
- deeper-indented string lines continue the same description:
  ```aver
  ? "Starts the CLI."
    "Dispatches one argv command."
  ```
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

When a law needs an explanation, optional `because` lines express ordered facts in ordinary, pure Aver:

```aver
verify identity law positive
    given value: Int = [-3, 0, 1, 7]
    when value > 0
    because value >= 1
    because value + 1 > 1
    using []
    identity(value) > 0 holds
```

Each explanation must return `Bool`. It may call a normal function whose `match` branches describe the argument. `verify` and `verify --hostile` check every explanation under the original `when`, as well as checking the claim. A false explanation fails even if the claim is true; it never restricts the law's domain.

A recursive explanation can guide induction when it has a checked structural descent on a list or a native, checked integer countdown (including floor division by a positive literal). The recursive step must establish the original guard and any earlier reason premises for its own arguments. This uses ordinary function recursion; there is no separate induction syntax.

`because` entries are ordered: the proof of a later fact can use earlier facts. The optional `using [function.law, Module.function.law]` list selects an unordered set of lemmas; omitting it keeps automatic selection, and `using []` selects none. Ordinary local bindings may appear between these clauses and remain expression shortcuts, not assertions. The first proof implementation targets Lean; see [law explanations](lean.md#law-explanations-in-aver) for the obligations, diagnostics, and limits.

`verify` is deterministic, not random. Regular cases run exactly as written. `verify ... law ...` expands the cartesian product of explicit `given` domains, capped at `10_000` cases — a project that means to go further says so in `aver.toml`, with `[verify] max-cases` for the whole project or `max-cases` in a `[[verify.costly]]` entry for the blocks of one function.

Oracle laws cover classified effectful functions:

```aver
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>
    ? "Deterministic Random.int stub."
    Result.Ok(4)

fn pickOne() -> Int
    ? "Rolls once."
    ! [Random.int]
    Random.int(1, 6)

verify pickOne law usesOracle
    given rnd: Random.int = [fairDie]
    Result.Ok(pickOne()) => rnd(BranchPath.Root, 0, 1, 6)
```

Inside any cases-form `verify <fn>` block, `given` can bind a capability operation or classified effect to one or more Aver stub functions for those explicit runtime cases. A pure capability stub has the operation's contract signature unchanged; an effectful/generative stub uses the Oracle shape with leading `BranchPath` and call index. In `verify <fn> law <name>`, proof export can additionally quantify over the oracle itself. Add `trace` when you want `.result` and `.trace.*` assertions over collected classified effect emissions.

A plain case may call a function with a non-empty effect declaration as long as
that concrete execution never reaches an effectful operation, or every reached
operation has an exact `given` stub. An unstubbed reached effect aborts before
host dispatch and points to `verify <fn> trace` or record/replay. Aver does not
infer path reachability from the function-wide effect list, and plain verify is
not a real-world smoke-test mode.

Effects outside Oracle's classified set still belong in record/replay, especially ambient state, persistent protocol sessions, terminal modes, and server loops. See [oracle.md](oracle.md) for the supported effect set, stub signatures, and trace API.

`aver check` expects pure, non-trivial, non-`main` functions to carry a colocated `verify` block.

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
Top-level functions are still first-class values, so higher-order helpers such as `HttpServer.listen(port, handle)` work without introducing lambda syntax or hidden captures.
There is no lambda syntax. List processing is typically written with recursion and pattern matching rather than callback-based helpers.

This means `Fn(...) -> ...` is a real type, but a function value may appear **only as a function parameter** — i.e. a named function (or builtin / constructor) passed directly in call-argument position, exactly as `HttpServer.listen(port, handle)` does. A `Fn(...)` type used as a function's **return type**, a **record or variant field**, a **collection or tuple element**, or nested inside another `Fn`, and binding a function value to a local (`g = double`) — are all rejected at type-check time. Function values therefore never escape callback-argument position, so the concrete callee at every call — and with it the set of effects it can perform — stays statically knowable, which is what the effect system, the Oracle, and `aver verify` rely on. If you need to select between functions dynamically, branch at the call site or model the choice as a sum type and `match` on it.

A callback effect list of `! [_]` means “forward the concrete named callback's
effects”. It is resolved statically at the helper call site; it is not an
ambient wildcard or a hidden capability grant.

```aver
fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int
    f(f(x))

fn inc(n: Int) -> Int
    n + 1
```

Most application code in Aver stays first-order and explicit. Use function parameters when they make an API cleaner, not as a default abstraction tool.

## Sets

Aver has no dedicated `Set` type. The idiomatic way to express a set is `Map<T, Unit>` — a map whose values carry no information. All `Map.*` operations work on sets:

```aver
seen: Map<String, Unit> = {}
seen2 = Map.set(seen, "alice", Unit)
Map.has(seen2, "alice")   // true
Map.len(seen2)            // 1
seen3 = Map.remove(seen2, "alice")
```

`Map.set(s, k, Unit)` adds an element, `Map.has(s, k)` checks membership, `Map.remove(s, k)` removes an element, and `Map.len(s)` returns cardinality. Map literals with `Unit` values work as set literals: `{"alice" => Unit, "bob" => Unit}`.

When targeting Dafny, the codegen lowers `Map<T, Unit>` to the native set type. Lean has no set type the generated project can reach, so there it stays an ordinary map:

| Backend | Aver type | Target type | `Map.set(s, k, Unit)` |
|---------|-----------|-------------|----------------------|
| Dafny | `Map<T, Unit>` | `set<T>` | `s + {k}` |
| Lean | `Map<T, Unit>` | `List (T × Unit)` | `AverMap.set s k ()` |

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

```aver
// Vector: indexed dense data (grids, buffers, lookup tables)
grid = Vector.new(100, 0)          // 100 zeros
updated = Vector.set(grid, 42, 1)  // Option<Vector<Int>>
value = Vector.get(grid, 42)       // Option<Int>
```

`Vector.new(size, fill)` is fallible for a dynamic size. A syntactic literal
in the portable `0..=1_048_576` element budget, as above, discharges directly
to `Vector<T>`; negative, oversized, or computed sizes keep
`Result<Vector<T>, String>`. The budget counts elements because Aver has no
backend-independent byte size for an arbitrary `T`.

## Tail-call optimization

Self and mutual tail recursion is optimized automatically. A transform pass after parsing rewrites tail-position calls into a trampoline — no stack growth for recursive functions in tail position. Tail position = last expression in function body, or each arm body in a `match` at tail position.

This is intentionally narrower than “all recursion”. Non-tail recursion can still be expensive on large inputs, so `aver check` warns when a recursive function still has non-tail recursive callsites after TCO. In practice, long linear traversals are best written in accumulator style when scale matters.

## Yielding functions

A function whose effect list names `yield` hands control back instead of performing the operations this program answers itself. A capability is answered by the program when its `[[providers.bindings]]` entry in `aver.toml` carries `answer = "<Module>"`, naming the module that computes the answer; every operation of such a capability is a request, and everything else the function calls runs where it is written. The function is written in direct style — read, then the next step — but it never runs as written: the compiler cuts it at every stop and turns it into plain data and pure functions, and a coordinator you write performs the operations and feeds the answers back. `yield` is an effect like any other: it appears in the function's `! [...]` and it must be covered by the module's `effects [...]`. A bare name in an effect list — a function's `! [...]` or a module's `effects [...]` — is `yield`, the forwarding marker `_`, or a standard capability namespace such as `Console`; a program-defined capability is named by its operations, never bare, so anything else — including a misspelled `yeild` or a bare program-defined capability name — is an error naming the unknown or disallowed effect. A yielding function is called only through its generated entry points: `__<fn>Start` and the answer functions are pure, so a coordinator that calls them declares no `yield`.

```aver
fn loop(id: Int, done: Int) -> Int
    ? "Claims handles for id until the pool answers None, summing them into done."
    ! [Pool.claim, yield]
    r = Pool.claim(id)
    match r
        Option.None -> done
        Option.Some(h) -> loop(id, done + h)
```

Inside a yielding function a call to an operation of an answered capability is a stop (a request), and the self tail call is a stop of kind `Yield`. Everything else runs inline, including an operation of a capability nobody answers: that one runs inside the turn, where it is written, and the generated function holding it declares it in its own `! [...]`. A function that declares `yield` and never stops — it calls no answered operation and does not tail-call itself — is an error that names both repairs: mark the capability, or drop `yield`. The mirror of that rule is `error[intercept-outside-yield]`: a function without `yield` that performs an operation of an answered capability has made a request nobody will answer, and the message names the answer module's own function to call instead. A program that answers a capability runs on the bytecode VM and on the Rust backend; `aver run --wasm-gc` and `--wasip2` refuse it with `error[work-target]`, because the generated reply types carry `Wait.Wake` and those two targets have no representation for the job handle it reaches. For `loop` the compiler generates, in the same module and in the reserved `__` namespace:

- `__LoopClaimState` — one sum type per request kind, with one variant per stop of that kind; a variant holds exactly the variables the rest of that path still reads (`AwaitR(Int, Int)` for `id` and `done`). A stop bound to a name is `Await<Name>`; an unbound stop is `Await<n>` with its ordinal in the function.
- `__LoopYieldState` — the state of the tail call: its argument tuple (`Await2(Int, Int)`).
- `__LoopRequest` — one constructor per kind carrying the operation's arguments and the state of that kind: `Claim(Int, __LoopClaimState) | Yield(__LoopYieldState)`.
- `__LoopOutcome` — `Done(Int) | Waiting(__LoopRequest)`.
- `__loopStart(id: Int, done: Int) -> __LoopOutcome` — runs to the first stop; it carries the original `? "..."` description.
- `__loopAnswerClaim(__state: __LoopClaimState, __answer: Option<Int>) -> __LoopOutcome` — matches the state variant and runs to the next stop or to `Done`. The answer type is the operation's result type, so pairing a state with the answer of another kind is a type error. An operation whose result is `Unit` has no answer to pass: its answer function takes the state only (`__loopAnswerPrint(__state)`).
- `__loopAnswerYield(__state: __LoopYieldState) -> __LoopOutcome` — re-enters `__loopStart` with the carried arguments.

These names are compiler-defined and callable. The original `loop` is removed after lowering; the coordinator answers the requests instead:

```aver
fn drive(outcome: __LoopOutcome, answers: List<Option<Int>>) -> Int
    ? "Answers every Claim request from the list and resumes every Yield request."
    match outcome
        __LoopOutcome.Done(v) -> v
        __LoopOutcome.Waiting(request) -> match request
            __LoopRequest.Yield(state) -> drive(__loopAnswerYield(state), answers)
            __LoopRequest.Claim(peer, state) -> match answers
                [] -> drive(__loopAnswerClaim(state, Option.None), [])
                [answer, ..rest] -> drive(__loopAnswerClaim(state, answer), rest)
```

Stops may sit anywhere the function runs unconditionally — in a binding, as a match subject, inside an argument — and inside `match` arms; a request in tail position, as the last expression of the body or as the leaf of a `match` arm, is a stop like any other, and the answer to it is what the function returns. The same operation may stop several times in one body, and `?` after a request works (`Err` leaves through `Done`). When code follows a stop that sits in a `match` arm of a non-tail statement, the rest of the path becomes a generated continuation function (`__loopJoin1`, `__loopAfterAwaitR`) the arms call. The generated items are ordinary types and pure functions: `aver verify` runs them, every backend compiles them, and `aver proof` exports them to Lean and Dafny like anything else, so the coordinator's laws can reason about the protocol.

A module that exposes a yielding function exposes its protocol in its place: `exposes [loop]` becomes the generated names, and an importer writes `Looper.__loopStart(...)`, matches `Looper.__LoopOutcome` and answers with `Looper.__loopAnswerClaim(...)`. The function itself is not on the module's surface at all, so `Looper.loop(...)` from a dependent module is the same error with the qualified recipe.

Two diagnostics guard the shape:

- calling a yielding function directly, from a function that does not yield — a plain function, a verify case, a dependent module — is a type error: `'loop' yields; call '__loopStart(...)' and answer its requests`;
- a yielding function that calls *itself* outside tail position is a type error with the recipe `pass what comes next as data, or make it a tail call`: a process nests another yielding function, not itself, because its own state would have to hold a copy of itself.

### Helpers and nested state

A process does not have to be one function. A yielding function may call another yielding function of the same module, and the compiler puts the callee's machine inside the caller's rather than asking you to fold the two together by hand. Nothing about the coordinator changes: the caller's protocol is what a coordinator seats and serves, and a request the helper makes reaches it as a request of the caller.

A **tail call** enters the helper's protocol. `f`'s segment that ends in `g(args)` stops with a `Yield` request carrying what `g` is entered with — the caller has nothing left to do, so nothing of it is kept — and answering that request calls `__gStart(args)`.

A **non-tail call** nests the helper's state inside the caller's. For `x = g(args)` with more work after it, the caller's state sum for every kind `g` waits on gains one variant per call site, `In<G>At<N>(<the helper's own state>, <the caller's live variables>)`; the caller's request sum gains the kinds `g` waits on that it does not already have; and the caller's answer function for such a kind hands the answer down to `__gAnswer<Kind>` and routes what comes back — a `Done(v)` binds `x = v` and continues the caller's segment, another request leaves again as a request of the caller with the new nested state. A nested call is not a stop by itself: a helper that waits on nothing runs to its result inside the caller's own segment.

That is the whole generated shape, for `walk` calling `fetch`:

```aver
type __WalkClaimState
    InFetchAt1(__FetchClaimState, Int, Int)

fn __walkStart(id: Int, seen: Int) -> __WalkOutcome
    __walkInFetchAt1(__fetchStart(id), id, seen)

fn __walkAnswerClaim(__state: __WalkClaimState, __answer: Int) -> __WalkOutcome
    match __state
        __WalkClaimState.InFetchAt1(__inner, id, seen) -> __walkInFetchAt1(__fetchAnswerClaim(__inner, __answer), id, seen)

fn __walkInFetchAt1(__outcome: __FetchOutcome, id: Int, seen: Int) -> __WalkOutcome
    match __outcome
        __FetchOutcome.Done(got) -> __walkJoin1(id, seen, got)
        __FetchOutcome.Waiting(__request) -> match __request
            __FetchRequest.Claim(__a0, __inner) -> (__WalkOutcome).Waiting((__WalkRequest).Claim(__a0, (__WalkClaimState).InFetchAt1(__inner, id, seen)))
```

The helper's own recursion stays inside the helper's machine: `g` looping on itself is `g`'s `Yield` request, which leaves as the caller's `Yield` request carrying the nested state and comes back to `__gAnswerYield`. These names are the caller's own `__` types; nothing new reaches the module's surface, and `aver context`, `AVER_YIELD_DUMP=1` and the pinned generated Aver show them.

The tail-call rule: a self tail call is the `Yield` request, and a tail call to another yielding function of the same module enters that function's protocol. What is still refused, each with the construct named in the message: mutual nesting — two yielding functions that call each other, because each one's state would have to hold the other's, so the message prints the cycle and two ways to break it, and a cycle written with tail calls only, where no state is held but each function's requests would have to carry the ones it hands over to; a request or a call to a yielding helper inside an independent product `(a, b)!`, because its branches run independently and a request leaves a process one at a time; a function value live across a stop; and a yielding function of *another* module, whose protocol is what that module exposes — call `Module.__fStart(...)` and answer its requests from the coordinator. A request in a product fires less often than it used to, because only an answered operation inside the product is refused.

## The coordinator

Writing that coordinator by hand is the part nobody enjoys: a slot table, an instance number per request, a wait set, a poll timeout, one dispatch arm per request kind per process, the seam a job result comes back through, and the turn around all of it. So a program does not write it. A program that puts a `[run]` table in its `aver.toml` writes **processes, answer modules and three policies, and nothing else** — no coordinator, no `main`, no seating, no slot table. The compiler generates the rest into the entry module, in the reserved `__` namespace, by the same pass that generates the protocol. The generated names stay callable, so a program that wants its own loop over the protocol still has one; that is a door, not the road.

The worked example is `tests/fixtures/run_all_slice/` (`examples/concurrency/README.md` points at it and says why it lives there): a peer that fetches block bodies, a walk that connects them, an accepting process, a dialling process and a ticker — five processes, three answer modules, one job kind, three policies, and not one line between them.

**What the program writes.** The manifest says who answers what and asks for the loop:

```toml
[[providers.bindings]]
capability = "Wire"
answer = "Sockets"

[[providers.bindings]]
capability = "Validation"
work = "Ledger.validate"
task = "Ledger.nextTask"
started = "Ledger.taskStarted"
landed = "Ledger.validated"

[run]
order = "Node.order"
admit = "Node.admit"
stop = "Node.stop"
view = "Node.View"
```

The processes are ordinary yielding functions, in direct style, written in the module the `[run]` table names, each taking no parameters and answering `Unit` — the loop seats one of each at start-up, and a seated process asks the module that answers its first request for whatever it needs. A yielding function in any other module of such a program is `error[run-binding]`: the loop is generated into one module and seats what it can see, so a process one module over would be lowered to its protocol and then never seated, never dispatched and never answered.

```aver
fn peer() -> Unit
    ? "Ask the pool what to fetch, let the helper fetch it, go round; a peer the helper could not finish with is handed back."
    ! [Pool.claim, Pool.gone, Wire.write, Wire.read, Blocks.deliver, Console.print, yield]
    Console.print("peer: asking the pool for work")
    match Pool.claim()
        Pool.Assignment.Stop -> Unit
        Pool.Assignment.Work(key, height) -> match fetchBody(key, height)
            true -> peer()
            false -> Pool.gone(key)

fn fetchBody(key: Int, height: Int) -> Bool
    ? "One body from one peer: the write, the read and the hand-over. True when the body landed, false when anything on the way said this peer is finished."
    ! [Wire.write, Wire.read, Blocks.deliver, yield]
    match Wire.write(key, Ledger.blockOf(height))
        Result.Err(_) -> false
        Result.Ok(_) -> match Wire.read(key, 4000000, 30000)
            Wire.Heard.EndOfStream -> false
            Wire.Heard.Failed(_) -> false
            Wire.Heard.TimedOut -> false
            Wire.Heard.Data(raw) -> match Blocks.deliver(key, height, raw)
                Blocks.Receipt.Accepted -> true
                Blocks.Receipt.Refused(_) -> false
```

`Console.print` there is not a request: `Console` is nobody's to answer, so it runs in place, inside the turn, and the generated function that holds it declares it. `fetchBody` is a yielding helper, not a process: the loop seats `peer`, and the three requests `fetchBody` waits on reach the turn as requests of `peer` carrying the helper's state — see "Helpers and nested state" above. A process's own effect list still names what its helpers perform, because the program as written calls them.

The answer modules are ordinary modules with one state each. Every operation of every capability they answer gets one function, threading that state and answering `Now(v)` or `Later(wake)`, and every one of them declares `fresh()`, the state before anything has happened, because that is where the loop starts them:

```aver
fn claim(state: State) -> Tuple<State, Pool.__ClaimReply>
    ? "The next height for the next idle peer, Stop once every height has been handed out, and a Later with a deadline while no peer is free to take one."
    match state.nextHeight > state.lastHeight
        true -> (state, Pool.__ClaimReply.Now(Pool.Assignment.Stop))
        false -> claimIdle(state, state.idle)
```

A `Later` leaves the **request** where it is, with the same instance number and the same request value, and **keeps the state the module returned**, exactly as a `Now` does. A `Later` is where a module records its own progress: a partial write's offset, a retry count, a deadline of its own. So the rule to remember is not "a `Later` changes nothing" — it is "a `Later` leaves the request unchanged and keeps the module's state".

The worked example is a real socket, in the example's own `Sockets` module: `Tcp.writeNow` takes as many of the bytes offered it as the socket has room for right now and answers that count, so a payload it did not take whole leaves an offset behind and parks on that socket becoming writable again. Nothing but the module's own state carries the offset across the park, and the ask after the park goes on from exactly there:

```aver
fn write(state: State, key: Int, payload: Bytes) -> Tuple<State, Wire.__WriteReply>
    ? "One ask at one peer's write. Every ask is counted, so a run can say how many asks its payloads took."
    ! [Tcp.writeNow]
    writing(State.update(state, asked = state.asked + 1), key, payload, Map.get(state.peers, key))

fn offered(state: State, key: Int, connection: Tcp.Connection, payload: Bytes, sofar: Int) -> Tuple<State, Wire.__WriteReply>
    ? "One ask at one socket. It answers how many of the bytes offered it actually took, which is a count between nothing and the whole offer; a payload with bytes still to go records how far it got and parks on the socket becoming writable again, and the last byte answers the write."
    ! [Tcp.writeNow]
    match Tcp.writeNow(connection, offer(payload, sofar))
        Result.Err(reason) -> (dropped(state, key), Wire.__WriteReply.Now(Result.Err(reason)))
        Result.Ok(count) -> match whole(payload, sofar, count)
            true -> (finished(state, key), Wire.__WriteReply.Now(Result.Ok(Unit)))
            false -> (parked(state, key, sofar, count), Wire.__WriteReply.Later(Wait.Wake.Item(Wait.Item.Socket(Tcp.Socket.Sending(connection)))))
```

An answer module may perform effects — `answer-shape` says so rather than refusing it, because an answer runs inside the turn and a slow one stalls every other process — and this one does: it owns the `Tcp.Listener` its peers arrive on and the `Tcp.Connection` each peer key stands for, answers `Wire.accept` from `Tcp.accept`, `Wire.read` from `Tcp.readNow`, and parks on `Connected` when nothing has arrived. That is what a `Wire` is for: the processes above it say what they want, and one module says how a socket gives it to them.

What a `Later` carries is a `Wait.Wake`, and the wake **gates the ask**: a parked request is not asked again until what it is waiting for has happened.

- `Item(Wait.Item)` parks on a socket or a job. That slot is asked again only in a turn whose `Wait.poll` reported its key, and false readiness is allowed — the module may answer `Later` again.
- `After(ms)` parks on a deadline. At park time the turn turns `ms` into the clock reading it falls due at, `due = now + ms`, and keeps the `ms` that was asked for beside it; that slot is asked again in a turn whose clock reading has reached `due`, or in a turn whose reading has fallen further back than `ms` — a wall clock that steps backwards would otherwise leave the request waiting for a reading that never comes, and would hand the wait a timeout longer than anything in the program asked for.
- `NextTurn` asks to be asked again immediately, which makes that turn poll with a zero timeout for as long as such a request exists — so prefer `Item` or `After` when either will do.

A freshly seated process and a process whose request was just answered are askable at once.

The worked example of `After` is a clock that gives out a tick fifty milliseconds after it was asked for one. The first ask arms the deadline and parks; the ask after the deadline has passed is the tick. Four ticks are eight asks — two per tick, whatever else the run is doing and however many turns it takes — plus the closing ask that answers `Closed`, which is why the slice's run ends with a ticker asked nine times:

```aver
fn tick(state: State) -> Tuple<State, Clock.__TickReply>
    ? "The next tick. Every ask is counted, including the ones that park."
    ticking(State.update(state, asks = state.asks + 1))

fn armed(state: State) -> Tuple<State, Clock.__TickReply>
    ? "A tick is not due the moment it is asked for. The first ask arms the deadline and parks on it; the ask after that deadline has passed is the tick."
    match state.armed
        false -> (State.update(state, armed = true), Clock.__TickReply.Later(Wait.Wake.After(50)))
        true -> (State.update(state, armed = false, left = state.left - 1), Clock.__TickReply.Now(Clock.Tick.Tock))
```

Finally the view and the three policies. The view is a record the program declares and the loop fills; the checker holds it to exactly that shape and prints the declaration it wants under `error[view-shape]`:

```aver
type Pending
    Accepting(Int, Wait.Wake)
    Dialling(Int, Wait.Wake)
    Ticker(Int, Wait.Wake)
    Peer(Int, Wait.Wake)
    Walk(Int, Wait.Wake)

record View
    pending: Map<Int, Pending>
    ready: List<Int>
    askable: List<Int>
    jobs: Int
    room: Int
    stopping: Bool

fn order(view: View) -> List<Int>
    ? "Every seated process, in slot order. The loop asks admit about each one in turn."
    Map.keys(view.pending)

fn admit(view: View, id: Int) -> Bool
    ? "Whether to serve this id in this turn."
    match Map.get(view.pending, id)
        Option.None -> false
        Option.Some(marker) -> admitted(view, marker)

fn stop(view: View) -> Bool
    ? "The run ends when the process asked us to, and not before: the flag was read by the turn and reaches here as data."
    view.stopping
```

One constructor per process, carrying the instance number of the request that process is waiting on and the wake it is parked on. `ready` is what the turn's one wait reported; `askable` is the ids the turn may ask in this turn, in slot order, which is `ready` read through every slot's wake plus everything parked on the next turn and everything whose deadline has passed. `order` and `admit` see the whole view — a policy may look at a slot it cannot ask — but the turn asks `admit` only about askable ids and never serves a slot that is not askable, whatever `order` returned. The view carries no capability resource on purpose: a law's `given` domain is a list of sample values written in Aver, a program cannot construct a `Tcp.Connection` or a `Work.Job`, and a policy that read the whole run could therefore never have a law with a non-trivial sample. This one can, and the example's priority law is exactly that — a ready peer request is admitted whatever the job table looks like, so the turn, which serves before it starts another job, serves the peer first.

**What the compiler generates.** `AVER_YIELD_DUMP=1 aver check main.av --module-root .` prints the whole of it after the protocol. In outline:

- `__Process` and `__Slot` and `__Run` — the slot table (`Map<Int, __Slot>`), one field per answer module holding its state, the job table, the count of answers that arrived too late, the stop flag as data, the clock reading this turn made, and the next free id. A slot carries its instance number, its request, the wake it is parked on, `due`, the clock reading an `After` falls due at, and `ms`, the delay that `After` asked for.
- `__seat<P>` / `__seated<P>` — one of every process, seated at its first request under its own slot id.
- `__current`, `__nextInstance`, `__settle<P>`, `__settledSlot<P>`, `__park`, `__parked`, `__dueOf`, `__msOf` — the two invariants of the table. An answer that carries the current instance replaces that process's one slot and raises its number; an answer that carries an older one changes nothing and is counted. A `Later` parks the request where it stands, keeps the state the module returned, and turns an `After(ms)` into the clock reading it falls due at, keeping the `ms` that was asked for beside it.
- `__view`, `__pendingOf`, `__markerOf`, `__askableOf`, `__askable`, `__askableSlot` — the view the policies read, filled from the run, including the gate: which slots this turn may ask, read off each slot's wake against the keys the wait reported and the clock reading the turn made. A slot parked on a deadline is askable once that reading has reached `due`, and also once that reading has fallen back past the moment the request was parked, so a clock that steps backwards cannot strand it.
- `__waitSet`, `__timeout`, `__remaining` — one wait per turn, keyed by slot id, plus one key per running job; the timeout is zero while any request asked for the next turn, the smallest `due - now` still ahead otherwise, and one second when nothing carries either.
- `__serve`, `__serve<P>`, `__serve<P><Kind>` — the dispatch: one arm per request kind of each process, calling the answer module's own function and settling or parking on what it answered. There is no arm for an unanswered operation, because the lowering only makes a request out of an answered one.
- `__Job`, `__roomLeft`, `__jobHandle`, `__takeEach<J>`, `__taken<J>`, `__reported<J>`, `__finished<J>`, `__landed<J>`, `__startable<J>`, `__startJobs<J>`, `__began<J>`, `__seatedJob<J>`, `__consumed<J>` — the job seam, once per job kind `<J>` over one shared table. `__Job` is a generated sum with one variant per kind, so `jobs: Map<Int, __Job>` carries every kind under one `[work] max-jobs` limit and `__jobHandle` unwraps it for the wait and the cancel. A job outcome is a coordinator event, not an answer to a request: it goes into the answer state through `landed` and resumes nobody. A `take` that answers `Ok(None)` is a job reported ready that has not finished: it keeps its handle for a later turn. A `take` that answers `Err` — cancelled, a body that stopped, an id the engine has forgotten — will never produce a payload: the handle leaves the table, the error reaches `landed` as `Result.Err(reason)`, and the run goes on rather than stopping on the take. The start side runs ask → `begin` → `started`: `__startable<J>` answers `None` while the table has no room, `__began<J>` records nothing on a `begin` that answered `Err`, and `__seatedJob<J>` puts the handle in the table and calls the manifest's `started` through `__consumed<J>` only once `begin` has answered `Ok`, so the task is consumed exactly when it is really running and the next ask of the same turn is offered a different one.
- `__turn`, `__serveEach`, `__serveIf`, `__serveAdmitted`, `__runAll`, `main` — observe the stop flag and the clock, wait once, serve the askable slots the policy ordered and admitted, take what is over of every job kind, start what fits of every job kind; turn until the policy says stop or nothing is seated. Taking before starting lets a job that landed this turn free a slot the same turn can use. The clock is read once per turn, after the wait has returned and before the turn serves, so a deadline that fell due while the turn was asleep is askable in that same turn rather than the next one; the wait of the turn after it is measured against the same reading, and a recording replays it.
- `__over`, `__cancelEach` — the end of a run. A program with a job kind cancels every job it is still holding a handle for rather than abandoning it, so `main` performs `Work.cancel`.

Each of those carries its own effects, not the program's: `__seat<P>` performs what that process performs on its way to its first request, `__serve<P><Kind>` performs what the answer module performs plus what the segment it resumes performs, and a process that touches nothing has every function the loop generates for it pure — so one generative effect in one process cannot oracle-lift the laws about another. Only `__serve`, `__serveEach`, `__serveIf` and the turn carry the union, because the dispatch reaches every process. The entry module's own `effects [...]` is widened to admit what is generated into it: the wait, the stop observation, the clock reading (`Time.unixMs`), both ends of every job kind and the cancel are added to the list the source declares, because the module boundary has to hold the loop as well as the processes.

**And the laws.** The generator emits the loop's invariants as `verify` laws over the generated functions, so the program does not write those either. Per process: a late answer leaves the slot table exactly as it was, a late answer is counted, and settling never grows the table. Over `__park`: a `Later` keeps the instance number. Over `__parked`: a `Later` keeps the request. Over `__askableSlot`: a slot parked on a deadline the clock reading has not reached, and has not fallen back behind either, is not askable, and a slot whose deadline the reading has fallen back behind is askable — the gate, in the two `Int` comparisons a law can carry. Over `__remaining`: the wait one deadline contributes is never longer than the `ms` that deadline asked for, which is what a clock that steps backwards would otherwise break. Over `__settledSlot<P>`: the slot an answer for the current instance writes back carries a strictly higher instance number than the one it answered, so the instance just answered can never be current again and a second answer to it is a late answer. Over `__current`: the slot written under an id is the slot read back from it. `aver verify` runs them and `aver proof --backend lean --check` puts them on the Lean wall, where all twenty-eight of the example's laws close as universals. The last to close was the size comparison in "settling never grows the table", one per process: it needed the fact that a store under a key the map already holds does not move its size, which the Lean prelude now carries along with the fact that a removal never grows a map. The two halves above are still stated apart; that split was made for the same missing fact, so whether one composed claim — the instance `__current` reads after a settle is higher than the one before it — now closes is worth re-measuring. The socket half of the gate has no law: it is `List.contains(ready, id)` over what the wait reported, and a law that reaches the wait does not reach the wall.

**Where it runs.** The loop is ordinary Aver, so it runs wherever the program does: on the bytecode VM under `aver run`, and as a native binary from `aver compile --target rust`, whose generated crate carries the same answer modules, the same policies and the same turn, with the wait, the job engine and the job kinds answered by `aver-rt` instead of by the VM's providers. The one thing not to read into a side-by-side run is the order of two processes' output. The example parks requests on `Wait.Wake.After(2)`, `After(5)` and `After(50)`, which are wall-clock deadlines, so which turn a finished job lands in depends on how long that job took; a compiled function is faster than the smallest deadline in the program and the VM's child interpreter is not, so the two backends do the same work in a different interleaving. What does not change between them is how often a parked request is asked: the wake gates the ask on both. `--target wasm-gc` and `--target wasip2` still refuse the program.

Two limits worth knowing before you reach them. A process takes no parameters and answers `Unit`, because the loop seats it and has nothing to hand it and nowhere to put its result — everything a process needs comes from the module that answers its first request. A yielding *helper* is not a process and is not held to that: it takes parameters and answers whatever its caller reads, because the process that calls it is what the loop seats.

## Modules

Module imports resolve from a module root (`--module-root`, default: current working directory).
Each module file must start with `module <Name>` and contain exactly one module declaration.

```aver
module Payments
    intent = "Processes transactions."
    effects [Disk.readText]
    depends [Data.Fibonacci]
    exposes [charge]
```

`effects [...]` declares the module's effect boundary — the union of the effects its functions may perform, in the same granular/namespace-shorthand form as function-level `! [...]`. It goes after `intent`. `aver check` warns when a module with functions omits it; a pure module declares `effects []` explicitly.

### Capability modules

A capability module declares host-provided atoms without choosing how a host binds them. It is still an ordinary module for `depends`, visibility, and naming, but its `operation` declarations have signatures instead of Aver bodies:

```aver
module Clock
    kind = capability
    semantics = effectful
    exposes [now]

operation now() -> Int
    ? "Reads the provider's clock."
    oracle = generative
    replay = recorded
    hostile = [zero]

fn zero(path: BranchPath, call: Int) -> Int
    0
```

`semantics` is mandatory and homogeneous for the module:

- `pure` operations are total, deterministic functions for proof purposes and carry no effect. They cannot declare `oracle`, `replay`, `hostile`, or `unmodelled` fields.
- `effectful` operations are their own effect identities (`Clock.now`). Every operation declares an Oracle dimension (`generative`, `output`, or `generativeOutput`) and replay behavior. Generative results use `recorded`; output requires a `Unit` result and uses `reissued` or `suppressed`; `snapshot` is reserved for standard-library effects whose read-only behavior Aver audits itself.
- An operation is a first-order provider boundary, not a value: it cannot take or return `Fn`, be assigned, or be passed as a callback. Call it directly, including inside `!` and `?!`. Capability effect declarations must name exact operations; namespace shorthand is rejected at module and function scope.

For effectful capabilities, `given` and `aver verify --hostile` use the same Oracle stub signatures as built-in effects. A hostile profile belongs to the capability module, must be pure, and receives `BranchPath`, call index, then the operation arguments. If the operation mints a resource, one unconstrained fresh token appears between the call index and the original arguments; it is not assumed distinct from any other token. A `given` stub for a pure capability instead has the operation's ordinary contract signature, with no Oracle coordinates. Proof trust headers pin two separate SHA-256 identities: `contract_hash` covers the provider ABI and all reachable boundary types, while `model_hash` additionally covers Oracle/replay metadata and the transitive source closure of hostile profiles. Both identities hash canonical `u64be` length-framed descriptors, so field concatenation cannot collide. Provider choice and binding stay outside both hashes and outside the theorem.

`resource Token` inside a capability is representation-less: only its bound provider can mint a value. This is deliberately distinct from `exposes opaque [T]`: the latter hides an ordinary represented Aver type while preserving its value semantics; a capability resource has no Aver representation or language-visible identity. It may occur at most once in an operation's success payload, directly or through transparent `Result`/`Option` wrappers; resource consumers must use recorded replay. Runtime handles are tagged by binding instance and canonical type, survive independent-product child VMs, and never expose the provider payload. Capability resources, including represented wrapper types that transitively contain one, deliberately have no display identity, equality, serialization as a host payload, or map-key semantics.

An embedded Rust host installs a VM provider with `aver::provider::ProviderBinding` and `ProviderRegistry`. A generated Rust host installs that same public `aver_rt::provider::ProviderBinding` through the generated library's `install_provider_bindings` entry. Registration pins the exact `contract_hash` and the complete operation set before execution. Providers implement `aver_rt::provider::CapabilityProvider` and exchange only the closed, transport-neutral `ProviderValue` tree—not VM `NanValue` or the general interpreter `Value`. A returned `ProviderValue::ResultErr` is ordinary Aver data; `ProviderFault` or a provider panic is a separate boundary failure. Duplicate, incomplete, extra-operation, hash-mismatched, and wrong-return-shape bindings fail closed with provider-specific diagnostics.

`aver verify` does not discover or install host packages. A source-local cases-form binding such as `given hash: Hash160.digest = [fixtureHash]` installs that Aver function only for each expanded verify case. Namespaced capabilities use the same full canonical path as calls and diagnostics—for example `given probe: Domain.Crypto.Hash160.digest = [fixtureHash]`; a shortened or misspelled path is a static error, never an ignored binding. The alias may be unused in the assertion: the binding still redirects reached dispatch. It never satisfies normal `aver run` provider preflight and does not test the provider implementation itself.

```rust
use std::sync::Arc;
use aver::provider::{ProviderBinding, ProviderRegistry};

// `capabilities` is the CapabilityRegistry returned by type checking.
let clock = capabilities.contract("Clock").expect("Clock contract");
let mut providers = ProviderRegistry::for_program(capabilities.clone())?;
providers.bind(ProviderBinding::new(
    "Clock",
    clock.contract_hash.clone(),
    ["Clock.now"],
    Arc::new(SystemClock), // implements aver_rt::provider::CapabilityProvider
))?;
vm.set_provider_registry(Arc::new(providers));
vm.run()?;
```

Target support is explicit rather than inferred from a missing provider row.
`aver capabilities app.av` emits one deterministic row per loaded capability
and shipped target (`vm`, `rust`, `wasm-gc`, `wasip2`). A row is `provided`,
`host-bound` when an embedder, JavaScript host, or Component Model host must
install a provider, or `unsupported(reason)` with a stable architectural reason
such as `wit-boundary-type-unsupported`. A custom contract is
`host-bound[wasm-gc-import-required]` on raw wasm-gc; a WIT-lowerable custom
contract is `host-bound[component-import-required]` on wasip2. The manifest lists the full
declared operation set separately from operations used by the program; unused
contracts remain visible but never block compilation. `--json` emits the
versioned machine-readable form, including the exact offending operation,
parameter/result position, and Aver type when WIT lowering is unavailable.

Consequently `error[capability-provider-missing]` is reserved for a target that
can accept a provider but has no live binding. Artifact targets without an
adapter report `error[capability-target-unsupported]` instead, including the
target, capability, required operations, contract/model hashes, and reason.

The registry is shared by the main VM and every `!` / `?!` child, so all branches see the same provider instance and resource store. Recording adds a sorted capability provenance table with `contract_hash`, `model_hash`, provider identity, and implementation fingerprint. `recorded` and `suppressed` replay consume without calling a provider; `reissued` consumes the event and calls live; pure operations call live without emitting an event. Live pure/reissued replay requires the same identity and fingerprint. The compiler-shipped native, wasm-gc, and wasip2 adapters for one standard capability form one explicit replay-compatibility family: their target-specific identities may differ, but the fingerprint must still match, so a standard trace remains portable between backends. Custom providers remain identity-exact. Provider fingerprints are audit metadata supplied by the host, not theorem hashes; the runtime can expose drift, but it cannot stop a dishonest host from reusing an old fingerprint for changed code.

Custom bindings have three host-bound routes. A Rust embedder can install one typed
in-process provider binding unchanged in the VM or a generated Rust artifact. A
raw wasm-gc artifact imports the complete contract under a deterministic module
name containing its `contract_hash`, using native GC values and `externref`
resources; a JavaScript/Workers/Node host supplies it. See
[`docs/wasm-gc-custom-capabilities.md`](wasm-gc-custom-capabilities.md) for the
ABI and generated value factories. A
wasip2 artifact can import a generated WIT interface when every parameter and
result in the complete contract is `Unit`, `Bool`, `Float`, or `String`; pure
and effectful operations use the same transport. The component import pins the
full `contract_hash` and publishes both hashes in its sibling WIT. An external
Component Model host may implement that interface directly. For local execution,
`aver run app.av --wasip2` instead links the Rust package bound in `aver.toml`
through the cached host and dynamically adapts its existing `ProviderBinding`
to the same WIT interface. Without a binding, `aver run --wasip2` fails
preflight with `error[capability-provider-missing]`. The stock generated Rust binary
likewise has no custom binding and fails preflight; a separate Rust host links
the provider crate through Cargo and installs the binding explicitly.
Standard `Time` remains a provided binding: its canonical source is shipped at
`stdlib/capabilities/time.av`, and VM, generated Rust, wasm-gc, and wasip2 each
declare an exact shipped binding of that one contract. See
[`docs/wasip2.md`](wasip2.md#custom-capability-imports-phase-3a) for the boundary
and host contract.

### Opaque types

`exposes opaque` makes a type visible in signatures but blocks direct construction, field access, and pattern matching from outside the module. The type can still be passed around, returned, and stored.

```aver
module Pricing
    exposes [mkDiscount, percent]
    exposes opaque [Discount]

record Discount
    percent: Float

fn mkDiscount(p: Float) -> Result<Discount, String>
    ? "Only way to create a Discount from outside."
    match p < 0.0
        true  -> Result.Err("Discount cannot be negative")
        false -> Result.Ok(Discount(percent = p))

fn percent(d: Discount) -> Float
    ? "Public accessor."
    d.percent
```

From outside the module:
- `Pricing.mkDiscount(50.0)` — works (returns `Result<Discount, String>`)
- `Pricing.percent(d)` — works (returns `Float`)
- `Discount(percent = 50.0)` — **compile error** (opaque: cannot construct)
- `d.percent` — **compile error** (opaque: cannot access fields)

With `--module-root examples`:

- `depends [Data.Fibonacci]` → `examples/data/fibonacci.av`, call as `Data.Fibonacci.fn(...)`
- `depends [Modules.Models.User]` → `examples/modules/models/user.av`, call as `Modules.Models.User.fn(...)`

A type may be written bare — `Step` rather than `Domain.State.Step` — when
exactly one module in scope declares that name. In scope means the module
itself, the modules it names in `depends [...]`, and the types those modules
re-expose: a dependency that lists another module's type in its own
`exposes [...]` hands that type on, still under the name of the module that
declares it. A module elsewhere in the program that nobody imported here has
no say, so declaring a type in one cannot change what a name means anywhere
else. When two modules a file does import declare the same type name, the
bare form is an error naming both candidates, and the reference has to be
qualified.

The entry module is not an exception to that rule. A dependency names the
modules it uses in its own `depends [...]`, and nothing names the entry, so
the entry's own declarations are in scope for the entry's own code and
nowhere else. Which file you point `run`, `verify` or `compile` at therefore
never changes what a name written inside a dependency means.

## Static type checking

Type errors block `run`, `check`, and `verify`. No partial execution. The checker covers function bodies, top-level statements, effect propagation, and duplicate binding detection.

## What Aver deliberately omits

| Absent | Reason |
|--------|--------|
| `if`/`else` | `match` is exhaustive — no silent missing cases |
| `for`/`while` | Use recursion, pattern matching, and explicit list operations |
| Streams / channels / async iterators | Recursive `?!` over lists gives streaming, backpressure, and fan-out parallelism with no new concepts |
| Async runtime | Aver doesn't try to make streaming a primitive. Its parallelism model is explicit independence (`?!`), not a full async runtime. If you need stream abstractions, you can build them — but the language itself stays small and reviewable |
| `null` | `Option<T>` with `Some`/`None` only |
| Exceptions | `Result<T, E>` only — errors are values |
| Global mutable state | No shared mutable state by design |
| Closures | All functions are top-level — no captured variables, explicit is better than implicit |
| Magic | No decorators, no implicit behaviour, no runtime reflection |
| Bitwise operators (`&`, `\|`, `^`, `~`, `<<`, `>>`) | The operations exist, named, in the `Bits` namespace. Keeping them out of the syntax is the same choice as `/` and `%`: a bit-level reading of an integer is worth spelling out. `Bits` is a namespace, not a type — its arguments and results are ordinary `Int` values, read as an infinite two's-complement bit sequence for one call. Fixed width is requested explicitly via `Bits.low`, never implied by a register size. Writing one of these operators reports which function replaces it |
