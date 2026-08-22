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

The leading `__` namespace is reserved for the compiler. User-written
function names, parameter names, bindings, and match-pattern binders cannot
begin with two underscores. Double underscores elsewhere in a name remain
legal (`walk__cursor` is a valid name).

```aver
name = "Alice"
age: Int = 30
xs: List<Int> = []
```

Optional type annotation provides a hint to the type checker; the annotation wins over inference when both are compatible. Binding to an empty list literal without a type annotation (`x = []`) is a type error.

Every name means one thing in its scope. Duplicate binding of the same name in the same scope is a type error, and so is shadowing: a binder — a function parameter, a statement binding, or a match-pattern binding — may not reuse any name already visible at that point, including a top-level function of its own module and the enclosing function's own name. The error names both sides and where the shadowed one is defined; the fix is one rename. Sibling match arms may bind the same name (neither is in the other's scope), and cross-module names are always `Module.fn`-qualified, so nothing outside the file can collide. In `aver repl` the session is the scope: the rule reads everything entered so far together with the entry being read, so a binder may not spell a function defined in an earlier entry — a refused entry is not added to the session, and `:clear` starts a fresh one.

## Operators

Arithmetic: `+`, `-`, `*` — operands must match (`Int+Int`, `Float+Float`, `String+String`). No implicit promotion; use `Float.fromInt` / `Int.fromFloat` to convert. The `/` operator is **Float-only**; integer `/` is a type error. For integers use `Int.div(a, b) : Result<Int, String>` (Euclidean; `b == 0` → `Result.Err`) and `Int.mod(a, b) : Result<Int, String>` — there is no integer `%`. `Int` is arbitrary-precision (ℤ): no overflow, no wraparound.
Bit-level operations live in the `Bits` namespace, not in the operator set: `Bits.and`, `Bits.or`, `Bits.xor`, `Bits.not` are `Int -> Int` under infinite two's complement (`Bits.not(x) == -x - 1`, `Bits.and(-1, x) == x`), and `Bits.shiftLeft(x, n)` / `Bits.shiftRight(x, n)` / `Bits.low(x, width)` are `x * 2^n` / `floor(x / 2^n)` / `x mod 2^width`, returning `Result<Int, String>` because a negative count is refused. `Bits` is a namespace, not a type: nothing here is a machine word, and `Int` still never overflows or wraps — width is requested explicitly through `Bits.low`, never implied. See [docs/services.md](services.md#bits-namespace).

Literal-divisor discharge: when the divisor of `Int.div` / `Int.mod` is a syntactic nonzero integer literal — `Int.div(x, 2)`, `Int.mod(x, -3)` — the call cannot fail, so it types as plain `Int` and every backend emits the division directly (no `Result`, no unwrapping). The boundary is exactly "a syntactic integer literal other than `0`, optionally under one unary minus": a `0` literal, an identifier, a named constant, or a constant expression like `8 + 8` all keep the `Result<Int, String>` type unchanged. Parentheses are transparent here, because the parser erases them around a single expression: `(16)`, `(-16)` and `-(16)` are the same syntax tree as `16` and `-16`, so all three discharge — while `(0)` is still zero and `(k)` is still an identifier, and both keep the `Result` type. This is a typing rule for these two functions only, not a general constant-propagation or refinement mechanism.
Literal smart-constructor discharge: the same idea extends to a validating smart constructor over a `List<Int>` carrier — the shape `stdlib/bytes.av` uses. When the argument is a syntactic list of integer literals and every element is inside the interval the refinement itself proves, the call cannot reach its `Result.Err` branch, so it types as the refined type and constructs the value directly: `Bytes.fromList([0, 10, 255]) : Bytes`, no `?` and no `match`. The empty list `Bytes.fromList([])` discharges too. The boundary is narrow and entirely syntactic on the argument side: there must be exactly one argument, it must be a list literal written out at the call site, and every element must be a plain integer literal with at most one unary minus. What decides is the function the call resolves to, never how it is spelled: `Bytes.fromList(...)` from outside and a bare `fromList(...)` inside the defining module both reach the constructor and both discharge, while a module that declares its own `fromList` shadows the imported one as usual — that call means the local function and is not discharged at all. Everything else keeps `Result<Bytes, String>` unchanged — an identifier (`Bytes.fromList(values)`), a computed list (`Bytes.fromList(List.concat(a, b))`), a computed element (`Bytes.fromList([n * 2])`), an out-of-range literal (`Bytes.fromList([65, 256])`), a negative one (`Bytes.fromList([-1])`), or a literal beyond `i64`. The bound is never hardcoded: it is read off the refinement's own validating predicate, so a user-defined refinement with a different range discharges against that range, and a record with no smart constructor never discharges at all. Programs run under `--self-host` are refused with an explicit error when they contain a discharged call, because the self-hosted resolver does not yet carry the rule.
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

fn fetchUser(id: String) -> Result<HttpResponse, String>
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

`verify` is deterministic, not random. Regular cases run exactly as written. `verify ... law ...` expands the cartesian product of explicit `given` domains, capped at `10_000` cases — a project that means to go further says so in `aver.toml`, with `[verify] max-cases` for the whole project or `max-cases` in a `[[verify.costly]]` entry for the blocks of one function.

Oracle laws cover classified effectful functions:

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

Inside any cases-form `verify <fn>` block, `given` can bind a capability operation or classified effect to one or more Aver stub functions for those explicit runtime cases. A pure capability stub has the operation's contract signature unchanged; an effectful/generative stub uses the Oracle shape with leading `BranchPath` and call index. In `verify <fn> law <name>`, proof export can additionally quantify over the oracle itself. Add `trace` when you want `.result` and `.trace.*` assertions over collected classified effect emissions.

A plain case may call a function with a non-empty effect declaration as long as
that concrete execution never reaches an effectful operation, or every reached
operation has an exact `given` stub. An unstubbed reached effect aborts before
host dispatch and points to `verify <fn> trace` or record/replay. Aver does not
infer path reachability from the function-wide effect list, and plain verify is
not a real-world smoke-test mode.

Effects outside Oracle's classified set still belong in record/replay, especially ambient state, persistent protocol sessions, terminal modes, and server callbacks. See [oracle.md](oracle.md) for the supported effect set, stub signatures, and trace API.

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
Top-level functions are still first-class values, so higher-order builtins such as `HttpServer.listenWith(port, context, handle)` work without introducing lambda syntax or hidden captures.
There is no lambda syntax. List processing is typically written with recursion and pattern matching rather than callback-based helpers.

This means `Fn(...) -> ...` is a real type, but a function value may appear **only as a function parameter** — i.e. a named function (or builtin / constructor) passed directly in call-argument position, exactly as `HttpServer.listenWith(port, context, handle)` does. A `Fn(...)` type used as a function's **return type**, a **record or variant field**, a **collection or tuple element**, or nested inside another `Fn`, and binding a function value to a local (`g = double`) — are all rejected at type-check time. Function values therefore never escape callback-argument position, so the concrete callee at every call — and with it the set of effects it can perform — stays statically knowable, which is what the effect system, the Oracle, and `aver verify` rely on. If you need to select between functions dynamically, branch at the call site or model the choice as a sum type and `match` on it.

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

## Tail-call optimization

Self and mutual tail recursion is optimized automatically. A transform pass after parsing rewrites tail-position calls into a trampoline — no stack growth for recursive functions in tail position. Tail position = last expression in function body, or each arm body in a `match` at tail position.

This is intentionally narrower than “all recursion”. Non-tail recursion can still be expensive on large inputs, so `aver check` warns when a recursive function still has non-tail recursive callsites after TCO. In practice, long linear traversals are best written in accumulator style when scale matters.

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
`host-bound` when an embedder or Component Model host must install a provider, or
`unsupported(reason)` with a stable architectural reason such as
`host-import-adapter-not-generated` or
`wit-boundary-type-unsupported`. A WIT-lowerable custom contract is
`host-bound[component-import-required]` on wasip2. The manifest lists the full
declared operation set separately from operations used by the program; unused
contracts remain visible but never block compilation. `--json` emits the
versioned machine-readable form, including the exact offending operation,
parameter/result position, and Aver type when WIT lowering is unavailable.

Consequently `error[capability-provider-missing]` is reserved for a target that
can accept a provider but has no live binding. Artifact targets without an
adapter report `error[capability-target-unsupported]` instead, including the
target, capability, required operations, contract/model hashes, and reason.

The registry is shared by the main VM and every `!` / `?!` child, so all branches see the same provider instance and resource store. Recording adds a sorted capability provenance table with `contract_hash`, `model_hash`, provider identity, and implementation fingerprint. `recorded` and `suppressed` replay consume without calling a provider; `reissued` consumes the event and calls live; pure operations call live without emitting an event. Live pure/reissued replay requires the same identity and fingerprint. Provider fingerprints are audit metadata supplied by the host, not theorem hashes; the runtime can expose drift, but it cannot stop a dishonest host from reusing an old fingerprint for changed code.

Custom bindings have two host-bound routes. A Rust embedder can install one typed
in-process provider binding unchanged in the VM or a generated Rust artifact. A
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
Bare wasm-gc still rejects arbitrary custom capabilities. Standard `Time` is a
provided exception: its canonical source is shipped at
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
