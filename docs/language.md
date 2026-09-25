# Aver — Language Guide

This guide covers the surface language: syntax, semantics, modules, and what the language leaves out on purpose.

For constructor-specific rules, see [constructors.md](constructors.md).

For namespaces, services, and standard library APIs, see [services.md](services.md).

For Oracle laws and trace assertions over classified effects, see [oracle.md](oracle.md).

## Types

Primitive: `Int`, `Float`, `String`, `Bool`, `Unit`
Compound: `Result<T, E>`, `Option<T>`, `List<T>`, `Vector<T>`, `Map<K, V>` (`K` must be a type that orders, see [Map literals](#map-literals)), `(A, B, ...)`, `Fn(A) -> B`, `Fn(A) -> B ! [Effect]`

Each primitive has exactly one spelling. The string type is written `String` and is never abbreviated.

There is no dedicated `Set` type; use `Map<T, Unit>` (see [Sets](#sets) below).
User-defined sum types: `type Shape` → `Shape.Circle(Float)`, `Shape.Rect(Float, Float)`
User-defined product types: `record User` → `User(name = "Alice", age = 30)`, `u.name`

Declare a type with its name on its own line and the members indented beneath it: one variant per line for a sum type, one `field: Type` per line for a record.

```aver
type Shape           // sum type
    Circle(Float)
    Rect(Float, Float)
    Point            // zero-arg variant — a bare singleton (Shape.Point)

record User          // product type
    name: String
    age: Int
```

`Unit` means "no meaningful value". It is close to `void`, but it is a real type, and diagnostics print its value as `()`. Effectful functions such as `Console.print` usually return `Unit`.

## Bindings

All bindings are immutable. There are no `val`/`var` keywords; writing one is a parse error.

The leading `__` namespace is reserved for the compiler. User-written names for modules, types, variants, fields, functions, operations, resources, parameters, bindings, match-pattern binders and decisions cannot begin with two underscores. Double underscores elsewhere in a name are still legal (`walk__cursor` is a valid name). The compiler defines some names in that namespace, for example the `__loopStart` / `__LoopOutcome` protocol of a [yielding function](#yielding-functions). Those are ordinary functions and types of the module. User code calls them, annotates with them, and matches on their constructors like anything it wrote itself.

```aver
name = "Alice"
age: Int = 30
xs: List<Int> = []
```

A type annotation is optional and gives the type checker a hint. When the annotation and inference are compatible, the annotation wins. Binding an empty list literal without an annotation (`x = []`) is a type error.

Every name means one thing in its scope. Binding the same name twice in one scope is a type error, and so is shadowing. A binder (a function parameter, a statement binding, or a match-pattern binding) may not reuse any name already visible at that point, including a top-level function of its own module and the name of the enclosing function. The error names both sides and where the shadowed one is defined, and the fix is one rename. Sibling match arms may bind the same name, since neither is in the other's scope. Names from other modules are always qualified as `Module.fn`, so nothing outside the file can collide. In `aver repl` the session is the scope. The rule reads everything entered so far together with the new entry, so a binder may not reuse the name of a function defined in an earlier entry. A refused entry is not added to the session, and `:clear` starts a fresh one.

## Operators

Arithmetic: `+`, `-`, `*`. Both operands must have the same type (`Int+Int`, `Float+Float`, `String+String`). There is no implicit promotion; convert with `Float.fromInt` / `Int.fromFloat`. The `/` operator is **Float-only**, and integer `/` is a type error. For integers use `Int.div(a, b) : Result<Int, String>` (Euclidean; `b == 0` gives `Result.Err`) and `Int.mod(a, b) : Result<Int, String>`. There is no integer `%`. `Int` is arbitrary-precision (ℤ), so it never overflows or wraps around.
Bit-level operations are functions in the `Bits` namespace rather than operators. `Bits.and`, `Bits.or`, `Bits.xor` and `Bits.not` are `Int -> Int` under infinite two's complement (`Bits.not(x) == -x - 1`, `Bits.and(-1, x) == x`). `Bits.shiftLeft(x, n)`, `Bits.shiftRight(x, n)` and `Bits.low(x, width)` compute `x * 2^n`, `floor(x / 2^n)` and `x mod 2^width`. Their `Result<Int, String>` rejects negative counts. The materialization cap applies only when a result may grow (`shiftLeft`, and `low` for negative inputs), never to `shiftRight`, which only shrinks. A syntactic bounded non-negative literal discharges `shiftLeft`/`low`, and any non-negative literal discharges `shiftRight`. `Bits` is a namespace, not a type. Nothing here is a machine word, and `Int` still never overflows or wraps. A width is always asked for explicitly through `Bits.low` and is never implied. See [docs/services.md](services.md#bits-namespace).

Literal-divisor discharge: when the divisor of `Int.div` / `Int.mod` is a syntactic nonzero integer literal (`Int.div(x, 2)`, `Int.mod(x, -3)`), the call cannot fail. It types as plain `Int`, and every backend emits the division directly, with no `Result` and no unwrapping. The boundary is exactly "a syntactic integer literal other than `0`, optionally under one unary minus". A `0` literal, an identifier, a named constant, or a constant expression like `8 + 8` all keep the `Result<Int, String>` type unchanged. Parentheses make no difference here, because the parser erases them around a single expression. `(16)`, `(-16)` and `-(16)` are the same syntax tree as `16` and `-16`, so all three discharge. `(0)` is still zero and `(k)` is still an identifier, and both keep the `Result` type. This typing rule applies to these two functions only. It is not a general constant-propagation or refinement mechanism.
Literal smart-constructor discharge: the same idea extends to a validating smart constructor over a `List<Int>` carrier, which is the shape `stdlib/bytes.av` uses. When the argument is a syntactic list of integer literals and every element lies inside the interval the refinement itself proves, the call cannot reach its `Result.Err` branch. It then types as the refined type and constructs the value directly: `Bytes.fromList([0, 10, 255]) : Bytes`, with no `?` and no `match`. The empty list `Bytes.fromList([])` discharges too. The boundary is narrow and entirely syntactic on the argument side. There must be exactly one argument, it must be a list literal written out at the call site, and every element must be a plain integer literal with at most one unary minus. What decides is the function the call resolves to, not how the call is spelled. `Bytes.fromList(...)` from outside and a bare `fromList(...)` inside the defining module both reach the constructor and both discharge. A module that declares its own `fromList` shadows the imported one as usual, so that call means the local function and is not discharged at all. Everything else keeps `Result<Bytes, String>` unchanged: an identifier (`Bytes.fromList(values)`), a computed list (`Bytes.fromList(List.concat(a, b))`), a computed element (`Bytes.fromList([n * 2])`), an out-of-range literal (`Bytes.fromList([65, 256])`), a negative one (`Bytes.fromList([-1])`), or a literal beyond `i64`. The bound is never hardcoded. It is read off the refinement's own validating predicate, so a user-defined refinement with a different range discharges against that range, and a record with no smart constructor never discharges. Programs run under `--self-host` are refused with an explicit error when they contain a discharged call, because the self-hosted resolver does not carry the rule yet.
Literal effect-contract discharge applies the same rule to `Random.int` and `Time.sleep`. Proven-valid literal arguments remove the `Result` wrapper, and the effect still executes. The backend unwrap is private and fail-closed: if a provider or Oracle stub returns `Err` despite those proven arguments, execution faults as a contract violation. The compiler never calls `Result.withDefault` and never invents a random or sleep result.
Unary minus negates a numeric expression: `-n` (the same as `0 - n`). Numeric literals may be written negative (`-3`, `-1.5`).
Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`.
Error propagation: `expr?` unwraps `Result.Ok` and propagates `Result.Err` as a `RuntimeError`.
Independent products: `(a, b)!` is the product of independent computations. `(a, b)?!` is the same with Result unwrapping: all must succeed, or the first error propagates. Elements cannot reference each other, so independence is structural. Products compose recursively for fan-out parallelism. See [independence.md](independence.md).

## String interpolation

Expressions inside `{}` are evaluated at runtime:

```aver
greeting = "Hello, {name}! You are {age} years old."
```

Interpolation renders primitives only. An embedded expression must be an `Int`, a `Float`, a `Bool` or a `String`. Anything else (a list, a record, a tuple, a `Map`, an `Option`/`Result`, a `Vector`, a refinement or other named type) is a type error. An interpolation site is a display site, and Aver requires every conversion to `String` to be named in the source. There is no built-in renderer for compound values and none is planned. Write a function that returns `String` and interpolate its result (`"cart: {cartLine(item)}"`), or convert at the call site with an explicit conversion such as `String.fromInt(n)`. The same rule makes `Console.print(list)` a type error; the interpolated form is sugar over the same display.

An embed whose type inference never pinned down is rejected too, with a diagnostic saying the type could not be determined. This happens when the value comes from a generic that is still open: matching on a bare `Option.None` or a bare `[]` binds the arm's variable to a type nothing in the program fixes. Give the subject a concrete type (`match someOption` where `someOption: Option<Int>`). The embed then becomes an ordinary primitive or an ordinary compound, and the usual answer applies to each.

## Constructors

An UpperCamel callee is a constructor, and a lowerCamel callee is a function call. Records take named arguments (`User(name = "A", age = 1)`). Variants take positional arguments (`Shape.Circle(3.14)`). Zero-argument constructors are bare singletons (`Option.None`, `Shape.Point`).

All constructors are namespaced. There is no bare `Ok`/`Err`/`Some`/`None`:

```aver
Result.Ok(42)
Result.Err("not found")
Option.Some("hello")
Option.None
```

## Match expressions

`match` is the only branching construct; there is no `if`/`else`. The patterns:

```aver
match value
    42 -> "exact"                          // Int literal
    "verack" -> "known command"            // String literal
    _ -> "anything"                        // wildcard
    x -> "bound to {x}"                    // identifier binding
    [] -> "empty list"                     // empty list
    [h, ..t] -> "head {h}, {List.len(t)} more"  // list cons
    [a, b] -> "exactly two"                // fixed-length list
    [a, b, ..rest] -> "at least two"       // leading elements + rest
    Result.Ok(v) -> "success: {v}"         // constructor
    Result.Err(e) -> "error: {e}"
    Shape.Circle(r) -> "circle r={r}"
    Shape.Point -> "point"
    (a, b) -> "pair: {a}, {b}"             // tuple destructuring
    ((x, y), z) -> "nested: {x}"           // nested tuple
    Option.Some(0) -> "zero"               // literal inside a constructor
    Result.Ok(Option.Some(v)) -> "{v}"     // constructor inside a constructor
```

Constructor patterns are always qualified (`Result.Ok`, `Option.None`, `Shape.Circle`). Records cannot be destructured positionally in a pattern. Bind the whole record and use field access (`user.name`, `user.age`).

A match may nest inside a match arm. The arm body must follow `->` on the same line, so move a complex expression into a named function.

### Nested patterns

Every field of a constructor pattern and every element of a list or tuple pattern is itself a pattern, at any depth. A literal, a constructor, a tuple or a list pattern may stand where a name may:

```aver
fn describe(o: Option<Int>) -> String
    ? "Zero and one get words; other values are printed."
    match o
        Option.Some(0) -> "zero"
        Option.Some(1) -> "one"
        Option.Some(n) -> "{n}"
        Option.None -> "nothing"

fn area(s: Shape) -> Int
    ? "A rectangle with a zero side is empty."
    match s
        Shape.Rect(0, _) -> 0
        Shape.Rect(_, 0) -> 0
        Shape.Rect(w, h) -> w * h
        Shape.Circle(r) -> 3 * r * r
        Shape.Point -> 0
```

List patterns match by length. `[]` is the empty list, `[a]` and `[a, b]` are lists of exactly one and two elements, and `[a, b, ..rest]` is a list of at least two, binding the remaining list to `rest` (`..rest` comes last; `.._` ignores it). `[..all]` matches any list. The elements are patterns too:

```aver
fn firstPresent(xs: List<Option<Int>>) -> Int
    ? "The first present value, or zero."
    match xs
        [Option.Some(x), ..rest] -> x
        [Option.None, ..rest] -> firstPresent(rest)
        [] -> 0
```

Arms are still tried top to bottom. Exhaustiveness counts every case: a literal never covers its constructor, so `Option.Some(0)` needs an `Option.Some(n)` or `Option.Some(_)` arm after it, and the checker names the missing case (`Non-exhaustive match: missing pattern Option.Some(_)`, `missing pattern [_, _, _, .._]` when a list of three or more has no arm). An arm that no value can reach is an error, also when it is covered only by several earlier arms together, as `Option.Some(_)` is after `Option.Some(true)` and `Option.Some(false)`.

The compiler turns a match with nested patterns into nested ordinary matches right after checking it, so every backend and the Lean export read the same program. Such a match inside a `yield` function is not supported yet; move it into a helper function.

### Literal patterns

An arm may be a literal instead of a binding. It fires when the subject equals the literal. This is how you dispatch on a command name or a tag byte. There is no `else if`, and there is no need to spread the decision over a chain of single-purpose helper functions:

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

`Int`, `String`, `Float` and `Bool` literals are all valid patterns. Only a `Bool` match can be exhausted by listing (`true` and `false`), so it is the only one that needs no catch-all. An `Int`, `String` or `Float` match must end in a wildcard `_` or an identifier arm, or the checker rejects it with `Non-exhaustive match: missing catch-all (_) pattern`. A repeated literal is rejected too. The later arm could never fire, and the error names the line that already covers it.

Three things that look like literal patterns are parse errors:

- a negative number. `-1 -> …` does not parse, because the `-` is a separate token and a pattern is not an expression. Branch on a comparison instead (`match n < 0` with `true ->` / `false ->`), or normalize the subject before the match.
- an integer beyond 64 bits, even though `Int` itself is arbitrary-precision. The error points at the replacement: `match n == 1267650600228229401496703205376`.
- an interpolated string. `"{x}" -> …` is rejected, because a pattern is a constant. Compare with `==` when the expected value is computed.

`Float` literal patterns compare exactly, so `0.1 + 0.2` does not match a `0.3` arm. Use them only for sentinels you produced yourself, and otherwise branch on a comparison.

## Record update

`update` creates a new record with the given fields overridden and every other field kept:

```aver
updated = User.update(u, age = 31)
```

## Map literals

```aver
m = {"key" => value, "other" => 42}
```

`=>` is required inside map literals; `:` stays type-only.

A map iterates its entries sorted by key. That holds when you run a program, in a compiled binary, and in the exported proof model, so the key type must be one that all of them order the same way. Most types qualify: `Int` numerically, `String` by codepoint, `Bool` false-first, a list or a `Bytes` lexicographically, a tuple componentwise, a record by its FIELD NAMES, a variant by its CONSTRUCTOR NAME and then its payload. Records are ordered by field name rather than by declaration order on purpose. Declaration order is not observable anywhere else, because a record is built and read by name. Ordering by it would mean that swapping two fields changes how every map on that key iterates.

`Float` cannot be a map key. A NaN has no place in the finite range, and neither a compiled binary nor the proof model can state an order the other agrees with. A `Map` or a `Vector` cannot be a key either, because neither has an order of its own. The rule reaches through your own types, so a record with a `Float` field cannot key a map, and the error names the field it found. Float is still legal as a map *value*.

## Effects

Effects are exact method names:

```aver
fn main() -> Unit
    ! [Console.print, Disk.readText]
    Console.print("starting")
    _ = Disk.readText("data.txt")
```

Both granular and namespace shorthand declarations are supported. `! [Disk.readText]` declares a single effect, and the namespace shorthand `! [Disk]` covers all `Disk.*` effects. `aver check` suggests narrowing when a shorthand could be more specific. `effects X = [...]` aliases are no longer supported.

Entries are separated by commas, and the comma is required. `! [Console.error Console.print]` is a parse error that names the effect it stopped after; it is not read as two effects. The list may span several lines, and a trailing comma is allowed.

## Command-line arguments

Programs read CLI arguments through the `Args` service:

```aver
fn main() -> Unit
    ! [Args.get, Console.print]
    args = Args.get()
    Console.print(args)
```

Run with: `aver run file.av -- arg1 arg2 arg3`

Arguments after `--` arrive as a `List<String>`. Without `--`, the list is empty. `Args.get()` requires `! [Args.get]`, so argument access shows in the signature like any other effect.

`aver run` starts from `main` by default. To record or run another top-level function, pass `-e '<call>'` (repeat it for a batch) or `--input-file PATH`: `aver run file.av -e 'load("PL")' --record recordings/`. In 0.10.1 the arguments are limited to literals, so wrap complex inputs in a helper function.

## Functions

```aver
fn add(a: Int, b: Int) -> Int
    a + b

fn fetchUser(id: String) -> Result<Http.Response, String>
    ? "Fetches a user record from an API."
    ! [Http.get]
    Http.get("https://api.example.com/users/{id}")
```

- `? "..."`: an optional prose description, which is part of the signature
- deeper-indented string lines continue the same description:
  ```aver
  ? "Starts the CLI."
    "Dispatches one argv command."
  ```
- `aver check` warns when a function other than `main` has no description
- `! [Effect]`: an optional effect declaration, enforced statically and at runtime
- effects can be declared per method: `Http.get`, `Disk.readText`, `Console.print`
- top-level functions are values and can be passed where `Fn(...)` is expected
- `main` often returns `Unit`, but `Result<Unit, String>` is also common; `aver run` treats a `Result.Err(...)` returned from `main` as a runtime failure
- function bodies use indentation
- the last expression in a function body is the return value

## Verify blocks

Regular `verify` blocks live directly under the function they cover:

```aver
verify add
    add(0, 0) => 0
    add(2, 3) => 5
```

A law-style verify block is a finite universal check over explicit domains:

```aver
verify add law commutative
    given a: Int = -2..2
    given b: Int = [-1, 0, 1]
    add(a, b) => add(b, a)
```

If the identifier after `law` names an existing pure function and the law body compares `foo(args)` with `fooSpec(args)`, Aver treats it as a spec law. `verify fib law fibSpec` is the preferred way to say "fib should match fibSpec".

This style is deliberate. An Aver author should usually write a simple spec function and a law that relates the implementation to it, rather than writing proof-oriented invariants directly in surface code.

When a law needs an explanation, optional `because` lines state ordered facts in ordinary, pure Aver:

```aver
verify identity law positive
    given value: Int = [-3, 0, 1, 7]
    when value > 0
    because value >= 1
    because value + 1 > 1
    using []
    identity(value) > 0 holds
```

Each explanation must return `Bool`. It may call a normal function whose `match` branches describe the argument. `verify` and `verify --hostile` check every explanation under the original `when`, in addition to checking the claim. A false explanation fails even when the claim is true. An explanation never restricts the law's domain.

A recursive explanation can guide induction when it has a checked structural descent on a list, or a native, checked integer countdown (including floor division by a positive literal). The recursive step must establish the original guard and any earlier reason premises for its own arguments. This is ordinary function recursion; there is no separate induction syntax.

`because` entries are ordered, so the proof of a later fact can use the earlier ones. The optional `using [function.law, Module.function.law]` list selects an unordered set of lemmas. Leaving it out keeps automatic selection, and `using []` selects none. Ordinary local bindings may appear between these clauses. They remain expression shortcuts and are not assertions. The first proof implementation targets Lean; see [law explanations](lean.md#law-explanations-in-aver) for the obligations, diagnostics and limits.

`verify` is deterministic, not random. Regular cases run exactly as written. `verify ... law ...` expands the cartesian product of the explicit `given` domains, capped at `10_000` cases. A project that needs more says so in `aver.toml`: `[verify] max-cases` for the whole project, or `max-cases` in a `[[verify.costly]]` entry for the blocks of one function.

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

In any cases-form `verify <fn>` block, `given` can bind a capability operation or classified effect to one or more Aver stub functions for those explicit runtime cases. A pure capability stub keeps the operation's contract signature unchanged. An effectful or generative stub uses the Oracle shape, with a leading `BranchPath` and call index. The call index counts the calls of that one operation, and `docs/oracle.md` lists the shapes where an exported proof numbers a call differently from the run. In `verify <fn> law <name>`, proof export can also quantify over the oracle itself. Add `trace` when you want `.result` and `.trace.*` assertions over the collected classified effect emissions.

A plain case may call a function with a non-empty effect declaration, as long as that concrete execution never reaches an effectful operation, or every operation it reaches has an exact `given` stub. A reached effect with no stub aborts before host dispatch and points to `verify <fn> trace` or record/replay. Aver does not infer path reachability from the function-wide effect list. Plain verify is not a way to smoke-test against the real world.

Effects outside Oracle's classified set still belong in record/replay, in particular ambient state, persistent protocol sessions, terminal modes and server loops. See [oracle.md](oracle.md) for the supported effect set, stub signatures and trace API.

`aver check` expects every pure, non-trivial function other than `main` to have a `verify` block next to it.

## Decision blocks

A `decision` block is top-level syntax for recording design rationale:

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

`chosen`, `rejected` and `impacts` may reference validated symbols or quoted semantic labels. `aver context ... --decisions-only` exports decisions.

## No closures

All user-defined functions are top-level. At call time a function sees globals and its own parameters; nothing is captured when it is defined. Top-level functions can still be passed as values, so higher-order helpers such as `HttpServer.listen(port, handle)` work without lambda syntax or hidden captures. There is no lambda syntax. List processing is usually written with recursion and pattern matching instead of callback helpers.

So `Fn(...) -> ...` is a real type, but a function value may appear **only as a function parameter**: a named function (or a builtin or constructor) passed directly in argument position, the way `HttpServer.listen(port, handle)` does. The type checker rejects a `Fn(...)` type used as a function's **return type**, as a **record or variant field**, as a **collection or tuple element**, or nested inside another `Fn`. It also rejects binding a function value to a local (`g = double`). Function values therefore never leave callback-argument position. The concrete callee at every call stays statically known, and so does the set of effects it can perform. The effect system, the Oracle and `aver verify` depend on that. To choose between functions at runtime, branch at the call site, or model the choice as a sum type and `match` on it.

A callback effect list of `! [_]` means "forward the effects of the concrete named callback". It is resolved statically at the helper's call site. It is not an ambient wildcard or a hidden capability grant.

```aver
fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int
    f(f(x))

fn inc(n: Int) -> Int
    n + 1
```

Most Aver application code stays first-order and explicit. Use function parameters when they make an API cleaner. They are not meant as the default way to abstract.

## Sets

Aver has no dedicated `Set` type. A set is written as `Map<T, Unit>`, a map whose values carry no information, and every `Map.*` operation works on it:

```aver
seen: Map<String, Unit> = {}
seen2 = Map.set(seen, "alice", Unit)
Map.has(seen2, "alice")   // true
Map.len(seen2)            // 1
seen3 = Map.remove(seen2, "alice")
```

`Map.set(s, k, Unit)` adds an element, `Map.has(s, k)` checks membership, `Map.remove(s, k)` removes an element, and `Map.len(s)` returns the cardinality. Map literals with `Unit` values work as set literals: `{"alice" => Unit, "bob" => Unit}`.

Lean has no set type the generated project can reach, so in the proof export a set stays an ordinary map: `Map<T, Unit>` becomes `List (T × Unit)`, and `Map.set(s, k, Unit)` becomes `AverMap.set s k ()`.

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

`Vector.new(size, fill)` is fallible when the size is dynamic. A syntactic literal inside the portable `0..=1_048_576` element budget, as above, discharges directly to `Vector<T>`. Negative, oversized or computed sizes keep `Result<Vector<T>, String>`. The budget counts elements because Aver has no backend-independent byte size for an arbitrary `T`.

## Tail-call optimization

Self and mutual tail recursion are optimized automatically. A transform pass after parsing rewrites calls in tail position into a trampoline, so recursive functions in tail position do not grow the stack. Tail position is the last expression in a function body, or each arm body of a `match` that is itself in tail position.

This deliberately covers less than all recursion. Non-tail recursion can still be expensive on large inputs, so `aver check` warns when a recursive function still has non-tail recursive callsites after TCO. When scale matters, write long linear traversals in accumulator style.

## Yielding functions

A function whose effect list names `yield` hands control back instead of performing the operations this program answers itself. The program answers a capability when one of its modules says so in its header with `answers [<Capability>]`; see [Capabilities the program answers](services.md#capabilities-the-program-answers--answers-runwake-and-the-generated-loop). Every operation of such a capability is a request. Everything else the function calls runs where it is written. The function is written in direct style (read, then the next step), but it never runs as written. The compiler cuts it at every stop and turns it into plain data and pure functions, and a coordinator performs the operations and feeds the answers back. The coordinator is generated (see [The coordinator](#the-coordinator)), or a program writes its own. `yield` is an ordinary effect: it appears in the function's `! [...]` and must be covered by the module's `effects [...]`. A bare name in an effect list (a function's `! [...]` or a module's `effects [...]`) must be `yield`, the forwarding marker `_`, or a standard capability namespace such as `Console`. A program-defined capability is named by its operations, never bare. Anything else, including a misspelled `yeild` or a bare program-defined capability name, is an error naming the unknown or disallowed effect. A yielding function is called only through its generated entry points. `__<fn>Start` and the answer functions are pure, so a coordinator that calls them declares no `yield`.

```aver
fn loop(id: Int, done: Int) -> Int
    ? "Claims handles for id until the pool answers None, summing them into done."
    ! [Pool.claim, yield]
    r = Pool.claim(id)
    match r
        Option.None -> done
        Option.Some(h) -> loop(id, done + h)
```

Inside a yielding function, a call to an operation of an answered capability is a stop (a request), and the self tail call is a stop of kind `Yield`. Everything else runs inline. That includes an operation of a capability nobody answers: it runs inside the turn, where it is written, and the generated function holding it declares it in its own `! [...]`. A function that declares `yield` and never stops (it calls no answered operation and does not tail-call itself) is an error that names both repairs: mark the capability, or drop `yield`. The reverse case is `error[intercept-outside-yield]`. A function without `yield` that performs an operation of an answered capability has made a request nobody will answer, and the message names the answer module's own function to call instead. A program that answers a capability runs on every backend: the bytecode VM, the Rust backend, `aver run --wasm-gc` and `aver run --wasip2`. The lowering leaves behind only state types and pure answer functions, and the job handle `Work.Job` that an answer's `Run.Wake` can reach has a representation on each backend. A job kind also runs on the two wasm targets. On wasm-gc it runs on a host worker beside the turn (the native runner and Wasmtime packs schedule worker instances, and the JavaScript adapter uses Workers), with the same queue at the job limit as the VM. On `wasip2` it runs inline at `begin`, because a component is single-threaded. Both targets bind the `Wait` and `Work` contracts, so `Wait.poll` and `Work.cancel` run there too. See "On wasm-gc and wasip2" under "Jobs" in `docs/services.md` for what a program reads differently there. For `loop`, the compiler generates the following, in the same module and in the reserved `__` namespace:

- `__LoopClaimState`: one sum type per request kind, with one variant per stop of that kind. A variant holds exactly the variables the rest of that path still reads (`AwaitR(Int, Int)` for `id` and `done`). A stop bound to a name is `Await<Name>`; an unbound stop is `Await<n>`, with its ordinal in the function.
- `__LoopYieldState`: the state of the tail call, which is its argument tuple (`Await2(Int, Int)`).
- `__LoopRequest`: one constructor per kind, carrying the operation's arguments and the state of that kind: `Claim(Int, __LoopClaimState) | Yield(__LoopYieldState)`.
- `__LoopOutcome`: `Done(Int) | Waiting(__LoopRequest)`.
- `__loopStart(id: Int, done: Int) -> __LoopOutcome`: runs to the first stop and carries the original `? "..."` description.
- `__loopAnswerClaim(__state: __LoopClaimState, __answer: Option<Int>) -> __LoopOutcome`: matches the state variant and runs to the next stop or to `Done`. The answer type is the operation's result type, so pairing a state with the answer of another kind is a type error. An operation whose result is `Unit` has no answer to pass, so its answer function takes only the state (`__loopAnswerPrint(__state)`).
- `__loopAnswerYield(__state: __LoopYieldState) -> __LoopOutcome`: re-enters `__loopStart` with the carried arguments.

These names are compiler-defined and callable. The original `loop` is removed after lowering, and the coordinator answers the requests instead:

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

Stops may sit anywhere the function runs unconditionally (in a binding, as a match subject, inside an argument) and inside `match` arms. A request in tail position, as the last expression of the body or as the leaf of a `match` arm, is a stop like any other, and its answer is what the function returns. The same operation may stop several times in one body, and `?` after a request works (`Err` leaves through `Done`). When code follows a stop that sits in a `match` arm of a non-tail statement, the rest of the path becomes a generated continuation function (`__loopJoin1`, `__loopAfterAwaitR`) that the arms call. The generated items are ordinary types and pure functions. `aver verify` runs them, every backend compiles them, and `aver proof` exports them to Lean like anything else, so the coordinator's laws can reason about the protocol.

A module that exposes a yielding function exposes its protocol in its place. `exposes [loop]` becomes the generated names, and an importer writes `Looper.__loopStart(...)`, matches `Looper.__LoopOutcome` and answers with `Looper.__loopAnswerClaim(...)`. A yielding importer may also write `Looper.loop(...)`: the compiler keeps the exported source signature and nests the library's protocol into the caller. Ordinary functions use the explicit protocol entry points. Default exports follow the same rule, and private helpers stay private.

Two diagnostics guard the shape:

- calling a yielding function directly from a function that does not yield (a plain function, including one in a dependent module) is a type error: `'loop' yields; call '__loopStart(...)' and answer its requests`;
- a yielding function that calls *itself* outside tail position is a type error with the recipe `pass what comes next as data, or make it a tail call`. A process can nest another yielding function but not itself, because its own state would have to hold a copy of itself.

### Testing a process with request stubs

A local cases-form `verify process` may call that process directly if it supplies an exact `given` for every request operation in the process protocol:

```aver
verify pair
    given answer: Pool.claim = [numbered]
    pair(2) => 15
    pair(7) => 25
```

The full example is `tests/fixtures/yield_verify_stubs/`. Its `numbered` stub has the signature `(BranchPath, Int, Int) -> Option<Int>`. The second argument is the number of `Pool.claim` requests this branch has already made, and the third is the requested peer. The verifier starts the generated protocol and answers each request with the selected stub. This exercises the lowered continuation, including local and imported nested helpers, self yields and `?` propagation. Stubs return operation results, not `Now`/`Later` replies, and the live answer module is not consulted.

Each case starts with fresh Oracle coordinates. Every operation counts its own calls, so a request is numbered among the calls of the same kind. `Pool.claim` and `Pool.finish` each start at 0, and an in-place `Time.unixMs()` between two `Pool.claim` requests leaves the second request at index 1. A self yield consumes no answer and no index. An in-place effect still needs its own stub if it is reached. Existing step limits and `[[verify.costly]]` settings use the source process name.

This first version runs in `aver verify` on the VM. Not supported yet: direct process laws, `trace` blocks, direct calls to an imported process from a verify block, WASM request stubs, and proof export of these cases. State proof laws over the generated protocol. Testing a process's responses does not test coordinator scheduling. The separate `tests/fixtures/run_schedule_cases/` scenarios cover service order, grouping, premature job readiness, and stale notifications after completion or cancellation. They enumerate explicit schedules; `--hostile` does not generate all coordinator interleavings automatically.

### Helpers and nested state

A process does not have to be one function. A yielding function may call another yielding function of the same module, or an exposed yielding function of an explicit dependency. The compiler puts the callee's machine inside the caller's, so you do not fold the two together by hand. The coordinator stays the same. It seats and serves the caller's protocol, and a request the helper makes reaches it as a request of the caller.

A **tail call** enters the helper's protocol. A segment of `f` that ends in `g(args)` stops with a `Yield` request carrying what `g` is entered with. The caller has nothing left to do, so none of it is kept. Answering that request calls `__gStart(args)`.

A **non-tail call** nests the helper's state inside the caller's. Take `x = g(args)` with more work after it. For every kind `g` waits on, the caller's state sum gains one variant per call site, `In<G>At<N>(<the helper's own state>, <the caller's live variables>)`. The caller's request sum gains the kinds `g` waits on that it does not already have. The caller's answer function for such a kind passes the answer down to `__gAnswer<Kind>` and routes what comes back. A `Done(v)` binds `x = v` and continues the caller's segment. Another request leaves again as a request of the caller, with the new nested state. A nested call is not a stop by itself: a helper that waits on nothing runs to its result inside the caller's own segment.

Here is the whole generated shape, for `walk` calling `fetch`:

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

The helper's own recursion stays inside the helper's machine. When `g` loops on itself, that is `g`'s `Yield` request. It leaves as the caller's `Yield` request carrying the nested state and comes back to `__gAnswerYield`. These names are the caller's own `__` types. Nothing new reaches the module's surface, and `aver context`, `AVER_YIELD_DUMP=1` and the pinned generated Aver show them.

The tail-call rule: a self tail call is the `Yield` request, and a tail call to another local or imported yielding function enters that function's protocol. A few constructs are still refused, and the message names each one. Mutual nesting is refused: two yielding functions that call each other, because each one's state would have to hold the other's. The message prints the cycle and two ways to break it. The same holds for a cycle written with tail calls only, where no state is held but each function's requests would have to carry the ones it hands over to. A request, or a call to a yielding helper, inside an independent product `(a, b)!` is refused, because its branches run independently and a request leaves a process one at a time. A function value live across a stop is refused. So is a yielding function passed as a function value, because protocol composition needs a direct named call. The loader rejects module dependency cycles before composition. The refusal of a request inside a product fires less often than it used to, because only an answered operation inside the product is refused.

## The coordinator

Writing the coordinator by hand is tedious work: a slot table, an instance number per request, a wait set, a poll timeout, one dispatch arm per request kind per process, and the turn around all of it. So a program does not write it. An entry module that writes processes gets it generated, in the reserved `__` namespace, in the same pass that generates the protocol. The program writes **processes, answer modules, one line per keyed process, and optionally a `stop` and an `admit` policy**. The generated names stay callable, so a program that wants its own loop over the protocol can still write one.

The worked example is `tests/fixtures/run_all_slice/` (`examples/concurrency/README.md` points at it and explains why it lives there). It has a peer that fetches block bodies, a walk that connects them, an accepting process, a dialling process and a ticker. That is five processes, three answer modules, one job kind and two policies, with no coordinator code written anywhere. `tests/fixtures/run_families/` seats one process per key.

**When the loop is generated.** The loop belongs to the program's entry module. It is generated when the entry writes a process the loop can seat (a yielding function that answers `Unit`) and either has no `main`, in which case the loop's own `main` is generated, or has a `main` that calls `Run.all()`. `Run.all()` runs the loop until it is over and answers `Result<Unit, String>`, so a program with a command line of its own can run the loop from one subcommand:

```aver
fn main() -> Result<Unit, String>
    ? "`node` runs the loop until it is over; anything else prints the usage and runs nothing."
    ! [Args.get, Console.print]
    match List.contains(Args.get(), "node")
        true -> Run.all()
        false -> usage()
```

The effect list of a `main` that calls `Run.all()` is widened by what the loop performs, the way the module's own `effects [...]` is. An entry whose `main` does not call `Run.all()` drives its processes itself, and no loop is generated. A yielding function of a dependency is a library helper: it runs when an entry process calls it, and the loop never seats it. See `tests/fixtures/yield_module_helpers/` for repeated and tail calls through a dependency chain, and `tests/fixtures/run_process_elsewhere/` for an imported helper under the generated loop.

**Processes.** The processes are ordinary yielding functions in direct style, written in the entry module. A process without parameters is seated once, at start-up. A process that takes one parameter is seated once per key, by a declaration in the entry module:

```aver
process member seated by Hub.members
```

`Hub.members` is a pure function of an answer module's state, `(Hub.State) -> List<Int>`, and the process's parameter has the list's element type. The loop reads the list once per turn, after the turn has served its requests, and at start-up:

- a listed key with no instance and not retired is seated, in list order, under the next free slot id;
- an instance whose key has left the list is dropped at that turn boundary: its slot goes, whatever request it waited on is forgotten, and `Run.View.dropped` counts it;
- an instance that returns retires its key, and a retired key is seated again only after it has been absent from the list once.

The same key listed twice seats one instance. Keys are compared as map keys, so a key is any type a map accepts. A request that names a key the answer module no longer knows is answered by that module, never parked: the module answers something like `Gone` and the process ends. The keys come from the module's state, so a module that wants a key reused only after its old instance is gone mints keys from a counter rather than reusing a freed one in the same turn.

A process's own effect list names what its helpers perform, because the program as written calls them. A process that answers anything other than `Unit`, or takes parameters without a seating line, is refused with `error[run-binding]`, as is a seating line whose function is not `(S) -> List<K>` of an answer module.

**Answer modules.** Every answer module declares `fresh()`, the state before anything has happened, because the loop starts it there, and one function per operation over that state (see `docs/services.md`). A module answers `Result.Ok(v)` or `Result.Err(wake)`, where the wake is a `Run.Wake`:

- `Until(items, deadline)` parks on a list of sockets and jobs and an optional deadline in milliseconds. The request is asked again in a turn whose `Wait.poll` reported one of its items, or whose clock reading has reached the deadline, whichever comes first. False readiness is allowed, and the module may answer `Err` again. The generated loop gives every item of every parked request a key of its own in the turn's one wait, so a request may wait on as many sockets and jobs as it needs.
- `Settled(deadline)` parks until this answer module has answered any request with something other than `Settled`, or until the deadline. Each answer module has a version that moves with every answer it gives except a `Settled` one, and a request parked on `Settled` is askable once the version has moved past the one it was parked at. The turn in which that happens waits with a zero timeout. A `Settled` answer never moves the version, so a request cannot wake itself; a module that needs to be asked again whatever happens answers `Until([], Option.Some(0))`, which asks again on the next turn.

A deadline is measured against the clock reading the turn makes once, after its wait and before it serves. At park time the turn turns `ms` into the reading it falls due at, `due = now + ms`, and keeps the `ms` beside it. The request is asked again once the reading has reached `due`, or once the reading has fallen further back than `ms`. Without the second condition, a wall clock that steps backwards would leave the request waiting for a reading that never comes. A negative deadline is due at once.

The answer function is not told *which* half of a wake fired. The module already knows how to find out and the turn does not: it probes the socket again (`Tcp.readNow` answering `None` means still nothing), or reads the clock in place, which an answer module may do.

An `Err` leaves the **request** where it is, with the same instance number and the same arguments, and it **keeps the state the module returned**. An `Err` repeats the original arguments against the module's current state when it is asked again; it never resumes an operation instance. Identity belongs in an argument or a handle.

The worked example of `Until` with a socket and a deadline is the read in the slice's `Sockets` module. On a read's first ask it records the clock reading the read falls due at, so later asks run out the deadline the caller named instead of starting a new one each time. It parks on the socket and on what is left of that deadline at once:

```aver
fn quiet(state: State, key: Int, now: Int, again: Run.Wake) -> Tuple<State, Result<Wire.Heard, Run.Wake>>
    ? "A peer that has said nothing yet, and the wake that would bring this read back: the socket it is listening to and what is left of its deadline at once, whichever comes first. Past the deadline the read is over and answers TimedOut; before it the request parks on that wake."
    match now >= dueOf(state, key)
        true -> (disarmed(state, key), Result.Ok(Wire.Heard.TimedOut))
        false -> (state, Result.Err(again))
```

The worked example of `Settled` is the walk in the same slice: the walk waits for the ledger to move, and a delivery, a take or the pool counting its own asks moves it.

**Policies.** The run ends once nothing is seated, or once a stop was requested. Two optional functions of the entry, found by name, change that:

- `fn stop(view: Run.View) -> Bool` ends the run when it answers `true`. The run still ends once nothing is seated, whatever `stop` says.
- `fn admit(view: Run.View, id: Int) -> Bool` is asked about every askable slot, in slot order, and a slot it refuses is not served in this turn.

`Run.View` and `Run.Pending` are generated, and the program names them through the standard module `Run`:

```
record Run.View
    pending: Map<Int, Run.Pending>
    ready: List<Int>
    askable: List<Int>
    dropped: Int
    stopping: Bool

type Run.Pending
    <Process>(Int, Run.Wake)          for a process without a key
    <Process>(K, Int, Run.Wake)       for a process seated by key
```

`Pending` has one constructor per process, named after it, carrying its key when it has one, the instance number of the request it is waiting on, and the wake it is parked on. `ready` is the slots the turn's one wait reported. `askable` is the slots this turn may ask, in slot order. `dropped` is how many keyed instances the loop has dropped. `stopping` is the stop flag as data. A policy is pure, and `verify` builds sample views with `Run.View(pending = …, ready = …, askable = …, dropped = …, stopping = …)`. The slice's `admit` always admits a peer and holds the walk and the ticker back once the run is stopping, and its priority law is stated over such samples.

**What the compiler generates.** `AVER_YIELD_DUMP=1 aver check main.av --module-root .` prints all of it after the protocol. In outline:

- `__Process`, `__Slot`, `__Run`: the slot table (`Map<Int, __Slot>`), one field per answer module holding its state as an `Option` (it is `None` only while one answer function holds the state), per keyed process the keys it has seated and retired, the version of every answer module, the count of answers that arrived too late, the count of dropped instances, the stop flag, the clock reading and the next free id. A slot carries its instance number, its request, the wake it is parked on, `due` and `ms`, and the answer module and version a `Settled` wake waits on.
- `__start`, `__seat<P>`, `__seatFamilies`, `__seatFamily<P>`: seating at start-up and at every turn boundary.
- `__current`, `__nextInstance`, `__settle<P>`, `__park`, `__bump`: the two invariants of the table. An answer that carries the current instance replaces that process's one slot and raises its number. An answer that carries an older one changes nothing and is counted. An `Err` parks the request where it stands and keeps the state the module returned.
- `__askable`, `__askableSlot`, `__deadlinePassed`, `__view`: the gate that decides which slots this turn may ask, and the view the policies read.
- `__waitPlan`, `__timeout`: the one wait of a turn, with one key per parked item, and its timeout: zero while some request can be asked already, the soonest deadline otherwise, and one second when nothing carries either.
- `__serve`, `__serve<P>`, `__take<Module>`, `__serve<P><Kind>`: the dispatch, one arm per request kind of each process, which hands the answer module's state out of the run, calls the module's own function with it, and settles or parks on what it answered, writing the state it returned back. Handing the state out means the answer function holds the only reference to it, so a Map or Vector in it is updated in place rather than copied on every request, on the Rust backend in particular, where the run is moved from each of these functions to the next rather than borrowed.
- `__turn`, `__serveEach`, `__runAll`, `__all`, `main`: observe the stop flag, wait once, read the clock, serve every askable slot in slot order, seat the families, and repeat until the run is over.
- `__over`, `__cancelWaited`: the end of a run, which cancels every job a parked request is still waiting on.

Each of those carries its own effects rather than the program's. `__seat<P>` performs what that process performs on its way to its first request. `__serve<P><Kind>` performs what the answer module performs plus what the resumed segment performs. A process that touches nothing gets only pure functions from the loop. Only the dispatch, the turn and the loop's entry carry the union.

The loop generates no laws into a program. Its invariants are stated once, as laws over the generated functions of `tests/fixtures/run_schedule_cases/`, which the test suite checks on the VM and on the Lean wall: a late answer changes nothing and is counted, an `Err` keeps the request's instance, the next instance is higher, a deadline that has passed fires and no deadline never does, the wait never exceeds a deadline, and a `Settled` answer moves no version. The source-trace observers a proof law can cite are generated in the entry module only when one of its own laws cites them; a library module keeps generating them for an importer's laws.

**Where it runs.** The loop is ordinary Aver, so it runs wherever the program does: the bytecode VM under `aver run`, a native binary from `aver compile --target rust`, whose generated crate carries the same loop with the wait and the job engine answered by `aver-rt`, the wasm-gc runner and Wasmtime packs, which run jobs on host threads and wake the loop when a job settles, and JavaScript hosts, which use the same job ABI and the generated post-wait step (see [Parallel Work on wasm-gc](wasm-work.md)). In a side-by-side run, do not read anything into the order of two processes' output: requests park on wall-clock deadlines and on jobs, so the turn in which a job lands depends on how long it took. The wasip2 job lowering is still inline and warns that `max-jobs` has no effect: there a job is over before the next expression, `take` never answers `Ok(None)`, a job that never ends blocks the turn, and a job whose body fails stops the component. On wasip2 the generated turn keeps `Run.View.stopping = false`, because WASI 0.2 has no signal subscription, so a run ends through `stop` or once nothing is seated.

A yielding *helper* is not a process and is not held to the process rules. It takes parameters and answers whatever its caller reads, because the process that calls it is what the loop seats.

## Modules

Module imports resolve from a module root (`--module-root`, default: the current working directory).
Each module file must start with `module <Name>` and contain exactly one module declaration.

```aver
module Payments
    intent = "Processes transactions."
    effects [Disk.readText]
    depends [Data.Fibonacci]
    exposes [charge]
```

`effects [...]` declares the module's effect boundary: the union of the effects its functions may perform, in the same granular or namespace-shorthand form as a function-level `! [...]`. It goes after `intent`. `aver check` warns when a module with functions omits it, and a pure module declares `effects []` explicitly.

### Capability modules

A capability module declares host-provided atoms without choosing how a host binds them. For `depends`, visibility and naming it is an ordinary module, but its `operation` declarations have signatures instead of Aver bodies:

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

`semantics` is mandatory and the same for the whole module:

- `pure` operations are total, deterministic functions for proof purposes and carry no effect. They cannot declare `oracle`, `replay`, `hostile` or `unmodelled` fields.
- `effectful` operations are their own effect identities (`Clock.now`). Every operation declares an Oracle dimension (`generative`, `output` or `generativeOutput`) and a replay behavior. Generative results use `recorded`. Output requires a `Unit` result and uses `reissued` or `suppressed`. `snapshot` is reserved for standard-library effects whose read-only behavior Aver audits itself.
- An operation is a first-order provider boundary and not a value. It cannot take or return `Fn`, be assigned, or be passed as a callback. Call it directly, including inside `!` and `?!`. Capability effect declarations must name exact operations, and namespace shorthand is rejected at module and function scope.

For effectful capabilities, `given` and `aver verify --hostile` use the same Oracle stub signatures as built-in effects. A hostile profile belongs to the capability module, must be pure, and receives `BranchPath`, the call index, then the operation arguments. If the operation mints a resource, one unconstrained fresh token appears between the call index and the original arguments, and it is not assumed distinct from any other token. A `given` stub for a pure capability has the operation's ordinary contract signature instead, with no Oracle coordinates. Proof trust headers pin two separate SHA-256 identities. `contract_hash` covers the provider ABI and all reachable boundary types, including the layouts of the dependency types a job kind names. `model_hash` also covers Oracle/replay metadata and the transitive source closure of hostile profiles. Both identities hash canonical `u64be` length-framed descriptors, so field concatenation cannot collide. Provider choice and binding stay outside both hashes and outside the theorem.

`resource Token` inside a capability has no representation, and only its bound provider can mint a value. This is deliberately different from `exposes opaque [T]`, which hides an ordinary represented Aver type while keeping its value semantics. A capability resource has no Aver representation and no identity visible to the language. It may occur at most once in an operation's success payload, directly or through transparent `Result`/`Option` wrappers, and resource consumers must use recorded replay. Runtime handles are tagged by binding instance and canonical type, survive independent-product child VMs, and never expose the provider payload. Capability resources, including represented wrapper types that transitively contain one, deliberately have no display identity, equality, serialization as a host payload, or map-key semantics.

An embedded Rust host installs a VM provider with `aver::provider::ProviderBinding` and `ProviderRegistry`. A generated Rust host installs the same public `aver_rt::provider::ProviderBinding` through the generated library's `install_provider_bindings` entry. Registration pins the exact `contract_hash` and the complete operation set before execution. Providers implement `aver_rt::provider::CapabilityProvider` and exchange only the closed, transport-neutral `ProviderValue` tree. They never see the VM's `NanValue` or the general interpreter `Value`. A returned `ProviderValue::ResultErr` is ordinary Aver data, while `ProviderFault` or a provider panic is a separate boundary failure. Duplicate, incomplete, extra-operation, hash-mismatched and wrong-return-shape bindings fail closed with provider-specific diagnostics.

`aver verify` does not discover or install host packages. A source-local cases-form binding such as `given hash: Hash160.digest = [fixtureHash]` installs that Aver function only for each expanded verify case. Namespaced capabilities use the same full canonical path as calls and diagnostics, for example `given probe: Domain.Crypto.Hash160.digest = [fixtureHash]`. A shortened or misspelled path is a static error and is never ignored as a binding. The alias may go unused in the assertion, and the binding still redirects the dispatch it reaches. It never satisfies the normal `aver run` provider preflight, and it does not test the provider implementation itself.

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

Target support is stated explicitly and never inferred from a missing provider row. `aver capabilities app.av` emits one deterministic row per loaded capability and shipped target (`vm`, `rust`, `wasm-gc`, `wasip2`). A row is `provided`; `host-bound`, when an embedder, JavaScript host or Component Model host must install a provider; or `unsupported(reason)` with a stable architectural reason such as `wit-boundary-type-unsupported`. A custom contract is `host-bound[wasm-gc-import-required]` on raw wasm-gc, and a WIT-lowerable custom contract is `host-bound[component-import-required]` on wasip2. The manifest lists the full declared operation set separately from the operations the program uses. Unused contracts stay visible but never block compilation. `--json` emits the versioned machine-readable form, including the exact offending operation, the parameter or result position, and the Aver type when WIT lowering is unavailable.

So `error[capability-provider-missing]` is reserved for a target that can accept a provider but has no live binding. Artifact targets without an adapter report `error[capability-target-unsupported]` instead, with the target, the capability, the required operations, the contract and model hashes, and the reason.

The main VM and every `!` / `?!` child share the registry, so all branches see the same provider instance and resource store. Recording adds a sorted capability provenance table with `contract_hash`, `model_hash`, provider identity and implementation fingerprint. `recorded` and `suppressed` replay consume the event without calling a provider. `reissued` consumes the event and calls the provider live. Pure operations call live and emit no event. Live pure or reissued replay requires the same identity and fingerprint. The native, wasm-gc and wasip2 adapters the compiler ships for one standard capability form one explicit replay-compatibility family. Their target-specific identities may differ, but the fingerprint must still match, so a standard trace stays portable between backends. Custom providers must match identity exactly. Provider fingerprints are audit metadata supplied by the host, not theorem hashes. The runtime can expose drift, but it cannot stop a dishonest host from reusing an old fingerprint for changed code.

Custom bindings have three host-bound routes. A Rust embedder can install one typed in-process provider binding, unchanged, in the VM or in a generated Rust artifact. A raw wasm-gc artifact imports the complete contract under a deterministic module name that contains its `contract_hash`, using native GC values and `externref` resources, and a JavaScript, Workers or Node host supplies it. See [`docs/wasm-gc-custom-capabilities.md`](wasm-gc-custom-capabilities.md) for the ABI and the generated value factories. A wasip2 artifact can import a generated WIT interface when every parameter and result in the complete contract is `Unit`, `Bool`, `Float` or `String`. Pure and effectful operations use the same transport. The component import pins the full `contract_hash` and publishes both hashes in its sibling WIT. An external Component Model host may implement that interface directly. For local execution, `aver run app.av --wasip2` instead links the Rust package bound in `aver.toml` through the cached host and dynamically adapts its existing `ProviderBinding` to the same WIT interface. Without a binding, `aver run --wasip2` fails preflight with `error[capability-provider-missing]`. The stock generated Rust binary has no custom binding either and fails preflight, so a separate Rust host links the provider crate through Cargo and installs the binding explicitly. Standard `Time` is still a provided binding. Its canonical source ships at `stdlib/capabilities/time.av`, and VM, generated Rust, wasm-gc and wasip2 each declare an exact shipped binding of that one contract. See [`docs/wasip2.md`](wasip2.md#custom-capability-imports-phase-3a) for the boundary and host contract.

### Opaque types

`exposes opaque` makes a type visible in signatures but blocks direct construction, field access and pattern matching from outside the module. The type can still be passed around, returned and stored.

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
- `Pricing.mkDiscount(50.0)` works (returns `Result<Discount, String>`)
- `Pricing.percent(d)` works (returns `Float`)
- `Discount(percent = 50.0)` is a **compile error** (opaque: cannot construct)
- `d.percent` is a **compile error** (opaque: cannot access fields)

With `--module-root examples`:

- `depends [Data.Fibonacci]` → `examples/data/fibonacci.av`, call as `Data.Fibonacci.fn(...)`
- `depends [Modules.Models.User]` → `examples/modules/models/user.av`, call as `Modules.Models.User.fn(...)`

A type may be written bare (`Step` rather than `Domain.State.Step`) when exactly one module in scope declares that name. In scope means the module itself, the modules it names in `depends [...]`, and the types those modules re-expose. A dependency that lists another module's type in its own `exposes [...]` passes that type on, still under the name of the module that declares it. A module elsewhere in the program that was not imported here has no say, so declaring a type in one cannot change what a name means anywhere else. When two modules that a file does import declare the same type name, the bare form is an error naming both candidates, and the reference has to be qualified.

The entry module follows the same rule. A dependency names the modules it uses in its own `depends [...]`, and nothing names the entry, so the entry's own declarations are in scope for the entry's own code and nowhere else. The file you point `run`, `verify` or `compile` at therefore never changes what a name written inside a dependency means.

## Static type checking

Type errors block `run`, `check` and `verify`. Nothing runs partially. The checker covers function bodies, top-level statements, effect propagation and duplicate binding detection.

## What Aver deliberately omits

| Absent | Reason |
|--------|--------|
| `if`/`else` | `match` is exhaustive, so no case goes missing silently |
| `for`/`while` | Use recursion, pattern matching, and explicit list operations |
| Streams / channels / async iterators | Recursive `?!` over lists gives streaming, backpressure and fan-out parallelism without new concepts |
| Async runtime | Aver does not make streaming a primitive. Its parallelism model is explicit independence (`?!`) rather than a full async runtime. You can build stream abstractions yourself, and the language stays small and reviewable |
| `null` | `Option<T>` with `Some`/`None` only |
| Exceptions | `Result<T, E>` only; errors are values |
| Global mutable state | No shared mutable state, by design |
| Closures | All functions are top-level, with no captured variables. Explicit is better than implicit |
| Magic | No decorators, no implicit behaviour, no runtime reflection |
| Bitwise operators (`&`, `\|`, `^`, `~`, `<<`, `>>`) | The operations exist as named functions in the `Bits` namespace. They stay out of the syntax for the same reason `/` and `%` do: a bit-level reading of an integer should be spelled out. `Bits` is a namespace, not a type. Its arguments and results are ordinary `Int` values, read as an infinite two's-complement bit sequence for one call. A fixed width is asked for explicitly with `Bits.low` and is never implied by a register size. Writing one of these operators reports which function replaces it |
