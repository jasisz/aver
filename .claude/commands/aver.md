You are an expert Aver programmer. Use the current language, not historical syntax.

## Core syntax

### Functions

```aver
fn name(param: Type) -> ReturnType
    ? "What this function does."
      "Optional continuation line."
    ! [Console.print, Disk.readText]
    x = expr
    expr
```

Rules:
- indentation-only function bodies — no braces, no `end`
- last expression is the return value, no `return` keyword
- all functions are top-level; no closures, lambdas, or anonymous fns
- top-level fns of the right shape can be passed where `Fn(...)` is expected
- `main` returns `Unit` or `Result<Unit, String>`

`?` descriptions:
- start with `? "..."` on the line after signature
- continuation lines are more string literals at deeper indent
- `aver check` warns when non-`main` functions omit the description

### Bindings

All bindings are immutable. No `let`, `val`, or `var`.

```aver
name = "Alice"
age: Int = 30
xs: List<Int> = []
```

### Types

Primitives: `Int`, `Float`, `String`, `Bool`, `Unit` — each has exactly one spelling; the string type is `String`, never abbreviated

Compound:
- `Result<T, E>`, `Option<T>`, `List<T>`, `Vector<T>`, `Map<K, V>` — `K` must be a type that orders; records, variants, lists and tuples all do, `Float` does not
- tuples: type is `Tuple<A, B, ...>` (2+ elements). Value literal and pattern are both paren: `(a, b)`. The type spelling and the value spelling are deliberately different.
- function types: `Fn(A) -> B`, `Fn(A) -> B ! [Console.print]`; `! [_]` on a callback parameter forwards the concrete named callback's effects at the helper call site

Notes:
- top-level named functions can be passed where `Fn(...)` is expected
- there are no lambdas and no closures
- no implicit type promotion; use `Float.fromInt(n)` / `Int.fromFloat(f)`

### User-defined types

Sum types:

```aver
type Shape
    Circle(Float)
    Rect(Float, Float)
    Point
```

Records:

```aver
record User
    name: String
    age: Int
```

Rules:
- constructors are qualified: `Shape.Circle(5.0)`, `Result.Ok(1)`, `Option.None`
- records use named fields: `User(name = "A", age = 1)`
- field access: `u.name`, `u.age`
- record update: `User.update(u, age = 31)`
- record positional pattern destructuring is not supported

### Match

```aver
match value
    Result.Ok(v) -> String.fromInt(v)
    Result.Err(e) -> e
```

Rules:
- `match` is the only branching construct (no `if` / `else`)
- **arm bodies must start on the same line as `->`** — multi-line bodies are a parse error; extract a helper function instead
- no colon after the subject
- no guards
- list patterns: `[]` and `[head, ..tail]` (the `..` rest must be named)
- tuple patterns: `(a, b)`
- constructor patterns always qualified: `Result.Ok`, `Option.None`, `Shape.Circle`
- literal patterns: `253 -> …` (`Int`), `"verack" -> …` (`String`), `1.5 -> …` (`Float`), `true` / `false` (`Bool`). An `Int` / `String` / `Float` match still needs a trailing `_ ->` or identifier arm; `-1 ->` is a parse error (no negative literal patterns) and so is an integer beyond 64 bits
- prefer one literal-pattern `match` over a chain of `match x == "lit"` with `true ->` / `false ->` helper functions — same behaviour, far less code
- boolean branching: `match x > 0` with `true ->` / `false ->`
- nested match in match arms is supported

### Effects

Effects are exact method-level names:

```aver
! [Http.get, Disk.readText, Console.print]
```

Rules:
- namespace shorthand `! [Disk]` covers all `Disk.*` effects
- `aver check` suggests narrowing when shorthand could be more specific
- effects propagate: callers must declare all effects of their callees
- no `effects X = [...]` aliases
- pure code stays pure; orchestration declares only the concrete effects it uses

### Modules

```aver
module Billing
    intent =
        "Billing application core."
        "Exports only the public entrypoints."
    exposes [charge, refund]
    depends [Core.Types, Infra.Store]
    effects [Console.print, Disk]
```

Rules:
- `module` must be the first top-level item in file-based programs
- `aver run` will execute a single file with no `module` as a convenience for quick throwaway scripts, but `aver check` requires the declaration (`error[missing-module]`) — declare `module <Name>` for anything you intend to keep, import, or check
- `intent` may be inline or multiline; formatter prefers multiline for multiline text
- `depends [...]` and `exposes [...]` are explicit
- opaque types: `exposes opaque [Discount]` — visible in signatures but cannot be constructed or destructured from outside
- `effects [...]` declares the module's effect surface. Every function's `! [Effect]` must be covered: a method-level entry like `Disk.readText` admits only that method, a namespace entry like `Disk` admits any `Disk.*` method. Underdeclared = type error; overdeclared = warning. A module with functions but no `effects [...]` triggers a warning to add the boundary (use `effects []` for a pure module).

### Verify blocks

Regular verify:

```aver
verify add
    add(1, 2) => 3
```

Law verify (finite universal checks):

```aver
verify add law commutative
    given a: Int = -2..2
    given b: Int = [-1, 0, 1]
    add(a, b) => add(b, a)
```

Rules:
- `verify` checks executable examples only
- law verify expands cartesian product of `given` domains (capped at 10,000 cases)
- `given x: T = [...]` describes the world / domain to test (values, or stubs for classified effects). `aver proof` quantifies universally over every value/stub — `given <Effect>` does **not** pin the law to one stub
- `when <pred>` is an explicit precondition on the law; cases where it's false are skipped (in runtime, proof, and `--hostile`). Use it to scope a law to assumed worlds (`when clock(BranchPath.Root, 1) > clock(BranchPath.Root, 0)`)
- `aver check` expects pure, non-trivial, non-`main` functions to carry a `verify` block
- plain `verify fn` on a fn with a generative effect (Random, Http, Time.now, etc.) warns — the case RHS is compared against a freshly-produced value and flaps. Use `verify fn law …` with `given` stubs or `verify fn trace` instead
- unclassified ambient state, persistent protocols, terminal modes, and server callbacks should use record/replay
- `aver audit --hostile` (or `aver verify --hostile`) layers adversarial worlds on top of every `verify <fn> law` block: typed `given`s get type-boundary values; classified effects get hostile profiles. Failures use slug `verify-hostile-mismatch`. Repair: `when <pred>` to scope the law, downgrade `law` → cases-form if it's stub-specific, or fix the impl. See `docs/oracle.md` for profile/boundary tables.

#### Oracle verify-trace (effectful functions)

Classified effectful fns get formal proof export via `verify <fn> trace`. Stubs bind oracles at verify time so the fn produces deterministic values and the trace can be asserted about.

```aver
fn roll() -> Int
    ? "roll a d6."
    ! [Random.int]
    Random.int(1, 6)

verify roll trace
    given rnd: Random.int = [highDie]
    rolled = roll()
    rolled.result => 6
    rolled.trace.length() => 1
    rolled.trace.contains(Random.int) => true

fn highDie(path: BranchPath, k: Int, lo: Int, hi: Int) -> Result<Int, String>
    ? "stub oracle: always max."
    Result.Ok(hi)
```

Rules:
- `given name: Effect.method = [stubFn, ...]` binds a stub for the classified effect. Multi-value list expands cartesian with cases; one `given` per effect — duplicates are rejected
- Stub signature for generative and generative-output effects (`Random.*`, `Process.stopRequested`, `Http.*`, `Disk.*`, `Tcp.*`, `Console.readLine`, `Time.*`, `Env.set`, and every `Terminal.*` operation except `size`): `(path: BranchPath, k: Int, args...) -> ReturnType`
- Stub signature for snapshot effects (`Args.get`, `Env.get`, `Terminal.size`): `(args...) -> ReturnType` — no path/counter prefix
- Output-only effects (`Console.print/.error/.warn`) don't need stubs; they append to the trace directly
- `BranchPath.Root` is a nullary value constructor — no parens, PascalCase. `BranchPath.child(parent, idx)` and `BranchPath.parse(str)` are the constructors for nested paths
- Case LHS projections:
  - `fn(args).result` — return value
  - `fn(args).trace` — full trace as a `Trace` record
  - `fn(args).trace.length()` — Int, event count
  - `fn(args).trace.event(k)` — `Option<EffectEvent>` at 0-based index
  - `fn(args).trace.contains(Effect.method)` — Bool, method-only predicate (ignores args)
  - `fn(args).trace.contains(Effect.method("arg"))` — Bool, exact event-literal match
  - `fn(args).trace.group(N).branch(idx).*` — tree-nav into `!`/`?!` independent products (0-based N, idx)
- Local bindings with `name = expr` go between `given` clauses and case assertions; they're substituted into every case (so each case still runs its own fresh `fn()` invocation)
- Every generative/gen+output effect the fn uses must have a `given` stub under `trace`; missing stubs are rejected with a pointer at the fix
- Whole server loops are not trace laws: verify pure `HttpWire` and handler functions separately, and use record/replay for the persistent `Tcp` session loop

### Decision blocks

A `decision` is top-level syntax, a sibling of `fn` and `type` — not a comment, not a markdown file next to the code. It records why the code looks the way it does: what was chosen, what was rejected, and which parts of the program the choice reaches. Write one whenever a reader would otherwise ask "why not the obvious thing?", and put it in the module the choice is about (a project may also gather them in one module, the way this repository uses `decisions/architecture.av`).

```aver
decision UseResultNotExceptions
    date = "2024-01-15"
    reason =
        "Exceptions hide in signatures."
        "Result forces explicit handling."
    chosen = "Result"
    rejected = ["Exceptions", "Nullable"]
    impacts = [safeDivide, safeRoot]
    author = "team"
```

Fields — all optional, any order:

- `date = "YYYY-MM-DD"` — a quoted string, when the decision was made
- `reason =` followed by an indented block of quoted strings, one claim per line — the argument itself
- `chosen = X` — one symbol or one quoted label: what won
- `rejected = [X, Y]` — symbols or quoted labels: what lost, and is therefore not to be re-proposed
- `impacts = [X, Y]` — symbols or quoted labels: what the decision reaches
- `author = "name"` — a quoted string, who to ask

A bare identifier in `chosen`, `rejected`, or `impacts` is a real reference: it must name a function, type, or effect the checker can see, and `aver check` reports an error on one that does not resolve. A quoted string is a free-form semantic label for anything the program does not contain (`"Exceptions"`, `"Braces"`, a rejected library). Prefer identifiers when the thing exists, so a rename or a deletion cannot quietly rot the record.

A real one, from this repository's own `decisions/architecture.av` — Aver explaining its own design in Aver:

```aver
decision SignificantIndentation
    date = "2024-01-20"
    author = "Aver core team"
    reason =
        "Braces are syntactic noise that adds no meaning and forces style debates."
        "Indentation is already how humans read code, so making it structural removes a class of inconsistency."
        "The lexer emits explicit INDENT and DEDENT tokens, keeping the parser context-free and easy to extend."
    chosen = "Indentation"
    rejected = ["Braces", "BeginEnd", "Keywords"]
    impacts = ["Lexer", "Parser", "AllModules"]
```

Reading them back:

```bash
aver context decisions/architecture.av --decisions-only
aver context main.av --module-root . --decisions-only --json
```

`--decisions-only` drops functions, types, and module intent from the context export and keeps only the `decision` blocks reachable from the entry through `depends [...]`, selected under the same `--budget` as an ordinary `aver context` run. That is the point of writing rationale as syntax instead of prose: it is checked, it is indexed, and one command hands a reader the whole argument behind a codebase without any of its code.

### Operators

- Arithmetic: `+`, `-`, `*` (operands must match types). `/` is **Float-only** — integer division is the `Int.div(a, b) : Result<Int, String>` function, not an operator (see gotchas below)
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Error propagation: `expr?` (unwraps `Result.Ok`, propagates `Result.Err`). **Result-only** — does not work on `Option`. For `Option`, use `Option.withDefault(opt, fallback)` or pattern-match.
- Independence: `(a, b)!` (parallel), `(a, b)?!` (parallel + Result unwrap)
- String interpolation: `"Hello, {name}!"` — **primitives only** (`Int`, `Float`, `Bool`, `String`). Embedding a list, record, tuple, `Map`, `Option`/`Result`, `Vector` or any named type is a type error; write a function returning `String` and interpolate its result.

**These operators do NOT exist** — do not use them. Writing one is an error that names the function replacing it (slug `rejected-operator`):

- no integer `/` — integer division is partial (it can divide by zero, and overflow on `i64::MIN / -1`), so use `Int.div(a, b)` which returns `Result<Int, String>`. Euclidean (flooring), the exact partner of `Int.mod`: `Int.div(-7, 2) = Result.Ok(-4)` and `Int.div(a,b)*b + Int.mod(a,b) == a` for every sign. `b == 0` returns `Result.Err("division by zero")`. The `/` operator stays total and works on `Float`
- no `%` (modulo) — use `Int.mod(a, b)` which returns `Result<Int, String>`. Euclidean modulo: result is always in `[0, |b|)`. `Int.mod(-7, 3) = Result.Ok(2)`, not `-1`. `b == 0` returns `Result.Err("division by zero")`
- no `&&`, `||` (boolean and/or) — use `Bool.and(a, b)`, `Bool.or(a, b)`, or nested `match`
- no `!` (boolean not) as prefix — use `Bool.not(x)`
- no `+=`, `-=`, `++`, `--` (mutation operators)
- no bitwise operators (`&`, `|`, `^`, `~`, `<<`, `>>`) — use the `Bits` namespace: `Bits.and(a, b)`, `Bits.or(a, b)`, `Bits.xor(a, b)`, `Bits.not(x)` are `Int -> Int`; `Bits.shiftLeft(x, n)`, `Bits.shiftRight(x, n)`, `Bits.low(x, width)` return `Result<Int, String>` (a negative count is `Result.Err`, and a syntactic non-negative literal count discharges to plain `Int` exactly as with `Int.div`). `Bits` is a NAMESPACE, not a type: arguments and results are ordinary mathematical `Int` values, read as an infinite two's-complement bit sequence for the duration of one call — so `Bits.not(x) == -x - 1`, `Bits.and(-1, x) == x`, `Bits.shiftRight(-3, 1) == -2` (arithmetic, not logical), and `Bits.shiftLeft(1, 100)` is exact, not truncated. Fixed width is requested explicitly with `Bits.low(x, 25)` rather than implied by a mask

### Recursion

There are no loops. Use recursion and pattern matching. Tail-call optimization is automatic.

```aver
fn sum(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [h, ..t] -> h + sum(t)
```

### Builtins and namespaces

Use namespaced builtins only.

Common pure namespaces:
- `Int`, `Float`, `String`, `List`, `Vector`, `Map`, `Bool`, `Bits`, `Crypto`, `Result`, `Option`

`Bytes` and `Crypto.Digest32` are embedded Aver modules. With
`depends [Bytes, Crypto.Digest32]`, `Crypto.sha256 : Bytes -> Digest32` is total
and pure: the input already guarantees octets and the result guarantees exactly
32 bytes.

- `Bytes.fromList : List<Int> -> Result<Bytes, String>` (a list literal whose every element is an integer literal in `0..=255` discharges to plain `Bytes`), `Bytes.octets : Bytes -> List<Int>`
- `Bytes.fromHex : String -> Result<Bytes, String>` (even length, case-insensitive, no `0x` prefix), `Bytes.toHex : Bytes -> String`
- `Crypto.Digest32.fromBytes : Bytes -> Result<Digest32, String>`, `Crypto.Digest32.bytes : Digest32 -> Bytes`
- `Crypto.Digest32.fromHex : String -> Result<Digest32, String>`, `Crypto.Digest32.toHex : Digest32 -> String`

Key `String` API:
- `String.len`, `String.contains`, `String.startsWith`, `String.endsWith`
- `String.byteLength : String -> Int` — UTF-8 byte count; `String.len` counts characters (Unicode scalar values), on every backend
- `String.charAt : (String, Int) -> Option<String>`, `String.slice : (String, Int, Int) -> String` — character indices; a slice with an out-of-range end clamps
- `String.toUpper`, `String.toLower`, `String.trim`, `String.replace : (String, String, String) -> String`
- `String.join`, `String.split`, `String.chars` — concat is the `+` operator
- `String.toUtf8 : String -> Bytes`, `String.fromUtf8 : Bytes -> Result<String, String>` — explicit, lossless encoding and validated decoding
- `Int.fromString : String -> Result<Int, String>`, `String.fromInt : Int -> String`
- `Int.toBigEndian`, `Int.toLittleEndian : (Int, Int) -> Result<Bytes, String>`; `Int.fromBigEndian`, `Int.fromLittleEndian : Bytes -> Int`
- `Float.fromString`, `String.fromFloat`, `String.fromBool` — convention: `<targetTyp>.from<source>`
- string interpolation: `"Hello, {name}!"` is the idiomatic way to render PRIMITIVES into text; reserve `String.fromInt` etc. for explicit data conversion (e.g. building keys: `"user:" + String.fromInt(id)`). Compound values have no built-in rendering — write your own `fn show(x: T) -> String`.

Key code-point API:
- `String.firstCodePoint : String -> Option<Int>` — first Unicode scalar value, `Option.None` for empty text
- `String.fromCodePoint : Int -> Option<String>` — code point to a 1-character string, `Option.None` for surrogates and out-of-range values

Key `Int` API:
- `Int.abs : Int -> Int`, `Int.min`, `Int.max` — `(Int, Int) -> Int`
- `Int.div`, `Int.mod` — `(Int, Int) -> Result<Int, String>`, Euclidean, see the operators section above
- `Int.fromString : String -> Result<Int, String>`, `Int.fromFloat : Float -> Int`
- `Int.toBigEndian`, `Int.toLittleEndian : (Int, Int) -> Result<Bytes, String>`; `Int.fromBigEndian`, `Int.fromLittleEndian : Bytes -> Int`

Key `Float` API:
- `Float.abs`, `Float.sqrt`, `Float.sin`, `Float.cos` — `Float -> Float`; the trig functions take radians
- `Float.pow`, `Float.atan2`, `Float.min`, `Float.max` — `(Float, Float) -> Float`
- `Float.floor`, `Float.ceil`, `Float.round` — `Float -> Int`, so they convert as well as round
- `Float.pi : () -> Float` — a nullary function, written `Float.pi()`
- `Float.fromInt : Int -> Float`, `Float.fromString : String -> Result<Float, String>`

Key `Result` / `Option` API:
- `Result.withDefault : (Result<T, E>, T) -> T`, `Option.withDefault : (Option<T>, T) -> T`
- `Result.fromOption : (Option<T>, E) -> Result<T, E>` — the bridge that lets an `Option` join a `?` chain

Key `List` API (small, recursion-first):
- `List.len`, `List.prepend`, `List.concat`, `List.reverse`, `List.contains`, `List.zip`, `List.take`, `List.drop`, `List.fromVector`
- No `List.map`, `List.filter`, `List.fold` — write with recursion
- empty list literal: `[]`

Key `Vector` API (O(1) indexed access):
- `Vector.new(n, default) -> Result<Vector<T>, String>` for a dynamic size; a syntactic literal in the portable `0..=1_048_576` element budget discharges directly to `Vector<T>`
- `Vector.get(v, i) -> Option<T>`, `Vector.set(v, i, val) -> Option<Vector<T>>`
- `Vector.len(v) -> Int`
- `Vector.fromList(l)` — conversion in the other direction lives on `List`

Key `Map` API:
- a map iterates sorted by key on every backend, so the key type must order. Records (by field name), variants (by constructor name), lists, tuples and `Bytes` all key a map fine. `Float` cannot — a NaN has no place in the finite range — and neither can `Map` or `Vector`. Float stays legal as a *value*.
- empty map literal: `{}` (type from context); non-empty: `{a => 1, b => 2}`. There is no `Map.empty()` builtin.
- `Map.fromList(pairs)`, `Map.get(m, k) -> Option<V>`, `Map.set(m, k, v)`, `Map.has(m, k)`, `Map.remove(m, k)`, `Map.keys(m)`, `Map.values(m)`, `Map.entries(m)`, `Map.len(m)`

Effectful namespaces:
- `Console`: print, error, warn, readLine — **`print`/`error`/`warn` take `String`**, not arbitrary values. Stringify at the call site: interpolation `"{x}"` for primitives, a per-type render fn (`fn show(r: Result<T, E>) -> String`) for compound shapes.
- `Http`: get, post, put, patch, delete, head
- `Disk`: readText, writeText, appendText, exists, delete, deleteDir, listDir, makeDir
- `Tcp`: connect, writeLine, writeBytes, readLine, readBytes, close, send, sendBytes, ping — `send`/`readLine` are text-only (UTF-8); binary payloads use nominal `Bytes` through `sendBytes`, `writeBytes`, and `readBytes`
- `Terminal`: every operation is fallible. Control/output calls return `Result<Unit, String>`, `readKey` returns `Result<Option<String>, String>`, and `size` returns `Result<Terminal.Size, String>`. The cursor move is `Terminal.moveTo(x, y)`; `Terminal.print` / `Terminal.setColor` take `String`.
- `Time`: now, unixMs, sleep — `Time.sleep(ms) -> Result<Unit, String>` for a dynamic duration; a valid non-negative i64 literal discharges directly to `Unit`
- `Random`: int, float — `Random.int(lo, hi) -> Result<Int, String>` is inclusive on both ends; safe literal bounds discharge directly to `Int`. `Random.float()` is in `[0.0, 1.0)`
- `Env`: get, set
- `Args`: get

Incoming HTTP is not an effect namespace: use pure `HttpWire`, native `HttpServe` over `Tcp` + `Process.stopRequested`, or an explicit `--handler <fn>` on fetch/proxy hosts.

### Common patterns

Recursive list processing (filter):
```aver
fn collectPositive(xs: List<Int>) -> List<Int>
    match xs
        [] -> []
        [h, ..t] -> match h > 0
            true  -> List.prepend(h, collectPositive(t))
            false -> collectPositive(t)
```

Error propagation chain:
```aver
fn parseAndDivide(a: String, b: String) -> Result<Int, String>
    x = Int.fromString(a)?
    y = Int.fromString(b)?
    safeDivide(x, y)?
```

Map lookup:
```aver
match Map.get(ages, "alice")
    Option.Some(age) -> "Alice is {age}"
    Option.None -> "Unknown"
```

### Common mistakes to avoid

1. Writing `.aver` files instead of `.av`
2. `if`/`else` — use `match`
3. `val`/`var`/`let` — just `name = expr`
4. Bare `Ok(x)` — must be `Result.Ok(x)`
5. `String.toInt(s)` — not a thing; use `Int.fromString(s)` which returns `Result<Int, String>`
6. Assuming `Console.readLine()` returns `String` — it returns `Result<String, String>`
7. Writing `=` instead of `=>` in verify cases — separator is always `=>`
8. Using `..` without a name in list patterns — write `[h, ..t]`, not `[h, ..]`
9. Missing `!` effect declaration — compiler errors
10. Closures/lambdas — not supported; use named top-level functions
11. Mutable variables — not supported, all bindings are immutable
12. `List.map`/`List.filter`/`List.fold` — not built-in; write with recursion
13. Pipe `|>` — not supported
14. Positional record destructuring in match — bind record, use field access
15. Multi-line match arms — body must follow `->` on the same line; extract complex logic into a named function
16. `BranchPath.Root()` / `BranchPath.root()` — it's a nullary value constructor, no parens: just `BranchPath.Root`
17. Two `given` for the same effect — rejected. Use a multi-value domain `given rnd: Random.int = [stubA, stubB]` for varied samples
18. Plain `verify fn` on a fn with generative effects — you get a lint warning, use `verify fn law …` with `given` stubs or `verify fn trace` instead
19. `()` as a Unit value literal — there is no `()` literal. Write `Unit`. Diagnostics render the value as `()`, but in source the only spelling is `Unit` (matches `Map<T, Unit>` set semantics, `Unit` field annotations, etc.). `Console.print(...)` returning Unit is implicit — you almost never write the literal directly
20. `expr?` on an `Option<T>` — `?` is Result-only. For `Option`, use `Option.withDefault(opt, fallback)`, or `match opt { Option.Some(v) -> … ; Option.None -> … }`. `Vector.get` and `Map.get` return `Option`, so neither composes with `?` directly — wrap the value first
21. `(A, B)` as a tuple type — type position uses `Tuple<A, B>` exclusively. Tuple **value** literals stay paren: `(1, 2)`, `[(1, 2), (3, 4)]`, `Result.Ok((a, b))`. Tuple **patterns** stay paren: `match p { (a, b) -> … }`. The type and the value spelling are deliberately different so grep-for-type and grep-for-value don't collide
22. Dispatching on a `String` or an `Int` through a chain of `match x == "lit"` with `true ->` / `false ->` helper functions — literal patterns exist: `match cmd { "verack" -> 1 ; "tx" -> 4 ; _ -> 0 }`. Only the trailing `_` arm is mandatory
23. Keying a map on a `Float` — the key type has to order and a NaN has no place in the finite range. Key on the value that orders (`Map<String, Reading>` rather than `Map<Float, Reading>`). A `Map` or a `Vector` in key position is refused for the same reason: neither has an order of its own

### Style

Prefer:
- explicit domain types (records, sum types)
- short, concrete `? "..."` descriptions
- exact method effects
- qualified constructors everywhere
- straightforward orchestration over clever higher-order helpers
- `verify` blocks on all pure non-trivial functions
- `decision` blocks for non-obvious architectural choices

Avoid:
- broad effect declarations when specific ones suffice
- hiding domain flow behind unnecessary abstraction
- functions longer than ~30 lines; split into named helpers
