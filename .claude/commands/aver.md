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
- function bodies use indentation only: no braces, no `end`
- the last expression is the return value; there is no `return` keyword
- all functions are top-level; no closures, lambdas, or anonymous fns
- top-level fns of the right shape can be passed where `Fn(...)` is expected
- `main` returns `Unit` or `Result<Unit, String>`

`?` descriptions:
- start with `? "..."` on the line after the signature
- continuation lines are more string literals at deeper indent
- `aver check` warns when non-`main` functions omit the description

### Bindings

All bindings are immutable. There is no `let`, `val`, or `var`.

```aver
name = "Alice"
age: Int = 30
xs: List<Int> = []
```

### Types

Primitives: `Int`, `Float`, `String`, `Bool`, `Unit`. Each has exactly one spelling. The string type is `String`, never abbreviated.

Compound:
- `Result<T, E>`, `Option<T>`, `List<T>`, `Vector<T>`, `Map<K, V>`. `K` must be a type that orders: records, variants, lists and tuples all do, `Float` does not
- tuples: the type is `Tuple<A, B, ...>` (2+ elements). The value literal and the pattern both use parens: `(a, b)`. The type spelling and the value spelling are deliberately different.
- function types: `Fn(A) -> B`, `Fn(A) -> B ! [Console.print]`; `! [_]` on a callback parameter forwards the concrete named callback's effects at the helper call site

Notes:
- top-level named functions can be passed where `Fn(...)` is expected
- there are no lambdas and no closures
- there is no implicit type promotion; use `Float.fromInt(n)` / `Int.fromFloat(f)`

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
- positional destructuring of records in patterns is not supported

### Match

```aver
match value
    Result.Ok(v) -> String.fromInt(v)
    Result.Err(e) -> e
```

Rules:
- `match` is the only branching construct (no `if` / `else`)
- **arm bodies must start on the same line as `->`.** A multi-line body is a parse error; extract a helper function instead
- no colon after the subject
- no guards
- list patterns: `[]` and `[head, ..tail]` (the `..` rest must be named)
- tuple patterns: `(a, b)`
- constructor patterns are always qualified: `Result.Ok`, `Option.None`, `Shape.Circle`
- literal patterns: `253 -> …` (`Int`), `"verack" -> …` (`String`), `1.5 -> …` (`Float`), `true` / `false` (`Bool`). An `Int` / `String` / `Float` match still needs a trailing `_ ->` or identifier arm. `-1 ->` is a parse error (there are no negative literal patterns), and so is an integer beyond 64 bits
- prefer one literal-pattern `match` over a chain of `match x == "lit"` with `true ->` / `false ->` helper functions. It behaves the same with far less code
- boolean branching: `match x > 0` with `true ->` / `false ->`
- nested match in match arms is supported

### Effects

Effects are exact method-level names:

```aver
! [Http.get, Disk.readText, Console.print]
```

Rules:
- namespace shorthand `! [Disk]` covers all `Disk.*` effects
- `aver check` suggests narrowing when the shorthand could be more specific
- effects propagate: callers must declare all effects of their callees
- there are no `effects X = [...]` aliases
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
- `aver run` will execute a single file with no `module` as a convenience for quick throwaway scripts, but `aver check` requires the declaration (`error[missing-module]`). Declare `module <Name>` for anything you intend to keep, import, or check
- `intent` may be inline or multiline; the formatter prefers multiline for multiline text
- `depends [...]` and `exposes [...]` are explicit
- module `A.B` lives at `a/b.av` under `--module-root` (the exact-case `A/B.av` is the fallback); the entry file may have any name
- opaque types: with `exposes opaque [Discount]` the type is visible in signatures but cannot be constructed or destructured from outside
- `effects [...]` declares the module's effect surface. Every function's `! [Effect]` must be covered: a method-level entry like `Disk.readText` admits only that method, and a namespace entry like `Disk` admits any `Disk.*` method. Underdeclaring is a type error; overdeclaring is a warning. A module with functions but no `effects [...]` gets a warning to add the boundary (use `effects []` for a pure module).

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
- law verify expands the cartesian product of the `given` domains (capped at 10,000 cases)
- `given x: T = [...]` describes the world or domain to test (values, or stubs for classified effects). `aver proof` quantifies universally over every value and stub, so `given <Effect>` does **not** pin the law to one stub
- `when <pred>` is an explicit precondition on the law. Cases where it is false are skipped (in runtime, proof, and `--hostile`). Use it to scope a law to assumed worlds (`when clock(BranchPath.Root, 1) > clock(BranchPath.Root, 0)`)
- `aver check` requires pure, non-trivial, non-`main` functions to carry a `verify` block. `error[missing-verify]` fails the check; a law block counts
- `<expr> holds` is a case whose `Bool` value must be `true`, so a law may state `a != b holds`
- plain `verify fn` on a fn with a generative effect (Random, Http, Time.now, etc.) warns, because the case RHS is compared against a freshly produced value and flaps. Use `verify fn law …` with `given` stubs or `verify fn trace` instead
- unclassified ambient state, persistent protocols, terminal modes, and server callbacks should use record/replay
- `aver audit --hostile` (or `aver verify --hostile`) layers adversarial worlds on top of every `verify <fn> law` block: typed `given`s get type-boundary values, and classified effects get hostile profiles. Failures use slug `verify-hostile-mismatch`. To repair: scope the law with `when <pred>`, downgrade `law` → cases-form if it is stub-specific, or fix the impl. See `docs/oracle.md` for the profile and boundary tables.

#### Oracle verify-trace (effectful functions)

Classified effectful fns get formal proof export through `verify <fn> trace`. Stubs bind oracles at verify time, so the fn produces deterministic values and you can assert about the trace.

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
- `given name: Effect.method = [stubFn, ...]` binds a stub for the classified effect. A multi-value list expands cartesian with the cases. One `given` per effect; duplicates are rejected
- Stub signature for generative and generative-output effects (`Random.*`, `Process.stopRequested`, `Http.*`, `Disk.*`, `Tcp.*`, `Console.readLine`, `Time.*`, `Env.set`, and every `Terminal.*` operation except `size`): `(path: BranchPath, k: Int, args...) -> ReturnType`
- `k` is the call index of that one operation on that branch path, counted from 0. Other operations never move it, so `match k` is a reply script for that operation alone, and it stays correct when the function under test adds a clock read or a log line between two calls
- Script a stub by call index within one function body, and let the claim reach the operation through one call. An exported proof numbers one body at a time. It cannot follow a run across a call into an effectful helper, on each turn of a recursion, past a `match` whose arms call the operation a different number of times, or across two calls in one claim (a run numbers those in sequence while the export numbers each from 0). `aver proof` declines such a law with a named reason instead of numbering it differently, and sampled cases still export. `docs/oracle.md` names the five shapes
- Stub signature for snapshot effects (`Args.get`, `Env.get`, `Terminal.size`): `(args...) -> ReturnType`, with no path or call-index prefix
- Output-only effects (`Console.print/.error/.warn`) need no stubs; they append to the trace directly
- `BranchPath.Root` is a nullary value constructor: no parens, PascalCase. `BranchPath.child(parent, idx)` and `BranchPath.parse(str)` are the constructors for nested paths
- Case LHS projections:
  - `fn(args).result`: the return value
  - `fn(args).trace`: the full trace as a `Trace` record
  - `fn(args).trace.length()`: Int, the event count
  - `fn(args).trace.event(k)`: `Option<EffectEvent>` at a 0-based index
  - `fn(args).trace.contains(Effect.method)`: Bool, a method-only predicate (ignores args)
  - `fn(args).trace.contains(Effect.method("arg"))`: Bool, an exact event-literal match
  - `fn(args).trace.group(N).branch(idx).*`: tree navigation into `!`/`?!` independent products (0-based N, idx)
- Local bindings with `name = expr` go between the `given` clauses and the case assertions. They are substituted into every case, so each case still runs its own fresh `fn()` invocation
- Every generative or gen+output effect the fn uses must have a `given` stub under `trace`; a missing stub is rejected with a pointer at the fix
- Whole server loops are not trace laws. Verify pure `HttpWire` and handler functions separately, and use record/replay for the persistent `Tcp` session loop

### Decision blocks

A `decision` is top-level syntax, a sibling of `fn` and `type`, written in the source rather than in a comment or a markdown file next to the code. It records why the code looks the way it does: what was chosen, what was rejected, and which parts of the program the choice reaches. Write one whenever a reader would otherwise ask "why not the obvious thing?". Put it in the module the choice is about (a project may also gather them in one module, the way this repository uses `decisions/architecture.av`).

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

Fields (all optional, any order):

- `date = "YYYY-MM-DD"`: a quoted string, when the decision was made
- `reason =` followed by an indented block of quoted strings, one claim per line: the argument itself
- `chosen = X`: one symbol or one quoted label, what won
- `rejected = [X, Y]`: symbols or quoted labels for what lost, which is therefore not to be proposed again
- `impacts = [X, Y]`: symbols or quoted labels for what the decision reaches
- `author = "name"`: a quoted string, who to ask

A bare identifier in `chosen`, `rejected`, or `impacts` is a real reference. It must name a function, type, or effect the checker can see, and `aver check` reports an error on one that does not resolve. A quoted string is a free-form semantic label for anything the program does not contain (`"Exceptions"`, `"Braces"`, a rejected library). Prefer identifiers when the thing exists, so a rename or a deletion cannot quietly rot the record.

A real one, from this repository's own `decisions/architecture.av`, where Aver explains its own design in Aver:

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

`--decisions-only` drops functions, types, and module intent from the context export. It keeps only the `decision` blocks reachable from the entry through `depends [...]`, selected under the same `--budget` as an ordinary `aver context` run. Rationale written as syntax is checked and indexed, and one command hands a reader the whole argument behind a codebase without any of its code.

### Operators

- Arithmetic: `+`, `-`, `*` (operands must match types). `/` is **Float-only**. Integer division is the function `Int.div(a, b) : Result<Int, String>`; there is no integer division operator (see gotchas below)
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Error propagation: `expr?` unwraps `Result.Ok` and propagates `Result.Err`. It is **Result-only** and does not work on `Option`. For `Option`, use `Option.withDefault(opt, fallback)` or pattern-match.
- Independence: `(a, b)!` (parallel), `(a, b)?!` (parallel + Result unwrap)
- String interpolation: `"Hello, {name}!"` takes **primitives only** (`Int`, `Float`, `Bool`, `String`). Embedding a list, record, tuple, `Map`, `Option`/`Result`, `Vector` or any named type is a type error. Write a function returning `String` and interpolate its result.

**These operators do NOT exist.** Do not use them. Writing one is an error that names the function that replaces it (slug `rejected-operator`):

- no integer `/`. Integer division is partial (it can divide by zero, and overflow on `i64::MIN / -1`), so use `Int.div(a, b)`, which returns `Result<Int, String>`. It is Euclidean (flooring), the exact partner of `Int.mod`: `Int.div(-7, 2) = Result.Ok(-4)` and `Int.div(a,b)*b + Int.mod(a,b) == a` for every sign. `b == 0` returns `Result.Err("division by zero")`. The `/` operator stays total and works on `Float`
- no `%` (modulo). Use `Int.mod(a, b)`, which returns `Result<Int, String>`. It is Euclidean modulo: the result is always in `[0, |b|)`, so `Int.mod(-7, 3) = Result.Ok(2)`, not `-1`. `b == 0` returns `Result.Err("division by zero")`
- no `&&`, `||` (boolean and/or). Use `Bool.and(a, b)`, `Bool.or(a, b)`, or nested `match`
- no prefix `!` (boolean not). Use `Bool.not(x)`
- no `+=`, `-=`, `++`, `--` (mutation operators)
- no bitwise operators (`&`, `|`, `^`, `~`, `<<`, `>>`). Use the `Bits` namespace: `Bits.and(a, b)`, `Bits.or(a, b)`, `Bits.xor(a, b)`, `Bits.not(x)` are `Int -> Int`; `Bits.shiftLeft(x, n)`, `Bits.shiftRight(x, n)`, `Bits.low(x, width)` return `Result<Int, String>` (a negative count is `Result.Err`, and a syntactic non-negative literal count discharges to plain `Int` exactly as with `Int.div`). `Bits` is a NAMESPACE, not a type. Arguments and results are ordinary mathematical `Int` values, read as an infinite two's-complement bit sequence for the duration of one call. So `Bits.not(x) == -x - 1`, `Bits.and(-1, x) == x`, `Bits.shiftRight(-3, 1) == -2` (arithmetic, not logical), and `Bits.shiftLeft(1, 100)` is exact, not truncated. Request a fixed width explicitly with `Bits.low(x, 25)`; a mask does not imply one

### Recursion

There are no loops. Use recursion and pattern matching. Tail-call optimization is automatic.

```aver
fn sum(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [h, ..t] -> h + sum(t)
```

### Processes, answer modules and the coordinator

A function whose effect list names `yield` is a process. You write it in direct style (ask, then the next step), but it never runs as written. Every call to an operation of a capability the program answers itself is a request, and the self tail call is a `Yield` request. The compiler cuts the function at each one into state types and pure answer functions under the reserved `__` namespace. The loop that seats the processes, waits once per turn and answers the requests is generated into the entry module. The smallest program of that shape is one ticker, one clock and one job kind, at `tests/fixtures/run_guide_example/` in the repository; `tests/fixtures/run_families/` adds a process seated once per key. Every block below is cut from those two as they stand in the files. Both programs check, verify and run, and a test pins the blocks to them.

**A process** is a yielding function of the entry module that answers `Unit`. Its effect list names the operations it asks for and `yield`. An operation of a capability the program answers becomes a request. Everything else (`Console.print` here) runs in place, inside the turn. A yielding helper it calls takes parameters like any function:

```aver
fn ticker() -> Unit
    ? "Scores three tasks off the turn, one tick apart, then says what they scored together."
    ! [Clock.score, Clock.tick, Console.print, yield]
    tallying([1, 2, 3], 0)

fn tallying(tasks: List<Int>, total: Int) -> Unit
    ? "Waits for a tick, then for the score of the next task, until no task is left."
    ! [Clock.score, Clock.tick, Console.print, yield]
    match tasks
        [] -> Console.print("scored {total}")
        [task, ..rest] -> match Clock.tick()
            Clock.Tick.Tock -> tallying(rest, total + Clock.score(task))
```

`Clock` is a capability module of the program. It declares its operations and nothing else; whoever answers them is not its business:

```aver
module Clock
    kind = capability
    semantics = effectful
    intent = "What the ticker asks for: a tick, and the score of one task."
    exposes [Tick, tick, score]

type Tick
    Tock

operation tick() -> Clock.Tick
    ? "The next tick."
    oracle = generative
    replay = recorded
```

**An answer module** says which capabilities it answers in its header, with `answers [Clock]`. It is part of the program like any other module: some module lists it in `depends`, usually the entry. Write one function per operation, all threading one state `S`, plus a pure `fresh() -> S` the loop starts the module from:

```
fn op(state: S, args...) -> Tuple<S, Result<R, Run.Wake>>
```

`R` is the operation's own result. `Result.Ok(v)` answers the request now. `Result.Err(wake)` answers nothing yet and says when to ask again. Both keep the state the function returned, so an `Err` is where the module records its own progress (an offset, a retry count, a job handle) while the request itself stays where it is. An answer function that is pure carries a `verify` block like any other, with the tuple it returns on the right-hand side:

```aver
module Clocked
    intent = "The answers this program gives to Clock: a tick fifty milliseconds after it was asked for, and a score computed by a Scoring job this module begins itself."
    depends [Clock, Scoring, Wait, Work]
    exposes [State, fresh, tick, score, points]
    effects [Scoring.begin, Scoring.take]
    answers [Clock]

record State
    armed: Bool
    jobs: Map<Int, Work.Job>

fn fresh() -> State
    ? "No deadline armed and no job running."
    State(armed = false, jobs = {})

fn tick(state: State) -> Tuple<State, Result<Clock.Tick, Run.Wake>>
    ? "The first ask arms a fifty-millisecond deadline and waits for it; the ask after the deadline is the tick."
    match state.armed
        false -> (State.update(state, armed = true), Result.Err(Run.Wake.Until([], Option.Some(50))))
        true -> (State.update(state, armed = false), Result.Ok(Clock.Tick.Tock))

verify tick
    tick(fresh()) => (State.update(fresh(), armed = true), Result.Err(Run.Wake.Until([], Option.Some(50))))
    tick(State.update(fresh(), armed = true)) => (fresh(), Result.Ok(Clock.Tick.Tock))
```

An `Err` repeats the original arguments against the answer module's current state; it never resumes an operation instance. Identity belongs in an argument or a handle. False readiness and another `Err` are ordinary.

The wake is a `Run.Wake`, from the standard module `Run`, which needs no `depends`:
- `Until(items, deadline)`: ask again in a turn whose wait reported one of the items (`Wait.Item.Socket(Tcp.Socket)` or `Wait.Item.Job(Work.Job)`), or once the deadline in milliseconds has passed, whichever comes first. `Until([], Option.Some(ms))` is a plain deadline. `Until([], Option.Some(0))` asks again on the next turn and makes that turn poll with a zero timeout, so use it only where nothing better will do. A clock reading that has fallen more than the deadline behind the moment of the park also fires it, so a clock that steps backwards cannot strand the request
- `Settled(deadline)`: ask again once this answer module has answered any request other than with `Settled`, or once the deadline has passed. It is how one process waits for another to change the module's state. A `Settled` answer does not count, so a request cannot wake itself; a module that needs to be asked again regardless answers `Until([], Option.Some(0))`

**A job** is the answer to a request. The answer module begins it, keeps its handle in its state and parks the request on it with `Until([Wait.Item.Job(job)], Option.None)`. The ask after the wait reported the job takes it: `Ok(None)` means it has not finished, so park again; `Ok(Some(r))` is the answer; `Err` means it never will finish:

```aver
fn score(state: State, task: Int) -> Tuple<State, Result<Int, Run.Wake>>
    ? "The first ask for a task begins its job and waits for it; an ask after the job has settled takes its result. A job that could not begin scores 0."
    ! [Scoring.begin, Scoring.take]
    match Map.get(state.jobs, task)
        Option.Some(job) -> taken(state, task, job)
        Option.None -> match Scoring.begin(task)
            Result.Err(_) -> (state, Result.Ok(0))
            Result.Ok(job) -> (State.update(state, jobs = Map.set(state.jobs, task, job)), Result.Err(Run.Wake.Until([Wait.Item.Job(job)], Option.None)))
```

A job kind is a capability of Work shape: `depends [Work]` and exactly these two operations, with `T` and `R` ordinary data of the program:

```aver
module Scoring
    kind = capability
    semantics = effectful
    depends [Work]
    intent = "The job kind that scores one task off the turn."
    exposes []

operation begin(task: Int) -> Result<Work.Job, String>
    ? "Starts scoring one task off the turn and answers its handle at once."
    oracle = generativeOutput
    replay = recorded

operation take(job: Work.Job) -> Result<Option<Int>, String>
    ? "None while the job runs, Some(score) once it finished."
    oracle = generativeOutput
    replay = recorded
```

`T` and `R` may also be types of the modules the job kind lists in `depends`, written with their owner: `operation begin(task: Ledger.Request) -> Result<Work.Job, String>`. A job kind is the only capability that may name another module's type, because a job is answered by a function of the same program and the two ship together. The module has to be in `depends` and has to expose the type, the same two gates an ordinary fn passes. The layouts it names, and the layouts those reach, go into its `contract_hash` under the name of the module that declares each one. A named type that holds a capability resource anywhere inside it (a socket or a job handle) is refused, and the refusal names the field and the type. Every other capability stays closed on its own declarations.

The manifest names the pure function that does the work. That, the job limit and host packages are all the manifest says:

```toml
[[providers.bindings]]
capability = "Scoring"
work = "Clocked.points"

[work]
max-jobs = 2
```

**A process per key.** A process that takes one parameter is seated once per key a pure function of an answer module lists:

```aver
process member seated by Hub.members
```

`Hub.members` is `(Hub.State) -> List<Int>`, and `member` takes an `Int`. At every turn boundary, after the turn has served its requests, the loop reads the list: it seats a member for every listed key that has none, in list order; it drops the member whose key has left the list, and counts it in `Run.View.dropped`; and a key whose member returned is not seated again until it has been absent from the list once. A request that names a key the module no longer knows is answered, never parked: here `next` answers `Gone`:

```aver
fn next(state: State, key: Int, seen: Int) -> Tuple<State, Result<Board.Step, Run.Wake>>
    ? "A value newer than seen answers at once. A member whose key is no longer wanted is told it is gone, which is how a request for an unknown key is answered, never by waiting. Otherwise the request waits until this module has answered something other than a wait."
    match List.contains(state.keys, key)
        false -> (state, Result.Ok(Board.Step.Gone))
        true -> match seen < List.len(state.values)
            true -> (state, Result.Ok(Board.Step.Value(valueAt(state.values, seen))))
            false -> match state.closed
                true -> (state, Result.Ok(Board.Step.Gone))
                false -> (state, Result.Err(Run.Wake.Settled(Option.None)))
```

**The loop.** An entry module that writes a process and no `main` gets the loop's own `main`. A program with a command line of its own writes `main` and calls `Run.all()` where the loop should run; its effect list is widened by what the loop performs. The run ends once nothing is seated or a stop was requested. Two policies are optional functions of the entry, found by name: `fn stop(view: Run.View) -> Bool` ends the run early, and `fn admit(view: Run.View, id: Int) -> Bool` holds back an askable request for a turn. `Run.View` is generated: `pending: Map<Int, Run.Pending>` (one marker per seated process: its key when it has one, the instance it waits on and its wake), `ready`, `askable`, `dropped` and `stopping`. A policy is pure and carries a `verify` block:

```aver
fn stop(view: Run.View) -> Bool
    ? "The run is over once a stop was requested; the loop also ends it once nothing is seated."
    view.stopping
```

Everything else is generated into the entry module under `__`: the slot table, the one `Wait.poll` per turn, the clock reading, the dispatch, the seating of keyed processes, the shutdown that cancels every job a parked request waits on, and `main`. The generated names stay callable, so a test can drive them; `tests/fixtures/run_schedule_cases/` states the loop's invariants as laws over them. `AVER_YIELD_DUMP=1 aver check main.av --module-root .` prints the generated Aver.

Rules:
- a loop that must not starve the others declares `yield`. The declaration fixes where control is handed back; it does not bound how long one step takes
- long pure work goes to a job kind, begun by the answer module that answers the request waiting for it
- "atomic between two yields" means no other step of the program runs in between. It does not mean atomicity of external effects, and it is not a time bound
- an answer module runs inside the turn, so a slow answer stalls every process. An answer with effects that can block is allowed, and `warning[answer-shape]` says so
- `Disk` operations stay synchronous inside the turn, and the turn budget does not see that time
- `Wait.poll` is one wait over sockets and jobs; `Tcp.poll` is the same wait over sockets only. A program that writes its own loop still uses it
- a wait set is keyed by any type a map accepts. A program waiting on several kinds of thing at once names each kind with a constructor instead of agreeing on an arithmetic convention: with `type Watch` declaring `Peer(Int)`, `Listener` and `Job`, `Wait.poll` takes `Map<Watch, Wait.Item>` and answers `List<Watch>` in that map's own key order, which is by constructor name and then payload. `Int` is one such key and needs no change. One program uses one wait key type. A wait set written empty at the call names no key of its own, so under a key of the program's own, write its type down: `idle: Map<Watch, Wait.Item> = {}`
- a job is pure, its result is data, and a recording replays it. `begin`, `take` and the wait are served back in the recorded turns. The VM and wasm-gc run the job's function again beside them, a `--target rust` binary serves the recorded results without running it, and a wasip2 component records nothing
- `yield` is an effect: declare it in `! [...]` and cover it in the module's `effects [...]`. The entry module's `effects [...]` is widened by what the loop generates into it
- a process without a `process ... seated by ...` line takes no parameters. A yielding helper of the same module may take parameters; a tail call enters its protocol, while a non-tail call nests its state under `In<G>At<N>`
- never call a yielding function from a function that does not yield (the error is `'loop' yields; call '__loopStart(...)' and answer its requests`), and never call an answered operation from a function that does not yield (`error[intercept-outside-yield]`)
- a yielding function calling itself outside tail position is an error. Pass what comes next as data, or make it a tail call
- stops may sit in bindings, as match subjects, inside arguments and inside match arms, and `?` after a request works. Mutual nesting, a request or a helper call inside `(a, b)!`, and a function value live across a request are rejected by name

Where it runs:
- the VM: `aver run main.av --module-root .`. A job runs on its own thread, `[work] max-jobs` bounds how many run at once, and at the limit `begin` queues the job and answers its handle; it never refuses and never blocks the turn
- `--target rust`: the same loop as a native binary. A job runs on a thread of `aver-rt`, and `Work.cancel` detaches the job without stopping it, so a cancelled job holds its slot until its body ends
- wasm-gc: the runner and Wasmtime packs execute jobs on host threads, bound them by `max-jobs` with the same queue, and support cancellation. Raw modules expose `aver:work/v1`; the JavaScript adapter uses workers and drives the generated loop between waits. See `docs/wasm-work.md`.
- wasip2: jobs currently run inline at `begin`, and `max-jobs` is ignored with `warning[work-max-jobs-ignored]`. There `take` never answers `Ok(None)`, a job that never ends blocks the turn, and a job whose body fails stops the component. The generated loop keeps `Run.View.stopping = false` and ends through `stop` or once nothing is seated; explicit `Process.stopRequested` calls are rejected.

**`max-jobs` is a deployment knob.** At the limit `begin` queues the job and answers its handle; the job starts, in the order it was begun, when a running body stops. So the same program gives the same answers under any limit, and a recording replays under any limit. `Work.cancel` takes a queued job out of the queue so it never starts. A running job that is cancelled keeps its place until its body stops: the VM stops it at its next cancellation check, wasm-gc at the next epoch check, the JavaScript adapter at once, and generated Rust checks no flag, so there the body runs to completion and the jobs queued behind it wait. `wasip2` has nothing to size.

**Driving the protocol by hand.** The generated names are compiler-defined and callable, so a program whose own `main` does not call `Run.all()` can drive a process itself. For `loop` the compiler generates, in the same module: `__LoopClaimState` (one state sum per request kind, one variant per stop, holding the live variables), `__LoopYieldState`, `__LoopRequest` (one constructor per kind: the operation's arguments plus the state), `__LoopOutcome = Done(<result>) | Waiting(__LoopRequest)`, `__loopStart(<params>)`, `__loopAnswerClaim(__state, __answer)` per kind (state only when the operation returns `Unit`) and `__loopAnswerYield(__state)`. The original function is removed:

```aver
fn loop(id: Int, done: Int) -> Int
    ? "Claims handles for id until the pool answers None, summing the handles into done."
    ! [Pool.claim, yield]
    r = Pool.claim(id)
    match r
        Option.None -> done
        Option.Some(h) -> loop(id, done + h)

fn drive(outcome: __LoopOutcome, answers: List<Option<Int>>) -> Int
    ? "Coordinator: answers every Claim request from the answers list (None once the list is empty), resumes every Yield request, and returns the final result."
    match outcome
        __LoopOutcome.Done(v) -> v
        __LoopOutcome.Waiting(request) -> match request
            __LoopRequest.Yield(state) -> drive(__loopAnswerYield(state), answers)
            __LoopRequest.Claim(peer, state) -> match answers
                [] -> drive(__loopAnswerClaim(state, Option.None), [])
                [answer, ..rest] -> drive(__loopAnswerClaim(state, answer), rest)
```

You can `verify` the generated names, match on them, and export them to Lean like any other item. A local cases-form `verify process` can also call its source name with exact `given` stubs for every request operation. That test drives the lowered protocol with operation results, runs on the VM, and uses the source name for case budgets. Direct process laws, trace blocks, WASM request stubs and proof export of these cases are not supported yet, so keep proof laws on the generated protocol. A module that exposes a yielding function exposes its protocol in its place, so an importer writes `Looper.__loopStart(...)`.

### Builtins and namespaces

Use namespaced builtins only.

Common pure namespaces:
- `Int`, `Float`, `String`, `List`, `Vector`, `Map`, `Bool`, `Bits`, `Crypto`, `Result`, `Option`

`Bytes` and `Crypto.Digest32` are embedded Aver modules. With `depends [Bytes, Crypto.Digest32]`, `Crypto.sha256 : Bytes -> Digest32` is total and pure: the input already guarantees octets, and the result guarantees exactly 32 bytes.

- `Bytes.fromList : List<Int> -> Result<Bytes, String>` (a list literal whose every element is an integer literal in `0..=255` discharges to plain `Bytes`), `Bytes.octets : Bytes -> List<Int>`
- `Bytes.fromHex : String -> Result<Bytes, String>` (even length, case-insensitive, no `0x` prefix), `Bytes.toHex : Bytes -> String`
- `Crypto.Digest32.fromBytes : Bytes -> Result<Digest32, String>`, `Crypto.Digest32.bytes : Digest32 -> Bytes`
- `Crypto.Digest32.fromHex : String -> Result<Digest32, String>`, `Crypto.Digest32.toHex : Digest32 -> String`

Key `String` API:
- `String.len`, `String.contains`, `String.startsWith`, `String.endsWith`
- `String.byteLength : String -> Int` is the UTF-8 byte count; `String.len` counts characters (Unicode scalar values), on every backend
- `String.charAt : (String, Int) -> Option<String>`, `String.slice : (String, Int, Int) -> String` take character indices; a slice with an out-of-range end clamps
- `String.toUpper`, `String.toLower`, `String.trim`, `String.replace : (String, String, String) -> String`
- `String.join`, `String.split`, `String.chars`; concatenation is the `+` operator
- `String.toUtf8 : String -> Bytes`, `String.fromUtf8 : Bytes -> Result<String, String>`: explicit, lossless encoding and validated decoding
- `Int.fromString : String -> Result<Int, String>`, `String.fromInt : Int -> String`
- `Int.toBigEndian`, `Int.toLittleEndian : (Int, Int) -> Result<Bytes, String>`; `Int.fromBigEndian`, `Int.fromLittleEndian : Bytes -> Int`
- `Float.fromString`, `String.fromFloat`, `String.fromBool`. The naming convention is `<targetTyp>.from<source>`
- string interpolation: `"Hello, {name}!"` is the idiomatic way to render PRIMITIVES into text. Reserve `String.fromInt` etc. for explicit data conversion (e.g. building keys: `"user:" + String.fromInt(id)`). Compound values have no built-in rendering, so write your own `fn show(x: T) -> String`.

Key code-point API:
- `String.firstCodePoint : String -> Option<Int>`: the first Unicode scalar value, `Option.None` for empty text
- `String.fromCodePoint : Int -> Option<String>`: a code point to a 1-character string, `Option.None` for surrogates and out-of-range values

Key `Int` API:
- `Int.abs : Int -> Int`; `Int.min`, `Int.max` are `(Int, Int) -> Int`
- `Int.div`, `Int.mod`: `(Int, Int) -> Result<Int, String>`, Euclidean, see the operators section above
- `Int.fromString : String -> Result<Int, String>`, `Int.fromFloat : Float -> Int`
- `Int.toBigEndian`, `Int.toLittleEndian : (Int, Int) -> Result<Bytes, String>`; `Int.fromBigEndian`, `Int.fromLittleEndian : Bytes -> Int`

Key `Float` API:
- `Float.abs`, `Float.sqrt`, `Float.sin`, `Float.cos`: `Float -> Float`; the trig functions take radians
- `Float.pow`, `Float.atan2`, `Float.min`, `Float.max`: `(Float, Float) -> Float`
- `Float.floor`, `Float.ceil`, `Float.round`: `Float -> Int`, so they convert as well as round
- `Float.pi : () -> Float` is a nullary function, written `Float.pi()`
- `Float.fromInt : Int -> Float`, `Float.fromString : String -> Result<Float, String>`

Key `Result` / `Option` API:
- `Result.withDefault : (Result<T, E>, T) -> T`, `Option.withDefault : (Option<T>, T) -> T`
- `Result.fromOption : (Option<T>, E) -> Result<T, E>` is the bridge that lets an `Option` join a `?` chain

Key `List` API (small, recursion-first):
- `List.len`, `List.prepend`, `List.concat`, `List.reverse`, `List.contains`, `List.zip`, `List.take`, `List.drop`, `List.fromVector`
- there is no `List.map`, `List.filter`, `List.fold`. Write these with recursion
- empty list literal: `[]`

Key `Vector` API (O(1) indexed access):
- `Vector.new(n, default) -> Result<Vector<T>, String>` for a dynamic size; a syntactic literal in the portable `0..=1_048_576` element budget discharges directly to `Vector<T>`
- `Vector.get(v, i) -> Option<T>`, `Vector.set(v, i, val) -> Option<Vector<T>>`
- `Vector.len(v) -> Int`
- `Vector.fromList(l)`; conversion in the other direction lives on `List`

Key `Map` API:
- a map iterates sorted by key on every backend, so the key type must order. Records (by field name), variants (by constructor name), lists, tuples and `Bytes` all key a map fine. `Float` cannot, because a NaN has no place in the finite range, and neither can `Map` or `Vector`. Float stays legal as a *value*.
- empty map literal: `{}` (type from context); non-empty: `{a => 1, b => 2}`. There is no `Map.empty()` builtin.
- `Map.fromList(pairs)`, `Map.get(m, k) -> Option<V>`, `Map.set(m, k, v)`, `Map.has(m, k)`, `Map.remove(m, k)`, `Map.keys(m)`, `Map.values(m)`, `Map.entries(m)`, `Map.len(m)`

Effectful namespaces:
- `Console`: print, error, warn, readLine. **`print`/`error`/`warn` take `String`**, not arbitrary values. Stringify at the call site: interpolation `"{x}"` for primitives, a per-type render fn (`fn show(r: Result<T, E>) -> String`) for compound shapes.
- `Http`: get, post, put, patch, delete, head
- `Disk`: readText, writeText, appendText, readBytes, readBytesAt, writeBytes, appendBytes, size, exists, delete, deleteDir, listDir, makeDir, sync. The byte methods move exact octets as nominal `Bytes`. `Disk.sync(path)` is `fsync`: it returns once the named file *or directory* is on stable storage, so a freshly created file needs its parent directory synced too
- `Tcp`: connect, writeLine, writeBytes, writeNow, readLine, readBytes, readSome, readNow, poll, close, send, sendBytes, ping. `send`/`readLine` are text-only (UTF-8); binary payloads use nominal `Bytes` through `sendBytes`, `writeBytes`, and `readBytes`. `readNow`/`writeNow` never block (`None` or a `0` count means the socket would block), and `poll` takes `Tcp.Socket.Connected` for read readiness or `Tcp.Socket.Sending` for write readiness
- `Terminal`: every operation is fallible. Control/output calls return `Result<Unit, String>`, `readKey` returns `Result<Option<String>, String>`, and `size` returns `Result<Terminal.Size, String>`. The cursor move is `Terminal.moveTo(x, y)`; `Terminal.print` / `Terminal.setColor` take `String`.
- `Time`: now, unixMs, sleep. `Time.sleep(ms) -> Result<Unit, String>` for a dynamic duration; a valid non-negative i64 literal discharges directly to `Unit`
- `Random`: int, float. `Random.int(lo, hi) -> Result<Int, String>` is inclusive on both ends; safe literal bounds discharge directly to `Int`. `Random.float()` is in `[0.0, 1.0)`
- `Env`: get, set
- `Args`: get

Incoming HTTP is not an effect namespace. Use pure `HttpWire`, native `HttpServe` over `Tcp` + `Process.stopRequested`, or an explicit `--handler <fn>` on fetch/proxy hosts.

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

Bounded outbox for `Tcp.writeNow`. A non-blocking write takes a prefix, so the program carries the queue: the payload at the head, how many of its bytes have gone, and the rest behind it. Register the `Sending` key only while bytes remain, and when the outbox is full, make the slow peer pay for it. A bulk producer (one request answered by many large payloads) must not enqueue them all at once. It keeps the list of what was asked for and renders the next payload once the queue is under its watermark, so a healthy peer is not dropped for reading slower than the program produces.

```aver
type Watch
    Peer(Int)
    Write(Int)
    Listener

record Outbox
    head: Bytes
    queue: List<Bytes>
    asked: List<Int>

fn flush(connection: Tcp.Connection, outbox: Outbox) -> Result<Outbox, String>
    ? "One non-blocking write from the head; what the socket did not take stays as data."
    ! [Tcp.writeNow]
    accepted = Tcp.writeNow(connection, outbox.head)?
    rest = Bytes.drop(outbox.head, accepted)
    match Bytes.len(rest) > 0
        true -> Result.Ok(Outbox.update(outbox, head = rest))
        false -> match outbox.queue
            [] -> Result.Ok(Outbox.update(outbox, head = Bytes.empty()))
            [next, ..later] -> Result.Ok(Outbox.update(outbox, head = next, queue = later))

fn interest(peer: Int, connection: Tcp.Connection, outbox: Outbox, items: Map<Watch, Wait.Item>) -> Map<Watch, Wait.Item>
    ? "Ask for writability only while the outbox still holds bytes. A wait set is keyed by what the program is waiting for, so a write interest and the peer's own readability are two constructors rather than two integers that must not collide."
    match Bytes.len(outbox.head) > 0
        true -> Map.set(items, Watch.Write(peer), Wait.Item.Socket(Tcp.Socket.Sending(connection)))
        false -> items

fn topUp(outbox: Outbox) -> Outbox
    ? "Render the next payload the peer asked for only under the watermark."
    match List.len(outbox.queue) < 4
        false -> outbox
        true -> match outbox.asked
            [] -> outbox
            [next, ..later] -> Outbox.update(outbox, queue = List.concat(outbox.queue, [render(next)]), asked = later)
```

### Common mistakes to avoid

1. Writing `.aver` files instead of `.av`
2. `if`/`else`: use `match`
3. `val`/`var`/`let`: write `name = expr`
4. Bare `Ok(x)`: it must be `Result.Ok(x)`
5. `String.toInt(s)` does not exist. Use `Int.fromString(s)`, which returns `Result<Int, String>`
6. Assuming `Console.readLine()` returns `String`: it returns `Result<String, String>`
7. Writing `=` instead of `=>` in verify cases. The separator is always `=>`
8. Using `..` without a name in list patterns: write `[h, ..t]`, not `[h, ..]`
9. Missing `!` effect declaration: the compiler errors
10. Closures/lambdas: not supported. Use named top-level functions
11. Mutable variables: not supported; all bindings are immutable
12. `List.map`/`List.filter`/`List.fold`: not built-in. Write them with recursion
13. Pipe `|>`: not supported
14. Positional record destructuring in match: bind the record and use field access
15. Multi-line match arms: the body must follow `->` on the same line. Extract complex logic into a named function
16. `BranchPath.Root()` / `BranchPath.root()`: it is a nullary value constructor with no parens. Write `BranchPath.Root`
17. Two `given` for the same effect: rejected. Use a multi-value domain `given rnd: Random.int = [stubA, stubB]` for varied samples
18. Plain `verify fn` on a fn with generative effects: you get a lint warning. Use `verify fn law …` with `given` stubs or `verify fn trace` instead
19. `()` as a Unit value literal: there is no `()` literal. Write `Unit`. Diagnostics render the value as `()`, but in source the only spelling is `Unit` (matching `Map<T, Unit>` set semantics, `Unit` field annotations, etc.). `Console.print(...)` returns Unit implicitly, so you almost never write the literal directly
20. `expr?` on an `Option<T>`: `?` is Result-only. For `Option`, use `Option.withDefault(opt, fallback)`, or `match opt { Option.Some(v) -> … ; Option.None -> … }`. `Vector.get` and `Map.get` return `Option`, so neither composes with `?` directly; wrap the value first
21. `(A, B)` as a tuple type: type position uses `Tuple<A, B>` exclusively. Tuple **value** literals stay paren: `(1, 2)`, `[(1, 2), (3, 4)]`, `Result.Ok((a, b))`. Tuple **patterns** stay paren: `match p { (a, b) -> … }`. The type and the value spelling are deliberately different, so grep-for-type and grep-for-value don't collide
22. Dispatching on a `String` or an `Int` through a chain of `match x == "lit"` with `true ->` / `false ->` helper functions. Literal patterns exist: `match cmd { "verack" -> 1 ; "tx" -> 4 ; _ -> 0 }`. Only the trailing `_` arm is mandatory
23. Keying a map on a `Float`: the key type has to order, and a NaN has no place in the finite range. Key on the value that orders (`Map<String, Reading>` rather than `Map<Float, Reading>`). A `Map` or a `Vector` in key position is refused for the same reason: neither has an order of its own

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
- functions longer than ~30 lines; split them into named helpers
