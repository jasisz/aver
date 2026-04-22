# Oracle v1 — release design plan

> Status: design, pre-implementation
> Release codename: **Oracle**
> Next release (deferred scope): **Relay** — `?!` cancel mode + cross-branch trace ordering
> Later release: **Ledger** — stateful effects (Store<K,V>) via ghost Map + refinement to trace

## Pitch (one sentence)

Effectful functions become first-class in `aver proof`: previously silently skipped, they are now lifted to pure form with per-dimension semantics (snapshot / generative / output) and branch-witness tree for effectful `!` and `?!` (complete mode). Built-in effects are classified by Aver authors, not by user. User surface barely changes.

## What changes for users (only two new conventions)

1. **Spec for an effectful function takes effects as explicit parameters** — reader functions for snapshot dims, oracle functions for generative dims.
2. **`verify fn trace` keyword** enables trace-aware laws using `.result` / `.trace` projections. Without it, law checks only return value (so adding a debug print does not break proofs that do not care about traces).

Everything else is identical to today: `! [Effect.method]` in function body, `verify fn law fnSpec`, `given` sample values, `!` / `?!` in source.

## Scope

### In

- **Snapshot** effects → capability threading: single reader function param, deterministic.
- **Generative** effects → oracle + counter threading: `(Int, args...) -> T` oracle, integer counter advanced per call.
- **Output** effects → trace threading: ghost `List<EffectCall>` appended per call, opt-in in law via `trace` keyword.
- **Effectful `!`** (parallel independence) → branch-witness tree.
- **Effectful `?!`** in `complete` mode → same witness tree, plus left-to-right error priority aggregation.
- **`Result` / `?` preservation** — ghost state (counter, trace) survives early return.
- **Structural trace addressing** matching replay JSON format: `group_id` + `branch_path` + `effect_occurrence`.

### Out (with clear rejection messages)

- **`?!` in `cancel` mode** — proofs assume `complete`; if `aver.toml` has `mode = cancel`, emit stderr warning.
- **Stateful effects** (Store, DB, any effect where write affects subsequent read) — replay only; Ledger release.
- **Higher-order effectful callbacks** (e.g. `HttpServer.listenWith(handler: Req -> ! [...] Resp)`) — proof subset rejection; runtime and replay unaffected.
- **Interactive / dialogue effects** (request-response protocols, subprocess stdin/stdout, LLM tool calls) — replay only.
- **Cross-branch global ordering laws** ("event A before event B across two `!` branches") — structurally inexpressible; explicit rejection. Within-branch ordering is fine.

## Effect classification (built-in, not user-declared)

Each built-in effect method is assigned one or more dimensions. This is a language-author decision, codified in the compiler. Users never declare dimensions.

| Effect | Method | Dimension(s) | Notes |
|---|---|---|---|
| `Args` | `get : Int -> String` | snapshot | stable within a run |
| `Env` | `get : String -> Option<String>` | snapshot | stable within a run |
| `ProjectConfig` | `get : String -> Option<Value>` | snapshot | config read once at startup |
| `Random` | `next : () -> Int` | generative | fresh value per call |
| `Time` | `now : () -> Int` | generative | non-deterministic |
| `File` | `read : String -> String` | generative | live FS; value may change between calls |
| `Console` | `print : String -> Unit` | output | trace-appending |
| `Log` | `write : (Level, String) -> Unit` | output | trace-appending |
| `Http` | `get : String -> String` | generative + output | request emitted to trace, response arbitrary |

Effects marked **stateful** or **interactive** (e.g. `Store.read/write`, `Tcp.requestResponse`) are not in this table — they remain replay-only. Classification for every built-in effect is locked in before release.

## User-facing examples

### 1. Pure function — unchanged from today

```aver
fn absVal(x: Int) -> Int
    ? "Returns absolute value."
    if x < 0 then -x else x

verify absVal law absValSpec
    given x: Int = -3..3
    absVal(x) => absValSpec(x)
```

### 2. Snapshot reader — Args.get

```aver
fn loadPort() -> Result<Int, String>
    ! [Args.get]
    parseInt(Args.get(1))

fn loadPortSpec(args: Int -> String) -> Result<Int, String>
    ? "Parse port from arg[1]."
    parseInt(args(1))

fn argsA(i: Int) -> String
    match i
        0 -> "myprogram"
        1 -> "8080"
        _ -> ""

fn argsB(i: Int) -> String
    match i
        0 -> "myprogram"
        1 -> "xyz"
        _ -> ""

verify loadPort law loadPortSpec
    given Args.get: Int -> String = [argsA, argsB]
    loadPort() => loadPortSpec(Args.get)
```

`doubleCheck(Args.get(0), Args.get(0))` proves `true` — correct, because args are stable within a run.

### 3. Generative — Random.next

```aver
fn pickThree() -> (Int, Int, Int)
    ! [Random.next]
    (Random.next(), Random.next(), Random.next())

fn pickThreeSpec(oracle: Int -> Int) -> (Int, Int, Int)
    ? "Three draws from the oracle."
    (oracle(0), oracle(1), oracle(2))

verify pickThree law pickThreeSpec
    given Random.next: Int -> Int = [seedA, seedB]
    pickThree() => pickThreeSpec(Random.next)
```

`doubleCheck(Random.next(), Random.next())` does **not** prove `true` — different indices, oracle may return different values.

### 4. Output — default (trace ignored)

```aver
fn greetAll(names: List<String>) -> Int
    ! [Console.print]
    names.each(n => Console.print("Hello, {n}!"))
    names.length()

verify greetAll
    greetAll([])                => 0
    greetAll(["Alice"])         => 1
    greetAll(["Alice", "Bob"])  => 2
```

No spec needed when impl is simple and only the return value matters. Adding a debug print does not break this proof.

### 5. Output — trace-aware via `trace` keyword

```aver
verify greetAll trace
    greetAll(["Alice"]).result                                           => 1
    greetAll(["Alice"]).trace.length()                                   => 1
    greetAll(["Alice"]).trace.contains(Console.print("Hello, Alice!"))   => true

verify greetAll trace law greetAllSpec
    given names: List<String> = [[], ["Alice"], ["Alice", "Bob"]]
    greetAll(names).result          => greetAllSpec(names)
    greetAll(names).trace.length()  => names.length()
```

The effect call `Console.print("Hello, Alice!")` used as a value inside `.trace.contains(...)` is the event literal — no separate event ADT for users to learn.

### 6. Parallel `!` — trace addressing matches replay JSON

Source:
```aver
fn fanOut(a: String, b: String) -> Unit
    ! [Console.print]
    Console.print("fetch outer-" + a) !
    Console.print("fetch outer-" + b) !
    {
        Console.print("fetch inner-A") !
        Console.print("fetch inner-B") !
    }
    Console.print("all fan-outs completed")
```

Replay JSON (from playground) shows:
```json
{"group_id":1,"branch_path":"0",  "effect_occurrence":0,"type":"Console.print","args":["fetch outer-1"]}
{"group_id":1,"branch_path":"1",  "effect_occurrence":0,"type":"Console.print","args":["fetch outer-2"]}
{"group_id":2,"branch_path":"2.0","effect_occurrence":0,"type":"Console.print","args":["fetch inner-A"]}
{"group_id":2,"branch_path":"2.1","effect_occurrence":0,"type":"Console.print","args":["fetch inner-B"]}
```

Law uses the same coordinates (copy-paste from replay JSON):
```aver
verify fanOut trace
    given a: String = ["1"], b: String = ["2"]
    fanOut(a, b).trace[1]["0"][0]   => Some(Console.print("fetch outer-1"))
    fanOut(a, b).trace[1]["1"][0]   => Some(Console.print("fetch outer-2"))
    fanOut(a, b).trace[2]["2.0"][0] => Some(Console.print("fetch inner-A"))
    fanOut(a, b).trace[2]["2.1"][0] => Some(Console.print("fetch inner-B"))
    fanOut(a, b).trace.sequential(4) => Some(Console.print("all fan-outs completed"))
```

Indexing: `trace[group_id][branch_path][effect_occurrence]`. Sequential (non-grouped) events addressed via `.sequential(n)`.

### 7. Fork-join `?!` complete mode

```aver
fn parseEither(raw: String) -> Result<(Config, Config), Error>
    ! [Parser.run]
    a = Parser.run("json", raw) ?!
    b = Parser.run("yaml", raw) ?!
    Ok((a, b))

fn parseEitherSpec(
    raw: String,
    parser: (String, String) -> Result<Config, Error>
) -> Result<(Config, Config), Error>
    ? "Try both parsers; first source-order Err wins."
    match (parser("json", raw), parser("yaml", raw))
        (Ok(a), Ok(b))  -> Ok((a, b))
        (Err(e), _)     -> Err(e)
        (Ok(_), Err(e)) -> Err(e)

verify parseEither law parseEitherSpec
    given raw: String = ["{\"port\": 8080}"]
    given Parser.run: (String, String) -> Result<Config, Error> = [goodParser]
    parseEither(raw) => parseEitherSpec(raw, Parser.run)
```

### 8. Mixed dimensions in one function

```aver
fn report(region: String) -> Unit
    ! [Args.get, Random.next, Console.print]
    prefix = Args.get(0)
    id     = Random.next()
    Console.print(prefix + " report " + toString(id) + " for " + region)

fn reportSpec(
    region: String,
    args: Int -> String,
    oracle: Int -> Int
) -> Unit
    ? "Report builds message from arg[0], a draw from oracle, and region."
    let _ = args(0) + " report " + toString(oracle(0)) + " for " + region
    Unit

verify report trace law reportSpec
    given region: String              = ["PL"]
    given Args.get: Int -> String     = [argsStub]
    given Random.next: Int -> Int     = [randStub]
    report(region).result              => reportSpec(region, Args.get, Random.next)
    report(region).trace.length()      => 1
```

## Codegen (internal, invisible to user)

### BranchWitness ADT

```
data BranchWitness
    = Leaf { counter: Int, trace: List<EffectCall> }
    | Parallel(List<BranchWitness>)      // for ! and for ?! complete
    | Sequence(List<BranchWitness>)      // for ;
```

`Parallel` serves both `!` and `?!` complete. In `complete` mode all branches run to completion; the only difference is the aggregation rule at join.

### Aggregation at join

- `!` block: standard tuple / unit aggregation.
- `?!` complete block:
  ```
  match branchResults
      all Ok  -> Ok(tuple_of_values)
      any Err -> first Err in source order
  ```

### Threading rules per dimension

- **Snapshot**: capability function parameter, unchanged through evaluation.
- **Generative**: `(Int, args) -> T` oracle parameter + `Int` counter; counter incremented after each call.
- **Output**: `List<EffectCall>` trace parameter, appended per call.
- **`Result` / `?` preservation**: lifted function returns `(Result<A, E>, counter, trace)`, so error paths preserve ghost state.

### Structural trace normalization

For any two legal runtime schedules of the same evaluation, the normalized structural trace is identical. This is the fundamental soundness lemma for trace-aware laws under `!` / `?!` complete. It is either proven in-compiler or treated as a definitional axiom of proof-trace semantics (decision deferred to implementation phase).

### Trace addressing model

- `group_id`: structural order of `!` / `?!` encounter within a function body (1-indexed, source-derived).
- `branch_path`: dewey-decimal string like `"2.0"` encoding the hierarchical branch position from the innermost enclosing group outward.
- `effect_occurrence`: 0-indexed counter within the branch.
- Sequential (non-grouped) effects: addressed via `.sequential(n)` in the proof API.

`group_id` is included in the proof API despite being structurally derivable, because:
- Sibling groups at the same nesting level need disambiguation.
- User already sees it in replay JSON; consistency between replay view and proof asserts matters.

## Trust assumptions (generated header in each `.lean` / `.dfy`)

```
// Trusted model assumptions for this Aver proof export:
//
// Effects and dimensions:
//   Args.get      — snapshot: stable return for same input within run
//   Random.next   — generative: each call returns arbitrary Int; no correlation assumed
//   Console.print — output: invocations appear in trace in source-structural order
//   ...
//
// Concurrency:
//   !  (independent parallel): proof holds for any legal schedule
//   ?! in complete mode: all branches run; error aggregated left-to-right
//   ?! in cancel mode: NOT COVERED (see aver.toml mode setting)
//
// Structural trace addressing:
//   Events addressed by (group_id, branch_path, effect_occurrence)
//   Cross-branch ordering NOT observable
//   Wall-clock and shared-channel adjacency NOT expressible
//
// Out of scope in this export:
//   - Stateful effects
//   - Higher-order effectful callbacks
//   - Interactive protocols
```

## Rejection diagnostics

```
error: function 'transferMoney' uses stateful effect 'Store.write'
       Stateful effects are not in Oracle v1's proof subset.
       Use replay (aver record / aver replay) for this function.
       Stateful proof support is planned for Ledger release.

error: function 'setupServer' uses higher-order effectful callback
         Handler: Request -> ! [...] Response
       Higher-order effectful callbacks are outside the proof subset.
       The function continues to work at runtime and in replay.

warning: project uses mode = cancel in aver.toml
         Proofs exported by aver proof assume mode = complete.
         Switch to mode = complete, or accept that exported proofs do not cover cancel semantics.

error: cross-branch ordering assertion in verify block:
         foo().trace["0"][0]._at_seq < foo().trace["1"][0]._at_seq
       Global ordering across independent branches is not observable in Oracle v1.
       Assert per-branch or switch to sequential ';'.
```

## Scope estimate

| Component | Effort |
|---|---|
| BranchWitness ADT + parser / AST support | 2 days |
| Codegen per-dimension lift (snapshot / generative / output) | 1.5 weeks |
| `Result` / `?` preservation across ghost state | 3 days |
| Branch witness tree construction for `!` and `?!` complete | 1 week |
| Aggregation rule for `?!` at join point | 2 days |
| `verify fn trace` keyword + trace-aware law parser | 3 days |
| Trace addressing API (`group_id` / `branch_path` / `effect_occurrence`) | 3 days |
| Dafny emission | 1 week |
| Lean emission | 1 week |
| Rejection diagnostics + trust-assumption header generator | 3 days |
| Normalization lemma / axiom (structural trace schedule-invariance) | 3 days |
| Tests + migration examples | 1 week |
| Docs | 3 days |

**Realistic total: 6–8 weeks focused work.**
**Pessimistic: 10 weeks** if normalization or `?!` aggregation prove subtler than expected.

## Risks

1. **Normalization theorem** for structural trace soundness: formulation may need care; subtleties may surface mid-implementation.
2. **Dafny solver cost** for deep branch nesting: N=2 fine; N=5+ nested `!` / `?!` unclear.
3. **Spec-vs-impl brittleness** for generative dim: counter alignment means spec and impl must consume oracle identically. A UX cliff for refactor-sensitive users.
4. **Debug prints affecting trace-aware laws** — documented as expected; some users may still find it surprising.
5. **`group_id` source-stability for recursion**: each recursive call generates a new group instance; proof API uses "order of encounter in function body" which is stable across non-recursive code but needs clear semantics for recursive fan-out (Aver repo already exercises this via playground).

## Follow-up releases

- **Relay** — adds `?!` cancel mode with cooperative cancellation semantics, cross-branch trace ordering via merge witnesses, and higher-order effectful callbacks.
- **Ledger** — stateful effects (Store<K,V>) via ghost `Map<K, V>` + refinement relation tying runtime replay trace to the abstract model. Introduces relation-based proofs ("proved relative to trusted Store laws / host adapter").

## Prior art

Design draws on Effekt's "effects as capabilities" framework (Brachthäuser et al.). Credit in release notes; no claim of novelty on the elaboration technique. Novelty is the product integration: a single capability abstraction drives proof export, replay artifacts, and WASM host interface in a consistent shape.
