# Oracle v1 — release design plan

> Status: design, pre-implementation
> Release codename: **Oracle**
> Next release (deferred scope): **Relay** — `?!` cancel mode, cross-branch trace ordering, higher-order effectful callbacks, user-defined effects
> Later release: **Ledger** — stateful effects (Store<K,V>) via ghost Map + refinement

## Pitch (one sentence)

Effectful functions become first-class in `aver proof`: previously silently skipped, they are now lifted to pure form with per-dimension semantics (snapshot / generative / output) and branch-witness tree for effectful `!` and `?!` (complete mode). Built-in effects are classified by Aver authors, not by user. User surface barely changes.

## What changes for users (only two new conventions)

1. **Spec for an effectful function takes effects as explicit parameters** — reader functions for snapshot dims, branch-indexed oracle functions for generative dims.
2. **`verify fn trace` keyword** enables trace-aware laws using `.result` / `.trace` projections. Without it, law checks only the return value (so adding a debug print does not break proofs that do not care about traces).

Everything else is identical to today: `! [Effect.method]` in function body, `verify fn law fnSpec`, `given` sample values, `!` / `?!` in source.

## Foundational model: structural path

Every Aver evaluation runs in a structural context path `BranchPath`:

- Sequential code: root path (`[]` or empty dewey-decimal string `""`).
- Entering branch `j` of a `!` or `?!` group: path extended with `j` (e.g. `[]` → `[0]`, `[0]` → `[0, 0]`).
- Nested groups extend the path further.

BranchPath is **source-derived and schedule-invariant**. Every effect occurrence has a canonical address `(BranchPath, occurrence_in_branch)`. There is no "branch path only in parallel" — sequential code uses the root path uniformly.

This model is the foundation of generative oracle signatures, trace addressing, and the normalization theorem below.

## Scope

### In

- **Snapshot** effects → capability threading: single reader function param, deterministic, not branch-indexed.
- **Generative** effects → oracle with branch-indexed signature `(BranchPath, Int, args...) -> T` plus per-branch integer counter advanced per call.
- **Output** effects → per-branch trace segment threading: ghost `List<EffectCall>` appended per call; structural tree assembled at join. Opt-in in law via `trace` keyword.
- **Effectful `!`** (parallel independence) → branch-witness tree with structural branch paths.
- **Effectful `?!`** in `complete` mode → same witness tree, plus left-to-right error priority aggregation.
- **`Result` / `?` preservation** — ghost state (counter, trace) survives early return.
- **Trace API** using the same three coordinates as the replay JSON (`group_id`, `branch_path`, `effect_occurrence`) so proof laws and recordings speak the same addressing.

### Out (with clear rejection)

- **`?!` in `cancel` mode** — hard error from `aver proof` unless explicit opt-in flag.
- **Stateful effects** (Store, DB, any effect where write affects subsequent read) — replay only; Ledger release.
- **Higher-order effectful callbacks** (e.g. `HttpServer.listenWith(handler: Req -> ! [...] Resp)`) — proof subset rejection; runtime and replay unaffected.
- **Interactive / dialogue effects** (request-response protocols, subprocess stdin/stdout, LLM tool calls) — replay only.
- **Cross-branch global ordering laws** ("event A before event B across two `!` branches") — structurally inexpressible; explicit rejection. Within-branch ordering is fine.
- **Custom (non-built-in) effects** — rejected from proof subset in Oracle v1; Relay release will add user-defined effect classification.

## Effect classification (built-in, closed set for Oracle v1)

Oracle v1 only covers functions whose effects all appear in this table. Classification is fixed by Aver language authors, not declared by user.

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

Effects marked **stateful** or **interactive** (`Store.read/write`, `Tcp.requestResponse`, `Db.*`, etc.) are not in this table — they remain replay-only. Full classification for every built-in effect is locked in before release.

## User-facing examples

### 1. Pure function — unchanged from today

```aver
fn absVal(x: Int) -> Int
    ? "Returns absolute value."
    match x < 0
        true  -> 0 - x
        false -> x

fn absValSpec(x: Int) -> Int
    ? "Reference."
    match x < 0
        true  -> 0 - x
        false -> x

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

`doubleCheck(Args.get(0), Args.get(0))` proves `true` — correct, because args are stable within a run. Snapshot capability is a pure extensional function; same input → same output.

### 3. Generative — Random.next (branch-indexed oracle)

```aver
fn pickThree() -> (Int, Int, Int)
    ! [Random.next]
    (Random.next(), Random.next(), Random.next())

fn pickThreeSpec(oracle: (BranchPath, Int) -> Int) -> (Int, Int, Int)
    ? "Three draws from the oracle in the root branch."
    (oracle([], 0), oracle([], 1), oracle([], 2))

verify pickThree law pickThreeSpec
    given Random.next: (BranchPath, Int) -> Int = [seedA, seedB]
    pickThree() => pickThreeSpec(Random.next)
```

Sequential code — all three calls happen in the root path `[]`. Counter distinguishes them (0, 1, 2). `doubleCheck(Random.next(), Random.next())` does **not** prove `true` — same path, different counters → `oracle([], 0)` and `oracle([], 1)` may return different values.

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

### 6. Parallel `!` — trace API mirrors replay JSON

Source:
```aver
fn fanOut(a: String, b: String) -> Unit
    ! [Console.print]
    Console.print("fetch outer-" + a) !     // group 1, branch "0"
    Console.print("fetch outer-" + b) !     // group 1, branch "1"
    {                                        // group 1, branch 2 contains nested group
        Console.print("fetch inner-A") !    //   group 2, branch "2.0"
        Console.print("fetch inner-B") !    //   group 2, branch "2.1"
    }
    Console.print("all fan-outs completed") // sequential, no group
```

Replay JSON (from playground Trace view) looks like:
```json
{"group_id":1,"branch_path":"0",  "effect_occurrence":0,"args":["fetch outer-1"]}
{"group_id":1,"branch_path":"1",  "effect_occurrence":0,"args":["fetch outer-2"]}
{"group_id":2,"branch_path":"2.0","effect_occurrence":0,"args":["fetch inner-A"]}
{"group_id":2,"branch_path":"2.1","effect_occurrence":0,"args":["fetch inner-B"]}
{                                                        "args":["all fan-outs completed"]}
```

Law uses the same three coordinates — copy-paste from the JSON:
```aver
verify fanOut trace
    given a: String = ["1"], b: String = ["2"]
    
    fanOut(a, b).trace.group(1).branch("0").event(0)
        => Some(Console.print("fetch outer-1"))
    
    fanOut(a, b).trace.group(1).branch("1").event(0)
        => Some(Console.print("fetch outer-2"))
    
    fanOut(a, b).trace.group(2).branch("2.0").event(0)
        => Some(Console.print("fetch inner-A"))
    
    fanOut(a, b).trace.group(2).branch("2.1").event(0)
        => Some(Console.print("fetch inner-B"))
    
    fanOut(a, b).trace.event(4)
        => Some(Console.print("all fan-outs completed"))
```

Three addressing primitives matching the replay schema 1:1:
- `.group(N)` — enter the `!`/`?!` group with `group_id: N` (structurally assigned: first group encountered in the function body is 1, second is 2, …).
- `.branch("path")` — select a branch by its dewey-decimal `branch_path` from the replay JSON; nested groups give paths like `"2.0"`.
- `.event(k)` — the k-th `effect_occurrence` at the current node.
- `.event(k)` at the trace root addresses sequential events outside any group (no `.group()` prefix needed).

**Structural, not runtime**. `group_id` numbering reflects source-order-of-encounter in the function body, not a global runtime counter. The same source compiled at any time gives the same IDs, so proofs remain stable across runs.

### 7. Fork-join `?!` complete mode

```aver
fn fetchBoth(urlA: String, urlB: String) -> Result<(String, String), String>
    ! [Http.get]
    a = Http.get(urlA) ?!
    b = Http.get(urlB) ?!
    Ok((a, b))

fn fetchBothSpec(
    urlA: String,
    urlB: String,
    http: (BranchPath, Int, String) -> Result<String, String>
) -> Result<(String, String), String>
    ? "Try both URLs; first source-order Err wins."
    match (http([0], 0, urlA), http([1], 0, urlB))
        (Ok(a), Ok(b))  -> Ok((a, b))
        (Err(e), _)     -> Err(e)
        (Ok(_), Err(e)) -> Err(e)

verify fetchBoth law fetchBothSpec
    given urlA: String = ["https://a"], urlB: String = ["https://b"]
    given Http.get: (BranchPath, Int, String) -> Result<String, String> = [httpStub]
    fetchBoth(urlA, urlB) => fetchBothSpec(urlA, urlB, Http.get)
```

`Http.get` is generative + output. Oracle covers the response; output side appears in trace when the verify block uses `trace` keyword. In `?!` complete, both branches run to completion; aggregation takes the first `Err` in source order.

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
    oracle: (BranchPath, Int) -> Int
) -> Unit
    ? "Report returns Unit; side-effect visible in trace."
    Unit

verify report trace law reportSpec
    given region: String                        = ["PL"]
    given Args.get: Int -> String               = [argsStub]
    given Random.next: (BranchPath, Int) -> Int = [randStub]
    report(region).result                       => reportSpec(region, Args.get, Random.next)
    report(region).trace.event(0).is(Console.print) => true
```

(Return value of `report` is `Unit`, so `reportSpec` trivially returns `Unit`. Real content of this law is in the trace assertion — exactly one event emitted at statement 2.)

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

- **Snapshot**: capability function parameter, unchanged through evaluation; NOT branch-indexed (stable across branches by definition).
- **Generative**: `(BranchPath, Int, args...) -> T` oracle parameter + **per-branch** `Int` counter. Counter resets at branch entry; BranchPath advances on branch entry.
- **Output**: per-branch `List<EffectCall>` trace segment appended per call; structural tree assembled from per-branch segments at join.
- **`Result` / `?` preservation**: lifted function returns `(Result<A, E>, counter, trace)`, so error paths preserve ghost state.

### Structural trace normalization (theorem, not axiom)

**Theorem (schedule-invariance of structural trace)**:

For any two legal runtime schedules `s1, s2` of the same Aver evaluation `e`:
```
normalize(runtime_trace(e, s1)) = normalize(runtime_trace(e, s2))
```
where `normalize` produces the structural trace indexed by `(BranchPath, occurrence_in_branch)`.

**Proof sketch**:
- For `!`: follows from the `!` annotation meaning "operations commute on state". Different orderings of commuting operations produce the same final state per branch, and since each branch maintains its own trace segment, normalization is order-independent.
- For `?!` complete: all branches run to completion (no cancellation); each branch's local trace is determined by its own execution independently of other branches' scheduling. Aggregation at join operates on completed branch Results and is schedule-invariant.
- For `;`: trivial — single ordering.

This is a theorem, not an axiom. It must be proven (either as a meta-lemma in the compiler, or emitted as a lemma-scheme that each generated proof references). A user inspecting an exported `.lean` / `.dfy` can either trust or mechanically verify it.

### Trace addressing

Three primitives matching the replay schema 1:1 (`group_id`, `branch_path`, `effect_occurrence`):

- `.group(N)` — enter the `!` / `?!` group whose `group_id` is `N` (source-structural: 1 for the first group encountered in the body, 2 for the second, …; unaffected by runtime).
- `.branch("path")` — select a branch by its dewey-decimal `branch_path` as it appears in replay JSON (e.g. `"0"`, `"1"`, `"2.0"` for nested).
- `.event(k)` — the k-th `effect_occurrence` at the current node.
- `.event(k)` at the trace root addresses sequential events outside any group.
- `.length()` — number of events at the current node (not recursive).

**Structural, not runtime**. `group_id` in the proof API means "order of encounter in source", so the same program recompiled gives the same IDs and proofs stay stable. Recursion — where one source group produces multiple runtime instances — is an open concern tracked with the other recursion-related risks.

**Recordings of any entry point share the addressing**: a trace captured via `aver run -e 'area(Shape.Circle(1.0))' --record dir/` (0.10.1) populates `group_id` / `branch_path` / `effect_occurrence` by exactly the same rules a proof law about `area` references via `.group(N).branch("path").event(k)`. User copy-pastes three numbers from the recording JSON into the verify block — no translation, no mental mapping. Record produces the concrete witness, proof expresses the universal claim, both speak the same three-coordinate structural language.

## Trust assumptions (generated header in each `.lean` / `.dfy`)

```
// Trusted model assumptions for this Aver proof export:
//
// Effects and dimensions:
//   Args.get      — snapshot: stable return for same input within run
//   Random.next   — generative: oracle indexed by (BranchPath, Int); each call fresh
//   Console.print — output: per-branch trace segment appended per call
//   ...
//
// Concurrency:
//   !  (independent parallel): proof holds for any legal schedule,
//        by schedule-invariance theorem of structural trace normalization.
//   ?! in complete mode: all branches run; error aggregated left-to-right.
//   ?! in cancel mode: NOT COVERED by this export. Project must set
//        mode = complete in aver.toml, or pass --allow-mode-mismatch
//        to aver proof.
//
// Structural trace addressing:
//   Events addressed by (BranchPath, occurrence_in_branch).
//   BranchPath is source-derived, schedule-invariant.
//   Cross-branch ordering NOT observable.
//   Wall-clock and shared-channel adjacency NOT expressible.
//
// Out of scope in this export:
//   - Stateful effects
//   - Higher-order effectful callbacks
//   - Interactive protocols
//   - Custom (non-built-in) effects
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

error: project uses mode = cancel in aver.toml, but aver proof only
       supports ?! in complete mode.

       To proceed:
       - set mode = complete in aver.toml (recommended), OR
       - pass --allow-mode-mismatch to aver proof
         (exported proofs do NOT cover cancel semantics; runtime must
          still be configured with mode = complete for the proofs to
          transfer to execution).

error: cross-branch ordering assertion at line N:
         foo().trace.group(1).branch("0").event(0) < foo().trace.group(1).branch("1").event(0)
       Global ordering across independent ! branches is not observable.
       Assert per-branch, or sequentialize the operations with ';'.

error: function 'customThing' uses unclassified effect 'MyEffect.do'
       Custom effects are not in Oracle v1's proof subset.
       Built-in effects supported in proof: Args, Env, ProjectConfig,
       Random, Time, File, Console, Log, Http.
       User-defined effects are planned for Relay release.
```

## Scope estimate

| Component | Effort |
|---|---|
| BranchWitness ADT + parser / AST support | 2 days |
| Codegen per-dimension lift with BranchPath threading | 2 weeks |
| `Result` / `?` preservation across ghost state | 3 days |
| Branch witness tree construction for `!` and `?!` complete | 1 week |
| Aggregation rule for `?!` at join point | 2 days |
| `verify fn trace` keyword + trace-aware law parser | 3 days |
| Trace API (group / branch / event addressing matching replay) | 4 days |
| Dafny emission | 1 week |
| Lean emission | 1 week |
| Rejection diagnostics + trust-assumption header generator | 3 days |
| Normalization theorem (proof, not axiom) | 1 week |
| Replay tooling — structural path display alongside runtime IDs | 2 days |
| Tests + migration examples | 1 week |
| Docs | 3 days |

**Realistic total: 7–9 weeks focused work.**
**Pessimistic: 11 weeks** if normalization theorem or `?!` aggregation prove subtler than expected.

## Risks

1. **Normalization theorem formulation** needs care; `?!` complete aggregation under different schedules demands precise statement. If proof turns out hard, falling back to axiom is a regression in soundness story — want to avoid.
2. **Dafny solver cost** for deep branch nesting: N=2 fine; N=5+ nested `!` / `?!` unclear.
3. **Spec-vs-impl brittleness** for generative dim: BranchPath + counter alignment means spec and impl must consume oracle identically. UX cliff for refactor-sensitive users.
4. **Debug prints affecting trace-aware laws** — documented as expected; some users may still find it surprising.
5. **Sibling groups at same statement level**: rare case (tuple of groups, etc.) needs `.group(g)` infix in API. Verify consistency across examples before committing.

## Follow-up releases

- **Relay** — `?!` cancel mode with cooperative cancellation semantics; cross-branch trace ordering via merge witnesses; higher-order effectful callbacks; user-defined effect classification.
- **Ledger** — stateful effects (Store<K,V>) via ghost `Map<K, V>` + refinement relation tying runtime replay trace to the abstract model. Relation-based proofs ("proved relative to trusted Store laws / host adapter").

## Prior art

Design draws on Effekt's "effects as capabilities" framework (Brachthäuser et al.). Credit in release notes; no claim of novelty on the elaboration technique. Novelty is the product integration: a single capability abstraction drives proof export, replay artifacts, and WASM host interface in a consistent shape.
