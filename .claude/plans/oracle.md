# Oracle v1 — release design plan

> Status: design, pre-implementation
> Release codename: **Oracle**
> Next release (deferred scope): **Relay** — `?!` cancel mode, cross-branch trace ordering, higher-order effectful callbacks, language support for user-definable effects (Aver has none today; all effects built-in) plus their classification
> Later release: **Ledger** — stateful effects (Store<K,V>) via ghost Map + refinement

## Pitch (one sentence)

Effectful functions become first-class in `aver proof`: previously silently skipped, they are now lifted to pure form with per-dimension semantics (snapshot / generative / output) and a branch-witness tree for effectful `!` and `?!` (complete mode). Built-in effects are classified by Aver authors; `! [Effect.method]` in impl bodies stays unchanged. Verify blocks gain a small set of new conventions described below.

## What changes for users

**Unchanged:** `! [Effect.method]` in function body, `verify fn law fnSpec`, `!` / `?!` in source, `aver run` and `aver replay` workflows, the `aver.toml` independence mode (the proof subset requires `complete`).

**New conventions in verify blocks for effectful functions:**

1. **`given` binds oracles with user-chosen names** — `given rnd: Random.int = [stubA, stubB]` declares a local oracle binding; the effect method reference (`Random.int`) appears only in type position. The compiler infers the oracle signature from the effect's classification — no hand-written arrow type.
2. **Specs take `path: BranchPath` as a leading parameter** — effectful specs thread the caller's current path through to oracle invocations. `BranchPath` is an opaque builtin with `.root`, `.child(path, idx)`, `.parse(s)` constructors. `BranchPath.root` appears only in top-level laws, never hardcoded inside helper specs.
3. **Specs take oracle and capability parameters** — reader functions for snapshot dimensions, branch-indexed oracle functions for generative dimensions.
4. **`verify fn trace` keyword** enables trace-aware laws using `.result` / `.trace` projections. Without it, the law checks only the return value (adding a debug print does not break proofs that do not care about traces).
5. **Trace navigation uses `.group(N) / .branch(idx) / .event(k)`** with local structural addressing. Bridges `.path() / trace.replay(...) / trace.replaySeq(...)` link to replay JSON coordinates and oracle paths.
6. **Effect calls in verify-trace contexts are event literals or match patterns, not invocations** — `Console.print("x")` inside `.contains(...)` or after `=>` is an `EffectEvent` value; inside a `match` arm it is a destructuring pattern. Verify blocks never execute effects.

## Foundational model: structural path

Every Aver evaluation runs in a structural context path. Proof specs see this path through an opaque builtin type `BranchPath` with three constructors:

- `BranchPath.root` — canonical root (sequential code outside any `!`/`?!` group).
- `BranchPath.child(path: BranchPath, idx: Int)` — extend a path by entering branch `idx` of a group.
- `BranchPath.parse(s: String)` — construct from a dewey-decimal string as stored in replay JSON (e.g. `"2.0"` for branch 0 of a group nested inside branch 2 of an outer group). Bridge for recording-to-spec lookups.

`BranchPath` is opaque (not a String alias) — user code cannot compose nonsensical paths, and solvers see structural values rather than stringly-typed data.

`BranchPath` is **source-derived and schedule-invariant**. It appears in two places: oracle calls (`oracle(path, counter, args...)`) and the trace-API bridge (`.path()` on branch nodes). Primary trace navigation uses tree addressing (`.group(N).branch(idx).event(k)`) rather than paths. Sequential code uses the root path uniformly.

Specs thread `path: BranchPath` explicitly as a leading parameter — they receive the caller's current path, use it directly for oracle calls, and extend it via `BranchPath.child(path, idx)` when descending into their own `!`/`?!` groups. `BranchPath.root` only appears in top-level `verify ... law ...` clauses, never hardcoded inside helper specs. This preserves compositionality: a sequential helper called from inside a branch of its caller stays correct without modification.

This model is the foundation of generative oracle signatures, trace addressing (via a bridge), and the normalization theorem below.

## Scope

### In

- **Snapshot** effects → capability threading: single reader function param, deterministic, not branch-indexed.
- **Generative** effects → oracle with branch-indexed signature `(BranchPath, Int, args...) -> T` plus per-branch integer counter advanced per call.
- **Output** effects → per-branch trace segment threading: ghost `List<EffectCall>` appended per call; structural tree assembled at join. Opt-in in law via `trace` keyword.
- **Effectful `!`** (parallel independence) → branch-witness tree with structural branch paths.
- **Effectful `?!`** in `complete` mode → same witness tree, plus left-to-right error priority aggregation.
- **`Result` / `?` preservation** — ghost state (counter, trace) survives early return.
- **Trace API** using source-structural addressing (`group_id` = N-th `!`/`?!` in source order, local `branch_path`, local `effect_occurrence`), with an explicit bridge to/from replay JSON's runtime-assigned coordinates.

### Out (with clear rejection)

- **`?!` in `cancel` mode** — hard error from `aver proof` unless explicit opt-in flag.
- **Stateful effects** (Store, DB, any effect where write affects subsequent read) — replay only; Ledger release.
- **Higher-order effectful callbacks** (e.g. `HttpServer.listenWith(handler: Req -> ! [...] Resp)`) — proof subset rejection; runtime and replay unaffected.
- **Interactive / dialogue effects** (request-response protocols, subprocess stdin/stdout, LLM tool calls) — replay only.
- **Cross-branch global ordering laws** ("event A before event B across two `!` branches") — structurally inexpressible; explicit rejection. Within-branch ordering is fine.
- **User-definable effects** — Aver has no user-definable effects today; all effects are built-in. Adding the language feature (how users declare new effects) plus classifying them for proof is planned for Relay. Oracle v1 covers only the closed built-in set classified above.

## Effect classification (built-in, closed set for Oracle v1)

Oracle v1 only covers functions whose effects all appear in this table. Classification is fixed by Aver language authors, not declared by user.

| Effect | Method | Dimension(s) | Notes |
|---|---|---|---|
| `Args.get` | `() -> List<String>` | snapshot | stable within a run (CLI args) |
| `Env.get` | `String -> Option<String>` | snapshot | stable within a run |
| `Random.int` | `(Int, Int) -> Int` | generative | fresh value per call, bounds `(min, max)` inclusive |
| `Random.float` | `() -> Float` | generative | fresh value per call, range `[0.0, 1.0)` |
| `Time.now` | `() -> String` | generative | UTC timestamp string (`...Z`) |
| `Time.unixMs` | `() -> Int` | generative | Unix epoch ms |
| `Disk.readText` | `String -> Result<String, String>` | generative | live FS; value may change between calls |
| `Console.print` | `T -> Unit` | output | trace-appending; `T` is the runtime's generic type — the same polymorphism transfers to proof |
| `Console.error` | `T -> Unit` | output | trace-appending |
| `Console.warn` | `T -> Unit` | output | trace-appending |
| `Http.get` / `.head` / `.delete` | `String -> Result<HttpResponse, String>` | generative + output | request emitted to trace, response from oracle |
| `Http.post` / `.put` / `.patch` | `(String, String, String, List<Header>) -> Result<HttpResponse, String>` | generative + output | `(url, body, content-type, headers)` emitted to trace, response from oracle |

`HttpResponse` and `Header` are records (see `docs/services.md`). Proof-side types match the runtime signatures exactly — Oracle v1 does not rewrite effect surface.

Effects NOT in this table remain replay-only for Oracle v1:

- **Stateful mutations:** `Env.set`, `Disk.writeText` / `.appendText` / `.delete` / `.deleteDir` / `.makeDir`, `Time.sleep`.
- **Interactive / dialogue:** `Console.readLine`, `Tcp.*` (send/readLine/writeLine/connect/close/ping), `HttpServer.*` (higher-order callback).
- **Complex stateful surface:** `Terminal.*` (cursor, raw mode, color).

These use `aver record` / `aver replay` for deterministic reproduction, not `aver proof`. Stateful proof support is planned for Ledger; interactive / higher-order callback support for Relay. Full classification for every built-in effect is locked in before release.

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
    match Args.get().get(0)
        Some(s) -> parseInt(s)
        None    -> Err("missing port arg")

fn loadPortSpec(args: () -> List<String>) -> Result<Int, String>
    ? "Parse port from arg[0]."
    match args().get(0)
        Some(s) -> parseInt(s)
        None    -> Err("missing port arg")

fn argsA() -> List<String>
    ["8080"]

fn argsB() -> List<String>
    ["xyz"]

verify loadPort law loadPortSpec
    given args: Args.get = [argsA, argsB]
    loadPort() => loadPortSpec(args)
```

`doubleCheck(Args.get(), Args.get())` proves `true` — correct, because args are stable within a run. Snapshot capability is a pure extensional function; same inputs → same outputs.

### 3. Generative — `Random.int` (branch-indexed oracle)

```aver
fn pickThree() -> (Int, Int, Int)
    ! [Random.int]
    (Random.int(1, 100), Random.int(1, 100), Random.int(1, 100))

fn pickThreeSpec(path: BranchPath, oracle: (BranchPath, Int, Int, Int) -> Int) -> (Int, Int, Int)
    ? "Three draws from the oracle at the caller's path."
    (oracle(path, 0, 1, 100), oracle(path, 1, 1, 100), oracle(path, 2, 1, 100))

verify pickThree law pickThreeSpec
    given rnd: Random.int = [seedA, seedB]
    pickThree() => pickThreeSpec(BranchPath.root, rnd)
```

Sequential code — all three calls happen at the caller's path (root when called from a top-level law). Counter distinguishes them (0, 1, 2). Generative oracle signature mirrors the runtime signature: `Random.int : (Int, Int) -> Int` (args `(min, max)`) lifts to `(BranchPath, Int, Int, Int) -> Int` (adds path + counter in front of the original args). `doubleCheck(Random.int(1,100), Random.int(1,100))` does **not** prove `true` — same path, different counters → `oracle(path, 0, 1, 100)` and `oracle(path, 1, 1, 100)` may return different values.

Threading `path` rather than hardcoding `BranchPath.root` makes the helper compositional: if `pickThree` is later called from inside a branch of an outer `!`, the caller's elaboration passes `BranchPath.child(callerPath, idx)` and this spec stays correct without modification.

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
    (
        Console.print("fetch outer-" + a),      // branch "0"
        Console.print("fetch outer-" + b),      // branch "1"
        (                                        // branch "2" — nested group
            Console.print("fetch inner-A"),     //   branch "2.0"
            Console.print("fetch inner-B"),     //   branch "2.1"
        )!,
    )!
    Console.print("all fan-outs completed")     // sequential, no group
```

Replay JSON (from playground Trace view) looks like:
```json
{"seq":1,"group_id":1,"branch_path":"0",  "effect_occurrence":0,"args":["fetch outer-1"]}
{"seq":2,"group_id":1,"branch_path":"1",  "effect_occurrence":0,"args":["fetch outer-2"]}
{"seq":3,"group_id":2,"branch_path":"2.0","effect_occurrence":0,"args":["fetch inner-A"]}
{"seq":4,"group_id":2,"branch_path":"2.1","effect_occurrence":0,"args":["fetch inner-B"]}
{"seq":5,                                                        "args":["all fan-outs completed"]}
```

**Important: `group_id` in replay JSON is runtime-assigned** (monotonic counter over every `enter_group` call during the run, `src/replay/runtime.rs`), not source-structural. For a non-recursive function called once, the first `!` group encountered gets runtime `group_id = 1`, matching its source position. For recursive functions or helpers called multiple times, runtime IDs diverge from source positions (a recursive source-group produces many runtime IDs across instances). `seq` is the monotonic per-recording sequence number.

Law navigates the trace locally — group by source index, branch by local index within that group:
```aver
verify fanOut trace
    given a: String = ["1"], b: String = ["2"]

    fanOut(a, b).trace.group(1).branch(0).event(0)
        => Some(Console.print("fetch outer-1"))

    fanOut(a, b).trace.group(1).branch(1).event(0)
        => Some(Console.print("fetch outer-2"))

    fanOut(a, b).trace.group(1).branch(2).group(2).branch(0).event(0)
        => Some(Console.print("fetch inner-A"))

    fanOut(a, b).trace.group(1).branch(2).group(2).branch(1).event(0)
        => Some(Console.print("fetch inner-B"))

    fanOut(a, b).trace.event(0)
        => Some(Console.print("all fan-outs completed"))
```

Three addressing primitives:
- `.group(N)` — enter group with source-structural `group_id: N` (first group in body is 1, second is 2, …).
- `.branch(idx: Int)` — select branch `idx` **locally within the currently-entered group**. Nested groups are re-entered with another `.group()` call.
- `.event(k)` — the k-th event at the current node.
- `.event(k)` at the trace root addresses sequential events outside any group (no `.group()` prefix needed).

**Structural, not runtime.** `group_id` numbering reflects source-order-of-encounter. The same source compiled at any time gives the same IDs, so proofs remain stable across runs.

**Oracle `BranchPath`, primary trace navigation, and replay bridge are three distinct addressing systems:**

| System | Scope | Identifier | Used for |
|---|---|---|---|
| Oracle `BranchPath` | Proof model | opaque `.root` / `.child` / `.parse` | Specs reference oracle at a branch |
| Primary trace API | Proof laws, source-stable | `.group(N)` = N-th `!`/`?!` in source + local `.branch(idx)` + `.event(k)` | Readable assertions that survive refactor |
| Replay bridge | Runtime recordings | `trace.replay(runtimeGroupId, deweyPath, k)` / `trace.replaySeq(seq)` | Copy-paste from JSON |

**Three bridges link them:**

- `.path() : BranchPath` on a branch node — returns the equivalent oracle-BranchPath for tying a trace event to its oracle at the same branch (e.g. `fanOut(a,b).trace.group(1).branch(0).path()` yields the BranchPath the oracle saw for that branch).
- `trace.replay(runtimeGroupId: Int, path: String, event: Int) : Option<EffectEvent>` — exact lookup by replay JSON's runtime coordinates. **Accepts runtime `group_id`** (monotonic, unique per `enter_group` call in the recording), so `(runtimeGroupId, path, event)` identifies at most one emission. For recursion and multi-call helpers, multiple runtime group_ids correspond to the same source group; each has its own runtime ID in JSON and its own exact `.replay(...)` lookup.
- `trace.replaySeq(seq: Int) : Option<EffectEvent>` — lookup by the monotonic per-recording `seq` field (1-indexed, globally unique within a recording). Equivalent addressing power to `.replay(...)`; preferred when the caller just wants the N-th emission regardless of group structure.

Primary API optimizes for readability and refactor stability. Bridges are for tooling/debug/recursion workflows.

### 7. Fork-join `?!` complete mode

```aver
fn fetchBoth(urlA: String, urlB: String) -> Result<(HttpResponse, HttpResponse), String>
    ! [Http.get]
    (Http.get(urlA), Http.get(urlB))?!

fn fetchBothSpec(
    path: BranchPath,
    urlA: String,
    urlB: String,
    http: (BranchPath, Int, String) -> Result<HttpResponse, String>
) -> Result<(HttpResponse, HttpResponse), String>
    ? "Try both URLs; first source-order Err wins."
    match (http(BranchPath.child(path, 0), 0, urlA),
           http(BranchPath.child(path, 1), 0, urlB))
        (Ok(a), Ok(b))  -> Ok((a, b))
        (Err(e), _)     -> Err(e)
        (Ok(_), Err(e)) -> Err(e)

verify fetchBoth law fetchBothSpec
    given urlA: String = ["https://a"], urlB: String = ["https://b"]
    given http: Http.get = [httpStub]
    fetchBoth(urlA, urlB) => fetchBothSpec(BranchPath.root, urlA, urlB, http)
```

`Http.get` is generative + output. Oracle covers the response; output side appears in trace when the verify block uses `trace` keyword. In `?!` complete, both branches run to completion; aggregation takes the first `Err` in source order.

### 8. Mixed dimensions in one function

```aver
fn report(region: String) -> Unit
    ! [Args.get, Random.int, Console.print]
    prefix = match Args.get().get(0)
        Some(s) -> s
        None    -> "default"
    id     = Random.int(1, 1000)
    Console.print(prefix + " report " + toString(id) + " for " + region)

fn reportSpec(
    path: BranchPath,
    region: String,
    args: () -> List<String>,
    oracle: (BranchPath, Int, Int, Int) -> Int
) -> Unit
    ? "Report returns Unit; side-effect visible in trace."
    Unit

verify report trace law reportSpec
    given region: String = ["PL"]
    given args: Args.get = [argsStub]
    given rnd: Random.int = [randStub]
    report(region).result                        => reportSpec(BranchPath.root, region, args, rnd)
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

Lifted specs take `path: BranchPath` as a leading parameter — the caller's current path, threaded through to oracle invocations. Entering a `!`/`?!` group in the spec body constructs child paths via `BranchPath.child(path, idx)`. Top-level `verify ... law ...` binds `path = BranchPath.root`.

- **Snapshot**: capability function parameter, unchanged through evaluation; NOT branch-indexed (stable across branches by definition).
- **Generative**: `(BranchPath, Int, args...) -> T` oracle parameter + **per-branch** `Int` counter. Counter resets at branch entry; `path` argument flows from caller, extended via `BranchPath.child` when descending into a group.
- **Output**: per-branch `List<EffectCall>` trace segment appended per call; structural tree assembled from per-branch segments at join.
- **`Result` / `?` preservation**: lifted function returns `(Result<A, E>, counter, trace)`, so error paths preserve ghost state.

### `given` bindings and verify elaboration

`given` clauses introduce local bindings for verify blocks:

```aver
given <name>: <EffectRef>  = [<sample stubs>]   // oracle for an effect
given <name>: <Type>       = [<sample values>]  // plain input sample
```

Form distinguishes by position: `<EffectRef>` is recognized when it names a built-in classified effect method (`Random.int`, `Http.get`, `Console.print`, …); the compiler infers the oracle signature from the effect's classification (no hand-written `(BranchPath, Int, …) -> T`).

**Name is user-chosen, not derived from the effect.** `given rnd: Random.int = [...]`, `given http: Http.get = [...]`, etc. The effect method reference (`Random.int`) appears only in type position; it is never the binding name. This eliminates cross-context ambiguity — effect-method identifiers like `Console.print` mean *only* effect-operation / event-literal / event-type-ref, never "the oracle I declared above". Multiple bindings of the same effect type (`given rndA: Random.int = [sA]`, `given rndB: Random.int = [sB]`) are permitted.

**Per-equation oracle resolution rule.** For each `=>` equation and each effect `E` used by the LHS impl:

- If exactly one `given` exists for `E` in the block → LHS is lifted with that binding (trivial).
- If multiple `given`s exist for `E` and the RHS references exactly one of them (by identifier appearance anywhere in the RHS expression) → LHS is lifted with that binding.
- Otherwise (RHS references zero or two-or-more bindings for `E`) → compiler error: *"ambiguous oracle for effect E at line N — RHS must reference exactly one binding (one of: rndA, rndB). Remove a redundant `given` or pick one on the RHS."*

This codifies the natural reading of `pickThree() => pickThreeSpec(path, rndA)` (impl uses the same oracle the spec was given) without introducing explicit `using` syntax. Trace-only equations (`foo().trace.length() => 3`) that don't reference an oracle can still be authored — they resolve fine when only one `given` exists for the effect, and require disambiguating spec-form or one-binding scoping when multiple do.

**Verify desugars to:**

- *Proof export (Dafny/Lean):* each `given` becomes a universally quantified variable in the emitted theorem. Stub lists are ignored. Each `LHS => RHS` becomes `forall <given vars>. lifted_LHS(<given vars>) == RHS(<given vars>)` with identical oracles passed to both sides (lifting of LHS redirects effect invocations in impl body to the oracle variable with the same effect type as referenced on RHS).
- *Concrete run (`aver verify`):* for each stub in the list (Cartesian product across multiple `given`s referenced by a law), substitute the stub for the oracle, run the impl with effect calls hooked to the stub, compute RHS with the same stub, assert equality.

No real effects run in verify context — stubs are pure functions. Verify blocks cannot invoke I/O, and the language enforces this: effect-list enforcement is scoped to normal code paths only.

### Structural trace normalization

**Theorem (schedule-invariance of the structural trace tree)**:

For any two legal runtime schedules `s1, s2` of the same Aver evaluation `e`:
```
tree(e, s1) = tree(e, s2)
```
where `tree(e, s)` is the structural trace tree as exposed by the primary API — a tree whose interior nodes are `!`/`?!` groups addressed by source-structural `group_id` with locally-indexed branches, whose leaves are event children at each node, and whose traversal primitives are `.group(N) / .branch(idx) / .event(k)`.

**Proof sketch** — three lemmas:

**Lemma 1 — branch locality.** For Oracle v1's closed effect classification (`Args.get`, `Env.get`, `Random.int`, `Random.float`, `Time.now`, `Time.unixMs`, `Disk.readText`, `Console.print`, `Console.error`, `Console.warn`, `Http.get`/`.head`/`.delete`/`.post`/`.put`/`.patch`), each branch within a `!`/`?!` group computes its subtree (sequence of event children + any nested group children, plus the branch's return value and counter) as a pure function of: (the branch's source code; the incoming `path`; the oracles for generative effects; the capability functions for snapshot effects). No effect in the classification observes or mutates state belonging to another branch — snapshot/generative are extensional lookups, output appends to the current branch's local trace segment.

**Lemma 2 — deterministic aggregation at join.**
- `!` aggregation = tuple construction over branch return values. Pure function of its components; commutative with respect to completion order.
- `?!` complete aggregation = first `Err` in **source order** (not completion order). Source order is a syntactic invariant of the AST, independent of runtime scheduling.

**Lemma 3 — faithful tree construction from runtime emissions.** For any legal schedule `s`, the runtime recorder stamps each effect emission with enough metadata (`caller_fn`, active group/branch context, per-branch occurrence counter) to place it at a unique address in the structural tree: event children under the appropriate node in the verified function's body, group children spawned at source-structural group_ids. The tree-construction function is a pure projection over the recorded emissions; its output depends only on which emissions occurred and their structural addresses, not on the order in which they appear in the recording.

**Theorem** follows: each branch's subtree is schedule-invariant (Lemma 1); aggregation at join is schedule-invariant (Lemma 2); the mapping from runtime emissions to tree addresses is a pure projection (Lemma 3). Therefore the tree is the same across schedules. The `;` case is trivial (single thread, single order).

**Formalization status and scope decision for v1.** The three lemmas above are argued prose in this plan, not mechanized. Emitting a formal theorem per generated `.dfy`/`.lean` would require lowering the Aver effect semantics, branch-locality property, and runtime correspondence into each target's logic — non-trivial meta-theory work, realistically 2–3 weeks per backend before the practical proofs on top.

For v1 we take the CompCert / Iris / F\*–Dijkstra monads–style stance: **schedule-invariance is a compiler-level invariant, trusted by generated artifacts, and proved mechanically (as a meta-theorem) as future work.** Emitted `.dfy`/`.lean` files reference this invariant by name in their trust-assumption header (see below), not as a per-file axiom. This localizes the trust claim to one external argument about the compiler, rather than sprinkling an `axiom schedule_invariance;` into every artifact (which would weaken all downstream proofs uniformly).

A user inspecting an exported proof therefore sees: domain-specific properties proved mechanically against the Oracle-lifted spec; the lifting itself relies on the compiler invariant documented in the header. Mechanization of that meta-theorem is a planned post-v1 upgrade.

### Trace addressing

Primary API — local structural navigation:

- `.group(N)` — enter `!`/`?!` group with source-structural `group_id: N` (1 for first group in body, 2 for second, …; unaffected by runtime).
- `.branch(idx: Int)` — select branch `idx` locally within the currently-entered group. Nested groups require another `.group()` call.
- `.event(k) : Option<EffectEvent>` — the k-th event at the current node; `None` if out of range.
- `.event(k)` at the trace root addresses sequential events outside any group.
- `.length() : Int` — number of events at the current node (not recursive).
- `.contains(x) : Bool` — overloaded:
  - `.contains(event: EffectEvent)` — exact match (same effect type, same args)
  - `.contains(effect_ref)` — type-only match, e.g. `trace.contains(Console.print)` → "any Console.print of any args"
- `event.is(effect_ref) : Bool` — type predicate on a specific event (e.g. `trace.event(0).is(Console.print)`)

**Events and groups are disjoint children of a node.** Every node in the trace tree has two kinds of children: *event children* (direct effect emissions at this level) and *group children* (nested `!`/`?!` groups rooted at this level). `.event(k)` indexes only event children; `.group(N)` enters a group child. `.length()` counts event children only — groups are not events.

Example: a function body `Console.print("a"); (Console.print("x"), Console.print("y"))!; Console.print("b"); Console.print("c"); (Console.print("p"), Console.print("q"))!; Console.print("d")` yields trace-root with four event children (`.event(0)` = "a", `.event(1)` = "b", `.event(2)` = "c", `.event(3)` = "d") and two group children (`.group(1)` and `.group(2)`). `"x"` is addressed as `.group(1).branch(0).event(0)`, never as `.event(N)` at root. Every event has exactly one canonical address.

Mapping from replay JSON: a root event has no `group_id`/`branch_path` fields and its root-level index is its position among JSON entries with missing `group_id` (in emission order). For a non-recursive function invoked once, a group event's runtime `group_id = N` in JSON corresponds to `.group(N)` (structural) because runtime counter and source order coincide in that case; `branch_path` dewey segments map to nested `.branch(i).group(M)...` steps and `effect_occurrence` → `.event(k)`. For recursive or multiply-invoked functions, runtime `group_id` diverges from structural — use `trace.replay(...)` or `trace.replaySeq(...)` bridges to address specific recordings.

### Effect calls in verify-trace context

Inside `verify fn trace ...` blocks, effect-method syntax has context-sensitive semantics:

| Syntax | Context | Meaning |
|---|---|---|
| `Console.print("x")` | Normal function body with `! [Console.print]` | Invoke the effect |
| `Console.print("x")` | Verify-trace block (RHS of `=>`, argument to `.contains` / trace navigation) | Event literal: `EffectEvent { type = "Console.print", args = ["x"] }` |
| `Console.print` (no parens) | Verify-trace block, `.contains(...)` or `.is(...)` argument | Effect-type reference (predicate) |
| `Console.print(msg)` | Pattern position in `match` arm inside verify block | Constructor-like pattern destructuring an `EffectEvent`; binds `msg` to first arg |

This dual-mode elaboration is localized to verify-trace contexts, where the language's "execute this effect" interpretation is nonsensical (verify blocks never run effects). Elaboration rewrites effect-call expressions to `EffectEvent` literal construction and effect-call patterns to `EffectEvent` destructuring. Effect-list enforcement is suspended in trace-assertion context — a verify block asserting about `Console.print` does not require the enclosing verify to declare that effect.

**EffectEvent.args typing.** `EffectEvent` carries `args: List<Value>` where each arg has the same runtime type as the corresponding parameter in the effect method's signature. For polymorphic methods (`Console.print : T -> Unit`, `Console.error`, `Console.warn`), the `T` is preserved — no narrowing, no coercion. Pattern `Console.print(msg)` binds `msg` to the actual Aver value that was emitted (if `Console.print(42)` was emitted, `msg : Int`; if `Console.print("x")` was emitted, `msg : String`). Equality comparison (`.contains(Console.print("x"))`) uses Aver's standard structural equality on Value, so `Console.print(5)` and `Console.print("5")` are distinct events.

In concrete-run verify (with stubs), runtime Values are compared directly. In proof export (Dafny/Lean), `EffectEvent` is encoded as a tagged union over the closed effect set, with each variant's args typed per the method signature; polymorphic `T` is represented as Aver's `Value` sum (Int | Float | String | Bool | …) in target. This is a faithful translation of the runtime's polymorphism rather than a special proof-time narrowing.

Complex queries (partial arg matching, guards) use `match` patterns on `.event(k)` results, which is idiomatic since Aver has no closures:
```aver
match trace.event(2)
    Some(Http.get(url)) -> url.startsWith("https://")
    _                   -> false
```

Higher-order trace queries (`.any`, `.all`, `.filter`) are deferred — they would require closures. Pattern matching on individual events covers the v1 surface.

**Structural, not runtime**. `group_id` in the proof API means "order of encounter in source", so the same program recompiled gives the same IDs and proofs stay stable. For recursion — where one source group produces multiple runtime instances that share the same structural `(group_id, branch_path, effect_occurrence)` triple — use `trace.replaySeq(seq)` to address a specific dynamic instance by its monotonic per-recording sequence number.

### Trace across helper boundaries

When the verified function calls a helper that itself emits effects, the trace API needs a clean rule that doesn't fabricate ordering where independence prevents it.

**Rule:** `fn.trace` exposes **only effects emitted directly from the verified function's own body**. Effects emitted from within helper calls are **not** in the caller's trace — they are addressable only through the helper's own verify block.

Concretely, given:
```aver
fn foo() -> Unit
    ! [Console.print]
    Console.print("before")
    bar()
    Console.print("after")

fn bar() -> Unit
    ! [Console.print]
    (Console.print("b1"), Console.print("b2"))!
```

`foo().trace.event(0)` is `Console.print("before")`, `.event(1)` is `Console.print("after")`, `.length()` is 2. Bar's two prints are **not** in foo's trace. To assert the independent-product semantics of bar, write `verify bar trace law barSpec` — in that scope, `bar().trace.group(1).branch(0).event(0) => Some(Console.print("b1"))`, etc.

**Why exclusion rather than flattening.** Two problems with flattening: (a) if bar contains a `!` group, its two branches have no observable cross-branch order (core Aver semantics) — flattening to a sequential list would fabricate one arbitrarily; (b) reconstructing the call-site of each helper emission requires metadata the runtime doesn't record today (`caller_fn` + `source_line` pinpoint where inside the helper an effect came from, not which call-site in the caller invoked that helper). Both problems disappear if helper emissions stay in the helper's trace.

**Source of truth.** `EffectRecord.caller_fn` (already in `src/replay/session.rs`) identifies which function emitted each effect. At trace-tree construction for `fn.trace`, the compiler filters recorded emissions to those with `caller_fn == fn`, producing a tree exactly over `fn`'s own body. Other emissions (from helpers, from recursive sub-calls) are invisible in this trace view, though they remain in the recording and in the helpers' own trace views.

**Composition of reasoning.** A proof about `foo` composes from foo's own law (about foo's direct emissions and return value) plus laws about the helpers it calls. This is idiomatic modular reasoning — each function verified on its own body — and it keeps the trace API semantically clean.

**If a user needs to assert cross-helper properties**, that's a Relay follow-up. Candidate shapes include `.call(helperFn, k) : Trace` descending into the k-th call of a helper, or unified multi-function verify blocks. Out of v1 scope.

**Effectful recursion + trace-aware laws: rejected in v1.** The `caller_fn == fn` filter used to scope trace to direct emissions cannot distinguish between the outermost invocation of `fn` and its recursive self-calls — both appear as `caller_fn == fn`. Rather than introducing call-instance metadata (new recording schema + new runtime infrastructure + new addressing semantics, all beyond v1 scope), `verify fn trace law ...` rejects functions that recursively call themselves while using any effect in the classification.

- **Non-recursive effectful functions**: full trace-aware law support.
- **Recursive functions without effects**: unchanged, same as today.
- **Recursive effectful functions with result-only laws** (`verify fn law ...`, no `trace` keyword): supported — proof-model lifting composes through the recurrence structure, trace is not referenced.
- **Recursive effectful functions with trace-aware laws**: compile-time rejection with a diagnostic pointing at either splitting the recursive accumulator into a non-recursive helper or using result-only laws. Planned for a post-v1 release.

### Bridge to replay JSON and oracle

Trace API navigation is separate from the opaque `BranchPath` used by oracle specs — two different addressing systems serving two different purposes (proof-model abstraction over branch context vs concrete navigation over the recorded structural tree). Two bridges link them:

- **`.path() : BranchPath`** — on any branch node, returns the equivalent `BranchPath` for tying a trace event to the oracle at the same branch. Enables assertions like "the value passed to oracle at this branch matches what was recorded here."
- **`trace.replay(groupId: Int, path: String, event: Int)`** — direct jump to an event by its replay JSON coordinates. Convenience for copy-paste from recordings: `trace.replay(2, "2.0", 0)` addresses the same event as `trace.group(1).branch(2).group(2).branch(0).event(0)`.

**Recordings populate the trace structure with runtime coordinates**. A recording captured via `aver run -e 'area(Shape.Circle(1.0))' --record dir/` (0.10.1) stores runtime `group_id` + `branch_path` + `effect_occurrence` + monotonic `seq` — all consumed by the bridge API. Primary API (source-structural) is what proof laws should prefer; replay bridges handle recording-specific addressing including recursion disambiguation via `seq`. Record produces the concrete witness, proof expresses the universal claim, and the bridges let you reference one from the other when it helps.

## Trust assumptions (generated header in each `.lean` / `.dfy`)

```
// Trusted model assumptions for this Aver proof export:
//
// Effects and dimensions:
//   Args.get, Env.get           — snapshot: stable return for same input within run
//   Random.int, Random.float,
//   Time.now, Time.unixMs,
//   Disk.readText               — generative: oracle indexed by (BranchPath, Int, args...); each call fresh
//   Console.print/.error/.warn  — output: per-branch trace segment appended per call
//   Http.get/.head/.delete/
//     .post/.put/.patch         — generative + output: request emitted to trace, response from oracle
//
// Concurrency and schedule invariance:
//   !  (independent parallel): proof holds for any legal schedule,
//        relying on the Aver compiler invariant "schedule-invariance
//        of structural trace normalization" (branch locality +
//        deterministic aggregation + runtime-provenance correspondence;
//        informally proved in Oracle v1 plan, mechanized meta-proof
//        is future work). This is a compiler-level trusted claim,
//        not emitted as a per-artifact axiom.
//   ?! in complete mode: all branches run; error aggregated
//        left-to-right in source order (not completion order) —
//        this is what makes ?! complete aggregation schedule-invariant.
//   ?! in cancel mode: NOT COVERED by this export. Project must set
//        mode = complete in aver.toml, or pass --allow-mode-mismatch
//        to aver proof. Note: aver proof exports describe complete-mode
//        semantics; runtime must also execute in complete mode for
//        exported proofs to transfer to execution.
//
// Structural trace addressing:
//   Events addressed by the structural tree via
//   .group(N).branch(idx).event(k) in primary API; BranchPath
//   (opaque type) is used for oracle bindings and via the
//   .path() bridge on branch nodes.
//   BranchPath is source-derived, schedule-invariant.
//   Cross-branch ordering NOT observable.
//   Wall-clock and shared-channel adjacency NOT expressible.
//
// Effect classification (closed for Oracle v1):
//   Only the classified built-in effects listed above are in the
//   proof subset. Other built-in effects (stateful or interactive:
//   Env.set, Disk.writeText / .appendText / .delete / .deleteDir /
//   .makeDir / .exists / .listDir, Time.sleep, Console.readLine,
//   Tcp.*, HttpServer.*, Terminal.*) are rejected by 'aver proof'
//   and remain replay-only for Oracle v1. Aver has no user-defined
//   effects in the language today; adding user-definable effects
//   plus their classification is planned for the Relay release.
//
// Backend independence:
//   Exported proofs hold uniformly across Aver backends (VM, compiled
//   Rust, WASM) under the schedule-invariance compiler invariant above.
//   Sequential execution (VM) and parallel execution (compiled Rust)
//   are both covered — both are legal schedules of the same evaluation.
//
// Recursion caveat:
//   Source-structural group_ids are stable across recompilation, but
//   recursive functions produce multiple runtime instances of the same
//   structural group that share (group_id, branch_path, effect_occurrence).
//   Proofs addressing groups via .group(N) refer to the structural
//   (source) position; per-instance addressing in recordings uses
//   trace.replaySeq(seq) with the monotonic sequence number from JSON.
//
//   Trace-aware laws for effectful recursive functions are REJECTED in
//   Oracle v1 — the caller_fn filter for fn.trace cannot distinguish
//   the outermost invocation from recursive self-calls without
//   call-instance metadata (deferred). Result-only laws for such
//   functions remain fully supported.
//
// Out of scope in this export:
//   - Stateful effects (Store, DB, shared mutable state)
//   - Higher-order effectful callbacks
//   - Interactive protocols (request-response, stdin/stdout dialogue)
//   - User-defined effects (Aver has none; language feature itself is deferred)
//   - ?! cancel mode
//   - Trace-aware laws on recursive effectful functions (result-only OK)
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

error: 'verify sumWithLogging trace law sumWithLoggingSpec' targets a
       recursive effectful function. Trace-aware laws on effectful
       recursion are not supported in Oracle v1 — the caller_fn filter
       that scopes fn.trace to direct emissions cannot distinguish the
       outermost invocation from recursive self-calls.

       To proceed, one of:
       - drop the 'trace' keyword to use a result-only law
         (verify sumWithLogging law sumWithLoggingSpec) — proof lifts
         through the recurrence; trace assertions just aren't available.
       - refactor sumWithLogging to call a non-recursive helper that
         emits the effects; verify the helper's trace separately.

       Full support is planned for a post-v1 release.

error: project uses mode = cancel in aver.toml, but aver proof only
       supports ?! in complete mode.

       To proceed:
       - set mode = complete in aver.toml (recommended), OR
       - pass --allow-mode-mismatch to aver proof
         (exported proofs do NOT cover cancel semantics; runtime must
          still be configured with mode = complete for the proofs to
          transfer to execution).

error: cross-branch ordering assertion at line N:
         foo().trace.group(1).branch(0).event(0) < foo().trace.group(1).branch(1).event(0)
       Global ordering across independent ! branches is not observable.
       Assert per-branch, or sequentialize the operations with ';'.

error: function 'writeLog' uses effect 'Disk.writeText' which is not
       classified in Oracle v1's proof subset (Disk.writeText is
       stateful — it mutates filesystem state across calls).
       Classified built-in effects in proof: Args.get, Env.get,
       Random.int, Random.float, Time.now, Time.unixMs, Disk.readText,
       Console.print/.error/.warn, Http.get/.head/.delete/.post/.put/.patch.
       Use 'aver record' / 'aver replay' for functions using stateful
       effects. Stateful proof support is planned for Ledger release.
```

## Scope estimate

| Component | Effort |
|---|---|
| `BranchPath` opaque builtin + `.root` / `.child` / `.parse` constructors | 2 days |
| BranchWitness ADT + parser / AST support | 2 days |
| Codegen per-dimension lift with path-threading in specs | 2 weeks |
| `Result` / `?` preservation across ghost state | 3 days |
| Branch witness tree construction for `!` and `?!` complete | 1 week |
| Aggregation rule for `?!` at join point | 2 days |
| `verify fn trace` keyword + trace-aware law parser | 3 days |
| Trace API: primary `.group().branch().event()` + bridges `.path()` / `trace.replay(...)` / `trace.replaySeq(...)` + helper-boundary exclusion rule (filter by `caller_fn`) | 5 days |
| Trace-context elaboration (effect-call as event-literal + effect-call as match pattern; dual-mode typechecker) | 4 days |
| Dafny emission | 1 week |
| Lean emission | 1 week |
| Rejection diagnostics + trust-assumption header generator | 3 days |
| Effectful-recursion detector for `verify fn trace law` + clear rejection diagnostic | 1 day |
| Normalization compiler invariant — three-lemma written argument + trust header wiring (mechanized meta-proof deferred post-v1) | 3 days |
| Replay tooling — structural path display alongside runtime IDs | 2 days |
| Reliable `caller_fn` attribution in VM runtime — currently some paths pass empty string; trace-exclusion rule needs accurate enclosing-fn name for every emission including inlined lambdas | 2 days |
| Tests + migration examples | 1 week |
| Docs | 3 days |

**Realistic total: 7–9 weeks focused work.**
**Pessimistic: 11 weeks** if `?!` aggregation or codegen emission prove subtler than expected. (Previously flagged: mechanized normalization theorem — now scoped out of v1, treated as compiler-level trusted invariant with written three-lemma argument, mechanization is future work.)

## Risks

1. **Normalization compiler invariant** formulation needs care; `?!` complete aggregation under different schedules demands precise statement. v1 ships with a written three-lemma argument (branch locality + deterministic aggregation + runtime-provenance correspondence) trusted at compiler level, not mechanized. Post-v1 upgrade to a mechanized meta-theorem (à la CompCert / Iris) is planned. The risk is that the written argument hides a subtle gap — mitigation: external formal-verification review before v1 ships.
2. **Dafny solver cost** for deep branch nesting: N=2 fine; N=5+ nested `!` / `?!` unclear.
3. **Spec-vs-impl brittleness** for generative dim: BranchPath + counter alignment means spec and impl must consume oracle identically. UX cliff for refactor-sensitive users.
4. **Debug prints affecting trace-aware laws** — documented as expected; some users may still find it surprising.
5. **Sibling groups at same statement level**: rare case (tuple of groups, etc.) needs `.group(g)` infix in API. Verify consistency across examples before committing.

## Follow-up releases

- **Relay** — `?!` cancel mode with cooperative cancellation semantics; cross-branch trace ordering via merge witnesses; higher-order effectful callbacks; **user-definable effects** (the language feature — how users declare new effects) plus their classification for proof.
- **Ledger** — stateful effects (Store<K,V>) via ghost `Map<K, V>` + refinement relation tying runtime replay trace to the abstract model. Relation-based proofs ("proved relative to trusted Store laws / host adapter").

## Prior art

Design draws on Effekt's "effects as capabilities" framework (Brachthäuser et al.). Credit in release notes; no claim of novelty on the elaboration technique. Novelty is the product integration: a single capability abstraction drives proof export, replay artifacts, and WASM host interface in a consistent shape.
