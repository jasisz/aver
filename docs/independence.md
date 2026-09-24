# Independent Products

## What `!` means

A plain tuple is a product of values. With `!` after it, it is a product of independent computations. With `?!` after it, it is a product of independent Result computations that propagates errors.

## Core definitions

**`(a, b)!`** is a product of independent computations.

For pure computations, independence follows from the structure. Tuple elements have no data dependency on each other, and Aver's core restrictions (no mutation, no closures, no shared state) leave them no way to interfere.

For effectful computations, `!` is the author's declaration that the elements' effects are safe to reorder or run concurrently. The compiler checks shape and types. It does not prove that the effects commute.

So the runtime may evaluate the elements one after another (left-to-right) or concurrently.

**`(a, b)?!`** is a product of independent `Result` computations.

If every branch produces `Ok`, `(a, b)?!` gives the tuple of unwrapped values. If one or more branches produce `Err`, the product is `Err`. Which error propagates is chosen deterministically, in left-to-right order.

**`(a, b)`** is a product of values, with ordinary tuple semantics and no claim of independence.

## Soundness envelope

`!` is sound by construction only for pure terms. For effectful terms it is a semantic contract that nobody checks. The programmer asserts that every schedule the execution mode allows gives an acceptable observable result.

## Formal semantics

Let τ range over observable effect traces. In an independent product, every branch starts from the same incoming trace prefix. The runtime may pick any schedule the execution mode allows.

**Pure / all-success case:**

```
⟨a, τ₀⟩ ⇓ ⟨Ok(v₁), τ₀ · τ_a⟩
⟨b, τ₀⟩ ⇓ ⟨Ok(v₂), τ₀ · τ_b⟩
τ ∈ Interleave(τ_a, τ_b)
────────────────────────────────────────────
⟨(a, b)?!, τ₀⟩ ⇓ ⟨Ok((v₁, v₂)), τ₀ · τ⟩
```

**Single-failure, complete mode:**

```
⟨a, τ₀⟩ ⇓ ⟨Err(e), τ₀ · τ_a⟩
⟨b, τ₀⟩ ⇓ ⟨r_b,    τ₀ · τ_b⟩
τ ∈ Interleave(τ_a, τ_b)
────────────────────────────────────────────
⟨(a, b)?!, τ₀⟩ ⇓ ⟨Err(e), τ₀ · τ⟩
```

**Single-failure, cancel mode:**

```
⟨a, τ₀⟩ ⇓ ⟨Err(e), τ₀ · τ_a⟩
⟨b, τ₀⟩ ⇓cancel ⟨r_b', τ₀ · τ_b'⟩
τ_b' ⊑ τ_b
τ ∈ Interleave(τ_a, τ_b')
────────────────────────────────────────────
⟨(a, b)?!, τ₀⟩ ⇓ ⟨Err(e), τ₀ · τ⟩
```

If several branches produce `Err`, the first `Err` in left-to-right order propagates. The single-failure rules extend to several failures by taking `Err(e)` to be the `Err` of the leftmost failing branch.

**Bare `!`:**

```
⟨a, τ₀⟩ ⇓ ⟨v₁, τ₀ · τ_a⟩
⟨b, τ₀⟩ ⇓ ⟨v₂, τ₀ · τ_b⟩
τ ∈ Interleave(τ_a, τ_b)
────────────────────────────────────────────
⟨(a, b)!, τ₀⟩ ⇓ ⟨(v₁, v₂), τ₀ · τ⟩
```

**Replay invariant:** replay records tuples of `(group_id, branch_path, effect_occurrence, effect_type, effect_args, result)`. Within a group, entries are matched by `(group_id, branch_path, effect_occurrence, effect_type, effect_args)`. Their position in the execution schedule plays no part. `branch_path` is a dotted path giving the branch position inside nested products (e.g. `"0.1"` = branch 0 of the outer product, branch 1 of the inner one). `effect_occurrence` is the 0-based count of effect emissions within a branch, which tells apart several emissions of the same effect in one branch. With both, replay stays deterministic across branch identity and repeated effects, in nested and recursive compositions alike. Reordering inside an independent product does not break replay.

**Cancellation and error priority:** a cancellation error is a by-product of execution and does not count as a primary failure. When `?!` unwraps results, a real `Result.Err` from one branch always wins over a cancellation error from a sibling. A cancellation error propagates only when no branch produced a real `Err`.

**Backend coverage:** the VM and the compiled Rust backend (`aver compile`) both implement cooperative cancellation. The VM checks at intervals while it runs bytecode. Compiled Rust checks at generated function boundaries and before effectful builtins. Every backend selects errors the same deterministic way (left-to-right). Every backend that supports replay records `branch_path` and `effect_occurrence`.

## Structural properties

1. **Structural independence**: tuple elements cannot refer to each other. A tuple expression has no binding site that could make one element visible to another.

2. **Composition**: `!` products compose exactly like tuples:
   - Nested: `(a, (b, c)!)!`
   - Recursive: `(f(x), g(xs))?!`
   - Flat: `(a, b, c, d)?!`

3. **Error algebra**: `(a, b)?!` is the independent product of `Result` computations. If every branch produces `Ok`, the result is the tuple of unwrapped values. If one or more branches produce `Err`, the product is `Err`, chosen by the error selection rule in Core definitions.

4. **Recursion builds products**: a recursive function over a list builds a product at each step, pairing the work for the current element with the work for the rest. With `?!` this is a recursive, structured fork/join. It can expose fan-out parallelism and hide latency without futures or async syntax.

5. **Execution model**: the language does not say how independent products are evaluated. Sequential and concurrent evaluation are both valid, provided the programmer declared effect independence correctly. Replay records effects and their grouping and accepts any order inside a product.

6. **Cancellation policy**: when one branch of a `?!` product fails, its siblings may already be running or finished. What the runtime does next is set in `aver.toml`:

   ```toml
   [independence]
   mode = "complete"   # default — all branches run to completion
   # mode = "cancel"   # signal siblings to stop on first error
   ```

   - **`complete`** (default): the runtime lets every branch finish and picks one error. So `?!` over effectful terms may do speculative work. A sibling's effect can run even though its result is thrown away because another branch failed.
   - **`cancel`**: when one branch fails, the runtime sets a shared cancellation flag. Sibling branches check it at intervals and stop early with a cancellation error. Effects that have already started still complete, because cancellation is cooperative and never preempts. Once a sibling sees the flag, it starts no new work. Cancel mode cuts wasted compute but not wasted I/O wait. A branch blocked in a kernel syscall (e.g. an HTTP request with a long timeout) sees the flag only after the syscall returns.

## `aver check` hazard heuristics

`aver check` emits `warning[independence-hazard]` when two branches of an independent product use effects that are likely unsafe or nondeterministic once reordered or overlapped.

The heuristic is deliberately small and conservative for now:

- Any mix of `Console.*` and `Terminal.*` warns. That covers pairs across the two namespaces, such as `Console.print` with `Terminal.flush`, since both write to the same terminal/output channel.
- Any pair of `Tcp.*` effects warns.
- `Disk.*` warns when at least one side is mutating: `writeText`, `appendText`, `writeBytes`, `appendBytes`, `delete`, `deleteDir`, `makeDir`, `sync`. `sync` writes no new content, but it is an ordering barrier. Making a new file durable means syncing the file and then its parent directory, and an independent product does not keep that order.
- `Http.*` warns when at least one side is mutating: `post`, `put`, `patch`, `delete`.
- `Env.*` warns when at least one side is mutating: `set`.
- Whole-namespace effects such as `! [Console]` or `! [Disk]` follow the same rules.

This is a heuristic and proves nothing. It does not yet reason about which concrete resource is touched, such as "same file path" or "same environment key". Treat it as a prompt to review. If the pattern is intended, suppress the warning with `[[check.suppress]]` and a reason in `aver.toml`.

### Serve-path warnings

`aver check` also emits `warning[serve-path]` when a poll loop hands one of its turns to an effectful loop. A function that calls a wait directly (`Tcp.poll`, or the `Wait.poll` the generated coordinator performs) is one turn of an event loop. Recursive loops are found in the module's call graph after removing every function that calls `Tcp.poll` directly, the poller included. A loop that goes through a poller goes through its wait and disappears with it. A loop that avoids every wait is still there after the cut. The walk starts at the poller's callees, never goes back into the poller, and stops at any function that calls `Tcp.poll` directly itself, since that is the next turn and not a stall. Suppose the walk reaches a function that is recursive in the reduced graph and declares an input operation among its effects: `Disk.read*`, `Disk.size`, `Disk.listDir`, `Disk.exists`, `Tcp.read*`, `Tcp.accept`, `Tcp.dialled`, `Tcp.peerAddress`, the dialling and round-tripping `Tcp.send`, `Tcp.sendBytes`, `Tcp.ping` and `Tcp.connect`, or a bare `Disk` or `Tcp`. That function runs to completion before the next wait, and no peer that became ready in the meantime is served until it returns. Writes alone (`Disk.write*`, `Disk.append*`, `Tcp.write*`, `Tcp.close`) do not count. A loop that only writes what it already holds is bounded by this turn's data, and `[verify] turn-budget` covers its length. One more shape is exempt: a loop over a list it was handed, where every recursive call in the loop passes back the `rest` of a `[_, ..rest]` match on that parameter, in the same position. A server serves the keys a poll returned this way, one key per step, and the loop ends when the list does. A loop that recurses on a counter, on a value read from the world, or on anything else is not exempt. The condition is purely structural: the call graph, its recursive components, the declared effect sets, and the syntactic shape of the recursive calls. The warning is placed on the poller's call into the path, once per (poller, loop) pair. To fix it, do one step of the loop per turn, or run the loop as its own command. `aver verify` can measure the same thing at run time with `[verify] turn-budget`. The check looks for a direct call to `Tcp.poll`. A wrapper that only forwards to `Tcp.poll` counts as the poller, so a loop started after such a wrapper in its caller goes unseen. This is a known limitation. As with the hazard heuristics, suppress an intended case with `[[check.suppress]]` and a reason.

## Examples

### Flat: multiple independent effects

```aver
fn loadDashboard(userId: String) -> Result<Dashboard, String>
    ? "Loads profile and settings independently."
    ! [Http.get, Disk.readText]
    data = (fetchProfile(userId), loadSettings(userId))?!
    match data
        (profile, settings) -> Result.Ok(Dashboard(profile = profile, settings = settings))
```

### Recursive fan-out over a list

```aver
fn fetchStep(url: String, rest: List<String>) -> Result<List<String>, String>
    ? "Fetches one URL and the rest independently."
    ! [Http.get]
    data = (fetchOne(url), fetchAll(rest))?!
    match data
        (body, others) -> Result.Ok(List.prepend(body, others))

fn fetchAll(urls: List<String>) -> Result<List<String>, String>
    ? "Fetches all URLs via recursive fan-out."
    ! [Http.get]
    match urls
        [] -> Result.Ok([])
        [url, ..rest] -> fetchStep(url, rest)
```

### Partial success with bare `!`

```aver
fn loadWithFallback(userId: String) -> String
    ? "Loads data, handles partial failures gracefully."
    ! [Http.get, Disk.readText]
    results = (fetchProfile(userId), loadSettings(userId))!
    match results
        (Result.Ok(profile), Result.Ok(settings)) -> "both: {profile}"
        (Result.Ok(profile), Result.Err(_)) -> "profile only: {profile}"
        (Result.Err(_), Result.Ok(settings)) -> "settings only: {settings}"
        (Result.Err(_), Result.Err(_)) -> "nothing loaded"
```

### Pipeline parallelism (double buffering)

```aver
fn pipelineContinue(ready: String, remaining: List<String>) -> Result<Unit, String>
    ? "Processes a ready result while fetching the next."
    ! [Http.get, Console.print]
    match remaining
        [] -> process(ready)
        [url, ..rest] ->
            data = (process(ready), fetchOne(url))?!
            match data
                (_, nextBody) -> pipelineContinue(nextBody, rest)
```

`process(ready)` and `fetchOne(url)` form an independent product, so item N is consumed while item N+1 is produced. Each recursive step overlaps one unit of consumption with one unit of production. There is no incremental delivery mechanism, so this does not count as streaming.

### Static-width windowing (bounded concurrency)

```aver
fn fetchThree(a: String, b: String, c: String) -> (Result<String, String>, Result<String, String>, Result<String, String>)
    ? "Fetch three URLs concurrently."
    ! [Http.get]
    (fetchOne(a), fetchOne(b), fetchOne(c))?!

fn processInWindows(urls: List<String>) -> Result<Unit, String>
    ? "Fetch at most 3 concurrently; recurse on the tail."
    ! [Http.get, Console.print]
    match urls
        [] -> Result.Ok(Unit)
        [a, b, c, ..rest] ->
            match fetchThree(a, b, c)
                (Result.Ok(_), Result.Ok(_), Result.Ok(_)) -> processInWindows(rest)
                _ -> Result.Err("partial failure in window")
        [a, b] -> match (fetchOne(a), fetchOne(b))?!
            (Result.Ok(_), Result.Ok(_)) -> Result.Ok(Unit)
            _ -> Result.Err("partial failure")
        [a] -> match fetchOne(a)
            Result.Ok(_) -> Result.Ok(Unit)
            Result.Err(e) -> Result.Err(e)
```

The window size is a compile-time constant (3 here). At runtime, recursive pattern matching cuts the list into chunks and hands each chunk to a fixed-arity `?!`. Concurrency is bounded by the window size, with no runtime setting.

### What these patterns are

Both patterns come from combining independent products with sequential control flow. Neither adds a semantic primitive.

- **Pipeline parallelism**: overlap production and consumption with `?!` + recursion.
- **Static-width windowing**: chunk a list by pattern and apply a fixed-arity `?!` to each chunk, which bounds concurrency.

They do not give element-by-element streaming, demand-driven backpressure, channel-based communication, or dynamic-arity fan-out. `?!` has a static shape: the number of concurrent branches is fixed at the call site.

## What Aver does not have

Tasks, futures, async/await, channels, streams, thread pools and executors are not language concepts in Aver. The language has products and independence, and the runtime picks the execution strategy.

## Why this works

Products describe the shape of a computation. `!` and `?!` also declare that the runtime may use the independence when it picks an evaluation schedule.

Fixed-width fan-out and recursive dynamic fan-out use the same expression-form operator. Aver has no separate user-facing concurrency construct for either case.
