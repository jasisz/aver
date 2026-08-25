# Independent Products

## What `!` means

A tuple denotes a product of values.
A tuple followed by `!` denotes a product of independent computations.
A tuple followed by `?!` denotes a product of independent Result computations with error propagation.

## Core definitions

**`(a, b)!`** — product of independent computations.

For pure computations, independence follows structurally: tuple elements have no data dependency on each other, and under Aver's core restrictions (no mutation, no closures, no shared state) they cannot interfere.

For effectful computations, `!` is a declaration by the author that the effects of the elements are safe to reorder or execute concurrently. The compiler checks shape and types, but does not prove effect commutativity.

The runtime may therefore evaluate elements sequentially (left-to-right) or concurrently.

**`(a, b)?!`** — product of independent Result computations.

`(a, b)?!` is the independent product of `Result` computations. If all branches produce `Ok`, the result is the tuple of unwrapped values. If one or more branches produce `Err`, the product evaluates to `Err`. The propagated error is selected deterministically in left-to-right order.

**`(a, b)`** — product of values. Standard tuple semantics. No independence claim.

## Soundness envelope

`!` is sound by construction only for pure terms. For effectful terms, it is an unchecked semantic contract: the programmer asserts that all schedules permitted by the execution mode are observationally acceptable.

## Formal semantics

Let τ range over observable effect traces. For an independent product, each branch is evaluated from the same incoming trace prefix. The runtime may choose any schedule consistent with the execution mode.

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

If multiple branches produce `Err`, the propagated error is the first `Err` in left-to-right order. The single-failure rules generalize to multiple failures by replacing `Err(e)` with the `Err` from the leftmost branch that produces one.

**Bare `!`:**

```
⟨a, τ₀⟩ ⇓ ⟨v₁, τ₀ · τ_a⟩
⟨b, τ₀⟩ ⇓ ⟨v₂, τ₀ · τ_b⟩
τ ∈ Interleave(τ_a, τ_b)
────────────────────────────────────────────
⟨(a, b)!, τ₀⟩ ⇓ ⟨(v₁, v₂), τ₀ · τ⟩
```

**Replay invariant:** replay records tuples of `(group_id, branch_path, effect_occurrence, effect_type, effect_args, result)`. Within a group, matching is by `(group_id, branch_path, effect_occurrence, effect_type, effect_args)`, not by position in the execution schedule. `branch_path` is a dotted path encoding the branch position within nested products (e.g. `"0.1"` = branch 0 of outer product, branch 1 of inner). `effect_occurrence` is the per-branch ordinal of effect emission (0-based), disambiguating multiple emissions of the same effect within a single branch. Together these make replay deterministic with respect to branch identity and repeated effects within nested and recursive compositions. Reordering within an independent product does not invalidate replay.

**Cancellation and error priority:** a cancellation error is an execution artifact, not a primary failure. When `?!` unwraps results, a real `Result.Err` from a branch always takes priority over a cancellation error from a sibling. A cancellation error propagates only if no branch produced a real `Err`.

**Backend coverage:** the VM backend and the compiled Rust backend (`aver compile`) both implement cooperative cancellation. The VM checks periodically while executing bytecode. The compiled Rust backend checks at generated function boundaries and before effectful builtins. Error selection is deterministic (left-to-right) across backends. Replay `branch_path` and `effect_occurrence` are recorded in all backends that support replay.

## Structural properties

1. **Structural independence** — tuple elements cannot reference each other. There is no binding site inside a tuple expression that could make one element visible to another.

2. **Composition** — `!` products compose exactly like tuples:
   - Nested: `(a, (b, c)!)!`
   - Recursive: `(f(x), g(xs))?!`
   - Flat: `(a, b, c, d)?!`

3. **Error algebra** — `(a, b)?!` is the independent product of `Result` computations. If all branches produce `Ok`, the result is the tuple of unwrapped values. If one or more branches produce `Err`, the product evaluates to `Err` according to the error selection policy described in Core definitions.

4. **Recursion builds products** — a recursive function over a list constructs a product at each step: the computation for the current element and the computation for the rest. With `?!`, this gives recursive structured fork/join, which can expose fan-out parallelism and latency hiding without introducing futures or async syntax.

5. **Execution model** — the language does not prescribe how independent products are evaluated. Sequential and concurrent evaluation are both valid implementations, given that the programmer has correctly declared effect independence. Replay records effects and their grouping, accepting any order within a product.

6. **Cancellation policy** — when one branch of a `?!` product fails, sibling branches may already be in flight or completed. The runtime behaviour is configurable via `aver.toml`:

   ```toml
   [independence]
   mode = "complete"   # default — all branches run to completion
   # mode = "cancel"   # signal siblings to stop on first error
   ```

   - **`complete`** (default) — the runtime lets all branches finish and chooses one error. This means `?!` on effectful terms may perform speculative work: a sibling effect can execute even if its result is ultimately discarded due to another branch's failure.
   - **`cancel`** — the runtime sets a shared cancellation flag when one branch fails. Sibling branches check this flag periodically and bail early with a cancellation error. Effects that already started will complete (cancellation is cooperative, not preemptive). Sibling branches stop initiating further work after they observe the cancellation flag. Cancel mode reduces wasted compute but cannot reduce wasted I/O wait: a branch blocked in a kernel syscall (e.g. an HTTP request with a long timeout) will not observe the flag until the syscall returns.

## `aver check` hazard heuristics

`aver check` emits `warning[independence-hazard]` when a branch pair in an independent product uses effects that are likely to be unsafe or nondeterministic when reordered or overlapped.

Today the heuristic is intentionally small and conservative:

- Any mix of `Console.*` and `Terminal.*` warns. This includes cross-namespace pairs like `Console.print` with `Terminal.flush`, because both share the terminal/output channel.
- Any pair of `Tcp.*` effects warns.
- `Disk.*` warns when at least one side is mutating: `writeText`, `appendText`, `delete`, `deleteDir`, `makeDir`.
- `Http.*` warns when at least one side is mutating: `post`, `put`, `patch`, `delete`.
- `Env.*` warns when at least one side is mutating: `set`.
- Broad namespace effects such as `! [Console]` or `! [Disk]` participate in the same rules.

This is a heuristic, not a proof system. It does not yet reason about concrete resource identity such as "same file path" or "same environment key". Use it as a review signal: if the warning is intentional, suppress it with `[[check.suppress]]` and a reason in `aver.toml`.

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

`process(ready)` and `fetchOne(url)` form an independent product: item N is consumed while item N+1 is produced. This is not streaming — there is no incremental delivery mechanism. It is structured overlap: each recursive step overlaps one unit of consumption with one unit of production.

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

The window size is a compile-time constant (3 here) — the list is chunked at runtime via recursive pattern matching, each chunk handed to a fixed-arity `?!`. Concurrency is bounded to the window size without a runtime knob.

### What these patterns are

These patterns are not new semantic primitives; they emerge from composing independent products with sequential control flow:

- **Pipeline parallelism** — overlap production and consumption via `?!` + recursion
- **Static-width windowing** — chunk a list by pattern, apply a fixed-arity `?!` per chunk, bounded concurrency falls out

These are not: element-by-element streaming, demand-driven backpressure, channel-based communication, or dynamic-arity fan-out (`?!` is statically shaped — the number of concurrent branches is fixed at the call site).

## What Aver does not have

Aver does not have tasks, futures, async/await, channels, streams, thread pools, or executors as language concepts. It has products and independence. The runtime handles execution strategy.

## Why this works

Products describe computation shape. `!` and `?!` additionally declare that the runtime may exploit independence when choosing an evaluation schedule.

Aver uses the same expression-form operator for fixed-width and recursive dynamic fan-out, instead of exposing separate user-facing concurrency constructs for those cases.
