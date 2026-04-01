# Independent Products

## What `!` means

A tuple denotes a product of values.
A tuple followed by `!` denotes a product of independent computations.
A tuple followed by `?!` denotes a product of independent Result computations with error propagation.

## Core definitions

**`(a, b)!`** — product of independent computations.

For pure computations, independence follows structurally: tuple elements have no data dependency on each other, and under Aver's core restrictions (no mutation, no closures, no shared state) they cannot interfere.

For effectful computations, `!` is a declaration by the author that the effects of the elements are safe to reorder or execute concurrently. The compiler checks shape and types, but does not prove effect commutativity.

The runtime may therefore evaluate elements sequentially, in any order permitted by the execution model, or concurrently.

**`(a, b)?!`** — product of independent Result computations.

If all elements produce `Ok`, the result is the tuple of unwrapped values. If one or more elements produce `Err`, evaluation fails with one error chosen by the execution schedule, or by replay policy when replay is enabled.

In sequential execution, this choice is deterministic. In parallel execution, it may be nondeterministic.

**`(a, b)`** — product of values. Standard tuple semantics. No independence claim.

## Soundness envelope

This construct is sound by construction for pure terms. For effectful terms, correctness depends on the author's declaration that the participating effects are non-interfering with respect to the program's intended observable behavior.

## Structural properties

1. **Structural independence** — tuple elements cannot reference each other. There is no binding site inside a tuple expression that could make one element visible to another.

2. **Composition** — `!` products compose exactly like tuples:
   - Nested: `(a, (b, c)!)!`
   - Recursive: `(f(x), g(xs))?!`
   - Flat: `(a, b, c, d)?!`

3. **Error algebra** — `?` lifts through the product. `(a, b)?!` evaluates the `!` product, then applies `?` to each component. In sequential execution, the first `Err` (left-to-right) propagates. In parallel execution, any produced `Err` may propagate.

4. **Recursion builds products** — a recursive function over a list constructs a product at each step: the computation for the current element and the computation for the rest. With `?!`, this gives recursive structured fork/join, which can expose fan-out parallelism and latency hiding without introducing futures or async syntax.

5. **Execution model** — the language does not prescribe how independent products are evaluated. Sequential and concurrent evaluation are both valid implementations, given that the programmer has correctly declared effect independence. Replay records effects and their grouping, accepting any order within a product.

6. **Cancellation policy** — when one branch of a `?!` product fails, sibling branches may already be in flight or completed. The runtime lets them finish and chooses one error. There is no cancellation of in-flight work. This means `?!` on effectful terms may perform speculative work: a sibling effect can execute even if its result is ultimately discarded due to another branch's failure.

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

### Windowed parallel fetch (bounded concurrency)

```aver
fn processInWindows(urls: List<String>, windowSize: Int) -> Result<Unit, String>
    ! [Http.get, Console.print]
    match urls
        [] -> Result.Ok(Unit)
        _ ->
            bodies = fetchAllParallel(List.take(urls, windowSize))?!
            processAll(bodies)?
            processInWindows(List.drop(urls, windowSize), windowSize)
```

`windowSize` bounds the number of concurrent effects — at most N in flight at any time. This is bounded concurrency, not backpressure (the producer does not wait for a demand signal from the consumer).

### What these patterns are

These patterns are not new semantic primitives; they emerge from composing independent products with sequential control flow:

- **Pipeline parallelism** — overlap production and consumption via `?!` + recursion
- **Bounded concurrency** — limit fan-out via window size parameter
- **Batched concurrent processing** — parallel fetch within window, sequential between windows

These are not: element-by-element streaming, demand-driven backpressure, or channel-based communication.

## What Aver does not have

Aver does not have tasks, futures, async/await, channels, streams, thread pools, or executors as language concepts. It has products and independence. The runtime handles execution strategy.

## Why this works

Products describe computation shape. `!` and `?!` additionally declare that the runtime may exploit independence when choosing an evaluation schedule.

Aver uses the same expression-form operator for fixed-width and recursive dynamic fan-out, instead of exposing separate user-facing concurrency constructs for those cases.
