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

## What Aver does not have

Aver does not have tasks, futures, async/await, channels, streams, thread pools, or executors as language concepts. It has products and independence. The runtime handles execution strategy.

## Why this works

Products are compositional, nestable, recursive, and structural. They are not an execution strategy — they are a shape of computation. Aver expresses shapes, not commands.
