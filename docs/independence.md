# Independent Products

## What `!` means

A tuple denotes a product of values.
A tuple followed by `!` denotes a product of independent computations.
A tuple followed by `?!` denotes a product of independent Result computations with error propagation.

## Core definitions

1. **`(a, b)`** — product of values. Both `a` and `b` are evaluated; result is a tuple. Standard semantics.

2. **`(a, b)!`** — product of independent computations. `a` and `b` are computations with no data dependency between them. The language guarantees they cannot observe each other. The runtime may evaluate them in any order or concurrently. Result is a tuple of their outcomes.

3. **`(a, b)?!`** — product of independent Result computations. Same as `!`, but each element must produce a `Result<T, E>`. All elements are evaluated; if any yields `Err`, the first error propagates. If all yield `Ok`, the unwrapped values form the result tuple.

4. **Structural independence** — tuple elements cannot reference each other. This is enforced by construction: there is no binding site inside a tuple expression that could make one element visible to another. Independence is not annotated; it is a consequence of the syntax.

5. **Composition** — `!` products compose exactly like tuples:
   - Nested: `(a, (b, c)!)!` — outer product of `a` and an inner independent product.
   - Recursive: `(f(x), g(xs))?!` — product of a computation and a recursive continuation.
   - Flat: `(a, b, c, d)?!` — product of N independent computations.

6. **Recursion builds products** — a recursive function over a list naturally constructs a product at each step: the computation for the current element and the computation for the rest. With `?!`, this gives fan-out parallelism, streaming, and backpressure — without introducing any of these as language concepts.

7. **Error algebra** — `?` lifts through the product. `(a, b)?!` is equivalent to: evaluate the `!` product, then apply `?` to each component. First `Err` wins. This is the same `?` that works on single expressions, extended to products.

8. **Execution model** — the language does not prescribe how independent products are evaluated. Sequential evaluation is correct. Concurrent evaluation is correct. The runtime chooses. Replay records the effects and their grouping, accepting any order within a product.

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

### Windowed streaming

```aver
fn processAllInWindow(window: List<Item>) -> Result<List<Processed>, String>
    ? "All items in one window are independent."
    ! [Process.item]
    match window
        [] -> Result.Ok([])
        [item, ..rest] ->
            data = (processItem(item), processAllInWindow(rest))?!
            match data
                (p, ps) -> Result.Ok(List.prepend(p, ps))

fn processWindowed(items: List<Item>, windowSize: Int) -> Result<List<Processed>, String>
    ? "Process items in sliding windows."
    ! [Process.item]
    match splitIntoWindows(items, windowSize)
        [] -> Result.Ok([])
        [window, ..remaining] ->
            data = (processAllInWindow(window), processWindowed(flatten(remaining), windowSize))?!
            match data
                (processedWindow, processedRest) -> Result.Ok(List.append(processedWindow, processedRest))
```

## What Aver does not have

Aver does not have tasks, futures, async/await, channels, streams, thread pools, or executors as language concepts. It has products and independence. The runtime handles the rest.

## Why this works

Products are compositional, nestable, recursive, and structural. They are not an execution strategy — they are a shape of computation. Aver expresses shapes, not commands.
