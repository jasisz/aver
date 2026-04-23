# Oracle — proving effectful functions

Oracle brings effectful functions into `aver proof` / `aver verify`. Previously, effectful fns were silently skipped by proof export; Oracle lifts them to a pure form with per-dimension semantics (snapshot / generative / output) and a branch-witness tree for `!` / `?!` (complete mode only).

This is a reference. For design context and theoretical background, read `.claude/plans/oracle.md`. For the surface language, read `docs/language.md`.

## The shape of a trace-aware law

```
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int
    ? "always 4"
    4

fn hello() -> Int
    ? "roll + print"
    ! [Random.int, Console.print]
    x = Random.int(1, 6)
    Console.print("rolled 4")
    x

verify hello trace
    given rnd: Random.int = [fairDie]
    expected = 4
    hello().result => expected
    hello().trace.length() => 2
    hello().trace.contains(Console.print("rolled 4")) => true
```

Breakdown:

- `verify hello trace` — cases-form verify with `trace` enables the trace-aware projections below.
- `given rnd: Random.int = [fairDie]` — oracle binding. `Random.int` is the effect reference (type position only); `fairDie` is the Aver fn the VM redirects to. Alias `rnd` is also usable in case expressions.
- `expected = 4` — block-scoped local binding. Substituted into every case's LHS / RHS.
- `hello().result` — the fn's return value (identity projection).
- `hello().trace.length() / .event(k) / .contains(_)` — tree-flat projections over every classified emission the fn makes.

## Effect classification (closed set)

| Namespace | Method | Dimension |
|---|---|---|
| `Args` | `get` | snapshot |
| `Env` | `get` | snapshot |
| `Random` | `int`, `float` | generative |
| `Time` | `now`, `unixMs` | generative |
| `Disk` | `readText` | generative |
| `Http` | `get`, `head`, `delete`, `post`, `put`, `patch` | generative + output |
| `Console` | `print`, `error`, `warn` | output |

Anything outside this set — `Env.set`, `Disk.writeText`, `Tcp.*`, `HttpServer.*`, `Time.sleep`, `Console.readLine` — is rejected by trace-aware laws and by `aver proof`. Those remain replay-only.

## Stub signatures

Derived from the effect's dimension:

- **Snapshot**: stub has the runtime signature unchanged.
  ```
  fn stubArgs() -> List<String>
  fn stubEnv(key: String) -> Option<String>
  ```
- **Generative / generative+output**: stub takes a leading `(path: BranchPath, counter: Int)`.
  ```
  fn fairDie(path: BranchPath, counter: Int, min: Int, max: Int) -> Int
  fn stubHttp(path: BranchPath, counter: Int, url: String)
      -> Result<HttpResponse, String>
  ```
- **Output**: no oracle. The trace API (`.contains`, `.event`, `.length`) is how assertions about output effects are expressed; `given` bindings for output effects are rejected.

Stub-signature mismatches are caught at `aver check` time — e.g., pasting a generative-shaped stub into a snapshot `given` gives a clear diagnostic.

## Trace API

### Flat projections

```
fn().trace                    -- Trace record (wraps all events)
fn().trace.length()           -- Int: number of emissions
fn().trace.event(k)           -- Option<EffectEvent>: k-th emission
fn().trace.contains(eventLit) -- Bool: structural equality on method + args
```

### Tree navigation

```
fn().trace.group(N)                   -- sub-trace of N-th !/?! group in source order
fn().trace.group(N).branch(idx)       -- narrow to one branch
fn().trace.group(N).branch(idx).event(k)
fn().trace.group(N).length()
```

Indices are 0-based in source order. Runtime / replay-JSON coordinates (group ids starting at 1) stay invisible at this layer.

### Sugar vs plain comparison — the boundary

Two shapes, two jobs:

- **Sugar at effect-call position** — inside `.contains(...)` the argument is elaborated as an event predicate.
  ```
  fn().trace.contains(Console.print("rolled 4"))  -- full event literal
  fn().trace.contains(Console.print)              -- method-only predicate
  ```
  Context is unambiguous here — a `.contains(_)` argument can't mean anything else — so elaboration is contextual, not a dual-mode typechecker.

- **Strict match = plain record literal** — everywhere else, comparisons are ordinary Aver.
  ```
  fn().trace.event(0) =>
      Option.Some(EffectEvent(method = "Random.int",
                              args = [1, 6],
                              path = ""))
  ```
  No magic. User sees exactly what is being compared, `path` included. Destructuring goes through field access (`ev.method` / `ev.args` / `ev.path`) and nested `match`, not through pattern sugar.

Match patterns on `EffectEvent` (`match ev with Console.print(msg) -> …`) are **not** in Oracle v1. They're expressible today via field access; the pattern-position sugar is scheduled for Oracle v1.1 once the VM compiler extensions can be reviewed properly.

### The `EffectEvent` record

```
EffectEvent(method: String, args: List<Unknown>, path: String)
```

`path` is the dewey-decimal structural position (`""` for sequential, `"0.1"` for branch 1 of a group nested inside branch 0 of an outer group). `BranchPath.parse(ev.path)` round-trips to the opaque `BranchPath` type used in oracle signatures.

`.trace.contains(X)` ignores the `path` field when matching — the user-authored event literal (`Console.print("x")`) can't carry a position, so "this event fired somewhere" stays expressible. For position-specific matching use `.event(k) => Option.Some(EffectEvent(..., path = "..."))`.

### Helper boundary

Only effect emissions whose immediate caller is the fn under `verify <fn> trace` land in the trace. Emissions from functions that fn calls internally are ghost — neither recorded nor leaked to stdout under `aver verify`.

```
fn helper(msg: String) -> Unit
    ! [Console.print]
    Console.print(msg)

fn top() -> Int
    ! [Console.print]
    Console.print("direct")
    helper("via-helper")
    42

verify top trace
    top().trace.length() => 1                                   -- only "direct"
    top().trace.contains(Console.print("direct")) => true
    top().trace.contains(Console.print("via-helper")) => false
```

## Rejection rules

`aver check` / `aver verify` reject the following shapes with actionable diagnostics:

1. **Unclassified effects** in a trace-aware law or verify-law — stateful / interactive effects cannot be lifted; replay them via `aver run --record` + `aver replay --test` instead.
2. **Missing `given` stub** for a generative / generative+output effect the fn uses. Without a stub the real effect fires and the law checks against non-deterministic output.
3. **Wrong stub signature** — the given value's inferred type must match the effect's oracle signature (computed from its dimension).
4. **Trace-aware law on a recursive effectful fn** — the helper-boundary filter can't distinguish the outermost invocation from a recursive self-call. Drop the `trace` keyword for a result-only law, or refactor the effect-emitting work into a non-recursive helper.
5. **`?!` cancel mode** — `aver.toml` must have `[independence] mode = "complete"` for proof exports. Cancel mode is rejected by `aver proof`.

## Trust assumptions

Generated proof files (`.dfy`, `.lean`) carry a trust-assumption header listing every classified effect, the schedule-invariance compiler invariant, the three-lemma argument (branch locality + deterministic aggregation + runtime-provenance correspondence), and the explicit out-of-scope list (stateful effects, higher-order callbacks, cancel mode, etc.). The header is emitted identically across Dafny and Lean.

## Out of scope for Oracle v1

Deferred to later releases:

- **Relay**: `?!` cancel mode, higher-order effectful callbacks, user-definable effects (Aver has no user-defined effects today).
- **Ledger**: stateful effects via ghost `Map<K, V>` with a refinement relation tying runtime replay to the abstract model.
- **Post-v1**: mechanized meta-proof of the schedule-invariance invariant (à la CompCert / Iris).
