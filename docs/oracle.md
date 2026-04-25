# Oracle: verifying effectful functions

Oracle is Aver's bridge between effects and `verify` / `proof`.

Instead of hiding effects behind mocks or relying on a replay file, a verify block names the effect explicitly and provides ordinary Aver functions as stubs. The verified function runs under those stubs. If the block uses `trace`, assertions can also inspect the classified effects the function emitted.

Use Oracle when:

- the function has a small, explicit effect surface
- the effects are in the classified built-in set below
- a deterministic stub describes the world you want to prove against
- the assertion should live next to the function, not in an external recording

Use record/replay when the flow depends on ambient mutable state, modal terminal
state, long-running protocols, or lifecycle invariants rather than one
observable call/result.

Runnable example: `examples/formal/oracle_trace.av`.

## Oracle laws

```aver
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int
    ? "Deterministic Random.int stub."
    4

fn pickOne() -> Int
    ? "Rolls once."
    ! [Random.int]
    Random.int(1, 6)

verify pickOne law usesOracle
    given rnd: Random.int = [fairDie]
    pickOne() => rnd(BranchPath.Root, 0, 1, 6)
```

Breakdown:

- `verify pickOne law usesOracle` is the proof-oriented Oracle form.
- `given rnd: Random.int = [fairDie]` redirects `Random.int` to `fairDie` for this verify block.
- `rnd` is a local alias for the oracle; the law can call it directly.
- `aver proof` can lift `pickOne` to a pure proof function and quantify over that oracle.

The stub is not special syntax. It is just an Aver function whose type matches the oracle signature for the effect.

The generated Lean shape is:

```lean
theorem pickOne_law_usesOracle :
    ∀ (rnd : BranchPath → Int → Int → Int → Int),
        pickOne BranchPath.Root rnd = rnd BranchPath.Root 0 1 6 := by
    intro rnd
    simp [pickOne]
```

Named spec functions are still useful for larger laws. For the simple one-call case, keeping the oracle call inline is clearer.

## Trace-aware cases

Use cases-form `verify <fn> trace` when you want runtime assertions over the collected trace:

```aver
verify pickOne trace
    given rnd: Random.int = [fairDie]
    picked = pickOne()
    picked.result => rnd(BranchPath.Root, 0, 1, 6)
    picked.trace.length() => 1
    picked.trace.contains(Random.int(1, 6)) => true
```

Here `.result` is the function's return value under the stub, and `.trace` is the collected trace of classified emissions. These trace projections are runtime checks; they are not the same thing as a universal theorem over all oracles.

## Effect classification

Oracle has a fixed built-in effect set:

| Namespace | Method | Dimension |
|---|---|---|
| `Args` | `get` | snapshot |
| `Env` | `get` | snapshot |
| `Console` | `readLine` | generative |
| `Random` | `int`, `float` | generative |
| `Time` | `now`, `unixMs` | generative |
| `Time` | `sleep` | output |
| `Disk` | `readText`, `exists`, `listDir` | generative |
| `Disk` | `writeText`, `appendText`, `delete`, `deleteDir`, `makeDir` | generative + output |
| `Http` | `get`, `head`, `delete`, `post`, `put`, `patch` | generative + output |
| `Tcp` | `send`, `ping` | generative + output |
| `Console` | `print`, `error`, `warn` | output |
| `Terminal` | `readKey` | generative |
| `Terminal` | `clear`, `moveTo`, `print`, `hideCursor`, `showCursor`, `flush` | output |

Anything outside this set is not modeled by Oracle.

Important boundary details:

- `Console.readLine` and `Terminal.readKey` are modeled as generative input:
  the proof receives a deterministic oracle value for each call.
- Mutating `Disk.*` calls are modeled as operation/result effects: the requested
  operation is emitted to the trace, and success/failure comes from the oracle.
  Oracle does not assert persistent filesystem state after the operation.
- One-shot `Tcp.send` / `Tcp.ping` follow the same request/result shape. Stateful
  TCP sessions are not in this model.
- Terminal drawing calls are output trace events. Terminal mode and color state
  are not modeled.

## Stub signatures

Stub signatures are derived from the effect dimension:

### Snapshot

Snapshot stubs keep the runtime signature unchanged.

```aver
fn stubArgs() -> List<String>
fn stubEnv(key: String) -> Option<String>
```

### Generative and generative + output

Generative stubs receive a leading `BranchPath` and per-branch call counter.

```aver
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int

fn fakeFetch(path: BranchPath, n: Int, url: String)
    -> Result<HttpResponse, String>
```

The original effect arguments are appended after `(path, n)`.

### Output

Output effects do not take stubs. Assert them through `.trace`:

```aver
verify hello trace
    hello().trace.contains(Console.print("rolled")) => true
```

`given out: Console.print = [...]` is rejected because output effects have no return value to replace.

## Multiple stubs

A `given` list is a concrete domain:

```aver
verify pickOne trace
    given rnd: Random.int = [lowDie, highDie]
    pickOne().result => rnd(BranchPath.Root, 0, 1, 6)
```

This expands to two cases. Multiple `given` lists expand as a cartesian product, capped at `10_000` cases. Stub names may be local (`lowDie`) or qualified imports (`Helpers.lowDie`).

## Trace API

Trace projections are only available inside `verify <fn> trace`.

```aver
fn().trace                    -- Trace
fn().trace.length()           -- Int
fn().trace.event(k)           -- Option<EffectEvent>
fn().trace.contains(eventLit) -- Bool
```

Tree navigation for `!` / `?!` groups:

```aver
fn().trace.group(n)
fn().trace.group(n).branch(i)
fn().trace.group(n).branch(i).event(k)
fn().trace.group(n).branch(i).length()
```

Indices are 0-based in source order.

## EffectEvent

```aver
EffectEvent(method: String, args: List<Unknown>, path: String)
```

`path` is the structural branch position:

- `""` means sequential/root
- `"0"` means branch 0 of a group
- `"0.1"` means branch 1 of a group nested inside branch 0

`BranchPath.parse(ev.path)` converts the string back to the opaque `BranchPath` type used by generative stubs and specs.

There are two comparison styles:

- `.trace.contains(Console.print("x"))` checks whether that event happened anywhere and ignores `path`.
- `.trace.event(0) => Option.Some(EffectEvent(...))` is strict structural equality and includes `path`.

This keeps common assertions readable while preserving exact event checks when you need them.

## Helper boundary

`verify <fn> trace` records direct emissions from the verified function. Emissions from helper functions it calls are suppressed and do not leak to stdout during `aver verify`.

```aver
fn helper(msg: String) -> Unit
    ! [Console.print]
    Console.print(msg)

fn top() -> Int
    ! [Console.print]
    Console.print("direct")
    helper("via-helper")
    42

verify top trace
    traced = top()
    traced.trace.length() => 1
    traced.trace.contains(Console.print("direct")) => true
    traced.trace.contains(Console.print("via-helper")) => false
```

Verify a helper's trace separately when the helper's own emissions matter.

## Proof export

`aver proof` lifts classified effectful functions to pure proof functions by adding explicit oracle/capability parameters. Generated Lean and Dafny files include a trust-assumption header for the runtime/compiler trace invariant.

Supported law shapes can become universal theorems. Concrete `given` domains still produce executable/sample checks. Unsupported proof shapes should fail clearly or remain as checked-domain/sample obligations, depending on backend and verify mode.

### `aver verify` vs `aver proof` — the same `verify` block, two different questions

A `verify <fn> law` block does double duty:

- `aver verify` runs it as a **finite sample check**: the cartesian product of the `given` domains is enumerated (capped at 10,000 cases) and evaluated against the law's RHS using whatever stubs you supplied.
- `aver proof` exports the same block as a **universally quantified theorem** in Lean / Dafny, where every classified effect becomes a function parameter and the law is asserted *for every possible such function* — not just for the stubs in `given`.

These two questions can have different answers on the same block. The canonical example is `examples/formal/randomness_paradox.av`:

```aver
fn distinctStub(path: BranchPath, n: Int) -> Float
    Float.fromInt(n) + 1.0

fn twoFloatsDistinct() -> Bool
    ! [Random.float]
    a = Random.float()
    b = Random.float()
    a != b

verify twoFloatsDistinct law alwaysDistinct
    given rnd: Random.float = [distinctStub]
    twoFloatsDistinct() => true
```

`aver verify` passes — under `distinctStub` the two calls return `1.0` and `2.0`, the law's RHS holds.

`aver proof` exports a theorem of shape `∀ rnd, twoFloatsDistinct rnd = true`, and both backends reject it for the same reason: there exist oracles (e.g. `fun _ _ => 0.5`) for which both calls return the same value, making the law false.

- `--backend lean` + `lake build` → `unsolved goals: (rnd BranchPath.Root 0 != rnd BranchPath.Root 1) = true`
- `--backend dafny` + `dafny verify` → `a postcondition could not be proved on this return path: ensures twoFloatsDistinct(BranchPath_Root, rnd) == true`

This is not a bug — it's the design. `verify` answers "does this hold for the stubs I wrote down?". `proof` answers "does this hold for every classified-effect implementation that has the right signature?". The second is strictly stronger and catches what the first cannot.

When a `verify` passes but `aver proof` rejects, the law is **stub-specific** — true under the chosen stubs, not universal. Either rewrite the law so it doesn't depend on hidden stub structure (e.g. assert against `rnd(...)` directly instead of a constant), or keep it as a sample-only check and don't export. `verify <fn> trace` is the cases form when the goal is "given this concrete stub, here's what I expect"; it doesn't export and doesn't pretend to.

## Current limits

Oracle does not try to model every side effect.

Not supported:

- `Env.set`. Process environment is same-process mutable state, and Oracle v1
  does not model the relation between a set and a later `Env.get`.
- Persistent TCP sessions: `Tcp.connect`, `Tcp.writeLine`, `Tcp.readLine`, and
  `Tcp.close`. The one-shot `Tcp.send` / `Tcp.ping` shape is classified.
- `HttpServer.listen` / `listenWith`. Server lifecycle and callbacks are
  long-running protocols, not one call/result effects.
- Terminal modal state: `Terminal.enableRawMode`, `disableRawMode`, `setColor`,
  `resetColor`, and `size`. Drawing calls and `readKey` are classified.
- Proof export for `?!` cancel mode. Oracle proof export expects complete independence mode so every branch has a stable trace position.
- Higher-order effectful callbacks. Oracle works best when the effect surface is visible in the verified function's signature.
- Trace-aware laws on recursive effectful functions. Use `verify <fn> law ...` without `trace`, or move the effect-emitting step into a non-recursive function and verify that trace.
- Machine-checked proof of the compiler/runtime trace invariant. Generated proof files state the assumptions explicitly.
