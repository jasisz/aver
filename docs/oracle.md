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

## Trace-aware verify

```aver
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int
    ? "Deterministic Random.int stub."
    4

fn pickOne() -> Int
    ? "Rolls once."
    ! [Random.int]
    Random.int(1, 6)

verify pickOne trace
    given rnd: Random.int = [fairDie]
    pickOne().result => rnd(BranchPath.Root, 0, 1, 6)
```

Breakdown:

- `verify pickOne trace` enables `.result` and `.trace` projections.
- `given rnd: Random.int = [fairDie]` redirects `Random.int` to `fairDie` for this verify block.
- `rnd` is a local alias you can call in case expressions.
- `pickOne().result` is the function's return value.
- `pickOne().trace` is the collected trace of classified emissions.

The stub is not special syntax. It is just an Aver function whose type matches the oracle signature for the effect.

## Result-only laws

Oracle also works without trace assertions:

```aver
fn pickOneSpec(path: BranchPath, rnd: Fn(BranchPath, Int, Int, Int) -> Int) -> Int
    ? "One draw at the caller's path."
    rnd(path, 0, 1, 6)

verify pickOne law consistent
    given rnd: Random.int = [fairDie]
    pickOne() => pickOneSpec(BranchPath.Root, rnd)
```

This is the proof-oriented style: the implementation uses effects, the spec receives explicit oracle functions, and the law connects them.

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
    top().trace.length() => 1
    top().trace.contains(Console.print("direct")) => true
    top().trace.contains(Console.print("via-helper")) => false
```

Verify a helper's trace separately when the helper's own emissions matter.

## Proof export

`aver proof` lifts classified effectful functions to pure proof functions by adding explicit oracle/capability parameters. Generated Lean and Dafny files include a trust-assumption header for the runtime/compiler trace invariant.

Supported law shapes can become universal theorems. Concrete `given` domains still produce executable/sample checks. Unsupported proof shapes should fail clearly or remain as checked-domain/sample obligations, depending on backend and verify mode.

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
- Trace-aware verify on recursive effectful functions. Use a result-only law, or move the effect-emitting step into a non-recursive function and verify that trace.
- Machine-checked proof of the compiler/runtime trace invariant. Generated proof files state the assumptions explicitly.
