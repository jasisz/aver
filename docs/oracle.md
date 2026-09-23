# Oracle: verifying effectful functions

Oracle connects effects to `verify` / `proof`.

A verify block names each effect explicitly and supplies ordinary Aver functions as stubs for it. There are no mocks and no replay file. The verified function runs under those stubs. If the block uses `trace`, assertions can also inspect the classified effects the function emitted.

Use Oracle when:

- the function has a small, explicit effect surface
- the effects are in the classified built-in set below
- a deterministic stub describes the world you want to prove against
- the assertion should sit next to the function instead of in an external recording

Use record/replay when the flow depends on ambient mutable state, modal terminal state, long-running protocols, or lifecycle invariants, and one observable call/result does not capture it.

Runnable example: `examples/formal/oracle_trace.av`.

## Oracle laws

```aver
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>
    ? "Deterministic Random.int stub."
    Result.Ok(4)

fn pickOne() -> Int
    ? "Rolls once."
    ! [Random.int]
    Random.int(1, 6)

verify pickOne law usesOracle
    given rnd: Random.int = [fairDie]
    Result.Ok(pickOne()) => rnd(BranchPath.Root, 0, 1, 6)
```

Breakdown:

- `verify pickOne law usesOracle` is the Oracle form meant for proofs.
- `given rnd: Random.int = [fairDie]` sends `Random.int` to `fairDie` inside this verify block.
- `rnd` is a local alias for the oracle, and the law can call it directly.
- `aver proof` can lift `pickOne` to a pure proof function and quantify over that oracle.

The comparison keeps the provider outcome as a `Result`: `Result.Ok(pickOne()) => rnd(...)`. Do not turn the oracle response into a sample with `Result.withDefault`. After literal discharge an `Err` means the provider broke its contract. It does not license pretending that a chosen default came out of the random source.

The stub has no special syntax. It is an ordinary Aver function whose type matches the oracle signature for the effect.

Schematically, the generated Lean keeps that `Except` boundary visible:

```lean
theorem pickOne_law_usesOracle :
    ∀ (rnd : BranchPath → Int → Int → Int → Except String Int),
        Except.ok (pickOne BranchPath.Root rnd) = rnd BranchPath.Root 0 1 6 := by
    intro rnd
    simp [pickOne]
```

Named spec functions still help with larger laws. For a single call, the inline oracle call is clearer.

## Plain cases and capability stubs

Cases-form `verify` can bind a provider operation without making the case a law and without enabling trace projections:

```aver
fn publishedEmptyHash(input: List<Int>) -> String
    ? "Stand in for the provider at one published vector."
    "b472a266d0bd89c13706a4132ccfb16f7c3b9fcb"

fn reported(input: List<Int>) -> String
    ? "Call the host-provided hash seam."
    Hash160.digest(input)

verify reported
    given hash: Hash160.digest = [publishedEmptyHash]
    reported([]) => "b472a266d0bd89c13706a4132ccfb16f7c3b9fcb"
```

The `hash` alias does not have to appear in the assertion. For each expanded case, the selected function goes into the VM's operation-stub map before the left side runs. A pure capability stub has exactly the declared operation signature. An effectful generative capability keeps the Oracle signature `(BranchPath, Int, args...) -> result`. Several functions in the domain give separate cases.

The operation after `given name:` is always written as its full canonical path. If the capability is loaded as `Domain.Crypto.Hash160`, write `Domain.Crypto.Hash160.digest`, exactly as at the call site and in diagnostics. A shorter `Hash160.digest` is not passed through to runtime. Checking rejects it and, when the suffix identifies one loaded operation, suggests the canonical path.

The binding only applies inside the verify block. It does not install a package, run the Rust provider implementation, or satisfy provider preflight for `aver run` and compiled artifacts. Provider cryptography is still tested in the provider's own tests. The Aver case checks the contract shape and how the caller behaves given an explicit result.

A plain cases block may call a function whose signature declares effects, as long as that concrete case never reaches an effectful operation. Aver decides this at execution time, from what the case actually runs, and ignores the function-wide `! [...]` list for this. If the case does reach an effect without an exact `given`, verification stops before host dispatch. Add `trace` (and a `given` for an effect that returns a generated value), or move a stateful/interactive flow to record/replay. Plain verify is never a one-shot smoke test against the real world.

Proof export keeps the same distinction. A passing case with no `given` is stated against the Oracle-lifted function and quantified over the missing oracle, which proves that the selected branch gives the expected result for every provider implementation. A case with a `given` is stated against its selected stub instead. Capability paths keep their full module qualification throughout lifting, so an operation such as `Infra.Kv.get` becomes the matching oracle argument in the Lean artifact, never a host call.

A local yielding process also supports plain cases, given an exact stub for every request operation. These cases drive its generated protocol on the VM and do not call its live answer module. A request kind is numbered like any other operation: each request operation counts its own calls, in-place effects do not move the count, and a self yield uses no index at all. Direct process cases have no proof-export or WASM-stub model yet. See [testing a process](language.md#testing-a-process-with-request-stubs).

## Trace-aware cases

Use cases-form `verify <fn> trace` when a case is meant to reach classified effects, or when you want runtime assertions over the collected trace:

```aver
verify pickOne trace
    given rnd: Random.int = [fairDie]
    picked = pickOne()
    Result.Ok(picked.result) => rnd(BranchPath.Root, 0, 1, 6)
    picked.trace.length() => 1
    picked.trace.contains(Random.int(1, 6)) => true
```

Here `.result` is the function's return value under the stub, and `.trace` is the collected trace of classified emissions. Trace projections are runtime checks. They are weaker than a universal theorem over all oracles.

## Effect classification

The compiler has no classification table. A classification is the `oracle = ...` attribute on the operation in its capability source, so a capability the program declares is classified the same way as the shipped ones. The shipped set is:

| Namespace | Method | Dimension |
|---|---|---|
| `Args` | `get` | snapshot |
| `Env` | `get` | snapshot |
| `Env` | `set` | generative + output |
| `Console` | `readLine` | generative |
| `Console` | `print`, `error`, `warn` | output |
| `Random` | `int`, `float` | generative |
| `Process` | `stopRequested` | generative |
| `Time` | `now`, `unixMs` | generative |
| `Time` | `sleep` | generative + output |
| `Disk` | `readText`, `readBytes`, `readBytesAt`, `size`, `exists`, `listDir` | generative |
| `Disk` | `writeText`, `appendText`, `writeBytes`, `appendBytes`, `delete`, `deleteDir`, `makeDir`, `sync` | generative + output |
| `Http` | `get`, `head`, `delete`, `post`, `put`, `patch` | generative + output |
| `Tcp` | `send`, `sendBytes`, `ping` | generative + output |
| `Tcp` | `connect`, `beginConnect`, `dialled`, `listen`, `accept`, `peerAddress`, `poll` | generative + output |
| `Tcp` | `readLine`, `readBytes`, `readSome`, `readNow`, `writeLine`, `writeBytes`, `writeNow` | generative + output |
| `Tcp` | `close`, `closeDial`, `closeListener` | generative + output |
| `Wait` | `poll` | generative + output |
| `Work` | `cancel` | generative + output |
| `Terminal` | `readKey` | generative |
| `Terminal` | `size` | snapshot |
| `Terminal` | `clear`, `moveTo`, `print`, `hideCursor`, `showCursor`, `flush` | generative + output |
| `Terminal` | `enableRawMode`, `disableRawMode`, `setColor`, `resetColor` | generative + output |

Every non-`output` operation above can be stubbed through `given`. An operation with no `oracle` attribute is not modeled by Oracle and belongs in record/replay.

## Effect stubs are stateless

> Prove the model, not the world.

Oracle stubs carry no state. A `Disk.writeText("a.txt", "hi")` in the trace does **not** make a later `Disk.readText("a.txt")` return `"hi"`. An `Env.set("KEY", "v")` does **not** make a later `Env.get("KEY")` return `"v"`. A first `Time.now()` call does **not** force a second one to return a larger value. A `Tcp.writeLine(c, "x")` does **not** affect what `Tcp.readLine(c)` returns next.

This is deliberate. Real wall clocks are not monotonic (NTP, leap seconds, suspend/resume, VM clock skew). Filesystems are not transactional. TCP connections drop, return partial reads, and misreport delivery. **Aver does not give external services nicer laws than the platform actually promises.** A Ledger-style "stateful capability model" would let you prove read-after-write consistency on `Disk.*`, and the proof would then claim a guarantee the OS never gave.

The fix is a functional core with an imperative shell:

- **State you own** (a `FileStore`, a `PaymentLedger`, a `WorkflowState`) lives in pure user code as ordinary data. Read-after-write consistency is a property of that data model, proven by `verify` over the pure functions.
- **The world you don't own** (`Disk.*`, `Time.*`, `Tcp.*`, `Http.*`) stays at the boundary. Oracle stubs return what the test says they return, whatever effect calls came before.

Two runnable examples show the pattern:

- `examples/formal/file_store_pure_core.av` + `examples/formal/file_store_shell.av`: a pure `FileStore` data model with read-after-write laws proven over pure code, and `Disk.writeText` only at the boundary as a stateless oracle.
- `examples/formal/clock_as_data.av`: time-dependent logic that takes `nowMs` as a parameter, with `Time.unixMs` only at the boundary.

A `verify` law that assumes ordering, accumulation, or memory across effect calls belongs in the pure core, outside Oracle.

Boundary notes:

- `Console.readLine` and `Terminal.readKey` are modeled as generative input. The proof gets a deterministic oracle value for each call.
- Mutating `Disk.*` calls are modeled as operation/result effects. The requested operation goes into the trace, and success or failure comes from the oracle. Oracle asserts nothing about persistent filesystem state after the operation.
- Tcp sessions use separate opaque `Tcp.Connection`, `Tcp.Dial`, and `Tcp.Listener` tokens. They are wrapped in the represented `Tcp.Socket` sum only when one readiness map has to carry every state. Stubs are stateless: a `writeLine` does not affect what a later `readLine` returns, and Oracle does not invent state for hidden kernel readiness. If the test wants request/response symmetry, write it into the stub explicitly.
- Terminal drawing and modal calls are output trace events. Mode (raw / cooked) and color state are not modeled, so assert the sequence of trace events instead.

## Stub signatures

The effect dimension determines the stub signature:

### Snapshot

Snapshot stubs keep the runtime signature as is.

```aver
fn stubArgs() -> List<String>
fn stubEnv(key: String) -> Option<String>
```

### Generative and generative + output

Generative stubs take a leading `BranchPath` and the call index of their own operation.

```aver
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>

fn fakeFetch(path: BranchPath, n: Int, url: String)
    -> Result<Http.Response, String>
```

The original effect arguments follow `(path, n)`.

`n` counts the calls of that one operation on that one branch path, starting at 0. It ignores whatever the function under test does between two of them, so `fairDie` sees 0 then 1 whether or not the code reads the clock, logs a line, or writes to a socket in between. This is why a stub can be scripted by call number: `match n` is a reply script for that operation, and nothing else changes it. Two different operations share no numbering at all, and each `!` / `?!` branch restarts every operation at 0 under its own path. "Where a law is declined instead of exported" below lists the shapes where an exported proof cannot number calls the way a run does, and which it declines instead of approximating.

### Output

Output effects take no stubs. Assert them through `.trace`:

```aver
verify hello trace
    hello().trace.contains(Console.print("rolled")) => true
```

`given out: Console.print = [...]` is rejected, because an output effect has no return value to replace.

### Where a law is declined instead of exported

An exported proof never numbers a stub call differently from a run. Where it cannot follow the run, it declines the law. It names the call, the operation and the reason in the report and in the emitted file, and counts the decline, so nothing is proved about that function. `aver verify` is unaffected: the run keeps its own numbering and the law still runs under its stubs.

A warning would not be enough here, so the law is declined. `aver verify` checks a law on samples, while the exported theorem covers every input. A law whose samples happen to agree with a differently numbered model would certify a statement about a function the run does not compute. Every step would pass and the conclusion would be false. Five shapes are declined:

- A call into an effectful function. The callee's lifted body starts every operation at index 0, while the run keeps counting across the call. If a function reads the peer once and then calls a helper that reads it again, the helper's read gets index 1 at run time and index 0 in the export.
- A recursive call. This is the same shape seen from inside. A loop that reads the peer once per turn gives its read index 0, then 1, then 2 at run time, and index 0 in every turn of the export.
- A second operation inside a polled loop. A function declaring `Process.stopRequested` carries one index through its recursion, and that index counts polls, so every other operation in the function would be numbered at the polling rate. Two clock reads per poll diverge on the second turn. A poll loop that reaches no other operation is exact and still exports, because the base passed into the recursive call is that one operation's own count.
- A call after a `match` whose arms call the operation a different number of times. The run charges the arm it took, and no single literal fits every arm. Arms that call an operation equally often (the usual case) are exact and still export.
- A claim that reaches one operation through more than one effectful call. The claim is not a function body, so every call in it is exported at index 0, while a run numbers the operation across the guard and both sides of one case. `readOne() => readOneToo()` gives the peer index 0 on the left and index 1 on the right. Two calls that share no operation are numbered from zero on both sides and still export. The `because` lines are not counted, because a run never evaluates them.

Calls inside a `!` or `?!` branch are exact wherever the rest of the body is. A branch is its own numbering scope on both sides. The run gives it a fresh slot for every operation each time it is entered, and the export numbers it from zero to match, whatever the surrounding body has already charged.

The stubs a law supplies do not lift the decline, on purpose. A law over a `given` bound to a function parameter is asserted for every function of that shape, including ones that read the index, so demonstrating it under a stub that ignores the index makes the theorem no safer.

Sampled `verify` cases are not declined. A case is one concrete evaluation that `aver verify` has already run. Either the exported model computes the same value, and the theorem holds of the run too, or it computes a different one, and the proof fails where a reader sees it. Neither outcome states something false about the run.

To certify a law over an effectful function, keep helper boundaries and recursion out of the function the law is about, and let the claim reach each operation through one call. Script the stub by call number within that one body.

## Driving a socket state machine with a scripted peer

Only a provider can mint a resource such as `Tcp.Connection`, so a case cannot build one and pass it in. Mint it inside the function under test and drive the exchange over it. A minting operation's stub gets the witness in an extra slot after `(path, n)`, and a successful dial only has to return it:

```aver
fn greet(host: String, port: Int) -> Result<String, String>
    ? "Mint the connection inside the function under test, then drive the exchange over it."
    ! [Tcp.connect, Tcp.writeLine, Tcp.readLine, Tcp.close]
    connection = Tcp.connect(host, port)?
    Tcp.writeLine(connection, "HELLO")?
    version = Tcp.readLine(connection)?
    Tcp.writeLine(connection, "READY")?
    banner = Tcp.readLine(connection)?
    Tcp.close(connection)?
    Result.Ok("{version}/{banner}")

fn peerAnswers(path: BranchPath, call: Int, fresh: Tcp.Connection, host: String, port: Int) -> Result<Tcp.Connection, String>
    ? "The dial succeeds and hands back the witness the provider would have minted."
    Result.Ok(fresh)

fn peerSpeaks(path: BranchPath, call: Int, connection: Tcp.Connection) -> Result<String, String>
    ? "The reply script: the peer announces its version, then its banner."
    match call
        0 -> Result.Ok("V2")
        _ -> Result.Ok("OK")

fn peerListens(path: BranchPath, call: Int, connection: Tcp.Connection, line: String) -> Result<Unit, String>
    ? "The peer accepts every line written to it."
    Result.Ok(Unit)

fn peerHangsUp(path: BranchPath, call: Int, connection: Tcp.Connection) -> Result<Unit, String>
    ? "Closing the connection succeeds."
    Result.Ok(Unit)

verify greet
    given dial: Tcp.connect = [peerAnswers]
    given reads: Tcp.readLine = [peerSpeaks]
    given writes: Tcp.writeLine = [peerListens]
    given closes: Tcp.close = [peerHangsUp]
    greet("example.test", 79) => Result.Ok("V2/OK")
```

`peerSpeaks` is a reply script for `Tcp.readLine` only: read 0 is the version, read 1 is the banner. The dial, the two writes and the close are not part of its numbering, so adding a log line or a clock read to `greet` does not change the script. Three rules limit what this pattern can express:

- Every operation `greet` reaches needs its own `given`, or the case aborts before host dispatch.
- Resource identity is unobservable, so a stub cannot tell which connection it is asked about. Script by call order, not by peer.
- Stubs are stateless. A `writeLine` does not change what the next `readLine` returns. If the exchange is a request/response pair, write the pairing into the script by hand, as `peerSpeaks` does.

## Multiple stubs

A `given` list is a concrete domain:

```aver
verify pickOne trace
    given rnd: Random.int = [lowDie, highDie]
    Result.Ok(pickOne().result) => rnd(BranchPath.Root, 0, 1, 6)
```

This expands to two cases. Several `given` lists expand as a cartesian product, capped at `10_000` cases (`[verify] max-cases` in `aver.toml` moves the cap for the project, and `max-cases` in a `[[verify.costly]]` entry moves it for one function). Stub names may be local (`lowDie`) or qualified imports (`Helpers.lowDie`).

## Trace API

Trace projections exist only inside `verify <fn> trace`.

```aver
fn().trace                    -- Trace
fn().trace.length()           -- Int
fn().trace.event(k)           -- Option<EffectEvent>
fn().trace.contains(eventLit) -- Bool
fn().trace.count(method)      -- Int  -- 0.13 Limit
```

`.trace.count(M)` returns the number of trace events whose method matches `M` (an effect-method reference like `Random.int` or a call literal like `Console.print("rolled")`). `.contains` answers yes or no. `.count` gives the number, so a law can pin "this fn calls the API exactly once" or "no extra Disk reads under hostile profiles".

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
EffectEvent(method: String, args: List<EffectArg>, path: String)
```

`path` is the structural branch position:

- `""` means sequential/root
- `"0"` means branch 0 of a group
- `"0.1"` means branch 1 of a group nested inside branch 0

`BranchPath.parse(ev.path)` validates the string and returns `Result<BranchPath, String>`. Use `?` in a Result-returning function, or match the error. A valid string literal such as `BranchPath.parse("0.1")` is checked at compile time and is typed directly as the opaque `BranchPath` that generative stubs and specs use. In the same way, `BranchPath.child(parent, index)` can fail for a dynamic index, while a syntactic non-negative integer literal discharges directly to `BranchPath`.

There are two ways to compare:

- `.trace.contains(Console.print("x"))` checks whether that event happened anywhere and ignores `path`.
- `.trace.event(0) => Option.Some(EffectEvent(...))` is strict structural equality and includes `path`.

Common assertions stay short, and exact event checks are there when you need them.

## Helper boundary

`verify <fn> trace` records only what the verified function emits directly. Emissions from helpers it calls are suppressed and do not reach stdout during `aver verify`.

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

When a helper's own emissions matter, verify its trace separately.

## Proof export

`aver proof` lifts classified effectful functions to pure proof functions by adding explicit oracle/capability parameters. Generated Lean and Dafny files start with a trust-assumption header for the runtime/compiler trace invariant.

Supported law shapes can become universal theorems. Concrete `given` domains still produce executable/sample checks. Unsupported proof shapes should either fail clearly or stay as checked-domain/sample obligations, depending on backend and verify mode.

### `aver verify` vs `aver proof` — the same `verify` block, two different questions

A `verify <fn> law` block serves two commands:

- `aver verify` runs it as a **finite sample check**. It enumerates the cartesian product of the `given` domains (capped at 10,000 cases, or whatever `max-cases` the project set for this function) and evaluates each case against the law's RHS with the stubs you supplied.
- `aver proof` exports the same block as a **universally quantified theorem** in Lean / Dafny. Every classified effect becomes a function parameter, and the law is asserted *for every possible such function*, including ones outside the stubs in `given`.

The two can give different answers on the same block. The standard example is `examples/formal/randomness_paradox.av`:

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

`aver verify` passes. Under `distinctStub` the two calls return `1.0` and `2.0`, and the law's RHS holds.

`aver proof` exports a theorem of the form `∀ rnd, twoFloatsDistinct rnd = true`, and both backends reject it for the same reason. Some oracles (e.g. `fun _ _ => 0.5`) return the same value for both calls, and for them the law is false.

- `--backend lean` + `lake build` → `unsolved goals: (rnd BranchPath.Root 0 != rnd BranchPath.Root 1) = true`
- `--backend dafny` + `dafny verify` → `a postcondition could not be proved on this return path: ensures twoFloatsDistinct(BranchPath_Root, rnd) == true`

This is intended. `verify` asks "does this hold for the stubs I wrote down?". `proof` asks "does this hold for every classified-effect implementation with the right signature?". The second is strictly stronger and catches what the first cannot.

When `verify` passes but `aver proof` rejects, the law is **stub-specific**: true under the chosen stubs and false in general. Either rewrite the law so it does not depend on hidden stub structure (e.g. assert against `rnd(...)` directly instead of a constant), or keep it as a sample-only check and don't export it. For "given this concrete stub, here's what I expect", use the cases form `verify <fn> trace`. It does not export and does not claim to.

## Hostile mode (`aver verify --hostile`)

> _In laws, examples are not limits. Preconditions are._

### Three roles, one frame

Read these three together. The rest of this section follows from them.

- **`given` is your chosen world.** The stub or value list you wrote is the world the law was demonstrated in, and `aver verify` runs the law there.
- **Hostile asks "what if your world was wrong?"** Under `--hostile`, Aver swaps adversarial profiles in for your `given` (frozen clocks, empty disks, network down, rolls stuck at the bound) and requires the same law to hold there too.
- **`when` filters the worlds a law assumes.** `when clock(root, 1) > clock(root, 0)` declares "this law assumes a monotonic clock". Hostile profiles that break the assumption are skipped, so the law only meets worlds it promised to hold for.

A `verify ... law` block makes a universal claim: "this holds for every value of the `given` clauses' types". The declared set (`given n: Int = [1, 5, 100]`) is the *exploration domain*, the values you expect to exercise the law. The claim itself covers the whole type, and `--hostile` checks that.

`--hostile` works on **three axes**, all tied to law form:

1. **Value-side**, on `verify <fn> law <name>` (with or without `trace`). Typed `given` clauses get the boundary set for their type added. Law form is a universal claim, and hostile checks the boundary the user did not exercise.
2. **Effect-side**, also on `verify <fn> law <name>` (with or without `trace`). Classified non-`Output` effects the fn declares are multiplied by a cartesian product of adversarial profiles. The user's `given <Effect>` stub is one chosen world. Hostile asks "what if you chose wrong?" by replacing the stub with each profile in turn.
3. **Order-side**, for laws whose fn contains an `(a, b)!` independent-product. Each case gets a twin in which the branches run right-to-left while results still land in their source positions. A pure law's tuple does not depend on order, so a difference shows that "independent" does not hold for the active stub map. Failures show `+reverse-eval` in the case's origin. Fns without `!` are skipped, because their twin would be a pure copy and tell you nothing.

| Form | Value-side | Effect-side | Order-side |
|---|---|---|---|
| `verify <fn>` (plain) | — | — | — |
| `verify <fn> trace` (cases-form trace) | — | — | — |
| `verify <fn> law <name>` | ✓ | ✓ | ✓ if `!` |
| `verify <fn> trace law <name>` | ✓ | ✓ | ✓ if `!` |

Cases form opts out. Both plain `verify <fn>` and `verify <fn> trace` (no `law`) are fixtures: explicit scenarios with chosen stubs. Multiplying a fixture by the adversarial product would turn "this scenario" into "every scenario", which the user did not write.

**Value-side boundary sets:**

- `Int`    → `0`, `1`, `-1`, `i64::MIN`, `i64::MAX`
- `Float`  → `0.0`, `1.0`, `-1.0`, `MIN`, `MAX`, `+/-Inf`, `NaN`
- `Bool`   → both
- `String` → `""`, `"a"`, 1024×`x`, `"\0"` (NUL embedded), multi-byte UTF-8
- `Unit`   → `Unit`

These are added to the declared list. Duplicates are dropped, so a value the user already wrote does not run twice.

**Effect-side adversarial profiles** (per classified non-`Output` effect):

| Effect | Profiles |
|---|---|
| `Time.now` / `Time.unixMs` | `normal` (advancing 1s/call), `frozen` / `frozen_zero`, `epoch` / `saturated`, `backward` (NTP correction), `fast_forward` (leap second / skew) |
| `Random.int` | `midrange`, `always_min`, `always_max`, `alternating` (per-call min↔max) |
| `Random.float` | `midrange` (0.5), `always_zero`, `always_one` |
| `Args.get` | `normal`, `empty`, `many` (edge values like `\0`, `--flag`) |
| `Env.get` | `normal`, `missing`, `empty` |
| `Terminal.size` | `normal` (80×24), `minimal` (1×1) |
| `Disk.readText` / `exists` / `listDir` | `normal` + `always_err` + format-specific (`empty_ok`, `never`, `always`) |
| `Disk.readBytes` | `normal`, `always_err`, `empty_ok` |
| `Disk.readBytesAt` | `normal`, `always_err`, `short_ok` (EOF before the requested upper bound) |
| `Disk.size` | `normal`, `zero`, `always_err` |
| `Console.readLine` | `normal`, `eof`, `empty` |
| `Terminal.readKey` | `normal`, `no_input` |
| `Http.{get,head,delete,post,put,patch}` | `normal_ok`, `always_err` |
| `Disk.{writeText,appendText,writeBytes,appendBytes,delete,deleteDir,makeDir,sync}` | `normal_ok`, `always_err` |
| `Tcp.{send,sendBytes,ping,readLine,writeLine,close,closeDial,closeListener,peerAddress}` | `normal_ok`, `always_err` |
| `Tcp.readBytes` | `normal_ok`, `short_read`, `always_err` |
| `Tcp.readNow` | `normal_ok`, `would_block` (`None`), `eof` (`Some(empty)`), `always_err` |
| `Tcp.writeBytes` | `normal_ok`, `always_err` |
| `Tcp.writeNow` | `normal_ok` (whole payload), `partial`, `would_block` (0), `always_err` |
| `Tcp.connect` | `normal_ok` (fresh connection resource), `always_err` |
| `Tcp.beginConnect` / `Tcp.listen` | `normal_ok` (fresh resource), `always_err` |
| `Tcp.dialled` | `connected`, `still_pending`, `refused` |
| `Tcp.accept` | `nothing_pending`, `once_then_nothing`, `always_err` |
| `Tcp.poll` | `none_ready`, `everything_ready`, `always_err` |

User-given pins do **not** exempt an effect. Hostile profiles are always layered on top, since the user's stub is itself an assumption. For the length of a hostile-profile case, the runtime stub installer replaces the user's stub, so the same law is evaluated in both worlds.

The full product (value-boundary cases × adversarial worlds) is capped at `10_000` cases per block. This is the same `max-cases` ceiling as parser-side declared expansion, resolved per block through the same `[[verify.costly]]` entries. A block over budget fails with a clear error that points at the law and gives the projected size. Narrow the `given` domain, add a `when` precondition, raise the ceiling in `aver.toml`, or run that block without `--hostile`.

`when` clauses still bind: a hostile case is dropped if the `when` guard returns `false`. That is the line `--hostile` respects. `given` ranges are hints for exploration, and `when` is the boundary of the law.

### Output

When a case injected by hostile mode fails, the diagnostic uses its own slug, so CI gates can route declared and adversarial failures separately:

- `verify-mismatch`: failure in the declared world, a real bug
- `verify-hostile-mismatch`: failure in an adversarial world, a missing precondition or unpinned effect

```
fail[verify-hostile-mismatch]: law violated under --hostile expansion
  at: prog.av:9:1
  block: isPositive spec alwaysPositive
  case: isPositive(0) == true
  expected: true
  actual: false
  given: n = 0
  law: isPositive(n) == true
  origin: hostile boundary expansion
  repair: this case isn't in the declared `given` — the claim isn't
          universal. Either add `when <precondition>` to scope it, or
          drop `law` form and use `verify <fn>` (cases form, example
          semantics) with the values you actually meant.
```

For effect-side hostile (a `verify <fn> trace` block where an adversarial profile replaces the user's oracle stub for that case), `origin` carries the profile label. Trace form does not support `when` yet (that keyword belongs to the value domain of `law` form), so the repair text covers the two options that exist today.

> **`when` as oracle assumption (0.13).** `verify <fn> trace law <name>` supports `when` predicates that refer to the effect-given oracle: `when clock(root, 1) > clock(root, 0)` for monotonicity, `when read(root, 1, "f") == Result.Ok("hello")` for read-your-writes. Under `--hostile`, profiles that break the assumption are *skipped*, and the law is checked only under oracle behaviors that satisfy it. The guard sees the same oracle as the case body: in the declared case the user's stub runs, and in each hostile-profile case the matching profile fn runs. There is no state model and there are no session types. The guard is a predicate over oracle outputs at that operation's own call indices, so `clock(root, 0)` and `clock(root, 1)` are the first two clock reads, whatever else runs between them.
>
> **`when` itself must be pure.** A call like `clock(root, 1)` inside a guard is a *query on the oracle* installed for this case. It is not a runtime effect call, and Aver does not look at the wall clock. The guard asks the same fn that supplies values to the law body.
>
> **The word "invariant" covers two different things. Keep them apart.** A user-written `when` is an *oracle assumption*: it belongs to one law, it is local, and the user states it explicitly. The axiom block that `aver proof` emits into Lean / Dafny carries *runtime invariants*: they are global and Aver guarantees them. `Random.int` respects its bounds, `Random.float ∈ [0,1]`, `Time.unixMs ≥ 0`, and `Process.stopRequested` is monotonic across calls (`i ≤ j ∧ stop(path, i) = true` implies `stop(path, j) = true`). The Process law is the first invariant that relates two oracle observations instead of constraining one result. Both kinds feed the proof side at different scopes: `when` covers one law, and axioms hold across the whole project.

```
  origin: effect profile: Time.unixMs/saturated
  repair: the law passes for the world your `given` stub describes
          but breaks under this adversarial profile. Three options:
          (a) adjust the impl to be robust against the profile (it
          models a real production world — frozen clock, empty disk,
          network down); (b) declare the oracle assumption with
          `when` (e.g. `when clock(root, 1) > clock(root, 0)` for
          monotonicity) so hostile skips profiles that violate it;
          (c) if the claim really only holds for the one stub you
          wrote, drop `law` form and use `verify <fn>` cases-form
          (example semantics) with that stub.
```

The block summary line splits the count by origin:

```
✗ isPositive spec alwaysPositive      4/7 passed (3/3 declared, 1/4 hostile)
```

`3/3 declared` means every value the user wrote passes. `1/4 hostile` means boundary expansion found 3 failures. This is the usual sign that a law is not universal.

JSON (`--json`) carries the same data in structured form:

- each diagnostic has `slug` `verify-hostile-mismatch` (or `verify-mismatch` for declared failures), a `from_hostile: true` flag, and `fields[origin] = "hostile boundary expansion"` or `"hostile effect profile: Time.unixMs/saturated"`
- `verify_summary.blocks[].declared_passed / declared_failed / hostile_passed / hostile_failed` let tooling tell a "law regression" (`declared_failed > 0`) from a "hostile coverage gap" (`hostile_failed > 0 && declared_failed == 0`).

#### jq one-liners

```sh
# Adversarial-only failures
aver audit --hostile --json prog.av | head -1 \
  | jq '.diagnostics[] | select(.slug == "verify-hostile-mismatch")'

# Group by adversarial profile
aver audit --hostile --json prog.av | head -1 \
  | jq '[.diagnostics[] | select(.slug == "verify-hostile-mismatch") |
         .fields[] | select(.[0] == "origin") | .[1]]
        | group_by(.) | map({profile: .[0], count: length})'
```

### Two responses to a hostile failure

1. **It IS a precondition you forgot.** Write it down: `when n > 0` states which `n` the law applies to. The hostile case `n = 0` is filtered out, the law passes again, and the precondition is visible to anyone reading the spec. Proof export can pick it up too.

2. **The values you wrote were *examples*, and you never meant a universal claim.** Drop the `law <name>` form and use plain `verify <fn>` (cases form):

   ```aver
   verify isPositive
       isPositive(1) => true
       isPositive(5) => true
       isPositive(100) => true
   ```

   The checks are the same, but the spec now says "these specific cases" instead of "for all `n`". `--hostile` does not add to `verify <fn>` cases. They are narrow on purpose.

Do not quietly widen the declared list to make the hostile failure go away. That hides a real assumption behind more examples. `when` makes the assumption explicit, and cases-form `verify` makes the spec narrower. Pick one.

### What `--hostile` does not do

- It does not invent values for user-defined types (`Type::Named`). If a given ranges over `MyShape`, `--hostile` leaves the declared list alone, since no boundary set could respect user constructors.
- It does not synthesise `List<T>` / `Option<T>` / `Result<T, E>` values. Their boundary set is empty, and declared values pass through unchanged.

## Current limits

Oracle does not try to model every side effect.

Not supported:

- Whole server loops. `HttpServer` is ordinary Aver over persistent `Tcp` resources. Verify its pure `HttpWire` and handler pieces separately.
- Stateful capability models, or hidden filesystem or clock state. Effect stubs are stateless by design (see the section above). If a property depends on memory across effect calls, model the state in pure user code.
- Proof export for `?!` cancel mode. Oracle proof export expects complete independence mode, so that every branch has a stable trace position.
- Higher-order effectful callbacks. Oracle works best when the effect surface is visible in the verified function's signature.
- Trace-aware laws on recursive effectful functions. Use `verify <fn> law ...` without `trace`, or move the step that emits effects into a non-recursive function and verify its trace.
- Machine-checked proof of the compiler/runtime trace invariant. Generated proof files state the assumptions explicitly.
