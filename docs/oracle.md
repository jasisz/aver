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
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>
    ? "Deterministic Random.int stub."
    Result.Ok(4)

fn pickOne() -> Int
    ? "Rolls once."
    ! [Random.int]
    Random.int(1, 6)

verify pickOne law usesOracle
    given rnd: Random.int = [fairDie]
    pickOne() => Result.withDefault(rnd(BranchPath.Root, 0, 1, 6), 1)
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

## Plain cases and capability stubs

Cases-form `verify` can bind a provider operation without turning the case into a law or enabling trace projections:

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

The `hash` alias does not have to appear in the assertion. For each expanded case, the selected function is installed in the VM's operation-stub map before the left side runs. A pure capability stub has exactly the declared operation signature; an effectful generative capability keeps the Oracle signature `(BranchPath, Int, args...) -> result`. Multiple functions in the domain produce separate cases.

The operation after `given name:` is always its full canonical path. If the capability is loaded as `Domain.Crypto.Hash160`, write `Domain.Crypto.Hash160.digest`, exactly as at the call site and in diagnostics. A shorter `Hash160.digest` does not fall through to runtime: checking rejects it and, when the suffix identifies one loaded operation, suggests the canonical path.

This binding is verify-local. It does not install a package, exercise the Rust provider implementation, or satisfy provider preflight for `aver run` and compiled artifacts. Provider cryptography still belongs in the provider's own tests; the Aver case checks the contract shape and the caller's behavior under an explicit result.

A plain cases block may call a function whose signature declares effects when
that concrete case never reaches an effectful operation. Aver decides this at
execution time, not from the function-wide `! [...]` list. If the case does
reach an effect without an exact `given`, verification stops before host
dispatch. Add `trace` (and a `given` for an effect that returns a generated
value), or move a stateful/interactive flow to record/replay. Plain verify is
never a one-shot real-world smoke test.

Proof export preserves that distinction. A passing case with no `given` is
stated against the Oracle-lifted function and quantified over the missing
oracle, proving that the selected branch has the expected result for every
provider implementation. A case with a `given` is stated against its selected
stub instead. Capability paths keep their full module qualification throughout
lifting, so an operation such as `Infra.Kv.get` becomes the corresponding
oracle argument rather than a host call in the Lean artifact.

## Trace-aware cases

Use cases-form `verify <fn> trace` when a case intentionally reaches classified
effects or when you want runtime assertions over the collected trace:

```aver
verify pickOne trace
    given rnd: Random.int = [fairDie]
    picked = pickOne()
    picked.result => Result.withDefault(rnd(BranchPath.Root, 0, 1, 6), 1)
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
| `Env` | `set` | output |
| `Console` | `readLine` | generative |
| `Random` | `int`, `float` | generative |
| `Time` | `now`, `unixMs` | generative |
| `Time` | `sleep` | output |
| `Disk` | `readText`, `exists`, `listDir` | generative |
| `Disk` | `writeText`, `appendText`, `delete`, `deleteDir`, `makeDir` | generative + output |
| `Http` | `get`, `head`, `delete`, `post`, `put`, `patch` | generative + output |
| `Tcp` | `send`, `sendBytes`, `ping` | generative + output |
| `Tcp` | `connect`, `readLine`, `readBytes`, `writeLine`, `writeBytes`, `close` | generative + output |
| `Console` | `print`, `error`, `warn` | output |
| `Terminal` | `readKey` | generative |
| `Terminal` | `size` | snapshot |
| `Terminal` | `clear`, `moveTo`, `print`, `hideCursor`, `showCursor`, `flush` | output |
| `Terminal` | `enableRawMode`, `disableRawMode`, `setColor`, `resetColor` | output |

Anything outside this set is not modeled by Oracle.

## Effect stubs are stateless

> Prove the model, not the world.

Oracle stubs do not imply state. A `Disk.writeText("a.txt", "hi")` in the trace does **not** make a later `Disk.readText("a.txt")` return `"hi"`. An `Env.set("KEY", "v")` does **not** make a later `Env.get("KEY")` return `"v"`. A first `Time.now()` call does **not** constrain the value of a second one to be greater. A `Tcp.writeLine(c, "x")` does **not** affect what `Tcp.readLine(c)` returns next.

This is by design. Wall clocks are not monotonic in the real world (NTP, leap seconds, suspend/resume, VM clock skew). Filesystems are not transactional. TCP connections drop, return partial reads, lie about delivery. **Aver does not pretend external services have nicer laws than the platform actually promises.** A Ledger-style "stateful capability model" would let you prove read-after-write consistency on `Disk.*` — and then your proof claims a guarantee the OS never gave.

The cure is functional core / imperative shell:

- **State you own** (a `FileStore`, a `PaymentLedger`, a `WorkflowState`) lives in pure user code as ordinary data. Read-after-write consistency is a property of that data model, proven by `verify` over the pure functions.
- **The world you don't own** (`Disk.*`, `Time.*`, `Tcp.*`, `Http.*`) stays at the boundary. Oracle stubs return what the test says they return, independent of preceding effect calls.

Two runnable examples ship the pattern:

- `examples/formal/file_store_pure_core.av` + `examples/formal/file_store_shell.av` — `FileStore` pure data model with read-after-write laws proven over pure code; `Disk.writeText` only at the boundary as a stateless oracle.
- `examples/formal/clock_as_data.av` — time-dependent logic with `nowMs` passed as a parameter; `Time.unixMs` only at the boundary.

If a `verify` law assumes ordering, accumulation, or memory across effect calls, it does not belong in Oracle — it belongs in the pure core.

Boundary notes:

- `Console.readLine` and `Terminal.readKey` are modeled as generative input:
  the proof receives a deterministic oracle value for each call.
- Mutating `Disk.*` calls are modeled as operation/result effects: the requested
  operation is emitted to the trace, and success/failure comes from the oracle.
  Oracle does not assert persistent filesystem state after the operation.
- `Tcp.connect` / `readLine` / `writeLine` / `close` are session methods using an opaque
  `Tcp.Connection` token. Stubs are stateless: a `writeLine` does not affect what a
  later `readLine` returns. If the test wants request/response symmetry, encode it
  explicitly in the stub.
- Terminal drawing and modal calls are output trace events. Mode (raw / cooked) and
  color state are not modeled — assert the sequence of trace events instead.

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
fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>

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
    pickOne().result => Result.withDefault(rnd(BranchPath.Root, 0, 1, 6), 1)
```

This expands to two cases. Multiple `given` lists expand as a cartesian product, capped at `10_000` cases (`[verify] max-cases` in `aver.toml` moves the cap for the project, and `max-cases` in a `[[verify.costly]]` entry moves it for one function). Stub names may be local (`lowDie`) or qualified imports (`Helpers.lowDie`).

## Trace API

Trace projections are only available inside `verify <fn> trace`.

```aver
fn().trace                    -- Trace
fn().trace.length()           -- Int
fn().trace.event(k)           -- Option<EffectEvent>
fn().trace.contains(eventLit) -- Bool
fn().trace.count(method)      -- Int  -- 0.13 Limit
```

`.trace.count(M)` returns the number of trace events whose method matches `M` (an effect-method reference like `Random.int` or a call literal like `Console.print("rolled")`). It complements `.contains` (boolean any-match) with a quantitative form so laws can pin "this fn calls the API exactly once" or "no extra Disk reads under hostile profiles".

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

`BranchPath.parse(ev.path)` validates the string and returns `Result<BranchPath, String>`; use `?` in a Result-returning function or match the error. A valid string literal such as `BranchPath.parse("0.1")` is checked at compile time and types directly as the opaque `BranchPath` used by generative stubs and specs. Likewise, `BranchPath.child(parent, index)` is catchable for a dynamic index, while a syntactic non-negative integer literal discharges directly to `BranchPath`.

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

- `aver verify` runs it as a **finite sample check**: the cartesian product of the `given` domains is enumerated (capped at 10,000 cases, or whatever `max-cases` the project set for this function) and evaluated against the law's RHS using whatever stubs you supplied.
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

## Hostile mode (`aver verify --hostile`)

> _In laws, examples are not limits. Preconditions are._

### Three roles, one frame

Read these together — every other detail in this section follows from them:

- **`given` is your chosen world.** The stub or value list you wrote is
  the world the law was demonstrated in. `aver verify` runs the law
  there.
- **Hostile is "what if your world was wrong?"** Under `--hostile`,
  Aver substitutes adversarial profiles in for your `given` — frozen
  clocks, empty disks, network down, rolls stuck at the bound — and
  asks the same law to hold there too.
- **`when` is the filter that says which worlds this law assumes.**
  `when clock(root, 1) > clock(root, 0)` declares "this law assumes a
  monotonic clock". Hostile profiles that violate the assumption are
  skipped; the law is exercised only against worlds it actually
  promised to hold for.

A `verify ... law` block is a universal claim: "this holds for every
value of the `given` clauses' types". The declared set
(`given n: Int = [1, 5, 100]`) is the *exploration domain* — values you
think will exercise the law — but the claim itself ranges over the
whole type. `--hostile` checks that.

`--hostile` works on **three axes**, all tied to law form:

1. **Value-side** — on `verify <fn> law <name>` (with or without
   `trace`). Typed `given` clauses get augmented with the per-type
   boundary set. Law form is a universal claim; hostile checks the
   boundary the user did not exercise.
2. **Effect-side** — also on `verify <fn> law <name>` (with or without
   `trace`). Classified non-`Output` effects the fn declares get
   multiplied by an adversarial profile cartesian. The user's `given
   <Effect>` stub is one chosen world; hostile asks "what if you chose
   wrong?" by overriding the stub with each profile in turn.
3. **Order-side** — for laws whose fn contains an `(a, b)!`
   independent-product. Each case gets a twin in which the branches
   execute right-to-left while results land in their source positions;
   a pure law's tuple is order-invariant, so a divergence proves
   "independent" doesn't actually hold for the active stub map.
   Failures show `+reverse-eval` in the case's origin. Skipped for fns
   without `!` because the twin would be a pure copy with no signal.

| Form | Value-side | Effect-side | Order-side |
|---|---|---|---|
| `verify <fn>` (plain) | — | — | — |
| `verify <fn> trace` (cases-form trace) | — | — | — |
| `verify <fn> law <name>` | ✓ | ✓ | ✓ if `!` |
| `verify <fn> trace law <name>` | ✓ | ✓ | ✓ if `!` |

Cases-form opts out — both plain `verify <fn>` and `verify <fn> trace`
(no `law`) are fixtures: explicit scenarios with chosen stubs.
Multiplying a fixture by the adversarial cartesian would turn "this
scenario" into "every scenario", which is not what the user wrote.

**Value-side boundary sets:**

- `Int`    → `0`, `1`, `-1`, `i64::MIN`, `i64::MAX`
- `Float`  → `0.0`, `1.0`, `-1.0`, `MIN`, `MAX`, `+/-Inf`, `NaN`
- `Bool`   → both
- `String` → `""`, `"a"`, 1024×`x`, `"\0"` (NUL embedded), multi-byte UTF-8
- `Unit`   → `Unit`

These augment the declared list — duplicates are dropped, so a value the
user already wrote is not re-run.

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
| `Disk.{writeText,appendText,writeBytes,appendBytes,delete,deleteDir,makeDir}` | `normal_ok`, `always_err` |
| `Tcp.{send,sendBytes,ping,readLine,writeLine,close}` | `normal_ok`, `always_err` |
| `Tcp.readBytes` | `normal_ok`, `short_read`, `always_err` |
| `Tcp.writeBytes` | `normal_ok`, `always_err` |
| `Tcp.connect` | `normal_ok` (fresh connection resource), `always_err` |

User-given pins are **not** a pre-empt: hostile profiles always layer
on top, since the user's stub is itself an assumption. The runtime stub
installer overwrites user-given for the duration of a hostile-profile
case so the same law gets evaluated under both worlds.

The full cartesian (value-boundary cases × adversarial worlds) is
capped at `10_000` cases per block — the same `max-cases` ceiling as
parser-side declared expansion, resolved per block through the same
`[[verify.costly]]` entries. Over-budget blocks fail with
a clear error pointing at the law and listing the projected size;
tighten the `given` domain, add a `when` precondition, raise the
ceiling in `aver.toml`, or run that block without `--hostile`.

`when` clauses stay binding: a hostile case is dropped if the `when`
guard returns `false`. That is the line `--hostile` honours — `given`
ranges are exploration hints, `when` is the law boundary.

### Output

When a hostile-injected case fails, the diagnostic uses a distinct slug
so CI gates can route declared vs adversarial failures separately:

- `verify-mismatch` — declared world failure, real bug
- `verify-hostile-mismatch` — adversarial world failure, missing
  precondition or unpinned effect

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

For effect-side hostile (`verify <fn> trace` block, adversarial profile
overriding the user's oracle stub for that case), `origin` carries the
profile label. Trace form does not currently support `when` (that
keyword lives on `law` form's value domain), so the repair speaks to
the two real options today.

> **`when` as oracle assumption (0.13).** `verify <fn> trace law <name>`
> supports `when` predicates that reference the effect-given oracle —
> `when clock(root, 1) > clock(root, 0)` for monotonicity, `when
> read(root, 1, "f") == Result.Ok("hello")` for read-your-writes. Under
> `--hostile`, profiles that violate the assumption are *skipped*; the
> law is checked only under oracle behaviors that satisfy the declared
> assumption. The guard observes the same oracle the case body sees:
> for the declared case the user's stub fires, for each hostile-profile
> case the corresponding profile fn fires. No state model, no session
> types — just a predicate over oracle outputs at counter indices.
>
> **`when` itself must be pure.** Calls like `clock(root, 1)` inside a
> guard are *queries on the oracle* installed for this case, not runtime
> effect calls. Don't read this as Aver inspecting the wall clock — the
> guard is asking the same fn that supplies values to the law body.
>
> **Two different concepts share the word "invariant" — keep them
> apart.** A user-written `when` is an *oracle assumption* (per-law,
> local, the user states it explicitly). The axiom block emitted into
> Lean / Dafny by `aver proof` carries *runtime invariants* (global,
> guaranteed by Aver: `Random.int` respects bounds, `Random.float ∈
> [0,1]`, `Time.unixMs ≥ 0`). Both feed the proof side, but they sit
> at different scopes — `when` scopes one law, axioms hold across the
> whole project.

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

The block summary line breaks the count down by origin:

```
✗ isPositive spec alwaysPositive      4/7 passed (3/3 declared, 1/4 hostile)
```

— `3/3 declared` = all values the user wrote pass, `1/4 hostile` = boundary
expansion found 3 fails. The classic "law is not universal" signal.

JSON (`--json`) carries the same data structurally:

- per-diagnostic `slug` is `verify-hostile-mismatch` (or `verify-mismatch`
  for declared failures), `from_hostile: true` flag, and
  `fields[origin] = "hostile boundary expansion"` or
  `"hostile effect profile: Time.unixMs/saturated"`
- `verify_summary.blocks[].declared_passed / declared_failed /
  hostile_passed / hostile_failed` — tooling can split "law regression"
  (`declared_failed > 0`) from "hostile coverage gap" (`hostile_failed >
  0 && declared_failed == 0`).

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

1. **It IS a precondition you forgot.** Encode it: `when n > 0` makes the
   law explicit about what `n` it applies to. The hostile case `n = 0`
   gets filtered, the law passes again, and the precondition is now
   visible to anyone reading the spec — and to proof export, which can
   pick it up too.

2. **The values you wrote were *examples*, not a universal claim.** Drop
   the `law <name>` form and use plain `verify <fn>` (cases form):

   ```aver
   verify isPositive
       isPositive(1) => true
       isPositive(5) => true
       isPositive(100) => true
   ```

   Same checks, but now the spec says "these specific cases", not
   "for all `n`". `--hostile` does not augment `verify <fn>` cases — they
   are intentionally narrow.

The wrong response is to silently widen the declared list to suppress the
hostile failure: that just covers up a real assumption with more
examples. `when` makes the assumption explicit; `verify` cases-form makes
the spec narrower. Pick one.

### What `--hostile` does not do

- It does not invent values for user-defined types (`Type::Named`). If a
  given is over `MyShape`, `--hostile` leaves the declared list alone —
  there is no boundary set that respects user constructors.
- It does not synthesise `List<T>` / `Option<T>` / `Result<T, E>` values.
  The boundary set for those is empty; declared values pass through
  unchanged.

## Current limits

Oracle does not try to model every side effect.

Not supported:

- `HttpServer.listen` / `listenWith`. Server lifecycle and callbacks are
  long-running protocols, not one call/result effects.
- Stateful capability models / hidden filesystem-or-clock state. Effect stubs are
  stateless by design (see the section above) — if a property depends on memory
  across effect calls, model the state in pure user code.
- Proof export for `?!` cancel mode. Oracle proof export expects complete independence mode so every branch has a stable trace position.
- Higher-order effectful callbacks. Oracle works best when the effect surface is visible in the verified function's signature.
- Trace-aware laws on recursive effectful functions. Use `verify <fn> law ...` without `trace`, or move the effect-emitting step into a non-recursive function and verify that trace.
- Machine-checked proof of the compiler/runtime trace invariant. Generated proof files state the assumptions explicitly.
