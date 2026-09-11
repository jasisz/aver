# Aver Diagnostic Slugs

Every diagnostic Aver emits carries a stable `slug` that consumers (CLI, LSP, playground, agent frameworks) can key off. This page is the canonical reference, grouped by category.

Source of truth: `src/diagnostics/classify.rs` (classifier) and `src/checker/*.rs` / `src/main/format_cmd.rs` (emitters). If you're editing this table, also check that the slug string actually appears in one of those files.

## Severity meanings

- **`error`** — compilation / contract failure; blocks downstream steps (e.g. type error stops `aver verify`).
- **`warning`** — non-blocking code smell; agent / reviewer should see it but the program still runs.
- **`fail`** — runtime / verify / replay divergence; the program typechecks but misbehaves at the specified case.
- **`hint`** — IDE-only nudge; LSP surfaces, CLI usually ignores.

## Type & lexer

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `parse-error` | error | Lexer or parser failed on the source. | Fix the syntax error; canonical examples in `docs/language.md`. |
| `type-error` | error | Typechecker reports any error the classifier didn't narrow. | See the underlying message; contract inference fell through. |
| `type-mismatch` | error | Expression's inferred type differs from the declared context. | Change the expression to produce the expected type. |
| `unknown-ident` | error | A referenced name has no binding in scope. | Check spelling or add the missing import. |
| `arity-mismatch` | error | Function or constructor called with the wrong number of args. | Adjust the number of arguments. |
| `map-key-unordered` | error | A map's key type has no order every backend and the proof model can state — `Float`, or a `Map` or `Vector` reached through the key. | Key on a value that orders: `Int`, `String`, `Bool`, or a record, variant, list or tuple built out of them. |
| `effect-violation` | error | A function calls an effect it doesn't declare in `! [...]`. | Add the missing effect to the function's `! [...]`. |
| `int-div` | error | The `/` operator was used on two `Int`s. Integer division is partial (the divisor may be zero → `Result.Err`), so it is a function, not an operator. | Use `Int.div(a, b) : Result<Int, String>`; handle with `match` or `Result.withDefault`. With a nonzero literal divisor, `Int.div(a, k)` is total and returns plain `Int`. |
| `error-prop-non-result` | error | `?` was applied to an expression that is not a `Result`. | Drop the `?`. A smart-constructor call over an all-literal list inside the refinement's proven element interval (`Bytes.fromList([0, 10, 255])`) is total and already returns the refined type. |
| `pattern-subject-mismatch` | error | A `Result` / `Option` constructor pattern was matched against a subject of some other type — no value can ever take the arm. | Match the value's own shape; a discharged literal smart-constructor call returns the refined type, not a `Result`. |
| `yield-direct-call` | error | A function, a verify case, or another yielding function's argument calls a `yield` function directly; the function never runs as written. | Call its generated `__<fn>Start(...)` and answer the requests its `__<Fn>Outcome` carries, with `__<fn>Answer<Kind>` per request kind. |
| `yield-non-tail-call` | error | A yielding function calls another yielding function outside tail position (nested states are not lowered in this phase). | Pass what comes next as data, or make it a tail call. |
| `yield-unsupported` | error | A yielding function holds a construct the lowering does not cut: a tail call to a different yielding function, a request inside an independent product `(a, b)!`, a function value live across a request, or no stop at all. | Restructure as the message says: fold the callee in, perform the requests one after another, pass data instead of a callback, or drop `yield`. |

## Intent / verify hygiene

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `missing-verify` | error | A pure, non-trivial function lacks a `verify` block. | Add a `verify` block with representative cases. |
| `missing-description` | warning | A function lacks a `? "..."` description. | Add a `? "description"` line after the signature. |
| `verify-coverage` | warning | Verify block has too few cases for a reachable return/input shape. | Add a case for the missing shape; if it is genuinely uninhabited, use a function-scoped `[[check.suppress]]` with a reason. |
| `verify-law` | warning | `verify law` names a function it never actually calls. | Use the named function in the law body or rename the law. |
| `verify-rhs` | error | Case calls the target on the right-hand side of `=>`. | Right side must be the expected value, not another call to the target. |

`verify-coverage` makes two kinds of claim and discharges them differently. The **return-shape** claims are `Result.Ok` / `Result.Err`, `Option.Some` / `Option.None`, `true` / `false`, and the constructors a returned sum-producing function can actually return. The sum image follows local calls transitively, including mutually recursive return paths; if an external or otherwise opaque return path prevents a closed answer, the lint makes no constructor-count claim instead of using the whole declared sum as a guessed denominator. A case satisfies a shape by writing it on the right of `=>`, either directly or through a local helper whose outer shape is statically unique (`blank()` that always returns `Result.Err`, for example). The checker does not execute arbitrary calls while linting: a builtin/provider call such as `Bytes.fromHex("zz")`, or a helper that can return several shapes depending on its arguments, is not credited from syntax alone even if a later `aver verify` run observes one arm. The `Ok` claim has a second form: applying `?` to a call of the function under verification, anywhere on the left of `=>`. `readOne([7, 9])?.value => 7` pins `Ok` at least as firmly as `=> Result.Ok(...)` does, because an error would have propagated and failed the case, and it pins a field as well. Return-shape claims are also inherited: a function outside its module's `exposes` list, whose mutual-recursion group is reached by exactly one caller from outside that group, is not asked for return-shape cases of its own when that caller's examples already pin every arm — the caller's vectors are what drive it. A file with no `module` declaration exposes every name that does not start with `_`, so nothing there counts as private and nothing is inherited. The **input-shape** claims (empty and non-empty list arguments, both branches of a `Bool` argument, every constructor of an argument's sum type, a recursive function's base case, an empty string for a parser) are never inherited: a caller covering both of its own arms says nothing about the range of arguments a helper was exercised with. Structurally unique local helpers are credited for Bool, list, and enum input shapes too; arbitrary calls remain opaque.

When a real signature shape is deliberately uninhabited, keep the residue explicit and narrow: `[[check.suppress]]` accepts an optional `fn = "eachInBranch"` beside `slug`, `files`, and the mandatory non-empty `reason`. The function scope matches only diagnostics carrying that exact function name, so it does not silence the same warning on neighbouring functions.

## Performance / code smells

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `non-tail-recursion` | warning | Recursive call isn't in tail position; compiler can't TCO. | Convert to accumulator style. |
| `perf-list-len` | warning | `List.len` called inside recursion; O(n²) emergent cost. | Cache length outside the recursion. |
| `perf-string-concat` | warning | String concatenation inside recursion. | Accumulate in a list; join once. |
| `perf-nested-match` | warning | Nested `match` on the same subject. | Combine into one `match`. |
| `perf-loop-invariant` | warning | Expression recomputed every recursive call but doesn't depend on recursion. | Hoist outside the recursion. |
| `cse-match` | warning | Subexpression computed in both the match condition and an arm body. | Bind once above the match. |
| `cse-duplicate` | warning | Expression computed multiple times in one function. | Bind and reuse. |
| `unused-binding` | warning | `let` binding introduced but never read. | Remove it or prefix with `_`. |
| `unused-effect` | warning | Declared effect never used in the function body. | Remove the effect from `! [...]`. |
| `effect-granularity` | warning | `! [Namespace]` declared but only specific methods used. | Narrow the declaration. |

## Independence / concurrency

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `independence-hazard` | warning | Independent product branches use effects that may conflict under reordering. | Keep sequential or suppress with `[[check.suppress]]` + reason. |
| `serve-path` | warning | A function that calls `Tcp.poll` reaches, without re-entering itself or another function that polls, a function that is recursive once every function that calls `Tcp.poll` is removed from the call graph, whose effects include an input operation (`Disk.read*`, `Disk.size`, `Disk.listDir`, `Disk.exists`, `Tcp.read*`, `Tcp.accept`, `Tcp.dialled`, `Tcp.peerAddress`, the dialling `Tcp.send`/`Tcp.sendBytes`/`Tcp.ping`/`Tcp.connect`, or bare `Disk`/`Tcp`), and whose recursion is not a walk over a list it was handed (`[_, ..rest]` passed back at the same position); that loop runs to completion inside one turn, so peers waiting on the poll are not served until it returns. Writes alone (`Disk.write*`, `Disk.append*`, `Tcp.write*`, `Tcp.close`) do not qualify: a loop that only writes what it holds is bounded by this turn's data. | Do one step of the loop per turn, run it as its own command, or suppress with `[[check.suppress]]` + reason. |
| `work-shape` | error | A capability names the job handle `Work.Job` at its boundary — which is what makes it a job kind — but does not have the Work shape: exactly `begin(task: T) -> Result<Work.Job, String>` and `take(job: Work.Job) -> Result<Option<R>, String>`, with `T` and `R` ordinary data of the program. | Declare exactly those two operations with those result shapes, and keep capability resources out of the task and the result. |
| `work-binding` | error | A job kind of the program has no `work` binding in `aver.toml`, or its binding names a function this program does not have, one that declares effects, or one whose parameter or result is not the `begin` task type and the `take` payload type. The same slug covers the job seam — the optional `task` and `landed` keys on that binding, the two ends between the turn and an answer state: both are declared or neither, both name a pure function of a module bound with `answer`, and their signatures are `(S) -> Option<T>` and `(S, R) -> S` for that module's state `S`, the `begin` task `T` and the `take` payload `R`. `work` beside `crate`/`package`/`factory`, `task` or `landed` without `work`, and a `work`, `task` or `landed` value that is not `Module.function`, are rejected by the manifest itself. | Add or fix one `[[providers.bindings]]` entry with `capability = "<JobKind>"` and `work = "Module.function"` naming a pure function of the program whose signature is `(T) -> R`; give it both seam keys or neither. |
| `answer-binding` | error | A capability is marked `answer = "Module"` in `aver.toml` and the module cannot answer it: the capability is one the compiler ships (`Console`, `Disk`, `Tcp`, `Time`, `Wait`, `Work`, …), which the runtime's own providers answer and the generated turn calls itself; or the module named is itself a capability module, which declares operations and computes nothing; or the program has no such module; or an answer function's first parameter is not the state, a type the answering module declares; or an operation of the capability has no answer function; or an answer function's signature is not `(S, a1: T1, …) -> Tuple<S, Cap.__<Op>Reply>`; or one module answering several capabilities threads more than one state; or two answered capabilities of one module share an operation name, so one function would have to answer both. `answer` beside `crate`/`package`/`factory`/`version`/`path` or beside `work`, `task`/`landed` beside `answer`, and an `answer` value that is not one module name, are rejected by the manifest itself. | Declare a capability of your own rather than marking one the compiler ships; write one answer function per operation of an ordinary module, taking that module's own state first and returning it beside the operation's generated reply sum; rename an operation when two answered capabilities collide. |
| `answer-shape` | error / warning | What an answer function itself does. **Error**: the function declares `yield` — an answer is computed inside the turn, so it cannot itself be a process the turn has to drive. **Warning**: the function declares effects; an answer runs inside the turn, so this is allowed, but it can stall every other process. | Drop `yield` from the answer function and make the process that needs it the caller instead; for the warning, accept the stall, or move the work into a job. |
| `run-binding` | error | `aver.toml` declares `[run]`, so the loop of this program is generated, and something it is generated from is missing or is the wrong shape: the module named by `view` writes no process; a process takes parameters, or answers something other than `Unit`, so the loop has nothing to seat it with or nowhere to put its result; the module writes its own `main`, which the loop generates; the module does not `depends` on a module the loop names; an answer module has no pure `fresh() -> <Module>.State`; two answer modules would hold their state in one field of the run table, or one of them would take a field the loop writes itself (`slots`, `jobs`, `dropped`, `stopping`, `nextId`); a process is written in another module of the program, where nothing seats it; the manifest declares a job kind and `[work] max-jobs` is not `1`, which would start one task once per slot of room; a policy is missing, effectful, or does not read the view; or the four `[run]` keys do not all name one module. A `[run]` table that declares fewer than all four keys, or a value that is not `Module.function` / `Module.Type`, is rejected by the manifest itself. | Write the processes, the answer modules and the three policies, and let the loop write the rest: give every process no parameters and a `Unit` result, write every process in the module the loop is generated into, give every answer module a pure `fresh` and a name no other answer module and no field of the run table shares, keep `[work] max-jobs` at `1`, and name all four `[run]` keys in that same module. |
| `view-shape` | error | The record the three policies read, or the sum it keys its pending table by, is not the shape the generated loop fills: a missing or extra field, a field of the wrong type, a missing `Pending` sum, a constructor for no process of this program, a process with no constructor, or a constructor that does not carry the instance number and the wake. The message prints the whole declaration the loop fills. | Declare the view record and the marker sum exactly as the message prints them. |
| `intercept-outside-yield` | error | A function that does not declare `yield` performs an operation of a capability `aver.toml` marks `answer = "Module"`. Such an operation is a request: the lowering cuts a process at it and hands it to the coordinator, and nothing answers it anywhere else — no provider is bound to the capability and no request kind exists for the call. | Add `yield` to the function's effect list so the call becomes a request, or call the answer module's own function directly — the message names it with its parameters — if what you wanted was the answer itself. |
| `work-target` | error | A program with a Work-shaped capability, or one that answers a capability with `answer = "Module"`, was compiled or run on a backend other than the bytecode VM (`--target rust`, `--target wasm-gc`, `--target wasip2`, `aver run --wasm-gc`, `aver run --wasip2`, `aver verify --wasm-gc`). An answered capability's generated reply types carry `Wait.Wake`, which reaches the job handle `Work.Job`, and only the VM has a representation for one. | Run the program on the VM; the Rust, wasm-gc and wasip2 backends follow in a later change. |

## Decisions / exposure

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `unknown-impact` | warning | A `decision`'s impact symbol doesn't resolve in scope. | Check spelling; remove if intentional. |
| `unused-expose` | warning | Module `exposes` a name nothing in the checked program(s) imports; a directory input judges every program in it. | Drop from `exposes` or start using it. |
| `stdlib-shadow` | warning | A `depends` entry names an embedded standard module while a same-named project file exists; the project file is silently ignored because the standard library wins resolution. | Rename the project module and its `depends [...]` entries to use the project file. |

`stdlib-shadow` reaches you on two channels and only one of them is suppressible: the structured finding honours `[[check.suppress]]` like every other warning, while the module loader's stderr `warning:` line is emitted at resolution time on every command (`run`, `verify`, `compile`, …) and deliberately ignores suppression — silently loading different code than the project file on disk is a change of program meaning, not a style opinion. The stderr line is printed once per process per shadowed module name.

## Naming conventions

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `bad-fn-name` | warning | Function name isn't camelCase (ignoring single leading `_`). | Rename the function to camelCase; fix call sites manually. |
| `bad-type-name` | warning | Type name isn't PascalCase. | Rename the type to PascalCase. |
| `bad-module-name` | warning | Module name isn't PascalCase. | Rename module; update `depends` and file path to match. |
| `bad-variant-name` | warning | Sum-type variant isn't PascalCase. | Rename the variant to PascalCase. |
| `bad-field-name` | warning | Record field isn't camelCase. | Rename the field to camelCase. |

## Verify / replay results

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `verify-mismatch` | fail | Declared verify case ran but the observed result ≠ the expected one. | Inspect `fields.expected` / `fields.actual`; fix the function or the case. |
| `verify-hostile-mismatch` | fail | Same as `verify-mismatch` but the case was injected by `aver verify --hostile`. Either a value-side boundary (in `law` form, when typed `given` is augmented with the per-type adversarial set) or an effect-side adversarial profile (in `trace` form, when a classified effect's user-given stub is overridden). `fields.origin` distinguishes the two; `from_hostile` is `true`. | **Value-side** (law form): add `when <precondition>` to scope the law, or drop `law` for plain `verify <fn>` cases-form. **Effect-side** (trace form, no `when` available there): adjust the impl to handle the adversarial world, or run the test without `--hostile` if it's intentionally example-only. Distinct slug so CI gates can route declared vs adversarial failures separately. |
| `verify-provider-setup` | fail | The configured provider host was built, but a binding could not be installed for this file's exact capability contract. | Inspect `fields.provider_error`; fix the binding contract hash, operation set, or provider factory. This is not a source type error. |
| `verify-runtime-error` | fail | Verify case crashed during evaluation (div-by-zero, pattern fail, etc.). | Fix the crash; add a case for the boundary if intentional. |
| `verify-declined` | fail | Verify case exceeded its per-case step budget, so it was not answered — neither a pass nor a counter-example. | Raise the budget for that fn with an `aver.toml` `[[verify.costly]]` entry and say why the case is expensive; or shrink the case. |
| `turn-budget` | warning | With `[verify] turn-budget = N` set, one turn of a case — the VM steps since its last `Tcp.poll`, or since the case began — ran past N; reported once per case with the innermost function at the limit. Only VM steps count. | Wait more often: do one step of the named loop per turn, or move it out of the poll loop. |
| `verify-unexpected-err` | fail | Case propagated a `Result.Err` via `?` the case didn't account for. | Either expect the `Err` in the case or handle it inside the function. |
| `replay-output-mismatch` | fail | Replayed recording's output differs from the recorded run. | Inspect `fields.diff`; update the function or re-record. |
| `replay-error` | fail | Replay couldn't complete (format mismatch, missing effects, crash). | Check `fields.error`; format drift usually means re-record. |
| `proof-citation-cycle` | warning | `aver proof` (Lean): two law blocks of one file reach each other's function, so no order of their theorems lets each cite the other; the pair keeps source order and the one written first cannot cite the second. Laws are otherwise declared before every law whose calls reach their function, wherever the file puts them. | Decide which law should cite the other and write that one second. |

## Format (mechanical rewrites)

All format slugs are `warning` severity. They fire from `aver format --check` (and `aver audit`). Every rewrite carries a `FormatViolation` with the stable `rule` slug below.

| Slug | Fires when |
|---|---|
| `needs-format` | Aggregate warning on a file; `regions` carry the per-rule violations. |
| `tab-indent` | Leading indent contains a tab character. |
| `bad-function-header` | `fn` signature spacing / parameter separators differ from canonical. |
| `effects-unsorted` | `! [...]` list isn't sorted alphabetically. |
| `effects-reshape` | Effect declaration needs canonical single/multi-line reshape. |
| `verify-misplaced` | Verify block isn't placed immediately after its function. |
| `excess-blank` | More than 2 consecutive blank lines inside a block. |
| `module-intent-reshape` | Module intent block needs the canonical multiline form. |
| `decision-inline` | Decision fields packed on a single line; each should be its own line. |
| `trailing-whitespace` | Line ends with whitespace. |
| `missing-final-newline` | File doesn't end with a newline. |

## Fallbacks

| Slug | Severity | Fires when |
|---|---|---|
| `check` | warning | The classifier couldn't narrow a finding to a known slug. If you see this often, add a new classifier branch. |

## LSP integration

Every Diagnostic carries its slug in LSP's `code` field, so editors with `code_description` support can link back to this page. The LSP server doesn't embed a per-slug URL today; consumers can build one using the slug as an anchor:

```
https://github.com/jasisz/aver/blob/main/docs/diagnostics-slugs.md#<slug>
```
