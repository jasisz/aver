# Aver Diagnostic Slugs

Every diagnostic Aver emits carries a stable `slug`. Consumers (CLI, LSP, playground, agent frameworks) can key off it. This page is the reference list, grouped by category.

Source of truth: `src/diagnostics/classify.rs` (classifier) and `src/checker/*.rs` / `src/main/format_cmd.rs` (emitters). If you edit a table here, check that the slug string actually appears in one of those files.

## Severity meanings

- **`error`**: compilation or contract failure. It blocks later steps (a type error stops `aver verify`, for example).
- **`warning`**: a code smell that does not block anything. An agent or reviewer should see it, but the program still runs.
- **`fail`**: a runtime, verify or replay divergence. The program typechecks but misbehaves on the given case.
- **`hint`**: an IDE-only nudge. The LSP shows it; the CLI usually ignores it.

## Type & lexer

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `parse-error` | error | Lexer or parser failed on the source. | Fix the syntax error; canonical examples are in `docs/language.md`. |
| `type-error` | error | Typechecker reports an error the classifier did not narrow. | Read the underlying message; contract inference fell through. |
| `type-mismatch` | error | Expression's inferred type differs from the declared context. | Change the expression to produce the expected type. |
| `unknown-ident` | error | A referenced name has no binding in scope. | Check spelling or add the missing import. |
| `arity-mismatch` | error | Function or constructor called with the wrong number of args. | Adjust the number of arguments. |
| `map-key-unordered` | error | A map's key type has no order that every backend and the proof model can state: `Float`, or a `Map` or `Vector` reached through the key. | Key on a value that has an order: `Int`, `String`, `Bool`, or a record, variant, list or tuple built out of them. |
| `effect-violation` | error | A function calls an effect it does not declare in `! [...]`. | Add the missing effect to the function's `! [...]`. |
| `int-div` | error | The `/` operator was used on two `Int`s. Integer division is partial (the divisor may be zero → `Result.Err`), so Aver makes it a function instead of an operator. | Use `Int.div(a, b) : Result<Int, String>` and handle it with `match` or `Result.withDefault`. With a nonzero literal divisor, `Int.div(a, k)` is total and returns plain `Int`. |
| `error-prop-non-result` | error | `?` was applied to an expression that is not a `Result`. | Drop the `?`. A smart-constructor call over an all-literal list inside the refinement's proven element interval (`Bytes.fromList([0, 10, 255])`) is total and already returns the refined type. |
| `pattern-subject-mismatch` | error | A `Result` / `Option` constructor pattern was matched against a subject of some other type, so no value can ever take the arm. | Match the value's own shape. A discharged literal smart-constructor call returns the refined type, not a `Result`. |
| `yield-direct-call` | error | A function, an unsupported verify form, or another yielding function's argument calls a `yield` function directly. The function never runs as written. | Call its generated `__<fn>Start(...)` and answer the requests its `__<Fn>Outcome` carries, using `__<fn>Answer<Kind>` for each request kind. |
| `yield-non-tail-call` | error | A yielding function calls itself outside tail position. A process can nest another yielding function, but not itself. | Pass what comes next as data, or make it a tail call. |
| `yield-unsupported` | error | A yielding function contains a construct the lowering does not cut: mutual nesting between two yielding functions, a cycle of tail calls between them, a request or a call to a yielding helper inside an independent product `(a, b)!`, a function value live across a request, or no stop at all. | Restructure as the message says: break the cycle, perform the calls one after another, pass data instead of a callback, or drop `yield`. |

## Intent / verify hygiene

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `missing-verify` | error | A pure, non-trivial function has no `verify` block. A function with a parameter no verify case can write (a capability resource, or a type that always carries one) is exempt. | Add a `verify` block with representative cases. |
| `missing-description` | warning | A function has no `? "..."` description. | Add a `? "description"` line after the signature. |
| `verify-coverage` | warning | Verify block has too few cases for a reachable return or input shape. | Add a case for the missing shape. If the shape really is uninhabited, use a function-scoped `[[check.suppress]]` with a reason. |
| `verify-law` | warning | `verify law` names a function it never actually calls. | Use the named function in the law body or rename the law. |
| `verify-rhs` | error | Case calls the target on the right-hand side of `=>`. | The right side must be the expected value, not another call to the target. |

`verify-coverage` checks two kinds of claim and discharges them differently.

**Return-shape** claims are `Result.Ok` / `Result.Err`, `Option.Some` / `Option.None`, `true` / `false`, and the constructors a returned sum-producing function can actually return. To find those constructors the lint follows local calls transitively, including mutually recursive return paths. If an external or otherwise opaque return path keeps it from reaching a closed answer, it makes no constructor-count claim at all; it does not guess by using the whole declared sum as the denominator. A case satisfies a shape by writing it on the right of `=>`, either directly or through a local helper whose outer shape is statically unique (for example a `blank()` that always returns `Result.Err`). The checker does not execute arbitrary calls while linting. A builtin or provider call such as `Bytes.fromHex("zz")`, or a helper that can return several shapes depending on its arguments, gets no credit from syntax alone, even if a later `aver verify` run observes one arm. The `Ok` claim can also be met another way: apply `?` to a call of the function under verification anywhere on the left of `=>`. `readOne([7, 9])?.value => 7` pins `Ok` at least as firmly as `=> Result.Ok(...)` does, because an error would have propagated and failed the case, and it pins a field as well.

Return-shape claims can also be inherited. Take a function outside its module's `exposes` list whose mutual-recursion group is reached by exactly one caller from outside that group. If that caller's examples already pin every arm, the function is not asked for return-shape cases of its own, because the caller's vectors are what drive it. A file with no `module` declaration exposes every name that does not start with `_`, so nothing there counts as private and nothing is inherited.

**Input-shape** claims are never inherited. These are empty and non-empty list arguments, both branches of a `Bool` argument, every constructor of an argument's sum type, a recursive function's base case, and an empty string for a parser. A caller that covers both of its own arms says nothing about which arguments a helper was exercised with. Structurally unique local helpers get credit for Bool, list and enum input shapes too. Arbitrary calls stay opaque.

When a real signature shape is deliberately uninhabited, keep the suppression explicit and narrow. `[[check.suppress]]` accepts an optional `fn = "eachInBranch"` beside `slug`, `files` and the mandatory non-empty `reason`. The function scope matches only diagnostics that carry that exact function name, so the same warning on neighbouring functions still shows.

## Performance / code smells

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `non-tail-recursion` | warning | Recursive call is not in tail position, so the compiler cannot apply TCO. | Convert to accumulator style. |
| `perf-list-len` | warning | `List.len` called inside recursion, which adds up to O(n²). | Compute the length once, outside the recursion. |
| `perf-string-concat` | warning | String concatenation inside recursion. | Accumulate in a list and join once. |
| `perf-nested-match` | warning | Nested `match` on the same subject. | Combine into one `match`. |
| `perf-shared-update` | warning | A Map or Vector read out of a record (`setting.window.created`) is updated in place while the record still holds it, in a function that runs again and again (recursive, reached from a recursive function of its module, or part of an answer module). The update is `Map.set`, `Map.remove` or `Vector.set` on the field, or a call that hands the field to one of them and returns the updated collection (followed into the dependencies the call names). The record still holds it when it is read again after that call, or when it was already passed whole to the call or record the update is an argument of. Each such update copies the whole collection. A record update or literal that reads the field it replaces once, reads nothing else of the record but other fields, and is the record's last use is not reported: the VM takes the field out first. Not seen: a caller that keeps the record it passed, an alias made through a binding, one collection in two records. | Take the field out of the record before updating it (bind the parts with a `match` and carry on with a record that no longer holds them, or give it an empty collection in their place), or read it at the record's last use. |
| `perf-loop-invariant` | warning | An expression is recomputed on every recursive call but does not depend on the recursion. | Hoist it outside the recursion. |
| `cse-match` | warning | Subexpression computed in both the match condition and an arm body. | Bind it once above the match. |
| `cse-duplicate` | warning | Expression computed more than once in one function. | Bind it and reuse it. |
| `unused-binding` | warning | `let` binding introduced but never read. | Remove it or prefix it with `_`. |
| `unused-effect` | warning | Declared effect never used in the function body. | Remove the effect from `! [...]`. |
| `effect-granularity` | warning | `! [Namespace]` declared but only specific methods used. | Narrow the declaration. |

## Independence / concurrency

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `independence-hazard` | warning | Independent product branches use effects that may conflict if reordered. | Keep them sequential, or suppress with `[[check.suppress]]` + reason. |
| `serve-path` | warning | A function that calls a wait (`Tcp.poll`, or `Wait.poll`, the wait the generated coordinator performs) reaches a loop that runs to completion inside one turn, so peers waiting on the poll are not served until it returns. The path must not re-enter the waiting function or go through another function that waits. The loop is a function that is still recursive once every function that calls a wait is removed from the call graph. Its effects include an input operation (`Disk.read*`, `Disk.size`, `Disk.listDir`, `Disk.exists`, `Tcp.read*`, `Tcp.accept`, `Tcp.dialled`, `Tcp.peerAddress`, the dialling `Tcp.send`/`Tcp.sendBytes`/`Tcp.ping`/`Tcp.connect`, or bare `Disk`/`Tcp`), and its recursion is not a walk over a list it was handed (`[_, ..rest]` passed back at the same position). Writes alone (`Disk.write*`, `Disk.append*`, `Tcp.write*`, `Tcp.close`) do not count: a loop that only writes what it holds is bounded by this turn's data. | Do one step of the loop per turn, run it as its own command, or suppress with `[[check.suppress]]` + reason. |
| `wait-key` | error | The program keys one wait set by one type and another by a different type, or hands a wait a map whose key type nothing in the program settles. A wait's key is the key of the map it was handed, and every backend names the wait's helpers from that one type. A turn has one wait, and its keys are how the program says what it is waiting for. A key nobody can name is one each backend would answer differently. | Make both kinds constructors of one type and key every wait by it. `Wait.poll` accepts any key a map accepts, so the type can say whatever the program needs it to say. For a wait whose key is not settled, bind the map to a name that writes its type down, `items: Map<Int, Wait.Item> = ...`, and hand that name to `Wait.poll`. |
| `work-shape` | error | A capability names the job handle `Work.Job` at its boundary, which makes it a job kind, but it does not have the Work shape. That shape is exactly `begin(task: T) -> Result<Work.Job, String>` and `take(job: Work.Job) -> Result<Option<R>, String>`, with `T` and `R` ordinary data of the program. | Declare exactly those two operations with those result shapes, and keep capability resources out of the task and the result. |
| `work-binding` | error | A job kind of the program has no `work` binding in `aver.toml`, or its binding names a function that the program does not have, that belongs to the entry module the command was pointed at, that declares effects, or whose parameter or result is not the `begin` task type and the `take` payload type. The manifest itself rejects `work` beside `crate`/`package`/`factory`, a `work` value that is not `Module.function`, and the `task`, `started` and `landed` keys of the removed job seam: an answer module now begins a job itself and parks the request on it. | Add or fix one `[[providers.bindings]]` entry with `capability = "<JobKind>"` and `work = "Module.function"`. It must name a pure function with signature `(T) -> R` in a module of the program other than the entry module. Replace a seam with a job the answer module begins and waits on with `Run.Wake.Until([Wait.Item.Job(job)], Option.None)`. |
| `answer-binding` | error | A module says `answers [Cap]` in its header and cannot answer that capability. This happens when: the capability is one the compiler ships (`Console`, `Disk`, `Tcp`, `Time`, `Wait`, `Work`, …), which the runtime's own providers answer and the generated turn calls itself; the module is itself a capability module, which declares operations and computes nothing; an answer function's first parameter is not the state, a type the answering module declares; an operation of the capability has no answer function; an answer function's signature is not `(S, a1: T1, …) -> Tuple<S, Result<R, Run.Wake>>`, with `R` the operation's own result; one module answering several capabilities threads more than one state; or two answered capabilities of one module share an operation name, so one function would have to answer both. The manifest itself rejects the `answer` key that used to say this, with the header to write. | Declare a capability of your own instead of answering one the compiler ships. Write one answer function per operation in an ordinary module; it takes that module's own state first and returns it beside `Result<R, Run.Wake>`. Rename an operation when two answered capabilities collide. |
| `answer-shape` | error / warning | Covers what an answer function itself does. **Error**: the function declares `yield`. An answer is computed inside the turn, so it cannot itself be a process the turn has to drive. **Warning**: the function declares effects that can block. This is allowed, since an answer runs inside the turn, but it can stall every other process. Effects that return at once are not named and do not warn: `Tcp.readNow`, `Tcp.writeNow`, `Tcp.accept`, `Tcp.dialled`, `Tcp.beginConnect`, `Tcp.listen`, `Tcp.close`, `Tcp.closeListener`, `Time.unixMs`, `Time.now`, `Random.int`, `Random.float`, `Process.stopRequested`, `Work.cancel`, and the `begin` and `take` of a job kind. | Drop `yield` from the answer function and make the process that needs it the caller instead. For the warning, accept the stall or move the work into a job. |
| `run-binding` | error | Something the generated loop is built from is missing or has the wrong shape. A process of the entry takes parameters and no `process <name> seated by <Module.function>` line names it, or it answers something other than `Unit`; a seating line names no yielding function of the entry, names a helper another process enters, or names a function that is not a pure `(S) -> List<K>` of an answer module whose `K` is the process's parameter type; an answer module has no pure `fresh() -> <Module>.State`; two answer modules would hold their state in one field of the run table, or one of them would take a field the loop writes itself (`slots`, `versions`, `late`, `dropped`, `stopping`, `now`, `nextId`, or a `seated…`/`retired…` field); the entry's `stop` or `admit` is not `stop(view: Run.View) -> Bool` or `admit(view: Run.View, id: Int) -> Bool`, or declares effects; `main` calls `Run.all()` in a module that writes no process the loop can seat; or a seating line stands in a module whose own `main` does not call `Run.all()`. The manifest itself rejects a `[run]` table, which is gone. | Write the processes, the answer modules and any policies, and let the loop write the rest. Declare a keyed process with its seating line; give every answer module a pure `fresh`; write `stop` and `admit` with the shapes the message prints; call `Run.all()` from `main` or remove `main`; remove `[run]` from the manifest. |
| `view-shape` | error | History. With custom `[run]` policies, the program declared the view record the policies read, and this slug refused one of the wrong shape. The view is now generated as `Run.View`, and nothing emits this slug any more. | Nothing to repair. Name the view `Run.View` in `stop` and `admit`. |
| `intercept-outside-yield` | error | A function that does not declare `yield` performs an operation of a capability that a module of the program answers with `answers [...]`. Such an operation is a request: the lowering cuts a process at it and hands it to the coordinator. Nothing answers it anywhere else, because no provider is bound to the capability and no request kind exists for the call. | Add `yield` to the function's effect list so the call becomes a request. If you wanted the answer itself, call the answer module's own function directly; the message names it with its parameters. |
| `work-target` | error | History. A program with a Work-shaped capability (a job kind) used to be refused on a wasm backend. Since jasisz/aver#1329 every target runs a job kind. The VM and `--target rust` run it on a thread beside the turn. `--target wasm-gc` / `--target wasip2` run it inline at `begin`, because a component and a wasm-gc module are single-threaded. Nothing emits this slug any more. | Nothing to repair. See [services.md](services.md#on-wasm-gc-and-wasip2) for what a job does on a wasm target. |
| `work-max-jobs-ignored` | warning | `aver.toml` sets `[work] max-jobs`, and a program that declares a job kind is compiled or run for `wasip2`. A program with no job kind starts no job, so the key changes nothing there either and no warning is emitted. On that target a job runs inline at `begin` and finishes before the next expression, so at most one is ever running and the key decides nothing. The warning names the key and the target, and says what differs there: `take` never answers `Ok(None)`, a job that never ends blocks the turn, and a job whose body fails stops the program. wasm-gc runs jobs on host workers and honours the key, so it gets no warning. | Keep the key for the VM, the Rust backend and wasm-gc, where it has an effect, or remove it if the program only ever runs on `wasip2`. |

## Decisions / exposure

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `unknown-impact` | warning | A `decision`'s impact symbol does not resolve in scope. | Check spelling; remove it if intentional. |
| `unused-expose` | warning | Module `exposes` a name nothing in the checked program(s) imports. Manifest-bound functions and types, and names the generated loop uses, count as used. A directory input judges every program in it. | Drop it from `exposes` or start using it. |
| `stdlib-shadow` | warning | A `depends` entry names an embedded standard module while a project file with the same name exists. The standard library wins resolution, so the project file is silently ignored. | Rename the project module and its `depends [...]` entries to use the project file. |

`stdlib-shadow` reaches you on two channels, and only one of them can be suppressed. The structured finding honours `[[check.suppress]]` like every other warning. The module loader's stderr `warning:` line is emitted at resolution time on every command (`run`, `verify`, `compile`, …) and ignores suppression on purpose: silently loading different code than the project file on disk changes what the program means, which is more than a style opinion. The stderr line is printed once per process per shadowed module name.

## Naming conventions

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `bad-fn-name` | warning | Function name is not camelCase (ignoring a single leading `_`). | Rename the function to camelCase; fix call sites by hand. |
| `bad-type-name` | warning | Type name is not PascalCase. | Rename the type to PascalCase. |
| `bad-module-name` | warning | Module name is not PascalCase. | Rename the module and update `depends` and the file path to match. |
| `bad-variant-name` | warning | Sum-type variant is not PascalCase. | Rename the variant to PascalCase. |
| `bad-field-name` | warning | Record field is not camelCase. | Rename the field to camelCase. |

## Verify / replay results

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `verify-mismatch` | fail | A declared verify case ran and the observed result ≠ the expected one. | Inspect `fields.expected` / `fields.actual`, then fix the function or the case. |
| `verify-hostile-mismatch` | fail | Same as `verify-mismatch`, but the case was injected by `aver verify --hostile`. It is either a value-side boundary (`law` form, when typed `given` is augmented with the per-type adversarial set) or an effect-side adversarial profile (`trace` form, when a classified effect's user-given stub is overridden). `fields.origin` tells the two apart; `from_hostile` is `true`. | **Value-side** (law form): add `when <precondition>` to scope the law, or drop `law` for plain `verify <fn>` cases-form. **Effect-side** (trace form, where `when` is not available): change the impl to handle the adversarial world, or run the test without `--hostile` if it is meant as example-only. The slug is separate so CI gates can route declared and adversarial failures differently. |
| `verify-provider-setup` | fail | The configured provider host was built, but a binding could not be installed for this file's exact capability contract. | Inspect `fields.provider_error` and fix the binding contract hash, operation set, or provider factory. This is not a source type error. |
| `verify-runtime-error` | fail | A verify case crashed during evaluation (div-by-zero, pattern fail, etc.). | Fix the crash; add a case for the boundary if it is intentional. |
| `verify-declined` | fail | A verify case exceeded its per-case step budget, so it was not answered: it is neither a pass nor a counter-example. | Raise the budget for that fn with an `aver.toml` `[[verify.costly]]` entry and say why the case is expensive, or shrink the case. |
| `turn-budget` | warning | With `[verify] turn-budget = N` set, one turn of a case ran past N. A turn counts the VM steps since the last wait, `Tcp.poll` or `Wait.poll`, or since the case began. Reported once per case, with the innermost function at the limit. Only VM steps count. | Wait more often: do one step of the named loop per turn, or move it out of the poll loop. |
| `verify-unexpected-err` | fail | A case propagated a `Result.Err` via `?` that the case did not account for. | Either expect the `Err` in the case or handle it inside the function. |
| `replay-output-mismatch` | fail | A replayed recording's output differs from the recorded run. | Inspect `fields.diff`, then update the function or re-record. |
| `replay-error` | fail | Replay could not complete (format mismatch, missing effects, crash). | Check `fields.error`. Format drift usually means you should re-record. |
| `proof-citation-cycle` | warning | `aver proof` (Lean): two law blocks of one file reach each other's function, so no order of their theorems lets each cite the other. The pair keeps source order, and the one written first cannot cite the second. Otherwise a law is declared before every law whose calls reach its function, wherever the file puts them. | Decide which law should cite the other and write that one second. |

## Format (mechanical rewrites)

All format slugs have `warning` severity. They fire from `aver format --check` (and `aver audit`). Every rewrite carries a `FormatViolation` with the stable `rule` slug below.

| Slug | Fires when |
|---|---|
| `needs-format` | Aggregate warning on a file; `regions` carry the per-rule violations. |
| `tab-indent` | Leading indent contains a tab character. |
| `bad-function-header` | `fn` signature spacing or parameter separators differ from canonical. |
| `effects-unsorted` | `! [...]` list is not sorted alphabetically. |
| `effects-reshape` | Effect declaration needs the canonical single-line or multi-line shape. |
| `verify-misplaced` | Verify block is not placed immediately after its function. |
| `excess-blank` | More than 2 consecutive blank lines inside a block. |
| `module-intent-reshape` | Module intent block needs the canonical multiline form. |
| `decision-inline` | Decision fields packed on a single line; each should be on its own line. |
| `trailing-whitespace` | Line ends with whitespace. |
| `missing-final-newline` | File does not end with a newline. |

## Fallbacks

| Slug | Severity | Fires when |
|---|---|---|
| `check` | warning | The classifier could not narrow a finding to a known slug. If you see this often, add a new classifier branch. |

## LSP integration

Every Diagnostic carries its slug in LSP's `code` field, so editors with `code_description` support can link back to this page. The LSP server does not embed a per-slug URL today. Consumers can build one with the slug as the anchor:

```
https://github.com/jasisz/aver/blob/main/docs/diagnostics-slugs.md#<slug>
```
