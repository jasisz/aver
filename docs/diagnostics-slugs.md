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
| `effect-violation` | error | A function calls an effect it doesn't declare in `! [...]`. | Add the missing effect to the function's `! [...]`. |
| `int-div` | error | The `/` operator was used on two `Int`s. Integer division is partial (the divisor may be zero → `Result.Err`), so it is a function, not an operator. | Use `Int.div(a, b) : Result<Int, String>`; handle with `match` or `Result.withDefault`. With a nonzero literal divisor, `Int.div(a, k)` is total and returns plain `Int`. |
| `error-prop-non-result` | error | `?` was applied to an expression that is not a `Result`. | Drop the `?`. A smart-constructor call over an all-literal list inside the refinement's proven element interval (`Bytes.fromList([0, 10, 255])`) is total and already returns the refined type. |
| `pattern-subject-mismatch` | error | A `Result` / `Option` constructor pattern was matched against a subject of some other type — no value can ever take the arm. | Match the value's own shape; a discharged literal smart-constructor call returns the refined type, not a `Result`. |

## Intent / verify hygiene

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `missing-verify` | error | A pure, non-trivial function lacks a `verify` block. | Add a `verify` block with representative cases. |
| `missing-description` | warning | A function lacks a `? "..."` description. | Add a `? "description"` line after the signature. |
| `verify-effectful` | warning | A function with effects has a `verify` block. | Remove `verify`; test via `aver run -e '<call>' --record` + `aver replay --test`. |
| `verify-coverage` | warning | Verify block has too few cases for the return type's shape. | Add cases covering the missing shape (Err/Ok, Some/None, etc.). |
| `verify-law` | warning | `verify law` names a function it never actually calls. | Use the named function in the law body or rename the law. |
| `verify-rhs` | error | Case calls the target on the right-hand side of `=>`. | Right side must be the expected value, not another call to the target. |

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

## Decisions / exposure

| Slug | Severity | Fires when | Repair |
|---|---|---|---|
| `unknown-impact` | warning | A `decision`'s impact symbol doesn't resolve in scope. | Check spelling; remove if intentional. |
| `unused-expose` | warning | Module `exposes` a name nobody imports. | Drop from `exposes` or start using it. |
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
| `verify-runtime-error` | fail | Verify case crashed during evaluation (div-by-zero, pattern fail, etc.). | Fix the crash; add a case for the boundary if intentional. |
| `verify-unexpected-err` | fail | Case propagated a `Result.Err` via `?` the case didn't account for. | Either expect the `Err` in the case or handle it inside the function. |
| `replay-output-mismatch` | fail | Replayed recording's output differs from the recorded run. | Inspect `fields.diff`; update the function or re-record. |
| `replay-error` | fail | Replay couldn't complete (format mismatch, missing effects, crash). | Check `fields.error`; format drift usually means re-record. |

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
