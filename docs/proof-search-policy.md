# Proof search policy checkpoint

Source lowering and automatic search now have separate entry points.
`proof_lower::lower` preserves source claims, premises, dependencies and
induction arguments. `proof_search::populate` optionally adds concrete
applications of earlier laws; both Lean and Dafny check those applications.
Its `ApplicationSearchBudget` is explicit and its report records application
counts and steps reaching a resource limit. Re-running search replaces its
suggestions. A zero budget removes them without changing source obligations.

Dafny's fixed-count unfolding policy runs inside the backend. Its former
`LawUnfolding.depth` field has been removed from ProofIR. The existing budget
values and supported source shapes are retained; this separation does not
make the policy complete or shared with Lean.

The experimental acyclic-reversal switch from the earlier draft of #1324
has been withdrawn. The list library still checks general reverse/append
and double-reversal lemmas, independently tested with true and false claims.
The reverse examples below remain source-level diagnostic inputs; their
Dafny success is no longer claimed by this change.

## Alias and import follow-up

Ordinary recursive laws now use a shared analysis view that eliminates local
value aliases without changing the exported function. It substitutes local
reads only, preserves type annotations, respects pattern shadowing and retains
bindings whose removal would capture a name. Calls and computed expressions
are not inlined. The same view feeds recursion contracts, ProofIR induction
and Lean's induction argument extraction; Lean reduces the original lets when
checking the source definition.

Concrete application search now admits exported ordinary suppliers from earlier
dependency modules as well as earlier laws in the same module. Module order
and visibility are checked independently of ProofIR's entry-first storage
order. Both emitters retain canonical function IDs and apply their existing
universal-statement admission gates. Lean's definition and dependency citation
sets use the resolved ProofIR cone, so an unrelated local `read` cannot replace
`Reader.read`.

Regression cases cover alias chains, recursive arguments through aliases,
imported readers, transitive modules, colliding local names and hidden supplier
visibility. A false imported supplier still fails universal verification even
when every supplied VM sample passes. This does not increase search budgets or
restore the withdrawn reversal strategy. Callable aliases, capture requiring
alpha-renaming and the remaining backend-specific guided induction analyses
were outside that alias/import checkpoint.

Re-running the same seven-source matrix on 2026-09-09 with compiler SHA-256
`338f1c3583f5fdd4a5c406b624835f4402fc58f1325ac9bb025714eaf2c4f51d`
proves all three laws in both `roundtrip_alias` and `roundtrip_import` on
both backends. The other ten rows retain their previous pass/open status,
including the two open Dafny reversal cases. All source hashes match the
original checkpoint's after-run below; the binary stayed unchanged and no
outer timeout fired. This uses the same strict source-law count and checker
budgets, not just a successful process exit.

## Guarded explanation follow-up

Guided laws also record one optional `LawInduction` plan per `because` step.
The plan follows the explanation function's actual recursive arguments and
retains the law guard plus all preceding explanations at those arguments.
Exhaustive nested Boolean matches contribute path conditions after the list's
head and tail have been bound. Value substitution preserves source and law
scopes even when their names overlap. These plans currently cover a single
recursive path in a function owned by the law's module; other shapes retain
the existing backend strategies.

Dafny emits checked recursive lemma calls from these plans, while Lean uses
the same source call and measure to select induction. Both still prove the
complete obligation. Dafny additionally checks the general cons equation for
`List.take` locally in laws that use it. This does not change the builtin's
definition or add axioms, and keeps that equation out of unrelated SMT queries.

## Original checkpoint comparison (#1324)

One local run per binary on 2026-09-09, with two concurrent checker invocations
and unchanged production checker budgets. The 90-second outer timeout did
not fire. Times and full checker output are recorded by the matrix tool;
this is a behavior comparison, not a controlled performance benchmark.

Before: draft #1324 at `4044ec64`, compiler SHA-256
`5c73435c63254d97ecf17cfcc9bdaacc2e11effa757fc44c2bd6b9a84b9c5a8c`.

After: compiler SHA-256
`c047210fe568cc5c28321fe721d9e7348856c78f83e452ce18998e46b65148ef`.

Both binaries stayed byte-identical during their respective runs, and all
source hashes agree between runs. The baseline's strict coverage fields were
computed from its retained sources, manifests and outputs after the run.

A pass requires all expected source laws: Lean's universal manifest count or
Dafny's emitted ordinary-law declarations plus its whole-file zero-error,
zero-axiom, zero-omission, zero-timeout gate. Merely exiting successfully is
insufficient. All inputs in this matrix use ordinary laws.

| Source variation | Backend | Before | After |
|---|---|---|---|
| roundtrip | lean | pass | pass |
| roundtrip | dafny | pass | pass |
| roundtrip_alias | lean | open | open |
| roundtrip_alias | dafny | open | open |
| roundtrip_equation | lean | pass | pass |
| roundtrip_equation | dafny | pass | pass |
| roundtrip_unrelated | lean | pass | pass |
| roundtrip_unrelated | dafny | pass | pass |
| roundtrip_import | lean | open | open |
| roundtrip_import | dafny | open | open |
| reverse_algebra | lean | pass | pass |
| reverse_algebra | dafny | pass | open |
| signed_frame | lean | pass | pass |
| signed_frame | dafny | pass | open |

The alias variant adds only `current = value` and tests the branch on
`current`. Lean exits successfully but its manifest contains only one of the
three source laws; Dafny omits two universals. Both are therefore open here.
Moving the reader and its law into an imported module leaves the roundtrip
open in both backends. These are pre-existing limitations, not new regressions.

Equation reversal and an unrelated earlier law retain their complete proofs.
Both Dafny reversal examples lose the experimental automatic result:
`reverse_algebra` reports three errors and `signed_frame` one solver timeout.
The checked library lemmas themselves continue to verify.

## Reproducing the measurement

~~~sh
python3 tools/proof_search_matrix.py --aver /absolute/path/to/aver --out /new/output/directory
~~~

The output directory must not exist. The tool saves sources, source and binary
hashes, all checker output and `report.json`. Its process exit denotes
completion of measurement; inspect each row's `strict_passed` for proof credit.

## Remaining work

This checkpoint separates the recent policies; older ProofIR strategies and
backend tactic portfolios still need review. Guided list and quotient analysis
also still have backend-local source-shape restrictions. Shared source-scope
analysis and dependency identity remain the direction for further work, with
these refactoring cases as controls. Adding solver triggers or unfold instructions
to the author's Aver proof is not a remedy for that analysis gap.
