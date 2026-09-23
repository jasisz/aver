# Proof search policy checkpoint

Source lowering and automatic search now have separate entry points.
`proof_lower::lower` keeps source claims, premises, dependencies and induction
arguments as written. `proof_search::populate` can add concrete applications of
earlier laws, and both Lean and Dafny check those applications. Its
`ApplicationSearchBudget` is explicit, and its report records how many
applications it made and which steps hit a resource limit. Running search again
replaces its earlier suggestions. A zero budget removes them and leaves the
source obligations alone.

Dafny's fixed-count unfolding policy now lives inside the backend, and the old
`LawUnfolding.depth` field is gone from ProofIR. Budget values and supported
source shapes are the same as before. Moving the policy does not make it
complete, and Lean does not share it.

The experimental acyclic-reversal switch from the earlier draft of #1324 has
been withdrawn. The list library still checks the general reverse/append and
double-reversal lemmas, which are tested separately with true and false claims.
The reverse examples below stay as source-level diagnostic inputs. This change
no longer claims that Dafny proves them.

## Alias and import follow-up

Ordinary recursive laws now go through a shared analysis view that removes local
value aliases. The exported function is unchanged. The view substitutes local
reads only, keeps type annotations, respects pattern shadowing, and keeps any
binding whose removal would capture a name. It does not inline calls or computed
expressions. Recursion contracts, ProofIR induction and Lean's induction
argument extraction all read this view. When Lean checks the source definition,
it reduces the original lets.

Concrete application search now accepts exported ordinary suppliers from earlier
dependency modules, in addition to earlier laws in the same module. Module order
and visibility are checked separately from ProofIR's entry-first storage order.
Both emitters keep canonical function IDs and apply their existing gates for
admitting universal statements. Lean builds its definition and dependency
citation sets from the resolved ProofIR cone, so an unrelated local `read`
cannot stand in for `Reader.read`.

Regression cases cover alias chains, recursive arguments passed through aliases,
imported readers, transitive modules, colliding local names and hidden supplier
visibility. A false imported supplier still fails universal verification, even
when every supplied VM sample passes. Search budgets did not grow, and the
withdrawn reversal strategy stays withdrawn. The alias/import checkpoint did not
cover callable aliases, capture that needs alpha-renaming, or the remaining
backend-specific guided induction analyses.

On 2026-09-09 the same seven-source matrix was run again with compiler SHA-256
`338f1c3583f5fdd4a5c406b624835f4402fc58f1325ac9bb025714eaf2c4f51d`.
All three laws in `roundtrip_alias` and in `roundtrip_import` now prove on both
backends. The other ten rows keep their earlier pass/open status, including the
two open Dafny reversal cases. Every source hash matches the after-run of the
original checkpoint below. The binary did not change during the run and no outer
timeout fired. Passing here means the same strict source-law count under the
same checker budgets, not only a successful process exit.

## Guarded explanation follow-up

Guided laws also record one optional `LawInduction` plan for each `because`
step. The plan follows the recursive arguments the explanation function actually
uses, and carries the law guard plus every earlier explanation at those
arguments. Exhaustive nested Boolean matches add path conditions once the list's
head and tail are bound. Value substitution keeps source and law scopes apart
even when their names overlap. For now these plans cover a single recursive path
in a function owned by the law's module. Other shapes use the existing backend
strategies.

Dafny turns these plans into checked recursive lemma calls. Lean uses the same
source call and measure to pick its induction. Both backends still prove the
complete obligation. In laws that use `List.take`, Dafny also checks the general
cons equation for it locally. The builtin's definition is unchanged, no axioms
are added, and the equation stays out of unrelated SMT queries.

## Original checkpoint comparison (#1324)

One local run per binary on 2026-09-09, with two checker invocations running at
once and the production checker budgets unchanged. The 90-second outer timeout
did not fire. The matrix tool records times and full checker output. This is a
behavior comparison. It is not a controlled performance benchmark.

Before: draft #1324 at `4044ec64`, compiler SHA-256
`5c73435c63254d97ecf17cfcc9bdaacc2e11effa757fc44c2bd6b9a84b9c5a8c`.

After: compiler SHA-256
`c047210fe568cc5c28321fe721d9e7348856c78f83e452ce18998e46b65148ef`.

Each binary stayed byte-identical through its run, and all source hashes agree
between the two runs. The baseline's strict coverage fields were computed after
the run from its saved sources, manifests and outputs.

A pass needs every expected source law. For Lean that means the universal count
in the manifest. For Dafny it means the emitted ordinary-law declarations plus
the whole-file gate: zero errors, zero axioms, zero omissions, zero timeouts. A
successful exit is not enough. Every input in this matrix uses ordinary laws.

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

The alias variant only adds `current = value` and branches on `current`. Lean
exits successfully, but its manifest holds just one of the three source laws,
and Dafny omits two universals, so both count as open here. Moving the reader
and its law into an imported module leaves the roundtrip open on both backends.
Both limitations existed before this change.

Equation reversal and the unrelated earlier law keep their complete proofs. Both
Dafny reversal examples lose the result the experimental switch gave them:
`reverse_algebra` reports three errors and `signed_frame` one solver timeout.
The checked library lemmas still verify.

## Reproducing the measurement

~~~sh
python3 tools/proof_search_matrix.py --aver /absolute/path/to/aver --out /new/output/directory
~~~

The output directory must not exist yet. The tool saves the sources, source and
binary hashes, all checker output and `report.json`. Its exit status only says
the measurement finished. For proof credit, read each row's `strict_passed`.

## Remaining work

This checkpoint separates the recent policies. Older ProofIR strategies and the
backend tactic portfolios still need review. Guided list and quotient analysis
still carry source-shape restrictions local to each backend. The plan is to move
toward shared source-scope analysis and dependency identity, using these
refactoring cases as controls. Adding solver triggers or unfold instructions to
the author's Aver proof does not fix that analysis gap.
