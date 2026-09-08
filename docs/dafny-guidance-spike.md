# Guided proof portability: the Dafny spike

This experiment sends identical Aver files through Lean and Dafny. It covers a
restricted first-order fragment of `because` and `using`; it does not
port K5's rounding proofs to Dafny.

## Supported fragment

Guided laws need an explicit `using` list (`using []` selects no helpers).
Their pure dependency cones may cross explicit module imports. Givens may
contain `Int`, `Bool`, `String`, lists, tuples, `Option`, `Result`, and local or
imported records and sum types. Every named field type, constructor, pattern,
called body and selected law is checked against the same restrictions.
Signatures, bodies and field annotations resolve in their declaring modules;
same-named functions or types in a caller do not replace imported declarations.

Arithmetic includes addition, subtraction, comparisons, multiplication of
arbitrary integers, and `Int.div` / `Int.mod`. Division preserves Aver’s exact
Euclidean semantics, including negative operands. A syntactically nonzero
literal divisor returns `Int`, using the same discharge rule as Aver’s type
checker. A zero or dynamic divisor retains `Result<Int, String>`: zero produces
`Result.Err("division by zero")`, and a nonzero value produces `Result.Ok` of the
quotient or remainder. For a nonzero divisor `d`, the quotient `q` and remainder
`r` satisfy `a = d * q + r` and `0 <= r < Int.abs(d)`. This does not add an integer
`/` operator or erase an error branch.

Selected first-order list operations use native sequences or total defined
helpers. Other unsupported operations still decline, even inside an unused
binding or a branch that the supplied examples never take.

Self-recursive helpers are admitted when the shared classifier recognizes a
guarded integer countdown that subtracts a positive literal, or a checked
quotient countdown by a fixed literal divisor of at least two. The quotient
contract must establish that the source argument is positive at every recursive
call and strictly shrinks. A division expression alone is not such a contract.
The backend validates the entire recursive body; Dafny checks its termination.
Boolean reason predicates can generalize the law’s givens, including growing
accumulators, while decreasing only the selected integer’s nonnegative measure.
An explicit recursive step call instantiates a checked quotient induction when
its source arguments can be translated safely. Integer equations use ordinary
unfolding without an unnecessary induction hint.

Native structural list descent is also admitted. Boolean list reasons can
generalize all givens while decreasing only the list length, so a fold can
change its accumulators. For a simple source match with a unique recursive
call, the backend instantiates the step lemma at that call’s arguments. Dafny
must prove its guard, earlier reasons and termination; no premise is invented.

Explicit `using` citations may select local or visible imported laws. A selected
ordinary law without `because` or `using` is also supported through a separately
checked universal restatement, described below. Automatic citation selection,
mutual recursion, fuel or opaque recursion fallbacks, unsupported countdowns,
higher-order or effectful calls, provider resources, refinements and `Float`
remain outside this guidance fragment. A supported-looking caller that selects
an unsupported helper is declined as well. Admission does not guarantee that
Dafny can complete the proof.

## What the backend checks

Every `because` becomes a separate lemma with the original `when` guard and all
earlier reasons as preconditions. The current reason is its conclusion, never
its own premise. A separate lemma checks the final implication. The parent law
calls every step in order, so an unproved intermediate reason cannot disappear
because the final claim happens to be true.

Explicit citations become checked calls under the cited law's own guard.
Imported statements and their binder types retain their declaring-module
identities when rendered in the caller. Dependency modules import reachable
transitive owners without opening their names, so a supplier’s statement can
refer to its own dependencies without making unrelated bare names visible.

For a selected ordinary law, the backend emits a separate universal lemma in
the consumer with the original guard and an assertion of the original claim.
It proves this statement again; a legacy lemma or finite sample check is not
used as universal evidence. Guided suppliers are called through their checked
parent lemmas. Neither path adds an unconditional assumption or axiom fallback.

Aver invokes Dafny with `--verify-included-files`, checking the generated file
and its included suppliers. A modular caller may verify while its supplier
fails; the experiment consequently grants no independent credit to that caller.
Only the strict whole-file result is compared.

Dafny/Boogie/Z3 verification is a different evidence path from Lean's kernel and
transitive axiom audit. This spike does not create a Dafny proof manifest or
claim that those two evidence paths are interchangeable.

## Reproduce the comparison

Build Aver with the usual local configuration, install the repository's Lean
toolchain and Dafny, then run:

```sh
python3 tools/dafny_guidance_spike.py --aver target/debug/aver --output out/dafny-spike-01
```

The output directory must be fresh. Each run uses zero error, sorry and declined
budgets. `matrix.json` records source and compiler hashes, tool versions, backend
summaries, source claim IDs and outcomes; each Lean row records its generated
toolchain pin and the actual Lean version, separately from ambient tool versions.
The individual logs and generated
projects stay beside it. Inputs are checked for changes between backend runs.
When both backends verify a file, the emitted law and step identities must match.
The coverage counter counts laws verified by both backends or only one, scoped
to these fixtures. It gives no independent credit to any law in a failing file.

The tables below retain the recorded checkpoints before the imports and division
extension. The current runner also exercises the newer controls; its fresh
`matrix.json` is authoritative for that run’s case and coverage counts.

At the thirteen-case checkpoint, twelve fixtures and the original K5 IntegerOrder
module produced these outcomes with Lean 4.33.1 and Dafny 4.11.0:

| Same source | Lean | Dafny |
|---|---|---|
| Guarded linear chain with a local citation | verified | verified |
| Missing guard, with passing examples | failed | failed |
| False intermediate reason | failed | failed |
| False universal goal restated as a reason | failed | failed |
| False cited helper and its modular caller | failed | failed |
| Recursive helper equation and nonpositive base, with a binding before the guard | verified | verified |
| False recursive Boolean reason, with a true final goal | failed | failed |
| Polynomial distributivity | verified | verified |
| Nonnegative product, square, guarded monotonicity and nonzero cancellation | failed | verified |
| Multiplication without its sign guard | failed | failed |
| Cancellation allowing a zero factor | failed | failed |
| False nonlinear citation and its modular caller | failed | failed |
| Original K5 IntegerOrder, including its recursive reason | verified | verified |

Coverage in this selected source set is nine laws checked by both backends and
four checked only by Dafny. The K5 row contributes four laws and eight steps
without changing its Aver source. This is a measured automation difference on
these statements, not a general ranking of the backends. The nonlinear positive
row contains true statements that Lean does not currently close with these
reasons; `failed` records incomplete proof evidence, not a counterexample.

The negative recursive control explicitly exercises Dafny induction. Its final
claim is tautological, so failure must come from checking the false intermediate
reason. All negative files retain zero error/sorry/declined budgets, and failed
supplier files grant no independent credit to their modular callers.

Timeouts, checker errors, refusals and partial evidence remain distinct results;
they cannot satisfy an expected failed-proof control.

Dafny already uses nonlinear arithmetic through Z3; this extension removes the
pilot rejection of variable products rather than enabling a new solver flag.
For more complex arguments, source lemmas and intermediate steps still matter.
The current extension admits imported citations and types and exact integer
division. Refinements, broader recursive proof patterns and solver completion
remain boundaries before K5 rounding can use this backend.

## Structured source controls

The next recorded checkpoint added six structured fixtures: three positive files
containing seven laws, and three negative files. All seven laws and fourteen
source steps passed both backends; all three negative files failed their strict
whole-file gates. Two positive laws are complete,
unchanged declarations from BTC `StackItem`, with their complete local function
dependencies; see the [source provenance](../tests/fixtures/dafny_guidance_structured/README.md).
The remaining positives exercise local sum types, list payloads, Result patterns
and local citations. Negative controls remove a necessary guard, insert a false
recursive reason despite a true final goal, and cite a false structured law.

These slices measure individual source portability. They do not imply that the
whole BTC StackItem module passes. At that checkpoint, other unsupported laws
and dependencies prevented whole-file proof credit.

At the nineteen-case checkpoint, sixteen laws passed both backends and four
passed only Dafny. All eleven negative files failed both backends. These counts
cover those selected fixtures and K5 IntegerOrder, not the full K5/BTC projects
or the later imports and division extension.

## Imports and division controls

The twenty-four-case comparison adds eleven laws and twenty-two obligations
from three positive source graphs. All eleven laws pass both backends. Two
additional negative files fail actual proof checking in both backends: one cites
a false imported supplier, and one loses a required recursive guard. Across the
complete comparison, **27 laws pass both backends and four pass only Dafny**;
all thirteen negative files fail both. These are fixture counts, not whole-project
K5 or BTC coverage. See the [control descriptions](../tests/fixtures/dafny_guidance_import_div/README.md).

The runner fingerprints imported Aver files as well as their entry points. An
edited, added or removed supplier invalidates that run’s evidence.

## Whole-project BTC measurement of the extension

The final measurement on BTC commit
`a6870c9de7280593d3e5a5928ceb62610a2ba316` still confirms only **7 of 104 own BTC
laws** under the strict whole-module gate. Those seven belong to `ScriptMath`.
This is a lower bound, not a count of all individually provable laws.

`CompactSize`, `Message`, `ScriptState` and `StackItem` now have **zero guidance
declines**. This records admission of their guided source; it does not mean
those modules passed verification. Open obligations remain, including solver
timeouts, four axiom fallbacks in `ScriptState`, and ten omitted obligations in
`StackItem`. `ScriptParse` and its importer `Chainwork` each report sixteen
declines, chiefly through mutually recursive parser/filter functions with growing
list accumulators. Those functions still use fuel-backed emission; accepting
their guidance as universal would require first establishing native termination.

The module runs had no outer wall-clock timeouts, but Dafny reported six solver
timeouts across the runs. Whole-module diagnostics include imported dependencies,
so their counts must not be summed as distinct source laws. The comparison and
BTC measurement used the same compiler SHA-256:
`952c21ed33868eef4eb4e2c19eee9c7a1d6a09153ef2ef45862ca41be70b3473`.
