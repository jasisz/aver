# Guided proof portability: the Dafny spike

This experiment runs identical Aver files through Lean and Dafny. It covers a
checked pure fragment of `because` and `using`. It does not port K5's rounding
proofs to Dafny.

## Supported fragment

Guided laws need an explicit `using` list (`using []` selects no helpers). Their
pure dependency cones may cross explicit module imports. Givens may contain
`Int`, `Bool`, `String`, `Unit`, lists, vectors, maps, tuples, `Option`,
`Result`, checked refinements, and local or imported records and sum types.
Every named field type, constructor, pattern, called body and selected law is
checked against the same restrictions. Signatures, bodies and field annotations
resolve in their declaring modules, so same-named functions or types in a caller
do not replace imported declarations.

Arithmetic covers addition, subtraction, comparisons, multiplication of
arbitrary integers, and `Int.div` / `Int.mod`. Division keeps Aver’s exact
Euclidean semantics, including for negative operands. A syntactically nonzero
literal divisor returns `Int`, by the same discharge rule Aver’s type checker
uses. A zero or dynamic divisor keeps `Result<Int, String>`: zero produces
`Result.Err("division by zero")`, and a nonzero value produces `Result.Ok` of
the quotient or remainder. For a nonzero divisor `d`, the quotient `q` and
remainder `r` satisfy `a = d * q + r` and `0 <= r < Int.abs(d)`. None of this
adds an integer `/` operator or erases an error branch.

Selected first-order list operations use native sequences or total defined
helpers. Other unsupported operations are still declined, even inside an unused
binding or a branch the supplied examples never take.

Self-recursive helpers are admitted when the shared classifier recognizes one of
two shapes: a guarded integer countdown that subtracts a positive literal, or a
checked quotient countdown by a fixed literal divisor of at least two. The
quotient contract must show that the source argument is positive at every
recursive call and strictly shrinks. A division expression on its own is not
such a contract. The backend validates the whole recursive body, and Dafny
checks its termination. Boolean reason predicates can generalize the law’s
givens, including growing accumulators, while only the selected integer's
nonnegative measure decreases. An explicit recursive step call instantiates a
checked quotient induction when its source arguments can be translated safely.
Integer equations use ordinary unfolding, without an unneeded induction hint.

Native structural list descent is admitted too. Boolean list reasons can
generalize all givens while only the list length decreases, so a fold can change
its accumulators. For a simple source match with a single recursive call, the
backend instantiates the step lemma at that call's arguments. Dafny must prove
its guard, the earlier reasons and termination. No premise is invented.

Mutually recursive functions are admitted when the backend actually emits their
whole cycle as native functions with checked termination. The common call-edge
analysis picks a sum of sequence lengths and the matching rank for each member.
A growing accumulator can be left out while the input decreases. List tails
count as shorter. List heads do not, and `take`/`drop` alone only imply a length
that does not grow. The checker validates every body and every member of the
cycle. Admission does not add a general mutual induction tactic, and callers of
a native function do not become native themselves.

Explicit `using` citations may select local or visible imported laws. Selecting
an ordinary law without `because` or `using` also works, through a separately
checked universal restatement described below. These stay outside the guidance
fragment: automatic citation selection, fuel or opaque recursion fallbacks,
unsupported countdowns, arbitrary function-valued givens, effectful calls,
provider resources and `Float`. Named pure callbacks are checked through their
whole source cones, including hidden recursion edges. A caller that looks
supported but selects an unsupported helper is declined as well. Admission does
not guarantee that Dafny can finish the proof.

## What the backend checks

Every `because` becomes a separate lemma whose preconditions are the original
`when` guard and all earlier reasons. The current reason is its conclusion and
is never its own premise. A separate lemma checks the final implication. The
parent law calls every step in order, so an unproved intermediate reason cannot
drop out just because the final claim happens to be true.

Explicit citations become checked calls under the cited law's own guard.
Imported statements and their binder types keep their declaring-module
identities when rendered in the caller. Dependency modules import reachable
transitive owners without opening their names. A supplier's statement can
therefore refer to its own dependencies without making unrelated bare names
visible.

For a selected ordinary law, the backend emits a separate universal lemma in the
consumer, with the original guard and an assertion of the original claim. It
proves this statement again. A legacy lemma or a finite sample check does not
count as universal evidence. Guided suppliers are called through their checked
parent lemmas. Neither path adds an unconditional assumption or an axiom
fallback.

Aver runs Dafny with `--verify-included-files`, which checks the generated file
and the suppliers it includes. A modular caller can verify while its supplier
fails, so the experiment gives that caller no credit of its own. Only the strict
whole-file result is compared.

Dafny/Boogie/Z3 verification is a separate kind of evidence from Lean's kernel
and transitive axiom audit. This spike does not create a Dafny proof manifest,
and it does not claim the two kinds of evidence can stand in for each other.

## Reproduce the comparison

Build Aver with the usual local configuration, install the repository's Lean
toolchain and Dafny, then run:

```sh
python3 tools/dafny_guidance_spike.py --aver target/debug/aver --output out/dafny-spike-01
```

The output directory must be fresh. Each run uses zero error, sorry and declined
budgets. `matrix.json` records source and compiler hashes, tool versions,
backend summaries, source claim IDs and outcomes. Each Lean row records its
generated toolchain pin and the Lean version actually used, separately from the
ambient tool versions. The individual logs and generated projects are kept next
to it. Inputs are checked for changes between backend runs. When both backends
verify a file, the emitted law and step identities must match. The coverage
counter counts laws verified by both backends or by only one, within these
fixtures. It gives no credit of its own to any law in a failing file.

The tables below keep the checkpoints recorded before the imports and division
extension. The current runner also runs the newer controls, and its fresh
`matrix.json` is the authority for that run's case and coverage counts.

At the thirteen-case checkpoint, twelve fixtures and the original K5
IntegerOrder module gave these outcomes with Lean 4.33.1 and Dafny 4.11.0:

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

In this selected source set, nine laws are checked by both backends and four
only by Dafny. The K5 row contributes four laws and eight steps with its Aver
source unchanged. This measures an automation difference on these statements
only; it does not rank the backends in general. The nonlinear positive row holds
true statements that Lean does not currently close with these reasons. There
`failed` means the proof evidence is incomplete. It does not mean a
counterexample was found.

The negative recursive control exercises Dafny induction on purpose. Its final
claim is a tautology, so a failure has to come from checking the false
intermediate reason. All negative files keep zero error/sorry/declined budgets,
and a failed supplier file gives its modular callers no credit of their own.

Timeouts, checker errors, refusals and partial evidence are kept as distinct
results. None of them can satisfy an expected failed-proof control.

Dafny already uses nonlinear arithmetic through Z3. This extension removes the
pilot's rejection of variable products; it does not turn on a new solver flag.
For more complex arguments, source lemmas and intermediate steps still matter.
The current extension admits imported citations and types and exact integer
division. Broader recursive proof patterns and solver completion are still open
before K5 rounding can use this backend.

## Structured source controls

The next recorded checkpoint added six structured fixtures: three positive files
with seven laws between them, and three negative files. All seven laws and
fourteen source steps passed both backends, and all three negative files failed
their strict whole-file gates. Two of the positive laws are complete, unchanged
declarations from BTC `StackItem`, together with their complete local function
dependencies; see the
[source provenance](../tests/fixtures/dafny_guidance_structured/README.md). The
other positives exercise local sum types, list payloads, Result patterns and
local citations. The negative controls remove a necessary guard, insert a false
recursive reason alongside a true final goal, and cite a false structured law.

These slices measure how well individual pieces of source port. They do not mean
the whole BTC StackItem module passes. At that checkpoint, other unsupported
laws and dependencies prevented whole-file proof credit.

At the nineteen-case checkpoint, sixteen laws passed both backends and four
passed only Dafny. All eleven negative files failed both backends. These counts
cover those selected fixtures and K5 IntegerOrder. They do not cover the full
K5/BTC projects or the later imports and division extension.

## Imports and division controls

The twenty-four-case checkpoint added eleven laws and twenty-two obligations
from three positive source graphs. All eleven laws passed both backends. Two
more negative files failed actual proof checking in both backends: one cites a
false imported supplier, and one loses a required recursive guard. Across the
comparison at that checkpoint, **27 laws passed both backends and four passed
only Dafny**, and all thirteen negative files failed both. These are fixture
counts. They say nothing about whole-project K5 or BTC coverage. See the
[control descriptions](../tests/fixtures/dafny_guidance_import_div/README.md).

The runner fingerprints imported Aver files as well as their entry points.
Editing, adding or removing a supplier invalidates that run's evidence.

## Whole-project BTC checkpoint before native mutual admission

The imports/division checkpoint on BTC commit
`a6870c9de7280593d3e5a5928ceb62610a2ba316` still confirms only **7 of 104 own
BTC laws** under the strict whole-module gate. All seven belong to `ScriptMath`.
This is a lower bound. It is not a count of every law that can be proved
individually.

`CompactSize`, `Message`, `ScriptState` and `StackItem` now have **zero guidance
declines**. That records that their guided source is admitted. It does not mean
those modules passed verification. Open obligations remain, including solver
timeouts, four axiom fallbacks in `ScriptState`, and ten omitted obligations in
`StackItem`. `ScriptParse` and its importer `Chainwork` each reported sixteen
declines, mostly from mutually recursive parser/filter functions with growing
list accumulators. At that checkpoint those functions still used fuel-backed
emission.

The module runs had no outer wall-clock timeouts, but Dafny reported six solver
timeouts across the runs. Whole-module diagnostics include imported
dependencies, so their counts must not be added up as distinct source laws. The
comparison and the BTC measurement used the same compiler SHA-256:
`952c21ed33868eef4eb4e2c19eee9c7a1d6a09153ef2ef45862ca41be70b3473`.

## Native mutual recursion checkpoint

The twenty-seven-case comparison adds four one-step laws and eight obligations
from local and imported mutual processing functions. Both backends verify those
source transitions. A false intermediate reason fails actual proof checking in
both. A separate cycle that does not shrink stays declined and is never run on
the VM. Across the comparison, **31 laws pass both backends and four pass only
Dafny**. All fourteen negative files fail both. These controls establish
termination and source transitions. They do not provide a general mutual
induction tactic. See the
[mutual controls](../tests/fixtures/dafny_guidance_mutual/README.md).

On the same unchanged BTC commit, the seven parser/filter functions `from`,
`nextOp`, `pushOf`, `taken`, `wide`, `keptUnlessMatched` and `without` are now
emitted natively. A filtered Dafny check of those functions verifies **34
assertions with zero errors**. Guidance declines in `ScriptParse` and
`Chainwork` drop from sixteen to nine. Eight point at string interpolation in a
parser error message, and one at Result propagation (`?`) in `withoutPushes`.
Seven filter laws now reach proof checking.

Whole BTC is still at **7/104 own laws** under the strict whole-module gate.
Native termination and admission do not close all the mathematical obligations.
The final runs still have four axiom fallbacks in `ScriptState`, ten omissions
in `StackItem`, and six reported solver timeouts across repeated module
dependencies. No outer wall-clock timeout occurred. The comparison and BTC runs
use compiler SHA-256
`f30a0974ac3d5e38bed72fad36337264f26031b3edd256a0f5d1a58639396830`.

## Pure source structure checkpoint

The structural suite has **52 positive laws**: 45 guided laws and seven ordinary
laws. All of them pass the strict Dafny gate, imported definitions included.
Eight negative files hold ten laws with false intermediate steps or invalid
subset constructors/updates. Their samples pass, but actual proof checking
fails. See the [fixture inventory](../tests/fixtures/dafny_structure/README.md).

Pure `?` and `?!` are normalized throughout expression trees. Error exits, eager
operand order and lazy branch selection are preserved. Unit, vectors,
extensional maps, Unit-valued maps, named pure callbacks, exact primitive
interpolation and defined text operations are admitted. Checked refinements keep
their source predicate. Bytes encoders return the refined type and prove that it
equals their arithmetic recurrence. Structural subset helpers prove their
concat/take/drop equations. No return constraint is erased to make an encoder or
record update pass.

The source typechecker now types ordinary law templates in their declared given
environment, as well as their expanded samples. A dynamic operation in a
template keeps its Result type, and literal discharge in a separate sample stays
independent of it. Proof rewrites preserve these types, so a renderer does not
have to guess them.

The expanded **32-case** comparison checks **31 laws in both backends and 12
only in Dafny**. The eight new Dafny-only laws are about primitive display and
named callbacks. Lean's existing attempts on the identical source are still
incomplete. All sixteen negative files fail actual proof checking in both
backends.

This closes gaps in expression translation. It does not supply every semantic
model or recursive proof strategy. Exact IEEE Float, Unicode case conversion,
UTF-8 and parsing, sorted map iteration and arbitrary callback givens are still
separate limits. Unclassified effects still need contracts. These structural
fixtures do not establish the full historical K5 rounding.

On unchanged BTC commit `a6870c9de7280593d3e5a5928ceb62610a2ba316`, guidance
declines in `ScriptParse` drop from nine to **one**. The one left is
`String.toLower`, reached through `withoutPushes`; propagation and interpolation
now translate. The same single refusal shows up in the generated Chainwork
import graph. Fuel rewrites also preserve record-field display types in
`Transaction`.

Strict whole-module coverage is still **7/104 own laws**. The primary 60-second
measurement records a Chainwork wall-clock timeout, plus solver timeouts and
open obligations elsewhere. It gives no per-law credit from failing files.
ScriptState still has four axiom fallbacks, and StackItem ten omissions. The
32-case comparison and the primary BTC run use compiler SHA-256
`d9cd78491e597b7743ef75f8da3f0dd4e1ed69a1c064922180ef634a91bd8ccb`.

An isolated Chainwork diagnostic with a 180-second outer limit finishes in 51.99
seconds: one guidance decline, 56 errors, two solver timeouts, and no axioms or
omissions. This diagnostic does not replace the primary measurement and adds no
whole-module law credit.

## Next phase: BTC and shared ProofIR

After this PR, let unchanged BTC source drive the remaining Dafny work. Prefer
to put shared semantic facts and proof analysis in ProofIR and its lowering
passes, so both backends consume the same canonical identities, refinement
predicates, recursion contracts and law obligations. These already have shared
representations. When a new fact is backend-neutral, extend those
representations instead of adding a second recognizer in a renderer.

Keep target syntax, automation and target-specific capability checks in their
backends. The current Dafny propagation normalizer is still local to the
backend, and this checkpoint does not claim the two renderers share every
transformation. Move a transformation into the common pipeline when both
consumers can keep the same source semantics, and check the change with the
same-source positive and negative controls. Merging this validated structural
checkpoint should not wait on a broad ProofIR refactor.

## Fixed-count unfolding checkpoint

Historical implementation note: the budget field described below was later
removed from ProofIR. The same bounded policy now lives in Dafny's emitter; see
[Proof arguments and search policy](dafny.md#proof-arguments-and-search-policy).
The measurements in this checkpoint are historical. They are not a new census.


The unchanged BTC `Message` module now passes all ten laws with its imports
checked, including the 2/4/8-byte read-after-write laws. Strict whole-module BTC
coverage goes from **7/104 to 17/104** (`Message` plus `ScriptMath`).

ProofIR carries an optional unfolding budget for each `because` step and for the
final claim. It follows small literal arguments through direct forwarding
wrappers to a checked integer countdown. It records the recursive function IDs
and concrete sequence-reversal instances in that obligation's source cone. The
first version of the strategy accepts Int/Bool givens and countdown literals up
to 16. Arbitrary sequence givens keep citation/induction search. Sample domains
and range premises neither set the budget nor restrict universal quantifiers.

Dafny renders these hints as local fuel attributes. It avoids putting broad
quantified citation facts into the same unfolding context, and it still checks
every cited supplier and every intermediate step. The original premises and
claim are unchanged. Lean keeps its existing automation. The new ProofIR field
is a search hint and adds no semantic assumption.

The regression controls use decimal and hexadecimal digits, widths 3 and 5,
imported functions next to colliding local names, and Bool sequence reversal.
Missing bounds, false intermediate steps and false unused suppliers all fail
actual Dafny checking. The other BTC modules still have open proofs or
unsupported operations, so this checkpoint does not establish full BTC or K5.

## Sequence composition checkpoint

All six `CompactSize` laws now pass with imports checked, which brings strict
whole-module BTC coverage to **23/104**. No BTC source or claim changed.

Dafny's checked sequence facts now also include
`([head] + prefix) + suffix == [head] + (prefix + suffix)`. With it, a parser's
head/tail match can connect to an already proved payload law at the complete
suffix. The fact is proved for each concrete element type. It does not assume a
cited law or change an obligation. It is a Dafny automation hint over existing
list semantics, so it needs no new source recognizer or ProofIR contract.

An independent decimal framing fixture reproduces two open obligations before
the change and passes after it. Bool and record payloads also check. Reordering
a trailing sequence incorrectly passes the fixture's empty/singleton samples but
fails universal verification, both as an intermediate reason and as the final
claim. A false cited field law is rejected as well.

## Native mutual sequence laws and shared dependencies

ProofIR now records each law's transitive, statically resolved pure function
cone, including explanations and the guard. Each function body is resolved in
its owning module. Cycles stop on canonical function IDs, and sample expressions
supply no dependencies. Lean's guided equation selection and Dafny's
ordinary-law unfolding both read this field. Which target equations are
eligible, termination checks and solver tactics are still decided per backend.

Dafny used to send even native, termination-checked mutual recursion through the
finite-sample fallback. Ordinary laws with a list binder now attempt their
actual universal claim when the cone reaches native mutual recursion and no
opaque function. Unfolding covers every cycle member, including helpers hidden
behind wrappers. The moderate fuel setting is a search budget. It does not limit
list length or quantified values. A checked assertion of the unchanged claim
also exposes recursive Bool equalities that can stay opaque in an `ensures`
alone. Unsupported signatures keep their existing exclusions, and the legacy
finite-Int lane stays separate.

Sample assertions instantiate the checked universal theorem. That theorem does
not call the samples, so a sample cycle cannot rescue a false universal. An
independent permutation fixture checks Int and Bool elements and imported lookup
functions next to colliding local names. Wrong order and missing length/index
guards pass the chosen samples but fail actual universal checking.

All four unchanged BTC `ScriptState.rearranged` laws now check without their old
axiom fallbacks, and their samples check too. Imported modules still have open
obligations, so these four do not yet count toward strict whole-module BTC
credit. This checkpoint does not claim full Lean/Dafny strategy parity. The
existing Lean ordinary-law lane still bounds one of the independent fixture's
two laws, while Dafny proves both universally.

## Native floor-division law checkpoint

A checked floor-division recursion no longer makes every ordinary law in its
cone sample-only. Dafny keeps the specialized division-window strategy where it
applies, and otherwise checks the actual universal obligation. If no proof is
found, the check fails. Native termination alone earns no law credit.

Shared source induction now accepts fixed literal arguments in the claim, such
as the empty seed in `digits(value, [])`. It keeps that seed in the statement
and recurses on the quantified inputs, using the source call's actual quotient.
A fully quantified anchor still wins when one is available. Sample values
neither choose the seed nor restrict the theorem's quantifiers.

ProofIR also carries the theorem premise instantiated at each recursive call.
Dafny introduces any list-pattern projections before it tests that premise. An
IH is available only in the branch where its own premise holds, and the original
claim must still be proved in all other branches. Lean keeps using the same
source-induction plan with its own functional-induction tactics.

The Dafny sequence-reversal helper now proves that membership is preserved, in
addition to length. The quantified element ranges over the union of input and
output, so the contract also supports generic elements that contain references.
An explicit, checked cons decomposition proves the contract from the recursive
implementation. No sequence property is assumed.

An independent decimal collector has seven universal laws checked by both
backends, and the Dafny controls also use radix seven. Missing guards and a
wrong prefix order pass their supplied VM samples but fail universal checking. A
separate list-accumulator control checks the scope of recursive premises in
Dafny, which also rejects its false variant. Negative digit laws are kept apart
from unrelated true sibling citations, so a concrete counterexample does not
turn into a quantifier-search timeout.

Nine of the ten ordinary `StackItem.littleEndian` laws that used to be omitted
now check on unchanged BTC source. The readback law and several separate
citation obligations are still open, so this does not establish the whole
module.


## Ordinary citation reuse checkpoint

A guided citation of an ordinary source law keeps its checked universal
restatement, with every given and its `when` premise. When the ordinary emitter
supplies the same universal signature, that restatement now calls the original
lemma. It no longer has to redo a proof that relied on earlier ordinary
decomposition laws. Both the original lemma and its caller stay obligations in
the full checker run.

Whether the lemma can be reused is decided from the canonical function cone
ProofIR already carries, the emitted recursion boundaries, and the existing
signature gate. Finite-Int mutual laws, opaque dependencies, omitted laws and
specialized signatures gain no universal credit from this reuse. Their citation
restatements still need their own universal proofs. Ordinary native sequence
universals may be reused. The backend-specific decision to call does not change
Lean's proof tactics.

The ordinary decomposition pool now follows the declaring module's source order,
even for imported suppliers. It contains only earlier ordinary laws, so
entry-module laws and guided consumers cannot introduce a dependency cycle.
Owner qualification also keeps an imported lemma separate from a local law with
the same name.

The independent decimal fixture has nine laws, checked universally from the same
source by Lean and Dafny. Before reuse, its two guided citation suppliers failed
Dafny verification although all seven ordinary laws passed. The controls cover
forward citations, an omitted premise, a false unused supplier whose samples
pass, and an imported false supplier next to colliding local names. A finite-Int
law over native mutual lookup stays bounded: its samples pass, but its false
universal citation fails. An ordinary native sequence permutation, which does
have a universal contract, can be reused successfully.

On unchanged BTC, `StackItem` goes from 15 errors to 6, with four solver
timeouts still open. `ScriptState` with its imports goes from 22 errors to 7,
and its solver timeouts go from two to four. Strict whole-module credit stays at
23/104 (`Message` 10, `CompactSize` 6, `ScriptMath` 7). Fewer duplicate citation
failures do not on their own establish more whole modules.
