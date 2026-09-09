# Guided proof portability: the Dafny spike

This experiment sends identical Aver files through Lean and Dafny. It covers a
checked pure fragment of `because` and `using`; it does not
port K5's rounding proofs to Dafny.

## Supported fragment

Guided laws need an explicit `using` list (`using []` selects no helpers).
Their pure dependency cones may cross explicit module imports. Givens may
contain `Int`, `Bool`, `String`, `Unit`, lists, vectors, maps, tuples, `Option`,
`Result`, checked refinements, and local or imported records and sum types. Every named field type, constructor, pattern,
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

Mutually recursive functions are admitted when the backend actually emits their
complete cycle as native functions with checked termination. The common call-edge
analysis chooses a sum of sequence lengths and the matching rank for each member.
A growing accumulator can be excluded while the input decreases. List tails count
as shorter; list heads do not, and `take`/`drop` alone imply only a non-growing
length. The checker validates every body and every member of the cycle. Admission
does not add a general mutual induction tactic or grant native status to callers
of a native function.

Explicit `using` citations may select local or visible imported laws. A selected
ordinary law without `because` or `using` is also supported through a separately
checked universal restatement, described below. Automatic citation selection,
fuel or opaque recursion fallbacks, unsupported countdowns,
arbitrary function-valued givens, effectful calls, provider resources and `Float`
remain outside this guidance fragment. Named pure callbacks are checked through
their entire source cones, including hidden recursion edges. A supported-looking caller that selects
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
division. Broader recursive proof patterns and solver completion remain
boundaries before K5 rounding can use this backend.

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

The twenty-four-case checkpoint added eleven laws and twenty-two obligations
from three positive source graphs. All eleven laws passed both backends. Two
additional negative files failed actual proof checking in both backends: one cites
a false imported supplier, and one loses a required recursive guard. Across the
comparison at that checkpoint, **27 laws passed both backends and four passed only Dafny**;
all thirteen negative files failed both. These are fixture counts, not whole-project
K5 or BTC coverage. See the [control descriptions](../tests/fixtures/dafny_guidance_import_div/README.md).

The runner fingerprints imported Aver files as well as their entry points. An
edited, added or removed supplier invalidates that run’s evidence.

## Whole-project BTC checkpoint before native mutual admission

The imports/division checkpoint on BTC commit
`a6870c9de7280593d3e5a5928ceb62610a2ba316` still confirms only **7 of 104 own BTC
laws** under the strict whole-module gate. Those seven belong to `ScriptMath`.
This is a lower bound, not a count of all individually provable laws.

`CompactSize`, `Message`, `ScriptState` and `StackItem` now have **zero guidance
declines**. This records admission of their guided source; it does not mean
those modules passed verification. Open obligations remain, including solver
timeouts, four axiom fallbacks in `ScriptState`, and ten omitted obligations in
`StackItem`. `ScriptParse` and its importer `Chainwork` each reported sixteen
declines, chiefly through mutually recursive parser/filter functions with growing
list accumulators. Those functions still used fuel-backed emission at that checkpoint.

The module runs had no outer wall-clock timeouts, but Dafny reported six solver
timeouts across the runs. Whole-module diagnostics include imported dependencies,
so their counts must not be summed as distinct source laws. The comparison and
BTC measurement used the same compiler SHA-256:
`952c21ed33868eef4eb4e2c19eee9c7a1d6a09153ef2ef45862ca41be70b3473`.

## Native mutual recursion checkpoint

The twenty-seven-case comparison adds four one-step laws and eight obligations
from local and imported mutual processing functions. Both backends verify those
source transitions. A false intermediate reason fails actual proof checking in
both; a separate nonshrinking cycle stays declined and is never VM-executed.
Across the comparison, **31 laws pass both backends and four pass only Dafny**.
All fourteen negative files fail both. These controls establish termination and
source transitions, not a general mutual induction tactic. See the
[mutual controls](../tests/fixtures/dafny_guidance_mutual/README.md).

On the same unchanged BTC commit, the seven parser/filter functions `from`,
`nextOp`, `pushOf`, `taken`, `wide`, `keptUnlessMatched` and `without` now emit
natively. A filtered Dafny check of those functions verifies **34 assertions
with zero errors**. `ScriptParse` and `Chainwork` guidance declines fall from
sixteen to nine: eight identify string interpolation in a parser error message,
and one identifies Result propagation (`?`) in `withoutPushes`. Seven filter
laws now reach proof checking.

Whole BTC remains **7/104 own laws** under the strict whole-module gate; native
termination and admission do not close all mathematical obligations. The final
runs retain four axiom fallbacks in `ScriptState`, ten omissions in `StackItem`,
and six reported solver timeouts across repeated module dependencies. No outer
wall-clock timeout occurred. The comparison and BTC runs use compiler SHA-256
`f30a0974ac3d5e38bed72fad36337264f26031b3edd256a0f5d1a58639396830`.

## Pure source structure checkpoint

The structural suite contains **52 positive laws**: 45 guided laws and seven
ordinary laws. All pass the strict Dafny gate, including imported definitions.
Eight negative files contain ten laws with false intermediate steps or invalid
subset constructors/updates; their samples pass but actual proof checking fails.
See the [fixture inventory](../tests/fixtures/dafny_structure/README.md).

Pure `?` and `?!` normalize throughout expression trees, preserving error exits,
eager operand order and lazy branch selection. Unit, vectors, extensional maps,
Unit-valued maps, named pure callbacks, exact primitive interpolation and defined
text operations are admitted. Checked refinements retain their source predicate;
Bytes encoders return the refined type and prove equality to their arithmetic
recurrence. Structural subset helpers prove their concat/take/drop equations.
No return constraint is erased to make an encoder or record update pass.

The source typechecker now types ordinary law templates in their declared given
environment as well as their expanded samples. A dynamic operation in a template
keeps its Result type; literal discharge in a separate sample stays independent.
Proof rewrites preserve these types rather than making a renderer guess them.

The expanded **32-case** comparison checks **31 laws in both backends and 12 only
in Dafny**. The eight new Dafny-only laws concern primitive display and named
callbacks; Lean's existing attempts remain incomplete on the identical source.
All sixteen negative files fail actual proof checking in both backends.

This closes expression-translation gaps, not every semantic model or recursive
proof strategy. Exact IEEE Float, Unicode case conversion, UTF-8 and parsing,
sorted map iteration and arbitrary callback givens remain separate boundaries.
Unclassified effects still require contracts. Full historical K5 rounding is not
established by these structural fixtures.

On unchanged BTC commit `a6870c9de7280593d3e5a5928ceb62610a2ba316`,
`ScriptParse` guidance declines fall from nine to **one**. Its remaining refusal
is `String.toLower` reached through `withoutPushes`; propagation and interpolation
now translate. The same single refusal appears in the generated Chainwork import
graph. Fuel rewrites preserve record-field display types in `Transaction` too.

Strict whole-module coverage remains **7/104 own laws**. The primary 60-second
measurement records a Chainwork wall-clock timeout, plus solver timeouts and
open obligations elsewhere; it does not grant per-law credit from failing files.
ScriptState still has four axiom fallbacks, and StackItem ten omissions. The
32-case comparison and primary BTC run use compiler SHA-256
`d9cd78491e597b7743ef75f8da3f0dd4e1ed69a1c064922180ef634a91bd8ccb`.

An isolated Chainwork diagnostic with a 180-second outer limit finishes in
51.99 seconds: one guidance decline, 56 errors, two solver timeouts, and no
axioms or omissions. This diagnostic does not replace the primary measurement
or establish any additional whole-module law credit.

## Next phase: BTC and shared ProofIR

After this PR, use unchanged BTC source to drive the remaining Dafny work.
Prefer putting shared semantic facts and proof analysis in ProofIR and its
lowering passes, with both backends consuming the same canonical identities,
refinement predicates, recursion contracts and law obligations. These already
have shared representations; extend those where a new fact is backend-neutral
instead of introducing a second recognizer in a renderer.

Keep target syntax, automation and target-specific capability checks in their
backends. The current Dafny propagation normalizer is still backend-local;
this checkpoint does not claim the two renderers share every transformation.
Move a transformation into the common pipeline when both consumers can retain
the same source semantics, and check the change with the same-source positive
and negative controls. Do not require a broad ProofIR refactor to merge this
validated structural checkpoint.

## Fixed-count unfolding checkpoint

The unchanged BTC `Message` module now passes all ten laws with its imports
checked, including the 2/4/8-byte read-after-write laws. Strict whole-module
BTC coverage rises from **7/104 to 17/104** (`Message` plus `ScriptMath`).

ProofIR carries an optional unfolding budget for each `because` step and the
final claim. It follows small literal arguments through direct forwarding
wrappers to a checked integer countdown, and records the recursive function
IDs and concrete sequence-reversal instances in that obligation's source cone.
The initial strategy accepts Int/Bool givens and countdown literals up to 16;
arbitrary sequence givens retain citation/induction search. Sample domains and
range premises do not determine the budget or restrict universal quantifiers.

Dafny renders these hints as local fuel attributes. It avoids introducing broad
quantified citation facts into the same unfolding context, while still checking
every cited supplier and every intermediate step. The original premises and
claim remain unchanged. Lean retains its existing automation; the new ProofIR
field is a search hint rather than a new semantic assumption.

Regression controls use decimal and hexadecimal digits, widths 3 and 5,
imported functions beside colliding local names, and Bool sequence reversal.
Missing bounds, false intermediate steps and false unused suppliers all fail
actual Dafny checking. The remaining BTC modules still have open proofs or
unsupported operations; this checkpoint does not establish full BTC or K5.

## Sequence composition checkpoint

All six `CompactSize` laws now pass with imports checked, bringing strict
whole-module BTC coverage to **23/104**. No BTC source or claim changed.

Dafny's checked sequence facts now also expose
`([head] + prefix) + suffix == [head] + (prefix + suffix)`. This lets a parser's
head/tail match connect to an already proved payload law at the complete suffix.
The fact is proved for each concrete element type; it neither assumes a cited
law nor changes an obligation. This is a Dafny automation hint over existing
list semantics, so it needs no new source recognizer or ProofIR contract.

An independent decimal framing fixture reproduces two open obligations before
the change and passes afterwards. Bool and record payloads also check. Incorrect
reordering of a trailing sequence passes the fixture's empty/singleton samples
but fails universal verification, both as an intermediate reason and as the
final claim. A false cited field law is rejected as well.

## Native mutual sequence laws and shared dependencies

ProofIR now records each law's transitive statically resolved pure function
cone, including explanations and the guard. Each function body is resolved in
its owning module; cycles terminate on canonical function IDs, and sample
expressions do not supply dependencies. Lean's guided equation selection and
Dafny's ordinary-law unfolding both consume this field. Target equation
eligibility, termination checks and solver tactics remain backend decisions.

Dafny previously sent even native, termination-checked mutual recursion through
the finite-sample fallback. Ordinary laws with a list binder now attempt their
actual universal claim when the cone reaches native mutual recursion and no
opaque function. Unfolding covers every cycle member, including helpers hidden
behind wrappers. The moderate fuel setting is a search budget, not a limit on
list length or quantified values. A checked assertion of the unchanged claim
also exposes recursive Bool equalities that can remain opaque in an `ensures`
alone. Unsupported signatures retain their existing
exclusions; the legacy finite-Int lane remains separate.

Sample assertions instantiate the checked universal theorem. That theorem does
not call the samples, so a false universal cannot be rescued by a sample cycle.
An independent permutation fixture checks Int and Bool elements and imported
lookup functions alongside colliding local names. Incorrect order and omitted
length/index guards pass the chosen samples but fail actual universal checking.

All four unchanged BTC `ScriptState.rearranged` laws now check without their
previous axiom fallbacks; their samples check too. Imported modules still have
open obligations, so these four do not yet earn strict whole-module BTC credit.
This checkpoint does not claim full Lean/Dafny strategy parity: the existing
Lean ordinary-law lane still bounds one of the independent fixture's two laws,
while Dafny proves both universally.

## Native floor-division law checkpoint

A checked floor-division recursion no longer makes every ordinary law in its
cone sample-only. Dafny retains the specialized division-window strategy when
available and otherwise checks the actual universal obligation. Failure to
find a proof remains a failed check; native termination alone is not law credit.

Shared source induction now accepts fixed literal arguments in the claim,
such as the empty seed in `digits(value, [])`. It keeps that seed in the
statement and recurses on the quantified inputs using the source call's actual
quotient. A fully quantified anchor still takes priority when one is available.
Sample values neither choose the seed nor restrict the theorem's quantifiers.

ProofIR also carries the theorem premise instantiated at each recursive call.
Dafny introduces any list-pattern projections before testing that premise; an
IH is available only in the branch where its own premise holds. The original
claim must still be proved in all other branches. Lean continues to consume
the same source-induction plan with its own functional-induction tactics.

The Dafny sequence-reversal helper now proves preservation of membership in
addition to length. The quantified element ranges over the input/output union,
so the contract also supports generic elements containing references. An
explicit, checked cons decomposition proves the contract from the recursive
implementation; no sequence property is assumed.

An independent decimal collector has seven universal laws checked by both
backends; the Dafny controls also use radix seven. Missing guards and an
incorrect prefix order pass their supplied VM samples but fail universal
checking. A separate list-accumulator control checks the scope of recursive
premises in Dafny, which also rejects its false variant. Negative digit laws are
isolated from unrelated true sibling citations to avoid turning a concrete
counterexample into a quantifier-search timeout.

Nine of the ten previously omitted ordinary `StackItem.littleEndian` laws
now check on unchanged BTC source. The readback law and several separate
citation obligations remain open, so this does not establish the whole module.


## Ordinary citation reuse checkpoint

A guided citation of an ordinary source law keeps its checked universal
restatement, including every given and its `when` premise. When the ordinary
emitter supplies the same universal signature, that restatement now calls the
original lemma. It no longer has to reproduce a proof that relied on earlier
ordinary decomposition laws. Both the original lemma and its caller remain
obligations in the full checker run.

Availability uses the canonical function cone already carried by ProofIR,
the emitted recursion boundaries, and the existing signature gate. Finite-Int
mutual laws, opaque dependencies, omitted laws and specialized signatures do
not gain universal credit from this reuse. Their citation restatements still
need independent universal proofs. Ordinary native sequence universals may be
reused. The backend-specific call decision does not change Lean's proof tactics.

The ordinary decomposition pool now comes from the declaring module's source
order, even for imported suppliers. It contains only earlier ordinary laws;
entry-module laws and guided consumers cannot introduce a dependency cycle.
Owner qualification also keeps an imported lemma distinct from a same-named
local law.

The independent decimal fixture has nine laws, checked universally from the
same source by Lean and Dafny. Before reuse its two guided citation suppliers
failed Dafny verification although all seven ordinary laws passed. Controls
cover forward citations, an omitted premise, a false unused supplier whose
samples pass, and an imported false supplier alongside colliding local names. A finite-Int
law over native mutual lookup remains bounded: its samples pass but its false
universal citation fails. An ordinary native sequence permutation, which does
have a universal contract, can be reused successfully.

On unchanged BTC, `StackItem` drops from 15 errors to 6, with four solver
timeouts still open. `ScriptState` including its imports drops from 22 errors
to 7; its solver timeouts change from two to four. Strict whole-module credit
remains 23/104 (`Message` 10, `CompactSize` 6, `ScriptMath` 7). Fewer duplicate
citation failures do not by themselves establish additional whole modules.
