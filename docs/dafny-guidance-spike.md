# Guided proof portability: the Dafny spike

This experiment sends identical Aver files through Lean and Dafny. It covers a
restricted integer/Boolean fragment of `because` and `using`; it does not
port K5's rounding proofs to Dafny.

## Supported fragment

Each law needs an explicit `using` list (`using []` selects no helpers), plain
`Int`/`Bool` givens, and a pure dependency cone in its own module.
Arithmetic includes addition, subtraction, comparisons and multiplication of
arbitrary integers. Boolean matches and selected scalar builtins are supported.
Selected local laws must satisfy the same restrictions.

Self-recursive helpers are admitted when the existing recursion classifier
recognizes a guarded integer countdown that subtracts a positive literal. The
backend validates the entire recursive body, including expressions after a
self-call. Dafny checks the function's termination. A step whose direct Boolean
predicate call is driven by an integer given can request induction on that given,
with a nonnegative decreases measure. Integer equations use ordinary unfolding
without an unnecessary induction hint. Neither strategy inserts an assumption.

Automatic selection, imported citations/functions, mutual recursion, unsupported
countdowns, collections, records, refinements and division are explicitly
declined. A supported-looking caller that selects an unsupported helper is
declined as well. Admitting a recursive expression does not guarantee that Dafny
can prove it; more complex induction arguments may still fail verification.

## What the backend checks

Every `because` becomes a separate lemma with the original `when` guard and all
earlier reasons as preconditions. The current reason is its conclusion, never
its own premise. A separate lemma checks the final implication. The parent law
calls every step in order, so an unproved intermediate reason cannot disappear
because the final claim happens to be true.

Explicit citations become checked calls under the cited law's own guard. They
provide no unconditional assumption or axiom fallback. Dafny verifies the entire
generated file, including every cited supplier. A modular caller may verify
while its supplier fails; the experiment consequently grants no independent
credit to that caller. Only the strict whole-file result is compared.

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

The thirteen source cases (twelve fixtures and the original K5 IntegerOrder module)
produced these outcomes with Lean 4.33.1 and Dafny 4.11.0:

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
Imported citations, exact division/result handling, records and broader induction
remain the next portability boundaries before K5 rounding can use this backend.
