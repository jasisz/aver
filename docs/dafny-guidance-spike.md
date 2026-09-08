# Guided proof portability: the Dafny spike

This experiment sends identical Aver files through Lean and Dafny. It covers a
small nonrecursive integer/Boolean fragment of `because` and `using`; it does not
port K5's rounding proofs to Dafny.

## Supported fragment

Each law needs an explicit `using` list (`using []` selects no helpers), plain
`Int`/`Bool` givens, and a pure, nonrecursive dependency cone in its own module.
Arithmetic is linear: addition, subtraction, comparisons and multiplication by
integer literals. Boolean matches and selected scalar builtins are supported.
Selected local laws must satisfy the same restrictions.

Automatic selection, imported citations/functions, recursive helpers, collections,
records, refinements, division and variable-by-variable multiplication are
explicitly declined. A supported-looking caller that selects an unsupported
helper is declined as well.

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
summaries, source claim IDs and outcomes; the individual logs and generated
projects stay beside it. Inputs are checked for changes between backend runs.
When both backends verify a file, the emitted law and step identities must match.
The coverage counter counts laws verified by both backends or only one, scoped
to these fixtures. It gives no independent credit to any law in a failing file.

The six fixtures in `tests/fixtures/dafny_guidance_spike/` produced these outcomes
with Lean 4.32.0 and Dafny 4.11.0:

| Same source | Lean | Dafny |
|---|---|---|
| Guarded linear chain with a local citation | verified | verified |
| Missing guard, with passing examples | failed | failed |
| False intermediate reason | failed | failed |
| False universal goal restated as a reason | failed | failed |
| False cited helper and its modular caller | failed | failed |
| Recursive helper equation | verified | declined |

The positive file contains two laws and six separately checked steps, with
matching source identities on both backends. Coverage in this fixture set is
two laws checked by both backends and one checked only by Lean.

Timeouts, checker errors, refusals and partial evidence remain distinct results;
they cannot satisfy an expected failed-proof control. The recursive row measures
the supported backend boundary, not a mathematical counterexample.

The next expansion needs further unchanged-source controls. K5's integer-order
helpers already use recursion, and its floor-division arguments add division and
nonlinear products; neither follows automatically from this linear pilot.
