# proof-corpus — external coverage corpus

A curated set of **formal proof tasks** (program + a `verify ... law` to prove)
used to measure how much of an INDEPENDENT, third-party body of inductive
problems Aver's proof engine can close. This is the honest-coverage / anti-overfit
counterpart to the hand-authored regression set.

## This is NOT a regression suite

The must-prove regression set lives in the compiler proper
(`examples/data`, `examples/formal` + `tests/proof_spec.rs` `assert_eq!`) and is
gated per-PR — every one of those MUST prove.

Here it is the opposite: **most tasks are expected to be OPEN** (out of the
engine's current reach) by design. An open task lowers the coverage number; it is
never a CI failure. Nothing in this directory is wired into the test gate. The
separation is by consumption mode, not location: regression = exact `assert_eq!`
per-PR; coverage = a number from `run.sh`, informational.

## Layout

- `tip/` — tasks translated from **TIP (Tons of Inductive Problems)**,
  `github.com/tip-org/benchmarks`, BSD-3-Clause. See `tip/PROVENANCE.md` and
  `tip/LICENSE.TIP` (upstream license retained for attribution; the repo itself
  stays MIT).
- `handwritten/` — license-clean canonical tasks we authored.
- `run.sh` — the coverage runner (lives with the corpus so it travels if the
  corpus ever graduates to its own repo).

## Metric

A task is **covered** when `aver proof <f> --check --check-json` reports
`"passed": true` — the Lean export of the universal law kernel-checks with no
sorry/axiom over budget, via any auto-mode strategy (structural induction,
accumulator-fold spec-equivalence, …). Lake-gated; run `cargo build --bin aver`
first, then `./run.sh`. (The `--discover` lemma-discovery class — codec roundtrips,
homomorphism enumeration — was measured to add nothing on the current TIP slice,
since its law-closing classes are codec-roundtrip, absent here, and additive-monoid,
already covered by the auto-mode strategy.)

## First measurement (Aver 0.24.1-dev, branch proof-lemma-discovery)

TIP isaplanner (86 problems), translated by per-problem agents from the upstream
`.smt2`:

- **77 / 85** translated (8 flagged untranslatable into first-order Aver: needs
  higher-order functions or multi-type-var pairs).
- **64** compile for proof (the other ~13 were translation errors, dropped).
- **1** proves: `prop_82` (`take n (zip xs ys) = zip (take n xs) (take n ys)`,
  closed by structural induction). Plus the handwritten `sum_acc_spec`
  (`sumFold = sumSpec`, accumulator-fold spec-equivalence).

So on the third-party TIP slice the honest figure is **~1 proved**. The value is
the gap map, not the number: the dominant blockers are (a) Peano `Nat` arithmetic
functions (`+`, `-`) reported "outside proof subset", and (b) general list/Nat
induction lemmas that no current strategy auto-discovers. Those are the roadmap.

## Adding tasks

Translate one upstream problem per `.av` file (Peano `Nat` → `type Nat { Z | S(Nat) }`,
`list a` → `List<Int>`, `++` → `List.concat`, `nil`/`cons` → `[]`/`[h, ..t]`,
the `forall` goal → a `verify <fn> law <name>` with sampled `given`s). Keep TIP
provenance per `tip/PROVENANCE.md`. A file only needs to compile under
`aver proof`; per-function `verify` blocks (which `aver check` wants) are optional
here — the law is what we measure.
