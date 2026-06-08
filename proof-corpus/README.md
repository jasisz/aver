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

KNOWN FLAKINESS: a sweep of all tasks transiently fails some `lake` builds under
load, which records false "open" (never false "proved"). `run.sh` retries a
non-passed task once to absorb that; the reported number is therefore a lower
bound. (One un-retried sweep undercounted 8 → 2.)

## Measurement (Aver 0.24.1-dev, branch proof-lemma-discovery)

154 tasks: TIP isaplanner (78) + TIP prod (74) + 2 handwritten. Translated by
per-problem agents from the upstream `.smt2` with a self-validation loop
(yield 88/88 compiling on the second batch).

- **152 / ~160** TIP problems translated + compile under `aver proof`
  (the residual are higher-order or multi-type-var-pair goals — not first-order).
- **8 / 154 proved** (retry-hardened): handwritten `sum_acc_spec`; isaplanner
  `prop_46` (`zip [] xs = []`), `prop_82` (`take n (zip xs ys) = zip (take n xs)
  (take n ys)`); prod `lemma_08`, `lemma_10`, `lemma_11`, `lemma_13`, `lemma_22`.

So ~**7/152 on third-party TIP** (~5%). The value is the gap map, not the number.
prod (Productive-Use-of-Failure, IH-generalization-heavy) contributes 5/8 — it
sits closer to Aver's accumulator-generalization reach than the isaplanner Nat
arithmetic. Dominant blockers: (a) Peano `Nat` `+`/`-` reported "outside proof
subset"; (b) general list/Nat induction lemmas no current strategy auto-discovers.
Those are the roadmap.

## Adding tasks

Translate one upstream problem per `.av` file (Peano `Nat` → `type Nat { Z | S(Nat) }`,
`list a` → `List<Int>`, `++` → `List.concat`, `nil`/`cons` → `[]`/`[h, ..t]`,
the `forall` goal → a `verify <fn> law <name>` with sampled `given`s). Keep TIP
provenance per `tip/PROVENANCE.md`. A file only needs to compile under
`aver proof`; per-function `verify` blocks (which `aver check` wants) are optional
here — the law is what we measure.
