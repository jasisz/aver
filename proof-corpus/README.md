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
- `handwritten/` — license-clean canonical tasks we authored, including
  rounding-grid floor-stability tasks over finer and coarser cells.
- `decomposed/` — OPEN `tip/` tasks closed by **LLM-written helper laws** via
  the discovery feedback loop (część A). A SEPARATE "loop reach" metric, NOT the
  baseline — `run.sh` excludes it so the coverage number stays "unaided on bare
  `tip/`". See `decomposed/README.md`.
- `run.sh` — the coverage runner (lives with the corpus so it travels if the
  corpus ever graduates to its own repo).

## Metric

A task is **covered** when `aver proof <f> --check --check-json` reports
`"passed": true` — the Lean export of the universal law kernel-checks with no
sorry/axiom over budget, via any auto-mode strategy (structural induction,
accumulator-fold spec-equivalence, …). Lake-gated; run `cargo build --bin aver`
first, then `./run.sh`.

`DISCOVER=1 ./run.sh` additionally runs `aver proof --discover` into each
task's output dir before the Lean check, so the committed kernel-proved
lemmas feed the law's own proof (the `ProofStrategy::SimpOverLemmas`
feedback loop). The default run stays discovery-free — that is the honest
no-discovery baseline, and the delta between the two runs is discovery's
measured coverage value. First measured win on this corpus: prod `prop_03`,
whose stated law needs the unstated `length (a ++ b) = plus (length a)
(length b)` homomorphism that discovery conjectures, kernel-proves, and
feeds back.

KNOWN FLAKINESS: a sweep of all tasks transiently fails some `lake` builds under
load, which records false "open" (never false "proved"). `run.sh` retries a
non-passed task once to absorb that; the reported number is therefore a lower
bound. (One un-retried sweep undercounted 8 → 2.)

## Measurement (Aver 0.24.1-dev, branch proof-lemma-discovery)

154 tasks: TIP isaplanner (78) + TIP prod (74) + 2 handwritten. Translated by
per-problem agents from the upstream `.smt2` with a self-validation loop
(yield 88/88 compiling on the second batch). 152 TIP compile under `aver proof`.

**Coverage (union) = 32 / 154 (~21%), retry-hardened. Lean: 8. Dafny: 32.**

The two backends differ a LOT, and Dafny is a STRICT SUPERSET here:
- **Dafny (Z3) proves 32** — every task Lean proves PLUS 24 more. Z3's automated
  induction + arithmetic closes Peano/Nat and list-induction lemmas (prop_11, 13,
  15, 17, …, lemma_02, 05, 07, …) that Aver's hand-rolled Lean strategies miss.
- **Lean proves 8** — only what Aver's bespoke strategies cover (structural
  induction, accumulator-fold spec-equivalence): `sum_acc_spec`; isaplanner
  `prop_46`, `prop_82`; prod `lemma_08`, `_10`, `_11`, `_13`, `_22`.
- **Lean-only = ∅** — Lean proves nothing Dafny doesn't.

Reading: "Lean = source of truth" is about TRUST (kernel-checked), not REACH.
Dafny has more reach (Z3 automation) but less trust (Z3 is trusted, not
kernel-certified). The 24 dafny-only tasks are "proved by Z3, not yet
kernel-certified in Lean".

The real frontier is the **~122 tasks NEITHER backend proves** — the genuinely
hard inductive theorems needing auxiliary-lemma discovery / IH-generalization
that no off-the-shelf prover auto-does. That is exactly where Aver's
lemma-discovery layer (the accumulator-generalization / relational-brick work) is
the differentiated bet, and what the corpus should be mined for next: tasks where
discovery cracks a goal Z3 cannot. (The Lean-strategy gap vs Dafny is a TRUST
play — kernel-certifying what Z3 already proves — not a reach play; it does not
grow the union.)

Since this measurement, a new handwritten task (`handwritten/cell_floor_grid.av`,
the floor-grid floor-stability pair) was added; it enters future sweeps and is
not reflected in the dated counts above.

## Adding tasks

Translate one upstream problem per `.av` file (Peano `Nat` → `type Nat { Z | S(Nat) }`,
`list a` → `List<Int>`, `++` → `List.concat`, `nil`/`cons` → `[]`/`[h, ..t]`,
the `forall` goal → a `verify <fn> law <name>` with sampled `given`s). Keep TIP
provenance per `tip/PROVENANCE.md`. A file only needs to compile under
`aver proof`; per-function `verify` blocks (which `aver check` wants) are optional
here — the law is what we measure.
