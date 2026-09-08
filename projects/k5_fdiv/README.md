# K5 FDIV — reproducing a processor divider proof in Aver

The executable Fraction truncation, away-from-zero rounding and sticky rounding
now agree with the normalized model. The universal laws
`Kernel.sameTruncation.normalizedModel`, `Kernel.sameAway.normalizedModel`, and
`Kernel.sameSticky.normalizedModel` prove, respectively:

```text
sameValue(truncFrac(fpValueGeneral(f), n), fpValueGeneral(fpTrunc(f, n)))
sameValue(awayFrac(fpValueGeneral(f), n), fpValueGeneral(fpAway(f, n)))
sameValue(stickyFrac(fpValueGeneral(f), n), fpValueGeneral(fpSticky(f, n)))
```

Their guards are `isFp(f)`, `f.width >= 1`, and `n >= 1`. They cover both signs,
every integer exponent, precision widening, upward carries across an exponent
boundary, and the one-bit sticky case. Executable counterexamples at `n = 0`
retain the truncation/sticky precision boundary. Away rounding agrees in that
zero-precision example but differs at `n = -1`; its positive-precision guard is
not claimed to be minimal.

The executable exponent now agrees with the normalized floating-point model:
`Kernel.normalizedValueExponent.executableExponent` states
`fracExpo(fpValueGeneral(f)) == f.exp` for `isFp(f)` and `f.width >= 1`.
It covers both signs and every integer exponent, including the reciprocal scale
below one. This is the exponent bridge needed to transport the model's rounding
proofs to the executable Fraction operations.

The source argument uses ordinary Aver laws. `IntegerOrder` proves multiplication
monotonicity by integer descent. `FractionOrder` transports cross-product bounds
to the existing exact rational order. `ModelScale` brackets a normalized model
value and proves uniqueness of a binary magnitude interval; `BinadeOrder` reads
the executable integer bracket as the same rational interval. The Kernel then
identifies the two exponents through `because` and `using`.

`TruncScale` then proves five reusable laws: equal positive-denominator ratios
have equal floors; the signed exponent scale factors into an ulp and a
significand scale; floor quotients and restored values respect that change of
scale; and positive magnitudes preserve a unit sign. `modelSign` identifies the
executable sign, `modelTruncation` proves the fixed-exponent formula, and
`truncationFormula` substitutes the established exponent into `truncFrac`.
The final equality composes those source laws. This argument was developed with
Aver source and `--explain`, without inspecting the generated Lean.

`RoundScale` adds generic integer cancellation, exactness and ceiling laws.
`AwayModel` proves that a significand carry preserves the value, including the
negative-exponent boundary. `ValueChain` composes cross-product equalities through
a positive middle denominator; its zero-denominator counterexample records why
that premise matters. The sticky argument separately proves the half-precision
quotient and exactness test, then handles the one-bit case before composing the
executable operation.

The source reports identified open arithmetic implications and checker limits at
large nested applications. Explicit product-regrouping `because` steps, directly
citable Bool helpers, and smaller model-level laws closed those gaps. All of these
arguments were developed through Aver source, `context`, `check`, `verify`, and
`proof --explain`, without inspecting generated Lean. A universally checked helper
with singleton sample domains also exposed a generic
citation-admission inconsistency: its source citation was listed as universal but
was unavailable to the proof composer. The compiler now admits guided source laws
consistently with their universal emission. The proof argument stayed in Aver;
diagnosing that inconsistency still required compiler work. Open obligations can
still require richer source diagnostics.

The Kernel export and its imports now contain **115 universal laws**, the same
**4 bounded laws**, and **252 universal proof steps**, with no `sorry` or build
errors. The earlier universal laws retain their proof tiers and axiom sets.
For an open step, `aver proof ... --check --explain` reports its Aver goal,
assumptions and cited requirements at the source location. Supported direct
citations also report the actual premises closed or left open in an isolated
application, with dependency and closure-axiom audits; see
[proof diagnostics](../../docs/lean.md#step-zero-which-law-failed).

General Fraction trunc-sticky composition, sticky-plus, final-round composition,
and the complete divider correctness theorem remain open. The wrapper still calls
`fpValue`; connecting
its negative-exponent inputs to `fpValueGeneral` is also outstanding.

In 1994 the Pentium FDIV bug — a wrong floating-point division — cost Intel about
$475M and a lot of trust. AMD's answer for its next chip (the K5 / AMD5K86) was to
**formally prove the divider correct**: *A Mechanically Checked Proof of the
Correctness of the Kernel of the AMD5K86 Floating-Point Division Algorithm*
(Moore, Lynch & Kaufmann, 1996), one of the flagship results of the ACL2 prover and
a landmark of industrial formal methods.

This project aims to reproduce that proof **as ordinary Aver code**. The audited
fragments and the remaining end-to-end gap are listed below.

## The point

This is **not a new idea.** The proof exists; ACL2 did it in 1996. What this project
is, is a **proof of capability**: the same theorem, written the way Aver wants you to
write everything — normal code plus `verify`/`law` blocks — and discharged by the
compiler. The grandson reproduces the grandfather's masterpiece. The day
`aver proof ... --check` establishes that the divider computes the correctly-rounded quotient for every input,
the question "is this language a toy?" stops being interesting.

## The discipline (this is load-bearing)

**We do not write the proof in Lean.** No hand-written tactics, no `nlinarith`, no
`.lean` script anyone edits. If K5 were 160 hand-written Lean proofs in disguise, it
would prove *nothing* — ACL2 already wrote those.

What we write is **clean, provable Aver**: the divider as normal code and
intermediate facts as `verify ... law` blocks. The machine checks the proofs;
the audited status distinguishes universal results from samples and open claims.
Lean currently checks the full K5 guided proof closure. Dafny's
[Int/Bool guidance pilot](../../docs/dafny-guidance-spike.md) also checks all four
laws and eight steps in the unchanged `domain/integerorder.av`, including its
recursive multiplication argument. Imported citations, records and division in
the rounding arguments remain outside that pilot's supported fragment.
A helper fact that the prover can't yet close is handled by *stating it as another
Aver law* (fed to the lemma pool via The Method) or by building a **generic** prover
strategy for its whole class — never a one-off Lean script.

Success means universal, kernel-checked theorems with no `sorry` or foreign
axioms (allowed core axioms: `propext`, `Classical.choice`, `Quot.sound`).
Aver no longer loads hand-written Lean or Dafny proof sidecars. Proof steps belong
in the source as `because` expressions and `using` citations; the backends check
those steps and the final implication.

`domain/stickyint.av` now proves all **9 laws universally**, including
`truncSig(stickySig(A, dn), jt) = truncSig(A, dn + jt - 1)` for every integer `A`,
`dn >= 0`, and `jt >= 1`. Its source argument factors the powers of two, collapses
nested floors, and discards the low sticky bit. The generic floor laws retain
their divisor and remainder premises when cited. The trivial `sigDouble` law
needs no manual Dafny body either.

The normalized floating-point composition is now proved as well.
`domain/stickyscale.av` transports the integer low-bit law through the two
significand scales for any positive divisor. `fpSticky.preservesCoarseTruncation`
then proves equality of the **entire truncated records** for `1 <= m < n`,
without requiring a normalized significand or a nonnegative exponent. The public
`truncStickyComposes.composesThroughSticky` law retains its original width guard
and follows by taking rational values of those equal records. A declared
counterexample at `m = n` makes the strict precision boundary executable.

A fresh Lean check of `round.av` and its imports reports **62 universal laws and
23 universal proof obligations**, with zero bounded laws, build errors or `sorry`.
All 50 previously universal laws retain their tier without growing their axiom
sets. This does **not** complete the divider proof: the stage notes below include
other parts of the project that have not received this proof audit.

The integer module is now named `stickyint.av`, matching the loader's path for
`Domain.StickyInt`; it was previously a standalone file named `sticky_int.av`.

## Stages

Each stage is independently useful as a verified corpus.

| Stage | What | Status |
|-------|------|--------|
| **0. Rationals** | exact `num/den` rationals, ring algebra (the paper models floats as exact rationals) | ✅ **proven** — `domain/rational.av`, 11 ring laws, `universal` on Lean (`[propext, Quot.sound]`) and Dafny/Z3 |
| **1. Float-as-rational (faithful normalized model)** | the paper's representation (Section 5.1, p.10): every value is `sign · s · 2^exp` with `sign` either `+1` or `−1`, a **normalized** rational significand `s ∈ [1,2)` (an n-bit integer `sigBits ∈ [2^(n-1), 2^n)`), an integer exponent, and the width n; the denoted value is the Stage-0 exact `Fraction` | 🟡 **seeded** — `domain/fprep.av`: **Lemma 7.1.2 (p.18)** landed `universal` on the Lean kernel, both halves — the significand is invariant under scaling, `s(x·2^j) = s_x`, and the exponent shifts, `e(x·2^j) = e_x + j` — which hold *definitionally* because `fpScale` renormalizes nothing. Plus the power-of-two homomorphism `pow2(m+n) == pow2(m)·pow2(n)` and the folklore value-of-scaling corollary `fpValue(x·2^j) == fpValue(x)·2^j`, all `universal` (`#print axioms ⊆ {propext, Classical.choice, Quot.sound}`, zero `sorry`). The value corollary is the **laws-as-lemmas composition** end to end, ACROSS the `Domain.Rational` module boundary: it re-proves nothing about powers of two — it *cites* the proven homomorphism and the compiler composes them via one generic strategy, never a per-figure proof. **Lemma 7.1.7 (p.18)** also landed `universal`: *if `x ≠ 0` and `y ≠ 0` then `e_x + e_y ≤ e(x·y) ≤ e_x + e_y + 1`* — the product-exponent range, stated exactly as the paper (a nonzero float is one with a nonzero significand). It is closed by the **generic match-splitting already in the engine**: the keystone's `grind` case-splits `fpMul`'s normalization branch (shift 0 vs 1) and bounds each arm — no new tactic, no per-figure code. **Multiplication's value-preservation** `fpMulValue` also landed `universal` on the Lean kernel (every exponent, `#print axioms ⊆ {propext, Classical.choice, Quot.sound}`, 0 `sorry`). Its obstacle was never the case-split (the same match-splitting that closes 7.1.7 reduces each branch cleanly) but a **pow2 homomorphism *rearrangement***: the product denominator `pow2(w_a+w_b−2)` must be seen as `pow2(w_a−1)·pow2(w_b−1)`, the homomorphism at the rearranged exponent `(w_a−1)+(w_b−1)` — which `grind`'s bare e-matcher misses on the syntactic `+`. The **signed-power-of-two homomorphism normalizer** (the `Fraction`-level companion of the integer pow2 normalizer) supplies exactly that: it canonicalizes `2^(m+n)`/`2^(m+n+1)` through the proven Aver homomorphism law, cited in cross-multiplied form, so `grind` closes the composition — a generic mechanism keyed on the signed-power-of-two cone shape, never a per-figure proof. **Lemma 7.2.12** needs `trunc` plus a strict product bound; then the rounding modes |
| **2. Newton–Raphson bounds** | the nonlinear error estimates — `domain/estimate.av`: square/product nonneg, monotonicity, the right-factor monotonicity bound, transitivity-through-products, the error-squaring identity, the contraction bound | ✅ **proven** — **all 8 laws `universal` on the Lean kernel** (`#print axioms ⊆ {propext, Classical.choice, Quot.sound}`, 0 sampled, 0 sorries); push-button on Z3/Dafny. Two reusable mechanisms do it, no per-figure tactics. (1) `aver_int_order`, the nonlinear analog of `omega` for the products-and-squares fragment — recurse on a product with `Int.mul_nonneg` (nonnegativity) / `Int.mul_le_mul` (`prod ≤ prod`) / `Int.mul_le_mul_of_nonneg_right` (shared-right-factor bound), sign-split squares — closes the nonneg sub-family (`sqNonneg`, `mulNonneg`, `tripleNonneg`), the monotonicities (`sqMono`, `mulLeMonoRight`), and the contraction bound (`nrContraction`); `grind` closes the error-squaring ring identity. (2) **order-law composition**: the transitivity bound `mulLeTrans` (`a·c ≤ m` when `a ≤ b`, `0 ≤ c`, `b·c ≤ m`) is closed by *citing* `mulLeMonoRight` — the proof composer restates an earlier inequality law as a rewrite trigger over its two comparison sides and chains the instantiated bound `a·c ≤ b·c` with the premise `b·c ≤ m`. Shape-keyed and derived from the cited Aver law (deleting `mulLeMonoRight` breaks it), not a re-proof — the same laws-as-lemmas composition that closes Stage 1, extended from equalities to inequalities |
| **Rounding — trunc** | Truncation, away/sticky rounding, error bounds and composition laws in `domain/round.av` | **All laws in this module and its imports are universal:** 62 laws, 23 source-proof obligations, no bounded claims or `sorry`. The trunc-sticky argument is source-local: `StickyInt` supplies the low-bit floor law, `StickyScale` cancels the precision scales, and `fpSticky.preservesCoarseTruncation` proves complete record equality for `1 <= m < n`. The public rational-value law follows from it. This result replaces the former manual Lean proof. |
| **3. Kernel divide** | The 32 straight-line steps and the end-to-end division theorems | **Partial source proofs.** The executable exponent has integer and rational magnitude bounds and agrees with the normalized model for either sign of the exponent. `divide.theorem_2` proves the wrapper equation under the explicit row-32 result-exponent bracket; deriving that bracket remains open. Complete divider correctness still has executable examples rather than a universal proof. The executable trunc/away/sticky bridges now agree with the normalized model at every positive precision, including one-bit sticky rounding. Remaining work includes general Fraction trunc-sticky and sticky-plus/final-round composition, reciprocal and digit/remainder bounds, the Section 9 bounds, and the wrapper's use of `fpValue` for negative exponents. |

The division driver is **straight-line** (two Newton iterations + four
quotient digits + a rounded sum). Its exponent and rounding helpers still use
recursion and require termination arguments. Modelling floats as exact rationals sidesteps floating-point/FFI semantics
entirely. The honest wall is **nonlinear rational arithmetic** (NR error bounds): Z3
proves it natively; the Lean side gets one reusable generic strategy rather than 160
bespoke proofs — the first task where the dual backend earns its keep.

## Verify it

```
aver check  projects/k5_fdiv/main.av --module-root projects/k5_fdiv
aver verify projects/k5_fdiv/domain/rational.av --module-root projects/k5_fdiv
aver proof  projects/k5_fdiv/domain/rational.av --check          # Lean kernel: 0 sorries, universal
aver proof  projects/k5_fdiv/domain/rational.av --check --backend dafny
aver proof  projects/k5_fdiv/domain/kernel.av --module-root projects/k5_fdiv --check --explain
aver verify projects/k5_fdiv/domain/kernel.av --module-root projects/k5_fdiv
```

## Source

Moore, Lynch & Kaufmann, *A Mechanically Checked Proof of the Correctness of the
Kernel of the AMD5K86 Floating-Point Division Algorithm*, 1996. (The corrected sticky
lemma is in the 1998 IEEE TC journal version, §2.2 / 6.3.2; stage 3 follows that form.)
