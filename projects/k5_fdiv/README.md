# K5 FDIV — a processor's divider, proven in Aver

In 1994 the Pentium FDIV bug — a wrong floating-point division — cost Intel about
$475M and a lot of trust. AMD's answer for its next chip (the K5 / AMD5K86) was to
**formally prove the divider correct**: *A Mechanically Checked Proof of the
Correctness of the Kernel of the AMD5K86 Floating-Point Division Algorithm*
(Moore, Lynch & Kaufmann, 1996), one of the flagship results of the ACL2 prover and
a landmark of industrial formal methods.

This project reproduces that proof — **as ordinary Aver code**.

## The point

This is **not a new idea.** The proof exists; ACL2 did it in 1996. What this project
is, is a **proof of capability**: the same theorem, written the way Aver wants you to
write everything — normal code plus `verify`/`law` blocks — and discharged by the
compiler. The grandson reproduces the grandfather's masterpiece. The day
`aver check` says the divider computes the correctly-rounded quotient for every input,
the question "is this language a toy?" stops being interesting.

## The discipline (this is load-bearing)

**We do not write the proof in Lean.** No hand-written tactics, no `nlinarith`, no
`.lean` script anyone edits. If K5 were 160 hand-written Lean proofs in disguise, it
would prove *nothing* — ACL2 already wrote those.

What we write is **clean, provable Aver**: the divider as normal code, the ~160
intermediate facts as `verify ... law` blocks. **The machine proves them.** Lean's
kernel and Z3/Dafny are the certifying backends underneath, invisible to the author.
A helper fact that the prover can't yet close is handled by *stating it as another
Aver law* (fed to the lemma pool via The Method) or by building a **generic** prover
strategy for its whole class — never a one-off Lean script.

Success is measured exactly one way: the corpus reports `universal` / kernel-genuine
(`#print axioms` ⊆ `{propext, Quot.sound}`, zero `sorry`), and the repository contains
**not one line of hand-written Lean**.

## Stages

Each stage is independently useful as a verified corpus.

| Stage | What | Status |
|-------|------|--------|
| **0. Rationals** | exact `num/den` rationals, ring algebra (the paper models floats as exact rationals) | ✅ **proven** — `domain/rational.av`, 10 ring laws, `universal` on Lean (`[propext, Quot.sound]`) and Dafny/Z3 |
| **1. Float-as-rational (faithful normalized model)** | the paper's representation (Section 5.1, p.10): every value is `sign · s · 2^exp` with `sign` either `+1` or `−1`, a **normalized** rational significand `s ∈ [1,2)` (an n-bit integer `sigBits ∈ [2^(n-1), 2^n)`), an integer exponent, and the width n; the denoted value is the Stage-0 exact `Fraction` | 🟡 **seeded** — `domain/fprep.av`: **Lemma 7.1.2 (p.18)** landed `universal` on the Lean kernel, both halves — the significand is invariant under scaling, `s(x·2^j) = s_x`, and the exponent shifts, `e(x·2^j) = e_x + j` — which hold *definitionally* because `fpScale` renormalizes nothing. Plus the power-of-two homomorphism `pow2(m+n) == pow2(m)·pow2(n)` and the folklore value-of-scaling corollary `fpValue(x·2^j) == fpValue(x)·2^j`, all `universal` (`#print axioms ⊆ {propext, Classical.choice, Quot.sound}`, zero `sorry`). The value corollary is the **laws-as-lemmas composition** end to end, ACROSS the `Domain.Rational` module boundary: it re-proves nothing about powers of two — it *cites* the proven homomorphism and the compiler composes them via one generic strategy, never a per-figure proof. **Lemma 7.1.7 (p.18)** also landed `universal`: *if `x ≠ 0` and `y ≠ 0` then `e_x + e_y ≤ e(x·y) ≤ e_x + e_y + 1`* — the product-exponent range, stated exactly as the paper (a nonzero float is one with a nonzero significand). It is closed by the **generic match-splitting already in the engine**: the keystone's `grind` case-splits `fpMul`'s normalization branch (shift 0 vs 1) and bounds each arm — no new tactic, no per-figure code. **Deferred** (the open frontier): multiplication's value-preservation (sample-verified `fpMulValue`; universal on Z3/Dafny). Its obstacle is **not** the case-split (the same match-splitting that closes 7.1.7 reduces each branch cleanly) but a **pow2 homomorphism *rearrangement***: the product denominator `pow2(w_a+w_b−2)` must equal `pow2(w_a−1)·pow2(w_b−1)`, i.e. the homomorphism at the rearranged exponent `(w_a−1)+(w_b−1)`; `grind`'s e-matcher splits the term's syntactic `+` (reading `w_a+w_b−2` as `(w_a+w_b−1)+(−1)`, whose negative summand the nonneg premise rejects) and never instantiates it. That needs a pow2 abelian-group-homomorphism normalizer (a new generic mechanism), not a case-split. **Lemma 7.2.12** needs `trunc` plus a strict product bound; then the rounding modes |
| **2. Newton–Raphson bounds** | the nonlinear error estimates — `domain/estimate.av`: square/product nonneg, monotonicity, the right-factor monotonicity bound, transitivity-through-products, the error-squaring identity, the contraction bound | ✅ **proven** — **all 8 laws `universal` on the Lean kernel** (`#print axioms ⊆ {propext, Classical.choice, Quot.sound}`, 0 sampled, 0 sorries); push-button on Z3/Dafny. Two reusable mechanisms do it, no per-figure tactics. (1) `aver_int_order`, the nonlinear analog of `omega` for the products-and-squares fragment — recurse on a product with `Int.mul_nonneg` (nonnegativity) / `Int.mul_le_mul` (`prod ≤ prod`) / `Int.mul_le_mul_of_nonneg_right` (shared-right-factor bound), sign-split squares — closes the nonneg sub-family (`sqNonneg`, `mulNonneg`, `tripleNonneg`), the monotonicities (`sqMono`, `mulLeMonoRight`), and the contraction bound (`nrContraction`); `grind` closes the error-squaring ring identity. (2) **order-law composition**: the transitivity bound `mulLeTrans` (`a·c ≤ m` when `a ≤ b`, `0 ≤ c`, `b·c ≤ m`) is closed by *citing* `mulLeMonoRight` — the proof composer restates an earlier inequality law as a rewrite trigger over its two comparison sides and chains the instantiated bound `a·c ≤ b·c` with the premise `b·c ≤ m`. Shape-keyed and derived from the cited Aver law (deleting `mulLeMonoRight` breaks it), not a re-proof — the same laws-as-lemmas composition that closes Stage 1, extended from equalities to inequalities |
| **3. Kernel divide** | the 32 straight-line steps + the final theorem: the result is the input quotient, correctly rounded | ⏳ planned |

Why it's tractable: the algorithm is **straight-line** (two Newton iterations + four
quotient digits + a rounded sum) — zero unbounded recursion, so no termination/fuel
problem. Modelling floats as exact rationals sidesteps floating-point/FFI semantics
entirely. The honest wall is **nonlinear rational arithmetic** (NR error bounds): Z3
proves it natively; the Lean side gets one reusable generic strategy rather than 160
bespoke proofs — the first task where the dual backend earns its keep.

## Verify it

```
aver check  projects/k5_fdiv/main.av --module-root projects/k5_fdiv --deps
aver verify projects/k5_fdiv/domain/rational.av --module-root projects/k5_fdiv
aver proof  projects/k5_fdiv/domain/rational.av --check          # Lean kernel: 0 sorries, universal
aver proof  projects/k5_fdiv/domain/rational.av --check --backend dafny
```

## Source

Moore, Lynch & Kaufmann, *A Mechanically Checked Proof of the Correctness of the
Kernel of the AMD5K86 Floating-Point Division Algorithm*, 1996. (The corrected sticky
lemma is in the 1998 IEEE TC journal version, §2.2 / 6.3.2; stage 3 follows that form.)
