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
| **Rounding — trunc** | the paper's `trunc(x,n)` truncation (§5.2, p.13) on the normalized model + its elementary lemmas — `domain/round.av` | 🟡 **seeded** — `trunc` modelled faithfully: the truncated significand integer is `floor(sigBits·2^(n-1) / 2^(width-1))`, a single Euclidean floor division, with the sign and the **stored** exponent carried through (which sidesteps the recursive binary-exponent function and is exact precisely because the model is already normalized, `s_x ∈ [1,2)`). Eight laws landed `universal` on the Lean kernel (`#print axioms ⊆ {propext, Classical.choice, Quot.sound}`, 0 sorries). Prior: **Lemma 7.2.1 (p.19)** `trunc(−x,i) = −trunc(x,i)` — truncation toward zero is sign-symmetric; and **Lemma 7.2.6 (p.19)** `e(trunc(x,i)) = e_x` for `x ≠ 0` — trunc keeps the exponent (closed by the generic conditional-keystone grind). New: **the recursive-expo-free `Int.ediv` floor window.** A single generic, shape-keyed figure (`FloorPow2Window`, beside the existing window figures) proves *any* law of the form `pow2(E)·floor(N, pow2(E)) ≤ N < pow2(E)·(floor(N, pow2(E))+1)` for arbitrary numerator `N` and exponent `E`, reusing the core `Int.ediv_add_emod` / `Int.emod_nonneg` / `Int.emod_lt_of_pos` bridge with power-of-two positivity — *no* binary-exponent recursion, the divisor is `pow2(E)` directly. It closes **both** `floorDivWindow(a, k)` (bare integer givens) and `truncFitsWindow(f, i)` (compound `N = sigBits·2^(i−1)`, divisor `2^(width−1)`) from the same figure — `truncFitsWindow` is exactly the **integer core of Lemma 7.2.2 / 7.2.13**: the truncated significand sits in one `2^(width−1)` cell. And **Lemma 7.2.2 (p.19), reconstruction half**: `trunc(x,i) = x − ε` — closed `universal` by the laws-as-lemmas keystone (the exact-rational ring identity `x − (x − trunc(x,i)) = trunc(x,i)`). The **signed-power-of-two-as-`Fraction`** surface needed for the bound is built and proven clean Aver: `pow2Signed(k)` denotes `2^k` for *any* integer `k` (`k≥0 → 2^k/1`, `k<0 → 1/2^(−k)`; agrees with `pow2Fraction` on the nonnegative half, `universal`), plus `absFraction` and the genuine rational order `lessThan` (robust for either denominator sign). And **Lemma 7.2.2 (p.19), the sign condition**: `ε = x − trunc(x,i)` has the same sign as `x` (`x ≥ 0 → ε ≥ 0`; `x < 0 → ε ≤ 0`) — closed `universal`, stated EXACTLY as the paper (`truncErrorSameSign`). Its algebra is two genuine Aver helper laws: `truncErrorMagnitudeNonneg` (the error magnitude core `2^(e_x)·((sigBits·2^(i−1) − 2^(width−1)·floor(…))·(2^(width−1)·2^(i−1)))` is `≥ 0` — its powers of two are positive and the floor remainder is `≥ 0` by `truncFitsWindow`'s lower bound) and `fpValueMagnitudePos` (a normalized float's `sigBits·2^(e_x)·2^(width−1)` is `> 0`), each closed by the nonlinear order primitive `aver_int_order` (now also discharging strict products via `Int.mul_pos`). The sign condition then **cites** those two laws and `rcases`-splits the float's stored sign — a generic, name-blind keystone arm (cite earlier `holds` laws over a prefix of the givens, supply power-of-two positivity at the record-field exponents, sign-split the format predicate's leading disjunction), reusing the proven window + helper laws (deleting any breaks it). The pow2 homomorphism normalizer is left OFF these rational-over-floor goals (it explodes on the squared denominators of the rational order) by routing them to this arm before it engages; the equational `fpMul` / `fpScale` value laws keep it. **Deferred (kwiatki):** Lemma 7.2.2's **strict bound** `\|ε\| < 2^(e_x−i+1)` is stated EXACTLY as the paper (`truncErrorBound`) and sample-verified, but not yet `universal`: it needs `truncFitsWindow`'s integer floor **upper** bound *composed* through the `Fraction` order (unfold `lessThan`/`absFraction`, cancel the `2^exp·2^(i−1)` factors via the pow2 homomorphism, case-split the sign of `e_x−i+1`), and `lessThan` squares the denominators, so the pow2-normalizer recurses into `pow2(pow2(…))` runaway on it — the homomorphism composition through the squared rational order is the open piece. Also deferred: 7.2.5 (monotonicity), 7.2.10 (composition `trunc(trunc(x,i),j) = trunc(x,i)`, `i ≤ j`), 7.2.14 (integer-fits), 7.2.12 (product floor workhorse), and away/sticky |
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
