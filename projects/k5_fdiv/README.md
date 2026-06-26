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
| **1. Float-as-rational + rounding** | format predicates, the six rounding modes ("floating-point folklore") | ⏳ planned |
| **2. Newton–Raphson bounds** | the nonlinear error estimates — `domain/estimate.av`: square/product nonneg, monotonicity, transitivity-through-products, the error-squaring identity, the contraction bound | 🟡 **proven on Z3/Dafny** (7 laws, push-button); **4 of the 7 also `universal` on the Lean kernel.** The generic nonnegativity primitive shipped — `aver_int_nonneg`, the nonlinear analog of `omega` for the products-and-squares fragment (one decision step: recurse on the product with `Int.mul_nonneg`, sign-split squares — *not* a per-figure tactic) — closing the square/product-nonneg sub-family kernel-genuine (`sqNonneg`, plus `mulNonneg`/`tripleNonneg` as TRUE universals with the `when`-guard threaded in as a hypothesis); `grind` closes the error-squaring ring identity. Remaining Lean frontier: the order sub-family (monotonicity `sqMono`, transitivity `mulLeTrans`) and the contraction bound, which composes from the simpler facts via laws-as-lemmas |
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
