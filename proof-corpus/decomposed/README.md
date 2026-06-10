# decomposed/ — LLM-written helper-law decompositions of TIP tasks

Each file here is a **bare TIP task from `../tip/` that the unaided engine leaves
OPEN**, plus one or more **helper `verify ... law` blocks written by an LLM** so
the target law closes through the discovery feedback loop (część A: a proved
earlier law becomes a `simp` lemma for a later one). They demonstrate the
loop's reach — *what an LLM can close by decomposition, with the pipeline as a
fail-closed judge* — NOT the automated baseline.

## This is a SEPARATE metric — it does NOT touch the baseline

`../run.sh` measures what Aver proves **unaided** on the bare `tip/` tasks
(`coverage (union)`). It explicitly **excludes this directory** (`find ... -not
-path '*/decomposed/*'`), so adding files here never inflates that number. The
bare task in `tip/` is left untouched; the honest baseline stays honest.

The number here answers a different question: *given the loop, how many
otherwise-open TIP theorems can an LLM close by writing helper laws?*

## Integrity rules (every file here satisfies them)

- The **target law statement, the function definitions, the `intent`, and the
  module name are byte-for-byte the bare `tip/` original** — only earlier
  `verify ... law` helper blocks were added, before the target.
- **Every law in the file — helpers included — is kernel-clean** (`aver proof
  --backend lean --check` reports `"universal": true`, `#print axioms` over
  `{propext, Classical.choice, Quot.sound}` only, zero `sorry`). A helper that
  only `sorry`s would propagate `sorryAx` and the whole file would report
  `universal: false` — so a helper can never be a trust shortcut. The judge,
  not the model's confidence, decides.
- `proof_spec.rs` re-verifies every file here stays `universal: true`
  (`decomposed_tip_tasks_stay_universal_when_lake_is_available`).

## Provenance

The underlying problems are translated from **TIP (Tons of Inductive
Problems)**, `github.com/tip-org/benchmarks`, BSD-3-Clause — see
`../tip/PROVENANCE.md` and `../tip/LICENSE.TIP`. The helper laws are our
authorship; the surrounding program is the upstream-derived task verbatim.

## Contents (10)

| file | target | bare-`tip/` status | what the decomposition added |
|------|--------|--------------------|------------------------------|
| `isaplanner/prop_03.av` | `count n xs ≤ count n (xs++ys)` | OPEN (frontier) | a cone-local count-monotonicity helper |
| `isaplanner/prop_04.av` | `S(count n xs) = count n (n::xs)` | OPEN (frontier) | `eqNat n n = true` (reflexivity) + count-cons |
| `isaplanner/prop_20.av` | `length(sort xs) = length xs` | OPEN (frontier) | `insort` preserves length |
| `isaplanner/prop_28.av` | `elem x (xs ++ [x])` | OPEN (frontier) | `eqNat x x = true` + elem-append |
| `isaplanner/prop_29.av` | `elem x (ins1 x xs)` | OPEN (frontier) | `eqNat x x = true` + elem-insert |
| `isaplanner/prop_30.av` | `elem x (insert x xs)` | OPEN (frontier) | `eqNat x x = true` + elem-insert |
| `isaplanner/prop_38.av` | `count n (xs ++ [n]) = S(count n xs)` | OPEN (frontier) | `eqNat n n = true` + count-snoc |
| `isaplanner/prop_75.av` | `count n [x] + count n xs = count n (x::xs)` | Dafny-only → also Lean-genuine | count-homomorphism + `plus` commutativity |
| `prod/prop_03.av` | `length(x++y) = plus(length y)(length x)` | Dafny-only → also Lean-genuine | the two "right" laws of `plus` |
| `prod/prop_25.av` | `even(length(x++y)) = even(length y + length x)` | OPEN (frontier) | length-homomorphism + `plus` commutativity |

Eight of the ten move the **union frontier** — the unaided engine (auto-mode,
`--discover`, and single-shot Dafny/Z3) closes none of them. The other two
upgrade a Dafny-only (Z3-automated) task to a kernel-checked Lean proof. Every
helper is a CANONICAL lemma (reflexivity, a homomorphism, a length/count
preservation, the right-laws of `plus`) — textbook decompositions, not exotic
tricks — and each bottoms out in something Aver's auto-prover discharges on its
own (structural induction / homomorphism / the canonical-Peano bridge). The
loop is exactly as strong as that leaf reach.
