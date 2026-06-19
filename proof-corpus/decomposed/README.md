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

## Contents (20)

The first ten are the original chunk; the next six are **decomposition-reach
chunk #1** (2026-06-10) — the reverse/accumulator family, opened up by the
accumulator-generalizing induction that landed in the auto-prover (`induction xs
generalizing acc`). The last four are **chunk #2** — a deterministic spread over
the fresh open pool (drop/concat, length-comm, rev-rev-append, mult-accumulator).
All were OPEN even to Z3/Dafny.

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
| `handwritten/qrev_rev.av` | `fastRev x = rev x` | OPEN (frontier) | accumulator-generalization: `qrev(xs,acc) = rev(xs)++acc` |
| `isaplanner/prop_19.av` | `len(drop n xs) = len xs − n` | OPEN (frontier) | the generalized `len∘drop` law over `n` |
| `prod/prop_27.av` | `rev x = qrev x []` | OPEN (frontier) | `qrev` spec `qrev(x,y) = rev(x)++y` |
| `prod/prop_28.av` | `revflat x = qrevflat x []` | OPEN (frontier) | append↔concat bridge + append right-identity/associativity + the `qrevflat` accumulator spec `qrevflat(x,y) = append(revflat x, y)` |
| `prod/prop_29.av` | `rev(qrev x []) = x` | OPEN (frontier) | qrev-spec + rev-append homomorphism + rev involution |
| `prod/prop_30.av` | `rev(rev x ++ []) = x` | OPEN (frontier) | rev-snoc + rev involution |
| `prod/prop_31.av` | `qrev(qrev x []) [] = x` | OPEN (frontier) | generalized `qrev` "master" law `qrev(qrev(x,y),[]) = qrev(y,x)` |
| `isaplanner/prop_55.av` | `drop n (xs++ys) = drop n xs ++ drop (n−len xs) ys` | OPEN (frontier) | the generalized drop-over-concat split law |
| `prod/prop_02.av` | `length(x++y) = length(y++x)` | OPEN (frontier) | `length = List.len` bridge (length-homomorphism via the builtin) |
| `prod/prop_19.av` | `rev(rev x)++y = rev(rev(x++y))` | OPEN (frontier) | append-nil-right + append-assoc + rev-distribution + rev-involution |
| `prod/prop_34.av` | `times x y = mult x y 0` | OPEN (frontier) | `plus`-right-zero + the mult-accumulator generalization |

Eighteen of the twenty move the **union frontier** — the unaided engine
(auto-mode, `--discover`, and single-shot Dafny/Z3) closes none of them. The
other two upgrade a Dafny-only (Z3-automated) task to a kernel-checked Lean
proof. Every helper is a CANONICAL lemma (reflexivity, a homomorphism, an
accumulator/qrev specification, a length/count preservation, the right-laws of
`plus`) — textbook decompositions, not exotic tricks — and each bottoms out in
something Aver's auto-prover discharges on its own (structural induction /
accumulator-generalizing induction / homomorphism / the canonical-Peano
bridge). The loop is exactly as strong as that leaf reach.
