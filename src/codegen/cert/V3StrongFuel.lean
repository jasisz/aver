/- LUKA-1 residual backbone: the general `wRunF` sequencing lemma.

   STUCK.md flagged that ordinary list induction and ordinary one-step fuel
   induction are both too weak for nested `ifElse`, because the audited
   `lowerNodesFuel (fuel+1)` recurses on the node tail at `fuel` AND lowers an
   `ifElse` branch through `lowerBlockFuel fuel` (→ `lowerNodesFuel (fuel-1)`).
   A `∀ smaller ≤ fuel` strong induction covers both.

   The reusable backbone that makes any such strong-fuel proof clean is the
   fact that the audited interpreter `wRunF` distributes over instruction
   concatenation.  V3LinearCore threads this per-case via a `∀ rest`
   continuation; proved once as a standalone lemma it discharges the append
   obligation in every node arm — linear leaves AND the `ifElse` branch — with
   a single rewrite, which is exactly what the full-grammar strong-fuel theorem
   needs.  This file proves it kernel-clean over the whole audited `WInstr`
   set, including the recursive `ifElse` instruction. -/
import CertPrelude

set_option maxRecDepth 100000
set_option maxHeartbeats 4000000

namespace V3StrongFuel
open CertPrelude

/-- Sequencing of an `Out`: continue with `ys` on a normal frame, halt on a
    return, propagate failure. -/
def seqOut
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (ys : List WInstr) : Option Out → Option Out
  | some (.ok locals stack) => wRunF host ar callee ys locals stack
  | some (.ret v) => some (.ret v)
  | none => none

/-- The audited interpreter distributes over instruction append.  Proved by
    induction on the prefix, generalising the frame; the `ifElse` instruction
    recurses on its branches (which do not depend on the appended tail) and
    then continues, so the same rewrite closes it. -/
theorem wRunF_append
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) :
    ∀ (xs ys : List WInstr) (locals stack : List WVal),
      wRunF host ar callee (xs ++ ys) locals stack =
        seqOut host ar callee ys (wRunF host ar callee xs locals stack) := by
  intro xs
  induction xs with
  | nil =>
      intro ys locals stack
      simp [seqOut, wRunF]
  | cons x xs ih =>
      intro ys locals stack
      cases x <;>
        rw [List.cons_append] <;>
        unfold wRunF <;>
        (repeat' split) <;>
        simp only [seqOut, ih]


/-! ## Why this closes the STUCK.md #2 obstacle

With `wRunF_append` in hand, the append obligation in every node arm of the
full lowering-correctness theorem — `wRunF (loweredNode ++ restInstrs ++ tail)
= seqOut (restInstrs ++ tail) (wRunF loweredNode)` — is a single rewrite,
uniformly for linear leaves AND for the `ifElse` instruction (whose branch
execution does not touch the appended tail).  The remaining LUKA-1 assembly is
then the mutual strong-fuel recursion:

    theorem mutualCorrect :
      ∀ fuel, (∀ smaller, smaller < fuel → NodesCorrect smaller ∧ BlockCorrect smaller)
        → NodesCorrect fuel ∧ BlockCorrect fuel

proved by `Nat.strongRecOn` on `fuel`.  At `fuel+1`: the audited
`lowerNodesFuel (fuel+1)` recurses the tail at `fuel` (use the IH's
`NodesCorrect fuel`) and lowers an `ifElse` branch at `lowerBlockFuel fuel`
(use the IH's `BlockCorrect fuel`, glued through the audited `.ifElse`
instruction by `V3ExprFragmentIfElse.blockOK_ifElse`); `BlockCorrect (fuel+1)`
calls `lowerNodesFuel fuel` (use `NodesCorrect fuel`).  Every sequencing step
is discharged by `wRunF_append`.  The residual beyond this skeleton is
case-transcription for the remaining leaf kinds and the `carrierSmall`
source-model bridge (STUCK.md #1, #3) — engineering, not a missing principle,
and `V3CorpusWitness.inAsciiDigit_corpus_statement` already exhibits the exact
target for a real branching+comparing corpus export, kernel-clean. -/

end V3StrongFuel
