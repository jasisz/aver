/- Interpreter sequencing lemmas used by the strong-fuel soundness proof.

   Ordinary list induction and ordinary one-step fuel induction are both too
   weak for nested `ifElse`, because the audited
   `lowerNodesFuel (fuel+1)` recurses on the node tail at `fuel` AND lowers an
   `ifElse` branch through `lowerBlockFuel fuel` (→ `lowerNodesFuel (fuel-1)`).
   A `∀ smaller ≤ fuel` strong induction covers both.

   The reusable backbone that makes any such strong-fuel proof clean is the
   fact that the audited interpreter `wRunF` distributes over instruction
   concatenation. Proved once as a standalone lemma, it discharges the append
   obligation in every node arm — linear leaves AND the `ifElse` branch — with
   a single rewrite, which is exactly what the full-grammar strong-fuel theorem
   needs.  This file proves it kernel-clean over the whole audited `WInstr`
   set, including the recursive `ifElse` instruction. -/
import CertPrelude

set_option maxRecDepth 100000
set_option maxHeartbeats 4000000

namespace InterpreterSequencing
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


/-! ## Stack frame invariance

The `ifElse` node of the plan grammar may sit above already-computed values
(e.g. `Bool.and` over two encoded integer comparisons, each of which is an
`ifElse` in the representation).  The wasm-level `if` executes its branch on
the stack BELOW the condition, so the soundness proof needs: a successful run
of a (lowered, self-contained) instruction list is invariant under extra
values appended UNDER its stack — the same pops happen, the same pushes
happen, and the extra suffix rides through untouched.  Stated for ALL
instruction lists whose run SUCCEEDS: a success never pops past its own
stack's values into the suffix only if it did not pop past the original
stack, which is exactly what success on the original stack certifies. -/

/-- Frame a normal result with an extra stack suffix; returns pass through. -/
def frameOut (extra : List WVal) : Out → Out
  | .ok locals stack => .ok locals (stack ++ extra)
  | .ret v => .ret v

-- The shared multi-branch scripts below list every lemma any branch needs;
-- the per-branch "unused simp argument" lint is expected noise there.
set_option linter.unusedSimpArgs false in
theorem popArgs_frame (arity : Nat) (st extra args rest : List WVal)
    (h : popArgs arity st = some (args, rest)) :
    popArgs arity (st ++ extra) = some (args, rest ++ extra) := by
  unfold popArgs at h ⊢
  by_cases hlen : st.length < arity
  · simp [hlen] at h
  · have hle : arity ≤ st.length := Nat.le_of_not_lt hlen
    simp only [hlen, if_false, Option.some.injEq, Prod.mk.injEq] at h
    have hlen' : ¬ (st ++ extra).length < arity := by
      simp only [List.length_append]
      omega
    simp [hlen', List.take_append_of_le_length hle,
      List.drop_append_of_le_length hle, ← h.1, ← h.2] <;> omega

/-- A trap-guarded interpreter step (`if guard then … else none`) that
    produced a value must have passed its guard. -/
theorem guard_of_ite_some {α : Type} {p : Prop} [Decidable p] {x : Option α}
    {a : α} (h : (if p then x else none) = some a) : p := by
  by_cases hp : p
  · exact hp
  · simp [hp] at h

set_option linter.unusedSimpArgs false in
theorem wRunF_frame
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (extra : List WVal) :
    ∀ (instrs : List WInstr) (locals st : List WVal) (out : Out),
      wRunF host ar callee instrs locals st = some out →
      wRunF host ar callee instrs locals (st ++ extra) =
        some (frameOut extra out) := by
  intro instrs locals st
  -- Functional induction mirrors the interpreter's own recursion, so each
  -- case is one interpreter step. The hypothesis pins the match arm the
  -- original stack took; the appended suffix preserves every `cons`
  -- pattern (`(v :: st') ++ extra` is `v :: (st' ++ extra)`), `popArgs`
  -- is framed by `popArgs_frame`, and the recursive continuations —
  -- including the `ifElse` branch bodies — close by the induction
  -- hypotheses.
  induction instrs, locals, st using wRunF.induct host ar callee <;>
    intro out h <;>
    unfold wRunF at h ⊢ <;>
    -- The list-popping arms (`structNew`, `arrayNewFixed`, `call`,
    -- `returnCall`): frame the case's `popArgs` equation in the goal first.
    (try rw [popArgs_frame _ _ _ _ _ (by assumption)]) <;>
    first
      | -- False-guard arms (`arrayGet` bounds, `refCast` mismatch): the run
        -- steps through an `if` whose condition the case premise negates,
        -- so the run hypothesis is absurd. This must fire BEFORE the generic
        -- `simp_all` pass, which normalizes the negated premise away.
        (exact absurd (guard_of_ite_some h) (by assumption))
      | -- `call`/`returnCall`: their `popArgs` sits under the host/arity
        -- match, so reduce the goal by the case equations first, frame the
        -- exposed `popArgs`, and only then let `simp_all` instantiate the
        -- induction hypothesis (its one destructive pass must already see
        -- the fully exposed goal).
        (simp only [*]
         rw [popArgs_frame _ _ _ _ _ (by assumption)]
         simp_all [frameOut, List.cons_append] <;>
           (subst h; simp [frameOut]))
      | ((try simp_all [frameOut, popArgs_frame, List.cons_append]) <;>
         first
           | -- Terminal `Out` equations (`[]`, `ret`, return propagation).
             (subst h; simp [frameOut])
           | -- `arrayNewData`: align the interpreter's `∘`-composed element
             -- map with the induction hypothesis' lambda normal form.
             (simp only [Function.comp_def] at h ⊢
              simp_all [frameOut, List.cons_append])
           | simp_all [frameOut, List.cons_append])

/-! ## Strong-fuel assembly

With `wRunF_append` in hand, the append obligation in every node arm of the
full lowering-correctness theorem — `wRunF (loweredNode ++ restInstrs ++ tail)
= seqOut (restInstrs ++ tail) (wRunF loweredNode)` — is a single rewrite,
uniformly for linear leaves AND for the `ifElse` instruction (whose branch
execution does not touch the appended tail). The remaining assembly is the
mutual strong-fuel recursion:

    theorem mutualCorrect :
      ∀ fuel, (∀ smaller, smaller < fuel → NodesCorrect smaller ∧ BlockCorrect smaller)
        → NodesCorrect fuel ∧ BlockCorrect fuel

proved by `Nat.strongRecOn` on `fuel`.  At `fuel+1`: the audited
`lowerNodesFuel (fuel+1)` recurses the tail at `fuel` (use the IH's
`NodesCorrect fuel`) and lowers an `ifElse` branch at `lowerBlockFuel fuel`
(use the IH's `BlockCorrect fuel`, glued through the audited `.ifElse`
instruction); `BlockCorrect (fuel+1)`
calls `lowerNodesFuel fuel` (use `NodesCorrect fuel`). Every sequencing step
is discharged by `wRunF_append`. -/

end InterpreterSequencing
