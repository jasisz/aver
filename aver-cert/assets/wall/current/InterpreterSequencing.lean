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
happen, and the extra suffix rides through untouched.

Stated for ALL instruction lists, hypothesised only on SUCCESS.  Success on
the original stack is what rules out reaching into the suffix: every arm
either matches a fixed number of stack tops, which `cons` distributes over
`++`, or pops through `popArgs`, whose only stack test is a length check
that the appended suffix can only relax while leaving the taken and dropped
segments unchanged.  Returns keep their value and abandon the stack, which
is what `frameOut` does to `.ret`. -/

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
    simp only [hlen, ite_false, Option.some.injEq, Prod.mk.injEq] at h
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

/-! ## The self tail loop

A function whose body makes a tail call to itself is emitted as one `loop`:
the body's locals are reset on entry, and each self tail call stores the new
arguments into the parameters and branches back to the loop
(`self_tail_loop.rs`). The loop's body is `proW n zs ++ convW f n 0 body ++
[ret]` (`WCode.loopBody`). The lemmas below show that such a function runs
exactly as the original `body` does, with each self tail call made through
the ordinary call semantics: `wEnter_loop`. -/

/-- `local.set (k-1); …; local.set 0`: the self call's arguments, top of the
    stack last, into the parameters. -/
def setsW : Nat → List CI
  | 0 => []
  | k + 1 => .w (.localSet k) :: setsW k

/-- The loop body of `body`: a tail call to `self` (with `n` parameters) at
    `d` enclosing `if`s becomes `setsW n` and a branch to the loop; every
    `if` becomes a control `if`. -/
def convW (self n : Nat) : Nat → List WInstr → List CI
  | _, [] => []
  | d, .returnCall g :: rest =>
      (if g = self then setsW n ++ [.br d] else [.w (.returnCall g)]) ++ convW self n d rest
  | d, .ifElse t e :: rest =>
      .ifElse (convW self n (d + 1) t) (convW self n (d + 1) e) :: convW self n d rest
  | d, x :: rest => .w x :: convW self n d rest
  termination_by _ xs => sizeOf xs

/-- `true` when `body` makes a tail call to `self`. -/
def hasSelfTail (self : Nat) : List WInstr → Bool
  | [] => false
  | .returnCall g :: rest => g == self || hasSelfTail self rest
  | .ifElse t e :: rest => hasSelfTail self t || hasSelfTail self e || hasSelfTail self rest
  | _ :: rest => hasSelfTail self rest
  termination_by xs => sizeOf xs

/-- The value a local-reset instruction pushes. -/
def zeroVal : WInstr → Option WVal
  | .i32Const 0 => some (.i32v 0)
  | .f64Const 0 => some (.f64v 0)
  | .refNull => some .null
  | _ => none

/-- The loop's entry: each declared local, from index `j` on, reset to the
    value its instruction in `zs` pushes. -/
def proW : Nat → List WInstr → List CI
  | _, [] => []
  | j, z :: zs => .w z :: .w (.localSet j) :: proW (j + 1) zs

/-- The locals after `proW j zs`. -/
def resetFrom : Nat → List WVal → List WVal → List WVal
  | _, [], l => l
  | j, v :: vs, l => resetFrom (j + 1) vs (l.set j v)

/-- Sequencing of a `COut`. -/
def cseq (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (ys : List CI) : Option COut → Option COut
  | some (.ok l st) => cRun host ar callee ys l st
  | some o => some o
  | none => none

theorem cRun_append (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) :
    ∀ (xs ys : List CI) (l st : List WVal),
      cRun host ar callee (xs ++ ys) l st = cseq host ar callee ys (cRun host ar callee xs l st) := by
  intro xs ys l st
  induction xs, l, st using cRun.induct host ar callee with
  | case1 l st =>
      rw [List.nil_append]
      conv => rhs; unfold cRun
      rfl
  | _ =>
      simp only [List.cons_append] <;>
      unfold cRun <;>
      simp_all [cseq]

theorem wRunF_ok_length (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) :
    ∀ (xs : List WInstr) (l st l' st' : List WVal),
      wRunF host ar callee xs l st = some (.ok l' st') → l'.length = l.length := by
  intro xs l st
  induction xs, l, st using wRunF.induct host ar callee <;>
    intro l' st' h <;>
    unfold wRunF at h <;>
    (repeat' (first | split at h | (simp only [Function.comp_def] at *))) <;>
    first
      | (cases h; done)
      | (cases h; simp; done)
      | (simp at h; done)
      | (exfalso; simp_all; omega)
      | (exfalso; solve_by_elim)
      | (simp_all; done)

/-- The locals after `setsW n` over the stack `st`. -/
def setTop : Nat → List WVal → List WVal → List WVal
  | 0, l, _ => l
  | k + 1, l, v :: st => setTop k (l.set k v) st
  | _ + 1, l, [] => l

theorem setsW_run (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) :
    ∀ (n : Nat) (rest : List CI) (l st : List WVal),
      cRun host ar callee (setsW n ++ rest) l st =
        if st.length < n then none
        else cRun host ar callee rest (setTop n l st) (st.drop n) := by
  intro n
  induction n with
  | zero => intro rest l st; simp [setsW, setTop]
  | succ k ih =>
      intro rest l st
      cases st with
      | nil =>
          simp only [setsW, List.cons_append]
          unfold cRun
          simp [wRunF]
      | cons v st =>
          simp only [setsW, List.cons_append]
          rw [cRun]
          simp only [wRunF]
          rw [ih]
          simp [setTop]

theorem setTop_eq : ∀ (n : Nat) (l st : List WVal), n ≤ l.length → n ≤ st.length →
    setTop n l st = (st.take n).reverse ++ l.drop n := by
  intro n
  induction n with
  | zero => intro l st _ _; simp [setTop]
  | succ k ih =>
      intro l st hl hs
      cases st with
      | nil => simp at hs
      | cons v st =>
          simp only [setTop]
          rw [ih _ _ (by simp; omega) (by simp at hs; omega)]
          rw [List.set_eq_take_append_cons_drop, if_pos (show k < l.length by omega)]
          simp only [List.take_succ_cons, List.reverse_cons, List.append_assoc,
            List.singleton_append]
          congr 1
          rw [List.drop_append_of_le_length (by simp; omega)]
          simp [List.drop_take_self]

/-- What a run of `convW self n d body` says about the run of `body`: a
    branch to the loop at depth `d` is the self tail call with the
    parameters it stored. -/
def simO (callee : Callee) (self n d : Nat) : Option COut → Option Out
  | some (.ok l st) => some (.ok l st)
  | some (.ret v) => some (.ret v)
  | some (.br d' l _) => if d' = d then (callee self (l.take n)).map Out.ret else none
  | none => none

theorem returnCall_rest (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (g : Nat) (rest : List WInstr) (l st : List WVal) :
    wRunF host ar callee (.returnCall g :: rest) l st =
      wRunF host ar callee [.returnCall g] l st := by
  unfold wRunF
  rfl

/-- The converted body runs as the original one does. -/
theorem conv_sim (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (self n : Nat) (har : ar self = some n) (d : Nat) (B : List WInstr) :
    ∀ (l st : List WVal), n ≤ l.length →
      wRunF host ar callee B l st =
        simO callee self n d (cRun host ar callee (convW self n d B) l st) := by
  induction d, B using convW.induct with
  | case1 d =>
      intro l st _
      simp only [convW]
      unfold cRun wRunF
      rfl
  | case2 d g rest ih =>
      intro l st hl
      simp only [convW]
      by_cases hg : g = self
      · subst hg
        simp only [if_true, List.append_assoc, List.singleton_append]
        rw [setsW_run, returnCall_rest]
        unfold wRunF
        simp only [har, popArgs]
        by_cases hs : st.length < n
        · simp [hs, simO]
        · simp only [hs, if_false]
          unfold cRun
          simp only [simO, if_true]
          rw [setTop_eq _ _ _ hl (by omega), List.take_left' (by simp; omega)]
          cases callee g (List.take n st).reverse <;> rfl
      · simp only [hg, if_false, List.singleton_append]
        rw [returnCall_rest]
        unfold cRun
        cases hw : wRunF host ar callee [.returnCall g] l st with
        | none => simp [simO]
        | some o =>
            cases o with
            | ret v => simp [simO]
            | ok l' st' =>
                exfalso
                unfold wRunF at hw
                repeat' split at hw
                all_goals simp_all
  | case3 d t e rest iht ihe ihr =>
      intro l st hl
      simp only [convW]
      unfold cRun wRunF
      cases st with
      | nil => simp [simO]
      | cons c st =>
          cases c with
          | i32v c =>
              simp only
              by_cases hc : c = 0
              · simp only [hc, if_true]
                rw [ihe l st hl]
                cases ho : cRun host ar callee (convW self n (d + 1) e) l st with
                | none => simp [simO]
                | some o =>
                    cases o with
                    | ok l' st' =>
                        have hl' : l'.length = l.length := by
                          have := ihe l st hl
                          rw [ho] at this
                          exact wRunF_ok_length host ar callee _ _ _ _ _ this
                        simp only [simO]
                        exact ihr l' st' (by omega)
                    | ret v => simp [simO]
                    | br d' l' st' =>
                        cases d' with
                        | zero => simp [simO]
                        | succ d' =>
                            by_cases hd : d' = d
                            · subst hd
                              simp only [simO, if_true]
                              cases callee self (List.take n l') <;> rfl
                            · simp [simO, hd]
              · simp only [hc, if_false]
                rw [iht l st hl]
                cases ho : cRun host ar callee (convW self n (d + 1) t) l st with
                | none => simp [simO]
                | some o =>
                    cases o with
                    | ok l' st' =>
                        have hl' : l'.length = l.length := by
                          have := iht l st hl
                          rw [ho] at this
                          exact wRunF_ok_length host ar callee _ _ _ _ _ this
                        simp only [simO]
                        exact ihr l' st' (by omega)
                    | ret v => simp [simO]
                    | br d' l' st' =>
                        cases d' with
                        | zero => simp [simO]
                        | succ d' =>
                            by_cases hd : d' = d
                            · subst hd
                              simp only [simO, if_true]
                              cases callee self (List.take n l') <;> rfl
                            · simp [simO, hd]
          | _ => simp [simO]
  | case4 d x rest h1 h2 ih =>
      intro l st hl
      rw [convW]
      · rw [show x :: rest = [x] ++ rest from rfl, wRunF_append]
        unfold cRun
        cases hw : wRunF host ar callee [x] l st with
        | none => simp [seqOut, simO]
        | some o =>
            cases o with
            | ok l' st' =>
                simp only [seqOut]
                exact ih l' st' (by rw [wRunF_ok_length host ar callee _ _ _ _ _ hw]; exact hl)
            | ret v => simp [seqOut, simO]
      · exact h1
      · exact h2

theorem proW_run (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) :
    ∀ (zs : List WInstr) (vs : List WVal), zs.mapM zeroVal = some vs →
      ∀ (j : Nat) (rest : List CI) (l st : List WVal),
        cRun host ar callee (proW j zs ++ rest) l st =
          cRun host ar callee rest (resetFrom j vs l) st := by
  intro zs
  induction zs with
  | nil => intro vs h j rest l st; simp at h; subst h; simp [proW, resetFrom]
  | cons z zs ih =>
      intro vs h j rest l st
      cases hv : zeroVal z with
      | none => simp [List.mapM_cons, hv] at h
      | some v =>
      cases hvs' : zs.mapM zeroVal with
      | none => simp [List.mapM_cons, hv, hvs'] at h
      | some vs' =>
      simp [List.mapM_cons, hv, hvs'] at h
      subst h
      simp only [proW, List.cons_append]
      have hz : wRunF host ar callee [z] l st = some (.ok l (v :: st)) := by
        unfold zeroVal at hv
        split at hv <;> simp at hv <;> subst hv <;> simp [wRunF]
      rw [cRun, hz]
      simp only
      rw [cRun]
      simp only [wRunF]
      rw [ih vs' hvs']
      rfl

theorem resetFrom_eq : ∀ (vs : List WVal) (j : Nat) (l : List WVal),
    l.length = j + vs.length → resetFrom j vs l = l.take j ++ vs := by
  intro vs
  induction vs with
  | nil => intro j l h; simp at h; simp [resetFrom, List.take_of_length_le (by omega : l.length ≤ j)]
  | cons v vs ih =>
      intro j l h
      simp only [resetFrom]
      rw [ih _ _ (by simp at h ⊢; omega)]
      rw [List.set_eq_take_append_cons_drop, if_pos (show j < l.length by simp at h; omega)]
      simp [List.take_append, List.take_take, Nat.min_eq_left (show j ≤ l.length by simp at h; omega)]

theorem cRun_length (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) :
    ∀ (xs : List CI) (l st : List WVal),
      (∀ l' st', cRun host ar callee xs l st = some (.ok l' st') → l'.length = l.length) ∧
      (∀ d l' st', cRun host ar callee xs l st = some (.br d l' st') → l'.length = l.length) := by
  intro xs l st
  induction xs, l, st using cRun.induct host ar callee with
  | case1 l st => constructor <;> intros <;> simp_all [cRun]
  | _ =>
      constructor <;> intros <;> rename_i h <;> rw [cRun] at h <;>
        (repeat' split at h) <;> simp_all <;>
        (try (subst_vars; rfl)) <;>
        exact wRunF_ok_length host ar callee _ _ _ _ _ (by assumption)

/-- The function result of a loop body's run. -/
def endLoop : Option Out → Option WVal
  | some (.ok _ (v :: _)) => some v
  | some (.ret v) => some v
  | _ => none

/-- The shape `GrammarLower.fnCodeAt` gives the code of a function `f` whose
    body tail-calls itself: its loop resets the declared locals to `vs` and
    runs the converted body, then returns. -/
structure LoopCode (c : WCode) (f : Nat) (zs : List WInstr) (vs : List WVal) : Prop where
  body : c.loopBody = some (proW c.arity zs ++ (convW f c.arity 0 c.body ++ [.w .ret]))
  zeros : zs.mapM zeroVal = some vs
  len : vs.length = c.nlocals

/-- One pass of the loop from parameters `p`. -/
def loopRun (code : CodeTbl) (host : HostTbl) (k f : Nat) (c : WCode) (vs p : List WVal) :
    Option WVal :=
  match cRun host (fun g => (code g).map (·.arity)) (fun g as => wFuncN code host k g as)
      (convW f c.arity 0 c.body ++ [.w .ret]) (p ++ vs) [] with
  | some (.ret v) => some v
  | some (.br 0 l _) => wEnter code host k f [] (some l)
  | _ => none

section Loop
variable {code : CodeTbl} {host : HostTbl} {f : Nat} {c : WCode} {zs : List WInstr}
  {vs : List WVal}

theorem wEnter_succ_loop (hc : code f = some c) (L : LoopCode c f zs vs) (k : Nat)
    (args : List WVal) (resume : Option (List WVal))
    (hlen : (resume.getD (initLocals c args)).length = c.arity + c.nlocals) :
    wEnter code host (k + 1) f args resume =
      loopRun code host k f c vs ((resume.getD (initLocals c args)).take c.arity) := by
  simp only [wEnter, hc, L.body]
  rw [proW_run _ _ _ zs vs L.zeros, resetFrom_eq _ _ _ (by rw [hlen, L.len])]
  rfl

theorem wEnter_resume (hc : code f = some c) (L : LoopCode c f zs vs) :
    ∀ (k : Nat) (l : List WVal), l.length = c.arity + c.nlocals →
      wEnter code host k f [] (some l) = wFuncN code host k f (l.take c.arity) := by
  intro k l hl
  cases k with
  | zero => simp [wEnter, wFuncN]
  | succ k =>
      unfold wFuncN
      have hi : (initLocals c (l.take c.arity)).length = c.arity + c.nlocals := by
        simp [initLocals]; omega
      rw [wEnter_succ_loop hc L k [] (some l) (by simpa using hl),
        wEnter_succ_loop hc L k _ none (by simpa using hi)]
      congr 1
      simp only [Option.getD_some, Option.getD_none, initLocals]
      rw [List.take_left' (by simp; omega)]

/-- A function with a self tail loop runs as its original body does, with
    every call (the self tail call included) made at the fuel below. -/
theorem wEnter_loop (hc : code f = some c) (L : LoopCode c f zs vs) (k : Nat)
    (args : List WVal) (resume : Option (List WVal))
    (hlen : (resume.getD (initLocals c args)).length = c.arity + c.nlocals) :
    wEnter code host (k + 1) f args resume =
      endLoop (wRunF host (fun g => (code g).map (·.arity))
        (fun g as => wFuncN code host k g as) c.body
        ((resume.getD (initLocals c args)).take c.arity ++ vs) []) := by
  rw [wEnter_succ_loop hc L k args resume hlen]
  have hp : ((resume.getD (initLocals c args)).take c.arity).length = c.arity := by
    simp; omega
  generalize (resume.getD (initLocals c args)).take c.arity = p at hp ⊢
  have har : (fun g => (code g).map (·.arity)) f = some c.arity := by simp [hc]
  rw [conv_sim host _ _ f c.arity har 0 c.body (p ++ vs) [] (by simp; omega)]
  unfold loopRun
  rw [cRun_append]
  have hlenB := (cRun_length host (fun g => (code g).map (·.arity))
    (fun g as => wFuncN code host k g as) (convW f c.arity 0 c.body) (p ++ vs) []).2
  cases ho : cRun host (fun g => (code g).map (·.arity)) (fun g as => wFuncN code host k g as)
      (convW f c.arity 0 c.body) (p ++ vs) [] with
  | none => rfl
  | some o =>
      cases o with
      | ok l' st' =>
          cases st' with
          | nil => simp [cseq, simO, endLoop, cRun, wRunF]
          | cons v st' => simp [cseq, simO, endLoop, cRun, wRunF]
      | ret v => rfl
      | br d l' st' =>
          cases d with
          | zero =>
              have hl' : l'.length = c.arity + c.nlocals := by
                rw [hlenB 0 l' st' ho, List.length_append, hp, L.len]
              simp only [cseq, simO, if_true]
              rw [wEnter_resume hc L k l' hl']
              cases wFuncN code host k f (l'.take c.arity) <;> rfl
          | succ d => simp [cseq, simO, endLoop]

/-- A function without a loop runs its body once, as before. -/
theorem wFuncN_plain (hc : code f = some c) (hl : c.loopBody = none) (k : Nat)
    (args : List WVal) :
    wFuncN code host (k + 1) f args =
      match wRunF host (fun g => (code g).map (·.arity))
          (fun g as => wFuncN code host k g as) c.body (initLocals c args) [] with
      | some (.ok _ [v]) => some v
      | some (.ret v) => some v
      | _ => none := by
  simp only [wFuncN, wEnter, hc, hl]
  rfl

end Loop

end InterpreterSequencing
