/- GrammarTotal — totality (level L3) for the one-grammar plan.

   Main's L3 (`Schema.Obligation.holdsTotal`, policy `simulatesModelTotally`)
   promises, under the partial contracts plus totality of the Int helpers the
   obligation's `totalityRole` selects (`.addSub`: add and sub; `.mul`: also
   mul), that the export returns a represented result of its model at fuel
   `n.natAbs + 1`, where `n` is the first (Int) argument. The termination
   evidence is the one canonical witness `{measure := .intNatAbs 0, descent :=
   -1}`, checked against the byte-bound plan: a floor guard `n <= 0` and the
   recursive argument `n - 1` (`Schema.checkTerm`, `checkTermMutual`).

   Here the same discipline is a decidable check over the plan, computed by
   the wall from the plan alone (`checkTermGroup`), and `fn_certified_total`
   proves the same promise for every member of a checked group: the wasm run
   at fuel `n.natAbs + 1` returns, and the fuel-indexed model at that fuel is
   defined and represented by the result.

   The admitted shape (every member of the group):

   * every parameter is `Int` (at least one), the result is `Int` or `Bool`;
   * the body is `if n <= 0 then base else step` over parameter 0 (the MIR
     `IfThenElse` the emitter lowers with the inline sign test);
   * `base` and `step` are built from Int / Bool literals, parameters, and
     `+`, `-`, `*` on Int; `step` may also
     call a member of the group, by `call` or `tailCall`, and every such call
     passes `n - 1` as its first argument; `step` makes at least one such
     call (a plan without recursion stays at L1, as on main).

   Against main this admits more (all of it proved below): other straight-line
   Int arithmetic in either arm, several member calls per arm, non-tail calls
   to other members of a mutual group, further Int parameters, and a Bool
   result. A literal multiplier (`k * f(n-1)`, recdecline's `wild`) is L3 at
   role `.mul` for every `k` the typing admits; main reached such a plan only
   when the producer's sampled i128 guard passed. A literal outside the i64
   band is declined by the typing (`tyOf`), so it never reaches this check.

   Runtime contracts: the partial ones `fn_certified_group` already takes,
   plus totality of box (the Int literal and the `n - 1` operand box an
   `i64`; on main box is the wall's own `boxRef`, see `boxRef_total`), add
   and sub, and mul when the group's role is `.mul` (a member multiplies). -/
import GrammarSound

set_option maxHeartbeats 4000000
set_option maxRecDepth 100000
set_option linter.unusedSectionVars false
set_option linter.unusedSimpArgs false

namespace AverCert.Grammar
open CertPrelude AverCert.Schema InterpreterSequencing

/-! ## The termination check -/

/-- The recursive argument `n - 1` over parameter 0. -/
def isDescent : Expr → Bool
  | .binOp .sub (.local i) (.literal (.int k)) => i == 0 && k == 1
  | _ => false

/-- The first argument of a member call is the descent. -/
def descentHead : List Expr → Bool
  | a :: _ => isDescent a
  | [] => false

mutual
  /-- The straight-line total fragment. `mem` is the group's member set,
      `mulOk` says the group's role admits `*`, and `calls` says member
      calls are admitted here (only in the step arm). -/
  def totE (mem : Nat → Bool) (mulOk calls : Bool) : Expr → Bool
    | .literal (.int _) => true
    | .literal (.bool _) => true
    | .local _ => true
    | .binOp op l r =>
        (op == .add || op == .sub || (op == .mul && mulOk)) &&
        totE mem mulOk calls l && totE mem mulOk calls r
    | .call (.fn g) args =>
        calls && mem g && descentHead args && totArgs mem mulOk calls args
    | .tailCall g args =>
        calls && mem g && descentHead args && totArgs mem mulOk calls args
    | _ => false
  def totArgs (mem : Nat → Bool) (mulOk calls : Bool) : List Expr → Bool
    | [] => true
    | e :: es => totE mem mulOk calls e && totArgs mem mulOk calls es
end

mutual
  /-- Some member call occurs. -/
  def hasCall : Expr → Bool
    | .binOp _ l r => hasCall l || hasCall r
    | .call (.fn _) _ => true
    | .tailCall _ _ => true
    | _ => false
end

mutual
  /-- Some Int multiplication occurs (the `.mul` totality role). -/
  def usesMul : Expr → Bool
    | .binOp op l r => op == .mul || usesMul l || usesMul r
    | .call _ args => usesMulArgs args
    | .tailCall _ args => usesMulArgs args
    | .ifThenElse c t e => usesMul c || usesMul t || usesMul e
    | _ => false
  def usesMulArgs : List Expr → Bool
    | [] => false
    | e :: es => usesMul e || usesMulArgs es
end

/-- The body: `if n <= 0 then base else step` over parameter 0. -/
def totBody (mem : Nat → Bool) (mulOk : Bool) : Expr → Bool
  | .ifThenElse (.binOp .lte (.local i) (.literal (.int k))) base step =>
      i == 0 && k == 0 && totE mem mulOk false base && totE mem mulOk true step &&
        hasCall step
  | _ => false

def isIntTy (t : Ty) : Bool := decide (t = .int)

/-- One member plan: Int parameters (at least one), an Int or Bool result,
    and a total body. -/
def totPlan (mem : Nat → Bool) (mulOk : Bool) (p : FnPlan) : Bool :=
  !p.sig.params.isEmpty && p.sig.params.all isIntTy &&
    (decide (p.sig.ret = .int) || decide (p.sig.ret = .bool)) &&
    totBody mem mulOk p.body

/-- The member set of a group given as `(function index, plan)` pairs. -/
def memOf (ms : List (Nat × FnPlan)) (g : Nat) : Bool := ms.any (·.1 == g)

/-- The group's totality role: `.mul` exactly when a member multiplies. -/
def groupRole (ms : List (Nat × FnPlan)) : TotalityRole :=
  if ms.any (fun m => usesMul m.2.body) then .mul else .addSub

/-- The wall's termination check for one group (an SCC, or one
    self-recursive function): every member passes `totPlan` over the group's
    member set, at the group's role. `some role` means L3 at that role. -/
def checkTermGroup (ms : List (Nat × FnPlan)) : Option TotalityRole :=
  if !ms.isEmpty && ms.all (fun m => totPlan (memOf ms) (groupRole ms == .mul) m.2) then
    some (groupRole ms)
  else none

/-- A single self-recursive function is the one-member group. -/
def checkTerm (self : Nat) (p : FnPlan) : Option TotalityRole := checkTermGroup [(self, p)]

/-- The canonical (and only) termination witness, as on main. -/
def canonicalWitness : TerminationWitness := { measure := .intNatAbs 0, descent := -1 }

/-- The claim axes of one group, derived from the plans alone: L3 with the
    canonical witness and the group's role when the check passes, else L1. -/
def groupPolicy (ms : List (Nat × FnPlan)) : Policy × Option TerminationWitness × TotalityRole :=
  match checkTermGroup ms with
  | some role => (.simulatesModelTotally, some canonicalWitness, role)
  | none => (.simulatesModel, none, .addSub)

/-- The group as a code-index map (first binding wins). -/
def groupOf (ms : List (Nat × FnPlan)) (f : Nat) : Option FnPlan :=
  (ms.find? (·.1 == f)).map (·.2)

/-! ## Check facts -/

theorem isDescent_eq {a : Expr} (h : isDescent a = true) :
    a = .binOp .sub (.local 0) (.literal (.int 1)) := by
  unfold isDescent at h
  split at h
  · simp only [Bool.and_eq_true, beq_iff_eq] at h
    obtain ⟨rfl, rfl⟩ := h
    rfl
  · cases h

theorem descentHead_eq {args : List Expr} (h : descentHead args = true) :
    ∃ rest, args = .binOp .sub (.local 0) (.literal (.int 1)) :: rest := by
  cases args with
  | nil => simp [descentHead] at h
  | cons a rest => exact ⟨rest, by rw [isDescent_eq h]⟩

theorem totBody_eq {mem : Nat → Bool} {mulOk : Bool} {b : Expr}
    (h : totBody mem mulOk b = true) :
    ∃ base step, b = .ifThenElse (.binOp .lte (.local 0) (.literal (.int 0))) base step ∧
      totE mem mulOk false base = true ∧ totE mem mulOk true step = true := by
  unfold totBody at h
  split at h
  · simp only [Bool.and_eq_true, beq_iff_eq] at h
    obtain ⟨⟨⟨⟨rfl, rfl⟩, hb⟩, hs⟩, _⟩ := h
    exact ⟨_, _, rfl, hb, hs⟩
  · cases h

theorem totPlan_spec {mem : Nat → Bool} {mulOk : Bool} {p : FnPlan}
    (h : totPlan mem mulOk p = true) :
    (∃ ps, p.sig.params = .int :: ps) ∧ (∀ t ∈ p.sig.params, t = .int) ∧
      (p.sig.ret = .int ∨ p.sig.ret = .bool) ∧ totBody mem mulOk p.body = true := by
  simp only [totPlan, Bool.and_eq_true, Bool.not_eq_true', List.isEmpty_eq_false_iff,
    List.all_eq_true, Bool.or_eq_true, decide_eq_true_eq, isIntTy] at h
  obtain ⟨⟨⟨hne, hall⟩, hret⟩, hb⟩ := h
  refine ⟨?_, hall, hret, hb⟩
  cases hps : p.sig.params with
  | nil => exact absurd hps hne
  | cons t ps =>
      have := hall t (by rw [hps]; exact List.mem_cons_self)
      exact ⟨ps, by rw [this]⟩

theorem checkTermGroup_spec {ms : List (Nat × FnPlan)} {role : TotalityRole}
    (h : checkTermGroup ms = some role) :
    role = groupRole ms ∧
      ∀ m ∈ ms, totPlan (memOf ms) (role == .mul) m.2 = true := by
  unfold checkTermGroup at h
  split at h
  · rename_i hc
    simp only [Option.some.injEq] at h
    subst h
    simp only [Bool.and_eq_true, Bool.not_eq_true', List.all_eq_true] at hc
    exact ⟨rfl, hc.2⟩
  · cases h

theorem groupOf_mem {ms : List (Nat × FnPlan)} {f : Nat} {p : FnPlan}
    (h : groupOf ms f = some p) : (f, p) ∈ ms := by
  unfold groupOf at h
  cases hf : ms.find? (·.1 == f) with
  | none => rw [hf] at h; cases h
  | some m =>
      rw [hf] at h
      simp only [Option.map_some, Option.some.injEq] at h
      have hm := List.mem_of_find?_eq_some hf
      have hk := List.find?_some hf
      simp only [beq_iff_eq] at hk
      subst h
      obtain ⟨a, b⟩ := m
      simp only at hk
      subst hk
      exact hm

theorem memOf_groupOf {ms : List (Nat × FnPlan)} {g : Nat} (h : memOf ms g = true) :
    ∃ p, groupOf ms g = some p := by
  unfold memOf at h
  unfold groupOf
  cases hf : ms.find? (·.1 == g) with
  | none =>
      rw [List.find?_eq_none] at hf
      obtain ⟨m, hm, hmg⟩ := List.any_eq_true.mp h
      exact absurd hmg (hf m hm)
  | some m => exact ⟨m.2, rfl⟩

theorem boxRef_total (C : Nat) (k : Int) : ∃ w, boxRef C [.i64v k] = some w :=
  ⟨_, rfl⟩

/-! ## Progress: a total-fragment node runs to completion

`agreement` says what a successful run means; the lemmas here say the run
succeeds. Each composite case runs its parts in order, taking the shape of
each intermediate frame from `agreement`. -/

theorem totE_binOp {mem : Nat → Bool} {mulOk calls : Bool} {op : BinOp} {l r : Expr}
    (h : totE mem mulOk calls (.binOp op l r) = true) :
    (op = .add ∨ op = .sub ∨ (op = .mul ∧ mulOk = true)) ∧
      totE mem mulOk calls l = true ∧ totE mem mulOk calls r = true := by
  simp only [totE, Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq] at h
  obtain ⟨⟨hop, hl⟩, hr⟩ := h
  refine ⟨?_, hl, hr⟩
  rcases hop with (h1 | h1) | h1
  · exact Or.inl h1
  · exact Or.inr (Or.inl h1)
  · exact Or.inr (Or.inr ⟨h1.1, h1.2⟩)

/-- A total-fragment node never has type `String` (so `+` is Int addition):
    parameters are Int and member results are Int or Bool. -/
theorem totE_notStr {M : MCtx} {n : Nat} {mem : Nat → Bool} {mulOk calls : Bool}
    (hSig : ∀ g sig, mem g = true → M.sigs g = some sig → sig.ret = .int ∨ sig.ret = .bool) :
    ∀ (e : Expr) (Γ : Nat → Option Ty) (tail : Bool),
      (∀ i T', Γ i = some T' → T' = .int) →
      totE mem mulOk calls e = true → tyOf M n Γ tail e ≠ some .string
  | .literal (.int k), Γ, tail, hΓ, _, h => by
      have := (tyOf_litInt_inv h).2; cases this
  | .literal (.bool v), Γ, tail, hΓ, _, h => by
      have := tyOf_litBool_inv h; cases this
  | .local i, Γ, tail, hΓ, _, h => by
      simp only [tyOf] at h
      have := hΓ i _ h; cases this
  | .binOp op l r, Γ, tail, hΓ, ht, h => by
      obtain ⟨_, hl, _⟩ := totE_binOp ht
      rcases tyOf_binOp_inv h with ⟨_, _, hT⟩ | ⟨_, _, _, hT⟩ | ⟨_, _, _, hT⟩ | ⟨htl, _, _⟩
      · split at hT <;> cases hT
      · cases hT
      · cases hT
      · exact totE_notStr hSig l Γ false hΓ hl htl
  | .call (.fn g) args, Γ, tail, hΓ, ht, h => by
      simp only [totE, Bool.and_eq_true] at ht
      obtain ⟨sig, hsig, _, hT⟩ := tyOf_callFn_inv h
      rcases hSig g sig ht.1.1.2 hsig with h2 | h2 <;> rw [h2] at hT <;> cases hT
  | .tailCall g args, Γ, tail, hΓ, ht, h => by
      simp only [totE, Bool.and_eq_true] at ht
      obtain ⟨_, sig, hsig, _, hT⟩ := tyOf_tailCall_inv h
      rcases hSig g sig ht.1.1.2 hsig with h2 | h2 <;> rw [h2] at hT <;> cases hT
  | .literal (.float _), _, _, _, ht, _ => by simp [totE] at ht
  | .literal (.str _), _, _, _, ht, _ => by simp [totE] at ht
  | .let_ _ _ _, _, _, _, ht, _ => by simp [totE] at ht
  | .call (.builtin _) _, _, _, _, ht, _ => by simp [totE] at ht
  | .call (.lazy _) _, _, _, _, ht, _ => by simp [totE] at ht
  | .neg _, _, _, _, ht, _ => by simp [totE] at ht
  | .ifThenElse _ _ _, _, _, _, ht, _ => by simp [totE] at ht
  | .recordCreate _ _, _, _, _, ht, _ => by simp [totE] at ht
  | .project _ _ _, _, _, _, ht, _ => by simp [totE] at ht
  | .match_ _ _, _, _, _, ht, _ => by simp [totE] at ht
  | .construct _ _ _, _, _, _, ht, _ => by simp [totE] at ht
  | .interp _, _, _, _, ht, _ => by simp [totE] at ht
  | .list _ _, _, _, _, ht, _ => by simp [totE] at ht

/-- The `n <= 0` guard over a represented Int in a local runs to its
    verdict (the inline sign test: no helper is called). -/
theorem guard_run {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (slot : Nat) (locals stack : List WVal) (n : Int) (w : WVal)
    (hget : locals[slot]? = some w)
    (hR : CanonRepr S n w) :
    wRunF host ar callee (eraseL (cmpArmB C slot .lte 0)) locals stack =
      some (.ok locals (b32 (decide (n ≤ 0)) :: stack)) := by
  rcases S.car n w hR.1 with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
  · have hs : s = n := S.smallElim n s sg hR.1
    subst hs
    simp [cmpArmB, eraseL, eraseI, bigCmpArm, smallCmpInstr, wRunF, hget, b32]
  · obtain ⟨hsign, hnz⟩ := S.bigElim n s lty les sg hR.1
    have hiff : (sg < 0) ↔ (n ≤ 0) := by
      constructor
      · intro h; have := hsign.mp h; omega
      · intro h; exact hsign.mpr (by omega)
    simp [cmpArmB, eraseL, eraseI, bigCmpArm, smallCmpInstr, wRunF, hget, b32, hiff]

section Progress
variable {C : Nat} (S : CarrierSpec C)
  (box add sub mul cmp eq neg : List WVal → Option WVal)
  (Ctr : Contracts S box add sub mul cmp eq)
  (hNegC : ∀ x w r, CanonRepr S x w → neg [w] = some r →
    CanonRepr S (-x) r)
  (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) (M : MCtx)
  (hCarrier : M.carrier = C)
  (hBox : host M.box = some (1, box)) (hAdd : host M.add = some (2, add))
  (hSub : host M.sub = some (2, sub)) (hMul : host M.mul = some (2, mul))
  (hNeg : host M.neg = some (1, neg))
  (hCmp : host M.cmp = some (2, cmp)) (hEq : host M.eq = some (2, eq))
  (R : XHost S M host)
  (F : Nat → List SVal → Option SVal)
  (hCallees : ∀ f sig, M.sigs f = some sig →
    Contract S M host ar callee f sig (F f))
  (X : LCtx)
  (hBoxT : ∀ k : Int, -(2 ^ 63 : Int) ≤ k → k < 2 ^ 63 → ∃ w, box [.i64v k] = some w)
  (hAddT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
  (hSubT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
  (mulOk : Bool)
  (hMulT : mulOk = true → ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, mul [va, vb] = some w)
  (mem : Nat → Bool) (calls : Bool) (n : Int)
  (hSig : ∀ g sig, mem g = true → M.sigs g = some sig → sig.ret = .int ∨ sig.ret = .bool)
  (hCallP : calls = true → ∀ g sig, mem g = true → M.sigs g = some sig →
    ∀ svs ws, HasTyL M (.i (n - 1) :: svs) sig.params →
      SReprL S M (.i (n - 1) :: svs) ws → ∃ r, callee g ws = some r)
include Ctr hNegC hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R hCallees hBoxT hAddT hSubT hMulT
  hSig hCallP

mutual
theorem progress :
    ∀ (e : Expr) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal),
      totE mem mulOk calls e = true →
      (∀ i T', Γ i = some T' → T' = .int) →
      tyOf M X.n Γ tail e = some T →
      EnvTy M env Γ →
      LRel S M X env wl →
      env 0 = some (.i n) →
      ∃ out, wRunF host ar callee (lowerW M X Γ tail e) wl st = some out
  | .literal (.int k), Γ, env, tail, T, wl, st, _, _, hty, _, _, _ => by
      obtain ⟨hband, _⟩ := tyOf_litInt_inv hty
      have hk : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
        simpa [inI64Band, Bool.and_eq_true, decide_eq_true_eq] using hband
      obtain ⟨w, hw⟩ := hBoxT k hk.1 hk.2
      simp [lowerW, lowerB, eraseL, eraseI, wRunF, hBox, popArgs, hw]
  | .literal (.bool v), Γ, env, tail, T, wl, st, _, _, _, _, _, _ => by
      simp [lowerW, lowerB, eraseL, eraseI, wRunF]
  | .local i, Γ, env, tail, T, wl, st, _, _, hty, henv, hl, _ => by
      simp only [tyOf] at hty
      obtain ⟨sv, hsv, _⟩ := envTy_get henv hty
      obtain ⟨_, w, hw, _⟩ := hl.2 i sv hsv
      simp [lowerW, lowerB, eraseL, eraseI, wRunF, hw]
  | .binOp op l r, Γ, env, tail, T, wl, st, htot, hΓ, hty, henv, hl, h0 => by
      obtain ⟨hop, htl0, htr0⟩ := totE_binOp htot
      have hA : op.isArith = true := by
        rcases hop with rfl | rfl | ⟨rfl, _⟩ <;> rfl
      rcases tyOf_binOp_inv hty with ⟨htl, htr, _⟩ | ⟨_, _, hq, _⟩ | ⟨_, _, hq, _⟩ |
        ⟨htl, _, _⟩
      · simp only [lowerW, lowerB, htl, hA, ↓reduceIte, eraseL_append, eraseL, eraseI]
        obtain ⟨o1, h1⟩ := progress l Γ env false .int wl st htl0 hΓ htl henv hl h0
        obtain ⟨sv1, _, hT1, hres1⟩ := agreement S box add sub mul cmp eq neg Ctr hNegC host ar
          callee M hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R F hCallees X l Γ env false .int
          wl st o1 htl henv hl h1
        obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
        obtain ⟨a, rfl⟩ := hasTy_int hT1
        obtain ⟨o2, h2⟩ := progress r Γ env false .int wl1 (w1 :: st) htr0 hΓ htr henv hl1 h0
        obtain ⟨sv2, _, hT2, hres2⟩ := agreement S box add sub mul cmp eq neg Ctr hNegC host ar
          callee M hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R F hCallees X r Γ env false .int
          wl1 (w1 :: st) o2 htr henv hl1 h2
        obtain ⟨wl2, w2, rfl, hw2, _⟩ := res_false hres2
        obtain ⟨b, rfl⟩ := hasTy_int hT2
        simp only [SRepr, CanonRepr] at hw1 hw2
        rw [wRunF_append, wRunF_append]
        simp only [lowerW] at h1 h2
        rw [h1]
        simp only [seqOut]
        rw [h2]
        simp only [seqOut]
        rcases hop with rfl | rfl | ⟨rfl, hm⟩
        · obtain ⟨w, hw⟩ := hAddT a b w1 w2 hw1.1 hw2.1
          simp [MCtx.arithIdx, wRunF, hAdd, popArgs_two, hw]
        · obtain ⟨w, hw⟩ := hSubT a b w1 w2 hw1.1 hw2.1
          simp [MCtx.arithIdx, wRunF, hSub, popArgs_two, hw]
        · obtain ⟨w, hw⟩ := hMulT hm a b w1 w2 hw1.1 hw2.1
          simp [MCtx.arithIdx, wRunF, hMul, popArgs_two, hw]
      · rcases hop with rfl | rfl | ⟨rfl, _⟩ <;> cases hq
      · rcases hop with rfl | rfl | ⟨rfl, _⟩ <;> cases hq
      · exact absurd htl (totE_notStr hSig l Γ false hΓ htl0)
  | .call (.fn g) args, Γ, env, tail, T, wl, st, htot, hΓ, hty, henv, hl, h0 => by
      simp only [totE, Bool.and_eq_true] at htot
      obtain ⟨⟨⟨hc, hmem⟩, hdh⟩, hargs⟩ := htot
      obtain ⟨sig, hsig, hts, rfl⟩ := tyOf_callFn_inv hty
      obtain ⟨hhost, har, _⟩ := hCallees g sig hsig
      simp only [lowerW, lowerB, eraseL_append]
      obtain ⟨o1, h1⟩ := progressArgs args Γ env sig.params wl st hargs hΓ hts henv hl h0
      obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, _⟩ :=
        agreementArgs S box add sub mul cmp eq neg Ctr hNegC host ar callee M hCarrier hBox hAdd
          hSub hMul hNeg hCmp hEq R F hCallees X args Γ env sig.params wl st o1 hts henv hl h1
      obtain ⟨rest, rfl⟩ := descentHead_eq hdh
      have hd : eval F env (.binOp .sub (.local 0) (.literal (.int 1))) = some (.i (n - 1)) := by
        simp [eval, h0, intBin]
      simp only [evalArgs, hd] at hevs
      cases hrs : evalArgs F env rest with
      | none => rw [hrs] at hevs; cases hevs
      | some svs' =>
          rw [hrs] at hevs
          simp only [Option.some.injEq] at hevs
          subst hevs
          obtain ⟨r, hr⟩ := hCallP hc g sig hmem hsig svs' ws hTs hrep
          have hlen : sig.params.length = ws.length := by
            rw [← hasTyL_length hTs, sreprL_length hrep]
          rw [wRunF_append]
          simp only [lowerArgsW] at h1
          rw [h1]
          simp [seqOut, eraseL, eraseI, wRunF, hhost, har, hlen, popArgs_rev, hr]
  | .tailCall g args, Γ, env, tail, T, wl, st, htot, hΓ, hty, henv, hl, h0 => by
      simp only [totE, Bool.and_eq_true] at htot
      obtain ⟨⟨⟨hc, hmem⟩, hdh⟩, hargs⟩ := htot
      obtain ⟨_, sig, hsig, hts, rfl⟩ := tyOf_tailCall_inv hty
      obtain ⟨_, har, _⟩ := hCallees g sig hsig
      simp only [lowerW, lowerB, eraseL_append]
      obtain ⟨o1, h1⟩ := progressArgs args Γ env sig.params wl st hargs hΓ hts henv hl h0
      obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, _⟩ :=
        agreementArgs S box add sub mul cmp eq neg Ctr hNegC host ar callee M hCarrier hBox hAdd
          hSub hMul hNeg hCmp hEq R F hCallees X args Γ env sig.params wl st o1 hts henv hl h1
      obtain ⟨rest, rfl⟩ := descentHead_eq hdh
      have hd : eval F env (.binOp .sub (.local 0) (.literal (.int 1))) = some (.i (n - 1)) := by
        simp [eval, h0, intBin]
      simp only [evalArgs, hd] at hevs
      cases hrs : evalArgs F env rest with
      | none => rw [hrs] at hevs; cases hevs
      | some svs' =>
          rw [hrs] at hevs
          simp only [Option.some.injEq] at hevs
          subst hevs
          obtain ⟨r, hr⟩ := hCallP hc g sig hmem hsig svs' ws hTs hrep
          have hlen : sig.params.length = ws.length := by
            rw [← hasTyL_length hTs, sreprL_length hrep]
          rw [wRunF_append]
          simp only [lowerArgsW] at h1
          rw [h1]
          simp [seqOut, eraseL, eraseI, wRunF, har, hlen, popArgs_rev, hr]
  | .literal (.float _), _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .literal (.str _), _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .let_ _ _ _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .call (.builtin _) _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .call (.lazy _) _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .neg _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .ifThenElse _ _ _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .recordCreate _ _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .project _ _ _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .match_ _ _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .construct _ _ _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .interp _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht
  | .list _ _, _, _, _, _, _, _, ht, _, _, _, _, _ => by simp [totE] at ht

theorem progressArgs :
    ∀ (es : List Expr) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (Ts : List Ty)
      (wl st : List WVal),
      totArgs mem mulOk calls es = true →
      (∀ i T', Γ i = some T' → T' = .int) →
      tysOf M X.n Γ es = some Ts →
      EnvTy M env Γ →
      LRel S M X env wl →
      env 0 = some (.i n) →
      ∃ out, wRunF host ar callee (lowerArgsW M X Γ es) wl st = some out
  | [], Γ, env, Ts, wl, st, _, _, _, _, _, _ => by
      simp [lowerArgsW, lowerArgsB, eraseL, wRunF]
  | e :: es, Γ, env, Ts, wl, st, htot, hΓ, hty, henv, hl, h0 => by
      simp only [totArgs, Bool.and_eq_true] at htot
      obtain ⟨t, ts, hte, htes, rfl⟩ := tysOf_cons_inv hty
      simp only [lowerArgsW, lowerArgsB, eraseL_append]
      obtain ⟨o1, h1⟩ := progress e Γ env false t wl st htot.1 hΓ hte henv hl h0
      obtain ⟨sv, _, _, hres⟩ := agreement S box add sub mul cmp eq neg Ctr hNegC host ar
        callee M hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R F hCallees X e Γ env false t
        wl st o1 hte henv hl h1
      obtain ⟨wl1, w, rfl, _, hl1⟩ := res_false hres
      obtain ⟨o2, h2⟩ := progressArgs es Γ env ts wl1 (w :: st) htot.2 hΓ htes henv hl1 h0
      rw [wRunF_append]
      simp only [lowerW] at h1
      rw [h1]
      simp only [lowerArgsW] at h2
      exact ⟨_, by simp only [seqOut]; exact h2⟩
end

end Progress

/-! ## Function level: a checked group returns at fuel `n.natAbs + 1` -/

/-- The callee contracts of a group at one fuel level: members through their
    `FnCertified`, functions outside the group through the outer ones. -/
theorem groupCallees {C : Nat} (S : CarrierSpec C) (M : MCtx) (code : CodeTbl) (host : HostTbl)
    (G : Nat → Option FnPlan) (outer : Nat → Nat → List SVal → Option SVal)
    (hOuter : ∀ f sig, M.sigs f = some sig → G f = none →
      FnCertified S M code host f sig (fun fuel => outer fuel f))
    (hSigOf : ∀ f p, G f = some p → M.sigs f = some p.sig)
    (hCert : ∀ f p, G f = some p →
      FnCertified S M code host f p.sig (fun fuel => groupModel outer G fuel f))
    (k : Nat) :
    ∀ g sig, M.sigs g = some sig →
      Contract S M host (fun g => (code g).map (·.arity))
        (fun g as => wFuncN code host k g as) g sig (groupModel outer G k g) := by
  intro g sig hsig
  cases hg : G g with
  | some p' =>
      have hs := hSigOf g p' hg
      rw [hsig] at hs
      cases hs
      exact (hCert g p' hg).contract k
  | none =>
      have hc := (hOuter g sig hsig hg).contract k
      refine ⟨hc.1, hc.2.1, ?_⟩
      intro svs' ws' r' hT' hr' hc'
      obtain ⟨sv, hm, hsv, hT⟩ := hc.2.2 svs' ws' r' hT' hr' hc'
      exact ⟨sv, by rw [groupModel_outer outer G hg]; exact hm, hsv, hT⟩

/-- The total face of one function (main's `holdsTotal` shape over the
    plan's model): every well-typed represented input has an Int first
    argument `n`, the run at fuel `n.natAbs + 1` returns, and the model at
    that fuel is defined and represented by the result. -/
def FnTotal {C : Nat} (S : CarrierSpec C) (M : MCtx)
    (code : CodeTbl) (host : HostTbl) (f : Nat) (sig : Sig)
    (model : Nat → List SVal → Option SVal) : Prop :=
  ∀ svs ws, HasTyL M svs sig.params → SReprL S M svs ws →
    ∃ n tl, svs = .i n :: tl ∧ ∃ r sv, wFuncN code host (n.natAbs + 1) f ws = some r ∧
      model (n.natAbs + 1) svs = some sv ∧ SRepr S M sv r ∧ HasTy M sv sig.ret

theorem paramsΓ_int {ts : List Ty} (h : ∀ t ∈ ts, t = .int) :
    ∀ i T', paramsΓ ts i = some T' → T' = .int := by
  intro i T' hi
  exact h T' (List.mem_of_getElem? hi)

/-- The group theorem at L3: every member of a group whose plans pass the
    termination check is certified (`FnCertified`, as `fn_certified_group`)
    and total (`FnTotal`), under the partial contracts plus totality of box,
    add and sub, and of mul when `mulOk` (the group's role is `.mul`). -/
theorem fn_certified_total {C : Nat} (S : CarrierSpec C)
    (box add sub mul cmp eq neg : List WVal → Option WVal)
    (Ctr : Contracts S box add sub mul cmp eq)
    (hNegC : ∀ x w r, CanonRepr S x w → neg [w] = some r →
      CanonRepr S (-x) r)
    (code : CodeTbl) (host : HostTbl) (M : MCtx)
    (hCarrier : M.carrier = C)
    (hBox : host M.box = some (1, box)) (hAdd : host M.add = some (2, add))
    (hSub : host M.sub = some (2, sub)) (hMul : host M.mul = some (2, mul))
    (hNeg : host M.neg = some (1, neg))
    (hCmp : host M.cmp = some (2, cmp)) (hEq : host M.eq = some (2, eq))
    (R : XHost S M host)
    (G : Nat → Option FnPlan) (outer : Nat → Nat → List SVal → Option SVal)
    (hOuter : ∀ f sig, M.sigs f = some sig → G f = none →
      FnCertified S M code host f sig (fun fuel => outer fuel f))
    (hMem : ∀ f p, G f = some p →
      M.sigs f = some p.sig ∧ planTyped M p = true ∧ host f = none ∧
        code f = some (fnCode M p))
    (hBoxT : ∀ k : Int, -(2 ^ 63 : Int) ≤ k → k < 2 ^ 63 → ∃ w, box [.i64v k] = some w)
    (hAddT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
    (hSubT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
    (mulOk : Bool)
    (hMulT : mulOk = true → ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, mul [va, vb] = some w)
    (mem : Nat → Bool) (hmem : ∀ g, mem g = true → ∃ p, G g = some p)
    (hTot : ∀ f p, G f = some p → totPlan mem mulOk p = true) :
    ∀ f p, G f = some p →
      FnCertified S M code host f p.sig (fun fuel => groupModel outer G fuel f) ∧
        FnTotal S M code host f p.sig (fun fuel => groupModel outer G fuel f) := by
  have hCert := fn_certified_group S box add sub mul cmp eq neg Ctr hNegC code host M hCarrier
    hBox hAdd hSub hMul hNeg hCmp hEq R G outer hOuter hMem
  have hSigOf : ∀ f p, G f = some p → M.sigs f = some p.sig := fun f p h => (hMem f p h).1
  have hSig : ∀ g sig, mem g = true → M.sigs g = some sig → sig.ret = .int ∨ sig.ret = .bool := by
    intro g sig hg hs
    obtain ⟨p, hp⟩ := hmem g hg
    have := hSigOf g p hp
    rw [hs] at this
    cases this
    exact (totPlan_spec (hTot g p hp)).2.2.1
  -- One fuel level, given every member returns one level below.
  have key : ∀ m,
      (∀ f p, G f = some p → ∀ n tl ws, HasTyL M (.i n :: tl) p.sig.params →
        SReprL S M (.i n :: tl) ws → n.natAbs + 1 = m → ∃ r, wFuncN code host m f ws = some r) →
      ∀ f p, G f = some p → ∀ n tl ws, HasTyL M (.i n :: tl) p.sig.params →
        SReprL S M (.i n :: tl) ws → n.natAbs = m →
        ∃ r, wFuncN code host (m + 1) f ws = some r := by
    intro m ih f p hG n tl ws hTs hrep hm
    obtain ⟨_, htyped, _, hcode⟩ := hMem f p hG
    obtain ⟨⟨ps, hps⟩, hall, _, hbody⟩ := totPlan_spec (hTot f p hG)
    obtain ⟨base, step, hb, hbase, hstep⟩ := totBody_eq hbody
    simp only [planTyped, Bool.and_eq_true, decide_eq_true_eq] at htyped
    obtain ⟨⟨hpn, hnl⟩, hty⟩ := htyped
    obtain ⟨hXn, hXc, hXs, hXsc⟩ := FnPlan.lctx_spec p
    rw [← hXn] at hty
    have hCallees := groupCallees S M code host G outer hOuter hSigOf hCert m
    have hlenw : ws.length = p.sig.params.length := by
      rw [← sreprL_length hrep, hasTyL_length hTs]
    have hLR : LRel S M p.lctx (argsEnv (.i n :: tl)) (initLocals (fnCode M p) ws) := by
      refine ⟨by simp [initLocals, fnCode]; omega, ?_⟩
      intro i sv hs
      rw [hXn]
      obtain ⟨w, hw, hsv⟩ := sreprL_get hrep hs
      have hi : i < ws.length := by
        rcases Nat.lt_or_ge i ws.length with h | h
        · exact h
        · rw [List.getElem?_eq_none h] at hw; simp at hw
      refine ⟨by omega, w, ?_, hsv⟩
      simp [initLocals, List.getElem?_append_left hi, hw]
    have henv := envTy_args hTs
    have hΓ := paramsΓ_int hall
    have h0 : argsEnv (.i n :: tl) 0 = some (.i n) := rfl
    obtain ⟨w0, ws', rfl, hw0, _⟩ := sreprL_cons_inv hrep
    have hget : (initLocals (fnCode M p) (w0 :: ws'))[0]? = some w0 := by
      simp [initLocals]
    have hΓ0 : paramsΓ p.sig.params 0 = some .int := by simp [paramsΓ, hps]
    -- the body runs to some outcome
    have hrun : ∃ out, wRunF host (fun g => (code g).map (·.arity))
        (fun g as => wFuncN code host m g as) (fnCode M p).body
        (initLocals (fnCode M p) (w0 :: ws')) [] = some out := by
      have hbodyEq : (fnCode M p).body = eraseL (cmpArmB C 0 .lte 0) ++
          [WInstr.ifElse (eraseL (lowerB M p.lctx (paramsΓ p.sig.params) true base))
            (eraseL (lowerB M p.lctx (paramsΓ p.sig.params) true step))] := by
        simp only [fnCode, hb, lowerW, lowerB, hΓ0, tyOf, BinOp.isArith, Bool.false_eq_true,
          ↓reduceIte, litInt?, slot?, eraseL_append, eraseL, eraseI, hCarrier]
      rw [hbodyEq, wRunF_append, guard_run S _ _ _ 0 _ [] n w0 hget hw0]
      simp only [seqOut, b32]
      rw [wRunF_ifElse_single]
      by_cases hn : n ≤ 0
      · simp only [hn, decide_true, ↓reduceIte, Int.reduceEq]
        have hbty := (tyOf_ite_inv (hb ▸ hty)).2.1
        exact progress (S := S) (box := box) (add := add) (sub := sub) (mul := mul) (cmp := cmp)
          (eq := eq) (neg := neg) (Ctr := Ctr) (hNegC := hNegC) (host := host)
          (ar := fun g => (code g).map (·.arity)) (callee := fun g as => wFuncN code host m g as)
          (M := M) (hCarrier := hCarrier) (hBox := hBox) (hAdd := hAdd) (hSub := hSub)
          (hMul := hMul) (hNeg := hNeg) (hCmp := hCmp) (hEq := hEq) (R := R)
          (F := groupModel outer G m) (hCallees := hCallees) (X := p.lctx) (hBoxT := hBoxT)
          (hAddT := hAddT) (hSubT := hSubT) (mulOk := mulOk) (hMulT := hMulT) (mem := mem)
          (calls := false) (n := n) (hSig := hSig) (hCallP := fun h => by cases h)
          base _ _ true p.sig.ret _ [] hbase hΓ hbty henv hLR h0
      · simp only [hn, decide_false, Bool.false_eq_true, ↓reduceIte]
        have hsty := (tyOf_ite_inv (hb ▸ hty)).2.2
        have hCallP : true = true → ∀ g sig, mem g = true → M.sigs g = some sig →
            ∀ svs ws, HasTyL M (.i (n - 1) :: svs) sig.params →
              SReprL S M (.i (n - 1) :: svs) ws →
              ∃ r, (fun g as => wFuncN code host m g as) g ws = some r := by
          intro _ g sig hg hs svs ws2 hT2 hr2
          obtain ⟨p', hp'⟩ := hmem g hg
          have := hSigOf g p' hp'
          rw [hs] at this
          cases this
          exact ih g p' hp' (n - 1) svs ws2 hT2 hr2 (by omega)
        exact progress (S := S) (box := box) (add := add) (sub := sub) (mul := mul) (cmp := cmp)
          (eq := eq) (neg := neg) (Ctr := Ctr) (hNegC := hNegC) (host := host)
          (ar := fun g => (code g).map (·.arity)) (callee := fun g as => wFuncN code host m g as)
          (M := M) (hCarrier := hCarrier) (hBox := hBox) (hAdd := hAdd) (hSub := hSub)
          (hMul := hMul) (hNeg := hNeg) (hCmp := hCmp) (hEq := hEq) (R := R)
          (F := groupModel outer G m) (hCallees := hCallees) (X := p.lctx) (hBoxT := hBoxT)
          (hAddT := hAddT) (hSubT := hSubT) (mulOk := mulOk) (hMulT := hMulT) (mem := mem)
          (calls := true) (n := n) (hSig := hSig) (hCallP := hCallP)
          step _ _ true p.sig.ret _ [] hstep hΓ hsty henv hLR h0
    obtain ⟨out, hout⟩ := hrun
    obtain ⟨sv, _, _, hres⟩ := agreement S box add sub mul cmp eq neg Ctr hNegC host
      (fun g => (code g).map (·.arity)) (fun g as => wFuncN code host m g as) M
      hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R (groupModel outer G m) hCallees
      p.lctx p.body (paramsΓ p.sig.params) (argsEnv (.i n :: tl)) true p.sig.ret
      (initLocals (fnCode M p) (w0 :: ws')) [] out hty henv hLR hout
    unfold wFuncN
    rw [hcode]
    simp only
    rw [hout]
    cases out with
    | ok wl' st' =>
        obtain ⟨w, rfl, _, _⟩ := hres
        exact ⟨w, rfl⟩
    | ret w => exact ⟨w, rfl⟩
  have term : ∀ m f p, G f = some p → ∀ n tl ws, HasTyL M (.i n :: tl) p.sig.params →
      SReprL S M (.i n :: tl) ws → n.natAbs = m →
      ∃ r, wFuncN code host (m + 1) f ws = some r := by
    intro m
    induction m with
    | zero => exact key 0 (fun _ _ _ _ _ _ _ _ h => by omega)
    | succ k ih =>
        exact key (k + 1) (fun f p hG n tl ws hT hr h => ih f p hG n tl ws hT hr (by omega))
  intro f p hG
  refine ⟨hCert f p hG, ?_⟩
  intro svs ws hTs hrep
  obtain ⟨⟨ps, hps⟩, _, _, _⟩ := totPlan_spec (hTot f p hG)
  rw [hps] at hTs
  obtain ⟨v, tl, rfl, hv, _⟩ := hasTyL_cons_inv hTs
  obtain ⟨n, rfl⟩ := hasTy_int hv
  rw [← hps] at hTs
  obtain ⟨r, hr⟩ := term n.natAbs f p hG n tl ws hTs hrep rfl
  obtain ⟨sv, hm, hsv, hT⟩ := (hCert f p hG).2.2 (n.natAbs + 1) _ ws r hTs hrep hr
  exact ⟨n, tl, rfl, r, sv, hr, hm, hsv, hT⟩

/-- `fn_certified_total` for a group the wall's check accepts: `G` is any
    code-index map whose bindings are exactly the group's pairs, and the
    role the check returns selects the mul totality premise. -/
theorem fn_certified_total_of_check {C : Nat} (S : CarrierSpec C)
    (box add sub mul cmp eq neg : List WVal → Option WVal)
    (Ctr : Contracts S box add sub mul cmp eq)
    (hNegC : ∀ x w r, CanonRepr S x w → neg [w] = some r →
      CanonRepr S (-x) r)
    (code : CodeTbl) (host : HostTbl) (M : MCtx)
    (hCarrier : M.carrier = C)
    (hBox : host M.box = some (1, box)) (hAdd : host M.add = some (2, add))
    (hSub : host M.sub = some (2, sub)) (hMul : host M.mul = some (2, mul))
    (hNeg : host M.neg = some (1, neg))
    (hCmp : host M.cmp = some (2, cmp)) (hEq : host M.eq = some (2, eq))
    (R : XHost S M host)
    (ms : List (Nat × FnPlan)) (role : TotalityRole) (hck : checkTermGroup ms = some role)
    (G : Nat → Option FnPlan)
    (hGin : ∀ f p, G f = some p → (f, p) ∈ ms) (hGall : ∀ m ∈ ms, G m.1 = some m.2)
    (outer : Nat → Nat → List SVal → Option SVal)
    (hOuter : ∀ f sig, M.sigs f = some sig → G f = none →
      FnCertified S M code host f sig (fun fuel => outer fuel f))
    (hMem : ∀ f p, G f = some p →
      M.sigs f = some p.sig ∧ planTyped M p = true ∧ host f = none ∧
        code f = some (fnCode M p))
    (hBoxT : ∀ k : Int, -(2 ^ 63 : Int) ≤ k → k < 2 ^ 63 → ∃ w, box [.i64v k] = some w)
    (hAddT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
    (hSubT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
    (hMulT : role = .mul → ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, mul [va, vb] = some w) :
    ∀ f p, G f = some p →
      FnCertified S M code host f p.sig (fun fuel => groupModel outer G fuel f) ∧
        FnTotal S M code host f p.sig (fun fuel => groupModel outer G fuel f) := by
  obtain ⟨_, hall⟩ := checkTermGroup_spec hck
  refine fn_certified_total S box add sub mul cmp eq neg Ctr hNegC code host M hCarrier hBox hAdd
    hSub hMul hNeg hCmp hEq R G outer hOuter hMem hBoxT hAddT hSubT (role == .mul)
    (fun h => hMulT (by simpa using h)) (memOf ms) ?_ ?_
  · intro g hg
    obtain ⟨m, hm, hmg⟩ := List.any_eq_true.mp hg
    simp only [beq_iff_eq] at hmg
    subst hmg
    exact ⟨m.2, hGall m hm⟩
  · intro f p hG
    exact hall (f, p) (hGin f p hG)

end AverCert.Grammar
