/- GrammarSound — the simulation theorem for the one-grammar plan (P2a, not
   yet wired).

   ONE statement, `agreement`, by structural induction over `Grammar.Expr`:
   a successful run of the lowered instructions in the audited interpreter
   means the source semantics succeeds with a value the run represents. It
   depends on no particular function body. Calls cite a `Contract` for the
   callee, never its body; `fn_certified_group` supplies those contracts for
   a whole group of functions (one SCC: self and mutual calls) by induction
   on the interpreter fuel, and takes functions outside the group as
   `FnCertified` hypotheses from earlier groups.

   Representation: Int is a canonical carrier word (`CanonRepr`, as in the
   existing wall), Bool is `i32` 0/1, a record is the struct of its type with
   pointwise represented fields. The locals relation `LRel` constrains only
   the slots the source environment defines, and all of them sit below the
   resolver slot count `n`; the const-compare scratch local at `n` is
   therefore free for the stash template to overwrite. -/
import GrammarLower
import InterpreterSequencing

set_option maxHeartbeats 4000000
set_option maxRecDepth 100000
set_option linter.unusedSectionVars false
set_option linter.unusedSimpArgs false

namespace AverCert.Grammar
open CertPrelude AverCert.Schema InterpreterSequencing

/-! ## Representation and relations -/

mutual
  def SRepr {C : Nat} (S : CarrierSpec C) (so : Nat → Nat) : SVal → WVal → Prop
    | .i n, w => RecordComputeBridge.CanonRepr S n w
    | .b v, w => w = b32 v
    | .record tid fs, w => ∃ ws, w = .structv (so tid) ws ∧ SReprL S so fs ws
  def SReprL {C : Nat} (S : CarrierSpec C) (so : Nat → Nat) :
      List SVal → List WVal → Prop
    | [], [] => True
    | v :: vs, w :: ws => SRepr S so v w ∧ SReprL S so vs ws
    | _, _ => False
end

/-- The source environment agrees with the typing environment. -/
def EnvTy (R : Nat → Option (List Ty)) (env : Nat → Option SVal) (Γ : Nat → Option Ty) :
    Prop :=
  ∀ i, (env i = none ∧ Γ i = none) ∨
    ∃ v T, env i = some v ∧ Γ i = some T ∧ HasTy R v T

section Rel
variable {C : Nat} (S : CarrierSpec C) (so : Nat → Nat) (n : Nat)

/-- Locals relation: the scratch slot `n` exists, and every defined source
    slot sits below `n` and is represented at its own wasm local. -/
def LRel (env : Nat → Option SVal) (wl : List WVal) : Prop :=
  n < wl.length ∧
    ∀ i v, env i = some v → i < n ∧ ∃ w, wl[i]? = some w ∧ SRepr S so v w

/-- What a node's run looks like: a normal run pushes exactly one represented
    value on the untouched stack and keeps the locals relation; a `.ret` (a
    `return_call`) happens only in tail position. -/
def Res (tail : Bool) (env : Nat → Option SVal) (st : List WVal) (sv : SVal) : Out → Prop
  | .ok wl' st' => ∃ w, st' = w :: st ∧ SRepr S so sv w ∧ LRel S so n env wl'
  | .ret w => tail = true ∧ SRepr S so sv w

end Rel

/-- Assume–guarantee contract of a code function `f` at signature `sig` for
    one opaque `callee`: the ONLY thing a caller knows about `f`. -/
def Contract {C : Nat} (S : CarrierSpec C) (so : Nat → Nat) (R : Nat → Option (List Ty))
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (f : Nat) (sig : Sig) (model : List SVal → Option SVal) : Prop :=
  host f = none ∧ ar f = some sig.params.length ∧
  ∀ svs ws r, HasTyL R svs sig.params → SReprL S so svs ws → callee f ws = some r →
    ∃ sv, model svs = some sv ∧ SRepr S so sv r ∧ HasTy R sv sig.ret

/-- The certificate face of one function, with a fuel-indexed model. -/
def FnCertified {C : Nat} (S : CarrierSpec C) (so : Nat → Nat) (R : Nat → Option (List Ty))
    (code : CodeTbl) (host : HostTbl) (f : Nat) (sig : Sig)
    (model : Nat → List SVal → Option SVal) : Prop :=
  host f = none ∧ (code f).map (·.arity) = some sig.params.length ∧
  ∀ fuel svs ws r, HasTyL R svs sig.params → SReprL S so svs ws →
    wFuncN code host fuel f ws = some r →
    ∃ sv, model fuel svs = some sv ∧ SRepr S so sv r ∧ HasTy R sv sig.ret

theorem FnCertified.contract {C : Nat} {S : CarrierSpec C} {so : Nat → Nat}
    {R : Nat → Option (List Ty)} {code : CodeTbl} {host : HostTbl} {f : Nat} {sig : Sig}
    {model : Nat → List SVal → Option SVal}
    (h : FnCertified S so R code host f sig model) (fuel : Nat) :
    Contract S so R host (fun g => (code g).map (·.arity))
      (fun g as => wFuncN code host fuel g as) f sig (model fuel) :=
  ⟨h.1, h.2.1, fun svs ws r hT hr hc => h.2.2 fuel svs ws r hT hr hc⟩

/-! ## Small lemmas -/

section Small
variable {R : Nat → Option (List Ty)}

theorem hasTy_int {v : SVal} (h : HasTy R v .int) : ∃ n, v = .i n := by
  cases v <;> simp_all [HasTy]

theorem hasTy_bool {v : SVal} (h : HasTy R v .bool) : ∃ b, v = .b b := by
  cases v <;> simp_all [HasTy]

theorem hasTy_record {v : SVal} {tid : Nat} (h : HasTy R v (.record tid)) :
    ∃ fs fts, v = .record tid fs ∧ R tid = some fts ∧ HasTyL R fs fts := by
  cases v with
  | i _ => simp [HasTy] at h
  | b _ => simp [HasTy] at h
  | record tid' fs =>
      simp only [HasTy] at h
      obtain ⟨rfl, fts, hR, hfs⟩ := h
      exact ⟨fs, fts, rfl, hR, hfs⟩

theorem hasTyL_length : ∀ {vs : List SVal} {ts : List Ty}, HasTyL R vs ts →
    vs.length = ts.length
  | [], [], _ => rfl
  | _ :: _, _ :: _, h => by simp [hasTyL_length h.2]
  | [], _ :: _, h => by simp [HasTyL] at h
  | _ :: _, [], h => by simp [HasTyL] at h

theorem hasTyL_get : ∀ {vs : List SVal} {ts : List Ty}, HasTyL R vs ts →
    ∀ {i : Nat} {t : Ty}, ts[i]? = some t → ∃ v, vs[i]? = some v ∧ HasTy R v t
  | [], [], _, i, t, ht => by simp at ht
  | v :: vs, t' :: ts, h, i, t, ht => by
      cases i with
      | zero =>
          simp only [List.getElem?_cons_zero, Option.some.injEq] at ht
          subst ht
          exact ⟨v, rfl, h.1⟩
      | succ i =>
          simp only [List.getElem?_cons_succ] at ht ⊢
          exact hasTyL_get h.2 ht
  | [], _ :: _, h, _, _, _ => by simp [HasTyL] at h
  | _ :: _, [], h, _, _, _ => by simp [HasTyL] at h

theorem hasTyL_get' : ∀ {vs : List SVal} {ts : List Ty}, HasTyL R vs ts →
    ∀ {i : Nat} {v : SVal}, vs[i]? = some v → ∃ t, ts[i]? = some t ∧ HasTy R v t
  | [], [], _, i, v, hv => by simp at hv
  | v' :: vs, t :: ts, h, i, v, hv => by
      cases i with
      | zero =>
          simp only [List.getElem?_cons_zero, Option.some.injEq] at hv
          subst hv
          exact ⟨t, rfl, h.1⟩
      | succ i =>
          simp only [List.getElem?_cons_succ] at hv ⊢
          exact hasTyL_get' h.2 hv
  | [], _ :: _, h, _, _, _ => by simp [HasTyL] at h
  | _ :: _, [], h, _, _, _ => by simp [HasTyL] at h

end Small

section SmallRepr
variable {C : Nat} {S : CarrierSpec C} {so : Nat → Nat}

theorem sreprL_length : ∀ {vs : List SVal} {ws : List WVal}, SReprL S so vs ws →
    vs.length = ws.length
  | [], [], _ => rfl
  | _ :: _, _ :: _, h => by simp [sreprL_length h.2]
  | [], _ :: _, h => by simp [SReprL] at h
  | _ :: _, [], h => by simp [SReprL] at h

theorem sreprL_get : ∀ {vs : List SVal} {ws : List WVal}, SReprL S so vs ws →
    ∀ {i : Nat} {v : SVal}, vs[i]? = some v → ∃ w, ws[i]? = some w ∧ SRepr S so v w
  | [], [], _, i, v, hv => by simp at hv
  | v' :: vs, w :: ws, h, i, v, hv => by
      cases i with
      | zero =>
          simp only [List.getElem?_cons_zero, Option.some.injEq] at hv
          subst hv
          exact ⟨w, rfl, h.1⟩
      | succ i =>
          simp only [List.getElem?_cons_succ] at hv ⊢
          exact sreprL_get h.2 hv
  | [], _ :: _, h, _, _, _ => by simp [SReprL] at h
  | _ :: _, [], h, _, _, _ => by simp [SReprL] at h

theorem srepr_b {v : Bool} {w : WVal} (h : SRepr S so (.b v) w) : w = b32 v := by
  simpa [SRepr] using h

end SmallRepr

theorem popArgs_rev {ws st : List WVal} :
    popArgs ws.length (ws.reverse ++ st) = some (ws, st) := by
  simp [popArgs]

theorem popArgs_one (a : WVal) (st : List WVal) : popArgs 1 (a :: st) = some ([a], st) := by
  simp [popArgs]

theorem popArgs_two (a b : WVal) (st : List WVal) :
    popArgs 2 (b :: a :: st) = some ([a, b], st) := by
  simp [popArgs]

theorem run_split {host : HostTbl} {ar : Nat → Option Nat} {callee : Callee}
    {xs ys : List WInstr} {l st : List WVal} {out : Out}
    (h : wRunF host ar callee (xs ++ ys) l st = some out) :
    ∃ o, wRunF host ar callee xs l st = some o ∧
      seqOut host ar callee ys (some o) = some out := by
  rw [wRunF_append] at h
  cases hx : wRunF host ar callee xs l st with
  | none => rw [hx] at h; simp [seqOut] at h
  | some o => exact ⟨o, rfl, by rw [hx] at h; exact h⟩

/-- Running a lone `if` on an i32 condition is running the chosen branch. -/
theorem wRunF_ifElse_single (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (tB eB : List WInstr) (l : List WVal) (c : Int) (st : List WVal) :
    wRunF host ar callee [.ifElse tB eB] l (.i32v c :: st) =
      if c = 0 then wRunF host ar callee eB l st else wRunF host ar callee tB l st := by
  by_cases hc : c = 0
  · simp only [wRunF, hc, ite_true]
    cases h : wRunF host ar callee eB l st with
    | none => rfl
    | some o => cases o <;> simp
  · simp only [wRunF, hc, ite_false]
    cases h : wRunF host ar callee tB l st with
    | none => rfl
    | some o => cases o <;> simp

theorem upd_same {α : Type} (f : Nat → Option α) (i : Nat) (a : α) : upd f i a i = some a := by
  simp [upd]

theorem upd_ne {α : Type} (f : Nat → Option α) {i j : Nat} (a : α) (h : j ≠ i) :
    upd f i a j = f j := by
  simp [upd, h]

/-! ## Locals-relation maintenance -/

section LRelLemmas
variable {C : Nat} {S : CarrierSpec C} {so : Nat → Nat} {n : Nat}

/-- Writing a slot the source environment does not define keeps the relation
    (the stash into the scratch local). -/
theorem lrel_set_free {env : Nat → Option SVal} {wl : List WVal} {j : Nat} (w : WVal)
    (hl : LRel S so n env wl) (hj : n ≤ j) : LRel S so n env (wl.set j w) := by
  refine ⟨by simpa using hl.1, ?_⟩
  intro i v hv
  obtain ⟨hi, w', hw', hr⟩ := hl.2 i v hv
  refine ⟨hi, w', ?_, hr⟩
  rw [List.getElem?_set_ne (by omega)]
  exact hw'

/-- Binding a fresh slot `b < n` on both sides keeps the relation. -/
theorem lrel_bind {env : Nat → Option SVal} {wl : List WVal} {b : Nat} {v : SVal} {w : WVal}
    (hl : LRel S so n env wl) (hb : b < n) (hr : SRepr S so v w) :
    LRel S so n (upd env b v) (wl.set b w) := by
  refine ⟨by simpa using hl.1, ?_⟩
  intro i v' hv'
  by_cases hib : i = b
  · subst hib
    rw [upd_same] at hv'
    cases hv'
    refine ⟨hb, w, ?_, hr⟩
    rw [List.getElem?_set_self (by have := hl.1; omega)]
  · rw [upd_ne _ _ hib] at hv'
    obtain ⟨hi, w', hw', hr'⟩ := hl.2 i v' hv'
    refine ⟨hi, w', ?_, hr'⟩
    rw [List.getElem?_set_ne (fun h => hib h.symm)]
    exact hw'

/-- A relation for an extended environment implies the one for the original
    when the new slot was unbound. -/
theorem lrel_of_upd {env : Nat → Option SVal} {wl : List WVal} {b : Nat} {v : SVal}
    (hfree : env b = none) (hl : LRel S so n (upd env b v) wl) : LRel S so n env wl := by
  refine ⟨hl.1, ?_⟩
  intro i v' hv'
  have hib : i ≠ b := by
    intro h; subst h; rw [hfree] at hv'; cases hv'
  exact hl.2 i v' (by rw [upd_ne _ _ hib]; exact hv')

theorem res_of_upd {tail : Bool} {env : Nat → Option SVal} {st : List WVal} {sv : SVal}
    {out : Out} {b : Nat} {v : SVal} (hfree : env b = none)
    (h : Res S so n tail (upd env b v) st sv out) : Res S so n tail env st sv out := by
  cases out with
  | ok wl' st' =>
      obtain ⟨w, h1, h2, h3⟩ := h
      exact ⟨w, h1, h2, lrel_of_upd hfree h3⟩
  | ret w => exact h

theorem res_ok {tail : Bool} {env : Nat → Option SVal} {st wl' : List WVal} {sv : SVal}
    {w : WVal} (h : SRepr S so sv w) (hl : LRel S so n env wl') :
    Res S so n tail env st sv (.ok wl' (w :: st)) :=
  ⟨w, rfl, h, hl⟩

/-- A non-tail node never returns: its run is a normal frame. -/
theorem res_false {env : Nat → Option SVal} {st : List WVal} {sv : SVal} {out : Out}
    (h : Res S so n false env st sv out) :
    ∃ wl' w, out = .ok wl' (w :: st) ∧ SRepr S so sv w ∧ LRel S so n env wl' := by
  cases out with
  | ok wl' st' =>
      obtain ⟨w, rfl, hw, hl⟩ := h
      exact ⟨wl', w, rfl, hw, hl⟩
  | ret w => exact absurd h.1 (by simp)

end LRelLemmas

theorem envTy_upd {R : Nat → Option (List Ty)} {env : Nat → Option SVal}
    {Γ : Nat → Option Ty} {b : Nat} {v : SVal} {T : Ty}
    (h : EnvTy R env Γ) (hv : HasTy R v T) : EnvTy R (upd env b v) (upd Γ b T) := by
  intro i
  by_cases hib : i = b
  · subst hib
    exact Or.inr ⟨v, T, upd_same _ _ _, upd_same _ _ _, hv⟩
  · rw [upd_ne _ _ hib, upd_ne _ _ hib]
    exact h i

theorem envTy_get {R : Nat → Option (List Ty)} {env : Nat → Option SVal}
    {Γ : Nat → Option Ty} {i : Nat} {T : Ty}
    (h : EnvTy R env Γ) (hT : Γ i = some T) : ∃ v, env i = some v ∧ HasTy R v T := by
  rcases h i with ⟨_, h2⟩ | ⟨v, T', h1, h2, h3⟩
  · rw [hT] at h2; cases h2
  · rw [hT] at h2; cases h2; exact ⟨v, h1, h3⟩

theorem envTy_free {R : Nat → Option (List Ty)} {env : Nat → Option SVal}
    {Γ : Nat → Option Ty} {i : Nat}
    (h : EnvTy R env Γ) (hT : Γ i = none) : env i = none := by
  rcases h i with ⟨h1, _⟩ | ⟨v, T', h1, h2, h3⟩
  · exact h1
  · rw [hT] at h2; cases h2

/-! ## Comparison semantics and the two comparison templates -/

/-- The source meaning of the six comparisons (arithmetic operators map to
    `false`; they never reach this function). -/
def cmpDen : BinOp → Int → Int → Bool
  | .eq, x, y => decide (x = y)
  | .neq, x, y => decide (x ≠ y)
  | .lt, x, y => decide (x < y)
  | .gt, x, y => decide (x > y)
  | .lte, x, y => decide (x ≤ y)
  | .gte, x, y => decide (x ≥ y)
  | _, _, _ => false

theorem intBin_cmp {op : BinOp} (h : op.isArith = false) (x y : Int) :
    intBin op x y = .b (cmpDen op x y) := by
  cases op <;> simp_all [intBin, cmpDen, BinOp.isArith]

theorem cmpDen_flip (op : BinOp) (x y : Int) : cmpDen op.flip x y = cmpDen op y x := by
  cases op <;> simp [cmpDen, BinOp.flip, eq_comm]

theorem flip_isArith {op : BinOp} (h : op.isArith = false) : op.flip.isArith = false := by
  cases op <;> simp_all [BinOp.flip, BinOp.isArith]

/-- One lemma for BOTH literal-comparison templates: the re-emit template
    reads the bare local's own slot, the stash template the scratch slot. -/
theorem cmpArm_step {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (op : BinOp) (hop : op.isArith = false) (k : Int) (slot : Nat)
    (locals stack : List WVal) (n : Int) (w : WVal) (out : Out)
    (hband : AverCert.PlanCheck.inI64Band k = true)
    (hget : locals[slot]? = some w)
    (hR : RecordComputeBridge.CanonRepr S n w)
    (hrun : wRunF host ar callee (eraseL (cmpArmB C slot op k)) locals stack = some out) :
    out = .ok locals (b32 (cmpDen op n k) :: stack) := by
  have hk : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
    simpa [AverCert.PlanCheck.inI64Band, Bool.and_eq_true, decide_eq_true_eq] using hband
  rcases S.car n w hR.1 with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
  · have hs : s = n := S.smallElim n s sg hR.1
    subst hs
    cases op <;> simp [BinOp.isArith] at hop <;>
      · simp [cmpArmB, eraseL, eraseI, bigCmpArm, smallCmpInstr, wRunF, hget, b32] at hrun
        subst hrun
        simp [cmpDen, b32]
  · obtain ⟨hnb, hsgne⟩ := S.canonBig n s lty les sg hR.1 hR.2
    obtain ⟨hsign, _hnz⟩ := S.bigElim n s lty les sg hR.1
    have hLtIff : (sg < 0) ↔ n < k := by
      constructor
      · intro h; have := hsign.mp h; omega
      · intro h; exact hsign.mpr (by omega)
    have hGtIff : (0 < sg) ↔ k < n := by
      constructor
      · intro h
        have hnn : ¬ n < 0 := by intro hc; have := hsign.mpr hc; omega
        omega
      · intro h
        have hnn : ¬ sg < 0 := by intro hc; have := hsign.mp hc; omega
        omega
    have hNe : ¬ n = k := by
      intro he; subst he; omega
    have hLe : (n ≤ k) = (n < k) := by
      simp only [eq_iff_iff]
      constructor
      · intro h; omega
      · intro h; omega
    have hGe : (k ≤ n) = (k < n) := by
      simp only [eq_iff_iff]
      constructor
      · intro h; omega
      · intro h; omega
    cases op <;> simp [BinOp.isArith] at hop <;>
      · simp [cmpArmB, eraseL, eraseI, bigCmpArm, smallCmpInstr, wRunF, hget, b32] at hrun
        subst hrun
        simp [cmpDen, b32, hLtIff, hGtIff, hNe, hLe, hGe]

section Templates
variable {C : Nat} (S : CarrierSpec C)
  (box add sub mul cmp eq : List WVal → Option WVal)
  (Ctr : RecordComputeBridge.Contracts S box add sub mul cmp eq)
  (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) (M : MCtx)
include Ctr

/-- Int against Int without a literal: `__aint_cmp` against `0`, or
    `__aint_eq` (and `i32.eqz`). -/
theorem intCmpTail_step
    (hCmp : host M.cmp = some (2, cmp)) (hEq : host M.eq = some (2, eq))
    (op : BinOp) (hop : op.isArith = false) (a b : Int) (wa wb : WVal)
    (ha : RecordComputeBridge.CanonRepr S a wa) (hb : RecordComputeBridge.CanonRepr S b wb)
    (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee (intCmpTail M op) wl (wb :: wa :: st) = some out) :
    out = .ok wl (b32 (cmpDen op a b) :: st) := by
  cases op <;> simp [BinOp.isArith] at hop
  · -- eq
    cases hr : eq [wa, wb] with
    | none => simp [intCmpTail, wRunF, hEq, popArgs_two, hr] at hrun
    | some r =>
        have hr' := Ctr.hEq a b wa wb r ha.1 hb.1 ha.2 hb.2 hr
        subst hr'
        simp [intCmpTail, wRunF, hEq, popArgs_two, hr] at hrun
        subst hrun
        by_cases h : a = b <;> simp [cmpDen, eqW, b32, h]
  · -- neq
    cases hr : eq [wa, wb] with
    | none => simp [intCmpTail, wRunF, hEq, popArgs_two, hr] at hrun
    | some r =>
        have hr' := Ctr.hEq a b wa wb r ha.1 hb.1 ha.2 hb.2 hr
        subst hr'
        simp [intCmpTail, wRunF, hEq, popArgs_two, hr] at hrun
        subst hrun
        by_cases h : a = b <;> simp [cmpDen, eqW, b32, h]
  all_goals
    cases hr : cmp [wa, wb] with
    | none => simp [intCmpTail, wRunF, hCmp, popArgs_two, hr] at hrun
    | some r =>
        have hr' := Ctr.hCmp a b wa wb r ha.1 hb.1 ha.2 hb.2 hr
        subst hr'
        simp [intCmpTail, wRunF, hCmp, popArgs_two, hr] at hrun
        subst hrun
        unfold cmpW
        by_cases h1 : a < b
        · simp [cmpDen, b32, h1] <;> omega
        · by_cases h2 : a = b
          · subst h2; simp [cmpDen, b32]
          · simp [cmpDen, b32, h1, h2] <;> omega

/-- Int arithmetic through the named helper contracts. -/
theorem arith_step (so : Nat → Nat) (R : Nat → Option (List Ty))
    (hAdd : host M.add = some (2, add)) (hSub : host M.sub = some (2, sub))
    (hMul : host M.mul = some (2, mul))
    (op : BinOp) (hop : op.isArith = true) (a b : Int) (wa wb : WVal)
    (ha : RecordComputeBridge.CanonRepr S a wa) (hb : RecordComputeBridge.CanonRepr S b wb)
    (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee [.call (M.arithIdx op)] wl (wb :: wa :: st) = some out) :
    ∃ w, out = .ok wl (w :: st) ∧ SRepr S so (intBin op a b) w ∧
      HasTy R (intBin op a b) .int := by
  cases op <;> simp [BinOp.isArith] at hop
  · cases hr : add [wa, wb] with
    | none => simp [MCtx.arithIdx, wRunF, hAdd, popArgs_two, hr] at hrun
    | some r =>
        simp [MCtx.arithIdx, wRunF, hAdd, popArgs_two, hr] at hrun
        exact ⟨r, hrun.symm, by simp only [intBin, SRepr]; exact Ctr.hAdd a b wa wb r ha.1 hb.1 hr,
          by simp [intBin, HasTy]⟩
  · cases hr : sub [wa, wb] with
    | none => simp [MCtx.arithIdx, wRunF, hSub, popArgs_two, hr] at hrun
    | some r =>
        simp [MCtx.arithIdx, wRunF, hSub, popArgs_two, hr] at hrun
        exact ⟨r, hrun.symm, by simp only [intBin, SRepr]; exact Ctr.hSub a b wa wb r ha.1 hb.1 hr,
          by simp [intBin, HasTy]⟩
  · cases hr : mul [wa, wb] with
    | none => simp [MCtx.arithIdx, wRunF, hMul, popArgs_two, hr] at hrun
    | some r =>
        simp [MCtx.arithIdx, wRunF, hMul, popArgs_two, hr] at hrun
        exact ⟨r, hrun.symm, by simp only [intBin, SRepr]; exact Ctr.hMul a b wa wb r ha.1 hb.1 hr,
          by simp [intBin, HasTy]⟩

end Templates

theorem boolCmp_step (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (op : BinOp) (hop : op.isEquality = true) (x y : Bool) (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee [boolCmpInstr op] wl (b32 y :: b32 x :: st) = some out) :
    ∃ v, boolBin op x y = some (.b v) ∧ out = .ok wl (b32 v :: st) := by
  cases op <;> simp [BinOp.isEquality] at hop
  · refine ⟨x == y, rfl, ?_⟩
    cases x <;> cases y <;> simp [boolCmpInstr, wRunF, b32] at hrun ⊢ <;>
      exact hrun.symm
  · refine ⟨x != y, rfl, ?_⟩
    cases x <;> cases y <;> simp [boolCmpInstr, wRunF, b32] at hrun ⊢ <;>
      exact hrun.symm

theorem hasTyL_cons_inv {R : Nat → Option (List Ty)} {svs : List SVal} {t : Ty}
    {ts : List Ty} (h : HasTyL R svs (t :: ts)) :
    ∃ v vs, svs = v :: vs ∧ HasTy R v t ∧ HasTyL R vs ts := by
  cases svs with
  | nil => simp [HasTyL] at h
  | cons v vs => exact ⟨v, vs, rfl, h.1, h.2⟩

theorem hasTyL_nil_inv {R : Nat → Option (List Ty)} {svs : List SVal}
    (h : HasTyL R svs []) : svs = [] := by
  cases svs with
  | nil => rfl
  | cons _ _ => simp [HasTyL] at h

theorem sreprL_cons_inv {C : Nat} {S : CarrierSpec C} {so : Nat → Nat} {v : SVal}
    {vs : List SVal} {ws : List WVal} (h : SReprL S so (v :: vs) ws) :
    ∃ w ws', ws = w :: ws' ∧ SRepr S so v w ∧ SReprL S so vs ws' := by
  cases ws with
  | nil => simp [SReprL] at h
  | cons w ws' => exact ⟨w, ws', rfl, h.1, h.2⟩

theorem sreprL_nil_inv {C : Nat} {S : CarrierSpec C} {so : Nat → Nat} {ws : List WVal}
    (h : SReprL S so [] ws) : ws = [] := by
  cases ws with
  | nil => rfl
  | cons _ _ => simp [SReprL] at h

theorem builtin_step (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    {C : Nat} {S : CarrierSpec C} {so : Nat → Nat} {R : Nat → Option (List Ty)}
    (bi : Builtin) (ts : List Ty) (T : Ty) (hty : builtinTy bi ts = some T)
    (svs : List SVal) (ws : List WVal) (hT : HasTyL R svs ts) (hr : SReprL S so svs ws)
    (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee [builtinInstr bi] wl (ws.reverse ++ st) = some out) :
    ∃ v, builtinEval bi svs = some (.b v) ∧ T = .bool ∧ out = .ok wl (b32 v :: st) := by
  unfold builtinTy at hty
  split at hty
  · simp only [Option.some.injEq] at hty
    subst hty
    obtain ⟨a, svs1, rfl, ha, hT1⟩ := hasTyL_cons_inv hT
    obtain ⟨b, svs2, rfl, hb, hT2⟩ := hasTyL_cons_inv hT1
    have := hasTyL_nil_inv hT2; subst this
    obtain ⟨wa, ws1, rfl, hwa, hr1⟩ := sreprL_cons_inv hr
    obtain ⟨wb, ws2, rfl, hwb, hr2⟩ := sreprL_cons_inv hr1
    have := sreprL_nil_inv hr2; subst this
    obtain ⟨x, rfl⟩ := hasTy_bool ha
    obtain ⟨y, rfl⟩ := hasTy_bool hb
    have hwa' := srepr_b hwa
    have hwb' := srepr_b hwb
    subst hwa' hwb'
    refine ⟨x && y, rfl, rfl, ?_⟩
    cases x <;> cases y <;> simp [builtinInstr, wRunF, b32] at hrun ⊢ <;> exact hrun.symm
  · simp only [Option.some.injEq] at hty
    subst hty
    obtain ⟨a, svs1, rfl, ha, hT1⟩ := hasTyL_cons_inv hT
    obtain ⟨b, svs2, rfl, hb, hT2⟩ := hasTyL_cons_inv hT1
    have := hasTyL_nil_inv hT2; subst this
    obtain ⟨wa, ws1, rfl, hwa, hr1⟩ := sreprL_cons_inv hr
    obtain ⟨wb, ws2, rfl, hwb, hr2⟩ := sreprL_cons_inv hr1
    have := sreprL_nil_inv hr2; subst this
    obtain ⟨x, rfl⟩ := hasTy_bool ha
    obtain ⟨y, rfl⟩ := hasTy_bool hb
    have hwa' := srepr_b hwa
    have hwb' := srepr_b hwb
    subst hwa' hwb'
    refine ⟨x || y, rfl, rfl, ?_⟩
    cases x <;> cases y <;> simp [builtinInstr, wRunF, b32] at hrun ⊢ <;> exact hrun.symm
  · simp only [Option.some.injEq] at hty
    subst hty
    obtain ⟨a, svs1, rfl, ha, hT1⟩ := hasTyL_cons_inv hT
    have := hasTyL_nil_inv hT1; subst this
    obtain ⟨wa, ws1, rfl, hwa, hr1⟩ := sreprL_cons_inv hr
    have := sreprL_nil_inv hr1; subst this
    obtain ⟨x, rfl⟩ := hasTy_bool ha
    have hwa' := srepr_b hwa
    subst hwa'
    refine ⟨!x, rfl, rfl, ?_⟩
    cases x <;> simp [builtinInstr, wRunF, b32] at hrun ⊢ <;> exact hrun.symm
  · simp at hty

/-! ## Typing inversions -/

section TypingInv
variable {M : MCtx} {n : Nat} {Γ : Nat → Option Ty} {tail : Bool}

theorem tyOf_litInt_inv {k : Int} {T : Ty}
    (h : tyOf M n Γ tail (.literal (.int k)) = some T) :
    AverCert.PlanCheck.inI64Band k = true ∧ T = .int := by
  simp only [tyOf] at h
  split at h <;> simp_all

theorem tyOf_litBool_inv {v : Bool} {T : Ty}
    (h : tyOf M n Γ tail (.literal (.bool v)) = some T) : T = .bool := by
  simp only [tyOf, Option.some.injEq] at h
  exact h.symm

theorem tyOf_let_inv {b : Nat} {v body : Expr} {T : Ty}
    (h : tyOf M n Γ tail (.let_ b v body) = some T) :
    b < n ∧ Γ b = none ∧ ∃ Tv, tyOf M n Γ false v = some Tv ∧
      tyOf M n (upd Γ b Tv) tail body = some T := by
  simp only [tyOf] at h
  split at h
  · rename_i hc
    split at h
    · rename_i Tv hv
      exact ⟨hc.1, hc.2, Tv, hv, h⟩
    · simp at h
  · simp at h

theorem tyOf_callFn_inv {f : Nat} {args : List Expr} {T : Ty}
    (h : tyOf M n Γ tail (.call (.fn f) args) = some T) :
    ∃ sig, M.sigs f = some sig ∧ tysOf M n Γ args = some sig.params ∧ T = sig.ret := by
  simp only [tyOf] at h
  split at h
  · rename_i sig ts hs hts
    split at h
    · simp_all
    · simp at h
  · simp at h

theorem tyOf_callBuiltin_inv {bi : Builtin} {args : List Expr} {T : Ty}
    (h : tyOf M n Γ tail (.call (.builtin bi) args) = some T) :
    ∃ ts, tysOf M n Γ args = some ts ∧ builtinTy bi ts = some T := by
  simp only [tyOf] at h
  split at h
  · rename_i ts hts
    exact ⟨ts, hts, h⟩
  · simp at h

theorem tyOf_tailCall_inv {f : Nat} {args : List Expr} {T : Ty}
    (h : tyOf M n Γ tail (.tailCall f args) = some T) :
    tail = true ∧ ∃ sig, M.sigs f = some sig ∧ tysOf M n Γ args = some sig.params ∧
      T = sig.ret := by
  simp only [tyOf] at h
  split at h
  · rename_i ht
    refine ⟨ht, ?_⟩
    split at h
    · rename_i sig ts hs hts
      split at h
      · simp_all
      · simp at h
    · simp at h
  · simp at h

theorem tyOf_binOp_inv {op : BinOp} {l r : Expr} {T : Ty}
    (h : tyOf M n Γ tail (.binOp op l r) = some T) :
    (tyOf M n Γ false l = some .int ∧ tyOf M n Γ false r = some .int ∧
      T = if op.isArith then .int else .bool) ∨
    (tyOf M n Γ false l = some .bool ∧ tyOf M n Γ false r = some .bool ∧
      op.isEquality = true ∧ T = .bool) := by
  simp only [tyOf] at h
  split at h
  · rename_i hl hr
    left
    refine ⟨hl, hr, ?_⟩
    split at h <;> simp_all
  · rename_i hl hr
    right
    split at h
    · simp_all
    · simp at h
  · simp at h

theorem tyOf_neg_inv {e : Expr} {T : Ty}
    (h : tyOf M n Γ tail (.neg e) = some T) : tyOf M n Γ false e = some .int ∧ T = .int := by
  simp only [tyOf] at h
  split at h <;> simp_all

theorem tyOf_ite_inv {c t e : Expr} {T : Ty}
    (h : tyOf M n Γ tail (.ifThenElse c t e) = some T) :
    tyOf M n Γ false c = some .bool ∧ tyOf M n Γ tail t = some T ∧
      tyOf M n Γ tail e = some T := by
  simp only [tyOf] at h
  split at h
  · split at h <;> simp_all
  · simp at h

theorem tyOf_rec_inv {tid : Nat} {fs : List Expr} {T : Ty}
    (h : tyOf M n Γ tail (.recordCreate tid fs) = some T) :
    ∃ fts, M.recFields tid = some fts ∧ tysOf M n Γ fs = some fts ∧ T = .record tid := by
  simp only [tyOf] at h
  split at h
  · rename_i fts ts hR hts
    split at h
    · rename_i hc
      simp only [Option.some.injEq] at h
      exact ⟨fts, hR, hc.2 ▸ hts, h.symm⟩
    · simp at h
  · simp at h

theorem tyOf_proj_inv {tid i : Nat} {base : Expr} {T : Ty}
    (h : tyOf M n Γ tail (.project tid i base) = some T) :
    tyOf M n Γ false base = some (.record tid) ∧
      ∃ fts, M.recFields tid = some fts ∧ fts[i]? = some T := by
  simp only [tyOf] at h
  split at h
  · rename_i tid' fts hb hR
    split at h
    · rename_i hc
      obtain ⟨rfl, _⟩ := hc
      exact ⟨hb, fts, hR, h⟩
    · simp at h
  · simp at h

theorem tysOf_cons_inv {e : Expr} {es : List Expr} {Ts : List Ty}
    (h : tysOf M n Γ (e :: es) = some Ts) :
    ∃ t ts, tyOf M n Γ false e = some t ∧ tysOf M n Γ es = some ts ∧ Ts = t :: ts := by
  simp only [tysOf] at h
  split at h <;> simp_all

theorem tysOf_length : ∀ {es : List Expr} {ts : List Ty}, tysOf M n Γ es = some ts →
    ts.length = es.length
  | [], ts, h => by simp [tysOf] at h; subst h; rfl
  | e :: es, ts, h => by
      obtain ⟨t, ts', _, hts, rfl⟩ := tysOf_cons_inv h
      simp [tysOf_length hts]

end TypingInv

theorem litInt?_some {e : Expr} {k : Int} (h : litInt? e = some k) : e = .literal (.int k) := by
  unfold litInt? at h
  split at h <;> simp_all

theorem slot?_some {e : Expr} {i : Nat} (h : slot? e = some i) : e = .local i := by
  unfold slot? at h
  split at h <;> simp_all

/-! ## The agreement theorem

ONE statement, by structural induction on the grammar (mutual over the nested
argument lists). The context fixes: the carrier specification and the named
host contracts at their indices, an arbitrary opaque `callee`, and a
`Contract` for every callee the typing admits. -/

section Agreement
variable {C : Nat} (S : CarrierSpec C)
  (box add sub mul cmp eq neg : List WVal → Option WVal)
  (Ctr : RecordComputeBridge.Contracts S box add sub mul cmp eq)
  (hNegC : ∀ x w r, RecordComputeBridge.CanonRepr S x w → neg [w] = some r →
    RecordComputeBridge.CanonRepr S (-x) r)
  (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) (M : MCtx)
  (hCarrier : M.carrier = C)
  (hBox : host M.box = some (1, box)) (hAdd : host M.add = some (2, add))
  (hSub : host M.sub = some (2, sub)) (hMul : host M.mul = some (2, mul))
  (hNeg : host M.neg = some (1, neg))
  (hCmp : host M.cmp = some (2, cmp)) (hEq : host M.eq = some (2, eq))
  (F : Nat → List SVal → Option SVal)
  (hCallees : ∀ f sig, M.sigs f = some sig →
    Contract S M.structOf M.recFields host ar callee f sig (F f))
  (n : Nat)
include Ctr hNegC hCarrier hBox hAdd hSub hMul hNeg hCmp hEq hCallees

mutual
theorem agreement :
    ∀ (e : Expr) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal) (out : Out),
      tyOf M n Γ tail e = some T →
      EnvTy M.recFields env Γ →
      LRel S M.structOf n env wl →
      wRunF host ar callee (lowerW M n Γ tail e) wl st = some out →
      ∃ sv, eval F env e = some sv ∧ HasTy M.recFields sv T ∧
        Res S M.structOf n tail env st sv out
  | .literal (.int k), Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨hband, rfl⟩ := tyOf_litInt_inv hty
      have hk : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
        simpa [AverCert.PlanCheck.inI64Band, Bool.and_eq_true, decide_eq_true_eq] using hband
      cases hb : box [.i64v k] with
      | none => simp [lowerW, lowerB, eraseL, eraseI, wRunF, hBox, popArgs, hb] at hrun
      | some r =>
          simp [lowerW, lowerB, eraseL, eraseI, wRunF, hBox, popArgs, hb] at hrun
          subst hrun
          refine ⟨.i k, by simp [eval], by simp [HasTy], res_ok ?_ hl⟩
          simp only [SRepr]
          exact Ctr.hBox k r hk.1 hk.2 hb
  | .literal (.bool v), Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      have hT := tyOf_litBool_inv hty
      subst hT
      simp [lowerW, lowerB, eraseL, eraseI, wRunF] at hrun
      subst hrun
      exact ⟨.b v, by simp [eval], by simp [HasTy], res_ok (by simp [SRepr, b32]) hl⟩
  | .local i, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      simp only [tyOf] at hty
      obtain ⟨sv, hsv, hT⟩ := envTy_get henv hty
      obtain ⟨_, w, hw, hrep⟩ := hl.2 i sv hsv
      simp [lowerW, lowerB, eraseL, eraseI, wRunF, hw] at hrun
      subst hrun
      exact ⟨sv, by simp [eval, hsv], hT, res_ok hrep hl⟩
  | .let_ b v body, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨hbn, hΓb, Tv, htv, htb⟩ := tyOf_let_inv hty
      simp only [lowerW, lowerB, htv, eraseL_append, List.append_assoc] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨sv1, hev1, hT1, hres1⟩ := agreement v Γ env false Tv wl st o1 htv henv hl h1
      obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
      simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append, wRunF] at hseq
      have henv' : EnvTy M.recFields (upd env b sv1) (upd Γ b Tv) := envTy_upd henv hT1
      have hl' := lrel_bind hl1 hbn hw1
      obtain ⟨sv, hev, hT, hres⟩ :=
        agreement body (upd Γ b Tv) (upd env b sv1) tail T (wl1.set b w1) st out htb henv' hl'
          hseq
      have hfree := envTy_free henv hΓb
      exact ⟨sv, by simp [eval, hev1, hev], hT, res_of_upd hfree hres⟩
  | .call (.fn f) args, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨sig, hsig, hts, rfl⟩ := tyOf_callFn_inv hty
      obtain ⟨hhost, har, hspec⟩ := hCallees f sig hsig
      simp only [lowerW, lowerB, eraseL_append] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
        agreementArgs args Γ env sig.params wl st o1 hts henv hl h1
      have hlen : sig.params.length = ws.length := by
        rw [← hasTyL_length hTs, sreprL_length hrep]
      simp only [seqOut, eraseL, eraseI] at hseq
      cases hr : callee f ws with
      | none => simp [wRunF, hhost, har, hlen, popArgs_rev, hr] at hseq
      | some r =>
          simp [wRunF, hhost, har, hlen, popArgs_rev, hr] at hseq
          subst hseq
          obtain ⟨sv, hm, hsv, hT⟩ := hspec svs ws r hTs hrep hr
          exact ⟨sv, by simp [eval, hevs, hm], hT, res_ok hsv hl1⟩
  | .call (.builtin bi) args, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨ts, hts, hbt⟩ := tyOf_callBuiltin_inv hty
      simp only [lowerW, lowerB, eraseL_append] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
        agreementArgs args Γ env ts wl st o1 hts henv hl h1
      simp only [seqOut, eraseL, eraseI] at hseq
      obtain ⟨v, hbe, rfl, rfl⟩ :=
        builtin_step host ar callee bi ts T hbt svs ws hTs hrep wl1 st out hseq
      exact ⟨.b v, by simp [eval, hevs, hbe], by simp [HasTy], res_ok (by simp [SRepr]) hl1⟩
  | .tailCall f args, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨rfl, sig, hsig, hts, rfl⟩ := tyOf_tailCall_inv hty
      obtain ⟨hhost, har, hspec⟩ := hCallees f sig hsig
      simp only [lowerW, lowerB, eraseL_append] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
        agreementArgs args Γ env sig.params wl st o1 hts henv hl h1
      have hlen : sig.params.length = ws.length := by
        rw [← hasTyL_length hTs, sreprL_length hrep]
      simp only [seqOut, eraseL, eraseI] at hseq
      cases hr : callee f ws with
      | none => simp [wRunF, har, hlen, popArgs_rev, hr] at hseq
      | some r =>
          simp [wRunF, har, hlen, popArgs_rev, hr] at hseq
          subst hseq
          obtain ⟨sv, hm, hsv, hT⟩ := hspec svs ws r hTs hrep hr
          exact ⟨sv, by simp [eval, hevs, hm], hT, ⟨rfl, hsv⟩⟩
  | .binOp op l r, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      rcases tyOf_binOp_inv hty with ⟨htl, htr, hT⟩ | ⟨htl, htr, hop, rfl⟩
      · -- Int operands
        simp only [lowerW, lowerB, htl] at hrun
        cases hA : op.isArith
        · -- a comparison
          simp only [hA, Bool.false_eq_true, ↓reduceIte] at hT hrun
          subst hT
          cases hlk : litInt? l with
          | some kk =>
              have hl0 := litInt?_some hlk
              subst hl0
              have hband := (tyOf_litInt_inv htl).1
              simp only [litInt?] at hrun
              cases hsr : slot? r with
              | some i =>
                  have hr0 := slot?_some hsr
                  subst hr0
                  simp only [slot?, hCarrier] at hrun
                  simp only [tyOf] at htr
                  obtain ⟨sv, hsv, hTv⟩ := envTy_get henv htr
                  obtain ⟨m, rfl⟩ := hasTy_int hTv
                  obtain ⟨_, w, hw, hrep⟩ := hl.2 i _ hsv
                  have hout := cmpArm_step S host ar callee op.flip (flip_isArith hA) kk i wl st
                    m w out hband hw hrep hrun
                  subst hout
                  refine ⟨.b (cmpDen op kk m), ?_, by simp [HasTy], res_ok ?_ hl⟩
                  · simp [eval, hsv, intBin_cmp hA]
                  · simp [SRepr, cmpDen_flip]
              | none =>
                  simp only [hsr, hCarrier, eraseL_append, List.append_assoc] at hrun
                  obtain ⟨o1, h1, hseq⟩ := run_split hrun
                  obtain ⟨sv1, hev1, hT1, hres1⟩ :=
                    agreement r Γ env false .int wl st o1 htr henv hl h1
                  obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
                  obtain ⟨m, rfl⟩ := hasTy_int hT1
                  simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append,
                    wRunF] at hseq
                  have hget : (wl1.set n w1)[n]? = some w1 := List.getElem?_set_self hl1.1
                  have hout := cmpArm_step S host ar callee op.flip (flip_isArith hA) kk n
                    (wl1.set n w1) st m w1 out hband hget hw1 hseq
                  subst hout
                  refine ⟨.b (cmpDen op kk m), ?_, by simp [HasTy],
                    res_ok ?_ (lrel_set_free w1 hl1 (Nat.le_refl n))⟩
                  · simp [eval, hev1, intBin_cmp hA]
                  · simp [SRepr, cmpDen_flip]
          | none =>
              cases hrk : litInt? r with
              | some kk =>
                  have hr0 := litInt?_some hrk
                  subst hr0
                  have hband := (tyOf_litInt_inv htr).1
                  rw [hlk] at hrun
                  simp only [litInt?] at hrun
                  cases hsl : slot? l with
                  | some i =>
                      have hl0 := slot?_some hsl
                      subst hl0
                      simp only [slot?, hCarrier] at hrun
                      simp only [tyOf] at htl
                      obtain ⟨sv, hsv, hTv⟩ := envTy_get henv htl
                      obtain ⟨m, rfl⟩ := hasTy_int hTv
                      obtain ⟨_, w, hw, hrep⟩ := hl.2 i _ hsv
                      have hout := cmpArm_step S host ar callee op hA kk i wl st m w out hband hw
                        hrep hrun
                      subst hout
                      refine ⟨.b (cmpDen op m kk), ?_, by simp [HasTy], res_ok ?_ hl⟩
                      · simp [eval, hsv, intBin_cmp hA]
                      · simp [SRepr]
                  | none =>
                      simp only [hsl, hCarrier, eraseL_append, List.append_assoc] at hrun
                      obtain ⟨o1, h1, hseq⟩ := run_split hrun
                      obtain ⟨sv1, hev1, hT1, hres1⟩ :=
                        agreement l Γ env false .int wl st o1 htl henv hl h1
                      obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
                      obtain ⟨m, rfl⟩ := hasTy_int hT1
                      simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append,
                        wRunF] at hseq
                      have hget : (wl1.set n w1)[n]? = some w1 := List.getElem?_set_self hl1.1
                      have hout := cmpArm_step S host ar callee op hA kk n (wl1.set n w1) st m w1
                        out hband hget hw1 hseq
                      subst hout
                      refine ⟨.b (cmpDen op m kk), ?_, by simp [HasTy],
                        res_ok ?_ (lrel_set_free w1 hl1 (Nat.le_refl n))⟩
                      · simp [eval, hev1, intBin_cmp hA]
                      · simp [SRepr]
              | none =>
                  simp only [hlk, hrk, eraseL_append, eraseL_ops, List.append_assoc] at hrun
                  obtain ⟨o1, h1, hseq⟩ := run_split hrun
                  obtain ⟨sva, heva, hTa, hresa⟩ :=
                    agreement l Γ env false .int wl st o1 htl henv hl h1
                  obtain ⟨wl1, wa, rfl, hwa, hl1⟩ := res_false hresa
                  simp only [seqOut] at hseq
                  obtain ⟨o2, h2, hseq2⟩ := run_split hseq
                  obtain ⟨svb, hevb, hTb, hresb⟩ :=
                    agreement r Γ env false .int wl1 (wa :: st) o2 htr henv hl1 h2
                  obtain ⟨wl2, wb, rfl, hwb, hl2⟩ := res_false hresb
                  simp only [seqOut] at hseq2
                  obtain ⟨x, rfl⟩ := hasTy_int hTa
                  obtain ⟨y, rfl⟩ := hasTy_int hTb
                  have hout := intCmpTail_step S box add sub mul cmp eq Ctr host ar callee M hCmp
                    hEq op hA x y wa wb hwa hwb wl2 st out hseq2
                  subst hout
                  refine ⟨.b (cmpDen op x y), ?_, by simp [HasTy], res_ok (by simp [SRepr]) hl2⟩
                  simp [eval, heva, hevb, intBin_cmp hA]
        · -- arithmetic
          simp only [hA, ↓reduceIte] at hT hrun
          subst hT
          simp only [eraseL_append, eraseL, eraseI, List.append_assoc] at hrun
          obtain ⟨o1, h1, hseq⟩ := run_split hrun
          obtain ⟨sva, heva, hTa, hresa⟩ := agreement l Γ env false .int wl st o1 htl henv hl h1
          obtain ⟨wl1, wa, rfl, hwa, hl1⟩ := res_false hresa
          simp only [seqOut] at hseq
          obtain ⟨o2, h2, hseq2⟩ := run_split hseq
          obtain ⟨svb, hevb, hTb, hresb⟩ :=
            agreement r Γ env false .int wl1 (wa :: st) o2 htr henv hl1 h2
          obtain ⟨wl2, wb, rfl, hwb, hl2⟩ := res_false hresb
          simp only [seqOut] at hseq2
          obtain ⟨x, rfl⟩ := hasTy_int hTa
          obtain ⟨y, rfl⟩ := hasTy_int hTb
          obtain ⟨w, rfl, hw, hTw⟩ := arith_step S box add sub mul cmp eq Ctr host ar callee M
            M.structOf M.recFields hAdd hSub hMul op hA x y wa wb hwa hwb wl2 st out hseq2
          exact ⟨intBin op x y, by simp [eval, heva, hevb], hTw, res_ok hw hl2⟩
      · -- Bool operands
        simp only [lowerW, lowerB, htl, eraseL_append, eraseL, eraseI, List.append_assoc] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sva, heva, hTa, hresa⟩ := agreement l Γ env false .bool wl st o1 htl henv hl h1
        obtain ⟨wl1, wa, rfl, hwa, hl1⟩ := res_false hresa
        simp only [seqOut] at hseq
        obtain ⟨o2, h2, hseq2⟩ := run_split hseq
        obtain ⟨svb, hevb, hTb, hresb⟩ :=
          agreement r Γ env false .bool wl1 (wa :: st) o2 htr henv hl1 h2
        obtain ⟨wl2, wb, rfl, hwb, hl2⟩ := res_false hresb
        simp only [seqOut] at hseq2
        obtain ⟨x, rfl⟩ := hasTy_bool hTa
        obtain ⟨y, rfl⟩ := hasTy_bool hTb
        have hwa' := srepr_b hwa
        have hwb' := srepr_b hwb
        subst hwa' hwb'
        obtain ⟨v, hbb, rfl⟩ := boolCmp_step host ar callee op hop x y wl2 st out hseq2
        exact ⟨.b v, by simp [eval, heva, hevb, hbb], by simp [HasTy],
          res_ok (by simp [SRepr]) hl2⟩
  | .neg e, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨hte, rfl⟩ := tyOf_neg_inv hty
      simp only [lowerW, lowerB, eraseL_append] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨sv1, hev1, hT1, hres1⟩ := agreement e Γ env false .int wl st o1 hte henv hl h1
      obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
      obtain ⟨x, rfl⟩ := hasTy_int hT1
      simp only [seqOut, eraseL, eraseI] at hseq
      cases hr : neg [w1] with
      | none => simp [wRunF, hNeg, popArgs_one, hr] at hseq
      | some r =>
          simp [wRunF, hNeg, popArgs_one, hr] at hseq
          subst hseq
          have hw1' : RecordComputeBridge.CanonRepr S x w1 := by simpa [SRepr] using hw1
          refine ⟨.i (-x), by simp [eval, hev1], by simp [HasTy], res_ok ?_ hl1⟩
          simp only [SRepr]
          exact hNegC x w1 r hw1' hr
  | .ifThenElse c t e, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨htc, htt, hte⟩ := tyOf_ite_inv hty
      simp only [lowerW, lowerB, eraseL_append, eraseL, eraseI] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨svc, hevc, hTc, hresc⟩ := agreement c Γ env false .bool wl st o1 htc henv hl h1
      obtain ⟨wl1, wc, rfl, hwc, hl1⟩ := res_false hresc
      obtain ⟨v, rfl⟩ := hasTy_bool hTc
      have hwc' := srepr_b hwc
      subst hwc'
      cases v with
      | false =>
          simp only [seqOut, b32, Bool.false_eq_true, ↓reduceIte] at hseq
          rw [wRunF_ifElse_single] at hseq
          simp only [↓reduceIte] at hseq
          obtain ⟨sv, hev, hT, hres⟩ := agreement e Γ env tail T wl1 st out hte henv hl1 hseq
          exact ⟨sv, by simp [eval, hevc, hev], hT, hres⟩
      | true =>
          simp only [seqOut, b32, ↓reduceIte] at hseq
          rw [wRunF_ifElse_single] at hseq
          simp only [Int.reduceEq, ↓reduceIte] at hseq
          obtain ⟨sv, hev, hT, hres⟩ := agreement t Γ env tail T wl1 st out htt henv hl1 hseq
          exact ⟨sv, by simp [eval, hevc, hev], hT, hres⟩
  | .recordCreate tid fs, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨fts, hR, hts, rfl⟩ := tyOf_rec_inv hty
      simp only [lowerW, lowerB, eraseL_append] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
        agreementArgs fs Γ env fts wl st o1 hts henv hl h1
      have hlen : fs.length = ws.length := by
        rw [← tysOf_length hts, ← hasTyL_length hTs, sreprL_length hrep]
      simp only [seqOut, eraseL, eraseI] at hseq
      simp only [hlen, wRunF, popArgs_rev, Option.some.injEq] at hseq
      subst hseq
      refine ⟨.record tid svs, by simp [eval, hevs], ?_, res_ok ?_ hl1⟩
      · simp only [HasTy, true_and]
        exact ⟨fts, hR, hTs⟩
      · simp only [SRepr]
        exact ⟨ws, rfl, hrep⟩
  | .project tid i base, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨htb, fts, hR, hi⟩ := tyOf_proj_inv hty
      simp only [lowerW, lowerB, eraseL_append] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨sv1, hev1, hT1, hres1⟩ :=
        agreement base Γ env false (.record tid) wl st o1 htb henv hl h1
      obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
      obtain ⟨fs, fts', rfl, hR', hfs⟩ := hasTy_record hT1
      rw [hR] at hR'
      cases hR'
      obtain ⟨sv, hsv, hTsv⟩ := hasTyL_get hfs hi
      simp only [SRepr] at hw1
      obtain ⟨ws, rfl, hws⟩ := hw1
      obtain ⟨w, hw, hwr⟩ := sreprL_get hws hsv
      simp only [seqOut, eraseL, eraseI] at hseq
      simp [wRunF, hw] at hseq
      subst hseq
      exact ⟨sv, by simp [eval, hev1, hsv], hTsv, res_ok hwr hl1⟩

theorem agreementArgs :
    ∀ (es : List Expr) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (Ts : List Ty)
      (wl st : List WVal) (out : Out),
      tysOf M n Γ es = some Ts →
      EnvTy M.recFields env Γ →
      LRel S M.structOf n env wl →
      wRunF host ar callee (lowerArgsW M n Γ es) wl st = some out →
      ∃ svs ws wl', out = .ok wl' (ws.reverse ++ st) ∧
        evalArgs F env es = some svs ∧ HasTyL M.recFields svs Ts ∧
        SReprL S M.structOf svs ws ∧ LRel S M.structOf n env wl'
  | [], Γ, env, Ts, wl, st, out, hty, henv, hl, hrun => by
      simp only [tysOf, Option.some.injEq] at hty
      subst hty
      simp only [lowerArgsW, lowerArgsB, eraseL, wRunF, Option.some.injEq] at hrun
      subst hrun
      exact ⟨[], [], wl, by simp, by simp [evalArgs], by simp [HasTyL], by simp [SReprL], hl⟩
  | e :: es, Γ, env, Ts, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨t, ts, hte, htes, rfl⟩ := tysOf_cons_inv hty
      simp only [lowerArgsW, lowerArgsB, eraseL_append] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨sv, hev, hT, hres⟩ := agreement e Γ env false t wl st o1 hte henv hl h1
      obtain ⟨wl1, w, rfl, hw, hl1⟩ := res_false hres
      simp only [seqOut] at hseq
      obtain ⟨svs, ws, wl2, rfl, hevs, hTs, hrep, hl2⟩ :=
        agreementArgs es Γ env ts wl1 (w :: st) out htes henv hl1 hseq
      exact ⟨sv :: svs, w :: ws, wl2, by simp, by simp [evalArgs, hev, hevs], ⟨hT, hTs⟩,
        ⟨hw, hrep⟩, hl2⟩
end

end Agreement

/-! ## Function level: fuel discharges every member contract of a group -/

theorem groupModel_outer (outer : Nat → Nat → List SVal → Option SVal)
    (G : Nat → Option FnPlan) {f : Nat} (hG : G f = none) (fuel : Nat) (args : List SVal) :
    groupModel outer G fuel f args = outer fuel f args := by
  cases fuel <;> simp [groupModel, hG]

theorem groupModel_member (outer : Nat → Nat → List SVal → Option SVal)
    (G : Nat → Option FnPlan) {f : Nat} {p : FnPlan} (hG : G f = some p) (fuel : Nat)
    (args : List SVal) :
    groupModel outer G (fuel + 1) f args =
      eval (groupModel outer G fuel) (argsEnv args) p.body := by
  simp [groupModel, hG]

theorem envTy_args {R : Nat → Option (List Ty)} {svs : List SVal} {ts : List Ty}
    (h : HasTyL R svs ts) : EnvTy R (argsEnv svs) (paramsΓ ts) := by
  intro i
  cases hv : svs[i]? with
  | none =>
      left
      refine ⟨hv, ?_⟩
      have hlen := hasTyL_length h
      have : ts.length ≤ i := by
        rw [← hlen]
        exact List.getElem?_eq_none_iff.mp hv
      exact List.getElem?_eq_none_iff.mpr this
  | some v =>
      right
      obtain ⟨t, ht, hvt⟩ := hasTyL_get' h hv
      exact ⟨v, t, hv, ht, hvt⟩

/-- The group theorem: every member of one group (an SCC, or a single
    function) is certified at its plan's model, given the planned code at its
    index, the typing checks, and `FnCertified` for every callee outside the
    group. Self and mutual calls need nothing more: at fuel `k + 1` the
    members' contracts at fuel `k` are the induction hypothesis. -/
theorem fn_certified_group {C : Nat} (S : CarrierSpec C)
    (box add sub mul cmp eq neg : List WVal → Option WVal)
    (Ctr : RecordComputeBridge.Contracts S box add sub mul cmp eq)
    (hNegC : ∀ x w r, RecordComputeBridge.CanonRepr S x w → neg [w] = some r →
      RecordComputeBridge.CanonRepr S (-x) r)
    (code : CodeTbl) (host : HostTbl) (M : MCtx)
    (hCarrier : M.carrier = C)
    (hBox : host M.box = some (1, box)) (hAdd : host M.add = some (2, add))
    (hSub : host M.sub = some (2, sub)) (hMul : host M.mul = some (2, mul))
    (hNeg : host M.neg = some (1, neg))
    (hCmp : host M.cmp = some (2, cmp)) (hEq : host M.eq = some (2, eq))
    (G : Nat → Option FnPlan) (outer : Nat → Nat → List SVal → Option SVal)
    (hOuter : ∀ f sig, M.sigs f = some sig → G f = none →
      FnCertified S M.structOf M.recFields code host f sig (fun fuel => outer fuel f))
    (hMem : ∀ f p, G f = some p →
      M.sigs f = some p.sig ∧ planTyped M p = true ∧ host f = none ∧
        code f = some (fnCode M p)) :
    ∀ f p, G f = some p →
      FnCertified S M.structOf M.recFields code host f p.sig
        (fun fuel => groupModel outer G fuel f) := by
  have core : ∀ fuel f p, G f = some p → ∀ svs ws r,
      HasTyL M.recFields svs p.sig.params → SReprL S M.structOf svs ws →
      wFuncN code host fuel f ws = some r →
      ∃ sv, groupModel outer G fuel f svs = some sv ∧ SRepr S M.structOf sv r ∧
        HasTy M.recFields sv p.sig.ret := by
    intro fuel
    induction fuel with
    | zero => intro f p _ svs ws r _ _ h; simp [wFuncN] at h
    | succ k ih =>
        intro f p hG svs ws r hTs hrep hrun
        obtain ⟨_, htyped, _, hcode⟩ := hMem f p hG
        simp only [planTyped, Bool.and_eq_true, decide_eq_true_eq] at htyped
        obtain ⟨⟨hpn, hnl⟩, hty⟩ := htyped
        have hCallees : ∀ g sig, M.sigs g = some sig →
            Contract S M.structOf M.recFields host (fun g => (code g).map (·.arity))
              (fun g as => wFuncN code host k g as) g sig (groupModel outer G k g) := by
          intro g sig hsig
          cases hg : G g with
          | some p' =>
              obtain ⟨hsig', _, hhost', hcode'⟩ := hMem g p' hg
              rw [hsig] at hsig'
              cases hsig'
              refine ⟨hhost', by simp [hcode', fnCode], ?_⟩
              intro svs' ws' r' hT' hr' hc'
              exact ih g p' hg svs' ws' r' hT' hr' hc'
          | none =>
              have hc := (hOuter g sig hsig hg).contract k
              refine ⟨hc.1, hc.2.1, ?_⟩
              intro svs' ws' r' hT' hr' hc'
              obtain ⟨sv, hm, hsv, hT⟩ := hc.2.2 svs' ws' r' hT' hr' hc'
              exact ⟨sv, by rw [groupModel_outer outer G hg]; exact hm, hsv, hT⟩
        have hlenw : ws.length = p.sig.params.length := by
          rw [← sreprL_length hrep, hasTyL_length hTs]
        have hLR : LRel S M.structOf p.nslots (argsEnv svs) (initLocals (fnCode M p) ws) := by
          refine ⟨by simp [initLocals, fnCode]; omega, ?_⟩
          intro i sv hs
          obtain ⟨w, hw, hsv⟩ := sreprL_get hrep hs
          have hi : i < ws.length := by
            rcases Nat.lt_or_ge i ws.length with h | h
            · exact h
            · rw [List.getElem?_eq_none h] at hw; simp at hw
          refine ⟨by omega, w, ?_, hsv⟩
          simp [initLocals, List.getElem?_append_left hi, hw]
        unfold wFuncN at hrun
        rw [hcode] at hrun
        simp only at hrun
        cases hw : wRunF host (fun g => (code g).map (·.arity))
            (fun g as => wFuncN code host k g as) (fnCode M p).body
            (initLocals (fnCode M p) ws) [] with
        | none => rw [hw] at hrun; simp at hrun
        | some o =>
            rw [hw] at hrun
            obtain ⟨sv, hev, hT, hres⟩ := agreement S box add sub mul cmp eq neg Ctr hNegC host
              (fun g => (code g).map (·.arity)) (fun g as => wFuncN code host k g as) M
              hCarrier hBox hAdd hSub hMul hNeg hCmp hEq (groupModel outer G k) hCallees
              p.nslots p.body (paramsΓ p.sig.params) (argsEnv svs) true p.sig.ret
              (initLocals (fnCode M p) ws) [] o hty (envTy_args hTs) hLR hw
            have hm : groupModel outer G (k + 1) f svs = some sv := by
              rw [groupModel_member outer G hG]; exact hev
            cases o with
            | ok wl' st' =>
                obtain ⟨w, rfl, hsv, _⟩ := hres
                simp at hrun
                subst hrun
                exact ⟨sv, hm, hsv, hT⟩
            | ret w =>
                obtain ⟨_, hsv⟩ := hres
                simp at hrun
                subst hrun
                exact ⟨sv, hm, hsv, hT⟩
  intro f p hG
  obtain ⟨_, _, hhost, hcode⟩ := hMem f p hG
  exact ⟨hhost, by simp [hcode, fnCode], fun fuel => core fuel f p hG⟩

end AverCert.Grammar
