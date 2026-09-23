/- GrammarSound — the simulation theorem for the one-grammar plan.

   ONE statement, `agreement`, by structural induction over `Grammar.Expr`
   (mutually with the argument lists and with one statement per admitted
   `Match` arm shape, each by induction over the arms):
   a successful run of the lowered instructions in the audited interpreter
   means the source semantics succeeds with a value the run represents. It
   depends on no particular function body. Calls cite a `Contract` for the
   callee, never its body; `fn_certified_group` supplies those contracts for
   a whole group of functions (one SCC: self and mutual calls) by induction
   on the interpreter fuel, and takes functions outside the group as
   `FnCertified` hypotheses from earlier groups.

   Representation: Int is a canonical carrier word (`CanonRepr`, as in the
   existing wall), Bool is `i32` 0/1, a record is the struct of its type (a
   one-field newtype record its field's value) and a variant the struct of
   its constructor, both with pointwise represented fields; an Option /
   Result is the struct of its instantiation with the tag in field 0; a
   Float is its `f64` bits, a String the `$string` array of its bytes, a
   Vector the array of its elements, a List `null` or a cons struct. The
   locals relation `LRel` constrains only the slots the source environment
   defines, and all of them sit below the resolver slot count `X.n`; the
   scratch locals (subject scratch, const-compare scratch) sit at or above it
   and are free for the templates to overwrite, and need not exist at all
   (a stash is read back at once, and that read fails on a missing local). A
   match reads its stashed subject only before any arm body runs, so a
   nested match reusing the same scratch is harmless.

   The String and Vector nodes call three more runtime helpers, taken with
   exactly the contracts `Schema.Obligation.holds` already assumes of them
   (`XHost`): `__wasmgc_concat_n`, `__wasmgc_string_eq`, `__aint_to_index`.

   The interpreter's `ref.test` is exact while wasm GC tests subtyping; the
   S-3 section below shows the two agree on constructor structs under the
   byte pin `GrammarLower.S3Pin`, which the acceptance must check. -/
import GrammarLower
import InterpreterSequencing

set_option maxHeartbeats 4000000
set_option maxRecDepth 100000
set_option linter.unusedSectionVars false
set_option linter.unusedSimpArgs false

namespace AverCert.Grammar
open CertPrelude AverCert.Schema InterpreterSequencing

/-! ## Relations -/

/-- The source environment agrees with the typing environment. -/
def EnvTy (M : MCtx) (env : Nat → Option SVal) (Γ : Nat → Option Ty) :
    Prop :=
  ∀ i, (env i = none ∧ Γ i = none) ∨
    ∃ v T, env i = some v ∧ Γ i = some T ∧ HasTy M v T

section Rel
variable {C : Nat} (S : CarrierSpec C) (M : MCtx) (X : LCtx)

/-- Locals relation: every resolver slot is a local, and every defined source
    slot sits below the resolver slot count `X.n` and is represented at its
    own wasm local. The scratch locals sit at or above `X.n`, so the
    templates may overwrite them freely. A scratch local need not exist (a
    carrier-free function declares no locals at all): a template that stashes
    into a missing scratch reads it back next, and that read fails. -/
def LRel (env : Nat → Option SVal) (wl : List WVal) : Prop :=
  (X.n ≤ X.cmp ∧ X.n ≤ X.subj ∧ X.n ≤ wl.length) ∧
    ∀ i v, env i = some v → i < X.n ∧ ∃ w, wl[i]? = some w ∧ SRepr S M v w

/-- What a node's run looks like: a normal run pushes exactly one represented
    value on the untouched stack and keeps the locals relation; a `.ret` (a
    `return_call`) happens only in tail position. -/
def Res (tail : Bool) (env : Nat → Option SVal) (st : List WVal) (sv : SVal) : Out → Prop
  | .ok wl' st' => ∃ w, st' = w :: st ∧ SRepr S M sv w ∧ LRel S M X env wl'
  | .ret w => tail = true ∧ SRepr S M sv w

end Rel

/-- The runtime helpers the String and Vector nodes call, at their indices,
    each with the contract `Schema.Obligation.holds` already assumes of it:
    `__wasmgc_concat_n` concatenates the byte arrays of its `Vector<String>`
    argument into a `$string` array, `__wasmgc_string_eq` is byte equality,
    and `__aint_to_index` maps a represented Int to its `i32` index or the
    `-1` sentinel; `__aint_divmod(a, b, want_mod)` is Euclidean division
    (`want_mod = 0`) or remainder (`want_mod = 1`) on a canonical pair with a
    nonzero divisor, with a canonical result. -/
structure XHost {C : Nat} (S : CarrierSpec C) (M : MCtx) (host : HostTbl) : Prop where
  concat : ∃ g, host M.concat = some (1, g) ∧
    ∀ parts c, g [parts] = some c → stringConcatW M.str parts = some c
  streq : ∃ g, host M.streq = some (2, g) ∧
    ∀ a b r, g [a, b] = some r → r = b32 (stringEqW a b)
  toIndex : ∃ g, host M.toIndex = some (1, g) ∧
    ∀ n v r, S.Repr n v → g [v] = some r → r = .i32v (toIndexW n)
  divmod : ∃ g, host M.divmod = some (3, g) ∧
    ∀ a b wa wb m r, CanonRepr S a wa → CanonRepr S b wb → b ≠ 0 → (m = 0 ∨ m = 1) →
      g [wa, wb, .i32v m] = some r → CanonRepr S (if m = 1 then a % b else a / b) r

/-- Assume–guarantee contract of a code function `f` at signature `sig` for
    one opaque `callee`: the ONLY thing a caller knows about `f`. -/
def Contract {C : Nat} (S : CarrierSpec C) (M : MCtx)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (f : Nat) (sig : Sig) (model : List SVal → Option SVal) : Prop :=
  host f = none ∧ ar f = some sig.params.length ∧
  ∀ svs ws r, HasTyL M svs sig.params → SReprL S M svs ws → callee f ws = some r →
    ∃ sv, model svs = some sv ∧ SRepr S M sv r ∧ HasTy M sv sig.ret

/-- The certificate face of one function, with a fuel-indexed model. -/
def FnCertified {C : Nat} (S : CarrierSpec C) (M : MCtx)
    (code : CodeTbl) (host : HostTbl) (f : Nat) (sig : Sig)
    (model : Nat → List SVal → Option SVal) : Prop :=
  host f = none ∧ (code f).map (·.arity) = some sig.params.length ∧
  ∀ fuel svs ws r, HasTyL M svs sig.params → SReprL S M svs ws →
    wFuncN code host fuel f ws = some r →
    ∃ sv, model fuel svs = some sv ∧ SRepr S M sv r ∧ HasTy M sv sig.ret

theorem FnCertified.contract {C : Nat} {S : CarrierSpec C} {M : MCtx}
    {code : CodeTbl} {host : HostTbl} {f : Nat} {sig : Sig}
    {model : Nat → List SVal → Option SVal}
    (h : FnCertified S M code host f sig model) (fuel : Nat) :
    Contract S M host (fun g => (code g).map (·.arity))
      (fun g as => wFuncN code host fuel g as) f sig (model fuel) :=
  ⟨h.1, h.2.1, fun svs ws r hT hr hc => h.2.2 fuel svs ws r hT hr hc⟩

/-! ## Small lemmas -/

section Small
variable {M : MCtx}

theorem hasTy_int {v : SVal} (h : HasTy M v .int) : ∃ n, v = .i n := by
  cases v <;> simp_all [HasTy]

theorem hasTy_bool {v : SVal} (h : HasTy M v .bool) : ∃ b, v = .b b := by
  cases v <;> simp_all [HasTy]

theorem hasTy_record {v : SVal} {tid : Nat} (h : HasTy M v (.record tid)) :
    ∃ fs fts, v = .record tid fs ∧ M.recFields tid = some fts ∧ HasTyL M fs fts := by
  cases v <;> simp only [HasTy] at h
  obtain ⟨rfl, fts, hR, hfs⟩ := h
  exact ⟨_, fts, rfl, hR, hfs⟩

theorem hasTy_sum {v : SVal} {tid : Nat} (h : HasTy M v (.sum tid)) :
    ∃ c fs fts, v = .variant tid c fs ∧ ctorFields M tid c = some fts ∧ HasTyL M fs fts := by
  cases v <;> simp only [HasTy] at h
  obtain ⟨rfl, fts, hR, hfs⟩ := h
  exact ⟨_, _, fts, rfl, hR, hfs⟩

theorem hasTy_option {v : SVal} {t : Ty} (h : HasTy M v (.option t)) :
    v = .none t ∨ ∃ x, v = .some t x ∧ HasTy M x t := by
  cases v <;> simp only [HasTy] at h
  · subst h; exact Or.inl rfl
  · obtain ⟨rfl, hx⟩ := h; exact Or.inr ⟨_, rfl, hx⟩

theorem hasTy_result {v : SVal} {t e : Ty} (h : HasTy M v (.result t e)) :
    (∃ x, v = .ok t e x ∧ HasTy M x t) ∨ (∃ x, v = .err t e x ∧ HasTy M x e) := by
  cases v <;> simp only [HasTy] at h
  · obtain ⟨rfl, rfl, hx⟩ := h; exact Or.inl ⟨_, rfl, hx⟩
  · obtain ⟨rfl, rfl, hx⟩ := h; exact Or.inr ⟨_, rfl, hx⟩

theorem hasTy_float {v : SVal} (h : HasTy M v .float) : ∃ x, v = .f x := by
  cases v <;> simp_all [HasTy]

theorem hasTy_string {v : SVal} (h : HasTy M v .string) : ∃ x, v = .s x := by
  cases v <;> simp_all [HasTy]

theorem hasTy_vec {v : SVal} {t : Ty} (h : HasTy M v (.vec t)) :
    ∃ vs, v = .vec t vs ∧ HasTyAll M vs t := by
  cases v <;> simp only [HasTy] at h
  obtain ⟨rfl, h⟩ := h
  exact ⟨_, rfl, h⟩

theorem hasTyAll_get : ∀ {vs : List SVal} {t : Ty}, HasTyAll M vs t →
    ∀ {i : Nat} {v : SVal}, vs[i]? = some v → HasTy M v t
  | [], _, _, i, v, hv => by simp at hv
  | v' :: vs, t, h, i, v, hv => by
      cases i with
      | zero =>
          simp only [List.getElem?_cons_zero, Option.some.injEq] at hv
          subst hv
          exact h.1
      | succ i =>
          simp only [List.getElem?_cons_succ] at hv
          exact hasTyAll_get h.2 hv

theorem hasTyL_length : ∀ {vs : List SVal} {ts : List Ty}, HasTyL M vs ts →
    vs.length = ts.length
  | [], [], _ => rfl
  | _ :: _, _ :: _, h => by simp [hasTyL_length h.2]
  | [], _ :: _, h => by simp [HasTyL] at h
  | _ :: _, [], h => by simp [HasTyL] at h

theorem hasTyL_get : ∀ {vs : List SVal} {ts : List Ty}, HasTyL M vs ts →
    ∀ {i : Nat} {t : Ty}, ts[i]? = some t → ∃ v, vs[i]? = some v ∧ HasTy M v t
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

theorem hasTyL_get' : ∀ {vs : List SVal} {ts : List Ty}, HasTyL M vs ts →
    ∀ {i : Nat} {v : SVal}, vs[i]? = some v → ∃ t, ts[i]? = some t ∧ HasTy M v t
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
variable {C : Nat} {S : CarrierSpec C} {M : MCtx}

theorem sreprL_length : ∀ {vs : List SVal} {ws : List WVal}, SReprL S M vs ws →
    vs.length = ws.length
  | [], [], _ => rfl
  | _ :: _, _ :: _, h => by simp [sreprL_length h.2]
  | [], _ :: _, h => by simp [SReprL] at h
  | _ :: _, [], h => by simp [SReprL] at h

theorem sreprL_get : ∀ {vs : List SVal} {ws : List WVal}, SReprL S M vs ws →
    ∀ {i : Nat} {v : SVal}, vs[i]? = some v → ∃ w, ws[i]? = some w ∧ SRepr S M v w
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

theorem srepr_b {v : Bool} {w : WVal} (h : SRepr S M (.b v) w) : w = b32 v := by
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
variable {C : Nat} {S : CarrierSpec C} {M : MCtx} {X : LCtx}

/-- Writing a slot the source environment does not define keeps the relation
    (the stash into the scratch local). -/
theorem lrel_set_free {env : Nat → Option SVal} {wl : List WVal} {j : Nat} (w : WVal)
    (hl : LRel S M X env wl) (hj : X.n ≤ j) : LRel S M X env (wl.set j w) := by
  refine ⟨by simpa using hl.1, ?_⟩
  intro i v hv
  obtain ⟨hi, w', hw', hr⟩ := hl.2 i v hv
  refine ⟨hi, w', ?_, hr⟩
  rw [List.getElem?_set_ne (by omega)]
  exact hw'

/-- Binding a fresh slot `b < n` on both sides keeps the relation. -/
theorem lrel_bind {env : Nat → Option SVal} {wl : List WVal} {b : Nat} {v : SVal} {w : WVal}
    (hl : LRel S M X env wl) (hb : b < X.n) (hr : SRepr S M v w) :
    LRel S M X (upd env b v) (wl.set b w) := by
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
    (hfree : env b = none) (hl : LRel S M X (upd env b v) wl) : LRel S M X env wl := by
  refine ⟨hl.1, ?_⟩
  intro i v' hv'
  have hib : i ≠ b := by
    intro h; subst h; rw [hfree] at hv'; cases hv'
  exact hl.2 i v' (by rw [upd_ne _ _ hib]; exact hv')

theorem res_of_upd {tail : Bool} {env : Nat → Option SVal} {st : List WVal} {sv : SVal}
    {out : Out} {b : Nat} {v : SVal} (hfree : env b = none)
    (h : Res S M X tail (upd env b v) st sv out) : Res S M X tail env st sv out := by
  cases out with
  | ok wl' st' =>
      obtain ⟨w, h1, h2, h3⟩ := h
      exact ⟨w, h1, h2, lrel_of_upd hfree h3⟩
  | ret w => exact h

theorem res_ok {tail : Bool} {env : Nat → Option SVal} {st wl' : List WVal} {sv : SVal}
    {w : WVal} (h : SRepr S M sv w) (hl : LRel S M X env wl') :
    Res S M X tail env st sv (.ok wl' (w :: st)) :=
  ⟨w, rfl, h, hl⟩

/-- A non-tail node never returns: its run is a normal frame. -/
theorem res_false {env : Nat → Option SVal} {st : List WVal} {sv : SVal} {out : Out}
    (h : Res S M X false env st sv out) :
    ∃ wl' w, out = .ok wl' (w :: st) ∧ SRepr S M sv w ∧ LRel S M X env wl' := by
  cases out with
  | ok wl' st' =>
      obtain ⟨w, rfl, hw, hl⟩ := h
      exact ⟨wl', w, rfl, hw, hl⟩
  | ret w => exact absurd h.1 (by simp)

end LRelLemmas

theorem envTy_upd {M : MCtx} {env : Nat → Option SVal}
    {Γ : Nat → Option Ty} {b : Nat} {v : SVal} {T : Ty}
    (h : EnvTy M env Γ) (hv : HasTy M v T) : EnvTy M (upd env b v) (upd Γ b T) := by
  intro i
  by_cases hib : i = b
  · subst hib
    exact Or.inr ⟨v, T, upd_same _ _ _, upd_same _ _ _, hv⟩
  · rw [upd_ne _ _ hib, upd_ne _ _ hib]
    exact h i

theorem envTy_get {M : MCtx} {env : Nat → Option SVal}
    {Γ : Nat → Option Ty} {i : Nat} {T : Ty}
    (h : EnvTy M env Γ) (hT : Γ i = some T) : ∃ v, env i = some v ∧ HasTy M v T := by
  rcases h i with ⟨_, h2⟩ | ⟨v, T', h1, h2, h3⟩
  · rw [hT] at h2; cases h2
  · rw [hT] at h2; cases h2; exact ⟨v, h1, h3⟩

theorem envTy_free {M : MCtx} {env : Nat → Option SVal}
    {Γ : Nat → Option Ty} {i : Nat}
    (h : EnvTy M env Γ) (hT : Γ i = none) : env i = none := by
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
    (hband : inI64Band k = true)
    (hget : locals[slot]? = some w)
    (hR : CanonRepr S n w)
    (hrun : wRunF host ar callee (eraseL (cmpArmB C slot op k)) locals stack = some out) :
    out = .ok locals (b32 (cmpDen op n k) :: stack) := by
  have hk : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
    simpa [inI64Band, Bool.and_eq_true, decide_eq_true_eq] using hband
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
  (Ctr : Contracts S box add sub mul cmp eq)
  (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) (M : MCtx)
include Ctr

/-- Int against Int without a literal: `__aint_cmp` against `0`, or
    `__aint_eq` (and `i32.eqz`). -/
theorem intCmpTail_step
    (hCmp : host M.cmp = some (2, cmp)) (hEq : host M.eq = some (2, eq))
    (op : BinOp) (hop : op.isArith = false) (a b : Int) (wa wb : WVal)
    (ha : CanonRepr S a wa) (hb : CanonRepr S b wb)
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
theorem arith_step
    (hAdd : host M.add = some (2, add)) (hSub : host M.sub = some (2, sub))
    (hMul : host M.mul = some (2, mul))
    (op : BinOp) (hop : op.isArith = true) (a b : Int) (wa wb : WVal)
    (ha : CanonRepr S a wa) (hb : CanonRepr S b wb)
    (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee [.call (M.arithIdx op)] wl (wb :: wa :: st) = some out) :
    ∃ w, out = .ok wl (w :: st) ∧ SRepr S M (intBin op a b) w ∧
      HasTy M (intBin op a b) .int := by
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

theorem hasTyL_cons_inv {M : MCtx} {svs : List SVal} {t : Ty}
    {ts : List Ty} (h : HasTyL M svs (t :: ts)) :
    ∃ v vs, svs = v :: vs ∧ HasTy M v t ∧ HasTyL M vs ts := by
  cases svs with
  | nil => simp [HasTyL] at h
  | cons v vs => exact ⟨v, vs, rfl, h.1, h.2⟩

theorem hasTyL_nil_inv {M : MCtx} {svs : List SVal}
    (h : HasTyL M svs []) : svs = [] := by
  cases svs with
  | nil => rfl
  | cons _ _ => simp [HasTyL] at h

theorem sreprL_cons_inv {C : Nat} {S : CarrierSpec C} {M : MCtx} {v : SVal}
    {vs : List SVal} {ws : List WVal} (h : SReprL S M (v :: vs) ws) :
    ∃ w ws', ws = w :: ws' ∧ SRepr S M v w ∧ SReprL S M vs ws' := by
  cases ws with
  | nil => simp [SReprL] at h
  | cons w ws' => exact ⟨w, ws', rfl, h.1, h.2⟩

theorem sreprL_nil_inv {C : Nat} {S : CarrierSpec C} {M : MCtx} {ws : List WVal}
    (h : SReprL S M [] ws) : ws = [] := by
  cases ws with
  | nil => rfl
  | cons _ _ => simp [SReprL] at h

theorem hasTy_list {M : MCtx} {v : SVal} {t : Ty} (h : HasTy M v (.list t)) :
    v = .nil t ∨ ∃ x r, v = .cons t x r ∧ HasTy M x t ∧ HasTy M r (.list t) := by
  cases v <;> simp only [HasTy] at h
  · left; rw [h]
  · obtain ⟨rfl, hx, hr⟩ := h
    exact Or.inr ⟨_, _, rfl, hx, hr⟩

/-- A builtin call's tail after its arguments: the Bool builtins are one
    `i32` instruction, `List.prepend` builds the cons cell of the tail's
    type. -/
theorem builtin_step (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    {C : Nat} {S : CarrierSpec C} {M : MCtx}
    (bi : Builtin) (ts : List Ty) (T : Ty) (hty : builtinTy bi ts = some T)
    (svs : List SVal) (ws : List WVal) (hT : HasTyL M svs ts) (hr : SReprL S M svs ws)
    (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee (eraseL (builtinTail M bi (some ts))) wl (ws.reverse ++ st) =
      some out) :
    ∃ sv w, builtinEval bi svs = some sv ∧ HasTy M sv T ∧ SRepr S M sv w ∧
      out = .ok wl (w :: st) := by
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
    cases x <;> cases y <;>
      simp [builtinTail, eraseL, eraseI, wRunF, b32, builtinEval, HasTy, SRepr] at hrun ⊢ <;>
      exact hrun.symm
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
    cases x <;> cases y <;>
      simp [builtinTail, eraseL, eraseI, wRunF, b32, builtinEval, HasTy, SRepr] at hrun ⊢ <;>
      exact hrun.symm
  · simp only [Option.some.injEq] at hty
    subst hty
    obtain ⟨a, svs1, rfl, ha, hT1⟩ := hasTyL_cons_inv hT
    have := hasTyL_nil_inv hT1; subst this
    obtain ⟨wa, ws1, rfl, hwa, hr1⟩ := sreprL_cons_inv hr
    have := sreprL_nil_inv hr1; subst this
    obtain ⟨x, rfl⟩ := hasTy_bool ha
    have hwa' := srepr_b hwa
    subst hwa'
    cases x <;>
      simp [builtinTail, eraseL, eraseI, wRunF, b32, builtinEval, HasTy, SRepr] at hrun ⊢ <;>
      exact hrun.symm
  · rename_i t t'
    split at hty
    · rename_i htt
      subst htt
      simp only [Option.some.injEq] at hty
      subst hty
      obtain ⟨h, svs1, rfl, hh, hT1⟩ := hasTyL_cons_inv hT
      obtain ⟨tl, svs2, rfl, htl, hT2⟩ := hasTyL_cons_inv hT1
      have := hasTyL_nil_inv hT2; subst this
      obtain ⟨wh, ws1, rfl, hwh, hr1⟩ := sreprL_cons_inv hr
      obtain ⟨wt, ws2, rfl, hwt, hr2⟩ := sreprL_cons_inv hr1
      have := sreprL_nil_inv hr2; subst this
      simp [builtinTail, eraseL, eraseI, wRunF, popArgs_two] at hrun
      subst hrun
      refine ⟨.cons t h tl, .structv (M.listStruct t) [wh, wt], ?_, ⟨rfl, hh, htl⟩,
        ⟨wh, wt, rfl, hwh, hwt⟩, rfl⟩
      rcases hasTy_list htl with rfl | ⟨x, r, rfl, _, _⟩ <;> rfl
    · cases hty
  · cases hty

/-! ## Strings and Floats -/

theorem newtype_false {M : MCtx} {tid : Nat} {fts : List Ty} (hR : M.recFields tid = some fts)
    (h2 : 2 ≤ fts.length) : M.newtype tid = false := by
  match fts, h2 with
  | _ :: _ :: _, _ => simp [MCtx.newtype, hR]

/-- A record of two or more fields is the struct of its type. -/
theorem srepr_record {C : Nat} {S : CarrierSpec C} {M : MCtx} {tid : Nat} {fts : List Ty}
    (hR : M.recFields tid = some fts) (h2 : 2 ≤ fts.length) (fs : List SVal) (w : WVal) :
    SRepr S M (.record tid fs) w ↔ ∃ ws, w = .structv (M.structOf tid) ws ∧ SReprL S M fs ws := by
  simp [SRepr, newtype_false hR h2]

theorem wByteListEq_map : ∀ (x y : List Nat),
    wByteListEq (x.map fun (b : Nat) => .i32v (b : Int)) (y.map fun (b : Nat) => .i32v (b : Int)) =
      (x == y)
  | [], [] => rfl
  | [], _ :: _ => rfl
  | _ :: _, [] => rfl
  | a :: x, b :: y => by
      simp only [List.map_cons, wByteListEq, wByteListEq_map x y]
      by_cases h : a = b
      · subst h; simp
      · have : (a :: x == b :: y) = false := by simp [h]
        rw [this]
        have hc : ((a : Int) == (b : Int)) = false := by
          simp only [beq_eq_false_iff_ne, ne_eq]
          intro hab
          exact h (by exact_mod_cast hab)
        rw [hc]
        rfl

theorem stringEqW_strW (M : MCtx) (x y : List Nat) :
    stringEqW (strW M x) (strW M y) = (x == y) := by
  unfold stringEqW strW
  exact wByteListEq_map x y

theorem wByteAppend_map (acc : List WVal) : ∀ x : List Nat,
    wByteAppend (x.map fun (b : Nat) => .i32v (b : Int)) acc =
      some (x.map (fun (b : Nat) => .i32v (b : Int)) ++ acc)
  | [] => rfl
  | a :: x => by
      simp only [List.map_cons, wByteAppend]
      rw [wByteAppend_map acc x]
      rfl

theorem stringConcatParts_of {C : Nat} {S : CarrierSpec C} {M : MCtx} :
    ∀ {svs : List SVal} {ws : List WVal} {bs : List Nat},
    SReprL S M svs ws → strCat svs = some bs →
    stringConcatParts ws = some (bs.map fun (b : Nat) => .i32v (b : Int))
  | [], [], bs, _, hc => by
      simp only [strCat, Option.some.injEq] at hc
      subst hc
      rfl
  | v :: vs, w :: ws, bs, h, hc => by
      cases v with
      | s x =>
          simp only [strCat] at hc
          cases hr : strCat vs with
          | none => simp [hr] at hc
          | some rest =>
              simp only [hr, Option.map_some, Option.some.injEq] at hc
              subst hc
              have hw : w = strW M x := h.1
              subst hw
              have ih := stringConcatParts_of h.2 hr
              simp [stringConcatParts, ih, strW, wByteAppend_map]
      | _ => simp [strCat] at hc
  | [], _ :: _, _, h, _ => by simp [SReprL] at h
  | _ :: _, [], _, h, _ => by simp [SReprL] at h

theorem allStr_cons {t : Ty} {ts : List Ty} (h : allStr (t :: ts) = true) :
    t = .string ∧ (ts = [] ∨ allStr ts = true) := by
  cases t <;> cases ts <;> simp_all [allStr]

theorem strCat_of_allStr {M : MCtx} : ∀ {svs : List SVal} {ts : List Ty},
    HasTyL M svs ts → allStr ts = true → ∃ bs, strCat svs = some bs
  | [], [], _, h => by simp [allStr] at h
  | v :: vs, t :: ts, hT, ha => by
      obtain ⟨rfl, hrest⟩ := allStr_cons ha
      obtain ⟨x, rfl⟩ := hasTy_string hT.1
      rcases hrest with rfl | hrest
      · have := hasTyL_nil_inv hT.2
        subst this
        exact ⟨x, by simp [strCat]⟩
      · obtain ⟨bs, hbs⟩ := strCat_of_allStr hT.2 hrest
        exact ⟨x ++ bs, by simp [strCat, hbs]⟩
  | [], _ :: _, h, _ => by simp [HasTyL] at h
  | _ :: _, [], h, _ => by simp [HasTyL] at h

theorem floatCmp_step (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (op : BinOp) (hop : op.isFloatCmp = true) (x y : UInt64) (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee [floatCmpInstr op] wl (.f64v y :: .f64v x :: st) = some out) :
    ∃ v, floatBin op x y = some (.b v) ∧ out = .ok wl (b32 v :: st) := by
  cases op <;> simp [BinOp.isFloatCmp] at hop <;>
    simp only [floatCmpInstr, wRunF, Option.some.injEq] at hrun <;>
    exact ⟨_, rfl, hrun.symm⟩

section StrHost
variable {C : Nat} {S : CarrierSpec C} {M : MCtx} {host : HostTbl}
  {ar : Nat → Option Nat} {callee : Callee}

/-- `__wasmgc_concat_n` over the Strings on the stack. -/
theorem concat_run (R : XHost S M host) {svs : List SVal} {ws : List WVal} {bs : List Nat}
    (hrep : SReprL S M svs ws) (hcat : strCat svs = some bs) (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee (eraseL (concatB M ws.length)) wl (ws.reverse ++ st) =
      some out) :
    out = .ok wl (strW M bs :: st) := by
  obtain ⟨g, hg, hgc⟩ := R.concat
  have hparts := stringConcatParts_of hrep hcat
  simp only [concatB, eraseL, eraseI, wRunF, popArgs_rev, hg, popArgs_one] at hrun
  cases hc : g [.arr M.strVec ws] with
  | none => simp [hc] at hrun
  | some c =>
      simp only [hc, Option.some.injEq] at hrun
      have h := hgc _ _ hc
      simp [stringConcatW, hparts] at h
      subst h
      try simp only [wRunF, Option.some.injEq] at hrun
      exact hrun.symm

/-- `__wasmgc_string_eq` (and `i32.eqz` for `!=`) on two Strings. -/
theorem streq_step (R : XHost S M host) (op : BinOp) (hs : op.isStrOp = true) (hne : op ≠ .add)
    (x y : List Nat) (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee (eraseL (strOpTail M op)) wl (strW M y :: strW M x :: st) =
      some out) :
    ∃ v, strBin op x y = some (.b v) ∧ out = .ok wl (b32 v :: st) := by
  obtain ⟨g, hg, hgc⟩ := R.streq
  cases hr : g [strW M x, strW M y] with
  | none =>
      cases op <;> simp [BinOp.isStrOp] at hs hne <;>
        simp [strOpTail, eraseL, eraseI, wRunF, hg, popArgs_two, hr] at hrun
  | some r =>
      have h := hgc _ _ _ hr
      rw [stringEqW_strW] at h
      subst h
      cases op <;> simp [BinOp.isStrOp] at hs hne
      · simp [strOpTail, eraseL, eraseI, wRunF, hg, popArgs_two, hr] at hrun
        exact ⟨_, rfl, hrun.symm⟩
      · simp [strOpTail, eraseL, eraseI, wRunF, hg, popArgs_two, hr] at hrun
        refine ⟨_, rfl, ?_⟩
        by_cases hxy : x = y <;> simp [b32, wRunF, hxy] at hrun ⊢ <;> exact hrun.symm

theorem toIndex_cond (n : Int) (len : Nat) (hlen : len < 2147483648) :
    (0 ≤ toIndexW n ∧ toIndexW n % 4294967296 < (len : Int) % 4294967296) ↔
      (0 ≤ n ∧ n < len) := by
  unfold toIndexW
  split <;> omega

/-- The bounds test of the fused `Vector.get`: `__aint_to_index` twice, the
    signed `>= 0` and the unsigned `< array.len`, conjoined. -/
theorem vecCond_run (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) (ti : Nat)
    (g : List WVal → Option WVal) (hg : host ti = some (1, g)) (wl st : List WVal) (wi : WVal)
    (ws : List WVal) (a : Int) (i v ty : Nat)
    (hwi : wl[i]? = some wi) (hwv : wl[v]? = some (.arr ty ws)) (hr : g [wi] = some (.i32v a)) :
    wRunF host ar callee [.localGet i, .call ti, .i32Const 0, .i32GeS, .localGet i, .call ti,
      .localGet v, .arrayLen, .i32LtU, .i32And] wl st =
    some (.ok wl (.i32v (if 0 ≤ a ∧ a % 4294967296 < (ws.length : Int) % 4294967296 then 1 else 0)
      :: st)) := by
  simp [wRunF, hwi, hwv, hg, hr, popArgs_one, b32]
  by_cases h1 : 0 ≤ a <;> by_cases h2 : a % 4294967296 < (ws.length : Int) % 4294967296 <;>
    simp [h1, h2]
  all_goals first | exact h2 | exact Int.not_lt.mp h2

/-- The fused `Vector.get`-or-default: in range, the element; otherwise the
    default's code runs. -/
theorem vecGetOr_step (R : XHost S M host) {v i : Nat} {t : Ty} {vs : List SVal} {n : Int}
    {wv wi : WVal} {wl st : List WVal} {dc : List BI} {out : Out}
    (hwv : wl[v]? = some wv) (hrv : SRepr S M (.vec t vs) wv)
    (hwi : wl[i]? = some wi) (hri : SRepr S M (.i n) wi)
    (hrun : wRunF host ar callee (eraseL (vecGetOrB M v i t dc)) wl st = some out) :
    ((0 ≤ n ∧ n < vs.length) ∧ ∃ x wx, vs[n.toNat]? = some x ∧ SRepr S M x wx ∧
      out = .ok wl (wx :: st)) ∨
    (¬(0 ≤ n ∧ n < vs.length) ∧ wRunF host ar callee (eraseL dc) wl st = some out) := by
  obtain ⟨g, hg, hgc⟩ := R.toIndex
  simp only [SRepr] at hrv hri
  obtain ⟨hlen, ws, rfl, hws⟩ := hrv
  have hwsl : ws.length = vs.length := (sreprL_length hws).symm
  cases hr : g [wi] with
  | none =>
      simp [vecGetOrB, eraseL, eraseI, wRunF, hwi, hg, popArgs_one, hr] at hrun
  | some r =>
      have hr' := hgc n wi r hri.1 hr
      subst hr'
      have hc := toIndex_cond n ws.length (by omega)
      have hpre := vecCond_run host ar callee M.toIndex g hg wl st wi ws (toIndexW n) i v
        (M.vecStruct t) hwi hwv hr
      have hsplit : eraseL (vecGetOrB M v i t dc) =
          [.localGet i, .call M.toIndex, .i32Const 0, .i32GeS, .localGet i, .call M.toIndex,
            .localGet v, .arrayLen, .i32LtU, .i32And] ++
          [.ifElse [.localGet v, .localGet i, .call M.toIndex, .arrayGet (M.vecStruct t)]
            (eraseL dc)] := rfl
      rw [hsplit, wRunF_append, hpre] at hrun
      have hseq := hrun
      simp only [seqOut] at hseq
      rw [wRunF_ifElse_single] at hseq
      by_cases hin : 0 ≤ n ∧ n < vs.length
      · left
        have hC := hc.mpr (by rw [hwsl]; exact hin)
        simp only [hC, and_self, ↓reduceIte, Int.reduceEq] at hseq
        have hti : toIndexW n = n := by unfold toIndexW; split <;> omega
        obtain ⟨x, hx⟩ : ∃ x, vs[n.toNat]? = some x :=
          ⟨vs[n.toNat]'(by omega), List.getElem?_eq_getElem (by omega)⟩
        obtain ⟨wx, hwx, hsx⟩ := sreprL_get hws hx
        refine ⟨hin, x, wx, hx, hsx, ?_⟩
        simp [wRunF, hwi, hwv, hg, popArgs_one, hr, hti, hin.1, hwx] at hseq
        exact hseq.symm
      · right
        have hC : ¬(0 ≤ toIndexW n ∧ toIndexW n % 4294967296 < (ws.length : Int) % 4294967296) :=
          fun h => hin (by have := hc.mp h; rw [hwsl] at this; exact this)
        simp only [hC, ↓reduceIte] at hseq
        exact ⟨hin, hseq⟩

end StrHost

theorem vecGetOr?_some {lb : LazyBuiltin} {o d : Expr} {v i : Nat}
    (h : vecGetOr? lb o d = some (v, i)) :
    lb = .optWithDefault ∧ o = .call (.builtin .vecGet) [.local v, .local i] ∧
      ∃ lit, d = .literal lit := by
  unfold vecGetOr? at h
  split at h
  · simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl⟩ := h
    exact ⟨rfl, rfl, _, rfl⟩
  · cases h

/-- `divOr?` names exactly the fused `Result.withDefault(Int.div/mod(a, b), k)`. -/
theorem divOr?_some {lb : LazyBuiltin} {o d : Expr} {m : Bool} {a b : Expr}
    (h : divOr? lb o d = some (m, a, b)) :
    lb = .resWithDefault ∧ o = .call (.builtin (if m then .intMod else .intDiv)) [a, b] ∧
      ∃ k, d = .literal (.int k) := by
  unfold divOr? at h
  split at h
  · simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl, rfl⟩ := h
    exact ⟨rfl, rfl, _, rfl⟩
  · simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl, rfl⟩ := h
    exact ⟨rfl, rfl, _, rfl⟩
  · cases h

theorem lowerStrArms_head (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
    (bt : Option Ty) (k : List Nat) (b : Expr) (r : Arms) :
    ∃ ys, eraseL (lowerStrArms M X Γ tail bt (.cons (.litStr k) b r)) =
      .localGet X.subj :: ys := by
  simp [lowerStrArms, eraseL, eraseI]

theorem extractB_head (ss idx : Nat) : ∀ (i : Nat) (bs : List Nat) (zs : List BI),
    bs.any (· != noSlot) = true →
    ∃ ys, eraseL (extractB ss idx i bs ++ zs) = .localGet ss :: ys
  | _, [], _, h => by simp at h
  | i, b :: bs, zs, h => by
      by_cases hb : b = noSlot
      · have h' : bs.any (· != noSlot) = true := by simpa [hb] using h
        obtain ⟨ys, hys⟩ := extractB_head ss idx (i + 1) bs zs h'
        refine ⟨ys, ?_⟩
        simpa [extractB, bindFieldB, hb] using hys
      · simp [extractB, bindFieldB, hb, eraseL, eraseI]

theorem tyTupArms_shape {M : MCtx} {n : Nat} {Γ : Nat → Option Ty} {tail : Bool} {tid : Nat}
    {arms : Arms} {T : Ty} {fts : List Ty}
    (hR : M.recFields tid = some fts) (h : tyTupArms M n Γ tail tid arms = some T) :
    ∃ bs b, 2 ≤ fts.length ∧ bs.any (· != noSlot) = true ∧ arms = .cons (.tuple bs) b .nil := by
  cases arms with
  | nil => simp [tyTupArms] at h
  | cons p b rest =>
      cases p
      case tuple bs =>
        cases rest with
        | cons _ _ _ => simp [tyTupArms] at h
        | nil =>
            simp only [tyTupArms, hR] at h
            split at h
            · rename_i hc
              exact ⟨bs, b, hc.1, hc.2, rfl⟩
            · cases h
      all_goals simp [tyTupArms] at h

theorem lowerTupArms_head {M : MCtx} {X : LCtx} {Γ : Nat → Option Ty} {tail : Bool} {tid : Nat}
    {arms : Arms} {bs : List Nat} {b : Expr} (hany : bs.any (· != noSlot) = true)
    (harms : arms = .cons (.tuple bs) b .nil) :
    ∃ ys, eraseL (lowerTupArms M X Γ tail tid arms) = .localGet X.subj :: ys := by
  subst harms
  simp only [lowerTupArms]
  exact extractB_head X.subj (M.structOf tid) 0 bs _ hany

/-! ## Binders, fillers and tag tests

The pieces the match and constructor templates share: the binder
extraction from a struct held in the subject scratch (one `local.set` per
non-ignored binder, in field order, mirrored by `bindTys` on the typing side
and `bindVals` on the value side), the default filler of an unused payload
field, and the Option / Result tag test. -/

theorem run_seq {host : HostTbl} {ar : Nat → Option Nat} {callee : Callee}
    {xs ys : List WInstr} {l st l' st' : List WVal} {out : Out}
    (hx : wRunF host ar callee xs l st = some (.ok l' st'))
    (h : wRunF host ar callee (xs ++ ys) l st = some out) :
    wRunF host ar callee ys l' st' = some out := by
  rw [wRunF_append, hx] at h
  exact h

theorem run_seq_eq {host : HostTbl} {ar : Nat → Option Nat} {callee : Callee}
    {xs ys : List WInstr} {l st l' st' : List WVal}
    (hx : wRunF host ar callee xs l st = some (.ok l' st')) :
    wRunF host ar callee (xs ++ ys) l st = wRunF host ar callee ys l' st' := by
  rw [wRunF_append, hx]
  rfl

theorem hasTyL_nil_inv' {M : MCtx} {svs : List SVal} (h : HasTyL M svs []) : svs = [] := by
  cases svs with
  | nil => rfl
  | cons _ _ => simp [HasTyL] at h

theorem popArgs_three (a b c : WVal) (st : List WVal) :
    popArgs 3 (c :: b :: a :: st) = some ([a, b, c], st) := by
  unfold popArgs
  split
  · rename_i h; simp at h; omega
  · simp

section Binders
variable {C : Nat} {S : CarrierSpec C} {M : MCtx} {X : LCtx}
  (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)

/-- A non-tail run is a run at any tail position. -/
theorem res_any_tail {tail : Bool} {env : Nat → Option SVal} {st : List WVal} {sv : SVal}
    {out : Out} (h : Res S M X false env st sv out) : Res S M X tail env st sv out := by
  cases out with
  | ok wl' st' => exact h
  | ret w => exact absurd h.1 (by simp)

/-- The default filler runs, pushes one value and touches no local. -/
theorem dflt_run (t : Ty) (ht : t.hasDefault = true) (wl st : List WVal) :
    ∃ d, wRunF host ar callee (eraseL (dfltB M t)) wl st = some (.ok wl (d :: st)) := by
  cases t <;> simp_all [Ty.hasDefault, dfltB, eraseL, eraseI, wRunF, popArgs_three]

/-- The Option / Result tag test on the struct in the subject scratch. -/
theorem tagTest_run (ss idx : Nat) (tag : Int) (fs : List WVal) (wl st : List WVal)
    (hss : wl[ss]? = some (.structv idx (.i32v tag :: fs))) :
    wRunF host ar callee (eraseL (tagTestB ss idx)) wl st =
      some (.ok wl (b32 (tag = 1) :: st)) := by
  simp [tagTestB, eraseL, eraseI, wRunF, hss, b32]

/-- One binder read from field `i` of the struct in the scratch `ss`. -/
theorem bindField_run (ss idx i b : Nat) (ws : List WVal) (x : WVal) (wl st : List WVal)
    (hss : wl[ss]? = some (.structv idx ws)) (hx : ws[i]? = some x) :
    wRunF host ar callee (eraseL (bindFieldB ss idx i b)) wl st =
      some (.ok (if b = noSlot then wl else wl.set b x) st) := by
  by_cases hb : b = noSlot
  · simp [bindFieldB, hb, eraseL, wRunF]
  · simp [bindFieldB, hb, eraseL, eraseI, wRunF, hss, hx]

/-- The binder extraction of a constructor arm: it binds exactly what
    `bindVals` binds, keeps the relations, leaves the scratch alone, and a
    result for the extended environment is one for the original. -/
theorem extract_run (ss idx : Nat) (ws : List WVal) :
    ∀ (bs : List Nat) (i : Nat) (vs : List SVal) (ts : List Ty) (fs : List WVal)
      (env : Nat → Option SVal) (Γ Γ' : Nat → Option Ty) (wl st : List WVal),
      X.n ≤ ss →
      wl[ss]? = some (.structv idx ws) →
      (∀ j y, fs[j]? = some y → ws[i + j]? = some y) →
      HasTyL M vs ts → SReprL S M vs fs →
      bindTys X.n Γ bs ts = some Γ' →
      EnvTy M env Γ → LRel S M X env wl →
      ∃ env' wl', bindVals env bs vs = some env' ∧ EnvTy M env' Γ' ∧
        LRel S M X env' wl' ∧ wl'[ss]? = some (.structv idx ws) ∧
        wRunF host ar callee (eraseL (extractB ss idx i bs)) wl st = some (.ok wl' st) ∧
        (∀ tail st' sv o, Res S M X tail env' st' sv o → Res S M X tail env st' sv o)
  | [], i, vs, ts, fs, env, Γ, Γ', wl, st, hssn, hss, hws, hT, hR, hb, henv, hl => by
      cases ts with
      | cons _ _ => simp [bindTys] at hb
      | nil =>
          simp only [bindTys, Option.some.injEq] at hb
          subst hb
          have := hasTyL_nil_inv' hT
          subst this
          exact ⟨env, wl, by simp [bindVals], henv, hl, hss, by simp [extractB, eraseL, wRunF],
            fun _ _ _ _ h => h⟩
  | b :: bs, i, vs, ts, fs, env, Γ, Γ', wl, st, hssn, hss, hws, hT, hR, hb, henv, hl => by
      cases ts with
      | nil => simp [bindTys] at hb
      | cons t ts =>
          obtain ⟨v, vs', rfl, hvT, hT'⟩ := hasTyL_cons_inv hT
          obtain ⟨x, fs', rfl, hvx, hR'⟩ := sreprL_cons_inv hR
          have hx : ws[i]? = some x := by simpa using hws 0 x (by simp)
          have hws' : ∀ j y, fs'[j]? = some y → ws[i + 1 + j]? = some y := by
            intro j y hy
            have := hws (j + 1) y (by simpa using hy)
            rw [show i + (j + 1) = i + 1 + j by omega] at this
            exact this
          have hstep := bindField_run host ar callee ss idx i b ws x wl st hss hx
          by_cases hns : b = noSlot
          · simp only [bindTys, hns, ↓reduceIte] at hb
            rw [show (if b = noSlot then wl else wl.set b x) = wl by simp [hns]] at hstep
            obtain ⟨env', wl', hbv, henv', hl', hss', hrun', hres⟩ :=
              extract_run ss idx ws bs (i + 1) vs' ts fs' env Γ Γ' wl st hssn hss hws' hT' hR'
                hb henv hl
            refine ⟨env', wl', by simp [bindVals, hns, hbv], henv', hl', hss', ?_, hres⟩
            simp only [extractB, eraseL_append]
            rw [run_seq_eq hstep]
            exact hrun'
          · simp only [bindTys, hns, ↓reduceIte] at hb
            split at hb
            · rename_i hfresh
              obtain ⟨hbn, hΓb⟩ := hfresh
              rw [show (if b = noSlot then wl else wl.set b x) = wl.set b x by simp [hns]] at hstep
              have hbs : b ≠ ss := by omega
              have hss1 : (wl.set b x)[ss]? = some (.structv idx ws) := by
                rw [List.getElem?_set_ne (fun h => hbs h)]
                exact hss
              obtain ⟨env', wl', hbv, henv', hl', hss', hrun', hres⟩ :=
                extract_run ss idx ws bs (i + 1) vs' ts fs' (upd env b v) (upd Γ b t) Γ'
                  (wl.set b x) st hssn hss1 hws' hT' hR' hb (envTy_upd henv hvT)
                  (lrel_bind hl hbn hvx)
              refine ⟨env', wl', by simp [bindVals, hns, hbv], henv', hl', hss', ?_, ?_⟩
              · simp only [extractB, eraseL_append]
                rw [run_seq_eq hstep]
                exact hrun'
              · intro tl st' sv o h
                exact res_of_upd (envTy_free henv hΓb) (hres tl st' sv o h)
            · cases hb

/-- A single payload binder (Option / Result), field `i`. -/
theorem bindOne_run (ss idx i b : Nat) (ws : List WVal) (v : SVal) (t : Ty) (x : WVal)
    (env : Nat → Option SVal) (Γ Γ' : Nat → Option Ty) (wl st : List WVal)
    (hssn : X.n ≤ ss) (hss : wl[ss]? = some (.structv idx ws)) (hx : ws[i]? = some x)
    (hvT : HasTy M v t) (hvx : SRepr S M v x) (hb : bindOne X.n Γ b t = some Γ')
    (henv : EnvTy M env Γ) (hl : LRel S M X env wl) :
    ∃ env' wl', bindVals env [b] [v] = some env' ∧ EnvTy M env' Γ' ∧
      LRel S M X env' wl' ∧
      wRunF host ar callee (eraseL (bindFieldB ss idx i b)) wl st = some (.ok wl' st) ∧
      (∀ tail st' sv o, Res S M X tail env' st' sv o → Res S M X tail env st' sv o) := by
  have hws : ∀ j y, [x][j]? = some y → ws[i + j]? = some y := by
    intro j y hy
    cases j with
    | zero => simp at hy; subst hy; simpa using hx
    | succ j => simp at hy
  obtain ⟨env', wl', hbv, henv', hl', _, hrun, hres⟩ :=
    extract_run host ar callee ss idx ws [b] i [v] [t] [x] env Γ Γ' wl st hssn hss hws
      ⟨hvT, trivial⟩ ⟨hvx, trivial⟩ hb henv hl
  refine ⟨env', wl', hbv, henv', hl', ?_, hres⟩
  simpa [extractB, eraseL_append, eraseL] using hrun

end Binders

/-! ## Typing inversions -/

section TypingInv
variable {M : MCtx} {n : Nat} {Γ : Nat → Option Ty} {tail : Bool}

theorem tyOf_litInt_inv {k : Int} {T : Ty}
    (h : tyOf M n Γ tail (.literal (.int k)) = some T) :
    inI64Band k = true ∧ T = .int := by
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
      op.isEquality = true ∧ T = .bool) ∨
    (tyOf M n Γ false l = some .float ∧ tyOf M n Γ false r = some .float ∧
      op.isFloatCmp = true ∧ T = .bool) ∨
    (tyOf M n Γ false l = some .string ∧ tyOf M n Γ false r = some .string ∧
      ((op = .add ∧ T = .string) ∨ (op.isStrOp = true ∧ op ≠ .add ∧ T = .bool))) := by
  simp only [tyOf] at h
  split at h
  · rename_i hl hr
    left
    refine ⟨hl, hr, ?_⟩
    split at h <;> simp_all
  · rename_i hl hr
    right; left
    split at h
    · simp_all
    · simp at h
  · rename_i hl hr
    right; right; left
    split at h
    · simp_all
    · simp at h
  · rename_i hl hr
    right; right; right
    refine ⟨hl, hr, ?_⟩
    split at h
    · simp_all
    · split at h
      · rename_i hne hs
        simp only [Option.some.injEq] at h
        exact Or.inr ⟨hs, hne, h.symm⟩
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
    ∃ fts, M.recFields tid = some fts ∧ tysOf M n Γ fs = some fts ∧ 2 ≤ fts.length ∧
      T = .record tid := by
  simp only [tyOf] at h
  split at h
  · rename_i fts ts hR hts
    split at h
    · rename_i hc
      simp only [Option.some.injEq] at h
      exact ⟨fts, hR, hc.2 ▸ hts, hc.1, h.symm⟩
    · simp at h
  · simp at h

theorem tyOf_proj_inv {tid i : Nat} {base : Expr} {T : Ty}
    (h : tyOf M n Γ tail (.project tid i base) = some T) :
    tyOf M n Γ false base = some (.record tid) ∧
      ∃ fts, M.recFields tid = some fts ∧ 2 ≤ fts.length ∧ fts[i]? = some T := by
  simp only [tyOf] at h
  split at h
  · rename_i tid' fts hb hR
    split at h
    · rename_i hc
      obtain ⟨rfl, h2⟩ := hc
      exact ⟨hb, fts, hR, h2, h⟩
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

theorem tyOf_lazy_inv {lb : LazyBuiltin} {o d : Expr} {T : Ty}
    (h : tyOf M n Γ tail (.call (.lazy lb) [o, d]) = some T) :
    (vecGetOr? lb o d = none ∧ divOr? lb o d = none ∧ ∃ to td, tyOf M n Γ false o = some to ∧
      tyOf M n Γ false d = some td ∧ lazyTy lb to td = some T) ∨
    (∃ v i t, vecGetOr? lb o d = some (v, i) ∧ Γ v = some (.vec t) ∧ Γ i = some .int ∧
      tyOf M n Γ false d = some t ∧ T = t) ∨
    (vecGetOr? lb o d = none ∧ ∃ m a b, divOr? lb o d = some (m, a, b) ∧
      tyOf M n Γ false a = some .int ∧ tyOf M n Γ false b = some .int ∧
      tyOf M n Γ false d = some .int ∧ T = .int) := by
  simp only [tyOf] at h
  split at h
  · rename_i v i hvg
    right; left
    split at h
    · rename_i t td hv hi hd
      split at h
      · rename_i htd
        subst htd
        simp only [Option.some.injEq] at h
        exact ⟨v, i, td, hvg, hv, hi, hd, h.symm⟩
      · simp at h
    · simp at h
  · rename_i hvg
    split at h
    · rename_i p hdg
      right; right
      obtain ⟨m, a, b⟩ := p
      obtain ⟨_, ho, _⟩ := divOr?_some hdg
      split at h
      · rename_i hc
        obtain ⟨hops, hd⟩ := hc
        simp only [Option.some.injEq] at h
        rw [ho] at hops
        simp only [tyDivOperands, tysOf] at hops
        refine ⟨hvg, m, a, b, hdg, ?_, ?_, hd, h.symm⟩
        · cases ha : tyOf M n Γ false a <;> cases hb : tyOf M n Γ false b <;>
            simp only [ha, hb] at hops <;> (try cases hops)
          rename_i ta tb
          cases ta <;> cases tb <;> simp_all
        · cases ha : tyOf M n Γ false a <;> cases hb : tyOf M n Γ false b <;>
            simp only [ha, hb] at hops <;> (try cases hops)
          rename_i ta tb
          cases ta <;> cases tb <;> simp_all
      · cases h
    · rename_i hdg
      left
      refine ⟨hvg, hdg, ?_⟩
      split at h
      · rename_i to td ho hd
        exact ⟨to, td, ho, hd, h⟩
      · simp at h

theorem tyOf_intrinsic_inv {ie : Intrinsic} {args : List Expr} {T : Ty}
    (h : tyOf M n Γ tail (.call (.intrinsic ie) args) = some T) :
    ∃ a k, args = [a, .literal (.int k)] ∧ k ≠ 0 ∧ inI64Band k = true ∧
      tyOf M n Γ false a = some .int ∧ T = .int := by
  rcases args with _ | ⟨a, _ | ⟨dv, _ | ⟨e3, rest⟩⟩⟩
  · simp [tyOf] at h
  · simp [tyOf] at h
  · simp only [tyOf] at h
    cases hdv : divisorLit? dv with
    | none => simp [hdv] at h
    | some k =>
        cases ha : tyOf M n Γ false a with
        | none => simp [hdv, ha] at h
        | some ta =>
            cases ta <;> simp [hdv, ha] at h
            subst h
            unfold divisorLit? at hdv
            split at hdv
            · rename_i k'
              split at hdv
              · rename_i hc
                simp only [Option.some.injEq] at hdv
                subst hdv
                exact ⟨a, k', rfl, hc.1, hc.2, ha, rfl⟩
              · cases hdv
            · cases hdv
  · simp [tyOf] at h

theorem tyOf_litFloat_inv {bits : UInt64} {T : Ty}
    (h : tyOf M n Γ tail (.literal (.float bits)) = some T) : T = .float := by
  simp only [tyOf, Option.some.injEq] at h
  exact h.symm

theorem tyOf_litStr_inv {bytes : List Nat} {T : Ty}
    (h : tyOf M n Γ tail (.literal (.str bytes)) = some T) : T = .string := by
  simp only [tyOf, Option.some.injEq] at h
  exact h.symm

theorem tyOf_interp_inv {parts : List Expr} {T : Ty}
    (h : tyOf M n Γ tail (.interp parts) = some T) :
    ∃ ts, tysOf M n Γ parts = some ts ∧ allStr ts = true ∧ T = .string := by
  simp only [tyOf] at h
  split at h
  · rename_i ts hts
    split at h
    · rename_i ha
      simp only [Option.some.injEq] at h
      exact ⟨ts, hts, ha, h.symm⟩
    · simp at h
  · simp at h

theorem tyOf_list_inv {t : Ty} {items : List Expr} {T : Ty}
    (h : tyOf M n Γ tail (.list t items) = some T) : items = [] ∧ T = .list t := by
  simp only [tyOf] at h
  split at h
  · simp only [Option.some.injEq] at h
    exact ⟨rfl, h.symm⟩
  · simp at h

theorem tyOf_construct_inv {c : CtorTag} {ty : Ty} {args : List Expr} {T : Ty}
    (h : tyOf M n Γ tail (.construct c ty args) = some T) :
    ∃ ts, tysOf M n Γ args = some ts ∧ ctorTy M c ty ts = some T := by
  simp only [tyOf] at h
  split at h
  · rename_i ts hts
    exact ⟨ts, hts, h⟩
  · simp at h

theorem tyOf_match_inv {s : Expr} {arms : Arms} {T : Ty}
    (h : tyOf M n Γ tail (.match_ s arms) = some T) :
    ∃ Ts, tyOf M n Γ false s = some Ts ∧
      ((Ts = .int ∧ arms.firstLit = true ∧ tyIntArms M n Γ tail arms = some T) ∨
       (Ts = .bool ∧ tyBoolArms M n Γ tail arms = some T) ∨
       (∃ t, Ts = .option t ∧ tyOptArms M n Γ tail t arms = some T) ∨
       (∃ t e, Ts = .result t e ∧ tyResArms M n Γ tail t e arms = some T) ∨
       (∃ tid, Ts = .sum tid ∧ sumOk M tid = true ∧ varExhaustive M tid arms = true ∧
          2 ≤ arms.length ∧ tyVarArms M n Γ tail tid arms = some T) ∨
       (Ts = .string ∧ tyStrArms M n Γ tail arms = some T) ∨
       (∃ tid, Ts = .record tid ∧ tyTupArms M n Γ tail tid arms = some T)) := by
  simp only [tyOf] at h
  split at h
  · rename_i hs
    split at h
    · rename_i hfl
      exact ⟨_, hs, Or.inl ⟨rfl, hfl, h⟩⟩
    · cases h
  · rename_i hs
    exact ⟨_, hs, Or.inr (Or.inl ⟨rfl, h⟩)⟩
  · rename_i t hs
    exact ⟨_, hs, Or.inr (Or.inr (Or.inl ⟨t, rfl, h⟩))⟩
  · rename_i t e hs
    exact ⟨_, hs, Or.inr (Or.inr (Or.inr (Or.inl ⟨t, e, rfl, h⟩)))⟩
  · rename_i tid hs
    split at h
    · rename_i hc
      obtain ⟨h1, h2, h3⟩ := hc
      exact ⟨_, hs, Or.inr (Or.inr (Or.inr (Or.inr (Or.inl ⟨tid, rfl, h1, h2, h3, h⟩))))⟩
    · cases h
  · rename_i hs
    exact ⟨_, hs, Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inl ⟨rfl, h⟩)))))⟩
  · rename_i tid hs
    exact ⟨_, hs, Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr ⟨tid, rfl, h⟩)))))⟩
  · cases h

end TypingInv

theorem litInt?_some {e : Expr} {k : Int} (h : litInt? e = some k) : e = .literal (.int k) := by
  unfold litInt? at h
  split at h <;> simp_all

theorem slot?_some {e : Expr} {i : Nat} (h : slot? e = some i) : e = .local i := by
  unfold slot? at h
  split at h <;> simp_all

/-! ## Match shapes: first-match meaning of the admitted arm shapes -/

theorem lowerIntArms_firstLit (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
    (sc : List BI) (bt : Option Ty) {arms : Arms} (h : arms.firstLit = true) :
    ∃ ys, lowerIntArms M X Γ tail sc bt arms = sc ++ ys := by
  cases arms with
  | nil => simp [Arms.firstLit] at h
  | cons p b rest =>
      cases p <;> simp [Arms.firstLit] at h
      exact ⟨_, rfl⟩

theorem ctorFields_lt {M : MCtx} {tid c : Nat} {fts : List Ty}
    (h : ctorFields M tid c = some fts) :
    ∃ cs, M.sumCtors tid = some cs ∧ c < cs.length := by
  unfold ctorFields at h
  cases hs : M.sumCtors tid with
  | none => simp [hs] at h
  | some cs =>
      refine ⟨cs, rfl, ?_⟩
      simp only [hs, Option.bind_some] at h
      rcases Nat.lt_or_ge c cs.length with hc | hc
      · exact hc
      · rw [List.getElem?_eq_none hc] at h; cases h

/-- `sumOk` makes the constructor struct indices of one sum pairwise
    distinct, so the interpreter's exact `ref.test` separates them. -/
theorem sumOk_inj {M : MCtx} {tid a b : Nat} {fa fb : List Ty} (hok : sumOk M tid = true)
    (ha : ctorFields M tid a = some fa) (hb : ctorFields M tid b = some fb)
    (h : M.ctorStruct tid a = M.ctorStruct tid b) : a = b := by
  obtain ⟨cs, hcs, hal⟩ := ctorFields_lt ha
  obtain ⟨cs', hcs', hbl⟩ := ctorFields_lt hb
  rw [hcs] at hcs'
  cases hcs'
  unfold sumOk at hok
  rw [hcs] at hok
  simp only [Bool.and_eq_true, List.all_eq_true, List.mem_range] at hok
  have := hok.2 a hal b hbl
  simpa [h] using this

theorem varExhaustive_covers {M : MCtx} {tid c : Nat} {arms : Arms} {fts : List Ty}
    (hex : varExhaustive M tid arms = true) (hc : ctorFields M tid c = some fts) :
    coversB c arms = true := by
  obtain ⟨cs, hcs, hcl⟩ := ctorFields_lt hc
  unfold varExhaustive at hex
  rw [hcs] at hex
  simp only [List.all_eq_true, List.mem_range] at hex
  exact hex c hcl

theorem optPick_spec {p1 p2 : Pat} {swap : Bool} {sb : Nat}
    (h : optPick p1 p2 = some (swap, sb)) :
    (swap = false ∧ p1 = .ctor .some [sb] ∧ (p2 = .ctor .none [] ∨ p2 = .wild)) ∨
    (swap = true ∧ p1 = .ctor .none [] ∧
      (p2 = .ctor .some [sb] ∨ (p2 = .wild ∧ sb = noSlot))) := by
  unfold optPick at h
  split at h <;> simp_all

theorem resPick_spec {p1 p2 : Pat} {swap : Bool} {ob eb : Nat}
    (h : resPick p1 p2 = some (swap, ob, eb)) :
    (swap = false ∧ p1 = .ctor .ok [ob] ∧
      (p2 = .ctor .err [eb] ∨ (p2 = .wild ∧ eb = noSlot))) ∨
    (swap = true ∧ p1 = .ctor .err [eb] ∧
      (p2 = .ctor .ok [ob] ∨ (p2 = .wild ∧ ob = noSlot))) := by
  unfold resPick at h
  split at h <;> simp_all

section ShapeEval
variable (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal)

/-- The Option shapes pick the `Some` arm for a `Some` value, binding `sb`. -/
theorem evalOpt_some {p1 p2 : Pat} {b1 b2 : Expr} {swap : Bool} {sb : Nat} {t : Ty}
    {x : SVal} (h : optPick p1 p2 = some (swap, sb)) :
    evalArms F env (.some t x) (.cons p1 b1 (.cons p2 b2 .nil)) =
      match bindVals env [sb] [x] with
      | some env' => eval F env' (if swap then b2 else b1)
      | none => none := by
  rcases optPick_spec h with ⟨rfl, rfl, rfl | rfl⟩ | ⟨rfl, rfl, rfl | ⟨rfl, rfl⟩⟩ <;>
    simp [evalArms, patMatch, bindVals, noSlot] <;> rfl

theorem evalOpt_none {p1 p2 : Pat} {b1 b2 : Expr} {swap : Bool} {sb : Nat} {t : Ty}
    (h : optPick p1 p2 = some (swap, sb)) :
    evalArms F env (.none t) (.cons p1 b1 (.cons p2 b2 .nil)) =
      eval F env (if swap then b1 else b2) := by
  rcases optPick_spec h with ⟨rfl, rfl, rfl | rfl⟩ | ⟨rfl, rfl, rfl | ⟨rfl, rfl⟩⟩ <;>
    simp [evalArms, patMatch, bindVals]

theorem evalRes_ok {p1 p2 : Pat} {b1 b2 : Expr} {swap : Bool} {ob eb : Nat} {t e : Ty}
    {x : SVal} (h : resPick p1 p2 = some (swap, ob, eb)) :
    evalArms F env (.ok t e x) (.cons p1 b1 (.cons p2 b2 .nil)) =
      match bindVals env [ob] [x] with
      | some env' => eval F env' (if swap then b2 else b1)
      | none => none := by
  rcases resPick_spec h with ⟨rfl, rfl, rfl | ⟨rfl, rfl⟩⟩ | ⟨rfl, rfl, rfl | ⟨rfl, rfl⟩⟩ <;>
    simp [evalArms, patMatch, bindVals, noSlot] <;> rfl

theorem evalRes_err {p1 p2 : Pat} {b1 b2 : Expr} {swap : Bool} {ob eb : Nat} {t e : Ty}
    {x : SVal} (h : resPick p1 p2 = some (swap, ob, eb)) :
    evalArms F env (.err t e x) (.cons p1 b1 (.cons p2 b2 .nil)) =
      match bindVals env [eb] [x] with
      | some env' => eval F env' (if swap then b1 else b2)
      | none => none := by
  rcases resPick_spec h with ⟨rfl, rfl, rfl | ⟨rfl, rfl⟩⟩ | ⟨rfl, rfl, rfl | ⟨rfl, rfl⟩⟩ <;>
    simp [evalArms, patMatch, bindVals, noSlot] <;> rfl

end ShapeEval

theorem run_test (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (ss ty t : Nat) (fs : List WVal) (ys : List WInstr) (wl st : List WVal)
    (h : wl[ss]? = some (.structv t fs)) :
    wRunF host ar callee (.localGet ss :: .refTest ty :: ys) wl st =
      wRunF host ar callee ys wl (b32 (t = ty) :: st) := by
  simp [wRunF, h]

theorem run_localSet (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (j : Nat) (w : WVal) (ys : List WInstr) (wl st : List WVal) :
    wRunF host ar callee (.localSet j :: ys) wl (w :: st) =
      wRunF host ar callee ys (wl.set j w) st := by
  simp [wRunF]

/-! ## Reading back a stashed scratch local

A scratch local need not exist (`LRel` does not demand it). Every template
that stashes a value reads the scratch back as its very next instruction, and
that read fails on a missing local, so a successful run proves the stash
landed. -/

theorem stash_read {host : HostTbl} {ar : Nat → Option Nat} {callee : Callee}
    {j : Nat} {w : WVal} {wl st : List WVal} {xs ys : List WInstr} {out : Out}
    (hx : xs = .localGet j :: ys)
    (h : wRunF host ar callee xs (wl.set j w) st = some out) :
    (wl.set j w)[j]? = some w := by
  by_cases hj : j < wl.length
  · exact List.getElem?_set_self hj
  · subst hx
    have hn : (wl.set j w)[j]? = none := by
      rw [List.getElem?_eq_none]
      simp only [List.length_set]
      omega
    simp [wRunF, hn] at h

theorem lowerOptArms_head {M : MCtx} {X : LCtx} {Γ : Nat → Option Ty} {tail : Bool}
    {bt : Option Ty} {t : Ty} {arms : Arms} {T : Ty}
    (h : tyOptArms M X.n Γ tail t arms = some T) :
    ∃ ys, eraseL (lowerOptArms M X Γ tail bt t arms) = .localGet X.subj :: ys := by
  match arms, h with
  | .nil, h => simp [tyOptArms] at h
  | .cons _ _ .nil, h => simp [tyOptArms] at h
  | .cons _ _ (.cons _ _ (.cons _ _ _)), h => simp [tyOptArms] at h
  | .cons p1 b1 (.cons p2 b2 .nil), h =>
      simp only [tyOptArms] at h
      cases hpk : optPick p1 p2 with
      | none => simp [hpk] at h
      | some pr =>
          obtain ⟨swap, sb⟩ := pr
          cases swap <;> simp [lowerOptArms, hpk, tagTestB, eraseL, eraseI]

theorem lowerResArms_head {M : MCtx} {X : LCtx} {Γ : Nat → Option Ty} {tail : Bool}
    {bt : Option Ty} {t e : Ty} {arms : Arms} {T : Ty}
    (h : tyResArms M X.n Γ tail t e arms = some T) :
    ∃ ys, eraseL (lowerResArms M X Γ tail bt t e arms) = .localGet X.subj :: ys := by
  match arms, h with
  | .nil, h => simp [tyResArms] at h
  | .cons _ _ .nil, h => simp [tyResArms] at h
  | .cons _ _ (.cons _ _ (.cons _ _ _)), h => simp [tyResArms] at h
  | .cons p1 b1 (.cons p2 b2 .nil), h =>
      simp only [tyResArms] at h
      cases hpk : resPick p1 p2 with
      | none => simp [hpk] at h
      | some pr =>
          obtain ⟨swap, ob, eb⟩ := pr
          cases swap <;> simp [lowerResArms, hpk, tagTestB, eraseL, eraseI]

theorem lowerVarArms_head {M : MCtx} {X : LCtx} {Γ : Nat → Option Ty} {tail : Bool}
    {bt : Option Ty} {tid : Nat} {arms : Arms} {T : Ty}
    (h : tyVarArms M X.n Γ tail tid arms = some T) (hlen : 2 ≤ arms.length) :
    ∃ ys, eraseL (lowerVarArms M X Γ tail bt tid arms) = .localGet X.subj :: ys := by
  match arms, h, hlen with
  | .nil, _, hlen => simp [Arms.length] at hlen
  | .cons _ _ .nil, _, hlen => simp [Arms.length] at hlen
  | .cons p b (.cons p' b' r), h, _ =>
      simp only [tyVarArms] at h
      cases p with
      | ctor cc bs =>
          cases cc with
          | user tid' c => simp [lowerVarArms, eraseL, eraseI]
          | _ => simp [Pat.isWild, varArmΓ] at h
      | _ => simp [Pat.isWild, varArmΓ] at h

/-! ## S-3: the exact `ref.test` on constructor structs is the wasm test

`declaredFinal` / `inRecGroup` read the pinned rec-group entries through the
binary format (`subtype ::= 0x4f vec(typeidx) comptype` is `sub final`).
`GcTestSpec` states the two wasm GC facts the argument uses: subtyping is
reflexive, and a type declared final has no subtype in its own rec group
other than itself (validation rejects a `sub` naming a final type, and
distinct positions of one rec group are distinct types under iso-recursive
equivalence). Under `S3Pin`, the interpreter's `t = ty` and the wasm
`t <: ty` agree on every pair of constructor structs of one sum, which are
the only pairs a typed cascade tests. -/

def declaredFinal (entries : List (List Nat)) (idx : Nat) : Prop :=
  ∃ e, entries[idx]? = some e ∧ e.head? = some 0x4f

def inRecGroup (entries : List (List Nat)) (idx : Nat) : Prop :=
  idx < entries.length

structure GcTestSpec (entries : List (List Nat)) (sub : Nat → Nat → Prop) : Prop where
  refl : ∀ t, sub t t
  final_sub : ∀ t ty, inRecGroup entries t → inRecGroup entries ty →
    declaredFinal entries ty → sub t ty → t = ty

theorem s3Pin_facts {M : MCtx} {tid ncs : Nat} {entries : List (List Nat)}
    (h : S3Pin M tid ncs entries = true) {c : Nat} (hc : c < ncs) :
    declaredFinal entries (M.ctorStruct tid c) ∧ inRecGroup entries (M.ctorStruct tid c) := by
  unfold S3Pin at h
  simp only [List.all_eq_true, List.mem_range] at h
  have hc' := h c hc
  split at hc'
  · rename_i e hd he hh
    refine ⟨⟨e, he, ?_⟩, ?_⟩
    · unfold ctorEntryHeader at hh
      cases hu : uleb32 (M.sumRoot tid) with
      | none => simp [hu] at hh
      | some u =>
          simp only [hu, Option.map_some, Option.some.injEq] at hh
          subst hh
          cases e with
          | nil => simp [List.isPrefixOf] at hc'
          | cons x xs =>
              simp only [List.cons_append, List.isPrefixOf, Bool.and_eq_true, beq_iff_eq] at hc'
              simp [hc'.1]
    · rcases Nat.lt_or_ge (M.ctorStruct tid c) entries.length with hl | hl
      · exact hl
      · rw [List.getElem?_eq_none hl] at he; cases he
  · cases hc'

/-- Under the pin, the interpreter's exact `ref.test` on two constructors of
    one sum is the wasm subtype test. -/
theorem ctor_refTest_exact {M : MCtx} {tid ncs : Nat} {entries : List (List Nat)}
    {sub : Nat → Nat → Prop} (hspec : GcTestSpec entries sub)
    (hpin : S3Pin M tid ncs entries = true) {a b : Nat} (ha : a < ncs) (hb : b < ncs) :
    M.ctorStruct tid a = M.ctorStruct tid b ↔ sub (M.ctorStruct tid a) (M.ctorStruct tid b) := by
  obtain ⟨_, hga⟩ := s3Pin_facts hpin ha
  obtain ⟨hfb, hgb⟩ := s3Pin_facts hpin hb
  exact ⟨fun h => h ▸ hspec.refl _, hspec.final_sub _ _ hga hgb hfb⟩


/-! ## Euclidean division: the intrinsic and the fused guarded form -/

/-- The zero test of the divisor on its carrier word (`$magf` null and
    `$small == 0`) is exactly `y = 0` on any represented word: a Small word
    carries its value, and a limb-carrying word is never zero. -/
theorem divZeroTest_run {C : Nat} {S : CarrierSpec C} (host : HostTbl)
    (ar : Nat → Option Nat) (callee : Callee) (j : Nat) (y : Int) (wb : WVal)
    (hb : S.Repr y wb) (ys : List WInstr) (wl st : List WVal) (hj : wl[j]? = some wb) :
    wRunF host ar callee (.localGet j :: .structGet C 1 :: .refIsNull :: .localGet j ::
        .structGet C 0 :: .i64Eqz :: .i32And :: ys) wl st =
      wRunF host ar callee ys wl (b32 (decide (y = 0)) :: st) := by
  rcases S.car y wb hb with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
  · have := S.smallElim y s sg hb
    subst this
    by_cases h0 : s = 0 <;> simp [wRunF, hj, b32, h0]
  · have hne := (S.bigElim y s lty les sg hb).2
    by_cases h0 : s = 0 <;> simp [wRunF, hj, b32, hne, h0]

/-- The fused guarded division after its three operands: the default when
    the divisor is zero, else the helper's Euclidean quotient / remainder. -/
theorem divOr_run {C : Nat} {S : CarrierSpec C} {M : MCtx} {host : HostTbl}
    (hCarrier : M.carrier = C) (R : XHost S M host)
    (ar : Nat → Option Nat) (callee : Callee) (X : LCtx) (isMod : Bool)
    (x y k : Int) (wa wb wd : WVal) (ha : CanonRepr S x wa) (hb : CanonRepr S y wb)
    (hd : CanonRepr S k wd) (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee (eraseL (divOrB M X isMod)) wl (wd :: wb :: wa :: st) =
      some out) :
    ∃ w, out = .ok (((wl.set (X.cmp + 3) wd).set (X.cmp + 2) wb).set (X.cmp + 1) wa)
        (w :: st) ∧
      CanonRepr S (if y = 0 then k else if isMod then x % y else x / y) w := by
  obtain ⟨g, hg, hgc⟩ := R.divmod
  subst hCarrier
  simp only [divOrB, eraseL, eraseI] at hrun
  rw [run_localSet, run_localSet, run_localSet] at hrun
  generalize hwl' : ((wl.set (X.cmp + 3) wd).set (X.cmp + 2) wb).set (X.cmp + 1) wa = wl' at hrun
  by_cases h2 : X.cmp + 2 < wl.length
  · have g2 : wl'[X.cmp + 2]? = some wb := by
      subst hwl'; simp [List.getElem?_set, h2]
    have g1 : wl'[X.cmp + 1]? = some wa := by
      subst hwl'; simp [List.getElem?_set]; omega
    rw [divZeroTest_run host ar callee (X.cmp + 2) y wb hb.1 _ wl' st g2] at hrun
    simp only [b32] at hrun
    by_cases hy : y = 0
    · simp only [hy, decide_true, ↓reduceIte] at hrun
      rw [wRunF_ifElse_single] at hrun
      simp only [Int.one_ne_zero, ↓reduceIte] at hrun
      by_cases h3 : X.cmp + 3 < wl.length
      · have g3 : wl'[X.cmp + 3]? = some wd := by
          subst hwl'; simp [List.getElem?_set, h3]
        simp [wRunF, g3] at hrun
        subst hrun
        exact ⟨wd, rfl, by simpa [hy] using hd⟩
      · have g3 : wl'[X.cmp + 3]? = none := by
          subst hwl'; simp [List.getElem?_set]; omega
        simp [wRunF, g3] at hrun
    · simp only [hy, decide_false, Bool.false_eq_true, ↓reduceIte] at hrun
      rw [wRunF_ifElse_single, if_pos rfl] at hrun
      cases hr : g [wa, wb, .i32v (if isMod then 1 else 0)] with
      | none => simp [wRunF, g1, g2, hg, popArgs_three, hr] at hrun
      | some r =>
          simp [wRunF, g1, g2, hg, popArgs_three, hr] at hrun
          subst hrun
          have hc := hgc x y wa wb _ r ha hb hy (by cases isMod <;> simp) hr
          refine ⟨r, rfl, ?_⟩
          cases isMod <;> simpa [hy] using hc
  · have g2 : wl'[X.cmp + 2]? = none := by
      subst hwl'; simp [List.getElem?_set]; omega
    simp [wRunF, g2] at hrun

/-- A Euclidean intrinsic after its two operands. -/
theorem intrinsic_run {C : Nat} {S : CarrierSpec C} {M : MCtx} {host : HostTbl}
    (R : XHost S M host) (ar : Nat → Option Nat) (callee : Callee) (ie : Intrinsic)
    (x y : Int) (hy : y ≠ 0) (wa wb : WVal) (ha : CanonRepr S x wa) (hb : CanonRepr S y wb)
    (wl st : List WVal) (out : Out)
    (hrun : wRunF host ar callee [.i32Const ie.flag, .call M.divmod] wl (wb :: wa :: st) =
      some out) :
    ∃ w sv, intrinsicEval ie [.i x, .i y] = some sv ∧ out = .ok wl (w :: st) ∧
      SRepr S M sv w := by
  obtain ⟨g, hg, hgc⟩ := R.divmod
  cases hr : g [wa, wb, .i32v ie.flag] with
  | none => simp [wRunF, hg, popArgs_three, hr] at hrun
  | some r =>
      simp [wRunF, hg, popArgs_three, hr] at hrun
      subst hrun
      have hc := hgc x y wa wb _ r ha hb hy (by cases ie <;> simp [Intrinsic.flag]) hr
      cases ie
      · exact ⟨r, .i (x / y), by simp [intrinsicEval, hy], rfl, by
          simpa [SRepr, Intrinsic.flag] using hc⟩
      · exact ⟨r, .i (x % y), by simp [intrinsicEval, hy], rfl, by
          simpa [SRepr, Intrinsic.flag] using hc⟩

/-! ## The agreement theorem

ONE statement, by structural induction on the grammar (mutual over the nested
argument lists). The context fixes: the carrier specification and the named
host contracts at their indices, an arbitrary opaque `callee`, and a
`Contract` for every callee the typing admits. -/

section Agreement
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
include Ctr hNegC hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R hCallees

mutual
theorem agreement :
    ∀ (e : Expr) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal) (out : Out),
      tyOf M X.n Γ tail e = some T →
      EnvTy M env Γ →
      LRel S M X env wl →
      wRunF host ar callee (lowerW M X Γ tail e) wl st = some out →
      ∃ sv, eval F env e = some sv ∧ HasTy M sv T ∧
        Res S M X tail env st sv out
  | .literal (.int k), Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨hband, rfl⟩ := tyOf_litInt_inv hty
      have hk : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
        simpa [inI64Band, Bool.and_eq_true, decide_eq_true_eq] using hband
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
  | .literal (.float bits), Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      have hT := tyOf_litFloat_inv hty
      subst hT
      simp [lowerW, lowerB, eraseL, eraseI, wRunF] at hrun
      subst hrun
      exact ⟨.f bits, by simp [eval], by simp [HasTy], res_ok (by simp [SRepr]) hl⟩
  | .literal (.str bytes), Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      have hT := tyOf_litStr_inv hty
      subst hT
      simp [lowerW, lowerB, strLitB, eraseL, eraseI, wRunF] at hrun
      subst hrun
      refine ⟨.s bytes, by simp [eval], by simp [HasTy], res_ok ?_ hl⟩
      simp [SRepr, strW, Function.comp_def]
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
      have henv' : EnvTy M (upd env b sv1) (upd Γ b Tv) := envTy_upd henv hT1
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
      simp only [lowerW, lowerB, eraseL_append, hts] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
        agreementArgs args Γ env ts wl st o1 hts henv hl h1
      simp only [seqOut] at hseq
      obtain ⟨sv, w, hbe, hT, hsw, rfl⟩ :=
        builtin_step host ar callee bi ts T hbt svs ws hTs hrep wl1 st out hseq
      exact ⟨sv, by simp [eval, hevs, hbe], hT, res_ok hsw hl1⟩
  | .call (.intrinsic ie) args, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨a, k, rfl, hk0, hband, hta, rfl⟩ := tyOf_intrinsic_inv hty
      simp only [lowerW, lowerB, lowerArgsB, eraseL_append, List.append_nil,
        List.append_assoc] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨sva, heva, hTa, hresa⟩ := agreement a Γ env false .int wl st o1 hta henv hl h1
      obtain ⟨wl1, wa, rfl, hwa, hl1⟩ := res_false hresa
      simp only [seqOut] at hseq
      obtain ⟨o2, h2, hseq2⟩ := run_split hseq
      obtain ⟨svd, hevd, hTd, hresd⟩ := agreement (.literal (.int k)) Γ env false .int wl1
        (wa :: st) o2 (by simp [tyOf, hband]) henv hl1 h2
      obtain ⟨wl2, wd, rfl, hwd, hl2⟩ := res_false hresd
      simp only [seqOut, eraseL, eraseI] at hseq2
      obtain ⟨x, rfl⟩ := hasTy_int hTa
      simp only [eval, Option.some.injEq] at hevd
      subst hevd
      obtain ⟨w, sv, hsv, rfl, hw⟩ := intrinsic_run R ar callee ie x k hk0 wa wd
        (by simpa [SRepr] using hwa) (by simpa [SRepr] using hwd) wl2 st out hseq2
      refine ⟨sv, by simp [eval, evalArgs, heva, hsv], ?_, res_ok hw hl2⟩
      cases ie <;> simp [intrinsicEval, hk0] at hsv <;> subst hsv <;> simp [HasTy]
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
      rcases tyOf_binOp_inv hty with ⟨htl, htr, hT⟩ | ⟨htl, htr, hop, rfl⟩ |
        ⟨htl, htr, hop, rfl⟩ | ⟨htl, htr, hsop⟩
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
                  have hget : (wl1.set X.cmp w1)[X.cmp]? = some w1 := stash_read rfl hseq
                  have hout := cmpArm_step S host ar callee op.flip (flip_isArith hA) kk X.cmp
                    (wl1.set X.cmp w1) st m w1 out hband hget hw1 hseq
                  subst hout
                  refine ⟨.b (cmpDen op kk m), ?_, by simp [HasTy],
                    res_ok ?_ (lrel_set_free w1 hl1 hl1.1.1)⟩
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
                      have hget : (wl1.set X.cmp w1)[X.cmp]? = some w1 := stash_read rfl hseq
                      have hout := cmpArm_step S host ar callee op hA kk X.cmp (wl1.set X.cmp w1) st m w1
                        out hband hget hw1 hseq
                      subst hout
                      refine ⟨.b (cmpDen op m kk), ?_, by simp [HasTy],
                        res_ok ?_ (lrel_set_free w1 hl1 hl1.1.1)⟩
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
            hAdd hSub hMul op hA x y wa wb hwa hwb wl2 st out hseq2
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
      · -- Float operands: one `f64` comparison
        simp only [lowerW, lowerB, htl, eraseL_append, eraseL, eraseI, List.append_assoc] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sva, heva, hTa, hresa⟩ := agreement l Γ env false .float wl st o1 htl henv hl h1
        obtain ⟨wl1, wa, rfl, hwa, hl1⟩ := res_false hresa
        simp only [seqOut] at hseq
        obtain ⟨o2, h2, hseq2⟩ := run_split hseq
        obtain ⟨svb, hevb, hTb, hresb⟩ :=
          agreement r Γ env false .float wl1 (wa :: st) o2 htr henv hl1 h2
        obtain ⟨wl2, wb, rfl, hwb, hl2⟩ := res_false hresb
        simp only [seqOut] at hseq2
        obtain ⟨x, rfl⟩ := hasTy_float hTa
        obtain ⟨y, rfl⟩ := hasTy_float hTb
        simp only [SRepr] at hwa hwb
        subst hwa hwb
        obtain ⟨v, hfb, rfl⟩ := floatCmp_step host ar callee op hop x y wl2 st out hseq2
        exact ⟨.b v, by simp [eval, heva, hevb, hfb], by simp [HasTy],
          res_ok (by simp [SRepr]) hl2⟩
      · -- String operands: concatenation, or byte equality
        simp only [lowerW, lowerB, htl, eraseL_append, List.append_assoc] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sva, heva, hTa, hresa⟩ := agreement l Γ env false .string wl st o1 htl henv hl h1
        obtain ⟨wl1, wa, rfl, hwa, hl1⟩ := res_false hresa
        simp only [seqOut] at hseq
        obtain ⟨o2, h2, hseq2⟩ := run_split hseq
        obtain ⟨svb, hevb, hTb, hresb⟩ :=
          agreement r Γ env false .string wl1 (wa :: st) o2 htr henv hl1 h2
        obtain ⟨wl2, wb, rfl, hwb, hl2⟩ := res_false hresb
        simp only [seqOut] at hseq2
        obtain ⟨x, rfl⟩ := hasTy_string hTa
        obtain ⟨y, rfl⟩ := hasTy_string hTb
        simp only [SRepr] at hwa hwb
        subst hwa hwb
        rcases hsop with ⟨rfl, rfl⟩ | ⟨hs, hne, rfl⟩
        · have hout := concat_run R (svs := [.s x, .s y]) (ws := [strW M x, strW M y])
            (bs := x ++ y) ⟨rfl, rfl, trivial⟩ (by simp [strCat]) wl2 st out
            (by simpa [strOpTail] using hseq2)
          subst hout
          exact ⟨.s (x ++ y), by simp [eval, heva, hevb, strBin], by simp [HasTy],
            res_ok (by simp [SRepr]) hl2⟩
        · obtain ⟨v, hsb, rfl⟩ := streq_step R op hs hne x y wl2 st out hseq2
          exact ⟨.b v, by simp [eval, heva, hevb, hsb], by simp [HasTy],
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
          have hw1' : CanonRepr S x w1 := by simpa [SRepr] using hw1
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
      obtain ⟨fts, hR, hts, h2, rfl⟩ := tyOf_rec_inv hty
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
      · exact (srepr_record hR h2 _ _).mpr ⟨ws, rfl, hrep⟩
  | .project tid i base, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨htb, fts, hR, h2, hi⟩ := tyOf_proj_inv hty
      simp only [lowerW, lowerB, eraseL_append] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨sv1, hev1, hT1, hres1⟩ :=
        agreement base Γ env false (.record tid) wl st o1 htb henv hl h1
      obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
      obtain ⟨fs, fts', rfl, hR', hfs⟩ := hasTy_record hT1
      rw [hR] at hR'
      cases hR'
      obtain ⟨sv, hsv, hTsv⟩ := hasTyL_get hfs hi
      obtain ⟨ws, rfl, hws⟩ := (srepr_record hR h2 _ _).mp hw1
      obtain ⟨w, hw, hwr⟩ := sreprL_get hws hsv
      simp only [seqOut, eraseL, eraseI] at hseq
      simp [wRunF, hw] at hseq
      subst hseq
      exact ⟨sv, by simp [eval, hev1, hsv], hTsv, res_ok hwr hl1⟩

  | .call (.lazy _) [], _, _, _, _, _, _, _, hty, _, _, _ => by simp [tyOf] at hty
  | .call (.lazy _) [_], _, _, _, _, _, _, _, hty, _, _, _ => by simp [tyOf] at hty
  | .call (.lazy _) (_ :: _ :: _ :: _), _, _, _, _, _, _, _, hty, _, _, _ => by
      simp [tyOf] at hty
  | .call (.lazy lb) [o, d], Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      rcases tyOf_lazy_inv hty with ⟨hvg, hdg, to, td, hto, htd, hlz⟩ |
        ⟨v, i, t, hvg, hΓv, hΓi, htd, rfl⟩ | ⟨hvg, m, a, b, hdg, hta, htb, htd, rfl⟩
      · -- the boxed `withDefault`: the default runs only on the `None` / `Err` side
        cases lb with
        | optWithDefault =>
            obtain ⟨t, rfl⟩ : ∃ t, to = .option t := by
              cases to <;> simp [lazyTy] at hlz
              exact ⟨_, rfl⟩
            simp only [lazyTy] at hlz
            split at hlz
            · rename_i hc
              obtain ⟨htd', _⟩ := hc
              subst td
              simp only [Option.some.injEq] at hlz
              subst T
              simp only [lowerW, lowerB, hvg, hdg, hto, eraseL_append, List.append_assoc] at hrun
              obtain ⟨o1, h1, hseq⟩ := run_split hrun
              obtain ⟨sv1, hev1, hT1, hres1⟩ :=
                agreement o Γ env false (.option t) wl st o1 hto henv hl h1
              obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
              simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append] at hseq
              rw [run_localSet] at hseq
              have hl2 := lrel_set_free w1 hl1 hl1.1.2.1
              have hss : (wl1.set X.subj w1)[X.subj]? = some w1 := stash_read rfl hseq
              rcases hasTy_option hT1 with rfl | ⟨x, rfl, hx⟩
              · simp only [SRepr] at hw1
                obtain ⟨dd, rfl⟩ := hw1
                rw [run_seq_eq (tagTest_run host ar callee _ _ 0 [dd] _ st hss)] at hseq
                simp only [b32, Int.reduceEq, decide_false, decide_true, Bool.false_eq_true, ↓reduceIte] at hseq
                rw [wRunF_ifElse_single] at hseq
                simp only [Int.reduceEq, ↓reduceIte] at hseq
                obtain ⟨sv, hev, hT, hres⟩ := agreement d Γ env false t _ st out htd henv hl2 hseq
                exact ⟨sv, by simp [eval, hev1, hev], hT, res_any_tail hres⟩
              · simp only [SRepr] at hw1
                obtain ⟨xw, rfl, hxw⟩ := hw1
                rw [run_seq_eq (tagTest_run host ar callee _ _ 1 [xw] _ st hss)] at hseq
                simp only [b32, Int.reduceEq, decide_false, decide_true, Bool.false_eq_true, ↓reduceIte] at hseq
                rw [wRunF_ifElse_single] at hseq
                simp [wRunF, hss] at hseq
                subst hseq
                exact ⟨x, by simp [eval, hev1], hx, res_ok hxw hl2⟩
            · cases hlz
        | resWithDefault =>
            obtain ⟨t, e, rfl⟩ : ∃ t e, to = .result t e := by
              cases to <;> simp [lazyTy] at hlz
              exact ⟨_, _, rfl⟩
            simp only [lazyTy] at hlz
            split at hlz
            · rename_i hc
              obtain ⟨htd', _⟩ := hc
              subst td
              simp only [Option.some.injEq] at hlz
              subst T
              simp only [lowerW, lowerB, hvg, hdg, hto, eraseL_append, List.append_assoc] at hrun
              obtain ⟨o1, h1, hseq⟩ := run_split hrun
              obtain ⟨sv1, hev1, hT1, hres1⟩ :=
                agreement o Γ env false (.result t e) wl st o1 hto henv hl h1
              obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
              simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append] at hseq
              rw [run_localSet] at hseq
              have hl2 := lrel_set_free w1 hl1 hl1.1.2.1
              have hss : (wl1.set X.subj w1)[X.subj]? = some w1 := stash_read rfl hseq
              rcases hasTy_result hT1 with ⟨x, rfl, hx⟩ | ⟨x, rfl, hx⟩
              · simp only [SRepr] at hw1
                obtain ⟨xw, dd, rfl, hxw⟩ := hw1
                rw [run_seq_eq (tagTest_run host ar callee _ _ 1 [xw, dd] _ st hss)] at hseq
                simp only [b32, Int.reduceEq, decide_false, decide_true, Bool.false_eq_true, ↓reduceIte] at hseq
                rw [wRunF_ifElse_single] at hseq
                simp [wRunF, hss] at hseq
                subst hseq
                exact ⟨x, by simp [eval, hev1], hx, res_ok hxw hl2⟩
              · simp only [SRepr] at hw1
                obtain ⟨dd, xw, rfl, hxw⟩ := hw1
                rw [run_seq_eq (tagTest_run host ar callee _ _ 0 [dd, xw] _ st hss)] at hseq
                simp only [b32, Int.reduceEq, decide_false, decide_true, Bool.false_eq_true, ↓reduceIte] at hseq
                rw [wRunF_ifElse_single] at hseq
                simp only [Int.reduceEq, ↓reduceIte] at hseq
                obtain ⟨sv, hev, hT, hres⟩ := agreement d Γ env false t _ st out htd henv hl2 hseq
                exact ⟨sv, by simp [eval, hev1, hev], hT, res_any_tail hres⟩
            · cases hlz
      · -- `Option.withDefault(Vector.get(v, i), <literal>)`, fused
        obtain ⟨rfl, rfl, -⟩ := vecGetOr?_some hvg
        obtain ⟨vv, hvv, hTv⟩ := envTy_get henv hΓv
        obtain ⟨iv, hiv, hTi⟩ := envTy_get henv hΓi
        obtain ⟨vs, rfl, hall⟩ := hasTy_vec hTv
        obtain ⟨n, rfl⟩ := hasTy_int hTi
        obtain ⟨_, wv, hwv, hrv⟩ := hl.2 v _ hvv
        obtain ⟨_, wi, hwi, hri⟩ := hl.2 i _ hiv
        simp only [lowerW, lowerB, hvg, hΓv] at hrun
        rcases vecGetOr_step R hwv hrv hwi hri hrun with ⟨hin, x, wx, hx, hwx, rfl⟩ | ⟨hout, hd⟩
        · refine ⟨x, ?_, hasTyAll_get hall hx, res_ok hwx hl⟩
          obtain ⟨hlt, hx'⟩ := List.getElem?_eq_some_iff.mp hx
          simp [eval, evalArgs, hvv, hiv, builtinEval, hin, List.getElem?_eq_getElem hlt, hx']
        · obtain ⟨sv, hev, hT, hres⟩ := agreement d Γ env false T wl st out htd henv hl hd
          refine ⟨sv, ?_, hT, res_any_tail hres⟩
          simp [eval, evalArgs, hvv, hiv, builtinEval, hout, hev]
      · -- `Result.withDefault(Int.div/mod(a, b), k)`, fused: the three
        -- operands once each, then the zero test and `__aint_divmod`
        obtain ⟨rfl, rfl, k, rfl⟩ := divOr?_some hdg
        have hband : inI64Band k = true := by
          simp only [tyOf] at htd
          split at htd
          · assumption
          · cases htd
        have htail : ∀ ts, builtinTail M (if m then .intMod else .intDiv) ts = [] := by
          intro ts; cases m <;> rfl
        simp only [lowerW, lowerB, hvg, hdg, lowerArgsB, htail, eraseL_append, List.append_nil,
          List.append_assoc] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sva, heva, hTa, hresa⟩ := agreement a Γ env false .int wl st o1 hta henv hl h1
        obtain ⟨wl1, wa, rfl, hwa, hl1⟩ := res_false hresa
        simp only [seqOut] at hseq
        obtain ⟨o2, h2, hseq2⟩ := run_split hseq
        obtain ⟨svb, hevb, hTb, hresb⟩ :=
          agreement b Γ env false .int wl1 (wa :: st) o2 htb henv hl1 h2
        obtain ⟨wl2, wb, rfl, hwb, hl2⟩ := res_false hresb
        simp only [seqOut] at hseq2
        obtain ⟨o3, h3, hseq3⟩ := run_split hseq2
        obtain ⟨svd, hevd, hTd, hresd⟩ := agreement (.literal (.int k)) Γ env false .int wl2
          (wb :: wa :: st) o3 htd henv hl2 h3
        obtain ⟨wl3, wd, rfl, hwd, hl3⟩ := res_false hresd
        simp only [seqOut] at hseq3
        obtain ⟨x, rfl⟩ := hasTy_int hTa
        obtain ⟨y, rfl⟩ := hasTy_int hTb
        simp only [eval, Option.some.injEq] at hevd
        subst hevd
        obtain ⟨w, rfl, hw⟩ := divOr_run hCarrier R ar callee X m x y k wa wb wd
          (by simpa [SRepr] using hwa) (by simpa [SRepr] using hwb)
          (by simpa [SRepr] using hwd) wl3 st out hseq3
        have hc := hl3.1.1
        have hl4 := lrel_set_free wa (lrel_set_free wb (lrel_set_free wd hl3
          (show X.n ≤ X.cmp + 3 by omega)) (show X.n ≤ X.cmp + 2 by omega))
          (show X.n ≤ X.cmp + 1 by omega)
        refine ⟨if y = 0 then .i k else if m then .i (x % y) else .i (x / y), ?_, ?_,
          res_ok ?_ hl4⟩
        · by_cases hy : y = 0 <;> cases m <;>
            simp [eval, evalArgs, heva, hevb, builtinEval, hy]
        · by_cases hy : y = 0 <;> cases m <;> simp [HasTy, hy]
        · by_cases hy : y = 0 <;> cases m <;> simpa [SRepr, hy] using hw

  | .construct c ty args, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨ts, hts, hct⟩ := tyOf_construct_inv hty
      cases c with
      | user tid k =>
          cases ty with
          | int => simp [ctorTy] at hct
          | bool => simp [ctorTy] at hct
          | record _ => simp [ctorTy] at hct
          | option _ => simp [ctorTy] at hct
          | result _ _ => simp [ctorTy] at hct
          | eqref => simp [ctorTy] at hct
          | float => simp [ctorTy] at hct
          | string => simp [ctorTy] at hct
          | vec _ => simp [ctorTy] at hct
          | list _ => simp [ctorTy] at hct
          | «opaque» _ => simp [ctorTy] at hct
          | sum tid' =>
          simp only [ctorTy] at hct
          split at hct
          · rename_i hc
            obtain ⟨htid, _, hcf⟩ := hc
            subst htid
            cases hct
            simp only [lowerW, lowerB, eraseL_append] at hrun
            obtain ⟨o1, h1, hseq⟩ := run_split hrun
            obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
              agreementArgs args Γ env ts wl st o1 hts henv hl h1
            have hlen : args.length = ws.length := by
              rw [← tysOf_length hts, ← hasTyL_length hTs, sreprL_length hrep]
            simp only [seqOut, eraseL, eraseI] at hseq
            simp only [hlen, wRunF, popArgs_rev, Option.some.injEq] at hseq
            subst hseq
            refine ⟨.variant tid k svs, by simp [eval, hevs, ctorVal], ?_, res_ok ?_ hl1⟩
            · simp only [HasTy, true_and]
              exact ⟨ts, hcf, hTs⟩
            · simp only [SRepr]
              exact ⟨ws, rfl, hrep⟩
          · cases hct
      | some =>
          cases ty with
          | int => simp [ctorTy] at hct
          | bool => simp [ctorTy] at hct
          | record _ => simp [ctorTy] at hct
          | sum _ => simp [ctorTy] at hct
          | result _ _ => simp [ctorTy] at hct
          | eqref => simp [ctorTy] at hct
          | float => simp [ctorTy] at hct
          | string => simp [ctorTy] at hct
          | vec _ => simp [ctorTy] at hct
          | list _ => simp [ctorTy] at hct
          | «opaque» _ => simp [ctorTy] at hct
          | option t =>
          simp only [ctorTy] at hct
          split at hct
          · rename_i hc
            obtain ⟨rfl, _⟩ := hc
            cases hct
            simp only [lowerW, lowerB, eraseL_append, List.append_assoc] at hrun
            simp only [eraseL, eraseI, List.cons_append, List.nil_append, wRunF] at hrun
            obtain ⟨o1, h1, hseq⟩ := run_split hrun
            obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
              agreementArgs args Γ env [t] wl (.i32v 1 :: st) o1 hts henv hl h1
            obtain ⟨v, svs', rfl, hv, hTs'⟩ := hasTyL_cons_inv hTs
            have := hasTyL_nil_inv hTs'
            subst this
            obtain ⟨w, ws', rfl, hw, hrep'⟩ := sreprL_cons_inv hrep
            have := sreprL_nil_inv hrep'
            subst this
            simp [seqOut, wRunF, popArgs_two] at hseq
            subst hseq
            refine ⟨.some t v, by simp [eval, hevs, ctorVal], by simp [HasTy, hv], res_ok ?_ hl1⟩
            simp only [SRepr]
            exact ⟨w, rfl, hw⟩
          · cases hct
      | none =>
          cases ty with
          | int => simp [ctorTy] at hct
          | bool => simp [ctorTy] at hct
          | record _ => simp [ctorTy] at hct
          | sum _ => simp [ctorTy] at hct
          | result _ _ => simp [ctorTy] at hct
          | eqref => simp [ctorTy] at hct
          | float => simp [ctorTy] at hct
          | string => simp [ctorTy] at hct
          | vec _ => simp [ctorTy] at hct
          | list _ => simp [ctorTy] at hct
          | «opaque» _ => simp [ctorTy] at hct
          | option t =>
          simp only [ctorTy] at hct
          split at hct
          · rename_i hc
            obtain ⟨rfl, hdt⟩ := hc
            cases hct
            simp only [lowerW, lowerB, eraseL_append, List.append_assoc] at hrun
            simp only [eraseL, eraseI, List.cons_append, List.nil_append, wRunF] at hrun
            obtain ⟨dd, hdd⟩ := dflt_run host ar callee (M := M) t hdt wl (.i32v 0 :: st)
            rw [run_seq_eq hdd] at hrun
            obtain ⟨o1, h1, hseq⟩ := run_split hrun
            obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
              agreementArgs args Γ env [] wl (dd :: .i32v 0 :: st) o1 hts henv hl h1
            have := hasTyL_nil_inv hTs
            subst this
            have := sreprL_nil_inv hrep
            subst this
            simp [seqOut, eraseL, eraseI, wRunF, popArgs_two] at hseq
            subst hseq
            refine ⟨.none t, by simp [eval, hevs, ctorVal], by simp [HasTy], res_ok ?_ hl1⟩
            simp only [SRepr]
            exact ⟨dd, rfl⟩
          · cases hct
      | ok =>
          cases ty with
          | int => simp [ctorTy] at hct
          | bool => simp [ctorTy] at hct
          | record _ => simp [ctorTy] at hct
          | sum _ => simp [ctorTy] at hct
          | option _ => simp [ctorTy] at hct
          | eqref => simp [ctorTy] at hct
          | float => simp [ctorTy] at hct
          | string => simp [ctorTy] at hct
          | vec _ => simp [ctorTy] at hct
          | list _ => simp [ctorTy] at hct
          | «opaque» _ => simp [ctorTy] at hct
          | result t e =>
          simp only [ctorTy] at hct
          split at hct
          · rename_i hc
            obtain ⟨rfl, _, hde⟩ := hc
            cases hct
            simp only [lowerW, lowerB, eraseL_append, List.append_assoc] at hrun
            simp only [eraseL, eraseI, List.cons_append, List.nil_append, wRunF] at hrun
            obtain ⟨o1, h1, hseq⟩ := run_split hrun
            obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
              agreementArgs args Γ env [t] wl (.i32v 1 :: st) o1 hts henv hl h1
            obtain ⟨v, svs', rfl, hv, hTs'⟩ := hasTyL_cons_inv hTs
            have := hasTyL_nil_inv hTs'
            subst this
            obtain ⟨w, ws', rfl, hw, hrep'⟩ := sreprL_cons_inv hrep
            have := sreprL_nil_inv hrep'
            subst this
            simp only [seqOut, List.reverse_cons, List.reverse_nil, List.nil_append,
              List.cons_append] at hseq
            obtain ⟨dd, hdd⟩ := dflt_run host ar callee (M := M) e hde wl1 (w :: .i32v 1 :: st)
            rw [run_seq_eq hdd] at hseq
            simp [eraseL, eraseI, wRunF, popArgs_three] at hseq
            subst hseq
            refine ⟨.ok t e v, by simp [eval, hevs, ctorVal], by simp [HasTy, hv],
              res_ok ?_ hl1⟩
            simp only [SRepr]
            exact ⟨w, dd, rfl, hw⟩
          · cases hct
      | err =>
          cases ty with
          | int => simp [ctorTy] at hct
          | bool => simp [ctorTy] at hct
          | record _ => simp [ctorTy] at hct
          | sum _ => simp [ctorTy] at hct
          | option _ => simp [ctorTy] at hct
          | eqref => simp [ctorTy] at hct
          | float => simp [ctorTy] at hct
          | string => simp [ctorTy] at hct
          | vec _ => simp [ctorTy] at hct
          | list _ => simp [ctorTy] at hct
          | «opaque» _ => simp [ctorTy] at hct
          | result t e =>
          simp only [ctorTy] at hct
          split at hct
          · rename_i hc
            obtain ⟨rfl, hdt, _⟩ := hc
            cases hct
            simp only [lowerW, lowerB, eraseL_append, List.append_assoc] at hrun
            simp only [eraseL, eraseI, List.cons_append, List.nil_append, wRunF] at hrun
            obtain ⟨dd, hdd⟩ := dflt_run host ar callee (M := M) t hdt wl (.i32v 0 :: st)
            rw [run_seq_eq hdd] at hrun
            obtain ⟨o1, h1, hseq⟩ := run_split hrun
            obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
              agreementArgs args Γ env [e] wl (dd :: .i32v 0 :: st) o1 hts henv hl h1
            obtain ⟨v, svs', rfl, hv, hTs'⟩ := hasTyL_cons_inv hTs
            have := hasTyL_nil_inv hTs'
            subst this
            obtain ⟨w, ws', rfl, hw, hrep'⟩ := sreprL_cons_inv hrep
            have := sreprL_nil_inv hrep'
            subst this
            simp [seqOut, eraseL, eraseI, wRunF, popArgs_three] at hseq
            subst hseq
            refine ⟨.err t e v, by simp [eval, hevs, ctorVal], by simp [HasTy, hv],
              res_ok ?_ hl1⟩
            simp only [SRepr]
            exact ⟨dd, w, rfl, hw⟩
          · cases hct
  | .match_ s arms, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨Ts, hts, hcases⟩ := tyOf_match_inv hty
      rcases hcases with ⟨rfl, hfl, hta⟩ | ⟨rfl, hta⟩ | ⟨t, rfl, hta⟩ | ⟨t, e, rfl, hta⟩ |
        ⟨tid, rfl, hok, hex, hlen2, hta⟩ | ⟨rfl, hta⟩ | ⟨tid, rfl, hta⟩
      · -- Int literal cascade: the subject is re-run per arm
        simp only [lowerW, lowerB, hts] at hrun
        obtain ⟨ys, hys⟩ := lowerIntArms_firstLit M X Γ tail (lowerB M X Γ false s)
          (tyOf M X.n Γ tail (.match_ s arms)) hfl
        have hrun0 := hrun
        rw [hys, eraseL_append] at hrun0
        obtain ⟨o1, h1, _⟩ := run_split hrun0
        obtain ⟨sv1, hev1, hT1, _⟩ := agreement s Γ env false .int wl st o1 hts henv hl h1
        obtain ⟨x, rfl⟩ := hasTy_int hT1
        have hsc : ∀ wl0 st0 out0, LRel S M X env wl0 →
            wRunF host ar callee (eraseL (lowerB M X Γ false s)) wl0 st0 = some out0 →
            ∃ wl1 w, out0 = .ok wl1 (w :: st0) ∧ CanonRepr S x w ∧
              LRel S M X env wl1 := by
          intro wl0 st0 out0 hl0 hr0
          obtain ⟨sv0, hev0, _, hres0⟩ := agreement s Γ env false .int wl0 st0 out0 hts henv hl0 hr0
          rw [hev1] at hev0
          cases hev0
          obtain ⟨wl1, w, rfl, hw, hl1⟩ := res_false hres0
          exact ⟨wl1, w, rfl, by simpa [SRepr] using hw, hl1⟩
        obtain ⟨sv, hev, hT, hres⟩ :=
          agreementIntArms arms Γ env tail T wl st out (lowerB M X Γ false s) _ x hsc hta henv hl
            hrun
        exact ⟨sv, by simp [eval, hev1, hev], hT, hres⟩
      · -- Bool: one `if` on the subject
        simp only [lowerW, lowerB, hts, eraseL_append] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sv1, hev1, hT1, hres1⟩ := agreement s Γ env false .bool wl st o1 hts henv hl h1
        obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
        obtain ⟨x, rfl⟩ := hasTy_bool hT1
        have hw1' := srepr_b hw1
        subst hw1'
        simp only [seqOut] at hseq
        obtain ⟨sv, hev, hT, hres⟩ :=
          agreementBoolArms arms Γ env tail T wl1 st out _ x hta henv hl1 hseq
        exact ⟨sv, by simp [eval, hev1, hev], hT, hres⟩
      · -- Option: stash, tag test, payload binder
        simp only [lowerW, lowerB, hts, eraseL_append, List.append_assoc] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sv1, hev1, hT1, hres1⟩ :=
          agreement s Γ env false (.option t) wl st o1 hts henv hl h1
        obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
        simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append] at hseq
        rw [run_localSet] at hseq
        have hss : (wl1.set X.subj w1)[X.subj]? = some w1 := by
          obtain ⟨ys, hys⟩ := lowerOptArms_head (bt := tyOf M X.n Γ tail (.match_ s arms)) hta
          exact stash_read hys hseq
        obtain ⟨sv, hev, hT, hres⟩ :=
          agreementOptArms arms Γ env tail T (wl1.set X.subj w1) st out _ t sv1 w1 hss hw1 hT1
            hta henv (lrel_set_free w1 hl1 hl1.1.2.1) hseq
        exact ⟨sv, by simp [eval, hev1, hev], hT, hres⟩
      · -- Result: stash, tag test, payload binders
        simp only [lowerW, lowerB, hts, eraseL_append, List.append_assoc] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sv1, hev1, hT1, hres1⟩ :=
          agreement s Γ env false (.result t e) wl st o1 hts henv hl h1
        obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
        simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append] at hseq
        rw [run_localSet] at hseq
        have hss : (wl1.set X.subj w1)[X.subj]? = some w1 := by
          obtain ⟨ys, hys⟩ := lowerResArms_head (bt := tyOf M X.n Γ tail (.match_ s arms)) hta
          exact stash_read hys hseq
        obtain ⟨sv, hev, hT, hres⟩ :=
          agreementResArms arms Γ env tail T (wl1.set X.subj w1) st out _ t e sv1 w1 hss hw1 hT1
            hta henv (lrel_set_free w1 hl1 hl1.1.2.1) hseq
        exact ⟨sv, by simp [eval, hev1, hev], hT, hres⟩
      · -- user variant: stash, `ref.test` cascade
        simp only [lowerW, lowerB, hts, eraseL_append, List.append_assoc] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sv1, hev1, hT1, hres1⟩ :=
          agreement s Γ env false (.sum tid) wl st o1 hts henv hl h1
        obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
        obtain ⟨cv, fs, fts, rfl, hcf, hfs⟩ := hasTy_sum hT1
        simp only [SRepr] at hw1
        obtain ⟨ws, rfl, hws⟩ := hw1
        simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append] at hseq
        rw [run_localSet] at hseq
        have hss : (wl1.set X.subj (.structv (M.ctorStruct tid cv) ws))[X.subj]? =
            some (.structv (M.ctorStruct tid cv) ws) := by
          obtain ⟨ys, hys⟩ :=
            lowerVarArms_head (bt := tyOf M X.n Γ tail (.match_ s arms)) hta hlen2
          exact stash_read hys hseq
        obtain ⟨sv, hev, hT, hres⟩ :=
          agreementVarArms arms Γ env tail T _ st out _ tid cv fs ws fts hss hws hcf hfs
            (varExhaustive_covers hex hcf)
            (fun c fc hc heq => sumOk_inj hok hc hcf heq) hta henv
            (lrel_set_free _ hl1 hl1.1.2.1) hseq
        exact ⟨sv, by simp [eval, hev1, hev], hT, hres⟩
      · -- String: stash, literal cascade through `__wasmgc_string_eq`
        simp only [lowerW, lowerB, hts, eraseL_append, List.append_assoc] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sv1, hev1, hT1, hres1⟩ := agreement s Γ env false .string wl st o1 hts henv hl h1
        obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
        obtain ⟨x, rfl⟩ := hasTy_string hT1
        simp only [SRepr] at hw1
        subst hw1
        simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append] at hseq
        rw [run_localSet] at hseq
        have hss : (∃ k b r, arms = .cons (.litStr k) b r) →
            (wl1.set X.subj (strW M x))[X.subj]? = some (strW M x) := by
          rintro ⟨k, b, r, rfl⟩
          obtain ⟨ys, hys⟩ := lowerStrArms_head M X Γ tail (tyOf M X.n Γ tail (.match_ s
            (.cons (.litStr k) b r))) k b r
          exact stash_read hys hseq
        obtain ⟨sv, hev, hT, hres⟩ :=
          agreementStrArms arms Γ env tail T _ st out _ x hss hta henv
            (lrel_set_free _ hl1 hl1.1.2.1) hseq
        exact ⟨sv, by simp [eval, hev1, hev], hT, hres⟩
      · -- tuple destructure: stash, bind the components
        simp only [lowerW, lowerB, hts, eraseL_append, List.append_assoc] at hrun
        obtain ⟨o1, h1, hseq⟩ := run_split hrun
        obtain ⟨sv1, hev1, hT1, hres1⟩ :=
          agreement s Γ env false (.record tid) wl st o1 hts henv hl h1
        obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
        obtain ⟨fs, fts, rfl, hR, hfs⟩ := hasTy_record hT1
        obtain ⟨bs, b, h2, hany, harms⟩ := tyTupArms_shape hR hta
        obtain ⟨ws, rfl, hws⟩ := (srepr_record hR h2 _ _).mp hw1
        simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append] at hseq
        rw [run_localSet] at hseq
        have hss : (wl1.set X.subj (.structv (M.structOf tid) ws))[X.subj]? =
            some (.structv (M.structOf tid) ws) := by
          obtain ⟨ys, hys⟩ := lowerTupArms_head (M := M) (X := X) (Γ := Γ) (tail := tail)
            (tid := tid) hany harms
          exact stash_read hys hseq
        obtain ⟨sv, hev, hT, hres⟩ :=
          agreementTupArms arms Γ env tail T _ st out tid fs ws fts hss hws hR hfs hta henv
            (lrel_set_free _ hl1 hl1.1.2.1) hseq
        exact ⟨sv, by simp [eval, hev1, hev], hT, hres⟩
  | .interp parts, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨ts, hts, hall, rfl⟩ := tyOf_interp_inv hty
      simp only [lowerW, lowerB, eraseL_append] at hrun
      obtain ⟨o1, h1, hseq⟩ := run_split hrun
      obtain ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, hl1⟩ :=
        agreementArgs parts Γ env ts wl st o1 hts henv hl h1
      obtain ⟨bs, hbs⟩ := strCat_of_allStr hTs hall
      have hlen : parts.length = ws.length := by
        rw [← tysOf_length hts, ← hasTyL_length hTs, sreprL_length hrep]
      simp only [seqOut, hlen] at hseq
      have hout := concat_run R hrep hbs wl1 st out hseq
      subst hout
      exact ⟨.s bs, by simp [eval, hevs, hbs], by simp [HasTy], res_ok (by simp [SRepr]) hl1⟩
  | .list t items, Γ, env, tail, T, wl, st, out, hty, henv, hl, hrun => by
      obtain ⟨rfl, rfl⟩ := tyOf_list_inv hty
      simp [lowerW, lowerB, eraseL, eraseI, wRunF] at hrun
      subst hrun
      exact ⟨.nil t, by simp [eval], by simp [HasTy], res_ok (by simp [SRepr]) hl⟩

theorem agreementArgs :
    ∀ (es : List Expr) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (Ts : List Ty)
      (wl st : List WVal) (out : Out),
      tysOf M X.n Γ es = some Ts →
      EnvTy M env Γ →
      LRel S M X env wl →
      wRunF host ar callee (lowerArgsW M X Γ es) wl st = some out →
      ∃ svs ws wl', out = .ok wl' (ws.reverse ++ st) ∧
        evalArgs F env es = some svs ∧ HasTyL M svs Ts ∧
        SReprL S M svs ws ∧ LRel S M X env wl'
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

/-- The Int literal cascade: `sc` is the subject's code, re-run per literal
    arm; every run yields the same Int `x` (evaluation is pure). -/
theorem agreementIntArms :
    ∀ (arms : Arms) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal) (out : Out) (sc : List BI) (bt : Option Ty) (x : Int),
      (∀ wl0 st0 out0, LRel S M X env wl0 →
        wRunF host ar callee (eraseL sc) wl0 st0 = some out0 →
        ∃ wl1 w, out0 = .ok wl1 (w :: st0) ∧ CanonRepr S x w ∧
          LRel S M X env wl1) →
      tyIntArms M X.n Γ tail arms = some T →
      EnvTy M env Γ → LRel S M X env wl →
      wRunF host ar callee (eraseL (lowerIntArms M X Γ tail sc bt arms)) wl st = some out →
      ∃ sv, evalArms F env (.i x) arms = some sv ∧ HasTy M sv T ∧
        Res S M X tail env st sv out
  | .nil, _, _, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by simp [tyIntArms] at hty
  | .cons p b rest, Γ, env, tail, T, wl, st, out, sc, bt, x, hsc, hty, henv, hl, hrun => by
      cases p with
      | litInt k =>
          simp only [tyIntArms] at hty
          by_cases hband : inI64Band k = true
          · simp only [hband, ↓reduceIte] at hty
            cases hb : tyOf M X.n Γ tail b with
            | none => simp [hb] at hty
            | some T1 =>
              cases hr : tyIntArms M X.n Γ tail rest with
              | none => simp [hb, hr] at hty
              | some T2 =>
                simp only [hb, hr] at hty
                by_cases hTT : T1 = T2
                · subst hTT
                  simp only [↓reduceIte, Option.some.injEq] at hty
                  subst hty
                  simp only [lowerIntArms, eraseL_append] at hrun
                  obtain ⟨o1, h1, hseq⟩ := run_split hrun
                  obtain ⟨wl1, w, rfl, hw, hl1⟩ := hsc wl st o1 hl h1
                  simp only [seqOut, eraseL, eraseI] at hseq
                  have hk : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
                    simpa [inI64Band, Bool.and_eq_true, decide_eq_true_eq]
                      using hband
                  cases hbx : box [.i64v k] with
                  | none => simp [wRunF, hBox, popArgs_one, hbx] at hseq
                  | some wk =>
                      have hwk := Ctr.hBox k wk hk.1 hk.2 hbx
                      cases hq : eq [w, wk] with
                      | none => simp [wRunF, hBox, hEq, popArgs_one, popArgs_two, hbx, hq] at hseq
                      | some r =>
                          have hr' := Ctr.hEq x k w wk r hw.1 hwk.1 hw.2 hwk.2 hq
                          subst hr'
                          have hpre : wRunF host ar callee [.i64Const k, .call M.box, .call M.eq]
                              wl1 (w :: st) = some (.ok wl1 (.i32v (eqW x k) :: st)) := by
                            simp [wRunF, hBox, hEq, popArgs_one, popArgs_two, hbx, hq]
                          have hseq' := run_seq (xs := [.i64Const k, .call M.box, .call M.eq])
                            (ys := [.ifElse (eraseL (lowerB M X Γ tail b))
                              (eraseL (lowerIntArms M X Γ tail sc bt rest))]) hpre hseq
                          rw [wRunF_ifElse_single] at hseq'
                          by_cases hxk : x = k
                          · subst hxk
                            simp only [eqW, ↓reduceIte, Int.reduceEq] at hseq'
                            obtain ⟨sv, hev, hT, hres⟩ :=
                              agreement b Γ env tail T1 wl1 st out hb henv hl1 hseq'
                            exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                          · simp only [eqW, hxk, ↓reduceIte] at hseq'
                            obtain ⟨sv, hev, hT, hres⟩ :=
                              agreementIntArms rest Γ env tail T1 wl1 st out sc bt x hsc hr henv
                                hl1 hseq'
                            exact ⟨sv, by simp [evalArms, patMatch, hxk, hev], hT, hres⟩
                · simp [hTT] at hty
          · simp [hband] at hty
      | wild =>
          cases rest with
          | cons _ _ _ => simp [tyIntArms] at hty
          | nil =>
              simp only [tyIntArms] at hty
              simp only [lowerIntArms] at hrun
              obtain ⟨sv, hev, hT, hres⟩ := agreement b Γ env tail T wl st out hty henv hl hrun
              exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
      | bind sl =>
          cases rest with
          | cons _ _ _ => simp [tyIntArms] at hty
          | nil =>
              simp only [tyIntArms] at hty
              split at hty
              · rename_i hc
                obtain ⟨hsn, hΓs, hns⟩ := hc
                simp only [lowerIntArms, eraseL_append, List.append_assoc] at hrun
                obtain ⟨o1, h1, hseq⟩ := run_split hrun
                obtain ⟨wl1, w, rfl, hw, hl1⟩ := hsc wl st o1 hl h1
                simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append] at hseq
                rw [run_localSet] at hseq
                have henv' : EnvTy M (upd env sl (.i x)) (upd Γ sl .int) :=
                  envTy_upd henv (by simp [HasTy])
                have hl' := lrel_bind hl1 hsn (v := .i x) (by simpa [SRepr] using hw)
                obtain ⟨sv, hev, hT, hres⟩ :=
                  agreement b (upd Γ sl .int) (upd env sl (.i x)) tail T _ st out hty henv' hl'
                    hseq
                exact ⟨sv, by simp [evalArms, patMatch, bindVals, hns, hev], hT,
                  res_of_upd (envTy_free henv hΓs) hres⟩
              · cases hty
      | litBool _ => simp [tyIntArms] at hty
      | ctor _ _ => simp [tyIntArms] at hty
      | litStr _ => simp [tyIntArms] at hty
      | tuple _ => simp [tyIntArms] at hty

/-- The two-arm Bool match: one `if` on the subject's `i32`. -/
theorem agreementBoolArms :
    ∀ (arms : Arms) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal) (out : Out) (bt : Option Ty) (x : Bool),
      tyBoolArms M X.n Γ tail arms = some T →
      EnvTy M env Γ → LRel S M X env wl →
      wRunF host ar callee (eraseL (lowerBoolArms M X Γ tail bt arms)) wl (b32 x :: st) =
        some out →
      ∃ sv, evalArms F env (.b x) arms = some sv ∧ HasTy M sv T ∧
        Res S M X tail env st sv out
  | .nil, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by simp [tyBoolArms] at hty
  | .cons p _ .nil, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by
      cases p <;> simp [tyBoolArms] at hty
  | .cons p _ (.cons _ _ (.cons _ _ _)), _, _, _, _, _, _, _, _, _, hty, _, _, _ => by
      cases p <;> simp [tyBoolArms] at hty
  | .cons p t (.cons p2 e .nil), Γ, env, tail, T, wl, st, out, bt, x, hty, henv, hl, hrun => by
      cases p with
      | litBool v =>
          simp only [tyBoolArms] at hty
          by_cases hp2 : p2 = .litBool (!v) ∨ p2 = .wild
          · simp only [hp2, ↓reduceIte] at hty
            cases hta : tyOf M X.n Γ tail t with
            | none => simp [hta] at hty
            | some a =>
              cases hte : tyOf M X.n Γ tail e with
              | none => simp [hta, hte] at hty
              | some c =>
                simp only [hta, hte] at hty
                by_cases hac : a = c
                · subst hac
                  simp only [↓reduceIte, Option.some.injEq] at hty
                  subst hty
                  simp only [b32] at hrun
                  rcases hp2 with rfl | rfl <;> cases v <;> cases x <;>
                    simp only [lowerBoolArms, Bool.false_eq_true, ↓reduceIte, eraseL, eraseI]
                      at hrun <;>
                    rw [wRunF_ifElse_single] at hrun <;>
                    simp only [Int.reduceEq, ↓reduceIte] at hrun
                  · obtain ⟨sv, hev, hT, hres⟩ := agreement t Γ env tail a wl st out hta henv hl hrun
                    exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                  · obtain ⟨sv, hev, hT, hres⟩ := agreement e Γ env tail a wl st out hte henv hl hrun
                    exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                  · obtain ⟨sv, hev, hT, hres⟩ := agreement e Γ env tail a wl st out hte henv hl hrun
                    exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                  · obtain ⟨sv, hev, hT, hres⟩ := agreement t Γ env tail a wl st out hta henv hl hrun
                    exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                  · obtain ⟨sv, hev, hT, hres⟩ := agreement t Γ env tail a wl st out hta henv hl hrun
                    exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                  · obtain ⟨sv, hev, hT, hres⟩ := agreement e Γ env tail a wl st out hte henv hl hrun
                    exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                  · obtain ⟨sv, hev, hT, hres⟩ := agreement e Γ env tail a wl st out hte henv hl hrun
                    exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                  · obtain ⟨sv, hev, hT, hres⟩ := agreement t Γ env tail a wl st out hta henv hl hrun
                    exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                · simp [hac] at hty
          · simp [hp2] at hty
      | wild => simp [tyBoolArms] at hty
      | litInt _ => simp [tyBoolArms] at hty
      | bind _ => simp [tyBoolArms] at hty
      | ctor _ _ => simp [tyBoolArms] at hty
      | litStr _ => simp [tyBoolArms] at hty
      | tuple _ => simp [tyBoolArms] at hty

/-- The two-arm Option match over the subject held in the scratch. -/
theorem agreementOptArms :
    ∀ (arms : Arms) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal) (out : Out) (bt : Option Ty) (t : Ty) (sv : SVal) (w : WVal),
      wl[X.subj]? = some w → SRepr S M sv w → HasTy M sv (.option t) →
      tyOptArms M X.n Γ tail t arms = some T →
      EnvTy M env Γ → LRel S M X env wl →
      wRunF host ar callee (eraseL (lowerOptArms M X Γ tail bt t arms)) wl st = some out →
      ∃ sv', evalArms F env sv arms = some sv' ∧ HasTy M sv' T ∧
        Res S M X tail env st sv' out
  | .nil, _, _, _, _, _, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by simp [tyOptArms] at hty
  | .cons _ _ .nil, _, _, _, _, _, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by
      simp [tyOptArms] at hty
  | .cons _ _ (.cons _ _ (.cons _ _ _)), _, _, _, _, _, _, _, _, _, _, _, _, _, _, hty, _, _,
      _ => by simp [tyOptArms] at hty
  | .cons p1 b1 (.cons p2 b2 .nil), Γ, env, tail, T, wl, st, out, bt, t, sv, w, hss, hsw, hsT,
      hty, henv, hl, hrun => by
      simp only [tyOptArms] at hty
      cases hpk : optPick p1 p2 with
      | none => simp [hpk] at hty
      | some pr =>
          obtain ⟨swap, sb⟩ := pr
          simp only [hpk] at hty
          cases hbo : bindOne X.n Γ sb t with
          | none => simp [hbo] at hty
          | some Γs =>
              simp only [hbo] at hty
              -- the arm bodies: `bs` runs under the binder, `bn` without
              have key : ∀ (bs bn : Expr) (a : Ty),
                  (∀ env' wl', EnvTy M env' Γs → LRel S M X env' wl' →
                    wRunF host ar callee (eraseL (lowerB M X Γs tail bs)) wl' st = some out →
                    ∃ sv', eval F env' bs = some sv' ∧ HasTy M sv' a ∧
                      Res S M X tail env' st sv' out) →
                  (wRunF host ar callee (eraseL (lowerB M X Γ tail bn)) wl st = some out →
                    ∃ sv', eval F env bn = some sv' ∧ HasTy M sv' a ∧
                      Res S M X tail env st sv' out) →
                  (∀ env', eval F env' (if swap then b2 else b1) = eval F env' bs) →
                  (eval F env (if swap then b1 else b2) = eval F env bn) →
                  wRunF host ar callee (eraseL (tagTestB X.subj (M.optStruct t) ++
                    [.ifElse bt (bindFieldB X.subj (M.optStruct t) 1 sb ++
                      lowerB M X Γs tail bs) (lowerB M X Γ tail bn)])) wl st = some out →
                  ∃ sv', evalArms F env sv (.cons p1 b1 (.cons p2 b2 .nil)) = some sv' ∧
                    HasTy M sv' a ∧ Res S M X tail env st sv' out := by
                intro bs bn a hAs hAn hbs hbn hr
                rw [eraseL_append] at hr
                rcases hasTy_option hsT with rfl | ⟨x, rfl, hx⟩
                · simp only [SRepr] at hsw
                  obtain ⟨dd, rfl⟩ := hsw
                  rw [run_seq_eq (tagTest_run host ar callee _ _ 0 [dd] _ st hss)] at hr
                  simp only [b32, Int.reduceEq, decide_false, decide_true, Bool.false_eq_true, ↓reduceIte, eraseL, eraseI] at hr
                  rw [wRunF_ifElse_single] at hr
                  simp only [Int.reduceEq, ↓reduceIte] at hr
                  obtain ⟨sv', hev, hT, hres⟩ := hAn hr
                  exact ⟨sv', by rw [evalOpt_none F env hpk, hbn]; exact hev, hT, hres⟩
                · simp only [SRepr] at hsw
                  obtain ⟨xw, rfl, hxw⟩ := hsw
                  obtain ⟨env', wl', hbv, henv', hl', hbrun, hres'⟩ :=
                    bindOne_run host ar callee X.subj (M.optStruct t) 1 sb [.i32v 1, xw] x t xw
                      env Γ Γs wl st hl.1.2.1 hss (by simp) hx hxw hbo henv hl
                  rw [run_seq_eq (tagTest_run host ar callee _ _ 1 [xw] _ st hss)] at hr
                  simp only [b32, Int.reduceEq, decide_false, decide_true, Bool.false_eq_true, ↓reduceIte, eraseL, eraseI] at hr
                  rw [wRunF_ifElse_single] at hr
                  simp only [Int.reduceEq, ↓reduceIte, eraseL_append] at hr
                  rw [run_seq_eq hbrun] at hr
                  obtain ⟨sv', hev, hT, hres⟩ := hAs env' wl' henv' hl' hr
                  refine ⟨sv', ?_, hT, hres' _ _ _ _ hres⟩
                  simp only [evalOpt_some F env hpk, hbv, hbs]
                  exact hev
              cases swap with
              | false =>
                  simp only at hty
                  cases hta : tyOf M X.n Γs tail b1 with
                  | none => simp [hta] at hty
                  | some a =>
                    cases hte : tyOf M X.n Γ tail b2 with
                    | none => simp [hta, hte] at hty
                    | some c =>
                      simp only [hta, hte] at hty
                      by_cases hac : a = c
                      · subst hac
                        simp only [↓reduceIte, Option.some.injEq] at hty
                        subst hty
                        simp only [lowerOptArms, hpk, hbo, Option.getD_some] at hrun
                        exact key b1 b2 a
                          (fun env' wl' he hl' hr => agreement b1 Γs env' tail a wl' st out hta he hl' hr)
                          (fun hr => agreement b2 Γ env tail a wl st out hte henv hl hr)
                          (fun _ => by simp) (by simp) hrun
                      · simp [hac] at hty
              | true =>
                  simp only at hty
                  cases hta : tyOf M X.n Γs tail b2 with
                  | none => simp [hta] at hty
                  | some a =>
                    cases hte : tyOf M X.n Γ tail b1 with
                    | none => simp [hta, hte] at hty
                    | some c =>
                      simp only [hta, hte] at hty
                      by_cases hac : a = c
                      · subst hac
                        simp only [↓reduceIte, Option.some.injEq] at hty
                        subst hty
                        simp only [lowerOptArms, hpk, hbo, Option.getD_some] at hrun
                        exact key b2 b1 a
                          (fun env' wl' he hl' hr => agreement b2 Γs env' tail a wl' st out hta he hl' hr)
                          (fun hr => agreement b1 Γ env tail a wl st out hte henv hl hr)
                          (fun _ => by simp) (by simp) hrun
                      · simp [hac] at hty

/-- The two-arm Result match: `Ok` in the `then` (payload field 1), `Err` in
    the `else` (payload field 2). -/
theorem agreementResArms :
    ∀ (arms : Arms) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal) (out : Out) (bt : Option Ty) (t e : Ty) (sv : SVal) (w : WVal),
      wl[X.subj]? = some w → SRepr S M sv w → HasTy M sv (.result t e) →
      tyResArms M X.n Γ tail t e arms = some T →
      EnvTy M env Γ → LRel S M X env wl →
      wRunF host ar callee (eraseL (lowerResArms M X Γ tail bt t e arms)) wl st = some out →
      ∃ sv', evalArms F env sv arms = some sv' ∧ HasTy M sv' T ∧
        Res S M X tail env st sv' out
  | .nil, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by
      simp [tyResArms] at hty
  | .cons _ _ .nil, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by
      simp [tyResArms] at hty
  | .cons _ _ (.cons _ _ (.cons _ _ _)), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, hty, _,
      _, _ => by simp [tyResArms] at hty
  | .cons p1 b1 (.cons p2 b2 .nil), Γ, env, tail, T, wl, st, out, bt, t, e, sv, w, hss, hsw,
      hsT, hty, henv, hl, hrun => by
      simp only [tyResArms] at hty
      cases hpk : resPick p1 p2 with
      | none => simp [hpk] at hty
      | some pr =>
          obtain ⟨swap, ob, eb⟩ := pr
          simp only [hpk] at hty
          cases hbo : bindOne X.n Γ ob t with
          | none => simp [hbo] at hty
          | some Γo =>
            cases hbe : bindOne X.n Γ eb e with
            | none => simp [hbo, hbe] at hty
            | some Γe =>
              simp only [hbo, hbe] at hty
              have key : ∀ (bo be : Expr) (a : Ty),
                  (∀ env' wl', EnvTy M env' Γo → LRel S M X env' wl' →
                    wRunF host ar callee (eraseL (lowerB M X Γo tail bo)) wl' st = some out →
                    ∃ sv', eval F env' bo = some sv' ∧ HasTy M sv' a ∧
                      Res S M X tail env' st sv' out) →
                  (∀ env' wl', EnvTy M env' Γe → LRel S M X env' wl' →
                    wRunF host ar callee (eraseL (lowerB M X Γe tail be)) wl' st = some out →
                    ∃ sv', eval F env' be = some sv' ∧ HasTy M sv' a ∧
                      Res S M X tail env' st sv' out) →
                  (∀ env', eval F env' (if swap then b2 else b1) = eval F env' bo) →
                  (∀ env', eval F env' (if swap then b1 else b2) = eval F env' be) →
                  wRunF host ar callee (eraseL (tagTestB X.subj (M.resStruct t e) ++
                    [.ifElse bt (bindFieldB X.subj (M.resStruct t e) 1 ob ++
                      lowerB M X Γo tail bo) (bindFieldB X.subj (M.resStruct t e) 2 eb ++
                      lowerB M X Γe tail be)])) wl st = some out →
                  ∃ sv', evalArms F env sv (.cons p1 b1 (.cons p2 b2 .nil)) = some sv' ∧
                    HasTy M sv' a ∧ Res S M X tail env st sv' out := by
                intro bo be a hAo hAe hbo' hbe' hr
                rw [eraseL_append] at hr
                rcases hasTy_result hsT with ⟨x, rfl, hx⟩ | ⟨x, rfl, hx⟩
                · simp only [SRepr] at hsw
                  obtain ⟨xw, dd, rfl, hxw⟩ := hsw
                  obtain ⟨env', wl', hbv, henv', hl', hbrun, hres'⟩ :=
                    bindOne_run host ar callee X.subj (M.resStruct t e) 1 ob [.i32v 1, xw, dd] x
                      t xw env Γ Γo wl st hl.1.2.1 hss (by simp) hx hxw hbo henv hl
                  rw [run_seq_eq (tagTest_run host ar callee _ _ 1 [xw, dd] _ st hss)] at hr
                  simp only [b32, Int.reduceEq, decide_false, decide_true, Bool.false_eq_true, ↓reduceIte, eraseL, eraseI] at hr
                  rw [wRunF_ifElse_single] at hr
                  simp only [Int.reduceEq, ↓reduceIte, eraseL_append] at hr
                  rw [run_seq_eq hbrun] at hr
                  obtain ⟨sv', hev, hT, hres⟩ := hAo env' wl' henv' hl' hr
                  refine ⟨sv', ?_, hT, hres' _ _ _ _ hres⟩
                  simp only [evalRes_ok F env hpk, hbv, hbo']
                  exact hev
                · simp only [SRepr] at hsw
                  obtain ⟨dd, xw, rfl, hxw⟩ := hsw
                  obtain ⟨env', wl', hbv, henv', hl', hbrun, hres'⟩ :=
                    bindOne_run host ar callee X.subj (M.resStruct t e) 2 eb [.i32v 0, dd, xw] x
                      e xw env Γ Γe wl st hl.1.2.1 hss (by simp) hx hxw hbe henv hl
                  rw [run_seq_eq (tagTest_run host ar callee _ _ 0 [dd, xw] _ st hss)] at hr
                  simp only [b32, Int.reduceEq, decide_false, decide_true, Bool.false_eq_true, ↓reduceIte, eraseL, eraseI] at hr
                  rw [wRunF_ifElse_single] at hr
                  simp only [Int.reduceEq, ↓reduceIte, eraseL_append] at hr
                  rw [run_seq_eq hbrun] at hr
                  obtain ⟨sv', hev, hT, hres⟩ := hAe env' wl' henv' hl' hr
                  refine ⟨sv', ?_, hT, hres' _ _ _ _ hres⟩
                  simp only [evalRes_err F env hpk, hbv, hbe']
                  exact hev
              cases swap with
              | false =>
                  simp only at hty
                  cases hta : tyOf M X.n Γo tail b1 with
                  | none => simp [hta] at hty
                  | some a =>
                    cases hte : tyOf M X.n Γe tail b2 with
                    | none => simp [hta, hte] at hty
                    | some c =>
                      simp only [hta, hte] at hty
                      by_cases hac : a = c
                      · subst hac
                        simp only [↓reduceIte, Option.some.injEq] at hty
                        subst hty
                        simp only [lowerResArms, hpk, hbo, hbe, Option.getD_some] at hrun
                        exact key b1 b2 a
                          (fun env' wl' he hl' hr => agreement b1 Γo env' tail a wl' st out hta he hl' hr)
                          (fun env' wl' he hl' hr => agreement b2 Γe env' tail a wl' st out hte he hl' hr)
                          (fun _ => by simp) (fun _ => by simp) hrun
                      · simp [hac] at hty
              | true =>
                  simp only at hty
                  cases hta : tyOf M X.n Γo tail b2 with
                  | none => simp [hta] at hty
                  | some a =>
                    cases hte : tyOf M X.n Γe tail b1 with
                    | none => simp [hta, hte] at hty
                    | some c =>
                      simp only [hta, hte] at hty
                      by_cases hac : a = c
                      · subst hac
                        simp only [↓reduceIte, Option.some.injEq] at hty
                        subst hty
                        simp only [lowerResArms, hpk, hbo, hbe, Option.getD_some] at hrun
                        exact key b2 b1 a
                          (fun env' wl' he hl' hr => agreement b2 Γo env' tail a wl' st out hta he hl' hr)
                          (fun env' wl' he hl' hr => agreement b1 Γe env' tail a wl' st out hte he hl' hr)
                          (fun _ => by simp) (fun _ => by simp) hrun
                      · simp [hac] at hty

/-- The user-variant `ref.test` cascade over the subject held in the
    scratch. `coversB` says some remaining arm reaches the subject's
    constructor, so the untested last arm is exactly that constructor. -/
theorem agreementVarArms :
    ∀ (arms : Arms) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal) (out : Out) (bt : Option Ty) (tid cv : Nat) (fs : List SVal)
      (ws : List WVal) (fts : List Ty),
      wl[X.subj]? = some (.structv (M.ctorStruct tid cv) ws) →
      SReprL S M fs ws → ctorFields M tid cv = some fts → HasTyL M fs fts →
      coversB cv arms = true →
      (∀ c fc, ctorFields M tid c = some fc → M.ctorStruct tid c = M.ctorStruct tid cv →
        c = cv) →
      tyVarArms M X.n Γ tail tid arms = some T →
      EnvTy M env Γ → LRel S M X env wl →
      wRunF host ar callee (eraseL (lowerVarArms M X Γ tail bt tid arms)) wl st = some out →
      ∃ sv, evalArms F env (.variant tid cv fs) arms = some sv ∧ HasTy M sv T ∧
        Res S M X tail env st sv out
  | .nil, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by
      simp [tyVarArms] at hty
  | .cons p b .nil, Γ, env, tail, T, wl, st, out, bt, tid, cv, fs, ws, fts, hss, hws, hcf, hfs,
      hcov, _, hty, henv, hl, hrun => by
      simp only [tyVarArms] at hty
      cases hva : varArmΓ M X.n Γ tid p with
      | none => simp [hva] at hty
      | some Γ' =>
          simp only [hva] at hty
          have hva0 := hva
          cases p with
          | wild =>
              simp only [varArmΓ, Option.some.injEq] at hva
              subst hva
              simp only [lowerVarArms] at hrun
              obtain ⟨sv, hev, hT, hres⟩ := agreement b Γ env tail T wl st out hty henv hl hrun
              exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
          | ctor cc bs =>
              cases cc with
              | user tid' c =>
                  simp only [varArmΓ] at hva
                  split at hva
                  · rename_i htid
                    subst tid'
                    cases hcfc : ctorFields M tid c with
                    | none => simp [hcfc] at hva
                    | some fts' =>
                        simp only [hcfc] at hva
                        have hc : c = cv := by simpa [coversB] using hcov
                        subst hc
                        rw [hcf] at hcfc
                        cases hcfc
                        simp only [lowerVarArms, hva0, Option.getD_some, eraseL_append] at hrun
                        obtain ⟨env', wl', hbv, henv', hl', _, hxrun, hres'⟩ :=
                          extract_run host ar callee X.subj (M.ctorStruct tid c) ws bs 0 fs fts ws
                            env Γ Γ' wl st hl.1.2.1 hss (by intro j y h; simpa using h) hfs hws
                            hva henv hl
                        rw [run_seq_eq hxrun] at hrun
                        obtain ⟨sv, hev, hT, hres⟩ :=
                          agreement b Γ' env' tail T wl' st out hty henv' hl' hrun
                        exact ⟨sv, by simp [evalArms, patMatch, hbv, hev], hT,
                          hres' _ _ _ _ hres⟩
                  · cases hva
              | some => simp [varArmΓ] at hva
              | none => simp [varArmΓ] at hva
              | ok => simp [varArmΓ] at hva
              | err => simp [varArmΓ] at hva
          | litInt _ => simp [varArmΓ] at hva
          | litBool _ => simp [varArmΓ] at hva
          | bind _ => simp [varArmΓ] at hva
          | litStr _ => simp [varArmΓ] at hva
          | tuple _ => simp [varArmΓ] at hva
  | .cons p b (.cons p' b' r), Γ, env, tail, T, wl, st, out, bt, tid, cv, fs, ws, fts, hss,
      hws, hcf, hfs, hcov, hinj, hty, henv, hl, hrun => by
      simp only [tyVarArms] at hty
      cases p with
      | wild => simp [Pat.isWild] at hty
      | ctor cc bs =>
          cases cc with
          | user tid' c =>
              simp only [Pat.isWild, Bool.false_eq_true, ↓reduceIte] at hty
              cases hva : varArmΓ M X.n Γ tid (.ctor (.user tid' c) bs) with
              | none => simp [hva] at hty
              | some Γ' =>
                  simp only [hva] at hty
                  cases hta : tyOf M X.n Γ' tail b with
                  | none => simp [hta] at hty
                  | some a =>
                      cases hr : tyVarArms M X.n Γ tail tid (.cons p' b' r) with
                      | none => simp [hta, hr] at hty
                      | some a' =>
                          simp only [hta, hr] at hty
                          split at hty
                          · rename_i haa
                            subst a'
                            simp only [Option.some.injEq] at hty
                            subst T
                            have hva0 := hva
                            simp only [varArmΓ] at hva
                            split at hva
                            · rename_i htid
                              subst tid'
                              cases hcfc : ctorFields M tid c with
                              | none => simp [hcfc] at hva
                              | some fts' =>
                                  simp only [hcfc] at hva
                                  simp only [lowerVarArms, hva0, Option.getD_some, eraseL,
                                    eraseI, List.cons_append, List.nil_append] at hrun
                                  rw [run_test host ar callee _ _ _ _ _ _ _ hss] at hrun
                                  simp only [b32] at hrun
                                  rw [wRunF_ifElse_single] at hrun
                                  by_cases hc : c = cv
                                  · subst hc
                                    rw [hcf] at hcfc
                                    cases hcfc
                                    simp only [decide_true, ↓reduceIte, Int.reduceEq,
                                      eraseL_append] at hrun
                                    obtain ⟨env', wl', hbv, henv', hl', _, hxrun, hres'⟩ :=
                                      extract_run host ar callee X.subj (M.ctorStruct tid c) ws bs 0
                                        fs fts ws env Γ Γ' wl st hl.1.2.1 hss
                                        (by intro j y h; simpa using h) hfs hws hva henv hl
                                    rw [run_seq_eq hxrun] at hrun
                                    obtain ⟨sv, hev, hT, hres⟩ :=
                                      agreement b Γ' env' tail a wl' st out hta henv' hl' hrun
                                    exact ⟨sv, by simp [evalArms, patMatch, hbv, hev], hT,
                                      hres' _ _ _ _ hres⟩
                                  · have hne : M.ctorStruct tid cv ≠ M.ctorStruct tid c := by
                                      intro h
                                      exact hc (hinj c fts' hcfc h.symm)
                                    simp only [hne, decide_false, Bool.false_eq_true,
                                      ↓reduceIte] at hrun
                                    have hcov' : coversB cv (.cons p' b' r) = true := by
                                      simpa [coversB, hc] using hcov
                                    obtain ⟨sv, hev, hT, hres⟩ :=
                                      agreementVarArms (.cons p' b' r) Γ env tail a wl st out bt tid
                                        cv fs ws fts hss hws hcf hfs hcov' hinj hr henv hl hrun
                                    refine ⟨sv, ?_, hT, hres⟩
                                    rw [← hev]
                                    conv => lhs; rw [evalArms]
                                    simp [patMatch, hc]
                            · cases hva
                          · cases hty
          | some => simp [varArmΓ] at hty
          | none => simp [varArmΓ] at hty
          | ok => simp [varArmΓ] at hty
          | err => simp [varArmΓ] at hty
      | litInt _ => simp [varArmΓ] at hty
      | litBool _ => simp [varArmΓ] at hty
      | bind _ => simp [varArmΓ] at hty
      | litStr _ => simp [varArmΓ] at hty
      | tuple _ => simp [varArmΓ] at hty

/-- The String literal cascade over the subject held in the scratch: each
    literal arm compares through `__wasmgc_string_eq`; `_` ends it. -/
theorem agreementStrArms :
    ∀ (arms : Arms) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal) (out : Out) (bt : Option Ty) (x : List Nat),
      ((∃ k b r, arms = .cons (.litStr k) b r) → wl[X.subj]? = some (strW M x)) →
      tyStrArms M X.n Γ tail arms = some T →
      EnvTy M env Γ → LRel S M X env wl →
      wRunF host ar callee (eraseL (lowerStrArms M X Γ tail bt arms)) wl st = some out →
      ∃ sv, evalArms F env (.s x) arms = some sv ∧ HasTy M sv T ∧
        Res S M X tail env st sv out
  | .nil, _, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by simp [tyStrArms] at hty
  | .cons p b rest, Γ, env, tail, T, wl, st, out, bt, x, hss, hty, henv, hl, hrun => by
      cases p
      case litStr k =>
          have hss' := hss ⟨k, b, rest, rfl⟩
          simp only [tyStrArms] at hty
          cases hb : tyOf M X.n Γ tail b with
          | none => simp [hb] at hty
          | some T1 =>
            cases hr : tyStrArms M X.n Γ tail rest with
            | none => simp [hb, hr] at hty
            | some T2 =>
              simp only [hb, hr] at hty
              by_cases hTT : T1 = T2
              · subst hTT
                simp only [↓reduceIte, Option.some.injEq] at hty
                subst hty
                obtain ⟨g, hg, hgc⟩ := R.streq
                have hrun' := hrun
                simp only [lowerStrArms, eraseL_append] at hrun'
                cases hq : g [strW M x, strW M k] with
                | none =>
                    simp [strLitB, eraseL, eraseI, wRunF, hss', strW, hg, popArgs_two,
                      Function.comp_def] at hrun'
                    simp only [strW] at hq
                    simp [hq] at hrun'
                | some q =>
                    have hq' := hgc _ _ _ hq
                    rw [stringEqW_strW] at hq'
                    subst hq'
                    have hpre : wRunF host ar callee
                        (eraseL ([BI.op (.localGet X.subj), .castNull M.str] ++ strLitB M k ++
                          [BI.op (.call M.streq)])) wl st =
                        some (.ok wl (b32 (x == k) :: st)) := by
                      simp only [strW] at hq
                      simp [strLitB, eraseL, eraseI, wRunF, hss', strW, hg, popArgs_two,
                        Function.comp_def, hq]
                    have hsplit : eraseL (lowerStrArms M X Γ tail bt (.cons (.litStr k) b rest)) =
                        eraseL ([BI.op (.localGet X.subj), .castNull M.str] ++ strLitB M k ++
                          [BI.op (.call M.streq)]) ++
                        [.ifElse (eraseL (lowerB M X Γ tail b))
                          (eraseL (lowerStrArms M X Γ tail bt rest))] := by
                      simp only [lowerStrArms, eraseL_append, eraseL, eraseI, List.append_assoc,
                        List.cons_append, List.nil_append]
                    rw [hsplit] at hrun
                    have hseq := run_seq hpre hrun
                    simp only [b32] at hseq
                    rw [wRunF_ifElse_single] at hseq
                    by_cases hxk : x = k
                    · subst hxk
                      simp [b32] at hseq
                      obtain ⟨sv, hev, hT, hres⟩ := agreement b Γ env tail T1 wl st out hb henv hl hseq
                      exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
                    · simp [b32, hxk] at hseq
                      obtain ⟨sv, hev, hT, hres⟩ :=
                        agreementStrArms rest Γ env tail T1 wl st out bt x (fun _ => hss') hr henv
                          hl hseq
                      exact ⟨sv, by simp [evalArms, patMatch, hxk, hev], hT, hres⟩
              · simp [hTT] at hty
      case wild =>
          cases rest with
          | cons _ _ _ => simp [tyStrArms] at hty
          | nil =>
              simp only [tyStrArms] at hty
              simp only [lowerStrArms] at hrun
              obtain ⟨sv, hev, hT, hres⟩ := agreement b Γ env tail T wl st out hty henv hl hrun
              exact ⟨sv, by simp [evalArms, patMatch, bindVals, hev], hT, hres⟩
      all_goals simp [tyStrArms] at hty

/-- The flat tuple destructure over the subject held in the scratch. -/
theorem agreementTupArms :
    ∀ (arms : Arms) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal) (out : Out) (tid : Nat) (fs : List SVal) (ws : List WVal)
      (fts : List Ty),
      wl[X.subj]? = some (.structv (M.structOf tid) ws) →
      SReprL S M fs ws → M.recFields tid = some fts → HasTyL M fs fts →
      tyTupArms M X.n Γ tail tid arms = some T →
      EnvTy M env Γ → LRel S M X env wl →
      wRunF host ar callee (eraseL (lowerTupArms M X Γ tail tid arms)) wl st = some out →
      ∃ sv, evalArms F env (.record tid fs) arms = some sv ∧ HasTy M sv T ∧
        Res S M X tail env st sv out
  | .nil, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, hty, _, _, _ => by
      simp [tyTupArms] at hty
  | .cons p b rest, Γ, env, tail, T, wl, st, out, tid, fs, ws, fts, hss, hws, hR, hfs, hty,
      henv, hl, hrun => by
      cases p
      case tuple bs =>
          cases rest with
          | cons _ _ _ => simp [tyTupArms] at hty
          | nil =>
              simp only [tyTupArms, hR] at hty
              split at hty
              · cases hbt : bindTys X.n Γ bs fts with
                | none => simp [hbt] at hty
                | some Γ' =>
                    simp only [hbt] at hty
                    simp only [lowerTupArms, hR, Option.bind_some, hbt, Option.getD_some,
                      eraseL_append] at hrun
                    obtain ⟨env', wl', hbv, henv', hl', _, hxrun, hres'⟩ :=
                      extract_run host ar callee X.subj (M.structOf tid) ws bs 0 fs fts ws env Γ
                        Γ' wl st hl.1.2.1 hss (by intro j y h; simpa using h) hfs hws hbt henv hl
                    rw [run_seq_eq hxrun] at hrun
                    obtain ⟨sv, hev, hT, hres⟩ :=
                      agreement b Γ' env' tail T wl' st out hty henv' hl' hrun
                    exact ⟨sv, by simp [evalArms, patMatch, hbv, hev], hT, hres' _ _ _ _ hres⟩
              · cases hty
      all_goals simp [tyTupArms] at hty
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

theorem envTy_args {M : MCtx} {svs : List SVal} {ts : List Ty}
    (h : HasTyL M svs ts) : EnvTy M (argsEnv svs) (paramsΓ ts) := by
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

theorem FnPlan.lctx_spec (p : FnPlan) :
    p.lctx.n = p.nslots ∧ p.nslots ≤ p.lctx.cmp ∧ p.lctx.subj = p.nslots ∧
      p.lctx.subj ≤ p.lctx.cmp := by
  unfold FnPlan.lctx
  split <;> simp

/-- The group theorem: every member of one group (an SCC, or a single
    function) is certified at its plan's model, given the planned code at its
    index, the typing checks, and `FnCertified` for every callee outside the
    group. Self and mutual calls need nothing more: at fuel `k + 1` the
    members' contracts at fuel `k` are the induction hypothesis. -/
theorem fn_certified_group {C : Nat} (S : CarrierSpec C)
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
        code f = some (fnCode M p)) :
    ∀ f p, G f = some p →
      FnCertified S M code host f p.sig
        (fun fuel => groupModel outer G fuel f) := by
  have core : ∀ fuel f p, G f = some p → ∀ svs ws r,
      HasTyL M svs p.sig.params → SReprL S M svs ws →
      wFuncN code host fuel f ws = some r →
      ∃ sv, groupModel outer G fuel f svs = some sv ∧ SRepr S M sv r ∧
        HasTy M sv p.sig.ret := by
    intro fuel
    induction fuel with
    | zero => intro f p _ svs ws r _ _ h; simp [wFuncN] at h
    | succ k ih =>
        intro f p hG svs ws r hTs hrep hrun
        obtain ⟨_, htyped, _, hcode⟩ := hMem f p hG
        simp only [planTyped, Bool.and_eq_true, decide_eq_true_eq] at htyped
        obtain ⟨⟨hpn, hnl⟩, hty⟩ := htyped
        obtain ⟨hXn, hXc, hXs, hXsc⟩ := FnPlan.lctx_spec p
        rw [← hXn] at hty
        have hCallees : ∀ g sig, M.sigs g = some sig →
            Contract S M host (fun g => (code g).map (·.arity))
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
        have hLR : LRel S M p.lctx (argsEnv svs) (initLocals (fnCode M p) ws) := by
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
              hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R (groupModel outer G k) hCallees
              p.lctx p.body (paramsΓ p.sig.params) (argsEnv svs) true p.sig.ret
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
