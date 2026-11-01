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
     call (a plan without recursion stays at L1, as on main);
   * either arm may compute its `+`, `-`, `*` over parameters and literals as
     an `Int` tree on the machine word (`Expr.intWord`, `WTree.totW`), and
     `n - 1` may be such a tree; the tree's words lie below the function's
     locals count, so its `i64` path has the locals it writes.
   * either arm may bind a value of the fragment by a `let` (the MIR binds a
     tree's call leaf so) and use it in the rest of the arm.

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

/-- The recursive argument `n - 1` over parameter 0: plain arithmetic, or
    the same difference computed on the machine word. -/
def isDescent : Expr → Bool
  | .binOp .sub (.local i) (.literal (.int k)) => i == 0 && k == 1
  | .intWord (.op .sub (.leaf (.local i)) (.lit k)) _ => i == 0 && k == 1
  | _ => false

/-- The `Int` trees on the machine word the total fragment admits: `+`, `-`
    and, under the `.mul` role, `*` over `Int` locals and literals. Their
    `i64` path calls nothing, and their exact path calls only box, add, sub
    and mul. -/
def WTree.totW (mulOk : Bool) : WTree → Bool
  | .leaf (.local _) => true
  | .leaf (.field _ _ _ _) => false
  | .leaf (.word _ _ _) => false
  | .op o a b => (o == .add || o == .sub || (o == .mul && mulOk)) && a.totW mulOk && b.totW mulOk
  | .lit _ => true
  | .pick _ _ _ => false
  | .mulLit _ _ a => mulOk && a.totW mulOk
  | .divLit _ _ => false
  | .modLit _ _ => false

/-- Some `*` occurs in the tree. -/
def WTree.usesMulW : WTree → Bool
  | .op o a b => o == .mul || a.usesMulW || b.usesMulW
  | .mulLit _ _ _ => true
  | _ => false

/-- The first argument of a member call is the descent. -/
def descentHead : List Expr → Bool
  | a :: _ => isDescent a
  | [] => false

mutual
  /-- The straight-line total fragment. `mem` is the group's member set,
      `mulOk` says the group's role admits `*`, `calls` says member calls
      are admitted here (only in the step arm), and `lim` is the number of
      locals the function has (the words of a tree lie below it). -/
  def totE (mem : Nat → Bool) (mulOk calls : Bool) (lim : Nat) : Expr → Bool
    | .literal (.int _) => true
    | .literal (.bool _) => true
    | .local _ => true
    | .binOp op l r =>
        (op == .add || op == .sub || (op == .mul && mulOk)) &&
        totE mem mulOk calls lim l && totE mem mulOk calls lim r
    | .call (.fn g) args =>
        calls && mem g && descentHead args && totArgs mem mulOk calls lim args
    | .tailCall g args =>
        calls && mem g && descentHead args && totArgs mem mulOk calls lim args
    | .intWord t w => t.totW mulOk && decide (w + 2 < lim)
    | .let_ _ v body => totE mem mulOk calls lim v && totE mem mulOk calls lim body
    | _ => false
  def totArgs (mem : Nat → Bool) (mulOk calls : Bool) (lim : Nat) : List Expr → Bool
    | [] => true
    | e :: es => totE mem mulOk calls lim e && totArgs mem mulOk calls lim es
end

mutual
/-- The total fragment has no `try_`: nothing in it returns early. -/
theorem totE_bare {mem : Nat → Bool} {mulOk calls : Bool} {lim : Nat} :
    ∀ e : Expr, totE mem mulOk calls lim e = true → bareTries e = []
  | .literal _, _ => by simp [bareTries]
  | .local _, _ => by simp [bareTries]
  | .binOp _ l r, h => by
      simp only [totE, Bool.and_eq_true] at h
      simp [bareTries, totE_bare l h.1.2, totE_bare r h.2]
  | .call (.fn _) args, h => by
      simp only [totE, Bool.and_eq_true] at h
      simp [bareTries, totArgs_bare args h.2]
  | .tailCall _ args, h => by
      simp only [totE, Bool.and_eq_true] at h
      simp [bareTries, totArgs_bare args h.2]
  | .let_ _ v body, h => by
      simp only [totE, Bool.and_eq_true] at h
      simp [bareTries, totE_bare v h.1, totE_bare body h.2]
  | .call (.builtin _) _, h => by simp [totE] at h
  | .call (.lazy _) _, h => by simp [totE] at h
  | .call (.intrinsic _) _, h => by simp [totE] at h
  | .neg _, h => by simp [totE] at h
  | .ifThenElse _ _ _, h => by simp [totE] at h
  | .recordCreate _ _, h => by simp [totE] at h
  | .project _ _ _, h => by simp [totE] at h
  | .match_ _ _, h => by simp [totE] at h
  | .construct _ _ _, h => by simp [totE] at h
  | .interp _, h => by simp [totE] at h
  | .list _ _, h => by simp [totE] at h
  | .try_ _ _, h => by simp [totE] at h
  | .scope _, h => by simp [totE] at h
  | .intWord _ _, _ => by simp [bareTries]
theorem totArgs_bare {mem : Nat → Bool} {mulOk calls : Bool} {lim : Nat} :
    ∀ es : List Expr, totArgs mem mulOk calls lim es = true → bareTriesL es = []
  | [], _ => by simp [bareTriesL]
  | e :: es, h => by
      simp only [totArgs, Bool.and_eq_true] at h
      simp [bareTriesL, totE_bare e h.1, totArgs_bare es h.2]
end

mutual
  /-- Some member call occurs. -/
  def hasCall : Expr → Bool
    | .binOp _ l r => hasCall l || hasCall r
    | .let_ _ v body => hasCall v || hasCall body
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
    | .intWord t _ => t.usesMulW
    | .let_ _ v body => usesMul v || usesMul body
    | _ => false
  def usesMulArgs : List Expr → Bool
    | [] => false
    | e :: es => usesMul e || usesMulArgs es
end

/-- The body: `if n <= 0 then base else step` over parameter 0. -/
def totBody (mem : Nat → Bool) (mulOk : Bool) (lim : Nat) : Expr → Bool
  | .ifThenElse (.binOp .lte (.local i) (.literal (.int k))) base step =>
      i == 0 && k == 0 && totE mem mulOk false lim base && totE mem mulOk true lim step &&
        hasCall step
  | _ => false

def isIntTy (t : Ty) : Bool := decide (t = .int)

/-- One member plan: Int parameters (at least one), an Int or Bool result,
    and a total body. -/
def totPlan (mem : Nat → Bool) (mulOk : Bool) (p : FnPlan) : Bool :=
  !p.sig.params.isEmpty && p.sig.params.all isIntTy &&
    (decide (p.sig.ret = .int) || decide (p.sig.ret = .bool)) &&
    totBody mem mulOk (p.sig.params.length + p.locals.length) p.body

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

/-- The recursive argument evaluates to `n - 1`. -/
theorem isDescent_eval {a : Expr} (h : isDescent a = true) (F : Nat → List SVal → Option SVal)
    {env : Nat → Option SVal} {n : Int} (h0 : env 0 = some (.i n)) :
    eval F env a = some (.i (n - 1)) := by
  unfold isDescent at h
  split at h
  · simp only [Bool.and_eq_true, beq_iff_eq] at h
    obtain ⟨rfl, rfl⟩ := h
    simp [eval, h0, intBin]
  · simp only [Bool.and_eq_true, beq_iff_eq] at h
    obtain ⟨rfl, rfl⟩ := h
    simp [eval, WTree.val, WLeaf.val, h0, intBin]
  · cases h

theorem descentHead_eq {args : List Expr} (h : descentHead args = true) :
    ∃ a rest, args = a :: rest ∧ isDescent a = true := by
  cases args with
  | nil => simp [descentHead] at h
  | cons a rest => exact ⟨a, rest, rfl, h⟩

theorem totBody_eq {mem : Nat → Bool} {mulOk : Bool} {lim : Nat} {b : Expr}
    (h : totBody mem mulOk lim b = true) :
    ∃ base step, b = .ifThenElse (.binOp .lte (.local 0) (.literal (.int 0))) base step ∧
      totE mem mulOk false lim base = true ∧ totE mem mulOk true lim step = true := by
  unfold totBody at h
  split at h
  · simp only [Bool.and_eq_true, beq_iff_eq] at h
    obtain ⟨⟨⟨⟨rfl, rfl⟩, hb⟩, hs⟩, _⟩ := h
    exact ⟨_, _, rfl, hb, hs⟩
  · cases h

theorem totPlan_spec {mem : Nat → Bool} {mulOk : Bool} {p : FnPlan}
    (h : totPlan mem mulOk p = true) :
    (∃ ps, p.sig.params = .int :: ps) ∧ (∀ t ∈ p.sig.params, t = .int) ∧
      (p.sig.ret = .int ∨ p.sig.ret = .bool) ∧
        totBody mem mulOk (p.sig.params.length + p.locals.length) p.body = true := by
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

theorem totE_binOp {mem : Nat → Bool} {mulOk calls : Bool} {lim : Nat} {op : BinOp} {l r : Expr}
    (h : totE mem mulOk calls lim (.binOp op l r) = true) :
    (op = .add ∨ op = .sub ∨ (op = .mul ∧ mulOk = true)) ∧
      totE mem mulOk calls lim l = true ∧ totE mem mulOk calls lim r = true := by
  simp only [totE, Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq] at h
  obtain ⟨⟨hop, hl⟩, hr⟩ := h
  refine ⟨?_, hl, hr⟩
  rcases hop with (h1 | h1) | h1
  · exact Or.inl h1
  · exact Or.inr (Or.inl h1)
  · exact Or.inr (Or.inr ⟨h1.1, h1.2⟩)

/-- A total-fragment node never has type `String` (so `+` is Int addition):
    parameters are Int and member results are Int or Bool. -/
theorem totE_notStr {M : MCtx} {n : Nat} {mem : Nat → Bool} {mulOk calls : Bool} {lim : Nat}
    (hSig : ∀ g sig, mem g = true → M.sigs g = some sig → sig.ret = .int ∨ sig.ret = .bool) :
    ∀ (e : Expr) (Γ : Nat → Option Ty) (tail : Bool),
      (∀ i T', Γ i = some T' → T' ≠ .string) →
      totE mem mulOk calls lim e = true → tyOf M n Γ tail e ≠ some .string
  | .literal (.int k), Γ, tail, hΓ, _, h => by
      have := (tyOf_litInt_inv h).2; cases this
  | .literal (.bool v), Γ, tail, hΓ, _, h => by
      have := tyOf_litBool_inv h; cases this
  | .local i, Γ, tail, hΓ, _, h => by
      simp only [tyOf] at h
      exact hΓ i _ h rfl
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
  | .let_ b v body, Γ, tail, hΓ, ht, h => by
      simp only [totE, Bool.and_eq_true] at ht
      obtain ⟨_, _, Tv, htv, htb⟩ := tyOf_let_inv h
      have hv := totE_notStr (n := n) hSig v Γ false hΓ ht.1
      refine totE_notStr hSig body (upd Γ b Tv) tail ?_ ht.2 htb
      intro i T' hi
      by_cases hib : i = b
      · subst hib
        rw [upd_same] at hi
        cases hi
        exact fun hs => hv (hs ▸ htv)
      · rw [upd_ne _ _ hib] at hi
        exact hΓ i T' hi
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
  | .try_ _ _, _, _, _, ht, _ => by simp [totE] at ht
  | .scope _, _, _, _, ht, _ => by simp [totE] at ht
  | .intWord _ _, _, _, _, _, h => by
      simp only [tyOf] at h
      split at h <;> cases h

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

/-! ## Progress: an `Int` tree on the machine word runs to completion

A tree of the total fragment (`WTree.totW`) has `Int` locals and literals for
leaves and `+`, `-` and, under the `.mul` role, `*` for operations. Its block
runs: the `i64` path reads each leaf's `$small` or branches to the exact path,
each machine operation finishes or branches there, and the exact path calls
only box, add, sub and mul, which the role makes total. The shape of each
intermediate frame comes from the partial lemmas (`fastW_run`, `exactW_run`)
run with nothing after them; the words the tree writes lie below the
function's locals count (`totE`). -/

theorem in64_wrap64 (v : Int) : in64 (wrap64 v) = true := by
  rw [in64_iff]; unfold wrap64; omega

theorem in64_xor64 (a b : Int) (ha : in64 a = true) (hb : in64 b = true) :
    in64 (xor64 a b) = true := by
  cases a <;> cases b <;> simp only [in64, xor64, Nat.blt_eq] at ha hb ⊢ <;>
    exact Nat.xor_lt_two_pow (n := 63) ha hb

theorem treeOp_progress (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) (w : Nat)
    (op : BinOp) (hop : op.isArith = true) (x y : Int) (hx : in64 x = true) (hy : in64 y = true)
    (wl st : List WVal) (hlen : w + 2 < wl.length) :
    ∃ o, wRunF host ar callee (treeOpW w op) wl (.i64v y :: .i64v x :: st) = some o := by
  have h2 : w < wl.length := by omega
  have h3 : w + 1 < wl.length := by omega
  cases op <;> simp [BinOp.isArith] at hop
  · simp [treeOpW, wRunF, h2, h3, hlen, b32, hx, hy, in64_wrap64, in64_xor64]
    split <;> simp
  · simp [treeOpW, wRunF, h2, h3, hlen, b32, hx, hy, in64_wrap64, in64_xor64]
    split <;> simp
  · simp [treeOpW, wRunF, h2, h3, b32, hx, hy, in64_wrap64]
    refine ⟨by decide, ?_⟩
    by_cases hg : gtU64 (wrap64 (x + 1)) 1 = false
    · have hd : (wrap64 (x * y)).tdiv 1 = wrap64 (x * y) := Int.tdiv_one _
      simp [hg, wRunF, hlen, h2, h3, b32, in64_wrap64, hd, hx, List.getElem?_set]
      exact ⟨by decide, by split <;> simp⟩
    · have hne := (gtU64_one x hx).mp (by simpa using hg)
      have hm : ¬ (wrap64 (x * y) = -9223372036854775808 ∧ x = -1) := fun h => hne.1 h.2
      simp [hg, wRunF, hlen, h2, h3, b32, in64_wrap64, hx, hne.2, hm, List.getElem?_set]
      exact ⟨by omega, by decide, by split <;> simp⟩

/-- Every leaf's local is there. -/
def LeavesPresent (wl : List WVal) (t : WTree) : Prop :=
  ∀ l ∈ t.leaves, ∃ wv, wl[l.loc]? = some wv

theorem LeavesPresent.after {C : Nat} {S : CarrierSpec C} {w : Nat} {env : Nat → Option SVal}
    {wl wl' : List WVal} {t : WTree} (hh : LeavesHeld S w env wl t) (h : LeavesPresent wl t)
    (hw : WordsOnly w wl wl') : LeavesPresent wl' t := by
  intro l hl
  obtain ⟨wv, hwv⟩ := h l hl
  exact ⟨wv, by rw [hw.2 _ (hh l hl).1]; exact hwv⟩

/-- The range test and the product by the literal run to completion: the
    test branches out or falls through, and the multiplication of two words
    in the `i64` range always runs. -/
theorem guardMul_progress (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) (w : Nat)
    (k x : Int) (hk : -9223372036854775808 ≤ k ∧ k < 9223372036854775808)
    (hx : -9223372036854775808 ≤ x ∧ x < 9223372036854775808)
    (wl st : List WVal) (hw : w < wl.length) :
    ∃ o, wRunF host ar callee (guardW (mulRange k).1 (mulRange k).2 w ++
      [.localGet w, .i64Const k, .i64Mul]) wl (.i64v x :: st) = some o := by
  have hx' : in64 x = true := (in64_iff x).mpr hx
  have hk' : in64 k = true := (in64_iff k).mpr hk
  have hset : (wl.set w (.i64v x))[w]? = some (.i64v x) := by simp [hw]
  generalize (mulRange k).1 = lo0
  generalize (mulRange k).2 = hi0
  unfold guardW
  simp only
  split
  · simp [wRunF]
  split
  · have hlo : in64 (max lo0 (-9223372036854775808)) = true := by
      rw [in64_iff]; omega
    simp only [List.cons_append, wRunF, hset, hx', hlo, hk', in64_wrap64, Bool.and_self,
      ↓reduceIte, b32]
    split
    · simp
    · simp only [↓reduceIte, List.nil_append, List.cons_append, wRunF]
      rw [hset]
      simp [wRunF, hx', hk']
  · simp only [List.cons_append, List.nil_append, wRunF]
    rw [hset]
    simp [wRunF, hx', hk']

theorem fastW_progress {C : Nat} (S : CarrierSpec C) (host : HostTbl) (ar : Nat → Option Nat)
    (callee : Callee) (w : Nat) (env : Nat → Option SVal) (mulOk : Bool) :
    ∀ (t : WTree) (wl st : List WVal),
      t.totW mulOk = true → t.arith = true → LeavesHeld S w env wl t → LeavesPresent wl t →
      w + 2 < wl.length →
      ∃ o, wRunF host ar callee (fastW C w t) wl st = some o
  | .leaf (.local i), wl, st, _, _, hh, hp, _ => by
      obtain ⟨_, x, _, hc⟩ := hh (.local i) (by simp [WTree.leaves])
      obtain ⟨wv, hwl⟩ := hp (.local i) (by simp [WTree.leaves])
      simp only [WLeaf.loc] at hwl
      have hx := hc wv hwl
      rcases S.car x wv hx.1 with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · simp [fastW, WLeaf.fastW, smallReadW, WLeaf.loc, wRunF, hwl, b32]
      · simp [fastW, WLeaf.fastW, smallReadW, WLeaf.loc, wRunF, hwl, b32]
  | .leaf (.field _ _ _ _), _, _, ht, _, _, _, _ => by simp [WTree.totW] at ht
  | .leaf (.word _ _ _), _, _, ht, _, _, _, _ => by simp [WTree.totW] at ht
  | .pick _ _ _, _, _, ht, _, _, _, _ => by simp [WTree.totW] at ht
  | .divLit _ _, _, _, ht, _, _, _, _ => by simp [WTree.totW] at ht
  | .modLit _ _, _, _, ht, _, _, _, _ => by simp [WTree.totW] at ht
  | .mulLit k lf a, wl, st, ht, har, hh, hp, hlen => by
      simp only [WTree.totW, Bool.and_eq_true] at ht
      simp only [WTree.arith, Bool.and_eq_true] at har
      have hk : -9223372036854775808 ≤ k ∧ k < 9223372036854775808 := by
        simpa [inI64Band, Bool.and_eq_true, decide_eq_true_eq] using har.1
      have hha : LeavesHeld S w env wl a := fun l hl => hh l (by simpa [WTree.leaves] using hl)
      have hpa : LeavesPresent wl a := fun l hl => hp l (by simpa [WTree.leaves] using hl)
      simp only [fastW, List.append_assoc]
      rw [wRunF_append]
      obtain ⟨oa, hoa⟩ := fastW_progress S host ar callee w env mulOk a wl st ht.2 har.2 hha hpa hlen
      have hoa' : wRunF host ar callee (fastW C w a ++ []) wl st = some oa := by simpa using hoa
      obtain ⟨L1, hL1, hr1⟩ := fastW_run S host ar callee w env a wl st [] oa har.2 hha hoa'
      rw [hoa]
      rcases hr1 with ⟨S', rfl⟩ | ⟨va, -, va1, va2, h1⟩
      · exact ⟨_, rfl⟩
      simp only [wRunF] at h1
      cases h1
      simp only [seqOut]
      exact guardMul_progress host ar callee w k va hk ⟨va1, va2⟩ L1 st
        (by rw [hL1.1]; omega)
  | .lit k, wl, st, _, _, _, _, _ => by simp [fastW, wRunF]
  | .op o a b, wl, st, ht, har, hh, hp, hlen => by
      simp only [WTree.totW, Bool.and_eq_true] at ht
      have har' := har
      simp only [WTree.arith, Bool.and_eq_true] at har'
      obtain ⟨⟨ho, ha⟩, hb⟩ := har'
      have hha : LeavesHeld S w env wl a := fun l hl => hh l (by simp [WTree.leaves, hl])
      have hhb : LeavesHeld S w env wl b := fun l hl => hh l (by simp [WTree.leaves, hl])
      have hpa : LeavesPresent wl a := fun l hl => hp l (by simp [WTree.leaves, hl])
      have hpb : LeavesPresent wl b := fun l hl => hp l (by simp [WTree.leaves, hl])
      simp only [fastW, List.append_assoc]
      rw [wRunF_append]
      obtain ⟨oa, hoa⟩ := fastW_progress S host ar callee w env mulOk a wl st ht.1.2 ha hha hpa hlen
      have hoa' : wRunF host ar callee (fastW C w a ++ []) wl st = some oa := by simpa using hoa
      obtain ⟨L1, hL1, hr1⟩ := fastW_run S host ar callee w env a wl st [] oa ha hha hoa'
      rw [hoa]
      rcases hr1 with ⟨S', rfl⟩ | ⟨va, -, va1, va2, h1⟩
      · exact ⟨_, rfl⟩
      simp only [wRunF] at h1
      cases h1
      simp only [seqOut]
      rw [wRunF_append]
      have hlen1 : w + 2 < L1.length := by rw [hL1.1]; exact hlen
      obtain ⟨ob, hob⟩ := fastW_progress S host ar callee w env mulOk b L1 _ ht.2 hb
        (hhb.after hL1) (LeavesPresent.after hhb hpb hL1) hlen1
      have hob' : wRunF host ar callee (fastW C w b ++ []) L1 (.i64v va :: st) = some ob := by
        simpa using hob
      obtain ⟨L2, hL2, hr2⟩ := fastW_run S host ar callee w env b L1 _ [] ob hb (hhb.after hL1) hob'
      rw [hob]
      rcases hr2 with ⟨S', rfl⟩ | ⟨vb, -, vb1, vb2, h2⟩
      · exact ⟨_, rfl⟩
      simp only [wRunF] at h2
      cases h2
      simp only [seqOut]
      have hlen2 : w + 2 < L2.length := by rw [hL2.1]; exact hlen1
      exact treeOp_progress host ar callee w o ho va vb ((in64_iff va).mpr ⟨va1, va2⟩)
        ((in64_iff vb).mpr ⟨vb1, vb2⟩) L2 st hlen2

theorem exactW_progress {C : Nat} (S : CarrierSpec C) (host : HostTbl) (ar : Nat → Option Nat)
    (callee : Callee) (M : MCtx) (box add sub mul cmp eq : List WVal → Option WVal)
    (Ctr : Contracts S box add sub mul cmp eq)
    (hBox : host M.box = some (1, box))
    (hAdd : host M.add = some (2, add)) (hSub : host M.sub = some (2, sub))
    (hMul : host M.mul = some (2, mul))
    (hCmp : host M.cmp = some (2, cmp)) (hEq : host M.eq = some (2, eq))
    (hDiv : DivmodSpec S M host)
    (hBoxT : ∀ k : Int, -(2 ^ 63 : Int) ≤ k → k < 2 ^ 63 → ∃ w, box [.i64v k] = some w)
    (hAddT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
    (hSubT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
    (mulOk : Bool)
    (hMulT : mulOk = true → ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, mul [va, vb] = some w)
    (env : Nat → Option SVal) :
    ∀ (t : WTree) (L st : List WVal),
      t.totW mulOk = true → t.arith = true →
      (∀ l ∈ t.leaves, ∃ x, l.val env = some (.i x) ∧
        ∀ wv, L[l.loc]? = some wv → l.holds S x wv) →
      LeavesPresent L t →
      ∃ o, wRunF host ar callee (exactW M t) L st = some o
  | .leaf (.local i), L, st, _, _, _, hp => by
      obtain ⟨wv, hwl⟩ := hp (.local i) (by simp [WTree.leaves])
      simp only [WLeaf.loc] at hwl
      simp [exactW_leaf, WLeaf.exactW, WLeaf.loc, wRunF, hwl]
  | .leaf (.field _ _ _ _), _, _, ht, _, _, _ => by simp [WTree.totW] at ht
  | .leaf (.word _ _ _), _, _, ht, _, _, _ => by simp [WTree.totW] at ht
  | .pick _ _ _, _, _, ht, _, _, _ => by simp [WTree.totW] at ht
  | .divLit _ _, _, _, ht, _, _, _ => by simp [WTree.totW] at ht
  | .modLit _ _, _, _, ht, _, _, _ => by simp [WTree.totW] at ht
  | .mulLit k lf a, L, st, ht, har, hh, hp => by
      simp only [WTree.totW, Bool.and_eq_true] at ht
      simp only [WTree.arith, Bool.and_eq_true] at har
      have hk : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
        simpa [inI64Band, Bool.and_eq_true, decide_eq_true_eq] using har.1
      have hha : ∀ l ∈ a.leaves, ∃ x, l.val env = some (.i x) ∧
          ∀ wv, L[l.loc]? = some wv → l.holds S x wv :=
        fun l hl => hh l (by simpa [WTree.leaves] using hl)
      have hpa : LeavesPresent L a := fun l hl => hp l (by simpa [WTree.leaves] using hl)
      obtain ⟨wk, hwk⟩ := hBoxT k hk.1 hk.2
      have hck := Ctr.hBox k wk hk.1 hk.2 hwk
      rw [exactW_mulLit]
      cases lf
      · simp only [Bool.false_eq_true, ↓reduceIte, List.append_assoc]
        rw [wRunF_append]
        obtain ⟨oa, hoa⟩ := exactW_progress S host ar callee M box add sub mul cmp eq Ctr hBox hAdd
          hSub hMul hCmp hEq hDiv hBoxT hAddT hSubT mulOk hMulT env a L st ht.2 har.2 hha hpa
        have hoa' : wRunF host ar callee (exactW M a ++ []) L st = some oa := by simpa using hoa
        obtain ⟨va, wa, -, hca, h1⟩ := exactW_run host ar callee S box add sub mul cmp eq Ctr hBox
          hAdd hSub hMul hCmp hEq hDiv env a L st [] oa har.2 hha hoa'
        simp only [wRunF] at h1
        cases h1
        rw [hoa]
        obtain ⟨r, hr⟩ := hMulT ht.1 va k wa wk hca.1 hck.1
        have h2 : ¬ (st.length + 1 + 1 < 2) := by omega
        simp [seqOut, wRunF, hBox, popArgs, hwk, hMul, hr, h2]
      · simp only [↓reduceIte, List.append_assoc, List.cons_append, List.nil_append]
        have h1 : wRunF host ar callee (.i64Const k :: .call M.box :: (exactW M a ++ [.call M.mul])) L st
            = wRunF host ar callee (exactW M a ++ [.call M.mul]) L (wk :: st) := by
          simp [wRunF, hBox, popArgs, hwk]
        rw [h1, wRunF_append]
        obtain ⟨oa, hoa⟩ := exactW_progress S host ar callee M box add sub mul cmp eq Ctr hBox hAdd
          hSub hMul hCmp hEq hDiv hBoxT hAddT hSubT mulOk hMulT env a L (wk :: st) ht.2 har.2 hha hpa
        have hoa' : wRunF host ar callee (exactW M a ++ []) L (wk :: st) = some oa := by
          simpa using hoa
        obtain ⟨va, wa, -, hca, h1⟩ := exactW_run host ar callee S box add sub mul cmp eq Ctr hBox
          hAdd hSub hMul hCmp hEq hDiv env a L (wk :: st) [] oa har.2 hha hoa'
        simp only [wRunF] at h1
        cases h1
        rw [hoa]
        obtain ⟨r, hr⟩ := hMulT ht.1 k va wk wa hck.1 hca.1
        simp [seqOut, wRunF, hMul, popArgs_two, hr]
  | .lit k, L, st, _, har, _, _ => by
      have hb : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
        simpa [WTree.arith, inI64Band, Bool.and_eq_true, decide_eq_true_eq] using har
      obtain ⟨wv, hwv⟩ := hBoxT k hb.1 hb.2
      simp [exactW_lit, wRunF, hBox, popArgs, hwv]
  | .op o a b, L, st, ht, har, hh, hp => by
      simp only [WTree.totW, Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq] at ht
      have har' := har
      simp only [WTree.arith, Bool.and_eq_true] at har'
      obtain ⟨⟨_, ha⟩, hb⟩ := har'
      have hha : ∀ l ∈ a.leaves, ∃ x, l.val env = some (.i x) ∧
          ∀ wv, L[l.loc]? = some wv → l.holds S x wv :=
        fun l hl => hh l (by simp [WTree.leaves, hl])
      have hhb : ∀ l ∈ b.leaves, ∃ x, l.val env = some (.i x) ∧
          ∀ wv, L[l.loc]? = some wv → l.holds S x wv :=
        fun l hl => hh l (by simp [WTree.leaves, hl])
      have hpa : LeavesPresent L a := fun l hl => hp l (by simp [WTree.leaves, hl])
      have hpb : LeavesPresent L b := fun l hl => hp l (by simp [WTree.leaves, hl])
      rw [exactW_op, List.append_assoc, wRunF_append]
      obtain ⟨oa, hoa⟩ := exactW_progress S host ar callee M box add sub mul cmp eq Ctr hBox hAdd
        hSub hMul hCmp hEq hDiv hBoxT hAddT hSubT mulOk hMulT env a L st ht.1.2 ha hha hpa
      have hoa' : wRunF host ar callee (exactW M a ++ []) L st = some oa := by simpa using hoa
      obtain ⟨va, wa, -, hca, h1⟩ := exactW_run host ar callee S box add sub mul cmp eq Ctr hBox
        hAdd hSub hMul hCmp hEq hDiv env a L st [] oa ha hha hoa'
      simp only [wRunF] at h1
      cases h1
      rw [hoa]
      simp only [seqOut]
      rw [wRunF_append]
      obtain ⟨ob, hob⟩ := exactW_progress S host ar callee M box add sub mul cmp eq Ctr hBox hAdd
        hSub hMul hCmp hEq hDiv hBoxT hAddT hSubT mulOk hMulT env b L (wa :: st) ht.2 hb hhb hpb
      have hob' : wRunF host ar callee (exactW M b ++ []) L (wa :: st) = some ob := by
        simpa using hob
      obtain ⟨vb, wb, -, hcb, h2⟩ := exactW_run host ar callee S box add sub mul cmp eq Ctr hBox
        hAdd hSub hMul hCmp hEq hDiv env b L (wa :: st) [] ob hb hhb hob'
      simp only [wRunF] at h2
      cases h2
      rw [hob]
      simp only [seqOut]
      rcases ht.1.1 with (rfl | rfl) | ⟨rfl, hm⟩
      · obtain ⟨r, hr⟩ := hAddT va vb wa wb hca.1 hcb.1
        simp [MCtx.arithIdx, wRunF, hAdd, popArgs_two, hr]
      · obtain ⟨r, hr⟩ := hSubT va vb wa wb hca.1 hcb.1
        simp [MCtx.arithIdx, wRunF, hSub, popArgs_two, hr]
      · obtain ⟨r, hr⟩ := hMulT hm va vb wa wb hca.1 hcb.1
        simp [MCtx.arithIdx, wRunF, hMul, popArgs_two, hr]

theorem wordBlock_progress {C : Nat} (S : CarrierSpec C) (host : HostTbl) (ar : Nat → Option Nat)
    (callee : Callee) (M : MCtx) (hC : M.carrier = C)
    (box add sub mul cmp eq : List WVal → Option WVal)
    (Ctr : Contracts S box add sub mul cmp eq)
    (hBox : host M.box = some (1, box))
    (hAdd : host M.add = some (2, add)) (hSub : host M.sub = some (2, sub))
    (hMul : host M.mul = some (2, mul))
    (hCmp : host M.cmp = some (2, cmp)) (hEq : host M.eq = some (2, eq))
    (hDiv : DivmodSpec S M host)
    (hBoxT : ∀ k : Int, -(2 ^ 63 : Int) ≤ k → k < 2 ^ 63 → ∃ w, box [.i64v k] = some w)
    (hAddT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
    (hSubT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
    (mulOk : Bool)
    (hMulT : mulOk = true → ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, mul [va, vb] = some w)
    (env : Nat → Option SVal) (t : WTree) (w : Nat) (wl st : List WVal)
    (ht : t.totW mulOk = true) (har : t.arith = true) (hh : LeavesHeld S w env wl t)
    (hp : LeavesPresent wl t) (hlen : w + 2 < wl.length) :
    ∃ out, wRunF host ar callee [eraseI (wordBlock M t w)] wl st = some out := by
  have hin : ∃ o, wRunF host ar callee (fastW M.carrier w t ++ [WInstr.call M.box, WInstr.br 1])
      wl st = some o := by
    rw [wRunF_append, hC]
    obtain ⟨o0, ho0⟩ := fastW_progress S host ar callee w env mulOk t wl st ht har hh hp hlen
    have ho0' : wRunF host ar callee (fastW C w t ++ []) wl st = some o0 := by simpa using ho0
    obtain ⟨L, _, hr⟩ := fastW_run S host ar callee w env t wl st [] o0 har hh ho0'
    rw [ho0]
    rcases hr with ⟨S', rfl⟩ | ⟨v, -, v1, v2, h1⟩
    · exact ⟨_, rfl⟩
    simp only [wRunF] at h1
    cases h1
    obtain ⟨r, hr⟩ := hBoxT v v1 v2
    simp [seqOut, wRunF, hBox, popArgs, hr]
  obtain ⟨o, hi⟩ := hin
  rw [eraseI_wordBlock, run_block, run_block, hi]
  rcases fast_run S box Ctr.hBox host ar callee M hC hBox w env t har wl st o hh hi with
      ⟨L, S', hL, rfl⟩ | ⟨L, v, wv, hL, hv, hc, rfl⟩
  · simp only [Nat.not_lt_zero, ↓reduceIte, List.take_zero, List.nil_append]
    have hhL := hh.after hL
    obtain ⟨oe, he⟩ := exactW_progress S host ar callee M box add sub mul cmp eq Ctr hBox hAdd hSub
      hMul hCmp hEq hDiv hBoxT hAddT hSubT mulOk hMulT env t L st ht har
      (fun l hl => let ⟨_, x, hx, hcx⟩ := hhL l hl; ⟨x, hx, hcx⟩) (LeavesPresent.after hh hp hL)
    have he' : wRunF host ar callee (exactW M t ++ []) L st = some oe := by simpa using he
    obtain ⟨_, wv, _, _, hr⟩ := exactW_run host ar callee S box add sub mul cmp eq Ctr hBox
      hAdd hSub hMul hCmp hEq hDiv env t L st [] oe har
      (fun l hl => let ⟨_, x, hx, hcx⟩ := hhL l hl; ⟨x, hx, hcx⟩) he'
    simp only [wRunF] at hr
    cases hr
    rw [he]
    simp [wRunF]
  · simp [wRunF]


theorem totW_leaf {M : MCtx} {n : Nat} {Γ : Nat → Option Ty} {mulOk : Bool} :
    ∀ {t : WTree}, t.totW mulOk = true → t.ok M n Γ = true →
      ∀ l ∈ t.leaves, ∃ i, l = .local i ∧ Γ i = some .int
  | .leaf (.local i), _, hok, l, hl => by
      simp only [WTree.leaves, List.mem_singleton] at hl
      subst hl
      simp only [WTree.ok, WLeaf.ok, beq_iff_eq] at hok
      exact ⟨i, rfl, hok⟩
  | .leaf (.field _ _ _ _), ht, _, _, _ => by simp [WTree.totW] at ht
  | .leaf (.word _ _ _), ht, _, _, _ => by simp [WTree.totW] at ht
  | .pick _ _ _, ht, _, _, _ => by simp [WTree.totW] at ht
  | .divLit _ _, ht, _, _, _ => by simp [WTree.totW] at ht
  | .modLit _ _, ht, _, _, _ => by simp [WTree.totW] at ht
  | .mulLit _ _ a, ht, hok, l, hl => by
      simp only [WTree.totW, Bool.and_eq_true] at ht
      simp only [WTree.ok, Bool.and_eq_true] at hok
      simp only [WTree.leaves] at hl
      exact totW_leaf ht.2 hok.2 l hl
  | .lit _, _, _, l, hl => by simp [WTree.leaves] at hl
  | .op _ a b, ht, hok, l, hl => by
      simp only [WTree.totW, Bool.and_eq_true] at ht
      simp only [WTree.ok, Bool.and_eq_true] at hok
      simp only [WTree.leaves, List.mem_append] at hl
      rcases hl with hl | hl
      · exact totW_leaf ht.1.2 hok.1.2 l hl
      · exact totW_leaf ht.2 hok.2 l hl

theorem aheadW_totW (M : MCtx) {mulOk : Bool} :
    ∀ {t : WTree}, t.totW mulOk = true → aheadW M t = []
  | .leaf (.local _), _ => rfl
  | .leaf (.field _ _ _ _), ht => by simp [WTree.totW] at ht
  | .leaf (.word _ _ _), ht => by simp [WTree.totW] at ht
  | .pick _ _ _, ht => by simp [WTree.totW] at ht
  | .divLit _ _, ht => by simp [WTree.totW] at ht
  | .modLit _ _, ht => by simp [WTree.totW] at ht
  | .mulLit _ _ a, ht => by
      simp only [WTree.totW, Bool.and_eq_true] at ht
      simp [aheadW, aheadW_totW M ht.2]
  | .lit _, _ => rfl
  | .op _ a b, ht => by
      simp only [WTree.totW, Bool.and_eq_true] at ht
      simp [aheadW, aheadW_totW M ht.1.2, aheadW_totW M ht.2]

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
  (hConsF : ∀ t f, M.listCons t = some f → ∀ h tl sv, HasTy M tl (.list t) →
    F f [h, tl] = some sv → sv = .cons t h tl)
  (X : LCtx)
  (hBoxT : ∀ k : Int, -(2 ^ 63 : Int) ≤ k → k < 2 ^ 63 → ∃ w, box [.i64v k] = some w)
  (hAddT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
  (hSubT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
  (mulOk : Bool)
  (hMulT : mulOk = true → ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, mul [va, vb] = some w)
  (mem : Nat → Bool) (calls : Bool) (n : Int) (lim : Nat)
  (hSig : ∀ g sig, mem g = true → M.sigs g = some sig → sig.ret = .int ∨ sig.ret = .bool)
  (hCallP : calls = true → ∀ g sig, mem g = true → M.sigs g = some sig →
    ∀ svs ws, HasTyL M (.i (n - 1) :: svs) sig.params →
      SReprL S M (.i (n - 1) :: svs) ws → ∃ r, callee g ws = some r)
include Ctr hNegC hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R hCallees hConsF hBoxT hAddT hSubT hMulT
  hSig hCallP

mutual
theorem progress :
    ∀ (e : Expr) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (tail : Bool) (T : Ty)
      (wl st : List WVal),
      totE mem mulOk calls lim e = true →
      (∀ i T', Γ i = some T' → T' ≠ .string) →
      tyOf M X.n Γ tail e = some T →
      EnvTy M env Γ →
      LRel S M X env wl →
      env 0 = some (.i n) →
      lim ≤ wl.length →
      ∃ out, wRunF host ar callee (lowerW M X Γ tail e) wl st = some out
  | .literal (.int k), Γ, env, tail, T, wl, st, _, _, hty, _, _, _, hlim => by
      obtain ⟨hband, _⟩ := tyOf_litInt_inv hty
      have hk : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
        simpa [inI64Band, Bool.and_eq_true, decide_eq_true_eq] using hband
      obtain ⟨w, hw⟩ := hBoxT k hk.1 hk.2
      simp [lowerW, lowerB, eraseL, eraseI, wRunF, hBox, popArgs, hw]
  | .literal (.bool v), Γ, env, tail, T, wl, st, _, _, _, _, _, _, hlim => by
      simp [lowerW, lowerB, eraseL, eraseI, wRunF]
  | .local i, Γ, env, tail, T, wl, st, _, _, hty, henv, hl, _, hlim => by
      simp only [tyOf] at hty
      obtain ⟨sv, hsv, _⟩ := envTy_get henv hty
      obtain ⟨_, w, hw, _⟩ := hl.2 i sv hsv
      simp [lowerW, lowerB, eraseL, eraseI, wRunF, hw]
  | .binOp op l r, Γ, env, tail, T, wl, st, htot, hΓ, hty, henv, hl, h0, hlim => by
      obtain ⟨hop, htl0, htr0⟩ := totE_binOp htot
      have hA : op.isArith = true := by
        rcases hop with rfl | rfl | ⟨rfl, _⟩ <;> rfl
      rcases tyOf_binOp_inv hty with ⟨htl, htr, _⟩ | ⟨_, _, hq, _⟩ | ⟨_, _, hq, _⟩ |
        ⟨htl, _, _⟩
      · simp only [lowerW, lowerB, htl, hA, ↓reduceIte, eraseL_append, eraseL, eraseI]
        obtain ⟨o1, h1⟩ := progress l Γ env false .int wl st htl0 hΓ htl henv hl h0 hlim
        rcases agreement S box add sub mul cmp eq neg Ctr hNegC host ar
          callee M hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R F hCallees hConsF X l Γ env false .int
          wl st o1 htl henv hl h1 with ⟨sv1, _, hT1, hres1⟩ | hE
        rotate_left
        · rw [totE_bare l htl0] at hE
          exact (esc_nil hE).elim
        obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
        obtain ⟨a, rfl⟩ := hasTy_int hT1
        obtain ⟨o2, h2⟩ := progress r Γ env false .int wl1 (w1 :: st) htr0 hΓ htr henv hl1 h0
          (by rw [wRunF_ok_length _ _ _ _ _ _ _ _ h1]; exact hlim)
        rcases agreement S box add sub mul cmp eq neg Ctr hNegC host ar
          callee M hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R F hCallees hConsF X r Γ env false .int
          wl1 (w1 :: st) o2 htr henv hl1 h2 with ⟨sv2, _, hT2, hres2⟩ | hE
        rotate_left
        · rw [totE_bare r htr0] at hE
          exact (esc_nil hE).elim
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
  | .call (.fn g) args, Γ, env, tail, T, wl, st, htot, hΓ, hty, henv, hl, h0, hlim => by
      simp only [totE, Bool.and_eq_true] at htot
      obtain ⟨⟨⟨hc, hmem⟩, hdh⟩, hargs⟩ := htot
      obtain ⟨sig, hsig, hts, rfl⟩ := tyOf_callFn_inv hty
      obtain ⟨hhost, har, _⟩ := hCallees g sig hsig
      simp only [lowerW, lowerB, eraseL_append]
      obtain ⟨o1, h1⟩ := progressArgs args Γ env sig.params wl st hargs hΓ hts henv hl h0 hlim
      rcases agreementArgs S box add sub mul cmp eq neg Ctr hNegC host ar callee M hCarrier hBox hAdd
          hSub hMul hNeg hCmp hEq R F hCallees hConsF X args Γ env sig.params wl st o1 hts henv hl h1
        with ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, _⟩ | hE
      rotate_left
      · rw [totArgs_bare args hargs] at hE
        exact (esc_nil hE).elim
      obtain ⟨a, rest, rfl, hda⟩ := descentHead_eq hdh
      have hd := isDescent_eval hda F h0
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
  | .tailCall g args, Γ, env, tail, T, wl, st, htot, hΓ, hty, henv, hl, h0, hlim => by
      simp only [totE, Bool.and_eq_true] at htot
      obtain ⟨⟨⟨hc, hmem⟩, hdh⟩, hargs⟩ := htot
      obtain ⟨_, sig, hsig, hts, rfl⟩ := tyOf_tailCall_inv hty
      obtain ⟨_, har, _⟩ := hCallees g sig hsig
      simp only [lowerW, lowerB, eraseL_append]
      obtain ⟨o1, h1⟩ := progressArgs args Γ env sig.params wl st hargs hΓ hts henv hl h0 hlim
      rcases agreementArgs S box add sub mul cmp eq neg Ctr hNegC host ar callee M hCarrier hBox hAdd
          hSub hMul hNeg hCmp hEq R F hCallees hConsF X args Γ env sig.params wl st o1 hts henv hl h1
        with ⟨svs, ws, wl1, rfl, hevs, hTs, hrep, _⟩ | hE
      rotate_left
      · rw [totArgs_bare args hargs] at hE
        exact (esc_nil hE).elim
      obtain ⟨a, rest, rfl, hda⟩ := descentHead_eq hdh
      have hd := isDescent_eval hda F h0
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
  | .literal (.float _), _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .literal (.str _), _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .let_ b v body, Γ, env, tail, T, wl, st, htot, hΓ, hty, henv, hl, h0, hlim => by
      simp only [totE, Bool.and_eq_true] at htot
      obtain ⟨hbn, hΓb, Tv, htv, htb⟩ := tyOf_let_inv hty
      simp only [lowerW, lowerB, htv, eraseL_append, List.append_assoc]
      obtain ⟨o1, h1⟩ := progress v Γ env false Tv wl st htot.1 hΓ htv henv hl h0 hlim
      rcases agreement S box add sub mul cmp eq neg Ctr hNegC host ar
        callee M hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R F hCallees hConsF X v Γ env false Tv
        wl st o1 htv henv hl h1 with ⟨sv1, _, hT1, hres1⟩ | hE
      rotate_left
      · rw [totE_bare v htot.1] at hE
        exact (esc_nil hE).elim
      obtain ⟨wl1, w1, rfl, hw1, hl1⟩ := res_false hres1
      have hlen1 := wRunF_ok_length _ _ _ _ _ _ _ _ h1
      rw [wRunF_append]
      simp only [lowerW] at h1
      rw [h1]
      simp only [seqOut, eraseL, eraseI, List.cons_append, List.nil_append, wRunF]
      have hv := totE_notStr (n := X.n) hSig v Γ false hΓ htot.1
      have hΓ' : ∀ i T', upd Γ b Tv i = some T' → T' ≠ .string := by
        intro i T' hi
        by_cases hib : i = b
        · subst hib
          rw [upd_same] at hi
          cases hi
          exact fun hs => hv (hs ▸ htv)
        · rw [upd_ne _ _ hib] at hi
          exact hΓ i T' hi
      have hb0 : (0 : Nat) ≠ b := by
        intro h
        have := envTy_free henv hΓb
        rw [← h, h0] at this
        cases this
      have h0' : upd env b sv1 0 = some (.i n) := by rw [upd_ne _ _ hb0]; exact h0
      exact progress body (upd Γ b Tv) (upd env b sv1) tail T (wl1.set b w1) st htot.2 hΓ' htb
        (envTy_upd henv hT1) (lrel_bind hl1 hbn hw1) h0' (by simp [hlen1]; exact hlim)
  | .call (.builtin _) _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .call (.lazy _) _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .neg _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .ifThenElse _ _ _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .recordCreate _ _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .project _ _ _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .match_ _ _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .construct _ _ _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .interp _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .list _ _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .try_ _ _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .scope _, _, _, _, _, _, _, ht, _, _, _, _, _, _ => by simp [totE] at ht
  | .intWord t w, Γ, env, tail, T, wl, st, htot, hΓ, hty, henv, hl, h0, hlim => by
      simp only [totE, Bool.and_eq_true, decide_eq_true_eq] at htot
      obtain ⟨htw, hwl⟩ := htot
      simp only [tyOf] at hty
      split at hty
      · rename_i hok
        simp only [wordOk, Bool.and_eq_true, decide_eq_true_eq] at hok
        obtain ⟨⟨htok, hnw⟩, _⟩ := hok
        have hleaf := totW_leaf htw htok
        have hh : LeavesHeld S w env wl t := by
          intro l hl'
          obtain ⟨i, rfl, hi⟩ := hleaf l hl'
          obtain ⟨v, hv, hTv⟩ := envTy_get henv hi
          obtain ⟨x, rfl⟩ := hasTy_int hTv
          obtain ⟨hin, wv, hw, hr⟩ := hl.2 i _ hv
          refine ⟨Or.inl (by simp only [WLeaf.loc]; omega), x, by simp [WLeaf.val, hv],
            fun wv' hw' => ?_⟩
          simp only [WLeaf.loc] at hw'
          rw [hw] at hw'
          cases hw'
          simpa [SRepr, WLeaf.holds] using hr
        have hp : LeavesPresent wl t := by
          intro l hl'
          obtain ⟨i, rfl, hi⟩ := hleaf l hl'
          obtain ⟨v, hv, _⟩ := envTy_get henv hi
          obtain ⟨_, wv, hw, _⟩ := hl.2 i _ hv
          exact ⟨wv, hw⟩
        simp only [lowerW, lowerB, aheadW_totW M htw, List.map_nil, List.nil_append, eraseL]
        exact wordBlock_progress S host ar callee M hCarrier box add sub mul cmp eq Ctr hBox hAdd
          hSub hMul hCmp hEq R.divmod hBoxT hAddT hSubT mulOk hMulT env t w wl st htw
          (WTree.arith_of_ok htok) hh hp (by omega)
      · cases hty

theorem progressArgs :
    ∀ (es : List Expr) (Γ : Nat → Option Ty) (env : Nat → Option SVal) (Ts : List Ty)
      (wl st : List WVal),
      totArgs mem mulOk calls lim es = true →
      (∀ i T', Γ i = some T' → T' ≠ .string) →
      tysOf M X.n Γ es = some Ts →
      EnvTy M env Γ →
      LRel S M X env wl →
      env 0 = some (.i n) →
      lim ≤ wl.length →
      ∃ out, wRunF host ar callee (lowerArgsW M X Γ es) wl st = some out
  | [], Γ, env, Ts, wl, st, _, _, _, _, _, _, hlim => by
      simp [lowerArgsW, lowerArgsB, eraseL, wRunF]
  | e :: es, Γ, env, Ts, wl, st, htot, hΓ, hty, henv, hl, h0, hlim => by
      simp only [totArgs, Bool.and_eq_true] at htot
      obtain ⟨t, ts, hte, htes, rfl⟩ := tysOf_cons_inv hty
      simp only [lowerArgsW, lowerArgsB, eraseL_append]
      obtain ⟨o1, h1⟩ := progress e Γ env false t wl st htot.1 hΓ hte henv hl h0 hlim
      rcases agreement S box add sub mul cmp eq neg Ctr hNegC host ar
        callee M hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R F hCallees hConsF X e Γ env false t
        wl st o1 hte henv hl h1 with ⟨sv, _, _, hres⟩ | hE
      rotate_left
      · rw [totE_bare e htot.1] at hE
        exact (esc_nil hE).elim
      obtain ⟨wl1, w, rfl, _, hl1⟩ := res_false hres
      obtain ⟨o2, h2⟩ := progressArgs es Γ env ts wl1 (w :: st) htot.2 hΓ htes henv hl1 h0
        (by rw [wRunF_ok_length _ _ _ _ _ _ _ _ h1]; exact hlim)
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
        code f = some (fnCodeAt M f p))
    (hCons : ∀ t f, M.listCons t = some f → ∀ k h tl sv, HasTy M tl (.list t) →
      groupModel outer G k f [h, tl] = some sv → sv = .cons t h tl)
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
    hBox hAdd hSub hMul hNeg hCmp hEq R G outer hOuter hMem hCons
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
    obtain ⟨⟨⟨hpn, hnl⟩, hty⟩, hbare⟩ := htyped
    have hbare' : bareTries p.body = [] := by simpa using hbare
    obtain ⟨hXn, hXc, hXs, hXsc⟩ := FnPlan.lctx_spec p
    rw [← hXn] at hty
    have hCallees := groupCallees S M code host G outer hOuter hSigOf hCert m
    have hlenw : ws.length = p.sig.params.length := by
      rw [← sreprL_length hrep, hasTyL_length hTs]
    have hLRp : ∀ pad : List WVal, pad.length = p.locals.length →
        LRel S M p.lctx (argsEnv (.i n :: tl)) (ws ++ pad) := by
      intro pad hpad
      refine ⟨by simp [hpad]; omega, ?_⟩
      intro i sv hs
      rw [hXn]
      obtain ⟨w, hw, hsv⟩ := sreprL_get hrep hs
      have hi : i < ws.length := by
        rcases Nat.lt_or_ge i ws.length with h | h
        · exact h
        · rw [List.getElem?_eq_none h] at hw; simp at hw
      refine ⟨by omega, w, ?_, hsv⟩
      simp [List.getElem?_append_left hi, hw]
    -- The entry locals of the body's run, and how its end becomes the
    -- function's result: as is, or as the self tail loop.
    have hentry : ∃ pad : List WVal, pad.length = p.locals.length ∧
        ∀ out, wRunF host (fun g => (code g).map (·.arity))
            (fun g as => wFuncN code host m g as) (fnCode M p).body (ws ++ pad) [] = some out →
          ((∃ l' w, out = .ok l' [w]) ∨ (∃ w, out = .ret w)) →
          ∃ r, wFuncN code host (m + 1) f ws = some r := by
      cases hst : p.selfLoop with
      | false =>
          rw [fnCodeAt_plain M f p hst] at hcode
          refine ⟨List.replicate p.locals.length .null, by simp, ?_⟩
          intro out hout hshape
          rw [InterpreterSequencing.wFuncN_plain hcode (fnCode_loopBody M p)]
          have hi : initLocals (fnCode M p) ws = ws ++ List.replicate p.locals.length .null := by
            simp [initLocals, fnCode]
          rw [hi, hout]
          rcases hshape with ⟨l', w, rfl⟩ | ⟨w, rfl⟩
          · exact ⟨w, rfl⟩
          · exact ⟨w, rfl⟩
      | true =>
          obtain ⟨zs, vs, L⟩ := fnCodeAt_loop M f p hst
          obtain ⟨har, hnl, hbody⟩ := fnCodeAt_shape M f p
          refine ⟨vs, by rw [L.len, hnl]; simp [fnCode], ?_⟩
          intro out hout hshape
          have hlen : ((none : Option (List WVal)).getD
              (initLocals (fnCodeAt M f p) ws)).length =
              (fnCodeAt M f p).arity + (fnCodeAt M f p).nlocals := by
            simp [initLocals, har, hnl, fnCode, hlenw]
          have hp : ((none : Option (List WVal)).getD
              (initLocals (fnCodeAt M f p) ws)).take (fnCodeAt M f p).arity = ws := by
            simp [initLocals, har, fnCode, hlenw]
          unfold wFuncN
          rw [InterpreterSequencing.wEnter_loop hcode L m ws none hlen, hp, hbody, hout]
          rcases hshape with ⟨l', w, rfl⟩ | ⟨w, rfl⟩
          · exact ⟨w, rfl⟩
          · exact ⟨w, rfl⟩
    obtain ⟨pad, hpad, hfin⟩ := hentry
    have hLR := hLRp pad hpad
    have henv := envTy_args hTs
    have hΓ : ∀ i T', paramsΓ p.sig.params i = some T' → T' ≠ .string := by
      intro i T' hi hs
      have := paramsΓ_int hall i T' hi
      rw [hs] at this
      cases this
    have h0 : argsEnv (.i n :: tl) 0 = some (.i n) := rfl
    obtain ⟨w0, ws', rfl, hw0, _⟩ := sreprL_cons_inv hrep
    have hget : ((w0 :: ws') ++ pad)[0]? = some w0 := by
      simp
    have hΓ0 : paramsΓ p.sig.params 0 = some .int := by simp [paramsΓ, hps]
    have hlimw : p.sig.params.length + p.locals.length ≤ ((w0 :: ws') ++ pad).length := by
      simp only [List.length_append, hpad]
      omega
    -- the body runs to some outcome
    have hrun : ∃ out, wRunF host (fun g => (code g).map (·.arity))
        (fun g as => wFuncN code host m g as) (fnCode M p).body
        ((w0 :: ws') ++ pad) [] = some out := by
      have hbodyEq : (fnCode M p).body = eraseL (cmpArmB C 0 .lte 0) ++
          [WInstr.ifElse (eraseL (lowerB M p.lctx (paramsΓ p.sig.params) true base))
            (eraseL (lowerB M p.lctx (paramsΓ p.sig.params) true step))] := by
        simp only [fnCode, hb, lowerW, lowerB, hΓ0, tyOf, BinOp.isArith, Bool.false_eq_true,
          ↓reduceIte, litInt?, slot?, eraseL_append, eraseL, eraseI, hCarrier]
      rw [hbodyEq, wRunF_append, guard_run S _ _ _ 0 _ [] n w0 hget hw0]
      simp only [seqOut, b32]
      rw [wRunF_ifElse_single _ _ _ _ _ _ _ _ (escFree_lowerB _ _ _ _ _ 0) (escFree_lowerB _ _ _ _ _ 0)]
      by_cases hn : n ≤ 0
      · simp only [hn, decide_true, ↓reduceIte, Int.reduceEq]
        have hbty := (tyOf_ite_inv (hb ▸ hty)).2.1
        exact progress (S := S) (box := box) (add := add) (sub := sub) (mul := mul) (cmp := cmp)
          (eq := eq) (neg := neg) (Ctr := Ctr) (hNegC := hNegC) (host := host)
          (ar := fun g => (code g).map (·.arity)) (callee := fun g as => wFuncN code host m g as)
          (M := M) (hCarrier := hCarrier) (hBox := hBox) (hAdd := hAdd) (hSub := hSub)
          (hMul := hMul) (hNeg := hNeg) (hCmp := hCmp) (hEq := hEq) (R := R)
          (F := groupModel outer G m) (hCallees := hCallees)
          (hConsF := fun t g hg h tl sv htl hm => hCons t g hg m h tl sv htl hm) (X := p.lctx) (hBoxT := hBoxT)
          (hAddT := hAddT) (hSubT := hSubT) (mulOk := mulOk) (hMulT := hMulT) (mem := mem)
          (calls := false) (n := n) (lim := p.sig.params.length + p.locals.length) (hSig := hSig) (hCallP := fun h => by cases h)
          base _ _ true p.sig.ret _ [] hbase hΓ hbty henv hLR h0 hlimw
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
          (F := groupModel outer G m) (hCallees := hCallees)
          (hConsF := fun t g hg h tl sv htl hm => hCons t g hg m h tl sv htl hm) (X := p.lctx) (hBoxT := hBoxT)
          (hAddT := hAddT) (hSubT := hSubT) (mulOk := mulOk) (hMulT := hMulT) (mem := mem)
          (calls := true) (n := n) (lim := p.sig.params.length + p.locals.length) (hSig := hSig) (hCallP := hCallP)
          step _ _ true p.sig.ret _ [] hstep hΓ hsty henv hLR h0 hlimw
    obtain ⟨out, hout⟩ := hrun
    rcases agreement S box add sub mul cmp eq neg Ctr hNegC host
      (fun g => (code g).map (·.arity)) (fun g as => wFuncN code host m g as) M
      hCarrier hBox hAdd hSub hMul hNeg hCmp hEq R (groupModel outer G m) hCallees
      (fun t g hg h tl sv htl hm => hCons t g hg m h tl sv htl hm)
      p.lctx p.body (paramsΓ p.sig.params) (argsEnv (.i n :: tl)) true p.sig.ret
      ((w0 :: ws') ++ pad) [] out hty henv hLR hout with ⟨sv, _, _, hres⟩ | hE
    rotate_left
    · rw [hbare'] at hE
      exact (esc_nil hE).elim
    refine hfin out hout ?_
    cases out with
    | ok wl' st' =>
        obtain ⟨w, rfl, _, _⟩ := hres
        exact Or.inl ⟨wl', w, rfl⟩
    | ret w => exact Or.inr ⟨w, rfl⟩
    | br _ _ _ => exact hres.elim
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
        code f = some (fnCodeAt M f p))
    (hCons : ∀ t f, M.listCons t = some f → ∀ k h tl sv, HasTy M tl (.list t) →
      groupModel outer G k f [h, tl] = some sv → sv = .cons t h tl)
    (hBoxT : ∀ k : Int, -(2 ^ 63 : Int) ≤ k → k < 2 ^ 63 → ∃ w, box [.i64v k] = some w)
    (hAddT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
    (hSubT : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
    (hMulT : role = .mul → ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, mul [va, vb] = some w) :
    ∀ f p, G f = some p →
      FnCertified S M code host f p.sig (fun fuel => groupModel outer G fuel f) ∧
        FnTotal S M code host f p.sig (fun fuel => groupModel outer G fuel f) := by
  obtain ⟨_, hall⟩ := checkTermGroup_spec hck
  refine fn_certified_total S box add sub mul cmp eq neg Ctr hNegC code host M hCarrier hBox hAdd
    hSub hMul hNeg hCmp hEq R G outer hOuter hMem hCons hBoxT hAddT hSubT (role == .mul)
    (fun h => hMulT (by simpa using h)) (memOf ms) ?_ ?_
  · intro g hg
    obtain ⟨m, hm, hmg⟩ := List.any_eq_true.mp hg
    simp only [beq_iff_eq] at hmg
    subst hmg
    exact ⟨m.2, hGall m hm⟩
  · intro f p hG
    exact hall (f, p) (hGin f p hG)

end AverCert.Grammar
