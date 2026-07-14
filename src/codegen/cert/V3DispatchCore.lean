import CertPrelude
import SchemaCore
import PlanCheck
import PlanLower

set_option maxRecDepth 100000
set_option maxHeartbeats 2000000

namespace V3Dispatch
open CertPrelude AverCert.Schema

/-! ### Semantic evaluator and family invariant -/

def evalLeaf : IntDispatchLeaf → Int → Int
  | .proj, x => x
  | .hostOp .add k true, x => k + x
  | .hostOp .add k false, x => x + k
  | .hostOp .sub k true, x => k - x
  | .hostOp .sub k false, x => x - k

/-- `EvalCascade S body tag fields n` is the semantic meaning of one admitted
    dispatch input. It relates the byte-origin plan to the existing source
    model without inventing a decoder for the abstract Int carrier. -/
inductive EvalCascade {C : Nat} (S : CarrierSpec C) :
    IntDispatchCascade → Nat → List WVal → Int → Prop where
  | default (k : Int) (tag : Nat) (fields : List WVal) :
      EvalCascade S (.default k) tag fields k
  | hit (tyIdx : Nat) (leaf : IntDispatchLeaf) (rest : IntDispatchCascade)
      (fields : List WVal) (x : Int) (v : WVal)
      (hfield : fields[0]? = some v) (hrepr : S.Repr x v) :
      EvalCascade S (.test tyIdx leaf rest) tyIdx fields (evalLeaf leaf x)
  | miss (tyIdx tag : Nat) (leaf : IntDispatchLeaf) (rest : IntDispatchCascade)
      (fields : List WVal) (n : Int) (hne : tag ≠ tyIdx)
      (hrest : EvalCascade S rest tag fields n) :
      EvalCascade S (.test tyIdx leaf rest) tag fields n

theorem evalCascade_hit_inv {C : Nat} {S : CarrierSpec C}
    {tyIdx : Nat} {leaf : IntDispatchLeaf} {rest : IntDispatchCascade}
    {fields : List WVal} {n : Int}
    (h : EvalCascade S (.test tyIdx leaf rest) tyIdx fields n) :
    ∃ x v, fields[0]? = some v ∧ S.Repr x v ∧ n = evalLeaf leaf x := by
  cases h with
  | hit _ _ _ _ x v hfield hrepr => exact ⟨x, v, hfield, hrepr, rfl⟩
  | miss _ _ _ _ _ _ hne _ => exact False.elim (hne rfl)

theorem evalCascade_miss_inv {C : Nat} {S : CarrierSpec C}
    {tyIdx tag : Nat} {leaf : IntDispatchLeaf} {rest : IntDispatchCascade}
    {fields : List WVal} {n : Int} (hne : tag ≠ tyIdx)
    (h : EvalCascade S (.test tyIdx leaf rest) tag fields n) :
    EvalCascade S rest tag fields n := by
  cases h with
  | hit _ _ _ _ _ _ _ _ => exact False.elim (hne rfl)
  | miss _ _ _ _ _ _ _ hrest => exact hrest

/-- The template's stack invariant at a block boundary: a successful sub-block
    leaves one represented result above the unchanged incoming stack. -/
def StackOK {C : Nat} (S : CarrierSpec C) (n : Int) (base : List WVal) :
    Option Out → Prop
  | some (.ok _ (w :: rest)) => rest = base ∧ S.Repr n w
  | _ => False

/-- A block has the same `StackOK`-preserving property at every nesting depth. -/
def BlockOK {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (n : Int) (base : List WVal) (instrs : List WInstr)
    (locals : List WVal) (stack : List WVal) : Prop :=
  ∀ out, wRunF host ar callee instrs locals stack = some out →
    StackOK S n base (some out)

theorem finishRun_nil
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (r : Option Out) :
    (match r with
     | some (.ok locals stack) => wRunF host ar callee [] locals stack
     | some (.ret value) => some (.ret value)
     | none => none) = r := by
  cases r with
  | none => rfl
  | some out => cases out <;> simp [wRunF]

theorem popArgs_one (a : WVal) (rest : List WVal) :
    popArgs 1 (a :: rest) = some ([a], rest) := by
  simp [popArgs, List.take, List.drop]

theorem popArgs_two (b a : WVal) (rest : List WVal) :
    popArgs 2 (b :: a :: rest) = some ([a, b], rest) := by
  simp [popArgs, List.take, List.drop]

/-! ### The branching arm, proved once -/

/-- Both sub-blocks satisfy the same invariant; selecting either branch
    preserves it. This is the only proof that unfolds nested-block sequencing. -/
theorem blockOK_ifElse {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (n : Int) (base : List WVal) (thenB elseB : List WInstr)
    (locals : List WVal) (stack : List WVal) (cond : Bool)
    (hbranch : BlockOK S host ar callee n base
      (if cond then thenB else elseB) locals stack) :
    BlockOK S host ar callee n base [.ifElse thenB elseB]
      locals (b32 cond :: stack) := by
  cases cond with
  | false =>
      intro out hrun
      cases hb : wRunF host ar callee elseB locals stack with
      | none => simp [wRunF, b32, hb] at hrun
      | some branchOut =>
          cases branchOut <;> simp [wRunF, b32, hb] at hrun
          all_goals subst out; exact hbranch _ (by simpa using hb)
  | true =>
      intro out hrun
      cases hb : wRunF host ar callee thenB locals stack with
      | none => simp [wRunF, b32, hb] at hrun
      | some branchOut =>
          cases branchOut <;> simp [wRunF, b32, hb] at hrun
          all_goals subst out; exact hbranch _ (by simpa using hb)

/-! ### Host-slot hypotheses -/

def HostSlots (C : Nat) (host : HostTbl)
    (hostTable : List (HostRole × Nat))
    (add sub : List WVal → Option WVal) : Prop :=
  (∀ idx, AverCert.PlanCheck.hostRoleIdx? hostTable .box = some idx →
      host idx = some (1, boxRef C)) ∧
  (∀ idx, AverCert.PlanCheck.hostRoleIdx? hostTable .add = some idx →
      host idx = some (2, add)) ∧
  (∀ idx, AverCert.PlanCheck.hostRoleIdx? hostTable .sub = some idx →
      host idx = some (2, sub))

/-! ### Family admission -/

/-- Every tested struct type must come from the byte-pinned nominal variant
    set for the exported sum root. The production raw checker supplies the
    profile guard; this family wrapper supplies the GuardIso index guard used
    by the generic theorem lane. -/
def checkCascadeTypes (pinned : List Nat) : IntDispatchCascade → Bool
  | .default _ => true
  | .test tyIdx _ rest => pinned.contains tyIdx && checkCascadeTypes pinned rest

def checkIntDispatchFamily (pinned : List Nat) (plan : IntDispatchRawPlan) : Bool :=
  AverCert.PlanCheck.checkIntDispatchRawPlan plan &&
    checkCascadeTypes pinned plan.body

/-! ### Leaf simulation -/

theorem simLeaf {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostTable : List (HostRole × Nat))
    (add sub : List WVal → Option WVal)
    (hslots : HostSlots C host hostTable add sub)
    (hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (scrutineeLocal fieldLocal tyIdx : Nat)
    (locals : List WVal) (fields : List WVal) (x : Int) (v : WVal)
    (hslot : locals[scrutineeLocal]? = some (.structv tyIdx fields))
    (hfieldLocal : fieldLocal < locals.length)
    (hfield : fields[0]? = some v) (hrepr : S.Repr x v) :
    ∀ leaf instrs base,
      AverCert.PlanLower.lowerIntDispatchArm hostTable
          scrutineeLocal fieldLocal tyIdx leaf = some instrs →
      BlockOK S host ar callee (evalLeaf leaf x) base instrs locals base := by
  intro leaf instrs base hlow
  cases leaf with
  | proj =>
      simp only [AverCert.PlanLower.lowerIntDispatchArm, Option.some.injEq] at hlow
      subst instrs
      have hset : (locals.set fieldLocal v)[fieldLocal]? = some v :=
        List.getElem?_set_self hfieldLocal
      simp [BlockOK, StackOK, wRunF, hslot, hfield, hset, evalLeaf, hrepr]
  | hostOp role k constFirst =>
      cases hb : AverCert.PlanCheck.hostRoleIdx? hostTable .box with
      | none => simp [AverCert.PlanLower.lowerIntDispatchArm, hb] at hlow
      | some boxIdx =>
          cases hh : AverCert.PlanCheck.hostRoleIdx? hostTable
              (AverCert.PlanCheck.intDispatchRoleHostRole role) with
          | none => simp [AverCert.PlanLower.lowerIntDispatchArm, hb, hh] at hlow
          | some hostIdx =>
              simp only [AverCert.PlanLower.lowerIntDispatchArm, hb, hh,
                Option.some.injEq] at hlow
              subst instrs
              have hset : (locals.set fieldLocal v)[fieldLocal]? = some v :=
                List.getElem?_set_self hfieldLocal
              have hbox := hslots.1 boxIdx hb
              cases role with
              | add =>
                  have hhost := hslots.2.1 hostIdx hh
                  cases constFirst with
                  | false =>
                      cases hop : add [v, carrierSmall C k] with
                      | none => simp [BlockOK, StackOK, wRunF, hslot, hfield,
                          hset, hbox, boxRef, hhost, popArgs_one, popArgs_two, hop]
                      | some w =>
                          have hw := hadd x k v (carrierSmall C k) w hrepr
                            (S.smallIntro k) hop
                          simp [BlockOK, StackOK, wRunF, hslot, hfield,
                            hset, hbox, boxRef, hhost, popArgs_one, popArgs_two,
                            hop, evalLeaf, hw]
                  | true =>
                      cases hop : add [carrierSmall C k, v] with
                      | none => simp [BlockOK, StackOK, wRunF, hslot, hfield,
                          hset, hbox, boxRef, hhost, popArgs_one, popArgs_two, hop]
                      | some w =>
                          have hw := hadd k x (carrierSmall C k) v w
                            (S.smallIntro k) hrepr hop
                          simp [BlockOK, StackOK, wRunF, hslot, hfield,
                            hset, hbox, boxRef, hhost, popArgs_one, popArgs_two,
                            hop, evalLeaf, hw]
              | sub =>
                  have hhost := hslots.2.2 hostIdx hh
                  cases constFirst with
                  | false =>
                      cases hop : sub [v, carrierSmall C k] with
                      | none => simp [BlockOK, StackOK, wRunF, hslot, hfield,
                          hset, hbox, boxRef, hhost, popArgs_one, popArgs_two, hop]
                      | some w =>
                          have hw := hsub x k v (carrierSmall C k) w hrepr
                            (S.smallIntro k) hop
                          simp [BlockOK, StackOK, wRunF, hslot, hfield,
                            hset, hbox, boxRef, hhost, popArgs_one, popArgs_two,
                            hop, evalLeaf, hw]
                  | true =>
                      cases hop : sub [carrierSmall C k, v] with
                      | none => simp [BlockOK, StackOK, wRunF, hslot, hfield,
                          hset, hbox, boxRef, hhost, popArgs_one, popArgs_two, hop]
                      | some w =>
                          have hw := hsub k x (carrierSmall C k) v w
                            (S.smallIntro k) hrepr hop
                          simp [BlockOK, StackOK, wRunF, hslot, hfield,
                            hset, hbox, boxRef, hhost, popArgs_one, popArgs_two,
                            hop, evalLeaf, hw]

/-! ### Nested cascade simulation -/

theorem simCascade {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostTable : List (HostRole × Nat))
    (add sub : List WVal → Option WVal)
    (hslots : HostSlots C host hostTable add sub)
    (hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (scrutineeLocal : Nat) (locals : List WVal)
    (tag : Nat) (fields : List WVal)
    (hslot : locals[scrutineeLocal]? = some (.structv tag fields)) :
    ∀ body pos first n instrs base,
      EvalCascade S body tag fields n →
      pos + AverCert.PlanCheck.intDispatchArmCount body < locals.length →
      (match body with | .default _ => first = false | .test _ _ _ => True) →
      AverCert.PlanLower.lowerIntDispatchCascade hostTable scrutineeLocal
          pos first body = some instrs →
      BlockOK S host ar callee n base instrs locals
        (if first then .structv tag fields :: base else base) := by
  intro body
  induction body with
  | default k =>
      intro pos first n instrs base hsem _hlen hfirst hlow
      cases hsem
      have hfirst' : first = false := hfirst
      subst first
      cases hb : AverCert.PlanCheck.hostRoleIdx? hostTable .box with
      | none => simp [AverCert.PlanLower.lowerIntDispatchCascade, hb] at hlow
      | some boxIdx =>
          simp only [AverCert.PlanLower.lowerIntDispatchCascade, hb,
            Option.some.injEq] at hlow
          subst instrs
          have hbox := hslots.1 boxIdx hb
          simpa [BlockOK, StackOK, wRunF, hbox, boxRef, popArgs_one]
            using S.smallIntro k
  | test tyIdx leaf rest ih =>
      intro pos first n instrs base hsem hlen _hfirst hlow
      cases ha : AverCert.PlanLower.lowerIntDispatchArm hostTable
          scrutineeLocal (pos + 1) tyIdx leaf with
      | none => simp [AverCert.PlanLower.lowerIntDispatchCascade, ha] at hlow
      | some hitInstrs =>
          cases hr : AverCert.PlanLower.lowerIntDispatchCascade hostTable
              scrutineeLocal (pos + 1) false rest with
          | none => simp [AverCert.PlanLower.lowerIntDispatchCascade, ha, hr] at hlow
          | some restInstrs =>
              simp only [AverCert.PlanLower.lowerIntDispatchCascade, ha, hr,
                Option.some.injEq] at hlow
              subst instrs
              have hfieldLocal : pos + 1 < locals.length := by
                simp only [AverCert.PlanCheck.intDispatchArmCount] at hlen
                omega
              by_cases htag : tag = tyIdx
              · subst tag
                obtain ⟨x, v, hfield, hrepr, hn⟩ := evalCascade_hit_inv hsem
                subst n
                have hhit := simLeaf S host ar callee hostTable add sub hslots
                  hadd hsub scrutineeLocal (pos + 1) tyIdx locals fields x v
                  hslot hfieldLocal hfield hrepr leaf hitInstrs base ha
                have hif := blockOK_ifElse S host ar callee
                  (evalLeaf leaf x) base hitInstrs restInstrs locals base true
                  (by simpa using hhit)
                cases first <;>
                  simpa [BlockOK, StackOK, wRunF, hslot, b32] using hif
              · have hrest := evalCascade_miss_inv htag hsem
                have hrestLen : pos + 1 +
                    AverCert.PlanCheck.intDispatchArmCount rest < locals.length := by
                  simp only [AverCert.PlanCheck.intDispatchArmCount] at hlen
                  omega
                have htail := ih (pos + 1) false n restInstrs base hrest
                  hrestLen (by cases rest <;> simp) hr
                have hif := blockOK_ifElse S host ar callee n base
                  hitInstrs restInstrs locals base false (by simpa using htail)
                cases first <;>
                  simp [BlockOK, StackOK, wRunF, hslot, b32, htag] at hif ⊢ <;>
                  exact hif

/-! ### Generic family certificate -/

theorem generic_int_dispatch_certified {C : Nat} (S : CarrierSpec C)
    (plan : IntDispatchRawPlan)
    (code : CodeTbl) (host : HostTbl) (self : Nat)
    (hostTable : List (HostRole × Nat))
    (add sub : List WVal → Option WVal)
    (hslots : HostSlots C host hostTable add sub)
    (hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (pinned : List Nat)
    (_hcheck : checkIntDispatchFamily pinned plan = true)
    (hroot : ∃ tyIdx leaf rest, plan.body = .test tyIdx leaf rest)
    (body : List WInstr)
    (hlow : AverCert.PlanLower.lowerIntDispatchBody hostTable plan = some body)
    (hself : code self = some {
      arity := 1,
      nlocals := AverCert.PlanCheck.intDispatchArmCount plan.body + 2,
      body := body }) :
    ∀ fuel tag fields n w,
      EvalCascade S plan.body tag fields n →
      wFuncN code host (fuel + 1) self [.structv tag fields] = some w →
      S.Repr n w := by
  intro fuel tag fields n w hsem hrun
  rcases hroot with ⟨rootTy, rootLeaf, rootRest, hroot⟩
  simp only [wFuncN, hself] at hrun
  let nlocals := AverCert.PlanCheck.intDispatchArmCount plan.body + 2
  let scrutineeLocal := AverCert.PlanCheck.intDispatchArmCount plan.body + 1
  let locals : List WVal := [WVal.structv tag fields] ++
    List.replicate nlocals WVal.null
  let updated := locals.set scrutineeLocal (WVal.structv tag fields)
  have hslt : scrutineeLocal < locals.length := by
    simp [scrutineeLocal, locals, nlocals]
  have hslot : updated[scrutineeLocal]? = some (.structv tag fields) := by
    exact List.getElem?_set_self hslt
  have hlen : 0 + AverCert.PlanCheck.intDispatchArmCount plan.body <
      updated.length := by
    simp [updated, locals, nlocals]
    omega
  cases hcascade : AverCert.PlanLower.lowerIntDispatchCascade hostTable
      scrutineeLocal 0 true plan.body with
  | none => simp [AverCert.PlanLower.lowerIntDispatchBody, scrutineeLocal,
      hcascade] at hlow
  | some cascade =>
      simp only [AverCert.PlanLower.lowerIntDispatchBody, scrutineeLocal,
        hcascade, Option.some.injEq] at hlow
      subst body
      have hsim := simCascade S host (fun g => (code g).map (·.arity))
        (fun g args => wFuncN code host fuel g args)
        hostTable add sub hslots hadd hsub scrutineeLocal updated tag fields hslot
        plan.body 0 true n cascade [] hsem hlen (by
          simp [hroot])
        hcascade
      change
        (match wRunF host (fun g => (code g).map (·.arity))
          (fun g args => wFuncN code host fuel g args)
          ([.localGet 0, .localSet scrutineeLocal, .localGet scrutineeLocal] ++ cascade)
          locals [] with
        | some (.ok _ [value]) => some value
        | some (.ret value) => some value
        | _ => none) = some w at hrun
      simp only [List.cons_append, List.nil_append, wRunF] at hrun
      have hzero : locals[0]? = some (WVal.structv tag fields) := by
        simp [locals]
      have hslotRaw : (locals.set scrutineeLocal
          (WVal.structv tag fields))[scrutineeLocal]? =
          some (WVal.structv tag fields) :=
        List.getElem?_set_self hslt
      simp only [hzero, hslotRaw] at hrun
      change
        (match wRunF host (fun g => (code g).map (·.arity))
          (fun g args => wFuncN code host fuel g args)
          cascade updated [WVal.structv tag fields] with
        | some (.ok _ [value]) => some value
        | some (.ret value) => some value
        | _ => none) = some w at hrun
      cases hr : wRunF host (fun g => (code g).map (·.arity))
          (fun g args => wFuncN code host fuel g args)
          cascade updated [.structv tag fields] with
      | none => simp [hr] at hrun
      | some out =>
          cases out with
          | ret value =>
              simp [hr] at hrun
              have hfalse := hsim (.ret value) hr
              simp [StackOK] at hfalse
          | ok finalLocals stack =>
              cases stack with
              | nil => simp [hr] at hrun
              | cons value rest =>
                  cases rest with
                  | nil =>
                      simp [hr] at hrun
                      subst w
                      have hok := hsim (.ok finalLocals [value]) hr
                      exact hok.2
                  | cons x xs => simp [hr] at hrun


end V3Dispatch
