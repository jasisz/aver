/- V3 RECURSION SKELETON SPIKE — generic fuel-induction soundness for the
   descent-by-one unary recursion family.

   One theorem, proven ONCE by fuel induction generically over parsed recursion
   plans, that subsumes every per-artifact generated fuel-recursion certificate
   of the unary family: if the (data-carrying) shape parser accepts a plan and
   the code table carries the plan's canonical lowering, then for every
   represented input, every terminating interpreter run returns a
   representation of the plan-derived model value.                            -/
import CertPrelude
import SchemaCore
import PlanCheck
import PlanLower

set_option maxRecDepth 100000

namespace V3Rec
open CertPrelude AverCert.Schema AverCert.PlanLower

/-! ### The parsed shape of a unary descent-by-one recursion plan -/

/-- The four byte-derived step variants of the unary family. -/
inductive RecStep where
  | inputSecond                 -- n + f (n-1)
  | constSecond (k : Int)       -- k + f (n-1)
  | inputFirst                  -- f (n-1) + n
  | constFirst (k : Int)        -- f (n-1) + k
deriving Repr, DecidableEq

structure RecShapeU where
  base : Int
  step : RecStep
deriving Repr, DecidableEq

/-! ### Data-carrying parsers (mirrors of PlanCheck's boolean shape checks) -/

def parseBaseU (boxIdx : Nat) (b : FragBlock) : Option Int :=
  match b.nodes with
  | [{ id := 0, ty := .i64, kind := .constI64 k },
     { id := 1, ty := .intCarrier, kind := .hostCall .box bi [0] }] =>
      if b.result = 1 ∧ bi = boxIdx then some k else none
  | _ => none

def parseStepU (self boxIdx addIdx subIdx : Nat) (b : FragBlock) : Option RecStep :=
  match b.nodes with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .intCarrier, kind := .local 0 },
     { id := 2, ty := .i64, kind := .constI64 one },
     { id := 3, ty := .intCarrier, kind := .hostCall .box bi [2] },
     { id := 4, ty := .intCarrier, kind := .hostCall .sub si [1, 3] },
     { id := 5, ty := .intCarrier, kind := .selfCall false sc [4] },
     { id := 6, ty := .intCarrier, kind := .hostCall .add ai [0, 5] }] =>
      if b.result = 6 ∧ one = 1 ∧ bi = boxIdx ∧ si = subIdx ∧ sc = self ∧ ai = addIdx then
        some .inputSecond
      else none
  | [{ id := 0, ty := .i64, kind := .constI64 k },
     { id := 1, ty := .intCarrier, kind := .hostCall .box bk [0] },
     { id := 2, ty := .intCarrier, kind := .local 0 },
     { id := 3, ty := .i64, kind := .constI64 one },
     { id := 4, ty := .intCarrier, kind := .hostCall .box bi [3] },
     { id := 5, ty := .intCarrier, kind := .hostCall .sub si [2, 4] },
     { id := 6, ty := .intCarrier, kind := .selfCall false sc [5] },
     { id := 7, ty := .intCarrier, kind := .hostCall .add ai [1, 6] }] =>
      if b.result = 7 ∧ one = 1 ∧ bk = boxIdx ∧ bi = boxIdx ∧ si = subIdx ∧ sc = self ∧ ai = addIdx then
        some (.constSecond k)
      else none
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .constI64 one },
     { id := 2, ty := .intCarrier, kind := .hostCall .box bi [1] },
     { id := 3, ty := .intCarrier, kind := .hostCall .sub si [0, 2] },
     { id := 4, ty := .intCarrier, kind := .selfCall false sc [3] },
     { id := 5, ty := .intCarrier, kind := .local 0 },
     { id := 6, ty := .intCarrier, kind := .hostCall .add ai [4, 5] }] =>
      if b.result = 6 ∧ one = 1 ∧ bi = boxIdx ∧ si = subIdx ∧ sc = self ∧ ai = addIdx then
        some .inputFirst
      else none
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .constI64 one },
     { id := 2, ty := .intCarrier, kind := .hostCall .box bi [1] },
     { id := 3, ty := .intCarrier, kind := .hostCall .sub si [0, 2] },
     { id := 4, ty := .intCarrier, kind := .selfCall false sc [3] },
     { id := 5, ty := .i64, kind := .constI64 k },
     { id := 6, ty := .intCarrier, kind := .hostCall .box bk [5] },
     { id := 7, ty := .intCarrier, kind := .hostCall .add ai [4, 6] }] =>
      if b.result = 7 ∧ one = 1 ∧ bi = boxIdx ∧ bk = boxIdx ∧ si = subIdx ∧ sc = self ∧ ai = addIdx then
        some (.constFirst k)
      else none
  | _ => none

def parseTopU (self boxIdx addIdx subIdx : Nat) (b : FragBlock) : Option RecShapeU :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .ref, kind := .structGet 1 0 },
     { id := 2, ty := .boolI32, kind := .refIsNull 1 },
     { id := 3, ty := .boolI32, kind := .ifElse 2
        ({ nodes := [{ id := 0, ty := .intCarrier, kind := .local 0 },
           { id := 1, ty := .i64, kind := .structGet 0 0 },
           { id := 2, ty := .i64, kind := .constI64 (0 : Int) },
           { id := 3, ty := .boolI32, kind := .prim .i64LeS [1, 2] }], result := 3 } : FragBlock)
        ({ nodes := [{ id := 0, ty := .intCarrier, kind := .local 0 },
           { id := 1, ty := .rawI32, kind := .structGet 2 0 },
           { id := 2, ty := .boolI32, kind := .constBool false },
           { id := 3, ty := .boolI32, kind := .prim .i32LtS [1, 2] }], result := 3 } : FragBlock) },
     { id := 4, ty := .intCarrier, kind := .ifElse 3 base step }], 4 =>
      match parseBaseU boxIdx base, parseStepU self boxIdx addIdx subIdx step with
      | some bv, some st => some ⟨bv, st⟩
      | _, _ => none
  | _, _ => none

def parseRecShapeU (self boxIdx addIdx subIdx : Nat)
    (plan : RecursionRawPlan) : Option RecShapeU :=
  if plan.profile = "recursion-plan-v1" ∧ plan.params = [FragTy.intCarrier] ∧
      AverCert.PlanCheck.sameTy plan.result .intCarrier then
    parseTopU self boxIdx addIdx subIdx plan.body
  else none

/-! ### The generic plan-derived model (fuel twin + natAbs face, once) -/

def stepEval : RecStep → Int → Int → Int
  | .inputSecond, n, r => n + r
  | .constSecond k, _, r => k + r
  | .inputFirst, n, r => r + n
  | .constFirst k, _, r => r + k

def evalRecUFuel (sh : RecShapeU) : Nat → Int → Int
  | 0, _ => 0
  | fuel + 1, n =>
      if n ≤ 0 then sh.base else stepEval sh.step n (evalRecUFuel sh fuel (n - 1))

def evalRecU (sh : RecShapeU) (n : Int) : Int :=
  evalRecUFuel sh (n.natAbs + 1) n

theorem evalRecU_fuel_irrel (sh : RecShapeU) :
    ∀ (t k1 k2 : Nat) (n : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      evalRecUFuel sh k1 n = evalRecUFuel sh k2 n := by
  intro t
  induction t with
  | zero => intro k1 k2 n ht _ _; omega
  | succ t ih =>
      intro k1 k2 n ht h1 h2
      cases k1 with
      | zero => omega
      | succ m1 =>
      cases k2 with
      | zero => omega
      | succ m2 =>
      by_cases hn : n ≤ 0
      · simp [evalRecUFuel, hn]
      · have hrec := ih m1 m2 (n - 1) (by omega) (by omega) (by omega)
        simp only [evalRecUFuel]
        rw [if_neg hn, if_neg hn, hrec]

theorem evalRecU_fuel_stable (sh : RecShapeU) (k : Nat) (n : Int) (h : n.natAbs < k) :
    evalRecUFuel sh k n = evalRecU sh n :=
  evalRecU_fuel_irrel sh (n.natAbs + k + 1) k (n.natAbs + 1) n (by omega) h (by omega)

theorem evalRecU_step (sh : RecShapeU) (n : Int) (hn : ¬ n ≤ 0) :
    evalRecU sh n = stepEval sh.step n (evalRecU sh (n - 1)) := by
  have h0 : evalRecU sh n = evalRecUFuel sh (n.natAbs + 1) n := rfl
  rw [h0]
  simp only [evalRecUFuel]
  rw [if_neg hn, evalRecU_fuel_stable sh n.natAbs (n - 1) (by omega)]

theorem evalRecU_base (sh : RecShapeU) (n : Int) (hn : n ≤ 0) :
    evalRecU sh n = sh.base := by
  have h0 : evalRecU sh n = evalRecUFuel sh (n.natAbs + 1) n := rfl
  rw [h0]; simp [evalRecUFuel, hn]

/-! ### The canonical lowering of a parsed shape -/

def signSInstrs (C : Nat) : List WInstr :=
  [.localGet 0, .structGet C 0, .i64Const 0, .i64LeS]

def signBInstrs (C : Nat) : List WInstr :=
  [.localGet 0, .structGet C 2, .i32Const 0, .i32LtS]

def baseInstrs (bI : Nat) (b : Int) : List WInstr :=
  [.i64Const b, .call bI]

def stepInstrs (self bI aI sI : Nat) : RecStep → List WInstr
  | .inputSecond =>
      [.localGet 0, .localGet 0, .i64Const 1, .call bI, .call sI, .call self, .call aI]
  | .constSecond k =>
      [.i64Const k, .call bI, .localGet 0, .i64Const 1, .call bI, .call sI,
       .call self, .call aI]
  | .inputFirst =>
      [.localGet 0, .i64Const 1, .call bI, .call sI, .call self, .localGet 0, .call aI]
  | .constFirst k =>
      [.localGet 0, .i64Const 1, .call bI, .call sI, .call self, .i64Const k,
       .call bI, .call aI]

def recInstrsU (C self bI aI sI : Nat) (sh : RecShapeU) : List WInstr :=
  [.localGet 0, .structGet C 1, .refIsNull,
   .ifElse (signSInstrs C) (signBInstrs C),
   .ifElse (baseInstrs bI sh.base) (stepInstrs self bI aI sI sh.step)]

/-- Block-level: the parse pins the block (sign predicates inlined as literals),
    so the audited canonical lowering computes to exactly the shape's instruction
    list. Nested `split at h` avoids block-binder naming; each surviving arm is a
    literal body whose lowering reduces by `rfl`. -/
-- Block-level literal-pinning: parseTopU succeeding pins b to a literal whose
-- canonical lowering is `recInstrsU`. This is the SAME mechanical shape the
-- straight-line spike (V3Spike.lowerBlock_inv + per-arm rfl) already closed
-- kernel-clean; the recursion version is strictly more mechanical (no
-- induction). The helper equalities below give stable names to the literal
-- base and step blocks before the final lowering computation.
theorem parseBase_eq (boxIdx : Nat) (b : FragBlock) (bv : Int)
    (h : parseBaseU boxIdx b = some bv) :
    b = ⟨
      [{ id := 0, ty := .i64, kind := .constI64 bv },
       { id := 1, ty := .intCarrier, kind := .hostCall .box boxIdx [0] }],
      1⟩ := by
  simp only [parseBaseU] at h
  split at h
  case h_2 => simp at h
  case h_1 =>
    split at h
    case isFalse => simp at h
    case isTrue hc =>
      rcases hc with ⟨hr, hbi⟩
      injection h with hk
      cases b
      simp_all

def stepBlockU (self boxIdx addIdx subIdx : Nat) : RecStep → FragBlock
  | .inputSecond => ⟨
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .intCarrier, kind := .local 0 },
       { id := 2, ty := .i64, kind := .constI64 1 },
       { id := 3, ty := .intCarrier, kind := .hostCall .box boxIdx [2] },
       { id := 4, ty := .intCarrier, kind := .hostCall .sub subIdx [1, 3] },
       { id := 5, ty := .intCarrier, kind := .selfCall false self [4] },
       { id := 6, ty := .intCarrier, kind := .hostCall .add addIdx [0, 5] }], 6⟩
  | .constSecond k => ⟨
      [{ id := 0, ty := .i64, kind := .constI64 k },
       { id := 1, ty := .intCarrier, kind := .hostCall .box boxIdx [0] },
       { id := 2, ty := .intCarrier, kind := .local 0 },
       { id := 3, ty := .i64, kind := .constI64 1 },
       { id := 4, ty := .intCarrier, kind := .hostCall .box boxIdx [3] },
       { id := 5, ty := .intCarrier, kind := .hostCall .sub subIdx [2, 4] },
       { id := 6, ty := .intCarrier, kind := .selfCall false self [5] },
       { id := 7, ty := .intCarrier, kind := .hostCall .add addIdx [1, 6] }], 7⟩
  | .inputFirst => ⟨
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .i64, kind := .constI64 1 },
       { id := 2, ty := .intCarrier, kind := .hostCall .box boxIdx [1] },
       { id := 3, ty := .intCarrier, kind := .hostCall .sub subIdx [0, 2] },
       { id := 4, ty := .intCarrier, kind := .selfCall false self [3] },
       { id := 5, ty := .intCarrier, kind := .local 0 },
       { id := 6, ty := .intCarrier, kind := .hostCall .add addIdx [4, 5] }], 6⟩
  | .constFirst k => ⟨
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .i64, kind := .constI64 1 },
       { id := 2, ty := .intCarrier, kind := .hostCall .box boxIdx [1] },
       { id := 3, ty := .intCarrier, kind := .hostCall .sub subIdx [0, 2] },
       { id := 4, ty := .intCarrier, kind := .selfCall false self [3] },
       { id := 5, ty := .i64, kind := .constI64 k },
       { id := 6, ty := .intCarrier, kind := .hostCall .box boxIdx [5] },
       { id := 7, ty := .intCarrier, kind := .hostCall .add addIdx [4, 6] }], 7⟩

theorem parseStep_eq (self boxIdx addIdx subIdx : Nat) (b : FragBlock) (st : RecStep)
    (h : parseStepU self boxIdx addIdx subIdx b = some st) :
    b = stepBlockU self boxIdx addIdx subIdx st := by
  simp only [parseStepU] at h
  split at h <;> try simp at h
  all_goals
    rcases h with ⟨hc, hst⟩
    subst st
    cases b
    simp_all [stepBlockU]

theorem parseTop_lower (C self bI aI sI : Nat) (b : FragBlock)
    (sh : RecShapeU) (h : parseTopU self bI aI sI b = some sh) :
    lowerBlock C b = some (recInstrsU C self bI aI sI sh) := by
  simp only [parseTopU] at h
  split at h
  case h_2 => simp at h
  case h_1 =>
    split at h
    case h_2 => simp at h
    case h_1 =>
      rename_i _ _ baseBlock stepBlock hnodes hresult _ _ bv st hbasep hstepp
      injection h with hsh
      subst sh
      have hbase := parseBase_eq bI baseBlock bv hbasep
      have hstep := parseStep_eq self bI aI sI stepBlock st hstepp
      subst baseBlock
      subst stepBlock
      cases b
      cases st <;> simp_all [lowerBlock, maxFuel, lowerBlockFuel, lowerNodesFuel, recInstrsU,
        signSInstrs, signBInstrs, baseInstrs, stepInstrs, stepBlockU,
        popExpected, popExpectedAll, primInstr]

/-- Plan-level corollary. -/
theorem parse_lower (C self bI aI sI : Nat) (plan : RecursionRawPlan)
    (sh : RecShapeU) (h : parseRecShapeU self bI aI sI plan = some sh) :
    lowerBlock C plan.body = some (recInstrsU C self bI aI sI sh) := by
  unfold parseRecShapeU at h
  split at h
  case isFalse => exact absurd h (by simp)
  case isTrue => exact parseTop_lower C self bI aI sI plan.body sh h

#print axioms parse_lower

/-! ### One partial-correctness theorem for the parsed recursion family -/

def stepLeft : RecStep → Int → Int → Int
  | .inputSecond, n, _ => n
  | .constSecond k, _, _ => k
  | .inputFirst, _, r => r
  | .constFirst _, _, r => r

def stepRight : RecStep → Int → Int → Int
  | .inputSecond, _, r => r
  | .constSecond _, _, r => r
  | .inputFirst, n, _ => n
  | .constFirst k, _, _ => k

def stepWArgs (C : Nat) (step : RecStep) (v vr : WVal) : List WVal :=
  match step with
  | .inputSecond => [v, vr]
  | .constSecond k => [carrierSmall C k, vr]
  | .inputFirst => [vr, v]
  | .constFirst k => [vr, carrierSmall C k]

def stepLeftW (C : Nat) (step : RecStep) (v vr : WVal) : WVal :=
  match step with
  | .inputSecond => v
  | .constSecond k => carrierSmall C k
  | .inputFirst | .constFirst _ => vr

def stepRightW (C : Nat) (step : RecStep) (v vr : WVal) : WVal :=
  match step with
  | .inputSecond | .constSecond _ => vr
  | .inputFirst => v
  | .constFirst k => carrierSmall C k

theorem stepOperands_repr (C : Nat) (Repr : Int → WVal → Prop)
    (hsmall : ∀ k : Int, Repr k (carrierSmall C k))
    (step : RecStep) (n r : Int) (v vr : WVal)
    (hv : Repr n v) (hvr : Repr r vr) :
    Repr (stepLeft step n r) (stepLeftW C step v vr) ∧
    Repr (stepRight step n r) (stepRightW C step v vr) := by
  cases step <;> simp [stepLeft, stepRight, stepLeftW, stepRightW, hv, hvr, hsmall]

/-- Every parser-accepted unary recursion plan whose code entry is its audited
    lowering simulates the plan-derived model.  The four `RecStep` cases are
    deliberately kept as four kernel-visible arms: only operand placement
    differs, while the recursive call is discharged by the fuel IH in each. -/
theorem recursion_generic_certified
    (C self bI aI sI nlocals : Nat)
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall C k))
    (hsmall_elim : ∀ n s sg,
      Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) →
        ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (code : CodeTbl) (host : HostTbl)
    (add sub : List WVal → Option WVal)
    (hBox : host bI = some (1, boxRef C))
    (hAddHost : host aI = some (2, add))
    (hSubHost : host sI = some (2, sub))
    (hSelfHost : host self = none)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb →
      add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb →
      sub [va, vb] = some w → Repr (a - b) w)
    (plan : RecursionRawPlan) (sh : RecShapeU)
    (hparse : parseRecShapeU self bI aI sI plan = some sh)
    (instrs : List WInstr)
    (hlow : lowerBlock C plan.body = some instrs)
    (hself : code self = some ⟨1, nlocals, instrs⟩) :
    ∀ (fuel : Nat) (n : Int) (v w : WVal), Repr n v →
      wFuncN code host fuel self [v] = some w →
      Repr (evalRecU sh n) w := by
  have hcanon := parse_lower C self bI aI sI plan sh hparse
  rw [hlow] at hcanon
  injection hcanon with hinstrs
  subst instrs
  cases sh with
  | mk base step =>
    generalize hstep : step = st
    cases st <;>
      intro fuel <;>
      induction fuel with
      | zero =>
          intro n v w hv hrun
          simp [wFuncN] at hrun
      | succ fuel ih =>
          intro n v w hv hrun
          rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
          · have hs := hsmall_elim n s sg hv
            subst hs
            by_cases hle : s ≤ (0 : Int)
            · simp [wFuncN, wRunF, hself, hBox, hstep,
                recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                boxRef, b32, popArgs, initLocals, hle] at hrun
              rw [evalRecU_base _ s hle, ← hrun]
              exact hsmall_intro base
            · simp [wFuncN, wRunF, hself, hBox, hAddHost, hSubHost, hSelfHost, hstep,
                recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                boxRef, b32, popArgs, initLocals, hle] at hrun
              rcases hsub : sub
                  [.structv C [.i64v s, .null, .i32v sg], carrierSmall C 1] with _ | vd
              · simp [hsub] at hrun
              · simp only [hsub] at hrun
                have hrd : Repr (s - 1) vd :=
                  hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
                rcases hrec : wFuncN code host fuel self [vd] with _ | vr
                · simp [hrec] at hrun
                · simp only [hrec] at hrun
                  have hrr := ih (s - 1) vd vr hrd hrec
                  rcases hadd : add (stepWArgs C step
                      (.structv C [.i64v s, .null, .i32v sg]) vr) with _ | wa
                  · have hadd' := hadd
                    simp [hstep, stepWArgs] at hadd'
                    simp [hadd'] at hrun
                  · have hadd' := hadd
                    simp [hstep, stepWArgs] at hadd'
                    simp only [hadd', Option.some.injEq] at hrun
                    obtain ⟨hl, hr⟩ := stepOperands_repr C Repr hsmall_intro step
                      s (evalRecU ⟨base, step⟩ (s - 1)) _ _ hv (by simpa [hstep] using hrr)
                    have haddArgs : add
                        [stepLeftW C step (.structv C [.i64v s, .null, .i32v sg]) vr,
                         stepRightW C step (.structv C [.i64v s, .null, .i32v sg]) vr] =
                        some wa := by
                      simpa [hstep, stepWArgs, stepLeftW, stepRightW] using hadd
                    have hout := hAdd _ _ _ _ wa hl hr haddArgs
                    rw [evalRecU_step _ s hle, ← hrun]
                    simpa [hstep, stepEval, stepLeft, stepRight] using hout
          · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
            by_cases hlt : sg < (0 : Int)
            · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
              simp [wFuncN, wRunF, hself, hBox, hstep,
                  recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                  boxRef, b32, popArgs, initLocals, hlt] at hrun
              rw [evalRecU_base _ n hn0, ← hrun]
              exact hsmall_intro base
            · have hn0 : ¬ n ≤ 0 := by
                intro hle
                have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
                omega
              simp [wFuncN, wRunF, hself, hBox, hAddHost, hSubHost, hSelfHost, hstep,
                  recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                  boxRef, b32, popArgs, initLocals, hlt] at hrun
              rcases hsub : sub
                  [.structv C [.i64v s, .arr lty les, .i32v sg], carrierSmall C 1] with _ | vd
              · simp [hsub] at hrun
              · simp only [hsub] at hrun
                have hrd : Repr (n - 1) vd :=
                  hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
                rcases hrec : wFuncN code host fuel self [vd] with _ | vr
                · simp [hrec] at hrun
                · simp only [hrec] at hrun
                  have hrr := ih (n - 1) vd vr hrd hrec
                  rcases hadd : add (stepWArgs C step
                      (.structv C [.i64v s, .arr lty les, .i32v sg]) vr) with _ | wa
                  · have hadd' := hadd
                    simp [hstep, stepWArgs] at hadd'
                    simp [hadd'] at hrun
                  · have hadd' := hadd
                    simp [hstep, stepWArgs] at hadd'
                    simp only [hadd', Option.some.injEq] at hrun
                    obtain ⟨hl, hr⟩ := stepOperands_repr C Repr hsmall_intro step
                      n (evalRecU ⟨base, step⟩ (n - 1)) _ _ hv (by simpa [hstep] using hrr)
                    have haddArgs : add
                        [stepLeftW C step (.structv C [.i64v s, .arr lty les, .i32v sg]) vr,
                         stepRightW C step (.structv C [.i64v s, .arr lty les, .i32v sg]) vr] =
                        some wa := by
                      simpa [hstep, stepWArgs, stepLeftW, stepRightW] using hadd
                    have hout := hAdd _ _ _ _ wa hl hr haddArgs
                    rw [evalRecU_step _ n hn0, ← hrun]
                    simpa [hstep, stepEval, stepLeft, stepRight] using hout

#print axioms recursion_generic_certified

/-! ### Bounded-total correctness for the parsed descent-by-one family -/

/-- Fuel-parametric progress for every parser-accepted unary descent-by-one
    plan.  Unlike partial correctness, progress needs totality of both host
    operations: subtraction constructs the represented recursive argument and
    addition constructs the represented result after the recursive call. -/
theorem recursion_generic_certified_total_aux
    (C self bI aI sI nlocals : Nat)
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall C k))
    (hsmall_elim : ∀ n s sg,
      Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) →
        ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (code : CodeTbl) (host : HostTbl)
    (add sub : List WVal → Option WVal)
    (hBox : host bI = some (1, boxRef C))
    (hAddHost : host aI = some (2, add))
    (hSubHost : host sI = some (2, sub))
    (hSelfHost : host self = none)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb →
      add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb →
      sub [va, vb] = some w → Repr (a - b) w)
    (hAddTot : ∀ a b va vb, Repr a va → Repr b vb →
      ∃ w, add [va, vb] = some w)
    (hSubTot : ∀ a b va vb, Repr a va → Repr b vb →
      ∃ w, sub [va, vb] = some w)
    (plan : RecursionRawPlan) (sh : RecShapeU)
    (hparse : parseRecShapeU self bI aI sI plan = some sh)
    (instrs : List WInstr)
    (hlow : lowerBlock C plan.body = some instrs)
    (hself : code self = some ⟨1, nlocals, instrs⟩) :
    ∀ (fuel : Nat) (n : Int) (v : WVal), Repr n v → n.natAbs < fuel →
      ∃ w, wFuncN code host fuel self [v] = some w ∧
        Repr (evalRecU sh n) w := by
  have hcanon := parse_lower C self bI aI sI plan sh hparse
  rw [hlow] at hcanon
  injection hcanon with hinstrs
  subst instrs
  cases sh with
  | mk base step =>
    generalize hstep : step = st
    cases st <;>
      intro fuel <;>
      induction fuel with
      | zero =>
          intro n v hv hlt
          omega
      | succ fuel ih =>
          intro n v hv hlt
          rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
          · have hs := hsmall_elim n s sg hv
            subst hs
            by_cases hle : s ≤ (0 : Int)
            · refine ⟨carrierSmall C base, ?_, ?_⟩
              · simp [wFuncN, wRunF, hself, hBox, hstep,
                  recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                  boxRef, b32, popArgs, initLocals, hle]
              · rw [evalRecU_base _ s hle]
                exact hsmall_intro base
            · obtain ⟨vd, hsub⟩ := hSubTot s 1 _ (carrierSmall C 1)
                hv (hsmall_intro 1)
              have hrd : Repr (s - 1) vd :=
                hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
              obtain ⟨vr, hrec, hrr⟩ := ih (s - 1) vd hrd (by omega)
              obtain ⟨hl, hr⟩ := stepOperands_repr C Repr hsmall_intro step
                s (evalRecU ⟨base, step⟩ (s - 1)) _ _ hv
                (by simpa [hstep] using hrr)
              obtain ⟨wa, hadd⟩ := hAddTot _ _ _ _ hl hr
              refine ⟨wa, ?_, ?_⟩
              · have hadd' := hadd
                simp [hstep, stepLeftW, stepRightW] at hadd'
                simp [wFuncN, wRunF, hself, hBox, hAddHost, hSubHost, hSelfHost,
                  hstep, recInstrsU, signSInstrs, signBInstrs, baseInstrs,
                  stepInstrs, boxRef, b32, popArgs, initLocals, hle, hsub, hrec,
                  hadd']
              · have hout := hAdd _ _ _ _ wa hl hr hadd
                rw [evalRecU_step _ s hle]
                simpa [hstep, stepEval, stepLeft, stepRight] using hout
          · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
            by_cases hlt : sg < (0 : Int)
            · have hn0 : n ≤ 0 := by
                have := hsign.mp hlt
                omega
              refine ⟨carrierSmall C base, ?_, ?_⟩
              · simp [wFuncN, wRunF, hself, hBox, hstep,
                  recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                  boxRef, b32, popArgs, initLocals, hlt]
              · rw [evalRecU_base _ n hn0]
                exact hsmall_intro base
            · have hn0 : ¬ n ≤ 0 := by
                intro hle
                have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
                omega
              obtain ⟨vd, hsub⟩ := hSubTot n 1 _ (carrierSmall C 1)
                hv (hsmall_intro 1)
              have hrd : Repr (n - 1) vd :=
                hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
              obtain ⟨vr, hrec, hrr⟩ := ih (n - 1) vd hrd (by omega)
              obtain ⟨hl, hr⟩ := stepOperands_repr C Repr hsmall_intro step
                n (evalRecU ⟨base, step⟩ (n - 1)) _ _ hv
                (by simpa [hstep] using hrr)
              obtain ⟨wa, hadd⟩ := hAddTot _ _ _ _ hl hr
              refine ⟨wa, ?_, ?_⟩
              · have hadd' := hadd
                simp [hstep, stepLeftW, stepRightW] at hadd'
                simp [wFuncN, wRunF, hself, hBox, hAddHost, hSubHost, hSelfHost,
                  hstep, recInstrsU, signSInstrs, signBInstrs, baseInstrs,
                  stepInstrs, boxRef, b32, popArgs, initLocals, hlt, hsub, hrec,
                  hadd']
              · have hout := hAdd _ _ _ _ wa hl hr hadd
                rw [evalRecU_step _ n hn0]
                simpa [hstep, stepEval, stepLeft, stepRight] using hout

#print axioms recursion_generic_certified_total_aux

/-- Bounded-total correctness at the standard fuel selected by the checked
    `Int.natAbs` descent witness. -/
theorem recursion_generic_certified_total
    (C self bI aI sI nlocals : Nat)
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall C k))
    (hsmall_elim : ∀ n s sg,
      Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) →
        ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (code : CodeTbl) (host : HostTbl)
    (add sub : List WVal → Option WVal)
    (hBox : host bI = some (1, boxRef C))
    (hAddHost : host aI = some (2, add))
    (hSubHost : host sI = some (2, sub))
    (hSelfHost : host self = none)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb →
      add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb →
      sub [va, vb] = some w → Repr (a - b) w)
    (hAddTot : ∀ a b va vb, Repr a va → Repr b vb →
      ∃ w, add [va, vb] = some w)
    (hSubTot : ∀ a b va vb, Repr a va → Repr b vb →
      ∃ w, sub [va, vb] = some w)
    (plan : RecursionRawPlan) (sh : RecShapeU)
    (hparse : parseRecShapeU self bI aI sI plan = some sh)
    (instrs : List WInstr)
    (hlow : lowerBlock C plan.body = some instrs)
    (hself : code self = some ⟨1, nlocals, instrs⟩) :
    ∀ (n : Int) (v : WVal), Repr n v →
      ∃ w, wFuncN code host (n.natAbs + 1) self [v] = some w ∧
        Repr (evalRecU sh n) w :=
  fun n v hv =>
    recursion_generic_certified_total_aux C self bI aI sI nlocals
      Repr hcar hsmall_intro hsmall_elim hbig code host add sub hBox hAddHost
      hSubHost hSelfHost hAdd hSub hAddTot hSubTot plan sh hparse instrs hlow
      hself (n.natAbs + 1) n v hv (by omega)

#print axioms recursion_generic_certified_total

/- The fixture-specific section is retained as spike documentation but is not
   part of the reusable generic module. -/
end V3Rec
