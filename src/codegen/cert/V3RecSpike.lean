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

/-- The semantic host operation performed after the recursive call.  Wasm
    bytes identify the exact helper index, while the option-(b) bridge binds
    that index to the corresponding independently quantified host contract. -/
inductive RecCombine where
  | add
  | mul
deriving Repr, DecidableEq

def combineHostRole : RecCombine → HostRole
  | .add => .add
  | .mul => .mul

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

def parseStepU (combine : RecCombine) (self boxIdx combineIdx subIdx : Nat)
    (b : FragBlock) : Option RecStep :=
  match b.nodes with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .intCarrier, kind := .local 0 },
     { id := 2, ty := .i64, kind := .constI64 one },
     { id := 3, ty := .intCarrier, kind := .hostCall .box bi [2] },
     { id := 4, ty := .intCarrier, kind := .hostCall .sub si [1, 3] },
     { id := 5, ty := .intCarrier, kind := .selfCall false sc [4] },
     { id := 6, ty := .intCarrier, kind := .hostCall role ci [0, 5] }] =>
      if b.result = 6 ∧ one = 1 ∧ bi = boxIdx ∧ si = subIdx ∧ sc = self ∧
          role = combineHostRole combine ∧ ci = combineIdx then
        some .inputSecond
      else none
  | [{ id := 0, ty := .i64, kind := .constI64 k },
     { id := 1, ty := .intCarrier, kind := .hostCall .box bk [0] },
     { id := 2, ty := .intCarrier, kind := .local 0 },
     { id := 3, ty := .i64, kind := .constI64 one },
     { id := 4, ty := .intCarrier, kind := .hostCall .box bi [3] },
     { id := 5, ty := .intCarrier, kind := .hostCall .sub si [2, 4] },
     { id := 6, ty := .intCarrier, kind := .selfCall false sc [5] },
     { id := 7, ty := .intCarrier, kind := .hostCall role ci [1, 6] }] =>
      if b.result = 7 ∧ one = 1 ∧ bk = boxIdx ∧ bi = boxIdx ∧ si = subIdx ∧ sc = self ∧
          role = combineHostRole combine ∧ ci = combineIdx then
        some (.constSecond k)
      else none
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .constI64 one },
     { id := 2, ty := .intCarrier, kind := .hostCall .box bi [1] },
     { id := 3, ty := .intCarrier, kind := .hostCall .sub si [0, 2] },
     { id := 4, ty := .intCarrier, kind := .selfCall false sc [3] },
     { id := 5, ty := .intCarrier, kind := .local 0 },
     { id := 6, ty := .intCarrier, kind := .hostCall role ci [4, 5] }] =>
      if b.result = 6 ∧ one = 1 ∧ bi = boxIdx ∧ si = subIdx ∧ sc = self ∧
          role = combineHostRole combine ∧ ci = combineIdx then
        some .inputFirst
      else none
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .constI64 one },
     { id := 2, ty := .intCarrier, kind := .hostCall .box bi [1] },
     { id := 3, ty := .intCarrier, kind := .hostCall .sub si [0, 2] },
     { id := 4, ty := .intCarrier, kind := .selfCall false sc [3] },
     { id := 5, ty := .i64, kind := .constI64 k },
     { id := 6, ty := .intCarrier, kind := .hostCall .box bk [5] },
     { id := 7, ty := .intCarrier, kind := .hostCall role ci [4, 6] }] =>
      if b.result = 7 ∧ one = 1 ∧ bi = boxIdx ∧ bk = boxIdx ∧ si = subIdx ∧ sc = self ∧
          role = combineHostRole combine ∧ ci = combineIdx then
        some (.constFirst k)
      else none
  | _ => none

def parseTopU (combine : RecCombine) (self boxIdx combineIdx subIdx : Nat)
    (b : FragBlock) : Option RecShapeU :=
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
      match parseBaseU boxIdx base,
          parseStepU combine self boxIdx combineIdx subIdx step with
      | some bv, some st => some ⟨bv, st⟩
      | _, _ => none
  | _, _ => none

def parseRecShapeU (combine : RecCombine) (self boxIdx combineIdx subIdx : Nat)
    (plan : RecursionRawPlan) : Option RecShapeU :=
  if plan.profile = "recursion-plan-v1" ∧ plan.params = [FragTy.intCarrier] ∧
      AverCert.PlanCheck.sameTy plan.result .intCarrier then
    parseTopU combine self boxIdx combineIdx subIdx plan.body
  else none

/-! ### The generic plan-derived model (fuel twin + natAbs face, once) -/

def combineEval : RecCombine → Int → Int → Int
  | .add, a, b => a + b
  | .mul, a, b => a * b

def stepEval (combine : RecCombine) : RecStep → Int → Int → Int
  | .inputSecond, n, r => combineEval combine n r
  | .constSecond k, _, r => combineEval combine k r
  | .inputFirst, n, r => combineEval combine r n
  | .constFirst k, _, r => combineEval combine r k

def evalRecUFuel (combine : RecCombine) (sh : RecShapeU) : Nat → Int → Int
  | 0, _ => 0
  | fuel + 1, n =>
      if n ≤ 0 then sh.base
      else stepEval combine sh.step n (evalRecUFuel combine sh fuel (n - 1))

def evalRecU (combine : RecCombine) (sh : RecShapeU) (n : Int) : Int :=
  evalRecUFuel combine sh (n.natAbs + 1) n

theorem evalRecU_fuel_irrel (combine : RecCombine) (sh : RecShapeU) :
    ∀ (t k1 k2 : Nat) (n : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      evalRecUFuel combine sh k1 n = evalRecUFuel combine sh k2 n := by
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

theorem evalRecU_fuel_stable (combine : RecCombine) (sh : RecShapeU)
    (k : Nat) (n : Int) (h : n.natAbs < k) :
    evalRecUFuel combine sh k n = evalRecU combine sh n :=
  evalRecU_fuel_irrel combine sh (n.natAbs + k + 1) k (n.natAbs + 1) n
    (by omega) h (by omega)

theorem evalRecU_step (combine : RecCombine) (sh : RecShapeU)
    (n : Int) (hn : ¬ n ≤ 0) :
    evalRecU combine sh n =
      stepEval combine sh.step n (evalRecU combine sh (n - 1)) := by
  have h0 : evalRecU combine sh n =
      evalRecUFuel combine sh (n.natAbs + 1) n := rfl
  rw [h0]
  simp only [evalRecUFuel]
  rw [if_neg hn, evalRecU_fuel_stable combine sh n.natAbs (n - 1) (by omega)]

theorem evalRecU_base (combine : RecCombine) (sh : RecShapeU)
    (n : Int) (hn : n ≤ 0) :
    evalRecU combine sh n = sh.base := by
  have h0 : evalRecU combine sh n =
      evalRecUFuel combine sh (n.natAbs + 1) n := rfl
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

def stepBlockU (combine : RecCombine) (self boxIdx combineIdx subIdx : Nat) : RecStep → FragBlock
  | .inputSecond => ⟨
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .intCarrier, kind := .local 0 },
       { id := 2, ty := .i64, kind := .constI64 1 },
       { id := 3, ty := .intCarrier, kind := .hostCall .box boxIdx [2] },
       { id := 4, ty := .intCarrier, kind := .hostCall .sub subIdx [1, 3] },
       { id := 5, ty := .intCarrier, kind := .selfCall false self [4] },
       { id := 6, ty := .intCarrier,
         kind := .hostCall (combineHostRole combine) combineIdx [0, 5] }], 6⟩
  | .constSecond k => ⟨
      [{ id := 0, ty := .i64, kind := .constI64 k },
       { id := 1, ty := .intCarrier, kind := .hostCall .box boxIdx [0] },
       { id := 2, ty := .intCarrier, kind := .local 0 },
       { id := 3, ty := .i64, kind := .constI64 1 },
       { id := 4, ty := .intCarrier, kind := .hostCall .box boxIdx [3] },
       { id := 5, ty := .intCarrier, kind := .hostCall .sub subIdx [2, 4] },
       { id := 6, ty := .intCarrier, kind := .selfCall false self [5] },
       { id := 7, ty := .intCarrier,
         kind := .hostCall (combineHostRole combine) combineIdx [1, 6] }], 7⟩
  | .inputFirst => ⟨
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .i64, kind := .constI64 1 },
       { id := 2, ty := .intCarrier, kind := .hostCall .box boxIdx [1] },
       { id := 3, ty := .intCarrier, kind := .hostCall .sub subIdx [0, 2] },
       { id := 4, ty := .intCarrier, kind := .selfCall false self [3] },
       { id := 5, ty := .intCarrier, kind := .local 0 },
       { id := 6, ty := .intCarrier,
         kind := .hostCall (combineHostRole combine) combineIdx [4, 5] }], 6⟩
  | .constFirst k => ⟨
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .i64, kind := .constI64 1 },
       { id := 2, ty := .intCarrier, kind := .hostCall .box boxIdx [1] },
       { id := 3, ty := .intCarrier, kind := .hostCall .sub subIdx [0, 2] },
       { id := 4, ty := .intCarrier, kind := .selfCall false self [3] },
       { id := 5, ty := .i64, kind := .constI64 k },
       { id := 6, ty := .intCarrier, kind := .hostCall .box boxIdx [5] },
       { id := 7, ty := .intCarrier,
         kind := .hostCall (combineHostRole combine) combineIdx [4, 6] }], 7⟩

theorem parseStep_eq (combine : RecCombine) (self boxIdx combineIdx subIdx : Nat)
    (b : FragBlock) (st : RecStep)
    (h : parseStepU combine self boxIdx combineIdx subIdx b = some st) :
    b = stepBlockU combine self boxIdx combineIdx subIdx st := by
  simp only [parseStepU] at h
  split at h <;> try simp at h
  all_goals
    rcases h with ⟨hc, hst⟩
    subst st
    cases b
    simp_all [stepBlockU]

theorem parseTop_lower (C : Nat) (combine : RecCombine) (self bI aI sI : Nat)
    (b : FragBlock) (sh : RecShapeU)
    (h : parseTopU combine self bI aI sI b = some sh) :
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
      have hstep := parseStep_eq combine self bI aI sI stepBlock st hstepp
      subst baseBlock
      subst stepBlock
      cases b
      cases st <;> simp_all [lowerBlock, maxFuel, lowerBlockFuel, lowerNodesFuel, recInstrsU,
        signSInstrs, signBInstrs, baseInstrs, stepInstrs, stepBlockU, combineHostRole,
        popExpected, popExpectedAll, primInstr]

/-- Plan-level corollary. -/
theorem parse_lower (C : Nat) (combine : RecCombine) (self bI aI sI : Nat)
    (plan : RecursionRawPlan)
    (sh : RecShapeU)
    (h : parseRecShapeU combine self bI aI sI plan = some sh) :
    lowerBlock C plan.body = some (recInstrsU C self bI aI sI sh) := by
  unfold parseRecShapeU at h
  split at h
  case isFalse => exact absurd h (by simp)
  case isTrue => exact parseTop_lower C combine self bI aI sI plan.body sh h


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

theorem stepOperands_repr (C : Nat) (S : CarrierSpec C)
    (step : RecStep) (n r : Int) (v vr : WVal)
    (hv : S.Repr n v) (hvr : S.Repr r vr) :
    S.Repr (stepLeft step n r) (stepLeftW C step v vr) ∧
    S.Repr (stepRight step n r) (stepRightW C step v vr) := by
  cases step <;>
    simp [stepLeft, stepRight, stepLeftW, stepRightW, hv, hvr, S.smallIntro]

/-- Every parser-accepted unary recursion plan whose code entry is its audited
    lowering simulates the plan-derived model.  The four `RecStep` cases are
    deliberately kept as four kernel-visible arms: only operand placement
    differs, while the recursive call is discharged by the fuel IH in each. -/
theorem recursion_generic_certified
    (C : Nat) (combineOp : RecCombine)
    (self bI cI sI nlocals : Nat)
    (S : CarrierSpec C)
    (code : CodeTbl) (host : HostTbl)
    (combine sub : List WVal → Option WVal)
    (hBox : host bI = some (1, boxRef C))
    (hCombineHost : host cI = some (2, combine))
    (hSubHost : host sI = some (2, sub))
    (hSelfHost : host self = none)
    (hCombine : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      combine [va, vb] = some w → S.Repr (combineEval combineOp a b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (plan : RecursionRawPlan) (sh : RecShapeU)
    (hparse : parseRecShapeU combineOp self bI cI sI plan = some sh)
    (instrs : List WInstr)
    (hlow : lowerBlock C plan.body = some instrs)
    (hself : code self = some ⟨1, nlocals, instrs⟩) :
    ∀ (fuel : Nat) (n : Int) (v w : WVal), S.Repr n v →
      wFuncN code host fuel self [v] = some w →
      S.Repr (evalRecU combineOp sh n) w := by
  have hcanon := parse_lower C combineOp self bI cI sI plan sh hparse
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
          rcases S.car n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
          · have hs := S.smallElim n s sg hv
            subst hs
            by_cases hle : s ≤ (0 : Int)
            · simp [wFuncN, wRunF, hself, hBox, hstep,
                recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                boxRef, b32, popArgs, initLocals, hle] at hrun
              rw [evalRecU_base combineOp _ s hle, ← hrun]
              exact S.smallIntro base
            · simp [wFuncN, wRunF, hself, hBox, hCombineHost, hSubHost, hSelfHost, hstep,
                recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                boxRef, b32, popArgs, initLocals, hle] at hrun
              rcases hsub : sub
                  [.structv C [.i64v s, .null, .i32v sg], carrierSmall C 1] with _ | vd
              · simp [hsub] at hrun
              · simp only [hsub] at hrun
                have hrd : S.Repr (s - 1) vd :=
                  hSub s 1 _ _ vd hv (S.smallIntro 1) hsub
                rcases hrec : wFuncN code host fuel self [vd] with _ | vr
                · simp [hrec] at hrun
                · simp only [hrec] at hrun
                  have hrr := ih (s - 1) vd vr hrd hrec
                  rcases hadd : combine (stepWArgs C step
                      (.structv C [.i64v s, .null, .i32v sg]) vr) with _ | wa
                  · have hadd' := hadd
                    simp [hstep, stepWArgs] at hadd'
                    simp [hadd'] at hrun
                  · have hadd' := hadd
                    simp [hstep, stepWArgs] at hadd'
                    simp only [hadd', Option.some.injEq] at hrun
                    obtain ⟨hl, hr⟩ := stepOperands_repr C S step
                      s (evalRecU combineOp ⟨base, step⟩ (s - 1)) _ _ hv
                      (by simpa [hstep] using hrr)
                    have haddArgs : combine
                        [stepLeftW C step (.structv C [.i64v s, .null, .i32v sg]) vr,
                         stepRightW C step (.structv C [.i64v s, .null, .i32v sg]) vr] =
                        some wa := by
                      simpa [hstep, stepWArgs, stepLeftW, stepRightW] using hadd
                    have hout := hCombine _ _ _ _ wa hl hr haddArgs
                    rw [evalRecU_step combineOp _ s hle, ← hrun]
                    simpa [hstep, stepEval, stepLeft, stepRight, combineEval] using hout
          · obtain ⟨hsign, hne⟩ := S.bigElim n s lty les sg hv
            by_cases hlt : sg < (0 : Int)
            · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
              simp [wFuncN, wRunF, hself, hBox, hstep,
                  recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                  boxRef, b32, popArgs, initLocals, hlt] at hrun
              rw [evalRecU_base combineOp _ n hn0, ← hrun]
              exact S.smallIntro base
            · have hn0 : ¬ n ≤ 0 := by
                intro hle
                have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
                omega
              simp [wFuncN, wRunF, hself, hBox, hCombineHost, hSubHost, hSelfHost, hstep,
                  recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                  boxRef, b32, popArgs, initLocals, hlt] at hrun
              rcases hsub : sub
                  [.structv C [.i64v s, .arr lty les, .i32v sg], carrierSmall C 1] with _ | vd
              · simp [hsub] at hrun
              · simp only [hsub] at hrun
                have hrd : S.Repr (n - 1) vd :=
                  hSub n 1 _ _ vd hv (S.smallIntro 1) hsub
                rcases hrec : wFuncN code host fuel self [vd] with _ | vr
                · simp [hrec] at hrun
                · simp only [hrec] at hrun
                  have hrr := ih (n - 1) vd vr hrd hrec
                  rcases hadd : combine (stepWArgs C step
                      (.structv C [.i64v s, .arr lty les, .i32v sg]) vr) with _ | wa
                  · have hadd' := hadd
                    simp [hstep, stepWArgs] at hadd'
                    simp [hadd'] at hrun
                  · have hadd' := hadd
                    simp [hstep, stepWArgs] at hadd'
                    simp only [hadd', Option.some.injEq] at hrun
                    obtain ⟨hl, hr⟩ := stepOperands_repr C S step
                      n (evalRecU combineOp ⟨base, step⟩ (n - 1)) _ _ hv
                      (by simpa [hstep] using hrr)
                    have haddArgs : combine
                        [stepLeftW C step (.structv C [.i64v s, .arr lty les, .i32v sg]) vr,
                         stepRightW C step (.structv C [.i64v s, .arr lty les, .i32v sg]) vr] =
                        some wa := by
                      simpa [hstep, stepWArgs, stepLeftW, stepRightW] using hadd
                    have hout := hCombine _ _ _ _ wa hl hr haddArgs
                    rw [evalRecU_step combineOp _ n hn0, ← hrun]
                    simpa [hstep, stepEval, stepLeft, stepRight, combineEval] using hout


/-! ### Bounded-total correctness for the parsed descent-by-one family -/

/-- Fuel-parametric progress for every parser-accepted unary descent-by-one
    plan.  Unlike partial correctness, progress needs totality of both host
    operations: subtraction constructs the represented recursive argument and
    addition constructs the represented result after the recursive call. -/
theorem recursion_generic_certified_total_aux
    (C : Nat) (combineOp : RecCombine)
    (self bI cI sI nlocals : Nat)
    (S : CarrierSpec C)
    (code : CodeTbl) (host : HostTbl)
    (combine sub : List WVal → Option WVal)
    (hBox : host bI = some (1, boxRef C))
    (hCombineHost : host cI = some (2, combine))
    (hSubHost : host sI = some (2, sub))
    (hSelfHost : host self = none)
    (hCombine : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      combine [va, vb] = some w → S.Repr (combineEval combineOp a b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (hCombineTot : ∀ a b va vb, S.Repr a va → S.Repr b vb →
      ∃ w, combine [va, vb] = some w)
    (hSubTot : ∀ a b va vb, S.Repr a va → S.Repr b vb →
      ∃ w, sub [va, vb] = some w)
    (plan : RecursionRawPlan) (sh : RecShapeU)
    (hparse : parseRecShapeU combineOp self bI cI sI plan = some sh)
    (instrs : List WInstr)
    (hlow : lowerBlock C plan.body = some instrs)
    (hself : code self = some ⟨1, nlocals, instrs⟩) :
    ∀ (fuel : Nat) (n : Int) (v : WVal), S.Repr n v → n.natAbs < fuel →
      ∃ w, wFuncN code host fuel self [v] = some w ∧
        S.Repr (evalRecU combineOp sh n) w := by
  have hcanon := parse_lower C combineOp self bI cI sI plan sh hparse
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
          rcases S.car n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
          · have hs := S.smallElim n s sg hv
            subst hs
            by_cases hle : s ≤ (0 : Int)
            · refine ⟨carrierSmall C base, ?_, ?_⟩
              · simp [wFuncN, wRunF, hself, hBox, hstep,
                  recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                  boxRef, b32, popArgs, initLocals, hle]
              · rw [evalRecU_base combineOp _ s hle]
                exact S.smallIntro base
            · obtain ⟨vd, hsub⟩ := hSubTot s 1 _ (carrierSmall C 1)
                hv (S.smallIntro 1)
              have hrd : S.Repr (s - 1) vd :=
                hSub s 1 _ _ vd hv (S.smallIntro 1) hsub
              obtain ⟨vr, hrec, hrr⟩ := ih (s - 1) vd hrd (by omega)
              obtain ⟨hl, hr⟩ := stepOperands_repr C S step
                s (evalRecU combineOp ⟨base, step⟩ (s - 1)) _ _ hv
                (by simpa [hstep] using hrr)
              obtain ⟨wa, hadd⟩ := hCombineTot _ _ _ _ hl hr
              refine ⟨wa, ?_, ?_⟩
              · have hadd' := hadd
                simp [hstep, stepLeftW, stepRightW] at hadd'
                simp [wFuncN, wRunF, hself, hBox, hCombineHost, hSubHost, hSelfHost,
                  hstep, recInstrsU, signSInstrs, signBInstrs, baseInstrs,
                  stepInstrs, boxRef, b32, popArgs, initLocals, hle, hsub, hrec,
                  hadd']
              · have hout := hCombine _ _ _ _ wa hl hr hadd
                rw [evalRecU_step combineOp _ s hle]
                simpa [hstep, stepEval, stepLeft, stepRight, combineEval] using hout
          · obtain ⟨hsign, hne⟩ := S.bigElim n s lty les sg hv
            by_cases hlt : sg < (0 : Int)
            · have hn0 : n ≤ 0 := by
                have := hsign.mp hlt
                omega
              refine ⟨carrierSmall C base, ?_, ?_⟩
              · simp [wFuncN, wRunF, hself, hBox, hstep,
                  recInstrsU, signSInstrs, signBInstrs, baseInstrs, stepInstrs,
                  boxRef, b32, popArgs, initLocals, hlt]
              · rw [evalRecU_base combineOp _ n hn0]
                exact S.smallIntro base
            · have hn0 : ¬ n ≤ 0 := by
                intro hle
                have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
                omega
              obtain ⟨vd, hsub⟩ := hSubTot n 1 _ (carrierSmall C 1)
                hv (S.smallIntro 1)
              have hrd : S.Repr (n - 1) vd :=
                hSub n 1 _ _ vd hv (S.smallIntro 1) hsub
              obtain ⟨vr, hrec, hrr⟩ := ih (n - 1) vd hrd (by omega)
              obtain ⟨hl, hr⟩ := stepOperands_repr C S step
                n (evalRecU combineOp ⟨base, step⟩ (n - 1)) _ _ hv
                (by simpa [hstep] using hrr)
              obtain ⟨wa, hadd⟩ := hCombineTot _ _ _ _ hl hr
              refine ⟨wa, ?_, ?_⟩
              · have hadd' := hadd
                simp [hstep, stepLeftW, stepRightW] at hadd'
                simp [wFuncN, wRunF, hself, hBox, hCombineHost, hSubHost, hSelfHost,
                  hstep, recInstrsU, signSInstrs, signBInstrs, baseInstrs,
                  stepInstrs, boxRef, b32, popArgs, initLocals, hlt, hsub, hrec,
                  hadd']
              · have hout := hCombine _ _ _ _ wa hl hr hadd
                rw [evalRecU_step combineOp _ n hn0]
                simpa [hstep, stepEval, stepLeft, stepRight, combineEval] using hout


/-- Bounded-total correctness at the standard fuel selected by the checked
    `Int.natAbs` descent witness. -/
theorem recursion_generic_certified_total
    (C : Nat) (combineOp : RecCombine)
    (self bI cI sI nlocals : Nat)
    (S : CarrierSpec C)
    (code : CodeTbl) (host : HostTbl)
    (combine sub : List WVal → Option WVal)
    (hBox : host bI = some (1, boxRef C))
    (hCombineHost : host cI = some (2, combine))
    (hSubHost : host sI = some (2, sub))
    (hSelfHost : host self = none)
    (hCombine : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      combine [va, vb] = some w → S.Repr (combineEval combineOp a b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (hCombineTot : ∀ a b va vb, S.Repr a va → S.Repr b vb →
      ∃ w, combine [va, vb] = some w)
    (hSubTot : ∀ a b va vb, S.Repr a va → S.Repr b vb →
      ∃ w, sub [va, vb] = some w)
    (plan : RecursionRawPlan) (sh : RecShapeU)
    (hparse : parseRecShapeU combineOp self bI cI sI plan = some sh)
    (instrs : List WInstr)
    (hlow : lowerBlock C plan.body = some instrs)
    (hself : code self = some ⟨1, nlocals, instrs⟩) :
    ∀ (n : Int) (v : WVal), S.Repr n v →
      ∃ w, wFuncN code host (n.natAbs + 1) self [v] = some w ∧
        S.Repr (evalRecU combineOp sh n) w :=
  fun n v hv =>
    recursion_generic_certified_total_aux C combineOp self bI cI sI nlocals
      S code host combine sub hBox hCombineHost hSubHost hSelfHost hCombine hSub
      hCombineTot hSubTot plan sh hparse instrs hlow hself
      (n.natAbs + 1) n v hv (by omega)


/-! ### Two-argument accumulator recursion -/

/-- The separately parsed arity-two family
    `f n acc = if n ≤ 0 then acc else f (n-1) (acc+n)`. -/
inductive RecShapeA where
  | accumulator
deriving Repr, DecidableEq

def parseBaseA (b : FragBlock) : Option Unit :=
  match b.nodes with
  | [{ id := 0, ty := .intCarrier, kind := .local 1 }] =>
      if b.result = 0 then some () else none
  | _ => none

def parseStepA (self boxIdx addIdx subIdx : Nat) (b : FragBlock) : Option Unit :=
  match b.nodes with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .constI64 one },
     { id := 2, ty := .intCarrier, kind := .hostCall .box bi [1] },
     { id := 3, ty := .intCarrier, kind := .hostCall .sub si [0, 2] },
     { id := 4, ty := .intCarrier, kind := .local 1 },
     { id := 5, ty := .intCarrier, kind := .local 0 },
     { id := 6, ty := .intCarrier, kind := .hostCall .add ai [4, 5] },
     { id := 7, ty := .intCarrier, kind := .selfCall true sc [3, 6] }] =>
      if b.result = 7 ∧ one = 1 ∧ bi = boxIdx ∧ si = subIdx ∧
          ai = addIdx ∧ sc = self then some () else none
  | _ => none

def parseTopA (self boxIdx addIdx subIdx : Nat)
    (b : FragBlock) : Option RecShapeA :=
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
      match parseBaseA base, parseStepA self boxIdx addIdx subIdx step with
      | some (), some () => some .accumulator
      | _, _ => none
  | _, _ => none

def parseRecShapeA (self boxIdx addIdx subIdx : Nat)
    (plan : RecursionRawPlan) : Option RecShapeA :=
  if plan.profile = "recursion-plan-v1" ∧
      plan.params = [FragTy.intCarrier, FragTy.intCarrier] ∧
      AverCert.PlanCheck.sameTy plan.result .intCarrier then
    parseTopA self boxIdx addIdx subIdx plan.body
  else none

def evalRecAFuel : Nat → Int → Int → Int
  | 0, _, _ => 0
  | fuel + 1, n, acc =>
      if n ≤ 0 then acc else evalRecAFuel fuel (n - 1) (acc + n)

def evalRecA (n acc : Int) : Int :=
  evalRecAFuel (n.natAbs + 1) n acc

theorem evalRecA_fuel_irrel :
    ∀ (t k1 k2 : Nat) (n acc : Int),
      n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      evalRecAFuel k1 n acc = evalRecAFuel k2 n acc := by
  intro t
  induction t with
  | zero => intro k1 k2 n acc ht _ _; omega
  | succ t ih =>
      intro k1 k2 n acc ht h1 h2
      cases k1 with
      | zero => omega
      | succ m1 =>
      cases k2 with
      | zero => omega
      | succ m2 =>
      by_cases hn : n ≤ 0
      · simp [evalRecAFuel, hn]
      · have hrec := ih m1 m2 (n - 1) (acc + n) (by omega) (by omega) (by omega)
        simp only [evalRecAFuel]
        rw [if_neg hn, if_neg hn, hrec]

theorem evalRecA_fuel_stable (k : Nat) (n acc : Int) (h : n.natAbs < k) :
    evalRecAFuel k n acc = evalRecA n acc :=
  evalRecA_fuel_irrel (n.natAbs + k + 1) k (n.natAbs + 1) n acc
    (by omega) h (by omega)

theorem evalRecA_step (n acc : Int) (hn : ¬ n ≤ 0) :
    evalRecA n acc = evalRecA (n - 1) (acc + n) := by
  have h0 : evalRecA n acc = evalRecAFuel (n.natAbs + 1) n acc := rfl
  rw [h0]
  simp only [evalRecAFuel]
  rw [if_neg hn, evalRecA_fuel_stable n.natAbs (n - 1) (acc + n) (by omega)]

theorem evalRecA_base (n acc : Int) (hn : n ≤ 0) :
    evalRecA n acc = acc := by
  have h0 : evalRecA n acc = evalRecAFuel (n.natAbs + 1) n acc := rfl
  rw [h0]
  simp [evalRecAFuel, hn]

def baseInstrsA : List WInstr := [.localGet 1]

def stepInstrsA (self bI aI sI : Nat) : List WInstr :=
  [.localGet 0, .i64Const 1, .call bI, .call sI,
   .localGet 1, .localGet 0, .call aI, .returnCall self]

def recInstrsA (C self bI aI sI : Nat) : List WInstr :=
  [.localGet 0, .structGet C 1, .refIsNull,
   .ifElse (signSInstrs C) (signBInstrs C),
   .ifElse baseInstrsA (stepInstrsA self bI aI sI)]

theorem parseBaseA_eq (b : FragBlock) (h : parseBaseA b = some ()) :
    b = ⟨[{ id := 0, ty := .intCarrier, kind := .local 1 }], 0⟩ := by
  simp only [parseBaseA] at h
  split at h
  case h_2 => simp at h
  case h_1 =>
    split at h
    case isFalse => simp at h
    case isTrue hr =>
      cases b
      simp_all

def stepBlockA (self boxIdx addIdx subIdx : Nat) : FragBlock := ⟨
  [{ id := 0, ty := .intCarrier, kind := .local 0 },
   { id := 1, ty := .i64, kind := .constI64 1 },
   { id := 2, ty := .intCarrier, kind := .hostCall .box boxIdx [1] },
   { id := 3, ty := .intCarrier, kind := .hostCall .sub subIdx [0, 2] },
   { id := 4, ty := .intCarrier, kind := .local 1 },
   { id := 5, ty := .intCarrier, kind := .local 0 },
   { id := 6, ty := .intCarrier, kind := .hostCall .add addIdx [4, 5] },
   { id := 7, ty := .intCarrier, kind := .selfCall true self [3, 6] }], 7⟩

theorem parseStepA_eq (self boxIdx addIdx subIdx : Nat) (b : FragBlock)
    (h : parseStepA self boxIdx addIdx subIdx b = some ()) :
    b = stepBlockA self boxIdx addIdx subIdx := by
  simp only [parseStepA] at h
  split at h
  case h_2 => simp at h
  case h_1 =>
    split at h
    case isFalse => simp at h
    case isTrue hc =>
      cases b
      simp_all [stepBlockA]

theorem parseTopA_lower (C self bI aI sI : Nat) (b : FragBlock)
    (sh : RecShapeA) (h : parseTopA self bI aI sI b = some sh) :
    lowerBlock C b = some (recInstrsA C self bI aI sI) := by
  simp only [parseTopA] at h
  split at h
  case h_2 => simp at h
  case h_1 =>
    split at h
    case h_2 => simp at h
    case h_1 =>
      rename_i _ _ baseBlock stepBlock hnodes hresult _ _ hbase hstep
      injection h with hsh
      subst sh
      have hbaseEq := parseBaseA_eq baseBlock hbase
      have hstepEq := parseStepA_eq self bI aI sI stepBlock hstep
      subst baseBlock
      subst stepBlock
      cases b
      simp_all [lowerBlock, maxFuel, lowerBlockFuel, lowerNodesFuel, recInstrsA,
        signSInstrs, signBInstrs, baseInstrsA, stepInstrsA, stepBlockA,
        popExpected, popExpectedAll, primInstr]

theorem parseA_lower (C self bI aI sI : Nat) (plan : RecursionRawPlan)
    (sh : RecShapeA) (h : parseRecShapeA self bI aI sI plan = some sh) :
    lowerBlock C plan.body = some (recInstrsA C self bI aI sI) := by
  unfold parseRecShapeA at h
  split at h
  case isFalse => exact absurd h (by simp)
  case isTrue => exact parseTopA_lower C self bI aI sI plan.body sh h


/-- Partial correctness for the parser-accepted two-argument accumulator
    family. The fuel induction is quantified over both the counter and the
    threaded accumulator; the recursive IH is instantiated at
    `(n - 1, acc + n)`. -/
theorem recursion_accumulator_generic_certified
    (C self bI aI sI nlocals : Nat)
    (S : CarrierSpec C)
    (code : CodeTbl) (host : HostTbl)
    (add sub : List WVal → Option WVal)
    (hBox : host bI = some (1, boxRef C))
    (hAddHost : host aI = some (2, add))
    (hSubHost : host sI = some (2, sub))
    (hSelfHost : host self = none)
    (hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (plan : RecursionRawPlan) (sh : RecShapeA)
    (hparse : parseRecShapeA self bI aI sI plan = some sh)
    (instrs : List WInstr)
    (hlow : lowerBlock C plan.body = some instrs)
    (hself : code self = some ⟨2, nlocals, instrs⟩) :
    ∀ (fuel : Nat) (n acc : Int) (vn vacc w : WVal),
      S.Repr n vn → S.Repr acc vacc →
      wFuncN code host fuel self [vn, vacc] = some w →
      S.Repr (evalRecA n acc) w := by
  have hcanon := parseA_lower C self bI aI sI plan sh hparse
  rw [hlow] at hcanon
  injection hcanon with hinstrs
  subst instrs
  cases sh
  intro fuel
  induction fuel with
  | zero =>
      intro n acc vn vacc w hvn hvacc hrun
      simp [wFuncN] at hrun
  | succ fuel ih =>
      intro n acc vn vacc w hvn hvacc hrun
      rcases S.car n vn hvn with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := S.smallElim n s sg hvn
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · simp [wFuncN, wRunF, hself, hBox, recInstrsA, signSInstrs,
            signBInstrs, baseInstrsA, stepInstrsA, boxRef, b32, popArgs,
            initLocals, hle] at hrun
          rw [evalRecA_base s acc hle, ← hrun]
          exact hvacc
        · simp [wFuncN, wRunF, hself, hBox, hAddHost, hSubHost, hSelfHost,
            recInstrsA, signSInstrs, signBInstrs, baseInstrsA, stepInstrsA,
            boxRef, b32, popArgs, initLocals, hle] at hrun
          rcases hsub : sub
              [.structv C [.i64v s, .null, .i32v sg], carrierSmall C 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : S.Repr (s - 1) vd :=
              hSub s 1 _ _ vd hvn (S.smallIntro 1) hsub
            rcases hadd : add
                [vacc, .structv C [.i64v s, .null, .i32v sg]] with _ | va
            · simp [hadd] at hrun
            · simp only [hadd] at hrun
              have hra : S.Repr (acc + s) va := hAdd acc s _ _ va hvacc hvn hadd
              rcases hrec : wFuncN code host fuel self [vd, va] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec, Option.some.injEq] at hrun
                rw [evalRecA_step s acc hle, ← hrun]
                exact ih (s - 1) (acc + s) vd va vr hrd hra hrec
      · obtain ⟨hsign, hne⟩ := S.bigElim n s lty les sg hvn
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
          simp [wFuncN, wRunF, hself, hBox, recInstrsA, signSInstrs,
            signBInstrs, baseInstrsA, stepInstrsA, boxRef, b32, popArgs,
            initLocals, hlt] at hrun
          rw [evalRecA_base n acc hn0, ← hrun]
          exact hvacc
        · have hn0 : ¬ n ≤ 0 := by
            intro hle
            have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
            omega
          simp [wFuncN, wRunF, hself, hBox, hAddHost, hSubHost, hSelfHost,
            recInstrsA, signSInstrs, signBInstrs, baseInstrsA, stepInstrsA,
            boxRef, b32, popArgs, initLocals, hlt] at hrun
          rcases hsub : sub
              [.structv C [.i64v s, .arr lty les, .i32v sg], carrierSmall C 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : S.Repr (n - 1) vd :=
              hSub n 1 _ _ vd hvn (S.smallIntro 1) hsub
            rcases hadd : add
                [vacc, .structv C [.i64v s, .arr lty les, .i32v sg]] with _ | va
            · simp [hadd] at hrun
            · simp only [hadd] at hrun
              have hra : S.Repr (acc + n) va := hAdd acc n _ _ va hvacc hvn hadd
              rcases hrec : wFuncN code host fuel self [vd, va] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec, Option.some.injEq] at hrun
                rw [evalRecA_step n acc hn0, ← hrun]
                exact ih (n - 1) (acc + n) vd va vr hrd hra hrec


theorem recursion_accumulator_generic_certified_total_aux
    (C self bI aI sI nlocals : Nat)
    (S : CarrierSpec C)
    (code : CodeTbl) (host : HostTbl)
    (add sub : List WVal → Option WVal)
    (hBox : host bI = some (1, boxRef C))
    (hAddHost : host aI = some (2, add))
    (hSubHost : host sI = some (2, sub))
    (hSelfHost : host self = none)
    (hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (hAddTot : ∀ a b va vb, S.Repr a va → S.Repr b vb →
      ∃ w, add [va, vb] = some w)
    (hSubTot : ∀ a b va vb, S.Repr a va → S.Repr b vb →
      ∃ w, sub [va, vb] = some w)
    (plan : RecursionRawPlan) (sh : RecShapeA)
    (hparse : parseRecShapeA self bI aI sI plan = some sh)
    (instrs : List WInstr)
    (hlow : lowerBlock C plan.body = some instrs)
    (hself : code self = some ⟨2, nlocals, instrs⟩) :
    ∀ (fuel : Nat) (n acc : Int) (vn vacc : WVal),
      S.Repr n vn → S.Repr acc vacc → n.natAbs < fuel →
      ∃ w, wFuncN code host fuel self [vn, vacc] = some w ∧
        S.Repr (evalRecA n acc) w := by
  have hcanon := parseA_lower C self bI aI sI plan sh hparse
  rw [hlow] at hcanon
  injection hcanon with hinstrs
  subst instrs
  cases sh
  intro fuel
  induction fuel with
  | zero =>
      intro n acc vn vacc hvn hvacc hlt
      omega
  | succ fuel ih =>
      intro n acc vn vacc hvn hvacc hfuel
      rcases S.car n vn hvn with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := S.smallElim n s sg hvn
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · refine ⟨vacc, ?_, ?_⟩
          · simp [wFuncN, wRunF, hself, recInstrsA, signSInstrs,
              signBInstrs, baseInstrsA, stepInstrsA, b32, initLocals, hle]
          · rw [evalRecA_base s acc hle]
            exact hvacc
        · obtain ⟨vd, hsub⟩ := hSubTot s 1 _ (carrierSmall C 1)
            hvn (S.smallIntro 1)
          have hrd : S.Repr (s - 1) vd :=
            hSub s 1 _ _ vd hvn (S.smallIntro 1) hsub
          obtain ⟨va, hadd⟩ := hAddTot acc s _ _ hvacc hvn
          have hra : S.Repr (acc + s) va := hAdd acc s _ _ va hvacc hvn hadd
          obtain ⟨w, hrec, hrepr⟩ := ih (s - 1) (acc + s) vd va hrd hra (by omega)
          refine ⟨w, ?_, ?_⟩
          · simp [wFuncN, wRunF, hself, hBox, hAddHost, hSubHost, hSelfHost,
              recInstrsA, signSInstrs, signBInstrs, baseInstrsA, stepInstrsA,
              boxRef, b32, popArgs, initLocals, hle, hsub, hadd, hrec]
          · rw [evalRecA_step s acc hle]
            exact hrepr
      · obtain ⟨hsign, hne⟩ := S.bigElim n s lty les sg hvn
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
          refine ⟨vacc, ?_, ?_⟩
          · simp [wFuncN, wRunF, hself, recInstrsA, signSInstrs,
              signBInstrs, baseInstrsA, stepInstrsA, b32, initLocals, hlt]
          · rw [evalRecA_base n acc hn0]
            exact hvacc
        · have hn0 : ¬ n ≤ 0 := by
            intro hle
            have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
            omega
          obtain ⟨vd, hsub⟩ := hSubTot n 1 _ (carrierSmall C 1)
            hvn (S.smallIntro 1)
          have hrd : S.Repr (n - 1) vd :=
            hSub n 1 _ _ vd hvn (S.smallIntro 1) hsub
          obtain ⟨va, hadd⟩ := hAddTot acc n _ _ hvacc hvn
          have hra : S.Repr (acc + n) va := hAdd acc n _ _ va hvacc hvn hadd
          obtain ⟨w, hrec, hrepr⟩ := ih (n - 1) (acc + n) vd va hrd hra (by omega)
          refine ⟨w, ?_, ?_⟩
          · simp [wFuncN, wRunF, hself, hBox, hAddHost, hSubHost, hSelfHost,
              recInstrsA, signSInstrs, signBInstrs, baseInstrsA, stepInstrsA,
              boxRef, b32, popArgs, initLocals, hlt, hsub, hadd, hrec]
          · rw [evalRecA_step n acc hn0]
            exact hrepr


theorem recursion_accumulator_generic_certified_total
    (C self bI aI sI nlocals : Nat)
    (S : CarrierSpec C)
    (code : CodeTbl) (host : HostTbl)
    (add sub : List WVal → Option WVal)
    (hBox : host bI = some (1, boxRef C))
    (hAddHost : host aI = some (2, add))
    (hSubHost : host sI = some (2, sub))
    (hSelfHost : host self = none)
    (hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (hAddTot : ∀ a b va vb, S.Repr a va → S.Repr b vb →
      ∃ w, add [va, vb] = some w)
    (hSubTot : ∀ a b va vb, S.Repr a va → S.Repr b vb →
      ∃ w, sub [va, vb] = some w)
    (plan : RecursionRawPlan) (sh : RecShapeA)
    (hparse : parseRecShapeA self bI aI sI plan = some sh)
    (instrs : List WInstr)
    (hlow : lowerBlock C plan.body = some instrs)
    (hself : code self = some ⟨2, nlocals, instrs⟩) :
    ∀ (n acc : Int) (vn vacc : WVal), S.Repr n vn → S.Repr acc vacc →
      ∃ w, wFuncN code host (n.natAbs + 1) self [vn, vacc] = some w ∧
        S.Repr (evalRecA n acc) w :=
  fun n acc vn vacc hvn hvacc =>
    recursion_accumulator_generic_certified_total_aux
      C self bI aI sI nlocals S code host add sub hBox hAddHost hSubHost hSelfHost hAdd hSub
      hAddTot hSubTot plan sh hparse instrs hlow hself
      (n.natAbs + 1) n acc vn vacc hvn hvacc (by omega)


/- The fixture-specific section is retained as spike documentation but is not
   part of the reusable generic module. -/
end V3Rec
