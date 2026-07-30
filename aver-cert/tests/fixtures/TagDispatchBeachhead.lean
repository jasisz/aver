import PlanCheck
import PlanLower
import PlanBytes
import SchemaCore
import ExprFragmentSemantics
import ExprFragmentSoundness
import DischargeExprFragment
import AcceptedArtifactCore
import StandardFace

set_option linter.unusedSimpArgs false
set_option linter.unusedVariables false

open AverCert
open AverCert.Schema
open AverCert.PlanCheck
open CertPrelude

namespace TagDispatchDev

/-- slotCount(egg: Option<Int>) -> Int = match egg { None -> 0 ; Some(_) -> 1 }.
    Tag test: field 0 == 1 (Some) -> 1, else -> 0. -/
def slotCountSym : SymRawPlan :=
  { profile := "sym-fragment-v1",
    params := [.app1 "Option" .int],
    result := .int,
    body :=
      { nodes :=
        [ { id := 0, ty := .app1 "Option" .int, kind := .param 0 },
          { id := 1, ty := .int,
            kind := .tagMatch "Option" 0 1
              { nodes := [{ id := 0, ty := .int, kind := .constInt 1 }], result := 0 }
              { nodes := [{ id := 0, ty := .int, kind := .constInt 0 }], result := 0 } } ],
        result := 1 } }

-- carrier (Int) = 3, Option struct index = 2, box helper func index = 6.
def hostTable : List (HostRole × Nat) := [(.box, 6)]
def structTable : List (String × Nat) := [("Option", 2)]
def carrier : Nat := 3

/-- The canonical encoded representation plan (rendered as the producer/renderer
    would emit it: one line, `: FragBlock` ascriptions). -/
def slotCountPlan : ExprFragmentRawPlan := { profile := "expr-fragment-v1", params := [.adtRef], result := .intCarrier, body := ({ nodes := [{ id := 0, ty := .adtRef, kind := .local 0 }, { id := 1, ty := .rawI32, kind := .structGetUser 2 0 0 }, { id := 2, ty := .rawI32, kind := .constI32 (1 : Int) }, { id := 3, ty := .boolI32, kind := .prim .i32Eq [1, 2] }, { id := 4, ty := .intCarrier, kind := .ifElse 3 ({ nodes := [{ id := 0, ty := .i64, kind := .constI64 (1 : Int) }, { id := 1, ty := .intCarrier, kind := .hostCall .box 6 [0] }], result := 1 } : FragBlock) ({ nodes := [{ id := 0, ty := .i64, kind := .constI64 (0 : Int) }, { id := 1, ty := .intCarrier, kind := .hostCall .box 6 [0] }], result := 1 } : FragBlock) }], result := 4 } : FragBlock) }

example : checkSymRawPlan slotCountSym = true := by rfl
example : encodeSymRawPlanToExprFragmentRawPlan hostTable structTable slotCountSym = some slotCountPlan := by rfl
example : checkExprFragmentRawPlan slotCountPlan = true := by rfl
#eval AverCert.PlanLower.lowerBlock carrier slotCountPlan.body
#eval AverCert.AcceptedArtifact.exprFragmentNLocals slotCountPlan

open AverCert.AcceptedArtifact AcceptanceSoundness ExprFragmentSemantics

/-- The lowered body (concrete WInstr tree), used for the obligation's code. -/
def slotCountInstrs : List WInstr :=
  (AverCert.PlanLower.lowerBlock carrier slotCountPlan.body).getD []

/-- The operational tag-dispatch obligation for slotCount.
    Dom = (tag, payload); model reads the tag and returns 1 on `Some` (tag 1)
    else 0 — a REPRESENTATION-level meaning, not a source-constructor relation. -/
def slotCountOb : Obligation :=
  { export_ := "slotCount",
    policy := .simulatesModel,
    carrier := carrier,
    code := fun i => if i = 1 then some { arity := 1, nlocals := 1, body := slotCountInstrs } else none,
    host := AverCert.StandardFace.tagDispatchHost carrier 6,
    self := 1,
    Dom := Int × WVal,
    Cod := Int,
    domRepr := fun _S p vs => vs = [.structv 2 [.i32v p.1, p.2]],
    codRepr := fun S v w => intRepr S v w,
    model := fun p => if p.1 = 1 then 1 else 0 }

def slotCountClaim : SymFragmentClaim :=
  { exportNameBytes := [115, 108, 111, 116, 67, 111, 117, 110, 116],
    exportName := "slotCount",
    carrier := carrier,
    hostTable := hostTable,
    structTable := structTable,
    plan := slotCountSym,
    obligation := slotCountOb }

/-- THE SOUNDNESS-CRITICAL PIECE: the tag-dispatch semantic bridge for slotCount.
    Under any representation `S`, if the emitted body runs to `w` on a represented
    Option value, the plan evaluator produces a represented result of the model. -/
theorem slotCount_exprFragmentSemanticBridge :
    exprFragmentSemanticBridge slotCountClaim slotCountPlan := by
  refine ⟨rfl, ?_⟩
  intro S add sub mul stringEq stringConcat toIndex hAdd hSub hMul hStringEq hStringConcat
    _hToIndex fuel x vs w hDom hRun
  dsimp only [slotCountClaim, slotCountOb] at x hDom ⊢
  subst hDom
  by_cases hx : x.1 = 1
  · refine ⟨[.structv 2 [.i32v x.1, x.2]], [.structv 2 [.i32v x.1, x.2], .null],
      carrierSmall carrier 1, rfl, rfl, ?_, ?_, ?_⟩
    · simp [ExprFragmentSoundness.blockCallsOK, ExprFragmentSoundness.nodesCallsOK,
        ExprFragmentSoundness.kindCallsOK, slotCountPlan, slotCountClaim, slotCountOb,
        AverCert.StandardFace.tagDispatchHost]
    · simp [ExprFragmentSemantics.evalSymRawPlan, slotCountClaim, slotCountOb, AverCert.StandardFace.tagDispatchHost,
        show encodeSymRawPlanToExprFragmentRawPlan hostTable structTable slotCountSym
          = some slotCountPlan from rfl, slotCountPlan, ExprFragmentSemantics.runBlock,
        AverCert.PlanLower.maxFuel, ExprFragmentSemantics.runBlockFuel,
        ExprFragmentSemantics.runNodesFuel, ExprFragmentSemantics.finishWith,
        AverCert.AcceptedArtifact.exprFragmentNLocals, initLocals, PlanLower.popExpected,
        PlanLower.popExpectedAll, PlanLower.primInstr, ExprFragmentSemantics.runPrim,
        wRunF, boxRef, popArgs, carrier, carrierSmall, b32, hx]
    · simpa [slotCountClaim, slotCountOb, carrier, AverCert.Schema.intRepr, hx]
        using S.smallIntro (1 : Int)
  · refine ⟨[.structv 2 [.i32v x.1, x.2]], [.structv 2 [.i32v x.1, x.2], .null],
      carrierSmall carrier 0, rfl, rfl, ?_, ?_, ?_⟩
    · simp [ExprFragmentSoundness.blockCallsOK, ExprFragmentSoundness.nodesCallsOK,
        ExprFragmentSoundness.kindCallsOK, slotCountPlan, slotCountClaim, slotCountOb,
        AverCert.StandardFace.tagDispatchHost]
    · simp [ExprFragmentSemantics.evalSymRawPlan, slotCountClaim, slotCountOb, AverCert.StandardFace.tagDispatchHost,
        show encodeSymRawPlanToExprFragmentRawPlan hostTable structTable slotCountSym
          = some slotCountPlan from rfl, slotCountPlan, ExprFragmentSemantics.runBlock,
        AverCert.PlanLower.maxFuel, ExprFragmentSemantics.runBlockFuel,
        ExprFragmentSemantics.runNodesFuel, ExprFragmentSemantics.finishWith,
        AverCert.AcceptedArtifact.exprFragmentNLocals, initLocals, PlanLower.popExpected,
        PlanLower.popExpectedAll, PlanLower.primInstr, ExprFragmentSemantics.runPrim,
        wRunF, boxRef, popArgs, carrier, carrierSmall, b32, hx]
    · simpa [slotCountClaim, slotCountOb, carrier, AverCert.Schema.intRepr, hx]
        using S.smallIntro (0 : Int)

#print axioms slotCount_exprFragmentSemanticBridge

/-- The decoded byte role table naming the box helper at index 6. -/
def slotCountRoles : CertDecode.AddSub.Roles :=
  { box := some 6, add := none, mul := none, sub := none, toIndex := none }

/-- The wall's face selector picks the tag-dispatch face for slotCount, and it
    MATCHES the operational obligation (Dom/domRepr/model/host all pinned). -/
theorem slotCount_symFragmentMatches :
    AverCert.StandardFace.symFragmentMatches slotCountRoles slotCountClaim := by
  refine ⟨rfl, ?_⟩
  simp only [AverCert.StandardFace.symFragmentFace,
    show encodeSymRawPlanToExprFragmentRawPlan slotCountClaim.hostTable
      slotCountClaim.structTable slotCountClaim.plan = some slotCountPlan from rfl,
    show AverCert.StandardFace.classifyIntAdd slotCountPlan = none from rfl,
    show AverCert.WasmSlice.exprProjectionFace? slotCountPlan = none from rfl,
    show AverCert.StandardFace.classifyTagDispatch slotCountPlan
      = some { optIdx := 2, boxIdx := 6, tag := 1, thenC := 1, elseC := 0 } from rfl]
  exact ⟨rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, rfl, HEq.rfl⟩

/-- The full side condition discharges through the tag-dispatch disjunct, using
    the semantic bridge above — so the wall's `exprFragment_claim_discharges`
    concludes `obligationHolds slotCountOb`. -/
theorem slotCount_sideCondition :
    exprFragmentSideCondition slotCountClaim :=
  Or.inr (Or.inl ⟨rfl, fun plan hp => by
    have hpe : plan = slotCountPlan := by
      have : (some slotCountPlan : Option ExprFragmentRawPlan) = some plan := by
        rw [← hp]; rfl
      exact (Option.some.inj this).symm
    subst hpe
    exact slotCount_exprFragmentSemanticBridge⟩)

#print axioms slotCount_symFragmentMatches
#print axioms slotCount_sideCondition

end TagDispatchDev
