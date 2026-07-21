/-
Acceptance-soundness wiring for source expression fragments.

Acceptance pins the audited SymRawPlan encoder, checked representation plan,
canonical lowering, and exact code entry.  The independent obligation
domain/model face and the plan-evaluator result stay explicit, following the
established family-discharge pattern.  The bridge chooses the exact input
values: comparison fragments use `carrierSmall`, Bool fragments use `b32`,
and contracted integer fragments preserve arbitrary represented inputs.  For
partial host contracts, the successful byte run is exposed only to rule out a
missing host result; the audited generic still identifies the evaluator result
with the byte result.
-/
import AcceptanceSoundnessCore

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

namespace AcceptanceSoundness

/-- The semantic face not carried by `symFragmentPlanAccepted`.  It relates an
arbitrary obligation-domain representation to the generic theorem's honest
input values and pins the SymRawPlan-derived evaluator's result to the
obligation's independently declared model/codomain relation. -/
def exprFragmentSemanticBridge
    (claim : SymFragmentClaim) (plan : ExprFragmentRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (add sub mul stringEq : List WVal → Option WVal)
    (stringConcat : Nat → List WVal → Option WVal)
    (toIndex : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (hMul : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      mul [va, vb] = some w → S.Repr (a * b) w)
    (hStringEq : ∀ a b w, stringEq [a, b] = some w →
      w = b32 (stringEqW a b))
    (hStringConcat : ∀ resultTy parts c,
      stringConcat resultTy [parts] = some c →
        stringConcatW resultTy parts = some c)
    (hToIndex : ∀ n v r, S.Repr n v → toIndex [v] = some r →
      r = .i32v (toIndexW n))
    (fuel : Nat) (x : claim.obligation.Dom) (vs : List WVal) (w : WVal),
    claim.obligation.domRepr S x vs →
    wFuncN claim.obligation.code
      (claim.obligation.host add sub mul stringEq stringConcat toIndex)
      (fuel + 1) claim.obligation.self vs = some w →
    ∃ (inputs : List WVal) (modelLocals : List WVal) (result : WVal),
      vs = inputs ∧
      inputs.length = plan.params.length ∧
      ExprFragmentSoundness.blockCallsOK
        (claim.obligation.host add sub mul stringEq stringConcat toIndex)
        (fun g => (claim.obligation.code g).map (fun c => c.arity))
        plan.body ∧
      ExprFragmentSemantics.evalSymRawPlan
        claim.hostTable claim.structTable
        (claim.obligation.host add sub mul stringEq stringConcat toIndex)
        (fun g => (claim.obligation.code g).map (fun c => c.arity))
        (fun g args => wFuncN claim.obligation.code
          (claim.obligation.host add sub mul stringEq stringConcat toIndex) fuel g args)
        claim.obligation.carrier claim.plan
        (initLocals ⟨plan.params.length, exprFragmentNLocals plan, []⟩
          inputs) =
          some (.ok modelLocals [result]) ∧
      claim.obligation.codRepr S (claim.obligation.model x) result

/-- The audited expression-fragment generic currently owns exactly the
integer/Bool boundary admitted by the producer classifier. -/
def exprFragmentUsesAuditedGeneric (claim : SymFragmentClaim) : Bool :=
  claim.plan.params.all (fun ty => ty = .int || ty = .bool) &&
  (claim.plan.result = .int || claim.plan.result = .bool)

/-- Float semantics are deliberately outside the audited integer/Bool model. A
float at the source boundary is the only bespoke residual admitted below. -/
def exprFragmentHasFloatBoundary (claim : SymFragmentClaim) : Bool :=
  claim.plan.params.any (· = .float) || claim.plan.result = .float

/-- Projection-faced expression fragments are migrated, but their canonical
discharge lives in the audited field-projection wall. -/
def exprFragmentHasFieldProjection (claim : SymFragmentClaim) : Bool :=
  claim.plan.body.nodes.any (fun node =>
    match node.kind with
    | .projectField _ _ _ _ => true
    | _ => false)

/-- Tag-dispatch fragments (Option/Result `match` returning an Int constant)
also discharge through the symbolic generic — their operational model is a
conditional over boxed constants — but their source scrutinee is an ADT, so
they are outside the int/Bool `exprFragmentUsesAuditedGeneric` gate. The gate
here is the encoded representation shape: an `adtRef` scrutinee, an `intCarrier`
result, and a `struct.get.user` tag read. -/
def exprFragmentIsTagDispatch (claim : SymFragmentClaim) : Bool :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some plan =>
      plan.params = [.adtRef] && plan.result = .intCarrier &&
      plan.body.nodes.any (fun node =>
        match node.kind with
        | .structGetUser _ _ _ => true
        | _ => false)
  | none => false

/-- Fused vector-read fragments (`Option.withDefault(Vector.get(p0, p1), d)`)
discharge through the audited template theorem
(`StandardFace.vectorGetOrDefault_simulates_model`), not through the symbolic
generic: their operational content is the monolithic bounds-checked template,
whose semantics the interpreter clauses prove once, generically over every
hole. The gate is the encoded representation shape: exactly the single
monolithic node. -/
def exprFragmentIsVectorGetOrDefault (claim : SymFragmentClaim) : Bool :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some plan =>
      match AverCert.WasmSlice.exprVectorGetOrDefaultArrTy? plan with
      | some _ => true
      | none => false
  | none => false

/-- Side condition for one source expression claim.  In-model claims must use
the symbolic generic. Projection claims may use the audited projection
generic. Fused vector-read claims discharge through the audited template
theorem. Only float-boundary claims may use a bespoke direct discharge. -/
def exprFragmentSideCondition (claim : SymFragmentClaim) : Prop :=
  (exprFragmentUsesAuditedGeneric claim = true ∧
    ∀ plan,
      AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
          claim.hostTable claim.structTable claim.plan = some plan →
        exprFragmentSemanticBridge claim plan) ∨
  (exprFragmentIsTagDispatch claim = true ∧
    ∀ plan,
      AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
          claim.hostTable claim.structTable claim.plan = some plan →
        exprFragmentSemanticBridge claim plan) ∨
  (exprFragmentIsVectorGetOrDefault claim = true ∧
    obligationHolds claim.obligation) ∨
  (exprFragmentHasFieldProjection claim = true ∧
    obligationHolds claim.obligation) ∨
  (exprFragmentHasFloatBoundary claim = true ∧
    obligationHolds claim.obligation)

def exprFragmentSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.symFragmentClaims, exprFragmentSideCondition claim

theorem exprFragment_claim_discharges_generic
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (claim : SymFragmentClaim)
    (hMem : claim ∈ artifact.symFragmentClaims)
    (hBridge : ∀ plan,
      AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
          claim.hostTable claim.structTable claim.plan = some plan →
        exprFragmentSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  have hClaim : symFragmentClaimAccepted artifact.modBytes artifact.modLen claim :=
    allClaims_of_mem
      (symFragmentClaimAccepted artifact.modBytes artifact.modLen)
      artifact.symFragmentClaims hAcc claim hMem
  unfold symFragmentClaimAccepted symFragmentPlanAccepted at hClaim
  cases hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | none => simp [hEncode] at hClaim
  | some plan =>
      have hAccepted : exprFragmentPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier plan claim.obligation := by
        simpa [hEncode] using hClaim
      rcases hAccepted with
        ⟨_hExport, hCarrier, body, codeEntry, binding, hPlanAccepted,
          _hFuncType, _hNominalTypes, hSelf, hCode⟩
      rcases hPlanAccepted with
        ⟨hCheck, hLowerExpr, _hCodeEntry, _hExactBinding⟩
      rcases hBridge plan hEncode with ⟨hPolicy, hSemantic⟩
      have hLower : AverCert.PlanLower.lowerBlock
          claim.obligation.carrier plan.body = some body := by
        simp only [AverCert.PlanLower.lowerExprFragmentBody, hCheck,
          if_true] at hLowerExpr
        simpa [hCarrier] using hLowerExpr
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨plan.params.length, exprFragmentNLocals plan, body⟩ := by
        simpa [← hSelf] using hCode
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat toIndex
        hAdd hSub hMul hStringEq hStringConcat _hToIndex fuel x vs w hDom hRun
      cases fuel with
      | zero => simp [wFuncN] at hRun
      | succ fuel =>
          rcases hSemantic S add sub mul stringEq stringConcat toIndex
              hAdd hSub hMul hStringEq hStringConcat _hToIndex fuel x vs w hDom hRun with
            ⟨inputs, modelLocals, result, rfl, hArity, hCalls, hEval, hCod⟩
          have hGeneric := ExprFragmentSoundness.exprfragment_generic_certified
            S claim.hostTable claim.structTable claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat toIndex)
            claim.plan plan hEncode hCheck body hLower claim.obligation.self
            (exprFragmentNLocals plan) fuel hCodeSelf vs hArity hCalls
            modelLocals result hEval
          rw [hGeneric] at hRun
          have hResult : result = w := Option.some.inj hRun
          simpa [hResult] using hCod

theorem exprFragment_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (claim : SymFragmentClaim)
    (hMem : claim ∈ artifact.symFragmentClaims)
    (hSide : exprFragmentSideCondition claim) :
    obligationHolds claim.obligation := by
  rcases hSide with hGeneric | hTagDispatch | hVectorGet | hProjection | hFloat
  · exact exprFragment_claim_discharges_generic artifact hAcc claim hMem hGeneric.2
  · exact exprFragment_claim_discharges_generic artifact hAcc claim hMem hTagDispatch.2
  · exact hVectorGet.2
  · exact hProjection.2
  · exact hFloat.2

theorem exprFragment_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (hSemantic : exprFragmentSemanticBridges artifact) :
    ∀ o ∈ artifact.symFragmentClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact exprFragment_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

end AcceptanceSoundness
