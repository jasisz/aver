/-
v3 master wiring — source expression-fragment discharge.

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
import V3Master

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

namespace V3Master

private theorem allClaims_of_mem {Claim : Type u} (accept : Claim → Prop)
    (claims : List Claim) (hAll : allClaims accept claims)
    (claim : Claim) (hMem : claim ∈ claims) : accept claim := by
  induction claims with
  | nil => simp at hMem
  | cons head tail ih =>
      simp only [allClaims] at hAll
      simp only [List.mem_cons] at hMem
      rcases hAll with ⟨hHead, hTail⟩
      rcases hMem with rfl | hMem
      · exact hHead
      · exact ih hTail hMem

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
    (fuel : Nat) (x : claim.obligation.Dom) (vs : List WVal) (w : WVal),
    claim.obligation.domRepr S x vs →
    wFuncN claim.obligation.code
      (claim.obligation.host add sub mul stringEq stringConcat)
      (fuel + 1) claim.obligation.self vs = some w →
    ∃ (inputs : List WVal) (modelLocals : List WVal) (result : WVal),
      vs = inputs ∧
      inputs.length = plan.params.length ∧
      V3ExprFragmentGeneric.blockCallsOK
        (claim.obligation.host add sub mul stringEq stringConcat)
        (fun g => (claim.obligation.code g).map (fun c => c.arity))
        plan.body ∧
      V3ExprFragmentFull.evalSymRawPlan
        claim.hostTable claim.structTable
        (claim.obligation.host add sub mul stringEq stringConcat)
        (fun g => (claim.obligation.code g).map (fun c => c.arity))
        (fun g args => wFuncN claim.obligation.code
          (claim.obligation.host add sub mul stringEq stringConcat) fuel g args)
        claim.obligation.carrier claim.plan
        (initLocals ⟨plan.params.length, exprFragmentNLocals plan, []⟩
          inputs) =
          some (.ok modelLocals [result]) ∧
      claim.obligation.codRepr S (claim.obligation.model x) result

def exprFragmentSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.symFragmentClaims,
    ∀ plan,
      AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
          claim.hostTable claim.structTable claim.plan = some plan →
        exprFragmentSemanticBridge claim plan

theorem exprFragment_claim_discharges
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
          hSelf, hCode⟩
      rcases hPlanAccepted with
        ⟨hCheck, hLowerExpr, _hCodeEntry, _hBinding, _hBindingCode⟩
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
      intro S add sub mul stringEq stringConcat
        hAdd hSub hMul hStringEq hStringConcat fuel x vs w hDom hRun
      cases fuel with
      | zero => simp [wFuncN] at hRun
      | succ fuel =>
          rcases hSemantic S add sub mul stringEq stringConcat
              hAdd hSub hMul hStringEq hStringConcat fuel x vs w hDom hRun with
            ⟨inputs, modelLocals, result, rfl, hArity, hCalls, hEval, hCod⟩
          have hGeneric := V3ExprFragmentGeneric.exprfragment_generic_certified
            S claim.hostTable claim.structTable claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            claim.plan plan hEncode hCheck body hLower claim.obligation.self
            (exprFragmentNLocals plan) fuel hCodeSelf vs hArity hCalls
            modelLocals result hEval
          rw [hGeneric] at hRun
          have hResult : result = w := Option.some.inj hRun
          simpa [hResult] using hCod

theorem exprFragment_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (hSemantic : exprFragmentSemanticBridges artifact) :
    ∀ o ∈ artifact.symFragmentClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact exprFragment_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

end V3Master

#print axioms V3Master.exprFragment_claim_discharges
#print axioms V3Master.exprFragment_discharges
