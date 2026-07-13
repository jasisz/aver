/-
v3 master wiring — source expression-fragment discharge.

Acceptance pins the audited SymRawPlan encoder, checked representation plan,
canonical lowering, and exact code entry.  The independent obligation
domain/model face and the plan-evaluator result stay explicit, following the
established family-discharge pattern.  In particular, source integer inputs
are materialised with `carrierSmall`, which is the exact comparison domain of
`exprfragment_generic_certified`.
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
`carrierSmall` source arguments and pins the SymRawPlan-derived evaluator's
result to the obligation's independently declared model/codomain relation. -/
def exprFragmentSemanticBridge
    (claim : SymFragmentClaim) (plan : ExprFragmentRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (add sub mul stringEq : List WVal → Option WVal)
    (stringConcat : Nat → List WVal → Option WVal)
    (fuel : Nat) (x : claim.obligation.Dom) (vs : List WVal),
    claim.obligation.domRepr S x vs →
    ∃ (sourceArgs : List Int) (modelLocals : List WVal) (result : WVal),
      vs = sourceArgs.map (carrierSmall claim.obligation.carrier) ∧
      sourceArgs.length = plan.params.length ∧
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
          (sourceArgs.map (carrierSmall claim.obligation.carrier))) =
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
        _hAdd _hSub _hMul _hStringEq _hStringConcat fuel x vs w hDom hRun
      cases fuel with
      | zero => simp [wFuncN] at hRun
      | succ fuel =>
          rcases hSemantic S add sub mul stringEq stringConcat fuel x vs hDom with
            ⟨sourceArgs, modelLocals, result, rfl, hArity, hCalls, hEval, hCod⟩
          have hGeneric := V3ExprFragmentGeneric.exprfragment_generic_certified
            S claim.hostTable claim.structTable claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            claim.plan plan hEncode hCheck body hLower claim.obligation.self
            (exprFragmentNLocals plan) fuel hCodeSelf sourceArgs hArity hCalls
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
