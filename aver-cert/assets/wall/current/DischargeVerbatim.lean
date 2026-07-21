/-
Acceptance-soundness wiring for verbatim plans.
-/
import AcceptanceSoundnessCore
import ConstructVerbatimSoundness

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

namespace AcceptanceSoundness

/-- The exact plan-derived `WVal` must represent the obligation's separately
declared model result. Plan admission is already part of artifact acceptance. -/
def verbatimSemanticBridge
    (claim : VerbatimClaim) (plan : VerbatimRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (x : claim.obligation.Dom) (vs : List WVal),
    claim.obligation.domRepr S x vs →
    ∃ v,
      vs = [v] ∧
      claim.obligation.codRepr S (claim.obligation.model x)
        (ConstructVerbatimSoundness.verbatimModel plan v)

def verbatimSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.verbatimClaims,
    ∀ plan,
      verbatimPlanForExport claim.exportName
          artifact.manifest.verbatimPlans = some plan →
        verbatimSemanticBridge claim plan

/-- The accepted byte/plan binding simulates its audited plan model. -/
theorem verbatim_accepted_call
    (artifact : ArtifactData)
    (hAcc : acceptedVerbatimFragments artifact)
    (claim : VerbatimClaim)
    (hMem : claim ∈ artifact.verbatimClaims) :
    ∃ plan,
      verbatimPlanForExport claim.exportName
          artifact.manifest.verbatimPlans = some plan ∧
      ∀ (host : HostTbl) (fuel : Nat) (v w : WVal),
        wFuncN claim.obligation.code host (fuel + 1)
            claim.obligation.self [v] = some w →
          w = ConstructVerbatimSoundness.verbatimModel plan v := by
  have hClaim : verbatimClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (verbatimClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.verbatimClaims hAcc claim hMem
  unfold verbatimClaimAccepted at hClaim
  cases hPlan : verbatimPlanForExport claim.exportName
      artifact.manifest.verbatimPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : verbatimPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier plan claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hAccepted with
        ⟨_hExport, _hCarrier, hCheck, codeEntry, binding,
          _hCodeEntry, _hExactBinding, hSelf, _hFuncType, _hPayload, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, verbatimNLocals plan,
            AverCert.PlanLower.lowerVerbatimBody plan⟩ := by
        simpa [← hSelf] using hCode
      refine ⟨plan, rfl, ?_⟩
      intro host fuel v w hRun
      exact ConstructVerbatimSoundness.generic_verbatim_certified
        plan claim.obligation.code host claim.obligation.self
        (verbatimNLocals plan) hCheck hCodeSelf fuel v w hRun

/-- Canonical option-(c) leaf bridge for a byte-derived verbatim dispatch.
The obligation model is the audited plan evaluator itself, so artifact-specific
callers supply only the reducible plan guard and code-table binding. -/
theorem verbatim_canonical_discharges
    (exportName : String) (carrier self : Nat)
    (plan : VerbatimRawPlan) (code : CodeTbl)
    (host :
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (Nat → List WVal → Option WVal) →
      (List WVal → Option WVal) → HostTbl)
    (hCheck : AverCert.PlanCheck.checkVerbatimPlan
      (verbatimNLocals plan) plan = true)
    (hCode : code self = some
      ⟨1, verbatimNLocals plan,
        AverCert.PlanLower.lowerVerbatimBody plan⟩) :
    Obligation.holds
      ({ export_ := exportName
         policy := .simulatesModel
         carrier := carrier
         code := code
         host := host
         self := self
         Dom := WVal
         Cod := WVal
         domRepr := fun _ v vs => vs = [v]
         codRepr := fun S v w => verbatimRepr S v w
         model := fun v => ConstructVerbatimSoundness.verbatimModel plan v } :
        Obligation) := by
  intro S add sub mul stringEq stringConcat toIndex
    _hAdd _hSub _hMul _hStringEq _hStringConcat _hToIndex fuel v vs w hDom hRun
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hCall := ConstructVerbatimSoundness.generic_verbatim_certified
        plan code (host add sub mul stringEq stringConcat toIndex) self
        (verbatimNLocals plan) hCheck hCode fuel v w hRun
      simpa [verbatimRepr] using hCall

theorem verbatim_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedVerbatimFragments artifact)
    (claim : VerbatimClaim)
    (hMem : claim ∈ artifact.verbatimClaims)
    (hBridge : ∀ plan,
      verbatimPlanForExport claim.exportName
          artifact.manifest.verbatimPlans = some plan →
        verbatimSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  rcases verbatim_accepted_call artifact hAcc claim hMem with
    ⟨plan, hPlan, hCall⟩
  rcases hBridge plan hPlan with ⟨hPolicy, hSemantic⟩
  rw [obligationHolds, hPolicy]
  intro S add sub mul stringEq stringConcat toIndex
    _hAdd _hSub _hMul _hStringEq _hStringConcat _hToIndex fuel x vs w hDom hRun
  rcases hSemantic S x vs hDom with ⟨v, hVs, hCod⟩
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hResult := hCall
        (claim.obligation.host add sub mul stringEq stringConcat toIndex)
        fuel v w hRun
      simpa [hResult] using hCod

theorem verbatim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedVerbatimFragments artifact)
    (hSemantic : verbatimSemanticBridges artifact) :
    ∀ o ∈ artifact.verbatimClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact verbatim_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

end AcceptanceSoundness
