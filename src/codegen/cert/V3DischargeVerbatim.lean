/-
v3 master wiring — verbatim-family discharge.

Besides the independent semantic face, this family exposes a real admission
gap: `checkVerbatimRawPlan` does not imply the stronger guards required by the
generic theorem.  The gap is stated explicitly and witnessed below.
-/
import V3Master
import V3ConstructVerbatim

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

/-- The exact extra guard required by `generic_verbatim_certified`. -/
def verbatimGenericGuards (plan : VerbatimRawPlan) : Prop :=
  V3ConstructVerbatim.checkVerbatimPlan (verbatimNLocals plan) plan = true

/-- A bare leaf is admitted by the audited raw checker but rejected by the
generic theorem's non-leaf-root guard.  Such a plan can describe a real export
whose bytes are exactly the constant leaf lowering; acceptance has no separate
conjunct excluding it. -/
def uncoveredVerbatimLeafPlan : VerbatimRawPlan :=
  { profile := "verbatim-plan-v1"
    scrutineeLocal := 0
    fieldLocal := 0
    resultSig := .refNull 0
    body := .leaf .refNull }

theorem verbatim_raw_allows_bare_leaf :
    AverCert.PlanCheck.checkVerbatimRawPlan uncoveredVerbatimLeafPlan = true := by
  decide

theorem verbatim_generic_rejects_bare_leaf :
    ¬ verbatimGenericGuards uncoveredVerbatimLeafPlan := by
  unfold verbatimGenericGuards
  decide

/-- The raw checker also leaves both scratch-local bounds unconstrained. -/
def uncoveredVerbatimLocalsPlan : VerbatimRawPlan :=
  { profile := "verbatim-plan-v1"
    scrutineeLocal := 9
    fieldLocal := 10
    resultSig := .refNull 0
    body := .test 0 .refNull (.leaf .refNull) }

theorem verbatim_raw_allows_oob_locals :
    AverCert.PlanCheck.checkVerbatimRawPlan uncoveredVerbatimLocalsPlan = true := by
  decide

theorem verbatim_generic_rejects_oob_locals :
    ¬ verbatimGenericGuards uncoveredVerbatimLocalsPlan := by
  unfold verbatimGenericGuards
  decide

/-- Semantic and generic-admission faces absent from
`verbatimPlanAccepted`.  The exact plan-derived `WVal` must represent the
obligation's separately declared model result. -/
def verbatimSemanticBridge
    (claim : VerbatimClaim) (plan : VerbatimRawPlan) : Prop :=
  verbatimGenericGuards plan ∧
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (x : claim.obligation.Dom) (vs : List WVal),
    claim.obligation.domRepr S x vs →
    ∃ v,
      vs = [v] ∧
      claim.obligation.codRepr S (claim.obligation.model x)
        (V3ConstructVerbatim.verbatimModel plan v)

def verbatimSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.verbatimClaims,
    ∀ plan,
      verbatimPlanForExport claim.exportName
          artifact.manifest.verbatimPlans = some plan →
        verbatimSemanticBridge claim plan

/-- Byte/plan half, parameterized only by the generic guards that current raw
acceptance does not establish. -/
theorem verbatim_accepted_call
    (artifact : ArtifactData)
    (hAcc : acceptedVerbatimFragments artifact)
    (claim : VerbatimClaim)
    (hMem : claim ∈ artifact.verbatimClaims)
    (hGuards : ∀ plan,
      verbatimPlanForExport claim.exportName
          artifact.manifest.verbatimPlans = some plan →
        verbatimGenericGuards plan) :
    ∃ plan,
      verbatimPlanForExport claim.exportName
          artifact.manifest.verbatimPlans = some plan ∧
      ∀ (host : HostTbl) (fuel : Nat) (v w : WVal),
        wFuncN claim.obligation.code host (fuel + 1)
            claim.obligation.self [v] = some w →
          w = V3ConstructVerbatim.verbatimModel plan v := by
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
        ⟨_hExport, _hCarrier, _hRaw, codeEntry, binding,
          _hCodeEntry, _hExportCode, _hBinding, hSelf, _hBindingCode,
          _hFuncType, _hPayload, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, verbatimNLocals plan,
            AverCert.PlanLower.lowerVerbatimBody plan⟩ := by
        simpa [← hSelf] using hCode
      refine ⟨plan, rfl, ?_⟩
      intro host fuel v w hRun
      exact V3ConstructVerbatim.generic_verbatim_certified
        plan claim.obligation.code host claim.obligation.self
        (verbatimNLocals plan) (hGuards plan hPlan) hCodeSelf fuel v w hRun

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
      rcases hBridge plan hPlan with ⟨hGuards, hPolicy, hSemantic⟩
      rcases hAccepted with
        ⟨_hExport, _hCarrier, _hRaw, codeEntry, binding,
          _hCodeEntry, _hExportCode, _hBinding, hSelf, _hBindingCode,
          _hFuncType, _hPayload, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, verbatimNLocals plan,
            AverCert.PlanLower.lowerVerbatimBody plan⟩ := by
        simpa [← hSelf] using hCode
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat
        _hAdd _hSub _hMul _hStringEq _hStringConcat fuel x vs w hDom hRun
      rcases hSemantic S x vs hDom with ⟨v, hVs, hCod⟩
      subst vs
      cases fuel with
      | zero => simp [wFuncN] at hRun
      | succ fuel =>
          have hCall := V3ConstructVerbatim.generic_verbatim_certified
            plan claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            claim.obligation.self (verbatimNLocals plan)
            hGuards hCodeSelf fuel v w hRun
          simpa [hCall] using hCod

theorem verbatim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedVerbatimFragments artifact)
    (hSemantic : verbatimSemanticBridges artifact) :
    ∀ o ∈ artifact.verbatimClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact verbatim_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

end V3Master

#print axioms V3Master.verbatim_raw_allows_bare_leaf
#print axioms V3Master.verbatim_generic_rejects_bare_leaf
#print axioms V3Master.verbatim_raw_allows_oob_locals
#print axioms V3Master.verbatim_generic_rejects_oob_locals
#print axioms V3Master.verbatim_accepted_call
#print axioms V3Master.verbatim_claim_discharges
#print axioms V3Master.verbatim_discharges
