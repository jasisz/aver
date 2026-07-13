/-
v3 master wiring — constructor-family discharge.

Acceptance supplies the checked plan, canonical lowering, and exact code
entry.  The independent semantic face is kept explicit, as in the established
field-projection discharge pattern.
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

/-- The semantic face not carried by `constructPlanAccepted`: represented
inputs have the plan's arity and the exact constructed `WVal` represents the
obligation's independently declared model result. -/
def constructSemanticBridge
    (claim : ConstructClaim) (plan : ConstructRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (x : claim.obligation.Dom) (args : List WVal),
    claim.obligation.domRepr S x args →
    args.length = plan.arity ∧
    claim.obligation.codRepr S (claim.obligation.model x)
      (.structv claim.structIdx
        (V3ConstructVerbatim.constructModelFields
          (args ++ List.replicate 1 .null) plan.fields))

def constructSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.constructClaims,
    ∀ plan,
      constructPlanForExport claim.exportName
          artifact.manifest.constructPlans = some plan →
        constructSemanticBridge claim plan

/-- Byte/plan half of one constructor claim. -/
theorem construct_accepted_call
    (artifact : ArtifactData)
    (hAcc : acceptedConstructFragments artifact)
    (claim : ConstructClaim)
    (hMem : claim ∈ artifact.constructClaims) :
    ∃ plan,
      constructPlanForExport claim.exportName
          artifact.manifest.constructPlans = some plan ∧
      ∀ (host : HostTbl) (args : List WVal),
        args.length = plan.arity →
        wFuncN claim.obligation.code host 1 claim.obligation.self args =
          some (.structv claim.structIdx
            (V3ConstructVerbatim.constructModelFields
              (args ++ List.replicate 1 .null) plan.fields)) := by
  have hClaim : constructClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (constructClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.constructClaims hAcc.1 claim hMem
  unfold constructClaimAccepted at hClaim
  cases hPlan : constructPlanForExport claim.exportName
      artifact.manifest.constructPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : constructPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.structIdx claim.fieldCount claim.elemTy claim.symPlan
          plan claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hAccepted with
        ⟨_hExport, _hCarrier, _hSym, _hMatches, hCheck, _hFields,
          body, codeEntry, binding, hLow, _hCodeEntry, _hExportCode,
          _hBinding, hSelf, _hBindingCode, _hStructTy, _hFuncTy, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some { arity := plan.arity, nlocals := 1, body := body } := by
        simpa [← hSelf] using hCode
      refine ⟨plan, rfl, ?_⟩
      intro host args hLen
      exact V3ConstructVerbatim.generic_construct_certified
        claim.structIdx plan claim.obligation.code host claim.obligation.self 1
        hCheck body hLow hCodeSelf args hLen

private theorem construct_run_succ_eq_one
    (structIdx : Nat) (plan : ConstructRawPlan)
    (code : CodeTbl) (host : HostTbl) (self : Nat)
    (hCheck : AverCert.PlanCheck.checkConstructRawPlan plan = true)
    (body : List WInstr)
    (hLow : AverCert.PlanLower.lowerConstructBody structIdx plan = some body)
    (hCode : code self = some
      { arity := plan.arity, nlocals := 1, body := body })
    (fuel : Nat) (args : List WVal) (hLen : args.length = plan.arity) :
    wFuncN code host (fuel + 1) self args =
      wFuncN code host 1 self args := by
  have hCanonical : body =
      AverCert.PlanLower.lowerConstructFields structIdx plan.fields ++
        [.structNew structIdx plan.fields.length] := by
    simp [AverCert.PlanLower.lowerConstructBody, hCheck] at hLow
    exact hLow.symm
  subst body
  have hReadable := V3ConstructVerbatim.accepted_fields_readable
    plan 1 args hCheck hLen
  have hFuel := V3ConstructVerbatim.simNodes_construct
    host (fun g => (code g).map (·.arity))
    (fun g as => wFuncN code host fuel g as)
    structIdx (args ++ List.replicate 1 .null) plan.fields hReadable
    [.structNew structIdx plan.fields.length] []
  have hOne := V3ConstructVerbatim.simNodes_construct
    host (fun g => (code g).map (·.arity)) (fun _ _ => none)
    structIdx (args ++ List.replicate 1 .null) plan.fields hReadable
    [.structNew structIdx plan.fields.length] []
  simp only [wFuncN, hCode, initLocals]
  change V3ConstructVerbatim.outValue
      (wRunF host (fun g => (code g).map (·.arity))
        (fun g as => wFuncN code host fuel g as)
        (AverCert.PlanLower.lowerConstructFields structIdx plan.fields ++
          [.structNew structIdx plan.fields.length])
        (args ++ List.replicate 1 .null) []) =
    V3ConstructVerbatim.outValue
      (wRunF host (fun g => (code g).map (·.arity)) (fun _ _ => none)
        (AverCert.PlanLower.lowerConstructFields structIdx plan.fields ++
          [.structNew structIdx plan.fields.length])
        (args ++ List.replicate 1 .null) [])
  rw [hFuel, hOne]
  simp [wRunF]

theorem construct_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedConstructFragments artifact)
    (claim : ConstructClaim)
    (hMem : claim ∈ artifact.constructClaims)
    (hBridge : ∀ plan,
      constructPlanForExport claim.exportName
          artifact.manifest.constructPlans = some plan →
        constructSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  have hClaim : constructClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (constructClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.constructClaims hAcc.1 claim hMem
  unfold constructClaimAccepted at hClaim
  cases hPlan : constructPlanForExport claim.exportName
      artifact.manifest.constructPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : constructPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.structIdx claim.fieldCount claim.elemTy claim.symPlan
          plan claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hBridge plan hPlan with ⟨hPolicy, hSemantic⟩
      rcases hAccepted with
        ⟨_hExport, _hCarrier, _hSym, _hMatches, hCheck, _hFields,
          body, codeEntry, binding, hLow, _hCodeEntry, _hExportCode,
          _hBinding, hSelf, _hBindingCode, _hStructTy, _hFuncTy, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some { arity := plan.arity, nlocals := 1, body := body } := by
        simpa [← hSelf] using hCode
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat
        _hAdd _hSub _hMul _hStringEq _hStringConcat fuel x args w hDom hRun
      rcases hSemantic S x args hDom with ⟨hLen, hCod⟩
      cases fuel with
      | zero => simp [wFuncN] at hRun
      | succ fuel =>
          have hCall := V3ConstructVerbatim.generic_construct_certified
            claim.structIdx plan claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            claim.obligation.self 1 hCheck body hLow hCodeSelf args hLen
          have hFuel := construct_run_succ_eq_one
            claim.structIdx plan claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            claim.obligation.self hCheck body hLow hCodeSelf fuel args hLen
          rw [hFuel, hCall] at hRun
          have hw :
              .structv claim.structIdx
                (V3ConstructVerbatim.constructModelFields
                  (args ++ List.replicate 1 .null) plan.fields) = w :=
            Option.some.inj hRun
          simpa [← hw] using hCod

theorem construct_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedConstructFragments artifact)
    (hSemantic : constructSemanticBridges artifact) :
    ∀ o ∈ artifact.constructClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact construct_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

end V3Master

#print axioms V3Master.construct_accepted_call
#print axioms V3Master.construct_claim_discharges
#print axioms V3Master.construct_discharges
