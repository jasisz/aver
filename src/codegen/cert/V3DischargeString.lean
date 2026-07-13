/-
v3 master wiring — String.eq and String.concat discharges.

The generic theorems consume exactly the named helper contracts quantified by
`Schema.Obligation.holds`; this file threads those hypotheses through the
audited canonical host wiring.  String results use the concrete `WVal` model
face (the generated obligations instantiate `codRepr` with `verbatimRepr`).
-/
import V3Master
import V3String

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

def stringEqSemanticBridge
    (claim : StringEqClaim) (plan : StringEqRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (x : claim.obligation.Dom) (vs : List WVal),
    claim.obligation.domRepr S x vs →
    ∃ v,
      vs = [v] ∧
      claim.obligation.codRepr S (claim.obligation.model x)
        (V3String.evalStringEq claim.stringTy plan v)

def stringConcatSemanticBridge
    (claim : StringConcatClaim) (plan : StringConcatRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (x : claim.obligation.Dom) (vs : List WVal),
    claim.obligation.domRepr S x vs →
    ∃ v,
      vs = [v] ∧
      claim.obligation.codRepr S (claim.obligation.model x)
        (V3String.evalStringConcat claim.resultTy claim.containerTy plan v)

def stringSemanticBridges (artifact : ArtifactData) : Prop :=
  (∀ claim ∈ artifact.stringEqClaims,
    ∀ plan,
      stringEqPlanForExport claim.exportName
          artifact.manifest.stringEqPlans = some plan →
        stringEqSemanticBridge claim plan) ∧
  (∀ claim ∈ artifact.stringConcatClaims,
    ∀ plan,
      stringConcatPlanForExport claim.exportName
          artifact.manifest.stringConcatPlans = some plan →
        stringConcatSemanticBridge claim plan)

/-- The string family's two adjacent slices in `claimObligations`. -/
def stringClaimObligations (artifact : ArtifactData) : List Obligation :=
  artifact.stringEqClaims.map (·.obligation) ++
  artifact.stringConcatClaims.map (·.obligation)

theorem stringEq_accepted_call
    (artifact : ArtifactData)
    (hAcc : acceptedStringEqFragments artifact)
    (claim : StringEqClaim)
    (hMem : claim ∈ artifact.stringEqClaims) :
    ∃ plan,
      stringEqPlanForExport claim.exportName
          artifact.manifest.stringEqPlans = some plan ∧
      ∀ (add sub mul stringEq : List WVal → Option WVal)
        (stringConcat : Nat → List WVal → Option WVal),
        (∀ a b w, stringEq [a, b] = some w →
          w = b32 (stringEqW a b)) →
        ∀ (fuel : Nat) (v w : WVal),
          wFuncN claim.obligation.code
              (claim.obligation.host add sub mul stringEq stringConcat)
              (fuel + 1) claim.obligation.self [v] = some w →
            w = V3String.evalStringEq claim.stringTy plan v := by
  have hClaim : stringEqClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (stringEqClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.stringEqClaims hAcc claim hMem
  unfold stringEqClaimAccepted at hClaim
  cases hPlan : stringEqPlanForExport claim.exportName
      artifact.manifest.stringEqPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : stringEqPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.stringTy claim.stringEqFuncIdx
          artifact.manifest.subject.stringHostRoles claim.symPlan plan
          claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hAccepted with
        ⟨_hExport, _hCarrier, _hRole, hHost, _hSym, _hMatches, hCheck,
          body, codeEntry, binding, hLow, _hCodeEntry, _hExportCode,
          _hBinding, hSelf, _hBindingCode, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, 2, body⟩ := by
        simpa [← hSelf] using hCode
      refine ⟨plan, rfl, ?_⟩
      intro add sub mul stringEq stringConcat hStringEq fuel v w hRun
      have hHostSlot :
          claim.obligation.host add sub mul stringEq stringConcat
              claim.stringEqFuncIdx = some (2, stringEq) := by
        rw [hHost]
        simp [stringEqCanonicalHost]
      exact V3String.generic_string_eq_certified
        claim.stringTy claim.stringEqFuncIdx plan claim.obligation.code
        (claim.obligation.host add sub mul stringEq stringConcat)
        claim.obligation.self stringEq hStringEq hCheck body hLow hCodeSelf
        hHostSlot fuel v w hRun

theorem stringConcat_accepted_call
    (artifact : ArtifactData)
    (hAcc : acceptedStringConcatFragments artifact)
    (claim : StringConcatClaim)
    (hMem : claim ∈ artifact.stringConcatClaims) :
    ∃ plan,
      stringConcatPlanForExport claim.exportName
          artifact.manifest.stringConcatPlans = some plan ∧
      ∀ (add sub mul stringEq : List WVal → Option WVal)
        (stringConcat : Nat → List WVal → Option WVal),
        (∀ resultTy parts c,
          stringConcat resultTy [parts] = some c →
            stringConcatW resultTy parts = some c) →
        ∀ (fuel : Nat) (v w : WVal),
          wFuncN claim.obligation.code
              (claim.obligation.host add sub mul stringEq stringConcat)
              (fuel + 1) claim.obligation.self [v] = some w →
            w = V3String.evalStringConcat
              claim.resultTy claim.containerTy plan v := by
  have hClaim : stringConcatClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (stringConcatClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.stringConcatClaims hAcc claim hMem
  unfold stringConcatClaimAccepted at hClaim
  cases hPlan : stringConcatPlanForExport claim.exportName
      artifact.manifest.stringConcatPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : stringConcatPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.resultTy claim.containerTy claim.concatFuncIdx
          artifact.manifest.subject.stringHostRoles claim.symPlan plan
          claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hAccepted with
        ⟨_hExport, _hCarrier, _hRole, hHost, body, codeEntry, binding,
          _hSym, _hMatches, hCheck, hLow, _hCodeEntry, _hBinding,
          _hBindingCode, hSelf, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, 1, body⟩ := by
        simpa [hSelf, stringConcatNLocals] using hCode
      refine ⟨plan, rfl, ?_⟩
      intro add sub mul stringEq stringConcat hStringConcat fuel v w hRun
      have hHostSlot :
          claim.obligation.host add sub mul stringEq stringConcat
              claim.concatFuncIdx =
            some (1, stringConcat claim.resultTy) := by
        rw [hHost]
        simp [stringConcatCanonicalHost]
      exact V3String.generic_string_concat_certified
        claim.resultTy claim.containerTy claim.concatFuncIdx plan
        claim.obligation.code
        (claim.obligation.host add sub mul stringEq stringConcat)
        claim.obligation.self stringConcat hStringConcat hCheck body hLow
        hCodeSelf hHostSlot fuel v w hRun

theorem stringEq_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedStringEqFragments artifact)
    (claim : StringEqClaim)
    (hMem : claim ∈ artifact.stringEqClaims)
    (hBridge : ∀ plan,
      stringEqPlanForExport claim.exportName
          artifact.manifest.stringEqPlans = some plan →
        stringEqSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  have hClaim : stringEqClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (stringEqClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.stringEqClaims hAcc claim hMem
  unfold stringEqClaimAccepted at hClaim
  cases hPlan : stringEqPlanForExport claim.exportName
      artifact.manifest.stringEqPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : stringEqPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.stringTy claim.stringEqFuncIdx
          artifact.manifest.subject.stringHostRoles claim.symPlan plan
          claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hBridge plan hPlan with ⟨hPolicy, hSemantic⟩
      rcases hAccepted with
        ⟨_hExport, _hCarrier, _hRole, hHost, _hSym, _hMatches, hCheck,
          body, codeEntry, binding, hLow, _hCodeEntry, _hExportCode,
          _hBinding, hSelf, _hBindingCode, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, 2, body⟩ := by
        simpa [← hSelf] using hCode
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat
        _hAdd _hSub _hMul hStringEq _hStringConcat fuel x vs w hDom hRun
      rcases hSemantic S x vs hDom with ⟨v, hVs, hCod⟩
      subst vs
      cases fuel with
      | zero => simp [wFuncN] at hRun
      | succ fuel =>
          have hHostSlot :
              claim.obligation.host add sub mul stringEq stringConcat
                  claim.stringEqFuncIdx = some (2, stringEq) := by
            rw [hHost]
            simp [stringEqCanonicalHost]
          have hCall := V3String.generic_string_eq_certified
            claim.stringTy claim.stringEqFuncIdx plan claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            claim.obligation.self stringEq hStringEq hCheck body hLow hCodeSelf
            hHostSlot fuel v w hRun
          simpa [hCall] using hCod

theorem stringConcat_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedStringConcatFragments artifact)
    (claim : StringConcatClaim)
    (hMem : claim ∈ artifact.stringConcatClaims)
    (hBridge : ∀ plan,
      stringConcatPlanForExport claim.exportName
          artifact.manifest.stringConcatPlans = some plan →
        stringConcatSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  have hClaim : stringConcatClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (stringConcatClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.stringConcatClaims hAcc claim hMem
  unfold stringConcatClaimAccepted at hClaim
  cases hPlan : stringConcatPlanForExport claim.exportName
      artifact.manifest.stringConcatPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : stringConcatPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.resultTy claim.containerTy claim.concatFuncIdx
          artifact.manifest.subject.stringHostRoles claim.symPlan plan
          claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hBridge plan hPlan with ⟨hPolicy, hSemantic⟩
      rcases hAccepted with
        ⟨_hExport, _hCarrier, _hRole, hHost, body, codeEntry, binding,
          _hSym, _hMatches, hCheck, hLow, _hCodeEntry, _hBinding,
          _hBindingCode, hSelf, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, 1, body⟩ := by
        simpa [hSelf, stringConcatNLocals] using hCode
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat
        _hAdd _hSub _hMul _hStringEq hStringConcat fuel x vs w hDom hRun
      rcases hSemantic S x vs hDom with ⟨v, hVs, hCod⟩
      subst vs
      cases fuel with
      | zero => simp [wFuncN] at hRun
      | succ fuel =>
          have hHostSlot :
              claim.obligation.host add sub mul stringEq stringConcat
                  claim.concatFuncIdx =
                some (1, stringConcat claim.resultTy) := by
            rw [hHost]
            simp [stringConcatCanonicalHost]
          have hCall := V3String.generic_string_concat_certified
            claim.resultTy claim.containerTy claim.concatFuncIdx plan
            claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            claim.obligation.self stringConcat hStringConcat hCheck body hLow
            hCodeSelf hHostSlot fuel v w hRun
          simpa [hCall] using hCod

theorem stringEq_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedStringEqFragments artifact)
    (hSemantic : stringSemanticBridges artifact) :
    ∀ o ∈ artifact.stringEqClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact stringEq_claim_discharges artifact hAcc claim hMem
    (hSemantic.1 claim hMem)

theorem stringConcat_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedStringConcatFragments artifact)
    (hSemantic : stringSemanticBridges artifact) :
    ∀ o ∈ artifact.stringConcatClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact stringConcat_claim_discharges artifact hAcc claim hMem
    (hSemantic.2 claim hMem)

/-- Combined string-family slice.  This is the small extension to the standard
pattern: string has two accepted claim lists and two generic theorems. -/
theorem string_discharges
    (artifact : ArtifactData)
    (hEqAcc : acceptedStringEqFragments artifact)
    (hConcatAcc : acceptedStringConcatFragments artifact)
    (hSemantic : stringSemanticBridges artifact) :
    ∀ o ∈ stringClaimObligations artifact, obligationHolds o := by
  intro o hObligation
  rcases List.mem_append.mp hObligation with hEq | hConcat
  · exact stringEq_discharges artifact hEqAcc hSemantic o hEq
  · exact stringConcat_discharges artifact hConcatAcc hSemantic o hConcat

end V3Master

#print axioms V3Master.stringEq_accepted_call
#print axioms V3Master.stringConcat_accepted_call
#print axioms V3Master.stringEq_claim_discharges
#print axioms V3Master.stringConcat_claim_discharges
#print axioms V3Master.stringEq_discharges
#print axioms V3Master.stringConcat_discharges
#print axioms V3Master.string_discharges
