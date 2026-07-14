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

/-- Canonical option-(c) leaf bridge for a String.eq obligation.  The model,
helper index, checked plan, lowering, and code binding are all explicit. -/
theorem stringEq_canonical_discharges
    (exportName : String)
    (carrier stringTy stringEqFuncIdx self : Nat)
    (plan : StringEqRawPlan) (code : CodeTbl)
    (host :
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (Nat → List WVal → Option WVal) → HostTbl)
    (hCheck : AverCert.PlanCheck.checkStringEqRawPlan plan = true)
    {body : List WInstr}
    (hLow : AverCert.PlanLower.lowerStringEqBody
      stringTy stringEqFuncIdx plan = some body)
    (hCode : code self = some ⟨1, 2, body⟩)
    (hHost : ∀ add sub mul stringEq stringConcat,
      host add sub mul stringEq stringConcat stringEqFuncIdx =
        some (2, stringEq)) :
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
         model := fun v => V3String.evalStringEq stringTy plan v } :
        Obligation) := by
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul hStringEq _hStringConcat fuel v vs w hDom hRun
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hCall := V3String.generic_string_eq_certified
        stringTy stringEqFuncIdx plan code
        (host add sub mul stringEq stringConcat) self stringEq hStringEq
        hCheck body hLow hCode (hHost add sub mul stringEq stringConcat)
        fuel v w hRun
      simpa [verbatimRepr] using hCall

/-- Canonical option-(c) leaf bridge for a String.concat obligation. -/
theorem stringConcat_canonical_discharges
    (exportName : String)
    (carrier resultTy containerTy concatFuncIdx self : Nat)
    (plan : StringConcatRawPlan) (code : CodeTbl)
    (host :
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (Nat → List WVal → Option WVal) → HostTbl)
    (hCheck : AverCert.PlanCheck.checkStringConcatRawPlan plan = true)
    {body : List WInstr}
    (hLow : AverCert.PlanLower.lowerStringConcatBody
      resultTy containerTy concatFuncIdx plan = some body)
    (hCode : code self = some ⟨1, 1, body⟩)
    (hHost : ∀ add sub mul stringEq stringConcat,
      host add sub mul stringEq stringConcat concatFuncIdx =
        some (1, stringConcat resultTy)) :
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
         model := fun v =>
           V3String.evalStringConcat resultTy containerTy plan v } :
        Obligation) := by
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul _hStringEq hStringConcat fuel v vs w hDom hRun
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hCall := V3String.generic_string_concat_certified
        resultTy containerTy concatFuncIdx plan code
        (host add sub mul stringEq stringConcat) self stringConcat hStringConcat
        hCheck body hLow hCode (hHost add sub mul stringEq stringConcat)
        fuel v w hRun
      simpa [verbatimRepr] using hCall

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
  rcases stringEq_accepted_call artifact hAcc claim hMem with
    ⟨plan, hPlan, hCall⟩
  rcases hBridge plan hPlan with ⟨hPolicy, hSemantic⟩
  rw [obligationHolds, hPolicy]
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul hStringEq _hStringConcat fuel x vs w hDom hRun
  rcases hSemantic S x vs hDom with ⟨v, hVs, hCod⟩
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hResult := hCall add sub mul stringEq stringConcat
        hStringEq fuel v w hRun
      simpa [hResult] using hCod

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
  rcases stringConcat_accepted_call artifact hAcc claim hMem with
    ⟨plan, hPlan, hCall⟩
  rcases hBridge plan hPlan with ⟨hPolicy, hSemantic⟩
  rw [obligationHolds, hPolicy]
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul _hStringEq hStringConcat fuel x vs w hDom hRun
  rcases hSemantic S x vs hDom with ⟨v, hVs, hCod⟩
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hResult := hCall add sub mul stringEq stringConcat
        hStringConcat fuel v w hRun
      simpa [hResult] using hCod

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

end V3Master
