/-
Acceptance-soundness wiring for constructors.

Acceptance supplies the checked plan, canonical lowering, and exact code
entry.  The independent semantic face is kept explicit, as in the established
field-projection discharge pattern.
-/
import AcceptanceSoundnessCore
import ConstructVerbatimSoundness

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

namespace AcceptanceSoundness

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
        (ConstructVerbatimSoundness.constructModelFields
          (args ++ List.replicate 1 .null) plan.fields))

def constructSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.constructClaims,
    ∀ plan,
      constructPlanForExport claim.exportName
          artifact.manifest.constructPlans = some plan →
        constructSemanticBridge claim plan

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
  have hReadable := ConstructVerbatimSoundness.accepted_fields_readable
    plan 1 args hCheck hLen
  have hFuel := ConstructVerbatimSoundness.simNodes_construct
    host (fun g => (code g).map (·.arity))
    (fun g as => wFuncN code host fuel g as)
    structIdx (args ++ List.replicate 1 .null) plan.fields hReadable
    [.structNew structIdx plan.fields.length] []
  have hOne := ConstructVerbatimSoundness.simNodes_construct
    host (fun g => (code g).map (·.arity)) (fun _ _ => none)
    structIdx (args ++ List.replicate 1 .null) plan.fields hReadable
    [.structNew structIdx plan.fields.length] []
  simp only [wFuncN, hCode, initLocals]
  change ConstructVerbatimSoundness.outValue
      (wRunF host (fun g => (code g).map (·.arity))
        (fun g as => wFuncN code host fuel g as)
        (AverCert.PlanLower.lowerConstructFields structIdx plan.fields ++
          [.structNew structIdx plan.fields.length])
        (args ++ List.replicate 1 .null) []) =
    ConstructVerbatimSoundness.outValue
      (wRunF host (fun g => (code g).map (·.arity)) (fun _ _ => none)
        (AverCert.PlanLower.lowerConstructFields structIdx plan.fields ++
          [.structNew structIdx plan.fields.length])
        (args ++ List.replicate 1 .null) [])
  rw [hFuel, hOne]
  simp [wRunF]

/-- The single byte-to-execution seam for one accepted constructor claim.
    It exposes the selected plan and its exact result at every positive fuel;
    downstream model discharge no longer reopens artifact acceptance. -/
theorem construct_accepted_call
    (artifact : ArtifactData)
    (hAcc : acceptedConstructFragments artifact)
    (claim : ConstructClaim)
    (hMem : claim ∈ artifact.constructClaims) :
    ∃ plan,
      constructPlanForExport claim.exportName
          artifact.manifest.constructPlans = some plan ∧
      ∀ (host : HostTbl) (fuel : Nat) (args : List WVal),
        args.length = plan.arity →
        wFuncN claim.obligation.code host (fuel + 1)
            claim.obligation.self args =
          some (.structv claim.structIdx
            (ConstructVerbatimSoundness.constructModelFields
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
          body, codeEntry, binding, hLow, _hCodeEntry, _hExactBinding,
          hSelf, _hStructTy, _hFuncTy, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some { arity := plan.arity, nlocals := 1, body := body } := by
        simpa [← hSelf] using hCode
      refine ⟨plan, rfl, ?_⟩
      intro host fuel args hLen
      have hOne := ConstructVerbatimSoundness.generic_construct_certified
        claim.structIdx plan claim.obligation.code host claim.obligation.self 1
        hCheck body hLow hCodeSelf args hLen
      exact (construct_run_succ_eq_one
        claim.structIdx plan claim.obligation.code host claim.obligation.self
        hCheck body hLow hCodeSelf fuel args hLen).trans hOne

/-- Per-obligation option-(b) discharge for a concrete model-bearing
constructor export. The checked plan, canonical lowering, and code binding are
data; `hSemantic` is the intentionally residual bridge from the named source
model to the exact plan-derived constructed value. -/
theorem construct_canonical_discharges
    (exportName : String) (carrier structIdx self : Nat)
    (plan : ConstructRawPlan) (code : CodeTbl)
    (host :
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (Nat → List WVal → Option WVal) → HostTbl)
    (Dom Cod : Type)
    (domRepr : CarrierSpec carrier → Dom → List WVal → Prop)
    (codRepr : CarrierSpec carrier → Cod → WVal → Prop)
    (model : Dom → Cod)
    (hCheck : AverCert.PlanCheck.checkConstructRawPlan plan = true)
    (body : List WInstr)
    (hLow : AverCert.PlanLower.lowerConstructBody structIdx plan = some body)
    (hCode : code self = some
      { arity := plan.arity, nlocals := 1, body := body })
    (hSemantic : ∀ (S : CarrierSpec carrier) (x : Dom) (args : List WVal),
      domRepr S x args →
      args.length = plan.arity ∧
      codRepr S (model x)
        (.structv structIdx
          (ConstructVerbatimSoundness.constructModelFields
            (args ++ List.replicate 1 .null) plan.fields))) :
    Obligation.holds
      ({ export_ := exportName
         policy := .simulatesModel
         carrier := carrier
         code := code
         host := host
         self := self
         Dom := Dom
         Cod := Cod
         domRepr := domRepr
         codRepr := codRepr
         model := model } : Obligation) := by
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul _hStringEq _hStringConcat fuel x args w hDom hRun
  rcases hSemantic S x args hDom with ⟨hLen, hCod⟩
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hCall := ConstructVerbatimSoundness.generic_construct_certified
        structIdx plan code (host add sub mul stringEq stringConcat) self 1
        hCheck body hLow hCode args hLen
      have hFuel := construct_run_succ_eq_one
        structIdx plan code (host add sub mul stringEq stringConcat) self
        hCheck body hLow hCode fuel args hLen
      rw [hFuel, hCall] at hRun
      have hw :
          .structv structIdx
            (ConstructVerbatimSoundness.constructModelFields
              (args ++ List.replicate 1 .null) plan.fields) = w :=
        Option.some.inj hRun
      simpa [← hw] using hCod

/-- Canonical option-(c) leaf bridge for a unary verbatim constructor pack. -/
theorem constructUnary_canonical_discharges
    (exportName : String) (carrier structIdx self : Nat)
    (plan : ConstructRawPlan) (code : CodeTbl)
    (host :
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (Nat → List WVal → Option WVal) → HostTbl)
    (hArity : plan.arity = 1)
    (hCheck : AverCert.PlanCheck.checkConstructRawPlan plan = true)
    {body : List WInstr}
    (hLow : AverCert.PlanLower.lowerConstructBody structIdx plan = some body)
    (hCode : code self = some
      { arity := plan.arity, nlocals := 1, body := body }) :
    Obligation.holds
      ({ export_ := exportName
         policy := .simulatesModel
         carrier := carrier
         code := code
         host := host
         self := self
         Dom := WVal
         Cod := WVal
         domRepr := fun _ p vs => vs = [p]
         codRepr := fun S v w => verbatimRepr S v w
         model := fun p => .structv structIdx
           (ConstructVerbatimSoundness.constructModelFields
             ([p] ++ List.replicate 1 .null) plan.fields) } : Obligation) := by
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul _hStringEq _hStringConcat fuel p args w hDom hRun
  subst args
  have hLen : [p].length = plan.arity := by simp [hArity]
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hCall := ConstructVerbatimSoundness.generic_construct_certified
        structIdx plan code (host add sub mul stringEq stringConcat) self 1
        hCheck body hLow hCode [p] hLen
      have hFuel := construct_run_succ_eq_one
        structIdx plan code (host add sub mul stringEq stringConcat) self
        hCheck body hLow hCode fuel [p] hLen
      rw [hFuel, hCall] at hRun
      exact (Option.some.inj hRun).symm

/-- Canonical option-(c) leaf bridge for a binary verbatim constructor pack. -/
theorem constructBinary_canonical_discharges
    (exportName : String) (carrier structIdx self : Nat)
    (plan : ConstructRawPlan) (code : CodeTbl)
    (host :
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (Nat → List WVal → Option WVal) → HostTbl)
    (hArity : plan.arity = 2)
    (hCheck : AverCert.PlanCheck.checkConstructRawPlan plan = true)
    {body : List WInstr}
    (hLow : AverCert.PlanLower.lowerConstructBody structIdx plan = some body)
    (hCode : code self = some
      { arity := plan.arity, nlocals := 1, body := body }) :
    Obligation.holds
      ({ export_ := exportName
         policy := .simulatesModel
         carrier := carrier
         code := code
         host := host
         self := self
         Dom := WVal × WVal
         Cod := WVal
         domRepr := fun _ p vs => vs = [p.1, p.2]
         codRepr := fun S v w => verbatimRepr S v w
         model := fun p => .structv structIdx
           (ConstructVerbatimSoundness.constructModelFields
             ([p.1, p.2] ++ List.replicate 1 .null) plan.fields) } : Obligation) := by
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul _hStringEq _hStringConcat fuel p args w hDom hRun
  subst args
  have hLen : [p.1, p.2].length = plan.arity := by simp [hArity]
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hCall := ConstructVerbatimSoundness.generic_construct_certified
        structIdx plan code (host add sub mul stringEq stringConcat) self 1
        hCheck body hLow hCode [p.1, p.2] hLen
      have hFuel := construct_run_succ_eq_one
        structIdx plan code (host add sub mul stringEq stringConcat) self
        hCheck body hLow hCode fuel [p.1, p.2] hLen
      rw [hFuel, hCall] at hRun
      exact (Option.some.inj hRun).symm

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
  rcases construct_accepted_call artifact hAcc claim hMem with
    ⟨plan, hPlan, hCall⟩
  rcases hBridge plan hPlan with ⟨hPolicy, hSemantic⟩
  rw [obligationHolds, hPolicy]
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul _hStringEq _hStringConcat fuel x args w hDom hRun
  rcases hSemantic S x args hDom with ⟨hLen, hCod⟩
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hResult := hCall
        (claim.obligation.host add sub mul stringEq stringConcat)
        fuel args hLen
      rw [hResult] at hRun
      have hw :
          .structv claim.structIdx
            (ConstructVerbatimSoundness.constructModelFields
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

end AcceptanceSoundness

-- Compatibility diagnostic; the checker enforces axioms once at the root.
#print axioms AcceptanceSoundness.construct_canonical_discharges
