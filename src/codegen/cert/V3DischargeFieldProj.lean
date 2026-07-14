/-
v3 master wiring — field-projection discharge spike.

This file deliberately separates the byte/plan theorem, which follows from
`acceptedFieldProjectionFragments`, from the semantic-face bridge required to
turn that theorem into `Obligation.holds`.
-/
import V3Master
import V3FieldProj

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

namespace V3Master

/-- The semantic face which the raw field-projection acceptance predicate does
not currently carry.  It says that represented inputs expose a two-field
struct and that the generic theorem's exact projected `WVal` represents the
obligation's independently declared model result. -/
def fieldProjectionSemanticBridge
    (claim : FieldProjectionClaim) (plan : FieldProjectionRawPlan) : Prop :=
  claim.fieldCount = 2 ∧
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (x : claim.obligation.Dom) (vs : List WVal),
    claim.obligation.domRepr S x vs →
    ∃ a b,
      vs = [.structv claim.structIdx [a, b]] ∧
      claim.obligation.codRepr S (claim.obligation.model x)
        (V3FieldProj.pairProjection plan.fieldIdx a b)

/-- Artifact-wide semantic bridges for all field-projection claims.  The plan
is selected by the same manifest lookup used by
`fieldProjectionClaimAccepted`, preventing a bridge from naming a different
field index. -/
def fieldProjectionSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.fieldProjectionClaims,
    ∀ plan,
      fieldProjectionPlanForExport claim.exportName
          artifact.manifest.fieldProjectionPlans = some plan →
        fieldProjectionSemanticBridge claim plan

/-- Byte/plan half of the family discharge.  Acceptance alone recovers the
checked manifest plan, canonical lowering, and the obligation's exact code
entry; the family generic then proves the raw projected result at fuel one. -/
theorem fieldProjection_accepted_call
    (artifact : ArtifactData)
    (hAcc : acceptedFieldProjectionFragments artifact)
    (claim : FieldProjectionClaim)
    (hMem : claim ∈ artifact.fieldProjectionClaims)
    (hTwo : claim.fieldCount = 2) :
    ∃ plan,
      fieldProjectionPlanForExport claim.exportName
          artifact.manifest.fieldProjectionPlans = some plan ∧
      ∀ (host : HostTbl) (a b : WVal),
        wFuncN claim.obligation.code host 1 claim.obligation.self
            [.structv claim.structIdx [a, b]] =
          some (V3FieldProj.pairProjection plan.fieldIdx a b) := by
  have hClaim : fieldProjectionClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (fieldProjectionClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.fieldProjectionClaims hAcc claim hMem
  unfold fieldProjectionClaimAccepted at hClaim
  cases hPlan : fieldProjectionPlanForExport claim.exportName
      artifact.manifest.fieldProjectionPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : fieldProjectionPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.structIdx claim.fieldCount claim.resultTy plan
          claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hAccepted with
        ⟨_hExport, _hCarrier, hCheck, body, codeEntry, binding,
          hLow, _hCodeEntry, _hBinding, _hBindingCode, _hStructTy,
          _hFuncTy, hSelf, hCode⟩
      have hCheckTwo : AverCert.PlanCheck.checkFieldProjectionRawPlan 2 plan = true := by
        simpa [hTwo] using hCheck
      have hLowTwo : AverCert.PlanLower.lowerFieldProjectionBody
          claim.structIdx 2 plan = some body := by
        simpa [hTwo] using hLow
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some { arity := 1, nlocals := 3, body := body } := by
        simpa [hSelf] using hCode
      refine ⟨plan, rfl, ?_⟩
      intro host a b
      exact V3FieldProj.generic_field_projection_certified
        claim.structIdx plan claim.obligation.code host claim.obligation.self
        hCheckTwo body hLowTwo hCodeSelf a b

private theorem fieldProjection_run_succ_eq_one
    (structIdx : Nat) (plan : FieldProjectionRawPlan)
    (code : CodeTbl) (host : HostTbl) (self : Nat)
    (hCheck : AverCert.PlanCheck.checkFieldProjectionRawPlan 2 plan = true)
    (body : List WInstr)
    (hLow : AverCert.PlanLower.lowerFieldProjectionBody structIdx 2 plan = some body)
    (hCode : code self = some { arity := 1, nlocals := 3, body := body })
    (fuel : Nat) (a b : WVal) :
    wFuncN code host (fuel + 1) self [.structv structIdx [a, b]] =
      wFuncN code host 1 self [.structv structIdx [a, b]] := by
  cases plan with
  | mk profile fieldIdx =>
      have hCanonical :
          [.localGet 0, .localSet 2, .localGet 2, .refCast structIdx,
            .structGet structIdx fieldIdx, .localSet 1, .localGet 1] = body := by
        rw [AverCert.PlanLower.lowerFieldProjectionBody, hCheck] at hLow
        simpa using hLow
      subst body
      simp [wFuncN, hCode, initLocals, wRunF]

/-- Canonical option-(c) leaf bridge for one field projection.  The obligation
face is fully canonical: a represented pair is lowered to a two-field struct,
the result is represented verbatim, and the model is the checked plan's pair
projection.  Artifact-specific callers supply only the reducible plan check and
code-table binding; no per-obligation semantic proof remains. -/
theorem fieldProjection_canonical_discharges
    (exportName : String)
    (carrier structIdx self : Nat)
    (plan : FieldProjectionRawPlan)
    (code : CodeTbl)
    (host :
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (Nat → List WVal → Option WVal) → HostTbl)
    (hCheck : AverCert.PlanCheck.checkFieldProjectionRawPlan 2 plan = true)
    (hCode : code self = some {
      arity := 1
      nlocals := 3
      body := [.localGet 0, .localSet 2, .localGet 2, .refCast structIdx,
        .structGet structIdx plan.fieldIdx, .localSet 1, .localGet 1] }) :
    Obligation.holds
      ({ export_ := exportName
         policy := .simulatesModel
         carrier := carrier
         code := code
         host := host
         self := self
         Dom := WVal × WVal
         Cod := WVal
         domRepr := fun _ p vs => vs = [.structv structIdx [p.1, p.2]]
         codRepr := fun S v w => verbatimRepr S v w
         model := fun p => V3FieldProj.pairProjection plan.fieldIdx p.1 p.2 } :
        Obligation) := by
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul _hStringEq _hStringConcat fuel p vs w hDom hRun
  rcases p with ⟨a, b⟩
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      let body : List WInstr :=
        [.localGet 0, .localSet 2, .localGet 2, .refCast structIdx,
          .structGet structIdx plan.fieldIdx, .localSet 1, .localGet 1]
      have hLow : AverCert.PlanLower.lowerFieldProjectionBody structIdx 2 plan =
          some body := by
        simp [body, AverCert.PlanLower.lowerFieldProjectionBody, hCheck]
      have hCall := V3FieldProj.generic_field_projection_certified
        structIdx plan code (host add sub mul stringEq stringConcat) self
        hCheck body hLow hCode a b
      have hFuel := fieldProjection_run_succ_eq_one
        structIdx plan code (host add sub mul stringEq stringConcat) self
        hCheck body hLow hCode fuel a b
      rw [hFuel, hCall] at hRun
      exact (Option.some.inj hRun).symm

/-- Canonical option-(c) leaf bridge for the projection-faced expression
fragment lowering.  This is the direct two-instruction sibling of
`fieldProjection_canonical_discharges`: the checked fragment loads its sole
struct argument and projects field zero or one without the legacy spill/cast
spine. -/
theorem fieldProjection_direct_canonical_discharges
    (exportName : String)
    (carrier structIdx self fieldIdx : Nat)
    (code : CodeTbl)
    (host :
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (Nat → List WVal → Option WVal) → HostTbl)
    (hField : fieldIdx < 2)
    (hCode : code self = some {
      arity := 1
      nlocals := 1
      body := [.localGet 0, .structGet structIdx fieldIdx] }) :
    Obligation.holds
      ({ export_ := exportName
         policy := .simulatesModel
         carrier := carrier
         code := code
         host := host
         self := self
         Dom := WVal × WVal
         Cod := WVal
         domRepr := fun _ p vs => vs = [.structv structIdx [p.1, p.2]]
         codRepr := fun S v w => verbatimRepr S v w
         model := fun p => V3FieldProj.pairProjection fieldIdx p.1 p.2 } :
        Obligation) := by
  intro S add sub mul stringEq stringConcat
    _hAdd _hSub _hMul _hStringEq _hStringConcat fuel p vs w hDom hRun
  rcases p with ⟨a, b⟩
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      cases fieldIdx with
      | zero =>
          simpa [V3FieldProj.pairProjection, verbatimRepr] using
            (Option.some.inj (by
              simpa [wFuncN, hCode, initLocals, wRunF] using hRun)).symm
      | succ fieldIdx =>
          cases fieldIdx with
          | zero =>
              simpa [V3FieldProj.pairProjection, verbatimRepr] using
                (Option.some.inj (by
                  simpa [wFuncN, hCode, initLocals, wRunF] using hRun)).symm
          | succ fieldIdx => omega

/-- One accepted claim discharges once its semantic face is tied to the checked
projection plan.  This is the complete reusable per-claim proof shape. -/
theorem fieldProjection_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedFieldProjectionFragments artifact)
    (claim : FieldProjectionClaim)
    (hMem : claim ∈ artifact.fieldProjectionClaims)
    (hBridge : ∀ plan,
      fieldProjectionPlanForExport claim.exportName
          artifact.manifest.fieldProjectionPlans = some plan →
        fieldProjectionSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  unfold fieldProjectionSemanticBridge at hBridge
  have hClaim : fieldProjectionClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (fieldProjectionClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.fieldProjectionClaims hAcc claim hMem
  unfold fieldProjectionClaimAccepted at hClaim
  cases hPlan : fieldProjectionPlanForExport claim.exportName
      artifact.manifest.fieldProjectionPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : fieldProjectionPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.structIdx claim.fieldCount claim.resultTy plan
          claim.obligation := by
        simpa [hPlan] using hClaim
      have hSem := hBridge plan hPlan
      rcases hSem with ⟨hTwo, hPolicy, hSemantic⟩
      rcases hAccepted with
        ⟨_hExport, _hCarrier, hCheck, body, codeEntry, binding,
          hLow, _hCodeEntry, _hBinding, _hBindingCode, _hStructTy,
          _hFuncTy, hSelf, hCode⟩
      have hCheckTwo : AverCert.PlanCheck.checkFieldProjectionRawPlan 2 plan = true := by
        simpa [hTwo] using hCheck
      have hLowTwo : AverCert.PlanLower.lowerFieldProjectionBody
          claim.structIdx 2 plan = some body := by
        simpa [hTwo] using hLow
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some { arity := 1, nlocals := 3, body := body } := by
        simpa [hSelf] using hCode
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat
        _hAdd _hSub _hMul _hStringEq _hStringConcat fuel x vs w hDom hRun
      rcases hSemantic S x vs hDom with ⟨a, b, hVs, hCod⟩
      subst vs
      cases fuel with
      | zero => simp [wFuncN] at hRun
      | succ fuel =>
          have hCall := V3FieldProj.generic_field_projection_certified
            claim.structIdx plan claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            claim.obligation.self hCheckTwo body hLowTwo hCodeSelf a b
          have hFuel := fieldProjection_run_succ_eq_one
            claim.structIdx plan claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            claim.obligation.self hCheckTwo body hLowTwo hCodeSelf fuel a b
          rw [hFuel, hCall] at hRun
          have hw : V3FieldProj.pairProjection plan.fieldIdx a b = w :=
            Option.some.inj hRun
          simpa [← hw] using hCod

/-- Family slice discharge with the currently missing semantic-face seam made
explicit.  `acceptedFieldProjectionFragments` supplies every byte/plan fact;
`fieldProjectionSemanticBridges` supplies only the independent model face. -/
theorem fieldProjection_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedFieldProjectionFragments artifact)
    (hSemantic : fieldProjectionSemanticBridges artifact) :
    ∀ o ∈ artifact.fieldProjectionClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact fieldProjection_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

end V3Master
