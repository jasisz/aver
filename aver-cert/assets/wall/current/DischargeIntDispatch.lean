/-
Acceptance-soundness wiring for integer dispatch.

Acceptance supplies the checked plan, canonical host wiring, lowering, and
exact code entry. The audited raw checker supplies the generic theorem's
non-default-root premise; only the independent domain/model face remains in
the semantic bridge.
-/
import AcceptanceSoundnessCore
import IntDispatchSoundness

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

namespace AcceptanceSoundness

private theorem intDispatchRoot_of_raw (plan : IntDispatchRawPlan)
    (hRaw : AverCert.PlanCheck.checkIntDispatchRawPlan plan = true) :
    ∃ tyIdx leaf rest, plan.body = .test tyIdx leaf rest := by
  cases hBody : plan.body with
  | default k =>
      simp [AverCert.PlanCheck.checkIntDispatchRawPlan, hBody] at hRaw
  | test tyIdx leaf rest =>
      exact ⟨tyIdx, leaf, rest, rfl⟩

private theorem hostRoleIdx_mem_pair
    (hostTable : List (HostRole × Nat)) (role : HostRole) (idx : Nat)
    (hLookup : AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx) :
    (role, idx) ∈ hostTable := by
  induction hostTable with
  | nil => simp [AverCert.PlanCheck.hostRoleIdx?] at hLookup
  | cons head rest ih =>
      rcases head with ⟨headRole, headIdx⟩
      by_cases hRole : headRole = role
      · subst headRole
        simp [AverCert.PlanCheck.hostRoleIdx?] at hLookup
        subst idx
        simp
      · simp [AverCert.PlanCheck.hostRoleIdx?, hRole] at hLookup
        simp [ih hLookup]

private def intDispatchExpectedSlot
    (C : Nat) (add sub mul : List WVal → Option WVal) :
    HostRole → Nat × (List WVal → Option WVal)
  | .box => (1, boxRef C)
  | .add => (2, add)
  | .mul => (2, mul)
  | .sub => (2, sub)
  | .toIndex => (1, fun _ => none)

private theorem canonicalSlot_of_lookup
    (C : Nat) (add sub mul : List WVal → Option WVal)
    (hostTable : List (HostRole × Nat))
    (hDistinct : AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true)
    (role : HostRole) (idx : Nat)
    (hLookup : AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx) :
    intDispatchCanonicalSlots C add sub mul hostTable idx =
      some (intDispatchExpectedSlot C add sub mul role) := by
  induction hostTable generalizing role idx with
  | nil => simp [AverCert.PlanCheck.hostRoleIdx?] at hLookup
  | cons head rest ih =>
      rcases head with ⟨headRole, headIdx⟩
      simp only [AverCert.PlanCheck.hostTableIndicesDistinct,
        AverCert.PlanCheck.natListNoDup, List.map_cons,
        Bool.and_eq_true] at hDistinct
      rcases hDistinct with ⟨hHeadFresh, hRestDistinct⟩
      by_cases hRole : headRole = role
      · subst headRole
        simp [AverCert.PlanCheck.hostRoleIdx?] at hLookup
        subst idx
        cases role <;>
          simp [intDispatchCanonicalSlots, intDispatchExpectedSlot]
      · have hTailLookup : AverCert.PlanCheck.hostRoleIdx? rest role = some idx := by
          simpa [AverCert.PlanCheck.hostRoleIdx?, hRole] using hLookup
        have hPairMem : (role, idx) ∈ rest :=
          hostRoleIdx_mem_pair rest role idx hTailLookup
        have hNe : idx ≠ headIdx := by
          intro hEq
          subst idx
          simp at hHeadFresh
          exact hHeadFresh role hPairMem
        change (if idx = headIdx then _ else
          intDispatchCanonicalSlots C add sub mul rest idx) = _
        rw [if_neg hNe]
        exact ih hRestDistinct role idx hTailLookup

private theorem canonicalHostSlots
    (C : Nat) (add sub mul : List WVal → Option WVal)
    (hostTable : List (HostRole × Nat))
    (hDistinct : AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true) :
    IntDispatchSoundness.HostSlots C
      (intDispatchCanonicalSlots C add sub mul hostTable) hostTable add sub := by
  constructor
  · intro idx hLookup
    simpa [intDispatchExpectedSlot] using
      canonicalSlot_of_lookup C add sub mul hostTable hDistinct .box idx hLookup
  · constructor
    · intro idx hLookup
      simpa [intDispatchExpectedSlot] using
        canonicalSlot_of_lookup C add sub mul hostTable hDistinct .add idx hLookup
    · intro idx hLookup
      simpa [intDispatchExpectedSlot] using
        canonicalSlot_of_lookup C add sub mul hostTable hDistinct .sub idx hLookup

/-- The semantic face intentionally absent from `intDispatchPlanAccepted`.
A represented source input must expose one runtime variant, `EvalCascade` must
relate that variant to the checked plan's Int result, and every representation
of that result must satisfy the independently declared codomain/model relation. -/
def intDispatchSemanticBridge
    (claim : IntDispatchClaim) (plan : IntDispatchRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (x : claim.obligation.Dom) (vs : List WVal),
    claim.obligation.domRepr S x vs →
    ∃ tag fields n,
      vs = [.structv tag fields] ∧
      IntDispatchSoundness.EvalCascade S plan.body tag fields n ∧
      ∀ w, S.Repr n w →
        claim.obligation.codRepr S (claim.obligation.model x) w

def intDispatchSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.intDispatchClaims,
    ∀ plan,
      intDispatchPlanForExport claim.exportName
          artifact.manifest.intDispatchPlans = some plan →
        intDispatchSemanticBridge claim plan

/-- Byte/plan and generic-application half for one accepted Int-dispatch claim. -/
theorem intDispatch_accepted_call
    (artifact : ArtifactData)
    (hAcc : acceptedIntDispatchFragments artifact)
    (claim : IntDispatchClaim)
    (hMem : claim ∈ artifact.intDispatchClaims) :
    ∃ plan,
      intDispatchPlanForExport claim.exportName
          artifact.manifest.intDispatchPlans = some plan ∧
      ∀ (S : CarrierSpec claim.obligation.carrier)
        (add sub mul stringEq : List WVal → Option WVal)
        (stringConcat : Nat → List WVal → Option WVal)
        (toIndex : List WVal → Option WVal),
        (∀ a b va vb w, S.Repr a va → S.Repr b vb →
          add [va, vb] = some w → S.Repr (a + b) w) →
        (∀ a b va vb w, S.Repr a va → S.Repr b vb →
          sub [va, vb] = some w → S.Repr (a - b) w) →
        ∀ fuel tag fields n w,
          IntDispatchSoundness.EvalCascade S plan.body tag fields n →
          wFuncN claim.obligation.code
              (claim.obligation.host add sub mul stringEq stringConcat toIndex)
              (fuel + 1) claim.obligation.self [.structv tag fields] = some w →
            S.Repr n w := by
  have hClaim : intDispatchClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim := by
    exact allClaims_of_mem
      (intDispatchClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.intDispatchClaims hAcc claim hMem
  unfold intDispatchClaimAccepted at hClaim
  cases hPlan : intDispatchPlanForExport claim.exportName
      artifact.manifest.intDispatchPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : intDispatchPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.hostTable plan claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hAccepted with
        ⟨_hExport, hCarrier, hRaw, hDistinct, hHost,
          body, codeEntry, binding, hLow, _hCodeEntry, _hExactBinding,
          hSelf, _hFuncType, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, AverCert.PlanCheck.bindArmCount plan.body + 2,
            body⟩ := by
        simpa [← hSelf] using hCode
      have hHost' : claim.obligation.host =
          intDispatchCanonicalHost claim.obligation.carrier claim.hostTable := by
        simpa [hCarrier] using hHost
      refine ⟨plan, rfl, ?_⟩
      intro S add sub mul stringEq stringConcat toIndex hAdd hSub
        fuel tag fields n w hSem hRun
      have hSlots : IntDispatchSoundness.HostSlots claim.obligation.carrier
          (claim.obligation.host add sub mul stringEq stringConcat toIndex)
          claim.hostTable add sub := by
        rw [hHost']
        exact canonicalHostSlots claim.obligation.carrier add sub mul
          claim.hostTable hDistinct
      exact IntDispatchSoundness.generic_int_dispatch_certified
        S plan claim.obligation.code
        (claim.obligation.host add sub mul stringEq stringConcat toIndex)
        claim.obligation.self claim.hostTable add sub hSlots hAdd hSub
        (intDispatchRoot_of_raw plan hRaw) body hLow hCodeSelf
        fuel tag fields n w hSem hRun

theorem intDispatch_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedIntDispatchFragments artifact)
    (claim : IntDispatchClaim)
    (hMem : claim ∈ artifact.intDispatchClaims)
    (hBridge : ∀ plan,
      intDispatchPlanForExport claim.exportName
          artifact.manifest.intDispatchPlans = some plan →
        intDispatchSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  have hCall := intDispatch_accepted_call artifact hAcc claim hMem
  rcases hCall with ⟨plan, hPlan, hGeneric⟩
  rcases hBridge plan hPlan with ⟨hPolicy, hSemantic⟩
  rw [obligationHolds, hPolicy]
  intro S add sub mul stringEq stringConcat toIndex
    hAdd hSub _hMul _hStringEq _hStringConcat _hToIndex fuel x vs w hDom hRun
  rcases hSemantic S x vs hDom with
    ⟨tag, fields, n, hVs, hCascade, hCod⟩
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      exact hCod w (hGeneric S add sub mul stringEq stringConcat toIndex hAdd hSub
        fuel tag fields n w hCascade hRun)

/-- Per-obligation option-(b) discharge for a concrete Int-dispatch export.
The checked plan, canonical host table, lowering, and code binding are data;
`hSemantic` is the intentionally residual source-model bridge emitted for the
user function.  Unlike the option-(c) leaf theorems, the model is not replaced
by a canonical evaluator: the bridge proves that the user's model agrees with
the byte-derived `EvalCascade` on every represented source constructor. -/
theorem intDispatch_canonical_discharges
    (exportName : String)
    (carrier self : Nat)
    (plan : IntDispatchRawPlan)
    (hostTable : List (HostRole × Nat))
    (code : CodeTbl)
    (host :
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (List WVal → Option WVal) →
      (Nat → List WVal → Option WVal) →
      (List WVal → Option WVal) → HostTbl)
    (Dom : Type)
    (domRepr : CarrierSpec carrier → Dom → List WVal → Prop)
    (model : Dom → Int)
    (hRaw : AverCert.PlanCheck.checkIntDispatchRawPlan plan = true)
    (hDistinct : AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true)
    (hHost : ∀ add sub mul stringEq stringConcat toIndex,
      host add sub mul stringEq stringConcat toIndex =
        intDispatchCanonicalHost carrier hostTable
          add sub mul stringEq stringConcat toIndex)
    (body : List WInstr)
    (hLow : AverCert.PlanLower.lowerIntDispatchBody hostTable plan = some body)
    (hCode : code self = some {
      arity := 1,
      nlocals := AverCert.PlanCheck.bindArmCount plan.body + 2,
      body := body })
    (hSemantic : ∀ (S : CarrierSpec carrier) (x : Dom) (vs : List WVal),
      domRepr S x vs →
      ∃ tag fields n,
        vs = [.structv tag fields] ∧
        IntDispatchSoundness.EvalCascade S plan.body tag fields n ∧
        ∀ w, S.Repr n w → intRepr S (model x) w) :
    Obligation.holds
      ({ export_ := exportName
         policy := .simulatesModel
         carrier := carrier
         code := code
         host := host
         self := self
         Dom := Dom
         Cod := Int
         domRepr := domRepr
         codRepr := fun S n w => intRepr S n w
         model := model } : Obligation) := by
  intro S add sub mul stringEq stringConcat toIndex
    hAdd hSub _hMul _hStringEq _hStringConcat _hToIndex fuel x vs w hDom hRun
  rcases hSemantic S x vs hDom with
    ⟨tag, fields, n, hVs, hCascade, hCod⟩
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      have hSlots : IntDispatchSoundness.HostSlots carrier
          (host add sub mul stringEq stringConcat toIndex) hostTable add sub := by
        rw [hHost]
        exact canonicalHostSlots carrier add sub mul hostTable hDistinct
      have hGeneric := IntDispatchSoundness.generic_int_dispatch_certified
        S plan code (host add sub mul stringEq stringConcat toIndex) self
        hostTable add sub hSlots hAdd hSub
        (intDispatchRoot_of_raw plan hRaw) body hLow hCode
      exact hCod w (hGeneric fuel tag fields n w hCascade hRun)

/-- Complete family slice under the residual semantic bridge. -/
theorem intDispatch_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedIntDispatchFragments artifact)
    (hSemantic : intDispatchSemanticBridges artifact) :
    ∀ o ∈ artifact.intDispatchClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact intDispatch_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

end AcceptanceSoundness
