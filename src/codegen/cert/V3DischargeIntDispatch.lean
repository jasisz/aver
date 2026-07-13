/-
v3 master wiring — Int-dispatch-family discharge.

Acceptance supplies the checked plan, canonical host wiring, lowering, and
exact code entry.  The independent domain/model face and the generic theorem's
non-default-root premise are explicit: the audited raw checker currently checks
only the profile string and therefore cannot derive that premise.
-/
import V3Master
import V3DispatchCore

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

/-- A raw plan accepted by `checkIntDispatchRawPlan` whose body has no test
root.  This witnesses the admission premise missing from current acceptance. -/
def uncoveredIntDispatchDefaultPlan : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .default 0 }

theorem intDispatch_raw_allows_default_root :
    AverCert.PlanCheck.checkIntDispatchRawPlan
      uncoveredIntDispatchDefaultPlan = true := by
  decide

theorem intDispatch_default_root_has_no_test :
    ¬ ∃ tyIdx leaf rest,
      uncoveredIntDispatchDefaultPlan.body = .test tyIdx leaf rest := by
  simp [uncoveredIntDispatchDefaultPlan]

/-- All type indices occurring in a cascade.  The generic's family check is
unused semantically, but its premise can be reconstructed for every raw-checked
plan by choosing this complete finite list. -/
def intDispatchPinnedTypes : IntDispatchCascade → List Nat
  | .default _ => []
  | .test tyIdx _ rest => tyIdx :: intDispatchPinnedTypes rest

private theorem checkCascadeTypes_pinned_more
    (extra : List Nat) (cascade : IntDispatchCascade) :
    V3Dispatch.checkCascadeTypes
      (extra ++ intDispatchPinnedTypes cascade) cascade = true := by
  induction cascade generalizing extra with
  | default k => simp [V3Dispatch.checkCascadeTypes]
  | test tyIdx leaf rest ih =>
      simp only [intDispatchPinnedTypes, V3Dispatch.checkCascadeTypes,
        Bool.and_eq_true]
      constructor
      · simp
      · simpa [List.append_assoc] using ih (extra ++ [tyIdx])

private theorem checkCascadeTypes_pinned (cascade : IntDispatchCascade) :
    V3Dispatch.checkCascadeTypes (intDispatchPinnedTypes cascade) cascade = true := by
  simpa using checkCascadeTypes_pinned_more [] cascade

private theorem checkIntDispatchFamily_of_raw (plan : IntDispatchRawPlan)
    (hRaw : AverCert.PlanCheck.checkIntDispatchRawPlan plan = true) :
    V3Dispatch.checkIntDispatchFamily
      (intDispatchPinnedTypes plan.body) plan = true := by
  simp [V3Dispatch.checkIntDispatchFamily, hRaw, checkCascadeTypes_pinned]

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
    (C : Nat) (add sub : List WVal → Option WVal) :
    HostRole → Nat × (List WVal → Option WVal)
  | .box => (1, boxRef C)
  | .add => (2, add)
  | .sub => (2, sub)

private theorem canonicalSlot_of_lookup
    (C : Nat) (add sub : List WVal → Option WVal)
    (hostTable : List (HostRole × Nat))
    (hDistinct : AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true)
    (role : HostRole) (idx : Nat)
    (hLookup : AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx) :
    intDispatchCanonicalSlots C add sub hostTable idx =
      some (intDispatchExpectedSlot C add sub role) := by
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
          intDispatchCanonicalSlots C add sub rest idx) = _
        rw [if_neg hNe]
        exact ih hRestDistinct role idx hTailLookup

private theorem canonicalHostSlots
    (C : Nat) (add sub : List WVal → Option WVal)
    (hostTable : List (HostRole × Nat))
    (hDistinct : AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true) :
    V3Dispatch.HostSlots C
      (intDispatchCanonicalSlots C add sub hostTable) hostTable add sub := by
  constructor
  · intro idx hLookup
    simpa [intDispatchExpectedSlot] using
      canonicalSlot_of_lookup C add sub hostTable hDistinct .box idx hLookup
  · constructor
    · intro idx hLookup
      simpa [intDispatchExpectedSlot] using
        canonicalSlot_of_lookup C add sub hostTable hDistinct .add idx hLookup
    · intro idx hLookup
      simpa [intDispatchExpectedSlot] using
        canonicalSlot_of_lookup C add sub hostTable hDistinct .sub idx hLookup

/-- The semantic and generic-admission face absent from
`intDispatchPlanAccepted`.  A represented source input must expose one runtime
variant, `EvalCascade` must relate that variant to the checked plan's Int
result, and every representation of that result must satisfy the obligation's
independently declared codomain/model relation. -/
def intDispatchSemanticBridge
    (claim : IntDispatchClaim) (plan : IntDispatchRawPlan) : Prop :=
  (∃ tyIdx leaf rest, plan.body = .test tyIdx leaf rest) ∧
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (x : claim.obligation.Dom) (vs : List WVal),
    claim.obligation.domRepr S x vs →
    ∃ tag fields n,
      vs = [.structv tag fields] ∧
      V3Dispatch.EvalCascade S plan.body tag fields n ∧
      ∀ w, S.Repr n w →
        claim.obligation.codRepr S (claim.obligation.model x) w

def intDispatchSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.intDispatchClaims,
    ∀ plan,
      intDispatchPlanForExport claim.exportName
          artifact.manifest.intDispatchPlans = some plan →
        intDispatchSemanticBridge claim plan

/-- Byte/plan and generic-application half for one accepted Int-dispatch claim.
Only the generic theorem's non-default-root premise remains explicit. -/
theorem intDispatch_accepted_call
    (artifact : ArtifactData)
    (hAcc : acceptedIntDispatchFragments artifact)
    (claim : IntDispatchClaim)
    (hMem : claim ∈ artifact.intDispatchClaims)
    (hRoot : ∀ plan,
      intDispatchPlanForExport claim.exportName
          artifact.manifest.intDispatchPlans = some plan →
        ∃ tyIdx leaf rest, plan.body = .test tyIdx leaf rest) :
    ∃ plan,
      intDispatchPlanForExport claim.exportName
          artifact.manifest.intDispatchPlans = some plan ∧
      ∀ (S : CarrierSpec claim.obligation.carrier)
        (add sub mul stringEq : List WVal → Option WVal)
        (stringConcat : Nat → List WVal → Option WVal),
        (∀ a b va vb w, S.Repr a va → S.Repr b vb →
          add [va, vb] = some w → S.Repr (a + b) w) →
        (∀ a b va vb w, S.Repr a va → S.Repr b vb →
          sub [va, vb] = some w → S.Repr (a - b) w) →
        ∀ fuel tag fields n w,
          V3Dispatch.EvalCascade S plan.body tag fields n →
          wFuncN claim.obligation.code
              (claim.obligation.host add sub mul stringEq stringConcat)
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
          body, codeEntry, binding, hLow, _hCodeEntry, _hExportCode,
          _hBinding, hSelf, _hBindingCode, _hFuncType, hCode⟩
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, AverCert.PlanCheck.intDispatchArmCount plan.body + 2,
            body⟩ := by
        simpa [← hSelf] using hCode
      have hHost' : claim.obligation.host =
          intDispatchCanonicalHost claim.obligation.carrier claim.hostTable := by
        simpa [hCarrier] using hHost
      refine ⟨plan, rfl, ?_⟩
      intro S add sub mul stringEq stringConcat hAdd hSub
        fuel tag fields n w hSem hRun
      have hSlots : V3Dispatch.HostSlots claim.obligation.carrier
          (claim.obligation.host add sub mul stringEq stringConcat)
          claim.hostTable add sub := by
        rw [hHost']
        exact canonicalHostSlots claim.obligation.carrier add sub
          claim.hostTable hDistinct
      exact V3Dispatch.generic_int_dispatch_certified
        S plan claim.obligation.code
        (claim.obligation.host add sub mul stringEq stringConcat)
        claim.obligation.self claim.hostTable add sub hSlots hAdd hSub
        (intDispatchPinnedTypes plan.body)
        (checkIntDispatchFamily_of_raw plan hRaw)
        (hRoot plan hPlan) body hLow hCodeSelf
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
    (fun plan hPlan => (hBridge plan hPlan).1)
  rcases hCall with ⟨plan, hPlan, hGeneric⟩
  rcases hBridge plan hPlan with ⟨_hRoot, hPolicy, hSemantic⟩
  rw [obligationHolds, hPolicy]
  intro S add sub mul stringEq stringConcat
    hAdd hSub _hMul _hStringEq _hStringConcat fuel x vs w hDom hRun
  rcases hSemantic S x vs hDom with
    ⟨tag, fields, n, hVs, hCascade, hCod⟩
  subst vs
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      exact hCod w (hGeneric S add sub mul stringEq stringConcat hAdd hSub
        fuel tag fields n w hCascade hRun)

/-- Complete family slice under the semantic/root bridge missing from current
artifact acceptance. -/
theorem intDispatch_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedIntDispatchFragments artifact)
    (hSemantic : intDispatchSemanticBridges artifact) :
    ∀ o ∈ artifact.intDispatchClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact intDispatch_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

end V3Master

#print axioms V3Master.intDispatch_raw_allows_default_root
#print axioms V3Master.intDispatch_default_root_has_no_test
#print axioms V3Master.intDispatch_accepted_call
#print axioms V3Master.intDispatch_claim_discharges
#print axioms V3Master.intDispatch_discharges
