/-
Acceptance-soundness wiring for unary, accumulator, and mutual recursion.

The accepted-plan predicates supply policy/termination admission and the exact
selected code entry.  The independent obligation host/domain/model faces stay
explicit, following the established discharge pattern.  Mutual recursion also
needs the shared-code/SCC package: one member's acceptance constrains only its
own obligation code table, while the k-generic theorem executes every member.
-/
import AcceptanceSoundnessCore
import RecursionSoundness
import MutualRecursionSoundness

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

namespace AcceptanceSoundness

/-- Host, unary-domain, and source-model faces not pinned by
`recursionPlanAccepted`.  The domain bridge decomposes every represented
obligation input and relates the generic evaluator to the source model. -/
def unaryRecursionSemanticBridge
    (claim : RecursionClaim) (plan : RecursionRawPlan) : Prop :=
  ∃ combineOp boxIdx combineIdx subIdx sh,
    RecursionSoundness.parseRecShapeU combineOp claim.obligation.self boxIdx combineIdx subIdx plan = some sh ∧
    claim.obligation.totalityRole =
      (match combineOp with | .add => .addSub | .mul => .mul) ∧
    (∀ add sub mul stringEq stringConcat,
      let host := claim.obligation.host add sub mul stringEq stringConcat
      host boxIdx = some (1, boxRef claim.obligation.carrier) ∧
      (match combineOp with
       | .add => host combineIdx = some (2, add)
       | .mul => host combineIdx = some (2, mul)) ∧
      host subIdx = some (2, sub) ∧
      host claim.obligation.self = none) ∧
    (∀ (S : CarrierSpec claim.obligation.carrier)
      (x : claim.obligation.Dom) (vs : List WVal),
      claim.obligation.domRepr S x vs →
      ∃ n v, vs = [v] ∧ S.Repr n v ∧
        ∀ w, S.Repr (RecursionSoundness.evalRecU combineOp sh n) w →
          claim.obligation.codRepr S (claim.obligation.model x) w)

/-- Host, arity-two domain, and source-model faces for the accumulator shape.
    The domain bridge pins the exact `[counter, accumulator]` ordering. -/
def accumulatorRecursionSemanticBridge
    (claim : RecursionClaim) (plan : RecursionRawPlan) : Prop :=
  ∃ boxIdx addIdx subIdx sh,
    RecursionSoundness.parseRecShapeA claim.obligation.self boxIdx addIdx subIdx plan = some sh ∧
    claim.obligation.totalityRole = .addSub ∧
    (∀ add sub mul stringEq stringConcat,
      let host := claim.obligation.host add sub mul stringEq stringConcat
      host boxIdx = some (1, boxRef claim.obligation.carrier) ∧
      host addIdx = some (2, add) ∧
      host subIdx = some (2, sub) ∧
      host claim.obligation.self = none) ∧
    (∀ (S : CarrierSpec claim.obligation.carrier)
      (x : claim.obligation.Dom) (vs : List WVal),
      claim.obligation.domRepr S x vs →
      ∃ n acc vn vacc,
        vs = [vn, vacc] ∧ S.Repr n vn ∧ S.Repr acc vacc ∧
        ∀ w, S.Repr (RecursionSoundness.evalRecA n acc) w →
          claim.obligation.codRepr S (claim.obligation.model x) w)

def recursionSemanticBridge
    (claim : RecursionClaim) (plan : RecursionRawPlan) : Prop :=
  unaryRecursionSemanticBridge claim plan ∨
  accumulatorRecursionSemanticBridge claim plan

def recursionSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.recursionClaims,
    ∀ plan,
      recursionPlanForExport claim.exportName
          artifact.manifest.recursionPlans = some plan →
        recursionSemanticBridge claim plan

theorem unary_recursion_claim_discharges
    (artifact : ArtifactData)
    (claim : RecursionClaim)
    (plan : RecursionRawPlan)
    (hAccepted : recursionPlanAccepted
      artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
      claim.carrier claim.hostTable plan claim.obligation)
    (hBridge : unaryRecursionSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  rcases hAccepted with
    ⟨_hExport, hCarrier, hRaw, _hTermination,
      body, codeEntry, binding, hLow, _hCodeEntry, _hExactBinding,
      hSelf, _hShape, _hType, hCode⟩
  rcases hBridge with
    ⟨combineOp, boxIdx, combineIdx, subIdx, sh,
      hParse, hTotalityRole, hHost, hModel⟩
  have hParams : plan.params = [.intCarrier] := by
    unfold RecursionSoundness.parseRecShapeU at hParse
    split at hParse
    next h => exact h.2.1
    next => simp at hParse
  have hLower : AverCert.PlanLower.lowerBlock claim.obligation.carrier
      plan.body = some body := by
    simpa [hCarrier, AverCert.PlanLower.lowerRecursionBody, hRaw] using hLow
  have hCodeSelf : claim.obligation.code claim.obligation.self =
      some ⟨1, 1, body⟩ := by
    simpa [hParams, recursionNLocals, ← hSelf] using hCode
  cases hPolicy : claim.obligation.policy with
  | simulatesModel =>
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat
        hAdd hSub _hMul _hStringEq _hStringConcat fuel x vs w hDom hRun
      rcases hModel S x vs hDom with ⟨n, v, rfl, hv, hCod⟩
      rcases hHost add sub mul stringEq stringConcat with
        ⟨hBox, hCombineHost, hSubHost, hSelfHost⟩
      apply hCod w
      cases combineOp with
      | add =>
          exact RecursionSoundness.recursion_generic_certified
            claim.obligation.carrier .add claim.obligation.self boxIdx
            combineIdx subIdx 1 S claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            add sub hBox hCombineHost hSubHost hSelfHost hAdd hSub plan sh
            hParse body hLower hCodeSelf fuel n v w hv hRun
      | mul =>
          exact RecursionSoundness.recursion_generic_certified
            claim.obligation.carrier .mul claim.obligation.self boxIdx
            combineIdx subIdx 1 S claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            mul sub hBox hCombineHost hSubHost hSelfHost _hMul hSub plan sh
            hParse body hLower hCodeSelf fuel n v w hv hRun
  | simulatesModelTotally =>
      cases combineOp with
      | add =>
          rw [obligationHolds, hPolicy]
          simp only [Obligation.holdsTotal, hTotalityRole]
          intro S add sub mul stringEq stringConcat
            hAdd hSub _hMul _hStringEq _hStringConcat
            hAddTot hSubTot x vs hDom
          rcases hModel S x vs hDom with ⟨n, v, rfl, hv, hCod⟩
          rcases hHost add sub mul stringEq stringConcat with
            ⟨hBox, hCombineHost, hSubHost, hSelfHost⟩
          obtain ⟨w, hRun, hRepr⟩ :=
            RecursionSoundness.recursion_generic_certified_total
              claim.obligation.carrier .add claim.obligation.self boxIdx
              combineIdx subIdx 1 S claim.obligation.code
              (claim.obligation.host add sub mul stringEq stringConcat)
              add sub hBox hCombineHost hSubHost hSelfHost hAdd hSub
              hAddTot hSubTot plan sh hParse body hLower hCodeSelf n v hv
          exact ⟨n, v, [], rfl, hv, w, hRun, hCod w hRepr⟩
      | mul =>
          rw [obligationHolds, hPolicy]
          simp only [Obligation.holdsTotal, hTotalityRole]
          intro S add sub mul stringEq stringConcat
            _hAdd hSub hMul _hStringEq _hStringConcat
            _hAddTot hSubTot hMulTot x vs hDom
          rcases hModel S x vs hDom with ⟨n, v, rfl, hv, hCod⟩
          rcases hHost add sub mul stringEq stringConcat with
            ⟨hBox, hCombineHost, hSubHost, hSelfHost⟩
          obtain ⟨w, hRun, hRepr⟩ :=
            RecursionSoundness.recursion_generic_certified_total
              claim.obligation.carrier .mul claim.obligation.self boxIdx
              combineIdx subIdx 1 S claim.obligation.code
              (claim.obligation.host add sub mul stringEq stringConcat)
              mul sub hBox hCombineHost hSubHost hSelfHost hMul hSub
              hMulTot hSubTot plan sh hParse body hLower hCodeSelf n v hv
          exact ⟨n, v, [], rfl, hv, w, hRun, hCod w hRepr⟩

theorem accumulator_recursion_claim_discharges
    (artifact : ArtifactData)
    (claim : RecursionClaim)
    (plan : RecursionRawPlan)
    (hAccepted : recursionPlanAccepted
      artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
      claim.carrier claim.hostTable plan claim.obligation)
    (hBridge : accumulatorRecursionSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  rcases hAccepted with
    ⟨_hExport, hCarrier, hRaw, _hTermination,
      body, codeEntry, binding, hLow, _hCodeEntry, _hExactBinding,
      hSelf, _hShape, _hType, hCode⟩
  rcases hBridge with
    ⟨boxIdx, addIdx, subIdx, sh, hParse, hTotalityRole, hHost, hModel⟩
  have hParams : plan.params = [.intCarrier, .intCarrier] := by
    unfold RecursionSoundness.parseRecShapeA at hParse
    split at hParse
    next h => exact h.2.1
    next => simp at hParse
  have hLower : AverCert.PlanLower.lowerBlock claim.obligation.carrier
      plan.body = some body := by
    simpa [hCarrier, AverCert.PlanLower.lowerRecursionBody, hRaw] using hLow
  have hCodeSelf : claim.obligation.code claim.obligation.self =
      some ⟨2, 1, body⟩ := by
    simpa [hParams, recursionNLocals, ← hSelf] using hCode
  cases hPolicy : claim.obligation.policy with
  | simulatesModel =>
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat
        hAdd hSub _hMul _hStringEq _hStringConcat fuel x vs w hDom hRun
      rcases hModel S x vs hDom with
        ⟨n, acc, vn, vacc, rfl, hvn, hvacc, hCod⟩
      rcases hHost add sub mul stringEq stringConcat with
        ⟨hBox, hAddHost, hSubHost, hSelfHost⟩
      apply hCod w
      exact RecursionSoundness.recursion_accumulator_generic_certified
        claim.obligation.carrier claim.obligation.self boxIdx addIdx
        subIdx 1 S claim.obligation.code
        (claim.obligation.host add sub mul stringEq stringConcat)
        add sub hBox hAddHost hSubHost hSelfHost hAdd hSub plan sh
        hParse body hLower hCodeSelf fuel n acc vn vacc w hvn hvacc hRun
  | simulatesModelTotally =>
      rw [obligationHolds, hPolicy]
      simp only [Obligation.holdsTotal, hTotalityRole]
      intro S add sub mul stringEq stringConcat
        hAdd hSub _hMul _hStringEq _hStringConcat
        hAddTot hSubTot x vs hDom
      rcases hModel S x vs hDom with
        ⟨n, acc, vn, vacc, rfl, hvn, hvacc, hCod⟩
      rcases hHost add sub mul stringEq stringConcat with
        ⟨hBox, hAddHost, hSubHost, hSelfHost⟩
      obtain ⟨w, hRun, hRepr⟩ :=
        RecursionSoundness.recursion_accumulator_generic_certified_total
          claim.obligation.carrier claim.obligation.self boxIdx addIdx
          subIdx 1 S claim.obligation.code
          (claim.obligation.host add sub mul stringEq stringConcat)
          add sub hBox hAddHost hSubHost hSelfHost hAdd hSub
          hAddTot hSubTot plan sh hParse body hLower hCodeSelf
          n acc vn vacc hvn hvacc
      exact ⟨n, vn, [vacc], rfl, hvn, w, hRun, hCod w hRepr⟩

theorem recursion_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedRecursionFragments artifact)
    (claim : RecursionClaim)
    (hMem : claim ∈ artifact.recursionClaims)
    (hBridge : ∀ plan,
      recursionPlanForExport claim.exportName
          artifact.manifest.recursionPlans = some plan →
        recursionSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  have hClaim : recursionClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim :=
    allClaims_of_mem
      (recursionClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.recursionClaims hAcc claim hMem
  unfold recursionClaimAccepted at hClaim
  cases hPlan : recursionPlanForExport claim.exportName
      artifact.manifest.recursionPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : recursionPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.hostTable plan claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hBridge plan hPlan with hUnary | hAccumulator
      · exact unary_recursion_claim_discharges
          artifact claim plan hAccepted hUnary
      · exact accumulator_recursion_claim_discharges
          artifact claim plan hAccepted hAccumulator

theorem recursion_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedRecursionFragments artifact)
    (hSemantic : recursionSemanticBridges artifact) :
    ∀ o ∈ artifact.recursionClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact recursion_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

/-- The cross-member faces absent from `mutualPlanAccepted`.  `scc` is the
k-generic conjunction package tied to the exact selected plan and to the raw
edge list computed from this artifact.  `codeOther` is necessary because
acceptance for another claim constrains that other claim's obligation code
table, not the selected obligation's shared table. -/
def mutualSemanticBridge
    (artifact : ArtifactData) (claim : MutualRecursionClaim)
    (plan : MutualRawPlan) : Prop :=
  ∃ k boxIdx subIdx,
    ∃ (scc : MutualRecursionSoundness.AdmittedScc k claim.obligation.carrier boxIdx subIdx)
      (i : Fin k),
    scc.plans i = plan ∧
    (scc.members i).self = claim.obligation.self ∧
    plan.params = [.intCarrier] ∧
    mutualClaimEdges artifact.manifest artifact.mutualRecursionClaims =
      some scc.rawEdges ∧
    (∀ j, j ≠ i → claim.obligation.code (scc.members j).self =
      some ⟨1, 1, MutualRecursionSoundness.mutualInstrs claim.obligation.carrier
        boxIdx subIdx scc.members j⟩) ∧
    (∀ add sub mul stringEq stringConcat,
      let host := claim.obligation.host add sub mul stringEq stringConcat
      host boxIdx = some (1, boxRef claim.obligation.carrier) ∧
      host subIdx = some (2, sub) ∧
      (∀ j, host (scc.members j).self = none)) ∧
    (∀ (S : CarrierSpec claim.obligation.carrier)
      (x : claim.obligation.Dom) (vs : List WVal),
      claim.obligation.domRepr S x vs →
      ∃ n v, vs = [v] ∧ S.Repr n v ∧
        ∀ w, S.Repr (MutualRecursionSoundness.evalMutualU scc.members i n) w →
          claim.obligation.codRepr S (claim.obligation.model x) w)

def mutualSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.mutualRecursionClaims,
    ∀ plan,
      mutualPlanForExport claim.exportName artifact.manifest.mutualPlans =
          some plan →
        mutualSemanticBridge artifact claim plan

theorem mutual_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedMutualRecursionFragments artifact)
    (claim : MutualRecursionClaim)
    (hMem : claim ∈ artifact.mutualRecursionClaims)
    (hBridge : ∀ plan,
      mutualPlanForExport claim.exportName artifact.manifest.mutualPlans =
          some plan →
        mutualSemanticBridge artifact claim plan) :
    obligationHolds claim.obligation := by
  have hClaim : mutualRecursionClaimAccepted artifact.modBytes artifact.modLen
      artifact.manifest claim :=
    allClaims_of_mem
      (mutualRecursionClaimAccepted artifact.modBytes artifact.modLen artifact.manifest)
      artifact.mutualRecursionClaims hAcc.1 claim hMem
  unfold mutualRecursionClaimAccepted at hClaim
  cases hPlan : mutualPlanForExport claim.exportName
      artifact.manifest.mutualPlans with
  | none => simp [hPlan] at hClaim
  | some plan =>
      have hAccepted : mutualPlanAccepted
          artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
          claim.carrier claim.memberSet claim.hostTable plan claim.obligation := by
        simpa [hPlan] using hClaim
      rcases hAccepted with
        ⟨_hExport, hCarrier, hTotalityRole, hRaw, _hTermination,
          body, codeEntry, binding, hLow, _hCodeEntry, _hExactBinding,
          hSelf, _hShape, _hType, hCode⟩
      rcases hBridge plan hPlan with
        ⟨k, boxIdx, subIdx, scc, i, hSccPlan, hSccSelf, hParams,
          hEdges, hCodeOther, hHost, hModel⟩
      have hArtifactClosed :
          mutualMembersFormClosedSccs scc.rawEdges = true := by
        have hClosed := hAcc.2
        unfold mutualClaimsFormClosedSccs at hClosed
        rw [hEdges] at hClosed
        exact hClosed
      have _hSameClosedProof :
          mutualMembersFormClosedSccs scc.rawEdges = true := scc.closed
      have hLower : AverCert.PlanLower.lowerMutualBody claim.obligation.carrier
          plan = some body := by
        simpa [hCarrier] using hLow
      have hCanonical : body = MutualRecursionSoundness.mutualInstrs
          claim.obligation.carrier boxIdx subIdx scc.members i := by
        have hSccLower := scc.lowered i
        rw [hSccPlan] at hSccLower
        rw [hLower] at hSccLower
        exact Option.some.inj hSccLower
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, 1, body⟩ := by
        simpa [hParams, mutualNLocals, ← hSelf] using hCode
      have hCodeAll : ∀ j, claim.obligation.code (scc.members j).self =
          some ⟨1, 1, MutualRecursionSoundness.mutualInstrs claim.obligation.carrier
            boxIdx subIdx scc.members j⟩ := by
        intro j
        by_cases hji : j = i
        · subst j
          simpa [hSccSelf, hCanonical] using hCodeSelf
        · exact hCodeOther j hji
      cases hPolicy : claim.obligation.policy with
      | simulatesModel =>
          rw [obligationHolds, hPolicy]
          intro S add sub mul stringEq stringConcat
            _hAdd hSub _hMul _hStringEq _hStringConcat fuel x vs w hDom hRun
          rcases hModel S x vs hDom with ⟨n, v, rfl, hv, hCod⟩
          rcases hHost add sub mul stringEq stringConcat with
            ⟨hBox, hSubHost, hMemberHost⟩
          have hRun' : wFuncN claim.obligation.code
              (claim.obligation.host add sub mul stringEq stringConcat)
              fuel (scc.members i).self [v] = some w := by
            simpa [hSccSelf] using hRun
          apply hCod w
          simpa [hSccSelf] using MutualRecursionSoundness.mutual_generic_certified
            k claim.obligation.carrier boxIdx subIdx scc S claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat)
            sub hBox hSubHost hMemberHost hCodeAll hSub fuel i n v w hv hRun'
      | simulatesModelTotally =>
          rw [obligationHolds, hPolicy]
          simp only [Obligation.holdsTotal, hTotalityRole]
          intro S add sub mul stringEq stringConcat
            _hAdd hSub _hMul _hStringEq _hStringConcat
            _hAddTot hSubTot x vs hDom
          rcases hModel S x vs hDom with ⟨n, v, rfl, hv, hCod⟩
          rcases hHost add sub mul stringEq stringConcat with
            ⟨hBox, hSubHost, hMemberHost⟩
          obtain ⟨w, hRun, hRepr⟩ :=
            MutualRecursionSoundness.mutual_generic_certified_total
              k claim.obligation.carrier boxIdx subIdx scc S claim.obligation.code
              (claim.obligation.host add sub mul stringEq stringConcat)
              sub hBox hSubHost hMemberHost hCodeAll hSub hSubTot i n v hv
          exact ⟨n, v, [], rfl, hv, w,
            by simpa [hSccSelf] using hRun, hCod w hRepr⟩

theorem mutual_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedMutualRecursionFragments artifact)
    (hSemantic : mutualSemanticBridges artifact) :
    ∀ o ∈ artifact.mutualRecursionClaims.map (·.obligation),
      obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact mutual_claim_discharges artifact hAcc claim hMem
    (hSemantic claim hMem)

end AcceptanceSoundness

-- Compatibility diagnostics; the checker enforces axioms once at the root.
#print axioms AcceptanceSoundness.recursion_claim_discharges
#print axioms AcceptanceSoundness.mutual_claim_discharges
