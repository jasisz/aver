/-
v3 master wiring — unary and mutual recursion-family discharges.

The accepted-plan predicates supply policy/termination admission and the exact
selected code entry.  The independent obligation host/domain/model faces stay
explicit, following the established discharge pattern.  Mutual recursion also
needs the shared-code/SCC package: one member's acceptance constrains only its
own obligation code table, while the k-generic theorem executes every member.
-/
import V3Master
import V3RecSpike
import V3MutualGeneric

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

/-- Host, unary-domain, and source-model faces not pinned by
`recursionPlanAccepted`.  Both domain directions are needed because `holds`
starts with an arbitrary represented obligation input, whereas `holdsTotal`
starts with an arbitrary represented integer and asks the obligation to
provide its domain witness. -/
def recursionSemanticBridge
    (claim : RecursionClaim) (plan : RecursionRawPlan) : Prop :=
  ∃ boxIdx addIdx subIdx sh,
    AverCert.PlanCheck.hostRoleIdx? claim.hostTable .box = some boxIdx ∧
    AverCert.PlanCheck.hostRoleIdx? claim.hostTable .add = some addIdx ∧
    AverCert.PlanCheck.hostRoleIdx? claim.hostTable .sub = some subIdx ∧
    V3Rec.parseRecShapeU claim.obligation.self boxIdx addIdx subIdx plan = some sh ∧
    (∀ add sub mul stringEq stringConcat,
      let host := claim.obligation.host add sub mul stringEq stringConcat
      host boxIdx = some (1, boxRef claim.obligation.carrier) ∧
      host addIdx = some (2, add) ∧
      host subIdx = some (2, sub) ∧
      host claim.obligation.self = none) ∧
    (∀ (S : CarrierSpec claim.obligation.carrier)
      (x : claim.obligation.Dom) (vs : List WVal),
      claim.obligation.domRepr S x vs →
      ∃ n v, vs = [v] ∧ S.Repr n v ∧
        ∀ w, S.Repr (V3Rec.evalRecU sh n) w →
          claim.obligation.codRepr S (claim.obligation.model x) w) ∧
    (∀ (S : CarrierSpec claim.obligation.carrier) (n : Int) (v : WVal),
      S.Repr n v →
      ∃ x : claim.obligation.Dom,
        claim.obligation.domRepr S x [v] ∧
        ∀ w, S.Repr (V3Rec.evalRecU sh n) w →
          claim.obligation.codRepr S (claim.obligation.model x) w)

def recursionSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.recursionClaims,
    ∀ plan,
      recursionPlanForExport claim.exportName
          artifact.manifest.recursionPlans = some plan →
        recursionSemanticBridge claim plan

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
      rcases hAccepted with
        ⟨_hExport, hCarrier, hRaw, hTermination,
          body, codeEntry, binding, hLow, _hCodeEntry, _hExportCode,
          _hBinding, hSelf, _hBindingCode, _hShape, _hType, hCode⟩
      rcases hBridge plan hPlan with
        ⟨boxIdx, addIdx, subIdx, sh, _hBoxLookup, _hAddLookup, _hSubLookup,
          hParse, hHost, hPartialModel, hTotalModel⟩
      have hParams : plan.params = [.intCarrier] := by
        unfold V3Rec.parseRecShapeU at hParse
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
          cases hWitness : claim.obligation.termination? with
          | some witness => simp [hPolicy, hWitness] at hTermination
          | none =>
              rw [obligationHolds, hPolicy]
              intro S add sub mul stringEq stringConcat
                hAdd hSub _hMul _hStringEq _hStringConcat fuel x vs w hDom hRun
              rcases hPartialModel S x vs hDom with
                ⟨n, v, rfl, hv, hCod⟩
              rcases hHost add sub mul stringEq stringConcat with
                ⟨hBox, hAddHost, hSubHost, hSelfHost⟩
              apply hCod w
              exact V3Rec.recursion_generic_certified
                claim.obligation.carrier claim.obligation.self boxIdx addIdx
                subIdx 1 S.Repr S.car S.smallIntro S.smallElim S.bigElim
                claim.obligation.code
                (claim.obligation.host add sub mul stringEq stringConcat)
                add sub hBox hAddHost hSubHost hSelfHost hAdd hSub plan sh
                hParse body hLower hCodeSelf fuel n v w hv hRun
      | simulatesModelTotally =>
          cases hWitness : claim.obligation.termination? with
          | none => simp [hPolicy, hWitness] at hTermination
          | some witness =>
              have _hCheckedTermination : checkTerm plan witness = true := by
                simpa [hPolicy, hWitness] using hTermination
              rw [obligationHolds, hPolicy]
              intro S add sub mul stringEq stringConcat
                hAdd hSub _hMul _hStringEq _hStringConcat hAddTot hSubTot n v hv
              rcases hTotalModel S n v hv with ⟨x, hDom, hCod⟩
              rcases hHost add sub mul stringEq stringConcat with
                ⟨hBox, hAddHost, hSubHost, hSelfHost⟩
              obtain ⟨w, hRun, hRepr⟩ := V3Rec.recursion_generic_certified_total
                claim.obligation.carrier claim.obligation.self boxIdx addIdx
                subIdx 1 S.Repr S.car S.smallIntro S.smallElim S.bigElim
                claim.obligation.code
                (claim.obligation.host add sub mul stringEq stringConcat)
                add sub hBox hAddHost hSubHost hSelfHost hAdd hSub
                hAddTot hSubTot plan sh hParse body hLower hCodeSelf n v hv
              exact ⟨x, hDom, w, hRun, hCod w hRepr⟩

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
    ∃ (scc : V3Mutual.AdmittedScc k claim.obligation.carrier boxIdx subIdx)
      (i : Fin k),
    scc.plans i = plan ∧
    (scc.members i).self = claim.obligation.self ∧
    plan.params = [.intCarrier] ∧
    mutualClaimEdges artifact.manifest artifact.mutualRecursionClaims =
      some scc.rawEdges ∧
    (∀ j, j ≠ i → claim.obligation.code (scc.members j).self =
      some ⟨1, 1, V3Mutual.mutualInstrs claim.obligation.carrier
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
        ∀ w, S.Repr (V3Mutual.evalMutualU scc.members i n) w →
          claim.obligation.codRepr S (claim.obligation.model x) w) ∧
    (∀ (S : CarrierSpec claim.obligation.carrier) (n : Int) (v : WVal),
      S.Repr n v →
      ∃ x : claim.obligation.Dom,
        claim.obligation.domRepr S x [v] ∧
        ∀ w, S.Repr (V3Mutual.evalMutualU scc.members i n) w →
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
        ⟨_hExport, hCarrier, hRaw, hTermination,
          body, codeEntry, binding, hLow, _hCodeEntry, _hExportCode,
          _hBinding, hSelf, _hBindingCode, _hShape, _hType, hCode⟩
      rcases hBridge plan hPlan with
        ⟨k, boxIdx, subIdx, scc, i, hSccPlan, hSccSelf, hParams,
          hEdges, hCodeOther, hHost, hPartialModel, hTotalModel⟩
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
      have hCanonical : body = V3Mutual.mutualInstrs
          claim.obligation.carrier boxIdx subIdx scc.members i := by
        have hSccLower := scc.lowered i
        rw [hSccPlan] at hSccLower
        rw [hLower] at hSccLower
        exact Option.some.inj hSccLower
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, 1, body⟩ := by
        simpa [hParams, mutualNLocals, ← hSelf] using hCode
      have hCodeAll : ∀ j, claim.obligation.code (scc.members j).self =
          some ⟨1, 1, V3Mutual.mutualInstrs claim.obligation.carrier
            boxIdx subIdx scc.members j⟩ := by
        intro j
        by_cases hji : j = i
        · subst j
          simpa [hSccSelf, hCanonical] using hCodeSelf
        · exact hCodeOther j hji
      cases hPolicy : claim.obligation.policy with
      | simulatesModel =>
          cases hWitness : claim.obligation.termination? with
          | some witness => simp [hPolicy, hWitness] at hTermination
          | none =>
              rw [obligationHolds, hPolicy]
              intro S add sub mul stringEq stringConcat
                _hAdd hSub _hMul _hStringEq _hStringConcat fuel x vs w hDom hRun
              rcases hPartialModel S x vs hDom with
                ⟨n, v, rfl, hv, hCod⟩
              rcases hHost add sub mul stringEq stringConcat with
                ⟨hBox, hSubHost, hMemberHost⟩
              have hRun' : wFuncN claim.obligation.code
                  (claim.obligation.host add sub mul stringEq stringConcat)
                  fuel (scc.members i).self [v] = some w := by
                simpa [hSccSelf] using hRun
              apply hCod w
              simpa [hSccSelf] using V3Mutual.mutual_generic_certified
                k claim.obligation.carrier boxIdx subIdx scc S.Repr S.car
                S.smallIntro S.smallElim S.bigElim claim.obligation.code
                (claim.obligation.host add sub mul stringEq stringConcat)
                sub hBox hSubHost hMemberHost hCodeAll hSub fuel i n v w hv hRun'
      | simulatesModelTotally =>
          cases hWitness : claim.obligation.termination? with
          | none => simp [hPolicy, hWitness] at hTermination
          | some witness =>
              have _hCheckedTermination : checkTermMutual plan witness = true := by
                simpa [hPolicy, hWitness] using hTermination
              rw [obligationHolds, hPolicy]
              intro S add sub mul stringEq stringConcat
                _hAdd hSub _hMul _hStringEq _hStringConcat _hAddTot hSubTot n v hv
              rcases hTotalModel S n v hv with ⟨x, hDom, hCod⟩
              rcases hHost add sub mul stringEq stringConcat with
                ⟨hBox, hSubHost, hMemberHost⟩
              obtain ⟨w, hRun, hRepr⟩ :=
                V3Mutual.mutual_generic_certified_total
                  k claim.obligation.carrier boxIdx subIdx scc S.Repr S.car
                  S.smallIntro S.smallElim S.bigElim claim.obligation.code
                  (claim.obligation.host add sub mul stringEq stringConcat)
                  sub hBox hSubHost hMemberHost hCodeAll hSub hSubTot i n v hv
              exact ⟨x, hDom, w, by simpa [hSccSelf] using hRun, hCod w hRepr⟩

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

end V3Master

#print axioms V3Master.recursion_claim_discharges
#print axioms V3Master.recursion_discharges
#print axioms V3Master.mutual_claim_discharges
#print axioms V3Master.mutual_discharges
