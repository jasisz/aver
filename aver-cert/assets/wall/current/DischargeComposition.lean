/-
Acceptance-soundness wiring for composition.

The audited acceptance predicate supplies the byte-derived root member,
closure membership, canonical lowering, and the shared obligation code table.
The semantic bridge ties the root's selected source models to the member facts
used by the generic composition theorem, preventing model-selection drift.
-/
import AcceptanceSoundnessCore
import CompositionSoundness

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

namespace AcceptanceSoundness

private theorem mem_foldl_insert (xs : List String) (s : Std.TreeSet String)
    (x : String)
    (h : x ∈ xs.foldl (fun set value => set.insert value) s) :
    x ∈ s ∨ x ∈ xs := by
  induction xs generalizing s with
  | nil => exact Or.inl h
  | cons y ys ih =>
      have h' := ih (s := s.insert y) h
      rcases h' with hset | hys
      · rw [Std.TreeSet.mem_insert] at hset
        simp only [Std.LawfulEqCmp.compare_eq_iff_eq] at hset
        rcases hset with hyx | hs
        · exact Or.inr (by simp [hyx])
        · exact Or.inl hs
      · exact Or.inr (by simp [hys])

private theorem mem_of_orderedSet_contains (xs : List String) (x : String)
    (h : (AverCert.WasmSlice.orderedSet xs).contains x = true) : x ∈ xs := by
  have hm := Std.TreeSet.contains_iff_mem.mp h
  rcases mem_foldl_insert xs Std.TreeSet.empty x hm with hempty | hxs
  · exact (Std.TreeSet.not_mem_emptyc hempty).elim
  · exact hxs

/-- A successful worklist run never drops an already-seen member. -/
private theorem compositionReachStep_preserves_seen
    (edgeIndex : Std.TreeMap String (List String))
    (memberNames : Std.TreeSet String) :
    ∀ fuel seen seenSet queuedSet work reached,
      compositionReachStep edgeIndex memberNames fuel
        seen seenSet queuedSet work = some reached →
      ∀ name ∈ seen, name ∈ reached := by
  intro fuel
  induction fuel with
  | zero =>
      intro seen seenSet queuedSet work reached h
      simp [compositionReachStep] at h
  | succ fuel ih =>
      intro seen seenSet queuedSet work reached h name hName
      cases work with
      | nil =>
          simp only [compositionReachStep, Option.some.injEq] at h
          simpa [← h] using hName
      | cons head work =>
          simp only [compositionReachStep] at h
          split at h
          next => exact ih _ _ _ _ _ h name hName
          next =>
            split at h
            next => simp at h
            next callees hLookup =>
              split at h
              next hAll =>
                exact ih _ _ _ _ _ h name (by simp [hName])
              next => simp at h

/-- Every successful nonzero closure computation retains its initial root. -/
private theorem compositionReachClosure_root_mem
    (edges : List (String × List String)) (fuel : Nat) (root : String)
    (h : compositionReachClosure edges (fuel + 1) [root] = some reached) :
    root ∈ reached := by
  change (match compositionReachStep
      (edges.foldl (fun index edge => index.insert edge.1 edge.2)
        Std.TreeMap.empty)
      (AverCert.WasmSlice.orderedSet (edges.map (fun edge => edge.1)))
      (fuel + 1) [] Std.TreeSet.empty
      (AverCert.WasmSlice.orderedSet [root]) [root] with
    | some reachedSet => some reachedSet.reverse
    | none => none) = some reached at h
  cases hStep : compositionReachStep
      (edges.foldl (fun index edge => index.insert edge.1 edge.2)
        Std.TreeMap.empty)
      (AverCert.WasmSlice.orderedSet (edges.map (fun edge => edge.1)))
      (fuel + 1) [] Std.TreeSet.empty
      (AverCert.WasmSlice.orderedSet [root]) [root] with
  | none => rw [hStep] at h; contradiction
  | some reachedSet =>
      rw [hStep] at h
      simp only [Option.some.injEq] at h
      subst reached
      simp only [List.mem_reverse]
      simp only [compositionReachStep] at hStep
      have hEmpty : (Std.TreeSet.empty : Std.TreeSet String).contains root = false := by
        exact Std.TreeSet.contains_emptyc
      simp only [hEmpty, Bool.false_eq_true, if_false] at hStep
      split at hStep
      next => simp at hStep
      next callees hLookup =>
        split at hStep
        next hAll =>
          exact compositionReachStep_preserves_seen _ _ fuel [root] _ _ _
            reachedSet hStep root (by simp)
        next => simp at hStep

/-- `compositionClosureBound` selects a chain-shaped root and puts it in the
checked member-name list. -/
private theorem compositionClosureBound_root
    (root : String) (memberNames : List String)
    (members : List CompositionMemberClaim)
    (funcTable : List (String × Nat))
    (hBound : compositionClosureBound root memberNames members funcTable = true) :
    ∃ rootMember callees,
      compositionMemberForName root members = some rootMember ∧
      rootMember.plan.shape = .chain callees ∧
      root ∈ memberNames := by
  unfold compositionClosureBound at hBound
  dsimp only at hBound
  simp only [Bool.and_eq_true] at hBound
  have hRoot := hBound.2
  cases hMember : compositionMemberForName root members with
  | none => simp [hMember] at hRoot
  | some rootMember =>
      rw [hMember] at hRoot
      cases hShape : rootMember.plan.shape with
      | selfSum => simp [hShape] at hRoot
      | chain callees =>
          simp only [hShape, Bool.true_and] at hRoot
          cases hReach : compositionReachClosure (compositionEdges members)
              (members.length + 1) [root] with
          | none => simp [hReach] at hRoot
          | some reached =>
              have hReached : root ∈ reached := by
                simpa [Nat.add_assoc] using
                  compositionReachClosure_root_mem
                    (compositionEdges members) members.length root hReach
              simp only [hReach] at hRoot
              unfold stringListSetEq at hRoot
              simp only [Bool.and_eq_true] at hRoot
              have hSetEq := hRoot.2
              unfold AverCert.WasmSlice.indexedSetEq at hSetEq
              simp only [Bool.and_eq_true] at hSetEq
              have hSubset := hSetEq.2
              unfold AverCert.WasmSlice.indexedSubset at hSubset
              have hContains := (List.all_eq_true.mp hSubset) root hReached
              exact ⟨rootMember, callees, by simp, hShape,
                mem_of_orderedSet_contains memberNames root hContains⟩

private theorem compositionNamedMemberAccepted_of_mem
    (modBytes modLen carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (funcTable : List (String × Nat))
    (obligation : Obligation)
    (members : List CompositionMemberClaim)
    (names : List String)
    (hAccepted : compositionNamedMembersAccepted modBytes modLen carrier
      hostTable funcTable obligation members names)
    (name : String) (hMem : name ∈ names) :
    ∃ member,
      compositionMemberForName name members = some member ∧
      compositionMemberPlanAccepted modBytes modLen carrier hostTable
        funcTable obligation member := by
  induction names with
  | nil => simp at hMem
  | cons head tail ih =>
      simp only [List.mem_cons] at hMem
      unfold compositionNamedMembersAccepted at hAccepted
      cases hLookup : compositionMemberForName head members with
      | none => simp [hLookup] at hAccepted
      | some member =>
          simp only [hLookup] at hAccepted
          rcases hAccepted with ⟨hHead, hTail⟩
          rcases hMem with rfl | hMem
          · exact ⟨member, hLookup, hHead⟩
          · exact ih hTail hMem

/-- A member's function-table target is exactly its byte-derived export
    binding.  Generated option-(b) bridges use this instead of re-evaluating
    the whole artifact byte parser or pinning a second function table. -/
theorem compositionFuncIdx_eq_binding
    (modBytes modLen : Nat) (members : List CompositionMemberClaim)
    (funcTable : List (String × Nat)) (name : String)
    (member : CompositionMemberClaim)
    (binding : AverCert.WasmSlice.FuncBinding)
    (hTable : compositionFuncTable modBytes modLen members = some funcTable)
    (hMember : compositionMemberForName name members = some member)
    (hBinding : AverCert.WasmSlice.funcBindingForExport
      modBytes modLen member.exportNameBytes = some binding) :
    AverCert.PlanLower.compositionFuncIdx? funcTable name =
      some binding.funcIdx := by
  induction members generalizing funcTable with
  | nil => simp [compositionMemberForName] at hMember
  | cons head tail ih =>
      unfold compositionFuncTable at hTable
      unfold compositionMemberBinding at hTable
      cases hHeadBinding : AverCert.WasmSlice.funcBindingForExport
          modBytes modLen head.exportNameBytes with
      | none => simp [hHeadBinding] at hTable
      | some headBinding =>
          cases hTailTable : compositionFuncTable modBytes modLen tail with
          | none => simp [hHeadBinding, hTailTable] at hTable
          | some tailTable =>
              simp only [hHeadBinding, hTailTable, Option.some.injEq] at hTable
              subst funcTable
              by_cases hName : head.exportName = name
              · subst name
                simp [compositionMemberForName] at hMember
                subst member
                rw [hHeadBinding] at hBinding
                have hEq : headBinding = binding := Option.some.inj hBinding
                subst binding
                simp [AverCert.PlanLower.compositionFuncIdx?]
              · have hMemberTail : compositionMemberForName name tail =
                    some member := by
                  simpa [compositionMemberForName, hName] using hMember
                have hTail := ih tailTable hTailTable hMemberTail
                have hBeq : (head.exportName == name) = false := by
                  simpa using hName
                simpa [AverCert.PlanLower.compositionFuncIdx?, hBeq] using hTail

/-- Per-obligation semantic bridge. The source model selection and member facts
    are existentially tied, so a generated bridge cannot prove facts for one
    model function and discharge a root using another. -/
def compositionClaimSemanticBridge
    (artifact : ArtifactData) (claim : CompositionClaim)
    (callees : List String) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (add sub mul stringEq : List WVal → Option WVal)
    (stringConcat : Nat → List WVal → Option WVal)
    (toIndex : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (hMul : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      mul [va, vb] = some w → S.Repr (a * b) w)
    (hStringEq : ∀ a b w, stringEq [a, b] = some w →
      w = b32 (stringEqW a b))
    (hStringConcat : ∀ resultTy parts c,
      stringConcat resultTy [parts] = some c →
        stringConcatW resultTy parts = some c)
    (hToIndex : ∀ n v r, S.Repr n v → toIndex [v] = some r →
      r = .i32v (toIndexW n))
    (x : claim.obligation.Dom) (vs : List WVal),
    claim.obligation.domRepr S x vs →
    ∃ (n : Int) (v : WVal) (models : String → Int → Int)
      (rootModel : Int → Int),
      vs = [v] ∧ S.Repr n v ∧
      (∀ input, rootModel input =
        CompositionSoundness.evalCompositionCalls models callees input) ∧
      (∀ w, S.Repr (rootModel n) w →
        claim.obligation.codRepr S (claim.obligation.model x) w) ∧
      ∀ funcTable,
        compositionFuncTable artifact.modBytes artifact.modLen
            artifact.compositionMembers = some funcTable →
        ∀ name ∈ callees,
          Nonempty (CompositionSoundness.MemberFact S artifact.compositionMembers
            funcTable claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat toIndex)
          models name)

/-- Artifact-wide form of the tied composition bridge. Each root's selected
    source models and member facts remain in one proposition. -/
def compositionClaimSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.compositionClaims,
    ∀ rootMember,
      compositionMemberForName claim.exportName artifact.compositionMembers =
          some rootMember →
      ∀ callees, rootMember.plan.shape = .chain callees →
        compositionClaimSemanticBridge artifact claim callees

/-- Per-obligation option-(b) composition discharge. Acceptance supplies the
    byte-derived root and member table; the single semantic bridge supplies a
    source model and member facts tied to that same model selection. -/
theorem composition_claim_discharges_with_bridge
    (artifact : ArtifactData)
    (claim : CompositionClaim)
    (hClaim : compositionClaimAccepted artifact.modBytes artifact.modLen
      artifact.compositionMembers claim)
    (hBridge : ∀ rootMember,
      compositionMemberForName claim.exportName artifact.compositionMembers =
          some rootMember →
      ∀ callees, rootMember.plan.shape = .chain callees →
        compositionClaimSemanticBridge artifact claim callees) :
    obligationHolds claim.obligation := by
  rcases hClaim with ⟨_hExport, hCarrier, _hHostCheck, hHost, hClaim⟩
  cases hTable : compositionFuncTable artifact.modBytes artifact.modLen
      artifact.compositionMembers with
  | none => simp [hTable] at hClaim
  | some funcTable =>
      simp only [hTable] at hClaim
      rcases hClaim with ⟨hClosure, hRootIdx, hNamed⟩
      obtain ⟨rootMember, callees, hRootLookup, hShape, hRootMem⟩ :=
        compositionClosureBound_root claim.exportName claim.memberNames
          artifact.compositionMembers funcTable hClosure
      obtain ⟨acceptedRoot, hAcceptedLookup, hRootAccepted⟩ :=
        compositionNamedMemberAccepted_of_mem artifact.modBytes artifact.modLen
          claim.carrier claim.hostTable funcTable claim.obligation
          artifact.compositionMembers claim.memberNames hNamed
          claim.exportName hRootMem
      rw [hRootLookup] at hAcceptedLookup
      have hRootEq : acceptedRoot = rootMember :=
        (Option.some.inj hAcceptedLookup).symm
      subst acceptedRoot
      clear hClosure
      rcases hRootAccepted with
        ⟨hCheck, body, codeEntry, binding, hLower, _hCodeEntry,
          hExactBinding, _hType, hCode⟩
      unfold AverCert.WasmSlice.exactFuncBindingForExport at hExactBinding
      have hBinding := Option.eq_some_of_filter_eq_some hExactBinding
      cases hTarget : AverCert.PlanLower.compositionFuncIdx?
              funcTable claim.exportName with
      | none => simp [hTarget] at hRootIdx
      | some rootIdx =>
          have hSelf : claim.obligation.self = rootIdx := by
            simpa [hTarget] using hRootIdx
          have hBindingIdx : binding.funcIdx = rootIdx := by
            have hResolved := compositionFuncIdx_eq_binding
              artifact.modBytes artifact.modLen artifact.compositionMembers
              funcTable claim.exportName rootMember binding hTable
              hRootLookup hBinding
            rw [hTarget] at hResolved
            exact (Option.some.inj hResolved).symm
          have hCodeSelf : claim.obligation.code claim.obligation.self =
              some ⟨1, compositionNLocals rootMember.plan, body⟩ := by
            simpa [hSelf, ← hBindingIdx] using hCode
          rcases hBridge rootMember hRootLookup callees hShape with
            ⟨hPolicy, hModel⟩
          rw [obligationHolds, hPolicy]
          intro S add sub mul stringEq stringConcat toIndex
            hAdd hSub hMul hStringEq hStringConcat _hToIndex fuel x vs w hDom hRun
          rcases hModel S add sub mul stringEq stringConcat toIndex
              hAdd hSub hMul hStringEq hStringConcat _hToIndex x vs hDom with
            ⟨n, v, models, rootModel, rfl, hv, hRootModel, hCod, hMembers⟩
          have hCertified := CompositionSoundness.generic_composition_certified
            S artifact.compositionMembers funcTable claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat toIndex)
            models rootModel claim.obligation.self claim.hostTable
            rootMember.plan callees hShape hCheck body hLower hCodeSelf
            (fun name hName => Classical.choice
              (hMembers funcTable hTable name hName)) hRootModel
          apply hCod
          exact hCertified fuel n v w hv hRun

/-- Family slice discharge through the tied root/member bridge consumed by the
production accept-sound capstone. -/
theorem composition_discharges_with_bridges
    (artifact : ArtifactData)
    (hAcc : acceptedCompositionFragments artifact)
    (hBridges : compositionClaimSemanticBridges artifact) :
    ∀ o ∈ artifact.compositionClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  have hClaim : compositionClaimAccepted artifact.modBytes artifact.modLen
      artifact.compositionMembers claim :=
    allClaims_of_mem
      (compositionClaimAccepted artifact.modBytes artifact.modLen
        artifact.compositionMembers)
      artifact.compositionClaims hAcc.1 claim hMem
  exact composition_claim_discharges_with_bridge artifact claim hClaim
    (hBridges claim hMem)

end AcceptanceSoundness
