/-
v3 ACCEPT-SOUND ASSEMBLY.

The audited byte predicates do not constrain the semantic faces of an
`Obligation` (`policy`, `Dom`, `Cod`, representations, and `model`).  The
family discharge theorems therefore expose those faces as semantic bridge
predicates.  Composition also exposes the facts for its called members.
`dischargeSideConditions` collects precisely those remaining assumptions;
everything else in this file is the mechanical ten-family assembly.
-/
import V3DischargeExprFragment
import V3DischargeFieldProj
import V3DischargeConstruct
import V3DischargeVerbatim
import V3DischargeString
import V3DischargeIntDispatch
import V3DischargeRecursion
import V3DischargeComposition

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact

namespace V3Master

/-- Semantic premises still exposed by the ten kernel-clean family discharge
theorems.  String equality and concatenation share `stringSemanticBridges`;
composition additionally needs semantic facts for each called member. -/
def dischargeSideConditions (artifact : ArtifactData) : Prop :=
  exprFragmentSemanticBridges artifact ∧
  stringSemanticBridges artifact ∧
  constructSemanticBridges artifact ∧
  recursionSemanticBridges artifact ∧
  mutualSemanticBridges artifact ∧
  verbatimSemanticBridges artifact ∧
  intDispatchSemanticBridges artifact ∧
  fieldProjectionSemanticBridges artifact ∧
  compositionSemanticBridges artifact ∧
  compositionMemberDischarges artifact

/-- Every claimed obligation holds.  Membership in the audited concatenation
is split into its ten family slices, then discharged by the matching generic
family theorem. -/
theorem hClaims_of_accepted
    (artifact : ArtifactData)
    (hAccepted : acceptedFragments artifact)
    (hSide : dischargeSideConditions artifact) :
    ∀ o ∈ claimObligations artifact, obligationHolds o := by
  rcases hAccepted with
    ⟨hSym, hStringEq, hStringConcat, hConstruct, hRecursion, hMutual,
      hVerbatim, hIntDispatch, hFieldProjection, hComposition, _⟩
  rcases hSide with
    ⟨hExprSemantic, hStringSemantic, hConstructSemantic, hRecursionSemantic,
      hMutualSemantic, hVerbatimSemantic, hIntDispatchSemantic,
      hFieldProjectionSemantic, hCompositionSemantic, hCompositionMembers⟩
  intro o ho
  simp only [claimObligations, List.mem_append] at ho
  rcases ho with ho | ho
  · rcases ho with ho | ho
    · rcases ho with ho | ho
      · rcases ho with ho | ho
        · rcases ho with ho | ho
          · rcases ho with ho | ho
            · rcases ho with ho | ho
              · rcases ho with ho | ho
                · rcases ho with ho | ho
                  · exact exprFragment_discharges artifact hSym hExprSemantic o ho
                  · exact stringEq_discharges artifact hStringEq hStringSemantic o ho
                · exact stringConcat_discharges artifact hStringConcat
                    hStringSemantic o ho
              · exact construct_discharges artifact hConstruct
                  hConstructSemantic o ho
            · exact recursion_discharges artifact hRecursion hRecursionSemantic o ho
          · exact mutual_discharges artifact hMutual hMutualSemantic o ho
        · exact verbatim_discharges artifact hVerbatim hVerbatimSemantic o ho
      · exact intDispatch_discharges artifact hIntDispatch
          hIntDispatchSemantic o ho
    · exact fieldProjection_discharges artifact hFieldProjection
        hFieldProjectionSemantic o ho
  · exact composition_discharges artifact hComposition hCompositionSemantic
      hCompositionMembers o ho

/-- The faithful accept-sound capstone.  Manifest coverage and export
uniqueness are recovered from `acceptedCompositionFragments`, which is an
unconditional slice of `acceptedFragments`.  The artifact hash remains an
explicit premise because no byte-acceptance predicate currently relates the
manifest's hash string to the caller-supplied audited wasm hash. -/
theorem accept_sound
    (wasmSha256 : String)
    (artifact : ArtifactData)
    (hHash : artifact.manifest.subject.artifactHash = wasmSha256)
    (hInManifest : fragmentClaimObligationsInManifest artifact)
    (hAccepted : acceptedFragments artifact)
    (hSide : dischargeSideConditions artifact) :
    holdsAtHash wasmSha256 artifact.manifest := by
  have hAcceptedParts := hAccepted
  rcases hAcceptedParts with
    ⟨_, _, _, _, _, _, _, _, _, hComposition, _⟩
  rcases hComposition with ⟨_, _, hCover, hUnique⟩
  exact ⟨hHash, holdsCore_of_claims artifact hCover hInManifest hUnique
    (hClaims_of_accepted artifact hAccepted hSide)⟩

/-- With the two residual seams supplied, the originally stated master target
follows.  Its subject-root, manifest-plan, and decoded-byte hypotheses are
stronger acceptance-context facts not needed by the final logical step. -/
theorem masterTarget_of_sideConditions
    (wasmSha256 : String)
    (artifact : ArtifactData)
    (hHash : artifact.manifest.subject.artifactHash = wasmSha256)
    (hSide : dischargeSideConditions artifact) :
    masterTarget wasmSha256 artifact := by
  intro _ hInManifest _ _ hAccepted _
  exact accept_sound wasmSha256 artifact hHash hInManifest hAccepted hSide

end V3Master

#print axioms V3Master.hClaims_of_accepted
#print axioms V3Master.accept_sound
