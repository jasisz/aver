/-
ACCEPTANCE-SOUNDNESS ASSEMBLY.

The audited byte predicates do not constrain the semantic faces of an
`Obligation` (`policy`, `Dom`, `Cod`, representations, and `model`).  The
family discharge theorems therefore expose those faces as semantic bridge
predicates.  For the Int-dispatch and named-ADT constructor families those
bridges are no longer producer premises: the checked standard faces carry
declared-index envelope pins.  String.concat likewise derives its bridge from a
checked standard face, but its ABI/type-section evidence is carried by
`stringConcatPlanAccepted` rather than by a synthetic declared envelope.
`dischargeSideConditions` collects precisely the remaining assumptions;
everything else in this file is the mechanical ten-family assembly.
-/
import DischargeExprFragment
import DischargeFieldProjection
import DischargeConstruct
import DischargeVerbatim
import DischargeString
import DischargeIntDispatch
import DischargeRecursion
import DischargeComposition
import StandardFace
import DeclaredEnvelopeAcceptTransport

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact

namespace AcceptanceSoundness

/-- The String.eq half of `stringSemanticBridges`; the String.concat half is
derived from the checked declared-envelope face. -/
def stringEqSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.stringEqClaims,
    ∀ plan,
      stringEqPlanForExport claim.exportName
          artifact.manifest.stringEqPlans = some plan →
        stringEqSemanticBridge claim plan

/-- The non-named slice of `constructSemanticBridges`: bridges for the wall
canonical `List` constructor packs. Named user-ADT constructor bridges are
derived from the checked declared-envelope face. -/
def constructListSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.constructClaims,
    (∀ name, claim.symPlan.result ≠ .named name) →
    ∀ plan,
      constructPlanForExport claim.exportName
          artifact.manifest.constructPlans = some plan →
        constructSemanticBridge claim plan

/-- Semantic premises still exposed by the kernel-clean family discharge
theorems after the declared-envelope faces took over the Int-dispatch,
named-ADT constructor, and String.concat bridges. Composition uses the tied
root/member bridge so a root's selected source models cannot be separated
from the member facts that discharge its calls. -/
def dischargeSideConditions (artifact : ArtifactData) : Prop :=
  exprFragmentSemanticBridges artifact ∧
  stringEqSemanticBridges artifact ∧
  constructListSemanticBridges artifact ∧
  recursionSemanticBridges artifact ∧
  mutualSemanticBridges artifact ∧
  verbatimSemanticBridges artifact ∧
  fieldProjectionSemanticBridges artifact ∧
  compositionClaimSemanticBridges artifact

/-! ### Face-derived semantic bridges

Each lemma peels the family slice out of `StandardFace.checkedFaces`, reduces
the checked plan lookup, and hands the checked face to the matching transport,
which emits the exact residual bridge body the family discharge consumes. -/

theorem intDispatch_bridges_of_faces
    (artifact : ArtifactData)
    (hFaces : AverCert.StandardFace.checkedFaces artifact) :
    intDispatchSemanticBridges artifact := by
  obtain ⟨-, -, -, -, -, -, -, -, hInt, -, -⟩ := hFaces
  intro claim hMem plan hPlan
  have hMatch := allClaims_of_mem _ artifact.intDispatchClaims hInt claim hMem
  obtain ⟨-, hArm⟩ := hMatch
  rw [hPlan] at hArm
  obtain ⟨hPolicy, -, typePrefix, env, hFace⟩ := hArm
  exact AverCert.DeclaredIndexEnvelope.face_gives_declaredIntDispatch_bridge
    artifact.modBytes artifact.modLen typePrefix env plan claim.obligation
    hFace hPolicy

theorem stringConcat_bridges_of_faces
    (artifact : ArtifactData)
    (hFaces : AverCert.StandardFace.checkedFaces artifact) :
    ∀ claim ∈ artifact.stringConcatClaims,
      ∀ plan,
        stringConcatPlanForExport claim.exportName
            artifact.manifest.stringConcatPlans = some plan →
          stringConcatSemanticBridge claim plan := by
  obtain ⟨-, -, -, hConcat, -, -, -, -, -, -, -⟩ := hFaces
  intro claim hMem plan hPlan
  have hMatch := allClaims_of_mem _ artifact.stringConcatClaims hConcat claim hMem
  rw [AverCert.StandardFace.stringConcatMatches, hPlan] at hMatch
  obtain ⟨-, -, typePrefix, env, hFace⟩ := hMatch
  exact AverCert.DeclaredIndexEnvelope.face_gives_declaredStringConcat_bridge
    artifact.modBytes artifact.modLen typePrefix env claim.resultTy
    claim.containerTy plan claim.obligation hFace

theorem construct_bridges_of_faces
    (artifact : ArtifactData)
    (hFaces : AverCert.StandardFace.checkedFaces artifact)
    (hList : constructListSemanticBridges artifact) :
    constructSemanticBridges artifact := by
  obtain ⟨-, -, -, -, hConstruct, -, -, -, -, -, -⟩ := hFaces
  intro claim hMem plan hPlan
  have hMatch := allClaims_of_mem _ artifact.constructClaims hConstruct claim hMem
  rw [AverCert.StandardFace.constructMatches, hPlan] at hMatch
  cases hRes : claim.symPlan.result with
  | named name =>
      rw [hRes] at hMatch
      obtain ⟨-, -, -, typePrefix, env, hhit, hFace⟩ := hMatch
      exact AverCert.DeclaredIndexEnvelope.face_gives_declaredConstruct_bridge
        artifact.modBytes artifact.modLen typePrefix env claim.structIdx hhit
        plan claim.obligation hFace
  | int => exact hList claim hMem (fun name h => by simp [hRes] at h) plan hPlan
  | float => exact hList claim hMem (fun name h => by simp [hRes] at h) plan hPlan
  | bool => exact hList claim hMem (fun name h => by simp [hRes] at h) plan hPlan
  | string => exact hList claim hMem (fun name h => by simp [hRes] at h) plan hPlan
  | app1 name arg =>
      exact hList claim hMem (fun n h => by simp [hRes] at h) plan hPlan
  | app2 name left right =>
      exact hList claim hMem (fun n h => by simp [hRes] at h) plan hPlan

/-- Every claimed obligation holds.  Membership in the audited concatenation
is split into its ten family slices, then discharged by the matching generic
family theorem.  The Int-dispatch, named-ADT constructor, and String.concat
bridges are derived from the checked declared-envelope faces; the remaining
families consume the explicit side conditions. -/
theorem hClaims_of_accepted
    (artifact : ArtifactData)
    (hFaces : AverCert.StandardFace.checkedFaces artifact)
    (hAccepted : acceptedFragments artifact)
    (hSide : dischargeSideConditions artifact) :
    ∀ o ∈ claimObligations artifact, obligationHolds o := by
  rcases hAccepted with
    ⟨hSym, hStringEq, hStringConcat, hConstruct, hRecursion, hMutual,
      hVerbatim, hIntDispatch, hFieldProjection, hComposition, _⟩
  rcases hSide with
    ⟨hExprSemantic, hStringEqSemantic, hConstructListSemantic,
      hRecursionSemantic, hMutualSemantic, hVerbatimSemantic,
      hFieldProjectionSemantic, hCompositionSemantic⟩
  have hStringSemantic : stringSemanticBridges artifact :=
    ⟨hStringEqSemantic, stringConcat_bridges_of_faces artifact hFaces⟩
  have hConstructSemantic : constructSemanticBridges artifact :=
    construct_bridges_of_faces artifact hFaces hConstructListSemantic
  have hIntDispatchSemantic : intDispatchSemanticBridges artifact :=
    intDispatch_bridges_of_faces artifact hFaces
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
                  · exact exprFragment_discharges artifact hSym hFaces.2.1
                      hExprSemantic o ho
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
  · exact composition_discharges_with_bridges artifact hComposition hCompositionSemantic
      o ho

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
    (hFaces : AverCert.StandardFace.checkedFaces artifact)
    (hAccepted : acceptedFragments artifact)
    (hSide : dischargeSideConditions artifact) :
    holdsAtHash wasmSha256 artifact.manifest := by
  have hAcceptedParts := hAccepted
  rcases hAcceptedParts with
    ⟨_, _, _, _, _, _, _, _, _, hComposition, _⟩
  rcases hComposition with ⟨_, _, hCover, hUnique⟩
  exact ⟨hHash, holdsCore_of_claims artifact hCover hInManifest hUnique
    (hClaims_of_accepted artifact hFaces hAccepted hSide)⟩

#print axioms intDispatch_bridges_of_faces
#print axioms stringConcat_bridges_of_faces
#print axioms construct_bridges_of_faces
#print axioms accept_sound

end AcceptanceSoundness
