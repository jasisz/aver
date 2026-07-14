import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

def hostileHost : AverCert.StandardFace.HostBuilder :=
  fun _ _ _ _ _ _ => none

def hostileAddTwoOb : Obligation :=
  { AverCert.addTwoOb with host := hostileHost }

-- Without a canonical whole-host face, partial correctness is vacuous: the
-- first host call traps and therefore no successful run needs to be related to
-- the source model.
theorem hostileAddTwoHolds : hostileAddTwoOb.holds := by
  intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat
    fuel x vs w hdom hrun
  change wFuncN CertModule.addTwoCode (fun _ => none) fuel 1 vs = some w at hrun
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ fuel =>
      simp [wFuncN, CertModule.addTwoCode, wRunF, initLocals] at hrun

def hostileAddTwoClaim : AcceptedArtifact.SymFragmentClaim :=
  { Artifact.symFragmentClaims.get ⟨0, by decide⟩ with
      obligation := hostileAddTwoOb }

def hostileSymFragmentClaims : List AcceptedArtifact.SymFragmentClaim :=
  hostileAddTwoClaim :: Artifact.symFragmentClaims.drop 1

def hostileManifest : Manifest :=
  { manifest with obligations := hostileAddTwoOb :: manifest.obligations.tail }

def hostileArtifact : AcceptedArtifact.ArtifactData :=
  { Artifact.data with
      manifest := hostileManifest
      symFragmentClaims := hostileSymFragmentClaims }

-- This is the previous acceptance spine, with exactly the new face guard
-- omitted. All byte, claim, manifest, and fragment checks remain present.
def acceptedWithoutStandardFaces
    (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  Schema.Holds artifact.manifest ∧
  AcceptedArtifact.subjectMatchesArtifactRoot artifact ∧
  AcceptedArtifact.fragmentClaimObligationsInManifest artifact ∧
  AcceptedArtifact.claimsMatchManifest artifact ∧
  AcceptedArtifact.decodedNonExprFacts artifact ∧
  AcceptedArtifact.acceptedFragments artifact

theorem hostileFinal : Schema.Holds hostileManifest := by
  refine ⟨Final.cert.1, ?_⟩
  intro obligation hmem
  have hcases : obligation = hostileAddTwoOb ∨
      obligation ∈ manifest.obligations.tail := by
    simpa [hostileManifest] using hmem
  rcases hcases with rfl | htail
  · exact hostileAddTwoHolds
  · apply Final.cert.2 obligation
    have hmanifest : manifest.obligations =
        AverCert.addTwoOb :: manifest.obligations.tail := by rfl
    rw [hmanifest]
    exact List.mem_cons_of_mem _ htail

-- Every old guard accepts the host mutation.
example : acceptedWithoutStandardFaces hostileArtifact := by
  rcases Artifact.certificate with
    ⟨_, hsubject, hobs, hmatch, _hfaces, hdecoded, hfragments⟩
  refine ⟨hostileFinal, ?_, ?_, ?_, ?_, ?_⟩
  · change AcceptedArtifact.subjectMatchesArtifactRoot Artifact.data
    exact hsubject
  · dsimp [AcceptedArtifact.fragmentClaimObligationsInManifest,
      AcceptedArtifact.claimObligations, AcceptedArtifact.claimObligationsInManifest,
      hostileArtifact, hostileManifest, hostileSymFragmentClaims,
      hostileAddTwoClaim, hostileAddTwoOb]
    repeat' constructor
  · change AcceptedArtifact.claimsMatchManifest Artifact.data
    exact hmatch
  · change AcceptedArtifact.decodedNonExprFacts Artifact.data
    exact hdecoded
  · change AcceptedArtifact.acceptedFragments Artifact.data
    exact hfragments

-- The new guard rejects the same mutation by comparing the complete host
-- builder. Looking only at the box slot is enough to expose the contradiction.
example : ¬ StandardFace.checkedFaces hostileArtifact := by
  intro hfaces
  have hfirst := hfaces.2.1.1
  change (StandardFace.StandardFace.known
    (StandardFace.intList 23 1
      (StandardFace.intAddHost 23
        ({ constant := 2, boxIdx := 34, addIdx := 35 } :
          StandardFace.IntAddFace)))).Matches hostileAddTwoOb at hfirst
  have hhost := hfirst.2.2.2.2.2.1
  have hslot := congrArg
    (fun host => (host (fun _ => none) (fun _ => none) (fun _ => none)
      (fun _ => none) (fun _ _ => none) 34).map Prod.fst) hhost
  simp [hostileAddTwoOb, hostileHost, StandardFace.intList,
    StandardFace.intAddHost] at hslot
