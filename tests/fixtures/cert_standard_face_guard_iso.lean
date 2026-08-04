import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

-- TEMPLATE, not standalone Lean. Each percent-delimited placeholder below is
-- a module layout index, and every one of them shifts as soon as
-- `tools/certkit/fixtures/cert_goals.av` gains or loses a function.
-- `standard_face_host_guard_is_isolating` in `tests/cert_verify_spec.rs`
-- reads them back out of the artifact it just compiled and substitutes them
-- here, so no module layout index may ever be written down as a literal. The
-- one bare number in a face below is the arity of the `intAdd` family, which
-- `StandardFace.classifyIntAdd` pins to a single carrier parameter.

def hostileHost : AverCert.StandardFace.HostBuilder :=
  fun _ _ _ _ _ _ _ _ _ => none

def hostileAddTwoOb : Obligation :=
  { AverCert.addTwoOb with host := hostileHost }

-- Without a canonical whole-host face, partial correctness is vacuous: the
-- first host call traps and therefore no successful run needs to be related to
-- the source model.
theorem hostileAddTwoHolds : hostileAddTwoOb.holds := by
  intro S add sub mul stringEq stringConcat toIndex cmp eq hadd hsub hmul hStringEq
    hStringConcat _hToIndex _hCmp _hEq fuel x vs w hdom hrun
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
  AverCert.ClaimAxes.checked artifact = true ∧
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
    ⟨_, hsubject, hobs, hmatch, _hfaces, haxes, hdecoded, hfragments⟩
  refine ⟨hostileFinal, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · change AcceptedArtifact.subjectMatchesArtifactRoot Artifact.data
    exact hsubject
  · dsimp [AcceptedArtifact.fragmentClaimObligationsInManifest,
      AcceptedArtifact.claimObligations, AcceptedArtifact.claimObligationsInManifest,
      hostileArtifact, hostileManifest, hostileSymFragmentClaims,
      hostileAddTwoClaim, hostileAddTwoOb]
    repeat' constructor
  · change AcceptedArtifact.claimsMatchManifest Artifact.data
    exact hmatch
  · change AverCert.ClaimAxes.checked Artifact.data = true
    exact haxes
  · change AcceptedArtifact.decodedNonExprFacts Artifact.data
    exact hdecoded
  · change AcceptedArtifact.acceptedFragments Artifact.data
    exact hfragments

-- The new guard rejects the same mutation by comparing the complete host
-- builder. Looking only at the box slot is enough to expose the contradiction.
example : ¬ StandardFace.checkedFaces hostileArtifact := by
  intro hfaces
  have hfirst := hfaces.2.1.1
  have hface := hfirst.2
  change (StandardFace.StandardFace.known
    (StandardFace.intList %carrier% 1
      (StandardFace.intAddHost %carrier%
        ({ constant := %addConstant%, boxIdx := %box%, addIdx := %add% } :
          StandardFace.IntAddFace)))).Matches hostileAddTwoOb at hface
  have hhost := hface.2.2.2.2.2.1
  have hslot := congrArg
    (fun host => (host (fun _ => none) (fun _ => none) (fun _ => none)
      (fun _ => none) (fun _ _ => none) (fun _ => none) (fun _ => none)
      (fun _ => none) %box%).map Prod.fst) hhost
  simp [hostileAddTwoOb, hostileHost, StandardFace.intList,
    StandardFace.intAddHost] at hslot

-- Role labels cannot be permuted while retaining the same helper indices.
example : StandardFace.hostTableBound manifest.subject.hostRoles
    [(.box, %box%), (.add, %add%), (.mul, %mul%), (.sub, %sub%)] = true := rfl
example : StandardFace.hostTableBound manifest.subject.hostRoles
    [(.box, %add%), (.add, %box%), (.mul, %mul%), (.sub, %sub%)] = false := rfl

-- Generic non-recursive fragments cannot smuggle a recursive call.
def genericSelfCall : FragBlock :=
  { nodes := [{ id := 0, ty := .i64, kind := .selfCall false 1 [] }]
    result := 0 }
example : StandardFace.genericFragmentAllowedFuel 2 genericSelfCall = false := rfl

-- Code-entry equality does not include the type section. The standard
-- expression face binds the exact declared parameter and result types too.
example : AverCert.WasmSlice.exprFragmentFuncTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    %addTwoTypeIdx% %carrier% [.intCarrier] .intCarrier = true := rfl
example : AverCert.WasmSlice.exprFragmentFuncTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    %addTwoTypeIdx% %carrier% [.f64] .intCarrier = false := rfl
example : AverCert.WasmSlice.exprFragmentFuncTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    %addTwoTypeIdx% %carrier% [.intCarrier] .boolI32 = false := rfl

-- Opaque projection faces are nominal: exact struct, two fields, and the
-- selected field's actual result type are decoded from the type section.
example : AverCert.WasmSlice.exprProjectionTypesMatch
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    %projectionTypeIdx% %carrier% %structIdx% %fieldIdx% = true := rfl
example : AverCert.WasmSlice.exprProjectionTypesMatch
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    %projectionTypeIdx% %structIdx% %structIdx% %fieldIdx% = false := rfl
example : AverCert.WasmSlice.exprProjectionTypesMatch
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    %projectionTypeIdx% %carrier% %structIdx% %otherFieldIdx% = false := rfl
