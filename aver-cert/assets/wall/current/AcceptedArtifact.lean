-- Lean-side artifact acceptance shim.
--
-- The dependency-closed acceptance machinery lives in
-- `AcceptedArtifactCore.lean`; this shim adds the one conjunction that uses
-- the Module-dependent `Schema.Holds` proposition.
import Schema
import AcceptedArtifactCore
import StandardFace

namespace AverCert.AcceptedArtifact

def accepted (artifact : ArtifactData) : Prop :=
  AverCert.Schema.Holds artifact.manifest ∧
  subjectMatchesArtifactRoot artifact ∧
  fragmentClaimObligationsInManifest artifact ∧
  claimsMatchManifest artifact ∧
  AverCert.StandardFace.checkedFaces artifact ∧
  decodedNonExprFacts artifact ∧
  acceptedFragments artifact

end AverCert.AcceptedArtifact
