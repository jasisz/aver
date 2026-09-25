-- Lean-side artifact acceptance shim.
--
-- The dependency-closed acceptance machinery lives in
-- `AcceptedArtifactCore.lean`; this shim adds the one conjunction that uses
-- the Module-dependent `Schema.Holds` proposition. `AcceptanceSoundness`
-- proves that `Holds` follows from the other conjuncts
-- (`AcceptanceSoundness.accept_sound`), so a certificate discharges it with
-- that theorem rather than asserting it.
import Schema
import AcceptedArtifactCore
import ClaimAxes
import ArtifactComponentBytes

namespace AverCert.AcceptedArtifact

def accepted (artifact : ArtifactData) : Prop :=
  _root_.AverCert.Schema.Holds artifact.manifest ∧
  artifactEnvelopeAccepted _root_.AverCert.ArtifactComponentBytes.componentBytes
    _root_.AverCert.ArtifactComponentBytes.componentLen artifact = true ∧
  subjectMatchesArtifactRoot artifact ∧
  obligationsDerived artifact ∧
  plansAccepted artifact = true ∧
  decodedHostRoleTable artifact ∧
  decodedStringHostRoles artifact ∧
  _root_.AverCert.ClaimAxes.checked artifact = true ∧
  acceptedWholeModule artifact

end AverCert.AcceptedArtifact
