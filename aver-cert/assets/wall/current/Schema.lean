-- AverCert statement schema shim (audited, fixed).
--
-- The single final certificate theorem is
--   `AverCert.Final.cert : AverCert.Schema.Holds manifest`.
-- The dependency-closed schema lives in `SchemaCore.lean`; this shim adds the
-- one conjunct that binds it to the artifact-specific `Module.lean` data.
import Module
import SchemaCore

namespace AverCert.Schema

/-- The single audited certificate proposition: the manifest's pinned hash is
    the module hash, its target/profile/ABI are this wall's fixed wasm-gc
    identities, and every certified export satisfies the partial or total
    model-simulation denotation selected by its policy. -/
def Holds (m : Manifest) : Prop :=
  m.subject.artifactHash = CertModule.wasmSha256 ∧
  m.subject.target = expectedArtifactTarget ∧
  m.subject.profile = expectedProfile ∧
  m.subject.abi = expectedRuntimeAbi ∧
  HoldsCore m

end AverCert.Schema
