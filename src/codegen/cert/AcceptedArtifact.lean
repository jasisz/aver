-- Lean-side artifact acceptance helpers.
--
-- This is the first bridge from expr-fragment byte-origin checking to the
-- schema obligation the final certificate theorem reasons about. It is still
-- per-fragment, not a full artifact predicate, but it pins the checked plan and
-- Wasm binding to `Schema.Obligation` fields instead of leaving them as loose
-- examples.
import CertPrelude
import Schema
import PlanCheck
import PlanLower
import PlanBytes
import ExprFragmentAccepted
import WasmSlice

namespace AverCert.AcceptedArtifact
open AverCert.Schema
open CertPrelude

def exprFragmentObligationAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (plan : ExprFragmentRawPlan)
    (body : List WInstr)
    (codeEntry : List Nat)
    (binding : AverCert.WasmSlice.FuncBinding)
    (obligation : Obligation) : Prop :=
  AverCert.ExprFragmentAccepted.accepted
      wasmBytes exportNameBytes carrier plan body codeEntry binding ∧
  obligation.export_ = exportName ∧
  obligation.carrier = carrier ∧
  obligation.self = binding.funcIdx ∧
  ∃ nlocals,
    obligation.code binding.funcIdx =
      some { arity := plan.params.length, nlocals := nlocals, body := body }

/-- Artifact-level acceptance for one expression-fragment export. Unlike
    `exprFragmentObligationAccepted`, this does not accept checker-rendered
    intermediate values as parameters. The body, canonical code-entry bytes, and
    function binding are witnesses to the audited Lean predicate
    `ExprFragmentAccepted.accepted`. This is the v2-shaped API: raw artifact
    bytes + raw plan + schema obligation. -/
def exprFragmentPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (plan : ExprFragmentRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
  obligation.carrier = carrier ∧
  ∃ body codeEntry binding,
    AverCert.ExprFragmentAccepted.accepted
      wasmBytes exportNameBytes carrier plan body codeEntry binding ∧
    obligation.self = binding.funcIdx ∧
    ∃ nlocals,
      obligation.code binding.funcIdx =
        some { arity := plan.params.length, nlocals := nlocals, body := body }

/-- Artifact-level acceptance for one source-level symbolic fragment export.
    The source plan is still untrusted data: the audited checker/encoder must
    accept it and produce the representation-level expr-fragment plan before
    the existing byte-origin predicate is allowed to run. -/
def symFragmentPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (structTable : List (String × Nat))
    (plan : SymRawPlan)
    (obligation : Obligation) : Prop :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      hostTable structTable plan with
  | some exprPlan =>
      exprFragmentPlanAccepted
        wasmBytes exportNameBytes exportName carrier exprPlan obligation
  | none => False

/-- One source-level symbolic fragment claim inside an artifact certificate.
    This is the preferred v2 shape for fragments whose meaning can already be
    stated in Aver-level terms. `hostTable`/`structTable` are representation
    context (the byte-derived host-role and struct-type indices), not part of
    the source plan: a wrong table encodes to a representation plan that cannot
    match the byte-bound `exprFragmentPlans` the checker pins, so the claim
    fail-closes. -/
structure SymFragmentClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  hostTable       : List (HostRole × Nat)
  structTable     : List (String × Nat)
  plan            : SymRawPlan
  obligation      : Obligation

def symFragmentClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (claim : SymFragmentClaim) : Prop :=
  symFragmentPlanAccepted
    wasmBytes
    claim.exportNameBytes
    claim.exportName
    claim.carrier
    claim.hostTable
    claim.structTable
    claim.plan
    claim.obligation

/-- Artifact-level acceptance for one String.concat export. The raw plan carries
    source-level chunks plus the encoder's data-index binding; the audited Lean
    lowerers rebuild both the semantic `WInstr` body and exact code-entry bytes,
    and the Wasm slicer binds those bytes to the exported function. -/
def stringConcatPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier resultTy containerTy concatFuncIdx : Nat)
    (symPlan : SymRawPlan)
    (plan : StringConcatRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
  obligation.carrier = carrier ∧
  ∃ body codeEntry binding,
    AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
    AverCert.PlanCheck.stringConcatPlanMatchesSymRawPlan symPlan plan = true ∧
    AverCert.PlanCheck.checkStringConcatRawPlan plan = true ∧
    AverCert.PlanLower.lowerStringConcatBody
      resultTy containerTy concatFuncIdx plan = some body ∧
    AverCert.PlanBytes.lowerStringConcatCodeEntry
      carrier resultTy containerTy concatFuncIdx plan = some codeEntry ∧
    AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
    binding.codeEntry = codeEntry ∧
    obligation.self = binding.funcIdx ∧
    ∃ nlocals,
      obligation.code binding.funcIdx =
        some { arity := 1, nlocals := nlocals, body := body }

def stringEqPlanAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier stringTy stringEqFuncIdx : Nat)
    (symPlan : SymRawPlan)
    (plan : StringEqRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
    AverCert.PlanCheck.stringEqPlanMatchesSymRawPlan symPlan plan = true ∧
    AverCert.PlanCheck.checkStringEqRawPlan plan = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerStringEqBody stringTy stringEqFuncIdx plan = some body ∧
      AverCert.PlanBytes.lowerStringEqCodeEntry carrier stringTy stringEqFuncIdx plan =
        some codeEntry ∧
      AverCert.WasmSlice.codeEntryForExport wasmBytes exportNameBytes = some codeEntry ∧
      AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
      binding.funcIdx = obligation.self ∧
      binding.codeEntry = codeEntry ∧
      obligation.code binding.funcIdx =
        some { arity := 1, nlocals := 2, body := body }

def stringEqPlanForExport
    (exportName : String) : List (String × StringEqRawPlan) →
    Option StringEqRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        stringEqPlanForExport exportName rest

def stringConcatPlanForExport
    (exportName : String) : List (String × StringConcatRawPlan) →
    Option StringConcatRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        stringConcatPlanForExport exportName rest

def constructPlanForExport
    (exportName : String) : List (String × ConstructRawPlan) →
    Option ConstructRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        constructPlanForExport exportName rest

/-- One source-level String.concat claim inside an artifact certificate. -/
structure StringConcatClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  resultTy        : Nat
  containerTy     : Nat
  concatFuncIdx   : Nat
  symPlan         : SymRawPlan
  obligation      : Obligation

/-- One source-level String.eq claim inside an artifact certificate. -/
structure StringEqClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  stringTy        : Nat
  stringEqFuncIdx : Nat
  symPlan         : SymRawPlan
  obligation      : Obligation

/-- One source-level ADT constructor claim inside an artifact certificate. The
    source `SymRawPlan` describes the Aver value being constructed; the
    target-bound `ConstructRawPlan` is taken from the manifest and checked
    against it before byte lowering. -/
structure ConstructClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  symPlan         : SymRawPlan
  obligation      : Obligation

def stringEqClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : StringEqClaim) : Prop :=
  match stringEqPlanForExport claim.exportName manifest.stringEqPlans with
  | some plan =>
      stringEqPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.stringTy
        claim.stringEqFuncIdx
        claim.symPlan
        plan
        claim.obligation
  | none => False

def stringConcatClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : StringConcatClaim) : Prop :=
  match stringConcatPlanForExport claim.exportName manifest.stringConcatPlans with
  | some plan =>
      stringConcatPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.resultTy
        claim.containerTy
        claim.concatFuncIdx
        claim.symPlan
        plan
        claim.obligation
  | none => False

def constructPlanAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (symPlan : SymRawPlan)
    (plan : ConstructRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
    AverCert.PlanCheck.constructPlanMatchesSymRawPlan symPlan plan = true ∧
    AverCert.PlanCheck.checkConstructRawPlan plan = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerConstructBody plan = some body ∧
      AverCert.PlanBytes.lowerConstructCodeEntry carrier plan = some codeEntry ∧
      AverCert.WasmSlice.codeEntryForExport wasmBytes exportNameBytes = some codeEntry ∧
      AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
      binding.funcIdx = obligation.self ∧
      binding.codeEntry = codeEntry ∧
      obligation.code binding.funcIdx =
        some { arity := plan.arity, nlocals := 1, body := body }

def constructClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : ConstructClaim) : Prop :=
  match constructPlanForExport claim.exportName manifest.constructPlans with
  | some plan =>
      constructPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.symPlan
        plan
        claim.obligation
  | none => False

/-- Aggregate source-level symbolic fragment acceptance for one artifact's
    source claim list. -/
def symFragmentClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq) :
    List SymFragmentClaim → Prop
  | [] => True
  | claim :: rest =>
      symFragmentClaimAccepted wasmBytes claim ∧
      symFragmentClaimsAccepted wasmBytes rest

/-- Aggregate source-level String.concat witness acceptance for one artifact's
    string claim list. -/
def stringConcatClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List StringConcatClaim → Prop
  | [] => True
  | claim :: rest =>
      stringConcatClaimAccepted wasmBytes manifest claim ∧
      stringConcatClaimsAccepted wasmBytes manifest rest

/-- Aggregate source-level String.eq witness acceptance for one artifact's
    string equality claim list. -/
def stringEqClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List StringEqClaim → Prop
  | [] => True
  | claim :: rest =>
      stringEqClaimAccepted wasmBytes manifest claim ∧
      stringEqClaimsAccepted wasmBytes manifest rest

/-- Aggregate source-level constructor witness acceptance for one artifact's
    constructor claim list. -/
def constructClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List ConstructClaim → Prop
  | [] => True
  | claim :: rest =>
      constructClaimAccepted wasmBytes manifest claim ∧
      constructClaimsAccepted wasmBytes manifest rest

/-- The source plans claimed by an artifact, projected into the same manifest
    surface used for pinning. Keeping this in the audited predicate means a
    self-checking artifact cannot prove acceptance for one claim list while
    advertising a different source-plan list in its manifest. -/
def symFragmentClaimPlanPairs
    (claims : List SymFragmentClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.plan))

/-- Representation plans induced by source-level claims. This is what keeps the
    artifact surface source-first: the byte-bound plan is computed by the
    audited encoder rather than carried as a separate claim. -/
def symFragmentClaimEncodedPlanPair?
    (claim : SymFragmentClaim) : Option (String × ExprFragmentRawPlan) :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some exprPlan => some (claim.exportName, exprPlan)
  | none => none

def symFragmentClaimEncodedPlanPairs :
    List SymFragmentClaim → Option (List (String × ExprFragmentRawPlan))
  | [] => some []
  | claim :: rest =>
      match symFragmentClaimEncodedPlanPair? claim,
            symFragmentClaimEncodedPlanPairs rest with
      | some pair, some pairs => some (pair :: pairs)
      | _, _ => none

def stringConcatClaimExportNames
    (claims : List StringConcatClaim) : List String :=
  claims.map (fun c => c.exportName)

def stringConcatManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.stringConcatPlans.map (fun p => p.1)

def stringEqClaimExportNames
    (claims : List StringEqClaim) : List String :=
  claims.map (fun c => c.exportName)

def stringEqManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.stringEqPlans.map (fun p => p.1)

def constructClaimExportNames
    (claims : List ConstructClaim) : List String :=
  claims.map (fun c => c.exportName)

def constructManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.constructPlans.map (fun p => p.1)

/-- The source `SymPlan`s carried by String.concat claims. These live in the
    manifest's common `symFragmentPlans` list; the byte-lowering-specific
    `StringConcatRawPlan` stays in `stringConcatPlans`. -/
def stringConcatClaimSymPlanPairs
    (claims : List StringConcatClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.symPlan))

def stringEqClaimSymPlanPairs
    (claims : List StringEqClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.symPlan))

def constructClaimSymPlanPairs
    (claims : List ConstructClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.symPlan))

/-- The checker-facing artifact data currently accepted by the Lean bridge.
    `symFragmentClaims` is the source-level expression surface. There is no
    artifact-level raw `ExprFragmentClaim`; representation plans are derived by
    the audited source encoder. Ordinary legacy obligations are still pinned by
    the verifier witness outside this artifact-level wrapper. -/
structure ArtifactData where
  wasmBytes          : AverCert.WasmSlice.ByteSeq
  manifest           : AverCert.Schema.Manifest
  symFragmentClaims  : List SymFragmentClaim
  stringEqClaims     : List StringEqClaim
  stringConcatClaims : List StringConcatClaim
  constructClaims    : List ConstructClaim

def acceptedSymFragments (artifact : ArtifactData) : Prop :=
  symFragmentClaimsAccepted artifact.wasmBytes artifact.symFragmentClaims

def acceptedStringConcatFragments (artifact : ArtifactData) : Prop :=
  stringConcatClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.stringConcatClaims

def acceptedStringEqFragments (artifact : ArtifactData) : Prop :=
  stringEqClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.stringEqClaims

def acceptedConstructFragments (artifact : ArtifactData) : Prop :=
  constructClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.constructClaims

def acceptedFragments (artifact : ArtifactData) : Prop :=
  acceptedSymFragments artifact ∧
  acceptedStringEqFragments artifact ∧
  acceptedStringConcatFragments artifact ∧
  acceptedConstructFragments artifact

def expectedArtifactRoot : String :=
  "AverCert.Artifact.certificate"

def subjectMatchesArtifactRoot (artifact : ArtifactData) : Prop :=
  artifact.manifest.subject.artifactRoot = expectedArtifactRoot

def claimObligationExports (artifact : ArtifactData) : List String :=
  artifact.symFragmentClaims.map (fun c => c.obligation.export_) ++
  artifact.stringEqClaims.map (fun c => c.obligation.export_) ++
  artifact.stringConcatClaims.map (fun c => c.obligation.export_) ++
  artifact.constructClaims.map (fun c => c.obligation.export_)

def claimObligations (artifact : ArtifactData) : List Obligation :=
  artifact.symFragmentClaims.map (fun c => c.obligation) ++
  artifact.stringEqClaims.map (fun c => c.obligation) ++
  artifact.stringConcatClaims.map (fun c => c.obligation) ++
  artifact.constructClaims.map (fun c => c.obligation)

def claimObligationsInManifest
    (manifestObligations : List Obligation) : List Obligation → Prop
  | [] => True
  | obligation :: rest =>
      manifestObligations.find?
        (fun o => o.export_ = obligation.export_) = some obligation ∧
      claimObligationsInManifest manifestObligations rest

def fragmentClaimObligationsInManifest (artifact : ArtifactData) : Prop :=
  claimObligationsInManifest
    artifact.manifest.obligations
    (claimObligations artifact)

def claimsMatchManifest (artifact : ArtifactData) : Prop :=
  match symFragmentClaimEncodedPlanPairs artifact.symFragmentClaims with
  | some encodedSymExprPlans =>
      symFragmentClaimPlanPairs artifact.symFragmentClaims ++
          stringEqClaimSymPlanPairs artifact.stringEqClaims ++
          stringConcatClaimSymPlanPairs artifact.stringConcatClaims ++
          constructClaimSymPlanPairs artifact.constructClaims =
          artifact.manifest.symFragmentPlans ∧
      stringEqClaimExportNames artifact.stringEqClaims =
          stringEqManifestPlanNames artifact.manifest ∧
      stringConcatClaimExportNames artifact.stringConcatClaims =
          stringConcatManifestPlanNames artifact.manifest ∧
      constructClaimExportNames artifact.constructClaims =
          constructManifestPlanNames artifact.manifest ∧
      encodedSymExprPlans = artifact.manifest.exprFragmentPlans
  | none => False

def accepted (artifact : ArtifactData) : Prop :=
  AverCert.Schema.Holds artifact.manifest ∧
  subjectMatchesArtifactRoot artifact ∧
  fragmentClaimObligationsInManifest artifact ∧
  claimsMatchManifest artifact ∧
  acceptedFragments artifact

end AverCert.AcceptedArtifact
