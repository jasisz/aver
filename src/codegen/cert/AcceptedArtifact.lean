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

/-- One expression-fragment claim inside an artifact certificate. The plan and
    obligation are still untrusted data until `exprFragmentClaimAccepted`
    checks them against the checker-owned artifact bytes. -/
structure ExprFragmentClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  plan            : ExprFragmentRawPlan
  obligation      : Obligation

def exprFragmentClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (claim : ExprFragmentClaim) : Prop :=
  exprFragmentPlanAccepted
    wasmBytes
    claim.exportNameBytes
    claim.exportName
    claim.carrier
    claim.plan
    claim.obligation

/-- Artifact-level acceptance for one source-level symbolic fragment export.
    The source plan is still untrusted data: the audited checker/encoder must
    accept it and produce the representation-level expr-fragment plan before
    the existing byte-origin predicate is allowed to run. -/
def symFragmentPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (plan : SymRawPlan)
    (obligation : Obligation) : Prop :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan plan with
  | some exprPlan =>
      exprFragmentPlanAccepted
        wasmBytes exportNameBytes exportName carrier exprPlan obligation
  | none => False

/-- One source-level symbolic fragment claim inside an artifact certificate.
    This is the preferred v2 shape for fragments whose meaning can already be
    stated in Aver-level terms. -/
structure SymFragmentClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
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

/-- One source-level String.concat claim inside an artifact certificate. -/
structure StringConcatClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  resultTy        : Nat
  containerTy     : Nat
  concatFuncIdx   : Nat
  symPlan         : SymRawPlan
  plan            : StringConcatRawPlan
  obligation      : Obligation

def stringConcatClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (claim : StringConcatClaim) : Prop :=
  stringConcatPlanAccepted
    wasmBytes
    claim.exportNameBytes
    claim.exportName
    claim.carrier
    claim.resultTy
    claim.containerTy
    claim.concatFuncIdx
    claim.symPlan
    claim.plan
    claim.obligation

/-- Aggregate expression-fragment acceptance for one artifact's fragment list.
    This is intentionally recursive rather than tactic-heavy, so a generated
    checker witness can prove it with a small nested pair term. -/
def exprFragmentClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq) :
    List ExprFragmentClaim → Prop
  | [] => True
  | claim :: rest =>
      exprFragmentClaimAccepted wasmBytes claim ∧
      exprFragmentClaimsAccepted wasmBytes rest

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
    (wasmBytes : AverCert.WasmSlice.ByteSeq) :
    List StringConcatClaim → Prop
  | [] => True
  | claim :: rest =>
      stringConcatClaimAccepted wasmBytes claim ∧
      stringConcatClaimsAccepted wasmBytes rest

/-- The source plans claimed by an artifact, projected into the same manifest
    surface used for pinning. Keeping this in the audited predicate means a
    self-checking artifact cannot prove acceptance for one claim list while
    advertising a different source-plan list in its manifest. -/
def symFragmentClaimPlanPairs
    (claims : List SymFragmentClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.plan))

/-- Representation plans induced by source-level claims. This is what lets a
    source-projectable fragment avoid carrying a duplicate `ExprFragmentClaim`:
    the byte-bound plan is computed by the audited encoder. -/
def symFragmentClaimEncodedPlanPair?
    (claim : SymFragmentClaim) : Option (String × ExprFragmentRawPlan) :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan claim.plan with
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

/-- The representation plans claimed by an artifact, projected into the same
    manifest surface used for pinning. -/
def exprFragmentClaimPlanPairs
    (claims : List ExprFragmentClaim) : List (String × ExprFragmentRawPlan) :=
  claims.map (fun c => (c.exportName, c.plan))

/-- The source-level String.concat plans claimed by an artifact, projected into
    the same manifest surface used for pinning. -/
def stringConcatClaimPlanPairs
    (claims : List StringConcatClaim) : List (String × StringConcatRawPlan) :=
  claims.map (fun c => (c.exportName, c.plan))

/-- The source `SymPlan`s carried by String.concat claims. These live in the
    manifest's common `symFragmentPlans` list; the byte-lowering-specific
    `StringConcatRawPlan` stays in `stringConcatPlans`. -/
def stringConcatClaimSymPlanPairs
    (claims : List StringConcatClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.symPlan))

/-- The checker-facing artifact data currently accepted by the Lean bridge.
    `symFragmentClaims` is the preferred source-level surface. Raw
    `exprFragmentClaims` remains as a fallback for representation-only fragments
    until the source grammar grows constructors for them. Ordinary legacy
    obligations are still pinned by the verifier witness outside this
    artifact-level wrapper. -/
structure ArtifactData where
  wasmBytes          : AverCert.WasmSlice.ByteSeq
  manifest           : AverCert.Schema.Manifest
  symFragmentClaims  : List SymFragmentClaim
  stringConcatClaims : List StringConcatClaim
  exprFragmentClaims : List ExprFragmentClaim

def acceptedSymFragments (artifact : ArtifactData) : Prop :=
  symFragmentClaimsAccepted artifact.wasmBytes artifact.symFragmentClaims

def acceptedStringConcatFragments (artifact : ArtifactData) : Prop :=
  stringConcatClaimsAccepted artifact.wasmBytes artifact.stringConcatClaims

def acceptedExprFragments (artifact : ArtifactData) : Prop :=
  exprFragmentClaimsAccepted artifact.wasmBytes artifact.exprFragmentClaims

def acceptedFragments (artifact : ArtifactData) : Prop :=
  acceptedSymFragments artifact ∧
  acceptedStringConcatFragments artifact ∧
  acceptedExprFragments artifact

def expectedArtifactRoot : String :=
  "AverCert.Artifact.certificate"

def subjectMatchesArtifactRoot (artifact : ArtifactData) : Prop :=
  artifact.manifest.subject.artifactRoot = expectedArtifactRoot

def claimObligationExports (artifact : ArtifactData) : List String :=
  artifact.symFragmentClaims.map (fun c => c.obligation.export_) ++
  artifact.stringConcatClaims.map (fun c => c.obligation.export_) ++
  artifact.exprFragmentClaims.map (fun c => c.obligation.export_)

def fragmentClaimObligationsInManifest (artifact : ArtifactData) : Prop :=
  (claimObligationExports artifact).all
    (fun exportName =>
      (artifact.manifest.obligations.map (fun o => o.export_)).contains exportName) = true

def claimsMatchManifest (artifact : ArtifactData) : Prop :=
  match symFragmentClaimEncodedPlanPairs artifact.symFragmentClaims with
  | some encodedSymExprPlans =>
      symFragmentClaimPlanPairs artifact.symFragmentClaims ++
          stringConcatClaimSymPlanPairs artifact.stringConcatClaims =
          artifact.manifest.symFragmentPlans ∧
      stringConcatClaimPlanPairs artifact.stringConcatClaims =
          artifact.manifest.stringConcatPlans ∧
      encodedSymExprPlans ++ exprFragmentClaimPlanPairs artifact.exprFragmentClaims =
          artifact.manifest.exprFragmentPlans
  | none => False

def accepted (artifact : ArtifactData) : Prop :=
  AverCert.Schema.Holds artifact.manifest ∧
  subjectMatchesArtifactRoot artifact ∧
  fragmentClaimObligationsInManifest artifact ∧
  claimsMatchManifest artifact ∧
  acceptedFragments artifact

end AverCert.AcceptedArtifact
