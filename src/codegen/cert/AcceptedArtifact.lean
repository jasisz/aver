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

/-- The source plans claimed by an artifact, projected into the same manifest
    surface used for pinning. Keeping this in the audited predicate means a
    self-checking artifact cannot prove acceptance for one claim list while
    advertising a different source-plan list in its manifest. -/
def symFragmentClaimPlanPairs
    (claims : List SymFragmentClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.plan))

/-- The representation plans claimed by an artifact, projected into the same
    manifest surface used for pinning. -/
def exprFragmentClaimPlanPairs
    (claims : List ExprFragmentClaim) : List (String × ExprFragmentRawPlan) :=
  claims.map (fun c => (c.exportName, c.plan))

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
  exprFragmentClaims : List ExprFragmentClaim

def acceptedSymFragments (artifact : ArtifactData) : Prop :=
  symFragmentClaimsAccepted artifact.wasmBytes artifact.symFragmentClaims

def acceptedExprFragments (artifact : ArtifactData) : Prop :=
  exprFragmentClaimsAccepted artifact.wasmBytes artifact.exprFragmentClaims

def acceptedFragments (artifact : ArtifactData) : Prop :=
  acceptedSymFragments artifact ∧
  acceptedExprFragments artifact

def claimsMatchManifest (artifact : ArtifactData) : Prop :=
  symFragmentClaimPlanPairs artifact.symFragmentClaims =
      artifact.manifest.symFragmentPlans ∧
  exprFragmentClaimPlanPairs artifact.exprFragmentClaims =
      artifact.manifest.exprFragmentPlans

def accepted (artifact : ArtifactData) : Prop :=
  AverCert.Schema.Holds artifact.manifest ∧
  claimsMatchManifest artifact ∧
  acceptedFragments artifact

end AverCert.AcceptedArtifact
