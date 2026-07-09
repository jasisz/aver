-- Lean-side artifact acceptance helpers.
--
-- This is the first bridge from expr-fragment byte-origin checking to the
-- schema obligation the final certificate theorem reasons about. It is still
-- per-fragment, not a full artifact predicate, but it pins the checked plan and
-- Wasm binding to `Schema.Obligation` fields instead of leaving them as loose
-- examples.
import CertPrelude
import Schema
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

end AverCert.AcceptedArtifact
