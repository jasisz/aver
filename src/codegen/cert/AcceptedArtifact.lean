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

end AverCert.AcceptedArtifact
