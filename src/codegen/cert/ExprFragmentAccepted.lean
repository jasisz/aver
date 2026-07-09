-- Lean-side acceptance predicate for one `expr-fragment-v1` certified export.
--
-- This intentionally aggregates the small audited pieces instead of adding a
-- second checker: structural plan check, plan -> WInstr lowering, plan -> exact
-- code-entry bytes, and wasm bytes -> export code-entry slicing.
import CertPrelude
import PlanCheck
import PlanLower
import PlanBytes
import WasmSlice

namespace AverCert.ExprFragmentAccepted
open AverCert.Schema
open CertPrelude

def accepted
    (wasmBytes exportName : AverCert.WasmSlice.ByteSeq)
    (carrier : Nat)
    (plan : ExprFragmentRawPlan)
    (body : List WInstr)
    (codeEntry : List Nat) : Prop :=
  AverCert.PlanCheck.checkExprFragmentRawPlan plan = true ∧
  AverCert.PlanLower.lowerExprFragmentBody carrier plan = some body ∧
  AverCert.PlanBytes.lowerExprFragmentCodeEntry carrier plan = some codeEntry ∧
  AverCert.WasmSlice.codeEntryForExport wasmBytes exportName = some codeEntry

end AverCert.ExprFragmentAccepted
