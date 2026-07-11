-- Lean-side acceptance predicate for one `expr-fragment-v1` certified export.
--
-- This intentionally aggregates the small audited pieces instead of adding a
-- second checker: structural plan check, plan -> WInstr lowering, plan -> exact
-- code-entry bytes, and wasm bytes -> export/function binding.
import CertPrelude
import PlanCheck
import PlanLower
import PlanBytes
import WasmSlice

namespace AverCert.ExprFragmentAccepted
open AverCert.Schema
open CertPrelude

def accepted
    (modBytes modLen : Nat)
    (exportName : AverCert.WasmSlice.ByteSeq)
    (carrier : Nat)
    (plan : ExprFragmentRawPlan)
    (body : List WInstr)
    (codeEntry : List Nat)
    (binding : AverCert.WasmSlice.FuncBinding) : Prop :=
  AverCert.PlanCheck.checkExprFragmentRawPlan plan = true ∧
  AverCert.PlanLower.lowerExprFragmentBody carrier plan = some body ∧
  AverCert.PlanBytes.lowerExprFragmentCodeEntry carrier plan = some codeEntry ∧
  AverCert.WasmSlice.funcBindingForExport modBytes modLen exportName = some binding ∧
  binding.codeEntry = codeEntry

end AverCert.ExprFragmentAccepted
