-- Used only by cases-form verify with a symbolic capability oracle.
-- Closed data steps use the same native-evaluation trust as native_decide.
-- No free oracle, loose bound variable, or metavariable is evaluated, and each
-- proposed literal is accompanied by a proof of equality to the original.
namespace __AverProofCases
open Lean Meta Simp

private def normalize (α : Type) [ToExpr α] (e : Expr) : SimpM Step := do
  try
    let ty := toTypeExpr α
    -- The wildcard simproc's pattern does not enforce a runtime value type.
    -- Check against the reflected type before calling the typed evaluator.
    unless ← isDefEq (← inferType e) ty do return .continue
    let value ← unsafe evalExpr α ty e
    let result := toExpr value
    if result == e then return .continue
    let proposition ← mkEq e result
    let decision ← mkDecide proposition
    let .success h ← nativeEqTrue `native_decide decision | return .continue
    let proof ← mkAppM ``of_decide_eq_true #[h]
    return .done { expr := result, proof? := some proof }
  catch _ =>
    return .continue

-- This is opt-in, not a global simp rule: universal law proofs must never
-- acquire native-evaluation assumptions from simplifying concrete subterms.
private def groundValue (e : Expr) : SimpM Step := do
  unless e.isApp && !e.hasFVar && !e.hasMVar && !e.hasLooseBVars do return .continue
  if let .const name _ := e.getAppFn then
    if (← getConstInfo name).isCtor then return .continue
  let ty ← inferType e
  if ty.isConstOf ``String then return ← normalize String e
  if ty.isConstOf ``Bool then return ← normalize Bool e
  if ty == toTypeExpr (List Int) then return ← normalize (List Int) e
  if ty == toTypeExpr (List String) then return ← normalize (List String) e
  if ty == toTypeExpr (Option Int) then return ← normalize (Option Int) e
  return .continue

-- Run reflected values before descending: normalize one closed key instead
-- of proving an equality for every intermediate string/byte computation.
-- Containers stay post-order; reducing them first can repeatedly expose the
-- same recursive helpers before simp has selected the branch beneath them.
simproc_decl nativeGroundValue (_) := groundValue

simproc_decl nativeGround (_) := fun e => do
  match ← groundValue e with
  | .continue => pure ()
  | step => return step
  unless e.isApp && !e.hasFVar && !e.hasMVar && !e.hasLooseBVars do return .continue
  if let .const name _ := e.getAppFn then
    if (← getConstInfo name).isCtor then return .continue
  let ty ← inferType e
  -- Containers of user records/refinements need no reflection instance.
  -- Expose only a closed constructor by kernel reduction; returning .done
  -- prevents simp from cycling between an unfolded container and its helpers.
  unless ty.isAppOf ``List || ty.isAppOf ``Option || ty.isAppOf ``Except do return .continue
  let result ← withTransparency .all <| whnf e
  if result == e then return .continue
  let .const name _ := result.getAppFn | return .continue
  unless (← getConstInfo name).isCtor do return .continue
  return .done { expr := result, proof? := some (← mkEqRefl e) }

end __AverProofCases
