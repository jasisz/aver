//! Isolated, opt-in direct-citation diagnostics. These twins are never part of
//! the counted build or its axiom audit. They inspect the emitter's actual
//! citation applications, preserving ordinary generated files byte for byte.

/// Exact emitted namespace and module root for an entry or dependency file.
/// This shares the emitter's naming rules, including reserved-word escaping.
pub fn module_name(ctx: &crate::codegen::CodegenContext, scope: Option<&str>) -> String {
    scope
        .map(super::syntax::aver_path_to_lean)
        .unwrap_or_else(|| super::lean_project_name(ctx))
}

/// Structured diagnostic records, produced only by the isolated probe.
pub const MARKER: &str = "AVER_CITATION_ATTEMPT:";

/// Append after `untranslate::AVER_DUMP_GOAL_ELAB` and imports, before entering
/// a generated module namespace. The existing expression serializer gives the
/// caller the same closed grammar and honest decline as ordinary goal dumps.
pub const ELAB: &str = r#"
open Lean Elab Tactic Meta in
private def averCitationGoalJson (g : MVarId) : TacticM String := g.withContext do
  let ty ← instantiateMVars (← g.getType)
  let fvars ← (← getLCtx).foldlM (init := (#[] : Array Expr)) fun acc d => do
    if d.isImplementationDetail || (← isProp d.type) then return acc
    return acc.push d.toExpr
  let closed ← instantiateMVars (← mkForallFVars fvars ty)
  return averExprJson [] closed

-- Audit the actual assigned term, including values of used local lets and
-- transitive dependencies of every referenced declaration. Unlike merely
-- listing _fact names, this catches globally registered grind lemmas too.
open Lean Elab Tactic Meta in
private def averCitationProofAudit (g : MVarId) : TacticM (String × Array Name) := do
  try
    g.withContext do
      let some value ← getExprMVarAssignment? g | return ("unavailable", #[])
      let value ← instantiateMVars value
      if value.hasMVar then return ("unavailable", #[])
      let fvars ← (← getLCtx).foldlM (init := (#[] : Array Expr)) fun acc d =>
        pure (acc.push d.toExpr)
      -- Keep let values; generalizing them into arbitrary hypotheses would
      -- erase any axioms carried by a locally bound proof.
      let closed ← instantiateMVars (← mkLambdaFVars fvars value
        (usedOnly := true) (generalizeNondepLet := false))
      if closed.hasMVar || closed.hasFVar || closed.hasLooseBVars then
        return ("unavailable", #[])
      let mut axioms : NameSet := {}
      for name in closed.getUsedConstants do
        -- collectAxioms tolerates missing declarations; an audit must not.
        if ((← getEnv).checked.get.find? name).isNone then
          return ("unavailable", #[])
        for axiomName in (← collectAxioms name) do
          axioms := axioms.insert axiomName
      return ("checked", axioms.toArray.qsort Name.lt)
  catch _ =>
    return ("unavailable", #[])

open Lean Elab Tactic Meta in
elab "aver_probe_citation " claim:str citation:str " prepare_by " preparation:tacticSeq
    " apply_by " application:tacticSeq
    " solve_by " solver:tacticSeq : tactic => do
  let saved ← saveState
  let mut outcome := "preparation_failed"
  let mut premises : Array String := #[]
  try
    evalTactic preparation
    outcome := "application_failed"
    evalTactic application
    outcome := "matched"
    -- An implication may carry a conjunction of source guards. Decompose it
    -- propositionally so diagnostics can report each remaining conjunct.
    evalTactic (← `(tactic| all_goals repeat' apply And.intro))
    let goals ← getUnsolvedGoals
    for g in goals do
      setGoals [g]
      let before ← saveState
      try
        evalTactic solver
      catch _ =>
        before.restore
      let status := if (← getUnsolvedGoals).isEmpty then "closed" else "open"
      let goalJson ← averCitationGoalJson g
      let (audit, axioms) ← if status == "closed" then averCitationProofAudit g
        else pure ("not_applicable", #[])
      let axiomsJson := String.intercalate "," (axioms.toList.map fun name =>
        "\"" ++ averJsonEsc (toString name) ++ "\"")
      premises := premises.push ("{\"status\":\"" ++ status ++ "\",\"goal\":" ++ goalJson
        ++ ",\"proof_audit\":\"" ++ audit ++ "\",\"proof_axioms\":[" ++ axiomsJson ++ "]}")
  catch _ =>
    if outcome == "matched" then outcome := "probe_error"
  saved.restore
  let json := "{\"claim\":\"" ++ averJsonEsc claim.getString
    ++ "\",\"citation\":\"" ++ averJsonEsc citation.getString
    ++ "\",\"phase\":\"diagnostic_direct_application\",\"outcome\":\"" ++ outcome
    ++ "\",\"premises\":[" ++ String.intercalate "," premises.toList ++ "]}"
  logInfo m!"AVER_CITATION_ATTEMPT:{json}"
"#;

/// Build a diagnostic twin from one emitted reason-obligation theorem block.
/// Only the direct application branch has an exact, reusable premise strategy;
/// other shapes return `None`, rather than inventing a different application.
/// `claim` is the source identity from the emitter's obligation marker.
pub fn probe_body(block: &[&str], probe_name: &str, claim: &str) -> Option<String> {
    let first = block.first()?.trim();
    let declaration = first.strip_prefix("theorem ")?;
    let (_, statement) = declaration.split_once(" : ")?;
    if !statement.ends_with(" := by") {
        return None;
    }
    let mut prefix = Vec::new();
    let mut facts = std::collections::BTreeMap::new();
    for line in block.iter().skip(1) {
        let trimmed = line.trim();
        if trimmed.starts_with("intro ") || trimmed.starts_with("try simp only ") {
            prefix.push(*line);
        } else if let Some(fact) = trimmed.strip_prefix("have ") {
            let (local, theorem) = fact.split_once(" := ")?;
            if !local.starts_with("_fact") {
                break;
            }
            facts.insert(local.to_string(), theorem.to_string());
            prefix.push(*line);
        } else {
            break;
        }
    }
    let mut attempts = Vec::new();
    for line in block {
        let Some(branch) = line.trim().strip_prefix("| (") else {
            continue;
        };
        let Some((application, solver)) = branch.split_once(" <;> ") else {
            continue;
        };
        let Some((preparation, local)) = application.rsplit_once("; with_reducible apply ") else {
            continue;
        };
        let Some(citation) = facts.get(local) else {
            continue;
        };
        let solver = solver.strip_suffix(')')?;
        attempts.push(format!(
            "  aver_probe_citation {} {} prepare_by ({preparation}) apply_by (with_reducible apply {local}) solve_by {solver}",
            serde_json::to_string(claim).ok()?,
            serde_json::to_string(citation).ok()?,
        ));
    }
    if attempts.is_empty() {
        return None;
    }
    Some(format!(
        "theorem {probe_name} : {statement}\n{}\n{}\n  sorry\n",
        prefix.join("\n"),
        attempts.join("\n")
    ))
}

#[cfg(test)]
mod tests {
    use super::probe_body;

    #[test]
    fn twin_preserves_actual_citation_and_solver_without_counted_strategy() {
        let block = [
            "theorem __aver_reason_demo_because1 : ∀ (a : Int), a = a := by",
            "  intro a",
            "  have _fact0 := Source.fact_law_identity",
            "  try simp only [Bool.and_eq_true] at _fact0",
            "  first",
            "  | (simp_all; omega)",
            "  | (simp only [Bool.and_eq_true] at *; with_reducible apply _fact0 <;> (first | assumption | omega))",
            "  | sorry",
        ];
        let body = probe_body(&block, "_probe", "demo.because1").unwrap();
        assert!(body.starts_with("theorem _probe : ∀ (a : Int), a = a := by"));
        assert!(body.contains("\"Source.fact_law_identity\" prepare_by (simp only [Bool.and_eq_true] at *) apply_by (with_reducible apply _fact0) solve_by (first | assumption | omega)"));
        assert!(!body.contains("| (simp_all; omega)"));
        assert!(body.ends_with("  sorry\n"));
    }

    #[test]
    fn no_direct_application_means_no_invented_probe() {
        let block = ["theorem foo : True := by", "  trivial"];
        assert!(probe_body(&block, "_probe", "foo").is_none());
    }
}
