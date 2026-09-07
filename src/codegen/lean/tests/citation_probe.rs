//! Exercise the actual opt-in elaborator under the same pinned Lean that
//! checks emitted proofs. These diagnostic twins never supply proof credit.

use crate::codegen::lean::{citation_probe, untranslate};

const SCENARIOS: &str = r#"
set_option linter.unusedSimpArgs false
set_option maxHeartbeats 200000
def ordered (a b factor : Int) : Bool := a * factor <= b * factor
theorem orderFact (a b factor : Int)
    (h : (decide (a <= b) && decide (factor >= 0)) = true) : ordered a b factor = true := by
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  simp [ordered]
  exact Int.mul_le_mul_of_nonneg_right h.1 h.2
example (a b : Int) (h : a >= 0) : ordered 0 a b = true := by
  have _fact0 := orderFact
  aver_probe_citation "healthy" "orderFact" prepare_by (simp only [Bool.and_eq_true, decide_eq_true_eq] at *) apply_by (with_reducible apply _fact0) solve_by (first | assumption | omega)
  sorry
-- The axiom is hidden behind a theorem, never named in the copied _fact list.
axiom inventedLowerBound (b : Int) : 0 <= b
theorem laterGlobal (b : Int) : 0 <= b := by
  exact inventedLowerBound b
example (a b : Int) (h : a >= 0) : ordered 0 a b = true := by
  have _fact0 := orderFact
  aver_probe_citation "foreign_global" "orderFact" prepare_by (simp only [Bool.and_eq_true, decide_eq_true_eq] at *) apply_by (with_reducible apply _fact0) solve_by (first | assumption | omega | exact laterGlobal _)
  sorry
theorem laterSorry (b : Int) : 0 <= b := by sorry
example (a b : Int) (h : a >= 0) : ordered 0 a b = true := by
  have _fact0 := orderFact
  let localProof : 0 <= b := laterSorry b
  aver_probe_citation "sorry_local_let" "orderFact" prepare_by (simp only [Bool.and_eq_true, decide_eq_true_eq] at *) apply_by (with_reducible apply _fact0) solve_by (first | exact localProof | assumption | omega)
  sorry
-- A later global grind registration changes the isolated search environment.
-- Its dependency must be found in the closing term, without guessing sources.
def mysterious (b : Int) : Int := b
theorem unrelatedLater (b : Int) : 0 <= mysterious b := by sorry
grind_pattern unrelatedLater => mysterious b
example (a b : Int) (h : a >= 0) : ordered 0 a (mysterious b) = true := by
  have _fact0 := orderFact
  aver_probe_citation "later_global_pattern" "orderFact" prepare_by (simp only [Bool.and_eq_true, decide_eq_true_eq] at *) apply_by (with_reducible apply _fact0) solve_by (first | assumption | omega | grind)
  sorry
-- Clearing the tactic goal list is not an assigned proof. Even this synthetic
-- apparent closure must fail the audit and cannot get an empty-axiom verdict.
example (a b : Int) (h : a >= 0) : ordered 0 a b = true := by
  have _fact0 := orderFact
  aver_probe_citation "unassigned_goal" "orderFact" prepare_by (simp only [Bool.and_eq_true, decide_eq_true_eq] at *) apply_by (with_reducible apply _fact0) solve_by (run_tac Lean.Elab.Tactic.setGoals [])
  sorry
"#;

#[test]
fn citation_probe_audits_actual_closures_through_globals_and_local_lets() {
    let dir = tempfile::Builder::new()
        .prefix("aver-citation-audit-")
        .tempdir()
        .unwrap();
    std::fs::write(
        dir.path().join("lean-toolchain"),
        super::super::prelude::generate_toolchain(),
    )
    .unwrap();
    std::fs::write(
        dir.path().join("Audit.lean"),
        format!(
            "{}\n{}\n{}",
            untranslate::AVER_DUMP_GOAL_ELAB,
            citation_probe::ELAB,
            SCENARIOS
        ),
    )
    .unwrap();
    let output = match std::process::Command::new("lean")
        .arg("Audit.lean")
        .current_dir(dir.path())
        .output()
    {
        Ok(output) => output,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return,
        Err(error) => panic!("run pinned citation probe: {error}"),
    };
    let transcript = format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        output.status.success(),
        "citation probe failed:\n{transcript}"
    );
    let records: std::collections::BTreeMap<String, serde_json::Value> = transcript
        .lines()
        .filter_map(|line| {
            let (_, json) = line.split_once(citation_probe::MARKER)?;
            let record: serde_json::Value =
                serde_json::from_str(json).expect("structured citation trace");
            Some((record["claim"].as_str().unwrap().to_string(), record))
        })
        .collect();
    assert_eq!(records.len(), 5);
    let healthy = &records["healthy"]["premises"];
    assert_eq!(healthy[0]["status"], "closed");
    assert_eq!(healthy[0]["proof_audit"], "checked");
    assert_eq!(healthy[0]["proof_axioms"], serde_json::json!([]));
    assert_eq!(healthy[1]["status"], "open");
    for (claim, axiom) in [
        ("foreign_global", "inventedLowerBound"),
        ("sorry_local_let", "sorryAx"),
        ("later_global_pattern", "sorryAx"),
    ] {
        let premise = &records[claim]["premises"][1];
        assert_eq!(premise["status"], "closed", "{claim}");
        assert_eq!(premise["proof_audit"], "checked", "{claim}");
        assert!(
            premise["proof_axioms"]
                .as_array()
                .unwrap()
                .iter()
                .any(|name| name.as_str() == Some(axiom)),
            "{claim}: {premise}"
        );
    }
    for premise in records["unassigned_goal"]["premises"].as_array().unwrap() {
        assert_eq!(premise["status"], "closed");
        assert_eq!(premise["proof_audit"], "unavailable");
    }
}
