//! Closed computations must simplify before a symbolic oracle case is decided.
//! The oracle stays arbitrary; native evaluation only proves closed data steps.

use super::*;

#[test]
fn symbolic_oracle_cases_over_hex_and_utf8_build_without_sorries() {
    if !lean_required::lake_available() {
        eprintln!("skipping symbolic oracle case test: `lake` not available");
        return;
    }
    let output_dir = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/lean_oracle_ground.av",
        output_dir.path(),
        0,
        &[],
    );
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["build_errors"].as_u64(),
            summary["universal_laws"].as_u64(),
            summary["model_panicked"].as_bool(),
        ),
        (Some(true), Some(0), Some(0), Some(1), Some(false)),
        "{summary}\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(output_dir.path().join("AverProofCases.lean")).unwrap();
    assert!(
        lean.contains("↓ _root_.__AverProofCases.nativeGroundValue"),
        "{lean}"
    );
    // A pre-order value step should prove the whole 128-character key, not
    // emit a native equality for each intermediate concatenation. Audit the
    // built theorem, not merely its generated tactic text.
    let audit = output_dir.path().join("GroundCaseAxioms.lean");
    std::fs::write(&audit, r#"import Lean
import AverProofCases
open Lean in
run_cmd do
  let axioms ← collectAxioms ``AverProofCases.__aver_verify_byExpanded_1
  if axioms.contains ``sorryAx then throwError "closed-key proof contains sorryAx"
  let native := axioms.filter fun name => (name.toString.splitOn "_native.native_decide.").length > 1
  if native.isEmpty then throwError "closed-key audit missed the native equality"
  if native.size > 8 then throwError "closed-key proof expanded into {native.size} native equalities"
  let lawAxioms ← collectAxioms ``AverProofCases.nativeGround_law_identity
  for name in lawAxioms do
    unless [``propext, ``Classical.choice, ``Quot.sound].contains name do
      throwError "universal law acquired a non-kernel axiom: {name}"
"#).unwrap();
    let audit = Command::new("lake")
        .args(["env", "lean"])
        .arg(&audit)
        .current_dir(output_dir.path())
        .output()
        .unwrap();
    assert!(audit.status.success(), "{}", format_output(&audit));
}

#[test]
fn native_ground_steps_do_not_hide_wrong_results_or_propagated_errors() {
    if !lean_required::lake_available() {
        eprintln!("skipping symbolic oracle negative controls: `lake` not available");
        return;
    }
    let original = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/lean_oracle_ground.av"),
    )
    .unwrap();
    for source in [
        original.replace("=> \"short\"", "=> \"long\""),
        original.replace("Bytes.fromHex(\"abcd\")?", "Bytes.fromHex(\"0g\")?"),
        // This branch does consult the arbitrary oracle. It cannot become a
        // closed native computation or pass merely by raising the budget.
        original.replace("byName(\"ab\")", "byName(\"long-name\")"),
    ] {
        let source_dir = tempfile::tempdir().unwrap();
        let file = source_dir.path().join("main.av");
        std::fs::write(&file, source).unwrap();
        let output_dir = tempfile::tempdir().unwrap();
        let (summary, run) =
            run_lean_check_json(file.to_str().unwrap(), output_dir.path(), 1000, &[]);
        assert_eq!(
            summary["passed"],
            false,
            "{summary}\n{}",
            format_output(&run)
        );
        assert!(
            summary["build_errors"].as_u64().unwrap() > 0,
            "a counterexample must fail outside isolation: {summary}\n{}",
            format_output(&run)
        );
        assert_eq!(summary["model_panicked"], false, "{summary}");
    }
}
