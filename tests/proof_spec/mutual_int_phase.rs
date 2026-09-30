//! A mutual counter needs an offset as well as a phase rank: a worker can
//! be called directly beyond the guard's boundary and must still terminate.

use super::*;

#[test]
fn mutual_int_phases_with_symbolic_oracles_prove_without_sorries() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping mutual Int phases: `lake` not available");
        return;
    }
    let output_dir = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/lean_mutual_int_phase.av",
        output_dir.path(),
        0,
        &[],
    );
    assert_eq!(
        summary["passed"],
        true,
        "{summary}\n{}",
        format_output(&run)
    );
    assert_eq!(summary["sorries"], 0, "{summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["model_panicked"], false, "{summary}");
    let lean = std::fs::read_to_string(output_dir.path().join("MutualIntPhase.lean")).unwrap();
    assert!(!lean.contains("partial def scan"), "{lean}");
    assert!(!lean.contains("scan__fuel"), "{lean}");
    assert!(!lean.contains("partial def down"), "{lean}");

    let audit = output_dir.path().join("PhaseAxioms.lean");
    std::fs::write(&audit, "import MutualIntPhase\n#print axioms MutualIntPhase.scan\n#print axioms MutualIntPhase.advance\n#print axioms MutualIntPhase.down\n#print axioms MutualIntPhase.takeRow\n").unwrap();
    let audit = Command::new("lake")
        .args(["env", "lean"])
        .arg(&audit)
        .current_dir(output_dir.path())
        .output()
        .unwrap();
    assert!(audit.status.success(), "{}", format_output(&audit));
    let axioms = String::from_utf8(audit.stdout).unwrap();
    assert!(!axioms.contains("sorryAx"), "{axioms}");
    assert!(!axioms.contains("native_decide"), "{axioms}");
    assert!(!axioms.contains("ofReduceBool"), "{axioms}");
}

#[test]
fn native_mutual_phases_do_not_hide_false_results_or_effect_errors() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let original = include_str!("../fixtures/lean_mutual_int_phase.av");
    for source in [
        original.replace(
            "=> Result.Ok([\"x\", \"x\", \"x\"])",
            "=> Result.Ok([\"wrong\"])",
        ),
        original.replace("true -> Result.Ok(\"x\")", "true -> Result.Err(\"broken\")"),
        original.replace(
            "scan(\"fixture\", 0, 3, [])",
            "scan(\"no-such-fixture\", 0, 3, [])",
        ),
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
        assert!(summary["build_errors"].as_u64().unwrap() > 0, "{summary}");
        assert_eq!(summary["model_panicked"], false, "{summary}");
    }
}
