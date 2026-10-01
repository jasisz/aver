//! Minimal #1462 reproductions: finite fixture runs do not make the general
//! effectful recursion total. Until the proof model represents these calls,
//! its isolated failures must remain visible to strict checking.

use super::*;

fn assert_open_fixture(fixture: &str, module: &str, function: &str, partials: &[&str]) {
    let verify = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(["verify", fixture])
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .unwrap();
    assert!(verify.status.success(), "{}", format_output(&verify));
    assert!(
        String::from_utf8_lossy(&verify.stdout).contains("3/3 cases passed"),
        "{}",
        format_output(&verify)
    );
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping unmeasured recursion proof check: `lake` not available");
        return;
    }

    let output_dir = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json(fixture, output_dir.path(), 0, &[]);
    let cases: Vec<_> = (1..=3)
        .map(|index| format!("{module}.__aver_verify_{function}_{index}"))
        .collect();
    assert_eq!(
        summary["passed"],
        false,
        "{summary}\n{}",
        format_output(&run)
    );
    assert!(!run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["build_succeeded"], true, "{summary}");
    assert_eq!(summary["model_panicked"], false, "{summary}");
    assert_eq!(summary["sorries"], 3, "{summary}");
    assert_eq!(summary["build_errors"], 3, "{summary}");
    assert_eq!(summary["isolated_errors"], serde_json::json!(cases));

    let lean = std::fs::read_to_string(output_dir.path().join(format!("{module}.lean"))).unwrap();
    for name in partials {
        assert!(lean.contains(&format!("partial def {name} ")), "{lean}");
        assert!(!lean.contains(&format!("{name}__fuel")), "{lean}");
    }

    // Lean exits successfully for guarded elaboration errors. Audit the
    // actual theorem bodies rather than treating that exit code as proof.
    let mut audit = format!("import Lean\nimport {module}\nopen Lean in\nrun_cmd do\n");
    for case in cases {
        audit.push_str(&format!(
            "  let axioms ← collectAxioms ``{case}\n\
             \x20 unless axioms.contains ``sorryAx do\n\
             \x20   throwError \"expected an honestly open fixture case: {case}\"\n"
        ));
    }
    let audit_file = output_dir.path().join("UnmeasuredCaseAudit.lean");
    std::fs::write(&audit_file, audit).unwrap();
    let audited = Command::new("lake")
        .args(["env", "lean"])
        .arg(audit_file)
        .current_dir(output_dir.path())
        .output()
        .unwrap();
    assert!(audited.status.success(), "{}", format_output(&audited));
}

#[test]
fn parent_chain_without_an_acyclicity_invariant_stays_honestly_open() {
    assert_open_fixture(
        "tests/fixtures/lean_oracle_parent_chain.av",
        "OracleParentChain",
        "lineage",
        &["lastRealBits", "realOrBack"],
    );
}

#[test]
fn open_scan_without_a_live_oracle_bound_stays_honestly_open() {
    assert_open_fixture(
        "tests/fixtures/lean_oracle_open_scan.av",
        "OracleOpenScan",
        "observed",
        &["cleared", "sweeping"],
    );
}
