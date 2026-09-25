use super::*;

/// One law whose exported proof escapes its `sorry` floor (`unsolved goals`
/// at the end of the theorem) must cost only that law. Before the isolation
/// guard it failed `lake build`, so nothing in the module was audited. Now the
/// build passes, the failed proof is charged as a sorry and reported by name,
/// and the laws next to it keep their universal credit.
#[test]
fn proof_isolation_one_failed_proof_keeps_the_rest_universal() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping proof isolation test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-isolation");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/proof_isolation.av", &output_dir, 1, &[]);
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["universal_laws"].as_u64(),
            summary["sorries"].as_u64(),
            summary["build_errors"].as_u64(),
        ),
        (Some(true), Some(2), Some(1), Some(1)),
        "the failed proof must be charged as one sorry and one hard error, and \
         the two other laws must stay universal:\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["isolated_errors"],
        serde_json::json!(["inRanges.agreesOnTheSamples"]),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorry_laws"],
        serde_json::json!(["inRanges.agreesOnTheSamples"]),
        "{}",
        format_output(&run)
    );
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("proof_manifest.json"))
            .expect("the manifest must be written"),
    )
    .expect("the manifest must be JSON");
    let tier = |law: &str| {
        manifest["laws"]
            .as_array()
            .and_then(|laws| laws.iter().find(|l| l["law"] == law))
            .and_then(|l| l["tier"].as_str().map(str::to_string))
    };
    assert_eq!(
        tier("inRanges.agreesOnTheSamples").as_deref(),
        Some("failed")
    );
    assert_eq!(tier("double.isTimesTwo").as_deref(), Some("universal"));
    assert_eq!(tier("clampLow.neverNegative").as_deref(), Some("universal"));
    let lean = std::fs::read_to_string(output_dir.join("ProofIsolation.lean"))
        .expect("ProofIsolation.lean must be emitted");
    assert!(
        lean.contains(&format!(
            "{}\n-- verify law inRanges.agreesOnTheSamples",
            aver::codegen::lean::isolate::ISOLATION_GUARD
        )),
        "the law theorem must sit behind the isolation guard:\n{lean}"
    );
    assert!(
        !lean.contains(&format!(
            "{}\ntheorem inRanges_law_agreesOnTheSamples_sample_1",
            aver::codegen::lean::isolate::ISOLATION_GUARD
        )),
        "bounded evidence must stay outside the guard:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);

    // The same export with no sorry budget: the failed proof still fails the
    // check, exactly as the hard error did.
    let output_dir = temp_output_dir("aver-proof-isolation-budget");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/proof_isolation.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "a proof that failed to elaborate must not pass a zero sorry budget:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}
