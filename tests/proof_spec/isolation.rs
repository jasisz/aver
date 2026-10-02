use super::*;

/// One law whose exported proof escapes its `sorry` floor (`unsolved goals`
/// at the end of the theorem) must cost only that law. Before the isolation
/// guard it failed `lake build`, so nothing in the module was audited. Now the
/// build passes, the failed proof is charged as a sorry and reported by name,
/// and the laws next to it keep their universal credit.
#[test]
fn proof_isolation_one_failed_proof_keeps_the_rest_universal() {
    if !lean_required::lake_available() {
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

/// A user function named like a guarded law theorem must not stand in for it.
///
/// `inRanges_law_agreesOnTheSamples` is the theorem the law below is exported
/// as, and a legal (if badly cased) function name. The theorem then fails with
/// "already declared", the isolation guard drops that error, and the check
/// used to find the function under the name, with no `sorryAx` in it: the law,
/// false at 131, passed a zero sorry budget with no trace. A guarded name must
/// now be a theorem.
#[test]
fn proof_isolation_a_function_named_like_a_law_theorem_fails_the_check() {
    if !lean_required::lake_available() {
        eprintln!("skipping law name collision test: `lake` not available");
        return;
    }
    let source = std::fs::read_to_string("tests/fixtures/proof_isolation.av")
        .expect("read the isolation fixture");
    let source = format!(
        "{source}\nfn inRanges_law_agreesOnTheSamples() -> Int\n    ? \"A function named like the law's theorem.\"\n    1\n"
    );
    let source_dir = temp_output_dir("aver-proof-isolation-collision-src");
    std::fs::create_dir_all(&source_dir).expect("create source dir");
    let file = source_dir.join("proof_isolation.av");
    std::fs::write(&file, source).expect("write source");
    let output_dir = temp_output_dir("aver-proof-isolation-collision");
    let (summary, run) = run_lean_check_json_with_args(
        file.to_str().expect("utf-8 path"),
        &output_dir,
        0,
        &[],
        &["--module-root", source_dir.to_str().expect("utf-8 path")],
    );
    let _ = std::fs::remove_dir_all(&source_dir);
    let _ = std::fs::remove_dir_all(&output_dir);
    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "a law whose theorem a function took must not pass:\n{}",
        format_output(&run)
    );
    // The build itself passes (the collision error is dropped behind the
    // guard), so the failure must come from the isolation check.
    assert_eq!(
        summary["isolated_errors"],
        serde_json::json!(["inRanges.agreesOnTheSamples"]),
        "{}",
        format_output(&run)
    );
}
