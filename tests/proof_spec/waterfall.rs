//! Opt-in integration canary, run with AVER_WATERFALL_DIR pointing to a
//! checkout built using the same Lean toolchain as the generated project.
use super::*;

#[test]
#[ignore = "requires a built waterfall checkout (AVER_WATERFALL_DIR)"]
fn waterfall_discovers_replays_and_rejects_false_reasons() {
    let waterfall = std::env::var("AVER_WATERFALL_DIR").expect("set AVER_WATERFALL_DIR");
    let dir = temp_output_dir("aver-waterfall");
    let (summary, output) = run_lean_check_json_with_args(
        "tools/waterfall/tree.av",
        &dir,
        0,
        &[],
        &["--waterfall", &waterfall],
    );
    assert!(output.status.success(), "{}", format_output(&output));
    assert_eq!(summary["universal_laws"], 2);
    assert_eq!(summary["sorries"], 0);
    let report = || -> serde_json::Value {
        serde_json::from_slice(&std::fs::read(dir.join("proof_waterfall.json")).unwrap()).unwrap()
    };
    assert!(
        report()
            .as_array()
            .unwrap()
            .iter()
            .any(|r| r["status"] == "discovered")
    );
    // No dependency is present for the second run. The retained proposal must
    // compile without importing waterfall, then pass the ordinary project gate.
    let missing = dir.join("missing-waterfall");
    std::fs::write(
        dir.join("Stale.lean"),
        "-- aver:waterfall broken metadata from an older export\n",
    )
    .unwrap();
    let (replay, output) = run_lean_check_json_with_args(
        "tools/waterfall/tree.av",
        &dir,
        0,
        &[],
        &["--waterfall", missing.to_str().unwrap()],
    );
    assert!(output.status.success(), "{}", format_output(&output));
    assert_eq!(replay["universal_laws"], 2);
    assert!(
        report()
            .as_array()
            .unwrap()
            .iter()
            .any(|r| r["status"] == "replayed")
    );
    // A corrupted retained script is a proposal, never proof credit.
    let path = dir.join("proof_waterfall_cache.json");
    let mut cache: serde_json::Value =
        serde_json::from_slice(&std::fs::read(&path).unwrap()).unwrap();
    for script in cache["scripts"].as_object_mut().unwrap().values_mut() {
        *script = "sorry".into();
    }
    std::fs::write(path, serde_json::to_vec(&cache).unwrap()).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["proof", "tools/waterfall/tree.av", "--check-json", "-o"])
        .arg(&dir)
        .arg("--waterfall")
        .arg(&missing)
        .output()
        .unwrap();
    assert!(
        !output.status.success(),
        "a sorry cache cannot supply proof credit"
    );
    assert_eq!(output.status.code(), Some(2));
    let bad = temp_output_dir("aver-waterfall-because");
    let (summary, output) = run_lean_check_json_with_args(
        "tests/fixtures/law_reasons.av",
        &bad,
        0,
        &[],
        &[
            "--waterfall",
            &waterfall,
            "--waterfall-law",
            "identity.badReasonCannotHideBehindEasyGoal",
        ],
    );
    assert!(!output.status.success());
    assert_eq!(
        summary["obligations"]["identity.badReasonCannotHideBehindEasyGoal.because1"],
        "failed"
    );
    assert_eq!(
        summary["obligations"]["identity.badReasonCannotHideBehindEasyGoal.implication"],
        "universal"
    );
    assert_eq!(summary["universal_laws"], 2);
    let guarded = temp_output_dir("aver-waterfall-guarded");
    let (summary, output) = run_lean_check_json_with_args(
        "tests/fixtures/waterfall_guarded.av",
        &guarded,
        0,
        &[],
        &["--waterfall", &waterfall],
    );
    assert!(output.status.success(), "{}", format_output(&output));
    assert_eq!(summary["universal_laws"], 1);
    assert_eq!(summary["bounded_laws"], 0);
    let generated = std::fs::read_to_string(guarded.join("WaterfallGuarded.lean")).unwrap();
    assert!(generated.contains("lessF zeroF x = true -> nonNeg x = true"));
    assert!(!generated.contains("import waterfall"));
    let _ = std::fs::remove_dir_all(guarded);
    let _ = std::fs::remove_dir_all(dir);
    let _ = std::fs::remove_dir_all(bad);
}
