use super::*;

#[test]
fn imported_in_place_effects_have_universal_mapping_and_splice_laws() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_imported_effects");
    let dir = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        source.join("main.av").to_str().unwrap(),
        dir.path(),
        0,
        &[],
        &["--module-root", source.to_str().unwrap()],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    for key in ["bounded_laws", "build_errors", "sorries"] {
        assert_eq!(summary[key], 0, "{summary}");
    }
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join("proof_manifest.json")).unwrap(),
    )
    .unwrap();
    let laws = manifest["laws"].as_array().unwrap();
    // Eight owning-module laws, eight nested-wrapper laws, and twenty
    // caller laws (including mapping, splice and cursor obligations).
    assert_eq!(
        laws.len(),
        36,
        "missing composition obligations: {manifest}"
    );
    for law in laws {
        assert_eq!(law["tier"], "universal", "{law}");
        for axiom in law["axioms"].as_array().unwrap() {
            assert!(
                ["propext", "Quot.sound", "Classical.choice"].contains(&axiom.as_str().unwrap()),
                "{law}"
            );
        }
    }
}

#[test]
fn equal_imported_results_do_not_hide_lost_or_reordered_effects() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let fixture =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_imported_effects");
    let source = tempfile::tempdir().unwrap();
    for name in [
        "main.av",
        "pool.av",
        "pooled.av",
        "looper.av",
        "wrapper.av",
        "aver.toml",
    ] {
        std::fs::copy(fixture.join(name), source.path().join(name)).unwrap();
    }
    let main = source.path().join("main.av");
    let mut text = std::fs::read_to_string(&main).unwrap();
    text.push_str(r#"
fn corrupt(observed: __EarlyTraceEarlyResult, mode: Int) -> __EarlyTraceEarlyResult
    match mode
        0 -> __EarlyTraceEarlyResult.update(observed, events = List.drop(observed.events, 1))
        1 -> __EarlyTraceEarlyResult.update(observed, events = List.reverse(observed.events))
        _ -> __EarlyTraceEarlyResult.update(observed, position = 0)

fn detectsCorruption(mode: Int) -> Bool
    inputs = [__EarlyTraceInput.AnswerHostTimeUnixMs(10), __EarlyTraceInput.AnswerFinish(Result.Err("stop")), __EarlyTraceInput.Foreign]
    original = __earlySourceTraceFrom(2, inputs, 7, [], 20)
    broken = corrupt(original, mode)
    Bool.and(original.value == Option.Some(Result.Err("stop")), Bool.and(original.value == broken.value, Bool.not(original == broken)))

verify detectsCorruption
    detectsCorruption(0) => true
    detectsCorruption(1) => true
    detectsCorruption(2) => true
"#);
    for (mode, name) in ["dropped", "reordered", "resetPosition"].iter().enumerate() {
        text.push_str(&format!("\nverify corrupt law {name}\n    given observed: __EarlyTraceEarlyResult = [__earlySourceTrace(0, [])]\n    when observed.value == Option.Some(Result.Err(\"stop\"))\n    when List.len(observed.events) > 1\n    when observed.position != 0\n    using []\n    corrupt(observed, {mode}) == observed holds\n"));
    }
    std::fs::write(&main, text).unwrap();
    let cases = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("verify")
        .arg(&main)
        .arg("--module-root")
        .arg(source.path())
        .output()
        .unwrap();
    assert!(cases.status.success(), "{}", format_output(&cases));
    let dir = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        main.to_str().unwrap(),
        dir.path(),
        0,
        &[],
        &["--module-root", source.path().to_str().unwrap()],
    );
    assert!(!run.status.success(), "false laws passed: {summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    assert_eq!(summary["universal_laws"], 36, "{summary}");
    for name in ["dropped", "reordered", "resetPosition"] {
        assert_eq!(
            summary["obligations"][format!("corrupt.{name}.implication")],
            "failed",
            "{summary}"
        );
    }
}
