use super::*;
use std::path::Path;

fn source_fixture() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_request_traces")
}

fn audit(dir: &Path, expected: usize) {
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    let laws = manifest["laws"].as_array().unwrap();
    assert_eq!(laws.len(), expected, "{manifest}");
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
fn source_request_traces_are_universal_including_in_place_effects() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source = source_fixture();
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
    assert_eq!(summary["universal_laws"], 10, "{summary}");
    audit(dir.path(), 10);
}

#[test]
fn unchanged_results_do_not_hide_six_different_trace_corruptions() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source = tempfile::tempdir().unwrap();
    for name in ["main.av", "pool.av", "pooled.av", "aver.toml"] {
        std::fs::copy(source_fixture().join(name), source.path().join(name)).unwrap();
    }
    let main = source.path().join("main.av");
    let mut text = std::fs::read_to_string(&main).unwrap();
    text.push_str(r#"
fn changeEvent(event: __IgnoredTraceEvent, mode: Int) -> __IgnoredTraceEvent
    match event
        __IgnoredTraceEvent.Empty -> __IgnoredTraceEvent.Empty
        __IgnoredTraceEvent.ObservedClaim(position, arg, answer) -> match mode
            3 -> __IgnoredTraceEvent.ObservedClaim(position, arg + 1, answer)
            4 -> __IgnoredTraceEvent.ObservedClaim(position, arg, Option.Some(999))
            _ -> __IgnoredTraceEvent.ObservedClaim(0, arg, answer)

fn changeEvents(events: List<__IgnoredTraceEvent>, mode: Int) -> List<__IgnoredTraceEvent>
    match events
        [] -> []
        [event, ..rest] -> List.prepend(changeEvent(event, mode), changeEvents(rest, mode))

fn brokenTrace(id: Int, inputs: List<__IgnoredTraceInput>, mode: Int) -> __IgnoredTraceIgnoredResult
    observed = __ignoredProtocolTrace(id, inputs)
    events = match mode
        0 -> List.drop(observed.events, 1)
        1 -> List.concat(observed.events, observed.events)
        2 -> List.reverse(observed.events)
        _ -> changeEvents(observed.events, mode)
    __IgnoredTraceIgnoredResult.update(observed, events = events)

fn detects(mode: Int) -> Bool
    inputs = [__IgnoredTraceInput.AnswerClaim(Option.Some(2)), __IgnoredTraceInput.AnswerClaim(Option.Some(3))]
    original = __ignoredSourceTrace(1, inputs)
    broken = brokenTrace(1, inputs, mode)
    Bool.and(original.value == broken.value, Bool.not(original.events == broken.events))

verify detects
    detects(0) => true
    detects(1) => true
    detects(2) => true
    detects(3) => true
    detects(4) => true
    detects(5) => true
"#);
    for (mode, name) in [
        "dropped",
        "duplicated",
        "reordered",
        "arguments",
        "answers",
        "resetCounter",
    ]
    .into_iter()
    .enumerate()
    {
        text.push_str(&format!("\nverify __ignoredSourceTrace law {name}\n    given id: Int = [1]\n    given inputs: List<__IgnoredTraceInput> = [[]]\n    when List.len(inputs) >= 2\n    using []\n    __ignoredSourceTrace(id, inputs) == brokenTrace(id, inputs, {mode}) holds\n"));
    }
    std::fs::write(&main, text).unwrap();
    let cases = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args([
            "verify",
            main.to_str().unwrap(),
            "--module-root",
            source.path().to_str().unwrap(),
        ])
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
    assert_eq!(summary["universal_laws"], 10, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    for name in [
        "dropped",
        "duplicated",
        "reordered",
        "arguments",
        "answers",
        "resetCounter",
    ] {
        assert_eq!(
            summary["obligations"][format!("__ignoredSourceTrace.{name}.implication")],
            "failed",
            "{summary}"
        );
    }
}

#[test]
fn imported_private_helpers_preserve_universal_request_traces() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/yield_request_trace_imports");
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
    assert_eq!(summary["universal_laws"], 2, "{summary}");
    audit(dir.path(), 2);
}
