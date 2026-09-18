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
    // Fifteen trace obligations, and six more for the segment interface: this
    // process observes two effectful segments, `__timedStart` and
    // `__timedAnswerClaim`, and each one contributes a cursor bound, an
    // event-history prefix and a protocol step.
    assert_eq!(summary["universal_laws"], 21, "{summary}");
    audit(dir.path(), 21);
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
    // The same twenty-one as above: the segment interface says nothing about
    // the corrupted claims below and none of them cites it.
    assert_eq!(summary["universal_laws"], 21, "{summary}");
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
    assert_eq!(summary["universal_laws"], 3, "{summary}");
    audit(dir.path(), 3);
}

#[test]
fn tail_entry_alignment_is_universal_across_local_and_imported_helpers() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_tail_traces");
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
    assert_eq!(summary["universal_laws"], 12, "{summary}");
    audit(dir.path(), 12);
}

#[test]
fn tail_pause_cannot_be_counted_as_an_answer_or_hidden_from_consumption() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let fixture =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_tail_traces");
    let source = tempfile::tempdir().unwrap();
    for name in ["main.av", "leaf.av", "pool.av", "pooled.av", "aver.toml"] {
        std::fs::copy(fixture.join(name), source.path().join(name)).unwrap();
    }
    let main = source.path().join("main.av");
    let mut text = std::fs::read_to_string(&main).unwrap();
    text.push_str(r#"
fn brokenPause(id: Int, inputs: List<__LocalTraceInput>, answer: Bool) -> __LocalTraceLocalResult
    observed = __localProtocolTrace(id, inputs)
    match answer
        true -> __LocalTraceLocalResult.update(observed, position = observed.position + 1)
        false -> __LocalTraceLocalResult.update(observed, consumed = observed.consumed - 1)

fn detectsPause(answer: Bool) -> Bool
    inputs = [__LocalTraceInput.Advance, __LocalTraceInput.AnswerClaim(9)]
    expected = __localSourceTrace(2, inputs)
    broken = brokenPause(2, inputs, answer)
    Bool.and(expected.value == broken.value, Bool.and(expected.events == broken.events, Bool.not(expected == broken)))

verify detectsPause
    detectsPause(true) => true
    detectsPause(false) => true
"#);
    for (name, answer) in [("inventedAnswer", true), ("hiddenResumption", false)] {
        text.push_str(&format!("\nverify __localSourceTrace law {name}\n    given id: Int = [2]\n    given inputs: List<__LocalTraceInput> = [[]]\n    when List.len(inputs) >= 2\n    using []\n    __localSourceTrace(id, inputs) == brokenPause(id, inputs, {answer}) holds\n"));
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
    assert!(!run.status.success(), "false pause laws passed: {summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["universal_laws"], 12, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    for name in ["inventedAnswer", "hiddenResumption"] {
        assert_eq!(
            summary["obligations"][format!("__localSourceTrace.{name}.implication")],
            "failed",
            "{summary}"
        );
    }
}

#[test]
fn recursive_helper_splices_are_universal_and_explicit_dependencies() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_recursive_traces");
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
    assert_eq!(summary["universal_laws"], 16, "{summary}");
    audit(dir.path(), 16);
    let lean = std::fs::read_to_string(dir.path().join("RecursiveTraces.lean")).unwrap();
    for root in ["parent", "tail", "repeated"] {
        let proof = lean
            .split(&format!(
                "theorem __aver_reason___{root}SourceTraceFrom_law_correspondence_implication"
            ))
            .nth(1)
            .unwrap()
            .split(&format!(
                "theorem __{root}SourceTraceFrom_law_correspondence :"
            ))
            .next()
            .unwrap();
        assert!(proof.contains("SourceLoop_law_correspondence"), "{proof}");
        assert!(proof.contains("Observed_law_splice"), "{proof}");
    }
}

#[test]
fn equal_helper_results_do_not_justify_corrupted_splice_cursors() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let fixture =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_recursive_traces");
    let source = tempfile::tempdir().unwrap();
    for name in ["main.av", "pool.av", "pooled.av", "aver.toml"] {
        std::fs::copy(fixture.join(name), source.path().join(name)).unwrap();
    }
    let main = source.path().join("main.av");
    let mut text = std::fs::read_to_string(&main).unwrap();
    text.push_str(r#"
fn corrupt(observed: __ParentTraceParentResult, mode: Int) -> __ParentTraceParentResult
    match mode
        0 -> __ParentTraceParentResult.update(observed, position = observed.position + 1)
        1 -> __ParentTraceParentResult.update(observed, consumed = observed.consumed + 1)
        2 -> __ParentTraceParentResult.update(observed, events = List.drop(observed.events, 1))
        _ -> __ParentTraceParentResult.update(observed, remaining = List.concat(observed.remaining, [__ParentTraceInput.Foreign]))

fn detectsCorruption(mode: Int) -> Bool
    inputs = [__ParentTraceInput.AnswerClaim(Option.Some(7)), __ParentTraceInput.Advance, __ParentTraceInput.AnswerClaim(Option.None), __ParentTraceInput.Advance]
    original = __parentSourceTrace(2, inputs)
    broken = corrupt(original, mode)
    Bool.and(original.value == broken.value, Bool.not(original == broken))

verify detectsCorruption
    detectsCorruption(0) => true
    detectsCorruption(1) => true
    detectsCorruption(2) => true
    detectsCorruption(3) => true
"#);
    for (mode, name) in ["position", "consumed", "events", "remaining"]
        .iter()
        .enumerate()
    {
        text.push_str(&format!("\nverify corrupt law {name}\n    given observed: __ParentTraceParentResult = [__parentSourceTrace(0, [])]\n    when observed.value == Option.Some(10)\n    when List.len(observed.events) > 0\n    using []\n    corrupt(observed, {mode}) == observed holds\n"));
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
    assert_eq!(summary["universal_laws"], 16, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    for name in ["position", "consumed", "events", "remaining"] {
        assert_eq!(
            summary["obligations"][format!("corrupt.{name}.implication")],
            "failed",
            "{summary}"
        );
    }
}

#[test]
fn recursive_splices_preserve_nominal_arguments_early_errors_and_in_place_effects() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_recursive_traces");
    let dir = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        source.join("mixed.av").to_str().unwrap(),
        dir.path(),
        0,
        &[],
        &["--module-root", source.to_str().unwrap()],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    for key in ["bounded_laws", "build_errors", "sorries"] {
        assert_eq!(summary[key], 0, "{summary}");
    }
    // Five trace obligations, and six more for the segment interface: this
    // process observes `__checkedStart` and `__checkedAnswerYield`, and each
    // one contributes a cursor bound, an event-history prefix and a protocol
    // step.
    assert_eq!(summary["universal_laws"], 11, "{summary}");
    audit(dir.path(), 11);
}

#[test]
fn implicit_trace_citations_keep_earlier_laws_when_adding_splice_dependencies() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let fixture =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_recursive_traces");
    let source = tempfile::tempdir().unwrap();
    for name in ["pool.av", "pooled.av", "aver.toml"] {
        std::fs::copy(fixture.join(name), source.path().join(name)).unwrap();
    }
    let text = std::fs::read_to_string(fixture.join("main.av")).unwrap();
    let text = text.replace("    using []\n", "").replacen(
        "fn loop",
        r#"fn marker(n: Int) -> Int
    n
verify marker law identity
    given n: Int = [0]
    marker(n) => n

fn loop"#,
        1,
    );
    let main = source.path().join("main.av");
    std::fs::write(&main, text).unwrap();
    let dir = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        main.to_str().unwrap(),
        dir.path(),
        0,
        &[],
        &["--module-root", source.path().to_str().unwrap()],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 17, "{summary}");
    audit(dir.path(), 17);
    let lean = std::fs::read_to_string(dir.path().join("RecursiveTraces.lean")).unwrap();
    let proof = lean
        .split("theorem __aver_reason___parentSourceTraceFrom_law_correspondence_implication")
        .nth(1)
        .unwrap()
        .split("theorem __parentSourceTraceFrom_law_correspondence :")
        .next()
        .unwrap();
    assert!(proof.contains("marker_law_identity"), "{proof}");
    assert!(proof.contains("Observed_law_splice"), "{proof}");
}

#[test]
fn recursive_imports_compose_universal_contracts_without_exposing_private_helpers() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_recursive_imports");
    for (entry, count) in [("main.av", 28), ("private.av", 13)] {
        let dir = tempfile::tempdir().unwrap();
        let (summary, run) = run_lean_check_json_with_args(
            source.join(entry).to_str().unwrap(),
            dir.path(),
            0,
            &[],
            &["--module-root", source.to_str().unwrap()],
        );
        assert!(run.status.success(), "{}", format_output(&run));
        for key in ["bounded_laws", "build_errors", "sorries"] {
            assert_eq!(summary[key], 0, "{summary}");
        }
        assert_eq!(summary["universal_laws"], count, "{summary}");
        audit(dir.path(), count);
    }
}
