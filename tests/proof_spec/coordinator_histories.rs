use super::*;

fn assert_audited_histories(dir: &std::path::Path) {
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    let laws = manifest["laws"].as_array().unwrap();
    for name in ["noNewProcesses", "retiredInstanceNeverReturns"] {
        assert!(
            laws.iter()
                .any(|law| law["law"] == format!("__historyRun.{name}"))
        );
    }
    // There is no job bound to hold: the engine queues a job begun at the
    // limit, so the generated source names no limit.
    assert!(
        !laws
            .iter()
            .any(|law| law["law"] == "__historyRun.jobsStayWithinLimit")
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
fn histories_cover_two_job_kinds_and_a_coordinator_without_jobs() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    for (fixture, jobs) in [("run_two_job_kinds", true), ("run_then", false)] {
        let module_root = format!("tests/fixtures/{fixture}");
        let dir = tempfile::tempdir().unwrap();
        let (summary, run) = run_lean_check_json_with_args(
            &format!("{module_root}/main.av"),
            dir.path(),
            0,
            &[],
            &["--module-root", &module_root],
        );
        assert!(run.status.success(), "{}", format_output(&run));
        for key in ["bounded_laws", "build_errors", "sorries"] {
            assert_eq!(summary[key], 0, "{summary}");
        }
        assert_audited_histories(dir.path());
        let entry = std::fs::read_to_string(dir.path().join("Node.lean")).unwrap();
        assert!(entry.contains("(events : List __HistoryEvent)"));
        // Neither a hand-written companion nor sampled list enumeration is
        // the proof: the emitted source law inducts on the whole list.
        assert!(
            entry.contains("induction events generalizing run"),
            "missing history induction"
        );
        if jobs {
            assert_eq!(summary["universal_laws"], 28, "{summary}");
        }
    }
}

#[test]
fn passing_empty_histories_cannot_hide_missing_safety_premises() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source = tempfile::tempdir().unwrap();
    let fixture =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/run_guide_example");
    for name in [
        "main.av",
        "clock.av",
        "clocked.av",
        "scoring.av",
        "aver.toml",
    ] {
        std::fs::copy(fixture.join(name), source.path().join(name)).unwrap();
    }
    let main = source.path().join("main.av");
    let mut text = std::fs::read_to_string(&main).unwrap();
    text.push_str(r#"
fn unguardedHistorySeatsAProcess() -> Int
    Map.len(__historyRun(__fresh(), [__HistoryEvent.SettleTicker(99, 0 - 1, __tickerStart())]).slots)

verify unguardedHistorySeatsAProcess
    unguardedHistorySeatsAProcess() => 1

verify __historyRun law missingAdmissibility
    given run: __Run = [__fresh()]
    given events: List<__HistoryEvent> = [[]]
    using []
    Map.len(__historyRun(run, events).slots) <= Map.len(run.slots) holds

verify __historyRun law missingInitialRetirement
    given run: __Run = [__sampleRun()]
    given events: List<__HistoryEvent> = [[]]
    given id: Int = [99]
    given seq: Int = [0]
    when __historyAdmissible(run, events)
    using []
    __retired(__historyRun(run, events), id, seq) holds
"#);
    std::fs::write(&main, text).unwrap();
    let cases = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(source.path())
        .args(["verify", "main.av", "--module-root", "."])
        .output()
        .unwrap();
    assert!(cases.status.success(), "{}", format_output(&cases));
    let target = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        main.to_str().unwrap(),
        target.path(),
        0,
        &[],
        &["--module-root", source.path().to_str().unwrap()],
    );
    assert!(!run.status.success(), "false laws passed: {summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    assert_eq!(summary["universal_laws"], 26, "{summary}");
    for law in ["missingAdmissibility", "missingInitialRetirement"] {
        assert_eq!(
            summary["obligations"][format!("__historyRun.{law}.implication")],
            "failed",
            "{summary}"
        );
    }
}
