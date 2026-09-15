use super::*;

#[test]
fn cited_transitions_compose_and_missing_event_guards_stay_open() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let file = "tests/fixtures/cited_transition_histories.av";
    let samples = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(["verify", file])
        .output()
        .unwrap();
    assert!(samples.status.success(), "{}", format_output(&samples));
    let dir = temp_output_dir("aver-transition-histories");
    let (summary, run) = run_lean_check_json(file, &dir, 0, &[]);
    assert!(
        !run.status.success(),
        "the missing event guard must be rejected"
    );
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["universal_laws"], 4, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    for name in [
        "consume.everyFiniteHistory",
        "collect.growingListAccumulator",
    ] {
        assert_eq!(
            summary["obligations"][format!("{name}.implication")],
            "universal",
            "{summary}"
        );
    }
    assert_eq!(
        summary["obligations"]["consume.rejectsMissingEventGuard.implication"], "failed",
        "{summary}"
    );
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    for law in manifest["laws"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|l| l["tier"] == "universal")
    {
        for axiom in law["axioms"].as_array().unwrap() {
            assert!(
                ["propext", "Quot.sound", "Classical.choice"].contains(&axiom.as_str().unwrap()),
                "{law}"
            );
        }
    }
    let _ = std::fs::remove_dir_all(dir);
}
