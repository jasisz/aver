use super::*;

#[test]
fn knowledge_provider_and_coordinator_laws_are_universal_and_audited() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-knowledge-provider-laws");
    let (summary, run) = run_lean_check_json_with_args(
        "examples/knowledge/main.av",
        &dir,
        0,
        &[],
        &["--module-root", "examples/knowledge"],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 53, "{summary}");
    for key in ["bounded_laws", "build_errors", "sorries"] {
        assert_eq!(summary[key], 0, "{summary}");
    }
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    let laws = manifest["laws"].as_array().unwrap();
    for name in [
        "Stored.offer.rejectedBatchChangesNothing",
        "Stored.offer.admittedBatchUsesTheModel",
        "Stored.accepted.knowledgeCommutes",
        "Stored.accepted.peersCannotWriteVerdicts",
        "Stored.read.stableAnswer",
        "Stored.read.stableHistory",
        "Stored.validated.failedWorkAddsNoKnowledge",
        "Stored.validated.workCannotRewriteBodies",
        "Knowledge.runBatches.anySchedule",
        "__consumedValidation.aStartedTaskIsNotAskedAgain",
    ] {
        assert!(laws.iter().any(|law| law["law"] == name), "missing {name}");
    }
    for law in laws {
        assert_eq!(law["tier"], "universal", "{law}");
        for axiom in law["axioms"].as_array().unwrap() {
            assert!(
                ["propext", "Quot.sound", "Classical.choice"].contains(&axiom.as_str().unwrap()),
                "{law}"
            );
        }
    }
    let _ = std::fs::remove_dir_all(dir);
}
