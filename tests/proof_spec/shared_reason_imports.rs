use super::*;

#[test]
fn imported_aliases_and_both_recursive_paths_keep_their_owners_and_premises() {
    let original = include_str!("../fixtures/shared_reason_imports/Paths.av");
    let consumer = include_str!("../fixtures/shared_reason_imports/Consumer.av");
    for (mutation, library, source, expected) in [
        ("positive", original.to_string(), consumer.to_string(), true),
        (
            "lost_guard",
            original.to_string(),
            consumer.replace(">= -100", ">= -101"),
            false,
        ),
        (
            "false_branch",
            original.replace("step(alias)", "n - 1000"),
            consumer.to_string(),
            false,
        ),
    ] {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("Paths.av"), library).unwrap();
        let entry = dir.path().join("Consumer.av");
        std::fs::write(&entry, source).unwrap();
        // Deliberately incomplete samples pass all variants; only universal
        // checking sees the missing premise and the untested false branch.
        let samples = Command::new(env!("CARGO_BIN_EXE_aver"))
            .arg("verify")
            .arg(&entry)
            .arg("--module-root")
            .arg(dir.path())
            .output()
            .unwrap();
        assert!(
            samples.status.success(),
            "{mutation}: {}",
            format_output(&samples)
        );
        for backend in ["lean", "dafny"] {
            if let Some(summary) = super::source_recursion::check(entry.to_str().unwrap(), backend)
            {
                assert_eq!(
                    summary["passed"], expected,
                    "{mutation}/{backend}: {summary}"
                );
            }
        }
    }
}
