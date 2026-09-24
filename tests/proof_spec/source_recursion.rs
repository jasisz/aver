use super::*;

pub(super) fn check(source: &str) -> Option<serde_json::Value> {
    if Command::new("lake").arg("--version").output().is_err() {
        return None;
    }
    let dir = temp_output_dir("aver-source-recursion");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["proof", source, "--check-json", "-o"])
        .arg(&dir)
        .arg("--module-root")
        .arg(std::path::Path::new(source).parent().unwrap())
        .output()
        .expect("run source-recursion proof");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let summary: serde_json::Value = serde_json::from_str(
        stdout
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap_or_else(|| panic!("{source}: {}", format_output(&output))),
    )
    .unwrap();
    assert_eq!(summary["passed"], output.status.success(), "{summary}");
    assert_eq!(summary["build_errors"], 0, "{source}: {summary}; {dir:?}");
    assert_eq!(summary["declined"].as_u64().unwrap_or(0), 0, "{summary}");
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    for law in manifest["laws"].as_array().unwrap() {
        if output.status.success() {
            assert_eq!(law["tier"], "universal", "{law}");
        }
        assert!(
            law["tier"] != "universal"
                || law["axioms"].as_array().unwrap().iter().all(|a| matches!(
                    a.as_str(),
                    Some("propext" | "Classical.choice" | "Quot.sound")
                )),
            "{law}"
        );
    }
    Some(summary)
}

#[test]
fn source_recursion_identical_positive_sources_pass() {
    for (source, laws) in [
        ("tests/fixtures/guarded_countdown_digits.av", 4),
        ("tests/fixtures/source_recursion/list_fold.av", 3),
        ("tests/fixtures/source_recursion/imported/main.av", 2),
        ("tests/fixtures/source_recursion/sequence_growth.av", 3),
        ("tests/fixtures/source_recursion/floor_digits.av", 7),
        ("tests/fixtures/source_recursion/floor_citation.av", 9),
    ] {
        let Some(summary) = check(source) else {
            continue;
        };
        assert_eq!(summary["passed"], true, "{source}: {summary}");
        assert_eq!(summary["universal_laws"], laws, "{summary}");
    }
}

#[test]
fn source_recursion_rejects_dropped_accumulator_and_failed_supplier() {
    let Some(summary) = check("tests/fixtures/source_recursion/false_accumulator.av") else {
        return;
    };
    assert_eq!(summary["passed"], false, "{summary}");
    // A citation may reuse the ordinary supplier instead of failing a
    // second proof of the same false statement. The original supplier
    // must still fail and keep the entire file outside universal credit.
    assert!(summary["sorries"].as_u64().unwrap() > 0, "{summary}");
}
