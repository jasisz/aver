use super::*;

fn check(source: &str, backend: &str) -> Option<serde_json::Value> {
    let checker = if backend == "lean" { "lake" } else { "dafny" };
    if Command::new(checker).arg("--version").output().is_err() {
        return None;
    }
    let dir = temp_output_dir("aver-source-recursion");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["proof", source, "--backend", backend, "--check-json", "-o"])
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
            .unwrap_or_else(|| panic!("{source}/{backend}: {}", format_output(&output))),
    )
    .unwrap();
    assert_eq!(summary["passed"], output.status.success(), "{summary}");
    for field in if backend == "lean" {
        &["build_errors"][..]
    } else {
        &["axioms", "omitted", "timeouts"][..]
    } {
        assert_eq!(summary[field], 0, "{source}/{backend}: {summary}; {dir:?}");
    }
    assert_eq!(summary["declined"].as_u64().unwrap_or(0), 0, "{summary}");
    if backend == "lean" {
        let manifest: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap(),
        )
        .unwrap();
        for law in manifest["laws"].as_array().unwrap() {
            assert_eq!(
                law["tier"],
                if output.status.success() {
                    "universal"
                } else {
                    "failed"
                },
                "{law}"
            );
            assert!(
                !output.status.success()
                    || law["axioms"].as_array().unwrap().iter().all(|a| matches!(
                        a.as_str(),
                        Some("propext" | "Classical.choice" | "Quot.sound")
                    )),
                "{law}"
            );
        }
    }
    Some(summary)
}

#[test]
fn source_recursion_identical_positive_sources_pass_both_checkers() {
    for (source, laws) in [
        ("tests/fixtures/guarded_countdown_digits.av", 4),
        ("tests/fixtures/source_recursion/list_fold.av", 3),
        ("tests/fixtures/source_recursion/imported/main.av", 2),
        ("tests/fixtures/source_recursion/sequence_growth.av", 3),
    ] {
        for backend in ["dafny", "lean"] {
            let Some(summary) = check(source, backend) else {
                continue;
            };
            assert_eq!(summary["passed"], true, "{source}/{backend}: {summary}");
            if backend == "lean" {
                assert_eq!(summary["universal_laws"], laws, "{summary}");
            }
        }
    }
}

#[test]
fn source_recursion_rejects_dropped_accumulator_and_failed_supplier() {
    for backend in ["dafny", "lean"] {
        let Some(summary) = check(
            "tests/fixtures/source_recursion/false_accumulator.av",
            backend,
        ) else {
            continue;
        };
        assert_eq!(summary["passed"], false, "{backend}: {summary}");
        let failures = if backend == "lean" {
            "sorries"
        } else {
            "errors"
        };
        assert!(
            summary[failures].as_u64().unwrap() >= if backend == "lean" { 1 } else { 2 },
            "{summary}"
        );
    }
}
