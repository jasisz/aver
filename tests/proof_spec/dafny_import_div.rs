use super::*;

const FIXTURES: &str = "tests/fixtures/dafny_guidance_import_div";
const POSITIVE: &[(&str, usize)] = &[
    ("imports_positive/main", 4),
    ("arithmetic_positive", 5),
    ("transitive_consumer/main", 2),
];
const NEGATIVE: &[&str] = &["imported_false/main", "arithmetic_false"];

fn command(fixture: &str, operation: &str) -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args([operation, &format!("{FIXTURES}/{fixture}.av")]);
    if let Some((directory, _)) = fixture.rsplit_once('/') {
        command.args(["--module-root", &format!("{FIXTURES}/{directory}")]);
    }
    command
}

fn run(fixture: &str, backend: &str) -> Option<(serde_json::Value, PathBuf)> {
    let checker = if backend == "lean" { "lake" } else { "dafny" };
    if Command::new(checker).arg("--version").output().is_err() {
        return None;
    }
    let dir = temp_output_dir(&format!(
        "aver-import-div-{}-{backend}",
        fixture.replace('/', "-")
    ));
    let output = command(fixture, "proof")
        .args(["--backend", backend, "--check-json", "-o"])
        .arg(&dir)
        .output()
        .expect("run imported arithmetic proof");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let summary: serde_json::Value = serde_json::from_str(
        stdout
            .lines()
            .rev()
            .find(|line| line.starts_with('{'))
            .unwrap_or_else(|| panic!("{fixture}/{backend}: {}", format_output(&output))),
    )
    .expect("parse proof summary");
    assert_eq!(summary["passed"], output.status.success(), "{summary}");
    Some((summary, dir))
}

fn assert_checked(summary: &serde_json::Value, backend: &str) {
    for field in if backend == "lean" {
        &["declined", "build_errors"][..]
    } else {
        &["declined", "axioms", "omitted", "timeouts"][..]
    } {
        assert_eq!(summary[field].as_u64().unwrap_or(0), 0, "{summary}");
    }
}

fn assert_universal_claims(dir: &std::path::Path, backend: &str, count: usize) {
    if backend == "lean" {
        let manifest: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap(),
        )
        .unwrap();
        let laws = manifest["laws"].as_array().unwrap();
        let obligations = manifest["obligations"].as_array().unwrap();
        assert_eq!(laws.len(), count, "{manifest}");
        assert_eq!(obligations.len(), count * 2, "{manifest}");
        for claim in laws.iter().chain(obligations) {
            assert_eq!(claim["tier"], "universal", "{claim}");
            assert!(
                claim["axioms"].as_array().unwrap().iter().all(|axiom| {
                    matches!(
                        axiom.as_str(),
                        Some("Classical.choice" | "Quot.sound" | "propext")
                    )
                }),
                "{claim}"
            );
        }
    } else {
        let generated = std::fs::read_dir(dir)
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.extension().is_some_and(|ext| ext == "dfy"))
            .map(|path| std::fs::read_to_string(path).unwrap())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!generated.contains("assume {:axiom}"), "{generated}");
        for (marker, expected) in [
            ("// aver:dafny-law ", count),
            ("// aver:dafny-obligation ", count * 2),
        ] {
            assert_eq!(
                generated
                    .lines()
                    .filter(|line| line.trim().starts_with(marker))
                    .count(),
                expected,
                "{generated}"
            );
        }
    }
}

#[test]
fn dafny_import_div_positive_sources_have_universal_credit_in_both_backends() {
    for &(fixture, count) in POSITIVE {
        for backend in ["lean", "dafny"] {
            let Some((summary, dir)) = run(fixture, backend) else {
                continue;
            };
            assert_checked(&summary, backend);
            assert_eq!(summary["passed"], true, "{fixture}/{backend}: {summary}");
            assert_eq!(
                summary[if backend == "lean" {
                    "sorries"
                } else {
                    "errors"
                }],
                0,
                "{summary}"
            );
            assert_universal_claims(&dir, backend, count);
            let _ = std::fs::remove_dir_all(dir);
        }
    }
}

#[test]
fn dafny_import_div_false_supplier_and_lost_recursive_guard_fail_actual_checking() {
    for fixture in NEGATIVE {
        for backend in ["lean", "dafny"] {
            let Some((summary, dir)) = run(fixture, backend) else {
                continue;
            };
            assert_checked(&summary, backend);
            assert_eq!(summary["passed"], false, "{fixture}/{backend}: {summary}");
            assert!(
                summary[if backend == "lean" {
                    "sorries"
                } else {
                    "errors"
                }]
                .as_u64()
                .unwrap()
                    > 0,
                "{summary}"
            );
            // The caller in imported_false is true. Its selected supplier is
            // false at value=1; checking the full imported proof is mandatory.
            let _ = std::fs::remove_dir_all(dir);
        }
    }
}

#[test]
fn dafny_import_div_transitive_unsupported_body_is_not_hidden_by_an_int_bool_signature() {
    let Some((summary, dir)) = run("imported_unsupported/main", "dafny") else {
        return;
    };
    assert_eq!(summary["passed"], false, "{summary}");
    assert!(summary["declined"].as_u64().unwrap() > 0, "{summary}");
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn dafny_import_div_finite_signed_zero_and_recursive_samples_pass() {
    for fixture in POSITIVE
        .iter()
        .map(|(fixture, _)| *fixture)
        .chain(NEGATIVE.iter().copied())
        .chain([
            "imported_unsupported/main",
            "plain_positive/main",
            "plain_false/main",
            "plain_positive/main_missing_guard",
            "plain_positive/main_implicit",
        ])
    {
        let output = command(fixture, "verify")
            .output()
            .expect("run imported arithmetic samples");
        assert!(
            output.status.success(),
            "{fixture}: {}",
            format_output(&output)
        );
    }
}

#[test]
fn dafny_import_div_explicit_plain_supplier_is_checked_universally() {
    // Lean currently exports this ordinary guarded supplier as bounded and
    // declines its use in a universal guided caller. Dafny checks the selected
    // supplier universally; its capability need not inherit that limitation.
    let Some((summary, dir)) = run("plain_positive/main", "dafny") else {
        return;
    };
    assert_checked(&summary, "dafny");
    assert_eq!(summary["passed"], true, "{summary}");
    assert_eq!(summary["errors"], 0, "{summary}");
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn dafny_import_div_plain_supplier_is_neither_assumed_true_nor_stripped_of_its_guard() {
    for fixture in ["plain_false/main", "plain_positive/main_missing_guard"] {
        for backend in ["lean", "dafny"] {
            let Some((summary, dir)) = run(fixture, backend) else {
                continue;
            };
            assert_checked(&summary, backend);
            assert_eq!(summary["passed"], false, "{fixture}/{backend}: {summary}");
            assert!(
                summary[if backend == "lean" {
                    "sorries"
                } else {
                    "errors"
                }]
                .as_u64()
                .unwrap()
                    > 0,
                "{summary}"
            );
            let _ = std::fs::remove_dir_all(dir);
        }
    }
}

#[test]
fn dafny_import_div_plain_supplier_support_does_not_enable_implicit_guidance() {
    let Some((summary, dir)) = run("plain_positive/main_implicit", "dafny") else {
        return;
    };
    assert_eq!(summary["passed"], false, "{summary}");
    assert!(summary["declined"].as_u64().unwrap() > 0, "{summary}");
    let _ = std::fs::remove_dir_all(dir);
}
