use super::*;

const FIXTURES: &str = "tests/fixtures/dafny_guidance_structured";
const POSITIVE: &[(&str, &[&str])] = &[
    (
        "btc_stackitem_slice",
        &[
            "bigEndian.largerPrefixStaysLarger",
            "bigEndian.positivePrefixStaysPositive",
        ],
    ),
    (
        "adt_positive",
        &["rebuild.identity", "wrap.rebuilt", "allRebuilt.identity"],
    ),
    ("result_positive", &["rebuild.identity", "unwrap.success"]),
];
const NEGATIVE: &[(&str, &[&str])] = &[
    ("missing_guard", &["bigEndian.missingGuard"]),
    ("recursive_false_reason", &["allZero.falseReason"]),
    (
        "failed_citation",
        &["identity.onlyZero", "forward.onlyZero"],
    ),
];

fn run_proof(fixture: &str, backend: &str) -> Option<(serde_json::Value, PathBuf)> {
    let checker = if backend == "lean" { "lake" } else { "dafny" };
    if Command::new(checker).arg("--version").output().is_err() {
        return None;
    }
    let dir = temp_output_dir(&format!("aver-structured-{fixture}-{backend}"));
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args([
            "proof",
            &format!("{FIXTURES}/{fixture}.av"),
            "--backend",
            backend,
            "--check-json",
            "-o",
        ])
        .arg(&dir)
        .output()
        .expect("run structured proof");
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
    assert_eq!(summary["declined"].as_u64().unwrap_or(0), 0, "{summary}");
    for field in if backend == "lean" {
        &["build_errors"][..]
    } else {
        &["axioms", "omitted", "timeouts"][..]
    } {
        assert_eq!(summary[field], 0, "{fixture}/{backend}: {summary}");
    }
    Some((summary, dir))
}

fn assert_claims(dir: &std::path::Path, backend: &str, laws: &[&str], positive: bool) {
    if backend == "lean" {
        let manifest: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap(),
        )
        .unwrap();
        let entries = manifest["laws"].as_array().unwrap();
        assert_eq!(entries.len(), laws.len(), "{manifest}");
        for id in laws {
            let claim = entries.iter().find(|claim| claim["law"] == *id).unwrap();
            assert_eq!(
                claim["tier"],
                if positive { "universal" } else { "failed" },
                "{claim}"
            );
        }
        if positive {
            let obligations = manifest["obligations"].as_array().unwrap();
            assert_eq!(obligations.len(), laws.len() * 2, "{manifest}");
            for claim in entries.iter().chain(obligations) {
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
            (
                "// aver:dafny-law ",
                laws.iter().map(|id| (*id).to_owned()).collect::<Vec<_>>(),
            ),
            (
                "// aver:dafny-obligation ",
                laws.iter()
                    .flat_map(|id| [format!("{id}.because1"), format!("{id}.implication")])
                    .collect(),
            ),
        ] {
            let actual: std::collections::BTreeSet<_> = generated
                .lines()
                .filter_map(|line| line.trim().strip_prefix(marker))
                .filter_map(|line| line.split_whitespace().last())
                .collect();
            assert_eq!(
                actual,
                expected.iter().map(String::as_str).collect(),
                "{generated}"
            );
        }
    }
}

#[test]
fn dafny_structured_identical_positive_sources_pass_both_backends() {
    for &(fixture, laws) in POSITIVE {
        for backend in ["lean", "dafny"] {
            let Some((summary, dir)) = run_proof(fixture, backend) else {
                continue;
            };
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
            assert_claims(&dir, backend, laws, true);
            let _ = std::fs::remove_dir_all(dir);
        }
    }
}

#[test]
fn dafny_structured_false_steps_and_citations_fail_whole_file_gates() {
    for &(fixture, laws) in NEGATIVE {
        for backend in ["lean", "dafny"] {
            let Some((summary, dir)) = run_proof(fixture, backend) else {
                continue;
            };
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
            // A caller may verify modularly against a failed supplier contract;
            // only a successful complete file is positive Dafny proof credit.
            assert_claims(&dir, backend, laws, false);
            let _ = std::fs::remove_dir_all(dir);
        }
    }
}

#[test]
fn dafny_structured_finite_samples_do_not_replace_universal_checks() {
    for &(fixture, _) in POSITIVE.iter().chain(NEGATIVE) {
        let output = Command::new(env!("CARGO_BIN_EXE_aver"))
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .args(["verify", &format!("{FIXTURES}/{fixture}.av")])
            .output()
            .expect("run structured samples");
        assert!(
            output.status.success(),
            "{fixture}: {}",
            format_output(&output)
        );
    }
}
