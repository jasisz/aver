use super::*;

const FIXTURES: &str = "tests/fixtures/dafny_guidance_spike";

fn backend_available(backend: &str) -> bool {
    let executable = if backend == "lean" { "lake" } else { "dafny" };
    Command::new(executable).arg("--version").output().is_ok()
}

fn check_fixture(name: &str, backend: &str) -> (serde_json::Value, std::process::Output, PathBuf) {
    let dir = temp_output_dir(&format!("aver-guidance-{name}-{backend}"));
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args([
            "proof",
            &format!("{FIXTURES}/{name}.av"),
            "--backend",
            backend,
            "--check-json",
            "-o",
        ])
        .arg(&dir)
        .output()
        .expect("run identical-source guidance fixture");
    let stdout = String::from_utf8_lossy(&run.stdout);
    let json = stdout
        .lines()
        .rev()
        .find(|line| line.starts_with('{'))
        .unwrap_or_else(|| panic!("no proof summary: {}", format_output(&run)));
    let summary = serde_json::from_str(json).expect("parse proof summary");
    (summary, run, dir)
}

fn lean_manifest(dir: &std::path::Path) -> serde_json::Value {
    serde_json::from_str(
        &std::fs::read_to_string(dir.join("proof_manifest.json")).expect("read Lean audit"),
    )
    .expect("parse Lean audit")
}

fn dafny_source(dir: &std::path::Path) -> String {
    std::fs::read_dir(dir)
        .expect("read Dafny output directory")
        .map(|entry| entry.expect("read output entry").path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "dfy"))
        .map(|path| std::fs::read_to_string(path).expect("read Dafny output"))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn dafny_guidance_positive_is_universal_on_both_backends() {
    let law_ids = ["nonnegative.guarded", "advance.guardedChain"];
    let step_ids = [
        "nonnegative.guarded.because1",
        "nonnegative.guarded.implication",
        "advance.guardedChain.because1",
        "advance.guardedChain.because2",
        "advance.guardedChain.because3",
        "advance.guardedChain.implication",
    ];
    for backend in ["lean", "dafny"] {
        if !backend_available(backend) {
            continue;
        }
        let (summary, run, dir) = check_fixture("positive", backend);
        assert!(run.status.success(), "{backend}: {}", format_output(&run));
        assert_eq!(summary["passed"], true, "{summary}");
        assert_eq!(summary["declined"].as_u64().unwrap_or(0), 0, "{summary}");
        if backend == "lean" {
            assert_eq!(summary["sorries"], 0, "{summary}");
            assert_eq!(summary["build_errors"], 0, "{summary}");
            let manifest = lean_manifest(&dir);
            for (kind, expected) in [
                ("laws", law_ids.as_slice()),
                ("obligations", step_ids.as_slice()),
            ] {
                let entries = manifest[kind].as_array().expect("claim array");
                assert_eq!(entries.len(), expected.len(), "{manifest}");
                for id in expected {
                    let claim = entries
                        .iter()
                        .find(|entry| entry["law"] == *id)
                        .unwrap_or_else(|| panic!("missing {id}: {manifest}"));
                    assert_eq!(claim["tier"], "universal", "{claim}");
                    assert!(
                        claim["axioms"]
                            .as_array()
                            .unwrap()
                            .iter()
                            .all(|axiom| matches!(
                                axiom.as_str(),
                                Some("Classical.choice" | "Quot.sound" | "propext")
                            )),
                        "{claim}"
                    );
                }
            }
        } else {
            for field in ["errors", "axioms", "omitted", "timeouts"] {
                assert_eq!(summary[field], 0, "{field}: {summary}");
            }
            let generated = dafny_source(&dir);
            for (marker, expected) in [
                ("// aver:dafny-law ", law_ids.as_slice()),
                ("// aver:dafny-obligation ", step_ids.as_slice()),
            ] {
                let ids: std::collections::BTreeSet<_> = generated
                    .lines()
                    .filter_map(|line| line.trim().strip_prefix(marker))
                    .filter_map(|line| line.split_whitespace().last())
                    .collect();
                assert_eq!(ids, expected.iter().copied().collect(), "{generated}");
            }
            let supplier = generated
                .lines()
                .filter_map(|line| line.trim().strip_prefix("// aver:dafny-law "))
                .find(|line| line.split_whitespace().last() == Some("nonnegative.guarded"))
                .and_then(|line| line.split_whitespace().next())
                .expect("helper lemma marker");
            let mut in_step = false;
            let mut caller_step = String::new();
            for line in generated.lines() {
                if line.trim().starts_with("// aver:dafny-") {
                    if in_step {
                        break;
                    }
                    in_step =
                        line.split_whitespace().last() == Some("advance.guardedChain.because1");
                } else if in_step {
                    caller_step.push_str(line);
                    caller_step.push('\n');
                }
            }
            assert!(
                caller_step.contains(&format!("{supplier}(")),
                "the local using citation must emit a checked lemma call: {caller_step}"
            );
            assert!(!generated.contains("assume {:axiom}"), "{generated}");
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn dafny_guidance_negative_claims_fail_both_strict_whole_file_gates() {
    for (fixture, failed_laws) in [
        ("missing_guard", &["advance.missingGuard"][..]),
        ("false_reason", &["identity.falseReason"][..]),
        ("restated_goal", &["identity.restatedGoal"][..]),
        (
            "failed_citation",
            &["identity.isZero", "forward.isZero"][..],
        ),
    ] {
        for backend in ["lean", "dafny"] {
            if !backend_available(backend) {
                continue;
            }
            let (summary, run, dir) = check_fixture(fixture, backend);
            assert!(
                !run.status.success(),
                "{fixture}/{backend}: {}",
                format_output(&run)
            );
            assert_eq!(summary["passed"], false, "{fixture}/{backend}: {summary}");
            assert_eq!(
                summary["declined"].as_u64().unwrap_or(0),
                0,
                "{fixture}/{backend}: {summary}"
            );
            if backend == "lean" {
                assert_eq!(summary["build_errors"], 0, "{summary}");
                assert!(summary["sorries"].as_u64().unwrap() > 0, "{summary}");
                let manifest = lean_manifest(&dir);
                for id in failed_laws {
                    let law = manifest["laws"]
                        .as_array()
                        .unwrap()
                        .iter()
                        .find(|law| law["law"] == *id)
                        .unwrap_or_else(|| panic!("missing {id}: {manifest}"));
                    assert_eq!(law["tier"], "failed", "{law}");
                }
            } else {
                assert!(summary["errors"].as_u64().unwrap() > 0, "{summary}");
                for field in ["axioms", "omitted", "timeouts"] {
                    assert_eq!(summary[field], 0, "{field}: {summary}");
                }
                // Dafny can verify a caller modularly against a failed helper's
                // contract. Only the strict whole-file result is proof credit.
                let generated = dafny_source(&dir);
                assert!(!generated.contains("assume {:axiom}"), "{generated}");
                for id in failed_laws {
                    assert!(
                        generated
                            .lines()
                            .any(|line| line.trim().starts_with("// aver:dafny-law ")
                                && line.split_whitespace().last() == Some(*id)),
                        "missing {id}: {generated}"
                    );
                }
            }
            let _ = std::fs::remove_dir_all(dir);
        }
    }
}

#[test]
fn dafny_guidance_samples_do_not_stand_in_for_universal_proofs() {
    for fixture in [
        "positive",
        "missing_guard",
        "restated_goal",
        "failed_citation",
        "recursive",
    ] {
        let run = Command::new(env!("CARGO_BIN_EXE_aver"))
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .args(["verify", &format!("{FIXTURES}/{fixture}.av")])
            .output()
            .expect("run declared samples");
        assert!(run.status.success(), "{fixture}: {}", format_output(&run));
    }
}

#[test]
fn dafny_guidance_recursive_control_is_verified_by_lean_and_declined_by_dafny() {
    for backend in ["lean", "dafny"] {
        if !backend_available(backend) {
            continue;
        }
        let (summary, run, dir) = check_fixture("recursive", backend);
        if backend == "lean" {
            assert!(run.status.success(), "{}", format_output(&run));
            assert_eq!(summary["passed"], true, "{summary}");
            assert_eq!(summary["sorries"], 0, "{summary}");
            assert_eq!(summary["build_errors"], 0, "{summary}");
            let manifest = lean_manifest(&dir);
            assert_eq!(
                manifest["laws"][0]["law"], "down.successorEquation",
                "{manifest}"
            );
            assert_eq!(manifest["laws"][0]["tier"], "universal", "{manifest}");
            assert!(
                manifest["obligations"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .all(|step| step["tier"] == "universal"),
                "{manifest}"
            );
        } else {
            assert!(!run.status.success(), "{}", format_output(&run));
            assert_eq!(summary["passed"], false, "{summary}");
            assert_eq!(summary["declined"], 1, "{summary}");
            assert_eq!(summary["errors"], 0, "{summary}");
            assert_eq!(summary["axioms"], 0, "{summary}");
            assert_eq!(summary["timeouts"], 0, "{summary}");
            assert_eq!(
                summary["declined_claims"][0]["claim"], "down.successorEquation",
                "{summary}"
            );
            assert_eq!(
                summary["declined_claims"].as_array().unwrap().len(),
                1,
                "{summary}"
            );
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}
