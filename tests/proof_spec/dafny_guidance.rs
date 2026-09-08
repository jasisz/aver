use super::*;

const FIXTURES: &str = "tests/fixtures/dafny_guidance_spike";

fn backend_available(backend: &str) -> bool {
    let executable = if backend == "lean" { "lake" } else { "dafny" };
    Command::new(executable).arg("--version").output().is_ok()
}

fn check_fixture(name: &str, backend: &str) -> (serde_json::Value, std::process::Output, PathBuf) {
    let dir = temp_output_dir(&format!("aver-guidance-{name}-{backend}"));
    let source = if name == "k5_integerorder" {
        "projects/k5_fdiv/domain/integerorder.av".to_owned()
    } else {
        format!("{FIXTURES}/{name}.av")
    };
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["proof", &source, "--backend", backend, "--check-json", "-o"])
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
    assert_positive_claims("positive", &law_ids, &step_ids, &["lean", "dafny"]);
}

fn assert_positive_claims(name: &str, law_ids: &[&str], step_ids: &[&str], backends: &[&str]) {
    for backend in backends.iter().copied() {
        if !backend_available(backend) {
            continue;
        }
        let (summary, run, dir) = check_fixture(name, backend);
        assert!(run.status.success(), "{backend}: {}", format_output(&run));
        assert_eq!(summary["passed"], true, "{summary}");
        assert_eq!(summary["declined"].as_u64().unwrap_or(0), 0, "{summary}");
        if backend == "lean" {
            assert_eq!(summary["sorries"], 0, "{summary}");
            assert_eq!(summary["build_errors"], 0, "{summary}");
            let manifest = lean_manifest(&dir);
            for (kind, expected) in [("laws", law_ids), ("obligations", step_ids)] {
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
                ("// aver:dafny-law ", law_ids),
                ("// aver:dafny-obligation ", step_ids),
            ] {
                let ids: std::collections::BTreeSet<_> = generated
                    .lines()
                    .filter_map(|line| line.trim().strip_prefix(marker))
                    .filter_map(|line| line.split_whitespace().last())
                    .collect();
                assert_eq!(ids, expected.iter().copied().collect(), "{generated}");
            }
            let citation = match name {
                "positive" => Some(("nonnegative.guarded", "advance.guardedChain.because1")),
                "nonlinear_positive" => Some((
                    "multiply.nonnegativeFactors",
                    "orderedProducts.nonnegativeFactor.because1",
                )),
                _ => None,
            };
            if let Some((supplier_id, caller_id)) = citation {
                let supplier = generated
                    .lines()
                    .filter_map(|line| line.trim().strip_prefix("// aver:dafny-law "))
                    .find(|line| line.split_whitespace().last() == Some(supplier_id))
                    .and_then(|line| line.split_whitespace().next())
                    .expect("helper lemma marker");
                let mut in_step = false;
                let mut caller_step = String::new();
                for line in generated.lines() {
                    if line.trim().starts_with("// aver:dafny-") {
                        if in_step {
                            break;
                        }
                        in_step = line.split_whitespace().last() == Some(caller_id);
                    } else if in_step {
                        caller_step.push_str(line);
                        caller_step.push('\n');
                    }
                }
                assert!(
                    caller_step.contains(&format!("{supplier}(")),
                    "the local using citation must emit a checked lemma call: {caller_step}"
                );
            }
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
        (
            "nonlinear_missing_sign",
            &["orderedProducts.missingSign"][..],
        ),
        ("nonlinear_zero_factor", &["cancelled.missingNonzero"][..]),
        (
            "nonlinear_failed_citation",
            &["square.equalsInput", "forwardedSquare.equalsInput"][..],
        ),
        ("recursive_false_reason", &["bad.falseInductionReason"][..]),
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
                if fixture == "recursive_false_reason" {
                    assert!(generated.contains("{:induction n}"), "{generated}");
                }
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
        "recursive_false_reason",
        "nonlinear_identity",
        "nonlinear_positive",
        "nonlinear_missing_sign",
        "nonlinear_zero_factor",
        "nonlinear_failed_citation",
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
fn dafny_guidance_recursive_control_is_universal_on_both_backends() {
    assert_positive_claims(
        "recursive",
        &["down.successorEquation", "down.nonpositiveBase"],
        &[
            "down.successorEquation.because1",
            "down.successorEquation.implication",
            "down.nonpositiveBase.because1",
            "down.nonpositiveBase.implication",
        ],
        &["lean", "dafny"],
    );
}

#[test]
fn dafny_guidance_nonlinear_identity_is_universal_on_both_backends() {
    assert_positive_claims(
        "nonlinear_identity",
        &["multiply.distributive"],
        &[
            "multiply.distributive.because1",
            "multiply.distributive.implication",
        ],
        &["lean", "dafny"],
    );
}

#[test]
fn dafny_guidance_nonlinear_order_uses_dafny_arithmetic_and_local_citation() {
    assert_positive_claims(
        "nonlinear_positive",
        &[
            "multiply.nonnegativeFactors",
            "square.nonnegative",
            "orderedProducts.nonnegativeFactor",
            "cancelled.nonzeroFactor",
        ],
        &[
            "multiply.nonnegativeFactors.because1",
            "multiply.nonnegativeFactors.implication",
            "square.nonnegative.because1",
            "square.nonnegative.implication",
            "orderedProducts.nonnegativeFactor.because1",
            "orderedProducts.nonnegativeFactor.because2",
            "orderedProducts.nonnegativeFactor.implication",
            "cancelled.nonzeroFactor.because1",
            "cancelled.nonzeroFactor.because2",
            "cancelled.nonzeroFactor.implication",
        ],
        &["dafny"],
    );
}

#[test]
fn dafny_guidance_original_k5_integerorder_is_universal_on_both_backends() {
    assert_positive_claims(
        "k5_integerorder",
        &[
            "multiplyLe.nonnegativeFactor",
            "multiplyLt.positiveFactor",
            "nonnegativeProduct.nonnegativeFactors",
            "positiveProduct.positiveFactors",
        ],
        &[
            "multiplyLe.nonnegativeFactor.because1",
            "multiplyLe.nonnegativeFactor.implication",
            "multiplyLt.positiveFactor.because1",
            "multiplyLt.positiveFactor.implication",
            "nonnegativeProduct.nonnegativeFactors.because1",
            "nonnegativeProduct.nonnegativeFactors.implication",
            "positiveProduct.positiveFactors.because1",
            "positiveProduct.positiveFactors.implication",
        ],
        &["lean", "dafny"],
    );
}
