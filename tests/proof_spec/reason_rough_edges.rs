use super::*;

#[test]
fn conditional_probe_handles_an_empty_unfold_set_with_sibling_lemmas() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-empty-simp");
    let probe_log = dir.with_extension("probe.log");
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args([
            "proof",
            "tests/fixtures/conditional_empty_simp.av",
            "--check-json",
            "-o",
        ])
        .arg(&dir)
        .env("AVER_SPECULATIVE_LOG", &probe_log)
        .output()
        .unwrap();
    let probe = std::fs::read_to_string(&probe_log).unwrap();
    assert!(!probe.contains("unexpected token"), "{probe}");
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    let sibling = manifest["laws"]
        .as_array()
        .unwrap()
        .iter()
        .find(|law| law["law"] == "digits.doesNotAddOutsideDigits")
        .unwrap();
    assert_eq!(sibling["tier"], "universal", "{}", format_output(&run));
    let _ = std::fs::remove_dir_all(dir);
    let _ = std::fs::remove_file(probe_log);
}

#[test]
fn cited_accumulator_equations_do_not_loop_as_simp_rules_in_reasons() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-cited-accumulator");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/cited_accumulator_reason.av", &dir, 0, &[]);
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["universal_laws"], 3, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
    assert_eq!(
        summary["obligations"]["digits.positivePrefixAndDigit.because1"],
        "universal"
    );
    assert_eq!(
        summary["obligations"]["digits.positivePrefixAndDigit.implication"],
        "universal"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn imported_empty_list_comparisons_keep_types_in_samples_and_guards() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-empty-list-comparisons");
    let (summary, run) = run_lean_check_json_with_args(
        "tests/fixtures/typed_empty_comparisons/main.av",
        &dir,
        0,
        &[],
        &["--module-root", "tests/fixtures/typed_empty_comparisons"],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["universal_laws"], 4, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
    let _ = std::fs::remove_dir_all(dir);
}
