use super::*;

const SOURCE: &str = include_str!("../fixtures/source_recursion/roundtrip.av");

#[test]
fn concrete_applications_roundtrip_passes_both_checkers_after_renaming_and_radix_change() {
    for (label, source) in [
        ("decimal", SOURCE.to_string()),
        ("equation_reversed", SOURCE.replace(
            "read(List.reverse(digits(value, [])), 0) => value",
            "value => read(List.reverse(digits(value, [])), 0)")),
        ("unrelated", SOURCE.replace("fn read(",
            "fn same(flag: Bool) -> Bool\n    flag\nverify same law identity\n    given flag: Bool = [false, true]\n    same(flag) => flag\n\nfn read(")),
        (
            "booleans",
            include_str!("../fixtures/source_recursion/boolean_counter.av").to_string(),
        ),
        (
            "renamed",
            SOURCE
                .replace("10", "16")
                .replace("read", "consume")
                .replace("digits", "collect")
                .replace("value", "remaining"),
        ),
    ] {
        let dir = temp_output_dir(&format!("aver-concrete-applications-{label}"));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("main.av");
        std::fs::write(&path, source).unwrap();
        for backend in ["dafny", "lean"] {
            let Some(summary) = super::source_recursion::check(path.to_str().unwrap(), backend)
            else {
                continue;
            };
            assert_eq!(summary["passed"], true, "{label}/{backend}: {summary}");
            if backend == "lean" {
                assert_eq!(summary["universal_laws"], if label == "unrelated" { 4 } else { 3 });
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn concrete_applications_reject_a_missing_domain_guard_and_a_false_supplier() {
    let missing_guard = SOURCE.replace("    when value >= 0\n", "");
    // This false supplier agrees at every retained zero/empty sample. Its
    // consumer is reflexive, so global rejection must come from the supplier.
    let false_supplier = SOURCE
        .replace("[-1, 0, 1, 10, 123]", "[0]")
        .replace("[[], [4], [2, 3]]", "[[]]")
        .replace(
            "List.concat(List.reverse(acc), digits(value, []))",
            "List.reverse(acc)",
        )
        .replace(
            "read(List.reverse(digits(value, [])), 0) => value",
            "digits(value, []) => digits(value, [])",
        );
    for (label, source) in [
        ("missing_guard", missing_guard),
        ("false_supplier", false_supplier),
    ] {
        let dir = temp_output_dir(&format!("aver-concrete-applications-{label}"));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("main.av");
        std::fs::write(&path, source).unwrap();
        let samples = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args(["verify", path.to_str().unwrap()])
            .output()
            .unwrap();
        assert!(
            samples.status.success(),
            "{label}: {}",
            format_output(&samples)
        );
        for backend in ["dafny", "lean"] {
            let checker = if backend == "lean" { "lake" } else { "dafny" };
            if Command::new(checker).arg("--version").output().is_err() {
                continue;
            }
            let output = Command::new(env!("CARGO_BIN_EXE_aver"))
                .args([
                    "proof",
                    path.to_str().unwrap(),
                    "--backend",
                    backend,
                    "--check-json",
                    "-o",
                ])
                .arg(dir.join(backend))
                .output()
                .unwrap();
            let stdout = String::from_utf8_lossy(&output.stdout);
            let summary: serde_json::Value = serde_json::from_str(
                stdout
                    .lines()
                    .rev()
                    .find(|line| line.starts_with('{'))
                    .unwrap_or_else(|| panic!("{}", format_output(&output))),
            )
            .unwrap();
            assert!(
                !output.status.success(),
                "{label}/{backend} must reject a false universal"
            );
            assert_eq!(summary["passed"], false, "{summary}");
            if backend == "lean" {
                assert_eq!(summary["build_errors"], 0, "{summary}");
                assert!(!summary["universal"].as_bool().unwrap(), "{summary}");
            } else {
                for field in ["axioms", "omitted"] {
                    assert_eq!(summary[field], 0, "{summary}");
                }
                let errors = summary["errors"].as_u64().unwrap();
                let timeouts = summary["timeouts"].as_u64().unwrap();
                // On the composed missing-guard claim Z3 may exhaust search
                // instead of constructing a counterexample. Either outcome
                // must refuse universal credit, without emission/axiom escapes.
                assert!(errors + timeouts > 0, "{summary}");
                if label == "false_supplier" {
                    assert_eq!(timeouts, 0, "{summary}");
                    assert!(errors > 0, "{summary}");
                }
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}
