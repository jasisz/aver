use super::*;

pub(super) fn check(source: &str, backend: &str) -> Option<serde_json::Value> {
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
fn dafny_bounded_unfolding_keeps_universal_values_and_checks_every_supplier_and_reason() {
    let source = include_str!("../fixtures/source_recursion/bounded_unfolding.av");
    for (name, source, expected) in [
        ("positive", source.to_string(), true),
        (
            "different_radix",
            source
                .replace(", 10)", ", 16)")
                .replace("acc * 10", "acc * 16")
                .replace("< 1000)", "< 4096)")
                .replace("< 100000)", "< 1048576)"),
            true,
        ),
        (
            "missing_guard",
            source.replace("    when Bool.and(value >= 0, value < 1000)\n", ""),
            false,
        ),
        (
            "false_reason",
            source.replace(
                "because read(digits(value, 5)) == value",
                "because read(digits(value, 5)) == value + 1",
            ),
            false,
        ),
        (
            "false_unused_supplier",
            source.replace("identity(value) => value", "identity(value) => value + 1"),
            false,
        ),
    ] {
        let dir = temp_output_dir(&format!("aver-bounded-unfolding-{name}"));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("main.av");
        std::fs::write(&path, source).unwrap();
        let Some(summary) = check(path.to_str().unwrap(), "dafny") else {
            return;
        };
        assert_eq!(summary["passed"], expected, "{name}: {summary}");
        if !expected {
            assert!(summary["errors"].as_u64().unwrap() > 0, "{name}: {summary}");
        }
    }
}

#[test]
fn dafny_bounded_unfolding_handles_imported_names_and_typed_reverse_helpers() {
    let dir = temp_output_dir("aver-bounded-import");
    std::fs::create_dir_all(&dir).unwrap();
    let source = include_str!("../fixtures/source_recursion/bounded_unfolding.av").replace(
        "module BoundedUnfolding",
        "module Codec\n    exposes [digits, read]",
    );
    std::fs::write(dir.join("codec.av"), source).unwrap();
    let entry = dir.join("main.av");
    std::fs::write(
        &entry,
        r#"module Main
    depends [Codec]
fn readFrom(n: Int) -> Int
    n + 100
fn digitsInto(n: Int) -> Int
    n + 200
fn identity(n: Int) -> Int
    n
verify identity law importedDigits
    given n: Int = [0, 999]
    when Bool.and(n >= 0, n < 1000)
    using []
    Codec.read(Codec.digits(n, 3)) => n
fn repeat(value: Bool, width: Int, acc: List<Bool>) -> List<Bool>
    match width <= 0
        true -> List.reverse(acc)
        false -> repeat(value, width - 1, List.prepend(value, acc))
verify repeat law threeValues
    given value: Bool = [true, false]
    using []
    repeat(value, 3, []) => [value, value, value]
"#,
    )
    .unwrap();
    let Some(summary) = check(entry.to_str().unwrap(), "dafny") else {
        return;
    };
    assert_eq!(summary["passed"], true, "{summary}");
}

#[test]
fn dafny_sequence_composition_preserves_order_and_checks_cited_fields() {
    let main = include_str!("../fixtures/source_recursion/framing/main.av");
    let codec = include_str!("../fixtures/source_recursion/framing/codec.av");
    let (before, reason_and_law) = main.split_once("fn roundtrip").unwrap();
    let (reason, law) = reason_and_law.split_once("verify send law").unwrap();
    let false_reason = format!(
        "{before}fn roundtrip{}verify send law{law}",
        reason.replace("rest = suffix", "rest = List.reverse(suffix)")
    );
    for (name, source, expected) in [
        ("positive", main.to_string(), true),
        ("reordered_reason", false_reason, false),
        (
            "reordered_claim",
            main.replace(
                "rest = suffix, consumed = List.len",
                "rest = List.reverse(suffix), consumed = List.len",
            ),
            false,
        ),
        (
            "false_supplier",
            main.replacen(
                "Packet(value = value, rest = suffix, consumed = 4)",
                "Packet(value = value + 1, rest = suffix, consumed = 4)",
                1,
            ),
            false,
        ),
    ] {
        let dir = temp_output_dir(&format!("aver-framing-{name}"));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("codec.av"), codec).unwrap();
        let path = dir.join("main.av");
        std::fs::write(&path, source).unwrap();
        if name.starts_with("reordered") {
            // Empty/singleton samples cannot distinguish either reordering.
            // The universal checker must still reject the incorrect law.
            let sampled = Command::new(env!("CARGO_BIN_EXE_aver"))
                .args([
                    "verify",
                    path.to_str().unwrap(),
                    "--module-root",
                    dir.to_str().unwrap(),
                ])
                .output()
                .unwrap();
            assert!(sampled.status.success(), "{}", format_output(&sampled));
        }
        let Some(summary) = check(path.to_str().unwrap(), "dafny") else {
            return;
        };
        assert_eq!(summary["passed"], expected, "{name}: {summary}");
        if !expected {
            assert!(summary["errors"].as_u64().unwrap() > 0, "{name}: {summary}");
        }
    }
    let Some(summary) = check("tests/fixtures/source_recursion/framing/typed.av", "dafny") else {
        return;
    };
    assert_eq!(summary["passed"], true, "{summary}");
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
