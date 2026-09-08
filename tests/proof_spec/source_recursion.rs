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
        ("tests/fixtures/source_recursion/floor_digits.av", 7),
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

#[test]
fn dafny_native_mutual_sequence_laws_prove_universally_and_reject_sample_blind_spots() {
    let source = include_str!("../fixtures/source_recursion/native_sequence.av");
    for (name, source, expected) in [
        ("positive", source.to_string(), true),
        (
            "wrong_order",
            source.replace(
                "shuffle(shuffle(shuffle(items)))",
                "shuffle(shuffle(items))",
            ),
            false,
        ),
        (
            "missing_length",
            source.replace("    when List.len(items) >= 3\n", ""),
            false,
        ),
        (
            "missing_index",
            source.replace("    when Bool.and(index >= 0, index < 3)\n", ""),
            false,
        ),
    ] {
        let dir = temp_output_dir(&format!("aver-native-sequence-{name}"));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("main.av");
        std::fs::write(&path, source).unwrap();
        // Symmetric samples deliberately miss the permutation error; all
        // supplied lists/indices also satisfy the guards removed above.
        let vm = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args(["verify", path.to_str().unwrap()])
            .output()
            .unwrap();
        assert!(vm.status.success(), "{name}: {}", format_output(&vm));
        if let Some(summary) = check(path.to_str().unwrap(), "dafny") {
            assert_eq!(summary["passed"], expected, "{name}: {summary}");
            if !expected {
                assert!(summary["errors"].as_u64().unwrap() > 0, "{name}: {summary}");
            }
        }
    }
}

#[test]
fn dafny_native_sequence_search_preserves_import_owners_and_boolean_elements() {
    let source = include_str!("../fixtures/source_recursion/native_sequence.av");
    let (definitions, claims) = source.split_once("fn shuffle").unwrap();
    let dir = temp_output_dir("aver-native-sequence-import");
    std::fs::create_dir_all(&dir).unwrap();
    let dependency =
        definitions.replace("module NativeSequence", "module Lookup\n    exposes [at]");
    std::fs::write(dir.join("lookup.av"), dependency).unwrap();
    let entry = format!(
        "module Consumer\n    depends [Lookup]\n\nfn at(n: Int) -> Int\n    n + 99\n\nfn select(n: Int) -> Int\n    n + 77\n\nfn shuffle{}",
        claims.replace("at(items,", "Lookup.at(items,")
    );
    let path = dir.join("main.av");
    std::fs::write(&path, entry).unwrap();
    if let Some(summary) = check(path.to_str().unwrap(), "dafny") {
        assert_eq!(summary["passed"], true, "{summary}");
    }
    let bool_source = source
        .replace("List<Int>", "List<Bool>")
        .replace("head: Int", "head: Bool")
        .replace(") -> Int", ") -> Bool")
        .replace("[] -> 0", "[] -> false")
        .replace("[4, 4, 4]", "[true, true, true]")
        .replace("[9, 9, 9, 9]", "[false, false, false, false]");
    let path = dir.join("bool.av");
    std::fs::write(&path, bool_source).unwrap();
    if let Some(summary) = check(path.to_str().unwrap(), "dafny") {
        assert_eq!(summary["passed"], true, "{summary}");
    }
}

#[test]
fn dafny_floor_division_laws_keep_fixed_seeds_and_require_real_universal_proofs() {
    let source = include_str!("../fixtures/source_recursion/floor_digits.av");
    for (name, source, expected) in [
        ("positive", source.to_string(), true),
        (
            "radix_seven",
            source.replace("10", "7").replace("[1, 9]", "[1, 6]"),
            true,
        ),
        (
            "missing_digit_guard",
            source.replace("    when Bool.and(value > 0, value < 10)\n", ""),
            false,
        ),
        (
            "missing_range_guard",
            source.replace("    when Bool.or(item < 0, item >= 10)\n", ""),
            false,
        ),
        (
            "missing_positive_guard",
            source.replace("    when value > 0\n", ""),
            false,
        ),
        (
            "wrong_prefix",
            source.replace("[2, 3]", "[2, 2]").replace(
                "List.concat(List.reverse(acc), digits(value, []))",
                "List.concat(acc, digits(value, []))",
            ),
            false,
        ),
    ] {
        // A counterexample to one law does not need the automatic pool of
        // other true laws. Isolate it so quantified citations do not turn
        // a concrete rejection into a search timeout.
        let source = if expected {
            source
        } else {
            let target = match name {
                "missing_digit_guard" => "oneDigit",
                "missing_range_guard" => "outsideRange",
                "missing_positive_guard" => "positive",
                "wrong_prefix" => "prefix",
                _ => unreachable!(),
            };
            let mut blocks = source.split("verify digits law ");
            let definitions = blocks.next().unwrap();
            let law = blocks
                .find(|b| b.starts_with(&format!("{target}\n")))
                .unwrap();
            format!("{definitions}verify digits law {law}")
        };
        let dir = temp_output_dir(&format!("aver-floor-laws-{name}"));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("main.av");
        std::fs::write(&path, source).unwrap();
        let sampled = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args(["verify", path.to_str().unwrap()])
            .output()
            .unwrap();
        assert!(
            sampled.status.success(),
            "{name}: {}",
            format_output(&sampled)
        );
        if let Some(summary) = check(path.to_str().unwrap(), "dafny") {
            assert_eq!(summary["passed"], expected, "{name}: {summary}");
            if !expected {
                assert!(summary["errors"].as_u64().unwrap() > 0, "{summary}");
            }
        }
    }
}

#[test]
fn dafny_recursive_premise_guards_bind_list_projections_without_excusing_a_false_law() {
    let source = include_str!("../fixtures/source_recursion/guarded_list_seed.av");
    for (name, source, expected) in [
        ("positive", source.to_string(), true),
        ("false", source.replace("Int.abs(head)", "head"), false),
    ] {
        let dir = temp_output_dir(&format!("aver-list-premise-{name}"));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("main.av");
        std::fs::write(&path, source).unwrap();
        let vm = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args(["verify", path.to_str().unwrap()])
            .output()
            .unwrap();
        assert!(vm.status.success(), "{}", format_output(&vm));
        if let Some(summary) = check(path.to_str().unwrap(), "dafny") {
            assert_eq!(summary["passed"], expected, "{name}: {summary}");
        }
    }
}
