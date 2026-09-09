use super::*;

const SOURCE: &str = include_str!("../fixtures/source_recursion/floor_citation.av");

// The positive corpus needs its decomposition pool. For a false supplier,
// isolate the counterexample from unrelated quantified sibling facts: otherwise
// Z3 can exhaust its search budget instead of reporting the false statement.
// Keep the same source function, false claim and misleading zero samples.
fn isolated_false_supplier() -> String {
    let definitions = SOURCE.split_once("verify digits law prefix").unwrap().0;
    let law = SOURCE
        .split_once("verify digits law singletonPrefix")
        .unwrap()
        .1
        .split_once("verify digits law oneDigit")
        .unwrap()
        .0;
    format!("{definitions}verify digits law singletonPrefix{law}")
        .replace("given item: Int = [2, 3]", "given item: Int = [0]")
        .replace(
            "digits(value, [item]) => List.prepend(item, digits(value, []))",
            "digits(value, [item]) => List.prepend(0, digits(value, []))",
        )
}

#[test]
fn dafny_citation_reuse_keeps_guards_and_checks_false_unused_suppliers() {
    let (ordinary, guided) = SOURCE.split_once("verify digits law citedPrefix").unwrap();
    let (before_positive, positive) = guided
        .split_once("verify digits law citedPositive")
        .unwrap();
    let false_ordinary = isolated_false_supplier();
    for (name, source, expected) in [
        ("positive", SOURCE.to_string(), true),
        (
            "forward_supplier",
            format!(
                "{}verify digits law citedPrefix{guided}{}",
                ordinary.split_once("verify digits law prefix").unwrap().0,
                &ordinary[ordinary.find("verify digits law prefix").unwrap()..]
            ),
            true,
        ),
        (
            "missing_premise",
            format!(
                "{ordinary}verify digits law citedPrefix{before_positive}verify digits law citedPositive{}",
                positive.replace("    when value > 0\n", "")
            ),
            false,
        ),
        (
            "false_unused_supplier",
            format!(
                "{false_ordinary}\nverify digits law citedPrefix{}",
                before_positive.replace(
                    "digits(value, [item]) => List.prepend(item, digits(value, []))",
                    "digits(value, [item]) => digits(value, [item])"
                )
            ),
            false,
        ),
    ] {
        let dir = temp_output_dir(&format!("aver-citation-reuse-{name}"));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("main.av");
        let source = if name == "false_unused_supplier" {
            source.replace("given item: Int = [2, 3]", "given item: Int = [0]")
        } else {
            source
        };
        std::fs::write(&path, source).unwrap();
        // Zero-valued samples conceal the false arbitrary-item supplier.
        let samples = Command::new(env!("CARGO_BIN_EXE_aver"))
            .arg("verify")
            .arg(&path)
            .output()
            .unwrap();
        assert!(
            samples.status.success(),
            "{name}: {}",
            format_output(&samples)
        );
        let Some(summary) = super::source_recursion::check(path.to_str().unwrap(), "dafny") else {
            return;
        };
        assert_eq!(summary["passed"], expected, "{name}: {summary}");
        if !expected {
            assert!(summary["errors"].as_u64().unwrap() > 0, "{name}: {summary}");
        }
    }
}

#[test]
fn dafny_citation_reuse_retains_import_owner_and_module_local_earlier_laws() {
    for false_supplier in [false, true] {
        let dir = temp_output_dir("aver-citation-reuse-import");
        std::fs::create_dir_all(&dir).unwrap();
        let source = if false_supplier {
            isolated_false_supplier()
        } else {
            SOURCE.to_string()
        }
        .replace("module FloorCitation", "module Codec\n    exposes [digits]");
        std::fs::write(dir.join("codec.av"), source).unwrap();
        let entry = dir.join("main.av");
        std::fs::write(
            &entry,
            r#"module Main
    depends [Codec]
fn digits(value: Int, acc: List<Int>) -> List<Int>
    acc
verify digits law singletonPrefix
    given value: Int = [0, 1]
    given acc: List<Int> = [[], [8]]
    digits(value, acc) => acc
verify digits law importedPrefix
    given value: Int = [0, 123]
    given item: Int = [0]
    using [Codec.digits.singletonPrefix]
    Codec.digits(value, [item]) => List.prepend(item, Codec.digits(value, []))
"#,
        )
        .unwrap();
        let Some(summary) = super::source_recursion::check(entry.to_str().unwrap(), "dafny") else {
            return;
        };
        assert_eq!(summary["passed"], !false_supplier, "{summary}");
        if false_supplier {
            assert!(summary["errors"].as_u64().unwrap() > 0, "{summary}");
        }
    }
}

#[test]
fn dafny_citation_reuse_does_not_promote_a_finite_mutual_law() {
    let dir = temp_output_dir("aver-citation-reuse-finite");
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("main.av");
    std::fs::write(
        &path,
        r#"module FiniteCitation
fn at(items: List<Int>, index: Int) -> Int
    match items
        [] -> 0
        [head, ..tail] -> select(head, tail, index)
fn select(head: Int, tail: List<Int>, index: Int) -> Int
    match index == 0
        true -> head
        false -> at(tail, index - 1)
verify at law sampled
    given n: Int = [0, 1]
    at([n], 0) => Int.abs(n)
verify at law reflexiveConsumer
    given n: Int = [0, 1]
    using [at.sampled]
    at([n], 0) => at([n], 0)
"#,
    )
    .unwrap();
    let samples = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("verify")
        .arg(&path)
        .output()
        .unwrap();
    assert!(samples.status.success(), "{}", format_output(&samples));
    let Some(summary) = super::source_recursion::check(path.to_str().unwrap(), "dafny") else {
        return;
    };
    assert_eq!(summary["passed"], false, "{summary}");
    assert!(summary["errors"].as_u64().unwrap() > 0, "{summary}");
}

#[test]
fn dafny_citation_reuse_supports_an_ordinary_native_sequence_universal() {
    let dir = temp_output_dir("aver-citation-reuse-native-sequence");
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("main.av");
    let source = include_str!("../fixtures/source_recursion/native_sequence.av");
    std::fs::write(
        &path,
        format!(
            "{source}\n{}",
            r#"verify shuffle law citedPermutation
    given items: List<Int> = [[4, 4, 4], [9, 9, 9, 9]]
    when List.len(items) >= 3
    using [shuffle.threeTurns]
    shuffle(shuffle(shuffle(items))) => List.take(items, 3)
"#
        ),
    )
    .unwrap();
    let Some(summary) = super::source_recursion::check(path.to_str().unwrap(), "dafny") else {
        return;
    };
    assert_eq!(summary["passed"], true, "{summary}");
}
