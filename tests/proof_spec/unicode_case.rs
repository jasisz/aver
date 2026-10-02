//! The ASCII identity fast path must preserve the VM's complete Unicode case
//! semantics, including expansions and sigma's surrounding case context.

use super::*;
use std::fmt::Write;

const FUNCTIONS: &str = r#"module UnicodeCase
    intent = "ASCII shortcuts preserve full Unicode case conversion."
    exposes [lower, upper, lowerPoints, upperPoints]

fn lower(s: String) -> String
    String.toLower(s)

fn upper(s: String) -> String
    String.toUpper(s)

fn lowerPoint(cp: Int) -> Int
    text = Option.withDefault(String.fromCodePoint(cp), "")
    Option.withDefault(String.firstCodePoint(lower(text)), -1)

fn upperPoint(cp: Int) -> Int
    text = Option.withDefault(String.fromCodePoint(cp), "")
    Option.withDefault(String.firstCodePoint(upper(text)), -1)

fn lowerPoints(points: List<Int>) -> List<Int>
    match points
        [] -> []
        [point, ..rest] -> List.prepend(lowerPoint(point), lowerPoints(rest))

fn upperPoints(points: List<Int>) -> List<Int>
    match points
        [] -> []
        [point, ..rest] -> List.prepend(upperPoint(point), upperPoints(rest))
"#;

#[test]
fn ascii_identity_paths_match_vm_without_losing_unicode_context_or_expansions() {
    let inputs = [
        "",
        "abcdef0123456789",
        "ABCDEF0123456789",
        "aBcDeF0123456789",
        " !@#$%^&*()",
        "İ",
        "Straße",
        "ﬃ",
        "K",
        "ΟΣ",
        "ΟΣΑ",
        "AΣ",
        "AΣA",
        "A'Σ",
        "A\u{301}Σ",
        "ĄĆĘ ŁÓŚ ΩΔ",
    ];
    let mut source = FUNCTIONS.to_owned();
    for (name, convert) in [
        ("lower", str::to_lowercase as fn(&str) -> String),
        ("upper", str::to_uppercase as fn(&str) -> String),
    ] {
        writeln!(source, "\nverify {name}").unwrap();
        for input in inputs {
            let input_literal = serde_json::to_string(input).unwrap();
            let expected = serde_json::to_string(&convert(input)).unwrap();
            writeln!(source, "    {name}({input_literal}) => {expected}").unwrap();
        }
    }
    // Include every control character, not just printable hex digits. Scalar
    // lists avoid depending on either language's string-literal escape syntax.
    let ascii: Vec<u32> = (0..128).collect();
    let lower: Vec<u32> = ascii
        .iter()
        .map(|&cp| char::from_u32(cp).unwrap().to_ascii_lowercase() as u32)
        .collect();
    let upper: Vec<u32> = ascii
        .iter()
        .map(|&cp| char::from_u32(cp).unwrap().to_ascii_uppercase() as u32)
        .collect();
    writeln!(
        source,
        "\nverify lowerPoints\n    lowerPoints({ascii:?}) => {lower:?}\n\
         \nverify upperPoints\n    upperPoints({ascii:?}) => {upper:?}"
    )
    .unwrap();
    let source_dir = tempfile::tempdir().unwrap();
    let file = source_dir.path().join("unicode_case.av");
    std::fs::write(&file, source).unwrap();
    let verify = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("verify")
        .arg(&file)
        .output()
        .unwrap();
    assert!(verify.status.success(), "{}", format_output(&verify));
    let total = inputs.len() * 2 + 2;
    assert!(
        String::from_utf8_lossy(&verify.stdout).contains(&format!("{total}/{total} cases passed")),
        "{}",
        format_output(&verify)
    );
    if !lean_required::lake_available() {
        eprintln!("skipping Unicode proof comparison: `lake` not available");
        return;
    }
    let output = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json(file.to_str().unwrap(), output.path(), 0, &[]);
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["passed"], true, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["model_panicked"], false, "{summary}");
}
