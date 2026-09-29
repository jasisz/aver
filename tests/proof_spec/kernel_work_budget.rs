//! A short sampled equation can hide unbounded kernel work in a dependency.
//! Check routing before building, so a regression fails without running the
//! memory-hungry kernel reduction that originally erased whole-program credit.

use super::*;

const HEAVY: &str = r#"module Heavy
    intent = "A compact call whose intermediate list is much larger than its result."
    exposes [longText, pairs]

fn maxItems() -> Int
    10000

fn pairs(n: Int, acc: List<String>) -> List<String>
    match n <= 0
        true -> acc
        false -> pairs(n - 1, List.prepend("00", acc))

fn longText() -> String
    String.join(pairs(maxItems() + 1, []), "")

verify longText
    String.len(longText()) => 20002

verify pairs
    pairs(2, []) => ["00", "00"]
"#;

const ENTRY: &str = r#"module Budget
    intent = "An imported recursive sample cannot erase an independent universal law."
    depends [Heavy]
    exposes [increment]

fn increment(n: Int) -> Int
    n + 1

verify increment
    increment(1) => 2

verify increment law successor
    given n: Int = [0, 1, 2]
    increment(n) => n + 1
"#;

#[test]
fn split_samples_use_kernel_only_for_literal_empty_or_single_char_delimiters() {
    let source = temp_output_dir("aver-split-kernel-source");
    std::fs::create_dir_all(&source).unwrap();
    let file = source.join("splits.av");
    std::fs::write(
        &file,
        r#"module Splits
    intent = "Only the empty and single-character split branches are kernel-reducible."
    exposes [multi, ascii, unicode, empty, dynamic]

fn multi(s: String) -> List<String>
    String.split(s, "::")
fn ascii(s: String) -> List<String>
    String.split(s, "|")
fn unicode(s: String) -> List<String>
    String.split(s, "🦀")
fn empty(s: String) -> List<String>
    String.split(s, "")
fn dynamic(s: String, separator: String) -> List<String>
    String.split(s, separator)

verify multi
    multi("a::b") => ["a", "b"]
verify ascii
    ascii("a|b") => ["a", "b"]
verify unicode
    unicode("a🦀b") => ["a", "b"]
verify empty
    empty("ab") => ["", "a", "b", ""]
verify dynamic
    dynamic("a|b", "|") => ["a", "b"]
"#,
    )
    .unwrap();
    let out = temp_output_dir("aver-split-kernel-output");
    let export = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("proof")
        .arg(&file)
        .arg("--module-root")
        .arg(&source)
        .arg("-o")
        .arg(&out)
        .output()
        .unwrap();
    assert!(export.status.success(), "{}", format_output(&export));
    let lean = std::fs::read_to_string(out.join("Splits.lean")).unwrap();
    for (name, tactic) in [
        ("multi", "native_decide"),
        ("dynamic", "native_decide"),
        ("ascii", "decide +kernel"),
        ("unicode", "decide +kernel"),
        ("empty", "decide +kernel"),
    ] {
        let case = lean
            .lines()
            .find(|line| line.starts_with(&format!("example : {name} ")))
            .unwrap_or_else(|| panic!("missing {name}:\n{lean}"));
        assert!(case.ends_with(&format!(":= by {tactic}")), "{case}");
    }
    if Command::new("lake").arg("--version").output().is_ok() {
        let build = Command::new("lake")
            .arg("build")
            .current_dir(&out)
            .output()
            .unwrap();
        assert!(build.status.success(), "{}", format_output(&build));
    }
    let _ = std::fs::remove_dir_all(&source);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn recursive_dependency_samples_stay_native_and_keep_sibling_law_credit() {
    let source = temp_output_dir("aver-kernel-work-source");
    std::fs::create_dir_all(&source).unwrap();
    std::fs::write(source.join("heavy.av"), HEAVY).unwrap();
    std::fs::write(source.join("budget.av"), ENTRY).unwrap();
    let out = temp_output_dir("aver-kernel-work-output");
    let run = |check: bool| {
        let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
        command
            .arg("proof")
            .arg(source.join("budget.av"))
            .arg("--module-root")
            .arg(&source)
            .arg("-o")
            .arg(&out);
        if check {
            command.arg("--check-json");
        }
        command.output().expect("run proof")
    };
    let export = run(false);
    assert!(export.status.success(), "{}", format_output(&export));
    let heavy = std::fs::read_to_string(out.join("Heavy.lean")).unwrap();
    for needle in ["longText.length", "pairs 2"] {
        let case = heavy
            .lines()
            .find(|line| line.starts_with("example : ") && line.contains(needle))
            .unwrap_or_else(|| panic!("missing {needle} sample:\n{heavy}"));
        assert!(case.len() < 256, "the equation must be small: {case}");
        assert!(
            case.ends_with(":= by native_decide"),
            "the term-size budget does not bound recursive work: {case}"
        );
    }
    let entry = std::fs::read_to_string(out.join("Budget.lean")).unwrap();
    assert!(
        entry.lines().any(|line| line.starts_with("example : ")
            && line.contains("increment 1")
            && line.ends_with(":= by decide +kernel")),
        "an unrelated nonrecursive function stays kernel-decided:\n{entry}"
    );
    if Command::new("lake").arg("--version").output().is_ok() {
        let checked = run(true);
        let json = String::from_utf8_lossy(&checked.stdout);
        let summary: serde_json::Value = serde_json::from_str(
            json.lines()
                .rev()
                .find(|line| line.starts_with('{'))
                .unwrap_or_else(|| panic!("no summary:\n{}", format_output(&checked))),
        )
        .unwrap();
        assert!(checked.status.success(), "{}", format_output(&checked));
        assert_eq!(summary["universal_laws"], 1, "{summary}");
        assert_eq!(summary["sorries"], 0, "{summary}");
        assert_eq!(summary["build_errors"], 0, "{summary}");
        assert_eq!(summary["build_succeeded"], true, "{summary}");
        assert_eq!(summary["build_exit_code"], 0, "{summary}");
    }
    let _ = std::fs::remove_dir_all(&source);
    let _ = std::fs::remove_dir_all(&out);
}

/// A failed process need not emit a source-located Lean diagnostic. Keep that
/// failure visible even when the old `build_errors` counter is zero.
#[cfg(unix)]
#[test]
fn proof_reports_a_failed_build_without_source_diagnostics() {
    use std::os::unix::fs::PermissionsExt;

    let source = temp_output_dir("aver-proof-build-status");
    std::fs::create_dir_all(&source).unwrap();
    std::fs::write(
        source.join("budget.av"),
        ENTRY.replace("    depends [Heavy]\n", ""),
    )
    .unwrap();
    let shim = source.join("bin");
    std::fs::create_dir_all(&shim).unwrap();
    let lake = shim.join("lake");
    std::fs::write(
        &lake,
        "#!/bin/sh\nprintf '%s\\n' 'error: Lean exited with code 137' >&2\nexit 1\n",
    )
    .unwrap();
    std::fs::set_permissions(&lake, std::fs::Permissions::from_mode(0o755)).unwrap();
    let mut paths = vec![shim];
    paths.extend(std::env::split_paths(
        &std::env::var_os("PATH").unwrap_or_default(),
    ));
    let out = source.join("out");
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .env("PATH", std::env::join_paths(paths).unwrap())
        .args(["proof", "--check-json", "--module-root"])
        .arg(&source)
        .arg(source.join("budget.av"))
        .arg("-o")
        .arg(&out)
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&run.stdout);
    let summary: serde_json::Value = serde_json::from_str(
        stdout
            .lines()
            .rev()
            .find(|line| line.starts_with('{'))
            .unwrap(),
    )
    .unwrap();
    assert!(!run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["passed"], false, "{summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["build_succeeded"], false, "{summary}");
    assert_eq!(summary["build_exit_code"], 1, "{summary}");
    assert_eq!(summary["universal_laws"], 0, "{summary}");
    let log = std::fs::read_to_string(out.join("proof_backend.log")).unwrap();
    assert!(log.contains("Lean exited with code 137"), "{log}");
    let _ = std::fs::remove_dir_all(&source);
}
