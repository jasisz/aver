//! Recursive effectful functions reach Lean through their oracle lift. They
//! have no recursion contract of their own, so the export measures them from
//! the lifted calls with the same call edge analysis the pure groups use; a
//! function that analysis finds no measure for stays `partial`.

use super::*;

fn export(fixture: &str, module_root: Option<&str>, prefix: &str) -> PathBuf {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir(prefix);
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_aver"));
    cmd.current_dir(&repo_root)
        .arg("proof")
        .arg("--examples")
        .arg(fixture);
    if let Some(root) = module_root {
        cmd.arg("--module-root").arg(root);
    }
    let run = cmd
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    output_dir
}

/// The declaration of `name` in `lean`, from its `def` line up to the next
/// blank line.
fn declaration<'a>(lean: &'a str, name: &str) -> &'a str {
    let start = lean
        .find(&format!("def {name} "))
        .unwrap_or_else(|| panic!("no declaration of {name}:\n{lean}"));
    let line_start = lean[..start].rfind('\n').map_or(0, |i| i + 1);
    let rest = &lean[line_start..];
    &rest[..rest.find("\n\n").unwrap_or(rest.len())]
}

/// A list walk, a three-member cycle around a guarded countdown, and a walk
/// over stored data. Before, every one of them was `partial def`: the lifted
/// path tried only the two-member counter walk and never consulted the
/// measure the pure groups get.
#[test]
fn recursive_effectful_functions_get_the_pure_measure() {
    let output_dir = export(
        "tests/fixtures/lean_lifted_recursion.av",
        None,
        "aver-proof-lifted-recursion-export",
    );
    let lean = std::fs::read_to_string(output_dir.join("LiftedRecursion.lean")).unwrap();
    let rows = declaration(&lean, "rows");
    assert!(!rows.contains("partial def"), "{rows}");
    assert!(rows.contains("termination_by sizeOf names"), "{rows}");
    for member in ["spend", "fetch", "keep"] {
        let decl = declaration(&lean, member);
        assert!(!decl.contains("partial def"), "{decl}");
        assert!(decl.contains("termination_by (Int.toNat left, "), "{decl}");
    }
    // Only the stored chain decides when `follow` stops: no measure, so the
    // fallback is unchanged.
    assert!(lean.contains("partial def follow "), "{lean}");
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// The lifted definitions build, and the cases with a free oracle reduce
/// through them: none is declined or left as a `sorry`, except the case of
/// the walk that stays `partial`, which is declined as before.
#[test]
fn verify_cases_close_through_measured_effectful_recursion() {
    if !lean_required::lake_available() {
        eprintln!("skipping lifted recursion: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-lifted-recursion-lean");
    let (summary, run) = run_lean_check_json_with_args(
        "tests/fixtures/lean_lifted_recursion.av",
        &output_dir,
        0,
        &[],
        &["--declined-budget", "1"],
    );
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
            summary["declined"].as_u64(),
            summary["model_panicked"].as_bool(),
        ),
        (Some(true), Some(0), Some(0), Some(1), Some(false)),
        "{summary}\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(output_dir.join("LiftedRecursion.lean")).unwrap();
    for case in ["verify rows case", "verify spend case", "verify fetch case"] {
        assert!(!lean.contains(case), "a case was declined:\n{lean}");
    }
    assert!(lean.contains("verify follow case 1"), "{lean}");
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Whether a lifted function recurses is read off the lifted call graph, not
/// the program-wide set of recursive pure function names: a dependency's
/// recursive `absorbed` used to make the entry's non-recursive effectful
/// `absorbed` a `partial def`, and its cases undecidable.
#[test]
fn a_recursive_pure_namesake_does_not_make_an_effectful_function_partial() {
    let output_dir = export(
        "tests/fixtures/lifted_name_collision/main.av",
        Some("tests/fixtures/lifted_name_collision"),
        "aver-proof-lifted-name-collision",
    );
    let entry = std::fs::read_to_string(output_dir.join("Main.lean")).unwrap();
    let absorbed = declaration(&entry, "absorbed");
    assert!(!absorbed.contains("partial def"), "{absorbed}");
    assert!(!entry.contains("verify absorbed case"), "{entry}");
    let _ = std::fs::remove_dir_all(&output_dir);
}
