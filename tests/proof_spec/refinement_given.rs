use super::*;

/// A `given` over a refinement record (`tests/fixtures/refinement_given_domain.av`).
/// `Natural` lifts to a Lean `Subtype`, so each sample is `⟨0, proof⟩`. Two
/// things used to break the build for every law in the file: the lakefile
/// declared the library under the module's name, and `Min` is a core Lean
/// name; and a sample substituted into the `when` had no expected type
/// (`(⟨0, …⟩).val`). Now the two
/// provable laws are universal and the nonlinear one is declined.
#[test]
fn proof_given_over_refinement_record_builds() {
    if !lean_required::lake_available() {
        eprintln!("skipping refinement given proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-refinement-given");
    let (summary, run) = run_lean_check_json_with_args(
        "tests/fixtures/refinement_given_domain.av",
        &output_dir,
        0,
        &[],
        &["--declined-budget", "1"],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
            summary["universal_laws"].as_u64(),
            summary["declined_claims"][0]["claim"].as_str(),
        ),
        (Some(0), Some(0), Some(2), Some("keep.squareBelow")),
        "every law over the refinement record must build and close.\n{}",
        format_output(&run)
    );
    let lean =
        std::fs::read_to_string(output_dir.join("Min.lean")).expect("Min.lean must be emitted");
    assert!(
        lean.contains("-- aver:law-class keep_law_squareBelow attempt"),
        "the nonlinear law must be stated universally as an attempt:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}
