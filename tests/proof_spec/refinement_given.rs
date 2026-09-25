use super::*;

/// A `given` over a refinement record (`tests/fixtures/refinement_given_domain.av`).
/// `Natural` lifts to a Lean `Subtype`, so each sample is `⟨0, proof⟩`. Three
/// things used to break the build for every law in the file: the lakefile
/// declared the library under the module's name, and `Min` is a core Lean
/// name; a sample substituted into the `when` had no expected type
/// (`(⟨0, …⟩).val`); and the sampled-domain proof ran `cases` on `d = ⟨0, …⟩`,
/// a dependent elimination failure outside any `sorry` floor. Now the two
/// provable laws are universal and the nonlinear one keeps its sampled proof.
#[test]
fn proof_given_over_refinement_record_builds() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping refinement given proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-refinement-given");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/refinement_given_domain.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(0), Some(0), Some(2), Some(1)),
        "every law over the refinement record must build and close.\n{}",
        format_output(&run)
    );
    let lean =
        std::fs::read_to_string(output_dir.join("Min.lean")).expect("Min.lean must be emitted");
    assert!(
        lean.contains("-- aver:law-class keep_law_squareBelow bounded-domain"),
        "the nonlinear law must keep its sampled-domain proof:\n{lean}"
    );
    assert!(
        lean.contains("first | subst h_d_case | cases h_d_case"),
        "the sampled proof must substitute the refinement sample:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}
