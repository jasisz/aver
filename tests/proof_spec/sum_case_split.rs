use super::*;

/// A law over a sum type whose variant carries a payload
/// (`tests/fixtures/sum_payload_case_split.av`): `step` matches on the event,
/// so neither a flat `simp` nor `grind` over the whole goal reaches the
/// arithmetic. The portfolio now splits the sum-typed given by constructor,
/// unfolds the cone of both sides in each case and closes with `omega`.
#[test]
fn proof_law_over_payload_sum_splits_by_constructor() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping sum case split proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-sum-case-split");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/sum_payload_case_split.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
            summary["universal_laws"].as_u64(),
        ),
        (Some(0), Some(0), Some(1)),
        "the law over the event sum must close universally.\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(output_dir.join("SumCases.lean"))
        .expect("SumCases.lean must be emitted");
    assert!(
        lean.contains("(cases e <;> simp ["),
        "the portfolio must split the sum-typed given:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}
