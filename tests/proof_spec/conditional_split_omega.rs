use super::*;

/// Non-inductive conditional closer for a `when`-law whose subject splits
/// on a guard (`tests/fixtures/conditional_split_omega.av`): `placed` puts a
/// sign byte in front of a byte list when the top byte is `>= 128` and
/// folds the sign into the top byte otherwise; the law says `readTop`
/// reads it back as the signed sum, for every `rest`. The list induction
/// probe used to fail here: its arms split the reader's `match` BEFORE the
/// subject's `if` was decided and lost the equation between the match
/// variable and the placed byte, so the law fell back to its sampled
/// domain. The new arm unfolds the subject alone, splits its guard, then
/// unfolds the rest, splits the residual `if`s, normalizes the arithmetic
/// and closes by `omega`. Live Lean gate: the law earns `universal` credit.
#[test]
fn proof_conditional_split_closer_lean_closes_universally() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping conditional-split proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-conditional-split");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/conditional_split_omega.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the split-guard law must close sorry-free.\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal"].as_bool(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(true), Some(1), Some(0)),
        "the when-law must be stated universally and certified, not degraded \
         to its sampled domain.\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(output_dir.join("ConditionalSplitOmega.lean"))
        .expect("ConditionalSplitOmega.lean must be emitted");
    assert!(
        lean.contains(
            "theorem placed_law_readsAsSigned : ∀ (top : Int) (rest : List Int) (negative : Bool), ((top >= 0) && (top <= 255)) = true -> readTop (placed top rest negative) = signedBy (sum (top :: rest) 0) negative := by"
        ),
        "the when-law must drop its sampled domain:\n{lean}"
    );
    assert!(
        lean.contains("-- aver:law-class placed_law_readsAsSigned universal"),
        "the law must be classed universal:\n{lean}"
    );
    assert!(
        lean.contains("(cases negative <;> simp only [placed, Bool.false_eq_true"),
        "both induction arms must carry the subject-first split closer:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Neither the marker's threshold nor the sign flag is fixed by its samples.
/// The old combined rewrite set looped on the reader's Boolean condition.
#[test]
fn proof_conditional_layout_with_arbitrary_threshold_closes() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let output_dir = temp_output_dir("aver-proof-conditional-layout");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/conditional_layout.av", &output_dir, 0, &[]);
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(0), Some(1), Some(0), Some(0)),
        "the layout law must prove beyond its sampled thresholds:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}
