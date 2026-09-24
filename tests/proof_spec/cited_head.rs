use super::*;

/// A cited conditional law whose conclusion is headed by a non-recursive
/// wrapper (`tests/fixtures/cited_lemma_folded_head.av`): `buildOk` needs
/// `pushOk` (`ok xs -> ok (push x xs)`) at the induction hypothesis. Every
/// older rung unfolded `ok` into its `match` before trying the law, so the
/// law never matched and was reported unused. The cons arm now also unfolds
/// the subject alone and lets `simp_all` apply the cited law with `ok` still
/// folded. Both laws are universal.
#[test]
fn proof_cited_law_applies_before_its_head_unfolds() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cited-head proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-cited-head");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/cited_lemma_folded_head.av",
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
        (Some(0), Some(0), Some(2)),
        "the citing law must close universally.\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(output_dir.join("CitedHead.lean"))
        .expect("CitedHead.lean must be emitted");
    assert!(
        lean.contains("(rw [CitedHead.build]; simp_all [push_law_pushOk]; done)"),
        "the cons arm must apply the cited law with its head folded:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}
