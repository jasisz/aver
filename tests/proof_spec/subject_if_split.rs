use super::*;

/// `count(y, insert(x, xs)) = count(y, x :: xs)`
/// (`tests/fixtures/subject_if_split_ih.av`). `count` branches on `y == h`,
/// and every older cons-arm rung ran `simp_all` over the whole goal first:
/// `count` stayed folded around `insert`'s `if`, and in the `y == h` branch
/// the substitution into the hypotheses broke the induction hypothesis. The
/// new rung unfolds the subject once, splits its own `if`, rewrites each
/// branch with `simp only [cone, ih]`, splits what is left and closes with
/// `omega`.
#[test]
fn proof_subject_if_split_keeps_the_induction_hypothesis() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping subject-split proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-subject-if-split");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/subject_if_split_ih.av", &output_dir, 0, &[]);
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
            summary["universal_laws"].as_u64(),
        ),
        (Some(0), Some(0), Some(1)),
        "the count-over-insert law must close universally.\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(output_dir.join("EqSplit.lean"))
        .expect("EqSplit.lean must be emitted");
    assert!(
        lean.contains("(rw [EqSplit.insert']; split <;> simp only ["),
        "the cons arm must split the subject first:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}
