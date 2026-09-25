use super::*;

/// Division by a variable (`tests/fixtures/int_divmod_variable.av`): the
/// quotient-remainder identity and the remainder bounds under `n >= 1`.
/// `omega` only knows `/` and `%` by a literal, so both laws used to stay
/// bounded. Aver's `Int.div` / `Int.mod` are Euclidean like Lean's `/` and
/// `%` on `Int`, and the conditional portfolio now cites the core facts
/// (`Int.mul_ediv_add_emod`, `Int.emod_nonneg`, `Int.emod_lt_of_pos`) when
/// the cone divides. Both laws become universal.
#[test]
fn proof_quotient_remainder_laws_close_universally() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping div/mod proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-int-divmod");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/int_divmod_variable.av", &output_dir, 0, &[]);
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(0), Some(0), Some(2), Some(0)),
        "both division laws must close universally.\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(output_dir.join("DivMod.lean"))
        .expect("DivMod.lean must be emitted");
    assert!(
        lean.contains(
            "theorem quot_law_quotRem : ∀ (a : Int) (n : Int), (n >= 1) = true -> ((n * quot a n) + rem a n) = a := by"
        ),
        "the identity must be stated without its sampled domain:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}
