use super::*;

/// Run `aver proof --check-json` on `fixture` and require every law to close
/// kernel-genuine, with no sorry and no hard error. Returns the Lean source of
/// the fixture's module for structural checks.
fn assert_all_universal(fixture: &str, prefix: &str, module: &str, laws: u64) -> Option<String> {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping {fixture}: `lake` not available");
        return None;
    }
    let output_dir = temp_output_dir(prefix);
    let (summary, run) = run_lean_check_json(fixture, &output_dir, 0, &[]);
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
            summary["sorries"].as_u64(),
            summary["build_errors"].as_u64(),
        ),
        (Some(laws), Some(0), Some(0), Some(0)),
        "every law of {fixture} must close universally:\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(output_dir.join(format!("{module}.lean")))
        .unwrap_or_else(|e| panic!("{module}.lean must be emitted: {e}"));
    let _ = std::fs::remove_dir_all(&output_dir);
    Some(lean)
}

/// `Bits.and(x, L)` with a literal mask: a low run, single bits, a mask of two
/// runs, and a mask read through another function. Each mask gets its closed
/// form from the `AverBits` bit lemmas; before, every one of these was a sorry.
#[test]
fn proof_literal_bit_masks_close_in_core() {
    let Some(lean) = assert_all_universal(
        "tests/fixtures/law_bit_masks.av",
        "aver-proof-bit-masks",
        "LawBitMasks",
        5,
    ) else {
        return;
    };
    assert!(
        lean.contains(
            "have _aver_mask_0 : ∀ y : Int, AverBits.and y 4259839 = y % 65536 + 4194304 * (y / 4194304 % 2) := by"
        ),
        "the two-run mask must get its closed form:\n{lean}"
    );
    assert!(
        lean.contains("AverBits.nat_land_split _ 4259839 65536 16 64 65535"),
        "the two-run mask must split at the end of its low run:\n{lean}"
    );
}

/// A function that matches an `Int` on literals, stated against the same set
/// as two ranges. The sign-split arm used to commit on it without closing it
/// (`unsolved goals`, which failed the build); the literal split closes it.
#[test]
fn proof_int_literal_match_against_ranges_closes() {
    let Some(lean) = assert_all_universal(
        "tests/fixtures/law_int_literal_match.av",
        "aver-proof-int-literal-match",
        "LawIntLiteralMatch",
        2,
    ) else {
        return;
    };
    assert!(
        lean.contains("(split <;> simp_all <;> omega)"),
        "the literal match must be split before the sign split:\n{lean}"
    );
}

/// Guarded claims whose truth rests on the sign of a product of two
/// non-constant terms. The product's sign facts from the core `Int.mul_*`
/// lemmas are stated as hypotheses; before, all three stayed on their
/// sampled domain.
#[test]
fn proof_product_sign_facts_close_guarded_claims() {
    let Some(lean) = assert_all_universal(
        "tests/fixtures/law_sign_facts.av",
        "aver-proof-sign-facts",
        "LawSignFacts",
        3,
    ) else {
        return;
    };
    assert!(
        lean.contains(
            "have _aver_sgn_0_5 : alpha < 0 → beta < 0 → 0 < alpha * beta := Int.mul_pos_of_neg_of_neg"
        ),
        "the product in the premise must get its sign facts:\n{lean}"
    );
    assert!(
        lean.contains("have _aver_sq_1 : 0 ≤ b * b :="),
        "the square in the cone must get its nonnegativity:\n{lean}"
    );
}

/// Guarded claims over a record and helpers that branch, one of them an
/// equation between `Option`s under a comparison. They go through the subject
/// function below a comparison, which the composition arm used to decline, so
/// both stayed on their sampled domain.
#[test]
fn proof_guarded_claims_over_branching_cone_close() {
    let Some(lean) = assert_all_universal(
        "tests/fixtures/law_guarded_cone.av",
        "aver-proof-guarded-cone",
        "LawGuardedCone",
        2,
    ) else {
        return;
    };
    assert!(
        lean.contains("-- aver:law-class place_law_staysUnderCap universal"),
        "the record claim must be stated universally:\n{lean}"
    );
    assert!(
        lean.contains("-- aver:law-class unheard_law_aPoolWithSomebodyKeepsTheRule universal"),
        "the Option claim must be stated universally:\n{lean}"
    );
}
