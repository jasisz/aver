use super::*;

/// Nonlinear-arithmetic wall (`tests/fixtures/nr_wall.av`):
/// laws whose unfolded cone multiplies two VARIABLES (`x * x`,
/// `(s*s - d*x)^2`) must DEGRADE to honest caught sorries, never to a
/// failing tactic. Pre-fix this file produced three distinct build
/// errors: `by_cases h_h_a : h_a ≥ 0` (case over a premise-HYPOTHESIS
/// name — application type mismatch), `omega` failing on a nonlinear
/// goal, and `_sample_N` theorems FALSE AS STATED (when-guard numerals
/// elaborated as Nat, truncating subtraction). Post-fix the export
/// builds GREEN: every `when`-guarded law closed bounded over its
/// declared domain and every emitted sample theorem true (Int-ascribed
/// guards).
///
/// The SHAPE-GATED `grind` rung (admitted on flat algebraic/ring goals,
/// skipped on inductive ones) now closes the unconditional nonlinear
/// polynomial ring identity `nrNewErrNum ≍ nrOldErrSq`
/// (`s⁴ - d·(x·(2s² - dx)) = (s² - dx)²`) — grind's `+ring` subsolver
/// carries the multi-variable nonlinear identity the hand-rolled AC-ring
/// simp package stopped normalizing at Lean 4.31. So the wall now lands
/// on exactly 1 honest caught sorry — the genuinely undecidable-by-grind
/// `sqNonneg` (`x·x ≥ 0`, needs `mul_self_nonneg`, off the whitelist
/// here) — down from 2. grind pulls `Classical.choice` on the closure
/// (whitelisted: ⊆ {propext, Classical.choice, Quot.sound}).
#[test]
fn proof_nonlinear_laws_degrade_to_honest_sorries() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping nonlinear-wall proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-nr-wall");
    let (summary, run) = run_lean_check_json("tests/fixtures/nr_wall.av", &output_dir, 1, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(1),
        "the nonlinear wall must land on exactly 1 honest caught sorry — \
         the shape-gated grind rung closes the nrNewErrNum≍nrOldErrSq ring \
         identity, leaving only sqNonneg (a build error OR a different count \
         is a regression).\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the nonlinear-wall export must BUILD green — failing tactics \
         (omega on var*var goals, by_cases over hypothesis names, \
         Nat-truncated sample guards) are build errors, not sorries.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Wide single-given domain (`tests/fixtures/wide_domain_law.av`):
/// a conditional law whose one given spans `0..299` makes
/// `law_theorem_prop` prepend a 300-way `a = v0 ∨ … ∨ a = v299`
/// disjunction. Unpartitioned, that statement blows Lean's default
/// `maxRecDepth` during elaboration (the scout bisected the wall at 252
/// values) and the WHOLE file fails to build — every law in it loses its
/// caught-sorry floor. Partitioning the domain into `_partN` theorems
/// keeps each part's disjunction below the wall, so the file builds green
/// and the check passes. Live lake.
#[test]
fn proof_wide_domain_law_partitions_and_builds_green() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping wide-domain proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-wide-domain");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/wide_domain_law.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the wide-domain export must BUILD green — without partitioning the \
         300-way disjunction exceeds maxRecDepth and the whole file fails.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the partitioned bounded law closes its sample/checked-domain checks.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["bounded_laws"].as_u64(),
        Some(1),
        "the partitioned `_partN` theorems fold to ONE bounded law in the audit.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Dafny side of the nonlinear-wall fixture: `when`-law samples come
/// from the UNFILTERED given cartesian product, so premise-violating
/// combinations (square-monotonicity at e=1, b=0) were asserted
/// unguarded and failed verification on a file whose universal lemmas
/// Z3 fully proves. Post-fix the samples are checked under
/// `if <instantiated premise> { … }` (mirroring Lean's `_sample_N`
/// premise-as-hypothesis form) and the whole file verifies: 0 errors,
/// 0 axioms.
///
/// Deliberately budget-only (no `passed` assert): this fixture's
/// genuinely nonlinear universal lemmas have platform-sensitive
/// verification wall-clock — a slower Z3 build can time an obligation
/// out (exit 4) without erroring, which is jitter, not the regression
/// under test. The when-filter regression itself surfaces as ERRORS
/// (reverting the guard yields 2 "assertion might not hold"), so the
/// error budget catches it on every platform.
#[test]
fn proof_dafny_when_filtered_samples() {
    assert_dafny_verifies("tests/fixtures/nr_wall.av", "aver-dafny-nr-wall");
}

/// Rationals fixture (`tests/fixtures/rational_probe.av`):
/// concrete-literal sample asserts over record arguments pushed Z3
/// into symbolic fuel unfolding — 150 s+ timeouts (`dafny verify`
/// exit 4) on a file whose universal lemmas verify in ~1 s, so the
/// exit-status gate failed an otherwise-proven file. Post-fix each
/// sample assert is seeded with the universal lemma instantiated at
/// the sample values and the file verifies end-to-end in seconds.
/// `passed` is asserted explicitly: a timeout leaves the parsed error
/// count at 0 and surfaces ONLY in the exit status, so an errors-only
/// assert cannot catch this regression.
#[test]
fn proof_dafny_rational_samples_no_timeout() {
    assert_dafny_verifies_and_passes("tests/fixtures/rational_probe.av", "aver-dafny-rational");
}
