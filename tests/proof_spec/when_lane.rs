use super::*;

/// Nonlinear-arithmetic wall (`tests/fixtures/nr_wall.av`):
/// laws whose unfolded cone multiplies two VARIABLES (`x * x`,
/// `(s*s - d*x)^2`). The export must BUILD green — a failing tactic
/// (`omega` on a var×var goal, `by_cases` over a hypothesis name,
/// Nat-truncated sample guards) is a build error, not an honest sorry.
///
/// Two generic engine steps now close the nonnegativity sub-family
/// kernel-genuine, so the wall lands on ZERO sorries:
///   - the `NonlinearNonneg` strategy + its shipped prelude primitive
///     `aver_int_nonneg` (the `omega`-analog for the products-and-squares
///     fragment — decompose with `Int.mul_nonneg`, sign-split squares)
///     closes `sqNonneg` (`x·x ≥ 0`), `mulNonneg`, and `tripleNonneg` —
///     the last two as TRUE universals `∀ …, <guard> = true -> claim`
///     (the `when`-guard threaded in as a hypothesis, not a finite sample);
///   - the shape-gated `grind` rung closes the unconditional nonlinear
///     polynomial ring identity `nrNewErrNum ≍ nrOldErrSq`
///     (`s⁴ - d·(x·(2s² - dx)) = (s² - dx)²`).
/// The order sub-family (`sqMono`, `mulLeTrans`) and the contraction
/// bound (`nrContraction`) are `<= `-claims this nonneg step does not
/// reach, so they keep their sound bounded sampled fallback — bounded,
/// not sorries. axioms stay within {propext, Classical.choice, Quot.sound}.
#[test]
fn proof_nonlinear_nonneg_laws_close_via_generic_primitive() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping nonlinear-wall proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-nr-wall");
    let (summary, run) = run_lean_check_json("tests/fixtures/nr_wall.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the nonlinear-nonneg wall must close on ZERO sorries — the \
         `aver_int_nonneg` primitive closes sqNonneg/mulNonneg/tripleNonneg \
         and the grind rung closes nrNewErrNum≍nrOldErrSq (a residual sorry, \
         a build error, or the bounded order laws regressing is a failure).\n{}",
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
