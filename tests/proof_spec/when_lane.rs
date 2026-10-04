use super::*;

/// Nonlinear-arithmetic wall (`tests/fixtures/nr_wall.av`):
/// laws whose unfolded cone multiplies two VARIABLES (`x * x`,
/// `(s*s - d*x)^2`). The export must BUILD green — a failing tactic
/// (`omega` on a var×var goal, `by_cases` over a hypothesis name,
/// Nat-truncated sample guards) is a build error, not an honest sorry.
///
/// Two generic engine steps now close the nonneg/order sub-family
/// kernel-genuine, so the wall lands on ZERO sorries:
///   - the `NonlinearNonneg` strategy + its shipped prelude primitive
///     `aver_int_order` (the `omega`-analog for the products-and-squares
///     fragment — recurse with `Int.mul_nonneg` on a nonneg goal or
///     `Int.mul_le_mul` on a `prod ≤ prod` goal, sign-split squares) closes
///     `sqNonneg` (`x·x ≥ 0`), `mulNonneg`, `tripleNonneg`, the squaring
///     monotonicity `sqMono` (`e·e ≤ b·b`), and the contraction bound
///     `nrContraction` (`((d·x-s)²)² ≤ s²`) — the premised ones as TRUE
///     universals `∀ …, <guard> = true -> claim` (the `when`-guard threaded
///     in as a hypothesis, not a finite sample);
///   - the shape-gated `grind` rung closes the unconditional nonlinear
///     polynomial ring identity `nrNewErrNum ≍ nrOldErrSq`
///     (`s⁴ - d·(x·(2s² - dx)) = (s² - dx)²`).
///
/// Only `mulLeTrans` (`a·c ≤ m`, a `prod ≤ var` transitivity needing a
/// `≤`-chain witness this step does not synthesize) is declined — not a
/// sorry. axioms stay within {propext, Classical.choice, Quot.sound}.
#[test]
fn proof_nonlinear_nonneg_laws_close_via_generic_primitive() {
    if !lean_required::lake_available() {
        eprintln!("skipping nonlinear-wall proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-nr-wall");
    let (summary, run) = run_lean_check_json_with_args(
        "tests/fixtures/nr_wall.av",
        &output_dir,
        0,
        &[],
        &["--declined-budget", "1"],
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the nonlinear-nonneg/order wall must close on ZERO sorries — the \
         `aver_int_order` primitive closes sqNonneg/mulNonneg/tripleNonneg/\
         sqMono/nrContraction and the grind rung closes nrNewErrNum≍nrOldErrSq \
         (a residual sorry or a build error is a failure).\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["declined_claims"][0]["claim"].as_str(),
        Some("mulLeTrans.guarded"),
        "{}",
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
