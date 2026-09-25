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
/// `≤`-chain witness this step does not synthesize) keeps its sound bounded
/// sampled fallback — bounded, not a sorry. axioms stay within {propext,
/// Classical.choice, Quot.sound}.
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
        "the nonlinear-nonneg/order wall must close on ZERO sorries — the \
         `aver_int_order` primitive closes sqNonneg/mulNonneg/tripleNonneg/\
         sqMono/nrContraction and the grind rung closes nrNewErrNum≍nrOldErrSq \
         (a residual sorry, a build error, or mulLeTrans regressing off its \
         bounded fallback is a failure).\n{}",
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
