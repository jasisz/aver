use super::*;

/// Length-guarded prefix arm of the conditional-inductive driver
/// (`tests/fixtures/list_prefix_guard.av`): three stack-shuffle laws whose
/// `when` guarantees the list is at least `k` items long (`List.len(items)
/// >= 2`, `> 2`, `4 <= List.len(items)`) and whose claim reads fixed
/// positions through `itemAt`. Plain list induction cannot close them (the
/// cons hypothesis says nothing about the tail's length), so they fell back
/// to their sampled domain. The cons arm now exposes the `k - 1` further
/// conses the premise guarantees by one `rcases` and lets `simp_all`
/// evaluate the fixed-position reads. Live Lean gate; the fourth law (a sum
/// over the reversed list under the same premise) needs induction proper
/// and must stay BOUNDED, never `sorry`.
///
/// The fixture also carries the fuel-induction trio (a base-256 countdown,
/// its accumulator law, a structural reader, and the `when m >= 0` round
/// trip). Under the speculative PROBE the keystone tries that round trip
/// first, and with every cone def abstract (all recursive) its simp list
/// came out EMPTY — `simp only [, Bool.and_eq_true, …]`, a syntax error
/// that failed the whole probe build and silently cost the three shuffle
/// laws their universal statements (the shape of the btc-listener stack
/// laws). The list is built as a list now; the probe must build.
#[test]
fn proof_list_prefix_guard_lean_closes_fixed_window_laws() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping list-prefix proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-list-prefix");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/list_prefix_guard.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the non-closing law must revert to bounded, never sorry.\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(6), Some(1)),
        "the three fixed-window laws and the countdown trio certify; the sum \
         law stays bounded.\n{}",
        format_output(&run)
    );
    assert!(
        !String::from_utf8_lossy(&run.stderr).contains("probe build failed"),
        "the speculative probe must build (no malformed arm):\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(output_dir.join("ListPrefixGuard.lean"))
        .expect("ListPrefixGuard.lean must be emitted");
    for (base, pattern) in [
        (
            "shuffled_law_swapTwiceIsTheTopPair",
            "rcases tl with _ | ⟨pfx1, pfxrest⟩ <;>",
        ),
        (
            "shuffled_law_rotThreeTimesIsTheTopThree",
            "rcases tl with _ | ⟨pfx1, _ | ⟨pfx2, pfxrest⟩⟩ <;>",
        ),
        (
            "shuffled_law_twoSwapTwiceIsTheTopFour",
            "rcases tl with _ | ⟨pfx1, _ | ⟨pfx2, _ | ⟨pfx3, pfxrest⟩⟩⟩ <;>",
        ),
    ] {
        let start = lean
            .find(&format!("theorem {base} :"))
            .unwrap_or_else(|| panic!("{base} theorem must be emitted:\n{lean}"));
        let body = &lean[start..];
        let end = body.find("_checked_domain").unwrap_or(body.len());
        let body = &body[..end];
        assert!(
            body.contains(pattern),
            "{base} must expose the premise's conses in its cons arm:\n{body}"
        );
        assert!(
            lean.contains(&format!("-- aver:law-class {base} universal")),
            "{base} must be classed universal:\n{lean}"
        );
    }
    assert!(
        !lean.contains("-- aver:law-class total_law_reverseKeepsTheSumOfLongLists universal"),
        "the sum law must not be credited universal:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}
