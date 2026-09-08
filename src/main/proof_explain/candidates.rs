//! Optional helper-law suggestions follow source proof reports. Failed inverse
//! translations belong in the diagnostic log, not the proof author's console.
use super::super::{
    GoalDump, PROVENANCE_MARKER_PREFIX, SampleVerdict, build_candidate_law, sample_check_candidate,
};

/// `aver proof --explain` Aver-space renderer (Lean-only, console). For each
/// open law with a dumped residual goal: un-translate it back to Aver, and print
/// a candidate `law` skeleton gated through the VM sample-check (passed →
/// add-and-cite; failed → counterexample). Technical declines are logged.
/// The agent/user surface is AVER-ONLY here — the raw Lean residual stays in the
/// internal `open_goal` channel. Pure console output: never affects tiers /
/// credit / `passed` / the exit code.
pub(crate) fn render(
    open_laws: &[(String, String)],
    goal_json: &std::collections::BTreeMap<String, GoalDump>,
    items: &[aver::ast::TopLevel],
    file: &str,
    module_root: &str,
    output_dir: &str,
) -> String {
    use aver::codegen::lean::lemma_calc::{self, CalcVerdict};
    use aver::codegen::lean::untranslate::{peano_ctx_for_law, untranslate_goal_ctx};
    use colored::Colorize;
    use std::fmt::Write;
    if open_laws.is_empty() {
        return String::new();
    }
    // The lemma calculator reads program facts (constructor names, fn return
    // types) as data; build it once for the whole render.
    let calc_env = lemma_calc::CalcEnv::from_items(items);
    let mut rendered = String::new();
    let mut technical = String::new();
    let mut showed_header = false;
    // Every open law gets a suggestion or a diagnostic log entry. A law's split
    // residual yields several branch goals: each
    // becomes a candidate, deduped by source; a branch outside the grammar records
    // its decline reason so a law with no in-grammar branch still gets one honest
    // gap verdict.
    for (label, _thm) in open_laws {
        let Some(dump) = goal_json.get(label) else {
            let _ = writeln!(
                technical,
                "{label}: residual not extractable (engine-form gap)"
            );
            continue;
        };
        // `fn.law`: the law name is the final segment, the fn everything before.
        let (fn_name, law_name) = match label.rsplit_once('.') {
            Some((f, l)) => (f, l),
            None => ("", label.as_str()),
        };
        // Thread a Peano context so a law over a canonical-Peano ADT inverts the
        // transpiler's `Nat`-lift (`Succ x → x + 1`) back to the ADT's
        // constructors; a non-Peano law gets the default (pre-V2) behavior.
        let ctx = peano_ctx_for_law(items, fn_name, law_name);
        // A calculated lemma is named `_calc` and rendered "calculated law"; a raw
        // #630 residual fallback keeps `_residual` and "candidate law". The names
        // must agree with the block the sample-check keys its verdict on.
        let calc_law_name = format!("{law_name}_calc");
        let residual_law_name = format!("{law_name}_residual");
        // The parent law's given names: the fresh lifted variables must dedup
        // against them (the candidate builder resolves givens by name against the
        // parent, so a colliding lift clones the wrong domain — name capture).
        let mut parent_givens: std::collections::HashSet<String> = std::collections::HashSet::new();
        for it in items {
            if let aver::ast::TopLevel::Verify(vb) = it
                && vb.fn_name == fn_name
                && let aver::ast::VerifyKind::Law(l) = &vb.kind
                && l.name == law_name
            {
                for g in &l.givens {
                    parent_givens.insert(g.name.clone());
                }
            }
        }
        // A split residual yields several branch goals. Each becomes a candidate,
        // deduped by source; the sample-check partitions them. We surface every
        // branch that PASSES (a forced lemma — prop_73 legitimately yields one per
        // branch), and fall back to a single honest negative (counterexample, then
        // gap) only when no branch passes. A branch outside the grammar records its
        // decline reason so a law with no in-grammar branch still gets a verdict.
        let mut seen: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        let mut passed: Vec<(String, bool)> = Vec::new();
        let mut first_fail: Option<(String, String)> = None;
        let mut gaps = Vec::new();
        let mut decline: Option<String> = None;
        for json in &dump.jsons {
            let goal = match untranslate_goal_ctx(json, &ctx) {
                Ok(g) => g,
                Err(gap) => {
                    gaps.push(gap.reason);
                    continue;
                }
            };
            // Prefer the calculator's forced lemma when it sample-checks; else
            // fall back to the raw residual candidate. The calculator only ever
            // ADDS a stronger lemma — it never downgrades a raw candidate that
            // would have passed, so a Lemma that fails the VM defers to the raw.
            // A `Decline` is an honest verdict of its own: record its reason so it
            // is surfaced before the fallback, never silently dropped.
            let mut chosen: Option<(String, SampleVerdict, bool)> = None;
            match lemma_calc::calculate(&goal, &calc_env, &parent_givens) {
                CalcVerdict::Lemma(g) => {
                    if let Ok(src) = build_candidate_law(
                        fn_name,
                        law_name,
                        &calc_law_name,
                        *g,
                        items,
                        ctx.peano.as_ref(),
                    ) {
                        let verdict = sample_check_candidate(
                            &src,
                            fn_name,
                            &calc_law_name,
                            file,
                            module_root,
                        );
                        if matches!(verdict, SampleVerdict::Pass) {
                            chosen = Some((src, verdict, true));
                        }
                    }
                }
                CalcVerdict::Decline(reason) => {
                    decline.get_or_insert(reason);
                }
            }
            let (src, verdict, calculated) = match chosen {
                Some(c) => c,
                None => {
                    let src = match build_candidate_law(
                        fn_name,
                        law_name,
                        &residual_law_name,
                        goal,
                        items,
                        ctx.peano.as_ref(),
                    ) {
                        Ok(s) => s,
                        Err(reason) => {
                            gaps.push(reason);
                            continue;
                        }
                    };
                    let verdict = sample_check_candidate(
                        &src,
                        fn_name,
                        &residual_law_name,
                        file,
                        module_root,
                    );
                    (src, verdict, false)
                }
            };
            if !seen.insert(src.clone()) {
                continue; // alpha-equivalent branch already accounted for
            }
            match verdict {
                SampleVerdict::Pass => passed.push((src, calculated)),
                SampleVerdict::Fail { counterexample } => {
                    first_fail.get_or_insert((counterexample, src));
                }
                SampleVerdict::Gap(reason) => {
                    gaps.push(reason);
                }
            }
        }
        // Preserve calculator declines in the log even when the raw residual
        // fallback succeeds; only usable source suggestions belong on stdout.
        if let Some(reason) = &decline {
            let _ = writeln!(technical, "{label}: not a forced lemma — {reason}");
        }
        let has_candidate = !passed.is_empty() || first_fail.is_some();
        if has_candidate && !showed_header {
            let _ = writeln!(
                rendered,
                "\n{}",
                "--explain: candidate Aver laws for open goals".bold()
            );
            showed_header = true;
        }
        if !passed.is_empty() {
            for (src, calculated) in &passed {
                let head = if *calculated {
                    "calculated law — sample-check passed, add it and cite:"
                } else {
                    "candidate law — sample-check passed, add it and cite:"
                };
                let _ = writeln!(rendered, "  {label}: {}", head.green());
                // A calculated block is machine-produced, so stamp its
                // provenance directly above the pasteable `verify …`: when the
                // user pastes it and it proves, `--check` records the value in
                // the new law's manifest entry (see `PROVENANCE_MARKER_PREFIX`).
                // `from=` points back at the stuck law this was calculated from.
                if *calculated {
                    let _ = writeln!(
                        rendered,
                        "      {PROVENANCE_MARKER_PREFIX}calculated from={label} tool=explain"
                    );
                }
                for line in src.lines() {
                    let _ = writeln!(rendered, "      {line}");
                }
            }
        } else if let Some((counterexample, src)) = first_fail {
            let _ = writeln!(
                rendered,
                "  {label}: {}",
                "candidate law failed its sample-check:".red()
            );
            let _ = writeln!(rendered, "      {counterexample}");
            for line in src.lines() {
                let _ = writeln!(rendered, "      {line}");
            }
        }
        if !has_candidate && gaps.is_empty() {
            gaps.push("no residual branch in grammar".to_string());
        }
        // A usable branch must not hide another branch's translation failure.
        for reason in gaps {
            let _ = writeln!(technical, "{label}: engine-form gap — {reason}");
        }
        // The multi-arm caveat applies to any verdict — a single-branch candidate
        // may not close a many-branch law whether it passed, failed, or gapped.
        if has_candidate && dump.multi_arm {
            let _ = writeln!(
                rendered,
                "      {}",
                "(the law has more than one open branch — a candidate may not close it alone)"
                    .yellow()
            );
        }
    }
    let log = std::path::Path::new(output_dir).join("proof_candidates.log");
    let saved = std::fs::write(log, &technical).is_ok();
    if !technical.is_empty() {
        if saved {
            let _ = writeln!(
                rendered,
                "  Some helper suggestions are unavailable; details are in proof_candidates.log."
            );
        } else {
            let _ = writeln!(
                rendered,
                "  Some helper suggestions are unavailable; their diagnostic log could not be written."
            );
        }
    }
    rendered
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unavailable_helper_suggestions_keep_their_reason_in_a_dedicated_log() {
        let dir = tempfile::tempdir().unwrap();
        let output = render(
            &[("f.open".into(), "f_law_open".into())],
            &Default::default(),
            &[],
            "unused.av",
            ".",
            dir.path().to_str().unwrap(),
        );
        let log = std::fs::read_to_string(dir.path().join("proof_candidates.log")).unwrap();
        assert!(log.contains("f.open: residual not extractable (engine-form gap)"));
        assert!(output.contains("Some helper suggestions are unavailable"));
        assert!(!output.contains("engine-form gap"));
        assert!(!output.contains("residual not extractable"));
        assert!(!output.contains("candidate Aver laws for open goals"));
    }

    #[test]
    fn usable_candidates_and_their_counterexamples_remain_visible() {
        use serde_json::json;
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("source.av");
        let source = "fn f(x: Int) -> Int\n    x\nverify f law identity\n    given x: Int = [0, 1]\n    f(x) => x\n";
        std::fs::write(&file, source).unwrap();
        let items = aver::source::parse_source(source).unwrap();
        for (rhs, passes) in [(json!({"var": "x"}), true), (json!({"nat": "7"}), false)] {
            let goal = json!({"forall": {"name": "x", "ty": {"const": "Int"}, "body": {"app": {"fn": {"const": "Eq"}, "args": [{"const": "Int"}, {"var": "x"}, rhs]}}}});
            let dump = GoalDump {
                jsons: vec![
                    json!({"opaque": "unsupported residual"}).to_string(),
                    goal.to_string(),
                ],
                multi_arm: true,
            };
            let goals = std::collections::BTreeMap::from([("f.identity".into(), dump)]);
            let output = render(
                &[("f.identity".into(), "f_law_identity".into())],
                &goals,
                &items,
                file.to_str().unwrap(),
                dir.path().to_str().unwrap(),
                dir.path().to_str().unwrap(),
            );
            let expected = if passes {
                "sample-check passed, add it and cite"
            } else {
                "candidate law failed its sample-check"
            };
            assert!(output.contains(expected), "{output}");
            assert!(
                output.contains("verify f law identity_residual"),
                "{output}"
            );
            assert!(!output.contains("engine-form gap"), "{output}");
            let log = std::fs::read_to_string(dir.path().join("proof_candidates.log")).unwrap();
            assert!(log.contains("f.identity: engine-form gap"), "{log}");
            assert!(output.contains("more than one open branch"), "{output}");
            assert!(
                !output.contains("law false as stated"),
                "a helper failure is not a parent-law counterexample: {output}"
            );
        }
    }
}
