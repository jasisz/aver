//! Proof structure supplied as ordinary Aver Bool expressions. Each explanation
//! and the final implication is an opaque, independently audited theorem. The
//! original law applies those theorems, retaining every dependency in its proof.

use crate::ast::{Expr, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::lean::{
    LAW_OBLIGATION_MARKER_PREFIX,
    expr::{aver_name_to_lean, emit_expr, resolve_rewrite_output},
};

mod composition;
mod equivalence;
mod induction;
mod list_induction;
mod transport;

pub(in crate::codegen::lean) struct ReasonClaim<'a> {
    pub base: &'a str,
    pub label: &'a str,
    pub binders: &'a [(String, String)],
    pub prop: &'a str,
    pub guard: Option<&'a str>,
}

pub(in crate::codegen::lean) fn dependencies(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<Vec<String>> {
    let blocks = super::shared::same_file_verify_blocks(ctx);
    let earlier: Vec<_> = blocks
        .into_iter()
        .take_while(|b| b.line != vb.line || b.fn_name != vb.fn_name)
        .collect();
    if let Some(selected) = &law.using {
        let mut names = Vec::new();
        // A set of dependencies: declaration order in `using` is immaterial.
        let mut selected = selected.clone();
        selected.sort();
        for name in selected {
            let local = earlier.iter().find_map(|b| {
                let VerifyKind::Law(l) = &b.kind else {
                    return None;
                };
                (name == format!("{}.{}", b.fn_name, l.name)).then_some((*b, l.as_ref()))
            });
            if let Some((b, l)) = local {
                names.push(crate::codegen::lean::toplevel::law_as_lemma_statement(b, l, ctx)?.0);
                continue;
            }
            let mut found = None;
            for module in &ctx.modules {
                for block in &module.verify_laws {
                    let VerifyKind::Law(dependency) = &block.kind else {
                        continue;
                    };
                    if name == format!("{}.{}.{}", module.prefix, block.fn_name, dependency.name) {
                        found = ctx.with_module_scope(Some(&module.prefix), || {
                            crate::codegen::lean::toplevel::law_as_lemma_statement(
                                block, dependency, ctx,
                            )
                            .map(|(theorem, _)| {
                                format!("{}.{}", aver_name_to_lean(&module.prefix), theorem)
                            })
                        });
                    }
                }
            }
            names.push(found?);
        }
        Some(names)
    } else {
        Some(
            super::induction::earlier_law_lemmas(vb, law, ctx)
                .into_iter()
                .map(|lemma| lemma.name)
                .collect(),
        )
    }
}

fn case_call(expr: &crate::ast::Spanned<Expr>, ctx: &CodegenContext) -> Option<String> {
    let Expr::FnCall(callee, _) = &expr.node else {
        return None;
    };
    let name = crate::checker::expr_to_str(callee);
    let scope = ctx.active_module_scope();
    let function = induction::callee(expr, ctx, scope.as_deref())?;
    let id = ctx.symbol_table.resolve_fn_id_in(&name, scope.as_deref())?;
    if !function.effects.is_empty() || ctx.recursive_fns.contains(&id) {
        return None;
    }
    Some(emit_expr(&resolve_rewrite_output(expr, ctx, None), ctx))
}

fn premise_chain(premises: &[String], prop: &str) -> String {
    premises
        .iter()
        .map(|p| format!("({p}) = true → "))
        .collect::<String>()
        + prop
}

fn solver(
    definitions: &induction::Definitions,
    label: &str,
    indent: &str,
    fact_count: usize,
    report_open: bool,
    saturate: bool,
) -> Vec<String> {
    // Cited theorems remain available to grind, but are not unconditional
    // rewrite rules: an accumulator equation can rewrite its own result.
    // A cone that calls `Map.set` gets the prelude's facts about it, and one
    // that calls `Map.remove` the removal's own size fact; the same names make
    // the demand-driven prelude ship their proofs.
    let mut cited: Vec<&str> = Vec::new();
    if definitions.map_facts {
        cited.extend(crate::codegen::lean::prelude::MAP_SET_FACT_LEMMAS);
    }
    if definitions.map_remove_facts {
        cited.extend(crate::codegen::lean::prelude::MAP_REMOVE_FACT_LEMMAS);
    }
    let map_facts = cited.join(", ");
    let simp_defs = [definitions.simp.as_str(), map_facts.as_str()]
        .into_iter()
        .filter(|s| !s.is_empty())
        .map(str::to_string)
        .chain((0..fact_count).map(|i| format!("-_fact{i}")))
        .collect::<Vec<_>>()
        .join(", ");
    let grind_defs = [
        definitions.grind.as_str(),
        map_facts.as_str(),
        // Aver counts are Ints; use the guarded library equations instead of
        // unfolding take's Nat recursion behind an opaque Int.toNat argument.
        "List.take_cons, List.drop_cons, Int.toNat_of_nonpos, List.reverse_eq_nil_iff",
    ]
    .into_iter()
    .filter(|s| !s.is_empty())
    .collect::<Vec<_>>()
    .join(", ");
    let mut lines = vec![
        format!("{indent}first"),
        // Definitionally equal expressions need no rewrite theorem. In
        // particular, closed `because` computations are checked by kernel
        // reduction, without native_decide or a builtin-specific lemma list.
        format!("{indent}| rfl"),
    ];
    // Try the law's own equations before expanding its implementation cone.
    // In particular, an induction hypothesis can summarize an opaque element
    // transformation; opening that transformation only enlarges the search.
    if definitions.structural_reason && !definitions.head_equations.is_empty() {
        lines.push(format!(
            "{indent}| (grind only [{}])",
            definitions.head_equations
        ));
    }
    lines.push(format!(
        "{indent}| (simp_all +zetaDelta [{simp_defs}]; done)"
    ));
    // Apply a cited conclusion before arithmetic normalization can erase its
    // matching syntax. This is one theorem application, with every remaining
    // premise checked from the current context; no recursive rewrite loop.
    // A premise can itself need a universally quantified citation (for example,
    // positivity of a recursive scale). Saturate that smaller goal using the
    // cited facts before the fallback expands the original recursive terms.
    for i in 0..fact_count {
        lines.push(format!("{indent}| (simp only [Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at *; with_reducible apply _fact{i} <;> (first | assumption | omega | (with_reducible grind only)))"));
    }
    // Expose named Bool facts before simp_all substitutes their truth values.
    // Normalize multiplication by constants before Presburger arithmetic treats
    // the remaining products as opaque integer terms.
    lines.push(format!(
        "{indent}| ((try simp only [Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq, {}] at *) <;> simp_all +zetaDelta [Int.mul_assoc, Int.add_assoc, {simp_defs}] <;> omega)",
        definitions.simp
    ));
    if saturate {
        lines.push(format!(
            "{indent}| ((try simp only [List.contains_eq_mem]); grind [{grind_defs}])"
        ));
        let mut steps = Vec::new();
        for (name, reason) in &definitions.unfold_once {
            let location = if *reason { " at *" } else { "" };
            steps.push(format!(
                "(try rw [{name}.eq_def]{location}) <;> (try simp only [{}] at *)",
                definitions.simp
            ));
            let prefix = steps.join(" <;> ");
            lines.push(format!(
                "{indent}| ((try simp only [{}] at *) <;> {prefix} <;> grind [{grind_defs}])",
                definitions.simp
            ));
        }
    }
    if report_open {
        lines.push(format!(
            "{indent}| (trace \"AVER_REASON_OPEN:{label}\"; trace_state; sorry)"
        ));
    }
    lines
}

pub(in crate::codegen::lean) fn emit_reason_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    claim: ReasonClaim<'_>,
) -> Vec<String> {
    let facts = dependencies(vb, law, ctx);
    let plans = law
        .because
        .iter()
        .enumerate()
        .map(|(index, r)| induction::plan(vb, index, r, law, ctx))
        .collect::<Vec<_>>();
    let definitions = induction::definitions(vb, law, ctx);
    let params = claim
        .binders
        .iter()
        .map(|(n, t)| format!("({n} : {t})"))
        .collect::<Vec<_>>()
        .join(" ");
    let args = claim
        .binders
        .iter()
        .map(|(n, _)| n.as_str())
        .collect::<Vec<_>>()
        .join(" ");
    let guard_intro = if claim.guard.is_some() { " h_when" } else { "" };
    let reasons = law
        .because
        .iter()
        .map(|r| emit_expr(&resolve_rewrite_output(r, ctx, None), ctx))
        .collect::<Vec<_>>();
    let and_rule = format!("__aver_reason_and_{}", claim.base);
    let mut lines = vec![
        format!(
            "private theorem {and_rule} {{a b : Bool}} (ha : a = true) (hb : a = true → b = true) : (a && b) = true :="
        ),
        "  by simp_all".to_string(),
    ];
    let mut previous = Vec::new();
    for index in 0..=reasons.len() {
        let final_step = index == reasons.len();
        let step = if final_step {
            "implication".to_string()
        } else {
            format!("because{}", index + 1)
        };
        let name = format!("__aver_reason_{}_{}", claim.base, step);
        let label = format!("{}.{}", claim.label, step);
        let prop = if final_step {
            claim.prop.to_string()
        } else {
            let goal = format!("({}) = true", reasons[index]);
            match claim.guard {
                Some(h) => format!("({h}) = true → {goal}"),
                None => goal,
            }
        };
        let waterfall_start = lines.len();
        lines.push(format!(
            "{LAW_OBLIGATION_MARKER_PREFIX}{name} universal {label}"
        ));
        lines.push(format!(
            "theorem {name} : ∀ {params}, {} := by",
            premise_chain(&reasons[..index], &prop)
        ));
        let hypotheses = previous.iter().map(|h| format!(" {h}")).collect::<String>();
        lines.push(format!("  intro {args}{hypotheses}{guard_intro}"));
        // Citation scope belongs to the law, including its final implication.
        let fact_count = facts.as_ref().map_or(0, Vec::len);
        if let Some(facts) = &facts {
            for (i, fact) in facts.iter().enumerate() {
                lines.push(format!("  have _fact{i} := {fact}"));
                lines.push(format!("  try simp only [List.contains_eq_mem, Bool.or_eq_true, Bool.and_eq_true, decide_eq_decide, decide_eq_true_eq, Bool.not_eq_true', ge_iff_le, gt_iff_lt] at _fact{i}"));
            }
        } else {
            lines.push(format!(
                "  trace \"AVER_REASON_OPEN:{label}:dependency has no available theorem\""
            ));
            lines.push("  sorry".to_string());
            if !final_step {
                previous.push(format!("h_reason{index}"));
            }
            continue;
        }
        let strategy_start = lines.len();
        if final_step {
            let mut inductive = list_induction::candidates(
                law,
                &definitions,
                &format!("{hypotheses}{guard_intro}"),
                fact_count,
            );
            if let Some(candidate) = equivalence::candidate(law, ctx, &definitions, fact_count) {
                inductive.push(candidate);
            }
            // Induction supplies recursive equations and saturates its leaves.
            // Repeating saturation on the original arbitrary history after a
            // failed induction can exhaust elaboration before reporting a gap.
            // Explicit explanations still need their equations: a proved
            // recursive Bool reason may expose the conclusion only by unfolding.
            let saturate = inductive.is_empty() || !law.because.is_empty();
            if !inductive.is_empty() {
                lines.push("  first".to_string());
                if let Some(candidate) = transport::candidate(vb, law, ctx, fact_count) {
                    lines.push(format!("  | {candidate}"));
                }
                if let Some(candidate) =
                    composition::candidate(vb, law, ctx, &definitions, fact_count)
                {
                    lines.push(format!("  | {candidate}"));
                }
                if let Some(candidate) =
                    composition::summary_candidate(vb, law, ctx, &definitions, fact_count)
                {
                    lines.push(format!("  | {candidate}"));
                }
                // Compose equations before spending work on induction. Bool
                // invariants go straight to induction: matching a predicate
                // on an arbitrary recursive result can itself exhaust isDefEq.
                if !matches!(law.rhs.node, Expr::Literal(crate::ast::Literal::Bool(true))) {
                    lines.push("  | (simp only [Bool.and_eq_true, beq_iff_eq, List.contains_eq_mem] at *; with_reducible grind only)".to_string());
                    // Reveal nonrecursive outer wrappers so cited conclusions
                    // match the source call they summarize. Recursive callees
                    // stay opaque; unfolding the full cone defeats composition.
                    if !definitions.heads.is_empty() {
                        lines.push(format!("  | (simp only [{}, Bool.and_eq_true, beq_iff_eq, List.contains_eq_mem] at *; with_reducible grind only)", definitions.heads));
                    }
                }
                for candidate in inductive {
                    lines.push(format!("  | {candidate}"));
                }
                lines.push("  |".to_string());
            }
            let final_start = lines.len();
            let cases = law
                .because
                .iter()
                .filter_map(|reason| case_call(reason, ctx))
                .collect::<Vec<_>>();
            let sum_givens = super::shared::user_sum_givens(ctx, law);
            // A small pair of changes can hide independent map writes behind
            // two matches. Bound the constructor product, and split only after
            // the ordinary solver has had a chance to compose existing facts.
            let split_maps = definitions.map_facts
                && !sum_givens.is_empty()
                && sum_givens.len() <= 2
                && sum_givens
                    .iter()
                    .try_fold(1usize, |n, (_, count)| n.checked_mul(*count))
                    .is_some_and(|count| count <= 16);
            if cases.is_empty() && !split_maps {
                lines.push("  all_goals".to_string());
                lines.extend(solver(
                    &definitions,
                    &label,
                    "    ",
                    fact_count,
                    true,
                    saturate,
                ));
            } else {
                // Earlier explanations already carry the facts needed by the
                // implication. Try composing them before fun_cases multiplies
                // the goals and repeats normalization in every branch. This
                // attempt has no admission floor: an open goal must backtrack
                // into the existing case-analysis strategy below.
                lines.push("  first".to_string());
                lines.push("  |".to_string());
                lines.extend(solver(
                    &definitions,
                    &label,
                    "    ",
                    fact_count,
                    false,
                    saturate,
                ));
                if split_maps {
                    let splits = sum_givens
                        .iter()
                        .map(|(name, _)| format!("cases {name}"))
                        .collect::<Vec<_>>()
                        .join(" <;> ");
                    let mut map_facts = crate::codegen::lean::prelude::MAP_SET_FACT_LEMMAS.to_vec();
                    if definitions.map_remove_facts {
                        map_facts.extend(crate::codegen::lean::prelude::MAP_REMOVE_FACT_LEMMAS);
                    }
                    let map_facts = map_facts.join(", ");
                    let excluded = (0..fact_count)
                        .map(|i| format!(", -_fact{i}"))
                        .collect::<String>();
                    // Reveal a result predicate before splitting its event:
                    // otherwise unchanged record fields stay hidden behind it.
                    // Retain removal facts beside insertion facts in each arm.
                    let heads = if definitions.heads.is_empty() {
                        String::new()
                    } else {
                        format!("(try simp only [{}] at *) <;> ", definitions.heads)
                    };
                    lines.push(format!("  | ({heads}{splits} <;> simp_all +zetaDelta [{}, {map_facts}{excluded}] <;> grind [{map_facts}])", definitions.simp));
                }
                lines.push("  |".to_string());
                for call in cases {
                    lines.push(format!("    all_goals try fun_cases {call}"));
                }
                lines.push("    all_goals".to_string());
                lines.extend(solver(
                    &definitions,
                    &label,
                    "      ",
                    fact_count,
                    true,
                    saturate,
                ));
            }
            if final_start > strategy_start {
                for line in &mut lines[final_start..] {
                    *line = format!("  {line}");
                }
            }
        } else {
            if let Some(plan) = &plans[index] {
                // Guards and previous explanations belong in the motive:
                // recursive calls must establish their own premises.
                if !hypotheses.is_empty() || !guard_intro.is_empty() {
                    lines.push(format!("  revert{hypotheses}{guard_intro}"));
                }
                lines.push(format!("  {plan}"));
            } else if let Some(call) = case_call(&law.because[index], ctx) {
                lines.push(format!("  fun_cases {call}"));
            }
            // Split the explanation's cases before its ordered facts, so the
            // right-hand goal keeps the same branch premises. Reduce local
            // lets only when they hide a case; neither operation unfolds a
            // recursive call or assumes a Bool-valued binding is true.
            lines.push(format!(
                "  all_goals repeat' first | (intro) | (split) | (dsimp only; split) | apply {and_rule}"
            ));
            lines.push("  all_goals".to_string());
            lines.extend(solver(&definitions, &label, "    ", fact_count, true, true));
            previous.push(format!("h_reason{index}"));
        }
        // First use the named facts without expanding their dependency cones.
        // A direct citation must also precede case analysis, which can erase
        // constant arguments needed to match the cited conclusion. Restricted
        // transparency prevents unrelated conclusions from unfolding recursion.
        let direct_facts = if !final_step && matches!(law.because[index].node, Expr::FnCall(..)) {
            fact_count
        } else {
            0
        };
        let conclusion_equations = final_step && !definitions.head_equations.is_empty();
        if !definitions.heads.is_empty() || direct_facts > 0 || conclusion_equations {
            let structured = lines.split_off(strategy_start);
            let excluded = (0..fact_count)
                .map(|i| format!("-_fact{i}"))
                .collect::<Vec<_>>()
                .join(", ");
            let shallow = format!(
                "simp only [Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq, {}] at *; simp_all only [and_self, and_true, true_and, {excluded}] <;> omega",
                definitions.heads
            );
            let premise_simp = [
                "Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq",
                definitions.simp.as_str(),
                excluded.as_str(),
            ]
            .into_iter()
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>()
            .join(", ");
            lines.push("  first".to_string());
            // The explanation is already a checked hypothesis here. Its
            // equation may expose the conclusion before wrappers or induction
            // expand unrelated recursive implementations.
            if conclusion_equations {
                lines.push(format!("  | (grind only [{}])", definitions.head_equations));
            }
            if !definitions.heads.is_empty() {
                lines.push(format!("  | ({shallow})"));
            }
            for i in 0..direct_facts {
                lines.push(format!("  | (simp only [Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at *; with_reducible apply _fact{i} <;> (first | assumption | omega | ({shallow}) | (simp_all only [{premise_simp}]; grind)))"));
                if !definitions.heads.is_empty() {
                    // Unwrap the named Bool explanation before applying a
                    // cited equation. Keeping its computed arguments opaque
                    // preserves the theorem's matching syntax.
                    lines.push(format!("  | (simp only [{}, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at *; with_reducible apply _fact{i} <;> (first | assumption | omega))", definitions.heads));
                }
            }
            lines.push("  |".to_string());
            lines.extend(structured.into_iter().map(|line| format!("  {line}")));
        }
        if let Some(hints) = &facts {
            crate::codegen::lean::waterfall::Candidate {
                name,
                label,
                statement: format!("∀ {params}, {}", premise_chain(&reasons[..index], &prop)),
                hints: hints.clone(),
                obligation: true,
                baseline_universal: true,
            }
            .wrap(&mut lines, waterfall_start);
        }
    }
    lines.push(format!(
        "theorem {} : ∀ {params}, {} := by",
        claim.base, claim.prop
    ));
    lines.push(format!("  intro {args}{guard_intro}"));
    for index in 0..reasons.len() {
        let earlier = (0..index)
            .map(|i| format!(" _reason{i}"))
            .collect::<String>();
        lines.push(format!(
            "  have _reason{index} := __aver_reason_{}_because{} {args}{earlier}{guard_intro}",
            claim.base,
            index + 1
        ));
    }
    let all = (0..reasons.len())
        .map(|i| format!(" _reason{i}"))
        .collect::<String>();
    lines.push(format!(
        "  exact __aver_reason_{}_implication {args}{all}{guard_intro}",
        claim.base
    ));
    lines
}
