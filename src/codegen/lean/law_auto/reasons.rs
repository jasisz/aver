//! Proof structure supplied as ordinary Aver Bool expressions. Each explanation
//! and the final implication is an opaque, independently audited theorem. The
//! original law applies those theorems, retaining every dependency in its proof.

use crate::ast::{Expr, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::lean::{
    LAW_OBLIGATION_MARKER_PREFIX,
    expr::{aver_name_to_lean, emit_expr, resolve_rewrite_output},
};

mod induction;

pub(in crate::codegen::lean) struct ReasonClaim<'a> {
    pub base: &'a str,
    pub label: &'a str,
    pub binders: &'a [(String, String)],
    pub prop: &'a str,
    pub guard: Option<&'a str>,
}

fn dependencies(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Option<Vec<String>> {
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
    let function = ctx.fn_def_by_name(&name, scope.as_deref())?;
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

fn solver(defs: &str, grind_defs: &str, label: &str, indent: &str) -> Vec<String> {
    let grind_defs = [grind_defs, "List.take, List.reverse_eq_nil_iff"]
        .into_iter()
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(", ");
    vec![
        format!("{indent}first"),
        format!("{indent}| (simp_all +zetaDelta [{defs}]; done)"),
        format!("{indent}| ((try simp only [List.contains_eq_mem]); grind [{grind_defs}])"),
        format!("{indent}| (trace \"AVER_REASON_OPEN:{label}\"; trace_state; sorry)"),
    ]
}

pub(in crate::codegen::lean) fn emit_reason_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    claim: ReasonClaim<'_>,
) -> Vec<String> {
    let definitions = induction::definitions(law, ctx);
    let defs = definitions.simp;
    let facts = dependencies(vb, law, ctx);
    let targets = law
        .because
        .iter()
        .map(|r| induction::target(r, law, ctx))
        .collect::<Vec<_>>();
    let grind_defs = if targets.iter().any(Option::is_some) {
        definitions.grind
    } else {
        defs.clone()
    };
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
        lines.push(format!(
            "{LAW_OBLIGATION_MARKER_PREFIX}{name} universal {label}"
        ));
        lines.push(format!(
            "theorem {name} : ∀ {params}, {} := by",
            premise_chain(&reasons[..index], &prop)
        ));
        let hypotheses = previous.iter().map(|h| format!(" {h}")).collect::<String>();
        lines.push(format!("  intro {args}{hypotheses}{guard_intro}"));
        if !final_step || reasons.is_empty() {
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
        }
        if final_step {
            for reason in &law.because {
                if let Some(call) = case_call(reason, ctx) {
                    lines.push(format!("  all_goals try fun_cases {call}"));
                }
            }
            lines.push("  all_goals".to_string());
            lines.extend(solver(&defs, &grind_defs, &label, "    "));
        } else {
            if let Some(call) = &targets[index] {
                // Guards and previous explanations belong in the motive:
                // recursive calls must establish their own premises.
                if !hypotheses.is_empty() || !guard_intro.is_empty() {
                    lines.push(format!("  revert{hypotheses}{guard_intro}"));
                }
                lines.push(format!("  fun_induction {call}"));
            } else if let Some(call) = case_call(&law.because[index], ctx) {
                lines.push(format!("  fun_cases {call}"));
            }
            // Each right-hand goal receives the checked left-hand fact as a
            // premise. No Bool-valued local binding becomes an assumption.
            lines.push(format!(
                "  all_goals repeat' first | apply {and_rule} | intro"
            ));
            lines.push("  all_goals".to_string());
            lines.extend(solver(&defs, &grind_defs, &label, "    "));
            previous.push(format!("h_reason{index}"));
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
