//! Normalize the interfaces of explicitly cited computations before induction.
//! Only pure nonrecursive definitions unfold during normalization. Checked
//! recursive equations are offered to `grind` after the citations rewrite the
//! goal, so a completed helper can reduce without expanding its whole history.
use super::induction::{self, Definitions};
use crate::ast::{BinOp, Expr, VerifyBlock, VerifyLaw};
use crate::codegen::{CodegenContext, common};
use std::collections::BTreeSet;

/// Compose cited summaries while leaving their recursive implementations opaque.
pub(super) fn summary_candidate(
    block: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    definitions: &Definitions,
    fact_count: usize,
) -> Option<String> {
    let selected = law.using.as_ref()?;
    if fact_count == 0 || !law.because.is_empty() {
        return None;
    }
    let scope = ctx.active_module_scope();
    let mut empty_cases = BTreeSet::new();
    let summarized: BTreeSet<_> = selected
        .iter()
        .filter_map(|selected| selected.rsplit_once('.'))
        .filter_map(|(function, _)| {
            let id = ctx
                .symbol_table
                .resolve_fn_id_in(function, scope.as_deref())?;
            let key = &ctx.symbol_table.fn_entry(id).key;
            let fd = ctx.fn_def_by_name(&key.name, key.scope_str())?;
            if induction::list_measure(fd, ctx).is_some()
                && let [crate::ast::Stmt::Expr(expr)] = fd.body.stmts()
                && let Expr::Match { arms, .. } = &expr.node
                && arms
                    .first()
                    .is_some_and(|arm| matches!(arm.pattern, crate::ast::Pattern::EmptyList))
            {
                empty_cases.insert(induction::lean_name(fd, ctx));
            }
            ctx.recursive_fns
                .contains(&id)
                .then(|| induction::lean_name(fd, ctx))
        })
        .collect();
    // A law about an arbitrary recursive history still needs induction when
    // its citations summarize only the individual transition. Do not expand
    // that history before the existing induction candidate can use them.
    if let Some(id) = ctx.law_target_fn_id(&block.fn_name)
        && ctx.recursive_fns.contains(&id)
    {
        let key = &ctx.symbol_table.fn_entry(id).key;
        let fd = ctx.fn_def_by_name(&key.name, key.scope_str())?;
        if !summarized.contains(&induction::lean_name(fd, ctx)) {
            return None;
        }
    }
    let mut equations: Vec<_> = definitions
        .grind
        .split(", ")
        .filter(|entry| !entry.is_empty())
        .filter(|entry| {
            !summarized.contains(entry.trim_start_matches("= ").trim_end_matches(".eq_def"))
        })
        .map(str::to_string)
        .collect();
    // A summary may mention its empty result. Its completed constructor is
    // safe to reduce without opening the recursive computation on arbitrary input.
    for name in &empty_cases {
        equations.push(format!("{name}.eq_1"));
    }
    equations.extend(
        [
            "List.reverse_cons",
            "List.reverse_append",
            "List.reverse_reverse",
            "List.reverse_nil",
            "List.nil_append",
            "List.append_nil",
            "List.cons_append",
            "List.append_assoc",
        ]
        .map(str::to_string),
    );
    Some(format!("(grind only [{}])", equations.join(", ")))
}

pub(super) fn candidate(
    block: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    definitions: &Definitions,
    fact_count: usize,
) -> Option<String> {
    let selected = law.using.as_ref()?;
    if fact_count == 0 || definitions.list_steps.is_empty() || !law.because.is_empty() {
        return None;
    }
    let (left, right) = match &law.lhs.node {
        Expr::BinOp(BinOp::Eq, a, b) => (a.as_ref(), b.as_ref()),
        _ => (&law.lhs, &law.rhs),
    };
    if !matches!(left.node, Expr::FnCall(..)) || !matches!(right.node, Expr::FnCall(..)) {
        return None;
    }
    let scope = ctx.active_module_scope();
    let mut sorted = selected.clone();
    sorted.sort();
    // Other earlier laws may be in scope through omitted `using`. Compose
    // only citations that mention a recursive computation in this goal's
    // cone; shared nonrecursive utilities do not make two traces related.
    let recursive: BTreeSet<_> = definitions.list_steps.split(", ").collect();
    let relevant: BTreeSet<_> = sorted
        .iter()
        .enumerate()
        .filter_map(|(index, selected)| {
            let (function, label) = selected.rsplit_once('.')?;
            let id = ctx
                .symbol_table
                .resolve_fn_id_in(function, scope.as_deref())?;
            let theorem = ctx
                .proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == id && t.law_name == label)?;
            theorem
                .function_cone
                .iter()
                .any(|id| {
                    let key = &ctx.symbol_table.fn_entry(*id).key;
                    ctx.fn_def_by_name(&key.name, key.scope_str())
                        .is_some_and(|fd| {
                            recursive.contains(induction::lean_name(fd, ctx).as_str())
                        })
                })
                .then_some(index)
        })
        .collect();
    let forward: Vec<_> = sorted.iter().enumerate().filter(|(index, _)| relevant.contains(index)).filter_map(|(index, selected)| {
        let (function, _) = selected.rsplit_once('.')?;
        let id = ctx.symbol_table.resolve_fn_id_in(function, scope.as_deref())?;
        let key = &ctx.symbol_table.fn_entry(id).key;
        let fd = ctx.fn_def_by_name(&key.name, key.scope_str())?;
        let wrapper = matches!(fd.body.stmts(), [crate::ast::Stmt::Expr(expr)] if matches!(expr.node, Expr::FnCall(..)));
        (!wrapper || ctx.recursive_fns.contains(&id)).then(|| format!("_fact{index}"))
    }).collect();
    if forward.is_empty() {
        return None;
    }

    let mut cones = Vec::new();
    let mut cited_cones = Vec::new();
    let mut summarized = BTreeSet::new();
    for (function, label) in sorted
        .iter()
        .enumerate()
        .filter(|(index, _)| relevant.contains(index))
        .filter_map(|(_, s)| s.rsplit_once('.'))
        .chain([(block.fn_name.as_str(), law.name.as_str())])
    {
        let id = ctx
            .symbol_table
            .resolve_fn_id_in(function, scope.as_deref())?;
        let theorem = ctx
            .proof_ir
            .law_theorems
            .iter()
            .find(|t| t.fn_id == id && t.law_name == label)?;
        if function != block.fn_name || label != law.name {
            cited_cones.extend(theorem.function_cone.iter().copied());
            let key = &ctx.symbol_table.fn_entry(id).key;
            if let Some(fd) = ctx.fn_def_by_name(&key.name, key.scope_str()) {
                let wrapper = matches!(fd.body.stmts(), [crate::ast::Stmt::Expr(expr)] if matches!(expr.node, Expr::FnCall(..)));
                if !wrapper || ctx.recursive_fns.contains(&id) {
                    // A checked summary is also a boundary when its adapter
                    // itself is nonrecursive. Keep that call available for
                    // rewriting before projecting a recursively computed record.
                    summarized.insert(induction::lean_name(fd, ctx));
                }
            }
        }
        cones.extend(theorem.function_cone.iter().copied());
    }
    let mut plain = BTreeSet::new();
    let mut completed_boundaries = BTreeSet::new();
    let mut input_match = false;
    let mut opaque = BTreeSet::new();
    for id in cited_cones {
        let key = &ctx.symbol_table.fn_entry(id).key;
        let Some(fd) = ctx.fn_def_by_name(&key.name, key.scope_str()) else {
            continue;
        };
        if ctx.recursive_fns.contains(&id) {
            continue;
        }
        for stmt in fd.body.stmts() {
            let (crate::ast::Stmt::Expr(expr) | crate::ast::Stmt::Binding(_, _, expr)) = stmt;
            crate::codegen::expr_walk::walk(expr, &mut |expr| {
                if let Some(callee) = induction::callee(expr, ctx, key.scope_str())
                    && induction::list_measure(callee, ctx).is_some()
                    && let Expr::FnCall(_, args) = &expr.node
                {
                    for arg in args {
                        if let Some(boundary) = induction::callee(arg, ctx, key.scope_str())
                            && let Expr::FnCall(_, values) = &arg.node
                            && values.iter().all(|value| match &value.node {
                                Expr::Ident(name) | Expr::Resolved { name, .. } => {
                                    fd.params.iter().any(|(param, _)| param == name)
                                }
                                _ => false,
                            })
                        {
                            opaque.insert(induction::lean_name(boundary, ctx));
                        }
                    }
                }
            });
        }
    }
    let mut steps: BTreeSet<_> = definitions
        .list_steps
        .split(", ")
        .filter(|name| !summarized.contains(*name))
        .map(|name| format!("= {name}.eq_def"))
        .collect();
    for id in cones {
        let key = &ctx.symbol_table.fn_entry(id).key;
        let Some(fd) = ctx.fn_def_by_name(&key.name, key.scope_str()) else {
            continue;
        };
        if !fd.effects.is_empty() {
            continue;
        }
        input_match |= !ctx.recursive_fns.contains(&id) && parameter_match(fd, true);
        let name = induction::lean_name(fd, ctx);
        if summarized.contains(&name) {
            continue;
        }
        if opaque.contains(&name) {
            if first_constructor_branch(fd) {
                completed_boundaries.insert(format!("{name}.eq_1"));
            }
            if parameter_match(fd, false) {
                steps.insert(format!("= {name}.eq_def"));
            }
        } else if common::fn_id_for_decl(ctx, fd).is_some_and(|id| !ctx.recursive_fns.contains(&id))
        {
            plain.insert(name);
        }
    }
    let facts = forward.join(", ");
    let completion = plain.union(&opaque).cloned().collect::<Vec<_>>().join(", ");
    let equations = steps
        .iter()
        .map(|s| s.trim_start_matches("= ").trim_end_matches(".eq_def"))
        .collect::<Vec<_>>()
        .join(", ");
    let opening = definitions
        .list_steps
        .split(", ")
        .filter(|name| !summarized.contains(*name))
        .map(|name| format!(" | (conv => rhs; rw [{name}.eq_def])"))
        .collect::<String>();
    let reverse = (0..fact_count)
        .map(|i| format!("(try (conv => rhs; rw [← _fact{i}]))"))
        .collect::<Vec<_>>()
        .join("; ");
    let staged_plain = plain
        .iter()
        .chain(completed_boundaries.iter())
        .cloned()
        .collect::<Vec<_>>()
        .join(", ");
    let plain = plain.into_iter().collect::<Vec<_>>().join(", ");
    let steps = steps.into_iter().collect::<Vec<_>>().join(", ");
    let splices: Vec<_> = relevant
        .iter()
        .map(|i| format!("_fact{i}"))
        .filter(|name| !forward.contains(name))
        .collect();
    let excluded = (0..fact_count)
        .map(|i| format!("-_fact{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let rewrite = splices
        .iter()
        .map(|name| format!(" | (conv => rhs; rw [← {name}])"))
        .collect::<String>();
    // Compose one cited boundary at a time, then distinguish completion from
    // suspension. Expanding all recursive results together duplicates every
    // cursor projection across subsequent calls and overwhelms congruence.
    // The number of cited boundaries bounds this attempt; an unsupported
    // composition must close by another candidate or remain an obligation.
    let staged = if splices.is_empty() {
        String::new()
    } else {
        // An in-place effect can put a finite input match outside the cited
        // call. Expose that prefix only when a direct rewrite cannot apply;
        // matching the right side first avoids splitting the helper's result
        // before the shared input prefix is known.
        let prefix_cases = if input_match {
            format!(
                " | ((repeat' first | rfl | (simp_all +zetaDelta only [{plain}, {excluded}]) | (symm; split <;> symm)); all_goals (first | rfl | (first{rewrite})))"
            )
        } else {
            String::new()
        };
        let step = format!(
            "all_goals (first | rfl | ((first | (first{rewrite}){prefix_cases}); all_goals (try split); all_goals (try simp_all +zetaDelta only [{staged_plain}, {excluded}]))); "
        );
        format!(
            " | ({}all_goals (repeat' first | rfl | (simp_all +zetaDelta [{completion}, {equations}, {excluded}]) | split); done)",
            step.repeat(splices.len())
        )
    };
    let prefix = if input_match {
        format!(
            " | ((first{opening}); (try simp only [{plain}]); (repeat' first | rfl | (simp_all +zetaDelta only [{facts}]) | split); all_goals ({reverse}); all_goals (simp_all +zetaDelta only [{completion}, {equations}]); done)"
        )
    } else {
        String::new()
    };
    // First open only the law's outer wrappers. Their matches may enclose a
    // computation already summarized by a citation; unfolding its helpers
    // before applying that summary can make a small composition intractable.
    let shallow_defs = [definitions.heads.as_str(), facts.as_str()]
        .into_iter()
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join(", ");
    let shallow = format!(
        "(simp only [{}]; (repeat' first | rfl | (simp_all +zetaDelta only [{shallow_defs}]) | split); done)",
        definitions.heads
    );
    Some(format!(
        "(first | {shallow} | (simp only [Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq, {plain}] at *; simp only [{facts}]; first{prefix}{staged} | (grind only [{steps}])))"
    ))
}

fn parameter_match(fd: &crate::ast::FnDef, lists_only: bool) -> bool {
    fd.body.stmts().iter().any(|stmt| {
        let (crate::ast::Stmt::Expr(expr) | crate::ast::Stmt::Binding(_, _, expr)) = stmt;
        crate::codegen::expr_walk::any(expr, &mut |expr| {
            if let Expr::Match { subject, arms } = &expr.node
                && arms.len() > 1
                && let Expr::Ident(name) | Expr::Resolved { name, .. } = &subject.node
            {
                return fd
                    .params
                    .iter()
                    .any(|(n, ty)| n == name && (!lists_only || ty.starts_with("List<")));
            }
            false
        })
    })
}

/// A constructor-specific equation advances a completed boundary without
/// unfolding a different call whose outcome is still unknown. The first flat
/// branch has Lean's first kernel-generated equation, irrespective of names.
pub(super) fn first_constructor_branch(fd: &crate::ast::FnDef) -> bool {
    let [crate::ast::Stmt::Expr(expr)] = fd.body.stmts() else {
        return false;
    };
    let Expr::Match { subject, arms } = &expr.node else {
        return false;
    };
    let (Expr::Ident(name) | Expr::Resolved { name, .. }) = &subject.node else {
        return false;
    };
    fd.params.iter().any(|(param, _)| param == name)
        && arms.first().is_some_and(|arm| {
            matches!(arm.pattern, crate::ast::Pattern::Constructor(..))
                && !crate::codegen::expr_walk::any(&arm.body, &mut |expr| {
                    matches!(expr.node, Expr::Match { .. })
                })
        })
}
