//! Induct on the original input when two checked folds communicate through
//! list maps. Keep result adapters opaque until the input branches agree;
//! singleton and append facts summarize maps without expanding arbitrary tails.
use super::induction;
use crate::ast::{BinOp, Expr, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::lean::expr::aver_name_to_lean;
use crate::codegen::{CodegenContext, common};
use std::collections::BTreeSet;

pub(super) fn candidate(
    block: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    fact_count: usize,
) -> Option<String> {
    if !law.because.is_empty() || fact_count < 2 {
        return None;
    }
    let selected = law.using.as_ref()?;
    let scope = ctx.active_module_scope();
    let (left, right) = match &law.lhs.node {
        Expr::BinOp(BinOp::Eq, a, b) => (a.as_ref(), b.as_ref()),
        _ => (&law.lhs, &law.rhs),
    };
    let left_fn = induction::callee(left, ctx, scope.as_deref())?;
    let right_fn = induction::callee(right, ctx, scope.as_deref())?;
    let [Stmt::Expr(call)] = left_fn.body.stmts() else {
        return None;
    };
    let driver = induction::callee(call, ctx, scope.as_deref())?;
    let measure = induction::list_measure(driver, ctx)?;
    let Expr::FnCall(_, args) = &call.node else {
        return None;
    };
    let index = driver.params.iter().position(|(name, _)| name == measure)?;
    let (Expr::Ident(parameter)
    | Expr::Resolved {
        name: parameter, ..
    }) = &args.get(index)?.node
    else {
        return None;
    };
    let parameter_index = left_fn
        .params
        .iter()
        .position(|(name, _)| name == parameter)?;
    let Expr::FnCall(_, args) = &left.node else {
        return None;
    };
    let (Expr::Ident(input) | Expr::Resolved { name: input, .. }) =
        &args.get(parameter_index)?.node
    else {
        return None;
    };
    if !law.givens.iter().any(|g| g.name == *input) {
        return None;
    }
    let mut cone = BTreeSet::new();
    let mut imported = false;
    for (function, label) in selected
        .iter()
        .filter_map(|s| s.rsplit_once('.'))
        .chain([(block.fn_name.as_str(), law.name.as_str())])
    {
        let id = ctx
            .symbol_table
            .resolve_fn_id_in(function, scope.as_deref())?;
        imported |= ctx.symbol_table.fn_entry(id).key.scope_str() != scope.as_deref();
        let theorem = ctx
            .proof_ir
            .law_theorems
            .iter()
            .find(|t| t.fn_id == id && t.law_name == label)?;
        cone.extend(theorem.function_cone.iter().copied());
    }
    if !imported {
        return None;
    }
    let functions: Vec<_> = cone
        .iter()
        .filter_map(|id| {
            let key = &ctx.symbol_table.fn_entry(*id).key;
            ctx.fn_def_by_name(&key.name, key.scope_str())
        })
        .collect();
    // Only the two folds compared by this law are induction boundaries.
    // Deeper imports may contribute further recursive helpers to its cone;
    // their presence does not change this transport obligation.
    let mut right_folds = BTreeSet::new();
    let owner = common::fn_owning_scope_for(ctx, right_fn);
    for stmt in right_fn.body.stmts() {
        let (Stmt::Expr(expr) | Stmt::Binding(_, _, expr)) = stmt;
        crate::codegen::expr_walk::walk(expr, &mut |expr| {
            if let Some(fd) = induction::callee(expr, ctx, owner)
                && induction::list_measure(fd, ctx).is_some()
                && !fd.return_type.starts_with("List<")
            {
                right_folds.insert(induction::lean_name(fd, ctx));
            }
        });
    }
    if right_folds.len() != 1 {
        return None;
    }
    let driver_name = induction::lean_name(driver, ctx);
    let folds: Vec<_> = functions
        .iter()
        .copied()
        .filter(|fd| {
            let name = induction::lean_name(fd, ctx);
            name == driver_name || right_folds.contains(&name)
        })
        .collect();
    if folds.len() != 2 {
        return None;
    }
    let maps: Vec<_> = functions
        .iter()
        .copied()
        .filter(|fd| {
            induction::list_measure(fd, ctx).is_some() && fd.return_type.starts_with("List<")
        })
        .collect();
    if maps.is_empty() {
        return None;
    }
    // A finite helper can inspect another mapped cell before the recursive
    // fold resumes. Relate that cell's tail to a slice of the original input
    // instead of treating the two tails as unrelated induction arguments.
    // These are local checked lemmas: a non-map list function simply fails
    // this candidate, rather than receiving a shape-based theorem.
    let map_lemmas = induction::checked_map_lemmas(
        &maps
            .iter()
            .filter(|fd| induction::is_unary_list_map(fd, ctx))
            .map(|fd| induction::lean_name(fd, ctx))
            .collect::<Vec<_>>(),
    );
    // Calls producing a fold's state stay opaque. Expanding their patterns is
    // unrelated to transporting that fold's observations across a list map.
    let boundary: BTreeSet<_> = folds
        .iter()
        .flat_map(|fd| fd.params.iter().map(|(_, ty)| ty.as_str()))
        .filter(|ty| !ty.starts_with("List<") && !["Int", "Bool", "String"].contains(ty))
        .collect();
    let plain: BTreeSet<_> = functions
        .iter()
        .copied()
        .filter(|fd| {
            fd.effects.is_empty()
                // Normalize the compared interfaces; a deeper import is a
                // shared computation whose result should remain opaque.
                && folds.iter().any(|fold| common::fn_owning_scope_for(ctx, fold) == common::fn_owning_scope_for(ctx, fd))
                && !boundary.contains(fd.return_type.as_str())
                && common::fn_id_for_decl(ctx, fd)
                    .is_some_and(|id| !ctx.recursive_fns.contains(&id))
        })
        .map(|fd| induction::lean_name(fd, ctx))
        .collect();
    let mut converters = BTreeSet::new();
    for fd in &maps {
        let owner = common::fn_owning_scope_for(ctx, fd);
        for stmt in fd.body.stmts() {
            let (Stmt::Expr(expr) | Stmt::Binding(_, _, expr)) = stmt;
            crate::codegen::expr_walk::walk(expr, &mut |expr| {
                if let Some(callee) = induction::callee(expr, ctx, owner) {
                    let name = induction::lean_name(callee, ctx);
                    if plain.contains(&name) {
                        converters.insert(name);
                    }
                }
            });
        }
    }
    // Expose finite computations performed by a fold before splitting their
    // returned observations. Result adapters around the complete fold stay
    // opaque until the two steps agree.
    let mut step_helpers = BTreeSet::new();
    let mut pending = folds.clone();
    while let Some(fd) = pending.pop() {
        let owner = common::fn_owning_scope_for(ctx, fd);
        for stmt in fd.body.stmts() {
            let (Stmt::Expr(expr) | Stmt::Binding(_, _, expr)) = stmt;
            crate::codegen::expr_walk::walk(expr, &mut |expr| {
                if let Some(callee) = induction::callee(expr, ctx, owner) {
                    let name = induction::lean_name(callee, ctx);
                    if plain.contains(&name) && step_helpers.insert(name) {
                        pending.push(callee);
                    }
                }
            });
        }
    }
    let excluded = (0..fact_count)
        .map(|i| format!("-_fact{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let plain = plain.into_iter().collect::<Vec<_>>().join(", ");
    let mapping = maps
        .iter()
        .map(|fd| induction::lean_name(fd, ctx))
        .chain(converters.iter().cloned())
        .collect::<Vec<_>>()
        .join(", ");
    let step_simp = std::iter::once(mapping.clone())
        .chain(step_helpers)
        .collect::<Vec<_>>()
        .join(", ");
    let equations = converters
        .iter()
        .map(|name| format!("= {name}.eq_def"))
        .chain(
            maps.iter()
                .filter_map(|fd| induction::map_constructor_equations(fd, ctx)),
        )
        .collect::<Vec<_>>()
        .join(", ");
    let steps = folds
        .iter()
        .map(|fd| {
            format!(
                "rw [{}.eq_def]; simp only [{mapping}]",
                induction::lean_name(fd, ctx)
            )
        })
        .collect::<Vec<_>>()
        .join("; ");
    let others = law
        .givens
        .iter()
        .filter(|g| g.name != *input)
        .map(|g| aver_name_to_lean(&g.name))
        .collect::<Vec<_>>()
        .join(" ");
    let generalizing = if others.is_empty() {
        String::new()
    } else {
        format!(" generalizing {others}")
    };
    let input = aver_name_to_lean(input);
    // A fold may continue on a drop of the current tail after observing a
    // finite helper. A cons-tail IH is too narrow for that checked decrease;
    // length induction provides the equation for every shorter suffix.
    let steps = format!(
        "all_goals ({steps}); all_goals (repeat' first | (simp_all +zetaDelta [{step_simp}, {excluded}]) | split); all_goals (simp_all +zetaDelta [{plain}, {mapping}, List.append_assoc, {excluded}]); all_goals ({map_lemmas}grind [List.drop_cons, List.drop_drop, List.length_drop, List.length_cons, {equations}]); done"
    );
    let induction = if crate::codegen::recursion::detect::single_list_structural_param_index(driver)
        .is_some()
    {
        format!("induction {input}{generalizing}; {steps}")
    } else {
        format!(
            "induction {input} using (measure List.length).wf.induction{generalizing} with | h {input} _aver_transport_ih => (dsimp only [WellFoundedRelation.rel, measure, invImage, InvImage, Nat.lt_wfRel] at *; cases {input}; {steps})"
        )
    };
    let normalize = (0..fact_count).map(|i| format!("(try simp only [{plain}, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at _fact{i}); ")).collect::<String>();
    Some(format!(
        "({normalize}simp only [beq_iff_eq, {}, {}]; {induction})",
        induction::lean_name(left_fn, ctx),
        induction::lean_name(right_fn, ctx),
    ))
}
