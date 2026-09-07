//! Functional induction for explanations with an existing checked recursion measure.
//! Recursion contracts and kernel-generated equations remain the source of truth.

use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::ast::{Expr, FnDef, Spanned, Stmt, VerifyLaw};
use crate::codegen::lean::expr::{aver_name_to_lean, emit_expr, resolve_rewrite_output};
use crate::codegen::{CodegenContext, common};

fn list_measure<'a>(fd: &FnDef, ctx: &'a CodegenContext) -> Option<&'a str> {
    match common::find_fn_contract_for_fn(ctx, fd).and_then(|c| c.recursion.as_ref()) {
        Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::SeqLenPlusOne { param },
        }) => Some(param),
        _ => None,
    }
}

pub(super) fn callee<'a>(
    expr: &Spanned<Expr>,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<&'a FnDef> {
    let (name, _) = super::super::shared::call_name_args(expr)?;
    let id = ctx.symbol_table.resolve_fn_id_in(&name, scope)?;
    let key = &ctx.symbol_table.fn_entry(id).key;
    ctx.fn_def_by_name(&key.name, key.scope_str())
}

fn lean_name(fd: &FnDef, ctx: &CodegenContext) -> String {
    match common::fn_owning_scope_for(ctx, fd) {
        Some(scope) => format!(
            "{}.{}",
            aver_name_to_lean(scope),
            aver_name_to_lean(&fd.name)
        ),
        None => super::super::shared::entry_qualified_lean_name(ctx, &fd.name),
    }
}

pub(super) fn plan(expr: &Spanned<Expr>, law: &VerifyLaw, ctx: &CodegenContext) -> Option<String> {
    let scope = ctx.active_module_scope();
    let fd = callee(expr, ctx, scope.as_deref())?;
    if !fd.effects.is_empty() {
        return None;
    }
    let measure = list_measure(fd, ctx);
    let native_integer = matches!(
        common::find_fn_contract_for_fn(ctx, fd).and_then(|c| c.recursion.as_ref()),
        Some(crate::ir::RecursionContract::WellFoundedToNat { .. })
    );
    if measure.is_none() && !native_integer {
        return None;
    }
    let Expr::FnCall(_, args) = &expr.node else {
        return None;
    };
    // Composite or repeated arguments would require retaining their equalities
    // while generalizing. Keep that outside this first structural rung.
    let mut seen = HashSet::new();
    for arg in args {
        let name = match &arg.node {
            Expr::Ident(name) | Expr::Resolved { name, .. } => name,
            _ => return None,
        };
        if !law.givens.iter().any(|g| g.name == *name) || !seen.insert(name) {
            return None;
        }
    }
    if native_integer {
        let call = emit_expr(&resolve_rewrite_output(expr, ctx, None), ctx);
        return Some(format!("fun_induction {call}"));
    }
    let measure = measure?;
    let position = fd.params.iter().position(|(name, _)| name == measure)?;
    let (Expr::Ident(argument) | Expr::Resolved { name: argument, .. }) = &args.get(position)?.node
    else {
        return None;
    };
    let others = law
        .givens
        .iter()
        .filter(|g| g.name != *argument)
        .map(|g| aver_name_to_lean(&g.name))
        .collect::<Vec<_>>();
    let generalizing = if others.is_empty() {
        String::new()
    } else {
        format!(" generalizing {}", others.join(" "))
    };
    let call = emit_expr(&resolve_rewrite_output(expr, ctx, None), ctx);
    // Lean can fail to construct a functional principle for a local match.
    // The checked length measure supplies an IH for every shorter list,
    // including computed slices; reduce its relation before solving leaves.
    Some(format!(
        "first | fun_induction {call} | (induction {} using (measure List.length).wf.induction{generalizing} <;> dsimp only [WellFoundedRelation.rel, measure, invImage, InvImage, Nat.lt_wfRel] at * <;> rw [{}.eq_def])",
        aver_name_to_lean(argument),
        lean_name(fd, ctx)
    ))
}

/// Walk only this law's calls, resolving every edge in its owner's scope.
/// Unsupported recursive functions stay opaque; no fuel equation is imported.
pub(super) struct Definitions {
    pub(super) heads: String,
    pub(super) simp: String,
    pub(super) grind: String,
    pub(super) unfold_once: Vec<(String, bool)>,
}

pub(super) fn definitions(law: &VerifyLaw, ctx: &CodegenContext) -> Definitions {
    fn visit(
        expr: &Spanned<Expr>,
        scope: Option<&str>,
        ctx: &CodegenContext,
        seen: &mut HashSet<crate::ir::FnId>,
        out: &mut BTreeMap<String, bool>,
        unfold_once: &mut Vec<String>,
    ) {
        if let Some(fd) = callee(expr, ctx, scope)
            && fd.effects.is_empty()
            && let Some(id) = common::fn_id_for_decl(ctx, fd)
            && seen.insert(id)
        {
            let recursive = ctx.recursive_fns.contains(&id);
            // Subtractive countdown equations expose fixed-width steps. Keep
            // floor-division recursion opaque: its equations recursively grow
            // the arithmetic search even when cited laws already summarize it.
            let subtractive = matches!(
                common::find_fn_contract_for_fn(ctx, fd).and_then(|c| c.recursion.as_ref()),
                Some(crate::ir::RecursionContract::WellFoundedToNat {
                    floor_div: None,
                    ..
                })
            );
            if matches!(
                common::find_fn_contract_for_fn(ctx, fd).and_then(|c| c.recursion.as_ref()),
                Some(crate::ir::RecursionContract::WellFoundedToNat {
                    floor_div: Some(_),
                    ..
                })
            ) {
                unfold_once.push(lean_name(fd, ctx));
            }
            if !recursive || list_measure(fd, ctx).is_some() || subtractive {
                out.insert(lean_name(fd, ctx), recursive);
            }
            let owner = common::fn_owning_scope_for(ctx, fd);
            for stmt in fd.body.stmts() {
                let (Stmt::Expr(body) | Stmt::Binding(_, _, body)) = stmt;
                visit(body, owner, ctx, seen, out, unfold_once);
            }
        }
        crate::codegen::expr_walk::for_each_child(expr, &mut |child| {
            visit(child, scope, ctx, seen, out, unfold_once)
        });
    }
    let scope = ctx.active_module_scope();
    let mut seen = HashSet::new();
    let mut out = BTreeMap::new();
    let mut unfold_once = Vec::new();
    for expr in law
        .because
        .iter()
        .chain([&law.lhs, &law.rhs])
        .chain(law.when.iter())
    {
        visit(
            expr,
            scope.as_deref(),
            ctx,
            &mut seen,
            &mut out,
            &mut unfold_once,
        );
    }
    // A mutual helper's original equation is available only when the same
    // checked measure that emits its native definition succeeds. Fuel remains opaque.
    common::route_pure_components_per_scope(
        ctx,
        |fd| fd.effects.is_empty(),
        |fns, _| {
            if fns.len() > 1
                && fns
                    .iter()
                    .any(|fd| common::fn_id_for_decl(ctx, fd).is_some_and(|id| seen.contains(&id)))
                && crate::codegen::lean::toplevel::emit_native_mutual_group(fns, ctx).is_some()
            {
                for fd in fns {
                    if common::fn_id_for_decl(ctx, fd).is_some_and(|id| seen.contains(&id)) {
                        out.insert(format!("= {}.eq_def", lean_name(fd, ctx)), true);
                    }
                }
            }
            Vec::new()
        },
    );
    // An induction hypothesis says reason(rest) = true. Its match equation
    // reveals the checked facts even when `rest` is not a known constructor.
    for reason in &law.because {
        if let Some(fd) = callee(reason, ctx, scope.as_deref())
            && list_measure(fd, ctx).is_some()
        {
            out.insert(format!("= {}.eq_def", lean_name(fd, ctx)), true);
        }
    }
    // Only outer calls: retain computations passed as arguments as opaque terms.
    let heads = law
        .because
        .iter()
        .chain([&law.lhs, &law.rhs])
        .filter_map(|expr| callee(expr, ctx, scope.as_deref()))
        .filter(|fd| {
            fd.effects.is_empty()
                && common::fn_id_for_decl(ctx, fd)
                    .is_some_and(|id| !ctx.recursive_fns.contains(&id))
        })
        .map(|fd| lean_name(fd, ctx))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
        .join(", ");
    Definitions {
        heads,
        unfold_once: unfold_once
            .into_iter()
            .map(|name| {
                let reason = law.because.iter().any(|expr| {
                    callee(expr, ctx, scope.as_deref()).is_some_and(|fd| lean_name(fd, ctx) == name)
                });
                (name, reason)
            })
            .collect(),
        simp: out
            .iter()
            .filter(|(_, recursive)| !**recursive)
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(", "),
        grind: out
            .into_iter()
            .map(|(name, recursive)| {
                if recursive {
                    name
                } else {
                    format!("= {name}.eq_def")
                }
            })
            .collect::<Vec<_>>()
            .join(", "),
    }
}
