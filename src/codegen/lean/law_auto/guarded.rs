use std::collections::BTreeSet;

use super::super::expr::{aver_name_to_lean, emit_expr};
use super::AutoProof;
use super::shared::{expr_dotted_name, find_fn_def, find_fn_def_by_call_name, matches_ident};
use crate::ast::{Expr, Stmt, TypeDef, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

enum GuardedStrategy {
    Cases,
    ListInduction,
}

pub(super) fn emit_guarded_universal_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let when_expr = law.when.as_ref()?;
    if law.givens.len() != 1 || intro_names.len() != 1 {
        return None;
    }

    let given = &law.givens[0];
    let target_lean = &intro_names[0];
    let strategy = guarded_strategy(ctx, &given.type_name)?;
    let (parser_name, serializer_name) = detect_roundtrip_layers(ctx, law, &given.name)?;
    let simp_list = guarded_roundtrip_simp_defs(ctx, vb, law, &parser_name, &serializer_name)
        .into_iter()
        .map(|name| aver_name_to_lean(&name))
        .collect::<Vec<_>>()
        .join(", ");

    let theorem_prop = format!(
        "{} = true -> {} = {}",
        emit_expr(when_expr, ctx),
        emit_expr(&law.lhs, ctx),
        emit_expr(&law.rhs, ctx)
    );

    let mut support_lines = Vec::new();
    support_lines.push(format!(
        "theorem {} : ∀ {}, {} := by",
        theorem_base, quant_params, theorem_prop
    ));
    support_lines.push(format!("  intro {} h_when", target_lean));
    match strategy {
        GuardedStrategy::Cases => {
            support_lines.push(format!(
                "  cases {} <;> simp [{}] at h_when ⊢",
                target_lean, simp_list
            ));
        }
        GuardedStrategy::ListInduction => {
            support_lines.push(format!("  induction {} with", target_lean));
            support_lines.push(format!("  | nil => simp [{}] at h_when ⊢", simp_list));
            support_lines.push(format!(
                "  | cons head tail ih => simp [{}] at h_when ⊢",
                simp_list
            ));
        }
    }

    Some(AutoProof {
        support_lines,
        proof_lines: Vec::new(),
        replaces_theorem: true,
    })
}

fn guarded_strategy(ctx: &CodegenContext, type_name: &str) -> Option<GuardedStrategy> {
    if is_list_type(type_name) {
        return Some(GuardedStrategy::ListInduction);
    }
    if has_named_user_type(ctx, type_name) {
        return Some(GuardedStrategy::Cases);
    }
    None
}

fn detect_roundtrip_layers(
    ctx: &CodegenContext,
    law: &VerifyLaw,
    given_name: &str,
) -> Option<(String, String)> {
    let Expr::FnCall(parser_callee, parser_args) = &law.lhs else {
        return None;
    };
    if parser_args.len() != 1 {
        return None;
    }
    let Expr::FnCall(serializer_callee, serializer_args) = &parser_args[0] else {
        return None;
    };
    if serializer_args.len() != 1 || !matches_ident(&serializer_args[0], given_name) {
        return None;
    }

    let parser_call = expr_dotted_name(parser_callee)?;
    let serializer_call = expr_dotted_name(serializer_callee)?;
    let parser_name = find_fn_def_by_call_name(ctx, &parser_call)?.name.clone();
    let serializer_name = find_fn_def_by_call_name(ctx, &serializer_call)?
        .name
        .clone();
    Some((parser_name, serializer_name))
}

fn guarded_roundtrip_simp_defs(
    ctx: &CodegenContext,
    vb: &VerifyBlock,
    law: &VerifyLaw,
    parser_name: &str,
    serializer_name: &str,
) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    names.insert(parser_name.to_string());
    names.insert(serializer_name.to_string());
    expand_pure_fn_names(ctx, &vb.fn_name, serializer_name, 2, &mut names);
    if let Some(when_expr) = &law.when {
        let mut frontier = BTreeSet::new();
        collect_direct_pure_user_fn_names(when_expr, ctx, &vb.fn_name, &mut frontier);
        for name in frontier.clone() {
            names.insert(name.clone());
            expand_pure_fn_names(ctx, &vb.fn_name, &name, 2, &mut names);
        }
    }
    names
}

fn expand_pure_fn_names(
    ctx: &CodegenContext,
    skip_fn: &str,
    root_name: &str,
    depth: usize,
    out: &mut BTreeSet<String>,
) {
    let mut frontier = vec![(root_name.to_string(), depth)];
    let mut visited = BTreeSet::new();
    while let Some((name, remaining)) = frontier.pop() {
        if !visited.insert((name.clone(), remaining)) || remaining == 0 {
            continue;
        }
        let Some(fd) = find_fn_def(ctx, &name) else {
            continue;
        };
        if !fd.effects.is_empty() || fd.name == "main" {
            continue;
        }
        let mut direct = BTreeSet::new();
        for stmt in fd.body.stmts() {
            match stmt {
                Stmt::Expr(expr) | Stmt::Binding(_, _, expr) => {
                    collect_direct_pure_user_fn_names(expr, ctx, skip_fn, &mut direct);
                }
            }
        }
        for child in direct {
            if out.insert(child.clone()) {
                frontier.push((child, remaining - 1));
            } else {
                frontier.push((child, remaining - 1));
            }
        }
    }
}

fn collect_direct_pure_user_fn_names(
    expr: &Expr,
    ctx: &CodegenContext,
    skip_fn: &str,
    out: &mut BTreeSet<String>,
) {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_dotted_name(callee)
                && let Some(fd) = find_fn_def_by_call_name(ctx, &name)
                && fd.effects.is_empty()
                && fd.name != "main"
                && fd.name != skip_fn
            {
                out.insert(fd.name.clone());
            }
            collect_direct_pure_user_fn_names(callee, ctx, skip_fn, out);
            for arg in args {
                collect_direct_pure_user_fn_names(arg, ctx, skip_fn, out);
            }
        }
        Expr::Attr(base, _) => collect_direct_pure_user_fn_names(base, ctx, skip_fn, out),
        Expr::BinOp(_, left, right) => {
            collect_direct_pure_user_fn_names(left, ctx, skip_fn, out);
            collect_direct_pure_user_fn_names(right, ctx, skip_fn, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_direct_pure_user_fn_names(subject, ctx, skip_fn, out);
            for arm in arms {
                collect_direct_pure_user_fn_names(&arm.body, ctx, skip_fn, out);
            }
        }
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner {
                collect_direct_pure_user_fn_names(inner, ctx, skip_fn, out);
            }
        }
        Expr::ErrorProp(inner) => collect_direct_pure_user_fn_names(inner, ctx, skip_fn, out),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    collect_direct_pure_user_fn_names(inner, ctx, skip_fn, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_direct_pure_user_fn_names(item, ctx, skip_fn, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_direct_pure_user_fn_names(key, ctx, skip_fn, out);
                collect_direct_pure_user_fn_names(value, ctx, skip_fn, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_direct_pure_user_fn_names(value, ctx, skip_fn, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_direct_pure_user_fn_names(base, ctx, skip_fn, out);
            for (_, value) in updates {
                collect_direct_pure_user_fn_names(value, ctx, skip_fn, out);
            }
        }
        Expr::TailCall(call) => {
            if let Some(fd) = find_fn_def_by_call_name(ctx, &call.0)
                && fd.effects.is_empty()
                && fd.name != "main"
                && fd.name != skip_fn
            {
                out.insert(fd.name.clone());
            }
            for arg in &call.1 {
                collect_direct_pure_user_fn_names(arg, ctx, skip_fn, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => {}
    }
}

fn is_list_type(type_name: &str) -> bool {
    let type_name = type_name.trim();
    type_name.starts_with("List<") && type_name.ends_with('>')
}

fn has_named_user_type(ctx: &CodegenContext, type_name: &str) -> bool {
    ctx.modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .any(|td| match td {
            TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name == type_name,
        })
}
