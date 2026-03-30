use std::collections::BTreeSet;

use super::super::expr::aver_name_to_lean;
use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

pub(super) fn body_terminal_expr(body: &FnBody) -> Option<&Spanned<Expr>> {
    match body.stmts() {
        [Stmt::Expr(expr)] => Some(expr),
        _ => None,
    }
}

pub(super) fn substitute_expr(
    expr: &Spanned<Expr>,
    bindings: &std::collections::HashMap<&str, &Spanned<Expr>>,
) -> Spanned<Expr> {
    let line = expr.line;
    let new_node = match &expr.node {
        Expr::Literal(lit) => Expr::Literal(lit.clone()),
        Expr::Ident(name) => {
            return bindings.get(name.as_str()).map_or_else(
                || Spanned::new(Expr::Ident(name.clone()), line),
                |bound| (*bound).clone(),
            );
        }
        Expr::Attr(base, field) => {
            Expr::Attr(Box::new(substitute_expr(base, bindings)), field.clone())
        }
        Expr::FnCall(callee, args) => Expr::FnCall(
            Box::new(substitute_expr(callee, bindings)),
            args.iter()
                .map(|arg| substitute_expr(arg, bindings))
                .collect(),
        ),
        Expr::BinOp(op, left, right) => Expr::BinOp(
            *op,
            Box::new(substitute_expr(left, bindings)),
            Box::new(substitute_expr(right, bindings)),
        ),
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(substitute_expr(subject, bindings)),
            arms: arms
                .iter()
                .map(|arm| crate::ast::MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(substitute_expr(&arm.body, bindings)),
                })
                .collect(),
        },
        Expr::Constructor(name, inner) => Expr::Constructor(
            name.clone(),
            inner
                .as_ref()
                .map(|expr| Box::new(substitute_expr(expr, bindings))),
        ),
        Expr::ErrorProp(inner) => Expr::ErrorProp(Box::new(substitute_expr(inner, bindings))),
        Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
            parts
                .iter()
                .map(|part| match part {
                    crate::ast::StrPart::Literal(s) => crate::ast::StrPart::Literal(s.clone()),
                    crate::ast::StrPart::Parsed(expr) => {
                        crate::ast::StrPart::Parsed(Box::new(substitute_expr(expr, bindings)))
                    }
                })
                .collect(),
        ),
        Expr::List(items) => Expr::List(
            items
                .iter()
                .map(|item| substitute_expr(item, bindings))
                .collect(),
        ),
        Expr::Tuple(items) => Expr::Tuple(
            items
                .iter()
                .map(|item| substitute_expr(item, bindings))
                .collect(),
        ),
        Expr::MapLiteral(entries) => Expr::MapLiteral(
            entries
                .iter()
                .map(|(key, value)| {
                    (
                        substitute_expr(key, bindings),
                        substitute_expr(value, bindings),
                    )
                })
                .collect(),
        ),
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| (name.clone(), substitute_expr(value, bindings)))
                .collect(),
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(substitute_expr(base, bindings)),
            updates: updates
                .iter()
                .map(|(name, value)| (name.clone(), substitute_expr(value, bindings)))
                .collect(),
        },
        Expr::TailCall(call) => Expr::TailCall(Box::new((
            call.0.clone(),
            call.1
                .iter()
                .map(|arg| substitute_expr(arg, bindings))
                .collect(),
        ))),
        Expr::Resolved(slot) => Expr::Resolved(*slot),
    };
    Spanned::new(new_node, line)
}

pub(super) fn law_simp_defs(
    ctx: &CodegenContext,
    vb: &VerifyBlock,
    law: &VerifyLaw,
) -> BTreeSet<String> {
    law_simp_source_names(ctx, vb, law)
        .into_iter()
        .map(|name| aver_name_to_lean(&name))
        .collect()
}

fn law_simp_source_names(
    ctx: &CodegenContext,
    vb: &VerifyBlock,
    law: &VerifyLaw,
) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    names.insert(vb.fn_name.clone());
    collect_user_fn_simp_names(&law.lhs, ctx, &vb.fn_name, &mut names);
    collect_user_fn_simp_names(&law.rhs, ctx, &vb.fn_name, &mut names);
    if let Some(when_expr) = &law.when {
        collect_user_fn_simp_names(when_expr, ctx, &vb.fn_name, &mut names);
    }
    expand_pure_fn_simp_names(ctx, &vb.fn_name, &mut names);
    names
}

fn expand_pure_fn_simp_names(ctx: &CodegenContext, skip_fn: &str, out: &mut BTreeSet<String>) {
    loop {
        let before = out.len();
        let current = out.iter().cloned().collect::<Vec<_>>();
        for name in current {
            let Some(fd) = find_fn_def(ctx, &name) else {
                continue;
            };
            if !fd.effects.is_empty() || fd.name == "main" {
                continue;
            }
            for stmt in fd.body.stmts() {
                match stmt {
                    Stmt::Expr(expr) | Stmt::Binding(_, _, expr) => {
                        collect_user_fn_simp_names(expr, ctx, skip_fn, out);
                    }
                }
            }
        }
        if out.len() == before {
            return;
        }
    }
}

fn collect_user_fn_simp_names(
    expr: &Spanned<Expr>,
    ctx: &CodegenContext,
    skip_fn: &str,
    out: &mut BTreeSet<String>,
) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_dotted_name(callee)
                && let Some(fd) = find_fn_def_by_call_name(ctx, &name)
                && fd.effects.is_empty()
                && fd.name != "main"
                && fd.name != skip_fn
            {
                out.insert(fd.name.clone());
            }
            collect_user_fn_simp_names(callee, ctx, skip_fn, out);
            for arg in args {
                collect_user_fn_simp_names(arg, ctx, skip_fn, out);
            }
        }
        Expr::Attr(base, _) => collect_user_fn_simp_names(base, ctx, skip_fn, out),
        Expr::BinOp(_, l, r) => {
            collect_user_fn_simp_names(l, ctx, skip_fn, out);
            collect_user_fn_simp_names(r, ctx, skip_fn, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_user_fn_simp_names(subject, ctx, skip_fn, out);
            for arm in arms {
                collect_user_fn_simp_names(&arm.body, ctx, skip_fn, out);
            }
        }
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner {
                collect_user_fn_simp_names(inner, ctx, skip_fn, out);
            }
        }
        Expr::ErrorProp(inner) => collect_user_fn_simp_names(inner, ctx, skip_fn, out),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    collect_user_fn_simp_names(inner, ctx, skip_fn, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_user_fn_simp_names(item, ctx, skip_fn, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_user_fn_simp_names(k, ctx, skip_fn, out);
                collect_user_fn_simp_names(v, ctx, skip_fn, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_user_fn_simp_names(v, ctx, skip_fn, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_user_fn_simp_names(base, ctx, skip_fn, out);
            for (_, v) in updates {
                collect_user_fn_simp_names(v, ctx, skip_fn, out);
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
                collect_user_fn_simp_names(arg, ctx, skip_fn, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => {}
    }
}

pub(super) fn find_fn_def<'a>(ctx: &'a CodegenContext, fn_name: &str) -> Option<&'a FnDef> {
    ctx.modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .find(|fd| fd.name == fn_name)
}

pub(super) fn find_fn_def_by_call_name<'a>(
    ctx: &'a CodegenContext,
    call_name: &str,
) -> Option<&'a FnDef> {
    find_fn_def(ctx, call_name).or_else(|| {
        let short = call_name.rsplit('.').next()?;
        find_fn_def(ctx, short)
    })
}

pub(super) fn expr_dotted_name(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(base, field) => expr_dotted_name(base).map(|p| format!("{p}.{field}")),
        _ => None,
    }
}

pub(super) fn matches_ident(expr: &Spanned<Expr>, name: &str) -> bool {
    matches!(&expr.node, Expr::Ident(n) if n == name)
}

pub(super) fn callee_matches_name(expr: &Spanned<Expr>, target: &str) -> bool {
    let Some(name) = expr_dotted_name(expr) else {
        return false;
    };
    name == target || name.rsplit('.').next() == Some(target)
}

pub(super) fn call2_args<'a>(
    expr: &'a Spanned<Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>)> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    if args.len() != 2 || !callee_matches_name(callee, fn_name) {
        return None;
    }
    Some((&args[0], &args[1]))
}

pub(super) fn call_named_args<'a>(
    expr: &'a Spanned<Expr>,
    full_name: &str,
) -> Option<&'a [Spanned<Expr>]> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let callee_name = expr_dotted_name(callee)?;
    if callee_name == full_name {
        Some(args.as_slice())
    } else {
        None
    }
}

pub(super) fn matches_binary_call(expr: &Spanned<Expr>, fn_name: &str, a: &str, b: &str) -> bool {
    let Some((x, y)) = call2_args(expr, fn_name) else {
        return false;
    };
    matches_ident(x, a) && matches_ident(y, b)
}

pub(super) fn matches_unary_call(expr: &Spanned<Expr>, fn_name: &str, arg: &str) -> bool {
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    args.len() == 1 && callee_matches_name(callee, fn_name) && matches_ident(&args[0], arg)
}

pub(super) fn binary_call_var_const(
    expr: &Spanned<Expr>,
    var_name: &str,
) -> Option<(String, bool, i64)> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    if args.len() != 2 {
        return None;
    }
    let callee_name = expr_dotted_name(callee)?;
    match (&args[0].node, &args[1].node) {
        (Expr::Ident(v), Expr::Literal(Literal::Int(n))) if v == var_name => {
            Some((callee_name, true, *n))
        }
        (Expr::Literal(Literal::Int(n)), Expr::Ident(v)) if v == var_name => {
            Some((callee_name, false, *n))
        }
        _ => None,
    }
}

pub(super) fn matches_assoc_nested(
    expr: &Spanned<Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
    c: &str,
) -> bool {
    let Some((ab, z)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((x, y)) = call2_args(ab, fn_name) else {
        return false;
    };
    matches_ident(x, a) && matches_ident(y, b) && matches_ident(z, c)
}

pub(super) fn matches_assoc_flat(
    expr: &Spanned<Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
    c: &str,
) -> bool {
    let Some((x, bc)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((y, z)) = call2_args(bc, fn_name) else {
        return false;
    };
    matches_ident(x, a) && matches_ident(y, b) && matches_ident(z, c)
}

pub(super) fn matches_identity_side(
    call_side: &Spanned<Expr>,
    ident_side: &Spanned<Expr>,
    fn_name: &str,
    given_name: &str,
    identity: i64,
) -> bool {
    if !matches_ident(ident_side, given_name) {
        return false;
    }
    let Some((x, y)) = call2_args(call_side, fn_name) else {
        return false;
    };
    (matches_ident(x, given_name) && matches_int_lit(y, identity))
        || (matches_int_lit(x, identity) && matches_ident(y, given_name))
}

pub(super) fn matches_sub_right_identity_side(
    call_side: &Spanned<Expr>,
    ident_side: &Spanned<Expr>,
    fn_name: &str,
    given_name: &str,
) -> bool {
    if !matches_ident(ident_side, given_name) {
        return false;
    }
    let Some((x, y)) = call2_args(call_side, fn_name) else {
        return false;
    };
    matches_ident(x, given_name) && matches_int_lit(y, 0)
}

pub(super) fn matches_neg_binary_call(
    expr: &Spanned<Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
) -> bool {
    match &expr.node {
        Expr::BinOp(BinOp::Sub, left, right) => {
            matches_int_lit(left, 0) && matches_binary_call(right, fn_name, a, b)
        }
        _ => false,
    }
}

pub(super) fn matches_int_lit(expr: &Spanned<Expr>, expected: i64) -> bool {
    matches!(&expr.node, Expr::Literal(Literal::Int(n)) if *n == expected)
}

pub(super) fn matches_bool_true(expr: &Spanned<Expr>) -> bool {
    matches!(&expr.node, Expr::Literal(Literal::Bool(true)))
}

pub(super) fn map_has_set_parts(
    expr: &Spanned<Expr>,
) -> Option<(&Spanned<Expr>, &Spanned<Expr>, &Spanned<Expr>)> {
    let has_args = call_named_args(expr, "Map.has")?;
    if has_args.len() != 2 {
        return None;
    }
    let set_args = call_named_args(&has_args[0], "Map.set")?;
    if set_args.len() != 3 {
        return None;
    }
    if set_args[1] != has_args[1] {
        return None;
    }
    Some((&set_args[0], &set_args[1], &set_args[2]))
}

pub(super) fn map_get_set_parts(
    expr: &Spanned<Expr>,
) -> Option<(&Spanned<Expr>, &Spanned<Expr>, &Spanned<Expr>)> {
    let get_args = call_named_args(expr, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    let set_args = call_named_args(&get_args[0], "Map.set")?;
    if set_args.len() != 3 {
        return None;
    }
    if set_args[1] != get_args[1] {
        return None;
    }
    Some((&set_args[0], &set_args[1], &set_args[2]))
}

pub(super) fn option_some_arg(expr: &Spanned<Expr>) -> Option<&Spanned<Expr>> {
    let args = call_named_args(expr, "Option.Some")?;
    (args.len() == 1).then_some(&args[0])
}

pub(super) fn map_has_after_fn_call<'a>(
    expr: &'a Spanned<Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>)> {
    let has_args = call_named_args(expr, "Map.has")?;
    if has_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &has_args[0].node else {
        return None;
    };
    if fn_args.len() != 2 || !matches_ident(callee, fn_name) || fn_args[1] != has_args[1] {
        return None;
    }
    Some((&fn_args[0], &fn_args[1]))
}

pub(super) fn map_get_after_fn_call<'a>(
    expr: &'a Spanned<Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>)> {
    let get_args = call_named_args(expr, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &get_args[0].node else {
        return None;
    };
    if fn_args.len() != 2 || !matches_ident(callee, fn_name) || fn_args[1] != get_args[1] {
        return None;
    }
    Some((&fn_args[0], &fn_args[1]))
}

pub(super) fn option_with_default_args(
    expr: &Spanned<Expr>,
) -> Option<(&Spanned<Expr>, &Spanned<Expr>)> {
    let args = call_named_args(expr, "Option.withDefault")?;
    (args.len() == 2).then_some((&args[0], &args[1]))
}

pub(super) fn defaulted_map_get(
    expr: &Spanned<Expr>,
) -> Option<(&Spanned<Expr>, &Spanned<Expr>, &Spanned<Expr>)> {
    let (inner, default) = option_with_default_args(expr)?;
    let get_args = call_named_args(inner, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    Some((&get_args[0], &get_args[1], default))
}

pub(super) fn defaulted_map_get_after_fn_call<'a>(
    expr: &'a Spanned<Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>, &'a Spanned<Expr>)> {
    let (inner, default) = option_with_default_args(expr)?;
    let (map_arg, key_arg) = map_get_after_fn_call(inner, fn_name)?;
    Some((map_arg, key_arg, default))
}

pub(super) fn is_map_get_call(expr: &Spanned<Expr>, map_param: &str, key_param: &str) -> bool {
    let Some(args) = call_named_args(expr, "Map.get") else {
        return false;
    };
    args.len() == 2 && matches_ident(&args[0], map_param) && matches_ident(&args[1], key_param)
}

pub(super) fn is_map_set_call(expr: &Spanned<Expr>, map_param: &str, key_param: &str) -> bool {
    let Some(args) = call_named_args(expr, "Map.set") else {
        return false;
    };
    args.len() == 3 && matches_ident(&args[0], map_param) && matches_ident(&args[1], key_param)
}

pub(super) fn atom(s: &str) -> String {
    if s.contains(' ') && !s.starts_with('(') {
        format!("({s})")
    } else {
        s.to_string()
    }
}
