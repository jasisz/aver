use std::collections::BTreeSet;

use super::super::expr::aver_name_to_lean;
use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

pub(super) fn body_terminal_expr(body: &FnBody) -> Option<&Expr> {
    match body.stmts() {
        [Stmt::Expr(expr)] => Some(expr),
        _ => None,
    }
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

pub(super) fn expr_dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(base, field) => expr_dotted_name(base).map(|p| format!("{p}.{field}")),
        _ => None,
    }
}

pub(super) fn matches_ident(expr: &Expr, name: &str) -> bool {
    matches!(expr, Expr::Ident(n) if n == name)
}

pub(super) fn callee_matches_name(expr: &Expr, target: &str) -> bool {
    let Some(name) = expr_dotted_name(expr) else {
        return false;
    };
    name == target || name.rsplit('.').next() == Some(target)
}

pub(super) fn call2_args<'a>(expr: &'a Expr, fn_name: &str) -> Option<(&'a Expr, &'a Expr)> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    if args.len() != 2 || !callee_matches_name(callee, fn_name) {
        return None;
    }
    Some((&args[0], &args[1]))
}

pub(super) fn call_named_args<'a>(expr: &'a Expr, full_name: &str) -> Option<&'a [Expr]> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    let callee_name = expr_dotted_name(callee)?;
    if callee_name == full_name {
        Some(args.as_slice())
    } else {
        None
    }
}

pub(super) fn matches_binary_call(expr: &Expr, fn_name: &str, a: &str, b: &str) -> bool {
    let Some((x, y)) = call2_args(expr, fn_name) else {
        return false;
    };
    matches_ident(x, a) && matches_ident(y, b)
}

pub(super) fn matches_unary_call(expr: &Expr, fn_name: &str, arg: &str) -> bool {
    let Expr::FnCall(callee, args) = expr else {
        return false;
    };
    args.len() == 1 && callee_matches_name(callee, fn_name) && matches_ident(&args[0], arg)
}

pub(super) fn binary_call_var_const(expr: &Expr, var_name: &str) -> Option<(String, bool, i64)> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    if args.len() != 2 {
        return None;
    }
    let callee_name = expr_dotted_name(callee)?;
    match (&args[0], &args[1]) {
        (Expr::Ident(v), Expr::Literal(Literal::Int(n))) if v == var_name => {
            Some((callee_name, true, *n))
        }
        (Expr::Literal(Literal::Int(n)), Expr::Ident(v)) if v == var_name => {
            Some((callee_name, false, *n))
        }
        _ => None,
    }
}

pub(super) fn matches_assoc_nested(expr: &Expr, fn_name: &str, a: &str, b: &str, c: &str) -> bool {
    let Some((ab, z)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((x, y)) = call2_args(ab, fn_name) else {
        return false;
    };
    matches_ident(x, a) && matches_ident(y, b) && matches_ident(z, c)
}

pub(super) fn matches_assoc_flat(expr: &Expr, fn_name: &str, a: &str, b: &str, c: &str) -> bool {
    let Some((x, bc)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((y, z)) = call2_args(bc, fn_name) else {
        return false;
    };
    matches_ident(x, a) && matches_ident(y, b) && matches_ident(z, c)
}

pub(super) fn matches_identity_side(
    call_side: &Expr,
    ident_side: &Expr,
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
    call_side: &Expr,
    ident_side: &Expr,
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

pub(super) fn matches_neg_binary_call(expr: &Expr, fn_name: &str, a: &str, b: &str) -> bool {
    match expr {
        Expr::BinOp(BinOp::Sub, left, right) => {
            matches_int_lit(left, 0) && matches_binary_call(right, fn_name, a, b)
        }
        _ => false,
    }
}

pub(super) fn matches_int_lit(expr: &Expr, expected: i64) -> bool {
    matches!(expr, Expr::Literal(Literal::Int(n)) if *n == expected)
}

pub(super) fn matches_bool_true(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(Literal::Bool(true)))
}

pub(super) fn map_has_set_parts(expr: &Expr) -> Option<(&Expr, &Expr, &Expr)> {
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

pub(super) fn map_get_set_parts(expr: &Expr) -> Option<(&Expr, &Expr, &Expr)> {
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

pub(super) fn option_some_arg(expr: &Expr) -> Option<&Expr> {
    let args = call_named_args(expr, "Option.Some")?;
    (args.len() == 1).then_some(&args[0])
}

pub(super) fn map_has_after_fn_call<'a>(
    expr: &'a Expr,
    fn_name: &str,
) -> Option<(&'a Expr, &'a Expr)> {
    let has_args = call_named_args(expr, "Map.has")?;
    if has_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &has_args[0] else {
        return None;
    };
    if fn_args.len() != 2 || !matches_ident(callee, fn_name) || fn_args[1] != has_args[1] {
        return None;
    }
    Some((&fn_args[0], &fn_args[1]))
}

pub(super) fn map_get_after_fn_call<'a>(
    expr: &'a Expr,
    fn_name: &str,
) -> Option<(&'a Expr, &'a Expr)> {
    let get_args = call_named_args(expr, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &get_args[0] else {
        return None;
    };
    if fn_args.len() != 2 || !matches_ident(callee, fn_name) || fn_args[1] != get_args[1] {
        return None;
    }
    Some((&fn_args[0], &fn_args[1]))
}

pub(super) fn map_has_after_agg_call<'a>(
    expr: &'a Expr,
    fn_name: &str,
) -> Option<(&'a Expr, &'a Expr)> {
    let has_args = call_named_args(expr, "Map.has")?;
    if has_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &has_args[0] else {
        return None;
    };
    if fn_args.len() != 1 || !callee_matches_name(callee, fn_name) {
        return None;
    }
    Some((&fn_args[0], &has_args[1]))
}

pub(super) fn option_with_default_args(expr: &Expr) -> Option<(&Expr, &Expr)> {
    let args = call_named_args(expr, "Option.withDefault")?;
    (args.len() == 2).then_some((&args[0], &args[1]))
}

pub(super) fn defaulted_map_get(expr: &Expr) -> Option<(&Expr, &Expr, &Expr)> {
    let (inner, default) = option_with_default_args(expr)?;
    let get_args = call_named_args(inner, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    Some((&get_args[0], &get_args[1], default))
}

pub(super) fn defaulted_map_get_after_fn_call<'a>(
    expr: &'a Expr,
    fn_name: &str,
) -> Option<(&'a Expr, &'a Expr, &'a Expr)> {
    let (inner, default) = option_with_default_args(expr)?;
    let (map_arg, key_arg) = map_get_after_fn_call(inner, fn_name)?;
    Some((map_arg, key_arg, default))
}

pub(super) fn defaulted_map_get_after_agg_call<'a>(
    expr: &'a Expr,
    fn_name: &str,
) -> Option<(&'a Expr, &'a Expr, &'a Expr)> {
    let (inner, default) = option_with_default_args(expr)?;
    let get_args = call_named_args(inner, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &get_args[0] else {
        return None;
    };
    if fn_args.len() != 1 || !callee_matches_name(callee, fn_name) {
        return None;
    }
    Some((&fn_args[0], &get_args[1], default))
}

pub(super) fn ident_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(name) => Some(name.as_str()),
        _ => None,
    }
}

pub(super) fn matches_list_contains_call(expr: &Expr, list_name: &str, item_name: &str) -> bool {
    let Some(args) = call_named_args(expr, "List.contains") else {
        return false;
    };
    args.len() == 2 && matches_ident(&args[0], list_name) && matches_ident(&args[1], item_name)
}

pub(super) fn matches_recursive_self_call(expr: &Expr, fn_name: &str, arg_name: &str) -> bool {
    match expr {
        Expr::FnCall(callee, args) => {
            args.len() == 1
                && callee_matches_name(callee, fn_name)
                && matches_ident(&args[0], arg_name)
        }
        Expr::TailCall(call) => {
            call.0 == fn_name && call.1.len() == 1 && matches_ident(&call.1[0], arg_name)
        }
        _ => false,
    }
}

pub(super) fn matches_equality_pair(expr: &Expr, left_name: &str, right_name: &str) -> bool {
    match expr {
        Expr::BinOp(BinOp::Eq, left, right) => {
            (matches_ident(left, left_name) && matches_ident(right, right_name))
                || (matches_ident(left, right_name) && matches_ident(right, left_name))
        }
        _ => false,
    }
}

pub(super) fn matches_recursive_counter_step(
    expr: &Expr,
    fn_name: &str,
    tail_name: &str,
    tracked_name: &str,
    add_one: bool,
) -> bool {
    if add_one {
        let Expr::BinOp(BinOp::Add, left, right) = expr else {
            return false;
        };
        matches_recursive_counter_step(left, fn_name, tail_name, tracked_name, false)
            && matches_int_lit(right, 1)
    } else {
        match expr {
            Expr::FnCall(callee, args) => {
                args.len() == 2
                    && callee_matches_name(callee, fn_name)
                    && matches_ident(&args[0], tail_name)
                    && matches_ident(&args[1], tracked_name)
            }
            Expr::TailCall(call) => {
                call.0 == fn_name
                    && call.1.len() == 2
                    && matches_ident(&call.1[0], tail_name)
                    && matches_ident(&call.1[1], tracked_name)
            }
            _ => false,
        }
    }
}

pub(super) fn is_map_get_call(expr: &Expr, map_param: &str, key_param: &str) -> bool {
    let Some(args) = call_named_args(expr, "Map.get") else {
        return false;
    };
    args.len() == 2 && matches_ident(&args[0], map_param) && matches_ident(&args[1], key_param)
}

pub(super) fn is_map_set_call(expr: &Expr, map_param: &str, key_param: &str) -> bool {
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
