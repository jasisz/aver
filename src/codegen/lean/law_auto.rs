/// Heuristics for auto-proving `verify law` theorems in Lean output.
///
/// This module is intentionally isolated from `toplevel.rs` so all heuristic
/// matching and proof-shape logic lives in one place.
use std::collections::BTreeSet;

use super::VerifyEmitMode;
use super::expr::{aver_name_to_lean, emit_expr};
use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

pub fn emit_verify_law_forall_auto_proof(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
) -> Option<Vec<String>> {
    if verify_mode != VerifyEmitMode::NativeDecide {
        return None;
    }

    let intro_names: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();

    if law.lhs == law.rhs {
        return Some(intro_then(&intro_names, vec!["rfl".to_string()]));
    }

    let op = int_binary_wrapper_op(ctx, &vb.fn_name);
    let fn_name = &vb.fn_name;
    let fn_lean = aver_name_to_lean(fn_name);

    // Law: f(a, b) = f(b, a)
    if let Some(op_lemma) = op.as_ref().and_then(comm_lemma_for_op)
        && law.givens.len() == 2
        && law.givens[0].type_name == "Int"
        && law.givens[1].type_name == "Int"
        && (matches_binary_call(&law.lhs, fn_name, &law.givens[0].name, &law.givens[1].name)
            && matches_binary_call(&law.rhs, fn_name, &law.givens[1].name, &law.givens[0].name)
            || matches_binary_call(&law.lhs, fn_name, &law.givens[1].name, &law.givens[0].name)
                && matches_binary_call(&law.rhs, fn_name, &law.givens[0].name, &law.givens[1].name))
    {
        return Some(intro_then(
            &intro_names,
            vec![format!("simp [{}, {}]", fn_lean, op_lemma)],
        ));
    }

    // Law: f(f(a,b),c) = f(a,f(b,c))
    if let Some(assoc_lemma) = op.as_ref().and_then(assoc_lemma_for_op)
        && law.givens.len() == 3
        && law.givens.iter().all(|g| g.type_name == "Int")
        && (matches_assoc_nested(
            &law.lhs,
            fn_name,
            &law.givens[0].name,
            &law.givens[1].name,
            &law.givens[2].name,
        ) && matches_assoc_flat(
            &law.rhs,
            fn_name,
            &law.givens[0].name,
            &law.givens[1].name,
            &law.givens[2].name,
        ) || matches_assoc_nested(
            &law.rhs,
            fn_name,
            &law.givens[0].name,
            &law.givens[1].name,
            &law.givens[2].name,
        ) && matches_assoc_flat(
            &law.lhs,
            fn_name,
            &law.givens[0].name,
            &law.givens[1].name,
            &law.givens[2].name,
        ))
    {
        return Some(intro_then(
            &intro_names,
            vec![format!("simp [{}, {}]", fn_lean, assoc_lemma)],
        ));
    }

    // Law: f(a, id) = a OR f(id, a) = a (also symmetric equation direction)
    if law.givens.len() == 1 && law.givens[0].type_name == "Int" {
        let g = &law.givens[0].name;
        let identity = match op.as_ref() {
            Some(BinOp::Add) => Some(0),
            Some(BinOp::Mul) => Some(1),
            _ => None,
        };
        if let Some(identity) = identity {
            let id_ok = matches_identity_side(&law.lhs, &law.rhs, fn_name, g, identity)
                || matches_identity_side(&law.rhs, &law.lhs, fn_name, g, identity);
            if id_ok {
                return Some(intro_then(
                    &intro_names,
                    vec![format!("simp [{}]", fn_lean)],
                ));
            }
        }
    }

    if let Some(op) = op.as_ref()
        && let Some(lines) = emit_sub_wrapper_law(vb, law, &intro_names, op, &fn_lean)
    {
        return Some(lines);
    }
    if let Some(lines) = emit_unary_wrapper_equivalence_law(vb, law, ctx, &intro_names) {
        return Some(lines);
    }
    if let Some(lines) = emit_direct_map_set_law(law, ctx, &intro_names) {
        return Some(lines);
    }
    if let Some(lines) = emit_map_update_law(vb, law, ctx, &intro_names) {
        return Some(lines);
    }

    None
}

fn intro_then(intro_names: &[String], steps: Vec<String>) -> Vec<String> {
    let mut lines = Vec::new();
    if !intro_names.is_empty() {
        lines.push(format!("intro {}", intro_names.join(" ")));
    }
    lines.extend(steps);
    lines.into_iter().map(|l| format!("  {}", l)).collect()
}

fn comm_lemma_for_op(op: &BinOp) -> Option<&'static str> {
    match op {
        BinOp::Add => Some("Int.add_comm"),
        BinOp::Mul => Some("Int.mul_comm"),
        _ => None,
    }
}

fn assoc_lemma_for_op(op: &BinOp) -> Option<&'static str> {
    match op {
        BinOp::Add => Some("Int.add_assoc"),
        BinOp::Mul => Some("Int.mul_assoc"),
        _ => None,
    }
}

fn emit_sub_wrapper_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    intro_names: &[String],
    op: &BinOp,
    fn_lean: &str,
) -> Option<Vec<String>> {
    if *op != BinOp::Sub {
        return None;
    }
    let fn_name = &vb.fn_name;

    // Law: sub(a, 0) = a (or symmetric equation side)
    if law.givens.len() == 1 && law.givens[0].type_name == "Int" {
        let g = &law.givens[0].name;
        let right_id = matches_sub_right_identity_side(&law.lhs, &law.rhs, fn_name, g)
            || matches_sub_right_identity_side(&law.rhs, &law.lhs, fn_name, g);
        if right_id {
            return Some(intro_then(intro_names, vec![format!("simp [{}]", fn_lean)]));
        }
    }

    // Law: sub(a, b) = -(sub(b, a)) (or symmetric equation side)
    if law.givens.len() == 2
        && law.givens[0].type_name == "Int"
        && law.givens[1].type_name == "Int"
        && (matches_binary_call(&law.lhs, fn_name, &law.givens[0].name, &law.givens[1].name)
            && matches_neg_binary_call(&law.rhs, fn_name, &law.givens[1].name, &law.givens[0].name)
            || matches_binary_call(&law.rhs, fn_name, &law.givens[0].name, &law.givens[1].name)
                && matches_neg_binary_call(
                    &law.lhs,
                    fn_name,
                    &law.givens[1].name,
                    &law.givens[0].name,
                ))
    {
        return Some(intro_then(
            intro_names,
            vec![format!(
                "simp [{}, sub_eq_add_neg, add_comm, add_left_comm, add_assoc]",
                fn_lean
            )],
        ));
    }

    None
}

fn emit_unary_wrapper_equivalence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    if law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return None;
    }
    let unary = int_unary_wrapper(ctx, &vb.fn_name)?;
    let given = &law.givens[0].name;

    let try_side = |call_side: &Expr, other_side: &Expr| -> Option<Vec<String>> {
        if !matches_unary_call(call_side, &vb.fn_name, given) {
            return None;
        }
        let (callee_name, var_first, lit) = binary_call_var_const(other_side, given)?;
        if lit != unary.constant || var_first != unary.var_first {
            return None;
        }
        let bin_op = int_binary_wrapper_op(ctx, &callee_name)?;
        if bin_op != unary.op {
            return None;
        }
        Some(intro_then(
            intro_names,
            vec![format!(
                "simp [{}, {}]",
                aver_name_to_lean(&vb.fn_name),
                aver_name_to_lean(&callee_name)
            )],
        ))
    };

    try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs))
}

fn emit_direct_map_set_law(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    // Law: Map.has(Map.set(m, k, v), k) = true
    let has_side = |side: &Expr, other: &Expr| -> Option<Vec<String>> {
        let (m, k, v) = map_has_set_parts(side)?;
        if !matches_bool_true(other) {
            return None;
        }
        Some(intro_then(
            intro_names,
            vec![format!(
                "simpa using AverMap.has_set_self {} {} {}",
                atom(&emit_expr(m, ctx)),
                atom(&emit_expr(k, ctx)),
                atom(&emit_expr(v, ctx))
            )],
        ))
    };
    if let Some(lines) = has_side(&law.lhs, &law.rhs).or_else(|| has_side(&law.rhs, &law.lhs)) {
        return Some(lines);
    }

    // Law: Map.get(Map.set(m, k, v), k) = Option.Some(v)
    let get_side = |side: &Expr, other: &Expr| -> Option<Vec<String>> {
        let (m, k, v) = map_get_set_parts(side)?;
        let some_v = option_some_arg(other)?;
        if some_v != v {
            return None;
        }
        Some(intro_then(
            intro_names,
            vec![format!(
                "simpa using AverMap.get_set_self {} {} {}",
                atom(&emit_expr(m, ctx)),
                atom(&emit_expr(k, ctx)),
                atom(&emit_expr(v, ctx))
            )],
        ))
    };
    get_side(&law.lhs, &law.rhs).or_else(|| get_side(&law.rhs, &law.lhs))
}

fn emit_map_update_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    map_key_update_shape(ctx, &vb.fn_name)?;
    let fn_lean = aver_name_to_lean(&vb.fn_name);

    // Law: Map.has(updateFn(m, k), k) = true
    let key_present_side = |side: &Expr, other: &Expr| -> Option<Vec<String>> {
        if !matches_bool_true(other) {
            return None;
        }
        let (map_arg, key_arg) = map_has_after_fn_call(side, &vb.fn_name)?;
        Some(intro_then(
            intro_names,
            vec![
                format!("simp [{}]", fn_lean),
                format!(
                    "cases h : AverMap.get {} {} <;> simp [AverMap.has_set_self]",
                    atom(&emit_expr(map_arg, ctx)),
                    atom(&emit_expr(key_arg, ctx))
                ),
            ],
        ))
    };
    if let Some(lines) =
        key_present_side(&law.lhs, &law.rhs).or_else(|| key_present_side(&law.rhs, &law.lhs))
    {
        return Some(lines);
    }

    // Law: Map.get(updateFn(m, k), k) = Option.Some(...)
    let get_after_update_side = |side: &Expr, other: &Expr| -> Option<Vec<String>> {
        let (map_arg, key_arg) = map_get_after_fn_call(side, &vb.fn_name)?;
        option_some_arg(other)?;

        let mut simp_defs: Vec<String> = law_simp_defs(ctx, vb, law).into_iter().collect();
        if !simp_defs.iter().any(|n| n == "AverMap.get_set_self") {
            // Keep helper theorem separate in second simp call for readability.
            simp_defs.sort();
        }
        let simp_list = format!("[{}]", simp_defs.join(", "));
        let extra = if simp_defs.is_empty() {
            String::new()
        } else {
            format!(", {}", simp_defs.join(", "))
        };

        Some(intro_then(
            intro_names,
            vec![
                format!("simp {}", simp_list),
                format!(
                    "cases h : AverMap.get {} {} <;> simp [AverMap.get_set_self{}]",
                    atom(&emit_expr(map_arg, ctx)),
                    atom(&emit_expr(key_arg, ctx)),
                    extra
                ),
            ],
        ))
    };

    get_after_update_side(&law.lhs, &law.rhs).or_else(|| get_after_update_side(&law.rhs, &law.lhs))
}

#[derive(Clone, Debug, PartialEq)]
struct UnaryIntWrapper {
    op: BinOp,
    constant: i64,
    var_first: bool,
}

#[derive(Clone, Debug)]
struct MapKeyUpdateShape {
    map_param: String,
    key_param: String,
}

fn int_binary_wrapper_op(ctx: &CodegenContext, fn_name: &str) -> Option<BinOp> {
    let fd = find_fn_def(ctx, fn_name)?;
    if fd.params.len() != 2 || fd.return_type != "Int" {
        return None;
    }
    let (p1, t1) = &fd.params[0];
    let (p2, t2) = &fd.params[1];
    if t1 != "Int" || t2 != "Int" {
        return None;
    }
    let FnBody::Expr(expr) = fd.body.as_ref() else {
        return None;
    };
    let Expr::BinOp(op, left, right) = expr else {
        return None;
    };
    if !matches_ident(left, p1) || !matches_ident(right, p2) {
        return None;
    }
    Some(op.clone())
}

fn int_unary_wrapper(ctx: &CodegenContext, fn_name: &str) -> Option<UnaryIntWrapper> {
    let fd = find_fn_def(ctx, fn_name)?;
    if fd.params.len() != 1 || fd.return_type != "Int" {
        return None;
    }
    let (param, param_ty) = &fd.params[0];
    if param_ty != "Int" {
        return None;
    }
    let FnBody::Expr(expr) = fd.body.as_ref() else {
        return None;
    };
    let Expr::BinOp(op, left, right) = expr else {
        return None;
    };

    match (left.as_ref(), right.as_ref()) {
        (Expr::Ident(id), Expr::Literal(Literal::Int(n))) if id == param => Some(UnaryIntWrapper {
            op: op.clone(),
            constant: *n,
            var_first: true,
        }),
        (Expr::Literal(Literal::Int(n)), Expr::Ident(id)) if id == param => Some(UnaryIntWrapper {
            op: op.clone(),
            constant: *n,
            var_first: false,
        }),
        _ => None,
    }
}

fn map_key_update_shape(ctx: &CodegenContext, fn_name: &str) -> Option<MapKeyUpdateShape> {
    let fd = find_fn_def(ctx, fn_name)?;
    if fd.params.len() != 2 {
        return None;
    }
    let map_param = fd.params[0].0.clone();
    let key_param = fd.params[1].0.clone();
    let shape = MapKeyUpdateShape {
        map_param,
        key_param,
    };

    let is_shape = match fd.body.as_ref() {
        FnBody::Expr(expr) => map_update_match_expr(expr, &shape, None),
        FnBody::Block(stmts) => map_update_block(stmts, &shape),
    };
    is_shape.then_some(shape)
}

fn map_update_block(stmts: &[Stmt], shape: &MapKeyUpdateShape) -> bool {
    if stmts.len() < 2 {
        return false;
    }
    let Some(last) = stmts.last() else {
        return false;
    };
    let mut bound_name: Option<&str> = None;
    for stmt in &stmts[..stmts.len() - 1] {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                if !is_map_get_call(expr, &shape.map_param, &shape.key_param) {
                    return false;
                }
                bound_name = Some(name);
            }
            Stmt::Expr(_) => return false,
        }
    }
    match last {
        Stmt::Expr(expr) => map_update_match_expr(expr, shape, bound_name),
        Stmt::Binding(_, _, _) => false,
    }
}

fn map_update_match_expr(expr: &Expr, shape: &MapKeyUpdateShape, bound_name: Option<&str>) -> bool {
    let Expr::Match { subject, arms, .. } = expr else {
        return false;
    };
    if arms.len() < 2 {
        return false;
    }
    let subject_ok = match bound_name {
        Some(name) => matches!(subject.as_ref(), Expr::Ident(id) if id == name),
        None => is_map_get_call(subject, &shape.map_param, &shape.key_param),
    };
    if !subject_ok {
        return false;
    }
    arms.iter()
        .all(|arm| is_map_set_call(&arm.body, &shape.map_param, &shape.key_param))
}

fn law_simp_defs(ctx: &CodegenContext, vb: &VerifyBlock, law: &VerifyLaw) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    names.insert(aver_name_to_lean(&vb.fn_name));
    collect_user_fn_simp_names(&law.lhs, ctx, &vb.fn_name, &mut names);
    collect_user_fn_simp_names(&law.rhs, ctx, &vb.fn_name, &mut names);
    names
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
                out.insert(aver_name_to_lean(&fd.name));
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
            for arg in &call.1 {
                collect_user_fn_simp_names(arg, ctx, skip_fn, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => {}
    }
}

fn find_fn_def<'a>(ctx: &'a CodegenContext, fn_name: &str) -> Option<&'a FnDef> {
    ctx.modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .find(|fd| fd.name == fn_name)
}

fn find_fn_def_by_call_name<'a>(ctx: &'a CodegenContext, call_name: &str) -> Option<&'a FnDef> {
    find_fn_def(ctx, call_name).or_else(|| {
        let short = call_name.rsplit('.').next()?;
        find_fn_def(ctx, short)
    })
}

fn expr_dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(base, field) => expr_dotted_name(base).map(|p| format!("{p}.{field}")),
        _ => None,
    }
}

fn matches_ident(expr: &Expr, name: &str) -> bool {
    matches!(expr, Expr::Ident(n) if n == name)
}

fn call2_args<'a>(expr: &'a Expr, fn_name: &str) -> Option<(&'a Expr, &'a Expr)> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    if args.len() != 2 || !matches_ident(callee, fn_name) {
        return None;
    }
    Some((&args[0], &args[1]))
}

fn call_named_args<'a>(expr: &'a Expr, full_name: &str) -> Option<&'a [Expr]> {
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

fn matches_binary_call(expr: &Expr, fn_name: &str, a: &str, b: &str) -> bool {
    let Some((x, y)) = call2_args(expr, fn_name) else {
        return false;
    };
    matches_ident(x, a) && matches_ident(y, b)
}

fn matches_unary_call(expr: &Expr, fn_name: &str, arg: &str) -> bool {
    let Expr::FnCall(callee, args) = expr else {
        return false;
    };
    args.len() == 1 && matches_ident(callee, fn_name) && matches_ident(&args[0], arg)
}

fn binary_call_var_const(expr: &Expr, var_name: &str) -> Option<(String, bool, i64)> {
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

fn matches_assoc_nested(expr: &Expr, fn_name: &str, a: &str, b: &str, c: &str) -> bool {
    let Some((ab, z)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((x, y)) = call2_args(ab, fn_name) else {
        return false;
    };
    matches_ident(x, a) && matches_ident(y, b) && matches_ident(z, c)
}

fn matches_assoc_flat(expr: &Expr, fn_name: &str, a: &str, b: &str, c: &str) -> bool {
    let Some((x, bc)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((y, z)) = call2_args(bc, fn_name) else {
        return false;
    };
    matches_ident(x, a) && matches_ident(y, b) && matches_ident(z, c)
}

fn matches_identity_side(
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

fn matches_sub_right_identity_side(
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

fn matches_neg_binary_call(expr: &Expr, fn_name: &str, a: &str, b: &str) -> bool {
    match expr {
        Expr::BinOp(BinOp::Sub, left, right) => {
            matches_int_lit(left, 0) && matches_binary_call(right, fn_name, a, b)
        }
        _ => false,
    }
}

fn matches_int_lit(expr: &Expr, expected: i64) -> bool {
    matches!(expr, Expr::Literal(Literal::Int(n)) if *n == expected)
}

fn matches_bool_true(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(Literal::Bool(true)))
}

fn map_has_set_parts(expr: &Expr) -> Option<(&Expr, &Expr, &Expr)> {
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

fn map_get_set_parts(expr: &Expr) -> Option<(&Expr, &Expr, &Expr)> {
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

fn option_some_arg(expr: &Expr) -> Option<&Expr> {
    let args = call_named_args(expr, "Option.Some")?;
    (args.len() == 1).then_some(&args[0])
}

fn map_has_after_fn_call<'a>(expr: &'a Expr, fn_name: &str) -> Option<(&'a Expr, &'a Expr)> {
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

fn map_get_after_fn_call<'a>(expr: &'a Expr, fn_name: &str) -> Option<(&'a Expr, &'a Expr)> {
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

fn is_map_get_call(expr: &Expr, map_param: &str, key_param: &str) -> bool {
    let Some(args) = call_named_args(expr, "Map.get") else {
        return false;
    };
    args.len() == 2 && matches_ident(&args[0], map_param) && matches_ident(&args[1], key_param)
}

fn is_map_set_call(expr: &Expr, map_param: &str, key_param: &str) -> bool {
    let Some(args) = call_named_args(expr, "Map.set") else {
        return false;
    };
    args.len() == 3 && matches_ident(&args[0], map_param) && matches_ident(&args[1], key_param)
}

fn atom(s: &str) -> String {
    if s.contains(' ') && !s.starts_with('(') {
        format!("({s})")
    } else {
        s.to_string()
    }
}
