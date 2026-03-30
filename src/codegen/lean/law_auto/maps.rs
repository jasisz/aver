use super::super::expr::{aver_name_to_lean, emit_expr};
use crate::ast::{BinOp, Expr, Pattern, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::intro_then;
use super::shared::{
    atom, defaulted_map_get, defaulted_map_get_after_fn_call, find_fn_def, is_map_get_call,
    is_map_set_call, law_simp_defs, map_get_after_fn_call, map_get_set_parts,
    map_has_after_fn_call, map_has_set_parts, matches_bool_true, matches_ident, matches_int_lit,
    option_some_arg,
};

pub(super) fn emit_direct_map_set_law(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    let has_side = |side: &Spanned<Expr>, other: &Spanned<Expr>| -> Option<Vec<String>> {
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

    let get_side = |side: &Spanned<Expr>, other: &Spanned<Expr>| -> Option<Vec<String>> {
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

pub(super) fn emit_map_update_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    map_key_update_shape(ctx, &vb.fn_name)?;
    let fn_lean = aver_name_to_lean(&vb.fn_name);

    let key_present_side = |side: &Spanned<Expr>, other: &Spanned<Expr>| -> Option<Vec<String>> {
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

    let get_after_update_side =
        |side: &Spanned<Expr>, other: &Spanned<Expr>| -> Option<Vec<String>> {
            let (map_arg, key_arg) = map_get_after_fn_call(side, &vb.fn_name)?;
            option_some_arg(other)?;

            let mut simp_defs: Vec<String> = law_simp_defs(ctx, vb, law).into_iter().collect();
            if !simp_defs.iter().any(|n| n == "AverMap.get_set_self") {
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

pub(super) fn emit_map_increment_tracked_count_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    map_increment_update_shape(ctx, &vb.fn_name)?;

    let tracked_count_side = |side: &Spanned<Expr>, other: &Spanned<Expr>| -> Option<Vec<String>> {
        let (map_arg, key_arg, default_arg) = defaulted_map_get_after_fn_call(side, &vb.fn_name)?;
        if !matches_int_lit(default_arg, 0) {
            return None;
        }
        let Expr::BinOp(BinOp::Add, base, one) = &other.node else {
            return None;
        };
        if !matches_int_lit(one, 1) {
            return None;
        }
        let (base_map, base_key, base_default) = defaulted_map_get(base)?;
        if map_arg != base_map || key_arg != base_key || default_arg != base_default {
            return None;
        }

        let map = atom(&emit_expr(map_arg, ctx));
        let key = atom(&emit_expr(key_arg, ctx));
        Some(intro_then(
            intro_names,
            vec![
                format!("simp [{}]", aver_name_to_lean(&vb.fn_name)),
                format!(
                    "cases h : AverMap.get {} {} <;> simp [AverMap.get_set_self, h]",
                    map, key
                ),
            ],
        ))
    };

    tracked_count_side(&law.lhs, &law.rhs).or_else(|| tracked_count_side(&law.rhs, &law.lhs))
}

#[derive(Clone, Debug)]
struct MapKeyUpdateShape {
    map_param: String,
    key_param: String,
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

    let is_shape = map_update_block(fd.body.stmts(), &shape);
    is_shape.then_some(shape)
}

fn map_increment_update_shape(ctx: &CodegenContext, fn_name: &str) -> Option<()> {
    let fd = find_fn_def(ctx, fn_name)?;
    if fd.params.len() != 2 {
        return None;
    }
    let map_param = fd.params[0].0.clone();
    let key_param = fd.params[1].0.clone();
    let stmts = fd.body.stmts();
    if stmts.len() != 2 {
        return None;
    }
    let Stmt::Binding(current, _, bound_expr) = &stmts[0] else {
        return None;
    };
    if !is_map_get_call(bound_expr, &map_param, &key_param) {
        return None;
    }
    let Stmt::Expr(last_expr) = &stmts[1] else {
        return None;
    };
    let Expr::Match { subject, arms, .. } = &last_expr.node else {
        return None;
    };
    if !matches_ident(subject, current) || arms.len() != 2 {
        return None;
    }

    let some_arm = arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Constructor(name, vars) if name == "Option.Some" && vars.len() == 1 => {
            Some((vars[0].as_str(), arm.body.as_ref()))
        }
        _ => None,
    })?;
    let none_arm = arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Constructor(name, vars) if name == "Option.None" && vars.is_empty() => {
            Some(arm.body.as_ref())
        }
        _ => None,
    })?;

    let (some_bound, some_body) = some_arm;
    let some_set = super::shared::call_named_args(some_body, "Map.set")?;
    let none_set = super::shared::call_named_args(none_arm, "Map.set")?;
    if some_set.len() != 3 || none_set.len() != 3 {
        return None;
    }
    if !matches_ident(&some_set[0], &map_param)
        || !matches_ident(&some_set[1], &key_param)
        || !matches_ident(&none_set[0], &map_param)
        || !matches_ident(&none_set[1], &key_param)
    {
        return None;
    }
    let Expr::BinOp(BinOp::Add, add_left, add_right) = &some_set[2].node else {
        return None;
    };
    if !matches_ident(add_left, some_bound) || !matches_int_lit(add_right, 1) {
        return None;
    }
    if !matches_int_lit(&none_set[2], 1) {
        return None;
    }

    Some(())
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

fn map_update_match_expr(
    expr: &Spanned<Expr>,
    shape: &MapKeyUpdateShape,
    bound_name: Option<&str>,
) -> bool {
    let Expr::Match { subject, arms, .. } = &expr.node else {
        return false;
    };
    if arms.len() < 2 {
        return false;
    }
    let subject_ok = match bound_name {
        Some(name) => matches!(&subject.node, Expr::Ident(id) if id == name),
        None => is_map_get_call(subject, &shape.map_param, &shape.key_param),
    };
    if !subject_ok {
        return false;
    }
    arms.iter()
        .all(|arm| is_map_set_call(&arm.body, &shape.map_param, &shape.key_param))
}
