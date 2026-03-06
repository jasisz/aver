use super::super::expr::{aver_name_to_lean, emit_expr};
use crate::ast::{BinOp, Expr, Literal, Pattern, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::shared::{
    atom, body_terminal_expr, defaulted_map_get, defaulted_map_get_after_agg_call,
    defaulted_map_get_after_fn_call, expr_dotted_name, find_fn_def, find_fn_def_by_call_name,
    ident_name, is_map_get_call, is_map_set_call, law_simp_defs, map_get_after_fn_call,
    map_get_set_parts, map_has_after_agg_call, map_has_after_fn_call, map_has_set_parts,
    matches_bool_true, matches_equality_pair, matches_ident, matches_int_lit,
    matches_list_contains_call, matches_recursive_counter_step, matches_recursive_self_call,
    option_some_arg,
};
use super::{indent_lines, intro_then};

pub(super) fn emit_direct_map_set_law(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
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

pub(super) fn emit_map_update_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    map_key_update_shape(ctx, &vb.fn_name)?;
    let fn_lean = aver_name_to_lean(&vb.fn_name);

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

    let get_after_update_side = |side: &Expr, other: &Expr| -> Option<Vec<String>> {
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

    let tracked_count_side = |side: &Expr, other: &Expr| -> Option<Vec<String>> {
        let (map_arg, key_arg, default_arg) = defaulted_map_get_after_fn_call(side, &vb.fn_name)?;
        if !matches_int_lit(default_arg, 0) {
            return None;
        }
        let Expr::BinOp(BinOp::Add, base, one) = other else {
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

pub(super) fn emit_recursive_map_presence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    _intro_names: &[String],
) -> Option<Vec<String>> {
    let acc = list_map_accumulator_shape(ctx, &vb.fn_name)?;
    let update = map_increment_update_shape(ctx, &acc.update_fn)?;
    if acc.update_fn != update.fn_name {
        return None;
    }

    let (list_var, tracked_var, observer) =
        presence_law_shape(&law.lhs, &law.rhs, &vb.fn_name, ctx)
            .or_else(|| presence_law_shape(&law.rhs, &law.lhs, &vb.fn_name, ctx))?;

    let list_lean = aver_name_to_lean(list_var);
    let tracked_lean = aver_name_to_lean(tracked_var);
    let head_lean = fresh_lean_name(
        &aver_name_to_lean(&acc.head_name),
        &[&list_lean, &tracked_lean],
    );
    let tail_lean = fresh_lean_name(
        &aver_name_to_lean(&acc.tail_name),
        &[&list_lean, &tracked_lean, &head_lean],
    );
    let acc_fn_lean = aver_name_to_lean(&vb.fn_name);
    let update_fn_lean = aver_name_to_lean(&acc.update_fn);
    let observer_simp = observer.simp_defs().map(aver_name_to_lean);
    let mut lines = vec![
        format!("intro {} {}", list_lean, tracked_lean),
        format!("induction {} with", list_lean),
        "| nil =>".to_string(),
        format!(
            "    simp [{}]",
            join_simp_defs([
                acc.base_simp_def.as_deref(),
                observer_simp.as_deref(),
                Some("AverMap.has"),
                Some("AverMap.empty"),
                Some(&acc_fn_lean),
            ])
        ),
        format!("| cons {} {} ih =>", head_lean, tail_lean),
        format!("    by_cases h : {} = {}", head_lean, tracked_lean),
        "    · subst h".to_string(),
        format!(
            "      simp [{}]",
            join_simp_defs([observer_simp.as_deref(), Some(&acc_fn_lean)])
        ),
        format!(
            "      cases hg : AverMap.get ({} {}) {} with",
            acc_fn_lean, tail_lean, head_lean
        ),
        "      | none =>".to_string(),
    ];
    lines.extend(emit_increment_update_have_eq(
        &update_fn_lean,
        &format!("({} {})", acc_fn_lean, tail_lean),
        &head_lean,
        None,
        10,
    ));
    lines.extend(indent_lines(
        vec![
            "rw [hstep]".to_string(),
            format!(
                "simpa using (AverMap.has_set_self ({} {}) {} (1 : Int))",
                acc_fn_lean, tail_lean, head_lean
            ),
        ],
        10,
    ));
    lines.push("      | some n =>".to_string());
    lines.extend(emit_increment_update_have_eq(
        &update_fn_lean,
        &format!("({} {})", acc_fn_lean, tail_lean),
        &head_lean,
        Some("n"),
        10,
    ));
    lines.extend(indent_lines(
        vec![
            "rw [hstep]".to_string(),
            format!(
                "simpa using (AverMap.has_set_self ({} {}) {} (n + 1))",
                acc_fn_lean, tail_lean, head_lean
            ),
        ],
        10,
    ));
    lines.push(format!(
        "    · have hw : {} ≠ {} := by",
        tracked_lean, head_lean
    ));
    lines.push("        intro hw_eq".to_string());
    lines.push("        exact h hw_eq.symm".to_string());
    lines.push(format!(
        "      simp [{}]",
        join_simp_defs([observer_simp.as_deref(), Some(&acc_fn_lean), Some("h")])
    ));
    lines.push(format!(
        "      cases hg : AverMap.get ({} {}) {} with",
        acc_fn_lean, tail_lean, head_lean
    ));
    lines.push("      | none =>".to_string());
    lines.extend(emit_increment_update_have_eq(
        &update_fn_lean,
        &format!("({} {})", acc_fn_lean, tail_lean),
        &head_lean,
        None,
        10,
    ));
    lines.extend(indent_lines(
        vec![
            "rw [hstep]".to_string(),
            format!(
                "simpa [AverMap.has_set_other, hw{}] using ih",
                observer.extra_simpa_suffix()
            ),
        ],
        10,
    ));
    lines.push("      | some n =>".to_string());
    lines.extend(emit_increment_update_have_eq(
        &update_fn_lean,
        &format!("({} {})", acc_fn_lean, tail_lean),
        &head_lean,
        Some("n"),
        10,
    ));
    lines.extend(indent_lines(
        vec![
            "rw [hstep]".to_string(),
            format!(
                "simpa [AverMap.has_set_other, hw{}] using ih",
                observer.extra_simpa_suffix()
            ),
        ],
        10,
    ));

    Some(indent_lines(lines, 2))
}

pub(super) fn emit_recursive_map_tracked_count_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    _intro_names: &[String],
) -> Option<Vec<String>> {
    let acc = list_map_accumulator_shape(ctx, &vb.fn_name)?;
    let update = map_increment_update_shape(ctx, &acc.update_fn)?;
    if acc.update_fn != update.fn_name {
        return None;
    }

    let (list_var, tracked_var, counter_fn) =
        tracked_count_law_shape(&law.lhs, &law.rhs, &vb.fn_name, ctx)
            .or_else(|| tracked_count_law_shape(&law.rhs, &law.lhs, &vb.fn_name, ctx))?;
    recursive_list_counter_shape(ctx, &counter_fn)?;

    let list_lean = aver_name_to_lean(list_var);
    let tracked_lean = aver_name_to_lean(tracked_var);
    let head_lean = fresh_lean_name(
        &aver_name_to_lean(&acc.head_name),
        &[&list_lean, &tracked_lean],
    );
    let tail_lean = fresh_lean_name(
        &aver_name_to_lean(&acc.tail_name),
        &[&list_lean, &tracked_lean, &head_lean],
    );
    let acc_fn_lean = aver_name_to_lean(&vb.fn_name);
    let update_fn_lean = aver_name_to_lean(&acc.update_fn);
    let counter_lean = aver_name_to_lean(&counter_fn);

    let mut lines = vec![
        format!("intro {} {}", list_lean, tracked_lean),
        format!("induction {} with", list_lean),
        "| nil =>".to_string(),
        format!(
            "    simp [{}]",
            join_simp_defs([
                acc.base_simp_def.as_deref(),
                Some(&counter_lean),
                Some("AverMap.get"),
                Some("AverMap.empty"),
                Some(&acc_fn_lean),
            ])
        ),
        format!("| cons {} {} ih =>", head_lean, tail_lean),
        format!("    by_cases h : {} = {}", head_lean, tracked_lean),
        "    · subst h".to_string(),
        format!(
            "      simp [{}]",
            join_simp_defs([Some(&acc_fn_lean), Some(&counter_lean)])
        ),
        format!(
            "      cases hg : AverMap.get ({} {}) {} with",
            acc_fn_lean, tail_lean, head_lean
        ),
        "      | none =>".to_string(),
        "          have ih' := ih".to_string(),
        "          simp [hg] at ih'".to_string(),
    ];
    lines.extend(emit_increment_update_have_eq(
        &update_fn_lean,
        &format!("({} {})", acc_fn_lean, tail_lean),
        &head_lean,
        None,
        10,
    ));
    lines.extend(indent_lines(
        vec![
            "rw [hstep]".to_string(),
            "simpa [AverMap.get_set_self] using congrArg (fun n => n + 1) ih'".to_string(),
        ],
        10,
    ));
    lines.push("      | some n =>".to_string());
    lines.push("          have ih' := ih".to_string());
    lines.push("          simp [hg] at ih'".to_string());
    lines.extend(emit_increment_update_have_eq(
        &update_fn_lean,
        &format!("({} {})", acc_fn_lean, tail_lean),
        &head_lean,
        Some("n"),
        10,
    ));
    lines.extend(indent_lines(
        vec![
            "rw [hstep]".to_string(),
            "simpa [AverMap.get_set_self] using ih'".to_string(),
        ],
        10,
    ));
    lines.push(format!(
        "    · have hw : {} ≠ {} := by",
        tracked_lean, head_lean
    ));
    lines.push("        intro hw_eq".to_string());
    lines.push("        exact h hw_eq.symm".to_string());
    lines.push(format!(
        "      simp [{}]",
        join_simp_defs([Some(&acc_fn_lean), Some(&counter_lean), Some("h")])
    ));
    lines.push(format!(
        "      cases hg : AverMap.get ({} {}) {} with",
        acc_fn_lean, tail_lean, head_lean
    ));
    lines.push("      | none =>".to_string());
    lines.extend(emit_increment_update_have_eq(
        &update_fn_lean,
        &format!("({} {})", acc_fn_lean, tail_lean),
        &head_lean,
        None,
        10,
    ));
    lines.extend(indent_lines(
        vec![
            "rw [hstep]".to_string(),
            "simpa [AverMap.get_set_other, hw] using ih".to_string(),
        ],
        10,
    ));
    lines.push("      | some n =>".to_string());
    lines.extend(emit_increment_update_have_eq(
        &update_fn_lean,
        &format!("({} {})", acc_fn_lean, tail_lean),
        &head_lean,
        Some("n"),
        10,
    ));
    lines.extend(indent_lines(
        vec![
            "rw [hstep]".to_string(),
            "simpa [AverMap.get_set_other, hw] using ih".to_string(),
        ],
        10,
    ));

    Some(indent_lines(lines, 2))
}

#[derive(Clone, Debug)]
struct MapKeyUpdateShape {
    map_param: String,
    key_param: String,
}

#[derive(Clone, Debug)]
struct MapIncrementUpdateShape {
    fn_name: String,
}

#[derive(Clone, Debug)]
struct ListMapAccumulatorShape {
    head_name: String,
    tail_name: String,
    update_fn: String,
    base_simp_def: Option<String>,
}

#[derive(Clone, Debug)]
enum PresenceObserver {
    DirectListContains,
    WrapperFn(String),
}

impl PresenceObserver {
    fn simp_defs(&self) -> Option<&str> {
        match self {
            Self::DirectListContains => None,
            Self::WrapperFn(name) => Some(name.as_str()),
        }
    }

    fn extra_simpa_suffix(&self) -> String {
        match self {
            Self::DirectListContains => String::new(),
            Self::WrapperFn(name) => format!(", {}", aver_name_to_lean(name)),
        }
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

    let is_shape = map_update_block(fd.body.stmts(), &shape);
    is_shape.then_some(shape)
}

fn map_increment_update_shape(
    ctx: &CodegenContext,
    fn_name: &str,
) -> Option<MapIncrementUpdateShape> {
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
    let Stmt::Expr(Expr::Match { subject, arms, .. }) = &stmts[1] else {
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
    let Expr::BinOp(BinOp::Add, add_left, add_right) = &some_set[2] else {
        return None;
    };
    if !matches_ident(add_left, some_bound) || !matches_int_lit(add_right, 1) {
        return None;
    }
    if !matches_int_lit(&none_set[2], 1) {
        return None;
    }

    Some(MapIncrementUpdateShape {
        fn_name: fd.name.clone(),
    })
}

fn list_map_accumulator_shape(
    ctx: &CodegenContext,
    fn_name: &str,
) -> Option<ListMapAccumulatorShape> {
    let fd = find_fn_def(ctx, fn_name)?;
    if fd.params.len() != 1 {
        return None;
    }
    let list_param = &fd.params[0].0;
    let Expr::Match { subject, arms, .. } = body_terminal_expr(fd.body.as_ref())? else {
        return None;
    };
    if !matches_ident(subject, list_param) || arms.len() != 2 {
        return None;
    }
    let empty_arm = arms
        .iter()
        .find(|arm| matches!(arm.pattern, Pattern::EmptyList))?;
    let cons_arm = arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Cons(head, tail) => Some((head.as_str(), tail.as_str(), arm.body.as_ref())),
        _ => None,
    })?;
    let (head_name, tail_name, cons_body) = cons_arm;
    let Expr::FnCall(callee, args) = cons_body else {
        return None;
    };
    if args.len() != 2 {
        return None;
    }
    let update_fn = expr_dotted_name(callee)?;
    if !matches_ident(&args[1], head_name) {
        return None;
    }
    if !matches_recursive_self_call(&args[0], fn_name, tail_name) {
        return None;
    }
    let base_simp_def = match empty_arm.body.as_ref() {
        Expr::FnCall(callee, args) if args.is_empty() => {
            expr_dotted_name(callee).map(|name| aver_name_to_lean(&name))
        }
        _ => None,
    };

    Some(ListMapAccumulatorShape {
        head_name: head_name.to_string(),
        tail_name: tail_name.to_string(),
        update_fn,
        base_simp_def,
    })
}

fn recursive_list_counter_shape(ctx: &CodegenContext, fn_name: &str) -> Option<()> {
    let fd = find_fn_def(ctx, fn_name)?;
    if fd.params.len() != 2 || fd.return_type != "Int" {
        return None;
    }
    let list_param = &fd.params[0].0;
    let tracked_param = &fd.params[1].0;
    let Expr::Match { subject, arms, .. } = body_terminal_expr(fd.body.as_ref())? else {
        return None;
    };
    if !matches_ident(subject, list_param) || arms.len() != 2 {
        return None;
    }
    let empty_arm = arms
        .iter()
        .find(|arm| matches!(arm.pattern, Pattern::EmptyList))?;
    if !matches_int_lit(&empty_arm.body, 0) {
        return None;
    }
    let (head_name, tail_name, cons_body) = arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Cons(head, tail) => Some((head.as_str(), tail.as_str(), arm.body.as_ref())),
        _ => None,
    })?;
    let Expr::Match {
        subject: cond,
        arms: bool_arms,
        ..
    } = cons_body
    else {
        return None;
    };
    if !matches_equality_pair(cond, head_name, tracked_param) || bool_arms.len() != 2 {
        return None;
    }
    let true_arm = bool_arms
        .iter()
        .find(|arm| matches!(arm.pattern, Pattern::Literal(Literal::Bool(true))))?;
    let false_arm = bool_arms
        .iter()
        .find(|arm| matches!(arm.pattern, Pattern::Literal(Literal::Bool(false))))?;
    if !matches_recursive_counter_step(&true_arm.body, fn_name, tail_name, tracked_param, true)
        || !matches_recursive_counter_step(
            &false_arm.body,
            fn_name,
            tail_name,
            tracked_param,
            false,
        )
    {
        return None;
    }
    Some(())
}

fn presence_law_shape<'a>(
    map_side: &'a Expr,
    other_side: &'a Expr,
    fn_name: &str,
    ctx: &CodegenContext,
) -> Option<(&'a str, &'a str, PresenceObserver)> {
    let (list_arg, tracked_arg) = map_has_after_agg_call(map_side, fn_name)?;
    let (list_name, tracked_name) = (ident_name(list_arg)?, ident_name(tracked_arg)?);
    if matches_list_contains_call(other_side, list_name, tracked_name) {
        return Some((
            list_name,
            tracked_name,
            PresenceObserver::DirectListContains,
        ));
    }
    let Expr::FnCall(callee, args) = other_side else {
        return None;
    };
    if args.len() != 2
        || !matches_ident(&args[0], list_name)
        || !matches_ident(&args[1], tracked_name)
    {
        return None;
    }
    let observer_fn = expr_dotted_name(callee)?;
    list_contains_wrapper_shape(ctx, &observer_fn)?;
    Some((
        list_name,
        tracked_name,
        PresenceObserver::WrapperFn(observer_fn),
    ))
}

fn tracked_count_law_shape<'a>(
    map_side: &'a Expr,
    other_side: &'a Expr,
    fn_name: &str,
    _ctx: &CodegenContext,
) -> Option<(&'a str, &'a str, String)> {
    let (list_arg, tracked_arg, default_arg) = defaulted_map_get_after_agg_call(map_side, fn_name)?;
    if !matches_int_lit(default_arg, 0) {
        return None;
    }
    let (list_name, tracked_name) = (ident_name(list_arg)?, ident_name(tracked_arg)?);
    let Expr::FnCall(callee, args) = other_side else {
        return None;
    };
    if args.len() != 2
        || !matches_ident(&args[0], list_name)
        || !matches_ident(&args[1], tracked_name)
    {
        return None;
    }
    Some((list_name, tracked_name, expr_dotted_name(callee)?))
}

fn list_contains_wrapper_shape(ctx: &CodegenContext, fn_name: &str) -> Option<()> {
    let fd = find_fn_def_by_call_name(ctx, fn_name)?;
    if fd.params.len() != 2 || fd.return_type != "Bool" {
        return None;
    }
    let body = body_terminal_expr(fd.body.as_ref())?;
    let args = super::shared::call_named_args(body, "List.contains")?;
    if args.len() != 2
        || !matches_ident(&args[0], &fd.params[0].0)
        || !matches_ident(&args[1], &fd.params[1].0)
    {
        return None;
    }
    Some(())
}

fn emit_increment_update_have_eq(
    update_fn_lean: &str,
    map_expr: &str,
    key_expr: &str,
    some_var: Option<&str>,
    indent: usize,
) -> Vec<String> {
    let target = match some_var {
        Some(name) => format!("AverMap.set {map_expr} {key_expr} ({name} + 1)"),
        None => format!("AverMap.set {map_expr} {key_expr} 1"),
    };
    let lines = vec![
        format!(
            "have hstep : {} {} {} = {} := by",
            update_fn_lean, map_expr, key_expr, target
        ),
        format!("  unfold {}", update_fn_lean),
        format!(
            "  change (let current := AverMap.get {} {};",
            map_expr, key_expr
        ),
        "    match h_cur : current with".to_string(),
        format!(
            "    | some n => AverMap.set {} {} (n + 1)",
            map_expr, key_expr
        ),
        format!(
            "    | none => AverMap.set {} {} 1) = {}",
            map_expr, key_expr, target
        ),
        format!("  cases h_cur : AverMap.get {} {} with", map_expr, key_expr),
        "  | none => simp_all".to_string(),
        "  | some m => simp_all".to_string(),
    ];
    indent_lines(lines, indent)
}

fn join_simp_defs<const N: usize>(defs: [Option<&str>; N]) -> String {
    defs.into_iter()
        .flatten()
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(", ")
}

fn fresh_lean_name(base: &str, taken: &[&str]) -> String {
    if !taken.contains(&base) {
        return base.to_string();
    }
    let mut index = 1;
    loop {
        let candidate = format!("{base}_{index}");
        if !taken.iter().any(|name| *name == candidate) {
            return candidate;
        }
        index += 1;
    }
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
