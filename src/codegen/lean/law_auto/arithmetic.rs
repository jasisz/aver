use super::super::expr::aver_name_to_lean;
use crate::ast::{BinOp, Expr, Literal, Spanned, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::intro_then;
use super::shared::{
    binary_call_var_const, body_terminal_expr, find_fn_def_by_call_name, matches_assoc_flat,
    matches_assoc_nested, matches_binary_call, matches_ident, matches_identity_side,
    matches_neg_binary_call, matches_sub_right_identity_side, matches_unary_call,
};

pub(super) fn emit_binary_wrapper_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    let op = int_binary_wrapper_op(ctx, &vb.fn_name);
    let fn_name = &vb.fn_name;
    let fn_lean = aver_name_to_lean(fn_name);

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
            intro_names,
            vec![format!("simp [{}, {}]", fn_lean, op_lemma)],
        ));
    }

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
            intro_names,
            vec![format!("simp [{}, {}]", fn_lean, assoc_lemma)],
        ));
    }

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
                return Some(intro_then(intro_names, vec![format!("simp [{}]", fn_lean)]));
            }
        }
    }

    let op = op.as_ref()?;
    emit_sub_wrapper_law(vb, law, intro_names, op, &fn_lean)
}

pub(super) fn emit_unary_wrapper_equivalence_law(
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

    let try_side = |call_side: &Spanned<Expr>, other_side: &Spanned<Expr>| -> Option<Vec<String>> {
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

    if law.givens.len() == 1 && law.givens[0].type_name == "Int" {
        let g = &law.givens[0].name;
        let right_id = matches_sub_right_identity_side(&law.lhs, &law.rhs, fn_name, g)
            || matches_sub_right_identity_side(&law.rhs, &law.lhs, fn_name, g);
        if right_id {
            return Some(intro_then(intro_names, vec![format!("simp [{}]", fn_lean)]));
        }
    }

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
        let a = aver_name_to_lean(&law.givens[0].name);
        let b = aver_name_to_lean(&law.givens[1].name);
        let step =
            if matches_binary_call(&law.lhs, fn_name, &law.givens[0].name, &law.givens[1].name)
                && matches_neg_binary_call(
                    &law.rhs,
                    fn_name,
                    &law.givens[1].name,
                    &law.givens[0].name,
                )
            {
                format!("simpa [{}] using (Int.neg_sub {} {}).symm", fn_lean, b, a)
            } else {
                format!("simpa [{}] using (Int.neg_sub {} {})", fn_lean, b, a)
            };
        return Some(intro_then(intro_names, vec![step]));
    }

    None
}

#[derive(Clone, Debug, PartialEq)]
struct UnaryIntWrapper {
    op: BinOp,
    constant: i64,
    var_first: bool,
}

fn int_binary_wrapper_op(ctx: &CodegenContext, fn_name: &str) -> Option<BinOp> {
    let fd = find_fn_def_by_call_name(ctx, fn_name)?;
    if fd.params.len() != 2 || fd.return_type != "Int" {
        return None;
    }
    let (p1, t1) = &fd.params[0];
    let (p2, t2) = &fd.params[1];
    if t1 != "Int" || t2 != "Int" {
        return None;
    }
    let expr = body_terminal_expr(fd.body.as_ref())?;
    let Expr::BinOp(op, left, right) = &expr.node else {
        return None;
    };
    if !matches_ident(left, p1) || !matches_ident(right, p2) {
        return None;
    }
    Some(*op)
}

fn int_unary_wrapper(ctx: &CodegenContext, fn_name: &str) -> Option<UnaryIntWrapper> {
    let fd = find_fn_def_by_call_name(ctx, fn_name)?;
    if fd.params.len() != 1 || fd.return_type != "Int" {
        return None;
    }
    let (param, param_ty) = &fd.params[0];
    if param_ty != "Int" {
        return None;
    }
    let expr = body_terminal_expr(fd.body.as_ref())?;
    let Expr::BinOp(op, left, right) = &expr.node else {
        return None;
    };

    match (&left.node, &right.node) {
        (Expr::Ident(id) | Expr::Resolved { name: id, .. }, Expr::Literal(Literal::Int(n)))
            if id == param =>
        {
            Some(UnaryIntWrapper {
                op: *op,
                constant: *n,
                var_first: true,
            })
        }
        (Expr::Literal(Literal::Int(n)), Expr::Ident(id) | Expr::Resolved { name: id, .. })
            if id == param =>
        {
            Some(UnaryIntWrapper {
                op: *op,
                constant: *n,
                var_first: false,
            })
        }
        _ => None,
    }
}
