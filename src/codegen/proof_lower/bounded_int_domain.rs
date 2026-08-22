//! Lower finite guarded-`Int` laws into a backend-neutral proof plan.
//!
//! The source shape is one `Int` given, a `Bool.and` guard spelling a
//! half-open literal interval, and a unary Bool subject asserted `true`.
//! Declaration selection and the non-recursive-cone gate use `FnId`; backends
//! receive the validated bounds and subject identity without rescanning AST.

use std::collections::BTreeSet;

use crate::ast::{BinOp, Expr, Literal, Spanned, VerifyLaw};
use crate::codegen::common::expr_to_dotted_name;
use crate::ir::FnId;

use super::{ProofLowerInputs, collect_fn_calls_expr};

pub(super) struct Plan {
    pub var: String,
    pub lo: i64,
    pub hi: i64,
    pub subject: FnId,
}

fn int_literal(expr: &Spanned<Expr>) -> Option<i64> {
    match &expr.node {
        Expr::Literal(Literal::Int(value)) => Some(*value),
        Expr::Neg(inner) => match &inner.node {
            Expr::Literal(Literal::Int(value)) => Some(-*value),
            _ => None,
        },
        _ => None,
    }
}

/// Return `(is_upper, bound)` for `LO <= var`, `var >= LO`, `var < HI`,
/// or `HI > var`.
fn bound(expr: &Spanned<Expr>, var: &str) -> Option<(bool, i64)> {
    let Expr::BinOp(op, left, right) = &expr.node else {
        return None;
    };
    let left_is_var = expr_to_dotted_name(&left.node).as_deref() == Some(var);
    let right_is_var = expr_to_dotted_name(&right.node).as_deref() == Some(var);
    match op {
        BinOp::Lte if right_is_var => int_literal(left).map(|value| (false, value)),
        BinOp::Gte if left_is_var => int_literal(right).map(|value| (false, value)),
        BinOp::Lt if left_is_var => int_literal(right).map(|value| (true, value)),
        BinOp::Gt if right_is_var => int_literal(left).map(|value| (true, value)),
        _ => None,
    }
}

fn insert_pure_callee(
    name: &str,
    scope: Option<&str>,
    inputs: &ProofLowerInputs<'_>,
    cone: &mut BTreeSet<FnId>,
) {
    let Some(id) = inputs.symbol_table.resolve_fn_id_in(name, scope) else {
        return;
    };
    let Some(fd) = inputs.find_fn_def_by_id(id) else {
        return;
    };
    if crate::codegen::common::is_pure_fn(fd) {
        cone.insert(id);
    }
}

fn cone_is_non_recursive(target: FnId, subject: FnId, inputs: &ProofLowerInputs<'_>) -> bool {
    let mut cone = BTreeSet::from([target, subject]);
    loop {
        let before = cone.len();
        let snapshot: Vec<FnId> = cone.iter().copied().collect();
        for id in snapshot {
            let Some(fd) = inputs.find_fn_def_by_id(id) else {
                return false;
            };
            if !crate::codegen::common::is_pure_fn(fd) {
                continue;
            }
            let owner_scope = inputs.fn_owning_scope(fd);
            let mut called = BTreeSet::new();
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, expr) | crate::ast::Stmt::Expr(expr) => {
                        collect_fn_calls_expr(expr, &mut called);
                    }
                }
            }
            for name in called {
                insert_pure_callee(&name, owner_scope, inputs, &mut cone);
            }
        }
        if cone.len() == before {
            break;
        }
    }
    !cone.iter().any(|id| inputs.recursive_fns.contains(id))
}

pub(super) fn detect(
    law: &VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs<'_>,
    scope: Option<&str>,
) -> Option<Plan> {
    let [given] = law.givens.as_slice() else {
        return None;
    };
    if given.type_name.trim() != "Int"
        || !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true)))
    {
        return None;
    }
    let var = given.name.clone();

    let Expr::FnCall(callee, args) = &law.lhs.node else {
        return None;
    };
    if args.len() != 1 || expr_to_dotted_name(&args[0].node).as_deref() != Some(var.as_str()) {
        return None;
    }
    let subject_name = expr_to_dotted_name(&callee.node)?;
    let subject = inputs.symbol_table.resolve_fn_id_in(&subject_name, scope)?;
    let subject_def = inputs.find_fn_def_by_id(subject)?;
    if !crate::codegen::common::is_pure_fn(subject_def) || subject_def.return_type.trim() != "Bool"
    {
        return None;
    }

    let when = law.when.as_ref()?;
    let Expr::FnCall(guard, conjuncts) = &when.node else {
        return None;
    };
    if expr_to_dotted_name(&guard.node).as_deref() != Some("Bool.and") || conjuncts.len() != 2 {
        return None;
    }
    let mut lo = None;
    let mut hi = None;
    for conjunct in conjuncts {
        match bound(conjunct, &var)? {
            (false, value) => lo = Some(value),
            (true, value) => hi = Some(value),
        }
    }
    let (lo, hi) = (lo?, hi?);
    let width = hi.checked_sub(lo)?;
    if width <= 0 || width > 4096 {
        return None;
    }

    let target = inputs.symbol_table.resolve_fn_id_in(fn_name, scope)?;
    cone_is_non_recursive(target, subject, inputs).then_some(Plan {
        var,
        lo,
        hi,
        subject,
    })
}
