//! Refinement-via-opaque: supported carriers and smart-constructor
//! guards.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

/// Carrier shapes whose smart-constructor invariant can be represented
/// faithfully by both proof backends today.
///
/// `Int` is the original scalar path. Structural containers and named
/// carriers add the two missing pieces needed by `Bytes` (`List<Int>`) and
/// nested refinements such as `Digest32` (`Bytes`). Float and String keep
/// their existing plain-record path: their proof models and automation have
/// separate, documented limitations that this container lift must not change
/// accidentally.
pub(super) fn proof_subtype_carrier_supported(carrier_type: &str) -> bool {
    matches!(
        crate::types::parse_type_str(carrier_type),
        crate::types::Type::Int
            | crate::types::Type::List(_)
            | crate::types::Type::Vector(_)
            | crate::types::Type::Map(_, _)
            | crate::types::Type::Result(_, _)
            | crate::types::Type::Option(_)
            | crate::types::Type::Tuple(_)
            | crate::types::Type::Named { .. }
    )
}

/// Find a single-param smart constructor in the unfold set whose
/// body is the canonical `match <bool-subj> { true → Ok; false →
/// Err }` shape. Returns the param name + bool subject of the
/// first match.
pub(super) fn extract_smart_constructor_guard(
    fn_names: &std::collections::BTreeSet<String>,
    inputs: &ProofLowerInputs,
) -> Option<crate::ir::SmartGuard> {
    use crate::ast::{Expr, MatchArm, Pattern, Stmt};
    for fd in iter_all_fn_defs(inputs) {
        if !fn_names.contains(&fd.name) {
            continue;
        }
        if !fd.return_type.starts_with("Result<") {
            continue;
        }
        if fd.params.len() != 1 {
            continue;
        }
        let (param_name, param_type) = &fd.params[0];
        if param_type != "Int" {
            continue;
        }
        let stmts = fd.body.stmts();
        if stmts.len() != 1 {
            continue;
        }
        let Stmt::Expr(body_expr) = &stmts[0] else {
            continue;
        };
        let Expr::Match { subject, arms } = &body_expr.node else {
            continue;
        };
        if !arms_match_bool_ok_err(arms) {
            continue;
        }
        let scope = inputs.fn_owning_scope(fd);
        return Some(crate::ir::SmartGuard {
            param: param_name.clone(),
            predicate: inputs.resolve_expr(subject, scope),
        });
        // Reference the type to satisfy the MatchArm import.
        #[allow(unreachable_code)]
        {
            let _: Option<&MatchArm> = None;
            let _: Option<&Pattern> = None;
        }
    }
    None
}

pub(super) fn arms_match_bool_ok_err(arms: &[crate::ast::MatchArm]) -> bool {
    use crate::ast::{Expr, Literal, Pattern};
    if arms.len() != 2 {
        return false;
    }
    let starts_with_ctor = |expr: &Spanned<Expr>, name: &str| -> bool {
        match &expr.node {
            Expr::Constructor(n, _) => n == name,
            Expr::FnCall(callee, _) => {
                if let Expr::Attr(obj, field) = &callee.node
                    && let Expr::Ident(ns) = &obj.node
                {
                    format!("{ns}.{field}") == name
                } else {
                    false
                }
            }
            _ => false,
        }
    };
    let mut saw_true_ok = false;
    let mut saw_false_err = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                if starts_with_ctor(&arm.body, "Result.Ok") {
                    saw_true_ok = true;
                }
            }
            Pattern::Literal(Literal::Bool(false)) => {
                if starts_with_ctor(&arm.body, "Result.Err") {
                    saw_false_err = true;
                }
            }
            _ => return false,
        }
    }
    saw_true_ok && saw_false_err
}
