//! Refinement-via-opaque lift: carrier discovery, smart-constructor
//! and guards.
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

/// Backend-neutral analogue of `codegen::common::refinement_lift_
/// for_given`. Walks `lhs` / `rhs` looking for a `RecordCreate {
/// type_name, fields: [(_, Ident(given))] }` shape where `type_
/// name` is a refined type whose carrier matches the given's
/// declared type. Returns the refined type name on first match.
///
/// The legacy version (common.rs) takes `&CodegenContext` and
/// borrows the type name from `ctx.items`. The lowerer reads
/// `refined_types` directly off the in-progress `ProofIR`
/// (populated by `populate_refined_types`, which runs before
/// `populate_law_theorems` in `lower(...)`).
pub(super) fn refinement_lift_for_given_ir(
    given_name: &str,
    lhs: &Spanned<crate::ast::Expr>,
    rhs: &Spanned<crate::ast::Expr>,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
    symbols: &crate::ir::SymbolTable,
    dep_modules: &[crate::codegen::ModuleInfo],
) -> Option<String> {
    let mut result: Option<String> = None;
    walk_for_refinement_carrier(
        lhs,
        given_name,
        refined_types,
        symbols,
        dep_modules,
        &mut result,
    );
    walk_for_refinement_carrier(
        rhs,
        given_name,
        refined_types,
        symbols,
        dep_modules,
        &mut result,
    );
    result
}

/// **syntax-discovery-only** (epic #170 Phase 7). Walks raw AST
/// looking for a `RecordCreate(type_name, [(field, Ident(given))])`
/// pattern that lifts a `given` through a refinement type's smart
/// constructor. The recursion descends into nested record-creates,
/// fn calls, and binops so a deeply-wrapped lift still gets found.
/// Identity is handed off to `resolve_refined_type_in_with_key`,
/// which canonicalises through `SymbolTable` before keying the
/// `refined_types` map — no bare-name keying past discovery.
pub(super) fn walk_for_refinement_carrier(
    expr: &Spanned<crate::ast::Expr>,
    given_name: &str,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
    symbols: &crate::ir::SymbolTable,
    dep_modules: &[crate::codegen::ModuleInfo],
    result: &mut Option<String>,
) {
    use crate::ast::Expr;
    if result.is_some() {
        return;
    }
    match &expr.node {
        Expr::RecordCreate { type_name, fields } if fields.len() == 1 => {
            let (_, fvalue) = &fields[0];
            let matches_var = matches!(
                &fvalue.node,
                Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == given_name
            );
            if matches_var
                && let Some((type_id, _decl)) =
                    crate::codegen::common::resolve_refined_type_in_with_key(
                        refined_types,
                        symbols,
                        dep_modules,
                        type_name,
                    )
            {
                // Stringify the canonical name via the symbol table's
                // type entry. The only consumer today reads `.is_some()`
                // (see `detect_simp_omega_unfold`), but recovering a
                // human-readable id keeps the diagnostic path honest.
                *result = Some(symbols.type_entry(type_id).key.canonical());
                return;
            }
            // Even non-matching RecordCreate may contain nested
            // refinement carriers (e.g. `Foo(value = Bar(value = a))`).
            for (_, v) in fields {
                walk_for_refinement_carrier(
                    v,
                    given_name,
                    refined_types,
                    symbols,
                    dep_modules,
                    result,
                );
            }
        }
        Expr::FnCall(callee, args) => {
            walk_for_refinement_carrier(
                callee,
                given_name,
                refined_types,
                symbols,
                dep_modules,
                result,
            );
            for a in args {
                walk_for_refinement_carrier(
                    a,
                    given_name,
                    refined_types,
                    symbols,
                    dep_modules,
                    result,
                );
            }
        }
        Expr::BinOp(_, l, r) => {
            walk_for_refinement_carrier(l, given_name, refined_types, symbols, dep_modules, result);
            walk_for_refinement_carrier(r, given_name, refined_types, symbols, dep_modules, result);
        }
        Expr::Match { subject, arms, .. } => {
            walk_for_refinement_carrier(
                subject,
                given_name,
                refined_types,
                symbols,
                dep_modules,
                result,
            );
            for arm in arms {
                walk_for_refinement_carrier(
                    &arm.body,
                    given_name,
                    refined_types,
                    symbols,
                    dep_modules,
                    result,
                );
            }
        }
        Expr::Attr(obj, _) => {
            walk_for_refinement_carrier(
                obj,
                given_name,
                refined_types,
                symbols,
                dep_modules,
                result,
            );
        }
        _ => {}
    }
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
