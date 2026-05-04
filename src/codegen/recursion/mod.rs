//! Shared proof-mode recursion analysis.
//!
//! Classifies each recursive pure fn into a [`RecursionPlan`] that tells
//! the proof backends (Lean, Dafny) how to emit a fuel-guarded helper
//! plus a wrapper with an appropriate fuel metric. The same classifier
//! feeds both backends so supported shapes stay consistent.
//!
//! Emission is backend-specific (syntax, termination-proof mechanism,
//! default-value for fuel exhaustion), but the recognition pass and the
//! AST transform that rewrites recursive calls into helper calls are
//! shared.

pub mod detect;

use std::collections::HashSet;

use crate::ast::{Expr, FnBody, MatchArm, Spanned, Stmt, StrPart, TailCallData};
use crate::codegen::common::expr_to_dotted_name;

pub use detect::analyze_plans;

/// Classification for a single recursive fn (or a whole mutual-recursion
/// SCC, in which case every fn in the SCC gets its own plan from the
/// same family).
///
/// `Eq` is deliberately omitted — `IntAscending` holds an AST expression
/// which only implements `PartialEq` (float literals inside it are
/// partially ordered). `PartialEq` still works for the uses this enum
/// sees (pattern matching, `matches!`, equality via `.eq`).
#[derive(Clone, Debug, PartialEq)]
pub enum RecursionPlan {
    /// Single-fn recursion where an `Int` parameter decreases by 1.
    /// The wrapper supplies `n.natAbs + 1` fuel so the helper terminates.
    IntCountdown { param_index: usize },
    /// Single-fn recursion where an `Int` param increases by 1 up to a
    /// bound. The bound is kept as an Aver AST expression so each
    /// backend renders it in its own idiom; the wrapper supplies
    /// `(bound - n).natAbs + 1` fuel.
    IntAscending {
        param_index: usize,
        bound: Spanned<Expr>,
    },
    /// Affine second-order recurrence like `fib(n) = fib(n-1) + fib(n-2)`
    /// with `0 / 1` bases and an `n < 0` guard. Emitted through a
    /// private Nat helper (pair-state), not a fuel helper.
    LinearRecurrence2,
    /// Single-fn structural recursion on a `List<_>` parameter; proof
    /// backends emit as structural recursion directly (no fuel).
    ListStructural { param_index: usize },
    /// Single-fn structural recursion on a recursive user ADT; proof
    /// backends emit through a sizeOf-guarded fuel helper.
    SizeOfStructural,
    /// Single-fn recursion where the first `String` is preserved and
    /// the second `Int` position parameter strictly advances (`pos +
    /// k`, k ≥ 1). Wrapper fuel is derived from `s.length - pos`.
    StringPosAdvance,
    /// Mutual recursion SCC where the first `Int` parameter decreases
    /// by 1 across every inter-fn call.
    MutualIntCountdown,
    /// Mutual recursion SCC where the first `String` is preserved and
    /// the second `Int` either advances or stays the same across
    /// rank-decreasing edges.
    MutualStringPosAdvance { rank: usize },
    /// Generic mutual recursion SCC using `sizeOf` on structural
    /// parameters plus rank for same-measure edges.
    MutualSizeOfRanked { rank: usize },
}

/// A diagnostic surfaced when a recursive fn falls outside the supported
/// patterns. Proof backends translate this into a warning and emit the
/// fn through a partial/axiom fallback (or skip it entirely).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProofModeIssue {
    pub line: usize,
    pub message: String,
}

/// Canonical suffix for a fuel-guarded helper fn. Deliberately contains
/// only lowercase ASCII + underscores so both Lean and Dafny accept it
/// as an identifier without renaming.
pub fn fuel_helper_name(name: &str) -> String {
    format!("{}__fuel", name)
}

/// AST transform: walk `expr` and replace every recursive call to a fn
/// in `targets` with `fn__fuel(fuel_var, …args)`. Inter-fn mutual calls
/// in the same SCC are rewritten identically (the fuel parameter is
/// threaded through the whole group).
pub fn rewrite_recursive_calls_expr(
    expr: &Spanned<Expr>,
    targets: &HashSet<String>,
    fuel_var: &str,
) -> Spanned<Expr> {
    let line = expr.line;
    let new_node = match &expr.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => return expr.clone(),
        Expr::Attr(obj, field) => Expr::Attr(
            Box::new(rewrite_recursive_calls_expr(obj, targets, fuel_var)),
            field.clone(),
        ),
        Expr::FnCall(callee, args) => {
            let rewritten_args: Vec<Spanned<Expr>> = args
                .iter()
                .map(|arg| rewrite_recursive_calls_expr(arg, targets, fuel_var))
                .collect();
            if let Some(name) = expr_to_dotted_name(&callee.node)
                && targets.contains(&name)
            {
                let mut call_args = Vec::with_capacity(rewritten_args.len() + 1);
                call_args.push(Spanned::new(Expr::Ident(fuel_var.to_string()), line));
                call_args.extend(rewritten_args);
                Expr::FnCall(
                    Box::new(Spanned::new(Expr::Ident(fuel_helper_name(&name)), line)),
                    call_args,
                )
            } else {
                Expr::FnCall(
                    Box::new(rewrite_recursive_calls_expr(callee, targets, fuel_var)),
                    rewritten_args,
                )
            }
        }
        Expr::BinOp(op, left, right) => Expr::BinOp(
            *op,
            Box::new(rewrite_recursive_calls_expr(left, targets, fuel_var)),
            Box::new(rewrite_recursive_calls_expr(right, targets, fuel_var)),
        ),
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(rewrite_recursive_calls_expr(subject, targets, fuel_var)),
            arms: arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(rewrite_recursive_calls_expr(&arm.body, targets, fuel_var)), binding_slots: std::sync::OnceLock::new() })
                .collect(),
        },
        Expr::Constructor(name, arg) => Expr::Constructor(
            name.clone(),
            arg.as_ref()
                .map(|inner| Box::new(rewrite_recursive_calls_expr(inner, targets, fuel_var))),
        ),
        Expr::ErrorProp(inner) => Expr::ErrorProp(Box::new(rewrite_recursive_calls_expr(
            inner, targets, fuel_var,
        ))),
        Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
            parts
                .iter()
                .map(|part| match part {
                    StrPart::Literal(_) => part.clone(),
                    StrPart::Parsed(inner) => StrPart::Parsed(Box::new(
                        rewrite_recursive_calls_expr(inner, targets, fuel_var),
                    )),
                })
                .collect(),
        ),
        Expr::List(items) => Expr::List(
            items
                .iter()
                .map(|item| rewrite_recursive_calls_expr(item, targets, fuel_var))
                .collect(),
        ),
        Expr::Tuple(items) => Expr::Tuple(
            items
                .iter()
                .map(|item| rewrite_recursive_calls_expr(item, targets, fuel_var))
                .collect(),
        ),
        Expr::IndependentProduct(items, flag) => Expr::IndependentProduct(
            items
                .iter()
                .map(|item| rewrite_recursive_calls_expr(item, targets, fuel_var))
                .collect(),
            *flag,
        ),
        Expr::MapLiteral(entries) => Expr::MapLiteral(
            entries
                .iter()
                .map(|(k, v)| {
                    (
                        rewrite_recursive_calls_expr(k, targets, fuel_var),
                        rewrite_recursive_calls_expr(v, targets, fuel_var),
                    )
                })
                .collect(),
        ),
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| {
                    (
                        name.clone(),
                        rewrite_recursive_calls_expr(value, targets, fuel_var),
                    )
                })
                .collect(),
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(rewrite_recursive_calls_expr(base, targets, fuel_var)),
            updates: updates
                .iter()
                .map(|(name, value)| {
                    (
                        name.clone(),
                        rewrite_recursive_calls_expr(value, targets, fuel_var),
                    )
                })
                .collect(),
        },
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            let rewritten_args: Vec<Spanned<Expr>> = args
                .iter()
                .map(|arg| rewrite_recursive_calls_expr(arg, targets, fuel_var))
                .collect();
            if targets.contains(target) {
                let mut call_args = Vec::with_capacity(rewritten_args.len() + 1);
                call_args.push(Spanned::new(Expr::Ident(fuel_var.to_string()), line));
                call_args.extend(rewritten_args);
                Expr::FnCall(
                    Box::new(Spanned::new(Expr::Ident(fuel_helper_name(target)), line)),
                    call_args,
                )
            } else {
                Expr::TailCall(Box::new(TailCallData::new(target.clone(), rewritten_args)))
            }
        }
    };
    Spanned::new(new_node, line)
}

/// Body-level wrapper around [`rewrite_recursive_calls_expr`].
pub fn rewrite_recursive_calls_body(
    body: &FnBody,
    targets: &HashSet<String>,
    fuel_var: &str,
) -> FnBody {
    FnBody::Block(
        body.stmts()
            .iter()
            .map(|stmt| match stmt {
                Stmt::Binding(name, ty, expr) => Stmt::Binding(
                    name.clone(),
                    ty.clone(),
                    rewrite_recursive_calls_expr(expr, targets, fuel_var),
                ),
                Stmt::Expr(expr) => {
                    Stmt::Expr(rewrite_recursive_calls_expr(expr, targets, fuel_var))
                }
            })
            .collect(),
    )
}
