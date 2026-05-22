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
    /// Same shape as `IntCountdown` but body is `match p { 0 -> BASE; _ -> rec(p-1, ...) }`
    /// and the fn is closed-world: every external callsite either passes a
    /// non-negative literal or sits in a guard branch that proves `p ≥ 0`.
    /// Proof backends emit a native def with an injected `p ≥ 0` precondition
    /// plus a wrapper handling the `p < 0` case from the source's `0` arm, skipping
    /// fuel entirely so Lean can `decide` / unfold for symbolic proofs. Carries
    /// both arm bodies so the Lean emitter can switch the elaborated shape to
    /// `if h_zero : p = 0 then BASE else REC` — needed because Lean's `match`
    /// elaborator does not expose the case-split hypothesis to `omega` for the
    /// recursive callsite's `p - 1 ≥ 0` discharge.
    IntCountdownGuarded {
        param_index: usize,
        /// Literal int from the body's literal-match arm — the value the
        /// `if h_zero : p = L then base else rec(p-1, ...)` aux splits on.
        base_arm_literal: i64,
        base_arm_body: Spanned<Expr>,
        wildcard_arm_body: Spanned<Expr>,
        /// Path-constraint chain extracted from the single external
        /// caller's surrounding `match (Bool) { true/false -> CALL ... }`
        /// (and `if/then/else`) guards. Already normalised to positive
        /// form by flipping the comparison BinOp on `false`-arm guards
        /// (`Lt ↔ Gte`, `Gt ↔ Lte`, `Eq ↔ Neq`), so every clause is a
        /// plain Aver Bool expression. Substituted into callee's
        /// variable space (caller's arg-binding renamed to callee's
        /// param name). Conjunction of all clauses is the aux's
        /// precondition.
        ///
        /// Empty means "no single external caller in `ctx`": the Lean
        /// emitter falls back to `(h_dom : p ≥ 0)` so a free-standing
        /// fibTR-shape (no caller in the proof artifact) keeps the
        /// legacy precondition. Same `Spanned<Expr>`-as-predicate
        /// representation as opaque types' smart-constructor predicate
        /// (`refinement_info_for`) and verify-law `when` clauses — the
        /// three sources differ only in where the predicate comes from.
        precondition: Vec<Spanned<Expr>>,
    },
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

/// Suffix for the native-emit auxiliary fn that carries the explicit
/// precondition parameter. Mirrors [`fuel_helper_name`]'s ASCII-only
/// shape so Lean accepts it verbatim.
pub fn native_aux_name(name: &str) -> String {
    format!("{}__aux", name)
}

/// Sentinel identifier injected as an extra synthetic argument at every
/// recursive callsite inside an `IntCountdownGuarded` body. Lean's expr
/// emitter recognises this name and renders it as `(by omega)`; Dafny's
/// codegen never sees it because Dafny discharges preconditions via
/// auto-inference at the existing fn-def emit path.
pub const OMEGA_PROOF_SENTINEL: &str = "__aver_omega_proof__";

/// Flip a comparison `BinOp` to its logical negation so a caller's
/// `match (a OP b) { false -> ... }` arm normalises into a positive
/// predicate. Returns `None` for non-comparison operators (`Add`,
/// `Sub`, etc.) — those can't legally land as a `match … { Bool ->
/// ... }` subject anyway. Used by the issue-84 caller-guard extractor
/// so every collected predicate is the same `Spanned<Expr>` shape
/// opaque types and verify `when` already use, avoiding a parallel
/// `(expr, negated)` representation.
pub fn flip_comparison_binop(expr: &Spanned<Expr>) -> Option<Spanned<Expr>> {
    let Expr::BinOp(op, left, right) = &expr.node else {
        return None;
    };
    use crate::ast::BinOp::*;
    let flipped = match op {
        Lt => Gte,
        Gt => Lte,
        Lte => Gt,
        Gte => Lt,
        Eq => Neq,
        Neq => Eq,
        _ => return None,
    };
    Some(Spanned::new(
        Expr::BinOp(flipped, left.clone(), right.clone()),
        expr.line,
    ))
}

// Re-export `substitute_ident_in_expr` from `codegen::common` so the
// recursion module's public API doesn't lose the symbol existing
// callers (issue 84 caller-guard walker, etc.) reach for. The
// definition lives in common because three predicate sources (opaque
// constructor, caller guard, verify `when`-redundancy check) all use
// the same substitution.
pub use crate::codegen::common::substitute_ident_in_expr;

/// True iff `expr` (recursively) mentions `name` as an `Ident`/
/// `Resolved` reference. Used by the caller-guard extractor to filter
/// out enclosing predicates that don't constrain the variable passed
/// at the countdown-param position — those predicates name caller
/// locals that aren't in scope inside the callee.
pub fn expr_references_ident(expr: &Spanned<Expr>, name: &str) -> bool {
    match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n == name,
        Expr::Literal(_) => false,
        Expr::Attr(obj, _) => expr_references_ident(obj, name),
        Expr::FnCall(callee, args) => {
            expr_references_ident(callee, name)
                || args.iter().any(|a| expr_references_ident(a, name))
        }
        Expr::BinOp(_, l, r) => expr_references_ident(l, name) || expr_references_ident(r, name),
        Expr::Neg(inner) => expr_references_ident(inner, name),
        Expr::Match { subject, arms } => {
            expr_references_ident(subject, name)
                || arms.iter().any(|a| expr_references_ident(&a.body, name))
        }
        Expr::Constructor(_, arg) => arg
            .as_deref()
            .is_some_and(|a| expr_references_ident(a, name)),
        Expr::ErrorProp(inner) => expr_references_ident(inner, name),
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(inner) => expr_references_ident(inner, name),
            _ => false,
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(|i| expr_references_ident(i, name))
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(k, v)| expr_references_ident(k, name) || expr_references_ident(v, name)),
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, v)| expr_references_ident(v, name))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            expr_references_ident(base, name)
                || updates.iter().any(|(_, v)| expr_references_ident(v, name))
        }
        Expr::TailCall(boxed) => boxed.args.iter().any(|a| expr_references_ident(a, name)),
    }
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
        Expr::Neg(inner) => Expr::Neg(Box::new(rewrite_recursive_calls_expr(
            inner, targets, fuel_var,
        ))),
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(rewrite_recursive_calls_expr(subject, targets, fuel_var)),
            arms: arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(rewrite_recursive_calls_expr(&arm.body, targets, fuel_var)),
                    binding_slots: std::sync::OnceLock::new(),
                })
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

/// Walk `expr` and rewrite every recursive call to `fn_name` into a
/// call to `aux_name` carrying an extra `OMEGA_PROOF_SENTINEL` ident
/// at the end of the argument list. Used by the proof-mode
/// `IntCountdownGuarded` lowering — the synthetic argument lets Lean
/// discharge the precondition at every recursive site without
/// touching the original Aver source.
pub fn rewrite_native_guarded_calls_expr(
    expr: &Spanned<Expr>,
    fn_name: &str,
    aux_name: &str,
) -> Spanned<Expr> {
    let line = expr.line;
    let new_node = match &expr.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => return expr.clone(),
        Expr::Attr(obj, field) => Expr::Attr(
            Box::new(rewrite_native_guarded_calls_expr(obj, fn_name, aux_name)),
            field.clone(),
        ),
        Expr::FnCall(callee, args) => {
            let rewritten_args: Vec<Spanned<Expr>> = args
                .iter()
                .map(|arg| rewrite_native_guarded_calls_expr(arg, fn_name, aux_name))
                .collect();
            if let Some(name) = expr_to_dotted_name(&callee.node)
                && name == fn_name
            {
                let mut call_args = rewritten_args;
                call_args.push(Spanned::new(
                    Expr::Ident(OMEGA_PROOF_SENTINEL.to_string()),
                    line,
                ));
                Expr::FnCall(
                    Box::new(Spanned::new(Expr::Ident(aux_name.to_string()), line)),
                    call_args,
                )
            } else {
                Expr::FnCall(
                    Box::new(rewrite_native_guarded_calls_expr(callee, fn_name, aux_name)),
                    rewritten_args,
                )
            }
        }
        Expr::BinOp(op, left, right) => Expr::BinOp(
            *op,
            Box::new(rewrite_native_guarded_calls_expr(left, fn_name, aux_name)),
            Box::new(rewrite_native_guarded_calls_expr(right, fn_name, aux_name)),
        ),
        Expr::Neg(inner) => Expr::Neg(Box::new(rewrite_native_guarded_calls_expr(
            inner, fn_name, aux_name,
        ))),
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(rewrite_native_guarded_calls_expr(
                subject, fn_name, aux_name,
            )),
            arms: arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(rewrite_native_guarded_calls_expr(
                        &arm.body, fn_name, aux_name,
                    )),
                    binding_slots: std::sync::OnceLock::new(),
                })
                .collect(),
        },
        Expr::Constructor(name, arg) => Expr::Constructor(
            name.clone(),
            arg.as_ref()
                .map(|inner| Box::new(rewrite_native_guarded_calls_expr(inner, fn_name, aux_name))),
        ),
        Expr::ErrorProp(inner) => Expr::ErrorProp(Box::new(rewrite_native_guarded_calls_expr(
            inner, fn_name, aux_name,
        ))),
        Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
            parts
                .iter()
                .map(|part| match part {
                    StrPart::Literal(_) => part.clone(),
                    StrPart::Parsed(inner) => StrPart::Parsed(Box::new(
                        rewrite_native_guarded_calls_expr(inner, fn_name, aux_name),
                    )),
                })
                .collect(),
        ),
        Expr::List(items) => Expr::List(
            items
                .iter()
                .map(|item| rewrite_native_guarded_calls_expr(item, fn_name, aux_name))
                .collect(),
        ),
        Expr::Tuple(items) => Expr::Tuple(
            items
                .iter()
                .map(|item| rewrite_native_guarded_calls_expr(item, fn_name, aux_name))
                .collect(),
        ),
        Expr::IndependentProduct(items, flag) => Expr::IndependentProduct(
            items
                .iter()
                .map(|item| rewrite_native_guarded_calls_expr(item, fn_name, aux_name))
                .collect(),
            *flag,
        ),
        Expr::MapLiteral(entries) => Expr::MapLiteral(
            entries
                .iter()
                .map(|(k, v)| {
                    (
                        rewrite_native_guarded_calls_expr(k, fn_name, aux_name),
                        rewrite_native_guarded_calls_expr(v, fn_name, aux_name),
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
                        rewrite_native_guarded_calls_expr(value, fn_name, aux_name),
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
            base: Box::new(rewrite_native_guarded_calls_expr(base, fn_name, aux_name)),
            updates: updates
                .iter()
                .map(|(name, value)| {
                    (
                        name.clone(),
                        rewrite_native_guarded_calls_expr(value, fn_name, aux_name),
                    )
                })
                .collect(),
        },
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            let rewritten_args: Vec<Spanned<Expr>> = args
                .iter()
                .map(|arg| rewrite_native_guarded_calls_expr(arg, fn_name, aux_name))
                .collect();
            if target == fn_name {
                let mut call_args = rewritten_args;
                call_args.push(Spanned::new(
                    Expr::Ident(OMEGA_PROOF_SENTINEL.to_string()),
                    line,
                ));
                Expr::FnCall(
                    Box::new(Spanned::new(Expr::Ident(aux_name.to_string()), line)),
                    call_args,
                )
            } else {
                Expr::TailCall(Box::new(TailCallData::new(target.clone(), rewritten_args)))
            }
        }
    };
    Spanned::new(new_node, line)
}

/// Body-level wrapper around [`rewrite_native_guarded_calls_expr`].
pub fn rewrite_native_guarded_calls_body(body: &FnBody, fn_name: &str, aux_name: &str) -> FnBody {
    FnBody::Block(
        body.stmts()
            .iter()
            .map(|stmt| match stmt {
                Stmt::Binding(name, ty, expr) => Stmt::Binding(
                    name.clone(),
                    ty.clone(),
                    rewrite_native_guarded_calls_expr(expr, fn_name, aux_name),
                ),
                Stmt::Expr(expr) => {
                    Stmt::Expr(rewrite_native_guarded_calls_expr(expr, fn_name, aux_name))
                }
            })
            .collect(),
    )
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
