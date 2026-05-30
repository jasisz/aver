//! HIR → MIR lowering, waves 1 + 2.
//!
//! Phase 3 of #252 lowers `ResolvedProgramView` into `MirProgram`
//! in widening waves so review surface stays small.
//!
//! **Wave 1** — leaf subset, no callee resolver, no patterns:
//! - `ResolvedExpr::Literal` → `MirExpr::Literal`
//! - `ResolvedExpr::Resolved { slot, .. }` → `MirExpr::Local`
//! - `ResolvedExpr::BinOp` → `MirExpr::BinOp`
//! - `ResolvedExpr::Neg` → `MirExpr::Neg`
//!
//! **Wave 2 (added in this commit)** — call shapes + product types:
//! - `ResolvedExpr::Call(Fn, args)` → `MirExpr::Call` w/ `MirCallee::Fn(FnId)`
//! - `ResolvedExpr::Call(Builtin, args)` → `MirExpr::Call` w/ `MirCallee::Builtin(name)`
//! - `ResolvedExpr::Ctor(User { ctor_id, .. }, args)` → `MirExpr::Construct`
//! - `ResolvedExpr::RecordCreate { type_id: Some(_), … }` → `MirExpr::RecordCreate`
//! - `ResolvedExpr::RecordUpdate { type_id: Some(_), … }` → `MirExpr::RecordUpdate`
//! - `ResolvedExpr::Attr(base, field)` → `MirExpr::Project`
//!
//! **Wave 3a (this commit)** — multi-stmt bodies via `Let` chains:
//! - `Stmt::Binding { name, value }` + `Stmt::Expr(value)` in
//!   sequence ending in `Stmt::Expr` right-folds into nested
//!   `MirExpr::Let { binding, value, body }` nodes.
//! - Binding slot comes from `FnResolution.local_slots[name]`;
//!   intermediate `Stmt::Expr` gets a fresh synthetic `LocalId`
//!   drawn from a counter starting at `local_count`.
//!
//! Wave 3b lands `match` arms + patterns; wave 3c lands `Try`,
//! tail calls, `IndependentProduct`, lists / tuples / maps /
//! interpolation, and built-in constructors (`Result.Ok` /
//! `Option.Some` — those need a typed identity story beyond raw
//! `CtorId`). The bind-and-propagate shape `let x = step()?; body`
//! lowers as `Let { binding, value: Try(step()), body }` — no
//! dedicated `TryBind` variant (dropped during wave 3 prep).
//!
//! Functions that use anything outside the waves' supported
//! subset are silently skipped — `MirProgram.fns` only contains
//! what the waves so far knew how to handle.
//!
//! ## LocalId mapping
//!
//! Wave 1+2 use the resolver's slot index
//! (`ResolvedExpr::Resolved { slot, .. }`) directly as the
//! `LocalId`. Phase 2's RFC pin was "assign at MIR construction
//! time" — that gives the future optimizer freedom to renumber,
//! but during lowering itself we use the slot the resolver
//! already assigned. The slot is already unique per function
//! body, so wave 1+2 don't need a fresh counter yet. Wave 3
//! will introduce one when `Let` bindings start producing fresh
//! locals that the resolver didn't see.

use crate::ast::{FnResolution, Spanned};
use crate::ir::hir::{
    ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedFnBody, ResolvedFnDef, ResolvedStmt,
    ResolvedTopLevel,
};

use super::expr::{
    MirBinOp, MirCall, MirCallee, MirConstruct, MirEffectAnnotation, MirExpr, MirLet, MirProject,
    MirRecordCreate, MirRecordField, MirRecordUpdate,
};
use super::program::{LocalId, MirFn, MirParam, MirProgram};

/// Lower an entry-module resolved-item list into a `MirProgram`.
/// Function bodies outside wave 1's supported subset are silently
/// skipped — `MirProgram.fns` only contains what this wave knew
/// how to handle. Wave 2 and 3 will widen the coverage; a future
/// "complete" assertion test can compare `lowered_fn_count` against
/// the total `ResolvedFnDef` count to track progress.
pub fn lower_program(items: &[ResolvedTopLevel]) -> MirProgram {
    let mut program = MirProgram::empty();
    for item in items {
        if let ResolvedTopLevel::FnDef(fd) = item
            && let Some(mir_fn) = lower_fn(fd)
        {
            program.fns.insert(fd.fn_id, mir_fn);
        }
    }
    program
}

/// Lower one `ResolvedFnDef` if its body fits wave 1. Returns
/// `None` for everything else; the caller drops the fn from the
/// MIR program in that case.
fn lower_fn(fd: &ResolvedFnDef) -> Option<MirFn> {
    let ResolvedFnBody::Block(stmts) = &*fd.body;
    if stmts.is_empty() {
        return None;
    }
    let body = match stmts.len() {
        1 => {
            // Single-stmt body: must be `Expr`. Bindings alone
            // can't produce a value.
            let ResolvedStmt::Expr(expr) = &stmts[0] else {
                return None;
            };
            lower_expr(expr)?
        }
        _ => {
            // Multi-stmt body (wave 3a): right-fold into `Let`
            // chains. Last stmt must be `Expr` — it's the body's
            // final value; everything before it gets opaque-let'd
            // so effectful intermediate calls survive into MIR.
            let resolution = fd.resolution.as_ref()?;
            let mut next_synthetic_local = u32::from(resolution.local_count);
            lower_stmt_chain(stmts, resolution, &mut next_synthetic_local)?
        }
    };

    let params = fd
        .params
        .iter()
        .enumerate()
        .map(|(i, (name, ty))| MirParam {
            // Wave 1's local numbering matches the resolver's
            // parameter-slot convention (params occupy the lowest
            // slot indices). When wave 2 adds let bindings, they
            // continue from `params.len()`.
            local: LocalId(i as u32),
            name: name.clone(),
            ty: format!("{ty:?}"),
        })
        .collect();
    let effects = fd
        .effects
        .iter()
        .map(|e| MirEffectAnnotation {
            name: e.node.clone(),
        })
        .collect();
    Some(MirFn {
        fn_id: fd.fn_id,
        name: fd.name.clone(),
        params,
        return_type: format!("{:?}", fd.return_type),
        effects,
        body,
    })
}

/// Wave 1 expression lowering. Returns `None` for any construct
/// outside the supported subset so `lower_fn` can drop the whole
/// function rather than emit a half-lowered body.
fn lower_expr(expr: &Spanned<ResolvedExpr>) -> Option<Spanned<MirExpr>> {
    let mir = match &expr.node {
        // ── Wave 1 ──────────────────────────────────────────────
        ResolvedExpr::Literal(lit) => MirExpr::Literal(wrap(lit.clone(), expr)),
        ResolvedExpr::Resolved { slot, .. } => {
            MirExpr::Local(wrap(LocalId(u32::from(*slot)), expr))
        }
        ResolvedExpr::BinOp(op, lhs, rhs) => MirExpr::BinOp(wrap(
            MirBinOp {
                op: *op,
                lhs: Box::new(lower_expr(lhs)?),
                rhs: Box::new(lower_expr(rhs)?),
            },
            expr,
        )),
        ResolvedExpr::Neg(inner) => MirExpr::Neg(Box::new(lower_expr(inner)?)),

        // ── Wave 2 ──────────────────────────────────────────────
        ResolvedExpr::Call(callee, args) => {
            let mir_callee = match callee {
                ResolvedCallee::Fn(fn_id) => MirCallee::Fn(*fn_id),
                ResolvedCallee::Builtin(name) => MirCallee::Builtin(name.clone()),
                // First-class fn values, intrinsics, and unresolved
                // callees aren't covered by wave 2. Wave 3+ will
                // grow `MirCallee` (or a separate node) for the
                // first-class-fn case once `Let` / closure shapes
                // need it. Intrinsics arrive only after lowering
                // passes the lowerer doesn't currently traverse.
                ResolvedCallee::Intrinsic(_)
                | ResolvedCallee::LocalSlot { .. }
                | ResolvedCallee::Unresolved { .. } => return None,
            };
            let mir_args: Option<Vec<_>> = args.iter().map(lower_expr).collect();
            MirExpr::Call(wrap(
                MirCall {
                    callee: mir_callee,
                    args: mir_args?,
                },
                expr,
            ))
        }
        ResolvedExpr::Ctor(ResolvedCtor::User { ctor_id, .. }, args) => {
            let mir_args: Option<Vec<_>> = args.iter().map(lower_expr).collect();
            MirExpr::Construct(wrap(
                MirConstruct {
                    ctor: *ctor_id,
                    args: mir_args?,
                },
                expr,
            ))
        }
        // Built-in ctors (`Result.Ok` / `Option.Some` / etc.)
        // don't carry user-program `CtorId`s. Wave 3 will introduce
        // a typed identity for them — until then, fns that touch
        // them get dropped from the MIR output.
        ResolvedExpr::Ctor(ResolvedCtor::Builtin(_) | ResolvedCtor::Unresolved { .. }, _) => {
            return None;
        }
        ResolvedExpr::RecordCreate {
            type_id: Some(type_id),
            fields,
            ..
        } => {
            let mir_fields: Option<Vec<_>> = fields
                .iter()
                .map(|(name, value)| {
                    Some(MirRecordField {
                        name: name.clone(),
                        value: lower_expr(value)?,
                    })
                })
                .collect();
            MirExpr::RecordCreate(wrap(
                MirRecordCreate {
                    type_id: *type_id,
                    fields: mir_fields?,
                },
                expr,
            ))
        }
        ResolvedExpr::RecordUpdate {
            type_id: Some(type_id),
            base,
            updates,
            ..
        } => {
            let mir_updates: Option<Vec<_>> = updates
                .iter()
                .map(|(name, value)| {
                    Some(MirRecordField {
                        name: name.clone(),
                        value: lower_expr(value)?,
                    })
                })
                .collect();
            MirExpr::RecordUpdate(wrap(
                MirRecordUpdate {
                    base: Box::new(lower_expr(base)?),
                    type_id: *type_id,
                    updates: mir_updates?,
                },
                expr,
            ))
        }
        // Records on built-in types (`HttpResponse`, `Header`,
        // `Buffer`, …) carry no `TypeId`. Wave 2 skips them; a
        // later phase will widen `MirRecordCreate` / `MirRecordUpdate`
        // to carry an optional source-type-name when the consumer
        // demands it (none does today).
        ResolvedExpr::RecordCreate { type_id: None, .. }
        | ResolvedExpr::RecordUpdate { type_id: None, .. } => return None,
        ResolvedExpr::Attr(base, field) => MirExpr::Project(wrap(
            MirProject {
                base: Box::new(lower_expr(base)?),
                field: field.clone(),
            },
            expr,
        )),

        // ── Wave 3+ ─────────────────────────────────────────────
        _ => return None,
    };
    Some(wrap(mir, expr))
}

/// Wrap a freshly-lowered node in `Spanned` while inheriting the
/// source's line. The MIR type stamp starts uninitialised — Phase
/// 6 optimizer passes may fill it later; consumers that need the
/// HIR-side stamp can still read `expr.ty()` on the original.
fn wrap<T, U>(node: T, source: &Spanned<U>) -> Spanned<T> {
    Spanned {
        node,
        line: source.line,
        ty: std::sync::OnceLock::new(),
    }
}

/// Right-fold a list of `ResolvedStmt`s into a single `MirExpr`
/// via `Let` nesting. Wave 3a — the first lowering that produces
/// multi-stmt bodies.
///
/// Rules:
/// - The last stmt must be `Stmt::Expr`; it becomes the innermost
///   body of the resulting `Let` chain.
/// - `Stmt::Binding { name, value }` uses the slot the resolver
///   already assigned (`resolution.local_slots[name]`) as the
///   binding's `LocalId`. Missing slot → the fn is skipped.
/// - `Stmt::Expr` at non-tail position is treated as a let
///   binding to a fresh synthetic `LocalId` so its (potentially
///   effectful) value still gets evaluated. The synthetic
///   counter starts at `resolution.local_count` so it can't
///   collide with the resolver's own slots.
fn lower_stmt_chain(
    stmts: &[ResolvedStmt],
    resolution: &FnResolution,
    next_synthetic_local: &mut u32,
) -> Option<Spanned<MirExpr>> {
    let (last, rest) = stmts.split_last()?;
    // Last stmt must produce a value.
    let ResolvedStmt::Expr(tail_expr) = last else {
        return None;
    };
    let mut body = lower_expr(tail_expr)?;

    // Right-fold: walk earlier stmts in reverse, wrapping each
    // around the accumulating `body`.
    for stmt in rest.iter().rev() {
        let (binding, value_expr) = match stmt {
            ResolvedStmt::Binding { name, value, .. } => {
                let slot = *resolution.local_slots.get(name)?;
                (LocalId(u32::from(slot)), value)
            }
            ResolvedStmt::Expr(expr) => {
                let fresh = LocalId(*next_synthetic_local);
                *next_synthetic_local += 1;
                (fresh, expr)
            }
        };
        let mir_value = lower_expr(value_expr)?;
        let span = mir_value.line;
        body = Spanned {
            node: MirExpr::Let(Spanned {
                node: MirLet {
                    binding,
                    value: Box::new(mir_value),
                    body: Box::new(body),
                },
                line: span,
                ty: std::sync::OnceLock::new(),
            }),
            line: span,
            ty: std::sync::OnceLock::new(),
        };
    }
    Some(body)
}
