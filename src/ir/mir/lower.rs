//! HIR → MIR lowering, wave 1.
//!
//! Phase 3 of #252 lowers `ResolvedProgramView` into `MirProgram`
//! in widening waves so review surface stays small. **Wave 1 (this
//! file)** covers the leaf subset that doesn't need a slot table,
//! a callee resolver, or pattern matching:
//!
//! - `ResolvedExpr::Literal` → `MirExpr::Literal`
//! - `ResolvedExpr::Resolved { slot, .. }` → `MirExpr::Local`
//! - `ResolvedExpr::BinOp` → `MirExpr::BinOp`
//! - `ResolvedExpr::Neg` → `MirExpr::Neg`
//! - single-stmt `Stmt::Expr` body
//!
//! Any function body that uses constructs outside this subset is
//! skipped (not added to the resulting `MirProgram.fns`). Wave 2
//! adds calls + constructors + records; wave 3 adds match, Try /
//! TryBind, tail calls, and independent products. Every wave is a
//! separate PR.
//!
//! ## LocalId mapping
//!
//! Wave 1 uses the resolver's slot index (`ResolvedExpr::Resolved {
//! slot, .. }`) directly as the `LocalId`. Phase 2's RFC pin was
//! "assign at MIR construction time" — that means the optimizer
//! can later renumber freely, but during lowering itself we use
//! the slot the resolver already assigned. This is the simplest
//! correct choice: the slot is already unique per function body.

use crate::ast::Spanned;
use crate::ir::hir::{ResolvedExpr, ResolvedFnBody, ResolvedFnDef, ResolvedStmt, ResolvedTopLevel};

use super::expr::{MirBinOp, MirEffectAnnotation, MirExpr};
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
    if stmts.len() != 1 {
        return None;
    }
    let ResolvedStmt::Expr(expr) = &stmts[0] else {
        return None;
    };
    let body = lower_expr(expr)?;

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
