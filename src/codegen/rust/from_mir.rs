//! Phase 5 wave 1 — Rust backend consumes MIR.
//!
//! Mirror of [`super::expr::emit_expr`] that walks
//! [`crate::ir::mir::MirExpr`] instead of `ResolvedExpr` and
//! emits the same Rust source string. The point is to land the
//! same deduplication that #252 Phase 4 brought to the VM: one
//! semantic walker per construct lives in MIR, and every
//! backend (VM done, Rust this wave, wasm-gc / wasip2 later)
//! reads from it instead of forking `ResolvedExpr`.
//!
//! ## Scope (Phase 5 wave 1)
//!
//! Subset of `MirExpr` covered here — mirrors Phase 4a's
//! starting subset on the VM side:
//!
//! - `Literal` — `super::expr::emit_literal`
//! - `Local { name, .. }` — `aver_name_to_rust(&name)`
//! - `BinOp` — `(lhs op rhs)` (Add / Sub / Mul / Div / Eq /
//!   Neq / Lt / Gt / Lte / Gte). String-concat / numeric
//!   inference is *not* mirrored — the HIR walker reads
//!   `ectx` to disambiguate `+` between numeric add and
//!   `AverStr` concat; MIR's type stamps would let us do the
//!   same but we keep this PoC numeric-only.
//! - `Neg(inner)` — `(-inner)`
//!
//! Everything else returns `None` so the caller knows the MIR
//! walker can't cover the construct yet and should fall back
//! to the HIR walker. Same fallback shape Phase 4 used.
//!
//! Wider waves (planned, not in this PR):
//! - wave 2: Call(Fn) + Call(Builtin), Let, Return
//! - wave 3: Construct (User + Builtin), Project, RecordCreate
//! - wave 4: Match (the big one, like Phase 4g for the VM)
//! - wave 5: Try, TailCall, List/Tuple/Map, InterpolatedStr,
//!   IndependentProduct

use crate::ast::{BinOp, Spanned};
use crate::ir::mir::MirExpr;

use super::expr::emit_literal;
use super::syntax::aver_name_to_rust;

/// Try to emit Rust source for `expr` directly from MIR.
/// Returns `None` for any variant outside the Phase 5 wave 1
/// subset — caller falls back to the HIR walker.
///
/// Mirror of [`super::expr::emit_expr`] for the covered
/// subset; output strings should be character-for-character
/// identical to the HIR walker's output on the same input
/// (modulo type-disambiguation paths the HIR walker takes via
/// `EmitCtx`, which we don't have access to here).
///
/// Dead-code-allowed until Phase 5 wave 2 wires the consumer
/// inside [`super::expr::emit_expr`] (try MIR first, fall back
/// to HIR walker on `None`).
#[allow(dead_code)]
pub(super) fn emit_mir_expr(expr: &Spanned<MirExpr>) -> Option<String> {
    match &expr.node {
        MirExpr::Literal(lit) => Some(emit_literal(&lit.node)),
        MirExpr::Local(spanned_local) => {
            let name = &spanned_local.node.name;
            if name.is_empty() {
                // Synthetic locals (intermediate stmt-chain
                // effectful expressions) carry no source name —
                // the Rust backend can't emit them as idents.
                // Caller falls back to HIR.
                return None;
            }
            Some(aver_name_to_rust(name))
        }
        MirExpr::Neg(inner) => Some(format!("(-{})", emit_mir_expr(inner)?)),
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            let l = emit_mir_expr(&bop.lhs)?;
            let r = emit_mir_expr(&bop.rhs)?;
            let op_str = match bop.op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
            };
            // Numeric-only path. String-concat (`+` on AverStr)
            // would need the HIR walker's `ectx.expr_is_numeric`
            // check; MIR has the same info on `ty()` stamps but
            // we keep wave 1 narrow.
            Some(format!("({} {} {})", l, op_str, r))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Type;
    use crate::ir::mir::{LocalId, MirBinOp, MirExpr, MirLocal};
    use std::sync::OnceLock;

    fn span<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            line: 0,
            ty: OnceLock::new(),
        }
    }

    fn span_ty<T>(node: T, ty: Type) -> Spanned<T> {
        let stamp = OnceLock::new();
        let _ = stamp.set(ty);
        Spanned {
            node,
            line: 0,
            ty: stamp,
        }
    }

    #[test]
    fn emits_int_literal_as_i64_suffix() {
        let lit = span(MirExpr::Literal(span(crate::ast::Literal::Int(42))));
        assert_eq!(emit_mir_expr(&lit).as_deref(), Some("42i64"));
    }

    #[test]
    fn emits_local_via_aver_name_to_rust() {
        let local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let expr = span(MirExpr::Local(span(local)));
        let emit = emit_mir_expr(&expr).expect("local should emit");
        // `aver_name_to_rust("x")` may return `"x"` directly or
        // a sanitised variant; either way it must be non-empty
        // and start with `x` for a plain ident.
        assert!(
            emit.contains("x"),
            "local emit should reference `x`: {emit}"
        );
    }

    #[test]
    fn returns_none_for_synthetic_local() {
        // Wave 3a stmt-chain synthetic locals have empty name —
        // Rust walker returns None so caller falls back to HIR.
        let local = MirLocal {
            slot: LocalId(7),
            last_use: false,
            name: String::new(),
        };
        let expr = span(MirExpr::Local(span(local)));
        assert!(emit_mir_expr(&expr).is_none());
    }

    #[test]
    fn emits_binop_add_as_paren_l_op_r() {
        let x = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let bop = MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span_ty(MirExpr::Local(span(x.clone())), Type::Int)),
            rhs: Box::new(span_ty(MirExpr::Local(span(x)), Type::Int)),
        };
        let expr = span(MirExpr::BinOp(span(bop)));
        let emit = emit_mir_expr(&expr).expect("binop should emit");
        assert!(
            emit.contains("(") && emit.contains(" + ") && emit.contains(")"),
            "expected `(x + x)`-shape Rust: {emit}"
        );
    }

    #[test]
    fn emits_neg_as_paren_minus_inner() {
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Neg(Box::new(inner)));
        let emit = emit_mir_expr(&expr).expect("neg should emit");
        assert_eq!(emit, "(-7i64)");
    }

    #[test]
    fn returns_none_for_unsupported_variant() {
        // `Tuple` isn't in wave 1's subset — must signal
        // fallback to HIR.
        let t = span(MirExpr::Tuple(vec![]));
        assert!(emit_mir_expr(&t).is_none());
    }
}
