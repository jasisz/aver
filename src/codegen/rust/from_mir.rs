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
//! - wave 2: Call(Fn) + Call(Builtin), Let, Return ✅
//! - wave 3: Construct (Builtin only — User pending ctx
//!   threading), Project, RecordCreate ✅ (partial)
//! - wave 4: Match (the big one, like Phase 4g for the VM) +
//!   User-ctor Construct + RecordCreate / RecordUpdate
//! - wave 5: Try, TailCall, List/Tuple/Map, InterpolatedStr,
//!   IndependentProduct

use crate::ast::{BinOp, Spanned, Type};
use crate::ir::SymbolTable;
use crate::ir::hir::BuiltinCtor;
use crate::ir::mir::{MirCallee, MirCtor, MirExpr};

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
pub(super) fn emit_mir_expr(expr: &Spanned<MirExpr>, symbol_table: &SymbolTable) -> Option<String> {
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
        MirExpr::Neg(inner) => Some(format!("(-{})", emit_mir_expr(inner, symbol_table)?)),
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            let l = emit_mir_expr(&bop.lhs, symbol_table)?;
            let r = emit_mir_expr(&bop.rhs, symbol_table)?;
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
            // Phase 5 wave 2: read type stamps to disambiguate
            // numeric `+` from `AverStr` concat. Same shape HIR
            // walker takes via `ectx.expr_is_numeric`. When both
            // operands are non-numeric (Str), the right side is
            // borrowed via `&` for `AverStr`'s `Add<&AverStr>`
            // impl.
            if matches!(bop.op, BinOp::Add)
                && !ty_is_numeric(bop.lhs.ty())
                && !ty_is_numeric(bop.rhs.ty())
            {
                Some(format!("({} + &{})", l, r))
            } else {
                Some(format!("({} {} {})", l, op_str, r))
            }
        }
        MirExpr::Call(spanned_call) => {
            let call = &spanned_call.node;
            match &call.callee {
                MirCallee::Fn(fn_id) => {
                    // Resolve canonical name through the same
                    // symbol table the HIR walker uses; emit
                    // `name(arg1, arg2, …)` in source order.
                    let name = symbol_table.fn_entry(*fn_id).key.canonical();
                    let mut args = Vec::with_capacity(call.args.len());
                    for a in &call.args {
                        args.push(emit_mir_expr(a, symbol_table)?);
                    }
                    Some(format!("{}({})", aver_name_to_rust(&name), args.join(", ")))
                }
                // Builtin / closure / unresolved callees ride the
                // HIR walker's classification (`CallPlan`) — too
                // many shapes to mirror in wave 2.
                MirCallee::Builtin(_) => None,
            }
        }
        MirExpr::Return(inner) => Some(format!("return {}", emit_mir_expr(inner, symbol_table)?)),
        MirExpr::Project(spanned_proj) => {
            // Phase 5 wave 3: `base.field` projection. Mirror of
            // HIR's `ResolvedLeafOp::FieldAccess` emit shape —
            // emit_expr(base) + "." + aver_name_to_rust(field).
            // No clone insertion here; the HIR walker handles
            // that via `maybe_clone` at outer call sites.
            let proj = &spanned_proj.node;
            let base = emit_mir_expr(&proj.base, symbol_table)?;
            Some(format!("{}.{}", base, aver_name_to_rust(&proj.field)))
        }
        MirExpr::Construct(spanned_ctor) => {
            // Phase 5 wave 3: built-in ctor variants only. User
            // ctors need `CodegenContext` (boxed_positions +
            // resolve_module_call) for `Module::Type::Variant`
            // path mangling — falls back to HIR until wave 4
            // threads the context through.
            let con = &spanned_ctor.node;
            match con.ctor {
                MirCtor::Builtin(builtin) => {
                    let (name, takes_arg) = match builtin {
                        BuiltinCtor::ResultOk => ("Ok", true),
                        BuiltinCtor::ResultErr => ("Err", true),
                        BuiltinCtor::OptionSome => ("Some", true),
                        BuiltinCtor::OptionNone => ("None", false),
                    };
                    if !takes_arg {
                        // `Option.None` — no args, no parens.
                        return Some(name.to_string());
                    }
                    let mut args = Vec::with_capacity(con.args.len());
                    for a in &con.args {
                        args.push(emit_mir_expr(a, symbol_table)?);
                    }
                    Some(format!("{}({})", name, args.join(", ")))
                }
                MirCtor::User(_) => None,
            }
        }
        _ => None,
    }
}

/// Phase 5 wave 2 helper: is the type stamp a primitive numeric?
/// `Int` / `Float` / `Byte` count; everything else (incl. `Str`)
/// doesn't. Mirror of HIR's `EmitCtx::expr_is_numeric` for the
/// MIR walker's `+` dispatch.
fn ty_is_numeric(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::Int | Type::Float))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::SymbolTable;
    use crate::ir::mir::{LocalId, MirBinOp, MirCall, MirExpr, MirLocal};
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

    fn empty_symbols() -> SymbolTable {
        SymbolTable::default()
    }

    #[test]
    fn emits_int_literal_as_i64_suffix() {
        let lit = span(MirExpr::Literal(span(crate::ast::Literal::Int(42))));
        assert_eq!(
            emit_mir_expr(&lit, &empty_symbols()).as_deref(),
            Some("42i64")
        );
    }

    #[test]
    fn emits_local_via_aver_name_to_rust() {
        let local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let expr = span(MirExpr::Local(span(local)));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("local should emit");
        assert!(
            emit.contains("x"),
            "local emit should reference `x`: {emit}"
        );
    }

    #[test]
    fn returns_none_for_synthetic_local() {
        let local = MirLocal {
            slot: LocalId(7),
            last_use: false,
            name: String::new(),
        };
        let expr = span(MirExpr::Local(span(local)));
        assert!(emit_mir_expr(&expr, &empty_symbols()).is_none());
    }

    #[test]
    fn emits_int_binop_add_as_plus() {
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
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("binop should emit");
        // Numeric path — both operands stamped Int → no `&` on
        // the right side.
        assert!(
            emit.contains(" + ") && !emit.contains(" + &"),
            "Int+Int should emit plain `+`, got: {emit}"
        );
    }

    #[test]
    fn emits_str_binop_add_as_concat() {
        // Phase 5 wave 2: when both operands are stamped `Str`,
        // the BinOp::Add path emits `(l + &r)` to match HIR's
        // `AverStr` concat shape.
        let s = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "s".to_string(),
        };
        let bop = MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span_ty(MirExpr::Local(span(s.clone())), Type::Str)),
            rhs: Box::new(span_ty(MirExpr::Local(span(s)), Type::Str)),
        };
        let expr = span(MirExpr::BinOp(span(bop)));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("binop should emit");
        assert!(
            emit.contains(" + &"),
            "Str+Str should emit `+ &` for AverStr concat: {emit}"
        );
    }

    #[test]
    fn emits_neg_as_paren_minus_inner() {
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Neg(Box::new(inner)));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("neg should emit");
        assert_eq!(emit, "(-7i64)");
    }

    #[test]
    fn returns_none_for_builtin_call() {
        // Builtin calls ride the HIR walker (call-plan
        // classification). MIR walker returns None.
        let call = MirCall {
            callee: MirCallee::Builtin("String.len".to_string()),
            args: vec![span(MirExpr::Literal(span(crate::ast::Literal::Str(
                "hello".to_string(),
            ))))],
        };
        let expr = span(MirExpr::Call(span(call)));
        assert!(emit_mir_expr(&expr, &empty_symbols()).is_none());
    }

    #[test]
    fn emits_return_keyword() {
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Return(Box::new(inner)));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("return should emit");
        assert_eq!(emit, "return 7i64");
    }

    #[test]
    fn returns_none_for_unsupported_variant() {
        let t = span(MirExpr::Tuple(vec![]));
        assert!(emit_mir_expr(&t, &empty_symbols()).is_none());
    }

    #[test]
    fn emits_project_as_dotted_field() {
        // Phase 5 wave 3: `base.field` projection.
        let local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "user".to_string(),
        };
        let base = span(MirExpr::Local(span(local)));
        let proj = crate::ir::mir::MirProject {
            base: Box::new(base),
            field: "name".to_string(),
        };
        let expr = span(MirExpr::Project(span(proj)));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("project should emit");
        assert!(
            emit.ends_with(".name"),
            "project should end with `.name`, got: {emit}"
        );
    }

    #[test]
    fn emits_result_ok_as_ok_call() {
        // Phase 5 wave 3: BuiltinCtor::ResultOk → `Ok(arg)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(42))));
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::Builtin(BuiltinCtor::ResultOk),
            args: vec![arg],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("construct should emit");
        assert_eq!(emit, "Ok(42i64)");
    }

    #[test]
    fn emits_option_none_as_bare_none() {
        // Phase 5 wave 3: BuiltinCtor::OptionNone has no args
        // and emits `None` without parens.
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
            args: vec![],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("construct should emit");
        assert_eq!(emit, "None");
    }

    #[test]
    fn returns_none_for_user_ctor() {
        // Phase 5 wave 3: User ctors need CodegenContext for
        // boxed_positions + module path resolution. Falls back
        // to HIR until wave 4.
        use crate::ir::CtorId;
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::User(CtorId(0)),
            args: vec![],
        };
        let expr = span(MirExpr::Construct(span(con)));
        assert!(emit_mir_expr(&expr, &empty_symbols()).is_none());
    }
}
