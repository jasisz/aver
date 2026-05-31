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
//! - wave 4a: Let ✅ (this PR — uses foundation #293
//!   `MirLet.binding_name` to emit block-expr `{ let x = …; … }`)
//! - wave 4b: Match (the big one, like Phase 4g for the VM) +
//!   User-ctor Construct (needs `&CodegenContext` threading
//!   for module-path resolution and boxed-position lookups)
//! - wave 4c: RecordCreate / RecordUpdate ✅ (this PR — module-
//!   unscoped record types via bare `key.name`; module-scoped
//!   records fall back to HIR until wave 4b)
//! - wave 5: Try / Tuple / List ✅ (`?` propagation + plain
//!   tuple / list literals reusing recursive walker)
//! - wave 6: Map literal ✅ (`HashMap::new()` /
//!   `vec![…].into_iter().collect::<HashMap<_, _>>()` mirror
//!   of HIR's emit shape, recursive on keys + values)
//! - wave 7: TailCall ✅ (this PR — emitted as a regular
//!   function call; HIR's self-TCO `continue` rewrite needs
//!   `ectx` so the wire-up's parity check is the safety net)
//! - wave 8: IndependentProduct (replay-runtime + parallel
//!   branch handling needs `ctx.emit_replay_runtime` and
//!   ParallelBranch wiring — large block, deferred to its own
//!   PR)
//!
//! Won't reach the walker: InterpolatedStr is dropped by
//! `interp_lower` before codegen runs.

use crate::ast::{BinOp, Spanned, Type};
use crate::ir::SymbolTable;
use crate::ir::hir::BuiltinCtor;
use crate::ir::mir::{MirCallee, MirCtor, MirExpr, MirProgram};

use super::expr::emit_literal;
use super::syntax::aver_name_to_rust;

/// Phase 5 diagnostic: how many fns the MIR walker can emit
/// standalone vs how many need HIR fallback. Pre-wire-up signal
/// so callers can track walker reach across the shipped corpus
/// without altering the codegen path.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CoverageReport {
    /// Total fn count in the lowered program.
    pub total: usize,
    /// Fns whose entire body the walker emits standalone
    /// (no `None` anywhere in the recursive walk).
    pub mir_covered: usize,
    /// Fns the walker can't emit — the recursive walk hit at
    /// least one variant that returned `None`. Caller would
    /// fall back to the HIR walker in a wire-up.
    pub hir_fallback: usize,
}

impl CoverageReport {
    /// Walker reach as a percentage of total fns. `0.0` when
    /// the program is empty (no fns lowered).
    pub fn ratio(&self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            self.mir_covered as f64 / self.total as f64
        }
    }
}

/// Walk every fn in `program` and report walker reach. For each
/// fn, calls [`emit_mir_expr`] on the body and counts
/// `Some` / `None`. Suitable for `--explain-mir-coverage`–style
/// diagnostics; the codegen path itself is untouched.
pub fn coverage_report(program: &MirProgram, symbol_table: &SymbolTable) -> CoverageReport {
    let mut report = CoverageReport::default();
    for (_, mir_fn) in program.iter() {
        report.total += 1;
        if emit_mir_expr(&mir_fn.body, symbol_table).is_some() {
            report.mir_covered += 1;
        } else {
            report.hir_fallback += 1;
        }
    }
    report
}

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
        MirExpr::TailCall(spanned_tc) => {
            // Phase 5 wave 7: tail call outside a self-TCO loop
            // emits as a regular function call — mirror of HIR's
            // `ResolvedExpr::TailCall` outside-loop branch
            // (which the resolver leaves intact and the emitter
            // routes through `emit_named_function_call`). When
            // the surrounding fn IS in a TCO loop, HIR rewrites
            // it to `continue` + param assigns — the walker
            // can't see that without `ectx`, so the wire-up
            // layer's parity check is the safety net (mismatch
            // → fall back to HIR).
            let tc = &spanned_tc.node;
            let name = symbol_table.fn_entry(tc.target).key.canonical();
            let mut args = Vec::with_capacity(tc.args.len());
            for a in &tc.args {
                args.push(emit_mir_expr(a, symbol_table)?);
            }
            Some(format!("{}({})", aver_name_to_rust(&name), args.join(", ")))
        }
        MirExpr::Try(inner) => {
            // Phase 5 wave 5: `value?` propagation. Mirror of
            // HIR's `ResolvedExpr::ErrorProp` emit — append `?`
            // to the inner expression's Rust form.
            Some(format!("{}?", emit_mir_expr(inner, symbol_table)?))
        }
        MirExpr::Tuple(items) => {
            // Phase 5 wave 5: `(a, b, c)` tuple literal. Mirror
            // of HIR's `ResolvedExpr::Tuple` emit, minus the
            // `clone_arg` insertion (no `ectx` here — borrowed-
            // param Locals signal the gap by returning their
            // raw name and the outer caller still gets a
            // well-formed string). For pure-value subtrees the
            // output is character-identical to HIR.
            let mut parts = Vec::with_capacity(items.len());
            for item in items {
                parts.push(emit_mir_expr(item, symbol_table)?);
            }
            Some(format!("({})", parts.join(", ")))
        }
        MirExpr::List(items) => {
            // Phase 5 wave 5: `[a, b, c]` list literal. Mirror
            // of HIR's `ResolvedExpr::List` — empty case folds
            // to `aver_rt::AverList::empty()`, non-empty to
            // `from_vec(vec![...])`.
            if items.is_empty() {
                return Some("aver_rt::AverList::empty()".to_string());
            }
            let mut parts = Vec::with_capacity(items.len());
            for item in items {
                parts.push(emit_mir_expr(item, symbol_table)?);
            }
            Some(format!(
                "aver_rt::AverList::from_vec(vec![{}])",
                parts.join(", ")
            ))
        }
        MirExpr::MapLiteral(entries) => {
            // Phase 5 wave 6: `{"k" => v, …}` map literal.
            // Mirror of HIR's `ResolvedExpr::MapLiteral` — empty
            // → `HashMap::new()`, non-empty →
            // `vec![(k, v), …].into_iter().collect::<HashMap<_, _>>()`.
            // No clone_arg insertion; pure-value subtrees match
            // HIR character-for-character.
            if entries.is_empty() {
                return Some("HashMap::new()".to_string());
            }
            let mut parts = Vec::with_capacity(entries.len());
            for (k, v) in entries {
                let key_str = emit_mir_expr(k, symbol_table)?;
                let val_str = emit_mir_expr(v, symbol_table)?;
                parts.push(format!("({}, {})", key_str, val_str));
            }
            Some(format!(
                "vec![{}].into_iter().collect::<HashMap<_, _>>()",
                parts.join(", ")
            ))
        }
        MirExpr::Let(spanned_let) => {
            // Phase 5 wave 4a: `let binding = value; body` →
            // Rust block-expression `{ let x = value; body }`.
            // Synthetic locals (intermediate effectful
            // `Stmt::Expr` at non-tail position) carry
            // `binding_name.is_empty()` — the Rust walker can't
            // emit them as named idents, so we fall back to
            // HIR. Mirror of the `MirLocal { name }` empty-name
            // fallback on the read side.
            let let_node = &spanned_let.node;
            if let_node.binding_name.is_empty() {
                return None;
            }
            let value = emit_mir_expr(&let_node.value, symbol_table)?;
            let body = emit_mir_expr(&let_node.body, symbol_table)?;
            let name = aver_name_to_rust(&let_node.binding_name);
            Some(format!("{{ let {} = {}; {} }}", name, value, body))
        }
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
        MirExpr::RecordCreate(spanned_rec) => {
            // Phase 5 wave 4c: `T { field = v, … }` record literal.
            // Mirror of HIR's `ResolvedExpr::RecordCreate` emit
            // shape — `{type_name} { field: value, … }`. Uses
            // bare `key.name` (not canonical) so the walker can
            // resolve without `CodegenContext`'s
            // `resolve_module_call` chain. Module-scoped record
            // types (e.g. `Tcp.Connection` → `Tcp_Connection`)
            // need ctx threading — until wave 4b's sig change
            // lands, the walker returns `None` whenever the
            // type's `key.scope` is non-empty so the HIR walker
            // handles the prefix rewrite.
            let rec = &spanned_rec.node;
            let entry = symbol_table.type_entry(rec.type_id);
            if entry.key.scope_str().is_some() {
                return None;
            }
            let type_name = entry.key.name.clone();
            let mut parts = Vec::with_capacity(rec.fields.len());
            for f in &rec.fields {
                let val = emit_mir_expr(&f.value, symbol_table)?;
                parts.push(format!("{}: {}", aver_name_to_rust(&f.name), val));
            }
            Some(format!("{} {{ {} }}", type_name, parts.join(", ")))
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            // Phase 5 wave 4c: `T.update(base, field = v, …)`
            // → `{type_name} { field: value, …, ..base }`.
            // Same module-scope gating as RecordCreate above.
            let upd = &spanned_upd.node;
            let entry = symbol_table.type_entry(upd.type_id);
            if entry.key.scope_str().is_some() {
                return None;
            }
            let type_name = entry.key.name.clone();
            let base = emit_mir_expr(&upd.base, symbol_table)?;
            let mut parts = Vec::with_capacity(upd.updates.len());
            for f in &upd.updates {
                let val = emit_mir_expr(&f.value, symbol_table)?;
                parts.push(format!("{}: {}", aver_name_to_rust(&f.name), val));
            }
            Some(format!(
                "{} {{ {}, ..{} }}",
                type_name,
                parts.join(", "),
                base
            ))
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

    fn symbols_with_one_type(name: &str, scoped: bool) -> SymbolTable {
        use crate::ir::ModuleId;
        use crate::ir::identity::TypeKey;
        use crate::ir::symbol_table::{ModuleEntry, TypeEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        let key = if scoped {
            TypeKey::in_module("Tcp", name)
        } else {
            TypeKey::entry(name)
        };
        st.types.push(TypeEntry {
            key,
            module: ModuleId(0),
            index_in_module: 0,
            variants: vec![],
            is_product: true,
        });
        st
    }

    #[test]
    fn emits_record_create_unscoped() {
        // Phase 5 wave 4c: `Point { x: 1, y: 2 }` for a
        // module-unscoped record type.
        let field_x = crate::ir::mir::MirRecordField {
            name: "x".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(1)))),
        };
        let field_y = crate::ir::mir::MirRecordField {
            name: "y".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(2)))),
        };
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: crate::ir::TypeId(0),
            fields: vec![field_x, field_y],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        let st = symbols_with_one_type("Point", false);
        let emit = emit_mir_expr(&expr, &st).expect("record create should emit");
        assert_eq!(emit, "Point { x: 1i64, y: 2i64 }");
    }

    #[test]
    fn returns_none_for_module_scoped_record() {
        // Phase 5 wave 4c: module-scoped record (e.g.
        // `Tcp.Connection`) needs ctx threading for the
        // `Tcp_Connection` prefix rewrite — walker bounces.
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: crate::ir::TypeId(0),
            fields: vec![],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        let st = symbols_with_one_type("Connection", true);
        assert!(emit_mir_expr(&expr, &st).is_none());
    }

    #[test]
    fn emits_record_update_unscoped() {
        // Phase 5 wave 4c: `T { field: v, ..base }`.
        let base = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "base".to_string(),
        };
        let update = crate::ir::mir::MirRecordField {
            name: "x".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(9)))),
        };
        let upd = crate::ir::mir::MirRecordUpdate {
            base: Box::new(span(MirExpr::Local(span(base)))),
            type_id: crate::ir::TypeId(0),
            updates: vec![update],
        };
        let expr = span(MirExpr::RecordUpdate(span(upd)));
        let st = symbols_with_one_type("Point", false);
        let emit = emit_mir_expr(&expr, &st).expect("record update should emit");
        assert_eq!(emit, "Point { x: 9i64, ..base }");
    }

    fn symbols_with_one_fn(name: &str) -> SymbolTable {
        use crate::ir::ModuleId;
        use crate::ir::identity::FnKey;
        use crate::ir::symbol_table::{FnEntry, ModuleEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        st.fns.push(FnEntry {
            key: FnKey::entry(name),
            module: ModuleId(0),
            index_in_module: 0,
        });
        st
    }

    #[test]
    fn emits_tail_call_as_regular_call() {
        // Phase 5 wave 7: outside-loop `TailCall` mirrors HIR's
        // regular-call emit shape — `name(args)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let tc = span(MirExpr::TailCall(span(crate::ir::mir::MirTailCall {
            target: crate::ir::FnId(0),
            args: vec![arg],
        })));
        let st = symbols_with_one_fn("loop_step");
        let emit = emit_mir_expr(&tc, &st).expect("tail call should emit");
        assert_eq!(emit, "loop_step(7i64)");
    }

    #[test]
    fn returns_none_for_unsupported_variant() {
        // Phase 5 wave 7: TailCall now covered. Pick a variant
        // the walker still bounces — IndependentProduct.
        let ip = span(MirExpr::IndependentProduct(span(
            crate::ir::mir::MirIndependentProduct {
                items: vec![],
                unwrap_results: false,
            },
        )));
        assert!(emit_mir_expr(&ip, &empty_symbols()).is_none());
    }

    #[test]
    fn emits_empty_map_as_hashmap_new() {
        // Phase 5 wave 6: empty map literal.
        let expr = span(MirExpr::MapLiteral(vec![]));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("map should emit");
        assert_eq!(emit, "HashMap::new()");
    }

    #[test]
    fn emits_nonempty_map_as_vec_into_iter_collect() {
        // Phase 5 wave 6: non-empty map literal.
        let k1 = span(MirExpr::Literal(span(crate::ast::Literal::Int(1))));
        let v1 = span(MirExpr::Literal(span(crate::ast::Literal::Int(10))));
        let k2 = span(MirExpr::Literal(span(crate::ast::Literal::Int(2))));
        let v2 = span(MirExpr::Literal(span(crate::ast::Literal::Int(20))));
        let expr = span(MirExpr::MapLiteral(vec![(k1, v1), (k2, v2)]));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("map should emit");
        assert_eq!(
            emit,
            "vec![(1i64, 10i64), (2i64, 20i64)].into_iter().collect::<HashMap<_, _>>()"
        );
    }

    #[test]
    fn emits_try_as_question_mark() {
        // Phase 5 wave 5: `Try(inner)` → `inner?`.
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Try(Box::new(inner)));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("try should emit");
        assert_eq!(emit, "7i64?");
    }

    #[test]
    fn emits_tuple_literal_as_paren_list() {
        // Phase 5 wave 5: `(7, 9)` tuple.
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(9))));
        let expr = span(MirExpr::Tuple(vec![a, b]));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("tuple should emit");
        assert_eq!(emit, "(7i64, 9i64)");
    }

    #[test]
    fn emits_empty_list_as_averlist_empty() {
        let expr = span(MirExpr::List(vec![]));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("list should emit");
        assert_eq!(emit, "aver_rt::AverList::empty()");
    }

    #[test]
    fn emits_nonempty_list_as_from_vec() {
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(1))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(2))));
        let expr = span(MirExpr::List(vec![a, b]));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("list should emit");
        assert_eq!(emit, "aver_rt::AverList::from_vec(vec![1i64, 2i64])");
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
    fn emits_let_as_block_expr() {
        // Phase 5 wave 4a: `let x = 7; x` → `{ let x = 7i64; x }`.
        let value = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let body_local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let body = span(MirExpr::Local(span(body_local)));
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(value),
            body: Box::new(body),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        let emit = emit_mir_expr(&expr, &empty_symbols()).expect("let should emit");
        assert_eq!(emit, "{ let x = 7i64; x }");
    }

    #[test]
    fn returns_none_for_synthetic_let() {
        // Phase 5 wave 4a: synthetic Let (intermediate
        // effectful Stmt::Expr at non-tail position) carries an
        // empty `binding_name`. Walker must fall back to HIR
        // since there's no source ident to bind in Rust.
        let value = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let body = span(MirExpr::Literal(span(crate::ast::Literal::Int(0))));
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(7),
            binding_name: String::new(),
            value: Box::new(value),
            body: Box::new(body),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        assert!(emit_mir_expr(&expr, &empty_symbols()).is_none());
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
