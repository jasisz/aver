//! Phase 5 wave 0 — wasm-gc backend consumes MIR.
//!
//! Mirror of [`super::emit::emit_expr`] that walks
//! [`crate::ir::mir::MirExpr`] instead of `ResolvedExpr` and emits the
//! **byte-identical** wasm. The point is the same deduplication #252
//! Phase 4 brought to the VM and Phase 5 wave 1 brought to the Rust
//! backend (`crate::codegen::rust::from_mir`): one semantic walker per
//! construct lives in MIR, and every backend reads it instead of
//! forking `ResolvedExpr`.
//!
//! ## Scope (waves 0–1)
//!
//! Wave 0 (the canary):
//! - `Literal(Int | Float | Bool | Unit | Str)` — mirror of
//!   `emit_expr`'s literal arms (incl. the `array.new_data` string-
//!   literal segment path).
//! - `Local { slot, .. }` — `local.get slot`. The MIR `LocalId` is
//!   seeded from the resolver's slot index (see
//!   [`crate::ir::mir::program::LocalId`] doc), so it is the wasm
//!   local index 1:1, exactly like `ResolvedExpr::Resolved { slot }`.
//! - `BinOp` (numeric only) — the canary. Dispatches on `bop.lhs.ty()`
//!   reading the MIR type stamp; `Int`/`Float` take the I64/F64 op set
//!   `emit_expr` selects via `wasm_type_of`. `Str` and compound-type
//!   operands return `None` (a later wave does string concat / eq).
//! - `Neg` — numeric unary minus (`f64.neg` / `i64.const 0; …; i64.sub`).
//! - `Return` — `… ; return` (never produced by the current lowering,
//!   carried for symmetry with the Rust walker).
//! - `Let { binding_name, value, body }` — named bindings emit
//!   `value` then `local.set slot` (slot from
//!   `ctx.self_local_slot(name)`), mirroring `emit_fn_body`'s
//!   `Binding` arm; the tail `body` is the return value. Synthetic
//!   lets (empty `binding_name`: non-tail `Stmt::Expr` intermediates
//!   and `_ = expr` discards) return `None` → whole-fn HIR fallback,
//!   same shape the Rust walker takes.
//!
//! Wave 1 (calls):
//! - `Call` with `MirCallee::Fn(FnId)` — mirror of `emit_expr`'s
//!   `ResolvedCallee::Fn` arm: `fn_map.by_id` lookup (same `FnId`
//!   identity), emit args, `call $idx`; a missing entry whose name is a
//!   local slot emits a polymorphic `unreachable` (higher-order
//!   verify-only fns), otherwise a hard error. `Builtin` / `Intrinsic`
//!   / `LocalSlot` callees fall back (later waves).
//! - `TailCall` — mirror of `emit_tail_call`: emit args, then the
//!   shared `emit_return_call_insn` (`return_call`, or `call` under
//!   `AVER_WASM_GC_NO_TAIL_CALL`).
//! - `FnValue` falls back — higher-order, no first-class fn lowering.
//!
//! Everything else returns `Ok(None)` so the caller
//! ([`super::emit_fn_body_via_mir`]) resets `func` and re-runs the
//! `ResolvedExpr` emitter for the whole fn. That keeps the corpus +
//! game suite green from PR 1 while coverage widens wave by wave.

use std::collections::{HashMap, HashSet};

use wasm_encoder::{Function, Instruction, ValType};

use crate::ast::Spanned;
use crate::ast::{BinOp, Literal};
use crate::ir::SymbolTable;
use crate::ir::hir::{ResolvedFnBody, ResolvedFnDef, ResolvedStmt};
use crate::ir::mir::{MirCallee, MirExpr, MirFn, MirProgram};
use crate::types::Type;

use super::super::WasmGcError;
use super::super::types::TypeRegistry;
use super::emit::emit_return_call_insn;
use super::infer::{aver_type_str_of, wasm_type_of};
use super::slots::count_value_params;
use super::{CallerFnCollector, EmitCtx, FnMap, SlotTable, Wasip2Lowering};

/// Lower `mir_fn.body` into `func`, mirroring [`super::emit_fn_body`]
/// byte-for-byte. Returns `Ok(Some(extra_locals))` on full coverage,
/// `Ok(None)` when any node falls outside the supported subset — the
/// caller then discards `func` and re-runs `emit_fn_body`.
///
/// The setup (slot table, binding-name set, `EmitCtx`, return-type
/// string) is identical to `emit_fn_body` and is driven entirely off
/// `rfd` — `SlotTable::build_for_fn` reads the resolver's
/// `local_slot_types`, not the MIR body — so the discovered
/// extra-locals match the `ResolvedExpr` path regardless of which body
/// walk runs. The byte-differential gate depends on that invariant.
#[allow(clippy::too_many_arguments)]
pub(crate) fn emit_fn_body_via_mir(
    func: &mut Function,
    rfd: &ResolvedFnDef,
    mir_fn: &MirFn,
    mir_program: &MirProgram,
    fn_map: &FnMap,
    self_wasm_idx: u32,
    registry: &TypeRegistry,
    symbol_table: &SymbolTable,
    effect_idx_lookup: &HashMap<String, u32>,
    caller_fn_collector: &std::cell::RefCell<CallerFnCollector>,
    wasip2_lowering: Option<&Wasip2Lowering>,
) -> Result<Option<Vec<ValType>>, WasmGcError> {
    let slots = SlotTable::build_for_fn(rfd, registry, fn_map)?;
    let return_type_str = rfd.return_type.display();

    // Precollect every `let`-bound name (mirror of `emit_fn_body`) so
    // `CallLowerCtx::is_local_value` recognises locals without a
    // parallel type table. Source it from the HIR `rfd`, NOT from
    // `mir_fn` — `EmitCtx` is shared with the `ResolvedExpr` emitter and
    // its recognition (`classify_leaf_op` / `classify_call_plan`) keys
    // off resolver-assigned names. Wave 0 never reaches that recognition
    // (no covered arm reads `binding_names`), but a later Call-coverage
    // wave will, and must keep this HIR-sourced — do not repopulate it
    // from `MirExpr`.
    let ResolvedFnBody::Block(stmts) = rfd.body.as_ref();
    let mut binding_names: HashSet<String> = HashSet::new();
    for s in stmts {
        if let ResolvedStmt::Binding { name, .. } = s {
            binding_names.insert(name.clone());
        }
    }

    let ctx = EmitCtx {
        fn_map,
        self_wasm_idx,
        self_fn_name: rfd.name.as_str(),
        return_type: &return_type_str,
        registry,
        symbol_table,
        resolution: rfd.resolution.as_ref(),
        params: &rfd.params,
        binding_names: &binding_names,
        effect_idx_lookup,
        caller_fn_collector,
        wasip2_lowering,
        mir_builtins: Some(&mir_program.builtins),
    };

    // Walk the body. `Ok(None)` mid-walk → caller falls back.
    let Some(produces_value) = emit_mir_expr(func, &mir_fn.body, &slots, &ctx)? else {
        return Ok(None);
    };

    // Tail handling — mirror of `emit_fn_body`'s `is_last` arm. The
    // body's value is the fn's return value, left on the stack.
    if return_type_str.trim() == "Unit" && produces_value {
        func.instruction(&Instruction::Drop);
    } else if return_type_str.trim() != "Unit" && !produces_value {
        return Err(WasmGcError::Validation(format!(
            "fn `{}` returns {} but trailing expression yields no value",
            rfd.name, return_type_str
        )));
    }
    func.instruction(&Instruction::End);

    Ok(Some(slots.extra_locals(count_value_params(&rfd.params))))
}

/// Emit instructions for a MIR `expr`, returning `Ok(Some(produces))`
/// where `produces` is `true` when evaluating `expr` leaves a value on
/// the stack (i.e. its type is not `Unit`) — the same
/// `aver_type_str_of(...).trim() != "Unit"` predicate `emit_fn_body`
/// uses for its drop / local.set decisions. `Ok(None)` signals an
/// unsupported variant; the caller falls back to the `ResolvedExpr`
/// emitter for the whole fn.
fn emit_mir_expr(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    match &expr.node {
        MirExpr::Literal(lit) => match &lit.node {
            Literal::Int(n) => {
                func.instruction(&Instruction::I64Const(*n));
                Ok(Some(true))
            }
            Literal::Float(f) => {
                func.instruction(&Instruction::F64Const((*f).into()));
                Ok(Some(true))
            }
            Literal::Bool(b) => {
                func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
                Ok(Some(true))
            }
            Literal::Unit => Ok(Some(false)),
            Literal::Str(s) => {
                // Mirror of `emit_expr`'s `Literal::Str` arm: passive
                // data segment → `array.new_data $string $seg` with
                // offset 0, size len.
                let bytes = s.as_bytes();
                let seg_idx =
                    ctx.registry
                        .string_literal_segment(bytes)
                        .ok_or(WasmGcError::Validation(format!(
                            "String literal `{s:?}` was not registered in the data segment table"
                        )))?;
                let string_type_idx =
                    ctx.registry
                        .string_array_type_idx
                        .ok_or(WasmGcError::Validation(
                            "String literal reachable but no String type slot allocated".into(),
                        ))?;
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32Const(bytes.len() as i32));
                func.instruction(&Instruction::ArrayNewData {
                    array_type_index: string_type_idx,
                    array_data_index: seg_idx,
                });
                Ok(Some(true))
            }
        },
        MirExpr::Local(local) => {
            // The MIR `LocalId` is the resolver slot index = wasm local
            // index 1:1 (mirror of `ResolvedExpr::Resolved { slot }`).
            func.instruction(&Instruction::LocalGet(local.node.slot.0));
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            // Read the operand type from the MIR type stamp — the
            // canary for the whole port. Aver's checker proved both
            // operands share a type, so the LHS suffices. Wave 0
            // covers numeric operands only; `Str` (string concat / eq)
            // and compound types (variant / record / list eq helpers)
            // fall back to the `ResolvedExpr` emitter.
            match bop.lhs.ty() {
                Some(Type::Int) | Some(Type::Float) => {
                    if emit_mir_numeric_binop(func, bop, slots, ctx)?.is_none() {
                        return Ok(None);
                    }
                    Ok(Some(true))
                }
                _ => Ok(None),
            }
        }
        MirExpr::Neg(inner) => {
            // Mirror of `emit_expr`'s `Neg` arm. Float keeps the IEEE
            // sign bit via `f64.neg`; Int has no dedicated insn and
            // lowers to `i64.const 0; <operand>; i64.sub`.
            let inner_ty = wasm_type_of(inner, ctx.registry)?;
            if inner_ty == Some(ValType::F64) {
                if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                func.instruction(&Instruction::F64Neg);
            } else {
                func.instruction(&Instruction::I64Const(0));
                if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                func.instruction(&Instruction::I64Sub);
            }
            Ok(Some(true))
        }
        MirExpr::Return(inner) => {
            // Not produced by the current HIR → MIR lowering (carried
            // for symmetry with the Rust walker); emit the value then
            // a wasm `return`.
            let Some(produces) = emit_mir_expr(func, inner, slots, ctx)? else {
                return Ok(None);
            };
            func.instruction(&Instruction::Return);
            Ok(Some(produces))
        }
        MirExpr::Let(spanned_let) => {
            let l = &spanned_let.node;
            if l.binding_name.is_empty() {
                // Synthetic let (non-tail `Stmt::Expr` intermediate, or
                // `_ = expr` discard — both lower to an empty
                // `binding_name`). No source ident; fall back to HIR,
                // same as the Rust walker.
                return Ok(None);
            }
            // Mirror of `emit_fn_body`'s `Binding` arm.
            let Some(value_produces) = emit_mir_expr(func, &l.value, slots, ctx)? else {
                return Ok(None);
            };
            let slot = ctx
                .self_local_slot(&l.binding_name)
                .ok_or(WasmGcError::Validation(format!(
                    "binding `{}` has no resolver slot",
                    l.binding_name
                )))?;
            // Unit-typed values push nothing; the slot may also be an
            // i32 placeholder out of `by_slot` range (preserved for
            // resolver index alignment) — neither stores.
            if value_produces && (slot as usize) < slots.by_slot.len() {
                func.instruction(&Instruction::LocalSet(slot));
            }
            // The chain's tail is the return value left on the stack.
            emit_mir_expr(func, &l.body, slots, ctx)
        }
        MirExpr::Call(spanned_call) => {
            let call = &spanned_call.node;
            match call.callee {
                MirCallee::Fn(fn_id) => {
                    // Mirror of `emit_expr`'s `ResolvedCallee::Fn` arm.
                    match ctx.fn_map.by_id.get(&fn_id) {
                        None => {
                            // No wasm idx: a `Fn(..)` value parked in a
                            // local slot (verify-only higher-order) emits
                            // a polymorphic `unreachable`; anything else
                            // is a hard error — identical to `emit_expr`.
                            let name = ctx.symbol_table.fn_entry(fn_id).key.name.clone();
                            if ctx.self_local_slot(&name).is_some() {
                                func.instruction(&Instruction::Unreachable);
                                Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
                            } else {
                                Err(WasmGcError::Validation(format!(
                                    "call to unknown fn `{name}` (FnId {fn_id:?})"
                                )))
                            }
                        }
                        Some(entry) => {
                            let wasm_idx = entry.wasm_idx;
                            if emit_mir_args_then_call(func, &call.args, slots, ctx, wasm_idx)?
                                .is_none()
                            {
                                return Ok(None);
                            }
                            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
                        }
                    }
                }
                MirCallee::Builtin(id) => {
                    // Resolve the dotted name lowering interned for this
                    // `BuiltinId`, then mirror `emit_dotted_builtin`'s
                    // first branch: a builtin with a registered helper
                    // wasm fn is just "push args, call $idx". List
                    // cons/empty (intercepted by the `ResolvedExpr` Call
                    // arm before `emit_dotted_builtin`) and every custom
                    // inline lowering (effects, Args.get, Float / List /
                    // Map / Vector / String, wasip2) are NOT registered
                    // helpers, so they fall back to the `ResolvedExpr`
                    // emitter.
                    // An out-of-range `BuiltinId` is a lowering-invariant
                    // violation (every `MirCallee::Builtin` is minted via
                    // `program.intern_builtin`, so `id` always indexes
                    // `program.builtins`); fall back safely rather than panic.
                    let Some(dotted) = ctx.mir_builtins.and_then(|names| names.get(id.0 as usize))
                    else {
                        return Ok(None);
                    };
                    let dotted = dotted.as_str();
                    if (dotted == "List.prepend" && call.args.len() == 2)
                        || (dotted == "List.empty" && call.args.is_empty())
                    {
                        return Ok(None);
                    }
                    match ctx.fn_map.builtins.get(dotted) {
                        Some(&wasm_idx) => {
                            if emit_mir_args_then_call(func, &call.args, slots, ctx, wasm_idx)?
                                .is_none()
                            {
                                return Ok(None);
                            }
                            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
                        }
                        None => Ok(None),
                    }
                }
                MirCallee::Intrinsic(intr) => {
                    // Mirror of `emit_expr`'s `Intrinsic` arm: route the
                    // bare intrinsic name through the registered-builtin
                    // fast path. (Buffer intrinsics aren't produced on the
                    // wasm-gc path — it skips `buffer_build` — so this is
                    // effectively unreachable; kept for parity.)
                    match ctx.fn_map.builtins.get(intr.name()) {
                        Some(&wasm_idx) => {
                            if emit_mir_args_then_call(func, &call.args, slots, ctx, wasm_idx)?
                                .is_none()
                            {
                                return Ok(None);
                            }
                            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
                        }
                        None => Ok(None),
                    }
                }
                // First-class local-slot calls are higher-order (wasm-gc
                // has no first-class fn lowering) → fall back.
                MirCallee::LocalSlot { .. } => Ok(None),
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            // Mirror of `emit_tail_call` (emit.rs): validate the target,
            // emit args, then the shared return-call instruction. The
            // `return_call` makes this a terminator; in tail position
            // `emit_fn_body_via_mir`'s trailing `End` is unreachable but
            // valid, exactly as the `ResolvedExpr` path.
            let tc = &spanned_tc.node;
            let wasm_idx = match ctx.fn_map.by_id.get(&tc.target) {
                Some(entry) => entry.wasm_idx,
                None => {
                    let name = ctx.symbol_table.fn_entry(tc.target).key.canonical();
                    return Err(WasmGcError::Validation(format!(
                        "tail call to unknown fn `{name}` (FnId {:?})",
                        tc.target
                    )));
                }
            };
            for arg in &tc.args {
                if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
                    return Ok(None);
                }
            }
            emit_return_call_insn(func, wasm_idx, ctx.self_wasm_idx);
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        // FnValue (a fn referenced as a value) is higher-order — wasm-gc
        // has no first-class fn representation — so it falls back to the
        // `ResolvedExpr` emitter pending a verified byte-identical shape.
        _ => Ok(None),
    }
}

/// Emit each MIR `arg` (returning `None` if any falls outside the
/// supported subset, propagated as a whole-fn fallback) then
/// `call $wasm_idx`. Shared by the `Fn` / `Builtin` / `Intrinsic`
/// callee arms; the caller adds the `produces_value` read from the
/// call expr's own type stamp.
fn emit_mir_args_then_call(
    func: &mut Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    wasm_idx: u32,
) -> Result<Option<()>, WasmGcError> {
    for arg in args {
        if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::Call(wasm_idx));
    Ok(Some(()))
}

/// The numeric (`Int` / `Float`) tail of `emit_expr`'s `BinOp` arm —
/// byte-for-byte. Returns `None` if an operand falls outside the
/// supported subset (propagated as whole-fn fallback). The I64 / F64
/// instruction selection reads `wasm_type_of`, identical to the
/// `ResolvedExpr` path, so `Int op Float` promotion matches.
fn emit_mir_numeric_binop(
    func: &mut Function,
    bop: &crate::ir::mir::MirBinOp,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let l = &bop.lhs;
    let r = &bop.rhs;
    let l_ty = wasm_type_of(l, ctx.registry)?;
    let r_ty = wasm_type_of(r, ctx.registry)?;
    let operand = if l_ty == Some(ValType::F64) || r_ty == Some(ValType::F64) {
        Some(ValType::F64)
    } else {
        l_ty
    };
    if emit_mir_expr(func, l, slots, ctx)?.is_none() {
        return Ok(None);
    }
    if operand == Some(ValType::F64) && l_ty == Some(ValType::I64) {
        func.instruction(&Instruction::F64ConvertI64S);
    }
    if emit_mir_expr(func, r, slots, ctx)?.is_none() {
        return Ok(None);
    }
    if operand == Some(ValType::F64) && r_ty == Some(ValType::I64) {
        func.instruction(&Instruction::F64ConvertI64S);
    }
    let inst = match (operand, bop.op) {
        (Some(ValType::F64), BinOp::Add) => Instruction::F64Add,
        (Some(ValType::F64), BinOp::Sub) => Instruction::F64Sub,
        (Some(ValType::F64), BinOp::Mul) => Instruction::F64Mul,
        (Some(ValType::F64), BinOp::Div) => Instruction::F64Div,
        (Some(ValType::F64), BinOp::Eq) => Instruction::F64Eq,
        (Some(ValType::F64), BinOp::Neq) => Instruction::F64Ne,
        (Some(ValType::F64), BinOp::Lt) => Instruction::F64Lt,
        (Some(ValType::F64), BinOp::Gt) => Instruction::F64Gt,
        (Some(ValType::F64), BinOp::Lte) => Instruction::F64Le,
        (Some(ValType::F64), BinOp::Gte) => Instruction::F64Ge,
        (_, BinOp::Add) => Instruction::I64Add,
        (_, BinOp::Sub) => Instruction::I64Sub,
        (_, BinOp::Mul) => Instruction::I64Mul,
        (_, BinOp::Div) => Instruction::I64DivS,
        (_, BinOp::Eq) => Instruction::I64Eq,
        (_, BinOp::Neq) => Instruction::I64Ne,
        (_, BinOp::Lt) => Instruction::I64LtS,
        (_, BinOp::Gt) => Instruction::I64GtS,
        (_, BinOp::Lte) => Instruction::I64LeS,
        (_, BinOp::Gte) => Instruction::I64GeS,
    };
    func.instruction(&inst);
    Ok(Some(()))
}

// ---------------------------------------------------------------------------
// Coverage diagnostic (mirrors `crate::codegen::rust::from_mir`)
// ---------------------------------------------------------------------------

/// Wave-0 backend reach over a lowered [`MirProgram`]: how many fns the
/// wasm-gc MIR walker emits standalone vs. how many would fall back to
/// the `ResolvedExpr` emitter. Mirror of
/// [`crate::codegen::rust::from_mir::CoverageReport`]; drives
/// `aver compile --explain-mir-coverage --target wasm-gc`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CoverageReport {
    /// Total fns in the lowered program.
    pub total: usize,
    /// Fns whose entire body the walker emits standalone.
    pub mir_covered: usize,
    /// Fns that hit at least one unsupported variant (HIR fallback).
    pub hir_fallback: usize,
}

impl CoverageReport {
    /// Walker reach as a fraction of total fns. `0.0` for an empty
    /// program.
    pub fn ratio(&self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            self.mir_covered as f64 / self.total as f64
        }
    }
}

/// Walk every fn in `program` and report wasm-gc backend reach. Uses
/// the pure structural predicate [`mir_expr_coverable`] (no emission /
/// registry needed) so the diagnostic can run from the CLI without a
/// full module context. The predicate is kept in lock-step with
/// [`emit_mir_expr`]'s `Some` / `None` decisions; the byte-differential
/// test is the gate that catches drift.
pub fn coverage_report(program: &MirProgram) -> CoverageReport {
    let mut report = CoverageReport::default();
    for (_, mir_fn) in program.iter() {
        report.total += 1;
        if mir_expr_coverable(&mir_fn.body) {
            report.mir_covered += 1;
        } else {
            report.hir_fallback += 1;
        }
    }
    report
}

/// `true` when [`emit_mir_expr`] would emit `expr` standalone (no
/// `None`). Structural mirror of the emitter's match arms.
fn mir_expr_coverable(expr: &Spanned<MirExpr>) -> bool {
    match &expr.node {
        MirExpr::Literal(_) | MirExpr::Local(_) => true,
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            matches!(bop.lhs.ty(), Some(Type::Int) | Some(Type::Float))
                && mir_expr_coverable(&bop.lhs)
                && mir_expr_coverable(&bop.rhs)
        }
        MirExpr::Neg(inner) | MirExpr::Return(inner) => mir_expr_coverable(inner),
        MirExpr::Let(spanned_let) => {
            let l = &spanned_let.node;
            !l.binding_name.is_empty()
                && mir_expr_coverable(&l.value)
                && mir_expr_coverable(&l.body)
        }
        MirExpr::Call(spanned_call) => {
            matches!(spanned_call.node.callee, MirCallee::Fn(_))
                && spanned_call.node.args.iter().all(mir_expr_coverable)
        }
        MirExpr::TailCall(spanned_tc) => spanned_tc.node.args.iter().all(mir_expr_coverable),
        _ => false,
    }
}
