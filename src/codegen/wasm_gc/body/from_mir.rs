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
//! ## Scope (waves 0–4a)
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
//! Wave 2 (builtins, breadth only):
//! - `Call` with `MirCallee::Builtin(BuiltinId)` / `MirCallee::Intrinsic`
//!   — mirror of `emit_dotted_builtin`'s registered-helper first branch
//!   (push args, `call $idx`). The dotted name comes from
//!   `EmitCtx::mir_builtins`. Custom-inline builtins (effects, Args.get,
//!   Float / String / List / Map / Vector, wasip2) + `List.prepend` /
//!   `List.empty` fall back — that "depth" is a later sub-wave.
//!
//! Wave 3a/3b (primitive-subject match):
//! - `Match` over a `Bool` subject (a single `if`/`else`), an `Int`
//!   subject (an `i64.eq` cascade, wildcard required), or a `String`
//!   subject (subject stashed in the reserved scratch, then a
//!   `__wasmgc_string_eq` cascade with the first non-literal arm as the
//!   default) — mirror of `emit_match` / `emit_int_match_cascade` /
//!   `emit_string_match`. Any constructor / list / tuple arm pattern, or
//!   a shape `emit_match` rejects outright, falls back. (The dispatch
//!   checks arm patterns before the subject type, the reverse of
//!   `emit_match`'s order, but the two are equivalent: typecheck forbids
//!   a primitive subject from carrying a constructor / list / tuple arm,
//!   so neither path can reach a branch the other wouldn't.)
//!
//! Wave 4a (built-in carrier match):
//! - `Match` over a `Result<T,E>` or `Option<T>` subject — mirror of
//!   `emit_result_match` / `emit_option_match`: stash the subject in the
//!   reserved scratch, test the tag field (struct field 0), and on each
//!   branch extract the payload (field 1 for `Ok`/`Some`, field 2 for
//!   `Err`) into the arm's binding slot before emitting the body. A
//!   wildcard is the `Err` / `None` catch-all. An `Option` match whose
//!   subject is `Map.get(m, k)` falls back (it has a fused HIR-only
//!   lowering). User-variant / list / tuple matches are wave 4b–4d.
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
use crate::ir::mir::{
    BuiltinCtor, MirCallee, MirCtor, MirExpr, MirFn, MirMatch, MirMatchArm, MirPattern, MirProgram,
};
use crate::types::Type;

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, aver_to_wasm};
use super::emit::{emit_return_call_insn, emit_string_literal_bytes};
use super::infer::{aver_type_canonical, aver_type_str_of, wasm_type_of};
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
        MirExpr::Match(spanned_match) => emit_mir_match(func, &spanned_match.node, slots, ctx),
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

/// Mirror of `emit_match` (emit.rs) for the wave-3a primitive-subject
/// shapes: `Bool` (a single `if`/`else`) and `Int` (an `i64.eq`
/// cascade). Any arm carrying a constructor / list / tuple pattern is
/// wave 4 → `Ok(None)` (whole-fn fallback). `String`-subject matches
/// (which need the reserved subject scratch + `__wasmgc_string_eq`) and
/// any other subject type also fall back. Shapes `emit_match` rejects
/// outright (a `Bool` match without exactly 2 true/false/wildcard arms,
/// an `Int` match without a wildcard, a bind pattern on a primitive
/// subject) return `Ok(None)` here — the `ResolvedExpr` emitter then
/// reproduces `emit_match`'s exact error, so behavior is unchanged.
fn emit_mir_match(
    func: &mut Function,
    m: &MirMatch,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    if m.arms.is_empty() {
        return Err(WasmGcError::Validation("match has no arms".into()));
    }
    // Patterns not yet covered: list (wave 4b), tuple (wave 4c), and
    // user-variant constructors (wave 4d) — fall back. Built-in
    // `Result` / `Option` constructor patterns (wave 4a) are handled
    // below.
    if m.arms.iter().any(|a| {
        matches!(
            a.pattern,
            MirPattern::EmptyList
                | MirPattern::Cons { .. }
                | MirPattern::Tuple(_)
                | MirPattern::Ctor {
                    ctor: MirCtor::User(_),
                    ..
                }
        )
    }) {
        return Ok(None);
    }

    // Result/block type — mirror of `emit_match`. The first arm's body
    // type is the match's type (typecheck proved all arms agree); a
    // `Unit` match lowers to `BlockType::Empty` and produces no value.
    let result_ty_str = aver_type_canonical(&m.arms[0].body, ctx.return_type, ctx.registry);
    let block_ty = match aver_to_wasm(&result_ty_str, Some(ctx.registry))? {
        Some(v) => wasm_encoder::BlockType::Result(v),
        None => wasm_encoder::BlockType::Empty,
    };
    let produces = !matches!(block_ty, wasm_encoder::BlockType::Empty);

    // Built-in `Result<T,E>` / `Option<T>` matches — tag-based dispatch.
    // `emit_match` checks Result before Option; mirror that order. An
    // Option match whose subject is `Map.get(m, k)` takes `emit_match`'s
    // fused (no-Option-alloc) path — defer that optimization, fall back.
    if m.arms.iter().any(arm_is_mir_result_ctor) {
        return Ok(emit_mir_result_match(func, m, block_ty, slots, ctx)?.map(|()| produces));
    }
    if m.arms.iter().any(arm_is_mir_option_ctor) {
        if subject_is_map_get(&m.subject, ctx) {
            return Ok(None);
        }
        return Ok(emit_mir_option_match(func, m, block_ty, slots, ctx)?.map(|()| produces));
    }

    match aver_type_str_of(&m.subject).trim() {
        "Bool" => {
            // Mirror of `emit_match`'s Bool special-case: a single
            // `if subject { true_body } else { false_body }`.
            if m.arms.len() != 2 {
                return Ok(None);
            }
            let mut true_body: Option<&Spanned<MirExpr>> = None;
            let mut false_body: Option<&Spanned<MirExpr>> = None;
            for arm in &m.arms {
                match &arm.pattern {
                    MirPattern::Literal(Literal::Bool(true)) => true_body = Some(&arm.body),
                    MirPattern::Literal(Literal::Bool(false)) => false_body = Some(&arm.body),
                    MirPattern::Wildcard => {
                        if true_body.is_none() {
                            true_body = Some(&arm.body);
                        } else {
                            false_body = Some(&arm.body);
                        }
                    }
                    _ => return Ok(None),
                }
            }
            let (Some(t), Some(f)) = (true_body, false_body) else {
                return Ok(None);
            };
            if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::If(block_ty));
            if emit_mir_expr(func, t, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::Else);
            if emit_mir_expr(func, f, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::End);
            Ok(Some(produces))
        }
        "Int" => {
            // Mirror of `emit_match`'s Int path + `emit_int_match_cascade`:
            // first-applicable wins, wildcard required.
            let mut wildcard_body: Option<&Spanned<MirExpr>> = None;
            let mut typed_arms: Vec<(i64, &Spanned<MirExpr>)> = Vec::new();
            for arm in &m.arms {
                match &arm.pattern {
                    MirPattern::Literal(Literal::Int(n)) => typed_arms.push((*n, &arm.body)),
                    MirPattern::Wildcard => {
                        // First wildcard wins (source-order semantics).
                        if wildcard_body.is_none() {
                            wildcard_body = Some(&arm.body);
                        }
                    }
                    _ => return Ok(None),
                }
            }
            let Some(wildcard) = wildcard_body else {
                return Ok(None);
            };
            if emit_mir_int_cascade(
                func,
                &m.subject,
                &typed_arms,
                wildcard,
                block_ty,
                slots,
                ctx,
            )?
            .is_none()
            {
                return Ok(None);
            }
            Ok(Some(produces))
        }
        "String" => {
            if emit_mir_string_match(func, m, block_ty, slots, ctx)?.is_none() {
                return Ok(None);
            }
            Ok(Some(produces))
        }
        // Non-primitive subjects (sum/record/etc.) fall back.
        _ => Ok(None),
    }
}

/// Mirror of `emit_string_match` (emit.rs): stash the subject in the
/// reserved `(ref null eq)` scratch, then a cascade of
/// `if __wasmgc_string_eq(subject, "lit") { body } else { … }` with the
/// first non-literal arm (typically `_`) as the innermost default.
/// Returns `None` (whole-fn fallback) if any subtree is unsupported or
/// the shape lacks the scratch / default the `ResolvedExpr` emitter
/// also requires.
fn emit_mir_string_match(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "String match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let eq_idx = ctx
        .fn_map
        .builtins
        .get("__wasmgc_string_eq")
        .copied()
        .ok_or(WasmGcError::Validation(
            "String match: __wasmgc_string_eq builtin wasn't registered".into(),
        ))?;
    let s_idx = ctx
        .registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String match needs the String type slot allocated".into(),
        ))?;

    // Stash the subject; read once per arm (cast `(ref null eq)` back to
    // `(ref null $string)` for `__wasmgc_string_eq`'s param shape).
    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));

    // Literal-string arms in source order, then the first non-literal
    // arm as the single default (mirror of `emit_string_match`).
    let mut literal_arms: Vec<(&str, &Spanned<MirExpr>)> = Vec::new();
    let mut default_body: Option<&Spanned<MirExpr>> = None;
    for arm in &m.arms {
        if let MirPattern::Literal(Literal::Str(s)) = &arm.pattern {
            literal_arms.push((s.as_str(), &arm.body));
        } else if default_body.is_none() {
            default_body = Some(&arm.body);
        }
    }
    let Some(default_body) = default_body else {
        // `emit_string_match` raises a Validation error here; fall back
        // so the `ResolvedExpr` emitter reproduces it.
        return Ok(None);
    };

    let mut ends_to_close = 0usize;
    for (lit, body) in &literal_arms {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNullable(
            wasm_encoder::HeapType::Concrete(s_idx),
        ));
        emit_string_literal_bytes(func, lit.as_bytes(), ctx)?;
        func.instruction(&Instruction::Call(eq_idx));
        func.instruction(&Instruction::If(block_ty));
        if emit_mir_expr(func, body, slots, ctx)?.is_none() {
            return Ok(None);
        }
        func.instruction(&Instruction::Else);
        ends_to_close += 1;
    }
    if emit_mir_expr(func, default_body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    for _ in 0..ends_to_close {
        func.instruction(&Instruction::End);
    }
    Ok(Some(()))
}

/// Mirror of `emit_int_match_cascade` (emit.rs): `subject == lit ?
/// body : <rest>`, recomputing the subject per arm (no scratch slot).
/// Returns `None` if any subtree falls outside the supported subset.
fn emit_mir_int_cascade(
    func: &mut Function,
    subject: &Spanned<MirExpr>,
    typed_arms: &[(i64, &Spanned<MirExpr>)],
    wildcard: &Spanned<MirExpr>,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let Some(((pat_lit, body), rest)) = typed_arms.split_first() else {
        // No typed arms left — emit the wildcard body.
        if emit_mir_expr(func, wildcard, slots, ctx)?.is_none() {
            return Ok(None);
        }
        return Ok(Some(()));
    };
    if emit_mir_expr(func, subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::I64Const(*pat_lit));
    func.instruction(&Instruction::I64Eq);
    func.instruction(&Instruction::If(block_ty));
    if emit_mir_expr(func, body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Else);
    if emit_mir_int_cascade(func, subject, rest, wildcard, block_ty, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// `true` for an arm whose pattern is a built-in `Result.Ok` /
/// `Result.Err` constructor (mirror of `arm_is_result_pattern_resolved`).
fn arm_is_mir_result_ctor(arm: &MirMatchArm) -> bool {
    matches!(
        &arm.pattern,
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(BuiltinCtor::ResultOk | BuiltinCtor::ResultErr),
            ..
        }
    )
}

/// `true` for an arm whose pattern is a built-in `Option.Some` /
/// `Option.None` constructor (mirror of `arm_is_option_pattern_resolved`).
fn arm_is_mir_option_ctor(arm: &MirMatchArm) -> bool {
    matches!(
        &arm.pattern,
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(BuiltinCtor::OptionSome | BuiltinCtor::OptionNone),
            ..
        }
    )
}

/// `true` when `subject` is `Map.get(m, k)` — the fused-match shape
/// `emit_match` lowers without allocating an `Option<V>`. Deferred
/// (wave 4a falls back) so the plain Option-match emit can't diverge
/// from `emit_map_get_match_fused`.
fn subject_is_map_get(subject: &Spanned<MirExpr>, ctx: &EmitCtx<'_>) -> bool {
    if let MirExpr::Call(call) = &subject.node
        && let MirCallee::Builtin(id) = call.node.callee
        && let Some(name) = ctx.mir_builtins.and_then(|names| names.get(id.0 as usize))
    {
        return name == "Map.get" && call.node.args.len() == 2;
    }
    false
}

/// The payload-binding wasm slot for a constructor-pattern arm, mirror
/// of `emit_option_match` / `emit_result_match`'s
/// `arm.binding_slots.get().first()` + `slot != u16::MAX` guard. The
/// MIR `bindings` are seeded from the resolver's `binding_slots`
/// (`lower.rs::take_pattern_bindings`), so the slot is identical; an
/// ignored / absent binding is `u16::MAX` → `None` (no extraction).
fn ctor_arm_binding_slot(arm: &MirMatchArm) -> Option<u32> {
    if let MirPattern::Ctor { bindings, .. } = &arm.pattern
        && let Some(slot) = bindings.first()
        && slot.0 != u32::from(u16::MAX)
    {
        return Some(slot.0);
    }
    None
}

/// Mirror of `emit_option_match` (emit.rs): stash the subject, test the
/// tag field (struct field 0 == 1 ⇒ `Some`), extract the payload
/// (field 1) into the `Some` arm's binding slot when present, then the
/// `if`/`else` bodies. The wildcard arm is the `None` catch-all.
fn emit_mir_option_match(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Option match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(&m.subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let opt_idx = ctx
        .registry
        .option_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option match: subject type `{subject_ty}` is not a registered Option<T>"
        )))?;

    // Locate Some / None arms; a wildcard is the None catch-all (then
    // Some) — same convention as `emit_option_match`.
    let mut some_arm: Option<&MirMatchArm> = None;
    let mut none_arm: Option<&MirMatchArm> = None;
    for arm in &m.arms {
        match &arm.pattern {
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
                ..
            } => some_arm = Some(arm),
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                ..
            } => none_arm = Some(arm),
            MirPattern::Wildcard => {
                if none_arm.is_none() {
                    none_arm = Some(arm);
                } else if some_arm.is_none() {
                    some_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let (Some(some_arm), Some(none_arm)) = (some_arm, none_arm) else {
        // `emit_option_match` raises a Validation error here; fall back.
        return Ok(None);
    };

    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));

    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(opt_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(block_ty));

    if let Some(slot) = ctor_arm_binding_slot(some_arm) {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(opt_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: opt_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::LocalSet(slot));
    }
    if emit_mir_expr(func, &some_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Else);
    if emit_mir_expr(func, &none_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// Mirror of `emit_result_match` (emit.rs): tag field 0 == 1 ⇒ `Ok`
/// (payload field 1), else `Err` (payload field 2). Each arm extracts
/// its payload into its binding slot when present. A wildcard is the
/// `Err` catch-all (then `Ok`).
fn emit_mir_result_match(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Result match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(&m.subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Result match: subject type `{subject_ty}` is not a registered Result<T,E>"
        )))?;

    let mut ok_arm: Option<&MirMatchArm> = None;
    let mut err_arm: Option<&MirMatchArm> = None;
    for arm in &m.arms {
        match &arm.pattern {
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::ResultOk),
                ..
            } => ok_arm = Some(arm),
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::ResultErr),
                ..
            } => err_arm = Some(arm),
            MirPattern::Wildcard => {
                if err_arm.is_none() {
                    err_arm = Some(arm);
                } else if ok_arm.is_none() {
                    ok_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let (Some(ok_arm), Some(err_arm)) = (ok_arm, err_arm) else {
        return Ok(None);
    };

    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));

    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(res_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(block_ty));

    if let Some(slot) = ctor_arm_binding_slot(ok_arm) {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: res_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::LocalSet(slot));
    }
    if emit_mir_expr(func, &ok_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Else);
    if let Some(slot) = ctor_arm_binding_slot(err_arm) {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: res_idx,
            field_index: 2,
        });
        func.instruction(&Instruction::LocalSet(slot));
    }
    if emit_mir_expr(func, &err_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
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
        MirExpr::Match(spanned_match) => {
            // Coarse, ctx-free mirror of `emit_mir_match`'s dispatch (the
            // predicate has no registry, so it can't model the Map.get
            // fused-Option fallback — a tolerable over-count, since this
            // only feeds `--explain-mir-coverage`; the real per-fn
            // dispatch is what the wire-up + differential test use).
            // List / tuple / user-variant arms are wave 4b/4c/4d.
            let m = &spanned_match.node;
            let unsupported_pat = m.arms.iter().any(|a| {
                matches!(
                    a.pattern,
                    MirPattern::EmptyList
                        | MirPattern::Cons { .. }
                        | MirPattern::Tuple(_)
                        | MirPattern::Ctor {
                            ctor: MirCtor::User(_),
                            ..
                        }
                )
            });
            // A primitive-subject match takes the Bool/Int/String branches
            // (literal / wildcard arms only; `Bind` falls back); a
            // Result/Option match carries built-in constructor arms.
            let is_primitive = matches!(m.subject.ty(), Some(Type::Bool | Type::Int | Type::Str))
                && !m
                    .arms
                    .iter()
                    .any(|a| matches!(a.pattern, MirPattern::Bind(..) | MirPattern::Ctor { .. }));
            let is_result_or_option = m.arms.iter().any(arm_is_mir_result_ctor)
                || m.arms.iter().any(arm_is_mir_option_ctor);
            !m.arms.is_empty()
                && !unsupported_pat
                && (is_primitive || is_result_or_option)
                && mir_expr_coverable(&m.subject)
                && m.arms.iter().all(|a| mir_expr_coverable(&a.body))
        }
        _ => false,
    }
}
