//! wasm-gc backend: emit function bodies from Core MIR.
//!
//! Mirror of [`super::emit::emit_expr`] that walks
//! [`crate::ir::mir::MirExpr`] instead of `ResolvedExpr` and emits
//! **byte-identical** wasm — one semantic walker per construct lives in
//! MIR, and every backend reads it instead of forking `ResolvedExpr`.
//!
//! [`emit_mir_expr`] is the dispatcher. Any construct it does not cover
//! returns `Ok(None)`; the caller ([`emit_fn_body_via_mir`]) then
//! discards `func` and re-runs the `ResolvedExpr` emitter for the whole
//! function. Two byte-differential tests compile
//! every single-file example and every multi-module game both ways (MIR
//! on vs forced off) and assert the modules are identical — the gate
//! that keeps each mirror exact.
//!
//! ## Covered constructs
//!
//! Dispatcher ([`emit_mir_expr`], this module): `Literal`, `Local`
//! (`local.get` of the resolver slot), numeric `BinOp` / `Neg`,
//! `Return`, `Let` (both named bindings and the statement-sequencing
//! synthetic lets — `_ = expr` discards and non-tail `Stmt::Expr`, which
//! emit the value, `drop` it if it produced one, then the body),
//! `Call(Fn)` / `TailCall`, `Project` (mirroring `emit_attr_get`), and
//! the `Tuple` / `MapLiteral` literals. Higher-order callees (`FnValue`,
//! `LocalSlot`) fall back. Registered-helper builtins and effect imports (`Console.*` /
//! `Disk.*` / `Tcp.*` / `Http.*` / `Random.*` / `Time.*`, each carrying
//! the host's `caller_fn` stamp) go through the `fn_map.builtins` /
//! `fn_map.effects` lookups here.
//!
//! - [`pattern_match`] — `Match` over `Bool` / `Int` / `String`,
//!   `Option` / `Result` / `List` carriers, and user sum types.
//! - [`constructors`] — `Construct` (user variants, `Option`, `Result`).
//! - [`records`] — `RecordCreate` / `RecordUpdate`.
//! - [`collections`] — `List` literals.
//! - [`builtins`] — the custom-inline `Float` / `Int` / `Bool` scalar
//!   ops, `Char.toCode`, the custom-inline `String` ops (`length` /
//!   `byteLength` / `split` / `join`), the `List` / `Vector` / `Map`
//!   families, the
//!   fused `Option.withDefault(Vector.get(v, i), <literal>)` /
//!   `Option.withDefault(Vector.set(v, i, x), v)` /
//!   `Result.withDefault(Int.mod(a, b), default)`, and the numeric
//!   `BinOp` tail.
//! - [`strings`] — `InterpolatedStr` and the `String` `BinOp` ops.
//! - [`control`] — `Try` (`?` propagation) and `IndependentProduct`
//!   (`(a, b, c)!` / `?!`).
//! - [`coverage`] — the `--explain-mir-coverage` predicate (diagnostic).
//!
//! Each submodule documents the exact `emit_expr` helper it mirrors and
//! the shapes that still fall back to the `ResolvedExpr` emitter.

pub(super) use std::collections::{HashMap, HashSet};

pub(super) use wasm_encoder::{Function, Instruction, ValType};

pub(super) use crate::ast::Spanned;
pub(super) use crate::ast::{BinOp, Literal};
pub(super) use crate::ir::CtorId;
pub(super) use crate::ir::SymbolTable;
pub(super) use crate::ir::hir::{ResolvedFnBody, ResolvedFnDef, ResolvedStmt};
pub(super) use crate::ir::mir::{
    BuiltinCtor, MirCallee, MirCtor, MirExpr, MirFn, MirIndependentProduct, MirMatch, MirMatchArm,
    MirPattern, MirProgram, MirRecordField, MirStrPart,
};
pub(super) use crate::types::Type;

pub(super) use super::super::WasmGcError;
pub(super) use super::super::types::{TypeRegistry, VariantInfo, aver_to_wasm, normalize_compound};
pub(super) use super::builtins::emit_args_get_inline;
pub(super) use super::emit::{
    emit_branch_marker, emit_caller_fn_idx, emit_default_value, emit_group_call,
    emit_return_call_insn, emit_string_literal_bytes, sum_or_record_eq_fn,
};
pub(super) use super::infer::{aver_type_canonical, aver_type_str_of, wasm_type_of};
pub(super) use super::slots::count_value_params;
pub(super) use super::{CallerFnCollector, EmitCtx, FnMap, SlotTable, Wasip2Lowering};

mod builtins;
mod collections;
mod constructors;
mod control;
mod coverage;
mod pattern_match;
mod records;
mod strings;

pub(super) use builtins::*;
pub(super) use collections::*;
pub(super) use constructors::*;
pub(super) use control::*;
pub(super) use pattern_match::*;
pub(super) use records::*;
pub(super) use strings::*;

pub use coverage::{CoverageReport, coverage_report};

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
    // off resolver-assigned names, so this set must stay HIR-sourced —
    // do not repopulate it from `MirExpr`.
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

/// Mirror of `emit_expr`'s `nullary_variant_idx`: if `operand` is a
/// nullary user-variant value (`Tile.Floor`, lowered to a `Construct`
/// with no args of a zero-field variant), return that variant struct's
/// type index — so `<expr> == Tile.Floor` can lower to a `ref.test`
/// rather than a structural eq-helper call, matching the oracle.
fn mir_nullary_variant_idx(operand: &Spanned<MirExpr>, ctx: &EmitCtx<'_>) -> Option<u32> {
    let MirExpr::Construct(c) = &operand.node else {
        return None;
    };
    let MirCtor::User(ctor_id) = c.node.ctor else {
        return None;
    };
    if !c.node.args.is_empty() {
        return None;
    }
    let info = mir_user_variant_info(ctor_id, ctx).ok()?;
    if info.fields.is_empty() {
        Some(info.type_idx)
    } else {
        None
    }
}

/// Emit instructions for a MIR `expr`, returning `Ok(Some(produces))`
/// where `produces` is `true` when evaluating `expr` leaves a value on
/// the stack (i.e. its type is not `Unit`) — the same
/// `aver_type_str_of(...).trim() != "Unit"` predicate `emit_fn_body`
/// uses for its drop / local.set decisions. `Ok(None)` signals an
/// unsupported variant; the caller falls back to the `ResolvedExpr`
/// emitter for the whole fn.
pub(crate) fn emit_mir_expr(
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
            // Read the operand type from the MIR type stamp. Aver's
            // checker proved both operands share a type, so the LHS
            // suffices. `String` operands take the dedicated concat / eq
            // / compare builtins; numeric operands the primitive op set;
            // compound types (variant / record eq helpers) fall back.
            if aver_type_str_of(&bop.lhs).trim() == "String" {
                return match emit_mir_string_binop(func, bop, slots, ctx)? {
                    Some(()) => Ok(Some(true)),
                    None => Ok(None),
                };
            }
            match bop.lhs.ty() {
                Some(Type::Int) | Some(Type::Float) => {
                    if emit_mir_numeric_binop(func, bop, slots, ctx)?.is_none() {
                        return Ok(None);
                    }
                    Ok(Some(true))
                }
                // `==` / `!=` on a user type — mirror of `emit_expr`'s
                // three sub-branches, in the SAME order so the bytes match:
                // (1) `<expr> == NullaryVariant` and (2) the flipped form
                // lower to a `ref.test` on the variant struct; (3) any
                // other sum / record / carrier compare goes through the
                // per-type `__eq_<T>` helper. `!=` appends `i32.eqz`.
                Some(lty) if matches!(bop.op, BinOp::Eq | BinOp::Neq) => {
                    let neq = matches!(bop.op, BinOp::Neq);
                    if let Some(vidx) = mir_nullary_variant_idx(&bop.rhs, ctx) {
                        if emit_mir_expr(func, &bop.lhs, slots, ctx)?.is_none() {
                            return Ok(None);
                        }
                        func.instruction(&Instruction::RefTestNonNull(
                            wasm_encoder::HeapType::Concrete(vidx),
                        ));
                        if neq {
                            func.instruction(&Instruction::I32Eqz);
                        }
                        return Ok(Some(true));
                    }
                    if let Some(vidx) = mir_nullary_variant_idx(&bop.lhs, ctx) {
                        if emit_mir_expr(func, &bop.rhs, slots, ctx)?.is_none() {
                            return Ok(None);
                        }
                        func.instruction(&Instruction::RefTestNonNull(
                            wasm_encoder::HeapType::Concrete(vidx),
                        ));
                        if neq {
                            func.instruction(&Instruction::I32Eqz);
                        }
                        return Ok(Some(true));
                    }
                    let Some(eq_fn) = sum_or_record_eq_fn(lty, ctx) else {
                        return Ok(None);
                    };
                    if emit_mir_expr(func, &bop.lhs, slots, ctx)?.is_none() {
                        return Ok(None);
                    }
                    if emit_mir_expr(func, &bop.rhs, slots, ctx)?.is_none() {
                        return Ok(None);
                    }
                    func.instruction(&Instruction::Call(eq_fn));
                    if neq {
                        func.instruction(&Instruction::I32Eqz);
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
                // DIAG: synthetic let coverage (emit value, drop-if-
                // produces, emit body) — reproducing the checkers divergence.
                let Some(value_produces) = emit_mir_expr(func, &l.value, slots, ctx)? else {
                    return Ok(None);
                };
                if value_produces {
                    func.instruction(&Instruction::Drop);
                }
                return emit_mir_expr(func, &l.body, slots, ctx);
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
                    // `Args.get` is intercepted *before* the effect /
                    // builtin dispatch in `emit_dotted_builtin` and expands
                    // to a custom inline (the `Args.len` loop building a
                    // `List<String>`), even though it is also registered in
                    // `fn_map.effects`. Reuse the oracle's inline verbatim —
                    // it is `ResolvedExpr`-free (func + slots + ctx) — so the
                    // bytes match and `Args.get` no longer forces a fallback.
                    if dotted == "Args.get" {
                        emit_args_get_inline(func, slots, ctx)?;
                        return Ok(Some(aver_type_str_of(expr).trim() != "Unit"));
                    }
                    // Registered effect import (`Console.*`, `Disk.*`,
                    // `Tcp.*`, `Http.*`, `Random.*`, `Time.*`, …) on the
                    // AverBridge target — mirror of `emit_dotted_builtin`'s
                    // `fn_map.effects` branch: emit args, push the
                    // `caller_fn` idx the host stamps onto the recorded
                    // effect, then `call` the import. (The wasip2 effect
                    // lowerings are a different target whose branches don't
                    // run on the AverBridge path the MIR emitter serves.)
                    if let Some(&effect_idx) = ctx.fn_map.effects.get(dotted) {
                        for arg in &call.args {
                            if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
                                return Ok(None);
                            }
                        }
                        emit_caller_fn_idx(func, ctx)?;
                        func.instruction(&Instruction::Call(effect_idx));
                        return Ok(Some(aver_type_str_of(expr).trim() != "Unit"));
                    }
                    // Native scalar builtins (Float / Int / Bool) lower to
                    // an inline instruction sequence, not a registered
                    // helper, so try them before the `fn_map.builtins`
                    // lookup (which would miss them).
                    match emit_mir_native_scalar_builtin(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // `List.*` custom-inline ops (helper dispatch +
                    // prepend / empty) — also not in `fn_map.builtins`.
                    match emit_mir_list_builtin(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // `Vector.*` custom-inline ops (len / new / get / set
                    // / fromList) — likewise not registered helpers.
                    match emit_mir_vector_builtin(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // `Map.*` per-instantiation helper dispatch — also not
                    // in `fn_map.builtins`.
                    match emit_mir_map_builtin(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // Fused `Option.withDefault(Vector.get(v, i), <literal>)`
                    // and `Option.withDefault(Vector.set(v, i, x), v)`.
                    // Other `withDefault` shapes fall through to fallback.
                    match emit_mir_option_with_default(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // `Result.withDefault` — the fused Euclidean-modulo
                    // form `(Int.mod(a, b), default)` and the boxed
                    // tag-dispatch form for every other Result.
                    match emit_mir_result_with_default(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
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
        MirExpr::Construct(spanned_ctor) => {
            // Mirror of `emit_expr`'s `Ctor` arm. A constructor always
            // produces a value (never `Unit`).
            let con = &spanned_ctor.node;
            let covered = match con.ctor {
                MirCtor::Builtin(BuiltinCtor::OptionSome) => {
                    if con.args.len() != 1 {
                        return Err(WasmGcError::Validation(format!(
                            "Option.Some constructor requires 1 arg, got {}",
                            con.args.len()
                        )));
                    }
                    emit_mir_option_constructor(func, Some(&con.args[0]), None, slots, ctx)?
                }
                MirCtor::Builtin(BuiltinCtor::OptionNone) => {
                    // Read T from the constructor's stamped type, mirror
                    // of the `ResolvedExpr` arm's hint derivation.
                    let stamped = aver_type_canonical(expr, ctx.return_type, ctx.registry);
                    let hint: String = stamped
                        .strip_prefix("Option<")
                        .and_then(|s| s.strip_suffix('>'))
                        .map(|inner| inner.to_string())
                        .unwrap_or_else(|| ctx.return_type.to_string());
                    emit_mir_option_constructor(func, None, Some(&hint), slots, ctx)?
                }
                MirCtor::Builtin(BuiltinCtor::ResultOk) => {
                    emit_mir_result_constructor(func, "Ok", con.args.first(), slots, ctx)?
                }
                MirCtor::Builtin(BuiltinCtor::ResultErr) => {
                    emit_mir_result_constructor(func, "Err", con.args.first(), slots, ctx)?
                }
                MirCtor::User(ctor_id) => {
                    let info = mir_user_variant_info(ctor_id, ctx)?;
                    emit_mir_constructor_with_args(func, info, &con.args, slots, ctx)?
                }
            };
            match covered {
                Some(()) => Ok(Some(aver_type_str_of(expr).trim() != "Unit")),
                None => Ok(None),
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            let rec = &spanned_rec.node;
            match emit_mir_record_create(func, &rec.type_name, &rec.fields, slots, ctx)? {
                Some(()) => Ok(Some(aver_type_str_of(expr).trim() != "Unit")),
                None => Ok(None),
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            let upd = &spanned_upd.node;
            match emit_mir_record_update(func, &upd.type_name, &upd.base, &upd.updates, slots, ctx)?
            {
                Some(()) => Ok(Some(aver_type_str_of(expr).trim() != "Unit")),
                None => Ok(None),
            }
        }
        MirExpr::Project(spanned_proj) => {
            // Mirror of `emit_attr_get`. A newtype `.field` is identity —
            // emit the base directly. Unknown / `Invalid` (namespace-
            // handle) record types fall back so the `ResolvedExpr`
            // emitter produces `emit_attr_get`'s diagnostic.
            let proj = &spanned_proj.node;
            let record_name = aver_type_str_of(&proj.base);
            if ctx.registry.newtype_underlying(&record_name).is_some() {
                return Ok(emit_mir_expr(func, &proj.base, slots, ctx)?
                    .map(|_| aver_type_str_of(expr).trim() != "Unit"));
            }
            let (Some(type_idx), Some(field_idx)) = (
                ctx.registry.record_type_idx(&record_name),
                ctx.registry.record_field_index(&record_name, &proj.field),
            ) else {
                return Ok(None);
            };
            if emit_mir_expr(func, &proj.base, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::StructGet {
                struct_type_index: type_idx,
                field_index: field_idx,
            });
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        MirExpr::Tuple(items) => {
            // Mirror of `emit_tuple_literal`: canonical from the elements'
            // stamped types, then push each element + `struct.new`.
            if items.len() < 2 {
                return Err(WasmGcError::Validation(format!(
                    "Tuple literal needs at least 2 elements; got {}",
                    items.len()
                )));
            }
            let elem_tys: Vec<String> = items.iter().map(aver_type_str_of).collect();
            let canonical = format!("Tuple<{}>", elem_tys.join(","))
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect::<String>();
            let tuple_idx =
                ctx.registry
                    .tuple_type_idx(&canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "Tuple literal: `{canonical}` slot not registered"
                    )))?;
            for item in items {
                if emit_mir_expr(func, item, slots, ctx)?.is_none() {
                    return Ok(None);
                }
            }
            func.instruction(&Instruction::StructNew(tuple_idx));
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        MirExpr::MapLiteral(entries) => {
            // Mirror of `emit_map_literal`: `Map.empty` then a `Map.set`
            // per entry. Canonical from the first entry's K/V stamped
            // types, or the sole registered `Map<K,V>` when empty.
            let canonical: String = if entries.is_empty() {
                if ctx.registry.map_order.len() == 1 {
                    ctx.registry.map_order[0].clone()
                } else {
                    return Err(WasmGcError::Validation(
                        "empty MapLiteral: cannot resolve Map<K,V> instantiation \
                         without context (multiple instantiations registered)"
                            .into(),
                    ));
                }
            } else {
                let k_aver = aver_type_str_of(&entries[0].0);
                let v_aver = aver_type_str_of(&entries[0].1);
                format!("Map<{},{}>", k_aver.trim(), v_aver.trim())
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect()
            };
            let (empty_fn, set_fn) = {
                let helpers =
                    ctx.fn_map
                        .map_helpers
                        .get(&canonical)
                        .ok_or(WasmGcError::Validation(format!(
                            "MapLiteral: helpers missing for `{canonical}`"
                        )))?;
                (helpers.empty, helpers.set)
            };
            func.instruction(&Instruction::Call(empty_fn));
            for (k_expr, v_expr) in entries {
                if emit_mir_expr(func, k_expr, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                if emit_mir_expr(func, v_expr, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                func.instruction(&Instruction::Call(set_fn));
            }
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        MirExpr::List(items) => match emit_mir_list_literal(func, expr, items, slots, ctx)? {
            Some(()) => Ok(Some(aver_type_str_of(expr).trim() != "Unit")),
            None => Ok(None),
        },
        MirExpr::Try(inner) => emit_mir_try(func, inner, slots, ctx),
        MirExpr::InterpolatedStr(parts) => emit_mir_interpolated_str(func, parts, slots, ctx),
        MirExpr::IndependentProduct(spanned_ip) => {
            emit_mir_independent_product(func, &spanned_ip.node, slots, ctx)
        }
        // `if cond { then } else { else }` — produced by the
        // `bool_match_to_if` optimizer from a two-arm `Bool` match.
        // Byte-equivalent to that match's lowering (`emit_mir_match`'s
        // `Bool` arm): the then-branch type is the result type (typecheck
        // proved both branches agree); a `Unit` if produces no value.
        MirExpr::IfThenElse(ite) => {
            let ite = &ite.node;
            let result_ty = aver_type_canonical(&ite.then_branch, ctx.return_type, ctx.registry);
            let block_ty = match aver_to_wasm(&result_ty, Some(ctx.registry))? {
                Some(v) => wasm_encoder::BlockType::Result(v),
                None => wasm_encoder::BlockType::Empty,
            };
            let produces = !matches!(block_ty, wasm_encoder::BlockType::Empty);
            if emit_mir_expr(func, &ite.cond, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::If(block_ty));
            if emit_mir_expr(func, &ite.then_branch, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::Else);
            if emit_mir_expr(func, &ite.else_branch, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::End);
            Ok(Some(produces))
        }
        // Catch-all fallback to the `ResolvedExpr` emitter. Reaches here:
        // `FnValue` (a fn referenced as a value — wasm-gc has no
        // first-class fn representation).
        _ => Ok(None),
    }
}

/// Emit each MIR `arg` (returning `None` if any falls outside the
/// supported subset, propagated as a whole-fn fallback) then
/// `call $wasm_idx`. Shared by the `Fn` / `Builtin` / `Intrinsic`
/// callee arms; the caller adds the `produces_value` read from the
/// call expr's own type stamp.
pub(crate) fn emit_mir_args_then_call(
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
