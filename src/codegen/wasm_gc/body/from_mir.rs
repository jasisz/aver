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
//! ## Scope (waves 0–9)
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
//!   lowering). User-variant / tuple matches are wave 4c–4d.
//!
//! Wave 4b (list match):
//! - `Match` over a `List<T>` subject (`[] -> …; [head, ..tail] -> …`)
//!   — mirror of `emit_list_match`: `ref.is_null` selects the empty
//!   arm, else the cons arm extracts head (struct field 0) + tail
//!   (field 1) into the `Cons` pattern's binding slots before its body.
//!   Checked before Result/Option, matching `emit_match`'s order.
//!
//! Wave 4d (user-variant / sum-type match):
//! - `Match` over a user sum type — mirror of `emit_single_variant_match`
//!   (a single irrefutable `Ctor` arm: newtype-bind / nullary-drop /
//!   inline `ref.cast` + `struct.get`) and `emit_variant_dispatch` +
//!   `emit_arm_body` (multi-arm: a `ref.test` cascade over the variant
//!   struct types, extracting each matched arm's fields from the
//!   scratch-held subject). `MirCtor::User(CtorId)` resolves to its
//!   `VariantInfo` via the symbol table + registry, identical to the
//!   `ctor_dotted_name` lookup. The single-file corpus barely exercises
//!   this; the multi-module games (`Tile`, `EntityKind`, …) do, and the
//!   games byte-differential verifies it. Tuple (wave 4c) and the
//!   multi-arm tuple-of-constructors path still fall back.
//!
//! Wave 5a (constructors):
//! - `Construct` — mirror of `emit_expr`'s `Ctor` arm: user variants via
//!   `emit_constructor_with_args` (newtype emits the payload directly,
//!   else push args + `struct.new`), `Option.Some/None` via
//!   `emit_option_constructor` (tag + payload / `default<T>`), and
//!   `Result.Ok/Err` via `emit_result_constructor` (instantiation
//!   resolved by single-registered / return-type / payload-type match,
//!   then tag + T-slot + E-slot, with a `Unit` position pushing the i32
//!   placeholder).
//!
//! Wave 5b (records):
//! - `RecordCreate` / `RecordUpdate` — mirror of `emit_record_create` /
//!   `emit_record_update`: a newtype emits its single field; otherwise
//!   push every declared field in order (`struct.get` from the base for
//!   un-overridden update fields) + `struct.new`. The `Option.None` /
//!   empty-list field special-cases use the field's *declared* type (the
//!   bare literal's own `.ty()` may be a generic `Var`).
//! - `Project` — mirror of `emit_attr_get`: newtype `.field` is identity,
//!   else `struct.get` the field index; unknown / `Invalid`
//!   (namespace-handle) record types fall back.
//!
//! Wave 6a (tuple / map literals):
//! - `Tuple` — mirror of `emit_tuple_literal`: canonical from the
//!   elements' stamped types, push each + `struct.new`.
//! - `MapLiteral` — mirror of `emit_map_literal`: `Map.empty` then a
//!   `Map.set` per entry (canonical from the first entry's K/V, or the
//!   sole registered `Map<K,V>` when empty).
//!
//! Wave 6b (list literals):
//! - `List` — mirror of `emit_list_literal`: resolve the `List<T>`
//!   instantiation (stamped type / first-element hint / sole registered
//!   / return type), `ref.null` for empty, else push each element +
//!   `ref.null` + N×`call $cons_T`. `Option.None` / empty-list elements
//!   emit against the resolved element type.
//!
//! Wave 7 (`?` propagation):
//! - `Try(inner)` — mirror of `emit_error_prop`: stash the
//!   `Result<T,E>` subject in the reserved scratch, test its tag
//!   (struct field 0). On `Ok` push the payload (field 1; nothing for a
//!   `Result<Unit,E>`), on `Err` rebuild a fresh `Result<EnclosingT,E>`
//!   (tag 0, `default<EnclosingT>`, the subject's err field 2) and
//!   `return` it so the type matches the enclosing fn. `produces` is
//!   `false` for a `Result<Unit,E>?`, else `true`.
//!
//! Wave 8 (string interpolation):
//! - `InterpolatedStr(parts)` — mirror of `emit_interpolated_str`
//!   (builtins.rs): build a `Vector<String>` of the parts and concat it
//!   with `__wasmgc_concat_n`. Each `Literal` part is an `array.new_data`
//!   over its segment; each `Expr` part is emitted then stringified by
//!   the `String.from{Int,Float,Bool}` dispatch (`String` is identity).
//!   An empty interpolation allocates a zero-length array directly. A
//!   compound-type `Expr` part — which the oracle rejects — falls back
//!   so the resolved-HIR path raises the identical error. The wasm-gc
//!   pipeline runs with `run_buffer_build = false`, so interpolation
//!   survives to MIR as `InterpolatedStr` (it is not deforested into
//!   buffer-write intrinsics); this wave covers it on the seam path.
//!
//! Wave 9 (native scalar builtins — first builtin-inline-depth slice):
//! - `Call` with `MirCallee::Builtin` whose dotted name is a native
//!   scalar `Float` / `Int` / `Bool` op — mirror of the inline arms of
//!   `emit_dotted_builtin`: `Float.{fromInt,floor,ceil,round,abs,sqrt,
//!   min,max,pi}`, `Int.{fromFloat,abs,min,max}`, `Bool.{and,or,not}`.
//!   These lower to a fixed `f64` / `i64` / `i32` instruction sequence
//!   (not a registered helper), so the wave-2 `fn_map.builtins` lookup
//!   missed them. The new `emit_mir_native_scalar_builtin` is tried
//!   before that lookup; un-recognized builtins fall through to it
//!   unchanged. `Int.mod` (a `Result` carrier with a fused form) and the
//!   `List` / `Vector` custom-inline families are later builtin
//!   sub-waves.
//!
//! Everything else returns `Ok(None)` so the caller
//! ([`super::emit_fn_body_via_mir`]) resets `func` and re-runs the
//! `ResolvedExpr` emitter for the whole fn. That keeps the corpus +
//! game suite green from PR 1 while coverage widens wave by wave.

use std::collections::{HashMap, HashSet};

use wasm_encoder::{Function, Instruction, ValType};

use crate::ast::Spanned;
use crate::ast::{BinOp, Literal};
use crate::ir::CtorId;
use crate::ir::SymbolTable;
use crate::ir::hir::{ResolvedFnBody, ResolvedFnDef, ResolvedStmt};
use crate::ir::mir::{
    BuiltinCtor, MirCallee, MirCtor, MirExpr, MirFn, MirMatch, MirMatchArm, MirPattern, MirProgram,
    MirRecordField, MirStrPart,
};
use crate::types::Type;

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, VariantInfo, aver_to_wasm};
use super::emit::{emit_default_value, emit_return_call_insn, emit_string_literal_bytes};
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
                    // Native scalar builtins (Float / Int / Bool) lower to
                    // an inline instruction sequence, not a registered
                    // helper, so try them before the `fn_map.builtins`
                    // lookup (which would miss them).
                    match emit_mir_native_scalar_builtin(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
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
    // Tuple arms are wave 4c (single-arm destructure) / the multi-arm
    // tuple-of-constructors path — both still fall back. List (4b),
    // built-in `Result` / `Option` (4a), and user-variant (4d)
    // constructor patterns are handled below.
    if m.arms
        .iter()
        .any(|a| matches!(a.pattern, MirPattern::Tuple(_)))
    {
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

    // List match (`[] -> …; [head, ..tail] -> …`). `emit_match` checks
    // this before Result/Option, so mirror that order.
    if m.arms
        .iter()
        .any(|a| matches!(a.pattern, MirPattern::EmptyList | MirPattern::Cons { .. }))
    {
        return Ok(emit_mir_list_match(func, m, block_ty, slots, ctx)?.map(|()| produces));
    }

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

    // User-variant (sum type) matches. `emit_match` routes a single
    // `Ctor` arm to `emit_single_variant_match` (direct cast, no test)
    // and a multi-arm match to `emit_variant_dispatch` (a `ref.test`
    // cascade) — mirror that split.
    if m.arms.iter().any(|a| {
        matches!(
            a.pattern,
            MirPattern::Ctor {
                ctor: MirCtor::User(_),
                ..
            }
        )
    }) {
        if m.arms.len() == 1 {
            return Ok(
                emit_mir_single_variant_match(func, &m.subject, &m.arms[0], slots, ctx)?
                    .map(|()| produces),
            );
        }
        return Ok(emit_mir_variant_dispatch(func, m, block_ty, slots, ctx)?.map(|()| produces));
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

/// Mirror of `emit_list_match` (emit.rs): a `ref.is_null` tag test —
/// null ⇒ the `[]` arm, else the `[head, ..tail]` arm, which extracts
/// head (struct field 0) and tail (field 1) into the `Cons` pattern's
/// binding slots (each guarded by the `u16::MAX` sentinel) before
/// emitting the body. A wildcard is the empty (then cons) catch-all.
fn emit_mir_list_match(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "List match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(&m.subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let list_idx = ctx
        .registry
        .list_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List match: subject type `{subject_ty}` is not a registered List<T>"
        )))?;

    let mut empty_arm: Option<&MirMatchArm> = None;
    let mut cons_arm: Option<&MirMatchArm> = None;
    for arm in &m.arms {
        match &arm.pattern {
            MirPattern::EmptyList => empty_arm = Some(arm),
            MirPattern::Cons { .. } => cons_arm = Some(arm),
            MirPattern::Wildcard => {
                if empty_arm.is_none() {
                    empty_arm = Some(arm);
                } else if cons_arm.is_none() {
                    cons_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let (Some(empty_arm), Some(cons_arm)) = (empty_arm, cons_arm) else {
        return Ok(None);
    };

    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));

    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefIsNull);
    func.instruction(&Instruction::If(block_ty));
    if emit_mir_expr(func, &empty_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Else);
    if let MirPattern::Cons { head, tail, .. } = &cons_arm.pattern {
        if head.0 != u32::from(u16::MAX) {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(list_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: list_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::LocalSet(head.0));
        }
        if tail.0 != u32::from(u16::MAX) {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(list_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: list_idx,
                field_index: 1,
            });
            func.instruction(&Instruction::LocalSet(tail.0));
        }
    }
    if emit_mir_expr(func, &cons_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// Resolve a `MirCtor::User(CtorId)` to its registry `VariantInfo`,
/// mirroring `emit_match`'s `ctor_dotted_name` + `variant_in` lookup:
/// the parent type name comes from the ctor's owning type's `key.name`,
/// the bare variant name from the ctor entry; the registry is keyed by
/// `(parent, bare)` (with a bare-name fallback for non-colliding types).
fn mir_user_variant_info<'a>(
    ctor_id: CtorId,
    ctx: &'a EmitCtx<'_>,
) -> Result<&'a VariantInfo, WasmGcError> {
    let ctor_entry = ctx.symbol_table.ctor_entry(ctor_id);
    let bare = ctor_entry.name.as_str();
    let parent = ctx
        .symbol_table
        .type_entry(ctor_entry.owning_type)
        .key
        .name
        .clone();
    ctx.registry
        .variant_in(&parent, bare)
        .or_else(|| ctx.registry.variant(bare))
        .ok_or(WasmGcError::Validation(format!(
            "unknown variant `{parent}.{bare}` in match"
        )))
}

/// Emit a covered arm body, returning `None` if the body falls outside
/// the supported subset (propagated as a whole-fn fallback).
fn emit_mir_arm_body_value(
    func: &mut Function,
    body: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    Ok(emit_mir_expr(func, body, slots, ctx)?.map(|_| ()))
}

/// Mirror of `emit_single_variant_match` (emit.rs): an irrefutable
/// single-arm sum-type destructure (the typechecker proved it's the
/// only variant) — newtype shapes bind the subject directly, nullary
/// constructors just drop it, single-binding uses an inline
/// `ref.cast` + `struct.get`, and multi-binding stashes the cast
/// subject in the reserved scratch and extracts each field. The MIR
/// `Ctor` bindings are the resolver's `binding_slots` (`u16::MAX`
/// sentinel for `_`), so every `local.set` matches byte-for-byte.
fn emit_mir_single_variant_match(
    func: &mut Function,
    subject: &Spanned<MirExpr>,
    arm: &MirMatchArm,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let MirPattern::Ctor {
        ctor: MirCtor::User(ctor_id),
        bindings,
        ..
    } = &arm.pattern
    else {
        return Ok(None);
    };
    let info = mir_user_variant_info(*ctor_id, ctx)?;
    const NO_SLOT: u32 = u16::MAX as u32;

    // Newtype: single-variant sum of a single primitive — bind the
    // subject directly, no cast / struct.get.
    if ctx.registry.newtype_underlying(&info.parent).is_some() && bindings.len() == 1 {
        let slot = bindings[0].0;
        if emit_mir_expr(func, subject, slots, ctx)?.is_none() {
            return Ok(None);
        }
        if slot != NO_SLOT {
            func.instruction(&Instruction::LocalSet(slot));
        } else {
            func.instruction(&Instruction::Drop);
        }
        return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
    }

    let variant_idx = info.type_idx;
    let cast_ty = wasm_encoder::HeapType::Concrete(variant_idx);

    if bindings.is_empty() {
        // Nullary — evaluate the subject for effects, drop, emit body.
        if emit_mir_expr(func, subject, slots, ctx)?.is_none() {
            return Ok(None);
        }
        func.instruction(&Instruction::Drop);
        return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
    }

    if emit_mir_expr(func, subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::RefCastNonNull(cast_ty));

    if bindings.len() == 1 {
        // Single binding — the cast ref is on the stack; `struct.get`
        // field 0 and bind (or drop for `_`).
        let slot = bindings[0].0;
        func.instruction(&Instruction::StructGet {
            struct_type_index: variant_idx,
            field_index: 0,
        });
        if slot != NO_SLOT {
            func.instruction(&Instruction::LocalSet(slot));
        } else {
            func.instruction(&Instruction::Drop);
        }
        return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
    }

    // Multi-binding — stash the cast subject, re-read + re-cast per
    // field. The scratch is `(ref null eq)`, so re-cast on each read.
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "multi-binding variant pattern needs subject_scratch but none was reserved".into(),
    ))?;
    func.instruction(&Instruction::LocalSet(scratch));
    for (i, slot) in bindings.iter().enumerate() {
        if slot.0 == NO_SLOT {
            continue;
        }
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(cast_ty));
        func.instruction(&Instruction::StructGet {
            struct_type_index: variant_idx,
            field_index: i as u32,
        });
        func.instruction(&Instruction::LocalSet(slot.0));
    }
    emit_mir_arm_body_value(func, &arm.body, slots, ctx)
}

/// Mirror of `emit_variant_dispatch` (emit.rs): stash the subject in
/// the reserved scratch, then a `ref.test` cascade over the arms.
fn emit_mir_variant_dispatch(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "multi-arm variant match needs a subject scratch slot but none was reserved".into(),
    ))?;
    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));
    emit_mir_variant_arm_cascade(func, &m.arms, block_ty, scratch, slots, ctx)
}

/// Mirror of `emit_variant_arm_cascade` (emit.rs): one arm left → the
/// default (no test); else `ref.test` the first arm's variant, emit its
/// body on match, recurse on the rest in the `else`.
fn emit_mir_variant_arm_cascade(
    func: &mut Function,
    arms: &[MirMatchArm],
    block_ty: wasm_encoder::BlockType,
    subject_scratch: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if arms.is_empty() {
        // Exhaustiveness already proven; reaching here means no arms —
        // emit `unreachable` so the validator treats it as polymorphic.
        func.instruction(&Instruction::Unreachable);
        return Ok(Some(()));
    }
    if arms.len() == 1 {
        return emit_mir_arm_body(func, &arms[0], subject_scratch, slots, ctx);
    }
    let arm = &arms[0];
    match &arm.pattern {
        MirPattern::Ctor {
            ctor: MirCtor::User(ctor_id),
            ..
        } => {
            let info = mir_user_variant_info(*ctor_id, ctx)?;
            func.instruction(&Instruction::LocalGet(subject_scratch));
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(info.type_idx),
            ));
            func.instruction(&Instruction::If(block_ty));
            if emit_mir_arm_body(func, arm, subject_scratch, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::Else);
            if emit_mir_variant_arm_cascade(
                func,
                &arms[1..],
                block_ty,
                subject_scratch,
                slots,
                ctx,
            )?
            .is_none()
            {
                return Ok(None);
            }
            func.instruction(&Instruction::End);
            Ok(Some(()))
        }
        MirPattern::Wildcard => emit_mir_arm_body(func, arm, subject_scratch, slots, ctx),
        // A non-Ctor / non-Wildcard arm here is `emit_match`'s
        // Unimplemented case — fall back.
        _ => Ok(None),
    }
}

/// Mirror of `emit_arm_body` (emit.rs): extract a `Ctor` arm's fields
/// from the scratch-held subject (newtype binds the scratch directly),
/// then emit the body; a wildcard arm just emits its body.
fn emit_mir_arm_body(
    func: &mut Function,
    arm: &MirMatchArm,
    subject_scratch: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if let MirPattern::Ctor {
        ctor: MirCtor::User(ctor_id),
        bindings,
        ..
    } = &arm.pattern
    {
        let info = mir_user_variant_info(*ctor_id, ctx)?;
        const NO_SLOT: u32 = u16::MAX as u32;
        if ctx.registry.newtype_underlying(&info.parent).is_some() && bindings.len() == 1 {
            let slot = bindings[0].0;
            if slot != NO_SLOT {
                func.instruction(&Instruction::LocalGet(subject_scratch));
                func.instruction(&Instruction::LocalSet(slot));
            }
            return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
        }
        for (i, slot) in bindings.iter().enumerate() {
            if slot.0 == NO_SLOT {
                continue;
            }
            func.instruction(&Instruction::LocalGet(subject_scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(info.type_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: info.type_idx,
                field_index: i as u32,
            });
            func.instruction(&Instruction::LocalSet(slot.0));
        }
        return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
    }
    // Wildcard / non-pattern arm — just emit the body.
    emit_mir_arm_body_value(func, &arm.body, slots, ctx)
}

/// Mirror of `emit_constructor_with_args` (emit.rs): a newtype emits
/// its single payload directly (no `struct.new`); otherwise push each
/// arg and `struct.new $variant`.
fn emit_mir_constructor_with_args(
    func: &mut Function,
    info: &VariantInfo,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if args.len() != info.fields.len() {
        return Err(WasmGcError::Validation(format!(
            "variant has {} field(s) but call supplied {}",
            info.fields.len(),
            args.len()
        )));
    }
    if ctx.registry.newtype_underlying(&info.parent).is_some() {
        return Ok(emit_mir_expr(func, &args[0], slots, ctx)?.map(|_| ()));
    }
    for arg in args {
        if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::StructNew(info.type_idx));
    Ok(Some(()))
}

/// Mirror of `emit_option_constructor` (emit.rs): `Some(v)` →
/// `i32.const 1; v; struct.new $option_T`; `None` →
/// `i32.const 0; default<T>; struct.new $option_T`. `T` comes from the
/// payload's stamped type (Some) or the caller's hint (None).
fn emit_mir_option_constructor(
    func: &mut Function,
    payload: Option<&Spanned<MirExpr>>,
    t_aver_hint: Option<&str>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let t_aver: String = match payload {
        Some(p) => aver_type_str_of(p),
        None => t_aver_hint
            .ok_or(WasmGcError::Validation(
                "Option.None without context — cannot infer the T in Option<T>".into(),
            ))?
            .to_string(),
    };
    let canonical = if t_aver.starts_with("Option<") {
        t_aver.clone()
    } else {
        format!("Option<{t_aver}>")
    };
    let opt_idx = ctx
        .registry
        .option_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option constructor: instantiation `{canonical}` was not registered"
        )))?;
    let inner_ty = TypeRegistry::option_element_type(&canonical).ok_or(WasmGcError::Validation(
        format!("Option canonical `{canonical}` has no element type"),
    ))?;
    match payload {
        Some(p) => {
            func.instruction(&Instruction::I32Const(1));
            if emit_mir_expr(func, p, slots, ctx)?.is_none() {
                return Ok(None);
            }
        }
        None => {
            func.instruction(&Instruction::I32Const(0));
            emit_default_value(func, inner_ty, ctx.registry)?;
        }
    }
    func.instruction(&Instruction::StructNew(opt_idx));
    Ok(Some(()))
}

/// Mirror of `emit_result_constructor` (emit.rs): resolve the
/// `Result<T,E>` instantiation (single registered / by return type / by
/// payload-type match), then `i32.const <tag>; <T-slot>; <E-slot>;
/// struct.new $result`. A `Unit` payload position pushes the `i32`
/// placeholder rather than the (no-value) `Unit`.
fn emit_mir_result_constructor(
    func: &mut Function,
    variant: &str,
    payload: Option<&Spanned<MirExpr>>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let payload = payload.ok_or(WasmGcError::Validation(format!(
        "Result.{variant} requires a payload"
    )))?;
    let payload_ty = aver_type_str_of(payload);
    let canonical = if ctx.registry.result_order.len() == 1 {
        ctx.registry.result_order[0].clone()
    } else {
        let return_canonical: String = ctx
            .return_type
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        if ctx.registry.result_type_idx(&return_canonical).is_some() {
            return_canonical
        } else {
            ctx.registry
                .result_order
                .iter()
                .find(|c| {
                    if let Some((t, e)) = TypeRegistry::result_te(c) {
                        let match_pos = if variant == "Ok" { t } else { e };
                        match_pos == payload_ty.trim()
                    } else {
                        false
                    }
                })
                .cloned()
                .ok_or(WasmGcError::Validation(format!(
                    "Result.{variant}({payload_ty}) — no registered Result<T,E> instantiation matches"
                )))?
        }
    };
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .expect("just-resolved canonical");
    let (t_aver, e_aver) = TypeRegistry::result_te(&canonical).ok_or(WasmGcError::Validation(
        format!("Result canonical `{canonical}` malformed"),
    ))?;

    // A `Unit` payload position pushes nothing via `emit_mir_expr`, but
    // the struct slot is i32-sized — push the placeholder ourselves.
    let emit_payload = |func: &mut Function, pos_ty: &str| -> Result<Option<()>, WasmGcError> {
        if pos_ty.trim() == "Unit" {
            func.instruction(&Instruction::I32Const(0));
            Ok(Some(()))
        } else {
            Ok(emit_mir_expr(func, payload, slots, ctx)?.map(|_| ()))
        }
    };
    if variant == "Ok" {
        func.instruction(&Instruction::I32Const(1));
        if emit_payload(func, t_aver)?.is_none() {
            return Ok(None);
        }
        emit_default_value(func, e_aver, ctx.registry)?;
    } else {
        func.instruction(&Instruction::I32Const(0));
        emit_default_value(func, t_aver, ctx.registry)?;
        if emit_payload(func, e_aver)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::StructNew(res_idx));
    Ok(Some(()))
}

/// Emit a record field / update value, mirroring `emit_record_create`'s
/// per-field special-cases: an `Option.None` value emits through the
/// constructor with the field's declared `T` (the bare-literal value's
/// own `.ty()` may be a generic `Var`, so the field declaration is the
/// authoritative shape), and an empty-list value emits `ref.null` of the
/// field's declared `List<T>`. Everything else recurses via
/// `emit_mir_expr`.
fn emit_mir_record_field_value(
    func: &mut Function,
    value: &Spanned<MirExpr>,
    decl_ty: &str,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if let MirExpr::Construct(c) = &value.node
        && matches!(c.node.ctor, MirCtor::Builtin(BuiltinCtor::OptionNone))
        && let Some(inner) = decl_ty
            .trim()
            .strip_prefix("Option<")
            .and_then(|s| s.strip_suffix('>'))
    {
        return emit_mir_option_constructor(func, None, Some(inner.trim()), slots, ctx);
    }
    if let MirExpr::List(items) = &value.node
        && items.is_empty()
    {
        let canonical: String = decl_ty.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(list_idx) = ctx.registry.list_type_idx(&canonical) {
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                list_idx,
            )));
            return Ok(Some(()));
        }
    }
    Ok(emit_mir_expr(func, value, slots, ctx)?.map(|_| ()))
}

/// Mirror of `emit_record_create` (emit.rs): a newtype record emits its
/// single field's value directly; otherwise push every declared field
/// (in declaration order) and `struct.new $type_idx`.
fn emit_mir_record_create(
    func: &mut Function,
    type_name: &str,
    fields: &[MirRecordField],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if ctx.registry.newtype_underlying(type_name).is_some() {
        let field = fields.first().ok_or(WasmGcError::Validation(format!(
            "newtype record `{type_name}` requires one field"
        )))?;
        return Ok(emit_mir_expr(func, &field.value, slots, ctx)?.map(|_| ()));
    }
    let type_idx = ctx
        .registry
        .record_type_idx(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "unknown record type `{type_name}`"
        )))?;
    let decl_fields = ctx
        .registry
        .record_fields
        .get(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "record `{type_name}` missing field list"
        )))?
        .clone();
    for (decl_name, decl_ty) in &decl_fields {
        let provided =
            fields
                .iter()
                .find(|f| &f.name == decl_name)
                .ok_or(WasmGcError::Validation(format!(
                    "record `{type_name}` missing field `{decl_name}`"
                )))?;
        if emit_mir_record_field_value(func, &provided.value, decl_ty, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::StructNew(type_idx));
    Ok(Some(()))
}

/// Mirror of `emit_record_update` (emit.rs): push each declared field in
/// order — the override value when present, else `struct.get` it from
/// the base — then `struct.new $type_idx`.
fn emit_mir_record_update(
    func: &mut Function,
    type_name: &str,
    base: &Spanned<MirExpr>,
    updates: &[MirRecordField],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let type_idx = ctx
        .registry
        .record_type_idx(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "unknown record type `{type_name}`"
        )))?;
    let decl_fields = ctx
        .registry
        .record_fields
        .get(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "record `{type_name}` missing field list"
        )))?
        .clone();
    for (decl_name, decl_ty) in &decl_fields {
        if let Some(override_field) = updates.iter().find(|f| &f.name == decl_name) {
            if emit_mir_record_field_value(func, &override_field.value, decl_ty, slots, ctx)?
                .is_none()
            {
                return Ok(None);
            }
        } else {
            let field_idx = ctx
                .registry
                .record_field_index(type_name, decl_name)
                .ok_or(WasmGcError::Validation(format!(
                    "record `{type_name}` has no field `{decl_name}` to copy from base"
                )))?;
            if emit_mir_expr(func, base, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::StructGet {
                struct_type_index: type_idx,
                field_index: field_idx,
            });
        }
    }
    func.instruction(&Instruction::StructNew(type_idx));
    Ok(Some(()))
}

/// `true` for a MIR `Option.None` constructor (mirror of
/// `is_option_none_expr`) — used to give it the declared element-type
/// hint inside list literals (its own `.ty()` may be a generic `Var`).
fn is_mir_option_none(expr: &Spanned<MirExpr>) -> bool {
    matches!(&expr.node,
        MirExpr::Construct(c)
            if matches!(c.node.ctor, MirCtor::Builtin(BuiltinCtor::OptionNone))
                && c.node.args.is_empty())
}

/// Mirror of `emit_list_literal` (emit.rs): resolve the `List<T>`
/// instantiation (stamped type, else first-element hint, else sole
/// registered, else return type), then for a non-empty literal push
/// each element left-to-right, push `null`, and `call $cons_T` N times
/// (the cons fold needs no scratch — vital for nested literals).
/// `Option.None` / empty-list elements emit against the resolved
/// element type, since their own stamp can be a generic `Var`.
fn emit_mir_list_literal(
    func: &mut Function,
    outer: &Spanned<MirExpr>,
    items: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let stamped = aver_type_canonical(outer, ctx.return_type, ctx.registry);
    let canonical =
        if stamped.starts_with("List<") && ctx.registry.list_type_idx(&stamped).is_some() {
            stamped
        } else if let Some(first) = items.first() {
            let needs_hint = matches!(&first.node, MirExpr::List(xs) if xs.is_empty())
                || is_mir_option_none(first);
            let elem_ty = if needs_hint {
                let ret: String = ctx
                    .return_type
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if let Some(inner) = ret.strip_prefix("List<").and_then(|s| s.strip_suffix('>')) {
                    inner.to_string()
                } else {
                    aver_type_str_of(first)
                }
            } else {
                aver_type_str_of(first)
            };
            format!("List<{elem_ty}>")
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect::<String>()
        } else if ctx.registry.list_order.len() == 1 {
            ctx.registry.list_order[0].clone()
        } else {
            let ret: String = ctx
                .return_type
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            if ret.starts_with("List<") {
                ret
            } else if let Some(first) = ctx.registry.list_order.first() {
                first.clone()
            } else {
                ret
            }
        };
    let list_idx = ctx
        .registry
        .list_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List literal: cannot resolve list instantiation (got `{canonical}`)"
        )))?;
    if items.is_empty() {
        func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
            list_idx,
        )));
        return Ok(Some(()));
    }
    let cons_fn = ctx
        .fn_map
        .list_ops_lookup(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List literal: cons helper for `{canonical}` not registered"
        )))?
        .cons;
    let elem_ty = TypeRegistry::list_element_type(&canonical).map(|s| s.to_string());
    for item in items {
        if let Some(elem) = elem_ty.as_deref()
            && is_mir_option_none(item)
            && let Some(inner) = elem
                .strip_prefix("Option<")
                .and_then(|s| s.strip_suffix('>'))
        {
            if emit_mir_option_constructor(func, None, Some(inner.trim()), slots, ctx)?.is_none() {
                return Ok(None);
            }
            continue;
        }
        if let Some(elem) = elem_ty.as_deref()
            && let MirExpr::List(xs) = &item.node
            && xs.is_empty()
            && let Some(inner_idx) = ctx.registry.list_type_idx(elem)
        {
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                inner_idx,
            )));
            continue;
        }
        if emit_mir_expr(func, item, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        list_idx,
    )));
    for _ in 0..items.len() {
        func.instruction(&Instruction::Call(cons_fn));
    }
    Ok(Some(()))
}

/// Outcome of trying to emit a builtin call through a specialized MIR
/// path, so the `Call(Builtin)` arm can try paths in order without
/// conflating "not this path's builtin" with "fall the whole fn back".
enum MirBuiltinEmit {
    /// Not a builtin this path recognizes — try the next path.
    NotHandled,
    /// Recognized, but an argument couldn't be emitted from MIR — the
    /// whole fn falls back to the resolved-HIR emitter.
    Fallback,
    /// Emitted; `produces` is whether a value is left on the stack.
    Produced(bool),
}

/// Mirror of the native scalar (`Float` / `Int` / `Bool`) arms of
/// `emit_dotted_builtin` (builtins.rs): builtins that lower to a fixed
/// inline wasm instruction sequence over `f64` / `i64` / `i32` values
/// rather than a registered helper call (so the wave-2 `fn_map.builtins`
/// lookup misses them and they fell back until now). Each recurses
/// `emit_mir_expr` on its args — the byte-identical analogue of the
/// oracle's `emit_expr`. `Int.abs` / `Int.min` / `Int.max` re-emit an
/// arg more than once (an `if`/`else` select), exactly as the oracle
/// does; a `None` from any re-emission is a clean whole-fn fallback
/// (`func` is reset by the caller). `Int.mod` is deliberately absent: it
/// builds a `Result<Int,String>` carrier and has a fused form, so it
/// stays on the HIR path for a later sub-wave.
fn emit_mir_native_scalar_builtin(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    // Emit one arg, mapping a `None` (uncovered sub-expr) to `Fallback`.
    macro_rules! arg {
        ($i:expr) => {
            if emit_mir_expr(func, &args[$i], slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    let i64_block = wasm_encoder::BlockType::Result(ValType::I64);
    match dotted {
        "Float.fromInt" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64ConvertI64S);
        }
        "Int.fromFloat" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::I64TruncF64S);
        }
        "Float.floor" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64Floor);
            func.instruction(&Instruction::I64TruncF64S);
        }
        "Float.ceil" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64Ceil);
            func.instruction(&Instruction::I64TruncF64S);
        }
        "Float.round" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64Nearest);
            func.instruction(&Instruction::I64TruncF64S);
        }
        "Float.abs" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64Abs);
        }
        "Float.sqrt" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64Sqrt);
        }
        "Float.min" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::F64Min);
        }
        "Float.max" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::F64Max);
        }
        "Float.pi" if args.is_empty() => {
            func.instruction(&Instruction::F64Const(std::f64::consts::PI.into()));
        }
        "Int.abs" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::I64Const(0));
            func.instruction(&Instruction::I64LtS);
            func.instruction(&Instruction::If(i64_block));
            func.instruction(&Instruction::I64Const(0));
            arg!(0);
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::Else);
            arg!(0);
            func.instruction(&Instruction::End);
        }
        "Int.min" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::I64LtS);
            func.instruction(&Instruction::If(i64_block));
            arg!(0);
            func.instruction(&Instruction::Else);
            arg!(1);
            func.instruction(&Instruction::End);
        }
        "Int.max" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::I64GtS);
            func.instruction(&Instruction::If(i64_block));
            arg!(0);
            func.instruction(&Instruction::Else);
            arg!(1);
            func.instruction(&Instruction::End);
        }
        "Bool.and" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::I32And);
        }
        "Bool.or" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::I32Or);
        }
        "Bool.not" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::I32Eqz);
        }
        _ => return Ok(MirBuiltinEmit::NotHandled),
    }
    Ok(MirBuiltinEmit::Produced(true))
}

/// Mirror of `emit_error_prop` (emit.rs): `value?` over a `Result<T,E>`.
/// Stash the subject, test the tag — on `Ok` push the payload (field 1;
/// nothing for `Result<Unit,E>`), on `Err` rebuild a fresh
/// `Result<EnclosingT, E>::Err` (tag 0, `default<EnclosingT>`, the
/// subject's err field) and `return` it so the type lines up with the
/// enclosing fn. Returns `Some(produces)` where `produces` is `false`
/// for a `Result<Unit,E>?` (no observable Ok value), else `true`.
fn emit_mir_try(
    func: &mut Function,
    inner: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "ErrorProp (`?`) requires a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(inner);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "ErrorProp: subject type `{subject_ty}` is not a registered Result<T,E>"
        )))?;
    let (t_aver, _e_aver) = TypeRegistry::result_te(&canonical).ok_or(WasmGcError::Validation(
        format!("ErrorProp: Result canonical `{canonical}` malformed"),
    ))?;
    let unit_ok = t_aver.trim() == "Unit";
    let block_ty = if unit_ok {
        wasm_encoder::BlockType::Empty
    } else {
        let ok_wasm = aver_to_wasm(t_aver, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
            format!("ErrorProp: Ok type `{t_aver}` has no wasm representation"),
        ))?;
        wasm_encoder::BlockType::Result(ok_wasm)
    };
    let enclosing_canonical: String = ctx
        .return_type
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let enclosing_idx =
        ctx.registry
            .result_type_idx(&enclosing_canonical)
            .ok_or(WasmGcError::Validation(format!(
                "ErrorProp: enclosing fn return `{}` is not a registered Result<T,E>",
                ctx.return_type
            )))?;
    let (enclosing_t_aver, _) =
        TypeRegistry::result_te(&enclosing_canonical).ok_or(WasmGcError::Validation(format!(
            "ErrorProp: enclosing Result canonical `{enclosing_canonical}` malformed"
        )))?;

    if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
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
    if !unit_ok {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: res_idx,
            field_index: 1,
        });
    }
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::I32Const(0));
    emit_default_value(func, enclosing_t_aver, ctx.registry)?;
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(res_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 2,
    });
    func.instruction(&Instruction::StructNew(enclosing_idx));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);
    Ok(Some(!unit_ok))
}

/// Mirror of `emit_interpolated_str` (builtins.rs): build a
/// `Vector<String>` of the parts and concat it with `__wasmgc_concat_n`.
/// Each `Literal` part becomes an `array.new_data` over its segment;
/// each `Expr` part is emitted then stringified by the same
/// `String.from{Int,Float,Bool}` dispatch (a `String` is identity).
/// An interpolation of a compound type — which `emit_interpolated_str`
/// rejects outright — returns `None` so the whole fn falls back to the
/// resolved-HIR emitter, which raises the identical error. The result
/// is always a `String`, so `produces` is `true` (empty interpolation
/// allocates a zero-length array directly, same as the oracle).
fn emit_mir_interpolated_str(
    func: &mut Function,
    parts: &[MirStrPart],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let string_type_idx = ctx
        .registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "InterpolatedStr reachable but no String type slot allocated".into(),
        ))?;
    if parts.is_empty() {
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        return Ok(Some(true));
    }
    let vec_idx = ctx
        .registry
        .vector_type_idx("Vector<String>")
        .ok_or(WasmGcError::Validation(
            "InterpolatedStr requires Vector<String> slot but it wasn't registered".into(),
        ))?;
    let concat_idx = ctx
        .fn_map
        .builtins
        .get("__wasmgc_concat_n")
        .copied()
        .ok_or(WasmGcError::Validation(
            "InterpolatedStr requires __wasmgc_concat_n builtin but it wasn't registered".into(),
        ))?;
    for part in parts {
        match part {
            MirStrPart::Literal(s) => {
                let bytes = s.as_bytes();
                let seg_idx =
                    ctx.registry
                        .string_literal_segment(bytes)
                        .ok_or(WasmGcError::Validation(format!(
                            "Interpolation literal `{s:?}` not in segment table"
                        )))?;
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32Const(bytes.len() as i32));
                func.instruction(&Instruction::ArrayNewData {
                    array_type_index: string_type_idx,
                    array_data_index: seg_idx,
                });
            }
            MirStrPart::Expr(inner) => {
                let aver_ty = aver_type_str_of(inner);
                if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                match aver_ty.trim() {
                    "String" => { /* identity */ }
                    "Int" => {
                        let to_string_idx =
                            ctx.fn_map.builtins.get("String.fromInt").copied().ok_or(
                                WasmGcError::Validation(
                                    "interpolation of Int requires String.fromInt builtin".into(),
                                ),
                            )?;
                        func.instruction(&Instruction::Call(to_string_idx));
                    }
                    "Float" => {
                        let to_string_idx =
                            ctx.fn_map.builtins.get("String.fromFloat").copied().ok_or(
                                WasmGcError::Validation(
                                    "interpolation of Float requires String.fromFloat builtin"
                                        .into(),
                                ),
                            )?;
                        func.instruction(&Instruction::Call(to_string_idx));
                    }
                    "Bool" => {
                        let to_string_idx =
                            ctx.fn_map.builtins.get("String.fromBool").copied().ok_or(
                                WasmGcError::Validation(
                                    "interpolation of Bool requires String.fromBool builtin".into(),
                                ),
                            )?;
                        func.instruction(&Instruction::Call(to_string_idx));
                    }
                    // Compound type: `emit_interpolated_str` errors here.
                    // Fall back so the resolved-HIR path raises it instead.
                    _ => return Ok(None),
                }
            }
        }
    }
    func.instruction(&Instruction::ArrayNewFixed {
        array_type_index: vec_idx,
        array_size: parts.len() as u32,
    });
    func.instruction(&Instruction::Call(concat_idx));
    Ok(Some(true))
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
        MirExpr::Construct(spanned_ctor) => spanned_ctor.node.args.iter().all(mir_expr_coverable),
        MirExpr::RecordCreate(spanned_rec) => spanned_rec
            .node
            .fields
            .iter()
            .all(|f| mir_expr_coverable(&f.value)),
        MirExpr::RecordUpdate(spanned_upd) => {
            mir_expr_coverable(&spanned_upd.node.base)
                && spanned_upd
                    .node
                    .updates
                    .iter()
                    .all(|f| mir_expr_coverable(&f.value))
        }
        MirExpr::Project(spanned_proj) => mir_expr_coverable(&spanned_proj.node.base),
        MirExpr::Tuple(items) => items.iter().all(mir_expr_coverable),
        MirExpr::MapLiteral(entries) => entries
            .iter()
            .all(|(k, v)| mir_expr_coverable(k) && mir_expr_coverable(v)),
        MirExpr::List(items) => items.iter().all(mir_expr_coverable),
        MirExpr::Try(inner) => mir_expr_coverable(inner),
        MirExpr::InterpolatedStr(parts) => parts.iter().all(|p| match p {
            // Coarse: a compound-type `Expr` part falls back at emit
            // time (the registry-free predicate can't see the type), a
            // tolerable over-count for `--explain-mir-coverage`.
            MirStrPart::Literal(_) => true,
            MirStrPart::Expr(e) => mir_expr_coverable(e),
        }),
        MirExpr::Match(spanned_match) => {
            // Coarse, ctx-free mirror of `emit_mir_match`'s dispatch (the
            // predicate has no registry, so it can't model the Map.get
            // fused-Option fallback — a tolerable over-count, since this
            // only feeds `--explain-mir-coverage`; the real per-fn
            // dispatch is what the wire-up + differential test use).
            // Tuple arms are wave 4c — not yet covered.
            let m = &spanned_match.node;
            let unsupported_pat = m
                .arms
                .iter()
                .any(|a| matches!(a.pattern, MirPattern::Tuple(_)));
            // A primitive-subject match takes the Bool/Int/String branches
            // (literal / wildcard arms only; `Bind` falls back); a
            // Result/Option match carries built-in constructor arms; a
            // list match carries `[]` / `[head, ..tail]` arms.
            let is_primitive = matches!(m.subject.ty(), Some(Type::Bool | Type::Int | Type::Str))
                && !m.arms.iter().any(|a| {
                    matches!(
                        a.pattern,
                        MirPattern::Bind(..)
                            | MirPattern::Ctor { .. }
                            | MirPattern::EmptyList
                            | MirPattern::Cons { .. }
                    )
                });
            let is_result_or_option = m.arms.iter().any(arm_is_mir_result_ctor)
                || m.arms.iter().any(arm_is_mir_option_ctor);
            let is_list = m
                .arms
                .iter()
                .any(|a| matches!(a.pattern, MirPattern::EmptyList | MirPattern::Cons { .. }));
            // A user-variant (sum type) match carries `MirCtor::User`
            // constructor arms (single-arm destructure or multi-arm
            // `ref.test` cascade).
            let is_variant = m.arms.iter().any(|a| {
                matches!(
                    a.pattern,
                    MirPattern::Ctor {
                        ctor: MirCtor::User(_),
                        ..
                    }
                )
            });
            !m.arms.is_empty()
                && !unsupported_pat
                && (is_primitive || is_result_or_option || is_list || is_variant)
                && mir_expr_coverable(&m.subject)
                && m.arms.iter().all(|a| mir_expr_coverable(&a.body))
        }
        _ => false,
    }
}
