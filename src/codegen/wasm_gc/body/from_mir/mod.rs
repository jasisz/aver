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
//! ## Scope (waves 0–12)
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
//!   `Vector` custom-inline family are later builtin sub-waves.
//!
//! Wave 10 (`List` custom-inline builtins):
//! - `Call` with `MirCallee::Builtin` whose dotted name is a `List` op —
//!   `emit_mir_list_builtin`, mirror of the custom-inline `List.*` arms
//!   of `emit_dotted_builtin` plus the `List.prepend` / `List.empty`
//!   intercepts in `emit_expr`'s `Call` arm. `reverse` / `len` /
//!   `length` / `concat` / `take` / `drop` / `contains` dispatch to the
//!   per-`List<T>` `fn_map.list_ops` helper; `zip` to `zip_ops`,
//!   `fromVector` to `vfl_ops`; `prepend` is a `struct.new $list_T`,
//!   `empty` a `ref.null $list_T`. `contains` over a non-eq-able `T`
//!   (no registered helper) falls back. Tried after the native-scalar
//!   path and before the `fn_map.builtins` lookup (none of these are in
//!   that table).
//!
//! Wave 11 (`Vector` custom-inline builtins):
//! - `Call` with `MirCallee::Builtin` whose dotted name is a `Vector`
//!   op — `emit_mir_vector_builtin`, mirror of the custom-inline
//!   `Vector.*` arms: `len` / `new` / `fromList` inline, the boxed
//!   `get` (bounds-checked `Option<T>`) and `set` (`Option<Vector<T>>`,
//!   in-place fast path when `mir_arg_uniquely_owned` — the MIR analogue
//!   of `arg_uniquely_owned`, keyed on `MirLocal.last_use` — else a
//!   clone-on-write through the scratch local). The fused
//!   `Option.withDefault(Vector.get/set, …)` shapes are not reached:
//!   `Option.withDefault` is uncovered, so a fused call falls the whole
//!   fn back before its inner `Vector` op.
//!
//! Wave 12 (`String` binary ops):
//! - `BinOp` with a `String` LHS — `emit_mir_string_binop`, mirror of
//!   the `String` branches of `emit_expr`'s `BinOp` arm: `+` is
//!   `__wasmgc_concat_n` over a 2-element `Vector<String>`, `==` / `!=`
//!   is `__wasmgc_string_eq` (+ `i32.eqz` for `!=`), and `<` / `>` /
//!   `<=` / `>=` is `__wasmgc_string_compare` post-composed with the
//!   matching `i32` comparison against `0`. Checked before the numeric
//!   branch, exactly as the oracle orders it. Compound-type `==` / `!=`
//!   (nullary-variant `ref.test`, sum / record `__eq_*` helpers) still
//!   fall back.
//!
//! Everything else returns `Ok(None)` so the caller
//! ([`super::emit_fn_body_via_mir`]) resets `func` and re-runs the
//! `ResolvedExpr` emitter for the whole fn. That keeps the corpus +
//! game suite green from PR 1 while coverage widens wave by wave.

pub(super) use std::collections::{HashMap, HashSet};

pub(super) use wasm_encoder::{Function, Instruction, ValType};

pub(super) use crate::ast::Spanned;
pub(super) use crate::ast::{BinOp, Literal};
pub(super) use crate::ir::CtorId;
pub(super) use crate::ir::SymbolTable;
pub(super) use crate::ir::hir::{ResolvedFnBody, ResolvedFnDef, ResolvedStmt};
pub(super) use crate::ir::mir::{
    BuiltinCtor, MirCallee, MirCtor, MirExpr, MirFn, MirMatch, MirMatchArm, MirPattern, MirProgram,
    MirRecordField, MirStrPart,
};
pub(super) use crate::types::Type;

pub(super) use super::super::WasmGcError;
pub(super) use super::super::types::{TypeRegistry, VariantInfo, aver_to_wasm, normalize_compound};
pub(super) use super::emit::{
    emit_default_value, emit_return_call_insn, emit_string_literal_bytes,
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
            // Read the operand type from the MIR type stamp — the
            // canary for the whole port. Aver's checker proved both
            // operands share a type, so the LHS suffices. `String`
            // operands take the dedicated concat / eq / compare builtins
            // (wave 12); numeric operands the primitive op set (wave 0);
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
