//! Shared emit helpers consumed by the MIR body emitter (`from_mir/`):
//! primitive default values, the `__eq_<Type>` helper lookup, the
//! `?!` / `!` independent-product structural-scope markers, the
//! per-fn `caller_fn` idx push, string-literal data-segment bytes, and
//! the self / mutual tail-call (`return_call`) instruction. The
//! `ResolvedExpr`-walking emitters that used to live here were retired
//! with the HIR walker — MIR is the only codegen path.

use wasm_encoder::{Function, Instruction, ValType};

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, aver_to_wasm};
use super::EmitCtx;

/// Emit a "default value" of the given Aver primitive / ref type onto
/// the wasm stack. Used by `Option.None` constructor to satisfy
/// `struct.new`'s requirement that every field has an initial value
/// — the value field of a None-tagged Option is never read by
/// well-typed Aver code (pattern match dispatches on tag first).
pub(super) fn emit_default_value(
    func: &mut Function,
    aver_ty: &str,
    registry: &TypeRegistry,
) -> Result<(), WasmGcError> {
    match aver_ty.trim() {
        "Int" => {
            func.instruction(&Instruction::I64Const(0));
            Ok(())
        }
        "Float" => {
            func.instruction(&Instruction::F64Const(0.0_f64.into()));
            Ok(())
        }
        "Bool" => {
            func.instruction(&Instruction::I32Const(0));
            Ok(())
        }
        "Unit" => {
            // Unit lowers to a placeholder i32 slot when it appears as a
            // payload position inside Result<Unit, E> / Result<T, Unit>
            // (struct shape stays uniform; the slot is never read).
            func.instruction(&Instruction::I32Const(0));
            Ok(())
        }
        other => {
            // Single-field records flattened to a primitive (newtype
            // optimisation) need the matching primitive zero, not a
            // ref.null. Ref types emit `ref.null $T`. The exact wasm
            // shape comes from `aver_to_wasm`.
            let val = aver_to_wasm(other, Some(registry))?;
            match val {
                Some(ValType::Ref(rt)) => {
                    func.instruction(&Instruction::RefNull(rt.heap_type));
                    Ok(())
                }
                Some(ValType::I64) => {
                    func.instruction(&Instruction::I64Const(0));
                    Ok(())
                }
                Some(ValType::F64) => {
                    func.instruction(&Instruction::F64Const(0.0_f64.into()));
                    Ok(())
                }
                Some(ValType::I32) => {
                    func.instruction(&Instruction::I32Const(0));
                    Ok(())
                }
                Some(other_ty) => Err(WasmGcError::Validation(format!(
                    "Option.None default for `{other}` resolved to {other_ty:?} — no default emitter for that wasm type"
                ))),
                None => Err(WasmGcError::Validation(format!(
                    "Option.None over `{other}` has no wasm representation"
                ))),
            }
        }
    }
}

/// Recognise the syntactic form `Tile.Floor` / `EntityKind.WildIfElse`
/// etc. — a bare attribute access on a registered sum-type's name
/// resolving to a nullary variant. Returns the variant's wasm struct
/// type index when the shape matches.
///
/// Also accepts `FnCall(Attr(parent, name), [])` (the `Tile.Floor()`
/// form) since the parser may insert empty arg lists for explicit-paren
/// calls. Matches both `Constructor(name, None)` and `Attr(Ident,
/// field)` shapes.
/// Looks up the per-type `__eq_<TypeName>` helper fn idx for an operand
/// of `BinOp::Eq` / `BinOp::Neq` whose stamped type is a record or sum.
/// Returns `None` for primitive operand types (the caller's default
/// `i64.eq` / `f64.eq` path handles those) or for nominal types whose
/// helper wasn't registered — discovery should've registered every
/// reachable site, so a miss surfaces as `None` and the call falls
/// through to the default arm where wasm validation will catch the
/// type mismatch.
pub(super) fn sum_or_record_eq_fn(ty: &crate::types::Type, ctx: &EmitCtx<'_>) -> Option<u32> {
    match ty {
        // Resolve the registry key through the post-flatten symbol
        // table: non-colliding dep types map to their bare
        // `TypeDef.name`; cross-module same-bare-name collisions map
        // to their renamed canonical `Prefix.Name` (#180 Phase 6 PR 3).
        crate::types::Type::Named { id, name } => {
            let key: String = match id {
                Some(type_id) => ctx.symbol_table.type_entry(*type_id).key.canonical(),
                None => name.clone(),
            };
            // Newtypes already lower to their underlying primitive —
            // no helper needed (the default i64/f64 eq handles them).
            if ctx.registry.newtype_underlying(&key).is_some() {
                return None;
            }
            let is_record = ctx.registry.record_fields.contains_key(&key);
            let is_sum = ctx
                .registry
                .variants
                .values()
                .flat_map(|v| v.iter())
                .any(|v| v.parent == key);
            if !is_record && !is_sum {
                return None;
            }
            ctx.fn_map.eq_helpers.get(&key).copied()
        }
        // Generic carriers — `Option<X>`, `Result<X,Y>`, `Tuple<…>`
        // have per-instantiation `__eq_<canonical>` helpers since
        // 0.16.3. Lookup by the type's display canonical (whitespace-
        // free, matching how the discovery walker registered it).
        crate::types::Type::Option(_)
        | crate::types::Type::Result(_, _)
        | crate::types::Type::Tuple(_) => {
            let canonical: String = ty
                .display()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            ctx.fn_map.eq_helpers.get(&canonical).copied()
        }
        // List / Vector — list_helpers / vfl_helpers slot the per-T
        // eq fn at registration time. Dispatch through there. Both
        // ops carry `eq: Option<u32>` (None when T isn't equality-
        // resolvable; falls through and the surrounding default i64
        // arm fails validation, same as before this PR).
        crate::types::Type::List(_) => {
            let canonical: String = ty
                .display()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            ctx.fn_map.list_ops.get(&canonical).and_then(|ops| ops.eq)
        }
        crate::types::Type::Vector(_) => {
            let canonical: String = ty
                .display()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            ctx.fn_map.vfl_ops.get(&canonical).and_then(|ops| ops.eq)
        }
        // Map<K,V> structural eq — `__eq_Map<K,V>` slot lives in
        // MapHelperRegistry but we mirror the fn idx into
        // `eq_helpers` so the lookup shape stays uniform with
        // record/sum/carrier/list/vec dispatch.
        crate::types::Type::Map(_, _) => {
            let canonical: String = ty
                .display()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            ctx.fn_map.eq_helpers.get(&canonical).copied()
        }
        _ => None,
    }
}

/// Emit `call $aver/<group_op>` if the program registered the
/// structural-scope marker imports (i.e. discovery saw any `?!` /
/// `!`). No-op otherwise — programs that never use independent
/// products don't pay the import slot. Trailing String arg is the
/// `caller_fn` stamp every effect import now carries; the host
/// ignores it for group markers but the wasm signature still has
/// to match.
pub(super) fn emit_group_call(func: &mut Function, ctx: &EmitCtx<'_>, op: &str) {
    if let Some(idx) = ctx.effect_idx_lookup.get(op) {
        if emit_caller_fn_idx(func, ctx).is_err() {
            // Should never trip — every fn def has a global allocated
            // in `TypeRegistry::build`.
            return;
        }
        func.instruction(&Instruction::Call(*idx));
    }
}

/// Emit `i64.const i; <caller_fn>; call $aver/__record_set_branch`
/// if the markers are registered. Used inside `?!` / `!` lowering
/// to switch the recorder's active branch before evaluating each
/// element. Trailing caller_fn ignored host-side but its slot is
/// part of the import signature.
pub(super) fn emit_branch_marker(func: &mut Function, ctx: &EmitCtx<'_>, branch_idx: u32) {
    if let Some(idx) = ctx.effect_idx_lookup.get("__record_set_branch") {
        func.instruction(&Instruction::I64Const(branch_idx as i64));
        if emit_caller_fn_idx(func, ctx).is_err() {
            return;
        }
        func.instruction(&Instruction::Call(*idx));
    }
}

/// Push the caller-fn idx as an `i32` immediate. Lazy-registers the
/// current fn name with the collector so the post-emit phase knows
/// exactly which fn names to put in the exported caller-fn table.
/// Single source of truth: any code path that wants to label its
/// effect call site with the originating fn just calls this — no
/// AST walker, no rozjazdy walker↔codegen. Hot path: 2-3 bytes
/// (`i32.const <idx>`), zero allocation.
pub(super) fn emit_caller_fn_idx(
    func: &mut Function,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let idx = ctx
        .caller_fn_collector
        .borrow_mut()
        .register(ctx.self_fn_name);
    func.instruction(&Instruction::I32Const(idx as i32));
    Ok(())
}

/// Push a `(ref null $string)` onto the wasm stack from a UTF-8 byte
/// slice. Looks up the literal in the registry's passive-segment
/// table; the segment is intern-ed by `collect_string_literals_in_*`
/// during pre-emit discovery.
pub(super) fn emit_string_literal_bytes(
    func: &mut Function,
    bytes: &[u8],
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let segment_idx = ctx
        .registry
        .string_literal_segment(bytes)
        .ok_or(WasmGcError::Validation(format!(
            "String literal `{:?}` was not registered in the data segment table",
            std::str::from_utf8(bytes).unwrap_or("<non-utf8>")
        )))?;
    let s_idx = ctx
        .registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String literal needs string slot allocated".into(),
        ))?;
    func.instruction(&Instruction::I32Const(0)); // offset
    func.instruction(&Instruction::I32Const(bytes.len() as i32));
    func.instruction(&Instruction::ArrayNewData {
        array_type_index: s_idx,
        array_data_index: segment_idx,
    });
    Ok(())
}

/// Emit the tail-call instruction for a call to `target_wasm_idx` from
/// the fn whose own wasm index is `self_wasm_idx`: `return_call`
/// normally, or a plain `call` + fall-through return when
/// `AVER_WASM_GC_NO_TAIL_CALL=1` (used to A/B whether the tail-call
/// proposal is doing meaningful work on a bench — deep recursion will
/// trash the stack with it on, so only flip it for shallow scenarios).
/// Shared by the `ResolvedExpr` tail-call emitter and the MIR
/// `TailCall` walker so both pick the byte-identical instruction.
///
/// The `target == self` branch is the self-tail-call check (a `FnId`
/// lookup already yielded `target_wasm_idx`, so equality with
/// `self_wasm_idx` means `target` names the current fn). It currently
/// resolves to the same index either way, but is kept explicit so the
/// self-recursion case stays a named, greppable site.
pub(super) fn emit_return_call_insn(func: &mut Function, target_wasm_idx: u32, self_wasm_idx: u32) {
    let target_idx = if target_wasm_idx == self_wasm_idx {
        self_wasm_idx
    } else {
        target_wasm_idx
    };
    if std::env::var_os("AVER_WASM_GC_NO_TAIL_CALL").is_some() {
        func.instruction(&Instruction::Call(target_idx));
    } else {
        func.instruction(&Instruction::ReturnCall(target_idx));
    }
}
