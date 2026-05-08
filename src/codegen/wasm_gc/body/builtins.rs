//! Dotted-builtin call lowering — `Type.method(args)` shapes that the
//! wasm-gc backend lowers either to inline instruction sequences or to
//! per-instantiation helper calls. Split out of `body.rs` — pure code
//! movement, no logic changes.

use wasm_encoder::{Function, Instruction, ValType};

use crate::ast::{Expr, Spanned};
use crate::ir::{LeafOp, classify_leaf_op};

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, aver_to_wasm};
use super::builtins_wasip2::{
    emit_args_get_wasip2, emit_console_print_wasip2, emit_console_read_line_wasip2,
    emit_disk_append_text_wasip2, emit_disk_delete_dir_wasip2, emit_disk_delete_wasip2,
    emit_disk_exists_wasip2, emit_disk_make_dir_wasip2, emit_disk_read_text_wasip2,
    emit_disk_write_text_wasip2, emit_env_get_wasip2, emit_random_float_wasip2,
    emit_random_int_wasip2, emit_time_now_wasip2, emit_time_sleep_wasip2,
    emit_time_unix_ms_wasip2,
};
use super::emit::{emit_default_value, emit_expr};
use super::infer::aver_type_str_of;
use super::{EmitCtx, SlotTable};

/// Lower a `Type.Variant(args...)` call (parsed as `FnCall(Attr, args)`)
/// to `struct.new $variant_type_idx`. Used by both the Constructor expr
/// path and the disguised-FnCall path.
/// Lower a dotted builtin call like `Float.fromInt(n)` or
/// `String.fromInt(n)`. The set is curated — phase 3b ships the
/// minimum the bench scenarios need; anything else surfaces an
/// "Unimplemented — phase 3c builtin" error so the missing one is
/// visible.
pub(super) fn emit_dotted_builtin(
    func: &mut Function,
    parent: &str,
    method: &str,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let dotted = format!("{parent}.{method}");

    // Registered helper builtin? Push args, emit `call $idx`.
    if let Some(&wasm_idx) = ctx.fn_map.builtins.get(&dotted) {
        for arg in args {
            emit_expr(func, arg, slots, ctx)?;
        }
        func.instruction(&Instruction::Call(wasm_idx));
        return Ok(());
    }

    // `Args.get()` (no args, returns List<String>) — short-circuit
    // before the effect dispatch. AverBridge inlines a `len` +
    // reverse-loop over `args_get(i)` cons-building. Wasip2 calls
    // `wasi:cli/environment.get-arguments` once and dispatches the
    // canonical-ABI list<string> retptr through the shared
    // `__rt_canonical_decode_list_string` helper.
    if dotted == "Args.get" && args.is_empty() {
        if ctx.wasip2_lowering.is_some() {
            return emit_args_get_wasip2(func, slots, ctx);
        }
        emit_args_get_inline(func, slots, ctx)?;
        return Ok(());
    }

    // Phase 1.2b1.5 — `--target wasip2` lowering for the
    // `Console.{print, error, warn}` trio. Bypasses the AverBridge
    // `aver/console_print` import path and emits canonical-ABI
    // calls directly: lazy-init the cached `output-stream` handle
    // (one i32 global, sentinel `-1`), marshal the Aver String into
    // LM[0..len] via `__rt_string_to_lm`, then call
    // `wasi:io/streams.[method]output-stream.blocking-write-and-flush`
    // with `(handle, ptr=0, len, retptr=(len+15)&-16)`. The retptr
    // area receives the host-written `result<_, stream-error>`
    // tag; we ignore it (Aver `Console.print` is `Unit`, matches
    // the wasm-gc target's fire-and-forget shape). Defensive
    // `memory.grow(1)` after the marshal ensures retptr+12 fits
    // even when `len` lands exactly on a page boundary.
    if ctx.wasip2_lowering.is_some() {
        if parent == "Console" && matches!(method, "print" | "error" | "warn") {
            return emit_console_print_wasip2(func, method, args, slots, ctx);
        }
        if parent == "Time" && method == "unixMs" {
            return emit_time_unix_ms_wasip2(func, args, ctx);
        }
        if parent == "Random" && method == "int" {
            return emit_random_int_wasip2(func, args, slots, ctx);
        }
        if parent == "Random" && method == "float" {
            return emit_random_float_wasip2(func, args, ctx);
        }
        if parent == "Env" && method == "get" {
            return emit_env_get_wasip2(func, args, slots, ctx);
        }
        if parent == "Time" && method == "now" {
            return emit_time_now_wasip2(func, args, ctx);
        }
        if parent == "Console" && method == "readLine" {
            return emit_console_read_line_wasip2(func, args, ctx);
        }
        if parent == "Time" && method == "sleep" {
            return emit_time_sleep_wasip2(func, args, slots, ctx);
        }
        if parent == "Disk" && method == "exists" {
            return emit_disk_exists_wasip2(func, args, slots, ctx);
        }
        if parent == "Disk" && method == "readText" {
            return emit_disk_read_text_wasip2(func, args, slots, ctx);
        }
        if parent == "Disk" && method == "writeText" {
            return emit_disk_write_text_wasip2(func, args, slots, ctx);
        }
        if parent == "Disk" && method == "appendText" {
            return emit_disk_append_text_wasip2(func, args, slots, ctx);
        }
        if parent == "Disk" && method == "delete" {
            return emit_disk_delete_wasip2(func, args, slots, ctx);
        }
        if parent == "Disk" && method == "deleteDir" {
            return emit_disk_delete_dir_wasip2(func, args, slots, ctx);
        }
        if parent == "Disk" && method == "makeDir" {
            return emit_disk_make_dir_wasip2(func, args, slots, ctx);
        }
    }

    // Registered effect import? Same shape — push args, push the
    // current fn name via `global.get` (one immutable global per fn
    // name, init by `array.new_data` at instantiation) so the host
    // can stamp `caller_fn` on the recorded effect, then call by idx.
    if let Some(&wasm_idx) = ctx.fn_map.effects.get(&dotted) {
        for arg in args {
            emit_expr(func, arg, slots, ctx)?;
        }
        super::emit::emit_caller_fn_idx(func, ctx)?;
        func.instruction(&Instruction::Call(wasm_idx));
        return Ok(());
    }

    // `BranchPath.*` and `Trace.*` reach the wasm-gc backend only
    // through verify / oracle givens; their bodies are dead from a
    // `_start` perspective. Emit a no-op that returns the universal
    // eqref carrier so the fn signature stays representable. The
    // verify executor (when it runs on wasm-gc — followup) will dispatch
    // these through real impls; bare `aver run --wasm-gc` on a
    // verify-only file can still build and instantiate without a
    // runtime call ever reaching here.
    if parent == "BranchPath" || parent == "Trace" {
        // Verify-only oracle / tracing methods. Their bodies are dead
        // from `_start`; emit `unreachable` and let wasm validation
        // treat the rest as polymorphic.
        func.instruction(&Instruction::Unreachable);
        return Ok(());
    }

    match dotted.as_str() {
        // Float.fromInt(Int) -> Float
        "Float.fromInt" => {
            if args.len() != 1 {
                return Err(WasmGcError::Validation(format!(
                    "Float.fromInt expects 1 arg, got {}",
                    args.len()
                )));
            }
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::F64ConvertI64S);
            Ok(())
        }
        // Int.fromFloat(Float) -> Int
        "Int.fromFloat" => {
            if args.len() != 1 {
                return Err(WasmGcError::Validation(format!(
                    "Int.fromFloat expects 1 arg, got {}",
                    args.len()
                )));
            }
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::I64TruncF64S);
            Ok(())
        }
        // Native single-instruction builtins (Float).
        // Aver `Float.floor / ceil / round` → Int (matches the legacy
        // semantics — the integer-valued result feeds straight into
        // arithmetic, not back through Float ops). Lower as f64 op +
        // truncate to i64.
        "Float.floor" if args.len() == 1 => {
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::F64Floor);
            func.instruction(&Instruction::I64TruncF64S);
            Ok(())
        }
        "Float.ceil" if args.len() == 1 => {
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::F64Ceil);
            func.instruction(&Instruction::I64TruncF64S);
            Ok(())
        }
        "Float.round" if args.len() == 1 => {
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::F64Nearest);
            func.instruction(&Instruction::I64TruncF64S);
            Ok(())
        }
        "Float.abs" if args.len() == 1 => {
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::F64Abs);
            Ok(())
        }
        "Float.sqrt" if args.len() == 1 => {
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::F64Sqrt);
            Ok(())
        }
        "Float.min" if args.len() == 2 => {
            emit_expr(func, &args[0], slots, ctx)?;
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::F64Min);
            Ok(())
        }
        "Float.max" if args.len() == 2 => {
            emit_expr(func, &args[0], slots, ctx)?;
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::F64Max);
            Ok(())
        }
        "Float.pi" if args.is_empty() => {
            func.instruction(&Instruction::F64Const(std::f64::consts::PI.into()));
            Ok(())
        }
        "Int.abs" if args.len() == 1 => {
            // Branched: if (x < 0) 0 - x else x. Two evaluations of x;
            // cheap when x is a Resolved local.
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::I64Const(0));
            func.instruction(&Instruction::I64LtS);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                ValType::I64,
            )));
            func.instruction(&Instruction::I64Const(0));
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::Else);
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::End);
            Ok(())
        }
        "Int.min" if args.len() == 2 => {
            // Branched: if (a < b) a else b. Two evaluations of each.
            emit_expr(func, &args[0], slots, ctx)?;
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::I64LtS);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                ValType::I64,
            )));
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::Else);
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::End);
            Ok(())
        }
        "Int.max" if args.len() == 2 => {
            emit_expr(func, &args[0], slots, ctx)?;
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::I64GtS);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                ValType::I64,
            )));
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::Else);
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::End);
            Ok(())
        }
        "Int.mod" if args.len() == 2 => {
            // Aver `Int.mod` returns `Result<Int, Error>` — Aver
            // surface code can match on `Result.Ok(n)` / `Result.Err(_)`
            // or feed the call into `Result.withDefault`. The fused
            // `Result.withDefault(Int.mod(a, b), default)` form collapses
            // to a bare `i64.rem_s` (or the default on b==0) inside
            // `emit_result_with_default`; reaching here means the
            // unfused shape — emit a real `Result<Int, String>`
            // struct so downstream pattern matching sees the canonical
            // tagged carrier instead of a raw i64.
            let res_idx = ctx.registry.result_type_idx("Result<Int,String>").ok_or(
                WasmGcError::Validation(
                    "Int.mod requires Result<Int,String> slot to be registered".into(),
                ),
            )?;
            let s_idx = ctx
                .registry
                .string_array_type_idx
                .ok_or(WasmGcError::Validation(
                    "Int.mod requires String slot to be registered".into(),
                ))?;
            // tag = 0 if b == 0 else 1
            // ok = (a rem b) on success, 0 placeholder on error
            // err = "Division by zero" String on error, null placeholder on success
            let block_ty_struct = wasm_encoder::BlockType::Result(wasm_encoder::ValType::Ref(
                wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(res_idx),
                },
            ));
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::I64Const(0));
            func.instruction(&Instruction::I64Eq);
            func.instruction(&Instruction::If(block_ty_struct));
            // Err: tag=0, T=0 (Int placeholder), E="Division by zero"
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I64Const(0));
            // Build the error string literal via the runtime LM bridge.
            let err_literal = "Division by zero";
            let bytes = err_literal.as_bytes();
            let seg_idx =
                ctx.registry
                    .string_literal_segment(bytes)
                    .ok_or(WasmGcError::Validation(format!(
                        "Int.mod error literal `{err_literal}` not in segment table"
                    )))?;
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32Const(bytes.len() as i32));
            func.instruction(&Instruction::ArrayNewData {
                array_type_index: s_idx,
                array_data_index: seg_idx,
            });
            func.instruction(&Instruction::StructNew(res_idx));
            func.instruction(&Instruction::Else);
            // Ok: tag=1, T=Int.mod(a, b) via Euclidean helper, E=null
            func.instruction(&Instruction::I32Const(1));
            emit_expr(func, &args[0], slots, ctx)?;
            emit_expr(func, &args[1], slots, ctx)?;
            let mod_idx = ctx.fn_map.builtins.get("__int_mod_euclid").copied().ok_or(
                WasmGcError::Validation(
                    "Int.mod requires __int_mod_euclid helper to be registered".into(),
                ),
            )?;
            func.instruction(&Instruction::Call(mod_idx));
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                s_idx,
            )));
            func.instruction(&Instruction::StructNew(res_idx));
            func.instruction(&Instruction::End);
            Ok(())
        }
        // Bool ops: Aver Bool == wasm i32. and/or/not are bitwise
        // single-instructions on i32.
        "Bool.and" if args.len() == 2 => {
            emit_expr(func, &args[0], slots, ctx)?;
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::I32And);
            Ok(())
        }
        "Bool.or" if args.len() == 2 => {
            emit_expr(func, &args[0], slots, ctx)?;
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::I32Or);
            Ok(())
        }
        "Bool.not" if args.len() == 1 => {
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::I32Eqz);
            Ok(())
        }
        // String.fromInt / String.fromFloat — surface names since 0.17.
        // Backend dispatches to the per-type digit-conversion helper
        // registered under the same canonical name.
        "String.fromInt" if args.len() == 1 => {
            let to_string_idx = ctx.fn_map.builtins.get("String.fromInt").copied().ok_or(
                WasmGcError::Validation("String.fromInt builtin helper not registered".into()),
            )?;
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::Call(to_string_idx));
            Ok(())
        }
        "String.fromFloat" if args.len() == 1 => {
            let to_string_idx = ctx.fn_map.builtins.get("String.fromFloat").copied().ok_or(
                WasmGcError::Validation("String.fromFloat builtin helper not registered".into()),
            )?;
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::Call(to_string_idx));
            Ok(())
        }
        // Vector.len is a single wasm instruction over our concrete
        // `(array T)` representation, plus widening to Aver's i64.
        "Vector.len" if args.len() == 1 => {
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::ArrayLen);
            func.instruction(&Instruction::I64ExtendI32U);
            Ok(())
        }
        // String.len already lives behind a builtin helper (legacy
        // matched behaviour). Map String.length here to keep both
        // surface spellings viable without a second helper.
        "String.length" | "String.byteLength" if args.len() == 1 => {
            let len_idx =
                ctx.fn_map
                    .builtins
                    .get("String.len")
                    .copied()
                    .ok_or(WasmGcError::Validation(
                        "String.length / byteLength require the String.len builtin".into(),
                    ))?;
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::Call(len_idx));
            Ok(())
        }
        // Char.toCode(s) -> Int — first byte of the 1-char string.
        // Aver `Char` is just a `String` (single byte today), so this
        // is a straight `array.get_u 0 + i64.extend`.
        "Char.toCode" if args.len() == 1 => {
            let s_idx = ctx
                .registry
                .string_array_type_idx
                .ok_or(WasmGcError::Validation(
                    "Char.toCode requires the String slot allocated".into(),
                ))?;
            emit_expr(func, &args[0], slots, ctx)?;
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::ArrayGetU(s_idx));
            func.instruction(&Instruction::I64ExtendI32U);
            Ok(())
        }
        // Vector.new(size, fill) -> Vector<T>. Element type T is read
        // off the fill argument. Lowers to native `array.new $vector_T`.
        "Vector.new" => emit_vector_new(func, args, slots, ctx),
        // Boxed `Vector.get(v, i) -> Option<T>` — bounds-check, return
        // `Option.Some(arr[i])` or `Option.None`. Used when the caller
        // doesn't fuse via `Option.withDefault` (e.g. pattern-match
        // through Option directly).
        "Vector.get" if args.len() == 2 => {
            emit_vector_get_boxed(func, &args[0], &args[1], slots, ctx)
        }
        // Boxed `Vector.set(v, i, x) -> Option<Vector<T>>`. Mutates
        // the backing array in place on bounds-check success and
        // returns `Option.Some(v)`; OOB returns `Option.None` without
        // touching the array. Aver's surface semantics match the
        // legacy backend (the fused `Option.withDefault(Vector.set,
        // v)` shape collapses to an in-place set-and-return-handle).
        "Vector.set" if args.len() == 3 => {
            emit_vector_set_boxed(func, &args[0], &args[1], &args[2], slots, ctx)
        }
        // Option.withDefault(opt, default) — recognise the two fused
        // shapes that show up in vector_ops without ever materialising
        // an Option<T>. Anything else needs real Option boxing, which
        // a later phase introduces when it stops being avoidable.
        "Option.withDefault" => emit_option_with_default(func, args, slots, ctx),
        "Result.withDefault" => emit_result_with_default(func, args, slots, ctx),
        // Option.toResult(opt, err) — `match opt { Some(v) -> Ok(v);
        // None -> Err(err) }`. Picks the Result<T, E> canonical out
        // of the inferred Option element type + the err arg's type.
        "Option.toResult" if args.len() == 2 => {
            emit_option_to_result(func, &args[0], &args[1], slots, ctx)
        }
        // Map<K, V> — dispatch to the per-instantiation helper. The
        // canonical comes from inferring the type of the map argument.
        // Empty map literals (`{}`) flow through `emit_map_literal`
        // — there is no `Map.empty()` builtin.
        "Map.set" | "Map.get" | "Map.len" | "Map.has" | "Map.keys" | "Map.values"
        | "Map.remove" | "Map.entries" => emit_map_kv_call(func, method, args, slots, ctx),
        "Map.fromList" if args.len() == 1 => emit_map_from_list_call(func, &args[0], slots, ctx),
        // List<T> — per-instantiation helpers via `lists::ListOps`.
        "List.reverse" if args.len() == 1 => {
            emit_list_op_call(func, &args[0], "reverse", slots, ctx)
        }
        "List.len" | "List.length" if args.len() == 1 => {
            emit_list_op_call(func, &args[0], "len", slots, ctx)
        }
        "List.concat" if args.len() == 2 => {
            emit_list_op_call_2(func, &args[0], &args[1], "concat", slots, ctx)
        }
        "List.take" if args.len() == 2 => {
            emit_list_op_call_2(func, &args[0], &args[1], "take", slots, ctx)
        }
        "List.drop" if args.len() == 2 => {
            emit_list_op_call_2(func, &args[0], &args[1], "drop", slots, ctx)
        }
        "List.contains" if args.len() == 2 => {
            emit_list_op_call_2(func, &args[0], &args[1], "contains", slots, ctx)
        }
        "List.zip" if args.len() == 2 => emit_list_zip_call(func, &args[0], &args[1], slots, ctx),
        // Vector.fromList(list: List<T>) -> Vector<T>
        "Vector.fromList" if args.len() == 1 => emit_vec_from_list_call(func, &args[0], slots, ctx),
        "List.fromVector" if args.len() == 1 => emit_vec_to_list_call(func, &args[0], slots, ctx),
        // String.split / String.join — singleton (T=String).
        "String.split" if args.len() == 2 => {
            let ops = ctx.fn_map.string_split_ops.ok_or(WasmGcError::Validation(
                "String.split called but split helper wasn't registered".into(),
            ))?;
            emit_expr(func, &args[0], slots, ctx)?;
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::Call(ops.split));
            Ok(())
        }
        "String.join" if args.len() == 2 => {
            let ops = ctx.fn_map.string_split_ops.ok_or(WasmGcError::Validation(
                "String.join called but join helper wasn't registered".into(),
            ))?;
            emit_expr(func, &args[0], slots, ctx)?;
            emit_expr(func, &args[1], slots, ctx)?;
            func.instruction(&Instruction::Call(ops.join));
            Ok(())
        }
        other => Err(WasmGcError::Validation(format!(
            "wasm-gc: builtin or method call `{other}` is not yet implemented \
             (no helper registered, no effect import, no inline lowering). \
             If this looks like it should work, file at \
             https://github.com/jasisz/aver/issues with the source program."
        ))),
    }
}

/// Inline lowering of `Args.get()` (no args, returns `List<String>`).
/// Host imports are `args_len(): i64` and `args_get(i: i64): String`;
/// no `args_get_all`. Walks `i = len-1 .. 0` cons-building the list so
/// the result lands in source-arg order without a final reverse pass.
/// Uses the four scratch slots reserved by `slots::SlotTable` (i, len,
/// acc, s).
pub(super) fn emit_args_get_inline(
    func: &mut Function,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let [i_slot, len_slot, acc_slot, s_slot] =
        slots.args_get_scratch.ok_or(WasmGcError::Validation(
            "Args.get() inline expansion requires 4 reserved scratch slots — \
             slots::SlotTable should have allocated them via fn_needs_args_get_scratch"
                .into(),
        ))?;
    let args_len_idx =
        ctx.fn_map
            .effects
            .get("Args.len")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Args.get() inline needs the Args.len effect import — \
             discovery walker should register it when Args.get() is reachable"
                    .into(),
            ))?;
    let args_get_idx =
        ctx.fn_map
            .effects
            .get("Args.get")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Args.get() inline needs the Args.get effect import (the i64 → String form)".into(),
            ))?;
    let list_string_idx =
        ctx.registry
            .list_type_idx("List<String>")
            .ok_or(WasmGcError::Validation(
                "Args.get() inline needs the List<String> registry slot".into(),
            ))?;

    // len = args_len()
    super::emit::emit_caller_fn_idx(func, ctx)?;
    func.instruction(&Instruction::Call(args_len_idx));
    func.instruction(&Instruction::LocalSet(len_slot));
    // i = len - 1
    func.instruction(&Instruction::LocalGet(len_slot));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::LocalSet(i_slot));
    // acc = ref.null List<String>
    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        list_string_idx,
    )));
    func.instruction(&Instruction::LocalSet(acc_slot));

    func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if i < 0: br out (depth 1 — the surrounding block)
    func.instruction(&Instruction::LocalGet(i_slot));
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64LtS);
    func.instruction(&Instruction::BrIf(1));
    // s = args_get(i)
    func.instruction(&Instruction::LocalGet(i_slot));
    super::emit::emit_caller_fn_idx(func, ctx)?;
    func.instruction(&Instruction::Call(args_get_idx));
    func.instruction(&Instruction::LocalSet(s_slot));
    // acc = struct.new List<String> { head: s, tail: acc }
    func.instruction(&Instruction::LocalGet(s_slot));
    func.instruction(&Instruction::LocalGet(acc_slot));
    func.instruction(&Instruction::StructNew(list_string_idx));
    func.instruction(&Instruction::LocalSet(acc_slot));
    // i = i - 1
    func.instruction(&Instruction::LocalGet(i_slot));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::LocalSet(i_slot));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // end loop
    func.instruction(&Instruction::End); // end block

    // result on stack: acc
    func.instruction(&Instruction::LocalGet(acc_slot));
    Ok(())
}

/// Map.set / Map.get / Map.len dispatch — the canonical is recovered
/// from the map argument's inferred type, helper indices come from
/// `fn_map.map_helpers`.
pub(super) fn emit_map_kv_call(
    func: &mut Function,
    method: &str,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let arity = match method {
        "set" => 3,
        "get" | "has" | "remove" => 2,
        "len" | "keys" | "values" | "entries" => 1,
        _ => panic!(
            "internal compiler error: emit_map_kv_call invoked for unknown \
             Map method `{method}`; dispatch in emit_dotted_builtin must only \
             route the listed Map methods here. \
             Please file at https://github.com/jasisz/aver/issues"
        ),
    };
    if args.len() != arity {
        return Err(WasmGcError::Validation(format!(
            "Map.{method} expects {arity} args, got {}",
            args.len()
        )));
    }
    let map_aver_raw = aver_type_str_of(&args[0]);
    let canonical: String = map_aver_raw
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let helpers = ctx
        .fn_map
        .map_helpers_lookup(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.{method}: map argument has type `{map_aver_raw}` but no helpers are registered"
        )))?;
    // `Map.has(m, k) -> Bool` reuses the `get_pair` helper which
    // returns `(found: i32, value: V)` and drops the value, leaving
    // just `found` on the stack — no Option<V> ever allocates.
    if method == "has" {
        for arg in args {
            emit_expr(func, arg, slots, ctx)?;
        }
        func.instruction(&Instruction::Call(helpers.get_pair));
        func.instruction(&Instruction::Drop);
        return Ok(());
    }
    let target_idx = match method {
        "set" => {
            // `Map.set` has two helpers: a clone-on-write `set` and a
            // `set_in_place` that skips the entry-time `array.copy`.
            // The IR alias pass + last-use tell us which is sound: if
            // the map slot is uniquely owned at this call site, every
            // alias-via-`Vector.new(_, m)` / `Map.get(_)` / param has
            // either died or never existed, so mutating its keys /
            // values arrays is safe.
            if ctx.arg_uniquely_owned(&args[0]) {
                helpers.set_in_place
            } else {
                helpers.set
            }
        }
        "get" => helpers.get,
        "len" => helpers.len,
        "keys" => helpers.keys,
        "values" => helpers.values,
        "remove" => helpers.remove,
        "entries" => helpers.entries,
        _ => panic!(
            "internal compiler error: emit_map_kv_call passed arity check \
             but failed helpers dispatch for method `{method}`; the two \
             match arms in this function must cover the same set. \
             Please file at https://github.com/jasisz/aver/issues"
        ),
    };
    for arg in args {
        emit_expr(func, arg, slots, ctx)?;
    }
    func.instruction(&Instruction::Call(target_idx));
    Ok(())
}

/// `Vector.new(size, fill)` → `array.new $vector_T`. Element type comes
/// from the fill argument's Aver type; the registry must already have
/// the matching `Vector<T>` slot (`TypeRegistry::build` walks fn
/// signatures so any reachable instantiation registers).
pub(super) fn emit_vector_new(
    func: &mut Function,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Vector.new expects 2 args, got {}",
            args.len()
        )));
    }
    let elem_aver = aver_type_str_of(&args[1]);
    // Registry trims whitespace from canonicals (`Map<String, Int>` →
    // `Map<String,Int>`). Match that here so a `Map<K, V>` element
    // produced by typecheck still resolves the right slot.
    let canonical: String = format!("Vector<{}>", elem_aver)
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.new: instantiation `{canonical}` was not registered \
             (TypeRegistry expected to discover it from a signature)"
        )))?;
    // wasm `array.new $T` pops [value, size:i32]. Aver pushes size
    // (i64) then fill — we re-order to match wasm's stack discipline.
    emit_expr(func, &args[1], slots, ctx)?; // fill
    emit_expr(func, &args[0], slots, ctx)?; // size i64
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::ArrayNew(vec_idx));
    Ok(())
}

/// `Option.withDefault(opt, default)` — recognise fused shapes that
/// avoid ever boxing an Option<T>:
///
/// - `Option.withDefault(Vector.set(v, i, x), v)` where the default IS
///   the same vector. Lowers to a bounds-checked `array.set` (in-place
///   mutation, semantically a fresh array) returning `v`. No Option
///   ever exists at runtime.
/// - `Option.withDefault(Vector.get(v, i), default_literal)` — bounds
///   check + `array.get`, falling through to the literal on out-of-range.
///
/// Anything else is a real Option<T> that survives past optimisation,
/// which a later phase will represent with a struct or a nullable ref;
/// today it surfaces as Unimplemented.
pub(super) fn emit_option_with_default(
    func: &mut Function,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Option.withDefault expects 2 args, got {}",
            args.len()
        )));
    }
    let opt_arg = &args[0];
    let default_arg = &args[1];

    // Try the shared IR-level leaf classifier first — same code path
    // the Rust / Lean backends use, so adding a new fused shape (e.g.
    // `IntModOrDefaultLiteral`) lights up across every backend
    // automatically. The classifier is re-run on the parent call
    // because `Option.withDefault` is the shape's outer shell.
    let outer_call = Expr::FnCall(
        Box::new(Spanned::new(
            Expr::Attr(
                Box::new(Spanned::new(Expr::Ident("Option".into()), 0)),
                "withDefault".into(),
            ),
            0,
        )),
        args.to_vec(),
    );
    if let Some(leaf) = classify_leaf_op(&outer_call, ctx) {
        match leaf {
            LeafOp::VectorSetOrDefaultSameVector {
                vector,
                index,
                value,
            } => {
                return emit_vector_set_or_default(func, vector, index, value, slots, ctx);
            }
            LeafOp::VectorGetOrDefaultLiteral {
                vector,
                index,
                default_literal,
            } => {
                let default_spanned = Spanned::new(Expr::Literal(default_literal.clone()), 0);
                return emit_vector_get_or_default(
                    func,
                    vector,
                    index,
                    &default_spanned,
                    slots,
                    ctx,
                );
            }
            _ => {}
        }
    }

    // `Option.withDefault(Map.get(m, k), default)` — Map fusion isn't
    // in `LeafOp` (legacy backends use runtime helpers and don't need
    // a per-shape leaf), so handle it locally.
    if let Expr::FnCall(inner_callee, inner_args) = &opt_arg.node
        && let Expr::Attr(parent, member) = &inner_callee.node
        && let Expr::Ident(p) = &parent.node
        && p == "Map"
        && member == "get"
        && inner_args.len() == 2
    {
        return emit_map_get_or_default(
            func,
            &inner_args[0],
            &inner_args[1],
            default_arg,
            slots,
            ctx,
        );
    }

    // Real Option<T> boxing fallback — `Option.withDefault` over an
    // arbitrary Option-producing call. The value materialises as a
    // concrete struct; dispatch through tag-based pattern match.
    emit_option_with_default_boxed(func, opt_arg, default_arg, slots, ctx)
}

/// `Result.withDefault(res, default)` — emits res, reads tag, returns
/// the Ok payload or the default. No fused shape today (no
/// surface-level Map.get-equivalent that produces Result; common
/// pattern is `Result.withDefault(Int.mod(a, b), 0)` which is fused
/// at the IR level by `LeafOp::IntModOrDefaultLiteral` — that one we
/// deliberately don't lower here yet because the bench scenarios
/// hitting it route through pattern match instead).
pub(super) fn emit_result_with_default(
    func: &mut Function,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Result.withDefault expects 2 args, got {}",
            args.len()
        )));
    }
    let res_arg = &args[0];
    let default_arg = &args[1];

    // Fused shape: `Result.withDefault(Int.mod(a, b), default)` —
    // `Int.mod` is lowered to the Euclidean-modulo helper (no Result
    // struct ever materialises), so the boxed-Result emit below would
    // expect a struct ref where there's only an i64 on the stack.
    // Emit the safe form here: if `b == 0` push `default`, else push
    // `__int_mod_euclid(a, b)`.
    if let Expr::FnCall(callee, inner_args) = &res_arg.node
        && let Expr::Attr(parent, member) = &callee.node
        && let Expr::Ident(p) = &parent.node
        && p == "Int"
        && member == "mod"
        && inner_args.len() == 2
    {
        let block_ty = wasm_encoder::BlockType::Result(ValType::I64);
        // if b == 0
        emit_expr(func, &inner_args[1], slots, ctx)?;
        func.instruction(&Instruction::I64Const(0));
        func.instruction(&Instruction::I64Eq);
        func.instruction(&Instruction::If(block_ty));
        emit_expr(func, default_arg, slots, ctx)?;
        func.instruction(&Instruction::Else);
        emit_expr(func, &inner_args[0], slots, ctx)?;
        emit_expr(func, &inner_args[1], slots, ctx)?;
        let mod_idx =
            ctx.fn_map
                .builtins
                .get("__int_mod_euclid")
                .copied()
                .ok_or(WasmGcError::Validation(
                    "Int.mod requires __int_mod_euclid helper to be registered".into(),
                ))?;
        func.instruction(&Instruction::Call(mod_idx));
        func.instruction(&Instruction::End);
        return Ok(());
    }

    let res_aver = aver_type_str_of(res_arg);
    let canonical: String = res_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Result.withDefault: arg of type `{res_aver}` is not a registered Result<T,E>"
        )))?;
    let (t_aver, _) = TypeRegistry::result_te(&canonical).ok_or(WasmGcError::Validation(
        format!("Result canonical `{canonical}` malformed"),
    ))?;
    let elem_val = aver_to_wasm(t_aver, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
        format!("Result.withDefault: T type `{t_aver}` has no wasm representation"),
    ))?;
    let block_ty = wasm_encoder::BlockType::Result(elem_val);
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Result.withDefault needs a scratch slot but none was reserved".into(),
    ))?;

    emit_expr(func, res_arg, slots, ctx)?;
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
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(res_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::Else);
    emit_expr(func, default_arg, slots, ctx)?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// Generic `Option.withDefault(opt, default)` — emits `opt`, reads
/// its tag, returns either the value field or the default. Used when
/// no fused shape applies. Allocates the Option if `opt` is itself a
/// shape that allocates (e.g. `Map.get`); the surrounding caller is
/// expected to use a fused emitter when the alloc is avoidable.
/// `Option.toResult(opt, err) -> Result<T, E>`. Inline-emit the
/// pattern match: tag-check on the boxed Option, then either build
/// `Result.Ok(opt.value)` or `Result.Err(err)`. T comes from the
/// inferred Option<T>, E from the err argument's type.
pub(super) fn emit_option_to_result(
    func: &mut Function,
    opt_arg: &Spanned<Expr>,
    err_arg: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let opt_aver = aver_type_str_of(opt_arg);
    let opt_canonical: String = opt_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let opt_idx = ctx
        .registry
        .option_type_idx(&opt_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option.toResult: opt arg of type `{opt_aver}` is not a registered Option<T>"
        )))?;
    let t_aver = super::super::types::TypeRegistry::option_element_type(&opt_canonical).ok_or(
        WasmGcError::Validation(format!(
            "Option.toResult: cannot parse element type from `{opt_canonical}`"
        )),
    )?;
    let e_aver = aver_type_str_of(err_arg);
    let result_canonical: String = format!("Result<{},{}>", t_aver.trim(), e_aver.trim())
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let res_idx = ctx
        .registry
        .result_type_idx(&result_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option.toResult: `{result_canonical}` slot was not registered (the Result instantiation \
             needs to appear in a fn signature or be auto-discovered from a builtin's return type)"
        )))?;

    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Option.toResult needs a scratch slot but none was reserved".into(),
    ))?;
    let res_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(res_idx),
    });
    let block_ty = wasm_encoder::BlockType::Result(res_ref);

    emit_expr(func, opt_arg, slots, ctx)?;
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
    // Result.Ok(opt.value)
    func.instruction(&Instruction::I32Const(1)); // tag
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(opt_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 1,
    });
    emit_default_value(func, e_aver.trim(), ctx.registry)?;
    func.instruction(&Instruction::StructNew(res_idx));
    func.instruction(&Instruction::Else);
    // Result.Err(err)
    func.instruction(&Instruction::I32Const(0));
    emit_default_value(func, t_aver.trim(), ctx.registry)?;
    emit_expr(func, err_arg, slots, ctx)?;
    func.instruction(&Instruction::StructNew(res_idx));
    func.instruction(&Instruction::End);
    Ok(())
}

pub(super) fn emit_option_with_default_boxed(
    func: &mut Function,
    opt_arg: &Spanned<Expr>,
    default_arg: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let opt_aver = aver_type_str_of(opt_arg);
    let canonical: String = opt_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let opt_idx = ctx
        .registry
        .option_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option.withDefault: opt arg of type `{opt_aver}` is not a registered Option<T>"
        )))?;
    let element = super::super::types::TypeRegistry::option_element_type(&canonical).ok_or(
        WasmGcError::Validation(format!(
            "Option.withDefault: cannot parse element type from `{canonical}`"
        )),
    )?;
    let elem_val = aver_to_wasm(element, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
        format!("Option.withDefault: element type `{element}` has no wasm representation"),
    ))?;
    let block_ty = wasm_encoder::BlockType::Result(elem_val);

    // Stash opt in scratch, peek at tag.
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Option.withDefault (boxed) needs a scratch slot but none was reserved".into(),
    ))?;
    emit_expr(func, opt_arg, slots, ctx)?;
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
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(opt_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::Else);
    emit_expr(func, default_arg, slots, ctx)?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// Fused `Option.withDefault(Map.get(m, k), default)` → call to the
/// per-instantiation `get_or_default` helper. No `Option<V>` ever
/// allocates on the hot lookup path.
pub(super) fn emit_map_get_or_default(
    func: &mut Function,
    map: &Spanned<Expr>,
    key: &Spanned<Expr>,
    default: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let map_aver = aver_type_str_of(map);
    let canonical: String = map_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let helpers = ctx
        .fn_map
        .map_helpers_lookup(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.get fusion: map argument has type `{map_aver}` but no helpers are registered"
        )))?;
    emit_expr(func, map, slots, ctx)?;
    emit_expr(func, key, slots, ctx)?;
    emit_expr(func, default, slots, ctx)?;
    func.instruction(&Instruction::Call(helpers.get_or_default));
    Ok(())
}

/// Fused `Option.withDefault(Vector.set(v, i, x), v)`: clone-on-write.
/// Allocates a fresh array of the same length, copies `v` into it,
/// conditionally writes the new cell, returns the copy. The previous
/// in-place `array.set` short-circuit was unsound: wasm-gc arrays are
/// reference types, and `Vector.new(n, inner)` produces N elements
/// pointing at the same `inner` ref, so a `Vector.get(outer, i)` /
/// `Vector.set(row, …)` chain would silently rewrite every alias of
/// that row. The scratch local that holds the copy is reserved at
/// slot-allocation time (`SlotTable::vector_set_scratch`), one per
/// distinct `Vector<T>` instantiation that this fn body actually
/// calls `Vector.set` on.
pub(super) fn emit_vector_set_or_default(
    func: &mut Function,
    vector: &Spanned<Expr>,
    index: &Spanned<Expr>,
    value: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let vec_aver = aver_type_str_of(vector);
    let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.set: vector arg of type `{vec_aver}` is not a registered Vector<T>"
        )))?;

    // Fast path: when the IR alias pass guarantees the vec slot has
    // no other live alias and `last_use` says the binding is dead,
    // the engine array can be mutated in place. Skip the
    // `array.new_default` + `array.copy` and just `array.set` on the
    // original handle — the result of the fused
    // `Result.withDefault(Vector.set(v, i, x), v)` is the same handle
    // anyway. No scratch local, no allocation.
    if ctx.arg_uniquely_owned(vector) {
        emit_expr(func, index, slots, ctx)?;
        func.instruction(&Instruction::I64Const(0));
        func.instruction(&Instruction::I64GeS);
        emit_expr(func, index, slots, ctx)?;
        func.instruction(&Instruction::I32WrapI64);
        emit_expr(func, vector, slots, ctx)?;
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::I32LtU);
        func.instruction(&Instruction::I32And);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        emit_expr(func, vector, slots, ctx)?;
        emit_expr(func, index, slots, ctx)?;
        func.instruction(&Instruction::I32WrapI64);
        emit_expr(func, value, slots, ctx)?;
        func.instruction(&Instruction::ArraySet(vec_idx));
        func.instruction(&Instruction::End);
        emit_expr(func, vector, slots, ctx)?;
        return Ok(());
    }

    let scratch = slots
        .vector_set_scratch
        .get(&canonical)
        .copied()
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "Vector.set: scratch local for `{canonical}` not reserved \
                 (slot-pre-pass missed this site)"
            ))
        })?;

    // Slow path (clone-on-write): the vec slot may share its engine
    // array with another live binding. Allocate a fresh array, copy
    // every cell, mutate the copy.
    emit_expr(func, vector, slots, ctx)?;
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::ArrayNewDefault(vec_idx));
    func.instruction(&Instruction::LocalSet(scratch));

    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::I32Const(0));
    emit_expr(func, vector, slots, ctx)?;
    func.instruction(&Instruction::I32Const(0));
    emit_expr(func, vector, slots, ctx)?;
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: vec_idx,
        array_type_index_src: vec_idx,
    });

    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64GeS);
    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::LocalGet(scratch));
    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    emit_expr(func, value, slots, ctx)?;
    func.instruction(&Instruction::ArraySet(vec_idx));
    func.instruction(&Instruction::End);

    func.instruction(&Instruction::LocalGet(scratch));
    Ok(())
}

/// `Expr::InterpolatedStr(parts)` — wasm-gc lowers interpolations to
/// `array.new_fixed (array (ref null $string)) N` + a single call to
/// the variadic concat helper. Each part is coerced to `String`:
/// - `String` → identity
/// - `Int` → `call $String.fromInt`
/// - other primitives surface as Unimplemented until their helpers land
///
/// `interp_lower` is skipped for this backend (`run_interp_lower=false`
/// in the wasm-gc pipeline config) because the `__buf_*` shape it
/// produces targets bump-allocator backends (linear memory + grow-on-
/// append). The variadic shape is O(total_len) bytes copied — same
/// asymptotics a real mutable buffer would achieve, without the
/// `(struct len array)` wrapper or the per-append realloc cost of a
/// left-folded concat chain. Same primitive will back `String.join`
/// once it lands (interleave separators, then call this helper).
pub(super) fn emit_interpolated_str(
    func: &mut Function,
    parts: &[crate::ast::StrPart],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    use crate::ast::StrPart;
    let string_type_idx = ctx
        .registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "InterpolatedStr reachable but no String type slot allocated".into(),
        ))?;
    if parts.is_empty() {
        // Empty interpolation → empty String. Allocate a zero-length
        // array directly; cheaper than going through the helper.
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        return Ok(());
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
            StrPart::Literal(s) => {
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
            StrPart::Parsed(inner) => {
                let aver_ty = aver_type_str_of(inner);
                emit_expr(func, inner, slots, ctx)?;
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
                    other => {
                        return Err(WasmGcError::Validation(format!(
                            "phase 3c — interpolation of compound type `{other}`. \
                             Stringify at the call site (e.g. a per-type render fn \
                             that pattern-matches into primitive interpolations) so \
                             Console.print's argument stays a plain String."
                        )));
                    }
                }
            }
        }
    }
    func.instruction(&Instruction::ArrayNewFixed {
        array_type_index: vec_idx,
        array_size: parts.len() as u32,
    });
    func.instruction(&Instruction::Call(concat_idx));
    Ok(())
}

/// Fused `Option.withDefault(Vector.get(v, i), default)`: bounds-checked
/// `array.get`, falls back to the default on out-of-range. The result
/// type is the vector's element type (Aver guarantees `default` agrees).
pub(super) fn emit_vector_get_or_default(
    func: &mut Function,
    vector: &Spanned<Expr>,
    index: &Spanned<Expr>,
    default: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let vec_aver = aver_type_str_of(vector);
    let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.get: vector arg of type `{vec_aver}` is not a registered Vector<T>"
        )))?;

    // Result wasm type = element type. Block must declare it so both
    // arms unify on the stack shape.
    let element = super::super::types::TypeRegistry::vector_element_type(&canonical).ok_or(
        WasmGcError::Validation(format!(
            "Vector.get: cannot parse element type from `{canonical}`"
        )),
    )?;
    let elem_val = aver_to_wasm(element, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
        format!("Vector.get: element type `{element}` has no wasm representation"),
    ))?;
    let block_ty = wasm_encoder::BlockType::Result(elem_val);

    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64GeS);

    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    emit_expr(func, vector, slots, ctx)?;
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);

    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(block_ty));
    emit_expr(func, vector, slots, ctx)?;
    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::ArrayGet(vec_idx));
    func.instruction(&Instruction::Else);
    emit_expr(func, default, slots, ctx)?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// `List.reverse(list)` / `List.len(list)` — dispatch to the
/// per-`List<T>` helper registered in `fn_map.list_ops`. The
/// canonical comes from `infer_aver_type(list)`.
pub(super) fn emit_list_op_call(
    func: &mut Function,
    list_arg: &Spanned<Expr>,
    op: &str,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let list_aver = aver_type_str_of(list_arg);
    let canonical = super::super::types::normalize_compound(&list_aver);
    let ops = ctx
        .fn_map
        .list_ops_lookup(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List.{op} called but `{canonical}` helper wasn't registered"
        )))?;
    emit_expr(func, list_arg, slots, ctx)?;
    let fn_idx = match op {
        "reverse" => ops.reverse,
        "len" => ops.len,
        _ => {
            return Err(WasmGcError::Validation(format!(
                "emit_list_op_call: unknown op `{op}`"
            )));
        }
    };
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// `List.concat(a, b) / take(l, n) / drop(l, n) / contains(l, x)` —
/// 2-arg per-`List<T>` helpers. The canonical comes from the first
/// list arg's inferred type. For `contains` over a `T` we can't
/// natively eq-compare (records, sums) the helper isn't registered
/// and the call surfaces a clear error.
pub(super) fn emit_list_op_call_2(
    func: &mut Function,
    list_arg: &Spanned<Expr>,
    second_arg: &Spanned<Expr>,
    op: &str,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let list_aver = aver_type_str_of(list_arg);
    let canonical = super::super::types::normalize_compound(&list_aver);
    let ops = ctx
        .fn_map
        .list_ops_lookup(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List.{op} called but `{canonical}` helper wasn't registered"
        )))?;
    emit_expr(func, list_arg, slots, ctx)?;
    emit_expr(func, second_arg, slots, ctx)?;
    let fn_idx = match op {
        "concat" => ops.concat,
        "take" => ops.take,
        "drop" => ops.drop,
        "contains" => ops.contains.ok_or(WasmGcError::Validation(format!(
            "List.contains over `{canonical}`: element type isn't natively eq-able \
             (only Int/Float/Bool/String/Char are supported today)"
        )))?,
        _ => {
            return Err(WasmGcError::Validation(format!(
                "emit_list_op_call_2: unknown op `{op}`"
            )));
        }
    };
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// `Map.fromList(l) -> Map<K, V>` — dispatch to the per-(K, V)
/// from_list helper. The Map<K, V> canonical comes from the input
/// list's element type (must be `Tuple<K, V>`).
pub(super) fn emit_map_from_list_call(
    func: &mut Function,
    list_arg: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let list_aver = aver_type_str_of(list_arg);
    let list_canonical: String = list_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let elem = TypeRegistry::list_element_type(&list_canonical).ok_or(WasmGcError::Validation(
        format!("Map.fromList: input `{list_aver}` is not a List<Tuple<K,V>>"),
    ))?;
    let (k, v) = TypeRegistry::tuple_ab(elem).ok_or(WasmGcError::Validation(format!(
        "Map.fromList: list element `{elem}` is not a Tuple<K, V>"
    )))?;
    let map_canonical = format!("Map<{},{}>", k.trim(), v.trim());
    let helpers = ctx
        .fn_map
        .map_helpers_lookup(&map_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.fromList: helpers for `{map_canonical}` not registered"
        )))?;
    emit_expr(func, list_arg, slots, ctx)?;
    func.instruction(&Instruction::Call(helpers.from_list));
    Ok(())
}

/// `List.zip(la, lb) -> List<Tuple<A, B>>` — dispatch to the
/// per-`Tuple<A,B>` zip helper. Recover A/B from the input lists'
/// element types.
pub(super) fn emit_list_zip_call(
    func: &mut Function,
    la: &Spanned<Expr>,
    lb: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let la_aver = aver_type_str_of(la);
    let lb_aver = aver_type_str_of(lb);
    let a = TypeRegistry::list_element_type(&la_aver).ok_or(WasmGcError::Validation(format!(
        "List.zip: first arg type `{la_aver}` is not a List<T>"
    )))?;
    let b = TypeRegistry::list_element_type(&lb_aver).ok_or(WasmGcError::Validation(format!(
        "List.zip: second arg type `{lb_aver}` is not a List<T>"
    )))?;
    let tup_canonical = format!("Tuple<{},{}>", a.trim(), b.trim());
    let zip_fn = ctx
        .fn_map
        .zip_ops_lookup(&tup_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List.zip: helper for `{tup_canonical}` wasn't registered"
        )))?;
    emit_expr(func, la, slots, ctx)?;
    emit_expr(func, lb, slots, ctx)?;
    func.instruction(&Instruction::Call(zip_fn));
    Ok(())
}

/// `Vector.fromList(list)` — dispatch to the `from_list` helper
/// registered for the matching `List<T>` canonical.
pub(super) fn emit_vec_from_list_call(
    func: &mut Function,
    list_arg: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let list_aver = aver_type_str_of(list_arg);
    let canonical = super::super::types::normalize_compound(&list_aver);
    let ops = ctx
        .fn_map
        .vfl_ops_lookup(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.fromList: helper for `{canonical}` wasn't registered \
             (matching Vector<T> may be missing from the registry)"
        )))?;
    emit_expr(func, list_arg, slots, ctx)?;
    func.instruction(&Instruction::Call(ops.from_list));
    Ok(())
}

/// `List.fromVector(vec)` — dispatch to the `to_list` helper. The
/// canonical is keyed on `List<T>` (`vfl_ops` indexes pairs by list
/// canonical), so we recover `T` from the vector arg's type and
/// build the list canonical from it.
pub(super) fn emit_vec_to_list_call(
    func: &mut Function,
    vec_arg: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let vec_aver = aver_type_str_of(vec_arg);
    let vec_canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let elem = super::super::types::TypeRegistry::vector_element_type(&vec_canonical).ok_or(
        WasmGcError::Validation(format!(
            "List.fromVector: cannot parse element type from `{vec_canonical}`"
        )),
    )?;
    let list_canonical = format!("List<{}>", elem.trim());
    let ops = ctx
        .fn_map
        .vfl_ops
        .get(&list_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List.fromVector: helper for `{list_canonical}` wasn't registered"
        )))?;
    emit_expr(func, vec_arg, slots, ctx)?;
    func.instruction(&Instruction::Call(ops.to_list));
    Ok(())
}

/// Boxed `Vector.get(v, i) -> Option<T>`. Bounds-check then build a
/// real `Option<T>` struct: `Option.Some(arr[i])` on success,
/// `Option.None` on out-of-range. Used when the call result actually
/// flows through pattern match (rather than collapsing via the fused
/// `Option.withDefault(Vector.get(...), default)` shape).
pub(super) fn emit_vector_get_boxed(
    func: &mut Function,
    vector: &Spanned<Expr>,
    index: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let vec_aver = aver_type_str_of(vector);
    let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.get: vector arg of type `{vec_aver}` is not a registered Vector<T>"
        )))?;
    let element = super::super::types::TypeRegistry::vector_element_type(&canonical).ok_or(
        WasmGcError::Validation(format!(
            "Vector.get: cannot parse element type from `{canonical}`"
        )),
    )?;
    let opt_canonical = format!("Option<{}>", element.trim());
    let opt_idx = ctx
        .registry
        .option_type_idx(&opt_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.get: `{opt_canonical}` slot was not registered"
        )))?;

    // Both arms push a `(ref null $option_T)` so the if-block's
    // result type is the option ref.
    let opt_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(opt_idx),
    });
    let block_ty = wasm_encoder::BlockType::Result(opt_ref);

    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64GeS);

    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    emit_expr(func, vector, slots, ctx)?;
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);

    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(block_ty));
    // In-range: Option.Some(arr[i]) → struct.new $option_T
    // Option layout: `(struct (mut i32 tag) (mut T value))`, tag = 1.
    func.instruction(&Instruction::I32Const(1));
    emit_expr(func, vector, slots, ctx)?;
    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::ArrayGet(vec_idx));
    func.instruction(&Instruction::StructNew(opt_idx));
    func.instruction(&Instruction::Else);
    // Out-of-range: Option.None → struct.new with tag = 0, default
    // value for the field.
    func.instruction(&Instruction::I32Const(0));
    emit_default_value(func, element, ctx.registry)?;
    func.instruction(&Instruction::StructNew(opt_idx));
    func.instruction(&Instruction::End);
    Ok(())
}

/// Boxed `Vector.set(v, i, x) -> Option<Vector<T>>`. Clone-on-write:
/// allocates a fresh array of the same length, copies `v` into it,
/// writes the new cell, returns `Option.Some(copy)`. OOB returns
/// `Option.None`. Aver-surface semantics: the input vector remains
/// observable through any other reference unchanged; the returned
/// `Some(_)` is the only path to the modified state. (See the
/// header comment on `emit_vector_set_or_default` for the aliasing
/// argument that forced the copy.)
pub(super) fn emit_vector_set_boxed(
    func: &mut Function,
    vector: &Spanned<Expr>,
    index: &Spanned<Expr>,
    value: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let vec_aver = aver_type_str_of(vector);
    let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.set: vector arg of type `{vec_aver}` is not a registered Vector<T>"
        )))?;
    let opt_canonical = format!("Option<{canonical}>");
    let opt_idx = ctx
        .registry
        .option_type_idx(&opt_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.set: `{opt_canonical}` slot was not registered"
        )))?;
    let opt_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(opt_idx),
    });
    let block_ty = wasm_encoder::BlockType::Result(opt_ref);

    // Fast path: alias-free + last_use → mutate the engine array in
    // place. The boxed result is `Some(v)` (same handle) when in
    // bounds, `None` otherwise — no `array.new_default` / `array.copy`.
    if ctx.arg_uniquely_owned(vector) {
        emit_expr(func, index, slots, ctx)?;
        func.instruction(&Instruction::I64Const(0));
        func.instruction(&Instruction::I64GeS);
        emit_expr(func, index, slots, ctx)?;
        func.instruction(&Instruction::I32WrapI64);
        emit_expr(func, vector, slots, ctx)?;
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::I32LtU);
        func.instruction(&Instruction::I32And);
        func.instruction(&Instruction::If(block_ty));
        emit_expr(func, vector, slots, ctx)?;
        emit_expr(func, index, slots, ctx)?;
        func.instruction(&Instruction::I32WrapI64);
        emit_expr(func, value, slots, ctx)?;
        func.instruction(&Instruction::ArraySet(vec_idx));
        func.instruction(&Instruction::I32Const(1));
        emit_expr(func, vector, slots, ctx)?;
        func.instruction(&Instruction::StructNew(opt_idx));
        func.instruction(&Instruction::Else);
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
            vec_idx,
        )));
        func.instruction(&Instruction::StructNew(opt_idx));
        func.instruction(&Instruction::End);
        return Ok(());
    }

    let scratch = slots
        .vector_set_scratch
        .get(&canonical)
        .copied()
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "Vector.set: scratch local for `{canonical}` not reserved \
                 (slot-pre-pass missed this site)"
            ))
        })?;

    // Slow path (clone-on-write): the vec slot may share its engine
    // array with another live binding. Bounds: 0 <= i < vec.len.
    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64GeS);
    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    emit_expr(func, vector, slots, ctx)?;
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(block_ty));

    // In-range: copy = array.new_default $T (array.len v); array.copy
    // copy 0 v 0 (array.len v); copy[i] = x; return Some(copy)
    emit_expr(func, vector, slots, ctx)?;
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::ArrayNewDefault(vec_idx));
    func.instruction(&Instruction::LocalSet(scratch));

    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::I32Const(0));
    emit_expr(func, vector, slots, ctx)?;
    func.instruction(&Instruction::I32Const(0));
    emit_expr(func, vector, slots, ctx)?;
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: vec_idx,
        array_type_index_src: vec_idx,
    });

    func.instruction(&Instruction::LocalGet(scratch));
    emit_expr(func, index, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    emit_expr(func, value, slots, ctx)?;
    func.instruction(&Instruction::ArraySet(vec_idx));

    // tag=1 + the new copy ref
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::StructNew(opt_idx));
    func.instruction(&Instruction::Else);
    // OOB: tag=0, value=null vec ref
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        vec_idx,
    )));
    func.instruction(&Instruction::StructNew(opt_idx));
    func.instruction(&Instruction::End);
    Ok(())
}

