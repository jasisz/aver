//! Wasip2-specific call-site emit functions for `Type.method(args)`
//! builtins. Each function here is the wasip2 counterpart of an
//! aver-bridge handler in `builtins.rs`'s main switch — `Console.*`,
//! `Args.get`, `Env.get`, `Time.*`, `Random.*`, `Disk.*` — and is
//! dispatched to from the MIR builtin emitter
//! (`from_mir::emit_mir_wasip2_effect`) when the surrounding emit ctx
//! is in `TargetMode::Wasip2`.
//!
//! The actual canonical-ABI helper bodies live one layer deeper in
//! `wasip2_helpers.rs` (sibling of `module.rs`); the functions here
//! are call-site marshalling — push args (via `emit_mir_expr`),
//! `Call(helper_fn_idx)`. They take MIR-form args because MIR is the
//! only codegen path; they never inspect arg structure beyond passing
//! each one to `emit_mir_expr`, so the conversion was purely a type +
//! emit-call swap.

use wasm_encoder::{BlockType, HeapType, Instruction, RefType, ValType};

use crate::ast::Spanned;
use crate::codegen::wasip2::CapabilityWitType;
use crate::ir::mir::MirExpr;

use super::super::WasmGcError;
use super::emit::{emit_default_value, emit_string_literal_bytes};
use super::from_mir::emit_mir_expr;
use super::{EmitCtx, SlotTable};

fn result_block_type(type_idx: u32) -> BlockType {
    BlockType::Result(ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(type_idx),
    }))
}

fn emit_result_err(
    func: &mut wasm_encoder::Function,
    result_type_idx: u32,
    ok_type: &str,
    message: &str,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    func.instruction(&Instruction::I32Const(0));
    emit_default_value(func, ok_type, ctx.registry)?;
    emit_string_literal_bytes(func, message.as_bytes(), ctx)?;
    func.instruction(&Instruction::StructNew(result_type_idx));
    Ok(())
}

/// Lower a custom capability call through its generated internal bridge.
/// Unit arguments are still evaluated in source order but occupy no wasm
/// value slot; every other phase-3a value is passed directly to the bridge.
pub(super) fn emit_capability_call_wasip2(
    func: &mut wasm_encoder::Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<bool, WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("custom capability call missing wasip2 lowering context".into())
    })?;
    let call = lowering.capability_calls.get(dotted).ok_or_else(|| {
        WasmGcError::Validation(format!(
            "custom capability `{dotted}` has no generated wasip2 bridge"
        ))
    })?;
    if args.len() != call.params.len() {
        return Err(WasmGcError::Validation(format!(
            "custom capability `{dotted}` expects {} arguments, got {}",
            call.params.len(),
            args.len()
        )));
    }
    for (argument, parameter) in args.iter().zip(&call.params) {
        let produced = emit_mir_expr(func, argument, slots, ctx)?.ok_or_else(|| {
            WasmGcError::Validation(format!(
                "custom capability `{dotted}` argument p{} is not covered by MIR wasm-gc lowering",
                parameter.index
            ))
        })?;
        match (parameter.ty, produced) {
            (CapabilityWitType::Unit, true) => {
                func.instruction(&Instruction::Drop);
            }
            (CapabilityWitType::Unit, false) | (_, true) => {}
            (_, false) => {
                return Err(WasmGcError::Validation(format!(
                    "custom capability `{dotted}` argument p{} produced no wasm value",
                    parameter.index
                )));
            }
        }
    }
    func.instruction(&Instruction::Call(call.helper_fn_idx));
    Ok(call.result != CapabilityWitType::Unit)
}

/// `Int = ℤ`: emit an `Int` effect ARGUMENT and CHECKED-lower it from the
/// `$AverInt` carrier to a raw i64 — the remaining machine-shaped wasip2
/// effect lowerings (notably network ports) compute on i64. An out-of-i64 Big TRAPS
/// (`__aint_to_i64_checked`) rather than saturating, so an out-of-range
/// at this internal ABI boundary. `Random.int` and `Time.sleep` do not use
/// this helper: their call-site validation constructs catchable Aver Results.
/// A no-op passthrough when no Int is reachable (the arg is already i64).
fn emit_aint_arg_as_i64_wasip2(
    func: &mut wasm_encoder::Function,
    arg: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    emit_mir_expr(func, arg, slots, ctx)?;
    if ctx.registry.bignum {
        let to_checked = ctx
            .fn_map
            .builtins
            .get("__aint_to_i64_checked")
            .copied()
            .ok_or(WasmGcError::Validation(
                "bignum active but __aint_to_i64_checked helper not registered".into(),
            ))?;
        func.instruction(&Instruction::Call(to_checked));
    }
    Ok(())
}

/// `Int = ℤ`: lift the raw i64 result of an `Int`-returning wasip2 effect
/// (`Random.int`, `Time.unixMs`) into the `$AverInt` carrier. No-op when
/// no Int is reachable.
fn lift_i64_result_to_aint_wasip2(
    func: &mut wasm_encoder::Function,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if ctx.registry.bignum {
        let from_i64 =
            ctx.fn_map
                .builtins
                .get("__aint_from_i64")
                .copied()
                .ok_or(WasmGcError::Validation(
                    "bignum active but __aint_from_i64 helper not registered".into(),
                ))?;
        func.instruction(&Instruction::Call(from_i64));
    }
    Ok(())
}

/// Phase 1.2b1.5 — call-site lowering for `Console.print` /
/// `Console.error` / `Console.warn` on `--target wasip2`.
///
/// Sequence (single arg `s: String`):
///   1. Lazy-init the cached `output-stream` handle. The handle
///      global starts at `-1`; on first call, invoke
///      `wasi:cli/{stdout,stderr}.get-stdout/stderr` (returns the
///      i32 resource handle) and store it.
///   2. Push `s` (engine-GC `(ref null $string)`), call
///      `__rt_string_to_lm` — that helper writes the utf-8 bytes
///      to LM[0..len], grows memory if needed, and returns `len`.
///      Stash `len` in the per-fn i32 scratch slot.
///   3. Defensive `memory.grow(1)` so the retptr area
///      `[(len+15)&-16, (len+15)&-16 + 12)` cannot fall past the
///      memory boundary even when `len` lands on a page boundary.
///   4. Call `wasi:io/streams.[method]output-stream.blocking-write-
///      and-flush(handle, ptr=0, len, retptr=(len+15)&-16)`. The
///      host writes a 12-byte `result<_, stream-error>` tag at
///      `retptr`; we ignore it (Aver `Console.*` is `Unit`).
pub(super) fn emit_console_print_wasip2(
    func: &mut wasm_encoder::Function,
    method: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation(
            "emit_console_print_wasip2 invoked without wasip2 lowering ctx".into(),
        )
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Console.{method} on `--target wasip2` expects 1 arg (the String), got {}",
            args.len()
        )));
    }
    let scratch = slots.console_print_wasip2_scratch.ok_or_else(|| {
        WasmGcError::Validation(
            "Console.* on wasip2: [len, offset] scratch slots were not allocated by \
             SlotTable — `fn_needs_console_print_wasip2_scratch` did not flag this fn"
                .into(),
        )
    })?;
    let len_local = scratch[0];
    let off_local = scratch[1];

    // Pick stream: stdout for `print`, stderr for `error` / `warn`.
    // The matching `get_*_fn_idx` and `*_handle_global` must be
    // populated whenever this method's effect is registered;
    // anything else is a wiring bug in `module::emit_module_with`.
    let (handle_global, get_fn) = match method {
        "print" => (
            lowering.stdout_handle_global.ok_or_else(|| {
                WasmGcError::Validation(
                    "Console.print on wasip2: stdout_handle global missing — \
                     wasip2_imports did not register CliGetStdout"
                        .into(),
                )
            })?,
            lowering.get_stdout_fn_idx.ok_or_else(|| {
                WasmGcError::Validation("Console.print on wasip2: get_stdout fn idx missing".into())
            })?,
        ),
        "error" | "warn" => (
            lowering.stderr_handle_global.ok_or_else(|| {
                WasmGcError::Validation(
                    "Console.error/warn on wasip2: stderr_handle global missing".into(),
                )
            })?,
            lowering.get_stderr_fn_idx.ok_or_else(|| {
                WasmGcError::Validation(
                    "Console.error/warn on wasip2: get_stderr fn idx missing".into(),
                )
            })?,
        ),
        _ => {
            return Err(WasmGcError::Validation(format!(
                "Console.{method} is not lowered on `--target wasip2`"
            )));
        }
    };

    // Step 1: lazy-init handle.
    func.instruction(&Instruction::GlobalGet(handle_global));
    func.instruction(&Instruction::I32Const(-1));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::Call(get_fn));
    func.instruction(&Instruction::GlobalSet(handle_global));
    func.instruction(&Instruction::End);

    // Step 2: marshal s + trailing '\n' through the println_to_lm
    // helper. Single Call writes the string bytes to LM[0..len],
    // appends `'\n'` at LM[len], and returns `len + 1`. VM and
    // wasm-gc AverBridge both treat `Console.print(s)` as
    // `println!(s)` (see `services::console::write_stdout` /
    // `write_stderr_*`); having a dedicated println helper keeps
    // that semantic at the bridge level instead of patching the
    // length post-hoc at every call site, and lets the chunked
    // write below stay shape-identical with Disk.* / Http.* (the
    // non-newline consumers of plain `__rt_string_to_lm`).
    let println_to_lm = lowering.println_to_lm_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Console.* on wasip2: __rt_println_to_lm fn idx missing — bridge not allocated".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(println_to_lm));
    func.instruction(&Instruction::LocalSet(len_local));

    // Step 3: defensive memory.grow(1) so retptr+12 stays in-bounds
    // even when len landed exactly on a page boundary.
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::MemoryGrow(0));
    func.instruction(&Instruction::Drop);

    let write_fn = lowering.blocking_write_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Console.* on wasip2: blocking-write-and-flush fn idx missing".into(),
        )
    })?;

    // Step 4: chunked write loop. The shared helper walks
    // LM[0..len] in 4096-byte slices (wasmtime-wasi caps single
    // blocking-write-and-flush calls there). retptr is computed
    // inline as `(len + 15) & -16` (16-byte aligned, just past
    // the string bytes); since `len` is a captured i32 local the
    // closure recomputes the alignment per iteration — wasmtime
    // optimises the trivial arithmetic, and we save a scratch
    // slot. Errors are ignored (Console.* is Unit, fire-and-
    // forget; matches the AverBridge / VM target semantics).
    super::super::wasip2_helpers::emit_chunked_blocking_write(
        func,
        len_local,
        off_local,
        write_fn,
        &|f| {
            f.instruction(&Instruction::GlobalGet(handle_global));
        },
        &|f| {
            f.instruction(&Instruction::LocalGet(len_local));
            f.instruction(&Instruction::I32Const(15));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::I32Const(-16));
            f.instruction(&Instruction::I32And);
        },
        None,
    );
    Ok(())
}

/// Phase 1.3.2 — `Args.get() -> List<String>` on `--target wasip2`.
///
/// Allocates an 8-byte retptr area via `cabi_realloc(0, 0, 4, 8)`,
/// calls `wasi:cli/environment.get-arguments(retptr)` (the host
/// uses `cabi_realloc` again to allocate the list payload bytes
/// in guest memory), then hands the retptr to the shared
/// `__rt_canonical_decode_list_string` helper which walks
/// `(list_ptr, list_len)` + per-entry `(str_ptr, str_len)` into a
/// cons-built Aver `List<String>`. Five instructions at the call
/// site; the per-element copy lives in the helper.
pub(super) fn emit_args_get_wasip2(
    func: &mut wasm_encoder::Function,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Args.get on wasip2: lowering ctx missing".into())
    })?;
    let cabi_realloc = lowering.cabi_realloc_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Args.get on wasip2: cabi_realloc fn idx missing — wasip2_imports must register \
             at least one slot for cabi_realloc to be allocated"
                .into(),
        )
    })?;
    let get_arguments = lowering.get_arguments_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Args.get on wasip2: wasi:cli/environment.get-arguments fn idx missing".into(),
        )
    })?;
    let decoder = lowering.decode_list_string_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Args.get on wasip2: __rt_canonical_decode_list_string fn idx missing".into(),
        )
    })?;
    let retptr_local = slots.args_get_wasip2_retptr_scratch.ok_or_else(|| {
        WasmGcError::Validation(
            "Args.get on wasip2: i32 retptr scratch slot missing — \
             SlotTable should have allocated via fn_needs_args_get_scratch"
                .into(),
        )
    })?;

    // retptr = cabi_realloc(0, 0, 4, 8)  (8 bytes for the list_ptr/
    // list_len pair, 4-byte aligned).
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::Call(cabi_realloc));
    func.instruction(&Instruction::LocalSet(retptr_local));

    // Host call writes (list_ptr, list_len) + per-entry (str_ptr,
    // str_len) + utf-8 bytes via further cabi_realloc calls.
    func.instruction(&Instruction::LocalGet(retptr_local));
    func.instruction(&Instruction::Call(get_arguments));

    // Decoder pushes the materialised List<String> onto the stack.
    func.instruction(&Instruction::LocalGet(retptr_local));
    func.instruction(&Instruction::Call(decoder));
    Ok(())
}

/// Phase 1.3.3 — `Env.get(name: String) -> Option<String>` on
/// `--target wasip2`. Marshals the key via `__rt_string_to_lm`
/// (writes utf-8 bytes at LM[0..key_len], returns key_len),
/// allocates a fresh 8-byte retptr via `cabi_realloc` (lands at
/// >= page 2, disjoint from key bytes), calls
/// > `wasi:cli/environment.get-environment(retptr)` (the host fills
/// > the retptr area with `(list_ptr, list_len)` and uses
/// > `cabi_realloc` callbacks for per-entry `(key_ptr, key_len,
/// val_ptr, val_len)` blocks plus the utf-8 buffers), then hands
/// > `(retptr, key_ptr=0, key_len)` to `__rt_canonical_env_lookup`
/// > which linear-searches and returns `Option.Some(value)` on hit
/// > or `Option.None` on miss — the helper wraps the discriminant
/// > itself so call sites don't need to.
pub(super) fn emit_env_get_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx
        .wasip2_lowering
        .ok_or_else(|| WasmGcError::Validation("Env.get on wasip2: lowering ctx missing".into()))?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Env.get on `--target wasip2` expects 1 arg (the key String), got {}",
            args.len()
        )));
    }
    let str_to_lm = lowering.str_to_lm_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Env.get on wasip2: __rt_string_to_lm fn idx missing — bridge not allocated".into(),
        )
    })?;
    let cabi_realloc = lowering.cabi_realloc_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Env.get on wasip2: cabi_realloc fn idx missing".into())
    })?;
    let get_environment = lowering.get_environment_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Env.get on wasip2: wasi:cli/environment.get-environment fn idx missing".into(),
        )
    })?;
    let lookup = lowering.env_get_lookup_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Env.get on wasip2: __rt_canonical_env_lookup fn idx missing".into(),
        )
    })?;
    let scratch = slots.env_get_wasip2_scratch.ok_or_else(|| {
        WasmGcError::Validation(
            "Env.get on wasip2: [retptr, key_len] scratch pair missing — \
             SlotTable should have allocated via fn_needs_env_get_wasip2_scratch"
                .into(),
        )
    })?;
    let retptr_local = scratch[0];
    let key_len_local = scratch[1];

    // key bytes → LM[0..key_len], stash key_len.
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(str_to_lm));
    func.instruction(&Instruction::LocalSet(key_len_local));

    // retptr = cabi_realloc(0, 0, 4, 8).
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::Call(cabi_realloc));
    func.instruction(&Instruction::LocalSet(retptr_local));

    // Host call writes (list_ptr, list_len) at retptr + per-entry
    // tuples + utf-8 buffers (all via further cabi_realloc bumps).
    func.instruction(&Instruction::LocalGet(retptr_local));
    func.instruction(&Instruction::Call(get_environment));

    // __rt_canonical_env_lookup(retptr, key_ptr=0, key_len) →
    // matching value String (or empty on no-match).
    func.instruction(&Instruction::LocalGet(retptr_local));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalGet(key_len_local));
    func.instruction(&Instruction::Call(lookup));
    Ok(())
}

/// Phase 1.4 — `Time.unixMs() -> Int` on `--target wasip2`.
///
/// Lowers to `wasi:clocks/wall-clock.now: () -> datetime` (canonical
/// ABI: `(retptr: i32) -> ()`; host writes 16 bytes at retptr —
/// `seconds: u64` at +0, `nanoseconds: u32` at +8, 4 bytes pad).
/// Reads back the two fields, computes
/// `seconds * 1000 + nanoseconds / 1_000_000` as i64.
///
/// Retptr placement: `LM[0..16]`. Console.print's transport buffer
/// also writes at LM[0..len], but the two effects run sequentially
/// inside the guest — Console.print bytes are stale from the host's
/// perspective the moment that call returns, so reusing LM[0..16]
/// for the clocks retptr is sound. Memory section is unconditionally
/// emitted on wasip2 with imports, so the page-1 LM is available.
pub(super) fn emit_time_unix_ms_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Time.unixMs on wasip2: lowering ctx missing".into())
    })?;
    if !args.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "Time.unixMs on `--target wasip2` expects 0 args, got {}",
            args.len()
        )));
    }
    let now_fn = lowering.clocks_now_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Time.unixMs on wasip2: clocks-now fn idx missing".into())
    })?;
    // Call now(retptr=0). Host writes datetime to LM[0..16].
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::Call(now_fn));
    // unixMs = seconds * 1000 + (nanoseconds / 1_000_000)
    //        = i64.load LM[0]  * 1000
    //        + i64.extend_i32_u (i32.load LM[8]) / 1_000_000
    let mem_arg = wasm_encoder::MemArg {
        offset: 0,
        align: 3, // log2(8) — i64 alignment
        memory_index: 0,
    };
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I64Load(mem_arg));
    func.instruction(&Instruction::I64Const(1000));
    func.instruction(&Instruction::I64Mul);
    let ns_mem_arg = wasm_encoder::MemArg {
        offset: 8,
        align: 2, // log2(4) — i32 alignment
        memory_index: 0,
    };
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Load(ns_mem_arg));
    func.instruction(&Instruction::I64ExtendI32U);
    func.instruction(&Instruction::I64Const(1_000_000));
    func.instruction(&Instruction::I64DivU);
    func.instruction(&Instruction::I64Add);
    // `Int = ℤ`: `Time.unixMs() -> Int` — lift the i64 epoch ms to $aint.
    lift_i64_result_to_aint_wasip2(func, ctx)?;
    Ok(())
}

/// Phase 1.4b — `Time.now() -> String` on `--target wasip2`.
///
/// Calls `wasi:clocks/wall-clock.now(retptr=0)` (the host writes
/// `(seconds: u64, nanoseconds: u32)` into LM[0..16] — same retptr
/// shape Time.unixMs already uses), loads the two fields, and
/// hands them to `__rt_format_iso8601` which materialises a fresh
/// 24-byte `(array i8)` containing the RFC3339-like string. The
/// helper itself never reads LM, so the LM[0..16] window is free
/// to be reused immediately afterwards.
pub(super) fn emit_time_now_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Time.now on wasip2: lowering ctx missing".into())
    })?;
    if !args.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "Time.now on `--target wasip2` expects 0 args, got {}",
            args.len()
        )));
    }
    let now_fn = lowering.clocks_now_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Time.now on wasip2: clocks-now fn idx missing — slot not registered".into(),
        )
    })?;
    let fmt_fn = lowering.fmt_iso8601_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Time.now on wasip2: __rt_format_iso8601 fn idx missing — helper not allocated".into(),
        )
    })?;

    // now(retptr=0). Host writes 16 bytes at LM[0..16].
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::Call(now_fn));

    // Load secs (i64 @ LM[0]) and nanos (i32 @ LM[8]); push to
    // stack in `(secs, nanos)` order matching the helper's params.
    let secs_mem = wasm_encoder::MemArg {
        offset: 0,
        align: 3, // log2(8)
        memory_index: 0,
    };
    let nanos_mem = wasm_encoder::MemArg {
        offset: 8,
        align: 2, // log2(4)
        memory_index: 0,
    };
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I64Load(secs_mem));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Load(nanos_mem));

    // Helper returns the formatted ref string on the stack.
    func.instruction(&Instruction::Call(fmt_fn));
    Ok(())
}

/// Phase 1.3.4 — `Console.readLine() -> Result<String, String>` on
/// `--target wasip2`. The whole machinery — stdin handle caching,
/// 1-byte blocking-read loop, buffer growth, `\n` / `\r` handling,
/// `Result` construction — lives in the
/// `__rt_console_read_line` helper. The call site is one
/// instruction: `Call $__rt_console_read_line`.
pub(super) fn emit_console_read_line_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Console.readLine on wasip2: lowering ctx missing".into())
    })?;
    if !args.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "Console.readLine on `--target wasip2` expects 0 args, got {}",
            args.len()
        )));
    }
    let read_fn = lowering.console_read_line_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Console.readLine on wasip2: __rt_console_read_line fn idx missing — \
             helper not allocated"
                .into(),
        )
    })?;
    func.instruction(&Instruction::Call(read_fn));
    Ok(())
}

/// Phase 1.4c — `Time.sleep(ms: Int) -> Result<Unit, String>` on
/// `--target wasip2`.
///
/// Emits the milliseconds expression onto the stack and calls
/// `__rt_time_sleep`, which subscribes a pollable on the
/// monotonic clock, waits for it via `wasi:io/poll.poll`, and
/// drops the pollable. Argument-contract failures are constructed in
/// guest code, before WASI is touched. A statically valid literal is
/// unwrapped by the fail-closed HIR `__result_proven` discharge; this
/// lowering still executes the sleep and returns its contract carrier.
pub(super) fn emit_time_sleep_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Time.sleep on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Time.sleep on `--target wasip2` expects 1 arg (ms: Int), got {}",
            args.len()
        )));
    }
    let sleep_fn = lowering.time_sleep_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Time.sleep on wasip2: __rt_time_sleep fn idx missing — helper not allocated".into(),
        )
    })?;
    let result_idx = ctx
        .registry
        .result_type_idx("Result<Unit,String>")
        .ok_or_else(|| {
            WasmGcError::Validation("Time.sleep on wasip2: Result<Unit,String> slot missing".into())
        })?;
    let [ms_scratch, _] = slots.validated_effect_wasip2_i64_scratch.ok_or_else(|| {
        WasmGcError::Validation("Time.sleep on wasip2: validated-effect i64 scratch missing".into())
    })?;
    let result_block = result_block_type(result_idx);

    if ctx.registry.bignum {
        let [ms_ref, _] = slots.validated_effect_wasip2_aint_scratch.ok_or_else(|| {
            WasmGcError::Validation(
                "Time.sleep on wasip2: validated-effect AverInt scratch missing".into(),
            )
        })?;
        let aint_idx = ctx.registry.aint_struct_idx.ok_or_else(|| {
            WasmGcError::Validation("Time.sleep on wasip2: AverInt type slot missing".into())
        })?;
        emit_mir_expr(func, &args[0], slots, ctx)?;
        func.instruction(&Instruction::LocalSet(ms_ref));
        func.instruction(&Instruction::LocalGet(ms_ref));
        func.instruction(&Instruction::StructGet {
            struct_type_index: aint_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::RefIsNull);
        func.instruction(&Instruction::If(result_block));
        func.instruction(&Instruction::LocalGet(ms_ref));
        let to_i64 = ctx
            .fn_map
            .builtins
            .get("__aint_to_i64_checked")
            .copied()
            .ok_or_else(|| {
                WasmGcError::Validation(
                    "Time.sleep on wasip2: __aint_to_i64_checked missing".into(),
                )
            })?;
        func.instruction(&Instruction::Call(to_i64));
        func.instruction(&Instruction::LocalSet(ms_scratch));
        emit_validated_sleep(func, sleep_fn, ms_scratch, result_idx, ctx)?;
        func.instruction(&Instruction::Else);
        emit_result_err(
            func,
            result_idx,
            "Unit",
            "Time.sleep: ms must fit a 64-bit integer",
            ctx,
        )?;
        func.instruction(&Instruction::End);
    } else {
        emit_mir_expr(func, &args[0], slots, ctx)?;
        func.instruction(&Instruction::LocalSet(ms_scratch));
        emit_validated_sleep(func, sleep_fn, ms_scratch, result_idx, ctx)?;
    }
    Ok(())
}

fn emit_validated_sleep(
    func: &mut wasm_encoder::Function,
    sleep_fn: u32,
    ms_scratch: u32,
    result_idx: u32,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    func.instruction(&Instruction::LocalGet(ms_scratch));
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64GeS);
    func.instruction(&Instruction::If(result_block_type(result_idx)));
    func.instruction(&Instruction::LocalGet(ms_scratch));
    func.instruction(&Instruction::Call(sleep_fn));
    func.instruction(&Instruction::I32Const(1));
    emit_default_value(func, "Unit", ctx.registry)?;
    emit_default_value(func, "String", ctx.registry)?;
    func.instruction(&Instruction::StructNew(result_idx));
    func.instruction(&Instruction::Else);
    emit_result_err(
        func,
        result_idx,
        "Unit",
        "Time.sleep: ms must be non-negative",
        ctx,
    )?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// Phase 1.5.1 — `Disk.exists(path: String) -> Bool` on
/// `--target wasip2`. Emits the path expression onto the stack
/// and calls `__rt_disk_exists`, which lazy-fetches a preopen
/// descriptor, runs `stat-at`, and returns the boolean tag.
/// `false` on no-preopens / Err / wasi-error; `true` on Ok.
pub(super) fn emit_disk_exists_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.exists on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Disk.exists on `--target wasip2` expects 1 arg (path: String), got {}",
            args.len()
        )));
    }
    let exists_fn = lowering.disk_exists_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Disk.exists on wasip2: __rt_disk_exists fn idx missing — \
             helper not allocated"
                .into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?; // path: ref string
    func.instruction(&Instruction::Call(exists_fn));
    Ok(())
}

/// Phase 4.5b (0.20) — `Tcp.ping(host, port) -> Result<Unit, String>`
/// on `--target wasip2`. Pushes (host, port) and calls the
/// light `__rt_tcp_ping` wrapper.
pub(super) fn emit_tcp_ping_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.ping on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.ping on `--target wasip2` expects 2 args (host, port), got {}",
            args.len()
        )));
    }
    let ping_fn = lowering.tcp_ping_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Tcp.ping on wasip2: __rt_tcp_ping fn idx missing".into())
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    // `Int = ℤ`: the `port` is the `$AverInt` carrier — saturate-lower to i64.
    emit_aint_arg_as_i64_wasip2(func, &args[1], slots, ctx)?;
    func.instruction(&Instruction::Call(ping_fn));
    Ok(())
}

/// Phase 4.5a (0.20) — `Tcp.send(host, port, data) ->
/// Result<String, String>` on `--target wasip2`. One-shot
/// pipeline: connect + writeLine + readLine + close. Pushes
/// (host, port, data) and calls the `__rt_tcp_send` orchestrator
/// helper.
pub(super) fn emit_tcp_send_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.send on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 3 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.send on `--target wasip2` expects 3 args (host, port, data), got {}",
            args.len()
        )));
    }
    let send_fn = lowering.tcp_send_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Tcp.send on wasip2: __rt_tcp_send fn idx missing".into())
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    // `Int = ℤ`: the `port` is the `$AverInt` carrier — saturate-lower to i64.
    emit_aint_arg_as_i64_wasip2(func, &args[1], slots, ctx)?;
    emit_mir_expr(func, &args[2], slots, ctx)?;
    func.instruction(&Instruction::Call(send_fn));
    Ok(())
}

pub(super) fn emit_tcp_send_bytes_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.sendBytes on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 3 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.sendBytes on `--target wasip2` expects 3 args (host, port, data), got {}",
            args.len()
        )));
    }
    let send_fn = lowering.tcp_send_bytes_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Tcp.sendBytes on wasip2: __rt_tcp_send_bytes fn idx missing".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    emit_aint_arg_as_i64_wasip2(func, &args[1], slots, ctx)?;
    emit_mir_expr(func, &args[2], slots, ctx)?;
    func.instruction(&Instruction::Call(send_fn));
    Ok(())
}

/// Phase 4.4b (0.20) — `Tcp.readLine(conn) -> Result<String, String>`
/// on `--target wasip2`. Pushes conn and calls the helper.
pub(super) fn emit_tcp_read_line_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.readLine on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.readLine on `--target wasip2` expects 1 arg (conn), got {}",
            args.len()
        )));
    }
    let read_line_fn = lowering.tcp_read_line_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Tcp.readLine on wasip2: __rt_tcp_read_line fn idx missing".into())
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(read_line_fn));
    Ok(())
}

pub(super) fn emit_tcp_read_bytes_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.readBytes on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.readBytes on `--target wasip2` expects 2 args (conn, count), got {}",
            args.len()
        )));
    }
    let read_bytes_fn = lowering.tcp_read_bytes_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Tcp.readBytes on wasip2: __rt_tcp_read_bytes fn idx missing".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    // Keep the arbitrary-precision carrier intact. The helper applies the
    // 10 MiB bound and returns Result.Err for negative or huge counts.
    emit_mir_expr(func, &args[1], slots, ctx)?;
    func.instruction(&Instruction::Call(read_bytes_fn));
    Ok(())
}

pub(super) fn emit_tcp_read_some_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.readSome on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.readSome on `--target wasip2` expects 2 args (conn, maxBytes), got {}",
            args.len()
        )));
    }
    let helper = lowering.tcp_read_some_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Tcp.readSome on wasip2: helper fn idx missing".into())
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    emit_mir_expr(func, &args[1], slots, ctx)?;
    func.instruction(&Instruction::Call(helper));
    Ok(())
}

pub(super) fn emit_tcp_poll_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.poll on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.poll on `--target wasip2` expects 2 args (connections, timeoutMs), got {}",
            args.len()
        )));
    }
    let helper = lowering.tcp_poll_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Tcp.poll on wasip2: helper fn idx missing".into())
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    emit_mir_expr(func, &args[1], slots, ctx)?;
    func.instruction(&Instruction::Call(helper));
    Ok(())
}

/// Phase 4.4a (0.20) — `Tcp.writeLine(conn, line) -> Result<Unit, String>`
/// on `--target wasip2`. Pushes (conn, line) and calls the
/// `__rt_tcp_write_line` helper.
pub(super) fn emit_tcp_write_line_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.writeLine on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.writeLine on `--target wasip2` expects 2 args (conn, line), got {}",
            args.len()
        )));
    }
    let write_line_fn = lowering.tcp_write_line_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Tcp.writeLine on wasip2: __rt_tcp_write_line fn idx missing".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    emit_mir_expr(func, &args[1], slots, ctx)?;
    func.instruction(&Instruction::Call(write_line_fn));
    Ok(())
}

/// `Tcp.writeBytes(conn, payload) -> Result<Unit, String>` on native WASI 0.2.
pub(super) fn emit_tcp_write_bytes_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.writeBytes on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.writeBytes on `--target wasip2` expects 2 args (conn, payload), got {}",
            args.len()
        )));
    }
    let write_bytes_fn = lowering.tcp_write_bytes_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Tcp.writeBytes on wasip2: __rt_tcp_write_bytes fn idx missing".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    emit_mir_expr(func, &args[1], slots, ctx)?;
    func.instruction(&Instruction::Call(write_bytes_fn));
    Ok(())
}

/// Phase 4.3 (0.20) — `Tcp.close(conn: Tcp.Connection) ->
/// Result<Unit, String>` on `--target wasip2`. Pushes the conn
/// ref onto the stack and calls the `__rt_tcp_close` helper.
pub(super) fn emit_tcp_close_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.close on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.close on `--target wasip2` expects 1 arg (conn: Tcp.Connection), got {}",
            args.len()
        )));
    }
    let close_fn = lowering.tcp_close_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Tcp.close on wasip2: __rt_tcp_close fn idx missing — helper not allocated".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(close_fn));
    Ok(())
}

/// Phase 4.2.1 (0.20) — `Tcp.connect(host: String, port: Int) ->
/// Result<Tcp.Connection, String>` on `--target wasip2`. Pushes
/// both args onto the stack and calls the `__rt_tcp_connect`
/// helper, whose stub body currently returns Result.Err. Real
/// DNS / socket / connect pipeline replaces the helper body in
/// Phase 4.2.2+ — this dispatcher stays put.
pub(super) fn emit_tcp_connect_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Tcp.connect on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Tcp.connect on `--target wasip2` expects 2 args (host: String, port: Int), got {}",
            args.len()
        )));
    }
    let connect_fn = lowering.tcp_connect_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Tcp.connect on wasip2: __rt_tcp_connect fn idx missing — \
             helper not allocated (check Result<Tcp.Connection,String> + \
             String slot + stub-error literal registration)"
                .into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?; // host: ref string
    // `Int = ℤ`: the `port` is the `$AverInt` carrier — saturate-lower to i64.
    emit_aint_arg_as_i64_wasip2(func, &args[1], slots, ctx)?; // port: i64
    func.instruction(&Instruction::Call(connect_fn));
    Ok(())
}

pub(super) fn emit_disk_read_text_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.readText on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Disk.readText on `--target wasip2` expects 1 arg (path: String), got {}",
            args.len()
        )));
    }
    let read_fn = lowering.disk_read_text_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Disk.readText on wasip2: __rt_disk_read_text fn idx missing — \
             helper not allocated"
                .into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?; // path: ref string
    func.instruction(&Instruction::Call(read_fn));
    Ok(())
}

pub(super) fn emit_disk_read_bytes_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.readBytes on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Disk.readBytes on `--target wasip2` expects 1 arg (path: String), got {}",
            args.len()
        )));
    }
    let read_fn = lowering.disk_read_bytes_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Disk.readBytes on wasip2: __rt_disk_read_bytes fn idx missing".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(read_fn));
    Ok(())
}

pub(super) fn emit_disk_read_bytes_at_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.readBytesAt on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 3 {
        return Err(WasmGcError::Validation(format!(
            "Disk.readBytesAt on `--target wasip2` expects 3 args (path, offset, length), got {}",
            args.len()
        )));
    }
    let read_fn = lowering.disk_read_bytes_at_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Disk.readBytesAt on wasip2: __rt_disk_read_bytes_at fn idx missing".into(),
        )
    })?;
    for arg in args {
        emit_mir_expr(func, arg, slots, ctx)?;
    }
    func.instruction(&Instruction::Call(read_fn));
    Ok(())
}

pub(super) fn emit_disk_size_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.size on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Disk.size on `--target wasip2` expects 1 path arg, got {}",
            args.len()
        )));
    }
    let size_fn = lowering.disk_size_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Disk.size on wasip2: __rt_disk_size fn idx missing".into())
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(size_fn));
    Ok(())
}

fn emit_disk_write_bytes_like_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    effect: &'static str,
    fn_idx: Option<u32>,
) -> Result<(), WasmGcError> {
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "{effect} on `--target wasip2` expects 2 args (path, content), got {}",
            args.len()
        )));
    }
    let helper = fn_idx.ok_or_else(|| {
        WasmGcError::Validation(format!(
            "{effect} on wasip2: binary Disk helper fn idx missing"
        ))
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    emit_mir_expr(func, &args[1], slots, ctx)?;
    func.instruction(&Instruction::Call(helper));
    Ok(())
}

pub(super) fn emit_disk_write_bytes_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.writeBytes on wasip2: lowering ctx missing".into())
    })?;
    emit_disk_write_bytes_like_wasip2(
        func,
        args,
        slots,
        ctx,
        "Disk.writeBytes",
        lowering.disk_write_bytes_fn_idx,
    )
}

pub(super) fn emit_disk_append_bytes_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.appendBytes on wasip2: lowering ctx missing".into())
    })?;
    emit_disk_write_bytes_like_wasip2(
        func,
        args,
        slots,
        ctx,
        "Disk.appendBytes",
        lowering.disk_append_bytes_fn_idx,
    )
}

/// Phase 1.4 — `Random.int(min: Int, max: Int) -> Result<Int, String>` on
/// `--target wasip2`.
///
/// Lowers to `wasi:random/random.get-random-u64: () -> u64` plus
/// guest-side validation, modulo by `(max - min + 1)`, and offset by `min`. The
/// modulo is the standard slightly-biased pattern (acceptable for
/// non-cryptographic use, matches the wasm-gc target's existing
/// shape via `aver/random_int`).
pub(super) fn emit_random_int_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Random.int on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Random.int on `--target wasip2` expects 2 args (min, max), got {}",
            args.len()
        )));
    }
    let rand_fn = lowering.random_u64_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Random.int on wasip2: random get-random-u64 fn idx missing".into())
    })?;
    let [min_scratch, max_scratch] =
        slots.validated_effect_wasip2_i64_scratch.ok_or_else(|| {
            WasmGcError::Validation(
                "Random.int on wasip2: validated-effect i64 scratch pair missing".into(),
            )
        })?;
    let result_idx = ctx
        .registry
        .result_type_idx("Result<Int,String>")
        .ok_or_else(|| {
            WasmGcError::Validation("Random.int on wasip2: Result<Int,String> slot missing".into())
        })?;

    // Evaluate both arguments before invoking randomness. This is observable
    // when either bound contains an effect, and matches Aver's eager,
    // left-to-right call semantics.
    if ctx.registry.bignum {
        let [min_ref, max_ref] = slots.validated_effect_wasip2_aint_scratch.ok_or_else(|| {
            WasmGcError::Validation(
                "Random.int on wasip2: validated-effect AverInt scratch pair missing".into(),
            )
        })?;
        let aint_idx = ctx.registry.aint_struct_idx.ok_or_else(|| {
            WasmGcError::Validation("Random.int on wasip2: AverInt type slot missing".into())
        })?;
        emit_mir_expr(func, &args[0], slots, ctx)?;
        emit_mir_expr(func, &args[1], slots, ctx)?;
        func.instruction(&Instruction::LocalSet(max_ref));
        func.instruction(&Instruction::LocalSet(min_ref));
        func.instruction(&Instruction::LocalGet(min_ref));
        func.instruction(&Instruction::StructGet {
            struct_type_index: aint_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::RefIsNull);
        func.instruction(&Instruction::LocalGet(max_ref));
        func.instruction(&Instruction::StructGet {
            struct_type_index: aint_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::RefIsNull);
        func.instruction(&Instruction::I32And);
        func.instruction(&Instruction::If(result_block_type(result_idx)));
        let to_i64 = ctx
            .fn_map
            .builtins
            .get("__aint_to_i64_checked")
            .copied()
            .ok_or_else(|| {
                WasmGcError::Validation(
                    "Random.int on wasip2: __aint_to_i64_checked missing".into(),
                )
            })?;
        func.instruction(&Instruction::LocalGet(min_ref));
        func.instruction(&Instruction::Call(to_i64));
        func.instruction(&Instruction::LocalSet(min_scratch));
        func.instruction(&Instruction::LocalGet(max_ref));
        func.instruction(&Instruction::Call(to_i64));
        func.instruction(&Instruction::LocalSet(max_scratch));
        emit_validated_random_int(func, rand_fn, min_scratch, max_scratch, result_idx, ctx)?;
        func.instruction(&Instruction::Else);
        emit_result_err(
            func,
            result_idx,
            "Int",
            "Random.int: bounds must fit a 64-bit integer",
            ctx,
        )?;
        func.instruction(&Instruction::End);
    } else {
        emit_mir_expr(func, &args[0], slots, ctx)?;
        emit_mir_expr(func, &args[1], slots, ctx)?;
        func.instruction(&Instruction::LocalSet(max_scratch));
        func.instruction(&Instruction::LocalSet(min_scratch));
        emit_validated_random_int(func, rand_fn, min_scratch, max_scratch, result_idx, ctx)?;
    }
    Ok(())
}

fn emit_validated_random_int(
    func: &mut wasm_encoder::Function,
    rand_fn: u32,
    min_scratch: u32,
    max_scratch: u32,
    result_idx: u32,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    func.instruction(&Instruction::LocalGet(min_scratch));
    func.instruction(&Instruction::LocalGet(max_scratch));
    func.instruction(&Instruction::I64LeS);
    func.instruction(&Instruction::If(result_block_type(result_idx)));
    func.instruction(&Instruction::I32Const(1));

    // A wrapped width of zero denotes the complete 2^64-sized i64 domain.
    // In that one case every random bit pattern is already a valid offset;
    // avoid `rem_u 0` while preserving a uniform rotation by `min`.
    func.instruction(&Instruction::LocalGet(max_scratch));
    func.instruction(&Instruction::LocalGet(min_scratch));
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::If(BlockType::Result(ValType::I64)));
    func.instruction(&Instruction::Call(rand_fn));
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::Call(rand_fn));
    func.instruction(&Instruction::LocalGet(max_scratch));
    func.instruction(&Instruction::LocalGet(min_scratch));
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64RemU);
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::LocalGet(min_scratch));
    func.instruction(&Instruction::I64Add);
    lift_i64_result_to_aint_wasip2(func, ctx)?;
    emit_default_value(func, "String", ctx.registry)?;
    func.instruction(&Instruction::StructNew(result_idx));
    func.instruction(&Instruction::Else);
    emit_result_err(
        func,
        result_idx,
        "Int",
        "Random.int: min must be <= max",
        ctx,
    )?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// Phase 1.4 — `Random.float() -> Float` on `--target wasip2`.
///
/// Lowers to `wasi:random/random.get-random-u64: () -> u64` plus
/// the standard 53-bit-precision scale to `[0.0, 1.0)`:
///   `(u64 >> 11) * 2^-53`.
/// Matches the convention used by JS `Math.random` and Rust's
/// `rand::Rng::gen::<f64>()` — both produce 53 random mantissa
/// bits with no exponent bits set.
pub(super) fn emit_random_float_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Random.float on wasip2: lowering ctx missing".into())
    })?;
    if !args.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "Random.float on `--target wasip2` expects 0 args, got {}",
            args.len()
        )));
    }
    let rand_fn = lowering.random_u64_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Random.float on wasip2: random get-random-u64 fn idx missing".into(),
        )
    })?;
    func.instruction(&Instruction::Call(rand_fn));
    func.instruction(&Instruction::I64Const(11));
    func.instruction(&Instruction::I64ShrU);
    func.instruction(&Instruction::F64ConvertI64U);
    // 2^-53 = 1.0 / (1 << 53). Computed as a literal const.
    func.instruction(&Instruction::F64Const(
        (1.0_f64 / (1u64 << 53) as f64).into(),
    ));
    func.instruction(&Instruction::F64Mul);
    Ok(())
}

/// Phase 1.5.3 — `Disk.writeText(path: String, content: String)
/// -> Result<Unit, String>` on `--target wasip2`. Pushes both
/// args onto the stack and calls `__rt_disk_write_text`, which
/// owns the open-at(create+truncate) + write-via-stream +
/// blocking-write-and-flush + per-call resource drops.
pub(super) fn emit_disk_write_text_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.writeText on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Disk.writeText on `--target wasip2` expects 2 args (path, content), got {}",
            args.len()
        )));
    }
    let write_fn = lowering.disk_write_text_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Disk.writeText on wasip2: __rt_disk_write_text fn idx missing — \
             helper not allocated"
                .into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?; // path
    emit_mir_expr(func, &args[1], slots, ctx)?; // content
    func.instruction(&Instruction::Call(write_fn));
    Ok(())
}

/// Phase 1.5.4 — `Disk.delete(path) -> Result<Unit, String>` on
/// `--target wasip2`. Pushes the path onto the stack and calls
/// `__rt_disk_delete` (single wasi `unlink-file-at` underneath).
pub(super) fn emit_disk_delete_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.delete on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Disk.delete on `--target wasip2` expects 1 arg (path), got {}",
            args.len()
        )));
    }
    let fn_idx = lowering.disk_delete_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Disk.delete on wasip2: __rt_disk_delete fn idx missing — helper not allocated".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// Phase 1.5.4 — `Disk.deleteDir(path) -> Result<Unit, String>`.
pub(super) fn emit_disk_delete_dir_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.deleteDir on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Disk.deleteDir on `--target wasip2` expects 1 arg (path), got {}",
            args.len()
        )));
    }
    let fn_idx = lowering.disk_delete_dir_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Disk.deleteDir on wasip2: __rt_disk_delete_dir fn idx missing".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// Phase 1.5.4 — `Disk.makeDir(path) -> Result<Unit, String>`.
pub(super) fn emit_disk_make_dir_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.makeDir on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Disk.makeDir on `--target wasip2` expects 1 arg (path), got {}",
            args.len()
        )));
    }
    let fn_idx = lowering.disk_make_dir_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Disk.makeDir on wasip2: __rt_disk_make_dir fn idx missing".into())
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// Phase 1.5.5 — `Disk.appendText(path, content) ->
/// Result<Unit, String>` on `--target wasip2`. Pushes both args
/// and calls `__rt_disk_append_text`, which uses the same body
/// emitter as `__rt_disk_write_text` flipped to append mode.
pub(super) fn emit_disk_append_text_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.appendText on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Disk.appendText on `--target wasip2` expects 2 args (path, content), got {}",
            args.len()
        )));
    }
    let fn_idx = lowering.disk_append_text_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Disk.appendText on wasip2: __rt_disk_append_text fn idx missing".into(),
        )
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    emit_mir_expr(func, &args[1], slots, ctx)?;
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// Phase 1.5.6 — `Disk.listDir(path) -> Result<List<String>, String>`
/// on `--target wasip2`. Pushes the path arg and calls
/// `__rt_disk_list_dir`, which owns the open-at(directory) +
/// read-directory + entry-iteration loop + drops.
pub(super) fn emit_disk_list_dir_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Disk.listDir on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Disk.listDir on `--target wasip2` expects 1 arg (path), got {}",
            args.len()
        )));
    }
    let fn_idx = lowering.disk_list_dir_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Disk.listDir on wasip2: __rt_disk_list_dir fn idx missing".into())
    })?;
    emit_mir_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// Phase 2 — `Http.*(url[, content_type, body, headers]) ->
/// Result<HttpResponse, String>` on `--target wasip2`. The shared
/// `__rt_http_request` helper takes 5 params: method_tag i32,
/// url ref string, content_type ref string, body ref string,
/// headers ref map. Per-method dispatchers push the appropriate
/// method ordinal from wasi:http's `method` variant (0=GET,
/// 1=HEAD, 2=POST, 3=PUT, 4=DELETE, 8=PATCH).
///
/// For body-less methods (GET/HEAD/DELETE) the dispatcher
/// synthesises empty content_type / body / headers — the helper
/// gates body marshalling on `method >= 2 && method != 4` and
/// the headers-iter loop is a cap-bounded no-op on an empty map.
fn emit_http_simple_method_wasip2(
    method_name: &str,
    method_tag: i32,
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation(format!("{method_name} on wasip2: lowering ctx missing"))
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "{method_name} on `--target wasip2` expects 1 arg (url), got {}",
            args.len()
        )));
    }
    let fn_idx = lowering.http_get_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(format!(
            "{method_name} on wasip2: __rt_http_request fn idx missing"
        ))
    })?;
    let registry = ctx.registry;
    let string_idx = registry.string_array_type_idx.ok_or_else(|| {
        WasmGcError::Validation(format!("{method_name} on wasip2: string type idx missing"))
    })?;
    let map_slots = registry
        .map_slots("Map<String,List<String>>")
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "{method_name} on wasip2: Map<String, List<String>> slots missing"
            ))
        })?;

    func.instruction(&Instruction::I32Const(method_tag));
    emit_mir_expr(func, &args[0], slots, ctx)?;
    // Empty content_type
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::ArrayNewDefault(string_idx));
    // Empty body
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::ArrayNewDefault(string_idx));
    // Empty headers map (size=0, cap=INITIAL_CAP, default arrays) —
    // the same shape `emit_map_empty` builds, sharing its constant so
    // the two cannot drift. The map grows on its own once the header
    // accumulation starts filling it.
    use crate::codegen::wasm_gc::maps::INITIAL_CAP;
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Const(INITIAL_CAP));
    func.instruction(&Instruction::I32Const(INITIAL_CAP));
    func.instruction(&Instruction::ArrayNewDefault(map_slots.keys_array));
    func.instruction(&Instruction::I32Const(INITIAL_CAP));
    func.instruction(&Instruction::ArrayNewDefault(map_slots.values_array));
    func.instruction(&Instruction::StructNew(map_slots.map));
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// Body-bearing dispatch shared by POST/PUT/PATCH. Aver source
/// signature: `(url: String, content_type: String, body: String,
/// headers: Map<String, List<String>>) -> Result<HttpResponse,
/// String>`.
fn emit_http_body_method_wasip2(
    method_name: &str,
    method_tag: i32,
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation(format!("{method_name} on wasip2: lowering ctx missing"))
    })?;
    if args.len() != 4 {
        return Err(WasmGcError::Validation(format!(
            "{method_name} on `--target wasip2` expects 4 args (url, content_type, body, headers), got {}",
            args.len()
        )));
    }
    let fn_idx = lowering.http_get_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(format!(
            "{method_name} on wasip2: __rt_http_request fn idx missing"
        ))
    })?;
    func.instruction(&Instruction::I32Const(method_tag));
    emit_mir_expr(func, &args[0], slots, ctx)?; // url
    emit_mir_expr(func, &args[1], slots, ctx)?; // content_type
    emit_mir_expr(func, &args[2], slots, ctx)?; // body
    emit_mir_expr(func, &args[3], slots, ctx)?; // headers
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

pub(super) fn emit_http_get_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    emit_http_simple_method_wasip2("Http.get", 0, func, args, slots, ctx)
}

pub(super) fn emit_http_head_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    emit_http_simple_method_wasip2("Http.head", 1, func, args, slots, ctx)
}

pub(super) fn emit_http_delete_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    emit_http_simple_method_wasip2("Http.delete", 4, func, args, slots, ctx)
}

pub(super) fn emit_http_post_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    emit_http_body_method_wasip2("Http.post", 2, func, args, slots, ctx)
}

pub(super) fn emit_http_put_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    emit_http_body_method_wasip2("Http.put", 3, func, args, slots, ctx)
}

pub(super) fn emit_http_patch_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    emit_http_body_method_wasip2("Http.patch", 8, func, args, slots, ctx)
}
