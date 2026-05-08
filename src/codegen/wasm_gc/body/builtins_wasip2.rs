//! Wasip2-specific call-site emit functions for `Type.method(args)`
//! builtins. Each function here is the wasip2 counterpart of an
//! aver-bridge handler in `builtins.rs`'s main switch — `Console.*`,
//! `Args.get`, `Env.get`, `Time.*`, `Random.*`, `Disk.*` — and is
//! dispatched to from `emit_dotted_builtin` when the surrounding
//! emit ctx is in `TargetMode::Wasip2`.
//!
//! The actual canonical-ABI helper bodies live one layer deeper in
//! `wasip2_helpers.rs` (sibling of `module.rs`); the functions here
//! are call-site marshalling — push args, `Call(helper_fn_idx)`.

use wasm_encoder::Instruction;

use crate::ast::{Expr, Spanned};

use super::super::WasmGcError;
use super::emit::emit_expr;
use super::{EmitCtx, SlotTable};

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
    args: &[Spanned<Expr>],
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
    let len_local = slots.console_print_wasip2_scratch.ok_or_else(|| {
        WasmGcError::Validation(
            "Console.* on wasip2: i32 scratch slot was not allocated by SlotTable — \
             `fn_needs_console_print_wasip2_scratch` did not flag this fn"
                .into(),
        )
    })?;

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
                WasmGcError::Validation(
                    "Console.print on wasip2: get_stdout fn idx missing".into(),
                )
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

    // Step 2: marshal s → LM[0..len], stash len.
    let str_to_lm = lowering.str_to_lm_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Console.* on wasip2: __rt_string_to_lm fn idx missing — bridge not allocated"
                .into(),
        )
    })?;
    emit_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(str_to_lm));
    func.instruction(&Instruction::LocalSet(len_local));

    // Step 3: defensive memory.grow(1) so retptr+12 stays in-bounds
    // even when len landed exactly on a page boundary.
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::MemoryGrow(0));
    func.instruction(&Instruction::Drop);

    // Step 4: blocking-write-and-flush(handle, ptr=0, len, retptr).
    func.instruction(&Instruction::GlobalGet(handle_global));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalGet(len_local));
    // retptr = (len + 15) & -16 (16-byte aligned, just past the
    // string bytes). Computed inline to avoid a second scratch slot.
    func.instruction(&Instruction::LocalGet(len_local));
    func.instruction(&Instruction::I32Const(15));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-16));
    func.instruction(&Instruction::I32And);
    let write_fn = lowering.blocking_write_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Console.* on wasip2: blocking-write-and-flush fn idx missing".into(),
        )
    })?;
    func.instruction(&Instruction::Call(write_fn));
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
/// `wasi:cli/environment.get-environment(retptr)` (the host fills
/// the retptr area with `(list_ptr, list_len)` and uses
/// `cabi_realloc` callbacks for per-entry `(key_ptr, key_len,
/// val_ptr, val_len)` blocks plus the utf-8 buffers), then hands
/// `(retptr, key_ptr=0, key_len)` to `__rt_canonical_env_lookup`
/// which linear-searches and returns `Option.Some(value)` on hit
/// or `Option.None` on miss — the helper wraps the discriminant
/// itself so call sites don't need to.
pub(super) fn emit_env_get_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let lowering = ctx.wasip2_lowering.ok_or_else(|| {
        WasmGcError::Validation("Env.get on wasip2: lowering ctx missing".into())
    })?;
    if args.len() != 1 {
        return Err(WasmGcError::Validation(format!(
            "Env.get on `--target wasip2` expects 1 arg (the key String), got {}",
            args.len()
        )));
    }
    let str_to_lm = lowering.str_to_lm_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Env.get on wasip2: __rt_string_to_lm fn idx missing — bridge not allocated"
                .into(),
        )
    })?;
    let cabi_realloc = lowering.cabi_realloc_fn_idx.ok_or_else(|| {
        WasmGcError::Validation(
            "Env.get on wasip2: cabi_realloc fn idx missing".into(),
        )
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
    emit_expr(func, &args[0], slots, ctx)?;
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
    args: &[Spanned<Expr>],
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
    args: &[Spanned<Expr>],
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
            "Time.now on wasip2: __rt_format_iso8601 fn idx missing — helper not allocated"
                .into(),
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
    args: &[Spanned<Expr>],
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

/// Phase 1.4c — `Time.sleep(ms: Int) -> Unit` on `--target wasip2`.
///
/// Emits the milliseconds expression onto the stack and calls
/// `__rt_time_sleep`, which subscribes a pollable on the
/// monotonic clock, waits for it via `wasi:io/poll.poll`, and
/// drops the pollable. Source-level Aver still sees the same
/// `Time.sleep(ms)` it sees on the VM target — pollables are
/// implementation detail.
pub(super) fn emit_time_sleep_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<Expr>],
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
            "Time.sleep on wasip2: __rt_time_sleep fn idx missing — helper not allocated"
                .into(),
        )
    })?;
    emit_expr(func, &args[0], slots, ctx)?; // ms: i64
    func.instruction(&Instruction::Call(sleep_fn));
    Ok(())
}

/// Phase 1.5.1 — `Disk.exists(path: String) -> Bool` on
/// `--target wasip2`. Emits the path expression onto the stack
/// and calls `__rt_disk_exists`, which lazy-fetches a preopen
/// descriptor, runs `stat-at`, and returns the boolean tag.
/// `false` on no-preopens / Err / wasi-error; `true` on Ok.
pub(super) fn emit_disk_exists_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<Expr>],
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
    emit_expr(func, &args[0], slots, ctx)?; // path: ref string
    func.instruction(&Instruction::Call(exists_fn));
    Ok(())
}

/// Phase 1.5.2 — `Disk.readText(path: String) ->
/// Result<String, String>` on `--target wasip2`. Pushes the path
/// expression onto the stack and calls `__rt_disk_read_text`,
/// which owns the open-at + read-via-stream + blocking-read loop
/// + per-call resource drops.
pub(super) fn emit_disk_read_text_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<Expr>],
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
    emit_expr(func, &args[0], slots, ctx)?; // path: ref string
    func.instruction(&Instruction::Call(read_fn));
    Ok(())
}

/// Phase 1.4 — `Random.int(min: Int, max: Int) -> Int` on
/// `--target wasip2`.
///
/// Lowers to `wasi:random/random.get-random-u64: () -> u64` plus
/// guest-side modulo by `(max - min + 1)` and offset by `min`. The
/// modulo is the standard slightly-biased pattern (acceptable for
/// non-cryptographic use, matches the wasm-gc target's existing
/// shape via `aver/random_int`).
pub(super) fn emit_random_int_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<Expr>],
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
        WasmGcError::Validation(
            "Random.int on wasip2: random get-random-u64 fn idx missing".into(),
        )
    })?;
    // result = min + ((u64 % (max - min + 1)) as i64).
    // Stack discipline:
    //   push min
    //   push (get-random-u64() % (max - min + 1))
    //   i64.add
    emit_expr(func, &args[0], slots, ctx)?; // min: i64
    func.instruction(&Instruction::Call(rand_fn)); // u64 -> i64 representation
    emit_expr(func, &args[1], slots, ctx)?; // max
    emit_expr(func, &args[0], slots, ctx)?; // min (re-eval for max - min)
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64RemU); // modulo unsigned
    func.instruction(&Instruction::I64Add);
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
    args: &[Spanned<Expr>],
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
    args: &[Spanned<Expr>],
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
    emit_expr(func, &args[0], slots, ctx)?; // path
    emit_expr(func, &args[1], slots, ctx)?; // content
    func.instruction(&Instruction::Call(write_fn));
    Ok(())
}

/// Phase 1.5.4 — `Disk.delete(path) -> Result<Unit, String>` on
/// `--target wasip2`. Pushes the path onto the stack and calls
/// `__rt_disk_delete` (single wasi `unlink-file-at` underneath).
pub(super) fn emit_disk_delete_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<Expr>],
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
            "Disk.delete on wasip2: __rt_disk_delete fn idx missing — helper not allocated"
                .into(),
        )
    })?;
    emit_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// Phase 1.5.4 — `Disk.deleteDir(path) -> Result<Unit, String>`.
pub(super) fn emit_disk_delete_dir_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<Expr>],
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
    emit_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// Phase 1.5.4 — `Disk.makeDir(path) -> Result<Unit, String>`.
pub(super) fn emit_disk_make_dir_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<Expr>],
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
        WasmGcError::Validation(
            "Disk.makeDir on wasip2: __rt_disk_make_dir fn idx missing".into(),
        )
    })?;
    emit_expr(func, &args[0], slots, ctx)?;
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}

/// Phase 1.5.5 — `Disk.appendText(path, content) ->
/// Result<Unit, String>` on `--target wasip2`. Pushes both args
/// and calls `__rt_disk_append_text`, which uses the same body
/// emitter as `__rt_disk_write_text` flipped to append mode.
pub(super) fn emit_disk_append_text_wasip2(
    func: &mut wasm_encoder::Function,
    args: &[Spanned<Expr>],
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
    emit_expr(func, &args[0], slots, ctx)?;
    emit_expr(func, &args[1], slots, ctx)?;
    func.instruction(&Instruction::Call(fn_idx));
    Ok(())
}
