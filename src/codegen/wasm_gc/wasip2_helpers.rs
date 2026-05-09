//! Wasm-encoder helper bodies for the `--target wasip2` codegen path.
//!
//! This module hosts the per-helper `emit_*` functions and their
//! `*Indices` companion structs that `module.rs`'s orchestrator
//! threads through type/funcs/codes section emission. Each helper
//! corresponds to one Aver effect (or one shared canonical-ABI
//! shape) lowered on `--target wasip2`:
//!
//! - `cabi_realloc` — Phase 1.3.1 bump allocator backing every
//!   list-returning effect.
//! - `__rt_canonical_decode_list_string` — Phase 1.3.2 helper
//!   shared by `Args.get` (today) and any future
//!   `list<string>`-returning effect.
//! - `__rt_canonical_env_lookup` — Phase 1.3.3 `Env.get`
//!   linear-search over `list<tuple<string, string>>`.
//! - `__rt_format_iso8601` — Phase 1.4b `Time.now` digit
//!   formatter on top of `wasi:clocks/wall-clock.now`.
//! - `__rt_console_read_line` — Phase 1.3.4 stdin line-reader.
//! - `__rt_time_sleep` — Phase 1.4c `subscribe-duration` +
//!   `poll` + `[resource-drop]pollable`.
//! - `__rt_disk_exists` — Phase 1.5.1 preopens cache + stat-at.
//! - `__rt_disk_read_text` — Phase 1.5.2 open-at + stream loop +
//!   per-call resource drops.
//!
//! Allocation order in `module.rs` MUST match the `funcs.function`
//! / `codes.function` append order — the fn idx assigned at
//! allocation time is what call sites later baked into
//! `Wasip2Lowering`. Getting the two out of sync routes
//! `Call(idx)` to the wrong helper body (caught here on
//! 2026-05-08 when Phase 1.4c surfaced a latent 1.4b
//! misalignment — see commit f7044e8c).

use wasm_encoder::ValType;

/// Phase 1.3.1 — `cabi_realloc` export indices. Allocated when
/// the wasip2 path is active so the Component Model canonical-ABI
/// realloc contract has a real impl. Bump-allocator backing global
/// lives in `Wasip2Globals::bump_alloc_ptr`.
pub(super) struct CabiReallocIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
}

/// Phase 1.3.2 — `__rt_canonical_decode_list_string(retptr) ->
/// List<String>` helper indices. Walks a canonical-ABI lowered
/// `list<string>` (`(list_ptr i32, list_len i32)` at retptr +
/// `(str_ptr i32, str_len i32)` per entry at `list_ptr + i*8`)
/// into an Aver `List<String>` (cons cells of GC `(array i8)`
/// strings). Allocated when at least one consumer is registered
/// (today: `Args.get`; lands later for `Disk.listDir`, etc.).
pub(super) struct DecodeListStringIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
    pub(super) string_type_idx: u32,
    pub(super) list_string_type_idx: u32,
}

/// Phase 1.3.3 — `__rt_canonical_env_lookup(retptr, key_ptr,
/// key_len) -> Option<String>` helper indices. Walks the
/// `list<tuple<string, string>>` canonical-ABI lowered at retptr
/// and returns `Option.Some(value)` for the matching key — or
/// `Option.None` when no entry matches. Aver's surface signature
/// is `Env.get(name) -> Option<String>`, so this helper has to
/// produce the discriminated struct itself; emitting a bare
/// `(array i8)` would force every call site to invent a wrapper.
pub(super) struct EnvGetLookupIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
    pub(super) string_type_idx: u32,
    pub(super) option_string_type_idx: u32,
}

/// Phase 1.4b — `__rt_format_iso8601(secs i64, nanos i32) ->
/// ref null $string` helper indices. Pure-compute helper, no
/// retptr / no LM read. Turns the (secs, nanos) datetime returned
/// by `wasi:clocks/wall-clock.now` into Aver's RFC3339-like
/// `Time.now() -> String`. Algorithm matches
/// `aver-rt::format_utc_rfc3339_like` (Howard Hinnant's
/// civil_from_days for date math, fixed-width digit emission for
/// the 24-byte output buffer).
pub(super) struct FormatIso8601Indices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
    pub(super) string_type_idx: u32,
}

/// Phase 1.3.4 — `__rt_console_read_line() -> ref null
/// $result_string_string` helper indices. Loops 1-byte
/// `blocking-read` calls until `\n` or EOF; the accumulator
/// lives in a `cabi_realloc`-owned LM buffer that doubles on
/// overflow, then gets copied into a fresh GC `(array i8)` for
/// the `Result.Ok` payload. EOF on the first read is the only
/// path to `Result.Err("EOF")`; partial-line-then-close yields
/// `Result.Ok(buf)` (Unix convention for missing trailing
/// newline).
pub(super) struct ConsoleReadLineIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
    pub(super) string_type_idx: u32,
    pub(super) result_string_string_type_idx: u32,
}

/// Phase 1.4c — `__rt_time_sleep(ms i64) -> ()` helper indices.
/// Wraps `subscribe-duration` + `poll` + `[resource-drop]pollable`
/// in one body so the pollable resource never escapes the helper.
/// Source-level Aver still sees `Time.sleep(ms) -> Unit` — the
/// pollable model is an implementation detail of the wasip2 path.
pub(super) struct TimeSleepIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
}

/// Phase 1.5.1 — `__rt_disk_exists(path: ref string) -> i32`
/// helper indices. Lazy-fetches the first preopen descriptor and
/// returns the bool tag of `stat-at` against the preopen.
pub(super) struct DiskExistsIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
}

/// Phase 1.5.2 — `__rt_disk_read_text(path: ref string) ->
/// ref null $result_string_string` helper indices. Reads the
/// file at `path` (relative to the cached preopen) into a fresh
/// GC `(array i8)` and returns it wrapped in `Result.Ok`. Any
/// failure (open / stream / read) collapses to a generic
/// `Result.Err("…")` describing the failed step.
pub(super) struct DiskReadTextIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
    pub(super) string_type_idx: u32,
    pub(super) result_string_string_type_idx: u32,
}

/// Phase 1.5.3 — `__rt_disk_write_text(path, content) ->
/// ref null $result_unit_string` helper indices. Mirrors
/// DiskReadTextIndices but for the write side: open-at with
/// `create | truncate` + `write-via-stream` +
/// `blocking-write-and-flush` + drops.
pub(super) struct DiskWriteTextIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
    pub(super) string_type_idx: u32,
    pub(super) result_unit_string_type_idx: u32,
}

/// Phase 1.5.4 — generic helper indices for the single-wasi-call
/// `Disk.{delete, deleteDir, makeDir}` ops, all of which share
/// the same `(this, path) -> result<_, error-code>` shape.
/// One instance per Aver effect — the body emit
/// (`emit_disk_simple_path_op`) is shared, only the wasi op fn
/// idx and the Err message differ.
pub(super) struct DiskSimplePathOpIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
    pub(super) string_type_idx: u32,
    pub(super) result_unit_string_type_idx: u32,
}

/// Phase 1.5.6 — `__rt_disk_list_dir(path: ref string) ->
/// ref null $result_list_string_string` helper indices. Drives
/// `read-directory-entry` until `Ok(None)`, accumulates entry
/// names into a cons-built `List<String>`. Order is filesystem-
/// dependent (matches POSIX `readdir` semantics).
pub(super) struct DiskListDirIndices {
    pub(super) fn_type: u32,
    pub(super) fn_idx: u32,
    pub(super) string_type_idx: u32,
    pub(super) list_string_type_idx: u32,
    pub(super) result_list_string_string_type_idx: u32,
}

/// Phase 1.3.1 — `cabi_realloc(old_ptr, old_size, align, new_size)
/// -> new_ptr` body. Bump-allocator over linear memory backed by
/// the wasip2 `bump_alloc_ptr` global (initialised to 65536 = page
/// 2 base). Behaviour:
///
/// - `align` is a power of two (1, 2, 4, 8, ...). The bump cursor
///   is aligned UP to `align` before the allocation lands.
/// - Allocation grows linear memory by enough pages to fit the
///   request when needed (`memory.grow` returns -1 on failure;
///   we propagate by returning the unaligned cursor — Component
///   Model treats out-of-memory as a trap regardless).
/// - Realloc (when `old_ptr != 0 && old_size > 0`) copies
///   `min(old_size, new_size)` bytes from the old buffer to the
///   newly-allocated one via `memory.copy`. The old bytes are
///   leaked (no free in a bump allocator) — fine for a CLI command
///   that runs to completion.
/// - When `new_size == 0` the function still returns a valid
///   pointer (the unchanged cursor); callers treat zero-size as
///   "free", which is a no-op for us.
///
/// `bump_global` is the wasm global idx of `Wasip2Globals::
/// bump_alloc_ptr` in the user module. The body emits `global.get`
/// / `global.set` against that exact idx.
pub(super) fn emit_cabi_realloc(bump_global: u32) -> wasm_encoder::Function {
    use wasm_encoder::{BlockType, Function, Instruction, MemArg};

    // Locals beyond params: $aligned (i32, the post-alignment
    // cursor), $end (i32, $aligned + new_size).
    let mut f = Function::new(vec![(2, ValType::I32)]);
    // Param indices: 0=old_ptr, 1=old_size, 2=align, 3=new_size.
    // Local indices: 4=aligned, 5=end.
    let p_old_ptr = 0u32;
    let p_old_size = 1u32;
    let p_align = 2u32;
    let p_new_size = 3u32;
    let l_aligned = 4u32;
    let l_end = 5u32;

    // aligned = (cursor + (align - 1)) & ~(align - 1)
    f.instruction(&Instruction::GlobalGet(bump_global));
    f.instruction(&Instruction::LocalGet(p_align));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(p_align));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Xor);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(l_aligned));

    // end = aligned + new_size
    f.instruction(&Instruction::LocalGet(l_aligned));
    f.instruction(&Instruction::LocalGet(p_new_size));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_end));

    // if end > memory.size * 65536: grow by ceil((end - memory_bytes) / 65536) pages.
    f.instruction(&Instruction::LocalGet(l_end));
    f.instruction(&Instruction::MemorySize(0));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32Shl); // memory.size * 65536
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // pages_needed = ((end + 65535) >> 16) - memory.size
        f.instruction(&Instruction::LocalGet(l_end));
        f.instruction(&Instruction::I32Const(65535));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::I32Const(16));
        f.instruction(&Instruction::I32ShrU);
        f.instruction(&Instruction::MemorySize(0));
        f.instruction(&Instruction::I32Sub);
        f.instruction(&Instruction::MemoryGrow(0));
        f.instruction(&Instruction::Drop); // -1 on failure leaves caller to fault on access
    }
    f.instruction(&Instruction::End);

    // Copy from old_ptr if this is a realloc (old_ptr != 0 && old_size > 0).
    f.instruction(&Instruction::LocalGet(p_old_ptr));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::LocalGet(p_old_size));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // memory.copy(dst=aligned, src=old_ptr, n=min(old_size, new_size))
        f.instruction(&Instruction::LocalGet(l_aligned));
        f.instruction(&Instruction::LocalGet(p_old_ptr));
        // n = old_size if old_size <= new_size else new_size
        f.instruction(&Instruction::LocalGet(p_old_size));
        f.instruction(&Instruction::LocalGet(p_new_size));
        f.instruction(&Instruction::LocalGet(p_old_size));
        f.instruction(&Instruction::LocalGet(p_new_size));
        f.instruction(&Instruction::I32LtU);
        f.instruction(&Instruction::Select);
        let _ = MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }; // unused — MemoryCopy takes src/dst memory indices
        f.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    }
    f.instruction(&Instruction::End);

    // Update the bump cursor and return the aligned ptr.
    f.instruction(&Instruction::LocalGet(l_end));
    f.instruction(&Instruction::GlobalSet(bump_global));
    f.instruction(&Instruction::LocalGet(l_aligned));
    f.instruction(&Instruction::End);
    f
}

/// Phase 1.3.2 — `__rt_canonical_decode_list_string(retptr) ->
/// List<String>` body. Reads the canonical-ABI lowered
/// `list<string>` at retptr (`(list_ptr i32, list_len i32)`),
/// then for each entry (`(str_ptr i32, str_len i32)` at
/// `list_ptr + i*8`) materialises a fresh GC `(array i8)`
/// string and conses it onto the accumulator. Walks the entries
/// in reverse so cons-built list comes out in source order.
pub(super) fn emit_decode_list_string(string_type_idx: u32, list_string_type_idx: u32) -> wasm_encoder::Function {
    use wasm_encoder::{BlockType, Function, HeapType, Instruction, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_type_idx),
    });
    let l_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_string_type_idx),
    });
    // Param 0: retptr (i32). Locals follow.
    let mut f = Function::new(vec![
        (7, ValType::I32), // 1=list_ptr, 2=list_len, 3=i, 4=entry_ptr, 5=str_ptr, 6=str_len, 7=j
        (1, s_ref),         // 8=arr
        (1, l_ref),         // 9=acc
    ]);
    let p_retptr = 0u32;
    let l_list_ptr = 1u32;
    let l_list_len = 2u32;
    let l_i = 3u32;
    let l_entry_ptr = 4u32;
    let l_str_ptr = 5u32;
    let l_str_len = 6u32;
    let l_j = 7u32;
    let l_arr = 8u32;
    let l_acc = 9u32;

    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_off4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    // list_ptr / list_len from retptr.
    f.instruction(&Instruction::LocalGet(p_retptr));
    f.instruction(&Instruction::I32Load(mem4));
    f.instruction(&Instruction::LocalSet(l_list_ptr));
    f.instruction(&Instruction::LocalGet(p_retptr));
    f.instruction(&Instruction::I32Load(mem4_off4));
    f.instruction(&Instruction::LocalSet(l_list_len));

    // acc = ref.null $list_string.
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_string_type_idx)));
    f.instruction(&Instruction::LocalSet(l_acc));

    // i = list_len - 1 (countdown so cons-built list ends up in source order).
    f.instruction(&Instruction::LocalGet(l_list_len));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(l_i));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // if i < 0: br to surrounding block (depth 1).
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::BrIf(1));

    // entry_ptr = list_ptr + i * 8.
    f.instruction(&Instruction::LocalGet(l_list_ptr));
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(3));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_entry_ptr));

    // str_ptr / str_len from entry_ptr.
    f.instruction(&Instruction::LocalGet(l_entry_ptr));
    f.instruction(&Instruction::I32Load(mem4));
    f.instruction(&Instruction::LocalSet(l_str_ptr));
    f.instruction(&Instruction::LocalGet(l_entry_ptr));
    f.instruction(&Instruction::I32Load(mem4_off4));
    f.instruction(&Instruction::LocalSet(l_str_len));

    // arr = array.new_default $string str_len.
    f.instruction(&Instruction::LocalGet(l_str_len));
    f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
    f.instruction(&Instruction::LocalSet(l_arr));

    // for j = 0; j < str_len; j++: arr[j] = LM[str_ptr + j].
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_str_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_str_ptr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::ArraySet(string_type_idx));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // inner loop
    f.instruction(&Instruction::End); // inner block

    // acc = struct.new $list_string {head: arr, tail: acc}.
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::LocalGet(l_acc));
    f.instruction(&Instruction::StructNew(list_string_type_idx));
    f.instruction(&Instruction::LocalSet(l_acc));

    // i -= 1.
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(l_i));

    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // outer loop
    f.instruction(&Instruction::End); // outer block

    f.instruction(&Instruction::LocalGet(l_acc));
    f.instruction(&Instruction::End); // fn end
    f
}

/// Phase 1.3.3 — `__rt_canonical_env_lookup(retptr, key_ptr,
/// key_len) -> Option<String>` body. Walks the canonical-ABI
/// lowered `list<tuple<string, string>>` at retptr, linear-
/// searches for an entry whose key matches the caller-supplied
/// LM byte range, and on hit returns
/// `Option.Some(<value bytes>)` — i.e. `struct.new $option_string`
/// with discriminant 1 and a freshly allocated `(array i8)`
/// payload. On miss returns `Option.None` (`struct.new
/// $option_string` with discriminant 0 + null payload). Aver's
/// `Env.get(name) -> Option<String>` is the source of truth here;
/// returning a bare `(array i8)` would force every call site to
/// invent its own wrapper.
pub(super) fn emit_env_get_lookup(
    string_type_idx: u32,
    option_string_type_idx: u32,
) -> wasm_encoder::Function {
    use wasm_encoder::{BlockType, Function, HeapType, Instruction, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_type_idx),
    });
    // Params: 0=retptr, 1=key_ptr, 2=key_len. Locals follow.
    let mut f = Function::new(vec![
        (8, ValType::I32), // 3=list_ptr 4=list_len 5=i 6=entry_ptr 7=e_key_ptr 8=e_key_len 9=j 10=mismatch
        (1, s_ref),         // 11=arr
    ]);
    let p_retptr = 0u32;
    let p_key_ptr = 1u32;
    let p_key_len = 2u32;
    let l_list_ptr = 3u32;
    let l_list_len = 4u32;
    let l_i = 5u32;
    let l_entry_ptr = 6u32;
    let l_e_key_ptr = 7u32;
    let l_e_key_len = 8u32;
    let l_j = 9u32;
    let l_mismatch = 10u32;
    let l_arr = 11u32;

    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_o4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem4_o8 = MemArg {
        offset: 8,
        align: 2,
        memory_index: 0,
    };
    let mem4_o12 = MemArg {
        offset: 12,
        align: 2,
        memory_index: 0,
    };
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    // list_ptr / list_len from retptr.
    f.instruction(&Instruction::LocalGet(p_retptr));
    f.instruction(&Instruction::I32Load(mem4));
    f.instruction(&Instruction::LocalSet(l_list_ptr));
    f.instruction(&Instruction::LocalGet(p_retptr));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_list_len));

    // i = 0
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_i));

    // Outer block(label=found-match) carries the matched-value
    // result; outer loop scans entries until we either br-found
    // (with the value String on the stack) or fall through.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    // if i >= list_len: br to surrounding block (no match, fall
    // through to empty-string return).
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::LocalGet(l_list_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));

    // entry_ptr = list_ptr + i * 16 (each tuple<string, string> is
    // 4 i32 fields = 16 bytes packed).
    f.instruction(&Instruction::LocalGet(l_list_ptr));
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_entry_ptr));

    // e_key_ptr / e_key_len from entry.
    f.instruction(&Instruction::LocalGet(l_entry_ptr));
    f.instruction(&Instruction::I32Load(mem4));
    f.instruction(&Instruction::LocalSet(l_e_key_ptr));
    f.instruction(&Instruction::LocalGet(l_entry_ptr));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_e_key_len));

    // Quick reject if lengths differ; advance i and restart the
    // outer loop. Depth count from inside this If: 0=If, 1=outer
    // Loop, 2=outer Block — `Br(1)` jumps to the Loop label
    // (= top of outer loop, the canonical "continue").
    f.instruction(&Instruction::LocalGet(l_e_key_len));
    f.instruction(&Instruction::LocalGet(p_key_len));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_i));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_i));
        f.instruction(&Instruction::Br(1));
    }
    f.instruction(&Instruction::End);

    // Lengths match. Byte-by-byte compare LM[key_ptr..] vs
    // LM[e_key_ptr..]. mismatch=0 by default; flip to 1 on first
    // diff and break out of the inner loop.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_mismatch));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(p_key_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(p_key_ptr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::LocalGet(l_e_key_ptr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalSet(l_mismatch));
        f.instruction(&Instruction::Br(2)); // break inner loop
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // inner loop
    f.instruction(&Instruction::End); // inner block

    // If mismatch: advance i, continue.
    f.instruction(&Instruction::LocalGet(l_mismatch));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Match! Read e_val_ptr / e_val_len from entry (offsets 8
        // and 12 — the second `string` of the flattened
        // tuple<string, string>) and copy bytes into a fresh GC
        // `(array i8)`. Return through the outer block.
        f.instruction(&Instruction::LocalGet(l_entry_ptr));
        f.instruction(&Instruction::I32Load(mem4_o8));
        f.instruction(&Instruction::LocalSet(l_e_key_ptr)); // reuse: now holds val_ptr
        f.instruction(&Instruction::LocalGet(l_entry_ptr));
        f.instruction(&Instruction::I32Load(mem4_o12));
        f.instruction(&Instruction::LocalSet(l_e_key_len)); // reuse: now holds val_len

        f.instruction(&Instruction::LocalGet(l_e_key_len));
        f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        f.instruction(&Instruction::LocalSet(l_arr));

        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalSet(l_j));
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::Loop(BlockType::Empty));
        f.instruction(&Instruction::LocalGet(l_j));
        f.instruction(&Instruction::LocalGet(l_e_key_len));
        f.instruction(&Instruction::I32GeU);
        f.instruction(&Instruction::BrIf(1));
        f.instruction(&Instruction::LocalGet(l_arr));
        f.instruction(&Instruction::LocalGet(l_j));
        f.instruction(&Instruction::LocalGet(l_e_key_ptr));
        f.instruction(&Instruction::LocalGet(l_j));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::I32Load8U(mem1));
        f.instruction(&Instruction::ArraySet(string_type_idx));
        f.instruction(&Instruction::LocalGet(l_j));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_j));
        f.instruction(&Instruction::Br(0));
        f.instruction(&Instruction::End); // copy loop
        f.instruction(&Instruction::End); // copy block

        // Build `Option.Some(arr)` and return it: discriminant=1
        // followed by the array payload, then `struct.new
        // $option_string`.
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalGet(l_arr));
        f.instruction(&Instruction::StructNew(option_string_type_idx));
        f.instruction(&Instruction::Return);
    }
    f.instruction(&Instruction::End); // mismatch==0 branch

    // Mismatch — advance i, retry outer loop.
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_i));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // outer loop
    f.instruction(&Instruction::End); // outer block

    // No-match fallthrough: build `Option.None` — discriminant=0
    // and a null `(array i8)` payload, wrapped in
    // `struct.new $option_string`. The match arm for `Option.None`
    // never reads the payload, so a null is the cheapest valid
    // value (avoids an `array.new_default` heap allocation).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
    f.instruction(&Instruction::StructNew(option_string_type_idx));
    f.instruction(&Instruction::End); // fn end
    f
}

/// Phase 1.4b — `__rt_format_iso8601(secs i64, nanos i32) -> ref
/// null $string` body. Pure-compute helper, no LM read / no
/// retptr. Builds the 24-byte UTF-8 buffer
/// `YYYY-MM-DDTHH:MM:SS.mmmZ` from the `(seconds, nanoseconds)`
/// pair `wasi:clocks/wall-clock.now` produces.
///
/// Date math uses Howard Hinnant's `civil_from_days` (the same
/// algorithm `aver-rt::format_utc_rfc3339_like` uses on the
/// host); positive-z fast path only — wasi-clocks returns u64
/// seconds so the value is always >= 0 in practice. The output
/// is materialised as a fresh `(array i8)` and the digit writes
/// are inlined: ~24 short stanzas × ~6 instructions each.
///
/// `wasm-opt -Oz` strips this when no source-level `Time.now`
/// reaches the helper (Time.unixMs alone never calls it).
pub(super) fn emit_format_iso8601(string_type_idx: u32) -> wasm_encoder::Function {
    use wasm_encoder::{Function, HeapType, Instruction, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_type_idx),
    });
    // Param indices: 0 = secs (i64), 1 = nanos (i32).
    // Local indices follow: i64 group 2..=9, i32 group 10..=20,
    // ref group at 21.
    let mut f = Function::new(vec![
        (8, ValType::I64),  // 2..=9: z, era, doe, yoe, y, doy, mp_i64, days
        (11, ValType::I32), // 10..=20: hour, minute, second, ms, mp, day, month, year, sod, _, _
        (1, s_ref),         // 21: arr
    ]);
    let p_secs = 0u32;
    let p_nanos = 1u32;
    let l_z = 2u32;
    let l_era = 3u32;
    let l_doe = 4u32;
    let l_yoe = 5u32;
    let l_y = 6u32;
    let l_doy = 7u32;
    let l_mp_i64 = 8u32;
    let l_days = 9u32;
    let l_hour = 10u32;
    let l_minute = 11u32;
    let l_second = 12u32;
    let l_ms = 13u32;
    let l_mp = 14u32;
    let l_day = 15u32;
    let l_month = 16u32;
    let l_year = 17u32;
    let l_sod = 18u32;
    let l_arr = 21u32;

    // ── time-of-day pieces ──────────────────────────────────
    // days = secs / 86400 (i64; positive-domain assumption)
    f.instruction(&Instruction::LocalGet(p_secs));
    f.instruction(&Instruction::I64Const(86_400));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::LocalSet(l_days));
    // sod = (secs % 86400) wrapped to i32 (always 0..86399)
    f.instruction(&Instruction::LocalGet(p_secs));
    f.instruction(&Instruction::I64Const(86_400));
    f.instruction(&Instruction::I64RemS);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(l_sod));
    // hour = sod / 3600
    f.instruction(&Instruction::LocalGet(l_sod));
    f.instruction(&Instruction::I32Const(3_600));
    f.instruction(&Instruction::I32DivU);
    f.instruction(&Instruction::LocalSet(l_hour));
    // minute = (sod % 3600) / 60
    f.instruction(&Instruction::LocalGet(l_sod));
    f.instruction(&Instruction::I32Const(3_600));
    f.instruction(&Instruction::I32RemU);
    f.instruction(&Instruction::I32Const(60));
    f.instruction(&Instruction::I32DivU);
    f.instruction(&Instruction::LocalSet(l_minute));
    // second = sod % 60
    f.instruction(&Instruction::LocalGet(l_sod));
    f.instruction(&Instruction::I32Const(60));
    f.instruction(&Instruction::I32RemU);
    f.instruction(&Instruction::LocalSet(l_second));
    // ms = nanos / 1_000_000
    f.instruction(&Instruction::LocalGet(p_nanos));
    f.instruction(&Instruction::I32Const(1_000_000));
    f.instruction(&Instruction::I32DivU);
    f.instruction(&Instruction::LocalSet(l_ms));

    // ── civil_from_days (positive-z fast path) ─────────────
    // z = days + 719468
    f.instruction(&Instruction::LocalGet(l_days));
    f.instruction(&Instruction::I64Const(719_468));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalSet(l_z));
    // era = z / 146_097
    f.instruction(&Instruction::LocalGet(l_z));
    f.instruction(&Instruction::I64Const(146_097));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::LocalSet(l_era));
    // doe = z - era * 146_097
    f.instruction(&Instruction::LocalGet(l_z));
    f.instruction(&Instruction::LocalGet(l_era));
    f.instruction(&Instruction::I64Const(146_097));
    f.instruction(&Instruction::I64Mul);
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalSet(l_doe));
    // yoe = (doe - doe/1460 + doe/36524 - doe/146096) / 365
    f.instruction(&Instruction::LocalGet(l_doe));
    f.instruction(&Instruction::LocalGet(l_doe));
    f.instruction(&Instruction::I64Const(1_460));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalGet(l_doe));
    f.instruction(&Instruction::I64Const(36_524));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalGet(l_doe));
    f.instruction(&Instruction::I64Const(146_096));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::I64Const(365));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::LocalSet(l_yoe));
    // y = yoe + era * 400
    f.instruction(&Instruction::LocalGet(l_yoe));
    f.instruction(&Instruction::LocalGet(l_era));
    f.instruction(&Instruction::I64Const(400));
    f.instruction(&Instruction::I64Mul);
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalSet(l_y));
    // doy = doe - (365*yoe + yoe/4 - yoe/100)
    f.instruction(&Instruction::LocalGet(l_doe));
    f.instruction(&Instruction::LocalGet(l_yoe));
    f.instruction(&Instruction::I64Const(365));
    f.instruction(&Instruction::I64Mul);
    f.instruction(&Instruction::LocalGet(l_yoe));
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalGet(l_yoe));
    f.instruction(&Instruction::I64Const(100));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalSet(l_doy));
    // mp = (5*doy + 2) / 153 (i64, then narrow to i32)
    f.instruction(&Instruction::LocalGet(l_doy));
    f.instruction(&Instruction::I64Const(5));
    f.instruction(&Instruction::I64Mul);
    f.instruction(&Instruction::I64Const(2));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Const(153));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::LocalSet(l_mp_i64));
    f.instruction(&Instruction::LocalGet(l_mp_i64));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(l_mp));
    // day = doy_lo - (153*mp + 2)/5 + 1 (all i32 — doy fits in i32 since 0..=365)
    f.instruction(&Instruction::LocalGet(l_doy));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalGet(l_mp));
    f.instruction(&Instruction::I32Const(153));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32DivS);
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_day));
    // month = mp + (mp < 10 ? 3 : -9)
    f.instruction(&Instruction::LocalGet(l_mp));
    f.instruction(&Instruction::I32Const(3));
    f.instruction(&Instruction::I32Const(-9));
    f.instruction(&Instruction::LocalGet(l_mp));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::Select);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_month));
    // year = y_lo + (month <= 2 ? 1 : 0)
    f.instruction(&Instruction::LocalGet(l_y));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l_month));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32LeS);
    f.instruction(&Instruction::Select);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_year));

    // arr = (array.new_default $string 24)
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
    f.instruction(&Instruction::LocalSet(l_arr));

    // ── digit / separator writes ───────────────────────────
    // Each stanza pushes (arr, idx, byte_value) and `array.set`s
    // it. Digit stanzas do `((val / divisor) % 10) + '0'`; ASCII
    // separators push the literal byte.
    write_digit(&mut f, l_arr, 0, l_year, 1_000, string_type_idx);
    write_digit(&mut f, l_arr, 1, l_year, 100, string_type_idx);
    write_digit(&mut f, l_arr, 2, l_year, 10, string_type_idx);
    write_digit(&mut f, l_arr, 3, l_year, 1, string_type_idx);
    write_byte(&mut f, l_arr, 4, b'-', string_type_idx);
    write_digit(&mut f, l_arr, 5, l_month, 10, string_type_idx);
    write_digit(&mut f, l_arr, 6, l_month, 1, string_type_idx);
    write_byte(&mut f, l_arr, 7, b'-', string_type_idx);
    write_digit(&mut f, l_arr, 8, l_day, 10, string_type_idx);
    write_digit(&mut f, l_arr, 9, l_day, 1, string_type_idx);
    write_byte(&mut f, l_arr, 10, b'T', string_type_idx);
    write_digit(&mut f, l_arr, 11, l_hour, 10, string_type_idx);
    write_digit(&mut f, l_arr, 12, l_hour, 1, string_type_idx);
    write_byte(&mut f, l_arr, 13, b':', string_type_idx);
    write_digit(&mut f, l_arr, 14, l_minute, 10, string_type_idx);
    write_digit(&mut f, l_arr, 15, l_minute, 1, string_type_idx);
    write_byte(&mut f, l_arr, 16, b':', string_type_idx);
    write_digit(&mut f, l_arr, 17, l_second, 10, string_type_idx);
    write_digit(&mut f, l_arr, 18, l_second, 1, string_type_idx);
    write_byte(&mut f, l_arr, 19, b'.', string_type_idx);
    write_digit(&mut f, l_arr, 20, l_ms, 100, string_type_idx);
    write_digit(&mut f, l_arr, 21, l_ms, 10, string_type_idx);
    write_digit(&mut f, l_arr, 22, l_ms, 1, string_type_idx);
    write_byte(&mut f, l_arr, 23, b'Z', string_type_idx);

    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::End); // fn end
    f
}

/// Inline helper for `emit_format_iso8601`: writes
/// `arr[pos] = ((val_local / divisor) % 10) + '0'`. `divisor == 1`
/// short-circuits the division step.
pub(super) fn write_digit(
    f: &mut wasm_encoder::Function,
    l_arr: u32,
    pos: i32,
    val_local: u32,
    divisor: i32,
    string_type_idx: u32,
) {
    use wasm_encoder::Instruction;
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::I32Const(pos));
    f.instruction(&Instruction::LocalGet(val_local));
    if divisor > 1 {
        f.instruction(&Instruction::I32Const(divisor));
        f.instruction(&Instruction::I32DivU);
    }
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32RemU);
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::ArraySet(string_type_idx));
}

/// Inline helper for `emit_format_iso8601`: writes a fixed ASCII
/// byte at `arr[pos]`.
pub(super) fn write_byte(
    f: &mut wasm_encoder::Function,
    l_arr: u32,
    pos: i32,
    byte: u8,
    string_type_idx: u32,
) {
    use wasm_encoder::Instruction;
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::I32Const(pos));
    f.instruction(&Instruction::I32Const(byte as i32));
    f.instruction(&Instruction::ArraySet(string_type_idx));
}

/// Phase 1.3.4 — `__rt_console_read_line() -> ref null
/// $result_string_string` body. Lazy-fetches stdin via
/// `wasi:cli/stdin.get-stdin` (cached in the supplied global),
/// allocates a 256-byte initial buffer + a 12-byte retptr in the
/// `cabi_realloc` heap, then loops 1-byte
/// `wasi:io/streams.[method]input-stream.blocking-read` calls.
///
/// Per iteration: read the result tag; on `Ok`, look at the
/// `(data_ptr, data_len)` pair; on `data_len == 0` exit as EOF;
/// otherwise inspect the byte — `\n` ends the line, `\r` is
/// silently skipped (Windows-style newline tolerance), anything
/// else is appended to the buffer (which doubles in capacity
/// when full). `Err` from the host is treated as EOF.
///
/// Final result: `Result.Ok(line)` whenever any bytes were
/// collected (even when terminated by close/error — Unix
/// convention for the missing trailing newline); `Result.Err("EOF")`
/// only when the very first read produced zero usable bytes.
pub(super) fn emit_console_read_line(
    string_type_idx: u32,
    result_type_idx: u32,
    stdin_handle_global: u32,
    cabi_realloc_fn: u32,
    get_stdin_fn: u32,
    blocking_read_fn: u32,
) -> wasm_encoder::Function {
    use wasm_encoder::{BlockType, Function, HeapType, Instruction, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_type_idx),
    });
    let r_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(result_type_idx),
    });

    // Locals (after zero params):
    // i32 group: stdin_handle, buf_ptr, buf_cap, buf_len, retptr,
    //            byte, j, data_ptr, data_len, should_err, new_cap (11)
    // ref s: arr (the OK payload)
    let mut f = Function::new(vec![
        (11, ValType::I32), // 0..=10
        (1, s_ref.clone()), // 11: arr
    ]);

    let l_stdin_handle = 0u32;
    let l_buf_ptr = 1u32;
    let l_buf_cap = 2u32;
    let l_buf_len = 3u32;
    let l_retptr = 4u32;
    let l_byte = 5u32;
    let l_j = 6u32;
    let l_data_ptr = 7u32;
    let l_data_len = 8u32;
    let l_should_err = 9u32;
    let l_new_cap = 10u32;
    let l_arr = 11u32;

    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_o4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem4_o8 = MemArg {
        offset: 8,
        align: 2,
        memory_index: 0,
    };
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    // ── lazy-init stdin handle ──────────────────────────────
    f.instruction(&Instruction::GlobalGet(stdin_handle_global));
    f.instruction(&Instruction::LocalSet(l_stdin_handle));
    f.instruction(&Instruction::LocalGet(l_stdin_handle));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::Call(get_stdin_fn));
        f.instruction(&Instruction::LocalTee(l_stdin_handle));
        f.instruction(&Instruction::GlobalSet(stdin_handle_global));
    }
    f.instruction(&Instruction::End);

    // ── alloc buffer (256 bytes, alignment=1) ────────────────
    f.instruction(&Instruction::I32Const(0)); // old_ptr
    f.instruction(&Instruction::I32Const(0)); // old_size
    f.instruction(&Instruction::I32Const(1)); // align
    f.instruction(&Instruction::I32Const(256)); // new_size
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_buf_ptr));
    f.instruction(&Instruction::I32Const(256));
    f.instruction(&Instruction::LocalSet(l_buf_cap));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_buf_len));

    // ── alloc retptr (12 bytes, alignment=4) ─────────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));

    // should_err = 0 (default; flipped to 1 only when EOF before any byte).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_should_err));

    // ── outer block "done" + inner loop "next" ──────────────
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    // blocking-read(stdin_handle, 1, retptr).
    f.instruction(&Instruction::LocalGet(l_stdin_handle));
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(blocking_read_fn));

    // Read the result tag at LM[retptr+0].
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Err branch — treat as EOF: flip should_err only when
        // no bytes were collected, then break out of the loop.
        f.instruction(&Instruction::LocalGet(l_buf_len));
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::LocalSet(l_should_err));
        }
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::Br(2)); // exit outer block
    }
    f.instruction(&Instruction::End);

    // Ok branch — load (data_ptr, data_len) at retptr+4 / +8.
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load(mem4_o8));
    f.instruction(&Instruction::LocalSet(l_data_len));

    // Empty Ok = EOF (host returned an empty list). Same handling
    // as the Err branch.
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_buf_len));
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::LocalSet(l_should_err));
        }
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::Br(2)); // exit outer block
    }
    f.instruction(&Instruction::End);

    // byte = LM[data_ptr]. (1-byte read => data_len == 1.)
    f.instruction(&Instruction::LocalGet(l_data_ptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::LocalSet(l_byte));

    // if byte == '\n' (10): exit outer block.
    f.instruction(&Instruction::LocalGet(l_byte));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::Br(2));
    }
    f.instruction(&Instruction::End);

    // if byte == '\r' (13): skip (continue).
    f.instruction(&Instruction::LocalGet(l_byte));
    f.instruction(&Instruction::I32Const(13));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::Br(1)); // continue inner loop
    }
    f.instruction(&Instruction::End);

    // Grow buffer if full (buf_len >= buf_cap).
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // new_cap = buf_cap * 2
        f.instruction(&Instruction::LocalGet(l_buf_cap));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalSet(l_new_cap));
        // buf_ptr = cabi_realloc(buf_ptr, buf_cap, 1, new_cap)
        f.instruction(&Instruction::LocalGet(l_buf_ptr));
        f.instruction(&Instruction::LocalGet(l_buf_cap));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalGet(l_new_cap));
        f.instruction(&Instruction::Call(cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_buf_ptr));
        f.instruction(&Instruction::LocalGet(l_new_cap));
        f.instruction(&Instruction::LocalSet(l_buf_cap));
    }
    f.instruction(&Instruction::End);

    // LM[buf_ptr + buf_len] = byte
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l_byte));
    f.instruction(&Instruction::I32Store8(mem1));

    // buf_len += 1
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_buf_len));

    // continue inner loop.
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // inner loop
    f.instruction(&Instruction::End); // outer block

    // ── build Result ────────────────────────────────────────
    f.instruction(&Instruction::LocalGet(l_should_err));
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Result.Err("EOF") — discriminant 0, ok=null payload, err=arr.
        f.instruction(&Instruction::I32Const(3));
        f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        f.instruction(&Instruction::LocalSet(l_arr));
        write_byte(&mut f, l_arr, 0, b'E', string_type_idx);
        write_byte(&mut f, l_arr, 1, b'O', string_type_idx);
        write_byte(&mut f, l_arr, 2, b'F', string_type_idx);
        // Stack: tag=0, ok=null, err=arr
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
        f.instruction(&Instruction::LocalGet(l_arr));
        f.instruction(&Instruction::StructNew(result_type_idx));
        f.instruction(&Instruction::Return);
    }
    f.instruction(&Instruction::End);

    // Result.Ok(line) — copy buf bytes to fresh GC array of size buf_len.
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
    f.instruction(&Instruction::LocalSet(l_arr));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::ArraySet(string_type_idx));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // copy loop
    f.instruction(&Instruction::End); // copy block

    // Stack: tag=1, ok=arr, err=null
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
    f.instruction(&Instruction::StructNew(result_type_idx));
    f.instruction(&Instruction::End); // fn end
    f
}

/// Phase 1.4c — `__rt_time_sleep(ms i64) -> ()` body.
///
/// Pipeline: convert milliseconds to nanoseconds, fetch a fresh
/// pollable from `wasi:clocks/monotonic-clock.subscribe-duration`,
/// stash that pollable handle in a 4-byte LM buffer (the borrow
/// list `poll` takes), allocate an 8-byte retptr for `poll`'s
/// `list<u32>` result, call `poll` (host blocks until the timer
/// pollable is ready), then `[resource-drop]pollable` the handle.
///
/// The pollable lives only inside this helper — no global cache,
/// no source-level `Pollable` type — so this is the simplest
/// place where the Component-Model "pollable" word actually
/// touches Aver, and even then it stays implementation detail.
/// Per-call `[resource-drop]` is mandatory: each `subscribe-
/// duration` returns a brand-new handle, leaving them undropped
/// would leak host-side resources at the rate of one per
/// `Time.sleep` call.
pub(super) fn emit_time_sleep(
    cabi_realloc_fn: u32,
    subscribe_duration_fn: u32,
    poll_fn: u32,
    drop_pollable_fn: u32,
) -> wasm_encoder::Function {
    use wasm_encoder::{Function, Instruction, MemArg};

    // Locals: 1 = pollable_handle, 2 = in_buf, 3 = retptr (param 0 = ms i64).
    let mut f = Function::new(vec![(3, ValType::I32)]);
    let p_ms = 0u32;
    let l_pollable = 1u32;
    let l_in_buf = 2u32;
    let l_retptr = 3u32;

    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };

    // ns = ms * 1_000_000  (i64; saturates well within i64 range
    // for any practical sleep)
    f.instruction(&Instruction::LocalGet(p_ms));
    f.instruction(&Instruction::I64Const(1_000_000));
    f.instruction(&Instruction::I64Mul);
    f.instruction(&Instruction::Call(subscribe_duration_fn));
    f.instruction(&Instruction::LocalSet(l_pollable));

    // in_buf = cabi_realloc(0, 0, 4, 4); LM[in_buf] = pollable
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_in_buf));
    f.instruction(&Instruction::LocalGet(l_in_buf));
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::I32Store(mem4));

    // retptr = cabi_realloc(0, 0, 4, 8) — for `list<u32>` result
    // of poll. Helper ignores the indices: the only pollable in
    // `in` is our timer, and "ready" is the only outcome.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));

    // poll(in_buf, 1, retptr) — blocks until at least one
    // pollable is ready (which means our timer fired).
    f.instruction(&Instruction::LocalGet(l_in_buf));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(poll_fn));

    // [resource-drop] pollable — release the host-side handle.
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::Call(drop_pollable_fn));

    f.instruction(&Instruction::End);
    f
}

/// Phase 1.5.1 — `__rt_disk_exists(path: ref string) -> i32` body.
///
/// Pipeline:
/// 1. Lazy-init the preopen descriptor — if the cache global is
///    `-1`, call `wasi:filesystem/preopens.get-directories`,
///    take the first entry's `(descriptor, _path_string)` tuple,
///    cache the descriptor handle. If the list is empty, leave
///    the cache at `-1` and return `0` (no preopens ⇒ nothing
///    can exist by definition).
/// 2. Marshal the path argument into LM[0..len] via
///    `__rt_string_to_lm`; that helper writes utf-8 bytes and
///    returns the byte count.
/// 3. Allocate a 96-byte retptr in the cabi_realloc heap (large
///    enough for `result<descriptor-stat, error-code>` —
///    descriptor-stat itself is ~72 bytes; padding + result tag
///    pushes the conservative size to 96).
/// 4. Call `stat-at(preopen, path-flags=1 (symlink-follow),
///    path_ptr=0, path_len, retptr)`.
/// 5. Read the result tag at `LM[retptr]`; return `1` for `Ok`,
///    `0` for `Err`.
///
/// We never read the descriptor-stat payload — `Disk.exists` is
/// a Bool, all we need is the tag.
pub(super) fn emit_disk_exists(
    preopen_global: u32,
    cabi_realloc_fn: u32,
    str_to_lm_fn: u32,
    get_directories_fn: u32,
    stat_at_fn: u32,
) -> wasm_encoder::Function {
    use wasm_encoder::{BlockType, Function, Instruction, MemArg};

    // Locals: 1 = preopen, 2 = path_len, 3 = retptr, 4 = list_ptr,
    // 5 = list_len. Param 0 is `ref null $string`.
    // We don't reference the param's exact ref type here — that's
    // baked into the function type allocated above; locals just
    // carry the i32 working values.
    let mut f = Function::new(vec![(5, ValType::I32)]);
    let p_path = 0u32;
    let l_preopen = 1u32;
    let l_path_len = 2u32;
    let l_retptr = 3u32;
    let l_list_ptr = 4u32;
    let l_list_len = 5u32;

    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_o4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    // ── lazy-init preopen ─────────────────────────────────────
    f.instruction(&Instruction::GlobalGet(preopen_global));
    f.instruction(&Instruction::LocalSet(l_preopen));
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // retptr = cabi_realloc(0, 0, 4, 8)
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(4));
        f.instruction(&Instruction::I32Const(8));
        f.instruction(&Instruction::Call(cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_retptr));
        f.instruction(&Instruction::LocalGet(l_retptr));
        f.instruction(&Instruction::Call(get_directories_fn));
        f.instruction(&Instruction::LocalGet(l_retptr));
        f.instruction(&Instruction::I32Load(mem4));
        f.instruction(&Instruction::LocalSet(l_list_ptr));
        f.instruction(&Instruction::LocalGet(l_retptr));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(l_list_len));
        // list_len > 0 → take first descriptor (LM[list_ptr])
        f.instruction(&Instruction::LocalGet(l_list_len));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32GtU);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_list_ptr));
            f.instruction(&Instruction::I32Load(mem4));
            f.instruction(&Instruction::LocalTee(l_preopen));
            f.instruction(&Instruction::GlobalSet(preopen_global));
        }
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::End);

    // No preopens? exists ⇒ false.
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::Return);
    }
    f.instruction(&Instruction::End);

    // ── marshal path bytes to LM[0..len] ──────────────────────
    f.instruction(&Instruction::LocalGet(p_path));
    f.instruction(&Instruction::Call(str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_path_len));

    // ── allocate retptr (96 bytes, alignment=8) ──────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Const(96));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));

    // ── stat-at(preopen, path_flags=1, path_ptr=0, path_len,
    //           retptr) ──
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(1)); // path-flags = symlink-follow
    f.instruction(&Instruction::I32Const(0)); // path_ptr = 0 (LM[0..len])
    f.instruction(&Instruction::LocalGet(l_path_len));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(stat_at_fn));

    // ── return tag == 0 (Ok) ⇒ 1; tag != 0 ⇒ 0 ───────────────
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Eqz);

    f.instruction(&Instruction::End); // fn end
    f
}

/// Phase 1.5.2 — `__rt_disk_read_text(path: ref string) ->
/// ref null $result_string_string` body.
///
/// Pipeline (failure at any step short-circuits to a generic
/// `Result.Err("…")`):
///   1. Lazy-init preopen via `wasi:filesystem/preopens.
///      get-directories` (same cache as `__rt_disk_exists`); if
///      no preopens are available ⇒ `Err("no preopens")`.
///   2. Marshal `path` through `__rt_string_to_lm` to LM[0..len].
///   3. `open-at(preopen, path-flags=symlink-follow, path,
///      open-flags=0, descriptor-flags=READ=1)` ⇒ on Err
///      `Err("open failed")`; on Ok stash the file descriptor.
///   4. `read-via-stream(fd, offset=0)` ⇒ on Err drop fd, return
///      `Err("read-via-stream failed")`; on Ok stash the
///      input-stream handle.
///   5. Loop `[method]input-stream.blocking-read(stream, 65536,
///      retptr)`: on Ok-with-bytes append into the cabi_realloc
///      growing buffer (doubles when full); on Ok-empty or
///      Err-closed exit cleanly; on Err-failure drop both
///      resources and return `Err("read failed")`.
///   6. Drop the input-stream and file descriptor (per-call
///      resources, mandatory or they leak host-side).
///   7. Materialise the buffer bytes into a fresh GC
///      `(array i8)` and wrap in `Result.Ok`.
pub(super) fn emit_disk_read_text(
    string_type_idx: u32,
    result_type_idx: u32,
    preopen_global: u32,
    cabi_realloc_fn: u32,
    str_to_lm_fn: u32,
    get_directories_fn: u32,
    open_at_fn: u32,
    read_via_stream_fn: u32,
    blocking_read_fn: u32,
    drop_descriptor_fn: u32,
    drop_input_stream_fn: u32,
) -> wasm_encoder::Function {
    use wasm_encoder::{BlockType, Function, HeapType, Instruction, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_type_idx),
    });

    // Locals:
    // i32 (16): preopen, path_len, retptr_open, retptr_stream,
    //           retptr_read, fd, stream, buf_ptr, buf_cap, buf_len,
    //           data_ptr, data_len, j, list_ptr, list_len, new_cap
    // ref s: arr (Ok payload OR per-call err string scratch)
    let mut f = Function::new(vec![(16, ValType::I32), (1, s_ref.clone())]);
    let p_path = 0u32;
    let l_preopen = 1u32;
    let l_path_len = 2u32;
    let l_retptr_open = 3u32;
    let l_retptr_stream = 4u32;
    let l_retptr_read = 5u32;
    let l_fd = 6u32;
    let l_stream = 7u32;
    let l_buf_ptr = 8u32;
    let l_buf_cap = 9u32;
    let l_buf_len = 10u32;
    let l_data_ptr = 11u32;
    let l_data_len = 12u32;
    let l_j = 13u32;
    let l_list_ptr = 14u32;
    let l_list_len = 15u32;
    let l_new_cap = 16u32;
    let l_arr = 17u32;

    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_o4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem4_o8 = MemArg {
        offset: 8,
        align: 2,
        memory_index: 0,
    };
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    // Helper closures factored as plain emit fns: build a fresh
    // string of the given bytes and push the constructed
    // `Result.Err(<bytes>)` then `Return`. Inlined per call site;
    // saves a few dozen lines vs. repeating the byte writes.
    let emit_err = |f: &mut Function, msg: &[u8]| {
        f.instruction(&Instruction::I32Const(msg.len() as i32));
        f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        f.instruction(&Instruction::LocalSet(l_arr));
        for (i, b) in msg.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(l_arr));
            f.instruction(&Instruction::I32Const(i as i32));
            f.instruction(&Instruction::I32Const(*b as i32));
            f.instruction(&Instruction::ArraySet(string_type_idx));
        }
        // tag=0 (Err), ok=null, err=arr
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
        f.instruction(&Instruction::LocalGet(l_arr));
        f.instruction(&Instruction::StructNew(result_type_idx));
        f.instruction(&Instruction::Return);
    };

    // ── lazy-init preopen (mirrors emit_disk_exists) ─────────
    f.instruction(&Instruction::GlobalGet(preopen_global));
    f.instruction(&Instruction::LocalSet(l_preopen));
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(4));
        f.instruction(&Instruction::I32Const(8));
        f.instruction(&Instruction::Call(cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_retptr_open));
        f.instruction(&Instruction::LocalGet(l_retptr_open));
        f.instruction(&Instruction::Call(get_directories_fn));
        f.instruction(&Instruction::LocalGet(l_retptr_open));
        f.instruction(&Instruction::I32Load(mem4));
        f.instruction(&Instruction::LocalSet(l_list_ptr));
        f.instruction(&Instruction::LocalGet(l_retptr_open));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(l_list_len));
        f.instruction(&Instruction::LocalGet(l_list_len));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32GtU);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_list_ptr));
            f.instruction(&Instruction::I32Load(mem4));
            f.instruction(&Instruction::LocalTee(l_preopen));
            f.instruction(&Instruction::GlobalSet(preopen_global));
        }
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        emit_err(&mut f, b"no preopens");
    }
    f.instruction(&Instruction::End);

    // ── marshal path bytes to LM[0..len] ──────────────────────
    f.instruction(&Instruction::LocalGet(p_path));
    f.instruction(&Instruction::Call(str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_path_len));

    // ── open-at(preopen, 1, 0, path_len, 0, 1, retptr_open) ──
    // open_flags=0 (no create/truncate/exclusive/directory),
    // descriptor_flags=1 (READ).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_open));

    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(1)); // path-flags = symlink-follow
    f.instruction(&Instruction::I32Const(0)); // path_ptr = 0
    f.instruction(&Instruction::LocalGet(l_path_len));
    f.instruction(&Instruction::I32Const(0)); // open-flags = 0
    f.instruction(&Instruction::I32Const(1)); // descriptor-flags = READ
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::Call(open_at_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        emit_err(&mut f, b"open failed");
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_fd));

    // ── read-via-stream(fd, 0_i64, retptr_stream) ─────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_stream));

    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::Call(read_via_stream_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Drop the file descriptor before returning Err.
        f.instruction(&Instruction::LocalGet(l_fd));
        f.instruction(&Instruction::Call(drop_descriptor_fn));
        emit_err(&mut f, b"read-via-stream failed");
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_stream));

    // ── allocate growing buffer + per-iteration retptr (12B) ─
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_buf_ptr));
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::LocalSet(l_buf_cap));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_buf_len));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_read));

    // ── read loop: blocking-read(stream, 65536, retptr_read) ─
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::I64Const(65_536));
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::Call(blocking_read_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Err — at retptr+4 sits the stream-error tag (0=
        // last-operation-failed, 1=closed). closed is EOF (good);
        // last-operation-failed is a real failure → drop + Err.
        f.instruction(&Instruction::LocalGet(l_retptr_read));
        f.instruction(&Instruction::I32Load8U(MemArg {
            offset: 4,
            align: 0,
            memory_index: 0,
        }));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            // closed → exit loop, build Ok(buf).
            f.instruction(&Instruction::Br(3));
        }
        f.instruction(&Instruction::End);
        // last-operation-failed → drop both, return Err.
        f.instruction(&Instruction::LocalGet(l_stream));
        f.instruction(&Instruction::Call(drop_input_stream_fn));
        f.instruction(&Instruction::LocalGet(l_fd));
        f.instruction(&Instruction::Call(drop_descriptor_fn));
        emit_err(&mut f, b"read failed");
    }
    f.instruction(&Instruction::End);

    // Ok branch — read (data_ptr, data_len) at retptr+4 / +8.
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load(mem4_o8));
    f.instruction(&Instruction::LocalSet(l_data_len));

    // Empty Ok = EOF; exit loop. Depth 0=Loop, 1=Block — br 1
    // jumps to the Block end (= falls through to drops + Ok build).
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));

    // Grow buffer if needed: while buf_cap < buf_len + data_len,
    // buf_cap *= 2; then realloc.
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::LocalSet(l_new_cap));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalSet(l_new_cap));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // grow loop
    f.instruction(&Instruction::End); // grow block
    // Realloc only if cap actually grew.
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_buf_ptr));
        f.instruction(&Instruction::LocalGet(l_buf_cap));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalGet(l_new_cap));
        f.instruction(&Instruction::Call(cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_buf_ptr));
        f.instruction(&Instruction::LocalGet(l_new_cap));
        f.instruction(&Instruction::LocalSet(l_buf_cap));
    }
    f.instruction(&Instruction::End);

    // memory.copy(dst=buf_ptr+buf_len, src=data_ptr, n=data_len)
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });

    // buf_len += data_len
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_buf_len));

    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // read loop
    f.instruction(&Instruction::End); // read block

    // ── drops ───────────────────────────────────────────────
    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::Call(drop_input_stream_fn));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::Call(drop_descriptor_fn));

    // ── Result.Ok(arr) — copy buf_ptr[0..buf_len] into a fresh
    //   `(array i8)` of size buf_len ───────────────────────────
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
    f.instruction(&Instruction::LocalSet(l_arr));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::ArraySet(string_type_idx));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // copy loop
    f.instruction(&Instruction::End); // copy block

    // tag=1 (Ok), ok=arr, err=null
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
    f.instruction(&Instruction::StructNew(result_type_idx));

    f.instruction(&Instruction::End); // fn end
    f
}


/// Phase 1.5.3 — `__rt_disk_write_text(path: ref string,
/// content: ref string) -> ref null $result_unit_string` body.
///
/// Pipeline (failure at any step short-circuits to a generic
/// `Result.Err("…")`):
///   1. Lazy-init preopen (shared cache global with `Disk.exists`
///      / `Disk.readText`); empty list ⇒ `Err("no preopens")`.
///   2. Marshal `path` through `__rt_string_to_lm` to LM[0..len].
///   3. `open-at(preopen, path-flags=symlink-follow, path,
///      open-flags=create|truncate (=5),
///      descriptor-flags=WRITE (=2))` ⇒ on Err `Err("open
///      failed")`; on Ok stash the file descriptor.
///   4. `write-via-stream(fd, offset=0)` ⇒ on Err drop fd,
///      `Err("write-via-stream failed")`; on Ok stash the
///      output-stream handle.
///   5. Marshal `content` through `__rt_string_to_lm` —
///      OVERWRITES the path bytes at LM[0..]. Fine because the
///      host already consumed the path during the open-at call;
///      we don't need those bytes any more.
///   6. `[method]output-stream.blocking-write-and-flush(stream,
///      ptr=0, len=content_len, retptr)` writes the bytes. The
///      retptr's tag at offset 0 says `0=Ok / 1=Err`. On Err
///      drop both resources and return `Err("write failed")`.
///   7. Drop output-stream + file descriptor (per-call resources;
///      mandatory or they leak host-side).
///   8. Build `Result.Ok(Unit)` — `Unit` payload is a single
///      `i32 = 0` placeholder (matches the wasm-gc Result struct
///      shape `(i32 tag, T ok, E err)` where `T = Unit` lowers to
///      i32).
pub(super) fn emit_disk_write_text(
    string_type_idx: u32,
    result_type_idx: u32,
    preopen_global: u32,
    cabi_realloc_fn: u32,
    str_to_lm_fn: u32,
    get_directories_fn: u32,
    open_at_fn: u32,
    write_via_stream_fn: u32,
    blocking_write_fn: u32,
    drop_descriptor_fn: u32,
    drop_output_stream_fn: u32,
    // When `true`, lower to `Disk.appendText` instead — opens the
    // file with `create` only (no truncate, so existing content
    // stays), and calls `append-via-stream(fd) -> result<output-
    // stream, _>` (1 arg + retptr) instead of `write-via-stream(
    // fd, offset=0, retptr)` (2 args + retptr). The host appends
    // at end-of-file in this mode.
    is_append: bool,
) -> wasm_encoder::Function {
    use wasm_encoder::{BlockType, Function, HeapType, Instruction, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_type_idx),
    });

    // Locals layout (after 2 ref-s params at idx 0, 1):
    //   10 i32 locals (idx 2..=11): preopen, path_len, content_len,
    //     retptr_open, retptr_stream, retptr_write, fd, stream,
    //     list_ptr, list_len.
    //   1 ref-s local at idx 12: arr (Err string scratch).
    let mut f = Function::new(vec![(10, ValType::I32), (1, s_ref.clone())]);
    let p_path = 0u32;
    let p_content = 1u32;
    let l_preopen = 2u32;
    let l_path_len = 3u32;
    let l_content_len = 4u32;
    let l_retptr_open = 5u32;
    let l_retptr_stream = 6u32;
    let l_retptr_write = 7u32;
    let l_fd = 8u32;
    let l_stream = 9u32;
    let l_list_ptr = 10u32;
    let l_list_len = 11u32;
    let l_arr = 12u32;

    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_o4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    let emit_err = |f: &mut Function, msg: &[u8]| {
        f.instruction(&Instruction::I32Const(msg.len() as i32));
        f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        f.instruction(&Instruction::LocalSet(l_arr));
        for (i, b) in msg.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(l_arr));
            f.instruction(&Instruction::I32Const(i as i32));
            f.instruction(&Instruction::I32Const(*b as i32));
            f.instruction(&Instruction::ArraySet(string_type_idx));
        }
        // tag=0 (Err), ok=0 (Unit placeholder), err=arr
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(l_arr));
        f.instruction(&Instruction::StructNew(result_type_idx));
        f.instruction(&Instruction::Return);
    };

    // ── lazy-init preopen ─────────────────────────────────────
    f.instruction(&Instruction::GlobalGet(preopen_global));
    f.instruction(&Instruction::LocalSet(l_preopen));
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(4));
        f.instruction(&Instruction::I32Const(8));
        f.instruction(&Instruction::Call(cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_retptr_open));
        f.instruction(&Instruction::LocalGet(l_retptr_open));
        f.instruction(&Instruction::Call(get_directories_fn));
        f.instruction(&Instruction::LocalGet(l_retptr_open));
        f.instruction(&Instruction::I32Load(mem4));
        f.instruction(&Instruction::LocalSet(l_list_ptr));
        f.instruction(&Instruction::LocalGet(l_retptr_open));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(l_list_len));
        f.instruction(&Instruction::LocalGet(l_list_len));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32GtU);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_list_ptr));
            f.instruction(&Instruction::I32Load(mem4));
            f.instruction(&Instruction::LocalTee(l_preopen));
            f.instruction(&Instruction::GlobalSet(preopen_global));
        }
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        emit_err(&mut f, b"no preopens");
    }
    f.instruction(&Instruction::End);

    // ── marshal path → LM[0..path_len] ────────────────────────
    f.instruction(&Instruction::LocalGet(p_path));
    f.instruction(&Instruction::Call(str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_path_len));

    // ── open-at(preopen, 1, 0, path_len, 5, 2, retptr) ───────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_open));

    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(1)); // path-flags = symlink-follow
    f.instruction(&Instruction::I32Const(0)); // path_ptr = 0
    f.instruction(&Instruction::LocalGet(l_path_len));
    // open-flags = CREATE (1) for append, CREATE | TRUNCATE (5)
    // for plain write. Both make the file if missing; only write
    // wipes pre-existing content.
    f.instruction(&Instruction::I32Const(if is_append { 1 } else { 5 }));
    f.instruction(&Instruction::I32Const(2)); // descriptor-flags = WRITE
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::Call(open_at_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        emit_err(&mut f, b"open failed");
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_fd));

    // ── {write,append}-via-stream(fd[, 0_i64], retptr_stream) ──
    // write-via-stream takes (fd, offset, retptr); append-via-
    // stream takes (fd, retptr) — no offset, host appends at EOF.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_stream));

    f.instruction(&Instruction::LocalGet(l_fd));
    if !is_append {
        f.instruction(&Instruction::I64Const(0));
    }
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::Call(write_via_stream_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_fd));
        f.instruction(&Instruction::Call(drop_descriptor_fn));
        emit_err(
            &mut f,
            if is_append {
                b"append-via-stream failed"
            } else {
                b"write-via-stream failed"
            },
        );
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_stream));

    // ── marshal content → LM[0..content_len] (overwrites path) ──
    f.instruction(&Instruction::LocalGet(p_content));
    f.instruction(&Instruction::Call(str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_content_len));

    // ── allocate retptr_write (12 bytes for result<_, stream-error>) ──
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_write));

    // ── blocking-write-and-flush(stream, 0, content_len, retptr) ──
    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::I32Const(0)); // ptr = 0 (LM[0..len])
    f.instruction(&Instruction::LocalGet(l_content_len));
    f.instruction(&Instruction::LocalGet(l_retptr_write));
    f.instruction(&Instruction::Call(blocking_write_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_write));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_stream));
        f.instruction(&Instruction::Call(drop_output_stream_fn));
        f.instruction(&Instruction::LocalGet(l_fd));
        f.instruction(&Instruction::Call(drop_descriptor_fn));
        emit_err(&mut f, b"write failed");
    }
    f.instruction(&Instruction::End);

    // ── drops ───────────────────────────────────────────────
    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::Call(drop_output_stream_fn));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::Call(drop_descriptor_fn));

    // ── Result.Ok(Unit) — tag=1, ok=0, err=null ─────────────
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
    f.instruction(&Instruction::StructNew(result_type_idx));

    f.instruction(&Instruction::End); // fn end
    f
}

/// Phase 1.5.4 — generic body for the single-call
/// `Disk.{delete, deleteDir, makeDir}` ops. Each op's wasi WIT
/// signature is `func(this: borrow<descriptor>, path: string)
/// -> result<_, error-code>`, so only the host fn idx +
/// Err-string distinguish the three.
///
/// Pipeline:
///   1. Lazy-init preopen (shared cache).
///   2. Marshal `path` to LM[0..path_len].
///   3. Allocate 4-byte retptr (`tag i8` + `error-code u8`,
///      padded). On Err host writes tag=1 + error code.
///   4. Call `<op>-at(preopen, path_ptr=0, path_len, retptr)`.
///   5. Read tag at retptr+0; tag == 0 ⇒ `Result.Ok(Unit)`,
///      tag != 0 ⇒ `Result.Err(<msg>)` with the caller-supplied
///      bytes inlined into a fresh GC `(array i8)`.
pub(super) fn emit_disk_simple_path_op(
    string_type_idx: u32,
    result_type_idx: u32,
    preopen_global: u32,
    cabi_realloc_fn: u32,
    str_to_lm_fn: u32,
    get_directories_fn: u32,
    op_fn: u32,
    err_msg: &[u8],
) -> wasm_encoder::Function {
    use wasm_encoder::{BlockType, Function, HeapType, Instruction, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_type_idx),
    });

    // Locals (after 1 ref-s param at idx 0):
    //   5 i32 locals (idx 1..=5): preopen, path_len, retptr,
    //     list_ptr, list_len.
    //   1 ref-s local at idx 6: arr (Err string scratch).
    let mut f = Function::new(vec![(5, ValType::I32), (1, s_ref.clone())]);
    let p_path = 0u32;
    let l_preopen = 1u32;
    let l_path_len = 2u32;
    let l_retptr = 3u32;
    let l_list_ptr = 4u32;
    let l_list_len = 5u32;
    let l_arr = 6u32;

    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_o4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    let emit_err = |f: &mut Function| {
        f.instruction(&Instruction::I32Const(err_msg.len() as i32));
        f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        f.instruction(&Instruction::LocalSet(l_arr));
        for (i, b) in err_msg.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(l_arr));
            f.instruction(&Instruction::I32Const(i as i32));
            f.instruction(&Instruction::I32Const(*b as i32));
            f.instruction(&Instruction::ArraySet(string_type_idx));
        }
        // tag=0 (Err), ok=0 (Unit placeholder), err=arr
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(l_arr));
        f.instruction(&Instruction::StructNew(result_type_idx));
        f.instruction(&Instruction::Return);
    };

    // ── lazy-init preopen ─────────────────────────────────────
    f.instruction(&Instruction::GlobalGet(preopen_global));
    f.instruction(&Instruction::LocalSet(l_preopen));
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(4));
        f.instruction(&Instruction::I32Const(8));
        f.instruction(&Instruction::Call(cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_retptr));
        f.instruction(&Instruction::LocalGet(l_retptr));
        f.instruction(&Instruction::Call(get_directories_fn));
        f.instruction(&Instruction::LocalGet(l_retptr));
        f.instruction(&Instruction::I32Load(mem4));
        f.instruction(&Instruction::LocalSet(l_list_ptr));
        f.instruction(&Instruction::LocalGet(l_retptr));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(l_list_len));
        f.instruction(&Instruction::LocalGet(l_list_len));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32GtU);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_list_ptr));
            f.instruction(&Instruction::I32Load(mem4));
            f.instruction(&Instruction::LocalTee(l_preopen));
            f.instruction(&Instruction::GlobalSet(preopen_global));
        }
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        emit_err(&mut f);
    }
    f.instruction(&Instruction::End);

    // ── marshal path → LM[0..path_len] ────────────────────────
    f.instruction(&Instruction::LocalGet(p_path));
    f.instruction(&Instruction::Call(str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_path_len));

    // ── allocate retptr (4 bytes) + call op ──────────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));

    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(0)); // path_ptr = 0
    f.instruction(&Instruction::LocalGet(l_path_len));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(op_fn));

    // ── tag check ───────────────────────────────────────────
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        emit_err(&mut f);
    }
    f.instruction(&Instruction::End);

    // ── Result.Ok(Unit) ─────────────────────────────────────
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
    f.instruction(&Instruction::StructNew(result_type_idx));

    f.instruction(&Instruction::End); // fn end
    f
}

/// Phase 1.5.6 — `__rt_disk_list_dir(path: ref string) ->
/// ref null $result_list_string_string` body.
///
/// Pipeline (any wasi failure ⇒ `Result.Err("…")`):
///   1. Lazy-init preopen.
///   2. Marshal `path` to LM[0..len].
///   3. `open-at(preopen, path-flags=symlink-follow, path,
///      open-flags=DIRECTORY (=2), descriptor-flags=READ (=1))`
///      ⇒ Err ⇒ `Result.Err("opendir failed")`.
///   4. `read-directory(fd)` ⇒ Err ⇒ drop fd, `Result.Err(
///      "read-directory failed")`. Ok ⇒ stash the
///      `directory-entry-stream` handle.
///   5. Loop `read-directory-entry(stream)`:
///      - `Ok(None)` ⇒ EOF, exit loop.
///      - `Ok(Some(entry))` ⇒ allocate fresh GC `(array i8)`
///        of size name_len, copy bytes from LM[name_ptr..],
///        cons onto growing list (head = newest).
///      - `Err(_)` ⇒ drop both, `Result.Err("readdir failed")`.
///   6. Drop directory-entry-stream + descriptor (per-call,
///      mandatory).
///   7. Wrap the cons-built list in `Result.Ok`.
///
/// Order of returned entries is filesystem-dependent — same
/// guarantee POSIX `readdir` makes (i.e. none).
///
/// retptr layout for `read-directory-entry`'s
/// `result<option<directory-entry>, error-code>` (20 bytes total,
/// 4-byte aligned):
///   - +0: result tag i8
///   - +4: option tag i8 (when result is Ok)
///   - +8: directory-entry.type i8 (when option is Some)
///   - +12: directory-entry.name_ptr i32
///   - +16: directory-entry.name_len i32
///   - +4 (when result is Err): error-code u8 (we ignore which)
pub(super) fn emit_disk_list_dir(
    string_type_idx: u32,
    list_string_type_idx: u32,
    result_type_idx: u32,
    preopen_global: u32,
    cabi_realloc_fn: u32,
    str_to_lm_fn: u32,
    get_directories_fn: u32,
    open_at_fn: u32,
    read_directory_fn: u32,
    read_directory_entry_fn: u32,
    drop_descriptor_fn: u32,
    drop_dir_entry_stream_fn: u32,
) -> wasm_encoder::Function {
    use wasm_encoder::{BlockType, Function, HeapType, Instruction, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_type_idx),
    });
    let l_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_string_type_idx),
    });

    // Locals (after 1 ref-s param at idx 0):
    //   12 i32 locals (idx 1..=12): preopen, path_len,
    //     retptr_open, retptr_stream, retptr_entry, fd, dstream,
    //     name_ptr, name_len, j, list_ptr, list_len.
    //   1 ref-s local at idx 13: arr (entry name + Err scratch).
    //   1 ref-list local at idx 14: acc (cons accumulator).
    let mut f = Function::new(vec![
        (12, ValType::I32),
        (1, s_ref.clone()),
        (1, l_ref.clone()),
    ]);
    let p_path = 0u32;
    let l_preopen = 1u32;
    let l_path_len = 2u32;
    let l_retptr_open = 3u32;
    let l_retptr_stream = 4u32;
    let l_retptr_entry = 5u32;
    let l_fd = 6u32;
    let l_dstream = 7u32;
    let l_name_ptr = 8u32;
    let l_name_len = 9u32;
    let l_j = 10u32;
    let l_list_ptr = 11u32;
    let l_list_len = 12u32;
    let l_arr = 13u32;
    let l_acc = 14u32;

    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_o4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    let emit_err = |f: &mut Function, msg: &[u8]| {
        f.instruction(&Instruction::I32Const(msg.len() as i32));
        f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        f.instruction(&Instruction::LocalSet(l_arr));
        for (i, b) in msg.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(l_arr));
            f.instruction(&Instruction::I32Const(i as i32));
            f.instruction(&Instruction::I32Const(*b as i32));
            f.instruction(&Instruction::ArraySet(string_type_idx));
        }
        // tag=0 (Err), ok=null list, err=arr
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::RefNull(HeapType::Concrete(list_string_type_idx)));
        f.instruction(&Instruction::LocalGet(l_arr));
        f.instruction(&Instruction::StructNew(result_type_idx));
        f.instruction(&Instruction::Return);
    };

    // ── lazy-init preopen ─────────────────────────────────────
    f.instruction(&Instruction::GlobalGet(preopen_global));
    f.instruction(&Instruction::LocalSet(l_preopen));
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(4));
        f.instruction(&Instruction::I32Const(8));
        f.instruction(&Instruction::Call(cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_retptr_open));
        f.instruction(&Instruction::LocalGet(l_retptr_open));
        f.instruction(&Instruction::Call(get_directories_fn));
        f.instruction(&Instruction::LocalGet(l_retptr_open));
        f.instruction(&Instruction::I32Load(mem4));
        f.instruction(&Instruction::LocalSet(l_list_ptr));
        f.instruction(&Instruction::LocalGet(l_retptr_open));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(l_list_len));
        f.instruction(&Instruction::LocalGet(l_list_len));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32GtU);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_list_ptr));
            f.instruction(&Instruction::I32Load(mem4));
            f.instruction(&Instruction::LocalTee(l_preopen));
            f.instruction(&Instruction::GlobalSet(preopen_global));
        }
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        emit_err(&mut f, b"no preopens");
    }
    f.instruction(&Instruction::End);

    // ── marshal path ─────────────────────────────────────────
    f.instruction(&Instruction::LocalGet(p_path));
    f.instruction(&Instruction::Call(str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_path_len));

    // ── open-at(preopen, 1, 0, path_len, 2 (DIRECTORY), 1
    //   (READ), retptr) ──
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_open));

    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(1)); // path-flags = symlink-follow
    f.instruction(&Instruction::I32Const(0)); // path_ptr = 0
    f.instruction(&Instruction::LocalGet(l_path_len));
    f.instruction(&Instruction::I32Const(2)); // open-flags = DIRECTORY
    f.instruction(&Instruction::I32Const(1)); // descriptor-flags = READ
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::Call(open_at_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        emit_err(&mut f, b"opendir failed");
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_fd));

    // ── read-directory(fd, retptr_stream) ─────────────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_stream));

    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::Call(read_directory_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_fd));
        f.instruction(&Instruction::Call(drop_descriptor_fn));
        emit_err(&mut f, b"read-directory failed");
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_dstream));

    // ── allocate retptr_entry (20 bytes) ─────────────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(20));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_entry));

    // ── acc = ref.null $list_string ──────────────────────────
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_string_type_idx)));
    f.instruction(&Instruction::LocalSet(l_acc));

    // ── iterate read-directory-entry until Ok(None) or Err ──
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    f.instruction(&Instruction::LocalGet(l_dstream));
    f.instruction(&Instruction::LocalGet(l_retptr_entry));
    f.instruction(&Instruction::Call(read_directory_entry_fn));

    // result tag at retptr+0
    f.instruction(&Instruction::LocalGet(l_retptr_entry));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Err — drop both, return Err("readdir failed")
        f.instruction(&Instruction::LocalGet(l_dstream));
        f.instruction(&Instruction::Call(drop_dir_entry_stream_fn));
        f.instruction(&Instruction::LocalGet(l_fd));
        f.instruction(&Instruction::Call(drop_descriptor_fn));
        emit_err(&mut f, b"readdir failed");
    }
    f.instruction(&Instruction::End);

    // option tag at retptr+4
    f.instruction(&Instruction::LocalGet(l_retptr_entry));
    f.instruction(&Instruction::I32Load8U(MemArg {
        offset: 4,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1)); // None ⇒ exit Block (EOF)

    // Some(entry) — read name_ptr at +12, name_len at +16
    f.instruction(&Instruction::LocalGet(l_retptr_entry));
    f.instruction(&Instruction::I32Load(MemArg {
        offset: 12,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(l_name_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr_entry));
    f.instruction(&Instruction::I32Load(MemArg {
        offset: 16,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(l_name_len));

    // arr = array.new_default $string name_len
    f.instruction(&Instruction::LocalGet(l_name_len));
    f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
    f.instruction(&Instruction::LocalSet(l_arr));

    // for j = 0..name_len: arr[j] = LM[name_ptr + j]
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_name_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_name_ptr));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::ArraySet(string_type_idx));
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_j));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // copy loop
    f.instruction(&Instruction::End); // copy block

    // acc = struct.new $list_string {head: arr, tail: acc}
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::LocalGet(l_acc));
    f.instruction(&Instruction::StructNew(list_string_type_idx));
    f.instruction(&Instruction::LocalSet(l_acc));

    // continue
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // outer loop
    f.instruction(&Instruction::End); // outer block

    // ── drops ───────────────────────────────────────────────
    f.instruction(&Instruction::LocalGet(l_dstream));
    f.instruction(&Instruction::Call(drop_dir_entry_stream_fn));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::Call(drop_descriptor_fn));

    // ── Result.Ok(acc) — tag=1, ok=acc, err=null ─────────────
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_acc));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
    f.instruction(&Instruction::StructNew(result_type_idx));

    f.instruction(&Instruction::End); // fn end
    f
}
