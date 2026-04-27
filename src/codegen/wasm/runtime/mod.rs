/// Emits inline WASM runtime functions for the typed ABI.
///
/// Native arithmetic uses WASM instructions directly (no runtime helpers).
/// Runtime functions handle: allocation, heap objects, IO/print, string ops,
/// list ops, vector ops, and map ops.
///
/// Split into submodules by domain:
/// - `indices` — function/type index structs and dispatch
/// - `alloc` — allocator, wrap/unwrap, object inspection
/// - `io` — stdout writing, number formatting, value printing
/// - `strings` — string equality, concatenation, numeric-to-string conversion
/// - `lists` — cons cells and linked-list operations
/// - `vectors` — flat array operations
/// - `maps` — association-list map operations
mod alloc;
mod indices;
mod io;
mod lists;
mod maps;
mod strings;
mod vectors;
mod wat_module;

use wasm_encoder::Function;


pub use indices::{
    AverRuntimeImports, RuntimeFuncIndices, emit_base_type_section, lookup_type_index,
    rt_type_index,
};
pub use wat_module::build_runtime_wasm;

/// Scratch area for IO in linear memory. Reserved: bytes 0-127.
/// Layout: [0..7] iovec, [8..11] nwritten, [16..37] int_buf,
///         [40] newline/scratch byte, [48..95] float_buf (48 bytes)
pub const IO_SCRATCH_SIZE: u32 = 128;
pub(crate) const IO_IOVEC: u32 = 0;
pub(crate) const IO_NWRITTEN: u32 = 8;
pub(crate) const IO_INT_BUF: u32 = 16;
pub const NEWLINE_ADDR: u32 = 40;
pub(crate) const IO_FLOAT_BUF: u32 = 48; // 48 bytes for float digits (48..95)

/// Emit all runtime function bodies.
///
/// Functions migrated to the `aver_runtime` imported WAT module are
/// intentionally absent here — they live in `runtime/wat/*.part.wat`
/// and are referenced through their import indices on `rt`.
/// Migrated so far: alloc, truncate, obj_kind/tag/meta, obj_field
/// (i64/f64/i32), unwrap (i64/f64/i32), wrap (i64/f64/i32), str_eq,
/// str_concat, list_cons (i64/f64), str_byte_len, str_find,
/// str_starts_with, str_ends_with, str_contains, list_take/drop/
/// concat/reverse/contains/zip, map_get/set/has/keys/entries,
/// vec_from_list/get/len/set/new/to_list, int_to_str, float_to_str,
/// i64_to_str_obj, f64_to_str_obj, str_len, char_to_code,
/// byte_to_hex, byte_from_hex, char_from_code, str_char_at,
/// str_to_lower, str_to_upper, str_trim, str_slice, str_chars,
/// str_split, str_join, str_replace, int_from_str, float_from_str.
#[allow(clippy::vec_init_then_push)]
pub fn emit_runtime_functions(rt: &RuntimeFuncIndices) -> Vec<Function> {
    let mut funcs = Vec::new();

    funcs.push(alloc::emit_collect_begin()); // $collect_begin
    funcs.push(alloc::emit_collect_end(rt)); // $collect_end
    funcs.push(alloc::emit_rebase_i32()); // $rebase_i32
    funcs.push(alloc::emit_retain_i32(rt)); // $retain_i32
    funcs.push(io::emit_fd_write_buf(rt)); // $fd_write_buf

    funcs
}
