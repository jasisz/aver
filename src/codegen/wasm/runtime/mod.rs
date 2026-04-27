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

use super::value::*;

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
/// Migrated so far: alloc, truncate.
#[allow(clippy::vec_init_then_push)]
pub fn emit_runtime_functions(rt: &RuntimeFuncIndices) -> Vec<Function> {
    let mut funcs = Vec::new();

    funcs.push(alloc::emit_collect_begin()); // $collect_begin
    funcs.push(alloc::emit_collect_end(rt)); // $collect_end
    funcs.push(alloc::emit_rebase_i32()); // $rebase_i32
    funcs.push(alloc::emit_retain_i32(rt)); // $retain_i32
    funcs.push(alloc::emit_wrap(rt, OBJ_WRAPPER)); // $wrap (i64 inner)
    funcs.push(alloc::emit_wrap_f64(rt)); // $wrap_f64
    funcs.push(alloc::emit_wrap_i32(rt)); // $wrap_i32
    funcs.push(alloc::emit_unwrap_i64()); // $unwrap
    funcs.push(alloc::emit_unwrap_f64()); // $unwrap_f64
    funcs.push(alloc::emit_unwrap_i32()); // $unwrap_i32
    funcs.push(alloc::emit_obj_kind()); // $obj_kind
    funcs.push(alloc::emit_obj_tag()); // $obj_tag
    funcs.push(alloc::emit_obj_meta()); // $obj_meta
    funcs.push(alloc::emit_obj_field_i64()); // $obj_field
    funcs.push(alloc::emit_obj_field_f64()); // $obj_field_f64
    funcs.push(alloc::emit_obj_field_i32()); // $obj_field_i32
    funcs.push(lists::emit_list_cons_i64(rt)); // $list_cons
    funcs.push(lists::emit_list_cons_f64(rt)); // $list_cons_f64
    funcs.push(io::emit_int_to_str()); // $int_to_str
    funcs.push(io::emit_float_to_str()); // $float_to_str
    funcs.push(io::emit_fd_write_buf(rt)); // $fd_write_buf
    funcs.push(strings::emit_str_eq()); // $str_eq
    funcs.push(strings::emit_str_concat(rt)); // $str_concat
    funcs.push(strings::emit_i64_to_str_obj(rt)); // $i64_to_str_obj
    funcs.push(strings::emit_f64_to_str_obj(rt)); // $f64_to_str_obj
    funcs.push(lists::emit_list_take(rt)); // $list_take
    funcs.push(lists::emit_list_drop()); // $list_drop
    funcs.push(lists::emit_list_concat(rt)); // $list_concat
    funcs.push(lists::emit_list_reverse(rt)); // $list_reverse
    funcs.push(lists::emit_list_contains(rt)); // $list_contains
    funcs.push(lists::emit_list_zip(rt)); // $list_zip
    funcs.push(maps::emit_map_get(rt)); // $map_get
    funcs.push(maps::emit_map_set(rt)); // $map_set
    funcs.push(maps::emit_map_has(rt)); // $map_has
    funcs.push(maps::emit_map_keys(rt)); // $map_keys
    funcs.push(maps::emit_map_entries(rt)); // $map_entries
    funcs.push(vectors::emit_vec_from_list(rt)); // $vec_from_list
    funcs.push(vectors::emit_vec_get(rt)); // $vec_get
    funcs.push(vectors::emit_vec_len()); // $vec_len
    funcs.push(vectors::emit_vec_set(rt)); // $vec_set
    funcs.push(vectors::emit_vec_new(rt)); // $vec_new
    funcs.push(vectors::emit_vec_to_list(rt)); // $vec_to_list
    funcs.push(strings::emit_str_len()); // $str_len
    funcs.push(strings::emit_str_byte_len()); // $str_byte_len
    funcs.push(strings::emit_str_find(rt)); // $str_find
    funcs.push(strings::emit_str_starts_with(rt)); // $str_starts_with
    funcs.push(strings::emit_str_ends_with(rt)); // $str_ends_with
    funcs.push(strings::emit_str_contains(rt)); // $str_contains
    funcs.push(strings::emit_str_char_at(rt)); // $str_char_at
    funcs.push(strings::emit_char_from_code(rt)); // $char_from_code
    funcs.push(strings::emit_char_to_code()); // $char_to_code
    funcs.push(strings::emit_byte_to_hex(rt)); // $byte_to_hex
    funcs.push(strings::emit_byte_from_hex(rt)); // $byte_from_hex
    funcs.push(strings::emit_str_trim(rt)); // $str_trim
    funcs.push(strings::emit_str_slice(rt)); // $str_slice
    funcs.push(strings::emit_str_chars(rt)); // $str_chars
    funcs.push(strings::emit_str_split(rt)); // $str_split
    funcs.push(strings::emit_str_join(rt)); // $str_join
    funcs.push(strings::emit_str_replace(rt)); // $str_replace
    funcs.push(strings::emit_str_to_lower(rt)); // $str_to_lower
    funcs.push(strings::emit_str_to_upper(rt)); // $str_to_upper
    funcs.push(strings::emit_int_from_str(rt)); // $int_from_str
    funcs.push(strings::emit_float_from_str(rt)); // $float_from_str

    funcs
}
