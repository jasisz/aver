/// Emits inline WASM runtime functions for the typed ABI.
///
/// Native arithmetic uses WASM instructions directly (no runtime helpers).
/// Runtime functions handle: allocation, heap objects, and IO/print.
use wasm_encoder::{Function, Instruction, ValType};

use super::value::*;

/// Index assignments for runtime functions within the module.
#[derive(Debug, Clone, Copy)]
pub struct RuntimeFuncIndices {
    pub alloc: u32,
    pub wrap: u32,      // (i32, i64) -> i32
    pub wrap_f64: u32,  // (i32, f64) -> i32
    pub wrap_i32: u32,  // (i32, i32) -> i32
    pub unwrap: u32,     // (i32) -> i64
    pub unwrap_f64: u32, // (i32) -> f64
    pub unwrap_i32: u32, // (i32) -> i32
    pub obj_kind: u32,   // (i32) -> i32
    pub obj_tag: u32,    // (i32) -> i32
    pub obj_field: u32,  // (i32, i32) -> i64
    pub obj_field_f64: u32, // (i32, i32) -> f64
    pub obj_field_i32: u32, // (i32, i32) -> i32
    pub list_cons: u32,     // (i64, i32) -> i32
    pub list_cons_f64: u32, // (f64, i32) -> i32
    pub print_i64: u32,     // (i64) -> ()
    pub print_f64: u32,     // (f64) -> ()
    pub print_string: u32,  // (i32) -> ()
    pub print_bool: u32,    // (i32) -> ()
    pub print_heap: u32,    // (i32) -> ()
    pub int_to_str: u32,    // (i64, i32) -> i32
    pub float_to_str: u32,  // (f64, i32) -> i32
    pub fd_write_buf: u32,  // (i32, i32) -> ()
    pub str_eq: u32,        // (i32, i32) -> i32
    pub str_concat: u32,    // (i32, i32) -> i32
    pub i64_to_str_obj: u32, // (i64) -> i32
    pub f64_to_str_obj: u32, // (f64) -> i32
    pub list_take: u32,     // (i32, i64) -> i32
    pub list_drop: u32,     // (i32, i64) -> i32
    pub list_concat: u32,   // (i32, i32) -> i32
    pub list_reverse: u32,  // (i32) -> i32
    pub list_contains: u32, // (i64, i32) -> i32 (value, list) -> bool
    pub list_zip: u32,      // (i32, i32) -> i32
    pub map_get: u32,       // (i32, i32) -> i32  (map, key) -> Option ptr
    pub map_set: u32,       // (i32, i32, i64) -> i32  (map, key, value) -> map
    pub map_has: u32,       // (i32, i32) -> i32  (map, key) -> bool
    pub map_keys: u32,      // (i32) -> i32  (map) -> list
    pub print_value: u32,     // (i64) -> ()  generic value printer
    pub vec_from_list: u32,   // (i32) -> i32  list → vector
    pub vec_get: u32,         // (i32, i64) -> i32  (vec, idx) → Option
    pub vec_len: u32,         // (i32) -> i64  vec → Int
    pub vec_set: u32,         // (i32, i64, i64) -> i32  (vec, idx, val) → Option<Vector>
    pub vec_new: u32,         // (i64, i64) -> i32  (size, fill) → vec
    /// Total number of runtime functions.
    pub count: u32,
    /// Import function index for writing to stdout (either WASI fd_write or aver/console_print).
    pub fd_write_import: u32,
    /// Which adapter mode is active.
    pub adapter: super::WasmAdapter,
}

impl RuntimeFuncIndices {
    pub fn new(base: u32) -> Self {
        let mut i = base;
        let mut next = || {
            let idx = i;
            i += 1;
            idx
        };
        RuntimeFuncIndices {
            alloc: next(),
            wrap: next(),
            wrap_f64: next(),
            wrap_i32: next(),
            unwrap: next(),
            unwrap_f64: next(),
            unwrap_i32: next(),
            obj_kind: next(),
            obj_tag: next(),
            obj_field: next(),
            obj_field_f64: next(),
            obj_field_i32: next(),
            list_cons: next(),
            list_cons_f64: next(),
            print_i64: next(),
            print_f64: next(),
            print_string: next(),
            print_bool: next(),
            print_heap: next(),
            int_to_str: next(),
            float_to_str: next(),
            fd_write_buf: next(),
            str_eq: next(),
            str_concat: next(),
            i64_to_str_obj: next(),
            f64_to_str_obj: next(),
            list_take: next(),
            list_drop: next(),
            list_concat: next(),
            list_reverse: next(),
            list_contains: next(),
            list_zip: next(),
            map_get: next(),
            map_set: next(),
            map_has: next(),
            map_keys: next(),
            print_value: next(),
            vec_from_list: next(),
            vec_get: next(),
            vec_len: next(),
            vec_set: next(),
            vec_new: next(),
            count: i - base,
            fd_write_import: 0,
            adapter: super::WasmAdapter::Aver,
        }
    }
}

/// Scratch area for IO in linear memory. Reserved: bytes 0-127.
/// Layout: [0..7] iovec, [8..11] nwritten, [16..37] int_buf,
///         [40] newline/scratch byte, [48..95] float_buf (48 bytes)
pub const IO_SCRATCH_SIZE: u32 = 128;
const IO_IOVEC: u32 = 0;
const IO_NWRITTEN: u32 = 8;
const IO_INT_BUF: u32 = 16;
pub const NEWLINE_ADDR: u32 = 40;
const IO_FLOAT_BUF: u32 = 48; // 48 bytes for float digits (48..95)

/// Addresses of runtime format strings in data section.
#[derive(Default)]
pub struct RtStrings {
    pub true_: (u32, u32),
    pub false_: (u32, u32),
    pub none: (u32, u32),
    pub empty_list: (u32, u32),
    pub result_ok: (u32, u32),
    pub result_err: (u32, u32),
    pub option_some: (u32, u32),
    pub close_paren: (u32, u32),
    pub open_bracket: (u32, u32),
    pub comma_space: (u32, u32),
}

/// Runtime function type signatures. Indices into the type section.
/// These must match the order in emitter.rs type_section construction.
#[derive(Debug, Clone, Copy)]
pub struct RtTypeIndices {
    pub alloc: u32,          // 0: (i32) -> i32
    pub wrap_i64: u32,       // 1: (i32, i64) -> i32
    pub wrap_f64: u32,       // 2: (i32, f64) -> i32
    pub wrap_i32: u32,       // 3: (i32, i32) -> i32
    pub unwrap_i64: u32,     // 4: (i32) -> i64
    pub unwrap_f64: u32,     // 5: (i32) -> f64
    pub unwrap_i32: u32,     // 6: (i32) -> i32
    pub obj_kind: u32,       // 7: (i32) -> i32  (same as unwrap_i32)
    pub obj_tag: u32,        // 8: (i32) -> i32  (same as unwrap_i32)
    pub obj_field_i64: u32,  // 9: (i32, i32) -> i64
    pub obj_field_f64: u32,  // 10: (i32, i32) -> f64
    pub obj_field_i32: u32,  // 11: (i32, i32) -> i32
    pub list_cons_i64: u32,  // 12: (i64, i32) -> i32
    pub list_cons_f64: u32,  // 13: (f64, i32) -> i32
    pub print_i64: u32,      // 14: (i64) -> ()
    pub print_f64: u32,      // 15: (f64) -> ()
    pub print_i32_void: u32, // 16: (i32) -> ()
    pub int_to_str: u32,     // 17: (i64, i32) -> i32
    pub float_to_str: u32,   // 18: (f64, i32) -> i32
    pub fd_write_buf: u32,   // 19: (i32, i32) -> ()
    pub wasi_fd_write: u32,  // 20: (i32, i32, i32, i32) -> i32
}

/// Get the type index for a given runtime function.
pub fn rt_type_index(rt: &RuntimeFuncIndices, rti: &RtTypeIndices, func_idx: u32, import_func_count: u32) -> u32 {
    let local_idx = func_idx - import_func_count;
    let alloc_local = rt.alloc - import_func_count;

    if local_idx == alloc_local { return rti.alloc; }
    if local_idx == rt.wrap - import_func_count { return rti.wrap_i64; }
    if local_idx == rt.wrap_f64 - import_func_count { return rti.wrap_f64; }
    if local_idx == rt.wrap_i32 - import_func_count { return rti.wrap_i32; }
    if local_idx == rt.unwrap - import_func_count { return rti.unwrap_i64; }
    if local_idx == rt.unwrap_f64 - import_func_count { return rti.unwrap_f64; }
    if local_idx == rt.unwrap_i32 - import_func_count { return rti.unwrap_i32; }
    if local_idx == rt.obj_kind - import_func_count { return rti.obj_kind; }
    if local_idx == rt.obj_tag - import_func_count { return rti.obj_tag; }
    if local_idx == rt.obj_field - import_func_count { return rti.obj_field_i64; }
    if local_idx == rt.obj_field_f64 - import_func_count { return rti.obj_field_f64; }
    if local_idx == rt.obj_field_i32 - import_func_count { return rti.obj_field_i32; }
    if local_idx == rt.list_cons - import_func_count { return rti.list_cons_i64; }
    if local_idx == rt.list_cons_f64 - import_func_count { return rti.list_cons_f64; }
    if local_idx == rt.print_i64 - import_func_count { return rti.print_i64; }
    if local_idx == rt.print_f64 - import_func_count { return rti.print_f64; }
    if local_idx == rt.print_string - import_func_count { return rti.print_i32_void; }
    if local_idx == rt.print_bool - import_func_count { return rti.print_i32_void; }
    if local_idx == rt.print_heap - import_func_count { return rti.print_i32_void; }
    if local_idx == rt.int_to_str - import_func_count { return rti.int_to_str; }
    if local_idx == rt.float_to_str - import_func_count { return rti.float_to_str; }
    if local_idx == rt.fd_write_buf - import_func_count { return rti.fd_write_buf; }
    if local_idx == rt.str_eq - import_func_count { return rti.wrap_i32; }         // (i32,i32)->i32
    if local_idx == rt.str_concat - import_func_count { return rti.wrap_i32; }     // (i32,i32)->i32
    if local_idx == rt.i64_to_str_obj - import_func_count { return 18; }           // (i64)->i32
    if local_idx == rt.f64_to_str_obj - import_func_count { return 19; }           // (f64)->i32
    if local_idx == rt.list_take - import_func_count { return rti.wrap_i32; }      // (i32,i32)->i32
    if local_idx == rt.list_drop - import_func_count { return rti.wrap_i32; }      // (i32,i32)->i32
    if local_idx == rt.list_concat - import_func_count { return rti.wrap_i32; }    // (i32,i32)->i32
    if local_idx == rt.list_reverse - import_func_count { return rti.alloc; }      // (i32)->i32
    if local_idx == rt.list_contains - import_func_count { return 20; }            // (i32,i64)->i32
    if local_idx == rt.list_zip - import_func_count { return rti.wrap_i32; }       // (i32,i32)->i32
    if local_idx == rt.map_get - import_func_count { return rti.wrap_i32; }        // (i32,i32)->i32
    if local_idx == rt.map_set - import_func_count { return 21; }                  // (i32,i32,i64)->i32
    if local_idx == rt.map_has - import_func_count { return rti.wrap_i32; }        // (i32,i32)->i32
    if local_idx == rt.map_keys - import_func_count { return rti.alloc; }          // (i32)->i32
    if local_idx == rt.print_value - import_func_count { return rti.print_i64; }  // (i64)->()
    if local_idx == rt.vec_from_list - import_func_count { return rti.alloc; }     // (i32)->i32
    if local_idx == rt.vec_get - import_func_count { return 20; }                  // (i32,i64)->i32
    if local_idx == rt.vec_len - import_func_count { return rti.unwrap_i64; }     // (i32)->i64
    if local_idx == rt.vec_set - import_func_count { return 22; }                  // (i32,i64,i64)->i32
    if local_idx == rt.vec_new - import_func_count { return 23; }                  // (i64,i64)->i32

    panic!(
        "Unknown runtime function index: {} (base={})",
        func_idx, import_func_count
    );
}

/// Emit all runtime function bodies.
pub fn emit_runtime_functions(rt: &RuntimeFuncIndices, strs: &RtStrings) -> Vec<Function> {
    let mut funcs = Vec::new();

    funcs.push(emit_alloc());                  // $alloc
    funcs.push(emit_wrap(rt, OBJ_WRAPPER, true));    // $wrap (i64 inner)
    funcs.push(emit_wrap_f64(rt));             // $wrap_f64
    funcs.push(emit_wrap_i32(rt));             // $wrap_i32
    funcs.push(emit_unwrap_i64());             // $unwrap
    funcs.push(emit_unwrap_f64());             // $unwrap_f64
    funcs.push(emit_unwrap_i32());             // $unwrap_i32
    funcs.push(emit_obj_kind());               // $obj_kind
    funcs.push(emit_obj_tag());                // $obj_tag
    funcs.push(emit_obj_field_i64());          // $obj_field
    funcs.push(emit_obj_field_f64());          // $obj_field_f64
    funcs.push(emit_obj_field_i32());          // $obj_field_i32
    funcs.push(emit_list_cons_i64(rt));        // $list_cons
    funcs.push(emit_list_cons_f64(rt));        // $list_cons_f64
    funcs.push(emit_print_i64(rt));            // $print_i64
    funcs.push(emit_print_f64(rt));            // $print_f64
    funcs.push(emit_print_string(rt, strs));   // $print_string
    funcs.push(emit_print_bool(rt, strs));     // $print_bool
    funcs.push(emit_print_heap(rt, strs));     // $print_heap
    funcs.push(emit_int_to_str());             // $int_to_str
    funcs.push(emit_float_to_str());           // $float_to_str
    funcs.push(emit_fd_write_buf(rt));         // $fd_write_buf
    funcs.push(emit_str_eq());                 // $str_eq
    funcs.push(emit_str_concat(rt));           // $str_concat
    funcs.push(emit_i64_to_str_obj(rt));       // $i64_to_str_obj
    funcs.push(emit_f64_to_str_obj(rt));       // $f64_to_str_obj
    funcs.push(emit_list_take(rt));            // $list_take
    funcs.push(emit_list_drop());              // $list_drop
    funcs.push(emit_list_concat(rt));          // $list_concat
    funcs.push(emit_list_reverse(rt));         // $list_reverse
    funcs.push(emit_list_contains(rt));        // $list_contains
    funcs.push(emit_list_zip(rt));             // $list_zip
    funcs.push(emit_map_get(rt));              // $map_get
    funcs.push(emit_map_set(rt));              // $map_set
    funcs.push(emit_map_has(rt));              // $map_has
    funcs.push(emit_map_keys(rt));             // $map_keys
    funcs.push(emit_print_value(rt, strs));    // $print_value
    funcs.push(emit_vec_from_list(rt));        // $vec_from_list
    funcs.push(emit_vec_get(rt));              // $vec_get
    funcs.push(emit_vec_len());                // $vec_len
    funcs.push(emit_vec_set(rt));              // $vec_set
    funcs.push(emit_vec_new(rt));              // $vec_new

    funcs
}

// ---------------------------------------------------------------------------
// Allocator
// ---------------------------------------------------------------------------

/// $alloc(size: i32) -> i32
fn emit_alloc() -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: $ptr
    f.instruction(&Instruction::GlobalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::GlobalGet(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::GlobalSet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    f
}

// ---------------------------------------------------------------------------
// Wrap/Unwrap (typed variants)
// ---------------------------------------------------------------------------

/// $wrap(tag: i32, inner: i64) -> i32
/// Allocates wrapper object: [header: i64][inner: i64]
/// Returns raw i32 pointer.
fn emit_wrap(rt: &RuntimeFuncIndices, obj_kind: u64, _inner_is_i64: bool) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: $ptr
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(2));
    // header: kind | (tag << 48) | 1
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const((obj_kind << HDR_KIND_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    // field[0] = inner (i64)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    // return ptr
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f
}

/// $wrap_f64(tag: i32, inner: f64) -> i32
fn emit_wrap_f64(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: $ptr
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(2));
    // header with OBJ_WRAPPER_F64
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const((OBJ_WRAPPER_F64 << HDR_KIND_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    // field[0] = inner (f64)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1)); // f64 param
    f.instruction(&Instruction::F64Store(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f
}

/// $wrap_i32(tag: i32, inner: i32) -> i32
fn emit_wrap_i32(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: $ptr
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(2));
    // header with OBJ_WRAPPER_I32
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const((OBJ_WRAPPER_I32 << HDR_KIND_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    // field[0] = inner (i32 extended to i64)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f
}

/// $unwrap(ptr: i32) -> i64
fn emit_unwrap_i64() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $unwrap_f64(ptr: i32) -> f64
fn emit_unwrap_f64() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $unwrap_i32(ptr: i32) -> i32
fn emit_unwrap_i32() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::End);
    f
}

// ---------------------------------------------------------------------------
// Object inspection
// ---------------------------------------------------------------------------

/// $obj_kind(ptr: i32) -> i32
fn emit_obj_kind() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(HDR_KIND_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::End);
    f
}

/// $obj_tag(ptr: i32) -> i32
fn emit_obj_tag() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::End);
    f
}

/// $obj_field(ptr: i32, idx: i32) -> i64
fn emit_obj_field_i64() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $obj_field_f64(ptr: i32, idx: i32) -> f64
fn emit_obj_field_f64() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $obj_field_i32(ptr: i32, idx: i32) -> i32
fn emit_obj_field_i32() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::End);
    f
}

// ---------------------------------------------------------------------------
// List cons
// ---------------------------------------------------------------------------

/// $list_cons(head: i64, tail: i32) -> i32
fn emit_list_cons_i64(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: $ptr
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(2));
    // header
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(make_header(OBJ_LIST_CONS, 0, 0, 2) as i64));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    // head (i64)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    // tail (i32 → store as i64)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f
}

/// $list_cons_f64(head: f64, tail: i32) -> i32
fn emit_list_cons_f64(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: $ptr
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(2));
    // header (OBJ_LIST_CONS_F64)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(make_header(OBJ_LIST_CONS_F64, 0, 0, 2) as i64));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    // head (f64)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Store(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    // tail (i32 → i64)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f
}

// ---------------------------------------------------------------------------
// IO functions
// ---------------------------------------------------------------------------

/// $write_stdout(ptr: i32, len: i32) -> ()
/// In aver/* mode: direct call to aver/console_print(ptr, len).
/// In WASI mode: iovec setup + call fd_write(stdout, iovec, 1, nwritten).
fn emit_fd_write_buf(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![]);
    match rt.adapter {
        super::WasmAdapter::Aver => {
            // Direct capability call — no iovec, no fd numbers
            f.instruction(&Instruction::LocalGet(0)); // ptr
            f.instruction(&Instruction::LocalGet(1)); // len
            f.instruction(&Instruction::Call(rt.fd_write_import));
            f.instruction(&Instruction::End);
        }
        super::WasmAdapter::Wasi => {
            // WASI: setup iovec buffer and call fd_write
            f.instruction(&Instruction::I32Const(IO_IOVEC as i32));
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                offset: 0, align: 2, memory_index: 0,
            }));
            f.instruction(&Instruction::I32Const(IO_IOVEC as i32));
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                offset: 4, align: 2, memory_index: 0,
            }));
            f.instruction(&Instruction::I32Const(1)); // fd=stdout
            f.instruction(&Instruction::I32Const(IO_IOVEC as i32));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Const(IO_NWRITTEN as i32));
            f.instruction(&Instruction::Call(rt.fd_write_import));
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::End);
        }
    }
    f
}

/// $int_to_str(val: i64, buf: i32) -> i32 — returns (pos<<16)|len
fn emit_int_to_str() -> Function {
    // params: val=0, buf=1. locals: n=2(i64), is_neg=3(i32), pos=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I64),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);
    // n = val (plain i64, no tag extraction needed)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    // n==0?
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Else);
    // is_neg
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::End);
    // pos=21
    f.instruction(&Instruction::I32Const(21));
    f.instruction(&Instruction::LocalSet(4));
    // digit loop
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // neg sign
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(b'-' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    // return (pos<<16)|(21-pos)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32Const(21));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::End); // else
    f.instruction(&Instruction::End); // func
    f
}

/// $float_to_str(val: f64, buf: i32) -> i32 — returns (pos<<16)|len
///
/// Fixed-precision: prints integer part, then '.' + fractional digits (up to 15),
/// stripping trailing zeros. Whole numbers print without decimal point.
fn emit_float_to_str() -> Function {
    // Shortest-roundtrip approach:
    // For N = 1..15, check if trunc(abs_val * 10^N) / 10^N == abs_val.
    // Use the smallest N that gives equality. Then format with N frac digits.
    //
    // params: val=0(f64), buf=1(i32)
    // locals: is_neg=2(i32), abs_val=3(f64), int_part=4(i64), pos=5(i32),
    //         start_pos=6(i32), pow=7(f64), n=8(i32), scaled=9(i64),
    //         frac_int=10(i64), frac_pos=11(i32), frac_digits=12(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32),  // 2: is_neg
        (1, ValType::F64),  // 3: abs_val
        (1, ValType::I64),  // 4: int_part
        (1, ValType::I32),  // 5: pos
        (1, ValType::I32),  // 6: start_pos
        (1, ValType::F64),  // 7: pow
        (1, ValType::I32),  // 8: n (frac digit count)
        (1, ValType::I64),  // 9: scaled
        (1, ValType::I64),  // 10: frac_int
        (1, ValType::I32),  // 11: frac_pos
        (1, ValType::I32),  // 12: frac_digits
    ]);

    // is_neg = val < 0.0
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Const(0.0));
    f.instruction(&Instruction::F64Lt);
    f.instruction(&Instruction::LocalSet(2));

    // abs_val = |val|
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Abs);
    f.instruction(&Instruction::LocalSet(3));

    // int_part = i64.trunc_f64_s(abs_val)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::F64Floor);
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::LocalSet(4));

    // --- Write integer part right-to-left at buf[0..21] ---
    f.instruction(&Instruction::I32Const(21));
    f.instruction(&Instruction::LocalSet(5));

    // int_part == 0?
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::Else);
    // digit loop
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    f.instruction(&Instruction::End); // if/else

    // neg sign
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(b'-' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::End);

    // start_pos = pos (beginning of integer digits in buf)
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalSet(6));

    // --- Check if value is exactly an integer ---
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::F64Floor);
    f.instruction(&Instruction::F64Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(ValType::I32)));
    // Integer: return (start_pos<<16)|(21-start_pos)
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32Const(21));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Or);

    f.instruction(&Instruction::Else);

    // --- Find shortest N: trunc(abs_val * 10^N) / 10^N == abs_val ---
    // pow = 1.0, n = 0
    f.instruction(&Instruction::F64Const(1.0));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(8));

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // n++
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(8));
    // pow *= 10.0
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::F64Const(10.0));
    f.instruction(&Instruction::F64Mul);
    f.instruction(&Instruction::LocalSet(7));
    // scaled = i64.trunc_f64_s(abs_val * pow)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::F64Mul);
    // Round to nearest (add 0.5 then floor — but this introduces error)
    // Actually, just use trunc (floor for positive values)
    f.instruction(&Instruction::F64Floor);
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::LocalSet(9));
    // check = f64.convert_i64_s(scaled) / pow
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::F64ConvertI64S);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::F64Div);
    // if check == abs_val → found shortest
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::F64Eq);
    f.instruction(&Instruction::BrIf(1)); // break out of loop
    // if n >= 15 → give up, use 15
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(15));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::Br(0)); // continue
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    // Now: n = number of frac digits, scaled = integer representation
    // frac_int = scaled - int_part_restored * pow_as_int
    // But simpler: frac_int = scaled % pow_i64
    // where pow_i64 = i64(pow)
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::I64RemS);
    // Make sure it's positive
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::I64RemS);
    f.instruction(&Instruction::LocalSet(10)); // frac_int (positive)

    // Write '.' at position 21
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(b'.' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 21, align: 0, memory_index: 0,
    }));

    // Write frac_int as exactly n digits at buf[22..22+n], right-to-left
    // frac_pos = 22 + n - 1 (rightmost digit position)
    f.instruction(&Instruction::I32Const(22));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(11));

    // frac_digits = n (counter)
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalSet(12));

    // Write digits right-to-left
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // store digit
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    // frac_int /= 10
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(10));
    // frac_pos--, frac_digits--
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(11));
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    // Strip trailing zeros from frac digits
    // frac_end = 22 + n
    f.instruction(&Instruction::I32Const(22));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(11)); // reuse frac_pos as frac_end

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if frac_end <= 22 → break
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(22));
    f.instruction(&Instruction::I32LeS);
    f.instruction(&Instruction::BrIf(1));
    // check last byte
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::BrIf(1)); // not zero, stop
    // frac_end--
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(11));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    // return (start_pos<<16) | (frac_end - start_pos)
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Or);

    f.instruction(&Instruction::End); // else (has frac)
    f.instruction(&Instruction::End); // func
    f
}

// ---------------------------------------------------------------------------
// Print functions
// ---------------------------------------------------------------------------

/// $print_i64(val: i64) -> ()
fn emit_print_i64(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: tmp
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(IO_INT_BUF as i32));
    f.instruction(&Instruction::Call(rt.int_to_str));
    f.instruction(&Instruction::LocalSet(1));
    // ptr = buf + (tmp >> 16), len = tmp & 0xFFFF
    f.instruction(&Instruction::I32Const(IO_INT_BUF as i32));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0xFFFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f
}

/// $print_f64(val: f64) -> ()
fn emit_print_f64(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: tmp
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(IO_FLOAT_BUF as i32));
    f.instruction(&Instruction::Call(rt.float_to_str));
    f.instruction(&Instruction::LocalSet(1));
    // ptr = buf + (tmp >> 16), len = tmp & 0xFFFF
    f.instruction(&Instruction::I32Const(IO_FLOAT_BUF as i32));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0xFFFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f
}

/// $print_string(ptr: i32) -> ()
fn emit_print_string(rt: &RuntimeFuncIndices, _strs: &RtStrings) -> Function {
    let mut f = Function::new(vec![]);
    // Read string length from header (lower 32 bits)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    // String bytes start at ptr+8
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f
}

/// $print_bool(val: i32) -> ()
fn emit_print_bool(rt: &RuntimeFuncIndices, strs: &RtStrings) -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // true
    f.instruction(&Instruction::I32Const(strs.true_.0 as i32));
    f.instruction(&Instruction::I32Const(strs.true_.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Else);
    // false
    f.instruction(&Instruction::I32Const(strs.false_.0 as i32));
    f.instruction(&Instruction::I32Const(strs.false_.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $print_heap(ptr: i32) -> ()
/// Polymorphic printer: reads object header to determine kind and format.
fn emit_print_heap(rt: &RuntimeFuncIndices, strs: &RtStrings) -> Function {
    // params: ptr=0. locals: header=1(i64), kind=2(i32), tag=3(i32), inner_ptr=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I64),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);

    macro_rules! print_static {
        ($f:expr, $rt:expr, $s:expr) => {
            $f.instruction(&Instruction::I32Const($s.0 as i32));
            $f.instruction(&Instruction::I32Const($s.1 as i32));
            $f.instruction(&Instruction::Call($rt.fd_write_buf));
        };
    }

    // Check for None sentinel (-1)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    print_static!(f, rt, strs.none);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Check for empty list (0)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    print_static!(f, rt, strs.empty_list);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Read header
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(1));

    // kind = (header >> 56) & 0xFF
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(56));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(2));

    // === OBJ_STRING (0) ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(rt.print_string));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_WRAPPER (3) — i64 inner ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // tag = (header >> 48) & 0xFF
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    // Print prefix
    emit_wrapper_prefix(&mut f, rt, strs, 3);
    // Print inner (i64)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_i64));
    print_static!(f, rt, strs.close_paren);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_WRAPPER_F64 (7) ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    emit_wrapper_prefix(&mut f, rt, strs, 3);
    // Print inner (f64)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_f64));
    print_static!(f, rt, strs.close_paren);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_WRAPPER_I32 (8) — string/bool inner ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_I32 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    emit_wrapper_prefix(&mut f, rt, strs, 3);
    // Print inner: load as i32 (string pointer), wrap with quotes
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4)); // inner_ptr
    // Print opening quote
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(b'"' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    // Print string content
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::Call(rt.print_string));
    // Print closing quote
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(b'"' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    print_static!(f, rt, strs.close_paren);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_LIST_CONS (4) — generic elements (i64, may be heap ptrs) ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    print_static!(f, rt, strs.open_bracket);
    // Print first element via print_value
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_value));
    // Traverse tail
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4)); // tail ptr
    // Loop through tail
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz); // tail == 0 (empty)
    f.instruction(&Instruction::BrIf(1));
    print_static!(f, rt, strs.comma_space);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_value));
    // next tail
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // Close bracket: store ']' at scratch, fd_write_buf it
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(b']' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_LIST_CONS_F64 (9) — f64 elements ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    print_static!(f, rt, strs.open_bracket);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_f64));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    print_static!(f, rt, strs.comma_space);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 8, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_f64));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(b']' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::End); // func
    f
}

/// $str_eq(a: i32, b: i32) -> i32
/// Compares two string objects by content. Returns 1 if equal, 0 if not.
fn emit_str_eq() -> Function {
    // params: a=0(i32), b=1(i32)
    // locals: len_a=2(i32), len_b=3(i32), i=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: len_a
        (1, ValType::I32), // 3: len_b
        (1, ValType::I32), // 4: i (loop counter)
    ]);

    // Same pointer → equal
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Else);

    // len_a = header_a & 0xFFFFFFFF
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(2));

    // len_b = header_b & 0xFFFFFFFF
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0, align: 3, memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));

    // len_a != len_b → not equal
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Else);

    // Byte-by-byte comparison using result local
    // Reuse local 4 as loop counter, local 2 doubles as result (set to 1 = equal)
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4)); // i = 0

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if i >= len_a → done (equal)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    // Compare byte at a+8+i vs b+8+i
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // Not equal → return 0 immediately
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // i++
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0)); // continue loop
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // All bytes matched → equal
    f.instruction(&Instruction::I32Const(1));

    f.instruction(&Instruction::End); // else (len check)
    f.instruction(&Instruction::End); // else (ptr check)
    f.instruction(&Instruction::End); // func
    f
}

/// $str_concat(a: i32, b: i32) -> i32
/// Concatenates two string objects, returns new string object pointer.
fn emit_str_concat(rt: &RuntimeFuncIndices) -> Function {
    // params: a=0, b=1. locals: len_a=2, len_b=3, ptr=4, total=5
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: len_a
        (1, ValType::I32), // 3: len_b
        (1, ValType::I32), // 4: ptr
        (1, ValType::I32), // 5: total_len
    ]);
    // len_a = header_a & 0xFFFFFFFF
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(2));
    // len_b
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));
    // total = len_a + len_b
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(5));
    // alloc(8 + ((total+7)&~7))
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    // header
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const((OBJ_STRING << HDR_KIND_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    // memory.copy a bytes: dst=ptr+8, src=a+8, len=len_a
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
    // memory.copy b bytes: dst=ptr+8+len_a, src=b+8, len=len_b
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
    // return ptr
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    f
}

/// $i64_to_str_obj(val: i64) -> i32
/// Converts an integer to a heap-allocated string object.
fn emit_i64_to_str_obj(rt: &RuntimeFuncIndices) -> Function {
    // params: val=0(i64). locals: packed=1(i32), pos=2(i32), len=3(i32), ptr=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: packed
        (1, ValType::I32), // 2: pos
        (1, ValType::I32), // 3: len
        (1, ValType::I32), // 4: ptr
    ]);
    // Use int_to_str to format into scratch buffer at IO_INT_BUF
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(IO_INT_BUF as i32));
    f.instruction(&Instruction::Call(rt.int_to_str));
    f.instruction(&Instruction::LocalSet(1)); // packed = (pos<<16)|len
    // pos = packed >> 16
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::LocalSet(2));
    // len = packed & 0xFFFF
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0xFFFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    // alloc string object: 8 + ((len+7)&~7)
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    // header
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const((OBJ_STRING << HDR_KIND_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    // copy bytes: dst=ptr+8, src=IO_INT_BUF+pos, len=len
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(IO_INT_BUF as i32));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    f
}

/// $f64_to_str_obj(val: f64) -> i32
fn emit_f64_to_str_obj(rt: &RuntimeFuncIndices) -> Function {
    // Same pattern as i64_to_str_obj but using float_to_str
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: packed
        (1, ValType::I32), // 2: pos
        (1, ValType::I32), // 3: len
        (1, ValType::I32), // 4: ptr
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(IO_FLOAT_BUF as i32));
    f.instruction(&Instruction::Call(rt.float_to_str));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0xFFFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const((OBJ_STRING << HDR_KIND_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(IO_FLOAT_BUF as i32));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    f
}

/// $list_take(list: i32, n: i32) -> i32
/// Takes first n elements from list. Returns new list.
fn emit_list_take(rt: &RuntimeFuncIndices) -> Function {
    // params: list=0, n=1. locals: result=2, ptr=3
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: result (accumulator, built in reverse then reversed)
        (1, ValType::I32), // 3: ptr (current position)
    ]);
    // Simple approach: traverse list, prepend to accumulator, reverse at end
    f.instruction(&Instruction::I32Const(0)); // empty list
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if n <= 0 or ptr == 0 → break
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // head = obj_field(ptr, 0)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    // result = list_cons(head, result)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(2));
    // ptr = tail
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(3));
    // n--
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // Reverse accumulated list
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_reverse));
    f.instruction(&Instruction::End);
    f
}

/// $list_drop(list: i32, n: i32) -> i32
fn emit_list_drop() -> Function {
    // params: list=0, n=1
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // list = tail
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg { offset: 16, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::End);
    f
}

/// $list_concat(a: i32, b: i32) -> i32
/// Concatenates two lists. Returns new list.
fn emit_list_concat(rt: &RuntimeFuncIndices) -> Function {
    // Reverse a, then prepend each element to b
    // params: a=0, b=1. locals: rev=2, ptr=3
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: reversed a
        (1, ValType::I32), // 3: ptr
    ]);
    // rev = reverse(a)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(rt.list_reverse));
    f.instruction(&Instruction::LocalSet(2));
    // result = b
    // Traverse rev, prepend each to result (b)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // head
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    // b = cons(head, b)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(1));
    // ptr = tail
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    f
}

/// $list_reverse(list: i32) -> i32
fn emit_list_reverse(rt: &RuntimeFuncIndices) -> Function {
    // params: list=0. locals: acc=1, ptr=2
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: acc
        (1, ValType::I32), // 2: ptr
    ]);
    f.instruction(&Instruction::I32Const(0)); // empty
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // head
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    // acc = cons(head, acc)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(1));
    // ptr = tail
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    f
}

/// $list_contains(list: i32, val: i64) -> i32
/// Returns 1 if list contains val (by i64 equality), 0 otherwise.
fn emit_list_contains(rt: &RuntimeFuncIndices) -> Function {
    // params: list=0, val=1(i64)
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // head == val?
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // list = tail
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(0));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0)); // not found
    f.instruction(&Instruction::End);
    f
}

/// $list_zip(a: i32, b: i32) -> i32
/// Zips two lists into list of tuples. Stops at shorter list.
fn emit_list_zip(rt: &RuntimeFuncIndices) -> Function {
    // params: a=0, b=1. locals: acc=2, head_a=3(i64), head_b=4(i64), ptr=5
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: acc
        (1, ValType::I64), // 3: head_a
        (1, ValType::I64), // 4: head_b
        (1, ValType::I32), // 5: tuple_ptr
    ]);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2)); // acc = empty
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // head_a, head_b
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::LocalSet(4));
    // alloc tuple(2): header + 2 fields
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I64Const(make_header(OBJ_TUPLE, 0, 0, 2) as i64));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 8, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 16, align: 3, memory_index: 0 }));
    // acc = cons(tuple_ptr_as_i64, acc)
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(2));
    // a = tail, b = tail
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Reverse acc
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_reverse));
    f.instruction(&Instruction::End);
    f
}

// ---------------------------------------------------------------------------
// Vector operations (flat array in linear memory)
// ---------------------------------------------------------------------------

/// $vec_from_list(list: i32) -> i32
/// Converts a linked list to a flat vector.
fn emit_vec_from_list(rt: &RuntimeFuncIndices) -> Function {
    // params: list=0. locals: len=1, ptr=2, vec=3, i=4, cur=5
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: len
        (1, ValType::I32), // 2: ptr (traversal)
        (1, ValType::I32), // 3: vec
        (1, ValType::I32), // 4: i
        (1, ValType::I32), // 5: cur
    ]);
    // First pass: count length
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Alloc: 8 + len * 8
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(3));
    // Header
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Const((OBJ_VECTOR << HDR_KIND_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    // Second pass: copy elements
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4)); // i = 0
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(5)); // cur = list
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // vec[i] = head
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // head as i64
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 8, align: 3, memory_index: 0 }));
    // i++, cur = tail
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    f
}

/// $vec_get(vec: i32, idx: i64) -> i32  (returns Option: wrapper or NONE)
fn emit_vec_get(rt: &RuntimeFuncIndices) -> Function {
    // params: vec=0, idx=1(i64). locals: len=2, i=3
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: len
        (1, ValType::I32), // 3: i (i32 index)
    ]);
    // len = header & 0xFFFFFFFF
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(2));
    // i = i32(idx)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));
    // Bounds check: i < 0 || i >= len → None
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::Else);
    // In bounds: return Some(vec[i])
    f.instruction(&Instruction::I32Const(WRAP_SOME as i32));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg { offset: 8, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::Call(rt.wrap)); // wrap(SOME, value_i64) → i32
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $vec_len(vec: i32) -> i64
fn emit_vec_len() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::End);
    f
}

/// $vec_set(vec: i32, idx: i64, val: i64) -> i32 (returns Option<Vector>)
/// Creates a NEW vector with the element at idx replaced.
fn emit_vec_set(rt: &RuntimeFuncIndices) -> Function {
    // params: vec=0, idx=1(i64), val=2(i64). locals: len=3, new_vec=4, i=5, bytes=6
    let mut f = Function::new(vec![
        (1, ValType::I32), // 3: len
        (1, ValType::I32), // 4: new_vec
        (1, ValType::I32), // 5: i (i32 idx)
        (1, ValType::I32), // 6: bytes
    ]);
    // len
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(5));
    // Bounds check
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::Else);
    // Copy entire vector
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(6)); // bytes = 8 + len*8
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    // memcpy
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
    // Update element at idx
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 8, align: 3, memory_index: 0 }));
    // Return Some(new_vec)
    f.instruction(&Instruction::I32Const(WRAP_SOME as i32));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::Call(rt.wrap));
    f.instruction(&Instruction::End); // else
    f.instruction(&Instruction::End);
    f
}

/// $vec_new(size: i64, fill: i64) -> i32
fn emit_vec_new(rt: &RuntimeFuncIndices) -> Function {
    // params: size=0(i64), fill=1(i64). locals: len=2, vec=3, i=4
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: len
        (1, ValType::I32), // 3: vec
        (1, ValType::I32), // 4: i
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(2));
    // alloc
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(3));
    // header
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Const((OBJ_VECTOR << HDR_KIND_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    // Fill
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 8, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    f
}

/// $print_value(val: i64) -> ()
/// Generic value printer. Checks if val looks like a heap pointer and dispatches.
fn emit_print_value(rt: &RuntimeFuncIndices, strs: &RtStrings) -> Function {
    // params: val=0(i64). locals: ptr=1(i32), kind=2(i32), count=3(i32), i=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: ptr
        (1, ValType::I32), // 2: kind
        (1, ValType::I32), // 3: count
        (1, ValType::I32), // 4: i (loop counter)
    ]);

    // ptr = i32.wrap(val)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(1));

    // Check: is val in heap range? ptr >= IO_SCRATCH_SIZE and ptr > 0
    // If val sign-extended from i32 equals val, it's a valid i32 pointer
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Eq);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(IO_SCRATCH_SIZE as i32));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

    // It's a heap pointer — dispatch on obj_kind
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.obj_kind));
    f.instruction(&Instruction::LocalSet(2));

    // OBJ_STRING (0)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // Print quoted string: "..."
    emit_scratch_byte(&mut f, rt, b'"');
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.print_string));
    emit_scratch_byte(&mut f, rt, b'"');
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // OBJ_TUPLE (5) — print (a, b, ...)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_TUPLE as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_scratch_byte(&mut f, rt, b'(');
    // count = header & 0xFFFFFFFF
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    // Print ", " separator (except first)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(strs.comma_space.0 as i32));
    f.instruction(&Instruction::I32Const(strs.comma_space.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    // Print element: obj_field(ptr, i)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg { offset: 8, align: 3, memory_index: 0 }));
    f.instruction(&Instruction::Call(rt.print_value)); // recursive!
    // i++
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    emit_scratch_byte(&mut f, rt, b')');
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End); // OBJ_TUPLE

    // OBJ_WRAPPER / OBJ_WRAPPER_F64 / OBJ_WRAPPER_I32 — delegate to print_heap
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_I32 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.print_heap));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // OBJ_LIST_CONS / OBJ_LIST_CONS_F64 — delegate to print_heap
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.print_heap));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // OBJ_VARIANT — print "Variant(...)"
    // For now just delegate to print_heap
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.print_heap));
    f.instruction(&Instruction::Return);

    f.instruction(&Instruction::End); // if (heap pointer)

    // Not a heap pointer — print as integer
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(rt.print_i64));
    f.instruction(&Instruction::End);
    f
}

/// Helper: write single byte to scratch area and fd_write_buf
fn emit_scratch_byte(f: &mut Function, rt: &RuntimeFuncIndices, byte: u8) {
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(byte as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0, align: 0, memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
}

// ---------------------------------------------------------------------------
// Map operations (Map = association list of (key_ptr, value_i64) tuples)
// ---------------------------------------------------------------------------

/// $map_get(map: i32, key: i32) -> i32  (returns Option ptr: wrapper or NONE_SENTINEL)
/// Searches association list for key using str_eq. Returns Option.Some(value) or Option.None.
fn emit_map_get(rt: &RuntimeFuncIndices) -> Function {
    // params: map=0, key=1. locals: ptr=2, entry=3(i32), entry_key=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: ptr (current cons cell)
        (1, ValType::I32), // 3: entry (tuple ptr)
        (1, ValType::I32), // 4: entry_key
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1)); // empty → not found
    // entry = head of cons cell (stored as i64, wrap to i32 ptr)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // i64
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3)); // entry tuple ptr
    // entry_key = tuple field 0 (i64 → i32)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // i64
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4));
    // Compare keys: str_eq(entry_key, key)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.str_eq));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // Found! Return Option.Some(value)
    // value = tuple field 1 (i64)
    f.instruction(&Instruction::I32Const(WRAP_SOME as i32));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field)); // value as i64
    f.instruction(&Instruction::Call(rt.wrap)); // wrap_i64(SOME, value) → i32 ptr
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // Next: ptr = tail
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // Not found → return None sentinel
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::End);
    f
}

/// $map_set(map: i32, key: i32, value: i64) -> i32
/// Prepends new (key, value) entry to the association list.
/// (Does NOT remove old entries with same key — get returns first match.)
fn emit_map_set(rt: &RuntimeFuncIndices) -> Function {
    // params: map=0, key=1, value=2(i64). locals: tuple_ptr=3
    let mut f = Function::new(vec![
        (1, ValType::I32), // 3: tuple_ptr
    ]);
    // Alloc tuple: header + 2 fields
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(3));
    // Header
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Const(make_header(OBJ_TUPLE, 0, 0, 2) as i64));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 }));
    // Field 0: key (i32 → i64)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 8, align: 3, memory_index: 0 }));
    // Field 1: value (i64)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg { offset: 16, align: 3, memory_index: 0 }));
    // Cons(tuple, map): tuple as i64
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::End);
    f
}

/// $map_has(map: i32, key: i32) -> i32  (bool)
fn emit_map_has(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: ptr
        (1, ValType::I32), // 3: entry_key
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // entry key
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::I32WrapI64); // tuple ptr
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::I32WrapI64); // key ptr
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.str_eq));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // next
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    f
}

/// $map_keys(map: i32) -> i32  (list of key ptrs)
fn emit_map_keys(rt: &RuntimeFuncIndices) -> Function {
    // params: map=0. locals: ptr=1, acc=2, key=3(i64)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: ptr
        (1, ValType::I32), // 2: acc (reversed list)
        (1, ValType::I64), // 3: key (i64 from tuple)
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(0)); // empty acc
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // key = entry.field0 (tuple.field0)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // tuple as i64
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // key as i64
    f.instruction(&Instruction::LocalSet(3));
    // acc = cons(key, acc)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(2));
    // next
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // reverse
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_reverse));
    f.instruction(&Instruction::End);
    f
}

/// Emit wrapper prefix based on tag local: "Result.Ok(" / "Result.Err(" / "Option.Some("
fn emit_wrapper_prefix(
    f: &mut Function,
    rt: &RuntimeFuncIndices,
    strs: &RtStrings,
    tag_local: u32,
) {
    // tag == 0 → Ok
    f.instruction(&Instruction::LocalGet(tag_local));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(strs.result_ok.0 as i32));
    f.instruction(&Instruction::I32Const(strs.result_ok.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Else);
    // tag == 1 → Err
    f.instruction(&Instruction::LocalGet(tag_local));
    f.instruction(&Instruction::I32Const(WRAP_ERR as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(strs.result_err.0 as i32));
    f.instruction(&Instruction::I32Const(strs.result_err.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Else);
    // tag == 2 → Some
    f.instruction(&Instruction::I32Const(strs.option_some.0 as i32));
    f.instruction(&Instruction::I32Const(strs.option_some.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
}
