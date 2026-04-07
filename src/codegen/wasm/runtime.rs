/// Emits inline WASM runtime functions (bump allocator, tagged arithmetic, object helpers).
///
/// These are emitted directly into the generated module — no external imports needed.
/// Each function is a few WASM instructions.
use wasm_encoder::{Function, Instruction, ValType};

use super::value::*;

/// Index assignments for runtime functions within the module.
/// These come before user-defined functions in the function index space.
#[derive(Debug, Clone, Copy)]
pub struct RuntimeFuncIndices {
    pub alloc: u32,
    pub int_add: u32,
    pub int_sub: u32,
    pub int_mul: u32,
    pub int_div: u32,
    pub int_eq: u32,
    pub int_lt: u32,
    pub int_gt: u32,
    pub int_le: u32,
    pub int_ge: u32,
    pub int_ne: u32,
    pub wrap: u32,
    pub unwrap: u32,
    pub obj_kind: u32,
    pub obj_tag: u32,
    pub obj_field: u32,
    pub list_cons: u32,
    /// Total number of runtime functions.
    pub count: u32,
}

impl RuntimeFuncIndices {
    /// Create index assignments starting at `base`.
    pub fn new(base: u32) -> Self {
        let mut i = base;
        let mut next = || {
            let idx = i;
            i += 1;
            idx
        };
        RuntimeFuncIndices {
            alloc: next(),
            int_add: next(),
            int_sub: next(),
            int_mul: next(),
            int_div: next(),
            int_eq: next(),
            int_lt: next(),
            int_gt: next(),
            int_le: next(),
            int_ge: next(),
            int_ne: next(),
            wrap: next(),
            unwrap: next(),
            obj_kind: next(),
            obj_tag: next(),
            obj_field: next(),
            list_cons: next(),
            count: i - base,
        }
    }
}

/// Type index assignments for runtime function signatures.
pub struct RuntimeTypeIndices {
    /// () -> i32
    pub alloc_type: u32,
    /// (i64, i64) -> i64
    pub binop_type: u32,
    /// (i32, i64) -> i64  — wrap(tag, inner)
    pub wrap_type: u32,
    /// (i64) -> i64  — unwrap(value)
    pub unop_type: u32,
    /// (i64) -> i32  — obj_kind/obj_tag
    pub inspect_type: u32,
    /// (i64, i32) -> i64  — obj_field(value, idx)
    pub field_type: u32,
}

/// Emit all runtime function bodies. Returns them in the same order as RuntimeFuncIndices.
pub fn emit_runtime_functions(rt: &RuntimeFuncIndices) -> Vec<Function> {
    let mut funcs = Vec::new();

    // $alloc(size: i32) -> i32
    funcs.push(emit_alloc());

    // Arithmetic: $int_add, $int_sub, $int_mul, $int_div
    funcs.push(emit_int_binop(Instruction::I64Add));
    funcs.push(emit_int_binop(Instruction::I64Sub));
    funcs.push(emit_int_binop(Instruction::I64Mul));
    funcs.push(emit_int_div());

    // Comparisons: $int_eq, $int_lt, $int_gt, $int_le, $int_ge, $int_ne
    funcs.push(emit_int_cmp(Instruction::I64Eq));
    funcs.push(emit_int_cmp(Instruction::I64LtS));
    funcs.push(emit_int_cmp(Instruction::I64GtS));
    funcs.push(emit_int_cmp(Instruction::I64LeS));
    funcs.push(emit_int_cmp(Instruction::I64GeS));
    funcs.push(emit_int_cmp(Instruction::I64Ne));

    // $wrap(tag: i32, inner: i64) -> i64
    funcs.push(emit_wrap(rt));

    // $unwrap(value: i64) -> i64
    funcs.push(emit_unwrap());

    // $obj_kind(value: i64) -> i32
    funcs.push(emit_obj_kind());

    // $obj_tag(value: i64) -> i32
    funcs.push(emit_obj_tag());

    // $obj_field(value: i64, idx: i32) -> i64
    funcs.push(emit_obj_field());

    // $list_cons(head: i64, tail: i64) -> i64
    funcs.push(emit_list_cons(rt));

    funcs
}

// ---------------------------------------------------------------------------
// Individual runtime function emitters
// ---------------------------------------------------------------------------

/// $alloc(size: i32) -> i32
/// Bump allocator: reads $heap_ptr global, advances it, grows memory if needed.
fn emit_alloc() -> Function {
    // Globals: 0 = $heap_ptr, 1 = $scratch_ptr
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: $ptr
    // $ptr = $heap_ptr
    f.instruction(&Instruction::GlobalGet(0));
    f.instruction(&Instruction::LocalSet(1)); // param is 0, local is 1
    // $heap_ptr += size (aligned to 8)
    f.instruction(&Instruction::GlobalGet(0));
    f.instruction(&Instruction::LocalGet(0)); // size param
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32)); // 0xFFFFFFF8 — align mask
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::GlobalSet(0));
    // return $ptr
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    f
}

/// Tagged int binary operation: extract 60-bit signed payloads, apply op, repack.
///
/// ```wasm
/// ;; extract a: (a << 4) >> 4 (arithmetic shift = sign-extend)
/// local.get $a
/// i64.const 4
/// i64.shl
/// i64.const 4
/// i64.shr_s
/// ;; extract b: same
/// local.get $b
/// i64.const 4
/// i64.shl
/// i64.const 4
/// i64.shr_s
/// ;; apply op
/// i64.add (or sub/mul)
/// ;; repack: mask to 60 bits, OR with tag=0
/// i64.const PAYLOAD_MASK
/// i64.and
/// ```
fn emit_int_binop(op: Instruction<'static>) -> Function {
    let mut f = Function::new(vec![]);
    // Extract a
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64ShrS);
    // Extract b
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64ShrS);
    // Apply op
    f.instruction(&op);
    // Repack (tag=0, so just mask payload)
    f.instruction(&Instruction::I64Const(PAYLOAD_MASK as i64));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::End);
    f
}

/// Tagged int division with div-by-zero check → returns CONST_UNIT on zero.
fn emit_int_div() -> Function {
    let mut f = Function::new(vec![(2, ValType::I64)]); // locals: $a_val, $b_val
    // Extract a
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64ShrS);
    f.instruction(&Instruction::LocalSet(2)); // $a_val
    // Extract b
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64ShrS);
    f.instruction(&Instruction::LocalSet(3)); // $b_val
    // Check b == 0
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I64,
    )));
    // div by zero → Unit
    f.instruction(&Instruction::I64Const(CONST_UNIT as i64));
    f.instruction(&Instruction::Else);
    // normal division
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64DivS);
    f.instruction(&Instruction::I64Const(PAYLOAD_MASK as i64));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::End); // end if
    f.instruction(&Instruction::End); // end func
    f
}

/// Tagged int comparison: extract payloads, compare, return CONST_TRUE/CONST_FALSE.
fn emit_int_cmp(op: Instruction<'static>) -> Function {
    let mut f = Function::new(vec![]);
    // Extract a
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64ShrS);
    // Extract b
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Const(4));
    f.instruction(&Instruction::I64ShrS);
    // Compare
    f.instruction(&op);
    // Convert i32 bool → Value (CONST_TRUE or CONST_FALSE)
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I64,
    )));
    f.instruction(&Instruction::I64Const(CONST_TRUE as i64));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I64Const(CONST_FALSE as i64));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $wrap(tag: i32, inner: i64) -> i64
/// Allocates a Wrapper object on heap: [header][inner_value]
fn emit_wrap(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: $ptr
    // alloc(16) — 8 byte header + 8 byte field
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(2)); // $ptr
    // Store header: kind=OBJ_WRAPPER, variant_tag=param0, type_id=0, field_count=1
    f.instruction(&Instruction::LocalGet(2));
    // Build header inline
    f.instruction(&Instruction::I64Const(
        (OBJ_WRAPPER << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(0)); // tag param (i32)
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Const(1)); // field_count = 1
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3, // 8-byte aligned
        memory_index: 0,
    }));
    // Store field[0] = inner value
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1)); // inner param
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    // Return HeapRef
    f.instruction(&Instruction::I64Const((TAG_HEAP << TAG_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::End);
    f
}

/// $unwrap(value: i64) -> i64
/// Reads field[0] from a HeapRef object.
fn emit_unwrap() -> Function {
    let mut f = Function::new(vec![]);
    // Extract handle from HeapRef
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(PAYLOAD_MASK as i64));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    // Load field[0] at offset 8
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $obj_kind(value: i64) -> i32
/// Reads the object kind from the header of a HeapRef.
fn emit_obj_kind() -> Function {
    let mut f = Function::new(vec![]);
    // Extract handle
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(PAYLOAD_MASK as i64));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    // Load header
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // Extract kind: header >> 56
    f.instruction(&Instruction::I64Const(HDR_KIND_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::End);
    f
}

/// $obj_tag(value: i64) -> i32
/// Reads the variant/wrapper tag from the header of a HeapRef.
fn emit_obj_tag() -> Function {
    let mut f = Function::new(vec![]);
    // Extract handle
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(PAYLOAD_MASK as i64));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    // Load header
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // Extract tag: (header >> 48) & 0xFF
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::End);
    f
}

/// $obj_field(value: i64, idx: i32) -> i64
/// Reads field[idx] from a HeapRef object (at offset 8 + idx*8).
fn emit_obj_field() -> Function {
    let mut f = Function::new(vec![]);
    // Extract handle
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(PAYLOAD_MASK as i64));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    // Compute field offset: handle + 8 + idx*8
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1)); // idx
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    // Load field
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $list_cons(head: i64, tail: i64) -> i64
/// Allocates a Cons cell: [header: OBJ_LIST_CONS, field_count=2][head][tail]
fn emit_list_cons(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: $ptr
    // alloc(24) — 8 header + 2×8 fields
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(2)); // $ptr
    // Store header
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(
        make_header(OBJ_LIST_CONS, 0, 0, 2) as i64
    ));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // Store head
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(0)); // head param
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    // Store tail
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1)); // tail param
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    // Return HeapRef
    f.instruction(&Instruction::I64Const((TAG_HEAP << TAG_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::End);
    f
}
