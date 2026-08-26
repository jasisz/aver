//! Bulk linear-memory transport for `Bytes` values.
//!
//! WebAssembly GC arrays are opaque to JavaScript. Without this bridge a raw
//! host has to construct or inspect `Bytes` through one exported call per
//! octet, first materialising a linked `List<Int>` and then asking the guest
//! to repack it. The bridge uses the same LM[0..len] scratch buffer as String.
//! Proof-packed Bytes need one guest array allocation; the boxed fallback
//! still builds its list inside wasm, avoiding a JS↔wasm call per octet.

use wasm_encoder::{
    BlockType, CodeSection, ExportKind, ExportSection, Function, FunctionSection, HeapType,
    Instruction, MemArg, RefType, TypeSection, ValType,
};

use super::types::{RESULT_OK_TAG, TypeRegistry};
use super::{WasmGcError, wat_helper};
use crate::codegen::proof_lower::PackedIntElement;

#[derive(Debug, Clone, Copy)]
struct OptionalSlot {
    type_idx: u32,
    fn_idx: u32,
}

#[derive(Debug, Clone, Copy)]
enum BytesRepresentation {
    Packed {
        type_idx: u32,
    },
    Boxed {
        bytes_idx: u32,
        list_idx: u32,
        from_i64: Option<u32>,
        to_i64: Option<u32>,
    },
}

impl BytesRepresentation {
    fn type_idx(self) -> u32 {
        match self {
            Self::Packed { type_idx } => type_idx,
            Self::Boxed { bytes_idx, .. } => bytes_idx,
        }
    }
}

/// Slots for the raw-host exports. The Result helper is present only when the
/// compiled graph actually carries `Result<Bytes, String>`.
pub(super) struct BytesBridge {
    representation: BytesRepresentation,
    from_lm_type: u32,
    to_lm_type: u32,
    from_lm_fn: u32,
    to_lm_fn: u32,
    result_ok_from_lm: Option<OptionalSlot>,
}

impl BytesBridge {
    pub(super) fn allocate(
        types: &mut TypeSection,
        next_type_idx: &mut u32,
        next_fn_idx: &mut u32,
        registry: &TypeRegistry,
    ) -> Result<Option<Self>, WasmGcError> {
        let representation = if let Some(bytes) = registry.packed_sequence("Bytes") {
            if !matches!(bytes.layout.element, PackedIntElement::U8) {
                return Err(WasmGcError::Validation(
                    "raw-host Bytes bridge requires the proof-packed U8 layout".into(),
                ));
            }
            BytesRepresentation::Packed {
                type_idx: bytes.type_idx,
            }
        } else if let Some(bytes_idx) = registry.record_type_idx("Bytes") {
            let canonical = registry.canonical_type_name("Bytes");
            let fields = registry
                .record_fields
                .get(canonical)
                .or_else(|| registry.record_fields.get("Bytes"));
            if !matches!(fields, Some(fields) if fields.len() == 1 && fields[0].1.replace(' ', "") == "List<Int>")
            {
                return Err(WasmGcError::Validation(
                    "raw-host Bytes bridge requires one List<Int> carrier field".into(),
                ));
            }
            let list_idx = registry.list_type_idx("List<Int>").ok_or_else(|| {
                WasmGcError::Validation("raw-host boxed Bytes bridge requires List<Int>".into())
            })?;
            let (from_i64, to_i64) = if registry.bignum {
                (
                    Some(registry.aint_from_i64_fn_idx.ok_or_else(|| {
                        WasmGcError::Validation(
                            "raw-host boxed Bytes bridge requires __aint_from_i64".into(),
                        )
                    })?),
                    Some(registry.aint_to_i64_checked_fn_idx.ok_or_else(|| {
                        WasmGcError::Validation(
                            "raw-host boxed Bytes bridge requires __aint_to_i64_checked".into(),
                        )
                    })?),
                )
            } else {
                (None, None)
            };
            BytesRepresentation::Boxed {
                bytes_idx,
                list_idx,
                from_i64,
                to_i64,
            }
        } else {
            return Ok(None);
        };
        let bytes_ref = ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(representation.type_idx()),
        });

        types.ty().function([ValType::I32], [bytes_ref]);
        let from_lm_type = *next_type_idx;
        *next_type_idx += 1;
        let from_lm_fn = *next_fn_idx;
        *next_fn_idx += 1;

        types.ty().function([bytes_ref], [ValType::I32]);
        let to_lm_type = *next_type_idx;
        *next_type_idx += 1;
        let to_lm_fn = *next_fn_idx;
        *next_fn_idx += 1;

        let result_ok_from_lm =
            registry
                .result_type_idx("Result<Bytes,String>")
                .map(|result_idx| {
                    let result_ref = ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Concrete(result_idx),
                    });
                    types.ty().function([ValType::I32], [result_ref]);
                    let slot = OptionalSlot {
                        type_idx: *next_type_idx,
                        fn_idx: *next_fn_idx,
                    };
                    *next_type_idx += 1;
                    *next_fn_idx += 1;
                    slot
                });

        Ok(Some(Self {
            representation,
            from_lm_type,
            to_lm_type,
            from_lm_fn,
            to_lm_fn,
            result_ok_from_lm,
        }))
    }

    pub(super) fn emit_function_entries(&self, functions: &mut FunctionSection) {
        functions.function(self.from_lm_type);
        functions.function(self.to_lm_type);
        if let Some(slot) = self.result_ok_from_lm {
            functions.function(slot.type_idx);
        }
    }

    pub(super) fn emit_exports(&self, exports: &mut ExportSection) {
        exports.export("__rt_bytes_from_lm", ExportKind::Func, self.from_lm_fn);
        exports.export("__rt_bytes_to_lm", ExportKind::Func, self.to_lm_fn);
        if let Some(slot) = self.result_ok_from_lm {
            exports.export(
                "__rt_result_bytes_string_ok_from_lm",
                ExportKind::Func,
                slot.fn_idx,
            );
        }
    }

    pub(super) fn emit_bodies(
        &self,
        codes: &mut CodeSection,
        registry: &TypeRegistry,
    ) -> Result<(), WasmGcError> {
        match self.representation {
            BytesRepresentation::Packed { type_idx } => {
                codes.function(&emit_from_lm_packed(type_idx)?);
                codes.function(&emit_to_lm_packed(type_idx)?);
            }
            BytesRepresentation::Boxed {
                bytes_idx,
                list_idx,
                from_i64,
                to_i64,
            } => {
                codes.function(&emit_from_lm_boxed(bytes_idx, list_idx, from_i64));
                codes.function(&emit_to_lm_boxed(bytes_idx, list_idx, to_i64));
            }
        }
        if self.result_ok_from_lm.is_some() {
            codes.function(&emit_result_ok_from_lm(registry, self.from_lm_fn)?);
        }
        Ok(())
    }
}

fn emit_from_lm_packed(bytes_idx: u32) -> Result<Function, WasmGcError> {
    let padding = wat_helper::padding_types(bytes_idx);
    let wat = format!(
        r#"
        (module
          {padding}
          (type $bytes (array (mut i8)))
          (memory 1)
          (func (export "helper") (param $len i32) (result (ref null $bytes))
            (local $bytes (ref null $bytes))
            (local $i i32)
            local.get $len
            array.new_default $bytes
            local.set $bytes
            i32.const 0
            local.set $i
            (block $break
              (loop $next
                local.get $i
                local.get $len
                i32.ge_u
                br_if $break

                local.get $bytes
                local.get $i
                local.get $i
                i32.load8_u
                array.set $bytes

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $next))
            local.get $bytes)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

fn emit_to_lm_packed(bytes_idx: u32) -> Result<Function, WasmGcError> {
    let padding = wat_helper::padding_types(bytes_idx);
    let wat = format!(
        r#"
        (module
          {padding}
          (type $bytes (array (mut i8)))
          (memory 1)
          (func (export "helper") (param $bytes (ref null $bytes)) (result i32)
            (local $len i32)
            (local $i i32)
            (local $needed i32)
            (local $current i32)
            local.get $bytes
            array.len
            local.set $len

            local.get $len
            i32.const 65535
            i32.add
            i32.const 16
            i32.shr_u
            local.set $needed

            memory.size
            local.set $current
            local.get $needed
            local.get $current
            i32.gt_u
            (if
              (then
                local.get $needed
                local.get $current
                i32.sub
                memory.grow
                drop))

            i32.const 0
            local.set $i
            (block $break
              (loop $next
                local.get $i
                local.get $len
                i32.ge_u
                br_if $break

                local.get $i
                local.get $bytes
                local.get $i
                array.get_u $bytes
                i32.store8

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $next))
            local.get $len)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

fn emit_from_lm_boxed(bytes_idx: u32, list_idx: u32, from_i64: Option<u32>) -> Function {
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    let mut function = Function::new([(1, list_ref), (1, ValType::I32)]);
    function.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    function.instruction(&Instruction::LocalSet(1));
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::LocalSet(2));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(2));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(2));
    function.instruction(&Instruction::I32Const(RESULT_OK_TAG));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalTee(2));
    function.instruction(&Instruction::I32Load8U(byte_memarg()));
    function.instruction(&Instruction::I64ExtendI32U);
    if let Some(from_i64) = from_i64 {
        function.instruction(&Instruction::Call(from_i64));
    }
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructNew(list_idx));
    function.instruction(&Instruction::LocalSet(1));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructNew(bytes_idx));
    function.instruction(&Instruction::End);
    function
}

fn emit_to_lm_boxed(bytes_idx: u32, list_idx: u32, to_i64: Option<u32>) -> Function {
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    let mut function = Function::new([(1, list_ref), (2, ValType::I32)]);

    // First pass: find the length so the helper can grow LM once.
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::StructGet {
        struct_type_index: bytes_idx,
        field_index: 0,
    });
    function.instruction(&Instruction::LocalSet(1));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(2));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::RefIsNull);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(2));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(2));
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    function.instruction(&Instruction::LocalSet(1));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Ensure LM[0..len] exists before the copy pass.
    function.instruction(&Instruction::LocalGet(2));
    function.instruction(&Instruction::I32Const(65_535));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(16));
    function.instruction(&Instruction::I32ShrU);
    function.instruction(&Instruction::MemorySize(0));
    function.instruction(&Instruction::I32GtU);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(2));
    function.instruction(&Instruction::I32Const(65_535));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(16));
    function.instruction(&Instruction::I32ShrU);
    function.instruction(&Instruction::MemorySize(0));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::MemoryGrow(0));
    function.instruction(&Instruction::Drop);
    function.instruction(&Instruction::End);

    // Second pass: checked-unbox each proven octet and store it in LM.
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::StructGet {
        struct_type_index: bytes_idx,
        field_index: 0,
    });
    function.instruction(&Instruction::LocalSet(1));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(3));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::RefIsNull);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(3));
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    if let Some(to_i64) = to_i64 {
        function.instruction(&Instruction::Call(to_i64));
    }
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::I32Store8(byte_memarg()));
    function.instruction(&Instruction::LocalGet(3));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(3));
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    function.instruction(&Instruction::LocalSet(1));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(2));
    function.instruction(&Instruction::End);
    function
}

fn byte_memarg() -> MemArg {
    MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }
}

fn emit_result_ok_from_lm(
    registry: &TypeRegistry,
    from_lm_fn: u32,
) -> Result<Function, WasmGcError> {
    let result_idx = registry
        .result_type_idx("Result<Bytes,String>")
        .expect("checked at allocation");
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "Result<Bytes, String> bridge requires String".into(),
        ))?;
    let mut function = Function::new([]);
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::Call(from_lm_fn));
    function.instruction(&Instruction::RefNull(HeapType::Concrete(string_idx)));
    function.instruction(&Instruction::StructNew(result_idx));
    function.instruction(&Instruction::End);
    Ok(function)
}
