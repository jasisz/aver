//! Construct/project bridges for proof-packed structural refinements.
//!
//! A packed nominal value is an array in wasm, while Aver code inside the
//! defining module still constructs and projects its declared `List<Int>`
//! carrier. Two small per-type helpers make that boundary explicit. They are
//! generic over the proof-selected integer width and never key on `Bytes`.

use std::collections::HashMap;

use wasm_encoder::{
    BlockType, CodeSection, Function, FunctionSection, HeapType, Instruction, RefType, TypeSection,
    ValType,
};

use super::WasmGcError;
use super::types::TypeRegistry;
use crate::codegen::proof_lower::PackedIntElement;

#[derive(Debug, Clone, Copy)]
pub(super) struct PackedSequenceOps {
    pub(super) pack: u32,
    pub(super) unpack: u32,
}

#[derive(Default)]
pub(super) struct PackedSequenceHelperRegistry {
    ops: HashMap<String, PackedSequenceOps>,
    type_indices: HashMap<String, (u32, u32)>,
    order: Vec<String>,
}

impl PackedSequenceHelperRegistry {
    pub(super) fn assign_slots(
        &mut self,
        registry: &TypeRegistry,
        next_fn_idx: &mut u32,
        next_type_idx: &mut u32,
    ) {
        for name in &registry.packed_sequence_order {
            let pack_type = *next_type_idx;
            *next_type_idx += 1;
            let unpack_type = *next_type_idx;
            *next_type_idx += 1;
            let pack = *next_fn_idx;
            *next_fn_idx += 1;
            let unpack = *next_fn_idx;
            *next_fn_idx += 1;
            self.ops
                .insert(name.clone(), PackedSequenceOps { pack, unpack });
            self.type_indices
                .insert(name.clone(), (pack_type, unpack_type));
            self.order.push(name.clone());
        }
    }

    /// Exact-name lookup only — no qualified→bare fallback, mirroring
    /// `TypeRegistry::packed_sequence`. A bare-name fallback would route
    /// a collision-renamed dep type through an unrelated pack/unpack pair.
    pub(super) fn ops_for(&self, type_name: &str) -> Option<PackedSequenceOps> {
        self.ops.get(type_name).copied()
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = (&str, PackedSequenceOps)> + '_ {
        self.order
            .iter()
            .map(|name| (name.as_str(), self.ops[name]))
    }

    pub(super) fn emit_helper_types(
        &self,
        types: &mut TypeSection,
        registry: &TypeRegistry,
    ) -> Result<(), WasmGcError> {
        if self.order.is_empty() {
            return Ok(());
        }
        let list_idx = registry
            .list_type_idx("List<Int>")
            .ok_or_else(|| WasmGcError::Validation("packed carrier requires List<Int>".into()))?;
        let list_ref = ref_ty(list_idx);
        for name in &self.order {
            let packed = registry
                .packed_sequence(name)
                .expect("packed helper without packed type");
            let packed_ref = ref_ty(packed.type_idx);
            types.ty().function([list_ref], [packed_ref]);
            types.ty().function([packed_ref], [list_ref]);
        }
        Ok(())
    }

    pub(super) fn emit_function_section(&self, funcs: &mut FunctionSection) {
        for name in &self.order {
            let (pack, unpack) = self.type_indices[name];
            funcs.function(pack);
            funcs.function(unpack);
        }
    }

    pub(super) fn emit_helper_bodies(
        &self,
        codes: &mut CodeSection,
        registry: &TypeRegistry,
    ) -> Result<(), WasmGcError> {
        for name in &self.order {
            codes.function(&emit_pack(name, registry)?);
            codes.function(&emit_unpack(name, registry)?);
        }
        Ok(())
    }
}

fn ref_ty(type_idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(type_idx),
    })
}

fn emit_pack(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let packed = registry
        .packed_sequence(name)
        .ok_or_else(|| WasmGcError::Validation(format!("packed type `{name}` missing")))?;
    let list_idx = registry
        .list_type_idx("List<Int>")
        .ok_or_else(|| WasmGcError::Validation("packed carrier requires List<Int>".into()))?;
    let list_ref = ref_ty(list_idx);
    let packed_ref = ref_ty(packed.type_idx);
    let mut f = Function::new([
        (1, list_ref),     // 1: cursor
        (1, ValType::I32), // 2: len
        (1, packed_ref),   // 3: output
        (1, ValType::I32), // 4: index
    ]);

    // Pass 1: count the linked list.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayNewDefault(packed.type_idx));
    f.instruction(&Instruction::LocalSet(3));

    // Pass 2: checked-unbox each Int and let the proven interval justify the
    // width narrowing performed by array.set.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    if registry.bignum {
        let to_i64 = registry.aint_to_i64_checked_fn_idx.ok_or_else(|| {
            WasmGcError::Validation("packed carrier requires __aint_to_i64_checked".into())
        })?;
        f.instruction(&Instruction::Call(to_i64));
    }
    if !matches!(packed.layout.element, PackedIntElement::I64) {
        f.instruction(&Instruction::I32WrapI64);
    }
    f.instruction(&Instruction::ArraySet(packed.type_idx));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_unpack(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let packed = registry
        .packed_sequence(name)
        .ok_or_else(|| WasmGcError::Validation(format!("packed type `{name}` missing")))?;
    let list_idx = registry
        .list_type_idx("List<Int>")
        .ok_or_else(|| WasmGcError::Validation("packed carrier requires List<Int>".into()))?;
    let list_ref = ref_ty(list_idx);
    let mut f = Function::new([
        (1, list_ref),     // 1: accumulator
        (1, ValType::I32), // 2: reverse index
    ]);
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(2));
    match packed.layout.element {
        PackedIntElement::U8 | PackedIntElement::U16 => {
            f.instruction(&Instruction::ArrayGetU(packed.type_idx));
            f.instruction(&Instruction::I64ExtendI32U);
        }
        PackedIntElement::I8 | PackedIntElement::I16 => {
            f.instruction(&Instruction::ArrayGetS(packed.type_idx));
            f.instruction(&Instruction::I64ExtendI32S);
        }
        PackedIntElement::U32 => {
            f.instruction(&Instruction::ArrayGet(packed.type_idx));
            f.instruction(&Instruction::I64ExtendI32U);
        }
        PackedIntElement::I32 => {
            f.instruction(&Instruction::ArrayGet(packed.type_idx));
            f.instruction(&Instruction::I64ExtendI32S);
        }
        PackedIntElement::I64 => {
            f.instruction(&Instruction::ArrayGet(packed.type_idx));
        }
    }
    if registry.bignum {
        let from_i64 = registry.aint_from_i64_fn_idx.ok_or_else(|| {
            WasmGcError::Validation("packed carrier requires __aint_from_i64".into())
        })?;
        f.instruction(&Instruction::Call(from_i64));
    }
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    Ok(f)
}
