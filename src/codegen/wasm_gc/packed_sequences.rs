//! Construct/project bridges for proof-packed structural refinements.
//!
//! A packed nominal value is an array in wasm, while Aver code inside the
//! defining module still constructs and projects its declared `List<Int>`
//! carrier. Per-type helpers make that boundary explicit and keep preserving
//! `concat` / `take` / `drop` pipelines packed. They are generic over the
//! proof-selected integer width and never key on `Bytes`.

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
    pub(super) concat: u32,
    pub(super) take: u32,
    pub(super) drop: u32,
    /// `get(packed, index) -> List<Int>`: the one-element list holding the
    /// element at `index`, or the empty list out of range. Present only
    /// when a program matches a list pattern on `List.drop` of a carrier.
    pub(super) get: Option<u32>,
    /// `byte_at(packed, position) -> i64`: the element at `position`, or
    /// `-1` at a negative position and at one at or past the length (the
    /// `__packed_byte_at` intrinsic, `ir::mir::optimize::packed_index`).
    /// Present only for a `U8` layout in a program whose MIR reads one.
    pub(super) byte_at: Option<u32>,
}

#[derive(Default)]
pub(super) struct PackedSequenceHelperRegistry {
    ops: HashMap<String, PackedSequenceOps>,
    type_indices: HashMap<String, (u32, u32, u32, u32)>,
    get_type_indices: HashMap<String, u32>,
    byte_at_type_indices: HashMap<String, u32>,
    order: Vec<String>,
}

impl PackedSequenceHelperRegistry {
    pub(super) fn assign_slots(
        &mut self,
        registry: &TypeRegistry,
        with_get: bool,
        with_byte_at: bool,
        next_fn_idx: &mut u32,
        next_type_idx: &mut u32,
    ) {
        for name in &registry.packed_sequence_order {
            let pack_type = *next_type_idx;
            *next_type_idx += 1;
            let unpack_type = *next_type_idx;
            *next_type_idx += 1;
            let concat_type = *next_type_idx;
            *next_type_idx += 1;
            let slice_type = *next_type_idx;
            *next_type_idx += 1;
            let pack = *next_fn_idx;
            *next_fn_idx += 1;
            let unpack = *next_fn_idx;
            *next_fn_idx += 1;
            let concat = *next_fn_idx;
            *next_fn_idx += 1;
            let take = *next_fn_idx;
            *next_fn_idx += 1;
            let drop = *next_fn_idx;
            *next_fn_idx += 1;
            let get = with_get.then(|| {
                let get_type = *next_type_idx;
                *next_type_idx += 1;
                self.get_type_indices.insert(name.clone(), get_type);
                let get = *next_fn_idx;
                *next_fn_idx += 1;
                get
            });
            let byte_wide = registry
                .packed_sequence(name)
                .is_some_and(|packed| matches!(packed.layout.element, PackedIntElement::U8));
            let byte_at = (with_byte_at && byte_wide).then(|| {
                let byte_at_type = *next_type_idx;
                *next_type_idx += 1;
                self.byte_at_type_indices.insert(name.clone(), byte_at_type);
                let byte_at = *next_fn_idx;
                *next_fn_idx += 1;
                byte_at
            });
            self.ops.insert(
                name.clone(),
                PackedSequenceOps {
                    pack,
                    unpack,
                    concat,
                    take,
                    drop,
                    get,
                    byte_at,
                },
            );
            self.type_indices.insert(
                name.clone(),
                (pack_type, unpack_type, concat_type, slice_type),
            );
            self.order.push(name.clone());
        }
        // Register the flatten-derived qualified aliases as extra lookup
        // KEYS onto the canonical ops — never extra helpers (`order` and
        // `type_indices` stay canonical-only, so slot assignment and
        // helper emission are untouched). An alias provably denotes the
        // same `TypeDef` (sole declarer), so sharing the pack/unpack pair
        // is identity-correct.
        for (alias, canonical) in &registry.type_name_aliases {
            if let Some(ops) = self.ops.get(canonical).copied() {
                self.ops.entry(alias.clone()).or_insert(ops);
            }
        }
    }

    /// Exact-name lookup — no qualified→bare suffix fallback, mirroring
    /// `TypeRegistry::packed_sequence`. A bare-name fallback would route
    /// a collision-renamed dep type through an unrelated pack/unpack pair.
    /// The only extra keys are the flatten-derived identity-preserving
    /// aliases registered by `assign_slots`.
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
            types.ty().function([packed_ref, packed_ref], [packed_ref]);
            types
                .ty()
                .function([packed_ref, ValType::I64], [packed_ref]);
            if self.get_type_indices.contains_key(name) {
                types.ty().function([packed_ref, ValType::I64], [list_ref]);
            }
            if self.byte_at_type_indices.contains_key(name) {
                types
                    .ty()
                    .function([packed_ref, ValType::I64], [ValType::I64]);
            }
        }
        Ok(())
    }

    pub(super) fn emit_function_section(&self, funcs: &mut FunctionSection) {
        for name in &self.order {
            let (pack, unpack, concat, slice) = self.type_indices[name];
            funcs.function(pack);
            funcs.function(unpack);
            funcs.function(concat);
            funcs.function(slice);
            funcs.function(slice);
            if let Some(get) = self.get_type_indices.get(name) {
                funcs.function(*get);
            }
            if let Some(byte_at) = self.byte_at_type_indices.get(name) {
                funcs.function(*byte_at);
            }
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
            codes.function(&emit_concat(name, registry)?);
            codes.function(&emit_take(name, registry)?);
            codes.function(&emit_drop(name, registry)?);
            if self.get_type_indices.contains_key(name) {
                codes.function(&emit_get(name, registry)?);
            }
            if self.byte_at_type_indices.contains_key(name) {
                codes.function(&emit_byte_at(name, registry)?);
            }
        }
        Ok(())
    }
}

fn emit_concat(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let packed = registry
        .packed_sequence(name)
        .ok_or_else(|| WasmGcError::Validation(format!("packed type `{name}` missing")))?;
    let packed_ref = ref_ty(packed.type_idx);
    // params: 0=left, 1=right. locals: 2=left_len, 3=right_len, 4=out.
    let mut f = Function::new([(2, ValType::I32), (1, packed_ref)]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::ArrayNewDefault(packed.type_idx));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: packed.type_idx,
        array_type_index_src: packed.type_idx,
    });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: packed.type_idx,
        array_type_index_src: packed.type_idx,
    });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_take(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let packed = registry
        .packed_sequence(name)
        .ok_or_else(|| WasmGcError::Validation(format!("packed type `{name}` missing")))?;
    let packed_ref = ref_ty(packed.type_idx);
    // params: 0=input, 1=count. locals: 2=len, 3=out_len, 4=out.
    let mut f = Function::new([(2, ValType::I32), (1, packed_ref)]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayNewDefault(packed.type_idx));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: packed.type_idx,
        array_type_index_src: packed.type_idx,
    });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_drop(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let packed = registry
        .packed_sequence(name)
        .ok_or_else(|| WasmGcError::Validation(format!("packed type `{name}` missing")))?;
    let packed_ref = ref_ty(packed.type_idx);
    // params: 0=input, 1=count. locals: 2=len, 3=offset, 4=out_len, 5=out.
    let mut f = Function::new([(3, ValType::I32), (1, packed_ref)]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::ArrayNewDefault(packed.type_idx));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: packed.type_idx,
        array_type_index_src: packed.type_idx,
    });
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::End);
    Ok(f)
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

/// The first element of `List.drop(carrier, index)` as a one-element
/// `List<Int>`, or the empty list when nothing is left: all a list match
/// that ignores the tail can tell of that value, read in O(1). A negative
/// index drops nothing, as `List.drop` does, so it reads element zero.
fn emit_get(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let packed = registry
        .packed_sequence(name)
        .ok_or_else(|| WasmGcError::Validation(format!("packed type `{name}` missing")))?;
    let list_idx = registry
        .list_type_idx("List<Int>")
        .ok_or_else(|| WasmGcError::Validation("packed carrier requires List<Int>".into()))?;
    // params: 0=input, 1=index. Clamp a negative index to zero, then
    // compare it against the length.
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::Select);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::If(BlockType::Result(ref_ty(list_idx))));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    emit_read_element(&mut f, packed.type_idx, packed.layout.element);
    if registry.bignum {
        let from_i64 = registry.aint_from_i64_fn_idx.ok_or_else(|| {
            WasmGcError::Validation("packed carrier requires __aint_from_i64".into())
        })?;
        f.instruction(&Instruction::Call(from_i64));
    }
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// The element at `position` of a `U8` carrier as an `i64`, or `-1` when
/// the position is negative or at or past the length: one unsigned compare
/// covers both, since a negative `i64` read unsigned is past any length.
/// No list cell and no `Option` is built.
fn emit_byte_at(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let packed = registry
        .packed_sequence(name)
        .ok_or_else(|| WasmGcError::Validation(format!("packed type `{name}` missing")))?;
    // params: 0=input, 1=position.
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I64)));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    emit_read_element(&mut f, packed.type_idx, packed.layout.element);
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I64Const(-1));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Reads one packed element (array and i32 index on the stack) as an `i64`.
fn emit_read_element(f: &mut Function, type_idx: u32, element: PackedIntElement) {
    match element {
        PackedIntElement::U8 | PackedIntElement::U16 => {
            f.instruction(&Instruction::ArrayGetU(type_idx));
            f.instruction(&Instruction::I64ExtendI32U);
        }
        PackedIntElement::I8 | PackedIntElement::I16 => {
            f.instruction(&Instruction::ArrayGetS(type_idx));
            f.instruction(&Instruction::I64ExtendI32S);
        }
        PackedIntElement::U32 => {
            f.instruction(&Instruction::ArrayGet(type_idx));
            f.instruction(&Instruction::I64ExtendI32U);
        }
        PackedIntElement::I32 => {
            f.instruction(&Instruction::ArrayGet(type_idx));
            f.instruction(&Instruction::I64ExtendI32S);
        }
        PackedIntElement::I64 => {
            f.instruction(&Instruction::ArrayGet(type_idx));
        }
    }
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
    emit_read_element(&mut f, packed.type_idx, packed.layout.element);
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
