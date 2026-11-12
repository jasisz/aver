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

thread_local! {
    /// Whether a packed refinement's values are views `(array, start, len)`
    /// in the module being emitted: every plain build, never a `--certify`
    /// one, whose arrays and helpers stay the forms the certificate's wall
    /// proves. One module is emitted on one thread.
    static PACKED_VIEWS: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Sets [`packed_views`] for one module's emission and puts it back after.
pub(super) struct PackedViews(bool);

impl PackedViews {
    pub(super) fn set(on: bool) -> Self {
        Self(PACKED_VIEWS.with(|flag| flag.replace(on)))
    }
}

impl Drop for PackedViews {
    fn drop(&mut self) {
        PACKED_VIEWS.with(|flag| flag.set(self.0));
    }
}

/// Whether packed values are views in the module being emitted.
pub(super) fn packed_views() -> bool {
    PACKED_VIEWS.with(std::cell::Cell::get)
}

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
    /// Where values are views: `array_of(view) -> array`, the array itself
    /// when the view covers it and a copy of the elements it shows
    /// otherwise, for code outside these helpers that reads the elements as
    /// an array.
    pub(super) views: Option<ViewOps>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct ViewOps {
    pub(super) array_of: u32,
}

#[derive(Default)]
pub(super) struct PackedSequenceHelperRegistry {
    ops: HashMap<String, PackedSequenceOps>,
    type_indices: HashMap<String, (u32, u32, u32, u32)>,
    get_type_indices: HashMap<String, u32>,
    byte_at_type_indices: HashMap<String, u32>,
    /// Per view type: the `array_of` function type.
    view_type_indices: HashMap<String, u32>,
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
            let is_view = registry
                .packed_sequence(name)
                .is_some_and(|packed| packed.is_view());
            let views = is_view.then(|| {
                let array_of_type = *next_type_idx;
                *next_type_idx += 1;
                self.view_type_indices.insert(name.clone(), array_of_type);
                let array_of = *next_fn_idx;
                *next_fn_idx += 1;
                ViewOps { array_of }
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
                    views,
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
            if self.view_type_indices.contains_key(name) {
                types
                    .ty()
                    .function([packed_ref], [ref_ty(packed.array_idx)]);
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
            if let Some(array_of) = self.view_type_indices.get(name) {
                funcs.function(*array_of);
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
            if self.view_type_indices.contains_key(name) {
                codes.function(&emit_array_of(name, registry)?);
            }
        }
        Ok(())
    }
}

fn emit_concat(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    if registry.packed_sequence(name).is_some_and(|p| p.is_view()) {
        return emit_concat_view(name, registry);
    }
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
    if registry.packed_sequence(name).is_some_and(|p| p.is_view()) {
        return emit_take_view(name, registry);
    }
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
    if registry.packed_sequence(name).is_some_and(|p| p.is_view()) {
        return emit_drop_view(name, registry);
    }
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
    let packed_ref = ref_ty(packed.array_idx);
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
    f.instruction(&Instruction::ArrayNewDefault(packed.array_idx));
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
    f.instruction(&Instruction::ArraySet(packed.array_idx));
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
    // A view of the whole array, where values are views.
    if packed.is_view() {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(2));
        no_cell(&mut f);
        f.instruction(&Instruction::StructNew(packed.type_idx));
    }
    f.instruction(&Instruction::End);
    Ok(f)
}

/// The first element of `List.drop(carrier, index)` as a one-element
/// `List<Int>`, or the empty list when nothing is left: all a list match
/// that ignores the tail can tell of that value, read in O(1). A negative
/// index drops nothing, as `List.drop` does, so it reads element zero.
fn emit_get(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    if registry.packed_sequence(name).is_some_and(|p| p.is_view()) {
        return emit_get_view(name, registry);
    }
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
    crate::codegen::wasm_gc::lists::push_len_unknown(&mut f);
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
    if registry.packed_sequence(name).is_some_and(|p| p.is_view()) {
        return emit_byte_at_view(name, registry);
    }
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
    if registry.packed_sequence(name).is_some_and(|p| p.is_view()) {
        return emit_unpack_view(name, registry);
    }
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
    crate::codegen::wasm_gc::lists::push_len_unknown(&mut f);
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

// ---- Views ---------------------------------------------------------------
//
// Where views are on (`packed_views`), a packed value is
// `(struct (ref null $array) i32 i32)`: the array, the place of the first
// element in it, and how many elements follow. `take` and `drop` make a new
// view of the same array; only `concat` and `array_of` copy elements.

/// The view type, its array type and the element layout of `name`.
fn view_parts(
    name: &str,
    registry: &TypeRegistry,
) -> Result<(u32, u32, PackedIntElement), WasmGcError> {
    let packed = registry
        .packed_sequence(name)
        .ok_or_else(|| WasmGcError::Validation(format!("packed type `{name}` missing")))?;
    Ok((packed.type_idx, packed.array_idx, packed.layout.element))
}

fn view_field(f: &mut Function, view: u32, field: u32) {
    f.instruction(&Instruction::StructGet {
        struct_type_index: view,
        field_index: field,
    });
}

/// Locals `arr`, `start` and `len` of the view in local `from`.
fn view_open(f: &mut Function, view: u32, from: u32, arr: u32, start: u32, len: u32) {
    for (field, local) in [(0, arr), (1, start), (2, len)] {
        f.instruction(&Instruction::LocalGet(from));
        view_field(f, view, field);
        f.instruction(&Instruction::LocalSet(local));
    }
}

/// The count an `i64` asks for, clamped to `0..=len` (`len` an i32 local),
/// left on the stack as an i32: what `List.take` / `List.drop` do with a
/// negative or too large count.
fn clamped_count(f: &mut Function, count: u32, len: u32) {
    f.instruction(&Instruction::LocalGet(count));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LeS);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(count));
    f.instruction(&Instruction::LocalGet(len));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::LocalGet(count));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(len));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
}

/// `array_of : (view) -> array`: the array when the view shows all of it,
/// a copy of the elements it shows otherwise.
fn emit_array_of(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (view, array, _) = view_parts(name, registry)?;
    // params: 0=view. locals: 1=arr, 2=start, 3=len, 4=out.
    let mut f = Function::new([(1, ref_ty(array)), (2, ValType::I32), (1, ref_ty(array))]);
    view_open(&mut f, view, 0, 1, 2, 3);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(BlockType::Result(ref_ty(array))));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayNewDefault(array));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: array,
        array_type_index_src: array,
    });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Push the "no cell" a view built outside `concat` carries.
pub(super) fn no_cell(f: &mut Function) {
    f.instruction(&Instruction::RefNull(HeapType::Abstract {
        shared: false,
        ty: wasm_encoder::AbstractHeapType::None,
    }));
}

/// The cell type views of `name` share (`PackedSequenceType::cell_idx`).
fn view_cell(name: &str, registry: &TypeRegistry) -> Result<u32, WasmGcError> {
    registry
        .packed_sequence(name)
        .filter(|packed| packed.is_view())
        .map(|packed| packed.cell_idx())
        .ok_or_else(|| WasmGcError::Validation(format!("packed view `{name}` missing")))
}

/// `take : (view, i64) -> view` of the same array, keeping its cell.
fn emit_take_view(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (view, array, _) = view_parts(name, registry)?;
    // params: 0=view, 1=count. locals: 2=arr, 3=start, 4=len.
    let mut f = Function::new([(1, ref_ty(array)), (2, ValType::I32)]);
    view_open(&mut f, view, 0, 2, 3, 4);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    clamped_count(&mut f, 1, 4);
    f.instruction(&Instruction::LocalGet(0));
    view_field(&mut f, view, 3);
    f.instruction(&Instruction::StructNew(view));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `drop : (view, i64) -> view` of the same array, keeping its cell.
fn emit_drop_view(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (view, array, _) = view_parts(name, registry)?;
    // params: 0=view, 1=count. locals: 2=arr, 3=start, 4=len, 5=k.
    let mut f = Function::new([(1, ref_ty(array)), (3, ValType::I32)]);
    view_open(&mut f, view, 0, 2, 3, 4);
    clamped_count(&mut f, 1, 4);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalGet(0));
    view_field(&mut f, view, 3);
    f.instruction(&Instruction::StructNew(view));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `concat : (view, view) -> view`: one of them when the other is empty.
///
/// A view `concat` builds carries a cell saying how much of its array the
/// views sharing it use. When the left side ends exactly there and the
/// array has room, the right side is written after it and the cell moves
/// on; the left side is not copied. No view of that array sees the new
/// elements: every one of them ends at or before the old mark. Otherwise
/// both sides are copied into a new array: exactly as long when the left
/// side cannot grow, with room for as much again when it could but was
/// full, so a run of appends copies each element a bounded number of times.
fn emit_concat_view(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (view, array, _) = view_parts(name, registry)?;
    let cell = view_cell(name, registry)?;
    let arr = ref_ty(array);
    let cell_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(cell),
    });
    // params: 0=left, 1=right. locals: 2=la, 3=ls, 4=ll, 5=ra, 6=rs, 7=rl,
    // 8=out, 9=cell, 10=end, 11=total, 12=cap.
    let mut f = Function::new([
        (1, arr),
        (2, ValType::I32),
        (1, arr),
        (2, ValType::I32),
        (1, arr),
        (1, cell_ref),
        (3, ValType::I32),
    ]);
    let copy = |f: &mut Function| {
        f.instruction(&Instruction::ArrayCopy {
            array_type_index_dst: array,
            array_type_index_src: array,
        });
    };
    view_open(&mut f, view, 0, 2, 3, 4);
    view_open(&mut f, view, 1, 5, 6, 7);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // end = ls + ll; total = ll + rl; cap = total.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(10));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalTee(11));
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::LocalGet(0));
    view_field(&mut f, view, 3);
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(cell)));
    f.instruction(&Instruction::LocalTee(9));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    // The left side ends where the array's use does: it may grow.
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::StructGet {
        struct_type_index: cell,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    // Room after it: append in place.
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(7));
    copy(&mut f);
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::StructSet {
        struct_type_index: cell,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::StructNew(view));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // Full: grow to twice what is needed, while that still fits an i32.
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(0x3fff_ffff));
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::ArrayNewDefault(array));
    f.instruction(&Instruction::LocalSet(8));
    for (dst_at, src, src_at, n) in [(None, 2, 3, 4), (Some(4), 5, 6, 7)] {
        f.instruction(&Instruction::LocalGet(8));
        match dst_at {
            None => f.instruction(&Instruction::I32Const(0)),
            Some(local) => f.instruction(&Instruction::LocalGet(local)),
        };
        f.instruction(&Instruction::LocalGet(src));
        f.instruction(&Instruction::LocalGet(src_at));
        f.instruction(&Instruction::LocalGet(n));
        copy(&mut f);
    }
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::StructNew(cell));
    f.instruction(&Instruction::StructNew(view));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `unpack : (view) -> List<Int>`: the elements the view shows, as cells.
fn emit_unpack_view(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (view, array, element) = view_parts(name, registry)?;
    let list_idx = registry
        .list_type_idx("List<Int>")
        .ok_or_else(|| WasmGcError::Validation("packed carrier requires List<Int>".into()))?;
    let list_ref = ref_ty(list_idx);
    // params: 0=view. locals: 1=acc, 2=i (counts down), 3=arr, 4=start, 5=len.
    let mut f = Function::new([
        (1, list_ref),
        (1, ValType::I32),
        (1, ref_ty(array)),
        (2, ValType::I32),
    ]);
    view_open(&mut f, view, 0, 3, 4, 5);
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(5));
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
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    emit_read_element(&mut f, array, element);
    if registry.bignum {
        let from_i64 = registry.aint_from_i64_fn_idx.ok_or_else(|| {
            WasmGcError::Validation("packed carrier requires __aint_from_i64".into())
        })?;
        f.instruction(&Instruction::Call(from_i64));
    }
    f.instruction(&Instruction::LocalGet(1));
    crate::codegen::wasm_gc::lists::push_len_unknown(&mut f);
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `get : (view, i64) -> List<Int>`, as `emit_get` on the elements shown.
fn emit_get_view(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (view, array, element) = view_parts(name, registry)?;
    let list_idx = registry
        .list_type_idx("List<Int>")
        .ok_or_else(|| WasmGcError::Validation("packed carrier requires List<Int>".into()))?;
    // params: 0=view, 1=index. locals: 2=arr, 3=start, 4=len.
    let mut f = Function::new([(1, ref_ty(array)), (2, ValType::I32)]);
    view_open(&mut f, view, 0, 2, 3, 4);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::Select);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::If(BlockType::Result(ref_ty(list_idx))));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Add);
    emit_read_element(&mut f, array, element);
    if registry.bignum {
        let from_i64 = registry.aint_from_i64_fn_idx.ok_or_else(|| {
            WasmGcError::Validation("packed carrier requires __aint_from_i64".into())
        })?;
        f.instruction(&Instruction::Call(from_i64));
    }
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    crate::codegen::wasm_gc::lists::push_len_unknown(&mut f);
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `byte_at : (view, i64) -> i64`, as `emit_byte_at` on the elements shown.
fn emit_byte_at_view(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (view, array, element) = view_parts(name, registry)?;
    // params: 0=view, 1=position.
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(0));
    view_field(&mut f, view, 2);
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I64)));
    f.instruction(&Instruction::LocalGet(0));
    view_field(&mut f, view, 0);
    f.instruction(&Instruction::LocalGet(0));
    view_field(&mut f, view, 1);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Add);
    emit_read_element(&mut f, array, element);
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I64Const(-1));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    Ok(f)
}
