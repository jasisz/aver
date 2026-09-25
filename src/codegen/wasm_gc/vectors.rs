//! Versions of one Vector that share its array.
//!
//! A `Vector<T>` value is a version struct over an `(array (mut T))`:
//! `arr`, `diff` and `held`. `Vector.set` writes the cell in place and
//! returns a new version over the same array, so a set costs one write and
//! two small structs, not a copy of the array. The vector it was given stays
//! a valid value: before the write, the old cell is recorded in a diff
//! struct hung on the old version, which is then "the new version, except
//! that cell `idx` holds `value`". This is the scheme `maps/versions.rs`
//! uses for a Map's buckets, after Baker ("Shallow binding makes functional
//! arrays fast", 1991).
//!
//! Exactly one version of a lineage owns the array's current contents: the
//! one whose `diff` is null. [`emit_vector_current`] makes a given version
//! that one and returns the array. It follows the `diff` chain to the
//! current version and walks back, swapping each recorded cell into the
//! array and hanging the swapped-out cell on the version it just left, so
//! every version stays readable and the chain now points the other way.
//! Every read of the elements goes through it; `len` reads the array's
//! length, which no version changes.
//!
//! `held` is set on a version once another version's diff points at it,
//! and never cleared. A set whose target is uniquely owned (nothing reads
//! that binding again) and that no other version is described against may
//! write the cell and return the same version, with nothing recorded:
//! nobody can see the old contents any more. That keeps a loop that builds
//! a vector by setting it free of allocation.
//!
//! Two versions of one lineage cannot both be current, so a helper that
//! reads two vectors at once (`eq`) copies one side when both share an
//! array. A host reads and writes a vector only through the capability ABI
//! helpers, which go through the same current version.

use wasm_encoder::{BlockType, Function, HeapType, Instruction, RefType, ValType};

use super::WasmGcError;
use super::types::{TypeRegistry, VectorSlots};

/// Version fields.
pub(super) const ARR_FIELD: u32 = 0;
const DIFF_FIELD: u32 = 1;
const HELD_FIELD: u32 = 2;

/// Diff fields.
const DIFF_NEXT: u32 = 0;
const DIFF_IDX: u32 = 1;
const DIFF_VALUE: u32 = 2;

fn nullable(idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(idx),
    })
}

/// The slots and the wasm element type of `canonical` (`Vector<T>`).
fn slots_and_element(
    canonical: &str,
    registry: &TypeRegistry,
) -> Result<(VectorSlots, ValType), WasmGcError> {
    let slots = registry.vector_slots(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("vector `{canonical}` has no version slots"))
    })?;
    let element = TypeRegistry::vector_element_type(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("vector `{canonical}` has no parsable element type"))
    })?;
    let element = super::types::aver_to_wasm(element, Some(registry))?.unwrap_or(ValType::I32);
    Ok((slots, element))
}

/// With an array on the stack, leave the version that owns it: a fresh
/// vector no other version is described against.
pub(super) fn emit_wrap_array(f: &mut Function, slots: VectorSlots) {
    f.instruction(&Instruction::RefNull(HeapType::Concrete(slots.diff)));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::StructNew(slots.version));
}

/// With a version on the stack, leave its array's length. Every version of
/// a lineage has the same length, so this needs no reroot.
pub(super) fn emit_version_len(f: &mut Function, slots: VectorSlots) {
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.version,
        field_index: ARR_FIELD,
    });
    f.instruction(&Instruction::ArrayLen);
}

/// `current(vector) -> array`: make `vector` the version that owns its
/// array's contents, and return the array.
pub(super) fn emit_vector_current(
    canonical: &str,
    registry: &TypeRegistry,
) -> Result<Function, WasmGcError> {
    let (slots, element) = slots_and_element(canonical, registry)?;
    let version_ref = nullable(slots.version);
    // params: 0=vector. locals: 1=prev, 2=cur, 3=diff, 4=next, 5=current
    // version, 6=version being restored, 7=idx, 8=array, 9=cell swapped out.
    let mut f = Function::new([
        (1, version_ref),
        (1, version_ref),
        (1, nullable(slots.diff)),
        (1, version_ref),
        (1, version_ref),
        (1, version_ref),
        (1, ValType::I32),
        (1, nullable(slots.array)),
        (1, element),
    ]);
    let get_version = |f: &mut Function, field| {
        f.instruction(&Instruction::StructGet {
            struct_type_index: slots.version,
            field_index: field,
        });
    };
    let set_version = |f: &mut Function, field| {
        f.instruction(&Instruction::StructSet {
            struct_type_index: slots.version,
            field_index: field,
        });
    };
    let get_diff = |f: &mut Function, field| {
        f.instruction(&Instruction::StructGet {
            struct_type_index: slots.diff,
            field_index: field,
        });
    };
    let set_diff = |f: &mut Function, field| {
        f.instruction(&Instruction::StructSet {
            struct_type_index: slots.diff,
            field_index: field,
        });
    };

    // Already current: every read of a vector used once.
    f.instruction(&Instruction::LocalGet(0));
    get_version(&mut f, DIFF_FIELD);
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    get_version(&mut f, ARR_FIELD);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Walk to the current version, turning each diff's `next` round to
    // point at the version before it. `prev` ends on the last stale
    // version, whose diff is relative to the current one.
    f.instruction(&Instruction::RefNull(HeapType::Concrete(slots.version)));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    get_version(&mut f, DIFF_FIELD);
    f.instruction(&Instruction::LocalTee(3));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(3));
    get_diff(&mut f, DIFF_NEXT);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    set_diff(&mut f, DIFF_NEXT);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // Every version in the chain shares the current version's array.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    get_version(&mut f, ARR_FIELD);
    f.instruction(&Instruction::LocalSet(8));

    // Walk back towards `vector`, one version at a time: put that version's
    // cell back into the array, and keep what was there as the diff of the
    // version just left, pointing forward to the restored one.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    get_version(&mut f, DIFF_FIELD);
    f.instruction(&Instruction::LocalSet(3));
    // prev = the version before this one (the reversed link).
    f.instruction(&Instruction::LocalGet(3));
    get_diff(&mut f, DIFF_NEXT);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(3));
    get_diff(&mut f, DIFF_IDX);
    f.instruction(&Instruction::LocalSet(7));
    // Swap the cell out, and the restored version's cell in.
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::ArrayGet(slots.array));
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(3));
    get_diff(&mut f, DIFF_VALUE);
    f.instruction(&Instruction::ArraySet(slots.array));
    // The diff now describes the version just left, relative to the
    // restored one, which is therefore held.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(9));
    set_diff(&mut f, DIFF_VALUE);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(6));
    set_diff(&mut f, DIFF_NEXT);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(3));
    set_version(&mut f, DIFF_FIELD);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(slots.diff)));
    set_version(&mut f, DIFF_FIELD);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(1));
    set_version(&mut f, HELD_FIELD);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `set(vector, idx, value, owned) -> vector`: the vector with cell `idx`
/// holding `value`. The caller has checked that `idx` is in range. `owned`
/// is non-zero when nothing reads `vector` after the set; the cell is then
/// written with nothing recorded when no other version is described
/// against `vector`.
pub(super) fn emit_vector_set(
    canonical: &str,
    registry: &TypeRegistry,
    current_fn: u32,
) -> Result<Function, WasmGcError> {
    let (slots, _) = slots_and_element(canonical, registry)?;
    // params: 0=vector, 1=idx, 2=value, 3=owned. locals: 4=array, 5=new version.
    let mut f = Function::new([(1, nullable(slots.array)), (1, nullable(slots.version))]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(current_fn));
    f.instruction(&Instruction::LocalSet(4));

    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.version,
        field_index: HELD_FIELD,
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArraySet(slots.array));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // The new version, held by the old one's diff.
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(slots.diff)));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::StructNew(slots.version));
    f.instruction(&Instruction::LocalSet(5));
    // The old version: the new one, except for the old cell.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::ArrayGet(slots.array));
    f.instruction(&Instruction::StructNew(slots.diff));
    f.instruction(&Instruction::StructSet {
        struct_type_index: slots.version,
        field_index: DIFF_FIELD,
    });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArraySet(slots.array));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// A function type: its params and results.
pub(super) type FuncType = (Vec<ValType>, Vec<ValType>);

/// The function types of the two helpers, in registration order:
/// `current : (vector) -> array`, `set : (vector, i32, T, i32) -> vector`.
pub(super) fn helper_types(
    canonical: &str,
    registry: &TypeRegistry,
) -> Result<[FuncType; 2], WasmGcError> {
    let (slots, element) = slots_and_element(canonical, registry)?;
    let version_ref = nullable(slots.version);
    Ok([
        (vec![version_ref], vec![nullable(slots.array)]),
        (
            vec![version_ref, ValType::I32, element, ValType::I32],
            vec![version_ref],
        ),
    ])
}
