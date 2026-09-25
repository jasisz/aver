//! Versions of one map that share its arrays.
//!
//! A `Map<K, V>` value is a `$map` struct over three arrays. `Map.set` and
//! `Map.remove` write into those arrays in place and return a new `$map`
//! struct over the same arrays, so an update costs a probe, not a copy of
//! the table. The map it was given stays a valid value: before the write,
//! the helper records what the bucket held in a `$diff` struct and hangs it
//! on the old `$map` (`diff` field). The old version is then "the new
//! version, except that bucket `idx` holds `key`, `value`, `hash`".
//!
//! Exactly one version of a lineage owns the arrays' current contents: the
//! one whose `diff` is null. [`emit_map_reroot`] makes a given version that
//! one. It follows the `diff` chain to the current version and walks back,
//! swapping each recorded bucket into the arrays and hanging the swapped-out
//! contents on the version it just left, so every version stays readable
//! and the chain now points the other way. Every helper that reads or writes
//! a map's arrays reroots first; `len` reads only the version's own `size`.
//!
//! A program that uses each map once, as the generated loop and most
//! recursive builders do, never has more than one diff to undo, so an update
//! is O(1). A program that keeps an old version and reads it again pays for
//! the buckets written since, once, and the version it moved away from pays
//! the same to come back. A grow allocates new arrays and leaves the old
//! ones to the old version, so no diff crosses a grow.
//!
//! This is the persistent-array technique of Baker ("Shallow binding makes
//! functional arrays fast", 1991), as in Conchon and Filliâtre's
//! "A Persistent Union-Find Data Structure" (2007).
//!
//! Two versions of one lineage cannot both be current. A helper that reads
//! two maps at once (`eq`) therefore copies one side when both share arrays.
//! The host reads a map's buckets directly only for the sockets of
//! `Tcp.poll` and the wait set of `Wait.poll`; those arguments are rerooted
//! before the import is called.

use wasm_encoder::{BlockType, Function, HeapType, Instruction, RefType, ValType};

use super::super::WasmGcError;
use super::super::types::{MapSlots, TypeRegistry};
use super::key_storage_val_type;

/// `$map` field holding the version's diff; null on the current version.
const DIFF_FIELD: u32 = 5;

const DIFF_NEXT: u32 = 0;
const DIFF_IDX: u32 = 1;
const DIFF_KEY: u32 = 2;
const DIFF_VALUE: u32 = 3;
const DIFF_HASH: u32 = 4;

fn nullable(idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(idx),
    })
}

/// Push a null diff: the operand every `struct.new $map` of a current
/// version ends with.
pub(in crate::codegen::wasm_gc) fn emit_no_diff(f: &mut Function, slots: MapSlots) {
    f.instruction(&Instruction::RefNull(HeapType::Concrete(slots.diff)));
}

/// `local.get map; call reroot; local.set map`: make the version in
/// `map_local` the current one before its arrays are read.
pub(in crate::codegen::wasm_gc) fn emit_reroot_local(
    f: &mut Function,
    reroot_fn: u32,
    map_local: u32,
) {
    f.instruction(&Instruction::LocalGet(map_local));
    f.instruction(&Instruction::Call(reroot_fn));
    f.instruction(&Instruction::LocalSet(map_local));
}

/// `reroot(map) -> map`: make `map` the version that owns its arrays'
/// contents, and return it.
pub(super) fn emit_map_reroot(
    canonical: &str,
    registry: &TypeRegistry,
    slots: MapSlots,
) -> Result<Function, WasmGcError> {
    let (k_aver, v_aver) = super::super::types::parse_map_kv(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("Map.reroot: bad canonical `{canonical}`"))
    })?;
    let v_val = super::super::types::aver_to_wasm(v_aver, Some(registry))?.ok_or_else(|| {
        WasmGcError::Validation(format!(
            "Map value type `{v_aver}` has no wasm representation"
        ))
    })?;
    let map_ref = nullable(slots.map);
    let diff_ref = nullable(slots.diff);
    // params: 0=map. locals: 1=prev, 2=cur, 3=diff, 4=next, 5=current
    // version, 6=version being restored, 7=idx, 8=keys, 9=values,
    // 10=hashes, 11/12/13 = the bucket's key/value/hash being swapped out.
    let mut f = Function::new([
        (1, map_ref),                                 // 1: prev
        (1, map_ref),                                 // 2: cur
        (1, diff_ref),                                // 3: diff
        (1, map_ref),                                 // 4: next
        (1, map_ref),                                 // 5: current version
        (1, map_ref),                                 // 6: version being restored
        (1, ValType::I32),                            // 7: idx
        (1, nullable(slots.keys_array)),              // 8: keys
        (1, nullable(slots.values_array)),            // 9: values
        (1, nullable(slots.hashes_array)),            // 10: hashes
        (1, key_storage_val_type(k_aver, registry)?), // 11: key out
        (1, v_val),                                   // 12: value out
        (1, ValType::I32),                            // 13: hash out
    ]);
    let get_map = |f: &mut Function, field| {
        f.instruction(&Instruction::StructGet {
            struct_type_index: slots.map,
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

    // Already current: nothing to do. This is every call on a map used once.
    f.instruction(&Instruction::LocalGet(0));
    get_map(&mut f, DIFF_FIELD);
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Walk to the current version, turning each diff's `next` round to
    // point at the version before it. `prev` ends on the last stale
    // version, whose diff is relative to the current one.
    f.instruction(&Instruction::RefNull(HeapType::Concrete(slots.map)));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    get_map(&mut f, DIFF_FIELD);
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

    // Every version in the chain shares the current version's arrays.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    get_map(&mut f, 2);
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(5));
    get_map(&mut f, 3);
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::LocalGet(5));
    get_map(&mut f, 4);
    f.instruction(&Instruction::LocalSet(10));

    // Walk back towards `map`, one version at a time: put that version's
    // bucket back into the arrays, and keep what was there as the diff of
    // the version just left, pointing forward to the restored one.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    get_map(&mut f, DIFF_FIELD);
    f.instruction(&Instruction::LocalSet(3));
    // prev = the version before this one (the reversed link).
    f.instruction(&Instruction::LocalGet(3));
    get_diff(&mut f, DIFF_NEXT);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(3));
    get_diff(&mut f, DIFF_IDX);
    f.instruction(&Instruction::LocalSet(7));
    // Swap the bucket out.
    for (array_local, array_type, out_local) in [
        (8, slots.keys_array, 11),
        (9, slots.values_array, 12),
        (10, slots.hashes_array, 13),
    ] {
        f.instruction(&Instruction::LocalGet(array_local));
        f.instruction(&Instruction::LocalGet(7));
        f.instruction(&Instruction::ArrayGet(array_type));
        f.instruction(&Instruction::LocalSet(out_local));
    }
    // Put the restored version's bucket in.
    for (array_local, array_type, field) in [
        (8, slots.keys_array, DIFF_KEY),
        (9, slots.values_array, DIFF_VALUE),
        (10, slots.hashes_array, DIFF_HASH),
    ] {
        f.instruction(&Instruction::LocalGet(array_local));
        f.instruction(&Instruction::LocalGet(7));
        f.instruction(&Instruction::LocalGet(3));
        get_diff(&mut f, field);
        f.instruction(&Instruction::ArraySet(array_type));
    }
    // The diff now describes the version just left, relative to the
    // restored one.
    for (out_local, field) in [(11, DIFF_KEY), (12, DIFF_VALUE), (13, DIFF_HASH)] {
        f.instruction(&Instruction::LocalGet(3));
        f.instruction(&Instruction::LocalGet(out_local));
        set_diff(&mut f, field);
    }
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(6));
    set_diff(&mut f, DIFF_NEXT);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructSet {
        struct_type_index: slots.map,
        field_index: DIFF_FIELD,
    });
    f.instruction(&Instruction::LocalGet(6));
    emit_no_diff(&mut f, slots);
    f.instruction(&Instruction::StructSet {
        struct_type_index: slots.map,
        field_index: DIFF_FIELD,
    });
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Locals a versioned write reads and writes. `version` holds the current
/// version before the write and the new one after it.
pub(super) struct VersionedWrite {
    pub version: u32,
    pub diff: u32,
    pub next: u32,
    pub idx: u32,
    pub cap: u32,
    pub keys: u32,
    pub values: u32,
    pub hashes: u32,
}

/// Write one bucket of the current version in `w.version` as a new version.
///
/// Records the bucket's key, value and hash on the old version, lets
/// `write` store the new ones (it runs with nothing on the stack and must
/// leave nothing), and moves `w.version` to a new `$map` over the same
/// arrays with the old version's size. The caller adjusts the size of the
/// last version it makes.
pub(super) fn emit_versioned_write(
    f: &mut Function,
    slots: MapSlots,
    w: &VersionedWrite,
    write: impl FnOnce(&mut Function),
) {
    f.instruction(&Instruction::RefNull(HeapType::Concrete(slots.map)));
    f.instruction(&Instruction::LocalGet(w.idx));
    for (array_local, array_type) in [
        (w.keys, slots.keys_array),
        (w.values, slots.values_array),
        (w.hashes, slots.hashes_array),
    ] {
        f.instruction(&Instruction::LocalGet(array_local));
        f.instruction(&Instruction::LocalGet(w.idx));
        f.instruction(&Instruction::ArrayGet(array_type));
    }
    f.instruction(&Instruction::StructNew(slots.diff));
    f.instruction(&Instruction::LocalSet(w.diff));

    write(f);

    f.instruction(&Instruction::LocalGet(w.version));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(w.cap));
    f.instruction(&Instruction::LocalGet(w.keys));
    f.instruction(&Instruction::LocalGet(w.values));
    f.instruction(&Instruction::LocalGet(w.hashes));
    emit_no_diff(f, slots);
    f.instruction(&Instruction::StructNew(slots.map));
    f.instruction(&Instruction::LocalSet(w.next));
    f.instruction(&Instruction::LocalGet(w.diff));
    f.instruction(&Instruction::LocalGet(w.next));
    f.instruction(&Instruction::StructSet {
        struct_type_index: slots.diff,
        field_index: DIFF_NEXT,
    });
    f.instruction(&Instruction::LocalGet(w.version));
    f.instruction(&Instruction::LocalGet(w.diff));
    f.instruction(&Instruction::StructSet {
        struct_type_index: slots.map,
        field_index: DIFF_FIELD,
    });
    f.instruction(&Instruction::LocalGet(w.next));
    f.instruction(&Instruction::LocalSet(w.version));
}
