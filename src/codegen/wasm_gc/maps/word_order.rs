//! Sorting the buckets of a `Map<Int, V>` on machine words.
//!
//! `keys`, `values` and `entries` sort the occupied buckets of a map by key
//! (see `emit_map_order_slots`). The general sort compares two keys through
//! the bucket array: index, key box, `$AverInt`, then the comparator. On a
//! map of a hundred thousand keys every step of that is a cache miss, and the
//! sort spends nearly all its time waiting on memory.
//!
//! When every key of the map is Small (no limbs, the value in `$small`), the
//! collecting pass also copies the keys' words into an `i64` array beside the
//! bucket indices, and the heapsort here orders the two arrays together,
//! comparing words. A canonical Int is Small exactly when it fits in an
//! `i64`, so the words are ordered as the keys are, and since a map's keys are
//! distinct the result is the permutation the general sort produces. A map
//! with any Big key takes the general sort.

use wasm_encoder::{BlockType, Function, HeapType, Instruction, RefType, ValType};

use super::super::WasmGcError;
use super::super::types::TypeRegistry;

/// The locals the word sort adds to `order_slots`, after the general ones.
pub(super) struct WordSort {
    /// The `$AverInt` struct and its `i64` limb array, which holds the words.
    aint: u32,
    words_ty: u32,
    /// The box a key is stored in, if the map boxes its keys.
    key_box: Option<u32>,
    words: u32,
    big: u32,
    child: u32,
    largest: u32,
    root: u32,
    word: u32,
    key: u32,
}

impl WordSort {
    /// `Some` for an `Int` key under the `$AverInt` carrier; `first` is the
    /// index of the first local it may take.
    pub(super) fn for_key(
        k_aver: &str,
        registry: &TypeRegistry,
        first: u32,
    ) -> Result<Option<Self>, WasmGcError> {
        if k_aver.trim() != "Int" || !registry.bignum {
            return Ok(None);
        }
        let missing = |what: &str| WasmGcError::Validation(format!("Map<Int, _> order: {what}"));
        Ok(Some(Self {
            aint: registry
                .aint_struct_idx
                .ok_or_else(|| missing("no $AverInt struct"))?,
            words_ty: registry
                .aint_mag_array_idx
                .ok_or_else(|| missing("no i64 array"))?,
            key_box: registry.primitive_key_box_idx(k_aver),
            words: first,
            big: first + 1,
            child: first + 2,
            largest: first + 3,
            root: first + 4,
            word: first + 5,
            key: first + 6,
        }))
    }

    /// The types of the locals, in the order their indices were given.
    pub(super) fn locals(&self) -> [(u32, ValType); 4] {
        [
            (1, ref_null(self.words_ty)),
            (4, ValType::I32),
            (1, ValType::I64),
            (1, ref_null(self.aint)),
        ]
    }

    /// Allocate the words array, `count` long.
    pub(super) fn emit_alloc(&self, f: &mut Function, count: u32) {
        f.instruction(&Instruction::LocalGet(count));
        f.instruction(&Instruction::ArrayNewDefault(self.words_ty));
        f.instruction(&Instruction::LocalSet(self.words));
    }

    /// Inside the collecting pass, for the occupied bucket `slot` about to be
    /// written at `used`: copy its key's word to `words[used]`, or mark the
    /// map as holding a Big key.
    pub(super) fn emit_collect(
        &self,
        f: &mut Function,
        keys_array: u32,
        keys: u32,
        slot: u32,
        used: u32,
    ) {
        f.instruction(&Instruction::LocalGet(keys));
        f.instruction(&Instruction::LocalGet(slot));
        f.instruction(&Instruction::ArrayGet(keys_array));
        if let Some(key_box) = self.key_box {
            f.instruction(&Instruction::StructGet {
                struct_type_index: key_box,
                field_index: 0,
            });
        }
        f.instruction(&Instruction::LocalTee(self.key));
        self.field(f, 1);
        f.instruction(&Instruction::RefIsNull);
        f.instruction(&Instruction::If(BlockType::Empty));
        f.instruction(&Instruction::LocalGet(self.words));
        f.instruction(&Instruction::LocalGet(used));
        f.instruction(&Instruction::LocalGet(self.key));
        self.field(f, 0);
        f.instruction(&Instruction::ArraySet(self.words_ty));
        f.instruction(&Instruction::Else);
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalSet(self.big));
        f.instruction(&Instruction::End);
    }

    /// When the map held no Big key, heapsort `words` and `indices` (both
    /// `count` long) together and return `indices`; otherwise fall through.
    pub(super) fn emit_sort_and_return(
        &self,
        f: &mut Function,
        order_idx: u32,
        indices: u32,
        count: u32,
        cursor: u32,
    ) {
        f.instruction(&Instruction::LocalGet(self.big));
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::If(BlockType::Empty));
        // Heapify: sift every parent, the last first.
        f.instruction(&Instruction::LocalGet(count));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32ShrU);
        f.instruction(&Instruction::LocalSet(cursor));
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::Loop(BlockType::Empty));
        f.instruction(&Instruction::LocalGet(cursor));
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::BrIf(1));
        f.instruction(&Instruction::LocalGet(cursor));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Sub);
        f.instruction(&Instruction::LocalTee(cursor));
        f.instruction(&Instruction::LocalSet(self.root));
        self.emit_sift(f, order_idx, indices, count);
        f.instruction(&Instruction::Br(0));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
        // Move the largest to the end of the shrinking heap, re-sift the root.
        f.instruction(&Instruction::LocalGet(count));
        f.instruction(&Instruction::LocalSet(cursor));
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::Loop(BlockType::Empty));
        f.instruction(&Instruction::LocalGet(cursor));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32LeU);
        f.instruction(&Instruction::BrIf(1));
        f.instruction(&Instruction::LocalGet(cursor));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Sub);
        f.instruction(&Instruction::LocalSet(cursor));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalSet(self.root));
        self.emit_swap(f, order_idx, indices, self.root, cursor);
        self.emit_sift(f, order_idx, indices, cursor);
        f.instruction(&Instruction::Br(0));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::LocalGet(indices));
        f.instruction(&Instruction::Return);
        f.instruction(&Instruction::End);
    }

    /// Restore the max-heap below `root` within the first `len` entries.
    fn emit_sift(&self, f: &mut Function, order_idx: u32, indices: u32, len: u32) {
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::Loop(BlockType::Empty));
        // child = 2 * root + 1; a leaf ends the sift.
        f.instruction(&Instruction::LocalGet(self.root));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalTee(self.child));
        f.instruction(&Instruction::LocalGet(len));
        f.instruction(&Instruction::I32GeU);
        f.instruction(&Instruction::BrIf(1));
        // largest = the larger child; word = its word.
        f.instruction(&Instruction::LocalGet(self.child));
        f.instruction(&Instruction::LocalSet(self.largest));
        self.word_at(f, self.child);
        f.instruction(&Instruction::LocalSet(self.word));
        f.instruction(&Instruction::LocalGet(self.child));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalTee(self.child));
        f.instruction(&Instruction::LocalGet(len));
        f.instruction(&Instruction::I32LtU);
        f.instruction(&Instruction::If(BlockType::Empty));
        self.word_at(f, self.child);
        f.instruction(&Instruction::LocalGet(self.word));
        f.instruction(&Instruction::I64GtS);
        f.instruction(&Instruction::If(BlockType::Empty));
        f.instruction(&Instruction::LocalGet(self.child));
        f.instruction(&Instruction::LocalSet(self.largest));
        self.word_at(f, self.child);
        f.instruction(&Instruction::LocalSet(self.word));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
        // The heap holds when the root is not smaller than its larger child.
        self.word_at(f, self.root);
        f.instruction(&Instruction::LocalGet(self.word));
        f.instruction(&Instruction::I64GeS);
        f.instruction(&Instruction::BrIf(1));
        self.emit_swap(f, order_idx, indices, self.root, self.largest);
        f.instruction(&Instruction::LocalGet(self.largest));
        f.instruction(&Instruction::LocalSet(self.root));
        f.instruction(&Instruction::Br(0));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
    }

    /// Swap entries `a` and `b` of both `words` and `indices`. Uses `word`
    /// and `child` as scratch.
    fn emit_swap(&self, f: &mut Function, order_idx: u32, indices: u32, a: u32, b: u32) {
        self.word_at(f, a);
        f.instruction(&Instruction::LocalSet(self.word));
        f.instruction(&Instruction::LocalGet(self.words));
        f.instruction(&Instruction::LocalGet(a));
        self.word_at(f, b);
        f.instruction(&Instruction::ArraySet(self.words_ty));
        f.instruction(&Instruction::LocalGet(self.words));
        f.instruction(&Instruction::LocalGet(b));
        f.instruction(&Instruction::LocalGet(self.word));
        f.instruction(&Instruction::ArraySet(self.words_ty));
        f.instruction(&Instruction::LocalGet(indices));
        f.instruction(&Instruction::LocalGet(a));
        f.instruction(&Instruction::ArrayGet(order_idx));
        f.instruction(&Instruction::LocalSet(self.child));
        f.instruction(&Instruction::LocalGet(indices));
        f.instruction(&Instruction::LocalGet(a));
        f.instruction(&Instruction::LocalGet(indices));
        f.instruction(&Instruction::LocalGet(b));
        f.instruction(&Instruction::ArrayGet(order_idx));
        f.instruction(&Instruction::ArraySet(order_idx));
        f.instruction(&Instruction::LocalGet(indices));
        f.instruction(&Instruction::LocalGet(b));
        f.instruction(&Instruction::LocalGet(self.child));
        f.instruction(&Instruction::ArraySet(order_idx));
    }

    fn word_at(&self, f: &mut Function, pos: u32) {
        f.instruction(&Instruction::LocalGet(self.words));
        f.instruction(&Instruction::LocalGet(pos));
        f.instruction(&Instruction::ArrayGet(self.words_ty));
    }

    fn field(&self, f: &mut Function, field_index: u32) {
        f.instruction(&Instruction::StructGet {
            struct_type_index: self.aint,
            field_index,
        });
    }
}

fn ref_null(type_idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(type_idx),
    })
}
