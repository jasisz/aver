//! The operand stack, and the per-slot live-reference count it carries.
//!
//! Locals are a WINDOW on the operand stack (`CallFrame::bp` + `local_count`),
//! not a separate array, so "how many live references does this arena slot
//! have?" has one honest answer at this layer: how many cells of
//! [`OperandStack`] hold that slot's index. That is what the count below is,
//! by definition, and [`OperandStack::audit`] is the recomputation of the
//! definition that the incremental maintenance is checked against.
//!
//! ## Why the count lives INSIDE the stack
//!
//! The count is a function of the cells. Keeping the two in separate fields
//! would leave every `Vec` method on the cells as a way to break the invariant
//! silently — a missed hook point is not a compile error, it is a wrong number
//! at a `debug_assert` months later. Wrapping the `Vec` moves that to the
//! compiler: the interpreter can only reach the cells through the methods
//! here, and every method that MOVES a cell maintains the count. The method
//! names deliberately match the `Vec` ones they replace, so the interpreter's
//! ~200 `self.stack.push` / `.pop` / `.truncate` sites read exactly as before.
//!
//! ## What the count is NOT
//!
//! It counts operand-stack cells and nothing else. A slot also reachable from
//! a global, a chunk constant, or from inside another arena entry reads as
//! uniquely held here. That is deliberate for phase P1, which decides nothing:
//! the number is maintained and audited, and the only thing consulted is the
//! DIRECTIONAL cross-check against the static owned mask
//! (`VM::cross_check_owned_mask`). Anything that later wants to take a decision
//! from it owes the other holders a separate argument.
//!
//! ## Relocation
//!
//! Arena collections rewrite indices, which would leave the count keyed on
//! addresses nothing holds any more. Every collection the interpreter runs is
//! bounded by a frame's regions and takes its roots from a Rust-side slice, and
//! the stack cells that could name a relocated slot are dropped BEFORE it runs:
//! `RETURN` truncates to `frame.bp` before `complete_frame_return`, and the two
//! tail calls truncate to `bp` before the boundary finalizer. The one
//! collection that rewrites the stack in place — `VM::collect_live_vm_roots` —
//! hands the rewritten cells back through [`OperandStack::overwrite_all`],
//! which recomputes the count from scratch.

use std::ops::{Index, Range, RangeFrom};

use super::VM;
use crate::nan_value::{HEAP_SPACE_COUNT, NanValue, split_heap_index};
use crate::vm::builtin::VmBuiltin;

/// Tallies from the directional cross-check between the compiler's static
/// owned mask and the runtime reference count.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct VmSlotUniquenessStats {
    /// Heap-backed arguments a `CALL_BUILTIN_OWNED` mask granted ownership of.
    pub owned_grants: u64,
    /// Of those, the ones the runtime count did NOT agree were uniquely held.
    ///
    /// This is the soundness direction: the static grant is a claim that
    /// nothing else can observe the mutation, so it has to be a subset of what
    /// the runtime sees as unique. A non-zero here is a bug in the whitelist or
    /// a bug in this bookkeeping, never a tolerable difference — which is why
    /// the same event is a `debug_assert` in a debug build and only falls back
    /// to being counted where assertions are compiled out.
    pub owned_grants_without_unique_slot: u64,
    /// Arguments the runtime count saw as uniquely held where the static mask
    /// declined to grant ownership.
    ///
    /// The opposite direction, and the expected one: static analysis gives up
    /// on spellings a running program has no doubt about. These are the calls a
    /// runtime decision would convert from copy to mutate-in-place, so the
    /// number is the payoff preview for that decision — and nothing asserts on
    /// it, because a static decline is always allowed.
    pub unique_slot_without_owned_grant: u64,
}

impl VmSlotUniquenessStats {
    pub fn merge(&mut self, other: &Self) {
        self.owned_grants += other.owned_grants;
        self.owned_grants_without_unique_slot += other.owned_grants_without_unique_slot;
        self.unique_slot_without_owned_grant += other.unique_slot_without_owned_grant;
    }
}

/// The interpreter's operand stack, carrying a live-reference count per arena
/// slot.
#[derive(Debug)]
pub(crate) struct OperandStack {
    cells: Vec<NanValue>,
    /// `refs[space][index]` — how many cells hold that slot. One vector per
    /// heap space, keyed the way `Arena`'s own per-slot side tables are: the
    /// spaces have separate entry vectors, so a single flat table over the
    /// encoded index would be four times as sparse as the arena it shadows.
    ///
    /// A slot the arena truncates away is left with whatever count it had. That
    /// is sound because the invariant says the count of a slot no cell holds is
    /// zero, and a slot inside a region a boundary drops is exactly that: the
    /// cells that could have named it were truncated first. The next allocation
    /// to land on the index therefore finds a zero. `audit` is what turns a
    /// violation of that into a message instead of a silent leak.
    refs: [Vec<u32>; HEAP_SPACE_COUNT],
    stats: VmSlotUniquenessStats,
}

impl OperandStack {
    pub(crate) fn with_capacity(capacity: usize) -> Self {
        OperandStack {
            cells: Vec::with_capacity(capacity),
            refs: std::array::from_fn(|_| Vec::new()),
            stats: VmSlotUniquenessStats::default(),
        }
    }

    // -- the count ---------------------------------------------------------

    #[inline(always)]
    fn retain(&mut self, value: NanValue) {
        if !value.may_hold_heap_index() {
            return;
        }
        let Some(index) = value.heap_index() else {
            return;
        };
        let (space, slot) = split_heap_index(index);
        let counts = &mut self.refs[space];
        if counts.len() <= slot {
            counts.resize(slot + 1, 0);
        }
        counts[slot] += 1;
    }

    #[inline(always)]
    fn release(&mut self, value: NanValue) {
        if !value.may_hold_heap_index() {
            return;
        }
        let Some(index) = value.heap_index() else {
            return;
        };
        let (space, slot) = split_heap_index(index);
        if let Some(count) = self.refs[space].get_mut(slot) {
            *count = count.saturating_sub(1);
        }
    }

    /// How many cells currently hold `index`.
    #[inline]
    pub(crate) fn live_refs(&self, index: u32) -> u32 {
        let (space, slot) = split_heap_index(index);
        self.refs[space].get(slot).copied().unwrap_or(0)
    }

    /// Whether `value`, having just been taken OFF the stack, was the only cell
    /// holding its slot.
    ///
    /// Read it at the point where the argument list has already been popped:
    /// "no cell left holds this" is the same statement as "the cell that held
    /// it was the only one", and it needs no correction for the argument's own
    /// reference. An immediate answers `false` — it names no slot, so there is
    /// nothing to hold uniquely and nothing to mutate in place.
    #[inline]
    pub(crate) fn slot_is_unheld(&self, value: NanValue) -> bool {
        value
            .heap_index()
            .is_some_and(|index| self.live_refs(index) == 0)
    }

    pub(crate) fn stats(&self) -> VmSlotUniquenessStats {
        self.stats
    }

    pub(crate) fn stats_mut(&mut self) -> &mut VmSlotUniquenessStats {
        &mut self.stats
    }

    /// Total live references across every slot — zero exactly when no cell on
    /// the stack holds an arena index.
    pub(crate) fn total_live_refs(&self) -> u64 {
        self.refs
            .iter()
            .flat_map(|counts| counts.iter())
            .map(|count| *count as u64)
            .sum()
    }

    // -- audit -------------------------------------------------------------

    /// Recompute the count from the cells and report the first slot the
    /// incrementally-maintained one disagrees on.
    ///
    /// This is the definition checked against its maintenance. It is `O(cells +
    /// slots)`, so the interpreter runs it only under
    /// `VM::set_slot_ref_audit`, which the bookkeeping tests turn on — and it
    /// exists only where the assertions do, alongside its one caller.
    #[cfg(debug_assertions)]
    pub(crate) fn audit(&self) -> Result<(), String> {
        let mut expected: [Vec<u32>; HEAP_SPACE_COUNT] = std::array::from_fn(|_| Vec::new());
        for cell in &self.cells {
            let Some(index) = cell.heap_index() else {
                continue;
            };
            let (space, slot) = split_heap_index(index);
            let counts = &mut expected[space];
            if counts.len() <= slot {
                counts.resize(slot + 1, 0);
            }
            counts[slot] += 1;
        }
        for (space, (held_counts, want_counts)) in self.refs.iter().zip(expected.iter()).enumerate()
        {
            let width = held_counts.len().max(want_counts.len());
            for slot in 0..width {
                let held = held_counts.get(slot).copied().unwrap_or(0);
                let want = want_counts.get(slot).copied().unwrap_or(0);
                if held != want {
                    return Err(format!(
                        "slot ref count drifted: space {space} slot {slot} \
                         bookkeeping says {held}, the {} stack cells say {want}",
                        self.cells.len()
                    ));
                }
            }
        }
        Ok(())
    }

    // -- Vec-shaped surface ------------------------------------------------

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.cells.len()
    }

    #[inline]
    pub(crate) fn last(&self) -> Option<&NanValue> {
        self.cells.last()
    }

    #[inline]
    pub(crate) fn iter(&self) -> std::slice::Iter<'_, NanValue> {
        self.cells.iter()
    }

    #[inline]
    pub(crate) fn push(&mut self, value: NanValue) {
        self.retain(value);
        self.cells.push(value);
    }

    #[inline]
    pub(crate) fn pop(&mut self) -> Option<NanValue> {
        let value = self.cells.pop();
        if let Some(value) = value {
            self.release(value);
        }
        value
    }

    pub(crate) fn truncate(&mut self, len: usize) {
        if len >= self.cells.len() {
            return;
        }
        for index in len..self.cells.len() {
            let value = self.cells[index];
            self.release(value);
        }
        self.cells.truncate(len);
    }

    pub(crate) fn remove(&mut self, index: usize) -> NanValue {
        let value = self.cells.remove(index);
        self.release(value);
        value
    }

    pub(crate) fn resize(&mut self, len: usize, value: NanValue) {
        if len <= self.cells.len() {
            self.truncate(len);
            return;
        }
        for _ in self.cells.len()..len {
            self.push(value);
        }
    }

    /// Move a run of cells to `dest`, overlapping ranges included — the count
    /// side of `Vec::copy_within`.
    pub(crate) fn copy_within(&mut self, src: Range<usize>, dest: usize) {
        for offset in 0..src.len() {
            // Read before write, and walk in the direction that cannot read a
            // cell the same call has already overwritten.
            let offset = if dest > src.start {
                src.len() - 1 - offset
            } else {
                offset
            };
            let value = self.cells[src.start + offset];
            self.store(dest + offset, value);
        }
    }

    // -- writes that a `Vec` would have done through `IndexMut` ------------

    /// Write one cell — the replacement for `stack[i] = v`, which cannot be an
    /// `IndexMut` here because handing out a `&mut NanValue` would let the
    /// caller overwrite a cell with the count looking the other way.
    #[inline]
    pub(crate) fn store(&mut self, index: usize, value: NanValue) {
        let previous = self.cells[index];
        self.release(previous);
        self.retain(value);
        self.cells[index] = value;
    }

    /// Overwrite the top cell in place.
    pub(crate) fn store_last(&mut self, value: NanValue) -> bool {
        match self.cells.len().checked_sub(1) {
            Some(index) => {
                self.store(index, value);
                true
            }
            None => false,
        }
    }

    /// Replace every cell, then recompute the count from scratch.
    ///
    /// The one caller is the stable collection that takes the whole stack as
    /// roots and hands it back relocated (`VM::collect_live_vm_roots`). Nothing
    /// incremental could be right there: the cells come back naming slots that
    /// did not exist when they went in, so the count is rebuilt rather than
    /// adjusted.
    pub(crate) fn overwrite_all(&mut self, values: &[NanValue]) {
        self.cells.copy_from_slice(values);
        self.rebuild_counts();
    }

    /// Recompute the count from the cells.
    pub(crate) fn rebuild_counts(&mut self) {
        for counts in &mut self.refs {
            counts.clear();
        }
        for index in 0..self.cells.len() {
            let value = self.cells[index];
            self.retain(value);
        }
    }
}

impl VM {
    /// Compare what the compiler's owned mask granted against what the runtime
    /// count sees, at a builtin call whose arguments have just been popped.
    ///
    /// The comparison is DIRECTIONAL on purpose. A static grant is a claim that
    /// nothing else can observe the mutation, so `granted ⊆ runtime-unique` has
    /// to hold and a violation is a bug — in the whitelist that granted it or in
    /// the bookkeeping that counted it — which is why it is an assertion where
    /// assertions are on and a counter where they are not. The other direction
    /// carries no obligation at all: static analysis is allowed to decline
    /// anything, and a decline the runtime could see through is the whole point
    /// of asking, so it is only ever counted.
    ///
    /// Phase P1 takes no decision from either number.
    pub(super) fn cross_check_owned_mask(
        &mut self,
        builtin: VmBuiltin,
        args: &[NanValue],
        owned_mask: u8,
    ) {
        // Only the receiver of a builtin that can mutate its target in place is
        // worth comparing. Every other argument is declined by a mask that
        // never had a bit for it, so counting those would drown the number that
        // means something in one that means "this builtin does not mutate".
        if !matches!(
            builtin,
            VmBuiltin::MapSet | VmBuiltin::VectorSet | VmBuiltin::MapRemove
        ) {
            return;
        }
        let Some(target) = args.first().copied() else {
            return;
        };
        if target.heap_index().is_none() {
            // An immediate receiver — an empty map or vector — owns no slot, so
            // there is nothing to hold uniquely and nothing to mutate.
            return;
        }
        let granted = owned_mask & 1 != 0;
        let unheld = self.stack.slot_is_unheld(target);
        let stats = self.stack.stats_mut();
        if granted {
            stats.owned_grants += 1;
            if !unheld {
                stats.owned_grants_without_unique_slot += 1;
            }
        } else if unheld {
            stats.unique_slot_without_owned_grant += 1;
        }
        debug_assert!(
            !granted || unheld,
            "{} was granted ownership of its target statically, but {} operand \
             stack cell(s) still hold that arena slot",
            builtin.name(),
            target
                .heap_index()
                .map(|index| self.stack.live_refs(index))
                .unwrap_or(0),
        );
    }
}

impl Index<usize> for OperandStack {
    type Output = NanValue;

    #[inline]
    fn index(&self, index: usize) -> &NanValue {
        &self.cells[index]
    }
}

impl Index<Range<usize>> for OperandStack {
    type Output = [NanValue];

    #[inline]
    fn index(&self, range: Range<usize>) -> &[NanValue] {
        &self.cells[range]
    }
}

impl Index<RangeFrom<usize>> for OperandStack {
    type Output = [NanValue];

    #[inline]
    fn index(&self, range: RangeFrom<usize>) -> &[NanValue] {
        &self.cells[range]
    }
}
