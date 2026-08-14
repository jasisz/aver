//! How many live references an arena slot has, and what the compiler thought.
//!
//! Locals are a WINDOW on the operand stack (`CallFrame::bp` + `local_count`),
//! not a separate array, so at this layer "how many live references does this
//! arena slot have?" has one honest answer: how many cells of `VM::stack` hold
//! its index. [`VM::live_refs_to_slot`] is that count.
//!
//! ## Why it is not kept incrementally
//!
//! The obvious carrier is a per-slot counter maintained on every stack move,
//! and it was built and measured first. Maintaining it means one test in front
//! of every `push` and every `pop`, and the interpreter's opcodes are small
//! enough that this is not cheap: against `origin/main`, naive `fib` cost 13%
//! more, a JSON parse-and-render loop 13%, and a vector fill-and-sum 27%, at
//! per-round spreads under 0.06. Splitting the table update out behind a
//! non-inlined call — a `push` whose tail can reach `Vec::resize` stops being
//! inlined into the dispatch loop at all — took `fib` from 27% down to 13%, and
//! there was nothing left to win after that: the remainder is the test itself,
//! on a path a program that never touches a collection still pays for.
//!
//! Computing the count where it is READ inverts that distribution. The question
//! is asked at one instruction — a collection builtin about to write — and the
//! answer costs one pass over the live cells, bounded by the call depth. A
//! program with no collection writes pays nothing at all, and a program with
//! many pays a walk of the stack against a copy of the collection it is
//! deciding about: the thing being avoided is `O(size)`, the question is
//! `O(depth)`.
//!
//! The pathological shape is a program writing to a small collection from the
//! bottom of a very deep recursion, where the walk is long and the copy it would
//! replace is short. Nothing in the corpus is that shape, and it is worth saying
//! out loud rather than capping the walk: a cap would make the answer depend on
//! how deeply the program happened to be nested.
//!
//! ## What the count is NOT
//!
//! It counts operand-stack cells and nothing else. A slot also reachable from a
//! global, from a chunk constant, or from inside another arena entry reads as
//! unheld here. That is deliberate for phase P1, which decides nothing: the
//! count is observed, and the only thing consulted is the DIRECTIONAL
//! cross-check against the static owned mask. Anything that later wants to take
//! a decision from it owes the other holders a separate argument.

use super::VM;
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;

/// Tallies from the directional cross-check between the compiler's static owned
/// mask and the runtime reference count.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct VmSlotUniquenessStats {
    /// Heap-backed collection targets a `CALL_BUILTIN_OWNED` mask granted
    /// ownership of.
    pub owned_grants: u64,
    /// Of those, the ones the runtime count did NOT agree were uniquely held.
    ///
    /// This is the soundness direction: the static grant is a claim that
    /// nothing else can observe the mutation, so it has to be a subset of what
    /// the runtime sees as unique. A non-zero here is a bug in the whitelist or
    /// a bug in the counting, never a tolerable difference — which is why the
    /// same event is a `debug_assert` in a debug build and only falls back to
    /// being counted where assertions are compiled out.
    pub owned_grants_without_unique_slot: u64,
    /// Collection targets the runtime count saw as uniquely held where the
    /// static mask declined to grant ownership.
    ///
    /// The opposite direction, and the expected one: static analysis gives up on
    /// spellings a running program has no doubt about. These are the writes a
    /// runtime decision would turn from copy into mutate-in-place, so the number
    /// previews that payoff — and nothing asserts on it, because a static
    /// decline is always allowed.
    ///
    /// It is a preview, not a list of grants. The count cannot see a global or a
    /// chunk constant holding the same slot, and a map literal's first insert
    /// targets exactly that, so whoever turns this into a decision owes the
    /// other root sets an argument of their own.
    pub unique_slot_without_owned_grant: u64,
}

impl VmSlotUniquenessStats {
    pub fn merge(&mut self, other: &Self) {
        self.owned_grants += other.owned_grants;
        self.owned_grants_without_unique_slot += other.owned_grants_without_unique_slot;
        self.unique_slot_without_owned_grant += other.unique_slot_without_owned_grant;
    }
}

impl VM {
    /// How many operand-stack cells hold `index`.
    ///
    /// Locals live in the same vector as the working area, so one pass over it
    /// sees every local of every suspended frame as well as every partially
    /// built argument list — which is the point: an argument of an enclosing
    /// call is exactly the holder a per-frame view would miss.
    pub fn live_refs_to_slot(&self, index: u32) -> u32 {
        self.stack
            .iter()
            .filter(|cell| cell.may_hold_heap_index() && cell.heap_index() == Some(index))
            .count() as u32
    }

    /// Whether `value`, having just been taken OFF the stack, was the only cell
    /// holding its slot.
    ///
    /// Read it once the argument list is popped: "no cell left holds this" and
    /// "the cell that held it was the only one" are then the same statement, so
    /// nothing has to correct for the argument's own reference. It walks from
    /// the top down and stops at the first holder, because the nearest alias is
    /// the likeliest one — a local of the frame making the call.
    ///
    /// An immediate answers `false`: it names no slot, so there is nothing to
    /// hold uniquely and nothing to mutate in place.
    pub fn slot_is_unheld(&self, value: NanValue) -> bool {
        let Some(index) = value.heap_index() else {
            return false;
        };
        !self
            .stack
            .iter()
            .rev()
            .any(|cell| cell.may_hold_heap_index() && cell.heap_index() == Some(index))
    }

    /// Total live references the operand stack holds across every arena slot.
    ///
    /// Zero once a run has finished is the whole-program form of "the count
    /// returns to zero at frame exit": every exit path the run went through gave
    /// back what it took, or a cell would still be standing here.
    pub fn live_slot_refs(&self) -> u64 {
        self.stack
            .iter()
            .filter(|cell| cell.heap_index().is_some())
            .count() as u64
    }

    /// Tallies from the cross-check against the compiler's static owned mask.
    pub fn slot_uniqueness_stats(&self) -> VmSlotUniquenessStats {
        self.slot_uniqueness
    }

    /// Compare what the compiler's owned mask granted against what the runtime
    /// count sees, at a builtin call whose arguments have just been popped.
    ///
    /// The comparison is DIRECTIONAL on purpose. A static grant claims nothing
    /// else can observe the mutation, so `granted ⊆ runtime-unique` has to hold,
    /// and a violation is a bug — in the whitelist that granted it or in the
    /// counting that checked it — which is why it is an assertion where
    /// assertions are on and a counter where they are not. The other direction
    /// carries no obligation at all: static analysis is allowed to decline
    /// anything, and a decline the runtime can see through is the whole reason
    /// for asking, so it is only ever counted.
    ///
    /// Phase P1 takes no decision from either number.
    pub(super) fn cross_check_owned_mask(
        &mut self,
        builtin: VmBuiltin,
        args: &[NanValue],
        owned_mask: u8,
    ) {
        // Only the receiver of a builtin that can mutate its target in place is
        // worth comparing. Every other argument is declined by a mask that never
        // had a bit for it, so counting those would drown the number that means
        // something in one that means "this builtin does not mutate".
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
        let unheld = self.slot_is_unheld(target);
        if granted {
            self.slot_uniqueness.owned_grants += 1;
            if !unheld {
                self.slot_uniqueness.owned_grants_without_unique_slot += 1;
            }
        } else if unheld {
            self.slot_uniqueness.unique_slot_without_owned_grant += 1;
        }
        debug_assert!(
            !granted || unheld,
            "{} was granted ownership of its target statically, but {} operand \
             stack cell(s) still hold that arena slot",
            builtin.name(),
            target
                .heap_index()
                .map(|index| self.live_refs_to_slot(index))
                .unwrap_or(0),
        );
    }
}
