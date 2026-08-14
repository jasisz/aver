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
//! enough that this is not cheap. Against `origin/main`, serially, with the two
//! binaries interleaved and per-round spreads under 0.06: naive `fib` cost 27%
//! more, a JSON parse-and-render loop 14%, a vector fill-and-sum 32%. Splitting
//! the table update out behind a non-inlined call took those to 13%, 13% and
//! 27% — a `push` whose tail can reach `Vec::resize` stops being inlined into
//! the dispatch loop at all, which was half the bill. There was nothing left to
//! win after that: what remains is the test itself, on a path a program that
//! never touches a collection still pays for.
//!
//! Computing the count where it is READ inverts that distribution. The question
//! is asked at one instruction — a collection builtin about to write — and the
//! answer costs one pass over the live cells, bounded by the call depth. A
//! program with no collection writes pays nothing at all, and a program with
//! many pays a walk of the stack against a copy of the collection it is
//! deciding about: the thing being avoided is `O(size)`, the question is
//! `O(depth)`.
//!
//! ## Who pays for the walk
//!
//! Nobody, unless they asked. The comparison decides nothing in P1, so the only
//! consumers are the assertion — compiled out of a release build — and the
//! `--profile` report. [`VM::cross_check_owned_mask`] therefore skips the walk
//! entirely in a release build that did not ask for a profile, and a default
//! release run pays one predictable branch per collection write instead.
//!
//! That gate is not bookkeeping tidiness. The shape that pays is a program
//! writing to a collection from the bottom of a deep NON-TAIL recursion: every
//! suspended frame's locals are cells the walk has to visit, and the granted
//! case — the in-place fold the optimizer worked hardest on — is precisely the
//! one where the walk finds no holder and therefore scans all of them. Measured
//! at depth 2000 with two million inserts underneath it, `--release`, serial,
//! the binaries interleaved, per-round spreads under 0.04: walking
//! unconditionally cost 12.9x against `main`, and the gate takes that to 0.99.
//! The same gated binary run with `--profile` is back at 3.61 s against the
//! ungated 3.38 s, which is the point: the walk is not gone, it is charged to
//! the run that asked for it. Capping the walk was the other option and is
//! worse, because the answer would then depend on how deeply the program
//! happened to be nested.
//!
//! ## What the count is NOT
//!
//! It counts operand-stack cells and nothing else. A slot also reachable from a
//! global, from a chunk constant, or from inside another arena entry reads as
//! unheld here. That is deliberate for phase P1, which decides nothing: the
//! count is observed, and the only thing consulted is the DIRECTIONAL
//! cross-check against the static owned mask. Anything that later wants to take
//! a decision from it owes the other holders a separate argument.
//!
//! ## What the cross-check never sees
//!
//! Four blind spots. All four undercount and none costs a guard, but the last
//! two are the reason the numbers below are a MAP-shaped picture, and P2 owes
//! each of them an answer before a decision is taken from any of this.
//!
//! - A parallel independent product gives each branch a VM of its own, so a
//!   branch's writes are counted there and go with it when its arena is dropped
//!   at the join. A branch that broke the soundness direction still asserts
//!   inside its own VM before the join is reached, so what is lost is the
//!   tally, not the check.
//! - A collection builtin reached as a first-class value goes through
//!   `CALL_VALUE`, which carries no owned mask and takes no owned path. There
//!   is no grant there to check; a write the runtime could have seen through
//!   goes uncounted.
//! - A DECLINED `Vector.set` never reaches this code at all. `mir` lowers it to
//!   the dedicated `VECTOR_SET` opcode and only routes the owned spelling
//!   through `CALL_BUILTIN_OWNED`, so `unique_slot_without_owned_grant` — the
//!   number that previews what a runtime decision would buy — is a statement
//!   about maps. P2 sizing the payoff from it would be reading a Map-shaped
//!   floor and calling it the whole picture.
//! - `VECTOR_SET_OR_KEEP`'s owned branch is not audited here, and the predicate
//!   does not extend to it. That branch is the VM's only true in-place arena
//!   write — the one static grant where a whitelist error rewrites an entry
//!   another holder can still read, which is exactly what the soundness
//!   direction exists to catch — and its grant is `vec_last_use ||
//!   def_last_use`. In the fused shape the inner vector read compiles to
//!   `LOAD_LOCAL` rather than `MOVE_LOCAL`, because `last_use` annotates the
//!   textually-last read, which the fusion deletes. So the target's own local
//!   cell is STILL LIVE at the write, and [`VM::slot_is_unheld`] would answer
//!   HELD at the strongest grant in the VM. The trick that makes the builtin
//!   check work — ask once the argument list has been popped — has no analogue
//!   at a fused opcode that never builds one. This one needs a predicate of its
//!   own, not another call site, and that is P2's to design.
//!
//! The two vector items are pinned rather than asserted in prose:
//! `a_vector_fold_writes_nothing_the_cross_check_can_see` in
//! `tests/vm_slot_uniqueness.rs` runs both vector spellings and expects all four
//! tallies to stay at zero, so the day either one starts being observed, that
//! test is what says the list above needs rewriting.

use super::VM;
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;

/// Tallies from the directional cross-check between the compiler's static owned
/// mask and the runtime reference count.
///
/// Every collection write the cross-check sees lands in exactly one of three
/// buckets — granted, declined with nobody else holding the slot, declined with
/// somebody holding it — so the three add up to the writes observed, and a
/// write that moves between buckets moves two numbers at once. That is what the
/// exit-path tests read: a frame that left a cell of its own standing turns a
/// declined-and-unheld write into a declined-and-held one.
///
/// They are per-VM and they are not maintained on the default release path: see
/// the module for who pays for the walk, and for the four writes the
/// cross-check never sees at all.
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
    /// It is a preview, not a list of grants, and it is a preview of the MAP
    /// side. The count cannot see a global or a chunk constant holding the same
    /// slot, and a map literal's first insert targets exactly that; a declined
    /// `Vector.set` never reaches the cross-check at all, because it is lowered
    /// to its own opcode. Whoever turns this into a decision owes both the other
    /// root sets and the vector side an argument of their own.
    pub unique_slot_without_owned_grant: u64,
    /// Collection targets another operand-stack cell was still holding where the
    /// static mask also declined to grant ownership.
    ///
    /// The two analyses agreeing, which is the uninteresting case for a decision
    /// and the load-bearing one for a test: it is the only tally that grows when
    /// a reference genuinely exists at the moment of a write, so a shape whose
    /// holders are known pins a number that is not zero. A frame that failed to
    /// give back a cell on the way out shows up here, one write at a time, as
    /// the same write leaving `unique_slot_without_owned_grant`.
    pub declined_with_slot_still_held: u64,
}

impl VmSlotUniquenessStats {
    pub fn merge(&mut self, other: &Self) {
        self.owned_grants += other.owned_grants;
        self.owned_grants_without_unique_slot += other.owned_grants_without_unique_slot;
        self.unique_slot_without_owned_grant += other.unique_slot_without_owned_grant;
        self.declined_with_slot_still_held += other.declined_with_slot_still_held;
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
    /// A whole-stack figure, useful where the stack is under the caller's
    /// control — the VM's own tests build one cell at a time. It is worth being
    /// clear about what it does NOT establish: reading it after a run has
    /// finished says nothing about any exit path in particular, because a
    /// successful run ends with an empty stack whatever an inner frame did. Its
    /// caller truncates to its own base on the way out and erases the evidence.
    /// The exit paths are pinned where the answer is used instead — see
    /// `tests/vm_slot_uniqueness.rs`.
    pub fn live_slot_refs(&self) -> u64 {
        self.stack
            .iter()
            .filter(|cell| cell.heap_index().is_some())
            .count() as u64
    }

    /// Tallies from the cross-check against the compiler's static owned mask.
    ///
    /// All zero from a release build that was not asked for a profile: the
    /// comparison behind them is skipped there rather than computed for nobody.
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
        // Only a builtin the runtime really hands its target to is worth
        // comparing: `invoke_builtin_with_owned` takes the owned path for
        // `Map.set` and `Vector.set` and falls through to the copying call for
        // everything else, so a mask bit on any other builtin grants nothing
        // and mutates nothing. `Map.remove` is the one that looks like it
        // belongs here and does not — the mask reaches it, the runtime ignores
        // it — and counting it would put writes that never happened in the same
        // number as writes that did.
        if !matches!(builtin, VmBuiltin::MapSet | VmBuiltin::VectorSet) {
            return;
        }
        // Nobody is listening on the default release path, so nobody pays for
        // the walk there. The assertion below is compiled out where assertions
        // are off, and the tallies are only ever read back out of the profile
        // report — so with neither in play the answer would be computed and
        // dropped, once per collection write, at a cost that grows with how
        // deeply the program happens to be nested. A debug build keeps the full
        // audit, and `--profile` buys it back in a release one.
        if !cfg!(debug_assertions) && self.profile.is_none() {
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
        match (granted, unheld) {
            (true, true) => self.slot_uniqueness.owned_grants += 1,
            (true, false) => {
                self.slot_uniqueness.owned_grants += 1;
                self.slot_uniqueness.owned_grants_without_unique_slot += 1;
            }
            (false, true) => self.slot_uniqueness.unique_slot_without_owned_grant += 1,
            (false, false) => self.slot_uniqueness.declined_with_slot_still_held += 1,
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
