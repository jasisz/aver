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
//! Nobody, unless they asked. The comparison decides nothing — the decision
//! below asks its own question — so the only consumers are the assertions,
//! compiled out of a release build, and the `--profile` report.
//! [`VM::cross_check_owned_mask`] therefore skips the walk entirely in a release
//! build that did not ask for a profile, and a default release run pays one
//! predictable branch per collection write instead.
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
//! ## What the count is NOT — and what a decision needs on top of it
//!
//! It counts operand-stack cells and nothing else. A slot also reachable from a
//! global, from a chunk constant, or from inside another arena entry reads as
//! unheld here. That is why phase P1 decided nothing from it.
//!
//! [`VM::runtime_owns_map_target`] — phase P2 — is the decision, and it pays the
//! other holders what P1 said they were owed. Emptying a slot is safe when NO
//! holder is left, and the holders split into two sets that need two different
//! instruments:
//!
//! - the consumer's own handles: every one of them is an operand-stack cell,
//!   and the walk above enumerates them exactly. That is a property the
//!   interpreter has to KEEP, not one it is given — see the independent
//!   product below, which held handles in Rust locals until it was made to
//!   leave them where they could be seen;
//! - everything else: another arena entry, a global, a chunk constant. Those
//!   cannot be enumerated per write, so they are recorded when the reference is
//!   MADE. `ArenaEntry::Map`'s `held_elsewhere` flag is that record, marked by
//!   `Arena::push` for every map an entry directly holds, by `Arena::push_map`
//!   for a table's own keys and values, and by this VM for the globals it
//!   stores and the constants it starts with.
//!
//! A grant needs both to say no, and the flag is checked first because it costs
//! one arena read against a walk.
//!
//! Having the flag lets the cross-check ask the STATIC mask the same question it
//! could only ever ask half of before. A whitelist that granted a map another
//! arena entry, a global or a chunk constant still holds would be exactly as
//! wrong as a runtime grant that did, and until there was somewhere to read that
//! from, the walk was the only instrument and it cannot see any of the three.
//! [`VM::cross_check_owned_mask`] now asserts both halves. The one shape that
//! trips the second half and is not a bug is a constant EMPTY map, which the
//! chunk table holds and a fold's first insert takes; taking an empty table in
//! place is the identity, so the assertion excludes it by size and nothing in the
//! corpus reaches it any other way.
//!
//! ## Who pays for the decision's walk
//!
//! The walk buys an `O(size)` duplication of the table and costs `O(depth)`, so
//! it is worth taking exactly when the stack is not deeper than the map is big.
//! That is the guard: [`WALK_SLACK`] cells of headroom over the map's own
//! length, and above that the write copies without looking. The measurements
//! behind the number are on the constant.
//!
//! ## The root set that was neither walked nor marked
//!
//! An independent product does NOT always give each branch a VM of its own.
//! `CALL_PAR` takes that arm only when it can: recording or replaying effects,
//! and a verify-time oracle-stub map, both keep state on the parent's runtime
//! that cannot be shared across threads, so the branches run one after another
//! ON THIS VM, against THIS arena. `aver run --record` reaches it, and so does
//! `aver verify` on an effectful law with `given` stubs.
//!
//! That arm used to pop every branch's callable and arguments off the operand
//! stack into Rust locals before running the first one, which put the siblings'
//! handles somewhere neither instrument looks: not a cell, so the walk found
//! nothing, and never marked, so the flag said nothing. A branch could take a
//! map in place that the branch after it was about to read, and the map came
//! back empty — a wrong grant, not an undercount, and the mirror agreed with the
//! decision because it searches the same four places.
//!
//! The cells therefore stay where they are for the length of the product, and
//! the branch being entered blanks its OWN bundle first, because a callee takes
//! its arguments as locals of its own frame — that is where every other call in
//! the VM leaves them, and a cell here as well would make a branch's own
//! argument read as somebody else's reference. What is left standing is exactly
//! the handles that have not been handed anywhere yet, plus the results of the
//! branches that have already finished. Both instruments see them, because both
//! read the operand stack, and neither needed a special case.
//!
//! `a_branch_of_an_independent_product_cannot_take_a_map_its_sibling_holds` in
//! `tests/vm_map_runtime_ownership.rs` is the shape, and the same file's
//! reader-first program is what says the fence is a record of which handles are
//! in flight rather than a refusal to decide inside a product: with the write
//! LAST, nothing holds the map any more and the grant is taken.
//!
//! ## What the decision never sees
//!
//! Four blind spots, all of them shared with the cross-check, and every one
//! still UNDERCOUNTS — a write the decision cannot see is a write that copies,
//! which is what the program did before any of this existed. None of the four
//! can produce a wrong grant, and the one that could is the section above.
//! What each one owes a later phase:
//!
//! - A parallel independent product — the arm above that DOES give each branch
//!   a VM of its own — counts a branch's writes there, and they go with it when
//!   its arena is dropped at the join. The decision is still taken correctly
//!   inside the branch: its own stack and its own arena are the whole world it
//!   can reach, and the join deep-imports rather than sharing slots. What is
//!   lost is only the tally. P3 owes this a merge at the join, not a guard.
//! - A collection builtin reached as a first-class value goes through
//!   `CALL_VALUE`, which carries no owned mask and never reaches the decision,
//!   so `Map.set` used as a value always copies. P3 owes this the same call —
//!   `CALL_VALUE` pops its arguments in the same place, so it is a call site to
//!   add, not a predicate to design.
//! - A DECLINED `Vector.set` never reaches this code at all. `mir` lowers it to
//!   the dedicated `VECTOR_SET` opcode (which always copies) and only routes
//!   the owned spelling through `CALL_BUILTIN_OWNED`. So the upgrade decision
//!   is a MAP decision, and the vector half of that payoff is still owed to
//!   P3. What the vector route DOES have now is the reverse direction:
//!   [`VM::runtime_confirms_vector_grant`] verifies every STATIC vector grant
//!   against the same two holder sets before `vec_set_nv_owned` is allowed to
//!   `mem::take` the slot, because the match-binder hole showed a static grant
//!   reaching a container-held vector and draining it silently. Vectors carry
//!   the `held_elsewhere` record now (`ArenaEntry::Vector`), marked through
//!   the same choke points as maps.
//! - `VECTOR_SET_OR_KEEP`'s owned branch is not audited here, and the predicate
//!   does not extend to it — which is why P2 does not touch it. That branch is
//!   the VM's only true in-place arena write, and its grant is `vec_last_use ||
//!   def_last_use`. In the fused shape the inner vector read compiles to
//!   `LOAD_LOCAL` rather than `MOVE_LOCAL`, because `last_use` annotates the
//!   textually-last read, which the fusion deletes. So the target's own local
//!   cell is STILL LIVE at the write, and [`VM::slot_is_unheld`] would answer
//!   HELD at the strongest grant in the VM. The trick that makes the builtin
//!   check work — ask once the argument list has been popped — has no analogue
//!   at a fused opcode that never builds one. P3 owes this a predicate of its
//!   own that excludes the target's own cell, not another call site.
//!
//! The two vector items are pinned rather than asserted in prose:
//! `a_vector_fold_writes_nothing_the_cross_check_can_see` in
//! `tests/vm_slot_uniqueness.rs` runs both vector spellings and expects all four
//! tallies to stay at zero, so the day either one starts being observed, that
//! test is what says the list above needs rewriting.
//!
//! ## The way out
//!
//! `AVER_NO_RUNTIME_MAP_OWNERSHIP=1` puts every declined write back on the
//! copying path without a rebuild. It is here because this is a transition on a
//! soundness surface and a field problem should not need a release to answer;
//! it is not a tuning knob, and P3 removes it once the transition has been
//! green for a while.

use std::sync::OnceLock;

use super::VM;
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;

/// How many operand-stack cells the decision will walk over and above the
/// number of map entries a copy would move.
///
/// The walk replaces an `O(size)` duplication with an `O(depth)` search, so the
/// bound that makes it always worth taking is `depth <= size`. That bound alone
/// gives up on the small maps, where the copy is cheap but a grant is what stops
/// the NEXT copy from being bigger — a three-entry literal seed never gets to
/// look, and a fold's first steps pay before the accumulator outgrows the frame
/// it runs in. The slack is what buys those back, and it is a constant, so what
/// a program can lose to a walk that finds a holder is a constant per write and
/// not a factor.
///
/// 128 is where the two costs actually cross, measured rather than guessed. The
/// probe is the worst ratio a program can offer: 200,000 declined writes to a
/// two-entry map, made from the bottom of a non-tail recursion of a given depth,
/// timed `--release` and serially with the binaries interleaved. Against a build
/// that always copies, a build that always walks runs 0.82x at depth 4, 0.85x at
/// 16, and 0.99x at 32 — that is the crossing — then 1.18x at 64, 1.79x at 200
/// and 9.9x at 2000. A frame of that shape occupies about four cells, so depth 32
/// is about 128 cells, and a build cut off there tracks the always-walking one up
/// to the crossing (0.89x at depth 24, 1.00x at 32) and the always-copying one
/// after it (1.02x to 1.05x from depth 48 all the way to 2000). Everything the
/// walk was winning is kept and the factor is gone.
///
/// The 1.05x that remains is what asking costs when the answer is no: one arena
/// read and one comparison per declined write, on a program that does nothing
/// else. It is a constant, and the shapes this exists for pay it once and then
/// stop — see the pull request body for the folds.
const WALK_SLACK: usize = 128;

/// Whether the runtime decision is switched on at all.
///
/// Read once. The transition needs a way back to the copying path that does not
/// need a rebuild, and it needs it to be the same answer for a whole run — a
/// decision that changed halfway through would leave the two halves of a fold
/// disagreeing about who owns the accumulator.
fn runtime_ownership_enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("AVER_NO_RUNTIME_MAP_OWNERSHIP").is_none())
}

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

/// What the runtime decided about the map writes the compiler declined.
///
/// Every write the compiler declined lands in exactly one of these four, so they
/// add up to the declined writes the run made and a write that changes its mind
/// moves two numbers at once. Unlike [`VmSlotUniquenessStats`] these are
/// maintained on every path, release included: they are the receipt for a
/// decision that was taken, not an observation somebody asked for.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct VmRuntimeOwnershipStats {
    /// Writes the compiler declined and the runtime handed to the owned path
    /// anyway, because nothing at all held the target's slot.
    pub grants: u64,
    /// Refused: another operand-stack cell still held the slot.
    ///
    /// A live local of this frame or of one suspended under it, or an argument
    /// half-built for an enclosing call. The whole shape of the aliasing corpus
    /// — a rename, a second binding, a value handed to a function that gave it
    /// back — lands here.
    pub refused_stack_holder: u64,
    /// Refused: something outside the operand stack held the slot — an arena
    /// entry, a global, a chunk constant.
    ///
    /// Read off `held_elsewhere`, which is recorded where the reference is made
    /// rather than searched for here. A map extracted from a record field or
    /// from another map's value lands here, and it is the one refusal the walk
    /// alone could never have made.
    pub refused_off_stack_holder: u64,
    /// Not examined: the walk would have cost more than the copy it was trying
    /// to avoid, so the write copied without asking.
    ///
    /// See [`WALK_SLACK`]. This is the only bucket that can hold a write the
    /// runtime WOULD have granted, which is what makes it worth its own number:
    /// a program that suddenly stops going linear says so here.
    pub unexamined_walk_too_costly: u64,
}

impl VmRuntimeOwnershipStats {
    pub fn merge(&mut self, other: &Self) {
        self.grants += other.grants;
        self.refused_stack_holder += other.refused_stack_holder;
        self.refused_off_stack_holder += other.refused_off_stack_holder;
        self.unexamined_walk_too_costly += other.unexamined_walk_too_costly;
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
        // `Map.set`, `Map.remove` and `Vector.set` and falls through to the
        // copying call for everything else, so a mask bit on any other builtin
        // grants nothing and mutates nothing. `Map.remove` joined the list when
        // the runtime started honouring the mask for it; before that the mask
        // reached it and the runtime ignored it, and counting it would have put
        // writes that never happened in the same number as writes that did.
        if !matches!(
            builtin,
            VmBuiltin::MapSet | VmBuiltin::MapRemove | VmBuiltin::VectorSet
        ) {
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
        // The same direction, asked of the root sets the walk above cannot
        // see — the half P1 said a decision would owe them. It applies to the
        // static mask too: `CALL_BUILTIN_OWNED` empties the slot it is given,
        // and a whitelist that granted a map another arena entry, a global or a
        // chunk constant still holds would be as wrong there as here.
        //
        // The one shape that trips it and is not a bug is a constant EMPTY map,
        // which the chunk table holds and the fold's first insert takes: taking
        // an empty table in place is the identity, so what came out is what was
        // left behind. Hence the size test — a constant with entries in it being
        // consumed is the thing worth hearing about, and nothing does it today.
        debug_assert!(
            !granted
                || self
                    .arena
                    .map_slot(target)
                    .is_none_or(|slot| !slot.held_elsewhere || slot.entries == 0),
            "{} was granted ownership of slot {} statically, and something off \
             the operand stack — an arena entry, a global, a chunk constant — \
             still holds it",
            builtin.name(),
            target.arena_index(),
        );
    }

    /// What the runtime decided about the map writes the compiler declined.
    ///
    /// Maintained on every path, release included — see
    /// [`VmRuntimeOwnershipStats`].
    pub fn runtime_ownership_stats(&self) -> VmRuntimeOwnershipStats {
        self.runtime_ownership
    }

    /// What the runtime decided about the vector writes the compiler GRANTED.
    ///
    /// The same four buckets as [`VM::runtime_ownership_stats`], read in the
    /// opposite direction: `grants` is a static grant the fence confirmed, the
    /// two `refused_*` buckets are static grants it revoked (the write copied
    /// instead), and `unexamined_walk_too_costly` is a grant revoked without
    /// looking because the walk would have cost more than the copy. Maintained
    /// on every path, release included.
    pub fn vector_ownership_stats(&self) -> VmRuntimeOwnershipStats {
        self.vector_ownership
    }

    /// Whether the owned path may have `target`, at a `Map.set` / `Map.remove`
    /// the compiler declined and whose arguments have just been popped.
    ///
    /// Answering yes means the write empties `target`'s arena slot and builds
    /// its result out of what came out, instead of duplicating the table to
    /// leave the original readable. So the question is whether anything is left
    /// to read it, and it is asked of both holder sets in cost order:
    ///
    /// 1. `held_elsewhere` — one arena read. It answers for every holder the VM
    ///    cannot enumerate: an arena entry that has the map inside it, a global,
    ///    a chunk constant. It is recorded when the reference is made.
    /// 2. the walk — bounded by [`WALK_SLACK`] over the map's own length. It
    ///    answers for the VM's own handles, which are all operand-stack cells.
    ///
    /// A "no" from either is final and costs only the copy the program was
    /// making anyway. A "yes" from both is the grant, and under debug assertions
    /// it is re-derived from scratch by [`VM::nothing_else_holds_slot`], which
    /// searches all four root sets directly rather than trusting the record.
    #[inline]
    pub(super) fn runtime_owns_map_target(&mut self, target: NanValue) -> bool {
        // The switch comes first so that turning it off really does put the
        // program back where it was, arena read included.
        if !runtime_ownership_enabled() {
            return false;
        }
        // An empty map is an immediate: it owns no slot, there is nothing to
        // empty, and the copying path is already O(1) on it.
        let Some(slot) = self.arena.map_slot(target) else {
            return false;
        };
        if slot.held_elsewhere {
            self.runtime_ownership.refused_off_stack_holder += 1;
            return false;
        }
        if self.stack.len() > slot.entries + WALK_SLACK {
            self.runtime_ownership.unexamined_walk_too_costly += 1;
            return false;
        }
        if !self.slot_is_unheld(target) {
            self.runtime_ownership.refused_stack_holder += 1;
            return false;
        }
        self.runtime_ownership.grants += 1;
        debug_assert!(
            // `None` is the budget being spent, not an answer: the assert is
            // skipped explicitly and counted, rather than reading an audit
            // nobody performed as a clean bill.
            self.nothing_else_holds_slot(target.arena_index()) != Some(false),
            "Map builtin took its target in place on the runtime's word, but \
             slot {} is reachable from somewhere the decision did not look — \
             {} operand stack cell(s) hold it, and the search over globals, \
             chunk constants and arena entries found another holder",
            target.arena_index(),
            self.live_refs_to_slot(target.arena_index()),
        );
        true
    }

    /// Whether the owned path may KEEP `target`, at a `Vector.set` the
    /// compiler granted statically and whose arguments have just been popped.
    ///
    /// The map decision above runs in the trusting direction: a static map
    /// grant is a claim the compiler must get right, and the runtime only ever
    /// ADDS grants to the declines. The vector route runs the other way. The
    /// owned `Vector.set` (`vec_set_nv_owned`) empties the target's arena slot
    /// with `mem::take`, and the match-binder hole showed a static grant
    /// reaching a vector a container still held — the container read back an
    /// empty vector and nothing failed loudly. So a static vector grant is a
    /// PROPOSAL: it is confirmed against the same two holder sets the map
    /// decision asks, in the same cost order — `held_elsewhere` first (one
    /// arena read, answering for every holder the stack walk cannot see), then
    /// the walk, bounded by [`WALK_SLACK`] over the vector's own length — and
    /// revoked when either answers "held" or the walk would cost more than the
    /// copy it was avoiding. A revoked grant costs exactly the copy the program
    /// made before the grant existed; a wrong confirmation would be the silent
    /// corruption this exists to stop, which is why the confirmed case is
    /// re-derived from scratch under debug assertions, same as the map grant.
    ///
    /// Deliberately NOT behind the `AVER_NO_RUNTIME_MAP_OWNERSHIP` switch: that
    /// switch turns an optimization off (declines stay declines, everything
    /// copies — always safe). Turning THIS off would mean trusting the static
    /// grant again, which is the unsound direction, and a safety switch must
    /// not have an unsafe setting.
    ///
    /// ## The wrapper toll on non-fused chains — a decision, not an accident
    ///
    /// `Vector.set` returns `Option<Vector>`, and a `Some` over a heap value
    /// allocates an `ArenaEntry::Boxed` holding it — so the owned set's OWN
    /// return wrapper marks the fresh result `held_elsewhere` the moment it is
    /// built. The box usually dies at the very next `UNWRAP_OR`, but the flag
    /// is monotone and neither instrument can see that the box is dead, so
    /// from the SECOND write of a chain spelled through the plain builtin the
    /// fence revokes and the write copies. This is the conservative side of
    /// the bargain and it is measured: the fused self-keep spelling
    /// (`VECTOR_SET_OR_KEEP`) never comes through here and keeps its in-place
    /// grant — that is the accumulator shape every measured workload uses —
    /// and `a_second_owned_set_in_a_non_fused_chain_pays_the_wrapper_toll`
    /// pins the revocation so a change in either direction is a recorded
    /// decision. Buying the non-fused chain back needs a way to say "this
    /// box's payload has exactly one live reader", which is the same
    /// still-owed predicate the module doc records for the fused opcode.
    #[inline]
    pub(super) fn runtime_confirms_vector_grant(&mut self, target: NanValue) -> bool {
        // An immediate (the empty vector) owns no slot: nothing to empty,
        // nothing to corrupt, and the copying path is O(1) on it. Decline —
        // the copying `Vector.set` answers identically.
        let Some(slot) = self.arena.vector_slot(target) else {
            return false;
        };
        if slot.held_elsewhere {
            self.vector_ownership.refused_off_stack_holder += 1;
            return false;
        }
        if self.stack.len() > slot.len + WALK_SLACK {
            self.vector_ownership.unexamined_walk_too_costly += 1;
            return false;
        }
        if !self.slot_is_unheld(target) {
            self.vector_ownership.refused_stack_holder += 1;
            return false;
        }
        self.vector_ownership.grants += 1;
        debug_assert!(
            self.nothing_else_holds_slot(target.arena_index()) != Some(false),
            "Vector.set kept its static in-place grant on the runtime's word, \
             but slot {} is reachable from somewhere the decision did not look \
             — {} operand stack cell(s) hold it, and the search over globals, \
             chunk constants and arena entries found another holder",
            target.arena_index(),
            self.live_refs_to_slot(target.arena_index()),
        );
        true
    }

    /// Search the roots the VM has for a holder of `index`, from scratch.
    ///
    /// The independent recomputation behind the grant. It shares nothing with
    /// the decision but the stack walk's definition: the other three root sets
    /// are SEARCHED here and RECORDED there, so a marking site somebody forgot
    /// shows up as a failed assertion at the first write that would have been
    /// wrong, rather than as a map that quietly came back empty.
    ///
    /// `Some(true)` is "searched, nothing holds it", `Some(false)` is "searched,
    /// something does", and `None` is "the arena half was not searched" — see
    /// the budget below. The caller asserts on `Some(false)` alone, so an
    /// unaffordable audit skips the assertion rather than passing it.
    ///
    /// ## The roots it searches, and the ones nothing has ever put a map in
    ///
    /// Four: the operand stack, the globals table, the chunk constant tables,
    /// and the arena's entries. That is every root a map can be reached from
    /// today, and it is the same list [`ArenaEntry::Map::held_elsewhere`] names
    /// its marking sites for — but the VM does have two more places a heap value
    /// can sit, and neither is searched here or marked there:
    ///
    /// - `VmSymbolTable`'s own cells — `VmSymbolKind::Constant` and a
    ///   namespace's `members`. The compiler interns exactly two constants:
    ///   `Option.None`, which is an immediate, and `BranchPath.Root`, which is a
    ///   record whose one field is a string. A namespace's members go through
    ///   `Arena::push`'s namespace arm when the arena stores them. So the arm
    ///   holds no map today, and it says which values it does hold rather than
    ///   claiming they are all immediates;
    /// - the eight-byte literals `MATCH_DISPATCH` and `MATCH_DISPATCH_CONST`
    ///   carry inside the bytecode. The MIR emitter restricts them to Int, Bool,
    ///   Unit and Float bits and the HIR path adds strings, so no map reaches
    ///   one.
    ///
    /// Both are latent, both are named rather than covered, and the day either
    /// one starts carrying a map it owes a marking site and an arm here.
    ///
    /// ## What the budget costs the claim
    ///
    /// The arena half is `O(live heap)` per grant, which no run can afford
    /// forever, so it spends from a per-thread budget and stops looking once
    /// that is gone. A program small enough to be a test is checked end to end;
    /// a long one is checked until it has been checked enough — and after that
    /// the mirror covers three root sets rather than four, which is a smaller
    /// statement than the one before the budget ran out. Every skip is counted,
    /// [`grants_the_mirror_could_not_afford`] reads the tally back, and
    /// `no_run_in_the_corpus_outspends_the_mirrors_budget` in
    /// `tests/vm_map_runtime_ownership.rs` is what says the corpus is on the
    /// covered side of that line.
    #[cfg(debug_assertions)]
    fn nothing_else_holds_slot(&self, index: u32) -> Option<bool> {
        if self.live_refs_to_slot(index) > 0 {
            return Some(false);
        }
        if self.globals.iter().any(|v| v.heap_index() == Some(index)) {
            return Some(false);
        }
        if self
            .code
            .functions
            .iter()
            .flat_map(|chunk| chunk.constants.iter())
            .any(|v| v.heap_index() == Some(index))
        {
            return Some(false);
        }
        let entries = self.arena.len();
        let affordable = AUDIT_BUDGET.with(|budget| {
            let left = budget.get();
            budget.set(left.saturating_sub(entries as u64));
            left >= entries as u64
        });
        if !affordable {
            AUDIT_SKIPPED.with(|skipped| skipped.set(skipped.get() + 1));
            return None;
        }
        Some(!self.arena.any_entry_holds_slot(index))
    }

    #[cfg(not(debug_assertions))]
    fn nothing_else_holds_slot(&self, _index: u32) -> Option<bool> {
        None
    }
}

/// Grants this thread took whose arena half the mirror could not afford to
/// search.
///
/// Zero means every grant made on this thread was mirrored against all four
/// root sets. Anything else is the number of grants the assertion was skipped
/// for — not a number of failures, a number of unasked questions. Always zero
/// where assertions are compiled out, because the mirror does not run there at
/// all.
pub fn grants_the_mirror_could_not_afford() -> u64 {
    #[cfg(debug_assertions)]
    {
        AUDIT_SKIPPED.with(|skipped| skipped.get())
    }
    #[cfg(not(debug_assertions))]
    {
        0
    }
}

#[cfg(debug_assertions)]
thread_local! {
    /// Entry visits the from-scratch search may still spend on this thread.
    ///
    /// Cargo gives each test its own thread, so each test gets its own budget
    /// and the small ones — which is all of the aliasing corpus — are covered
    /// from the first write to the last.
    static AUDIT_BUDGET: std::cell::Cell<u64> = const { std::cell::Cell::new(50_000_000) };

    /// Grants whose arena half went unsearched because the budget was spent.
    ///
    /// The number that keeps the mirror's claim the size it really is. Reading
    /// it back is the difference between "every grant was mirrored" and "every
    /// grant until the budget ran out was".
    static AUDIT_SKIPPED: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}
