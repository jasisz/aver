//! The table of the canonical nodes of an interned recursive sum.
//!
//! `[rust] intern` in `aver.toml` names recursive sums whose constructors
//! give back the node that already holds the same fields, when one is alive,
//! instead of building another (see `Thin::intern`). Such a node is
//! canonical: no other live canonical node of its type holds fields equal to
//! its own, so two canonical values are equal exactly when they are the same
//! node, and `==` compares two pointers. A node whose fields cannot be
//! compared that way (one holds a call `lazy` deferred and nobody has made
//! yet) is built as before and is not canonical; `==` with it walks, as it
//! always did.
//!
//! The table holds no owner of the nodes in it. A node leaves the table when
//! its last owner lets it go, before its memory is given back, and a lookup
//! takes a new owner only of a node whose count is not 0 yet (see
//! [`retain`]), so it never brings back one that is being freed: it builds
//! a new node beside it instead, and the table holds both until the old one
//! leaves.
//!
//! One table serves every thread. It is split into [`SHARDS`] parts by the
//! hash of the fields, each behind its own lock, held only while one lookup
//! probes it, one node is put in or one is taken out, never while anything is
//! freed, so two locks are never held at once.

use std::cell::UnsafeCell;
use std::hint;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};

use crate::shared_hash::NodeHasher;

/// How many parts the table is split into, each with its own lock.
pub const SHARDS: usize = 64;

/// An entry slot nobody has used.
const EMPTY: *mut u8 = std::ptr::null_mut();
/// An entry slot whose node has left.
const GONE: *mut u8 = std::ptr::without_provenance_mut(1);

/// The fields of a constructor of an interned sum, as the table compares
/// them. The Rust backend implements it for each constructor's field struct.
pub trait InternFields {
    /// Whether every field of an interned type has an identity, given to it
    /// now if nothing asked before (see `Thin::identity`): not when one holds
    /// a call `lazy` deferred that nobody has made yet.
    fn settle(&self) -> bool;
    /// Feeds the fields to the hasher: a field of an interned type as the
    /// hash of its node ([`Canon::canon_hash`]), any other field by value.
    fn hash_into(&self, hasher: &mut NodeHasher);
    /// Whether the fields of two nodes are equal, once both are settled: a
    /// field of an interned type by identity, any other one by `==`.
    fn same(&self, other: &Self) -> bool;
}

/// A field of an interned type, or an `Option` of one, as
/// [`InternFields`] sees it. `thin_sum!` implements it for an interned sum.
pub trait Canon {
    /// See [`InternFields::settle`].
    fn settle(&self) -> bool;
    /// The hash of the value, made from what it holds, the same for equal
    /// values however they were built.
    fn canon_hash(&self) -> u64;
    /// Whether two settled values have the same identity.
    fn same(&self, other: &Self) -> bool;
}

impl<T: Canon> Canon for Option<T> {
    #[inline]
    fn settle(&self) -> bool {
        self.as_ref().is_none_or(Canon::settle)
    }

    #[inline]
    fn canon_hash(&self) -> u64 {
        self.as_ref()
            .map_or(0, |value| value.canon_hash().rotate_left(1))
    }

    #[inline]
    fn same(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.same(b),
            (None, None) => true,
            _ => false,
        }
    }
}

/// How far below the outermost one a node may be put in the table, in bytes
/// of stack: past it the node is left without an identity, and `==` on it
/// walks.
const DEEP: usize = 256 * 1024;

thread_local! {
    /// Where on the stack the outermost [`Deeper`] of this thread began.
    static TOP: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// One more node being put in the table, inside the ones above it.
pub(crate) struct Deeper {
    outermost: bool,
}

impl Deeper {
    /// `None` once [`DEEP`] bytes of stack below the outermost one.
    #[inline]
    pub(crate) fn enter() -> Option<Self> {
        let here = crate::shared_eq::stack_here();
        TOP.with(|top| match top.get() {
            0 => {
                top.set(here);
                Some(Deeper { outermost: true })
            }
            start if start.saturating_sub(here) > DEEP => None,
            _ => Some(Deeper { outermost: false }),
        })
    }
}

impl Drop for Deeper {
    #[inline]
    fn drop(&mut self) {
        if self.outermost {
            TOP.with(|top| top.set(0));
        }
    }
}

/// Takes one more owner of a node found in the table: only while it has
/// one, so a node whose last owner has let it go stays gone. A count past
/// `max` aborts, as a clone does.
#[inline]
pub(crate) fn retain(count: &AtomicU32, max: u32) -> bool {
    let mut now = count.load(Ordering::Relaxed);
    loop {
        if now == 0 {
            return false;
        }
        if now > max {
            std::process::abort();
        }
        match count.compare_exchange_weak(now, now + 1, Ordering::Acquire, Ordering::Relaxed) {
            Ok(_) => return true,
            Err(seen) => now = seen,
        }
    }
}

/// Where a probe for `hash` starts: past its lowest bit, which is 1 in every
/// hash of a canonical node.
#[inline]
fn slot(hash: u64) -> usize {
    (hash >> 1) as usize
}

/// One canonical node: its hash, and where it is (or [`EMPTY`], [`GONE`]).
#[derive(Clone, Copy)]
struct Entry {
    hash: u64,
    node: *mut u8,
}

/// One part of the table: open addressing, probed one slot after another.
pub(crate) struct Slots {
    entries: Vec<Entry>,
    /// Entries holding a node.
    live: usize,
    /// Entries a node has left, which a probe goes past.
    gone: usize,
}

impl Slots {
    const fn new() -> Self {
        Slots {
            entries: Vec::new(),
            live: 0,
            gone: 0,
        }
    }

    #[inline]
    fn mask(&self) -> usize {
        self.entries.len().wrapping_sub(1)
    }

    /// The first node of hash `hash` that `take` accepts.
    #[inline]
    pub(crate) fn find(&self, hash: u64, mut take: impl FnMut(*mut u8) -> bool) -> Option<*mut u8> {
        if self.live == 0 {
            return None;
        }
        let mask = self.mask();
        let mut at = slot(hash) & mask;
        loop {
            let entry = self.entries[at];
            if entry.node == EMPTY {
                return None;
            }
            if entry.hash == hash && entry.node != GONE && take(entry.node) {
                return Some(entry.node);
            }
            at = (at + 1) & mask;
        }
    }

    pub(crate) fn insert(&mut self, hash: u64, node: *mut u8) {
        if (self.live + self.gone + 1) * 2 > self.entries.len() {
            self.resize();
        }
        let mask = self.mask();
        let mut at = slot(hash) & mask;
        loop {
            let held = self.entries[at].node;
            if held == EMPTY {
                break;
            }
            if held == GONE {
                self.gone -= 1;
                break;
            }
            at = (at + 1) & mask;
        }
        self.entries[at] = Entry { hash, node };
        self.live += 1;
    }

    fn remove(&mut self, hash: u64, node: *mut u8) {
        let mask = self.mask();
        let mut at = slot(hash) & mask;
        loop {
            let entry = self.entries[at];
            assert_ne!(entry.node, EMPTY, "a canonical node is in the table");
            if entry.node == node {
                break;
            }
            at = (at + 1) & mask;
        }
        self.entries[at].node = GONE;
        self.live -= 1;
        self.gone += 1;
        if self.live == 0 {
            // Nothing left to find: no probe needs to go past the gone ones.
            self.entries.iter_mut().for_each(|entry| entry.node = EMPTY);
            self.gone = 0;
        }
    }

    /// Room for twice the live entries, at least 16, without the gone ones.
    #[cold]
    #[inline(never)]
    fn resize(&mut self) {
        let size = ((self.live + 1) * 4).next_power_of_two().max(16);
        let old = std::mem::replace(
            &mut self.entries,
            vec![
                Entry {
                    hash: 0,
                    node: EMPTY,
                };
                size
            ],
        );
        let mask = self.mask();
        for entry in old
            .into_iter()
            .filter(|entry| entry.node != EMPTY && entry.node != GONE)
        {
            let mut at = slot(entry.hash) & mask;
            while self.entries[at].node != EMPTY {
                at = (at + 1) & mask;
            }
            self.entries[at] = entry;
        }
        self.gone = 0;
    }
}

/// One part of the table behind its lock: a flag spun on, since it is held
/// for a probe only.
struct Shard {
    locked: AtomicBool,
    slots: UnsafeCell<Slots>,
}

/// Holds the lock of one part.
pub(crate) struct Locked<'a> {
    shard: &'a Shard,
}

impl Shard {
    const fn new() -> Self {
        Shard {
            locked: AtomicBool::new(false),
            slots: UnsafeCell::new(Slots::new()),
        }
    }

    #[inline]
    fn lock(&self) -> Locked<'_> {
        if self
            .locked
            .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            self.lock_slow();
        }
        Locked { shard: self }
    }

    #[cold]
    #[inline(never)]
    fn lock_slow(&self) {
        let mut spins = 0u32;
        loop {
            while self.locked.load(Ordering::Relaxed) {
                if spins < 64 {
                    hint::spin_loop();
                    spins += 1;
                } else {
                    std::thread::yield_now();
                }
            }
            if self
                .locked
                .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
            {
                return;
            }
        }
    }
}

impl std::ops::Deref for Locked<'_> {
    type Target = Slots;

    #[inline]
    fn deref(&self) -> &Slots {
        // SAFETY: the lock is held.
        unsafe { &*self.shard.slots.get() }
    }
}

impl std::ops::DerefMut for Locked<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Slots {
        // SAFETY: the lock is held, by this guard only.
        unsafe { &mut *self.shard.slots.get() }
    }
}

impl Drop for Locked<'_> {
    #[inline]
    fn drop(&mut self) {
        self.shard.locked.store(false, Ordering::Release);
    }
}

/// The canonical nodes of one interned sum (see the module docs).
pub struct Table {
    shards: [Shard; SHARDS],
}

// SAFETY: every part is read and written only under its lock, and what it
// holds are addresses of nodes, whose own type says where they may go.
unsafe impl Sync for Table {}
unsafe impl Send for Table {}

impl Default for Table {
    fn default() -> Self {
        Self::new()
    }
}

impl Table {
    pub const fn new() -> Self {
        Table {
            shards: [const { Shard::new() }; SHARDS],
        }
    }

    #[inline]
    fn shard(&self, hash: u64) -> &Shard {
        &self.shards[(hash >> 58) as usize % SHARDS]
    }

    /// The part nodes of hash `hash` are in, locked. Nothing is dropped
    /// while it is held: a drop may free a node, which locks its part.
    #[inline]
    pub(crate) fn lock(&self, hash: u64) -> Locked<'_> {
        self.shard(hash).lock()
    }

    /// Takes out the node at `node`, of hash `hash`, whose last owner let it
    /// go.
    #[inline]
    pub(crate) fn remove(&self, hash: u64, node: *mut u8) {
        self.shard(hash).lock().remove(hash, node);
    }

    /// How many nodes the table holds: those alive, and those whose last
    /// owner is letting them go right now.
    pub fn len(&self) -> usize {
        self.shards.iter().map(|shard| shard.lock().live).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::{EMPTY, Slots};

    fn at(n: usize) -> *mut u8 {
        std::ptr::without_provenance_mut(n * 8)
    }

    #[test]
    fn slots_find_insert_and_remove_by_address() {
        let mut slots = Slots::new();
        for node in 2..1000usize {
            slots.insert((node % 7) as u64, at(node));
        }
        assert_eq!(slots.live, 998);
        assert_eq!(slots.find(3, |node| node == at(81)), None);
        assert_eq!(slots.find(3, |node| node == at(3)), Some(at(3)));
        for node in 2..1000usize {
            slots.remove((node % 7) as u64, at(node));
        }
        assert_eq!(slots.live, 0);
        assert!(slots.entries.iter().all(|entry| entry.node == EMPTY));
        assert_eq!(slots.find(3, |_| true), None);
    }

    #[test]
    fn two_entries_of_one_hash_are_told_apart_by_address() {
        let mut slots = Slots::new();
        slots.insert(5, at(100));
        slots.insert(5, at(200));
        slots.remove(5, at(100));
        assert_eq!(slots.find(5, |_| true), Some(at(200)));
        slots.insert(5, at(300));
        slots.remove(5, at(200));
        assert_eq!(slots.find(5, |_| true), Some(at(300)));
    }
}

/// The whole mechanism, on sums laid out as the Rust backend lays out an
/// interned one. Each test has its own sum, so each its own table.
#[cfg(test)]
#[allow(non_camel_case_types)]
mod interned_sums {
    use super::{Canon, InternFields};
    use crate::shared_hash::NodeHasher;
    use crate::thin::{HandOver, Pending};
    use std::hash::Hasher;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// Declares an interned sum `$name` with a leaf of a `u64` and a pair,
    /// and `leaf`, `pair` and `nil` to build them.
    macro_rules! interned_sum {
        ($module:ident, $table:ident $( , lazy $lazy:literal )?) => {
            #[allow(dead_code)]
            mod $module {
                use super::*;

                #[derive(Clone)]
                pub struct E__Leaf(pub u64);
                #[derive(Clone)]
                pub struct E__Pair(pub E, pub E, pub u8);

                impl HandOver for E__Leaf {}

                impl HandOver for E__Pair {
                    fn hand_over(self, pending: Pending<'_>) {
                        self.0.hand_over(pending);
                        self.1.hand_over(pending);
                    }
                }

                impl InternFields for E__Leaf {
                    fn settle(&self) -> bool {
                        true
                    }
                    fn hash_into(&self, hasher: &mut NodeHasher) {
                        hasher.write_u64(self.0);
                    }
                    fn same(&self, other: &Self) -> bool {
                        self.0 == other.0
                    }
                }

                impl InternFields for E__Pair {
                    fn settle(&self) -> bool {
                        Canon::settle(&self.0) && Canon::settle(&self.1)
                    }
                    fn hash_into(&self, hasher: &mut NodeHasher) {
                        hasher.write_u64(Canon::canon_hash(&self.0));
                        hasher.write_u64(Canon::canon_hash(&self.1));
                        hasher.write_u8(self.2);
                    }
                    fn same(&self, other: &Self) -> bool {
                        self.2 == other.2 && Canon::same(&self.0, &other.0) && Canon::same(&self.1, &other.1)
                    }
                }

                crate::thin_sum! {
                    pub E / E__View / E__Pairs {
                        0 Leaf(E__Leaf),
                        1 Nil,
                        2 Pair(E__Pair),
                    } $( lazy $lazy )? interned $table
                }

                /// `==` as the Rust backend gives it: by identity when both
                /// are canonical, else by walking.
                pub fn equal(a: &E, b: &E) -> bool {
                    if let Some(same) = E::canon_eq(a, b) {
                        return same;
                    }
                    match E::view_pair(a, b) {
                        None => false,
                        Some(E__Pairs::Leaf(x, y)) => x.0 == y.0,
                        Some(E__Pairs::Nil) => true,
                        Some(E__Pairs::Pair(x, y)) => {
                            x.2 == y.2 && equal(&x.0, &y.0) && equal(&x.1, &y.1)
                        }
                    }
                }

                pub fn leaf(n: u64) -> E {
                    E::Leaf(E__Leaf(n))
                }

                pub fn pair(a: E, b: E, n: u8) -> E {
                    E::Pair(E__Pair(a, b, n))
                }

                pub fn nil() -> E {
                    E::Nil
                }
            }
        };
    }

    /// A full tree `depth` levels deep over leaves numbered from `first`,
    /// built node by node, no node shared.
    macro_rules! full {
        ($m:ident) => {
            fn full(depth: u32, first: u64) -> $m::E {
                match depth {
                    0 => $m::leaf(first),
                    _ => $m::pair(
                        full(depth - 1, first),
                        full(depth - 1, first + (1 << (depth - 1))),
                        depth as u8,
                    ),
                }
            }
        };
    }

    interned_sum!(one, ONE);

    #[test]
    fn equal_values_get_one_identity_when_asked_and_the_table_empties_when_they_die() {
        use one::*;
        full!(one);
        let a = full(10, 0);
        let b = full(10, 0);
        // Nothing is in the table until something asks for an identity.
        assert!(ONE.is_empty());
        assert!(!E::ptr_eq(&a, &b));
        assert_eq!(E::known_eq(&a, &b), None);
        assert_eq!(E::canon_eq(&a, &b), Some(true));
        // 2^11 - 1 nodes of `a`; those of `b` stand for them.
        assert_eq!(ONE.len(), 2047);
        assert_eq!(E::known_eq(&a, &b), Some(true));
        let c = full(10, 1);
        assert_eq!(E::canon_eq(&a, &c), Some(false));
        assert!(!equal(&a, &c));
        assert_eq!(E::canon_hash(&a), E::canon_hash(&b));
        assert_ne!(E::canon_hash(&a), E::canon_hash(&c));
        assert_eq!(E::canon_eq(&nil(), &E::Nil), Some(true));
        // `a` dies first: `b` still stands for its nodes, which stay.
        drop(a);
        assert_eq!(E::canon_eq(&b, &full(10, 0)), Some(true));
        drop((b, c));
        assert!(ONE.is_empty());
        let again = pair(leaf(1), nil(), 0);
        assert_eq!(E::canon_eq(&again, &pair(leaf(1), nil(), 0)), Some(true));
        drop(again);
        assert!(ONE.is_empty());
    }

    interned_sum!(deep, DEEP);

    #[test]
    fn a_deep_chain_is_compared_by_walking_and_frees_without_deep_recursion() {
        use deep::*;
        let chain = |n: u64| (0..n).fold(nil(), |chain, i| pair(leaf(i), chain, 0));
        let (a, b) = (chain(1_000_000), chain(1_000_000));
        // Too deep to put in the table at once: no identity, `==` walks.
        assert_eq!(E::canon_eq(&a, &b), None);
        let (c, d) = (chain(200), chain(200));
        assert_eq!(E::canon_eq(&c, &d), Some(true));
        // The leaves met on the way down to where it stopped are in too.
        assert!(DEEP.len() >= 400);
        drop((a, b, c, d));
        assert!(DEEP.is_empty());
    }

    interned_sum!(many, MANY);

    /// Eight threads build the same terms, ask for their identities, let
    /// them go and build them again, so a node's last owner lets it go while
    /// another thread looks it up: equal values get one identity, and the
    /// table empties at the end.
    #[test]
    fn threads_building_and_dropping_equal_terms_agree_on_one_identity() {
        use many::*;
        full!(many);
        let rounds = if cfg!(miri) { 4 } else { 400 };
        let shared = Arc::new(full(6, 0));
        let threads: Vec<_> = (0..8)
            .map(|t| {
                let shared = shared.clone();
                std::thread::spawn(move || {
                    for round in 0..rounds {
                        let mine = full(6, 0);
                        assert_eq!(E::canon_eq(&mine, &shared), Some(true));
                        // Small terms built and dropped at once: their count
                        // goes 1 to 0 while others look them up.
                        for i in 0..32u64 {
                            let small = pair(leaf(i), leaf((round + t) % 4), 1);
                            let other = pair(leaf(i), leaf((round + t) % 4), 1);
                            assert_eq!(E::canon_eq(&small, &other), Some(true));
                            assert_eq!(E::canon_hash(&small), E::canon_hash(&other));
                        }
                    }
                })
            })
            .collect();
        for thread in threads {
            thread.join().unwrap();
        }
        drop(shared);
        assert!(MANY.is_empty());
    }

    interned_sum!(lazy, LAZY, lazy 3);

    #[test]
    fn a_deferred_call_once_made_has_the_identity_of_what_it_returned() {
        use lazy::*;
        static MADE: AtomicUsize = AtomicUsize::new(0);
        let built = pair(leaf(7), nil(), 2);
        let deferred = E::deferred((crate::thin::Kept(7u64), ()), |(n, ())| {
            MADE.fetch_add(1, Ordering::SeqCst);
            pair(leaf(n.0), nil(), 2)
        });
        // Not made: no identity, and asking makes no call.
        assert_eq!(E::canon_eq(&deferred, &built), None);
        let over = pair(deferred.clone(), leaf(1), 0);
        assert_eq!(E::canon_eq(&over, &pair(built.clone(), leaf(1), 0)), None);
        assert_eq!(MADE.load(Ordering::SeqCst), 0);
        // A look makes the call; from then on it has the identity of what
        // it returned, and so does a node over it.
        assert!(equal(&deferred, &built));
        assert_eq!(MADE.load(Ordering::SeqCst), 1);
        assert_eq!(E::canon_eq(&deferred, &built), Some(true));
        let after = pair(built.clone(), leaf(1), 0);
        assert_eq!(E::canon_eq(&over, &after), Some(true));
        assert_eq!(E::canon_hash(&over), E::canon_hash(&after));
        // A call returning a constructor without fields stands for it.
        let empty = E::deferred((), |()| nil());
        assert!(equal(&empty, &nil()));
        assert_eq!(E::canon_eq(&empty, &nil()), Some(true));
        drop((built, deferred, over, after, empty));
        assert!(LAZY.is_empty());
    }
}
