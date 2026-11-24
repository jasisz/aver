//! Equality classes: pairs of nodes `==` has found equal, so that it does
//! not compare them again.
//!
//! Two equal values built apart are different nodes, and `==` compares them
//! node by node each time it meets them. When the same pairs come back —
//! a term checked against its copy again and again — so does the work.
//! Here a comparison that walked a pair of nodes of a recursive sum, went at
//! least `MIN_STEPS` steps of its budget deep, pairs of nodes and of
//! collections counted (a parameter of
//! [`Walk::node_classed`](crate::Walk::node_classed), which the code
//! generator sets from `[rust] equality_classes`) and found them equal joins
//! the two nodes into one class, in a union-find of this thread; a later
//! comparison of
//! two nodes of one class answers at once. Nothing is assumed: a class only
//! ever holds nodes found equal, and structural equality is transitive, so
//! any two nodes of a class are equal (for a sum holding `Float`s too: a
//! pair found equal holds no NaN where it was compared).
//!
//! # Where a class is kept
//!
//! The tag word of a node of a [`thin`](crate::thin) sum is 32 bits, of
//! which a constructor's tag takes the low eight (`MAX_TAGS`), and `==`
//! reads both for a pair at once (`view_pair_classed`). The upper 24
//! hold the node's class: the slot of the thread that gave it ([`SLOT_BITS`]
//! bits) and the class id plus one ([`ID_BITS`] bits); 0 is no class. Each
//! thread keeps, for each id it gave, its parent in the union-find (`par`)
//! and the address of the node it gave it to (`owner`). A sum some
//! `lazy f(…)` returns (`ThinSum::LAZY`; only such a sum has deferred
//! calls) keeps the state of its calls in those bits, also after a call is
//! made, and has no classes; neither has a constructor without fields,
//! which has no node. Every reader of the tag of a sum with classes masks
//! the word to the constructor's byte, which no class is written into.
//!
//! # Invariants
//!
//! - **I1.** A thread takes a node's class bits as a class only when their
//!   slot is one of its own and its `owner` of their id is the node's
//!   address.
//! - **I2.** A thread gives each id to at most one node until it empties
//!   `par` and `owner` ([`CAP`] ids at most, then it starts over).
//! - **I3.** Class bits are written only by a thread giving one of its ids;
//!   a node is made, and a freed node's memory made into a new one, with
//!   bits 0 (`Thin::new`, `Thin::new_in` write the whole tag word).
//! - **I4.** Two classes are joined only when a comparison found a node of
//!   each equal.
//!
//! # Why an answer from a class is right
//!
//! - *One thread, live node.* By I1 and I2 the id read from node `x` was
//!   given to `x` itself, which lives at the address `owner` holds; by I4
//!   every node of its class was found equal to some node of it, so equal
//!   to `x`.
//! - *An address used again.* Node `y` with id `j` is freed and node `x` is
//!   made at the same address: by I3 `x` has bits 0, or later bits of an id
//!   this thread gave it, which by I2 is not `j`. No live node shows `j`:
//!   its only node is gone.
//! - *Starting over.* Emptying `owner` makes every id given before fail I1
//!   until it is given again; an id given again to node `v` holds for `v`
//!   only, and any other live node `x` showing it lives elsewhere. A class
//!   lost is a comparison done in full, never a wrong answer.
//! - *Several threads.* A thread never takes an id another thread gave, the
//!   slots differ, so ids of different threads cannot be confused even when
//!   their numbers meet; a thread writing a node's bits over another's only
//!   loses the other's class. The tag word is read and written whole, with
//!   `Relaxed` atomics, and a write keeps the tag's byte, which never
//!   changes in a sum without `lazy`: a read sees the bits of one write,
//!   and of two writes at once one is kept, never parts of both. Parts of
//!   two writes (a slot and high id byte of one, the low id byte of
//!   another, as written in 0.37.1) could name an id this thread once gave
//!   to a freed node at the same address, which I1 would take; whole bits
//!   of this thread were written by it for this very node (I3). A node is
//!   freed only when no other owner is left to write it. At
//!   most [`SLOTS`] threads hold a slot at once; a thread past them has no
//!   classes until one ends.
//! - *A slot used again.* A thread gives its slots back when it ends, and
//!   its further slots when it starts over while another thread waits for
//!   a first one (it forgets their ids then). A thread that takes such a
//!   slot, as its first or as a further one, has given no id in it yet:
//!   its ids there are past every id it gave. For it the bits the old
//!   thread wrote are as bits from before a start over, which fail I1
//!   unless it gave that id to that very node, and then it wrote the bits
//!   again; the lock on the free slots orders every write of the old
//!   thread before its own.
//!
//! What a class needs of `==` is only that a `true` is transitive, which the
//! generated structural `==` is: a `Float` is compared with IEEE `==`,
//! whose `true` is transitive (`-0.0 == 0.0`, and a NaN is never equal), so
//! a pair holding `Float`s found equal holds no NaN, and `x == x` is `true`
//! for any node of a class. Only the generated `==` of a recursive sum
//! reaches [`Walk::node`](crate::Walk::node), the one place classes are made
//! and read; a sum whose `==` may be `false` on one node (it holds a
//! `Float`) has no `ptr_eq` shortcut in generated code, and its node is
//! never in a class with a NaN inside.
//!
//! With the `equality-classes-check` feature every answer from a class is
//! compared in full as well, and a wrong one panics.

use std::cell::{Cell, RefCell};
use std::sync::Mutex;
use std::sync::atomic::{AtomicU32, Ordering};

/// The bits of the class id (plus one) in a node's class bits.
pub const ID_BITS: u32 = 16;

/// The bits of the thread slot above them: 255 threads have classes at
/// once. With four bits (15 threads, 2^20 ids) a check of mathlib on 32
/// threads spent 10% more instructions, as many threads had no classes.
/// One slot gives a thread 2^16 ids only, so a thread that runs out of them
/// takes a further free slot before it starts over (see `Classes::room`):
/// with one slot a thread started over so often on a long check that it
/// spent 13% more.
pub const SLOT_BITS: u32 = 8;

// A thread's place among its slots is kept in a byte.
const _: () = assert!(SLOTS <= u8::MAX as u32);

/// How many threads have classes: slot 0 is "no class".
pub const SLOTS: u32 = (1 << SLOT_BITS) - 1;

/// How many ids one slot gives: an id is written plus one, 0 is no class.
const PER_SLOT: usize = (1 << ID_BITS) - 1;

/// How many further slots all threads hold at once, a budget of memory:
/// each slot a thread fills costs 12 bytes per id, 0.75 MB, so the budget
/// is 48 MB, whether one thread holds it all or many share it. A thread
/// with a first slot takes a further one while the budget lasts; a thread
/// that found no first slot gets one from the further slots of others
/// (see [`WANTED`]). Measured on a Lean kernel checker: one long check on
/// one thread spent 16% fewer instructions with 64 slots than with 16, and
/// with no budget mathlib on 4 threads held 0.23 GB more for nothing.
pub const FURTHER_SLOTS: u32 = 64;

/// How many ids a thread gives at most before it starts over.
pub const CAP: usize = PER_SLOT * (1 + FURTHER_SLOTS as usize);

/// How many further slots all threads hold now (see [`FURTHER_SLOTS`]).
static FURTHER: AtomicU32 = AtomicU32::new(0);

const ID_MASK: u32 = (1 << ID_BITS) - 1;

/// The slot past the last, for threads that have none.
const NO_SLOT: u32 = SLOTS + 1;

static NEXT_SLOT: AtomicU32 = AtomicU32::new(1);

/// How many threads asked for a first slot and found none. While it is
/// above 0 no thread takes a further slot, and a thread that runs out of
/// ids gives its further slots back before it starts over, so the threads
/// waiting get one; each thread holds at least its first slot.
static WANTED: AtomicU32 = AtomicU32::new(0);

/// Slots given back by threads that ended.
static FREE_SLOTS: Mutex<Vec<u32>> = Mutex::new(Vec::new());

/// How many slots [`FREE_SLOTS`] holds, kept beside it so that a thread
/// with no slot looks at the lock only when one may be free. Without it,
/// every join of every thread past [`SLOTS`] took the lock, and on many
/// threads they spent most of their time waiting for it.
static FREE_COUNT: AtomicU32 = AtomicU32::new(0);

struct Classes {
    /// This thread's first slot: 0 until it first gives an id, [`NO_SLOT`] when
    /// all were taken.
    slot: u32,
    /// Whether it asked for a first slot, found none, and is counted in
    /// [`WANTED`].
    wanting: bool,
    /// The further slots it took, in order: id `k * PER_SLOT + i` is
    /// written in slot `more[k - 1]` for `k` above 0.
    more: Vec<u32>,
    /// For each slot, 1 plus its place among this thread's slots (0 for
    /// the first, `k` for `more[k - 1]`), 0 when it is not this thread's.
    rank: [u8; 1 << SLOT_BITS],
    /// The union-find parent of each id.
    par: Vec<u32>,
    /// The address of the node each id was given to.
    owner: Vec<usize>,
}

thread_local! {
    static CLASSES: RefCell<Classes> = const {
        RefCell::new(Classes {
            slot: 0,
            wanting: false,
            more: Vec::new(),
            rank: [0; 1 << SLOT_BITS],
            par: Vec::new(),
            owner: Vec::new(),
        })
    };
    /// Set while the check feature compares a pair in full.
    static CHECKING: Cell<bool> = const { Cell::new(false) };
}

impl Classes {
    /// The id the class bits `bits` of the node at `addr` stand for in this
    /// thread (I1).
    #[inline]
    fn id(&self, addr: usize, bits: u32) -> Option<u32> {
        let rank = self.rank[(bits >> ID_BITS) as usize & ((1 << SLOT_BITS) - 1)] as usize;
        let low = (bits & ID_MASK) as usize;
        if rank == 0 || low == 0 {
            return None;
        }
        let id = (rank - 1) * PER_SLOT + low - 1;
        (self.owner.get(id) == Some(&addr)).then_some(id as u32)
    }

    #[inline]
    fn find(&mut self, mut x: u32) -> u32 {
        while self.par[x as usize] != x {
            let up = self.par[self.par[x as usize] as usize];
            self.par[x as usize] = up;
            x = up;
        }
        x
    }

    /// The id of `node`, given now if it has none of this thread's.
    fn id_of<N: crate::NodeId>(&mut self, node: &N) -> u32 {
        let addr = node.addr();
        if let Some(id) = self.id(addr, node.class_bits()) {
            return id;
        }
        let id = self.owner.len() as u32;
        self.owner.push(addr);
        self.par.push(id);
        let (k, low) = (id as usize / PER_SLOT, id as usize % PER_SLOT);
        let slot = if k == 0 { self.slot } else { self.more[k - 1] };
        node.set_class_bits((slot << ID_BITS) | (low as u32 + 1));
        id
    }

    /// Takes a slot for this thread if it has none: one given back, or the
    /// next never used. A thread that found none is counted in [`WANTED`]
    /// and tries again later.
    fn slot(&mut self) -> bool {
        if self.slot == 0 || self.slot == NO_SLOT {
            self.slot = take_slot().unwrap_or(NO_SLOT);
            let found = self.slot != NO_SLOT;
            if found {
                self.rank[self.slot as usize] = 1;
            }
            if found == self.wanting {
                self.wanting = !found;
                match found {
                    true => WANTED.fetch_sub(1, Ordering::Relaxed),
                    false => WANTED.fetch_add(1, Ordering::Relaxed),
                };
            }
        }
        self.slot != NO_SLOT
    }

    /// Room for two more ids. Called on the cold path of a join, it checks
    /// only when this thread's ids are all given: then, while no thread
    /// waits for a first slot, it takes a further one (given back, or never
    /// used) while all threads hold fewer than [`FURTHER_SLOTS`]; else it starts over, which forgets
    /// every class of this thread, and gives its further slots back when a
    /// thread waits.
    fn room(&mut self) {
        if self.owner.len() + 2 <= (1 + self.more.len()) * PER_SLOT {
            return;
        }
        let waiting = WANTED.load(Ordering::Relaxed) > 0;
        if !waiting
            && FURTHER
                .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |held| {
                    (held < FURTHER_SLOTS).then_some(held + 1)
                })
                .is_ok()
        {
            if let Some(slot) = take_slot() {
                self.more.push(slot);
                self.rank[slot as usize] = (1 + self.more.len()) as u8;
                return;
            }
            FURTHER.fetch_sub(1, Ordering::Relaxed);
        }
        self.owner.clear();
        self.par.clear();
        if waiting && !self.more.is_empty() {
            for &slot in &self.more {
                self.rank[slot as usize] = 0;
            }
            give_back(&self.more);
            FURTHER.fetch_sub(self.more.len() as u32, Ordering::Relaxed);
            self.more.clear();
        }
    }
}

/// A free slot: one given back, else the next never used.
fn take_slot() -> Option<u32> {
    let back = if FREE_COUNT.load(Ordering::Relaxed) == 0 {
        None
    } else {
        FREE_SLOTS.lock().ok().and_then(|mut free| {
            let slot = free.pop();
            FREE_COUNT.store(free.len() as u32, Ordering::Relaxed);
            slot
        })
    };
    // A thread that loses the race to another taking a slot tries the next
    // one; only all slots taken is none.
    back.or_else(|| {
        NEXT_SLOT
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |next| {
                (next <= SLOTS).then_some(next + 1)
            })
            .ok()
    })
}

/// Gives slots back for other threads to take (see *A slot used again*):
/// the lock orders every write of their bits by this thread before any
/// write by the thread that takes them.
fn give_back(slots: &[u32]) {
    if let Ok(mut free) = FREE_SLOTS.lock() {
        free.extend_from_slice(slots);
        FREE_COUNT.store(free.len() as u32, Ordering::Relaxed);
    }
}

impl Drop for Classes {
    /// The thread ends: its slots are given back (see *A slot used again*).
    fn drop(&mut self) {
        if self.wanting {
            WANTED.fetch_sub(1, Ordering::Relaxed);
        }
        FURTHER.fetch_sub(self.more.len() as u32, Ordering::Relaxed);
        if (1..=SLOTS).contains(&self.slot) {
            let mut slots = self.more.clone();
            slots.push(self.slot);
            give_back(&slots);
        }
    }
}

/// Whether the nodes at `a` and `b`, with class bits `bits_a` and `bits_b`
/// (both nonzero), are of one class of this thread.
#[inline]
pub(crate) fn same(a: usize, bits_a: u32, b: usize, bits_b: u32) -> bool {
    if CHECKING.get() {
        return false;
    }
    CLASSES
        .try_with(|classes| {
            let Ok(mut classes) = classes.try_borrow_mut() else {
                return false;
            };
            match (classes.id(a, bits_a), classes.id(b, bits_b)) {
                (Some(x), Some(y)) => classes.find(x) == classes.find(y),
                _ => false,
            }
        })
        .unwrap_or(false)
}

/// Joins the classes of `a` and `b`, found equal (I4).
#[cold]
#[inline(never)]
pub(crate) fn join<N: crate::NodeId>(a: &N, b: &N) {
    if CHECKING.get() {
        return;
    }
    let _ = CLASSES.try_with(|classes| {
        let Ok(mut classes) = classes.try_borrow_mut() else {
            return;
        };
        if !classes.slot() {
            return;
        }
        classes.room();
        let x = classes.id_of(a);
        let y = classes.id_of(b);
        let (x, y) = (classes.find(x), classes.find(y));
        if x != y {
            classes.par[x as usize] = y;
        }
    });
}

/// For the check feature: runs `full` with classes out of the way.
#[cfg(feature = "equality-classes-check")]
pub(crate) fn checking<T>(full: impl FnOnce() -> T) -> T {
    let was = CHECKING.replace(true);
    let result = full();
    CHECKING.set(was);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::NodeId;
    use std::cell::Cell as StdCell;

    /// A node with an address and class bits of its own.
    struct Fake {
        addr: usize,
        bits: StdCell<u32>,
    }

    impl NodeId for Fake {
        fn addr(&self) -> usize {
            self.addr
        }
        fn count(&self) -> usize {
            2
        }
        fn class_bits(&self) -> u32 {
            self.bits.get()
        }
        fn set_class_bits(&self, bits: u32) {
            self.bits.set(bits)
        }
    }

    fn fake(addr: usize) -> Fake {
        Fake {
            addr,
            bits: StdCell::new(0),
        }
    }

    fn same_nodes(a: &Fake, b: &Fake) -> bool {
        a.bits.get() != 0 && b.bits.get() != 0 && same(a.addr, a.bits.get(), b.addr, b.bits.get())
    }

    /// The tests take slots of the whole process and one of them takes
    /// all; they run one at a time.
    fn serial() -> std::sync::MutexGuard<'static, ()> {
        static SERIAL: Mutex<()> = Mutex::new(());
        SERIAL.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// Joins `pairs` pairs of new nodes from `base` on: two ids each.
    fn fill(base: usize, pairs: usize) {
        for k in 0..pairs {
            let (x, y) = (fake(base + k * 0x40), fake(base + k * 0x40 + 0x20));
            join(&x, &y);
        }
    }

    /// This thread's further slots and how many ids it gave.
    fn held() -> (Vec<u32>, usize) {
        CLASSES.with(|c| {
            let c = c.borrow();
            (c.more.clone(), c.owner.len())
        })
    }

    #[test]
    fn a_thread_past_one_slot_of_ids_takes_a_further_one() {
        let _serial = serial();
        std::thread::spawn(|| {
            let (a, b) = (fake(0x100_0000), fake(0x100_1000));
            join(&a, &b);
            fill(0x200_0000, PER_SLOT / 2 + 8);
            let (more, ids) = held();
            assert_eq!(more.len(), 1);
            assert!(ids > PER_SLOT);
            // The first pair kept its class: no start over.
            assert!(same_nodes(&a, &b));
        })
        .join()
        .unwrap();
    }

    #[test]
    fn many_threads_share_the_slots_out() {
        // More threads than half the slots, each with a first slot, then
        // each past one slot of ids at once: the slots left go to further
        // ones as far as the budget goes, and no thread loses its first.
        let _serial = serial();
        if cfg!(miri) {
            return;
        }
        let threads = 160;
        let start = std::sync::Barrier::new(threads);
        let started = std::sync::Barrier::new(threads);
        let done = std::sync::Barrier::new(threads);
        let (firsts, furthers) = (AtomicU32::new(0), AtomicU32::new(0));
        std::thread::scope(|scope| {
            for t in 0..threads {
                let (start, started, done) = (&start, &started, &done);
                let (firsts, furthers) = (&firsts, &furthers);
                scope.spawn(move || {
                    let base = 0x1000_0000 + t * 0x400_0000;
                    let (a, b) = (fake(base), fake(base + 0x20));
                    start.wait();
                    join(&a, &b);
                    if same_nodes(&a, &b) {
                        firsts.fetch_add(1, Ordering::Relaxed);
                    }
                    started.wait();
                    fill(base + 0x40, PER_SLOT / 2 + 8);
                    furthers.fetch_add(held().0.len() as u32, Ordering::Relaxed);
                    // Every thread holds its slots until all are counted.
                    done.wait();
                });
            }
        });
        assert_eq!(firsts.load(Ordering::Relaxed), threads as u32);
        let left = SLOTS - threads as u32;
        assert_eq!(furthers.load(Ordering::Relaxed), left.min(FURTHER_SLOTS));
    }

    #[test]
    fn a_thread_starting_over_keeps_its_first_slot() {
        let _serial = serial();
        WANTED.fetch_add(1, Ordering::Relaxed);
        let result = std::thread::spawn(|| {
            let (a, b) = (fake(0x100_0000), fake(0x100_1000));
            join(&a, &b);
            let first = CLASSES.with(|c| c.borrow().slot);
            // A thread waits: no further slot, a start over instead.
            fill(0x200_0000, PER_SLOT / 2 + 8);
            let (more, ids) = held();
            assert!(more.is_empty());
            assert!(ids < PER_SLOT);
            assert!(!same_nodes(&a, &b));
            assert_eq!(CLASSES.with(|c| c.borrow().slot), first);
            // Its classes work as before.
            let (x, y) = (fake(0x300_0000), fake(0x300_1000));
            join(&x, &y);
            assert!(same_nodes(&x, &y));
        })
        .join();
        WANTED.fetch_sub(1, Ordering::Relaxed);
        result.unwrap();
    }

    #[test]
    fn a_thread_waiting_for_a_slot_gets_one_given_back() {
        use std::sync::mpsc::channel;
        let _serial = serial();
        // A thread holding a further slot, waiting for word to go on.
        let (go, wait) = channel::<()>();
        let (grew, grown) = channel::<()>();
        let holder = std::thread::spawn(move || {
            fill(0x200_0000, PER_SLOT / 2 + 8);
            assert_eq!(held().0.len(), 1);
            grew.send(()).unwrap();
            wait.recv().unwrap();
            // It runs out of ids again while a thread waits.
            fill(0x400_0000, PER_SLOT);
            let (more, _) = held();
            assert!(more.is_empty(), "further slots given back");
            grew.send(()).unwrap();
            wait.recv().unwrap();
        });
        grown.recv().unwrap();
        // Every other slot is taken.
        let next = NEXT_SLOT.swap(SLOTS + 1, Ordering::Relaxed);
        let free: Vec<u32> = {
            let mut free = FREE_SLOTS.lock().unwrap();
            FREE_COUNT.store(0, Ordering::Relaxed);
            std::mem::take(&mut *free)
        };
        let (ask, asked) = channel::<()>();
        let (answer, answered) = channel::<bool>();
        let latecomer = std::thread::spawn(move || {
            while asked.recv().is_ok() {
                let (x, y) = (fake(0x600_0000), fake(0x600_1000));
                join(&x, &y);
                answer.send(same_nodes(&x, &y)).unwrap();
            }
        });
        ask.send(()).unwrap();
        assert!(!answered.recv().unwrap(), "no slot left for it");
        assert_eq!(WANTED.load(Ordering::Relaxed), 1);
        go.send(()).unwrap();
        grown.recv().unwrap();
        ask.send(()).unwrap();
        assert!(answered.recv().unwrap(), "it took the slot given back");
        assert_eq!(WANTED.load(Ordering::Relaxed), 0);
        drop(ask);
        latecomer.join().unwrap();
        go.send(()).unwrap();
        holder.join().unwrap();
        let mut back = FREE_SLOTS.lock().unwrap();
        back.extend(free);
        FREE_COUNT.store(back.len() as u32, Ordering::Relaxed);
        NEXT_SLOT.store(next, Ordering::Relaxed);
    }

    #[test]
    fn joined_nodes_are_one_class_transitively() {
        let _serial = serial();
        let (a, b, c, d) = (fake(0x1000), fake(0x2000), fake(0x3000), fake(0x4000));
        assert!(!same_nodes(&a, &b));
        join(&a, &b);
        join(&b, &c);
        assert!(same_nodes(&a, &c));
        assert!(!same_nodes(&a, &d));
    }

    #[test]
    fn an_address_used_again_has_no_class() {
        let _serial = serial();
        let (y, z) = (fake(0x5000), fake(0x6000));
        join(&y, &z);
        // `y` is freed and `x` made at its address: a new node, bits 0.
        let x = fake(0x5000);
        assert!(!same_nodes(&x, &z));
        // Even if `x` showed `y`'s old bits copied from elsewhere, it is
        // `y`'s id at `y`'s address: the same node by I1 and I2; a node at
        // another address showing them is refused.
        let w = Fake {
            addr: 0x7000,
            bits: StdCell::new(y.bits.get()),
        };
        assert!(!same_nodes(&w, &z));
    }

    #[test]
    fn starting_over_forgets_classes() {
        let _serial = serial();
        let (a, b) = (fake(0x8000), fake(0x9000));
        join(&a, &b);
        assert!(same_nodes(&a, &b));
        CLASSES.with(|c| {
            let mut c = c.borrow_mut();
            c.owner.clear();
            c.par.clear();
        });
        assert!(!same_nodes(&a, &b));
        // An id given again to another node does not stand for `a`.
        let (v, u) = (fake(0xa000), fake(0xb000));
        join(&v, &u);
        assert!(!same_nodes(&a, &u));
        assert!(same_nodes(&v, &u));
    }

    #[test]
    fn a_slot_given_back_does_not_take_the_old_threads_ids() {
        let _serial = serial();
        // The first thread joins two nodes and ends, giving its slot back.
        let (bits_a, bits_b) = std::thread::spawn(|| {
            let (a, b) = (fake(0xe000), fake(0xf000));
            join(&a, &b);
            assert!(same_nodes(&a, &b));
            (a.bits.get(), b.bits.get())
        })
        .join()
        .unwrap();
        // Threads that come later take slots again, the old one among them,
        // and the old bits stand for nothing in any of them. Each ends
        // before the next begins, so a few take the slots given back.
        for _ in 0..32 {
            let refused = std::thread::spawn(move || {
                let (x, y) = (fake(0x1_0000), fake(0x1_1000));
                join(&x, &y);
                let classes = same_nodes(&x, &y);
                (classes, same(0xe000, bits_a, 0xf000, bits_b))
            })
            .join()
            .unwrap();
            assert_eq!(refused, (true, false));
        }
    }

    #[test]
    fn threads_taking_slots_at_once_each_get_one() {
        let _serial = serial();
        // Threads that ask for their first slot together race on the next
        // slot never used; a thread that loses takes the one after.
        let threads = 4;
        let start = std::sync::Barrier::new(threads);
        std::thread::scope(|scope| {
            for t in 0..threads {
                let start = &start;
                scope.spawn(move || {
                    let (x, y) = (fake(0x2_0000 + t * 0x2000), fake(0x2_1000 + t * 0x2000));
                    start.wait();
                    join(&x, &y);
                    assert!(same_nodes(&x, &y));
                });
            }
        });
    }

    #[test]
    fn threads_past_the_slots_still_join_and_a_freed_slot_is_taken() {
        let _serial = serial();
        // More threads than slots at once: those without a slot join many
        // times (each join once took the lock), and none of them breaks.
        // Under Miri each thread joins a few times only.
        let threads = SLOTS as usize + 16;
        let joins = if cfg!(miri) { 8 } else { 1000 };
        let start = std::sync::Barrier::new(threads);
        let with_classes = AtomicU32::new(0);
        std::thread::scope(|scope| {
            for t in 0..threads {
                let (start, with_classes) = (&start, &with_classes);
                scope.spawn(move || {
                    start.wait();
                    for k in 0..joins {
                        let base = 0x10_0000 + (t * joins + k) * 0x20;
                        let (x, y) = (fake(base), fake(base + 0x10));
                        join(&x, &y);
                        if k == 0 && same_nodes(&x, &y) {
                            with_classes.fetch_add(1, Ordering::Relaxed);
                        }
                    }
                });
            }
        });
        assert!(with_classes.load(Ordering::Relaxed) >= 1);
        // Their slots came back: a later thread has classes again.
        let later = std::thread::spawn(|| {
            let (x, y) = (fake(0x9_0000), fake(0x9_1000));
            join(&x, &y);
            same_nodes(&x, &y)
        })
        .join()
        .unwrap();
        assert!(later);
    }

    #[test]
    fn ids_of_another_thread_are_refused() {
        let _serial = serial();
        let (a, b) = (fake(0xc000), fake(0xd000));
        join(&a, &b);
        let (bits_a, bits_b) = (a.bits.get(), b.bits.get());
        let seen = std::thread::spawn(move || {
            // The other thread gives its own ids to nodes at the same
            // addresses, with the same numbers.
            let (x, y) = (fake(0xc000), fake(0xd000));
            join(&x, &y);
            assert_ne!(x.bits.get() >> ID_BITS, bits_a >> ID_BITS);
            // Bits written by the first thread are not its own.
            same(0xc000, bits_a, 0xd000, bits_b)
        })
        .join()
        .unwrap();
        assert!(!seen);
        assert!(same_nodes(&a, &b));
    }
}
