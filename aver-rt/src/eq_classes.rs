//! Equality classes: pairs of nodes `==` has found equal, so that it does
//! not compare them again.
//!
//! Two equal values built apart are different nodes, and `==` compares them
//! node by node each time it meets them. When the same pairs come back —
//! a term checked against its copy again and again — so does the work.
//! Here a comparison that walked a pair of nodes of a recursive sum, went at
//! least `MIN_STEPS` pairs deep (a parameter of
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
//! which a constructor's tag takes the low eight (`MAX_TAGS`). The upper 24
//! hold the node's class: the slot of the thread that gave it ([`SLOT_BITS`]
//! bits) and the class id plus one ([`ID_BITS`] bits); 0 is no class. Each
//! thread keeps, for each id it gave, its parent in the union-find (`par`)
//! and the address of the node it gave it to (`owner`). A sum some
//! `lazy f(…)` returns (`ThinSum::LAZY`; only such a sum has deferred
//! calls) keeps the state of its calls in those bits, also after a call is
//! made, and has no classes; neither has a constructor without fields,
//! which has no node. Every reader of the tag masks it to the constructor's
//! byte.
//!
//! # Invariants
//!
//! - **I1.** A thread takes a node's class bits as a class only when their
//!   slot is its own and its `owner` of their id is the node's address.
//! - **I2.** A thread gives each id to at most one node until it empties
//!   `par` and `owner` ([`CAP`] ids, then it starts over).
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
//!   loses the other's class. The tag word is atomic and the bits are
//!   written with a `Relaxed` load and store that keep its low eight bits,
//!   which never change in a sum without `lazy`; a node is freed only when
//!   no other owner is left to write it. At most [`SLOTS`] threads hold a
//!   slot at once; a thread past them has no classes until one ends.
//! - *A slot used again.* A thread that ends gives its slot back, and a new
//!   thread takes it with `par` and `owner` empty: for it the bits the old
//!   thread wrote are as bits from before a start over, which fail I1
//!   unless the new thread gave that id to that very node, and then it
//!   wrote the bits again.
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
pub const ID_BITS: u32 = 20;

/// The bits of the thread slot above them.
pub const SLOT_BITS: u32 = 4;

/// How many threads have classes: slot 0 is "no class".
pub const SLOTS: u32 = (1 << SLOT_BITS) - 1;

/// How many ids a thread gives before it starts over.
pub const CAP: usize = (1 << ID_BITS) - 2;

const ID_MASK: u32 = (1 << ID_BITS) - 1;

/// The slot past the last, for threads that have none.
const NO_SLOT: u32 = SLOTS + 1;

static NEXT_SLOT: AtomicU32 = AtomicU32::new(1);

/// Slots given back by threads that ended.
static FREE_SLOTS: Mutex<Vec<u32>> = Mutex::new(Vec::new());

struct Classes {
    /// This thread's slot: 0 until it first gives an id, [`NO_SLOT`] when
    /// all were taken.
    slot: u32,
    /// The union-find parent of each id.
    par: Vec<u32>,
    /// The address of the node each id was given to.
    owner: Vec<usize>,
}

thread_local! {
    static CLASSES: RefCell<Classes> = const {
        RefCell::new(Classes { slot: 0, par: Vec::new(), owner: Vec::new() })
    };
    /// Set while the check feature compares a pair in full.
    static CHECKING: Cell<bool> = const { Cell::new(false) };
}

impl Classes {
    /// The id the class bits `bits` of the node at `addr` stand for in this
    /// thread (I1).
    #[inline]
    fn id(&self, addr: usize, bits: u32) -> Option<u32> {
        if bits >> ID_BITS != self.slot || self.slot == 0 {
            return None;
        }
        let id = (bits & ID_MASK).wrapping_sub(1) as usize;
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
        node.set_class_bits((self.slot << ID_BITS) | (id + 1));
        id
    }

    /// Takes a slot for this thread if it has none: one given back, or the
    /// next never used. A thread that found none tries again later.
    fn slot(&mut self) -> bool {
        if self.slot == 0 || self.slot == NO_SLOT {
            let back = FREE_SLOTS.lock().ok().and_then(|mut free| free.pop());
            self.slot = match back {
                Some(slot) => slot,
                // A thread that loses the race to another taking a slot
                // tries the next one; only all slots taken is none.
                None => NEXT_SLOT
                    .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |next| {
                        (next <= SLOTS).then_some(next + 1)
                    })
                    .unwrap_or(NO_SLOT),
            };
        }
        self.slot != NO_SLOT
    }
}

impl Drop for Classes {
    /// The thread ends: its slot is given back (see *A slot used again*).
    fn drop(&mut self) {
        if (1..=SLOTS).contains(&self.slot)
            && let Ok(mut free) = FREE_SLOTS.lock()
        {
            free.push(self.slot);
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
        if classes.owner.len() + 2 > CAP {
            classes.owner.clear();
            classes.par.clear();
        }
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

    #[test]
    fn joined_nodes_are_one_class_transitively() {
        let (a, b, c, d) = (fake(0x1000), fake(0x2000), fake(0x3000), fake(0x4000));
        assert!(!same_nodes(&a, &b));
        join(&a, &b);
        join(&b, &c);
        assert!(same_nodes(&a, &c));
        assert!(!same_nodes(&a, &d));
    }

    #[test]
    fn an_address_used_again_has_no_class() {
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
        // and the old bits stand for nothing in any of them.
        for _ in 0..(2 * SLOTS) {
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
    fn ids_of_another_thread_are_refused() {
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
