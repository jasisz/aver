//! `==` on values of a recursive sum at the cost of the graph they share.
//!
//! A recursive sum keeps each constructor's fields in one node, so a
//! subterm can sit under many nodes. Two equal values built apart are
//! different objects all the way down, and a comparison node by node meets a
//! shared subterm once for every path to it: the work is the size of the
//! unfolded tree, exponential in the depth of the sharing. Here a comparison
//! runs as it always did for at most [`BUDGET`] constructor pairs; one that
//! needs more goes on from where it is, remembering from then on the pairs of
//! shared nodes it finds equal, and does not compare them again.
//!
//! A comparison that goes on past its budget may also go deep: a value a
//! million constructors deep is compared a million calls deep. Once such a
//! comparison is [`DEEP`] bytes of stack below where it began to remember,
//! it leaves each pair it meets for later, and the `==` that began it
//! compares the pairs left, each from its own stack again, once all the rest
//! is equal. `==` is true when every pair is equal, so the order the pairs
//! are compared in does not change what it answers.

use crate::Shared;
use std::cell::Cell;

/// How many constructor pairs a comparison meets before it starts to
/// remember. A comparison within it pays a counter.
pub const BUDGET: u32 = 1024;

/// How many bytes of stack a comparison with memory goes down before it
/// leaves the pairs it meets for later.
pub const DEEP: usize = 256 * 1024;

/// What a comparison keeps once its budget is spent. It lives on the stack
/// of the `==` that began the comparison, so the values compared keep every
/// remembered node alive, and it is gone when that `==` returns or unwinds.
#[derive(Default)]
struct Memory<'v> {
    /// The address on the stack the comparison measures how deep it is
    /// from, 0 until its budget is spent.
    top: Cell<usize>,
    /// Empty until a pair is remembered or left for later.
    spent: Cell<Option<Box<Spent<'v>>>>,
}

#[derive(Default)]
struct Spent<'v> {
    /// The pairs of shared nodes found equal.
    equal: Pairs,
    /// The pairs left for later.
    later: Vec<Later<'v>>,
}

/// Addresses of pairs of nodes, the left one first.
type Pairs = ahash::AHashSet<(usize, usize)>;

/// A pair left for later: the `==` of two values `'v` borrows.
type Later<'v> = Box<dyn for<'w> FnOnce(Walk<'w, 'v>) -> bool + 'v>;

/// How a comparison goes on: within its budget while `left` is above zero,
/// with memory once it is spent. `'v` is how long the values compared are
/// borrowed for, which every pair left for later lives as long as.
#[derive(Clone, Copy)]
pub struct Walk<'w, 'v> {
    left: &'w Cell<u32>,
    memory: &'w Memory<'v>,
}

/// `==` that carries the [`Walk`] it is part of: what the Rust backend
/// generates for a recursive sum, for the payload of each of its
/// constructors, and for a record holding one.
pub trait WalkEq {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: Walk<'_, 'v>) -> bool;
}

impl<T: WalkEq> WalkEq for Option<T> {
    #[inline]
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: Walk<'_, 'v>) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.walk_eq(b, walk),
            (None, None) => true,
            _ => false,
        }
    }
}

/// A top-level `==`: the `PartialEq` of a recursive sum.
#[inline]
pub fn walk_eq<T: WalkEq>(a: &T, b: &T) -> bool {
    let left = Cell::new(BUDGET);
    // Nothing is made before the budget is spent.
    let memory = Memory::default();
    let walk = Walk {
        left: &left,
        memory: &memory,
    };
    a.walk_eq(b, walk) && walk.rest()
}

/// A top-level `==` of `a` and `b` that `eq` makes within a walk of its own.
pub(crate) fn walk_with<'v, T>(
    a: &'v T,
    b: &'v T,
    eq: impl for<'w> FnOnce(&'v T, &'v T, Walk<'w, 'v>) -> bool,
) -> bool {
    let left = Cell::new(BUDGET);
    let memory = Memory::default();
    let walk = Walk {
        left: &left,
        memory: &memory,
    };
    eq(a, b, walk) && walk.rest()
}

/// A node of a recursive sum, as a comparison sees it: its address, which
/// identifies it while it lives, and how many owners it has.
pub trait NodeId {
    fn addr(&self) -> usize;
    fn count(&self) -> usize;
}

/// The payload of a shared constructor of a sum that is not thin, compared
/// by [`Walk::node`] as one node.
impl<T: WalkEq> WalkEq for Shared<T> {
    #[inline]
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: Walk<'_, 'v>) -> bool {
        (**self).walk_eq(&**other, walk)
    }
}

impl<T> NodeId for Shared<T> {
    #[inline]
    fn addr(&self) -> usize {
        Shared::as_ptr(self) as usize
    }

    #[inline]
    fn count(&self) -> usize {
        Shared::strong_count(self)
    }
}

/// Where the stack of the caller is: its stack pointer, read without
/// making room on the stack, so a call in tail position stays one.
#[cfg(all(target_arch = "aarch64", not(miri)))]
#[inline(always)]
fn stack_here() -> usize {
    let sp: usize;
    // SAFETY: reads a register.
    unsafe { std::arch::asm!("mov {}, sp", out(reg) sp, options(nomem, nostack, preserves_flags)) };
    sp
}

#[cfg(all(target_arch = "x86_64", not(miri)))]
#[inline(always)]
fn stack_here() -> usize {
    let sp: usize;
    // SAFETY: reads a register.
    unsafe {
        std::arch::asm!("mov {}, rsp", out(reg) sp, options(nomem, nostack, preserves_flags))
    };
    sp
}

/// Where the stack of the caller is: the address of a local of its own.
#[cfg(any(miri, not(any(target_arch = "aarch64", target_arch = "x86_64"))))]
#[inline(never)]
fn stack_here() -> usize {
    let marker = 0u8;
    std::hint::black_box(&marker) as *const u8 as usize
}

impl<'v> Walk<'_, 'v> {
    /// Two nodes of one constructor of a recursive sum, `fields_eq` comparing
    /// their fields. `reflexive` says their `==` is (they hold no `Float`),
    /// so one node is equal to itself without being read.
    #[inline]
    pub fn node<N: NodeId + WalkEq>(
        self,
        a: &'v N,
        b: &'v N,
        reflexive: bool,
        fields_eq: impl FnOnce(Self) -> bool,
    ) -> bool {
        if reflexive && a.addr() == b.addr() {
            return true;
        }
        let left = self.left.get();
        if left == 0 {
            // The pairs under way go on where they are; only the ones met
            // from now on are remembered.
            return self.remembering(a, b, fields_eq);
        }
        self.left.set(left - 1);
        fields_eq(self)
    }

    /// [`Walk::node`] with memory: a pair met [`DEEP`] bytes below where
    /// the memory began is left for later; a pair of nodes both held in
    /// several places is looked up, and remembered once found equal. A node
    /// held in one place is reached by one path only.
    #[cold]
    #[inline(never)]
    fn remembering<N: NodeId + WalkEq>(
        self,
        a: &'v N,
        b: &'v N,
        fields_eq: impl FnOnce(Self) -> bool,
    ) -> bool {
        if self.deep() {
            return self.leave(a, b);
        }
        if a.count() == 1 || b.count() == 1 {
            return fields_eq(self);
        }
        let pair = (a.addr(), b.addr());
        let spent = self.memory.spent.take();
        let known = spent
            .as_ref()
            .is_some_and(|spent| spent.equal.contains(&pair));
        self.memory.spent.set(spent);
        if known {
            return true;
        }
        let same = fields_eq(self);
        if same {
            let mut spent = self.memory.spent.take().unwrap_or_default();
            spent.equal.insert(pair);
            self.memory.spent.set(Some(spent));
        }
        same
    }

    /// Whether the walk is [`DEEP`] bytes below where its memory began,
    /// which the first pair met with memory marks.
    #[inline]
    pub(crate) fn deep(self) -> bool {
        let (here, top) = (stack_here(), self.memory.top.get());
        if top == 0 {
            self.memory.top.set(here);
            return false;
        }
        top.saturating_sub(here) > DEEP
    }

    /// Leaves the pair for later (see [`Walk::rest`]): equal for now.
    #[cold]
    #[inline(never)]
    fn leave<N: WalkEq>(self, a: &'v N, b: &'v N) -> bool {
        self.defer(move |walk| a.walk_eq(b, walk))
    }

    /// Leaves `later` for later (see [`Walk::rest`]): equal for now.
    #[cold]
    #[inline(never)]
    pub(crate) fn defer(self, later: impl for<'w> FnOnce(Walk<'w, 'v>) -> bool + 'v) -> bool {
        let mut spent = self.memory.spent.take().unwrap_or_default();
        spent.later.push(Box::new(later));
        self.memory.spent.set(Some(spent));
        true
    }

    /// Takes one step of the budget, or answers that it is spent: the pairs
    /// met from now on are remembered.
    #[inline]
    pub(crate) fn step(self) -> bool {
        let left = self.left.get();
        self.left.set(left.saturating_sub(1));
        left > 0
    }

    /// Two values stored at `a` and `b` met once the budget is spent and
    /// below a check of [`Walk::deep`], `fields_eq` comparing them: looked
    /// up, and remembered once found equal.
    #[cold]
    #[inline(never)]
    pub(crate) fn remember(self, a: usize, b: usize, fields_eq: impl FnOnce(Self) -> bool) -> bool {
        let pair = (a, b);
        let spent = self.memory.spent.take();
        let known = spent
            .as_ref()
            .is_some_and(|spent| spent.equal.contains(&pair));
        self.memory.spent.set(spent);
        if known {
            return true;
        }
        let same = fields_eq(self);
        if same {
            let mut spent = self.memory.spent.take().unwrap_or_default();
            spent.equal.insert(pair);
            self.memory.spent.set(Some(spent));
        }
        same
    }

    /// The pairs left for later, once the rest of the comparison is equal.
    #[inline]
    fn rest(self) -> bool {
        self.memory.top.get() == 0 || self.later()
    }

    #[cold]
    #[inline(never)]
    fn later(self) -> bool {
        // Each pair is compared from here, and goes as deep again.
        self.memory.top.set(stack_here());
        loop {
            let mut spent = self.memory.spent.take().unwrap_or_default();
            let next = spent.later.pop();
            self.memory.spent.set(Some(spent));
            match next {
                None => return true,
                Some(pair) => {
                    if !pair(self) {
                        return false;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BUDGET, Memory, NodeId, Walk, WalkEq, walk_eq};
    use crate::Shared;
    use std::cell::Cell;

    thread_local! {
        static VISITS: Cell<usize> = const { Cell::new(0) };
    }

    /// The shape the Rust backend gives a recursive sum: `Pair` counts the
    /// payloads it compares.
    #[derive(Clone, Debug)]
    enum Node {
        Leaf(Shared<Leaf>),
        Pair(Shared<Pair>),
    }

    #[derive(Debug, PartialEq)]
    struct Leaf(f64);

    #[derive(Debug)]
    struct Pair(Node, Node);

    impl WalkEq for Leaf {
        fn walk_eq<'v>(&'v self, other: &'v Self, _: Walk<'_, 'v>) -> bool {
            self == other
        }
    }

    impl WalkEq for Pair {
        fn walk_eq<'v>(&'v self, other: &'v Self, walk: Walk<'_, 'v>) -> bool {
            VISITS.with(|v| v.set(v.get() + 1));
            self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
        }
    }

    impl NodeId for Node {
        fn addr(&self) -> usize {
            match self {
                Node::Leaf(a) => a.addr(),
                Node::Pair(a) => a.addr(),
            }
        }

        fn count(&self) -> usize {
            match self {
                Node::Leaf(a) => a.count(),
                Node::Pair(a) => a.count(),
            }
        }
    }

    impl WalkEq for Node {
        fn walk_eq<'v>(&'v self, other: &'v Self, walk: Walk<'_, 'v>) -> bool {
            match (self, other) {
                (Node::Leaf(a), Node::Leaf(b)) => {
                    walk.node(self, other, false, |walk| (**a).walk_eq(&**b, walk))
                }
                (Node::Pair(a), Node::Pair(b)) => {
                    walk.node(self, other, false, |walk| (**a).walk_eq(&**b, walk))
                }
                _ => false,
            }
        }
    }

    impl PartialEq for Node {
        fn eq(&self, other: &Self) -> bool {
            walk_eq(self, other)
        }
    }

    fn leaf(n: f64) -> Node {
        Node::Leaf(Shared::new(Leaf(n)))
    }

    fn pair(a: Node, b: Node) -> Node {
        Node::Pair(Shared::new(Pair(a, b)))
    }

    /// `n` levels above `leaf(seed)`, each holding the one below twice:
    /// n + 1 nodes, 2^n leaves.
    fn twice(n: usize, seed: f64) -> Node {
        (0..n).fold(leaf(seed), |below, _| pair(below.clone(), below))
    }

    /// The tree of `twice(n, seed)` with its rightmost leaf `last`, built
    /// with a node held once at every level of its right spine.
    fn spine(n: usize, seed: f64, last: f64) -> Node {
        (0..n).fold(leaf(last), |below, i| pair(twice(i, seed), below))
    }

    fn compare(a: &Node, b: &Node) -> (bool, usize) {
        VISITS.with(|v| v.set(0));
        let same = a == b;
        (same, VISITS.with(Cell::get))
    }

    /// What a comparison of 60 levels may pay: the walk within [`BUDGET`],
    /// then a few pairs per level.
    const SPENT: usize = BUDGET as usize + 4 * 60;

    #[test]
    fn equal_values_built_apart_cost_the_graph_not_the_tree() {
        let (same, visits) = compare(&twice(60, 1.0), &twice(60, 1.0));
        assert!(same);
        assert!(visits <= SPENT, "{visits} pairs compared");
        let (same, visits) = compare(&twice(60, 1.0), &spine(60, 1.0, 1.0));
        assert!(same);
        assert!(visits <= SPENT + 60 * 60, "{visits} pairs compared");
    }

    /// A full tree `n` levels deep, no node of it held twice.
    fn full(n: usize, seed: f64) -> Node {
        match n {
            0 => leaf(seed),
            _ => pair(full(n - 1, seed), full(n - 1, seed)),
        }
    }

    /// The budget runs out once the first half, a tree with no sharing, has
    /// been compared: the walk goes on into the second half, and does not
    /// compare the first again. The first half is 511 pairs and the second
    /// about 1830; starting over would compare the first half twice.
    #[test]
    fn a_walk_past_its_budget_goes_on_where_it_is() {
        let value = |last: f64| pair(full(9, 1.0), spine(60, 1.0, last));
        let (same, visits) = compare(&value(1.0), &value(1.0));
        assert!(same);
        assert!(visits <= 512 + 1900, "{visits} pairs compared");
        let (same, visits) = compare(&value(1.0), &value(2.0));
        assert!(!same);
        assert!(visits <= 512 + 1900, "{visits} pairs compared");
        assert!(pair(full(9, 1.0), twice(60, 1.0)) != pair(full(9, 2.0), twice(60, 1.0)));
    }

    #[test]
    fn a_difference_deep_in_the_graph_is_still_found() {
        let (same, visits) = compare(&twice(60, 1.0), &spine(60, 1.0, 2.0));
        assert!(!same);
        assert!(visits <= SPENT + 60 * 60, "{visits} pairs compared");
        assert!(twice(12, 1.0) != twice(12, 2.0));
        assert!(twice(12, 1.0) != twice(13, 1.0));
    }

    /// `n` pairs deep, each holding the one below on its left and a leaf
    /// on its right, the deepest leaf `last`.
    fn chain(n: usize, last: f64) -> Node {
        (0..n).fold(leaf(last), |below, i| pair(below, leaf(i as f64)))
    }

    /// Lets a chain go one pair at a time: dropping it whole would take a
    /// call per pair.
    fn let_go(mut node: Node) {
        while let Node::Pair(p) = node {
            match Shared::try_unwrap(p) {
                Ok(Pair(below, _)) => node = below,
                Err(_) => return,
            }
        }
    }

    /// A chain far deeper than a test thread's stack takes one call per
    /// pair for is compared: past [`super::DEEP`] bytes of stack the pairs
    /// are left for later, and a difference at the very bottom is found.
    #[test]
    fn a_deep_value_is_compared_on_a_stack_of_fixed_size() {
        let n = if cfg!(miri) { 3_000 } else { 200_000 };
        let (a, b, c) = (chain(n, 1.0), chain(n, 1.0), chain(n, 2.0));
        let (same, visits) = compare(&a, &b);
        assert!(same);
        assert_eq!(visits, n);
        assert!(a != c && c != b);
        assert!(pair(a.clone(), leaf(0.0)) == pair(b.clone(), leaf(0.0)));
        assert!(pair(a.clone(), leaf(0.0)) != pair(b.clone(), leaf(1.0)));
        for value in [a, b, c] {
            let_go(value);
        }
    }

    #[test]
    fn one_object_is_equal_to_itself_only_when_its_payload_is() {
        VISITS.with(|v| v.set(0));
        let one = pair(leaf(1.0), leaf(1.0));
        let again = one.clone();
        let left = Cell::new(BUDGET);
        let memory = Memory::default();
        let walk = Walk {
            left: &left,
            memory: &memory,
        };
        let Node::Pair(payload) = &one else {
            unreachable!()
        };
        assert!(walk.node(&one, &again, true, |walk| {
            (**payload).walk_eq(payload, walk)
        }));
        assert_eq!(VISITS.with(Cell::get), 0);
        let nan = leaf(f64::NAN);
        assert!(nan != nan.clone());
    }
}
