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

use crate::Shared;
use std::cell::Cell;

/// How many constructor pairs a comparison meets before it starts to
/// remember. A comparison within it pays a counter.
pub const BUDGET: u32 = 1024;

/// The pairs of shared nodes found equal by a comparison with memory. It
/// lives on the stack of the `==` that began the comparison, so the values
/// compared keep every remembered node alive, and it is gone when that `==`
/// returns or unwinds.
#[derive(Default)]
struct Memory {
    equal: Cell<Option<Box<Pairs>>>,
}

/// Addresses of pairs of nodes, the left one first.
type Pairs = ahash::AHashSet<(usize, usize)>;

/// How a comparison goes on: within its budget while `left` is above zero,
/// with memory once it is spent.
#[derive(Clone, Copy)]
pub struct Walk<'a> {
    left: &'a Cell<u32>,
    memory: &'a Memory,
}

/// `==` that carries the [`Walk`] it is part of: what the Rust backend
/// generates for a recursive sum, for the payload of each of its
/// constructors, and for a record holding one.
pub trait WalkEq {
    fn walk_eq(&self, other: &Self, walk: Walk<'_>) -> bool;
}

impl<T: WalkEq> WalkEq for Option<T> {
    #[inline]
    fn walk_eq(&self, other: &Self, walk: Walk<'_>) -> bool {
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
    // Empty until the budget is spent: no table is made before then.
    let memory = Memory::default();
    a.walk_eq(
        b,
        Walk {
            left: &left,
            memory: &memory,
        },
    )
}

/// A node of a recursive sum, as a comparison sees it: its address, which
/// identifies it while it lives, and how many owners it has.
pub trait NodeId {
    fn addr(&self) -> usize;
    fn count(&self) -> usize;
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

impl Walk<'_> {
    /// The payloads of one constructor of a recursive sum. `reflexive` says
    /// their `==` is (they hold no `Float`), so one object is equal to itself
    /// without being read.
    #[inline]
    pub fn shared<T: WalkEq>(self, a: &Shared<T>, b: &Shared<T>, reflexive: bool) -> bool {
        self.node(a, b, reflexive, |walk| (**a).walk_eq(&**b, walk))
    }

    /// Two nodes of one constructor of a recursive sum, `fields_eq` comparing
    /// their fields. `reflexive` says their `==` is (they hold no `Float`),
    /// so one node is equal to itself without being read.
    #[inline]
    pub fn node<N: NodeId>(
        self,
        a: &N,
        b: &N,
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
            return self.remembering(self.memory, a, b, fields_eq);
        }
        self.left.set(left - 1);
        fields_eq(self)
    }

    /// [`Walk::node`] with memory: a pair of nodes both held in several
    /// places is looked up, and remembered once found equal. A node held in
    /// one place is reached by one path only.
    #[cold]
    #[inline(never)]
    fn remembering<N: NodeId>(
        self,
        memory: &Memory,
        a: &N,
        b: &N,
        fields_eq: impl FnOnce(Self) -> bool,
    ) -> bool {
        if a.count() == 1 || b.count() == 1 {
            return fields_eq(self);
        }
        let pair = (a.addr(), b.addr());
        let seen = memory.equal.take();
        let known = seen.as_ref().is_some_and(|seen| seen.contains(&pair));
        memory.equal.set(seen);
        if known {
            return true;
        }
        let same = fields_eq(self);
        if same {
            let mut seen = memory.equal.take().unwrap_or_default();
            seen.insert(pair);
            memory.equal.set(Some(seen));
        }
        same
    }
}

#[cfg(test)]
mod tests {
    use super::{BUDGET, Memory, Walk, WalkEq, walk_eq};
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
        fn walk_eq(&self, other: &Self, _: Walk<'_>) -> bool {
            self == other
        }
    }

    impl WalkEq for Pair {
        fn walk_eq(&self, other: &Self, walk: Walk<'_>) -> bool {
            VISITS.with(|v| v.set(v.get() + 1));
            self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
        }
    }

    impl WalkEq for Node {
        fn walk_eq(&self, other: &Self, walk: Walk<'_>) -> bool {
            match (self, other) {
                (Node::Leaf(a), Node::Leaf(b)) => walk.shared(a, b, false),
                (Node::Pair(a), Node::Pair(b)) => walk.shared(a, b, false),
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

    #[test]
    fn one_object_is_equal_to_itself_only_when_its_payload_is() {
        VISITS.with(|v| v.set(0));
        let pair = Shared::new(Pair(leaf(1.0), leaf(1.0)));
        let left = Cell::new(BUDGET);
        let memory = Memory::default();
        let walk = Walk {
            left: &left,
            memory: &memory,
        };
        assert!(walk.shared(&pair, &pair.clone(), true));
        assert_eq!(VISITS.with(Cell::get), 0);
        let nan = leaf(f64::NAN);
        assert!(nan != nan.clone());
    }
}
