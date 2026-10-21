//! `==` on values of a recursive sum at the cost of the graph they share.
//!
//! A recursive sum keeps each constructor's fields behind one [`Shared`], so a
//! subterm can sit under many nodes. Two equal values built apart are
//! different objects all the way down, and a comparison node by node meets a
//! shared subterm once for every path to it: the work is the size of the
//! unfolded tree, exponential in the depth of the sharing. Here a comparison
//! runs as it always did for at most [`BUDGET`] constructor pairs; one that
//! needs more starts again remembering the pairs of shared nodes it found
//! equal, and does not compare them again.

use crate::Shared;
use std::cell::Cell;

/// How many constructor pairs a comparison meets before it starts again
/// with memory. A comparison within it pays a counter; one past it repeats
/// at most this much work.
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

/// How a comparison goes on: within its budget, or with memory.
#[derive(Clone, Copy)]
pub struct Walk<'a> {
    left: &'a Cell<u32>,
    memory: Option<&'a Memory>,
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
    let same = a.walk_eq(
        b,
        Walk {
            left: &left,
            memory: None,
        },
    );
    if left.get() > 0 {
        return same;
    }
    with_memory(a, b)
}

/// [`walk_eq`] once the budget ran out: the answer a walk within it gave
/// up on, from the start.
#[cold]
#[inline(never)]
fn with_memory<T: WalkEq>(a: &T, b: &T) -> bool {
    let memory = Memory::default();
    let left = Cell::new(0);
    a.walk_eq(
        b,
        Walk {
            left: &left,
            memory: Some(&memory),
        },
    )
}

impl Walk<'_> {
    /// The payloads of one constructor of a recursive sum. `reflexive` says
    /// their `==` is (they hold no `Float`), so one object is equal to itself
    /// without being read.
    #[inline]
    pub fn shared<T: WalkEq>(self, a: &Shared<T>, b: &Shared<T>, reflexive: bool) -> bool {
        if reflexive && Shared::ptr_eq(a, b) {
            return true;
        }
        if let Some(memory) = self.memory {
            return self.remembering(memory, a, b);
        }
        // Out of budget: the answer is thrown away and found again.
        let left = self.left.get();
        if left == 0 {
            return false;
        }
        self.left.set(left - 1);
        a.walk_eq(b, self)
    }

    /// [`Walk::shared`] with memory: a pair of nodes both held in several
    /// places is looked up, and remembered once found equal. A node held in
    /// one place is reached by one path only.
    #[cold]
    #[inline(never)]
    fn remembering<T: WalkEq>(self, memory: &Memory, a: &Shared<T>, b: &Shared<T>) -> bool {
        if Shared::strong_count(a) == 1 || Shared::strong_count(b) == 1 {
            return a.walk_eq(b, self);
        }
        let pair = (Shared::as_ptr(a) as usize, Shared::as_ptr(b) as usize);
        let seen = memory.equal.take();
        let known = seen.as_ref().is_some_and(|seen| seen.contains(&pair));
        memory.equal.set(seen);
        if known {
            return true;
        }
        let same = a.walk_eq(b, self);
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
    use super::{BUDGET, Walk, WalkEq, walk_eq};
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

    /// What a comparison of 60 levels may pay: the walk within [`BUDGET`]
    /// it gives up, then a few pairs per level.
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
        let walk = Walk {
            left: &left,
            memory: None,
        };
        assert!(walk.shared(&pair, &pair.clone(), true));
        assert_eq!(VISITS.with(Cell::get), 0);
        let nan = leaf(f64::NAN);
        assert!(nan != nan.clone());
    }
}
