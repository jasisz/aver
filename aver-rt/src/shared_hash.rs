//! The hash of a value of a recursive sum at the cost of the graph it shares.
//!
//! A subterm can sit under many nodes of a recursive sum, and a hash that
//! feeds the value to the hasher node by node meets it once for every path
//! to it: the work is the size of the unfolded tree, exponential in the depth
//! of the sharing. A hash feeds the value node by node, as it always did,
//! through at most [`BUDGET`] nodes where the walk branches: a node holding
//! two or more such values, the only place a walk meets a subterm once per
//! path. A walk down a single field of each node is as long as the value, and
//! counts nothing. Past the budget the hash stops and adds the hash of the
//! whole value made from its parts: a node's hash is made from its
//! constructor, its plain fields and the hashes of the nodes it holds, so the
//! hash of a node held in several places is made once and remembered.
//!
//! Equal values unfold alike, so they feed the same nodes before the budget
//! runs out, stop at the same place, and hash equally however they were
//! built.

use crate::NodeId;
pub use crate::shared_eq::BUDGET;
use std::cell::RefCell;
use std::hash::{BuildHasher, Hash, Hasher};

/// What the hash of one node is made with: fixed keys, so a node hashes the
/// same wherever and whenever it is met.
pub type NodeHasher = ahash::AHasher;

const NODE_KEYS: ahash::RandomState = ahash::RandomState::with_seeds(
    0x243f_6a88_85a3_08d3,
    0x1319_8a2e_0370_7344,
    0xa409_3822_299f_31d0,
    0x082e_fa98_ec4e_6c89,
);

/// A fresh hasher for one node, or for a record holding nodes.
#[inline]
pub fn node_hasher() -> NodeHasher {
    NODE_KEYS.build_hasher()
}

/// The hash of a constructor without fields, which has no node.
#[inline]
pub fn tag_hash(tag: u32) -> u64 {
    (u64::from(tag) + 1).wrapping_mul(0x9e37_79b9_7f4a_7c15)
}

/// The memory of a hash made from parts: the hashes of nodes held in more
/// than one place, by address.
#[derive(Clone, Copy)]
pub struct HashWalk<'a> {
    memory: &'a RefCell<ahash::AHashMap<usize, u64>>,
}

/// The hash of a recursive sum, and of a record on its cycle, as the Rust
/// backend generates it.
pub trait WalkHash {
    /// Feeds the value to `state` node by node, as a derived `Hash` would,
    /// out of a budget of `left` nodes where the walk branches, and answers
    /// what is left of it. Such a node met once the budget is spent feeds
    /// nothing more and answers 0.
    fn stream<H: Hasher>(&self, state: &mut H, left: u32) -> u32;
    /// The hash made from the value's parts.
    fn walk_hash(&self, walk: HashWalk<'_>) -> u64;
}

impl<T: WalkHash> WalkHash for Option<T> {
    #[inline]
    fn stream<H: Hasher>(&self, state: &mut H, left: u32) -> u32 {
        core::mem::discriminant(self).hash(state);
        match self {
            Some(value) => value.stream(state, left),
            None => left,
        }
    }

    #[inline]
    fn walk_hash(&self, walk: HashWalk<'_>) -> u64 {
        match self {
            Some(value) => value.walk_hash(walk),
            None => 0,
        }
    }
}

/// A top-level hash: what the `Hash` of a recursive sum does. A branching
/// node met once the budget is spent returns at once, so a walk that runs
/// out ends after a walk down the fields it was in.
#[inline]
pub fn hash<T: WalkHash, H: Hasher>(value: &T, state: &mut H) {
    if value.stream(state, BUDGET) == 0 {
        state.write_u64(from_parts(value));
    }
}

/// The hash of a value made from its parts.
#[cold]
#[inline(never)]
pub fn from_parts<T: WalkHash>(value: &T) -> u64 {
    let memory = RefCell::default();
    value.walk_hash(HashWalk { memory: &memory })
}

impl HashWalk<'_> {
    /// The hash of one node of a recursive sum: `fields` writes its tag and
    /// fields to the hasher, a field holding nodes as its
    /// [`WalkHash::walk_hash`]. The hash of a node held in several places is
    /// looked up, and remembered once made; the value hashed holds every
    /// node it meets, so an address names one node for the whole hash. A
    /// node held in one place is reached by one path only.
    #[inline]
    pub fn node<N: NodeId>(self, node: &N, fields: impl FnOnce(Self, &mut NodeHasher)) -> u64 {
        let made = |walk| {
            let mut hasher = node_hasher();
            fields(walk, &mut hasher);
            hasher.finish()
        };
        if node.count() == 1 {
            return made(self);
        }
        let known = self.memory.borrow().get(&node.addr()).copied();
        if let Some(hash) = known {
            return hash;
        }
        let hash = made(self);
        self.memory.borrow_mut().insert(node.addr(), hash);
        hash
    }
}

#[cfg(test)]
mod tests {
    use super::{BUDGET, HashWalk, WalkHash, from_parts, tag_hash};
    use crate::Shared;
    use std::cell::Cell;
    use std::hash::{BuildHasher, Hash, Hasher};

    thread_local! {
        static VISITS: Cell<usize> = const { Cell::new(0) };
    }

    /// The shape the Rust backend gives a recursive sum: `Pair` counts the
    /// nodes it hashes from parts.
    #[derive(Clone)]
    enum Node {
        Leaf(i64),
        Pair(Shared<(Node, Node)>),
    }

    impl WalkHash for Node {
        fn stream<H: Hasher>(&self, state: &mut H, left: u32) -> u32 {
            match self {
                Node::Leaf(n) => {
                    0u32.hash(state);
                    n.hash(state);
                    left
                }
                Node::Pair(pair) => {
                    if left == 0 {
                        return 0;
                    }
                    1u32.hash(state);
                    let left = pair.0.stream(state, left - 1);
                    pair.1.stream(state, left)
                }
            }
        }

        fn walk_hash(&self, walk: HashWalk<'_>) -> u64 {
            match self {
                Node::Leaf(n) => tag_hash(*n as u32),
                Node::Pair(pair) => walk.node(pair, |walk, h| {
                    VISITS.with(|v| v.set(v.get() + 1));
                    1u32.hash(h);
                    h.write_u64(pair.0.walk_hash(walk));
                    h.write_u64(pair.1.walk_hash(walk));
                }),
            }
        }
    }

    impl Hash for Node {
        fn hash<H: Hasher>(&self, state: &mut H) {
            super::hash(self, state)
        }
    }

    fn pair(a: Node, b: Node) -> Node {
        Node::Pair(Shared::new((a, b)))
    }

    /// `n` levels above `Leaf(seed)`, each holding the one below twice.
    fn twice(n: usize, seed: i64) -> Node {
        (0..n).fold(Node::Leaf(seed), |below, _| pair(below.clone(), below))
    }

    /// The tree of `twice(n, seed)` with its rightmost leaf `last`, built
    /// with a node held once at every level of its right spine.
    fn spine(n: usize, seed: i64, last: i64) -> Node {
        (0..n).fold(Node::Leaf(last), |below, i| pair(twice(i, seed), below))
    }

    fn hash(value: &Node) -> (u64, usize) {
        VISITS.with(|v| v.set(0));
        let hash = ahash::RandomState::with_seeds(1, 2, 3, 4).hash_one(value);
        (hash, VISITS.with(Cell::get))
    }

    #[test]
    fn equal_values_built_apart_hash_equally_at_the_cost_of_the_graph() {
        let (a, visits) = hash(&twice(60, 1));
        assert!(visits <= 61, "{visits} nodes hashed from parts");
        let (b, visits) = hash(&spine(60, 1, 1));
        assert!(visits <= 61 * 61, "{visits} nodes hashed from parts");
        assert_eq!(a, b);
        // A value within the budget is fed node by node only.
        let (small, visits) = hash(&twice(5, 1));
        assert_eq!(visits, 0);
        assert_eq!(small, hash(&spine(5, 1, 1)).0);
    }

    #[test]
    fn different_values_hash_differently() {
        assert_ne!(hash(&twice(60, 1)).0, hash(&spine(60, 1, 2)).0);
        assert_ne!(hash(&twice(60, 1)).0, hash(&twice(60, 2)).0);
        assert_ne!(hash(&twice(12, 1)).0, hash(&twice(13, 1)).0);
        assert_ne!(from_parts(&twice(12, 1)), from_parts(&twice(12, 2)));
    }

    #[test]
    fn the_budget_counts_the_unfolded_value() {
        let mut sink = ahash::RandomState::with_seeds(1, 2, 3, 4).build_hasher();
        assert_eq!(twice(9, 1).stream(&mut sink, BUDGET), BUDGET - 511);
        assert_eq!(twice(11, 1).stream(&mut sink, BUDGET), 0);
        // A walk past the budget ends: 2^60 nodes unfolded.
        assert_eq!(twice(60, 1).stream(&mut sink, BUDGET), 0);
    }
}
