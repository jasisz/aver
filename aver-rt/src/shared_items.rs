//! `==` and the hash of a collection on the cycle of a recursive type.
//!
//! A type can hold itself through a `List`, a `Vector` or a `Map`, and a
//! value of it can then hold one subterm in many places: a list holding the
//! same value twice holds two copies of one pointer to the same items. Two
//! equal values built apart are different objects all the way down, and a
//! comparison item by item meets a shared subterm once for every path to it,
//! exponential in the depth of the sharing. The `==` and the hash of such a
//! type carry the walk of `aver_rt::Walk` and [`HashWalk`] through these
//! collections instead of beginning one of their own for each item. Each
//! pair of collections takes a step of the budget, its items compared at
//! once; once it is spent a pair of items is known by where its two items
//! are stored: an item is stored in one place, however many paths lead to
//! it.

use crate::Shared as Rc;
use crate::shared_hash::{HashWalk, node_hasher};
use crate::{AverList, AverListInner, AverListIter, AverMap, AverVector, NodeId, Walk};
use std::hash::{Hash, Hasher};

/// An item of a collection, as a walk sees it: where it is stored, and
/// whether its collection alone holds it there. An item a collection held
/// in one place alone holds is reached by as many paths as the collection
/// is, and is not remembered itself.
struct Item {
    addr: usize,
    alone: bool,
}

impl NodeId for Item {
    #[inline]
    fn addr(&self) -> usize {
        self.addr
    }

    #[inline]
    fn count(&self) -> usize {
        match self.alone {
            true => 1,
            false => 2,
        }
    }
}

#[inline]
fn item<T>(x: &T, alone: bool) -> Item {
    Item {
        addr: addr(x),
        alone,
    }
}

/// Where `x` is stored, which names it while the values compared live.
#[inline]
fn addr<T>(x: &T) -> usize {
    std::ptr::from_ref(x) as usize
}

/// The items of a list, each with whether the list alone holds it: every
/// node from the list down to the one storing the item is held in one
/// place. A list of a shape other than a flat body or a chain of prepends
/// counts as sharing all of its items.
struct Held<'a, T> {
    below: Option<&'a AverList<T>>,
    alone: bool,
    flat: std::slice::Iter<'a, T>,
    rest: Option<AverListIter<'a, T>>,
}

fn held<T>(list: &AverList<T>) -> Held<'_, T> {
    Held {
        below: Some(list),
        alone: true,
        flat: [].iter(),
        rest: None,
    }
}

impl<'a, T> Iterator for Held<'a, T> {
    type Item = (&'a T, bool);

    fn next(&mut self) -> Option<(&'a T, bool)> {
        if let Some(x) = self.flat.next() {
            return Some((x, self.alone));
        }
        if let Some(rest) = &mut self.rest {
            return rest.next().map(|x| (x, false));
        }
        let list = self.below.take()?;
        let node = list.inner.as_ref()?;
        self.alone &= Rc::strong_count(node) == 1;
        match &**node {
            AverListInner::Prepend { head, tail, .. } => {
                self.below = Some(tail);
                Some((head, self.alone))
            }
            AverListInner::Flat { items } => {
                self.alone &= Rc::strong_count(items) == 1;
                self.flat = items.get(list.start..).unwrap_or(&[]).iter();
                self.next()
            }
            _ => {
                self.rest = Some(list.iter());
                self.next()
            }
        }
    }
}

fn vector_alone<T>(vector: &AverVector<T>) -> bool {
    Rc::strong_count(&vector.inner) == 1
}

fn map_alone<K, V>(map: &AverMap<K, V>) -> bool {
    map.inner
        .as_ref()
        .is_none_or(|table| Rc::strong_count(table) == 1)
}

/// A collection that holds its items alone: no other collection holds any
/// of them, so each is reached by as many paths as the collection is.
pub trait Alone {
    fn alone(&self) -> bool;
}

/// A list holds its items alone when every node of it is held in one place:
/// a chain of prepends down to a flat body, read node by node.
impl<T> Alone for AverList<T> {
    fn alone(&self) -> bool {
        let mut list = self;
        loop {
            let Some(node) = list.inner.as_ref() else {
                return true;
            };
            if Rc::strong_count(node) != 1 {
                return false;
            }
            match &**node {
                AverListInner::Flat { items } => return Rc::strong_count(items) == 1,
                AverListInner::Prepend { tail, .. } => list = tail,
                _ => return false,
            }
        }
    }
}

impl<T> Alone for AverVector<T> {
    fn alone(&self) -> bool {
        vector_alone(self)
    }
}

impl<K, V> Alone for AverMap<K, V> {
    fn alone(&self) -> bool {
        map_alone(self)
    }
}

/// `==` on two collections on the cycle of a type, from an `==` that
/// carries no walk: the collections' own `==` when either holds its items
/// alone, as no item is met twice there; otherwise a walk of the pair,
/// `walk_eq` comparing it within the walk.
#[inline]
pub fn fast_eq<'v, C: Alone + PartialEq>(
    a: &'v C,
    b: &'v C,
    walk_eq: impl for<'w> FnOnce(&'v C, &'v C, Walk<'w, 'v>) -> bool,
) -> bool {
    match a.alone() || b.alone() {
        true => a == b,
        false => walked(a, b, walk_eq),
    }
}

#[cold]
#[inline(never)]
fn walked<'v, C>(
    a: &'v C,
    b: &'v C,
    walk_eq: impl for<'w> FnOnce(&'v C, &'v C, Walk<'w, 'v>) -> bool,
) -> bool {
    crate::shared_eq::walk_with(a, b, walk_eq)
}

/// How two items are compared within a walk: a closure the Rust backend
/// generates, holding nothing, so a pair of items can be left for later.
pub trait ItemEq<'v, T: 'v>: for<'w> Fn(&'v T, &'v T, Walk<'w, 'v>) -> bool + Copy + 'v {}

impl<'v, T: 'v, F> ItemEq<'v, T> for F where
    F: for<'w> Fn(&'v T, &'v T, Walk<'w, 'v>) -> bool + Copy + 'v
{
}

/// `==` on two lists, `eq` comparing two items within `walk`.
#[inline]
pub fn eq_list<'v, T>(
    a: &'v AverList<T>,
    b: &'v AverList<T>,
    walk: Walk<'_, 'v>,
    eq: impl ItemEq<'v, T>,
) -> bool {
    if a.len() != b.len() {
        return false;
    }
    if !walk.step() {
        return list_spent(a, b, walk, eq);
    }
    match (a.as_slice(), b.as_slice()) {
        (Some(x), Some(y)) => x.iter().zip(y).all(|(x, y)| eq(x, y, walk)),
        _ => a.iter().zip(b).all(|(x, y)| eq(x, y, walk)),
    }
}

/// [`eq_list`] once the budget is spent: left for later when the walk is
/// deep, else item by item, each pair of items neither list alone holds
/// known by where it is stored.
#[cold]
#[inline(never)]
fn list_spent<'v, T>(
    a: &'v AverList<T>,
    b: &'v AverList<T>,
    walk: Walk<'_, 'v>,
    eq: impl ItemEq<'v, T>,
) -> bool {
    if walk.deep() {
        return walk.defer(move |walk| list_spent(a, b, walk, eq));
    }
    match (a.as_slice(), b.as_slice()) {
        (Some(x), Some(y)) if a.alone() || b.alone() => {
            x.iter().zip(y).all(|(x, y)| eq(x, y, walk))
        }
        _ => held(a)
            .zip(held(b))
            .all(|((x, alone), (y, too))| eq_item(x, y, alone || too, walk, eq)),
    }
}

/// `==` on two vectors, `eq` comparing two items within `walk`.
#[inline]
pub fn eq_vector<'v, T: Clone>(
    a: &'v AverVector<T>,
    b: &'v AverVector<T>,
    walk: Walk<'_, 'v>,
    eq: impl ItemEq<'v, T>,
) -> bool {
    if a.len() != b.len() {
        return false;
    }
    if !walk.step() {
        return vector_spent(a, b, walk, eq);
    }
    a.iter().zip(b.iter()).all(|(x, y)| eq(x, y, walk))
}

#[cold]
#[inline(never)]
fn vector_spent<'v, T: Clone>(
    a: &'v AverVector<T>,
    b: &'v AverVector<T>,
    walk: Walk<'_, 'v>,
    eq: impl ItemEq<'v, T>,
) -> bool {
    if walk.deep() {
        return walk.defer(move |walk| vector_spent(a, b, walk, eq));
    }
    let alone = vector_alone(a) || vector_alone(b);
    a.iter()
        .zip(b.iter())
        .all(|(x, y)| eq_item(x, y, alone, walk, eq))
}

/// `==` on two maps, `eq` comparing the two values of a key within `walk`.
/// The keys are compared as the map compares them.
#[inline]
pub fn eq_map<'v, K: Eq + Hash + Clone, V: Clone>(
    a: &'v AverMap<K, V>,
    b: &'v AverMap<K, V>,
    walk: Walk<'_, 'v>,
    eq: impl ItemEq<'v, V>,
) -> bool {
    let (Some(x), Some(y)) = (&a.inner, &b.inner) else {
        // A map with no table is empty, as a table emptied by removals.
        return a.len() == b.len();
    };
    if x.len() != y.len() {
        return false;
    }
    if !walk.step() {
        return map_spent(a, b, walk, eq);
    }
    x.iter()
        .all(|(k, v)| y.get(k).is_some_and(|w| eq(v, w, walk)))
}

#[cold]
#[inline(never)]
fn map_spent<'v, K: Eq + Hash + Clone, V: Clone>(
    a: &'v AverMap<K, V>,
    b: &'v AverMap<K, V>,
    walk: Walk<'_, 'v>,
    eq: impl ItemEq<'v, V>,
) -> bool {
    if walk.deep() {
        return walk.defer(move |walk| map_spent(a, b, walk, eq));
    }
    let alone = map_alone(a) || map_alone(b);
    a.iter()
        .all(|(k, v)| b.get(k).is_some_and(|w| eq_item(v, w, alone, walk, eq)))
}

/// Two items once the budget is spent: compared at once when a collection
/// alone holds either, and otherwise known by where they are stored.
#[inline]
fn eq_item<'v, T: 'v>(
    x: &'v T,
    y: &'v T,
    alone: bool,
    walk: Walk<'_, 'v>,
    eq: impl ItemEq<'v, T>,
) -> bool {
    match alone {
        true => eq(x, y, walk),
        false => walk.remember(addr(x), addr(y), |walk| eq(x, y, walk)),
    }
}

/// Feeds the items of a list or a vector to `state` (see
/// [`crate::WalkHash::stream`]), `stream` feeding one item. A collection of
/// two or more items is where the walk branches, and takes one from the
/// budget.
#[inline]
pub fn stream_items<'a, T: 'a, H: Hasher>(
    items: impl ExactSizeIterator<Item = &'a T>,
    state: &mut H,
    left: u32,
    stream: impl Fn(&'a T, &mut H, u32) -> u32,
) -> u32 {
    items.len().hash(state);
    let left = match items.len() {
        0 | 1 => left,
        _ if left == 0 => return 0,
        _ => left - 1,
    };
    items.fold(left, |left, x| stream(x, state, left))
}

/// [`stream_items`] for a map: its entries in the order of their keys, the
/// key as the map hashes it and the value fed by `stream`.
pub fn stream_map<K: Eq + Hash + Clone + Ord, V: Clone, H: Hasher>(
    map: &AverMap<K, V>,
    state: &mut H,
    left: u32,
    stream: impl Fn(&V, &mut H, u32) -> u32,
) -> u32 {
    let mut entries: Vec<(&K, &V)> = map.iter().collect();
    entries.sort_by(|a, b| a.0.cmp(b.0));
    stream_items(entries.iter(), state, left, |entry, state, left| {
        entry.0.hash(state);
        stream(entry.1, state, left)
    })
}

/// The hash of a list made from parts (see [`crate::WalkHash::walk_hash`]),
/// `hash` making the hash of one item. The hash of an item its list may
/// share is made once and remembered by where the item is stored.
pub fn parts_list<T>(
    list: &AverList<T>,
    walk: HashWalk<'_>,
    hash: impl Fn(&T, HashWalk<'_>) -> u64,
) -> u64 {
    parts_items(list.len(), held(list), walk, hash)
}

/// [`parts_list`] for a vector.
pub fn parts_vector<T: Clone>(
    vector: &AverVector<T>,
    walk: HashWalk<'_>,
    hash: impl Fn(&T, HashWalk<'_>) -> u64,
) -> u64 {
    let alone = vector_alone(vector);
    parts_items(vector.len(), vector.iter().map(|x| (x, alone)), walk, hash)
}

fn parts_items<'a, T: 'a>(
    len: usize,
    items: impl Iterator<Item = (&'a T, bool)>,
    walk: HashWalk<'_>,
    hash: impl Fn(&'a T, HashWalk<'_>) -> u64,
) -> u64 {
    let mut h = node_hasher();
    len.hash(&mut h);
    for (x, alone) in items {
        h.write_u64(walk.node(&item(x, alone), |walk, h| h.write_u64(hash(x, walk))));
    }
    h.finish()
}

/// [`parts_list`] for a map: its entries in the order of their keys.
pub fn parts_map<K: Eq + Hash + Clone + Ord, V: Clone>(
    map: &AverMap<K, V>,
    walk: HashWalk<'_>,
    hash: impl Fn(&V, HashWalk<'_>) -> u64,
) -> u64 {
    let alone = map_alone(map);
    let mut entries: Vec<(&K, &V)> = map.iter().collect();
    entries.sort_by(|a, b| a.0.cmp(b.0));
    let mut h = node_hasher();
    entries.len().hash(&mut h);
    for (k, v) in entries {
        k.hash(&mut h);
        h.write_u64(walk.node(&item(v, alone), |walk, h| h.write_u64(hash(v, walk))));
    }
    h.finish()
}

#[cfg(test)]
mod tests {
    use crate::shared_hash::{BUDGET, HashWalk, WalkHash};
    use crate::{AverList, AverMap, Walk, WalkEq, walk_eq};
    use std::cell::Cell;
    use std::hash::{Hash, Hasher};

    thread_local! {
        static VISITS: Cell<usize> = const { Cell::new(0) };
    }

    /// The shape the Rust backend gives a sum holding itself through a list:
    /// `Node` counts the pairs of lists it compares and the lists it hashes
    /// from parts.
    #[derive(Clone, Debug)]
    enum Tree {
        Leaf(f64),
        Node(AverList<Tree>),
        Named(AverMap<i64, Tree>),
    }

    impl WalkEq for Tree {
        fn walk_eq<'v>(&'v self, other: &'v Self, walk: Walk<'_, 'v>) -> bool {
            match (self, other) {
                (Tree::Leaf(a), Tree::Leaf(b)) => a == b,
                (Tree::Node(a), Tree::Node(b)) => {
                    VISITS.with(|v| v.set(v.get() + 1));
                    super::eq_list(a, b, walk, |x, y, walk| x.walk_eq(y, walk))
                }
                (Tree::Named(a), Tree::Named(b)) => {
                    super::eq_map(a, b, walk, |x, y, walk| x.walk_eq(y, walk))
                }
                _ => false,
            }
        }
    }

    impl PartialEq for Tree {
        fn eq(&self, other: &Self) -> bool {
            walk_eq(self, other)
        }
    }

    impl WalkHash for Tree {
        fn stream<H: Hasher>(&self, state: &mut H, left: u32) -> u32 {
            match self {
                Tree::Leaf(n) => {
                    0u32.hash(state);
                    n.to_bits().hash(state);
                    left
                }
                Tree::Node(items) => {
                    1u32.hash(state);
                    super::stream_items(items.iter(), state, left, |x, state, left| {
                        x.stream(state, left)
                    })
                }
                Tree::Named(map) => {
                    2u32.hash(state);
                    super::stream_map(map, state, left, |x, state, left| x.stream(state, left))
                }
            }
        }

        fn walk_hash(&self, walk: HashWalk<'_>) -> u64 {
            match self {
                Tree::Leaf(n) => {
                    let mut h = crate::shared_hash::node_hasher();
                    n.to_bits().hash(&mut h);
                    h.finish()
                }
                Tree::Node(items) => {
                    VISITS.with(|v| v.set(v.get() + 1));
                    super::parts_list(items, walk, |x, walk| x.walk_hash(walk))
                }
                Tree::Named(map) => super::parts_map(map, walk, |x, walk| x.walk_hash(walk)),
            }
        }
    }

    impl Hash for Tree {
        fn hash<H: Hasher>(&self, state: &mut H) {
            crate::shared_hash::hash(self, state)
        }
    }

    fn leaf(n: f64) -> Tree {
        Tree::Leaf(n)
    }

    fn node(items: Vec<Tree>) -> Tree {
        Tree::Node(AverList::from_vec(items))
    }

    /// `n` levels above `leaf(seed)`, each a list holding the one below
    /// twice: n + 1 lists, 2^n leaves.
    fn twice(n: usize, seed: f64) -> Tree {
        (0..n).fold(leaf(seed), |below, _| node(vec![below.clone(), below]))
    }

    /// The tree of `twice(n, seed)` with its rightmost leaf `last`, built
    /// with a list held once at every level of its right spine, through
    /// lists of each shape and a map.
    fn spine(n: usize, seed: f64, last: f64) -> Tree {
        (0..n).fold(leaf(last), |below, i| match i % 3 {
            0 => node(vec![twice(i, seed), below]),
            1 => Tree::Node(AverList::prepend(
                twice(i, seed),
                &AverList::from_vec(vec![below]),
            )),
            _ => {
                let inner = AverMap::new().insert_owned(0, below);
                node(vec![twice(i, seed), Tree::Named(inner)])
            }
        })
    }

    /// How deep the shared values of the tests go: under Miri, deep enough
    /// to spend the budget, and no deeper.
    const N: usize = if cfg!(miri) { 12 } else { 60 };

    fn compare(a: &Tree, b: &Tree) -> (bool, usize) {
        VISITS.with(|v| v.set(0));
        let same = a == b;
        (same, VISITS.with(Cell::get))
    }

    #[test]
    fn equal_values_built_apart_cost_the_graph_not_the_tree() {
        let (same, visits) = compare(&twice(N, 1.0), &twice(N, 1.0));
        assert!(same);
        assert!(visits <= BUDGET as usize + 4 * N, "{visits} pairs compared");
        let (same, visits) = compare(&spine(N, 1.0, 1.0), &spine(N, 1.0, 1.0));
        assert!(same);
        assert!(visits <= BUDGET as usize + N * N, "{visits} pairs compared");
    }

    /// `n` levels, each a list of two lists that put a different leaf in
    /// front of one shared tail holding the level below: the lists in front
    /// are held once, the tail twice.
    fn tails(n: usize, last: f64) -> Tree {
        (0..n).fold(leaf(last), |below, _| {
            let tail = AverList::from_vec(vec![below]);
            node(vec![
                Tree::Node(AverList::prepend(leaf(1.0), &tail)),
                Tree::Node(AverList::prepend(leaf(2.0), &tail)),
            ])
        })
    }

    #[test]
    fn a_tail_two_lists_share_is_compared_once() {
        let (same, visits) = compare(&tails(N, 1.0), &tails(N, 1.0));
        assert!(same);
        assert!(visits <= BUDGET as usize + 4 * N, "{visits} pairs compared");
        let (same, visits) = compare(&tails(N, 1.0), &tails(N, 2.0));
        assert!(!same);
        assert!(visits <= BUDGET as usize + 4 * N, "{visits} pairs compared");
        let (a, visits) = hash(&tails(N, 1.0));
        assert!(visits <= 4 * (N + 1), "{visits} lists hashed from parts");
        assert_eq!(a, hash(&tails(N, 1.0)).0);
        assert_ne!(a, hash(&tails(N, 2.0)).0);
    }

    #[test]
    fn a_difference_deep_in_the_graph_is_still_found() {
        let (same, visits) = compare(&spine(N, 1.0, 1.0), &spine(N, 1.0, 2.0));
        assert!(!same);
        assert!(visits <= BUDGET as usize + N * N, "{visits} pairs compared");
        assert!(twice(12, 1.0) != twice(12, 2.0));
        assert!(twice(12, 1.0) != twice(13, 1.0));
        let nan = twice(12, f64::NAN);
        assert!(nan != nan.clone());
    }

    fn hash(value: &Tree) -> (u64, usize) {
        VISITS.with(|v| v.set(0));
        let hash = ahash::RandomState::with_seeds(1, 2, 3, 4).hash_one(value);
        (hash, VISITS.with(Cell::get))
    }

    #[test]
    fn equal_values_built_apart_hash_equally_at_the_cost_of_the_graph() {
        let (a, visits) = hash(&spine(N, 1.0, 1.0));
        assert!(
            visits <= (N + 1) * (N + 1),
            "{visits} lists hashed from parts"
        );
        let (b, _) = hash(&spine(N, 1.0, 1.0));
        assert_eq!(a, b);
        assert_ne!(a, hash(&spine(N, 1.0, 2.0)).0);
        // Each level holds two copies of the list below, stored apart: each
        // is hashed once, and the items under them once.
        let (a, visits) = hash(&twice(N, 1.0));
        assert!(visits <= 2 * (N + 1), "{visits} lists hashed from parts");
        assert_ne!(a, hash(&twice(N, 2.0)).0);
        // A value within the budget is fed item by item only.
        let (small, visits) = hash(&twice(5, 1.0));
        assert_eq!(visits, 0);
        assert_ne!(small, hash(&twice(6, 1.0)).0);
    }

    /// Lets a chain go one list at a time: dropping it whole would take a
    /// call per list.
    fn let_go(mut tree: Tree) {
        while let Tree::Node(items) = tree {
            match items.uncons_owned() {
                Some((below, _)) => tree = below,
                None => return,
            }
        }
    }

    /// A chain of lists far deeper than a test thread's stack takes one
    /// call per list for is compared: past `DEEP` bytes of stack the pairs
    /// of items are left for later, and a difference at the very bottom is
    /// found.
    #[test]
    fn a_deep_value_is_compared_on_a_stack_of_fixed_size() {
        let n = if cfg!(miri) { 1_500 } else { 200_000 };
        let chain = move |last: f64| (0..n).fold(leaf(last), |below, _| node(vec![below]));
        // The walk within the budget is a call per list: a thousand of them
        // fit in a debug build's frames on this stack, two hundred thousand
        // would not.
        std::thread::Builder::new()
            .stack_size(8 << 20)
            .spawn(move || {
                let (a, b, c) = (chain(1.0), chain(1.0), chain(2.0));
                let (same, visits) = compare(&a, &b);
                assert!(same);
                assert_eq!(visits, n);
                assert!(a != c && c != b);
                for tree in [a, b, c] {
                    let_go(tree);
                }
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
