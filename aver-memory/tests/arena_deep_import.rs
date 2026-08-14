//! What `Arena::deep_import` costs to carry a map between two arenas.
//!
//! The import is the fourth place in the tree that built a persistent map by
//! rebuilding it — `new_map = new_map.insert(..)` over a table nothing else
//! could reach, once per entry. It is reached whenever a value crosses an arena
//! boundary: an independent product's arguments and its results, the constants
//! and globals of the parallel base context, and the values a replayed effect
//! boundary carries.
//!
//! The map type here is a stand-in, not the VM's: this crate's default build
//! has no `PersistentMap`, and the property under test is not about hashing at
//! all. It is about which of the two insert spellings the import calls. Both
//! are provided, both give the same map, and they differ only in whether the
//! table the caller was handed has to survive — so a double that can tell them
//! apart is exactly the instrument this needs. `TallyMap` mirrors
//! `aver_rt::AverMap` where that matters: `insert` clones and then inserts,
//! `insert_owned` inserts through `Arc::make_mut`, and `table_id` is the
//! backing allocation's address, which changes if and only if a rebuild
//! happened.

use aver_memory::{Arena, ArenaEntry, ArenaTypes, FnValueName, MapLike, NanValue};
use std::sync::Arc;

#[derive(Debug, Clone)]
struct NoFn;

impl FnValueName for NoFn {
    fn name(&self) -> &str {
        "<none>"
    }
}

/// A persistent map with `aver_rt::AverMap`'s copy-on-write behaviour and none
/// of its hashing: entries are kept in insertion order in a shared `Vec`.
#[derive(Debug, Clone, Default)]
struct TallyMap {
    entries: Arc<Vec<(u64, (NanValue, NanValue))>>,
}

impl MapLike for TallyMap {
    fn new() -> Self {
        Self::default()
    }

    fn get(&self, key: &u64) -> Option<&(NanValue, NanValue)> {
        self.entries.iter().find(|(k, _)| k == key).map(|(_, v)| v)
    }

    /// Preserving insert: the caller keeps the map it handed in, so the table
    /// has to be duplicated. Same shape as `AverMap::insert`.
    fn insert(&self, key: u64, value: (NanValue, NanValue)) -> Self {
        self.clone().insert_owned(key, value)
    }

    /// Consuming insert: writes through `Arc::make_mut`, which duplicates only
    /// when a second owner exists. Same shape as `AverMap::insert_owned`.
    fn insert_owned(mut self, key: u64, value: (NanValue, NanValue)) -> Self {
        let entries = Arc::make_mut(&mut self.entries);
        match entries.iter_mut().find(|(k, _)| *k == key) {
            Some(slot) => slot.1 = value,
            None => entries.push((key, value)),
        }
        self
    }

    fn rewrite_values_mut(&mut self, mut f: impl FnMut(&mut (NanValue, NanValue))) {
        for (_, value) in Arc::make_mut(&mut self.entries).iter_mut() {
            f(value);
        }
    }

    fn table_id(&self) -> usize {
        Arc::as_ptr(&self.entries) as *const u8 as usize
    }

    fn len(&self) -> usize {
        self.entries.len()
    }

    fn iter(&self) -> impl Iterator<Item = (&u64, &(NanValue, NanValue))> {
        self.entries.iter().map(|(k, v)| (k, v))
    }

    fn values(&self) -> impl Iterator<Item = &(NanValue, NanValue)> {
        self.entries.iter().map(|(_, v)| v)
    }
}

#[derive(Debug, Clone)]
struct TallyTypes;

impl ArenaTypes for TallyTypes {
    type Fn = NoFn;
    type Map = TallyMap;
}

/// A map of `n` string-keyed, string-valued entries in a fresh arena, with the
/// `NanValue` that points at it.
///
/// Both halves of every entry are heap-backed on purpose: the import recurses
/// into each one, which is what makes a rebuild-per-entry loop expensive in the
/// first place, and immediates would let the import skip the recursion.
fn arena_holding_map(n: usize) -> (Arena<TallyTypes>, NanValue) {
    let mut arena: Arena<TallyTypes> = Arena::new();
    let mut map = TallyMap::new();
    for i in 0..n {
        let key = NanValue::new_string_value(&format!("k{i}"), &mut arena);
        let value = NanValue::new_string_value(&format!("v{i}"), &mut arena);
        map = map.insert_owned(i as u64, (key, value));
    }
    let idx = arena.push_map(map);
    (arena, NanValue::new_map(idx))
}

/// Entries the destination arena duplicated while importing an `n`-entry map.
fn import_copies(n: usize) -> u64 {
    let (source, value) = arena_holding_map(n);
    let mut dest: Arena<TallyTypes> = Arena::new();
    let imported = dest.deep_import(value, &source);

    let ArenaEntry::Map { map, .. } = dest.get(imported.heap_index().expect("map is heap-backed"))
    else {
        panic!("import did not produce a map");
    };
    assert_eq!(map.len(), n, "import lost entries");

    dest.map_entries_copied()
}

#[test]
fn importing_a_map_into_another_arena_does_not_rebuild_its_table_per_entry() {
    // The map being filled is a local of the import, created one line above the
    // loop and pushed into the arena one line below it — nothing else can hold
    // it while it is being written. Spelling the loop with the preserving
    // `insert` therefore duplicated the whole table on every single entry, for
    // no one's benefit: 190 entries copied to import a 20-entry map, 4,950 to
    // import a 100-entry one, n^2/2 in general.
    //
    // Two sizes rather than one, because a per-entry constant and a quadratic
    // both look like "nonzero" at a single size.
    let small = import_copies(20);
    let large = import_copies(100);

    assert_eq!(
        (small, large),
        (0, 0),
        "the import rebuilt the map it was building: 20 entries cost {small} \
         duplications, 100 cost {large}",
    );
}

#[test]
fn an_imported_map_is_a_faithful_copy_in_the_destination_arena() {
    // The control for the test above: consuming the table under construction
    // must not change what the import produces. Every key and value has to
    // arrive with the same content and a heap index belonging to the
    // destination, not the source.
    let (source, value) = arena_holding_map(8);
    let mut dest: Arena<TallyTypes> = Arena::new();
    let imported = dest.deep_import(value, &source);

    let ArenaEntry::Map { map, .. } = dest.get(imported.heap_index().expect("map is heap-backed"))
    else {
        panic!("import did not produce a map");
    };
    let mut seen: Vec<(String, String)> = map
        .iter()
        .map(|(_, (k, v))| {
            (
                dest.get_string_value(*k).to_string(),
                dest.get_string_value(*v).to_string(),
            )
        })
        .collect();
    seen.sort();

    let expected: Vec<(String, String)> =
        (0..8).map(|i| (format!("k{i}"), format!("v{i}"))).collect();
    assert_eq!(seen, expected, "the imported map lost or altered entries");
}
