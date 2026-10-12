/// Map namespace — immutable key/value map helpers.
///
/// Methods:
///   Map.set(map, key, value)    → Map<K, V>
///   Map.get(map, key)           → Option<V>
///   Map.remove(map, key)        → Map<K, V>
///   Map.has(map, key)           → Bool
///   Map.keys(map)               → List<K>
///   Map.values(map)             → List<V>
///   Map.entries(map)            → List<Tuple<K, V>>
///   Map.len(map)                → Int
///   Map.fromList(pairs)         → Map<K, V> where each pair is (key, value)
///
/// The empty map is the literal `{}` (with type from context); there is
/// no `Map.empty()` builtin since 0.17 — symmetric with `[]` for List.
///
/// Key constraint: only scalar keys are allowed (Int, Float, String, Bool).
///
/// No effects required.
use std::cmp::Ordering;

use aver_memory::TakenMap;

use crate::nan_value::{Arena, LaneMark, NanValue, PersistentMap};
use crate::value::{RuntimeError, Value, aver_repr};

/// Order the NaN-boxed representation's keys the way [`compare_scalar_keys`]
/// orders the tree-walk representation's.
///
/// Both VM representations have to iterate a map identically, and the sort
/// used to be by *printed* key here — so `{2, 10}` came out `[10, 2]` on the
/// NaN-boxed path and `[2, 10]` on the tree-walk path, and a string key
/// containing an escape sorted by its escape sequence rather than by its
/// characters. Converting the key back to a `Value` keeps one comparator for
/// both paths rather than a second copy that can drift.
fn sort_keys_nv(keys: &mut [NanValue], arena: &Arena) {
    use crate::nan_value::NanValueConvert;
    let mut keyed: Vec<(Value, NanValue)> = keys.iter().map(|k| (k.to_value(arena), *k)).collect();
    keyed.sort_by(|(a, _), (b, _)| compare_keys(a, b));
    for (slot, (_, key)) in keys.iter_mut().zip(keyed) {
        *slot = key;
    }
}

/// [`sort_keys_nv`] for `(key, value)` pairs — ordered by key alone.
fn sort_entries_nv(entries: &mut [(NanValue, NanValue)], arena: &Arena) {
    use crate::nan_value::NanValueConvert;
    let mut keyed: Vec<(Value, (NanValue, NanValue))> = entries
        .iter()
        .map(|pair| (pair.0.to_value(arena), *pair))
        .collect();
    keyed.sort_by(|(a, _), (b, _)| compare_keys(a, b));
    for (slot, (_, pair)) in entries.iter_mut().zip(keyed) {
        *slot = pair;
    }
}

/// The canonical order a map iterates its keys in.
///
/// It has to be a function of the key's CONTENT, because every backend and
/// the proof model state it independently and they have to agree. Ordering a
/// composite key by its printed form — which is what this used to fall back
/// on — is not that: it reads the renderer, so `(10, 1)` sorted below `(2, 1)`
/// on the VM while compiled Rust, comparing componentwise, put it above.
///
/// A record orders by FIELD NAME, not by the order the fields were declared
/// in. Declaration order is not observable anywhere else in the language —
/// records are built and read by name, and there is no positional pattern —
/// so ordering by it would make a neutral refactor change how every map on
/// that key iterates. A variant orders by constructor name for the same
/// reason, then by its payload.
/// The one order a map shows its keys in, shared with the replay codec so a
/// recording lists entries the way every other door does.
pub(crate) fn compare_keys(a: &Value, b: &Value) -> Ordering {
    fn seq(xs: &[Value], ys: &[Value]) -> Ordering {
        for (x, y) in xs.iter().zip(ys.iter()) {
            let ord = compare_keys(x, y);
            if ord != Ordering::Equal {
                return ord;
            }
        }
        xs.len().cmp(&ys.len())
    }

    match (a, b) {
        (Value::Unit, Value::Unit) => Ordering::Equal,
        (Value::None, Value::None) => Ordering::Equal,
        (Value::None, Value::Some(_)) => Ordering::Less,
        (Value::Some(_), Value::None) => Ordering::Greater,
        (Value::Some(x), Value::Some(y)) => compare_keys(x, y),
        (Value::Ok(x), Value::Ok(y)) => compare_keys(x, y),
        (Value::Err(x), Value::Err(y)) => compare_keys(x, y),
        (Value::Ok(_), Value::Err(_)) => Ordering::Less,
        (Value::Err(_), Value::Ok(_)) => Ordering::Greater,
        (Value::Tuple(xs), Value::Tuple(ys)) => seq(xs, ys),
        (Value::List(xs), Value::List(ys)) => {
            let xs: Vec<Value> = xs.iter().cloned().collect();
            let ys: Vec<Value> = ys.iter().cloned().collect();
            seq(&xs, &ys)
        }
        (Value::Vector(xs), Value::Vector(ys)) => {
            let xs: Vec<Value> = xs.iter().cloned().collect();
            let ys: Vec<Value> = ys.iter().cloned().collect();
            seq(&xs, &ys)
        }
        (Value::Record { fields: xs, .. }, Value::Record { fields: ys, .. }) => {
            compare_fields_by_name(xs, ys)
        }
        (
            Value::Variant {
                variant: vx,
                fields: xs,
                ..
            },
            Value::Variant {
                variant: vy,
                fields: ys,
                ..
            },
        ) => vx.cmp(vy).then_with(|| seq(xs, ys)),
        _ => compare_scalar_keys(a, b),
    }
}

/// Compare two records of the same type field by field, taking the fields in
/// alphabetical order of their names.
fn compare_fields_by_name(xs: &[(String, Value)], ys: &[(String, Value)]) -> Ordering {
    // Both sides are the same record type, so they carry the same names in the
    // same layout; one permutation orders both.
    let mut order: Vec<usize> = (0..xs.len()).collect();
    order.sort_by(|&i, &j| xs[i].0.cmp(&xs[j].0));
    for i in order {
        if i >= ys.len() {
            return Ordering::Greater;
        }
        let ord = compare_keys(&xs[i].1, &ys[i].1);
        if ord != Ordering::Equal {
            return ord;
        }
    }
    xs.len().cmp(&ys.len())
}

fn compare_scalar_keys(a: &Value, b: &Value) -> Ordering {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => x.cmp(y),
        // `total_cmp` and not `partial_cmp` with a bit-pattern fallback: that
        // combination is not a total order and the standard library detects it
        // and aborts. A NaN compares greater than `1.0` by bit pattern and less
        // than `-1.0` by bit pattern (the sign bit outranks the exponent), while
        // `-1.0 < 1.0` — a three-element cycle, which `sort_by` reports as
        // "user-provided comparison function does not correctly implement a
        // total order". `total_cmp` is IEEE 754 `totalOrder`: every NaN sits
        // outside the finite range on the side its sign bit names, so the
        // comparison is decided by the values alone and never cycles.
        //
        // `-0.0` sorts just below `0.0` here rather than comparing equal, which
        // costs nothing: the map folds the two into a single key long before
        // the sort sees them (`NanValue::hash_in` hashes any zero as `+0.0`),
        // so no map ever holds both.
        (Value::Float(x), Value::Float(y)) => x.total_cmp(y),
        (Value::Str(x), Value::Str(y)) => x.cmp(y),
        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
        _ => aver_repr(a).cmp(&aver_repr(b)),
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Map.set" => Some(set_nv(args, arena)),
        "Map.get" => Some(get_nv(args, arena)),
        "Map.remove" => Some(remove_nv(args, arena)),
        "Map.has" => Some(has_nv(args, arena)),
        "Map.keys" => Some(keys_nv(args, arena)),
        "Map.values" => Some(values_nv(args, arena)),
        "Map.entries" => Some(entries_nv(args, arena)),
        "Map.len" => Some(len_nv(args, arena)),
        "Map.fromList" => Some(from_list_nv(args, arena)),
        _ => None,
    }
}

fn is_hashable_nv(v: NanValue) -> bool {
    // Functions are the only Aver value not hashable — see the `Value`
    // path above. Everything else (variants, tuples, records, lists,
    // vectors, wrappers, scalars) participates in `hash_in` and
    // `eq_in`.
    !v.is_fn()
}

fn ensure_hashable_nv(name: &str, v: NanValue) -> Result<(), RuntimeError> {
    if is_hashable_nv(v) {
        Ok(())
    } else {
        Err(RuntimeError::Error(format!(
            "{}: key must be hashable (functions are not)",
            name
        )))
    }
}

/// Where `key` lives in `map`, or where an insert has to put it.
///
/// Every `Map.*` builtin finds a key through this and nothing else: the table
/// is addressed by the key's hash, and a hash does not identify a key.
fn key_slot(map: &PersistentMap, key: NanValue, arena: &Arena) -> Result<u64, u64> {
    aver_memory::map_key_slot_from(map, key, arena, key_home(key, arena))
}

/// The slot the search for `key` starts at: its hash.
#[cfg(not(test))]
fn key_home(key: NanValue, arena: &Arena) -> u64 {
    key.map_key_hash_deep(arena)
}

/// The slot the search for `key` starts at: its hash, unless the running test
/// placed the key (`tests::place`). Three keys with one real hash are out of
/// reach to compute, and the tests of longer runs of keys need them.
#[cfg(test)]
fn key_home(key: NanValue, arena: &Arena) -> u64 {
    tests::placed(key).unwrap_or_else(|| key.map_key_hash_deep(arena))
}

/// Remove `key` from a table the caller owns, keeping every other key findable.
///
/// A key whose own slot was taken sits in the slot after it, and a lookup stops
/// at the first empty slot. Emptying a slot can therefore cut a later key off
/// from where its search starts, so the keys behind the removed one are moved
/// up into the gap whenever that brings them no further than their own hash.
/// `moved(from, to)` reports each such move. A map with no shared hash has
/// nothing behind the removed key and pays one table read for the check.
fn remove_key(
    map: PersistentMap,
    key: NanValue,
    arena: &Arena,
    mut moved: impl FnMut(u64, u64),
) -> (PersistentMap, Option<u64>) {
    let Ok(removed) = key_slot(&map, key, arena) else {
        return (map, None);
    };
    let mut map = map.remove_owned(&removed);
    let mut gap = removed;
    let mut probe = gap.wrapping_add(1);
    while let Some(&(stored_key, stored_value)) = map.get(&probe) {
        let home = key_home(stored_key, arena);
        if gap.wrapping_sub(home) < probe.wrapping_sub(home) {
            map = map
                .remove_owned(&probe)
                .insert_owned(gap, (stored_key, stored_value));
            moved(probe, gap);
            gap = probe;
        }
        probe = probe.wrapping_add(1);
    }
    (map, Some(removed))
}

/// Transient proof supplied by the VM frame that owns the next destructive
/// boundary. It is never stored in an arena entry: it only lets an owned map
/// update retain the logical age of the table bulk that predates that frame.
/// A newly inserted pair that does not predate it is recorded separately in
/// the map's remembered set instead of making the entire table look young.
#[derive(Debug, Clone, Copy)]
pub(crate) struct OwnedMapFrameProof {
    pub arena_mark: u32,
    pub yard_mark: u32,
    pub handoff_mark: u32,
    pub lane_mark: LaneMark,
    pub inplace_write_escaped: bool,
}

impl OwnedMapFrameProof {
    fn direct_value_predates_frame(self, value: NanValue, arena: &Arena) -> bool {
        value.is_immediate()
            || value.heap_index().is_some_and(|index| {
                !arena.is_frame_local_index(
                    index,
                    self.arena_mark,
                    self.yard_mark,
                    self.handoff_mark,
                )
            })
    }

    fn receipt_for_owned_source(self, source: NanValue, arena: &Arena) -> Option<LaneMark> {
        if self.inplace_write_escaped || !arena.lane_mark_is_valid(self.lane_mark) {
            return None;
        }

        let source_receipt = arena.map_scan_receipt_value(source);
        let source_is_clean = source.is_empty_map_immediate()
            || arena.map_all_immediate_value(source)
            || (arena.lane_mark_is_valid(source_receipt) && source_receipt <= self.lane_mark);

        source_is_clean.then_some(self.lane_mark)
    }

    fn pair_predates_frame(self, key: NanValue, value: NanValue, arena: &Arena) -> bool {
        self.direct_value_predates_frame(key, arena)
            && self.direct_value_predates_frame(value, arena)
    }
}

/// Map.set with sole-owned first argument — takes instead of cloning.
pub(crate) fn set_nv_owned(
    args: &[NanValue],
    arena: &mut Arena,
    frame_proof: Option<OwnedMapFrameProof>,
) -> Result<NanValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "Map.set() takes 3 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.set() first argument must be a Map".to_string(),
        ));
    }
    ensure_hashable_nv("Map.set", args[1])?;
    let source = args[0];
    // The one map builder that is O(1) per insert, so it is also the one that
    // must not re-decide `all_immediate` by reading the table — that would put
    // the per-step walk back under a different name. The new map holds exactly
    // what the old one held plus this key and value, and an insert can only
    // drop an entry, never add one this pair did not bring, so the old flag and
    // the two arguments decide the new flag between them.
    let all_immediate =
        arena.map_all_immediate_value(source) && args[1].is_immediate() && args[2].is_immediate();
    let inherited_provenance = frame_proof.and_then(|proof| {
        proof
            .receipt_for_owned_source(source, arena)
            .map(|receipt| (receipt, proof.pair_predates_frame(args[1], args[2], arena)))
    });
    // The one pair `push_map`'s pass would have marked, marked here instead:
    // the new entry holds this key and this value, and if either is a map, this
    // entry is a holder of its slot from now on. Everything else in the table
    // was marked when it went in.
    arena.note_held_elsewhere(args[1]);
    arena.note_held_elsewhere(args[2]);
    let TakenMap {
        map: old_map,
        scan_receipt: _,
        mut pending_scan_keys,
    } = arena.take_map_value(source);
    let (Ok(key_hash) | Err(key_hash)) = key_slot(&old_map, args[1], arena);
    let new_map = old_map.insert_owned(key_hash, (args[1], args[2]));
    let scan_receipt = if let Some((receipt, pair_predates_frame)) = inherited_provenance {
        if pair_predates_frame {
            pending_scan_keys.retain(|pending| *pending != key_hash);
        } else if !pending_scan_keys.contains(&key_hash) {
            pending_scan_keys.push(key_hash);
        }
        receipt
    } else {
        // The current mark covers the entire table, including the inserted
        // pair, so any older remembered exceptions are subsumed by it.
        pending_scan_keys.clear();
        arena.lane_mark()
    };
    let map_idx = arena.push_inheriting_source_space(
        aver_memory::ArenaEntry::Map {
            map: new_map,
            all_immediate,
            scan_receipt,
            pending_scan_keys,
            // A fresh entry nobody has been handed. The table it carries came
            // out of a slot the caller proved nothing else reaches, so nothing
            // reaches this one either until somebody stores it.
            holder_count: 0,
        },
        source,
    );
    Ok(NanValue::new_map(map_idx))
}

/// Map.remove with sole-owned first argument — takes instead of cloning.
///
/// The mirror of [`set_nv_owned`], and the same three moves: derive the flag
/// from the map being consumed rather than by reading the table, take the table
/// out of its slot, and put the result back in the space the source came from.
/// A removal adds nothing, so there is no pair to mark and the `all_immediate`
/// claim can only get truer than the one it inherits.
pub fn remove_nv_owned(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Map.remove() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.remove() first argument must be a Map".to_string(),
        ));
    }
    ensure_hashable_nv("Map.remove", args[1])?;
    let source = args[0];
    let all_immediate = arena.map_all_immediate_value(source);
    let TakenMap {
        map: old_map,
        scan_receipt,
        mut pending_scan_keys,
    } = arena.take_map_value(source);
    // The remembered set names pairs by slot, so it follows the table: the
    // removed slot is forgotten first, then a pair that moved is renamed.
    let mut renamed: Vec<(u64, u64)> = Vec::new();
    let (new_map, removed) = remove_key(old_map, args[1], arena, |from, to| {
        renamed.push((from, to));
    });
    if let Some(removed) = removed {
        pending_scan_keys.retain(|pending| *pending != removed);
    }
    for (from, to) in renamed {
        for pending in pending_scan_keys.iter_mut() {
            if *pending == from {
                *pending = to;
            }
        }
    }
    if new_map.is_empty() {
        return Ok(NanValue::EMPTY_MAP);
    }
    let map_idx = arena.push_inheriting_source_space(
        aver_memory::ArenaEntry::Map {
            map: new_map,
            all_immediate,
            scan_receipt,
            pending_scan_keys,
            holder_count: 0,
        },
        source,
    );
    Ok(NanValue::new_map(map_idx))
}

fn set_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "Map.set() takes 3 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.set() first argument must be a Map".to_string(),
        ));
    }
    ensure_hashable_nv("Map.set", args[1])?;
    let old_map = arena.clone_map_value(args[0]);
    // The target stays reachable through its arena slot, so `insert` duplicates
    // the whole storage. `set_nv_owned` is the path that avoids this.
    arena.note_map_entries_copied(old_map.len());
    let (Ok(key_hash) | Err(key_hash)) = key_slot(&old_map, args[1], arena);
    let new_map = old_map.insert(key_hash, (args[1], args[2]));
    let map_idx = arena.push_map(new_map);
    Ok(NanValue::new_map(map_idx))
}

fn get_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Map.get() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.get() first argument must be a Map".to_string(),
        ));
    }
    ensure_hashable_nv("Map.get", args[1])?;
    let map = arena.map_ref_value(args[0]);
    let found = key_slot(map, args[1], arena)
        .ok()
        .and_then(|slot| map.get(&slot))
        .map(|(_, value)| *value);
    match found {
        Some(v) => Ok(NanValue::new_some_value(v, arena)),
        None => Ok(NanValue::NONE),
    }
}

fn remove_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Map.remove() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.remove() first argument must be a Map".to_string(),
        ));
    }
    ensure_hashable_nv("Map.remove", args[1])?;
    let old_map = arena.clone_map_value(args[0]);
    let (entries, table) = (old_map.len(), old_map.table_id());
    // The arena still holds this table, so the first write duplicates it and
    // the rest of the removal works on that private copy. An absent key writes
    // nothing, and then nothing was copied.
    let (new_map, _) = remove_key(old_map, args[1], arena, |_, _| {});
    if new_map.table_id() != table {
        arena.note_map_entries_copied(entries);
    }
    if new_map.is_empty() {
        Ok(NanValue::EMPTY_MAP)
    } else {
        let map_idx = arena.push_map(new_map);
        Ok(NanValue::new_map(map_idx))
    }
}

fn has_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Map.has() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.has() first argument must be a Map".to_string(),
        ));
    }
    ensure_hashable_nv("Map.has", args[1])?;
    let map = arena.map_ref_value(args[0]);
    Ok(NanValue::new_bool(key_slot(map, args[1], arena).is_ok()))
}

fn keys_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Map.keys() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.keys() argument must be a Map".to_string(),
        ));
    }
    let map = arena.clone_map_value(args[0]);
    let mut keys: Vec<NanValue> = map.values().map(|(k, _)| *k).collect();
    sort_keys_nv(&mut keys, arena);
    if keys.is_empty() {
        return Ok(NanValue::EMPTY_LIST);
    }
    let list_idx = arena.push_list(keys);
    Ok(NanValue::new_list(list_idx))
}

fn values_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Map.values() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.values() argument must be a Map".to_string(),
        ));
    }
    let map = arena.clone_map_value(args[0]);
    let mut entries: Vec<(NanValue, NanValue)> = map.values().cloned().collect();
    sort_entries_nv(&mut entries, arena);
    let vals: Vec<NanValue> = entries.into_iter().map(|(_, v)| v).collect();
    if vals.is_empty() {
        return Ok(NanValue::EMPTY_LIST);
    }
    let list_idx = arena.push_list(vals);
    Ok(NanValue::new_list(list_idx))
}

fn entries_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Map.entries() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.entries() argument must be a Map".to_string(),
        ));
    }
    let map = arena.clone_map_value(args[0]);
    let mut entries: Vec<(NanValue, NanValue)> = map.values().cloned().collect();
    sort_entries_nv(&mut entries, arena);
    let pairs: Vec<NanValue> = entries
        .into_iter()
        .map(|(k, v)| {
            let tuple_idx = arena.push_tuple(vec![k, v]);
            NanValue::new_tuple(tuple_idx)
        })
        .collect();
    if pairs.is_empty() {
        return Ok(NanValue::EMPTY_LIST);
    }
    let list_idx = arena.push_list(pairs);
    Ok(NanValue::new_list(list_idx))
}

fn len_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Map.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_map() {
        return Err(RuntimeError::Error(
            "Map.len() argument must be a Map".to_string(),
        ));
    }
    let map = arena.map_ref_value(args[0]);
    Ok(NanValue::new_int(map.len() as i64, arena))
}

fn from_list_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Map.fromList() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "Map.fromList() argument must be a List of (key, value) tuples".to_string(),
        ));
    }
    let items = arena.list_to_vec_value(args[0]);
    let mut out = PersistentMap::new();
    // The map under construction is ours alone until it is pushed into the
    // arena, so every entry goes in through the owned path. Spelling this loop
    // `out = out.insert(..)` — which is what it used to be — made `Map.fromList`
    // quadratic in its own input all by itself: `insert` takes `&self` and has
    // to preserve the map it is handed, so `Rc::make_mut` rebuilt the whole
    // table once per entry. That is n^2/2 entries to turn a list of pairs into a
    // map, which is the replay-a-log shape from issue #900 and the one the empty
    // seed `Map.fromList([])` never reached.
    //
    // `table_id` is what keeps that honest. It changes only when the table was
    // actually rebuilt, so the count below is the duplication that happened
    // rather than an inference from which method this line names.
    let mut table = out.table_id();
    for (idx, pair) in items.iter().enumerate() {
        if !pair.is_tuple() {
            return Err(RuntimeError::Error(format!(
                "Map.fromList() item {} must be (key, value)",
                idx + 1
            )));
        }
        let parts = arena.get_tuple(pair.arena_index());
        if parts.len() != 2 {
            return Err(RuntimeError::Error(format!(
                "Map.fromList() item {} must have 2 elements",
                idx + 1
            )));
        }
        let key = parts[0];
        let value = parts[1];
        ensure_hashable_nv("Map.fromList", key)?;
        let (Ok(key_hash) | Err(key_hash)) = key_slot(&out, key, arena);
        let entries_before = out.len();
        out = out.insert_owned(key_hash, (key, value));
        let table_after = out.table_id();
        if table_after != table {
            arena.note_map_entries_copied(entries_before);
            table = table_after;
        }
    }
    if out.is_empty() {
        Ok(NanValue::EMPTY_MAP)
    } else {
        let map_idx = arena.push_map(out);
        Ok(NanValue::new_map(map_idx))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn frame_proof(arena: &Arena) -> OwnedMapFrameProof {
        OwnedMapFrameProof {
            arena_mark: arena.young_len() as u32,
            yard_mark: arena.yard_len() as u32,
            handoff_mark: arena.handoff_len() as u32,
            lane_mark: arena.lane_mark(),
            inplace_write_escaped: false,
        }
    }

    fn heap_pair(arena: &mut Arena, suffix: &str) -> (NanValue, NanValue) {
        (
            NanValue::new_string_value(&format!("proof-key-{suffix}"), arena),
            NanValue::new_string_value(&format!("proof-value-{suffix}"), arena),
        )
    }

    fn map_flag_and_actual(arena: &Arena, value: NanValue) -> (bool, bool) {
        let aver_memory::ArenaEntry::Map {
            map, all_immediate, ..
        } = arena.get(value.arena_index())
        else {
            panic!("expected heap map");
        };
        (*all_immediate, aver_memory::map_all_immediate(map))
    }

    /// The collector trusts `all_immediate` without re-reading the table, so
    /// every owned producer that can preserve `true` proves it here instead.
    #[test]
    fn owned_map_producers_never_claim_immediate_for_a_heap_backed_table() {
        let mut arena = Arena::new();
        let one = NanValue::new_int(1, &mut arena);
        let two = NanValue::new_int(2, &mut arena);
        let three = NanValue::new_int(3, &mut arena);
        let four = NanValue::new_int(4, &mut arena);

        let first = set_nv_owned(&[NanValue::EMPTY_MAP, one, two], &mut arena, None)
            .expect("owned immediate insert succeeds");
        assert_eq!(map_flag_and_actual(&arena, first), (true, true));

        let second = set_nv_owned(&[first, three, four], &mut arena, None)
            .expect("second owned immediate insert succeeds");
        assert_eq!(map_flag_and_actual(&arena, second), (true, true));

        let removed = remove_nv_owned(&[second, one], &mut arena)
            .expect("owned removal keeps the other immediate pair");
        assert_eq!(map_flag_and_actual(&arena, removed), (true, true));

        let heap_value = NanValue::new_string_value("owned-map-heap-value", &mut arena);
        let mixed = set_nv_owned(&[removed, one, heap_value], &mut arena, None)
            .expect("owned heap insert succeeds");
        assert_eq!(map_flag_and_actual(&arena, mixed), (false, false));
    }

    #[test]
    fn owned_set_inherits_the_exact_pre_frame_receipt() {
        let mut arena = Arena::new();
        let (key, value) = heap_pair(&mut arena, "old");
        let proof = frame_proof(&arena);

        arena.push_string("post-mark-padding");
        let result = set_nv_owned(&[NanValue::EMPTY_MAP, key, value], &mut arena, Some(proof))
            .expect("owned insert with pre-frame values should succeed");
        assert_eq!(
            arena.map_scan_receipt_value(result),
            proof.lane_mark,
            "the fresh container slot must retain the age of its references",
        );
        let aver_memory::ArenaEntry::Map {
            pending_scan_keys, ..
        } = arena.get(result.arena_index())
        else {
            panic!("expected owned map result");
        };
        assert!(pending_scan_keys.is_empty());
    }

    #[test]
    fn owned_set_without_a_valid_frame_proof_stamps_current() {
        let mut arena = Arena::new();
        let (key, value) = heap_pair(&mut arena, "old");
        let valid = frame_proof(&arena);

        arena.push_string("post-mark-padding");
        let current_before_none = arena.lane_mark();
        let without_proof = set_nv_owned(&[NanValue::EMPTY_MAP, key, value], &mut arena, None)
            .expect("owned insert without a frame should remain valid");
        assert_eq!(
            arena.map_scan_receipt_value(without_proof),
            current_before_none,
            "a missing caller frame must not manufacture pre-frame provenance",
        );

        arena.push_string("more-post-mark-padding");
        let current_before_invalid = arena.lane_mark();
        let invalid_proof = OwnedMapFrameProof {
            lane_mark: 0,
            ..valid
        };
        let with_invalid_proof = set_nv_owned(
            &[NanValue::EMPTY_MAP, key, value],
            &mut arena,
            Some(invalid_proof),
        )
        .expect("owned insert with an invalid proof should remain valid");
        assert_eq!(
            arena.map_scan_receipt_value(with_invalid_proof),
            current_before_invalid,
            "the invalid lane sentinel must fail closed",
        );
    }

    #[test]
    fn owned_set_remembers_post_frame_values_and_refuses_invalid_source_receipts() {
        let mut arena = Arena::new();
        let proof = frame_proof(&arena);
        let (fresh_key, fresh_value) = heap_pair(&mut arena, "fresh");
        let fresh_hash = fresh_key.map_key_hash(&arena);
        let result = set_nv_owned(
            &[NanValue::EMPTY_MAP, fresh_key, fresh_value],
            &mut arena,
            Some(proof),
        )
        .expect("a fresh pair should use a remembered entry");
        assert_eq!(
            arena.map_scan_receipt_value(result),
            proof.lane_mark,
            "the old bulk remains covered by the frame receipt",
        );
        let aver_memory::ArenaEntry::Map {
            pending_scan_keys, ..
        } = arena.get(result.arena_index())
        else {
            panic!("expected owned map result");
        };
        assert_eq!(pending_scan_keys, &[fresh_hash]);

        let (old_key, old_value) = heap_pair(&mut arena, "invalid-source");
        let mut table = crate::nan_value::PersistentMap::new();
        table = table.insert_owned(old_key.map_key_hash(&arena), (old_key, old_value));
        let source = NanValue::new_map(arena.push(aver_memory::ArenaEntry::Map {
            map: table,
            all_immediate: false,
            scan_receipt: 0,
            pending_scan_keys: Vec::new(),
            holder_count: 0,
        }));
        let proof = frame_proof(&arena);
        assert_eq!(
            proof.receipt_for_owned_source(source, &arena),
            None,
            "an invalid source receipt cannot be upgraded by an owned insert",
        );
    }

    #[test]
    fn promotion_rewrites_only_the_remembered_map_pair() {
        let mut arena = Arena::new();
        let (old_key, old_value) = heap_pair(&mut arena, "old-bulk");
        let old_hash = old_key.map_key_hash(&arena);
        let mut table = crate::nan_value::PersistentMap::new();
        table = table.insert_owned(old_hash, (old_key, old_value));
        let source = NanValue::new_map(arena.push_map(table));
        let proof = frame_proof(&arena);

        let (fresh_key, fresh_value) = heap_pair(&mut arena, "fresh-exception");
        let fresh_hash = fresh_key.map_key_hash(&arena);
        let result = set_nv_owned(&[source, fresh_key, fresh_value], &mut arena, Some(proof))
            .expect("owned insert should remember its fresh pair");

        let mut roots = [result];
        arena.promote_young_roots_to_yard(proof.arena_mark, proof.lane_mark, &mut roots, false);

        assert_eq!(arena.map_entries_scanned(), 1);
        let map = arena.map_ref_value(roots[0]);
        assert_eq!(map.len(), 2);
        let (rewritten_key, rewritten_value) = map.get(&fresh_hash).expect("fresh pair survived");
        assert_eq!(
            arena.get_string_value(*rewritten_key),
            "proof-key-fresh-exception"
        );
        assert_eq!(
            arena.get_string_value(*rewritten_value),
            "proof-value-fresh-exception"
        );
        assert!(map.get(&old_hash).is_some(), "old bulk entry survived");
        let aver_memory::ArenaEntry::Map {
            pending_scan_keys, ..
        } = arena.get(roots[0].arena_index())
        else {
            panic!("expected promoted map");
        };
        assert!(pending_scan_keys.is_empty());
    }

    #[test]
    fn inplace_vector_escape_blocks_owned_map_receipt_inheritance() {
        let mut arena = Arena::new();
        let old_item = NanValue::new_string_value("old-vector-item", &mut arena);
        let _vector = NanValue::new_vector(arena.push_vector(vec![old_item]));
        let escaped = OwnedMapFrameProof {
            inplace_write_escaped: true,
            ..frame_proof(&arena)
        };
        arena.push_string("post-mark-padding");
        assert_eq!(
            escaped.receipt_for_owned_source(NanValue::EMPTY_MAP, &arena),
            None,
            "a nested mutable vector may contain a post-mark child after its in-place write",
        );
    }

    #[test]
    fn owned_remove_preserves_the_exact_source_receipt() {
        let mut arena = Arena::new();
        let (key_a, value_a) = heap_pair(&mut arena, "a");
        let (key_b, value_b) = heap_pair(&mut arena, "b");
        let mut table = crate::nan_value::PersistentMap::new();
        table = table.insert_owned(key_a.map_key_hash(&arena), (key_a, value_a));
        table = table.insert_owned(key_b.map_key_hash(&arena), (key_b, value_b));
        let source = NanValue::new_map(arena.push_map(table));
        let source_receipt = arena.map_scan_receipt_value(source);

        arena.push_string("newer-than-source");
        let result = remove_nv_owned(&[source, key_a], &mut arena)
            .expect("owned removal should preserve the remaining entry");
        assert_eq!(arena.map_ref_value(result).len(), 1);
        assert_eq!(
            arena.map_scan_receipt_value(result),
            source_receipt,
            "removing a reference must not make every retained reference look newer",
        );
    }

    #[test]
    fn owned_remove_discards_the_removed_remembered_pair() {
        let mut arena = Arena::new();
        let (old_key, old_value) = heap_pair(&mut arena, "kept");
        let mut table = crate::nan_value::PersistentMap::new();
        table = table.insert_owned(old_key.map_key_hash(&arena), (old_key, old_value));
        let source = NanValue::new_map(arena.push_map(table));
        let proof = frame_proof(&arena);
        let (fresh_key, fresh_value) = heap_pair(&mut arena, "removed");
        let with_fresh = set_nv_owned(&[source, fresh_key, fresh_value], &mut arena, Some(proof))
            .expect("owned insert should remember its fresh pair");

        let result = remove_nv_owned(&[with_fresh, fresh_key], &mut arena)
            .expect("owned removal should retain the old pair");
        let aver_memory::ArenaEntry::Map {
            pending_scan_keys, ..
        } = arena.get(result.arena_index())
        else {
            panic!("expected owned map result");
        };
        assert!(pending_scan_keys.is_empty());
    }

    /// Two different Int keys with one key hash, and two different String
    /// keys with one key hash — the keys `tests/fixtures/map_colliding_keys_app.av`
    /// runs on every backend.
    const INT_A: i64 = -1195071822729167888;
    const INT_B: i64 = 6146533188634962189;
    const TEXT_A: &str = "ka8971bb89fa1b124";
    const TEXT_B: &str = "k0c777abb5c4901e3";

    fn colliding_ints(arena: &mut Arena) -> (NanValue, NanValue) {
        (
            NanValue::new_int(INT_A, arena),
            NanValue::new_int(INT_B, arena),
        )
    }

    fn get(arena: &mut Arena, map: NanValue, key: NanValue) -> Option<i64> {
        let found = get_nv(&[map, key], arena).expect("Map.get succeeds");
        if found.is_none() {
            return None;
        }
        use crate::nan_value::NanValueConvert;
        match found.to_value(arena) {
            Value::Some(inner) => match *inner {
                Value::Int(n) => n.to_i64(),
                other => panic!("expected an Int value, got {other:?}"),
            },
            other => panic!("expected an Option, got {other:?}"),
        }
    }

    fn len(arena: &mut Arena, map: NanValue) -> usize {
        arena.map_ref_value(map).len()
    }

    /// The regression tests below and the cross-backend fixture mean nothing
    /// if these keys stop colliding, which a change to the key hash would do
    /// without failing anything else. Find a new pair then; do not delete this.
    #[test]
    fn the_fixture_keys_still_share_a_hash() {
        let mut arena = Arena::new();
        let (a, b) = colliding_ints(&mut arena);
        assert!(!a.eq_in(b, &arena));
        assert_eq!(a.map_key_hash_deep(&arena), b.map_key_hash_deep(&arena));

        let text_a = NanValue::new_string_value(TEXT_A, &mut arena);
        let text_b = NanValue::new_string_value(TEXT_B, &mut arena);
        assert!(!text_a.eq_in(text_b, &arena));
        assert_eq!(
            text_a.map_key_hash_deep(&arena),
            text_b.map_key_hash_deep(&arena)
        );
    }

    /// The bug: a map addressed by key hash alone answered for the wrong key.
    #[test]
    fn two_keys_with_one_hash_are_two_keys() {
        let mut arena = Arena::new();
        let (a, b) = colliding_ints(&mut arena);
        let one = NanValue::new_int(1, &mut arena);
        let two = NanValue::new_int(2, &mut arena);

        let only_a = set_nv(&[NanValue::EMPTY_MAP, a, one], &mut arena).unwrap();
        assert_eq!(get(&mut arena, only_a, b), None);
        assert!(!has_nv(&[only_a, b], &mut arena).unwrap().as_bool());

        let both = set_nv(&[only_a, b, two], &mut arena).unwrap();
        assert_eq!(len(&mut arena, both), 2);
        assert_eq!(get(&mut arena, both, a), Some(1));
        assert_eq!(get(&mut arena, both, b), Some(2));
        assert_eq!(len(&mut arena, only_a), 1, "the source map is untouched");

        // Setting a key that is already there replaces its own entry only.
        let three = NanValue::new_int(3, &mut arena);
        let replaced = set_nv(&[both, b, three], &mut arena).unwrap();
        assert_eq!(len(&mut arena, replaced), 2);
        assert_eq!(get(&mut arena, replaced, a), Some(1));
        assert_eq!(get(&mut arena, replaced, b), Some(3));

        let pairs = vec![
            NanValue::new_tuple(arena.push_tuple(vec![b, two])),
            NanValue::new_tuple(arena.push_tuple(vec![a, one])),
        ];
        let list = NanValue::new_list(arena.push_list(pairs));
        let listed = from_list_nv(&[list], &mut arena).unwrap();
        assert_eq!(len(&mut arena, listed), 2);
        assert_eq!(get(&mut arena, listed, a), Some(1));
        assert_eq!(get(&mut arena, listed, b), Some(2));
    }

    /// Removing the key that holds the shared slot must leave the other one
    /// where a lookup finds it, on the copying path and on the owned one.
    #[test]
    fn removing_one_of_two_keys_with_one_hash_keeps_the_other() {
        for owned in [false, true] {
            for remove_first in [false, true] {
                let mut arena = Arena::new();
                let (a, b) = colliding_ints(&mut arena);
                let one = NanValue::new_int(1, &mut arena);
                let two = NanValue::new_int(2, &mut arena);
                let only_a = set_nv_owned(&[NanValue::EMPTY_MAP, a, one], &mut arena, None)
                    .expect("owned insert succeeds");
                let both =
                    set_nv_owned(&[only_a, b, two], &mut arena, None).expect("second insert");
                assert_eq!(len(&mut arena, both), 2);

                let (gone, kept, kept_value) = if remove_first { (a, b, 2) } else { (b, a, 1) };
                let rest = if owned {
                    remove_nv_owned(&[both, gone], &mut arena)
                } else {
                    remove_nv(&[both, gone], &mut arena)
                }
                .expect("removal succeeds");
                let case = format!("owned={owned} remove_first={remove_first}");
                assert_eq!(len(&mut arena, rest), 1, "{case}");
                assert_eq!(get(&mut arena, rest, gone), None, "{case}");
                assert_eq!(get(&mut arena, rest, kept), Some(kept_value), "{case}");

                // The kept key is back in the slot its own hash names, so a
                // later insert of the removed key cannot shadow it.
                let hash = kept.map_key_hash_deep(&arena);
                assert!(arena.map_ref_value(rest).get(&hash).is_some(), "{case}");
                let nine = NanValue::new_int(9, &mut arena);
                let again = set_nv(&[rest, gone, nine], &mut arena).unwrap();
                assert_eq!(len(&mut arena, again), 2, "{case}");
                assert_eq!(get(&mut arena, again, gone), Some(9), "{case}");
                assert_eq!(get(&mut arena, again, kept), Some(kept_value), "{case}");
            }
        }
    }

    /// Removing a key the map does not hold changes nothing, even when a
    /// different key with the same hash is there.
    #[test]
    fn removing_an_absent_key_with_a_taken_hash_removes_nothing() {
        let mut arena = Arena::new();
        let (a, b) = colliding_ints(&mut arena);
        let one = NanValue::new_int(1, &mut arena);
        let only_a = set_nv(&[NanValue::EMPTY_MAP, a, one], &mut arena).unwrap();

        let copied = remove_nv(&[only_a, b], &mut arena).unwrap();
        assert_eq!(get(&mut arena, copied, a), Some(1));
        let owned = remove_nv_owned(&[copied, b], &mut arena).unwrap();
        assert_eq!(len(&mut arena, owned), 1);
        assert_eq!(get(&mut arena, owned, a), Some(1));
    }

    /// Map equality pairs entries by key. The two maps below hold the same two
    /// entries in opposite slots, and the two one-key maps hold different keys
    /// in the same slot.
    #[test]
    fn map_equality_compares_keys_not_slots() {
        let mut arena = Arena::new();
        let (a, b) = colliding_ints(&mut arena);
        let one = NanValue::new_int(1, &mut arena);
        let two = NanValue::new_int(2, &mut arena);

        let only_a = set_nv(&[NanValue::EMPTY_MAP, a, one], &mut arena).unwrap();
        let only_b = set_nv(&[NanValue::EMPTY_MAP, b, one], &mut arena).unwrap();
        assert!(!only_a.eq_in(only_b, &arena));

        let a_then_b = set_nv(&[only_a, b, two], &mut arena).unwrap();
        let b_first = set_nv(&[NanValue::EMPTY_MAP, b, two], &mut arena).unwrap();
        let b_then_a = set_nv(&[b_first, a, one], &mut arena).unwrap();
        assert!(a_then_b.eq_in(b_then_a, &arena));
        assert!(b_then_a.eq_in(a_then_b, &arena));

        let swapped = set_nv(&[only_b, a, two], &mut arena).unwrap();
        assert!(!a_then_b.eq_in(swapped, &arena), "same keys, other values");
    }

    /// The collector's remembered set names a pair by its slot. A pair that
    /// was remembered and then moved up by a removal has to be remembered
    /// under the slot it moved to, or its rewrite is silently skipped.
    #[test]
    fn owned_remove_renames_a_remembered_pair_it_moves() {
        let mut arena = Arena::new();
        let (a, b) = colliding_ints(&mut arena);
        let one = NanValue::new_int(1, &mut arena);
        let source = set_nv(&[NanValue::EMPTY_MAP, a, one], &mut arena).unwrap();
        let proof = frame_proof(&arena);
        let fresh_value = NanValue::new_string_value("remembered-colliding-value", &mut arena);
        let both = set_nv_owned(&[source, b, fresh_value], &mut arena, Some(proof))
            .expect("owned insert should remember its fresh pair");
        let home = a.map_key_hash_deep(&arena);
        let aver_memory::ArenaEntry::Map {
            pending_scan_keys, ..
        } = arena.get(both.arena_index())
        else {
            panic!("expected owned map result");
        };
        assert_eq!(
            pending_scan_keys,
            &[home.wrapping_add(1)],
            "the second key with this hash lives one slot on"
        );

        let rest = remove_nv_owned(&[both, a], &mut arena).expect("owned removal succeeds");
        let aver_memory::ArenaEntry::Map {
            map,
            pending_scan_keys,
            ..
        } = arena.get(rest.arena_index())
        else {
            panic!("expected owned map result");
        };
        assert_eq!(pending_scan_keys, &[home]);
        assert!(map.get(&home).is_some_and(|(key, _)| key.eq_in(b, &arena)));
    }

    // ─── Runs of more than two keys ──────────────────────────────────────
    //
    // A third key with the hash of the two above cannot be computed, so these
    // tests say where the search for a key starts instead of deriving it from
    // the key. Everything after that — the search, the insert, the removal and
    // the remembered set — is the code a program runs.

    thread_local! {
        static PLACED: std::cell::RefCell<Vec<(u64, u64)>> =
            const { std::cell::RefCell::new(Vec::new()) };
    }

    /// The slot a test gave `key`, if the running test placed it.
    pub(super) fn placed(key: NanValue) -> Option<u64> {
        PLACED.with(|placed| {
            placed
                .borrow()
                .iter()
                .find(|(bits, _)| *bits == key.bits())
                .map(|(_, home)| *home)
        })
    }

    /// Forgets the placements when the test ends, so a test harness that
    /// reuses the thread does not carry them into the next test.
    struct Placement;

    impl Drop for Placement {
        fn drop(&mut self) {
            PLACED.with(|placed| placed.borrow_mut().clear());
        }
    }

    /// Small Int keys `1..=homes.len()`, key `i` searched for from `homes[i - 1]`.
    fn place(arena: &mut Arena, homes: &[u64]) -> (Placement, Vec<NanValue>) {
        let keys: Vec<NanValue> = (1..=homes.len() as i64)
            .map(|n| NanValue::new_int(n, arena))
            .collect();
        PLACED.with(|placed| {
            *placed.borrow_mut() = keys
                .iter()
                .zip(homes)
                .map(|(key, home)| {
                    assert!(key.is_immediate(), "a placed key is named by its bits");
                    (key.bits(), *home)
                })
                .collect();
        });
        (Placement, keys)
    }

    /// Key `i` set to value `i * 10`, in the order given, on the copying path
    /// or the owned one.
    fn build(arena: &mut Arena, owned: bool, keys: &[NanValue]) -> NanValue {
        let mut map = NanValue::EMPTY_MAP;
        for (index, key) in keys.iter().enumerate() {
            let value = NanValue::new_int((index as i64 + 1) * 10, arena);
            map = if owned {
                set_nv_owned(&[map, *key, value], arena, None)
            } else {
                set_nv(&[map, *key, value], arena)
            }
            .expect("insert succeeds");
        }
        map
    }

    fn remove(arena: &mut Arena, owned: bool, map: NanValue, key: NanValue) -> NanValue {
        if owned {
            remove_nv_owned(&[map, key], arena)
        } else {
            remove_nv(&[map, key], arena)
        }
        .expect("removal succeeds")
    }

    /// The slot each of `keys` sits in, `None` for a key the table lacks. Read
    /// off the table itself, not through the search under test.
    fn slots(arena: &Arena, map: NanValue, keys: &[NanValue]) -> Vec<Option<u64>> {
        let table = arena.map_ref_value(map);
        keys.iter()
            .map(|key| {
                table
                    .iter()
                    .find(|(_, (stored, _))| stored.eq_in(*key, arena))
                    .map(|(slot, _)| *slot)
            })
            .collect()
    }

    /// What `Map.get` and `Map.has` answer for each of `keys`.
    fn answers(arena: &mut Arena, map: NanValue, keys: &[NanValue]) -> Vec<Option<i64>> {
        keys.iter()
            .map(|key| {
                let found = get(arena, map, *key);
                let has = has_nv(&[map, *key], arena).unwrap().as_bool();
                assert_eq!(has, found.is_some(), "Map.has and Map.get disagree");
                found
            })
            .collect()
    }

    /// One removal from a run of keys: where every key sits before and after,
    /// and that each key left is still found with its own value.
    fn check_removal(
        homes: &[u64],
        before: &[u64],
        gone: usize,
        after: &[Option<u64>],
        what: &str,
    ) {
        for owned in [false, true] {
            let case = format!("{what}, owned={owned}");
            let mut arena = Arena::new();
            let (_placement, keys) = place(&mut arena, homes);
            let map = build(&mut arena, owned, &keys);
            let expected_before: Vec<Option<u64>> = before.iter().map(|s| Some(*s)).collect();
            assert_eq!(slots(&arena, map, &keys), expected_before, "{case}: built");
            let all: Vec<Option<i64>> = (1..=keys.len() as i64).map(|n| Some(n * 10)).collect();
            assert_eq!(answers(&mut arena, map, &keys), all, "{case}: built");

            let rest = remove(&mut arena, owned, map, keys[gone]);
            assert_eq!(slots(&arena, rest, &keys), after, "{case}: removed");
            let mut left = all.clone();
            left[gone] = None;
            assert_eq!(answers(&mut arena, rest, &keys), left, "{case}: removed");
            assert_eq!(len(&mut arena, rest), keys.len() - 1, "{case}");
            if !owned {
                assert_eq!(
                    answers(&mut arena, map, &keys),
                    all,
                    "{case}: the source map"
                );
            }

            // The removed key goes back in at the end of the run, and setting
            // a key that stayed replaces its entry where it is.
            let back = NanValue::new_int(7, &mut arena);
            let again = set_nv(&[rest, keys[gone], back], &mut arena).unwrap();
            let mut restored = left.clone();
            restored[gone] = Some(7);
            assert_eq!(answers(&mut arena, again, &keys), restored, "{case}: back");
            assert_eq!(len(&mut arena, again), keys.len(), "{case}: back");
            for (index, key) in keys.iter().enumerate() {
                if index == gone {
                    continue;
                }
                let other = NanValue::new_int(-1, &mut arena);
                let replaced = set_nv(&[again, *key, other], &mut arena).unwrap();
                assert_eq!(len(&mut arena, replaced), keys.len(), "{case}: replace");
                assert_eq!(get(&mut arena, replaced, *key), Some(-1), "{case}: replace");
            }
        }
    }

    #[test]
    fn three_keys_with_one_home_survive_the_removal_of_any_one() {
        let h = 1_000;
        let homes = [h, h, h];
        let before = [h, h + 1, h + 2];
        check_removal(
            &homes,
            &before,
            0,
            &[None, Some(h), Some(h + 1)],
            "the first of three",
        );
        check_removal(
            &homes,
            &before,
            1,
            &[Some(h), None, Some(h + 1)],
            "the middle of three",
        );
        check_removal(
            &homes,
            &before,
            2,
            &[Some(h), Some(h + 1), None],
            "the last of three",
        );
    }

    /// Key 3 sits in the slot its own hash names, in the middle of a run of
    /// keys that started one slot earlier. Moving it up would put it before
    /// the slot its search starts at, where no lookup would find it; the
    /// removal has to step over it and move the key behind it instead.
    #[test]
    fn a_removal_steps_over_a_key_in_its_own_slot() {
        let h = 5_000;
        let homes = [h, h, h + 2, h];
        let before = [h, h + 1, h + 2, h + 3];
        check_removal(
            &homes,
            &before,
            0,
            &[None, Some(h), Some(h + 2), Some(h + 1)],
            "the first, with a key at home behind it",
        );
        check_removal(
            &homes,
            &before,
            1,
            &[Some(h), None, Some(h + 2), Some(h + 1)],
            "the second, with a key at home behind it",
        );
        check_removal(
            &homes,
            &before,
            2,
            &[Some(h), Some(h + 1), None, Some(h + 2)],
            "the key at home",
        );
        check_removal(
            &homes,
            &before,
            3,
            &[Some(h), Some(h + 1), Some(h + 2), None],
            "the last",
        );
    }

    /// A run that starts at the last slot continues at slot 0.
    #[test]
    fn a_run_of_keys_wraps_past_the_last_slot() {
        let last = u64::MAX;
        let homes = [last, last, last];
        let before = [last, 0, 1];
        check_removal(
            &homes,
            &before,
            0,
            &[None, Some(last), Some(0)],
            "the first of a wrapped run",
        );
        check_removal(
            &homes,
            &before,
            1,
            &[Some(last), None, Some(0)],
            "the middle of a wrapped run",
        );
        check_removal(
            &homes,
            &before,
            2,
            &[Some(last), Some(0), None],
            "the last of a wrapped run",
        );

        // A key at home in slot 0 stays there when the run it interrupts,
        // which began at the last slot, closes up around it.
        let homes = [last, 0, last];
        let before = [last, 0, 1];
        check_removal(
            &homes,
            &before,
            0,
            &[None, Some(0), Some(last)],
            "the first, with a key at home in slot 0",
        );
    }

    /// One owned removal that moves two remembered pairs: both are renamed,
    /// and the collector then rewrites exactly those two.
    #[test]
    fn owned_remove_renames_every_remembered_pair_it_moves() {
        let h = 9_000;
        let mut arena = Arena::new();
        let (_placement, keys) = place(&mut arena, &[h, h, h]);
        let ten = NanValue::new_int(10, &mut arena);
        let source = set_nv(&[NanValue::EMPTY_MAP, keys[0], ten], &mut arena).unwrap();
        let proof = frame_proof(&arena);
        let second = NanValue::new_string_value("remembered-second-value", &mut arena);
        let third = NanValue::new_string_value("remembered-third-value", &mut arena);
        let two = set_nv_owned(&[source, keys[1], second], &mut arena, Some(proof))
            .expect("owned insert should remember its fresh pair");
        let three = set_nv_owned(&[two, keys[2], third], &mut arena, Some(proof))
            .expect("owned insert should remember its fresh pair");
        let pending = |arena: &Arena, map: NanValue| -> Vec<u64> {
            let aver_memory::ArenaEntry::Map {
                pending_scan_keys, ..
            } = arena.get(map.arena_index())
            else {
                panic!("expected a heap map");
            };
            pending_scan_keys.clone()
        };
        assert_eq!(pending(&arena, three), vec![h + 1, h + 2]);

        let rest = remove_nv_owned(&[three, keys[0]], &mut arena).expect("owned removal");
        assert_eq!(pending(&arena, rest), vec![h, h + 1]);
        assert_eq!(
            slots(&arena, rest, &keys),
            vec![None, Some(h), Some(h + 1)],
            "both pairs moved up one slot"
        );

        let mut roots = [rest];
        arena.promote_young_roots_to_yard(proof.arena_mark, proof.lane_mark, &mut roots, false);
        assert_eq!(arena.map_entries_scanned(), 2);
        let table = arena.map_ref_value(roots[0]);
        let (_, kept_second) = table.get(&h).expect("the second pair survived");
        let (_, kept_third) = table.get(&(h + 1)).expect("the third pair survived");
        assert_eq!(
            arena.get_string_value(*kept_second),
            "remembered-second-value"
        );
        assert_eq!(
            arena.get_string_value(*kept_third),
            "remembered-third-value"
        );
        assert!(pending(&arena, roots[0]).is_empty());
    }

    /// Every float a program can put in a map key position, including the
    /// three the ordering used to get wrong.
    fn float_key_corpus() -> Vec<Value> {
        [
            f64::NAN,
            -f64::NAN,
            0.0,
            -0.0,
            1.0,
            -1.0,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::MIN_POSITIVE,
            -f64::MIN_POSITIVE,
            f64::MAX,
            f64::MIN,
        ]
        .into_iter()
        .map(Value::Float)
        .collect()
    }

    /// `compare_scalar_keys` is what `sort_by` is handed, and `sort_by`
    /// aborts the process when what it is handed is not a total order.
    /// Check the three laws directly rather than hoping a sort of the right
    /// length trips the standard library's own detector: it only samples.
    #[test]
    fn float_key_ordering_is_a_total_order() {
        let corpus = float_key_corpus();
        for a in &corpus {
            assert_eq!(
                compare_scalar_keys(a, a),
                Ordering::Equal,
                "a key must compare equal to itself, {:?} did not",
                a
            );
            for b in &corpus {
                assert_eq!(
                    compare_scalar_keys(a, b),
                    compare_scalar_keys(b, a).reverse(),
                    "comparing {:?} against {:?} must reverse when the arguments swap",
                    a,
                    b
                );
                for c in &corpus {
                    let ab = compare_scalar_keys(a, b);
                    let bc = compare_scalar_keys(b, c);
                    if ab == bc && ab != Ordering::Equal {
                        assert_eq!(
                            compare_scalar_keys(a, c),
                            ab,
                            "{:?} {:?} {:?} and {:?} {:?} {:?} must give {:?} {:?} {:?}",
                            a,
                            ab,
                            b,
                            b,
                            bc,
                            c,
                            a,
                            ab,
                            c
                        );
                    }
                }
            }
        }
    }

    /// The tree-walk representation's sort completes on a float map holding a
    /// NaN, and lands on one answer rather than one per input permutation.
    ///
    /// This is the shape that took the process down: a NaN sorts above `1.0`
    /// and below `-1.0` under a raw bit-pattern fallback, so `sort_by` found
    /// the cycle and panicked with "user-provided comparison function does not
    /// correctly implement a total order". Sixty-one keys, because the
    /// standard library only runs that check on inputs past its insertion-sort
    /// threshold.
    #[test]
    fn a_float_map_holding_nan_sorts_to_one_stable_order() {
        let mut keys: Vec<Value> = (0..58).map(|i| Value::Float(f64::from(i) - 29.0)).collect();
        // Both NaN signs. IEEE 754 does not fix the sign of a NaN produced by
        // an invalid operation, so `0.0 / 0.0` comes out negative on some
        // machines and positive on others — and under a total order that sign
        // decides whether the NaN sorts below every finite key or above every
        // one. Carry both, so what is tested here does not depend on which one
        // the hardware happened to produce.
        keys.push(Value::Float(f64::NAN));
        keys.push(Value::Float(-f64::NAN));
        keys.push(Value::Float(-0.0));
        keys.push(Value::Float(0.0));
        assert_eq!(keys.len(), 62);

        let sort = |mut input: Vec<Value>| {
            input.sort_by(compare_scalar_keys);
            input
        };
        let expected = sort(keys.clone());

        // Same keys, three different arrival orders, one answer.
        let mut reversed = keys.clone();
        reversed.reverse();
        let mut rotated = keys.clone();
        rotated.rotate_left(17);
        for (label, permutation) in [
            ("reversed", reversed),
            ("rotated", rotated),
            ("already sorted", expected.clone()),
        ] {
            let got = sort(permutation);
            let render = |vs: &[Value]| -> Vec<String> {
                vs.iter()
                    .map(|v| match v {
                        Value::Float(f) => format!("{:016x}", f.to_bits()),
                        other => format!("{:?}", other),
                    })
                    .collect()
            };
            assert_eq!(
                render(&got),
                render(&expected),
                "the {} arrival order must sort to the same sequence",
                label
            );
        }

        // A map folds `-0.0` and `0.0` into one key, so the sort is never
        // asked to break that tie. Pin the answer anyway — whichever of the
        // two survived insertion, the order it lands in is decided by the
        // value and not by which one arrived first.
        assert_eq!(
            compare_scalar_keys(&Value::Float(-0.0), &Value::Float(0.0)),
            Ordering::Less,
            "-0.0 must order below 0.0 deterministically"
        );
    }
}
