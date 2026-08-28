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

use crate::nan_value::{Arena, LaneMark, NanValue};
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
fn compare_keys(a: &Value, b: &Value) -> Ordering {
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

fn nv_key_bits(v: NanValue, arena: &Arena) -> u64 {
    v.map_key_hash_deep(arena)
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
    let key_hash = nv_key_bits(args[1], arena);
    let TakenMap {
        map: old_map,
        scan_receipt: _,
        mut pending_scan_keys,
    } = arena.take_map_value(source);
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
    let key_hash = nv_key_bits(args[1], arena);
    let TakenMap {
        map: old_map,
        scan_receipt,
        mut pending_scan_keys,
    } = arena.take_map_value(source);
    let new_map = old_map.remove_owned(&key_hash);
    pending_scan_keys.retain(|pending| *pending != key_hash);
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
    let key_hash = nv_key_bits(args[1], arena);
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
    let key_hash = nv_key_bits(args[1], arena);
    let map = arena.map_ref_value(args[0]);
    match map.get(&key_hash) {
        Some((_, v)) => Ok(NanValue::new_some_value(*v, arena)),
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
    arena.note_map_entries_copied(old_map.len());
    let key_hash = nv_key_bits(args[1], arena);
    let new_map = old_map.remove(&key_hash);
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
    let key_hash = nv_key_bits(args[1], arena);
    let map = arena.map_ref_value(args[0]);
    Ok(NanValue::new_bool(map.contains_key(&key_hash)))
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
    let mut out = crate::nan_value::PersistentMap::new();
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
        let key_hash = nv_key_bits(key, arena);
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
