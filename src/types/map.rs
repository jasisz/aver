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
use std::collections::HashMap;
use std::sync::Arc as Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value, aver_repr, list_from_vec, list_view};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "set", "get", "remove", "has", "keys", "values", "entries", "len", "fromList",
    ] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Map.{}", method)),
        );
    }
    global.insert(
        "Map".to_string(),
        Value::Namespace {
            name: "Map".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

/// Returns `Some(result)` when `name` is owned by this namespace, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Map.set" => Some(set(args)),
        "Map.get" => Some(get(args)),
        "Map.remove" => Some(remove(args)),
        "Map.has" => Some(has(args)),
        "Map.keys" => Some(keys(args)),
        "Map.values" => Some(values(args)),
        "Map.entries" => Some(entries(args)),
        "Map.len" => Some(len(args)),
        "Map.fromList" => Some(from_list(args)),
        _ => None,
    }
}

fn set(args: &[Value]) -> Result<Value, RuntimeError> {
    let [map_val, key, value] = three_args("Map.set", args)?;
    let Value::Map(map) = map_val else {
        return Err(RuntimeError::Error(
            "Map.set() first argument must be a Map".to_string(),
        ));
    };
    ensure_hashable_key("Map.set", key)?;
    let mut out = map.clone();
    out.insert(key.clone(), value.clone());
    Ok(Value::Map(out))
}

fn get(args: &[Value]) -> Result<Value, RuntimeError> {
    let [map_val, key] = two_args("Map.get", args)?;
    let Value::Map(map) = map_val else {
        return Err(RuntimeError::Error(
            "Map.get() first argument must be a Map".to_string(),
        ));
    };
    ensure_hashable_key("Map.get", key)?;
    Ok(match map.get(key) {
        Some(v) => Value::Some(Box::new(v.clone())),
        None => Value::None,
    })
}

fn remove(args: &[Value]) -> Result<Value, RuntimeError> {
    let [map_val, key] = two_args("Map.remove", args)?;
    let Value::Map(map) = map_val else {
        return Err(RuntimeError::Error(
            "Map.remove() first argument must be a Map".to_string(),
        ));
    };
    ensure_hashable_key("Map.remove", key)?;
    let mut out = map.clone();
    out.remove(key);
    Ok(Value::Map(out))
}

fn has(args: &[Value]) -> Result<Value, RuntimeError> {
    let [map_val, key] = two_args("Map.has", args)?;
    let Value::Map(map) = map_val else {
        return Err(RuntimeError::Error(
            "Map.has() first argument must be a Map".to_string(),
        ));
    };
    ensure_hashable_key("Map.has", key)?;
    Ok(Value::Bool(map.contains_key(key)))
}

fn keys(args: &[Value]) -> Result<Value, RuntimeError> {
    let [map_val] = one_arg("Map.keys", args)?;
    let Value::Map(map) = map_val else {
        return Err(RuntimeError::Error(
            "Map.keys() argument must be a Map".to_string(),
        ));
    };
    let mut out = map.keys().cloned().collect::<Vec<_>>();
    out.sort_by(compare_scalar_keys);
    Ok(list_from_vec(out))
}

fn values(args: &[Value]) -> Result<Value, RuntimeError> {
    let [map_val] = one_arg("Map.values", args)?;
    let Value::Map(map) = map_val else {
        return Err(RuntimeError::Error(
            "Map.values() argument must be a Map".to_string(),
        ));
    };
    let mut entries = map.iter().collect::<Vec<_>>();
    entries.sort_by(|(k1, _), (k2, _)| compare_scalar_keys(k1, k2));
    let out = entries
        .into_iter()
        .map(|(_, v)| v.clone())
        .collect::<Vec<_>>();
    Ok(list_from_vec(out))
}

fn entries(args: &[Value]) -> Result<Value, RuntimeError> {
    let [map_val] = one_arg("Map.entries", args)?;
    let Value::Map(map) = map_val else {
        return Err(RuntimeError::Error(
            "Map.entries() argument must be a Map".to_string(),
        ));
    };
    let mut entries = map.iter().collect::<Vec<_>>();
    entries.sort_by(|(k1, _), (k2, _)| compare_scalar_keys(k1, k2));
    let out = entries
        .into_iter()
        .map(|(k, v)| Value::Tuple(vec![k.clone(), v.clone()]))
        .collect::<Vec<_>>();
    Ok(list_from_vec(out))
}

fn len(args: &[Value]) -> Result<Value, RuntimeError> {
    let [map_val] = one_arg("Map.len", args)?;
    let Value::Map(map) = map_val else {
        return Err(RuntimeError::Error(
            "Map.len() argument must be a Map".to_string(),
        ));
    };
    Ok(Value::int(map.len() as i64))
}

fn from_list(args: &[Value]) -> Result<Value, RuntimeError> {
    let [pairs] = one_arg("Map.fromList", args)?;
    let items = list_view(pairs).ok_or_else(|| {
        RuntimeError::Error(
            "Map.fromList() argument must be a List of (key, value) tuples".to_string(),
        )
    })?;

    let mut out = HashMap::new();
    for (idx, pair) in items.iter().enumerate() {
        let Value::Tuple(parts) = pair else {
            return Err(RuntimeError::Error(format!(
                "Map.fromList() item {} must be (key, value)",
                idx + 1
            )));
        };
        if parts.len() != 2 {
            return Err(RuntimeError::Error(format!(
                "Map.fromList() item {} must have 2 elements",
                idx + 1
            )));
        }

        let key = &parts[0];
        let value = &parts[1];
        ensure_hashable_key("Map.fromList", key)?;
        out.insert(key.clone(), value.clone());
    }
    Ok(Value::Map(out))
}

fn is_hashable_key(value: &Value) -> bool {
    !matches!(value, Value::Fn(_))
}

fn ensure_hashable_key(name: &str, value: &Value) -> Result<(), RuntimeError> {
    if is_hashable_key(value) {
        Ok(())
    } else {
        Err(RuntimeError::Error(format!(
            "{}: key must be hashable (functions are not)",
            name
        )))
    }
}

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
    keyed.sort_by(|(a, _), (b, _)| compare_scalar_keys(a, b));
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
    keyed.sort_by(|(a, _), (b, _)| compare_scalar_keys(a, b));
    for (slot, (_, pair)) in entries.iter_mut().zip(keyed) {
        *slot = pair;
    }
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

fn one_arg<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 1], RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument, got {}",
            name,
            args.len()
        )));
    }
    Ok([&args[0]])
}

fn two_args<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 2], RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 2 arguments, got {}",
            name,
            args.len()
        )));
    }
    Ok([&args[0], &args[1]])
}

fn three_args<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 3], RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 3 arguments, got {}",
            name,
            args.len()
        )));
    }
    Ok([&args[0], &args[1], &args[2]])
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &[
        "empty", "set", "get", "remove", "has", "keys", "values", "entries", "len", "fromList",
    ];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Map.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Map"),
        members,
    });
    global.insert("Map".to_string(), NanValue::new_namespace(ns_idx));
}

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

/// Map.set with sole-owned first argument — takes instead of cloning.
pub fn set_nv_owned(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
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
    let old_map = arena.take_map_value(source);
    let key_hash = nv_key_bits(args[1], arena);
    let new_map = old_map.insert_owned(key_hash, (args[1], args[2]));
    let map_idx = arena.push_inheriting_source_space(
        aver_memory::ArenaEntry::Map {
            map: new_map,
            all_immediate,
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
