/// Map namespace — immutable key/value map helpers.
///
/// Methods:
///   Map.empty()                 → Map<K, V>
///   Map.set(map, key, value)    → Map<K, V>
///   Map.get(map, key)           → Option<V>
///   Map.remove(map, key)        → Map<K, V>
///   Map.has(map, key)           → Bool
///   Map.keys(map)               → List<K>
///   Map.values(map)             → List<V>
///   Map.entries(map)            → List<(K, V)>
///   Map.len(map)                → Int
///   Map.fromList(pairs)         → Map<K, V> where each pair is (key, value)
///
/// Key constraint: only scalar keys are allowed (Int, Float, String, Bool).
///
/// No effects required.
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value, aver_repr, list_from_vec, list_view};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "empty", "set", "get", "remove", "has", "keys", "values", "entries", "len", "fromList",
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
        "Map.empty" => Some(empty(args)),
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

fn empty(args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Map.empty() takes 0 arguments, got {}",
            args.len()
        )));
    }
    Ok(Value::Map(HashMap::new()))
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
    Ok(Value::Int(map.len() as i64))
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
    matches!(
        value,
        Value::Int(_) | Value::Float(_) | Value::Str(_) | Value::Bool(_)
    )
}

fn ensure_hashable_key(name: &str, value: &Value) -> Result<(), RuntimeError> {
    if is_hashable_key(value) {
        Ok(())
    } else {
        Err(RuntimeError::Error(format!(
            "{}: key must be Int, Float, String, or Bool",
            name
        )))
    }
}

fn compare_scalar_keys(a: &Value, b: &Value) -> Ordering {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => x.cmp(y),
        (Value::Float(x), Value::Float(y)) => x.partial_cmp(y).unwrap_or_else(|| {
            let xb = x.to_bits();
            let yb = y.to_bits();
            xb.cmp(&yb)
        }),
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
        "Map.empty" => Some(empty_nv(args, arena)),
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
    v.is_int() || v.is_float() || v.is_string() || v.is_bool()
}

fn ensure_hashable_nv(name: &str, v: NanValue) -> Result<(), RuntimeError> {
    if is_hashable_nv(v) {
        Ok(())
    } else {
        Err(RuntimeError::Error(format!(
            "{}: key must be Int, Float, String, or Bool",
            name
        )))
    }
}

fn nv_key_bits(v: NanValue, arena: &Arena) -> u64 {
    v.map_key_hash(arena)
}

fn empty_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Map.empty() takes 0 arguments, got {}",
            args.len()
        )));
    }
    let map_idx = arena.push_map(HashMap::new());
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
    let old_map = arena.get_map(args[0].arena_index()).clone();
    let mut new_map = old_map;
    let key_hash = nv_key_bits(args[1], arena);
    new_map.insert(key_hash, (args[1], args[2]));
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
    let map = arena.get_map(args[0].arena_index());
    match map.get(&key_hash) {
        Some((_, v)) => {
            let box_idx = arena.push_boxed(*v);
            Ok(NanValue::new_some(box_idx))
        }
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
    let old_map = arena.get_map(args[0].arena_index()).clone();
    let mut new_map = old_map;
    let key_hash = nv_key_bits(args[1], arena);
    new_map.remove(&key_hash);
    let map_idx = arena.push_map(new_map);
    Ok(NanValue::new_map(map_idx))
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
    let map = arena.get_map(args[0].arena_index());
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
    let map = arena.get_map(args[0].arena_index()).clone();
    let mut keys: Vec<NanValue> = map.values().map(|(k, _)| *k).collect();
    keys.sort_by_key(|a| a.repr(arena));
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
    let map = arena.get_map(args[0].arena_index()).clone();
    let mut entries: Vec<(NanValue, NanValue)> = map.values().cloned().collect();
    entries.sort_by(|(a, _), (b, _)| a.repr(arena).cmp(&b.repr(arena)));
    let vals: Vec<NanValue> = entries.into_iter().map(|(_, v)| v).collect();
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
    let map = arena.get_map(args[0].arena_index()).clone();
    let mut entries: Vec<(NanValue, NanValue)> = map.values().cloned().collect();
    entries.sort_by(|(a, _), (b, _)| a.repr(arena).cmp(&b.repr(arena)));
    let pairs: Vec<NanValue> = entries
        .into_iter()
        .map(|(k, v)| {
            let tuple_idx = arena.push_tuple(vec![k, v]);
            NanValue::new_tuple(tuple_idx)
        })
        .collect();
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
    let map = arena.get_map(args[0].arena_index());
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
    let items = arena.get_list(args[0].arena_index()).to_vec();
    let mut out: HashMap<u64, (NanValue, NanValue)> = HashMap::new();
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
        out.insert(key_hash, (key, value));
    }
    let map_idx = arena.push_map(out);
    Ok(NanValue::new_map(map_idx))
}
