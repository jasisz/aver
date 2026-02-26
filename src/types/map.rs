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

use crate::value::{aver_repr, list_from_vec, list_slice, RuntimeError, Value};

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
    let items = list_slice(pairs).ok_or_else(|| {
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
