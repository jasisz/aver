/// List namespace — list manipulation helpers.
///
/// Methods:
///   List.len(list)           → Int                    — number of elements
///   List.get(list, index)    → Option<T>              — element at index
///   List.push(list, val)     → List<T>                — append element (returns new list)
///   List.head(list)          → Option<T>              — first element
///   List.tail(list)          → Option<List<T>>        — all but first
///   List.zip(a, b)           → List<(A, B)>           — pair elements from two lists
///
/// Note: List.map, List.filter, List.fold, List.find, List.any, List.flatMap are handled
/// directly in the interpreter because they need to invoke closures via
/// `self.call_value`.
///
/// No effects required.
use std::collections::HashMap;

use crate::value::{
    list_from_vec, list_head, list_len, list_slice, list_tail_view, list_to_vec, RuntimeError,
    Value,
};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "len", "map", "filter", "fold", "get", "push", "head", "tail", "find", "any", "zip",
        "flatMap",
    ] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("List.{}", method)),
        );
    }
    global.insert(
        "List".to_string(),
        Value::Namespace {
            name: "List".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

/// Returns `Some(result)` when `name` is owned by this namespace, `None` otherwise.
/// Note: List.map, List.filter, List.fold are NOT handled here (they need interpreter access).
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "List.len" => Some(len(&args)),
        "List.get" => Some(get(&args)),
        "List.push" => Some(push(&args)),
        "List.head" => Some(head(&args)),
        "List.tail" => Some(tail(&args)),
        "List.zip" => Some(zip(&args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn len(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    list_len(&args[0])
        .map(|n| Value::Int(n as i64))
        .ok_or_else(|| RuntimeError::Error("List.len() argument must be a List".to_string()))
}

fn get(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.get() takes 2 arguments (list, index), got {}",
            args.len()
        )));
    }
    let list = list_slice(&args[0]).ok_or_else(|| {
        RuntimeError::Error("List.get() first argument must be a List".to_string())
    })?;
    let index = match &args[1] {
        Value::Int(i) => *i,
        _ => {
            return Err(RuntimeError::Error(
                "List.get() index must be an Int".to_string(),
            ))
        }
    };
    if index < 0 || index as usize >= list.len() {
        Ok(Value::None)
    } else {
        Ok(Value::Some(Box::new(list[index as usize].clone())))
    }
}

fn push(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.push() takes 2 arguments (list, val), got {}",
            args.len()
        )));
    }
    let mut list = list_to_vec(&args[0]).ok_or_else(|| {
        RuntimeError::Error("List.push() first argument must be a List".to_string())
    })?;
    list.push(args[1].clone());
    Ok(list_from_vec(list))
}

fn head(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.head() takes 1 argument, got {}",
            args.len()
        )));
    }
    match list_head(&args[0]) {
        Some(v) => Ok(Value::Some(Box::new(v))),
        None if list_len(&args[0]).is_some() => Ok(Value::None),
        None => Err(RuntimeError::Error(
            "List.head() argument must be a List".to_string(),
        )),
    }
}

fn tail(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.tail() takes 1 argument, got {}",
            args.len()
        )));
    }
    match list_tail_view(&args[0]) {
        Some(v) => Ok(Value::Some(Box::new(v))),
        None if list_len(&args[0]).is_some() => Ok(Value::None),
        None => Err(RuntimeError::Error(
            "List.tail() argument must be a List".to_string(),
        )),
    }
}

fn zip(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.zip() takes 2 arguments (list, list), got {}",
            args.len()
        )));
    }
    let a = list_slice(&args[0]).ok_or_else(|| {
        RuntimeError::Error("List.zip() first argument must be a List".to_string())
    })?;
    let b = list_slice(&args[1]).ok_or_else(|| {
        RuntimeError::Error("List.zip() second argument must be a List".to_string())
    })?;
    let pairs: Vec<Value> = a
        .iter()
        .zip(b.iter())
        .map(|(x, y)| Value::Tuple(vec![x.clone(), y.clone()]))
        .collect();
    Ok(list_from_vec(pairs))
}
