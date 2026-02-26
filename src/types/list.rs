/// List namespace — list manipulation helpers.
///
/// Methods:
///   List.len(list)           → Int                    — number of elements
///   List.get(list, index)    → Result<T, String>      — element at index
///   List.push(list, val)     → List<T>                — append element (returns new list)
///   List.head(list)          → Result<T, String>      — first element
///   List.tail(list)          → Result<List<T>, String> — all but first
///
/// Note: List.map, List.filter, List.fold are handled directly in the
/// interpreter because they need to invoke closures via `self.call_value`.
///
/// No effects required.
use std::collections::HashMap;

use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "len", "map", "filter", "fold", "get", "push", "head", "tail",
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
pub fn call(name: &str, args: Vec<Value>) -> Option<Result<Value, RuntimeError>> {
    match name {
        "List.len" => Some(len(args)),
        "List.get" => Some(get(args)),
        "List.push" => Some(push(args)),
        "List.head" => Some(head(args)),
        "List.tail" => Some(tail(args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn len(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    match &args[0] {
        Value::List(items) => Ok(Value::Int(items.len() as i64)),
        _ => Err(RuntimeError::Error(
            "List.len() argument must be a List".to_string(),
        )),
    }
}

fn get(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.get() takes 2 arguments (list, index), got {}",
            args.len()
        )));
    }
    let list = match args[0].clone() {
        Value::List(items) => items,
        _ => {
            return Err(RuntimeError::Error(
                "List.get() first argument must be a List".to_string(),
            ))
        }
    };
    let index = match &args[1] {
        Value::Int(i) => *i,
        _ => {
            return Err(RuntimeError::Error(
                "List.get() index must be an Int".to_string(),
            ))
        }
    };
    if index < 0 || index as usize >= list.len() {
        Ok(Value::Err(Box::new(Value::Str(format!(
            "index {} out of bounds",
            index
        )))))
    } else {
        Ok(Value::Ok(Box::new(list[index as usize].clone())))
    }
}

fn push(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.push() takes 2 arguments (list, val), got {}",
            args.len()
        )));
    }
    let mut list = match args[0].clone() {
        Value::List(items) => items,
        _ => {
            return Err(RuntimeError::Error(
                "List.push() first argument must be a List".to_string(),
            ))
        }
    };
    list.push(args[1].clone());
    Ok(Value::List(list))
}

fn head(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.head() takes 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].clone() {
        Value::List(items) => {
            if items.is_empty() {
                Ok(Value::Err(Box::new(Value::Str("empty list".to_string()))))
            } else {
                Ok(Value::Ok(Box::new(items[0].clone())))
            }
        }
        _ => Err(RuntimeError::Error(
            "List.head() argument must be a List".to_string(),
        )),
    }
}

fn tail(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.tail() takes 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].clone() {
        Value::List(items) => {
            if items.is_empty() {
                Ok(Value::Err(Box::new(Value::Str("empty list".to_string()))))
            } else {
                Ok(Value::Ok(Box::new(Value::List(items[1..].to_vec()))))
            }
        }
        _ => Err(RuntimeError::Error(
            "List.tail() argument must be a List".to_string(),
        )),
    }
}
