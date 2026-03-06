/// List namespace — list manipulation helpers.
///
/// Methods:
///   List.len(list)           → Int                    — number of elements
///   List.get(list, index)    → Option<T>              — element at index
///   List.append(list, val)    → List<T>                — append element (returns new list)
///   List.prepend(val, list)  → List<T>                — prepend element
///   List.concat(a, b)        → List<T>                — concatenate two lists
///   List.reverse(list)       → List<T>                — reverse elements
///   List.contains(list, val) → Bool                   — membership by `==`
///   List.zip(a, b)           → List<(A, B)>           — pair elements from two lists
///
/// No effects required.
use std::collections::HashMap;

use crate::value::{
    RuntimeError, Value, list_append, list_concat, list_get, list_len, list_prepend, list_reverse,
    list_view,
};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "len", "get", "append", "prepend", "concat", "reverse", "contains", "zip",
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
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "List.len" => Some(len(args)),
        "List.get" => Some(get(args)),
        "List.append" => Some(append(args)),
        "List.prepend" => Some(prepend(args)),
        "List.concat" => Some(concat(args)),
        "List.reverse" => Some(reverse(args)),
        "List.contains" => Some(contains(args)),
        "List.zip" => Some(zip(args)),
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
    list_view(&args[0]).ok_or_else(|| {
        RuntimeError::Error("List.get() first argument must be a List".to_string())
    })?;
    let index = match &args[1] {
        Value::Int(i) => *i,
        _ => {
            return Err(RuntimeError::Error(
                "List.get() index must be an Int".to_string(),
            ));
        }
    };
    if index < 0 {
        Ok(Value::None)
    } else {
        Ok(match list_get(&args[0], index as usize) {
            Some(value) => Value::Some(Box::new(value)),
            None => Value::None,
        })
    }
}

fn append(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.append() takes 2 arguments (list, val), got {}",
            args.len()
        )));
    }
    list_append(&args[0], args[1].clone()).ok_or_else(|| {
        RuntimeError::Error("List.append() first argument must be a List".to_string())
    })
}

fn prepend(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.prepend() takes 2 arguments (val, list), got {}",
            args.len()
        )));
    }
    list_prepend(args[0].clone(), &args[1]).ok_or_else(|| {
        RuntimeError::Error("List.prepend() second argument must be a List".to_string())
    })
}

fn concat(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.concat() takes 2 arguments (list, list), got {}",
            args.len()
        )));
    }
    list_view(&args[0]).ok_or_else(|| {
        RuntimeError::Error("List.concat() first argument must be a List".to_string())
    })?;
    list_view(&args[1]).ok_or_else(|| {
        RuntimeError::Error("List.concat() second argument must be a List".to_string())
    })?;
    Ok(list_concat(&args[0], &args[1]).expect("validated list arguments above"))
}

fn reverse(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.reverse() takes 1 argument, got {}",
            args.len()
        )));
    }
    list_reverse(&args[0])
        .ok_or_else(|| RuntimeError::Error("List.reverse() argument must be a List".to_string()))
}

fn contains(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.contains() takes 2 arguments (list, value), got {}",
            args.len()
        )));
    }
    let list = list_view(&args[0]).ok_or_else(|| {
        RuntimeError::Error("List.contains() first argument must be a List".to_string())
    })?;
    let target = &args[1];
    Ok(Value::Bool(list.iter().any(|item| item == target)))
}

fn zip(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.zip() takes 2 arguments (list, list), got {}",
            args.len()
        )));
    }
    let a = list_view(&args[0]).ok_or_else(|| {
        RuntimeError::Error("List.zip() first argument must be a List".to_string())
    })?;
    let b = list_view(&args[1]).ok_or_else(|| {
        RuntimeError::Error("List.zip() second argument must be a List".to_string())
    })?;
    let pairs: Vec<Value> = a
        .iter()
        .zip(b.iter())
        .map(|(x, y)| Value::Tuple(vec![x.clone(), y.clone()]))
        .collect();
    Ok(crate::value::list_from_vec(pairs))
}
