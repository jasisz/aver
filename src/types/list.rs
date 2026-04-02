/// List namespace — list manipulation helpers.
///
/// Methods:
///   List.len(list)           → Int                    — number of elements
///   List.prepend(val, list)  → List<T>                — prepend element
///   List.take(list, n)       → List<T>                — first `n` elements
///   List.drop(list, n)       → List<T>                — all but first `n` elements
///   List.concat(a, b)        → List<T>                — concatenate two lists
///   List.reverse(list)       → List<T>                — reverse elements
///   List.contains(list, val) → Bool                   — membership by `==`
///   List.zip(a, b)           → List<(A, B)>           — pair elements from two lists
///
/// No effects required.
use std::collections::HashMap;
use std::sync::Arc as Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{
    RuntimeError, Value, list_concat, list_len, list_prepend, list_reverse, list_view,
};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "len", "prepend", "take", "drop", "concat", "reverse", "contains", "zip",
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
        "List.prepend" => Some(prepend(args)),
        "List.take" => Some(take(args)),
        "List.drop" => Some(drop(args)),
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

fn take(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.take() takes 2 arguments (list, n), got {}",
            args.len()
        )));
    }
    let list = list_view(&args[0]).ok_or_else(|| {
        RuntimeError::Error("List.take() first argument must be a List".to_string())
    })?;
    let count = match args[1] {
        Value::Int(n) if n <= 0 => 0usize,
        Value::Int(n) => usize::try_from(n).unwrap_or(usize::MAX),
        _ => {
            return Err(RuntimeError::Error(
                "List.take() second argument must be an Int".to_string(),
            ));
        }
    };
    Ok(crate::value::list_from_vec(
        list.iter().take(count).cloned().collect(),
    ))
}

fn drop(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.drop() takes 2 arguments (list, n), got {}",
            args.len()
        )));
    }
    let list = list_view(&args[0]).ok_or_else(|| {
        RuntimeError::Error("List.drop() first argument must be a List".to_string())
    })?;
    let count = match args[1] {
        Value::Int(n) if n <= 0 => 0usize,
        Value::Int(n) => usize::try_from(n).unwrap_or(usize::MAX),
        _ => {
            return Err(RuntimeError::Error(
                "List.drop() second argument must be an Int".to_string(),
            ));
        }
    };
    Ok(crate::value::list_from_vec(
        list.iter().skip(count).cloned().collect(),
    ))
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

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &[
        "len", "prepend", "take", "drop", "concat", "reverse", "contains", "zip",
    ];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("List.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("List"),
        members,
    });
    global.insert("List".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "List.len" => Some(len_nv(args, arena)),
        "List.prepend" => Some(prepend_nv(args, arena)),
        "List.take" => Some(take_nv(args, arena)),
        "List.drop" => Some(drop_nv(args, arena)),
        "List.concat" => Some(concat_nv(args, arena)),
        "List.reverse" => Some(reverse_nv(args, arena)),
        "List.contains" => Some(contains_nv(args, arena)),
        "List.zip" => Some(zip_nv(args, arena)),
        _ => None,
    }
}

fn len_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.len() argument must be a List".to_string(),
        ));
    }
    Ok(NanValue::new_int(
        arena.list_len_value(args[0]) as i64,
        arena,
    ))
}

fn prepend_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.prepend() takes 2 arguments (val, list), got {}",
            args.len()
        )));
    }
    if !args[1].is_list() {
        return Err(RuntimeError::Error(
            "List.prepend() second argument must be a List".to_string(),
        ));
    }
    let list_idx = arena.push_list_prepend(args[0], args[1]);
    Ok(NanValue::new_list(list_idx))
}

fn take_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.take() takes 2 arguments (list, n), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.take() first argument must be a List".to_string(),
        ));
    }
    let count = if args[1].is_int() {
        let n = args[1].as_int(arena);
        if n <= 0 {
            0usize
        } else {
            usize::try_from(n).unwrap_or(usize::MAX)
        }
    } else {
        return Err(RuntimeError::Error(
            "List.take() second argument must be an Int".to_string(),
        ));
    };
    let items: Vec<NanValue> = arena
        .list_to_vec_value(args[0])
        .into_iter()
        .take(count)
        .collect();
    if items.is_empty() {
        return Ok(NanValue::EMPTY_LIST);
    }
    let list_idx = arena.push_list(items);
    Ok(NanValue::new_list(list_idx))
}

fn drop_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.drop() takes 2 arguments (list, n), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.drop() first argument must be a List".to_string(),
        ));
    }
    let count = if args[1].is_int() {
        let n = args[1].as_int(arena);
        if n <= 0 {
            0usize
        } else {
            usize::try_from(n).unwrap_or(usize::MAX)
        }
    } else {
        return Err(RuntimeError::Error(
            "List.drop() second argument must be an Int".to_string(),
        ));
    };
    let items: Vec<NanValue> = arena
        .list_to_vec_value(args[0])
        .into_iter()
        .skip(count)
        .collect();
    if items.is_empty() {
        return Ok(NanValue::EMPTY_LIST);
    }
    let list_idx = arena.push_list(items);
    Ok(NanValue::new_list(list_idx))
}

fn concat_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.concat() takes 2 arguments (list, list), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.concat() first argument must be a List".to_string(),
        ));
    }
    if !args[1].is_list() {
        return Err(RuntimeError::Error(
            "List.concat() second argument must be a List".to_string(),
        ));
    }
    if arena.list_is_empty_value(args[0]) {
        return Ok(args[1]);
    }
    if arena.list_is_empty_value(args[1]) {
        return Ok(args[0]);
    }
    let list_idx = arena.push_list_concat(args[0], args[1]);
    Ok(NanValue::new_list(list_idx))
}

fn reverse_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.reverse() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.reverse() argument must be a List".to_string(),
        ));
    }
    let mut items = arena.list_to_vec_value(args[0]);
    items.reverse();
    if items.is_empty() {
        return Ok(NanValue::EMPTY_LIST);
    }
    let list_idx = arena.push_list(items);
    Ok(NanValue::new_list(list_idx))
}

fn contains_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.contains() takes 2 arguments (list, value), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.contains() first argument must be a List".to_string(),
        ));
    }
    let items = arena.list_to_vec_value(args[0]);
    let target = args[1];
    let found = items.iter().any(|item| item.eq_in(target, arena));
    Ok(NanValue::new_bool(found))
}

fn zip_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.zip() takes 2 arguments (list, list), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.zip() first argument must be a List".to_string(),
        ));
    }
    if !args[1].is_list() {
        return Err(RuntimeError::Error(
            "List.zip() second argument must be a List".to_string(),
        ));
    }
    let a = arena.list_to_vec_value(args[0]);
    let b = arena.list_to_vec_value(args[1]);
    let pairs: Vec<NanValue> = a
        .iter()
        .zip(b.iter())
        .map(|(x, y)| {
            let tuple_idx = arena.push_tuple(vec![*x, *y]);
            NanValue::new_tuple(tuple_idx)
        })
        .collect();
    if pairs.is_empty() {
        return Ok(NanValue::EMPTY_LIST);
    }
    let list_idx = arena.push_list(pairs);
    Ok(NanValue::new_list(list_idx))
}
