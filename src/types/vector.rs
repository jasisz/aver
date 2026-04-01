/// Vector namespace — fixed-size indexed sequence helpers.
///
/// Methods:
///   Vector.new(size, default)     → Vector<T>
///   Vector.get(vec, idx)          → Option<T>
///   Vector.set(vec, idx, val)     → Option<Vector<T>>
///   Vector.len(vec)               → Int
///   Vector.fromList(xs)           → Vector<T>
///   Vector.toList(vec)            → List<T>
///
/// No effects required.
use std::collections::HashMap;
use std::sync::Arc as Rc;

use aver_rt::AverVector;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value, list_from_vec, list_view};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["new", "get", "set", "len", "fromList", "toList"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Vector.{}", method)),
        );
    }
    global.insert(
        "Vector".to_string(),
        Value::Namespace {
            name: "Vector".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Vector.new" => Some(vec_new(args)),
        "Vector.get" => Some(vec_get(args)),
        "Vector.set" => Some(vec_set(args)),
        "Vector.len" => Some(vec_len(args)),
        "Vector.fromList" => Some(vec_from_list(args)),
        "Vector.toList" => Some(vec_to_list(args)),
        _ => None,
    }
}

fn vec_new(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Vector.new() takes 2 arguments (size, default), got {}",
            args.len()
        )));
    }
    let Value::Int(size) = &args[0] else {
        return Err(RuntimeError::Error(
            "Vector.new: size must be an Int".to_string(),
        ));
    };
    if *size < 0 {
        return Err(RuntimeError::Error(
            "Vector.new: size must not be negative".to_string(),
        ));
    }
    Ok(Value::Vector(AverVector::new(
        *size as usize,
        args[1].clone(),
    )))
}

fn vec_get(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Vector.get() takes 2 arguments, got {}",
            args.len()
        )));
    }
    let Value::Vector(vec) = &args[0] else {
        return Err(RuntimeError::Error(
            "Vector.get: first argument must be a Vector".to_string(),
        ));
    };
    let Value::Int(idx) = &args[1] else {
        return Err(RuntimeError::Error(
            "Vector.get: index must be an Int".to_string(),
        ));
    };
    if *idx < 0 {
        return Ok(Value::None);
    }
    Ok(match vec.get(*idx as usize) {
        Some(v) => Value::Some(Box::new(v.clone())),
        None => Value::None,
    })
}

fn vec_set(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "Vector.set() takes 3 arguments, got {}",
            args.len()
        )));
    }
    let Value::Vector(vec) = &args[0] else {
        return Err(RuntimeError::Error(
            "Vector.set: first argument must be a Vector".to_string(),
        ));
    };
    let Value::Int(idx) = &args[1] else {
        return Err(RuntimeError::Error(
            "Vector.set: index must be an Int".to_string(),
        ));
    };
    if *idx < 0 {
        return Ok(Value::None);
    }
    Ok(match vec.set(*idx as usize, args[2].clone()) {
        Some(new_vec) => Value::Some(Box::new(Value::Vector(new_vec))),
        None => Value::None,
    })
}

fn vec_len(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Vector.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    let Value::Vector(vec) = &args[0] else {
        return Err(RuntimeError::Error(
            "Vector.len: argument must be a Vector".to_string(),
        ));
    };
    Ok(Value::Int(vec.len() as i64))
}

fn vec_from_list(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Vector.fromList() takes 1 argument, got {}",
            args.len()
        )));
    }
    let items = list_view(&args[0]).ok_or_else(|| {
        RuntimeError::Error("Vector.fromList: argument must be a List".to_string())
    })?;
    Ok(Value::Vector(AverVector::from_list(items)))
}

fn vec_to_list(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Vector.toList() takes 1 argument, got {}",
            args.len()
        )));
    }
    let Value::Vector(vec) = &args[0] else {
        return Err(RuntimeError::Error(
            "Vector.toList: argument must be a Vector".to_string(),
        ));
    };
    Ok(list_from_vec(vec.to_vec()))
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &["new", "get", "set", "len", "fromList", "toList"];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Vector.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Vector"),
        members,
    });
    global.insert("Vector".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Vector.new" => Some(vec_new_nv(args, arena)),
        "Vector.get" => Some(vec_get_nv(args, arena)),
        "Vector.set" => Some(vec_set_nv(args, arena)),
        "Vector.len" => Some(vec_len_nv(args, arena)),
        "Vector.fromList" => Some(vec_from_list_nv(args, arena)),
        "Vector.toList" => Some(vec_to_list_nv(args, arena)),
        _ => None,
    }
}

fn vec_new_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Vector.new() takes 2 arguments (size, default), got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "Vector.new: size must be an Int".to_string(),
        ));
    }
    let size = args[0].as_int(arena);
    if size < 0 {
        return Err(RuntimeError::Error(
            "Vector.new: size must not be negative".to_string(),
        ));
    }
    let items = vec![args[1]; size as usize];
    if items.is_empty() {
        Ok(NanValue::EMPTY_VECTOR)
    } else {
        Ok(NanValue::new_vector(arena.push_vector(items)))
    }
}

fn vec_get_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Vector.get() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_vector() {
        return Err(RuntimeError::Error(
            "Vector.get: first argument must be a Vector".to_string(),
        ));
    }
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "Vector.get: index must be an Int".to_string(),
        ));
    }
    let idx = args[1].as_int(arena);
    if idx < 0 {
        return Ok(NanValue::NONE);
    }
    let items = arena.vector_ref_value(args[0]);
    match items.get(idx as usize) {
        Some(&v) => Ok(NanValue::new_some_value(v, arena)),
        None => Ok(NanValue::NONE),
    }
}

fn vec_set_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "Vector.set() takes 3 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_vector() {
        return Err(RuntimeError::Error(
            "Vector.set: first argument must be a Vector".to_string(),
        ));
    }
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "Vector.set: index must be an Int".to_string(),
        ));
    }
    let idx = args[1].as_int(arena);
    if idx < 0 {
        return Ok(NanValue::NONE);
    }
    let mut items = arena.clone_vector_value(args[0]);
    let uidx = idx as usize;
    if uidx >= items.len() {
        return Ok(NanValue::NONE);
    }
    items[uidx] = args[2];
    let new_vec_idx = arena.push_vector(items);
    Ok(NanValue::new_some_value(
        NanValue::new_vector(new_vec_idx),
        arena,
    ))
}

fn vec_len_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Vector.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_vector() {
        return Err(RuntimeError::Error(
            "Vector.len: argument must be a Vector".to_string(),
        ));
    }
    let items = arena.vector_ref_value(args[0]);
    Ok(NanValue::new_int(items.len() as i64, arena))
}

fn vec_from_list_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Vector.fromList() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "Vector.fromList: argument must be a List".to_string(),
        ));
    }
    let items = arena.list_to_vec_value(args[0]);
    if items.is_empty() {
        Ok(NanValue::EMPTY_VECTOR)
    } else {
        Ok(NanValue::new_vector(arena.push_vector(items)))
    }
}

fn vec_to_list_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Vector.toList() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_vector() {
        return Err(RuntimeError::Error(
            "Vector.toList: argument must be a Vector".to_string(),
        ));
    }
    let items = arena.clone_vector_value(args[0]);
    if items.is_empty() {
        Ok(NanValue::EMPTY_LIST)
    } else {
        let list_idx = arena.push_list(items);
        Ok(NanValue::new_list(list_idx))
    }
}
