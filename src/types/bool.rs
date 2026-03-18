/// Bool namespace — logical combinators.
///
/// Methods:
///   Bool.or(a, b)   → Bool  — logical OR
///   Bool.and(a, b)  → Bool  — logical AND
///   Bool.not(a)     → Bool  — logical NOT
///
/// No effects required.
use std::collections::HashMap;
use std::rc::Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["or", "and", "not"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Bool.{}", method)),
        );
    }
    global.insert(
        "Bool".to_string(),
        Value::Namespace {
            name: "Bool".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Bool.or" => Some(bool_or(args)),
        "Bool.and" => Some(bool_and(args)),
        "Bool.not" => Some(bool_not(args)),
        _ => None,
    }
}

fn bool_or(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = args else {
        return Err(RuntimeError::Error(format!(
            "Bool.or() takes 2 arguments, got {}",
            args.len()
        )));
    };
    let Value::Bool(a) = a else {
        return Err(RuntimeError::Error(
            "Bool.or: first argument must be a Bool".to_string(),
        ));
    };
    let Value::Bool(b) = b else {
        return Err(RuntimeError::Error(
            "Bool.or: second argument must be a Bool".to_string(),
        ));
    };
    Ok(Value::Bool(*a || *b))
}

fn bool_and(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = args else {
        return Err(RuntimeError::Error(format!(
            "Bool.and() takes 2 arguments, got {}",
            args.len()
        )));
    };
    let Value::Bool(a) = a else {
        return Err(RuntimeError::Error(
            "Bool.and: first argument must be a Bool".to_string(),
        ));
    };
    let Value::Bool(b) = b else {
        return Err(RuntimeError::Error(
            "Bool.and: second argument must be a Bool".to_string(),
        ));
    };
    Ok(Value::Bool(*a && *b))
}

fn bool_not(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a] = args else {
        return Err(RuntimeError::Error(format!(
            "Bool.not() takes 1 argument, got {}",
            args.len()
        )));
    };
    let Value::Bool(a) = a else {
        return Err(RuntimeError::Error(
            "Bool.not: argument must be a Bool".to_string(),
        ));
    };
    Ok(Value::Bool(!*a))
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &["or", "and", "not"];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Bool.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Bool"),
        members,
    });
    global.insert("Bool".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    _arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Bool.or" => Some(bool_or_nv(args)),
        "Bool.and" => Some(bool_and_nv(args)),
        "Bool.not" => Some(bool_not_nv(args)),
        _ => None,
    }
}

fn bool_or_nv(args: &[NanValue]) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Bool.or() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.or: first argument must be a Bool".to_string(),
        ));
    }
    if !args[1].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.or: second argument must be a Bool".to_string(),
        ));
    }
    Ok(NanValue::new_bool(args[0].as_bool() || args[1].as_bool()))
}

fn bool_and_nv(args: &[NanValue]) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Bool.and() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.and: first argument must be a Bool".to_string(),
        ));
    }
    if !args[1].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.and: second argument must be a Bool".to_string(),
        ));
    }
    Ok(NanValue::new_bool(args[0].as_bool() && args[1].as_bool()))
}

fn bool_not_nv(args: &[NanValue]) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Bool.not() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.not: argument must be a Bool".to_string(),
        ));
    }
    Ok(NanValue::new_bool(!args[0].as_bool()))
}
