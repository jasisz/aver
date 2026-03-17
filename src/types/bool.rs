/// Bool namespace — logical combinators.
///
/// Methods:
///   Bool.or(a, b)   → Bool  — logical OR
///   Bool.and(a, b)  → Bool  — logical AND
///   Bool.not(a)     → Bool  — logical NOT
///
/// No effects required.
use std::collections::HashMap;

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
