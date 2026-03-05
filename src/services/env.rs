/// Env service — environment variables.
///
/// Methods:
///   Env.get(key)        — returns Option<String>
///   Env.set(key, value) — sets process env var, returns Unit
///
/// Effects are granular:
/// - Env.get
/// - Env.set
use std::collections::HashMap;

use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["get", "set"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Env.{}", method)),
        );
    }
    global.insert(
        "Env".to_string(),
        Value::Namespace {
            name: "Env".to_string(),
            members,
        },
    );
}

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Env.get" => &["Env.get"],
        "Env.set" => &["Env.set"],
        _ => &[],
    }
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Env.get" => Some(get(args)),
        "Env.set" => Some(set(args)),
        _ => None,
    }
}

fn get(args: &[Value]) -> Result<Value, RuntimeError> {
    let [key_val] = args else {
        return Err(RuntimeError::Error(format!(
            "Env.get() takes 1 argument (key), got {}",
            args.len()
        )));
    };
    let Value::Str(key) = key_val else {
        return Err(RuntimeError::Error(
            "Env.get: key must be a String".to_string(),
        ));
    };
    match std::env::var(key) {
        Ok(v) => Ok(Value::Some(Box::new(Value::Str(v)))),
        Err(_) => Ok(Value::None),
    }
}

fn set(args: &[Value]) -> Result<Value, RuntimeError> {
    let [key_val, value_val] = args else {
        return Err(RuntimeError::Error(format!(
            "Env.set() takes 2 arguments (key, value), got {}",
            args.len()
        )));
    };
    let Value::Str(key) = key_val else {
        return Err(RuntimeError::Error(
            "Env.set: key must be a String".to_string(),
        ));
    };
    let Value::Str(value) = value_val else {
        return Err(RuntimeError::Error(
            "Env.set: value must be a String".to_string(),
        ));
    };

    validate_key(key)?;
    if value.contains('\0') {
        return Err(RuntimeError::Error(
            "Env.set: value must not contain NUL".to_string(),
        ));
    }

    // SAFETY: key/value are validated to avoid unsupported env names/values.
    unsafe {
        std::env::set_var(key, value);
    }
    Ok(Value::Unit)
}

fn validate_key(key: &str) -> Result<(), RuntimeError> {
    if key.is_empty() {
        return Err(RuntimeError::Error(
            "Env.set: key must not be empty".to_string(),
        ));
    }
    if key.contains('=') {
        return Err(RuntimeError::Error(
            "Env.set: key must not contain '='".to_string(),
        ));
    }
    if key.contains('\0') {
        return Err(RuntimeError::Error(
            "Env.set: key must not contain NUL".to_string(),
        ));
    }
    Ok(())
}
