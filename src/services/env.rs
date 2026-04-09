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
use std::sync::Arc as Rc;

use crate::nan_value::{Arena, NanValue};
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

pub const DECLARED_EFFECTS: &[&str] = &["Env.get", "Env.set"];

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
    match aver_rt::env_get(key) {
        Some(v) => Ok(Value::Some(Box::new(Value::Str(v)))),
        None => Ok(Value::None),
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

    if let Err(e) = aver_rt::env_set(key, value) {
        return Err(RuntimeError::Error(e));
    }
    Ok(Value::Unit)
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &["get", "set"];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Env.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Env"),
        members,
    });
    global.insert("Env".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Env.get" => Some(get_nv(args, arena)),
        "Env.set" => Some(set_nv(args, arena)),
        _ => None,
    }
}

fn get_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Env.get() takes 1 argument (key), got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "Env.get: key must be a String".to_string(),
        ));
    }
    let key = arena.get_string_value(args[0]).to_string();
    match aver_rt::env_get(&key) {
        Some(v) => {
            let inner = NanValue::new_string_value(&v, arena);
            Ok(NanValue::new_some_value(inner, arena))
        }
        None => Ok(NanValue::NONE),
    }
}

fn set_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Env.set() takes 2 arguments (key, value), got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "Env.set: key must be a String".to_string(),
        ));
    }
    if !args[1].is_string() {
        return Err(RuntimeError::Error(
            "Env.set: value must be a String".to_string(),
        ));
    }
    let key = arena.get_string_value(args[0]).to_string();
    let value = arena.get_string_value(args[1]).to_string();
    if let Err(e) = aver_rt::env_set(&key, &value) {
        return Err(RuntimeError::Error(e));
    }
    Ok(NanValue::UNIT)
}
