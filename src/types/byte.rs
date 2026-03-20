/// Byte namespace — byte-level operations on integers.
///
/// Byte is NOT a type — these are functions operating on Int (0–255).
/// Same pattern as Char: namespace of operations on existing types.
///
/// Methods:
///   Byte.toHex(n: Int)        → Result<String, String>  — byte to 2-char lowercase hex
///   Byte.fromHex(s: String)   → Result<Int, String>     — 2-char hex to byte
///
/// No effects required.
use std::collections::HashMap;
use std::rc::Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["toHex", "fromHex"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Byte.{}", method)),
        );
    }
    global.insert(
        "Byte".to_string(),
        Value::Namespace {
            name: "Byte".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Byte.toHex" => Some(to_hex(args)),
        "Byte.fromHex" => Some(from_hex(args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn to_hex(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Byte.toHex() takes 1 argument, got {}",
            args.len()
        )));
    }
    let Value::Int(n) = &args[0] else {
        return Err(RuntimeError::Error(
            "Byte.toHex: argument must be an Int".to_string(),
        ));
    };
    let n = *n;
    if !(0..=255).contains(&n) {
        return Ok(Value::Err(Box::new(Value::Str(format!(
            "Byte.toHex: {} is out of range 0–255",
            n
        )))));
    }
    Ok(Value::Ok(Box::new(Value::Str(format!("{:02x}", n)))))
}

fn from_hex(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Byte.fromHex() takes 1 argument, got {}",
            args.len()
        )));
    }
    let Value::Str(s) = &args[0] else {
        return Err(RuntimeError::Error(
            "Byte.fromHex: argument must be a String".to_string(),
        ));
    };
    if s.len() != 2 {
        return Ok(Value::Err(Box::new(Value::Str(format!(
            "Byte.fromHex: expected exactly 2 hex chars, got '{}'",
            s
        )))));
    }
    match u8::from_str_radix(s, 16) {
        Ok(n) => Ok(Value::Ok(Box::new(Value::Int(n as i64)))),
        Err(_) => Ok(Value::Err(Box::new(Value::Str(format!(
            "Byte.fromHex: invalid hex '{}'",
            s
        ))))),
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &["toHex", "fromHex"];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Byte.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Byte"),
        members,
    });
    global.insert("Byte".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Byte.toHex" => Some(to_hex_nv(args, arena)),
        "Byte.fromHex" => Some(from_hex_nv(args, arena)),
        _ => None,
    }
}

fn nv_ok_str(s: &str, arena: &mut Arena) -> NanValue {
    let s_idx = arena.push_string(s);
    let inner = NanValue::new_string(s_idx);
    NanValue::new_ok_value(inner, arena)
}

fn nv_err_str(s: &str, arena: &mut Arena) -> NanValue {
    let s_idx = arena.push_string(s);
    let inner = NanValue::new_string(s_idx);
    NanValue::new_err_value(inner, arena)
}

fn to_hex_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Byte.toHex() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "Byte.toHex: argument must be an Int".to_string(),
        ));
    }
    let n = args[0].as_int(arena);
    if !(0..=255).contains(&n) {
        return Ok(nv_err_str(
            &format!("Byte.toHex: {} is out of range 0-255", n),
            arena,
        ));
    }
    Ok(nv_ok_str(&format!("{:02x}", n), arena))
}

fn from_hex_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Byte.fromHex() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "Byte.fromHex: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index()).to_string();
    if s.len() != 2 {
        return Ok(nv_err_str(
            &format!("Byte.fromHex: expected exactly 2 hex chars, got '{}'", s),
            arena,
        ));
    }
    match u8::from_str_radix(&s, 16) {
        Ok(n) => {
            let inner = NanValue::new_int(n as i64, arena);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        Err(_) => Ok(nv_err_str(
            &format!("Byte.fromHex: invalid hex '{}'", s),
            arena,
        )),
    }
}
