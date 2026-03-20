/// Char namespace — Unicode scalar value operations on strings.
///
/// Char is NOT a type — these are functions operating on String (first character)
/// and Int (code point). Same pattern as Byte: namespace of operations on existing types.
///
/// Methods:
///   Char.toCode(s: String)    → Int             — Unicode scalar value of first char
///   Char.fromCode(n: Int)     → Option<String>  — code point to 1-char string
///
/// No effects required.
use std::collections::HashMap;
use std::rc::Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["toCode", "fromCode"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Char.{}", method)),
        );
    }
    global.insert(
        "Char".to_string(),
        Value::Namespace {
            name: "Char".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Char.toCode" => Some(to_code(args)),
        "Char.fromCode" => Some(from_code(args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn to_code(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Char.toCode() takes 1 argument, got {}",
            args.len()
        )));
    }
    let Value::Str(s) = &args[0] else {
        return Err(RuntimeError::Error(
            "Char.toCode: argument must be a String".to_string(),
        ));
    };
    match s.chars().next() {
        Some(c) => Ok(Value::Int(c as i64)),
        None => Err(RuntimeError::Error(
            "Char.toCode: string is empty".to_string(),
        )),
    }
}

fn from_code(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Char.fromCode() takes 1 argument, got {}",
            args.len()
        )));
    }
    let Value::Int(n) = &args[0] else {
        return Err(RuntimeError::Error(
            "Char.fromCode: argument must be an Int".to_string(),
        ));
    };
    let n = *n;
    if n < 0 {
        return Ok(Value::None);
    }
    let Ok(code) = u32::try_from(n) else {
        return Ok(Value::None);
    };
    match char::from_u32(code) {
        Some(c) => Ok(Value::Some(Box::new(Value::Str(c.to_string())))),
        None => Ok(Value::None),
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &["toCode", "fromCode"];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Char.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Char"),
        members,
    });
    global.insert("Char".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Char.toCode" => Some(to_code_nv(args, arena)),
        "Char.fromCode" => Some(from_code_nv(args, arena)),
        _ => None,
    }
}

fn to_code_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Char.toCode() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "Char.toCode: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index());
    match s.chars().next() {
        Some(c) => Ok(NanValue::new_int(c as i64, arena)),
        None => Err(RuntimeError::Error(
            "Char.toCode: string is empty".to_string(),
        )),
    }
}

fn from_code_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Char.fromCode() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "Char.fromCode: argument must be an Int".to_string(),
        ));
    }
    let n = args[0].as_int(arena);
    if n < 0 {
        return Ok(NanValue::NONE);
    }
    let Ok(code) = u32::try_from(n) else {
        return Ok(NanValue::NONE);
    };
    match char::from_u32(code) {
        Some(c) => {
            let s = c.to_string();
            let s_idx = arena.push_string(&s);
            let inner = NanValue::new_string(s_idx);
            Ok(NanValue::new_some_value(inner, arena))
        }
        None => Ok(NanValue::NONE),
    }
}
