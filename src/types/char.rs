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
    match char::from_u32(n as u32) {
        Some(c) => Ok(Value::Some(Box::new(Value::Str(c.to_string())))),
        None => Ok(Value::None),
    }
}
