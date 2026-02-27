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
    if n < 0 || n > 255 {
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
