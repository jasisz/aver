/// String namespace — text manipulation helpers.
///
/// Methods:
///   String.length(s)            → Int            — char count (code points)
///   String.byteLength(s)        → Int            — byte count (UTF-8)
///   String.startsWith(s, pre)   → Bool
///   String.endsWith(s, suf)     → Bool
///   String.contains(s, sub)     → Bool
///   String.slice(s, from, to)   → String         — code-point based substring
///   String.trim(s)              → String
///   String.split(s, delim)      → List<String>
///   String.replace(s, old, new) → String
///   String.join(list, sep)      → String
///   String.chars(s)             → List<String>   — each char as 1-char string
///   String.fromInt(n)           → String
///   String.fromFloat(f)         → String
///   String.fromBool(b)          → String
///
/// No effects required.
use std::collections::HashMap;

use crate::value::{list_from_vec, list_slice, RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "length",
        "byteLength",
        "startsWith",
        "endsWith",
        "contains",
        "slice",
        "trim",
        "split",
        "replace",
        "join",
        "chars",
        "fromInt",
        "fromFloat",
        "fromBool",
    ] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("String.{}", method)),
        );
    }
    global.insert(
        "String".to_string(),
        Value::Namespace {
            name: "String".to_string(),
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
        "String.length" => Some(length(&args)),
        "String.byteLength" => Some(byte_length(&args)),
        "String.startsWith" => Some(starts_with(&args)),
        "String.endsWith" => Some(ends_with(&args)),
        "String.contains" => Some(contains(&args)),
        "String.slice" => Some(slice(&args)),
        "String.trim" => Some(trim(&args)),
        "String.split" => Some(split(&args)),
        "String.replace" => Some(replace(&args)),
        "String.join" => Some(join(&args)),
        "String.chars" => Some(chars(&args)),
        "String.fromInt" => Some(from_int(&args)),
        "String.fromFloat" => Some(from_float(&args)),
        "String.fromBool" => Some(from_bool(&args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn length(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.length", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.length: argument must be a String".to_string(),
        ));
    };
    Ok(Value::Int(s.chars().count() as i64))
}

fn byte_length(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.byteLength", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.byteLength: argument must be a String".to_string(),
        ));
    };
    Ok(Value::Int(s.len() as i64))
}

fn starts_with(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.startsWith", args)?;
    let (Value::Str(s), Value::Str(prefix)) = (a, b) else {
        return Err(RuntimeError::Error(
            "String.startsWith: both arguments must be String".to_string(),
        ));
    };
    Ok(Value::Bool(s.starts_with(prefix.as_str())))
}

fn ends_with(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.endsWith", args)?;
    let (Value::Str(s), Value::Str(suffix)) = (a, b) else {
        return Err(RuntimeError::Error(
            "String.endsWith: both arguments must be String".to_string(),
        ));
    };
    Ok(Value::Bool(s.ends_with(suffix.as_str())))
}

fn contains(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.contains", args)?;
    let (Value::Str(s), Value::Str(sub)) = (a, b) else {
        return Err(RuntimeError::Error(
            "String.contains: both arguments must be String".to_string(),
        ));
    };
    Ok(Value::Bool(s.contains(sub.as_str())))
}

fn slice(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "String.slice() takes 3 arguments (s, from, to), got {}",
            args.len()
        )));
    }
    let Value::Str(s) = &args[0] else {
        return Err(RuntimeError::Error(
            "String.slice: first argument must be a String".to_string(),
        ));
    };
    let Value::Int(from) = &args[1] else {
        return Err(RuntimeError::Error(
            "String.slice: second argument must be an Int".to_string(),
        ));
    };
    let Value::Int(to) = &args[2] else {
        return Err(RuntimeError::Error(
            "String.slice: third argument must be an Int".to_string(),
        ));
    };
    let from = *from as usize;
    let to = *to as usize;
    let result: String = s.chars().skip(from).take(to.saturating_sub(from)).collect();
    Ok(Value::Str(result))
}

fn trim(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.trim", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.trim: argument must be a String".to_string(),
        ));
    };
    Ok(Value::Str(s.trim().to_string()))
}

fn split(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.split", args)?;
    let (Value::Str(s), Value::Str(delim)) = (a, b) else {
        return Err(RuntimeError::Error(
            "String.split: both arguments must be String".to_string(),
        ));
    };
    let parts: Vec<Value> = s
        .split(delim.as_str())
        .map(|p| Value::Str(p.to_string()))
        .collect();
    Ok(list_from_vec(parts))
}

fn replace(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "String.replace() takes 3 arguments (s, old, new), got {}",
            args.len()
        )));
    }
    let (Value::Str(s), Value::Str(old), Value::Str(new)) = (&args[0], &args[1], &args[2]) else {
        return Err(RuntimeError::Error(
            "String.replace: all arguments must be String".to_string(),
        ));
    };
    Ok(Value::Str(s.replace(old.as_str(), new.as_str())))
}

fn join(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.join", args)?;
    let items = list_slice(a).ok_or_else(|| {
        RuntimeError::Error("String.join: first argument must be a List".to_string())
    })?;
    let Value::Str(sep) = b else {
        return Err(RuntimeError::Error(
            "String.join: second argument must be a String".to_string(),
        ));
    };
    let strs: Result<Vec<String>, RuntimeError> = items
        .iter()
        .map(|v| match v {
            Value::Str(s) => Ok(s.clone()),
            _ => Err(RuntimeError::Error(
                "String.join: list elements must be String".to_string(),
            )),
        })
        .collect();
    Ok(Value::Str(strs?.join(sep.as_str())))
}

fn chars(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.chars", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.chars: argument must be a String".to_string(),
        ));
    };
    let result: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
    Ok(list_from_vec(result))
}

fn from_int(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.fromInt", args)?;
    let Value::Int(n) = val else {
        return Err(RuntimeError::Error(
            "String.fromInt: argument must be an Int".to_string(),
        ));
    };
    Ok(Value::Str(format!("{}", n)))
}

fn from_float(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.fromFloat", args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "String.fromFloat: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Str(format!("{}", f)))
}

fn from_bool(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.fromBool", args)?;
    let Value::Bool(b) = val else {
        return Err(RuntimeError::Error(
            "String.fromBool: argument must be a Bool".to_string(),
        ));
    };
    Ok(Value::Str(if *b { "true" } else { "false" }.to_string()))
}

// ─── Helpers ────────────────────────────────────────────────────────────────

fn one_arg<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 1], RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument, got {}",
            name,
            args.len()
        )));
    }
    Ok([&args[0]])
}

fn two_args<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 2], RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 2 arguments, got {}",
            name,
            args.len()
        )));
    }
    Ok([&args[0], &args[1]])
}
