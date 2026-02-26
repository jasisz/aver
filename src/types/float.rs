/// Float namespace — numeric helpers for floating-point values.
///
/// Methods:
///   Float.fromString(s)  → Result<Float, String>  — parse string to float
///   Float.fromInt(n)     → Float                  — widen int to float
///   Float.toString(f)    → String                 — format float as string
///   Float.abs(f)         → Float                  — absolute value
///   Float.floor(f)       → Int                    — floor to int
///   Float.ceil(f)        → Int                    — ceil to int
///   Float.round(f)       → Int                    — round to int
///   Float.min(a, b)      → Float                  — minimum of two floats
///   Float.max(a, b)      → Float                  — maximum of two floats
///
/// No effects required.
use std::collections::HashMap;

use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "fromString",
        "fromInt",
        "toString",
        "abs",
        "floor",
        "ceil",
        "round",
        "min",
        "max",
    ] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Float.{}", method)),
        );
    }
    global.insert(
        "Float".to_string(),
        Value::Namespace {
            name: "Float".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

/// Returns `Some(result)` when `name` is owned by this namespace, `None` otherwise.
pub fn call(name: &str, args: Vec<Value>) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Float.fromString" => Some(from_string(args)),
        "Float.fromInt" => Some(from_int(args)),
        "Float.toString" => Some(to_string(args)),
        "Float.abs" => Some(abs(args)),
        "Float.floor" => Some(floor(args)),
        "Float.ceil" => Some(ceil(args)),
        "Float.round" => Some(round(args)),
        "Float.min" => Some(min(args)),
        "Float.max" => Some(max(args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn from_string(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.fromString", &args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "Float.fromString: argument must be a String".to_string(),
        ));
    };
    match s.parse::<f64>() {
        Ok(f) => Ok(Value::Ok(Box::new(Value::Float(f)))),
        Err(_) => Ok(Value::Err(Box::new(Value::Str(format!(
            "Cannot parse '{}' as Float",
            s
        ))))),
    }
}

fn from_int(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.fromInt", &args)?;
    let Value::Int(n) = val else {
        return Err(RuntimeError::Error(
            "Float.fromInt: argument must be an Int".to_string(),
        ));
    };
    Ok(Value::Float(*n as f64))
}

fn to_string(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.toString", &args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.toString: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Str(format!("{}", f)))
}

fn abs(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.abs", &args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.abs: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Float(f.abs()))
}

fn floor(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.floor", &args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.floor: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Int(f.floor() as i64))
}

fn ceil(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.ceil", &args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.ceil: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Int(f.ceil() as i64))
}

fn round(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.round", &args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.round: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Int(f.round() as i64))
}

fn min(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Float.min", &args)?;
    let (Value::Float(x), Value::Float(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Float.min: both arguments must be Float".to_string(),
        ));
    };
    Ok(Value::Float(f64::min(*x, *y)))
}

fn max(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Float.max", &args)?;
    let (Value::Float(x), Value::Float(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Float.max: both arguments must be Float".to_string(),
        ));
    };
    Ok(Value::Float(f64::max(*x, *y)))
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
