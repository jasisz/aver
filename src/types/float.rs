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
use std::rc::Rc;

use crate::nan_value::{Arena, NanValue};
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
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
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

fn from_string(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.fromString", args)?;
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

fn from_int(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.fromInt", args)?;
    let Value::Int(n) = val else {
        return Err(RuntimeError::Error(
            "Float.fromInt: argument must be an Int".to_string(),
        ));
    };
    Ok(Value::Float(*n as f64))
}

fn to_string(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.toString", args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.toString: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Str(format!("{}", f)))
}

fn abs(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.abs", args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.abs: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Float(f.abs()))
}

fn floor(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.floor", args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.floor: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Int(f.floor() as i64))
}

fn ceil(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.ceil", args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.ceil: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Int(f.ceil() as i64))
}

fn round(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Float.round", args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Float.round: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Int(f.round() as i64))
}

fn min(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Float.min", args)?;
    let (Value::Float(x), Value::Float(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Float.min: both arguments must be Float".to_string(),
        ));
    };
    Ok(Value::Float(f64::min(*x, *y)))
}

fn max(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Float.max", args)?;
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

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &[
        "fromString", "fromInt", "toString", "abs", "floor", "ceil", "round", "min", "max",
    ];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Float.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Float"),
        members,
    });
    global.insert("Float".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(name: &str, args: &[NanValue], arena: &mut Arena) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Float.fromString" => Some(from_string_nv(args, arena)),
        "Float.fromInt" => Some(from_int_nv(args, arena)),
        "Float.toString" => Some(to_string_nv(args, arena)),
        "Float.abs" => Some(abs_nv(args, arena)),
        "Float.floor" => Some(floor_nv(args, arena)),
        "Float.ceil" => Some(ceil_nv(args, arena)),
        "Float.round" => Some(round_nv(args, arena)),
        "Float.min" => Some(min_nv(args, arena)),
        "Float.max" => Some(max_nv(args, arena)),
        _ => None,
    }
}

fn nv_check1(name: &str, args: &[NanValue]) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!("{}() takes 1 argument, got {}", name, args.len())));
    }
    Ok(args[0])
}

fn nv_check2(name: &str, args: &[NanValue]) -> Result<(NanValue, NanValue), RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!("{}() takes 2 arguments, got {}", name, args.len())));
    }
    Ok((args[0], args[1]))
}

fn from_string_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.fromString", args)?;
    if !v.is_string() {
        return Err(RuntimeError::Error("Float.fromString: argument must be a String".to_string()));
    }
    let s = arena.get_string(v.arena_index());
    match s.parse::<f64>() {
        Ok(f) => {
            let inner = NanValue::new_float(f);
            let idx = arena.push_boxed(inner);
            Ok(NanValue::new_ok(idx))
        }
        Err(_) => {
            let msg = format!("Cannot parse '{}' as Float", s);
            let inner_idx = arena.push_string(&msg);
            let inner = NanValue::new_string(inner_idx);
            let idx = arena.push_boxed(inner);
            Ok(NanValue::new_err(idx))
        }
    }
}

fn from_int_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.fromInt", args)?;
    if !v.is_int() {
        return Err(RuntimeError::Error("Float.fromInt: argument must be an Int".to_string()));
    }
    Ok(NanValue::new_float(v.as_int(arena) as f64))
}

fn to_string_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.toString", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error("Float.toString: argument must be a Float".to_string()));
    }
    let s = format!("{}", v.as_float());
    let idx = arena.push_string(&s);
    Ok(NanValue::new_string(idx))
}

fn abs_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.abs", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error("Float.abs: argument must be a Float".to_string()));
    }
    Ok(NanValue::new_float(v.as_float().abs()))
}

fn floor_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.floor", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error("Float.floor: argument must be a Float".to_string()));
    }
    Ok(NanValue::new_int(v.as_float().floor() as i64, arena))
}

fn ceil_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.ceil", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error("Float.ceil: argument must be a Float".to_string()));
    }
    Ok(NanValue::new_int(v.as_float().ceil() as i64, arena))
}

fn round_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.round", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error("Float.round: argument must be a Float".to_string()));
    }
    Ok(NanValue::new_int(v.as_float().round() as i64, arena))
}

fn min_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Float.min", args)?;
    if !a.is_float() || !b.is_float() {
        return Err(RuntimeError::Error("Float.min: both arguments must be Float".to_string()));
    }
    Ok(NanValue::new_float(f64::min(a.as_float(), b.as_float())))
}

fn max_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Float.max", args)?;
    if !a.is_float() || !b.is_float() {
        return Err(RuntimeError::Error("Float.max: both arguments must be Float".to_string()));
    }
    Ok(NanValue::new_float(f64::max(a.as_float(), b.as_float())))
}
