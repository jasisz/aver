/// Int namespace — numeric helpers for integer values.
///
/// Methods:
///   Int.fromString(s)   → Result<Int, String>  — parse string to int
///   Int.fromFloat(f)    → Int                  — truncate float to int
///   Int.toString(n)     → String               — format int as string
///   Int.abs(n)          → Int                  — absolute value
///   Int.min(a, b)       → Int                  — minimum of two ints
///   Int.max(a, b)       → Int                  — maximum of two ints
///   Int.mod(a, b)       → Result<Int, String>  — modulo (error on b=0)
///   Int.toFloat(n)      → Float                — widen int to float
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
        "fromFloat",
        "toString",
        "abs",
        "min",
        "max",
        "mod",
        "toFloat",
    ] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Int.{}", method)),
        );
    }
    global.insert(
        "Int".to_string(),
        Value::Namespace {
            name: "Int".to_string(),
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
        "Int.fromString" => Some(from_string(args)),
        "Int.fromFloat" => Some(from_float(args)),
        "Int.toString" => Some(to_string(args)),
        "Int.abs" => Some(abs(args)),
        "Int.min" => Some(min(args)),
        "Int.max" => Some(max(args)),
        "Int.mod" => Some(modulo(args)),
        "Int.toFloat" => Some(to_float(args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn from_string(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Int.fromString", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "Int.fromString: argument must be a String".to_string(),
        ));
    };
    match s.parse::<i64>() {
        Ok(n) => Ok(Value::Ok(Box::new(Value::Int(n)))),
        Err(_) => Ok(Value::Err(Box::new(Value::Str(format!(
            "Cannot parse '{}' as Int",
            s
        ))))),
    }
}

fn from_float(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Int.fromFloat", args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "Int.fromFloat: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Int(*f as i64))
}

fn to_string(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Int.toString", args)?;
    let Value::Int(n) = val else {
        return Err(RuntimeError::Error(
            "Int.toString: argument must be an Int".to_string(),
        ));
    };
    Ok(Value::Str(format!("{}", n)))
}

fn abs(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Int.abs", args)?;
    let Value::Int(n) = val else {
        return Err(RuntimeError::Error(
            "Int.abs: argument must be an Int".to_string(),
        ));
    };
    Ok(Value::Int(n.abs()))
}

fn min(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Int.min", args)?;
    let (Value::Int(x), Value::Int(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Int.min: both arguments must be Int".to_string(),
        ));
    };
    Ok(Value::Int(std::cmp::min(*x, *y)))
}

fn max(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Int.max", args)?;
    let (Value::Int(x), Value::Int(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Int.max: both arguments must be Int".to_string(),
        ));
    };
    Ok(Value::Int(std::cmp::max(*x, *y)))
}

fn modulo(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Int.mod", args)?;
    let (Value::Int(x), Value::Int(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Int.mod: both arguments must be Int".to_string(),
        ));
    };
    if *y == 0 {
        Ok(Value::Err(Box::new(Value::Str(
            "division by zero".to_string(),
        ))))
    } else {
        Ok(Value::Ok(Box::new(Value::Int(x % y))))
    }
}

fn to_float(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Int.toFloat", args)?;
    let Value::Int(n) = val else {
        return Err(RuntimeError::Error(
            "Int.toFloat: argument must be an Int".to_string(),
        ));
    };
    Ok(Value::Float(*n as f64))
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
        "fromString",
        "fromFloat",
        "toString",
        "abs",
        "min",
        "max",
        "mod",
        "toFloat",
    ];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Int.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Int"),
        members,
    });
    global.insert("Int".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Int.fromString" => Some(from_string_nv(args, arena)),
        "Int.fromFloat" => Some(from_float_nv(args, arena)),
        "Int.toString" => Some(to_string_nv(args, arena)),
        "Int.abs" => Some(abs_nv(args, arena)),
        "Int.min" => Some(min_nv(args, arena)),
        "Int.max" => Some(max_nv(args, arena)),
        "Int.mod" => Some(modulo_nv(args, arena)),
        "Int.toFloat" => Some(to_float_nv(args, arena)),
        _ => None,
    }
}

fn nv_check1(name: &str, args: &[NanValue]) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument, got {}",
            name,
            args.len()
        )));
    }
    Ok(args[0])
}

fn nv_check2(name: &str, args: &[NanValue]) -> Result<(NanValue, NanValue), RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 2 arguments, got {}",
            name,
            args.len()
        )));
    }
    Ok((args[0], args[1]))
}

fn from_string_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Int.fromString", args)?;
    if !v.is_string() {
        return Err(RuntimeError::Error(
            "Int.fromString: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string_value(v);
    match s.parse::<i64>() {
        Ok(n) => {
            let inner = NanValue::new_int(n, arena);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        Err(_) => {
            let msg = format!("Cannot parse '{}' as Int", s);
            let inner = NanValue::new_string_value(&msg, arena);
            Ok(NanValue::new_err_value(inner, arena))
        }
    }
}

fn from_float_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Int.fromFloat", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error(
            "Int.fromFloat: argument must be a Float".to_string(),
        ));
    }
    Ok(NanValue::new_int(v.as_float() as i64, arena))
}

fn to_string_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Int.toString", args)?;
    if !v.is_int() {
        return Err(RuntimeError::Error(
            "Int.toString: argument must be an Int".to_string(),
        ));
    }
    let s = format!("{}", v.as_int(arena));
    Ok(NanValue::new_string_value(&s, arena))
}

fn abs_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Int.abs", args)?;
    if !v.is_int() {
        return Err(RuntimeError::Error(
            "Int.abs: argument must be an Int".to_string(),
        ));
    }
    Ok(NanValue::new_int(v.as_int(arena).abs(), arena))
}

fn min_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Int.min", args)?;
    if !a.is_int() || !b.is_int() {
        return Err(RuntimeError::Error(
            "Int.min: both arguments must be Int".to_string(),
        ));
    }
    Ok(NanValue::new_int(
        std::cmp::min(a.as_int(arena), b.as_int(arena)),
        arena,
    ))
}

fn max_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Int.max", args)?;
    if !a.is_int() || !b.is_int() {
        return Err(RuntimeError::Error(
            "Int.max: both arguments must be Int".to_string(),
        ));
    }
    Ok(NanValue::new_int(
        std::cmp::max(a.as_int(arena), b.as_int(arena)),
        arena,
    ))
}

fn modulo_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Int.mod", args)?;
    if !a.is_int() || !b.is_int() {
        return Err(RuntimeError::Error(
            "Int.mod: both arguments must be Int".to_string(),
        ));
    }
    let x = a.as_int(arena);
    let y = b.as_int(arena);
    if y == 0 {
        let inner = NanValue::new_string_value("division by zero", arena);
        Ok(NanValue::new_err_value(inner, arena))
    } else {
        let inner = NanValue::new_int(x % y, arena);
        Ok(NanValue::new_ok_value(inner, arena))
    }
}

fn to_float_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Int.toFloat", args)?;
    if !v.is_int() {
        return Err(RuntimeError::Error(
            "Int.toFloat: argument must be an Int".to_string(),
        ));
    }
    Ok(NanValue::new_float(v.as_int(arena) as f64))
}
