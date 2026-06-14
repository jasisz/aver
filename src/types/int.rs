/// Int namespace — numeric helpers for integer values.
///
/// Methods:
///   Int.fromString(s)   → Result<Int, String>  — parse string to int
///   Int.fromFloat(f)    → Int                  — truncate float to int
///   Int.abs(n)          → Int                  — absolute value
///   Int.min(a, b)       → Int                  — minimum of two ints
///   Int.max(a, b)       → Int                  — maximum of two ints
///   Int.mod(a, b)       → Result<Int, String>  — Euclidean modulo:
///                                                  result has the sign of `b`
///                                                  (always >= 0 for b > 0).
///                                                  Errors on b == 0.
///   Int.div(a, b)       → Result<Int, String>  — truncating integer division
///                                                  (rounds toward zero, same as
///                                                  the old `/` operator).
///                                                  Errors on b == 0.
///
/// Stringification goes through `String.fromInt` (or `"{n}"` interpolation);
/// widening to Float goes through `Float.fromInt`.
///
/// No effects required.
use std::collections::HashMap;
use std::str::FromStr;
use std::sync::Arc as Rc;

use aver_rt::AverInt;

use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["fromString", "fromFloat", "abs", "min", "max", "mod", "div"] {
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
        "Int.abs" => Some(abs(args)),
        "Int.min" => Some(min(args)),
        "Int.max" => Some(max(args)),
        "Int.mod" => Some(modulo(args)),
        "Int.div" => Some(divide(args)),
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
    // `Int` is mathematical ℤ, so parsing is unbounded (no `i64::MAX` cliff).
    match AverInt::from_str(s) {
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
    Ok(Value::Int(float_to_aver_int(*f)))
}

fn abs(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Int.abs", args)?;
    let Value::Int(n) = val else {
        return Err(RuntimeError::Error(
            "Int.abs: argument must be an Int".to_string(),
        ));
    };
    // Over ℤ there is no `i64::MIN.abs()` overflow: it promotes cleanly.
    Ok(Value::Int(n.abs()))
}

fn min(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Int.min", args)?;
    let (Value::Int(x), Value::Int(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Int.min: both arguments must be Int".to_string(),
        ));
    };
    Ok(Value::Int(x.min_ref(y)))
}

fn max(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Int.max", args)?;
    let (Value::Int(x), Value::Int(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Int.max: both arguments must be Int".to_string(),
        ));
    };
    Ok(Value::Int(x.max_ref(y)))
}

fn modulo(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Int.mod", args)?;
    let (Value::Int(x), Value::Int(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Int.mod: both arguments must be Int".to_string(),
        ));
    };
    match x.rem_euclid(y) {
        Some(r) => Ok(Value::Ok(Box::new(Value::Int(r)))),
        None => Ok(Value::Err(Box::new(Value::Str(
            "division by zero".to_string(),
        )))),
    }
}

fn divide(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("Int.div", args)?;
    let (Value::Int(x), Value::Int(y)) = (a, b) else {
        return Err(RuntimeError::Error(
            "Int.div: both arguments must be Int".to_string(),
        ));
    };
    // Euclidean (flooring) division, so `Int.div` is the exact partner of
    // `Int.mod` (Euclidean): `div(a,b)*b + mod(a,b) == a` for all signs.
    // Over ℤ the only partiality left is divisor-zero — the `i64::MIN / -1`
    // overflow that used to error is just `i64::MAX + 1`, a valid `Result.Ok`.
    match x.div_euclid(y) {
        Some(q) => Ok(Value::Ok(Box::new(Value::Int(q)))),
        None => Ok(Value::Err(Box::new(Value::Str(
            "division by zero".to_string(),
        )))),
    }
}

/// Truncate a finite `f64` toward zero into ℤ. Matches the runtime cast
/// semantics (`f as i64`) for in-range values, but does not clamp huge
/// finite magnitudes to `i64::MAX`/`MIN` — ℤ represents them exactly. NaN and
/// ±∞ map to 0 (there is no integer for them; the cast already returns 0).
pub(crate) fn float_to_aver_int(f: f64) -> AverInt {
    use num_bigint::BigInt;
    use num_traits::FromPrimitive;
    use num_traits::cast::ToPrimitive;
    if !f.is_finite() {
        return AverInt::zero();
    }
    let truncated = f.trunc();
    if let Some(n) = truncated.to_i64() {
        AverInt::from_i64(n)
    } else {
        // Out of i64 range but finite: represent exactly via BigInt.
        match BigInt::from_f64(truncated) {
            Some(b) => AverInt::from_str(&b.to_string()).unwrap_or_else(|_| AverInt::zero()),
            None => AverInt::zero(),
        }
    }
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
    let methods = &["fromString", "fromFloat", "abs", "min", "max", "mod", "div"];
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
        "Int.abs" => Some(abs_nv(args, arena)),
        "Int.min" => Some(min_nv(args, arena)),
        "Int.max" => Some(max_nv(args, arena)),
        "Int.mod" => Some(modulo_nv(args, arena)),
        "Int.div" => Some(divide_nv(args, arena)),
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
    // `Int` is mathematical ℤ, so parsing is unbounded.
    let parsed = AverInt::from_str(&arena.get_string_value(v));
    match parsed {
        Ok(n) => {
            let inner = NanValue::from_aver_int(n, arena);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        Err(_) => {
            let msg = format!("Cannot parse '{}' as Int", arena.get_string_value(v));
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
    Ok(NanValue::from_aver_int(
        float_to_aver_int(v.as_float()),
        arena,
    ))
}

fn abs_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Int.abs", args)?;
    if !v.is_int() {
        return Err(RuntimeError::Error(
            "Int.abs: argument must be an Int".to_string(),
        ));
    }
    let r = v.as_aver_int(arena).abs();
    Ok(NanValue::from_aver_int(r, arena))
}

fn min_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Int.min", args)?;
    if !a.is_int() || !b.is_int() {
        return Err(RuntimeError::Error(
            "Int.min: both arguments must be Int".to_string(),
        ));
    }
    let r = a.as_aver_int(arena).min_ref(&b.as_aver_int(arena));
    Ok(NanValue::from_aver_int(r, arena))
}

fn max_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Int.max", args)?;
    if !a.is_int() || !b.is_int() {
        return Err(RuntimeError::Error(
            "Int.max: both arguments must be Int".to_string(),
        ));
    }
    let r = a.as_aver_int(arena).max_ref(&b.as_aver_int(arena));
    Ok(NanValue::from_aver_int(r, arena))
}

fn modulo_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Int.mod", args)?;
    if !a.is_int() || !b.is_int() {
        return Err(RuntimeError::Error(
            "Int.mod: both arguments must be Int".to_string(),
        ));
    }
    let x = a.as_aver_int(arena);
    let y = b.as_aver_int(arena);
    match x.rem_euclid(&y) {
        Some(r) => {
            let inner = NanValue::from_aver_int(r, arena);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        None => {
            let inner = NanValue::new_string_value("division by zero", arena);
            Ok(NanValue::new_err_value(inner, arena))
        }
    }
}

fn divide_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Int.div", args)?;
    if !a.is_int() || !b.is_int() {
        return Err(RuntimeError::Error(
            "Int.div: both arguments must be Int".to_string(),
        ));
    }
    let x = a.as_aver_int(arena);
    let y = b.as_aver_int(arena);
    // Euclidean division (partner of Euclidean `Int.mod`). Over ℤ the only
    // remaining partiality is divisor-zero; the old `i64::MIN / -1` overflow
    // is now just a valid (large) `Result.Ok`.
    match x.div_euclid(&y) {
        Some(q) => {
            let inner = NanValue::from_aver_int(q, arena);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        None => {
            let inner = NanValue::new_string_value("division by zero", arena);
            Ok(NanValue::new_err_value(inner, arena))
        }
    }
}
