/// Int namespace — numeric helpers for integer values.
///
/// Methods:
///   Int.fromString(s)   → Result<Int, String>  — parse string to int
///   Int.fromFloat(f)    → Int                  — truncate float to int
///   Int.abs(n)          → Int                  — absolute value
///   Int.min(a, b)       → Int                  — minimum of two ints
///   Int.max(a, b)       → Int                  — maximum of two ints
///   Int.mod(a, b)       → Result<Int, String>  — Euclidean modulo: the result
///                                                  is always in [0, |b|), i.e.
///                                                  non-negative for every sign
///                                                  of `a` and `b` (mod(7,-2) ==
///                                                  Ok(1)). Errors on b == 0.
///   Int.div(a, b)       → Result<Int, String>  — Euclidean integer division,
///                                                  the unique q with a == q*b +
///                                                  r, 0 <= r < |b| (div(-7,2) ==
///                                                  Ok(-4)). Errors on b == 0.
///
/// When the divisor is a syntactic nonzero integer literal, the typechecker
/// discharges `Int.div` / `Int.mod` to plain `Int` and the compiler emits the
/// division directly (see `is_literal_nonzero_int_divisor` in `src/ast`).
///
/// Stringification goes through `String.fromInt` (or `"{n}"` interpolation);
/// widening to Float goes through `Float.fromInt`.
///
/// No effects required.
use std::str::FromStr;

use aver_rt::AverInt;

use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::value::RuntimeError;

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

// ─── NanValue-native API ─────────────────────────────────────────────────────

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
