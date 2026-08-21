/// Float namespace — numeric helpers for floating-point values.
///
/// Methods:
///   Float.fromString(s)  → Result<Float, String>  — parse string to float
///   Float.fromInt(n)     → Float                  — widen int to float
///   Float.abs(f)         → Float                  — absolute value
///   Float.floor(f)       → Int                    — floor to int
///   Float.ceil(f)        → Int                    — ceil to int
///   Float.round(f)       → Int                    — round to int
///   Float.min(a, b)      → Float                  — minimum of two floats
///   Float.max(a, b)      → Float                  — maximum of two floats
///   Float.sin(f)         → Float                  — sine (radians)
///   Float.cos(f)         → Float                  — cosine (radians)
///   Float.sqrt(f)        → Float                  — square root
///   Float.pow(base, exp) → Float                  — exponentiation
///   Float.atan2(y, x)    → Float                  — two-argument arctangent
///   Float.pi()           → Float                  — π constant
///
/// No effects required.
use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::value::RuntimeError;

/// Returns `Some(result)` when `name` is owned by this namespace, `None` otherwise.
pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Float.fromString" => Some(from_string_nv(args, arena)),
        "Float.fromInt" => Some(from_int_nv(args, arena)),
        "Float.abs" => Some(abs_nv(args, arena)),
        "Float.floor" => Some(floor_nv(args, arena)),
        "Float.ceil" => Some(ceil_nv(args, arena)),
        "Float.round" => Some(round_nv(args, arena)),
        "Float.min" => Some(min_nv(args, arena)),
        "Float.max" => Some(max_nv(args, arena)),
        "Float.sin" => Some(sin_nv(args, arena)),
        "Float.cos" => Some(cos_nv(args, arena)),
        "Float.sqrt" => Some(sqrt_nv(args, arena)),
        "Float.pow" => Some(pow_nv(args, arena)),
        "Float.atan2" => Some(atan2_nv(args, arena)),
        "Float.pi" => Some(pi_nv(args)),
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
    let v = nv_check1("Float.fromString", args)?;
    if !v.is_string() {
        return Err(RuntimeError::Error(
            "Float.fromString: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string_value(v);
    match s.parse::<f64>() {
        Ok(f) => {
            let inner = NanValue::new_float(f);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        Err(_) => {
            let msg = format!("Cannot parse '{}' as Float", s);
            let inner = NanValue::new_string_value(&msg, arena);
            Ok(NanValue::new_err_value(inner, arena))
        }
    }
}

fn from_int_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.fromInt", args)?;
    if !v.is_int() {
        return Err(RuntimeError::Error(
            "Float.fromInt: argument must be an Int".to_string(),
        ));
    }
    Ok(NanValue::new_float(v.as_aver_int(arena).to_f64()))
}

fn abs_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.abs", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error(
            "Float.abs: argument must be a Float".to_string(),
        ));
    }
    Ok(NanValue::new_float(v.as_float().abs()))
}

fn floor_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.floor", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error(
            "Float.floor: argument must be a Float".to_string(),
        ));
    }
    let r = crate::types::int::float_to_aver_int(v.as_float().floor());
    Ok(NanValue::from_aver_int(r, arena))
}

fn ceil_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.ceil", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error(
            "Float.ceil: argument must be a Float".to_string(),
        ));
    }
    let r = crate::types::int::float_to_aver_int(v.as_float().ceil());
    Ok(NanValue::from_aver_int(r, arena))
}

fn round_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.round", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error(
            "Float.round: argument must be a Float".to_string(),
        ));
    }
    let r = crate::types::int::float_to_aver_int(v.as_float().round());
    Ok(NanValue::from_aver_int(r, arena))
}

fn min_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Float.min", args)?;
    if !a.is_float() || !b.is_float() {
        return Err(RuntimeError::Error(
            "Float.min: both arguments must be Float".to_string(),
        ));
    }
    Ok(NanValue::new_float(f64::min(a.as_float(), b.as_float())))
}

fn max_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Float.max", args)?;
    if !a.is_float() || !b.is_float() {
        return Err(RuntimeError::Error(
            "Float.max: both arguments must be Float".to_string(),
        ));
    }
    Ok(NanValue::new_float(f64::max(a.as_float(), b.as_float())))
}

fn sin_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.sin", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error(
            "Float.sin: argument must be a Float".to_string(),
        ));
    }
    Ok(NanValue::new_float(v.as_float().sin()))
}

fn cos_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.cos", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error(
            "Float.cos: argument must be a Float".to_string(),
        ));
    }
    Ok(NanValue::new_float(v.as_float().cos()))
}

fn sqrt_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let v = nv_check1("Float.sqrt", args)?;
    if !v.is_float() {
        return Err(RuntimeError::Error(
            "Float.sqrt: argument must be a Float".to_string(),
        ));
    }
    Ok(NanValue::new_float(v.as_float().sqrt()))
}

fn pow_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Float.pow", args)?;
    if !a.is_float() || !b.is_float() {
        return Err(RuntimeError::Error(
            "Float.pow: both arguments must be Float".to_string(),
        ));
    }
    Ok(NanValue::new_float(a.as_float().powf(b.as_float())))
}

fn atan2_nv(args: &[NanValue], _arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (a, b) = nv_check2("Float.atan2", args)?;
    if !a.is_float() || !b.is_float() {
        return Err(RuntimeError::Error(
            "Float.atan2: both arguments must be Float".to_string(),
        ));
    }
    Ok(NanValue::new_float(a.as_float().atan2(b.as_float())))
}

fn pi_nv(args: &[NanValue]) -> Result<NanValue, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Float.pi() takes 0 arguments, got {}",
            args.len()
        )));
    }
    Ok(NanValue::new_float(std::f64::consts::PI))
}
