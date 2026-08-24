/// Unicode scalar-value operations owned by the String namespace.
///
/// Methods:
///   String.firstCodePoint(s: String) → Option<Int>    — first Unicode scalar value
///   String.fromCodePoint(n: Int)     → Option<String> — code point to 1-char string
///
/// No effects required.
use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::value::RuntimeError;

// ─── Implementations ────────────────────────────────────────────────────────

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "String.firstCodePoint" => Some(first_code_point_nv(args, arena)),
        "String.fromCodePoint" => Some(from_code_point_nv(args, arena)),
        _ => None,
    }
}

fn first_code_point_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.firstCodePoint() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.firstCodePoint: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]);
    match s.chars().next() {
        Some(c) => {
            let code = NanValue::new_int(c as i64, arena);
            Ok(NanValue::new_some_value(code, arena))
        }
        None => Ok(NanValue::NONE),
    }
}

fn from_code_point_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.fromCodePoint() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "String.fromCodePoint: argument must be an Int".to_string(),
        ));
    }
    let Some(code) = args[0].as_aver_int(arena).to_u32() else {
        return Ok(NanValue::NONE);
    };
    match char::from_u32(code) {
        Some(c) => {
            let s = c.to_string();
            let inner = NanValue::new_string_value(&s, arena);
            Ok(NanValue::new_some_value(inner, arena))
        }
        None => Ok(NanValue::NONE),
    }
}
