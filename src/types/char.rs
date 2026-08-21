/// Char namespace — Unicode scalar value operations on strings.
///
/// Char is NOT a type — these are functions operating on String (first character)
/// and Int (code point).
///
/// Methods:
///   Char.toCode(s: String)    → Int             — Unicode scalar value of first char
///   Char.fromCode(n: Int)     → Option<String>  — code point to 1-char string
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
        "Char.toCode" => Some(to_code_nv(args, arena)),
        "Char.fromCode" => Some(from_code_nv(args, arena)),
        _ => None,
    }
}

fn to_code_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Char.toCode() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "Char.toCode: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]);
    match s.chars().next() {
        Some(c) => Ok(NanValue::new_int(c as i64, arena)),
        None => Err(RuntimeError::Error(
            "Char.toCode: string is empty".to_string(),
        )),
    }
}

fn from_code_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Char.fromCode() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "Char.fromCode: argument must be an Int".to_string(),
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
