/// Bool namespace — logical combinators.
///
/// Methods:
///   Bool.or(a, b)   → Bool  — logical OR
///   Bool.and(a, b)  → Bool  — logical AND
///   Bool.not(a)     → Bool  — logical NOT
///
/// No effects required.
use crate::nan_value::{Arena, NanValue};
use crate::value::RuntimeError;

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    _arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Bool.or" => Some(bool_or_nv(args)),
        "Bool.and" => Some(bool_and_nv(args)),
        "Bool.not" => Some(bool_not_nv(args)),
        _ => None,
    }
}

fn bool_or_nv(args: &[NanValue]) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Bool.or() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.or: first argument must be a Bool".to_string(),
        ));
    }
    if !args[1].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.or: second argument must be a Bool".to_string(),
        ));
    }
    Ok(NanValue::new_bool(args[0].as_bool() || args[1].as_bool()))
}

fn bool_and_nv(args: &[NanValue]) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Bool.and() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.and: first argument must be a Bool".to_string(),
        ));
    }
    if !args[1].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.and: second argument must be a Bool".to_string(),
        ));
    }
    Ok(NanValue::new_bool(args[0].as_bool() && args[1].as_bool()))
}

fn bool_not_nv(args: &[NanValue]) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Bool.not() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_bool() {
        return Err(RuntimeError::Error(
            "Bool.not: argument must be a Bool".to_string(),
        ));
    }
    Ok(NanValue::new_bool(!args[0].as_bool()))
}
