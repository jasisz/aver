/// Result namespace — combinators for Result<T, E>.
///
/// Methods:
///   Result.withDefault(result, default) → T  — unwrap Ok or return default
///
/// Constructors (Ok, Err) are registered separately in vm/runtime.rs.
/// No effects required.
use std::collections::HashMap;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    // Result namespace already exists (created in vm/runtime.rs for Ok/Err ctors).
    // We merge our members into it via a separate step in core.rs.
    // This function registers nothing — see `extra_result_members()`.
    let _ = global;
}

/// Members to merge into the existing Result namespace.
pub fn extra_members() -> Vec<(&'static str, String)> {
    vec![("withDefault", "Result.withDefault".to_string())]
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Result.withDefault" => Some(with_default(args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn with_default(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Result.withDefault() takes 2 arguments (result, default), got {}",
            args.len()
        )));
    }
    match &args[0] {
        Value::Ok(v) => Ok(*v.clone()),
        Value::Err(_) => Ok(args[1].clone()),
        _ => Err(RuntimeError::Error(
            "Result.withDefault: first argument must be a Result".to_string(),
        )),
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Result.withDefault" => Some(with_default_nv(args, arena)),
        _ => None,
    }
}

fn with_default_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Result.withDefault() takes 2 arguments (result, default), got {}",
            args.len()
        )));
    }
    let v = args[0];
    if v.is_ok() {
        Ok(v.wrapper_inner(arena))
    } else if v.is_err() {
        Ok(args[1])
    } else {
        Err(RuntimeError::Error(
            "Result.withDefault: first argument must be a Result".to_string(),
        ))
    }
}
