/// Result namespace — combinators for Result<T, E>.
///
/// Methods:
///   Result.withDefault(result, default) → T  — unwrap Ok or return default
///
/// Constructors (Ok, Err) are registered separately in interpreter/core.rs.
/// No effects required.
use std::collections::HashMap;

use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    // Result namespace already exists (created in interpreter/core.rs for Ok/Err ctors).
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
