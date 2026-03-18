/// Option namespace — combinators for Option<T>.
///
/// Methods:
///   Option.withDefault(option, default) → T              — unwrap Some or return default
///   Option.toResult(option, err)        → Result<T, E>   — convert Option to Result
///
/// Constructors (Some, None) are registered separately in interpreter/core.rs.
/// No effects required.
use std::collections::HashMap;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    // Option namespace already exists (created in interpreter/core.rs for Some/None).
    // We merge our members into it via a separate step in core.rs.
    let _ = global;
}

/// Members to merge into the existing Option namespace.
pub fn extra_members() -> Vec<(&'static str, String)> {
    vec![
        ("withDefault", "Option.withDefault".to_string()),
        ("toResult", "Option.toResult".to_string()),
    ]
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Option.withDefault" => Some(with_default(args)),
        "Option.toResult" => Some(to_result(args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn with_default(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Option.withDefault() takes 2 arguments (option, default), got {}",
            args.len()
        )));
    }
    match &args[0] {
        Value::Some(v) => Ok(*v.clone()),
        Value::None => Ok(args[1].clone()),
        _ => Err(RuntimeError::Error(
            "Option.withDefault: first argument must be an Option".to_string(),
        )),
    }
}

fn to_result(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Option.toResult() takes 2 arguments (option, err), got {}",
            args.len()
        )));
    }
    match &args[0] {
        Value::Some(v) => Ok(Value::Ok(v.clone())),
        Value::None => Ok(Value::Err(Box::new(args[1].clone()))),
        _ => Err(RuntimeError::Error(
            "Option.toResult: first argument must be an Option".to_string(),
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
        "Option.withDefault" => Some(with_default_nv(args, arena)),
        "Option.toResult" => Some(to_result_nv(args, arena)),
        _ => None,
    }
}

fn with_default_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Option.withDefault() takes 2 arguments (option, default), got {}",
            args.len()
        )));
    }
    let v = args[0];
    if v.is_some() {
        Ok(arena.get_boxed(v.wrapper_index()))
    } else if v.is_none() {
        Ok(args[1])
    } else {
        Err(RuntimeError::Error(
            "Option.withDefault: first argument must be an Option".to_string(),
        ))
    }
}

fn to_result_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Option.toResult() takes 2 arguments (option, err), got {}",
            args.len()
        )));
    }
    let v = args[0];
    if v.is_some() {
        // Some(inner) -> Ok(inner) — reuse the same boxed index
        Ok(NanValue::new_ok(v.wrapper_index()))
    } else if v.is_none() {
        let box_idx = arena.push_boxed(args[1]);
        Ok(NanValue::new_err(box_idx))
    } else {
        Err(RuntimeError::Error(
            "Option.toResult: first argument must be an Option".to_string(),
        ))
    }
}
