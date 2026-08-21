/// Option namespace — combinators for Option<T>.
///
/// Methods:
///   Option.withDefault(option, default) → T              — unwrap Some or return default
///   Option.toResult(option, err)        → Result<T, E>   — convert Option to Result
///
/// Constructors (Some, None) are registered separately in vm/runtime.rs.
/// No effects required.
use crate::nan_value::{Arena, NanValue};
use crate::value::RuntimeError;

/// Members to merge into the existing Option namespace.
pub fn extra_members() -> Vec<(&'static str, String)> {
    vec![
        ("withDefault", "Option.withDefault".to_string()),
        ("toResult", "Option.toResult".to_string()),
    ]
}

// ─── Implementations ────────────────────────────────────────────────────────

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
        Ok(v.wrapper_inner(arena))
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
        let inner = v.wrapper_inner(arena);
        Ok(NanValue::new_ok_value(inner, arena))
    } else if v.is_none() {
        Ok(NanValue::new_err_value(args[1], arena))
    } else {
        Err(RuntimeError::Error(
            "Option.toResult: first argument must be an Option".to_string(),
        ))
    }
}
