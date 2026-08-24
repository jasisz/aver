/// Result namespace — combinators for Result<T, E>.
///
/// Methods:
///   Result.withDefault(result, default) → T  — unwrap Ok or return default
///   Result.fromOption(option, err)       → Result<T, E> — convert Option to Result
///
/// Constructors (Ok, Err) are registered separately in vm/runtime.rs.
/// No effects required.
use crate::nan_value::{Arena, NanValue};
use crate::value::RuntimeError;

/// Members to merge into the existing Result namespace.
pub fn extra_members() -> Vec<(&'static str, String)> {
    vec![
        ("withDefault", "Result.withDefault".to_string()),
        ("fromOption", "Result.fromOption".to_string()),
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
        "Result.withDefault" => Some(with_default_nv(args, arena)),
        "Result.fromOption" => Some(from_option_nv(args, arena)),
        _ => None,
    }
}

fn from_option_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Result.fromOption() takes 2 arguments (option, err), got {}",
            args.len()
        )));
    }
    let option = args[0];
    if option.is_some() {
        Ok(NanValue::new_ok_value(option.wrapper_inner(arena), arena))
    } else if option.is_none() {
        Ok(NanValue::new_err_value(args[1], arena))
    } else {
        Err(RuntimeError::Error(
            "Result.fromOption: first argument must be an Option".to_string(),
        ))
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
