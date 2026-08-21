/// Result namespace — combinators for Result<T, E>.
///
/// Methods:
///   Result.withDefault(result, default) → T  — unwrap Ok or return default
///
/// Constructors (Ok, Err) are registered separately in vm/runtime.rs.
/// No effects required.
use crate::nan_value::{Arena, NanValue};
use crate::value::RuntimeError;

/// Members to merge into the existing Result namespace.
pub fn extra_members() -> Vec<(&'static str, String)> {
    vec![("withDefault", "Result.withDefault".to_string())]
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
