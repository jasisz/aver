/// Env service — environment variables.
///
/// Methods:
///   Env.get(key)        — returns Option<String>
///   Env.set(key, value) — sets process env var, returns Result<Unit,String>
///
/// Effects are granular:
/// - Env.get
/// - Env.set
use crate::nan_value::{Arena, NanValue};
use crate::value::RuntimeError;

pub const DECLARED_EFFECTS: &[&str] = &["Env.get", "Env.set"];

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Env.get" => &["Env.get"],
        "Env.set" => &["Env.set"],
        _ => &[],
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Env.get" => Some(get_nv(args, arena)),
        "Env.set" => Some(set_nv(args, arena)),
        _ => None,
    }
}

fn get_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Env.get() takes 1 argument (key), got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "Env.get: key must be a String".to_string(),
        ));
    }
    let key = arena.get_string_value(args[0]).to_string();
    match aver_rt::env_get(&key) {
        Some(v) => {
            let inner = NanValue::new_string_value(&v, arena);
            Ok(NanValue::new_some_value(inner, arena))
        }
        None => Ok(NanValue::NONE),
    }
}

fn set_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Env.set() takes 2 arguments (key, value), got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "Env.set: key must be a String".to_string(),
        ));
    }
    if !args[1].is_string() {
        return Err(RuntimeError::Error(
            "Env.set: value must be a String".to_string(),
        ));
    }
    let key = arena.get_string_value(args[0]).to_string();
    let value = arena.get_string_value(args[1]).to_string();
    Ok(match aver_rt::env_set(&key, &value) {
        Ok(()) => NanValue::new_ok_value(NanValue::UNIT, arena),
        Err(message) => {
            let error = NanValue::new_string_value(&message, arena);
            NanValue::new_err_value(error, arena)
        }
    })
}
