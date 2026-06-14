/// Time service — wall clock and sleeping.
///
/// Methods:
///   Time.now()      — current UTC timestamp as RFC3339-like string
///   Time.unixMs()   — unix epoch milliseconds as Int
///   Time.sleep(ms)  — sleep current thread for `ms` milliseconds
///
/// Effects are granular:
/// - Time.now
/// - Time.unixMs
/// - Time.sleep
use std::collections::HashMap;
use std::sync::Arc as Rc;

use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["now", "unixMs", "sleep"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Time.{}", method)),
        );
    }
    global.insert(
        "Time".to_string(),
        Value::Namespace {
            name: "Time".to_string(),
            members,
        },
    );
}

pub const DECLARED_EFFECTS: &[&str] = &["Time.now", "Time.unixMs", "Time.sleep"];

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Time.now" => &["Time.now"],
        "Time.unixMs" => &["Time.unixMs"],
        "Time.sleep" => &["Time.sleep"],
        _ => &[],
    }
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Time.now" => Some(now(args)),
        "Time.unixMs" => Some(unix_ms(args)),
        "Time.sleep" => Some(sleep(args)),
        _ => None,
    }
}

fn now(args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Time.now() takes 0 arguments, got {}",
            args.len()
        )));
    }
    Ok(Value::Str(aver_rt::time_now()))
}

fn unix_ms(args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Time.unixMs() takes 0 arguments, got {}",
            args.len()
        )));
    }
    Ok(Value::int(aver_rt::time_unix_ms()))
}

fn sleep(args: &[Value]) -> Result<Value, RuntimeError> {
    let [ms] = args else {
        return Err(RuntimeError::Error(format!(
            "Time.sleep() takes 1 argument (ms), got {}",
            args.len()
        )));
    };
    let Value::Int(ms) = ms else {
        return Err(RuntimeError::Error(
            "Time.sleep: ms must be an Int".to_string(),
        ));
    };
    let Some(ms) = ms.to_i64() else {
        return Err(RuntimeError::Error(
            "Time.sleep: ms must fit a 64-bit integer".to_string(),
        ));
    };
    if ms < 0 {
        return Err(RuntimeError::Error(
            "Time.sleep: ms must be non-negative".to_string(),
        ));
    }
    aver_rt::time_sleep(ms);
    Ok(Value::Unit)
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &["now", "unixMs", "sleep"];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Time.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Time"),
        members,
    });
    global.insert("Time".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Time.now" => Some(now_nv(args, arena)),
        "Time.unixMs" => Some(unix_ms_nv(args, arena)),
        "Time.sleep" => Some(sleep_nv(args, arena)),
        _ => None,
    }
}

fn now_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Time.now() takes 0 arguments, got {}",
            args.len()
        )));
    }
    let s = aver_rt::time_now();
    Ok(NanValue::new_string_value(&s, arena))
}

fn unix_ms_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Time.unixMs() takes 0 arguments, got {}",
            args.len()
        )));
    }
    Ok(NanValue::new_int(aver_rt::time_unix_ms(), arena))
}

fn sleep_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Time.sleep() takes 1 argument (ms), got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "Time.sleep: ms must be an Int".to_string(),
        ));
    }
    let Some(ms) = args[0].as_aver_int(arena).to_i64() else {
        return Err(RuntimeError::Error(
            "Time.sleep: ms must fit a 64-bit integer".to_string(),
        ));
    };
    if ms < 0 {
        return Err(RuntimeError::Error(
            "Time.sleep: ms must be non-negative".to_string(),
        ));
    }
    aver_rt::time_sleep(ms);
    Ok(NanValue::UNIT)
}
