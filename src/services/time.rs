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
    Ok(Value::Int(aver_rt::time_unix_ms()))
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
    if *ms < 0 {
        return Err(RuntimeError::Error(
            "Time.sleep: ms must be non-negative".to_string(),
        ));
    }
    aver_rt::time_sleep(*ms);
    Ok(Value::Unit)
}
