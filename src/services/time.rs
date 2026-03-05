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
use std::time::{Duration, SystemTime, UNIX_EPOCH};

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
    let (secs, nanos) = unix_parts_now()?;
    let ts = format_utc_rfc3339_like(secs, nanos);
    Ok(Value::Str(ts))
}

fn unix_ms(args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Time.unixMs() takes 0 arguments, got {}",
            args.len()
        )));
    }
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| RuntimeError::Error(format!("Time.unixMs: system clock error: {}", e)))?;
    let ms: i64 = now
        .as_millis()
        .try_into()
        .map_err(|_| RuntimeError::Error("Time.unixMs: value out of Int range".to_string()))?;
    Ok(Value::Int(ms))
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
    let ms_u64: u64 = (*ms)
        .try_into()
        .map_err(|_| RuntimeError::Error("Time.sleep: ms out of range".to_string()))?;
    std::thread::sleep(Duration::from_millis(ms_u64));
    Ok(Value::Unit)
}

fn unix_parts_now() -> Result<(i64, u32), RuntimeError> {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => {
            let secs: i64 = d
                .as_secs()
                .try_into()
                .map_err(|_| RuntimeError::Error("Time.now: seconds out of range".to_string()))?;
            Ok((secs, d.subsec_nanos()))
        }
        Err(e) => {
            // Clock before UNIX_EPOCH: represent as negative unix seconds.
            let d = e.duration();
            let secs: i64 = d
                .as_secs()
                .try_into()
                .map_err(|_| RuntimeError::Error("Time.now: seconds out of range".to_string()))?;
            let nanos = d.subsec_nanos();
            if nanos == 0 {
                Ok((-secs, 0))
            } else {
                // -(secs + nanos) = -(secs+1) + (1e9 - nanos)
                Ok((-(secs + 1), 1_000_000_000 - nanos))
            }
        }
    }
}

fn format_utc_rfc3339_like(unix_secs: i64, nanos: u32) -> String {
    let days = unix_secs.div_euclid(86_400);
    let sod = unix_secs.rem_euclid(86_400);
    let hour = sod / 3_600;
    let minute = (sod % 3_600) / 60;
    let second = sod % 60;
    let millis = nanos / 1_000_000;

    let (year, month, day) = civil_from_days(days);
    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
        year, month, day, hour, minute, second, millis
    )
}

/// Convert days since unix epoch (1970-01-01) to Gregorian Y-M-D.
/// Algorithm adapted from Howard Hinnant's civil date conversion.
fn civil_from_days(days_since_epoch: i64) -> (i32, u32, u32) {
    let z = days_since_epoch + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097; // [0, 146096]
    let yoe = (doe - doe / 1_460 + doe / 36_524 - doe / 146_096) / 365; // [0,399]
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100); // [0,365]
    let mp = (5 * doy + 2) / 153; // [0,11]
    let day = doy - (153 * mp + 2) / 5 + 1; // [1,31]
    let month = mp + if mp < 10 { 3 } else { -9 }; // [1,12]
    let year = y + if month <= 2 { 1 } else { 0 };
    (year as i32, month as u32, day as u32)
}
