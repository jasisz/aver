/// Terminal service — raw mode, cursor, color, key input.
///
/// Methods:
///   Terminal.enableRawMode()   — enter raw mode → Result<Unit,String>
///   Terminal.disableRawMode()  — leave raw mode → Result<Unit,String>
///   Terminal.clear()           — clear screen → Result<Unit,String>
///   Terminal.moveTo(x, y)      — move cursor → Result<Unit,String>
///   Terminal.print(s)          — print string at cursor → Result<Unit,String>
///   Terminal.setColor(color)   — set foreground color → Result<Unit,String>
///   Terminal.resetColor()      — reset colors → Result<Unit,String>
///   Terminal.readKey()         — non-blocking poll → Result<Option<String>,String>
///   Terminal.size()            — terminal size → Result<Terminal.Size,String>
///   Terminal.hideCursor()      — hide cursor → Result<Unit,String>
///   Terminal.showCursor()      — show cursor → Result<Unit,String>
///   Terminal.flush()           — flush stdout → Result<Unit,String>
///
/// Effects are granular per method.
use crate::nan_value::{Arena, NanValue, NanValueConvert};
use crate::value::{RuntimeError, Value};

pub const DECLARED_EFFECTS: &[&str] = &[
    "Terminal.enableRawMode",
    "Terminal.disableRawMode",
    "Terminal.clear",
    "Terminal.moveTo",
    "Terminal.print",
    "Terminal.setColor",
    "Terminal.resetColor",
    "Terminal.readKey",
    "Terminal.size",
    "Terminal.hideCursor",
    "Terminal.showCursor",
    "Terminal.flush",
];

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Terminal.enableRawMode" => Some(no_args_result(name, args, || {
            aver_rt::terminal_enable_raw_mode()
        })),
        "Terminal.disableRawMode" => Some(no_args_result(name, args, || {
            aver_rt::terminal_disable_raw_mode()
        })),
        "Terminal.clear" => Some(no_args_result(name, args, aver_rt::terminal_clear)),
        "Terminal.moveTo" => Some(move_to(args)),
        "Terminal.print" => Some(print(args)),
        "Terminal.setColor" => Some(set_color(args)),
        "Terminal.resetColor" => Some(no_args_result(name, args, aver_rt::terminal_reset_color)),
        "Terminal.readKey" => Some(read_key(args)),
        "Terminal.size" => Some(size(args)),
        "Terminal.hideCursor" => Some(no_args_result(name, args, aver_rt::terminal_hide_cursor)),
        "Terminal.showCursor" => Some(no_args_result(name, args, aver_rt::terminal_show_cursor)),
        "Terminal.flush" => Some(no_args_result(name, args, aver_rt::terminal_flush)),
        _ => None,
    }
}

fn no_args_result<F>(name: &str, args: &[Value], f: F) -> Result<Value, RuntimeError>
where
    F: FnOnce() -> Result<(), String>,
{
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "{}() takes 0 arguments, got {}",
            name,
            args.len()
        )));
    }
    Ok(unit_result(f()))
}

fn unit_result(result: Result<(), String>) -> Value {
    match result {
        Ok(()) => Value::Ok(Box::new(Value::Unit)),
        Err(message) => Value::Err(Box::new(Value::Str(message))),
    }
}

fn move_to(args: &[Value]) -> Result<Value, RuntimeError> {
    let [x_val, y_val] = args else {
        return Err(RuntimeError::Error(format!(
            "Terminal.moveTo() takes 2 arguments (x, y), got {}",
            args.len()
        )));
    };
    let Value::Int(x) = x_val else {
        return Err(RuntimeError::Error(
            "Terminal.moveTo: x must be an Int".to_string(),
        ));
    };
    let Value::Int(y) = y_val else {
        return Err(RuntimeError::Error(
            "Terminal.moveTo: y must be an Int".to_string(),
        ));
    };
    let (Some(x), Some(y)) = (x.to_i64(), y.to_i64()) else {
        return Ok(Value::Err(Box::new(Value::Str(
            "Terminal.moveTo: coordinates must fit a 64-bit integer".to_string(),
        ))));
    };
    Ok(match aver_rt::terminal_move_to(x, y) {
        Ok(()) => Value::Ok(Box::new(Value::Unit)),
        Err(message) => Value::Err(Box::new(Value::Str(message))),
    })
}

fn size(args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Terminal.size() takes 0 arguments, got {}",
            args.len()
        )));
    }
    Ok(match aver_rt::terminal_size() {
        Ok((w, h)) => Value::Ok(Box::new(Value::Record {
            type_name: "Terminal.Size".to_string(),
            fields: vec![
                ("width".to_string(), Value::int(w)),
                ("height".to_string(), Value::int(h)),
            ]
            .into(),
        })),
        Err(message) => Value::Err(Box::new(Value::Str(message))),
    })
}

fn read_key(args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Terminal.readKey() takes 0 arguments, got {}",
            args.len()
        )));
    }
    Ok(match aver_rt::terminal_read_key() {
        Ok(Some(key)) => Value::Ok(Box::new(Value::Some(Box::new(Value::Str(key))))),
        Ok(None) => Value::Ok(Box::new(Value::None)),
        Err(message) => Value::Err(Box::new(Value::Str(message))),
    })
}

fn print(args: &[Value]) -> Result<Value, RuntimeError> {
    let [s_val] = args else {
        return Err(RuntimeError::Error(format!(
            "Terminal.print() takes 1 argument, got {}",
            args.len()
        )));
    };
    let s = crate::value::aver_display(s_val).unwrap_or_default();
    Ok(unit_result(aver_rt::terminal_print(&s)))
}

fn set_color(args: &[Value]) -> Result<Value, RuntimeError> {
    let [c_val] = args else {
        return Err(RuntimeError::Error(format!(
            "Terminal.setColor() takes 1 argument, got {}",
            args.len()
        )));
    };
    let Value::Str(color) = c_val else {
        return Err(RuntimeError::Error(
            "Terminal.setColor: argument must be a String".to_string(),
        ));
    };
    Ok(unit_result(aver_rt::terminal_set_color(color)))
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

/// Bridge: convert NanValue args to Value, call old implementation, convert result back.
pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    // Check ownership
    if !name.starts_with("Terminal.") {
        return None;
    }
    let old_args: Vec<Value> = args.iter().map(|nv| nv.to_value(arena)).collect();
    let result = call(name, &old_args)?;
    Some(result.map(|v| NanValue::from_value(&v, arena)))
}
