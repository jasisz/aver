/// Terminal service — raw mode, cursor, color, key input.
///
/// Methods:
///   Terminal.enableRawMode()   — enter raw mode
///   Terminal.disableRawMode()  — leave raw mode
///   Terminal.clear()           — clear screen
///   Terminal.moveTo(x, y)      — move cursor to column x, row y
///   Terminal.print(s)          — print string at cursor (no newline)
///   Terminal.setColor(color)   — set foreground color
///   Terminal.resetColor()      — reset colors to default
///   Terminal.readKey()         — non-blocking key poll → Option<String>
///   Terminal.size()            — terminal size → Terminal.Size { width, height }
///   Terminal.hideCursor()      — hide cursor
///   Terminal.showCursor()      — show cursor
///   Terminal.flush()           — flush stdout
///
/// Effects are granular per method.
use std::collections::HashMap;

use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "enableRawMode",
        "disableRawMode",
        "clear",
        "moveTo",
        "print",
        "setColor",
        "resetColor",
        "readKey",
        "size",
        "hideCursor",
        "showCursor",
        "flush",
    ] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Terminal.{}", method)),
        );
    }
    global.insert(
        "Terminal".to_string(),
        Value::Namespace {
            name: "Terminal".to_string(),
            members,
        },
    );
}

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Terminal.enableRawMode" => &["Terminal.enableRawMode"],
        "Terminal.disableRawMode" => &["Terminal.disableRawMode"],
        "Terminal.clear" => &["Terminal.clear"],
        "Terminal.moveTo" => &["Terminal.moveTo"],
        "Terminal.print" => &["Terminal.print"],
        "Terminal.setColor" => &["Terminal.setColor"],
        "Terminal.resetColor" => &["Terminal.resetColor"],
        "Terminal.readKey" => &["Terminal.readKey"],
        "Terminal.size" => &["Terminal.size"],
        "Terminal.hideCursor" => &["Terminal.hideCursor"],
        "Terminal.showCursor" => &["Terminal.showCursor"],
        "Terminal.flush" => &["Terminal.flush"],
        _ => &[],
    }
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Terminal.enableRawMode" => Some(no_args(name, args, || {
            aver_rt::terminal_enable_raw_mode().map(|()| Value::Unit)
        })),
        "Terminal.disableRawMode" => Some(no_args(name, args, || {
            aver_rt::terminal_disable_raw_mode().map(|()| Value::Unit)
        })),
        "Terminal.clear" => Some(no_args(name, args, || {
            aver_rt::terminal_clear().map(|()| Value::Unit)
        })),
        "Terminal.moveTo" => Some(move_to(args)),
        "Terminal.print" => Some(print(args)),
        "Terminal.setColor" => Some(set_color(args)),
        "Terminal.resetColor" => Some(no_args(name, args, || {
            aver_rt::terminal_reset_color().map(|()| Value::Unit)
        })),
        "Terminal.readKey" => Some(no_args(name, args, || {
            Ok(match aver_rt::terminal_read_key() {
                Some(key) => Value::Some(Box::new(Value::Str(key))),
                None => Value::None,
            })
        })),
        "Terminal.size" => Some(no_args(name, args, || {
            aver_rt::terminal_size().map(|(w, h)| Value::Record {
                type_name: "Terminal.Size".to_string(),
                fields: vec![
                    ("width".to_string(), Value::Int(w)),
                    ("height".to_string(), Value::Int(h)),
                ]
                .into(),
            })
        })),
        "Terminal.hideCursor" => Some(no_args(name, args, || {
            aver_rt::terminal_hide_cursor().map(|()| Value::Unit)
        })),
        "Terminal.showCursor" => Some(no_args(name, args, || {
            aver_rt::terminal_show_cursor().map(|()| Value::Unit)
        })),
        "Terminal.flush" => Some(no_args(name, args, || {
            aver_rt::terminal_flush().map(|()| Value::Unit)
        })),
        _ => None,
    }
}

fn no_args<F>(name: &str, args: &[Value], f: F) -> Result<Value, RuntimeError>
where
    F: FnOnce() -> Result<Value, String>,
{
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "{}() takes 0 arguments, got {}",
            name,
            args.len()
        )));
    }
    f().map_err(RuntimeError::Error)
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
    aver_rt::terminal_move_to(*x, *y).map_err(RuntimeError::Error)?;
    Ok(Value::Unit)
}

fn print(args: &[Value]) -> Result<Value, RuntimeError> {
    let [s_val] = args else {
        return Err(RuntimeError::Error(format!(
            "Terminal.print() takes 1 argument, got {}",
            args.len()
        )));
    };
    let s = crate::value::aver_display(s_val).unwrap_or_default();
    aver_rt::terminal_print(&s).map_err(RuntimeError::Error)?;
    Ok(Value::Unit)
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
    aver_rt::terminal_set_color(color).map_err(RuntimeError::Error)?;
    Ok(Value::Unit)
}
