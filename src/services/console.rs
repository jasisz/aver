/// Console service — terminal I/O.
///
/// Methods:
///   Console.print(msg)    — print to stdout (same as top-level `print`)
///   Console.error(msg)    — print to stderr (no colour, raw message)
///   Console.warn(msg)     — print to stderr prefixed with "[warn] "
///   Console.readLine()    — read one line from stdin; Ok(line) or Err("EOF")
///
/// All methods require `! [Console]`.
use std::collections::HashMap;

use crate::value::{aver_display, RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["print", "error", "warn", "readLine"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Console.{}", method)),
        );
    }
    global.insert(
        "Console".to_string(),
        Value::Namespace {
            name: "Console".to_string(),
            members,
        },
    );
}

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Console.print" | "Console.error" | "Console.warn" | "Console.readLine" => &["Console"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Console.print" => Some(one_msg(name, &args, |s| {
            println!("{}", s);
        })),
        "Console.error" => Some(one_msg(name, &args, |s| {
            eprintln!("{}", s);
        })),
        "Console.warn" => Some(one_msg(name, &args, |s| {
            eprintln!("[warn] {}", s);
        })),
        "Console.readLine" => Some(read_line(&args)),
        _ => None,
    }
}

// ─── Private helpers ──────────────────────────────────────────────────────────

fn one_msg(name: &str, args: &[Value], emit: impl Fn(&str)) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument, got {}",
            name,
            args.len()
        )));
    }
    if let Some(s) = aver_display(&args[0]) {
        emit(&s);
    }
    Ok(Value::Unit)
}

fn read_line(args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Console.readLine() takes 0 arguments, got {}",
            args.len()
        )));
    }
    let mut line = String::new();
    match std::io::stdin().read_line(&mut line) {
        Ok(0) => Ok(Value::Err(Box::new(Value::Str("EOF".to_string())))),
        Ok(_) => {
            // Strip trailing newline
            if line.ends_with('\n') {
                line.pop();
            }
            if line.ends_with('\r') {
                line.pop();
            }
            Ok(Value::Ok(Box::new(Value::Str(line))))
        }
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}
