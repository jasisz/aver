/// Console service — wraps the built-in `print` function as a named namespace.
///
/// Registers `Console.print` so Aver code can call it as `Console.print(msg)`.
/// The plain `print(msg)` top-level function is kept for ergonomics and shares
/// the same `"Console"` effect requirement.
use std::collections::HashMap;

use crate::value::{aver_display, RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    members.insert("print".to_string(), Value::Builtin("Console.print".to_string()));
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
        "print" | "Console.print" => &["Console"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: Vec<Value>) -> Option<Result<Value, RuntimeError>> {
    match name {
        "print" | "Console.print" => Some((|| {
            if args.len() != 1 {
                return Err(RuntimeError::Error(format!(
                    "print() takes 1 argument, got {}",
                    args.len()
                )));
            }
            if let Some(s) = aver_display(&args[0]) {
                println!("{}", s);
            }
            Ok(Value::Unit)
        })()),
        _ => None,
    }
}
