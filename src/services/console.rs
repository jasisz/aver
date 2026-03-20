/// Console service — terminal I/O.
///
/// Methods:
///   Console.print(msg)    — print to stdout (same as top-level `print`)
///   Console.error(msg)    — print to stderr (no colour, raw message)
///   Console.warn(msg)     — print to stderr prefixed with "[warn] "
///   Console.readLine()    — read one line from stdin; Ok(line) or Err("EOF")
///
/// Each method requires its own exact effect (`Console.print`, `Console.error`, etc.).
use std::collections::HashMap;
use std::rc::Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value, aver_display};

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
        "Console.print" => &["Console.print"],
        "Console.error" => &["Console.error"],
        "Console.warn" => &["Console.warn"],
        "Console.readLine" => &["Console.readLine"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Console.print" => Some(one_msg(name, args, |s| {
            println!("{}", s);
        })),
        "Console.error" => Some(one_msg(name, args, |s| {
            eprintln!("{}", s);
        })),
        "Console.warn" => Some(one_msg(name, args, |s| {
            eprintln!("[warn] {}", s);
        })),
        "Console.readLine" => Some(read_line(args)),
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
    match aver_rt::read_line() {
        Ok(line) => Ok(Value::Ok(Box::new(Value::Str(line)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &["print", "error", "warn", "readLine"];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Console.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Console"),
        members,
    });
    global.insert("Console".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Console.print" => Some(one_msg_nv(name, args, arena, |s| {
            println!("{}", s);
        })),
        "Console.error" => Some(one_msg_nv(name, args, arena, |s| {
            eprintln!("{}", s);
        })),
        "Console.warn" => Some(one_msg_nv(name, args, arena, |s| {
            eprintln!("[warn] {}", s);
        })),
        "Console.readLine" => Some(read_line_nv(args, arena)),
        _ => None,
    }
}

fn one_msg_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
    emit: impl Fn(&str),
) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument, got {}",
            name,
            args.len()
        )));
    }
    if let Some(s) = args[0].display(arena) {
        emit(&s);
    }
    Ok(NanValue::UNIT)
}

fn read_line_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Console.readLine() takes 0 arguments, got {}",
            args.len()
        )));
    }
    match aver_rt::read_line() {
        Ok(line) => {
            let s_idx = arena.push_string(&line);
            let inner = NanValue::new_string(s_idx);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        Err(e) => {
            let s_idx = arena.push_string(&e);
            let inner = NanValue::new_string(s_idx);
            Ok(NanValue::new_err_value(inner, arena))
        }
    }
}
