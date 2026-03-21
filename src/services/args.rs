use std::collections::HashMap;
use std::rc::Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value};
use aver_rt::AverList;

/// Args service — command-line arguments.
///
/// Methods:
///   Args.get() → List<String>   ! [Args.get]
pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    members.insert("get".to_string(), Value::Builtin("Args.get".to_string()));
    global.insert(
        "Args".to_string(),
        Value::Namespace {
            name: "Args".to_string(),
            members,
        },
    );
}

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Args.get" => &["Args.get"],
        _ => &[],
    }
}

pub fn call(
    name: &str,
    args: &[Value],
    cli_args: &[String],
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Args.get" => Some(get_args(args, cli_args)),
        _ => None,
    }
}

fn get_args(args: &[Value], cli_args: &[String]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Args.get() takes 0 arguments, got {}",
            args.len()
        )));
    }
    let list_vals: Vec<Value> = cli_args.iter().map(|s| Value::Str(s.clone())).collect();
    Ok(Value::List(AverList::from_vec(list_vals)))
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let idx = arena.push_builtin("Args.get");
    let members: Vec<(Rc<str>, NanValue)> = vec![(Rc::from("get"), NanValue::new_builtin(idx))];
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Args"),
        members,
    });
    global.insert("Args".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    cli_args: &[String],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Args.get" => Some(get_args_nv(args, cli_args, arena)),
        _ => None,
    }
}

fn get_args_nv(
    args: &[NanValue],
    cli_args: &[String],
    arena: &mut Arena,
) -> Result<NanValue, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Args.get() takes 0 arguments, got {}",
            args.len()
        )));
    }
    let items: Vec<NanValue> = cli_args
        .iter()
        .map(|s| NanValue::new_string_value(s, arena))
        .collect();
    let list_idx = arena.push_list(items);
    Ok(NanValue::new_list(list_idx))
}
