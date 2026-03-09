use std::collections::HashMap;

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
