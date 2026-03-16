/// Random service — effectful random number generation.
///
/// Methods:
///   Random.int(min, max)  — random Int in [min, max] inclusive; ! [Random]
///   Random.float()        — random Float in [0.0, 1.0); ! [Random]
///
/// Backed by `aver_rt::random` (OS entropy via `rand` crate).
use std::collections::HashMap;

use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["int", "float"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Random.{}", method)),
        );
    }
    global.insert(
        "Random".to_string(),
        Value::Namespace {
            name: "Random".to_string(),
            members,
        },
    );
}

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Random.int" => &["Random.int"],
        "Random.float" => &["Random.float"],
        _ => &[],
    }
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Random.int" => Some(random_int(args)),
        "Random.float" => Some(random_float(args)),
        _ => None,
    }
}

fn random_int(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Random.int takes 2 arguments (min, max), got {}",
            args.len()
        )));
    }
    let Value::Int(min) = &args[0] else {
        return Err(RuntimeError::Error(
            "Random.int: first argument must be an Int".to_string(),
        ));
    };
    let Value::Int(max) = &args[1] else {
        return Err(RuntimeError::Error(
            "Random.int: second argument must be an Int".to_string(),
        ));
    };

    match aver_rt::random::random_int(*min, *max) {
        Ok(value) => Ok(Value::Int(value)),
        Err(msg) => Err(RuntimeError::Error(msg)),
    }
}

fn random_float(args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Random.float takes 0 arguments, got {}",
            args.len()
        )));
    }
    Ok(Value::Float(aver_rt::random::random_float()))
}
