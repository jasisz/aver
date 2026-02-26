/// HttpServer service namespace.
///
/// Runtime implementation of `HttpServer.listen` lives in the interpreter
/// because it must execute Aver callbacks.
use std::collections::HashMap;

use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    members.insert(
        "listen".to_string(),
        Value::Builtin("HttpServer.listen".to_string()),
    );
    members.insert(
        "listenWith".to_string(),
        Value::Builtin("HttpServer.listenWith".to_string()),
    );
    global.insert(
        "HttpServer".to_string(),
        Value::Namespace {
            name: "HttpServer".to_string(),
            members,
        },
    );
}

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "HttpServer.listen" | "HttpServer.listenWith" => &["HttpServer"],
        _ => &[],
    }
}

/// Always returns `None` — `HttpServer.listen` is executed by `Interpreter`
/// so it can invoke user-defined callbacks.
pub fn call(_name: &str, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    None
}
