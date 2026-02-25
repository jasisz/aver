/// Core Aver runtime value type and associated utilities.
///
/// Lives in its own module so both the interpreter and the service
/// implementations (`services::*`) can import it without circular
/// dependencies.
use std::collections::HashMap;
use thiserror::Error;

use crate::ast::{FnBody};

// ---------------------------------------------------------------------------
// RuntimeError
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Runtime error: {0}")]
    Error(String),
    /// Internal signal: `?` operator encountered Err — caught by call_value to
    /// do early return. Never surfaces to the user (type checker prevents
    /// top-level use).
    #[error("Error propagation")]
    ErrProp(Box<Value>),
}

// ---------------------------------------------------------------------------
// Value
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Unit,
    Ok(Box<Value>),
    Err(Box<Value>),
    Some(Box<Value>),
    None,
    List(Vec<Value>),
    Fn {
        name: String,
        params: Vec<(String, String)>,
        effects: Vec<String>,
        body: FnBody,
        closure: HashMap<String, Value>,
    },
    Builtin(String),
    /// User-defined sum type variant, e.g. `Shape.Circle(3.14)`
    Variant {
        type_name: String,
        variant: String,
        fields: Vec<Value>,
    },
    /// User-defined product type (record), e.g. `User(name: "Alice", age: 30)`
    Record {
        type_name: String,
        fields: Vec<(String, Value)>,
    },
    /// Type namespace: `Shape` — provides `Shape.Circle`, `Shape.Rect`, etc.
    Namespace {
        name: String,
        members: HashMap<String, Value>,
    },
}

// ---------------------------------------------------------------------------
// Environment
// ---------------------------------------------------------------------------

/// Scope stack: innermost scope last.
pub type Env = Vec<HashMap<String, Value>>;

// ---------------------------------------------------------------------------
// Display helpers
// ---------------------------------------------------------------------------

/// Human-readable representation of a value (used by `str()` and `:env`).
pub fn aver_repr(val: &Value) -> String {
    match val {
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Str(s) => s.clone(),
        Value::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Value::Unit => "()".to_string(),
        Value::Ok(v) => format!("Ok({})", aver_repr_inner(v)),
        Value::Err(v) => format!("Err({})", aver_repr_inner(v)),
        Value::Some(v) => format!("Some({})", aver_repr_inner(v)),
        Value::None => "None".to_string(),
        Value::List(items) => {
            let parts: Vec<String> = items.iter().map(aver_repr_inner).collect();
            format!("[{}]", parts.join(", "))
        }
        Value::Fn { name, .. } => format!("<fn {}>", name),
        Value::Builtin(name) => format!("<builtin {}>", name),
        Value::Variant { variant, fields, .. } => {
            if fields.is_empty() {
                variant.clone()
            } else {
                let parts: Vec<String> = fields.iter().map(aver_repr_inner).collect();
                format!("{}({})", variant, parts.join(", "))
            }
        }
        Value::Record { type_name, fields } => {
            let parts: Vec<String> = fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, aver_repr_inner(v)))
                .collect();
            format!("{}({})", type_name, parts.join(", "))
        }
        Value::Namespace { name, .. } => format!("<type {}>", name),
    }
}

/// Like `aver_repr` but strings get quoted — used inside constructors and lists.
fn aver_repr_inner(val: &Value) -> String {
    match val {
        Value::Str(s) => format!("\"{}\"", s),
        Value::List(items) => {
            let parts: Vec<String> = items.iter().map(aver_repr_inner).collect();
            format!("[{}]", parts.join(", "))
        }
        other => aver_repr(other),
    }
}

/// Returns the display string for `print()` — `None` for `Unit` (silent).
pub fn aver_display(val: &Value) -> Option<String> {
    match val {
        Value::Unit => None,
        other => Some(aver_repr(other)),
    }
}
