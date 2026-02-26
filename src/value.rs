/// Core Aver runtime value type and associated utilities.
///
/// Lives in its own module so both the interpreter and the service
/// implementations (`services::*`) can import it without circular
/// dependencies.
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

use crate::ast::FnBody;

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

#[derive(Debug, Clone)]
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
    /// Shared list view (`items[start..]`) to avoid O(n^2) tail copying in
    /// recursive list processing.
    ListSlice {
        items: Rc<Vec<Value>>,
        start: usize,
    },
    Fn {
        name: String,
        params: Vec<(String, String)>,
        effects: Vec<String>,
        body: Rc<FnBody>,
        closure: Rc<HashMap<String, Rc<Value>>>,
        /// Slot-based closure for resolved functions (produced by resolver pass).
        closure_slots: Option<Rc<Vec<Rc<Value>>>>,
        /// Compile-time resolution metadata (slot layout for locals).
        resolution: Option<crate::ast::FnResolution>,
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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (list_slice(self), list_slice(other)) {
            (Some(xs), Some(ys)) => return xs == ys,
            (Some(_), None) | (None, Some(_)) => return false,
            (None, None) => {}
        }

        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            (Value::Ok(a), Value::Ok(b)) => a == b,
            (Value::Err(a), Value::Err(b)) => a == b,
            (Value::Some(a), Value::Some(b)) => a == b,
            (Value::None, Value::None) => true,
            (
                Value::Fn {
                    name: n1,
                    params: p1,
                    effects: e1,
                    body: b1,
                    closure: c1,
                    ..
                },
                Value::Fn {
                    name: n2,
                    params: p2,
                    effects: e2,
                    body: b2,
                    closure: c2,
                    ..
                },
            ) => n1 == n2 && p1 == p2 && e1 == e2 && b1 == b2 && c1 == c2,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (
                Value::Variant {
                    type_name: t1,
                    variant: v1,
                    fields: f1,
                },
                Value::Variant {
                    type_name: t2,
                    variant: v2,
                    fields: f2,
                },
            ) => t1 == t2 && v1 == v2 && f1 == f2,
            (
                Value::Record {
                    type_name: t1,
                    fields: f1,
                },
                Value::Record {
                    type_name: t2,
                    fields: f2,
                },
            ) => t1 == t2 && f1 == f2,
            (
                Value::Namespace {
                    name: n1,
                    members: m1,
                },
                Value::Namespace {
                    name: n2,
                    members: m2,
                },
            ) => n1 == n2 && m1 == m2,
            _ => false,
        }
    }
}

// ---------------------------------------------------------------------------
// Environment
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum EnvFrame {
    Owned(HashMap<String, Rc<Value>>),
    Shared(Rc<HashMap<String, Rc<Value>>>),
    /// Slot-indexed frame for resolved function bodies — O(1) lookup.
    Slots(Vec<Rc<Value>>),
    /// Shared slot-indexed closure captured from a resolved function.
    SharedSlots(Rc<Vec<Rc<Value>>>),
}

/// Scope stack: innermost scope last.
pub type Env = Vec<EnvFrame>;

// ---------------------------------------------------------------------------
// List helpers
// ---------------------------------------------------------------------------

pub fn list_slice(value: &Value) -> Option<&[Value]> {
    match value {
        Value::List(items) => Some(items.as_slice()),
        Value::ListSlice { items, start } => Some(items.get(*start..).unwrap_or(&[])),
        _ => None,
    }
}

pub fn list_from_vec(items: Vec<Value>) -> Value {
    Value::ListSlice {
        items: Rc::new(items),
        start: 0,
    }
}

pub fn list_to_vec(value: &Value) -> Option<Vec<Value>> {
    list_slice(value).map(|items| items.to_vec())
}

pub fn list_len(value: &Value) -> Option<usize> {
    list_slice(value).map(|items| items.len())
}

pub fn list_head(value: &Value) -> Option<Value> {
    list_slice(value).and_then(|items| items.first().cloned())
}

pub fn list_tail_view(value: &Value) -> Option<Value> {
    match value {
        Value::List(items) => {
            if items.is_empty() {
                None
            } else {
                Some(Value::ListSlice {
                    items: Rc::new(items.clone()),
                    start: 1,
                })
            }
        }
        Value::ListSlice { items, start } => {
            if *start >= items.len() {
                None
            } else {
                Some(Value::ListSlice {
                    items: Rc::clone(items),
                    start: start + 1,
                })
            }
        }
        _ => None,
    }
}

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
        Value::Ok(v) => format!("Result.Ok({})", aver_repr_inner(v)),
        Value::Err(v) => format!("Result.Err({})", aver_repr_inner(v)),
        Value::Some(v) => format!("Option.Some({})", aver_repr_inner(v)),
        Value::None => "Option.None".to_string(),
        Value::List(_) | Value::ListSlice { .. } => {
            let items = list_slice(val).expect("list variants must have a slice view");
            let parts: Vec<String> = items.iter().map(aver_repr_inner).collect();
            format!("[{}]", parts.join(", "))
        }
        Value::Fn { name, .. } => format!("<fn {}>", name),
        Value::Builtin(name) => format!("<builtin {}>", name),
        Value::Variant {
            variant, fields, ..
        } => {
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
        Value::List(_) | Value::ListSlice { .. } => {
            let items = list_slice(val).expect("list variants must have a slice view");
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

// ---------------------------------------------------------------------------
// Memo hashing — only for memo-safe (scalar + record/variant of scalar) values
// ---------------------------------------------------------------------------

/// Hash a slice of memo-safe argument values into a single u64.
pub fn hash_memo_args(args: &[Value]) -> u64 {
    use std::hash::Hasher;
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for arg in args {
        hash_value(arg, &mut hasher);
    }
    hasher.finish()
}

fn hash_value(val: &Value, hasher: &mut impl std::hash::Hasher) {
    use std::hash::Hash;
    match val {
        Value::Int(i) => {
            0u8.hash(hasher);
            i.hash(hasher);
        }
        Value::Float(f) => {
            1u8.hash(hasher);
            f.to_bits().hash(hasher);
        }
        Value::Str(s) => {
            2u8.hash(hasher);
            s.hash(hasher);
        }
        Value::Bool(b) => {
            3u8.hash(hasher);
            b.hash(hasher);
        }
        Value::Unit => {
            4u8.hash(hasher);
        }
        Value::Record { type_name, fields } => {
            5u8.hash(hasher);
            type_name.hash(hasher);
            for (k, v) in fields {
                k.hash(hasher);
                hash_value(v, hasher);
            }
        }
        Value::Variant {
            type_name,
            variant,
            fields,
        } => {
            6u8.hash(hasher);
            type_name.hash(hasher);
            variant.hash(hasher);
            for v in fields {
                hash_value(v, hasher);
            }
        }
        // Should not reach here for non-memo-safe values; no-op to avoid panic.
        _ => {
            255u8.hash(hasher);
        }
    }
}
