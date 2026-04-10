/// Core Aver runtime value type and associated utilities.
///
/// Lives in its own module so the VM and the service
/// implementations (`services::*`) can import it without circular
/// dependencies.
#[cfg(feature = "runtime")]
use aver_rt::{AverList, AverVector};
#[cfg(not(feature = "runtime"))]
type AverList<T> = Vec<T>;
#[cfg(not(feature = "runtime"))]
type AverVector<T> = Vec<T>;
use std::collections::HashMap;
use std::sync::Arc as Rc;
use thiserror::Error;

use crate::ast::FnBody;
use crate::nan_value::NanValue;

mod memo;

pub use memo::hash_memo_args;

// ---------------------------------------------------------------------------
// RuntimeError
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Runtime error: {0}")]
    Error(String),
    #[error("Runtime error [line {line}]: {msg}")]
    ErrorAt { msg: String, line: usize },
    /// Internal signal: `?` operator encountered Err — caught by call_value to
    /// do early return. Never surfaces to the user (type checker prevents
    /// top-level use).
    #[error("Error propagation")]
    ErrProp(NanValue), // NanValue is Copy (8 bytes), no Box needed
    /// Internal signal: tail-call — caught by the trampoline in call_fn_ref.
    /// Never surfaces to the user. Boxed to keep RuntimeError small.
    #[error("Tail call")]
    TailCall(Box<(String, Vec<NanValue>)>),
    #[error("Replay mismatch at seq {seq}: expected '{expected}', got '{got}'")]
    ReplayMismatch {
        seq: u32,
        expected: String,
        got: String,
    },
    #[error(
        "Replay args mismatch at seq {seq} for '{effect_type}': expected {expected}, got {got}"
    )]
    ReplayArgsMismatch {
        seq: u32,
        effect_type: String,
        expected: String,
        got: String,
    },
    #[error("Replay exhausted at position {position}: no recorded effect for call '{effect_type}'")]
    ReplayExhausted {
        effect_type: String,
        position: usize,
    },
    #[error("Replay has {remaining} unconsumed effect(s)")]
    ReplayUnconsumed { remaining: usize },
    #[error("Replay serialization error: {0}")]
    ReplaySerialization(String),
}

impl RuntimeError {
    /// Attach a source line to an undecorated error. Already-decorated errors
    /// and internal signals (TailCall, ErrProp) pass through unchanged.
    pub fn at_line(self, line: usize) -> Self {
        if line == 0 {
            return self;
        }
        match self {
            RuntimeError::Error(msg) => RuntimeError::ErrorAt { msg, line },
            other => other,
        }
    }

    /// Extract the human-readable message regardless of variant.
    pub fn message(&self) -> &str {
        match self {
            RuntimeError::Error(msg) | RuntimeError::ErrorAt { msg, .. } => msg,
            other => {
                // For non-message variants, fall back to thiserror Display.
                // This is a cold path — only needed for unusual error kinds.
                // Return a static placeholder; callers should use Display.
                match other {
                    RuntimeError::Error(_) | RuntimeError::ErrorAt { .. } => unreachable!(),
                    _ => "",
                }
            }
        }
    }

    /// Source line if available (ErrorAt only).
    pub fn source_line(&self) -> Option<usize> {
        match self {
            RuntimeError::ErrorAt { line, .. } if *line > 0 => Some(*line),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// Value
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct FunctionValue {
    pub name: Rc<String>,
    pub params: Rc<Vec<(String, String)>>,
    pub return_type: Rc<String>,
    pub effects: Rc<Vec<String>>,
    pub body: Rc<FnBody>,
    /// Compile-time resolution metadata (slot layout for locals).
    pub resolution: Option<crate::ast::FnResolution>,
    pub memo_eligible: bool,
    /// Optional function-specific global scope (used by imported module
    /// functions so they resolve names in their home module).
    pub home_globals: Option<Rc<HashMap<String, NanValue>>>,
}

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
    List(AverList<Value>),
    Vector(AverVector<Value>),
    Tuple(Vec<Value>),
    Map(HashMap<Value, Value>),
    Fn(Rc<FunctionValue>),
    Builtin(String),
    /// User-defined sum type variant, e.g. `Shape.Circle(3.14)`
    Variant {
        type_name: String,
        variant: String,
        fields: Rc<[Value]>,
    },
    /// User-defined product type (record), e.g. `User(name = "Alice", age = 30)`
    Record {
        type_name: String,
        fields: Rc<[(String, Value)]>,
    },
    /// Type namespace: `Shape` — provides `Shape.Circle`, `Shape.Rect`, etc.
    Namespace {
        name: String,
        members: HashMap<String, Value>,
    },
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (list_view(self), list_view(other)) {
            (Some(xs), Some(ys)) => return xs.iter().eq(ys.iter()),
            (Some(_), None) | (None, Some(_)) => return false,
            (None, None) => {}
        }

        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => {
                if a.is_nan() || b.is_nan() {
                    a.to_bits() == b.to_bits()
                } else {
                    a == b
                }
            }
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            (Value::Ok(a), Value::Ok(b)) => a == b,
            (Value::Err(a), Value::Err(b)) => a == b,
            (Value::Some(a), Value::Some(b)) => a == b,
            (Value::None, Value::None) => true,
            (Value::Vector(a), Value::Vector(b)) => a == b,
            (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Fn(a), Value::Fn(b)) => {
                a.name == b.name
                    && a.params == b.params
                    && a.return_type == b.return_type
                    && a.effects == b.effects
                    && a.body == b.body
            }
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

impl Eq for Value {}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if let Some(items) = list_view(self) {
            8u8.hash(state);
            items.len().hash(state);
            for item in items.iter() {
                item.hash(state);
            }
            return;
        }

        match self {
            Value::Int(i) => {
                0u8.hash(state);
                i.hash(state);
            }
            Value::Float(f) => {
                1u8.hash(state);
                let bits = if *f == 0.0 {
                    0.0f64.to_bits()
                } else {
                    f.to_bits()
                };
                bits.hash(state);
            }
            Value::Str(s) => {
                2u8.hash(state);
                s.hash(state);
            }
            Value::Bool(b) => {
                3u8.hash(state);
                b.hash(state);
            }
            Value::Unit => {
                4u8.hash(state);
            }
            Value::Ok(v) => {
                5u8.hash(state);
                v.hash(state);
            }
            Value::Err(v) => {
                6u8.hash(state);
                v.hash(state);
            }
            Value::Some(v) => {
                7u8.hash(state);
                v.hash(state);
            }
            Value::None => {
                9u8.hash(state);
            }
            Value::Map(map) => {
                10u8.hash(state);
                let mut entries = map.iter().collect::<Vec<_>>();
                entries.sort_by(|(k1, _), (k2, _)| aver_repr(k1).cmp(&aver_repr(k2)));
                for (k, v) in entries {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Value::Vector(vec) => {
                17u8.hash(state);
                vec.hash(state);
            }
            Value::Tuple(items) => {
                16u8.hash(state);
                items.hash(state);
            }
            Value::Fn(function) => {
                11u8.hash(state);
                function.name.hash(state);
                function.params.hash(state);
                function.return_type.hash(state);
                function.effects.hash(state);
                format!("{:?}", function.body).hash(state);
            }
            Value::Builtin(name) => {
                12u8.hash(state);
                name.hash(state);
            }
            Value::Variant {
                type_name,
                variant,
                fields,
            } => {
                13u8.hash(state);
                type_name.hash(state);
                variant.hash(state);
                fields.hash(state);
            }
            Value::Record { type_name, fields } => {
                14u8.hash(state);
                type_name.hash(state);
                fields.hash(state);
            }
            Value::Namespace { name, members } => {
                15u8.hash(state);
                name.hash(state);
                let mut keys = members.keys().collect::<Vec<_>>();
                keys.sort();
                for key in keys {
                    key.hash(state);
                    if let Some(value) = members.get(key) {
                        value.hash(state);
                    }
                }
            }
            Value::List(_) => unreachable!("list hashed above"),
        }
    }
}

// ---------------------------------------------------------------------------
// Environment
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum EnvFrame {
    Owned(HashMap<String, NanValue>),
    Shared(Rc<HashMap<String, NanValue>>),
    /// Slot-indexed frame for resolved function bodies — O(1) lookup.
    Slots(Vec<NanValue>),
}

/// Scope stack: innermost scope last.
pub type Env = Vec<EnvFrame>;

// ---------------------------------------------------------------------------
// List helpers
// ---------------------------------------------------------------------------

pub(crate) type ListView<'a> = &'a AverList<Value>;

pub(crate) fn list_view(value: &Value) -> Option<ListView<'_>> {
    match value {
        Value::List(items) => Some(items),
        _ => None,
    }
}

#[cfg(feature = "runtime")]
pub fn list_slice(value: &Value) -> Option<&[Value]> {
    list_view(value).and_then(AverList::as_slice)
}

#[cfg(feature = "runtime")]
pub fn list_from_vec(items: Vec<Value>) -> Value {
    Value::List(AverList::from_vec(items))
}

#[cfg(feature = "runtime")]
pub fn list_to_vec(value: &Value) -> Option<Vec<Value>> {
    list_view(value).map(AverList::to_vec)
}

#[cfg(feature = "runtime")]
pub fn list_len(value: &Value) -> Option<usize> {
    list_view(value).map(AverList::len)
}

#[cfg(feature = "runtime")]
pub fn list_head(value: &Value) -> Option<Value> {
    list_view(value).and_then(|items| items.first().cloned())
}

#[cfg(feature = "runtime")]
pub(crate) fn list_prepend(item: Value, list: &Value) -> Option<Value> {
    list_view(list).map(|items| Value::List(AverList::prepend(item, items)))
}

#[cfg(feature = "runtime")]
pub(crate) fn list_concat(left: &Value, right: &Value) -> Option<Value> {
    let left = list_view(left)?;
    let right = list_view(right)?;
    Some(Value::List(AverList::concat(left, right)))
}

#[cfg(feature = "runtime")]
pub(crate) fn list_reverse(list: &Value) -> Option<Value> {
    list_view(list).map(|items| Value::List(items.reverse()))
}

// ---------------------------------------------------------------------------
// Effect inspection
// ---------------------------------------------------------------------------

/// Extract the declared effects from a callable value.
pub fn callable_declared_effects(fn_val: &Value) -> Vec<String> {
    match fn_val {
        Value::Fn(function) => function.effects.as_ref().clone(),
        _ => vec![],
    }
}

// ---------------------------------------------------------------------------
// Display helpers
// ---------------------------------------------------------------------------

/// Human-readable representation of a value (used by `str()` and `:env`).
pub fn aver_repr(val: &Value) -> String {
    if let Some(items) = list_view(val) {
        let parts: Vec<String> = items.iter().map(aver_repr_inner).collect();
        return format!("[{}]", parts.join(", "));
    }

    match val {
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Str(s) => s.clone(),
        Value::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Value::Unit => "Unit".to_string(),
        Value::Ok(v) => format!("Result.Ok({})", aver_repr_inner(v)),
        Value::Err(v) => format!("Result.Err({})", aver_repr_inner(v)),
        Value::Some(v) => format!("Option.Some({})", aver_repr_inner(v)),
        Value::None => "Option.None".to_string(),
        Value::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(aver_repr_inner).collect();
            format!("({})", parts.join(", "))
        }
        Value::Vector(vec) => {
            let parts: Vec<String> = vec.iter().map(aver_repr_inner).collect();
            format!("Vector[{}]", parts.join(", "))
        }
        Value::List(_) => unreachable!("handled via list_view above"),
        Value::Map(entries) => {
            let mut pairs = entries
                .iter()
                .map(|(k, v)| (aver_repr_inner(k), aver_repr_inner(v)))
                .collect::<Vec<_>>();
            pairs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));
            let parts = pairs
                .into_iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<_>>();
            format!("{{{}}}", parts.join(", "))
        }
        Value::Fn(function) => format!("<fn {}>", function.name),
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
    if let Some(items) = list_view(val) {
        let parts: Vec<String> = items.iter().map(aver_repr_inner).collect();
        return format!("[{}]", parts.join(", "));
    }

    match val {
        Value::Str(s) => format!("\"{}\"", s),
        Value::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(aver_repr_inner).collect();
            format!("({})", parts.join(", "))
        }
        Value::Vector(vec) => {
            let parts: Vec<String> = vec.iter().map(aver_repr_inner).collect();
            format!("Vector[{}]", parts.join(", "))
        }
        Value::List(_) => unreachable!("handled via list_view above"),
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
