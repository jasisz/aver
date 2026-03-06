/// Core Aver runtime value type and associated utilities.
///
/// Lives in its own module so both the interpreter and the service
/// implementations (`services::*`) can import it without circular
/// dependencies.
use std::collections::HashMap;
use std::iter::FusedIterator;
use std::rc::Rc;
use thiserror::Error;

use crate::ast::FnBody;

mod memo;

pub use memo::hash_memo_args;

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
    /// Internal signal: tail-call — caught by the trampoline in call_fn_ref.
    /// Never surfaces to the user. Boxed to keep RuntimeError small.
    #[error("Tail call")]
    TailCall(Box<(String, Vec<Value>)>),
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
    /// Lazy cons node: O(1) prepend while preserving immutable sharing.
    ListPrepend {
        head: Box<Value>,
        tail: Rc<Value>,
        len: usize,
    },
    /// Lazy concatenation node: O(1) append while preserving immutable sharing.
    ListConcat {
        left: Rc<Value>,
        right: Rc<Value>,
        len: usize,
    },
    Tuple(Vec<Value>),
    Map(HashMap<Value, Value>),
    Fn {
        name: String,
        params: Vec<(String, String)>,
        return_type: String,
        effects: Vec<String>,
        body: Rc<FnBody>,
        /// Compile-time resolution metadata (slot layout for locals).
        resolution: Option<crate::ast::FnResolution>,
        /// True only for functions selected by `compute_memo_fns` in the
        /// interpreter that defined them.
        memo_eligible: bool,
        /// Optional function-specific global scope (used by imported module
        /// functions so they resolve names in their home module).
        home_globals: Option<Rc<HashMap<String, Rc<Value>>>>,
    },
    Builtin(String),
    /// User-defined sum type variant, e.g. `Shape.Circle(3.14)`
    Variant {
        type_name: String,
        variant: String,
        fields: Vec<Value>,
    },
    /// User-defined product type (record), e.g. `User(name = "Alice", age = 30)`
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
            (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (
                Value::Fn {
                    name: n1,
                    params: p1,
                    return_type: r1,
                    effects: e1,
                    body: b1,
                    ..
                },
                Value::Fn {
                    name: n2,
                    params: p2,
                    return_type: r2,
                    effects: e2,
                    body: b2,
                    ..
                },
            ) => n1 == n2 && p1 == p2 && r1 == r2 && e1 == e2 && b1 == b2,
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
            Value::Tuple(items) => {
                16u8.hash(state);
                items.hash(state);
            }
            Value::Fn {
                name,
                params,
                return_type,
                effects,
                body,
                ..
            } => {
                11u8.hash(state);
                name.hash(state);
                params.hash(state);
                return_type.hash(state);
                effects.hash(state);
                format!("{:?}", body).hash(state);
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
            Value::List(_)
            | Value::ListSlice { .. }
            | Value::ListPrepend { .. }
            | Value::ListConcat { .. } => unreachable!("list hashed above"),
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
}

/// Scope stack: innermost scope last.
pub type Env = Vec<EnvFrame>;

// ---------------------------------------------------------------------------
// List helpers
// ---------------------------------------------------------------------------

/// Opaque read-only view over an Aver list.
///
/// Runtime code should prefer this API over matching on list variants or
/// assuming a particular backing container. That keeps future representation
/// changes local to this module.
#[derive(Clone, Copy)]
pub(crate) struct ListView<'a> {
    root: &'a Value,
}

#[derive(Clone, Copy)]
enum ListCursor<'a> {
    Node(&'a Value),
    Slice(&'a [Value], usize),
}

pub(crate) struct ListIter<'a> {
    stack: Vec<ListCursor<'a>>,
    remaining: usize,
}

impl<'a> ListIter<'a> {
    fn new(root: &'a Value) -> Self {
        Self {
            stack: vec![ListCursor::Node(root)],
            remaining: list_len_cached(root).unwrap_or(0),
        }
    }
}

impl<'a> Iterator for ListIter<'a> {
    type Item = &'a Value;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(cursor) = self.stack.pop() {
            match cursor {
                ListCursor::Slice(items, index) => {
                    if let Some(item) = items.get(index) {
                        self.stack.push(ListCursor::Slice(items, index + 1));
                        self.remaining = self.remaining.saturating_sub(1);
                        return Some(item);
                    }
                }
                ListCursor::Node(value) => match value {
                    Value::List(items) => {
                        if !items.is_empty() {
                            self.stack
                                .push(ListCursor::Slice(items.as_slice(), 0));
                        }
                    }
                    Value::ListSlice { items, start } => {
                        let slice = items.get(*start..).unwrap_or(&[]);
                        if !slice.is_empty() {
                            self.stack.push(ListCursor::Slice(slice, 0));
                        }
                    }
                    Value::ListPrepend { head, tail, .. } => {
                        self.stack.push(ListCursor::Node(tail.as_ref()));
                        self.remaining = self.remaining.saturating_sub(1);
                        return Some(head.as_ref());
                    }
                    Value::ListConcat { left, right, .. } => {
                        self.stack.push(ListCursor::Node(right.as_ref()));
                        self.stack.push(ListCursor::Node(left.as_ref()));
                    }
                    _ => unreachable!("ListIter only traverses list values"),
                },
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl ExactSizeIterator for ListIter<'_> {
    fn len(&self) -> usize {
        self.remaining
    }
}

impl FusedIterator for ListIter<'_> {}

fn list_len_cached(value: &Value) -> Option<usize> {
    match value {
        Value::List(items) => Some(items.len()),
        Value::ListSlice { items, start } => Some(items.len().saturating_sub(*start)),
        Value::ListPrepend { len, .. } | Value::ListConcat { len, .. } => Some(*len),
        _ => None,
    }
}

fn make_list_concat(left: &Value, right: &Value) -> Option<Value> {
    let left_len = list_len_cached(left)?;
    let right_len = list_len_cached(right)?;
    if left_len == 0 {
        return Some(right.clone());
    }
    if right_len == 0 {
        return Some(left.clone());
    }
    Some(Value::ListConcat {
        left: Rc::new(left.clone()),
        right: Rc::new(right.clone()),
        len: left_len + right_len,
    })
}

fn make_list_prepend(head: Value, tail: &Value) -> Option<Value> {
    let tail_len = list_len_cached(tail)?;
    if tail_len == 0 {
        return Some(list_from_vec(vec![head]));
    }
    Some(Value::ListPrepend {
        head: Box::new(head),
        tail: Rc::new(tail.clone()),
        len: tail_len + 1,
    })
}

impl<'a> ListView<'a> {
    pub(crate) fn len(self) -> usize {
        list_len_cached(self.root).expect("ListView root must be a list")
    }

    pub(crate) fn is_empty(self) -> bool {
        self.len() == 0
    }

    pub(crate) fn get(self, index: usize) -> Option<&'a Value> {
        self.iter().nth(index)
    }

    pub(crate) fn first(self) -> Option<&'a Value> {
        self.iter().next()
    }

    pub(crate) fn iter(self) -> ListIter<'a> {
        ListIter::new(self.root)
    }

    pub(crate) fn to_vec(self) -> Vec<Value> {
        let mut out = Vec::with_capacity(self.len());
        out.extend(self.iter().cloned());
        out
    }
}

pub(crate) fn list_view(value: &Value) -> Option<ListView<'_>> {
    match value {
        Value::List(_)
        | Value::ListSlice { .. }
        | Value::ListPrepend { .. }
        | Value::ListConcat { .. } => Some(ListView { root: value }),
        _ => None,
    }
}

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

pub(crate) fn list_empty() -> Value {
    list_from_vec(vec![])
}

pub fn list_to_vec(value: &Value) -> Option<Vec<Value>> {
    list_view(value).map(ListView::to_vec)
}

pub fn list_len(value: &Value) -> Option<usize> {
    list_view(value).map(ListView::len)
}

pub(crate) fn list_get(value: &Value, index: usize) -> Option<Value> {
    list_view(value).and_then(|items| items.get(index).cloned())
}

pub fn list_head(value: &Value) -> Option<Value> {
    list_view(value).and_then(|items| items.first().cloned())
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
        Value::ListPrepend { tail, .. } => Some((**tail).clone()),
        Value::ListConcat { left, right, .. } => {
            let left_len = list_len_cached(left.as_ref()).expect("left side must be a list");
            if left_len == 0 {
                list_tail_view(right.as_ref())
            } else if left_len == 1 {
                Some((**right).clone())
            } else {
                let left_tail =
                    list_tail_view(left.as_ref()).expect("non-empty left side must have a tail");
                make_list_concat(&left_tail, right.as_ref())
            }
        }
        _ => None,
    }
}

pub(crate) fn list_push(list: &Value, item: Value) -> Option<Value> {
    let singleton = list_from_vec(vec![item]);
    make_list_concat(list, &singleton)
}

pub(crate) fn list_prepend(item: Value, list: &Value) -> Option<Value> {
    make_list_prepend(item, list)
}

pub(crate) fn list_append(left: &Value, right: &Value) -> Option<Value> {
    make_list_concat(left, right)
}

pub(crate) fn list_reverse(list: &Value) -> Option<Value> {
    let mut out = list_to_vec(list)?;
    out.reverse();
    Some(list_from_vec(out))
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
        Value::Unit => "()".to_string(),
        Value::Ok(v) => format!("Result.Ok({})", aver_repr_inner(v)),
        Value::Err(v) => format!("Result.Err({})", aver_repr_inner(v)),
        Value::Some(v) => format!("Option.Some({})", aver_repr_inner(v)),
        Value::None => "Option.None".to_string(),
        Value::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(aver_repr_inner).collect();
            format!("({})", parts.join(", "))
        }
        Value::List(_)
        | Value::ListSlice { .. }
        | Value::ListPrepend { .. }
        | Value::ListConcat { .. } => unreachable!("handled via list_view above"),
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
        Value::List(_)
        | Value::ListSlice { .. }
        | Value::ListPrepend { .. }
        | Value::ListConcat { .. } => unreachable!("handled via list_view above"),
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
