/// String namespace — text manipulation helpers.
///
/// Methods:
///   String.len(s)               → Int            — char count (code points)
///   String.byteLength(s)        → Int            — byte count (UTF-8)
///   String.startsWith(s, pre)   → Bool
///   String.endsWith(s, suf)     → Bool
///   String.contains(s, sub)     → Bool
///   String.slice(s, from, to)   → String         — code-point based substring
///   String.trim(s)              → String
///   String.split(s, delim)      → List<String>
///   String.replace(s, old, new) → String
///   String.join(list, sep)      → String
///   String.charAt(s, index)     → Option<String>  — O(1)-ish char at code-point index
///   String.chars(s)             → List<String>   — each char as 1-char string
///   String.fromInt(n)           → String
///   String.fromFloat(f)         → String
///   String.fromBool(b)          → String
///   String.toLower(s)           → String         — lowercase (Unicode-aware)
///   String.toUpper(s)           → String         — uppercase (Unicode-aware)
///
/// No effects required.
use std::collections::HashMap;
use std::rc::Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value, list_from_vec, list_view};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "len",
        "byteLength",
        "startsWith",
        "endsWith",
        "contains",
        "slice",
        "trim",
        "split",
        "replace",
        "join",
        "charAt",
        "chars",
        "fromInt",
        "fromFloat",
        "fromBool",
        "toLower",
        "toUpper",
    ] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("String.{}", method)),
        );
    }
    global.insert(
        "String".to_string(),
        Value::Namespace {
            name: "String".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

/// Returns `Some(result)` when `name` is owned by this namespace, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "String.len" => Some(length(args)),
        "String.byteLength" => Some(byte_length(args)),
        "String.startsWith" => Some(starts_with(args)),
        "String.endsWith" => Some(ends_with(args)),
        "String.contains" => Some(contains(args)),
        "String.slice" => Some(slice(args)),
        "String.trim" => Some(trim(args)),
        "String.split" => Some(split(args)),
        "String.replace" => Some(replace(args)),
        "String.join" => Some(join(args)),
        "String.charAt" => Some(char_at(args)),
        "String.chars" => Some(chars(args)),
        "String.fromInt" => Some(from_int(args)),
        "String.fromFloat" => Some(from_float(args)),
        "String.fromBool" => Some(from_bool(args)),
        "String.toLower" => Some(to_lower(args)),
        "String.toUpper" => Some(to_upper(args)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

fn length(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.len", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.len: argument must be a String".to_string(),
        ));
    };
    Ok(Value::Int(s.chars().count() as i64))
}

fn byte_length(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.byteLength", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.byteLength: argument must be a String".to_string(),
        ));
    };
    Ok(Value::Int(s.len() as i64))
}

fn starts_with(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.startsWith", args)?;
    let (Value::Str(s), Value::Str(prefix)) = (a, b) else {
        return Err(RuntimeError::Error(
            "String.startsWith: both arguments must be String".to_string(),
        ));
    };
    Ok(Value::Bool(s.starts_with(prefix.as_str())))
}

fn ends_with(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.endsWith", args)?;
    let (Value::Str(s), Value::Str(suffix)) = (a, b) else {
        return Err(RuntimeError::Error(
            "String.endsWith: both arguments must be String".to_string(),
        ));
    };
    Ok(Value::Bool(s.ends_with(suffix.as_str())))
}

fn contains(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.contains", args)?;
    let (Value::Str(s), Value::Str(sub)) = (a, b) else {
        return Err(RuntimeError::Error(
            "String.contains: both arguments must be String".to_string(),
        ));
    };
    Ok(Value::Bool(s.contains(sub.as_str())))
}

fn slice(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "String.slice() takes 3 arguments (s, from, to), got {}",
            args.len()
        )));
    }
    let Value::Str(s) = &args[0] else {
        return Err(RuntimeError::Error(
            "String.slice: first argument must be a String".to_string(),
        ));
    };
    let Value::Int(from) = &args[1] else {
        return Err(RuntimeError::Error(
            "String.slice: second argument must be an Int".to_string(),
        ));
    };
    let Value::Int(to) = &args[2] else {
        return Err(RuntimeError::Error(
            "String.slice: third argument must be an Int".to_string(),
        ));
    };
    Ok(Value::Str(aver_rt::string_slice(s, *from, *to)))
}

fn trim(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.trim", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.trim: argument must be a String".to_string(),
        ));
    };
    Ok(Value::Str(s.trim().to_string()))
}

fn split(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.split", args)?;
    let (Value::Str(s), Value::Str(delim)) = (a, b) else {
        return Err(RuntimeError::Error(
            "String.split: both arguments must be String".to_string(),
        ));
    };
    let parts: Vec<Value> = s
        .split(delim.as_str())
        .map(|p| Value::Str(p.to_string()))
        .collect();
    Ok(list_from_vec(parts))
}

fn replace(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "String.replace() takes 3 arguments (s, old, new), got {}",
            args.len()
        )));
    }
    let (Value::Str(s), Value::Str(old), Value::Str(new)) = (&args[0], &args[1], &args[2]) else {
        return Err(RuntimeError::Error(
            "String.replace: all arguments must be String".to_string(),
        ));
    };
    Ok(Value::Str(s.replace(old.as_str(), new.as_str())))
}

fn join(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.join", args)?;
    let items = list_view(a).ok_or_else(|| {
        RuntimeError::Error("String.join: first argument must be a List".to_string())
    })?;
    let Value::Str(sep) = b else {
        return Err(RuntimeError::Error(
            "String.join: second argument must be a String".to_string(),
        ));
    };
    let strs: Result<Vec<String>, RuntimeError> = items
        .iter()
        .map(|v| match v {
            Value::Str(s) => Ok(s.clone()),
            _ => Err(RuntimeError::Error(
                "String.join: list elements must be String".to_string(),
            )),
        })
        .collect();
    Ok(Value::Str(strs?.join(sep.as_str())))
}

fn char_at(args: &[Value]) -> Result<Value, RuntimeError> {
    let [a, b] = two_args("String.charAt", args)?;
    let Value::Str(s) = a else {
        return Err(RuntimeError::Error(
            "String.charAt: first argument must be a String".to_string(),
        ));
    };
    let Value::Int(idx) = b else {
        return Err(RuntimeError::Error(
            "String.charAt: second argument must be an Int".to_string(),
        ));
    };
    let idx = *idx as usize;
    match s.chars().nth(idx) {
        Some(c) => Ok(Value::Some(Box::new(Value::Str(c.to_string())))),
        None => Ok(Value::None),
    }
}

fn chars(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.chars", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.chars: argument must be a String".to_string(),
        ));
    };
    let result: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
    Ok(list_from_vec(result))
}

fn from_int(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.fromInt", args)?;
    let Value::Int(n) = val else {
        return Err(RuntimeError::Error(
            "String.fromInt: argument must be an Int".to_string(),
        ));
    };
    Ok(Value::Str(format!("{}", n)))
}

fn from_float(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.fromFloat", args)?;
    let Value::Float(f) = val else {
        return Err(RuntimeError::Error(
            "String.fromFloat: argument must be a Float".to_string(),
        ));
    };
    Ok(Value::Str(format!("{}", f)))
}

fn from_bool(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.fromBool", args)?;
    let Value::Bool(b) = val else {
        return Err(RuntimeError::Error(
            "String.fromBool: argument must be a Bool".to_string(),
        ));
    };
    Ok(Value::Str(if *b { "true" } else { "false" }.to_string()))
}

fn to_lower(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.toLower", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.toLower: argument must be a String".to_string(),
        ));
    };
    Ok(Value::Str(s.to_lowercase()))
}

fn to_upper(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("String.toUpper", args)?;
    let Value::Str(s) = val else {
        return Err(RuntimeError::Error(
            "String.toUpper: argument must be a String".to_string(),
        ));
    };
    Ok(Value::Str(s.to_uppercase()))
}

// ─── Helpers ────────────────────────────────────────────────────────────────

fn one_arg<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 1], RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument, got {}",
            name,
            args.len()
        )));
    }
    Ok([&args[0]])
}

fn two_args<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 2], RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 2 arguments, got {}",
            name,
            args.len()
        )));
    }
    Ok([&args[0], &args[1]])
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &[
        "len",
        "byteLength",
        "startsWith",
        "endsWith",
        "contains",
        "slice",
        "trim",
        "split",
        "replace",
        "join",
        "charAt",
        "chars",
        "fromInt",
        "fromFloat",
        "fromBool",
        "toLower",
        "toUpper",
    ];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("String.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("String"),
        members,
    });
    global.insert("String".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "String.len" => Some(length_nv(args, arena)),
        "String.byteLength" => Some(byte_length_nv(args, arena)),
        "String.startsWith" => Some(starts_with_nv(args, arena)),
        "String.endsWith" => Some(ends_with_nv(args, arena)),
        "String.contains" => Some(contains_nv(args, arena)),
        "String.slice" => Some(slice_nv(args, arena)),
        "String.trim" => Some(trim_nv(args, arena)),
        "String.split" => Some(split_nv(args, arena)),
        "String.replace" => Some(replace_nv(args, arena)),
        "String.join" => Some(join_nv(args, arena)),
        "String.charAt" => Some(char_at_nv(args, arena)),
        "String.chars" => Some(chars_nv(args, arena)),
        "String.fromInt" => Some(from_int_nv(args, arena)),
        "String.fromFloat" => Some(from_float_nv(args, arena)),
        "String.fromBool" => Some(from_bool_nv(args, arena)),
        "String.toLower" => Some(to_lower_nv(args, arena)),
        "String.toUpper" => Some(to_upper_nv(args, arena)),
        _ => None,
    }
}

fn nv_str(v: NanValue, arena: &Arena) -> Option<&str> {
    if v.is_string() {
        Some(arena.get_string(v.arena_index()))
    } else {
        None
    }
}

fn length_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    let s = nv_str(args[0], arena)
        .ok_or_else(|| RuntimeError::Error("String.len: argument must be a String".to_string()))?;
    Ok(NanValue::new_int(s.chars().count() as i64, arena))
}

fn byte_length_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.byteLength() takes 1 argument, got {}",
            args.len()
        )));
    }
    let s = nv_str(args[0], arena).ok_or_else(|| {
        RuntimeError::Error("String.byteLength: argument must be a String".to_string())
    })?;
    Ok(NanValue::new_int(s.len() as i64, arena))
}

fn starts_with_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.startsWith() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.startsWith: both arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index());
    let prefix = arena.get_string(args[1].arena_index());
    let result = s.starts_with(prefix);
    Ok(NanValue::new_bool(result))
}

fn ends_with_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.endsWith() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.endsWith: both arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index());
    let suffix = arena.get_string(args[1].arena_index());
    let result = s.ends_with(suffix);
    Ok(NanValue::new_bool(result))
}

fn contains_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.contains() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.contains: both arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index());
    let sub = arena.get_string(args[1].arena_index());
    let result = s.contains(sub);
    Ok(NanValue::new_bool(result))
}

fn slice_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "String.slice() takes 3 arguments (s, from, to), got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.slice: first argument must be a String".to_string(),
        ));
    }
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "String.slice: second argument must be an Int".to_string(),
        ));
    }
    if !args[2].is_int() {
        return Err(RuntimeError::Error(
            "String.slice: third argument must be an Int".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index()).to_string();
    let from = args[1].as_int(arena);
    let to = args[2].as_int(arena);
    let result = aver_rt::string_slice(&s, from, to);
    let idx = arena.push_string(&result);
    Ok(NanValue::new_string(idx))
}

fn trim_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.trim() takes 1 argument, got {}",
            args.len()
        )));
    }
    let s = nv_str(args[0], arena)
        .ok_or_else(|| RuntimeError::Error("String.trim: argument must be a String".to_string()))?;
    let result = s.trim().to_string();
    let idx = arena.push_string(&result);
    Ok(NanValue::new_string(idx))
}

fn split_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.split() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.split: both arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index()).to_string();
    let delim = arena.get_string(args[1].arena_index()).to_string();
    let parts: Vec<NanValue> = s
        .split(&*delim)
        .map(|p| {
            let idx = arena.push_string(p);
            NanValue::new_string(idx)
        })
        .collect();
    let list_idx = arena.push_list(parts);
    Ok(NanValue::new_list(list_idx))
}

fn replace_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "String.replace() takes 3 arguments (s, old, new), got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() || !args[2].is_string() {
        return Err(RuntimeError::Error(
            "String.replace: all arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index()).to_string();
    let old = arena.get_string(args[1].arena_index()).to_string();
    let new = arena.get_string(args[2].arena_index()).to_string();
    let result = s.replace(&*old, &new);
    let idx = arena.push_string(&result);
    Ok(NanValue::new_string(idx))
}

fn join_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.join() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "String.join: first argument must be a List".to_string(),
        ));
    }
    if !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.join: second argument must be a String".to_string(),
        ));
    }
    let items = arena.list_to_vec(args[0].arena_index());
    let sep = arena.get_string(args[1].arena_index()).to_string();
    let mut strs: Vec<String> = Vec::with_capacity(items.len());
    for item in &items {
        if !item.is_string() {
            return Err(RuntimeError::Error(
                "String.join: list elements must be String".to_string(),
            ));
        }
        strs.push(arena.get_string(item.arena_index()).to_string());
    }
    let result = strs.join(&sep);
    let idx = arena.push_string(&result);
    Ok(NanValue::new_string(idx))
}

fn char_at_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.charAt() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.charAt: first argument must be a String".to_string(),
        ));
    }
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "String.charAt: second argument must be an Int".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index());
    let idx_val = args[1].as_int(arena) as usize;
    match s.chars().nth(idx_val) {
        Some(c) => {
            let cs = c.to_string();
            let s_idx = arena.push_string(&cs);
            let inner = NanValue::new_string(s_idx);
            let box_idx = arena.push_boxed(inner);
            Ok(NanValue::new_some(box_idx))
        }
        None => Ok(NanValue::NONE),
    }
}

fn chars_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.chars() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.chars: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index()).to_string();
    let items: Vec<NanValue> = s
        .chars()
        .map(|c| {
            let cs = c.to_string();
            let idx = arena.push_string(&cs);
            NanValue::new_string(idx)
        })
        .collect();
    let list_idx = arena.push_list(items);
    Ok(NanValue::new_list(list_idx))
}

fn from_int_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.fromInt() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "String.fromInt: argument must be an Int".to_string(),
        ));
    }
    let s = format!("{}", args[0].as_int(arena));
    let idx = arena.push_string(&s);
    Ok(NanValue::new_string(idx))
}

fn from_float_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.fromFloat() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_float() {
        return Err(RuntimeError::Error(
            "String.fromFloat: argument must be a Float".to_string(),
        ));
    }
    let s = format!("{}", args[0].as_float());
    let idx = arena.push_string(&s);
    Ok(NanValue::new_string(idx))
}

fn from_bool_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.fromBool() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_bool() {
        return Err(RuntimeError::Error(
            "String.fromBool: argument must be a Bool".to_string(),
        ));
    }
    let s = if args[0].as_bool() { "true" } else { "false" };
    let idx = arena.push_string(s);
    Ok(NanValue::new_string(idx))
}

fn to_lower_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.toLower() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.toLower: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index()).to_lowercase();
    let idx = arena.push_string(&s);
    Ok(NanValue::new_string(idx))
}

fn to_upper_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.toUpper() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.toUpper: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string(args[0].arena_index()).to_uppercase();
    let idx = arena.push_string(&s);
    Ok(NanValue::new_string(idx))
}
