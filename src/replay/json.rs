use std::collections::{BTreeMap, HashMap};
use std::fmt::Write as _;

use crate::value::{Value, aver_repr, list_from_vec, list_view};

mod parser;

#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(BTreeMap<String, JsonValue>),
}

pub fn parse_json(input: &str) -> Result<JsonValue, String> {
    parser::parse_json(input)
}

pub fn json_to_string(value: &JsonValue) -> String {
    let mut out = String::new();
    write_json_compact(&mut out, value);
    out
}

pub fn format_json(value: &JsonValue) -> String {
    let mut out = String::new();
    write_json_pretty(&mut out, value, 0);
    out
}

fn get_required<'a>(
    obj: &'a BTreeMap<String, JsonValue>,
    key: &str,
    path: &str,
) -> Result<&'a JsonValue, String> {
    obj.get(key)
        .ok_or_else(|| format!("{}: missing required field '{}'", path, key))
}

fn expect_object<'a>(
    value: &'a JsonValue,
    path: &str,
) -> Result<&'a BTreeMap<String, JsonValue>, String> {
    match value {
        JsonValue::Object(obj) => Ok(obj),
        _ => Err(format!("{} must be an object", path)),
    }
}

fn parse_array<'a>(value: &'a JsonValue, path: &str) -> Result<&'a Vec<JsonValue>, String> {
    match value {
        JsonValue::Array(arr) => Ok(arr),
        _ => Err(format!("{} must be an array", path)),
    }
}

fn parse_string<'a>(value: &'a JsonValue, path: &str) -> Result<&'a str, String> {
    match value {
        JsonValue::String(s) => Ok(s),
        _ => Err(format!("{} must be a string", path)),
    }
}

pub fn values_to_json(values: &[Value]) -> Result<Vec<JsonValue>, String> {
    values.iter().map(value_to_json).collect()
}

pub fn values_to_json_lossy(values: &[Value]) -> Vec<JsonValue> {
    values.iter().map(value_to_json_lossy).collect()
}

pub fn json_values_to_values(values: &[JsonValue]) -> Result<Vec<Value>, String> {
    values.iter().map(json_to_value).collect()
}

pub fn value_to_json(value: &Value) -> Result<JsonValue, String> {
    if let Some(items) = list_view(value) {
        let mut arr = Vec::with_capacity(items.len());
        for item in items.iter() {
            arr.push(value_to_json(item)?);
        }
        return Ok(JsonValue::Array(arr));
    }

    match value {
        Value::Int(i) => Ok(JsonValue::Int(*i)),
        Value::Float(f) => {
            if !f.is_finite() {
                return Err("cannot serialize non-finite float (NaN/inf)".to_string());
            }
            Ok(JsonValue::Float(*f))
        }
        Value::Str(s) => Ok(JsonValue::String(s.clone())),
        Value::Bool(b) => Ok(JsonValue::Bool(*b)),
        Value::Unit => Ok(JsonValue::Null),
        Value::Ok(inner) => Ok(wrap_marker("$ok", value_to_json(inner)?)),
        Value::Err(inner) => Ok(wrap_marker("$err", value_to_json(inner)?)),
        Value::Some(inner) => Ok(wrap_marker("$some", value_to_json(inner)?)),
        Value::None => Ok(wrap_marker("$none", JsonValue::Bool(true))),
        Value::List(_)
        | Value::ListSlice { .. }
        | Value::ListPrepend { .. }
        | Value::ListConcat { .. } => unreachable!("handled via list_view above"),
        Value::Tuple(items) => {
            let mut arr = Vec::with_capacity(items.len());
            for item in items {
                arr.push(value_to_json(item)?);
            }
            Ok(wrap_marker("$tuple", JsonValue::Array(arr)))
        }
        Value::Map(entries) => {
            if entries.keys().all(|k| matches!(k, Value::Str(_))) {
                let mut obj = BTreeMap::new();
                for (k, v) in entries {
                    let Value::Str(key) = k else {
                        unreachable!("checked above");
                    };
                    obj.insert(key.clone(), value_to_json(v)?);
                }
                Ok(JsonValue::Object(obj))
            } else {
                let mut pairs = Vec::with_capacity(entries.len());
                for (k, v) in entries {
                    pairs.push(JsonValue::Array(vec![value_to_json(k)?, value_to_json(v)?]));
                }
                Ok(wrap_marker("$map", JsonValue::Array(pairs)))
            }
        }
        Value::Record { type_name, fields } => {
            let mut fields_obj = BTreeMap::new();
            for (name, field_value) in fields {
                fields_obj.insert(name.clone(), value_to_json(field_value)?);
            }
            let mut payload = BTreeMap::new();
            payload.insert("type".to_string(), JsonValue::String(type_name.clone()));
            payload.insert("fields".to_string(), JsonValue::Object(fields_obj));
            Ok(wrap_marker("$record", JsonValue::Object(payload)))
        }
        Value::Variant {
            type_name,
            variant,
            fields,
        } => {
            let mut field_vals = Vec::with_capacity(fields.len());
            for field in fields {
                field_vals.push(value_to_json(field)?);
            }
            let mut payload = BTreeMap::new();
            payload.insert("type".to_string(), JsonValue::String(type_name.clone()));
            payload.insert("name".to_string(), JsonValue::String(variant.clone()));
            payload.insert("fields".to_string(), JsonValue::Array(field_vals));
            Ok(wrap_marker("$variant", JsonValue::Object(payload)))
        }
        Value::Fn { .. } | Value::Builtin(_) | Value::Namespace { .. } => Err(format!(
            "cannot serialize non-replay-safe value: {}",
            aver_repr(value)
        )),
    }
}

pub fn json_to_value(json: &JsonValue) -> Result<Value, String> {
    match json {
        JsonValue::Null => Ok(Value::Unit),
        JsonValue::Bool(b) => Ok(Value::Bool(*b)),
        JsonValue::Int(i) => Ok(Value::Int(*i)),
        JsonValue::Float(f) => Ok(Value::Float(*f)),
        JsonValue::String(s) => Ok(Value::Str(s.clone())),
        JsonValue::Array(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(json_to_value(item)?);
            }
            Ok(list_from_vec(out))
        }
        JsonValue::Object(obj) => {
            if let Some((marker, payload)) = marker_single_key(obj) {
                return decode_marker(marker, payload);
            }
            let mut map = HashMap::with_capacity(obj.len());
            for (k, v) in obj {
                map.insert(Value::Str(k.clone()), json_to_value(v)?);
            }
            Ok(Value::Map(map))
        }
    }
}

pub fn first_diff_path(expected: &JsonValue, got: &JsonValue) -> Option<String> {
    first_diff_path_inner(expected, got, "$")
}

pub fn value_to_json_lossy(value: &Value) -> JsonValue {
    match value_to_json(value) {
        Ok(v) => v,
        Err(_) => {
            let mut obj = BTreeMap::new();
            obj.insert("$opaque".to_string(), JsonValue::String(aver_repr(value)));
            JsonValue::Object(obj)
        }
    }
}

fn wrap_marker(name: &str, value: JsonValue) -> JsonValue {
    let mut obj = BTreeMap::new();
    obj.insert(name.to_string(), value);
    JsonValue::Object(obj)
}

fn marker_single_key(obj: &BTreeMap<String, JsonValue>) -> Option<(&str, &JsonValue)> {
    if obj.len() != 1 {
        return None;
    }
    obj.iter().next().map(|(k, v)| (k.as_str(), v))
}

fn decode_marker(marker: &str, payload: &JsonValue) -> Result<Value, String> {
    match marker {
        "$ok" => Ok(Value::Ok(Box::new(json_to_value(payload)?))),
        "$err" => Ok(Value::Err(Box::new(json_to_value(payload)?))),
        "$some" => Ok(Value::Some(Box::new(json_to_value(payload)?))),
        "$none" => Ok(Value::None),
        "$tuple" => decode_tuple(payload),
        "$map" => decode_map(payload),
        "$record" => decode_record(payload),
        "$variant" => decode_variant(payload),
        _ => Err(format!("unknown replay marker '{}'", marker)),
    }
}

fn decode_tuple(payload: &JsonValue) -> Result<Value, String> {
    let items = parse_array(payload, "$tuple")?;
    let mut out = Vec::with_capacity(items.len());
    for item in items {
        out.push(json_to_value(item)?);
    }
    Ok(Value::Tuple(out))
}

fn decode_map(payload: &JsonValue) -> Result<Value, String> {
    let pairs = parse_array(payload, "$map")?;
    let mut out = HashMap::with_capacity(pairs.len());
    for (idx, pair_json) in pairs.iter().enumerate() {
        let pair = parse_array(pair_json, &format!("$map[{}]", idx))?;
        if pair.len() != 2 {
            return Err(format!("$map[{}] must be a 2-element array", idx));
        }
        let key = json_to_value(&pair[0])?;
        let value = json_to_value(&pair[1])?;
        out.insert(key, value);
    }
    Ok(Value::Map(out))
}

fn decode_record(payload: &JsonValue) -> Result<Value, String> {
    let obj = expect_object(payload, "$record")?;
    let type_name =
        parse_string(get_required(obj, "type", "$record")?, "$record.type")?.to_string();
    let fields_obj = expect_object(get_required(obj, "fields", "$record")?, "$record.fields")?;
    let mut fields = Vec::with_capacity(fields_obj.len());
    for (key, field_val) in fields_obj {
        fields.push((key.clone(), json_to_value(field_val)?));
    }
    Ok(Value::Record { type_name, fields })
}

fn decode_variant(payload: &JsonValue) -> Result<Value, String> {
    let obj = expect_object(payload, "$variant")?;
    let type_name =
        parse_string(get_required(obj, "type", "$variant")?, "$variant.type")?.to_string();
    let variant =
        parse_string(get_required(obj, "name", "$variant")?, "$variant.name")?.to_string();
    let fields_arr = parse_array(get_required(obj, "fields", "$variant")?, "$variant.fields")?;
    let mut fields = Vec::with_capacity(fields_arr.len());
    for val in fields_arr {
        fields.push(json_to_value(val)?);
    }
    Ok(Value::Variant {
        type_name,
        variant,
        fields,
    })
}

fn first_diff_path_inner(expected: &JsonValue, got: &JsonValue, path: &str) -> Option<String> {
    match (expected, got) {
        (JsonValue::Object(a), JsonValue::Object(b)) => {
            let mut keys = a.keys().chain(b.keys()).cloned().collect::<Vec<_>>();
            keys.sort();
            keys.dedup();
            for key in keys {
                let next_path = if path == "$" {
                    format!("$.{}", key)
                } else {
                    format!("{}.{}", path, key)
                };
                match (a.get(&key), b.get(&key)) {
                    (Some(av), Some(bv)) => {
                        if let Some(diff) = first_diff_path_inner(av, bv, &next_path) {
                            return Some(diff);
                        }
                    }
                    _ => return Some(next_path),
                }
            }
            None
        }
        (JsonValue::Array(a), JsonValue::Array(b)) => {
            if a.len() != b.len() {
                return Some(format!("{}[len]", path));
            }
            for (idx, (av, bv)) in a.iter().zip(b.iter()).enumerate() {
                let next_path = format!("{}[{}]", path, idx);
                if let Some(diff) = first_diff_path_inner(av, bv, &next_path) {
                    return Some(diff);
                }
            }
            None
        }
        _ => {
            if expected == got {
                None
            } else {
                Some(path.to_string())
            }
        }
    }
}

fn write_json_compact(out: &mut String, value: &JsonValue) {
    match value {
        JsonValue::Null => out.push_str("null"),
        JsonValue::Bool(true) => out.push_str("true"),
        JsonValue::Bool(false) => out.push_str("false"),
        JsonValue::Int(i) => {
            let _ = write!(out, "{}", i);
        }
        JsonValue::Float(f) => out.push_str(&format_float(*f)),
        JsonValue::String(s) => write_json_string(out, s),
        JsonValue::Array(arr) => {
            out.push('[');
            for (idx, item) in arr.iter().enumerate() {
                if idx > 0 {
                    out.push(',');
                }
                write_json_compact(out, item);
            }
            out.push(']');
        }
        JsonValue::Object(obj) => {
            out.push('{');
            for (idx, (k, v)) in obj.iter().enumerate() {
                if idx > 0 {
                    out.push(',');
                }
                write_json_string(out, k);
                out.push(':');
                write_json_compact(out, v);
            }
            out.push('}');
        }
    }
}

fn write_json_pretty(out: &mut String, value: &JsonValue, indent: usize) {
    match value {
        JsonValue::Array(arr) => {
            if arr.is_empty() {
                out.push_str("[]");
                return;
            }
            out.push_str("[\n");
            for (idx, item) in arr.iter().enumerate() {
                push_indent(out, indent + 2);
                write_json_pretty(out, item, indent + 2);
                if idx + 1 < arr.len() {
                    out.push(',');
                }
                out.push('\n');
            }
            push_indent(out, indent);
            out.push(']');
        }
        JsonValue::Object(obj) => {
            if obj.is_empty() {
                out.push_str("{}");
                return;
            }
            out.push_str("{\n");
            for (idx, (k, v)) in obj.iter().enumerate() {
                push_indent(out, indent + 2);
                write_json_string(out, k);
                out.push_str(": ");
                write_json_pretty(out, v, indent + 2);
                if idx + 1 < obj.len() {
                    out.push(',');
                }
                out.push('\n');
            }
            push_indent(out, indent);
            out.push('}');
        }
        _ => write_json_compact(out, value),
    }
}

fn push_indent(out: &mut String, indent: usize) {
    for _ in 0..indent {
        out.push(' ');
    }
}

fn write_json_string(out: &mut String, s: &str) {
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{08}' => out.push_str("\\b"),
            '\u{0C}' => out.push_str("\\f"),
            c if c < '\u{20}' => {
                let _ = write!(out, "\\u{:04X}", c as u32);
            }
            c => out.push(c),
        }
    }
    out.push('"');
}

fn format_float(f: f64) -> String {
    let mut s = format!("{}", f);
    if !s.contains('.') && !s.contains('e') && !s.contains('E') {
        s.push_str(".0");
    }
    s
}
