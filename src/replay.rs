use std::collections::BTreeMap;
use std::fmt::Write as _;

use crate::value::{aver_repr, list_slice, Value};

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

#[derive(Debug, Clone, PartialEq)]
pub enum RecordedOutcome {
    Value(JsonValue),
    RuntimeError(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EffectRecord {
    pub seq: u32,
    pub effect_type: String,
    pub args: Vec<JsonValue>,
    pub outcome: RecordedOutcome,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SessionRecording {
    pub schema_version: u32,
    pub request_id: String,
    pub timestamp: String,
    pub program_file: String,
    pub module_root: String,
    pub entry_fn: String,
    pub input: JsonValue,
    pub effects: Vec<EffectRecord>,
    pub output: RecordedOutcome,
}

pub fn parse_json(input: &str) -> Result<JsonValue, String> {
    JsonParser::new(input).parse()
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

pub fn parse_session_recording(input: &str) -> Result<SessionRecording, String> {
    let json = parse_json(input)?;
    session_recording_from_json(&json)
}

pub fn session_recording_to_string_pretty(recording: &SessionRecording) -> String {
    format_json(&session_recording_to_json(recording))
}

pub fn session_recording_to_json(recording: &SessionRecording) -> JsonValue {
    let mut obj = BTreeMap::new();
    obj.insert(
        "schema_version".to_string(),
        JsonValue::Int(recording.schema_version as i64),
    );
    obj.insert(
        "request_id".to_string(),
        JsonValue::String(recording.request_id.clone()),
    );
    obj.insert(
        "timestamp".to_string(),
        JsonValue::String(recording.timestamp.clone()),
    );
    obj.insert(
        "program_file".to_string(),
        JsonValue::String(recording.program_file.clone()),
    );
    obj.insert(
        "module_root".to_string(),
        JsonValue::String(recording.module_root.clone()),
    );
    obj.insert(
        "entry_fn".to_string(),
        JsonValue::String(recording.entry_fn.clone()),
    );
    obj.insert("input".to_string(), recording.input.clone());

    let effects = recording
        .effects
        .iter()
        .map(effect_record_to_json)
        .collect::<Vec<_>>();
    obj.insert("effects".to_string(), JsonValue::Array(effects));
    obj.insert("output".to_string(), outcome_to_json(&recording.output));

    JsonValue::Object(obj)
}

pub fn session_recording_from_json(json: &JsonValue) -> Result<SessionRecording, String> {
    let obj = expect_object(json, "recording")?;

    let schema_version = match obj.get("schema_version") {
        Some(v) => parse_u32(v, "recording.schema_version")?,
        None => 1,
    };

    let request_id = parse_string(
        get_required(obj, "request_id", "recording")?,
        "recording.request_id",
    )?
    .to_string();
    let timestamp = parse_string(
        get_required(obj, "timestamp", "recording")?,
        "recording.timestamp",
    )?
    .to_string();
    let program_file = parse_string(
        get_required(obj, "program_file", "recording")?,
        "recording.program_file",
    )?
    .to_string();
    let module_root = parse_string(
        get_required(obj, "module_root", "recording")?,
        "recording.module_root",
    )?
    .to_string();
    let entry_fn = parse_string(
        get_required(obj, "entry_fn", "recording")?,
        "recording.entry_fn",
    )?
    .to_string();

    let input = get_required(obj, "input", "recording")?.clone();

    let effects_json = parse_array(
        get_required(obj, "effects", "recording")?,
        "recording.effects",
    )?;
    let mut effects = Vec::with_capacity(effects_json.len());
    for (idx, effect_json) in effects_json.iter().enumerate() {
        let path = format!("recording.effects[{}]", idx);
        effects.push(effect_record_from_json(effect_json, &path)?);
    }

    let output = outcome_from_json(
        get_required(obj, "output", "recording")?,
        "recording.output",
    )?;

    Ok(SessionRecording {
        schema_version,
        request_id,
        timestamp,
        program_file,
        module_root,
        entry_fn,
        input,
        effects,
        output,
    })
}

fn effect_record_to_json(effect: &EffectRecord) -> JsonValue {
    let mut obj = BTreeMap::new();
    obj.insert("seq".to_string(), JsonValue::Int(effect.seq as i64));
    obj.insert(
        "type".to_string(),
        JsonValue::String(effect.effect_type.clone()),
    );
    obj.insert("args".to_string(), JsonValue::Array(effect.args.clone()));
    obj.insert("outcome".to_string(), outcome_to_json(&effect.outcome));
    JsonValue::Object(obj)
}

fn effect_record_from_json(json: &JsonValue, path: &str) -> Result<EffectRecord, String> {
    let obj = expect_object(json, path)?;
    let seq = parse_u32(get_required(obj, "seq", path)?, &format!("{}.seq", path))?;
    let effect_type =
        parse_string(get_required(obj, "type", path)?, &format!("{}.type", path))?.to_string();
    let args = parse_array(get_required(obj, "args", path)?, &format!("{}.args", path))?.clone();
    let outcome = outcome_from_json(
        get_required(obj, "outcome", path)?,
        &format!("{}.outcome", path),
    )?;

    Ok(EffectRecord {
        seq,
        effect_type,
        args,
        outcome,
    })
}

fn outcome_to_json(outcome: &RecordedOutcome) -> JsonValue {
    let mut obj = BTreeMap::new();
    match outcome {
        RecordedOutcome::Value(value) => {
            obj.insert("kind".to_string(), JsonValue::String("value".to_string()));
            obj.insert("value".to_string(), value.clone());
        }
        RecordedOutcome::RuntimeError(message) => {
            obj.insert(
                "kind".to_string(),
                JsonValue::String("runtime_error".to_string()),
            );
            obj.insert("message".to_string(), JsonValue::String(message.clone()));
        }
    }
    JsonValue::Object(obj)
}

fn outcome_from_json(json: &JsonValue, path: &str) -> Result<RecordedOutcome, String> {
    let obj = expect_object(json, path)?;
    let kind = parse_string(get_required(obj, "kind", path)?, &format!("{}.kind", path))?;

    match kind {
        "value" => Ok(RecordedOutcome::Value(
            get_required(obj, "value", path)?.clone(),
        )),
        "runtime_error" => Ok(RecordedOutcome::RuntimeError(
            parse_string(
                get_required(obj, "message", path)?,
                &format!("{}.message", path),
            )?
            .to_string(),
        )),
        _ => Err(format!("{}: unknown outcome kind '{}'", path, kind)),
    }
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

fn parse_u32(value: &JsonValue, path: &str) -> Result<u32, String> {
    match value {
        JsonValue::Int(n) if *n >= 0 => {
            u32::try_from(*n).map_err(|_| format!("{} out of range for u32", path))
        }
        JsonValue::Float(n) if *n >= 0.0 && n.fract() == 0.0 => {
            let as_i64 = *n as i64;
            u32::try_from(as_i64).map_err(|_| format!("{} out of range for u32", path))
        }
        _ => Err(format!("{} must be a non-negative integer", path)),
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
        Value::List(_) | Value::ListSlice { .. } => {
            let items =
                list_slice(value).ok_or_else(|| "invalid list representation".to_string())?;
            let mut arr = Vec::with_capacity(items.len());
            for item in items {
                arr.push(value_to_json(item)?);
            }
            Ok(JsonValue::Array(arr))
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
            Ok(Value::List(out))
        }
        JsonValue::Object(obj) => {
            if let Some((marker, payload)) = marker_single_key(obj) {
                return decode_marker(marker, payload);
            }
            let mut fields = Vec::with_capacity(obj.len());
            for (k, v) in obj {
                fields.push((k.clone(), json_to_value(v)?));
            }
            Ok(Value::Record {
                type_name: "JsonObject".to_string(),
                fields,
            })
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
        "$record" => decode_record(payload),
        "$variant" => decode_variant(payload),
        _ => Err(format!("unknown replay marker '{}'", marker)),
    }
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

struct JsonParser<'a> {
    src: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> JsonParser<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            bytes: src.as_bytes(),
            pos: 0,
        }
    }

    fn parse(mut self) -> Result<JsonValue, String> {
        self.skip_ws();
        let value = self.parse_value()?;
        self.skip_ws();
        if self.pos != self.bytes.len() {
            return Err(self.error("trailing characters after JSON value"));
        }
        Ok(value)
    }

    fn parse_value(&mut self) -> Result<JsonValue, String> {
        self.skip_ws();
        let Some(byte) = self.peek() else {
            return Err(self.error("unexpected end of input"));
        };

        match byte {
            b'n' => {
                self.expect_keyword("null")?;
                Ok(JsonValue::Null)
            }
            b't' => {
                self.expect_keyword("true")?;
                Ok(JsonValue::Bool(true))
            }
            b'f' => {
                self.expect_keyword("false")?;
                Ok(JsonValue::Bool(false))
            }
            b'"' => Ok(JsonValue::String(self.parse_string()?)),
            b'[' => self.parse_array(),
            b'{' => self.parse_object(),
            b'-' | b'0'..=b'9' => self.parse_number(),
            _ => Err(self.error("unexpected token")),
        }
    }

    fn parse_array(&mut self) -> Result<JsonValue, String> {
        self.expect_byte(b'[')?;
        self.skip_ws();

        let mut items = Vec::new();
        if self.peek() == Some(b']') {
            self.pos += 1;
            return Ok(JsonValue::Array(items));
        }

        loop {
            items.push(self.parse_value()?);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                    self.skip_ws();
                }
                Some(b']') => {
                    self.pos += 1;
                    break;
                }
                _ => return Err(self.error("expected ',' or ']' in array")),
            }
        }

        Ok(JsonValue::Array(items))
    }

    fn parse_object(&mut self) -> Result<JsonValue, String> {
        self.expect_byte(b'{')?;
        self.skip_ws();

        let mut fields = BTreeMap::new();
        if self.peek() == Some(b'}') {
            self.pos += 1;
            return Ok(JsonValue::Object(fields));
        }

        loop {
            let key = self.parse_string()?;
            self.skip_ws();
            self.expect_byte(b':')?;
            self.skip_ws();
            let value = self.parse_value()?;
            fields.insert(key, value);
            self.skip_ws();

            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                    self.skip_ws();
                }
                Some(b'}') => {
                    self.pos += 1;
                    break;
                }
                _ => return Err(self.error("expected ',' or '}' in object")),
            }
        }

        Ok(JsonValue::Object(fields))
    }

    fn parse_string(&mut self) -> Result<String, String> {
        self.expect_byte(b'"')?;
        let mut out = String::new();
        let mut chunk_start = self.pos;

        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            match b {
                b'"' => {
                    if chunk_start < self.pos {
                        out.push_str(
                            std::str::from_utf8(&self.bytes[chunk_start..self.pos])
                                .map_err(|_| self.error("invalid UTF-8 in string"))?,
                        );
                    }
                    self.pos += 1;
                    return Ok(out);
                }
                b'\\' => {
                    if chunk_start < self.pos {
                        out.push_str(
                            std::str::from_utf8(&self.bytes[chunk_start..self.pos])
                                .map_err(|_| self.error("invalid UTF-8 in string"))?,
                        );
                    }
                    self.pos += 1;
                    out.push(self.parse_escape_sequence()?);
                    chunk_start = self.pos;
                }
                0x00..=0x1F => {
                    return Err(self.error("control character in string literal"));
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        Err(self.error("unterminated string literal"))
    }

    fn parse_escape_sequence(&mut self) -> Result<char, String> {
        let Some(ch) = self.next_byte() else {
            return Err(self.error("unterminated escape sequence"));
        };

        match ch {
            b'"' => Ok('"'),
            b'\\' => Ok('\\'),
            b'/' => Ok('/'),
            b'b' => Ok('\u{08}'),
            b'f' => Ok('\u{0C}'),
            b'n' => Ok('\n'),
            b'r' => Ok('\r'),
            b't' => Ok('\t'),
            b'u' => self.parse_unicode_escape(),
            _ => Err(self.error("invalid escape sequence")),
        }
    }

    fn parse_unicode_escape(&mut self) -> Result<char, String> {
        let first = self.parse_hex_u16()?;

        if (0xD800..=0xDBFF).contains(&first) {
            self.expect_byte(b'\\')?;
            self.expect_byte(b'u')?;
            let second = self.parse_hex_u16()?;
            if !(0xDC00..=0xDFFF).contains(&second) {
                return Err(self.error("invalid low surrogate in unicode escape"));
            }
            let high = (first as u32) - 0xD800;
            let low = (second as u32) - 0xDC00;
            let codepoint = 0x10000 + ((high << 10) | low);
            return char::from_u32(codepoint)
                .ok_or_else(|| self.error("invalid unicode codepoint"));
        }

        if (0xDC00..=0xDFFF).contains(&first) {
            return Err(self.error("unexpected low surrogate in unicode escape"));
        }

        char::from_u32(first as u32).ok_or_else(|| self.error("invalid unicode codepoint"))
    }

    fn parse_hex_u16(&mut self) -> Result<u16, String> {
        let mut value: u16 = 0;
        for _ in 0..4 {
            let Some(b) = self.next_byte() else {
                return Err(self.error("incomplete unicode escape"));
            };
            value = value
                .checked_mul(16)
                .ok_or_else(|| self.error("unicode escape overflow"))?;
            value = value
                .checked_add(hex_digit(b).ok_or_else(|| self.error("invalid hex digit"))? as u16)
                .ok_or_else(|| self.error("unicode escape overflow"))?;
        }
        Ok(value)
    }

    fn parse_number(&mut self) -> Result<JsonValue, String> {
        let start = self.pos;

        if self.peek() == Some(b'-') {
            self.pos += 1;
        }

        match self.peek() {
            Some(b'0') => {
                self.pos += 1;
                if let Some(b'0'..=b'9') = self.peek() {
                    return Err(self.error("leading zero in number"));
                }
            }
            Some(b'1'..=b'9') => {
                self.pos += 1;
                while let Some(b'0'..=b'9') = self.peek() {
                    self.pos += 1;
                }
            }
            _ => return Err(self.error("invalid number")),
        }

        let mut is_float = false;

        if self.peek() == Some(b'.') {
            is_float = true;
            self.pos += 1;
            let frac_start = self.pos;
            while let Some(b'0'..=b'9') = self.peek() {
                self.pos += 1;
            }
            if self.pos == frac_start {
                return Err(self.error("missing digits after decimal point"));
            }
        }

        if matches!(self.peek(), Some(b'e' | b'E')) {
            is_float = true;
            self.pos += 1;
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.pos += 1;
            }
            let exp_start = self.pos;
            while let Some(b'0'..=b'9') = self.peek() {
                self.pos += 1;
            }
            if self.pos == exp_start {
                return Err(self.error("missing exponent digits"));
            }
        }

        let number_text = &self.src[start..self.pos];
        if is_float {
            let value = number_text
                .parse::<f64>()
                .map_err(|_| self.error("invalid floating-point number"))?;
            if !value.is_finite() {
                return Err(self.error("non-finite number is not allowed"));
            }
            Ok(JsonValue::Float(value))
        } else {
            let value = number_text
                .parse::<i64>()
                .map_err(|_| self.error("integer out of i64 range"))?;
            Ok(JsonValue::Int(value))
        }
    }

    fn expect_keyword(&mut self, keyword: &str) -> Result<(), String> {
        let end = self.pos + keyword.len();
        if end > self.bytes.len() || &self.src[self.pos..end] != keyword {
            return Err(self.error(&format!("expected '{}'", keyword)));
        }
        self.pos = end;
        Ok(())
    }

    fn expect_byte(&mut self, expected: u8) -> Result<(), String> {
        match self.next_byte() {
            Some(b) if b == expected => Ok(()),
            _ => Err(self.error(&format!("expected '{}'", expected as char))),
        }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn next_byte(&mut self) -> Option<u8> {
        let b = self.peek()?;
        self.pos += 1;
        Some(b)
    }

    fn skip_ws(&mut self) {
        while let Some(b) = self.peek() {
            if matches!(b, b' ' | b'\n' | b'\r' | b'\t') {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn error(&self, msg: &str) -> String {
        format!("JSON parse error at byte {}: {}", self.pos, msg)
    }
}

fn hex_digit(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}
