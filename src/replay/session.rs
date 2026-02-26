use std::collections::BTreeMap;

use super::json::{format_json, parse_json, JsonValue};

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
