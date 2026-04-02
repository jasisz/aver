use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use serde_json::Value as SerdeJsonValue;

use super::json::JsonValue;

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
    /// Name of the function that made this effect call (empty = unknown).
    pub caller_fn: String,
    /// Source line of the call expression that triggered this effect (0 = unknown).
    pub source_line: usize,
    /// Independent product group: effects sharing a `group_id` are order-independent.
    /// During replay, effects within a group are matched by (branch_path, type, args).
    pub group_id: Option<u32>,
    /// Branch path within nested independent products (e.g. "0.1" = branch 0 of outer,
    /// branch 1 of inner). Disambiguates effects from different branches.
    pub branch_path: Option<String>,
    /// Per-branch ordinal of effect emission (0-based). Disambiguates multiple
    /// emissions of the same effect type+args within a single branch.
    pub branch_occurrence: Option<u32>,
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct SerdeSessionRecording {
    schema_version: u32,
    request_id: String,
    timestamp: String,
    program_file: String,
    module_root: String,
    entry_fn: String,
    input: SerdeJsonValue,
    effects: Vec<SerdeEffectRecord>,
    output: SerdeRecordedOutcome,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct SerdeEffectRecord {
    seq: u32,
    #[serde(rename = "type")]
    effect_type: String,
    args: Vec<SerdeJsonValue>,
    outcome: SerdeRecordedOutcome,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    caller_fn: String,
    #[serde(default, skip_serializing_if = "is_zero")]
    source_line: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    group_id: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    branch_path: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    branch_occurrence: Option<u32>,
}

fn is_zero(v: &usize) -> bool {
    *v == 0
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind")]
enum SerdeRecordedOutcome {
    #[serde(rename = "value")]
    Value { value: SerdeJsonValue },
    #[serde(rename = "runtime_error")]
    RuntimeError { message: String },
}

pub fn parse_session_recording(input: &str) -> Result<SessionRecording, String> {
    let recording: SerdeSessionRecording =
        serde_json::from_str(input).map_err(|e| format!("invalid replay recording: {}", e))?;
    session_recording_from_serde(recording)
}

pub fn session_recording_to_string_pretty(recording: &SessionRecording) -> String {
    serde_json::to_string_pretty(&session_recording_to_serde(recording))
        .expect("SessionRecording should always serialize to JSON")
}

pub fn session_recording_to_json(recording: &SessionRecording) -> JsonValue {
    let value = serde_json::to_value(session_recording_to_serde(recording))
        .expect("SessionRecording should always convert to serde_json::Value");
    serde_to_json_value(value).expect("serde_json::Value should always convert to JsonValue")
}

pub fn session_recording_from_json(json: &JsonValue) -> Result<SessionRecording, String> {
    let value = json_value_to_serde(json);
    let recording: SerdeSessionRecording =
        serde_json::from_value(value).map_err(|e| format!("invalid replay recording: {}", e))?;
    session_recording_from_serde(recording)
}

fn session_recording_to_serde(recording: &SessionRecording) -> SerdeSessionRecording {
    SerdeSessionRecording {
        schema_version: recording.schema_version,
        request_id: recording.request_id.clone(),
        timestamp: recording.timestamp.clone(),
        program_file: recording.program_file.clone(),
        module_root: recording.module_root.clone(),
        entry_fn: recording.entry_fn.clone(),
        input: json_value_to_serde(&recording.input),
        effects: recording
            .effects
            .iter()
            .map(effect_record_to_serde)
            .collect(),
        output: outcome_to_serde(&recording.output),
    }
}

fn session_recording_from_serde(
    recording: SerdeSessionRecording,
) -> Result<SessionRecording, String> {
    Ok(SessionRecording {
        schema_version: recording.schema_version,
        request_id: recording.request_id,
        timestamp: recording.timestamp,
        program_file: recording.program_file,
        module_root: recording.module_root,
        entry_fn: recording.entry_fn,
        input: serde_to_json_value(recording.input)?,
        effects: recording
            .effects
            .into_iter()
            .map(effect_record_from_serde)
            .collect::<Result<Vec<_>, _>>()?,
        output: outcome_from_serde(recording.output)?,
    })
}

fn effect_record_to_serde(effect: &EffectRecord) -> SerdeEffectRecord {
    SerdeEffectRecord {
        seq: effect.seq,
        effect_type: effect.effect_type.clone(),
        args: effect.args.iter().map(json_value_to_serde).collect(),
        outcome: outcome_to_serde(&effect.outcome),
        caller_fn: effect.caller_fn.clone(),
        source_line: effect.source_line,
        group_id: effect.group_id,
        branch_path: effect.branch_path.clone(),
        branch_occurrence: effect.branch_occurrence,
    }
}

fn effect_record_from_serde(effect: SerdeEffectRecord) -> Result<EffectRecord, String> {
    Ok(EffectRecord {
        seq: effect.seq,
        effect_type: effect.effect_type,
        args: effect
            .args
            .into_iter()
            .map(serde_to_json_value)
            .collect::<Result<Vec<_>, _>>()?,
        outcome: outcome_from_serde(effect.outcome)?,
        caller_fn: effect.caller_fn,
        source_line: effect.source_line,
        group_id: effect.group_id,
        branch_path: effect.branch_path,
        branch_occurrence: effect.branch_occurrence,
    })
}

fn outcome_to_serde(outcome: &RecordedOutcome) -> SerdeRecordedOutcome {
    match outcome {
        RecordedOutcome::Value(value) => SerdeRecordedOutcome::Value {
            value: json_value_to_serde(value),
        },
        RecordedOutcome::RuntimeError(message) => SerdeRecordedOutcome::RuntimeError {
            message: message.clone(),
        },
    }
}

fn outcome_from_serde(outcome: SerdeRecordedOutcome) -> Result<RecordedOutcome, String> {
    match outcome {
        SerdeRecordedOutcome::Value { value } => {
            Ok(RecordedOutcome::Value(serde_to_json_value(value)?))
        }
        SerdeRecordedOutcome::RuntimeError { message } => {
            Ok(RecordedOutcome::RuntimeError(message))
        }
    }
}

fn json_value_to_serde(value: &JsonValue) -> SerdeJsonValue {
    match value {
        JsonValue::Null => SerdeJsonValue::Null,
        JsonValue::Bool(b) => SerdeJsonValue::Bool(*b),
        JsonValue::Int(i) => SerdeJsonValue::Number((*i).into()),
        JsonValue::Float(f) => SerdeJsonValue::Number(
            serde_json::Number::from_f64(*f).expect("replay JSON cannot encode non-finite floats"),
        ),
        JsonValue::String(s) => SerdeJsonValue::String(s.clone()),
        JsonValue::Array(items) => {
            SerdeJsonValue::Array(items.iter().map(json_value_to_serde).collect())
        }
        JsonValue::Object(obj) => SerdeJsonValue::Object(
            obj.iter()
                .map(|(k, v)| (k.clone(), json_value_to_serde(v)))
                .collect(),
        ),
    }
}

fn serde_to_json_value(value: SerdeJsonValue) -> Result<JsonValue, String> {
    match value {
        SerdeJsonValue::Null => Ok(JsonValue::Null),
        SerdeJsonValue::Bool(b) => Ok(JsonValue::Bool(b)),
        SerdeJsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(JsonValue::Int(i))
            } else if let Some(u) = n.as_u64() {
                let i = i64::try_from(u)
                    .map_err(|_| format!("JSON integer {} is out of range for i64", u))?;
                Ok(JsonValue::Int(i))
            } else if let Some(f) = n.as_f64() {
                Ok(JsonValue::Float(f))
            } else {
                Err(format!("unsupported JSON number: {}", n))
            }
        }
        SerdeJsonValue::String(s) => Ok(JsonValue::String(s)),
        SerdeJsonValue::Array(items) => Ok(JsonValue::Array(
            items
                .into_iter()
                .map(serde_to_json_value)
                .collect::<Result<Vec<_>, _>>()?,
        )),
        SerdeJsonValue::Object(obj) => {
            let mut out = BTreeMap::new();
            for (key, value) in obj {
                out.insert(key, serde_to_json_value(value)?);
            }
            Ok(JsonValue::Object(out))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn session_recording_json_shape_stays_stable() {
        let recording = SessionRecording {
            schema_version: 1,
            request_id: "rec-1".to_string(),
            timestamp: "unix-1".to_string(),
            program_file: "examples/app.av".to_string(),
            module_root: ".".to_string(),
            entry_fn: "main".to_string(),
            input: JsonValue::Null,
            effects: vec![EffectRecord {
                seq: 1,
                effect_type: "Console.print".to_string(),
                args: vec![JsonValue::String("hi".to_string())],
                outcome: RecordedOutcome::Value(JsonValue::Null),
                caller_fn: String::new(),
                source_line: 0,
                group_id: None,
                branch_path: None,
                branch_occurrence: None,
            }],
            output: RecordedOutcome::RuntimeError("boom".to_string()),
        };

        let json = session_recording_to_json(&recording);
        let JsonValue::Object(root) = json else {
            panic!("recording should serialize as object");
        };
        let JsonValue::Array(effects) = root.get("effects").expect("effects field") else {
            panic!("effects should be array");
        };
        let JsonValue::Object(effect) = &effects[0] else {
            panic!("effect should be object");
        };
        let JsonValue::String(effect_type) = effect.get("type").expect("type field") else {
            panic!("type should be string");
        };
        assert_eq!(effect_type, "Console.print");

        let JsonValue::Object(output) = root.get("output").expect("output field") else {
            panic!("output should be object");
        };
        let JsonValue::String(kind) = output.get("kind").expect("kind field") else {
            panic!("kind should be string");
        };
        assert_eq!(kind, "runtime_error");
    }

    #[test]
    fn parse_session_recording_roundtrips_existing_shape() {
        let raw = r#"{
  "schema_version": 1,
  "request_id": "rec-1",
  "timestamp": "unix-1",
  "program_file": "examples/app.av",
  "module_root": ".",
  "entry_fn": "main",
  "input": null,
  "effects": [
    {
      "seq": 1,
      "type": "Console.print",
      "args": ["hi"],
      "outcome": { "kind": "value", "value": null }
    }
  ],
  "output": { "kind": "value", "value": {"ok": true} }
}"#;

        let recording = parse_session_recording(raw).expect("parse recording");
        assert_eq!(recording.effects[0].effect_type, "Console.print");
        assert_eq!(
            recording.effects[0].args,
            vec![JsonValue::String("hi".to_string())]
        );
        assert_eq!(
            recording.output,
            RecordedOutcome::Value(JsonValue::Object(BTreeMap::from([(
                "ok".to_string(),
                JsonValue::Bool(true),
            )])))
        );
    }
}
