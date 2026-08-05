/// Crypto namespace — pure cryptographic operations over standard-library
/// byte refinements.
///
/// Methods:
///   Crypto.sha256(bytes: Bytes) → Digest32
///
/// `Bytes` guarantees octets and SHA-256 always returns exactly 32 of them, so
/// the public operation is total and its result preserves that fact nominally.
use std::collections::HashMap;
use std::sync::Arc as Rc;

use aver_rt::AverList;
use sha2::{Digest, Sha256};

use crate::nan_value::{Arena, NanValue, NanValueConvert};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    members.insert(
        "sha256".to_string(),
        Value::Builtin("Crypto.sha256".to_string()),
    );
    global.insert(
        "Crypto".to_string(),
        Value::Namespace {
            name: "Crypto".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Crypto.sha256" => Some(sha256(args)),
        _ => None,
    }
}

fn sha256(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Crypto.sha256() takes 1 argument (bytes), got {}",
            args.len()
        )));
    }
    let bytes = bytes_arg(&args[0], "Crypto.sha256")?;

    let digest = Sha256::digest(&bytes);
    let items: Vec<Value> = digest.iter().map(|b| Value::int(*b as i64)).collect();
    let digest_bytes = Value::Record {
        type_name: "Bytes".to_string(),
        fields: vec![("values".to_string(), Value::List(AverList::from_vec(items)))].into(),
    };
    Ok(Value::Record {
        type_name: "Digest32".to_string(),
        fields: vec![("bytes".to_string(), digest_bytes)].into(),
    })
}

/// Project the standard-library `Bytes` carrier into host bytes.
///
/// User code can only obtain `Bytes` through its validating constructors. The
/// range checks below defend the host boundary against malformed internal
/// values; violating the refinement is a runtime invariant error rather than a
/// catchable branch in SHA-256's public type.
fn bytes_arg(val: &Value, method: &str) -> Result<Vec<u8>, RuntimeError> {
    let Value::Record { type_name, fields } = val else {
        return Err(RuntimeError::Error(format!(
            "{method}: argument must be Bytes"
        )));
    };
    if type_name.rsplit('.').next() != Some("Bytes") {
        return Err(RuntimeError::Error(format!(
            "{method}: argument must be Bytes"
        )));
    }
    let Some((_, Value::List(items))) = fields.iter().find(|(name, _)| name == "values") else {
        return Err(RuntimeError::Error(format!(
            "{method}: malformed Bytes carrier"
        )));
    };
    let mut out = Vec::with_capacity(items.len());
    for (idx, item) in items.iter().enumerate() {
        let Value::Int(n) = item else {
            return Err(RuntimeError::Error(format!(
                "{method}: malformed Bytes value at index {idx}"
            )));
        };
        let Some(n) = n.to_i64() else {
            return Err(RuntimeError::Error(format!(
                "{method}: malformed Bytes value at index {idx}"
            )));
        };
        let byte = u8::try_from(n).map_err(|_| {
            RuntimeError::Error(format!("{method}: malformed Bytes value at index {idx}"))
        })?;
        out.push(byte);
    }
    Ok(out)
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let idx = arena.push_builtin("Crypto.sha256");
    let members: Vec<(Rc<str>, NanValue)> = vec![(Rc::from("sha256"), NanValue::new_builtin(idx))];
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Crypto"),
        members,
    });
    global.insert("Crypto".to_string(), NanValue::new_namespace(ns_idx));
}

/// Bridge: convert NanValue args to Value, hash, convert the result back.
/// The conversion is negligible next to the digest itself, so the shared
/// implementation is worth more here than a hand-written native path.
pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    if !matches!(name, "Crypto.sha256") {
        return None;
    }
    let old_args: Vec<Value> = args.iter().map(|nv| nv.to_value(arena)).collect();
    let result = call(name, &old_args)?;
    Some(result.map(|v| NanValue::from_value(&v, arena)))
}
