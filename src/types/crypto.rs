/// Crypto namespace — pure cryptographic operations over standard-library
/// byte refinements.
///
/// Methods:
///   Crypto.sha256(bytes: Bytes) → Digest32
///
/// `Bytes` guarantees octets and SHA-256 always returns exactly 32 of them, so
/// the public operation is total and its result preserves that fact nominally.
// Deliberately `sha2` directly, NOT `aver_rt::crypto`: `aver-rt` is an
// optional dependency (behind the `runtime` feature), while this module
// compiles in every feature combination that builds `src/types`. The
// `aver_rt::crypto` helper exists for generated Rust projects, which
// always link `aver-rt`.
use sha2::{Digest, Sha256};

use crate::nan_value::{Arena, NanValue, NanValueConvert};
use crate::value::{RuntimeError, Value};

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
    let bytes = super::bytes::project(&args[0], "Crypto.sha256")?;

    let digest = Sha256::digest(&bytes);
    let digest_bytes = super::bytes::from_host(&digest);
    Ok(Value::Record {
        type_name: "Digest32".to_string(),
        fields: vec![("bytes".to_string(), digest_bytes)].into(),
    })
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

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
