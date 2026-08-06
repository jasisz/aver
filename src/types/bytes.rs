//! Runtime bridge for the source-defined `Bytes` refinement.
//!
//! `Bytes` remains ordinary Aver source in `stdlib/bytes.av`. Host-backed
//! builtins use these helpers only at the trusted boundary where its opaque
//! record must become native octets or native octets must re-enter Aver.

use aver_rt::AverList;

use crate::value::{RuntimeError, Value};

/// Project a validated `Bytes` record into host octets.
///
/// User code cannot forge this carrier because the type is opaque. The checks
/// remain fail-closed so an internal compiler/runtime bug cannot silently feed
/// malformed data into a host primitive.
pub(crate) fn project(value: &Value, method: &str) -> Result<Vec<u8>, RuntimeError> {
    let Value::Record { type_name, fields } = value else {
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
        let Value::Int(value) = item else {
            return Err(RuntimeError::Error(format!(
                "{method}: malformed Bytes value at index {idx}"
            )));
        };
        let Some(value) = value.to_i64() else {
            return Err(RuntimeError::Error(format!(
                "{method}: byte {value} at index {idx} is out of range (0–255)"
            )));
        };
        let byte = u8::try_from(value).map_err(|_| {
            RuntimeError::Error(format!(
                "{method}: byte {value} at index {idx} is out of range (0–255)"
            ))
        })?;
        out.push(byte);
    }
    Ok(out)
}

/// Re-enter Aver through the nominal `Bytes` carrier.
pub(crate) fn from_host(bytes: &[u8]) -> Value {
    let items = bytes
        .iter()
        .map(|byte| Value::int(i64::from(*byte)))
        .collect();
    Value::Record {
        type_name: "Bytes".to_string(),
        fields: vec![("values".to_string(), Value::List(AverList::from_vec(items)))].into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn host_round_trip_preserves_octets() {
        let value = from_host(&[0, 127, 255]);
        assert_eq!(project(&value, "test").unwrap(), vec![0, 127, 255]);
    }

    #[test]
    fn projection_rejects_a_spoofed_out_of_range_carrier() {
        let value = Value::Record {
            type_name: "Bytes".to_string(),
            fields: vec![(
                "values".to_string(),
                Value::List(AverList::from_vec(vec![Value::int(256)])),
            )]
            .into(),
        };
        let error = project(&value, "test").unwrap_err().to_string();
        assert!(error.contains("byte 256 at index 0"), "{error}");
    }
}
