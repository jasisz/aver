use aver_rt::provider::ProviderValue;

/// Canonical structural bytes used only to order provider-bound map entries.
/// This is not a contract hash or a serialization format. Length framing and
/// exact Float bits make it a deterministic total order even for distinct NaN
/// payloads whose human-readable representations are identical.
pub(super) fn provider_value_order_key(value: &ProviderValue) -> Result<Vec<u8>, String> {
    let mut out = Vec::new();
    encode(value, &mut out)?;
    Ok(out)
}

fn encode(value: &ProviderValue, out: &mut Vec<u8>) -> Result<(), String> {
    match value {
        ProviderValue::Int(value) => {
            out.push(0);
            push_bytes(out, value.to_string().as_bytes());
        }
        ProviderValue::Float(value) => {
            out.push(1);
            out.extend_from_slice(&value.to_bits().to_be_bytes());
        }
        ProviderValue::String(value) => {
            out.push(2);
            push_bytes(out, value.as_bytes());
        }
        ProviderValue::Bool(value) => {
            out.extend_from_slice(&[3, u8::from(*value)]);
        }
        ProviderValue::Unit => out.push(4),
        ProviderValue::Tuple(values) => encode_sequence(5, values, out)?,
        ProviderValue::List(values) => encode_sequence(6, values, out)?,
        ProviderValue::Vector(values) => encode_sequence(7, values, out)?,
        ProviderValue::Map(entries) => {
            out.push(8);
            push_len(out, entries.len());
            for (key, value) in entries {
                encode_framed(key, out)?;
                encode_framed(value, out)?;
            }
        }
        ProviderValue::ResultOk(value) => {
            out.push(9);
            encode_framed(value, out)?;
        }
        ProviderValue::ResultErr(value) => {
            out.push(10);
            encode_framed(value, out)?;
        }
        ProviderValue::OptionSome(value) => {
            out.push(11);
            encode_framed(value, out)?;
        }
        ProviderValue::OptionNone => out.push(12),
        ProviderValue::Record { type_name, fields } => {
            out.push(13);
            push_bytes(out, type_name.as_bytes());
            push_len(out, fields.len());
            for (name, value) in fields {
                push_bytes(out, name.as_bytes());
                encode_framed(value, out)?;
            }
        }
        ProviderValue::Variant {
            type_name,
            variant,
            fields,
        } => {
            out.push(14);
            push_bytes(out, type_name.as_bytes());
            push_bytes(out, variant.as_bytes());
            encode_sequence(15, fields, out)?;
        }
        ProviderValue::Resource(_) => {
            return Err(
                "capability resources cannot be ordered or used as provider Map keys".to_string(),
            );
        }
    }
    Ok(())
}

fn encode_sequence(tag: u8, values: &[ProviderValue], out: &mut Vec<u8>) -> Result<(), String> {
    out.push(tag);
    push_len(out, values.len());
    for value in values {
        encode_framed(value, out)?;
    }
    Ok(())
}

fn encode_framed(value: &ProviderValue, out: &mut Vec<u8>) -> Result<(), String> {
    let mut encoded = Vec::new();
    encode(value, &mut encoded)?;
    push_bytes(out, &encoded);
    Ok(())
}

fn push_bytes(out: &mut Vec<u8>, bytes: &[u8]) {
    push_len(out, bytes.len());
    out.extend_from_slice(bytes);
}

fn push_len(out: &mut Vec<u8>, len: usize) {
    out.extend_from_slice(&(len as u64).to_be_bytes());
}
