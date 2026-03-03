use super::Value;

/// Hash a slice of memo-safe argument values into a single u64.
pub fn hash_memo_args(args: &[Value]) -> u64 {
    use std::hash::Hasher;
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for arg in args {
        hash_value(arg, &mut hasher);
    }
    hasher.finish()
}

fn hash_value(val: &Value, hasher: &mut impl std::hash::Hasher) {
    use std::hash::Hash;
    match val {
        Value::Int(i) => {
            0u8.hash(hasher);
            i.hash(hasher);
        }
        Value::Float(f) => {
            1u8.hash(hasher);
            f.to_bits().hash(hasher);
        }
        Value::Str(s) => {
            2u8.hash(hasher);
            s.hash(hasher);
        }
        Value::Bool(b) => {
            3u8.hash(hasher);
            b.hash(hasher);
        }
        Value::Unit => {
            4u8.hash(hasher);
        }
        Value::Tuple(items) => {
            7u8.hash(hasher);
            items.len().hash(hasher);
            for item in items {
                hash_value(item, hasher);
            }
        }
        Value::Record { type_name, fields } => {
            5u8.hash(hasher);
            type_name.hash(hasher);
            for (k, v) in fields {
                k.hash(hasher);
                hash_value(v, hasher);
            }
        }
        Value::Variant {
            type_name,
            variant,
            fields,
        } => {
            6u8.hash(hasher);
            type_name.hash(hasher);
            variant.hash(hasher);
            for v in fields {
                hash_value(v, hasher);
            }
        }
        // Should not reach here for non-memo-safe values; no-op to avoid panic.
        _ => {
            255u8.hash(hasher);
        }
    }
}
