use std::sync::Arc;

use crate::AverInt;

use super::{NativeProviderRegistry, ProviderValue};

/// Conversion implemented by native generated-Rust boundary types.
///
/// Built-in first-order types implement it in `aver-rt`; generated records,
/// sums, and opaque resource wrappers implement it in the generated crate.
/// The registry arguments are inert for ordinary data and are used only by a
/// resource codec to resolve/store the provider-private payload.
pub trait ProviderCodec: Sized {
    fn into_provider_value(
        self,
        registry: &NativeProviderRegistry,
        capability: &str,
    ) -> Result<ProviderValue, String>;

    fn from_provider_value(
        value: ProviderValue,
        registry: &NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String>;
}

macro_rules! scalar_codec {
    ($rust:ty, $variant:ident, $label:literal) => {
        impl ProviderCodec for $rust {
            fn into_provider_value(
                self,
                _registry: &NativeProviderRegistry,
                _capability: &str,
            ) -> Result<ProviderValue, String> {
                Ok(ProviderValue::$variant(self))
            }

            fn from_provider_value(
                value: ProviderValue,
                _registry: &NativeProviderRegistry,
                _capability: &str,
                _minted_resource: Option<&str>,
            ) -> Result<Self, String> {
                match value {
                    ProviderValue::$variant(value) => Ok(value),
                    other => Err(format!("expected {}, got {}", $label, other.shape())),
                }
            }
        }
    };
}

scalar_codec!(AverInt, Int, "Int");
scalar_codec!(f64, Float, "Float");
scalar_codec!(bool, Bool, "Bool");

impl ProviderCodec for crate::AverStr {
    fn into_provider_value(
        self,
        _registry: &NativeProviderRegistry,
        _capability: &str,
    ) -> Result<ProviderValue, String> {
        Ok(ProviderValue::String(self.to_string()))
    }

    fn from_provider_value(
        value: ProviderValue,
        _registry: &NativeProviderRegistry,
        _capability: &str,
        _minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        match value {
            ProviderValue::String(value) => Ok(value.into()),
            other => Err(format!("expected String, got {}", other.shape())),
        }
    }
}

impl ProviderCodec for () {
    fn into_provider_value(
        self,
        _registry: &NativeProviderRegistry,
        _capability: &str,
    ) -> Result<ProviderValue, String> {
        Ok(ProviderValue::Unit)
    }

    fn from_provider_value(
        value: ProviderValue,
        _registry: &NativeProviderRegistry,
        _capability: &str,
        _minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        match value {
            ProviderValue::Unit => Ok(()),
            other => Err(format!("expected Unit, got {}", other.shape())),
        }
    }
}

impl<T: ProviderCodec + Clone> ProviderCodec for Arc<T> {
    fn into_provider_value(
        self,
        registry: &NativeProviderRegistry,
        capability: &str,
    ) -> Result<ProviderValue, String> {
        (*self).clone().into_provider_value(registry, capability)
    }

    fn from_provider_value(
        value: ProviderValue,
        registry: &NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        T::from_provider_value(value, registry, capability, minted_resource).map(Arc::new)
    }
}

impl<T: ProviderCodec> ProviderCodec for Option<T> {
    fn into_provider_value(
        self,
        registry: &NativeProviderRegistry,
        capability: &str,
    ) -> Result<ProviderValue, String> {
        match self {
            Some(value) => Ok(ProviderValue::OptionSome(Box::new(
                value.into_provider_value(registry, capability)?,
            ))),
            None => Ok(ProviderValue::OptionNone),
        }
    }

    fn from_provider_value(
        value: ProviderValue,
        registry: &NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        match value {
            ProviderValue::OptionSome(value) => Ok(Some(T::from_provider_value(
                *value,
                registry,
                capability,
                minted_resource,
            )?)),
            ProviderValue::OptionNone => Ok(None),
            other => Err(format!("expected Option, got {}", other.shape())),
        }
    }
}

impl<T: ProviderCodec, E: ProviderCodec> ProviderCodec for Result<T, E> {
    fn into_provider_value(
        self,
        registry: &NativeProviderRegistry,
        capability: &str,
    ) -> Result<ProviderValue, String> {
        match self {
            Ok(value) => Ok(ProviderValue::ResultOk(Box::new(
                value.into_provider_value(registry, capability)?,
            ))),
            Err(value) => Ok(ProviderValue::ResultErr(Box::new(
                value.into_provider_value(registry, capability)?,
            ))),
        }
    }

    fn from_provider_value(
        value: ProviderValue,
        registry: &NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        match value {
            ProviderValue::ResultOk(value) => Ok(Ok(T::from_provider_value(
                *value,
                registry,
                capability,
                minted_resource,
            )?)),
            ProviderValue::ResultErr(value) => Ok(Err(E::from_provider_value(
                *value,
                registry,
                capability,
                minted_resource,
            )?)),
            other => Err(format!("expected Result, got {}", other.shape())),
        }
    }
}

impl<T: ProviderCodec + Clone> ProviderCodec for crate::AverList<T> {
    fn into_provider_value(
        self,
        registry: &NativeProviderRegistry,
        capability: &str,
    ) -> Result<ProviderValue, String> {
        Ok(ProviderValue::List(
            self.iter()
                .cloned()
                .map(|value| value.into_provider_value(registry, capability))
                .collect::<Result<Vec<_>, _>>()?,
        ))
    }

    fn from_provider_value(
        value: ProviderValue,
        registry: &NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        match value {
            ProviderValue::List(values) => Ok(Self::from_vec(
                values
                    .into_iter()
                    .map(|value| {
                        T::from_provider_value(value, registry, capability, minted_resource)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            other => Err(format!("expected List, got {}", other.shape())),
        }
    }
}

impl<T: ProviderCodec + Clone> ProviderCodec for crate::AverVector<T> {
    fn into_provider_value(
        self,
        registry: &NativeProviderRegistry,
        capability: &str,
    ) -> Result<ProviderValue, String> {
        Ok(ProviderValue::Vector(
            self.iter()
                .cloned()
                .map(|value| value.into_provider_value(registry, capability))
                .collect::<Result<Vec<_>, _>>()?,
        ))
    }

    fn from_provider_value(
        value: ProviderValue,
        registry: &NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        match value {
            ProviderValue::Vector(values) => Ok(Self::from_vec(
                values
                    .into_iter()
                    .map(|value| {
                        T::from_provider_value(value, registry, capability, minted_resource)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            other => Err(format!("expected Vector, got {}", other.shape())),
        }
    }
}

impl<K, V> ProviderCodec for crate::AverMap<K, V>
where
    K: ProviderCodec + Eq + std::hash::Hash + Clone,
    V: ProviderCodec + Clone,
{
    fn into_provider_value(
        self,
        registry: &NativeProviderRegistry,
        capability: &str,
    ) -> Result<ProviderValue, String> {
        let mut entries = self
            .iter()
            .map(|(key, value)| {
                let key = key.clone().into_provider_value(registry, capability)?;
                let value = value.clone().into_provider_value(registry, capability)?;
                Ok((provider_value_order_key(&key)?, key, value))
            })
            .collect::<Result<Vec<_>, String>>()?;
        entries.sort_by(|left, right| left.0.cmp(&right.0));
        Ok(ProviderValue::Map(
            entries
                .into_iter()
                .map(|(_, key, value)| (key, value))
                .collect(),
        ))
    }

    fn from_provider_value(
        value: ProviderValue,
        registry: &NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        let ProviderValue::Map(values) = value else {
            return Err(format!("expected Map, got {}", value.shape()));
        };
        let mut out = Self::new();
        for (key, value) in values {
            let key = K::from_provider_value(key, registry, capability, minted_resource)?;
            if out.contains_key(&key) {
                return Err("provider Map contains a duplicate key".to_string());
            }
            let value = V::from_provider_value(value, registry, capability, minted_resource)?;
            out = out.insert_owned(key, value);
        }
        Ok(out)
    }
}

macro_rules! tuple_codec {
    ($($ty:ident:$value:ident),+) => {
        impl<$($ty: ProviderCodec),+> ProviderCodec for ($($ty,)+) {
            fn into_provider_value(
                self,
                registry: &NativeProviderRegistry,
                capability: &str,
            ) -> Result<ProviderValue, String> {
                let ($($value,)+) = self;
                Ok(ProviderValue::Tuple(vec![
                    $($value.into_provider_value(registry, capability)?,)+
                ]))
            }

            fn from_provider_value(
                value: ProviderValue,
                registry: &NativeProviderRegistry,
                capability: &str,
                minted_resource: Option<&str>,
            ) -> Result<Self, String> {
                let ProviderValue::Tuple(values) = value else {
                    return Err(format!("expected Tuple, got {}", value.shape()));
                };
                let mut values = values.into_iter();
                let result = (
                    $($ty::from_provider_value(
                        values.next().ok_or("provider Tuple has too few fields")?,
                        registry,
                        capability,
                        minted_resource,
                    )?,)+
                );
                if values.next().is_some() {
                    return Err("provider Tuple has too many fields".to_string());
                }
                Ok(result)
            }
        }
    };
}

tuple_codec!(A:a, B:b);
tuple_codec!(A:a, B:b, C:c);
tuple_codec!(A:a, B:b, C:c, D:d);
tuple_codec!(A:a, B:b, C:c, D:d, E:e);
tuple_codec!(A:a, B:b, C:c, D:d, E:e, F:f);
tuple_codec!(A:a, B:b, C:c, D:d, E:e, F:f, G:g);
tuple_codec!(A:a, B:b, C:c, D:d, E:e, F:f, G:g, H:h);

/// Canonical structural bytes used only to order provider-bound map entries.
/// This is not a contract hash or serialization format.
pub fn provider_value_order_key(value: &ProviderValue) -> Result<Vec<u8>, String> {
    fn push_len(out: &mut Vec<u8>, len: usize) {
        out.extend_from_slice(&(len as u64).to_be_bytes());
    }
    fn push_bytes(out: &mut Vec<u8>, bytes: &[u8]) {
        push_len(out, bytes.len());
        out.extend_from_slice(bytes);
    }
    fn framed(value: &ProviderValue, out: &mut Vec<u8>) -> Result<(), String> {
        let mut encoded = Vec::new();
        encode(value, &mut encoded)?;
        push_bytes(out, &encoded);
        Ok(())
    }
    fn sequence(tag: u8, values: &[ProviderValue], out: &mut Vec<u8>) -> Result<(), String> {
        out.push(tag);
        push_len(out, values.len());
        for value in values {
            framed(value, out)?;
        }
        Ok(())
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
            ProviderValue::Bytes(value) => {
                out.push(16);
                push_bytes(out, value);
            }
            ProviderValue::Bool(value) => out.extend_from_slice(&[3, u8::from(*value)]),
            ProviderValue::Unit => out.push(4),
            ProviderValue::Tuple(values) => sequence(5, values, out)?,
            ProviderValue::List(values) => sequence(6, values, out)?,
            ProviderValue::Vector(values) => sequence(7, values, out)?,
            ProviderValue::Map(entries) => {
                out.push(8);
                push_len(out, entries.len());
                for (key, value) in entries {
                    framed(key, out)?;
                    framed(value, out)?;
                }
            }
            ProviderValue::ResultOk(value) => {
                out.push(9);
                framed(value, out)?;
            }
            ProviderValue::ResultErr(value) => {
                out.push(10);
                framed(value, out)?;
            }
            ProviderValue::OptionSome(value) => {
                out.push(11);
                framed(value, out)?;
            }
            ProviderValue::OptionNone => out.push(12),
            ProviderValue::Record { type_name, fields } => {
                out.push(13);
                push_bytes(out, type_name.as_bytes());
                push_len(out, fields.len());
                for (name, value) in fields {
                    push_bytes(out, name.as_bytes());
                    framed(value, out)?;
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
                sequence(15, fields, out)?;
            }
            ProviderValue::Resource(_) => {
                return Err(
                    "capability resources cannot be ordered or used as provider Map keys"
                        .to_string(),
                );
            }
        }
        Ok(())
    }

    let mut out = Vec::new();
    encode(value, &mut out)?;
    Ok(out)
}
