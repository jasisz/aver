//! Stable replay JSON bridge for values crossing a custom provider import.

use std::collections::BTreeMap;

use aver::ast::{Type, TypeDef};
use aver::provider::{ProviderRegistry, ProviderResource, ProviderValue};
use aver::replay::JsonValue;

use super::codec::{canonical_named, is_bytes};

/// A resource reconstructed from a trace can correlate later recorded calls,
/// but it must never cross into a live or reissued provider.
#[derive(Clone)]
pub(crate) struct ReplayResource(pub(crate) u64);

pub(super) fn provider_values_to_json_lossy(
    values: &[ProviderValue],
    types: &[Type],
    scope: &str,
    providers: &ProviderRegistry,
) -> Vec<JsonValue> {
    values
        .iter()
        .zip(types)
        .map(|(value, ty)| {
            provider_value_to_json(value, ty, scope, providers)
                .unwrap_or_else(|_| opaque_json(value))
        })
        .collect()
}

fn opaque_json(value: &ProviderValue) -> JsonValue {
    JsonValue::Object(BTreeMap::from([(
        "$opaque".to_string(),
        JsonValue::String(value.shape()),
    )]))
}

fn marker(name: &str, value: JsonValue) -> JsonValue {
    JsonValue::Object(BTreeMap::from([(name.to_string(), value)]))
}

pub(super) fn provider_value_to_json(
    value: &ProviderValue,
    ty: &Type,
    scope: &str,
    providers: &ProviderRegistry,
) -> Result<JsonValue, String> {
    match value {
        ProviderValue::Int(value) => value
            .to_i64()
            .map(JsonValue::Int)
            .ok_or_else(|| "cannot serialize an integer outside the 64-bit JSON range".into()),
        ProviderValue::Float(value) if value.is_finite() => Ok(JsonValue::Float(*value)),
        ProviderValue::Float(_) => Err("cannot serialize non-finite float".into()),
        ProviderValue::String(value) => Ok(JsonValue::String(value.clone())),
        ProviderValue::Bytes(bytes) => Ok(marker(
            "$record",
            JsonValue::Object(BTreeMap::from([
                ("type".into(), JsonValue::String("Bytes".into())),
                (
                    "fields".into(),
                    JsonValue::Object(BTreeMap::from([(
                        "values".into(),
                        JsonValue::Array(
                            bytes
                                .iter()
                                .map(|byte| JsonValue::Int(i64::from(*byte)))
                                .collect(),
                        ),
                    )])),
                ),
            ])),
        )),
        ProviderValue::Bool(value) => Ok(JsonValue::Bool(*value)),
        ProviderValue::Unit => Ok(JsonValue::Null),
        ProviderValue::Tuple(values) => {
            let Type::Tuple(types) = ty else {
                return Err(expected_shape(ty, value));
            };
            if values.len() != types.len() {
                return Err("provider Tuple has wrong arity".into());
            }
            Ok(marker(
                "$tuple",
                JsonValue::Array(
                    values
                        .iter()
                        .zip(types)
                        .map(|(value, ty)| provider_value_to_json(value, ty, scope, providers))
                        .collect::<Result<_, _>>()?,
                ),
            ))
        }
        ProviderValue::List(values) => {
            let Type::List(inner) = ty else {
                return Err(expected_shape(ty, value));
            };
            Ok(JsonValue::Array(
                values
                    .iter()
                    .map(|value| provider_value_to_json(value, inner, scope, providers))
                    .collect::<Result<_, _>>()?,
            ))
        }
        ProviderValue::Vector(values) => {
            let Type::Vector(inner) = ty else {
                return Err(expected_shape(ty, value));
            };
            Ok(marker(
                "$vector",
                JsonValue::Array(
                    values
                        .iter()
                        .map(|value| provider_value_to_json(value, inner, scope, providers))
                        .collect::<Result<_, _>>()?,
                ),
            ))
        }
        ProviderValue::Map(values) => {
            let Type::Map(key_ty, value_ty) = ty else {
                return Err(expected_shape(ty, value));
            };
            Ok(marker(
                "$map",
                JsonValue::Array(
                    values
                        .iter()
                        .map(|(key, value)| {
                            Ok(JsonValue::Array(vec![
                                provider_value_to_json(key, key_ty, scope, providers)?,
                                provider_value_to_json(value, value_ty, scope, providers)?,
                            ]))
                        })
                        .collect::<Result<_, String>>()?,
                ),
            ))
        }
        ProviderValue::ResultOk(value) => {
            let Type::Result(ok, _) = ty else {
                return Err(expected_shape(ty, value));
            };
            Ok(marker(
                "$ok",
                provider_value_to_json(value, ok, scope, providers)?,
            ))
        }
        ProviderValue::ResultErr(value) => {
            let Type::Result(_, err) = ty else {
                return Err(expected_shape(ty, value));
            };
            Ok(marker(
                "$err",
                provider_value_to_json(value, err, scope, providers)?,
            ))
        }
        ProviderValue::OptionSome(value) => {
            let Type::Option(inner) = ty else {
                return Err(expected_shape(ty, value));
            };
            Ok(marker(
                "$some",
                provider_value_to_json(value, inner, scope, providers)?,
            ))
        }
        ProviderValue::OptionNone => Ok(marker("$none", JsonValue::Bool(true))),
        ProviderValue::Record { type_name, fields } => {
            let definition = represented_definition(ty, scope, providers)?;
            let TypeDef::Product {
                fields: field_types,
                ..
            } = definition
            else {
                return Err(format!(
                    "expected record, got represented sum '{type_name}'"
                ));
            };
            let fields = fields
                .iter()
                .map(|(name, value)| {
                    let source_ty = field_types
                        .iter()
                        .find_map(|(field, ty)| (field == name).then_some(ty))
                        .ok_or_else(|| {
                            format!("record '{type_name}' has unknown field '{name}'")
                        })?;
                    let ty = aver::types::parse_type_str_strict(source_ty)
                        .map_err(|bad| format!("invalid field type '{bad}'"))?;
                    Ok((
                        name.clone(),
                        provider_value_to_json(value, &ty, scope, providers)?,
                    ))
                })
                .collect::<Result<_, String>>()?;
            Ok(marker(
                "$record",
                JsonValue::Object(BTreeMap::from([
                    ("type".into(), JsonValue::String(type_name.clone())),
                    ("fields".into(), JsonValue::Object(fields)),
                ])),
            ))
        }
        ProviderValue::Variant {
            type_name,
            variant,
            fields,
        } => {
            let definition = represented_definition(ty, scope, providers)?;
            let TypeDef::Sum { variants, .. } = definition else {
                return Err(format!(
                    "expected sum, got represented record '{type_name}'"
                ));
            };
            let variant_definition = variants
                .iter()
                .find(|candidate| candidate.name == *variant)
                .ok_or_else(|| format!("sum '{type_name}' has unknown variant '{variant}'"))?;
            if fields.len() != variant_definition.fields.len() {
                return Err(format!("variant '{type_name}.{variant}' has wrong arity"));
            }
            let fields = fields
                .iter()
                .zip(&variant_definition.fields)
                .map(|(value, source_ty)| {
                    let ty = aver::types::parse_type_str_strict(source_ty)
                        .map_err(|bad| format!("invalid variant field type '{bad}'"))?;
                    provider_value_to_json(value, &ty, scope, providers)
                })
                .collect::<Result<_, _>>()?;
            Ok(marker(
                "$variant",
                JsonValue::Object(BTreeMap::from([
                    ("type".into(), JsonValue::String(type_name.clone())),
                    ("name".into(), JsonValue::String(variant.clone())),
                    ("fields".into(), JsonValue::Array(fields)),
                ])),
            ))
        }
        ProviderValue::Resource(resource) => {
            let Type::Named { name, .. } = ty else {
                return Err(expected_shape(ty, value));
            };
            let type_name = canonical_named(scope, name, providers);
            if !providers
                .contracts()
                .resource_types()
                .any(|known| known == &type_name)
            {
                return Err(format!("type '{type_name}' is not a capability resource"));
            }
            let trace = resource
                .downcast_ref::<ReplayResource>()
                .map_or(resource.id(), |token| token.0);
            Ok(marker(
                "$capabilityResource",
                JsonValue::Object(BTreeMap::from([
                    ("type".into(), JsonValue::String(type_name)),
                    ("trace".into(), JsonValue::String(trace.to_string())),
                ])),
            ))
        }
    }
}

fn represented_definition<'a>(
    ty: &Type,
    scope: &str,
    providers: &'a ProviderRegistry,
) -> Result<&'a TypeDef, String> {
    let Type::Named { name, .. } = ty else {
        return Err(format!("expected represented type, got {}", ty.display()));
    };
    let canonical = canonical_named(scope, name, providers);
    providers
        .contracts()
        .boundary_type(&canonical)
        .ok_or_else(|| format!("unknown represented boundary type '{canonical}'"))
}

fn expected_shape(ty: &Type, value: &ProviderValue) -> String {
    format!("expected {}, got {}", ty.display(), value.shape())
}

pub(super) fn provider_value_from_json(
    json: &JsonValue,
    ty: &Type,
    scope: &str,
    providers: &ProviderRegistry,
) -> Result<ProviderValue, String> {
    match (ty, json) {
        (Type::Int, JsonValue::Int(value)) => Ok(ProviderValue::Int((*value).into())),
        (Type::Float, JsonValue::Float(value)) => Ok(ProviderValue::Float(*value)),
        (Type::Float, JsonValue::Int(value)) => Ok(ProviderValue::Float(*value as f64)),
        (Type::Str, JsonValue::String(value)) => Ok(ProviderValue::String(value.clone())),
        (Type::Bool, JsonValue::Bool(value)) => Ok(ProviderValue::Bool(*value)),
        (Type::Unit, JsonValue::Null) => Ok(ProviderValue::Unit),
        (Type::List(inner), JsonValue::Array(values)) => Ok(ProviderValue::List(
            values
                .iter()
                .map(|value| provider_value_from_json(value, inner, scope, providers))
                .collect::<Result<_, _>>()?,
        )),
        (Type::Result(ok, _), _) if marker_payload(json, "$ok").is_some() => {
            Ok(ProviderValue::ResultOk(Box::new(provider_value_from_json(
                marker_payload(json, "$ok").expect("guarded"),
                ok,
                scope,
                providers,
            )?)))
        }
        (Type::Result(_, err), _) if marker_payload(json, "$err").is_some() => Ok(
            ProviderValue::ResultErr(Box::new(provider_value_from_json(
                marker_payload(json, "$err").expect("guarded"),
                err,
                scope,
                providers,
            )?)),
        ),
        (Type::Option(inner), _) if marker_payload(json, "$some").is_some() => Ok(
            ProviderValue::OptionSome(Box::new(provider_value_from_json(
                marker_payload(json, "$some").expect("guarded"),
                inner,
                scope,
                providers,
            )?)),
        ),
        (Type::Option(_), _) if marker_payload(json, "$none") == Some(&JsonValue::Bool(true)) => {
            Ok(ProviderValue::OptionNone)
        }
        (Type::Tuple(types), _) => {
            let values = json_array(marker_payload(json, "$tuple"), "$tuple")?;
            if values.len() != types.len() {
                return Err("recorded tuple has wrong arity".into());
            }
            Ok(ProviderValue::Tuple(
                types
                    .iter()
                    .zip(values)
                    .map(|(ty, value)| provider_value_from_json(value, ty, scope, providers))
                    .collect::<Result<_, _>>()?,
            ))
        }
        (Type::Vector(inner), _) => Ok(ProviderValue::Vector(
            json_array(marker_payload(json, "$vector"), "$vector")?
                .iter()
                .map(|value| provider_value_from_json(value, inner, scope, providers))
                .collect::<Result<_, _>>()?,
        )),
        (Type::Map(key, value), _) => {
            let pairs = json_array(marker_payload(json, "$map"), "$map")?;
            let mut values = Vec::with_capacity(pairs.len());
            for pair in pairs {
                let pair = json_array(Some(pair), "$map pair")?;
                if pair.len() != 2 {
                    return Err("recorded Map pair has wrong arity".into());
                }
                values.push((
                    provider_value_from_json(&pair[0], key, scope, providers)?,
                    provider_value_from_json(&pair[1], value, scope, providers)?,
                ));
            }
            Ok(ProviderValue::Map(values))
        }
        (Type::Named { name, .. }, _) if is_bytes(name) => decode_json_bytes(json),
        (Type::Named { name, .. }, _) => {
            let canonical = canonical_named(scope, name, providers);
            if providers
                .contracts()
                .resource_types()
                .any(|known| known == &canonical)
            {
                let payload = json_object(
                    marker_payload(json, "$capabilityResource"),
                    "$capabilityResource",
                )?;
                let JsonValue::String(type_name) = payload
                    .get("type")
                    .ok_or_else(|| "recorded resource is missing type".to_string())?
                else {
                    return Err("recorded resource type is not a string".into());
                };
                if type_name != &canonical {
                    return Err(format!(
                        "recorded resource type '{type_name}' is not '{canonical}'"
                    ));
                }
                let JsonValue::String(trace) = payload
                    .get("trace")
                    .ok_or_else(|| "recorded resource is missing trace".to_string())?
                else {
                    return Err("recorded resource trace is not a string".into());
                };
                let trace = trace
                    .parse::<u64>()
                    .map_err(|_| "recorded resource trace is not a u64".to_string())?;
                if trace == 0 {
                    return Err("recorded resource trace must be non-zero".into());
                }
                return Ok(ProviderValue::Resource(ProviderResource::new(
                    ReplayResource(trace),
                )));
            }
            let definition = providers
                .contracts()
                .boundary_type(&canonical)
                .ok_or_else(|| format!("unknown represented boundary type '{canonical}'"))?;
            provider_represented_from_json(json, definition, &canonical, scope, providers)
        }
        _ => Err(format!(
            "expected {}, received recorded JSON {json:?}",
            ty.display()
        )),
    }
}

fn provider_represented_from_json(
    json: &JsonValue,
    definition: &TypeDef,
    canonical: &str,
    scope: &str,
    providers: &ProviderRegistry,
) -> Result<ProviderValue, String> {
    match definition {
        TypeDef::Product { fields, .. } => {
            let payload = json_object(marker_payload(json, "$record"), "$record")?;
            let JsonValue::String(type_name) = payload
                .get("type")
                .ok_or_else(|| "recorded record is missing type".to_string())?
            else {
                return Err("recorded record type is not a string".into());
            };
            if type_name != canonical
                && type_name.rsplit('.').next() != canonical.rsplit('.').next()
            {
                return Err(format!(
                    "recorded record type '{type_name}' is not '{canonical}'"
                ));
            }
            let values = json_object(payload.get("fields"), "$record.fields")?;
            if values.len() != fields.len() {
                return Err(format!(
                    "recorded record '{canonical}' has the wrong field set"
                ));
            }
            let mut out = Vec::with_capacity(fields.len());
            for (name, source_ty) in fields {
                let ty = aver::types::parse_type_str_strict(source_ty)
                    .map_err(|bad| format!("invalid field type '{bad}'"))?;
                out.push((
                    name.clone(),
                    provider_value_from_json(
                        values
                            .get(name)
                            .ok_or_else(|| format!("recorded record is missing field '{name}'"))?,
                        &ty,
                        scope,
                        providers,
                    )?,
                ));
            }
            Ok(ProviderValue::Record {
                type_name: canonical.to_string(),
                fields: out,
            })
        }
        TypeDef::Sum { variants, .. } => {
            let payload = json_object(marker_payload(json, "$variant"), "$variant")?;
            let JsonValue::String(type_name) = payload
                .get("type")
                .ok_or_else(|| "recorded variant is missing type".to_string())?
            else {
                return Err("recorded variant type is not a string".into());
            };
            if type_name != canonical
                && type_name.rsplit('.').next() != canonical.rsplit('.').next()
            {
                return Err(format!(
                    "recorded variant type '{type_name}' is not '{canonical}'"
                ));
            }
            let JsonValue::String(variant) = payload
                .get("name")
                .ok_or_else(|| "recorded variant is missing name".to_string())?
            else {
                return Err("recorded variant name is not a string".into());
            };
            let definition = variants
                .iter()
                .find(|candidate| candidate.name == *variant)
                .ok_or_else(|| format!("unknown recorded variant '{canonical}.{variant}'"))?;
            let values = json_array(payload.get("fields"), "$variant.fields")?;
            if values.len() != definition.fields.len() {
                return Err("recorded variant has wrong arity".into());
            }
            let fields = definition
                .fields
                .iter()
                .zip(values)
                .map(|(source_ty, value)| {
                    let ty = aver::types::parse_type_str_strict(source_ty)
                        .map_err(|bad| format!("invalid variant field type '{bad}'"))?;
                    provider_value_from_json(value, &ty, scope, providers)
                })
                .collect::<Result<_, _>>()?;
            Ok(ProviderValue::Variant {
                type_name: canonical.to_string(),
                variant: variant.clone(),
                fields,
            })
        }
    }
}

fn decode_json_bytes(json: &JsonValue) -> Result<ProviderValue, String> {
    let payload = json_object(marker_payload(json, "$record"), "$record")?;
    let JsonValue::String(type_name) = payload
        .get("type")
        .ok_or_else(|| "recorded Bytes is missing type".to_string())?
    else {
        return Err("recorded Bytes type is not a string".into());
    };
    if !is_bytes(type_name) {
        return Err(format!("recorded Bytes has type '{type_name}'"));
    }
    let fields = json_object(payload.get("fields"), "$record.fields")?;
    let values = json_array(fields.get("values"), "Bytes.values")?;
    let mut bytes = Vec::with_capacity(values.len());
    for value in values {
        let JsonValue::Int(value) = value else {
            return Err("recorded Bytes contains a non-integer octet".into());
        };
        bytes.push(u8::try_from(*value).map_err(|_| "recorded Bytes octet is out of range")?);
    }
    Ok(ProviderValue::Bytes(bytes))
}

fn marker_payload<'a>(json: &'a JsonValue, name: &str) -> Option<&'a JsonValue> {
    let JsonValue::Object(values) = json else {
        return None;
    };
    (values.len() == 1).then(|| values.get(name)).flatten()
}

fn json_array<'a>(value: Option<&'a JsonValue>, path: &str) -> Result<&'a [JsonValue], String> {
    match value {
        Some(JsonValue::Array(values)) => Ok(values),
        _ => Err(format!("{path} is not an array")),
    }
}

fn json_object<'a>(
    value: Option<&'a JsonValue>,
    path: &str,
) -> Result<&'a BTreeMap<String, JsonValue>, String> {
    match value {
        Some(JsonValue::Object(values)) => Ok(values),
        _ => Err(format!("{path} is not an object")),
    }
}
