use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};

use aver_rt::provider::ProviderValue;

use crate::ast::{Type, TypeDef};
use crate::capability::CapabilityRegistry;
use crate::value::Value;

use super::ordering::provider_value_order_key;
use super::{CapabilityResourceHandle, ResourceStore};

pub(super) fn to_provider_value(
    value: &Value,
    ty: &Type,
    scope: &str,
    contracts: &CapabilityRegistry,
    binding_id: u64,
    resources: &Arc<Mutex<ResourceStore>>,
) -> Result<ProviderValue, String> {
    match (ty, value) {
        (Type::Int, Value::Int(value)) => Ok(ProviderValue::Int(value.clone())),
        (Type::Float, Value::Float(value)) => Ok(ProviderValue::Float(*value)),
        (Type::Str, Value::Str(value)) => Ok(ProviderValue::String(value.clone())),
        (Type::Bool, Value::Bool(value)) => Ok(ProviderValue::Bool(*value)),
        (Type::Unit, Value::Unit) => Ok(ProviderValue::Unit),
        (Type::Tuple(types), Value::Tuple(values)) if types.len() == values.len() => {
            Ok(ProviderValue::Tuple(
                types
                    .iter()
                    .zip(values)
                    .map(|(ty, value)| {
                        to_provider_value(value, ty, scope, contracts, binding_id, resources)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
        (Type::List(inner), Value::List(values)) => Ok(ProviderValue::List(
            values
                .iter()
                .map(|value| {
                    to_provider_value(value, inner, scope, contracts, binding_id, resources)
                })
                .collect::<Result<Vec<_>, _>>()?,
        )),
        (Type::Vector(inner), Value::Vector(values)) => Ok(ProviderValue::Vector(
            values
                .iter()
                .map(|value| {
                    to_provider_value(value, inner, scope, contracts, binding_id, resources)
                })
                .collect::<Result<Vec<_>, _>>()?,
        )),
        (Type::Map(key_ty, value_ty), Value::Map(values)) => {
            let mut ordered = values
                .iter()
                .map(|(key, value)| {
                    let key =
                        to_provider_value(key, key_ty, scope, contracts, binding_id, resources)?;
                    let value = to_provider_value(
                        value, value_ty, scope, contracts, binding_id, resources,
                    )?;
                    Ok((provider_value_order_key(&key)?, key, value))
                })
                .collect::<Result<Vec<_>, String>>()?;
            ordered.sort_by(|left, right| left.0.cmp(&right.0));
            Ok(ProviderValue::Map(
                ordered
                    .into_iter()
                    .map(|(_, key, value)| (key, value))
                    .collect(),
            ))
        }
        (Type::Result(ok, _), Value::Ok(value)) => Ok(ProviderValue::ResultOk(Box::new(
            to_provider_value(value, ok, scope, contracts, binding_id, resources)?,
        ))),
        (Type::Result(_, err), Value::Err(value)) => Ok(ProviderValue::ResultErr(Box::new(
            to_provider_value(value, err, scope, contracts, binding_id, resources)?,
        ))),
        (Type::Option(inner), Value::Some(value)) => Ok(ProviderValue::OptionSome(Box::new(
            to_provider_value(value, inner, scope, contracts, binding_id, resources)?,
        ))),
        (Type::Option(_), Value::None) => Ok(ProviderValue::OptionNone),
        (Type::Named { name, .. }, Value::CapabilityResource(handle)) => {
            let canonical = canonical_type(scope, name);
            if !contracts.opaque_types().any(|known| known == &canonical) {
                return Err(format!("type '{}' is not a capability resource", canonical));
            }
            if handle.binding_id() != binding_id {
                return Err(format!(
                    "resource '{}' belongs to a different provider binding",
                    canonical
                ));
            }
            if handle.type_name() != canonical {
                return Err(format!(
                    "resource has type '{}', expected resource type '{}'",
                    handle.type_name(),
                    canonical
                ));
            }
            let store = resources.lock().map_err(|_| "resource store poisoned")?;
            let resource = store
                .resources
                .get(&(handle.binding_id(), handle.slot(), handle.generation()))
                .cloned()
                .ok_or_else(|| format!("resource '{}' is stale", canonical))?;
            Ok(ProviderValue::Resource(resource))
        }
        (Type::Named { name, .. }, value) => {
            let canonical = canonical_type(scope, name);
            let type_def = contracts
                .boundary_type(&canonical)
                .ok_or_else(|| format!("unknown boundary type '{}'", canonical))?;
            represented_to_provider(
                value, type_def, &canonical, scope, contracts, binding_id, resources,
            )
        }
        _ => Err(format!(
            "expected {}, got {}",
            ty.display(),
            crate::value::aver_repr(value)
        )),
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn from_provider_value(
    value: ProviderValue,
    ty: &Type,
    scope: &str,
    contracts: &CapabilityRegistry,
    binding_id: u64,
    minted_resource: Option<&str>,
    resources: &Arc<Mutex<ResourceStore>>,
) -> Result<Value, String> {
    match (ty, value) {
        (Type::Int, ProviderValue::Int(value)) => Ok(Value::Int(value)),
        (Type::Float, ProviderValue::Float(value)) => Ok(Value::Float(value)),
        (Type::Str, ProviderValue::String(value)) => Ok(Value::Str(value)),
        (Type::Bool, ProviderValue::Bool(value)) => Ok(Value::Bool(value)),
        (Type::Unit, ProviderValue::Unit) => Ok(Value::Unit),
        (Type::Tuple(types), ProviderValue::Tuple(values)) if types.len() == values.len() => {
            Ok(Value::Tuple(
                types
                    .iter()
                    .zip(values)
                    .map(|(ty, value)| {
                        from_provider_value(
                            value,
                            ty,
                            scope,
                            contracts,
                            binding_id,
                            minted_resource,
                            resources,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
        (Type::List(inner), ProviderValue::List(values)) => Ok(crate::value::list_from_vec(
            values
                .into_iter()
                .map(|value| {
                    from_provider_value(
                        value,
                        inner,
                        scope,
                        contracts,
                        binding_id,
                        minted_resource,
                        resources,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?,
        )),
        (Type::Vector(inner), ProviderValue::Vector(values)) => {
            Ok(Value::Vector(aver_rt::AverVector::from_vec(
                values
                    .into_iter()
                    .map(|value| {
                        from_provider_value(
                            value,
                            inner,
                            scope,
                            contracts,
                            binding_id,
                            minted_resource,
                            resources,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )))
        }
        (Type::Map(key_ty, value_ty), ProviderValue::Map(values)) => {
            let mut map = HashMap::new();
            for (key, value) in values {
                let key = from_provider_value(
                    key,
                    key_ty,
                    scope,
                    contracts,
                    binding_id,
                    minted_resource,
                    resources,
                )?;
                let value = from_provider_value(
                    value,
                    value_ty,
                    scope,
                    contracts,
                    binding_id,
                    minted_resource,
                    resources,
                )?;
                if map.insert(key, value).is_some() {
                    return Err("provider Map contains a duplicate key".to_string());
                }
            }
            Ok(Value::Map(map))
        }
        (Type::Result(ok, _), ProviderValue::ResultOk(value)) => {
            Ok(Value::Ok(Box::new(from_provider_value(
                *value,
                ok,
                scope,
                contracts,
                binding_id,
                minted_resource,
                resources,
            )?)))
        }
        (Type::Result(_, err), ProviderValue::ResultErr(value)) => {
            Ok(Value::Err(Box::new(from_provider_value(
                *value,
                err,
                scope,
                contracts,
                binding_id,
                minted_resource,
                resources,
            )?)))
        }
        (Type::Option(inner), ProviderValue::OptionSome(value)) => {
            Ok(Value::Some(Box::new(from_provider_value(
                *value,
                inner,
                scope,
                contracts,
                binding_id,
                minted_resource,
                resources,
            )?)))
        }
        (Type::Option(_), ProviderValue::OptionNone) => Ok(Value::None),
        (Type::Named { name, .. }, ProviderValue::Resource(resource)) => {
            let canonical = canonical_type(scope, name);
            if minted_resource != Some(canonical.as_str()) {
                return Err(format!(
                    "resource '{}' may only be returned by its minting operation",
                    canonical
                ));
            }
            let mut store = resources.lock().map_err(|_| "resource store poisoned")?;
            let slot = store.next_slot;
            store.next_slot = store
                .next_slot
                .checked_add(1)
                .ok_or("capability resource store exhausted")?;
            let generation = resource.id();
            store
                .resources
                .insert((binding_id, slot, generation), resource);
            Ok(Value::CapabilityResource(
                CapabilityResourceHandle::from_runtime_parts(
                    binding_id, canonical, slot, generation,
                ),
            ))
        }
        (Type::Named { name, .. }, value) => {
            let canonical = canonical_type(scope, name);
            let type_def = contracts
                .boundary_type(&canonical)
                .ok_or_else(|| format!("unknown boundary type '{}'", canonical))?;
            represented_from_provider(
                value,
                type_def,
                &canonical,
                scope,
                contracts,
                binding_id,
                minted_resource,
                resources,
            )
        }
        (expected, actual) => Err(format!(
            "expected {}, got {}",
            expected.display(),
            actual.shape()
        )),
    }
}

#[allow(clippy::too_many_arguments)]
fn represented_to_provider(
    value: &Value,
    type_def: &TypeDef,
    canonical: &str,
    scope: &str,
    contracts: &CapabilityRegistry,
    binding_id: u64,
    resources: &Arc<Mutex<ResourceStore>>,
) -> Result<ProviderValue, String> {
    match (type_def, value) {
        (
            TypeDef::Product { fields, .. },
            Value::Record {
                type_name,
                fields: record_fields,
            },
        ) if same_type(canonical, type_name) => {
            let mut values = BTreeMap::new();
            for (name, value) in record_fields.iter() {
                if values.insert(name.as_str(), value).is_some() {
                    return Err(format!(
                        "record '{}' contains duplicate field '{}'",
                        canonical, name
                    ));
                }
            }
            let mut out = Vec::new();
            for (name, source_ty) in fields {
                let ty = crate::types::parse_type_str_strict(source_ty)
                    .map_err(|_| format!("invalid field type '{}'", source_ty))?;
                let value = values
                    .remove(name.as_str())
                    .ok_or_else(|| format!("record '{}' is missing field '{}'", canonical, name))?;
                out.push((
                    name.clone(),
                    to_provider_value(value, &ty, scope, contracts, binding_id, resources)?,
                ));
            }
            if !values.is_empty() {
                return Err(format!("record '{}' has unknown fields", canonical));
            }
            Ok(ProviderValue::Record {
                type_name: canonical.to_string(),
                fields: out,
            })
        }
        (
            TypeDef::Sum { variants, .. },
            Value::Variant {
                type_name,
                variant,
                fields,
            },
        ) if same_type(canonical, type_name) => {
            let definition = variants
                .iter()
                .find(|candidate| candidate.name == *variant)
                .ok_or_else(|| format!("unknown variant '{}.{}'", canonical, variant))?;
            if definition.fields.len() != fields.len() {
                return Err(format!(
                    "variant '{}.{}' has wrong arity",
                    canonical, variant
                ));
            }
            let fields = definition
                .fields
                .iter()
                .zip(fields.iter())
                .map(|(source_ty, value)| {
                    let ty = crate::types::parse_type_str_strict(source_ty)
                        .map_err(|_| format!("invalid variant field type '{}'", source_ty))?;
                    to_provider_value(value, &ty, scope, contracts, binding_id, resources)
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ProviderValue::Variant {
                type_name: canonical.to_string(),
                variant: variant.clone(),
                fields,
            })
        }
        _ => Err(format!(
            "expected represented boundary type '{}'",
            canonical
        )),
    }
}

#[allow(clippy::too_many_arguments)]
fn represented_from_provider(
    value: ProviderValue,
    type_def: &TypeDef,
    canonical: &str,
    scope: &str,
    contracts: &CapabilityRegistry,
    binding_id: u64,
    minted_resource: Option<&str>,
    resources: &Arc<Mutex<ResourceStore>>,
) -> Result<Value, String> {
    match (type_def, value) {
        (
            TypeDef::Product { fields, .. },
            ProviderValue::Record {
                type_name,
                fields: values,
            },
        ) if canonical == type_name => {
            let mut by_name = BTreeMap::new();
            for (name, value) in values {
                if by_name.insert(name.clone(), value).is_some() {
                    return Err(format!(
                        "record '{}' contains duplicate field '{}'",
                        canonical, name
                    ));
                }
            }
            let mut out = Vec::new();
            for (name, source_ty) in fields {
                let ty = crate::types::parse_type_str_strict(source_ty)
                    .map_err(|_| format!("invalid field type '{}'", source_ty))?;
                let value = by_name
                    .remove(name)
                    .ok_or_else(|| format!("record '{}' is missing field '{}'", canonical, name))?;
                out.push((
                    name.clone(),
                    from_provider_value(
                        value,
                        &ty,
                        scope,
                        contracts,
                        binding_id,
                        minted_resource,
                        resources,
                    )?,
                ));
            }
            if !by_name.is_empty() {
                return Err(format!("record '{}' has unknown fields", canonical));
            }
            Ok(Value::Record {
                type_name: canonical.to_string(),
                fields: out.into(),
            })
        }
        (
            TypeDef::Sum { variants, .. },
            ProviderValue::Variant {
                type_name,
                variant,
                fields,
            },
        ) if canonical == type_name => {
            let definition = variants
                .iter()
                .find(|candidate| candidate.name == variant)
                .ok_or_else(|| format!("unknown variant '{}.{}'", canonical, variant))?;
            if definition.fields.len() != fields.len() {
                return Err(format!(
                    "variant '{}.{}' has wrong arity",
                    canonical, variant
                ));
            }
            let fields = definition
                .fields
                .iter()
                .zip(fields)
                .map(|(source_ty, value)| {
                    let ty = crate::types::parse_type_str_strict(source_ty)
                        .map_err(|_| format!("invalid variant field type '{}'", source_ty))?;
                    from_provider_value(
                        value,
                        &ty,
                        scope,
                        contracts,
                        binding_id,
                        minted_resource,
                        resources,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::Variant {
                type_name: canonical.to_string(),
                variant,
                fields: fields.into(),
            })
        }
        (_, actual) => Err(format!(
            "expected represented boundary type '{}', got {}",
            canonical,
            actual.shape()
        )),
    }
}

fn canonical_type(scope: &str, name: &str) -> String {
    if name.contains('.') {
        name.to_string()
    } else {
        format!("{scope}.{name}")
    }
}

fn same_type(canonical: &str, actual: &str) -> bool {
    canonical == actual
        || canonical
            .rsplit_once('.')
            .is_some_and(|(_, bare)| bare == actual)
}
