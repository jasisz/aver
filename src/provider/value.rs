use std::collections::{BTreeMap, HashMap};

use aver_rt::provider::{NativeProviderRegistry, ProviderValue};

use crate::ast::{Type, TypeDef};
use crate::capability::CapabilityRegistry;
use crate::value::Value;

use super::ordering::provider_value_order_key;
pub(super) fn to_provider_value(
    value: &Value,
    ty: &Type,
    scope: &str,
    contracts: &CapabilityRegistry,
    native: &NativeProviderRegistry,
) -> Result<ProviderValue, String> {
    match (ty, value) {
        // A type variable in an operation's signature says the provider
        // carries this value and never reads it. `Wait.poll`'s key is the one
        // such position: the caller owns its keys, the wait correlates and
        // orders them, and nothing at the boundary interprets them. So the
        // value crosses by its own shape rather than against a declared
        // boundary type, and the capability contract binds no layout for it.
        (Type::Var(_), value) => opaque_to_provider(value),
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
                    .map(|(ty, value)| to_provider_value(value, ty, scope, contracts, native))
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
        (Type::List(inner), Value::List(values)) => Ok(ProviderValue::List(
            values
                .iter()
                .map(|value| to_provider_value(value, inner, scope, contracts, native))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        (Type::Vector(inner), Value::Vector(values)) => Ok(ProviderValue::Vector(
            values
                .iter()
                .map(|value| to_provider_value(value, inner, scope, contracts, native))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        (Type::Map(key_ty, value_ty), Value::Map(values)) => {
            let mut ordered = values
                .iter()
                .map(|(key, value)| {
                    let key = to_provider_value(key, key_ty, scope, contracts, native)?;
                    let value = to_provider_value(value, value_ty, scope, contracts, native)?;
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
            to_provider_value(value, ok, scope, contracts, native)?,
        ))),
        (Type::Result(_, err), Value::Err(value)) => Ok(ProviderValue::ResultErr(Box::new(
            to_provider_value(value, err, scope, contracts, native)?,
        ))),
        (Type::Option(inner), Value::Some(value)) => Ok(ProviderValue::OptionSome(Box::new(
            to_provider_value(value, inner, scope, contracts, native)?,
        ))),
        (Type::Option(_), Value::None) => Ok(ProviderValue::OptionNone),
        (Type::Named { name, .. }, value) if is_standard_bytes(name) => {
            crate::types::bytes::project(value, "capability provider boundary")
                .map(ProviderValue::Bytes)
                .map_err(|error| error.to_string())
        }
        (Type::Named { name, .. }, Value::CapabilityResource(handle)) => {
            let canonical = canonical_type(scope, name);
            if !contracts.resource_types().any(|known| known == &canonical) {
                return Err(format!("type '{}' is not a capability resource", canonical));
            }
            // A resource is minted by whichever capability returns it, which
            // need not be the capability that declares the type: `Work.Job` is
            // declared by `Work`, minted by every job kind, and read by `Work`
            // and `Wait`. Binding identity therefore cannot decide on its own.
            // Prefer it where it holds, and otherwise accept a handle whose
            // type matches and whose minting binding is still installed.
            native
                .resolve_resource(scope, &canonical, handle)
                .or_else(|_| native.resolve_foreign_resource(&canonical, handle))
                .map(ProviderValue::Resource)
        }
        (Type::Named { name, .. }, value) => {
            let canonical = canonical_type(scope, name);
            let type_def = contracts
                .boundary_type(&canonical)
                .ok_or_else(|| format!("unknown boundary type '{}'", canonical))?;
            represented_to_provider(value, type_def, &canonical, scope, contracts, native)
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
    minted_resource: Option<&str>,
    native: &NativeProviderRegistry,
) -> Result<Value, String> {
    match (ty, value) {
        // The other half of the carried-not-read position: what the provider
        // hands back under a type variable is one of the values it was given,
        // so it comes back by its own shape.
        (Type::Var(_), value) => opaque_from_provider(value),
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
                        from_provider_value(value, ty, scope, contracts, minted_resource, native)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
        (Type::List(inner), ProviderValue::List(values)) => Ok(crate::value::list_from_vec(
            values
                .into_iter()
                .map(|value| {
                    from_provider_value(value, inner, scope, contracts, minted_resource, native)
                })
                .collect::<Result<Vec<_>, _>>()?,
        )),
        (Type::Vector(inner), ProviderValue::Vector(values)) => {
            Ok(Value::Vector(aver_rt::AverVector::from_vec(
                values
                    .into_iter()
                    .map(|value| {
                        from_provider_value(value, inner, scope, contracts, minted_resource, native)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )))
        }
        (Type::Map(key_ty, value_ty), ProviderValue::Map(values)) => {
            let mut map = HashMap::new();
            for (key, value) in values {
                let key =
                    from_provider_value(key, key_ty, scope, contracts, minted_resource, native)?;
                let value = from_provider_value(
                    value,
                    value_ty,
                    scope,
                    contracts,
                    minted_resource,
                    native,
                )?;
                if map.insert(key, value).is_some() {
                    return Err("provider Map contains a duplicate key".to_string());
                }
            }
            Ok(Value::Map(map))
        }
        (Type::Result(ok, _), ProviderValue::ResultOk(value)) => Ok(Value::Ok(Box::new(
            from_provider_value(*value, ok, scope, contracts, minted_resource, native)?,
        ))),
        (Type::Result(_, err), ProviderValue::ResultErr(value)) => Ok(Value::Err(Box::new(
            from_provider_value(*value, err, scope, contracts, minted_resource, native)?,
        ))),
        (Type::Option(inner), ProviderValue::OptionSome(value)) => Ok(Value::Some(Box::new(
            from_provider_value(*value, inner, scope, contracts, minted_resource, native)?,
        ))),
        (Type::Option(_), ProviderValue::OptionNone) => Ok(Value::None),
        (Type::Named { name, .. }, value) if is_standard_bytes(name) => match value {
            ProviderValue::Bytes(bytes) => Ok(crate::types::bytes::from_host(&bytes)),
            other => Err(format!("expected Bytes, got {}", other.shape())),
        },
        (Type::Named { name, .. }, ProviderValue::Resource(resource)) => {
            let canonical = canonical_type(scope, name);
            if minted_resource != Some(canonical.as_str()) {
                return Err(format!(
                    "resource '{}' may only be returned by its minting operation",
                    canonical
                ));
            }
            Ok(Value::CapabilityResource(
                native.store_resource(scope, canonical, resource)?,
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
                minted_resource,
                native,
            )
        }
        (expected, actual) => Err(format!(
            "expected {}, got {}",
            expected.display(),
            actual.shape()
        )),
    }
}

fn is_standard_bytes(name: &str) -> bool {
    matches!(name, "Bytes" | "Bytes.Bytes")
}

/// Carry one value across a boundary position the operation declared as a
/// type variable, by its own shape.
///
/// No contract is consulted, because none binds this position: the operation
/// said the provider does not read it. A capability resource is refused all
/// the same. Resource identity is deliberately unobservable, so a resource
/// here would be a key the provider cannot tell apart from another, and the
/// caller would get readiness for the wrong one.
fn opaque_to_provider(value: &Value) -> Result<ProviderValue, String> {
    let each = |values: &[Value]| -> Result<Vec<ProviderValue>, String> {
        values.iter().map(opaque_to_provider).collect()
    };
    match value {
        Value::Int(value) => Ok(ProviderValue::Int(value.clone())),
        Value::Float(value) => Ok(ProviderValue::Float(*value)),
        Value::Str(value) => Ok(ProviderValue::String(value.clone())),
        Value::Bool(value) => Ok(ProviderValue::Bool(*value)),
        Value::Unit => Ok(ProviderValue::Unit),
        Value::Ok(value) => Ok(ProviderValue::ResultOk(Box::new(opaque_to_provider(value)?))),
        Value::Err(value) => Ok(ProviderValue::ResultErr(Box::new(opaque_to_provider(
            value,
        )?))),
        Value::Some(value) => Ok(ProviderValue::OptionSome(Box::new(opaque_to_provider(
            value,
        )?))),
        Value::None => Ok(ProviderValue::OptionNone),
        Value::Tuple(values) => Ok(ProviderValue::Tuple(each(values)?)),
        Value::List(values) => Ok(ProviderValue::List(
            values.iter().map(opaque_to_provider).collect::<Result<_, _>>()?,
        )),
        Value::Vector(values) => Ok(ProviderValue::Vector(
            values.iter().map(opaque_to_provider).collect::<Result<_, _>>()?,
        )),
        Value::Variant {
            type_name,
            variant,
            fields,
        } => Ok(ProviderValue::Variant {
            type_name: type_name.clone(),
            variant: variant.clone(),
            fields: each(fields)?,
        }),
        Value::Record { type_name, fields } => Ok(ProviderValue::Record {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| Ok((name.clone(), opaque_to_provider(value)?)))
                .collect::<Result<Vec<_>, String>>()?,
        }),
        Value::CapabilityResource(_) => Err(
            "a capability resource cannot cross a boundary position the provider does not read; resource identity is not observable".to_string(),
        ),
        other => Err(format!(
            "value of shape {} cannot cross a boundary position the provider does not read",
            crate::value::aver_repr(other)
        )),
    }
}

/// The inverse of [`opaque_to_provider`]: one of the values the provider was
/// handed, coming back.
fn opaque_from_provider(value: ProviderValue) -> Result<Value, String> {
    let each = |values: Vec<ProviderValue>| {
        values
            .into_iter()
            .map(opaque_from_provider)
            .collect::<Result<Vec<_>, String>>()
    };
    match value {
        ProviderValue::Int(value) => Ok(Value::Int(value)),
        ProviderValue::Float(value) => Ok(Value::Float(value)),
        ProviderValue::String(value) => Ok(Value::Str(value)),
        ProviderValue::Bool(value) => Ok(Value::Bool(value)),
        ProviderValue::Unit => Ok(Value::Unit),
        ProviderValue::ResultOk(value) => Ok(Value::Ok(Box::new(opaque_from_provider(*value)?))),
        ProviderValue::ResultErr(value) => Ok(Value::Err(Box::new(opaque_from_provider(*value)?))),
        ProviderValue::OptionSome(value) => {
            Ok(Value::Some(Box::new(opaque_from_provider(*value)?)))
        }
        ProviderValue::OptionNone => Ok(Value::None),
        ProviderValue::Tuple(values) => Ok(Value::Tuple(each(values)?)),
        ProviderValue::List(values) => Ok(crate::value::list_from_vec(each(values)?)),
        ProviderValue::Vector(values) => {
            Ok(Value::Vector(aver_rt::AverVector::from_vec(each(values)?)))
        }
        ProviderValue::Variant {
            type_name,
            variant,
            fields,
        } => Ok(Value::Variant {
            type_name,
            variant,
            fields: each(fields)?.into(),
        }),
        ProviderValue::Record { type_name, fields } => Ok(Value::Record {
            type_name,
            fields: fields
                .into_iter()
                .map(|(name, value)| Ok((name, opaque_from_provider(value)?)))
                .collect::<Result<Vec<_>, String>>()?
                .into(),
        }),
        other => Err(format!(
            "provider returned {} in a position it does not read",
            other.shape()
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
    native: &NativeProviderRegistry,
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
                    to_provider_value(value, &ty, scope, contracts, native)?,
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
                    to_provider_value(value, &ty, scope, contracts, native)
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
    minted_resource: Option<&str>,
    native: &NativeProviderRegistry,
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
                    from_provider_value(value, &ty, scope, contracts, minted_resource, native)?,
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
                    from_provider_value(value, &ty, scope, contracts, minted_resource, native)
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

/// A recorded or replayed value carried a record tag that is not the
/// capability-owned type the operation declares: not the canonical
/// `Module.Type` spelling, and not the type's own short name either.
/// `expected`/`received` keep both spellings for the diagnostic.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundaryTypeMismatch {
    pub expected: String,
    pub received: String,
}

impl std::fmt::Display for BoundaryTypeMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected '{}', got '{}'", self.expected, self.received)
    }
}

/// Rewrite the type tag of every capability-owned record or variant inside
/// `value` to the canonical `Module.Type` spelling the provider boundary
/// uses: `represented_from_provider` emits it, `to_provider_value` accepts
/// it, and a recording is a ledger of boundary crossings, so both sides of
/// one operation must carry the same name.
///
/// `expected` is the type the operation signature declares for the slot the
/// value occupies; each represented type nested inside then directs its own
/// subtree. The acceptance rule is `same_type`: a tag may spell the expected
/// type canonically or by that type's own short name — the two spellings one
/// nominal type legitimately carries, program side and boundary side. Any
/// other tag belongs to a different type that happens to share letters and
/// is reported, never silently rewritten: the tag is the identity.
///
/// Slots whose expected type is not a represented boundary type — scalars,
/// capability resources, mismatched shapes — pass through untouched; the
/// boundary and the replay comparer already own their diagnostics.
pub fn canonicalize_boundary_names(
    value: &Value,
    expected: &Type,
    scope: &str,
    contracts: &CapabilityRegistry,
) -> Result<Value, BoundaryTypeMismatch> {
    Ok(match (expected, value) {
        (Type::Result(ok, _), Value::Ok(inner)) => Value::Ok(Box::new(
            canonicalize_boundary_names(inner, ok, scope, contracts)?,
        )),
        (Type::Result(_, err), Value::Err(inner)) => Value::Err(Box::new(
            canonicalize_boundary_names(inner, err, scope, contracts)?,
        )),
        (Type::Option(inner), Value::Some(payload)) => Value::Some(Box::new(
            canonicalize_boundary_names(payload, inner, scope, contracts)?,
        )),
        (Type::Option(_), Value::None) => Value::None,
        (Type::List(inner), _) => match crate::value::list_to_vec(value) {
            Some(items) => crate::value::list_from_vec(
                items
                    .iter()
                    .map(|item| canonicalize_boundary_names(item, inner, scope, contracts))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            None => value.clone(),
        },
        (Type::Vector(inner), Value::Vector(items)) => {
            Value::Vector(aver_rt::AverVector::from_vec(
                items
                    .iter()
                    .map(|item| canonicalize_boundary_names(item, inner, scope, contracts))
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
        (Type::Tuple(types), Value::Tuple(items)) if types.len() == items.len() => Value::Tuple(
            types
                .iter()
                .zip(items.iter())
                .map(|(ty, item)| canonicalize_boundary_names(item, ty, scope, contracts))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        (Type::Map(key_ty, value_ty), Value::Map(entries)) => Value::Map(
            entries
                .iter()
                .map(|(key, entry)| {
                    Ok((
                        canonicalize_boundary_names(key, key_ty, scope, contracts)?,
                        canonicalize_boundary_names(entry, value_ty, scope, contracts)?,
                    ))
                })
                .collect::<Result<HashMap<_, _>, BoundaryTypeMismatch>>()?,
        ),
        (
            Type::Named { name, .. },
            Value::Record {
                type_name,
                fields: record_fields,
            },
        ) => {
            let canonical = canonical_type(scope, name);
            let Some(TypeDef::Product { fields, .. }) = contracts.boundary_type(&canonical) else {
                return Ok(value.clone());
            };
            if !same_type(&canonical, type_name) {
                return Err(BoundaryTypeMismatch {
                    expected: canonical,
                    received: type_name.clone(),
                });
            }
            let record_scope = canonical.rsplit_once('.').map_or(scope, |(m, _)| m);
            let mut out = Vec::with_capacity(record_fields.len());
            for (field_name, field_value) in record_fields.iter() {
                let field_ty = fields
                    .iter()
                    .find(|(declared, _)| declared == field_name)
                    .and_then(|(_, source)| crate::types::parse_type_str_strict(source).ok());
                let field_value = match field_ty {
                    Some(ty) => {
                        canonicalize_boundary_names(field_value, &ty, record_scope, contracts)?
                    }
                    None => field_value.clone(),
                };
                out.push((field_name.clone(), field_value));
            }
            Value::Record {
                type_name: canonical,
                fields: out.into(),
            }
        }
        (
            Type::Named { name, .. },
            Value::Variant {
                type_name,
                variant,
                fields: variant_fields,
            },
        ) => {
            let canonical = canonical_type(scope, name);
            let Some(TypeDef::Sum { variants, .. }) = contracts.boundary_type(&canonical) else {
                return Ok(value.clone());
            };
            if !same_type(&canonical, type_name) {
                return Err(BoundaryTypeMismatch {
                    expected: canonical,
                    received: type_name.clone(),
                });
            }
            let variant_scope = canonical.rsplit_once('.').map_or(scope, |(m, _)| m);
            let declared = variants
                .iter()
                .find(|candidate| candidate.name == *variant)
                .map(|candidate| candidate.fields.as_slice())
                .unwrap_or(&[]);
            let mut out = Vec::with_capacity(variant_fields.len());
            for (index, field_value) in variant_fields.iter().enumerate() {
                let field_ty = declared
                    .get(index)
                    .and_then(|source| crate::types::parse_type_str_strict(source).ok());
                let field_value = match field_ty {
                    Some(ty) => {
                        canonicalize_boundary_names(field_value, &ty, variant_scope, contracts)?
                    }
                    None => field_value.clone(),
                };
                out.push(field_value);
            }
            Value::Variant {
                type_name: canonical,
                variant: variant.clone(),
                fields: out.into(),
            }
        }
        _ => value.clone(),
    })
}
