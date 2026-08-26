//! Contract-directed bridge between native wasm-gc values and ProviderValue.

use std::collections::{BTreeMap, BTreeSet};

use aver::ast::{Type, TypeDef};
use aver::provider::{ProviderRegistry, ProviderValue};
use wasmtime::{Caller, ExternRef, Val};

use super::guest::{
    decode_bytes, decode_int, decode_string, encode_bytes, encode_int, encode_string,
    expected_value, helper_call, helper_i32, helper_optional_call, helper_optional_value,
    helper_value, required_val,
};
use super::{BoundResource, ReplayResource, RunWasmGcHost};

pub(super) fn decode_value(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: Option<&Val>,
    ty: &Type,
    scope: &str,
    providers: &ProviderRegistry,
) -> Result<ProviderValue, String> {
    match ty {
        Type::Int => decode_int(caller, required_val(value, ty)?).map(ProviderValue::Int),
        Type::Float => match value {
            Some(Val::F64(bits)) => Ok(ProviderValue::Float(f64::from_bits(*bits))),
            _ => Err(expected_value(ty, value)),
        },
        Type::Str => decode_string(caller, required_val(value, ty)?).map(ProviderValue::String),
        Type::Bool => match value {
            Some(Val::I32(value)) => Ok(ProviderValue::Bool(*value != 0)),
            _ => Err(expected_value(ty, value)),
        },
        Type::Unit => Ok(ProviderValue::Unit),
        Type::Result(ok, err) => {
            let value = required_val(value, ty)?;
            match helper_i32(caller, ty, "tag", std::slice::from_ref(value))? {
                1 => {
                    let payload = helper_optional_value(caller, ty, "ok_value", value)?;
                    decode_value(caller, payload.as_ref(), ok, scope, providers)
                        .map(|value| ProviderValue::ResultOk(Box::new(value)))
                }
                _ => {
                    let payload = helper_optional_value(caller, ty, "err_value", value)?;
                    decode_value(caller, payload.as_ref(), err, scope, providers)
                        .map(|value| ProviderValue::ResultErr(Box::new(value)))
                }
            }
        }
        Type::Option(inner) => {
            let value = required_val(value, ty)?;
            if helper_i32(caller, ty, "tag", std::slice::from_ref(value))? == 1 {
                let payload = helper_optional_value(caller, ty, "value", value)?;
                decode_value(caller, payload.as_ref(), inner, scope, providers)
                    .map(|value| ProviderValue::OptionSome(Box::new(value)))
            } else {
                Ok(ProviderValue::OptionNone)
            }
        }
        Type::List(inner) => decode_list(
            caller,
            required_val(value, ty)?,
            ty,
            inner,
            scope,
            providers,
        )
        .map(ProviderValue::List),
        Type::Tuple(types) => {
            let value = required_val(value, ty)?;
            let mut fields = Vec::with_capacity(types.len());
            for (index, field_ty) in types.iter().enumerate() {
                let field = if matches!(field_ty, Type::Unit) {
                    None
                } else {
                    Some(helper_value(
                        caller,
                        ty,
                        &format!("field_{index}"),
                        std::slice::from_ref(value),
                    )?)
                };
                fields.push(decode_value(
                    caller,
                    field.as_ref(),
                    field_ty,
                    scope,
                    providers,
                )?);
            }
            Ok(ProviderValue::Tuple(fields))
        }
        Type::Vector(inner) => {
            let value = required_val(value, ty)?;
            let len = helper_i32(caller, ty, "len", std::slice::from_ref(value))?.max(0);
            let mut values = Vec::with_capacity(len as usize);
            for index in 0..len {
                let item =
                    helper_optional_call(caller, ty, "get", &[value.to_owned(), Val::I32(index)])?;
                values.push(decode_value(
                    caller,
                    item.as_ref(),
                    inner,
                    scope,
                    providers,
                )?);
            }
            Ok(ProviderValue::Vector(values))
        }
        Type::Map(key_ty, value_ty) => {
            let map = required_val(value, ty)?;
            let keys_ty = Type::List(key_ty.clone());
            let keys = helper_value(caller, ty, "keys", std::slice::from_ref(map))?;
            let keys =
                decode_list_with_guest_values(caller, &keys, &keys_ty, key_ty, scope, providers)?;
            let mut entries = Vec::with_capacity(keys.len());
            for (key, guest_key) in keys {
                let option_ty = Type::Option(value_ty.clone());
                let option = helper_value(caller, ty, "get", &[map.to_owned(), guest_key])?;
                if helper_i32(caller, &option_ty, "tag", std::slice::from_ref(&option))? != 1 {
                    return Err("Map.keys returned a key that Map.get could not find".into());
                }
                let guest_value = helper_optional_value(caller, &option_ty, "value", &option)?;
                let value = decode_value(caller, guest_value.as_ref(), value_ty, scope, providers)?;
                entries.push((key, value));
            }
            Ok(ProviderValue::Map(entries))
        }
        Type::Named { name, .. } if is_bytes(name) => {
            decode_bytes(caller, required_val(value, ty)?).map(ProviderValue::Bytes)
        }
        Type::Named { name, .. } => {
            let canonical = canonical_named(scope, name, providers);
            if providers
                .contracts()
                .resource_types()
                .any(|known| known == &canonical)
            {
                return decode_resource(
                    caller,
                    required_val(value, ty)?,
                    &canonical,
                    scope,
                    providers,
                );
            }
            let definition = providers
                .contracts()
                .boundary_type(&canonical)
                .ok_or_else(|| format!("unknown represented boundary type '{canonical}'"))?;
            decode_represented(
                caller,
                required_val(value, ty)?,
                ty,
                &canonical,
                definition,
                scope,
                providers,
            )
        }
        Type::Fn(_, _, _) | Type::Var(_) | Type::Invalid => Err(format!(
            "unsupported provider boundary type {}",
            ty.display()
        )),
    }
}

pub(super) fn encode_value(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: ProviderValue,
    ty: &Type,
    scope: &str,
    minted_resource: Option<&str>,
    providers: &ProviderRegistry,
) -> Result<Option<Val>, String> {
    match (ty, value) {
        (Type::Int, ProviderValue::Int(value)) => encode_int(caller, &value).map(Some),
        (Type::Float, ProviderValue::Float(value)) => Ok(Some(Val::F64(value.to_bits()))),
        (Type::Str, ProviderValue::String(value)) => encode_string(caller, &value).map(Some),
        (Type::Bool, ProviderValue::Bool(value)) => Ok(Some(Val::I32(i32::from(value)))),
        (Type::Unit, ProviderValue::Unit) => Ok(None),
        (Type::Result(ok, _), ProviderValue::ResultOk(value)) => {
            let value = encode_value(caller, *value, ok, scope, minted_resource, providers)?;
            helper_optional_call(caller, ty, "ok", &value.into_iter().collect::<Vec<_>>())
        }
        (Type::Result(_, err), ProviderValue::ResultErr(value)) => {
            let value = encode_value(caller, *value, err, scope, minted_resource, providers)?;
            helper_optional_call(caller, ty, "err", &value.into_iter().collect::<Vec<_>>())
        }
        (Type::Option(inner), ProviderValue::OptionSome(value)) => {
            let value = encode_value(caller, *value, inner, scope, minted_resource, providers)?;
            helper_optional_call(caller, ty, "some", &value.into_iter().collect::<Vec<_>>())
        }
        (Type::Option(_), ProviderValue::OptionNone) => {
            helper_optional_call(caller, ty, "none", &[])
        }
        (Type::List(inner), ProviderValue::List(values)) => {
            let mut list = helper_value(caller, ty, "nil", &[])?;
            for value in values.into_iter().rev() {
                let mut params =
                    encode_value(caller, value, inner, scope, minted_resource, providers)?
                        .into_iter()
                        .collect::<Vec<_>>();
                params.push(list);
                list = helper_value(caller, ty, "cons", &params)?;
            }
            Ok(Some(list))
        }
        (Type::Tuple(types), ProviderValue::Tuple(values)) if types.len() == values.len() => {
            let mut params = Vec::new();
            for (field_ty, value) in types.iter().zip(values) {
                params.extend(encode_value(
                    caller,
                    value,
                    field_ty,
                    scope,
                    minted_resource,
                    providers,
                )?);
            }
            helper_optional_call(caller, ty, "make", &params)
        }
        (Type::Vector(inner), ProviderValue::Vector(values)) => {
            let len = i32::try_from(values.len())
                .map_err(|_| "provider Vector exceeds the wasm-gc i32 length limit")?;
            let vector = helper_value(caller, ty, "new", &[Val::I32(len)])?;
            for (index, value) in values.into_iter().enumerate() {
                let mut params = vec![vector, Val::I32(index as i32)];
                params.extend(encode_value(
                    caller,
                    value,
                    inner,
                    scope,
                    minted_resource,
                    providers,
                )?);
                helper_call(caller, ty, "set", &params)?;
            }
            Ok(Some(vector))
        }
        (Type::Map(key_ty, value_ty), ProviderValue::Map(values)) => {
            let mut map = helper_value(caller, ty, "empty", &[])?;
            let mut seen = BTreeSet::new();
            for (key, value) in values {
                let order = aver_rt::provider::provider_value_order_key(&key)?;
                if !seen.insert(order) {
                    return Err("provider Map contains a duplicate key".into());
                }
                let mut params = vec![map];
                params.extend(encode_value(
                    caller,
                    key,
                    key_ty,
                    scope,
                    minted_resource,
                    providers,
                )?);
                params.extend(encode_value(
                    caller,
                    value,
                    value_ty,
                    scope,
                    minted_resource,
                    providers,
                )?);
                map = helper_value(caller, ty, "set", &params)?;
            }
            Ok(Some(map))
        }
        (Type::Named { name, .. }, ProviderValue::Bytes(bytes)) if is_bytes(name) => {
            encode_bytes(caller, &bytes).map(Some)
        }
        (Type::Named { name, .. }, ProviderValue::Resource(resource)) => {
            let canonical = canonical_named(scope, name, providers);
            if !providers
                .contracts()
                .resource_types()
                .any(|known| known == &canonical)
            {
                return Err(format!("type '{canonical}' is not a capability resource"));
            }
            if minted_resource != Some(canonical.as_str()) {
                return Err(format!(
                    "resource '{canonical}' may only be returned by its minting operation"
                ));
            }
            let binding = providers
                .binding(scope)
                .ok_or_else(|| format!("capability '{scope}' has no live provider binding"))?;
            let reference = ExternRef::new(
                &mut *caller,
                BoundResource {
                    binding_id: binding.runtime_id(),
                    type_name: canonical,
                    resource,
                },
            )
            .map_err(|error| format!("allocate capability resource externref: {error:#}"))?;
            Ok(Some(Val::ExternRef(Some(reference))))
        }
        (Type::Named { name, .. }, value) => {
            let canonical = canonical_named(scope, name, providers);
            let definition = providers
                .contracts()
                .boundary_type(&canonical)
                .ok_or_else(|| format!("unknown represented boundary type '{canonical}'"))?;
            encode_represented(
                caller,
                value,
                ty,
                &canonical,
                definition,
                scope,
                minted_resource,
                providers,
            )
            .map(Some)
        }
        (expected, actual) => Err(format!(
            "expected {}, got {}",
            expected.display(),
            actual.shape()
        )),
    }
}

fn decode_represented(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: &Val,
    helper_ty: &Type,
    canonical: &str,
    definition: &TypeDef,
    scope: &str,
    providers: &ProviderRegistry,
) -> Result<ProviderValue, String> {
    match definition {
        TypeDef::Product { fields, .. } => {
            let mut values = Vec::with_capacity(fields.len());
            for (name, source_ty) in fields {
                let ty = aver::types::parse_type_str_strict(source_ty)
                    .map_err(|bad| format!("invalid field type '{bad}'"))?;
                let field = if matches!(ty, Type::Unit) {
                    None
                } else {
                    Some(helper_value(
                        caller,
                        helper_ty,
                        &format!(
                            "field_{}",
                            aver::codegen::wasip2::plan::encode_interface_identifier(name)
                        ),
                        std::slice::from_ref(value),
                    )?)
                };
                values.push((
                    name.clone(),
                    decode_value(caller, field.as_ref(), &ty, scope, providers)?,
                ));
            }
            Ok(ProviderValue::Record {
                type_name: canonical.to_string(),
                fields: values,
            })
        }
        TypeDef::Sum { variants, .. } => {
            let kind = helper_i32(caller, helper_ty, "kind", std::slice::from_ref(value))?;
            let variant = variants
                .get(kind.max(0) as usize)
                .ok_or_else(|| format!("sum '{canonical}' returned invalid variant tag {kind}"))?;
            let variant_stem = format!(
                "variant_{}",
                aver::codegen::wasip2::plan::encode_interface_identifier(&variant.name)
            );
            let mut fields = Vec::with_capacity(variant.fields.len());
            for (index, source_ty) in variant.fields.iter().enumerate() {
                let ty = aver::types::parse_type_str_strict(source_ty)
                    .map_err(|bad| format!("invalid variant field type '{bad}'"))?;
                let field = if matches!(ty, Type::Unit) {
                    None
                } else {
                    Some(helper_value(
                        caller,
                        helper_ty,
                        &format!("{variant_stem}_field_{index}"),
                        std::slice::from_ref(value),
                    )?)
                };
                fields.push(decode_value(caller, field.as_ref(), &ty, scope, providers)?);
            }
            Ok(ProviderValue::Variant {
                type_name: canonical.to_string(),
                variant: variant.name.clone(),
                fields,
            })
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn encode_represented(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: ProviderValue,
    helper_ty: &Type,
    canonical: &str,
    definition: &TypeDef,
    scope: &str,
    minted_resource: Option<&str>,
    providers: &ProviderRegistry,
) -> Result<Val, String> {
    match (definition, value) {
        (
            TypeDef::Product { fields, .. },
            ProviderValue::Record {
                type_name,
                fields: values,
            },
        ) if type_name == canonical => {
            let mut by_name = BTreeMap::new();
            for (name, value) in values {
                if by_name.insert(name.clone(), value).is_some() {
                    return Err(format!("record '{canonical}' repeats field '{name}'"));
                }
            }
            let mut params = Vec::new();
            for (name, source_ty) in fields {
                let ty = aver::types::parse_type_str_strict(source_ty)
                    .map_err(|bad| format!("invalid field type '{bad}'"))?;
                let value = by_name
                    .remove(name)
                    .ok_or_else(|| format!("record '{canonical}' is missing field '{name}'"))?;
                params.extend(encode_value(
                    caller,
                    value,
                    &ty,
                    scope,
                    minted_resource,
                    providers,
                )?);
            }
            if !by_name.is_empty() {
                return Err(format!("record '{canonical}' has unknown fields"));
            }
            helper_value(caller, helper_ty, "make", &params)
        }
        (
            TypeDef::Sum { variants, .. },
            ProviderValue::Variant {
                type_name,
                variant,
                fields,
            },
        ) if type_name == canonical => {
            let definition = variants
                .iter()
                .find(|candidate| candidate.name == variant)
                .ok_or_else(|| format!("unknown variant '{canonical}.{variant}'"))?;
            if definition.fields.len() != fields.len() {
                return Err(format!("variant '{canonical}.{variant}' has wrong arity"));
            }
            let mut params = Vec::new();
            for (source_ty, value) in definition.fields.iter().zip(fields) {
                let ty = aver::types::parse_type_str_strict(source_ty)
                    .map_err(|bad| format!("invalid variant field type '{bad}'"))?;
                params.extend(encode_value(
                    caller,
                    value,
                    &ty,
                    scope,
                    minted_resource,
                    providers,
                )?);
            }
            helper_value(
                caller,
                helper_ty,
                &format!(
                    "variant_{}_make",
                    aver::codegen::wasip2::plan::encode_interface_identifier(&variant)
                ),
                &params,
            )
        }
        _ => Err(format!("expected represented boundary type '{canonical}'")),
    }
}

fn decode_list(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: &Val,
    list_ty: &Type,
    inner: &Type,
    scope: &str,
    providers: &ProviderRegistry,
) -> Result<Vec<ProviderValue>, String> {
    decode_list_with_guest_values(caller, value, list_ty, inner, scope, providers)
        .map(|values| values.into_iter().map(|(value, _)| value).collect())
}

fn decode_list_with_guest_values(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: &Val,
    list_ty: &Type,
    inner: &Type,
    scope: &str,
    providers: &ProviderRegistry,
) -> Result<Vec<(ProviderValue, Val)>, String> {
    let mut cursor = *value;
    let mut values = Vec::new();
    while helper_i32(caller, list_ty, "is_empty", std::slice::from_ref(&cursor))? == 0 {
        let head = helper_optional_value(caller, list_ty, "head", &cursor)?;
        let decoded = decode_value(caller, head.as_ref(), inner, scope, providers)?;
        let guest = head.unwrap_or(Val::I32(0));
        values.push((decoded, guest));
        cursor = helper_value(caller, list_ty, "tail", std::slice::from_ref(&cursor))?;
    }
    Ok(values)
}

fn decode_resource(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: &Val,
    expected: &str,
    capability: &str,
    providers: &ProviderRegistry,
) -> Result<ProviderValue, String> {
    let Val::ExternRef(Some(reference)) = value else {
        return Err(format!(
            "expected non-null externref for resource '{expected}'"
        ));
    };
    let data = reference
        .data(&*caller)
        .map_err(|error| format!("read resource externref: {error:#}"))?
        .ok_or_else(|| "resource externref has no host payload".to_string())?;
    let bound = data
        .downcast_ref::<BoundResource>()
        .ok_or_else(|| "resource externref was not minted by the provider adapter".to_string())?;
    let binding = providers
        .binding(capability)
        .ok_or_else(|| format!("capability '{capability}' has no provider binding"))?;
    if bound.binding_id != binding.runtime_id() || bound.type_name != expected {
        return Err(format!(
            "resource identity mismatch: expected '{expected}' from the current '{capability}' binding"
        ));
    }
    Ok(ProviderValue::Resource(bound.resource.clone()))
}

pub(super) fn ensure_live_resources(values: &[ProviderValue]) -> Result<(), String> {
    fn walk(value: &ProviderValue) -> Result<(), String> {
        match value {
            ProviderValue::Resource(resource)
                if resource.downcast_ref::<ReplayResource>().is_some() =>
            {
                Err("replay-only capability resource cannot cross into a live provider".into())
            }
            ProviderValue::Tuple(values)
            | ProviderValue::List(values)
            | ProviderValue::Vector(values) => {
                for value in values {
                    walk(value)?;
                }
                Ok(())
            }
            ProviderValue::Map(values) => {
                for (key, value) in values {
                    walk(key)?;
                    walk(value)?;
                }
                Ok(())
            }
            ProviderValue::ResultOk(value)
            | ProviderValue::ResultErr(value)
            | ProviderValue::OptionSome(value) => walk(value),
            ProviderValue::Record { fields, .. } => {
                for (_, value) in fields {
                    walk(value)?;
                }
                Ok(())
            }
            ProviderValue::Variant { fields, .. } => {
                for value in fields {
                    walk(value)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
    for value in values {
        walk(value)?;
    }
    Ok(())
}

pub(super) fn canonical_named(scope: &str, name: &str, providers: &ProviderRegistry) -> String {
    if name.contains('.') || is_bytes(name) {
        return name.to_string();
    }
    let scoped = format!("{scope}.{name}");
    if providers.contracts().boundary_type(&scoped).is_some()
        || providers
            .contracts()
            .resource_types()
            .any(|resource| resource == &scoped)
    {
        scoped
    } else {
        name.to_string()
    }
}

pub(super) fn is_bytes(name: &str) -> bool {
    matches!(name, "Bytes" | "Bytes.Bytes")
}
