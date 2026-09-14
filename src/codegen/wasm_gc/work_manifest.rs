//! Byte-bound schema for external hosts of the Work ABI. Type names address
//! the existing capability factories, so JS never guesses GC layouts.

use super::{WasmGcError, types::TypeRegistry};
use crate::{ast::Type, capability::work::JobKindPlan};
use serde_json::{Value, json};
use std::collections::{BTreeMap, HashSet};

pub(super) fn render(
    kinds: &[JobKindPlan],
    registry: &TypeRegistry,
) -> Result<Vec<u8>, WasmGcError> {
    let mut boundary = BTreeMap::new();
    for kind in kinds {
        super::capability_abi::collect_type(
            &kind.begin.return_type,
            registry,
            &mut boundary,
            &mut HashSet::new(),
        );
        for ty in kind.recorded_types() {
            super::capability_abi::collect_type(&ty, registry, &mut boundary, &mut HashSet::new());
        }
    }
    for name in [
        "Map<Int,Wait.Item>",
        "Result<Unit,String>",
        "Result<Int,String>",
    ] {
        if registry.map_slots(name).is_some() || registry.result_type_idx(name).is_some() {
            super::capability_abi::collect_type(
                &crate::types::parse_type_str(name),
                registry,
                &mut boundary,
                &mut HashSet::new(),
            );
        }
    }
    let descriptors = boundary
        .iter()
        .map(|(name, ty)| Ok((name.clone(), describe(ty, registry)?)))
        .collect::<Result<BTreeMap<_, _>, WasmGcError>>()?;
    let kinds: Vec<_> = kinds.iter().enumerate().map(|(index, kind)| json!({
        "name": kind.shape.capability, "task": kind.shape.task.display(),
        "payload": kind.shape.payload.display(), "run": format!("__work_v1_run_{index}"),
        "boxedTask": kind.recorded_types()[0].display(), "answer": kind.recorded_types()[1].display(),
    })).collect();
    serde_json::to_vec(&json!({"version": 1, "kinds": kinds, "types": descriptors}))
        .map_err(|error| WasmGcError::Validation(format!("work ABI metadata: {error}")))
}

fn describe(ty: &Type, registry: &TypeRegistry) -> Result<Value, WasmGcError> {
    let descriptor = match ty {
        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit => {
            json!({"kind": ty.display()})
        }
        Type::Result(a, b) => json!({"kind":"Result", "args":[a.display(), b.display()]}),
        Type::Map(a, b) => json!({"kind":"Map", "args":[a.display(), b.display()]}),
        Type::Option(a) => json!({"kind":"Option", "args":[a.display()]}),
        Type::List(a) => json!({"kind":"List", "args":[a.display()]}),
        Type::Vector(a) => json!({"kind":"Vector", "args":[a.display()]}),
        Type::Tuple(args) => {
            json!({"kind":"Tuple", "args":args.iter().map(Type::display).collect::<Vec<_>>()})
        }
        // backend-link-stage: collect_type supplies names from the linked
        // TypeRegistry, including parsed field types without source TypeIds.
        // Describe the same name-keyed representations as capability_abi's
        // exported factories, which the external host uses to move values.
        Type::Named { name, .. } if name == "Work.Job" || registry.is_capability_resource(name) => {
            json!({"kind":"Resource"})
        }
        Type::Named { name, .. } if name == "Bytes" || name == "Bytes.Bytes" => {
            json!({"kind":"Bytes"})
        }
        Type::Named { name, .. } => {
            if let Some(fields) = registry.record_fields.get(name).or_else(|| {
                name.rsplit_once('.')
                    .and_then(|(_, bare)| registry.record_fields.get(bare))
            }) {
                json!({"kind":"Record", "fields":fields.iter().map(|(name, ty)| (name, crate::types::parse_type_str(ty).display())).collect::<Vec<_>>()})
            } else {
                let mut variants: Vec<_> = registry
                    .variants
                    .iter()
                    .flat_map(|(variant_name, variants)| {
                        variants.iter().map(move |variant| (variant_name, variant))
                    })
                    .filter(|(_, variant)| {
                        variant.parent == *name
                            || name
                                .rsplit_once('.')
                                .is_some_and(|(_, bare)| variant.parent == bare)
                    })
                    .collect();
                variants.sort_by_key(|(_, variant)| variant.type_idx);
                if variants.is_empty() {
                    return Err(WasmGcError::Validation(format!(
                        "work ABI cannot describe {name}"
                    )));
                }
                json!({"kind":"Sum", "variants":variants.iter().map(|(name, variant)| json!({"name":name, "fields":variant.fields.iter().map(|ty| crate::types::parse_type_str(ty).display()).collect::<Vec<_>>()})).collect::<Vec<_>>()})
            }
        }
        _ => {
            return Err(WasmGcError::Validation(format!(
                "work ABI cannot transport {}",
                ty.display()
            )));
        }
    };
    Ok(descriptor)
}
