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
    // The wait set is named by the key this program keys its waits by, so an
    // external host reads the four type names out of the descriptor rather
    // than assuming whole numbers. `waitSet` and `ready` below say which they
    // are; the descriptors themselves are what the host moves values through.
    let wait = registry.wait_set_type_names();
    let mut names = vec![
        "Result<Unit,String>".to_string(),
        "Result<Int,String>".to_string(),
    ];
    if let Some(wait) = &wait {
        names.push(wait.set.clone());
        names.push(wait.list.clone());
        names.push(wait.result.clone());
    }
    for name in &names {
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
    let mut manifest = serde_json::Map::new();
    manifest.insert("version".to_string(), json!(1));
    manifest.insert("kinds".to_string(), json!(kinds));
    manifest.insert("types".to_string(), json!(descriptors));
    if let Some(wait) = &wait {
        // The names here address the descriptors above, which are keyed the
        // way a type prints rather than the way the registry normalises one.
        let named = |name: &str| crate::types::parse_type_str(name).display();
        manifest.insert(
            "wait".to_string(),
            json!({
                "set": named(&wait.set),
                "key": named(&wait.key),
                "ready": named(&wait.list),
                "answer": named(&wait.result),
            }),
        );
    }
    serde_json::to_vec(&Value::Object(manifest))
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
