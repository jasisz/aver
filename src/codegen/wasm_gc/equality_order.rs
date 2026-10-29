//! Adapts flattened wasm declarations to the shared comparison-shape walk.

use super::types::TypeRegistry;
use crate::ir::TypeKey;
use crate::ir::compare_shape::{NominalFields, cheap_fields_first};
use crate::types::{Type, parse_type_str};

/// Comparison positions mapped to original struct field indices. Layout and
/// hashing retain declaration order; only equality visits scalar fields first.
pub(super) fn field_order(types: &[&str], registry: &TypeRegistry) -> Vec<usize> {
    let parsed: Vec<Type> = types.iter().map(|ty| parse_type_str(ty)).collect();
    cheap_fields_first(&parsed, None, &|_, name, _| {
        // Flattening has already resolved module-local spellings and assigned
        // distinct names to declarations that shared a bare source name.
        let name = registry.canonical_type_name(name);
        let fields = if let Some(fields) = registry.record_fields.get(name) {
            fields.iter().map(|(_, ty)| parse_type_str(ty)).collect()
        } else if registry.sum_roots.contains_key(name) {
            registry
                .variants
                .values()
                .flatten()
                .filter(|variant| variant.parent == name)
                .flat_map(|variant| variant.fields.iter())
                .map(|ty| parse_type_str(ty))
                .collect()
        } else {
            return None;
        };
        Some(NominalFields {
            key: TypeKey::entry(name),
            fields,
        })
    })
    .unwrap_or_else(|| (0..types.len()).collect())
}
