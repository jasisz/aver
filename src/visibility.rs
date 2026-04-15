use crate::ast::{TopLevel, TypeDef};

/// Type definition collected from a module — backend-agnostic metadata.
#[derive(Debug, Clone)]
pub struct ModuleTypeDef {
    pub bare_name: String,
    pub kind: ModuleTypeKind,
}

#[derive(Debug, Clone)]
pub enum ModuleTypeKind {
    Record { field_names: Vec<String> },
    Sum { variant_names: Vec<String> },
}

/// Collect all type definitions from parsed AST items.
/// Pure function over AST — no backend state.
pub fn collect_module_types(items: &[TopLevel]) -> Vec<ModuleTypeDef> {
    items
        .iter()
        .filter_map(|item| {
            let TopLevel::TypeDef(td) = item else {
                return None;
            };
            Some(match td {
                TypeDef::Product { name, fields, .. } => ModuleTypeDef {
                    bare_name: name.clone(),
                    kind: ModuleTypeKind::Record {
                        field_names: fields.iter().map(|(n, _)| n.clone()).collect(),
                    },
                },
                TypeDef::Sum { name, variants, .. } => ModuleTypeDef {
                    bare_name: name.clone(),
                    kind: ModuleTypeKind::Sum {
                        variant_names: variants.iter().map(|v| v.name.clone()).collect(),
                    },
                },
            })
        })
        .collect()
}

/// Check whether a module item is exposed to the outside.
/// `exposes = None` means the module uses the default rule (hide `_`-prefixed items).
/// `exposes = Some(list)` means only items in the explicit list are exposed.
pub fn is_exposed(name: &str, exposes: Option<&[String]>) -> bool {
    match exposes {
        Some(list) => list.iter().any(|e| e == name),
        None => !name.starts_with('_'),
    }
}
