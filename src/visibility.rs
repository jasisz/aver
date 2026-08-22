use std::collections::{HashMap, HashSet};

use crate::ast::{FnDef, Module, TopLevel, TypeDef, TypeVariant};

mod type_exports;

pub use type_exports::{
    ExportedTypeTarget, ModuleTypeExports, ModuleTypeSurface, collect_module_type_exports,
    collect_type_exports,
};

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

// ---------------------------------------------------------------------------
// Module exports — the shared answer to "what can importers see?"
// ---------------------------------------------------------------------------

/// An exported type definition with its opaque flag.
pub struct ExportedTypeDef<'a> {
    pub def: &'a TypeDef,
    pub is_opaque: bool,
}

/// Everything a module exports — functions and types that passed visibility filtering.
pub struct ModuleExports<'a> {
    pub functions: Vec<&'a FnDef>,
    pub types: Vec<ExportedTypeDef<'a>>,
}

/// Extract the module declaration from parsed items.
pub fn module_decl(items: &[TopLevel]) -> Option<&Module> {
    items.iter().find_map(|i| {
        if let TopLevel::Module(m) = i {
            Some(m)
        } else {
            None
        }
    })
}

/// An `exposes [...]` list as visibility reads it: an empty list is the
/// absence of the clause (default rule), never "expose nothing".
pub fn declared_exposes(exposes: &[String]) -> Option<&[String]> {
    if exposes.is_empty() {
        None
    } else {
        Some(exposes)
    }
}

/// The explicit `exposes` list a file declares, if any.
/// The single derivation of the argument `is_exposed` expects, so lints and
/// codegen can never disagree about what "private" means.
pub fn effective_exposes(items: &[TopLevel]) -> Option<&[String]> {
    module_decl(items).and_then(|m| declared_exposes(&m.exposes))
}

/// Collect all exported items from a parsed module.
/// Applies visibility rules: exposes list, underscore convention, opaque types.
pub fn collect_module_exports<'a>(items: &'a [TopLevel]) -> ModuleExports<'a> {
    let module = module_decl(items);
    let exposes = effective_exposes(items);

    let opaque_names: Vec<&str> = module
        .map(|m| m.exposes_opaque.iter().map(|s| s.as_str()).collect())
        .unwrap_or_default();

    let functions = items
        .iter()
        .filter_map(|item| {
            let TopLevel::FnDef(fd) = item else {
                return None;
            };
            if is_exposed(&fd.name, exposes) {
                Some(fd)
            } else {
                None
            }
        })
        .collect();

    let types = items
        .iter()
        .filter_map(|item| {
            let TopLevel::TypeDef(td) = item else {
                return None;
            };
            let name = match td {
                TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name.as_str(),
            };
            let is_opaque = opaque_names.contains(&name);
            if is_exposed(name, exposes) || is_opaque {
                Some(ExportedTypeDef { def: td, is_opaque })
            } else {
                None
            }
        })
        .collect();

    ModuleExports { functions, types }
}

/// Collect ALL functions and types from a module — no visibility filtering.
/// Used by codegen which emits full module implementations including private helpers.
pub fn collect_all_module_symbols<'a>(items: &'a [TopLevel]) -> ModuleExports<'a> {
    let functions = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some(fd)
            } else {
                None
            }
        })
        .collect();

    let module = module_decl(items);
    let opaque_names: Vec<&str> = module
        .map(|m| m.exposes_opaque.iter().map(|s| s.as_str()).collect())
        .unwrap_or_default();

    let types = items
        .iter()
        .filter_map(|item| {
            let TopLevel::TypeDef(td) = item else {
                return None;
            };
            let name = match td {
                TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name.as_str(),
            };
            Some(ExportedTypeDef {
                def: td,
                is_opaque: opaque_names.contains(&name),
            })
        })
        .collect();

    ModuleExports { functions, types }
}

// ---------------------------------------------------------------------------
// Canonical symbol key construction
// ---------------------------------------------------------------------------

/// "Module.function" — qualified name for cross-module function references.
pub fn qualified_name(module: &str, name: &str) -> String {
    format!("{}.{}", module, name)
}

/// "Type.member" — type-scoped key (constructor, field, variant).
pub fn member_key(type_name: &str, member: &str) -> String {
    format!("{}.{}", type_name, member)
}

/// "Module.Type.member" — fully-qualified type-scoped key.
pub fn qualified_member_key(module: &str, type_name: &str, member: &str) -> String {
    format!("{}.{}.{}", module, type_name, member)
}

// ---------------------------------------------------------------------------
// SymbolRegistry — aggregated view of all module exports
// ---------------------------------------------------------------------------

/// A registered symbol with its canonical name and kind.
#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub id: u32,
    pub canonical_name: String,
    pub alias: Option<String>,
    pub module: String,
    pub kind: SymbolKind,
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Function {
        name: String,
        params: Vec<(String, String)>,
        return_type: String,
        effects: Vec<String>,
    },
    OpaqueType {
        name: String,
    },
    SumType {
        name: String,
        variants: Vec<String>,
    },
    /// Top-level entry for a `record` (product) type. Lets
    /// `Type::Named("MyRecord")` resolve to a canonical
    /// `Module.MyRecord` the same way sum types do, instead of
    /// being implicit-from-its-fields and forcing per-backend
    /// special-casing of the bare name.
    ProductType {
        name: String,
        fields: Vec<String>,
    },
    Constructor {
        type_name: String,
        variant_name: String,
        field_types: Vec<String>,
    },
    RecordField {
        type_name: String,
        field_name: String,
        field_type: String,
    },
}

/// All symbols exported by a module tree — canonical source of truth.
#[derive(Debug, Clone, Default)]
pub struct SymbolRegistry {
    pub entries: Vec<SymbolEntry>,
}

impl SymbolRegistry {
    /// Build a registry of exported symbols from a set of loaded modules.
    pub fn from_modules(modules: &[(String, Vec<TopLevel>)]) -> Self {
        let mut entries = Vec::new();
        for (module_name, items) in modules {
            let exports = collect_module_exports(items);
            Self::collect_from_exports(module_name, &exports, &mut entries);
        }
        SymbolRegistry { entries }
    }

    /// Build the symbols visible through a module's direct dependency list.
    ///
    /// Functions come only from the named dependency modules themselves.
    /// Types come from each dependency's resolved export surface, so an
    /// explicit re-export carries the original declaration (including record
    /// fields / sum constructors and opacity) without exposing unrelated
    /// transitive modules.
    pub fn from_visible_modules(
        modules: &[(String, Vec<TopLevel>)],
        visible_modules: &[String],
        type_exports: &ModuleTypeExports,
    ) -> Self {
        let visible: HashSet<&str> = visible_modules.iter().map(String::as_str).collect();
        let mut entries = Vec::new();

        // Public functions are never pulled transitively. Type re-exports are
        // handled separately below because their declaring module differs from
        // the module whose `exposes` list made them visible.
        for (module_name, items) in modules {
            if !visible.contains(module_name.as_str()) {
                continue;
            }
            let exports = collect_module_exports(items);
            let functions_only = ModuleExports {
                functions: exports.functions,
                types: Vec::new(),
            };
            Self::collect_from_exports(module_name, &functions_only, &mut entries);
        }

        // The same declaration may be reachable through two facades. Emit its
        // metadata once; if any visible path exposes the representation, the
        // importer may use that public path and the aggregate is non-opaque.
        let mut targets: HashMap<(String, String), bool> = HashMap::new();
        for visible_module in visible_modules {
            let Some(exports) = type_exports.get(visible_module) else {
                continue;
            };
            for target in exports.values() {
                targets
                    .entry((target.module.clone(), target.name.clone()))
                    .and_modify(|is_opaque| *is_opaque &= target.is_opaque)
                    .or_insert(target.is_opaque);
            }
        }

        // Walk in loader/source order so registry construction stays stable.
        for (module_name, items) in modules {
            for item in items {
                let TopLevel::TypeDef(type_def) = item else {
                    continue;
                };
                let type_name = match type_def {
                    TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
                };
                let key = (module_name.clone(), type_name.clone());
                let Some(is_opaque) = targets.remove(&key) else {
                    continue;
                };
                let one_type = ModuleExports {
                    functions: Vec::new(),
                    types: vec![ExportedTypeDef {
                        def: type_def,
                        is_opaque,
                    }],
                };
                Self::collect_from_exports(module_name, &one_type, &mut entries);
            }
        }

        SymbolRegistry { entries }
    }

    /// Build a registry of ALL symbols (including private) from loaded modules.
    /// Used by codegen which emits full module implementations.
    pub fn from_modules_all(modules: &[(String, Vec<TopLevel>)]) -> Self {
        let mut entries = Vec::new();
        for (module_name, items) in modules {
            let all = collect_all_module_symbols(items);
            Self::collect_from_exports(module_name, &all, &mut entries);
        }
        SymbolRegistry { entries }
    }

    fn collect_from_exports(
        module_name: &str,
        exports: &ModuleExports<'_>,
        entries: &mut Vec<SymbolEntry>,
    ) {
        for fd in &exports.functions {
            let id = entries.len() as u32;
            entries.push(SymbolEntry {
                id,
                canonical_name: qualified_name(module_name, &fd.name),
                alias: None,
                module: module_name.to_string(),
                kind: SymbolKind::Function {
                    name: fd.name.clone(),
                    params: fd.params.clone(),
                    return_type: fd.return_type.clone(),
                    effects: fd.effects.iter().map(|e| e.node.clone()).collect(),
                },
            });
        }

        for et in &exports.types {
            match et.def {
                TypeDef::Sum {
                    name: type_name,
                    variants,
                    ..
                } => {
                    if et.is_opaque {
                        let id = entries.len() as u32;
                        entries.push(SymbolEntry {
                            id,
                            canonical_name: qualified_name(module_name, type_name),
                            alias: Some(type_name.clone()),
                            module: module_name.to_string(),
                            kind: SymbolKind::OpaqueType {
                                name: type_name.clone(),
                            },
                        });
                    } else {
                        let id = entries.len() as u32;
                        entries.push(SymbolEntry {
                            id,
                            canonical_name: qualified_name(module_name, type_name),
                            alias: Some(type_name.clone()),
                            module: module_name.to_string(),
                            kind: SymbolKind::SumType {
                                name: type_name.clone(),
                                variants: variants.iter().map(|v| v.name.clone()).collect(),
                            },
                        });
                        Self::collect_variant_entries(
                            module_name,
                            type_name.as_str(),
                            variants,
                            entries,
                        );
                    }
                }
                TypeDef::Product {
                    name: type_name,
                    fields,
                    ..
                } => {
                    if et.is_opaque {
                        let id = entries.len() as u32;
                        entries.push(SymbolEntry {
                            id,
                            canonical_name: qualified_name(module_name, type_name),
                            alias: Some(type_name.clone()),
                            module: module_name.to_string(),
                            kind: SymbolKind::OpaqueType {
                                name: type_name.clone(),
                            },
                        });
                    } else {
                        // Record types also get a top-level entry alongside their
                        // per-field entries. Pre-A3 they were implicit (only the
                        // RecordField entries existed), which left no SymbolEntry
                        // for `Type::Named("MyRecord")` to resolve against and
                        // forced every backend to special-case the bare name.
                        let id = entries.len() as u32;
                        entries.push(SymbolEntry {
                            id,
                            canonical_name: qualified_name(module_name, type_name),
                            alias: Some(type_name.clone()),
                            module: module_name.to_string(),
                            kind: SymbolKind::ProductType {
                                name: type_name.clone(),
                                fields: fields.iter().map(|(n, _)| n.clone()).collect(),
                            },
                        });
                        for (field_name, ty_str) in fields {
                            let id = entries.len() as u32;
                            let canonical =
                                qualified_member_key(module_name, type_name, field_name);
                            let alias = member_key(type_name, field_name);
                            entries.push(SymbolEntry {
                                id,
                                canonical_name: canonical,
                                alias: Some(alias),
                                module: module_name.to_string(),
                                kind: SymbolKind::RecordField {
                                    type_name: type_name.clone(),
                                    field_name: field_name.clone(),
                                    field_type: ty_str.clone(),
                                },
                            });
                        }
                    }
                }
            }
        }
    }

    fn collect_variant_entries(
        module_name: &str,
        type_name: &str,
        variants: &[TypeVariant],
        entries: &mut Vec<SymbolEntry>,
    ) {
        for variant in variants {
            let id = entries.len() as u32;
            let canonical = qualified_member_key(module_name, type_name, &variant.name);
            let alias = member_key(type_name, &variant.name);
            entries.push(SymbolEntry {
                id,
                canonical_name: canonical,
                alias: Some(alias),
                module: module_name.to_string(),
                kind: SymbolKind::Constructor {
                    type_name: type_name.to_string(),
                    variant_name: variant.name.clone(),
                    field_types: variant.fields.clone(),
                },
            });
        }
    }
}
