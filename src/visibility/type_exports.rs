//! Explicit type re-export resolution.
//!
//! This module computes names at module boundaries while preserving the
//! declaration owner's nominal identity. Symbol metadata materialisation stays
//! in the parent `visibility` module.

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::ast::{TopLevel, TypeDef};

use super::{effective_exposes, is_exposed, module_decl};

/// The declaration behind a type name exported by a module.
///
/// For an ordinary local export, `module` is the exporter itself. For an
/// explicit re-export, it remains the module that owns the declaration, so
/// every facade preserves the original nominal identity instead of inventing
/// a fresh `Facade.Type`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportedTypeTarget {
    pub module: String,
    pub name: String,
    pub is_opaque: bool,
}

/// Per-module public type names after resolving explicit re-exports.
pub type ModuleTypeExports = HashMap<String, HashMap<String, ExportedTypeTarget>>;

/// Resolve the type surface exported by every module in a loaded tree.
///
/// Default visibility exports only local non-underscore declarations. A type
/// from a dependency crosses another module boundary only when that module
/// names it in an explicit `exposes [...]` (or `exposes opaque [...]`) list.
/// Re-export chains retain the `module + name` of the original declaration.
pub fn collect_module_type_exports(modules: &[(String, Vec<TopLevel>)]) -> ModuleTypeExports {
    let mut surfaces = ModuleTypeExports::new();

    // Seed each surface with its locally declared public types.
    for (module_name, items) in modules {
        let mut exported = HashMap::new();
        let exposes = effective_exposes(items);
        let opaque_names: HashSet<&str> = module_decl(items)
            .map(|module| module.exposes_opaque.iter().map(String::as_str).collect())
            .unwrap_or_default();
        for item in items {
            let TopLevel::TypeDef(type_def) = item else {
                continue;
            };
            let name = match type_def {
                TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
            };
            let is_opaque = opaque_names.contains(name.as_str());
            if is_exposed(name, exposes) || is_opaque {
                exported.insert(
                    name.clone(),
                    ExportedTypeTarget {
                        module: module_name.clone(),
                        name: name.clone(),
                        is_opaque,
                    },
                );
            }
        }
        surfaces.insert(module_name.clone(), exported);
    }

    // The loader returns dependencies before importers, so one pass normally
    // suffices. Iterate to a fixed point as a cheap guard for virtual/preloaded
    // callers that provide an equivalent acyclic tree in another order.
    loop {
        let mut additions = Vec::new();
        for (module_name, items) in modules {
            let Some(decl) = module_decl(items) else {
                continue;
            };
            let current = surfaces.get(module_name);

            // `true` means this boundary explicitly narrows the re-export to
            // opaque. If both public and opaque lists mention the same name,
            // the restrictive form wins.
            let mut requested = BTreeMap::new();
            for name in &decl.exposes {
                requested.entry(name.clone()).or_insert(false);
            }
            for name in &decl.exposes_opaque {
                requested.insert(name.clone(), true);
            }

            for (name, force_opaque) in requested {
                if current.is_some_and(|exports| exports.contains_key(&name)) {
                    continue;
                }

                let mut identity: Option<(String, String)> = None;
                let mut ambiguous = false;
                let mut opaque_on_every_path = true;
                for dependency in &decl.depends {
                    let Some(target) = surfaces
                        .get(dependency)
                        .and_then(|exports| exports.get(&name))
                    else {
                        continue;
                    };
                    let next_identity = (target.module.clone(), target.name.clone());
                    match &identity {
                        None => identity = Some(next_identity),
                        Some(existing) if existing == &next_identity => {}
                        Some(_) => {
                            ambiguous = true;
                            break;
                        }
                    }
                    opaque_on_every_path &= target.is_opaque;
                }

                if !ambiguous && let Some((target_module, target_name)) = identity {
                    additions.push((
                        module_name.clone(),
                        name,
                        ExportedTypeTarget {
                            module: target_module,
                            name: target_name,
                            is_opaque: force_opaque || opaque_on_every_path,
                        },
                    ));
                }
            }
        }

        if additions.is_empty() {
            break;
        }
        let mut changed = false;
        for (module, alias, target) in additions {
            changed |= surfaces
                .entry(module)
                .or_default()
                .insert(alias, target)
                .is_none();
        }
        if !changed {
            break;
        }
    }

    surfaces
}
