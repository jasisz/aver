//! Explicit type re-export resolution.
//!
//! This module computes names at module boundaries while preserving the
//! declaration owner's nominal identity. Symbol metadata materialisation stays
//! in the parent `visibility` module.

use std::collections::{BTreeMap, HashMap};

use crate::ast::{TopLevel, TypeDef};

use super::{declared_exposes, is_exposed, module_decl};

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

/// The header facts one module's public type surface is computed from.
///
/// Two layers need the same answer from the same inputs: the type checker,
/// which holds parsed `TopLevel` items, and the symbol table, which holds the
/// `ModuleInfo` projection of them. Both describe a module this way and call
/// [`collect_type_exports`], so "which types does this module export?" has one
/// implementation rather than two that can drift apart.
pub struct ModuleTypeSurface<'a> {
    /// The path importers spell in `depends [...]` (`Domain.User`).
    pub name: &'a str,
    /// This module's own `depends [...]`, in source order.
    pub depends: &'a [String],
    /// The `exposes [...]` list, or `None` for the default rule (everything
    /// that does not start with `_`). See [`declared_exposes`].
    pub exposes: Option<&'a [String]>,
    /// The `exposes opaque [...]` list.
    pub exposes_opaque: &'a [String],
    /// Names of the types this module declares itself, in source order.
    pub declared_types: Vec<&'a str>,
}

impl<'a> ModuleTypeSurface<'a> {
    /// Describe a module from its parsed items.
    pub fn from_items(name: &'a str, items: &'a [TopLevel]) -> Self {
        let decl = module_decl(items);
        Self {
            name,
            depends: decl.map(|m| m.depends.as_slice()).unwrap_or(&[]),
            exposes: decl.and_then(|m| declared_exposes(&m.exposes)),
            exposes_opaque: decl.map(|m| m.exposes_opaque.as_slice()).unwrap_or(&[]),
            declared_types: items
                .iter()
                .filter_map(|item| match item {
                    TopLevel::TypeDef(TypeDef::Sum { name, .. })
                    | TopLevel::TypeDef(TypeDef::Product { name, .. }) => Some(name.as_str()),
                    _ => None,
                })
                .collect(),
        }
    }
}

/// Resolve the type surface exported by every module in a loaded tree.
pub fn collect_module_type_exports(modules: &[(String, Vec<TopLevel>)]) -> ModuleTypeExports {
    let surfaces: Vec<ModuleTypeSurface<'_>> = modules
        .iter()
        .map(|(name, items)| ModuleTypeSurface::from_items(name, items))
        .collect();
    collect_type_exports(&surfaces)
}

/// Resolve the type surface exported by every module described in `surfaces`.
///
/// Default visibility exports only local non-underscore declarations. A type
/// from a dependency crosses another module boundary only when that module
/// names it in an explicit `exposes [...]` (or `exposes opaque [...]`) list.
/// Re-export chains retain the `module + name` of the original declaration,
/// and they compose: if `B` re-exposes `A.Fraction` and `C` re-exposes it in
/// turn, `C`'s importers see `A.Fraction`. Each hop must name it — a module
/// that merely depends on a re-exporter without re-exposing the name does not
/// pass it on.
pub fn collect_type_exports(surfaces: &[ModuleTypeSurface<'_>]) -> ModuleTypeExports {
    let mut exports = ModuleTypeExports::new();

    // Seed each surface with its locally declared public types.
    for surface in surfaces {
        let mut exported = HashMap::new();
        for name in &surface.declared_types {
            let is_opaque = surface.exposes_opaque.iter().any(|o| o == name);
            if is_exposed(name, surface.exposes) || is_opaque {
                exported.insert(
                    (*name).to_string(),
                    ExportedTypeTarget {
                        module: surface.name.to_string(),
                        name: (*name).to_string(),
                        is_opaque,
                    },
                );
            }
        }
        exports.insert(surface.name.to_string(), exported);
    }

    // The loader returns dependencies before importers, so one pass normally
    // suffices. Iterate to a fixed point as a cheap guard for virtual/preloaded
    // callers that provide an equivalent acyclic tree in another order — and
    // because that is what makes a re-export chain compose.
    loop {
        let mut additions = Vec::new();
        for surface in surfaces {
            let current = exports.get(surface.name);

            // `true` means this boundary explicitly narrows the re-export to
            // opaque. If both public and opaque lists mention the same name,
            // the restrictive form wins.
            let mut requested = BTreeMap::new();
            for name in surface.exposes.unwrap_or(&[]) {
                requested.entry(name.clone()).or_insert(false);
            }
            for name in surface.exposes_opaque {
                requested.insert(name.clone(), true);
            }

            for (name, force_opaque) in requested {
                if current.is_some_and(|exports| exports.contains_key(&name)) {
                    continue;
                }

                let mut identity: Option<(String, String)> = None;
                let mut ambiguous = false;
                let mut opaque_on_every_path = true;
                for dependency in surface.depends {
                    let Some(target) = exports
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
                        surface.name.to_string(),
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
            changed |= exports
                .entry(module)
                .or_default()
                .insert(alias, target)
                .is_none();
        }
        if !changed {
            break;
        }
    }

    exports
}
