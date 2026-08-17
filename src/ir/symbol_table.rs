//! Resolved-identity layer for the compiler.
//!
//! Issue #138 phase E. Stable opaque IDs (`FnId`, `TypeId`,
//! `CtorId`, `ModuleId`) for every named declaration in the
//! program, paired with bidirectional lookup against the typed
//! identity keys from [`crate::ir::identity`].
//!
//! ## Why
//!
//! Six rounds of reviewer audit (PRs #130-#136) and the typed
//! `FnKey` / `TypeKey` bridge (#137, #139) closed the bug class
//! *for the IR maps that already cross the boundary*. The cause
//! underneath was a missing pass: name resolution. Today every
//! consumer (typechecker, proof lowering, codegen, opaque checks,
//! recursion analysis) re-derives identity from `fd.name` /
//! `td.name` at each lookup. The resolver is implicit and
//! duplicated; the bug class is "two implicit resolvers
//! disagree".
//!
//! The fix is to make resolution **explicit and one-shot**: after
//! module load, build a [`SymbolTable`] once. Every subsequent
//! pass takes a `FnId` / `TypeId` and never re-resolves a string.
//! Backend mangling (`FnId → "Module.fn"` / `FnId →
//! "Aver_Module.fn"`) is a single function applied at emit time.
//!
//! ## Foundation, not migration
//!
//! This module is the **foundation only**. It introduces the
//! types and the build function. No consumer is migrated yet —
//! `ProofIR.fn_contracts` stays keyed by `FnKey`, `Type::Named`
//! stays `String`-bodied, backends keep their bare-string
//! lookups. Migration is staged across follow-up PRs (#138
//! tracks the phases).
//!
//! Today's foundation lets a downstream pass say
//! `symbols.fn_id_of(&fn_key)` and get a stable opaque handle
//! back, without disturbing anything that already works.

use std::collections::HashMap;

use crate::ast::{CapabilityItem, FnDef, TopLevel, TypeDef, TypeVariant};
use crate::capability::{OracleDimension, ReplaySemantics};
use crate::codegen::ModuleInfo;
use crate::ir::identity::{BuiltinId, CtorId, FnId, FnKey, ModuleId, TypeId, TypeKey};

/// One entry in the function table. The `key` field is the public
/// canonical handle; `module` is the owning scope; `index_in_module`
/// is the position of this fn in its scope's source-order fn list
/// (useful for stable display ordering).
#[derive(Debug, Clone)]
pub struct FnEntry {
    pub key: FnKey,
    pub module: ModuleId,
    pub index_in_module: u32,
}

/// One entry in the type table.
#[derive(Debug, Clone)]
pub struct TypeEntry {
    pub key: TypeKey,
    pub module: ModuleId,
    pub index_in_module: u32,
    /// The variants list of a sum type, in source order. Empty
    /// for record (product) types — those use a single implicit
    /// constructor whose `CtorId` is registered separately.
    pub variants: Vec<CtorId>,
    /// `true` when the type is a product (record). The record has
    /// one [`CtorId`] entry in the [`SymbolTable::ctors`] table
    /// even though it has no source-level variant list; this lets
    /// emit code uniformly route record creation through the
    /// constructor path.
    pub is_product: bool,
    /// Representation-less type minted only by a capability provider.
    pub is_capability_opaque: bool,
}

/// One entry in the constructor table. A constructor belongs to
/// a single type; sum-type variants get one entry each, product
/// (record) types get one entry total.
#[derive(Debug, Clone)]
pub struct CtorEntry {
    pub owning_type: TypeId,
    /// Source name of the variant (`"Ok"`, `"Err"`, `"Some"`,
    /// `"None"`, `"Circle"`, …). For record types this is the
    /// type's own name (records share their type's name as the
    /// constructor name in source syntax: `User { ... }`).
    pub name: String,
}

/// One entry in the module table. `ModuleId(0)` is always the
/// synthetic entry scope; subsequent IDs correspond to
/// `inputs.dep_modules` in walk order.
#[derive(Debug, Clone)]
pub struct ModuleEntry {
    /// `None` for the entry scope; `Some(prefix)` for dep modules.
    pub prefix: Option<String>,
}

/// Runtime-relevant part of a provider-bound declaration. The canonical
/// contract registry performs validation; this copy lets every backend route
/// the already-validated operation without re-parsing source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CapabilityOperationInfo {
    pub effectful: bool,
    pub oracle: Option<OracleDimension>,
    pub replay: Option<ReplaySemantics>,
    pub mints_resource: bool,
}

/// Resolved-identity table for an Aver program. Built once after
/// module load + before typecheck (eventually — today no caller
/// invokes this yet); consumers thereafter look up IDs and never
/// re-resolve names.
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub modules: Vec<ModuleEntry>,
    pub fns: Vec<FnEntry>,
    pub types: Vec<TypeEntry>,
    pub ctors: Vec<CtorEntry>,
    /// Phase 6 wave 11 — interned built-in function names. Grows
    /// lazily as `lower_program` encounters new
    /// `ResolvedCallee::Builtin(name)` shapes via
    /// `SymbolTable::intern_builtin`. Re-interning the same name
    /// returns the same `BuiltinId` so MIR's call sites end up
    /// with stable identity per program.
    pub builtins: Vec<BuiltinEntry>,

    fn_index: HashMap<FnKey, FnId>,
    type_index: HashMap<TypeKey, TypeId>,
    /// Per-type lookup of `variant_name → CtorId`. The outer
    /// `TypeId` plus inner `String` lets two unrelated sum types
    /// share a variant name (`Result.Ok` vs a user's
    /// `Validation.Ok`) without collision.
    ctor_index: HashMap<(TypeId, String), CtorId>,
    /// Builtin canonical name → `BuiltinId` (Phase 6 wave 11).
    builtin_index: HashMap<String, BuiltinId>,
    /// Canonical provider-bound operation name → whether the call establishes
    /// an effect. These have no `FnId` because there is no Aver body.
    capability_operations: HashMap<String, CapabilityOperationInfo>,
    /// Canonical capability module → checked contract/model identities. This is
    /// populated from the same `CapabilityRegistry` as type checking and is
    /// copied into VM bytecode so installing a registry from another program
    /// cannot silently change a compiled operation's boundary or replay model.
    capability_contract_hashes: HashMap<String, (String, String)>,
    /// Recognized literal-dischargeable smart constructors — see
    /// [`SymbolTable::literal_refinements`].
    literal_refinements: crate::analysis::literal_refinement::LiteralRefinementTable,
}

/// Phase 6 wave 11 — interned record for a built-in fn name.
/// Carries the canonical name so VM / Rust / wasm-gc backends
/// can look it up through their existing string-keyed builtin
/// registries; MIR-internal consumers route through `BuiltinId`
/// instead and never re-parse the string.
#[derive(Debug, Clone)]
pub struct BuiltinEntry {
    pub name: String,
}

impl SymbolTable {
    /// Build a `SymbolTable` from entry items + dep modules. The
    /// build order is fully deterministic: modules in
    /// `dep_modules` walk order with the entry scope prepended,
    /// fns in source order within each module, types in source
    /// order within each module, ctors in variant-source order
    /// within each type.
    pub fn build(entry_items: &[TopLevel], dep_modules: &[ModuleInfo]) -> Self {
        let mut table = SymbolTable::default();

        // ModuleId(0) is always the entry scope.
        table.modules.push(ModuleEntry { prefix: None });
        for m in dep_modules {
            table.modules.push(ModuleEntry {
                prefix: Some(m.prefix.clone()),
            });
        }

        // Helpful pre-traversal: gather (scope, fns, types) tuples
        // so the actual indexing pass is uniform across entry vs
        // module scopes.
        let entry_fns: Vec<&FnDef> = entry_items
            .iter()
            .filter_map(|i| match i {
                TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let entry_types: Vec<&TypeDef> = entry_items
            .iter()
            .filter_map(|i| match i {
                TopLevel::TypeDef(td) => Some(td),
                _ => None,
            })
            .collect();
        let entry_capability_items: Vec<&CapabilityItem> = entry_items
            .iter()
            .filter_map(|i| match i {
                TopLevel::Capability(item) => Some(item),
                _ => None,
            })
            .collect();
        let entry_semantics = entry_items.iter().find_map(|i| match i {
            TopLevel::Module(module) => module.semantics.as_deref(),
            _ => None,
        });
        let entry_scope = entry_items.iter().find_map(|i| match i {
            TopLevel::Module(module) => Some(module.name.as_str()),
            _ => None,
        });

        type ScopeWalk<'a> = (
            ModuleId,
            Option<&'a str>,
            Vec<&'a FnDef>,
            Vec<&'a TypeDef>,
            Vec<&'a CapabilityItem>,
            Option<&'a str>,
            Option<&'a str>,
        );
        let scopes: Vec<ScopeWalk> = std::iter::once((
            ModuleId::ENTRY,
            None,
            entry_fns,
            entry_types,
            entry_capability_items,
            entry_semantics,
            entry_scope,
        ))
        .chain(dep_modules.iter().enumerate().map(|(i, m)| {
            let module_id = ModuleId((i + 1) as u32);
            let fns: Vec<&FnDef> = m.fn_defs.iter().collect();
            let types: Vec<&TypeDef> = m.type_defs.iter().collect();
            let capability_items: Vec<&CapabilityItem> = m.capability_items.iter().collect();
            (
                module_id,
                Some(m.prefix.as_str()),
                fns,
                types,
                capability_items,
                m.capability_semantics.as_deref(),
                Some(m.prefix.as_str()),
            )
        }))
        .collect();

        for (module_id, prefix, fns, types, capability_items, semantics, capability_scope) in scopes
        {
            for (index_in_module, fd) in fns.into_iter().enumerate() {
                let key = match prefix {
                    Some(p) => FnKey::in_module(p.to_string(), fd.name.clone()),
                    None => FnKey::entry(fd.name.clone()),
                };
                let id = FnId(table.fns.len() as u32);
                table.fn_index.insert(key.clone(), id);
                table.fns.push(FnEntry {
                    key,
                    module: module_id,
                    index_in_module: index_in_module as u32,
                });
            }
            for (index_in_module, td) in types.into_iter().enumerate() {
                let (type_name, variants, is_product) = match td {
                    TypeDef::Sum { name, variants, .. } => (name.clone(), variants.clone(), false),
                    TypeDef::Product { name, .. } => (name.clone(), Vec::new(), true),
                };
                let key = match prefix {
                    Some(p) => TypeKey::in_module(p.to_string(), type_name.clone()),
                    None => TypeKey::entry(type_name.clone()),
                };
                let type_id = TypeId(table.types.len() as u32);
                table.type_index.insert(key.clone(), type_id);
                // Register variants (sum) or single constructor
                // (product) into the ctor table.
                let ctor_ids: Vec<CtorId> = if is_product {
                    let cid = CtorId(table.ctors.len() as u32);
                    table.ctors.push(CtorEntry {
                        owning_type: type_id,
                        name: type_name.clone(),
                    });
                    table.ctor_index.insert((type_id, type_name.clone()), cid);
                    vec![cid]
                } else {
                    let mut ids = Vec::with_capacity(variants.len());
                    for v in &variants {
                        let cid = CtorId(table.ctors.len() as u32);
                        table.ctors.push(CtorEntry {
                            owning_type: type_id,
                            name: v.name.clone(),
                        });
                        table.ctor_index.insert((type_id, v.name.clone()), cid);
                        ids.push(cid);
                    }
                    ids
                };
                table.types.push(TypeEntry {
                    key,
                    module: module_id,
                    index_in_module: index_in_module as u32,
                    variants: ctor_ids,
                    is_product,
                    is_capability_opaque: false,
                });
            }
            let capability_opaque_names = capability_items
                .iter()
                .filter_map(|item| match item {
                    CapabilityItem::Opaque { name, .. } => Some(name.as_str()),
                    CapabilityItem::Operation(_) => None,
                })
                .collect::<std::collections::HashSet<_>>();
            for item in capability_items {
                match item {
                    CapabilityItem::Opaque { name, .. } => {
                        let key = match prefix {
                            Some(p) => TypeKey::in_module(p.to_string(), name.clone()),
                            None => TypeKey::entry(name.clone()),
                        };
                        if table.type_index.contains_key(&key) {
                            continue;
                        }
                        let type_id = TypeId(table.types.len() as u32);
                        table.type_index.insert(key.clone(), type_id);
                        table.types.push(TypeEntry {
                            key,
                            module: module_id,
                            index_in_module: table.types.len() as u32,
                            variants: Vec::new(),
                            is_product: false,
                            is_capability_opaque: true,
                        });
                    }
                    CapabilityItem::Operation(operation) => {
                        if let Some(prefix) = capability_scope {
                            let oracle = match operation.oracle.as_deref() {
                                Some("snapshot") => Some(OracleDimension::Snapshot),
                                Some("generative") => Some(OracleDimension::Generative),
                                Some("output") => Some(OracleDimension::Output),
                                Some("generativeOutput") => Some(OracleDimension::GenerativeOutput),
                                _ => None,
                            };
                            let replay = match operation.replay.as_deref() {
                                Some("recorded") => Some(ReplaySemantics::Recorded),
                                Some("reissued") => Some(ReplaySemantics::Reissued),
                                Some("suppressed") => Some(ReplaySemantics::Suppressed),
                                _ => None,
                            };
                            let mints_resource =
                                crate::types::parse_type_str_strict(&operation.return_type)
                                    .is_ok_and(|ty| {
                                        fn transparent_resource(
                                            ty: &crate::types::Type,
                                            opaque: &std::collections::HashSet<&str>,
                                        ) -> bool {
                                            match ty {
                                                crate::types::Type::Result(ok, _)
                                                | crate::types::Type::Option(ok) => {
                                                    transparent_resource(ok, opaque)
                                                }
                                                crate::types::Type::Named { name, .. } => opaque
                                                    .contains(
                                                        name.rsplit('.').next().unwrap_or(name),
                                                    ),
                                                _ => false,
                                            }
                                        }
                                        transparent_resource(&ty, &capability_opaque_names)
                                    });
                            table.capability_operations.insert(
                                format!("{prefix}.{}", operation.name),
                                CapabilityOperationInfo {
                                    effectful: semantics == Some("effectful"),
                                    oracle,
                                    replay,
                                    mints_resource,
                                },
                            );
                        }
                    }
                }
            }
        }

        // Standard capabilities are implicitly visible language atoms even
        // for embedders that build/resolve VM code without invoking the full
        // typechecker or module loader. Their source contracts remain the
        // authority; this only installs the already-validated identities into
        // the symbolic view used by every backend.
        table.merge_capability_registry(crate::stdlib::standard_capability_registry_ref());

        // Derived last: the recognizer resolves each refinement's
        // per-element predicate against the table that is being built,
        // so it needs every fn/type already indexed. It reads the table
        // through the ordinary resolver, whose own discharge rewrite
        // consults `literal_refinements` — still empty at this point, so
        // the derivation can never recurse into itself.
        table.literal_refinements =
            crate::analysis::literal_refinement::LiteralRefinementTable::build(
                entry_items,
                dep_modules,
                &table,
            );

        table
    }

    pub fn capability_operation(&self, canonical_name: &str) -> Option<CapabilityOperationInfo> {
        self.capability_operations.get(canonical_name).copied()
    }

    pub fn capability_operations(&self) -> impl Iterator<Item = (&str, CapabilityOperationInfo)> {
        self.capability_operations
            .iter()
            .map(|(name, info)| (name.as_str(), *info))
    }

    pub fn capability_contract_hashes(&self, module: &str) -> Option<(&str, &str)> {
        self.capability_contract_hashes
            .get(module)
            .map(|(contract, model)| (contract.as_str(), model.as_str()))
    }

    /// Add already-validated capability identities to a symbolic link view.
    /// Used for compiler-shipped implicit standards whose operation atoms must
    /// survive raw embedder builds and backend flattening alike.
    pub(crate) fn merge_capability_registry(
        &mut self,
        registry: &crate::capability::CapabilityRegistry,
    ) {
        for contract in registry.contracts() {
            self.capability_contract_hashes.insert(
                contract.module.clone(),
                (contract.contract_hash.clone(), contract.model_hash.clone()),
            );
        }
        for operation in registry.operations() {
            self.capability_operations.insert(
                operation.canonical_name.clone(),
                CapabilityOperationInfo {
                    effectful: operation.is_effectful(),
                    oracle: operation.oracle,
                    replay: operation.replay,
                    mints_resource: operation.minted_resource.is_some(),
                },
            );
        }
    }

    /// Derived gate for the literal smart-constructor discharge.
    ///
    /// Lives on the symbol table because that is the single place where
    /// entry items and dependency modules are jointly in hand, and
    /// because the coupling is exactly right: a qualified
    /// `Dep.fromList(…)` can only RESOLVE against a table that knows
    /// `Dep`, so every consumer able to resolve the call is also able to
    /// see whether it discharges. Both the typechecker and the HIR
    /// resolver read this one table, which is what keeps the checked and
    /// unchecked pipelines from forking.
    pub fn literal_refinements(
        &self,
    ) -> &crate::analysis::literal_refinement::LiteralRefinementTable {
        &self.literal_refinements
    }

    /// Resolve a `FnKey` to its `FnId`. `None` when the key
    /// doesn't name any function in the program.
    pub fn fn_id_of(&self, key: &FnKey) -> Option<FnId> {
        self.fn_index.get(key).copied()
    }

    /// Resolve a `TypeKey` to its `TypeId`.
    pub fn type_id_of(&self, key: &TypeKey) -> Option<TypeId> {
        self.type_index.get(key).copied()
    }

    /// Resolve a *bare* type name (no module prefix) against every
    /// scope in the table — entry first, then each dep module — and
    /// return the unique match. Returns `None` when the name doesn't
    /// resolve OR when two or more scopes legitimately share it
    /// (ambiguous reference; caller must qualify).
    ///
    /// Phase-E cross-module ctor resolution: an Aver expression like
    /// `Val.ValOk(x)` written from a module that doesn't host the
    /// `Val` type still needs to resolve to the right `TypeId` so the
    /// resolver can lift the call to `ResolvedCtor::User`. Pre-PR-8
    /// the rust codegen worked around the gap by matching ctors by
    /// raw string after the resolver had moved on; PR 8 made the
    /// resolved-form classification authoritative, which exposed
    /// this missing lookup. Adding it here keeps identity logic on
    /// the symbol table where the rest of identity resolution lives.
    pub fn type_id_by_bare_name(&self, name: &str) -> Option<TypeId> {
        let mut found: Option<TypeId> = None;
        for (key, id) in &self.type_index {
            if key.name == name {
                if found.is_some() {
                    // Ambiguous bare reference across scopes; the
                    // caller must qualify with a module prefix.
                    return None;
                }
                found = Some(*id);
            }
        }
        found
    }

    /// Resolve a constructor by (owning type, variant name).
    pub fn ctor_id_of(&self, owning_type: TypeId, variant: &str) -> Option<CtorId> {
        self.ctor_index
            .get(&(owning_type, variant.to_string()))
            .copied()
    }

    /// Borrow the entry for a `FnId`. Panics on an invalid `FnId`
    /// — the only way to obtain one is through this table, so an
    /// invalid `FnId` is a logic error, not a runtime input
    /// failure.
    pub fn fn_entry(&self, id: FnId) -> &FnEntry {
        &self.fns[id.0 as usize]
    }

    pub fn type_entry(&self, id: TypeId) -> &TypeEntry {
        &self.types[id.0 as usize]
    }

    pub fn ctor_entry(&self, id: CtorId) -> &CtorEntry {
        &self.ctors[id.0 as usize]
    }

    pub fn module_entry(&self, id: ModuleId) -> &ModuleEntry {
        &self.modules[id.0 as usize]
    }

    /// Phase 6 wave 11 — look up an interned built-in name. Panics
    /// the same way the other `*_entry` lookups do when the id is
    /// out of range (consumers should always hold ids minted by
    /// `intern_builtin`).
    pub fn builtin_entry(&self, id: BuiltinId) -> &BuiltinEntry {
        &self.builtins[id.0 as usize]
    }

    /// Phase 6 wave 11 — intern a built-in fn name. Returns the
    /// stable `BuiltinId` for `name`, reusing the existing slot
    /// when the name has already been interned. Callers
    /// (`lower_program` for the MIR side) get the same id for the
    /// same name across the whole program so MIR consumers can
    /// compare callees by identity.
    pub fn intern_builtin(&mut self, name: &str) -> BuiltinId {
        if let Some(id) = self.builtin_index.get(name) {
            return *id;
        }
        let id = BuiltinId(self.builtins.len() as u32);
        self.builtins.push(BuiltinEntry {
            name: name.to_string(),
        });
        self.builtin_index.insert(name.to_string(), id);
        id
    }

    /// Phase 6 wave 11 — look up a `BuiltinId` by name without
    /// interning. Returns `None` when the name hasn't been
    /// registered yet. Useful for consumers that want to detect
    /// "is this name a known builtin" without mutating state.
    pub fn builtin_id_of(&self, name: &str) -> Option<BuiltinId> {
        self.builtin_index.get(name).copied()
    }

    /// Build-time invariant check: every registered key resolves
    /// back to a `FnEntry` whose stored key equals the lookup
    /// key. Helps catch programmer errors in this module — never
    /// expected to fail at runtime in production callers.
    #[cfg(test)]
    pub(crate) fn assert_consistent(&self) {
        for (i, entry) in self.fns.iter().enumerate() {
            assert_eq!(
                self.fn_index.get(&entry.key),
                Some(&FnId(i as u32)),
                "fn_index out of sync at index {i}"
            );
        }
        for (i, entry) in self.types.iter().enumerate() {
            assert_eq!(
                self.type_index.get(&entry.key),
                Some(&TypeId(i as u32)),
                "type_index out of sync at index {i}"
            );
        }
        for (i, entry) in self.ctors.iter().enumerate() {
            assert_eq!(
                self.ctor_index
                    .get(&(entry.owning_type, entry.name.clone())),
                Some(&CtorId(i as u32)),
                "ctor_index out of sync at index {i}"
            );
        }
    }
}

#[allow(dead_code)] // suppresses warning while no production caller uses these helpers
fn _no_warning_for_unused_variant_field(_v: &TypeVariant) {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{TypeDef, TypeVariant};

    fn product(name: &str, line: usize) -> TypeDef {
        TypeDef::Product {
            name: name.to_string(),
            fields: vec![("value".to_string(), "Int".to_string())],
            line,
        }
    }

    fn sum(name: &str, variants: &[&str], line: usize) -> TypeDef {
        TypeDef::Sum {
            name: name.to_string(),
            variants: variants
                .iter()
                .map(|v| TypeVariant {
                    name: v.to_string(),
                    fields: Vec::new(),
                })
                .collect(),
            line,
        }
    }

    fn module(prefix: &str, fns: Vec<FnDef>, types: Vec<TypeDef>) -> ModuleInfo {
        ModuleInfo {
            prefix: prefix.to_string(),
            depends: Vec::new(),
            type_defs: types,
            fn_defs: fns,
            capability_items: Vec::new(),
            capability_semantics: None,
            verify_laws: Vec::new(),
            analysis: None,
        }
    }

    fn fn_def(name: &str) -> FnDef {
        use crate::ast::{FnBody, Literal, Spanned};
        FnDef {
            name: name.to_string(),
            line: 1,
            params: Vec::new(),
            return_type: "Int".to_string(),
            effects: Vec::new(),
            desc: None,
            body: std::sync::Arc::new(FnBody::from_expr(Spanned::new(
                crate::ast::Expr::Literal(Literal::Int(0)),
                1,
            ))),
            resolution: None,
        }
    }

    #[test]
    fn entry_only_program_gets_entry_module_and_indexed_fns() {
        let items = vec![
            TopLevel::FnDef(fn_def("foo")),
            TopLevel::FnDef(fn_def("bar")),
        ];
        let table = SymbolTable::build(&items, &[]);
        table.assert_consistent();

        assert_eq!(table.modules.len(), 1, "entry-only build has just ENTRY");
        assert!(table.modules[0].prefix.is_none());

        let foo_id = table.fn_id_of(&FnKey::entry("foo")).expect("foo missing");
        let bar_id = table.fn_id_of(&FnKey::entry("bar")).expect("bar missing");
        assert_ne!(foo_id, bar_id);
        assert!(table.fn_entry(foo_id).module.is_entry());
    }

    #[test]
    fn cross_module_same_bare_name_fns_get_distinct_ids() {
        // Two modules each declaring `foo`. Pre-symbol-table the
        // bare-name lookup would collapse them; the table keeps
        // distinct entries indexed by canonical `FnKey`.
        let mod_a = module("A", vec![fn_def("foo")], vec![]);
        let mod_b = module("B", vec![fn_def("foo")], vec![]);
        let table = SymbolTable::build(&[], &[mod_a, mod_b]);
        table.assert_consistent();

        let a_foo = table
            .fn_id_of(&FnKey::in_module("A", "foo"))
            .expect("A.foo missing");
        let b_foo = table
            .fn_id_of(&FnKey::in_module("B", "foo"))
            .expect("B.foo missing");
        assert_ne!(a_foo, b_foo);

        // A bare-name lookup should NOT silently resolve to one of
        // them — the table refuses ambiguity rather than guessing.
        assert!(
            table.fn_id_of(&FnKey::entry("foo")).is_none(),
            "bare-entry `foo` must miss when only module fns named `foo` exist"
        );
    }

    #[test]
    fn product_type_has_one_constructor_under_its_own_name() {
        let items = vec![TopLevel::TypeDef(product("Natural", 1))];
        let table = SymbolTable::build(&items, &[]);
        table.assert_consistent();

        let nat_id = table.type_id_of(&TypeKey::entry("Natural")).unwrap();
        let entry = table.type_entry(nat_id);
        assert!(entry.is_product);
        assert_eq!(entry.variants.len(), 1);
        let ctor_id = entry.variants[0];
        assert_eq!(table.ctor_entry(ctor_id).name, "Natural");
        assert_eq!(table.ctor_entry(ctor_id).owning_type, nat_id);
        assert_eq!(table.ctor_id_of(nat_id, "Natural"), Some(ctor_id));
    }

    #[test]
    fn sum_type_registers_each_variant_under_its_type() {
        let items = vec![TopLevel::TypeDef(sum("Shape", &["Circle", "Square"], 1))];
        let table = SymbolTable::build(&items, &[]);
        table.assert_consistent();

        let shape_id = table.type_id_of(&TypeKey::entry("Shape")).unwrap();
        let entry = table.type_entry(shape_id);
        assert!(!entry.is_product);
        assert_eq!(entry.variants.len(), 2);
        let circle = table.ctor_id_of(shape_id, "Circle").unwrap();
        let square = table.ctor_id_of(shape_id, "Square").unwrap();
        assert_ne!(circle, square);
        assert_eq!(table.ctor_entry(circle).name, "Circle");
        assert_eq!(table.ctor_entry(square).name, "Square");
    }

    #[test]
    fn variant_name_collision_across_unrelated_sums_is_resolved_per_type() {
        // Two sum types each with an `Ok` variant — distinct
        // `CtorId`s because owning_type differs. Mirror of the
        // real-world Result.Ok vs user's `Validation.Ok` case
        // the symbol table needs to handle once consumers
        // migrate.
        let items = vec![
            TopLevel::TypeDef(sum("Validation", &["Ok", "Invalid"], 1)),
            TopLevel::TypeDef(sum("Status", &["Ok", "Failed"], 5)),
        ];
        let table = SymbolTable::build(&items, &[]);
        table.assert_consistent();

        let v_id = table.type_id_of(&TypeKey::entry("Validation")).unwrap();
        let s_id = table.type_id_of(&TypeKey::entry("Status")).unwrap();
        let v_ok = table.ctor_id_of(v_id, "Ok").unwrap();
        let s_ok = table.ctor_id_of(s_id, "Ok").unwrap();
        assert_ne!(
            v_ok, s_ok,
            "same-bare-name variants across unrelated sums must be distinct"
        );
    }

    #[test]
    fn deterministic_indexing_across_repeated_builds() {
        // Two identical inputs produce identical ID assignments.
        // Property the `_for_test` consumers downstream rely on
        // when comparing two builds.
        let items_a = vec![
            TopLevel::FnDef(fn_def("a")),
            TopLevel::FnDef(fn_def("b")),
            TopLevel::TypeDef(product("R", 1)),
        ];
        let items_b = items_a.clone();
        let t1 = SymbolTable::build(&items_a, &[]);
        let t2 = SymbolTable::build(&items_b, &[]);
        assert_eq!(
            t1.fn_id_of(&FnKey::entry("a")),
            t2.fn_id_of(&FnKey::entry("a"))
        );
        assert_eq!(
            t1.type_id_of(&TypeKey::entry("R")),
            t2.type_id_of(&TypeKey::entry("R"))
        );
    }

    #[test]
    fn entry_scope_is_always_module_id_zero() {
        // Doc invariant — `ModuleId::ENTRY` is the entry scope's
        // ID regardless of how many dep modules a project has.
        let mod_a = module("A", vec![fn_def("x")], vec![]);
        let mod_b = module("B", vec![fn_def("y")], vec![]);
        let table = SymbolTable::build(&[TopLevel::FnDef(fn_def("entry_fn"))], &[mod_a, mod_b]);
        table.assert_consistent();
        let entry_fn = table.fn_id_of(&FnKey::entry("entry_fn")).unwrap();
        assert_eq!(table.fn_entry(entry_fn).module, ModuleId::ENTRY);
        assert_eq!(table.module_entry(ModuleId::ENTRY).prefix, None);
    }

    #[test]
    fn type_id_by_bare_name_resolves_unique_cross_module_type() {
        // Phase-E PR 9.4: a callsite in module `A` referring to type
        // `Val` declared in module `B` (no qualifier in source) gets
        // the right `TypeId` via this fallback. The rule is: "bare
        // type name from any scope resolves iff exactly one scope
        // declares it".
        let mod_a = module("A", vec![], vec![sum("Color", &["Red", "Blue"], 1)]);
        let mod_b = module("B", vec![], vec![product("Val", 1)]);
        let table = SymbolTable::build(&[], &[mod_a, mod_b]);
        table.assert_consistent();

        let val_id = table.type_id_by_bare_name("Val");
        let qualified = table.type_id_of(&TypeKey::in_module("B", "Val"));
        assert_eq!(val_id, qualified, "bare `Val` resolves to B.Val");
        assert!(val_id.is_some());

        let color_id = table.type_id_by_bare_name("Color");
        assert_eq!(
            color_id,
            table.type_id_of(&TypeKey::in_module("A", "Color"))
        );
    }

    #[test]
    fn type_id_by_bare_name_returns_none_on_ambiguity() {
        // Two modules each declaring a type called `T` — bare
        // reference is ambiguous, caller must qualify. The cross-
        // module ctor-classification path falls back to
        // `Unresolved` here, which the typechecker then flags as a
        // user-facing error (or the caller already qualified, in
        // which case the higher-priority `TypeKey::in_module`
        // lookup succeeded before this method ran).
        let mod_a = module("A", vec![], vec![product("T", 1)]);
        let mod_b = module("B", vec![], vec![product("T", 1)]);
        let table = SymbolTable::build(&[], &[mod_a, mod_b]);
        table.assert_consistent();

        assert_eq!(table.type_id_by_bare_name("T"), None);

        // Qualified lookups still resolve.
        assert!(table.type_id_of(&TypeKey::in_module("A", "T")).is_some());
        assert!(table.type_id_of(&TypeKey::in_module("B", "T")).is_some());
    }

    #[test]
    fn type_id_by_bare_name_finds_entry_scope_type() {
        // Bare lookup also reaches the entry scope when only the
        // entry declares the type — same uniqueness rule.
        let entry_type = product("Cfg", 1);
        let mod_a = module("A", vec![], vec![sum("Other", &["X"], 1)]);
        let table = SymbolTable::build(&[TopLevel::TypeDef(entry_type)], &[mod_a]);
        table.assert_consistent();

        let cfg = table.type_id_by_bare_name("Cfg");
        assert_eq!(cfg, table.type_id_of(&TypeKey::entry("Cfg")));
        assert!(cfg.is_some());
    }

    #[test]
    fn type_id_by_bare_name_missing_name_is_none() {
        let mod_a = module("A", vec![], vec![product("X", 1)]);
        let table = SymbolTable::build(&[], &[mod_a]);
        table.assert_consistent();
        assert_eq!(table.type_id_by_bare_name("NotATypeAnywhere"), None);
    }
}
