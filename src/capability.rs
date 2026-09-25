//! Canonical contracts for externally provided capability modules.
//!
//! The source declaration deliberately contains no provider binding. This
//! module turns it into two stable identities:
//!
//! - `contract_hash`: callable ABI + reachable boundary types;
//! - `model_hash`: the contract plus oracle/replay declarations and the
//!   transitive source closure of every hostile profile.
//!
//! Runtime target bindings are intentionally absent from both hashes. Moving
//! the same provider from an in-process VM adapter to WIT must not change what
//! the Aver program requires or what its proof assumes.

use std::collections::{BTreeMap, BTreeSet};

use sha2::{Digest, Sha256};

use crate::ast::{CapabilityItem, Expr, FnDef, Module, Stmt, TopLevel, Type, TypeDef};

pub mod answer;
mod descriptor;
#[cfg(test)]
mod tests;
mod validation;
pub mod work;

use descriptor::{
    hash_descriptor, reachable_type_defs, render_contract_descriptor, render_model_descriptor,
};
use validation::{
    resource_tainted_type_names, type_def_name, validate_boundary_type_ownership,
    validate_hostile_profiles, validate_operation_boundaries, validate_resource_map_keys,
};

/// A type name as one module writes it, qualified to the module that
/// declares it: `State` inside `Ledger` is `Ledger.State` everywhere else.
pub fn canonicalize_type_names(ty: Type, scope: &str) -> Type {
    validation::canonicalize_type_names(ty, scope)
}

/// Whether a bare type name belongs to the compiler rather than to any
/// module of the program: `Bytes` written inside `Ledger` is `Bytes`, never
/// `Ledger.Bytes`.
pub fn is_compiler_shipped_type_name(name: &str) -> bool {
    validation::is_compiler_shipped_type_name(name)
}

/// What the rest of the program declares, as a capability contract needs to
/// see it: every module's plain data types under their canonical name, and
/// every capability resource name the program mints.
///
/// A job kind — a capability of Work shape — may name a data type from a
/// module it depends on, and that type's layout enters its `contract_hash`.
/// Building the contract therefore needs the layouts, and refusing a type
/// that hides a resource needs the resource names. An ordinary capability
/// reads neither: it stays closed on its own declarations.
#[derive(Debug, Clone, Default)]
pub struct DependencyTypes {
    types: BTreeMap<String, TypeDef>,
    resources: BTreeSet<String>,
    depends: BTreeMap<String, Vec<String>>,
    exposes: BTreeMap<String, Vec<String>>,
}

impl DependencyTypes {
    /// Record one parsed module under the canonical path its dependants use.
    pub fn add_module(&mut self, scope: &str, items: &[TopLevel]) {
        for item in items {
            match item {
                TopLevel::TypeDef(td) => {
                    self.types
                        .insert(format!("{scope}.{}", type_def_name(td)), td.clone());
                }
                TopLevel::Capability(CapabilityItem::Resource { name, .. }) => {
                    self.resources.insert(format!("{scope}.{name}"));
                }
                TopLevel::Module(module) => {
                    self.depends
                        .insert(scope.to_string(), module.depends.clone());
                    self.exposes
                        .insert(scope.to_string(), module.exposes.clone());
                }
                _ => {}
            }
        }
    }

    /// The canonical name of the type `spelling` refers to inside `owner`.
    ///
    /// A module may write an imported type bare, so a bare name is the
    /// module's own declaration when it has one and otherwise the one
    /// declaration of that name among the modules it depends on. A name that
    /// two dependencies both declare resolves to neither: the layout would be
    /// ambiguous, and an ambiguous layout is not one a hash may bind.
    pub fn resolve(&self, owner: &str, spelling: &str) -> Option<String> {
        if spelling.contains('.') {
            return Some(spelling.to_string());
        }
        let own = format!("{owner}.{spelling}");
        if self.types.contains_key(&own) || self.resources.contains(&own) {
            return Some(own);
        }
        let mut found = None;
        for dependency in self.depends.get(owner).into_iter().flatten() {
            let candidate = format!("{dependency}.{spelling}");
            if self.types.contains_key(&candidate) || self.resources.contains(&candidate) {
                if found.is_some() {
                    return None;
                }
                found = Some(candidate);
            }
        }
        found.or(Some(own))
    }

    /// The layout of one type, addressed the way every module but its own
    /// writes it.
    pub fn type_def(&self, canonical_name: &str) -> Option<&TypeDef> {
        self.types.get(canonical_name)
    }

    /// Whether module `owner` offers the declaration `bare_name` to the
    /// modules that depend on it, by the one rule the rest of the compiler
    /// uses: an explicit `exposes` list names everything it offers, and no
    /// list at all offers everything that does not start with `_`.
    ///
    /// A module this table never saw answers yes: it is not the place that
    /// decides whether the program is complete.
    pub fn exposes_type(&self, owner: &str, bare_name: &str) -> bool {
        match self.exposes.get(owner) {
            Some(exposes) => crate::visibility::is_exposed(
                bare_name,
                crate::visibility::declared_exposes(exposes),
            ),
            None => true,
        }
    }

    /// Whether `canonical_name` is a capability resource: a handle the
    /// provider owns, with no layout a contract could bind. The compiler
    /// ships some of these itself, and a program's own capability modules
    /// declare the rest.
    pub fn is_resource(&self, canonical_name: &str) -> bool {
        self.resources.contains(canonical_name)
            || crate::stdlib::embedded_capability_resources().contains(canonical_name)
    }

    /// The resource `canonical_name` holds somewhere inside it, if any.
    ///
    /// A resource answers with itself. A type answers with the first resource
    /// its layout reaches: `Tcp.Socket` is an ordinary sum whose every
    /// variant carries a provider handle, and a job may no more carry it than
    /// carry the handle directly.
    pub fn resource_within(&self, canonical_name: &str) -> Option<String> {
        self.resource_within_seen(canonical_name, &mut BTreeSet::new())
    }

    fn resource_within_seen(
        &self,
        canonical_name: &str,
        visiting: &mut BTreeSet<String>,
    ) -> Option<String> {
        if self.is_resource(canonical_name) {
            return Some(canonical_name.to_string());
        }
        if !visiting.insert(canonical_name.to_string()) {
            return None;
        }
        let type_def = self.type_def(canonical_name)?;
        let owner = canonical_name
            .rsplit_once('.')
            .map_or("", |(owner, _)| owner);
        let sources: Vec<String> = match type_def {
            TypeDef::Product { fields, .. } => fields.iter().map(|(_, ty)| ty.clone()).collect(),
            TypeDef::Sum { variants, .. } => variants
                .iter()
                .flat_map(|variant| variant.fields.iter().cloned())
                .collect(),
        };
        for source in sources {
            let Ok(ty) = crate::types::parse_type_str_strict(&source) else {
                continue;
            };
            let mut spelled = BTreeSet::new();
            collect_spelled_names(&ty, &mut spelled);
            for name in spelled {
                let Some(canonical) = self.resolve(owner, &name) else {
                    continue;
                };
                if let Some(found) = self.resource_within_seen(&canonical, visiting) {
                    return Some(found);
                }
            }
        }
        None
    }
}

/// Every named type in `ty`, spelled exactly as its module wrote it.
fn collect_spelled_names(ty: &Type, out: &mut BTreeSet<String>) {
    match ty {
        Type::Named { name, .. } => {
            out.insert(name.clone());
        }
        Type::Result(left, right) | Type::Map(left, right) => {
            collect_spelled_names(left, out);
            collect_spelled_names(right, out);
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            collect_spelled_names(inner, out)
        }
        Type::Tuple(items) | Type::Fn(items, _, _) => {
            for item in items {
                collect_spelled_names(item, out);
            }
            if let Type::Fn(_, ret, _) = ty {
                collect_spelled_names(ret, out);
            }
        }
        _ => {}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapabilitySemantics {
    Pure,
    Effectful,
}

impl CapabilitySemantics {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Pure => "pure",
            Self::Effectful => "effectful",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OracleDimension {
    Snapshot,
    Generative,
    Output,
    GenerativeOutput,
}

impl OracleDimension {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Snapshot => "snapshot",
            Self::Generative => "generative",
            Self::Output => "output",
            Self::GenerativeOutput => "generativeOutput",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReplaySemantics {
    Recorded,
    Reissued,
    Suppressed,
}

impl ReplaySemantics {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Recorded => "recorded",
            Self::Reissued => "reissued",
            Self::Suppressed => "suppressed",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CapabilityOperation {
    pub canonical_name: String,
    pub module: String,
    pub name: String,
    pub line: usize,
    pub exposed: bool,
    /// The key type this operation is generic over, if any. Exactly the way
    /// `Map` is generic over its key: the operation's own signature names it,
    /// and each call site pins it to one concrete type the program wrote.
    /// Only a capability this compiler ships may declare one.
    pub type_params: Vec<String>,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub semantics: CapabilitySemantics,
    pub oracle: Option<OracleDimension>,
    pub replay: Option<ReplaySemantics>,
    /// Canonical capability-owned resource type minted in the success payload,
    /// if any. The verifier passes an unconstrained value of this type to the
    /// operation's oracle; no distinctness between calls is assumed.
    pub minted_resource: Option<String>,
    pub hostile: Vec<String>,
    pub unmodelled: Vec<String>,
}

impl CapabilityOperation {
    pub fn is_effectful(&self) -> bool {
        self.semantics == CapabilitySemantics::Effectful
    }

    pub fn oracle_params(&self) -> Vec<Type> {
        let mut params = if self.oracle == Some(OracleDimension::Snapshot) {
            Vec::new()
        } else {
            vec![
                Type::named(crate::types::branch_path::TYPE_NAME.to_string()),
                Type::Int,
            ]
        };
        if let Some(resource) = &self.minted_resource {
            params.push(Type::named(resource.clone()));
        }
        params.extend(self.params.iter().map(|(_, ty)| ty.clone()));
        params
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilityContract {
    pub module: String,
    pub semantics: CapabilitySemantics,
    pub contract_descriptor: Vec<u8>,
    pub model_descriptor: Vec<u8>,
    pub contract_hash: String,
    pub model_hash: String,
    /// Canonical names of the dependency types this contract's boundary
    /// named, layouts included. Empty for every capability but a job kind
    /// that named one. A deployment pack reads it to learn which modules it
    /// has to carry so the hash can be recomputed away from the project tree.
    pub imported_types: BTreeSet<String>,
}

#[derive(Debug, Clone, Default)]
pub struct CapabilityRegistry {
    contracts: BTreeMap<String, CapabilityContract>,
    operations: BTreeMap<String, CapabilityOperation>,
    resource_types: BTreeSet<String>,
    resource_tainted_types: BTreeSet<String>,
    /// Capability-owned represented boundary types, keyed canonically.
    boundary_types: BTreeMap<String, TypeDef>,
    /// `(operation identity, explicit profile references)` observed in verify
    /// laws across the entry module and dependency closure. Kept separately
    /// from the contract so proof reports can distinguish model-local hostile
    /// profiles from user-authored adversaries without changing either hash.
    profile_givens: Vec<(String, Vec<String>)>,
}

impl CapabilityRegistry {
    pub fn contracts(&self) -> impl Iterator<Item = &CapabilityContract> {
        self.contracts.values()
    }

    pub fn operations(&self) -> impl Iterator<Item = &CapabilityOperation> {
        self.operations.values()
    }

    pub fn operation(&self, canonical_name: &str) -> Option<&CapabilityOperation> {
        self.operations.get(canonical_name)
    }

    pub fn contract(&self, module: &str) -> Option<&CapabilityContract> {
        self.contracts.get(module)
    }

    /// Whether any validated contract uses the compiler-owned octet wire type.
    ///
    /// The canonical descriptor owns this distinction: a capability-local
    /// declaration also named `Bytes` is rendered as an ordinary owned type,
    /// while only the standard wire type emits this exact descriptor field.
    /// Backends consume the validated contract fact instead of repeating
    /// source-name-based type resolution.
    pub fn uses_standard_bytes(&self) -> bool {
        const WIRE_TYPE: &[u8] = b"Aver::Bytes = octets";
        self.contracts.values().any(|contract| {
            contract
                .contract_descriptor
                .windows(WIRE_TYPE.len())
                .any(|window| window == WIRE_TYPE)
        })
    }

    pub fn resource_types(&self) -> impl Iterator<Item = &String> {
        self.resource_types.iter()
    }

    /// Capability resources, plus represented types whose value transitively
    /// contains one. Equality or hashing any of these would expose provider
    /// token identity, which the contract deliberately does not define.
    pub fn resource_tainted_types(&self) -> impl Iterator<Item = &String> {
        self.resource_tainted_types.iter()
    }

    pub fn is_resource_tainted(&self, canonical_name: &str) -> bool {
        self.resource_tainted_types.contains(canonical_name)
    }

    pub fn boundary_type(&self, canonical_name: &str) -> Option<&TypeDef> {
        self.boundary_types.get(canonical_name)
    }

    pub fn boundary_types(&self) -> impl Iterator<Item = (&String, &TypeDef)> {
        self.boundary_types.iter()
    }

    pub fn profile_source_counts(&self, canonical_name: &str) -> (usize, usize) {
        let declared = self
            .operation(canonical_name)
            .map(|operation| operation.hostile.len())
            .unwrap_or(0);
        let hostile = self
            .operation(canonical_name)
            .map(|operation| operation.hostile.iter().cloned().collect::<BTreeSet<_>>())
            .unwrap_or_default();
        let mut user_profiles = BTreeSet::new();
        for (operation, profiles) in &self.profile_givens {
            if operation != canonical_name {
                continue;
            }
            for profile in profiles {
                let local = profile.rsplit('.').next().unwrap_or(profile);
                let is_model_local = !profile.contains('.') && hostile.contains(local);
                if !is_model_local {
                    user_profiles.insert(profile.clone());
                }
            }
        }
        (declared, user_profiles.len())
    }

    /// Function bodies that belong exclusively to this capability's proof
    /// model and therefore have no place in a runtime artifact.
    ///
    /// Hostile declarations name model roots, but their transitive helpers
    /// are part of the model as well.  A helper can also be reachable from an
    /// exported ordinary function, so subtract the complete runtime closure
    /// rather than dropping hostile roots (or private functions) by name.
    pub(crate) fn verification_only_function_names(
        &self,
        module: &str,
        fn_defs: &[FnDef],
        exposes: &[String],
    ) -> BTreeSet<String> {
        let functions: BTreeMap<&str, &FnDef> =
            fn_defs.iter().map(|fd| (fd.name.as_str(), fd)).collect();
        let model_roots: BTreeSet<String> = self
            .operations()
            .filter(|operation| operation.module == module)
            .flat_map(|operation| operation.hostile.iter().cloned())
            .collect();
        if model_roots.is_empty() {
            return BTreeSet::new();
        }

        let mut errors = Vec::new();
        let model_closure =
            descriptor::function_closure(module, &model_roots, &functions, &mut errors);
        debug_assert!(
            errors.is_empty(),
            "validated capability model closure became incomplete: {errors:?}"
        );

        let declared_exposes = crate::visibility::declared_exposes(exposes);
        let runtime_roots: BTreeSet<String> = fn_defs
            .iter()
            .filter(|fd| crate::visibility::is_exposed(&fd.name, declared_exposes))
            .map(|fd| fd.name.clone())
            .collect();
        let runtime_closure =
            descriptor::function_closure(module, &runtime_roots, &functions, &mut errors);
        debug_assert!(
            errors.is_empty(),
            "validated capability runtime closure became incomplete: {errors:?}"
        );

        model_closure
            .difference(&runtime_closure)
            .cloned()
            .collect()
    }

    pub fn merge(&mut self, other: CapabilityRegistry) {
        self.contracts.extend(other.contracts);
        self.operations.extend(other.operations);
        self.resource_types.extend(other.resource_types);
        self.resource_tainted_types
            .extend(other.resource_tainted_types);
        self.boundary_types.extend(other.boundary_types);
        self.profile_givens.extend(other.profile_givens);
    }

    /// Build contracts from one parsed module. `scope` is the canonical
    /// dependency path used at call sites; for an entry module it should be
    /// the declared module name.
    ///
    /// A module read on its own sees no other module, so a job kind that
    /// names a dependency type needs [`Self::from_module_in_program`]. Every
    /// caller that has the rest of the program in hand passes it.
    pub fn from_module(
        scope: &str,
        items: &[TopLevel],
    ) -> (CapabilityRegistry, Vec<CapabilityError>) {
        Self::from_module_in_program(scope, items, &DependencyTypes::default())
    }

    /// Build contracts from one parsed module, with the program's other
    /// declarations available.
    ///
    /// The only thing `dependencies` changes is what a job kind may name at
    /// its boundary. Every other contract is built identically, and a job
    /// kind that names nothing from a dependency keeps the identity it
    /// already published.
    pub fn from_module_in_program(
        scope: &str,
        items: &[TopLevel],
        dependencies: &DependencyTypes,
    ) -> (CapabilityRegistry, Vec<CapabilityError>) {
        let mut registry = CapabilityRegistry::default();
        let mut errors = Vec::new();
        registry.profile_givens = collect_profile_givens(items);
        let module = items.iter().find_map(|item| match item {
            TopLevel::Module(module) => Some(module),
            _ => None,
        });
        let capability_items: Vec<&CapabilityItem> = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::Capability(item) => Some(item),
                _ => None,
            })
            .collect();

        let Some(module) = module else {
            for item in capability_items {
                errors.push(CapabilityError::at(
                    item.line(),
                    format!(
                        "`{} {}` is only legal in a module declaring `kind = capability`",
                        item.keyword(),
                        item.name()
                    ),
                ));
            }
            return (registry, errors);
        };

        match module.kind.as_deref() {
            None => {
                if let Some(semantics) = &module.semantics {
                    errors.push(CapabilityError::at(
                        module.semantics_line.unwrap_or(module.line),
                        format!(
                            "module '{}' declares `semantics = {semantics}` without `kind = capability`",
                            module.name
                        ),
                    ));
                }
                for item in capability_items {
                    errors.push(CapabilityError::at(
                        item.line(),
                        format!(
                            "`{} {}` is only legal in a module declaring `kind = capability`",
                            item.keyword(),
                            item.name()
                        ),
                    ));
                }
                return (registry, errors);
            }
            Some("capability") => {}
            Some(other) => {
                errors.push(CapabilityError::at(
                    module.kind_line.unwrap_or(module.line),
                    format!(
                        "unknown module kind '{other}'; the only supported kind is `capability`"
                    ),
                ));
                return (registry, errors);
            }
        }

        let semantics = match module.semantics.as_deref() {
            Some("pure") => CapabilitySemantics::Pure,
            Some("effectful") => CapabilitySemantics::Effectful,
            Some(other) => {
                errors.push(CapabilityError::at(
                    module.semantics_line.unwrap_or(module.line),
                    format!(
                        "capability module '{}' has unknown semantics '{other}'; expected `pure` or `effectful`",
                        module.name
                    ),
                ));
                return (registry, errors);
            }
            None => {
                errors.push(CapabilityError::at(
                    module.kind_line.unwrap_or(module.line),
                    format!(
                        "capability module '{}' must declare `semantics = pure` or `semantics = effectful`",
                        module.name
                    ),
                ));
                return (registry, errors);
            }
        };

        let mut operations = Vec::new();
        let mut resources = Vec::new();
        let mut seen = BTreeSet::new();
        let ordinary_fn_names: BTreeSet<&str> = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd.name.as_str()),
                _ => None,
            })
            .collect();
        let ordinary_type_names: BTreeSet<&str> = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::TypeDef(td) => Some(type_def_name(td)),
                _ => None,
            })
            .collect();
        for item in capability_items {
            if !seen.insert(item.name().to_string()) {
                errors.push(CapabilityError::at(
                    item.line(),
                    format!(
                        "capability module '{}' declares '{}' more than once",
                        module.name,
                        item.name()
                    ),
                ));
                continue;
            }
            match item {
                CapabilityItem::Resource { name, .. } => {
                    if ordinary_type_names.contains(name.as_str()) {
                        errors.push(CapabilityError::at(
                            item.line(),
                            format!(
                                "capability resource '{}.{}' conflicts with a represented type of the same name",
                                scope, name
                            ),
                        ));
                    }
                    resources.push(name.clone())
                }
                CapabilityItem::Operation(op) => {
                    if ordinary_fn_names.contains(op.name.as_str()) {
                        errors.push(CapabilityError::at(
                            op.line,
                            format!(
                                "capability operation '{}.{}' conflicts with a function body of the same name",
                                scope, op.name
                            ),
                        ));
                    }
                    if let Some(parsed) = parse_operation(scope, module, semantics, op, &mut errors)
                    {
                        operations.push(parsed);
                    }
                }
            }
        }

        let type_defs: BTreeMap<String, &TypeDef> = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::TypeDef(td) => Some((type_def_name(td).to_string(), td)),
                _ => None,
            })
            .collect();
        let mut locally_declared: BTreeSet<String> = type_defs.keys().cloned().collect();
        locally_declared.extend(resources.iter().cloned());
        // Naming `Work.Job` at the boundary is what makes a capability a job
        // kind, and it is the whole gate on this privilege: a job is answered
        // by a function of the same program, so there is no host ABI for a
        // dependency's layout to move behind `contract_hash`. `Work` and
        // `Wait` read the handle rather than mint one, exactly as
        // `work::job_kinds` excludes them.
        let is_job_kind = scope != work::WORK_MODULE
            && scope != work::WAIT_MODULE
            && operations.iter().any(|operation| {
                operation
                    .params
                    .iter()
                    .any(|(_, ty)| work::mentions_job(ty, scope))
                    || work::mentions_job(&operation.return_type, scope)
            });
        let imported_types = validate_boundary_type_ownership(
            scope,
            &operations,
            &locally_declared,
            &type_defs,
            &module.depends,
            if is_job_kind {
                Some(dependencies)
            } else {
                None
            },
            &mut errors,
        );
        let reachable_types = reachable_type_defs(&operations, &type_defs);
        let resource_tainted = resource_tainted_type_names(&resources, &type_defs);
        validate_operation_boundaries(
            scope,
            &mut operations,
            &resources,
            &resource_tainted,
            &mut errors,
        );
        validate_resource_map_keys(&operations, &resource_tainted, &mut errors);
        validate_hostile_profiles(&operations, items, &mut errors);
        let contract_descriptor = render_contract_descriptor(
            scope,
            &module.name,
            &operations,
            &resources,
            &reachable_types,
            &imported_types,
            dependencies,
        );
        let contract_hash = hash_descriptor(&contract_descriptor);
        let model_descriptor =
            render_model_descriptor(scope, &contract_hash, &operations, items, &mut errors);
        let contract = CapabilityContract {
            module: scope.to_string(),
            semantics,
            contract_hash,
            model_hash: hash_descriptor(&model_descriptor),
            contract_descriptor,
            model_descriptor,
            imported_types: imported_types.keys().cloned().collect(),
        };

        for name in resources {
            let canonical = format!("{scope}.{name}");
            registry.resource_types.insert(canonical.clone());
            registry.resource_tainted_types.insert(canonical);
        }
        for name in resource_tainted {
            registry
                .resource_tainted_types
                .insert(format!("{scope}.{name}"));
        }
        for (name, type_def) in type_defs {
            registry
                .boundary_types
                .insert(format!("{scope}.{name}"), type_def.clone());
        }
        // A dependency layout the contract binds is a boundary type like any
        // other: the VM's value conversion, the wasm-gc plan and the Rust
        // codec generator all ask this table for it, under the canonical name
        // the descriptor prints. Its owning module declares it, so the Rust
        // backend emits the codec beside the struct the program already has
        // rather than declaring a second one.
        for (canonical, type_def) in imported_types {
            registry.boundary_types.insert(canonical, type_def);
        }
        for operation in operations {
            registry
                .operations
                .insert(operation.canonical_name.clone(), operation);
        }
        registry.contracts.insert(scope.to_string(), contract);
        (registry, errors)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilityError {
    pub line: usize,
    pub message: String,
}

impl CapabilityError {
    fn at(line: usize, message: String) -> Self {
        Self { line, message }
    }
}

fn collect_profile_givens(items: &[TopLevel]) -> Vec<(String, Vec<String>)> {
    let mut out = Vec::new();
    for item in items {
        let TopLevel::Verify(block) = item else {
            continue;
        };
        let crate::ast::VerifyKind::Law(law) = &block.kind else {
            continue;
        };
        for given in &law.givens {
            let crate::ast::VerifyGivenDomain::Explicit(values) = &given.domain else {
                continue;
            };
            let profiles = values
                .iter()
                .filter_map(|value| crate::ir::expr_to_dotted_name(&value.node))
                .collect::<Vec<_>>();
            if !profiles.is_empty() {
                out.push((given.type_name.clone(), profiles));
            }
        }
    }
    out
}

fn parse_operation(
    scope: &str,
    module: &Module,
    semantics: CapabilitySemantics,
    op: &crate::ast::Operation,
    errors: &mut Vec<CapabilityError>,
) -> Option<CapabilityOperation> {
    // A type parameter is a compiler-shipped surface. A program declares its
    // own capabilities, and a program writes concrete types: refusing it here,
    // by name, keeps `<K>` from becoming a way to write generics in a program.
    if !op.type_params.is_empty() && !crate::stdlib::has_shipped_provider(scope) {
        errors.push(CapabilityError::at(
            op.line,
            format!(
                "operation '{}.{}' declares a type parameter, and only a capability this compiler ships may do that; write the concrete type this operation carries",
                scope, op.name
            ),
        ));
    }
    let type_params = if crate::stdlib::has_shipped_provider(scope) {
        op.type_params.clone()
    } else {
        Vec::new()
    };
    let generic = |ty: Type| bind_type_params(ty, &type_params);
    let mut params = Vec::new();
    for (name, source) in &op.params {
        match crate::types::parse_type_str_strict(source) {
            Ok(ty) => params.push((name.clone(), generic(ty))),
            Err(unknown) => errors.push(CapabilityError::at(
                op.line,
                format!(
                    "operation '{}.{}' has unknown parameter type '{}' for '{}'",
                    scope, op.name, unknown, name
                ),
            )),
        }
    }
    let return_type = match crate::types::parse_type_str_strict(&op.return_type).map(generic) {
        Ok(ty) => ty,
        Err(unknown) => {
            errors.push(CapabilityError::at(
                op.line,
                format!(
                    "operation '{}.{}' has unknown return type '{}'",
                    scope, op.name, unknown
                ),
            ));
            return None;
        }
    };
    for (name, ty) in &params {
        if type_contains_fn(ty) {
            errors.push(CapabilityError::at(
                op.line,
                format!(
                    "operation '{}.{}' cannot take function-valued parameter '{}'; providers must not call back into the Aver program",
                    scope, op.name, name
                ),
            ));
        }
    }
    if type_contains_fn(&return_type) {
        errors.push(CapabilityError::at(
            op.line,
            format!(
                "operation '{}.{}' cannot return a function value; capability operations are first-order provider boundaries",
                scope, op.name
            ),
        ));
    }

    let (oracle, replay) = match semantics {
        CapabilitySemantics::Pure => {
            if op.oracle.is_some()
                || op.replay.is_some()
                || !op.hostile.is_empty()
                || !op.unmodelled.is_empty()
            {
                errors.push(CapabilityError::at(
                    op.line,
                    format!(
                        "pure capability operation '{}.{}' cannot declare oracle, replay, hostile, or unmodelled fields",
                        scope, op.name
                    ),
                ));
            }
            (None, None)
        }
        CapabilitySemantics::Effectful => {
            let oracle = match op.oracle.as_deref() {
                Some("generative") => Some(OracleDimension::Generative),
                Some("output") => Some(OracleDimension::Output),
                Some("generativeOutput") => Some(OracleDimension::GenerativeOutput),
                Some("snapshot") if crate::stdlib::is_standard_capability(scope) => {
                    Some(OracleDimension::Snapshot)
                }
                Some("snapshot") => {
                    errors.push(CapabilityError::at(
                        op.line,
                        format!(
                            "operation '{}.{}' cannot claim `oracle = snapshot`; program-defined capabilities cannot prove that the world stays unchanged between calls",
                            scope, op.name
                        ),
                    ));
                    None
                }
                Some(other) => {
                    errors.push(CapabilityError::at(
                        op.line,
                        format!(
                            "operation '{}.{}' has unknown oracle '{other}'; expected generative, output, or generativeOutput",
                            scope, op.name
                        ),
                    ));
                    None
                }
                None => {
                    errors.push(CapabilityError::at(
                        op.line,
                        format!(
                            "effectful capability operation '{}.{}' must declare `oracle = ...`",
                            scope, op.name
                        ),
                    ));
                    None
                }
            };
            let replay = match op.replay.as_deref() {
                Some("recorded") => Some(ReplaySemantics::Recorded),
                Some("reissued") => Some(ReplaySemantics::Reissued),
                Some("suppressed") => Some(ReplaySemantics::Suppressed),
                Some(other) => {
                    errors.push(CapabilityError::at(
                        op.line,
                        format!(
                            "operation '{}.{}' has unknown replay semantics '{other}'; expected recorded, reissued, or suppressed",
                            scope, op.name
                        ),
                    ));
                    None
                }
                None => {
                    errors.push(CapabilityError::at(
                        op.line,
                        format!(
                            "effectful capability operation '{}.{}' must declare `replay = ...`",
                            scope, op.name
                        ),
                    ));
                    None
                }
            };
            if let (Some(oracle), Some(replay)) = (oracle, replay) {
                let valid = match oracle {
                    OracleDimension::Generative | OracleDimension::GenerativeOutput => {
                        replay == ReplaySemantics::Recorded
                    }
                    OracleDimension::Output => matches!(
                        replay,
                        ReplaySemantics::Reissued | ReplaySemantics::Suppressed
                    ),
                    OracleDimension::Snapshot => replay == ReplaySemantics::Recorded,
                };
                if !valid {
                    errors.push(CapabilityError::at(
                        op.line,
                        format!(
                            "operation '{}.{}' has incompatible `oracle = {}` and `replay = {}`",
                            scope,
                            op.name,
                            oracle.as_str(),
                            replay.as_str()
                        ),
                    ));
                }
            }
            if oracle == Some(OracleDimension::Output) && return_type != Type::Unit {
                errors.push(CapabilityError::at(
                    op.line,
                    format!(
                        "output operation '{}.{}' must return Unit; output has no result oracle",
                        scope, op.name
                    ),
                ));
            }
            (oracle, replay)
        }
    };

    Some(CapabilityOperation {
        canonical_name: format!("{scope}.{}", op.name),
        module: scope.to_string(),
        name: op.name.clone(),
        line: op.line,
        exposed: crate::visibility::is_exposed(
            &op.name,
            (!module.exposes.is_empty()).then_some(module.exposes.as_slice()),
        ),
        type_params,
        params,
        return_type,
        semantics,
        oracle,
        replay,
        minted_resource: None,
        hostile: op.hostile.clone(),
        unmodelled: op.unmodelled.clone(),
    })
}

/// Turn every mention of a declared type parameter into a real type variable.
///
/// `parse_type_str_strict` reads a bare capitalised word as a named type, so
/// `Map<K, Wait.Item>` arrives as a map keyed by a type called `K`. This is
/// the one place that rewrite happens; after it, `K` is a `Type::Var` and
/// every checker and backend that already understands one understands this.
fn bind_type_params(ty: Type, type_params: &[String]) -> Type {
    if type_params.is_empty() {
        return ty;
    }
    let bind = |ty: Type| bind_type_params(ty, type_params);
    match ty {
        Type::Named { name, .. } if type_params.contains(&name) => Type::Var(name),
        Type::Result(ok, err) => Type::Result(Box::new(bind(*ok)), Box::new(bind(*err))),
        Type::Option(inner) => Type::Option(Box::new(bind(*inner))),
        Type::List(inner) => Type::List(Box::new(bind(*inner))),
        Type::Vector(inner) => Type::Vector(Box::new(bind(*inner))),
        Type::Map(key, value) => Type::Map(Box::new(bind(*key)), Box::new(bind(*value))),
        Type::Tuple(items) => Type::Tuple(items.into_iter().map(bind).collect()),
        other => other,
    }
}

fn type_contains_fn(ty: &Type) -> bool {
    match ty {
        Type::Fn(_, _, _) => true,
        Type::Result(left, right) | Type::Map(left, right) => {
            type_contains_fn(left) || type_contains_fn(right)
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => type_contains_fn(inner),
        Type::Tuple(items) => items.iter().any(type_contains_fn),
        Type::Named { .. }
        | Type::Int
        | Type::Float
        | Type::Str
        | Type::Bool
        | Type::Unit
        | Type::Var(_)
        | Type::Invalid => false,
    }
}
