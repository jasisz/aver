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

mod descriptor;
#[cfg(test)]
mod tests;
mod validation;

use descriptor::{
    hash_descriptor, reachable_type_defs, render_contract_descriptor, render_model_descriptor,
};
use validation::{
    resource_tainted_type_names, type_def_name, validate_boundary_type_ownership,
    validate_hostile_profiles, validate_operation_boundaries, validate_resource_map_keys,
};

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
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub semantics: CapabilitySemantics,
    pub oracle: Option<OracleDimension>,
    pub replay: Option<ReplaySemantics>,
    /// Canonical capability-owned opaque type minted in the success payload,
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
        let mut params = vec![
            Type::named(crate::types::branch_path::TYPE_NAME.to_string()),
            Type::Int,
        ];
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
}

#[derive(Debug, Clone, Default)]
pub struct CapabilityRegistry {
    contracts: BTreeMap<String, CapabilityContract>,
    operations: BTreeMap<String, CapabilityOperation>,
    opaque_types: BTreeSet<String>,
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

    pub fn opaque_types(&self) -> impl Iterator<Item = &String> {
        self.opaque_types.iter()
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

    pub fn merge(&mut self, other: CapabilityRegistry) {
        self.contracts.extend(other.contracts);
        self.operations.extend(other.operations);
        self.opaque_types.extend(other.opaque_types);
        self.resource_tainted_types
            .extend(other.resource_tainted_types);
        self.boundary_types.extend(other.boundary_types);
        self.profile_givens.extend(other.profile_givens);
    }

    /// Build contracts from one parsed module. `scope` is the canonical
    /// dependency path used at call sites; for an entry module it should be
    /// the declared module name.
    pub fn from_module(
        scope: &str,
        items: &[TopLevel],
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
        let mut opaque = Vec::new();
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
                CapabilityItem::Opaque { name, .. } => {
                    if ordinary_type_names.contains(name.as_str()) {
                        errors.push(CapabilityError::at(
                            item.line(),
                            format!(
                                "capability opaque type '{}.{}' conflicts with a represented type of the same name",
                                scope, name
                            ),
                        ));
                    }
                    opaque.push(name.clone())
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
        locally_declared.extend(opaque.iter().cloned());
        validate_boundary_type_ownership(scope, &operations, &locally_declared, &mut errors);
        let reachable_types = reachable_type_defs(&operations, &type_defs);
        let resource_tainted = resource_tainted_type_names(&opaque, &type_defs);
        validate_operation_boundaries(
            scope,
            &mut operations,
            &opaque,
            &resource_tainted,
            &mut errors,
        );
        validate_resource_map_keys(&operations, &resource_tainted, &mut errors);
        validate_hostile_profiles(&operations, items, &mut errors);
        let contract_descriptor =
            render_contract_descriptor(scope, &module.name, &operations, &opaque, &reachable_types);
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
        };

        for name in opaque {
            let canonical = format!("{scope}.{name}");
            registry.opaque_types.insert(canonical.clone());
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
    let mut params = Vec::new();
    for (name, source) in &op.params {
        match crate::types::parse_type_str_strict(source) {
            Ok(ty) => params.push((name.clone(), ty)),
            Err(unknown) => errors.push(CapabilityError::at(
                op.line,
                format!(
                    "operation '{}.{}' has unknown parameter type '{}' for '{}'",
                    scope, op.name, unknown, name
                ),
            )),
        }
    }
    let return_type = match crate::types::parse_type_str_strict(&op.return_type) {
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
                    OracleDimension::Snapshot => false,
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
