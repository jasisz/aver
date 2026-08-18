//! Target-total capability binding accounting.
//!
//! A missing row is never used to mean "unsupported". Every loaded
//! capability has one explicit row for every shipped target, including
//! capabilities that the program declares but does not call.

use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;

use crate::ast::{Expr, FnDef, Spanned, Stmt, TopLevel};
use crate::capability::{CapabilityContract, CapabilityRegistry};
use crate::codegen::ModuleInfo;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CapabilityTarget {
    Vm,
    Rust,
    WasmGc,
    Wasip2,
}

impl CapabilityTarget {
    pub const ALL: [Self; 4] = [Self::Vm, Self::Rust, Self::WasmGc, Self::Wasip2];

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Vm => "vm",
            Self::Rust => "rust",
            Self::WasmGc => "wasm-gc",
            Self::Wasip2 => "wasip2",
        }
    }
}

impl std::fmt::Display for CapabilityTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for CapabilityTarget {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "vm" => Ok(Self::Vm),
            "rust" => Ok(Self::Rust),
            "wasm-gc" => Ok(Self::WasmGc),
            "wasip2" => Ok(Self::Wasip2),
            other => Err(format!(
                "error[capability-target-unknown]: unknown capability target `{other}`; expected one of: vm, rust, wasm-gc, wasip2"
            )),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HostBindingReason {
    RuntimeProviderRequired,
    ComponentImportRequired,
}

impl HostBindingReason {
    pub const fn code(self) -> &'static str {
        match self {
            Self::RuntimeProviderRequired => "runtime-provider-required",
            Self::ComponentImportRequired => "component-import-required",
        }
    }

    pub const fn description(self) -> &'static str {
        match self {
            Self::RuntimeProviderRequired => {
                "the native target accepts this contract through a host-installed ProviderRegistry binding"
            }
            Self::ComponentImportRequired => {
                "the component imports this contract as WIT and requires the host to supply it"
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnsupportedReason {
    HostImportAdapterNotGenerated,
    WitBoundaryTypeUnsupported(crate::codegen::wasip2::CapabilityWitUnsupported),
}

impl UnsupportedReason {
    pub const fn code(&self) -> &'static str {
        match self {
            Self::HostImportAdapterNotGenerated => "host-import-adapter-not-generated",
            Self::WitBoundaryTypeUnsupported(_) => "wit-boundary-type-unsupported",
        }
    }

    pub fn description(&self) -> String {
        match self {
            Self::HostImportAdapterNotGenerated => {
                "wasm-gc has no generated host-import adapter for this capability contract"
                    .to_string()
            }
            Self::WitBoundaryTypeUnsupported(detail) => detail.description(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetProvider {
    pub identity: String,
    pub fingerprint: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetBindingStatus {
    Provided(TargetProvider),
    HostBound { reason: HostBindingReason },
    Unsupported { reason: UnsupportedReason },
}

impl TargetBindingStatus {
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::Provided(_) => "provided",
            Self::HostBound { .. } => "host-bound",
            Self::Unsupported { .. } => "unsupported",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilityTargetRow {
    pub target: CapabilityTarget,
    pub capability: String,
    pub contract_hash: String,
    pub model_hash: String,
    pub declared_operations: BTreeSet<String>,
    pub required_operations: BTreeSet<String>,
    pub status: TargetBindingStatus,
}

impl CapabilityTargetRow {
    pub fn is_required(&self) -> bool {
        !self.required_operations.is_empty()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CapabilityTargetManifest {
    rows: Vec<CapabilityTargetRow>,
}

impl CapabilityTargetManifest {
    pub fn build(
        contracts: &CapabilityRegistry,
        required_operations: &BTreeSet<String>,
    ) -> Result<Self, String> {
        let mut required_by_capability: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for name in required_operations {
            let operation = contracts.operation(name).ok_or_else(|| {
                format!("capability target manifest received unknown required operation `{name}`")
            })?;
            required_by_capability
                .entry(operation.module.clone())
                .or_default()
                .insert(name.clone());
        }

        let mut rows =
            Vec::with_capacity(contracts.contracts().count() * CapabilityTarget::ALL.len());
        for contract in contracts.contracts() {
            let declared_operations = contracts
                .operations()
                .filter(|operation| operation.module == contract.module)
                .map(|operation| operation.canonical_name.clone())
                .collect::<BTreeSet<_>>();
            let required_operations = required_by_capability
                .get(&contract.module)
                .cloned()
                .unwrap_or_default();

            for target in CapabilityTarget::ALL {
                rows.push(CapabilityTargetRow {
                    target,
                    capability: contract.module.clone(),
                    contract_hash: contract.contract_hash.clone(),
                    model_hash: contract.model_hash.clone(),
                    declared_operations: declared_operations.clone(),
                    required_operations: required_operations.clone(),
                    status: binding_status(target, contract, contracts),
                });
            }
        }
        Ok(Self { rows })
    }

    pub fn rows(&self) -> &[CapabilityTargetRow] {
        &self.rows
    }

    pub fn for_target(
        &self,
        target: CapabilityTarget,
    ) -> impl Iterator<Item = &CapabilityTargetRow> {
        self.rows.iter().filter(move |row| row.target == target)
    }

    pub fn required_unsupported(
        &self,
        target: CapabilityTarget,
    ) -> impl Iterator<Item = &CapabilityTargetRow> {
        self.for_target(target).filter(|row| {
            row.is_required() && matches!(row.status, TargetBindingStatus::Unsupported { .. })
        })
    }
}

fn binding_status(
    target: CapabilityTarget,
    contract: &CapabilityContract,
    contracts: &CapabilityRegistry,
) -> TargetBindingStatus {
    if is_canonical_standard_time(contract) {
        let identity = match target {
            CapabilityTarget::Vm | CapabilityTarget::Rust => {
                aver_rt::provider::STANDARD_TIME_NATIVE_IDENTITY
            }
            CapabilityTarget::WasmGc => "aver.standard.Time/wasm-gc-imports",
            CapabilityTarget::Wasip2 => "aver.standard.Time/wasip2-wasi",
        };
        return TargetBindingStatus::Provided(TargetProvider {
            identity: identity.to_string(),
            fingerprint: aver_rt::provider::STANDARD_TIME_FINGERPRINT.to_string(),
        });
    }

    match target {
        CapabilityTarget::Vm | CapabilityTarget::Rust => TargetBindingStatus::HostBound {
            reason: HostBindingReason::RuntimeProviderRequired,
        },
        CapabilityTarget::WasmGc => TargetBindingStatus::Unsupported {
            reason: UnsupportedReason::HostImportAdapterNotGenerated,
        },
        CapabilityTarget::Wasip2 => {
            match crate::codegen::wasip2::CapabilityWitInterfacePlan::build(contracts, contract) {
                Ok(_) => TargetBindingStatus::HostBound {
                    reason: HostBindingReason::ComponentImportRequired,
                },
                Err(detail) => TargetBindingStatus::Unsupported {
                    reason: UnsupportedReason::WitBoundaryTypeUnsupported(detail),
                },
            }
        }
    }
}

fn is_canonical_standard_time(contract: &CapabilityContract) -> bool {
    if contract.module != "Time" {
        return false;
    }
    let canonical = crate::stdlib::standard_capability_registry();
    let Some(expected) = canonical.contract("Time") else {
        return false;
    };
    contract.contract_hash == expected.contract_hash && contract.model_hash == expected.model_hash
}

/// Syntactically required capability operations across the loaded program.
/// A declaration alone contributes no required operation.
pub fn required_capability_operations(
    items: &[TopLevel],
    modules: &[ModuleInfo],
    registry: &CapabilityRegistry,
) -> BTreeSet<String> {
    fn scan_expr(
        expr: &Spanned<Expr>,
        registry: &CapabilityRegistry,
        required: &mut BTreeSet<String>,
    ) {
        crate::codegen::expr_walk::walk(expr, &mut |node| {
            if let Some(name) = crate::ir::expr_to_dotted_name(&node.node)
                && registry.operation(&name).is_some()
            {
                required.insert(name);
            }
        });
    }

    fn scan_fn(function: &FnDef, registry: &CapabilityRegistry, required: &mut BTreeSet<String>) {
        for statement in function.body.stmts() {
            match statement {
                Stmt::Binding(_, _, expression) | Stmt::Expr(expression) => {
                    scan_expr(expression, registry, required);
                }
            }
        }
    }

    let mut required = BTreeSet::new();
    for item in items {
        match item {
            TopLevel::FnDef(function) => scan_fn(function, registry, &mut required),
            TopLevel::Stmt(statement) => match statement {
                Stmt::Binding(_, _, expression) | Stmt::Expr(expression) => {
                    scan_expr(expression, registry, &mut required);
                }
            },
            _ => {}
        }
    }
    for module in modules {
        for function in &module.fn_defs {
            scan_fn(function, registry, &mut required);
        }
    }
    required
}

pub fn shipped_target_provenance(
    target: CapabilityTarget,
    contracts: &CapabilityRegistry,
) -> Vec<crate::replay::CapabilityProvenance> {
    CapabilityTargetManifest::build(contracts, &BTreeSet::new())
        .expect("an empty required-operation set is valid")
        .for_target(target)
        .filter_map(|row| match &row.status {
            TargetBindingStatus::Provided(provider) => Some(crate::replay::CapabilityProvenance {
                capability: row.capability.clone(),
                contract_hash: row.contract_hash.clone(),
                model_hash: row.model_hash.clone(),
                provider: provider.identity.clone(),
                fingerprint: provider.fingerprint.clone(),
            }),
            TargetBindingStatus::HostBound { .. } | TargetBindingStatus::Unsupported { .. } => None,
        })
        .collect()
}
