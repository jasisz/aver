//! Feature-independent planning for custom capability WIT interfaces.
//!
//! The plan is transport metadata, not a provider implementation. It is
//! deliberately independent of `wit-encoder` so target accounting can use
//! the same lowerability decision even when the wasip2 feature is disabled.
//! Keeping this plan in `codegen::wasip2` makes it the single owner of WIT
//! names, boundary types, canonical signatures, and target lowerability.

use std::collections::BTreeSet;

use crate::ast::Type;
use crate::capability::{CapabilityContract, CapabilityRegistry};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapabilityWitType {
    Unit,
    Bool,
    F64,
    String,
}

impl CapabilityWitType {
    fn lower(ty: &Type) -> Option<Self> {
        match ty {
            Type::Unit => Some(Self::Unit),
            Type::Bool => Some(Self::Bool),
            Type::Float => Some(Self::F64),
            Type::Str => Some(Self::String),
            Type::Int
            | Type::Result(_, _)
            | Type::Option(_)
            | Type::List(_)
            | Type::Tuple(_)
            | Type::Map(_, _)
            | Type::Vector(_)
            | Type::Fn(_, _, _)
            | Type::Var(_)
            | Type::Invalid
            | Type::Named { .. } => None,
        }
    }

    pub const fn wit_name(self) -> Option<&'static str> {
        match self {
            Self::Unit => None,
            Self::Bool => Some("bool"),
            Self::F64 => Some("f64"),
            Self::String => Some("string"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CapabilityWitTypePosition {
    Parameter(usize),
    Result,
}

impl CapabilityWitTypePosition {
    pub fn description(&self) -> String {
        match self {
            Self::Parameter(index) => format!("parameter {index}"),
            Self::Result => "result".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilityWitUnsupported {
    pub capability: String,
    pub operation: String,
    pub position: CapabilityWitTypePosition,
    pub aver_type: String,
}

impl CapabilityWitUnsupported {
    pub fn description(&self) -> String {
        format!(
            "operation `{}` {} has Aver type `{}`, which is outside the phase-3a WIT boundary subset",
            self.operation,
            self.position.description(),
            self.aver_type
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilityWitParameterPlan {
    /// Source position, retained even when Unit contributes no WIT value.
    pub index: usize,
    pub ty: CapabilityWitType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilityWitOperationPlan {
    pub canonical_name: String,
    pub wit_name: String,
    pub effectful: bool,
    pub params: Vec<CapabilityWitParameterPlan>,
    pub result: CapabilityWitType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilityWitInterfacePlan {
    pub capability: String,
    pub contract_hash: String,
    pub model_hash: String,
    pub interface_name: String,
    pub operations: Vec<CapabilityWitOperationPlan>,
}

impl CapabilityWitInterfacePlan {
    pub fn build(
        registry: &CapabilityRegistry,
        contract: &CapabilityContract,
    ) -> Result<Self, CapabilityWitUnsupported> {
        let mut operations = registry
            .operations()
            .filter(|operation| operation.module == contract.module)
            .map(|operation| {
                let mut params = Vec::with_capacity(operation.params.len());
                for (index, (_, ty)) in operation.params.iter().enumerate() {
                    let Some(ty) = CapabilityWitType::lower(ty) else {
                        return Err(CapabilityWitUnsupported {
                            capability: contract.module.clone(),
                            operation: operation.canonical_name.clone(),
                            position: CapabilityWitTypePosition::Parameter(index),
                            aver_type: ty.display(),
                        });
                    };
                    params.push(CapabilityWitParameterPlan { index, ty });
                }
                let Some(result) = CapabilityWitType::lower(&operation.return_type) else {
                    return Err(CapabilityWitUnsupported {
                        capability: contract.module.clone(),
                        operation: operation.canonical_name.clone(),
                        position: CapabilityWitTypePosition::Result,
                        aver_type: operation.return_type.display(),
                    });
                };
                Ok(CapabilityWitOperationPlan {
                    canonical_name: operation.canonical_name.clone(),
                    wit_name: format!("op-{}", encode_wit_identifier(&operation.name)),
                    effectful: operation.is_effectful(),
                    params,
                    result,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        operations.sort_by(|left, right| left.canonical_name.cmp(&right.canonical_name));

        Ok(Self {
            capability: contract.module.clone(),
            contract_hash: contract.contract_hash.clone(),
            model_hash: contract.model_hash.clone(),
            interface_name: format!(
                "cap-{}-c{}",
                encode_wit_identifier(&contract.module),
                contract_hash_hex(contract)
            ),
            operations,
        })
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CapabilityWitPlan {
    interfaces: Vec<CapabilityWitInterfacePlan>,
}

impl CapabilityWitPlan {
    /// Build interfaces only for capabilities used by the program. Once one
    /// operation is required, the interface retains the full contract.
    pub fn build(
        registry: &CapabilityRegistry,
        required_operations: &BTreeSet<String>,
    ) -> Result<Self, CapabilityWitUnsupported> {
        let mut required_capabilities = BTreeSet::new();
        for operation_name in required_operations {
            let operation =
                registry
                    .operation(operation_name)
                    .ok_or_else(|| CapabilityWitUnsupported {
                        capability: "<unknown>".to_string(),
                        operation: operation_name.clone(),
                        position: CapabilityWitTypePosition::Result,
                        aver_type: "<unknown operation>".to_string(),
                    })?;
            required_capabilities.insert(operation.module.clone());
        }

        let mut interfaces = Vec::with_capacity(required_capabilities.len());
        for capability in required_capabilities {
            let contract = registry
                .contract(&capability)
                .expect("required operation has an owning contract");
            if is_canonical_standard_time(contract) {
                continue;
            }
            interfaces.push(CapabilityWitInterfacePlan::build(registry, contract)?);
        }
        Ok(Self { interfaces })
    }

    pub fn interfaces(&self) -> &[CapabilityWitInterfacePlan] {
        &self.interfaces
    }
}

fn is_canonical_standard_time(contract: &CapabilityContract) -> bool {
    if contract.module != "Time" {
        return false;
    }
    crate::stdlib::standard_capability_registry()
        .contract("Time")
        .is_some_and(|standard| {
            contract.contract_hash == standard.contract_hash
                && contract.model_hash == standard.model_hash
        })
}

fn contract_hash_hex(contract: &CapabilityContract) -> &str {
    contract
        .contract_hash
        .strip_prefix("sha256:")
        .unwrap_or(&contract.contract_hash)
}

/// Injective encoding from Aver identifiers/module paths to lower-case WIT
/// identifiers. Hex-encoding the complete UTF-8 byte sequence avoids WIT's
/// restrictions on consecutive/trailing hyphens and makes collision freedom
/// obvious rather than dependent on the current Aver identifier grammar.
fn encode_wit_identifier(source: &str) -> String {
    let mut encoded = String::from("n");
    for byte in source.bytes() {
        encoded.push_str(&format!("{byte:02x}"));
    }
    encoded
}

#[cfg(test)]
#[path = "plan_tests.rs"]
mod tests;
