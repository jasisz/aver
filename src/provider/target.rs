//! Auditable bindings shipped by compiler artifact targets.

use std::collections::BTreeSet;

use crate::capability::CapabilityRegistry;

/// Binding metadata for a compiler-shipped static/host target.
///
/// These rows do not make arbitrary capabilities available on artifact
/// targets. They recognize the one canonical standard contract whose target
/// adapters already ship with Aver.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetBindingInfo {
    pub target: String,
    pub capability: String,
    pub contract_hash: String,
    pub model_hash: String,
    pub provider: String,
    pub fingerprint: String,
    pub operations: BTreeSet<String>,
}

/// Compiler-shipped target bindings. Only canonical standard `Time` is
/// recognized here; custom capabilities remain VM-only in phase 1.
pub fn shipped_target_bindings(
    target: &str,
    contracts: &CapabilityRegistry,
) -> Vec<TargetBindingInfo> {
    let Some(contract) = contracts.contract("Time") else {
        return Vec::new();
    };
    let canonical = crate::stdlib::standard_capability_registry();
    let Some(expected) = canonical.contract("Time") else {
        return Vec::new();
    };
    if contract.contract_hash != expected.contract_hash
        || contract.model_hash != expected.model_hash
    {
        return Vec::new();
    }
    let provider = match target {
        "vm" => "aver.standard.Time/native",
        "rust" => "aver.standard.Time/rust-static",
        "wasm-gc" => "aver.standard.Time/wasm-gc-imports",
        "wasip2" => "aver.standard.Time/wasip2-wasi",
        _ => return Vec::new(),
    };
    vec![TargetBindingInfo {
        target: target.to_string(),
        capability: "Time".to_string(),
        contract_hash: contract.contract_hash.clone(),
        model_hash: contract.model_hash.clone(),
        provider: provider.to_string(),
        fingerprint: aver_rt::provider::STANDARD_TIME_FINGERPRINT.to_string(),
        operations: contracts
            .operations()
            .filter(|operation| operation.module == "Time")
            .map(|operation| operation.canonical_name.clone())
            .collect(),
    }]
}

pub fn shipped_target_provenance(
    target: &str,
    contracts: &CapabilityRegistry,
) -> Vec<crate::replay::CapabilityProvenance> {
    shipped_target_bindings(target, contracts)
        .into_iter()
        .map(|binding| crate::replay::CapabilityProvenance {
            capability: binding.capability,
            contract_hash: binding.contract_hash,
            model_hash: binding.model_hash,
            provider: binding.provider,
            fingerprint: binding.fingerprint,
        })
        .collect()
}
