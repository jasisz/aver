//! Typed bindings between canonical Aver capability contracts and hosts.
//!
//! Contract declarations describe what a program needs. A `ProviderBinding`
//! supplies how one exact contract is implemented for an execution target.
//! The registry checks that seam before the first operation can run.

mod ordering;
mod provenance;
mod target;
#[cfg(test)]
mod tests;
mod value;

use std::collections::{BTreeMap, BTreeSet};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use crate::capability::{CapabilityOperation, CapabilityRegistry};
pub use aver_rt::provider::{
    CapabilityProvider, ProviderContext, ProviderFault, ProviderResource, ProviderValue,
};
pub use target::{
    CapabilityTarget, CapabilityTargetManifest, CapabilityTargetRow, HostBindingReason,
    TargetBindingStatus, TargetProvider, UnsupportedReason, required_capability_operations,
    shipped_target_provenance,
};

/// Opaque language-side reference to a provider-owned payload.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CapabilityResourceHandle {
    binding_id: u64,
    type_name: String,
    slot: u64,
    generation: u64,
}

impl std::fmt::Debug for CapabilityResourceHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("CapabilityResourceHandle(<opaque>)")
    }
}

impl CapabilityResourceHandle {
    pub(crate) fn binding_id(&self) -> u64 {
        self.binding_id
    }

    pub(crate) fn type_name(&self) -> &str {
        &self.type_name
    }

    pub(crate) fn slot(&self) -> u64 {
        self.slot
    }

    pub(crate) fn generation(&self) -> u64 {
        self.generation
    }

    pub(crate) fn from_runtime_parts(
        binding_id: u64,
        type_name: String,
        slot: u64,
        generation: u64,
    ) -> Self {
        Self {
            binding_id,
            type_name,
            slot,
            generation,
        }
    }
}

#[derive(Clone)]
pub struct ProviderBinding {
    id: u64,
    capability: String,
    contract_hash: String,
    operations: BTreeSet<String>,
    provider: Arc<dyn CapabilityProvider>,
}

impl ProviderBinding {
    pub fn new(
        capability: impl Into<String>,
        contract_hash: impl Into<String>,
        operations: impl IntoIterator<Item = impl Into<String>>,
        provider: Arc<dyn CapabilityProvider>,
    ) -> Self {
        static NEXT_BINDING_ID: AtomicU64 = AtomicU64::new(1);
        Self {
            id: NEXT_BINDING_ID.fetch_add(1, Ordering::Relaxed),
            capability: capability.into(),
            contract_hash: contract_hash.into(),
            operations: operations.into_iter().map(Into::into).collect(),
            provider,
        }
    }

    pub fn capability(&self) -> &str {
        &self.capability
    }

    pub fn contract_hash(&self) -> &str {
        &self.contract_hash
    }

    pub fn provider_identity(&self) -> &str {
        self.provider.identity()
    }

    pub fn provider_fingerprint(&self) -> &str {
        self.provider.fingerprint()
    }
}

#[derive(Default)]
struct ResourceStore {
    next_slot: u64,
    resources: BTreeMap<(u64, u64, u64), ProviderResource>,
}

/// Shared, contract-checked provider set used by a VM and all of its parallel
/// branch VMs.
#[derive(Clone, Default)]
pub struct ProviderRegistry {
    contracts: CapabilityRegistry,
    bindings: BTreeMap<String, ProviderBinding>,
    resources: Arc<Mutex<ResourceStore>>,
}

impl ProviderRegistry {
    pub fn standard() -> Self {
        Self::for_program(crate::stdlib::standard_capability_registry())
            .expect("standard capability bindings must match their contracts")
    }

    /// Build the provider view for one checked program. Custom contracts enter
    /// unbound; compiler-shipped native bindings are installed only after
    /// their embedded contract hash matches the program registry exactly.
    pub fn for_program(contracts: CapabilityRegistry) -> Result<Self, String> {
        let mut registry = Self::for_contracts(contracts);
        let Some(contract) = registry.contracts.contract("Time").cloned() else {
            return Ok(registry);
        };
        let canonical = crate::stdlib::standard_capability_registry();
        let expected = canonical.contract("Time").expect("standard Time contract");
        if contract.contract_hash != expected.contract_hash {
            return Err(format!(
                "reserved standard capability 'Time' has contract_hash {}, expected {}",
                contract.contract_hash, expected.contract_hash
            ));
        }
        if contract.model_hash != expected.model_hash {
            return Err(format!(
                "reserved standard capability 'Time' has model_hash {}, expected {}",
                contract.model_hash, expected.model_hash
            ));
        }
        let operations = canonical
            .operations()
            .filter(|operation| operation.module == "Time")
            .map(|operation| operation.canonical_name.clone())
            .collect::<Vec<_>>();
        registry.bind(ProviderBinding::new(
            "Time",
            contract.contract_hash,
            operations,
            Arc::new(aver_rt::provider::StandardTimeProvider),
        ))?;
        Ok(registry)
    }

    pub fn for_contracts(contracts: CapabilityRegistry) -> Self {
        Self {
            contracts,
            bindings: BTreeMap::new(),
            resources: Arc::new(Mutex::new(ResourceStore::default())),
        }
    }

    pub fn bind(&mut self, binding: ProviderBinding) -> Result<(), String> {
        if self.bindings.contains_key(binding.capability()) {
            return Err(format!(
                "error[capability-provider-duplicate]: capability '{}' already has a provider binding",
                binding.capability()
            ));
        }
        self.validate_binding(&binding)?;
        self.bindings.insert(binding.capability.clone(), binding);
        Ok(())
    }

    /// Explicitly replace one installed binding. This is the only API that
    /// overrides compiler-shipped defaults; CLI/environment discovery does not
    /// call it.
    pub fn replace_binding(&mut self, binding: ProviderBinding) -> Result<(), String> {
        if !self.bindings.contains_key(binding.capability()) {
            return Err(format!(
                "error[capability-provider-missing]: cannot replace unbound capability '{}'",
                binding.capability()
            ));
        }
        self.validate_binding(&binding)?;
        self.bindings.insert(binding.capability.clone(), binding);
        Ok(())
    }

    fn validate_binding(&self, binding: &ProviderBinding) -> Result<(), String> {
        let contract = self
            .contracts
            .contract(binding.capability())
            .ok_or_else(|| {
                format!(
                    "provider binding names unknown capability '{}'",
                    binding.capability()
                )
            })?;
        if binding.contract_hash() != contract.contract_hash {
            return Err(format!(
                "error[capability-provider-mismatch]: provider '{}' for '{}' supplied contract_hash {}, expected {}",
                binding.provider_identity(),
                binding.capability(),
                binding.contract_hash(),
                contract.contract_hash
            ));
        }

        let required: BTreeSet<String> = self
            .contracts
            .operations()
            .filter(|operation| operation.module == binding.capability())
            .map(|operation| operation.canonical_name.clone())
            .collect();
        let missing: Vec<_> = required.difference(&binding.operations).cloned().collect();
        let unknown: Vec<_> = binding.operations.difference(&required).cloned().collect();
        if !missing.is_empty() || !unknown.is_empty() {
            let mut differences = Vec::new();
            if !missing.is_empty() {
                differences.push(format!("missing: {}", missing.join(", ")));
            }
            if !unknown.is_empty() {
                differences.push(format!("unknown: {}", unknown.join(", ")));
            }
            return Err(format!(
                "error[capability-provider-incomplete]: provider binding for '{}' does not match the contract operation set ({})",
                binding.capability(),
                differences.join("; ")
            ));
        }
        Ok(())
    }

    pub fn unbind(&mut self, capability: &str) {
        self.bindings.remove(capability);
    }

    pub fn contracts(&self) -> &CapabilityRegistry {
        &self.contracts
    }

    pub fn binding(&self, capability: &str) -> Option<&ProviderBinding> {
        self.bindings.get(capability)
    }

    pub fn preflight<'a>(
        &self,
        required_operations: impl IntoIterator<Item = &'a str>,
    ) -> Result<(), String> {
        for name in required_operations {
            let operation = self
                .contracts
                .operation(name)
                .ok_or_else(|| format!("capability contract missing at runtime for '{name}'"))?;
            if self.bindings.contains_key(&operation.module) {
                continue;
            }
            let contract = self
                .contracts
                .contract(&operation.module)
                .expect("operation has an owning capability contract");
            return Err(format!(
                "error[capability-provider-missing]: capability provider missing for '{}' (contract_hash {})",
                operation.canonical_name, contract.contract_hash
            ));
        }
        Ok(())
    }

    pub fn invoke(
        &self,
        operation: &CapabilityOperation,
        args: &[crate::value::Value],
    ) -> Result<crate::value::Value, String> {
        let binding = self.bindings.get(&operation.module).ok_or_else(|| {
            format!(
                "error[capability-provider-missing]: capability provider missing for '{}' (contract_hash {})",
                operation.canonical_name,
                self.contracts
                    .contract(&operation.module)
                    .map(|contract| contract.contract_hash.as_str())
                    .unwrap_or("<unknown>")
            )
        })?;
        if !binding.operations.contains(&operation.canonical_name) {
            return Err(format!(
                "provider binding '{}' does not implement '{}'",
                binding.provider.identity(),
                operation.canonical_name
            ));
        }

        let provider_args = operation
            .params
            .iter()
            .zip(args)
            .map(|((_, ty), value)| {
                value::to_provider_value(
                    value,
                    ty,
                    &operation.module,
                    &self.contracts,
                    binding.id,
                    &self.resources,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        if provider_args.len() != operation.params.len() || args.len() != operation.params.len() {
            return Err(format!(
                "provider boundary '{}' expected {} argument(s), got {}",
                operation.canonical_name,
                operation.params.len(),
                args.len()
            ));
        }

        let contract = self
            .contracts
            .contract(&operation.module)
            .expect("binding validation pinned a known contract");
        let context = ProviderContext {
            capability: operation.module.clone(),
            operation: operation.canonical_name.clone(),
            contract_hash: contract.contract_hash.clone(),
            model_hash: contract.model_hash.clone(),
        };
        let result = catch_unwind(AssertUnwindSafe(|| {
            binding.provider.invoke(&context, &provider_args)
        }))
        .map_err(|panic| {
            let message = panic
                .downcast_ref::<&str>()
                .copied()
                .or_else(|| panic.downcast_ref::<String>().map(String::as_str))
                .unwrap_or("non-string panic payload");
            format!(
                "error[capability-provider-panic]: provider '{}' panicked while calling '{}': {}",
                binding.provider.identity(),
                operation.canonical_name,
                message
            )
        })?
        .map_err(|fault| {
            format!(
                "error[capability-provider-fault]: provider fault from '{}' while calling '{}': {}",
                binding.provider.identity(),
                operation.canonical_name,
                fault
            )
        })?;
        let received_shape = result.shape();
        value::from_provider_value(
            result,
            &operation.return_type,
            &operation.module,
            &self.contracts,
            binding.id,
            operation.minted_resource.as_deref(),
            &self.resources,
        )
        .map_err(|message| {
            format!(
                "error[capability-provider-invalid-return]: provider '{}' returned an invalid value for '{}': expected {}, received {}; {}",
                binding.provider.identity(),
                operation.canonical_name,
                operation.return_type.display(),
                received_shape,
                message
            )
        })
    }
}
