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

use std::sync::Arc;

use crate::capability::{CapabilityOperation, CapabilityRegistry};
pub use aver_rt::provider::{
    CapabilityProvider, NativeProviderContractProvenance, NativeProviderProvenance,
    NativeProviderRegistry, ProviderBinding, ProviderContext, ProviderContractSpec, ProviderFault,
    ProviderResource, ProviderResourceHandle, ProviderValue,
};
pub use target::{
    CapabilityTarget, CapabilityTargetManifest, CapabilityTargetRow, HostBindingReason,
    TargetBindingStatus, TargetProvider, UnsupportedReason, required_capability_operations,
    shipped_target_provenance,
};

pub type CapabilityResourceHandle = ProviderResourceHandle;

/// Shared, contract-checked provider set used by a VM and all of its parallel
/// branch VMs.
#[derive(Clone, Default)]
pub struct ProviderRegistry {
    contracts: CapabilityRegistry,
    native: NativeProviderRegistry,
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
        let native = NativeProviderRegistry::new(contracts.contracts().map(|contract| {
            ProviderContractSpec::new(
                contract.module.clone(),
                contract.contract_hash.clone(),
                contract.model_hash.clone(),
                contracts
                    .operations()
                    .filter(|operation| operation.module == contract.module)
                    .map(|operation| operation.canonical_name.clone()),
            )
        }))
        .expect("CapabilityRegistry has unique capability identities");
        Self { contracts, native }
    }

    pub fn bind(&mut self, binding: ProviderBinding) -> Result<(), String> {
        self.native.bind(binding)
    }

    /// Explicitly replace one installed binding. This is the only API that
    /// overrides compiler-shipped defaults; CLI/environment discovery does not
    /// call it.
    pub fn replace_binding(&mut self, binding: ProviderBinding) -> Result<(), String> {
        self.native.replace_binding(binding)
    }

    pub fn unbind(&mut self, capability: &str) {
        self.native.unbind(capability);
    }

    pub fn contracts(&self) -> &CapabilityRegistry {
        &self.contracts
    }

    pub fn binding(&self, capability: &str) -> Option<&ProviderBinding> {
        self.native.binding(capability)
    }

    pub fn preflight<'a>(
        &self,
        required_operations: impl IntoIterator<Item = &'a str>,
    ) -> Result<(), String> {
        self.native.preflight(required_operations)
    }

    pub fn invoke(
        &self,
        operation: &CapabilityOperation,
        args: &[crate::value::Value],
    ) -> Result<crate::value::Value, String> {
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
                    &self.native,
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

        let result = self
            .native
            .invoke(&operation.canonical_name, &provider_args)?;
        let received_shape = result.shape();
        value::from_provider_value(
            result,
            &operation.return_type,
            &operation.module,
            &self.contracts,
            operation.minted_resource.as_deref(),
            &self.native,
        )
        .map_err(|message| {
            format!(
                "error[capability-provider-invalid-return]: provider '{}' returned an invalid value for '{}': expected {}, received {}; {}",
                self.native
                    .provider_identity_for(&operation.module)
                    .unwrap_or("<missing>"),
                operation.canonical_name,
                operation.return_type.display(),
                received_shape,
                message
            )
        })
    }
}
