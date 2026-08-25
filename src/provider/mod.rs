//! Typed bindings between canonical Aver capability contracts and hosts.
//!
//! Contract declarations describe what a program needs. A `ProviderBinding`
//! supplies how one exact contract is implemented for an execution target.
//! The registry checks that seam before the first operation can run.

mod ordering;
mod provenance;
pub(crate) mod standard;
mod target;
#[cfg(test)]
mod tests;
mod value;

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

/// Identify failures owned by host/provider composition rather than source
/// type checking or a target backend. CLI front doors use this to keep batch
/// skip summaries and audit diagnostics honest.
pub fn is_provider_setup_error(error: &str) -> bool {
    error.starts_with("error[capability-provider-")
        || error.starts_with("provider binding ")
        || error.starts_with("reserved standard capability ")
}

/// Shared, contract-checked provider set used by a VM and all of its parallel
/// branch VMs.
#[derive(Clone, Default)]
pub struct ProviderRegistry {
    contracts: CapabilityRegistry,
    native: NativeProviderRegistry,
    standard_args: Vec<String>,
    standard_args_binding_id: Option<u64>,
    standard_tcp_settings: aver_rt::tcp::TcpSettings,
    /// Runtime identity of the compiler-installed Tcp binding. Provider
    /// identity strings are host-supplied metadata and therefore cannot prove
    /// ownership: an explicit provider may deliberately use the same label.
    standard_tcp_binding_id: Option<u64>,
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
        let canonical = crate::stdlib::standard_capability_registry();
        for standard in standard::StandardCapabilityBinding::ALL {
            let module = standard.module();
            let Some(contract) = registry.contracts.contract(module).cloned() else {
                continue;
            };
            let expected = canonical
                .contract(module)
                .expect("standard capability registry is complete");
            if contract.contract_hash != expected.contract_hash {
                return Err(format!(
                    "reserved standard capability '{}' has contract_hash {}, expected {}",
                    module, contract.contract_hash, expected.contract_hash
                ));
            }
            if contract.model_hash != expected.model_hash {
                return Err(format!(
                    "reserved standard capability '{}' has model_hash {}, expected {}",
                    module, contract.model_hash, expected.model_hash
                ));
            }
            let operations = canonical
                .operations()
                .filter(|operation| operation.module == module)
                .map(|operation| operation.canonical_name.clone())
                .collect::<Vec<_>>();
            let provider = standard.native_provider();
            let binding =
                ProviderBinding::new(module, contract.contract_hash, operations, provider);
            let binding_id = binding.runtime_id();
            registry.bind(binding)?;
            match module {
                "Args" => registry.standard_args_binding_id = Some(binding_id),
                "Tcp" => registry.standard_tcp_binding_id = Some(binding_id),
                _ => {}
            }
        }
        Ok(registry)
    }

    /// Build the checked provider view for one program and overlay explicit
    /// host bindings. The same path serves embedded hosts and the cached CLI
    /// provider host: compiler defaults are installed first, then an explicit
    /// binding may replace one default or fill one custom capability slot.
    pub fn for_program_with_bindings(
        contracts: CapabilityRegistry,
        bindings: impl IntoIterator<Item = ProviderBinding>,
    ) -> Result<Self, String> {
        let mut registry = Self::for_program(contracts)?;
        let mut supplied = std::collections::BTreeSet::new();
        for binding in bindings {
            if !supplied.insert(binding.capability().to_string()) {
                return Err(format!(
                    "error[capability-provider-duplicate]: capability '{}' has more than one host-supplied provider binding",
                    binding.capability()
                ));
            }
            if registry.binding(binding.capability()).is_some() {
                registry.replace_binding(binding)?;
            } else {
                registry.bind(binding)?;
            }
        }
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
        Self {
            contracts,
            native,
            standard_args: Vec::new(),
            standard_args_binding_id: None,
            standard_tcp_settings: aver_rt::tcp::TcpSettings::default(),
            standard_tcp_binding_id: None,
        }
    }

    /// Give the compiler-installed Args provider the guest argument vector.
    /// Explicit host bindings remain authoritative and are never replaced.
    pub fn configure_standard_args(&mut self, args: Vec<String>) -> Result<(), String> {
        if self.standard_args == args {
            return Ok(());
        }
        let Some(binding) = self.binding("Args") else {
            self.standard_args = args;
            return Ok(());
        };
        if Some(binding.runtime_id()) != self.standard_args_binding_id {
            self.standard_args = args;
            return Ok(());
        }
        let contract = self
            .contracts
            .contract("Args")
            .expect("an installed standard Args binding has a contract");
        let operations = self
            .contracts
            .operations()
            .filter(|operation| operation.module == "Args")
            .map(|operation| operation.canonical_name.clone())
            .collect::<Vec<_>>();
        let replacement = ProviderBinding::new(
            "Args",
            contract.contract_hash.clone(),
            operations,
            std::sync::Arc::new(aver_rt::provider::StandardArgsProvider::new(args.clone())),
        );
        let replacement_id = replacement.runtime_id();
        self.replace_binding(replacement)?;
        self.standard_args_binding_id = Some(replacement_id);
        self.standard_args = args;
        Ok(())
    }

    /// Configure the compiler-shipped Tcp provider before execution begins.
    /// An explicitly installed host provider remains untouched: deployment
    /// settings are inputs to that provider, not authority to replace it.
    pub fn configure_standard_tcp(
        &mut self,
        settings: aver_rt::tcp::TcpSettings,
    ) -> Result<(), String> {
        if self.standard_tcp_settings == settings {
            return Ok(());
        }
        let Some(binding) = self.binding("Tcp") else {
            self.standard_tcp_settings = settings;
            return Ok(());
        };
        if Some(binding.runtime_id()) != self.standard_tcp_binding_id {
            self.standard_tcp_settings = settings;
            return Ok(());
        }

        let contract = self
            .contracts
            .contract("Tcp")
            .expect("an installed standard Tcp binding has a contract");
        let operations = self
            .contracts
            .operations()
            .filter(|operation| operation.module == "Tcp")
            .map(|operation| operation.canonical_name.clone())
            .collect::<Vec<_>>();
        let replacement = ProviderBinding::new(
            "Tcp",
            contract.contract_hash.clone(),
            operations,
            std::sync::Arc::new(aver_rt::provider::StandardTcpProvider::new(settings)),
        );
        let replacement_id = replacement.runtime_id();
        self.replace_binding(replacement)?;
        self.standard_tcp_binding_id = Some(replacement_id);
        self.standard_tcp_settings = settings;
        Ok(())
    }

    pub fn standard_tcp_settings(&self) -> aver_rt::tcp::TcpSettings {
        self.standard_tcp_settings
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

    /// Invoke the contract-checked native core with transport-neutral values.
    ///
    /// VM and generated-Rust adapters own their richer Aver type codecs. A
    /// Component Model adapter has already been type-checked by WIT and only
    /// needs the common binding, panic, and provider-fault boundary before it
    /// validates the returned WIT shape.
    #[cfg(feature = "wasip2")]
    pub(crate) fn invoke_provider_values(
        &self,
        operation: &str,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, String> {
        self.native.invoke(operation, args)
    }
}
