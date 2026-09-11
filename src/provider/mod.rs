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
pub mod work;

use crate::capability::{CapabilityOperation, CapabilityRegistry};
pub use aver_rt::provider::{
    CapabilityProvider, NativeProviderContractProvenance, NativeProviderProvenance,
    NativeProviderRegistry, ProviderBinding, ProviderContext, ProviderContractSpec, ProviderFault,
    ProviderResource, ProviderResourceHandle, ProviderValue,
};
pub use target::{
    CapabilityTarget, CapabilityTargetManifest, CapabilityTargetRow, HostBindingReason,
    TargetBindingStatus, TargetProvider, UnsupportedReason, required_capability_operations,
    shipped_target_provenance, standard_operations_bound_on,
};
pub use work::{WorkBaseContext, WorkProvider};

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
    /// Job kinds answered by a function of this program, keyed by capability.
    work: std::collections::BTreeMap<String, std::sync::Arc<work::WorkProvider>>,
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
            work: std::collections::BTreeMap::new(),
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

    /// Install one provider per job kind the manifest binds to a function of
    /// this program. `limit` bounds the jobs that may run at once.
    ///
    /// A capability the program does not declare is skipped in silence: the
    /// manifest gate at the program door has already reported it.
    pub fn install_work_bindings(
        &mut self,
        bindings: &[crate::config::ProviderWorkBinding],
        limit: usize,
    ) -> Result<(), String> {
        if bindings.is_empty() {
            return Ok(());
        }
        let engine = aver_rt::work::JobEngine::new(limit);
        let contracts = std::sync::Arc::new(self.contracts.clone());
        for binding in bindings {
            let Some(contract) = self.contracts.contract(&binding.capability).cloned() else {
                continue;
            };
            let Some((task, payload)) = work::boundary_types(&self.contracts, &binding.capability)
            else {
                continue;
            };
            let operations = self
                .contracts
                .operations()
                .filter(|operation| operation.module == binding.capability)
                .map(|operation| operation.canonical_name.clone())
                .collect::<Vec<_>>();
            let provider = std::sync::Arc::new(work::WorkProvider::new(
                &binding.capability,
                &binding.function,
                task,
                payload,
                contracts.clone(),
                engine.clone(),
            ));
            let installed = ProviderBinding::new(
                binding.capability.clone(),
                contract.contract_hash.clone(),
                operations,
                provider.clone(),
            );
            if self.binding(&binding.capability).is_some() {
                self.replace_binding(installed)?;
            } else {
                self.bind(installed)?;
            }
            self.work.insert(binding.capability.clone(), provider);
        }
        Ok(())
    }

    /// Whether a job kind of this program is still waiting for the compiled
    /// program its jobs run.
    pub fn work_needs_base_context(&self, capability: &str) -> bool {
        self.work
            .get(capability)
            .is_some_and(|provider| !provider.has_base_context())
    }

    /// Hand every job kind the compiled program its jobs run.
    pub fn install_work_base_context(&self, base: std::sync::Arc<work::WorkBaseContext>) {
        for provider in self.work.values() {
            provider.install_base_context(base.clone());
        }
    }

    /// The job kind that owns one operation, if any.
    pub fn work_provider_for(
        &self,
        operation: &str,
    ) -> Option<&std::sync::Arc<work::WorkProvider>> {
        let (module, _) = operation.rsplit_once('.')?;
        self.work.get(module)
    }

    /// Start, during replay, the job a recorded `begin` started.
    ///
    /// The job is pure, so recomputing it is the check: what the recording
    /// says the program saw is returned to the program, and what the function
    /// produces this time is what `take` compares it against.
    pub fn work_replay_begin(
        &self,
        operation: &CapabilityOperation,
        args: &[crate::value::Value],
        token: u64,
    ) -> Result<(), String> {
        let Some(provider) = self.work_provider_for(&operation.canonical_name) else {
            return Ok(());
        };
        let Some(((_, task_type), task)) = operation.params.first().zip(args.first()) else {
            return Err(format!(
                "{} was replayed without a task",
                operation.canonical_name
            ));
        };
        let task = value::to_provider_value(
            task,
            task_type,
            &operation.module,
            &self.contracts,
            &self.native,
        )?;
        provider.start_replay_job(token, task)
    }

    /// What the recomputed job holds, without collecting it.
    pub fn work_replay_peek(
        &self,
        capability: &str,
        token: u64,
    ) -> Option<Result<Option<crate::value::Value>, String>> {
        self.work.get(capability)?.replay_peek(token)
    }

    /// Collect the recomputed job's answer, so the live job and the recording
    /// agree about how many answers this job had.
    pub fn work_replay_consume(&self, capability: &str, token: u64) {
        if let Some(provider) = self.work.get(capability) {
            provider.replay_consume(token);
        }
    }

    /// Stop a recomputed job whose recording cancelled it.
    pub fn work_replay_cancel(&self, token: u64) {
        for provider in self.work.values() {
            provider.replay_cancel(token);
        }
    }

    /// Install the job kinds this project's `aver.toml` binds to functions of
    /// the program, with the job limit that manifest asks for.
    ///
    /// Every front door that runs a program on the VM comes through here, so
    /// `aver run`, `aver replay` and `aver verify` agree about what a job kind
    /// means and how many jobs may run at once.
    pub fn install_project_work_bindings(
        &mut self,
        module_root: &std::path::Path,
    ) -> Result<(), String> {
        // The loader's own errors already name the file they came from, so
        // repeating it here would say `aver.toml` twice in one line.
        let config = crate::config::ProjectConfig::load_from_dir(module_root)?;
        let Some(config) = config else {
            return Ok(());
        };
        let Some(manifest) = &config.provider_manifest else {
            return Ok(());
        };
        let limit = config.work_max_jobs();
        self.install_work_bindings(&manifest.work_bindings, limit)
    }

    /// Whether this program answers any job kind with a function of its own.
    pub fn has_work_bindings(&self) -> bool {
        !self.work.is_empty()
    }

    /// Cancel every running job and wait a bounded moment for them.
    pub fn shutdown_jobs(&self) {
        let mut engines = Vec::new();
        for provider in self.work.values() {
            if !engines
                .iter()
                .any(|engine| std::sync::Arc::ptr_eq(engine, provider.engine()))
            {
                engines.push(provider.engine().clone());
            }
        }
        for engine in engines {
            engine.shutdown();
        }
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
    #[cfg(any(feature = "wasm", feature = "wasip2"))]
    pub(crate) fn invoke_provider_values(
        &self,
        operation: &str,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, String> {
        self.native.invoke(operation, args)
    }
}
