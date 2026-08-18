use std::collections::BTreeMap;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::{Arc, Mutex};

use crate::AverInt;

use super::{
    CapabilityProvider, NativeProviderContractProvenance, NativeProviderProvenance,
    ProviderBinding, ProviderContext, ProviderContractSpec, ProviderFault, ProviderResource,
    ProviderResourceHandle, ProviderValue,
};

#[derive(Default)]
struct ResourceStore {
    next_slot: u64,
    resources: BTreeMap<(u64, u64, u64), ProviderResource>,
}

/// Contract-checked native provider core shared by VM and generated Rust.
///
/// It owns no compiler or VM values. Adapters convert their native values to
/// [`ProviderValue`], use this core for binding/resource validation and live
/// invocation, then validate/decode the result against their own type table.
#[derive(Clone, Default)]
pub struct NativeProviderRegistry {
    contracts: BTreeMap<String, ProviderContractSpec>,
    bindings: BTreeMap<String, ProviderBinding>,
    resources: Arc<Mutex<ResourceStore>>,
}

impl NativeProviderRegistry {
    pub fn new(contracts: impl IntoIterator<Item = ProviderContractSpec>) -> Result<Self, String> {
        let mut by_name = BTreeMap::new();
        for contract in contracts {
            if by_name
                .insert(contract.capability.clone(), contract)
                .is_some()
            {
                return Err("duplicate capability contract in native provider registry".to_string());
            }
        }
        Ok(Self {
            contracts: by_name,
            bindings: BTreeMap::new(),
            resources: Arc::new(Mutex::new(ResourceStore::default())),
        })
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
        let contract = self.contracts.get(binding.capability()).ok_or_else(|| {
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
        let missing = contract
            .operations
            .difference(binding.operations())
            .cloned()
            .collect::<Vec<_>>();
        let unknown = binding
            .operations()
            .difference(&contract.operations)
            .cloned()
            .collect::<Vec<_>>();
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

    pub fn binding(&self, capability: &str) -> Option<&ProviderBinding> {
        self.bindings.get(capability)
    }

    pub fn contract(&self, capability: &str) -> Option<&ProviderContractSpec> {
        self.contracts.get(capability)
    }

    pub fn preflight<'a>(
        &self,
        required_operations: impl IntoIterator<Item = &'a str>,
    ) -> Result<(), String> {
        for operation in required_operations {
            let contract = self
                .contracts
                .values()
                .find(|contract| contract.operations.contains(operation))
                .ok_or_else(|| {
                    format!("capability contract missing at runtime for '{operation}'")
                })?;
            if !self.bindings.contains_key(&contract.capability) {
                return Err(format!(
                    "error[capability-provider-missing]: capability provider missing for '{}' (contract_hash {})",
                    operation, contract.contract_hash
                ));
            }
        }
        Ok(())
    }

    pub fn invoke(&self, operation: &str, args: &[ProviderValue]) -> Result<ProviderValue, String> {
        let contract = self
            .contracts
            .values()
            .find(|contract| contract.operations.contains(operation))
            .ok_or_else(|| format!("capability contract missing at runtime for '{operation}'"))?;
        let binding = self.bindings.get(&contract.capability).ok_or_else(|| {
            format!(
                "error[capability-provider-missing]: capability provider missing for '{}' (contract_hash {})",
                operation, contract.contract_hash
            )
        })?;
        let context = ProviderContext {
            capability: contract.capability.clone(),
            operation: operation.to_string(),
            contract_hash: contract.contract_hash.clone(),
            model_hash: contract.model_hash.clone(),
        };
        catch_unwind(AssertUnwindSafe(|| binding.provider.invoke(&context, args)))
            .map_err(|panic| {
                let message = panic
                    .downcast_ref::<&str>()
                    .copied()
                    .or_else(|| panic.downcast_ref::<String>().map(String::as_str))
                    .unwrap_or("non-string panic payload");
                format!(
                    "error[capability-provider-panic]: provider '{}' panicked while calling '{}': {}",
                    binding.provider_identity(), operation, message
                )
            })?
            .map_err(|fault| {
                format!(
                    "error[capability-provider-fault]: provider fault from '{}' while calling '{}': {}",
                    binding.provider_identity(), operation, fault
                )
            })
    }

    pub fn provider_identity_for(&self, capability: &str) -> Option<&str> {
        self.binding(capability)
            .map(ProviderBinding::provider_identity)
    }

    /// Installed bindings in canonical capability order.
    pub fn provenance(&self) -> Vec<NativeProviderProvenance> {
        self.contracts
            .values()
            .filter_map(|contract| {
                let binding = self.bindings.get(&contract.capability)?;
                Some(NativeProviderProvenance {
                    capability: contract.capability.clone(),
                    contract_hash: contract.contract_hash.clone(),
                    model_hash: contract.model_hash.clone(),
                    provider: binding.provider_identity().to_string(),
                    fingerprint: binding.provider_fingerprint().to_string(),
                })
            })
            .collect()
    }

    pub fn contract_provenance(&self) -> Vec<NativeProviderContractProvenance> {
        self.contracts
            .values()
            .map(|contract| {
                let binding = self.bindings.get(&contract.capability);
                NativeProviderContractProvenance {
                    capability: contract.capability.clone(),
                    contract_hash: contract.contract_hash.clone(),
                    model_hash: contract.model_hash.clone(),
                    provider: binding.map(|binding| binding.provider_identity().to_string()),
                    fingerprint: binding.map(|binding| binding.provider_fingerprint().to_string()),
                }
            })
            .collect()
    }

    pub fn binding_id_for(&self, capability: &str) -> Result<u64, String> {
        self.binding(capability)
            .map(|binding| binding.id)
            .ok_or_else(|| {
                format!(
                    "error[capability-provider-missing]: capability provider missing for '{}'",
                    capability
                )
            })
    }

    pub fn resolve_resource(
        &self,
        capability: &str,
        expected_type: &str,
        handle: &ProviderResourceHandle,
    ) -> Result<ProviderResource, String> {
        let binding_id = self.binding_id_for(capability)?;
        if handle.binding_id != binding_id {
            return Err(format!(
                "resource '{}' belongs to a different provider binding",
                expected_type
            ));
        }
        if handle.type_name != expected_type {
            return Err(format!(
                "resource has type '{}', expected resource type '{}'",
                handle.type_name, expected_type
            ));
        }
        self.resources
            .lock()
            .map_err(|_| "resource store poisoned".to_string())?
            .resources
            .get(&(handle.binding_id, handle.slot, handle.generation))
            .cloned()
            .ok_or_else(|| format!("resource '{}' is stale", expected_type))
    }

    pub fn store_resource(
        &self,
        capability: &str,
        type_name: impl Into<String>,
        resource: ProviderResource,
    ) -> Result<ProviderResourceHandle, String> {
        let binding_id = self.binding_id_for(capability)?;
        let mut store = self
            .resources
            .lock()
            .map_err(|_| "resource store poisoned".to_string())?;
        let slot = store.next_slot;
        store.next_slot = store
            .next_slot
            .checked_add(1)
            .ok_or_else(|| "capability resource store exhausted".to_string())?;
        let generation = resource.id();
        store
            .resources
            .insert((binding_id, slot, generation), resource);
        Ok(ProviderResourceHandle {
            binding_id,
            type_name: type_name.into(),
            slot,
            generation,
        })
    }
}

/// Standard native Time provider shared by the bytecode VM and generated
/// Rust artifacts. The VM selects it through a registry; native artifacts
/// bind the same adapter statically.
pub struct StandardTimeProvider;

pub const STANDARD_TIME_NATIVE_IDENTITY: &str = "aver.standard.Time/native";
pub const STANDARD_TIME_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

pub fn standard_time_now() -> String {
    crate::time_now()
}

pub fn standard_time_unix_ms() -> AverInt {
    AverInt::from_i64(crate::time_unix_ms())
}

pub fn standard_time_sleep(ms: &AverInt) -> Result<(), ProviderFault> {
    let ms = ms.to_i64().ok_or_else(|| {
        ProviderFault::new(
            "integer_out_of_range",
            "Time.sleep: ms must fit a 64-bit integer",
        )
    })?;
    if ms < 0 {
        return Err(ProviderFault::new(
            "negative_duration",
            "Time.sleep: ms must be non-negative",
        ));
    }
    crate::time_sleep(ms);
    Ok(())
}

impl CapabilityProvider for StandardTimeProvider {
    fn identity(&self) -> &str {
        STANDARD_TIME_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_TIME_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match context.operation.as_str() {
            "Time.now" if args.is_empty() => Ok(ProviderValue::String(standard_time_now())),
            "Time.unixMs" if args.is_empty() => Ok(ProviderValue::Int(standard_time_unix_ms())),
            "Time.sleep" => {
                let [ProviderValue::Int(ms)] = args else {
                    return Err(ProviderFault::new(
                        "invalid_arguments",
                        format!("Time.sleep expects one Int argument, got {}", args.len()),
                    ));
                };
                standard_time_sleep(ms)?;
                Ok(ProviderValue::Unit)
            }
            operation => Err(ProviderFault::new(
                "unknown_operation",
                format!("standard Time provider cannot invoke '{operation}'"),
            )),
        }
    }
}

/// Standard native Random provider shared by the bytecode VM and generated
/// Rust artifacts. Target-specific wasm adapters bind the same source-owned
/// contract to their existing host imports.
#[cfg(feature = "random")]
pub struct StandardRandomProvider;

#[cfg(feature = "random")]
pub const STANDARD_RANDOM_NATIVE_IDENTITY: &str = "aver.standard.Random/native";
#[cfg(feature = "random")]
pub const STANDARD_RANDOM_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

#[cfg(feature = "random")]
pub fn standard_random_int(min: &AverInt, max: &AverInt) -> Result<AverInt, ProviderFault> {
    let min = min.to_i64().ok_or_else(|| {
        ProviderFault::new(
            "integer_out_of_range",
            "Random.int: bounds must fit a 64-bit integer",
        )
    })?;
    let max = max.to_i64().ok_or_else(|| {
        ProviderFault::new(
            "integer_out_of_range",
            "Random.int: bounds must fit a 64-bit integer",
        )
    })?;
    crate::random::random_int(min, max)
        .map(AverInt::from_i64)
        .map_err(|message| ProviderFault::new("invalid_range", message))
}

#[cfg(feature = "random")]
pub fn standard_random_float() -> f64 {
    crate::random::random_float()
}

#[cfg(feature = "random")]
impl CapabilityProvider for StandardRandomProvider {
    fn identity(&self) -> &str {
        STANDARD_RANDOM_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_RANDOM_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match context.operation.as_str() {
            "Random.int" => {
                let [ProviderValue::Int(min), ProviderValue::Int(max)] = args else {
                    return Err(ProviderFault::new(
                        "invalid_arguments",
                        format!("Random.int expects two Int arguments, got {}", args.len()),
                    ));
                };
                standard_random_int(min, max).map(ProviderValue::Int)
            }
            "Random.float" if args.is_empty() => Ok(ProviderValue::Float(standard_random_float())),
            operation => Err(ProviderFault::new(
                "unknown_operation",
                format!("standard Random provider cannot invoke '{operation}'"),
            )),
        }
    }
}
