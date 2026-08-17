//! Replay provenance validation for capability provider bindings.

use std::collections::{BTreeMap, BTreeSet};

use super::{ProviderBinding, ProviderRegistry};

impl ProviderRegistry {
    pub fn provenance(&self) -> Vec<crate::replay::CapabilityProvenance> {
        let mut out = Vec::new();
        for contract in self.contracts.contracts() {
            let Some(binding) = self.bindings.get(&contract.module) else {
                continue;
            };
            out.push(crate::replay::CapabilityProvenance {
                capability: contract.module.clone(),
                contract_hash: contract.contract_hash.clone(),
                model_hash: contract.model_hash.clone(),
                provider: binding.provider.identity().to_string(),
                fingerprint: binding.provider.fingerprint().to_string(),
            });
        }
        out
    }

    /// Validate replay metadata against the capability operations reachable
    /// from the compiled program. Pure and `reissued` operations execute a
    /// live provider during replay, so their implementation identity must be
    /// pinned even though pure calls never appear in the effect transcript.
    /// Merely declaring an unused capability does not require a binding.
    pub fn validate_replay_provenance_for_operations<'a>(
        &self,
        recorded: &[crate::replay::CapabilityProvenance],
        effects: &[crate::replay::EffectRecord],
        required_operations: impl IntoIterator<Item = &'a str>,
    ) -> Result<(), String> {
        let mut by_capability = BTreeMap::new();
        for entry in recorded {
            if by_capability
                .insert(entry.capability.as_str(), entry)
                .is_some()
            {
                return Err(format!(
                    "replay contains duplicate capability provenance for '{}'",
                    entry.capability
                ));
            }
        }

        for entry in recorded {
            let contract = self.contracts.contract(&entry.capability).ok_or_else(|| {
                format!(
                    "replay names capability '{}' which the current program does not declare",
                    entry.capability
                )
            })?;
            if entry.contract_hash != contract.contract_hash {
                return Err(format!(
                    "replay contract mismatch for '{}': recorded {}, current {}",
                    entry.capability, entry.contract_hash, contract.contract_hash
                ));
            }
            if entry.model_hash != contract.model_hash {
                return Err(format!(
                    "replay model mismatch for '{}': recorded {}, current {}",
                    entry.capability, entry.model_hash, contract.model_hash
                ));
            }
        }

        let mut live_capabilities = BTreeSet::new();
        for operation_name in required_operations {
            let Some(operation) = self.contracts.operation(operation_name) else {
                continue;
            };
            let Some(contract) = self.contracts.contract(&operation.module) else {
                continue;
            };
            if contract.semantics == crate::capability::CapabilitySemantics::Pure
                || operation.replay == Some(crate::capability::ReplaySemantics::Reissued)
            {
                live_capabilities.insert(operation.module.as_str());
            }
        }

        // Pure operations are intentionally absent from the effect transcript,
        // while `reissued` operations execute instead of consuming their
        // recorded outcome. Both therefore need the same live implementation.
        for capability in live_capabilities {
            let provenance = by_capability.get(capability).ok_or_else(|| {
                format!(
                    "live replay capability '{}' has no provider provenance in the replay",
                    capability
                )
            })?;
            let binding = self.bindings.get(capability).ok_or_else(|| {
                if let Some(operation) = effects.iter().find_map(|effect| {
                    let operation = self.contracts.operation(&effect.effect_type)?;
                    (operation.module == capability
                        && operation.replay == Some(crate::capability::ReplaySemantics::Reissued))
                    .then_some(operation)
                }) {
                    return format!(
                        "reissued replay event '{}' requires a live provider",
                        operation.canonical_name
                    );
                }
                format!(
                    "capability '{}' requires a live provider during replay",
                    capability
                )
            })?;
            validate_live_provider(provenance, binding, capability)?;
        }

        for effect in effects {
            let Some(operation) = self.contracts.operation(&effect.effect_type) else {
                continue;
            };
            let provenance = by_capability.get(operation.module.as_str()).copied();
            if provenance.is_none() && operation.module != "Time" {
                return Err(format!(
                    "legacy replay event '{}' has no capability contract/model provenance; refusing to guess",
                    operation.canonical_name
                ));
            }
            if operation.replay == Some(crate::capability::ReplaySemantics::Reissued) {
                let provenance = provenance.ok_or_else(|| {
                    format!(
                        "reissued replay event '{}' has no provider provenance",
                        operation.canonical_name
                    )
                })?;
                let binding = self.bindings.get(&operation.module).ok_or_else(|| {
                    format!(
                        "reissued replay event '{}' requires a live provider",
                        operation.canonical_name
                    )
                })?;
                validate_live_provider(provenance, binding, &operation.canonical_name)?;
            }
        }
        Ok(())
    }
}

fn validate_live_provider(
    provenance: &crate::replay::CapabilityProvenance,
    binding: &ProviderBinding,
    boundary: &str,
) -> Result<(), String> {
    if provenance.provider == binding.provider.identity()
        && provenance.fingerprint == binding.provider.fingerprint()
    {
        return Ok(());
    }
    Err(format!(
        "live provider mismatch for '{}': recorded {}@{}, current {}@{}",
        boundary,
        provenance.provider,
        provenance.fingerprint,
        binding.provider.identity(),
        binding.provider.fingerprint()
    ))
}
