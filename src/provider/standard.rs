//! Compiler-shipped capability bindings.
//!
//! Standard capability meaning still comes from embedded Aver source. This
//! catalog contains only execution metadata that source cannot name: the
//! concrete native adapter type and each shipped target's binding identity.

use std::sync::Arc;

use super::CapabilityProvider;
use super::target::CapabilityTarget;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StandardCapabilityBinding {
    Disk,
    Random,
    Time,
}

impl StandardCapabilityBinding {
    pub(crate) const ALL: [Self; 3] = [Self::Disk, Self::Random, Self::Time];

    pub(crate) const fn module(self) -> &'static str {
        match self {
            Self::Disk => "Disk",
            Self::Random => "Random",
            Self::Time => "Time",
        }
    }

    pub(crate) fn native_provider(self) -> Arc<dyn CapabilityProvider> {
        match self {
            Self::Disk => Arc::new(aver_rt::provider::StandardDiskProvider),
            Self::Random => Arc::new(aver_rt::provider::StandardRandomProvider),
            Self::Time => Arc::new(aver_rt::provider::StandardTimeProvider),
        }
    }

    pub(crate) const fn generated_rust_provider_type(self) -> &'static str {
        match self {
            Self::Disk => "aver_rt::provider::StandardDiskProvider",
            Self::Random => "aver_rt::provider::StandardRandomProvider",
            Self::Time => "aver_rt::provider::StandardTimeProvider",
        }
    }

    pub(crate) const fn fingerprint(self) -> &'static str {
        match self {
            Self::Disk => aver_rt::provider::STANDARD_DISK_FINGERPRINT,
            Self::Random => aver_rt::provider::STANDARD_RANDOM_FINGERPRINT,
            Self::Time => aver_rt::provider::STANDARD_TIME_FINGERPRINT,
        }
    }

    pub(crate) const fn target_identity(self, target: CapabilityTarget) -> &'static str {
        match (self, target) {
            (Self::Disk, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                aver_rt::provider::STANDARD_DISK_NATIVE_IDENTITY
            }
            (Self::Disk, CapabilityTarget::WasmGc) => "aver.standard.Disk/wasm-gc-imports",
            (Self::Disk, CapabilityTarget::Wasip2) => "aver.standard.Disk/wasip2-wasi",
            (Self::Random, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                aver_rt::provider::STANDARD_RANDOM_NATIVE_IDENTITY
            }
            (Self::Random, CapabilityTarget::WasmGc) => "aver.standard.Random/wasm-gc-imports",
            (Self::Random, CapabilityTarget::Wasip2) => "aver.standard.Random/wasip2-wasi",
            (Self::Time, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                aver_rt::provider::STANDARD_TIME_NATIVE_IDENTITY
            }
            (Self::Time, CapabilityTarget::WasmGc) => "aver.standard.Time/wasm-gc-imports",
            (Self::Time, CapabilityTarget::Wasip2) => "aver.standard.Time/wasip2-wasi",
        }
    }
}

pub(crate) fn for_module(module: &str) -> Option<StandardCapabilityBinding> {
    StandardCapabilityBinding::ALL
        .into_iter()
        .find(|binding| binding.module() == module)
}

#[cfg(test)]
mod tests {
    #[test]
    fn execution_catalog_exactly_covers_embedded_standard_capabilities() {
        let catalog = super::StandardCapabilityBinding::ALL.map(|binding| binding.module());
        assert_eq!(
            catalog.as_slice(),
            crate::stdlib::STANDARD_CAPABILITY_MODULES
        );
    }
}
