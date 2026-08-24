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
    Process,
    Random,
    Tcp,
    Time,
}

impl StandardCapabilityBinding {
    pub(crate) const ALL: [Self; 5] = [
        Self::Disk,
        Self::Process,
        Self::Random,
        Self::Tcp,
        Self::Time,
    ];

    pub(crate) const fn module(self) -> &'static str {
        match self {
            Self::Disk => "Disk",
            Self::Process => "Process",
            Self::Random => "Random",
            Self::Tcp => "Tcp",
            Self::Time => "Time",
        }
    }

    pub(crate) fn native_provider(self) -> Arc<dyn CapabilityProvider> {
        match self {
            Self::Disk => Arc::new(aver_rt::provider::StandardDiskProvider),
            Self::Process => Arc::new(aver_rt::provider::StandardProcessProvider),
            Self::Random => Arc::new(aver_rt::provider::StandardRandomProvider),
            Self::Tcp => Arc::new(aver_rt::provider::StandardTcpProvider::default()),
            Self::Time => Arc::new(aver_rt::provider::StandardTimeProvider),
        }
    }

    pub(crate) const fn generated_rust_provider_type(self) -> &'static str {
        match self {
            Self::Disk => "aver_rt::provider::StandardDiskProvider",
            Self::Process => "aver_rt::provider::StandardProcessProvider",
            Self::Random => "aver_rt::provider::StandardRandomProvider",
            Self::Tcp => "aver_rt::provider::StandardTcpProvider",
            Self::Time => "aver_rt::provider::StandardTimeProvider",
        }
    }

    pub(crate) const fn fingerprint(self) -> &'static str {
        match self {
            Self::Disk => aver_rt::provider::STANDARD_DISK_FINGERPRINT,
            Self::Process => aver_rt::provider::STANDARD_PROCESS_FINGERPRINT,
            Self::Random => aver_rt::provider::STANDARD_RANDOM_FINGERPRINT,
            Self::Tcp => aver_rt::provider::STANDARD_TCP_FINGERPRINT,
            Self::Time => aver_rt::provider::STANDARD_TIME_FINGERPRINT,
        }
    }

    pub(crate) const fn target_identity(self, target: CapabilityTarget) -> Option<&'static str> {
        match (self, target) {
            (Self::Disk, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_DISK_NATIVE_IDENTITY)
            }
            (Self::Disk, CapabilityTarget::WasmGc) => Some("aver.standard.Disk/wasm-gc-imports"),
            (Self::Disk, CapabilityTarget::Wasip2) => Some("aver.standard.Disk/wasip2-wasi"),
            (Self::Process, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_PROCESS_NATIVE_IDENTITY)
            }
            (Self::Process, CapabilityTarget::WasmGc) => {
                Some("aver.standard.Process/wasm-gc-imports")
            }
            (Self::Process, CapabilityTarget::Wasip2) => None,
            (Self::Random, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_RANDOM_NATIVE_IDENTITY)
            }
            (Self::Random, CapabilityTarget::WasmGc) => {
                Some("aver.standard.Random/wasm-gc-imports")
            }
            (Self::Random, CapabilityTarget::Wasip2) => Some("aver.standard.Random/wasip2-wasi"),
            (Self::Tcp, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_TCP_NATIVE_IDENTITY)
            }
            (Self::Tcp, CapabilityTarget::WasmGc) => Some("aver.standard.Tcp/wasm-gc-imports"),
            (Self::Tcp, CapabilityTarget::Wasip2) => Some("aver.standard.Tcp/wasip2-wasi"),
            (Self::Time, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_TIME_NATIVE_IDENTITY)
            }
            (Self::Time, CapabilityTarget::WasmGc) => Some("aver.standard.Time/wasm-gc-imports"),
            (Self::Time, CapabilityTarget::Wasip2) => Some("aver.standard.Time/wasip2-wasi"),
        }
    }

    pub(crate) const fn unsupported_target_detail(self, target: CapabilityTarget) -> &'static str {
        match (self, target) {
            (Self::Process, CapabilityTarget::Wasip2) => {
                "WASI 0.2 has no SIGINT/SIGTERM subscription API; use target `wasm-gc` and provide the `aver.process_stop_requested` host import"
            }
            _ => {
                "the compiler ships no binding for this standard capability on the selected target"
            }
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
