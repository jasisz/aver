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
    Args,
    Console,
    Disk,
    Env,
    Http,
    Process,
    Random,
    Tcp,
    Terminal,
    Time,
}

impl StandardCapabilityBinding {
    pub(crate) const ALL: [Self; 10] = [
        Self::Args,
        Self::Console,
        Self::Disk,
        Self::Env,
        Self::Http,
        Self::Process,
        Self::Random,
        Self::Tcp,
        Self::Terminal,
        Self::Time,
    ];

    pub(crate) const fn module(self) -> &'static str {
        match self {
            Self::Args => "Args",
            Self::Console => "Console",
            Self::Disk => "Disk",
            Self::Env => "Env",
            Self::Http => "Http",
            Self::Process => "Process",
            Self::Random => "Random",
            Self::Tcp => "Tcp",
            Self::Terminal => "Terminal",
            Self::Time => "Time",
        }
    }

    pub(crate) fn native_provider(self) -> Arc<dyn CapabilityProvider> {
        match self {
            Self::Args => Arc::new(aver_rt::provider::StandardArgsProvider::default()),
            Self::Console => Arc::new(aver_rt::provider::StandardConsoleProvider),
            Self::Disk => Arc::new(aver_rt::provider::StandardDiskProvider),
            Self::Env => Arc::new(aver_rt::provider::StandardEnvProvider),
            Self::Http => Arc::new(aver_rt::provider::StandardHttpProvider),
            Self::Process => Arc::new(aver_rt::provider::StandardProcessProvider),
            Self::Random => Arc::new(aver_rt::provider::StandardRandomProvider),
            Self::Tcp => Arc::new(aver_rt::provider::StandardTcpProvider::default()),
            Self::Terminal => Arc::new(aver_rt::provider::StandardTerminalProvider),
            Self::Time => Arc::new(aver_rt::provider::StandardTimeProvider),
        }
    }

    pub(crate) const fn generated_rust_provider_type(self) -> &'static str {
        match self {
            Self::Args => "aver_rt::provider::StandardArgsProvider::default()",
            Self::Console => "aver_rt::provider::StandardConsoleProvider",
            Self::Disk => "aver_rt::provider::StandardDiskProvider",
            Self::Env => "aver_rt::provider::StandardEnvProvider",
            Self::Http => "aver_rt::provider::StandardHttpProvider",
            Self::Process => "aver_rt::provider::StandardProcessProvider",
            Self::Random => "aver_rt::provider::StandardRandomProvider",
            Self::Tcp => "aver_rt::provider::StandardTcpProvider",
            Self::Terminal => "aver_rt::provider::StandardTerminalProvider",
            Self::Time => "aver_rt::provider::StandardTimeProvider",
        }
    }

    pub(crate) const fn fingerprint(self) -> &'static str {
        match self {
            Self::Args => aver_rt::provider::STANDARD_ARGS_FINGERPRINT,
            Self::Console => aver_rt::provider::STANDARD_CONSOLE_FINGERPRINT,
            Self::Disk => aver_rt::provider::STANDARD_DISK_FINGERPRINT,
            Self::Env => aver_rt::provider::STANDARD_ENV_FINGERPRINT,
            Self::Http => aver_rt::provider::STANDARD_HTTP_FINGERPRINT,
            Self::Process => aver_rt::provider::STANDARD_PROCESS_FINGERPRINT,
            Self::Random => aver_rt::provider::STANDARD_RANDOM_FINGERPRINT,
            Self::Tcp => aver_rt::provider::STANDARD_TCP_FINGERPRINT,
            Self::Terminal => aver_rt::provider::STANDARD_TERMINAL_FINGERPRINT,
            Self::Time => aver_rt::provider::STANDARD_TIME_FINGERPRINT,
        }
    }

    pub(crate) const fn target_identity(self, target: CapabilityTarget) -> Option<&'static str> {
        match (self, target) {
            (Self::Args, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_ARGS_NATIVE_IDENTITY)
            }
            (Self::Args, CapabilityTarget::WasmGc) => Some("aver.standard.Args/wasm-gc-imports"),
            (Self::Args, CapabilityTarget::Wasip2) => Some("aver.standard.Args/wasip2-wasi"),
            (Self::Console, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_CONSOLE_NATIVE_IDENTITY)
            }
            (Self::Console, CapabilityTarget::WasmGc) => {
                Some("aver.standard.Console/wasm-gc-imports")
            }
            (Self::Console, CapabilityTarget::Wasip2) => Some("aver.standard.Console/wasip2-wasi"),
            (Self::Disk, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_DISK_NATIVE_IDENTITY)
            }
            (Self::Disk, CapabilityTarget::WasmGc) => Some("aver.standard.Disk/wasm-gc-imports"),
            (Self::Disk, CapabilityTarget::Wasip2) => Some("aver.standard.Disk/wasip2-wasi"),
            (Self::Env, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_ENV_NATIVE_IDENTITY)
            }
            (Self::Env, CapabilityTarget::WasmGc) => Some("aver.standard.Env/wasm-gc-imports"),
            (Self::Env, CapabilityTarget::Wasip2) => Some("aver.standard.Env/wasip2-wasi"),
            (Self::Http, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_HTTP_NATIVE_IDENTITY)
            }
            (Self::Http, CapabilityTarget::WasmGc) => Some("aver.standard.Http/wasm-gc-imports"),
            (Self::Http, CapabilityTarget::Wasip2) => Some("aver.standard.Http/wasip2-wasi"),
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
            (Self::Terminal, CapabilityTarget::Vm | CapabilityTarget::Rust) => {
                Some(aver_rt::provider::STANDARD_TERMINAL_NATIVE_IDENTITY)
            }
            (Self::Terminal, CapabilityTarget::WasmGc) => {
                Some("aver.standard.Terminal/wasm-gc-imports")
            }
            (Self::Terminal, CapabilityTarget::Wasip2) => None,
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
            (Self::Terminal, CapabilityTarget::Wasip2) => {
                "WASI 0.2 has no portable raw-terminal, cursor, color, key-input, or terminal-size interface"
            }
            _ => {
                "the compiler ships no binding for this standard capability on the selected target"
            }
        }
    }

    /// A target may bind most of a standard capability while deliberately
    /// lacking one operation. Keep that fact beside the target identity so
    /// the manifest, not a backend-specific effect-name table, decides
    /// whether the operations required by a program are available.
    pub(crate) fn unsupported_operation_detail(
        self,
        target: CapabilityTarget,
        operation: &str,
    ) -> Option<&'static str> {
        match (self, target, operation) {
            (Self::Env, CapabilityTarget::Wasip2, "Env.set") => Some(
                "WASI 0.2 exposes the environment as a read-only snapshot and has no portable environment mutation operation",
            ),
            (
                Self::Tcp,
                CapabilityTarget::Wasip2,
                "Tcp.beginConnect" | "Tcp.dialled" | "Tcp.listen" | "Tcp.accept"
                | "Tcp.peerAddress" | "Tcp.closeDial" | "Tcp.closeListener",
            ) => Some(
                "this target's WASI 0.2 socket binding owns blocking connects and connected-socket I/O only, with no non-blocking dial, listener, or peer-address resource; use `Tcp.connect` and the connected-socket operations",
            ),
            _ => None,
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

    /// The migration off legacy service builtins is a property, not a list:
    /// no operation of any registered standard capability may also be a
    /// compiler builtin. A builtin row is a second declaration of a name the
    /// `.av` contract already owns, and two declarations can disagree about
    /// arity, types, or effects.
    ///
    /// This used to be a hand-written roster of the twenty-five operations
    /// that were migrated at the time. A roster only covers the names on it:
    /// an eleventh capability, or a new operation on an existing one, could
    /// reintroduce exactly the duplication the migration removed and the
    /// guard would report success. The roster is now the registry.
    #[test]
    fn no_standard_capability_operation_is_also_a_compiler_builtin() {
        let vm_rows = crate::vm::builtin::VmBuiltin::ALL
            .iter()
            .map(|builtin| builtin.name())
            .collect::<std::collections::BTreeSet<_>>();
        let checker_rows = crate::types::checker::builtin_signature_names();

        let mut duplicated = Vec::new();
        let mut operations = 0;
        for operation in crate::stdlib::standard_capability_registry_ref().operations() {
            operations += 1;
            let name = operation.canonical_name.as_str();
            if vm_rows.contains(name) {
                duplicated.push(format!("{name} also has a `vm_builtins!` row"));
            }
            if checker_rows.contains(name) {
                duplicated.push(format!(
                    "{name} also has a signature in `checker/builtins.rs`"
                ));
            }
        }
        assert!(
            operations > 0,
            "the standard capability registry declared no operations at all"
        );
        assert!(
            duplicated.is_empty(),
            "standard capability operations declared a second time as compiler \
             builtins: {duplicated:?}"
        );
    }
}
