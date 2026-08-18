//! One native Rust provider used unchanged by VM and generated-Rust hosts.

use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use aver_rt::provider::{
    CapabilityProvider, ProviderBinding, ProviderContext, ProviderFault, ProviderValue,
};

pub const CLOCK_CONTRACT_HASH: &str =
    "sha256:1fd9c680f96ea752d3eaf1665322c2aca584d088726d874640da3a310f97bc75";
pub const SHAPES_CONTRACT_HASH: &str =
    "sha256:32a895c71442e3cf3bf5fbad50ec7aaaa1b3a2c4de3c8ee5d8745abf3840242c";
pub const VAULT_CONTRACT_HASH: &str =
    "sha256:051ca6b17c994d429bc48c46f339761ce6206c8ee91201c8cf1e78c761799f73";
pub const MODES_CONTRACT_HASH: &str =
    "sha256:a721f2c2e4106b7e2abebf4a6600937ac59dfa78b9fc4b880ed3ae6c0574f9d5";
pub const PURE_PROBE_CONTRACT_HASH: &str =
    "sha256:842c8dcaef4ca39100285d8866c78b2848fdc10d0819c4def11a5fb018320120";

pub struct ClockProvider {
    calls: Arc<AtomicUsize>,
    fingerprint: &'static str,
}

impl CapabilityProvider for ClockProvider {
    fn identity(&self) -> &str {
        "example.counter-clock@1"
    }

    fn fingerprint(&self) -> &str {
        self.fingerprint
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        if context.operation != "Clock.now" || !args.is_empty() {
            return Err(ProviderFault::new("bad_call", &context.operation));
        }
        Ok(ProviderValue::Int(
            (self.calls.fetch_add(1, Ordering::SeqCst) as i64).into(),
        ))
    }
}

pub fn clock_binding(calls: Arc<AtomicUsize>, fingerprint: &'static str) -> ProviderBinding {
    clock_binding_for_contract(CLOCK_CONTRACT_HASH, calls, fingerprint)
}

pub fn clock_binding_for_contract(
    contract_hash: impl Into<String>,
    calls: Arc<AtomicUsize>,
    fingerprint: &'static str,
) -> ProviderBinding {
    ProviderBinding::new(
        "Clock",
        contract_hash,
        ["Clock.now"],
        Arc::new(ClockProvider { calls, fingerprint }),
    )
}

pub struct ShapesProvider;

impl CapabilityProvider for ShapesProvider {
    fn identity(&self) -> &str {
        "example.shapes-echo@1"
    }

    fn fingerprint(&self) -> &str {
        "shapes-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        let [value] = args else {
            return Err(ProviderFault::new("bad_args", "expected one Bundle"));
        };
        if context.operation != "Shapes.echo" {
            return Err(ProviderFault::new("bad_call", &context.operation));
        }
        Ok(ProviderValue::ResultOk(Box::new(value.clone())))
    }
}

pub fn shapes_binding() -> ProviderBinding {
    ProviderBinding::new(
        "Shapes",
        SHAPES_CONTRACT_HASH,
        ["Shapes.echo"],
        Arc::new(ShapesProvider),
    )
}

enum BoundaryFailure {
    WrongShape,
    Fault,
    Panic,
}

struct BoundaryFailureProvider(BoundaryFailure);

impl CapabilityProvider for BoundaryFailureProvider {
    fn identity(&self) -> &str {
        match self.0 {
            BoundaryFailure::WrongShape => "example.boundary-wrong-shape@1",
            BoundaryFailure::Fault => "example.boundary-fault@1",
            BoundaryFailure::Panic => "example.boundary-panic@1",
        }
    }

    fn fingerprint(&self) -> &str {
        "boundary-failure-v1"
    }

    fn invoke(
        &self,
        _context: &ProviderContext,
        _args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match self.0 {
            BoundaryFailure::WrongShape => {
                Ok(ProviderValue::String("private wrong value".to_string()))
            }
            BoundaryFailure::Fault => Err(ProviderFault::new("offline", "host transport failed")),
            BoundaryFailure::Panic => panic!("fixture provider exploded"),
        }
    }
}

fn shapes_failure_binding(failure: BoundaryFailure) -> ProviderBinding {
    ProviderBinding::new(
        "Shapes",
        SHAPES_CONTRACT_HASH,
        ["Shapes.echo"],
        Arc::new(BoundaryFailureProvider(failure)),
    )
}

pub fn wrong_shapes_binding() -> ProviderBinding {
    shapes_failure_binding(BoundaryFailure::WrongShape)
}

pub fn fault_shapes_binding() -> ProviderBinding {
    shapes_failure_binding(BoundaryFailure::Fault)
}

pub fn panic_shapes_binding() -> ProviderBinding {
    shapes_failure_binding(BoundaryFailure::Panic)
}

pub struct VaultProvider;
struct VaultSecret(&'static str);

impl CapabilityProvider for VaultProvider {
    fn identity(&self) -> &str {
        "example.vault@1"
    }

    fn fingerprint(&self) -> &str {
        "vault-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match context.operation.as_str() {
            "Vault.open" if args.is_empty() => Ok(ProviderValue::ResultOk(Box::new(
                ProviderValue::Resource(aver_rt::provider::ProviderResource::new(VaultSecret(
                    "provider-private-secret",
                ))),
            ))),
            "Vault.read" => {
                let [ProviderValue::Resource(resource)] = args else {
                    return Err(ProviderFault::new("bad_args", "expected Vault.Token"));
                };
                let secret = resource
                    .downcast_ref::<VaultSecret>()
                    .ok_or_else(|| ProviderFault::new("bad_resource", "wrong payload"))?;
                if secret.0 != "provider-private-secret" {
                    return Err(ProviderFault::new("bad_resource", "corrupt payload"));
                }
                Ok(ProviderValue::ResultOk(Box::new(ProviderValue::Int(
                    41.into(),
                ))))
            }
            other => Err(ProviderFault::new("bad_call", other)),
        }
    }
}

pub fn vault_binding() -> ProviderBinding {
    ProviderBinding::new(
        "Vault",
        VAULT_CONTRACT_HASH,
        ["Vault.open", "Vault.read"],
        Arc::new(VaultProvider),
    )
}

#[derive(Default)]
pub struct ReplayCounts {
    pub pure: AtomicUsize,
    pub recorded: AtomicUsize,
    pub suppressed: AtomicUsize,
    pub reissued: AtomicUsize,
}

struct ReplayMatrixProvider {
    counts: Arc<ReplayCounts>,
    fingerprint: &'static str,
}

impl CapabilityProvider for ReplayMatrixProvider {
    fn identity(&self) -> &str {
        "example.replay-matrix@1"
    }

    fn fingerprint(&self) -> &str {
        self.fingerprint
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        if !args.is_empty() {
            return Err(ProviderFault::new("bad_args", "expected no arguments"));
        }
        match context.operation.as_str() {
            "PureProbe.read" => {
                self.counts.pure.fetch_add(1, Ordering::SeqCst);
                Ok(ProviderValue::Int(10.into()))
            }
            "Modes.recorded" => {
                self.counts.recorded.fetch_add(1, Ordering::SeqCst);
                Ok(ProviderValue::Int(20.into()))
            }
            "Modes.suppressed" => {
                self.counts.suppressed.fetch_add(1, Ordering::SeqCst);
                Ok(ProviderValue::Unit)
            }
            "Modes.reissued" => {
                self.counts.reissued.fetch_add(1, Ordering::SeqCst);
                Ok(ProviderValue::Unit)
            }
            other => Err(ProviderFault::new("bad_call", other)),
        }
    }
}

pub fn replay_bindings() -> (Vec<ProviderBinding>, Arc<ReplayCounts>) {
    replay_bindings_with_fingerprint("replay-matrix-v1")
}

pub fn replay_bindings_with_fingerprint(
    fingerprint: &'static str,
) -> (Vec<ProviderBinding>, Arc<ReplayCounts>) {
    let counts = Arc::new(ReplayCounts::default());
    let provider = Arc::new(ReplayMatrixProvider {
        counts: counts.clone(),
        fingerprint,
    });
    (
        vec![
            ProviderBinding::new(
                "PureProbe",
                PURE_PROBE_CONTRACT_HASH,
                ["PureProbe.read"],
                provider.clone(),
            ),
            ProviderBinding::new(
                "Modes",
                MODES_CONTRACT_HASH,
                ["Modes.recorded", "Modes.reissued", "Modes.suppressed"],
                provider,
            ),
        ],
        counts,
    )
}
