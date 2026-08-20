//! One native Rust provider used unchanged by VM and generated-Rust hosts.

use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use aver_rt::provider::{
    CapabilityProvider, ProviderBinding, ProviderContext, ProviderFault, ProviderValue,
};

pub const CLOCK_CONTRACT_HASH: &str =
    "sha256:1fd9c680f96ea752d3eaf1665322c2aca584d088726d874640da3a310f97bc75";
pub const ECHO_CONTRACT_HASH: &str =
    "sha256:36832abf9ae258a8d018106da7ea75b15618c40d9f45a1959ab5d59e59358586";
pub const SHAPES_CONTRACT_HASH: &str =
    "sha256:32a895c71442e3cf3bf5fbad50ec7aaaa1b3a2c4de3c8ee5d8745abf3840242c";
pub const VAULT_CONTRACT_HASH: &str =
    "sha256:9d47d09bba1f2905acf09cbb1f5d164feccf1e88d9fa36ac4c704ffe7038e138";
pub const MODES_CONTRACT_HASH: &str =
    "sha256:a721f2c2e4106b7e2abebf4a6600937ac59dfa78b9fc4b880ed3ae6c0574f9d5";
pub const PURE_PROBE_CONTRACT_HASH: &str =
    "sha256:842c8dcaef4ca39100285d8866c78b2848fdc10d0819c4def11a5fb018320120";
pub const TIME_CONTRACT_HASH: &str =
    "sha256:c7bd82159c4e5922771531cbf583bf6ff74a85dbb5c2c362d1e3b156c5720a49";

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

struct WitEchoProvider;

static COUNTED_WIT_ECHO_FACTORIES: AtomicUsize = AtomicUsize::new(0);

impl CapabilityProvider for WitEchoProvider {
    fn identity(&self) -> &str {
        "example.wit-echo@1"
    }

    fn fingerprint(&self) -> &str {
        "wit-echo-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        if COUNTED_WIT_ECHO_FACTORIES.load(Ordering::SeqCst) != 1 {
            return Err(ProviderFault::new(
                "duplicate_factory",
                "WIT Echo provider factory must run exactly once",
            ));
        }
        match (context.operation.as_str(), args) {
            ("Echo.echo", [ProviderValue::String(value)]) => {
                Ok(ProviderValue::String(format!("host echoed: {value}")))
            }
            ("Echo.healthy", []) => Ok(ProviderValue::Bool(true)),
            (operation, _) => Err(ProviderFault::new("bad_call", operation)),
        }
    }
}

pub fn counted_wit_echo_binding() -> ProviderBinding {
    COUNTED_WIT_ECHO_FACTORIES.fetch_add(1, Ordering::SeqCst);
    ProviderBinding::new(
        "Echo",
        ECHO_CONTRACT_HASH,
        ["Echo.echo", "Echo.healthy"],
        Arc::new(WitEchoProvider),
    )
}

pub struct ShapesProvider;

static COUNTED_SHAPES_FACTORIES: AtomicUsize = AtomicUsize::new(0);

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

struct CountedShapesProvider;

impl CapabilityProvider for CountedShapesProvider {
    fn identity(&self) -> &str {
        "example.counted-shapes-echo@1"
    }

    fn fingerprint(&self) -> &str {
        "counted-shapes-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        let factories = COUNTED_SHAPES_FACTORIES.load(Ordering::SeqCst);
        if factories != 1 {
            return Err(ProviderFault::new(
                "duplicate_factory",
                format!("expected one binding construction, observed {factories}"),
            ));
        }
        ShapesProvider.invoke(context, args)
    }
}

/// Host-workflow fixture: every direct/parallel invocation faults unless the
/// binding factory was evaluated exactly once in this process.
pub fn counted_shapes_binding() -> ProviderBinding {
    COUNTED_SHAPES_FACTORIES.fetch_add(1, Ordering::SeqCst);
    ProviderBinding::new(
        "Shapes",
        SHAPES_CONTRACT_HASH,
        ["Shapes.echo"],
        Arc::new(CountedShapesProvider),
    )
}

pub fn mismatched_shapes_binding() -> ProviderBinding {
    ProviderBinding::new(
        "Shapes",
        "sha256:0000000000000000000000000000000000000000000000000000000000000000",
        ["Shapes.echo"],
        Arc::new(ShapesProvider),
    )
}

pub fn not_a_binding() -> usize {
    0
}

struct FixedTimeProvider;

impl CapabilityProvider for FixedTimeProvider {
    fn identity(&self) -> &str {
        "example.fixed-time@1"
    }

    fn fingerprint(&self) -> &str {
        "fixed-time-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        _args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match context.operation.as_str() {
            "Time.now" => Ok(ProviderValue::String("fixed-time".to_string())),
            "Time.unixMs" => Ok(ProviderValue::Int(0.into())),
            "Time.sleep" => Ok(ProviderValue::Unit),
            other => Err(ProviderFault::new("bad_call", other)),
        }
    }
}

pub fn fixed_time_binding() -> ProviderBinding {
    ProviderBinding::new(
        "Time",
        TIME_CONTRACT_HASH,
        ["Time.now", "Time.sleep", "Time.unixMs"],
        Arc::new(FixedTimeProvider),
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
