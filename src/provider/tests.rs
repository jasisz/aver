use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use aver_rt::provider::{
    CapabilityProvider, ProviderContext, ProviderFault, ProviderResource, ProviderValue,
};

use super::{
    CapabilityResourceHandle, CapabilityTarget, CapabilityTargetManifest, HostBindingReason,
    ProviderBinding, ProviderRegistry, TargetBindingStatus, UnsupportedReason,
};
use crate::capability::CapabilityRegistry;
use crate::codegen::wasip2::{CapabilityWitTypePosition, CapabilityWitUnsupported};
use crate::replay::{CapabilityProvenance, EffectRecord, JsonValue, RecordedOutcome};
use crate::value::Value;

fn contracts(module: &str, source: &str) -> CapabilityRegistry {
    let items = crate::source::parse_source(source).expect("parse capability fixture");
    let (registry, errors) = CapabilityRegistry::from_module(module, &items);
    assert!(errors.is_empty(), "capability errors: {errors:?}");
    registry
}

fn binding(
    registry: &CapabilityRegistry,
    module: &str,
    operations: &[&str],
    provider: Arc<dyn CapabilityProvider>,
) -> ProviderBinding {
    ProviderBinding::new(
        module,
        registry
            .contract(module)
            .expect("fixture contract")
            .contract_hash
            .clone(),
        operations.iter().copied(),
        provider,
    )
}

struct FixedProvider {
    identity: &'static str,
    fingerprint: &'static str,
    calls: Arc<AtomicUsize>,
    value: ProviderValue,
}

impl CapabilityProvider for FixedProvider {
    fn identity(&self) -> &str {
        self.identity
    }

    fn fingerprint(&self) -> &str {
        self.fingerprint
    }

    fn invoke(
        &self,
        _context: &ProviderContext,
        _args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        Ok(self.value.clone())
    }
}

struct FaultProvider;

impl CapabilityProvider for FaultProvider {
    fn identity(&self) -> &str {
        "test/fault"
    }

    fn fingerprint(&self) -> &str {
        "fault-v1"
    }

    fn invoke(
        &self,
        _context: &ProviderContext,
        _args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        Err(ProviderFault::new("offline", "host transport failed"))
    }
}

struct PanicProvider;

impl CapabilityProvider for PanicProvider {
    fn identity(&self) -> &str {
        "test/panic"
    }

    fn fingerprint(&self) -> &str {
        "panic-v1"
    }

    fn invoke(
        &self,
        _context: &ProviderContext,
        _args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        panic!("provider exploded")
    }
}

struct EchoProvider;

impl CapabilityProvider for EchoProvider {
    fn identity(&self) -> &str {
        "test/echo"
    }

    fn fingerprint(&self) -> &str {
        "echo-v1"
    }

    fn invoke(
        &self,
        _context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        let [value] = args else {
            return Err(ProviderFault::new("bad_args", "expected one value"));
        };
        Ok(value.clone())
    }
}

struct MapOrderProvider {
    seen: Arc<Mutex<Vec<u64>>>,
}

impl CapabilityProvider for MapOrderProvider {
    fn identity(&self) -> &str {
        "test/map-order"
    }

    fn fingerprint(&self) -> &str {
        "map-order-v1"
    }

    fn invoke(
        &self,
        _context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        let [ProviderValue::Map(entries)] = args else {
            return Err(ProviderFault::new("bad_args", "expected one Map"));
        };
        let bits = entries
            .iter()
            .map(|(key, _)| match key {
                ProviderValue::Float(value) => Ok(value.to_bits()),
                _ => Err(ProviderFault::new("bad_key", "expected Float key")),
            })
            .collect::<Result<Vec<_>, _>>()?;
        *self.seen.lock().expect("map order capture") = bits;
        Ok(ProviderValue::Int((entries.len() as i64).into()))
    }
}

const PURE: &str = "\
module Probe
    kind = capability
    semantics = pure
    exposes [read]

operation read() -> Result<Int, String>
";

#[test]
fn registration_is_exact_complete_and_explicit() {
    let registry = contracts("Probe", PURE);
    let calls = Arc::new(AtomicUsize::new(0));
    let provider = Arc::new(FixedProvider {
        identity: "test/fixed",
        fingerprint: "fixed-v1",
        calls,
        value: ProviderValue::ResultOk(Box::new(ProviderValue::Int(7.into()))),
    });

    let mut providers = ProviderRegistry::for_contracts(registry.clone());
    let mismatch = ProviderBinding::new("Probe", "sha256:wrong", ["Probe.read"], provider.clone());
    let error = providers.bind(mismatch).expect_err("hash mismatch");
    assert!(error.contains("error[capability-provider-mismatch]"));
    assert!(error.contains("test/fixed") && error.contains("sha256:wrong"));

    let incomplete = binding(&registry, "Probe", &[], provider.clone());
    assert!(
        providers
            .bind(incomplete)
            .expect_err("incomplete binding")
            .contains("Probe.read")
    );

    let extra = binding(
        &registry,
        "Probe",
        &["Probe.read", "Probe.extra"],
        provider.clone(),
    );
    assert!(
        providers
            .bind(extra)
            .expect_err("extra operation")
            .contains("Probe.extra")
    );

    let incomplete_and_extra = binding(&registry, "Probe", &["Probe.extra"], provider.clone());
    let error = providers
        .bind(incomplete_and_extra)
        .expect_err("missing and extra operations");
    assert!(error.contains("missing: Probe.read"));
    assert!(error.contains("unknown: Probe.extra"));

    providers
        .bind(binding(
            &registry,
            "Probe",
            &["Probe.read"],
            provider.clone(),
        ))
        .expect("exact binding");
    assert!(
        providers
            .bind(binding(
                &registry,
                "Probe",
                &["Probe.read"],
                provider.clone(),
            ))
            .expect_err("duplicate binding")
            .contains("error[capability-provider-duplicate]")
    );
    providers
        .replace_binding(binding(&registry, "Probe", &["Probe.read"], provider))
        .expect("explicit replacement");
}

#[test]
fn program_registry_overlays_one_checked_host_binding_and_rejects_duplicates() {
    let registry = contracts("Probe", PURE);
    let provider = Arc::new(FixedProvider {
        identity: "test/host-overlay",
        fingerprint: "host-overlay-v1",
        calls: Arc::new(AtomicUsize::new(0)),
        value: ProviderValue::ResultOk(Box::new(ProviderValue::Int(7.into()))),
    });
    let supplied = binding(&registry, "Probe", &["Probe.read"], provider.clone());
    let providers =
        ProviderRegistry::for_program_with_bindings(registry.clone(), [supplied.clone()])
            .expect("checked host overlay");
    assert_eq!(
        providers
            .binding("Probe")
            .expect("installed host binding")
            .provider_identity(),
        "test/host-overlay"
    );

    let error = ProviderRegistry::for_program_with_bindings(registry, [supplied.clone(), supplied])
        .err()
        .expect("duplicate host bindings");
    assert!(error.contains("error[capability-provider-duplicate]"));
}

#[test]
fn result_err_is_data_but_fault_panic_and_wrong_shape_are_boundary_errors() {
    let registry = contracts("Probe", PURE);
    let operation = registry.operation("Probe.read").expect("read operation");

    let mut providers = ProviderRegistry::for_contracts(registry.clone());
    providers
        .bind(binding(
            &registry,
            "Probe",
            &["Probe.read"],
            Arc::new(FixedProvider {
                identity: "test/err-data",
                fingerprint: "err-v1",
                calls: Arc::new(AtomicUsize::new(0)),
                value: ProviderValue::ResultErr(Box::new(ProviderValue::String(
                    "declared".to_string(),
                ))),
            }),
        ))
        .expect("bind Result.Err provider");
    assert_eq!(
        providers
            .invoke(operation, &[])
            .expect("Result.Err is data"),
        Value::Err(Box::new(Value::Str("declared".to_string())))
    );

    providers.unbind("Probe");
    providers
        .bind(binding(
            &registry,
            "Probe",
            &["Probe.read"],
            Arc::new(FaultProvider),
        ))
        .expect("bind fault provider");
    assert!(
        providers
            .invoke(operation, &[])
            .expect_err("provider fault")
            .contains("error[capability-provider-fault]")
    );

    providers.unbind("Probe");
    providers
        .bind(binding(
            &registry,
            "Probe",
            &["Probe.read"],
            Arc::new(PanicProvider),
        ))
        .expect("bind panic provider");
    assert!(
        providers
            .invoke(operation, &[])
            .expect_err("provider panic")
            .contains("error[capability-provider-panic]")
    );

    providers.unbind("Probe");
    providers
        .bind(binding(
            &registry,
            "Probe",
            &["Probe.read"],
            Arc::new(FixedProvider {
                identity: "test/wrong-shape",
                fingerprint: "wrong-v1",
                calls: Arc::new(AtomicUsize::new(0)),
                value: ProviderValue::String("not a Result".to_string()),
            }),
        ))
        .expect("bind wrong-shape provider");
    let error = providers.invoke(operation, &[]).expect_err("wrong shape");
    assert!(error.contains("error[capability-provider-invalid-return]"));
    assert!(error.contains("expected Result<Int, String>, received String"));
    assert!(
        !error.contains("not a Result"),
        "provider data leaked: {error}"
    );
}

#[test]
fn closed_provider_value_tree_round_trips_every_represented_boundary_shape() {
    const SHAPES: &str = "\
module Shapes
    kind = capability
    semantics = pure
    exposes [echo]

record Bundle
    number: Int
    ratio: Float
    text: String
    flag: Bool
    nothing: Unit
    pair: Tuple<Int, String>
    items: List<Int>
    vector: Vector<String>
    index: Map<String, Int>
    maybe: Option<Bool>
    outcome: Result<Int, String>
    state: State

type State
    Ready(Int)
    Empty

operation echo(value: Bundle) -> Bundle
";
    let registry = contracts("Shapes", SHAPES);
    let mut providers = ProviderRegistry::for_contracts(registry.clone());
    providers
        .bind(binding(
            &registry,
            "Shapes",
            &["Shapes.echo"],
            Arc::new(EchoProvider),
        ))
        .expect("bind shape echo");

    let mut index = std::collections::HashMap::new();
    index.insert(Value::Str("b".to_string()), Value::int(2));
    index.insert(Value::Str("a".to_string()), Value::int(1));
    let input = Value::Record {
        type_name: "Shapes.Bundle".to_string(),
        fields: vec![
            ("number".to_string(), Value::int(7)),
            ("ratio".to_string(), Value::Float(1.5)),
            ("text".to_string(), Value::Str("hello".to_string())),
            ("flag".to_string(), Value::Bool(true)),
            ("nothing".to_string(), Value::Unit),
            (
                "pair".to_string(),
                Value::Tuple(vec![Value::int(3), Value::Str("three".to_string())]),
            ),
            (
                "items".to_string(),
                crate::value::list_from_vec(vec![Value::int(1), Value::int(2)]),
            ),
            (
                "vector".to_string(),
                Value::Vector(aver_rt::AverVector::from_vec(vec![Value::Str(
                    "v".to_string(),
                )])),
            ),
            ("index".to_string(), Value::Map(index)),
            (
                "maybe".to_string(),
                Value::Some(Box::new(Value::Bool(false))),
            ),
            (
                "outcome".to_string(),
                Value::Err(Box::new(Value::Str("declared".to_string()))),
            ),
            (
                "state".to_string(),
                Value::Variant {
                    type_name: "Shapes.State".to_string(),
                    variant: "Ready".to_string(),
                    fields: vec![Value::int(9)].into(),
                },
            ),
        ]
        .into(),
    };
    let output = providers
        .invoke(
            registry.operation("Shapes.echo").unwrap(),
            std::slice::from_ref(&input),
        )
        .expect("round-trip closed boundary tree");
    assert_eq!(output, input);
}

#[test]
fn provider_maps_use_a_total_structural_order_even_for_distinct_nan_keys() {
    const MAPS: &str = "\
module Maps
    kind = capability
    semantics = pure
    exposes [inspect]

operation inspect(values: Map<Float, Int>) -> Int
";
    let registry = contracts("Maps", MAPS);
    let seen = Arc::new(Mutex::new(Vec::new()));
    let mut providers = ProviderRegistry::for_contracts(registry.clone());
    providers
        .bind(binding(
            &registry,
            "Maps",
            &["Maps.inspect"],
            Arc::new(MapOrderProvider { seen: seen.clone() }),
        ))
        .expect("bind map inspector");

    let first_bits = 0x7ff8_0000_0000_0001;
    let second_bits = 0x7ff8_0000_0000_0002;
    let mut values = std::collections::HashMap::new();
    values.insert(Value::Float(f64::from_bits(second_bits)), Value::int(2));
    values.insert(Value::Float(f64::from_bits(first_bits)), Value::int(1));
    assert_eq!(
        providers
            .invoke(
                registry.operation("Maps.inspect").unwrap(),
                &[Value::Map(values)],
            )
            .expect("inspect deterministic Map"),
        Value::int(2)
    );
    assert_eq!(
        *seen.lock().expect("captured map order"),
        [first_bits, second_bits]
    );
}

#[test]
fn provider_float_boundary_preserves_non_finite_bit_patterns() {
    const FLOATS: &str = "\
module Floats
    kind = capability
    semantics = pure
    exposes [read]

operation read() -> Float
";
    let registry = contracts("Floats", FLOATS);
    let nan_bits = 0x7ff8_0000_0000_0042;
    let mut providers = ProviderRegistry::for_contracts(registry.clone());
    providers
        .bind(binding(
            &registry,
            "Floats",
            &["Floats.read"],
            Arc::new(FixedProvider {
                identity: "test/nan",
                fingerprint: "nan-v1",
                calls: Arc::new(AtomicUsize::new(0)),
                value: ProviderValue::Float(f64::from_bits(nan_bits)),
            }),
        ))
        .expect("bind Float provider");
    let Value::Float(value) = providers
        .invoke(registry.operation("Floats.read").unwrap(), &[])
        .expect("Float boundary accepts every f64 bit pattern")
    else {
        panic!("expected Float")
    };
    assert_eq!(value.to_bits(), nan_bits);
}

#[test]
fn provider_named_values_require_canonical_type_names() {
    const NAMED: &str = "\
module Named
    kind = capability
    semantics = pure
    exposes [make]

record Box
    value: Int

operation make() -> Box
";
    let registry = contracts("Named", NAMED);
    let mut providers = ProviderRegistry::for_contracts(registry.clone());
    providers
        .bind(binding(
            &registry,
            "Named",
            &["Named.make"],
            Arc::new(FixedProvider {
                identity: "test/bare-record",
                fingerprint: "bare-v1",
                calls: Arc::new(AtomicUsize::new(0)),
                value: ProviderValue::Record {
                    type_name: "Box".to_string(),
                    fields: vec![("value".to_string(), ProviderValue::Int(1.into()))],
                },
            }),
        ))
        .expect("bind bare-name provider");
    let error = providers
        .invoke(registry.operation("Named.make").unwrap(), &[])
        .expect_err("provider type names are canonical boundary identities");
    assert!(error.contains("expected represented boundary type 'Named.Box'"));
    assert!(error.contains("received record Box"));
}

struct ResourceProvider;

impl CapabilityProvider for ResourceProvider {
    fn identity(&self) -> &str {
        "test/resource"
    }

    fn fingerprint(&self) -> &str {
        "resource-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match context.operation.as_str() {
            "Vault.open" => Ok(ProviderValue::ResultOk(Box::new(ProviderValue::Resource(
                ProviderResource::new(41_u64),
            )))),
            "Vault.read" | "Vault.readOther" => {
                let [ProviderValue::Resource(resource)] = args else {
                    return Err(ProviderFault::new("bad_args", "expected resource"));
                };
                let payload = resource
                    .downcast_ref::<u64>()
                    .copied()
                    .ok_or_else(|| ProviderFault::new("bad_resource", "wrong payload"))?;
                Ok(ProviderValue::ResultOk(Box::new(ProviderValue::Int(
                    (payload as i64).into(),
                ))))
            }
            other => Err(ProviderFault::new("unknown", other)),
        }
    }
}

const RESOURCE: &str = "\
module Vault
    kind = capability
    semantics = effectful
    exposes [open, read, readOther]

resource Token
resource Other

operation open() -> Result<Token, String>
    oracle = generative
    replay = recorded

operation read(token: Token) -> Result<Int, String>
    oracle = generative
    replay = recorded

operation readOther(token: Other) -> Result<Int, String>
    oracle = generative
    replay = recorded
";

#[test]
fn resources_are_binding_typed_unobservable_and_trace_serializable() {
    let registry = contracts("Vault", RESOURCE);
    let mut providers = ProviderRegistry::for_contracts(registry.clone());
    providers
        .bind(binding(
            &registry,
            "Vault",
            &["Vault.open", "Vault.read", "Vault.readOther"],
            Arc::new(ResourceProvider),
        ))
        .expect("bind resource provider");

    let opened = providers
        .invoke(registry.operation("Vault.open").unwrap(), &[])
        .expect("mint resource");
    let Value::Ok(token) = opened else {
        panic!("expected Result.Ok resource")
    };
    assert_eq!(crate::value::aver_repr(&token), "<capability-resource>");
    let debug = format!("{token:?}");
    assert!(debug.contains("<opaque>"));
    assert!(!debug.contains("binding_id") && !debug.contains("generation"));
    let read = providers
        .invoke(
            registry.operation("Vault.read").unwrap(),
            &[(*token).clone()],
        )
        .expect("consume resource");
    assert_eq!(read, Value::Ok(Box::new(Value::int(41))));
    let wrong_type = providers
        .invoke(
            registry.operation("Vault.readOther").unwrap(),
            &[(*token).clone()],
        )
        .expect_err("resource cannot cross opaque types");
    assert!(wrong_type.contains("expected resource type 'Vault.Other'"));

    let mut other_binding = ProviderRegistry::for_contracts(registry.clone());
    other_binding
        .bind(binding(
            &registry,
            "Vault",
            &["Vault.open", "Vault.read", "Vault.readOther"],
            Arc::new(ResourceProvider),
        ))
        .expect("bind a distinct Vault provider instance");
    assert!(
        other_binding
            .invoke(
                registry.operation("Vault.read").unwrap(),
                &[(*token).clone()],
            )
            .expect_err("resource cannot cross provider instances")
            .contains("different provider binding")
    );

    let encoded = crate::replay::value_to_json(&token).expect("trace-local token encoding");
    let decoded = crate::replay::json_to_value(&encoded).expect("replay-only token decoding");
    assert_eq!(crate::value::aver_repr(&decoded), "<capability-resource>");
    let error = providers
        .invoke(registry.operation("Vault.read").unwrap(), &[decoded])
        .expect_err("replay token cannot enter live provider");
    assert!(error.contains("different provider binding"));

    let stale = Value::CapabilityResource(CapabilityResourceHandle::from_runtime_parts(
        providers.binding("Vault").unwrap().runtime_id(),
        "Vault.Token".to_string(),
        u64::MAX,
        1,
    ));
    assert!(
        providers
            .invoke(registry.operation("Vault.read").unwrap(), &[stale])
            .expect_err("stale token")
            .contains("is stale")
    );
}

#[test]
fn tcp_settings_replace_only_the_compiler_installed_provider() {
    let settings = aver_rt::tcp::TcpSettings::from_secs(7, 41).expect("valid settings");
    let mut standard = ProviderRegistry::standard();
    let initial_id = standard.binding("Tcp").expect("standard Tcp").runtime_id();
    standard
        .configure_standard_tcp(settings)
        .expect("configure standard Tcp");
    let configured_id = standard
        .binding("Tcp")
        .expect("configured standard Tcp")
        .runtime_id();
    assert_ne!(configured_id, initial_id);
    assert_eq!(standard.standard_tcp_settings(), settings);

    // Provider identity is descriptive, not authority. Even a host binding
    // that reuses the compiler provider's public identity must not be replaced.
    let contract = standard.contracts().contract("Tcp").expect("Tcp contract");
    let operations = standard
        .contracts()
        .operations()
        .filter(|operation| operation.module == "Tcp")
        .map(|operation| operation.canonical_name.clone())
        .collect::<Vec<_>>();
    let explicit = ProviderBinding::new(
        "Tcp",
        contract.contract_hash.clone(),
        operations,
        Arc::new(FixedProvider {
            identity: aver_rt::provider::STANDARD_TCP_NATIVE_IDENTITY,
            fingerprint: "explicit-host-v1",
            calls: Arc::new(AtomicUsize::new(0)),
            value: ProviderValue::ResultOk(Box::new(ProviderValue::Unit)),
        }),
    );
    let explicit_id = explicit.runtime_id();
    standard
        .replace_binding(explicit)
        .expect("install explicit Tcp provider");
    let newer_settings = aver_rt::tcp::TcpSettings::from_secs(9, 43).expect("valid settings");
    standard
        .configure_standard_tcp(newer_settings)
        .expect("preserve explicit provider");
    assert_eq!(
        standard
            .binding("Tcp")
            .expect("explicit Tcp provider")
            .runtime_id(),
        explicit_id
    );
    assert_eq!(standard.standard_tcp_settings(), newer_settings);
}

#[test]
fn replay_checks_live_pure_and_reissued_provider_fingerprints() {
    let registry = contracts("Probe", PURE);
    let make_registry = |fingerprint: &'static str| {
        let mut providers = ProviderRegistry::for_contracts(registry.clone());
        providers
            .bind(binding(
                &registry,
                "Probe",
                &["Probe.read"],
                Arc::new(FixedProvider {
                    identity: "test/pure",
                    fingerprint,
                    calls: Arc::new(AtomicUsize::new(0)),
                    value: ProviderValue::ResultOk(Box::new(ProviderValue::Int(1.into()))),
                }),
            ))
            .expect("bind pure provider");
        providers
    };
    let providers = make_registry("pure-v1");
    let provenance = providers.provenance();
    providers
        .validate_replay_provenance_for_operations(&provenance, &[], ["Probe.read"])
        .expect("same pure provider");
    assert!(
        make_registry("pure-v2")
            .validate_replay_provenance_for_operations(&provenance, &[], ["Probe.read"])
            .expect_err("changed pure implementation")
            .contains("live provider mismatch")
    );

    const REISSUED: &str = "\
module Log
    kind = capability
    semantics = effectful
    exposes [flush]

operation flush() -> Unit
    oracle = output
    replay = reissued
";
    let registry = contracts("Log", REISSUED);
    let mut live = ProviderRegistry::for_contracts(registry.clone());
    live.bind(binding(
        &registry,
        "Log",
        &["Log.flush"],
        Arc::new(FixedProvider {
            identity: "test/log",
            fingerprint: "log-v1",
            calls: Arc::new(AtomicUsize::new(0)),
            value: ProviderValue::Unit,
        }),
    ))
    .expect("bind reissued provider");
    let effects = vec![EffectRecord {
        seq: 1,
        effect_type: "Log.flush".to_string(),
        args: Vec::new(),
        outcome: RecordedOutcome::Value(JsonValue::Null),
        caller_fn: "main".to_string(),
        source_line: 1,
        group_id: None,
        branch_path: None,
        effect_occurrence: None,
    }];
    live.validate_replay_provenance_for_operations(&live.provenance(), &effects, ["Log.flush"])
        .expect("same reissued provider");
    let wrong = vec![CapabilityProvenance {
        fingerprint: "log-v2".to_string(),
        ..live.provenance().remove(0)
    }];
    assert!(
        live.validate_replay_provenance_for_operations(&wrong, &effects, ["Log.flush"])
            .expect_err("changed reissued implementation")
            .contains("live provider mismatch")
    );
}

#[test]
fn replay_rejects_duplicate_capability_provenance() {
    let registry = contracts("Probe", PURE);
    let mut providers = ProviderRegistry::for_contracts(registry.clone());
    providers
        .bind(binding(
            &registry,
            "Probe",
            &["Probe.read"],
            Arc::new(FixedProvider {
                identity: "test/pure",
                fingerprint: "pure-v1",
                calls: Arc::new(AtomicUsize::new(0)),
                value: ProviderValue::ResultOk(Box::new(ProviderValue::Int(1.into()))),
            }),
        ))
        .expect("bind pure provider");
    let entry = providers.provenance().remove(0);
    let error = providers
        .validate_replay_provenance_for_operations(&[entry.clone(), entry], &[], ["Probe.read"])
        .expect_err("duplicate provenance must be ambiguous");
    assert!(error.contains("duplicate capability provenance for 'Probe'"));
}

#[test]
fn unused_pure_capability_does_not_require_replay_provenance() {
    let registry = contracts("Probe", PURE);
    let providers = ProviderRegistry::for_contracts(registry);
    providers
        .validate_replay_provenance_for_operations(&[], &[], std::iter::empty())
        .expect("an unused declaration needs neither provenance nor a binding");
}

#[test]
fn replay_pins_contract_and_model_hashes_independently_of_provider_identity() {
    const V1: &str = "\
module Clock
    kind = capability
    semantics = effectful
    exposes [read]

operation read() -> Int
    oracle = generative
    replay = recorded
    hostile = [normal]

fn normal(path: BranchPath, call: Int) -> Int
    1
";
    let original = contracts("Clock", V1);
    let original_contract = original.contract("Clock").expect("Clock contract");
    let provenance = vec![CapabilityProvenance {
        capability: "Clock".to_string(),
        contract_hash: original_contract.contract_hash.clone(),
        model_hash: original_contract.model_hash.clone(),
        provider: "test/clock".to_string(),
        fingerprint: "clock-v1".to_string(),
    }];

    let changed_model = contracts("Clock", &V1.replace("    1\n", "    2\n"));
    assert_eq!(
        changed_model.contract("Clock").unwrap().contract_hash,
        original_contract.contract_hash
    );
    assert_ne!(
        changed_model.contract("Clock").unwrap().model_hash,
        original_contract.model_hash
    );
    assert!(
        ProviderRegistry::for_contracts(changed_model)
            .validate_replay_provenance_for_operations(&provenance, &[], ["Clock.read"],)
            .expect_err("changed model")
            .contains("replay model mismatch")
    );

    let changed_contract = contracts(
        "Clock",
        &V1.replace("read() -> Int", "read() -> String")
            .replace(") -> Int\n    1", ") -> String\n    \"one\""),
    );
    assert!(
        ProviderRegistry::for_contracts(changed_contract)
            .validate_replay_provenance_for_operations(&provenance, &[], ["Clock.read"],)
            .expect_err("changed contract")
            .contains("replay contract mismatch")
    );
}

#[test]
fn target_manifest_is_total_and_standard_capabilities_are_provided_everywhere() {
    let empty =
        CapabilityTargetManifest::build(&CapabilityRegistry::default(), &Default::default())
            .expect("empty manifest");
    assert!(empty.rows().is_empty());

    let registry = crate::stdlib::standard_capability_registry();
    let required = [
        "Disk.readText".to_string(),
        "Random.int".to_string(),
        "Tcp.ping".to_string(),
        "Time.now".to_string(),
    ]
    .into_iter()
    .collect();
    let manifest = CapabilityTargetManifest::build(&registry, &required).expect("manifest");
    assert_eq!(manifest.rows().len(), 16);
    for (capability, operations, required_operation, native, wasm_gc, wasip2, fingerprint) in [
        (
            "Disk",
            &[
                "Disk.appendBytes",
                "Disk.appendText",
                "Disk.delete",
                "Disk.deleteDir",
                "Disk.exists",
                "Disk.listDir",
                "Disk.makeDir",
                "Disk.readBytes",
                "Disk.readBytesAt",
                "Disk.readText",
                "Disk.size",
                "Disk.writeBytes",
                "Disk.writeText",
            ][..],
            "Disk.readText",
            "aver.standard.Disk/native",
            "aver.standard.Disk/wasm-gc-imports",
            "aver.standard.Disk/wasip2-wasi",
            aver_rt::provider::STANDARD_DISK_FINGERPRINT,
        ),
        (
            "Random",
            &["Random.float", "Random.int"][..],
            "Random.int",
            "aver.standard.Random/native",
            "aver.standard.Random/wasm-gc-imports",
            "aver.standard.Random/wasip2-wasi",
            aver_rt::provider::STANDARD_RANDOM_FINGERPRINT,
        ),
        (
            "Tcp",
            &[
                "Tcp.close",
                "Tcp.connect",
                "Tcp.ping",
                "Tcp.poll",
                "Tcp.readBytes",
                "Tcp.readLine",
                "Tcp.readSome",
                "Tcp.send",
                "Tcp.sendBytes",
                "Tcp.writeBytes",
                "Tcp.writeLine",
            ][..],
            "Tcp.ping",
            "aver.standard.Tcp/native",
            "aver.standard.Tcp/wasm-gc-imports",
            "aver.standard.Tcp/wasip2-wasi",
            aver_rt::provider::STANDARD_TCP_FINGERPRINT,
        ),
        (
            "Time",
            &["Time.now", "Time.sleep", "Time.unixMs"][..],
            "Time.now",
            "aver.standard.Time/native",
            "aver.standard.Time/wasm-gc-imports",
            "aver.standard.Time/wasip2-wasi",
            aver_rt::provider::STANDARD_TIME_FINGERPRINT,
        ),
    ] {
        let contract = registry.contract(capability).expect("standard contract");
        for (target, identity) in [
            (CapabilityTarget::Vm, native),
            (CapabilityTarget::Rust, native),
            (CapabilityTarget::WasmGc, wasm_gc),
            (CapabilityTarget::Wasip2, wasip2),
        ] {
            let row = manifest
                .for_target(target)
                .find(|row| row.capability == capability)
                .expect("target row");
            let TargetBindingStatus::Provided(provider) = &row.status else {
                panic!("{capability} must be provided on {target}");
            };
            assert_eq!(provider.identity, identity);
            assert_eq!(provider.fingerprint, fingerprint);
            assert_eq!(row.contract_hash, contract.contract_hash);
            assert_eq!(row.model_hash, contract.model_hash);
            assert_eq!(
                row.declared_operations,
                operations.iter().map(|name| (*name).to_string()).collect()
            );
            assert_eq!(
                row.required_operations,
                [required_operation.to_string()].into_iter().collect()
            );
        }
    }
    assert!(
        "unknown"
            .parse::<CapabilityTarget>()
            .expect_err("unknown target")
            .contains("error[capability-target-unknown]")
    );
}

#[test]
fn shipped_provenance_projects_only_provided_manifest_rows() {
    let mut registry = crate::stdlib::standard_capability_registry();
    registry.merge(contracts("Probe", PURE));

    for target in CapabilityTarget::ALL {
        let provenance = super::shipped_target_provenance(target, &registry);
        assert_eq!(
            provenance.len(),
            4,
            "all four standards are provided on {target}"
        );
        assert_eq!(
            provenance
                .iter()
                .map(|entry| entry.capability.as_str())
                .collect::<Vec<_>>(),
            vec!["Disk", "Random", "Tcp", "Time"]
        );
        assert_eq!(
            provenance[0].fingerprint,
            aver_rt::provider::STANDARD_DISK_FINGERPRINT
        );
        assert_eq!(
            provenance[1].fingerprint,
            aver_rt::provider::STANDARD_RANDOM_FINGERPRINT
        );
        assert_eq!(
            provenance[2].fingerprint,
            aver_rt::provider::STANDARD_TCP_FINGERPRINT
        );
        assert_eq!(
            provenance[3].fingerprint,
            aver_rt::provider::STANDARD_TIME_FINGERPRINT
        );
    }
}

#[test]
fn custom_capability_is_host_bound_on_native_targets_and_reports_unavailable_wasm_adapters() {
    let registry = contracts("Probe", PURE);
    let manifest = CapabilityTargetManifest::build(&registry, &Default::default())
        .expect("unused custom capability manifest");
    assert_eq!(manifest.rows().len(), 4);
    assert!(manifest.rows().iter().all(|row| !row.is_required()));

    for target in [CapabilityTarget::Vm, CapabilityTarget::Rust] {
        let row = manifest.for_target(target).next().expect("native row");
        assert_eq!(
            row.status,
            TargetBindingStatus::HostBound {
                reason: HostBindingReason::RuntimeProviderRequired
            }
        );
    }
    for (target, expected) in [
        (
            CapabilityTarget::WasmGc,
            UnsupportedReason::HostImportAdapterNotGenerated,
        ),
        (
            CapabilityTarget::Wasip2,
            UnsupportedReason::WitBoundaryTypeUnsupported(CapabilityWitUnsupported {
                capability: "Probe".to_string(),
                operation: "Probe.read".to_string(),
                position: CapabilityWitTypePosition::Result,
                aver_type: "Result<Int, String>".to_string(),
            }),
        ),
    ] {
        let row = manifest.for_target(target).next().expect("target row");
        assert_eq!(
            row.status,
            TargetBindingStatus::Unsupported { reason: expected }
        );
    }
}

#[test]
fn wit_lowerable_custom_capability_is_host_bound_on_wasip2() {
    let registry = contracts(
        "Echo",
        "\
module Echo
    kind = capability
    semantics = effectful
    exposes [echo]

operation echo(value: String) -> String
    oracle = generative
    replay = recorded
",
    );
    let manifest = CapabilityTargetManifest::build(&registry, &Default::default())
        .expect("lowerable custom capability manifest");
    let row = manifest
        .for_target(CapabilityTarget::Wasip2)
        .next()
        .expect("wasip2 row");
    assert_eq!(
        row.status,
        TargetBindingStatus::HostBound {
            reason: HostBindingReason::ComponentImportRequired,
        }
    );
}

#[test]
fn wrapper_alias_and_independent_product_require_the_underlying_operation_once() {
    let registry = contracts(
        "Clock",
        "\
module Clock
    kind = capability
    semantics = effectful
    exposes [now, tick]

operation now() -> Result<Int, String>
    oracle = generative
    replay = recorded

operation tick() -> Int
    oracle = generative
    replay = recorded
",
    );
    let items = crate::source::parse_source(
        "\
module Client
    depends [Clock]
    exposes [main]

fn wrapped() -> Result<Int, String>
    ! [Clock.now]
    Clock.now()

fn main() -> Result<Tuple<Int, Int>, String>
    ! [Clock.now]
    alias = wrapped
    (alias(), alias())?!
",
    )
    .expect("parse wrapper/product program");
    let required = super::required_capability_operations(&items, &[], &registry);
    assert_eq!(required, ["Clock.now".to_string()].into_iter().collect());
}

#[test]
fn standard_time_override_is_explicit_exact_and_does_not_change_contract_identity() {
    let contracts = crate::stdlib::standard_capability_registry();
    let expected = contracts.contract("Time").expect("Time contract").clone();
    let provider = Arc::new(FixedProvider {
        identity: "test/time",
        fingerprint: "time-test-v1",
        calls: Arc::new(AtomicUsize::new(0)),
        value: ProviderValue::Unit,
    });
    let mut providers = ProviderRegistry::for_program(contracts).expect("standard binding");
    let wrong = ProviderBinding::new(
        "Time",
        "sha256:wrong",
        ["Time.now", "Time.unixMs", "Time.sleep"],
        provider.clone(),
    );
    assert!(
        providers
            .replace_binding(wrong)
            .expect_err("override must pin the exact contract")
            .contains("error[capability-provider-mismatch]")
    );
    providers
        .replace_binding(ProviderBinding::new(
            "Time",
            expected.contract_hash.clone(),
            ["Time.now", "Time.unixMs", "Time.sleep"],
            provider,
        ))
        .expect("explicit exact override");
    let after = providers
        .contracts()
        .contract("Time")
        .expect("Time contract");
    assert_eq!(after.contract_hash, expected.contract_hash);
    assert_eq!(after.model_hash, expected.model_hash);
}

#[test]
fn standard_time_default_rejects_a_shadow_model_with_the_same_provider_abi() {
    let source = crate::stdlib::find("Time")
        .expect("standard Time source")
        .source;
    let changed = source.replace("2026-01-01T00:00:00Z", "2026-01-01T00:00:04Z");
    let shadow = contracts("Time", &changed);
    let canonical = crate::stdlib::standard_capability_registry();
    assert_eq!(
        shadow.contract("Time").unwrap().contract_hash,
        canonical.contract("Time").unwrap().contract_hash
    );
    assert_ne!(
        shadow.contract("Time").unwrap().model_hash,
        canonical.contract("Time").unwrap().model_hash
    );
    let error = match ProviderRegistry::for_program(shadow) {
        Ok(_) => panic!("the standard Time model identity is reserved"),
        Err(error) => error,
    };
    assert!(error.contains("reserved standard capability 'Time' has model_hash"));
}
