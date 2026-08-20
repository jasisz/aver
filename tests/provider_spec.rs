//! Public embedding and VM conformance tests for typed capability providers.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use aver::codegen::ModuleInfo;
use aver::ir::{PipelineConfig, TypecheckMode};
use aver::nan_value::{Arena, NanValueConvert};
use aver::provider::{
    CapabilityProvider, ProviderBinding, ProviderContext, ProviderFault, ProviderRegistry,
    ProviderResource, ProviderValue,
};
use aver::value::Value;
use aver::vm;

fn temp_root(tag: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!(
        "aver-provider-{tag}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("clock")
            .as_nanos()
    ));
    fs::create_dir_all(&path).expect("create provider fixture root");
    path
}

fn compile_vm(root: &Path, entry: &str) -> (vm::VM, aver::capability::CapabilityRegistry) {
    let source = fs::read_to_string(root.join(entry)).expect("read entry source");
    let mut items = aver::source::parse_source(&source).expect("parse entry source");
    let mut depends = items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::Module(module) => Some(module.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    depends.extend(aver::stdlib::implicit_stdlib_deps(&items));
    depends.sort();
    depends.dedup();
    let root_text = root.to_str().expect("UTF-8 fixture root");
    let loaded = aver::source::load_module_tree(&depends, root_text).expect("load capabilities");
    let modules: Vec<ModuleInfo> = loaded.iter().map(ModuleInfo::from_loaded).collect();
    let pipeline = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(root_text),
            }),
            dep_modules: &modules,
            ..Default::default()
        },
    );
    let typecheck = pipeline.typecheck.expect("typecheck result");
    assert!(
        typecheck.errors.is_empty(),
        "provider fixture type errors: {:?}",
        typecheck.errors
    );
    let contracts = typecheck.capabilities.clone();

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_modules(
        &pipeline.resolved_items,
        &pipeline.symbol_table,
        &mut arena,
        Some(root_text),
        entry,
        pipeline.analysis.as_ref(),
    )
    .expect("compile provider fixture");
    (vm::VM::new(code, globals, arena), contracts)
}

const CLOCK: &str = "\
module Clock
    kind = capability
    semantics = effectful
    exposes [now]

operation now() -> Int
    oracle = generative
    replay = recorded
";

fn bind_clock(
    contracts: &aver::capability::CapabilityRegistry,
    calls: Arc<AtomicUsize>,
    fingerprint: &'static str,
) -> ProviderRegistry {
    let mut providers = ProviderRegistry::for_program(contracts.clone()).expect("program registry");
    providers
        .bind(native_provider_fixture::clock_binding(calls, fingerprint))
        .expect("bind Clock");
    providers
}

#[test]
fn external_embedder_installs_one_provider_shared_by_direct_and_parallel_calls() {
    let root = temp_root("public-parallel");
    fs::write(root.join("Clock.av"), CLOCK).expect("write Clock");
    fs::write(
        root.join("main.av"),
        "\
module Client
    depends [Clock]
    exposes [main]
    effects [Clock.now]

fn main() -> Tuple<Int, Tuple<Int, Int>>
    ! [Clock.now]
    first = Clock.now()
    pair = (Clock.now(), Clock.now())!
    (first, pair)
",
    )
    .expect("write entry");

    let (mut machine, contracts) = compile_vm(&root, "main.av");
    let calls = Arc::new(AtomicUsize::new(0));
    let before = contracts.contract("Clock").expect("Clock contract").clone();
    let providers = bind_clock(&contracts, calls.clone(), "counter-v1");
    let after = providers
        .contracts()
        .contract("Clock")
        .expect("bound Clock contract");
    assert_eq!(after.contract_hash, before.contract_hash);
    assert_eq!(after.model_hash, before.model_hash);
    machine.set_provider_registry(Arc::new(providers));
    let result = machine.run().expect("provider-backed VM run");
    let Value::Tuple(outer) = result.to_value(&machine.arena) else {
        panic!("expected outer tuple")
    };
    assert_eq!(outer[0], Value::int(0), "direct call must happen first");
    let Value::Tuple(pair) = &outer[1] else {
        panic!("expected parallel pair")
    };
    let mut branch_values = pair
        .iter()
        .map(|value| match value {
            Value::Int(value) => value.to_i64().expect("small provider result"),
            other => panic!("expected Int, got {other:?}"),
        })
        .collect::<Vec<_>>();
    branch_values.sort();
    assert_eq!(branch_values, vec![1, 2]);
    assert_eq!(calls.load(Ordering::SeqCst), 3, "exactly-once dispatch");
}

#[test]
fn vm_preflight_rejects_a_registry_built_for_a_different_contract() {
    let root = temp_root("compiled-contract-pin");
    fs::write(root.join("Clock.av"), CLOCK).expect("write Clock");
    fs::write(
        root.join("main.av"),
        "\
module Client
    depends [Clock]
    exposes [main]
    effects [Clock.now]

fn main() -> Int
    ! [Clock.now]
    Clock.now()
",
    )
    .expect("write entry");
    let (mut machine, expected_contracts) = compile_vm(&root, "main.av");

    let other = temp_root("different-contract");
    fs::write(
        other.join("Clock.av"),
        CLOCK.replace("operation now() -> Int", "operation now() -> String"),
    )
    .expect("write incompatible Clock");
    fs::write(
        other.join("main.av"),
        "\
module OtherClient
    depends [Clock]
    exposes [main]
    effects [Clock.now]

fn main() -> String
    ! [Clock.now]
    Clock.now()
",
    )
    .expect("write incompatible entry");
    let (_, other_contracts) = compile_vm(&other, "main.av");
    assert_ne!(
        expected_contracts
            .contract("Clock")
            .expect("expected Clock")
            .contract_hash,
        other_contracts
            .contract("Clock")
            .expect("other Clock")
            .contract_hash
    );

    let calls = Arc::new(AtomicUsize::new(0));
    let mut other_providers = ProviderRegistry::for_program(other_contracts.clone())
        .expect("incompatible program registry");
    other_providers
        .bind(native_provider_fixture::clock_binding_for_contract(
            other_contracts
                .contract("Clock")
                .expect("other Clock")
                .contract_hash
                .clone(),
            calls.clone(),
            "other-v1",
        ))
        .expect("binding is valid for the incompatible registry");
    machine.set_provider_registry(Arc::new(other_providers));
    let error = machine
        .run()
        .expect_err("bytecode must pin its checked contract");
    let message = error.to_string();
    assert!(message.contains("error[capability-provider-mismatch]"));
    assert!(message.contains("supplied contract_hash") && message.contains("expected sha256:"));
    assert_eq!(
        calls.load(Ordering::SeqCst),
        0,
        "preflight runs before code"
    );

    let changed_model = temp_root("different-model");
    let changed_model_source = format!(
        "{}\nfn normal(path: BranchPath, call: Int) -> Int\n    0\n",
        CLOCK.replace(
            "replay = recorded",
            "replay = recorded\n    hostile = [normal]"
        )
    );
    fs::write(changed_model.join("Clock.av"), changed_model_source)
        .expect("write model-changed Clock");
    fs::write(
        changed_model.join("main.av"),
        fs::read_to_string(root.join("main.av")).expect("read original entry"),
    )
    .expect("write model-changed entry");
    let (_, changed_model_contracts) = compile_vm(&changed_model, "main.av");
    let expected = expected_contracts
        .contract("Clock")
        .expect("expected Clock");
    let changed = changed_model_contracts
        .contract("Clock")
        .expect("model-changed Clock");
    assert_eq!(expected.contract_hash, changed.contract_hash);
    assert_ne!(expected.model_hash, changed.model_hash);

    let (mut model_machine, _) = compile_vm(&root, "main.av");
    model_machine.set_provider_registry(Arc::new(bind_clock(
        &changed_model_contracts,
        Arc::new(AtomicUsize::new(0)),
        "model-v2",
    )));
    let error = model_machine
        .run()
        .expect_err("bytecode must also pin its checked replay model");
    assert!(error.to_string().contains("supplied model_hash"));
}

struct ResultCounterProvider {
    calls: Arc<AtomicUsize>,
}

impl CapabilityProvider for ResultCounterProvider {
    fn identity(&self) -> &str {
        "example.result-counter@1"
    }

    fn fingerprint(&self) -> &str {
        "result-counter-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        if context.operation != "Source.read" || !args.is_empty() {
            return Err(ProviderFault::new("bad_call", &context.operation));
        }
        Ok(ProviderValue::ResultOk(Box::new(ProviderValue::Int(
            (self.calls.fetch_add(1, Ordering::SeqCst) as i64).into(),
        ))))
    }
}

#[test]
fn result_provider_dispatches_once_through_alias_wrapper_and_question_bang_product() {
    let root = temp_root("public-result-parallel");
    fs::write(
        root.join("Source.av"),
        "\
module Source
    kind = capability
    semantics = effectful
    exposes [read]

operation read() -> Result<Int, String>
    oracle = generative
    replay = recorded
",
    )
    .expect("write Source");
    fs::write(
        root.join("main.av"),
        "\
module Client
    depends [Source]
    exposes [main]
    effects [Source.read]

fn wrapped() -> Result<Int, String>
    ! [Source.read]
    Source.read()

fn through(reader: Fn() -> Result<Int, String> ! [Source.read]) -> Result<Int, String>
    ! [Source.read]
    reader()

fn main() -> Result<Tuple<Int, Int>, String>
    ! [Source.read]
    Result.Ok((through(wrapped), wrapped())?!)
",
    )
    .expect("write entry");

    let (mut machine, contracts) = compile_vm(&root, "main.av");
    let contract = contracts.contract("Source").expect("Source contract");
    let calls = Arc::new(AtomicUsize::new(0));
    let mut providers = ProviderRegistry::for_program(contracts.clone()).expect("registry");
    providers
        .bind(ProviderBinding::new(
            "Source",
            contract.contract_hash.clone(),
            ["Source.read"],
            Arc::new(ResultCounterProvider {
                calls: calls.clone(),
            }),
        ))
        .expect("bind Source");
    machine.set_provider_registry(Arc::new(providers));

    let result = machine.run().expect("provider-backed ?! run");
    let Value::Ok(tuple) = result.to_value(&machine.arena) else {
        panic!("expected Result.Ok tuple")
    };
    let Value::Tuple(values) = *tuple else {
        panic!("expected tuple payload")
    };
    let mut values = values
        .into_iter()
        .map(|value| match value {
            Value::Int(value) => value.to_i64().expect("small result"),
            other => panic!("expected Int, got {other:?}"),
        })
        .collect::<Vec<_>>();
    values.sort();
    assert_eq!(values, vec![0, 1]);
    assert_eq!(calls.load(Ordering::SeqCst), 2, "exactly once per branch");
}

#[test]
fn recorded_replay_consumes_custom_capability_without_a_live_provider() {
    let root = temp_root("recorded-replay");
    fs::write(root.join("Clock.av"), CLOCK).expect("write Clock");
    fs::write(
        root.join("main.av"),
        "\
module Client
    depends [Clock]
    exposes [main]
    effects [Clock.now]

fn main() -> Int
    ! [Clock.now]
    Clock.now()
",
    )
    .expect("write entry");

    let (mut recording_vm, contracts) = compile_vm(&root, "main.av");
    let calls = Arc::new(AtomicUsize::new(0));
    let live = bind_clock(&contracts, calls.clone(), "counter-v1");
    let provenance = live.provenance();
    recording_vm.set_provider_registry(Arc::new(live));
    recording_vm.start_recording();
    let recorded_value = recording_vm.run().expect("record provider call");
    assert_eq!(recorded_value.to_value(&recording_vm.arena), Value::int(0));
    assert_eq!(calls.load(Ordering::SeqCst), 1);
    let effects = recording_vm.recorded_effects().to_vec();
    assert_eq!(effects.len(), 1);

    let (mut replay_vm, replay_contracts) = compile_vm(&root, "main.av");
    let no_custom_provider = ProviderRegistry::for_program(replay_contracts).expect("registry");
    replay_vm.set_provider_registry(Arc::new(no_custom_provider));
    replay_vm
        .start_replay_with_provenance(effects, &provenance, true)
        .expect("recorded replay provenance");
    let replayed = replay_vm.run().expect("replay without live Clock");
    assert_eq!(replayed.to_value(&replay_vm.arena), Value::int(0));
    replay_vm
        .ensure_replay_consumed()
        .expect("transcript consumed");

    let (mut legacy_vm, legacy_contracts) = compile_vm(&root, "main.av");
    legacy_vm.set_provider_registry(Arc::new(
        ProviderRegistry::for_program(legacy_contracts).expect("legacy registry"),
    ));
    legacy_vm.start_replay(recording_vm.recorded_effects().to_vec(), true);
    let error = legacy_vm
        .run()
        .expect_err("custom capability replay without provenance must fail");
    assert!(
        error
            .to_string()
            .contains("no capability contract/model provenance")
    );
}

#[test]
fn removing_standard_time_binding_proves_there_is_no_legacy_vm_bypass() {
    let root = temp_root("time-fault-injection");
    fs::write(
        root.join("main.av"),
        "\
module Client
    exposes [main]
    effects [Time.now]

fn main() -> String
    ! [Time.now]
    Time.now()
",
    )
    .expect("write entry");

    let (mut machine, contracts) = compile_vm(&root, "main.av");
    let mut providers = ProviderRegistry::for_program(contracts).expect("standard registry");
    providers.unbind("Time");
    machine.set_provider_registry(Arc::new(providers));
    let error = machine.run().expect_err("Time must not bypass registry");
    let message = error.to_string();
    assert!(message.contains("error[capability-provider-missing]"));
    assert!(message.contains("Time.now") && message.contains("contract_hash sha256:"));
}

#[test]
fn standard_random_uses_the_default_provider_and_cannot_bypass_it() {
    let root = temp_root("random-fault-injection");
    fs::write(
        root.join("main.av"),
        "\
module Client
    exposes [main]
    effects [Random.int, Random.float]

fn main() -> Tuple<Int, Float>
    ! [Random.int, Random.float]
    (Random.int(4, 4), Random.float())
",
    )
    .expect("write entry");

    let (mut machine, _contracts) = compile_vm(&root, "main.av");
    let value = machine
        .run()
        .expect("default Random provider")
        .to_value(&machine.arena);
    let Value::Tuple(values) = value else {
        panic!("standard Random provider must return the tuple")
    };
    assert_eq!(values[0], Value::int(4));
    let Value::Float(float) = &values[1] else {
        panic!("Random.float must return Float")
    };
    assert!((0.0..1.0).contains(float));

    let (mut machine, contracts) = compile_vm(&root, "main.av");
    let mut providers = ProviderRegistry::for_program(contracts).expect("standard registry");
    providers.unbind("Random");
    machine.set_provider_registry(Arc::new(providers));
    let error = machine.run().expect_err("Random must not bypass registry");
    let message = error.to_string();
    assert!(message.contains("error[capability-provider-missing]"));
    assert!(message.contains("Random.") && message.contains("contract_hash sha256:"));
}

struct CountingRandomProvider {
    calls: Arc<AtomicUsize>,
}

impl CapabilityProvider for CountingRandomProvider {
    fn identity(&self) -> &str {
        "test.random@1"
    }

    fn fingerprint(&self) -> &str {
        "fixed-random-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        _args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        match context.operation.as_str() {
            "Random.int" => Ok(ProviderValue::Int(4.into())),
            "Random.float" => Ok(ProviderValue::Float(0.5)),
            operation => Err(ProviderFault::new("bad_call", operation)),
        }
    }
}

fn replace_random_provider(
    contracts: &aver::capability::CapabilityRegistry,
    calls: Arc<AtomicUsize>,
) -> ProviderRegistry {
    let contract = contracts.contract("Random").expect("Random contract");
    let mut providers =
        ProviderRegistry::for_program(contracts.clone()).expect("standard registry");
    providers
        .replace_binding(ProviderBinding::new(
            "Random",
            contract.contract_hash.clone(),
            ["Random.int", "Random.float"],
            Arc::new(CountingRandomProvider { calls }),
        ))
        .expect("replace Random through the explicit exact-contract API");
    providers
}

#[test]
fn standard_random_recording_uses_an_explicit_override_and_replay_stays_offline() {
    let root = temp_root("random-recorded-replay");
    fs::write(
        root.join("main.av"),
        "\
module Client
    exposes [main]
    effects [Random.int]

fn main() -> Int
    ! [Random.int]
    Random.int(1, 6)
",
    )
    .expect("write entry");

    let (mut recording_vm, contracts) = compile_vm(&root, "main.av");
    let recording_calls = Arc::new(AtomicUsize::new(0));
    let providers = replace_random_provider(&contracts, recording_calls.clone());
    let provenance = providers.provenance();
    recording_vm.set_provider_registry(Arc::new(providers));
    recording_vm.start_recording();
    let recorded = recording_vm.run().expect("record Random provider call");
    assert_eq!(recorded.to_value(&recording_vm.arena), Value::int(4));
    assert_eq!(recording_calls.load(Ordering::SeqCst), 1);
    let effects = recording_vm.recorded_effects().to_vec();
    assert_eq!(effects.len(), 1);

    let (mut replay_vm, replay_contracts) = compile_vm(&root, "main.av");
    let replay_calls = Arc::new(AtomicUsize::new(0));
    replay_vm.set_provider_registry(Arc::new(replace_random_provider(
        &replay_contracts,
        replay_calls.clone(),
    )));
    replay_vm
        .start_replay_with_provenance(effects, &provenance, true)
        .expect("Random replay provenance");
    let replayed = replay_vm.run().expect("offline Random replay");
    assert_eq!(replayed.to_value(&replay_vm.arena), Value::int(4));
    assert_eq!(replay_calls.load(Ordering::SeqCst), 0);
    replay_vm
        .ensure_replay_consumed()
        .expect("Random transcript consumed");
}

#[test]
fn standard_disk_uses_the_default_provider_and_cannot_bypass_it() {
    // Disk is the falsifier for the capability bet (#864): the first
    // standard service whose builtin path was privileged to move onto
    // providers. The default native provider must serve real file IO,
    // and unbinding must fail closed with the contract identity.
    let root = temp_root("disk-default-provider");
    let file = root.join("data.txt");
    fs::write(
        root.join("main.av"),
        format!(
            "\
module Client
    exposes [main]
    effects [Disk.writeText, Disk.readText, Disk.exists]

fn main() -> Result<String, String>
    ! [Disk.writeText, Disk.readText, Disk.exists]
    written = Disk.writeText({path:?}, \"payload\")?
    present = Disk.exists({path:?})
    text = Disk.readText({path:?})?
    Result.Ok(\"{{text}} ({{present}})\")
",
            path = file.to_string_lossy()
        ),
    )
    .expect("write entry");

    let (mut machine, _contracts) = compile_vm(&root, "main.av");
    let value = machine
        .run()
        .expect("default Disk provider")
        .to_value(&machine.arena);
    assert_eq!(
        value,
        Value::Ok(Box::new(Value::Str("payload (true)".to_string())))
    );

    let (mut machine, contracts) = compile_vm(&root, "main.av");
    let mut providers = ProviderRegistry::for_program(contracts).expect("standard registry");
    providers.unbind("Disk");
    machine.set_provider_registry(Arc::new(providers));
    let error = machine.run().expect_err("Disk must not bypass registry");
    let message = error.to_string();
    assert!(message.contains("error[capability-provider-missing]"));
    assert!(message.contains("Disk.") && message.contains("contract_hash sha256:"));
}

#[test]
fn standard_disk_recording_replays_offline_after_the_file_disappears() {
    // The Disk-specific proof of offline replay: the recorded read is
    // served from the transcript after the file itself is gone. A live
    // provider dispatch would return Err("..."), so passing this test
    // means the replay door really did not touch the filesystem.
    let root = temp_root("disk-recorded-replay");
    let file = root.join("vanishing.txt");
    fs::write(&file, "recorded payload").expect("seed file");
    fs::write(
        root.join("main.av"),
        format!(
            "\
module Client
    exposes [main]
    effects [Disk.readText]

fn main() -> Result<String, String>
    ! [Disk.readText]
    Disk.readText({path:?})
",
            path = file.to_string_lossy()
        ),
    )
    .expect("write entry");

    let (mut recording_vm, contracts) = compile_vm(&root, "main.av");
    let providers = ProviderRegistry::for_program(contracts.clone()).expect("standard registry");
    let provenance = providers.provenance();
    recording_vm.set_provider_registry(Arc::new(providers));
    recording_vm.start_recording();
    let recorded = recording_vm.run().expect("record Disk read");
    assert_eq!(
        recorded.to_value(&recording_vm.arena),
        Value::Ok(Box::new(Value::Str("recorded payload".to_string())))
    );
    let effects = recording_vm.recorded_effects().to_vec();
    assert_eq!(effects.len(), 1);

    // The file is gone; only the transcript can answer now.
    fs::remove_file(&file).expect("remove the recorded file");

    let (mut replay_vm, replay_contracts) = compile_vm(&root, "main.av");
    let replay_providers =
        ProviderRegistry::for_program(replay_contracts).expect("standard registry");
    replay_vm.set_provider_registry(Arc::new(replay_providers));
    replay_vm
        .start_replay_with_provenance(effects, &provenance, true)
        .expect("Disk replay provenance");
    let replayed = replay_vm.run().expect("offline Disk replay");
    assert_eq!(
        replayed.to_value(&replay_vm.arena),
        Value::Ok(Box::new(Value::Str("recorded payload".to_string())))
    );
    replay_vm
        .ensure_replay_consumed()
        .expect("Disk transcript consumed");
}

struct PureCounterProvider {
    calls: Arc<AtomicUsize>,
    fingerprint: &'static str,
}

impl CapabilityProvider for PureCounterProvider {
    fn identity(&self) -> &str {
        "example.pure-counter@1"
    }

    fn fingerprint(&self) -> &str {
        self.fingerprint
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        if context.operation != "Counter.next" || !args.is_empty() {
            return Err(ProviderFault::new("bad_call", &context.operation));
        }
        Ok(ProviderValue::Int(
            (self.calls.fetch_add(1, Ordering::SeqCst) as i64).into(),
        ))
    }
}

fn bind_pure_counter(
    contracts: &aver::capability::CapabilityRegistry,
    calls: Arc<AtomicUsize>,
    fingerprint: &'static str,
) -> ProviderRegistry {
    let mut providers = ProviderRegistry::for_program(contracts.clone()).expect("program registry");
    let contract = contracts.contract("Counter").expect("Counter contract");
    providers
        .bind(ProviderBinding::new(
            "Counter",
            contract.contract_hash.clone(),
            ["Counter.next"],
            Arc::new(PureCounterProvider { calls, fingerprint }),
        ))
        .expect("bind Counter");
    providers
}

#[test]
fn pure_provider_repeats_live_without_effect_events_and_pins_replay_fingerprint() {
    let root = temp_root("pure-replay");
    fs::write(
        root.join("Counter.av"),
        "\
module Counter
    kind = capability
    semantics = pure
    exposes [next]

operation next() -> Int
",
    )
    .expect("write Counter");
    fs::write(
        root.join("main.av"),
        "\
module Client
    depends [Counter]
    exposes [main]

fn main() -> Tuple<Int, Int>
    (Counter.next(), Counter.next())
",
    )
    .expect("write entry");

    let (mut recording_vm, contracts) = compile_vm(&root, "main.av");
    let calls = Arc::new(AtomicUsize::new(0));
    let providers = bind_pure_counter(&contracts, calls.clone(), "pure-v1");
    let provenance = providers.provenance();
    recording_vm.set_provider_registry(Arc::new(providers));
    recording_vm.start_recording();
    let result = recording_vm.run().expect("pure provider record run");
    assert_eq!(
        result.to_value(&recording_vm.arena),
        Value::Tuple(vec![Value::int(0), Value::int(1)])
    );
    assert_eq!(calls.load(Ordering::SeqCst), 2);
    assert!(
        recording_vm.recorded_effects().is_empty(),
        "pure calls must not enter the effect transcript"
    );

    let (mut replay_vm, replay_contracts) = compile_vm(&root, "main.av");
    let replay_calls = Arc::new(AtomicUsize::new(0));
    replay_vm.set_provider_registry(Arc::new(bind_pure_counter(
        &replay_contracts,
        replay_calls.clone(),
        "pure-v1",
    )));
    replay_vm
        .start_replay_with_provenance(Vec::new(), &provenance, true)
        .expect("same pure provider fingerprint");
    replay_vm.run().expect("pure live replay");
    assert_eq!(replay_calls.load(Ordering::SeqCst), 2);

    let (mut changed_vm, changed_contracts) = compile_vm(&root, "main.av");
    changed_vm.set_provider_registry(Arc::new(bind_pure_counter(
        &changed_contracts,
        Arc::new(AtomicUsize::new(0)),
        "pure-v2",
    )));
    assert!(
        changed_vm
            .start_replay_with_provenance(Vec::new(), &provenance, true)
            .expect_err("changed pure provider fingerprint")
            .contains("live provider mismatch")
    );
}

struct VaultProvider;

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
                ProviderValue::Resource(ProviderResource::new(41_i64)),
            ))),
            "Vault.read" => {
                let [ProviderValue::Resource(resource)] = args else {
                    return Err(ProviderFault::new("bad_args", "expected Vault.Token"));
                };
                let value = resource
                    .downcast_ref::<i64>()
                    .copied()
                    .ok_or_else(|| ProviderFault::new("bad_resource", "wrong payload"))?;
                Ok(ProviderValue::ResultOk(Box::new(ProviderValue::Int(
                    value.into(),
                ))))
            }
            other => Err(ProviderFault::new("bad_call", other)),
        }
    }
}

#[test]
fn opaque_resource_mints_consumes_records_and_replays_without_private_payload() {
    let root = temp_root("resource-replay");
    fs::write(
        root.join("Vault.av"),
        "\
module Vault
    kind = capability
    semantics = effectful
    exposes [open, read]

opaque Token

operation open() -> Result<Token, String>
    oracle = generative
    replay = recorded

operation read(token: Token) -> Result<Int, String>
    oracle = generative
    replay = recorded
",
    )
    .expect("write Vault");
    fs::write(
        root.join("main.av"),
        "\
module Client
    depends [Vault]
    exposes [main]
    effects [Vault.open, Vault.read]

fn main() -> Result<Int, String>
    ! [Vault.open, Vault.read]
    token = Vault.open()?
    Vault.read(token)
",
    )
    .expect("write entry");

    let (mut recording_vm, contracts) = compile_vm(&root, "main.av");
    let contract = contracts.contract("Vault").expect("Vault contract");
    let mut providers = ProviderRegistry::for_program(contracts.clone()).expect("program registry");
    providers
        .bind(ProviderBinding::new(
            "Vault",
            contract.contract_hash.clone(),
            ["Vault.open", "Vault.read"],
            Arc::new(VaultProvider),
        ))
        .expect("bind Vault");
    let provenance = providers.provenance();
    recording_vm.set_provider_registry(Arc::new(providers));
    recording_vm.start_recording();
    let result = recording_vm.run().expect("resource provider run");
    assert_eq!(
        result.to_value(&recording_vm.arena),
        Value::Ok(Box::new(Value::int(41)))
    );
    let effects = recording_vm.recorded_effects().to_vec();
    assert_eq!(effects.len(), 2);
    let recording_json = aver::replay::format_json(&aver::replay::JsonValue::Array(
        effects
            .iter()
            .flat_map(|effect| effect.args.iter().cloned())
            .collect(),
    ));
    assert!(recording_json.contains("$capabilityResource"));
    assert!(
        !recording_json.contains("41"),
        "provider payload leaked: {recording_json}"
    );

    let (mut replay_vm, replay_contracts) = compile_vm(&root, "main.av");
    replay_vm.set_provider_registry(Arc::new(
        ProviderRegistry::for_program(replay_contracts).expect("unbound replay registry"),
    ));
    replay_vm
        .start_replay_with_provenance(effects, &provenance, true)
        .expect("resource replay provenance");
    let replayed = replay_vm.run().expect("resource replay without provider");
    assert_eq!(
        replayed.to_value(&replay_vm.arena),
        Value::Ok(Box::new(Value::int(41)))
    );
    replay_vm
        .ensure_replay_consumed()
        .expect("resource transcript consumed");
}

#[test]
fn resources_minted_in_parallel_children_remain_valid_in_the_parent_vm() {
    let root = temp_root("resource-parallel-join");
    fs::write(
        root.join("Vault.av"),
        "\
module Vault
    kind = capability
    semantics = effectful
    exposes [open, read]

opaque Token

operation open() -> Result<Token, String>
    oracle = generative
    replay = recorded

operation read(token: Token) -> Result<Int, String>
    oracle = generative
    replay = recorded
",
    )
    .expect("write Vault");
    fs::write(
        root.join("main.av"),
        "\
module Client
    depends [Vault]
    exposes [main]
    effects [Vault.open]

fn main() -> Tuple<Result<Vault.Token, String>, Result<Vault.Token, String>>
    ! [Vault.open]
    (Vault.open(), Vault.open())!
",
    )
    .expect("write parallel resource entry");

    let (mut machine, contracts) = compile_vm(&root, "main.av");
    let contract = contracts.contract("Vault").expect("Vault contract");
    let mut providers = ProviderRegistry::for_program(contracts.clone()).expect("program registry");
    providers
        .bind(ProviderBinding::new(
            "Vault",
            contract.contract_hash.clone(),
            ["Vault.open", "Vault.read"],
            Arc::new(VaultProvider),
        ))
        .expect("bind Vault");
    machine.set_provider_registry(Arc::new(providers));

    let result = machine
        .run()
        .expect("parallel resource mint")
        .to_value(&machine.arena);
    let Value::Tuple(branches) = result else {
        panic!("expected resource tuple")
    };
    assert_eq!(branches.len(), 2);
    let providers = machine.provider_registry();
    let read = providers
        .contracts()
        .operation("Vault.read")
        .expect("Vault.read")
        .clone();
    for branch in branches {
        let Value::Ok(token) = branch else {
            panic!("expected Result.Ok resource")
        };
        assert_eq!(
            providers
                .invoke(&read, &[*token])
                .expect("parent consumes child-minted resource"),
            Value::Ok(Box::new(Value::int(41)))
        );
    }
}

struct OutputProvider {
    calls: Arc<Mutex<Vec<String>>>,
    fingerprint: &'static str,
}

impl CapabilityProvider for OutputProvider {
    fn identity(&self) -> &str {
        "example.output@1"
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
        self.calls
            .lock()
            .expect("output call log")
            .push(context.operation.clone());
        Ok(ProviderValue::Unit)
    }
}

fn bind_output(
    contracts: &aver::capability::CapabilityRegistry,
    calls: Arc<Mutex<Vec<String>>>,
    fingerprint: &'static str,
) -> ProviderRegistry {
    let mut providers = ProviderRegistry::for_program(contracts.clone()).expect("program registry");
    let contract = contracts.contract("Output").expect("Output contract");
    providers
        .bind(ProviderBinding::new(
            "Output",
            contract.contract_hash.clone(),
            ["Output.quiet", "Output.loud"],
            Arc::new(OutputProvider { calls, fingerprint }),
        ))
        .expect("bind Output");
    providers
}

#[test]
fn suppressed_and_reissued_share_a_mixed_transcript_without_shifting_it() {
    let root = temp_root("mixed-replay");
    fs::write(
        root.join("Output.av"),
        "\
module Output
    kind = capability
    semantics = effectful
    exposes [quiet, loud]

operation quiet() -> Unit
    oracle = output
    replay = suppressed

operation loud() -> Unit
    oracle = output
    replay = reissued
",
    )
    .expect("write Output");
    fs::write(
        root.join("main.av"),
        "\
module Client
    depends [Output]
    exposes [main]
    effects [Output.quiet, Output.loud]

fn main() -> Int
    ! [Output.quiet, Output.loud]
    Output.quiet()
    Output.loud()
    42
",
    )
    .expect("write entry");

    let (mut recording_vm, contracts) = compile_vm(&root, "main.av");
    let record_calls = Arc::new(Mutex::new(Vec::new()));
    let providers = bind_output(&contracts, record_calls.clone(), "output-v1");
    let provenance = providers.provenance();
    recording_vm.set_provider_registry(Arc::new(providers));
    recording_vm.start_recording();
    recording_vm.run().expect("record mixed outputs");
    assert_eq!(
        *record_calls.lock().expect("record call log"),
        vec!["Output.quiet".to_string(), "Output.loud".to_string()]
    );
    let effects = recording_vm.recorded_effects().to_vec();
    assert_eq!(effects.len(), 2);

    let (mut replay_vm, replay_contracts) = compile_vm(&root, "main.av");
    let replay_calls = Arc::new(Mutex::new(Vec::new()));
    replay_vm.set_provider_registry(Arc::new(bind_output(
        &replay_contracts,
        replay_calls.clone(),
        "output-v1",
    )));
    replay_vm
        .start_replay_with_provenance(effects, &provenance, true)
        .expect("mixed replay provenance");
    let result = replay_vm.run().expect("mixed replay");
    assert_eq!(result.to_value(&replay_vm.arena), Value::int(42));
    assert_eq!(
        *replay_calls.lock().expect("replay call log"),
        vec!["Output.loud".to_string()],
        "suppressed skips live dispatch; reissued calls exactly once"
    );
    replay_vm
        .ensure_replay_consumed()
        .expect("both events consumed");
}

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn standard_capability_target_fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/standard_capability_targets.av")
}

#[test]
fn check_and_rust_compile_report_standard_binding_identities() {
    let fixture = standard_capability_target_fixture();
    let check = Command::new(aver_bin())
        .args(["check", fixture.to_str().expect("fixture path")])
        .output()
        .expect("check standard capability canary");
    let check_text = String::from_utf8_lossy(&check.stdout);
    assert!(check.status.success(), "{check_text}");
    assert!(check_text.contains("capability Time: contract_hash=sha256:"));
    assert!(check_text.contains("model_hash=sha256:"));
    assert!(check_text.contains("vm:aver.standard.Time/native"));
    assert!(check_text.contains("rust:aver.standard.Time/native"));
    assert!(check_text.contains("wasm-gc:aver.standard.Time/wasm-gc-imports"));
    assert!(check_text.contains("wasip2:aver.standard.Time/wasip2-wasi"));
    assert!(check_text.contains("capability Random: contract_hash=sha256:"));
    assert!(check_text.contains("vm:aver.standard.Random/native"));
    assert!(check_text.contains("rust:aver.standard.Random/native"));
    assert!(check_text.contains("wasm-gc:aver.standard.Random/wasm-gc-imports"));
    assert!(check_text.contains("wasip2:aver.standard.Random/wasip2-wasi"));
    assert!(check_text.contains("capability Disk: contract_hash=sha256:"));
    assert!(check_text.contains("vm:aver.standard.Disk/native"));
    assert!(check_text.contains("rust:aver.standard.Disk/native"));
    assert!(check_text.contains("wasm-gc:aver.standard.Disk/wasm-gc-imports"));
    assert!(check_text.contains("wasip2:aver.standard.Disk/wasip2-wasi"));

    let output = temp_root("rust-accounting");
    let compile = Command::new(aver_bin())
        .args([
            "compile",
            fixture.to_str().expect("fixture path"),
            "--with-replay",
            "--output",
            output.to_str().expect("output path"),
        ])
        .output()
        .expect("compile standard capability canary to Rust");
    let compile_text = String::from_utf8_lossy(&compile.stdout);
    assert!(compile.status.success(), "{compile_text}");
    assert!(compile_text.contains("aver.standard.Time/native@aver-rt/"));
    assert!(compile_text.contains("aver.standard.Random/native@aver-rt/"));
    assert!(compile_text.contains("aver.standard.Disk/native@aver-rt/"));
    assert!(compile_text.contains("contract_hash=sha256:"));
    assert!(compile_text.contains("model_hash=sha256:"));
    let replay_support =
        fs::read_to_string(output.join("src/replay_support.rs")).expect("generated replay runtime");
    assert!(replay_support.contains("crate::provider_support::registry().provenance()"));
    let provider_support = fs::read_to_string(output.join("src/provider_support.rs"))
        .expect("generated provider runtime");
    assert!(provider_support.contains("StandardTimeProvider"));
    assert!(provider_support.contains("StandardRandomProvider"));
    assert!(provider_support.contains("StandardDiskProvider"));
    assert!(
        provider_support
            .contains("sha256:c7bd82159c4e5922771531cbf583bf6ff74a85dbb5c2c362d1e3b156c5720a49")
    );
    assert!(
        provider_support
            .contains("sha256:3b9239af56c4e89e527a53ce6fe4a470a42f84b203b10078c8633f39a6cec5f6")
    );
    assert!(
        provider_support
            .contains("sha256:d134b487a92f2094eb6ad478bff0984c5a481577df07a7f993652e9bc1f9d537")
    );
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_runs_standard_capabilities_through_registered_bindings() {
    let fixture = standard_capability_target_fixture();
    let run = Command::new(aver_bin())
        .args(["run", fixture.to_str().expect("fixture path"), "--wasm-gc"])
        .output()
        .expect("run standard capability canary on wasm-gc");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert!(run.status.success(), "{report}");
}

#[cfg(feature = "wasip2")]
#[test]
fn wasip2_runs_standard_capabilities_through_registered_bindings() {
    let fixture = standard_capability_target_fixture();
    let run = Command::new(aver_bin())
        .args(["run", fixture.to_str().expect("fixture path"), "--wasip2"])
        .output()
        .expect("run standard capability canary on wasip2");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert!(run.status.success(), "{report}");
}
