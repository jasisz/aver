#![cfg(feature = "wasip2")]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use wasmtime::component::{Component, Linker, ResourceTable};
use wasmtime::{Config, Engine, Store, StoreContextMut};
use wasmtime_wasi::p2::bindings::sync::Command as WasiCommand;
use wasmtime_wasi::p2::pipe::MemoryOutputPipe;
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

use aver::provider::{
    CapabilityProvider, ProviderBinding, ProviderContext, ProviderFault, ProviderValue,
};

struct ConfiguredEchoProvider {
    calls: Arc<AtomicUsize>,
}

struct ConfiguredProbeProvider {
    unit_calls: Arc<AtomicUsize>,
}

impl CapabilityProvider for ConfiguredProbeProvider {
    fn identity(&self) -> &str {
        "test.configured-wit-probe@1"
    }

    fn fingerprint(&self) -> &str {
        "configured-wit-probe-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match (context.operation.as_str(), args) {
            ("Probe.flip", [ProviderValue::Bool(value)]) => Ok(ProviderValue::Bool(!value)),
            ("Probe.scale", [ProviderValue::Float(value)]) => Ok(ProviderValue::Float(value * 2.0)),
            ("Probe.ping", [ProviderValue::Unit]) => {
                self.unit_calls.fetch_add(1, Ordering::SeqCst);
                Ok(ProviderValue::Unit)
            }
            (operation, _) => Err(ProviderFault::new("bad_call", operation)),
        }
    }
}

impl CapabilityProvider for ConfiguredEchoProvider {
    fn identity(&self) -> &str {
        "test.configured-wit-echo@1"
    }

    fn fingerprint(&self) -> &str {
        "configured-wit-echo-v1"
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match (context.operation.as_str(), args) {
            ("Echo.echo", [ProviderValue::String(value)]) => {
                self.calls.fetch_add(1, Ordering::SeqCst);
                Ok(ProviderValue::String(format!("host echoed: {value}")))
            }
            ("Echo.healthy", []) => Ok(ProviderValue::Bool(true)),
            (operation, _) => Err(ProviderFault::new("bad_call", operation)),
        }
    }
}

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/capability_wit")
}

struct Host {
    ctx: WasiCtx,
    table: ResourceTable,
    unit_calls: Arc<AtomicUsize>,
}

impl WasiView for Host {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.ctx,
            table: &mut self.table,
        }
    }
}

#[test]
fn echo_component_import_runs_through_a_separately_installed_host() {
    let root = fixture_root();
    let output = tempfile::tempdir().expect("component output dir");
    let compile = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("compile")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .args(["--target", "wasip2", "-o"])
        .arg(output.path())
        .output()
        .expect("compile Echo component");
    assert!(
        compile.status.success(),
        "component compile failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let component_bytes =
        std::fs::read(output.path().join("main.component.wasm")).expect("component bytes");
    let wit = std::fs::read_to_string(output.path().join("main.wit")).expect("sibling WIT");
    let interface = wit
        .lines()
        .find_map(|line| {
            line.trim()
                .strip_prefix("interface ")
                .and_then(|rest| rest.strip_suffix(" {"))
        })
        .expect("generated capability interface");
    assert!(wit.contains("contract_hash: sha256:"));
    assert!(wit.contains("model_hash: sha256:"));
    assert!(wit.contains("import cap-n4563686f-c"));

    let mut config = Config::new();
    config.wasm_component_model(true);
    config.wasm_gc(true);
    config.wasm_function_references(true);
    let engine = Engine::new(&config).expect("wasmtime engine");
    let component = Component::from_binary(&engine, &component_bytes).expect("valid component");

    let stdout = MemoryOutputPipe::new(4096);
    let mut wasi = WasiCtxBuilder::new();
    wasi.stdout(stdout.clone());
    let mut store = Store::new(
        &engine,
        Host {
            ctx: wasi.build(),
            table: ResourceTable::new(),
            unit_calls: Arc::new(AtomicUsize::new(0)),
        },
    );
    let mut linker = Linker::<Host>::new(&engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker).expect("WASI host bindings");
    {
        let mut capability = linker
            .instance(&format!("aver:user/{interface}"))
            .expect("custom capability linker instance");
        capability
            .func_wrap(
                "op-n6563686f",
                |_store: StoreContextMut<'_, Host>, (value,): (String,)| {
                    Ok((format!("host echoed: {value}"),))
                },
            )
            .expect("Echo.echo host implementation");
        capability
            .func_wrap(
                "op-n6865616c746879",
                |_store: StoreContextMut<'_, Host>, (): ()| Ok((true,)),
            )
            .expect("Echo.healthy host implementation");
    }

    let command =
        WasiCommand::instantiate(&mut store, &component, &linker).expect("instantiate component");
    assert_eq!(
        command
            .wasi_cli_run()
            .call_run(&mut store)
            .expect("run component"),
        Ok(())
    );
    assert_eq!(
        String::from_utf8(stdout.contents().to_vec()).expect("utf-8 stdout"),
        "host echoed: hello from Aver\n"
    );
}

#[test]
fn configured_rust_binding_runs_through_the_stock_wasip2_cli_adapter() {
    let root = fixture_root();
    let calls = Arc::new(AtomicUsize::new(0));
    let binding = ProviderBinding::new(
        "Echo",
        "sha256:36832abf9ae258a8d018106da7ea75b15618c40d9f45a1959ab5d59e59358586",
        ["Echo.echo", "Echo.healthy"],
        Arc::new(ConfiguredEchoProvider {
            calls: Arc::clone(&calls),
        }),
    );
    aver::cli_entry::main_with_args_and_provider_bindings(
        vec![
            "aver".into(),
            "run".into(),
            root.join("main.av").into_os_string(),
            "--module-root".into(),
            root.into_os_string(),
            "--wasip2".into(),
        ],
        vec![binding],
    );
    assert_eq!(calls.load(Ordering::SeqCst), 1);
}

#[test]
#[ignore = "builds a nested wasip2-enabled cached Rust host; run explicitly for package smoke"]
fn configured_package_factory_runs_and_then_reuses_the_cached_wasip2_host() {
    let root = fixture_root();
    let cache = tempfile::tempdir().expect("wasip2 provider-host cache");
    let run = |cargo: Option<&str>| {
        let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
        command
            .arg("run")
            .arg(root.join("main.av"))
            .arg("--module-root")
            .arg(&root)
            .arg("--wasip2")
            .env("AVER_PROVIDER_HOST_CACHE", cache.path())
            .env("CARGO_NET_OFFLINE", "true")
            .env_remove("RUSTC_WRAPPER")
            .env_remove("RUSTC_WORKSPACE_WRAPPER");
        if let Some(cargo) = cargo {
            command.env("CARGO", cargo);
        }
        command.output().expect("run cached wasip2 provider host")
    };

    let first = run(None);
    let first_report = format!(
        "{}{}",
        String::from_utf8_lossy(&first.stdout),
        String::from_utf8_lossy(&first.stderr)
    );
    assert!(first.status.success(), "{first_report}");
    assert!(first_report.contains("Building provider host"));
    assert!(first_report.contains("host echoed: hello from Aver"));

    let cached = run(Some("/definitely/missing/cargo"));
    let cached_report = format!(
        "{}{}",
        String::from_utf8_lossy(&cached.stdout),
        String::from_utf8_lossy(&cached.stderr)
    );
    assert!(cached.status.success(), "{cached_report}");
    assert!(cached_report.contains("host echoed: hello from Aver"));
    assert!(!cached_report.contains("provider host"));
}

#[test]
fn configured_pure_binding_round_trips_bool_float_and_unit() {
    let root = fixture_root();
    let unit_calls = Arc::new(AtomicUsize::new(0));
    let binding = ProviderBinding::new(
        "Probe",
        "sha256:dcb1c1be42562497bcef73b92de089a3c39049905d7ee686e0b0cccb30b30c92",
        ["Probe.flip", "Probe.ping", "Probe.scale"],
        Arc::new(ConfiguredProbeProvider {
            unit_calls: Arc::clone(&unit_calls),
        }),
    );
    aver::cli_entry::main_with_args_and_provider_bindings(
        vec![
            "aver".into(),
            "run".into(),
            root.join("probes.av").into_os_string(),
            "--module-root".into(),
            root.into_os_string(),
            "--wasip2".into(),
        ],
        vec![binding],
    );
    assert_eq!(unit_calls.load(Ordering::SeqCst), 1);
}

#[test]
fn embedded_runner_reports_provider_missing_before_linking() {
    let root = fixture_root();
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .arg("--wasip2")
        .output()
        .expect("run Echo through embedded wasip2 runner");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert!(!run.status.success(), "embedded runner unexpectedly linked");
    assert!(report.contains("error[capability-provider-missing]"));
    assert!(report.contains("capability: Echo"));
    assert!(!report.contains("capability-target-unsupported"));
    assert!(!report.contains("unknown import"));
}

#[test]
fn unsupported_boundary_fixtures_fail_at_the_exact_type_path() {
    let root = fixture_root().join("unsupported");
    let cases = [
        ("int.av", "WideInt", "WideInt.read", "Int"),
        (
            "result.av",
            "Results",
            "Results.read",
            "Result<String, String>",
        ),
        (
            "collection.av",
            "Collections",
            "Collections.read",
            "List<String>",
        ),
        (
            "represented_client.av",
            "Represented",
            "Represented.read",
            "Reply",
        ),
        ("opaque.av", "Secrets", "Secrets.read", "Token"),
    ];

    for (entry, capability, operation, aver_type) in cases {
        let output = tempfile::tempdir().expect("unsupported component output dir");
        let compile = Command::new(env!("CARGO_BIN_EXE_aver"))
            .arg("compile")
            .arg(root.join(entry))
            .arg("--module-root")
            .arg(&root)
            .args(["--target", "wasip2", "-o"])
            .arg(output.path())
            .output()
            .expect("compile unsupported capability fixture");
        let report = format!(
            "{}{}",
            String::from_utf8_lossy(&compile.stdout),
            String::from_utf8_lossy(&compile.stderr)
        );
        assert!(!compile.status.success(), "{entry} unexpectedly compiled");
        let contains = |needle: &str| {
            assert!(
                report.contains(needle),
                "{entry} report did not contain {needle:?}:\n{report}"
            );
        };
        contains("error[capability-target-unsupported]");
        contains("reason[wit-boundary-type-unsupported]");
        contains(&format!("capability `{capability}`"));
        contains(&format!("operation `{operation}` result"));
        contains(&format!("Aver type `{aver_type}`"));
        contains("contract_hash: sha256:");
        contains("model_hash: sha256:");
    }
}

#[test]
fn pure_bool_float_and_unit_use_the_same_component_import_route() {
    let root = fixture_root();
    let output = tempfile::tempdir().expect("component output dir");
    let compile = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("compile")
        .arg(root.join("probes.av"))
        .arg("--module-root")
        .arg(&root)
        .args(["--target", "wasip2", "-o"])
        .arg(output.path())
        .output()
        .expect("compile Probe component");
    assert!(
        compile.status.success(),
        "Probe component compile failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let component_bytes =
        std::fs::read(output.path().join("probes.component.wasm")).expect("Probe component bytes");
    let wit = std::fs::read_to_string(output.path().join("probes.wit")).expect("Probe sibling WIT");
    let interface = wit
        .lines()
        .find_map(|line| {
            line.trim()
                .strip_prefix("interface ")
                .and_then(|rest| rest.strip_suffix(" {"))
        })
        .expect("generated Probe interface");

    let mut config = Config::new();
    config.wasm_component_model(true);
    config.wasm_gc(true);
    config.wasm_function_references(true);
    let engine = Engine::new(&config).expect("wasmtime engine");
    let component = Component::from_binary(&engine, &component_bytes).expect("valid component");
    let stdout = MemoryOutputPipe::new(4096);
    let unit_calls = Arc::new(AtomicUsize::new(0));
    let mut wasi = WasiCtxBuilder::new();
    wasi.stdout(stdout.clone());
    let mut store = Store::new(
        &engine,
        Host {
            ctx: wasi.build(),
            table: ResourceTable::new(),
            unit_calls: Arc::clone(&unit_calls),
        },
    );
    let mut linker = Linker::<Host>::new(&engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker).expect("WASI host bindings");
    {
        let mut capability = linker
            .instance(&format!("aver:user/{interface}"))
            .expect("Probe linker instance");
        capability
            .func_wrap(
                "op-n666c6970",
                |_store: StoreContextMut<'_, Host>, (value,): (bool,)| Ok((!value,)),
            )
            .expect("Probe.flip host implementation");
        capability
            .func_wrap(
                "op-n70696e67",
                |mut store: StoreContextMut<'_, Host>, (): ()| {
                    store.data_mut().unit_calls.fetch_add(1, Ordering::SeqCst);
                    Ok(())
                },
            )
            .expect("Probe.ping host implementation");
        capability
            .func_wrap(
                "op-n7363616c65",
                |_store: StoreContextMut<'_, Host>, (value,): (f64,)| Ok((value * 2.0,)),
            )
            .expect("Probe.scale host implementation");
    }
    let command =
        WasiCommand::instantiate(&mut store, &component, &linker).expect("instantiate component");
    assert_eq!(
        command
            .wasi_cli_run()
            .call_run(&mut store)
            .expect("run component"),
        Ok(())
    );
    assert_eq!(
        String::from_utf8(stdout.contents().to_vec()).expect("utf-8 stdout"),
        "probe-ok\n"
    );
    assert_eq!(unit_calls.load(Ordering::SeqCst), 1);
}
