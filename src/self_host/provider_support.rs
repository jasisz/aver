use aver_rt::provider::{
    NativeProviderRegistry, ProviderBinding, ProviderCodec, ProviderContractSpec, ProviderValue,
};
use std::sync::OnceLock;

static PROVIDERS: OnceLock<NativeProviderRegistry> = OnceLock::new();

fn build_registry(
    bindings: Vec<ProviderBinding>,
    include_defaults: bool,
) -> Result<NativeProviderRegistry, String> {
    let mut registry = NativeProviderRegistry::new(vec![
        ProviderContractSpec::new(
            "Disk",
            "sha256:21ba58983c2ba61c06153df36a9c205770994c36a61ae280c1f49da336e63e23",
            "sha256:cf55979e264c3a26a246bb77663422011efb71e6ce2ba973ad4b267195f25570",
            vec![
                "Disk.appendBytes".to_string(),
                "Disk.appendText".to_string(),
                "Disk.delete".to_string(),
                "Disk.deleteDir".to_string(),
                "Disk.exists".to_string(),
                "Disk.listDir".to_string(),
                "Disk.makeDir".to_string(),
                "Disk.readBytes".to_string(),
                "Disk.readBytesAt".to_string(),
                "Disk.readText".to_string(),
                "Disk.size".to_string(),
                "Disk.writeBytes".to_string(),
                "Disk.writeText".to_string(),
            ],
        ),
        ProviderContractSpec::new(
            "Random",
            "sha256:5c23bcf6fe8a6515ea430de874828421cff538f89b3bc142d03f2e6cc014dec7",
            "sha256:88b58ba022e2decf378a0c16597808b09ec6923c2d07977ae9ed28502ea5878b",
            vec!["Random.float".to_string(), "Random.int".to_string()],
        ),
        ProviderContractSpec::new(
            "Tcp",
            "sha256:29c33e54d33ef032f469b3f4543c81ea48a3c28d985f5cc4fede4acbfda18385",
            "sha256:3ea8be66d5660ffd5005ce718841c23c8903be734297b2fbd2f5a6b140615d87",
            vec![
                "Tcp.close".to_string(),
                "Tcp.connect".to_string(),
                "Tcp.ping".to_string(),
                "Tcp.poll".to_string(),
                "Tcp.readBytes".to_string(),
                "Tcp.readLine".to_string(),
                "Tcp.readSome".to_string(),
                "Tcp.send".to_string(),
                "Tcp.sendBytes".to_string(),
                "Tcp.writeBytes".to_string(),
                "Tcp.writeLine".to_string(),
            ],
        ),
        ProviderContractSpec::new(
            "Time",
            "sha256:e80d264b61f2808b4db4d765ded0d3db1a9a019c814d27686ef7e71bc4c208af",
            "sha256:30d2e4e49965d6cf8eec13daf864fd10c5ee22d5ec188251e39421cec466e7e1",
            vec![
                "Time.now".to_string(),
                "Time.sleep".to_string(),
                "Time.unixMs".to_string(),
            ],
        ),
    ])?;
    let standard_tcp_settings = crate::aver_replay::tcp_provider_settings_from_env()?;
    if include_defaults {
        registry.bind(ProviderBinding::new(
            "Disk",
            "sha256:21ba58983c2ba61c06153df36a9c205770994c36a61ae280c1f49da336e63e23",
            vec![
                "Disk.appendBytes".to_string(),
                "Disk.appendText".to_string(),
                "Disk.delete".to_string(),
                "Disk.deleteDir".to_string(),
                "Disk.exists".to_string(),
                "Disk.listDir".to_string(),
                "Disk.makeDir".to_string(),
                "Disk.readBytes".to_string(),
                "Disk.readBytesAt".to_string(),
                "Disk.readText".to_string(),
                "Disk.size".to_string(),
                "Disk.writeBytes".to_string(),
                "Disk.writeText".to_string(),
            ],
            std::sync::Arc::new(aver_rt::provider::StandardDiskProvider),
        ))?;
    }
    if include_defaults {
        registry.bind(ProviderBinding::new(
            "Random",
            "sha256:5c23bcf6fe8a6515ea430de874828421cff538f89b3bc142d03f2e6cc014dec7",
            vec!["Random.float".to_string(), "Random.int".to_string()],
            std::sync::Arc::new(aver_rt::provider::StandardRandomProvider),
        ))?;
    }
    if include_defaults {
        registry.bind(ProviderBinding::new(
            "Tcp",
            "sha256:29c33e54d33ef032f469b3f4543c81ea48a3c28d985f5cc4fede4acbfda18385",
            vec![
                "Tcp.close".to_string(),
                "Tcp.connect".to_string(),
                "Tcp.ping".to_string(),
                "Tcp.poll".to_string(),
                "Tcp.readBytes".to_string(),
                "Tcp.readLine".to_string(),
                "Tcp.readSome".to_string(),
                "Tcp.send".to_string(),
                "Tcp.sendBytes".to_string(),
                "Tcp.writeBytes".to_string(),
                "Tcp.writeLine".to_string(),
            ],
            std::sync::Arc::new(aver_rt::provider::StandardTcpProvider::new(
                standard_tcp_settings,
            )),
        ))?;
    }
    if include_defaults {
        registry.bind(ProviderBinding::new(
            "Time",
            "sha256:e80d264b61f2808b4db4d765ded0d3db1a9a019c814d27686ef7e71bc4c208af",
            vec![
                "Time.now".to_string(),
                "Time.sleep".to_string(),
                "Time.unixMs".to_string(),
            ],
            std::sync::Arc::new(aver_rt::provider::StandardTimeProvider),
        ))?;
    }
    let mut supplied = std::collections::BTreeSet::new();
    for binding in bindings {
        if !supplied.insert(binding.capability().to_string()) {
            return Err(format!(
                "error[capability-provider-duplicate]: capability '{}' has more than one host-supplied provider binding",
                binding.capability()
            ));
        }
        if registry.binding(binding.capability()).is_some() {
            registry.replace_binding(binding)?;
        } else {
            registry.bind(binding)?;
        }
    }
    Ok(registry)
}

/// Install the native providers for this generated artifact. The set is
/// immutable after the first Aver entry starts, so parallel branches cannot
/// observe different provider instances.
pub fn install_provider_bindings(bindings: Vec<ProviderBinding>) -> Result<(), String> {
    let registry = build_registry(bindings, true)?;
    PROVIDERS.set(registry).map_err(|_| "error[capability-provider-already-installed]: provider bindings were already installed for this artifact".to_string())
}

/// Install exactly the supplied bindings, without compiler-shipped defaults.
/// This is useful for fully explicit hosts and fault-injection tests.
pub fn install_provider_bindings_exact(bindings: Vec<ProviderBinding>) -> Result<(), String> {
    let registry = build_registry(bindings, false)?;
    PROVIDERS.set(registry).map_err(|_| "error[capability-provider-already-installed]: provider bindings were already installed for this artifact".to_string())
}

pub fn ensure_default_provider_bindings() {
    PROVIDERS.get_or_init(|| {
        build_registry(Vec::new(), true)
            .expect("compiler-shipped provider bindings must match embedded contracts")
    });
}

pub fn registry() -> &'static NativeProviderRegistry {
    ensure_default_provider_bindings();
    PROVIDERS.get().expect("provider registry initialized")
}

pub fn preflight_required_providers() -> Result<(), String> {
    registry().preflight([
        "Disk.appendText",
        "Disk.delete",
        "Disk.deleteDir",
        "Disk.exists",
        "Disk.listDir",
        "Disk.makeDir",
        "Disk.readText",
        "Disk.writeText",
        "Random.int",
        "Tcp.close",
        "Tcp.connect",
        "Tcp.ping",
        "Tcp.readLine",
        "Tcp.send",
        "Tcp.writeLine",
        "Time.now",
        "Time.sleep",
        "Time.unixMs",
    ])
}

pub fn encode<T: ProviderCodec>(value: T, capability: &str) -> ProviderValue {
    value
        .into_provider_value(registry(), capability)
        .unwrap_or_else(|message| {
            panic!("error[capability-provider-invalid-argument]: {}", message)
        })
}

pub fn invoke<T: ProviderCodec>(
    capability: &str,
    operation: &str,
    args: Vec<ProviderValue>,
    minted_resource: Option<&str>,
    expected: &str,
) -> T {
    let registry = registry();
    let value = registry
        .invoke(operation, &args)
        .unwrap_or_else(|message| panic!("{}", message));
    let received = value.shape();
    T::from_provider_value(value, registry, capability, minted_resource).unwrap_or_else(|message| {
let provider = registry.provider_identity_for(capability).unwrap_or("<missing>");
panic!("error[capability-provider-invalid-return]: provider '{}' returned an invalid value for '{}': expected {}, received {}; {}", provider, operation, expected, received, message)
})
}
