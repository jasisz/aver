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
            "Args",
            "sha256:8326d0f27169e1b73a9f45b8a2d2f3f0d89d6c6e4eb64dc646781082ee585265",
            "sha256:c50c9f100b581e6d40a4559e6c1acd79e703098e123530e83fa1bf195aab003d",
            vec!["Args.get".to_string()],
        ),
        ProviderContractSpec::new(
            "Console",
            "sha256:878f543df5342a616fddc83b61f1e035931cc24b89bfc523a52de5b0525a62b4",
            "sha256:bd56971f80627262dd095709bd3c3a9e86eb79bd1afc3d131e640a5f7b9f65fd",
            vec![
                "Console.error".to_string(),
                "Console.print".to_string(),
                "Console.readLine".to_string(),
                "Console.warn".to_string(),
            ],
        ),
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
            "Env",
            "sha256:ab75df52ca80c64a557ccfd3e7b7a1f9510f346ae7d08ffcb0f9138a7caa9d5b",
            "sha256:20f5c3c834fe829c80c88b4b1dd2696ba7c4627992f905b400e13bdcd4ee2171",
            vec!["Env.get".to_string(), "Env.set".to_string()],
        ),
        ProviderContractSpec::new(
            "Http",
            "sha256:5d61418e444a4a9ec5fcacbb18134b566ef811525462cfb57a549338adc73b51",
            "sha256:b8a6d1028ce713390d47c2baf72cbeef75127328ee24b19cf23401342a3116f1",
            vec![
                "Http.delete".to_string(),
                "Http.get".to_string(),
                "Http.head".to_string(),
                "Http.patch".to_string(),
                "Http.post".to_string(),
                "Http.put".to_string(),
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
            "sha256:2f32788e56fb4be7a05fa348315e52285e09c8e671ba02c05285105a68911af9",
            "sha256:4d6a91832fe25919d9dcce68bb86397a72f959e24c4a685923abde7d3359a407",
            vec![
                "Tcp.accept".to_string(),
                "Tcp.beginConnect".to_string(),
                "Tcp.close".to_string(),
                "Tcp.closeDial".to_string(),
                "Tcp.closeListener".to_string(),
                "Tcp.connect".to_string(),
                "Tcp.dialled".to_string(),
                "Tcp.listen".to_string(),
                "Tcp.peerAddress".to_string(),
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
            "Terminal",
            "sha256:3cb1bc67569388135723ecad0346164a5ce277f09109fa647eba057dc91de9c6",
            "sha256:c7c181b2bb3fd57e121a5128ead2eff523d63f1a8e2ba70b4eab9889b629e703",
            vec![
                "Terminal.clear".to_string(),
                "Terminal.disableRawMode".to_string(),
                "Terminal.enableRawMode".to_string(),
                "Terminal.flush".to_string(),
                "Terminal.hideCursor".to_string(),
                "Terminal.moveTo".to_string(),
                "Terminal.print".to_string(),
                "Terminal.readKey".to_string(),
                "Terminal.resetColor".to_string(),
                "Terminal.setColor".to_string(),
                "Terminal.showCursor".to_string(),
                "Terminal.size".to_string(),
            ],
        ),
        ProviderContractSpec::new(
            "Time",
            "sha256:e80d264b61f2808b4db4d765ded0d3db1a9a019c814d27686ef7e71bc4c208af",
            "sha256:07ad032ad093e63f61e39f59f9452b4787936c18f97953a955c998e6593ac294",
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
            "Args",
            "sha256:8326d0f27169e1b73a9f45b8a2d2f3f0d89d6c6e4eb64dc646781082ee585265",
            vec!["Args.get".to_string()],
            std::sync::Arc::new(aver_rt::provider::StandardArgsProvider::default()),
        ))?;
    }
    if include_defaults {
        registry.bind(ProviderBinding::new(
            "Console",
            "sha256:878f543df5342a616fddc83b61f1e035931cc24b89bfc523a52de5b0525a62b4",
            vec![
                "Console.error".to_string(),
                "Console.print".to_string(),
                "Console.readLine".to_string(),
                "Console.warn".to_string(),
            ],
            std::sync::Arc::new(aver_rt::provider::StandardConsoleProvider),
        ))?;
    }
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
            "Env",
            "sha256:ab75df52ca80c64a557ccfd3e7b7a1f9510f346ae7d08ffcb0f9138a7caa9d5b",
            vec!["Env.get".to_string(), "Env.set".to_string()],
            std::sync::Arc::new(aver_rt::provider::StandardEnvProvider),
        ))?;
    }
    if include_defaults {
        registry.bind(ProviderBinding::new(
            "Http",
            "sha256:5d61418e444a4a9ec5fcacbb18134b566ef811525462cfb57a549338adc73b51",
            vec![
                "Http.delete".to_string(),
                "Http.get".to_string(),
                "Http.head".to_string(),
                "Http.patch".to_string(),
                "Http.post".to_string(),
                "Http.put".to_string(),
            ],
            std::sync::Arc::new(aver_rt::provider::StandardHttpProvider),
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
            "sha256:2f32788e56fb4be7a05fa348315e52285e09c8e671ba02c05285105a68911af9",
            vec![
                "Tcp.accept".to_string(),
                "Tcp.beginConnect".to_string(),
                "Tcp.close".to_string(),
                "Tcp.closeDial".to_string(),
                "Tcp.closeListener".to_string(),
                "Tcp.connect".to_string(),
                "Tcp.dialled".to_string(),
                "Tcp.listen".to_string(),
                "Tcp.peerAddress".to_string(),
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
            "Terminal",
            "sha256:3cb1bc67569388135723ecad0346164a5ce277f09109fa647eba057dc91de9c6",
            vec![
                "Terminal.clear".to_string(),
                "Terminal.disableRawMode".to_string(),
                "Terminal.enableRawMode".to_string(),
                "Terminal.flush".to_string(),
                "Terminal.hideCursor".to_string(),
                "Terminal.moveTo".to_string(),
                "Terminal.print".to_string(),
                "Terminal.readKey".to_string(),
                "Terminal.resetColor".to_string(),
                "Terminal.setColor".to_string(),
                "Terminal.showCursor".to_string(),
                "Terminal.size".to_string(),
            ],
            std::sync::Arc::new(aver_rt::provider::StandardTerminalProvider),
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
        "Args.get",
        "Console.error",
        "Console.print",
        "Console.readLine",
        "Console.warn",
        "Disk.appendText",
        "Disk.delete",
        "Disk.deleteDir",
        "Disk.exists",
        "Disk.listDir",
        "Disk.makeDir",
        "Disk.readText",
        "Disk.writeText",
        "Env.get",
        "Env.set",
        "Http.delete",
        "Http.get",
        "Http.head",
        "Http.patch",
        "Http.post",
        "Http.put",
        "Random.int",
        "Tcp.accept",
        "Tcp.beginConnect",
        "Tcp.close",
        "Tcp.closeDial",
        "Tcp.closeListener",
        "Tcp.connect",
        "Tcp.dialled",
        "Tcp.listen",
        "Tcp.peerAddress",
        "Tcp.ping",
        "Tcp.poll",
        "Tcp.readLine",
        "Tcp.send",
        "Tcp.writeLine",
        "Terminal.clear",
        "Terminal.disableRawMode",
        "Terminal.enableRawMode",
        "Terminal.flush",
        "Terminal.hideCursor",
        "Terminal.moveTo",
        "Terminal.print",
        "Terminal.readKey",
        "Terminal.resetColor",
        "Terminal.setColor",
        "Terminal.showCursor",
        "Terminal.size",
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
