//! Private, generated transport for compiler-owned native job bindings.
//!
//! The scheduler still carries ProviderValue, but a task/result can own its
//! Rust value instead of walking it through a codec. Explicit host bindings
//! always use the public value ABI, even if they copy a default's identity.

pub(super) const SUPPORT: &str = r#"
struct NativeWorkValue(std::sync::Mutex<Option<Box<dyn std::any::Any + Send>>>);

fn pack_native_work<T: Send + 'static>(value: T) -> ProviderValue {
    ProviderValue::Resource(aver_rt::provider::ProviderResource::new(
        NativeWorkValue(std::sync::Mutex::new(Some(Box::new(value)))),
    ))
}

fn decode_work_value<T: ProviderCodec + Send + 'static>(
    value: ProviderValue,
    registry: &NativeProviderRegistry,
    capability: &str,
    minted_resource: Option<&str>,
) -> Result<(T, bool), String> {
    if let ProviderValue::Resource(resource) = &value {
        if let Some(native) = resource.downcast_ref::<NativeWorkValue>() {
            let boxed = native.0.lock().map_err(|_| "work: native transfer lock poisoned")?
                .take().ok_or("work: native value already transferred")?;
            return boxed.downcast::<T>().map(|value| (*value, true))
                .map_err(|_| "work: native transfer type mismatch".to_string());
        }
    }
    T::from_provider_value(value, registry, capability, minted_resource).map(|value| (value, false))
}

pub fn encode_work_task<T: ProviderCodec + Send + 'static>(value: T, capability: &str) -> ProviderValue {
    ensure_default_provider_bindings();
    let providers = PROVIDERS.get().expect("provider registry initialized");
    if native_work_enabled() && providers.native_work.contains(capability) {
        pack_native_work(value)
    } else {
        encode(value, capability)
    }
}

// Only this wrapper accepts the private transfer envelope. Ordinary resource
// codecs and host-provider contracts remain unchanged.
struct WorkAnswer<T>(T);

impl<T: ProviderCodec + Send + 'static> ProviderCodec for WorkAnswer<T> {
    fn into_provider_value(self, registry: &NativeProviderRegistry, capability: &str) -> Result<ProviderValue, String> {
        self.0.into_provider_value(registry, capability)
    }

    fn from_provider_value(value: ProviderValue, registry: &NativeProviderRegistry, capability: &str, minted_resource: Option<&str>) -> Result<Self, String> {
        decode_work_value(value, registry, capability, minted_resource).map(|(value, _)| Self(value))
    }
}

pub fn invoke_work_take<T: ProviderCodec + Send + 'static>(
    capability: &str, operation: &str, args: Vec<ProviderValue>, expected: &str,
) -> Result<Option<T>, aver_rt::AverStr> {
    invoke::<Result<Option<WorkAnswer<T>>, aver_rt::AverStr>>(capability, operation, args, None, expected)
        .map(|answer| answer.map(|answer| answer.0))
}
"#;
