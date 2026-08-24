#[allow(unused_imports)]
use crate::aver_generated::bytes::*;
#[allow(unused_imports)]
use crate::*;

#[derive(Clone, PartialEq, Eq, Hash)]
enum ConnectionState {
    Live(aver_rt::provider::ProviderResourceHandle),
    Replay(u64),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Connection(ConnectionState);

impl std::fmt::Debug for Connection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Tcp.Connection(<resource>)")
    }
}

impl aver_rt::AverDisplay for Connection {
    fn aver_display(&self) -> String {
        "Tcp.Connection(<resource>)".to_string()
    }
}

impl aver_rt::provider::ProviderCodec for Connection {
    fn into_provider_value(
        self,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        if capability != "Tcp" {
            return Err(format!(
                "resource 'Tcp.Connection' belongs to capability 'Tcp', not '{}'",
                capability
            ));
        }
        match self.0 {
            ConnectionState::Live(handle) => registry.resolve_resource(capability, "Tcp.Connection", &handle).map(aver_rt::provider::ProviderValue::Resource),
            ConnectionState::Replay(_) => Err("replay-only capability resource 'Tcp.Connection' cannot enter a live provider call".to_string()),
        }
    }

    fn from_provider_value(
        value: aver_rt::provider::ProviderValue,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        if capability != "Tcp" || minted_resource != Some("Tcp.Connection") {
            return Err(
                "resource 'Tcp.Connection' may only be returned by its minting operation"
                    .to_string(),
            );
        }
        match value {
            aver_rt::provider::ProviderValue::Resource(resource) => registry
                .store_resource(capability, "Tcp.Connection", resource)
                .map(|handle| Self(ConnectionState::Live(handle))),
            other => Err(format!(
                "expected capability resource Tcp.Connection, got {}",
                other.shape()
            )),
        }
    }
}

impl crate::aver_replay::ReplayValue for Connection {
    fn to_replay_json(&self) -> serde_json::Value {
        match &self.0 {
            ConnectionState::Live(handle) => {
                crate::aver_replay::encode_live_capability_resource("Tcp.Connection", handle)
            }
            ConnectionState::Replay(trace) => {
                crate::aver_replay::encode_replay_capability_resource("Tcp.Connection", *trace)
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        crate::aver_replay::decode_capability_resource(value, "Tcp.Connection")
            .map(|trace| Self(ConnectionState::Replay(trace)))
    }
}

pub type Tcp_Connection = Connection;
