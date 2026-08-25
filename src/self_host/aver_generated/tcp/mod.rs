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

#[derive(Clone, PartialEq, Eq, Hash)]
enum DialState {
    Live(aver_rt::provider::ProviderResourceHandle),
    Replay(u64),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Dial(DialState);

impl std::fmt::Debug for Dial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Tcp.Dial(<resource>)")
    }
}

impl aver_rt::AverDisplay for Dial {
    fn aver_display(&self) -> String {
        "Tcp.Dial(<resource>)".to_string()
    }
}

impl aver_rt::provider::ProviderCodec for Dial {
    fn into_provider_value(
        self,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        if capability != "Tcp" {
            return Err(format!(
                "resource 'Tcp.Dial' belongs to capability 'Tcp', not '{}'",
                capability
            ));
        }
        match self.0 {
            DialState::Live(handle) => registry
                .resolve_resource(capability, "Tcp.Dial", &handle)
                .map(aver_rt::provider::ProviderValue::Resource),
            DialState::Replay(_) => Err(
                "replay-only capability resource 'Tcp.Dial' cannot enter a live provider call"
                    .to_string(),
            ),
        }
    }

    fn from_provider_value(
        value: aver_rt::provider::ProviderValue,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        if capability != "Tcp" || minted_resource != Some("Tcp.Dial") {
            return Err(
                "resource 'Tcp.Dial' may only be returned by its minting operation".to_string(),
            );
        }
        match value {
            aver_rt::provider::ProviderValue::Resource(resource) => registry
                .store_resource(capability, "Tcp.Dial", resource)
                .map(|handle| Self(DialState::Live(handle))),
            other => Err(format!(
                "expected capability resource Tcp.Dial, got {}",
                other.shape()
            )),
        }
    }
}

impl crate::aver_replay::ReplayValue for Dial {
    fn to_replay_json(&self) -> serde_json::Value {
        match &self.0 {
            DialState::Live(handle) => {
                crate::aver_replay::encode_live_capability_resource("Tcp.Dial", handle)
            }
            DialState::Replay(trace) => {
                crate::aver_replay::encode_replay_capability_resource("Tcp.Dial", *trace)
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        crate::aver_replay::decode_capability_resource(value, "Tcp.Dial")
            .map(|trace| Self(DialState::Replay(trace)))
    }
}

pub type Tcp_Dial = Dial;

#[derive(Clone, PartialEq, Eq, Hash)]
enum ListenerState {
    Live(aver_rt::provider::ProviderResourceHandle),
    Replay(u64),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Listener(ListenerState);

impl std::fmt::Debug for Listener {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Tcp.Listener(<resource>)")
    }
}

impl aver_rt::AverDisplay for Listener {
    fn aver_display(&self) -> String {
        "Tcp.Listener(<resource>)".to_string()
    }
}

impl aver_rt::provider::ProviderCodec for Listener {
    fn into_provider_value(
        self,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        if capability != "Tcp" {
            return Err(format!(
                "resource 'Tcp.Listener' belongs to capability 'Tcp', not '{}'",
                capability
            ));
        }
        match self.0 {
            ListenerState::Live(handle) => registry
                .resolve_resource(capability, "Tcp.Listener", &handle)
                .map(aver_rt::provider::ProviderValue::Resource),
            ListenerState::Replay(_) => Err(
                "replay-only capability resource 'Tcp.Listener' cannot enter a live provider call"
                    .to_string(),
            ),
        }
    }

    fn from_provider_value(
        value: aver_rt::provider::ProviderValue,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        if capability != "Tcp" || minted_resource != Some("Tcp.Listener") {
            return Err(
                "resource 'Tcp.Listener' may only be returned by its minting operation".to_string(),
            );
        }
        match value {
            aver_rt::provider::ProviderValue::Resource(resource) => registry
                .store_resource(capability, "Tcp.Listener", resource)
                .map(|handle| Self(ListenerState::Live(handle))),
            other => Err(format!(
                "expected capability resource Tcp.Listener, got {}",
                other.shape()
            )),
        }
    }
}

impl crate::aver_replay::ReplayValue for Listener {
    fn to_replay_json(&self) -> serde_json::Value {
        match &self.0 {
            ListenerState::Live(handle) => {
                crate::aver_replay::encode_live_capability_resource("Tcp.Listener", handle)
            }
            ListenerState::Replay(trace) => {
                crate::aver_replay::encode_replay_capability_resource("Tcp.Listener", *trace)
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        crate::aver_replay::decode_capability_resource(value, "Tcp.Listener")
            .map(|trace| Self(ListenerState::Replay(trace)))
    }
}

pub type Tcp_Listener = Listener;

#[derive(Clone, Debug, PartialEq)]
pub enum Socket {
    Listening(Listener),
    Dialing(Dial),
    Connected(Connection),
}

impl aver_rt::AverDisplay for Socket {
    fn aver_display(&self) -> String {
        match self {
            Socket::Listening(f0) => format!("Listening({})", f0.aver_display_inner()),
            Socket::Dialing(f0) => format!("Dialing({})", f0.aver_display_inner()),
            Socket::Connected(f0) => format!("Connected({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_rt::provider::ProviderCodec for Socket {
    fn into_provider_value(
        self,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        use aver_rt::provider::ProviderCodec as _;
        Ok(match self {
            Self::Listening(field0) => aver_rt::provider::ProviderValue::Variant {
                type_name: "Tcp.Socket".to_string(),
                variant: "Listening".to_string(),
                fields: vec![field0.into_provider_value(registry, capability)?],
            },
            Self::Dialing(field0) => aver_rt::provider::ProviderValue::Variant {
                type_name: "Tcp.Socket".to_string(),
                variant: "Dialing".to_string(),
                fields: vec![field0.into_provider_value(registry, capability)?],
            },
            Self::Connected(field0) => aver_rt::provider::ProviderValue::Variant {
                type_name: "Tcp.Socket".to_string(),
                variant: "Connected".to_string(),
                fields: vec![field0.into_provider_value(registry, capability)?],
            },
        })
    }

    fn from_provider_value(
        value: aver_rt::provider::ProviderValue,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        let aver_rt::provider::ProviderValue::Variant {
            type_name,
            variant,
            fields,
        } = value
        else {
            return Err(format!(
                "expected represented boundary type 'Tcp.Socket', got {}",
                value.shape()
            ));
        };
        if type_name != "Tcp.Socket" {
            return Err(format!(
                "expected represented boundary type 'Tcp.Socket', got variant {}.{}",
                type_name, variant
            ));
        }
        let field_count = fields.len();
        let mut fields = fields.into_iter();
        match variant.as_str() {
            "Listening" if field_count == 1 => Ok(Self::Listening(
                <Tcp_Listener as aver_rt::provider::ProviderCodec>::from_provider_value(
                    fields.next().expect("validated variant field 0"),
                    registry,
                    capability,
                    minted_resource,
                )?,
            )),
            "Listening" => Err(format!(
                "variant 'Tcp.Socket.Listening' expected 1 field(s), got {}",
                field_count
            )),
            "Dialing" if field_count == 1 => Ok(Self::Dialing(
                <Tcp_Dial as aver_rt::provider::ProviderCodec>::from_provider_value(
                    fields.next().expect("validated variant field 0"),
                    registry,
                    capability,
                    minted_resource,
                )?,
            )),
            "Dialing" => Err(format!(
                "variant 'Tcp.Socket.Dialing' expected 1 field(s), got {}",
                field_count
            )),
            "Connected" if field_count == 1 => Ok(Self::Connected(
                <Tcp_Connection as aver_rt::provider::ProviderCodec>::from_provider_value(
                    fields.next().expect("validated variant field 0"),
                    registry,
                    capability,
                    minted_resource,
                )?,
            )),
            "Connected" => Err(format!(
                "variant 'Tcp.Socket.Connected' expected 1 field(s), got {}",
                field_count
            )),
            other => Err(format!("unknown variant 'Tcp.Socket.{}'", other)),
        }
    }
}

#[allow(non_camel_case_types)]
pub type Tcp_Socket = Socket;

impl aver_replay::ReplayValue for Socket {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Socket".to_string()),
        );
        match self {
            Socket::Listening(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("Listening".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Socket::Dialing(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("Dialing".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Socket::Connected(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("Connected".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "Socket" {
            return Err(format!(
                "$variant type mismatch: expected Socket, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "Listening" => Ok(Socket::Listening(
                <Tcp_Listener as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant Listening missing field #{}", 0))?,
                )?,
            )),
            "Dialing" => Ok(Socket::Dialing(
                <Tcp_Dial as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant Dialing missing field #{}", 0))?,
                )?,
            )),
            "Connected" => Ok(Socket::Connected(
                <Tcp_Connection as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant Connected missing field #{}", 0))?,
                )?,
            )),
            _ => Err(format!("unknown variant '{}' for Socket", variant_name)),
        }
    }
}
