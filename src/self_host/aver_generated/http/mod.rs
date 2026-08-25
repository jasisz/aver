#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Response {
    pub status: aver_rt::AverInt,
    pub body: AverStr,
    pub headers: aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>>,
}

impl aver_rt::AverDisplay for Response {
    fn aver_display(&self) -> String {
        format!(
            "Response({})",
            vec![
                format!("status: {}", self.status.aver_display_inner()),
                format!("body: {}", self.body.aver_display_inner()),
                format!("headers: {}", self.headers.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_rt::provider::ProviderCodec for Response {
    fn into_provider_value(
        self,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        use aver_rt::provider::ProviderCodec as _;
        Ok(aver_rt::provider::ProviderValue::Record {
            type_name: "Http.Response".to_string(),
            fields: vec![
                (
                    "status".to_string(),
                    self.status.into_provider_value(registry, capability)?,
                ),
                (
                    "body".to_string(),
                    self.body.into_provider_value(registry, capability)?,
                ),
                (
                    "headers".to_string(),
                    self.headers.into_provider_value(registry, capability)?,
                ),
            ],
        })
    }

    fn from_provider_value(
        value: aver_rt::provider::ProviderValue,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
        minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        let aver_rt::provider::ProviderValue::Record { type_name, fields } = value else {
            return Err(format!(
                "expected represented boundary type 'Http.Response', got {}",
                value.shape()
            ));
        };
        if type_name != "Http.Response" {
            return Err(format!(
                "expected represented boundary type 'Http.Response', got record {}",
                type_name
            ));
        }
        let mut by_name = std::collections::BTreeMap::new();
        for (field, value) in fields {
            if by_name.insert(field.clone(), value).is_some() {
                return Err(format!(
                    "record 'Http.Response' contains duplicate field '{}'",
                    field
                ));
            }
        }
        let decoded = Self {
            status: <aver_rt::AverInt as aver_rt::provider::ProviderCodec>::from_provider_value(by_name.remove("status").ok_or_else(|| "record 'Http.Response' is missing field 'status'".to_string())?, registry, capability, minted_resource)?,
            body: <AverStr as aver_rt::provider::ProviderCodec>::from_provider_value(by_name.remove("body").ok_or_else(|| "record 'Http.Response' is missing field 'body'".to_string())?, registry, capability, minted_resource)?,
            headers: <aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>> as aver_rt::provider::ProviderCodec>::from_provider_value(by_name.remove("headers").ok_or_else(|| "record 'Http.Response' is missing field 'headers'".to_string())?, registry, capability, minted_resource)?,
        };
        if !by_name.is_empty() {
            return Err(format!(
                "record 'Http.Response' has unknown fields: {:?}",
                by_name.keys().collect::<Vec<_>>()
            ));
        }
        Ok(decoded)
    }
}

#[allow(non_camel_case_types)]
pub type Http_Response = Response;

impl aver_replay::ReplayValue for Response {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert(
            "status".to_string(),
            ReplayValue::to_replay_json(&self.status),
        );
        fields.insert("body".to_string(), ReplayValue::to_replay_json(&self.body));
        fields.insert(
            "headers".to_string(),
            ReplayValue::to_replay_json(&self.headers),
        );
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Response".to_string()),
        );
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$record missing field 'type'".to_string())?,
            "$record.type",
        )?;
        if type_name != "Response" {
            return Err(format!(
                "$record type mismatch: expected Response, got {}",
                type_name
            ));
        }
        let fields = aver_replay::expect_object(
            obj.get("fields")
                .ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
                status: <aver_rt::AverInt as ReplayValue>::from_replay_json(fields.get("status").ok_or_else(|| "$record Response missing field 'status'".to_string())?)?,
                body: <AverStr as ReplayValue>::from_replay_json(fields.get("body").ok_or_else(|| "$record Response missing field 'body'".to_string())?)?,
                headers: <aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>> as ReplayValue>::from_replay_json(fields.get("headers").ok_or_else(|| "$record Response missing field 'headers'".to_string())?)?,
        })
    }
}
