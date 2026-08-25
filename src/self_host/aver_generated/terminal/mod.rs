#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Size {
    pub width: aver_rt::AverInt,
    pub height: aver_rt::AverInt,
}

impl PartialOrd for Size {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Size {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.height.cmp(&other.height))
            .then_with(|| self.width.cmp(&other.width))
    }
}

impl aver_rt::AverDisplay for Size {
    fn aver_display(&self) -> String {
        format!(
            "Size({})",
            vec![
                format!("width: {}", self.width.aver_display_inner()),
                format!("height: {}", self.height.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_rt::provider::ProviderCodec for Size {
    fn into_provider_value(
        self,
        registry: &aver_rt::provider::NativeProviderRegistry,
        capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        use aver_rt::provider::ProviderCodec as _;
        Ok(aver_rt::provider::ProviderValue::Record {
            type_name: "Terminal.Size".to_string(),
            fields: vec![
                (
                    "width".to_string(),
                    self.width.into_provider_value(registry, capability)?,
                ),
                (
                    "height".to_string(),
                    self.height.into_provider_value(registry, capability)?,
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
                "expected represented boundary type 'Terminal.Size', got {}",
                value.shape()
            ));
        };
        if type_name != "Terminal.Size" {
            return Err(format!(
                "expected represented boundary type 'Terminal.Size', got record {}",
                type_name
            ));
        }
        let mut by_name = std::collections::BTreeMap::new();
        for (field, value) in fields {
            if by_name.insert(field.clone(), value).is_some() {
                return Err(format!(
                    "record 'Terminal.Size' contains duplicate field '{}'",
                    field
                ));
            }
        }
        let decoded = Self {
            width: <aver_rt::AverInt as aver_rt::provider::ProviderCodec>::from_provider_value(
                by_name
                    .remove("width")
                    .ok_or_else(|| "record 'Terminal.Size' is missing field 'width'".to_string())?,
                registry,
                capability,
                minted_resource,
            )?,
            height: <aver_rt::AverInt as aver_rt::provider::ProviderCodec>::from_provider_value(
                by_name.remove("height").ok_or_else(|| {
                    "record 'Terminal.Size' is missing field 'height'".to_string()
                })?,
                registry,
                capability,
                minted_resource,
            )?,
        };
        if !by_name.is_empty() {
            return Err(format!(
                "record 'Terminal.Size' has unknown fields: {:?}",
                by_name.keys().collect::<Vec<_>>()
            ));
        }
        Ok(decoded)
    }
}

#[allow(non_camel_case_types)]
pub type Terminal_Size = Size;

impl aver_replay::ReplayValue for Size {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert(
            "width".to_string(),
            ReplayValue::to_replay_json(&self.width),
        );
        fields.insert(
            "height".to_string(),
            ReplayValue::to_replay_json(&self.height),
        );
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Size".to_string()),
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
        if type_name != "Size" {
            return Err(format!(
                "$record type mismatch: expected Size, got {}",
                type_name
            ));
        }
        let fields = aver_replay::expect_object(
            obj.get("fields")
                .ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            width: <aver_rt::AverInt as ReplayValue>::from_replay_json(
                fields
                    .get("width")
                    .ok_or_else(|| "$record Size missing field 'width'".to_string())?,
            )?,
            height: <aver_rt::AverInt as ReplayValue>::from_replay_json(
                fields
                    .get("height")
                    .ok_or_else(|| "$record Size missing field 'height'".to_string())?,
            )?,
        })
    }
}
