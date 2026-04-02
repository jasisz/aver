use crate::ast::{TypeDef, TypeVariant};

use super::syntax::aver_name_to_rust;
use super::types::type_annotation_to_rust;

pub fn generate_replay_runtime(
    has_embedded_policy: bool,
    has_runtime_policy: bool,
    has_terminal_types: bool,
    has_tcp_types: bool,
    has_http_types: bool,
    has_http_server_types: bool,
) -> String {
    let policy_check = if has_runtime_policy {
        RUNTIME_POLICY_CHECK_SNIPPET
    } else if has_embedded_policy {
        EMBEDDED_POLICY_CHECK_SNIPPET
    } else {
        "    #[derive(Clone, Debug, Default)]\n    struct RuntimePolicy;\n\n    fn load_runtime_policy_from_env() -> Result<Option<RuntimePolicy>, String> {\n        Ok(None)\n    }\n\n    fn check_policy(_effect_type: &str, _args: &[ReplayJson]) {}"
    };

    let mut sections = vec![REPLAY_RUNTIME_TEMPLATE.replace("__POLICY_CHECK__", policy_check)];

    if has_http_types {
        sections.push(http_type_impls());
    }
    if has_http_server_types {
        sections.push(http_server_type_impls());
    }
    if has_terminal_types {
        sections.push(terminal_type_impls());
    }
    if has_tcp_types {
        sections.push(tcp_type_impls());
    }

    format!("{}\n", sections.join("\n\n"))
}

pub fn emit_replay_value_impl(td: &TypeDef) -> String {
    match td {
        TypeDef::Product { name, fields, .. } => emit_record_impl(name, fields),
        TypeDef::Sum { name, variants, .. } => emit_variant_impl(name, variants),
    }
}

fn emit_record_impl(name: &str, fields: &[(String, String)]) -> String {
    let to_fields = fields
        .iter()
        .map(|(field_name, _)| {
            format!(
                "            fields.insert({:?}.to_string(), ReplayValue::to_replay_json(&self.{}));",
                field_name,
                aver_name_to_rust(field_name)
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let from_fields = fields
        .iter()
        .map(|(field_name, field_type)| {
            format!(
                "                {}: <{} as ReplayValue>::from_replay_json(fields.get({:?}).ok_or_else(|| {:?}.to_string())?)?,",
                aver_name_to_rust(field_name),
                type_annotation_to_rust(field_type),
                field_name,
                format!("$record {name} missing field '{field_name}'")
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        r#"impl aver_replay::ReplayValue for {name} {{
    fn to_replay_json(&self) -> serde_json::Value {{
        let mut fields = serde_json::Map::new();
{to_fields}
        let mut payload = serde_json::Map::new();
        payload.insert("type".to_string(), serde_json::Value::String({name:?}.to_string()));
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }}

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {{
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let type_name = aver_replay::expect_string(
            obj.get("type").ok_or_else(|| "$record missing field 'type'".to_string())?,
            "$record.type",
        )?;
        if type_name != {name:?} {{
            return Err(format!("$record type mismatch: expected {name}, got {{}}", type_name));
        }}
        let fields = aver_replay::expect_object(
            obj.get("fields").ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {{
{from_fields}
        }})
    }}
}}"#
    )
}

fn emit_variant_impl(name: &str, variants: &[TypeVariant]) -> String {
    let to_arms = variants
        .iter()
        .map(|variant| emit_variant_to_arm(name, variant))
        .collect::<Vec<_>>()
        .join("\n");

    let from_arms = variants
        .iter()
        .map(|variant| emit_variant_from_arm(name, variant))
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        r#"impl aver_replay::ReplayValue for {name} {{
    fn to_replay_json(&self) -> serde_json::Value {{
        let mut payload = serde_json::Map::new();
        payload.insert("type".to_string(), serde_json::Value::String({name:?}.to_string()));
        match self {{
{to_arms}
        }}
    }}

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {{
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type").ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != {name:?} {{
            return Err(format!("$variant type mismatch: expected {name}, got {{}}", type_name));
        }}
        let variant_name = aver_replay::expect_string(
            obj.get("name").ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields").ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {{
{from_arms}
            _ => Err(format!("unknown variant '{{}}' for {name}", variant_name)),
        }}
    }}
}}"#
    )
}

fn emit_variant_to_arm(type_name: &str, variant: &TypeVariant) -> String {
    if variant.fields.is_empty() {
        return format!(
            "            {type_name}::{variant_name} => {{\n                payload.insert(\"name\".to_string(), serde_json::Value::String({variant_name:?}.to_string()));\n                payload.insert(\"fields\".to_string(), serde_json::Value::Array(vec![]));\n                aver_replay::wrap_marker(\"$variant\", serde_json::Value::Object(payload))\n            }}",
            variant_name = variant.name
        );
    }

    let bindings = (0..variant.fields.len())
        .map(|idx| format!("f{}", idx))
        .collect::<Vec<_>>();
    let field_json = bindings
        .iter()
        .map(|name| format!("ReplayValue::to_replay_json({})", name))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "            {type_name}::{variant_name}({bindings}) => {{\n                payload.insert(\"name\".to_string(), serde_json::Value::String({variant_name:?}.to_string()));\n                payload.insert(\"fields\".to_string(), serde_json::Value::Array(vec![{field_json}]));\n                aver_replay::wrap_marker(\"$variant\", serde_json::Value::Object(payload))\n            }}",
        variant_name = variant.name,
        bindings = bindings.join(", ")
    )
}

fn emit_variant_from_arm(type_name: &str, variant: &TypeVariant) -> String {
    if variant.fields.is_empty() {
        return format!(
            "            {variant_name:?} => Ok({type_name}::{variant_name}),",
            variant_name = variant.name
        );
    }

    let field_loads = variant
        .fields
        .iter()
        .enumerate()
        .map(|(idx, field_type)| {
            format!(
                "<{} as ReplayValue>::from_replay_json(fields.get({}).ok_or_else(|| format!(\"$variant {} missing field #{{}}\", {}))?)?",
                replay_variant_field_type(type_name, field_type),
                idx,
                variant.name,
                idx
            )
        })
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "            {variant_name:?} => Ok({type_name}::{variant_name}({field_loads})),",
        variant_name = variant.name
    )
}

fn replay_variant_field_type(type_name: &str, field_type: &str) -> String {
    let rust_ty = type_annotation_to_rust(field_type);
    if field_type == type_name {
        format!("std::sync::Arc<{rust_ty}>")
    } else {
        rust_ty
    }
}

fn http_type_impls() -> String {
    r#"impl aver_replay::ReplayValue for aver_rt::Header {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert("name".to_string(), ReplayValue::to_replay_json(&self.name));
        fields.insert("value".to_string(), ReplayValue::to_replay_json(&self.value));
        let mut payload = serde_json::Map::new();
        payload.insert("type".to_string(), serde_json::Value::String("Header".to_string()));
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let fields = aver_replay::expect_object(
            obj.get("fields").ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            name: <aver_rt::AverStr as ReplayValue>::from_replay_json(
                fields.get("name").ok_or_else(|| "$record Header missing field 'name'".to_string())?,
            )?,
            value: <aver_rt::AverStr as ReplayValue>::from_replay_json(
                fields.get("value").ok_or_else(|| "$record Header missing field 'value'".to_string())?,
            )?,
        })
    }
}

impl aver_replay::ReplayValue for aver_rt::HttpResponse {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert("status".to_string(), ReplayValue::to_replay_json(&self.status));
        fields.insert("body".to_string(), ReplayValue::to_replay_json(&self.body));
        fields.insert("headers".to_string(), ReplayValue::to_replay_json(&self.headers));
        let mut payload = serde_json::Map::new();
        payload.insert("type".to_string(), serde_json::Value::String("HttpResponse".to_string()));
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let fields = aver_replay::expect_object(
            obj.get("fields").ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            status: <i64 as ReplayValue>::from_replay_json(
                fields.get("status").ok_or_else(|| "$record HttpResponse missing field 'status'".to_string())?,
            )?,
            body: <aver_rt::AverStr as ReplayValue>::from_replay_json(
                fields.get("body").ok_or_else(|| "$record HttpResponse missing field 'body'".to_string())?,
            )?,
            headers: <aver_rt::AverList<aver_rt::Header> as ReplayValue>::from_replay_json(
                fields.get("headers").ok_or_else(|| "$record HttpResponse missing field 'headers'".to_string())?,
            )?,
        })
    }
}"#
    .to_string()
}

fn http_server_type_impls() -> String {
    r#"impl aver_replay::ReplayValue for aver_rt::HttpRequest {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert("method".to_string(), ReplayValue::to_replay_json(&self.method));
        fields.insert("path".to_string(), ReplayValue::to_replay_json(&self.path));
        fields.insert("body".to_string(), ReplayValue::to_replay_json(&self.body));
        fields.insert("headers".to_string(), ReplayValue::to_replay_json(&self.headers));
        let mut payload = serde_json::Map::new();
        payload.insert("type".to_string(), serde_json::Value::String("HttpRequest".to_string()));
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let fields = aver_replay::expect_object(
            obj.get("fields").ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            method: <aver_rt::AverStr as ReplayValue>::from_replay_json(
                fields.get("method").ok_or_else(|| "$record HttpRequest missing field 'method'".to_string())?,
            )?,
            path: <aver_rt::AverStr as ReplayValue>::from_replay_json(
                fields.get("path").ok_or_else(|| "$record HttpRequest missing field 'path'".to_string())?,
            )?,
            body: <aver_rt::AverStr as ReplayValue>::from_replay_json(
                fields.get("body").ok_or_else(|| "$record HttpRequest missing field 'body'".to_string())?,
            )?,
            headers: <aver_rt::AverList<aver_rt::Header> as ReplayValue>::from_replay_json(
                fields.get("headers").ok_or_else(|| "$record HttpRequest missing field 'headers'".to_string())?,
            )?,
        })
    }
}"#
    .to_string()
}

fn terminal_type_impls() -> String {
    r#"impl aver_replay::ReplayValue for aver_rt::TerminalSize {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert("width".to_string(), ReplayValue::to_replay_json(&self.width));
        fields.insert("height".to_string(), ReplayValue::to_replay_json(&self.height));
        let mut payload = serde_json::Map::new();
        payload.insert("type".to_string(), serde_json::Value::String("Terminal.Size".to_string()));
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let fields = aver_replay::expect_object(
            obj.get("fields").ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            width: <i64 as ReplayValue>::from_replay_json(
                fields.get("width").ok_or_else(|| "$record Terminal.Size missing field 'width'".to_string())?,
            )?,
            height: <i64 as ReplayValue>::from_replay_json(
                fields.get("height").ok_or_else(|| "$record Terminal.Size missing field 'height'".to_string())?,
            )?,
        })
    }
}"#
    .to_string()
}

fn tcp_type_impls() -> String {
    r#"impl aver_replay::ReplayValue for aver_rt::TcpConnection {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert("id".to_string(), ReplayValue::to_replay_json(&self.id));
        fields.insert("host".to_string(), ReplayValue::to_replay_json(&self.host));
        fields.insert("port".to_string(), ReplayValue::to_replay_json(&self.port));
        let mut payload = serde_json::Map::new();
        payload.insert("type".to_string(), serde_json::Value::String("Tcp.Connection".to_string()));
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let fields = aver_replay::expect_object(
            obj.get("fields").ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            id: <aver_rt::AverStr as ReplayValue>::from_replay_json(
                fields.get("id").ok_or_else(|| "$record Tcp.Connection missing field 'id'".to_string())?,
            )?,
            host: <aver_rt::AverStr as ReplayValue>::from_replay_json(
                fields.get("host").ok_or_else(|| "$record Tcp.Connection missing field 'host'".to_string())?,
            )?,
            port: <i64 as ReplayValue>::from_replay_json(
                fields.get("port").ok_or_else(|| "$record Tcp.Connection missing field 'port'".to_string())?,
            )?,
        })
    }
}"#
    .to_string()
}

const EMBEDDED_POLICY_CHECK_SNIPPET: &str = r#"    #[derive(Clone, Debug, Default)]
    struct RuntimePolicy;

    fn load_runtime_policy_from_env() -> Result<Option<RuntimePolicy>, String> {
        Ok(None)
    }

    fn check_policy(effect_type: &str, args: &[ReplayJson]) {
        match (effect_type.split('.').next(), args.first().and_then(|value| value.as_str())) {
            (Some("Http"), Some(url)) => {
                aver_policy::check_http(effect_type, url)
                    .expect("aver.toml policy violation");
            }
            (Some("Disk"), Some(path)) => {
                aver_policy::check_disk(effect_type, path)
                    .expect("aver.toml policy violation");
            }
            (Some("Env"), Some(key)) => {
                aver_policy::check_env(effect_type, key)
                    .expect("aver.toml policy violation");
            }
            _ => {}
        }
    }"#;

const RUNTIME_POLICY_CHECK_SNIPPET: &str = r#"    #[derive(Clone, Debug, Default)]
    struct RuntimeEffectPolicy {
        hosts: Vec<String>,
        paths: Vec<String>,
        keys: Vec<String>,
    }

    #[derive(Clone, Debug, Default)]
    struct RuntimePolicy {
        effect_policies: BTreeMap<String, RuntimeEffectPolicy>,
    }

    impl RuntimePolicy {
        fn load_from_dir(dir: &Path) -> Result<Option<Self>, String> {
            let path = dir.join("aver.toml");
            let content = match std::fs::read_to_string(&path) {
                Ok(content) => content,
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
                Err(err) => return Err(format!("Failed to read {}: {}", path.display(), err)),
            };
            Self::parse(&content).map(Some)
        }

        fn parse(content: &str) -> Result<Self, String> {
            let table: toml::Table = content
                .parse()
                .map_err(|err: toml::de::Error| format!("aver.toml parse error: {}", err))?;

            let mut effect_policies = BTreeMap::new();
            if let Some(toml::Value::Table(effects_table)) = table.get("effects") {
                for (name, value) in effects_table {
                    let section = value
                        .as_table()
                        .ok_or_else(|| format!("aver.toml: [effects.{}] must be a table", name))?;
                    effect_policies.insert(
                        name.clone(),
                        RuntimeEffectPolicy {
                            hosts: parse_policy_list(section, "hosts", name)?,
                            paths: parse_policy_list(section, "paths", name)?,
                            keys: parse_policy_list(section, "keys", name)?,
                        },
                    );
                }
            }

            Ok(Self { effect_policies })
        }

        fn check_http(&self, method_name: &str, url_str: &str) -> Result<(), String> {
            let Some(policy) = self.find_policy(method_name) else {
                return Ok(());
            };
            if policy.hosts.is_empty() {
                return Ok(());
            }
            let parsed = url::Url::parse(url_str).map_err(|err| {
                format!(
                    "{} denied by aver.toml: invalid URL '{}': {}",
                    method_name, url_str, err
                )
            })?;
            let host = parsed.host_str().unwrap_or("");
            for allowed in &policy.hosts {
                if host_matches(host, allowed) {
                    return Ok(());
                }
            }
            Err(format!(
                "{} to '{}' denied by aver.toml policy (host '{}' not in allowed list)",
                method_name, url_str, host
            ))
        }

        fn check_disk(&self, method_name: &str, path_str: &str) -> Result<(), String> {
            let Some(policy) = self.find_policy(method_name) else {
                return Ok(());
            };
            if policy.paths.is_empty() {
                return Ok(());
            }
            let normalized = normalize_path(path_str);
            for allowed in &policy.paths {
                if path_matches(&normalized, allowed) {
                    return Ok(());
                }
            }
            Err(format!(
                "{} on '{}' denied by aver.toml policy (path not in allowed list)",
                method_name, path_str
            ))
        }

        fn check_env(&self, method_name: &str, key: &str) -> Result<(), String> {
            let Some(policy) = self.find_policy(method_name) else {
                return Ok(());
            };
            if policy.keys.is_empty() {
                return Ok(());
            }
            for allowed in &policy.keys {
                if env_key_matches(key, allowed) {
                    return Ok(());
                }
            }
            Err(format!(
                "{} on '{}' denied by aver.toml policy (key not in allowed list)",
                method_name, key
            ))
        }

        fn find_policy(&self, method_name: &str) -> Option<&RuntimeEffectPolicy> {
            let namespace = method_name.split('.').next().unwrap_or(method_name);
            self.effect_policies
                .get(method_name)
                .or_else(|| self.effect_policies.get(namespace))
        }
    }

    fn parse_policy_list(
        section: &toml::Table,
        key: &str,
        name: &str,
    ) -> Result<Vec<String>, String> {
        let Some(value) = section.get(key) else {
            return Ok(Vec::new());
        };
        let arr = value
            .as_array()
            .ok_or_else(|| format!("aver.toml: [effects.{}].{} must be an array", name, key))?;
        arr.iter()
            .enumerate()
            .map(|(idx, value)| {
                value.as_str().map(|item| item.to_string()).ok_or_else(|| {
                    format!(
                        "aver.toml: [effects.{}].{}[{}] must be a string",
                        name, key, idx
                    )
                })
            })
            .collect()
    }

    fn check_policy(effect_type: &str, args: &[ReplayJson]) {
        let policy = SCOPE_STATE.with(|cell| match &*cell.borrow() {
            ScopeState::Inactive => None,
            ScopeState::Active(scope) => scope.runtime_policy.as_ref().cloned(),
        });
        let Some(policy) = policy else {
            return;
        };

        match (effect_type.split('.').next(), args.first().and_then(|value| value.as_str())) {
            (Some("Http"), Some(url)) => {
                policy
                    .check_http(effect_type, url)
                    .expect("aver.toml policy violation");
            }
            (Some("Disk"), Some(path)) => {
                policy
                    .check_disk(effect_type, path)
                    .expect("aver.toml policy violation");
            }
            (Some("Env"), Some(key)) => {
                policy
                    .check_env(effect_type, key)
                    .expect("aver.toml policy violation");
            }
            _ => {}
        }
    }

    fn load_runtime_policy_from_env() -> Result<Option<RuntimePolicy>, String> {
        let module_root = env_var("AVER_REPLAY_MODULE_ROOT").unwrap_or_else(|| ".".to_string());
        RuntimePolicy::load_from_dir(Path::new(&module_root)).map_err(|err| format!("aver.toml: {}", err))
    }

    fn host_matches(host: &str, pattern: &str) -> bool {
        if pattern == host {
            return true;
        }
        if let Some(suffix) = pattern.strip_prefix("*.") {
            host.ends_with(suffix)
                && host.len() > suffix.len()
                && host.as_bytes()[host.len() - suffix.len() - 1] == b'.'
        } else {
            false
        }
    }

    fn normalize_path(path: &str) -> String {
        use std::path::Component;

        let mut components: Vec<String> = Vec::new();
        let mut is_absolute = false;

        for component in Path::new(path).components() {
            match component {
                Component::RootDir => {
                    is_absolute = true;
                    components.clear();
                }
                Component::CurDir => {}
                Component::ParentDir => {
                    if components.last().is_some_and(|item| item != "..") {
                        components.pop();
                    } else if !is_absolute {
                        components.push("..".to_string());
                    }
                }
                Component::Normal(segment) => {
                    components.push(segment.to_string_lossy().to_string());
                }
                Component::Prefix(prefix) => {
                    components.push(prefix.as_os_str().to_string_lossy().to_string());
                }
            }
        }

        let joined = components.join("/");
        if is_absolute {
            format!("/{}", joined)
        } else {
            joined
        }
    }

    fn path_matches(normalized: &str, pattern: &str) -> bool {
        let clean_pattern = normalize_path(pattern.strip_suffix("/**").unwrap_or(pattern));
        if normalized == clean_pattern {
            return true;
        }
        if normalized.starts_with(&clean_pattern) {
            let rest = &normalized[clean_pattern.len()..];
            if rest.starts_with('/') {
                return true;
            }
        }
        false
    }

    fn env_key_matches(key: &str, pattern: &str) -> bool {
        if pattern == key {
            return true;
        }
        if let Some(prefix) = pattern.strip_suffix('*') {
            key.starts_with(prefix)
        } else {
            false
        }
    }"#;

const REPLAY_RUNTIME_TEMPLATE: &str = r#"pub mod aver_replay {
    use std::cell::RefCell;
    use std::collections::BTreeMap;
    use std::hash::Hash;
    use std::path::{Path, PathBuf};

    use crate::IntoAverStr;
    use serde::{Deserialize, Serialize};
    use serde_json::Value as ReplayJson;

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    #[serde(tag = "kind")]
    pub enum RecordedOutcome {
        #[serde(rename = "value")]
        Value { value: ReplayJson },
        #[serde(rename = "runtime_error")]
        RuntimeError { message: String },
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    pub struct EffectRecord {
        pub seq: u32,
        #[serde(rename = "type")]
        pub effect_type: String,
        pub args: Vec<ReplayJson>,
        pub outcome: RecordedOutcome,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub group_id: Option<u32>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub branch_path: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub effect_occurrence: Option<u32>,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    pub struct SessionRecording {
        pub schema_version: u32,
        pub request_id: String,
        pub timestamp: String,
        pub program_file: String,
        pub module_root: String,
        pub entry_fn: String,
        pub input: ReplayJson,
        pub effects: Vec<EffectRecord>,
        pub output: RecordedOutcome,
    }

    pub trait ReplayValue: Sized {
        fn to_replay_json(&self) -> ReplayJson;
        fn from_replay_json(value: &ReplayJson) -> Result<Self, String>;
    }

    pub trait ReplayKey {
        fn replay_string_key(&self) -> Option<String>;
    }

    impl ReplayKey for aver_rt::AverStr {
        fn replay_string_key(&self) -> Option<String> {
            Some(self.to_string())
        }
    }

    impl ReplayValue for () {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::Null
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            match value {
                ReplayJson::Null => Ok(()),
                _ => Err("expected null".to_string()),
            }
        }
    }

    impl ReplayValue for bool {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::Bool(*self)
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            value.as_bool().ok_or_else(|| "expected bool".to_string())
        }
    }

    impl ReplayValue for i64 {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::Number((*self).into())
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            value.as_i64().ok_or_else(|| "expected int".to_string())
        }
    }

    impl ReplayValue for f64 {
        fn to_replay_json(&self) -> ReplayJson {
            let number = serde_json::Number::from_f64(*self)
                .expect("replay cannot encode non-finite float");
            ReplayJson::Number(number)
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            value.as_f64().ok_or_else(|| "expected float".to_string())
        }
    }

    impl ReplayValue for aver_rt::AverStr {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::String(self.to_string())
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            value
                .as_str()
                .map(aver_rt::AverStr::from)
                .ok_or_else(|| "expected string".to_string())
        }
    }

    impl<T: ReplayValue> ReplayValue for std::sync::Arc<T> {
        fn to_replay_json(&self) -> ReplayJson {
            (**self).to_replay_json()
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            Ok(std::sync::Arc::new(T::from_replay_json(value)?))
        }
    }

    impl<T: ReplayValue> ReplayValue for Option<T> {
        fn to_replay_json(&self) -> ReplayJson {
            match self {
                Some(value) => wrap_marker("$some", value.to_replay_json()),
                None => wrap_marker("$none", ReplayJson::Bool(true)),
            }
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            if let Some(payload) = marker_payload(value, "$some") {
                return Ok(Some(T::from_replay_json(payload)?));
            }
            if marker_payload(value, "$none").is_some() {
                return Ok(None);
            }
            Err("expected Option replay marker".to_string())
        }
    }

    impl<T: ReplayValue, E: ReplayValue> ReplayValue for Result<T, E> {
        fn to_replay_json(&self) -> ReplayJson {
            match self {
                Ok(value) => wrap_marker("$ok", value.to_replay_json()),
                Err(value) => wrap_marker("$err", value.to_replay_json()),
            }
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            if let Some(payload) = marker_payload(value, "$ok") {
                return Ok(Ok(T::from_replay_json(payload)?));
            }
            if let Some(payload) = marker_payload(value, "$err") {
                return Ok(Err(E::from_replay_json(payload)?));
            }
            Err("expected Result replay marker".to_string())
        }
    }

    impl<T: ReplayValue> ReplayValue for aver_rt::AverList<T> {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::Array(self.iter().map(ReplayValue::to_replay_json).collect())
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            let arr = expect_array(value, "list")?;
            let mut items = Vec::with_capacity(arr.len());
            for item in arr {
                items.push(T::from_replay_json(item)?);
            }
            Ok(aver_rt::AverList::from_vec(items))
        }
    }

    impl<T: ReplayValue + Clone> ReplayValue for aver_rt::AverVector<T> {
        fn to_replay_json(&self) -> ReplayJson {
            wrap_marker(
                "$vector",
                ReplayJson::Array(self.iter().map(ReplayValue::to_replay_json).collect()),
            )
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            let payload = expect_marker(value, "$vector")?;
            let arr = expect_array(payload, "$vector")?;
            let mut items = Vec::with_capacity(arr.len());
            for item in arr {
                items.push(T::from_replay_json(item)?);
            }
            Ok(aver_rt::AverVector::from_vec(items))
        }
    }

    impl<K, V> ReplayValue for aver_rt::AverMap<K, V>
    where
        K: ReplayValue + ReplayKey + Eq + Hash + Clone,
        V: ReplayValue + Clone,
    {
        fn to_replay_json(&self) -> ReplayJson {
            if self.iter().all(|(key, _)| key.replay_string_key().is_some()) {
                let mut obj = serde_json::Map::new();
                for (key, value) in self.iter() {
                    let key_str = key.replay_string_key().expect("checked above");
                    obj.insert(key_str, value.to_replay_json());
                }
                ReplayJson::Object(obj)
            } else {
                let pairs = self
                    .iter()
                    .map(|(key, value)| {
                        ReplayJson::Array(vec![key.to_replay_json(), value.to_replay_json()])
                    })
                    .collect();
                wrap_marker("$map", ReplayJson::Array(pairs))
            }
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            match value {
                ReplayJson::Object(obj) => {
                    let mut map = aver_rt::AverMap::new();
                    for (key, value) in obj {
                        map = map.insert_owned(
                            K::from_replay_json(&ReplayJson::String(key.clone()))?,
                            V::from_replay_json(value)?,
                        );
                    }
                    Ok(map)
                }
                _ => {
                    let payload = expect_marker(value, "$map")?;
                    let arr = expect_array(payload, "$map")?;
                    let mut map = aver_rt::AverMap::new();
                    for (idx, pair) in arr.iter().enumerate() {
                        let pair_arr = expect_array(pair, &format!("$map[{idx}]"))?;
                        if pair_arr.len() != 2 {
                            return Err(format!("$map[{idx}] must be a 2-element array"));
                        }
                        map = map.insert_owned(
                            K::from_replay_json(&pair_arr[0])?,
                            V::from_replay_json(&pair_arr[1])?,
                        );
                    }
                    Ok(map)
                }
            }
        }
    }

    macro_rules! impl_tuple_replay {
        ($($name:ident : $idx:tt),+) => {
            impl<$($name: ReplayValue),+> ReplayValue for ($($name,)+) {
                fn to_replay_json(&self) -> ReplayJson {
                    wrap_marker(
                        "$tuple",
                        ReplayJson::Array(vec![$(self.$idx.to_replay_json(),)+]),
                    )
                }

                fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
                    let payload = expect_marker(value, "$tuple")?;
                    let arr = expect_array(payload, "$tuple")?;
                    Ok((
                        $(
                            <$name as ReplayValue>::from_replay_json(
                                arr.get($idx).ok_or_else(|| format!("$tuple missing item {}", $idx))?,
                            )?,
                        )+
                    ))
                }
            }
        };
    }

    impl_tuple_replay!(A: 0, B: 1);
    impl_tuple_replay!(A: 0, B: 1, C: 2);
    impl_tuple_replay!(A: 0, B: 1, C: 2, D: 3);
    impl_tuple_replay!(A: 0, B: 1, C: 2, D: 3, E: 4);
    impl_tuple_replay!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5);

    #[derive(Clone)]
    enum ScopeMode {
        Normal,
        Record {
            path: PathBuf,
            session: SessionRecording,
        },
        Replay {
            session: SessionRecording,
            position: usize,
            check_args: bool,
        },
    }

    #[derive(Clone)]
    struct ActiveScope {
        mode: ScopeMode,
        guest_args: Option<aver_rt::AverList<crate::AverStr>>,
        runtime_policy: Option<RuntimePolicy>,
        group_stack: Vec<u32>,
        branch_stack: Vec<u32>,
        effect_emission_count: u32,
        next_group_id: u32,
    }

    #[derive(Clone)]
    enum ScopeState {
        Inactive,
        Active(ActiveScope),
    }

    thread_local! {
        static SCOPE_STATE: RefCell<ScopeState> = const { RefCell::new(ScopeState::Inactive) };
    }

    pub fn entry_input(args: Vec<ReplayJson>) -> ReplayJson {
        match args.len() {
            0 => ReplayJson::Null,
            1 => args.into_iter().next().expect("single input"),
            _ => ReplayJson::Array(args),
        }
    }

    pub fn with_guest_scope<T, F>(entry_fn: &str, input: ReplayJson, run: F) -> T
    where
        T: ReplayValue,
        F: FnOnce() -> T,
    {
        with_guest_scope_args_inner(entry_fn, input, None, run)
    }

    pub fn with_guest_scope_result<T, E, F>(entry_fn: &str, input: ReplayJson, run: F) -> Result<T, E>
    where
        T: ReplayValue,
        E: aver_rt::AverDisplay,
        F: FnOnce() -> Result<T, E>,
    {
        with_guest_scope_result_args_inner(entry_fn, input, None, run)
    }

    pub fn with_guest_scope_args<T, F>(
        entry_fn: &str,
        input: ReplayJson,
        guest_args: aver_rt::AverList<crate::AverStr>,
        run: F,
    ) -> T
    where
        T: ReplayValue,
        F: FnOnce() -> T,
    {
        with_guest_scope_args_inner(entry_fn, input, Some(guest_args), run)
    }

    pub fn with_guest_scope_args_result<T, E, F>(
        entry_fn: &str,
        input: ReplayJson,
        guest_args: aver_rt::AverList<crate::AverStr>,
        run: F,
    ) -> Result<T, E>
    where
        T: ReplayValue,
        E: aver_rt::AverDisplay,
        F: FnOnce() -> Result<T, E>,
    {
        with_guest_scope_result_args_inner(entry_fn, input, Some(guest_args), run)
    }

    fn with_guest_scope_args_inner<T, F>(
        entry_fn: &str,
        input: ReplayJson,
        guest_args: Option<aver_rt::AverList<crate::AverStr>>,
        run: F,
    ) -> T
    where
        T: ReplayValue,
        F: FnOnce() -> T,
    {
        if scope_is_active() {
            return run();
        }

        let mode = load_scope_mode(entry_fn, input.clone());
        activate_scope(mode.clone(), guest_args);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(run));
        match result {
            Ok(value) => {
                finish_scope_success(&value);
                clear_scope();
                value
            }
            Err(payload) => {
                let panic_message = panic_payload_to_string(&payload);
                finish_scope_panic(&panic_message);
                clear_scope();
                std::panic::resume_unwind(payload);
            }
        }
    }

    fn with_guest_scope_result_args_inner<T, E, F>(
        entry_fn: &str,
        input: ReplayJson,
        guest_args: Option<aver_rt::AverList<crate::AverStr>>,
        run: F,
    ) -> Result<T, E>
    where
        T: ReplayValue,
        E: aver_rt::AverDisplay,
        F: FnOnce() -> Result<T, E>,
    {
        if scope_is_active() {
            return run();
        }

        let mode = load_scope_mode(entry_fn, input.clone());
        activate_scope(mode.clone(), guest_args);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(run));
        match result {
            Ok(Ok(value)) => {
                finish_scope_success(&value);
                clear_scope();
                Ok(value)
            }
            Ok(Err(err)) => {
                finish_scope_error(entry_fn, &err);
                clear_scope();
                Err(err)
            }
            Err(payload) => {
                let panic_message = panic_payload_to_string(&payload);
                finish_scope_panic(&panic_message);
                clear_scope();
                std::panic::resume_unwind(payload);
            }
        }
    }

    pub fn current_cli_args() -> aver_rt::AverList<crate::AverStr> {
        SCOPE_STATE.with(|cell| match &*cell.borrow() {
            ScopeState::Inactive => aver_rt::cli_args().into_aver(),
            ScopeState::Active(scope) => match &scope.guest_args {
                Some(args) => args.clone(),
                None => aver_rt::cli_args().into_aver(),
            },
        })
    }

    pub fn is_record_mode() -> bool {
        matches!(current_scope_mode(), Some(ScopeMode::Record { .. }))
    }

    /// True when effects are being recorded or replayed — callers should
    /// execute ?!/! elements sequentially so effect tracking works correctly.
    pub fn is_effect_tracking_active() -> bool {
        matches!(
            current_scope_mode(),
            Some(ScopeMode::Record { .. } | ScopeMode::Replay { .. })
        )
    }

    pub fn enter_effect_group() {
        SCOPE_STATE.with(|cell| {
            if let ScopeState::Active(scope) = &mut *cell.borrow_mut() {
                scope.next_group_id += 1;
                scope.group_stack.push(scope.next_group_id);
                scope.branch_stack.push(0);
            }
        });
    }

    pub fn exit_effect_group() {
        SCOPE_STATE.with(|cell| {
            if let ScopeState::Active(scope) = &mut *cell.borrow_mut() {
                scope.group_stack.pop();
                scope.branch_stack.pop();
            }
        });
    }

    pub fn set_effect_branch(index: u32) {
        SCOPE_STATE.with(|cell| {
            if let ScopeState::Active(scope) = &mut *cell.borrow_mut() {
                if let Some(last) = scope.branch_stack.last_mut() {
                    *last = index;
                }
                scope.effect_emission_count = 0;
            }
        });
    }

    pub fn invoke_effect<T, F>(effect_type: &str, args: Vec<ReplayJson>, call: F) -> T
    where
        T: ReplayValue,
        F: FnOnce() -> T,
    {
        let mode = current_scope_mode();
        match mode {
            None => call(),
            Some(ScopeMode::Normal) => {
                check_policy(effect_type, &args);
                call()
            }
            Some(ScopeMode::Record { .. }) => {
                check_policy(effect_type, &args);
                let result = call();
                let outcome = RecordedOutcome::Value {
                    value: result.to_replay_json(),
                };
                SCOPE_STATE.with(|cell| {
                    if let ScopeState::Active(scope) = &mut *cell.borrow_mut() {
                        let group_id = scope.group_stack.last().copied();
                        if let ScopeMode::Record { session, .. } = &mut scope.mode {
                            let seq = session.effects.len() as u32 + 1;
                            session.effects.push(EffectRecord {
                                seq,
                                effect_type: effect_type.to_string(),
                                args,
                                outcome,
                                group_id,
                                branch_path: if scope.branch_stack.is_empty() {
                                    None
                                } else {
                                    Some(scope.branch_stack.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("."))
                                },
                                effect_occurrence: if scope.branch_stack.is_empty() {
                                    None
                                } else {
                                    let occ = scope.effect_emission_count;
                                    scope.effect_emission_count += 1;
                                    Some(occ)
                                },
                            });
                        }
                    }
                });
                result
            }
            Some(ScopeMode::Replay { .. }) => replay_effect(effect_type, args),
        }
    }

__POLICY_CHECK__

    pub fn wrap_marker(name: &str, value: ReplayJson) -> ReplayJson {
        let mut obj = serde_json::Map::new();
        obj.insert(name.to_string(), value);
        ReplayJson::Object(obj)
    }

    pub fn expect_marker<'a>(value: &'a ReplayJson, marker: &str) -> Result<&'a ReplayJson, String> {
        marker_payload(value, marker)
            .ok_or_else(|| format!("expected replay marker '{}'", marker))
    }

    pub fn expect_object<'a>(
        value: &'a ReplayJson,
        path: &str,
    ) -> Result<&'a serde_json::Map<String, ReplayJson>, String> {
        match value {
            ReplayJson::Object(obj) => Ok(obj),
            _ => Err(format!("{} must be an object", path)),
        }
    }

    pub fn expect_array<'a>(value: &'a ReplayJson, path: &str) -> Result<&'a Vec<ReplayJson>, String> {
        match value {
            ReplayJson::Array(arr) => Ok(arr),
            _ => Err(format!("{} must be an array", path)),
        }
    }

    pub fn expect_string<'a>(value: &'a ReplayJson, path: &str) -> Result<&'a str, String> {
        value
            .as_str()
            .ok_or_else(|| format!("{} must be a string", path))
    }

    fn marker_payload<'a>(value: &'a ReplayJson, marker: &str) -> Option<&'a ReplayJson> {
        match value {
            ReplayJson::Object(obj) if obj.len() == 1 => obj.get(marker),
            _ => None,
        }
    }

    fn scope_is_active() -> bool {
        SCOPE_STATE.with(|cell| matches!(*cell.borrow(), ScopeState::Active(_)))
    }

    fn current_scope_mode() -> Option<ScopeMode> {
        SCOPE_STATE.with(|cell| match &*cell.borrow() {
            ScopeState::Inactive => None,
            ScopeState::Active(scope) => Some(scope.mode.clone()),
        })
    }

    fn activate_scope(mode: ScopeMode, guest_args: Option<aver_rt::AverList<crate::AverStr>>) {
        let runtime_policy = match &mode {
            ScopeMode::Replay { .. } => None,
            ScopeMode::Normal | ScopeMode::Record { .. } => load_runtime_policy_from_env()
                .unwrap_or_else(|err| panic!("{}", err)),
        };
        SCOPE_STATE.with(|cell| {
            *cell.borrow_mut() = ScopeState::Active(ActiveScope {
                mode,
                guest_args,
                runtime_policy,
                group_stack: Vec::new(),
                branch_stack: Vec::new(),
                effect_emission_count: 0,
                next_group_id: 0,
            });
        });
    }

    fn clear_scope() {
        SCOPE_STATE.with(|cell| {
            *cell.borrow_mut() = ScopeState::Inactive;
        });
    }

    fn replay_entry_name(entry_fn: &str) -> String {
        env_var("AVER_REPLAY_ENTRY_FN").unwrap_or_else(|| entry_fn.to_string())
    }

    fn load_scope_mode(entry_fn: &str, input: ReplayJson) -> ScopeMode {
        let logical_entry_fn = replay_entry_name(entry_fn);
        let record_path = env_var("AVER_REPLAY_RECORD");
        let replay_path = env_var("AVER_REPLAY_REPLAY");
        if record_path.is_some() && replay_path.is_some() {
            panic!("AVER_REPLAY_RECORD and AVER_REPLAY_REPLAY cannot both be set");
        }

        if let Some(path) = replay_path {
            let raw = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("Cannot read replay recording '{}': {}", path, e));
            let session: SessionRecording = serde_json::from_str(&raw)
                .unwrap_or_else(|e| panic!("Invalid replay recording '{}': {}", path, e));
            if session.entry_fn != logical_entry_fn {
                panic!(
                    "Replay entry mismatch: recording expects '{}', generated guest scope is '{}'",
                    session.entry_fn, logical_entry_fn
                );
            }
            return ScopeMode::Replay {
                session,
                position: 0,
                check_args: env_flag("AVER_REPLAY_CHECK_ARGS"),
            };
        }

        if let Some(path) = record_path {
            let request_id = env_var("AVER_REPLAY_REQUEST_ID")
                .unwrap_or_else(default_request_id);
            let timestamp = env_var("AVER_REPLAY_TIMESTAMP")
                .unwrap_or_else(default_timestamp);
            let program_file = env_var("AVER_REPLAY_PROGRAM_FILE").unwrap_or_default();
            let module_root =
                env_var("AVER_REPLAY_MODULE_ROOT").unwrap_or_else(|| ".".to_string());
            return ScopeMode::Record {
                path: PathBuf::from(path),
                session: SessionRecording {
                    schema_version: 1,
                    request_id,
                    timestamp,
                    program_file,
                    module_root,
                    entry_fn: logical_entry_fn,
                    input,
                    effects: Vec::new(),
                    output: RecordedOutcome::Value {
                        value: ReplayJson::Null,
                    },
                },
            };
        }

        ScopeMode::Normal
    }

    fn finish_scope_success<T: ReplayValue>(value: &T) {
        SCOPE_STATE.with(|cell| {
            let mut state = cell.borrow_mut();
            let ScopeState::Active(scope) = &mut *state else {
                return;
            };
            match &mut scope.mode {
                ScopeMode::Normal => {}
                ScopeMode::Record { path, session } => {
                    session.output = RecordedOutcome::Value {
                        value: value.to_replay_json(),
                    };
                    write_recording(path, session);
                }
                ScopeMode::Replay {
                    session,
                    position,
                    ..
                } => {
                    if *position != session.effects.len() {
                        panic!(
                            "Replay finished with {} unconsumed recorded effect(s)",
                            session.effects.len().saturating_sub(*position)
                        );
                    }
                    let actual = RecordedOutcome::Value {
                        value: value.to_replay_json(),
                    };
                    if actual != session.output {
                        panic!(
                            "Replay output mismatch for '{}': expected {:?}, got {:?}",
                            session.entry_fn, session.output, actual
                        );
                    }
                }
            }
        });
    }

    fn returned_error_message<E: aver_rt::AverDisplay>(entry_fn: &str, err: &E) -> String {
        let logical_entry_fn = replay_entry_name(entry_fn);
        if logical_entry_fn == "main" {
            format!("Main returned error: {}", err.aver_display())
        } else {
            format!("{} returned error: {}", logical_entry_fn, err.aver_display())
        }
    }

    fn finish_scope_error<E: aver_rt::AverDisplay>(entry_fn: &str, err: &E) {
        let message = returned_error_message(entry_fn, err);
        SCOPE_STATE.with(|cell| {
            let mut state = cell.borrow_mut();
            let ScopeState::Active(scope) = &mut *state else {
                return;
            };
            match &mut scope.mode {
                ScopeMode::Normal => {}
                ScopeMode::Record { path, session } => {
                    session.output = RecordedOutcome::RuntimeError {
                        message: message.clone(),
                    };
                    write_recording(path, session);
                }
                ScopeMode::Replay {
                    session,
                    position,
                    ..
                } => {
                    if *position != session.effects.len() {
                        panic!(
                            "Replay finished with {} unconsumed recorded effect(s)",
                            session.effects.len().saturating_sub(*position)
                        );
                    }
                    let actual = RecordedOutcome::RuntimeError {
                        message: message.clone(),
                    };
                    if actual != session.output {
                        panic!(
                            "Replay output mismatch for '{}': expected {:?}, got {:?}",
                            session.entry_fn, session.output, actual
                        );
                    }
                }
            }
        });
    }

    fn finish_scope_panic(message: &str) {
        SCOPE_STATE.with(|cell| {
            let mut state = cell.borrow_mut();
            let ScopeState::Active(scope) = &mut *state else {
                return;
            };
            match &mut scope.mode {
                ScopeMode::Normal => {}
                ScopeMode::Record { path, session } => {
                    session.output = RecordedOutcome::RuntimeError {
                        message: message.to_string(),
                    };
                    write_recording(path, session);
                }
                ScopeMode::Replay {
                    session,
                    position,
                    ..
                } => {
                    if *position != session.effects.len() {
                        panic!(
                            "Replay finished with {} unconsumed recorded effect(s)",
                            session.effects.len().saturating_sub(*position)
                        );
                    }
                    let actual = RecordedOutcome::RuntimeError {
                        message: message.to_string(),
                    };
                    if actual != session.output {
                        panic!(
                            "Replay output mismatch for '{}': expected {:?}, got {:?}",
                            session.entry_fn, session.output, actual
                        );
                    }
                }
            }
        });
    }

    fn replay_effect<T: ReplayValue>(effect_type: &str, args: Vec<ReplayJson>) -> T {
        let record = SCOPE_STATE.with(|cell| {
            let mut state = cell.borrow_mut();
            let ScopeState::Active(scope) = &mut *state
            else {
                panic!("replay scope is not active");
            };
            let ScopeMode::Replay {
                session,
                position,
                check_args,
            } = &mut scope.mode
            else {
                panic!("replay scope is not active");
            };

            let Some(record) = session.effects.get(*position).cloned() else {
                panic!("Replay exhausted: no more recorded effects for '{}'", effect_type);
            };

            // Group-aware matching: if current effect has a group_id,
            // search within the group by type+args instead of position
            if let Some(gid) = record.group_id {
                let group_start = *position;
                let group_end = session.effects[group_start..]
                    .iter()
                    .position(|e| e.group_id != Some(gid))
                    .map(|offset| group_start + offset)
                    .unwrap_or(session.effects.len());
                // Prefer exact branch match; fall back to type-only
                let current_bp: Option<String> = if scope.branch_stack.is_empty() {
                    None
                } else {
                    Some(scope.branch_stack.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("."))
                };
                let mut fallback_idx: Option<usize> = None;
                for idx in group_start..group_end {
                    let candidate = &session.effects[idx];
                    if candidate.effect_type != effect_type {
                        continue;
                    }
                    match (&current_bp, &candidate.branch_path) {
                        (Some(got), Some(rec)) if got == rec => {
                            let matched = candidate.clone();
                            if idx != *position {
                                session.effects.swap(*position, idx);
                            }
                            *position += 1;
                            return matched;
                        }
                        (Some(_), Some(_)) => continue,
                        _ => {
                            if fallback_idx.is_none() {
                                fallback_idx = Some(idx);
                            }
                        }
                    }
                }
                if let Some(idx) = fallback_idx {
                    let matched = session.effects[idx].clone();
                    if idx != *position {
                        session.effects.swap(*position, idx);
                    }
                    *position += 1;
                    return matched;
                }
                panic!(
                    "Replay group mismatch: no '{}' found in group {}",
                    effect_type, gid
                );
            }

            if record.effect_type != effect_type {
                panic!(
                    "Replay mismatch at #{}: expected '{}', got '{}'",
                    record.seq, record.effect_type, effect_type
                );
            }
            if *check_args && record.args != args {
                panic!("Replay args mismatch at #{} for '{}'", record.seq, effect_type);
            }
            *position += 1;
            record
        });

        match record.outcome {
            RecordedOutcome::Value { value } => T::from_replay_json(&value)
                .unwrap_or_else(|e| panic!("Replay decode failed for '{}': {}", effect_type, e)),
            RecordedOutcome::RuntimeError { message } => {
                panic!("Replayed runtime error for '{}': {}", effect_type, message)
            }
        }
    }

    fn write_recording(path: &PathBuf, session: &SessionRecording) {
        let parent = path.parent().map(PathBuf::from);
        if let Some(parent) = parent {
            std::fs::create_dir_all(&parent).unwrap_or_else(|e| {
                panic!("Cannot create replay dir '{}': {}", parent.display(), e)
            });
        }
        let json = serde_json::to_string_pretty(session)
            .expect("generated replay recording should serialize");
        std::fs::write(path, json)
            .unwrap_or_else(|e| panic!("Cannot write replay recording '{}': {}", path.display(), e));
    }

    fn env_var(name: &str) -> Option<String> {
        std::env::var(name)
            .ok()
            .map(|value| value.trim().to_string())
            .filter(|value| !value.is_empty())
    }

    fn env_flag(name: &str) -> bool {
        env_var(name)
            .map(|value| matches!(value.as_str(), "1" | "true" | "TRUE" | "yes" | "YES"))
            .unwrap_or(false)
    }

    fn default_request_id() -> String {
        format!("generated-{}", unix_millis())
    }

    fn default_timestamp() -> String {
        format!("unix-{}", unix_millis())
    }

    fn unix_millis() -> u128 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_millis()
    }

    fn panic_payload_to_string(payload: &Box<dyn std::any::Any + Send>) -> String {
        if let Some(msg) = payload.downcast_ref::<String>() {
            return msg.clone();
        }
        if let Some(msg) = payload.downcast_ref::<&str>() {
            return (*msg).to_string();
        }
        "panic".to_string()
    }
}

pub use aver_replay::*;"#;
