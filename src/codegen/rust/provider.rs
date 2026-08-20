//! Native capability-provider support emitted into generated Rust projects.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;

use crate::ast::{TypeDef, TypeVariant};
use crate::capability::CapabilityRegistry;
use crate::provider::required_capability_operations;

use super::syntax::aver_name_to_rust;
use super::types::type_annotation_to_rust;

/// Emit the runtime registry shared by every custom-capability call in one
/// generated artifact. A host may install bindings once before calling an Aver
/// entry point; the ordinary binary installs compiler-shipped defaults only.
pub(super) fn generate_provider_runtime(
    contracts: &CapabilityRegistry,
    required: &BTreeSet<String>,
) -> String {
    let mut out = String::new();
    out.push_str(
        "use std::sync::OnceLock;\n\
         use aver_rt::provider::{NativeProviderRegistry, ProviderBinding, ProviderCodec, ProviderContractSpec, ProviderValue};\n\n\
         static PROVIDERS: OnceLock<NativeProviderRegistry> = OnceLock::new();\n\n",
    );

    out.push_str("fn build_registry(bindings: Vec<ProviderBinding>, include_defaults: bool) -> Result<NativeProviderRegistry, String> {\n");
    out.push_str("    let mut registry = NativeProviderRegistry::new(vec![\n");
    for contract in contracts.contracts() {
        let operations = contracts
            .operations()
            .filter(|operation| operation.module == contract.module)
            .map(|operation| format!("{:?}.to_string()", operation.canonical_name))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(
            out,
            "        ProviderContractSpec::new({:?}, {:?}, {:?}, vec![{}]),",
            contract.module, contract.contract_hash, contract.model_hash, operations
        )
        .unwrap();
    }
    out.push_str("    ])?;\n");

    for standard in crate::provider::standard::StandardCapabilityBinding::ALL {
        let module = standard.module();
        let Some(contract) = contracts.contract(module) else {
            continue;
        };
        let operations = contracts
            .operations()
            .filter(|operation| operation.module == module)
            .map(|operation| format!("{:?}.to_string()", operation.canonical_name))
            .collect::<Vec<_>>()
            .join(", ");
        let provider_type = standard.generated_rust_provider_type();
        writeln!(
            out,
            "    if include_defaults {{ registry.bind(ProviderBinding::new({:?}, {:?}, vec![{}], std::sync::Arc::new({})))?; }}",
            module, contract.contract_hash, operations, provider_type
        )
        .unwrap();
    }

    out.push_str(
        "    let mut supplied = std::collections::BTreeSet::new();\n\
         for binding in bindings {\n\
             if !supplied.insert(binding.capability().to_string()) {\n\
                 return Err(format!(\"error[capability-provider-duplicate]: capability '{}' has more than one host-supplied provider binding\", binding.capability()));\n\
             }\n\
             if registry.binding(binding.capability()).is_some() {\n\
                 registry.replace_binding(binding)?;\n\
             } else {\n\
                 registry.bind(binding)?;\n\
             }\n\
         }\n\
         Ok(registry)\n\
         }\n\n",
    );

    out.push_str(
        "/// Install the native providers for this generated artifact. The set is\n\
         /// immutable after the first Aver entry starts, so parallel branches cannot\n\
         /// observe different provider instances.\n\
         pub fn install_provider_bindings(bindings: Vec<ProviderBinding>) -> Result<(), String> {\n\
             let registry = build_registry(bindings, true)?;\n\
             PROVIDERS.set(registry).map_err(|_| \"error[capability-provider-already-installed]: provider bindings were already installed for this artifact\".to_string())\n\
         }\n\n\
         /// Install exactly the supplied bindings, without compiler-shipped defaults.\n\
         /// This is useful for fully explicit hosts and fault-injection tests.\n\
         pub fn install_provider_bindings_exact(bindings: Vec<ProviderBinding>) -> Result<(), String> {\n\
             let registry = build_registry(bindings, false)?;\n\
             PROVIDERS.set(registry).map_err(|_| \"error[capability-provider-already-installed]: provider bindings were already installed for this artifact\".to_string())\n\
         }\n\n\
         pub fn ensure_default_provider_bindings() {\n\
             PROVIDERS.get_or_init(|| build_registry(Vec::new(), true).expect(\"compiler-shipped provider bindings must match embedded contracts\"));\n\
         }\n\n\
         pub fn registry() -> &'static NativeProviderRegistry {\n\
             ensure_default_provider_bindings();\n\
             PROVIDERS.get().expect(\"provider registry initialized\")\n\
         }\n\n",
    );

    let required = required
        .iter()
        .map(|operation| format!("{:?}", operation))
        .collect::<Vec<_>>()
        .join(", ");
    writeln!(
        out,
        "pub fn preflight_required_providers() -> Result<(), String> {{ registry().preflight([{}]) }}\n",
        required
    )
    .unwrap();

    out.push_str(
        "pub fn encode<T: ProviderCodec>(value: T, capability: &str) -> ProviderValue {\n\
             value.into_provider_value(registry(), capability).unwrap_or_else(|message| {\n\
                 panic!(\"error[capability-provider-invalid-argument]: {}\", message)\n\
             })\n\
         }\n\n\
         pub fn invoke<T: ProviderCodec>(\n\
             capability: &str,\n\
             operation: &str,\n\
             args: Vec<ProviderValue>,\n\
             minted_resource: Option<&str>,\n\
             expected: &str,\n\
         ) -> T {\n\
             let registry = registry();\n\
             let value = registry.invoke(operation, &args).unwrap_or_else(|message| panic!(\"{}\", message));\n\
             let received = value.shape();\n\
             T::from_provider_value(value, registry, capability, minted_resource).unwrap_or_else(|message| {\n\
                 let provider = registry.provider_identity_for(capability).unwrap_or(\"<missing>\");\n\
                 panic!(\"error[capability-provider-invalid-return]: provider '{}' returned an invalid value for '{}': expected {}, received {}; {}\", provider, operation, expected, received, message)\n\
             })\n\
         }\n",
    );
    out
}

pub(super) fn required_operations(ctx: &crate::codegen::CodegenContext) -> BTreeSet<String> {
    required_capability_operations(&ctx.items, &ctx.modules, &ctx.capabilities)
}

/// Canonical capability-owned opaque types grouped by owning module.
pub(super) fn opaque_types_by_module(
    contracts: &CapabilityRegistry,
) -> BTreeMap<String, Vec<String>> {
    let mut out = BTreeMap::<String, Vec<String>>::new();
    for canonical in contracts.opaque_types() {
        let Some((module, name)) = canonical.rsplit_once('.') else {
            continue;
        };
        out.entry(module.to_string())
            .or_default()
            .push(name.to_string());
    }
    for names in out.values_mut() {
        names.sort();
    }
    out
}

/// `Bytes` is nominal source data but canonical octets at a provider boundary.
/// Emit this inside the generated `Bytes` module so it can project the opaque
/// record without exposing that representation to providers.
pub(super) fn emit_standard_bytes_codec() -> String {
    r#"impl aver_rt::provider::ProviderCodec for Bytes {
    fn into_provider_value(
        self,
        _registry: &aver_rt::provider::NativeProviderRegistry,
        _capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        let mut bytes = Vec::with_capacity(self.values.len());
        for (index, value) in self.values.iter().enumerate() {
            let Some(value) = value.to_i64() else {
                return Err(format!("Bytes value at index {} is outside the host integer range", index));
            };
            let byte = u8::try_from(value)
                .map_err(|_| format!("byte {} at index {} is outside 0..=255", value, index))?;
            bytes.push(byte);
        }
        Ok(aver_rt::provider::ProviderValue::Bytes(bytes))
    }

    fn from_provider_value(
        value: aver_rt::provider::ProviderValue,
        _registry: &aver_rt::provider::NativeProviderRegistry,
        _capability: &str,
        _minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        match value {
            aver_rt::provider::ProviderValue::Bytes(bytes) => Ok(Self {
                values: aver_rt::AverList::from_vec(
                    bytes
                        .into_iter()
                        .map(|byte| aver_rt::AverInt::from(i64::from(byte)))
                        .collect(),
                ),
            }),
            other => Err(format!("expected Bytes, got {}", other.shape())),
        }
    }
}"#
        .to_string()
}

pub(super) fn emit_opaque_type(module: &str, name: &str, with_replay: bool) -> String {
    let canonical = format!("{module}.{name}");
    let flat = canonical.replace('.', "_");
    let state = if with_replay {
        format!(
            "#[derive(Clone, PartialEq, Eq, Hash)]\nenum {name}State {{\n    Live(aver_rt::provider::ProviderResourceHandle),\n    Replay(u64),\n}}\n\n#[derive(Clone, PartialEq, Eq, Hash)]\npub struct {name}({name}State);"
        )
    } else {
        format!(
            "#[derive(Clone, PartialEq, Eq, Hash)]\npub struct {name}(aver_rt::provider::ProviderResourceHandle);"
        )
    };
    let live_handle = if with_replay {
        format!(
            "match self.0 {{\n            {name}State::Live(handle) => registry.resolve_resource(capability, {canonical:?}, &handle).map(aver_rt::provider::ProviderValue::Resource),\n            {name}State::Replay(_) => Err(\"replay-only capability resource '{canonical}' cannot enter a live provider call\".to_string()),\n        }}"
        )
    } else {
        format!(
            "registry.resolve_resource(capability, {canonical:?}, &self.0).map(aver_rt::provider::ProviderValue::Resource)"
        )
    };
    let stored = if with_replay {
        format!(
            "registry.store_resource(capability, {canonical:?}, resource).map(|handle| Self({name}State::Live(handle)))"
        )
    } else {
        format!("registry.store_resource(capability, {canonical:?}, resource).map(Self)")
    };
    let replay_impl = if with_replay {
        format!(
            r#"

impl crate::aver_replay::ReplayValue for {name} {{
    fn to_replay_json(&self) -> serde_json::Value {{
        match &self.0 {{
            {name}State::Live(handle) => crate::aver_replay::encode_live_capability_resource({canonical:?}, handle),
            {name}State::Replay(trace) => crate::aver_replay::encode_replay_capability_resource({canonical:?}, *trace),
        }}
    }}

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {{
        crate::aver_replay::decode_capability_resource(value, {canonical:?})
            .map(|trace| Self({name}State::Replay(trace)))
    }}
}}"#
        )
    } else {
        String::new()
    };
    format!(
        "{state}\n\n\
         impl std::fmt::Debug for {name} {{\n\
             fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{\n\
                 f.write_str(\"{canonical}(<opaque>)\")\n\
             }}\n\
         }}\n\n\
         impl aver_rt::AverDisplay for {name} {{\n\
             fn aver_display(&self) -> String {{\n\
                 \"{canonical}(<opaque>)\".to_string()\n\
             }}\n\
         }}\n\n\
         impl aver_rt::provider::ProviderCodec for {name} {{\n\
             fn into_provider_value(self, registry: &aver_rt::provider::NativeProviderRegistry, capability: &str) -> Result<aver_rt::provider::ProviderValue, String> {{\n\
                 if capability != {module:?} {{ return Err(format!(\"resource '{canonical}' belongs to capability '{module}', not '{{}}'\", capability)); }}\n\
                 {live_handle}\n\
             }}\n\n\
             fn from_provider_value(value: aver_rt::provider::ProviderValue, registry: &aver_rt::provider::NativeProviderRegistry, capability: &str, minted_resource: Option<&str>) -> Result<Self, String> {{\n\
                 if capability != {module:?} || minted_resource != Some({canonical:?}) {{ return Err(\"resource '{canonical}' may only be returned by its minting operation\".to_string()); }}\n\
                 match value {{\n\
                     aver_rt::provider::ProviderValue::Resource(resource) => {stored},\n\
                     other => Err(format!(\"expected capability resource {canonical}, got {{}}\", other.shape())),\n\
                 }}\n\
             }}\n\
         }}{replay_impl}\n\n\
         pub type {flat} = {name};"
    )
}

/// Emit the native boundary codec for one capability-owned represented type.
/// Canonical names stay in ProviderValue while generated Rust keeps its local
/// bare struct/enum name inside the owning module.
pub(super) fn emit_represented_type_codec(module: &str, type_def: &TypeDef) -> String {
    let bare = crate::codegen::common::type_def_name(type_def);
    let canonical = format!("{module}.{bare}");
    let codec = match type_def {
        TypeDef::Product { name, fields, .. } => emit_record_codec(name, fields, &canonical),
        TypeDef::Sum { name, variants, .. } => emit_sum_codec(name, variants, &canonical),
    };
    format!(
        "{codec}\n\n#[allow(non_camel_case_types)]\npub type {} = {bare};",
        canonical.replace('.', "_")
    )
}

fn emit_record_codec(name: &str, fields: &[(String, String)], canonical: &str) -> String {
    let encoded = fields
        .iter()
        .map(|(field, _)| {
            let rust_field = aver_name_to_rust(field);
            format!(
                "            ({field:?}.to_string(), self.{rust_field}.into_provider_value(registry, capability)?),"
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let decoded = fields
        .iter()
        .map(|(field, source_type)| {
            let rust_field = aver_name_to_rust(field);
            let rust_type = type_annotation_to_rust(source_type);
            format!(
                "            {rust_field}: <{rust_type} as aver_rt::provider::ProviderCodec>::from_provider_value(by_name.remove({field:?}).ok_or_else(|| \"record '{canonical}' is missing field '{field}'\".to_string())?, registry, capability, minted_resource)?,"
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        r#"impl aver_rt::provider::ProviderCodec for {name} {{
    fn into_provider_value(self, registry: &aver_rt::provider::NativeProviderRegistry, capability: &str) -> Result<aver_rt::provider::ProviderValue, String> {{
        use aver_rt::provider::ProviderCodec as _;
        Ok(aver_rt::provider::ProviderValue::Record {{
            type_name: {canonical:?}.to_string(),
            fields: vec![
{encoded}
            ],
        }})
    }}

    fn from_provider_value(value: aver_rt::provider::ProviderValue, registry: &aver_rt::provider::NativeProviderRegistry, capability: &str, minted_resource: Option<&str>) -> Result<Self, String> {{
        let aver_rt::provider::ProviderValue::Record {{ type_name, fields }} = value else {{
            return Err(format!("expected represented boundary type '{canonical}', got {{}}", value.shape()));
        }};
        if type_name != {canonical:?} {{
            return Err(format!("expected represented boundary type '{canonical}', got record {{}}", type_name));
        }}
        let mut by_name = std::collections::BTreeMap::new();
        for (field, value) in fields {{
            if by_name.insert(field.clone(), value).is_some() {{
                return Err(format!("record '{canonical}' contains duplicate field '{{}}'", field));
            }}
        }}
        let decoded = Self {{
{decoded}
        }};
        if !by_name.is_empty() {{
            return Err(format!("record '{canonical}' has unknown fields: {{:?}}", by_name.keys().collect::<Vec<_>>()));
        }}
        Ok(decoded)
    }}
}}"#
    )
}

fn sum_field_rust_type(owner: &str, source_type: &str) -> String {
    let rust_type = type_annotation_to_rust(source_type);
    if source_type == owner {
        format!("std::sync::Arc<{rust_type}>")
    } else {
        rust_type
    }
}

fn emit_sum_codec(name: &str, variants: &[TypeVariant], canonical: &str) -> String {
    let encode_arms = variants
        .iter()
        .map(|variant| {
            let bindings = (0..variant.fields.len())
                .map(|index| format!("field{index}"))
                .collect::<Vec<_>>();
            let pattern = if bindings.is_empty() {
                format!("Self::{}", variant.name)
            } else {
                format!("Self::{}({})", variant.name, bindings.join(", "))
            };
            let encoded = bindings
                .iter()
                .map(|field| format!("{field}.into_provider_value(registry, capability)?"))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "            {pattern} => aver_rt::provider::ProviderValue::Variant {{ type_name: {canonical:?}.to_string(), variant: {:?}.to_string(), fields: vec![{encoded}] }},",
                variant.name
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let decode_arms = variants
        .iter()
        .map(|variant| {
            let decoded = variant
                .fields
                .iter()
                .enumerate()
                .map(|(index, source_type)| {
                    let rust_type = sum_field_rust_type(name, source_type);
                    format!(
                        "<{rust_type} as aver_rt::provider::ProviderCodec>::from_provider_value(fields.next().expect(\"validated variant arity\"), registry, capability, minted_resource)?"
                    )
                    .replace("expect(\"validated variant arity\")", &format!("expect(\"validated variant field {index}\")"))
                })
                .collect::<Vec<_>>();
            let constructor = if decoded.is_empty() {
                format!("Self::{}", variant.name)
            } else {
                format!("Self::{}({})", variant.name, decoded.join(", "))
            };
            format!(
                "            {:?} if field_count == {} => Ok({constructor}),\n            {:?} => Err(format!(\"variant '{canonical}.{}' expected {} field(s), got {{}}\", field_count)),",
                variant.name,
                variant.fields.len(),
                variant.name,
                variant.name,
                variant.fields.len()
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        r#"impl aver_rt::provider::ProviderCodec for {name} {{
    fn into_provider_value(self, registry: &aver_rt::provider::NativeProviderRegistry, capability: &str) -> Result<aver_rt::provider::ProviderValue, String> {{
        use aver_rt::provider::ProviderCodec as _;
        Ok(match self {{
{encode_arms}
        }})
    }}

    fn from_provider_value(value: aver_rt::provider::ProviderValue, registry: &aver_rt::provider::NativeProviderRegistry, capability: &str, minted_resource: Option<&str>) -> Result<Self, String> {{
        let aver_rt::provider::ProviderValue::Variant {{ type_name, variant, fields }} = value else {{
            return Err(format!("expected represented boundary type '{canonical}', got {{}}", value.shape()));
        }};
        if type_name != {canonical:?} {{
            return Err(format!("expected represented boundary type '{canonical}', got variant {{}}.{{}}", type_name, variant));
        }}
        let field_count = fields.len();
        let mut fields = fields.into_iter();
        match variant.as_str() {{
{decode_arms}
            other => Err(format!("unknown variant '{canonical}.{{}}'", other)),
        }}
    }}
}}"#
    )
}
