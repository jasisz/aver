//! Native capability-provider support emitted into generated Rust projects.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;

use crate::ast::{TypeDef, TypeVariant};
use crate::capability::CapabilityRegistry;
use crate::provider::required_capability_operations;

use super::syntax::aver_name_to_rust;
use super::types::type_annotation_to_rust_scoped;

/// One job kind of this program, resolved to what the generated crate needs:
/// the capability it answers, and the Rust call that runs one task.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct WorkKindEmit {
    pub capability: String,
    /// The generated wrapper's own name, e.g. `work_body_Validation`.
    pub body_fn: String,
    /// The call that runs one task, with `__task` already decoded.
    pub call: String,
}

/// The job engine limit this artifact is built with.
///
/// `[work] max-jobs` is part of the program, so a declared limit is a literal
/// in the generated bootstrap. A manifest that names none leaves the VM's own
/// default, which is the running host's parallelism — resolving that at
/// compile time would pin the build machine's core count into the binary.
fn work_limit_expression(declared: Option<usize>) -> String {
    match declared {
        Some(limit) => limit.to_string(),
        None => "aver_rt::work::JobEngine::default_limit()".to_string(),
    }
}

/// Resolve every job kind this program reaches into the Rust call that runs
/// one of its tasks.
///
/// The bound function is an ordinary function of the same program, so it is
/// called through the crate's own generated module path, and through the same
/// borrow mask every other call site of it uses — a collection task is passed
/// by reference exactly where the emitted signature borrows it.
pub(super) fn plan_work_kinds(
    ctx: &crate::codegen::CodegenContext,
    work_kinds: &[super::composition::ProviderCompositionWorkKind],
) -> Vec<WorkKindEmit> {
    work_kinds
        .iter()
        .map(|kind| {
            let path = generated_fn_path(&kind.function, ctx);
            let borrows = super::expr::callee_borrow_mask(&kind.function, 1, ctx)
                .first()
                .copied()
                .unwrap_or(false);
            let argument = if borrows { "&__task" } else { "__task" };
            WorkKindEmit {
                capability: kind.capability.clone(),
                body_fn: format!("work_body_{}", aver_name_to_rust(&kind.capability)),
                call: format!("{path}({argument})"),
            }
        })
        .collect()
}

/// The Rust path naming one `Module.function` of the program being generated.
fn generated_fn_path(dotted: &str, ctx: &crate::codegen::CodegenContext) -> String {
    if let Some((prefix, bare)) = crate::codegen::common::resolve_module_call(dotted, ctx) {
        return format!(
            "{}::{}",
            crate::codegen::common::module_prefix_to_rust_path(prefix),
            aver_name_to_rust(bare)
        );
    }
    // A dotted name that resolves to no module of the program does not reach
    // this far: `work-binding` refuses a binding naming the entry module, and
    // a binding naming nothing at all, before any target is asked to lower
    // it. The entry module's own path is still the honest answer here.
    let bare = dotted
        .rsplit_once('.')
        .map(|(_, bare)| bare)
        .unwrap_or(dotted);
    format!("crate::aver_generated::entry::{}", aver_name_to_rust(bare))
}

/// Emit the runtime registry shared by every custom-capability call in one
/// generated artifact. A host may install bindings once before calling an Aver
/// entry point; the ordinary binary installs compiler-shipped defaults only.
pub(super) fn generate_provider_runtime(
    contracts: &CapabilityRegistry,
    required: &BTreeSet<String>,
    embedded_tcp_settings: Option<crate::config::TcpEffectSettings>,
    runtime_policy_from_env: bool,
    work_kinds: &[WorkKindEmit],
    work_max_jobs: Option<usize>,
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

    if contracts.contract("Tcp").is_some() {
        let settings = if runtime_policy_from_env {
            "crate::aver_replay::tcp_provider_settings_from_env()?".to_string()
        } else if let Some(settings) = embedded_tcp_settings {
            format!(
                "aver_rt::tcp::TcpSettings::from_policy({}, {}, {})?",
                settings.connect_timeout_secs,
                settings.request_idle_timeout_secs,
                settings.max_connections
            )
        } else {
            "aver_rt::tcp::TcpSettings::default()".to_string()
        };
        writeln!(out, "    let standard_tcp_settings = {settings};").unwrap();
    }

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
        let provider = if standard == crate::provider::standard::StandardCapabilityBinding::Tcp {
            format!(
                "{}::new(standard_tcp_settings)",
                standard.generated_rust_provider_type()
            )
        } else {
            standard.generated_rust_provider_type().to_string()
        };
        writeln!(
            out,
            "    if include_defaults {{ registry.bind(ProviderBinding::new({:?}, {:?}, vec![{}], std::sync::Arc::new({})))?; }}",
            module, contract.contract_hash, operations, provider
        )
        .unwrap();
    }

    for kind in work_kinds {
        let Some(contract) = contracts.contract(&kind.capability) else {
            continue;
        };
        let operations = contracts
            .operations()
            .filter(|operation| operation.module == kind.capability)
            .map(|operation| format!("{:?}.to_string()", operation.canonical_name))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(
            out,
            "    if include_defaults {{ registry.bind(ProviderBinding::new({:?}, {:?}, vec![{}], std::sync::Arc::new(aver_rt::provider::WorkKindProvider::new({:?}, work_job_engine().clone(), {}))))?; }}",
            kind.capability, contract.contract_hash, operations, kind.capability, kind.body_fn
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

    if !work_kinds.is_empty() {
        writeln!(
            out,
            "/// The one job engine of this artifact. Every job kind shares it, so\n\
             /// `[work] max-jobs` bounds the program and not one capability of it.\n\
             fn work_job_engine() -> &'static std::sync::Arc<aver_rt::work::JobEngine> {{\n\
                 static ENGINE: OnceLock<std::sync::Arc<aver_rt::work::JobEngine>> = OnceLock::new();\n\
                 ENGINE.get_or_init(|| aver_rt::work::JobEngine::new({}))\n\
             }}\n",
            work_limit_expression(work_max_jobs)
        )
        .unwrap();
    }
    for kind in work_kinds {
        let capability = &kind.capability;
        writeln!(
            out,
            "/// One task of job kind '{capability}', off the turn: decode it, run the\n\
             /// function `aver.toml` bound, and hand the answer back as a value.\n\
             fn {}(__task: ProviderValue) -> Result<ProviderValue, String> {{\n\
                 let __registry = registry();\n\
                 let __task = ProviderCodec::from_provider_value(__task, __registry, {capability:?}, None)\n\
                     .map_err(|why| format!(\"work: job kind '{capability}' was handed an unusable task: {{}}\", why))?;\n\
                 let __produced = {};\n\
                 __produced.into_provider_value(__registry, {capability:?})\n\
                     .map_err(|why| format!(\"work: job kind '{capability}' produced an unusable result: {{}}\", why))\n\
             }}\n",
            kind.body_fn, kind.call
        )
        .unwrap();
    }

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

/// Canonical capability-owned resource types grouped by owning module.
pub(super) fn resource_types_by_module(
    contracts: &CapabilityRegistry,
) -> BTreeMap<String, Vec<String>> {
    let mut out = BTreeMap::<String, Vec<String>>::new();
    for canonical in contracts.resource_types() {
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
pub(super) fn emit_standard_bytes_codec(packed_u8: bool) -> String {
    if packed_u8 {
        return r#"impl aver_rt::provider::ProviderCodec for Bytes {
    fn into_provider_value(
        self,
        _registry: &aver_rt::provider::NativeProviderRegistry,
        _capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        Ok(aver_rt::provider::ProviderValue::Bytes(self.values.into_vec()))
    }

    fn from_provider_value(
        value: aver_rt::provider::ProviderValue,
        _registry: &aver_rt::provider::NativeProviderRegistry,
        _capability: &str,
        _minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        match value {
            aver_rt::provider::ProviderValue::Bytes(bytes) => Ok(Self {
                values: aver_rt::AverPackedU8::from_vec(bytes),
            }),
            other => Err(format!("expected Bytes, got {}", other.shape())),
        }
    }
}"#
        .to_string();
    }
    r#"impl aver_rt::provider::ProviderCodec for Bytes {
    fn into_provider_value(
        self,
        _registry: &aver_rt::provider::NativeProviderRegistry,
        _capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        let mut bytes = Vec::with_capacity(self.values.len());
        for (index, value) in self.values.iter_cloned().enumerate() {
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
                values: aver_rt::AverIntList::from_vec(
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

pub(super) fn emit_resource_type(module: &str, name: &str, with_replay: bool) -> String {
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
    // A resource is minted by whichever capability returns it, which need not
    // be the capability that declares the type: `Work.Job` is declared by
    // `Work`, minted by every job kind, and read by `Work` and `Wait`. Prefer
    // binding identity where it holds, and otherwise accept a handle whose
    // type matches and whose minting binding is still installed — the same
    // two-step the VM boundary in `src/provider/value.rs` uses.
    let resolve = format!(
        "registry.resolve_resource(capability, {canonical:?}, {{handle}}).or_else(|_| registry.resolve_foreign_resource({canonical:?}, {{handle}})).map(aver_rt::provider::ProviderValue::Resource)"
    );
    let live_handle = if with_replay {
        let resolved = resolve.replace("{handle}", "&handle");
        format!(
            "match self.0 {{\n            {name}State::Live(handle) => {resolved},\n            {name}State::Replay(_) => Err(\"replay-only capability resource '{canonical}' cannot enter a live provider call\".to_string()),\n        }}"
        )
    } else {
        resolve.replace("{handle}", "&self.0")
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
                 f.write_str(\"{canonical}(<resource>)\")\n\
             }}\n\
         }}\n\n\
         impl aver_rt::AverDisplay for {name} {{\n\
             fn aver_display(&self) -> String {{\n\
                 \"{canonical}(<resource>)\".to_string()\n\
             }}\n\
         }}\n\n\
         impl aver_rt::provider::ProviderCodec for {name} {{\n\
             fn into_provider_value(self, registry: &aver_rt::provider::NativeProviderRegistry, capability: &str) -> Result<aver_rt::provider::ProviderValue, String> {{\n\
                 {live_handle}\n\
             }}\n\n\
             fn from_provider_value(value: aver_rt::provider::ProviderValue, registry: &aver_rt::provider::NativeProviderRegistry, capability: &str, minted_resource: Option<&str>) -> Result<Self, String> {{\n\
                 if minted_resource != Some({canonical:?}) {{ return Err(\"resource '{canonical}' may only be returned by its minting operation\".to_string()); }}\n\
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
pub(super) fn emit_represented_type_codec(
    module: &str,
    type_def: &TypeDef,
    ctx: &crate::codegen::CodegenContext,
) -> String {
    let bare = crate::codegen::common::type_def_name(type_def);
    let canonical = format!("{module}.{bare}");
    // Name every field type exactly as the emitted declaration names it: the
    // codec sits in the capability's own module, so a payload owned by
    // another module (`Wait.Item.Socket(Tcp.Socket)`, `Wire.Heard.Data(Bytes)`)
    // must carry that module's path rather than a flat alias it cannot see.
    let field_type = |source: &str| type_annotation_to_rust_scoped(source, ctx, Some(module));
    let codec = match type_def {
        TypeDef::Product { name, fields, .. } => {
            emit_record_codec(name, fields, &canonical, &field_type)
        }
        TypeDef::Sum { name, variants, .. } => {
            emit_sum_codec(name, variants, &canonical, &field_type)
        }
    };
    format!(
        "{codec}\n\n#[allow(non_camel_case_types)]\npub type {} = {bare};",
        canonical.replace('.', "_")
    )
}

fn emit_record_codec(
    name: &str,
    fields: &[(String, String)],
    canonical: &str,
    field_type: &dyn Fn(&str) -> String,
) -> String {
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
            let rust_type = field_type(source_type);
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

fn sum_field_rust_type(
    owner: &str,
    source_type: &str,
    field_type: &dyn Fn(&str) -> String,
) -> String {
    let rust_type = field_type(source_type);
    if source_type == owner {
        format!("std::sync::Arc<{rust_type}>")
    } else {
        rust_type
    }
}

fn emit_sum_codec(
    name: &str,
    variants: &[TypeVariant],
    canonical: &str,
    field_type: &dyn Fn(&str) -> String,
) -> String {
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
                    let rust_type = sum_field_rust_type(name, source_type, field_type);
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
