//! Self-contained Wasmtime bundle manifest and executable entry point.
//!
//! The manifest carries deployment facts, not authority: the host recomputes
//! the wasm hash, import signatures, capability contract/model hashes, and
//! linked provider provenance before instantiation. Custom capability source is
//! retained only as ABI metadata so this reconstruction needs no project tree.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use aver::ast::Type;
use aver::capability::CapabilityRegistry;
use aver::codegen::wasm_gc::CapabilityWasmGcPlan;
use aver::provider::{ProviderBinding, ProviderRegistry};

use super::{CustomProviderConfig, EffectMode, run_wasm_gc_with_host};

pub const BUNDLE_MANIFEST_SCHEMA: u32 = 1;
const BUNDLE_TARGET: &str = "wasm-gc";
const BUNDLE_ENGINE: &str = "wasmtime-gc";
const BUNDLE_MANIFEST_FILE: &str = "manifest.json";

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BundleManifest {
    schema_version: u32,
    target: String,
    engine: String,
    aver_version: String,
    artifact: BundleArtifact,
    build: BundleBuild,
    entry: BundleEntry,
    imports: Vec<BundleImport>,
    capabilities: Vec<BundleCapability>,
    capability_sources: Vec<BundleCapabilitySource>,
    runtime_policy_toml: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BundleArtifact {
    file: String,
    sha256: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BundleBuild {
    optimization: String,
    certified: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BundleEntry {
    export: String,
    return_type: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BundleImport {
    module: String,
    name: String,
    params: Vec<String>,
    results: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BundleCapability {
    name: String,
    contract_hash: String,
    model_hash: String,
    required_operations: Vec<String>,
    provider_identity: String,
    provider_fingerprint: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BundleCapabilitySource {
    name: String,
    source: String,
}

/// Inputs already proved by the ordinary wasm-gc compile/provider pipeline.
/// This borrows those facts so manifest production cannot discover a subtly
/// different program graph.
pub struct BundleManifestInput<'a> {
    pub artifact_file: &'a str,
    pub wasm_bytes: &'a [u8],
    pub return_type: &'a Type,
    pub capabilities: &'a CapabilityRegistry,
    pub required_operations: &'a BTreeSet<String>,
    pub custom_plan: &'a CapabilityWasmGcPlan,
    pub capability_sources: &'a BTreeMap<String, String>,
    pub providers: &'a ProviderRegistry,
    pub runtime_policy_toml: &'a str,
    pub optimization: &'a str,
    pub certified: bool,
}

/// Render the canonical JSON manifest written beside a Wasmtime host pack.
pub fn manifest_json(input: BundleManifestInput<'_>) -> Result<String, String> {
    require_sibling_file(input.artifact_file)?;
    input
        .providers
        .preflight(input.required_operations.iter().map(String::as_str))?;

    let provenance = input
        .providers
        .provenance()
        .into_iter()
        .map(|entry| (entry.capability.clone(), entry))
        .collect::<BTreeMap<_, _>>();
    let mut required_by_capability = BTreeMap::<String, Vec<String>>::new();
    for operation_name in input.required_operations {
        let operation = input
            .capabilities
            .operation(operation_name)
            .ok_or_else(|| {
                format!("bundle received unknown required operation '{operation_name}'")
            })?;
        required_by_capability
            .entry(operation.module.clone())
            .or_default()
            .push(operation_name.clone());
    }
    let mut capabilities = Vec::with_capacity(required_by_capability.len());
    for (name, required_operations) in required_by_capability {
        let contract = input
            .capabilities
            .contract(&name)
            .ok_or_else(|| format!("bundle capability '{name}' has no contract"))?;
        let provider = provenance
            .get(&name)
            .ok_or_else(|| format!("bundle capability '{name}' has no provider"))?;
        capabilities.push(BundleCapability {
            name,
            contract_hash: contract.contract_hash.clone(),
            model_hash: contract.model_hash.clone(),
            required_operations,
            provider_identity: provider.provider.clone(),
            provider_fingerprint: provider.fingerprint.clone(),
        });
    }

    let mut capability_sources = Vec::with_capacity(input.custom_plan.interfaces().len());
    for interface in input.custom_plan.interfaces() {
        let source = input
            .capability_sources
            .get(&interface.capability)
            .ok_or_else(|| {
                format!(
                    "cannot pack custom capability '{}': pristine contract source is missing",
                    interface.capability
                )
            })?;
        capability_sources.push(BundleCapabilitySource {
            name: interface.capability.clone(),
            source: source.clone(),
        });
    }

    let manifest = BundleManifest {
        schema_version: BUNDLE_MANIFEST_SCHEMA,
        target: BUNDLE_TARGET.to_string(),
        engine: BUNDLE_ENGINE.to_string(),
        aver_version: env!("CARGO_PKG_VERSION").to_string(),
        artifact: BundleArtifact {
            file: input.artifact_file.to_string(),
            sha256: sha256(input.wasm_bytes),
        },
        build: BundleBuild {
            optimization: input.optimization.to_string(),
            certified: input.certified,
        },
        entry: BundleEntry {
            export: "main".to_string(),
            return_type: input.return_type.display(),
        },
        imports: inspect_imports(input.wasm_bytes)?,
        capabilities,
        capability_sources,
        runtime_policy_toml: input.runtime_policy_toml.to_string(),
    };
    serde_json::to_string_pretty(&manifest)
        .map(|json| format!("{json}\n"))
        .map_err(|error| format!("serialize Wasmtime bundle manifest: {error}"))
}

/// Run the bundle placed beside the current executable. Compilation and source
/// loading stay in `aver`; execution reuses the same Wasmtime path as
/// `aver run --wasm-gc`.
pub fn run_bundle_from_current_exe(bindings: Vec<ProviderBinding>) -> Result<(), String> {
    let executable =
        std::env::current_exe().map_err(|error| format!("locate aver-wasmtime-host: {error}"))?;
    let directory = executable
        .parent()
        .ok_or_else(|| "aver-wasmtime-host has no parent directory".to_string())?;
    let program_args = std::env::args_os()
        .skip(1)
        .map(|arg| {
            arg.into_string()
                .map_err(|_| "program arguments must be valid UTF-8".to_string())
        })
        .collect::<Result<Vec<_>, _>>()?;
    run_bundle(directory, program_args, bindings)
}

fn run_bundle(
    directory: &Path,
    program_args: Vec<String>,
    bindings: Vec<ProviderBinding>,
) -> Result<(), String> {
    let manifest_path = directory.join(BUNDLE_MANIFEST_FILE);
    let manifest_bytes = std::fs::read(&manifest_path)
        .map_err(|error| format!("read '{}': {error}", manifest_path.display()))?;
    let manifest: BundleManifest = serde_json::from_slice(&manifest_bytes)
        .map_err(|error| format!("parse '{}': {error}", manifest_path.display()))?;
    validate_manifest_header(&manifest)?;
    require_sibling_file(&manifest.artifact.file)?;

    let wasm_path = directory.join(&manifest.artifact.file);
    let wasm_bytes = std::fs::read(&wasm_path)
        .map_err(|error| format!("read '{}': {error}", wasm_path.display()))?;
    let actual_hash = sha256(&wasm_bytes);
    if actual_hash != manifest.artifact.sha256 {
        return Err(format!(
            "error[wasmtime-bundle-artifact-mismatch]: '{}' has sha256 {}, manifest requires {}",
            wasm_path.display(),
            actual_hash,
            manifest.artifact.sha256
        ));
    }
    let actual_imports = inspect_imports(&wasm_bytes)?;
    if actual_imports != manifest.imports {
        return Err(
            "error[wasmtime-bundle-abi-mismatch]: wasm import names or signatures differ from manifest"
                .to_string(),
        );
    }

    let mut registry = aver::stdlib::standard_capability_registry();
    let mut source_names = BTreeSet::new();
    for capability in &manifest.capability_sources {
        if !source_names.insert(capability.name.clone()) {
            return Err(format!(
                "error[wasmtime-bundle-manifest]: duplicate capability source '{}'",
                capability.name
            ));
        }
        let items = aver::source::parse_source(&capability.source).map_err(|error| {
            format!(
                "error[wasmtime-bundle-contract]: cannot parse capability '{}': {error}",
                capability.name
            )
        })?;
        let (part, errors) = CapabilityRegistry::from_module(&capability.name, &items);
        if !errors.is_empty() {
            return Err(format!(
                "error[wasmtime-bundle-contract]: capability '{}' is invalid: {}",
                capability.name,
                errors
                    .iter()
                    .map(|error| format!("line {}: {}", error.line, error.message))
                    .collect::<Vec<_>>()
                    .join("; ")
            ));
        }
        registry.merge(part);
    }

    let mut required = BTreeSet::new();
    let mut expected_capabilities = BTreeMap::new();
    for capability in &manifest.capabilities {
        if expected_capabilities
            .insert(capability.name.clone(), capability)
            .is_some()
        {
            return Err(format!(
                "error[wasmtime-bundle-manifest]: duplicate capability '{}'",
                capability.name
            ));
        }
        let contract = registry.contract(&capability.name).ok_or_else(|| {
            format!(
                "error[wasmtime-bundle-contract]: capability '{}' is absent from bundled contracts",
                capability.name
            )
        })?;
        if contract.contract_hash != capability.contract_hash
            || contract.model_hash != capability.model_hash
        {
            return Err(format!(
                "error[wasmtime-bundle-contract-mismatch]: capability '{}' recomputed contract/model hashes ({}, {}) but manifest requires ({}, {})",
                capability.name,
                contract.contract_hash,
                contract.model_hash,
                capability.contract_hash,
                capability.model_hash
            ));
        }
        for operation in &capability.required_operations {
            let declared = registry.operation(operation).ok_or_else(|| {
                format!(
                    "error[wasmtime-bundle-contract]: required operation '{operation}' is not declared"
                )
            })?;
            if declared.module != capability.name {
                return Err(format!(
                    "error[wasmtime-bundle-contract]: operation '{operation}' belongs to '{}', not '{}'",
                    declared.module, capability.name
                ));
            }
            required.insert(operation.clone());
        }
    }

    let standard = aver::stdlib::standard_capability_registry();
    for binding in &bindings {
        if standard.contract(binding.capability()).is_some() {
            return Err(format!(
                "error[wasmtime-bundle-provider]: linked binding for standard capability '{}' cannot replace its specialised wasm-gc host import",
                binding.capability()
            ));
        }
    }
    let custom_plan = CapabilityWasmGcPlan::build(&registry, &required)?;
    validate_import_owners(&actual_imports, &custom_plan)?;
    let providers = ProviderRegistry::for_program_with_bindings(registry, bindings)?;
    providers.preflight(required.iter().map(String::as_str))?;
    let actual_provenance = providers
        .provenance()
        .into_iter()
        .map(|entry| (entry.capability.clone(), entry))
        .collect::<BTreeMap<_, _>>();
    for (name, expected) in expected_capabilities {
        let actual = actual_provenance.get(&name).ok_or_else(|| {
            format!("error[wasmtime-bundle-provider]: capability '{name}' has no linked provider")
        })?;
        if actual.contract_hash != expected.contract_hash
            || actual.model_hash != expected.model_hash
            || actual.provider != expected.provider_identity
            || actual.fingerprint != expected.provider_fingerprint
        {
            return Err(format!(
                "error[wasmtime-bundle-provider-mismatch]: capability '{name}' is linked to {}@{} with contract/model ({}, {}), manifest requires {}@{} with ({}, {})",
                actual.provider,
                actual.fingerprint,
                actual.contract_hash,
                actual.model_hash,
                expected.provider_identity,
                expected.provider_fingerprint,
                expected.contract_hash,
                expected.model_hash
            ));
        }
    }

    if manifest.entry.export != "main" {
        return Err(format!(
            "error[wasmtime-bundle-entry]: unsupported entry export '{}'; schema 1 requires 'main'",
            manifest.entry.export
        ));
    }
    let return_type =
        aver::types::parse_type_str_strict(&manifest.entry.return_type).map_err(|_| {
            format!(
                "error[wasmtime-bundle-entry]: invalid return type '{}'",
                manifest.entry.return_type
            )
        })?;
    let project_config = aver::config::ProjectConfig::parse(&manifest.runtime_policy_toml)?;
    let tcp_settings = project_config.tcp_settings.native();
    run_wasm_gc_with_host(
        &wasm_bytes,
        &program_args,
        &EffectMode::Normal,
        tcp_settings,
        Some(project_config),
        None,
        &return_type,
        Some(&CustomProviderConfig {
            plan: custom_plan,
            providers,
        }),
    )
    .map(|_| ())
    .map_err(|error| format!("WASM execution error: {error}"))
}

fn validate_manifest_header(manifest: &BundleManifest) -> Result<(), String> {
    if manifest.schema_version != BUNDLE_MANIFEST_SCHEMA {
        return Err(format!(
            "error[wasmtime-bundle-schema]: manifest schema {} is unsupported; host requires {}",
            manifest.schema_version, BUNDLE_MANIFEST_SCHEMA
        ));
    }
    if manifest.target != BUNDLE_TARGET || manifest.engine != BUNDLE_ENGINE {
        return Err(format!(
            "error[wasmtime-bundle-target]: manifest declares target '{}' on '{}'; host requires '{}' on '{}'",
            manifest.target, manifest.engine, BUNDLE_TARGET, BUNDLE_ENGINE
        ));
    }
    if manifest.aver_version != env!("CARGO_PKG_VERSION") {
        return Err(format!(
            "error[wasmtime-bundle-version]: manifest was emitted by Aver {}, host is Aver {}",
            manifest.aver_version,
            env!("CARGO_PKG_VERSION")
        ));
    }
    if !matches!(
        manifest.build.optimization.as_str(),
        "none" | "speed" | "size"
    ) {
        return Err(format!(
            "error[wasmtime-bundle-manifest]: unknown optimization mode '{}'",
            manifest.build.optimization
        ));
    }
    Ok(())
}

fn validate_import_owners(
    imports: &[BundleImport],
    custom_plan: &CapabilityWasmGcPlan,
) -> Result<(), String> {
    let custom = custom_plan
        .interfaces()
        .iter()
        .flat_map(|interface| {
            let module = format!("aver:user/{}", interface.interface_name);
            interface
                .operations
                .iter()
                .map(move |operation| (module.clone(), operation.import_name.clone()))
        })
        .collect::<BTreeSet<_>>();
    for import in imports {
        if import.module == "aver" {
            continue;
        }
        if custom.contains(&(import.module.clone(), import.name.clone())) {
            continue;
        }
        return Err(format!(
            "error[wasmtime-bundle-abi]: host does not provide import '{}.{}'",
            import.module, import.name
        ));
    }
    Ok(())
}

fn inspect_imports(bytes: &[u8]) -> Result<Vec<BundleImport>, String> {
    use wasmtime::ExternType;

    let engine = wasmtime::Engine::new(&crate::runtime::wasmtime_gc_engine_config())
        .map_err(|error| format!("inspect bundle engine: {error:#}"))?;
    let module = wasmtime::Module::new(&engine, bytes)
        .map_err(|error| format!("inspect bundle module: {error:#}"))?;
    module
        .imports()
        .map(|import| {
            let ExternType::Func(function) = import.ty() else {
                return Err(format!(
                    "error[wasmtime-bundle-abi]: non-function import '{}.{}' is unsupported",
                    import.module(),
                    import.name()
                ));
            };
            Ok(BundleImport {
                module: import.module().to_string(),
                name: import.name().to_string(),
                params: function.params().map(|ty| ty.to_string()).collect(),
                results: function.results().map(|ty| ty.to_string()).collect(),
            })
        })
        .collect()
}

fn require_sibling_file(file: &str) -> Result<(), String> {
    let mut components = Path::new(file).components();
    if matches!(components.next(), Some(Component::Normal(_))) && components.next().is_none() {
        return Ok(());
    }
    Err(format!(
        "error[wasmtime-bundle-manifest]: artifact file '{file}' must be a sibling file name"
    ))
}

fn sha256(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_manifest(wasm_bytes: &[u8]) -> BundleManifest {
        BundleManifest {
            schema_version: BUNDLE_MANIFEST_SCHEMA,
            target: BUNDLE_TARGET.to_string(),
            engine: BUNDLE_ENGINE.to_string(),
            aver_version: env!("CARGO_PKG_VERSION").to_string(),
            artifact: BundleArtifact {
                file: "main.wasm".to_string(),
                sha256: sha256(wasm_bytes),
            },
            build: BundleBuild {
                optimization: "none".to_string(),
                certified: false,
            },
            entry: BundleEntry {
                export: "main".to_string(),
                return_type: "Unit".to_string(),
            },
            imports: inspect_imports(wasm_bytes).expect("inspect test wasm"),
            capabilities: Vec::new(),
            capability_sources: Vec::new(),
            runtime_policy_toml: String::new(),
        }
    }

    fn write_test_bundle(directory: &Path, manifest: &BundleManifest, wasm_bytes: &[u8]) {
        std::fs::write(directory.join("main.wasm"), wasm_bytes).expect("write test wasm");
        std::fs::write(
            directory.join(BUNDLE_MANIFEST_FILE),
            serde_json::to_vec_pretty(manifest).expect("serialize test manifest"),
        )
        .expect("write test manifest");
    }

    #[test]
    fn artifact_name_must_stay_inside_bundle_directory() {
        assert!(require_sibling_file("main.wasm").is_ok());
        assert!(require_sibling_file("../main.wasm").is_err());
        assert!(require_sibling_file("nested/main.wasm").is_err());
        assert!(require_sibling_file("/main.wasm").is_err());
    }

    #[test]
    fn artifact_hash_mismatch_stops_before_wasm_compilation() {
        let wasm = wat::parse_str("(module (func (export \"main\")))").expect("test wasm");
        let mut manifest = test_manifest(&wasm);
        manifest.artifact.sha256 = "sha256:tampered".to_string();
        let directory = tempfile::tempdir().expect("bundle directory");
        write_test_bundle(directory.path(), &manifest, &wasm);

        let error = run_bundle(directory.path(), Vec::new(), Vec::new()).unwrap_err();
        assert!(
            error.contains("wasmtime-bundle-artifact-mismatch"),
            "{error}"
        );
    }

    #[test]
    fn import_manifest_mismatch_stops_before_instantiation() {
        let wasm = wat::parse_str("(module (func (export \"main\")))").expect("test wasm");
        let mut manifest = test_manifest(&wasm);
        manifest.imports.push(BundleImport {
            module: "aver".to_string(),
            name: "invented".to_string(),
            params: Vec::new(),
            results: Vec::new(),
        });
        let directory = tempfile::tempdir().expect("bundle directory");
        write_test_bundle(directory.path(), &manifest, &wasm);

        let error = run_bundle(directory.path(), Vec::new(), Vec::new()).unwrap_err();
        assert!(error.contains("wasmtime-bundle-abi-mismatch"), "{error}");
    }
}
