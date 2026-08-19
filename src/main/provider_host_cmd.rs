//! CLI-side planning and repair text for the cached provider VM host.

use std::collections::BTreeSet;
use std::ffi::OsString;
use std::path::Path;
use std::process::ExitStatus;

use aver::codegen::rust as rust_codegen;

use super::cli::Commands;
use super::commands::resolve_av_inputs;
use super::shared::{format_type_errors, parse_file, read_file};

pub(super) fn run_if_requested(
    command: &Commands,
    raw_args: &[OsString],
) -> Option<Result<ExitStatus, String>> {
    let (composition, backend) = match command {
        Commands::Run {
            file,
            module_root,
            providers: true,
            wasip2,
            ..
        } => {
            let module_root = super::shared::resolve_module_root(module_root.as_deref());
            let backend = if *wasip2 {
                crate::provider_vm_host::ProviderHostBackend::Wasip2
            } else {
                crate::provider_vm_host::ProviderHostBackend::Vm
            };
            (plan_for_run(file, &module_root), backend)
        }
        Commands::Verify {
            file,
            module_root,
            providers: true,
            ..
        } => {
            let module_root = super::shared::resolve_module_root(module_root.as_deref());
            (
                plan_for_verify(file, &module_root),
                crate::provider_vm_host::ProviderHostBackend::Vm,
            )
        }
        Commands::Audit {
            path,
            module_root,
            providers: true,
            ..
        } => {
            let module_root = super::shared::resolve_module_root(module_root.as_deref());
            (
                plan_for_verify(path, &module_root),
                crate::provider_vm_host::ProviderHostBackend::Vm,
            )
        }
        _ => return None,
    };
    Some(composition.and_then(|composition| {
        crate::provider_vm_host::run_cached_host(raw_args, &composition, backend)
    }))
}

/// Validate the complete schema-1 composition before provider code reaches
/// Cargo. Generated Rust and the cached VM host intentionally share this plan.
pub(super) fn plan_for_run(
    file: &str,
    module_root: &str,
) -> Result<rust_codegen::composition::ProviderComposition, String> {
    plan_for_files(&[file.to_string()], module_root)
}

pub(super) fn plan_for_verify(
    path: &str,
    module_root: &str,
) -> Result<rust_codegen::composition::ProviderComposition, String> {
    let files = resolve_av_inputs(path)?;
    plan_for_files(&files, module_root)
}

fn plan_for_files(
    files: &[String],
    module_root: &str,
) -> Result<rust_codegen::composition::ProviderComposition, String> {
    let config = aver::config::ProjectConfig::load_from_dir(Path::new(module_root))?
        .ok_or_else(|| missing_manifest_error(module_root))?;
    let manifest = config
        .provider_manifest
        .as_ref()
        .ok_or_else(|| missing_manifest_error(module_root))?;

    let mut capabilities = aver::capability::CapabilityRegistry::default();
    let mut required = BTreeSet::new();
    for file in files {
        let source = read_file(file)?;
        let mut items = parse_file(&source)?;
        aver::ir::pipeline::tco(&mut items);
        let modules = aver::source::load_compile_deps(&items, module_root)?;
        let tc = aver::ir::pipeline::typecheck_gate(
            &items,
            &aver::ir::TypecheckMode::Full {
                base_dir: Some(module_root),
            },
            &items,
        );
        if !tc.errors.is_empty() {
            return Err(format_type_errors(&tc.errors));
        }
        required.extend(aver::provider::required_capability_operations(
            &items,
            &modules,
            &tc.capabilities,
        ));
        capabilities.merge(tc.capabilities);
    }

    let known_capabilities = known_project_capabilities(module_root, &capabilities, Some(manifest));
    rust_codegen::composition::plan_for_project(
        &capabilities,
        &required,
        Some(manifest),
        &known_capabilities,
    )
}

/// Resolve only the manifest capability names that are outside the current
/// entry program. This follows normal module-root lookup instead of scanning
/// and typechecking unrelated application files: a project binding is known
/// when its canonical module resolves to a valid capability contract.
pub(super) fn known_project_capabilities(
    module_root: &str,
    program_registry: &aver::capability::CapabilityRegistry,
    manifest: Option<&aver::config::ProviderPackageManifest>,
) -> BTreeSet<String> {
    let mut known = aver::stdlib::standard_capability_registry()
        .contracts()
        .map(|contract| contract.module.clone())
        .collect::<BTreeSet<_>>();
    known.extend(
        program_registry
            .contracts()
            .map(|contract| contract.module.clone()),
    );
    let Some(manifest) = manifest else {
        return known;
    };

    for binding in &manifest.bindings {
        if known.contains(&binding.capability) {
            continue;
        }
        let Some(path) = aver::source::find_module_file(&binding.capability, module_root) else {
            continue;
        };
        let Ok(source) = std::fs::read_to_string(path) else {
            continue;
        };
        let Ok(items) = aver::source::parse_source(&source) else {
            continue;
        };
        let declares_expected_module = items.iter().any(|item| {
            matches!(
                item,
                aver::ast::TopLevel::Module(module) if module.name == binding.capability
            )
        });
        if !declares_expected_module {
            continue;
        }
        let (registry, errors) =
            aver::capability::CapabilityRegistry::from_module(&binding.capability, &items);
        if errors.is_empty() && registry.contract(&binding.capability).is_some() {
            known.insert(binding.capability.clone());
        }
    }

    known
}

fn missing_manifest_error(module_root: &str) -> String {
    format!(
        "--providers requires [providers] schema = 1 in {}/aver.toml",
        Path::new(module_root).display()
    )
}

pub(super) fn missing_provider_repair(
    error: &str,
    module_root: &str,
    command: &str,
    file: &str,
) -> Option<String> {
    let marker = "capability provider missing for '";
    let operation = error.split_once(marker)?.1.split_once('\'')?.0;
    let capability = operation.rsplit_once('.')?.0;
    let configured = aver::config::ProjectConfig::load_from_dir(Path::new(module_root))
        .ok()
        .flatten()
        .and_then(|config| config.provider_manifest)
        .is_some_and(|manifest| {
            manifest
                .bindings
                .iter()
                .any(|binding| binding.capability == capability)
        });

    if configured {
        let mut repair = format!(
            "hint: capability '{capability}' has a Rust provider configured in aver.toml.\n\
             Run the real configured provider with:\n  aver {command} {file} --module-root {module_root} --providers"
        );
        if command == "verify" {
            let stub_name = operation
                .rsplit_once('.')
                .map(|(_, name)| name)
                .unwrap_or("call");
            repair.push_str(&format!(
                "\n\nor bind a verify-local stand-in:\n  given {stub_name}: {operation} = [stub]"
            ));
        }
        Some(repair)
    } else {
        Some(format!(
            "hint: capability '{capability}' has no Rust provider configured in aver.toml.\n\
             Add a matching [[providers.bindings]] entry before using --providers."
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::missing_provider_repair;

    #[test]
    fn unrelated_text_has_no_provider_repair() {
        assert!(missing_provider_repair("ordinary error", ".", "run", "app.av").is_none());
    }
}
