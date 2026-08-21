//! CLI-side planning and repair text for the cached provider VM host.
//!
//! A project that binds providers in `aver.toml` has said what its programs
//! mean. `run`, `verify` and `audit` therefore build (once) and reuse the
//! Rust host that links those packages whenever the program reaches a bound
//! capability; a backend that cannot host a Rust provider refuses instead of
//! running without it.

use std::collections::BTreeSet;
use std::ffi::OsString;
use std::path::Path;
use std::process::ExitStatus;

use aver::codegen::rust as rust_codegen;

use super::cli::Commands;
use super::commands::resolve_av_inputs;
use super::shared::{parse_file, read_file};

/// Where a command executes the program, seen from the provider host.
enum HostBackend {
    Hosted(crate::provider_vm_host::ProviderHostBackend),
    /// A backend with no provider host; bound providers are an error there.
    Unhosted(&'static str),
}

/// Run `command` inside the cached provider host when the project binds a
/// provider the program reaches. `None` means the command runs in this
/// process: no `[providers]` table, or none of its bindings is active for
/// the program.
pub(super) fn run_if_requested(
    command: &Commands,
    raw_args: &[OsString],
) -> Option<Result<ExitStatus, String>> {
    let (input, files, module_root, backend, command_name) = match command {
        Commands::Run {
            file,
            module_root,
            self_host,
            wasm_gc,
            wasip2,
            ..
        } => {
            let backend = if *wasip2 {
                HostBackend::Hosted(crate::provider_vm_host::ProviderHostBackend::Wasip2)
            } else if *wasm_gc {
                HostBackend::Unhosted("wasm-gc")
            } else if *self_host {
                HostBackend::Unhosted("self-host")
            } else {
                HostBackend::Hosted(crate::provider_vm_host::ProviderHostBackend::Vm)
            };
            (
                file.clone(),
                vec![file.clone()],
                module_root,
                backend,
                "run",
            )
        }
        Commands::Verify {
            file,
            module_root,
            wasm_gc,
            ..
        } => {
            let backend = if *wasm_gc {
                HostBackend::Unhosted("wasm-gc")
            } else {
                HostBackend::Hosted(crate::provider_vm_host::ProviderHostBackend::Vm)
            };
            (
                file.clone(),
                resolve_av_inputs(file).ok()?,
                module_root,
                backend,
                "verify",
            )
        }
        Commands::Audit {
            path, module_root, ..
        } => (
            path.clone(),
            resolve_av_inputs(path).ok()?,
            module_root,
            HostBackend::Hosted(crate::provider_vm_host::ProviderHostBackend::Vm),
            "audit",
        ),
        _ => return None,
    };
    let module_root = super::shared::resolve_module_root(module_root.as_deref());
    // A manifest that does not load is the command's own error to report.
    let manifest = aver::config::ProjectConfig::load_from_dir(Path::new(&module_root))
        .ok()
        .flatten()?
        .provider_manifest?;
    let plan = match plan_for_files(&files, &module_root, &manifest) {
        Ok(plan) => plan,
        Err(error) => return Some(Err(error)),
    };
    if plan.composition.bindings.is_empty() {
        return None;
    }
    let binding_line = |binding: &rust_codegen::composition::ProviderCompositionBinding| {
        format!(
            "\n  capability '{}' -> package '{}' from {}",
            binding.capability,
            binding.package,
            crate::provider_vm_host::describe_source(&binding.source, Path::new(&module_root))
        )
    };
    Some(match backend {
        HostBackend::Hosted(backend) => {
            // The wasip2 host adapts a binding through the generated WIT
            // import, which exists only for WIT-lowerable contracts. Say so
            // before Cargo builds anything.
            if backend == crate::provider_vm_host::ProviderHostBackend::Wasip2 {
                let manifest = aver::provider::CapabilityTargetManifest::build(
                    &plan.capabilities,
                    &plan.required,
                )
                .expect("required operations came from the capability registry");
                let unhostable = manifest
                    .required_unsupported(aver::provider::CapabilityTarget::Wasip2)
                    .filter_map(|row| {
                        let binding = plan
                            .composition
                            .bindings
                            .iter()
                            .find(|binding| binding.capability == row.capability)?;
                        let aver::provider::TargetBindingStatus::Unsupported { reason } =
                            &row.status
                        else {
                            return None;
                        };
                        Some(format!(
                            "{} ({}: {})",
                            binding_line(binding),
                            reason.code(),
                            reason.description()
                        ))
                    })
                    .collect::<String>();
                if !unhostable.is_empty() {
                    return Some(Err(unhosted_provider_error(
                        &unhostable,
                        "wasip2",
                        command_name,
                        &input,
                        &module_root,
                    )));
                }
            }
            crate::provider_vm_host::run_cached_host(
                raw_args,
                &plan.composition,
                backend,
                Path::new(&module_root),
            )
        }
        HostBackend::Unhosted(backend) => {
            let bindings = plan
                .composition
                .bindings
                .iter()
                .map(binding_line)
                .collect::<String>();
            Err(unhosted_provider_error(
                &bindings,
                backend,
                command_name,
                &input,
                &module_root,
            ))
        }
    })
}

/// What the provider host needs to know about the program(s) a command was
/// pointed at.
struct ProgramPlan {
    composition: rust_codegen::composition::ProviderComposition,
    capabilities: aver::capability::CapabilityRegistry,
    required: BTreeSet<String>,
}

/// Validate the complete schema-1 composition before provider code reaches
/// Cargo. Generated Rust and the cached VM host intentionally share this plan.
///
/// A file that does not parse, load, or type is left out of the plan: the
/// command itself reports it, and no binding can be active for it.
fn plan_for_files(
    files: &[String],
    module_root: &str,
    manifest: &aver::config::ProviderPackageManifest,
) -> Result<ProgramPlan, String> {
    let mut capabilities = aver::capability::CapabilityRegistry::default();
    let mut required = BTreeSet::new();
    for file in files {
        let Ok(source) = read_file(file) else {
            continue;
        };
        let Ok(mut items) = parse_file(&source) else {
            continue;
        };
        aver::ir::pipeline::tco(&mut items);
        let Ok(modules) = aver::source::load_compile_deps(&items, module_root) else {
            continue;
        };
        let tc = aver::ir::pipeline::typecheck_gate(
            &items,
            &aver::ir::TypecheckMode::Full {
                base_dir: Some(module_root),
            },
            &items,
        );
        if !tc.errors.is_empty() {
            continue;
        }
        required.extend(aver::provider::required_capability_operations(
            &items,
            &modules,
            &tc.capabilities,
        ));
        capabilities.merge(tc.capabilities);
    }

    let known_capabilities = known_project_capabilities(module_root, &capabilities, Some(manifest));
    let composition = rust_codegen::composition::plan_for_project(
        &capabilities,
        &required,
        Some(manifest),
        &known_capabilities,
    )?;
    Ok(ProgramPlan {
        composition,
        capabilities,
        required,
    })
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
        // A dotted binding such as `Infra.Kv` names the module by its path;
        // the file itself declares only the last segment (`module Kv`), the
        // same rule the loader applies to `depends [Infra.Kv]`.
        let expected = binding
            .capability
            .rsplit('.')
            .next()
            .unwrap_or(binding.capability.as_str());
        let declares_expected_module = items.iter().any(|item| {
            matches!(
                item,
                aver::ast::TopLevel::Module(module)
                    if module.name == binding.capability || module.name == expected
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

/// The program reaches a capability whose provider `aver.toml` binds, on a
/// backend that cannot host it. `bindings` lists them, one line each. Says
/// what is bound, where it would run, and what to change; the repair
/// command repeats `input` as the user spelled it, directory or file.
fn unhosted_provider_error(
    bindings: &str,
    backend: &str,
    command: &str,
    input: &str,
    module_root: &str,
) -> String {
    format!(
        "error[capability-provider-unhosted]: the {backend} backend cannot host a Rust provider, and this program reaches a capability that aver.toml binds to one:{bindings}\n  \
         Run it on the bytecode VM, which builds and reuses the provider host: `aver {command} {input} --module-root {module_root}`; \
         or compile with `aver compile --target rust`, which links the same package; \
         or remove the binding from [providers] in aver.toml."
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

    let mut repair = if configured {
        format!(
            "hint: capability '{capability}' has a Rust provider configured in aver.toml, but this run did not install it.\n\
             The bytecode VM builds and reuses the provider host:\n  aver {command} {file} --module-root {module_root}"
        )
    } else {
        format!(
            "hint: capability '{capability}' has no Rust provider configured in aver.toml.\n\
             Add a [[providers.bindings]] entry for it under [providers]; aver {command} then builds and reuses the provider host."
        )
    };
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
}

#[cfg(test)]
mod tests {
    use super::missing_provider_repair;

    #[test]
    fn unrelated_text_has_no_provider_repair() {
        assert!(missing_provider_repair("ordinary error", ".", "run", "app.av").is_none());
    }
}
