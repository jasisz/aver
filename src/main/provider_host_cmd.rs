//! CLI-side planning and repair text for the cached provider VM host.
//!
//! A project that binds providers in `aver.toml` has said what its programs
//! mean. VM commands, embedded wasip2, and wasm-gc run/replay therefore build
//! (once) and reuse the Rust host that links those packages whenever the
//! program reaches a bound capability; a backend without an adapter refuses
//! instead of running a different implementation.

use std::collections::{BTreeSet, HashMap, HashSet};
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::ExitStatus;

use aver::codegen::rust as rust_codegen;

use super::cli::Commands;
use super::commands::{load_report_program_with_cache, resolve_av_inputs};

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
    if let Commands::Compile {
        file,
        module_root,
        target: super::cli::CompileTarget::WasmGc,
        pack: Some(super::cli::DeployPack::Wasmtime),
        preset: None,
        ..
    } = command
    {
        let module_root = super::shared::resolve_module_root(module_root.as_deref());
        let config = match aver::config::ProjectConfig::load_from_dir(Path::new(&module_root)) {
            Ok(config) => config,
            Err(error) => return Some(Err(error)),
        };
        let composition = match config.and_then(|config| config.provider_manifest) {
            Some(manifest) => {
                match plan_for_programs(std::slice::from_ref(file), &module_root, &manifest) {
                    Ok(plan) => plan.composition,
                    Err(error) => return Some(Err(error)),
                }
            }
            None => rust_codegen::composition::ProviderComposition::default(),
        };
        if let Some(error) =
            wasm_gc_standard_override_error(&composition, "compile", file, &module_root)
        {
            return Some(Err(error));
        }
        return Some(crate::provider_vm_host::run_cached_pack_host(
            raw_args,
            &composition,
            Path::new(&module_root),
        ));
    }

    if let Commands::Replay {
        recording,
        wasm_gc: true,
        ..
    } = command
    {
        return run_wasm_gc_replay_if_requested(recording, raw_args);
    }

    let (input, inputs, module_root, backend, command_name) = match command {
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
                HostBackend::Hosted(crate::provider_vm_host::ProviderHostBackend::WasmGc)
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
    let plan = match plan_for_programs(&inputs, &module_root, &manifest) {
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
            if backend == crate::provider_vm_host::ProviderHostBackend::WasmGc
                && let Some(error) = wasm_gc_standard_override_error(
                    &plan.composition,
                    command_name,
                    &input,
                    &module_root,
                )
            {
                return Some(Err(error));
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

/// Replay discovers its program and module root from the recording rather
/// than from command-line arguments. All recordings handled by one cached
/// host must therefore belong to the same project/provider composition.
fn run_wasm_gc_replay_if_requested(
    recording_path: &str,
    raw_args: &[OsString],
) -> Option<Result<ExitStatus, String>> {
    let files = match super::replay_cmd::collect_recording_files(recording_path) {
        Ok(files) => files,
        Err(_) => return None,
    };
    let mut module_root = None::<String>;
    let mut inputs = Vec::with_capacity(files.len());
    for path in &files {
        let raw = match std::fs::read_to_string(path) {
            Ok(raw) => raw,
            Err(_) => return None,
        };
        let recording = match aver::replay::parse_session_recording(&raw) {
            Ok(recording) => recording,
            Err(_) => return None,
        };
        let root = super::replay_cmd::resolve_replay_module_root(path, &recording);
        if module_root.as_ref().is_some_and(|known| known != &root) {
            return Some(Err(format!(
                "error[capability-provider-replay-project]: `aver replay --wasm-gc` cannot link one cached Rust provider host for recordings from multiple module roots ('{}' and '{root}'); replay each project separately",
                module_root.as_deref().unwrap_or_default()
            )));
        }
        inputs.push(super::replay_cmd::resolve_replay_program_file(
            &recording, &root,
        ));
        module_root = Some(root);
    }
    let module_root = module_root?;
    let manifest = aver::config::ProjectConfig::load_from_dir(Path::new(&module_root))
        .ok()
        .flatten()?
        .provider_manifest?;
    let plan = match plan_for_programs(&inputs, &module_root, &manifest) {
        Ok(plan) => plan,
        Err(error) => return Some(Err(error)),
    };
    if plan.composition.bindings.is_empty() {
        return None;
    }
    if let Some(error) =
        wasm_gc_standard_override_error(&plan.composition, "replay", recording_path, &module_root)
    {
        return Some(Err(error));
    }
    Some(crate::provider_vm_host::run_cached_host(
        raw_args,
        &plan.composition,
        crate::provider_vm_host::ProviderHostBackend::WasmGc,
        Path::new(&module_root),
    ))
}

/// Program-defined capabilities use the contract-derived `aver:user/*` ABI.
/// Compiler-shipped standard capabilities still use their older specialised
/// `aver/*` imports, so silently accepting an override here would run a
/// different provider than aver.toml selected.
fn wasm_gc_standard_override_error(
    composition: &rust_codegen::composition::ProviderComposition,
    command: &str,
    input: &str,
    module_root: &str,
) -> Option<String> {
    let standard = aver::stdlib::standard_capability_registry_ref();
    let binding = composition
        .bindings
        .iter()
        .find(|binding| standard.contract(&binding.capability).is_some())?;
    Some(format!(
        "error[capability-provider-runner-adapter-unavailable]: `aver {command} {input}` selected the configured Rust provider for standard capability '{}', but the embedded wasm-gc runner cannot yet replace that capability's compiler-shipped `aver/*` import adapter. The wasm-gc target supports this capability; this is an embedded-runner adapter limitation, not target incompatibility. Run the same project on the bytecode VM, compile with `--target rust`, or supply the wasm-gc imports from an external host. Provider configuration: {}",
        binding.capability,
        crate::provider_vm_host::describe_source(&binding.source, Path::new(module_root))
    ))
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
/// The plan spans the modules the command itself walks: every module of
/// every input's program, each once. A program that does not load, or a
/// module that does not parse or type, is left out: the command itself
/// reports it, and no binding can be active for it.
fn plan_for_programs(
    inputs: &[String],
    module_root: &str,
    manifest: &aver::config::ProviderPackageManifest,
) -> Result<ProgramPlan, String> {
    let mut capabilities = aver::capability::CapabilityRegistry::default();
    let mut required = BTreeSet::new();
    let mut planned = HashSet::new();
    let mut load_cache = aver::source::ProgramLoadCache::default();
    let mut checked: HashMap<PathBuf, Result<(), String>> = HashMap::new();
    for input in inputs {
        let Ok(program) = load_report_program_with_cache(input, module_root, &mut load_cache)
        else {
            continue;
        };
        for module in program
            .report_units()
            .filter(|module| planned.insert(aver::source::canonicalize_path(&module.path)))
        {
            let key = aver::source::canonicalize_path(&module.path);
            if let Some(fault) = &module.fault {
                checked.insert(key, Err(fault.to_string()));
                continue;
            }
            let loaded = match program.loaded_dependencies_for(module) {
                Ok(loaded) => loaded,
                Err(error) => {
                    checked.insert(key, Err(error.to_string()));
                    continue;
                }
            };
            let failed_dependency = loaded.iter().find_map(|dependency| {
                checked
                    .get(&aver::source::canonicalize_path(&dependency.path))
                    .and_then(|status| status.as_ref().err())
            });
            if let Some(error) = failed_dependency {
                checked.insert(key, Err(error.clone()));
                continue;
            }

            let mut items = module.items.clone();
            let user_program_len = items.len();
            let tc = aver::ir::pipeline::front_gate(
                &mut items,
                &aver::ir::TypecheckMode::WithCheckedLoaded(&loaded),
                user_program_len,
            );
            if !tc.errors.is_empty() {
                checked.insert(
                    key,
                    Err(tc
                        .errors
                        .iter()
                        .map(|error| error.message.as_str())
                        .collect::<Vec<_>>()
                        .join("\n")),
                );
                continue;
            }
            let modules = loaded
                .iter()
                .map(aver::codegen::ModuleInfo::from_loaded)
                .collect::<Vec<_>>();
            required.extend(aver::provider::required_capability_operations(
                &items,
                &modules,
                &tc.capabilities,
            ));
            capabilities.merge(tc.capabilities);
            checked.insert(key, Ok(()));
        }
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
        let Ok(source) = std::fs::read_to_string(&path) else {
            continue;
        };
        let Ok(items) =
            aver::source::parse_project_source(&source, module_root, &path.to_string_lossy())
        else {
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
    let manifest = aver::config::ProjectConfig::load_from_dir(Path::new(module_root))
        .ok()
        .flatten()
        .and_then(|config| config.provider_manifest);
    let configured = manifest.as_ref().is_some_and(|manifest| {
        manifest
            .bindings
            .iter()
            .any(|binding| binding.capability == capability)
    });
    // A job kind is answered by a function of the program, never by a
    // provider package: pointing its user at [[providers.bindings]] would
    // send them to the one binding that conflicts with the one they wrote.
    let work_binding = manifest.as_ref().and_then(|manifest| {
        manifest
            .work_bindings
            .iter()
            .find(|binding| binding.capability == capability)
    });
    if let Some(binding) = work_binding {
        return Some(format!(
            "hint: capability '{capability}' is a job kind bound to work = \"{}\", and no backend runs a job in this build yet.\n\
             Nothing else is missing: the binding is what will start '{}' off the turn once jobs run.",
            binding.function, binding.function
        ));
    }

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

/// The one door where a program's job kinds meet the manifest bindings that
/// run them, for every command that prepares a program for execution.
///
/// `aver check` and `aver audit` report the same findings through canonical
/// source analysis, which owns their spans and their JSON shape; this door
/// serves the commands that go straight to a backend.
///
/// Detection is parse-level: a job kind is a property of a capability
/// contract, so a program without one pays only for the walk it was going to
/// do anyway, and the full typecheck runs only when a job kind exists.
pub(super) fn work_program_rejection(command: &Commands) -> Option<String> {
    use aver::capability::work::WorkTarget;

    let (input, module_root, target) = match command {
        Commands::Run {
            file,
            module_root,
            wasm_gc,
            wasip2,
            ..
        } => {
            let target = if *wasip2 {
                WorkTarget::Wasip2
            } else if *wasm_gc {
                WorkTarget::WasmGc
            } else {
                WorkTarget::Vm
            };
            (file.clone(), module_root, target)
        }
        Commands::Compile {
            file,
            module_root,
            target,
            ..
        } => {
            let target = match target {
                super::cli::CompileTarget::Rust => WorkTarget::Rust,
                super::cli::CompileTarget::WasmGc => WorkTarget::WasmGc,
                super::cli::CompileTarget::Wasip2 => WorkTarget::Wasip2,
            };
            (file.clone(), module_root, target)
        }
        Commands::Verify {
            file,
            module_root,
            wasm_gc,
            ..
        } => {
            let target = if *wasm_gc {
                WorkTarget::WasmGc
            } else {
                WorkTarget::Vm
            };
            (file.clone(), module_root, target)
        }
        _ => return None,
    };
    // `aver verify` and `aver compile` accept a directory as well as a file;
    // the door is the same for every program the command was pointed at.
    let inputs = if Path::new(&input).is_file() {
        vec![input]
    } else {
        resolve_av_inputs(&input).ok()?
    };
    let module_root = super::shared::resolve_module_root(module_root.as_deref());
    let mut cache = aver::source::ProgramLoadCache::default();
    let mut findings = Vec::new();
    for input in inputs {
        for rendered in work_input_rejections(&input, &module_root, target, &mut cache) {
            if !findings.contains(&rendered) {
                findings.push(rendered);
            }
        }
    }
    if findings.is_empty() {
        return None;
    }
    Some(findings.join("\n"))
}

/// The same door for one program file: its job kinds, its manifest bindings
/// and the target it was asked for, rendered as complete diagnostic lines.
fn work_input_rejections(
    input: &str,
    module_root: &str,
    target: aver::capability::work::WorkTarget,
    cache: &mut aver::source::ProgramLoadCache,
) -> Vec<String> {
    let empty = Vec::new();
    let Ok(program) = load_report_program_with_cache(input, module_root, cache) else {
        return empty;
    };
    let mut capabilities = aver::capability::CapabilityRegistry::default();
    for module in program.report_units() {
        if module.fault.is_some() {
            continue;
        }
        let (part, errors) =
            aver::capability::CapabilityRegistry::from_module(&module.dep_name, &module.items);
        if errors.is_empty() {
            capabilities.merge(part);
        }
    }
    if aver::capability::work::job_kinds(&capabilities).is_empty() {
        return empty;
    }

    // A job kind is present, so the bound function's own signature decides.
    let Some(entry) = program.report_units().last() else {
        return empty;
    };
    let mut items = entry.items.clone();
    let user_program_len = items.len();
    let Ok(loaded) = program.loaded_dependencies_for(entry) else {
        return empty;
    };
    let tc = aver::ir::pipeline::front_gate(
        &mut items,
        &aver::ir::TypecheckMode::WithLoaded(&loaded),
        user_program_len,
    );
    if !tc.errors.is_empty() {
        // Type errors are the command's own report; a binding cannot be
        // judged against signatures that did not survive the typecheck.
        return empty;
    }
    let manifest = aver::config::ProjectConfig::load_from_dir(Path::new(module_root))
        .ok()
        .flatten()
        .and_then(|config| config.provider_manifest);
    aver::capability::work::gate(
        &tc.capabilities,
        manifest.as_ref(),
        &tc.fn_sigs,
        aver::visibility::module_decl(&entry.items).map(|module| module.name.as_str()),
        target,
    )
    .iter()
    .map(aver::capability::work::WorkDiagnostic::rendered)
    .collect()
}
