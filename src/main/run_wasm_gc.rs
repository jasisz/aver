//! `aver run --wasm-gc` — CLI bridge over `aver::runtime::wasm_gc`.
//!
//! Owns file I/O, multi-module flatten, recording-file persistence,
//! and pretty-printed error exits. The actual compile-and-execute
//! pipeline lives in `aver::runtime::wasm_gc` so other embedders
//! (parity fuzz target, verify executor) share the same path
//! without going through `process::exit`.

use colored::Colorize;
use std::process;

#[cfg(feature = "wasm")]
use aver::runtime::wasm_gc as rt;

#[cfg(feature = "wasm")]
use super::shared;
#[cfg(feature = "wasm")]
use super::shared::{parse_file, read_file, resolve_module_root};

#[cfg(feature = "wasm")]
use super::commands::{flatten_multimodule, load_compile_deps};

#[cfg(feature = "wasm")]
pub(super) use rt::EffectMode;
#[cfg(feature = "wasm")]
#[allow(unused_imports)]
pub(super) use rt::RunOutcome;

/// `cfg(not(feature = "wasm"))` placeholder so other CLI modules
/// (`replay_cmd::backends`) keep compiling against the same type
/// names. None of the unwrap-this-into-real-effect paths are
/// reachable without the `wasm` feature.
#[cfg(not(feature = "wasm"))]
#[allow(dead_code)]
pub(super) enum EffectMode {
    Normal,
    Record,
    Replay((), bool),
}

#[cfg(not(feature = "wasm"))]
#[allow(dead_code)]
pub(super) struct RunOutcome {
    pub output: (),
    pub effects_consumed: usize,
    pub effects_total: usize,
    pub args_diff_count: usize,
}

pub(super) fn cmd_run_wasm_gc(
    file: &str,
    module_root_override: Option<&str>,
    program_args: Vec<String>,
    record_dir: Option<&str>,
    entry_expr: Option<&str>,
    test_boxed_sequences: bool,
    provider_bindings: &[aver::provider::ProviderBinding],
) {
    #[cfg(feature = "wasm")]
    {
        let mode = match record_dir {
            Some(_) => rt::EffectMode::Record,
            None => rt::EffectMode::Normal,
        };
        let entry_info = match entry_expr {
            Some(src) => match shared::parse_call_expression(src) {
                Ok(info) => Some(info),
                Err(e) => {
                    eprintln!("{}", format!("--expr: {}", e).red());
                    process::exit(1);
                }
            },
            None => None,
        };
        cmd_run_wasm_gc_with_mode(
            file,
            module_root_override,
            program_args,
            mode,
            entry_info,
            record_dir,
            !test_boxed_sequences,
            provider_bindings,
        );
    }
    #[cfg(not(feature = "wasm"))]
    {
        let _ = (
            file,
            module_root_override,
            program_args,
            record_dir,
            entry_expr,
            test_boxed_sequences,
            provider_bindings,
        );
        eprintln!("{}", "WASM requires --features wasm".red());
        process::exit(1);
    }
}

/// CLI wrapper: pretty-prints any failure (parse / typecheck /
/// codegen / runtime) and exits 1. Internal callers that need to
/// keep iterating (`aver replay <dir>`) should use
/// [`try_run_wasm_gc`] directly and inspect the `Result`.
#[cfg(feature = "wasm")]
#[allow(clippy::too_many_arguments)]
pub(super) fn cmd_run_wasm_gc_with_mode(
    file: &str,
    module_root_override: Option<&str>,
    program_args: Vec<String>,
    mode: rt::EffectMode,
    entry_info: Option<(String, Vec<aver::value::Value>)>,
    record_dir: Option<&str>,
    packed_sequences_enabled: bool,
    provider_bindings: &[aver::provider::ProviderBinding],
) {
    if let Err(e) = try_run_wasm_gc(
        file,
        module_root_override,
        program_args,
        mode,
        entry_info,
        record_dir,
        packed_sequences_enabled,
        provider_bindings,
    ) {
        eprintln!("{}", e.red());
        process::exit(1);
    }
}

/// Pure `Result`-returning CLI entry point: file I/O + multi-module
/// flatten + run via `aver::runtime::wasm_gc::run_in_process`, then
/// persist the trace as JSON when `record_dir` is set. Every
/// pipeline stage (parse, typecheck, codegen, instantiate, trap,
/// replay failure, file write) maps onto a single `Err(String)`.
#[cfg(feature = "wasm")]
#[allow(clippy::too_many_arguments)]
pub(super) fn try_run_wasm_gc(
    file: &str,
    module_root_override: Option<&str>,
    program_args: Vec<String>,
    mode: rt::EffectMode,
    entry_info: Option<(String, Vec<aver::value::Value>)>,
    record_dir: Option<&str>,
    packed_sequences_enabled: bool,
    provider_bindings: &[aver::provider::ProviderBinding],
) -> Result<rt::RunOutcome, String> {
    use aver::ir::{NeutralAllocPolicy, PipelineConfig, TypecheckMode};

    let module_root = resolve_module_root(module_root_override);
    let tcp_settings =
        aver::config::ProjectConfig::load_from_dir(std::path::Path::new(&module_root))?
            .map_or_else(aver_rt::tcp::TcpSettings::default, |config| {
                config.tcp_settings.native()
            });
    let source = read_file(file)?;
    let mut items = parse_file(&source, &module_root, file)?;
    let dep_modules = load_compile_deps(
        &items,
        &module_root,
        super::commands::DepLowering::STRING_INDEX_ONLY,
    );
    let neutral_policy = NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            alloc_policy: Some(&neutral_policy),
            dep_modules: &dep_modules,
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: false,
            run_string_index: true,
            run_list_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        return Err(shared::format_type_errors(&tc.errors));
    }
    if let Some(error) = super::commands::capability_target_rejection(
        &items,
        &dep_modules,
        &result
            .typecheck
            .as_ref()
            .expect("wasm-gc run pipeline requested typechecking")
            .capabilities,
        aver::provider::CapabilityTarget::WasmGc,
    ) {
        return Err(error);
    }
    let capabilities = &result
        .typecheck
        .as_ref()
        .expect("wasm-gc run pipeline requested typechecking")
        .capabilities;
    let required =
        aver::provider::required_capability_operations(&items, &dep_modules, capabilities);
    let standard = aver::stdlib::standard_capability_registry_ref();
    if let Some(binding) = provider_bindings.iter().find(|binding| {
        standard.contract(binding.capability()).is_some()
            && required
                .iter()
                .any(|operation| operation.starts_with(&format!("{}.", binding.capability())))
    }) {
        return Err(format!(
            "error[capability-provider-runner-adapter-unavailable]: the embedded wasm-gc runner cannot yet replace standard capability '{}' with provider '{}'. The wasm-gc target supports this capability; this is an embedded-runner adapter limitation, not target incompatibility",
            binding.capability(),
            binding.provider_identity()
        ));
    }
    let custom_plan = aver::codegen::wasm_gc::CapabilityWasmGcPlan::build(capabilities, &required)?;
    let providers = aver::provider::ProviderRegistry::for_program_with_bindings(
        capabilities.clone(),
        provider_bindings.iter().cloned(),
    )?;
    if let Err(preflight) = providers.preflight(required.iter().map(String::as_str)) {
        let mut error = preflight;
        for interface in custom_plan.interfaces() {
            error.push_str(&format!(
                "\n  capability: {}\n  required operations: {}\n  contract_hash: {}\n  model_hash: {}",
                interface.capability,
                interface
                    .operations
                    .iter()
                    .map(|operation| operation.operation.canonical_name.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
                interface.contract_hash,
                interface.model_hash,
            ));
        }
        error.push_str(
            "\n  hint: bind the capability under [providers] in aver.toml so `aver run --wasm-gc` links it through the provider host, or compile with `--target wasm-gc` and supply the generated imports from an external host",
        );
        return Err(error);
    }
    if let rt::EffectMode::Replay(recording, _) = &mode {
        providers.validate_replay_provenance_for_operations(
            &recording.capabilities,
            &recording.effects,
            required.iter().map(String::as_str),
        )?;
    }
    let type_aliases = flatten_multimodule(
        &mut items,
        &dep_modules,
        &result
            .typecheck
            .as_ref()
            .expect("wasm-gc run pipeline requested typechecking")
            .capabilities,
    );
    // Re-run resolver after multi-module flatten so the freshly
    // appended dep fns get a `FnResolution` (slot map + slot_types).
    // The first `pipeline::run` only saw entry items; without this
    // pass, dep fn bodies fall back to the `slots::build_for_fn`
    // params-only path and any local beyond a param trips the wasm
    // validator with a slot-type mismatch. The `_and_reannotate` half
    // is load-bearing: a bare re-resolve wipes `aliased_slots` back to
    // all-`false`, and the wasm-gc in-place fast path then mutated
    // map-held vectors through extracted locals (#950).
    aver::ir::pipeline::resolve_and_reannotate(&mut items);

    let outcome = rt::run_in_process(
        &items,
        result.analysis.as_ref(),
        rt::RunConfig {
            program_args,
            entry_info: entry_info.clone(),
            mode,
            tcp_settings,
            type_aliases,
            packed_sequences_enabled,
            custom_providers: Some(rt::CustomProviderConfig {
                plan: custom_plan.clone(),
                providers: providers.clone(),
            }),
        },
    )?;

    // CLI-side recording persistence. Lib hands back
    // `recorded_effects`; we turn it into a `SessionRecording` and
    // write the JSON file the user asked for via `--record <dir>`.
    if let Some(dir) = record_dir
        && let Some(effects) = outcome.recorded_effects.as_ref()
    {
        let request_id = super::commands::generate_request_id();
        let timestamp = super::commands::generate_timestamp();
        let (record_program_file, record_module_root) =
            super::commands::recording_paths(file, &module_root);
        // For `--expr` runs use the readable `add-7-35` style stem;
        // default `main` runs keep the timestamped request id.
        let file_stem = match &entry_info {
            Some((fn_name, args)) => aver::replay::recording_stem(fn_name, args),
            None => request_id.clone(),
        };
        let out_path = super::commands::prepare_recording_path(dir, &file_stem)
            .map_err(|e| format!("prepare recording path: {}", e))?;
        let entry_fn_label = entry_info
            .as_ref()
            .map(|(n, _)| n.clone())
            .unwrap_or_else(|| "main".to_string());
        let input = match &entry_info {
            Some((_, args)) => aver::replay::encode_entry_args(args)
                .map_err(|e| format!("encode entry args: {}", e))?,
            None => aver::replay::JsonValue::Null,
        };
        let recording = aver::replay::SessionRecording {
            schema_version: 1,
            request_id,
            timestamp,
            program_file: record_program_file,
            module_root: record_module_root,
            entry_fn: entry_fn_label,
            input,
            capabilities: wasm_gc_provider_provenance(capabilities, &custom_plan, &providers),
            effects: effects.clone(),
            output: aver::replay::RecordedOutcome::Value(outcome.output.clone()),
        };
        let json = aver::replay::session_recording_to_string_pretty(&recording);
        std::fs::write(&out_path, json)
            .map_err(|e| format!("write recording {}: {}", out_path.display(), e))?;
        eprintln!("Recorded → {}", out_path.display());
    }

    Ok(outcome)
}

#[cfg(feature = "wasm")]
fn wasm_gc_provider_provenance(
    capabilities: &aver::capability::CapabilityRegistry,
    custom_plan: &aver::codegen::wasm_gc::CapabilityWasmGcPlan,
    providers: &aver::provider::ProviderRegistry,
) -> Vec<aver::replay::CapabilityProvenance> {
    let custom = custom_plan
        .interfaces()
        .iter()
        .map(|interface| interface.capability.as_str())
        .collect::<std::collections::BTreeSet<_>>();
    let mut provenance = aver::provider::shipped_target_provenance(
        aver::provider::CapabilityTarget::WasmGc,
        capabilities,
    )
    .into_iter()
    .filter(|entry| !custom.contains(entry.capability.as_str()))
    .collect::<Vec<_>>();
    provenance.extend(
        providers
            .provenance()
            .into_iter()
            .filter(|entry| custom.contains(entry.capability.as_str())),
    );
    provenance.sort_by(|left, right| left.capability.cmp(&right.capability));
    provenance
}
