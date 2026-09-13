//! In-process adapter from the contract-derived raw wasm-gc ABI to the
//! transport-neutral Rust provider boundary.
//!
//! Provider packages do not opt into execution targets. The same checked
//! `ProviderBinding` used by the VM is linked into the cached CLI host; this
//! adapter only lifts guest values to `ProviderValue` and lowers the answer
//! back through the compiler-emitted `__cap_abi_*` helpers.

use std::collections::HashMap;

use aver::ast::Type;
use aver::codegen::wasm_gc::CapabilityWasmGcOperationPlan;
use aver::provider::{ProviderRegistry, ProviderResource, ProviderValue};
use aver::replay::RecordedOutcome;
use wasmtime::{Caller, Val};

use super::{CustomProviderConfig, RunWasmGcHost};

mod codec;
mod guest;
mod replay;

use codec::{decode_value, encode_value, ensure_live_resources};
pub(super) use replay::ReplayResource;
use replay::{provider_value_from_json, provider_value_to_json, provider_values_to_json_lossy};

#[derive(Clone)]
pub(super) struct BoundOperation {
    capability: String,
    operation: CapabilityWasmGcOperationPlan,
    providers: ProviderRegistry,
}

/// Host metadata carried inside a guest `externref`. Wasm source cannot forge
/// it, and every later provider call checks both the nominal resource type and
/// the process-local binding identity before exposing the payload.
#[derive(Clone)]
struct BoundResource {
    binding_id: u64,
    type_name: String,
    resource: ProviderResource,
}

/// A resource reconstructed from a recorded trace can correlate subsequent
/// recorded calls, but it must never cross into a live/reissued provider.
pub(super) fn operation_imports(
    config: &CustomProviderConfig,
) -> Result<HashMap<(String, String), BoundOperation>, String> {
    let mut imports = HashMap::new();
    for interface in config.plan.interfaces() {
        let module = format!("aver:user/{}", interface.interface_name);
        for operation in &interface.operations {
            let key = (module.clone(), operation.import_name.clone());
            if imports
                .insert(
                    key.clone(),
                    BoundOperation {
                        capability: interface.capability.clone(),
                        operation: operation.clone(),
                        providers: config.providers.clone(),
                    },
                )
                .is_some()
            {
                return Err(format!(
                    "error[capability-provider-adapter]: duplicate wasm-gc provider import `{}.{}`",
                    key.0, key.1
                ));
            }
        }
    }
    Ok(imports)
}

pub(super) fn invoke(
    bound: &BoundOperation,
    caller: &mut Caller<'_, RunWasmGcHost>,
    params: &[Val],
    results: &mut [Val],
) -> Result<(), wasmtime::Error> {
    let operation = &bound.operation.operation;
    let mut incoming = params.iter();
    let mut args = Vec::with_capacity(bound.operation.abi_params.len());
    for (index, ty) in bound.operation.abi_params.iter().enumerate() {
        let val = if matches!(ty, Type::Unit) {
            None
        } else {
            incoming.next()
        };
        args.push(decode_value(
            caller,
            val,
            ty,
            &bound.capability,
            &bound.providers,
        )
        .map_err(|message| {
            wasmtime::Error::msg(format!(
                "error[capability-provider-adapter-arguments]: invalid wasm-gc value for '{}' parameter {}: {message}",
                operation.canonical_name, index
            ))
        })?);
    }

    // Every raw capability import carries the source caller id after its
    // declared parameters. It is transport metadata, not a provider value.
    let caller_id = match incoming.next() {
        Some(Val::I32(value)) => *value,
        other => {
            return Err(wasmtime::Error::msg(format!(
                "error[capability-provider-adapter-arguments]: '{}' is missing its trailing caller id (received {other:?})",
                operation.canonical_name
            )));
        }
    };
    if incoming.next().is_some() {
        return Err(wasmtime::Error::msg(format!(
            "error[capability-provider-adapter-arguments]: '{}' received too many wasm-gc arguments",
            operation.canonical_name
        )));
    }

    let args_json = provider_values_to_json_lossy(
        &args,
        &bound.operation.abi_params,
        &bound.capability,
        &bound.providers,
    );
    if operation.is_effectful()
        && caller
            .data()
            .recorder
            .as_ref()
            .is_some_and(|state| state.mode() == aver::replay::EffectReplayMode::Replay)
    {
        let recorded = caller
            .data_mut()
            .recorder
            .as_mut()
            .expect("replay mode checked above")
            .replay_effect(&operation.canonical_name, Some(args_json.clone()))
            .map_err(|error| {
                wasmtime::Error::msg(format!("replay {}: {error:?}", operation.canonical_name))
            })?;
        let RecordedOutcome::Value(json) = recorded else {
            let RecordedOutcome::RuntimeError(message) = recorded else {
                unreachable!()
            };
            return Err(wasmtime::Error::msg(format!(
                "replay {}: trace recorded a runtime error ({message})",
                operation.canonical_name
            )));
        };
        let recorded_value = provider_value_from_json(
            &json,
            &bound.operation.abi_result,
            &bound.capability,
            &bound.providers,
        )
        .map_err(|message| {
            wasmtime::Error::msg(format!(
                "replay {}: recorded outcome does not match {}: {message}",
                operation.canonical_name,
                bound.operation.abi_result.display()
            ))
        })?;
        if matches!(
            operation.replay,
            Some(
                aver::capability::ReplaySemantics::Recorded
                    | aver::capability::ReplaySemantics::Suppressed
            )
        ) {
            return encode_result(bound, caller, recorded_value, results);
        }
    }

    ensure_live_resources(&args).map_err(wasmtime::Error::msg)?;
    let value = bound
        .providers
        .invoke_provider_values(&operation.canonical_name, &args)
        .map_err(wasmtime::Error::msg)?;

    if operation.is_effectful()
        && caller
            .data()
            .recorder
            .as_ref()
            .is_some_and(|state| state.mode() == aver::replay::EffectReplayMode::Record)
    {
        let outcome = provider_value_to_json(
            &value,
            &bound.operation.abi_result,
            &bound.capability,
            &bound.providers,
        )
        .map_or_else(RecordedOutcome::RuntimeError, RecordedOutcome::Value);
        let caller_fn = caller
            .data()
            .caller_fn_table
            .get(caller_id.max(0) as usize)
            .cloned()
            .unwrap_or_else(|| "main".to_string());
        caller
            .data_mut()
            .recorder
            .as_mut()
            .expect("record mode checked above")
            .record_effect(
                &operation.canonical_name,
                args_json,
                outcome,
                &caller_fn,
                operation.line,
            );
    }

    encode_result(bound, caller, value, results)
}

fn encode_result(
    bound: &BoundOperation,
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: ProviderValue,
    results: &mut [Val],
) -> Result<(), wasmtime::Error> {
    let operation = &bound.operation.operation;
    let received = value.shape();
    let encoded = encode_value(
        caller,
        value,
        &bound.operation.abi_result,
        &bound.capability,
        operation.minted_resource.as_deref(),
        &bound.providers,
    )
    .map_err(|message| {
        let provider = bound
            .providers
            .binding(&bound.capability)
            .map(|binding| binding.provider_identity())
            .unwrap_or("<missing>");
        wasmtime::Error::msg(format!(
            "error[capability-provider-invalid-return]: provider '{provider}' returned an invalid value for '{}': expected {}, received {received}; {message}",
            operation.canonical_name,
            bound.operation.abi_result.display()
        ))
    })?;
    match (encoded, results) {
        (None, []) => Ok(()),
        (Some(value), [slot]) => {
            *slot = value;
            Ok(())
        }
        (None, slots) => Err(wasmtime::Error::msg(format!(
            "error[capability-provider-adapter-result]: '{}' allocated {} result slot(s), expected 0",
            operation.canonical_name,
            slots.len()
        ))),
        (Some(_), slots) => Err(wasmtime::Error::msg(format!(
            "error[capability-provider-adapter-result]: '{}' allocated {} result slot(s), expected 1",
            operation.canonical_name,
            slots.len()
        ))),
    }
}

/// jasisz/aver#1329 — put one job-kind operation in the recording, and in
/// replay hand back what was recorded.
///
/// The module has already run the bound function and minted the handle; this
/// is only the turn's record of it, so the recording a wasm-gc run writes has
/// the VM's shape and a VM recording replays here. The boundary value crosses
/// as the ABI the capability plan emitted helpers for: the task boxed in an
/// `Option`, and `take`'s answer as the `Result<Option<R>, String>` it is.
pub(super) fn record_job_operation(
    caller: &mut Caller<'_, RunWasmGcHost>,
    kind_index: i32,
    take: bool,
    boundary: Option<&Val>,
    handle_id: i64,
    caller_fn: &str,
) -> Result<Option<Option<wasmtime::Rooted<wasmtime::AnyRef>>>, wasmtime::Error> {
    // A run that neither records nor replays has nothing for this import to
    // do: the module already has the answer. Leaving before the boundary
    // value is read is what keeps a recorder-shaped import invisible to an
    // ordinary run.
    let mode = caller
        .data()
        .recorder
        .as_ref()
        .map_or(aver::replay::EffectReplayMode::Normal, |state| state.mode());
    if matches!(mode, aver::replay::EffectReplayMode::Normal) {
        return Ok(None);
    }
    let Some(kind) = caller
        .data()
        .job_kinds
        .get(usize::try_from(kind_index.max(0)).unwrap_or(0))
        .cloned()
    else {
        return Err(wasmtime::Error::msg(format!(
            "work: the module named job kind {kind_index}, which this program does not declare"
        )));
    };
    let Some(providers) = caller.data().providers.clone() else {
        return Err(wasmtime::Error::msg(
            "work: recording a job needs the program's provider registry",
        ));
    };
    let scope = kind.shape.capability.clone();
    let [task_ty, answer_ty] = kind.recorded_types();
    let recorded_ty = answer_ty.clone();
    let handle_json = super::imports::json_work_job(handle_id);
    let operation = if take {
        kind.take.canonical_name.clone()
    } else {
        kind.begin.canonical_name.clone()
    };

    let (args, boundary_ty) = if take {
        (vec![handle_json.clone()], recorded_ty)
    } else {
        let boxed = decode_value(caller, boundary, &task_ty, &scope, &providers)
            .map_err(|message| wasmtime::Error::msg(format!("{operation}: {message}")))?;
        let ProviderValue::OptionSome(task) = boxed else {
            return Err(wasmtime::Error::msg(format!(
                "{operation}: the module boxed no task"
            )));
        };
        let json = provider_value_to_json(&task, &kind.shape.task, &scope, &providers)
            .map_err(|message| wasmtime::Error::msg(format!("{operation}: {message}")))?;
        (vec![json], task_ty)
    };
    let _ = boundary_ty;

    if matches!(mode, aver::replay::EffectReplayMode::Replay) {
        let recorded = caller
            .data_mut()
            .recorder
            .as_mut()
            .expect("replay mode checked above")
            .replay_effect(&operation, Some(args))
            .map_err(|error| wasmtime::Error::msg(format!("replay {operation}: {error:?}")))?;
        let RecordedOutcome::Value(json) = recorded else {
            return Err(wasmtime::Error::msg(format!(
                "replay {operation}: the trace recorded a runtime error"
            )));
        };
        if !take {
            return Ok(Some(None));
        }
        let value =
            provider_value_from_json(&json, &answer_ty, &scope, &providers).map_err(|message| {
                wasmtime::Error::msg(format!(
                    "replay {operation}: recorded outcome does not match {}: {message}",
                    answer_ty.display()
                ))
            })?;
        let encoded = encode_value(caller, value, &answer_ty, &scope, None, &providers)
            .map_err(|message| wasmtime::Error::msg(format!("replay {operation}: {message}")))?;
        return Ok(Some(match encoded {
            Some(Val::AnyRef(value)) => value,
            _ => None,
        }));
    }

    if matches!(mode, aver::replay::EffectReplayMode::Record) {
        let outcome = if take {
            let value = decode_value(caller, boundary, &answer_ty, &scope, &providers)
                .map_err(|message| wasmtime::Error::msg(format!("{operation}: {message}")))?;
            provider_value_to_json(&value, &answer_ty, &scope, &providers)
                .map_or_else(RecordedOutcome::RuntimeError, RecordedOutcome::Value)
        } else {
            RecordedOutcome::Value(super::imports::json_ok_work_job(handle_id))
        };
        caller
            .data_mut()
            .recorder
            .as_mut()
            .expect("record mode checked above")
            .record_effect(&operation, args, outcome, caller_fn, 0);
    }
    Ok(None)
}
