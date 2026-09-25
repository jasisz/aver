//! Owned-value transport and recording at the Work scheduling boundary.

use aver::ast::Type;
use aver::provider::ProviderValue;
use aver::replay::{EffectReplayMode, JsonValue, RecordedOutcome};
use wasmtime::{Caller, Val};

use super::super::{RunWasmGcHost, imports, provider_host};

pub(in crate::runtime::wasm_gc) fn dispatch(
    name: &str,
    caller: &mut Caller<'_, RunWasmGcHost>,
    params: &[Val],
    results: &mut [Val],
) -> Result<(), wasmtime::Error> {
    dispatch_inner(name, caller, params, results).map_err(wasmtime::Error::msg)
}

fn dispatch_inner(
    name: &str,
    caller: &mut Caller<'_, RunWasmGcHost>,
    params: &[Val],
    results: &mut [Val],
) -> Result<(), String> {
    let index = match params.first() {
        Some(Val::I32(index)) if *index >= 0 => *index as usize,
        _ => return Err("work: invalid job kind index".into()),
    };
    let kind = caller
        .data()
        .job_kinds
        .get(index)
        .cloned()
        .ok_or("work: undeclared job kind")?;
    let providers = caller
        .data()
        .providers
        .clone()
        .ok_or("work: missing boundary registry")?;
    let scope = &kind.shape.capability;
    let [task_type, answer_type] = kind.recorded_types();
    if matches!(name, "task" | "complete") {
        let job = caller
            .data_mut()
            .worker_job
            .as_mut()
            .ok_or("work: worker transport called outside a worker")?;
        if job.kind != index {
            return Err("work: worker kind mismatch".into());
        }
        if name == "task" {
            let task = job.task.take().ok_or("work: worker task already read")?;
            results[0] = provider_host::encode_value(
                caller,
                ProviderValue::OptionSome(Box::new(task)),
                &task_type,
                scope,
                None,
                &providers,
            )?
            .ok_or("work: missing boxed task")?;
        } else {
            if job.outcome.is_some() {
                return Err("work: worker completed twice".into());
            }
            let ty = Type::Option(Box::new(kind.shape.payload));
            let value = provider_host::decode_value(caller, params.get(1), &ty, scope, &providers)?;
            let ProviderValue::OptionSome(value) = value else {
                return Err("work: missing boxed answer".into());
            };
            caller
                .data_mut()
                .worker_job
                .as_mut()
                .expect("worker checked")
                .outcome = Some(*value);
        }
        return Ok(());
    }
    let work = caller
        .data()
        .host_work
        .clone()
        .ok_or("work: host has no scheduler")?;
    let tracing = caller
        .data()
        .recorder
        .as_ref()
        .is_some_and(|recorder| recorder.mode() != EffectReplayMode::Normal);
    let caller_fn = match params.last() {
        Some(Val::I32(token)) => caller
            .data()
            .caller_fn_table
            .get(*token as usize)
            .cloned()
            .unwrap_or_default(),
        _ => String::new(),
    };
    if name == "submit" {
        let value =
            provider_host::decode_value(caller, params.get(1), &task_type, scope, &providers)?;
        let ProviderValue::OptionSome(task) = value else {
            return Err("work: missing boxed task".into());
        };
        let args = if tracing {
            vec![provider_host::provider_value_to_json(
                &task,
                &kind.shape.task,
                scope,
                &providers,
            )?]
        } else {
            Vec::new()
        };
        let recorded = replay(caller, &kind.begin.canonical_name, &args)?;
        if let Some(json) = &recorded
            && let Some(error) = json.get("$err").and_then(JsonValue::as_str)
        {
            results[0] = refused(caller, error)?;
            return Ok(());
        }
        let trace = recorded
            .as_ref()
            .map(|json| {
                let handle = json
                    .get("$ok")
                    .and_then(|ok| ok.get("$capabilityResource"))
                    .ok_or("work: malformed recorded handle")?;
                if handle.get("type").and_then(JsonValue::as_str) != Some("Work.Job") {
                    return Err("work: wrong recorded resource type");
                }
                handle
                    .get("trace")
                    .and_then(JsonValue::as_str)
                    .and_then(|trace| trace.parse::<i64>().ok())
                    .filter(|trace| *trace >= 0)
                    .map(|trace| trace as u64)
                    .ok_or("work: malformed recorded trace token")
            })
            .transpose()?;
        // The engine queues a job begun at the limit rather than refusing it,
        // so a replay starts the recorded job whatever `max-jobs` this host
        // runs with.
        let outcome = work.begin(index, (*task).clone(), trace);
        let json = match &outcome {
            Ok(job) => imports::json_ok_work_job(trace.unwrap_or(job.id()) as i64),
            Err(error) => serde_json::json!({"$err": error}),
        };
        if let Some(recorded) = recorded
            && recorded != json
        {
            return Err(format!(
                "replay {}: job admission differs from the recording",
                kind.begin.canonical_name
            ));
        }
        record(caller, &kind.begin.canonical_name, args, json, &caller_fn);
        results[0] = match outcome {
            Ok(job) => factory(
                caller,
                "__work_v1_started",
                &[
                    Val::I64(trace.unwrap_or(job.id()) as i64),
                    Val::I32(index as i32),
                ],
            )?,
            Err(error) => refused(caller, &error)?,
        };
        return Ok(());
    }
    if name == "take" {
        let id = imports::work_job_id(caller, params.get(1).ok_or("work: missing handle")?)
            .map_err(|e| e.to_string())?;
        let args = vec![imports::json_work_job(id)];
        let recorded = replay(caller, &kind.take.canonical_name, &args)?;
        let job = work.job(id, Some(index));
        let value = if let Some(json) = recorded {
            let expected =
                provider_host::provider_value_from_json(&json, &answer_type, scope, &providers)?;
            // A recorded pending result remains pending even if today's CPU
            // has already completed it. A recorded completion waits for and
            // checks the actual pure computation before consuming its result.
            if !matches!(&expected, ProviderValue::ResultOk(value) if matches!(**value, ProviderValue::OptionNone))
            {
                if let Ok(job) = &job {
                    job.settle_by(std::time::Instant::now() + std::time::Duration::from_secs(30));
                }
                let actual = take_value(job.and_then(|job| job.take()));
                let actual = provider_host::provider_value_to_json(
                    &actual,
                    &answer_type,
                    scope,
                    &providers,
                )?;
                if actual != json {
                    return Err(format!(
                        "replay {}: recomputed job outcome differs",
                        kind.take.canonical_name
                    ));
                }
            }
            expected
        } else {
            take_value(job.and_then(|job| job.take()))
        };
        if tracing {
            let json =
                provider_host::provider_value_to_json(&value, &answer_type, scope, &providers)?;
            record(caller, &kind.take.canonical_name, args, json, &caller_fn);
        }
        results[0] =
            provider_host::encode_value(caller, value, &answer_type, scope, None, &providers)?
                .ok_or("work: missing take result")?;
        return Ok(());
    }
    Err(format!("work: unknown ABI operation {name}"))
}

fn take_value(answer: Result<Option<ProviderValue>, String>) -> ProviderValue {
    match answer {
        Ok(Some(value)) => {
            ProviderValue::ResultOk(Box::new(ProviderValue::OptionSome(Box::new(value))))
        }
        Ok(None) => ProviderValue::ResultOk(Box::new(ProviderValue::OptionNone)),
        Err(error) => ProviderValue::ResultErr(Box::new(ProviderValue::String(error))),
    }
}

fn refused(caller: &mut Caller<'_, RunWasmGcHost>, error: &str) -> Result<Val, String> {
    let providers = caller
        .data()
        .providers
        .clone()
        .ok_or("work: missing boundary registry")?;
    let value = provider_host::encode_value(
        caller,
        ProviderValue::String(error.into()),
        &Type::Str,
        "Work",
        None,
        &providers,
    )?
    .ok_or("work: missing error string")?;
    factory(caller, "__work_v1_refused", &[value])
}

fn factory(
    caller: &mut Caller<'_, RunWasmGcHost>,
    name: &str,
    params: &[Val],
) -> Result<Val, String> {
    let function = caller
        .get_export(name)
        .and_then(|export| export.into_func())
        .ok_or_else(|| format!("work: missing factory {name}"))?;
    let mut result = [Val::AnyRef(None)];
    function
        .call(caller, params, &mut result)
        .map_err(|error| format!("work: factory: {error:#}"))?;
    Ok(result[0])
}

fn replay(
    caller: &mut Caller<'_, RunWasmGcHost>,
    operation: &str,
    args: &[JsonValue],
) -> Result<Option<JsonValue>, String> {
    let Some(recorder) = caller.data_mut().recorder.as_mut() else {
        return Ok(None);
    };
    if recorder.mode() != EffectReplayMode::Replay {
        return Ok(None);
    }
    match recorder
        .replay_effect(operation, Some(args.to_vec()))
        .map_err(|error| format!("replay {operation}: {error:?}"))?
    {
        RecordedOutcome::Value(value) => Ok(Some(value)),
        RecordedOutcome::RuntimeError(error) => Err(format!("replay {operation}: {error}")),
    }
}

fn record(
    caller: &mut Caller<'_, RunWasmGcHost>,
    operation: &str,
    args: Vec<JsonValue>,
    value: JsonValue,
    function: &str,
) {
    if let Some(recorder) = caller.data_mut().recorder.as_mut()
        && recorder.mode() == EffectReplayMode::Record
    {
        recorder.record_effect(operation, args, RecordedOutcome::Value(value), function, 0);
    }
}
