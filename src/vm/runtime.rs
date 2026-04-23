use crate::nan_value::{Arena, NanValue, NanValueConvert};
use crate::replay::session::RecordedOutcome;
use crate::replay::{
    EffectRecord, EffectReplayMode, EffectReplayState, ReplayFailure, json_to_value, value_to_json,
    values_to_json_lossy,
};
use crate::value::Value;

use super::builtin::VmBuiltin;
use super::symbol::VmSymbolTable;
use super::types::VmError;

/// VM execution mode for record/replay.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VmExecutionMode {
    Normal,
    Record,
    Replay,
}

/// Host/runtime bridge for builtin dispatch, effects, and record/replay.
///
/// This is intentionally separate from the core execute loop so the VM stays
/// focused on bytecode mechanics rather than service plumbing.
pub(super) struct VmRuntime {
    allowed_effects: Vec<u32>,
    cli_args: Vec<String>,
    silent_console: bool,
    replay_state: EffectReplayState,
    runtime_policy: Option<crate::config::ProjectConfig>,
    /// Oracle v1: during `aver verify` for an effectful law, install a map
    /// from effect method name (`"Random.int"`) to the fn_id of a stub
    /// function supplied via `given name: Effect.method = [stub]`. When
    /// the VM dispatches a classified effect that has a stub installed,
    /// it calls the stub with `(BranchPath.root, counter, orig_args...)`
    /// instead of invoking the real effect. Counter increments per call;
    /// reset when stubs are installed or cleared. Empty map ⇒ no hook.
    pub(super) oracle_stubs: std::collections::HashMap<String, u32>,
    pub(super) oracle_counter: u32,
    /// Oracle v1: during a verify-trace case, the VM collects every
    /// effect emission the LHS impl makes — effect method name + argument
    /// snapshot as a JSON-ish value list. The verify runner reads this
    /// after LHS eval and wraps it into a `Trace` record for the
    /// `.trace.contains(...)` / `.trace.event(k)` / `.trace.length()`
    /// projections. Empty when not in verify-trace mode.
    pub(super) collected_trace_events: Vec<crate::value::Value>,
    /// When true, every dispatched effect is recorded into
    /// `collected_trace_events` regardless of whether an oracle stub
    /// handled it. The verify runner flips this on before LHS eval and
    /// off after RHS eval, so only LHS emissions land in the trace.
    pub(super) trace_collecting: bool,
}

impl Default for VmRuntime {
    fn default() -> Self {
        Self::new()
    }
}

impl VmRuntime {
    pub(super) fn new() -> Self {
        Self {
            allowed_effects: Vec::new(),
            cli_args: Vec::new(),
            silent_console: false,
            replay_state: EffectReplayState::default(),
            runtime_policy: None,
            oracle_stubs: std::collections::HashMap::new(),
            oracle_counter: 0,
            collected_trace_events: Vec::new(),
            trace_collecting: false,
        }
    }

    pub(super) fn start_trace_collection(&mut self) {
        self.collected_trace_events.clear();
        self.trace_collecting = true;
    }

    pub(super) fn stop_trace_collection(&mut self) {
        self.trace_collecting = false;
    }

    pub(super) fn take_trace_events(&mut self) -> Vec<crate::value::Value> {
        std::mem::take(&mut self.collected_trace_events)
    }

    pub(super) fn record_trace_event(&mut self, effect_name: &str, args: &[crate::value::Value]) {
        if !self.trace_collecting {
            return;
        }
        let event = crate::value::Value::Record {
            type_name: crate::types::effect_event::TYPE_NAME.to_string(),
            fields: vec![
                (
                    crate::types::effect_event::FIELD_METHOD.to_string(),
                    crate::value::Value::Str(effect_name.to_string()),
                ),
                (
                    crate::types::effect_event::FIELD_ARGS.to_string(),
                    crate::value::list_from_vec(args.to_vec()),
                ),
            ]
            .into(),
        };
        self.collected_trace_events.push(event);
    }

    /// Install the oracle-stub map for the current scope (typically a
    /// single verify-law case). `stubs` maps classified effect method
    /// names (e.g. `"Random.int"`) to the fn_id of an Aver stub function
    /// with signature `(BranchPath, Int, orig_args...) -> T`.
    pub(super) fn install_oracle_stubs(&mut self, stubs: std::collections::HashMap<String, u32>) {
        self.oracle_stubs = stubs;
        self.oracle_counter = 0;
    }

    /// Clear the oracle-stub map and reset the counter. Called at the end
    /// of each verify-law case and on any mode transition.
    pub(super) fn clear_oracle_stubs(&mut self) {
        self.oracle_stubs.clear();
        self.oracle_counter = 0;
    }

    pub(super) fn oracle_stub_for(&self, effect_name: &str) -> Option<u32> {
        self.oracle_stubs.get(effect_name).copied()
    }

    pub(super) fn allowed_effects(&self) -> &[u32] {
        &self.allowed_effects
    }

    pub(super) fn set_allowed_effects(&mut self, effects: Vec<u32>) {
        self.allowed_effects = effects;
    }

    pub(super) fn swap_allowed_effects(&mut self, effects: Vec<u32>) -> Vec<u32> {
        std::mem::replace(&mut self.allowed_effects, effects)
    }

    /// Check if a required effect is allowed, supporting namespace shorthand.
    /// E.g., allowed "Disk" (id=X) covers required "Disk.readText" (id=Y).
    fn vm_effect_allowed(&self, required_id: u32, symbols: &VmSymbolTable) -> bool {
        if self.allowed_effects.contains(&required_id) {
            return true;
        }
        // Namespace shorthand: check if any allowed effect is a prefix
        let required_name = match symbols.get(required_id) {
            Some(info) => &info.name,
            None => return false,
        };
        for allowed_id in &self.allowed_effects {
            if let Some(info) = symbols.get(*allowed_id)
                && crate::effects::effect_satisfies(&info.name, required_name)
            {
                return true;
            }
        }
        false
    }

    pub(super) fn set_cli_args(&mut self, args: Vec<String>) {
        self.cli_args = args;
    }

    pub(super) fn cli_args(&self) -> &[String] {
        &self.cli_args
    }

    pub(super) fn set_silent_console(&mut self, silent: bool) {
        self.silent_console = silent;
    }

    pub(super) fn silent_console(&self) -> bool {
        self.silent_console
    }

    pub(super) fn set_runtime_policy(&mut self, config: crate::config::ProjectConfig) {
        self.runtime_policy = Some(config);
    }

    pub(super) fn runtime_policy(&self) -> Option<&crate::config::ProjectConfig> {
        self.runtime_policy.as_ref()
    }

    pub(super) fn independence_mode(&self) -> crate::config::IndependenceMode {
        self.runtime_policy
            .as_ref()
            .map_or(crate::config::IndependenceMode::default(), |c| {
                c.independence_mode
            })
    }

    pub(super) fn start_recording(&mut self) {
        self.replay_state.start_recording();
    }

    pub(super) fn set_record_cap(&mut self, cap: Option<usize>) {
        self.replay_state.set_record_cap(cap);
    }

    pub(super) fn start_replay(&mut self, effects: Vec<EffectRecord>, validate_args: bool) {
        self.replay_state.start_replay(effects, validate_args);
    }

    pub(super) fn execution_mode(&self) -> VmExecutionMode {
        match self.replay_state.mode() {
            EffectReplayMode::Normal => VmExecutionMode::Normal,
            EffectReplayMode::Record => VmExecutionMode::Record,
            EffectReplayMode::Replay => VmExecutionMode::Replay,
        }
    }

    pub fn recorded_effects(&self) -> &[EffectRecord] {
        self.replay_state.recorded_effects()
    }

    pub(super) fn replay_progress(&self) -> (usize, usize) {
        self.replay_state.replay_progress()
    }

    pub(super) fn args_diff_count(&self) -> usize {
        self.replay_state.args_diff_count()
    }

    pub(super) fn is_effect_tracking(&self) -> bool {
        matches!(
            self.replay_state.mode(),
            EffectReplayMode::Record | EffectReplayMode::Replay
        )
    }

    pub(super) fn replay_enter_group(&mut self) {
        self.replay_state.enter_group();
    }

    pub(super) fn replay_exit_group(&mut self) {
        self.replay_state.exit_group();
    }

    pub(super) fn replay_set_branch(&mut self, index: u32) {
        self.replay_state.set_branch(index);
    }

    /// Oracle v1: grab the current (path, counter) pair for an oracle-
    /// stub dispatch and advance the counter. If we're inside a `!`/`?!`
    /// group, use the replay state's branch-aware tracking; otherwise
    /// fall back to the VM-level `oracle_counter` that covers flat
    /// (root-level) effect calls.
    pub(super) fn take_oracle_coordinates(&mut self) -> (String, u32) {
        if self.replay_state.is_inside_group() {
            let path = self.replay_state.oracle_path_string();
            let counter = self.replay_state.oracle_branch_counter().unwrap_or(0);
            self.replay_state.bump_oracle_branch_counter();
            (path, counter)
        } else {
            let c = self.oracle_counter;
            self.oracle_counter += 1;
            (String::new(), c)
        }
    }

    pub(super) fn ensure_replay_consumed(&self) -> Result<(), VmError> {
        self.replay_state
            .ensure_replay_consumed()
            .map_err(|err| match err {
                ReplayFailure::Unconsumed { remaining } => VmError::runtime(format!(
                    "Replay finished with {} unconsumed recorded effect(s)",
                    remaining
                )),
                other => VmError::runtime(format!("invalid replay state: {:?}", other)),
            })
    }

    pub(super) fn invoke_builtin_with_owned(
        &mut self,
        symbols: &VmSymbolTable,
        builtin: VmBuiltin,
        args: &[NanValue],
        arena: &mut Arena,
        owned_mask: u8,
    ) -> Result<NanValue, VmError> {
        // Fast path: if arg 0 is owned and this is a collection mutator,
        // call the owned variant that takes instead of cloning.
        if owned_mask & 1 != 0 {
            let owned_result = match builtin {
                VmBuiltin::MapSet => Some(crate::types::map::set_nv_owned(args, arena)),
                VmBuiltin::VectorSet => Some(crate::types::vector::vec_set_nv_owned(args, arena)),
                _ => None,
            };
            if let Some(result) = owned_result {
                return result.map_err(|err| match err {
                    crate::value::RuntimeError::Error(msg)
                    | crate::value::RuntimeError::ErrorAt { msg, .. } => VmError::runtime(msg),
                    other => VmError::runtime(format!("{:?}", other)),
                });
            }
        }
        self.invoke_builtin(symbols, builtin, args, arena)
    }

    pub(super) fn invoke_builtin(
        &mut self,
        symbols: &VmSymbolTable,
        builtin: VmBuiltin,
        args: &[NanValue],
        arena: &mut Arena,
    ) -> Result<NanValue, VmError> {
        debug_assert!(
            !builtin.is_http_server(),
            "HttpServer builtins require VM callback handling outside VmRuntime"
        );
        self.ensure_builtin_effects_allowed(symbols, builtin)?;
        self.check_runtime_policy(builtin.name(), args, arena)?;

        let builtin_name = builtin.name();
        let required_effects = symbols
            .find(builtin_name)
            .and_then(|symbol_id| symbols.get(symbol_id))
            .map(|info| info.required_effects.as_slice())
            .unwrap_or(&[]);
        let is_effectful = !required_effects.is_empty();
        // Oracle v1: when verify-trace collection is active, record every
        // classified effect dispatched by the LHS impl (pre-invocation
        // snapshot of args) so `.trace.contains(...)` / `.trace.event(k)`
        // can query them after LHS returns.
        if self.trace_collecting
            && is_effectful
            && crate::types::checker::effect_classification::is_classified(builtin_name)
        {
            let arg_vals: Vec<crate::value::Value> =
                args.iter().map(|a| a.to_value(arena)).collect();
            self.record_trace_event(builtin_name, &arg_vals);
        }
        match (is_effectful, self.execution_mode()) {
            (_, VmExecutionMode::Normal) | (false, _) => builtin
                .invoke_nv(args, arena, &self.cli_args, self.silent_console)
                .map_err(|err| match err {
                    crate::value::RuntimeError::Error(msg)
                    | crate::value::RuntimeError::ErrorAt { msg, .. } => VmError::runtime(msg),
                    other => VmError::runtime(format!("{:?}", other)),
                }),
            (true, VmExecutionMode::Record) => {
                // Record-cap safety net: caller (e.g. the browser
                // playground) can set a ceiling via set_record_cap so
                // runaway loops (game on an input-starved stub) stop
                // cleanly instead of hanging the wasm main thread.
                // The partial recording stays intact — callers can
                // still replay everything captured up to the cap.
                if self.replay_state.record_full() {
                    return Err(VmError::runtime(format!(
                        "record cap reached (kept {} effects so far) while calling {} — program was still running. Recording below is a prefix.",
                        self.replay_state.recorded_effects().len(),
                        builtin_name
                    )));
                }
                let args_json = {
                    let vals: Vec<_> = args.iter().map(|a| a.to_value(arena)).collect();
                    values_to_json_lossy(&vals)
                };
                let nv_result = builtin
                    .invoke_nv(args, arena, &self.cli_args, self.silent_console)
                    .map_err(|err| match err {
                        crate::value::RuntimeError::Error(msg)
                        | crate::value::RuntimeError::ErrorAt { msg, .. } => VmError::runtime(msg),
                        other => VmError::runtime(format!("{:?}", other)),
                    })?;
                let result_val = nv_result.to_value(arena);
                let outcome = match value_to_json(&result_val) {
                    Ok(json) => RecordedOutcome::Value(json),
                    Err(e) => RecordedOutcome::RuntimeError(e),
                };
                self.replay_state
                    .record_effect(builtin_name, args_json, outcome, "", 0); // VM: no caller fn/line yet
                Ok(nv_result)
            }
            (true, VmExecutionMode::Replay) => self.replay_builtin(builtin_name, args, arena),
        }
    }

    fn replay_builtin(
        &mut self,
        builtin_name: &str,
        args: &[NanValue],
        arena: &mut Arena,
    ) -> Result<NanValue, VmError> {
        let got_args = {
            let vals: Vec<_> = args.iter().map(|a| a.to_value(arena)).collect();
            values_to_json_lossy(&vals)
        };
        let record = self
            .replay_state
            .replay_effect(builtin_name, Some(got_args))
            .map_err(|err| match err {
                ReplayFailure::Exhausted { effect_type, .. } => VmError::runtime(format!(
                    "Replay exhausted: no more recorded effects for '{}'",
                    effect_type
                )),
                ReplayFailure::Mismatch { seq, expected, got } => VmError::runtime(format!(
                    "Replay mismatch at #{}: expected '{}', got '{}'",
                    seq, expected, got
                )),
                ReplayFailure::ArgsMismatch {
                    seq, effect_type, ..
                } => VmError::runtime(format!(
                    "Replay args mismatch at #{} for '{}'",
                    seq, effect_type
                )),
                ReplayFailure::Unconsumed { remaining } => VmError::runtime(format!(
                    "Replay finished with {} unconsumed recorded effect(s)",
                    remaining
                )),
            })?;
        let result = match &record {
            RecordedOutcome::Value(json) => {
                let val = json_to_value(json).map_err(VmError::runtime)?;
                NanValue::from_value(&val, arena)
            }
            RecordedOutcome::RuntimeError(msg) => return Err(VmError::runtime(msg.clone())),
        };
        Ok(result)
    }

    pub(super) fn ensure_effects_allowed(
        &self,
        symbols: &VmSymbolTable,
        callable_name: &str,
        required_effects: &[u32],
    ) -> Result<(), VmError> {
        if required_effects.is_empty() {
            return Ok(());
        }
        for effect_id in required_effects {
            if !self.vm_effect_allowed(*effect_id, symbols) {
                // Oracle v1: during a verify-law case, an effect that has
                // an installed stub counts as satisfied at this call edge —
                // the stub replaces the effect when the dispatcher gets to
                // it, so the requirement is met even though no surrounding
                // fn declared the effect.
                if let Some(info) = symbols.get(*effect_id)
                    && self.oracle_stubs.contains_key(&info.name)
                {
                    continue;
                }
                let effect_name = symbols
                    .get(*effect_id)
                    .map(|info| info.name.as_str())
                    .unwrap_or("<unknown>");
                return Err(VmError::runtime(format!(
                    "Runtime effect violation: cannot call '{}' (missing effect: {})",
                    callable_name, effect_name
                )));
            }
        }
        Ok(())
    }

    pub(super) fn ensure_builtin_effects_allowed(
        &self,
        symbols: &VmSymbolTable,
        builtin: VmBuiltin,
    ) -> Result<(), VmError> {
        let builtin_name = builtin.name();
        let required_effects = symbols
            .find(builtin_name)
            .and_then(|symbol_id| symbols.get(symbol_id))
            .map(|info| info.required_effects.as_slice())
            .unwrap_or(&[]);
        self.ensure_effects_allowed(symbols, builtin_name, required_effects)
    }

    fn check_runtime_policy(
        &self,
        builtin_name: &str,
        args: &[NanValue],
        arena: &Arena,
    ) -> Result<(), VmError> {
        if self.execution_mode() == VmExecutionMode::Replay {
            return Ok(());
        }
        let Some(policy) = &self.runtime_policy else {
            return Ok(());
        };

        match (builtin_name.split('.').next(), args.first()) {
            (Some("Http"), Some(arg)) => {
                if let Value::Str(url) = arg.to_value(arena) {
                    policy
                        .check_http_host(builtin_name, &url)
                        .map_err(VmError::runtime)?;
                }
            }
            (Some("Disk"), Some(arg)) => {
                if let Value::Str(path) = arg.to_value(arena) {
                    policy
                        .check_disk_path(builtin_name, &path)
                        .map_err(VmError::runtime)?;
                }
            }
            (Some("Env"), Some(arg)) => {
                if let Value::Str(key) = arg.to_value(arena) {
                    policy
                        .check_env_key(builtin_name, &key)
                        .map_err(VmError::runtime)?;
                }
            }
            _ => {}
        }

        Ok(())
    }
}
