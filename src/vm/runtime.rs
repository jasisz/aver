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
    /// Oracle v1: per-event structural coordinates captured at record
    /// time. `group_id` is the `!`/`?!` group index in source order
    /// (None when the emission is outside any group). `branch_idx` is
    /// the current branch inside that group (None at the sequential
    /// level). Parallel to `collected_trace_events` — same length.
    /// Used to project `.trace.group(N).branch(idx).event(k)` chains
    /// without embedding tree metadata inside the Value-level
    /// `EffectEvent` record.
    pub(super) collected_trace_coords: Vec<TraceCoord>,
    /// When true, every dispatched effect is recorded into
    /// `collected_trace_events` regardless of whether an oracle stub
    /// handled it. The verify runner flips this on before LHS eval and
    /// off after RHS eval, so only LHS emissions land in the trace.
    pub(super) trace_collecting: bool,
    /// Oracle v1: the fn under `verify <fn> trace` — set by the verify
    /// runner before the LHS helper runs. Trace events whose immediate
    /// caller fn_id != this root are helper emissions and get filtered
    /// out of `.trace.*` projections. When `None`, no filter applies
    /// (every classified effect is collected — used by non-verify
    /// tests that drive trace collection directly).
    pub(super) trace_root_fn_id: Option<u32>,
    /// Updated by VmExecute on entry to every effect dispatch with the
    /// `fn_id` of the frame that issued the call. Used by
    /// `record_trace_event` to apply the helper-boundary filter.
    pub(super) trace_caller_fn_id: u32,
}

/// Oracle v1: structural coordinates for a recorded trace event.
/// `group_id` / `branch_idx` are `None` for sequential code outside
/// any `!`/`?!` group. `dewey` is the dewey-decimal path string (same
/// form `BranchPath.parse(s)` consumes), empty at the sequential
/// level — used by the `.path()` bridge to produce an Aver-level
/// `BranchPath` value from a recorded event.
#[derive(Debug, Clone, Default)]
pub struct TraceCoord {
    pub group_id: Option<u32>,
    pub branch_idx: Option<u32>,
    pub dewey: String,
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
            collected_trace_coords: Vec::new(),
            trace_collecting: false,
            trace_root_fn_id: None,
            trace_caller_fn_id: 0,
        }
    }

    pub(super) fn start_trace_collection(&mut self) {
        self.collected_trace_events.clear();
        self.collected_trace_coords.clear();
        // Reset the replay scope so `!`/`?!` group ids start at 1 for
        // each verify-trace case. Without this, the ids accumulate
        // across cases and user-visible indices like `.trace.group(0)`
        // stop matching after the first case.
        self.replay_state.reset_scope();
        self.trace_collecting = true;
    }

    pub(super) fn stop_trace_collection(&mut self) {
        self.trace_collecting = false;
        self.trace_root_fn_id = None;
    }

    pub(super) fn set_trace_root_fn_id(&mut self, fn_id: Option<u32>) {
        self.trace_root_fn_id = fn_id;
    }

    pub(super) fn sync_caller_fn_id(&mut self, fn_id: u32) {
        self.trace_caller_fn_id = fn_id;
    }

    /// Oracle v1: should this effect emission land in the user-visible
    /// trace? Filters out helper-boundary emissions — if a root fn is
    /// set, only direct emissions by that fn count. When no root is
    /// set (tests driving trace directly), every event counts.
    fn trace_event_is_direct(&self) -> bool {
        match self.trace_root_fn_id {
            Some(root) => self.trace_caller_fn_id == root,
            None => true,
        }
    }

    pub(super) fn take_trace_events(&mut self) -> Vec<crate::value::Value> {
        self.collected_trace_coords.clear();
        std::mem::take(&mut self.collected_trace_events)
    }

    /// Oracle v1: take both the events and their structural coordinates
    /// together. Coords are parallel to events (same length, same
    /// ordering). Used by `.trace.group(N).*` projections.
    pub(super) fn take_trace_events_with_coords(
        &mut self,
    ) -> (Vec<crate::value::Value>, Vec<TraceCoord>) {
        let events = std::mem::take(&mut self.collected_trace_events);
        let coords = std::mem::take(&mut self.collected_trace_coords);
        (events, coords)
    }

    pub(super) fn record_trace_event(&mut self, effect_name: &str, args: &[crate::value::Value]) {
        if !self.trace_collecting || !self.trace_event_is_direct() {
            return;
        }
        let dewey = self.replay_state.oracle_path_string();
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
                (
                    crate::types::effect_event::FIELD_PATH.to_string(),
                    crate::value::Value::Str(dewey.clone()),
                ),
            ]
            .into(),
        };
        // Oracle v1: capture structural coordinates alongside the
        // event — group_id / branch_idx read from the replay state's
        // live stacks. At the sequential level (outside any group),
        // both are None and the dewey is the empty string (which is
        // the canonical `BranchPath.root` representation).
        let coord = TraceCoord {
            group_id: self.replay_state.current_group_id(),
            branch_idx: self.replay_state.current_branch_idx(),
            dewey,
        };
        self.collected_trace_events.push(event);
        self.collected_trace_coords.push(coord);
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
        //
        // Output-dimension effects (Console.print / .error / .warn) are
        // suppressed — they return Unit, and letting the real host
        // handler fire would pollute the terminal of `aver verify`.
        // Generative / snapshot effects still dispatch for real unless
        // the user supplied a stub via `given` (oracle dispatch is
        // handled earlier in the call site).
        if self.trace_collecting
            && is_effectful
            && crate::types::checker::effect_classification::is_classified(builtin_name)
        {
            // Recording is filtered by helper-boundary
            // (record_trace_event checks trace_event_is_direct);
            // suppression of output effects is NOT filtered, so a
            // helper's `Console.print` call doesn't leak to the
            // terminal of `aver verify` either.
            let arg_vals: Vec<crate::value::Value> =
                args.iter().map(|a| a.to_value(arena)).collect();
            self.record_trace_event(builtin_name, &arg_vals);
            if let Some(classification) =
                crate::types::checker::effect_classification::classify(builtin_name)
            {
                use crate::types::checker::effect_classification::EffectDimension;
                if matches!(classification.dimension, EffectDimension::Output) {
                    return Ok(NanValue::UNIT);
                }
            }
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
                // Oracle v1: during a verify-law case, a classified
                // effect counts as satisfied at this call edge when
                // either:
                //
                //   (a) it has an installed oracle stub (the stub
                //       replaces the effect when the dispatcher gets
                //       to it), or
                //   (b) trace collection is active — we're running the
                //       effectful impl under verify, so classified
                //       output effects (Console.print etc.) are still
                //       legal even without a stub; they get executed
                //       and recorded in the trace buffer.
                if let Some(info) = symbols.get(*effect_id) {
                    let classified =
                        crate::types::checker::effect_classification::is_classified(&info.name);
                    if self.oracle_stubs.contains_key(&info.name) {
                        continue;
                    }
                    if classified && self.trace_collecting {
                        continue;
                    }
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
