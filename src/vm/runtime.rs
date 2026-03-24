use crate::nan_value::{Arena, NanValue};
use crate::replay::session::{EffectRecord, RecordedOutcome};
use crate::replay::{json_to_value, value_to_json, values_to_json_lossy};

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
    execution_mode: VmExecutionMode,
    recorded_effects: Vec<EffectRecord>,
    replay_effects: Vec<EffectRecord>,
    replay_pos: usize,
    validate_replay_args: bool,
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
            execution_mode: VmExecutionMode::Normal,
            recorded_effects: Vec::new(),
            replay_effects: Vec::new(),
            replay_pos: 0,
            validate_replay_args: false,
        }
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
            if let Some(info) = symbols.get(*allowed_id) {
                if crate::effects::effect_satisfies(&info.name, required_name) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn set_cli_args(&mut self, args: Vec<String>) {
        self.cli_args = args;
    }

    pub(super) fn set_silent_console(&mut self, silent: bool) {
        self.silent_console = silent;
    }

    pub(super) fn start_recording(&mut self) {
        self.execution_mode = VmExecutionMode::Record;
        self.recorded_effects.clear();
    }

    pub(super) fn start_replay(&mut self, effects: Vec<EffectRecord>, validate_args: bool) {
        self.execution_mode = VmExecutionMode::Replay;
        self.replay_effects = effects;
        self.replay_pos = 0;
        self.validate_replay_args = validate_args;
    }

    pub(super) fn execution_mode(&self) -> VmExecutionMode {
        self.execution_mode
    }

    pub fn recorded_effects(&self) -> &[EffectRecord] {
        &self.recorded_effects
    }

    pub(super) fn replay_progress(&self) -> (usize, usize) {
        (self.replay_pos, self.replay_effects.len())
    }

    pub(super) fn ensure_replay_consumed(&self) -> Result<(), VmError> {
        if self.execution_mode == VmExecutionMode::Replay
            && self.replay_pos < self.replay_effects.len()
        {
            let remaining = self.replay_effects.len() - self.replay_pos;
            return Err(VmError::Runtime(format!(
                "Replay finished with {} unconsumed recorded effect(s)",
                remaining
            )));
        }
        Ok(())
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

        let builtin_name = builtin.name();
        let required_effects = symbols
            .find(builtin_name)
            .and_then(|symbol_id| symbols.get(symbol_id))
            .map(|info| info.required_effects.as_slice())
            .unwrap_or(&[]);
        let is_effectful = !required_effects.is_empty();
        match (is_effectful, self.execution_mode) {
            (_, VmExecutionMode::Normal) | (false, _) => builtin
                .invoke_nv(args, arena, &self.cli_args, self.silent_console)
                .map_err(|err| match err {
                    crate::value::RuntimeError::Error(msg) => VmError::Runtime(msg),
                    other => VmError::Runtime(format!("{:?}", other)),
                }),
            (true, VmExecutionMode::Record) => {
                let args_json = {
                    let vals: Vec<_> = args.iter().map(|a| a.to_value(arena)).collect();
                    values_to_json_lossy(&vals)
                };
                let nv_result = builtin
                    .invoke_nv(args, arena, &self.cli_args, self.silent_console)
                    .map_err(|err| match err {
                        crate::value::RuntimeError::Error(msg) => VmError::Runtime(msg),
                        other => VmError::Runtime(format!("{:?}", other)),
                    })?;
                let result_val = nv_result.to_value(arena);
                let outcome = match value_to_json(&result_val) {
                    Ok(json) => RecordedOutcome::Value(json),
                    Err(e) => RecordedOutcome::RuntimeError(e),
                };
                let seq = self.recorded_effects.len() as u32 + 1;
                self.recorded_effects.push(EffectRecord {
                    seq,
                    effect_type: builtin_name.to_string(),
                    args: args_json,
                    outcome,
                });
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
        if self.replay_pos >= self.replay_effects.len() {
            return Err(VmError::Runtime(format!(
                "Replay exhausted: no more recorded effects for '{}'",
                builtin_name
            )));
        }
        let record = &self.replay_effects[self.replay_pos];
        if record.effect_type != builtin_name {
            return Err(VmError::Runtime(format!(
                "Replay mismatch at #{}: expected '{}', got '{}'",
                record.seq, record.effect_type, builtin_name
            )));
        }
        if self.validate_replay_args {
            let got_args = {
                let vals: Vec<_> = args.iter().map(|a| a.to_value(arena)).collect();
                values_to_json_lossy(&vals)
            };
            if got_args != record.args {
                return Err(VmError::Runtime(format!(
                    "Replay args mismatch at #{} for '{}'",
                    record.seq, builtin_name
                )));
            }
        }
        let result = match &record.outcome {
            RecordedOutcome::Value(json) => {
                let val = json_to_value(json).map_err(VmError::Runtime)?;
                NanValue::from_value(&val, arena)
            }
            RecordedOutcome::RuntimeError(msg) => return Err(VmError::Runtime(msg.clone())),
        };
        self.replay_pos += 1;
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
                let effect_name = symbols
                    .get(*effect_id)
                    .map(|info| info.name.as_str())
                    .unwrap_or("<unknown>");
                return Err(VmError::Runtime(format!(
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
}
