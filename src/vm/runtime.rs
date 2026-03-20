use crate::effects::effect_satisfies;
use crate::nan_value::{Arena, NanValue};
use crate::replay::session::{EffectRecord, RecordedOutcome};
use crate::replay::{json_to_value, value_to_json, values_to_json_lossy};
use crate::services::{console, disk, env, http, random, tcp, time};
use crate::types::{bool, byte, char, float, int, list, map, option, result, string};
use crate::value::RuntimeError;

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
    allowed_effects: Vec<String>,
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

    pub(super) fn set_allowed_effects(&mut self, effects: Vec<String>) {
        self.allowed_effects = effects;
    }

    pub(super) fn swap_allowed_effects(&mut self, effects: Vec<String>) -> Vec<String> {
        std::mem::replace(&mut self.allowed_effects, effects)
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
        builtin_name: &str,
        args: &[NanValue],
        arena: &mut Arena,
    ) -> Result<NanValue, VmError> {
        debug_assert!(
            !is_http_server_builtin(builtin_name),
            "HttpServer builtins require VM callback handling outside VmRuntime"
        );
        self.check_builtin_effects(builtin_name)?;

        let is_effectful = !builtin_effects(builtin_name).is_empty();
        match (is_effectful, self.execution_mode) {
            (_, VmExecutionMode::Normal) | (false, _) => dispatch_builtin_nv(
                builtin_name,
                args,
                arena,
                &self.cli_args,
                self.silent_console,
            ),
            (true, VmExecutionMode::Record) => {
                let args_json = {
                    let vals: Vec<_> = args.iter().map(|a| a.to_value(arena)).collect();
                    values_to_json_lossy(&vals)
                };
                let nv_result = dispatch_builtin_nv(
                    builtin_name,
                    args,
                    arena,
                    &self.cli_args,
                    self.silent_console,
                )?;
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

    fn check_builtin_effects(&self, builtin_name: &str) -> Result<(), VmError> {
        let required = builtin_effects(builtin_name);
        if required.is_empty() {
            return Ok(());
        }
        for effect in required {
            if !self
                .allowed_effects
                .iter()
                .any(|allowed| effect_satisfies(allowed, effect))
            {
                return Err(VmError::Runtime(format!(
                    "Runtime effect violation: cannot call '{}' (missing effect: {})",
                    builtin_name, effect
                )));
            }
        }
        Ok(())
    }
}

pub(super) fn is_http_server_builtin(name: &str) -> bool {
    name.starts_with("HttpServer.")
}

/// Look up which effects a builtin requires.
fn builtin_effects(name: &str) -> &'static [&'static str] {
    let namespace = name.split_once('.').map(|(ns, _)| ns);
    match namespace {
        Some("Console") => console::effects(name),
        Some("Http") => http::effects(name),
        Some("Disk") => disk::effects(name),
        Some("Env") => env::effects(name),
        Some("Random") => random::effects(name),
        Some("Tcp") => tcp::effects(name),
        #[cfg(feature = "terminal")]
        Some("Terminal") => crate::services::terminal::effects(name),
        Some("Time") => time::effects(name),
        _ => &[],
    }
}

/// Dispatch a builtin call by name to the appropriate service/type module.
/// Reuses the existing `call_nv` functions from interpreter services.
fn dispatch_builtin_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
    cli_args: &[String],
    silent_console: bool,
) -> Result<NanValue, VmError> {
    if silent_console && matches!(name, "Console.print" | "Console.error" | "Console.warn") {
        return Ok(NanValue::UNIT);
    }

    let namespace = name.split_once('.').map(|(ns, _)| ns);

    let result = match namespace {
        Some("Args") => crate::services::args::call_nv(name, args, cli_args, arena),
        Some("Console") => console::call_nv(name, args, arena),
        Some("Http") => http::call_nv(name, args, arena),
        Some("Disk") => disk::call_nv(name, args, arena),
        Some("Env") => env::call_nv(name, args, arena),
        Some("Random") => random::call_nv(name, args, arena),
        Some("Tcp") => tcp::call_nv(name, args, arena),
        #[cfg(feature = "terminal")]
        Some("Terminal") => crate::services::terminal::call_nv(name, args, arena),
        Some("Time") => time::call_nv(name, args, arena),
        Some("Bool") => bool::call_nv(name, args, arena),
        Some("Int") => int::call_nv(name, args, arena),
        Some("Float") => float::call_nv(name, args, arena),
        Some("String") => string::call_nv(name, args, arena),
        Some("List") => list::call_nv(name, args, arena),
        Some("Map") => map::call_nv(name, args, arena),
        Some("Char") => char::call_nv(name, args, arena),
        Some("Byte") => byte::call_nv(name, args, arena),
        Some("Result") => result::call_nv(name, args, arena),
        Some("Option") => option::call_nv(name, args, arena),
        _ => None,
    };

    match result {
        Some(Ok(val)) => Ok(val),
        Some(Err(RuntimeError::Error(msg))) => Err(VmError::Runtime(msg)),
        Some(Err(e)) => Err(VmError::Runtime(format!("{:?}", e))),
        None => Err(VmError::Runtime(format!("unknown builtin: {}", name))),
    }
}
