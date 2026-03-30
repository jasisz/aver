mod boundary;
mod dispatch;
mod host;
mod ops;

#[cfg(test)]
mod tests;

use super::runtime::VmRuntime;
use super::types::{CallFrame, CodeStore, VmError};
use super::{VmProfileReport, profile::VmProfileState};
use crate::nan_value::{Arena, NanValue};

/// The Aver bytecode virtual machine.
pub struct VM {
    stack: Vec<NanValue>,
    frames: Vec<CallFrame>,
    globals: Vec<NanValue>,
    code: CodeStore,
    pub arena: Arena,
    runtime: VmRuntime,
    profile: Option<VmProfileState>,
}

enum ReturnControl {
    Done(NanValue),
    Resume {
        result: NanValue,
        fn_id: u32,
        ip: usize,
        bp: usize,
    },
}

impl VM {
    pub fn new(code: CodeStore, globals: Vec<NanValue>, arena: Arena) -> Self {
        VM {
            stack: Vec::with_capacity(1024),
            frames: Vec::with_capacity(64),
            globals,
            code,
            arena,
            runtime: VmRuntime::new(),
            profile: None,
        }
    }

    pub fn start_profiling(&mut self) {
        self.profile = Some(VmProfileState::new(self.code.functions.len()));
    }

    pub fn clear_profile(&mut self) {
        self.profile = None;
    }

    pub fn profile_report(&self) -> Option<VmProfileReport> {
        self.profile
            .as_ref()
            .map(|profile| profile.report(&self.code))
    }

    pub fn profile_top_bigrams(&self, n: usize) -> Vec<((u8, u8), u64)> {
        self.profile
            .as_ref()
            .map(|p| p.top_bigrams(n))
            .unwrap_or_default()
    }

    /// Set CLI arguments for Args.get().
    pub fn set_cli_args(&mut self, args: Vec<String>) {
        self.runtime.set_cli_args(args);
    }

    pub fn set_silent_console(&mut self, silent: bool) {
        self.runtime.set_silent_console(silent);
    }

    /// Set the runtime policy loaded from `aver.toml`.
    pub fn set_runtime_policy(&mut self, config: crate::config::ProjectConfig) {
        self.runtime.set_runtime_policy(config);
    }

    /// Start recording effectful calls.
    pub fn start_recording(&mut self) {
        self.runtime.start_recording();
    }

    /// Start replaying from recorded effects.
    pub fn start_replay(
        &mut self,
        effects: Vec<crate::replay::session::EffectRecord>,
        validate_args: bool,
    ) {
        self.runtime.start_replay(effects, validate_args);
    }

    pub fn recorded_effects(&self) -> &[crate::replay::session::EffectRecord] {
        self.runtime.recorded_effects()
    }

    pub fn replay_progress(&self) -> (usize, usize) {
        self.runtime.replay_progress()
    }

    pub fn args_diff_count(&self) -> usize {
        self.runtime.args_diff_count()
    }

    pub fn ensure_replay_consumed(&self) -> Result<(), VmError> {
        self.runtime.ensure_replay_consumed()
    }

    pub fn run(&mut self) -> Result<NanValue, VmError> {
        self.run_top_level()?;
        self.run_named_function("main", &[])
    }

    pub fn run_top_level(&mut self) -> Result<(), VmError> {
        if let Some(top_id) = self.code.find("__top_level__") {
            let _ = self.call_function(top_id, &[])?;
        }
        Ok(())
    }

    pub fn run_named_function(
        &mut self,
        name: &str,
        args: &[NanValue],
    ) -> Result<NanValue, VmError> {
        let fn_id = self
            .code
            .symbols
            .find(name)
            .and_then(|symbol_id| self.code.symbols.resolve_function(symbol_id))
            .or_else(|| self.code.find(name))
            .ok_or_else(|| VmError::Runtime(format!("function '{}' not found", name)))?;
        self.runtime
            .set_allowed_effects(self.code.get(fn_id).effects.clone());
        self.call_function(fn_id, args)
    }

    pub fn call_function(&mut self, fn_id: u32, args: &[NanValue]) -> Result<NanValue, VmError> {
        let chunk = self.code.get(fn_id);
        let caller_depth = self.frames.len();
        let arena_mark = self.arena.young_len() as u32;
        let yard_mark = self.arena.yard_len() as u32;
        let handoff_mark = self.arena.handoff_len() as u32;
        let bp = self.stack.len() as u32;
        for arg in args {
            self.stack.push(*arg);
        }
        for _ in args.len()..(chunk.local_count as usize) {
            self.stack.push(NanValue::UNIT);
        }
        self.frames.push(CallFrame {
            fn_id,
            ip: 0,
            bp,
            local_count: chunk.local_count,
            arena_mark,
            yard_base: yard_mark,
            yard_mark,
            handoff_mark,
            globals_dirty: false,
            yard_dirty: false,
            handoff_dirty: false,
            thin: chunk.thin,
            parent_thin: chunk.parent_thin,
        });
        if let Some(profile) = self.profile.as_mut() {
            profile.record_function_entry(chunk, fn_id);
        }
        self.execute_until(caller_depth)
    }
}
