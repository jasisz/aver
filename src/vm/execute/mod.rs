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
    /// Last executing (fn_id, ip) — updated at top of dispatch loop for error reporting.
    error_fn_id: u32,
    error_ip: u32,
    /// Cooperative cancellation flag — set by sibling threads on error.
    cancelled: Option<std::sync::Arc<std::sync::atomic::AtomicBool>>,
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
            error_fn_id: 0,
            error_ip: 0,
            cancelled: None,
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

    /// Cap the recorder at `cap` events. Useful for browser record
    /// runs where a game with no quit path would otherwise hang the
    /// wasm main thread. `None` (default) = unlimited, matching CLI.
    pub fn set_record_cap(&mut self, cap: Option<usize>) {
        self.runtime.set_record_cap(cap);
    }

    /// Start replaying from recorded effects.
    pub fn start_replay(
        &mut self,
        effects: Vec<crate::replay::session::EffectRecord>,
        validate_args: bool,
    ) {
        self.runtime.start_replay(effects, validate_args);
    }

    pub fn set_allowed_effects(&mut self, effects: Vec<u32>) {
        self.runtime.set_allowed_effects(effects);
    }

    /// Oracle v1: install the oracle-stub map for a verify-law case.
    /// Maps classified effect method names (e.g. `"Random.int"`) to the
    /// fn_id of an Aver stub function with signature
    /// `(BranchPath, Int, orig_args...) -> T`. Counter resets to 0.
    pub fn install_oracle_stubs(&mut self, stubs: std::collections::HashMap<String, u32>) {
        self.runtime.install_oracle_stubs(stubs);
    }

    /// Clear the oracle-stub map and reset the counter. Always call this
    /// at the end of the verify-law case so the next case (or normal
    /// evaluation) doesn't see stale substitutions.
    pub fn clear_oracle_stubs(&mut self) {
        self.runtime.clear_oracle_stubs();
    }

    /// Resolve an Aver top-level function name to its VM fn_id. Used by
    /// the verify runner when wiring stubs from a `given` clause.
    pub fn find_fn_id(&self, name: &str) -> Option<u32> {
        self.code.find(name)
    }

    /// Oracle v1: start collecting classified-effect emissions into a
    /// per-case trace buffer. Call before evaluating a verify-trace
    /// case's LHS; pair with `take_trace_events` after.
    pub fn start_trace_collection(&mut self) {
        self.runtime.start_trace_collection();
    }

    /// Oracle v1: set (or clear) the root fn_id used by the helper-
    /// boundary filter — only emissions whose immediate caller fn_id
    /// matches the root count towards `.trace.*` projections. Pass
    /// `None` (or don't call it) to disable filtering, so every
    /// classified effect lands in the trace.
    pub fn set_trace_root_fn_id(&mut self, fn_id: Option<u32>) {
        self.runtime.set_trace_root_fn_id(fn_id);
    }

    /// Oracle v1: stop collection without consuming the buffer.
    pub fn stop_trace_collection(&mut self) {
        self.runtime.stop_trace_collection();
    }

    /// Oracle v1: take the collected trace events, stopping collection
    /// and clearing the buffer. The returned list is what
    /// `fn.trace.contains(...)` / `.event(k)` / `.length()` operate on.
    pub fn take_trace_events(&mut self) -> Vec<crate::value::Value> {
        let events = self.runtime.take_trace_events();
        self.runtime.stop_trace_collection();
        events
    }

    /// Oracle v1: take both events and structural coordinates
    /// together. Used by tree-navigation projections like
    /// `.trace.group(N).event(k)` — the coords identify which
    /// `!`/`?!` group each event came from in source order.
    pub fn take_trace_events_with_coords(
        &mut self,
    ) -> (
        Vec<crate::value::Value>,
        Vec<crate::vm::runtime::TraceCoord>,
    ) {
        let out = self.runtime.take_trace_events_with_coords();
        self.runtime.stop_trace_collection();
        out
    }

    pub fn set_cancelled(&mut self, flag: std::sync::Arc<std::sync::atomic::AtomicBool>) {
        self.cancelled = Some(flag);
    }

    /// Check if this VM has been cancelled by a sibling branch.
    fn is_cancelled(&self) -> bool {
        self.cancelled
            .as_ref()
            .is_some_and(|f| f.load(std::sync::atomic::Ordering::Relaxed))
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
        // If there is no `main` function, finish silently (as expected).
        let has_main = self
            .code
            .symbols
            .find("main")
            .and_then(|sid| self.code.symbols.resolve_function(sid))
            .or_else(|| self.code.find("main"))
            .is_some();
        if has_main {
            self.run_named_function("main", &[])
        } else {
            Ok(NanValue::UNIT)
        }
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
            .ok_or_else(|| VmError::runtime(format!("function '{}' not found", name)))?;
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
        self.execute_until(caller_depth).map_err(|err| {
            // Cold path: resolve source location from line_table.
            let loc = self
                .code
                .resolve_source_location(self.error_fn_id, self.error_ip);
            err.with_location(loc.map(|(file, line)| super::types::VmSourceLoc {
                file: file.to_string(),
                line,
                fn_name: self.code.get(self.error_fn_id).name.clone(),
            }))
        })
    }
}
