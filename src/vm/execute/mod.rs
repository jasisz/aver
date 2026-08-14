mod boundary;
mod dispatch;
mod host;
mod ops;
mod slots;

#[cfg(test)]
mod tests;

pub use slots::VmSlotUniquenessStats;

use super::runtime::VmRuntime;
use super::types::{CallFrame, CodeStore, VmError};
use super::{VmProfileReport, profile::VmProfileState};
use crate::nan_value::{Arena, NanValue};
use slots::OperandStack;

/// The Aver bytecode virtual machine.
pub struct VM {
    /// Operand stack, and the per-arena-slot live-reference count derived from
    /// it. Locals are a window on this stack, so one structure carries both.
    stack: OperandStack,
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
    /// Optional cap on dispatched opcodes per `run_named_function` /
    /// `run` call. `None` (default) = unlimited, the production `aver run`
    /// path. `Some(n)` is set by the verify runner so a single case can't
    /// pin the host on a tail-recursive fn that never converges — AFL
    /// nightly's hang corpus is full of those shapes (e.g. `fn id(x) =
    /// id(-7)` where TCO turns infinite recursion into a goto-loop with
    /// no stack growth). Counter resets at the top of each
    /// `run_named_function` so consecutive cases don't share budget.
    step_limit: Option<u64>,
    /// Mutable scratch buffers for the deforestation lowering's `__buf_*`
    /// intrinsics (0.15 Traversal). Slots are `Option<String>` so finalize
    /// can take ownership and leave a tombstone; `BUFFER_NEW` reuses freed
    /// slots before extending. The pool lives on the host heap, opaque to
    /// the arena GC — buffer handles travel as `Int(idx)` NanValues.
    buffer_pool: Vec<Option<String>>,
    /// Recompute the per-slot reference count from the operand stack on every
    /// dispatched opcode and panic on the first disagreement.
    ///
    /// Off by default, and compiled out entirely without debug assertions: the
    /// audit is `O(stack)` per opcode, which is the price of catching a missed
    /// hook point at the instruction that missed it rather than at whatever
    /// later reader happens to notice a wrong number.
    #[cfg(debug_assertions)]
    audit_slot_refs: bool,
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
            stack: OperandStack::with_capacity(1024),
            frames: Vec::with_capacity(64),
            globals,
            code,
            arena,
            runtime: VmRuntime::new(),
            profile: None,
            error_fn_id: 0,
            error_ip: 0,
            cancelled: None,
            buffer_pool: Vec::new(),
            step_limit: None,
            #[cfg(debug_assertions)]
            audit_slot_refs: false,
        }
    }

    /// Per-slot live-reference tallies from the cross-check against the
    /// compiler's static owned mask.
    pub fn slot_uniqueness_stats(&self) -> VmSlotUniquenessStats {
        self.stack.stats()
    }

    /// Total live references the operand stack holds across every arena slot.
    ///
    /// Zero once a run has finished is the whole-program form of "the count
    /// returns to zero at frame exit": the stack is empty, so no slot may still
    /// be spoken for.
    pub fn live_slot_refs(&self) -> u64 {
        self.stack.total_live_refs()
    }

    /// Live references to one arena slot.
    pub fn live_refs_to_slot(&self, index: u32) -> u32 {
        self.stack.live_refs(index)
    }

    /// Recompute the per-slot count from the operand stack after every
    /// dispatched opcode, and panic on the first disagreement.
    ///
    /// Costs `O(stack)` per instruction, so it is for tests that want a missed
    /// hook point reported at the instruction that missed it. A release build
    /// ignores the request — the audit is compiled out with the assertions.
    pub fn set_slot_ref_audit(&mut self, enabled: bool) {
        #[cfg(debug_assertions)]
        {
            self.audit_slot_refs = enabled;
        }
        #[cfg(not(debug_assertions))]
        let _ = enabled;
    }

    /// Cap the number of dispatched opcodes per `run_named_function` /
    /// `run` call. The verify runner uses this so a tail-recursive case
    /// without a base case (very easy for AFL byte-havoc to produce by
    /// dropping a terminating arm) bails as a `Failure` instead of
    /// pinning the host. `None` removes the cap.
    pub fn set_step_limit(&mut self, limit: Option<u64>) {
        self.step_limit = limit;
    }

    pub fn start_profiling(&mut self) {
        self.profile = Some(VmProfileState::new(self.code.functions.len()));
    }

    pub fn clear_profile(&mut self) {
        self.profile = None;
    }

    pub fn profile_report(&self) -> Option<VmProfileReport> {
        let slot_uniqueness = self.stack.stats();
        self.profile
            .as_ref()
            .map(|profile| profile.report(&self.code, slot_uniqueness))
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

    /// Hostile order-axis: when `true`, the next `CALL_PAR`
    /// (`(a, b)!` independent-product) executes its branches in
    /// reverse source order but assigns each result back to its
    /// source position. The verify runner flips this on for
    /// hostile-order twin cases — a pure law's tuple must stay
    /// invariant, so a divergence proves the "independent" claim
    /// doesn't hold for the active stub map. Always pair with a
    /// reset to `false` before the next case to avoid leaking the
    /// flag into unrelated runs.
    pub fn set_reverse_independent_eval(&mut self, value: bool) {
        self.runtime.set_reverse_independent_eval(value);
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
            inplace_write_escaped: false,
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
