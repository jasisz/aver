mod boundary;
mod dispatch;
mod host;
mod ops;
mod slots;

#[cfg(test)]
mod tests;

pub use slots::{
    VmRuntimeOwnershipStats, VmSlotUniquenessStats, grants_the_mirror_could_not_afford,
};

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
    /// Deferred setup error for the effects-only legacy replay API. Keeping
    /// its existing infallible signature is source-compatible, while the
    /// first run still refuses custom capability events that lack provenance.
    replay_setup_error: Option<String>,
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
    /// Dispatched opcodes consumed by the call currently running. Zeroed and
    /// restored by `execute_until`, so a nested call never spends the outer
    /// call's budget.
    step_count: u64,
    /// Dispatched opcodes the last completed call consumed — however it
    /// ended. The verify runner reads it to report what a case actually cost,
    /// which is the only way a raised budget can be shown to have bought
    /// something.
    last_step_count: u64,
    /// Verify compiles the whole module but executes one concrete case at a
    /// time. It still validates checked contract/model identities up front,
    /// while an absent custom binding is allowed to fail only if that case
    /// actually dispatches the capability operation.
    defer_missing_capability_providers_to_dispatch: bool,
    /// Mutable scratch buffers for the deforestation lowering's `__buf_*`
    /// intrinsics (0.15 Traversal). Slots are `Option<String>` so finalize
    /// can take ownership and leave a tombstone; `BUFFER_NEW` reuses freed
    /// slots before extending. The pool lives on the host heap, opaque to
    /// the arena GC — buffer handles travel as `Int(idx)` NanValues.
    buffer_pool: Vec<Option<String>>,
    /// Element vectors for the list-build lowering's `__lst_*`
    /// intrinsics. Slots are `Option<Vec<NanValue>>` so finalize can take
    /// ownership and leave a tombstone; `LIST_BUILDER_NEW` reuses freed
    /// slots before extending.
    ///
    /// Like the buffer pool this lives on the host heap, opaque to the
    /// arena collector — which is exactly why nothing with an arena index
    /// may be stored here. `LIST_BUILDER_PUSH` checks every element and
    /// hands the builder back as an ordinary cons chain the moment one is
    /// not immediate, so a value the collector would move is never held
    /// anywhere the collector cannot see.
    ///
    /// A builder whose loop exits without finalizing (the error arm of a
    /// parser, say) leaves its slot behind. [`LIST_BUILDER_POOL_SLOTS`]
    /// caps how many can accumulate; past the cap a new builder starts
    /// as the cons chain instead, which needs no slot at all.
    list_builder_pool: Vec<Option<Vec<NanValue>>>,
    /// Slots [`Self::list_builder_pool`] has handed back, so allocating
    /// one is a pop rather than a scan for the first hole.
    list_builder_free: Vec<usize>,
    /// Byte vectors for the byte-sink lowering's `__byt_*` intrinsics,
    /// pooled exactly like [`Self::list_builder_pool`] and for the same
    /// reason — a growing arena object threaded through a tail-recursive
    /// loop is rewritten at every frame boundary. Simpler than the list
    /// pool in one way: every payload is a `u8` and the recorded
    /// offender is a host value, so nothing here ever carries an arena
    /// index and there is no mid-build transition to the cons chain.
    /// Pool exhaustion is the only fallback (the builder then travels
    /// as the reversed cons chain and the finalizer validates it
    /// natively).
    byte_builder_pool: Vec<Option<VmByteBuilder>>,
    /// Slots [`Self::byte_builder_pool`] has handed back.
    byte_builder_free: Vec<usize>,
    /// How the compiler's static owned mask and the operand stack's own view of
    /// who holds a slot compared, over this VM's lifetime. Maintained where the
    /// answer has a reader — a debug build, where the same comparison is also an
    /// assertion, or a run that asked for a profile — and left at zero on the
    /// default release path, where computing it would mean walking the stack
    /// once per collection write for nobody.
    slot_uniqueness: VmSlotUniquenessStats,
    /// What the runtime decided about the map writes the compiler declined.
    /// Maintained everywhere, release included: it is the receipt for a decision
    /// this VM took, not an observation somebody asked for.
    runtime_ownership: VmRuntimeOwnershipStats,
    /// What the runtime decided about the vector writes the compiler GRANTED —
    /// the same buckets read in the revocation direction (see
    /// [`VM::vector_ownership_stats`]). Maintained everywhere, release
    /// included, for the same reason as `runtime_ownership`.
    vector_ownership: VmRuntimeOwnershipStats,
}

/// One pooled byte builder: the octets collected so far, and the first
/// pushed element that was not one. `bad` is sticky — `Bytes.fromList`
/// reports the FIRST offender, so once it is set the later pushes have
/// nothing left to decide. The offender travels as a host
/// [`aver_rt::AverInt`] copy, never an arena reference, which is what
/// keeps this pool invisible to the collector by construction.
struct VmByteBuilder {
    bytes: Vec<u8>,
    bad: Option<(aver_rt::AverInt, usize)>,
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
    pub fn new(code: CodeStore, globals: Vec<NanValue>, mut arena: Arena) -> Self {
        // This hidden type must exist before `build_parallel_base_context`
        // clones the arena: child results preserve record type ids when they
        // are deep-imported back into the parent.
        crate::nan_value::register_capability_resource_type(&mut arena);
        // Two root sets the arena has no way to learn about on its own, both
        // fixed before the first instruction runs. A global and a chunk
        // constant hold their map for as long as the program does, so a map
        // reachable from either must never be taken in place — and a constant
        // is the sharper of the two, because every re-evaluation of the literal
        // that produced it gets the same slot back.
        for value in &globals {
            arena.note_held_elsewhere(*value);
        }
        for chunk in &code.functions {
            for value in &chunk.constants {
                arena.note_held_elsewhere(*value);
            }
        }
        VM {
            stack: Vec::with_capacity(1024),
            frames: Vec::with_capacity(64),
            globals,
            code,
            arena,
            runtime: VmRuntime::new(),
            replay_setup_error: None,
            profile: None,
            error_fn_id: 0,
            error_ip: 0,
            cancelled: None,
            buffer_pool: Vec::new(),
            list_builder_pool: Vec::new(),
            list_builder_free: Vec::new(),
            byte_builder_pool: Vec::new(),
            byte_builder_free: Vec::new(),
            step_limit: None,
            step_count: 0,
            last_step_count: 0,
            defer_missing_capability_providers_to_dispatch: false,
            slot_uniqueness: VmSlotUniquenessStats::default(),
            runtime_ownership: VmRuntimeOwnershipStats::default(),
            vector_ownership: VmRuntimeOwnershipStats::default(),
        }
    }

    /// Cap the number of dispatched opcodes per `run_named_function` /
    /// `run` call. The verify runner uses this so a tail-recursive case
    /// without a base case (very easy for AFL byte-havoc to produce by
    /// dropping a terminating arm) bails as a `Failure` instead of
    /// pinning the host. `None` removes the cap.
    pub fn set_step_limit(&mut self, limit: Option<u64>) {
        self.step_limit = limit;
    }

    /// Dispatched opcodes the last `run_named_function` / `run` call
    /// consumed, whether it returned a value or hit the step limit.
    pub fn last_step_count(&self) -> u64 {
        self.last_step_count
    }

    /// Keep contract/hash preflight, but let an absent provider fail at the
    /// operation boundary. Used by concrete verify cases so an unrelated case
    /// is not rejected merely because another function needs a custom host.
    pub fn defer_missing_capability_providers_to_dispatch(&mut self, defer: bool) {
        self.defer_missing_capability_providers_to_dispatch = defer;
    }

    pub fn start_profiling(&mut self) {
        self.profile = Some(VmProfileState::new(self.code.functions.len()));
    }

    pub fn clear_profile(&mut self) {
        self.profile = None;
    }

    pub fn profile_report(&self) -> Option<VmProfileReport> {
        let slot_uniqueness = self.slot_uniqueness;
        let runtime_ownership = self.runtime_ownership;
        let vector_ownership = self.vector_ownership;
        self.profile.as_ref().map(|profile| {
            profile.report(
                &self.code,
                slot_uniqueness,
                runtime_ownership,
                vector_ownership,
            )
        })
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

    /// Install an explicitly constructed, contract-checked provider set.
    /// Intended for embedded hosts and tests; there is no CLI/env override.
    pub fn set_provider_registry(
        &mut self,
        providers: std::sync::Arc<crate::provider::ProviderRegistry>,
    ) {
        self.runtime.set_provider_registry(providers);
    }

    pub fn provider_registry(&self) -> std::sync::Arc<crate::provider::ProviderRegistry> {
        self.runtime.provider_registry()
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
        self.replay_setup_error = self
            .runtime
            .provider_registry()
            .validate_replay_provenance_for_operations(
                &[],
                &effects,
                self.code
                    .required_capability_operations
                    .iter()
                    .map(String::as_str),
            )
            .err();
        self.runtime.start_replay(effects, validate_args);
    }

    pub fn start_replay_with_provenance(
        &mut self,
        effects: Vec<crate::replay::session::EffectRecord>,
        provenance: &[crate::replay::CapabilityProvenance],
        validate_args: bool,
    ) -> Result<(), String> {
        self.runtime
            .provider_registry()
            .validate_replay_provenance_for_operations(
                provenance,
                &effects,
                self.code
                    .required_capability_operations
                    .iter()
                    .map(String::as_str),
            )?;
        self.replay_setup_error = None;
        self.runtime.start_replay(effects, validate_args);
        Ok(())
    }

    pub fn set_allowed_effects(&mut self, effects: Vec<u32>) {
        self.runtime.set_allowed_effects(effects);
    }

    /// Install the operation/Oracle stub map for one expanded verify case.
    /// Keys are canonical operation names; values are Aver fn ids.
    pub fn install_oracle_stubs(&mut self, stubs: std::collections::HashMap<String, u32>) {
        self.runtime.install_oracle_stubs(stubs);
    }

    /// Clear verify-time operation stubs and reset the Oracle counter.
    pub fn clear_oracle_stubs(&mut self) {
        self.runtime.clear_oracle_stubs();
    }

    /// Guard a plain cases-form verify against crossing an unstubbed host
    /// effect boundary. `None` restores normal execution semantics.
    pub fn set_plain_verify_fn(&mut self, fn_name: Option<String>) {
        self.runtime.set_plain_verify_fn(fn_name);
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

    fn preflight_capability_providers(&self) -> Result<(), VmError> {
        if let Some(error) = &self.replay_setup_error {
            return Err(VmError::runtime(error.clone()));
        }
        let contracts = self.runtime.provider_registry();
        let mut required = Vec::new();
        for name in &self.code.required_capability_operations {
            // Verify/oracle execution substitutes the declared capability
            // model before provider dispatch. It deliberately has no live
            // binding and is already pinned by the checked model hash in the
            // proof/trust path.
            if self.runtime.oracle_stub_for(name).is_some() {
                continue;
            }
            let operation = contracts.contracts().operation(name).ok_or_else(|| {
                VmError::runtime(format!(
                    "capability contract missing at runtime for '{name}'"
                ))
            })?;
            let module = operation.module.as_str();
            let (expected_contract_hash, expected_model_hash) = self
                .code
                .required_capability_contracts
                .get(module)
                .ok_or_else(|| {
                    VmError::runtime(format!(
                        "error[capability-provider-mismatch]: compiled capability '{}' has no checked contract_hash",
                        module
                    ))
                })?;
            let supplied_contract = contracts.contracts().contract(module).ok_or_else(|| {
                VmError::runtime(format!(
                    "capability contract missing at runtime for '{name}'"
                ))
            })?;
            if supplied_contract.contract_hash != *expected_contract_hash {
                let provider = contracts
                    .binding(module)
                    .map(|binding| binding.provider_identity())
                    .unwrap_or("<unbound>");
                return Err(VmError::runtime(format!(
                    "error[capability-provider-mismatch]: provider '{}' for '{}' supplied contract_hash {}, expected {}",
                    provider, module, supplied_contract.contract_hash, expected_contract_hash
                )));
            }
            if supplied_contract.model_hash != *expected_model_hash {
                return Err(VmError::runtime(format!(
                    "error[capability-provider-mismatch]: runtime registry for '{}' supplied model_hash {}, expected {}",
                    module, supplied_contract.model_hash, expected_model_hash
                )));
            }
            if self.runtime.trace_collecting && operation.is_effectful() {
                continue;
            }
            if self.runtime.execution_mode() == super::runtime::VmExecutionMode::Replay
                && operation.is_effectful()
                && operation.replay != Some(crate::capability::ReplaySemantics::Reissued)
            {
                continue;
            }
            required.push(name.as_str());
        }
        if self.defer_missing_capability_providers_to_dispatch {
            Ok(())
        } else {
            contracts.preflight(required).map_err(VmError::runtime)
        }
    }

    pub fn run(&mut self) -> Result<NanValue, VmError> {
        self.preflight_capability_providers()?;
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
        self.preflight_capability_providers()?;
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
        self.preflight_capability_providers()?;
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
        let lane_mark = self.arena.lane_mark();
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
            lane_base: lane_mark,
            lane_mark,
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
