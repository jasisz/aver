use super::VM;
use crate::nan_value::{Arena, NanValue, NanValueConvert};
use crate::value::Value;
use crate::vm::builtin::VmBuiltin;
use crate::vm::opcode::{MATCH_DISPATCH, MATCH_DISPATCH_CONST, opcode_operand_width};
use crate::vm::runtime::VmExecutionMode;
use crate::vm::types::{CodeStore, VmError};

impl VM {
    fn rebase_dispatch_table_values(
        &self,
        code: &mut CodeStore,
        arena: &mut Arena,
        source_arena: &Arena,
    ) {
        const DISPATCH_KIND_STRING: u8 = 2;

        struct Patch {
            fn_idx: usize,
            bits_pos: usize,
            value: NanValue,
        }

        let mut patches = Vec::new();

        for (fn_idx, chunk) in code.functions.iter().enumerate() {
            let bytes = &chunk.code;
            let mut ip = 0usize;
            while ip < bytes.len() {
                let op = bytes[ip];
                ip += 1;
                match op {
                    MATCH_DISPATCH => {
                        if ip + 3 > bytes.len() {
                            break;
                        }
                        let count = bytes[ip] as usize;
                        ip += 1; // count
                        ip += 2; // default offset
                        for _ in 0..count {
                            let kind = bytes[ip];
                            ip += 1;
                            let expected_pos = ip;
                            let expected_bits = u64::from_be_bytes([
                                bytes[ip],
                                bytes[ip + 1],
                                bytes[ip + 2],
                                bytes[ip + 3],
                                bytes[ip + 4],
                                bytes[ip + 5],
                                bytes[ip + 6],
                                bytes[ip + 7],
                            ]);
                            ip += 8;
                            ip += 2; // jump offset

                            let expected = NanValue::from_bits(expected_bits);
                            if kind == DISPATCH_KIND_STRING || expected.heap_index().is_some() {
                                patches.push(Patch {
                                    fn_idx,
                                    bits_pos: expected_pos,
                                    value: arena.deep_import(expected, source_arena),
                                });
                            }
                        }
                    }
                    MATCH_DISPATCH_CONST => {
                        if ip + 3 > bytes.len() {
                            break;
                        }
                        let count = bytes[ip] as usize;
                        ip += 1; // count
                        ip += 2; // default offset
                        for _ in 0..count {
                            let kind = bytes[ip];
                            ip += 1;
                            let expected_pos = ip;
                            let expected_bits = u64::from_be_bytes([
                                bytes[ip],
                                bytes[ip + 1],
                                bytes[ip + 2],
                                bytes[ip + 3],
                                bytes[ip + 4],
                                bytes[ip + 5],
                                bytes[ip + 6],
                                bytes[ip + 7],
                            ]);
                            ip += 8;
                            let result_pos = ip;
                            let result_bits = u64::from_be_bytes([
                                bytes[ip],
                                bytes[ip + 1],
                                bytes[ip + 2],
                                bytes[ip + 3],
                                bytes[ip + 4],
                                bytes[ip + 5],
                                bytes[ip + 6],
                                bytes[ip + 7],
                            ]);
                            ip += 8;

                            let expected = NanValue::from_bits(expected_bits);
                            if kind == DISPATCH_KIND_STRING || expected.heap_index().is_some() {
                                patches.push(Patch {
                                    fn_idx,
                                    bits_pos: expected_pos,
                                    value: arena.deep_import(expected, source_arena),
                                });
                            }

                            let result = NanValue::from_bits(result_bits);
                            if result.heap_index().is_some() {
                                patches.push(Patch {
                                    fn_idx,
                                    bits_pos: result_pos,
                                    value: arena.deep_import(result, source_arena),
                                });
                            }
                        }
                    }
                    _ => {
                        ip += opcode_operand_width(op, bytes, ip);
                    }
                }
            }
        }

        if patches.is_empty() {
            return;
        }

        let mut roots: Vec<NanValue> = patches.iter().map(|patch| patch.value).collect();
        arena.promote_roots_to_stable(&mut roots);

        for (patch, value) in patches.into_iter().zip(roots) {
            code.functions[patch.fn_idx].code[patch.bits_pos..patch.bits_pos + 8]
                .copy_from_slice(&value.bits().to_be_bytes());
        }
    }

    pub(super) fn invoke_callable_value(
        &mut self,
        callable: NanValue,
        args: &[NanValue],
        caller_fn_id: u32,
        caller_ip: usize,
    ) -> Result<NanValue, VmError> {
        if let Some(symbol_id) = self.decode_vm_symbol_id(callable) {
            if let Some(fn_id) = self.code.symbols.resolve_function(symbol_id) {
                return self.call_function(fn_id, args);
            }

            if let Some(builtin) = self.code.symbols.resolve_builtin(symbol_id) {
                if let Some(profile) = self.profile.as_mut() {
                    profile.record_builtin_call(builtin.name());
                }
                if builtin.is_http_server() {
                    self.runtime.ensure_builtin_effects_allowed(
                        &self.code.symbols,
                        builtin,
                        symbol_id,
                    )?;
                    return self.dispatch_http_server(builtin, args);
                }
                // Oracle v1: sync caller fn_id so the trace-collector's
                // helper-boundary filter sees the right origin for
                // calls routed through `invoke_callable_value` (e.g.
                // parallel-product branches).
                self.runtime.sync_caller_fn_id(caller_fn_id);
                // If the current verify-law case installed a stub for
                // this effect method, redirect the call to the stub
                // with prepended (BranchPath.root, counter, …).
                // Counter increments per dispatched effect; flat bodies
                // only — nested `!`/`?!` branches will thread path via
                // the lifting transform in a later integration step.
                if let Some(stub_fn_id) = self.runtime.oracle_stub_for(builtin.name()) {
                    return self.dispatch_oracle_stub(stub_fn_id, args);
                }
                return self.runtime.invoke_builtin(
                    &self.code.symbols,
                    builtin,
                    symbol_id,
                    args,
                    &mut self.arena,
                );
            }

            if let Some(wrap_kind) = self.code.symbols.resolve_wrapper(symbol_id) {
                let name = self
                    .code
                    .symbols
                    .get(symbol_id)
                    .map(|info| info.name.as_str())
                    .unwrap_or("<wrapper>");
                if args.len() != 1 {
                    return Err(VmError::runtime(format!(
                        "{} expects 1 argument, got {}",
                        name,
                        args.len()
                    )));
                }
                return match wrap_kind {
                    0 => Ok(NanValue::new_ok_value(args[0], &mut self.arena)),
                    1 => Ok(NanValue::new_err_value(args[0], &mut self.arena)),
                    2 => Ok(NanValue::new_some_value(args[0], &mut self.arena)),
                    _ => Err(VmError::runtime("invalid wrap kind")),
                };
            }

            if let Some(ctor) = self.code.symbols.resolve_variant_ctor(symbol_id) {
                let name = self
                    .code
                    .symbols
                    .get(symbol_id)
                    .map(|info| info.name.as_str())
                    .unwrap_or("<ctor>");
                if args.len() != ctor.field_count as usize {
                    return Err(VmError::runtime(format!(
                        "{} expects {} argument(s), got {}",
                        name,
                        ctor.field_count,
                        args.len()
                    )));
                }
                if ctor.field_count == 0 {
                    return Ok(NanValue::new_nullary_variant(
                        self.arena.push_nullary_variant_symbol(ctor.ctor_id),
                    ));
                }
                let idx = self
                    .arena
                    .push_variant(ctor.type_id, ctor.variant_id, args.to_vec());
                return Ok(NanValue::new_variant(idx));
            }

            if let Some(value) = self.code.symbols.resolve_constant(symbol_id) {
                let name = self
                    .code
                    .symbols
                    .get(symbol_id)
                    .map(|info| info.name.as_str())
                    .unwrap_or("<constant>");
                return Err(VmError::runtime(format!(
                    "cannot call constant {} = {}",
                    name,
                    self.value_repr(value)
                )));
            }
        }

        let target_fn_id = self.decode_vm_fn_ref(callable, caller_fn_id, caller_ip)?;
        self.call_function(target_fn_id, args)
    }

    pub(super) fn build_parallel_base_context(&self) -> (CodeStore, Vec<NanValue>, Arena) {
        let mut base_arena = self.arena.clone_static();
        let mut code = self.code.clone();
        let constant_lens: Vec<usize> = code
            .functions
            .iter()
            .map(|chunk| chunk.constants.len())
            .collect();

        let mut roots = Vec::new();
        for chunk in &mut code.functions {
            for constant in &mut chunk.constants {
                *constant = base_arena.deep_import(*constant, &self.arena);
                roots.push(*constant);
            }
        }
        let globals_start = roots.len();
        for global in &self.globals {
            roots.push(base_arena.deep_import(*global, &self.arena));
        }
        base_arena.promote_roots_to_stable(&mut roots);

        // Freeze the parallel context into a fresh static-only arena so child
        // branches never depend on any transient young suffix left behind by
        // the import pass.
        let mut frozen_arena = base_arena.clone_static();
        let mut frozen_roots = Vec::with_capacity(roots.len());
        for root in roots {
            frozen_roots.push(frozen_arena.deep_import(root, &base_arena));
        }
        frozen_arena.promote_roots_to_stable(&mut frozen_roots);
        frozen_arena.truncate_to(0);
        frozen_arena.truncate_yard_to(0);
        frozen_arena.truncate_handoff_to(0);
        debug_assert!(
            frozen_roots
                .iter()
                .all(|value| value.heap_index().is_none_or(Arena::is_stable_index)),
            "parallel base context left non-stable roots behind"
        );
        let roots = frozen_roots;
        base_arena = frozen_arena;

        self.rebase_dispatch_table_values(&mut code, &mut base_arena, &self.arena);
        base_arena.truncate_to(0);
        base_arena.truncate_yard_to(0);
        base_arena.truncate_handoff_to(0);
        debug_assert_eq!(
            base_arena.young_len(),
            0,
            "parallel base context must not retain transient young entries"
        );
        debug_assert_eq!(
            base_arena.yard_len(),
            0,
            "parallel base context must not retain transient yard entries"
        );
        debug_assert_eq!(
            base_arena.handoff_len(),
            0,
            "parallel base context must not retain transient handoff entries"
        );

        let mut offset = 0;
        for (chunk, len) in code.functions.iter_mut().zip(constant_lens) {
            chunk
                .constants
                .copy_from_slice(&roots[offset..offset + len]);
            offset += len;
        }
        let globals = roots[globals_start..].to_vec();
        (code, globals, base_arena)
    }

    pub(super) fn collect_live_vm_roots(&mut self) {
        let stack_count = self.stack.len();
        let global_count = self.globals.len();
        let constant_count: usize = self
            .code
            .functions
            .iter()
            .map(|chunk| chunk.constants.len())
            .sum();
        let mut roots = Vec::with_capacity(stack_count + global_count + constant_count);
        roots.extend(self.stack.iter().copied());
        roots.extend(self.globals.iter().copied());
        for chunk in &self.code.functions {
            roots.extend(chunk.constants.iter().copied());
        }
        self.arena.collect_stable_from_roots(&mut roots);

        self.stack.copy_from_slice(&roots[..stack_count]);
        for (dst, src) in self.globals.iter_mut().zip(
            roots[stack_count..stack_count + global_count]
                .iter()
                .copied(),
        ) {
            *dst = src;
        }
        let mut constant_offset = stack_count + global_count;
        for chunk in &mut self.code.functions {
            let len = chunk.constants.len();
            chunk
                .constants
                .copy_from_slice(&roots[constant_offset..constant_offset + len]);
            constant_offset += len;
        }
    }

    /// Oracle v1: dispatch an effect call to its verify-time stub.
    ///
    /// The stub has the Aver signature
    /// `(BranchPath, Int, orig_args...) -> T` — v0 threads the root
    /// path and a per-verify-run counter; branch-aware path extension
    /// follows once the dispatcher sees `enter_group` / `set_branch`
    /// during lifted-body evaluation.
    pub(super) fn dispatch_oracle_stub(
        &mut self,
        stub_fn_id: u32,
        args: &[NanValue],
    ) -> Result<NanValue, VmError> {
        // Oracle v1: record the effect emission into the verify-trace
        // collector before redirecting. The recorded event uses the
        // original effect method name + pre-stub args, matching what the
        // user asserts about via `.trace.contains(Effect.method(...))`.
        // Reverse-lookup once: the name drives both trace recording and
        // the snapshot-dimension branch for stub-arg construction below.
        let original_effect: Option<String> = self
            .runtime
            .oracle_stubs
            .iter()
            .find_map(|(effect, fn_id)| (*fn_id == stub_fn_id).then(|| effect.clone()));

        if self.runtime.trace_collecting
            && let Some(effect_name) = &original_effect
        {
            let arg_vals: Vec<crate::value::Value> =
                args.iter().map(|a| a.to_value(&self.arena)).collect();
            self.runtime.record_trace_event(effect_name, &arg_vals);
        }

        // Oracle v1: snapshot-dimension effects (Args.get, Env.get) are
        // capability readers — deterministic, not branch-indexed. Their
        // stub signature mirrors the runtime signature with no leading
        // (BranchPath, Int). Generative / generative+output stubs do
        // take a leading (BranchPath, Int) so per-branch oracle
        // invocations stay distinct; output effects never reach this
        // path (they have no oracle).
        use crate::types::checker::effect_classification::{EffectDimension, classify};
        let is_snapshot = original_effect
            .as_deref()
            .and_then(classify)
            .map(|c| matches!(c.dimension, EffectDimension::Snapshot))
            .unwrap_or(false);

        if is_snapshot {
            return self.call_function(stub_fn_id, args);
        }

        // Generative / generative+output path: thread (BranchPath,
        // counter) from the VM's structural-trace state. Outside any
        // `!`/`?!` group the dewey is empty (BranchPath.root) and the
        // counter is the VM-level oracle_counter; inside a group,
        // dewey is the branch-indexed path and the counter is the
        // per-branch slot from enter_group's effect_count_stack.
        let (dewey, counter) = self.runtime.take_oracle_coordinates();

        // Build BranchPath(dewey) record — matches the arena registration
        // from vm::register_service_types and the Aver-source opaque
        // builtin `{ dewey: String }`.
        let type_id = self
            .arena
            .find_type_id(crate::types::branch_path::TYPE_NAME)
            .ok_or_else(|| VmError::runtime("BranchPath type not registered"))?;
        let dewey_nv = NanValue::new_string_value(&dewey, &mut self.arena);
        let path_idx = self.arena.push_record(type_id, vec![dewey_nv]);
        let path_nv = NanValue::new_record(path_idx);

        let counter_nv = NanValue::new_int(counter as i64, &mut self.arena);

        let mut new_args = Vec::with_capacity(args.len() + 2);
        new_args.push(path_nv);
        new_args.push(counter_nv);
        new_args.extend_from_slice(args);

        self.call_function(stub_fn_id, &new_args)
    }

    /// Handle HttpServer.listen/listenWith with VM callback support.
    /// Uses unsafe self-pointer for re-entrant callback into VM.call_function.
    pub(super) fn dispatch_http_server(
        &mut self,
        builtin: VmBuiltin,
        args: &[NanValue],
    ) -> Result<NanValue, VmError> {
        use crate::services::http_server;

        let val_args: Vec<Value> = args.iter().map(|a| a.to_value(&self.arena)).collect();

        let vm_ptr = self as *mut VM;
        let invoke_handler = |handler: Value, callback_args: Vec<Value>, _entry: String| {
            let vm = unsafe { &mut *vm_ptr };

            let handler_fn_id = match &handler {
                // A handler is a function symbol id; it must fit `u32`. A
                // ℤ-overflow value can never be a valid handle → error.
                Value::Int(id) => match id.to_u32() {
                    Some(sym) => vm.code.symbols.resolve_function(sym),
                    None => {
                        return Err(crate::value::RuntimeError::Error(
                            "HttpServer: handler is not a valid VM function".into(),
                        ));
                    }
                },
                _ => {
                    return Err(crate::value::RuntimeError::Error(
                        "HttpServer: handler is not a valid VM function".into(),
                    ));
                }
            }
            .ok_or_else(|| {
                crate::value::RuntimeError::Error(
                    "HttpServer: handler is not a valid VM function".into(),
                )
            })?;

            let nv_args: Vec<NanValue> = callback_args
                .iter()
                .map(|v| NanValue::from_value(v, &mut vm.arena))
                .collect();

            let handler_effects = vm.code.get(handler_fn_id).effects.clone();
            let previous_effects = vm.runtime.swap_allowed_effects(handler_effects);
            let result_nv = match vm.call_function(handler_fn_id, &nv_args) {
                Ok(result) => {
                    vm.runtime.set_allowed_effects(previous_effects);
                    result
                }
                Err(e) => {
                    vm.runtime.set_allowed_effects(previous_effects);
                    return Err(crate::value::RuntimeError::Error(format!("{}", e)));
                }
            };

            let result = result_nv.to_value(&vm.arena);
            vm.collect_live_vm_roots();
            Ok(result)
        };

        let skip = self.runtime.execution_mode() == VmExecutionMode::Record;
        match http_server::call_with_runtime(builtin.name(), &val_args, invoke_handler, skip) {
            Some(Ok(val)) => Ok(NanValue::from_value(&val, &mut self.arena)),
            Some(Err(
                crate::value::RuntimeError::Error(msg)
                | crate::value::RuntimeError::ErrorAt { msg, .. },
            )) => Err(VmError::runtime(msg)),
            Some(Err(e)) => Err(VmError::runtime(format!("{:?}", e))),
            None => Err(VmError::runtime(format!(
                "unknown HttpServer builtin: {}",
                builtin.name()
            ))),
        }
    }
}
