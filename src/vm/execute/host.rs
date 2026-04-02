use super::VM;
use crate::nan_value::{Arena, NanValue};
use crate::value::Value;
use crate::vm::builtin::VmBuiltin;
use crate::vm::runtime::VmExecutionMode;
use crate::vm::types::{CodeStore, VmError};

impl VM {
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
                    self.runtime
                        .ensure_builtin_effects_allowed(&self.code.symbols, builtin)?;
                    return self.dispatch_http_server(builtin, args);
                }
                return self.runtime.invoke_builtin(
                    &self.code.symbols,
                    builtin,
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
                Value::Int(id) if *id >= 0 => vm.code.symbols.resolve_function(*id as u32),
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
