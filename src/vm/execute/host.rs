use super::VM;
use crate::nan_value::NanValue;
use crate::value::Value;
use crate::vm::builtin::VmBuiltin;
use crate::vm::runtime::VmExecutionMode;
use crate::vm::types::VmError;

impl VM {
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
            Some(Err(crate::value::RuntimeError::Error(msg))) => Err(VmError::Runtime(msg)),
            Some(Err(e)) => Err(VmError::Runtime(format!("{:?}", e))),
            None => Err(VmError::Runtime(format!(
                "unknown HttpServer builtin: {}",
                builtin.name()
            ))),
        }
    }
}
