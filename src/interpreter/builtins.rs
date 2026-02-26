use super::*;

impl Interpreter {
    pub(super) fn call_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let is_effectful = !Self::builtin_effects(name).is_empty();
        if is_effectful {
            return self.execute_effect(name, args);
        }
        self.dispatch_builtin(name, args)
    }

    pub(super) fn execute_effect(
        &mut self,
        effect_type: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        match self.execution_mode {
            ExecutionMode::Normal => self.dispatch_builtin(effect_type, args),
            ExecutionMode::Record => {
                let args_json = values_to_json_lossy(args);
                let result = self.dispatch_builtin(effect_type, args);
                let outcome = match &result {
                    Ok(value) => RecordedOutcome::Value(
                        value_to_json(value).map_err(RuntimeError::ReplaySerialization)?,
                    ),
                    Err(err) => RecordedOutcome::RuntimeError(err.to_string()),
                };
                let seq = self.recorded_effects.len() as u32 + 1;
                self.recorded_effects.push(EffectRecord {
                    seq,
                    effect_type: effect_type.to_string(),
                    args: args_json,
                    outcome,
                });
                result
            }
            ExecutionMode::Replay => {
                if self.replay_pos >= self.replay_effects.len() {
                    return Err(RuntimeError::ReplayExhausted {
                        effect_type: effect_type.to_string(),
                        position: self.replay_pos + 1,
                    });
                }

                let record = self.replay_effects[self.replay_pos].clone();
                if record.effect_type != effect_type {
                    return Err(RuntimeError::ReplayMismatch {
                        seq: record.seq,
                        expected: record.effect_type,
                        got: effect_type.to_string(),
                    });
                }

                if self.validate_replay_args {
                    let got_args = values_to_json_lossy(args);
                    if got_args != record.args {
                        let expected = json_to_string(&JsonValue::Array(record.args.clone()));
                        let got = json_to_string(&JsonValue::Array(got_args));
                        return Err(RuntimeError::ReplayArgsMismatch {
                            seq: record.seq,
                            effect_type: effect_type.to_string(),
                            expected,
                            got,
                        });
                    }
                }

                self.replay_pos += 1;
                match record.outcome {
                    RecordedOutcome::Value(value_json) => crate::replay::json_to_value(&value_json)
                        .map_err(RuntimeError::ReplaySerialization),
                    RecordedOutcome::RuntimeError(msg) => Err(RuntimeError::Error(msg)),
                }
            }
        }
    }

    pub(super) fn dispatch_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        match name {
            "__ctor:Result.Ok" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Result.Ok() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                return Ok(Value::Ok(Box::new(args[0].clone())));
            }
            "__ctor:Result.Err" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Result.Err() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                return Ok(Value::Err(Box::new(args[0].clone())));
            }
            "__ctor:Option.Some" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Option.Some() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                return Ok(Value::Some(Box::new(args[0].clone())));
            }
            "List.map" => {
                if args.len() != 2 {
                    return Err(RuntimeError::Error(format!(
                        "List.map() takes 2 arguments (list, fn), got {}",
                        args.len()
                    )));
                }
                let items = list_slice(&args[0]).ok_or_else(|| {
                    RuntimeError::Error("List.map() first argument must be a List".to_string())
                })?;
                let func = args[1].clone();
                let mut result = Vec::new();
                for item in items.iter().cloned() {
                    let val = self.call_fn_ref(&func, vec![item])?;
                    result.push(val);
                }
                Ok(list_from_vec(result))
            }
            "List.filter" => {
                if args.len() != 2 {
                    return Err(RuntimeError::Error(format!(
                        "List.filter() takes 2 arguments (list, fn), got {}",
                        args.len()
                    )));
                }
                let items = list_slice(&args[0]).ok_or_else(|| {
                    RuntimeError::Error("List.filter() first argument must be a List".to_string())
                })?;
                let func = args[1].clone();
                let mut result = Vec::new();
                for item in items.iter().cloned() {
                    let keep = self.call_fn_ref(&func, vec![item.clone()])?;
                    match keep {
                        Value::Bool(true) => result.push(item),
                        Value::Bool(false) => {}
                        _ => {
                            return Err(RuntimeError::Error(
                                "List.filter() predicate must return Bool".to_string(),
                            ))
                        }
                    }
                }
                Ok(list_from_vec(result))
            }
            "List.fold" => {
                if args.len() != 3 {
                    return Err(RuntimeError::Error(format!(
                        "List.fold() takes 3 arguments (list, init, fn), got {}",
                        args.len()
                    )));
                }
                let items = list_slice(&args[0]).ok_or_else(|| {
                    RuntimeError::Error("List.fold() first argument must be a List".to_string())
                })?;
                let init = args[1].clone();
                let func = args[2].clone();
                let mut acc = init;
                for item in items.iter().cloned() {
                    acc = self.call_fn_ref(&func, vec![acc, item])?;
                }
                Ok(acc)
            }
            name if name.starts_with("__ctor:") => {
                // Format: __ctor:TypeName:VariantName
                let parts: Vec<&str> = name.splitn(3, ':').collect();
                let type_name = parts.get(1).copied().unwrap_or("").to_string();
                let variant = parts.get(2).copied().unwrap_or("").to_string();
                Ok(Value::Variant {
                    type_name,
                    variant,
                    fields: args.to_vec(),
                })
            }

            "Disk.makeDir" => {
                let [path_val] = args else {
                    return Err(RuntimeError::Error(format!(
                        "Disk.makeDir() takes 1 argument (path), got {}",
                        args.len()
                    )));
                };
                let Value::Str(path) = path_val else {
                    return Err(RuntimeError::Error(
                        "Disk.makeDir: path must be a String".to_string(),
                    ));
                };
                match std::fs::create_dir_all(path) {
                    Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
                    Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
                }
            }

            _ => {
                if let Some(r) = http_server::call_with_runtime(
                    name,
                    args,
                    |handler, callback_args, callback_entry| {
                        let callback_effects = Self::callable_declared_effects(&handler);
                        self.call_value_with_effects_pub(
                            handler,
                            callback_args,
                            &callback_entry,
                            callback_effects,
                        )
                    },
                ) {
                    return r;
                }
                if let Some(r) = console::call(name, args) {
                    return r;
                }
                if let Some(r) = http::call(name, args) {
                    return r;
                }
                if let Some(r) = disk::call(name, args) {
                    return r;
                }
                if let Some(r) = tcp::call(name, args) {
                    return r;
                }
                if let Some(r) = int::call(name, args) {
                    return r;
                }
                if let Some(r) = float::call(name, args) {
                    return r;
                }
                if let Some(r) = string::call(name, args) {
                    return r;
                }
                if let Some(r) = list::call(name, args) {
                    return r;
                }
                Err(RuntimeError::Error(format!(
                    "Unknown builtin function: '{}'",
                    name
                )))
            }
        }
    }
}
