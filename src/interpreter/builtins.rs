use super::*;

impl Interpreter {
    #[allow(dead_code)]
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
        match self.execution_mode() {
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
                self.replay_state
                    .record_effect(effect_type, args_json, outcome, 0); // TODO: pass real source line
                // Autosave snapshots on every recorded effect so long-running
                // processes (like HttpServer) still persist replay data.
                self.persist_recording_snapshot(RecordedOutcome::Value(JsonValue::Null))?;
                result
            }
            ExecutionMode::Replay => {
                let record = self
                    .replay_state
                    .replay_effect(effect_type, Some(values_to_json_lossy(args)))
                    .map_err(|err| match err {
                        crate::replay::ReplayFailure::Exhausted {
                            effect_type,
                            position,
                        } => RuntimeError::ReplayExhausted {
                            effect_type,
                            position,
                        },
                        crate::replay::ReplayFailure::Mismatch { seq, expected, got } => {
                            RuntimeError::ReplayMismatch { seq, expected, got }
                        }
                        crate::replay::ReplayFailure::ArgsMismatch {
                            seq,
                            effect_type,
                            expected,
                            got,
                        } => RuntimeError::ReplayArgsMismatch {
                            seq,
                            effect_type,
                            expected,
                            got,
                        },
                        crate::replay::ReplayFailure::Unconsumed { remaining } => {
                            RuntimeError::ReplayUnconsumed { remaining }
                        }
                    })?;
                match record {
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
        // Runtime policy check for Http/Disk/Env calls
        if matches!(Self::builtin_namespace(name), Some("Http" | "Disk" | "Env")) {
            self.check_runtime_policy(name, args)?;
        }
        match name {
            "__ctor:Result.Ok" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Result.Ok() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                Ok(Value::Ok(Box::new(args[0].clone())))
            }
            "__ctor:Result.Err" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Result.Err() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                Ok(Value::Err(Box::new(args[0].clone())))
            }
            "__ctor:Option.Some" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Option.Some() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                Ok(Value::Some(Box::new(args[0].clone())))
            }
            name if name.starts_with("__ctor:") => {
                // Format: __ctor:TypeName:VariantName
                let parts: Vec<&str> = name.splitn(3, ':').collect();
                let type_name = parts.get(1).copied().unwrap_or("").to_string();
                let variant = parts.get(2).copied().unwrap_or("").to_string();
                Ok(Value::Variant {
                    type_name,
                    variant,
                    fields: args.to_vec().into(),
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
                let skip_server = matches!(self.execution_mode(), ExecutionMode::Record);
                match Self::builtin_namespace(name) {
                    Some("HttpServer") => http_server::call_with_runtime(
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
                        skip_server,
                    )
                    .unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Args") => args::call(name, args, &self.cli_args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Console") => console::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Http") => http::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Disk") => disk::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Env") => env::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Random") => random::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Tcp") => tcp::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    #[cfg(feature = "terminal")]
                    Some("Terminal") => terminal::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Time") => time::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Bool") => bool::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Int") => int::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Float") => float::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("String") => string::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("List") => list::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Map") => map::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Vector") => vector::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Char") => char::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Byte") => byte::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Result") => result::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    Some("Option") => option::call(name, args).unwrap_or_else(|| {
                        Err(RuntimeError::Error(format!(
                            "Unknown builtin function: '{}'",
                            name
                        )))
                    }),
                    _ => Err(RuntimeError::Error(format!(
                        "Unknown builtin function: '{}'",
                        name
                    ))),
                }
            }
        }
    }

    // ─── NanValue-native builtin dispatch ───────────────────────────────────

    /// NanValue-native builtin call — avoids Value↔NanValue conversion.
    pub(super) fn call_builtin_nv(
        &mut self,
        name: &str,
        nv_args: &[NanValue],
    ) -> Result<NanValue, RuntimeError> {
        let is_effectful = !Self::builtin_effects(name).is_empty();
        if is_effectful {
            return self.execute_effect_nv(name, nv_args);
        }
        self.dispatch_builtin_nv(name, nv_args)
    }

    fn execute_effect_nv(
        &mut self,
        effect_type: &str,
        nv_args: &[NanValue],
    ) -> Result<NanValue, RuntimeError> {
        match self.execution_mode() {
            ExecutionMode::Normal => self.dispatch_builtin_nv(effect_type, nv_args),
            ExecutionMode::Record | ExecutionMode::Replay => {
                // For record/replay, fall back to Value-based path (JSON serialization needs Value)
                let args: Vec<Value> = nv_args.iter().map(|nv| nv.to_value(&self.arena)).collect();
                let result = self.execute_effect(effect_type, &args)?;
                Ok(NanValue::from_value(&result, &mut self.arena))
            }
        }
    }

    fn dispatch_builtin_nv(
        &mut self,
        name: &str,
        args: &[NanValue],
    ) -> Result<NanValue, RuntimeError> {
        // Runtime policy check — needs Value for Http/Disk/Env
        if matches!(Self::builtin_namespace(name), Some("Http" | "Disk" | "Env")) {
            let old_args: Vec<Value> = args.iter().map(|nv| nv.to_value(&self.arena)).collect();
            self.check_runtime_policy(name, &old_args)?;
        }

        match name {
            "__ctor:Result.Ok" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Result.Ok() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                Ok(NanValue::new_ok_value(args[0], &mut self.arena))
            }
            "__ctor:Result.Err" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Result.Err() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                Ok(NanValue::new_err_value(args[0], &mut self.arena))
            }
            "__ctor:Option.Some" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Option.Some() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                Ok(NanValue::new_some_value(args[0], &mut self.arena))
            }
            name if name.starts_with("__ctor:") => {
                // Variant constructors — need type registry, fall back to bridge
                let old_args: Vec<Value> = args.iter().map(|nv| nv.to_value(&self.arena)).collect();
                let result = self.dispatch_builtin(name, &old_args)?;
                Ok(NanValue::from_value(&result, &mut self.arena))
            }

            _ => {
                let err = || -> Result<NanValue, RuntimeError> {
                    Err(RuntimeError::Error(format!(
                        "Unknown builtin function: '{}'",
                        name
                    )))
                };
                let skip_server = matches!(self.execution_mode(), ExecutionMode::Record);
                match Self::builtin_namespace(name) {
                    Some("HttpServer") => {
                        // HttpServer needs callback support — bridge through Value
                        let old_args: Vec<Value> =
                            args.iter().map(|nv| nv.to_value(&self.arena)).collect();
                        let result = http_server::call_with_runtime(
                            name,
                            &old_args,
                            |handler, callback_args, callback_entry| {
                                let callback_effects = Self::callable_declared_effects(&handler);
                                self.call_value_with_effects_pub(
                                    handler,
                                    callback_args,
                                    &callback_entry,
                                    callback_effects,
                                )
                            },
                            skip_server,
                        )
                        .unwrap_or_else(|| {
                            Err(RuntimeError::Error(format!(
                                "Unknown builtin function: '{}'",
                                name
                            )))
                        })?;
                        Ok(NanValue::from_value(&result, &mut self.arena))
                    }
                    Some("Args") => args::call_nv(name, args, &self.cli_args, &mut self.arena)
                        .unwrap_or_else(err),
                    Some("Console") => {
                        console::call_nv(name, args, &mut self.arena).unwrap_or_else(err)
                    }
                    Some("Http") => http::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Disk") => disk::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Env") => env::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Random") => {
                        random::call_nv(name, args, &mut self.arena).unwrap_or_else(err)
                    }
                    Some("Tcp") => tcp::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    #[cfg(feature = "terminal")]
                    Some("Terminal") => {
                        terminal::call_nv(name, args, &mut self.arena).unwrap_or_else(err)
                    }
                    Some("Time") => time::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Bool") => bool::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Int") => int::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Float") => {
                        float::call_nv(name, args, &mut self.arena).unwrap_or_else(err)
                    }
                    Some("String") => {
                        string::call_nv(name, args, &mut self.arena).unwrap_or_else(err)
                    }
                    Some("List") => list::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Map") => map::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Vector") => {
                        vector::call_nv(name, args, &mut self.arena).unwrap_or_else(err)
                    }
                    Some("Char") => char::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Byte") => byte::call_nv(name, args, &mut self.arena).unwrap_or_else(err),
                    Some("Result") => {
                        result::call_nv(name, args, &mut self.arena).unwrap_or_else(err)
                    }
                    Some("Option") => {
                        option::call_nv(name, args, &mut self.arena).unwrap_or_else(err)
                    }
                    _ => err(),
                }
            }
        }
    }
}
