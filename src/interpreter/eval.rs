use super::*;

impl Interpreter {
    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(lit) => Ok(self.eval_literal(lit)),
            Expr::Resolved(slot) => self.lookup_slot(*slot),
            Expr::Ident(name) => self.lookup(name),
            Expr::Attr(obj, field) => {
                // Fast path: `Ident.field` avoids cloning the entire namespace/record
                if let Expr::Ident(name) = obj.as_ref() {
                    let rc = self.lookup_rc(name)?;
                    return match rc.as_ref() {
                        Value::Namespace { name, members } => {
                            members.get(field.as_str()).cloned().ok_or_else(|| {
                                RuntimeError::Error(format!("Unknown member '{}.{}'", name, field))
                            })
                        }
                        Value::Record { fields, .. } => fields
                            .iter()
                            .find(|(k, _)| k == field)
                            .map(|(_, v)| Ok(v.clone()))
                            .unwrap_or_else(|| {
                                Err(RuntimeError::Error(format!("Unknown field '{}'", field)))
                            }),
                        _ => Err(RuntimeError::Error(format!(
                            "Field access '{}' is not supported on this value",
                            field
                        ))),
                    };
                }
                // General path: computed object expression
                let obj_val = self.eval_expr(obj)?;
                match obj_val {
                    Value::Record { fields, .. } => fields
                        .into_iter()
                        .find(|(k, _)| k == field)
                        .map(|(_, v)| Ok(v))
                        .unwrap_or_else(|| {
                            Err(RuntimeError::Error(format!("Unknown field '{}'", field)))
                        }),
                    Value::Namespace { name, members } => {
                        members.get(field).cloned().ok_or_else(|| {
                            RuntimeError::Error(format!("Unknown member '{}.{}'", name, field))
                        })
                    }
                    _ => Err(RuntimeError::Error(format!(
                        "Field access '{}' is not supported on this value",
                        field
                    ))),
                }
            }
            Expr::FnCall(fn_expr, args) => {
                let fn_val = self.eval_expr(fn_expr)?;
                let mut arg_vals = Vec::new();
                for a in args {
                    arg_vals.push(self.eval_expr(a)?);
                }
                self.call_value(fn_val, arg_vals)
            }
            Expr::BinOp(op, left, right) => {
                let lv = self.eval_expr(left)?;
                let rv = self.eval_expr(right)?;
                self.eval_binop(op, lv, rv)
            }
            Expr::Match { subject, arms, .. } => {
                let sv = self.eval_expr(subject)?;
                self.eval_match(sv, arms)
            }
            Expr::Pipe(left, right) => {
                let left_val = self.eval_expr(left)?;
                let fn_val = self.eval_expr(right)?;
                self.call_value(fn_val, vec![left_val])
            }
            Expr::Constructor(name, arg) => {
                let arg_val = match arg {
                    Some(a) => self.eval_expr(a)?,
                    None => Value::Unit,
                };
                match name.as_str() {
                    "Ok" => Ok(Value::Ok(Box::new(arg_val))),
                    "Err" => Ok(Value::Err(Box::new(arg_val))),
                    "Some" => Ok(Value::Some(Box::new(arg_val))),
                    "None" => Ok(Value::None),
                    _ => Err(RuntimeError::Error(format!(
                        "Unknown constructor: {}",
                        name
                    ))),
                }
            }
            Expr::ErrorProp(inner) => {
                let val = self.eval_expr(inner)?;
                match val {
                    Value::Ok(v) => Ok(*v),
                    Value::Err(e) => Err(RuntimeError::ErrProp(e)),
                    _ => Err(RuntimeError::Error(
                        "Operator '?' can only be applied to Result".to_string(),
                    )),
                }
            }
            Expr::InterpolatedStr(parts) => {
                let mut result = String::new();
                for part in parts {
                    match part {
                        StrPart::Literal(s) => result.push_str(s),
                        StrPart::Parsed(expr) => {
                            let val = self.eval_expr(expr)?;
                            result.push_str(&aver_repr(&val));
                        }
                    }
                }
                Ok(Value::Str(result))
            }
            Expr::List(elements) => {
                let mut values = Vec::new();
                for elem in elements {
                    values.push(self.eval_expr(elem)?);
                }
                Ok(list_from_vec(values))
            }
            Expr::Tuple(items) => {
                let mut values = Vec::with_capacity(items.len());
                for item in items {
                    values.push(self.eval_expr(item)?);
                }
                Ok(Value::Tuple(values))
            }
            Expr::MapLiteral(entries) => self.eval_map_literal(entries),
            Expr::RecordCreate { type_name, fields } => self.eval_record_create(type_name, fields),
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => self.eval_record_update(type_name, base, updates),
            Expr::TailCall(boxed) => self.eval_tail_call(boxed),
        }
    }

    /// Evaluate a TailCall expression — separate fn to keep `eval_expr` frame small.
    #[inline(never)]
    pub(super) fn eval_tail_call(
        &mut self,
        boxed: &(String, Vec<Expr>),
    ) -> Result<Value, RuntimeError> {
        let (target, args) = boxed;
        let mut vals = Vec::with_capacity(args.len());
        for a in args {
            vals.push(self.eval_expr(a)?);
        }
        Err(RuntimeError::TailCall(Box::new((target.clone(), vals))))
    }

    /// Evaluate a record constructor.
    /// Kept out of `eval_expr` to avoid inflating the recursive eval stack frame.
    #[inline(never)]
    fn eval_record_create(
        &mut self,
        type_name: &str,
        fields: &[(String, Expr)],
    ) -> Result<Value, RuntimeError> {
        let mut field_vals = Vec::with_capacity(fields.len());
        let mut seen = HashSet::new();
        for (name, expr) in fields {
            if !seen.insert(name.clone()) {
                return Err(RuntimeError::Error(format!(
                    "Record '{}' field '{}' provided more than once",
                    type_name, name
                )));
            }
            let val = self.eval_expr(expr)?;
            field_vals.push((name.clone(), val));
        }

        if let Some(schema) = self.record_schemas.get(type_name) {
            let mut by_name = HashMap::with_capacity(field_vals.len());
            for (name, val) in field_vals {
                by_name.insert(name, val);
            }

            for provided in by_name.keys() {
                if !schema.iter().any(|f| f == provided) {
                    return Err(RuntimeError::Error(format!(
                        "Record '{}' has no field '{}'",
                        type_name, provided
                    )));
                }
            }

            let mut ordered = Vec::with_capacity(schema.len());
            for required in schema {
                let val = by_name.remove(required).ok_or_else(|| {
                    RuntimeError::Error(format!(
                        "Record '{}' missing required field '{}'",
                        type_name, required
                    ))
                })?;
                ordered.push((required.clone(), val));
            }

            return Ok(Value::Record {
                type_name: type_name.to_string(),
                fields: ordered,
            });
        }

        Ok(Value::Record {
            type_name: type_name.to_string(),
            fields: field_vals,
        })
    }

    /// Evaluate a record update: `Type.update(base, field = newVal, ...)`.
    #[inline(never)]
    fn eval_record_update(
        &mut self,
        type_name: &str,
        base: &Expr,
        updates: &[(String, Expr)],
    ) -> Result<Value, RuntimeError> {
        let base_val = self.eval_expr(base)?;
        let (base_type, base_fields) = match base_val {
            Value::Record {
                type_name: t,
                fields,
            } => (t, fields),
            _ => {
                return Err(RuntimeError::Error(format!(
                    "{}.update: base must be a {} record",
                    type_name, type_name
                )));
            }
        };
        if base_type != type_name {
            return Err(RuntimeError::Error(format!(
                "{}.update: base is a {} record, expected {}",
                type_name, base_type, type_name
            )));
        }

        let mut update_vals = Vec::with_capacity(updates.len());
        for (name, expr) in updates {
            let val = self.eval_expr(expr)?;
            update_vals.push((name.clone(), val));
        }

        // Validate update field names against schema
        if let Some(schema) = self.record_schemas.get(type_name) {
            for (field_name, _) in &update_vals {
                if !schema.iter().any(|f| f == field_name) {
                    return Err(RuntimeError::Error(format!(
                        "Record '{}' has no field '{}'",
                        type_name, field_name
                    )));
                }
            }
        }

        // Clone base fields, apply overrides
        let mut fields: Vec<(String, Value)> = base_fields;
        for (upd_name, upd_val) in update_vals {
            if let Some(field) = fields.iter_mut().find(|(k, _)| k == &upd_name) {
                field.1 = upd_val;
            } else {
                return Err(RuntimeError::Error(format!(
                    "Record '{}' has no field '{}'",
                    type_name, upd_name
                )));
            }
        }

        Ok(Value::Record {
            type_name: type_name.to_string(),
            fields,
        })
    }

    #[inline(never)]
    fn eval_map_literal(&mut self, entries: &[(Expr, Expr)]) -> Result<Value, RuntimeError> {
        let mut map = HashMap::with_capacity(entries.len());
        for (key_expr, value_expr) in entries {
            let key = self.eval_expr(key_expr)?;
            if !Self::is_hashable_map_key(&key) {
                return Err(RuntimeError::Error(
                    "Map literal key must be Int, Float, String, or Bool".to_string(),
                ));
            }
            let value = self.eval_expr(value_expr)?;
            map.insert(key, value);
        }
        Ok(Value::Map(map))
    }

    fn is_hashable_map_key(value: &Value) -> bool {
        matches!(
            value,
            Value::Int(_) | Value::Float(_) | Value::Str(_) | Value::Bool(_)
        )
    }

    pub(super) fn eval_literal(&self, lit: &Literal) -> Value {
        match lit {
            Literal::Int(i) => Value::Int(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
        }
    }

    /// Call a function value (owned). Delegates to `call_fn_ref` for `Value::Fn`.
    pub(super) fn call_value(
        &mut self,
        fn_val: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match &fn_val {
            Value::Builtin(name) => {
                self.ensure_effects_allowed(name, Self::builtin_effects(name).iter().copied())?;
                self.call_builtin(name, &args)
            }
            Value::Fn { .. } => self.call_fn_ref(&fn_val, args),
            _ => Err(RuntimeError::Error(format!(
                "Cannot call value: {:?}",
                fn_val
            ))),
        }
    }

    /// Call a `Value::Fn` by reference — avoids cloning name/params/effects.
    /// Used in map/filter/fold hot loops.
    /// When the body returns `RuntimeError::TailCall`, delegates to
    /// `tco_trampoline` to iterate without growing the call stack.
    pub(super) fn call_fn_ref(
        &mut self,
        fn_val: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let Value::Fn {
            name,
            params,
            effects,
            body,
            resolution,
            memo_eligible,
            home_globals,
            ..
        } = fn_val
        else {
            return Err(RuntimeError::Error(format!(
                "Cannot call value: {:?}",
                fn_val
            )));
        };

        if args.len() != params.len() {
            return Err(RuntimeError::Error(format!(
                "Function '{}' expects {} arguments, got {}",
                name,
                params.len(),
                args.len()
            )));
        }
        self.ensure_effects_allowed(name, effects.iter().map(String::as_str))?;

        // Auto-memoization: check cache before executing
        let is_memo = *memo_eligible;
        let memo_key = if is_memo {
            let key = hash_memo_args(&args);
            if let Some(cached) = self
                .memo_cache
                .entry(name.clone())
                .or_default()
                .get(key, &args)
            {
                return Ok(cached);
            }
            Some((key, args.clone()))
        } else {
            None
        };

        self.call_stack.push(CallFrame {
            name: name.clone(),
            effects: effects.clone(),
        });

        let prev_local_slots = self.active_local_slots.take();
        let saved_frames: Vec<EnvFrame> = self.env.drain(1..).collect();
        let prev_global = if let Some(home) = home_globals {
            let global = self
                .env
                .first_mut()
                .ok_or_else(|| RuntimeError::Error("No global scope".to_string()))?;
            Some(std::mem::replace(global, EnvFrame::Shared(Rc::clone(home))))
        } else {
            None
        };

        let result = if let Some(res) = resolution {
            // Resolved path: push Slots frame only.
            // lookup_rc skips Slots frames, so globals at env[0] are visible.
            let mut slots = vec![Rc::new(Value::Unit); res.local_count as usize];
            for ((param_name, _), arg_val) in params.iter().zip(args.into_iter()) {
                if let Some(&slot) = res.local_slots.get(param_name) {
                    slots[slot as usize] = Rc::new(arg_val);
                }
            }
            self.active_local_slots = Some(res.local_slots.clone());
            self.push_env(EnvFrame::Slots(slots));
            let r = match &**body {
                FnBody::Expr(e) => self.eval_expr(e),
                FnBody::Block(stmts) => self.exec_body_resolved(stmts, &res.local_slots),
            };
            self.pop_env();
            r
        } else {
            // Unresolved path (REPL): env is already isolated to [globals].
            let mut params_scope = HashMap::new();
            for ((param_name, _), arg_val) in params.iter().zip(args.into_iter()) {
                params_scope.insert(param_name.clone(), Rc::new(arg_val));
            }
            self.push_env(EnvFrame::Owned(params_scope));
            let r = match &**body {
                FnBody::Expr(e) => self.eval_expr(e),
                FnBody::Block(stmts) => self.exec_body(stmts),
            };
            self.pop_env();
            r
        };

        // If TailCall, enter trampoline — only then do we clone fn state.
        let result = match result {
            Err(RuntimeError::TailCall(boxed)) => {
                let (target, new_args) = *boxed;
                self.tco_trampoline(name, params, body, resolution, target, new_args)
            }
            other => other,
        };

        self.active_local_slots = prev_local_slots;
        if let Some(prev) = prev_global {
            if let Some(global) = self.env.first_mut() {
                *global = prev;
            }
        }
        self.env.truncate(1);
        self.env.extend(saved_frames);

        self.call_stack.pop();
        let final_result = match result {
            Ok(v) => Ok(v),
            Err(RuntimeError::ErrProp(e)) => Ok(Value::Err(e)),
            Err(e) => Err(e),
        };

        // Auto-memoization: store result in cache
        if let (Some((key, memo_args)), Ok(ref val)) = (memo_key, &final_result) {
            let fn_name = name.clone();
            let cache = self.memo_cache.entry(fn_name).or_default();
            cache.insert(key, memo_args, val.clone(), MEMO_CACHE_CAP_PER_FN);
        }

        final_result
    }

    /// Trampoline loop for tail-call optimization.
    /// Called only when `call_fn_ref` encounters a `TailCall` — keeps the call
    /// stack flat by re-binding args in a loop instead of recursing.
    #[inline(never)]
    pub(super) fn tco_trampoline(
        &mut self,
        orig_name: &str,
        orig_params: &[(String, String)],
        orig_body: &Rc<FnBody>,
        orig_resolution: &Option<FnResolution>,
        first_target: String,
        first_args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut cur_name = orig_name.to_string();
        let mut cur_params: Vec<(String, String)> = orig_params.to_vec();
        let mut cur_body = Rc::clone(orig_body);
        let mut cur_resolution = orig_resolution.clone();

        // Handle the initial TailCall
        let mut target = first_target;
        let mut new_args = first_args;

        loop {
            // Switch to target if different from current
            if target != cur_name {
                let target_fn = self.lookup(&target)?;
                match target_fn {
                    Value::Fn {
                        name: tn,
                        params: tp,
                        effects: te,
                        body: tb,
                        resolution: tr,
                        ..
                    } => {
                        cur_name = tn;
                        cur_params = tp;
                        cur_body = tb;
                        cur_resolution = tr;
                        if let Some(frame) = self.call_stack.last_mut() {
                            frame.name = cur_name.clone();
                            frame.effects = te;
                        }
                    }
                    _ => {
                        return Err(RuntimeError::Error(format!(
                            "TCO target '{}' is not a function",
                            target
                        )));
                    }
                }
            }

            // Execute body with new args
            let exec_result = if let Some(ref res) = cur_resolution {
                let mut slots = vec![Rc::new(Value::Unit); res.local_count as usize];
                for ((param_name, _), arg_val) in cur_params.iter().zip(new_args.into_iter()) {
                    if let Some(&slot) = res.local_slots.get(param_name) {
                        slots[slot as usize] = Rc::new(arg_val);
                    }
                }
                self.active_local_slots = Some(res.local_slots.clone());
                self.push_env(EnvFrame::Slots(slots));
                let r = match &*cur_body {
                    FnBody::Expr(e) => self.eval_expr(e),
                    FnBody::Block(stmts) => self.exec_body_resolved(stmts, &res.local_slots),
                };
                self.pop_env();
                r
            } else {
                let mut params_scope = HashMap::new();
                for ((param_name, _), arg_val) in cur_params.iter().zip(new_args.into_iter()) {
                    params_scope.insert(param_name.clone(), Rc::new(arg_val));
                }
                let saved_frames: Vec<EnvFrame> = self.env.drain(1..).collect();
                self.push_env(EnvFrame::Owned(params_scope));
                let r = match &*cur_body {
                    FnBody::Expr(e) => self.eval_expr(e),
                    FnBody::Block(stmts) => self.exec_body(stmts),
                };
                self.pop_env();
                self.env.extend(saved_frames);
                r
            };

            match exec_result {
                Err(RuntimeError::TailCall(boxed)) => {
                    let (next_target, next_args) = *boxed;
                    target = next_target;
                    new_args = next_args;
                    continue;
                }
                other => return other,
            }
        }
    }
}
