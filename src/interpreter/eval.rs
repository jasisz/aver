use super::*;

#[derive(Debug)]
enum EvalState {
    Expr(Expr),
    Body {
        body: Rc<FnBody>,
        idx: usize,
        local_slots: Option<Rc<HashMap<String, u16>>>,
        last: Value,
    },
    Apply(Result<Value, RuntimeError>),
}

#[derive(Debug, Clone)]
enum EvalCont {
    Attr(String),
    Call {
        args: Vec<Expr>,
        idx: usize,
        fn_val: Option<Value>,
        arg_vals: Vec<Value>,
    },
    BinOpLeft {
        op: BinOp,
        right: Expr,
    },
    BinOpRight {
        op: BinOp,
        left: Value,
    },
    Match {
        arms: Vec<MatchArm>,
        line: usize,
    },
    Constructor(String),
    ErrorProp,
    InterpolatedStr {
        parts: Vec<StrPart>,
        idx: usize,
        result: String,
    },
    List {
        items: Vec<Expr>,
        idx: usize,
        values: Vec<Value>,
    },
    Tuple {
        items: Vec<Expr>,
        idx: usize,
        values: Vec<Value>,
    },
    MapKey {
        entries: Vec<(Expr, Expr)>,
        idx: usize,
        map: HashMap<Value, Value>,
    },
    MapValue {
        entries: Vec<(Expr, Expr)>,
        idx: usize,
        map: HashMap<Value, Value>,
        key: Value,
    },
    RecordCreate {
        type_name: String,
        fields: Vec<(String, Expr)>,
        idx: usize,
        seen: HashSet<String>,
        values: Vec<(String, Value)>,
    },
    RecordUpdateBase {
        type_name: String,
        updates: Vec<(String, Expr)>,
    },
    RecordUpdateField(RecordUpdateProgress),
    TailCallArgs {
        target: String,
        args: Vec<Expr>,
        idx: usize,
        values: Vec<Value>,
    },
    BodyBinding {
        name: String,
        next_idx: usize,
        body: Rc<FnBody>,
        local_slots: Option<Rc<HashMap<String, u16>>>,
    },
    BodyExpr {
        next_idx: usize,
        body: Rc<FnBody>,
        local_slots: Option<Rc<HashMap<String, u16>>>,
    },
    MatchScope,
    FunctionReturn(FunctionFrame),
}

#[derive(Debug, Clone)]
struct ActiveFunction {
    name: String,
    params: Vec<(String, String)>,
    body: Rc<FnBody>,
    resolution: Option<FnResolution>,
}

#[derive(Debug, Clone)]
struct FunctionFrame {
    active: ActiveFunction,
    prev_local_slots: Option<HashMap<String, u16>>,
    saved_frames: Vec<EnvFrame>,
    prev_global: Option<EnvFrame>,
    memo_key: Option<(u64, Vec<Value>)>,
}

#[derive(Debug, Clone)]
struct RecordUpdateProgress {
    type_name: String,
    base_type: String,
    base_fields: Vec<(String, Value)>,
    updates: Vec<(String, Expr)>,
    idx: usize,
    update_vals: Vec<(String, Value)>,
}

enum CallDispatch {
    Immediate(Result<Value, RuntimeError>),
    EnterFunction {
        frame: Box<FunctionFrame>,
        state: EvalState,
    },
}

impl Interpreter {
    fn empty_slots(local_count: u16) -> Vec<Rc<Value>> {
        let unit = Rc::new(Value::Unit);
        vec![unit; local_count as usize]
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        self.eval_loop(EvalState::Expr(expr.clone()), Vec::new())
    }

    fn eval_loop(
        &mut self,
        initial: EvalState,
        mut conts: Vec<EvalCont>,
    ) -> Result<Value, RuntimeError> {
        let mut state = initial;

        loop {
            state = match state {
                EvalState::Expr(expr) => self.step_expr(expr, &mut conts),
                EvalState::Body {
                    body,
                    idx,
                    local_slots,
                    last,
                } => self.step_body(body, idx, local_slots, last, &mut conts),
                EvalState::Apply(result) => {
                    let Some(cont) = conts.pop() else {
                        return result;
                    };
                    self.apply_cont(cont, result, &mut conts)
                }
            };
        }
    }

    fn step_expr(&mut self, expr: Expr, conts: &mut Vec<EvalCont>) -> EvalState {
        match expr {
            Expr::Literal(lit) => EvalState::Apply(Ok(self.eval_literal(&lit))),
            Expr::Resolved(slot) => EvalState::Apply(self.lookup_slot(slot)),
            Expr::Ident(name) => EvalState::Apply(self.lookup(&name)),
            Expr::Attr(obj, field) => {
                if let Expr::Ident(name) = obj.as_ref() {
                    let rc = match self.lookup_rc(name) {
                        Ok(rc) => rc,
                        Err(err) => return EvalState::Apply(Err(err)),
                    };
                    let result = match rc.as_ref() {
                        Value::Namespace { name, members } => {
                            members.get(field.as_str()).cloned().ok_or_else(|| {
                                RuntimeError::Error(format!("Unknown member '{}.{}'", name, field))
                            })
                        }
                        Value::Record { fields, .. } => fields
                            .iter()
                            .find(|(k, _)| k == &field)
                            .map(|(_, value)| Ok(value.clone()))
                            .unwrap_or_else(|| {
                                Err(RuntimeError::Error(format!("Unknown field '{}'", field)))
                            }),
                        _ => Err(RuntimeError::Error(format!(
                            "Field access '{}' is not supported on this value",
                            field
                        ))),
                    };
                    return EvalState::Apply(result);
                }

                conts.push(EvalCont::Attr(field));
                EvalState::Expr(*obj)
            }
            Expr::FnCall(fn_expr, args) => {
                conts.push(EvalCont::Call {
                    args,
                    idx: 0,
                    fn_val: None,
                    arg_vals: Vec::new(),
                });
                EvalState::Expr(*fn_expr)
            }
            Expr::BinOp(op, left, right) => {
                conts.push(EvalCont::BinOpLeft { op, right: *right });
                EvalState::Expr(*left)
            }
            Expr::Match {
                subject,
                arms,
                line,
            } => {
                conts.push(EvalCont::Match { arms, line });
                EvalState::Expr(*subject)
            }
            Expr::Constructor(name, arg) => match arg {
                Some(inner) => {
                    conts.push(EvalCont::Constructor(name));
                    EvalState::Expr(*inner)
                }
                None => EvalState::Apply(match name.as_str() {
                    "None" => Ok(Value::None),
                    "Ok" | "Err" | "Some" => Err(RuntimeError::Error(format!(
                        "Constructor '{}' expects an argument",
                        name
                    ))),
                    _ => Err(RuntimeError::Error(format!(
                        "Unknown constructor: {}",
                        name
                    ))),
                }),
            },
            Expr::ErrorProp(inner) => {
                conts.push(EvalCont::ErrorProp);
                EvalState::Expr(*inner)
            }
            Expr::InterpolatedStr(parts) => {
                self.resume_interpolated_str(parts, 0, String::new(), conts)
            }
            Expr::List(items) => self.resume_list(items, 0, Vec::new(), conts),
            Expr::Tuple(items) => {
                let cap = items.len();
                self.resume_tuple(items, 0, Vec::with_capacity(cap), conts)
            }
            Expr::MapLiteral(entries) => self.resume_map(entries, 0, HashMap::new(), conts),
            Expr::RecordCreate { type_name, fields } => {
                self.resume_record_create(type_name, fields, 0, HashSet::new(), Vec::new(), conts)
            }
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => {
                conts.push(EvalCont::RecordUpdateBase { type_name, updates });
                EvalState::Expr(*base)
            }
            Expr::TailCall(boxed) => {
                let (target, args) = *boxed;
                self.resume_tail_call(target, args, 0, Vec::new(), conts)
            }
        }
    }

    fn step_body(
        &mut self,
        body: Rc<FnBody>,
        idx: usize,
        local_slots: Option<Rc<HashMap<String, u16>>>,
        last: Value,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        let stmts = body.stmts();
        if idx >= stmts.len() {
            return EvalState::Apply(Ok(last));
        }

        match stmts[idx].clone() {
            Stmt::Binding(name, _, expr) => {
                conts.push(EvalCont::BodyBinding {
                    name,
                    next_idx: idx + 1,
                    body,
                    local_slots,
                });
                EvalState::Expr(expr)
            }
            Stmt::Expr(expr) => {
                conts.push(EvalCont::BodyExpr {
                    next_idx: idx + 1,
                    body,
                    local_slots,
                });
                EvalState::Expr(expr)
            }
        }
    }

    fn apply_cont(
        &mut self,
        cont: EvalCont,
        result: Result<Value, RuntimeError>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        match cont {
            EvalCont::Attr(field) => match result {
                Ok(obj_val) => EvalState::Apply(match obj_val {
                    Value::Record { fields, .. } => fields
                        .into_iter()
                        .find(|(k, _)| k == &field)
                        .map(|(_, value)| Ok(value))
                        .unwrap_or_else(|| {
                            Err(RuntimeError::Error(format!("Unknown field '{}'", field)))
                        }),
                    Value::Namespace { name, members } => {
                        members.get(&field).cloned().ok_or_else(|| {
                            RuntimeError::Error(format!("Unknown member '{}.{}'", name, field))
                        })
                    }
                    _ => Err(RuntimeError::Error(format!(
                        "Field access '{}' is not supported on this value",
                        field
                    ))),
                }),
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::Call {
                args,
                mut idx,
                mut fn_val,
                mut arg_vals,
            } => match result {
                Ok(value) => {
                    if fn_val.is_none() {
                        fn_val = Some(value);
                        if args.is_empty() {
                            return self.dispatch_call(
                                fn_val.expect("function value set before dispatch"),
                                arg_vals,
                                conts,
                            );
                        }
                        conts.push(EvalCont::Call {
                            args: args.clone(),
                            idx,
                            fn_val,
                            arg_vals,
                        });
                        return EvalState::Expr(args[idx].clone());
                    }

                    arg_vals.push(value);
                    idx += 1;
                    if idx < args.len() {
                        conts.push(EvalCont::Call {
                            args: args.clone(),
                            idx,
                            fn_val,
                            arg_vals,
                        });
                        EvalState::Expr(args[idx].clone())
                    } else {
                        self.dispatch_call(
                            fn_val.expect("function value present when args are done"),
                            arg_vals,
                            conts,
                        )
                    }
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::BinOpLeft { op, right } => match result {
                Ok(left) => {
                    conts.push(EvalCont::BinOpRight { op, left });
                    EvalState::Expr(right)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::BinOpRight { op, left } => match result {
                Ok(right) => EvalState::Apply(self.eval_binop(&op, left, right)),
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::Match { arms, line } => match result {
                Ok(subject) => self.dispatch_match(subject, arms, line, conts),
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::Constructor(name) => match result {
                Ok(value) => EvalState::Apply(match name.as_str() {
                    "Ok" => Ok(Value::Ok(Box::new(value))),
                    "Err" => Ok(Value::Err(Box::new(value))),
                    "Some" => Ok(Value::Some(Box::new(value))),
                    "None" => Err(RuntimeError::Error(
                        "Constructor 'None' does not take an argument".to_string(),
                    )),
                    _ => Err(RuntimeError::Error(format!(
                        "Unknown constructor: {}",
                        name
                    ))),
                }),
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::ErrorProp => match result {
                Ok(value) => EvalState::Apply(match value {
                    Value::Ok(inner) => Ok(*inner),
                    Value::Err(err) => Err(RuntimeError::ErrProp(err)),
                    _ => Err(RuntimeError::Error(
                        "Operator '?' can only be applied to Result".to_string(),
                    )),
                }),
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::InterpolatedStr {
                parts,
                idx,
                result: mut text,
            } => match result {
                Ok(value) => {
                    text.push_str(&aver_repr(&value));
                    self.resume_interpolated_str(parts, idx, text, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::List {
                items,
                idx,
                mut values,
            } => match result {
                Ok(value) => {
                    values.push(value);
                    self.resume_list(items, idx, values, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::Tuple {
                items,
                idx,
                mut values,
            } => match result {
                Ok(value) => {
                    values.push(value);
                    self.resume_tuple(items, idx, values, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::MapKey { entries, idx, map } => match result {
                Ok(key) => {
                    if !Self::is_hashable_map_key(&key) {
                        return EvalState::Apply(Err(RuntimeError::Error(
                            "Map literal key must be Int, Float, String, or Bool".to_string(),
                        )));
                    }
                    conts.push(EvalCont::MapValue {
                        entries: entries.clone(),
                        idx,
                        map,
                        key,
                    });
                    EvalState::Expr(entries[idx].1.clone())
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::MapValue {
                entries,
                idx,
                mut map,
                key,
            } => match result {
                Ok(value) => {
                    map.insert(key, value);
                    self.resume_map(entries, idx + 1, map, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::RecordCreate {
                type_name,
                fields,
                idx,
                mut seen,
                mut values,
            } => match result {
                Ok(value) => {
                    let field_name = fields[idx].0.clone();
                    if !seen.insert(field_name.clone()) {
                        return EvalState::Apply(Err(RuntimeError::Error(format!(
                            "Record '{}' field '{}' provided more than once",
                            type_name, field_name
                        ))));
                    }
                    values.push((field_name, value));
                    self.resume_record_create(type_name, fields, idx + 1, seen, values, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::RecordUpdateBase { type_name, updates } => match result {
                Ok(base_val) => match base_val {
                    Value::Record {
                        type_name: base_type,
                        fields,
                    } => {
                        if base_type != type_name {
                            return EvalState::Apply(Err(RuntimeError::Error(format!(
                                "{}.update: base is a {} record, expected {}",
                                type_name, base_type, type_name
                            ))));
                        }
                        self.resume_record_update(
                            RecordUpdateProgress {
                                type_name,
                                base_type,
                                base_fields: fields,
                                updates,
                                idx: 0,
                                update_vals: Vec::new(),
                            },
                            conts,
                        )
                    }
                    _ => EvalState::Apply(Err(RuntimeError::Error(format!(
                        "{}.update: base must be a {} record",
                        type_name, type_name
                    )))),
                },
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::RecordUpdateField(mut progress) => match result {
                Ok(value) => {
                    progress
                        .update_vals
                        .push((progress.updates[progress.idx].0.clone(), value));
                    progress.idx += 1;
                    self.resume_record_update(progress, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::TailCallArgs {
                target,
                args,
                idx,
                mut values,
            } => match result {
                Ok(value) => {
                    values.push(value);
                    self.resume_tail_call(target, args, idx + 1, values, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::BodyBinding {
                name,
                next_idx,
                body,
                local_slots,
            } => match result {
                Ok(value) => {
                    if let Some(local_slots) = local_slots.as_ref()
                        && let Some(&slot) = local_slots.get(&name)
                    {
                        self.define_slot(slot, value);
                    } else {
                        self.define(name, value);
                    }
                    EvalState::Body {
                        body,
                        idx: next_idx,
                        local_slots,
                        last: Value::Unit,
                    }
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::BodyExpr {
                next_idx,
                body,
                local_slots,
            } => match result {
                Ok(value) => EvalState::Body {
                    body,
                    idx: next_idx,
                    local_slots,
                    last: value,
                },
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::MatchScope => {
                self.pop_env();
                EvalState::Apply(result)
            }
            EvalCont::FunctionReturn(frame) => self.finish_function_call(frame, result, conts),
        }
    }

    fn dispatch_match(
        &mut self,
        subject: Value,
        arms: Vec<MatchArm>,
        line: usize,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        let arm_count = arms.len();
        for (arm_idx, arm) in arms.into_iter().enumerate() {
            if let Some(bindings) = self.match_pattern(&arm.pattern, &subject) {
                self.note_verify_match_arm(line, arm_count, arm_idx);
                if let Some(local_slots) = self.active_local_slots.clone() {
                    let all_slotted = bindings.keys().all(|name| local_slots.contains_key(name));
                    if all_slotted {
                        for (name, value) in bindings {
                            if let Some(&slot) = local_slots.get(&name) {
                                self.define_slot(slot, value);
                            }
                        }
                        return EvalState::Expr(*arm.body);
                    }
                }

                let rc_scope = bindings
                    .into_iter()
                    .map(|(k, v)| (k, Rc::new(v)))
                    .collect::<HashMap<_, _>>();
                self.push_env(EnvFrame::Owned(rc_scope));
                conts.push(EvalCont::MatchScope);
                return EvalState::Expr(*arm.body);
            }
        }

        EvalState::Apply(Err(RuntimeError::Error(format!(
            "No match found for value {}",
            aver_repr(&subject)
        ))))
    }

    fn dispatch_call(
        &mut self,
        fn_val: Value,
        args: Vec<Value>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        match self.start_call(fn_val, args) {
            Ok(CallDispatch::Immediate(result)) => EvalState::Apply(result),
            Ok(CallDispatch::EnterFunction { frame, state }) => {
                conts.push(EvalCont::FunctionReturn(*frame));
                state
            }
            Err(err) => EvalState::Apply(Err(err)),
        }
    }

    fn start_call(
        &mut self,
        fn_val: Value,
        args: Vec<Value>,
    ) -> Result<CallDispatch, RuntimeError> {
        match &fn_val {
            Value::Builtin(name) => {
                self.ensure_effects_allowed(name, Self::builtin_effects(name).iter().copied())?;
                Ok(CallDispatch::Immediate(self.call_builtin(name, &args)))
            }
            Value::Fn {
                name,
                params,
                effects,
                body,
                resolution,
                memo_eligible,
                home_globals,
                ..
            } => {
                if args.len() != params.len() {
                    return Err(RuntimeError::Error(format!(
                        "Function '{}' expects {} arguments, got {}",
                        name,
                        params.len(),
                        args.len()
                    )));
                }
                self.ensure_effects_allowed(name, effects.iter().map(String::as_str))?;

                let memo_key = if *memo_eligible {
                    let key = hash_memo_args(&args);
                    if let Some(cached) = self
                        .memo_cache
                        .entry(name.clone())
                        .or_default()
                        .get(key, &args)
                    {
                        return Ok(CallDispatch::Immediate(Ok(cached)));
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

                let active = ActiveFunction {
                    name: name.clone(),
                    params: params.clone(),
                    body: Rc::clone(body),
                    resolution: resolution.clone(),
                };
                let frame = FunctionFrame {
                    active,
                    prev_local_slots,
                    saved_frames,
                    prev_global,
                    memo_key,
                };
                let state = self.enter_function_body(&frame.active, args);
                Ok(CallDispatch::EnterFunction {
                    frame: Box::new(frame),
                    state,
                })
            }
            _ => Err(RuntimeError::Error(format!(
                "Cannot call value: {:?}",
                fn_val
            ))),
        }
    }

    fn enter_function_body(&mut self, active: &ActiveFunction, args: Vec<Value>) -> EvalState {
        if let Some(resolution) = &active.resolution {
            let local_slots = Rc::new(resolution.local_slots.clone());
            let mut slots = Self::empty_slots(resolution.local_count);
            for ((param_name, _), arg_val) in active.params.iter().zip(args.into_iter()) {
                if let Some(&slot) = resolution.local_slots.get(param_name) {
                    slots[slot as usize] = Rc::new(arg_val);
                }
            }
            self.active_local_slots = Some(resolution.local_slots.clone());
            self.push_env(EnvFrame::Slots(slots));
            EvalState::Body {
                body: Rc::clone(&active.body),
                idx: 0,
                local_slots: Some(local_slots),
                last: Value::Unit,
            }
        } else {
            let mut params_scope = HashMap::new();
            for ((param_name, _), arg_val) in active.params.iter().zip(args.into_iter()) {
                params_scope.insert(param_name.clone(), Rc::new(arg_val));
            }
            self.push_env(EnvFrame::Owned(params_scope));
            EvalState::Body {
                body: Rc::clone(&active.body),
                idx: 0,
                local_slots: None,
                last: Value::Unit,
            }
        }
    }

    fn finish_function_call(
        &mut self,
        mut frame: FunctionFrame,
        result: Result<Value, RuntimeError>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        self.pop_env();

        match result {
            Err(RuntimeError::TailCall(boxed)) => {
                let (target, args) = *boxed;
                let next_active = if target == frame.active.name {
                    frame.active.clone()
                } else {
                    match self.lookup(&target) {
                        Ok(Value::Fn {
                            name,
                            params,
                            effects,
                            body,
                            resolution,
                            home_globals: _,
                            ..
                        }) => {
                            if let Some(call_frame) = self.call_stack.last_mut() {
                                call_frame.name = name.clone();
                                call_frame.effects = effects.clone();
                            }
                            ActiveFunction {
                                name,
                                params,
                                body,
                                resolution,
                            }
                        }
                        Ok(other) => {
                            return EvalState::Apply(Err(RuntimeError::Error(format!(
                                "TCO target '{}' is not a function: {:?}",
                                target, other
                            ))));
                        }
                        Err(err) => return EvalState::Apply(Err(err)),
                    }
                };

                frame.active = next_active;
                let state = self.enter_function_body(&frame.active, args);
                conts.push(EvalCont::FunctionReturn(frame));
                state
            }
            other => {
                self.active_local_slots = frame.prev_local_slots;
                if let Some(prev) = frame.prev_global
                    && let Some(global) = self.env.first_mut()
                {
                    *global = prev;
                }
                self.env.truncate(1);
                self.env.extend(frame.saved_frames);
                self.call_stack.pop();

                let final_result = match other {
                    Ok(value) => Ok(value),
                    Err(RuntimeError::ErrProp(err)) => Ok(Value::Err(err)),
                    Err(err) => Err(err),
                };

                if let (Some((key, memo_args)), Ok(value)) = (frame.memo_key, &final_result) {
                    let cache = self.memo_cache.entry(frame.active.name).or_default();
                    cache.insert(key, memo_args, value.clone(), MEMO_CACHE_CAP_PER_FN);
                }

                EvalState::Apply(final_result)
            }
        }
    }

    fn resume_interpolated_str(
        &mut self,
        parts: Vec<StrPart>,
        mut idx: usize,
        mut result: String,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        while idx < parts.len() {
            match parts[idx].clone() {
                StrPart::Literal(text) => {
                    result.push_str(&text);
                    idx += 1;
                }
                StrPart::Parsed(expr) => {
                    conts.push(EvalCont::InterpolatedStr {
                        parts,
                        idx: idx + 1,
                        result,
                    });
                    return EvalState::Expr(*expr);
                }
            }
        }
        EvalState::Apply(Ok(Value::Str(result)))
    }

    fn resume_list(
        &mut self,
        items: Vec<Expr>,
        idx: usize,
        values: Vec<Value>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= items.len() {
            return EvalState::Apply(Ok(list_from_vec(values)));
        }

        conts.push(EvalCont::List {
            items: items.clone(),
            idx: idx + 1,
            values,
        });
        EvalState::Expr(items[idx].clone())
    }

    fn resume_tuple(
        &mut self,
        items: Vec<Expr>,
        idx: usize,
        values: Vec<Value>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= items.len() {
            return EvalState::Apply(Ok(Value::Tuple(values)));
        }

        conts.push(EvalCont::Tuple {
            items: items.clone(),
            idx: idx + 1,
            values,
        });
        EvalState::Expr(items[idx].clone())
    }

    fn resume_map(
        &mut self,
        entries: Vec<(Expr, Expr)>,
        idx: usize,
        map: HashMap<Value, Value>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= entries.len() {
            return EvalState::Apply(Ok(Value::Map(map)));
        }

        conts.push(EvalCont::MapKey {
            entries: entries.clone(),
            idx,
            map,
        });
        EvalState::Expr(entries[idx].0.clone())
    }

    fn resume_record_create(
        &mut self,
        type_name: String,
        fields: Vec<(String, Expr)>,
        idx: usize,
        seen: HashSet<String>,
        values: Vec<(String, Value)>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= fields.len() {
            return EvalState::Apply(self.build_record_create_value(&type_name, values));
        }

        conts.push(EvalCont::RecordCreate {
            type_name,
            fields: fields.clone(),
            idx,
            seen,
            values,
        });
        EvalState::Expr(fields[idx].1.clone())
    }

    fn resume_record_update(
        &mut self,
        progress: RecordUpdateProgress,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if progress.idx >= progress.updates.len() {
            return EvalState::Apply(self.build_record_update_value(
                &progress.type_name,
                progress.base_type,
                progress.base_fields,
                progress.update_vals,
            ));
        }

        let next_expr = progress.updates[progress.idx].1.clone();
        conts.push(EvalCont::RecordUpdateField(progress));
        EvalState::Expr(next_expr)
    }

    fn resume_tail_call(
        &mut self,
        target: String,
        args: Vec<Expr>,
        idx: usize,
        values: Vec<Value>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= args.len() {
            return EvalState::Apply(Err(RuntimeError::TailCall(Box::new((target, values)))));
        }

        conts.push(EvalCont::TailCallArgs {
            target,
            args: args.clone(),
            idx,
            values,
        });
        EvalState::Expr(args[idx].clone())
    }

    fn build_record_create_value(
        &self,
        type_name: &str,
        field_vals: Vec<(String, Value)>,
    ) -> Result<Value, RuntimeError> {
        if let Some(schema) = self.record_schemas.get(type_name) {
            let mut by_name = HashMap::with_capacity(field_vals.len());
            for (name, value) in field_vals {
                if by_name.insert(name.clone(), value).is_some() {
                    return Err(RuntimeError::Error(format!(
                        "Record '{}' field '{}' provided more than once",
                        type_name, name
                    )));
                }
            }

            for provided in by_name.keys() {
                if !schema.iter().any(|field| field == provided) {
                    return Err(RuntimeError::Error(format!(
                        "Record '{}' has no field '{}'",
                        type_name, provided
                    )));
                }
            }

            let mut ordered = Vec::with_capacity(schema.len());
            for required in schema {
                let value = by_name.remove(required).ok_or_else(|| {
                    RuntimeError::Error(format!(
                        "Record '{}' missing required field '{}'",
                        type_name, required
                    ))
                })?;
                ordered.push((required.clone(), value));
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

    fn build_record_update_value(
        &self,
        type_name: &str,
        base_type: String,
        mut base_fields: Vec<(String, Value)>,
        update_vals: Vec<(String, Value)>,
    ) -> Result<Value, RuntimeError> {
        if base_type != type_name {
            return Err(RuntimeError::Error(format!(
                "{}.update: base is a {} record, expected {}",
                type_name, base_type, type_name
            )));
        }

        if let Some(schema) = self.record_schemas.get(type_name) {
            for (field_name, _) in &update_vals {
                if !schema.iter().any(|field| field == field_name) {
                    return Err(RuntimeError::Error(format!(
                        "Record '{}' has no field '{}'",
                        type_name, field_name
                    )));
                }
            }
        }

        for (update_name, update_val) in update_vals {
            if let Some(field) = base_fields
                .iter_mut()
                .find(|(name, _)| name == &update_name)
            {
                field.1 = update_val;
            } else {
                return Err(RuntimeError::Error(format!(
                    "Record '{}' has no field '{}'",
                    type_name, update_name
                )));
            }
        }

        Ok(Value::Record {
            type_name: type_name.to_string(),
            fields: base_fields,
        })
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
            Literal::Unit => Value::Unit,
        }
    }

    pub(super) fn call_value(
        &mut self,
        fn_val: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match self.start_call(fn_val, args)? {
            CallDispatch::Immediate(result) => result,
            CallDispatch::EnterFunction { frame, state } => {
                self.eval_loop(state, vec![EvalCont::FunctionReturn(*frame)])
            }
        }
    }
}
