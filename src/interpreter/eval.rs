use super::lowered::{
    self, ExprId, LoweredExpr, LoweredFunctionBody, LoweredMatchArm, LoweredStmt, LoweredStrPart,
};
use super::*;

type SharedExprs = Rc<[ExprId]>;
type SharedStrParts = Rc<[LoweredStrPart]>;
type SharedMapEntries = Rc<[(ExprId, ExprId)]>;
type SharedRecordFields = Rc<[(String, ExprId)]>;
type SharedMatchArms = Rc<[LoweredMatchArm]>;

#[derive(Debug)]
enum EvalState {
    Expr {
        lowered: Rc<LoweredFunctionBody>,
        expr: ExprId,
    },
    Body {
        lowered: Rc<LoweredFunctionBody>,
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
        lowered: Rc<LoweredFunctionBody>,
        args: SharedExprs,
        idx: usize,
        fn_val: Option<Value>,
        arg_vals: Vec<Value>,
    },
    BinOpLeft {
        lowered: Rc<LoweredFunctionBody>,
        op: BinOp,
        right: ExprId,
    },
    BinOpRight {
        op: BinOp,
        left: Value,
    },
    Match {
        lowered: Rc<LoweredFunctionBody>,
        arms: SharedMatchArms,
        line: usize,
    },
    Constructor(String),
    ErrorProp,
    InterpolatedStr {
        lowered: Rc<LoweredFunctionBody>,
        parts: SharedStrParts,
        idx: usize,
        result: String,
    },
    List {
        lowered: Rc<LoweredFunctionBody>,
        items: SharedExprs,
        idx: usize,
        values: Vec<Value>,
    },
    Tuple {
        lowered: Rc<LoweredFunctionBody>,
        items: SharedExprs,
        idx: usize,
        values: Vec<Value>,
    },
    MapKey {
        lowered: Rc<LoweredFunctionBody>,
        entries: SharedMapEntries,
        idx: usize,
        map: HashMap<Value, Value>,
    },
    MapValue {
        lowered: Rc<LoweredFunctionBody>,
        entries: SharedMapEntries,
        idx: usize,
        map: HashMap<Value, Value>,
        key: Value,
    },
    RecordCreate(RecordCreateProgress),
    RecordUpdateBase {
        lowered: Rc<LoweredFunctionBody>,
        type_name: String,
        updates: SharedRecordFields,
    },
    RecordUpdateField(RecordUpdateProgress),
    TailCallArgs {
        lowered: Rc<LoweredFunctionBody>,
        target: String,
        args: SharedExprs,
        idx: usize,
        values: Vec<Value>,
    },
    BodyBinding {
        name: String,
        next_idx: usize,
        lowered: Rc<LoweredFunctionBody>,
        local_slots: Option<Rc<HashMap<String, u16>>>,
    },
    BodyExpr {
        next_idx: usize,
        lowered: Rc<LoweredFunctionBody>,
        local_slots: Option<Rc<HashMap<String, u16>>>,
    },
    MatchScope,
    FunctionReturn(FunctionFrame),
}

#[derive(Debug, Clone)]
struct ActiveFunction {
    function: Rc<crate::value::FunctionValue>,
}

#[derive(Debug, Clone)]
struct FunctionFrame {
    active: ActiveFunction,
    prev_local_slots: Option<Rc<HashMap<String, u16>>>,
    saved_frames: Vec<EnvFrame>,
    prev_global: Option<EnvFrame>,
    memo_key: Option<(u64, Vec<Value>)>,
}

#[derive(Debug, Clone)]
struct RecordCreateProgress {
    lowered: Rc<LoweredFunctionBody>,
    type_name: String,
    fields: SharedRecordFields,
    idx: usize,
    seen: HashSet<String>,
    values: Vec<(String, Value)>,
}

#[derive(Debug, Clone)]
struct RecordUpdateProgress {
    lowered: Rc<LoweredFunctionBody>,
    type_name: String,
    base_type: String,
    base_fields: Vec<(String, Value)>,
    updates: SharedRecordFields,
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
    fn empty_slots(local_count: u16) -> Vec<Value> {
        vec![Value::Unit; local_count as usize]
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        let (lowered, root) = lowered::lower_expr_root(expr);
        self.eval_loop(
            EvalState::Expr {
                lowered,
                expr: root,
            },
            Vec::new(),
        )
    }

    fn eval_loop(
        &mut self,
        initial: EvalState,
        mut conts: Vec<EvalCont>,
    ) -> Result<Value, RuntimeError> {
        let mut state = initial;

        loop {
            state = match state {
                EvalState::Expr { lowered, expr } => self.step_expr(lowered, expr, &mut conts),
                EvalState::Body {
                    lowered,
                    idx,
                    local_slots,
                    last,
                } => self.step_body(lowered, idx, local_slots, last, &mut conts),
                EvalState::Apply(result) => {
                    let Some(cont) = conts.pop() else {
                        return result;
                    };
                    self.apply_cont(cont, result, &mut conts)
                }
            };
        }
    }

    fn step_expr(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        expr_id: ExprId,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        match lowered.expr(expr_id).clone() {
            LoweredExpr::Literal(lit) => EvalState::Apply(Ok(self.eval_literal(&lit))),
            LoweredExpr::Resolved(slot) => EvalState::Apply(self.lookup_slot(slot)),
            LoweredExpr::Ident(name) => EvalState::Apply(self.lookup(&name)),
            LoweredExpr::Attr { obj, field } => {
                if let LoweredExpr::Ident(name) = lowered.expr(obj) {
                    let value = match self.lookup_ref(name) {
                        Ok(value) => value,
                        Err(err) => return EvalState::Apply(Err(err)),
                    };
                    let result = match value {
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
                EvalState::Expr { lowered, expr: obj }
            }
            LoweredExpr::FnCall { fn_expr, args } => {
                conts.push(EvalCont::Call {
                    lowered: Rc::clone(&lowered),
                    args,
                    idx: 0,
                    fn_val: None,
                    arg_vals: Vec::new(),
                });
                EvalState::Expr {
                    lowered,
                    expr: fn_expr,
                }
            }
            LoweredExpr::BinOp { op, left, right } => {
                conts.push(EvalCont::BinOpLeft {
                    lowered: Rc::clone(&lowered),
                    op,
                    right,
                });
                EvalState::Expr {
                    lowered,
                    expr: left,
                }
            }
            LoweredExpr::Match {
                subject,
                arms,
                line,
            } => {
                conts.push(EvalCont::Match {
                    lowered: Rc::clone(&lowered),
                    arms,
                    line,
                });
                EvalState::Expr {
                    lowered,
                    expr: subject,
                }
            }
            LoweredExpr::Constructor { name, arg } => match arg {
                Some(inner) => {
                    conts.push(EvalCont::Constructor(name));
                    EvalState::Expr {
                        lowered,
                        expr: inner,
                    }
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
            LoweredExpr::ErrorProp { inner } => {
                conts.push(EvalCont::ErrorProp);
                EvalState::Expr {
                    lowered,
                    expr: inner,
                }
            }
            LoweredExpr::InterpolatedStr(parts) => {
                self.resume_interpolated_str(lowered, parts, 0, String::new(), conts)
            }
            LoweredExpr::List(items) => self.resume_list(lowered, items, 0, Vec::new(), conts),
            LoweredExpr::Tuple(items) => {
                let cap = items.len();
                self.resume_tuple(lowered, items, 0, Vec::with_capacity(cap), conts)
            }
            LoweredExpr::MapLiteral(entries) => {
                self.resume_map(lowered, entries, 0, HashMap::new(), conts)
            }
            LoweredExpr::RecordCreate { type_name, fields } => self.resume_record_create(
                RecordCreateProgress {
                    lowered,
                    type_name,
                    fields,
                    idx: 0,
                    seen: HashSet::new(),
                    values: Vec::new(),
                },
                conts,
            ),
            LoweredExpr::RecordUpdate {
                type_name,
                base,
                updates,
            } => {
                conts.push(EvalCont::RecordUpdateBase {
                    lowered: Rc::clone(&lowered),
                    type_name,
                    updates,
                });
                EvalState::Expr {
                    lowered,
                    expr: base,
                }
            }
            LoweredExpr::TailCall { target, args } => {
                self.resume_tail_call(lowered, target, args, 0, Vec::new(), conts)
            }
        }
    }

    fn step_body(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        idx: usize,
        local_slots: Option<Rc<HashMap<String, u16>>>,
        last: Value,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        let Some(stmt) = lowered.stmt(idx).cloned() else {
            return EvalState::Apply(Ok(last));
        };

        match stmt {
            LoweredStmt::Binding(name, expr) => {
                conts.push(EvalCont::BodyBinding {
                    name: name.clone(),
                    next_idx: idx + 1,
                    lowered: Rc::clone(&lowered),
                    local_slots,
                });
                EvalState::Expr { lowered, expr }
            }
            LoweredStmt::Expr(expr) => {
                conts.push(EvalCont::BodyExpr {
                    next_idx: idx + 1,
                    lowered: Rc::clone(&lowered),
                    local_slots,
                });
                EvalState::Expr { lowered, expr }
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
                        .iter()
                        .find(|(k, _)| k == &field)
                        .map(|(_, value)| Ok(value.clone()))
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
                lowered,
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
                            lowered: Rc::clone(&lowered),
                            args: Rc::clone(&args),
                            idx,
                            fn_val,
                            arg_vals,
                        });
                        return EvalState::Expr {
                            lowered,
                            expr: args[idx],
                        };
                    }

                    arg_vals.push(value);
                    idx += 1;
                    if idx < args.len() {
                        conts.push(EvalCont::Call {
                            lowered: Rc::clone(&lowered),
                            args: Rc::clone(&args),
                            idx,
                            fn_val,
                            arg_vals,
                        });
                        EvalState::Expr {
                            lowered,
                            expr: args[idx],
                        }
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
            EvalCont::BinOpLeft { lowered, op, right } => match result {
                Ok(left) => {
                    conts.push(EvalCont::BinOpRight { op, left });
                    EvalState::Expr {
                        lowered,
                        expr: right,
                    }
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::BinOpRight { op, left } => match result {
                Ok(right) => EvalState::Apply(self.eval_binop(&op, left, right)),
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::Match {
                lowered,
                arms,
                line,
            } => match result {
                Ok(subject) => self.dispatch_match(lowered, subject, arms, line, conts),
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
                lowered,
                parts,
                idx,
                result: mut text,
            } => match result {
                Ok(value) => {
                    text.push_str(&aver_repr(&value));
                    self.resume_interpolated_str(lowered, parts, idx, text, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::List {
                lowered,
                items,
                idx,
                mut values,
            } => match result {
                Ok(value) => {
                    values.push(value);
                    self.resume_list(lowered, items, idx, values, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::Tuple {
                lowered,
                items,
                idx,
                mut values,
            } => match result {
                Ok(value) => {
                    values.push(value);
                    self.resume_tuple(lowered, items, idx, values, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::MapKey {
                lowered,
                entries,
                idx,
                map,
            } => match result {
                Ok(key) => {
                    if !Self::is_hashable_map_key(&key) {
                        return EvalState::Apply(Err(RuntimeError::Error(
                            "Map literal key must be Int, Float, String, or Bool".to_string(),
                        )));
                    }
                    conts.push(EvalCont::MapValue {
                        lowered: Rc::clone(&lowered),
                        entries: Rc::clone(&entries),
                        idx,
                        map,
                        key,
                    });
                    EvalState::Expr {
                        lowered,
                        expr: entries[idx].1,
                    }
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::MapValue {
                lowered,
                entries,
                idx,
                mut map,
                key,
            } => match result {
                Ok(value) => {
                    map.insert(key, value);
                    self.resume_map(lowered, entries, idx + 1, map, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::RecordCreate(mut progress) => match result {
                Ok(value) => {
                    let field_name = progress.fields[progress.idx].0.clone();
                    if !progress.seen.insert(field_name.clone()) {
                        return EvalState::Apply(Err(RuntimeError::Error(format!(
                            "Record '{}' field '{}' provided more than once",
                            progress.type_name, field_name
                        ))));
                    }
                    progress.values.push((field_name, value));
                    progress.idx += 1;
                    self.resume_record_create(progress, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::RecordUpdateBase {
                lowered,
                type_name,
                updates,
            } => match result {
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
                                lowered,
                                type_name,
                                base_type,
                                base_fields: fields.iter().cloned().collect(),
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
                lowered,
                target,
                args,
                idx,
                mut values,
            } => match result {
                Ok(value) => {
                    values.push(value);
                    self.resume_tail_call(lowered, target, args, idx + 1, values, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::BodyBinding {
                name,
                next_idx,
                lowered,
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
                        lowered,
                        idx: next_idx,
                        local_slots,
                        last: Value::Unit,
                    }
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::BodyExpr {
                next_idx,
                lowered,
                local_slots,
            } => match result {
                Ok(value) => EvalState::Body {
                    lowered,
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
        lowered: Rc<LoweredFunctionBody>,
        subject: Value,
        arms: SharedMatchArms,
        line: usize,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        let arm_count = arms.len();
        for (arm_idx, arm) in arms.iter().enumerate() {
            if let Some(bindings) = self.match_pattern(&arm.pattern, &subject) {
                self.note_verify_match_arm(line, arm_count, arm_idx);
                if let Some(local_slots) = self.active_local_slots.clone() {
                    let all_slotted = bindings
                        .iter()
                        .all(|(name, _)| local_slots.contains_key(name));
                    if all_slotted {
                        for (name, value) in bindings {
                            if let Some(&slot) = local_slots.get(&name) {
                                self.define_slot(slot, value);
                            }
                        }
                        return EvalState::Expr {
                            lowered,
                            expr: arm.body,
                        };
                    }
                }

                if bindings.is_empty() {
                    return EvalState::Expr {
                        lowered,
                        expr: arm.body,
                    };
                }

                let rc_scope = bindings.into_iter().collect::<HashMap<_, _>>();
                self.push_env(EnvFrame::Owned(rc_scope));
                conts.push(EvalCont::MatchScope);
                return EvalState::Expr {
                    lowered,
                    expr: arm.body,
                };
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
        match fn_val {
            Value::Builtin(name) => {
                self.ensure_effects_allowed(&name, Self::builtin_effects(&name).iter().copied())?;
                Ok(CallDispatch::Immediate(self.call_builtin(&name, &args)))
            }
            Value::Fn(function) => {
                if args.len() != function.params.len() {
                    return Err(RuntimeError::Error(format!(
                        "Function '{}' expects {} arguments, got {}",
                        function.name,
                        function.params.len(),
                        args.len()
                    )));
                }
                self.ensure_effects_allowed(
                    function.name.as_str(),
                    function.effects.iter().map(String::as_str),
                )?;

                let memo_key = if function.memo_eligible {
                    let key = hash_memo_args(&args);
                    if let Some(cached) = self
                        .memo_cache
                        .entry(function.name.as_ref().clone())
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
                    name: Rc::clone(&function.name),
                    effects: Rc::clone(&function.effects),
                });

                let prev_local_slots = self.active_local_slots.take();
                let saved_frames = self.env.split_off(1);
                let prev_global = if let Some(home) = function.home_globals.as_ref() {
                    let global = self
                        .env
                        .first_mut()
                        .ok_or_else(|| RuntimeError::Error("No global scope".to_string()))?;
                    Some(std::mem::replace(global, EnvFrame::Shared(Rc::clone(home))))
                } else {
                    None
                };

                let active = ActiveFunction { function };
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
        if let Some(resolution) = &active.function.resolution {
            let local_slots = Rc::clone(&resolution.local_slots);
            let mut slots = Self::empty_slots(resolution.local_count);
            for ((param_name, _), arg_val) in active.function.params.iter().zip(args.into_iter()) {
                if let Some(&slot) = resolution.local_slots.get(param_name) {
                    slots[slot as usize] = arg_val;
                }
            }
            self.active_local_slots = Some(Rc::clone(&local_slots));
            self.push_env(EnvFrame::Slots(slots));
            EvalState::Body {
                lowered: Rc::clone(&active.function.lowered_body),
                idx: 0,
                local_slots: Some(local_slots),
                last: Value::Unit,
            }
        } else {
            let mut params_scope = HashMap::new();
            for ((param_name, _), arg_val) in active.function.params.iter().zip(args.into_iter()) {
                params_scope.insert(param_name.clone(), arg_val);
            }
            self.push_env(EnvFrame::Owned(params_scope));
            EvalState::Body {
                lowered: Rc::clone(&active.function.lowered_body),
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
                let next_active = if target == frame.active.function.name.as_str() {
                    frame.active.clone()
                } else {
                    let next_function = match self.lookup_ref(&target) {
                        Ok(value) => match value {
                            Value::Fn(function) => Rc::clone(function),
                            other => {
                                return EvalState::Apply(Err(RuntimeError::Error(format!(
                                    "TCO target '{}' is not a function: {:?}",
                                    target, other
                                ))));
                            }
                        },
                        Err(err) => return EvalState::Apply(Err(err)),
                    };

                    if let Some(call_frame) = self.call_stack.last_mut() {
                        call_frame.name = Rc::clone(&next_function.name);
                        call_frame.effects = Rc::clone(&next_function.effects);
                    }

                    ActiveFunction {
                        function: next_function,
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
                self.env.append(&mut frame.saved_frames);
                self.call_stack.pop();

                let final_result = match other {
                    Ok(value) => Ok(value),
                    Err(RuntimeError::ErrProp(err)) => Ok(Value::Err(err)),
                    Err(err) => Err(err),
                };

                if let (Some((key, memo_args)), Ok(value)) = (frame.memo_key, &final_result) {
                    let cache = self
                        .memo_cache
                        .entry(frame.active.function.name.as_ref().clone())
                        .or_default();
                    cache.insert(key, memo_args, value.clone(), MEMO_CACHE_CAP_PER_FN);
                }

                EvalState::Apply(final_result)
            }
        }
    }

    fn resume_interpolated_str(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        parts: SharedStrParts,
        mut idx: usize,
        mut result: String,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        while idx < parts.len() {
            match parts[idx].clone() {
                LoweredStrPart::Literal(text) => {
                    result.push_str(&text);
                    idx += 1;
                }
                LoweredStrPart::Parsed(expr) => {
                    conts.push(EvalCont::InterpolatedStr {
                        lowered: Rc::clone(&lowered),
                        parts: Rc::clone(&parts),
                        idx: idx + 1,
                        result,
                    });
                    return EvalState::Expr { lowered, expr };
                }
            }
        }
        EvalState::Apply(Ok(Value::Str(result)))
    }

    fn resume_list(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        items: SharedExprs,
        idx: usize,
        values: Vec<Value>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= items.len() {
            return EvalState::Apply(Ok(list_from_vec(values)));
        }

        conts.push(EvalCont::List {
            lowered: Rc::clone(&lowered),
            items: Rc::clone(&items),
            idx: idx + 1,
            values,
        });
        EvalState::Expr {
            lowered,
            expr: items[idx],
        }
    }

    fn resume_tuple(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        items: SharedExprs,
        idx: usize,
        values: Vec<Value>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= items.len() {
            return EvalState::Apply(Ok(Value::Tuple(values)));
        }

        conts.push(EvalCont::Tuple {
            lowered: Rc::clone(&lowered),
            items: Rc::clone(&items),
            idx: idx + 1,
            values,
        });
        EvalState::Expr {
            lowered,
            expr: items[idx],
        }
    }

    fn resume_map(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        entries: SharedMapEntries,
        idx: usize,
        map: HashMap<Value, Value>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= entries.len() {
            return EvalState::Apply(Ok(Value::Map(map)));
        }

        conts.push(EvalCont::MapKey {
            lowered: Rc::clone(&lowered),
            entries: Rc::clone(&entries),
            idx,
            map,
        });
        EvalState::Expr {
            lowered,
            expr: entries[idx].0,
        }
    }

    fn resume_record_create(
        &mut self,
        progress: RecordCreateProgress,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if progress.idx >= progress.fields.len() {
            return EvalState::Apply(
                self.build_record_create_value(&progress.type_name, progress.values),
            );
        }

        let lowered = Rc::clone(&progress.lowered);
        let expr = progress.fields[progress.idx].1;
        conts.push(EvalCont::RecordCreate(progress));
        EvalState::Expr { lowered, expr }
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

        let next_expr = progress.updates[progress.idx].1;
        let lowered = Rc::clone(&progress.lowered);
        conts.push(EvalCont::RecordUpdateField(progress));
        EvalState::Expr {
            lowered,
            expr: next_expr,
        }
    }

    fn resume_tail_call(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        target: String,
        args: SharedExprs,
        idx: usize,
        values: Vec<Value>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= args.len() {
            return EvalState::Apply(Err(RuntimeError::TailCall(Box::new((target, values)))));
        }

        conts.push(EvalCont::TailCallArgs {
            lowered: Rc::clone(&lowered),
            target,
            args: Rc::clone(&args),
            idx,
            values,
        });
        EvalState::Expr {
            lowered,
            expr: args[idx],
        }
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
                fields: ordered.into(),
            });
        }

        Ok(Value::Record {
            type_name: type_name.to_string(),
            fields: field_vals.into(),
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
            fields: base_fields.into(),
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
