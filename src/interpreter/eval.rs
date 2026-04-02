use super::lowered::{
    self, ExprId, LoweredDirectCallTarget, LoweredExpr, LoweredForwardArg, LoweredFunctionBody,
    LoweredLeafOp, LoweredMatchArm, LoweredStmt, LoweredStrPart, LoweredTailCallTarget,
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
        last: NanValue,
    },
    Apply(Result<NanValue, RuntimeError>),
}

impl EvalState {
    /// Decorate an error in `Apply(Err(...))` with a source line.
    fn map_err_line(self, line: usize) -> Self {
        match self {
            EvalState::Apply(Err(err)) => EvalState::Apply(Err(err.at_line(line))),
            other => other,
        }
    }
}

#[derive(Debug, Clone)]
enum EvalCont {
    Attr(String, usize),
    Call {
        lowered: Rc<LoweredFunctionBody>,
        args: SharedExprs,
        idx: usize,
        fn_val: Option<NanValue>,
        arg_vals: Vec<NanValue>,
        call_line: usize,
    },
    BinOpLeft {
        lowered: Rc<LoweredFunctionBody>,
        op: BinOp,
        right: ExprId,
        line: usize,
    },
    BinOpRight {
        op: BinOp,
        left: NanValue,
        line: usize,
    },
    Match {
        lowered: Rc<LoweredFunctionBody>,
        arms: SharedMatchArms,
        line: usize,
    },
    DirectCall {
        lowered: Rc<LoweredFunctionBody>,
        target: LoweredDirectCallTarget,
        idx: usize,
        args: SharedExprs,
        arg_vals: Vec<NanValue>,
        call_line: usize,
    },
    Leaf {
        lowered: Rc<LoweredFunctionBody>,
        leaf: LoweredLeafOp,
        idx: usize,
        values: Vec<NanValue>,
    },
    Constructor(String),
    ErrorProp(usize),
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
        values: Vec<NanValue>,
    },
    Tuple {
        lowered: Rc<LoweredFunctionBody>,
        items: SharedExprs,
        idx: usize,
        values: Vec<NanValue>,
    },
    IndependentProduct {
        lowered: Rc<LoweredFunctionBody>,
        items: SharedExprs,
        idx: usize,
        values: Vec<NanValue>,
        unwrap: bool,
    },
    MapKey {
        lowered: Rc<LoweredFunctionBody>,
        entries: SharedMapEntries,
        idx: usize,
        map: crate::nan_value::PersistentMap,
    },
    MapValue {
        lowered: Rc<LoweredFunctionBody>,
        entries: SharedMapEntries,
        idx: usize,
        map: crate::nan_value::PersistentMap,
        key: NanValue,
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
        target: LoweredTailCallTarget,
        args: SharedExprs,
        idx: usize,
        values: Vec<NanValue>,
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
    /// The env_base of the caller — restored on return so the caller's
    /// frames become visible to lookup_ref again.
    saved_base: usize,
    prev_global: Option<EnvFrame>,
    memo_key: Option<(u64, Vec<NanValue>)>,
}

#[derive(Debug, Clone)]
struct RecordCreateProgress {
    lowered: Rc<LoweredFunctionBody>,
    type_name: String,
    fields: SharedRecordFields,
    idx: usize,
    seen: HashSet<String>,
    values: Vec<(String, NanValue)>,
}

#[derive(Debug, Clone)]
struct RecordUpdateProgress {
    lowered: Rc<LoweredFunctionBody>,
    type_name: String,
    base_type_id: u32,
    base_fields: Vec<NanValue>,
    base_field_names: Vec<String>,
    updates: SharedRecordFields,
    idx: usize,
    update_vals: Vec<(String, NanValue)>,
}

enum CallDispatch {
    Immediate(Result<NanValue, RuntimeError>),
    EnterFunction {
        frame: Box<FunctionFrame>,
        state: EvalState,
    },
}

impl Interpreter {
    fn empty_slots_nv(local_count: u16) -> Vec<NanValue> {
        vec![NanValue::UNIT; local_count as usize]
    }

    /// Public eval_expr — returns Value for external callers.
    pub fn eval_expr(&mut self, expr: &Spanned<Expr>) -> Result<Value, RuntimeError> {
        let nv = self.eval_expr_nv(expr)?;
        Ok(nv.to_value(&self.arena))
    }

    /// Internal eval_expr — returns NanValue natively.
    pub(super) fn eval_expr_nv(&mut self, expr: &Spanned<Expr>) -> Result<NanValue, RuntimeError> {
        let ctx = super::ir_bridge::InterpreterLowerCtx::new(self);
        let (lowered, root) = lowered::lower_expr_root(expr, &ctx);
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
    ) -> Result<NanValue, RuntimeError> {
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
        match lowered.expr(expr_id) {
            LoweredExpr::Literal(lit) => EvalState::Apply(Ok(self.eval_literal_nv(lit))),
            &LoweredExpr::Resolved(slot) => EvalState::Apply(self.lookup_slot(slot)),
            LoweredExpr::Ident(name) => EvalState::Apply(self.lookup_nv(name)),
            LoweredExpr::Attr { obj, field, line } => {
                let obj = *obj;
                let line = *line;
                if let LoweredExpr::Ident(name) = lowered.expr(obj) {
                    let nv = match self.lookup_nv(name) {
                        Ok(nv) => nv,
                        Err(err) => return EvalState::Apply(Err(err)),
                    };
                    let result = self.attr_access_nv(nv, field);
                    return EvalState::Apply(result.map_err(|e| e.at_line(line)));
                }

                let field = field.clone();
                conts.push(EvalCont::Attr(field, line));
                EvalState::Expr { lowered, expr: obj }
            }
            LoweredExpr::FnCall {
                fn_expr,
                args,
                call_line,
            } => {
                let fn_expr = *fn_expr;
                let args = Rc::clone(args);
                let call_line = *call_line;
                conts.push(EvalCont::Call {
                    lowered: Rc::clone(&lowered),
                    idx: 0,
                    fn_val: None,
                    arg_vals: Vec::with_capacity(args.len()),
                    args,
                    call_line,
                });
                EvalState::Expr {
                    lowered,
                    expr: fn_expr,
                }
            }
            LoweredExpr::DirectCall {
                target,
                args,
                call_line,
            } => self.resume_direct_call(
                Rc::clone(&lowered),
                target.clone(),
                Rc::clone(args),
                0,
                Vec::with_capacity(args.len()),
                *call_line,
                conts,
            ),
            LoweredExpr::ForwardCall {
                target,
                args,
                call_line,
            } => self.dispatch_forward_call(target.clone(), args, *call_line, conts),
            &LoweredExpr::BinOp {
                op,
                left,
                right,
                line,
            } => {
                conts.push(EvalCont::BinOpLeft {
                    lowered: Rc::clone(&lowered),
                    op,
                    right,
                    line,
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
                let subject = *subject;
                let line = *line;
                conts.push(EvalCont::Match {
                    lowered: Rc::clone(&lowered),
                    arms: Rc::clone(arms),
                    line,
                });
                EvalState::Expr {
                    lowered,
                    expr: subject,
                }
            }
            LoweredExpr::Leaf(leaf) => {
                let leaf = leaf.clone();
                self.resume_leaf(
                    Rc::clone(&lowered),
                    leaf.clone(),
                    0,
                    Vec::with_capacity(leaf.arity()),
                    conts,
                )
            }
            LoweredExpr::Constructor { name, arg } => match arg {
                Some(inner) => {
                    let inner = *inner;
                    conts.push(EvalCont::Constructor(name.clone()));
                    EvalState::Expr {
                        lowered,
                        expr: inner,
                    }
                }
                None => EvalState::Apply(self.apply_runtime_constructor_nv(name, None)),
            },
            &LoweredExpr::ErrorProp { inner, line } => {
                conts.push(EvalCont::ErrorProp(line));
                EvalState::Expr {
                    lowered,
                    expr: inner,
                }
            }
            LoweredExpr::InterpolatedStr(parts) => {
                let parts = Rc::clone(parts);
                self.resume_interpolated_str(lowered, parts, 0, String::new(), conts)
            }
            LoweredExpr::List(items) => {
                let items = Rc::clone(items);
                self.resume_list(lowered, items, 0, Vec::new(), conts)
            }
            LoweredExpr::Tuple(items) => {
                let cap = items.len();
                let items = Rc::clone(items);
                self.resume_tuple(lowered, items, 0, Vec::with_capacity(cap), conts)
            }
            LoweredExpr::IndependentProduct { items, unwrap } => {
                let cap = items.len();
                let items = Rc::clone(items);
                let unwrap = *unwrap;
                self.resume_independent_product(lowered, items, 0, Vec::with_capacity(cap), unwrap, conts)
            }
            LoweredExpr::MapLiteral(entries) => {
                let entries = Rc::clone(entries);
                self.resume_map(
                    lowered,
                    entries,
                    0,
                    crate::nan_value::PersistentMap::new(),
                    conts,
                )
            }
            LoweredExpr::RecordCreate { type_name, fields } => {
                let type_name = type_name.clone();
                let fields = Rc::clone(fields);
                self.resume_record_create(
                    RecordCreateProgress {
                        lowered,
                        type_name,
                        fields,
                        idx: 0,
                        seen: HashSet::new(),
                        values: Vec::new(),
                    },
                    conts,
                )
            }
            LoweredExpr::RecordUpdate {
                type_name,
                base,
                updates,
            } => {
                let base = *base;
                conts.push(EvalCont::RecordUpdateBase {
                    lowered: Rc::clone(&lowered),
                    type_name: type_name.clone(),
                    updates: Rc::clone(updates),
                });
                EvalState::Expr {
                    lowered,
                    expr: base,
                }
            }
            LoweredExpr::TailCall { target, args } => {
                let target = target.clone();
                let args = Rc::clone(args);
                self.resume_tail_call(lowered, target, args, 0, Vec::new(), conts)
            }
        }
    }

    fn step_body(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        idx: usize,
        local_slots: Option<Rc<HashMap<String, u16>>>,
        last: NanValue,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        let Some(stmt) = lowered.stmt(idx) else {
            return EvalState::Apply(Ok(last));
        };

        match stmt {
            LoweredStmt::Binding(name, expr) => {
                let expr = *expr;
                conts.push(EvalCont::BodyBinding {
                    name: name.clone(),
                    next_idx: idx + 1,
                    lowered: Rc::clone(&lowered),
                    local_slots,
                });
                EvalState::Expr { lowered, expr }
            }
            &LoweredStmt::Expr(expr) => {
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
        result: Result<NanValue, RuntimeError>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        match cont {
            EvalCont::Attr(field, line) => match result {
                Ok(nv) => {
                    EvalState::Apply(self.attr_access_nv(nv, &field).map_err(|e| e.at_line(line)))
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::Call {
                lowered,
                args,
                mut idx,
                mut fn_val,
                mut arg_vals,
                call_line,
            } => match result {
                Ok(value) => {
                    if fn_val.is_none() {
                        fn_val = Some(value);
                        if args.is_empty() {
                            self.last_call_line = call_line;
                            let state = self.dispatch_call(
                                fn_val.expect("function value set before dispatch"),
                                arg_vals,
                                conts,
                            );
                            return state.map_err_line(call_line);
                        }
                        conts.push(EvalCont::Call {
                            lowered: Rc::clone(&lowered),
                            args: Rc::clone(&args),
                            idx,
                            fn_val,
                            arg_vals,
                            call_line,
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
                            call_line,
                        });
                        EvalState::Expr {
                            lowered,
                            expr: args[idx],
                        }
                    } else {
                        self.last_call_line = call_line;
                        let state = self.dispatch_call(
                            fn_val.expect("function value present when args are done"),
                            arg_vals,
                            conts,
                        );
                        state.map_err_line(call_line)
                    }
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::BinOpLeft {
                lowered,
                op,
                right,
                line,
            } => match result {
                Ok(left) => {
                    conts.push(EvalCont::BinOpRight { op, left, line });
                    EvalState::Expr {
                        lowered,
                        expr: right,
                    }
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::BinOpRight { op, left, line } => match result {
                Ok(right) => EvalState::Apply(
                    self.eval_binop_nv(&op, left, right)
                        .map_err(|e| e.at_line(line)),
                ),
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
            EvalCont::DirectCall {
                lowered,
                target,
                idx,
                args,
                mut arg_vals,
                call_line,
            } => match result {
                Ok(value) => {
                    arg_vals.push(value);
                    self.resume_direct_call(
                        lowered,
                        target,
                        args,
                        idx + 1,
                        arg_vals,
                        call_line,
                        conts,
                    )
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::Leaf {
                lowered,
                leaf,
                idx,
                mut values,
            } => match result {
                Ok(value) => {
                    values.push(value);
                    self.resume_leaf(lowered, leaf, idx + 1, values, conts)
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::Constructor(name) => match result {
                Ok(inner) => {
                    EvalState::Apply(self.apply_runtime_constructor_nv(&name, Some(inner)))
                }
                Err(err) => EvalState::Apply(Err(err)),
            },
            EvalCont::ErrorProp(line) => match result {
                Ok(value) => EvalState::Apply(if value.is_ok() {
                    let inner = value.wrapper_inner(&self.arena);
                    Ok(inner)
                } else if value.is_err() {
                    let inner = value.wrapper_inner(&self.arena);
                    Err(RuntimeError::ErrProp(inner))
                } else {
                    Err(RuntimeError::Error(
                        "Operator '?' can only be applied to Result".to_string(),
                    )
                    .at_line(line))
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
                    text.push_str(&value.repr(&self.arena));
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
            EvalCont::IndependentProduct {
                lowered,
                items,
                idx,
                mut values,
                unwrap,
            } => match result {
                Ok(value) => {
                    values.push(value);
                    self.resume_independent_product(lowered, items, idx, values, unwrap, conts)
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
                    if !Self::is_hashable_map_key_nv(key) {
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
                map,
                key,
            } => match result {
                Ok(value) => {
                    let hash = key.map_key_hash(&self.arena);
                    let map = map.insert(hash, (key, value));
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
                Ok(base_nv) => {
                    if !base_nv.is_record() {
                        return EvalState::Apply(Err(RuntimeError::Error(format!(
                            "{}.update: base must be a {} record",
                            type_name, type_name
                        ))));
                    }
                    let (base_type_id, base_fields) = self.arena.get_record(base_nv.arena_index());
                    let base_type_name = self.arena.get_type_name(base_type_id).to_string();
                    if base_type_name != type_name {
                        return EvalState::Apply(Err(RuntimeError::Error(format!(
                            "{}.update: base is a {} record, expected {}",
                            type_name, base_type_name, type_name
                        ))));
                    }
                    let base_field_names: Vec<String> =
                        self.arena.get_field_names(base_type_id).to_vec();
                    let base_fields_vec: Vec<NanValue> = base_fields.to_vec();
                    self.resume_record_update(
                        RecordUpdateProgress {
                            lowered,
                            type_name,
                            base_type_id,
                            base_fields: base_fields_vec,
                            base_field_names,
                            updates,
                            idx: 0,
                            update_vals: Vec::new(),
                        },
                        conts,
                    )
                }
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
                        self.define_nv(name, value);
                    }
                    EvalState::Body {
                        lowered,
                        idx: next_idx,
                        local_slots,
                        last: NanValue::UNIT,
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

    /// Attribute access on a NanValue.
    fn attr_access_nv(&self, nv: NanValue, field: &str) -> Result<NanValue, RuntimeError> {
        if nv.is_record() {
            let (tid, fields) = self.arena.get_record(nv.arena_index());
            let field_names = self.arena.get_field_names(tid);
            if let Some(pos) = field_names.iter().position(|n| n == field) {
                return Ok(fields[pos]);
            }
            return Err(RuntimeError::Error(format!("Unknown field '{}'", field)));
        }
        if nv.is_namespace() {
            let (name, members) = self.arena.get_namespace(nv.symbol_index());
            if let Some((_, member_nv)) = members.iter().find(|(k, _)| k.as_ref() == field) {
                return Ok(*member_nv);
            }
            return Err(RuntimeError::Error(format!(
                "Unknown member '{}.{}'",
                name, field
            )));
        }
        Err(RuntimeError::Error(format!(
            "Field access '{}' is not supported on this value",
            field
        )))
    }

    fn dispatch_match(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        subject: NanValue,
        arms: SharedMatchArms,
        line: usize,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if let Some((arm_idx, bindings)) = self.try_dispatch_match_plan_nv(subject, arms.as_ref()) {
            let arm_count = arms.len();
            let arm = &arms[arm_idx];
            self.note_verify_match_arm(line, arm_count, arm_idx);
            if bindings.is_empty() {
                return EvalState::Expr {
                    lowered,
                    expr: arm.body,
                };
            }

            if let Some(local_slots) = self.active_local_slots.clone() {
                let all_slotted = bindings
                    .iter()
                    .all(|(name, _)| local_slots.contains_key(name));
                if all_slotted {
                    for (name, nv) in bindings {
                        if let Some(&slot) = local_slots.get(&name) {
                            self.define_slot(slot, nv);
                        }
                    }
                    return EvalState::Expr {
                        lowered,
                        expr: arm.body,
                    };
                }
            }

            let scope: HashMap<String, NanValue> = bindings.into_iter().collect();
            self.push_env(EnvFrame::Owned(scope));
            conts.push(EvalCont::MatchScope);
            return EvalState::Expr {
                lowered,
                expr: arm.body,
            };
        }

        let arm_count = arms.len();
        for (arm_idx, arm) in arms.iter().enumerate() {
            if let Some(bindings) = self.match_pattern_nv(&arm.pattern, subject) {
                self.note_verify_match_arm(line, arm_count, arm_idx);
                if bindings.is_empty() {
                    return EvalState::Expr {
                        lowered,
                        expr: arm.body,
                    };
                }

                if let Some(local_slots) = self.active_local_slots.clone() {
                    let all_slotted = bindings
                        .iter()
                        .all(|(name, _)| local_slots.contains_key(name));
                    if all_slotted {
                        for (name, nv) in bindings {
                            if let Some(&slot) = local_slots.get(&name) {
                                self.define_slot(slot, nv);
                            }
                        }
                        return EvalState::Expr {
                            lowered,
                            expr: arm.body,
                        };
                    }
                }

                let scope: HashMap<String, NanValue> = bindings.into_iter().collect();
                self.push_env(EnvFrame::Owned(scope));
                conts.push(EvalCont::MatchScope);
                return EvalState::Expr {
                    lowered,
                    expr: arm.body,
                };
            }
        }

        EvalState::Apply(Err(RuntimeError::Error(format!(
            "No match found for value {}",
            subject.repr(&self.arena)
        ))))
    }

    fn dispatch_call(
        &mut self,
        fn_val: NanValue,
        args: Vec<NanValue>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        match self.start_call_nv(fn_val, args) {
            Ok(CallDispatch::Immediate(result)) => EvalState::Apply(result),
            Ok(CallDispatch::EnterFunction { frame, state }) => {
                conts.push(EvalCont::FunctionReturn(*frame));
                state
            }
            Err(err) => EvalState::Apply(Err(err)),
        }
    }

    fn start_call_nv(
        &mut self,
        fn_val: NanValue,
        args: Vec<NanValue>,
    ) -> Result<CallDispatch, RuntimeError> {
        if fn_val.is_builtin() {
            let name = self.arena.get_builtin(fn_val.symbol_index()).to_string();
            self.ensure_effects_allowed(&name, Self::builtin_effects(&name).iter().copied())?;
            let result = self.call_builtin_nv(&name, &args);
            return Ok(CallDispatch::Immediate(result));
        }
        if fn_val.is_fn() {
            let function = Rc::clone(self.arena.get_fn_rc(fn_val.symbol_index()));
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
                let key = hash_memo_args_nv(&args, &self.arena);
                let fn_name: &str = function.name.as_ref();
                if let Some(cache) = self.memo_cache.get_mut(fn_name)
                    && let Some(cached_val) = cache.get_nv_as_value(key, &args, &self.arena)
                {
                    let cached_nv = NanValue::from_value(&cached_val, &mut self.arena);
                    return Ok(CallDispatch::Immediate(Ok(cached_nv)));
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
            let saved_base = self.env_base;
            self.env_base = self.env.len();
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
                saved_base,
                prev_global,
                memo_key,
            };
            let state = self.enter_function_body_nv(&frame.active, args);
            return Ok(CallDispatch::EnterFunction {
                frame: Box::new(frame),
                state,
            });
        }
        Err(RuntimeError::Error(format!(
            "Cannot call value: {}",
            fn_val.repr(&self.arena)
        )))
    }

    fn enter_function_body_nv(
        &mut self,
        active: &ActiveFunction,
        args: Vec<NanValue>,
    ) -> EvalState {
        if let Some(resolution) = &active.function.resolution {
            let local_slots = Rc::clone(&resolution.local_slots);
            let mut slots = Self::empty_slots_nv(resolution.local_count);
            for ((param_name, _), arg_nv) in active.function.params.iter().zip(args.into_iter()) {
                if let Some(&slot) = resolution.local_slots.get(param_name) {
                    slots[slot as usize] = arg_nv;
                }
            }
            self.active_local_slots = Some(Rc::clone(&local_slots));
            self.push_env(EnvFrame::Slots(slots));
            EvalState::Body {
                lowered: Rc::clone(&active.function.lowered_body),
                idx: 0,
                local_slots: Some(local_slots),
                last: NanValue::UNIT,
            }
        } else {
            let mut params_scope: HashMap<String, NanValue> = HashMap::new();
            for ((param_name, _), arg_nv) in active.function.params.iter().zip(args.into_iter()) {
                params_scope.insert(param_name.clone(), arg_nv);
            }
            self.push_env(EnvFrame::Owned(params_scope));
            EvalState::Body {
                lowered: Rc::clone(&active.function.lowered_body),
                idx: 0,
                local_slots: None,
                last: NanValue::UNIT,
            }
        }
    }

    fn finish_function_call(
        &mut self,
        mut frame: FunctionFrame,
        result: Result<NanValue, RuntimeError>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        self.pop_env();

        match result {
            Err(RuntimeError::TailCall(boxed)) => {
                let (target, nv_args) = *boxed;
                let next_active = if target == frame.active.function.name.as_str() {
                    frame.active.clone()
                } else {
                    let next_function = match self.lookup_nv(&target) {
                        Ok(nv) => {
                            if !nv.is_fn() {
                                return EvalState::Apply(Err(RuntimeError::Error(format!(
                                    "TCO target '{}' is not a function: {}",
                                    target,
                                    nv.repr(&self.arena)
                                ))));
                            }
                            Rc::clone(self.arena.get_fn_rc(nv.symbol_index()))
                        }
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
                let state = self.enter_function_body_nv(&frame.active, nv_args);
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
                self.env_base = frame.saved_base;
                self.call_stack.pop();

                let final_result = match other {
                    Ok(value) => Ok(value),
                    Err(RuntimeError::ErrProp(err_nv)) => {
                        Ok(NanValue::new_err_value(err_nv, &mut self.arena))
                    }
                    Err(err) => Err(err),
                };

                if let (Some((key, memo_args)), Ok(value)) = (&frame.memo_key, &final_result) {
                    let fn_name: &str = frame.active.function.name.as_ref();
                    let cache = if let Some(c) = self.memo_cache.get_mut(fn_name) {
                        c
                    } else {
                        self.memo_cache
                            .entry(frame.active.function.name.as_ref().clone())
                            .or_default()
                    };
                    cache.insert_nv(
                        *key,
                        memo_args.clone(),
                        *value,
                        &self.arena,
                        MEMO_CACHE_CAP_PER_FN,
                    );
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
        EvalState::Apply(Ok(NanValue::new_string_value(&result, &mut self.arena)))
    }

    #[allow(clippy::too_many_arguments)]
    fn resume_direct_call(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        target: LoweredDirectCallTarget,
        args: SharedExprs,
        idx: usize,
        arg_vals: Vec<NanValue>,
        call_line: usize,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= args.len() {
            self.last_call_line = call_line;
            return self
                .dispatch_direct_call(target, arg_vals, conts)
                .map_err_line(call_line);
        }

        conts.push(EvalCont::DirectCall {
            lowered: Rc::clone(&lowered),
            target,
            idx,
            args: Rc::clone(&args),
            arg_vals,
            call_line,
        });
        EvalState::Expr {
            lowered,
            expr: args[idx],
        }
    }

    fn resume_leaf(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        leaf: LoweredLeafOp,
        idx: usize,
        values: Vec<NanValue>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if let Some(expr) = leaf.arg_at(idx) {
            conts.push(EvalCont::Leaf {
                lowered: Rc::clone(&lowered),
                leaf,
                idx,
                values,
            });
            EvalState::Expr { lowered, expr }
        } else {
            EvalState::Apply(self.apply_leaf_op_nv(&leaf, &values))
        }
    }

    fn resume_list(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        items: SharedExprs,
        idx: usize,
        values: Vec<NanValue>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= items.len() {
            let list_idx = self.arena.push_list(values);
            return EvalState::Apply(Ok(NanValue::new_list(list_idx)));
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
        values: Vec<NanValue>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= items.len() {
            let tuple_idx = self.arena.push_tuple(values);
            return EvalState::Apply(Ok(NanValue::new_tuple(tuple_idx)));
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

    fn resume_independent_product(
        &mut self,
        lowered: Rc<LoweredFunctionBody>,
        items: SharedExprs,
        idx: usize,
        values: Vec<NanValue>,
        unwrap: bool,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        // Enter replay group when starting the independent product
        if idx == 0 {
            self.replay_state.enter_group();
        }

        if idx >= items.len() {
            // Exit replay group when all elements are evaluated
            self.replay_state.exit_group();

            if unwrap {
                // Unwrap each Result: if any is Err, propagate it.
                let mut unwrapped = Vec::with_capacity(values.len());
                for v in &values {
                    if v.is_ok() {
                        unwrapped.push(v.wrapper_inner(&self.arena));
                    } else if v.is_err() {
                        let inner = v.wrapper_inner(&self.arena);
                        return EvalState::Apply(Err(RuntimeError::ErrProp(inner)));
                    } else {
                        return EvalState::Apply(Err(RuntimeError::Error(
                            "Independent product with '?' can only contain Result values".to_string(),
                        )));
                    }
                }
                let tuple_idx = self.arena.push_tuple(unwrapped);
                return EvalState::Apply(Ok(NanValue::new_tuple(tuple_idx)));
            } else {
                let tuple_idx = self.arena.push_tuple(values);
                return EvalState::Apply(Ok(NanValue::new_tuple(tuple_idx)));
            }
        }

        conts.push(EvalCont::IndependentProduct {
            lowered: Rc::clone(&lowered),
            items: Rc::clone(&items),
            idx: idx + 1,
            values,
            unwrap,
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
        map: crate::nan_value::PersistentMap,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= entries.len() {
            let map_idx = self.arena.push_map(map);
            return EvalState::Apply(Ok(NanValue::new_map(map_idx)));
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
                self.build_record_create_nv(&progress.type_name, progress.values),
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
            return EvalState::Apply(self.build_record_update_nv(
                &progress.type_name,
                progress.base_type_id,
                progress.base_fields,
                progress.base_field_names,
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
        target: LoweredTailCallTarget,
        args: SharedExprs,
        idx: usize,
        values: Vec<NanValue>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        if idx >= args.len() {
            let resolved_target = match target {
                LoweredTailCallTarget::SelfCall => self
                    .call_stack
                    .last()
                    .map(|frame| frame.name.as_ref().clone())
                    .unwrap_or_default(),
                LoweredTailCallTarget::KnownFunction(name)
                | LoweredTailCallTarget::Unknown(name) => name,
            };
            return EvalState::Apply(Err(RuntimeError::TailCall(Box::new((
                resolved_target,
                values,
            )))));
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

    fn build_record_create_nv(
        &mut self,
        type_name: &str,
        field_vals: Vec<(String, NanValue)>,
    ) -> Result<NanValue, RuntimeError> {
        if let Some(schema) = self.record_schemas.get(type_name) {
            // When we have a schema, register the type using schema field order
            // (which is the declaration order from the record definition).
            let schema_clone = schema.clone();
            let type_id = self.arena.find_type_id(type_name).unwrap_or_else(|| {
                self.arena
                    .register_record_type(type_name, schema_clone.clone())
            });
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
                if !schema_clone.iter().any(|field| field == provided) {
                    return Err(RuntimeError::Error(format!(
                        "Record '{}' has no field '{}'",
                        type_name, provided
                    )));
                }
            }

            let mut ordered = Vec::with_capacity(schema_clone.len());
            for required in &schema_clone {
                let value = by_name.remove(required).ok_or_else(|| {
                    RuntimeError::Error(format!(
                        "Record '{}' missing required field '{}'",
                        type_name, required
                    ))
                })?;
                ordered.push(value);
            }

            let rec_idx = self.arena.push_record(type_id, ordered);
            return Ok(NanValue::new_record(rec_idx));
        }

        // No schema — just use field order as-is
        let type_id = self.arena.find_type_id(type_name).unwrap_or_else(|| {
            let field_names: Vec<String> = field_vals.iter().map(|(n, _)| n.clone()).collect();
            self.arena.register_record_type(type_name, field_names)
        });
        let nv_fields: Vec<NanValue> = field_vals.iter().map(|(_, v)| *v).collect();
        let rec_idx = self.arena.push_record(type_id, nv_fields);
        Ok(NanValue::new_record(rec_idx))
    }

    fn apply_leaf_op_nv(
        &mut self,
        leaf: &LoweredLeafOp,
        args: &[NanValue],
    ) -> Result<NanValue, RuntimeError> {
        match leaf {
            LoweredLeafOp::MapGet { .. } => {
                map::call_nv("Map.get", args, &mut self.arena).expect("Map.get leaf owned by Map")
            }
            LoweredLeafOp::MapSet { .. } => {
                map::call_nv("Map.set", args, &mut self.arena).expect("Map.set leaf owned by Map")
            }
            LoweredLeafOp::VectorNew { .. } => vector::call_nv("Vector.new", args, &mut self.arena)
                .expect("Vector.new leaf owned by Vector"),
            LoweredLeafOp::VectorGetOrDefaultLiteral {
                default_literal, ..
            } => {
                let opt = vector::call_nv("Vector.get", args, &mut self.arena)
                    .expect("Vector.get leaf owned by Vector")?;
                if opt.is_some() {
                    Ok(opt.wrapper_inner(&self.arena))
                } else if opt.is_none() {
                    Ok(self.eval_literal_nv(default_literal))
                } else {
                    Err(RuntimeError::Error(
                        "Vector.get leaf expected Option result".to_string(),
                    ))
                }
            }
            LoweredLeafOp::IntModOrDefaultLiteral {
                default_literal, ..
            } => {
                let a = args[0].as_int(&self.arena);
                let b = args[1].as_int(&self.arena);
                if b == 0 {
                    Ok(self.eval_literal_nv(default_literal))
                } else {
                    Ok(NanValue::new_int(a.rem_euclid(b), &mut self.arena))
                }
            }
        }
    }

    fn dispatch_direct_call(
        &mut self,
        target: LoweredDirectCallTarget,
        args: Vec<NanValue>,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        match target {
            LoweredDirectCallTarget::Builtin(name) | LoweredDirectCallTarget::Function(name) => {
                match self.lookup_path_nv(&name) {
                    Ok(fn_val) => self.dispatch_call(fn_val, args, conts),
                    Err(err) => EvalState::Apply(Err(err)),
                }
            }
            LoweredDirectCallTarget::Wrapper(kind) => match args.as_slice() {
                [inner] => {
                    let value = match kind {
                        crate::ir::WrapperKind::ResultOk => {
                            NanValue::new_ok_value(*inner, &mut self.arena)
                        }
                        crate::ir::WrapperKind::ResultErr => {
                            NanValue::new_err_value(*inner, &mut self.arena)
                        }
                        crate::ir::WrapperKind::OptionSome => {
                            NanValue::new_some_value(*inner, &mut self.arena)
                        }
                    };
                    EvalState::Apply(Ok(value))
                }
                _ => EvalState::Apply(Err(RuntimeError::Error(
                    "Wrapper constructor expects exactly 1 argument".to_string(),
                ))),
            },
            LoweredDirectCallTarget::NoneValue => {
                if args.is_empty() {
                    EvalState::Apply(Ok(NanValue::NONE))
                } else {
                    EvalState::Apply(Err(RuntimeError::Error(
                        "Option.None expects 0 arguments".to_string(),
                    )))
                }
            }
            LoweredDirectCallTarget::TypeConstructor {
                qualified_type_name,
                variant_name,
            } => {
                let name = format!("{qualified_type_name}.{variant_name}");
                EvalState::Apply(self.apply_runtime_constructor_args_nv(&name, &args))
            }
        }
    }

    fn dispatch_forward_call(
        &mut self,
        target: LoweredDirectCallTarget,
        args: &[LoweredForwardArg],
        call_line: usize,
        conts: &mut Vec<EvalCont>,
    ) -> EvalState {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            let value = match arg {
                LoweredForwardArg::Local(name) => match self.lookup_nv(name) {
                    Ok(value) => value,
                    Err(err) => return EvalState::Apply(Err(err)),
                },
                LoweredForwardArg::Slot(slot) => match self.lookup_slot(*slot) {
                    Ok(value) => value,
                    Err(err) => return EvalState::Apply(Err(err)),
                },
            };
            values.push(value);
        }
        self.last_call_line = call_line;
        self.dispatch_direct_call(target, values, conts)
            .map_err_line(call_line)
    }

    fn build_record_update_nv(
        &mut self,
        type_name: &str,
        base_type_id: u32,
        mut base_fields: Vec<NanValue>,
        base_field_names: Vec<String>,
        update_vals: Vec<(String, NanValue)>,
    ) -> Result<NanValue, RuntimeError> {
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
            if let Some(pos) = base_field_names.iter().position(|n| n == &update_name) {
                base_fields[pos] = update_val;
            } else {
                return Err(RuntimeError::Error(format!(
                    "Record '{}' has no field '{}'",
                    type_name, update_name
                )));
            }
        }

        let rec_idx = self.arena.push_record(base_type_id, base_fields);
        Ok(NanValue::new_record(rec_idx))
    }

    fn is_hashable_map_key_nv(value: NanValue) -> bool {
        value.is_int() || value.is_float() || value.is_string() || value.is_bool()
    }

    pub(super) fn eval_literal_nv(&mut self, lit: &Literal) -> NanValue {
        match lit {
            Literal::Int(i) => NanValue::new_int(*i, &mut self.arena),
            Literal::Float(f) => NanValue::new_float(*f),
            Literal::Str(s) => NanValue::new_string_value(s, &mut self.arena),
            Literal::Bool(b) => NanValue::new_bool(*b),
            Literal::Unit => NanValue::UNIT,
        }
    }

    #[allow(dead_code)]
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
        let fn_nv = NanValue::from_value(&fn_val, &mut self.arena);
        let nv_args: Vec<NanValue> = args
            .iter()
            .map(|v| NanValue::from_value(v, &mut self.arena))
            .collect();
        match self.start_call_nv(fn_nv, nv_args)? {
            CallDispatch::Immediate(result) => result.map(|nv| nv.to_value(&self.arena)),
            CallDispatch::EnterFunction { frame, state } => {
                let nv = self.eval_loop(state, vec![EvalCont::FunctionReturn(*frame)])?;
                Ok(nv.to_value(&self.arena))
            }
        }
    }
}

/// Hash NanValue args for memo cache — uses arena for content-based hashing.
fn hash_memo_args_nv(args: &[NanValue], arena: &Arena) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    args.len().hash(&mut hasher);
    for arg in args {
        arg.hash_in(&mut hasher, arena);
    }
    hasher.finish()
}
