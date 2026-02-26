use super::*;

impl Interpreter {
    pub(super) fn eval_match(
        &mut self,
        subject: Value,
        arms: &[MatchArm],
    ) -> Result<Value, RuntimeError> {
        for arm in arms {
            if let Some(bindings) = self.match_pattern(&arm.pattern, &subject) {
                // If we're in a resolved function and all bindings have slots,
                // write directly into the Slots frame (no extra scope push).
                if let Some(local_slots) = self.active_local_slots.clone() {
                    let all_slotted = bindings.keys().all(|name| local_slots.contains_key(name));
                    if all_slotted {
                        for (name, val) in bindings {
                            if let Some(&slot) = local_slots.get(&name) {
                                self.define_slot(slot, val);
                            }
                        }
                        return self.eval_expr(&arm.body);
                    }
                }
                // Fallback: HashMap-based scope for unresolved functions / REPL
                let rc_scope = bindings
                    .into_iter()
                    .map(|(k, v)| (k, Rc::new(v)))
                    .collect::<HashMap<_, _>>();
                self.push_env(EnvFrame::Owned(rc_scope));
                let result = self.eval_expr(&arm.body);
                self.pop_env();
                return result;
            }
        }
        Err(RuntimeError::Error(format!(
            "No match found for value {}",
            aver_repr(&subject)
        )))
    }

    pub(super) fn match_pattern(
        &self,
        pattern: &Pattern,
        value: &Value,
    ) -> Option<HashMap<String, Value>> {
        match pattern {
            Pattern::Wildcard => Some(HashMap::new()),
            Pattern::Literal(lit) => {
                let matches = match (lit, value) {
                    (Literal::Int(i), Value::Int(v)) => i == v,
                    (Literal::Float(f), Value::Float(v)) => f == v,
                    (Literal::Str(s), Value::Str(v)) => s == v,
                    (Literal::Bool(b), Value::Bool(v)) => b == v,
                    _ => false,
                };
                if matches {
                    Some(HashMap::new())
                } else {
                    None
                }
            }
            Pattern::Ident(name) => {
                let mut bindings = HashMap::new();
                bindings.insert(name.clone(), value.clone());
                Some(bindings)
            }
            Pattern::EmptyList => {
                if list_len(value) == Some(0) {
                    Some(HashMap::new())
                } else {
                    None
                }
            }
            Pattern::Cons(head, tail) => {
                let items = list_slice(value)?;
                if items.is_empty() {
                    return None;
                }
                let mut map = HashMap::new();
                if head != "_" {
                    map.insert(head.clone(), items[0].clone());
                }
                if tail != "_" {
                    map.insert(
                        tail.clone(),
                        list_tail_view(value).unwrap_or_else(|| list_from_vec(vec![])),
                    );
                }
                Some(map)
            }
            Pattern::Constructor(ctor_name, bindings) => {
                match (ctor_name.as_str(), value) {
                    ("Option.None", Value::None) => Some(HashMap::new()),
                    ("Result.Ok", Value::Ok(inner)) => {
                        let mut map = HashMap::new();
                        if let Some(name) = bindings.first() {
                            if name != "_" {
                                map.insert(name.clone(), *inner.clone());
                            }
                        }
                        Some(map)
                    }
                    ("Result.Err", Value::Err(inner)) => {
                        let mut map = HashMap::new();
                        if let Some(name) = bindings.first() {
                            if name != "_" {
                                map.insert(name.clone(), *inner.clone());
                            }
                        }
                        Some(map)
                    }
                    ("Option.Some", Value::Some(inner)) => {
                        let mut map = HashMap::new();
                        if let Some(name) = bindings.first() {
                            if name != "_" {
                                map.insert(name.clone(), *inner.clone());
                            }
                        }
                        Some(map)
                    }
                    // User-defined variant: match by variant name, qualified or unqualified
                    (
                        ctor,
                        Value::Variant {
                            type_name,
                            variant,
                            fields,
                            ..
                        },
                    ) => {
                        let matches = if ctor.contains('.') {
                            // Qualified: "Shape.Circle"
                            let mut parts = ctor.splitn(2, '.');
                            parts.next().map_or(false, |t| t == type_name)
                                && parts.next().map_or(false, |v| v == variant)
                        } else {
                            // Unqualified (builtins like Ok/Err handled above; this catches
                            // any legacy unqualified user-defined patterns)
                            ctor == variant
                        };
                        if !matches {
                            return None;
                        }
                        if !bindings.is_empty() && bindings.len() != fields.len() {
                            return None;
                        }
                        let mut map = HashMap::new();
                        for (name, val) in bindings.iter().zip(fields.iter()) {
                            if name != "_" {
                                map.insert(name.clone(), val.clone());
                            }
                        }
                        Some(map)
                    }
                    // Record destructuring: positional, matched by type_name
                    (
                        ctor,
                        Value::Record {
                            type_name,
                            fields: rf,
                            ..
                        },
                    ) if ctor == type_name => {
                        if !bindings.is_empty() && bindings.len() != rf.len() {
                            return None;
                        }
                        let mut map = HashMap::new();
                        for (name, (_, val)) in bindings.iter().zip(rf.iter()) {
                            if name != "_" {
                                map.insert(name.clone(), val.clone());
                            }
                        }
                        Some(map)
                    }
                    _ => None,
                }
            }
        }
    }
}
