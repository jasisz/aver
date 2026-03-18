use super::*;

impl Interpreter {
    fn ctor_type_and_variant(ctor_name: &str) -> Option<(&str, &str)> {
        let mut parts = ctor_name.rsplit('.');
        let variant = parts.next()?;
        let type_name = parts.next()?;
        Some((type_name, variant))
    }

    /// Old Value-based match_pattern — kept for external callers (checker/verify.rs).
    pub(super) fn match_pattern(
        &self,
        pattern: &Pattern,
        value: &Value,
    ) -> Option<Vec<(String, Value)>> {
        match pattern {
            Pattern::Wildcard => Some(Vec::new()),
            Pattern::Literal(lit) => {
                let matches = match (lit, value) {
                    (Literal::Int(i), Value::Int(v)) => i == v,
                    (Literal::Float(f), Value::Float(v)) => f == v,
                    (Literal::Str(s), Value::Str(v)) => s == v,
                    (Literal::Bool(b), Value::Bool(v)) => b == v,
                    (Literal::Unit, Value::Unit) => true,
                    _ => false,
                };
                if matches { Some(Vec::new()) } else { None }
            }
            Pattern::Ident(name) => Some(vec![(name.clone(), value.clone())]),
            Pattern::EmptyList => {
                if list_len(value) == Some(0) {
                    Some(Vec::new())
                } else {
                    None
                }
            }
            Pattern::Cons(head, tail) => {
                let (head_value, tail_value) = {
                    let items = list_view(value)?;
                    let (head_value, tail_items) = aver_rt::list_uncons_cloned(items)?;
                    (head_value, Value::List(tail_items))
                };

                let mut bindings = Vec::with_capacity(2);
                if head != "_" {
                    bindings.push((head.clone(), head_value));
                }
                if tail != "_" {
                    bindings.push((tail.clone(), tail_value));
                }
                Some(bindings)
            }
            Pattern::Tuple(patterns) => {
                let Value::Tuple(values) = value else {
                    return None;
                };
                if patterns.len() != values.len() {
                    return None;
                }
                let mut all = Vec::new();
                for (p, v) in patterns.iter().zip(values.iter()) {
                    let sub = self.match_pattern(p, v)?;
                    all.extend(sub);
                }
                Some(all)
            }
            Pattern::Constructor(ctor_name, bindings) => {
                match (ctor_name.as_str(), value) {
                    ("Option.None", Value::None) => Some(Vec::new()),
                    ("Result.Ok", Value::Ok(inner)) => {
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), *inner.clone()));
                        }
                        Some(result)
                    }
                    ("Result.Err", Value::Err(inner)) => {
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), *inner.clone()));
                        }
                        Some(result)
                    }
                    ("Option.Some", Value::Some(inner)) => {
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), *inner.clone()));
                        }
                        Some(result)
                    }
                    // User-defined variant: match by fully-qualified constructor name.
                    (
                        ctor,
                        Value::Variant {
                            type_name,
                            variant,
                            fields,
                            ..
                        },
                    ) => {
                        let matches = Self::ctor_type_and_variant(ctor).is_some_and(
                            |(ctor_type, ctor_variant)| {
                                ctor_type == type_name && ctor_variant == variant
                            },
                        );
                        if !matches {
                            return None;
                        }
                        if !bindings.is_empty() && bindings.len() != fields.len() {
                            return None;
                        }
                        let mut result = Vec::with_capacity(bindings.len());
                        for (name, val) in bindings.iter().zip(fields.iter()) {
                            if name != "_" {
                                result.push((name.clone(), val.clone()));
                            }
                        }
                        Some(result)
                    }
                    _ => None,
                }
            }
        }
    }

    /// NanValue-native match_pattern — used by the eval loop.
    /// Takes `&mut self` because Cons pattern needs to allocate the tail list in arena.
    pub(super) fn match_pattern_nv(
        &mut self,
        pattern: &Pattern,
        value: NanValue,
    ) -> Option<Vec<(String, NanValue)>> {
        match pattern {
            Pattern::Wildcard => Some(Vec::new()),
            Pattern::Literal(lit) => {
                let matches = match lit {
                    Literal::Int(i) => value.is_int() && value.as_int(&self.arena) == *i,
                    Literal::Float(f) => value.is_float() && value.as_float() == *f,
                    Literal::Str(s) => {
                        value.is_string()
                            && self.arena.get_string(value.arena_index()) == s.as_str()
                    }
                    Literal::Bool(b) => value.is_bool() && value.as_bool() == *b,
                    Literal::Unit => value.is_unit(),
                };
                if matches {
                    Some(Vec::new())
                } else {
                    None
                }
            }
            Pattern::Ident(name) => Some(vec![(name.clone(), value)]),
            Pattern::EmptyList => {
                if value.is_list() && self.arena.get_list(value.arena_index()).is_empty() {
                    Some(Vec::new())
                } else {
                    None
                }
            }
            Pattern::Cons(head, tail) => {
                if !value.is_list() {
                    return None;
                }
                let items = self.arena.get_list(value.arena_index());
                if items.is_empty() {
                    return None;
                }
                let head_val = items[0];
                let tail_items: Vec<NanValue> = items[1..].to_vec();

                let mut bindings = Vec::with_capacity(2);
                if head != "_" {
                    bindings.push((head.clone(), head_val));
                }
                if tail != "_" {
                    let tail_idx = self.arena.push_list(tail_items);
                    bindings.push((tail.clone(), NanValue::new_list(tail_idx)));
                }
                Some(bindings)
            }
            Pattern::Tuple(patterns) => {
                if !value.is_tuple() {
                    return None;
                }
                let items: Vec<NanValue> = self.arena.get_tuple(value.arena_index()).to_vec();
                if patterns.len() != items.len() {
                    return None;
                }
                let mut all = Vec::new();
                for (p, v) in patterns.iter().zip(items.iter()) {
                    let sub = self.match_pattern_nv(p, *v)?;
                    all.extend(sub);
                }
                Some(all)
            }
            Pattern::Constructor(ctor_name, bindings) => {
                match ctor_name.as_str() {
                    "Option.None" => {
                        if value.is_none() {
                            Some(Vec::new())
                        } else {
                            None
                        }
                    }
                    "Result.Ok" => {
                        if !value.is_ok() {
                            return None;
                        }
                        let inner = self.arena.get_boxed(value.wrapper_index());
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), inner));
                        }
                        Some(result)
                    }
                    "Result.Err" => {
                        if !value.is_err() {
                            return None;
                        }
                        let inner = self.arena.get_boxed(value.wrapper_index());
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), inner));
                        }
                        Some(result)
                    }
                    "Option.Some" => {
                        if !value.is_some() {
                            return None;
                        }
                        let inner = self.arena.get_boxed(value.wrapper_index());
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), inner));
                        }
                        Some(result)
                    }
                    ctor => {
                        // User-defined variant
                        if !value.is_variant() {
                            return None;
                        }
                        let (type_id, variant_id, fields) =
                            self.arena.get_variant(value.arena_index());
                        let type_name = self.arena.get_type_name(type_id);
                        let variant_name = self.arena.get_variant_name(type_id, variant_id);

                        let matches = Self::ctor_type_and_variant(ctor).is_some_and(
                            |(ctor_type, ctor_variant)| {
                                ctor_type == type_name && ctor_variant == variant_name
                            },
                        );
                        if !matches {
                            return None;
                        }
                        if !bindings.is_empty() && bindings.len() != fields.len() {
                            return None;
                        }
                        let fields_copy: Vec<NanValue> = fields.to_vec();
                        let mut result = Vec::with_capacity(bindings.len());
                        for (name, val) in bindings.iter().zip(fields_copy.iter()) {
                            if name != "_" {
                                result.push((name.clone(), *val));
                            }
                        }
                        Some(result)
                    }
                }
            }
        }
    }
}
