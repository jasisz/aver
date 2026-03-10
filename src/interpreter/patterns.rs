use super::*;

impl Interpreter {
    fn ctor_type_and_variant(ctor_name: &str) -> Option<(&str, &str)> {
        let mut parts = ctor_name.rsplit('.');
        let variant = parts.next()?;
        let type_name = parts.next()?;
        Some((type_name, variant))
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
                    (Literal::Unit, Value::Unit) => true,
                    _ => false,
                };
                if matches { Some(HashMap::new()) } else { None }
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
                let (head_value, tail_value) = crate::value::list_uncons_value(value)?;
                let mut map = HashMap::new();
                if head != "_" {
                    map.insert(head.clone(), head_value);
                }
                if tail != "_" {
                    map.insert(tail.clone(), tail_value);
                }
                Some(map)
            }
            Pattern::Tuple(patterns) => {
                let Value::Tuple(values) = value else {
                    return None;
                };
                if patterns.len() != values.len() {
                    return None;
                }
                let mut all = HashMap::new();
                for (p, v) in patterns.iter().zip(values.iter()) {
                    let sub = self.match_pattern(p, v)?;
                    all.extend(sub);
                }
                Some(all)
            }
            Pattern::Constructor(ctor_name, bindings) => {
                match (ctor_name.as_str(), value) {
                    ("Option.None", Value::None) => Some(HashMap::new()),
                    ("Result.Ok", Value::Ok(inner)) => {
                        let mut map = HashMap::new();
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            map.insert(name.clone(), *inner.clone());
                        }
                        Some(map)
                    }
                    ("Result.Err", Value::Err(inner)) => {
                        let mut map = HashMap::new();
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            map.insert(name.clone(), *inner.clone());
                        }
                        Some(map)
                    }
                    ("Option.Some", Value::Some(inner)) => {
                        let mut map = HashMap::new();
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            map.insert(name.clone(), *inner.clone());
                        }
                        Some(map)
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
                        let mut map = HashMap::new();
                        for (name, val) in bindings.iter().zip(fields.iter()) {
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
