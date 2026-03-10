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
}
