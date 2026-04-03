use super::*;
use crate::ir::{SemanticConstructor, WrapperKind};

impl Interpreter {
    #[allow(dead_code)]
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
                match (self.classify_runtime_constructor_name(ctor_name), value) {
                    (SemanticConstructor::NoneValue, Value::None) => Some(Vec::new()),
                    (SemanticConstructor::Wrapper(WrapperKind::ResultOk), Value::Ok(inner)) => {
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), *inner.clone()));
                        }
                        Some(result)
                    }
                    (SemanticConstructor::Wrapper(WrapperKind::ResultErr), Value::Err(inner)) => {
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), *inner.clone()));
                        }
                        Some(result)
                    }
                    (SemanticConstructor::Wrapper(WrapperKind::OptionSome), Value::Some(inner)) => {
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
                        SemanticConstructor::TypeConstructor {
                            qualified_type_name,
                            variant_name,
                        },
                        Value::Variant {
                            type_name,
                            variant,
                            fields,
                            ..
                        },
                    ) => {
                        if !Self::ctor_type_matches_runtime(&qualified_type_name, type_name)
                            || variant_name != *variant
                        {
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
                            && self.arena.get_string_value(value).as_str() == s.as_str()
                    }
                    Literal::Bool(b) => value.is_bool() && value.as_bool() == *b,
                    Literal::Unit => value.is_unit(),
                };
                if matches { Some(Vec::new()) } else { None }
            }
            Pattern::Ident(name) => Some(vec![(name.clone(), value)]),
            Pattern::EmptyList => {
                if value.is_list() && self.arena.list_is_empty_value(value) {
                    Some(Vec::new())
                } else {
                    None
                }
            }
            Pattern::Cons(head, tail) => {
                if !value.is_list() {
                    return None;
                }
                let (head_val, tail_val) = self.arena.list_uncons(value)?;

                let mut bindings = Vec::with_capacity(2);
                if head != "_" {
                    bindings.push((head.clone(), head_val));
                }
                if tail != "_" {
                    bindings.push((tail.clone(), tail_val));
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
                match self.classify_runtime_constructor_name(ctor_name) {
                    SemanticConstructor::NoneValue => {
                        if value.is_none() {
                            Some(Vec::new())
                        } else {
                            None
                        }
                    }
                    SemanticConstructor::Wrapper(WrapperKind::ResultOk) => {
                        if !value.is_ok() {
                            return None;
                        }
                        let inner = value.wrapper_inner(&self.arena);
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), inner));
                        }
                        Some(result)
                    }
                    SemanticConstructor::Wrapper(WrapperKind::ResultErr) => {
                        if !value.is_err() {
                            return None;
                        }
                        let inner = value.wrapper_inner(&self.arena);
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), inner));
                        }
                        Some(result)
                    }
                    SemanticConstructor::Wrapper(WrapperKind::OptionSome) => {
                        if !value.is_some() {
                            return None;
                        }
                        let inner = value.wrapper_inner(&self.arena);
                        let mut result = Vec::with_capacity(1);
                        if let Some(name) = bindings.first()
                            && name != "_"
                        {
                            result.push((name.clone(), inner));
                        }
                        Some(result)
                    }
                    SemanticConstructor::TypeConstructor {
                        qualified_type_name,
                        variant_name,
                    } => {
                        // User-defined variant
                        if !value.is_variant() {
                            return None;
                        }
                        let (type_name, runtime_variant_name, fields) =
                            if let Some((type_id, variant_id, inner)) =
                                value.inline_variant_info(&self.arena)
                            {
                                (
                                    self.arena.get_type_name(type_id),
                                    self.arena.get_variant_name(type_id, variant_id),
                                    vec![inner],
                                )
                            } else {
                                let (type_id, variant_id, fields) =
                                    value.variant_parts(&self.arena)?;
                                (
                                    self.arena.get_type_name(type_id),
                                    self.arena.get_variant_name(type_id, variant_id),
                                    fields.to_vec(),
                                )
                            };

                        if !Self::ctor_type_matches_runtime(&qualified_type_name, type_name)
                            || variant_name != runtime_variant_name
                        {
                            return None;
                        }
                        if !bindings.is_empty() && bindings.len() != fields.len() {
                            return None;
                        }
                        let mut result = Vec::with_capacity(bindings.len());
                        for (name, val) in bindings.iter().zip(fields.iter()) {
                            if name != "_" {
                                result.push((name.clone(), *val));
                            }
                        }
                        Some(result)
                    }
                    SemanticConstructor::Unknown(_) => None,
                }
            }
        }
    }
}
