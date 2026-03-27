use super::*;
use crate::interpreter::lowered::LoweredMatchArm;
use crate::ir::{
    CallLowerCtx, DispatchLiteral, DispatchTableShape, MatchDispatchPlan, SemanticConstructor,
    SemanticDispatchPattern, WrapperKind, classify_constructor_name,
    classify_match_dispatch_plan_from_patterns,
};

pub(super) struct InterpreterLowerCtx<'a> {
    interpreter: &'a Interpreter,
}

impl<'a> InterpreterLowerCtx<'a> {
    pub(super) fn new(interpreter: &'a Interpreter) -> Self {
        Self { interpreter }
    }
}

impl CallLowerCtx for InterpreterLowerCtx<'_> {
    fn is_local_value(&self, _name: &str) -> bool {
        false
    }

    fn is_user_type(&self, name: &str) -> bool {
        self.interpreter.arena.find_type_id(name).is_some()
            || name
                .rsplit('.')
                .next()
                .is_some_and(|leaf| self.interpreter.arena.find_type_id(leaf).is_some())
    }

    fn resolve_module_call<'a>(&self, dotted: &'a str) -> Option<(&'a str, &'a str)> {
        for (idx, ch) in dotted.char_indices().rev() {
            if ch != '.' {
                continue;
            }
            let prefix = &dotted[..idx];
            let suffix = &dotted[idx + 1..];
            if suffix.is_empty() || !suffix.contains('.') {
                continue;
            }
            if self.interpreter.namespace_path_exists(prefix) {
                return Some((prefix, suffix));
            }
        }
        None
    }
}

impl Interpreter {
    pub(super) fn classify_runtime_constructor_name(&self, name: &str) -> SemanticConstructor {
        match name {
            "None" => SemanticConstructor::NoneValue,
            "Ok" => SemanticConstructor::Wrapper(WrapperKind::ResultOk),
            "Err" => SemanticConstructor::Wrapper(WrapperKind::ResultErr),
            "Some" => SemanticConstructor::Wrapper(WrapperKind::OptionSome),
            _ => classify_constructor_name(name, &InterpreterLowerCtx::new(self)),
        }
    }

    pub(super) fn ctor_type_matches_runtime(
        qualified_type_name: &str,
        runtime_type_name: &str,
    ) -> bool {
        qualified_type_name == runtime_type_name
            || qualified_type_name
                .rsplit('.')
                .next()
                .is_some_and(|leaf| leaf == runtime_type_name)
    }

    pub(super) fn namespace_path_exists(&self, path: &str) -> bool {
        let mut parts = path.split('.').filter(|part| !part.is_empty());
        let Some(first) = parts.next() else {
            return false;
        };
        let Ok(mut current) = self.lookup_nv(first) else {
            return false;
        };
        for part in parts {
            if !current.is_namespace() {
                return false;
            }
            let (_, members) = self.arena.get_namespace(current.symbol_index());
            let Some((_, next)) = members.iter().find(|(name, _)| name.as_ref() == part) else {
                return false;
            };
            current = *next;
        }
        current.is_namespace()
    }

    pub(super) fn apply_runtime_constructor_nv(
        &mut self,
        name: &str,
        inner: Option<NanValue>,
    ) -> Result<NanValue, RuntimeError> {
        match self.classify_runtime_constructor_name(name) {
            SemanticConstructor::NoneValue => match inner {
                None => Ok(NanValue::NONE),
                Some(_) => Err(RuntimeError::Error(format!(
                    "Constructor '{}' does not take an argument",
                    name
                ))),
            },
            SemanticConstructor::Wrapper(kind) => {
                let Some(inner) = inner else {
                    return Err(RuntimeError::Error(format!(
                        "Constructor '{}' expects an argument",
                        name
                    )));
                };
                match kind {
                    WrapperKind::ResultOk => Ok(NanValue::new_ok_value(inner, &mut self.arena)),
                    WrapperKind::ResultErr => Ok(NanValue::new_err_value(inner, &mut self.arena)),
                    WrapperKind::OptionSome => Ok(NanValue::new_some_value(inner, &mut self.arena)),
                }
            }
            SemanticConstructor::TypeConstructor {
                qualified_type_name,
                variant_name,
            } => {
                let runtime_type_name = qualified_type_name
                    .rsplit('.')
                    .next()
                    .unwrap_or(&qualified_type_name);
                let type_id = self
                    .arena
                    .find_type_id(runtime_type_name)
                    .unwrap_or_else(|| {
                        self.arena
                            .register_sum_type(runtime_type_name, vec![variant_name.clone()])
                    });
                let variant_id = self
                    .arena
                    .find_variant_id(type_id, &variant_name)
                    .unwrap_or_else(|| {
                        self.arena
                            .register_variant_name(type_id, variant_name.clone())
                    });

                let value = match inner {
                    Some(inner) => {
                        let variant_idx = self.arena.push_variant(type_id, variant_id, vec![inner]);
                        NanValue::new_variant(variant_idx)
                    }
                    None => {
                        let ctor_id =
                            self.arena
                                .find_ctor_id(type_id, variant_id)
                                .ok_or_else(|| {
                                    RuntimeError::Error(format!("Unknown constructor: {}", name))
                                })?;
                        let symbol = self.arena.push_nullary_variant_symbol(ctor_id);
                        NanValue::new_nullary_variant(symbol)
                    }
                };
                Ok(value)
            }
            SemanticConstructor::Unknown(_) => Err(RuntimeError::Error(format!(
                "Unknown constructor: {}",
                name
            ))),
        }
    }

    pub(super) fn try_dispatch_match_plan_nv(
        &mut self,
        subject: NanValue,
        arms: &[LoweredMatchArm],
    ) -> Option<(usize, Vec<(String, NanValue)>)> {
        let patterns: Vec<&Pattern> = arms.iter().map(|arm| &arm.pattern).collect();
        let plan =
            classify_match_dispatch_plan_from_patterns(&patterns, &InterpreterLowerCtx::new(self))?;

        let arm_index = match plan {
            MatchDispatchPlan::Bool(shape) => {
                if !subject.is_bool() {
                    return None;
                }
                if subject.as_bool() {
                    shape.true_arm_index
                } else {
                    shape.false_arm_index
                }
            }
            MatchDispatchPlan::List(shape) => {
                if !subject.is_list() {
                    return None;
                }
                if self.arena.list_is_empty_value(subject) {
                    shape.empty_arm_index
                } else {
                    shape.cons_arm_index
                }
            }
            MatchDispatchPlan::Table(shape) => self.dispatch_table_match_arm(subject, &shape)?,
        };

        self.match_pattern_nv(&arms[arm_index].pattern, subject)
            .map(|bindings| (arm_index, bindings))
    }

    fn dispatch_table_match_arm(
        &self,
        subject: NanValue,
        shape: &DispatchTableShape,
    ) -> Option<usize> {
        for entry in &shape.entries {
            if self.dispatch_pattern_matches_nv(subject, &entry.pattern) {
                return Some(entry.arm_index);
            }
        }
        shape.default_arm.as_ref().map(|default| default.arm_index)
    }

    fn dispatch_pattern_matches_nv(
        &self,
        subject: NanValue,
        pattern: &SemanticDispatchPattern,
    ) -> bool {
        match pattern {
            SemanticDispatchPattern::Literal(DispatchLiteral::Int(i)) => {
                subject.is_int() && subject.as_int(&self.arena) == *i
            }
            SemanticDispatchPattern::Literal(DispatchLiteral::Float(f)) => {
                subject.is_float()
                    && f.parse::<f64>()
                        .ok()
                        .is_some_and(|expected| subject.as_float().to_bits() == expected.to_bits())
            }
            SemanticDispatchPattern::Literal(DispatchLiteral::Bool(b)) => {
                subject.is_bool() && subject.as_bool() == *b
            }
            SemanticDispatchPattern::Literal(DispatchLiteral::Str(s)) => {
                subject.is_string() && self.arena.get_string_value(subject).as_str() == s.as_str()
            }
            SemanticDispatchPattern::Literal(DispatchLiteral::Unit) => subject.is_unit(),
            SemanticDispatchPattern::EmptyList => {
                subject.is_list() && self.arena.list_is_empty_value(subject)
            }
            SemanticDispatchPattern::NoneValue => subject.is_none(),
            SemanticDispatchPattern::WrapperTag(WrapperKind::ResultOk) => subject.is_ok(),
            SemanticDispatchPattern::WrapperTag(WrapperKind::ResultErr) => subject.is_err(),
            SemanticDispatchPattern::WrapperTag(WrapperKind::OptionSome) => subject.is_some(),
        }
    }
}
