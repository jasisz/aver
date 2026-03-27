use crate::ast::{Literal, MatchArm, Pattern};

use super::{CallLowerCtx, SemanticConstructor, WrapperKind, classify_constructor_name};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DispatchLiteral {
    Int(i64),
    Float(String),
    Bool(bool),
    Str(String),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticDispatchPattern {
    Literal(DispatchLiteral),
    EmptyList,
    NoneValue,
    WrapperTag(WrapperKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoolMatchShape {
    pub true_arm_index: usize,
    pub false_arm_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ListMatchShape {
    pub empty_arm_index: usize,
    pub cons_arm_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DispatchBindingPlan {
    None,
    WrapperPayload(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DispatchArmPlan {
    pub pattern: SemanticDispatchPattern,
    pub arm_index: usize,
    pub binding: DispatchBindingPlan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DispatchDefaultPlan {
    pub arm_index: usize,
    pub binding_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DispatchTableShape {
    pub entries: Vec<DispatchArmPlan>,
    pub default_arm: Option<DispatchDefaultPlan>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchDispatchPlan {
    Bool(BoolMatchShape),
    List(ListMatchShape),
    Table(DispatchTableShape),
}

pub fn classify_dispatch_pattern(
    pattern: &Pattern,
    ctx: &impl CallLowerCtx,
) -> Option<SemanticDispatchPattern> {
    match pattern {
        Pattern::Literal(lit) => Some(SemanticDispatchPattern::Literal(dispatch_literal_from_ast(
            lit,
        ))),
        Pattern::EmptyList => Some(SemanticDispatchPattern::EmptyList),
        Pattern::Constructor(name, bindings) => match classify_constructor_name(name, ctx) {
            SemanticConstructor::NoneValue if bindings.is_empty() => {
                Some(SemanticDispatchPattern::NoneValue)
            }
            SemanticConstructor::Wrapper(kind) if bindings.len() <= 1 => {
                Some(SemanticDispatchPattern::WrapperTag(kind))
            }
            _ => None,
        },
        _ => None,
    }
}

pub fn classify_bool_match_shape(arms: &[MatchArm]) -> Option<BoolMatchShape> {
    if arms.len() != 2 {
        return None;
    }

    match (&arms[0].pattern, &arms[1].pattern) {
        (Pattern::Literal(Literal::Bool(true)), Pattern::Literal(Literal::Bool(false))) => {
            Some(BoolMatchShape {
                true_arm_index: 0,
                false_arm_index: 1,
            })
        }
        (Pattern::Literal(Literal::Bool(false)), Pattern::Literal(Literal::Bool(true))) => {
            Some(BoolMatchShape {
                true_arm_index: 1,
                false_arm_index: 0,
            })
        }
        (Pattern::Literal(Literal::Bool(true)), Pattern::Wildcard | Pattern::Ident(_)) => {
            Some(BoolMatchShape {
                true_arm_index: 0,
                false_arm_index: 1,
            })
        }
        _ => None,
    }
}

pub fn classify_list_match_shape(arms: &[MatchArm]) -> Option<ListMatchShape> {
    if arms.len() != 2 {
        return None;
    }

    match (&arms[0].pattern, &arms[1].pattern) {
        (Pattern::EmptyList, Pattern::Cons(_, _)) => Some(ListMatchShape {
            empty_arm_index: 0,
            cons_arm_index: 1,
        }),
        (Pattern::Cons(_, _), Pattern::EmptyList) => Some(ListMatchShape {
            empty_arm_index: 1,
            cons_arm_index: 0,
        }),
        _ => None,
    }
}

pub fn classify_dispatch_table_shape(
    arms: &[MatchArm],
    ctx: &impl CallLowerCtx,
) -> Option<DispatchTableShape> {
    if arms.len() < 2 {
        return None;
    }

    let has_default = matches!(
        arms.last().map(|a| &a.pattern),
        Some(Pattern::Wildcard | Pattern::Ident(_))
    );
    let dispatchable_end = if has_default {
        arms.len() - 1
    } else {
        arms.len()
    };

    let mut entries = Vec::new();
    for (arm_index, arm) in arms[..dispatchable_end].iter().enumerate() {
        let pattern = classify_dispatch_pattern(&arm.pattern, ctx)?;
        entries.push(DispatchArmPlan {
            binding: classify_dispatch_binding(&arm.pattern, &pattern),
            pattern,
            arm_index,
        });
    }

    if entries.len() < 2 {
        return None;
    }

    Some(DispatchTableShape {
        entries,
        default_arm: has_default
            .then(|| classify_default_arm_plan(&arms[arms.len() - 1].pattern, arms.len() - 1)),
    })
}

pub fn classify_match_dispatch_plan(
    arms: &[MatchArm],
    ctx: &impl CallLowerCtx,
) -> Option<MatchDispatchPlan> {
    if let Some(shape) = classify_bool_match_shape(arms) {
        return Some(MatchDispatchPlan::Bool(shape));
    }

    if let Some(shape) = classify_list_match_shape(arms) {
        return Some(MatchDispatchPlan::List(shape));
    }

    classify_dispatch_table_shape(arms, ctx).map(MatchDispatchPlan::Table)
}

fn dispatch_literal_from_ast(lit: &Literal) -> DispatchLiteral {
    match lit {
        Literal::Int(i) => DispatchLiteral::Int(*i),
        Literal::Float(f) => DispatchLiteral::Float(f.to_string()),
        Literal::Bool(b) => DispatchLiteral::Bool(*b),
        Literal::Str(s) => DispatchLiteral::Str(s.clone()),
        Literal::Unit => DispatchLiteral::Unit,
    }
}

fn classify_dispatch_binding(
    pattern: &Pattern,
    semantic: &SemanticDispatchPattern,
) -> DispatchBindingPlan {
    match (pattern, semantic) {
        (Pattern::Constructor(_, bindings), SemanticDispatchPattern::WrapperTag(_))
            if !bindings.is_empty() && bindings[0] != "_" =>
        {
            DispatchBindingPlan::WrapperPayload(bindings[0].clone())
        }
        _ => DispatchBindingPlan::None,
    }
}

fn classify_default_arm_plan(pattern: &Pattern, arm_index: usize) -> DispatchDefaultPlan {
    let binding_name = match pattern {
        Pattern::Ident(name) if name != "_" => Some(name.clone()),
        _ => None,
    };

    DispatchDefaultPlan {
        arm_index,
        binding_name,
    }
}
