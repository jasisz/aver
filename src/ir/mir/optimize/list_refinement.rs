//! Runtime provenance for closed `List<Int>` refinements.
//!
//! A carrier projector from a closed refinement proves that every element of
//! the resulting list satisfies that refinement's derived interval. The proof
//! survives only operations that cannot introduce an element (`concat` of two
//! equally-proven lists, `take`, and `drop`). Feeding the fact back to the same
//! smart constructor makes its validation branch unreachable, so this pass
//! replaces the call with the constructor's `Result.Ok(record)` answer.
//!
//! Identity is the soundness boundary: constructor, projector, and nominal are
//! matched by `FnId` / `TypeId`. No source spelling such as `Bytes` is special.

use std::collections::HashMap;

use crate::analysis::literal_refinement::LiteralRefinementTable;
use crate::ast::{Spanned, Type};
use crate::ir::{BuiltinId, FnId, TypeId};

use super::super::expr::{
    MirConstruct, MirCtor, MirExpr, MirRecordCreate, MirRecordField, walk_children_mut,
};
use super::super::program::{LocalId, MirProgram};
use crate::ir::hir::BuiltinCtor;

#[derive(Debug, Clone)]
struct RefinementPlan {
    type_id: TypeId,
    type_name: String,
    carrier_field: String,
}

#[derive(Debug, Clone)]
struct ListFact {
    type_id: TypeId,
    /// Present only for an immediately nested exact carrier projection. It is
    /// safe to replace `from(project(value))` with `Ok(value)` because this
    /// removes the projection without duplicating evaluation. Facts stored in
    /// locals deliberately drop this field.
    direct_nominal: Option<Spanned<MirExpr>>,
}

#[derive(Debug, Clone, Copy)]
enum PreservingListOp {
    Concat,
    TakeOrDrop,
}

/// Discharge smart-constructor validation when the input list carries a
/// closed, identity-matching refinement provenance fact.
pub fn discharge_proven_lists(
    mut program: MirProgram,
    refinements: &LiteralRefinementTable,
) -> MirProgram {
    let mut constructors: HashMap<FnId, RefinementPlan> = HashMap::new();
    let mut projectors: HashMap<FnId, TypeId> = HashMap::new();
    let mut by_type: HashMap<TypeId, RefinementPlan> = HashMap::new();
    for ctor in refinements.iter().filter(|ctor| ctor.runtime_provenance) {
        let plan = RefinementPlan {
            type_id: ctor.type_id,
            type_name: ctor.type_name.clone(),
            carrier_field: ctor.carrier_field.clone(),
        };
        constructors.insert(ctor.fn_id, plan.clone());
        by_type.insert(ctor.type_id, plan);
        for projector in &ctor.projector_fns {
            projectors.insert(*projector, ctor.type_id);
        }
    }
    if constructors.is_empty() {
        return program;
    }

    let preserving_builtins: HashMap<BuiltinId, PreservingListOp> = program
        .builtins
        .iter()
        .enumerate()
        .filter_map(|(index, name)| {
            let op = match name.as_str() {
                "List.concat" => PreservingListOp::Concat,
                "List.take" | "List.drop" => PreservingListOp::TakeOrDrop,
                _ => return None,
            };
            Some((BuiltinId(index as u32), op))
        })
        .collect();

    for mir_fn in program.fns.values_mut() {
        let mut locals = HashMap::new();
        rewrite_expr(
            &mut mir_fn.body,
            &mut locals,
            &constructors,
            &projectors,
            &by_type,
            &preserving_builtins,
        );
    }
    program
}

fn rewrite_expr(
    expr: &mut Spanned<MirExpr>,
    locals: &mut HashMap<LocalId, ListFact>,
    constructors: &HashMap<FnId, RefinementPlan>,
    projectors: &HashMap<FnId, TypeId>,
    by_type: &HashMap<TypeId, RefinementPlan>,
    preserving_builtins: &HashMap<BuiltinId, PreservingListOp>,
) -> Option<ListFact> {
    match &mut expr.node {
        MirExpr::Local(local) => locals.get(&local.node.slot).cloned().map(|mut fact| {
            fact.direct_nominal = None;
            fact
        }),
        MirExpr::Let(let_node) => {
            let value_fact = rewrite_expr(
                &mut let_node.node.value,
                locals,
                constructors,
                projectors,
                by_type,
                preserving_builtins,
            );
            let binding = let_node.node.binding;
            let previous = match value_fact {
                Some(mut fact) => {
                    fact.direct_nominal = None;
                    locals.insert(binding, fact)
                }
                None => locals.remove(&binding),
            };
            let body_fact = rewrite_expr(
                &mut let_node.node.body,
                locals,
                constructors,
                projectors,
                by_type,
                preserving_builtins,
            );
            match previous {
                Some(fact) => {
                    locals.insert(binding, fact);
                }
                None => {
                    locals.remove(&binding);
                }
            }
            body_fact
        }
        MirExpr::Call(call_node) => {
            let arg_facts = call_node
                .node
                .args
                .iter_mut()
                .map(|arg| {
                    rewrite_expr(
                        arg,
                        locals,
                        constructors,
                        projectors,
                        by_type,
                        preserving_builtins,
                    )
                })
                .collect::<Vec<_>>();

            if let super::super::expr::MirCallee::Fn(fn_id) = call_node.node.callee {
                if let Some(plan) = constructors.get(&fn_id)
                    && let [Some(fact)] = arg_facts.as_slice()
                    && fact.type_id == plan.type_id
                {
                    let direct = fact.direct_nominal.clone();
                    let carrier = call_node.node.args.remove(0);
                    replace_with_success(expr, plan, direct, carrier);
                    return None;
                }
                if let Some(type_id) = projectors.get(&fn_id).copied()
                    && call_node.node.args.len() == 1
                    && call_node.node.args[0].ty().and_then(Type::named_id) == Some(type_id)
                {
                    return Some(ListFact {
                        type_id,
                        direct_nominal: Some(call_node.node.args[0].clone()),
                    });
                }
            }

            if let super::super::expr::MirCallee::Builtin(builtin) = call_node.node.callee {
                match preserving_builtins.get(&builtin) {
                    Some(PreservingListOp::Concat) => {
                        if let [Some(left), Some(right)] = arg_facts.as_slice()
                            && left.type_id == right.type_id
                        {
                            return Some(ListFact {
                                type_id: left.type_id,
                                direct_nominal: None,
                            });
                        }
                    }
                    Some(PreservingListOp::TakeOrDrop) => {
                        if let Some(Some(source)) = arg_facts.first() {
                            return Some(ListFact {
                                type_id: source.type_id,
                                direct_nominal: None,
                            });
                        }
                    }
                    None => {}
                }
            }
            None
        }
        MirExpr::Project(project) => {
            rewrite_expr(
                &mut project.node.base,
                locals,
                constructors,
                projectors,
                by_type,
                preserving_builtins,
            );
            let type_id = project.node.base.ty().and_then(Type::named_id)?;
            let plan = by_type.get(&type_id)?;
            (project.node.field == plan.carrier_field).then(|| ListFact {
                type_id,
                direct_nominal: Some((*project.node.base).clone()),
            })
        }
        MirExpr::IfThenElse(branches) => {
            rewrite_expr(
                &mut branches.node.cond,
                locals,
                constructors,
                projectors,
                by_type,
                preserving_builtins,
            );
            let mut then_locals = locals.clone();
            let then_fact = rewrite_expr(
                &mut branches.node.then_branch,
                &mut then_locals,
                constructors,
                projectors,
                by_type,
                preserving_builtins,
            );
            let mut else_locals = locals.clone();
            let else_fact = rewrite_expr(
                &mut branches.node.else_branch,
                &mut else_locals,
                constructors,
                projectors,
                by_type,
                preserving_builtins,
            );
            same_fact(then_fact, else_fact)
        }
        MirExpr::Match(match_node) => {
            rewrite_expr(
                &mut match_node.node.subject,
                locals,
                constructors,
                projectors,
                by_type,
                preserving_builtins,
            );
            let mut facts = Vec::with_capacity(match_node.node.arms.len());
            for arm in &mut match_node.node.arms {
                let mut arm_locals = locals.clone();
                facts.push(rewrite_expr(
                    &mut arm.body,
                    &mut arm_locals,
                    constructors,
                    projectors,
                    by_type,
                    preserving_builtins,
                ));
            }
            common_fact(facts)
        }
        MirExpr::Return(inner) => rewrite_expr(
            inner,
            locals,
            constructors,
            projectors,
            by_type,
            preserving_builtins,
        ),
        _ => {
            walk_children_mut(&mut expr.node, &mut |child| {
                let _ = rewrite_expr(
                    child,
                    locals,
                    constructors,
                    projectors,
                    by_type,
                    preserving_builtins,
                );
            });
            None
        }
    }
}

fn same_fact(left: Option<ListFact>, right: Option<ListFact>) -> Option<ListFact> {
    match (left, right) {
        (Some(left), Some(right)) if left.type_id == right.type_id => Some(ListFact {
            type_id: left.type_id,
            direct_nominal: None,
        }),
        _ => None,
    }
}

fn common_fact(facts: Vec<Option<ListFact>>) -> Option<ListFact> {
    let mut facts = facts.into_iter();
    let first = facts.next()??;
    facts
        .all(|fact| fact.is_some_and(|fact| fact.type_id == first.type_id))
        .then(|| ListFact {
            type_id: first.type_id,
            direct_nominal: None,
        })
}

fn replace_with_success(
    expr: &mut Spanned<MirExpr>,
    plan: &RefinementPlan,
    direct_nominal: Option<Spanned<MirExpr>>,
    carrier: Spanned<MirExpr>,
) {
    let line = expr.line;
    let result_ty = expr.ty().cloned();
    let nominal_ty = Type::named_resolved(plan.type_id, plan.type_name.clone());
    let nominal = direct_nominal.unwrap_or_else(|| {
        let record = typed_span(
            MirRecordCreate {
                type_id: Some(plan.type_id),
                type_name: plan.type_name.clone(),
                fields: vec![MirRecordField {
                    name: plan.carrier_field.clone(),
                    value: carrier,
                }],
            },
            line,
            Some(nominal_ty.clone()),
        );
        typed_span(
            MirExpr::RecordCreate(record),
            line,
            Some(nominal_ty.clone()),
        )
    });
    let construct = typed_span(
        MirConstruct {
            ctor: MirCtor::Builtin(BuiltinCtor::ResultOk),
            args: vec![nominal],
        },
        line,
        result_ty,
    );
    expr.node = MirExpr::Construct(construct);
}

fn typed_span<T>(node: T, line: crate::ast::SourceLine, ty: Option<Type>) -> Spanned<T> {
    let cell = std::sync::OnceLock::new();
    if let Some(ty) = ty {
        let _ = cell.set(ty);
    }
    Spanned {
        node,
        line,
        ty: cell,
    }
}

#[cfg(test)]
mod tests;
