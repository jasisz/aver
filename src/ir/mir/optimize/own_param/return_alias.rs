//! Return-alias summaries for named-function collection results.
//!
//! A function is summarized only when every normal and explicit return can be
//! traced to fresh collection storage or to a complete set of its parameters.
//! Absence is the conservative unknown state; an empty set means fresh.

use std::collections::{HashMap, HashSet};

use crate::ast::Spanned;
use crate::ir::FnId;
use crate::ir::mir::expr::{MirCallee, MirExpr, walk_children};
use crate::ir::mir::program::MirProgram;

use super::{MAX_DEPTH, OwnershipModel};

/// Named functions whose result has a complete alias proof, mapped to the
/// parameter indices whose collection backing the result may carry.
pub(super) type ReturnAliasSummary = HashMap<FnId, HashSet<usize>>;

/// Prove which parameters may supply the collection backing returned by each
/// statically named function.
///
/// The lattice is deliberately one-way: a function is absent (unknown) until
/// its complete result shape can be expressed as a set of parameter indices.
/// A known empty set means fresh. Acyclic helper chains become known as their
/// callees do; recursive result cycles remain unknown rather than guessing a
/// fixpoint. That conservative loss of precision is safe and does not affect
/// ordinary tail-recursive accumulator functions, whose recursive *arguments*
/// are the consumer of this summary rather than their own returned call.
pub(super) fn compute_return_alias_summary(
    program: &MirProgram,
    provenance: &HashMap<FnId, HashMap<u32, Spanned<MirExpr>>>,
    builtins: &[String],
    model: OwnershipModel,
) -> ReturnAliasSummary {
    let param_slots: HashMap<FnId, HashMap<u32, usize>> = program
        .iter()
        .map(|(id, f)| {
            (
                *id,
                f.params
                    .iter()
                    .enumerate()
                    .map(|(index, param)| (param.local.0, index))
                    .collect(),
            )
        })
        .collect();
    let mut summary = ReturnAliasSummary::new();

    loop {
        let mut changed = false;
        for (id, f) in program.iter() {
            if summary.contains_key(id) {
                continue;
            }
            let Some(slots) = param_slots.get(id) else {
                continue;
            };
            let empty_provenance = HashMap::new();
            let prov = provenance.get(id).unwrap_or(&empty_provenance);
            let Some(mut aliases) =
                return_aliases_of_expr(&f.body.node, slots, prov, builtins, &summary, model, 0)
            else {
                continue;
            };
            if !collect_explicit_return_aliases(
                &f.body.node,
                slots,
                prov,
                builtins,
                &summary,
                model,
                &mut aliases,
                0,
            ) {
                continue;
            }
            summary.insert(*id, aliases);
            changed = true;
        }
        if !changed {
            break;
        }
    }

    summary
}

/// Alias sources of the value produced normally by `expr`. `None` is the
/// conservative answer: the expression may surface collection backing that
/// this proof cannot trace completely.
fn return_aliases_of_expr(
    expr: &MirExpr,
    param_slots: &HashMap<u32, usize>,
    provenance: &HashMap<u32, Spanned<MirExpr>>,
    builtins: &[String],
    summary: &ReturnAliasSummary,
    model: OwnershipModel,
    depth: u32,
) -> Option<HashSet<usize>> {
    if depth > MAX_DEPTH {
        return None;
    }

    let aliases_of = |expr: &MirExpr| {
        return_aliases_of_expr(
            expr,
            param_slots,
            provenance,
            builtins,
            summary,
            model,
            depth + 1,
        )
    };
    let union = |expressions: Vec<&MirExpr>| -> Option<HashSet<usize>> {
        let mut aliases = HashSet::new();
        for expr in expressions {
            aliases.extend(aliases_of(expr)?);
        }
        Some(aliases)
    };

    match expr {
        MirExpr::Local(local) => {
            let slot = local.node.slot.0;
            if let Some(&index) = param_slots.get(&slot) {
                return Some(HashSet::from([index]));
            }
            aliases_of(&provenance.get(&slot)?.node)
        }
        MirExpr::Let(let_expr) => aliases_of(&let_expr.node.body.node),
        MirExpr::Match(match_expr) => union(
            match_expr
                .node
                .arms
                .iter()
                .map(|arm| &arm.body.node)
                .collect(),
        ),
        MirExpr::IfThenElse(branches) => union(vec![
            &branches.node.then_branch.node,
            &branches.node.else_branch.node,
        ]),
        MirExpr::Return(inner) | MirExpr::Box(inner) | MirExpr::Unbox(inner) => {
            aliases_of(&inner.node)
        }
        MirExpr::Try(inner) if model.returned_aggregates_are_consumed() => aliases_of(&inner.node),
        MirExpr::Construct(construct) if model.returned_aggregates_are_consumed() => {
            union(construct.node.args.iter().map(|arg| &arg.node).collect())
        }
        MirExpr::Call(call) => match &call.node.callee {
            MirCallee::Fn(target) => {
                let sources = summary.get(target)?;
                let mut aliases = HashSet::new();
                for &source in sources {
                    aliases.extend(aliases_of(&call.node.args.get(source)?.node)?);
                }
                Some(aliases)
            }
            MirCallee::Builtin(id) => {
                let name = builtins
                    .get(id.0 as usize)
                    .map(String::as_str)
                    .unwrap_or("");
                match name {
                    // Fresh collection backings.
                    "Map.new" | "Map.fromList" | "Vector.new" | "Vector.fromList" => {
                        Some(HashSet::new())
                    }
                    // Persistent mutation successors carry only their target
                    // collection's backing.
                    "Map.set" | "Map.remove" | "Vector.set" => {
                        aliases_of(&call.node.args.first()?.node)
                    }
                    // The result is one of the two handles.
                    "Option.withDefault" if call.node.args.len() == 2 => {
                        union(vec![&call.node.args[0].node, &call.node.args[1].node])
                    }
                    _ => None,
                }
            }
            MirCallee::LocalSlot { .. } | MirCallee::Intrinsic(_) => None,
        },
        MirExpr::TailCall(call) => {
            let sources = summary.get(&call.node.target)?;
            let mut aliases = HashSet::new();
            for &source in sources {
                aliases.extend(aliases_of(&call.node.args.get(source)?.node)?);
            }
            Some(aliases)
        }
        // A map literal owns a new backing table regardless of the values it
        // contains. Other result shapes are irrelevant to direct Map/Vector
        // parameter ownership and stay unknown rather than being generalized
        // accidentally across aggregate projections.
        MirExpr::MapLiteral(_) => Some(HashSet::new()),
        MirExpr::Literal(_)
        | MirExpr::BinOp(_)
        | MirExpr::Neg(_)
        | MirExpr::InterpolatedStr(_)
        | MirExpr::FnValue(_)
            if model.returned_aggregates_are_consumed() =>
        {
            Some(HashSet::new())
        }
        MirExpr::Literal(_)
        | MirExpr::BinOp(_)
        | MirExpr::Neg(_)
        | MirExpr::Try(_)
        | MirExpr::List(_)
        | MirExpr::Tuple(_)
        | MirExpr::Construct(_)
        | MirExpr::RecordCreate(_)
        | MirExpr::RecordUpdate(_)
        | MirExpr::Project(_)
        | MirExpr::InterpolatedStr(_)
        | MirExpr::IndependentProduct(_)
        | MirExpr::FnValue(_) => None,
    }
}

/// Add every explicit early-return source to the normal tail result. A return
/// nested in a condition or let-value is still a function exit and therefore
/// part of the alias contract even though it is not the surrounding
/// expression's normal value.
#[allow(clippy::too_many_arguments)]
fn collect_explicit_return_aliases(
    expr: &MirExpr,
    param_slots: &HashMap<u32, usize>,
    provenance: &HashMap<u32, Spanned<MirExpr>>,
    builtins: &[String],
    summary: &ReturnAliasSummary,
    model: OwnershipModel,
    out: &mut HashSet<usize>,
    depth: u32,
) -> bool {
    if depth > MAX_DEPTH {
        return false;
    }
    if let MirExpr::Return(inner) = expr {
        let Some(aliases) = return_aliases_of_expr(
            &inner.node,
            param_slots,
            provenance,
            builtins,
            summary,
            model,
            depth + 1,
        ) else {
            return false;
        };
        out.extend(aliases);
    }

    let mut complete = true;
    walk_children(expr, &mut |child| {
        complete &= collect_explicit_return_aliases(
            child,
            param_slots,
            provenance,
            builtins,
            summary,
            model,
            out,
            depth + 1,
        );
    });
    complete
}
