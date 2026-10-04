//! Module-level bindings in a law's unfold lists.
//!
//! A module-level binding (`base = 40` outside any fn) is a Lean constant
//! (`def base : Int := 40`, `lean::transpile::emit_module_binding`). A fn
//! that reads it unfolds to a goal that still names `base`, and `omega` or
//! `simp only [f]` treat that name as an unknown atom: `f x = x + 40` over
//! `f(x) = base + x` stays open. The strategy detectors build their unfold
//! lists from fn calls, where a binding read does not appear, so this pass
//! adds, after a strategy is chosen, every binding the listed fns read —
//! and, through each binding's value, the bindings it reads and the
//! non-recursive fns it calls — to the lists the renderer unfolds. A
//! binding is a constant, not a fn: nothing here changes which strategy a
//! law gets, only what its proof is allowed to see through.

use std::collections::{BTreeSet, HashSet};

use crate::ast::{Expr, FnDef, Spanned, Stmt, TopLevel};
use crate::ir::ProofStrategy;

use super::ProofLowerInputs;
use super::induction::collect_fn_calls_expr;

/// `strategy` with the bindings its unfold lists reach appended to them.
pub(super) fn with_binding_unfolds(
    mut strategy: ProofStrategy,
    law: &crate::ast::VerifyLaw,
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
) -> ProofStrategy {
    let has_bindings = inputs
        .entry_items
        .iter()
        .any(|item| matches!(item, TopLevel::Stmt(Stmt::Binding(..))))
        || inputs.dep_modules.iter().any(|m| !m.bindings.is_empty());
    if !has_bindings {
        return strategy;
    }
    let list = match &mut strategy {
        ProofStrategy::LinearArithmetic { unfold_fns, .. }
        | ProofStrategy::EnumConstantFold { unfold_fns }
        | ProofStrategy::SimpOverPreludeLemmas { unfold_fns, .. }
        | ProofStrategy::RingIdentity { unfold_fns }
        | ProofStrategy::NonlinearNonneg { unfold_fns } => unfold_fns,
        ProofStrategy::MapUpdatePostcondition { extra_unfolds, .. }
        | ProofStrategy::SpecEquivalence { extra_unfolds, .. }
        | ProofStrategy::SpecEquivalenceSimpNormalized { extra_unfolds } => extra_unfolds,
        _ => return strategy,
    };
    let mut seeds: BTreeSet<String> = list.iter().cloned().collect();
    collect_fn_calls_expr(&law.lhs, &mut seeds);
    collect_fn_calls_expr(&law.rhs, &mut seeds);
    if let Some(when) = &law.when {
        collect_fn_calls_expr(when, &mut seeds);
    }
    for name in reached_bindings(&seeds, inputs, scope) {
        if !list.contains(&name) {
            list.push(name);
        }
    }
    strategy
}

/// One fn or binding to scan: where it lives (a dependency's prefix, or
/// `None` for the entry), how the law spells that module (`Some(prefix)`
/// when it was reached through a qualified name), and the expressions to
/// read.
struct Pending {
    owner: Option<String>,
    spelling: Option<String>,
    exprs: Vec<Spanned<Expr>>,
}

/// The bindings (and the fns their values call) the fns in `seeds` reach,
/// spelled the way the law's file names them, in discovery order.
fn reached_bindings(
    seeds: &BTreeSet<String>,
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
) -> Vec<String> {
    let mut added: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = seeds.iter().cloned().collect();
    let mut pending: Vec<Pending> = seeds
        .iter()
        .filter_map(|name| fn_pending(name, None, inputs, scope))
        .map(|(_, pending)| pending)
        .collect();
    while let Some(item) = pending.pop() {
        let bindings = super::module_bindings_of(inputs, item.owner.as_deref());
        for expr in &item.exprs {
            let mut reads: Vec<String> = Vec::new();
            crate::call_graph::walk_expr(expr, &mut |node| {
                if let Expr::Ident(name) = node
                    && bindings.iter().any(|b| &b.name == name)
                {
                    reads.push(name.clone());
                }
            });
            for read in reads {
                let spelled = spell(item.spelling.as_deref(), &read);
                if !seen.insert(spelled.clone()) {
                    continue;
                }
                added.push(spelled);
                let Some(binding) = bindings.iter().find(|b| b.name == read) else {
                    continue;
                };
                // A binding's value is unfolded with the binding, so the
                // fns it calls must unfold too — the ones a proof can see
                // through in one step. A recursive one stays folded.
                let mut calls = BTreeSet::new();
                collect_fn_calls_expr(&binding.value, &mut calls);
                for call in calls {
                    let spelled_call = if call.contains('.') {
                        call.clone()
                    } else {
                        spell(item.spelling.as_deref(), &call)
                    };
                    if seen.contains(&spelled_call) {
                        continue;
                    }
                    if let Some((recursive, fn_item)) =
                        fn_pending(&call, item.owner.as_deref(), inputs, scope)
                        && !recursive
                    {
                        seen.insert(spelled_call.clone());
                        added.push(spelled_call);
                        pending.push(Pending {
                            spelling: if call.contains('.') {
                                fn_item.spelling
                            } else {
                                item.spelling.clone()
                            },
                            ..fn_item
                        });
                    }
                }
                pending.push(Pending {
                    owner: item.owner.clone(),
                    spelling: item.spelling.clone(),
                    exprs: vec![binding.value.clone()],
                });
            }
        }
    }
    added
}

fn spell(spelling: Option<&str>, name: &str) -> String {
    match spelling {
        Some(prefix) => format!("{prefix}.{name}"),
        None => name.to_string(),
    }
}

/// The fn a call name denotes from inside `from` (a dependency's prefix, or
/// the law's scope when `None`), whether it is recursive, and its body to
/// scan.
fn fn_pending(
    call: &str,
    from: Option<&str>,
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
) -> Option<(bool, Pending)> {
    let in_module = |prefix: &str, name: &str| -> Option<&FnDef> {
        inputs
            .dep_modules
            .iter()
            .find(|m| m.prefix == prefix)
            .and_then(|m| m.fn_defs.iter().find(|fd| fd.name == name))
    };
    let (owner, spelling, fd) = match call.rsplit_once('.') {
        Some((prefix, short)) => (
            Some(prefix.to_string()),
            Some(prefix.to_string()),
            in_module(prefix, short)?,
        ),
        None => match from.or(scope) {
            Some(prefix) if in_module(prefix, call).is_some() => {
                (Some(prefix.to_string()), None, in_module(prefix, call)?)
            }
            _ => (
                None,
                None,
                inputs.entry_items.iter().find_map(|item| match item {
                    TopLevel::FnDef(fd) if fd.name == call => Some(fd),
                    _ => None,
                })?,
            ),
        },
    };
    let key = match &owner {
        Some(prefix) => crate::ir::FnKey::in_module(prefix.clone(), &fd.name),
        None => crate::ir::FnKey::entry(&fd.name),
    };
    let recursive = inputs
        .symbol_table
        .fn_id_of(&key)
        .is_some_and(|id| inputs.recursive_fns.contains(&id));
    let exprs = fd
        .body
        .stmts()
        .iter()
        .map(|stmt| {
            let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
            expr.clone()
        })
        .collect();
    Some((
        recursive,
        Pending {
            owner,
            spelling,
            exprs,
        },
    ))
}
