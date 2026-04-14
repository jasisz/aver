use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, FnDef, Spanned, Stmt, TailCallData};

use super::collect_codegen_deps_body;
use super::scc::{tarjan_sccs, topo_components};

/// Deterministic function emission order for codegen backends.
///
/// Returns SCC components in callee-before-caller topological order.
/// Each inner vector is one SCC (single function or mutual-recursive group).
/// Function references passed as call arguments (e.g. `apply(f, x)`)
/// are treated as dependencies for ordering.
pub fn ordered_fn_components<'a>(
    fns: &[&'a FnDef],
    module_prefixes: &HashSet<String>,
) -> Vec<Vec<&'a FnDef>> {
    if fns.is_empty() {
        return vec![];
    }

    let fn_map: HashMap<String, &FnDef> = fns.iter().map(|fd| (fd.name.clone(), *fd)).collect();
    let names: Vec<String> = fn_map.keys().cloned().collect();
    let name_set: HashSet<String> = names.iter().cloned().collect();

    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    for fd in fns {
        let mut deps = HashSet::new();
        collect_codegen_deps_body(&fd.body, &name_set, module_prefixes, &mut deps);
        let mut sorted = deps.into_iter().collect::<Vec<_>>();
        sorted.sort();
        graph.insert(fd.name.clone(), sorted);
    }

    let sccs = tarjan_sccs(&names, &graph);
    let mut comp_of: HashMap<String, usize> = HashMap::new();
    for (idx, comp) in sccs.iter().enumerate() {
        for name in comp {
            comp_of.insert(name.clone(), idx);
        }
    }

    let mut comp_graph: HashMap<usize, HashSet<usize>> = HashMap::new();
    for (caller, deps) in &graph {
        let from = comp_of[caller];
        for callee in deps {
            let to = comp_of[callee];
            if from != to {
                comp_graph.entry(from).or_default().insert(to);
            }
        }
    }

    let comp_order = topo_components(&sccs, &comp_graph);
    comp_order
        .into_iter()
        .map(|idx| {
            let mut group: Vec<&FnDef> = sccs[idx]
                .iter()
                .filter_map(|name| fn_map.get(name).copied())
                .collect();
            group.sort_by(|a, b| a.name.cmp(&b.name));
            group
        })
        .collect()
}

/// Tail-call SCCs for mutual-trampoline codegen.
///
/// Returns only components with more than one function, sorted deterministically
/// by the first function name in each SCC.
pub fn tailcall_scc_components<'a>(fns: &[&'a FnDef]) -> Vec<Vec<&'a FnDef>> {
    if fns.is_empty() {
        return vec![];
    }

    let fn_map: HashMap<String, &FnDef> = fns.iter().map(|fd| (fd.name.clone(), *fd)).collect();
    let names: Vec<String> = fn_map.keys().cloned().collect();
    let name_set: HashSet<String> = names.iter().cloned().collect();

    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    for fd in fns {
        let mut deps = HashSet::new();
        collect_tailcall_deps_body(&fd.body, &name_set, &mut deps);
        let mut sorted = deps.into_iter().collect::<Vec<_>>();
        sorted.sort();
        graph.insert(fd.name.clone(), sorted);
    }

    tarjan_sccs(&names, &graph)
        .into_iter()
        .filter(|comp| comp.len() > 1)
        .map(|comp| {
            let mut group: Vec<&FnDef> = comp
                .iter()
                .filter_map(|name| fn_map.get(name).copied())
                .collect();
            group.sort_by(|a, b| a.name.cmp(&b.name));
            group
        })
        .collect()
}

fn collect_tailcall_deps_body(
    body: &FnBody,
    fn_names: &HashSet<String>,
    out: &mut HashSet<String>,
) {
    for stmt in body.stmts() {
        match stmt {
            Stmt::Expr(expr) | Stmt::Binding(_, _, expr) => {
                collect_tailcall_deps_expr(expr, fn_names, out);
            }
        }
    }
}

fn collect_tailcall_deps_expr(
    expr: &Spanned<Expr>,
    fn_names: &HashSet<String>,
    out: &mut HashSet<String>,
) {
    match &expr.node {
        Expr::TailCall(boxed) => {
            let TailCallData {
                target: target,
                args: _,
                ..
            } = boxed.as_ref();
            if fn_names.contains(target) {
                out.insert(target.clone());
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                collect_tailcall_deps_expr(&arm.body, fn_names, out);
            }
        }
        _ => {}
    }
}
