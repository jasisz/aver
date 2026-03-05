/// Call-graph analysis and Tarjan's SCC algorithm.
///
/// Given a parsed program, builds a directed graph of function calls
/// and finds strongly-connected components.  A function is *recursive*
/// if it belongs to an SCC with a cycle (size > 1, or size 1 with a
/// self-edge).
use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, Stmt, StrPart, TopLevel};

mod codegen;
mod scc;

pub use codegen::ordered_fn_components;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Returns the SCC groups that contain cycles (self or mutual recursion).
/// Each group is a `HashSet<String>` of function names in the SCC.
pub fn find_tco_groups(items: &[TopLevel]) -> Vec<HashSet<String>> {
    let graph = build_call_graph(items);
    let user_fns = user_fn_names(items);
    recursive_sccs(&graph, &user_fns)
        .into_iter()
        .map(|scc| scc.into_iter().collect())
        .collect()
}

/// Returns the set of user-defined function names that are recursive
/// (directly or mutually).
pub fn find_recursive_fns(items: &[TopLevel]) -> HashSet<String> {
    let graph = build_call_graph(items);
    let user_fns = user_fn_names(items);
    let mut recursive = HashSet::new();
    for scc in recursive_sccs(&graph, &user_fns) {
        for name in scc {
            recursive.insert(name);
        }
    }
    recursive
}

/// Direct call summary per user-defined function (unique + sorted).
pub fn direct_calls(items: &[TopLevel]) -> HashMap<String, Vec<String>> {
    let graph = build_call_graph(items);
    let mut out = HashMap::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            let mut callees = graph
                .get(&fd.name)
                .cloned()
                .unwrap_or_default()
                .into_iter()
                .collect::<Vec<_>>();
            callees.sort();
            out.insert(fd.name.clone(), callees);
        }
    }
    out
}

/// Count recursive callsites per user-defined function, scoped to caller SCC.
///
/// Callsite definition:
/// - one syntactic `FnCall` or `TailCall` node in the function body,
/// - whose callee is a user-defined function in the same recursive SCC
///   as the caller.
///
/// This is a syntactic metric over AST nodes (not dynamic execution count,
/// not CFG edges), so it stays stable across control-flow rewrites.
pub fn recursive_callsite_counts(items: &[TopLevel]) -> HashMap<String, usize> {
    let graph = build_call_graph(items);
    let user_fns = user_fn_names(items);
    let sccs = recursive_sccs(&graph, &user_fns);
    let mut scc_members: HashMap<String, HashSet<String>> = HashMap::new();
    for scc in sccs {
        let members: HashSet<String> = scc.iter().cloned().collect();
        for name in scc {
            scc_members.insert(name, members.clone());
        }
    }

    let mut out = HashMap::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            let mut count = 0usize;
            if let Some(members) = scc_members.get(&fd.name) {
                count_recursive_calls_body(&fd.body, members, &mut count);
            }
            out.insert(fd.name.clone(), count);
        }
    }
    out
}

/// Deterministic recursive SCC id per function (1-based).
/// Non-recursive functions are absent from the returned map.
pub fn recursive_scc_ids(items: &[TopLevel]) -> HashMap<String, usize> {
    let graph = build_call_graph(items);
    let user_fns = user_fn_names(items);
    let mut sccs = recursive_sccs(&graph, &user_fns);
    for scc in &mut sccs {
        scc.sort();
    }
    sccs.sort_by(|a, b| a.first().cmp(&b.first()));

    let mut out = HashMap::new();
    for (idx, scc) in sccs.into_iter().enumerate() {
        let id = idx + 1;
        for name in scc {
            out.insert(name, id);
        }
    }
    out
}

fn collect_codegen_deps_body(body: &FnBody, fn_names: &HashSet<String>, out: &mut HashSet<String>) {
    match body {
        FnBody::Expr(e) => collect_codegen_deps_expr(e, fn_names, out),
        FnBody::Block(stmts) => {
            for s in stmts {
                match s {
                    Stmt::Binding(_, _, e) | Stmt::Expr(e) => {
                        collect_codegen_deps_expr(e, fn_names, out)
                    }
                }
            }
        }
    }
}

fn collect_codegen_deps_expr(expr: &Expr, fn_names: &HashSet<String>, out: &mut HashSet<String>) {
    walk_expr(expr, &mut |node| match node {
        Expr::FnCall(func, args) => {
            if let Some(callee) = expr_to_dotted_name(func.as_ref())
                && fn_names.contains(&callee)
            {
                out.insert(callee);
            }
            for arg in args {
                // function-as-value dependency, e.g. List.fold(xs, init, f)
                if let Some(qname) = expr_to_dotted_name(arg)
                    && fn_names.contains(&qname)
                {
                    out.insert(qname);
                }
            }
        }
        Expr::TailCall(boxed) => {
            if fn_names.contains(&boxed.0) {
                out.insert(boxed.0.clone());
            }
        }
        _ => {}
    });
}

fn expr_to_dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(obj, field) => {
            let head = expr_to_dotted_name(obj)?;
            Some(format!("{}.{}", head, field))
        }
        _ => None,
    }
}

fn walk_expr(expr: &Expr, visit: &mut impl FnMut(&Expr)) {
    visit(expr);
    match expr {
        Expr::FnCall(func, args) => {
            walk_expr(func, visit);
            for arg in args {
                walk_expr(arg, visit);
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.1 {
                walk_expr(arg, visit);
            }
        }
        Expr::Attr(obj, _) => walk_expr(obj, visit),
        Expr::BinOp(_, l, r) => {
            walk_expr(l, visit);
            walk_expr(r, visit);
        }
        Expr::Match { subject, arms, .. } => {
            walk_expr(subject, visit);
            for arm in arms {
                walk_expr(&arm.body, visit);
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                walk_expr(item, visit);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                walk_expr(k, visit);
                walk_expr(v, visit);
            }
        }
        Expr::Constructor(_, maybe) => {
            if let Some(inner) = maybe {
                walk_expr(inner, visit);
            }
        }
        Expr::ErrorProp(inner) => walk_expr(inner, visit),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(e) = part {
                    walk_expr(e, visit);
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                walk_expr(e, visit);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk_expr(base, visit);
            for (_, e) in updates {
                walk_expr(e, visit);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => {}
    }
}

// ---------------------------------------------------------------------------
// Call graph construction
// ---------------------------------------------------------------------------

fn build_call_graph(items: &[TopLevel]) -> HashMap<String, HashSet<String>> {
    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            let mut callees = HashSet::new();
            collect_callees_body(&fd.body, &mut callees);
            graph.insert(fd.name.clone(), callees);
        }
    }
    graph
}

fn user_fn_names(items: &[TopLevel]) -> HashSet<String> {
    items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some(fd.name.clone())
            } else {
                None
            }
        })
        .collect()
}

fn recursive_sccs(
    graph: &HashMap<String, HashSet<String>>,
    user_fns: &HashSet<String>,
) -> Vec<Vec<String>> {
    let mut names = user_fns.iter().cloned().collect::<Vec<_>>();
    names.sort();

    let mut adj: HashMap<String, Vec<String>> = HashMap::new();
    for name in &names {
        let mut deps = graph
            .get(name)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .filter(|callee| user_fns.contains(callee))
            .collect::<Vec<_>>();
        deps.sort();
        adj.insert(name.clone(), deps);
    }

    scc::tarjan_sccs(&names, &adj)
        .into_iter()
        .filter(|scc| is_recursive_scc(scc, graph))
        .collect()
}

fn is_recursive_scc(scc: &[String], graph: &HashMap<String, HashSet<String>>) -> bool {
    if scc.len() > 1 {
        return true;
    }
    if let Some(name) = scc.first() {
        return graph
            .get(name)
            .is_some_and(|callees| callees.contains(name));
    }
    false
}

pub(crate) fn collect_callees_body(body: &FnBody, callees: &mut HashSet<String>) {
    match body {
        FnBody::Expr(e) => collect_callees_expr(e, callees),
        FnBody::Block(stmts) => {
            for s in stmts {
                collect_callees_stmt(s, callees);
            }
        }
    }
}

fn count_recursive_calls_body(body: &FnBody, recursive: &HashSet<String>, out: &mut usize) {
    match body {
        FnBody::Expr(e) => count_recursive_calls_expr(e, recursive, out),
        FnBody::Block(stmts) => {
            for s in stmts {
                count_recursive_calls_stmt(s, recursive, out);
            }
        }
    }
}

fn count_recursive_calls_stmt(stmt: &Stmt, recursive: &HashSet<String>, out: &mut usize) {
    match stmt {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => count_recursive_calls_expr(e, recursive, out),
    }
}

fn count_recursive_calls_expr(expr: &Expr, recursive: &HashSet<String>, out: &mut usize) {
    match expr {
        Expr::FnCall(func, args) => {
            match func.as_ref() {
                Expr::Ident(name) => {
                    if recursive.contains(name) {
                        *out += 1;
                    }
                }
                Expr::Attr(obj, member) => {
                    if let Expr::Ident(ns) = obj.as_ref() {
                        let q = format!("{}.{}", ns, member);
                        if recursive.contains(&q) {
                            *out += 1;
                        }
                    } else {
                        count_recursive_calls_expr(obj, recursive, out);
                    }
                }
                other => count_recursive_calls_expr(other, recursive, out),
            }
            for arg in args {
                count_recursive_calls_expr(arg, recursive, out);
            }
        }
        Expr::TailCall(boxed) => {
            if recursive.contains(&boxed.0) {
                *out += 1;
            }
            for arg in &boxed.1 {
                count_recursive_calls_expr(arg, recursive, out);
            }
        }
        Expr::Literal(_) | Expr::Resolved(_) | Expr::Ident(_) => {}
        Expr::Attr(obj, _) => count_recursive_calls_expr(obj, recursive, out),
        Expr::BinOp(_, l, r) => {
            count_recursive_calls_expr(l, recursive, out);
            count_recursive_calls_expr(r, recursive, out);
        }
        Expr::Match {
            subject: scrutinee,
            arms,
            ..
        } => {
            count_recursive_calls_expr(scrutinee, recursive, out);
            for arm in arms {
                count_recursive_calls_expr(&arm.body, recursive, out);
            }
        }
        Expr::List(elems) | Expr::Tuple(elems) => {
            for e in elems {
                count_recursive_calls_expr(e, recursive, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                count_recursive_calls_expr(k, recursive, out);
                count_recursive_calls_expr(v, recursive, out);
            }
        }
        Expr::Constructor(_, arg) => {
            if let Some(a) = arg {
                count_recursive_calls_expr(a, recursive, out);
            }
        }
        Expr::ErrorProp(inner) => count_recursive_calls_expr(inner, recursive, out),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(expr) = part {
                    count_recursive_calls_expr(expr, recursive, out);
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                count_recursive_calls_expr(e, recursive, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            count_recursive_calls_expr(base, recursive, out);
            for (_, e) in updates {
                count_recursive_calls_expr(e, recursive, out);
            }
        }
    }
}

fn collect_callees_stmt(stmt: &Stmt, callees: &mut HashSet<String>) {
    match stmt {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => {
            collect_callees_expr(e, callees);
        }
    }
}

fn collect_callees_expr(expr: &Expr, callees: &mut HashSet<String>) {
    walk_expr(expr, &mut |node| match node {
        Expr::FnCall(func, _) => {
            if let Some(callee) = expr_to_dotted_name(func.as_ref()) {
                callees.insert(callee);
            }
        }
        Expr::TailCall(boxed) => {
            callees.insert(boxed.0.clone());
        }
        _ => {}
    });
}

#[cfg(test)]
mod tests;
