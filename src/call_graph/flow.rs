//! Caller-side views of the call graph: who calls a function, and which
//! functions form one mutually-recursive group.
//!
//! Both are built from the same edge set as codegen ordering, which counts a
//! function passed as a call argument (`apply(f, x)`) and a call written with
//! the defining module's own prefix (`Decoder.readByte(...)`). A lint that
//! reasons about "how many callers does this have" must not miss either.

use std::collections::{BTreeSet, HashMap, HashSet};

use crate::ast::{Stmt, TopLevel};

use super::scc::tarjan_sccs;
use super::{collect_codegen_deps_body, collect_codegen_deps_expr, user_fn_names};

/// Stand-in caller for call edges that start in a top-level statement
/// rather than in a function body.
pub const TOP_LEVEL_CALLER: &str = "<top-level>";

fn call_edges(items: &[TopLevel]) -> HashMap<String, Vec<String>> {
    let fn_names = user_fn_names(items);
    // Only the file's own module name: a call through some *other* module's
    // prefix names that module's function, not the local one.
    let module_prefixes: HashSet<String> = crate::visibility::module_decl(items)
        .map(|m| m.name.clone())
        .into_iter()
        .collect();

    let mut edges: HashMap<String, Vec<String>> = HashMap::new();
    let mut top_level: HashSet<String> = HashSet::new();
    for item in items {
        match item {
            TopLevel::FnDef(fd) => {
                let mut deps = HashSet::new();
                collect_codegen_deps_body(&fd.body, &fn_names, &module_prefixes, &mut deps);
                let mut deps: Vec<String> = deps.into_iter().collect();
                deps.sort();
                edges.insert(fd.name.clone(), deps);
            }
            TopLevel::Stmt(Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) => {
                collect_codegen_deps_expr(expr, &fn_names, &module_prefixes, &mut top_level);
            }
            _ => {}
        }
    }

    let mut top_level: Vec<String> = top_level.into_iter().collect();
    top_level.sort();
    edges.insert(TOP_LEVEL_CALLER.to_string(), top_level);
    edges
}

/// Inverse call graph over user-defined functions: callee -> its callers.
/// Self-edges are excluded, so a self-recursive function is not its own caller.
pub fn callers_of(items: &[TopLevel]) -> HashMap<String, BTreeSet<String>> {
    let mut out: HashMap<String, BTreeSet<String>> = HashMap::new();
    for (caller, callees) in call_edges(items) {
        for callee in callees {
            if callee != caller {
                out.entry(callee).or_default().insert(caller.clone());
            }
        }
    }
    out
}

/// Strongly connected components of the call graph — one entry per
/// user-defined function group, each sorted, in a deterministic order.
pub fn call_components(items: &[TopLevel]) -> Vec<Vec<String>> {
    let names: Vec<String> = user_fn_names(items).into_iter().collect();
    tarjan_sccs(&names, &call_edges(items))
}
