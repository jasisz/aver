/// Call-graph analysis and Tarjan's SCC algorithm.
///
/// Given a parsed program, builds a directed graph of function calls
/// and finds strongly-connected components.  A function is *recursive*
/// if it belongs to an SCC with a cycle (size > 1, or size 1 with a
/// self-edge).
use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, Stmt, TopLevel};

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Returns the SCC groups that contain cycles (self or mutual recursion).
/// Each group is a `HashSet<String>` of function names in the SCC.
pub fn find_tco_groups(items: &[TopLevel]) -> Vec<HashSet<String>> {
    let graph = build_call_graph(items);
    let user_fns: HashSet<String> = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some(fd.name.clone())
            } else {
                None
            }
        })
        .collect();

    let sccs = tarjan_scc(&graph, &user_fns);
    let mut groups = Vec::new();
    for scc in sccs {
        if scc.len() > 1 {
            groups.push(scc.into_iter().collect());
        } else if scc.len() == 1 {
            let name = &scc[0];
            if let Some(callees) = graph.get(name) {
                if callees.contains(name) {
                    groups.push(scc.into_iter().collect());
                }
            }
        }
    }
    groups
}

/// Returns the set of user-defined function names that are recursive
/// (directly or mutually).
pub fn find_recursive_fns(items: &[TopLevel]) -> HashSet<String> {
    let graph = build_call_graph(items);
    let user_fns: HashSet<String> = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some(fd.name.clone())
            } else {
                None
            }
        })
        .collect();

    let sccs = tarjan_scc(&graph, &user_fns);
    let mut recursive = HashSet::new();
    for scc in &sccs {
        if scc.len() > 1 {
            // Mutual recursion — all members are recursive
            for name in scc {
                recursive.insert(name.clone());
            }
        } else if scc.len() == 1 {
            let name = &scc[0];
            // Self-recursive if it calls itself
            if let Some(callees) = graph.get(name) {
                if callees.contains(name) {
                    recursive.insert(name.clone());
                }
            }
        }
    }
    recursive
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

fn collect_callees_body(body: &FnBody, callees: &mut HashSet<String>) {
    match body {
        FnBody::Expr(e) => collect_callees_expr(e, callees),
        FnBody::Block(stmts) => {
            for s in stmts {
                collect_callees_stmt(s, callees);
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
    match expr {
        Expr::FnCall(func, args) => {
            // Extract callee name
            match func.as_ref() {
                Expr::Ident(name) => {
                    callees.insert(name.clone());
                }
                Expr::Attr(obj, member) => {
                    if let Expr::Ident(ns) = obj.as_ref() {
                        callees.insert(format!("{}.{}", ns, member));
                    }
                }
                _ => collect_callees_expr(func, callees),
            }
            for arg in args {
                collect_callees_expr(arg, callees);
            }
        }
        Expr::Literal(_) | Expr::Resolved(_) => {}
        Expr::Ident(_) => {}
        Expr::Attr(obj, _) => collect_callees_expr(obj, callees),
        Expr::BinOp(_, l, r) => {
            collect_callees_expr(l, callees);
            collect_callees_expr(r, callees);
        }
        Expr::Pipe(l, r) => {
            collect_callees_expr(l, callees);
            collect_callees_expr(r, callees);
        }
        Expr::Match(scrutinee, arms) => {
            collect_callees_expr(scrutinee, callees);
            for arm in arms {
                collect_callees_expr(&arm.body, callees);
            }
        }
        Expr::List(elems) => {
            for e in elems {
                collect_callees_expr(e, callees);
            }
        }
        Expr::Tuple(items) => {
            for item in items {
                collect_callees_expr(item, callees);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_callees_expr(key, callees);
                collect_callees_expr(value, callees);
            }
        }
        Expr::Constructor(_, arg) => {
            if let Some(a) = arg {
                collect_callees_expr(a, callees);
            }
        }
        Expr::ErrorProp(inner) => collect_callees_expr(inner, callees),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(expr) = part {
                    collect_callees_expr(expr, callees);
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_callees_expr(e, callees);
            }
        }
        Expr::TailCall(boxed) => {
            callees.insert(boxed.0.clone());
            for arg in &boxed.1 {
                collect_callees_expr(arg, callees);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Tarjan's SCC algorithm
// ---------------------------------------------------------------------------

struct TarjanState {
    index_counter: usize,
    stack: Vec<String>,
    on_stack: HashSet<String>,
    indices: HashMap<String, usize>,
    lowlinks: HashMap<String, usize>,
    sccs: Vec<Vec<String>>,
}

fn tarjan_scc(
    graph: &HashMap<String, HashSet<String>>,
    nodes: &HashSet<String>,
) -> Vec<Vec<String>> {
    let mut state = TarjanState {
        index_counter: 0,
        stack: Vec::new(),
        on_stack: HashSet::new(),
        indices: HashMap::new(),
        lowlinks: HashMap::new(),
        sccs: Vec::new(),
    };

    for node in nodes {
        if !state.indices.contains_key(node) {
            strongconnect(node, graph, &mut state);
        }
    }

    state.sccs
}

fn strongconnect(v: &str, graph: &HashMap<String, HashSet<String>>, state: &mut TarjanState) {
    let idx = state.index_counter;
    state.index_counter += 1;
    state.indices.insert(v.to_string(), idx);
    state.lowlinks.insert(v.to_string(), idx);
    state.stack.push(v.to_string());
    state.on_stack.insert(v.to_string());

    if let Some(callees) = graph.get(v) {
        for w in callees {
            if !state.indices.contains_key(w) {
                // Only recurse into nodes that are in our function set
                if graph.contains_key(w) {
                    strongconnect(w, graph, state);
                    let w_low = state.lowlinks[w];
                    let v_low = state.lowlinks[v];
                    if w_low < v_low {
                        state.lowlinks.insert(v.to_string(), w_low);
                    }
                }
            } else if state.on_stack.contains(w) {
                let w_idx = state.indices[w];
                let v_low = state.lowlinks[v];
                if w_idx < v_low {
                    state.lowlinks.insert(v.to_string(), w_idx);
                }
            }
        }
    }

    // If v is a root node, pop the SCC
    if state.lowlinks[v] == state.indices[v] {
        let mut scc = Vec::new();
        loop {
            let w = state.stack.pop().unwrap();
            state.on_stack.remove(&w);
            scc.push(w.clone());
            if w == v {
                break;
            }
        }
        state.sccs.push(scc);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_self_recursion() {
        let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
        let items = parse(src);
        let rec = find_recursive_fns(&items);
        assert!(
            rec.contains("fib"),
            "fib should be recursive, got: {:?}",
            rec
        );
    }

    #[test]
    fn non_recursive_fn() {
        let src = "fn double(x: Int) -> Int\n    = x + x\n";
        let items = parse(src);
        let rec = find_recursive_fns(&items);
        assert!(
            rec.is_empty(),
            "double should not be recursive, got: {:?}",
            rec
        );
    }

    #[test]
    fn mutual_recursion() {
        let src = r#"
fn isEven(n: Int) -> Bool
    match n
        0 -> true
        _ -> isOdd(n - 1)

fn isOdd(n: Int) -> Bool
    match n
        0 -> false
        _ -> isEven(n - 1)
"#;
        let items = parse(src);
        let rec = find_recursive_fns(&items);
        assert!(rec.contains("isEven"), "isEven should be recursive");
        assert!(rec.contains("isOdd"), "isOdd should be recursive");
    }

    fn parse(src: &str) -> Vec<TopLevel> {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex failed");
        let mut parser = crate::parser::Parser::new(tokens);
        parser.parse().expect("parse failed")
    }
}
