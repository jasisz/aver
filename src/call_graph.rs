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
    tarjan_scc(graph, user_fns)
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
        Expr::BinOp(_, l, r) | Expr::Pipe(l, r) => {
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
        Expr::Match {
            subject: scrutinee,
            arms,
            ..
        } => {
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
        Expr::RecordUpdate { base, updates, .. } => {
            collect_callees_expr(base, callees);
            for (_, e) in updates {
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

    #[test]
    fn recursive_callsites_count_syntactic_occurrences() {
        let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
        let items = parse(src);
        let counts = recursive_callsite_counts(&items);
        assert_eq!(counts.get("fib").copied().unwrap_or(0), 2);
    }

    #[test]
    fn recursive_callsites_are_scoped_to_scc() {
        let src = r#"
fn a(n: Int) -> Int
    match n
        0 -> 0
        _ -> b(n - 1) + fib(n)

fn b(n: Int) -> Int
    match n
        0 -> 0
        _ -> a(n - 1)

fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
        let items = parse(src);
        let counts = recursive_callsite_counts(&items);
        assert_eq!(counts.get("a").copied().unwrap_or(0), 1);
        assert_eq!(counts.get("b").copied().unwrap_or(0), 1);
        assert_eq!(counts.get("fib").copied().unwrap_or(0), 2);
    }

    #[test]
    fn recursive_scc_ids_are_deterministic_by_group_name() {
        let src = r#"
fn z(n: Int) -> Int
    match n
        0 -> 0
        _ -> z(n - 1)

fn a(n: Int) -> Int
    match n
        0 -> 0
        _ -> b(n - 1)

fn b(n: Int) -> Int
    match n
        0 -> 0
        _ -> a(n - 1)
"#;
        let items = parse(src);
        let ids = recursive_scc_ids(&items);
        // Group {a,b} gets id=1 (min name "a"), group {z} gets id=2.
        assert_eq!(ids.get("a").copied().unwrap_or(0), 1);
        assert_eq!(ids.get("b").copied().unwrap_or(0), 1);
        assert_eq!(ids.get("z").copied().unwrap_or(0), 2);
    }

    fn parse(src: &str) -> Vec<TopLevel> {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex failed");
        let mut parser = crate::parser::Parser::new(tokens);
        parser.parse().expect("parse failed")
    }
}
